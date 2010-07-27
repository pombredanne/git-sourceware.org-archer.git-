/* A recursive-descent C/C++ parser for GDB, the GNU debugger.

   Copyright (C) 2010 Free Software Foundation, Inc.

   This file is part of GDB.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

#include "defs.h"
#include "expression.h"
#include "arch-utils.h"
#include "dfp.h"
#include "gdb_assert.h"
#include "gdb_string.h"
#include "gdbtypes.h"
#include "language.h"
#include "parser-defs.h"
#include "vec.h"

#define parse_type builtin_type (parse_gdbarch)

/* Lexer token table:
   OP(name, string description): operator tokens
   TK(name, string description): parser tokens

   Hmm... Still need reserved keywords...
*/
#define TOKEN_TABLE				\
  OP(EQ,		"=")			\
  OP(NOT,		"!")			\
  OP(PLUS,		"+")			\
  OP(MINUS,		"-")			\
  OP(MULT,		"*")			\
  OP(DIV,		"/")			\
  OP(MOD,		"%")			\
  OP(AND,		"&")			\
  OP(OR,		"|")			\
  OP(XOR,		"^")			\
  OP(RSHIFT,		">>")			\
  OP(LSHIFT,		"<<")			\
  OP(COMPL,		"~")			\
  OP(AND_AND,		"&&")			\
  OP(OR_OR,		"||")			\
  OP(QUERY,		"?")			\
  OP(COLON,		":")			\
  OP(COMMA,		",")			\
  OP(OPEN_PAREN,	"(")			\
  OP(CLOSE_PAREN,	")")			\
  OP(EQ_EQ,		"==")			\
  OP(NOT_EQ,		"!=")			\
  OP(GREATER,		">")			\
  OP(LESS,		"<")			\
  OP(GREATER_EQ,	">=")			\
  OP(LESS_EQ,		"<=")			\
  OP(PLUS_EQ,		"+=")			\
  OP(MINUS_EQ,		"-=")			\
  OP(MULTI_EQ,		"*=")			\
  OP(DIV_EQ,		"/=")			\
  OP(MOD_EQ,		"%=")			\
  OP(AND_EQ,		"&=")			\
  OP(OR_EQ,		"|=")			\
  OP(XOR_EQ,		"^=")			\
  OP(RSHIFT_EQ,		">>=")			\
  OP(LSHIFT_EQ,		"<<=")			\
  OP(OPEN_SQUARE,	"[")			\
  OP(CLOSE_SQUARE,	"]")			\
  OP(OPEN_BRACE,	"{")			\
  OP(CLOSE_BRACE, 	"}")			\
  OP(PLUS_PLUS, 	"++")			\
  OP(MINUS_MINUS, 	"--")			\
  OP(DEREF,		"->")			\
  OP(DOT,		".")			\
  OP(SCOPE,		"::")			\
  OP(DEREF_STAR,	"->*")			\
  OP(DOT_STAR,		".*")			\
  OP(ATSIGN,		"@")			\
  TK(EOF,		NONE)			\
  TK(ERROR,		NONE)			\
  TK(NAME,		IDENT)			\
  TK(NUMBER,		LITERAL)		\
  TK(CHAR,		LITERAL)		\
  TK(OTHER,		LITERAL)		\
  TK(STRING,		LITERAL)

#define OP(e,s) TTYPE_ ## e,
#define TK(e,s) TTYPE_ ## e,

typedef enum
{
  TOKEN_TABLE
  N_TOKEN_TYPES
} token_type;
#undef OP
#undef TK

#define OP(e,s) "TTYPE_" # e,
#define TK(e,s) "TTYPE_" # e,
static const char *token_table_strings[(int) N_TOKEN_TYPES] =
  {
    TOKEN_TABLE
  };
#undef OP
#undef TK

typedef unsigned char token_value;
typedef struct
{
  token_type type;
  token_value *value;		/* Must be free'd (necesary?) */
} cp_token;

/* A token signifying the end of input.  */
static cp_token EOF_token = {TTYPE_EOF, 0};

/* A buffer for lexing the input.  */
typedef struct
{
  /* The actual buffer holding the input.  */
  const char *buffer;

  /* A pointer to the current lexing location.  */
  const char *cur;
} cp_buffer;

/* A vector used to hold the token stream.  */
DEF_VEC_O (cp_token);

/* A parser instance  */
typedef struct
{
  /* Buffer used for lexing  */
  cp_buffer buffer;

  /* The token stream  */
  VEC (cp_token) *token_fifo;
} cp_parser;

/* Operator precedence levels  */
enum cp_precedence
{
  PREC_NOT_OPERATOR,
  PREC_LOGICAL_OR_EXPRESSION,
  PREC_LOGICAL_AND_EXPRESSION,
  PREC_INCLUSIVE_OR_EXPRESSION,
  PREC_EXCLUSIVE_OR_EXPRESSION,
  PREC_AND_EXPRESSION,
  PREC_EQUALITY_EXPRESSION,
  PREC_RELATIONAL_EXPRESSION,
  PREC_SHIFT_EXPRESSION,
  PREC_ADDITIVE_EXPRESSION,
  PREC_MULTIPLICATIVE_EXPRESSION,
  NUM_PREC_VALUES
};

/* Expression codes for joining expression chains  */
/* [Can we re-use gdb's own expression operators?] */
enum expr_code
{
  EQ_EXPR,
  MULT_EXPR,
  DIV_EXPR,
  MOD_EXPR,
  PLUS_EXPR,
  MINUS_EXPR,
  AND_EXPR,
  OR_EXPR,
  XOR_EXPR,
  RSHIFT_EXPR,
  LSHIFT_EXPR,
  AND_AND_EXPR,
  OR_OR_EXPR,
  EQ_EQ_EXPR,
  NOT_EQ_EXPR,
  GREATER_EXPR,
  LESS_EXPR,
  GREATER_EQ_EXPR,
  LESS_EQ_EXPR,
};

/* An expression "chain" which wraps the parser globals
   expout, expout_size, and expout_ptr.  */
typedef struct _cp_expression
{
  /* The actual expression  */
  struct expression *exp;

  /* The allocated number of elements in the expression.  */
  int size;

  /* A pointer to the next free slot in the expression.  */
  int ptr;
} cp_expression;

/* A stack is used to deal with operator precedence.  This
   structure describes a stack entry for this.  */
typedef struct cp_expression_stack_entry
{
  /* The expression that is being suspended.  */
  cp_expression *lhs;

  /* The operator that is being suspended.  */
  enum expr_code operator;

  /* The precedence level of this sub-expression.  */
  enum cp_precedence prec;
} cp_expression_stack_entry;

/* Type definition for the operator precedence stack.  */
typedef cp_expression_stack_entry cp_expression_stack[NUM_PREC_VALUES];

/* A node used to create a convenient mapping between binary operator
   token types and precedence levels and expression codes.  */
struct binary_operations_node
{
  /* The token  */
  token_type token_type;

  /* The expression code of this operator  */
  enum expr_code code;

  /* The precedence of this operator  */
  enum cp_precedence prec;
};

/* A list of all binary operators.  This list will be used to
   construct a map keyed on the token type in the initializer function.  */
static const struct binary_operations_node binary_ops[] = {
  { TTYPE_MULT, MULT_EXPR, PREC_MULTIPLICATIVE_EXPRESSION },
  { TTYPE_DIV, DIV_EXPR, PREC_MULTIPLICATIVE_EXPRESSION },
  { TTYPE_MOD, MOD_EXPR, PREC_MULTIPLICATIVE_EXPRESSION },

  { TTYPE_PLUS, PLUS_EXPR, PREC_ADDITIVE_EXPRESSION },
  { TTYPE_MINUS, MINUS_EXPR, PREC_ADDITIVE_EXPRESSION },

  { TTYPE_AND, AND_EXPR, PREC_AND_EXPRESSION },

  { TTYPE_OR, OR_EXPR, PREC_INCLUSIVE_OR_EXPRESSION },

  { TTYPE_XOR, XOR_EXPR, PREC_EXCLUSIVE_OR_EXPRESSION },

  { TTYPE_RSHIFT, RSHIFT_EXPR, PREC_SHIFT_EXPRESSION },
  { TTYPE_LSHIFT, LSHIFT_EXPR, PREC_SHIFT_EXPRESSION },

  { TTYPE_AND_AND, AND_AND_EXPR, PREC_LOGICAL_AND_EXPRESSION },

  { TTYPE_OR_OR, OR_OR_EXPR, PREC_LOGICAL_OR_EXPRESSION },

  { TTYPE_EQ_EQ, EQ_EQ_EXPR, PREC_EQUALITY_EXPRESSION },
  { TTYPE_NOT_EQ, NOT_EQ_EXPR, PREC_EQUALITY_EXPRESSION },

  { TTYPE_GREATER, GREATER_EXPR, PREC_RELATIONAL_EXPRESSION },
  { TTYPE_LESS, LESS_EXPR, PREC_RELATIONAL_EXPRESSION },
  { TTYPE_GREATER_EQ, GREATER_EQ_EXPR, PREC_RELATIONAL_EXPRESSION },
  { TTYPE_LESS_EQ, LESS_EQ_EXPR, PREC_RELATIONAL_EXPRESSION },
};

/* The actual map that will be built.  */
static struct binary_operations_node binary_ops_token[N_TOKEN_TYPES];

/* The types of numbers that can be parsed.  */
enum typed_number_kind
{
  /* Integer  */
  INT,

  /* Floating point  */
  FLOAT,

  /* Decimal floating point  */
  DECFLOAT
};

/* A structure used to parse numbers  */
typedef struct
{
  enum typed_number_kind kind;
  struct type *type;
  union {
    LONGEST lval;
    DOUBLEST dval;
    gdb_byte bytes[16];
  };
} cp_typed_number;

/* Returns the current expression chain (from expout).  If the argument
   is non-NULL, then the current chain is appended to it.

   In both cases, expout will be reset.  */
static cp_expression *cp_parse_expression (cp_parser *);



static inline void
cp_push_token (cp_parser *parser, const cp_token token)
{
  VEC_safe_push (cp_token, parser->token_fifo, &token);
}

static inline cp_token
cp_peek_token (const cp_parser *parser)
{
  if (VEC_empty (cp_token, parser->token_fifo))
    return EOF_token;
  return *VEC_index (cp_token, parser->token_fifo, 0);
}

#if 0
static inline cp_token
cp_peek_token_2 (const cp_parser *parser)
{
  if (VEC_length (cp_token, parser->token_fifo) < 2)
    return EOF_token;
  return *VEC_index (cp_token, parser->token_fifo, 1);
}
#endif

static inline void
cp_consume_token (const cp_parser *parser)
{
  if (!VEC_empty (cp_token, parser->token_fifo))
    {
      cp_token token = cp_peek_token (parser);
      if (token.value)
	xfree (token.value);
      VEC_ordered_remove (cp_token, parser->token_fifo, 0);
    }
}

static inline int
cp_is_eof_token (cp_token token)
{
  return token.type == TTYPE_EOF;
}



static void
cp_expression_append_chain (cp_expression *dest, cp_expression *src)
{
  int size = dest->size;
  while (size <= dest->ptr + src->ptr)
    size *= 2;

  if (size > dest->size)
    {
      dest->size = size;
      dest->exp = (struct expression *)
	xrealloc ((char *) dest->exp, sizeof (struct expression)
		  + EXP_ELEM_TO_BYTES (dest->size));
    }

  memcpy (&dest->exp->elts[dest->ptr], src->exp->elts,
	  EXP_ELEM_TO_BYTES (src->ptr));
  dest->ptr += src->ptr;
  dest->exp->nelts += src->exp->nelts;
}

/* Frees all memory allocated in the given chain.  */
static void
free_expression_chain (cp_expression *chain)
{
  xfree (chain->exp);
  xfree (chain);
}

static cp_expression *
cp_expression_chain (cp_expression *chain)
{
  if (chain == NULL)
    {
      /* new chain */
      chain = (cp_expression *) xmalloc (sizeof (cp_expression));
      chain->size = expout_size;
      chain->ptr = expout_ptr;
      chain->exp = expout;
      chain->exp->nelts = expout_ptr;
    }
  else
    {
      /* append chain */
      cp_expression tmp;
      tmp.size = expout_size;
      tmp.ptr = expout_ptr;
      tmp.exp = expout;
      tmp.exp->nelts = expout_ptr;
      cp_expression_append_chain (chain, &tmp);
    }

  /* Reset globals for next expression  */
  expout_size = 10;
  expout_ptr = 0;
  expout = (struct expression *)
    xmalloc (sizeof (struct expression) + EXP_ELEM_TO_BYTES (expout_size));
  expout->language_defn = current_language;
  expout->gdbarch = get_current_arch ();

  return chain;
}

static void
parse_number (const cp_parser *parser, token_value *value, cp_typed_number *result)
{
  /* FIXME: Shouldn't these be unsigned?  We don't deal with negative values
     here, and we do kind of silly things like cast to unsigned.  */
  LONGEST n = 0;
  LONGEST prevn = 0;
  ULONGEST un;

  int i = 0;
  int c;
  int base = input_radix;
  int unsigned_p = 0;

  /* Number of "L" suffixes encountered.  */
  int long_p = 0;

  /* We have found a "L" or "U" suffix.  */
  int found_suffix = 0;

  ULONGEST high_bit;
  struct type *signed_type;
  struct type *unsigned_type;

  int parsed_float = (strpbrk ((char *) value, ".eE") != NULL);
  int len = strlen (value);

  token_value *p = value;

  if (parsed_float)
    {
      /* It's a float since it contains a point or an exponent.  */
      token_value *s;
      int num;
      token_value saved_char;

      /* If it ends with "df", "dd", or "dl", take it as a type of decimal
	 floating point.  Result is DECFLOAT.  */
#if 1
      if (len >= 2 && p[len - 2] == 'd' && p[len - 1] == 'f')
	{
	  p[len - 2] = '\0';
	  result->kind = DECFLOAT;
	  result->type = parse_type->builtin_decfloat;
	  decimal_from_string (result->bytes, 4,
			       gdbarch_byte_order (parse_gdbarch), p);
	  p[len - 2] = 'd';
	  return;
	}

      if (len >= 2 && p[len - 2] == 'd' && p[len - 1] == 'd')
	{
	  p[len - 2] = '\0';
	  result->kind = DECFLOAT;
	  result->type = parse_type->builtin_decdouble;
	  decimal_from_string (result->bytes, 8,
			       gdbarch_byte_order (parse_gdbarch), p);
	  p[len - 2] = 'd';
	  return;
	}

      if (len >= 2 && p[len - 2] == 'd' && p[len - 1] == 'l')
	{
	  p[len - 2] = '\0';
	  result->kind = DECFLOAT;
	  result->type = parse_type->builtin_declong;
	  decimal_from_string (result->bytes, 16,
			       gdbarch_byte_order (parse_gdbarch), p);
	  return;
	}
#else
      /* an attempt to rewrite some of this mess... needs work, though. */
      if (len >= 2 && p[len - 2] == 'd')
	{
	  int len;
	  struct type *type;

	  if (p[len - 1] == 'f')
	    {
	      saved_char = 'f';
	      len = 4;
	      type = parse_type->builtin_decfloat;
	    }
	  else if (p[len - 1] == 'd')
	    {
	      saved_char = 'd';
	      len = 8;
	      type = parse_type->builtin_decdouble;
	    }
	  else if (p[len - 1] == 'l')
	    {
	      saved_char = 'l';
	      len = 16;
	      type = parse_type->builtin_declong;
	    }
	  else
	    break; /* this doesn't work! */

	  p[len - 2] = '\0';
	  decimal_from_string (result->bytes, len,
			       gdbarch_byte_order (parse_gdbarch), p);
	  p[len - 2] = saved_char;
	  return;
	}
#endif

      s = alloca (len);
      saved_char = p[len];
      p[len] = 0;       /* null-terminate the token */
      num = sscanf (p, "%" DOUBLEST_SCAN_FORMAT "%s", &result->dval, s);
      p[len] = saved_char;      /* restore the input stream */

      if (num == 1)
        result->type = parse_type->builtin_double;

      if (num == 2 )
        {
          /* See if it has any float suffix: 'f' for float, 'l' for long 
             double.  */
          if (!strcasecmp (s, "f"))
            result->type = parse_type->builtin_float;
          else if (!strcasecmp (s, "l"))
            result->type = parse_type->builtin_long_double;
          else
	    goto error;
        }

      result->kind = FLOAT;
      return;
    }

  /* Handle base-switching prefixes 0x, 0t, 0d, 0 */
  if (p[0] == '0')
    switch (p[1])
      {
      case 'x':
      case 'X':
        if (len >= 3)
          {
            p += 2;
            base = 16;
            len -= 2;
          }
        break;

      case 'b':
      case 'B':
        if (len >= 3)
          {
            p += 2;
            base = 2;
            len -= 2;
          }
        break;

      case 't':
      case 'T':
      case 'd':
      case 'D':
        if (len >= 3)
          {
            p += 2;
            base = 10;
            len -= 2;
          }
        break;

      default:
        base = 8;
        break;
      }

  while (len-- > 0)
    {
      c = *p++;
      if (c >= 'A' && c <= 'Z')
        c += 'a' - 'A';
      if (c != 'l' && c != 'u')
        n *= base;
      if (c >= '0' && c <= '9')
        {
          if (found_suffix)
            goto error;
          n += i = c - '0';
        }
      else
        {
          if (base > 10 && c >= 'a' && c <= 'f')
            {
              if (found_suffix)
		goto error;
              n += i = c - 'a' + 10;
            }
          else if (c == 'l')
            {
              ++long_p;
              found_suffix = 1;
            }
          else if (c == 'u')
            {
              unsigned_p = 1;
              found_suffix = 1;
            }
          else
            goto error;       /* Char not a digit */
        }
      if (i >= base)
        goto error;           /* Invalid digit in this base */

      /* Portably test for overflow (only works for nonzero values, so make
         a second check for zero).  FIXME: Can't we just make n and prevn
         unsigned and avoid this?  */
      if (c != 'l' && c != 'u' && (prevn >= n) && n != 0)
        unsigned_p = 1;         /* Try something unsigned */

      /* Portably test for unsigned overflow.
         FIXME: This check is wrong; for example it doesn't find overflow
         on 0x123456789 when LONGEST is 32 bits.  */
      if (c != 'l' && c != 'u' && n != 0)
        {
          if ((unsigned_p && (ULONGEST) prevn >= (ULONGEST) n))
            error (_("Numeric constant too large."));
        }
      prevn = n;
    }

  /* An integer constant is an int, a long, or a long long.  An L
     suffix forces it to be long; an LL suffix forces it to be long
     long.  If not forced to a larger size, it gets the first type of
     the above that it fits in.  To figure out whether it fits, we
     shift it right and see whether anything remains.  Note that we
     can't shift sizeof (LONGEST) * HOST_CHAR_BIT bits or more in one
     operation, because many compilers will warn about such a shift
     (which always produces a zero result).  Sometimes gdbarch_int_bit
     or gdbarch_long_bit will be that big, sometimes not.  To deal with
     the case where it is we just always shift the value more than
     once, with fewer bits each time.  */

  un = (ULONGEST)n >> 2;
  if (long_p == 0
      && (un >> (gdbarch_int_bit (parse_gdbarch) - 2)) == 0)
    {
      high_bit = ((ULONGEST) 1) << (gdbarch_int_bit (parse_gdbarch) - 1);

      /* A large decimal (not hex or octal) constant (between INT_MAX
         and UINT_MAX) is a long or unsigned long, according to ANSI,
         never an unsigned int, but this code treats it as unsigned
         int.  This probably should be fixed.  GCC gives a warning on
         such constants.  */

      unsigned_type = parse_type->builtin_unsigned_int;
      signed_type = parse_type->builtin_int;
    }
  else if (long_p <= 1
           && (un >> (gdbarch_long_bit (parse_gdbarch) - 2)) == 0)
    {
      high_bit = ((ULONGEST) 1) << (gdbarch_long_bit (parse_gdbarch) - 1);
      unsigned_type = parse_type->builtin_unsigned_long;
      signed_type = parse_type->builtin_long;
    }
  else
    {
      int shift;
      if (sizeof (ULONGEST) * HOST_CHAR_BIT
          < gdbarch_long_long_bit (parse_gdbarch))
        /* A long long does not fit in a LONGEST.  */
        shift = (sizeof (ULONGEST) * HOST_CHAR_BIT - 1);
      else
        shift = (gdbarch_long_long_bit (parse_gdbarch) - 1);
      high_bit = (ULONGEST) 1 << shift;
      unsigned_type = parse_type->builtin_unsigned_long_long;
      signed_type = parse_type->builtin_long_long;
    }

  result->lval = n;

   /* If the high bit of the worked out type is set then this number
      has to be unsigned. */
   result->type = (unsigned_p || (n & high_bit)) ? unsigned_type : signed_type;
   result->kind = INT;
   return;

 error:
   error (_("Invalid number \"%s\"."), value);
}

static cp_expression *
cp_parse_primary_expression (cp_parser *parser)
{
  cp_token token = cp_peek_token (parser);
  switch (token.type)
    {
    case TTYPE_NUMBER:
      {
	int r;
	cp_typed_number number;
	parse_number (parser, token.value, &number);
	cp_consume_token (parser);

	switch (number.kind)
	  {
	  case INT:
	    write_exp_elt_opcode (OP_LONG);
	    write_exp_elt_type (number.type);
	    write_exp_elt_longcst (number.lval);
	    write_exp_elt_opcode (OP_LONG);
	    break;

	  case FLOAT:
	    write_exp_elt_opcode (OP_DOUBLE);
	    write_exp_elt_type (number.type);
	    write_exp_elt_dblcst (number.dval);
	    write_exp_elt_opcode (OP_DOUBLE);
	    break;

	  case DECFLOAT:
	    write_exp_elt_opcode (OP_DECFLOAT);
	    write_exp_elt_type (number.type);
	    write_exp_elt_decfloatcst (number.bytes);
	    write_exp_elt_opcode (OP_DECFLOAT);
	    break;
	  }

	/* expout now contains the number's expression chain. So
	   we need to save this and reset expout. */
	return cp_expression_chain (NULL);
      }
      break;

    default:
      error (_("expected primary expression"));
    }

  /* probably want a special marker like g++ does */
  return NULL;
}

static cp_expression *
cp_parse_postfix_expression (cp_parser *parser)
{
  cp_token token = cp_peek_token (parser);

  switch (token.type)
    {
    default:
      return cp_parse_primary_expression (parser);
      break;
    }

  return NULL;
}

static cp_expression *
cp_parse_unary_expression (cp_parser *parser)
{
  cp_token token = cp_peek_token (parser);

  /* check for keyword */
  /* check for scope operator */
  /* look for unary operator */

  return cp_parse_postfix_expression (parser);
}

static cp_expression *
cp_cast_expression (cp_parser *parser)
{
  /* determine if parser is looking at a cast of some sort */
  if (cp_peek_token (parser).type == TTYPE_OPEN_PAREN)
    {
      /* we could be looking at a cast... */
      return NULL;
    }

  return cp_parse_unary_expression (parser);
}

static cp_expression *
cp_parse_simple_cast_expression (cp_parser *parser)
{
  return cp_cast_expression (parser);
}

static cp_expression *
build_binary_op (cp_expression *lhs, cp_expression *rhs, enum expr_code code)
{
  enum exp_opcode operator;

  switch (code)
    {
    case MULT_EXPR:
      operator = BINOP_MUL;
      break;

    case DIV_EXPR:
      operator = BINOP_DIV;
      break;

    case MOD_EXPR:
      operator = BINOP_MOD;
      break;

    case PLUS_EXPR:
      operator = BINOP_ADD;
      break;

    case MINUS_EXPR:
      operator = BINOP_SUB;
      break;

    case AND_EXPR:
      operator = BINOP_BITWISE_AND;
      break;

    case OR_EXPR:
      operator = BINOP_BITWISE_IOR;
      break;

    case XOR_EXPR:
      operator = BINOP_BITWISE_XOR;
      break;

    case RSHIFT_EXPR:
      operator = BINOP_RSH;
      break;

    case LSHIFT_EXPR:
      operator = BINOP_LSH;
      break;

    case AND_AND_EXPR:
      operator = BINOP_LOGICAL_AND;
      break;

    case OR_OR_EXPR:
      operator = BINOP_LOGICAL_OR;
      break;

    case EQ_EQ_EXPR:
      operator = BINOP_EQUAL;
      break;

    case NOT_EQ_EXPR:
      operator = BINOP_NOTEQUAL;
      break;

    case GREATER_EXPR:
      operator = BINOP_GTR;
      break;

    case LESS_EXPR:
      operator = BINOP_LESS;
      break;

    case GREATER_EQ_EXPR:
      operator = BINOP_GEQ;
      break;

    case LESS_EQ_EXPR:
      operator = BINOP_LEQ;
      break;

    default:
      error (_("unhandled operator in build_binary_op"));
    }

  /* All operators L -> R (not true for all operators?)
     So append to LHS and return LHS.  */
  cp_expression_append_chain (lhs, rhs);
  write_exp_elt_opcode (operator);
  lhs = cp_expression_chain (lhs);

  /* We no longer need RHS, so free its memory.  */
  free_expression_chain (rhs);

  return lhs;
}

static cp_expression *
cp_parse_binary_expression (cp_parser *parser, enum cp_precedence prec)
{
  cp_expression_stack stack;
  cp_expression_stack_entry *sp = &stack[0];
  cp_token token;
  enum cp_precedence new_prec, lookahead_prec;
  cp_expression *lhs, *rhs;
  enum expr_code operator;

  lhs = cp_cast_expression (parser);
  for (;;)
    {
      token = cp_peek_token (parser);
      new_prec = binary_ops_token[token.type].prec;

      if (new_prec <= prec)
	{
	  if (sp == stack)
	    break;
	  else
	    goto pop;
	}

    get_rhs:

      operator = binary_ops_token[token.type].code;
      cp_consume_token (parser);

      rhs = cp_parse_simple_cast_expression (parser);

      token = cp_peek_token (parser);
      lookahead_prec = binary_ops_token[token.type].prec;
      if (lookahead_prec > new_prec)
	{
	  /* new token has higher precedence than old... */
	  sp->prec = prec;
	  sp->operator = operator;
	  sp->lhs = lhs;
	  sp++;
	  lhs = rhs;
	  prec = new_prec;
	  new_prec = lookahead_prec;
	  goto get_rhs;

	pop:
	  lookahead_prec = new_prec;
	  /* If the stack is not empty, LHS holds the right side of a
	     previously suspended expression.  */
	  --sp;
	  prec = sp->prec;
	  operator = sp->operator;
	  rhs = lhs;
	  lhs = sp->lhs;
	}

      lhs = build_binary_op (lhs, rhs, operator);
    }

  return lhs;
}

static cp_expression *
cp_parse_expression (cp_parser *parser)
{
  cp_token token = cp_peek_token (parser);

  switch (token.type)
    {
      /* Unary operators  */
    case TTYPE_MULT:
      cp_consume_token (parser);
      cp_parse_expression (parser);
      write_exp_elt_opcode (UNOP_IND);
      break;

    case TTYPE_AND:
      cp_consume_token (parser);
      cp_parse_expression (parser);
      write_exp_elt_opcode (UNOP_ADDR);
      break;

    case TTYPE_MINUS:
      cp_consume_token (parser);
      cp_parse_expression (parser);
      write_exp_elt_opcode (UNOP_NEG);
      break;

    case TTYPE_PLUS:
      cp_consume_token (parser);
      cp_parse_expression (parser);
      write_exp_elt_opcode (UNOP_PLUS);
      break;

    case TTYPE_NOT:
      cp_consume_token (parser);
      cp_parse_expression (parser);
      write_exp_elt_opcode (UNOP_LOGICAL_NOT);
      break;

    case TTYPE_COMPL:
      cp_consume_token (parser);
      cp_parse_expression (parser);
      write_exp_elt_opcode (UNOP_COMPLEMENT);
      break;

    case TTYPE_EOF:
      /* Need better error handling?  */
      error (_("unexpected end-of-line while parsing input"));

    default:
      return cp_parse_binary_expression (parser, PREC_NOT_OPERATOR);
    }

  return NULL;
}


static cp_token
cp_lex_number (cp_parser *parser)
{
  const char *start = --parser->buffer.cur;
  const char *p = start;
  int hex = input_radix > 10;
  int got_dot = 0, got_e = 0;
  cp_token result;

  if (*p == '0' && (p[1] == 'x' || p[1] == 'X'))
    {
      p += 2;
      hex = 1;
    }
  else if (*p == '0' && (p[1]=='t' || p[1]=='T' || p[1]=='d' || p[1]=='D'))
    {
      p += 2;
      hex = 0;
    }

  for ( ; *p != '\000'; ++p)
    {
      /* This test includes !hex because 'e' is a valid hex digit
	 and thus does not indicate a floating point number when
	 the radix is hex.  */
      if (!hex && !got_e && (*p == 'e' || *p == 'E'))
	got_dot = got_e = 1;
      /* This test does not include !hex, because a '.' always indicates
	 a decimal floating point number regardless of the radix.  */
      else if (!got_dot && *p == '.')
	got_dot = 1;
      else if (got_e && (p[-1] == 'e' || p[-1] == 'E')
	       && (*p == '-' || *p == '+'))
	/* This is the sign of the exponent, not the end of the
	   number.  */
	continue;
      /* We will take any letters or digits.  parse_number will
	 complain if past the radix, or if L or U are not final.  */
      else if ((*p < '0' || *p > '9')
	       && ((*p < 'a' || *p > 'z')
		   && (*p < 'A' || *p > 'Z')))
	break;
    }

  result.type = TTYPE_NUMBER;
  result.value = savestring (start, p - start);
  parser->buffer.cur += p - start;
  return result;
}

#define IF_NEXT_IS(CHAR, THEN_TYPE, ELSE_TYPE)	\
  do {						\
      result.type = ELSE_TYPE;			\
      if (*parser->buffer.cur == CHAR)		\
	{					\
	  parser->buffer.cur++;			\
	  result.type = THEN_TYPE;		\
	}					\
  } while (0)

static cp_token
cp_lex_one_token (cp_parser *parser)
{
  char c;
  cp_token result;
  result.value = 0;

 retry:
  c = *parser->buffer.cur++;

  switch (c)
    {
    case ' ': case '\t': case '\n':
      goto retry;

    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
      result = cp_lex_number (parser);
      break;

    case '*':
      result.type = TTYPE_MULT;
      break;

    case '/':
      result.type = TTYPE_DIV;
      break;

    case '+':
      result.type = TTYPE_PLUS;
      break;

    case '-':
      IF_NEXT_IS ('=', TTYPE_MINUS_EQ, TTYPE_MINUS);
      break;

    case '%':
      IF_NEXT_IS ('=', TTYPE_MOD_EQ, TTYPE_MOD);
      break;

    case '!':
      IF_NEXT_IS ('=', TTYPE_NOT_EQ, TTYPE_NOT);
      break;

    case '~':
      result.type = TTYPE_COMPL;
      break;

    case '&':
      IF_NEXT_IS ('&', TTYPE_AND_AND, TTYPE_AND);
      break;

    case '|':
      IF_NEXT_IS ('|', TTYPE_OR_OR, TTYPE_OR);
      break;

    case '=':
      IF_NEXT_IS ('=', TTYPE_EQ_EQ, TTYPE_EQ);
      break;

    case '>':
      if (*parser->buffer.cur == '=')
	{
	  result.type = TTYPE_GREATER_EQ;
	  parser->buffer.cur++;
	}
      else if (*parser->buffer.cur == '>')
	{
	  parser->buffer.cur++;
	  IF_NEXT_IS ('=', TTYPE_RSHIFT_EQ, TTYPE_RSHIFT);
	}
      else
	result.type = TTYPE_GREATER;
      break;

    case '<':
      if (*parser->buffer.cur == '=')
	{
	  result.type = TTYPE_LESS_EQ;
	  parser->buffer.cur++;
	}
      else if (*parser->buffer.cur == '<')
	{
	  parser->buffer.cur++;
	  IF_NEXT_IS ('=', TTYPE_LSHIFT_EQ, TTYPE_LSHIFT);
	}
      else
	result.type = TTYPE_LESS;
      break;

    case 0:
      --parser->buffer.cur;
      result = EOF_token;
      break;

    default:
      /* we should only get here if we did not implement something
	 above.  */
      error (_("unknown lexer token"));
  }

  return result;
}

static void
cp_lex (cp_parser *parser)
{
  cp_token token;

  do
    {
      token = cp_lex_one_token (parser);
      cp_push_token (parser, token);
    }
  while (!cp_is_eof_token (token));
}

#if 1
static void
_cp_dump_token_stream (const cp_parser *parser)
{
  int i;
  cp_token *elt;
  printf ("token stream:\n");
  for (i = 0; VEC_iterate (cp_token, parser->token_fifo, i, elt); ++i)
    printf ("\t[%d] token = %s, value = \"%s\"\n", i,
	    token_table_strings[(int) elt->type], elt->value);
}
#endif

int
c_parse (void)
{
  char *start = lexptr;
  cp_expression *expr;
  cp_parser parser;

  parser.buffer.buffer = start;
  parser.buffer.cur = parser.buffer.buffer;
  parser.token_fifo = 0;

  cp_lex (&parser);
  _cp_dump_token_stream (&parser);

  lexptr += parser.buffer.cur - start;
  expr = cp_parse_expression (&parser);
  expout = expr->exp;
  expout_size = expr->size;
  expout_ptr = expr->ptr;
  xfree (expr);

  return 0;
}

void
c_error (char *msg)
{
  char *lexptr = "unknown";
  error ("A %s in expression, near %s'.", (msg ? msg : "error"), lexptr);
}

void
_initialize_cparser (void)
{
  int i;

  for (i = 0; i < sizeof (binary_ops) / sizeof (binary_ops[0]); ++i)
    binary_ops_token[binary_ops[i].token_type] = binary_ops[i];
}
