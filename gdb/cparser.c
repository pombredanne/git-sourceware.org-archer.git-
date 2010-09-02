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
#include "objfiles.h"
#include "parser-defs.h"
#include "vec.h"

#define parse_type builtin_type (parse_gdbarch)

/* Lexer token table:
   OP(name, string description): operator tokens
   TK(name, string description): parser tokens
*/
#define TOKEN_TABLE				\
  OP(EQ,		"=")			\
  OP(NOT,		"!")			\
  OP(GREATER,		">")			\
  OP(LESS,		"<")			\
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
  OP(GREATER_EQ,	">=")			\
  OP(LESS_EQ,		"<=")			\
  OP(PLUS_EQ,		"+=")			\
  OP(MINUS_EQ,		"-=")			\
  OP(MULT_EQ,		"*=")			\
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
  TK(STRING,		LITERAL)		\
  TK(KEYWORD,           NONE)

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

typedef enum cp_keyword
 {
   KEYWORD_UNSIGNED,
   KEYWORD_TEMPLATE,
   KEYWORD_VOLATILE,
   KEYWORD_STRUCT,
   KEYWORD_SIGNED,
   KEYWORD_SIZEOF,
   KEYWORD_DOUBLE,
   KEYWORD_FALSE,
   KEYWORD_CLASS,
   KEYWORD_UNION,
   KEYWORD_SHORT,
   KEYWORD_CONST,
   KEYWORD_ENUM,
   KEYWORD_LONG,
   KEYWORD_TRUE,
   KEYWORD_INT,
   KEYWORD_NEW,
   KEYWORD_DELETE,
   KEYWORD_OPERATOR, /* ?? */

   KEYWORD_AND,
   KEYWORD_AND_EQ,
   KEYWORD_BITAND,
   KEYWORD_BITOR,
   KEYWORD_COMPL,
   KEYWORD_NOT,
   KEYWORD_NOT_EQ,
   KEYWORD_OR,
   KEYWORD_OR_EQ,
   KEYWORD_XOR,
   KEYWORD_XOR_EQ,

   KEYWORD_CONST_CAST,
   KEYWORD_DYNAMIC_CAST,
   KEYWORD_STATIC_CAST,
   KEYWORD_REINTERPRET_CAST,
   KEYWORD_MAX /* a marker */
 } cp_keyword;

typedef unsigned char token_value;
typedef struct cp_token
{
  token_type type;
  cp_keyword keyword;
  token_value *value;		/* Must be free'd (necesary?) */
} cp_token;

typedef struct cp_token_list
{
  cp_token *token;
  struct cp_token_list *next;
} cp_token_list;

/* A token signifying the end of input.  */
static cp_token *EOF_token;

/* A buffer for lexing the input.  */
typedef struct
{
  /* The actual buffer holding the input.  */
  const char *buffer;

  /* A pointer to the current lexing location.  */
  const char *cur;
} cp_buffer;

typedef struct
{
  /* Buffer used for lexing  */
  cp_buffer buffer;

  /* The token stream  */
  cp_token_list *tokens;
  cp_token_list *head;

  /* Parsing language  */
  unsigned char language;
} cp_lexer;

/* An expression "chain" which wraps the parser globals
   expout, expout_size, and expout_ptr.  */
typedef struct cp_expression
{
  /* The actual expression  */
  struct expression *exp;

  /* The allocated number of elements in the expression.  */
  int size;

  /* A pointer to the next free slot in the expression.  */
  int ptr;
} cp_expression;

/* A parser instance  */
typedef struct
{
  /* The lexer  */
  cp_lexer *lexer;

  cp_expression *scope;
  cp_expression *qualifying_scope;
  cp_expression *object_scope;
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
  ERROR_CODE,
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
  INDIRECT_REF,
  ADDR_EXPR,
  PREINCREMENT_EXPR,
  PREDECREMENT_EXPR,
  UNARY_PLUS_EXPR,
  NEGATE_EXPR,
  TRUTH_NOT_EXPR,
  BIT_NOT_EXPR
};

/* An expression representing the global namespace.  */
static cp_expression *global_namespace;

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

  /* A cleanup for the expression being suspended.  */
  struct cleanup *cleanup;
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

struct reserved_keywords
{
  const char *name;
  cp_keyword keyword;
  unsigned char valid_languages;
};

#define CP_LANGUAGE_C 0x1
#define CP_LANGUAGE_CPLUS 0x2
#define CP_LANGUAGE_ALL_C (CP_LANGUAGE_C | CP_LANGUAGE_CPLUS)

static const struct reserved_keywords reserved_keywords[] =
{
  {"unsigned", KEYWORD_UNSIGNED, CP_LANGUAGE_ALL_C},
  {"template", KEYWORD_TEMPLATE, CP_LANGUAGE_CPLUS},
  {"volatile", KEYWORD_VOLATILE, CP_LANGUAGE_ALL_C},
  {"struct", KEYWORD_STRUCT, CP_LANGUAGE_ALL_C},
  {"signed", KEYWORD_SIGNED, CP_LANGUAGE_ALL_C},
  {"sizeof", KEYWORD_SIZEOF, CP_LANGUAGE_ALL_C},
  {"double", KEYWORD_DOUBLE, CP_LANGUAGE_ALL_C},
  {"false", KEYWORD_FALSE, CP_LANGUAGE_CPLUS},
  {"class", KEYWORD_CLASS, CP_LANGUAGE_CPLUS},
  {"union", KEYWORD_UNION, CP_LANGUAGE_ALL_C},
  {"short", KEYWORD_SHORT, CP_LANGUAGE_ALL_C},
  {"const", KEYWORD_CONST, CP_LANGUAGE_ALL_C},
  {"enum", KEYWORD_ENUM, CP_LANGUAGE_ALL_C},
  {"long", KEYWORD_LONG, CP_LANGUAGE_ALL_C},
  {"true", KEYWORD_TRUE, CP_LANGUAGE_CPLUS},
  {"int", KEYWORD_INT, CP_LANGUAGE_ALL_C},
  {"new", KEYWORD_NEW, CP_LANGUAGE_CPLUS},
  {"delete", KEYWORD_DELETE, CP_LANGUAGE_CPLUS},
  {"operator", KEYWORD_OPERATOR, CP_LANGUAGE_CPLUS},

  {"and", KEYWORD_AND, CP_LANGUAGE_CPLUS},
  {"and_eq", KEYWORD_AND_EQ, CP_LANGUAGE_CPLUS},
  {"bitand", KEYWORD_BITAND, CP_LANGUAGE_CPLUS},
  {"bitor", KEYWORD_BITOR, CP_LANGUAGE_CPLUS},
  {"compl", KEYWORD_COMPL, CP_LANGUAGE_CPLUS},
  {"not", KEYWORD_NOT, CP_LANGUAGE_CPLUS},
  {"not_eq", KEYWORD_NOT_EQ, CP_LANGUAGE_CPLUS},
  {"or", KEYWORD_OR, CP_LANGUAGE_CPLUS},
  {"or_eq", KEYWORD_OR_EQ, CP_LANGUAGE_CPLUS},
  {"xor", KEYWORD_XOR, CP_LANGUAGE_CPLUS},
  {"xor_eq", KEYWORD_XOR_EQ, CP_LANGUAGE_CPLUS},

  {"const_cast", KEYWORD_CONST_CAST, CP_LANGUAGE_CPLUS},
  {"dynamic_cast", KEYWORD_DYNAMIC_CAST, CP_LANGUAGE_CPLUS},
  {"static_cast", KEYWORD_STATIC_CAST, CP_LANGUAGE_CPLUS},
  {"reinterpret_cast", KEYWORD_REINTERPRET_CAST, CP_LANGUAGE_CPLUS}
};

/* Returns the current expression chain (from expout).  If the argument
   is non-NULL, then the current chain is appended to it.

   In both cases, expout will be reset.  */
static cp_expression *cp_parse_expression (cp_parser *);

static cp_expression *cp_cast_expression (cp_parser *);



/* Allocate a new CP_TOKEN.  */

static cp_token *
new_token (void)
{
  return (cp_token *) xcalloc (1, sizeof (cp_token));
}

/* Push TOKEN onto LEXER's token stream.  */

static void
cp_lexer_push_token (cp_lexer *lexer, cp_token *token)
{
  cp_token_list *l;
  cp_token_list *new = (cp_token_list *) xmalloc (sizeof (cp_token_list));
  new->token = token;
  new->next = NULL;

  if (lexer->tokens == NULL)
    {
      lexer->tokens = new;
      lexer->head = new;
    }
  else
    {
      for (l = lexer->tokens; l->next != NULL; l = l->next)
        ;
      l->next = new;
    }
}

/* Returns the next token in the token stream.  It is not removed from
   the stream until cp_lexer_consume_token is called.  */

static cp_token *
cp_lexer_peek_token (const cp_lexer *lexer)
{
  if (lexer->tokens == NULL)
    return EOF_token;
  return lexer->tokens->token;
}

/* Advance the token stream past the current token.  */

static cp_token *
cp_lexer_consume_token (cp_lexer *lexer)
{
  cp_token_list *head = lexer->tokens;
  lexer->tokens = head->next;
  return head->token;
}

/* Returns true if the next token in LEXER has the given TYPE.  */

static int
cp_lexer_next_token_is (cp_lexer *lexer, token_type type)
{
  return (cp_lexer_peek_token (lexer)->type == type);
}

/* Returns true if the next token is the EOF marker token.  */

static int
cp_is_eof_token (cp_token *token)
{
  return (token == EOF_token);
}



/* Append the SRC expression chain to DEST.  */

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
free_expression_chain (void *chain)
{
  cp_expression *expr = (cp_expression *) chain;
  xfree (expr->exp);
  xfree (expr);
}

/* Returns an expression chain containing the current contents of EXPOUT.
   If CHAIN is non-NULL, the EXPOUT is copied into it instead.  */

static cp_expression *
cp_expression_chain (cp_expression *chain)
{
  if (chain == NULL)
    {
      /* Create a new chain for the expression in EXPOUT.  */
      chain = (cp_expression *) xmalloc (sizeof (cp_expression));
      chain->size = expout_size;
      chain->ptr = expout_ptr;
      chain->exp = expout;
      chain->exp->nelts = expout_ptr;

      /* Reset globals for next expression  */
      expout_size = 10;
      expout_ptr = 0;
      expout = (struct expression *)
	xmalloc (sizeof (struct expression) + EXP_ELEM_TO_BYTES (expout_size));
      expout->language_defn = current_language;
      expout->gdbarch = get_current_arch ();
    }
  else
    {
      /* Append the expression in EXPOUT to CHAIN.   */
      cp_expression tmp;
      tmp.size = expout_size;
      tmp.ptr = expout_ptr;
      tmp.exp = expout;
      tmp.exp->nelts = expout_ptr;
      cp_expression_append_chain (chain, &tmp);

      /* Reset globals for next expression.  When appending a chain to
	 another chain, the contents of expout are copied into the
	 destination chain, so there is no need to allocate new memory
	 for expout.  Simply reset the pointer to zero.  */
      expout_ptr = 0;
    }

  return chain;
}

/* A helper function to parse numbers from token values.  */

static void
parse_number (const cp_parser *parser, token_value *value,
	      cp_typed_number *result)
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

/* Print out an appropriate error message for an expected missing
   token of type TYPE.  */

static void
cp_required_token_error (cp_parser *parser, token_type type)
{
  /* keiths-FIXME: This is not really sufficiently user-friendly.  */
  error (_("syntax error: expected %s"), token_table_strings[(int) type]);
}

/* If the next token in PARSER is not of type TYPE, throw an error.
   Otherwise, consume the token and return the next token.  */

static cp_token *
cp_require_token (cp_parser *parser, token_type type)
{
  if (cp_lexer_next_token_is (parser->lexer, type))
    return cp_lexer_consume_token (parser->lexer);

  cp_required_token_error (parser, type);
  return NULL;
}

static cp_expression *
cp_parse_identifier (cp_parser *parser)
{
  cp_token *token = cp_require_token (parser, TTYPE_NAME);
  if (token != NULL)
    {
      /* FIXME: Can we use parser->scope? */
      struct symbol *sym = lookup_symbol (token->value,
					  expression_context_block, VAR_DOMAIN,
					  NULL);
      if (sym != NULL)
	{
	  write_exp_elt_opcode (OP_VAR_VALUE);
	  write_exp_elt_block (expression_context_block);
	  write_exp_elt_sym (sym);
	  write_exp_elt_opcode (OP_VAR_VALUE);
	  return cp_expression_chain (NULL);
	}
      else
	{
	  struct minimal_symbol *msym;
	  msym = lookup_minimal_symbol (token->value, NULL, NULL);
	  if (msym != NULL)
	    {
	      write_exp_msymbol (msym);
	      return cp_expression_chain (NULL);
	    }
	  else if (!have_full_symbols () && !have_partial_symbols ())
	    error (_("No symbol table is loaded.  Use the \"file\" command."));
	  else
	    error (_("No symbol \"%s\" in current context."), token->value);
	}
    }

  return NULL;
}

static cp_expression *
cp_parse_global_scope_opt (cp_parser *parser, int current_scope_valid_p)
{
  cp_token *token = cp_lexer_peek_token (parser->lexer);
  if (token->type == TTYPE_SCOPE)
    {
      cp_lexer_consume_token (parser->lexer);
      parser->scope = global_namespace;
      parser->qualifying_scope = global_namespace;
      parser->object_scope = NULL;

      return parser->scope;
    }
  else if (!current_scope_valid_p)
    {
      parser->scope = NULL;
      parser->qualifying_scope = NULL;
      parser->object_scope = NULL;
    }

  return NULL;
}

static cp_expression *
cp_parse_unqualified_id (cp_parser *parser, int optional_p)
{
  cp_token *token = cp_lexer_peek_token (parser->lexer);

  switch (token->type)
    {
    case TTYPE_NAME:
      return cp_parse_identifier (parser);

    default:
      if (optional_p)
	return NULL;
      error (_("expected unqualified-id"));
    }
}

static cp_expression *
cp_parse_id_expression (cp_parser *parser, int optional_p)
{
  int global_scope_p
    = (cp_parse_global_scope_opt (parser, /*current_scope_valid_p=*/ 0)
       != NULL);

  if (global_scope_p)
    {
      cp_token *token = cp_lexer_peek_token (parser->lexer);
      if (token->type == TTYPE_NAME)
	return cp_parse_identifier (parser);
    }

  return cp_parse_unqualified_id (parser, optional_p);
}

static cp_expression *
cp_parse_primary_expression (cp_parser *parser)
{
  cp_token *token = cp_lexer_peek_token (parser->lexer);
  switch (token->type)
    {
    case TTYPE_NUMBER:
      {
	int r;
	cp_typed_number number;
	parse_number (parser, token->value, &number);
	cp_lexer_consume_token (parser->lexer);

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
	   we need to save this and reset expout.  */
	return cp_expression_chain (NULL);
      }
      break;

    case TTYPE_NAME:
      return cp_parse_id_expression (parser, /*optional_p=*/ 0);

    default:
      error (_("expected primary expression"));
    }

  /* probably want a special marker like g++ does */
  return NULL;
}

static cp_expression *
cp_parse_postfix_open_square_expression (cp_parser *parser, cp_expression *expr)
{
  cp_lexer_consume_token (parser->lexer);
  cp_expression_append_chain (expr, cp_parse_expression (parser));
  cp_require_token (parser, TTYPE_CLOSE_SQUARE);
  write_exp_elt_opcode (BINOP_SUBSCRIPT);
  return cp_expression_chain (expr);
}

static cp_expression *
cp_parse_postfix_expression (cp_parser *parser)
{
  cp_expression *expr;
  cp_token *token = cp_lexer_peek_token (parser->lexer);

  switch (token->type)
    {
    default:
      expr = cp_parse_primary_expression (parser);
      break;
    }

  while (1)
    {
      token = cp_lexer_peek_token (parser->lexer);

      switch (token->type)
	{
	case TTYPE_OPEN_SQUARE:
	  expr = cp_parse_postfix_open_square_expression (parser, expr);
	  break;

	case TTYPE_OPEN_PAREN:
	  break;

	case TTYPE_DOT:
	case TTYPE_DEREF:
	  break;

	case TTYPE_PLUS_PLUS:
	  break;

	case TTYPE_MINUS_MINUS:
	  break;

	default:
	  return expr;
	}
    }

  error (_("should not get here"));
  return NULL;
}

static enum expr_code
cp_unary_operator (cp_token *token)
{
  switch (token->type)
    {
    case TTYPE_MULT:
      return INDIRECT_REF;

    case TTYPE_AND:
      return ADDR_EXPR;

    case TTYPE_PLUS:
      return UNARY_PLUS_EXPR;

    case TTYPE_MINUS:
      return NEGATE_EXPR;

    case TTYPE_NOT:
      return TRUTH_NOT_EXPR;

    case TTYPE_COMPL:
      return BIT_NOT_EXPR;

    default:
      return ERROR_CODE;
    }
}

static cp_expression *
cp_parse_unary_expression (cp_parser *parser)
{
  cp_token *token;
  enum expr_code unary_operator;

  token  = cp_lexer_peek_token (parser->lexer);
  if (token->type == TTYPE_KEYWORD)
    {
      ;
    }

  if (cp_lexer_next_token_is (parser->lexer, TTYPE_SCOPE))
    {
      ;
    }

  unary_operator = cp_unary_operator (token);
  if (unary_operator == ERROR_CODE)
    {
      if (token->type == TTYPE_PLUS_PLUS)
	unary_operator = PREINCREMENT_EXPR;
      else if (token->type == TTYPE_MINUS_MINUS)
	unary_operator = PREDECREMENT_EXPR;
      /* GNU extension TTYPE_AND_AND? */
    }

  if (unary_operator != ERROR_CODE)
    {
      cp_expression *cast_expr;
      enum exp_opcode operator;

      token = cp_lexer_consume_token (parser->lexer);
      cast_expr = cp_cast_expression (parser);
      switch (unary_operator)
	{
	case INDIRECT_REF:
	  operator = UNOP_IND;
	  break;

	case ADDR_EXPR:
	  operator = UNOP_ADDR;
	  break;

	case BIT_NOT_EXPR:
	  operator = UNOP_COMPLEMENT;
	  break;

	case PREINCREMENT_EXPR:
	  operator = UNOP_PREINCREMENT;
	  break;

	case PREDECREMENT_EXPR:
	  operator = UNOP_PREDECREMENT;
	  break;

	case UNARY_PLUS_EXPR:
	  operator = UNOP_PLUS;
	  break;

	case NEGATE_EXPR:
	  operator = UNOP_NEG;
	  break;

	case TRUTH_NOT_EXPR:
	  operator = UNOP_LOGICAL_NOT;
	  break;

	default:
	  internal_error (__FILE__, __LINE__, _("unreachable statement!"));
	}

      write_exp_elt_opcode (operator);
      return cp_expression_chain (cast_expr);
    }

  return cp_parse_postfix_expression (parser);
}

static cp_expression *
cp_cast_expression (cp_parser *parser)
{
  /* determine if parser is looking at a cast of some sort */
  if (cp_lexer_next_token_is (parser->lexer, TTYPE_OPEN_PAREN))
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

  return lhs;
}

static cp_expression *
cp_parse_binary_expression (cp_parser *parser, enum cp_precedence prec)
{
  struct cleanup *back_to, *cleanup;
  cp_expression_stack stack;
  cp_expression_stack_entry *sp = &stack[0];
  cp_token *token;
  enum cp_precedence new_prec, lookahead_prec;
  cp_expression *lhs, *rhs;
  enum expr_code operator;

  lhs = cp_cast_expression (parser);
  back_to = make_cleanup (free_expression_chain, lhs);

  for (;;)
    {
      token = cp_lexer_peek_token (parser->lexer);
      new_prec = binary_ops_token[token->type].prec;

      if (new_prec <= prec)
	{
	  if (sp == stack)
	    break;
	  else
	    goto pop;
	}

    get_rhs:

      operator = binary_ops_token[token->type].code;
      cp_lexer_consume_token (parser->lexer);

      rhs = cp_parse_simple_cast_expression (parser);
      cleanup = make_cleanup (free_expression_chain, rhs);

      token = cp_lexer_peek_token (parser->lexer);
      lookahead_prec = binary_ops_token[token->type].prec;
      if (lookahead_prec > new_prec)
	{
	  /* new token has higher precedence than old... */
	  sp->prec = prec;
	  sp->operator = operator;
	  sp->lhs = lhs;
	  sp->cleanup = cleanup;
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
	  cleanup = sp->cleanup;
	}

      lhs = build_binary_op (lhs, rhs, operator);
      do_cleanups (cleanup);
    }

  discard_cleanups (back_to);
  return lhs;
}

static cp_expression *
cp_parse_expression (cp_parser *parser)
{
  return cp_parse_binary_expression (parser, PREC_NOT_OPERATOR);
}



static void
cp_lex_number (cp_lexer *lexer, cp_token *result)
{
  const char *start = --lexer->buffer.cur;
  const char *p = start;
  int hex = input_radix > 10;
  int got_dot = 0, got_e = 0;

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

  result->type = TTYPE_NUMBER;
  result->value = savestring (start, p - start);
  lexer->buffer.cur += p - start;
}

#define IF_NEXT_IS(CHAR, THEN_TYPE, ELSE_TYPE)	\
  do {						\
      result->type = ELSE_TYPE;			\
      if (*lexer->buffer.cur == CHAR)		\
	{					\
	  ++lexer->buffer.cur;			\
	  result->type = THEN_TYPE;		\
	}					\
  } while (0)

#define IF_NEXT_IS_ELSE_IF(CHAR_THEN, THEN_TYPE,	\
			   CHAR_ELSE_IF, ELSE_IF_TYPE,	\
			   ELSE_TYPE)			\
  do {							\
      result->type = ELSE_TYPE;				\
      if (*lexer->buffer.cur == CHAR_THEN)		\
	{						\
	  ++lexer->buffer.cur;				\
	  result->type = THEN_TYPE;			\
	}						\
      else if (*lexer->buffer.cur == CHAR_ELSE_IF)	\
	{						\
	  ++lexer->buffer.cur;				\
	  result->type = ELSE_IF_TYPE;			\
	}						\
  } while (0)

static cp_token *
cp_lex_one_token (cp_lexer *lexer)
{
  char c;
  cp_token *result = new_token ();

 retry:
  c = *lexer->buffer.cur++;

  switch (c)
    {
    case ' ': case '\t': case '\n':
      goto retry;

    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
      cp_lex_number (lexer, result);
      break;

    case '*':
      IF_NEXT_IS ('=', TTYPE_MULT_EQ, TTYPE_MULT);
      break;

    case '/':
      IF_NEXT_IS ('=', TTYPE_DIV_EQ, TTYPE_DIV);
      break;

    case '+':
      IF_NEXT_IS_ELSE_IF ('+', TTYPE_PLUS_PLUS,
			  '=', TTYPE_PLUS_EQ,
			  TTYPE_PLUS);
      break;

    case '-':
      if (*lexer->buffer.cur == '-')
	{
	  ++lexer->buffer.cur;
	  result->type = TTYPE_MINUS_MINUS;
	}
      else if (*lexer->buffer.cur == '>')
	{
	  ++lexer->buffer.cur;
	  IF_NEXT_IS ('*', TTYPE_DEREF_STAR, TTYPE_DEREF);
	}
      else if (*lexer->buffer.cur == '=')
	{
	  ++lexer->buffer.cur;
	  result->type = TTYPE_MINUS_EQ;
	}
      else
	result->type = TTYPE_MINUS;
      break;

    case '%':
      IF_NEXT_IS ('=', TTYPE_MOD_EQ, TTYPE_MOD);
      break;

    case '!':
      IF_NEXT_IS ('=', TTYPE_NOT_EQ, TTYPE_NOT);
      break;

    case '~':
      result->type = TTYPE_COMPL;
      break;

    case '&':
      IF_NEXT_IS_ELSE_IF ('&', TTYPE_AND_AND,
			  '=', TTYPE_AND_EQ,
			  TTYPE_AND);
      break;

    case '|':
      IF_NEXT_IS_ELSE_IF ('|', TTYPE_OR_OR,
			  '=', TTYPE_OR_EQ,
			  TTYPE_OR);
      break;

    case '=':
      IF_NEXT_IS ('=', TTYPE_EQ_EQ, TTYPE_EQ);
      break;

    case '>':
      if (*lexer->buffer.cur == '=')
	{
	  ++lexer->buffer.cur;
	  result->type = TTYPE_GREATER_EQ;
	}
      else if (*lexer->buffer.cur == '>')
	{
	  ++lexer->buffer.cur;
	  IF_NEXT_IS ('=', TTYPE_RSHIFT_EQ, TTYPE_RSHIFT);
	}
      else
	result->type = TTYPE_GREATER;
      break;

    case '<':
      if (*lexer->buffer.cur == '=')
	{
	  ++lexer->buffer.cur;
	  result->type = TTYPE_LESS_EQ;
	}
      else if (*lexer->buffer.cur == '<')
	{
	  ++lexer->buffer.cur;
	  IF_NEXT_IS ('=', TTYPE_LSHIFT_EQ, TTYPE_LSHIFT);
	}
      else
	result->type = TTYPE_LESS;
      break;

    case '?':
      result->type = TTYPE_QUERY;
      break;

    case ':':
      IF_NEXT_IS (':', TTYPE_SCOPE, TTYPE_COLON);
      break;

    case ',':
      result->type = TTYPE_COMMA;
      break;

    case '(':
      result->type = TTYPE_OPEN_PAREN;
      break;

    case ')':
      result->type = TTYPE_CLOSE_PAREN;
      break;

    case '[':
      result->type = TTYPE_OPEN_SQUARE;
      break;

    case ']':
      result->type = TTYPE_CLOSE_SQUARE;
      break;

    case '{':
      result->type = TTYPE_OPEN_BRACE;
      break;

    case '}':
      result->type = TTYPE_CLOSE_BRACE;
      break;

    case '.':
      IF_NEXT_IS ('*', TTYPE_DOT_STAR, TTYPE_DOT);
      break;

    case '@':
      result->type = TTYPE_ATSIGN;
      break;

    case 'L':
    case 'u':
    case 'U':
    case 'R':
      /* Fall through  */

    case '_':
    case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
    case 'g': case 'h': case 'i': case 'j': case 'k': case 'l':
    case 'm': case 'n': case 'o': case 'p': case 'q': case 'r':
    case 's': case 't':           case 'v': case 'w': case 'x':
    case 'y': case 'z':
    case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
    case 'G': case 'H': case 'I': case 'J': case 'K':
    case 'M': case 'N': case 'O': case 'P': case 'Q':
    case 'S': case 'T':           case 'V': case 'W': case 'X':
    case 'Y': case 'Z':
      {
	int i, len;
	char *copy;
	struct stoken stoken;

	result->type = TTYPE_NAME;
	result->keyword = KEYWORD_MAX;

	/* Find the end of the word.  */
	len = 0;
	for (c = lexer->buffer.cur[len];
	     (c == '_' || c == '$' || (c >= '0' && c <= '9')
	      || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')); )
	  c = lexer->buffer.cur[++len];

	stoken.length = len + 1;
	stoken.ptr = (char *) (lexer->buffer.cur - 1);
	copy = copy_name (stoken);
	for (i = 0;
	     i < sizeof (reserved_keywords) / sizeof (reserved_keywords[0]);
	     ++i)
	  {
	    if (lexer->language & reserved_keywords[i].valid_languages
		&& strcmp (reserved_keywords[i].name, copy) == 0)
	      {
		result->type = TTYPE_KEYWORD;
		result->keyword = reserved_keywords[i].keyword;
	      }
	  }
	lexer->buffer.cur += len;
	result->value = savestring (stoken.ptr, stoken.length);
      }
    break;

    case 0:
      --lexer->buffer.cur;
      xfree (result);
      result = EOF_token;
      break;

    default:
      /* we should only get here if we did not implement something
	 above.  */
      xfree (result);
      error (_("unknown lexer token"));
  }

  return result;
}

static void
cp_lex (cp_lexer *lexer)
{
  cp_token *token;
  do
    {
      token = cp_lex_one_token (lexer);
      cp_lexer_push_token (lexer, token);
    }
  while (!cp_is_eof_token (token));
}

#if 1
static void
_cp_dump_token_stream (const cp_lexer *lexer)
{
  if (lexer->tokens == NULL)
    printf ("\t<empty>\n");
  else
    {
      int i;
      cp_token_list *l;
      for (i = 0, l = lexer->tokens; l != NULL; l = l->next)
        {
          cp_token *elt = l->token;
          printf ("\t[%d] token = %s, value = \"%s\"\n", i++,
                  token_table_strings[(int) elt->type], elt->value);
        }
    }
}
#endif

static void
free_cp_parser (void *parser_ptr)
{
  cp_parser *parser = (cp_parser *) parser_ptr;
  cp_token_list *l = parser->lexer->head;

  while (l != NULL)
    {
      cp_token_list *p = l;
      l = l->next;
      if (p->token != EOF_token)
        {
          if (p->token->value != NULL)
            xfree (p->token->value);
          xfree (p->token);
        }
      xfree (p);
    }

  /* free saved_tokens, if any (shouldn't be) */
  xfree (parser->lexer);
  xfree (parser);
}

static cp_parser *
new_parser (char *start)
{
  cp_parser *parser = xcalloc (1, sizeof (cp_parser));
  parser->lexer = (cp_lexer *) xcalloc (1, sizeof (cp_lexer));
  parser->lexer->buffer.buffer = start;
  parser->lexer->buffer.cur = parser->lexer->buffer.buffer;

  switch (parse_language->la_language)
    {
    case language_cplus:
      parser->lexer->language = CP_LANGUAGE_CPLUS;
      break;

    default:
      parser->lexer->language = CP_LANGUAGE_C;
      break;
    }

  return parser;
}

int
c_parse (void)
{
  char *start = lexptr;
  cp_expression *expr;
  struct cleanup *back_to;
  cp_parser *parser = new_parser (lexptr);

  back_to = make_cleanup (free_cp_parser, parser);

  /* Lex and parse input  */
  cp_lex (parser->lexer);
  _cp_dump_token_stream (parser->lexer);
  lexptr += parser->lexer->buffer.cur - start;

  /* Parse input and reset global variables  */
  expr = cp_parse_expression (parser);
  xfree (expout);
  expout = expr->exp;
  expout_size = expr->size;
  expout_ptr = expr->ptr;

  /* Free parser and expression memory  */
  do_cleanups (back_to);
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

  EOF_token = new_token ();
  EOF_token->type = TTYPE_EOF;
  global_namespace = xcalloc (1, sizeof (cp_expression));

  for (i = 0; i < sizeof (binary_ops) / sizeof (binary_ops[0]); ++i)
    binary_ops_token[binary_ops[i].token_type] = binary_ops[i];
}
