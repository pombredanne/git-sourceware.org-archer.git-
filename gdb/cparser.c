/* A recursive-descent C/C++ parser for GDB, the GNU debugger.

   This file is largely based on the GNU CC C++ parser (g++). 
   Simularities are not coincidental.

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
#include "arch-utils.h"
#include "block.h"
#include "expression.h"		/* Must appear before dfp.h.   */
#include "dfp.h"
#include "gdb_assert.h"
#include "gdb_string.h"
#include "gdbtypes.h"
#include "language.h"
#include "objfiles.h"
#include "parser-defs.h"
#include "symtab.h"
#include "value.h"
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
   KEYWORD_CHAR,
   KEYWORD_BOOL,
   KEYWORD_FLOAT,
   KEYWORD_VOID,
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
  token_value *value;
} cp_token;

typedef struct cp_token_list
{
  cp_token *token;
  struct cp_token_list *next;
} cp_token_list;
typedef cp_token_list *cp_saved_token_list;
DEF_VEC_P (cp_saved_token_list);

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

  /* Saved tokens  */
  VEC(cp_saved_token_list) *saved_tokens;
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

/* The status of a tentative parse.  */

typedef enum cp_parser_status_kind
{
  /* No errors have occurred.  */
  CP_PARSER_STATUS_KIND_NO_ERROR,

  /* An error has occurred.  */
  CP_PARSER_STATUS_KIND_ERROR,

  /* We are committed to this tentative parse, whether or not an error
     has occurred.  */
  CP_PARSER_STATUS_KIND_COMMITTED
} cp_parser_status_kind;

/* Context that is saved and restored when parsing tentatively.  */
typedef struct cp_parser_context
{
  /* If this is a tentative parsing context, the status of the
     tentative parse.  */
  enum cp_parser_status_kind status;

  /* If non-NULL, we have just seen a `x->' or `x.' expression.  Names
     that are looked up in this context must be looked up both in the
     scope given by OBJECT_TYPE (the type of `x' or `*x') and also in
     the context of the containing expression.  */
  struct type *object_type;

  /* The next parsing context in the stack.  */
  struct cp_parser_context *next;
} cp_parser_context;

/* Flags that are passed to some parsing functions.  These values can
   be bitwise-ored together.  */

enum
{
  /* No flags.  */
  CP_PARSER_FLAGS_NONE = 0x0,
  /* The construct is optional.  If it is not present, then no error
     should be issued.  */
  CP_PARSER_FLAGS_OPTIONAL = 0x1,
  /* When parsing a type-specifier, treat user-defined type-names
     as non-type identifiers.  */
  CP_PARSER_FLAGS_NO_USER_DEFINED_TYPES = 0x2,
  /* When parsing a type-specifier, do not try to parse a class-specifier
     or enum-specifier.  */
  CP_PARSER_FLAGS_NO_TYPE_DEFINITIONS = 0x4
};

/* This type is used for parameters and variables which hold
   combinations of the above flags.  */
typedef int cp_parser_flags;

/* A parser instance  */
typedef struct cp_parser
{
  /* The lexer  */
  cp_lexer *lexer;

  /* A stack of parsing contexts.  All but the bottom entry on the stack
     will be tentative contexts.

     We parse tentatively in order to determine which construct is in use
     in some situations.  */
  cp_parser_context *context;

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
  NOP_EXPR,
  MULT_EXPR,
  DIV_EXPR,
  MOD_EXPR,
  PLUS_EXPR,
  MINUS_EXPR,
  BIT_AND_EXPR,
  BIT_XOR_EXPR,
  BIT_IOR_EXPR,
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

/* The various kinds of non-integral constants we may encounter.  */
typedef enum non_integral_constant
{
  NIC_NONE,

  /* assignment operator */
  NIC_ASSIGNMENT,

#ifdef I_DONT_KNOW
  /* a cast */
  NIC_CAST,
#endif

  /* comma operator */
  NIC_COMMA
} non_integral_constant;

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

  { TTYPE_AND, BIT_AND_EXPR, PREC_AND_EXPRESSION },

  { TTYPE_OR, BIT_IOR_EXPR, PREC_INCLUSIVE_OR_EXPRESSION },

  { TTYPE_XOR, BIT_XOR_EXPR, PREC_EXCLUSIVE_OR_EXPRESSION },

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
  {"char", KEYWORD_CHAR, CP_LANGUAGE_ALL_C},
  {"bool", KEYWORD_BOOL, CP_LANGUAGE_CPLUS},
  {"float", KEYWORD_FLOAT, CP_LANGUAGE_ALL_C},
  {"void", KEYWORD_VOID, CP_LANGUAGE_ALL_C},
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

/* An individual decl-specifier.  */

typedef enum cp_decl_spec
{
  ds_first,
  ds_signed = ds_first,
  ds_unsigned,
  ds_short,
  ds_long,
  ds_const,
  ds_volatile,
  ds_restrict,
  ds_inline,
  ds_virtual,
  ds_explicit,
  ds_friend,
  ds_typedef,
  ds_constexpr,
  ds_complex,
  ds_thread,
  ds_last
} cp_decl_spec;


/* A decl-specifier-seq.  */
typedef struct cp_decl_specifier_seq
{
  /* The number of times each of the keywords has been seen.  */
  unsigned specs[(int) ds_last];

  /* The primary type, if any, given by the decl-specifier-seq.
     Modifiers like "short", "const", and "unsigned" are not recflected
     here. !! TRUE?? */
  struct type *type;
} cp_decl_specifier_seq;

/* An enumeration of the kind of tags that C++ accepts.  */
enum tag_types {
  /* Not a tag type  */
  none_type = 0,

  /* "struct" types  */
  struct_type,

  /* "class" types  */
  class_type,

  /* "union" types  */
  union_type,

  /* "enum" types  */
  enum_type
};

static cp_parser_context *cp_parser_context_free_list;

/* Returns the current expression chain (from expout).  If the argument
   is non-NULL, then the current chain is appended to it.

   In both cases, expout will be reset.  */
static cp_expression *cp_parse_expression (cp_parser *, int);

static cp_expression *cp_cast_expression (cp_parser *, int, int);



/* Allocate a new CP_TOKEN.  */

static cp_token *
new_token (void)
{
  cp_token *token = (cp_token *) xcalloc (1, sizeof (cp_token));
  token->keyword = KEYWORD_MAX;
  return token;
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

/* Return true if the next token is the indicated KEYWORD.  */

static int
cp_lexer_next_token_is_keyword (cp_lexer *lexer, cp_keyword keyword)
{
  return cp_lexer_peek_token (lexer)->keyword == keyword;
}


/* */

static void
cp_lexer_save_tokens (cp_lexer *lexer)
{
  VEC_safe_push (cp_saved_token_list, lexer->saved_tokens, lexer->tokens);
}

/* */

static void
cp_lexer_rollback_tokens (cp_lexer *lexer)
{
  lexer->tokens = VEC_pop (cp_saved_token_list, lexer->saved_tokens);
}


/* Construct a new parsing context.  The context below this one on the
   stack is given by NEXT.  */

static cp_parser_context *
cp_new_context (cp_parser_context *next)
{
  cp_parser_context *context;

  if (cp_parser_context_free_list != NULL)
    {
      /* Pull the first entry from the free list.  */
      context = cp_parser_context_free_list;
      cp_parser_context_free_list = context->next;
      memset (context, 0, sizeof (*context));
    }
  else
    context = (cp_parser_context *) xcalloc (1, sizeof (cp_parser_context));

  /* No errors have occurred in this context yet.  */
  context->status = CP_PARSER_STATUS_KIND_NO_ERROR;

  /* If this is not the bottom-most context, copy information that we
     need from the previous context.  */
  if (next)
    {
      /* If, in the NEXT context, we are parsing an `x->' or `x.'
	 expression, then we are parsing one this context, too.  */
      context->object_type = next->object_type;

      /* Thread the stack.  */
      context->next = next;
    }

  return context;
}

/* Returns non-zero if we are parsing tentatively.  */

static int
cp_parsing_tentatively (cp_parser *parser)
{
  return parser->context->next != NULL;
}

/* Returns non-zero if we are parsing tentatively and are not committed to
   this tentative parse.  */

static int
cp_uncommitted_to_tentative_parse_p (cp_parser *parser)
{
  return (cp_parsing_tentatively (parser)
	  && parser->context->status != CP_PARSER_STATUS_KIND_COMMITTED);
}

/* If we are parsing tentatively, remember that an error has occurred
   during this tentative parse.  Returns non-zero if the error was simluated;
   zero if a message should be issued by the caller.  */

static int
cp_simulate_error (cp_parser *parser)
{
  if (cp_uncommitted_to_tentative_parse_p (parser))
    {
      parser->context->status = CP_PARSER_STATUS_KIND_ERROR;
      return 1;
    }

  return 0;
}

/* Begin parsing tentatively.  We always save tokens while parsing
   tentatively sotaht if the parsing fails, we can restore the tokens.  */

static void
cp_parse_tentatively (cp_parser *parser)
{
  /* Enter a new parsing context.  */
  parser->context = cp_new_context (parser->context);

  /* Begin saving tokens.  */
  cp_lexer_save_tokens (parser->lexer);
}

static void
cp_lexer_commit_tokens (cp_lexer *lexer)
{
  VEC_pop (cp_saved_token_list, lexer->saved_tokens);
}


static void
cp_parse_error (cp_parser *parser, const char *format, ...)
{
  va_list args;

  va_start (args, format);
  if (!cp_simulate_error (parser))
    verror (format, args);
  va_end (args);
}


/* Returns non-zero if an error occurred during the most recent tentative
   parse.  */

static int
cp_parse_error_occurred (cp_parser *parser)
{
  return (cp_parsing_tentatively (parser)
	  && parser->context->status == CP_PARSER_STATUS_KIND_ERROR);
}

/* Stop parsing tentatively.  If a parse error has occurred, restore the
   token stream.  Otherwise, commit to the tokens we have consumed.

   Returns non-zero if no error occured; 0 otherwise.  */

static int
cp_parse_definitely (cp_parser *parser)
{
  int error_occurred;
  cp_parser_context *context;

  /* Remember whether or not an error occurred, since we are about to
     destroy that information.  */
  error_occurred = cp_parse_error_occurred (parser);

  /* Remove the top-most context from the stack.  */
  context = parser->context;
  parser->context = context->next;

  /* If no parse errors occurred, commit to the tentative parse.  */
  if (!error_occurred)
    {
      /* Commit to the tokens read tentatively, unless that was
	 already done. */
      if (context->status != CP_PARSER_STATUS_KIND_COMMITTED)
	cp_lexer_commit_tokens (parser->lexer);
    }
  /* Otherwise if errors occurred, roll back our state so that things
     are just as they were before we began the tentative parse.  */
  else
    cp_lexer_rollback_tokens (parser->lexer);

  /* Add the context to the front of the free list.  */
  context->next = cp_parser_context_free_list;
  cp_parser_context_free_list = context;

  return !error_occurred;
}

/* Abort the currently active tentative parse.  All consumed tokens
   will be rolled back, and no diagnostics will be issued.  */

static void
cp_abort_tentative_parse (cp_parser* parser)
{
  cp_simulate_error (parser);
  /* Now, pretend that we want to see if the construct was
     successfully parsed.  */
  cp_parse_definitely (parser);
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

/* Utility function to write the given string to the expression
   chain.  */
static void
write_expression_string (char *string)
{
  struct stoken tmp;
  tmp.ptr = string;
  tmp.length = strlen (string);
  write_exp_string (tmp);
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

/* */
static int
cp_skip_to_closing_parenthesis (cp_parser *parser, int recovering,
				int or_comma, int consume_paren)
{
  unsigned paren_depth = 0;
  unsigned brace_depth = 0;
  unsigned square_depth = 0;

  while (1)
    {
      cp_token *token = cp_lexer_peek_token (parser->lexer);

      switch (token->type)
	{
	case TTYPE_EOF:
	  return 0;

	case TTYPE_OPEN_SQUARE:
	  ++square_depth;
	  break;

	case TTYPE_CLOSE_SQUARE:
	  if (!square_depth--)
	    return 0;
	  break;

	case TTYPE_OPEN_BRACE:
	  ++brace_depth;
	  break;

	case TTYPE_CLOSE_BRACE:
	  if (!brace_depth--)
	    return 0;
	  break;

	case TTYPE_COMMA:
	  if (recovering && or_comma && !brace_depth && !paren_depth
	      && !square_depth)
	    return -1;
	  break;

	case TTYPE_OPEN_PAREN:
	  if (!brace_depth)
	    ++paren_depth;
	  break;

	case TTYPE_CLOSE_PAREN:
	  if (!brace_depth && !paren_depth--)
	    {
	      if (consume_paren)
		cp_lexer_consume_token (parser->lexer);
	      return 1;
	    }
	  break;

	default:
	  break;
	}

      cp_lexer_consume_token (parser->lexer);
    }
}

/* Print out an appropriate error message for an expected missing
   token of type TYPE.  */

static void
cp_required_token_error (cp_parser *parser, token_type type)
{
  /* !!FIXME!! This is not really sufficiently user-friendly.  */
  cp_parse_error (parser, _("syntax error: expected %s"),
		  token_table_strings[(int) type]);
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

/* Looks up NAME in the current scope, as given by PARSER->SCOPE.
   NAME should have one of the representations used for an
   id-expression.  If NAME is the ERROR_MARK_NODE, the ERROR_MARK_NODE
   is returned.  If PARSER->SCOPE is a dependent type, then a
   SCOPE_REF is returned.

   If NAME is a TEMPLATE_ID_EXPR, then it will be immediately
   returned; the name was already resolved when the TEMPLATE_ID_EXPR
   was formed.  Abstractly, such entities should not be passed to this
   function, because they do not need to be looked up, but it is
   simpler to check for this special case here, rather than at the
   call-sites.

   In cases not explicitly covered above, this function returns a
   DECL, OVERLOAD, or baselink representing the result of the lookup.
   If there was no entity with the indicated NAME, the ERROR_MARK_NODE
   is returned.

   If TAG_TYPE is not NONE_TYPE, it indicates an explicit type keyword
   (e.g., "struct") that was used.  In that case bindings that do not
   refer to types are ignored.

   If IS_TEMPLATE is TRUE, bindings that do not refer to templates are
   ignored.

   If IS_NAMESPACE is TRUE, bindings that do not refer to namespaces
   are ignored.

   If CHECK_DEPENDENCY is TRUE, names are not looked up in dependent
   types.

   If AMBIGUOUS_DECLS is non-NULL, *AMBIGUOUS_DECLS is set to a
   TREE_LIST of candidates if name-lookup results in an ambiguity, and
   NULL_TREE otherwise.  */

static cp_expression *
cp_lookup_name (cp_parser *parser, char *identifier)
{
  struct symbol *sym;
  cp_expression *expr;
  int is_a_field_of_this = 0;

  if (identifier == NULL)
    return NULL;

  sym = lookup_symbol (identifier, expression_context_block, VAR_DOMAIN,
		       (parser->lexer->language & CP_LANGUAGE_CPLUS
			? &is_a_field_of_this : NULL));

  /* !!FIXME!! I don't know how much of this is really necessary... Test.  */
  if (sym)
    {
      if (symbol_read_needs_frame (sym))
	{
	  if (innermost_block == 0
	      || contained_in (block_found, innermost_block))
	    innermost_block = block_found;
	}

      write_exp_elt_opcode (OP_VAR_VALUE);
      write_exp_elt_block (block_found);
      write_exp_elt_sym (sym);
      write_exp_elt_opcode (OP_VAR_VALUE);
      return cp_expression_chain (NULL);
    }
  else if (is_a_field_of_this)
    {
      /* C++: it hangs off `this'.  Must not inadvertantly convert from a method
	 call to a data ref.  */
      if (innermost_block == 0
	  || contained_in (block_found, innermost_block))
	innermost_block = block_found;

      write_exp_elt_opcode (OP_THIS);
      write_exp_elt_opcode (OP_THIS);
      write_exp_elt_opcode (STRUCTOP_PTR);
      write_expression_string (identifier);
      write_exp_elt_opcode (STRUCTOP_PTR);
    }
  else
    {
      struct minimal_symbol *msymbol
	= lookup_minimal_symbol (identifier, NULL, NULL);
      if (msymbol != NULL)
	write_exp_msymbol (msymbol);
      else if (!have_full_symbols () && !have_partial_symbols ())
	cp_parse_error (parser, _("No symbol table is loaded.  use the \"file\" command."));
      else
	{
	  cp_parse_error (parser, _("No symbol \"%s\" in current context."),
			  identifier);
	}
    }

  return NULL;
}

static struct type *
cp_lookup_type_name (cp_parser *parser, char *identifier,
		     enum tag_types tag_type)
{
  int is_a_field_of_this;
  struct symbol *sym;
  struct type *type = NULL;

  if (identifier == NULL)
    return NULL;

  sym = lookup_symbol (identifier, expression_context_block, VAR_DOMAIN,
		       (parser->lexer->language & CP_LANGUAGE_CPLUS
			? &is_a_field_of_this : NULL));
  
  if (sym != NULL && SYMBOL_CLASS (sym) == LOC_TYPEDEF)
    type = SYMBOL_TYPE (sym);

  if (type == NULL)
    {
      /* See if it is a type.  */
      switch (tag_type)
	{
	case enum_type:
	  type = lookup_enum (identifier, expression_context_block);
	  break;

	case class_type:
	case struct_type:
	  type = lookup_struct (identifier, expression_context_block);
	  break;
	}
    }

  return type;
}

static char *
cp_parse_identifier (cp_parser *parser)
{
  cp_token *token = cp_require_token (parser, TTYPE_NAME);
  return token ? token->value : NULL;
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

/* Parse an unqualified-id.

   unqualified-id:
     identifier
     operator-function-id
     conversion-function-id
     ~ class-name
     template-id

   Returns a representation of unqualified-id.  For the `identifier'
   production, an IDENTIFIER_NODE is returned.  For the `~ class-name'
   production a BIT_NOT_EXPR is returned; the operand of the
   BIT_NOT_EXPR is an IDENTIFIER_NODE for the class-name.  For the
   other productions, see the documentation accompanying the
   corresponding parsing functions.  */

static char *
cp_parse_unqualified_id (cp_parser *parser, int optional_p)
{
  cp_token *token = cp_lexer_peek_token (parser->lexer);

  switch (token->type)
    {
    case TTYPE_NAME:
      {
	/* We don't know yet whether or not this will be a template-id.  */
	cp_parse_tentatively (parser);

	/* Try a template-id.  */
	/* = cp_parse_template_id (...) */
	/* for now... */
	cp_simulate_error (parser);

	/* If that worked, we're done.  */
	if (cp_parse_definitely (parser))
	  return NULL; /* !!FIXME!! */

	/* Otherwise it's an ordinary identifier.  */
	/* this is a problem... For some things, we want string
	   (qualified names); for others, we want symbols (other
	   variable productions).  */
	return cp_parse_identifier (parser);
      }

    default:
      if (optional_p)
	return NULL;
      cp_parse_error (parser, _("expected unqualified-id"));
      return NULL;
    }
}

static char *
cp_parser_operator_function_id (cp_parser *parser)
{
  return NULL;
}

/* Parse an (optional) nested-name-specifier.

   nested-name-specifier: [C++98]
     class-or-namespace-name :: nested-name-specifier [opt]
     class-or-namespace-name :: template nested-name-specifier [opt]

   nested-name-specifier: [C++0x]
     type-name ::
     namespace-name ::
     nested-name-specifier identifier ::
     nested-name-specifier template [opt] simple-template-id ::

   PARSER->SCOPE should be set appropriately before this function is
   called.  TYPE_P is non-zero if non-type bindings should be ignored
   in name lookups.

   Sets PARSER->SCOPE to the class or namespace 
   specified by the nested-name-specifier, or leaves
   it unchanged if there is no nested-name-specifier.  Returns the new
   scope iff there is a nested-name-specifier, or NULL otherwise.  */

static cp_expression *
cp_parse_nested_name_specifier_opt (cp_parser *parser, int type_p)
{
  return NULL;
}

/* Parse an id-expression.

   id-expression:
     unqualified-id
     qualified-id

   qualified-id:
     :: [opt] nested-name-specifier template [opt] unqualified-id
     :: identifier
     :: operator-function-id
     :: template-id

   Returns a representation of the unqualified portion of the
   identifier.  Sets PARSER->SCOPE to the qualifying scope if there is
   a `::' or nested-name-specifier. */

static char *
cp_parse_id_expression (cp_parser *parser, int optional_p)
{
  int global_scope_p;
  int nested_name_specifier_p;

  global_scope_p
    = (cp_parse_global_scope_opt (parser, /*current_scope_valid_p=*/ 0)
       != NULL);

  nested_name_specifier_p
    = (cp_parse_nested_name_specifier_opt (parser, /*type_p=*/0) != NULL);

  /* If there is a nested-name-specifier, then we are looking at
     the first qualified-id production.  */
  if (nested_name_specifier_p)
    {
      /* stuff */
      return NULL;
    }
  /* Otherwise, if we are in global scope, then we are looking at one
     of the other qualified-id productions.  */
  else if (global_scope_p)
    {
      cp_token *token;

      /* Peek at the next token.  */
      token = cp_lexer_peek_token (parser->lexer);

      /* If it's an identifier, and the next token is not a `<', then
	 we can avoid the template-id case.  This is an optimization
	 for this common case.  */
      if (token->type == TTYPE_NAME)
	{
	  char *ident = cp_parse_identifier (parser);
	  return NULL;
	}

      /* Peek at the next token.  (Changes in the token buffer may
	 have invalidated the pointer obtained above.)  */
      token = cp_lexer_peek_token (parser->lexer);
      switch (token->type)
	{
	case TTYPE_NAME:
	  {
	    char *ident = cp_parse_identifier (parser);
	    return NULL;
	  }

	case TTYPE_KEYWORD:
	  if (token->keyword == KEYWORD_OPERATOR)
	    return cp_parser_operator_function_id (parser);
	  /* Fall through  */

	default:
	  cp_parse_error (parser, _("expected id-expression"));
	  return NULL;
	}
    }
  else
    return cp_parse_unqualified_id (parser, optional_p);
}

/* Parse a primary-expression.

   primary-expression:
     literal
     this
     ( expression )
     id-expression

   Returns a representation of the expression.  */

static cp_expression *
cp_parse_primary_expression (cp_parser *parser, int address_p, int cast_p)
{
  cp_token *token = cp_lexer_peek_token (parser->lexer);
  switch (token->type)
    {
      /* literal:
	   integer-literal
	   character-literal
	   floating-literal
	   string-literal
	   boolean-literal  */
    case TTYPE_CHAR:
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

    case TTYPE_STRING:
      break;

    case TTYPE_OPEN_PAREN:
      {
	cp_expression *expr;
	struct cleanup *c;

	/* Consume the `('.  */
	cp_lexer_consume_token (parser->lexer);

	/* Parse the parenthesized expression.  */
	expr = cp_parse_expression (parser, cast_p);
	c = make_cleanup (free_expression_chain, (void *) expr);

	/* Consume the `)'.  */
	cp_require_token (parser, TTYPE_CLOSE_PAREN);

	discard_cleanups (c);

	return expr;
      }
      break;

    case TTYPE_KEYWORD:
      break;

    case TTYPE_NAME:
    case TTYPE_SCOPE:
      {
	char *id;
	cp_expression *expr;

      id_expression:
	/* Parse the id-expression.  */
	id = cp_parse_id_expression (parser, /*optional_p=*/0);
	if (id == NULL)
	  return NULL;

	/* Look up the name.  */
	expr = cp_lookup_name (parser, id);
	/* !!FIXME!! I don't think that a primary_expression can
	   ever be optional?  */
	if (expr == NULL)
	  cp_parse_error (parser, _("expected primary-expression"));

	return expr;
      }

    default:
      cp_parse_error (parser, _("expected primary expression"));
    }

  /* probably want a special marker like g++ does */
  return NULL;
}

/* If parsing an integral constant-expression, issue an error mesage
   about the fact that THING appeared and return non-zero.  Otherwise,
   return zero.  In either case, set
   PARSER->NON_INTEGRAL_CONSTANT_EXPRESSION_P.  !!FIXME!! Do we need this?? */
static int
cp_non_integral_constant_expression (cp_parser *parser,
				     non_integral_constant thing)
{
  return 0;
}

/* Parse an (optional) assignment-operator.

   assignment-operator: one of
     = *= /= %= -= >>= <<= &= ^+ |=

   If the next token is an assignment oeprator, the corresponding
   expression code is returned, and the token is consumed.  Otherwise
   ERROR_CODE is returned.  */

static enum expr_code
cp_parse_assignment_operator_opt (cp_parser *parser)
{
  enum expr_code op;
  cp_token *token;

  /* Peek at the next token.  */
  token = cp_lexer_peek_token (parser->lexer);

  switch (token->type)
    {
    case TTYPE_EQ:
      op = NOP_EXPR;
      break;

    case TTYPE_MULT_EQ:
      op = MULT_EXPR;
      break;

    case TTYPE_DIV_EQ:
      op = DIV_EXPR;
      break;

    case TTYPE_MOD_EQ:
      op = MOD_EXPR;
      break;

    case TTYPE_PLUS_EQ:
      op = PLUS_EXPR;
      break;

    case TTYPE_MINUS_EQ:
      op = MINUS_EXPR;
      break;

    case TTYPE_RSHIFT_EQ:
      op = RSHIFT_EXPR;
      break;

    case TTYPE_LSHIFT_EQ:
      op = LSHIFT_EXPR;
      break;

    case TTYPE_AND_EQ:
      op = BIT_XOR_EXPR;
      break;

    case TTYPE_OR_EQ:
      op = BIT_IOR_EXPR;
      break;

    default:
      /* Nothing else is an assignment operator.  */
      op = ERROR_CODE;
    }

  /* If it was an assignment operator, consume the token.  */
  if (op != ERROR_CODE)
    cp_lexer_consume_token (parser->lexer);

  return op;
}

/* Parse a cast-expression that is not the operand of a unary `&'.  */

static cp_expression *
cp_parse_simple_cast_expression (cp_parser *parser)
{
  return cp_cast_expression (parser, /*address_p=*/0, /*cast_p=*/0);
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

    case BIT_AND_EXPR:
      operator = BINOP_BITWISE_AND;
      break;

    case BIT_IOR_EXPR:
      operator = BINOP_BITWISE_IOR;
      break;

    case BIT_XOR_EXPR:
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
      gdb_assert_not_reached ("unexpected binary operator");
    }

  /* All operators L -> R (not true for all operators?)
     So append to LHS and return LHS.  */
  cp_expression_append_chain (lhs, rhs);
  write_exp_elt_opcode (operator);
  lhs = cp_expression_chain (lhs);

  return lhs;
}

/* Parse a binary expression of the general form:

   pm-expression:
     cast-expression
     pm-expression .* cast-expression
     pm-expression ->* cast-expression

   multiplicative-expression:
     pm-expression
     multiplicative-expression * pm-expression
     multiplicative-expression / pm-expression
     multiplicative-expression % pm-expression

   additive-expression:
     multiplicative-expression
     additive-expression + multiplicative-expression
     additive-expression - multiplicative-expression

   shift-expression:
     additive-expression
     shift-expression << additive-expression
     shift-expression >> additive-expression

   relational-expression:
     shift-expression
     relational-expression < shift-expression
     relational-expression > shift-expression
     relational-expression <= shift-expression
     relational-expression >= shift-expression

   All these are implemented with a single function like:

   binary-expression:
     simple-cast-expression
     binary-expression <token> binary-expression

   CAST_P is true if this expression is the target of a cast.

   The binary_ops_token map is used to get the tree codes for each <token>
   type. binary-expressions are associated according to a precedence table.  */

static cp_expression *
cp_parse_binary_expression (cp_parser *parser, int cast_p,
			    enum cp_precedence prec)
{
  struct cleanup *back_to, *cleanup;
  cp_expression_stack stack;
  cp_expression_stack_entry *sp = &stack[0];
  cp_token *token;
  enum cp_precedence new_prec, lookahead_prec;
  cp_expression *lhs, *rhs;
  enum expr_code operator;

  lhs = cp_cast_expression (parser, /*address_p=*/0, cast_p);
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
cp_parse_initializer_clause (cp_parser *parser, int *non_constant_p)
{
  return NULL;
}

/* Build the assign-modify expression of the form:

   lhs OPERATOR= rhs

  Returns the resultant expression of the form:

    lhs
    rhs
    BINOP_ASSIGN_MODIFY
    OPERATOR
    BINOP_ASSIGN_MODIFY  */

static cp_expression *
build_x_modify_expr (cp_expression *lhs, enum expr_code modifycode,
		     cp_expression *rhs)
{
  /* Append RHS to LHS.  */
  cp_expression_append_chain (lhs, rhs);

  /* RHS is no longer needed.  */
  free_expression_chain (rhs);

  /* Write the assignment operator and append those elements to
     the expression chain.  */
  if (modifycode == NOP_EXPR)
    write_exp_elt_opcode (BINOP_ASSIGN);
  else
    {
      write_exp_elt_opcode (BINOP_ASSIGN_MODIFY);
      switch (modifycode)
	{
	case MULT_EXPR:
	  write_exp_elt_opcode (BINOP_MUL);
	  break;

	case DIV_EXPR:
	  write_exp_elt_opcode (BINOP_DIV);
	  break;

	case MOD_EXPR:
	  write_exp_elt_opcode (BINOP_REM);
	  break;

	case PLUS_EXPR:
	  write_exp_elt_opcode (BINOP_ADD);
	  break;

	case MINUS_EXPR:
	  write_exp_elt_opcode (BINOP_SUB);
	  break;

	case RSHIFT_EXPR:
	  write_exp_elt_opcode (BINOP_RSH);
	  break;

	case LSHIFT_EXPR:
	  write_exp_elt_opcode (BINOP_LSH);
	  break;

	case BIT_AND_EXPR:
	  write_exp_elt_opcode (BINOP_BITWISE_AND);
	  break;

	case BIT_XOR_EXPR:
	  write_exp_elt_opcode (BINOP_BITWISE_XOR);
	  break;

	case BIT_IOR_EXPR:
	  write_exp_elt_opcode (BINOP_BITWISE_IOR);
	  break;

	default:
	  (gdb_assert_not_reached
	   ("unreachable statement in build_x_modify_expr"));
	  break;
	}
      write_exp_elt_opcode (BINOP_ASSIGN_MODIFY);
    }

  return cp_expression_chain (lhs);
};


/* Parse an assignment-expression.

   assignment-expression:
     conditional-expression
     logical-or-expression assignment-operator assignment-expression

   CAST_P is non-zero if this expression is the target of a cast.

   Returns a representation of the expression.  */

static cp_expression *
cp_parse_assignment_expression (cp_parser *parser, int cast_p)
{
  cp_expression *expr;

  /* Parse the binary expressions (logical-or-expression).  */
  expr = cp_parse_binary_expression (parser, cast_p, PREC_NOT_OPERATOR);

  /* If the next token is a `?', then we are actually looking at a
     conditional-expression.  */
  if (cp_lexer_next_token_is (parser->lexer, TTYPE_QUERY))
    return NULL /*cp_parse_question_colon_clause (parser, expr)*/;
  else
    {
      enum expr_code assignment_operator;

      /* If it's an assignment-operator, we're using the second
	 production.  */
      assignment_operator
	= cp_parse_assignment_operator_opt (parser);
      if (assignment_operator != ERROR_CODE)
	{
	  int non_constant_p;
	  cp_expression *rhs;

	  /* Parse the right-hand side of the assignment.  */
	  rhs = cp_parse_initializer_clause (parser, &non_constant_p);

	  /* An assignment may not apear in a constant-expression.  */
	  if (cp_non_integral_constant_expression (parser, NIC_ASSIGNMENT))
	    return NULL;

	  /* Build the assignment expression.  */
	  expr = build_x_modify_expr (expr, assignment_operator, rhs);
	}
    }

  return expr;
}

static cp_expression *
build_x_compound_expr (cp_expression *a, cp_expression *b)
{
  return a;
}

/* Parse an expression.

   expression:
     assignment-expression
     expression , assignment-expression

  CAST_P is non-zero if this expression is the target of a cast.

  Returns a representation of the expression.  */

static cp_expression *
cp_parse_expression (cp_parser *parser, int cast_p)
{
  cp_expression *expression = NULL;

  while (1)
    {
      cp_expression *assignment_expression;

      /* Parse the next assignment-expression.  */
      assignment_expression
	= cp_parse_assignment_expression (parser, cast_p);

      /* If this is the first assignment-expression, we can just
	 save it away.  */
      if (expression == NULL)
	expression = assignment_expression;
      else
	expression = build_x_compound_expr (expression,
					    assignment_expression);

      /* If the next token is not a comma, then we are done with the
	 expression.  */
      if (!cp_lexer_next_token_is (parser->lexer, TTYPE_COMMA))
	break;

      /* Consume the `,'.  */
      cp_lexer_consume_token (parser->lexer);

      /* A comma operator cannot appear in a constant-expression.  */
      if (cp_non_integral_constant_expression (parser, NIC_COMMA))
	expression = NULL;
    }

  return expression;
}

/* Update the DECL_SPECS to reflect the TYPE_SPEC.  If USER_DEFINED_P
   is non-zero, the type is a user-defined type; otherwise it is a built-in
   type specified by a keyword.  */

static void
cp_set_decl_spec_type (cp_decl_specifier_seq *decl_specs,
		       struct type *type_spec, int user_defined_p)
{
  decl_specs->type = type_spec;
}

/* A subroutine of cp_parse_postfix_expression.  We're looking for

     postfix-expression [ expression ]

   Returns a representation of the expression.  */

static cp_expression *
cp_parse_postfix_open_square_expression (cp_parser *parser)
{
  cp_expression *expr;

  /* Consume the `[' token.  */
  cp_lexer_consume_token (parser->lexer);

  /* Parse the expression.  */
  expr = cp_parse_expression (parser, /*cast_p=*/0);

  /* Look for the closing `]'.  */
  cp_require_token (parser, TTYPE_CLOSE_SQUARE);

  /* Construct and return the final postfix-expression.  */
  write_exp_elt_opcode (BINOP_SUBSCRIPT);
  return cp_expression_chain (expr);
}

static cp_expression *
cp_parse_functional_cast (cp_parser *parser, struct type *type)
{
  return NULL;
}

/* Parse a class-name.

   class-name:
     identifier
     template-id

   TAG_TYPE indicates the explicit tag given before
   the type name, if any.

   Returns the type representing the class.  */

static struct type *
cp_parse_class_name (cp_parser *parser, enum tag_types tag_type)
{
  cp_token *token;
  struct type *type;

  /* All class-names start with an identifier.  */
  token = cp_lexer_peek_token (parser->lexer);
  if (token->type != TTYPE_NAME /*&& token->type != TTYPE_TEMPLATE_ID*/)
    {
      cp_parse_error (parser, _("expected class-name"));
      return NULL;
    }

  /* Handle the common case (an identifier, but not a template-id)
     efficiently.  */
  if (token->type == TTYPE_NAME
      /* && !cp_nth_token_starts_template_argument_list_p (parser, 2)*/)
    {
      char *identifier = cp_parse_identifier (parser);

      /* If the next token isn't an identifier, we are certainly not
	 looking at a class-name.  */
      if (identifier == NULL)
	type = NULL;
      else
	{
	  /* Look up the name.  */
	  type = cp_lookup_type_name (parser, identifier, tag_type);
	}
    }
  else
    {
      /* Try a template-id.  */
      ;
    }

  if (type == NULL)
    cp_parse_error (parser, _("expected class-name"));

  return type;
}

/* Parse a non-class type name.

  enum-name:
    identifier

  typedef-name:
    identifier

  Returns the type.  */

static struct type *
cp_parse_nonclass_name (cp_parser *parser)
{
  struct type *type;
  char *identifier;

  cp_token *token = cp_lexer_peek_token (parser->lexer);
  identifier = cp_parse_identifier (parser);
  if (identifier == NULL)
    return NULL;

  /* Look up the type-name.  */
  type = cp_lookup_type_name (parser, identifier, none_type);

  /* Issue an error if we did not find a type-name.  */
  if (type == NULL)
    {
      if (!cp_simulate_error (parser))
	cp_parse_error (parser, _("invalid type \"%s\" (!!FIXME!!?)"),
			identifier);
    }

  return type;
}

/* Parse a type-name.

   type-name:
     class-name
     enum-name
     typedef-name

   enum-name:
     identifier

   typedef-name:
     identifier

   Returns the type.  */

static struct type *
cp_parse_type_name (cp_parser *parser)
{
  struct type *type;

  /* We can't know yet whether it is a  class-name or not.  */
  cp_parse_tentatively (parser);

  /* Try a class-name.  */
  type = cp_parse_class_name (parser, none_type);

  /* If it's not a class-name, keep looking.  */
  if (!cp_parse_definitely (parser))
    {
      /* It must be a typedef-name or an enum-name.  */
      return cp_parse_nonclass_name (parser);
    }

  return type;
}

/* Parse a simple-type-specifier.

   simple-type-specifier:
     :: [opt] nested-name-specifier [opt] type-name
     :: [opt] nested-name-specifier template template-id
     char
     wchar_t
     bool
     short
     int
     long
     signed
     unsigned
     float
     double
     void

   Returns the indicated type expression.  If DECL_SPECS is not NULL, it is
   appropriately updated.  */

static struct type *
cp_parse_simple_type_specifier (cp_parser *parser,
				cp_decl_specifier_seq *decl_specs,
				cp_parser_flags flags)
{
  cp_token *token;
  struct type *type = NULL;

  /* Peek at the next token.  */
  token = cp_lexer_peek_token (parser->lexer);

  /* If we're looking at a keyword, things are easy.  */
  switch (token->keyword)
    {
    case KEYWORD_CHAR:
      type = language_lookup_primitive_type_by_name (parse_language,
						     parse_gdbarch,
						     token->value);
      break;

    case KEYWORD_BOOL:
      type = language_lookup_primitive_type_by_name (parse_language,
						     parse_gdbarch,
						     token->value);
      break;

    case KEYWORD_SHORT:
      if (decl_specs)
	++decl_specs->specs[(int) ds_short];
      type = language_lookup_primitive_type_by_name (parse_language,
						     parse_gdbarch,
						     token->value);
      break;

    case KEYWORD_INT:
      type = language_lookup_primitive_type_by_name (parse_language,
						     parse_gdbarch,
						     token->value);
      break;

    case KEYWORD_LONG:
      if (decl_specs)
	++decl_specs->specs[(int) ds_long];
      type = language_lookup_primitive_type_by_name (parse_language,
						     parse_gdbarch,
						     token->value);
      break;

    case KEYWORD_UNSIGNED:
      if (decl_specs)
	++decl_specs->specs[(int) ds_unsigned];
      type = language_lookup_primitive_type_by_name (parse_language,
						     parse_gdbarch,
						     token->value);
      break;

    case KEYWORD_SIGNED:
      if (decl_specs)
	++decl_specs->specs[(int) ds_signed];
      type = language_lookup_primitive_type_by_name (parse_language,
						     parse_gdbarch,
						     token->value);
      break;

    case KEYWORD_FLOAT:
      type = language_lookup_primitive_type_by_name (parse_language,
						     parse_gdbarch,
						     token->value);
      break;

    case KEYWORD_DOUBLE:
      type = language_lookup_primitive_type_by_name (parse_language,
						     parse_gdbarch,
						     token->value);
      break;

    case KEYWORD_VOID:
      type = language_lookup_primitive_type_by_name (parse_language,
						     parse_gdbarch,
						     token->value);
      break;

    default:
      break;
    }

  /* If the type-specifier was for a built-in type, we're done.  */
  if (type != NULL)
    {
      /* Record the type.  */
      if (decl_specs
	  && (token->keyword != KEYWORD_SIGNED
	      && token->keyword != KEYWORD_UNSIGNED
	      && token->keyword != KEYWORD_SHORT
	      && token->keyword != KEYWORD_LONG))
	cp_set_decl_spec_type (decl_specs, type, /*user_defined_p=*/0);

      /* Consume the token.  */
      cp_lexer_consume_token (parser->lexer);

      /* There is no valid C++ program where a non-template type is
	 followed by a "<".  That usually indicates the the user thought
	 that the type was  a template.  */
      /*cp_parser_check_for_invalid_template_id (parser, type);*/
      return type;
    }

  /* The type-specifier must be a user-defined type.  */
  /* !!FIXME!! Is this necessary? */
  if (!(flags & CP_PARSER_FLAGS_NO_USER_DEFINED_TYPES))
    {
      int qualified_p;
      int global_p;

      /* Don't gobble tokens or issue error messages if this is an
	 optional type-specifier.  */
      if (flags & CP_PARSER_FLAGS_OPTIONAL)
	cp_parse_tentatively (parser);

      /* Look for the optional `::' operator.  */
      global_p
	= (cp_parse_global_scope_opt (parser,
				       /*current_scope_valid_p=*/0)
	   != NULL);

      /* Look for the nested-name specifier.  */
      qualified_p
	= (cp_parse_nested_name_specifier_opt (parser, /*type_p=*/0)
	   != NULL);

      token = cp_lexer_peek_token (parser->lexer);

      /* Look for a type-name.  */
      type = cp_parse_type_name (parser);

      /* If it didn't work out, we don't have a TYPE.  */
      if ((flags & CP_PARSER_FLAGS_OPTIONAL)
	   && !cp_parse_definitely (parser))
	  type = NULL;

      if (type && decl_specs)
	cp_set_decl_spec_type (decl_specs, type, /*user_defined_p=*/1);
    }

  /* If we didn't get a type-name, issue an error message.  */
  if (type == NULL && !(flags & CP_PARSER_FLAGS_OPTIONAL))
    cp_parse_error (parser, _("expected type-name"));

  /* There is no valid C++ program wher a non-template type is
     followed by `<'.  That usually indicates that the user thought
     the type was a template.  */
  /*cp_check_for_invalid_template_id (...); */

  return type;
}

static enum tag_types
cp_token_is_class_key (cp_token *token)
{
  switch (token->keyword)
    {
    case KEYWORD_CLASS:
      return class_type;
    case KEYWORD_STRUCT:
      return struct_type;
    case KEYWORD_UNION:
      return union_type;

    default:
      return none_type;
    }  
}

static enum tag_types
cp_class_key (cp_parser *parser)
{
  cp_token *token;
  enum tag_types tag_type;

  /* Look for the class-key.  */
  token = cp_require_token (parser, TTYPE_KEYWORD);
  if (!token)
    return none_type;

  /* Check to see if the TOKEN is a class-key.  */
  tag_type = cp_token_is_class_key (token);
  if (!tag_type)
    cp_parse_error (parser, _("expected class-key"));
  return tag_type;
}

/* Issue an error message if the CLASS_KEY does not match the TYPE.  */

static void
cp_check_class_key (enum tag_types class_key, struct type *type)
{
  if ((TYPE_CODE (type) == TYPE_CODE_UNION) != (class_key == union_type))
    error (_("%s tag used in naming %s"),
	   class_key == union_type ? "union"
	   : class_key == struct_type ? "struct" : "class",
	   TYPE_NAME (type));
}

/* Parse an elaborated-type-specifier.  Note that the grammar given
   here incorporates the resolution to DR68.

   elaborated-type-specifier:
     class-key :: [opt] nested-name-specifier [opt] identifier
     class-key :: [opt] nested-name-specifier [opt] template [opt] template-id
     enum-key :: [opt] nested-name-specifier [opt] identifier

   Returns the TYPE specified.  */

static struct type *
cp_parse_elaborated_type_specifier (cp_parser *parser)
{
  enum tag_types tag_type;
  char *identifier;
  cp_token *token;
  struct type *type;

  /* See if we're looing at the `enum' keyword.  */
  if (cp_lexer_next_token_is_keyword (parser->lexer, KEYWORD_ENUM))
    {
      /* Consume the `enum' token.  */
      cp_lexer_consume_token (parser->lexer);

      /* Remember that it's an enumeration type.  */
      tag_type = enum_type;
    }
  else
    /* Otherwise it must be a class-key.  */
    {
      tag_type = cp_class_key (parser);
      if (tag_type == none_type)
	return NULL;
    }

  cp_parse_nested_name_specifier_opt (parser, /*type_p=*/1);

  /* For everything but enumeration types, consider a template-id.
     For an enumeration type, consider only a plain identifier.  */
  if (tag_type != enum_type)
    {
      ;
    }

  token = cp_lexer_peek_token (parser->lexer);
  identifier = cp_parse_identifier (parser);

  if (identifier == NULL)
    {
      /* I don't think this is necessary, either.  !!FIXME!! */
      parser->scope = NULL;
      return NULL;
    }

  /* Look up a qualified name in the usual way.  */
  type = cp_lookup_type_name (parser, identifier, tag_type);
  if (type == NULL)
    return NULL;

  if (tag_type != enum_type)
    cp_check_class_key (tag_type, type);

  /* A `<' cannot follow an elaborated type specifier.  If that
     happens, the user was probably trying to form a template-id.  */
  /*cp_check_for_invalid_template_id (parser, type);*/

  return type;
}

/* Parse a type-specifier.

   type-specifier:
     simple-type-specifier
     class-specifier
     enum-specifier
     elaborated-type-specifier
     cv-qualifier

   Returns a representation of the type-specifier.  

   The parser flags FLAGS is used to control type-specifier parsing.

   If IS_DECLARATION is TRUE, then this type-specifier is appearing
   in a decl-specifier-seq.
   If DECLARES_CLASS_OR_ENUM is non-NULL, and the type-specifier is a
   class-specifier, enum-specifier, or elaborated-type-specifier, then
   *DECLARES_CLASS_OR_ENUM is set to a nonzero value.  The value is 1
   if a type is declared; 2 if it is defined.  Otherwise, it is set to
   zero.

   If IS_CV_QUALIFIER is non-NULL, and the type-specifier is a
   cv-qualifier, then IS_CV_QUALIFIER is set to TRUE.  Otherwise, it
   is set to FALSE.  */

static struct type *
cp_parse_type_specifier (cp_parser *parser, cp_parser_flags flags,
			 cp_decl_specifier_seq *decl_specs,
			 int is_declaration, int *declares_class_or_enum,
			 int *is_cv_qualifier)
{
  struct type *type_spec;
  cp_token *token;
  enum cp_keyword keyword;
  cp_decl_spec ds = ds_last;

  /* Assume this type-specifier does not declare a new type.  */
  if (declares_class_or_enum)
    *declares_class_or_enum = 0;
  /* And that it does not specify a cv-qualifier.  */
  if (is_cv_qualifier)
    *is_cv_qualifier = 0;

  token = cp_lexer_peek_token (parser->lexer);

  /* If we're looking at a keyword, we can use that to guide the production
     we choose.  */
  keyword = token->keyword;
  switch (keyword)
    {
    case KEYWORD_CLASS:
    case KEYWORD_STRUCT:
    case KEYWORD_UNION:
    case KEYWORD_ENUM:
      /* We're declaring (not defining) a class or enum.  */
      if (declares_class_or_enum)
	*declares_class_or_enum = 1;

      type_spec = cp_parse_elaborated_type_specifier (parser);
      if (decl_specs)
	cp_set_decl_spec_type (decl_specs, type_spec, /*user_defined_p=*/1);
      return type_spec;

    case KEYWORD_CONST:
      ds = ds_const;
      if (is_cv_qualifier)
	*is_cv_qualifier = 1;
      break;

    case KEYWORD_VOLATILE:
      ds = ds_volatile;
      if (is_cv_qualifier)
	*is_cv_qualifier = 1;
      break;

    default:
      break;
    }

  /* Handle simple keywords.  */
  if (ds != ds_last)
    {
      if (decl_specs)
	{
	  ++decl_specs->specs[(int) ds];
	  /* decl_specs->any_specifiers_p = 1;*/
	}
      /* ummm.... */
      return NULL /*cp_lexer_consume_token (parser->lexer)->value*/;
    }

  /* If we do not alerady have a type-specifier, assume we are looking at a
     simple-type-specifier.  */
  type_spec = cp_parse_simple_type_specifier (parser, decl_specs, flags);

  /* If we didn't find a type-specifier, and a type-specifier was not
     optional in this context, issue an error message.  */
  if (!type_spec && !(flags & CP_PARSER_FLAGS_OPTIONAL))
    {
      cp_parse_error (parser, _("expected type-specifier"));
      return NULL;
    }

  return type_spec;
}

/* Parse a postfix-expression.

   postfix-expression:
     primary-expression
     postfix-expression [ expression ]
     postfix-expression ( expression-list [opt] )
     simple-type-specifier ( expression-list [opt] )
     typename :: [opt] nested-name-specifier identifier
       ( expression-list [opt] )
     typename :: [opt] nested-name-specifier template [opt] template-id
       ( expression-list [opt] )
     postfix-expression . template [opt] id-expression
     postifx-expression -> template [opt] id-expression
     postfix-expresssion . pseudo-destructor-name
     postfix-expression -> pseudo-destructor-name
     postfix-expression ++
     postfix-expression --
     dynamic_cast < type-id > ( expression )
     static_cast < type-id > ( expression )
     reinterpret_cast < type-id > ( expression )
     const_cast < type-id > ( expression )
     typeid ( expression )
     typeid ( type-id )

  If ADDRESS_P is true, the postifx expression is the operand of the
  `&' operator.  CAST_P is true if this expression is the target of a
  cast.

  Returns a representation of the expression.  */

static cp_expression *
cp_parse_postfix_expression (cp_parser *parser, int address_p, int cast_p)
{
  cp_expression *expr;
  cp_token *token = cp_lexer_peek_token (parser->lexer);

  switch (token->type)
    {
    default:
      {
        struct type *type;

	/* If the next thing is a simple-type-specifier, we may be
	   looking at a function cast.  We could also be looking at
	   an id-expression.  So we try the functional cast, and if that
	   doesn't work, we fall back to the primary-expression.  */
	cp_parse_tentatively (parser);

	/* Look for the simple-type-specifier.  */
	type = cp_parse_simple_type_specifier (parser, /*decl_specs=*/NULL,
					       CP_PARSER_FLAGS_NONE);

	/* Parse the cast itself.  */
	if (!cp_parse_error_occurred (parser))
	  expr = cp_parse_functional_cast (parser, type);

	/* If that worked, we're done.  */
	if (cp_parse_definitely (parser))
	  break;

	/* It must be a primary-expression.  */
	expr = cp_parse_primary_expression (parser, address_p, cast_p);
	break;
      }
    }

  while (1)
    {
      token = cp_lexer_peek_token (parser->lexer);

      switch (token->type)
	{
	case TTYPE_OPEN_SQUARE:
	  cp_expression_append_chain (expr,
				      (cp_parse_postfix_open_square_expression
				       (parser)));
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

  gdb_assert_not_reached ("unreachable statement parsing postfix expression");
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

/* Parse a unary-expression.

   unary-expression:
     postfix-expression
     ++ cast-expression
     -- cast-expression
     unary-operator cast-expression
     sizeof unary-expression
     sizeof ( type-id )
     new-expression
     delete-expression

   ADDRESS_P is true if the unary-expression is appearing as the
   operand of the `&' operator.   CAST_P is true if this expression is
   the target of a cast.

   Returns a representation of the expression.  */

static cp_expression *
cp_parse_unary_expression (cp_parser *parser, int address_p, int cast_p)
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

  /* Look for a unary operator.  */
  unary_operator = cp_unary_operator (token);
  /* The `++' and `--' operators can be handled similarly, even though
     they are not technically unary-operators in the grammar.  */
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
      cast_expr = cp_cast_expression (parser, unary_operator == ADDR_EXPR,
				      /*cast_p=*/0);
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
	  gdb_assert_not_reached ("unexpected unary operator");
	}

      write_exp_elt_opcode (operator);
      return cp_expression_chain (cast_expr);
    }

  return cp_parse_postfix_expression (parser, address_p, cast_p);
}

/* Set *DECL_SPECS to represent an empty decl-specifier-seq.  */

static void
clear_decl_specs (cp_decl_specifier_seq *decl_specs)
{
  memset (decl_specs, 0, sizeof (cp_decl_specifier_seq));
}

/* Check for repeated decl-specifiers.  */

static void
cp_check_decl_specs (cp_decl_specifier_seq *decl_specs)
{
  int ds;

  for (ds = ds_first; ds != ds_last; ++ds)
    {
      unsigned count = decl_specs->specs[ds];
      if (count < 2)
	continue;
      /* The "long" specifier is a special case because of "long long".  */
      if (ds == ds_long)
	{
	  if (count > 2)
	    error (_("\"long long long\" is too long"));
	}
      else if (count > 1)
	{
	  static const char *const decl_spec_names[] = {
            "signed",
            "unsigned",
            "short",
            "long",
            "const",
            "volatile",
            "restrict",
            "inline",
            "virtual",
            "explicit",
            "friend",
            "typedef",
            "constexpr",
            "__complex",
            "__thread"
          };
          error (_("duplicate %s"), decl_spec_names[ds]);

	}
    }
}

static void
cp_parse_type_specifier_seq (cp_parser *parser, int is_declaration,
			     int is_trailing_return,
			     cp_decl_specifier_seq *type_specifier_seq)
{
  int seen_type_specifier = 0;
  cp_parser_flags flags = CP_PARSER_FLAGS_OPTIONAL;
  struct type *type_specifier;

  clear_decl_specs (type_specifier_seq);

  while (1)
    {
      int is_cv_qualifier;
      type_specifier = cp_parse_type_specifier (parser, flags,
						type_specifier_seq,
						/*is_declaration=*/0,
						NULL, &is_cv_qualifier);
      if (!type_specifier)
	{
	  /* If the first type-specifier could not be found, this is not a
             type-specifier-seq at all.  */
          if (!seen_type_specifier)
            {
              cp_parse_error (parser, _("expected type-specifier"));
              type_specifier_seq->type = NULL;
              return;
            }
          /* If subsequent type-specifiers could not be found, the
             type-specifier-seq is complete.  */
          break;
	}

      seen_type_specifier = 1;
      if (is_declaration && !is_cv_qualifier)
	;
    }

  cp_check_decl_specs (type_specifier_seq);
}

static struct type *
cp_parse_type_id_1 (cp_parser *parser, int is_template_arg,
		    int is_trailing_return)
{
  cp_decl_specifier_seq type_specifier_seq;
  cp_parse_type_specifier_seq (parser, /*is_declaration=*/0,
			       is_trailing_return, &type_specifier_seq);

  /* need the abstract-declarator stuff, which includes ptr-operator
     and similar. */
  return type_specifier_seq.type;
}

static struct type *
cp_parse_type_id (cp_parser *parser)
{
  return cp_parse_type_id_1 (parser, 0, 0);
}

static int
cp_token_starts_cast_expression (cp_token *token)
{
  switch (token->type)
    {
    case TTYPE_COMMA:
    case TTYPE_QUERY:
    case TTYPE_COLON:
    case TTYPE_OPEN_SQUARE:
    case TTYPE_CLOSE_SQUARE:
    case TTYPE_CLOSE_PAREN:
    case TTYPE_CLOSE_BRACE:
    case TTYPE_DOT:
    case TTYPE_DOT_STAR:
    case TTYPE_DEREF:
    case TTYPE_DEREF_STAR:
    case TTYPE_DIV:
    case TTYPE_MOD:
    case TTYPE_LSHIFT:
    case TTYPE_RSHIFT:
    case TTYPE_LESS:
    case TTYPE_GREATER:
    case TTYPE_LESS_EQ:
    case TTYPE_GREATER_EQ:
    case TTYPE_EQ_EQ:
    case TTYPE_NOT_EQ:
    case TTYPE_EQ:
    case TTYPE_MULT_EQ:
    case TTYPE_DIV_EQ:
    case TTYPE_MOD_EQ:
    case TTYPE_PLUS_EQ:
    case TTYPE_MINUS_EQ:
    case TTYPE_RSHIFT_EQ:
    case TTYPE_LSHIFT_EQ:
    case TTYPE_AND_EQ:
    case TTYPE_XOR_EQ:
    case TTYPE_OR_EQ:
    case TTYPE_XOR:
    case TTYPE_OR:
    case TTYPE_OR_OR:
    case TTYPE_EOF:
      return 0;

    default:
      return 1;
    }
}

/* Parse a cast-expression.

   cast-expression:
     unary-expression
     ( type-id ) cast-expression

   ADDRESS_P is true if the unary-expression is appearing as the
   operand of the `&' operator.   CAST_P is true if this expression is
   the target of a cast.

   Returns a representation of the expression.  */

static cp_expression *
cp_cast_expression (cp_parser *parser, int address_p, int cast_p)
{
  struct type *type;
  cp_expression *expr;

   /* If the next token is `(', we might be looking at a cast.  */
  if (cp_lexer_next_token_is (parser->lexer, TTYPE_OPEN_PAREN))
    {
      int compound_literal_p;

      /* There is no way to know yet whether or this is as cast.
	 For example, `(int (3))' is a unary-expression, while
	 `(int) 3' is a cast. So we resort to parsing tentatively.  */
      cp_parse_tentatively (parser);

      /* Consume the '('.  */
      cp_lexer_consume_token (parser->lexer);

      /* A very tricky bit is that `(struct S) { 3 }' is a
	 compound-literal (which we permit in C++ as an extension).
	 But that construct is not a cast-expression -- it is a
	 postfix-expression.  (The reason is that `(struct S) { 3 }.i'
	 is legal; if the compound-literal were a cast-expression,
	 you'd need an extra set of parentheses.) But if we parse
	 the type-id and it happens to be a class-specifier, then we
	 will commit to the parse at that point, because we cannot
	 undo the action that is done when creating a new class.  So,
         then we cannot back up and do a postfix-expression.

         Therefore, we scan ahead to the closing `)', and check to see
         if the token after the `)' is a `{'.  If so, we are not
         looking at a cast-expression.

         Save tokens so that we can put them back.  */
      cp_lexer_save_tokens (parser->lexer);

      /* Skip tokens until the next closing parenthesis.  If the next
	 token is then '{', we know we are looking at a compound-literal.  */
      compound_literal_p
	= (cp_skip_to_closing_parenthesis (parser, 0, 0,
					   /*consume_paren=*/ 1)
	   && cp_lexer_next_token_is (parser->lexer, TTYPE_OPEN_BRACE));

      /* Roll back the tokens we skipped.  */
      cp_lexer_rollback_tokens (parser->lexer);
      
      /* If we were looking at a compound-literal, simulate an error
	 so that the call to cp_parse_definitely below will fail.  */
      if (compound_literal_p)
	cp_simulate_error (parser);
      else
	{
	  /* Look for the type-id.  */
	  type = cp_parse_type_id (parser);

	  /* Look for the closing `)'.  */
	  cp_require_token (parser, TTYPE_CLOSE_PAREN);
	}

      /* At this point, the expression can only be either a cast or a
	 parenthesized ctor such as `(T ())' that looks like a cast to a
	 function returning T.  */
      if (!cp_parse_error_occurred (parser)
	  && cp_token_starts_cast_expression (cp_lexer_peek_token
					      (parser->lexer)))
	{
	  cp_parse_definitely (parser);
	  expr = cp_cast_expression (parser, /*address_p=*/0, /*cast_p=*/1);

#ifdef I_DONT_KNOW
	  /* Only type conversions to integral or enumueration types
	     can be used in constant-expressions.  */ /*!!FIXME!! Necessary? */
	  if (!cast_valid_in_integral_constant_expression (type)
	      && cp_non_integral_constant_expression (parser, NIC_CAST))
	    return NULL;
#endif
	  if (expr != NULL)
	    {
	      /* Perform that cast.  */
	      write_exp_elt_opcode (UNOP_CAST);
	      write_exp_elt_type (type);
	      write_exp_elt_opcode (UNOP_CAST);
	      return cp_expression_chain (expr);
	    }
	}
      else
	cp_abort_tentative_parse (parser);
    }

  return cp_parse_unary_expression (parser, address_p, cast_p);
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
  parser->context = cp_new_context (NULL);

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
  if (parser_debug)
    _cp_dump_token_stream (parser->lexer);

  /* Parse input and reset global variables  */
  expr = cp_parse_expression (parser, /*cast_p=*/0);

  if (!cp_is_eof_token (cp_lexer_peek_token (parser->lexer)))
    {
      free_expression_chain (expr);
      error (_("Junk at end of expression"));
    }

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

  /* Create the EOF token marker.  */
  EOF_token = new_token ();
  EOF_token->type = TTYPE_EOF;

  /* Create a global namespace marker.  !!FIXME!! needed? */
  global_namespace = xcalloc (1, sizeof (cp_expression));

  /* Populate the binary operator map used by cp_parse_binary_expression.  */
  for (i = 0; i < sizeof (binary_ops) / sizeof (binary_ops[0]); ++i)
    binary_ops_token[binary_ops[i].token_type] = binary_ops[i];
}
