/* Parser for linespec for the GNU debugger, GDB.

   Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995,
   1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2007, 2008,
   2009, 2010, 2011 Free Software Foundation, Inc.

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
#include "symtab.h"
#include "frame.h"
#include "command.h"
#include "symfile.h"
#include "objfiles.h"
#include "source.h"
#include "demangle.h"
#include "value.h"
#include "completer.h"
#include "cp-abi.h"
#include "cp-support.h"
#include "parser-defs.h"
#include "block.h"
#include "objc-lang.h"
#include "linespec.h"
#include "exceptions.h"
#include "language.h"
#include "interps.h"
#include "mi/mi-cmds.h"
#include "target.h"
#include "arch-utils.h"
#include <ctype.h>
#include "cli/cli-utils.h"
#include "vec.h"

/* Token types  */
enum ls_token_type
{
  /* Terminal keyword.  */
  LSTOKEN_TERMINAL,

  /* A colon "separator."  */
  LSTOKEN_COLON,

  /* Scope operator "::" in C++ or "." in Java.  */
  LSTOKEN_SCOPE,

  /* A string.  */
  LSTOKEN_STRING,

  /* A number.  */
  LSTOKEN_NUMBER,

  /* EOF  */
  LSTOKEN_EOF,

  /* Unknown/unhandled  */
  LSTOKEN_UNKNOWN,

  /* Consumed token  */
  LSTOKEN_CONSUMED
};

/* Terminal types  */
enum ls_terminal_type
{
  LSTERMINAL_IF,
  LSTERMINAL_THREAD,
  LSTERMINAL_TASK
};

/* A linespec terminal  */
struct ls_terminal
{
  const char *string;
  enum ls_terminal_type type;
};

/* List of terminals  */
const struct ls_terminal linespec_terminals[] = { {"if", LSTERMINAL_IF}, {"thread", LSTERMINAL_THREAD}, {"task", LSTERMINAL_TASK} };

/* A token  */
struct ls_token
{
  enum ls_token_type type;
  union
  {
    struct stoken string;
    const struct ls_terminal *terminal;
  } data;
};

#define LS_TOKEN_STOKEN(TOK) (TOK).data.string
#define LS_TOKEN_TERMINAL(TOK) (TOK).data.terminal

/* A result of parsing a linespec (before it is converted into a SAL).  */
struct linespec_symbol
{
  char *name;
  struct symbol *function;
  struct symbol *label;
  struct minimal_symbol *msymbol;
};
#define LS_SYMBOL_NAME(S) (S)->name
#define LS_SYMBOL_FUNC(S) (S)->function
#define LS_SYMBOL_LABEL(S) (S)->label
#define LS_SYMBOL_MSYM(S) (S)->msymbol

typedef struct linespec_symbol ls_symbol;
DEF_VEC_O (ls_symbol);

#define LS_SYMBOL_ASSIGN(SYM, N, F, M, L)			\
  do								\
    {								\
      LS_SYMBOL_NAME (SYM) = savestring (N, strlen (N));	\
      LS_SYMBOL_FUNC (SYM) = F;					\
      LS_SYMBOL_MSYM (SYM) = M;					\
      LS_SYMBOL_LABEL (SYM) = L;				\
    } while (0)

/* An instance of the linespec parser.  */
struct linespec_parser
{
  /* Lexer internal data  */
  struct
  {
    /* Saved head of input stream.  */
    char *saved_arg;

    /* Head of the input stream  */
    char **stream;

    /* The current token  */
    struct ls_token current;
  } lexer;

  /* If FUNFIRSTLINE is nonzero, we want the first line of real code
     inside a function when a function is specified, and it is not OK
     to specify a variable or type to get its line number.  */
  int funfirstline;

  /* The file to use if none is specified.  It defaults to
     current_source_symtab.  */
  struct symtab *default_symtab;

  /* The default line number to use for relative line numbers (which start
     with signs).  Defaults to current_source_line.  */
  int default_line;

  /*  If non-NULL, store an array of strings containing the canonical
      line specs there if necessary.  Currently overloaded member functions
      and line numbers or static functions without a frame yield a
      canonical line spec.  The array and the line spec strings are
      allocated on the heap;  it is the caller's responsibility to free
      them.  */
  /* !!Do we need this here?  */
  struct linespec_result *canonical;

  /* If a file scope was specified for the line spec, this is that
     file's symtab;  NULL otherwise.  */
  struct symtab *file_symtab;
};

typedef struct linespec_parser linespec_parser;

#define QUOTE_CHAR '\''
#define QUOTE_CHAR_STRING "\'"
#define IS_QUOTE(C) ((C) == QUOTE_CHAR)

/* Prototypes for local functions.  */

static void initialize_defaults (struct symtab **default_symtab,
				 int *default_line);

struct symtabs_and_lines parse_linespec (char **argptr, int funfirstline,
					 struct symtab *default_symtab,
					 int default_line,
					 struct linespec_result *canonical);

static struct ls_token linespec_lexer_lex_one (linespec_parser *parser);

static struct symtabs_and_lines decode_objc (char **argptr,
					     int funfirstline,
					     struct symtab *file_symtab,
					     struct linespec_result *canonical,
					     char *saved_arg);

static void cplusplus_error (const char *name, const char *fmt, ...)
     ATTRIBUTE_NORETURN ATTRIBUTE_PRINTF (2, 3);

static int total_number_of_methods (struct type *type);

static int find_methods (struct type *, char *,
			 enum language, struct symbol **, struct symtab *);

static int add_matching_methods (int method_counter, struct type *t,
				 enum language language,
				 struct symbol **sym_arr);

static int add_constructors (int method_counter, struct type *t,
			     enum language language,
			     struct symbol **sym_arr);

static void build_canonical_line_spec (struct symtab_and_line *,
				       char *, struct linespec_result *);

#if 0
static int is_objc_method_format (const char *s);
#endif

static struct symtab *symtab_from_filename (struct stoken *stok);

static void minsym_found (linespec_parser *parser, ls_symbol *lsymbol,
			  struct symtab_and_line *sal);

/* Lexer functions.  */
static struct ls_token
linespec_lexer_consume_token (linespec_parser *parser)
{
  parser->lexer.current.type = LSTOKEN_CONSUMED;
  return linespec_lexer_lex_one (parser);
}

/* Helper functions.  */

/* Issue a helpful hint on using the command completion feature on
   single quoted demangled C++ symbols as part of the completion
   error.  */

static void
cplusplus_error (const char *name, const char *fmt, ...)
{
  struct ui_file *tmp_stream;
  char *message;

  tmp_stream = mem_fileopen ();
  make_cleanup_ui_file_delete (tmp_stream);

  {
    va_list args;

    va_start (args, fmt);
    vfprintf_unfiltered (tmp_stream, fmt, args);
    va_end (args);
  }

  while (*name == '\'')
    name++;
  fprintf_unfiltered (tmp_stream,
		      ("Hint: try '%s<TAB> or '%s<ESC-?>\n"
		       "(Note leading single quote.)"),
		      name, name);

  message = ui_file_xstrdup (tmp_stream, NULL);
  make_cleanup (xfree, message);
  throw_error (NOT_FOUND_ERROR, "%s", message);
}

/* Return the number of methods described for TYPE, including the
   methods from types it derives from.  This can't be done in the symbol
   reader because the type of the baseclass might still be stubbed
   when the definition of the derived class is parsed.  */

static int
total_number_of_methods (struct type *type)
{
  int n;
  int count;

  CHECK_TYPEDEF (type);
  if (! HAVE_CPLUS_STRUCT (type))
    return 0;
  count = TYPE_NFN_FIELDS_TOTAL (type);

  for (n = 0; n < TYPE_N_BASECLASSES (type); n++)
    count += total_number_of_methods (TYPE_BASECLASS (type, n));

  return count;
}

/* Returns the block to be used for symbol searches for the given SYMTAB,
   which may be NULL.  */

static struct block *
get_search_block (struct symtab *symtab)
{
  struct block *block;

  if (symtab != NULL)
    block = BLOCKVECTOR_BLOCK (BLOCKVECTOR (symtab), STATIC_BLOCK);
  else
    {
      enum language save_language;

      /* get_selected_block can change the current language when there is
	 no selected frame yet.  */
      save_language = current_language->la_language;
      block = get_selected_block (0);
      set_language (save_language);
    }

  return block;
}

/* Recursive helper function for decode_line_1.
   Look for methods named NAME in type T.
   Return number of matches.
   Put matches in SYM_ARR, which should have been allocated with
   a size of total_number_of_methods (T) * sizeof (struct symbol *).
   Note that this function is g++ specific.  */

static int
find_methods (struct type *t, char *name, enum language language,
	      struct symbol **sym_arr, struct symtab *file_symtab)
{
  int i1 = 0;
  int ibase;
  char *class_name = type_name_no_tag (t);
  struct cleanup *cleanup;
  char *canon;

  /* NAME is typed by the user: it needs to be canonicalized before
     passing to lookup_symbol.  */
  canon = cp_canonicalize_string_no_typedefs (name);
  if (canon != NULL)
    {
      name = canon;
      cleanup = make_cleanup (xfree, name);
    }
  else
    cleanup = make_cleanup (null_cleanup, NULL);

  /* Ignore this class if it doesn't have a name.  This is ugly, but
     unless we figure out how to get the physname without the name of
     the class, then the loop can't do any good.  */
  if (class_name
      && (lookup_symbol_in_language (class_name, get_search_block (file_symtab),
			 STRUCT_DOMAIN, language, (int *) NULL)))
    {
      int method_counter;
      int name_len = strlen (name);

      CHECK_TYPEDEF (t);

      /* Loop over each method name.  At this level, all overloads of a name
         are counted as a single name.  There is an inner loop which loops over
         each overload.  */

      for (method_counter = TYPE_NFN_FIELDS (t) - 1;
	   method_counter >= 0;
	   --method_counter)
	{
	  char *method_name = TYPE_FN_FIELDLIST_NAME (t, method_counter);
	  char dem_opname[64];

	  if (strncmp (method_name, "__", 2) == 0 ||
	      strncmp (method_name, "op", 2) == 0 ||
	      strncmp (method_name, "type", 4) == 0)
	    {
	      if (cplus_demangle_opname (method_name, dem_opname, DMGL_ANSI))
		method_name = dem_opname;
	      else if (cplus_demangle_opname (method_name, dem_opname, 0))
		method_name = dem_opname;
	    }

	  if (strcmp_iw (name, method_name) == 0)
	    /* Find all the overloaded methods with that name.  */
	    i1 += add_matching_methods (method_counter, t, language,
					sym_arr + i1);
	  else if (strncmp (class_name, name, name_len) == 0
		   && (class_name[name_len] == '\0'
		       || class_name[name_len] == '<'))
	    i1 += add_constructors (method_counter, t, language,
				    sym_arr + i1);
	}
    }

  /* Only search baseclasses if there is no match yet, since names in
     derived classes override those in baseclasses.

     FIXME: The above is not true; it is only true of member functions
     if they have the same number of arguments (??? - section 13.1 of the
     ARM says the function members are not in the same scope but doesn't
     really spell out the rules in a way I understand.  In any case, if
     the number of arguments differ this is a case in which we can overload
     rather than hiding without any problem, and gcc 2.4.5 does overload
     rather than hiding in this case).  */

  if (i1 == 0)
    for (ibase = 0; ibase < TYPE_N_BASECLASSES (t); ibase++)
      i1 += find_methods (TYPE_BASECLASS (t, ibase), name,
			  language, sym_arr + i1, file_symtab);

  do_cleanups (cleanup);
  return i1;
}

/* Add the symbols associated to methods of the class whose type is T
   and whose name matches the method indexed by METHOD_COUNTER in the
   array SYM_ARR.  Return the number of methods added.  */

static int
add_matching_methods (int method_counter, struct type *t,
		      enum language language, struct symbol **sym_arr)
{
  int field_counter;
  int i1 = 0;

  for (field_counter = TYPE_FN_FIELDLIST_LENGTH (t, method_counter) - 1;
       field_counter >= 0;
       --field_counter)
    {
      struct fn_field *f;
      const char *phys_name;

      f = TYPE_FN_FIELDLIST1 (t, method_counter);

      if (TYPE_FN_FIELD_STUB (f, field_counter))
	{
	  char *tmp_name, *tmp2;

	  tmp_name = gdb_mangle_name (t,
				      method_counter,
				      field_counter);
	  tmp2 = alloca (strlen (tmp_name) + 1);
	  strcpy (tmp2, tmp_name);
	  xfree (tmp_name);
	  phys_name = tmp2;
	}
      else
	phys_name = TYPE_FN_FIELD_PHYSNAME (f, field_counter);

      sym_arr[i1] = lookup_symbol_in_language (phys_name,
				   NULL, VAR_DOMAIN,
				   language,
				   (int *) NULL);
      if (sym_arr[i1])
	i1++;
      else
	{
	  /* This error message gets printed, but the method
	     still seems to be found.
	     fputs_filtered("(Cannot find method ", gdb_stdout);
	     fprintf_symbol_filtered (gdb_stdout, phys_name,
	                              language_cplus,
	                              DMGL_PARAMS | DMGL_ANSI);
	     fputs_filtered(" - possibly inlined.)\n", gdb_stdout);
	  */
	}
    }

  return i1;
}

/* Add the symbols associated to constructors of the class whose type
   is CLASS_TYPE and which are indexed by by METHOD_COUNTER to the
   array SYM_ARR.  Return the number of methods added.  */

static int
add_constructors (int method_counter, struct type *t,
		  enum language language, struct symbol **sym_arr)
{
  int field_counter;
  int i1 = 0;

  /* For GCC 3.x and stabs, constructors and destructors
     have names like __base_ctor and __complete_dtor.
     Check the physname for now if we're looking for a
     constructor.  */
  for (field_counter
	 = TYPE_FN_FIELDLIST_LENGTH (t, method_counter) - 1;
       field_counter >= 0;
       --field_counter)
    {
      struct fn_field *f;
      const char *phys_name;

      f = TYPE_FN_FIELDLIST1 (t, method_counter);

      /* GCC 3.x will never produce stabs stub methods, so
	 we don't need to handle this case.  */
      if (TYPE_FN_FIELD_STUB (f, field_counter))
	continue;
      phys_name = TYPE_FN_FIELD_PHYSNAME (f, field_counter);
      if (! is_constructor_name (phys_name))
	continue;

      /* If this method is actually defined, include it in the
	 list.  */
      sym_arr[i1] = lookup_symbol_in_language (phys_name,
				   NULL, VAR_DOMAIN,
				   language,
				   (int *) NULL);
      if (sym_arr[i1])
	i1++;
    }

  return i1;
}

/* Helper function for decode_line_1.
   Build a canonical line spec in CANONICAL if it is non-NULL and if
   the SAL has a symtab.
   If SYMNAME is non-NULL the canonical line spec is `filename:symname'.
   If SYMNAME is NULL the line number from SAL is used and the canonical
   line spec is `filename:linenum'.  */

static void
build_canonical_line_spec (struct symtab_and_line *sal, char *symname,
			   struct linespec_result *canonical)
{
  char **canonical_arr;
  char *canonical_name;
  char *filename;
  struct symtab *s = sal->symtab;

  if (s == (struct symtab *) NULL
      || s->filename == (char *) NULL
      || canonical == NULL)
    return;

  canonical_arr = (char **) xmalloc (sizeof (char *));
  canonical->canonical = canonical_arr;

  filename = s->filename;
  if (symname != NULL)
    {
      canonical_name = xmalloc (strlen (filename) + strlen (symname) + 2);
      sprintf (canonical_name, "%s:%s", filename, symname);
    }
  else
    {
      canonical_name = xmalloc (strlen (filename) + 30);
      sprintf (canonical_name, "%s:%d", filename, sal->line);
    }
  canonical_arr[0] = canonical_name;
}

#if 0
/* Determines if the gives string corresponds to an Objective-C method
   representation, such as -[Foo bar:] or +[Foo bar].  Objective-C symbols
   are allowed to have spaces and parentheses in them.  */

static int 
is_objc_method_format (const char *s)
{
  if (s == NULL || *s == '\0')
    return 0;
  /* Handle arguments with the format FILENAME:SYMBOL.  */
  if ((s[0] == ':') && (strchr ("+-", s[1]) != NULL) 
      && (s[2] == '[') && strchr(s, ']'))
    return 1;
  /* Handle arguments that are just SYMBOL.  */
  else if ((strchr ("+-", s[0]) != NULL) && (s[1] == '[') && strchr(s, ']'))
    return 1;
  return 0;
}
#endif

/* Given a list of NELTS symbols in SYM_ARR, return a list of lines to
   operate on (ask user if necessary).
   If CANONICAL is non-NULL return a corresponding array of mangled names
   as canonical line specs there.  */

static struct symtabs_and_lines
decode_line_2 (struct symbol *sym_arr[], int nelts, int funfirstline,
	       struct linespec_result *canonical)
{
  struct symtabs_and_lines values, return_values;
  char *args, *arg1;
  int i;
  char *prompt;
  char *symname;
  struct cleanup *old_chain;
  char **canonical_arr = (char **) NULL;
  const char *select_mode = multiple_symbols_select_mode ();

  if (select_mode == multiple_symbols_cancel)
    error (_("canceled because the command is ambiguous\n"
	     "See set/show multiple-symbol."));

  values.sals = (struct symtab_and_line *)
    alloca (nelts * sizeof (struct symtab_and_line));
  return_values.sals = (struct symtab_and_line *)
    xmalloc (nelts * sizeof (struct symtab_and_line));
  old_chain = make_cleanup (xfree, return_values.sals);

  if (canonical)
    {
      canonical_arr = (char **) xmalloc (nelts * sizeof (char *));
      make_cleanup (xfree, canonical_arr);
      memset (canonical_arr, 0, nelts * sizeof (char *));
      canonical->canonical = canonical_arr;
    }

  i = 0;
  while (i < nelts)
    {
      init_sal (&return_values.sals[i]);	/* Initialize to zeroes.  */
      init_sal (&values.sals[i]);
      if (sym_arr[i] && SYMBOL_CLASS (sym_arr[i]) == LOC_BLOCK)
	values.sals[i] = find_function_start_sal (sym_arr[i], funfirstline);
      i++;
    }

  /* If select_mode is "all", then do not print the multiple-choice
     menu and act as if the user had chosen choice "1" (all).  */
  if (select_mode == multiple_symbols_all
      || ui_out_is_mi_like_p (interp_ui_out (top_level_interpreter ())))
    args = "1";
  else
    {
      i = 0;
      printf_unfiltered (_("[0] cancel\n[1] all\n"));
      while (i < nelts)
        {
          if (sym_arr[i] && SYMBOL_CLASS (sym_arr[i]) == LOC_BLOCK)
            {
              if (values.sals[i].symtab)
                printf_unfiltered ("[%d] %s at %s:%d\n",
                                   (i + 2),
                                   SYMBOL_PRINT_NAME (sym_arr[i]),
                                   values.sals[i].symtab->filename,
                                   values.sals[i].line);
              else
                printf_unfiltered (_("[%d] %s at ?FILE:%d [No symtab? "
				     "Probably broken debug info...]\n"),
                                   (i + 2),
                                   SYMBOL_PRINT_NAME (sym_arr[i]),
                                   values.sals[i].line);

            }
          else
            printf_unfiltered (_("?HERE\n"));
          i++;
        }

      prompt = getenv ("PS2");
      if (prompt == NULL)
        {
          prompt = "> ";
        }
      args = command_line_input (prompt, 0, "overload-choice");
    }

  if (args == 0 || *args == 0)
    error_no_arg (_("one or more choice numbers"));

  i = 0;
  while (*args)
    {
      int num;

      arg1 = args;
      while (*arg1 >= '0' && *arg1 <= '9')
	arg1++;
      if (*arg1 && *arg1 != ' ' && *arg1 != '\t')
	error (_("Arguments must be choice numbers."));

      num = atoi (args);

      if (num == 0)
	error (_("canceled"));
      else if (num == 1)
	{
	  if (canonical_arr)
	    {
	      for (i = 0; i < nelts; i++)
		{
		  if (canonical_arr[i] == NULL)
		    {
		      symname = SYMBOL_LINKAGE_NAME (sym_arr[i]);
		      canonical_arr[i] = xstrdup (symname);
		    }
		}
	    }
	  memcpy (return_values.sals, values.sals,
		  (nelts * sizeof (struct symtab_and_line)));
	  return_values.nelts = nelts;
	  discard_cleanups (old_chain);
	  return return_values;
	}

      if (num >= nelts + 2)
	{
	  printf_unfiltered (_("No choice number %d.\n"), num);
	}
      else
	{
	  num -= 2;
	  if (values.sals[num].pc)
	    {
	      if (canonical_arr)
		{
		  symname = SYMBOL_LINKAGE_NAME (sym_arr[num]);
		  make_cleanup (xfree, symname);
		  canonical_arr[i] = xstrdup (symname);
		}
	      return_values.sals[i++] = values.sals[num];
	      values.sals[num].pc = 0;
	    }
	  else
	    {
	      printf_unfiltered (_("duplicate request for %d ignored.\n"),
				 num);
	    }
	}

      args = arg1;
      while (*args == ' ' || *args == '\t')
	args++;
    }
  return_values.nelts = i;
  discard_cleanups (old_chain);
  return return_values;
}

/* Given a list of NELTS symbols in SYM_ARR, return a list of lines to
   operate on (ask user if necessary).
   If CANONICAL is non-NULL return a corresponding array of mangled names
   as canonical line specs there.  */

static void
decode_multiple (linespec_parser *parser, struct symbol *sym_class,
		 char *method_name, struct symbol *sym_arr[], int nelts,
		 VEC (ls_symbol) **results)
{
  struct symtabs_and_lines values;
  char *args, *arg1;
  int i;
  char *prompt;
  char *symname;
  ls_symbol sym;
  const char *select_mode = multiple_symbols_select_mode ();

  if (select_mode == multiple_symbols_cancel)
    error (_("canceled because the command is ambiguous\n"
	     "See set/show multiple-symbol."));

  /* !!FIXME: This is evil.  There must be a better way.
     We have to keep this for line number info in the ask menu?!  */
  i = 0;
  while (i < nelts)
    {
      init_sal (&values.sals[i]);	/* Initialize to zeroes.  */
      if (sym_arr[i] && SYMBOL_CLASS (sym_arr[i]) == LOC_BLOCK)
	values.sals[i] = find_function_start_sal (sym_arr[i],
						  parser->funfirstline);
      i++;
    }

  /* If select_mode is "all", then do not print the multiple-choice
     menu and act as if the user had chosen choice "1" (all).  */
  if (select_mode == multiple_symbols_all
      || ui_out_is_mi_like_p (interp_ui_out (top_level_interpreter ())))
    args = "1";
  else
    {
      i = 0;
      printf_unfiltered (_("[0] cancel\n[1] all\n"));
      while (i < nelts)
        {
          if (sym_arr[i] && SYMBOL_CLASS (sym_arr[i]) == LOC_BLOCK)
            {
              if (values.sals[i].symtab)
                printf_unfiltered ("[%d] %s at %s:%d\n",
                                   (i + 2),
                                   SYMBOL_PRINT_NAME (sym_arr[i]),
                                   values.sals[i].symtab->filename,
                                   values.sals[i].line);
              else
                printf_unfiltered (_("[%d] %s at ?FILE:%d [No symtab? "
				     "Probably broken debug info...]\n"),
                                   (i + 2),
                                   SYMBOL_PRINT_NAME (sym_arr[i]),
                                   values.sals[i].line);

            }
          else
            printf_unfiltered (_("?HERE\n"));
          i++;
        }

      prompt = getenv ("PS2");
      if (prompt == NULL)
        {
          prompt = "> ";
        }
      args = command_line_input (prompt, 0, "overload-choice");
    }

  if (args == 0 || *args == 0)
    error_no_arg (_("one or more choice numbers"));

   while (*args)
    {
      int num;

      arg1 = args;
      while (*arg1 >= '0' && *arg1 <= '9')
	arg1++;
      if (*arg1 && *arg1 != ' ' && *arg1 != '\t')
	error (_("Arguments must be choice numbers."));

      num = atoi (args);

      if (num == 0)
	error (_("canceled"));
      else if (num == 1)
	{
	  for (i = 0; i < nelts; ++i)
	    {
	      LS_SYMBOL_ASSIGN (&sym, method_name, sym_arr[i], NULL, NULL);
	      VEC_safe_push (ls_symbol, *results, &sym);
	    }
	  return;
	}

      if (num >= nelts + 2)
	{
	  printf_unfiltered (_("No choice number %d.\n"), num);
	}
      else
	{
	  num -= 2;
	  if (sym_arr[num] != NULL)
	    {
	      LS_SYMBOL_ASSIGN (&sym, method_name, sym_arr[num], NULL, NULL);
	      VEC_safe_push (ls_symbol, *results, &sym);
	      sym_arr[num] = NULL;
	    }
	  else
	    {
	      printf_unfiltered (_("duplicate request for %d ignored.\n"),
				 num);
	    }
	}

      args = arg1;
      while (*args == ' ' || *args == '\t')
	args++;
    }
}



/* The parser of linespec itself.  */

/* Parse a string that specifies a line number.
   Pass the address of a char * variable; that variable will be
   advanced over the characters actually parsed.

   The string can be:

   LINENUM -- that line number in current file.  PC returned is 0.
   FILE:LINENUM -- that line in that file.  PC returned is 0.
   FUNCTION -- line number of openbrace of that function.
   PC returned is the start of the function.
   LABEL -- a label in the current scope
   VARIABLE -- line number of definition of that variable.
   PC returned is 0.
   FILE:FUNCTION -- likewise, but prefer functions in that file.
   *EXPR -- line in which address EXPR appears.

   This may all be followed by an "if EXPR", which we ignore.

   FUNCTION may be an undebuggable function found in minimal symbol table.

   If the argument FUNFIRSTLINE is nonzero, we want the first line
   of real code inside a function when a function is specified, and it is
   not OK to specify a variable or type to get its line number.

   DEFAULT_SYMTAB specifies the file to use if none is specified.
   It defaults to current_source_symtab.
   DEFAULT_LINE specifies the line number to use for relative
   line numbers (that start with signs).  Defaults to current_source_line.
   If CANONICAL is non-NULL, store an array of strings containing the canonical
   line specs there if necessary.  Currently overloaded member functions and
   line numbers or static functions without a filename yield a canonical
   line spec.  The array and the line spec strings are allocated on the heap,
   it is the callers responsibility to free them.

   Note that it is possible to return zero for the symtab
   if no file is validly specified.  Callers must check that.
   Also, the line number returned may be invalid.  */

/* We allow single quotes in various places.  This is a hideous
   kludge, which exists because the completer can't yet deal with the
   lack of single quotes.  FIXME: write a linespec_completer which we
   can use as appropriate instead of make_symbol_completion_list.  */

struct symtabs_and_lines
decode_line_1 (char **argptr, int funfirstline, struct symtab *default_symtab,
	       int default_line, struct linespec_result *canonical)
{
  return parse_linespec (argptr, funfirstline, default_symtab,
			 default_line, canonical);
}



/* Now, more helper functions for decode_line_1.  Some conventions
   that these functions follow:

   Decode_line_1 typically passes along some of its arguments or local
   variables to the subfunctions.  It passes the variables by
   reference if they are modified by the subfunction, and by value
   otherwise.

   Some of the functions have side effects that don't arise from
   variables that are passed by reference.  In particular, if a
   function is passed ARGPTR as an argument, it modifies what ARGPTR
   points to; typically, it advances *ARGPTR past whatever substring
   it has just looked at.  (If it doesn't modify *ARGPTR, then the
   function gets passed *ARGPTR instead, which is then called ARG.)
   Also, functions that return a struct symtabs_and_lines may modify
   CANONICAL, as in the description of decode_line_1.

   If a function returns a struct symtabs_and_lines, then that struct
   will immediately make its way up the call chain to be returned by
   decode_line_1.  In particular, all of the functions decode_XXX
   calculate the appropriate struct symtabs_and_lines, under the
   assumption that their argument is of the form XXX.  */

/* First, some functions to initialize stuff at the beggining of the
   function.  */

static void
initialize_defaults (struct symtab **default_symtab, int *default_line)
{
  if (*default_symtab == 0)
    {
      /* Use whatever we have for the default source line.  We don't use
         get_current_or_default_symtab_and_line as it can recurse and call
	 us back!  */
      struct symtab_and_line cursal = 
	get_current_source_symtab_and_line ();
      
      *default_symtab = cursal.symtab;
      *default_line = cursal.line;
    }
}



/* FIXME: Really should take a token for input and an explanatory message.  */

static void
malformed_linespec_error (linespec_parser *parser)
{
  error (_("unexpected linespec input: %s"), *parser->lexer.stream);
}

/* Decode arg of the form *PC.  */

static struct symtabs_and_lines
decode_expr_spec (linespec_parser *parser)
{
  struct symtabs_and_lines values;
  CORE_ADDR pc;
  struct ls_token token;
  char *str, *p;
  int len;

  token = linespec_lexer_lex_one (parser);
  switch (token.type)
    {
    case LSTOKEN_STRING: case LSTOKEN_NUMBER:
    case LSTOKEN_SCOPE:
      break;

    default:
      malformed_linespec_error (parser);
    }

  p = LS_TOKEN_STOKEN (token).ptr;
  if (*p == '*')
    ++p;

  len = LS_TOKEN_STOKEN (token).length;
  if (p != LS_TOKEN_STOKEN (token).ptr)
    --len;
  str = alloca (len + 1);
  memcpy (str, p, len);
  str[len] = '\0';

  pc = value_as_address (parse_to_comma_and_eval (&str));

  values.sals = (struct symtab_and_line *)
    xmalloc (sizeof (struct symtab_and_line));

  values.nelts = 1;
  values.sals[0] = find_pc_line (pc, 0);
  values.sals[0].pc = pc;
  values.sals[0].section = find_pc_overlay (pc);
  values.sals[0].explicit_pc = 1;

  return values;
}



/* Here's where we recognise an Objective-C Selector.  An Objective C
   selector may be implemented by more than one class, therefore it
   may represent more than one method/function.  This gives us a
   situation somewhat analogous to C++ overloading.  If there's more
   than one method that could represent the selector, then use some of
   the existing C++ code to let the user choose one.  */

struct symtabs_and_lines
decode_objc (char **argptr, int funfirstline, struct symtab *file_symtab,
	     struct linespec_result *canonical, char *saved_arg)
{
  struct symtabs_and_lines values;
  struct symbol **sym_arr = NULL;
  struct symbol *sym = NULL;
  struct block *block = NULL;
  unsigned i1 = 0;
  unsigned i2 = 0;

  values.sals = NULL;
  values.nelts = 0;

  find_imps (file_symtab, get_search_block (file_symtab), *argptr,
	     NULL, &i1, &i2); 
    
  if (i1 > 0)
    {
      sym_arr = (struct symbol **)
	alloca ((i1 + 1) * sizeof (struct symbol *));
      sym_arr[i1] = NULL;

      *argptr = find_imps (file_symtab, block, *argptr, sym_arr, &i1, &i2);
    }

  /* i1 now represents the TOTAL number of matches found.
     i2 represents how many HIGH-LEVEL (struct symbol) matches,
     which will come first in the sym_arr array.  Any low-level
     (minimal_symbol) matches will follow those.  */
      
  if (i1 == 1)
    {
      if (i2 > 0)
	{
	  /* Already a struct symbol.  */
	  sym = sym_arr[0];
	}
      else
	{
	  sym = find_pc_function (SYMBOL_VALUE_ADDRESS (sym_arr[0]));
	  if ((sym != NULL) && strcmp (SYMBOL_LINKAGE_NAME (sym_arr[0]),
				       SYMBOL_LINKAGE_NAME (sym)) != 0)
	    {
	      warning (_("debugging symbol \"%s\" does "
			 "not match selector; ignoring"),
		       SYMBOL_LINKAGE_NAME (sym));
	      sym = NULL;
	    }
	}
	      
      values.sals = (struct symtab_and_line *)
	xmalloc (sizeof (struct symtab_and_line));
      values.nelts = 1;
	      
      if (sym && SYMBOL_CLASS (sym) == LOC_BLOCK)
	{
	  /* Canonicalize this, so it remains resolved for dylib loads.  */
	  values.sals[0] = find_function_start_sal (sym, funfirstline);
	  build_canonical_line_spec (values.sals,
				     SYMBOL_NATURAL_NAME (sym), canonical);
	}
      else
	{
	  /* The only match was a non-debuggable symbol, which might point
	     to a function descriptor; resolve it to the actual code address
	     instead.  */
	  struct minimal_symbol *msymbol = (struct minimal_symbol *)sym_arr[0];
	  struct objfile *objfile = msymbol_objfile (msymbol);
	  struct gdbarch *gdbarch = get_objfile_arch (objfile);
	  CORE_ADDR pc = SYMBOL_VALUE_ADDRESS (msymbol);

	  pc = gdbarch_convert_from_func_ptr_addr (gdbarch, pc,
						   &current_target);

	  init_sal (&values.sals[0]);
	  values.sals[0].pc = pc;
	}
      return values;
    }

  if (i1 > 1)
    {
      /* More than one match.  The user must choose one or more.  */
      return decode_line_2 (sym_arr, i2, funfirstline, canonical);
    }

  return values;
}

/* !!keiths Ugh. Yet again... Move to cp-support.c.  What about
   other languages?  */

static int
is_overloaded (const char *copy)
{
  const char *p = copy + cp_validate_operator (copy);

  return strchr (p, '(') != NULL;
}

/* This finds the method(s) named METHOD_NAME in the class whose type is T
   and whose symbol is SYM_CLASS.  */

static void
find_method (linespec_parser *parser, struct type *t,
	     struct symbol *sym_class, char *method_name,
	     VEC (ls_symbol) **results)
{
  int i1;	/*  Counter for the symbol array.  */
  struct symbol **sym_arr;
  ls_symbol sym;

  /* Find all methods with a matching name, and put them in
     sym_arr.  */

  sym_arr = alloca (total_number_of_methods (t) * sizeof (struct symbol *));
  i1 = find_methods (t, method_name, SYMBOL_LANGUAGE (sym_class),
		     sym_arr, parser->file_symtab);

  /* If we were given a specific overload instance in COPY, defer the field
     acceptance until the strcmp_iw verification below, even if we found just
     a single field with that name.  */
  if (i1 == 1 && !is_overloaded (method_name))
    {
      /* There is exactly one field with that name.  */
      if (sym_arr[0] && SYMBOL_CLASS (sym_arr[0]) == LOC_BLOCK)
	{
	  LS_SYMBOL_ASSIGN (&sym, method_name, sym_arr[0], NULL, NULL);
	  VEC_safe_push (ls_symbol, *results, &sym);
	}

      return;
    }

  if (i1 > 0)
    {
      /* If we were given a specific overload instance, use that
	 (or error if no matches were found).  Otherwise ask the user
	 which one to use.  */
      if (is_overloaded (method_name))
	{
	  int i;
	  char *name;
	  char *canon;
	  struct cleanup *cleanup;

	  /* Construct the proper search name based on SYM_CLASS and COPY.
	     SAVED_ARG may contain a valid name, but that name might not be
	     what is actually stored in the symbol table.  For example,
	     if SAVED_ARG (and SYM_CLASS) were found via an import
	     ("using namespace" in C++), then the physname of
	     SYM_CLASS ("A::myclass") may not be the same as SAVED_ARG
	     ("myclass").  */
	  name = xmalloc (strlen (SYMBOL_NATURAL_NAME (sym_class))
			  + 2 /* "::" */ + strlen (method_name) + 1);
	  strcpy (name, SYMBOL_NATURAL_NAME (sym_class));
	  strcat (name, "::");
	  strcat (name, method_name);
	  canon = cp_canonicalize_string_no_typedefs (name);
	  if (canon != NULL)
	    {
	      xfree (name);
	      name = canon;
	    }
	  cleanup = make_cleanup (xfree, name);

	  for (i = 0; i < i1; ++i)
	    {
	      if (strcmp_iw (name, SYMBOL_LINKAGE_NAME (sym_arr[i])) == 0)
		{
		  LS_SYMBOL_ASSIGN (&sym, method_name, sym_arr[i], NULL, NULL);
		  VEC_safe_push (ls_symbol, *results, &sym);
		  do_cleanups (cleanup);
		  return;
		}
	    }

	  cplusplus_error (parser->lexer.saved_arg,
			   _("the class `%s' does not have "
			     "any method instance named %s"),
			   SYMBOL_PRINT_NAME (sym_class),
			   method_name);
	}

      decode_multiple (parser, sym_class, method_name, sym_arr, i1, results);
      return;
    }
  else
    {
      if (method_name[0] == '~')
	cplusplus_error (parser->lexer.saved_arg,
			 "the class `%s' does not have destructor defined\n",
			 SYMBOL_PRINT_NAME (sym_class));
      else
	cplusplus_error (parser->lexer.saved_arg,
			 "the class `%s' does not have any method named %s\n",
			 SYMBOL_PRINT_NAME (sym_class), method_name);
    }
}



/* Return the symtab associated to the filename given by the string
   in STOK.  */

static struct symtab *
symtab_from_filename (struct stoken *stok)
{
  char *copy;
  struct symtab *file_symtab;

  copy = (char *) alloca (stok->length + 1);
  memcpy (copy, stok->ptr, stok->length);
  copy[stok->length] = '\0';

  /* Find that file's data.  */
  file_symtab = lookup_symtab (copy);
  if (file_symtab == 0)
    {
      if (!have_full_symbols () && !have_partial_symbols ())
	throw_error (NOT_FOUND_ERROR,
		     _("No symbol table is loaded.  "
		       "Use the \"file\" command."));
      throw_error (NOT_FOUND_ERROR, _("No source file named %s."), copy);
    }

  return file_symtab;
}



/*!!keiths I don't like this.  It doesn't "flow" with the rest of this
  design.  */
static struct symtabs_and_lines
decode_lineno (linespec_parser *parser)
{
  char *p;
  struct symtabs_and_lines values;
  struct symtab_and_line val;
  struct ls_token token;

  enum sign
    {
      none, plus, minus
    }
  sign = none;

  /* We might need a canonical line spec if no file was specified.  */
  int need_canonical = (parser->file_symtab == NULL) ? 1 : 0;

  init_sal (&val);

  val.pspace = current_program_space;

  token = linespec_lexer_lex_one (parser);

  /* This is where we need to make sure that we have good defaults.
     We must guarantee that this section of code is never executed
     when we are called with just a function name, since
     set_default_source_symtab_and_line uses
     select_source_symtab that calls us with such an argument.  */

  if (parser->file_symtab == 0 && parser->default_symtab == 0)
    {
      /* Make sure we have at least a default source file.  */
      set_default_source_symtab_and_line ();
      initialize_defaults (&parser->default_symtab, &parser->default_line);
    }

  p = LS_TOKEN_STOKEN (token).ptr;
  if (*p == '+')
    sign = plus, p++;
  else if (*p == '-')
    sign = minus, p++;
  val.line = atoi (p); /* FIXME: decimal only!  */
  switch (sign)
    {
    case plus:
      if (*p == '\0')
	val.line = 5;
      if (parser->file_symtab == 0)
	val.line = parser->default_line + val.line;
      break;
    case minus:
      if (*p == '\0')
	val.line = 15;
      if (parser->file_symtab == 0)
	val.line = parser->default_line - val.line;
      else
	val.line = 1;
      break;
    case none:
      break;		/* No need to adjust val.line.  */
    }

  if (parser->file_symtab == 0)
    parser->file_symtab = parser->default_symtab;

  /* It is possible that this source file has more than one symtab, 
     and that the new line number specification has moved us from the
     default (in file_symtab) to a new one.  */
  val.symtab = find_line_symtab (parser->file_symtab, val.line, NULL, NULL);
  if (val.symtab == 0)
    val.symtab = parser->file_symtab;

  val.pspace = SYMTAB_PSPACE (val.symtab);
  val.pc = 0;
  values.sals = (struct symtab_and_line *)
    xmalloc (sizeof (struct symtab_and_line));
  values.sals[0] = val;
  values.nelts = 1;
  if (need_canonical)
    build_canonical_line_spec (values.sals, NULL, parser->canonical);
  values.sals[0].explicit_line = 1;
  return values;
}



/* A helper for decode_line_1 that tries to find a label.  The label
   is searched for in the current block.
   FUNCTION_SYMBOL is the enclosing function; or NULL if none
   specified.
   COPY is the name of the label to find.
   CANONICAL is the same as the "canonical" argument to decode_line_1.
   RESULT is a pointer to a symtabs_and_lines structure which will be
   filled in on success.
   This function returns 1 if a label was found, 0 otherwise.  */

static void
decode_label (linespec_parser *parser, char *copy, ls_symbol *result)
{
  int i;
  struct block *block;
  struct symbol *sym, *function_symbol;

  function_symbol = LS_SYMBOL_FUNC (result);

  if (function_symbol)
    block = SYMBOL_BLOCK_VALUE (function_symbol);
  else
    {
      block = get_selected_block (0);
      for (;
	   block && !BLOCK_FUNCTION (block);
	   block = BLOCK_SUPERBLOCK (block))
	;
      if (!block)
	return;
      function_symbol = BLOCK_FUNCTION (block);
    }

  sym = lookup_symbol (copy, block, LABEL_DOMAIN, 0);
  if (sym)
    LS_SYMBOL_ASSIGN (result, copy, function_symbol, NULL, sym);
}



/* Now come some functions that are called from multiple places within
   decode_line_1.  */

/* We've found a symbol SYM to associate with our linespec; build a
   corresponding struct symtabs_and_lines.  */

/* funfirstline = parser->funfirstline
   canonical = parser->canonical
   copy = user-input name of symbol
   sym = the actual symbol
   file_symtab = parser->file_symtab
   function_symbol = parser->symbol[].func_or_method
   to construct canonical only.  */
/* !!this whole thing needs to be split up? it's just does too much.  */
static void
symbol_found (linespec_parser *parser, ls_symbol *lsymbol,
		 struct symtab_and_line *sal)
{
  char *sym_name;

  init_sal (sal);

  /* If lsymbol.function is NULL, something went very wrong.  */
  gdb_assert (LS_SYMBOL_FUNC (lsymbol) != NULL);
  sym_name = LS_SYMBOL_NAME (lsymbol);

  if (LS_SYMBOL_LABEL (lsymbol) == NULL
      && SYMBOL_CLASS (LS_SYMBOL_FUNC (lsymbol)) == LOC_BLOCK)
    {
      /* Arg is the name of a function.  */
      *sal = find_function_start_sal (LS_SYMBOL_FUNC (lsymbol),
				      parser->funfirstline);

      /* Don't use the SYMBOL_LINE; if used at all it points to
	 the line containing the parameters or thereabouts, not
	 the first line of code.  */

      /* We might need a canonical line spec if it is a static
	 function.  */
      if (parser->file_symtab == 0)
	{
	  struct blockvector *bv
	    = BLOCKVECTOR (SYMBOL_SYMTAB (LS_SYMBOL_FUNC (lsymbol)));
	  struct block *b = BLOCKVECTOR_BLOCK (bv, STATIC_BLOCK);

	  if (lookup_block_symbol (b, sym_name, VAR_DOMAIN) != NULL)
	    build_canonical_line_spec (sal, sym_name, parser->canonical);
	}
    }
  else
    {
      if (LS_SYMBOL_LABEL (lsymbol)
	  && SYMBOL_CLASS (LS_SYMBOL_LABEL (lsymbol)) == LOC_LABEL
	  && SYMBOL_VALUE_ADDRESS (LS_SYMBOL_LABEL (lsymbol)) != 0)
	{
	  /* We know its line number.  */
	  sal->symtab = SYMBOL_SYMTAB (LS_SYMBOL_LABEL (lsymbol));
	  sal->line = SYMBOL_LINE (LS_SYMBOL_LABEL (lsymbol));
	  sal->pc = SYMBOL_VALUE_ADDRESS (LS_SYMBOL_LABEL (lsymbol));
	  sal->pspace
	    = SYMTAB_PSPACE (SYMBOL_SYMTAB (LS_SYMBOL_LABEL (lsymbol)));
	  sal->explicit_pc = 1;

	  if (parser->canonical)
	    {
	      parser->canonical->special_display = 1;
	      parser->canonical->canonical = xmalloc (sizeof (char *));
	      parser->canonical->canonical[0]
		= xstrprintf ("%s:%s",
			      SYMBOL_NATURAL_NAME (LS_SYMBOL_FUNC (lsymbol)),
			      SYMBOL_NATURAL_NAME (LS_SYMBOL_LABEL (lsymbol)));
	    }
	}
      else if (parser->funfirstline)
	{
	  /* NOT_FOUND_ERROR is not correct but it ensures SYM_NAME will be
	     searched also as a minimal symbol.  */

	  throw_error (NOT_FOUND_ERROR,
		       _("\"%s\" is not a function"), sym_name);
	}
      else if (SYMBOL_LINE (LS_SYMBOL_FUNC (lsymbol)) != 0)
	{
	  /* We know its line number.  */
	  sal->symtab = SYMBOL_SYMTAB (LS_SYMBOL_FUNC (lsymbol));
	  sal->line = SYMBOL_LINE (LS_SYMBOL_FUNC (lsymbol));
	  sal->pspace
	    = SYMTAB_PSPACE (SYMBOL_SYMTAB (LS_SYMBOL_FUNC (lsymbol)));
	}
      else
	/* This can happen if it is compiled with a compiler which doesn't
	   put out line numbers for variables.  */
	/* FIXME: Shouldn't we just set .line and .symtab to zero
	   and return?  For example, "info line foo" could print
	   the address.  */
	error (_("Line number not known for symbol \"%s\""), sym_name);
    }
}

/* We've found a minimal symbol in LSYM to associate with our
   linespec; build a corresponding struct symtab_and_line.  */

static void
minsym_found (linespec_parser *parser, ls_symbol *lsym,
		 struct symtab_and_line *sal)
{
  struct objfile *objfile = msymbol_objfile (LS_SYMBOL_MSYM (lsym));
  struct gdbarch *gdbarch = get_objfile_arch (objfile);
  struct symtabs_and_lines values;
  CORE_ADDR pc;

  *sal = find_pc_sect_line (SYMBOL_VALUE_ADDRESS (LS_SYMBOL_MSYM (lsym)),
			    (struct obj_section *) 0, 0);
  sal->section = SYMBOL_OBJ_SECTION (LS_SYMBOL_MSYM (lsym));

  /* The minimal symbol might point to a function descriptor;
     resolve it to the actual code address instead.  */
  pc = gdbarch_convert_from_func_ptr_addr (gdbarch,
                                           sal->pc,
                                           &current_target);
  if (pc != sal->pc)
    *sal = find_pc_sect_line (pc, NULL, 0);

  if (parser->funfirstline)
    skip_prologue_sal (sal);
}



static struct ls_token
linespec_lex_number (linespec_parser *parser)
{
  int is_hex;
  struct ls_token token;
  int (*is_a_digit) (int);

  token.type = LSTOKEN_NUMBER;
  LS_TOKEN_STOKEN (token).length = 0;
  LS_TOKEN_STOKEN (token).ptr = *parser->lexer.stream;
  is_a_digit = isdigit;

  /* Keep any sign at the start of STREAM.  */
  if (**parser->lexer.stream == '+' || **parser->lexer.stream == '-')
    ++(*parser->lexer.stream);

  /* If STREAM starts with "0x", keep it.  */
  if ((*parser->lexer.stream)[0] == '0' && (*parser->lexer.stream)[1] == 'x')
    {
      LS_TOKEN_STOKEN (token).length += 2;
      *parser->lexer.stream += 2;
      is_a_digit = isxdigit;
    }

  while ((*is_a_digit) (**parser->lexer.stream))
    {
      ++LS_TOKEN_STOKEN (token).length;
      ++(*parser->lexer.stream);
    }

  return token;
}

/* Does P represent one of the terminal keywords?  If so, return
   the terminal.  If not, return NULL.  */

static const struct ls_terminal *
get_terminal (const char *p)
{
  int i;

  if (p != NULL)
    {
      for (i = 0; i < ARRAY_SIZE (linespec_terminals); ++i)
	{
	  int len = strlen (linespec_terminals[i].string);

	  /* If P begins with one of the terminals and the
	     next character is not a valid identifier character,
	     we have found a terminal.  */
	  if (strncmp (p, linespec_terminals[i].string, len) == 0
	      && !(isalnum (p[len]) || p[len] == '_'))
	    return &linespec_terminals[i];
	}
    }

  return NULL;
}

/* Lex an identifier.  Could be a filename or a function/method.  */

static struct ls_token
linespec_lex_string (linespec_parser *parser)
{
  struct ls_token token;
  char *start = *parser->lexer.stream;

  token.type = LSTOKEN_STRING;

  /* If INPUT starts with the quote character, skip to the next quote
     character, regardless of the content.  */
  if (IS_QUOTE (**parser->lexer.stream))
    {
      /* Skip the beginning quote.  */
      ++(*parser->lexer.stream);

      /* Mark the start of the string.  */
      LS_TOKEN_STOKEN (token).ptr = *parser->lexer.stream;

      /* Skip to the ending quote.  */
      *parser->lexer.stream = skip_quoted_chars (*parser->lexer.stream,
						 QUOTE_CHAR_STRING, "");

      /* Error if the input did not terminate properly.  */
      if (!IS_QUOTE (*(*parser->lexer.stream - 1)))
	error (_("unmatched quote"));

      /* Mark the length of the string.  */
      LS_TOKEN_STOKEN (token).length = *parser->lexer.stream - 2 - start;
    }
  else
    {
      char *p;

      /* Otherwise, only identifier characters are permitted.
	 Spaces are the exception.  In general, we keep spaces,
	 but only if the next characters in the input do not resolve
	 to one of the terminals.

	 This allows users to forgo quoting CV-qualifiers, template arguments,
	 and similar common language constructs.  */

      /* Check for valid operator name.  */
      *parser->lexer.stream +=	cp_validate_operator (*parser->lexer.stream);

      while (1)
	{
	  if (isspace (**parser->lexer.stream))
	    {
	      p = skip_spaces (*parser->lexer.stream);
	      if (get_terminal (p) != NULL)
		{
		  LS_TOKEN_STOKEN (token).ptr = start;
		  LS_TOKEN_STOKEN (token).length
		    = *parser->lexer.stream - start;
		  return token;
		}

	      /* Advance past the whitespace.  */
	      *parser->lexer.stream = p;
	    }

	  switch (**parser->lexer.stream)
	    {
	    case 0: case ':':
	      LS_TOKEN_STOKEN (token).ptr = start;
	      LS_TOKEN_STOKEN (token).length = *parser->lexer.stream - start;
	      return token;

	    default:
	      /* Keep going  */
	      break;
	    }

	  ++(*parser->lexer.stream);
	}
    }

  return token;
}

static struct ls_token
linespec_lexer_lex_one (linespec_parser *parser)
{
  const struct ls_terminal *terminal;

  if (parser->lexer.current.type == LSTOKEN_CONSUMED)
    {
      /* Check for a terminal.  */
      terminal = get_terminal (*parser->lexer.stream);
      if (terminal != NULL)
	{
	  parser->lexer.current.type = LSTOKEN_TERMINAL;
	  LS_TOKEN_TERMINAL (parser->lexer.current) = terminal;

	  /* Skip INPUT past the token and return it.  */
	  *(parser->lexer.stream) += strlen (terminal->string);
	  return parser->lexer.current;
	}

      /* Handle other tokens.  */
      switch (**parser->lexer.stream)
	{
	case 0:
	  parser->lexer.current.type = LSTOKEN_EOF;
	  break;

	case '+': case '-':
	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
	  parser->lexer.current = linespec_lex_number (parser);
	  break;

#if 0
	  /* !!keiths The thought here was to treat '.' like '::' for java,
	     but that interferes with a lot of stuff.  Revisit this
	     or force jave through some other path.  */
	case '.':
	  parser->lexer.current.type = LSTOKEN_COLON;
	  LS_TOKEN_STOKEN (parser->lexer.current).ptr
	    = (*parser->lexer.stream)++;
	  LS_TOKEN_STOKEN (parser->lexer.current).length = 1;
	  break;
#endif

	case ':':
	  {
	    char *p = (*parser->lexer.stream)++;

	    if (**parser->lexer.stream == ':')
	      {
		++(*parser->lexer.stream);
		parser->lexer.current.type = LSTOKEN_SCOPE;
		LS_TOKEN_STOKEN (parser->lexer.current).ptr = p;
		LS_TOKEN_STOKEN (parser->lexer.current).length = 2;
	      }
	    else
	      parser->lexer.current.type = LSTOKEN_COLON;
	  }
	  break;

	  /* Everything else must be a string.  */
	default:
	  parser->lexer.current = linespec_lex_string (parser);
	  break;
	}
    }

  return parser->lexer.current;
}

/* Find any matching/requested symbols given by PARSER in
   PARSER->SYMBOLS.  This could be a method, function, or a label.  */

static void
find_linespec_symbol (linespec_parser *parser, VEC (ls_symbol) **results)
{
  int len;
  char *canon, *name;
  struct stoken func_name, class_name;
  struct ls_token token;
  ls_symbol ls;

  /* The decode_basic_spec should have guaranteed this.  */
  token = linespec_lexer_lex_one (parser);
  gdb_assert (token.type == LSTOKEN_STRING);

  len = 0;
  func_name.ptr = NULL;

  /* The class name starts with the current token.  */
  class_name = LS_TOKEN_STOKEN (token);
  len = class_name.length;

  /* Get the next token.  */
  token = linespec_lexer_consume_token (parser);

  while (token.type == LSTOKEN_SCOPE)
    {
      /* Get the next token.  */
      token = linespec_lexer_consume_token (parser);

      if (token.type != LSTOKEN_STRING)
	malformed_linespec_error (parser);

      func_name = LS_TOKEN_STOKEN (token);

      /* Get the next token.  */
      token = linespec_lexer_consume_token (parser);
      if (token.type == LSTOKEN_SCOPE)
	len += LS_TOKEN_STOKEN (token).length + func_name.length;
    }

  /* If we did not actually find LSTOKEN_SCOPE token, then the user
     only specified a function.  */
  if (func_name.ptr == NULL)
    {
      func_name = class_name;
      class_name.ptr = NULL;
    }

  /* We now have the two "parts" of the method name:  the class (or
     namespaces) prefix and the actual method name.  */
  if (class_name.ptr != NULL)
    {
      struct symbol *the_class;
      struct cleanup *back_to;

      name = alloca (len + 1);
      memcpy (name, class_name.ptr, len);
      name[len] = '\0';

      back_to = make_cleanup (null_cleanup, NULL);
      canon = cp_canonicalize_string_no_typedefs (name);
      if (canon != NULL)
	{
	  name = canon;
	  make_cleanup (xfree, name);
	}

      the_class = lookup_symbol (name,
				 get_search_block (parser->file_symtab),
				 STRUCT_DOMAIN, 0);
      do_cleanups (back_to);

      /* If we did not find a class, then class_name must represent
	 namespaces, and those get simply smashed together with the
	 function name for lookup.  */
      if (the_class == NULL)
	{
	  /* Readjust LEN to include the length of all tokens.  */
	  len = func_name.ptr - class_name.ptr + func_name.length;
	  func_name = class_name;
	  class_name.ptr = NULL;
	}
      else
	{
	  struct type *t;

	  /* Find the method in the class.  */
	  if (the_class &&
	      (t = check_typedef (SYMBOL_TYPE (the_class)),
	       (TYPE_CODE (t) == TYPE_CODE_STRUCT
		|| TYPE_CODE (t) == TYPE_CODE_UNION)))
	    {
	      char *n = alloca (func_name.length + 1);

	      memcpy (n, func_name.ptr, func_name.length);
	      n[func_name.length] = '\0';
	      find_method (parser, t, the_class, n, results);

	      /* If a symbol was found, return it ...  */
	      if (!VEC_empty (ls_symbol, *results))
		return;

	      /* ... otherwise, fallthrough to function lookup.  */
	    }
	}
    }

  name = alloca (len + 1);
  memcpy (name, func_name.ptr, len);
  name[len] = '\0';

  /* Look up FUNC_NAME as a label.  */
  memset (&ls, 0, sizeof (ls_symbol));
  decode_label (parser, name, &ls);
  if (ls.label == NULL)
    {
      struct symbol *sym;

      /* Look up FUNC_NAME as a function. */
      sym = lookup_symbol (name, get_search_block (parser->file_symtab),
			   VAR_DOMAIN, 0);
      if (sym != NULL)
	{
	  LS_SYMBOL_ASSIGN (&ls, name, sym, NULL, NULL);
	  VEC_safe_push (ls_symbol, *results, &ls);
	}
      else
	{
	  struct minimal_symbol *msym;

	  /* Failed to find a suitable symbol, so try to find a minimal symbol
	     with that name.  */

	  msym = lookup_minimal_symbol (name, NULL, NULL);
	  if (msym != NULL)
	    {
	      ls_symbol ls;

	      LS_SYMBOL_ASSIGN (&ls, name, NULL, msym, NULL);
	      VEC_safe_push (ls_symbol, *results, &ls);
	      return;
	    }

	  /* We could not find a symbol.  Throw a NOT_FOUND_ERROR with
	     an appropriate error message.  */
	  if (!have_full_symbols ()
	      && !have_partial_symbols ()
	      && !have_minimal_symbols ())
	    throw_error (NOT_FOUND_ERROR,
			 _("No symbol table is loaded.  Use the \"file\" command."));
	  throw_error (NOT_FOUND_ERROR, _("Function \"%s\" not defined."),
		       name);
	}
    }
}

static struct symtabs_and_lines
decode_basic_spec (linespec_parser *parser)
{
  int i;
  struct symbol *sym;
  struct ls_token token;
  struct symtabs_and_lines values;
  ls_symbol *lsp;
  VEC (ls_symbol) *symbols;

 /* Get the next token.  */
  token = linespec_lexer_lex_one (parser);

  /* If it is a NUMBER, we have the simple case of LINENO.  */
  if (token.type == LSTOKEN_NUMBER)
    return decode_lineno (parser);

  /* First check for the "global" namespace specification, of the from
     "::foo".  If found, skip over the colons and jump to nomral
     symbol processing.  */
  if (token.type == LSTOKEN_SCOPE)
    token = linespec_lexer_consume_token (parser);

  /* Next token must be LSTOKEN_STRING.  */
  if (token.type != LSTOKEN_STRING)
    malformed_linespec_error (parser);

  /* The current token (and possibly some subsequent tokens) represent
     a function(s) or method(s) or label(s).  */
  symbols = VEC_alloc (ls_symbol, 5);
  find_linespec_symbol (parser, &symbols);

  /* Get the next token.  */
  token = linespec_lexer_lex_one (parser);

  /* Allocate all of the SALS -- not all may be used.  */
  values.sals
    = xmalloc (VEC_length (ls_symbol, symbols)
	       * sizeof (struct symtab_and_line));

  if (token.type == LSTOKEN_COLON)
    {
      /* User specified a label or a lineno.  */
      token = linespec_lexer_consume_token (parser);
      if (token.type == LSTOKEN_NUMBER)
	/* We could do this, but it is a new feature.  */
	error (_("func:line is unimplemented"));
      else if (token.type == LSTOKEN_STRING)
	{
	  int i;
	  char *label = alloca (LS_TOKEN_STOKEN (token).length + 1);

	  memcpy (label, LS_TOKEN_STOKEN (token).ptr,
		  LS_TOKEN_STOKEN (token).length);
	  label[LS_TOKEN_STOKEN (token).length] = '\0';

	  for (i = 0; VEC_iterate (ls_symbol, symbols, i, lsp); ++i)
	    {
	      decode_label (parser, label, lsp);
#if 0
	      /* !!keihts Do I have to replace this? */
	      if (LS_SYMBOL_LABEL (lsp) != NULL)
		VEC_replace (ls_symbol, symbols, i, lsp);
#endif
	    }
	}
      else
	malformed_linespec_error (parser);
    }

  values.nelts = VEC_length (ls_symbol, symbols);
  values.sals = XCALLOC (values.nelts, struct symtab_and_line);
  for (i = 0; VEC_iterate (ls_symbol, symbols, i, lsp); ++i)
    {
      if (LS_SYMBOL_FUNC (lsp) != NULL)
	symbol_found (parser, lsp, &values.sals[i]);
      else
	{
	  gdb_assert (LS_SYMBOL_MSYM (lsp) != NULL);
	  minsym_found (parser, lsp, &values.sals[i]);
	}
    }

  /* !!I think there is some data that needs freeing, too.  */
  /* Free the vector.  */
  VEC_free (ls_symbol, symbols);

  return values;
}

/* Basic grammar:

   linespec ->
      file_spec basic_spec | basic_spec | expr_spec

   file_spec ->
      STRING ':'

   expr_spec ->
      '*' STRING

   basic_spec ->
      func_label | lineno

   func_label ->
      func | label | func ':' label | func ':' lineno

   lineno ->
      NUMBER

   func ->
      STRING | STRING ('::' | '.') func

   label ->
      STRING
*/
struct symtabs_and_lines
parse_linespec (char **argptr, int funfirstline,
		struct symtab *default_symtab, int default_line,
		struct linespec_result *canonical)
{
  struct ls_token token;
  struct symtabs_and_lines values;
  linespec_parser parser;
  volatile struct gdb_exception except;

  memset (&values, 0, sizeof (struct symtabs_and_lines));
  parser.lexer.saved_arg = *argptr;
  parser.lexer.stream = argptr;
  parser.lexer.current.type = LSTOKEN_CONSUMED;
  parser.funfirstline = funfirstline;
  parser.default_symtab = default_symtab;
  parser.default_line = default_line;
  parser.canonical = canonical;
  parser.file_symtab = NULL;

  /* Defaults have defaults.  */
  initialize_defaults (&parser.default_symtab, &parser.default_line);

  /* Get the first token.  */
  token = linespec_lexer_lex_one (&parser);

  /* It must be either LSTOKEN_STRING or LSTOKEN_NUMBER.  */
  if (token.type == LSTOKEN_STRING && *LS_TOKEN_STOKEN (token).ptr == '*')
    {
      /* User specified an expression, *EXPR.  */
      return decode_expr_spec (&parser);
    }
  else if (token.type != LSTOKEN_STRING && token.type != LSTOKEN_NUMBER)
    malformed_linespec_error (&parser);

  /* Check if the input is a filename.  */
  TRY_CATCH (except, RETURN_MASK_ERROR)
    {
      /* !!keiths We could return a list here, and then loop over the
	 this list.  */
      parser.file_symtab = symtab_from_filename (&LS_TOKEN_STOKEN (token));
    }

  if (except.reason >= 0 && parser.file_symtab != NULL)
    {
      /* Get the next token.  */
      token = linespec_lexer_consume_token (&parser);

      /* This token must be LSTOKEN_COLON.  */
      if (token.type != LSTOKEN_COLON)
	malformed_linespec_error (&parser);

      /* Consume the LSTOKEN_COLON.  */
      linespec_lexer_consume_token (&parser);
    }

  /* Short-circuit for objc selector.  */
  values = decode_objc (argptr, funfirstline, parser.file_symtab,
			canonical, parser.lexer.saved_arg);
  if (values.sals != NULL)
    return values;

  return decode_basic_spec (&parser);
}

void
init_linespec_result (struct linespec_result *lr)
{
  memset (lr, 0, sizeof (*lr));
}
