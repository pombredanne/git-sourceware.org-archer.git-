/* DWARF 2 location expression support for GDB.

   Copyright (C) 2003, 2005, 2007, 2008, 2009, 2010
   Free Software Foundation, Inc.

   Contributed by Daniel Jacobowitz, MontaVista Software, Inc.

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
#include "ui-out.h"
#include "value.h"
#include "frame.h"
#include "gdbcore.h"
#include "target.h"
#include "inferior.h"
#include "ax.h"
#include "ax-gdb.h"
#include "regcache.h"
#include "objfiles.h"
#include "exceptions.h"
#include "block.h"

#include "dwarf2.h"
#include "dwarf2expr.h"
#include "dwarf2loc.h"
#include "dwarf2-frame.h"

#include "gdb_string.h"
#include "gdb_assert.h"

static void
dwarf_expr_frame_base_1 (struct symbol *framefunc, CORE_ADDR pc,
			 gdb_byte **start, size_t *length);

static struct value *dwarf2_evaluate_loc_desc (struct type *type,
					       struct frame_info *frame,
					       gdb_byte *data,
					       unsigned short size,
					     struct dwarf2_per_cu_data *per_cu);

/* A helper function for dealing with location lists.  Given a
   symbol baton (BATON) and a pc value (PC), find the appropriate
   location expression, set *LOCEXPR_LENGTH, and return a pointer
   to the beginning of the expression.  Returns NULL on failure.

   For now, only return the first matching location expression; there
   can be more than one in the list.  */

static gdb_byte *
find_location_expression (struct dwarf2_loclist_baton *baton,
			  size_t *locexpr_length, CORE_ADDR pc)
{
  CORE_ADDR low, high;
  gdb_byte *loc_ptr, *buf_end;
  int length;
  struct objfile *objfile = dwarf2_per_cu_objfile (baton->per_cu);
  struct gdbarch *gdbarch = get_objfile_arch (objfile);
  enum bfd_endian byte_order = gdbarch_byte_order (gdbarch);
  unsigned int addr_size = dwarf2_per_cu_addr_size (baton->per_cu);
  CORE_ADDR base_mask = ~(~(CORE_ADDR)1 << (addr_size * 8 - 1));
  /* Adjust base_address for relocatable objects.  */
  CORE_ADDR base_offset = ANOFFSET (objfile->section_offsets,
				    SECT_OFF_TEXT (objfile));
  CORE_ADDR base_address = baton->base_address + base_offset;

  loc_ptr = baton->data;
  buf_end = baton->data + baton->size;

  while (1)
    {
      if (buf_end - loc_ptr < 2 * addr_size)
	error (_("find_location_expression: Corrupted DWARF expression."));

      low = extract_unsigned_integer (loc_ptr, addr_size, byte_order);
      loc_ptr += addr_size;

      /* A base-address-selection entry.  */
      if (low == base_mask)
	{
	  base_address = dwarf2_read_address (gdbarch,
					      loc_ptr, buf_end, addr_size);
	  loc_ptr += addr_size;
	  continue;
	}

      high = extract_unsigned_integer (loc_ptr, addr_size, byte_order);
      loc_ptr += addr_size;

      /* An end-of-list entry.  */
      if (low == 0 && high == 0)
	return NULL;

      /* Otherwise, a location expression entry.  */
      low += base_address;
      high += base_address;

      length = extract_unsigned_integer (loc_ptr, 2, byte_order);
      loc_ptr += 2;

      if (pc >= low && pc < high)
	{
	  *locexpr_length = length;
	  return loc_ptr;
	}

      loc_ptr += length;
    }
}

/* This is the baton used when performing dwarf2 expression
   evaluation.  */
struct dwarf_expr_baton
{
  struct frame_info *frame;
  struct objfile *objfile;
  /* From DW_TAG_variable's DW_AT_location (not DW_TAG_type's
     DW_AT_data_location) for DW_OP_push_object_address.  */
  CORE_ADDR object_address;
};

/* Helper functions for dwarf2_evaluate_loc_desc.  */

/* Using the frame specified in BATON, return the value of register
   REGNUM, treated as a pointer.  */
static CORE_ADDR
dwarf_expr_read_reg (void *baton, int dwarf_regnum)
{
  struct dwarf_expr_baton *debaton = (struct dwarf_expr_baton *) baton;
  struct gdbarch *gdbarch = get_frame_arch (debaton->frame);
  CORE_ADDR result;
  int regnum;

  regnum = gdbarch_dwarf2_reg_to_regnum (gdbarch, dwarf_regnum);
  result = address_from_register (builtin_type (gdbarch)->builtin_data_ptr,
				  regnum, debaton->frame);
  return result;
}

/* Read memory at ADDR (length LEN) into BUF.  */

static void
dwarf_expr_read_mem (void *baton, gdb_byte *buf, CORE_ADDR addr, size_t len)
{
  read_memory (addr, buf, len);
}

/* Using the frame specified in BATON, find the location expression
   describing the frame base.  Return a pointer to it in START and
   its length in LENGTH.  */
static void
dwarf_expr_frame_base (void *baton, gdb_byte **start, size_t * length)
{
  /* FIXME: cagney/2003-03-26: This code should be using
     get_frame_base_address(), and then implement a dwarf2 specific
     this_base method.  */
  struct symbol *framefunc;
  struct dwarf_expr_baton *debaton = (struct dwarf_expr_baton *) baton;

  /* Use block_linkage_function, which returns a real (not inlined)
     function, instead of get_frame_function, which may return an
     inlined function.  */
  framefunc = block_linkage_function (get_frame_block (debaton->frame, NULL));

  /* If we found a frame-relative symbol then it was certainly within
     some function associated with a frame. If we can't find the frame,
     something has gone wrong.  */
  gdb_assert (framefunc != NULL);

  dwarf_expr_frame_base_1 (framefunc,
			   get_frame_address_in_block (debaton->frame),
			   start, length);
}

static void
dwarf_expr_frame_base_1 (struct symbol *framefunc, CORE_ADDR pc,
			 gdb_byte **start, size_t *length)
{
  if (SYMBOL_LOCATION_BATON (framefunc) == NULL)
    *start = NULL;
  else if (SYMBOL_COMPUTED_OPS (framefunc) == &dwarf2_loclist_funcs)
    {
      struct dwarf2_loclist_baton *symbaton;

      symbaton = SYMBOL_LOCATION_BATON (framefunc);
      *start = find_location_expression (symbaton, length, pc);
    }
  else if (SYMBOL_COMPUTED_OPS (framefunc) == &dwarf2_locexpr_funcs)
    {
      struct dwarf2_locexpr_baton *symbaton;

      symbaton = SYMBOL_LOCATION_BATON (framefunc);
      gdb_assert (symbaton != NULL);
      *start = symbaton->data;
      *length = symbaton->size;
    }
  else if (SYMBOL_COMPUTED_OPS (framefunc) == &dwarf2_missing_funcs)
    {
      struct dwarf2_locexpr_baton *symbaton;

      symbaton = SYMBOL_LOCATION_BATON (framefunc);
      gdb_assert (symbaton == NULL);
      *start = NULL;
      *length = 0;	/* unused */
    }
  else
    internal_error (__FILE__, __LINE__,
		    _("Unsupported SYMBOL_COMPUTED_OPS %p for \"%s\""),
		    SYMBOL_COMPUTED_OPS (framefunc),
		    SYMBOL_PRINT_NAME (framefunc));

  if (*start == NULL)
    error (_("Could not find the frame base for \"%s\"."),
	   SYMBOL_PRINT_NAME (framefunc));
}

/* Helper function for dwarf2_evaluate_loc_desc.  Computes the CFA for
   the frame in BATON.  */

static CORE_ADDR
dwarf_expr_frame_cfa (void *baton)
{
  struct dwarf_expr_baton *debaton = (struct dwarf_expr_baton *) baton;
  return dwarf2_frame_cfa (debaton->frame);
}

/* Using the objfile specified in BATON, find the address for the
   current thread's thread-local storage with offset OFFSET.  */
static CORE_ADDR
dwarf_expr_tls_address (void *baton, CORE_ADDR offset)
{
  struct dwarf_expr_baton *debaton = (struct dwarf_expr_baton *) baton;

  return target_translate_tls_address (debaton->objfile, offset);
}

static CORE_ADDR
dwarf_expr_object_address (void *baton)
{
  struct dwarf_expr_baton *debaton = baton;

  /* The message is suppressed in DWARF_BLOCK_EXEC.  */
  if (debaton->object_address == 0)
    error (_("Cannot resolve DW_OP_push_object_address for a missing object"));

  return debaton->object_address;
}

/* Address of the variable we are currently referring to.  It is set from
   DW_TAG_variable's DW_AT_location (not DW_TAG_type's DW_AT_data_location) for
   DW_OP_push_object_address.  */

static CORE_ADDR object_address;

/* Callers use object_address_set while their callers use the result set so we
   cannot run the cleanup at the local block of our direct caller.  Still we
   should reset OBJECT_ADDRESS at least for the next GDB command.  */

static void
object_address_cleanup (void *prev_save_voidp)
{
  CORE_ADDR *prev_save = prev_save_voidp;

  object_address = *prev_save;
  xfree (prev_save);
}

/* Set the base address - DW_AT_location - of a variable.  It is being later
   used to derive other object addresses by DW_OP_push_object_address.

   It would be useful to sanity check ADDRESS - such as for some objects with
   unset value_raw_address - but some valid addresses may be zero (such as first
   objects in relocatable .o files).  */

void
object_address_set (CORE_ADDR address)
{
  CORE_ADDR *prev_save;

  prev_save = xmalloc (sizeof *prev_save);
  *prev_save = object_address;
  make_cleanup (object_address_cleanup, prev_save);

  object_address = address;
}

/* Evaluate DWARF expression at DATA ... DATA + SIZE with its result readable
   by dwarf_expr_fetch (RETVAL, 0).  FRAME parameter can be NULL to call
   get_selected_frame to find it.  Returned dwarf_expr_context freeing is
   pushed on the cleanup chain.  */

static struct dwarf_expr_context *
dwarf_expr_prep_ctx (struct frame_info *frame, gdb_byte *data,
		     unsigned short size, struct dwarf2_per_cu_data *per_cu)
{
  struct dwarf_expr_context *ctx;
  struct dwarf_expr_baton baton;

  if (!frame)
    frame = get_selected_frame (NULL);

  baton.frame = frame;
  baton.objfile = dwarf2_per_cu_objfile (per_cu);
  baton.object_address = object_address;

  ctx = new_dwarf_expr_context ();
  make_cleanup_free_dwarf_expr_context (ctx);

  ctx->gdbarch = get_objfile_arch (baton.objfile);
  ctx->addr_size = dwarf2_per_cu_addr_size (per_cu);
  ctx->baton = &baton;
  ctx->read_reg = dwarf_expr_read_reg;
  ctx->read_mem = dwarf_expr_read_mem;
  ctx->get_frame_base = dwarf_expr_frame_base;
  ctx->get_frame_cfa = dwarf_expr_frame_cfa;
  ctx->get_tls_address = dwarf_expr_tls_address;
  ctx->get_object_address = dwarf_expr_object_address;

  dwarf_expr_eval (ctx, data, size);

  /* It was used only during dwarf_expr_eval.  */
  ctx->baton = NULL;

  return ctx;
}

/* Evaluate DWARF expression at DLBATON expecting it produces exactly one
   CORE_ADDR result on the DWARF stack stack.  */

CORE_ADDR
dwarf_locexpr_baton_eval (struct dwarf2_locexpr_baton *dlbaton)
{
  struct dwarf_expr_context *ctx;
  CORE_ADDR retval;
  struct cleanup *back_to = make_cleanup (null_cleanup, 0);

  ctx = dwarf_expr_prep_ctx (NULL, dlbaton->data, dlbaton->size,
			     dlbaton->per_cu);
  if (ctx->num_pieces > 0)
    error (_("DW_OP_*piece is unsupported for DW_FORM_block"));

  retval = dwarf_expr_fetch (ctx, 0);

  if (ctx->location == DWARF_VALUE_REGISTER)
    {
      /* Inlined dwarf_expr_read_reg as we no longer have the baton. */

      int gdb_regnum = gdbarch_dwarf2_reg_to_regnum (ctx->gdbarch, retval);
      struct type *type = builtin_type (ctx->gdbarch)->builtin_data_ptr;
      struct frame_info *frame = get_selected_frame (NULL);

      retval = address_from_register (type, gdb_regnum, frame);
    }

  do_cleanups (back_to);

  return retval;
}

/* Evaluate DWARF location list at DLLBATON expecting it produces exactly one
   CORE_ADDR result stored to *ADDRP on the DWARF stack stack.  If the result
   could not be found return zero and keep *ADDRP unchanged.  */

int
dwarf_loclist_baton_eval (struct dwarf2_loclist_baton *dllbaton,
			  struct type *type, CORE_ADDR *addrp)
{
  struct frame_info *frame = get_selected_frame (NULL);
  gdb_byte *data;
  size_t size;
  struct value *val;

  if (!dllbaton)
    return 0;

  data = find_location_expression (dllbaton, &size,
				   get_frame_address_in_block (frame));
  if (data == NULL)
    return 0;

  val = dwarf2_evaluate_loc_desc (type, frame, data, size, dllbaton->per_cu);
  if (value_optimized_out (val))
    return 0;

  *addrp = value_as_address (val);
  return 1;
}

struct piece_closure
{
  /* The number of pieces used to describe this variable.  */
  int n_pieces;

  /* The architecture, used only for DWARF_VALUE_STACK.  */
  struct gdbarch *arch;

  /* The pieces themselves.  */
  struct dwarf_expr_piece *pieces;
};

/* Allocate a closure for a value formed from separately-described
   PIECES.  */

static struct piece_closure *
allocate_piece_closure (int n_pieces, struct dwarf_expr_piece *pieces,
			struct gdbarch *arch)
{
  struct piece_closure *c = XZALLOC (struct piece_closure);

  c->n_pieces = n_pieces;
  c->arch = arch;
  c->pieces = XCALLOC (n_pieces, struct dwarf_expr_piece);

  memcpy (c->pieces, pieces, n_pieces * sizeof (struct dwarf_expr_piece));

  return c;
}

static void
read_pieced_value (struct value *v)
{
  int i;
  long offset = 0;
  gdb_byte *contents;
  struct piece_closure *c = (struct piece_closure *) value_computed_closure (v);
  struct frame_info *frame = frame_find_by_id (VALUE_FRAME_ID (v));

  contents = value_contents_raw (v);
  for (i = 0; i < c->n_pieces; i++)
    {
      struct dwarf_expr_piece *p = &c->pieces[i];
      switch (p->location)
	{
	case DWARF_VALUE_REGISTER:
	  {
	    struct gdbarch *arch = get_frame_arch (frame);
	    int gdb_regnum = gdbarch_dwarf2_reg_to_regnum (arch,
							   p->v.expr.value);
	    int reg_offset = 0;

	    if (gdbarch_byte_order (arch) == BFD_ENDIAN_BIG
		&& p->size < register_size (arch, gdb_regnum))
	      /* Big-endian, and we want less than full size.  */
	      reg_offset = register_size (arch, gdb_regnum) - p->size;

	    get_frame_register_bytes (frame, gdb_regnum, reg_offset, p->size,
				      contents + offset);
	  }
	  break;

	case DWARF_VALUE_MEMORY:
	  if (p->v.expr.in_stack_memory)
	    read_stack (p->v.expr.value, contents + offset, p->size);
	  else
	    read_memory (p->v.expr.value, contents + offset, p->size);
	  break;

	case DWARF_VALUE_STACK:
	  {
	    gdb_byte bytes[sizeof (ULONGEST)];
	    size_t n;
	    int addr_size = gdbarch_addr_bit (c->arch) / 8;
	    store_unsigned_integer (bytes, addr_size,
				    gdbarch_byte_order (c->arch),
				    p->v.expr.value);
	    n = p->size;
	    if (n > addr_size)
	      n = addr_size;
	    memcpy (contents + offset, bytes, n);
	  }
	  break;

	case DWARF_VALUE_LITERAL:
	  {
	    size_t n = p->size;
	    if (n > p->v.literal.length)
	      n = p->v.literal.length;
	    memcpy (contents + offset, p->v.literal.data, n);
	  }
	  break;

	default:
	  internal_error (__FILE__, __LINE__, _("invalid location type"));
	}
      offset += p->size;
    }
}

static void
write_pieced_value (struct value *to, struct value *from)
{
  int i;
  long offset = 0;
  gdb_byte *contents;
  struct piece_closure *c = (struct piece_closure *) value_computed_closure (to);
  struct frame_info *frame = frame_find_by_id (VALUE_FRAME_ID (to));

  if (frame == NULL)
    {
      set_value_optimized_out (to, 1);
      return;
    }

  contents = value_contents_raw (from);
  for (i = 0; i < c->n_pieces; i++)
    {
      struct dwarf_expr_piece *p = &c->pieces[i];
      switch (p->location)
	{
	case DWARF_VALUE_REGISTER:
	  {
	    struct gdbarch *arch = get_frame_arch (frame);
	    int gdb_regnum = gdbarch_dwarf2_reg_to_regnum (arch, p->v.expr.value);
	    int reg_offset = 0;

	    if (gdbarch_byte_order (arch) == BFD_ENDIAN_BIG
		&& p->size < register_size (arch, gdb_regnum))
	      /* Big-endian, and we want less than full size.  */
	      reg_offset = register_size (arch, gdb_regnum) - p->size;

	    put_frame_register_bytes (frame, gdb_regnum, reg_offset, p->size,
				      contents + offset);
	  }
	  break;
	case DWARF_VALUE_MEMORY:
	  write_memory (p->v.expr.value, contents + offset, p->size);
	  break;
	default:
	  set_value_optimized_out (to, 1);
	  return;
	}
      offset += p->size;
    }
}

static void *
copy_pieced_value_closure (struct value *v)
{
  struct piece_closure *c = (struct piece_closure *) value_computed_closure (v);
  
  return allocate_piece_closure (c->n_pieces, c->pieces, c->arch);
}

static void
free_pieced_value_closure (struct value *v)
{
  struct piece_closure *c = (struct piece_closure *) value_computed_closure (v);

  xfree (c->pieces);
  xfree (c);
}

/* Functions for accessing a variable described by DW_OP_piece.  */
static struct lval_funcs pieced_value_funcs = {
  read_pieced_value,
  write_pieced_value,
  copy_pieced_value_closure,
  free_pieced_value_closure
};

/* Evaluate a location description, starting at DATA and with length
   SIZE, to find the current location of variable VAR in the context
   of FRAME.  */
static struct value *
dwarf2_evaluate_loc_desc (struct type *type, struct frame_info *frame,
			  gdb_byte *data, unsigned short size,
			  struct dwarf2_per_cu_data *per_cu)
{
  struct value *retval;
  struct dwarf_expr_context *ctx;
  struct cleanup *old_chain = make_cleanup (null_cleanup, 0);

  if (size == 0)
    {
      retval = allocate_value (type);
      VALUE_LVAL (retval) = not_lval;
      set_value_optimized_out (retval, 1);
      return retval;
    }

  ctx = dwarf_expr_prep_ctx (frame, data, size, per_cu);

  if (ctx->num_pieces > 0)
    {
      struct piece_closure *c;
      struct frame_id frame_id = get_frame_id (frame);

      c = allocate_piece_closure (ctx->num_pieces, ctx->pieces, ctx->gdbarch);
      retval = allocate_computed_value (type, &pieced_value_funcs, c);
      VALUE_FRAME_ID (retval) = frame_id;
    }
  else
    {
      switch (ctx->location)
	{
	case DWARF_VALUE_REGISTER:
	  {
	    struct gdbarch *arch = get_frame_arch (frame);
	    CORE_ADDR dwarf_regnum = dwarf_expr_fetch (ctx, 0);
	    int gdb_regnum = gdbarch_dwarf2_reg_to_regnum (arch, dwarf_regnum);
	    retval = value_from_register (type, gdb_regnum, frame);
	  }
	  break;

	case DWARF_VALUE_MEMORY:
	  {
	    CORE_ADDR address = dwarf_expr_fetch (ctx, 0);
	    int in_stack_memory = dwarf_expr_fetch_in_stack_memory (ctx, 0);

	    /* object_address_set called here is required in ALLOCATE_VALUE's
	       CHECK_TYPEDEF for the object's possible
	       DW_OP_push_object_address.  */
	    object_address_set (address);

	    retval = allocate_value (type);
	    VALUE_LVAL (retval) = lval_memory;
	    set_value_lazy (retval, 1);
	    if (in_stack_memory)
	      set_value_stack (retval, 1);
	    set_value_address (retval, address);
	  }
	  break;

	case DWARF_VALUE_STACK:
	  {
	    gdb_byte bytes[sizeof (ULONGEST)];
	    ULONGEST value = (ULONGEST) dwarf_expr_fetch (ctx, 0);
	    bfd_byte *contents;
	    size_t n = ctx->addr_size;

	    store_unsigned_integer (bytes, ctx->addr_size,
				    gdbarch_byte_order (ctx->gdbarch),
				    value);
	    retval = allocate_value (type);
	    contents = value_contents_raw (retval);
	    if (n > TYPE_LENGTH (type))
	      n = TYPE_LENGTH (type);
	    memcpy (contents, bytes, n);
	  }
	  break;

	case DWARF_VALUE_LITERAL:
	  {
	    bfd_byte *contents;
	    size_t n = ctx->len;

	    retval = allocate_value (type);
	    contents = value_contents_raw (retval);
	    if (n > TYPE_LENGTH (type))
	      n = TYPE_LENGTH (type);
	    memcpy (contents, ctx->data, n);
	  }
	  break;

	default:
	  internal_error (__FILE__, __LINE__, _("invalid location type"));
	}
    }

  set_value_initialized (retval, ctx->initialized);

  do_cleanups (old_chain);

  return retval;
}

/* Helper functions and baton for dwarf2_loc_desc_needs_frame.  */

struct needs_frame_baton
{
  int needs_frame;
};

/* Reads from registers do require a frame.  */
static CORE_ADDR
needs_frame_read_reg (void *baton, int regnum)
{
  struct needs_frame_baton *nf_baton = baton;
  nf_baton->needs_frame = 1;
  return 1;
}

/* Reads from memory do not require a frame.  */
static void
needs_frame_read_mem (void *baton, gdb_byte *buf, CORE_ADDR addr, size_t len)
{
  memset (buf, 0, len);
}

/* Frame-relative accesses do require a frame.  */
static void
needs_frame_frame_base (void *baton, gdb_byte **start, size_t * length)
{
  static gdb_byte lit0 = DW_OP_lit0;
  struct needs_frame_baton *nf_baton = baton;

  *start = &lit0;
  *length = 1;

  nf_baton->needs_frame = 1;
}

/* CFA accesses require a frame.  */

static CORE_ADDR
needs_frame_frame_cfa (void *baton)
{
  struct needs_frame_baton *nf_baton = baton;
  nf_baton->needs_frame = 1;
  return 1;
}

/* Thread-local accesses do require a frame.  */
static CORE_ADDR
needs_frame_tls_address (void *baton, CORE_ADDR offset)
{
  struct needs_frame_baton *nf_baton = baton;
  nf_baton->needs_frame = 1;
  return 1;
}

/* Return non-zero iff the location expression at DATA (length SIZE)
   requires a frame to evaluate.  */

static int
dwarf2_loc_desc_needs_frame (gdb_byte *data, unsigned short size,
			     struct dwarf2_per_cu_data *per_cu)
{
  struct needs_frame_baton baton;
  struct dwarf_expr_context *ctx;
  int in_reg;
  struct cleanup *old_chain;

  baton.needs_frame = 0;

  ctx = new_dwarf_expr_context ();
  old_chain = make_cleanup_free_dwarf_expr_context (ctx);

  ctx->gdbarch = get_objfile_arch (dwarf2_per_cu_objfile (per_cu));
  ctx->addr_size = dwarf2_per_cu_addr_size (per_cu);
  ctx->baton = &baton;
  ctx->read_reg = needs_frame_read_reg;
  ctx->read_mem = needs_frame_read_mem;
  ctx->get_frame_base = needs_frame_frame_base;
  ctx->get_frame_cfa = needs_frame_frame_cfa;
  ctx->get_tls_address = needs_frame_tls_address;

  dwarf_expr_eval (ctx, data, size);

  in_reg = ctx->location == DWARF_VALUE_REGISTER;

  if (ctx->num_pieces > 0)
    {
      int i;

      /* If the location has several pieces, and any of them are in
         registers, then we will need a frame to fetch them from.  */
      for (i = 0; i < ctx->num_pieces; i++)
        if (ctx->pieces[i].location == DWARF_VALUE_REGISTER)
          in_reg = 1;
    }

  do_cleanups (old_chain);

  return baton.needs_frame || in_reg;
}

static void
dwarf2_tracepoint_var_ref (struct symbol *symbol, struct gdbarch *gdbarch,
			   struct agent_expr *ax, struct axs_value *value,
			   gdb_byte *data, int size)
{
  if (size == 0)
    error (_("Symbol \"%s\" has been optimized out."),
	   SYMBOL_PRINT_NAME (symbol));

  if (size == 1
      && data[0] >= DW_OP_reg0
      && data[0] <= DW_OP_reg31)
    {
      value->kind = axs_lvalue_register;
      value->u.reg = data[0] - DW_OP_reg0;
    }
  else if (data[0] == DW_OP_regx)
    {
      ULONGEST reg;
      read_uleb128 (data + 1, data + size, &reg);
      value->kind = axs_lvalue_register;
      value->u.reg = reg;
    }
  else if (data[0] == DW_OP_fbreg)
    {
      struct block *b;
      struct symbol *framefunc;
      int frame_reg = 0;
      LONGEST frame_offset;
      gdb_byte *buf_end;
      gdb_byte *base_data;
      size_t base_size;
      LONGEST base_offset = 0;

      b = block_for_pc (ax->scope);

      if (!b)
	error (_("No block found for address"));

      framefunc = block_linkage_function (b);

      if (!framefunc)
	error (_("No function found for block"));

      dwarf_expr_frame_base_1 (framefunc, ax->scope,
			       &base_data, &base_size);

      if (base_data[0] >= DW_OP_breg0
	   && base_data[0] <= DW_OP_breg31)
	{
	  frame_reg = base_data[0] - DW_OP_breg0;
	  buf_end = read_sleb128 (base_data + 1, base_data + base_size, &base_offset);
	  if (buf_end != base_data + base_size)
	    error (_("Unexpected opcode after DW_OP_breg%u for symbol \"%s\"."),
		   frame_reg, SYMBOL_PRINT_NAME (symbol));
	}
      else
	{
	  /* We don't know what to do with the frame base expression,
	     so we can't trace this variable; give up.  */
	  error (_("Cannot generate expression to collect symbol \"%s\"; DWARF 2 encoding not handled"),
		 SYMBOL_PRINT_NAME (symbol));
	}

      buf_end = read_sleb128 (data + 1, data + size, &frame_offset);
      if (buf_end != data + size)
	error (_("Unexpected opcode after DW_OP_fbreg for symbol \"%s\"."),
	       SYMBOL_PRINT_NAME (symbol));

      ax_reg (ax, frame_reg);
      ax_const_l (ax, base_offset + frame_offset);
      ax_simple (ax, aop_add);

      value->kind = axs_lvalue_memory;
    }
  else if (data[0] >= DW_OP_breg0
	   && data[0] <= DW_OP_breg31)
    {
      unsigned int reg;
      LONGEST offset;
      gdb_byte *buf_end;

      reg = data[0] - DW_OP_breg0;
      buf_end = read_sleb128 (data + 1, data + size, &offset);
      if (buf_end != data + size)
	error (_("Unexpected opcode after DW_OP_breg%u for symbol \"%s\"."),
	       reg, SYMBOL_PRINT_NAME (symbol));

      ax_reg (ax, reg);
      ax_const_l (ax, offset);
      ax_simple (ax, aop_add);

      value->kind = axs_lvalue_memory;
    }
  else
    error (_("Unsupported DWARF opcode 0x%x in the location of \"%s\"."),
	   data[0], SYMBOL_PRINT_NAME (symbol));
}

/* Return the value of SYMBOL in FRAME using the DWARF-2 expression
   evaluator to calculate the location.  */
static struct value *
locexpr_read_variable (struct symbol *symbol, struct frame_info *frame)
{
  struct dwarf2_locexpr_baton *dlbaton = SYMBOL_LOCATION_BATON (symbol);
  struct value *val;
  val = dwarf2_evaluate_loc_desc (SYMBOL_TYPE (symbol), frame, dlbaton->data,
				  dlbaton->size, dlbaton->per_cu);

  return val;
}

/* Return non-zero iff we need a frame to evaluate SYMBOL.  */
static int
locexpr_read_needs_frame (struct symbol *symbol)
{
  struct dwarf2_locexpr_baton *dlbaton = SYMBOL_LOCATION_BATON (symbol);
  return dwarf2_loc_desc_needs_frame (dlbaton->data, dlbaton->size,
				      dlbaton->per_cu);
}

/* Print a natural-language description of SYMBOL to STREAM.  */
static int
locexpr_describe_location (struct symbol *symbol, struct ui_file *stream)
{
  /* FIXME: be more extensive.  */
  struct dwarf2_locexpr_baton *dlbaton = SYMBOL_LOCATION_BATON (symbol);
  int addr_size = dwarf2_per_cu_addr_size (dlbaton->per_cu);

  if (dlbaton->size == 1
      && dlbaton->data[0] >= DW_OP_reg0
      && dlbaton->data[0] <= DW_OP_reg31)
    {
      struct objfile *objfile = dwarf2_per_cu_objfile (dlbaton->per_cu);
      struct gdbarch *gdbarch = get_objfile_arch (objfile);
      int regno = gdbarch_dwarf2_reg_to_regnum (gdbarch,
						dlbaton->data[0] - DW_OP_reg0);
      fprintf_filtered (stream,
			"a variable in register %s",
			gdbarch_register_name (gdbarch, regno));
      return 1;
    }

  /* The location expression for a TLS variable looks like this (on a
     64-bit LE machine):

     DW_AT_location    : 10 byte block: 3 4 0 0 0 0 0 0 0 e0
                        (DW_OP_addr: 4; DW_OP_GNU_push_tls_address)
     
     0x3 is the encoding for DW_OP_addr, which has an operand as long
     as the size of an address on the target machine (here is 8
     bytes).  0xe0 is the encoding for DW_OP_GNU_push_tls_address.
     The operand represents the offset at which the variable is within
     the thread local storage.  */

  if (dlbaton->size > 1 
      && dlbaton->data[dlbaton->size - 1] == DW_OP_GNU_push_tls_address)
    if (dlbaton->data[0] == DW_OP_addr)
      {
	struct objfile *objfile = dwarf2_per_cu_objfile (dlbaton->per_cu);
	struct gdbarch *gdbarch = get_objfile_arch (objfile);
	CORE_ADDR offset = dwarf2_read_address (gdbarch,
						&dlbaton->data[1],
						&dlbaton->data[dlbaton->size - 1],
						addr_size);
	fprintf_filtered (stream, 
			  "a thread-local variable at offset %s in the "
			  "thread-local storage for `%s'",
			  paddress (gdbarch, offset), objfile->name);
	return 1;
      }
  

  fprintf_filtered (stream,
		    "a variable with complex or multiple locations (DWARF2)");
  return 1;
}


/* Describe the location of SYMBOL as an agent value in VALUE, generating
   any necessary bytecode in AX.

   NOTE drow/2003-02-26: This function is extremely minimal, because
   doing it correctly is extremely complicated and there is no
   publicly available stub with tracepoint support for me to test
   against.  When there is one this function should be revisited.  */

static void
locexpr_tracepoint_var_ref (struct symbol *symbol, struct gdbarch *gdbarch,
			    struct agent_expr *ax, struct axs_value *value)
{
  struct dwarf2_locexpr_baton *dlbaton = SYMBOL_LOCATION_BATON (symbol);

  dwarf2_tracepoint_var_ref (symbol, gdbarch, ax, value,
			     dlbaton->data, dlbaton->size);
}

/* The set of location functions used with the DWARF-2 expression
   evaluator.  */
const struct symbol_computed_ops dwarf2_locexpr_funcs = {
  locexpr_read_variable,
  locexpr_read_needs_frame,
  locexpr_describe_location,
  locexpr_tracepoint_var_ref
};


/* Wrapper functions for location lists.  These generally find
   the appropriate location expression and call something above.  */

/* Return the value of SYMBOL in FRAME using the DWARF-2 expression
   evaluator to calculate the location.  */
static struct value *
loclist_read_variable (struct symbol *symbol, struct frame_info *frame)
{
  struct dwarf2_loclist_baton *dlbaton = SYMBOL_LOCATION_BATON (symbol);
  struct value *val;
  gdb_byte *data;
  size_t size;

  data = find_location_expression (dlbaton, &size,
				   frame ? get_frame_address_in_block (frame)
				   : 0);
  if (data == NULL)
    {
      val = allocate_value (SYMBOL_TYPE (symbol));
      VALUE_LVAL (val) = not_lval;
      set_value_optimized_out (val, 1);
    }
  else
    val = dwarf2_evaluate_loc_desc (SYMBOL_TYPE (symbol), frame, data, size,
				    dlbaton->per_cu);

  return val;
}

/* Return non-zero iff we need a frame to evaluate SYMBOL.  */
static int
loclist_read_needs_frame (struct symbol *symbol)
{
  /* If there's a location list, then assume we need to have a frame
     to choose the appropriate location expression.  With tracking of
     global variables this is not necessarily true, but such tracking
     is disabled in GCC at the moment until we figure out how to
     represent it.  */

  return 1;
}

/* Print a natural-language description of SYMBOL to STREAM.  */
static int
loclist_describe_location (struct symbol *symbol, struct ui_file *stream)
{
  /* FIXME: Could print the entire list of locations.  */
  fprintf_filtered (stream, _("a variable with multiple locations"));
  return 1;
}

/* Describe the location of SYMBOL as an agent value in VALUE, generating
   any necessary bytecode in AX.  */
static void
loclist_tracepoint_var_ref (struct symbol *symbol, struct gdbarch *gdbarch,
			    struct agent_expr *ax, struct axs_value *value)
{
  struct dwarf2_loclist_baton *dlbaton = SYMBOL_LOCATION_BATON (symbol);
  gdb_byte *data;
  size_t size;

  data = find_location_expression (dlbaton, &size, ax->scope);
  if (data == NULL)
    error (_("Variable \"%s\" is not available."), SYMBOL_PRINT_NAME (symbol));

  dwarf2_tracepoint_var_ref (symbol, gdbarch, ax, value, data, size);
}

/* The set of location functions used with the DWARF-2 location lists.  */
const struct symbol_computed_ops dwarf2_loclist_funcs = {
  loclist_read_variable,
  loclist_read_needs_frame,
  loclist_describe_location,
  loclist_tracepoint_var_ref
};

static struct value *
missing_read_variable (struct symbol *symbol, struct frame_info *frame)
{
  struct dwarf2_loclist_baton *dlbaton = SYMBOL_LOCATION_BATON (symbol);

  gdb_assert (dlbaton == NULL);
  error (_("Unable to resolve variable \"%s\""), SYMBOL_PRINT_NAME (symbol));
}

static int
missing_read_needs_frame (struct symbol *symbol)
{
  return 0;
}

static int
missing_describe_location (struct symbol *symbol, struct ui_file *stream)
{
  fprintf_filtered (stream, _("a variable we are unable to resolve"));
  return 1;
}

static void
missing_tracepoint_var_ref (struct symbol *symbol, struct gdbarch *gdbarch,
			    struct agent_expr *ax, struct axs_value *value)
{
  struct dwarf2_loclist_baton *dlbaton = SYMBOL_LOCATION_BATON (symbol);

  gdb_assert (dlbaton == NULL);
  error (_("Unable to resolve variable \"%s\""), SYMBOL_PRINT_NAME (symbol));
}

/* The set of location functions used with the DWARF-2 evaluator when we are
   unable to resolve the symbols.  */
const struct symbol_computed_ops dwarf2_missing_funcs = {
  missing_read_variable,
  missing_read_needs_frame,
  missing_describe_location,
  missing_tracepoint_var_ref
};
