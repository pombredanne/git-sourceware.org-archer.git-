/* This testcase is part of GDB, the GNU debugger.

   Copyright 2013 Free Software Foundation, Inc.

   Contributed by Intel Corp. <markus.t.metzger@intel.com>

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.


   This file has been generated using:
   gcc -S -O2 -g x86-tailcall.c -o x86-tailcall.S  */

	.file	"x86-tailcall.c"
	.section	.debug_abbrev,"",@progbits
.Ldebug_abbrev0:
	.section	.debug_info,"",@progbits
.Ldebug_info0:
	.section	.debug_line,"",@progbits
.Ldebug_line0:
	.text
.Ltext0:
	.p2align 4,,15
	.type	bar, @function
bar:
.LFB0:
	.file 1 "gdb/testsuite/gdb.btrace/x86-tailcall.c"
	.loc 1 22 0
	.cfi_startproc
	.loc 1 24 0
	movl	$42, %eax
	ret
	.cfi_endproc
.LFE0:
	.size	bar, .-bar
	.p2align 4,,15
	.type	foo, @function
foo:
.LFB1:
	.loc 1 28 0
	.cfi_startproc
	.loc 1 29 0
	jmp	bar
	.cfi_endproc
.LFE1:
	.size	foo, .-foo
	.p2align 4,,15
.globl main
	.type	main, @function
main:
.LFB2:
	.loc 1 34 0
	.cfi_startproc
	.loc 1 37 0
	call	foo
.LVL0:
	addl	$1, %eax
.LVL1:
	.loc 1 39 0
	ret
	.cfi_endproc
.LFE2:
	.size	main, .-main
.Letext0:
	.section	.debug_loc,"",@progbits
.Ldebug_loc0:
.LLST0:
	.quad	.LVL0-.Ltext0
	.quad	.LVL1-.Ltext0
	.value	0x3
	.byte	0x70
	.sleb128 1
	.byte	0x9f
	.quad	.LVL1-.Ltext0
	.quad	.LFE2-.Ltext0
	.value	0x1
	.byte	0x50
	.quad	0x0
	.quad	0x0
	.section	.debug_info
	.long	0x9c
	.value	0x3
	.long	.Ldebug_abbrev0
	.byte	0x8
	.uleb128 0x1
	.long	.LASF0
	.byte	0x1
	.long	.LASF1
	.long	.LASF2
	.quad	.Ltext0
	.quad	.Letext0
	.long	.Ldebug_line0
	.uleb128 0x2
	.string	"bar"
	.byte	0x1
	.byte	0x15
	.byte	0x1
	.long	0x4b
	.quad	.LFB0
	.quad	.LFE0
	.byte	0x1
	.byte	0x9c
	.uleb128 0x3
	.byte	0x4
	.byte	0x5
	.string	"int"
	.uleb128 0x2
	.string	"foo"
	.byte	0x1
	.byte	0x1b
	.byte	0x1
	.long	0x4b
	.quad	.LFB1
	.quad	.LFE1
	.byte	0x1
	.byte	0x9c
	.uleb128 0x4
	.byte	0x1
	.long	.LASF3
	.byte	0x1
	.byte	0x21
	.byte	0x1
	.long	0x4b
	.quad	.LFB2
	.quad	.LFE2
	.byte	0x1
	.byte	0x9c
	.uleb128 0x5
	.long	.LASF4
	.byte	0x1
	.byte	0x23
	.long	0x4b
	.long	.LLST0
	.byte	0x0
	.byte	0x0
	.section	.debug_abbrev
	.uleb128 0x1
	.uleb128 0x11
	.byte	0x1
	.uleb128 0x25
	.uleb128 0xe
	.uleb128 0x13
	.uleb128 0xb
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x1b
	.uleb128 0xe
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x1
	.uleb128 0x10
	.uleb128 0x6
	.byte	0x0
	.byte	0x0
	.uleb128 0x2
	.uleb128 0x2e
	.byte	0x0
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x27
	.uleb128 0xc
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x1
	.uleb128 0x40
	.uleb128 0xa
	.byte	0x0
	.byte	0x0
	.uleb128 0x3
	.uleb128 0x24
	.byte	0x0
	.uleb128 0xb
	.uleb128 0xb
	.uleb128 0x3e
	.uleb128 0xb
	.uleb128 0x3
	.uleb128 0x8
	.byte	0x0
	.byte	0x0
	.uleb128 0x4
	.uleb128 0x2e
	.byte	0x1
	.uleb128 0x3f
	.uleb128 0xc
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x27
	.uleb128 0xc
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x1
	.uleb128 0x40
	.uleb128 0xa
	.byte	0x0
	.byte	0x0
	.uleb128 0x5
	.uleb128 0x34
	.byte	0x0
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x2
	.uleb128 0x6
	.byte	0x0
	.byte	0x0
	.byte	0x0
	.section	.debug_pubnames,"",@progbits
	.long	0x17
	.value	0x2
	.long	.Ldebug_info0
	.long	0xa0
	.long	0x70
	.string	"main"
	.long	0x0
	.section	.debug_aranges,"",@progbits
	.long	0x2c
	.value	0x2
	.long	.Ldebug_info0
	.byte	0x8
	.byte	0x0
	.value	0x0
	.value	0x0
	.quad	.Ltext0
	.quad	.Letext0-.Ltext0
	.quad	0x0
	.quad	0x0
	.section	.debug_str,"MS",@progbits,1
.LASF1:
	.string	"gdb/testsuite/gdb.btrace/x86-tailcall.c"
.LASF4:
	.string	"answer"
.LASF0:
	.string	"GNU C 4.4.4 20100726 (Red Hat 4.4.4-13)"
.LASF3:
	.string	"main"
.LASF2:
	.string	"/users/mmetzger/gdb/gerrit/git"
	.ident	"GCC: (GNU) 4.4.4 20100726 (Red Hat 4.4.4-13)"
	.section	.note.GNU-stack,"",@progbits
