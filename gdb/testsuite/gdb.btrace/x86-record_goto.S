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
   gcc -S -g record_goto.c -o x86-record_goto.S  */

	.file	"record_goto.c"
	.section	.debug_abbrev,"",@progbits
.Ldebug_abbrev0:
	.section	.debug_info,"",@progbits
.Ldebug_info0:
	.section	.debug_line,"",@progbits
.Ldebug_line0:
	.text
.Ltext0:
.globl fun1
	.type	fun1, @function
fun1:
.LFB0:
	.file 1 "record_goto.c"
	.loc 1 22 0
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	movq	%rsp, %rbp
	.cfi_offset 6, -16
	.cfi_def_cfa_register 6
	.loc 1 23 0
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE0:
	.size	fun1, .-fun1
.globl fun2
	.type	fun2, @function
fun2:
.LFB1:
	.loc 1 27 0
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	movq	%rsp, %rbp
	.cfi_offset 6, -16
	.cfi_def_cfa_register 6
	.loc 1 28 0
	call	fun1
	.loc 1 29 0
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE1:
	.size	fun2, .-fun2
.globl fun3
	.type	fun3, @function
fun3:
.LFB2:
	.loc 1 33 0
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	movq	%rsp, %rbp
	.cfi_offset 6, -16
	.cfi_def_cfa_register 6
	.loc 1 34 0
	call	fun1
	.loc 1 35 0
	call	fun2
	.loc 1 36 0
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2:
	.size	fun3, .-fun3
.globl fun4
	.type	fun4, @function
fun4:
.LFB3:
	.loc 1 40 0
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	movq	%rsp, %rbp
	.cfi_offset 6, -16
	.cfi_def_cfa_register 6
	.loc 1 41 0
	call	fun1
	.loc 1 42 0
	call	fun2
	.loc 1 43 0
	call	fun3
	.loc 1 44 0
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE3:
	.size	fun4, .-fun4
.globl main
	.type	main, @function
main:
.LFB4:
	.loc 1 48 0
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	movq	%rsp, %rbp
	.cfi_offset 6, -16
	.cfi_def_cfa_register 6
	.loc 1 49 0
	call	fun4
	.loc 1 50 0
	movl	$0, %eax
	.loc 1 51 0
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE4:
	.size	main, .-main
.Letext0:
	.section	.debug_info
	.long	0xbc
	.value	0x3
	.long	.Ldebug_abbrev0
	.byte	0x8
	.uleb128 0x1
	.long	.LASF4
	.byte	0x1
	.long	.LASF5
	.long	.LASF6
	.quad	.Ltext0
	.quad	.Letext0
	.long	.Ldebug_line0
	.uleb128 0x2
	.byte	0x1
	.long	.LASF0
	.byte	0x1
	.byte	0x15
	.byte	0x1
	.quad	.LFB0
	.quad	.LFE0
	.byte	0x1
	.byte	0x9c
	.uleb128 0x2
	.byte	0x1
	.long	.LASF1
	.byte	0x1
	.byte	0x1a
	.byte	0x1
	.quad	.LFB1
	.quad	.LFE1
	.byte	0x1
	.byte	0x9c
	.uleb128 0x2
	.byte	0x1
	.long	.LASF2
	.byte	0x1
	.byte	0x20
	.byte	0x1
	.quad	.LFB2
	.quad	.LFE2
	.byte	0x1
	.byte	0x9c
	.uleb128 0x2
	.byte	0x1
	.long	.LASF3
	.byte	0x1
	.byte	0x27
	.byte	0x1
	.quad	.LFB3
	.quad	.LFE3
	.byte	0x1
	.byte	0x9c
	.uleb128 0x3
	.byte	0x1
	.long	.LASF7
	.byte	0x1
	.byte	0x2f
	.byte	0x1
	.long	0xb8
	.quad	.LFB4
	.quad	.LFE4
	.byte	0x1
	.byte	0x9c
	.uleb128 0x4
	.byte	0x4
	.byte	0x5
	.string	"int"
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
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x1
	.uleb128 0x40
	.uleb128 0xa
	.byte	0x0
	.byte	0x0
	.uleb128 0x3
	.uleb128 0x2e
	.byte	0x0
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
	.uleb128 0x4
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
	.byte	0x0
	.section	.debug_pubnames,"",@progbits
	.long	0x3b
	.value	0x2
	.long	.Ldebug_info0
	.long	0xc0
	.long	0x2d
	.string	"fun1"
	.long	0x48
	.string	"fun2"
	.long	0x63
	.string	"fun3"
	.long	0x7e
	.string	"fun4"
	.long	0x99
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
.LASF3:
	.string	"fun4"
.LASF5:
	.string	"record_goto.c"
.LASF4:
	.string	"GNU C 4.4.4 20100726 (Red Hat 4.4.4-13)"
.LASF7:
	.string	"main"
.LASF1:
	.string	"fun2"
.LASF0:
	.string	"fun1"
.LASF6:
	.string	"/users/mmetzger/gdb/gerrit/git/gdb/testsuite/gdb.btrace"
.LASF2:
	.string	"fun3"
	.ident	"GCC: (GNU) 4.4.4 20100726 (Red Hat 4.4.4-13)"
	.section	.note.GNU-stack,"",@progbits
