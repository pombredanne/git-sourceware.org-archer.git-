#...
\.bss1 *0x0*20000 *0x10
#...
\.bss2 *0x0*20000 *0x30 load address 0x0*20010
#...
\.bss3 *0x0*20000 *0x20 load address 0x0*20040
#...
.*0x0+020030[ 	]*end_of_bss_overlays.*
#...
\.mtext *0x0*10000 *0x20 load address 0x0*30000
#...
\.mbss *0x0*20030 *0x230 load address 0x0*20060
#...
\.text1 *0x0*10020 *0x80 load address 0x0*30020
#...
\.text2 *0x0*10020 *0x40 load address 0x0*300a0
#...
\.text3 *0x0*10020 *0x20 load address 0x0*300e0
#...
.*0x0+0100a0[ 	]*end_of_text_overlays.*
#...
\.data1 *0x0*20260 *0x30 load address 0x0*30100
#...
\.data2 *0x0*20260 *0x40 load address 0x0*30130
#...
\.data3 *0x0*20260 *0x50 load address 0x0*30170
#...
.*0x0+0202b0[ 	]*end_of_data_overlays.*
#pass