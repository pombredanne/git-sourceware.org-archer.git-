#source: maxpage1.s
#as: --32
#ld: -z max-page-size=0x200000 -T maxpage4.t
#readelf: -l --wide
#target: x86_64-*-linux* i?86-*-linux-gnu

#...
  LOAD+.*0x200000
#pass