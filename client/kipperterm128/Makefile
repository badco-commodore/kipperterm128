AS=ca65
LD=ld65
CFLAGS=-Oirs -t $(TARGET)
AFLAGS= 

INCFILES=\
  ../inc/common.i\
  ../inc/commonprint.i\
  ../inc/net.i\
  ../inc/menu.i\
  ../inc/kipper_constants.i\
  ../inc/version.i\

IP65LIB=../ip65/ip65.lib
IP65TCPLIB=../ip65/ip65_tcp.lib
C64RRNETLIB=../drivers/c64rrnet.lib
IP65WIZNETLIB=../ip65/ip65_wiznet.lib
C64WIZNETLIB=../drivers/c64wiznet.lib


all: ip65 drivers  kipperterm128.prg kt2wiz.prg kipperterm128.d64 kt2wiz.d64


kipperterm128.o: kipperterm128.s $(INCFILES) ../inc/telnet.i ../inc/config_menu.i ../inc/gopher.i
	ruby -e "puts '.byte \"' + Time.now.strftime('%Y-%m-%d')+'\"'" > timestamp.i
	$(AS) $(AFLAGS) -o $@ $<

%.o: %.s $(INCFILES)
	$(AS) $(AFLAGS) $<

%.prg: %.o $(IP65LIB) $(C64RRNETLIB) $(INCFILES) ../cfg/c128fullprg.cfg
	$(LD) -m  $*.map -vm -C ../cfg/c128fullprg.cfg -o  $*.prg  $(AFLAGS) $< $(IP65TCPLIB) $(C64RRNETLIB)  
	ruby ../carts/set_ip_config.rb $@ mac auto

kt2wiz.prg: kipperterm128.o $(IP65WIZNETLIB) $(C64WIZNETLIB) $(INCFILES) ../cfg/c128fullprg.cfg
	$(LD) -m  kt2wiz.map -vm -C ../cfg/c128fullprg.cfg -o  kt2wiz.prg  $(AFLAGS) $< $(IP65WIZNETLIB) $(C64WIZNETLIB)  
	cp kt2wiz.prg ../../server/boot


kipperterm128.d64: kipperterm128.prg addresses.txt manual.txt abe.bas readme.bas
	c1541  -format kipperterm2,k2 d64 $@ -attach $@ -write kipperterm128.prg -write "addresses.txt" "addresses.txt,s" -write "manual.txt" "manual.txt,s" -write abe -write readme
	cp kipperterm128.d64 ../../server/boot

kt2wiz.d64: kt2wiz.prg addresses.txt abe.bas
	c1541  -format kt2wiz,kw d64 $@ -attach $@ -write kt2wiz.prg -write "addresses.txt" "addresses.txt,s" -write abe
	cp kt2wiz.d64 ../wiznet/

clean:
	rm -f *.o *.bin *.map *.prg *.pg2 *.dsk *.d64

distclean: clean
	rm -f *~

ip65:
	make -C ../ip65 all

drivers:
	make -C ../drivers all
