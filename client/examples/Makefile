CC=cl65
AS=ca65

DA=dasm
LD=ld65
CFLAGS=-Oirs -t $(TARGET)
AFLAGS=



IP65TCPLIB=../ip65/ip65_tcp.lib

C64RRNETLIB=../drivers/c64rrnet.lib

#NT2PLAY=nt2play.o

INCFILES=\
  ../inc/common.i\
  ../inc/commonprint.i\
  ../inc/net.i\


all: 	upnatom.prg upnatom.d64  webnoter.d64


%.o: %.s 
	$(AS) $(AFLAGS) $<

upnatom.o: upnatom.s sine_data.i 
	$(AS) $(AFLAGS) $<


sine_data.i: make_sine_data.rb
	ruby make_sine_data.rb



webnoter.o: webnoter.s form.html 
	$(AS) $(AFLAGS) $<


webnoter.prg: webnoter.o  $(IP65TCPLIB) $(C64RRNETLIB) $(INCFILES) ../cfg/c64prg.cfg
	$(LD) -m  webnoter.map -vm -C ../cfg/c64prg.cfg -o  webnoter.prg  $(AFLAGS) $< $(IP65TCPLIB) $(C64RRNETLIB) $(NT2PLAY)



upnatom.prg: upnatom.o  $(IP65TCPLIB) $(C64RRNETLIB) $(INCFILES) ../cfg/c64prg.cfg
	$(LD) -m  upnatom.map -vm -C ../cfg/c64prg.cfg -o  upnatom.prg  $(AFLAGS) $< $(IP65TCPLIB) $(C64RRNETLIB) $(NT2PLAY)

#	cp upnatom.prg ../../server/boot/

%.prg: %.o $(IP65TCPLIB) $(C64RRNETLIB) $(INCFILES) ../cfg/c64prg.cfg
	$(LD) -m  $*.map -vm -C ../cfg/c64prg.cfg -o  $*.prg  $(AFLAGS) $< $(IP65TCPLIB) $(C64RRNETLIB)  



upnatom.d64:  upnatom.prg url.cfg
	cp upnatom.prg autoexec.prg
	c1541  -format upnatom,up d64 $@ -attach $@  -write ../carts/kipperkart.prg "kipperkart.prg" -write autoexec.prg -write "url.cfg" "url.cfg,s"


webnoter.d64:  webnoter.prg 
	cp webnoter.prg autoexec.prg
	c1541  -format webnoter,wn d64 $@ -attach $@ -write ../carts/kipperkart.prg "kipperkart.prg" -write autoexec.prg 

httpd.prg: httpd.asm
	$(DA) httpd.asm -ohttpd.prg



%.d64:  %.prg 
	cp $*.prg autoexec.prg
	c1541  -format kipper,kp d64 $@ -attach $@ -write autoexec.prg 
	
clean:
	rm -f *.o *.pg2 *.prg *.map upnatom.d64

  
distclean: clean
	rm -f *~
