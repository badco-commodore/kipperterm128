Assuming I didn't mess it up, this should be the codebase for Kipperterm for the Commodore 128.  This was ported from the original KIPPTERTERM code base written by Jonno Jones.

I originally built this on a PC so the init.cmd file is a setup file to ensure the makes build properly.  Set the values appropriately for your environment and execute the init.cmd before you try to run the make files.

You will require the c1541 program to create the d64, and will need cc65 to cross compile the source code.

There is a lot of code from the original Kipperterm still in place.  There are some #*# files floating around as well, as I used Emacs to edit so I will try to clean them up.. but they are not needed.

Its been over a year since I worked on this, so I will try to document the best I can. One of the main changes is that it is building with the C128fullprg.cfg as the CC65 setup.  Obviously the 128 has different memory locations etc from the 128.

Kipperterm128 fully resides in BANK0 of the C128, it uses BANK 1 for its buffer.

You will also find chunks of code simply commented out, they are just remnants of things for Kipperterm 2 for the 64 that weren't really relevant, or were replaced completely by other code during the port.

Some of the code was literally stolen from old code I wrote as a teenager, so don't beat me up too hard..buffer.s for example is nearly all code from stuff I wrote in the late 80s/early 90s.

HOW TO BUILD

You will need MAKE on your machine, (on my machine it was installed in the C:\contiki\gnuwin32\bin.. but your machien will be different and as mentioned above c1541 if you wish to create d64 containing the files.

You should just need to (after setting init.cmd properly and running it) 
cd into client/kipperterm128 
and run Make
make 

Assuming I haven't forgotten anything, that should be all you need to do.

