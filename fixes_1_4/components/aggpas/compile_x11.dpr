{skip}
program compile_x11;

uses
 Math ,
 SysUtils ,
 CTypes ,
 X ,
 Xlib ,
 Xutil ,
 Xatom ,
 keysym ,
 libc ;

BEGIN
 writeln('For compilation of AggPas on Linux X11 we need the following units:' );
 writeln('  Math' );
 writeln('  SysUtils' );
 writeln('  CTypes' );
 writeln('  X' );
 writeln('  Xlib' );
 writeln('  Xutil' );
 writeln('  Xatom' );
 writeln('  keysym' );
 writeln('  libc' );

END.