{%MainUnit definetemplates.pas}
Score:=100;
Targets:='*';
Add('rtl');
Add('packages/fcl-');

Score:=1;
Add('packages');
Add('packages/fv');  // better than packages/amunits (unit msgbox)
Add('packages/ncurses');  // better than packages/palmunits (unit from)
Add('packages/hash');  // better than packages/palmunits (unit crc)

Score:=-10;
Add('tests');
Add('packages/libgbafpc/examples');
Add('packages/ptc/examples');
Add('packages/ncurses/examples');
Add('packages/libndsfpc/examples');
Add('packages/regexpr/src/old');
Add('packages/amunits/src/otherlibs');
Add('packages/httpd20/examples');
Add('packages/httpd22/examples');
Add('packages/cocoaint/utils/cocoa-skel/src');
Add('packages/fcl-base/src/wince'); // for unit src/fileinfo and src/wince/fileinfo

Score:=10;
Targets:='wince';
Add('rtl/wince');
Add('packages/winceunits');
Add('packages/fcl-base/src/wince');

Score:=-10;
Targets:='win32,win64';
Add('packages/winceunits');

Score:=-10;
Targets:='win';
Add('packages/winunits-base');

Score:=105;
Targets:='unix';
Add('packages/libc');
Score:=10;
Targets:='netware';
Add('rtl/netwlibc');

// jvm uses the java system.pp on both android and java
Score:=10;
Targets:='jvm';
Add('rtl/java');

// end.

