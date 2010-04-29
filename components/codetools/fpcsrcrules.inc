{%MainUnit definetemplates.pas}
Score:=100;
Targets:='*';
Add('rtl');
Add('packages/fcl-');

Score:=1;
Add('packages');

Score:=-10;
Add('packages/libgbafpc/examples');
Add('packages/ptc/examples');
Add('packages/ncurses/examples');
Add('packages/libndsfpc/examples');
Add('packages/regexpr/src/old');
Add('packages/amunits/src/otherlibs');

Score:=10;
Targets:='wince';
Add('packages/winceunits');
Targets:='win';
Add('packages/winunits-base');

// end.

