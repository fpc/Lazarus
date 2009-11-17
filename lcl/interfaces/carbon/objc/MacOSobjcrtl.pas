unit MacOSobjcrtl;

{$mode objfpc}{$H+}

interface

uses
  MacOSAll, objcrtl, objcrtl10, objcrtl20;

implementation

procedure InitObjCRunTime;
var
  MacVersion : SInt32;
begin
  if (Gestalt(gestaltSystemVersionMinor, MacVersion) = noErr) then begin
    if MacVersion >= 5
      then InitializeObjcRtl20(DefaultObjCLibName)
      else InitializeObjcRtl10(DefaultObjCLibName);
  end else
    InitializeObjcRtl20(DefaultObjCLibName);
end;


initialization
  InitObjCRuntime;

end.

