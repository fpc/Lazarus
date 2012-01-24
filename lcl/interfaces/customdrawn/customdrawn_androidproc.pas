unit customdrawn_androidproc;

{$mode objfpc}{$H+}

interface

uses
  // rtl+ftl
  Types, Classes, SysUtils,
  fpimage, fpcanvas, ctypes,
  // Android headers
  jni,
  // Custom Drawn Canvas
  IntfGraphics, lazcanvas,
  //
  GraphType, Controls, Graphics, LCLMessageGlue, WSControls, LCLType, LCLProc,
  customdrawnproc;

function FPColorToAndroidColor(AValue: TFPColor): jint;

implementation

// Android color is in the format: Alpha-Red-Green-Blue
function FPColorToAndroidColor(AValue: TFPColor): jint;
begin
  Result:= $FF000000 or ((AValue.Blue shr 8) and $ff)
       or (AValue.Green and $ff00)
       or ((AValue.Red shl 8) and $ff0000);
end;

end.

