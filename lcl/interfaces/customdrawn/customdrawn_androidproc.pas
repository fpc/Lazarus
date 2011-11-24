unit customdrawn_androidproc;

{$mode objfpc}{$H+}

interface

uses
  // rtl+ftl
  Types, Classes, SysUtils,
  fpimage, fpcanvas, ctypes,
  // Custom Drawn Canvas
  IntfGraphics, lazcanvas,
  //
  GraphType, Controls, LCLMessageGlue, WSControls, LCLType, LCLProc,
  customdrawnproc;

type
  TAndroidWindowInfo = class
  public
//    Window: X.TWindow;
    LCLControl: TWinControl;
    Children: TFPList; // of TCDWinControl;
    // painting objects
    Image: TLazIntfImage;
    Canvas: TLazCanvas;
  end;

implementation

end.

