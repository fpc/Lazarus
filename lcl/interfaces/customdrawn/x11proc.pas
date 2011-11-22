unit x11proc;

{$mode objfpc}{$H+}

interface

uses
  // rtl+ftl
  Types, Classes, SysUtils,
  fpimage, fpcanvas,
  // Custom Drawn Canvas
  IntfGraphics, lazcanvas,
  //
  GraphType, Controls, LCLMessageGlue, WSControls, LCLType, LCLProc;

type
  TX11WindowInfo = class
  public
    Image: TLazIntfImage;
    Canvas: TLazCanvas;
  end;

implementation

end.

