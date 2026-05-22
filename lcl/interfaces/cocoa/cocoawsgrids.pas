unit CocoaWSGrids;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Graphics,
  WSGrids;

type

  { TCocoaWSCustomGrid }

  TCocoaWSCustomGrid = class( TWSCustomGrid )
    class function GetPickListEditorBoundsFromCellRect(ACanvas: TCanvas;
      var ACellRect: TRect; const AColumnLayout: TTextLayout): Boolean;
      override;
  end;

implementation

{ TCocoaWSCustomGrid }

class function TCocoaWSCustomGrid.GetPickListEditorBoundsFromCellRect(
  ACanvas: TCanvas; var ACellRect: TRect; const AColumnLayout: TTextLayout
  ): Boolean;
var
  offset: Integer;
begin
  offset:= (ACellRect.Height - 20) div 2;
  Inc( ACellRect.Top, offset );
  Dec( ACellRect.Right );
  Result:= True;
end;

end.

