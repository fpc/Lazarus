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
    class function GetEditorBoundsFromCellRect(ACanvas: TCanvas;
      const ACellRect: TRect; const AColumnLayout: TTextLayout): TRect;
      override;
    class function GetPickListEditorBoundsFromCellRect(ACanvas: TCanvas;
      var ACellRect: TRect; const AColumnLayout: TTextLayout): Boolean;
      override;
  end;

implementation

{ TCocoaWSCustomGrid }

class function TCocoaWSCustomGrid.GetEditorBoundsFromCellRect(ACanvas: TCanvas;
  const ACellRect: TRect; const AColumnLayout: TTextLayout): TRect;
begin
  Result:= ACellRect;
  Dec( Result.Left );
  case AColumnLayout of
    tlCenter: Dec( Result.Top );
    tlBottom: Dec( Result.Bottom );
  end;
end;

class function TCocoaWSCustomGrid.GetPickListEditorBoundsFromCellRect(
  ACanvas: TCanvas; var ACellRect: TRect; const AColumnLayout: TTextLayout
  ): Boolean;
const
  EDITOR_HEIGHT = 20;
var
  offset: Integer;
begin
  Dec( ACellRect.Left );
  Dec( ACellRect.Right );
  case AColumnLayout of
    tlTop: Inc( ACellRect.Top );
    tlCenter: Inc( ACellRect.Top, (ACellRect.Height-EDITOR_HEIGHT) div 2 );
    tlBottom: ACellRect.Top:= ACellRect.Bottom - EDITOR_HEIGHT - 1;
  end;
  ACellRect.Bottom:= ACellRect.Top + EDITOR_HEIGHT;
  Result:= True;
end;

end.

