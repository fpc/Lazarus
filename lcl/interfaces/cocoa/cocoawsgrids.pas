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
const
  MIN_HEIGHT = 18;
begin
  Result:= ACellRect;
  Dec( Result.Left );
  case AColumnLayout of
    tlTop: begin
      if Result.Height < MIN_HEIGHT then
        Result.Height:= MIN_HEIGHT;
    end;
    tlCenter: begin
      if Result.Height < MIN_HEIGHT then begin
        Dec( Result.Top, (MIN_HEIGHT-Result.Height) div 2 );
        Result.Height:= MIN_HEIGHT;
      end;
    end;
    tlBottom: begin
      if Result.Height < MIN_HEIGHT then
        Result.Top:= Result.Bottom - MIN_HEIGHT;
    end;
  end;
end;

class function TCocoaWSCustomGrid.GetPickListEditorBoundsFromCellRect(
  ACanvas: TCanvas; var ACellRect: TRect; const AColumnLayout: TTextLayout
  ): Boolean;
const
  EDITOR_HEIGHT = 20;
begin
  Dec( ACellRect.Left );
  Dec( ACellRect.Right );
  case AColumnLayout of
    tlTop: Inc( ACellRect.Top );
    tlCenter: Inc( ACellRect.Top, (ACellRect.Height-EDITOR_HEIGHT) div 2 );
    tlBottom: ACellRect.Top:= ACellRect.Bottom - EDITOR_HEIGHT - 1;
  end;
  ACellRect.Height:= EDITOR_HEIGHT;
  Result:= True;
end;

end.

