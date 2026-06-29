unit umain;

{$mode objfpc}{$H+}

//can be compiled only with lazarus trunk 4.99 (29.06.2026) and Lazarus 5.0.

interface

uses
  Classes, SysUtils, Types, Forms, Controls, Graphics, ComCtrls, StdCtrls,
  ExtCtrls, Grids, InterfaceBase, LCLType, LCLPlatformDef;

type

  { TMainForm }

  TMainForm = class(TForm)
    lblRunning: TLabel;
    lblNote: TLabel;
    lblValue: TLabel;
    pbCanvas: TPaintBox;
    pnlTop: TPanel;
    sgStatus: TStringGrid;
    tbOpacity: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pbCanvasPaint(Sender: TObject);
    procedure tbOpacityChange(Sender: TObject);
  private
    FBmp: TBitmap;
    procedure BuildFallbackImage;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  APngPath: string;
  APic: TPicture;
begin
  sgStatus.ColCount := 2;
  sgStatus.RowCount := 8;
  sgStatus.FixedRows := 1;
  sgStatus.FixedCols := 0;
  sgStatus.ColWidths[0] := 110;
  sgStatus.ColWidths[1] := 250;
  sgStatus.Options := sgStatus.Options - [goEditing, goRangeSelect];
  sgStatus.Cells[0, 0] := 'Widget set';
  sgStatus.Cells[1, 0] := 'DC opacity';

  sgStatus.Cells[0, 1] := 'gtk2';
  sgStatus.Cells[1, 1] := 'No (needs heavy rework)';

  sgStatus.Cells[0, 2] := 'gtk3';
  sgStatus.Cells[1, 2] := 'Yes';

  sgStatus.Cells[0, 3] := 'qt';
  sgStatus.Cells[1, 3] := 'Yes';

  sgStatus.Cells[0, 4] := 'qt5';
  sgStatus.Cells[1, 4] := 'Yes';

  sgStatus.Cells[0, 5] := 'qt6';
  sgStatus.Cells[1, 5] := 'Yes';

  sgStatus.Cells[0, 6] := 'win32';
  sgStatus.Cells[1, 6] := 'No (planned)';

  sgStatus.Cells[0, 7] := 'cocoa';
  sgStatus.Cells[1, 7] := 'Yes';

  lblRunning.Caption := 'Running widget set: ' + LCLPlatformDisplayNames[WidgetSet.LCLPlatform];
  lblValue.Caption := 'Opacity: ' + IntToStr(tbOpacity.Position);

  FBmp := TBitmap.Create;
  APngPath := ExtractFilePath(ParamStr(0)) + 'opacity_test.png';
  if FileExists(APngPath) then
  begin
    APic := TPicture.Create;
    try
      APic.LoadFromFile(APngPath);
      FBmp.Assign(APic.Graphic);
    finally
      APic.Free;
    end;
  end else
    BuildFallbackImage;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FBmp.Free;
end;

procedure TMainForm.BuildFallbackImage;
var
  x, y: Integer;
begin
  FBmp.SetSize(96, 96);
  FBmp.Canvas.Brush.Style := bsSolid;
  for y := 0 to 7 do
    for x := 0 to 7 do
    begin
      if ((x + y) and 1) = 0 then
        FBmp.Canvas.Brush.Color := RGBToColor(40 + x * 24, 70, 210 - y * 22)
      else
        FBmp.Canvas.Brush.Color := RGBToColor(220 - x * 22, 130 + y * 12, 50);
      FBmp.Canvas.FillRect(x * 12, y * 12, x * 12 + 12, y * 12 + 12);
    end;
  FBmp.Canvas.Brush.Style := bsClear;
  FBmp.Canvas.Pen.Color := clBlack;
  FBmp.Canvas.Rectangle(0, 0, 96, 96);
end;

procedure TMainForm.tbOpacityChange(Sender: TObject);
begin
  lblValue.Caption := 'Opacity: ' + IntToStr(tbOpacity.Position);
  pbCanvas.Invalidate;
end;

procedure TMainForm.pbCanvasPaint(Sender: TObject);
var
  SavedOpacity: Byte;
  i: Integer;
  Bars: array[0..4] of TColor;
  C: TCanvas;
begin
  C := pbCanvas.Canvas;
  C.Brush.Color := clWhite;
  C.FillRect(pbCanvas.ClientRect);

  Bars[0] := clRed;
  Bars[1] := clLime;
  Bars[2] := clBlue;
  Bars[3] := clYellow;
  Bars[4] := clAqua;
  for i := 0 to High(Bars) do
  begin
    C.Brush.Color := Bars[i];
    C.FillRect(10 + i * 35, 10, 10 + i * 35 + 31, 180);
  end;

  SavedOpacity := WidgetSet.GetDCOpacity(C.Handle);
  WidgetSet.SetDCOpacity(C.Handle, tbOpacity.Position);
  C.Brush.Style := bsClear;
  C.Font.Color := clBlack;
  C.Font.Height := 24;
  C.Font.Style := [fsBold];
  C.TextOut(14, 75, 'WATERMARK');
  C.Brush.Style := bsSolid;
  WidgetSet.SetDCOpacity(C.Handle, SavedOpacity);

  WidgetSet.SetDCOpacity(C.Handle, tbOpacity.Position);
  C.Pen.Style := psClear;
  C.Brush.Color := clRed;
  C.Ellipse(220, 15, 320, 115);
  C.Brush.Color := clLime;
  C.Ellipse(260, 15, 360, 115);
  C.Brush.Color := clBlue;
  C.Ellipse(240, 55, 340, 155);
  C.Brush.Color := clYellow;
  C.Ellipse(270, 65, 350, 145);
  C.Pen.Style := psSolid;
  WidgetSet.SetDCOpacity(C.Handle, SavedOpacity);

  C.Font.Style := [];
  C.Font.Height := 16;
  C.Font.Color := clBlack;
  C.Brush.Style := bsClear;
  C.TextOut(372, 8, 'image normal');
  C.TextOut(496, 8, 'image opacity');
  C.Brush.Style := bsSolid;

  C.StretchDraw(Rect(372, 30, 472, 130), FBmp);

  WidgetSet.SetDCOpacity(C.Handle, tbOpacity.Position);
  C.StretchDraw(Rect(496, 30, 596, 130), FBmp);
  WidgetSet.SetDCOpacity(C.Handle, SavedOpacity);
end;

end.
