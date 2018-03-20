{
 *****************************************************************************
  This file is part of the Printer4Lazarus package

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit framePageSetup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  LCLIntf, LCLProc, LResources, Controls, Graphics, Forms, ExtCtrls, StdCtrls,
  Spin, Printers,
  // Printers
  OsPrinters, CupsLCL;

type
  { TframePageSetup }

  TframePageSetup = class(TFrame)
    cbPaper: TComboBox;
    cbSource: TComboBox;
    panMargins: TPanel;
    boxShadow: TShape;
    txtLeft: TFloatSpinEdit;
    txtTop: TFloatSpinEdit;
    txtRight: TFloatSpinEdit;
    txtBottom: TFloatSpinEdit;
    gpPaper: TGroupBox;
    gpOrientation: TGroupBox;
    gpMargins: TGroupBox;
    lblSource: TLabel;
    lblPaper: TLabel;
    lblLeft: TLabel;
    lblRight: TLabel;
    lblTop: TLabel;
    lblBottom: TLabel;
    pbPreview: TPaintBox;
    panSetup: TPanel;
    panPreview: TPanel;
    radLandscape: TRadioButton;
    radPortrait: TRadioButton;
    procedure cbPaperChange(Sender: TObject);
    procedure panPreviewResize(Sender: TObject);
    procedure pbPreviewMouseDown(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure pbPreviewMouseWheelDown(Sender: TObject; {%H-}Shift: TShiftState;
      {%H-}MousePos: TPoint; var Handled: Boolean);
    procedure pbPreviewMouseWheelUp(Sender: TObject; {%H-}Shift: TShiftState;
      {%H-}MousePos: TPoint; var Handled: Boolean);
    procedure pbPreviewPaint(Sender: TObject);
    procedure radPortraitClick(Sender: TObject);
    procedure txtLeftChange(Sender: TObject);
  private
    FHeightTallest: Integer;
    FHardMargins: TRect;
    FFactorX, FFactorY, FZoom: Double;
    EnablePreview: boolean;
    EnableMargins: boolean;
    EnablePapers: boolean;
    EnableOrientation: boolean;
    function NToInches: double;
    procedure RotateMargins(AOrder: boolean);
    procedure UpdateMaxValues;
  public
    UnitInches: boolean;
    CurPageWidth: double;
    CurPageHeight: double;
    procedure Initialize(AEnablePreview, AEnableMargins, AEnablePapers,
      AEnableOrientation: boolean);
    procedure UpdatePageSize;
  end;

implementation

{$R framepagesetup.lfm}

{ TframePageSetup }

function TframePageSetup.NToInches: double;
begin
  if UnitInches then
    Result:= 1
  else
    Result:= 1/25.4;
end;

procedure TframePageSetup.RotateMargins(AOrder: boolean);
var
  m_l, m_t, m_r, m_b: double;
begin
  m_l:= txtLeft.Value;
  m_t:= txtTop.Value;
  m_r:= txtRight.Value;
  m_b:= txtBottom.Value;

  if AOrder then
  begin
    txtLeft.Value:= m_b;
    txtTop.Value:= m_l;
    txtRight.Value:= m_t;
    txtBottom.Value:= m_r;
  end
  else
  begin
    txtLeft.Value:= m_t;
    txtTop.Value:= m_r;
    txtRight.Value:= m_b;
    txtBottom.Value:= m_l;
  end;
end;

procedure TframePageSetup.pbPreviewPaint(Sender: TObject);
  procedure DrawMargin(AIndex: Integer; ASize: Integer);
  begin
    with pbPreview do
    case AIndex of
      0: // Left
        begin
          Canvas.MoveTo(ASize, 1);
          Canvas.LineTo(ASize, Height-1);
        end;
      1: //Top
        begin
          Canvas.MoveTo(1,ASize);
          Canvas.LineTo(Width-1, ASize);
        end;
      2: // Right
        begin
          Canvas.MoveTo(Width-1-ASize, 1);
          Canvas.LineTo(Width-1-ASize,Height-1);
        end;
      3: // Bottom
        begin
          Canvas.MoveTo(1,Height-1-Asize);
          Canvas.LineTo(Width-1, Height-1-ASize);
        end;
    end;
  end;
begin
  if not EnablePreview then
    exit;

  with pbPreview do
  begin
    Canvas.Pen.Color := clBlack;
    Canvas.Brush.Color := clWhite;
    Canvas.Rectangle(0, 0, Width, Height);

    if EnableMargins then
    begin
      Canvas.Pen.Color := clHighlight;
      DrawMargin(0, Round(txtLeft.Value * NToInches * Printer.XDPI * FFactorX * FZoom) ); //FHardMargins.Left
      DrawMargin(1, Round(txtTop.Value * NToInches * Printer.YDPI * FFactorY * FZoom) );
      DrawMargin(2, Round(txtRight.Value * NToInches * Printer.XDPI * FFactorX * FZoom) );
      DrawMargin(3, Round(txtBottom.Value * NToInches * Printer.YDPI * FFactorY * FZoom) );
    end;
  end;
end;

procedure TframePageSetup.radPortraitClick(Sender: TObject);
begin
  RotateMargins(radPortrait.Checked);
  if radPortrait.Checked then
    Printer.Orientation := poPortrait
  else
    Printer.Orientation := poLandsCape;
  UpdatePageSize;
end;

procedure TframePageSetup.txtLeftChange(Sender: TObject);
begin
  pbPreview.Update;
end;

procedure TframePageSetup.cbPaperChange(Sender: TObject);
begin
  if Printer.PaperSize.DefaultPapers then
  begin
    if cbPaper.ItemIndex>=0 then
      Printer.PaperSize.PaperName := cbPaper.Items[cbPaper.ItemIndex];
  end else
    Printer.PaperSize.PaperName := GetCupsComboKeyValue(cbPaper);
  UpdatePageSize;
end;

procedure TframePageSetup.panPreviewResize(Sender: TObject);
var
  TallH: Integer;
begin
  if not EnablePreview then
    exit;

  TallH := Round(FheightTallest * FFactorY);

  with PanPreview do
  if (Height<>C_BOTHSPACES) and (TallH>(Height-C_BOTHSPACES)) then
    FZoom := (Height-C_BOTHSPACES)/TallH
  else
    FZoom := 1.0;
end;

procedure TframePageSetup.pbPreviewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button=mbMiddle then
  begin
    FZoom := 1;
    UpdatePageSize;
  end;
end;

procedure TframePageSetup.pbPreviewMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  FZoom := FZoom - 0.2;
  if FZoom<0.5 then
    FZoom := 0.5;
  UpdatePageSize;
  Handled := true;
end;

procedure TframePageSetup.pbPreviewMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  FZoom := FZoom + 0.2;
  UpdatePageSize;
  Handled := true;
end;

procedure TframePageSetup.UpdatePageSize;
begin
  if not EnablePreview then
    exit;

  with Printer.PaperSize.PaperRect.PhysicalRect do
  begin
    PbPreview.Width := Round(FFactorX * (Right - Left) * FZoom) + 2;
    PbPreview.Height := Round(FFactorY * (Bottom - Top) * FZoom) + 2;

    boxShadow.Width := pbPreview.Width;
    boxShadow.Height := pbPreview.Height;
  end;

  with Printer.PaperSize.PaperRect do
  begin
    FHardMargins.Left := Round(FFactorX * (WorkRect.Left-PhysicalRect.Left) * FZoom);
    FHardMargins.Right := Round(FFactorX * (Physicalrect.Right-WorkRect.Right) * FZoom);
    FHardMargins.Top := Round(FFactorY * (WorkRect.Top-PhysicalRect.Top) * FZoom);
    FHardMargins.Bottom := Round(FFactorY * (PhysicalRect.Bottom-WorkRect.Bottom) * FZoom);

    CurPageWidth := (PhysicalRect.Right-PhysicalRect.Left)/Printer.XDPI/NToInches;
    CurPageHeight := (PhysicalRect.Bottom-PhysicalRect.Top)/Printer.YDPI/NToInches;
  end;

  {$IFDEF DebugCUPS}
  with FHardMargins do
  begin
    DebugLn(' Kh=%.2f Kw=%.2f',[FKh,FKw]);
    DebugLn(' BoxLimits L=0 T=0 R=%d B=%d',[PbPreview.Width-1,PbPreview.Height-1]);
    DebugLn('OrgMargins L=%d T=%d R=%d B=%d',[Left,Top,Right,Bottom]);
  end;
  {$ENDIF}

  UpdateMaxValues;
end;

procedure TframePageSetup.UpdateMaxValues;
begin
  txtLeft.MaxValue := CurPageWidth/2;
  txtRight.MaxValue := CurPageWidth/2;
  txtTop.MaxValue := CurPageHeight/2;
  txtBottom.MaxValue := CurPageHeight/2;
end;

procedure TframePageSetup.Initialize(AEnablePreview, AEnableMargins, AEnablePapers,
  AEnableOrientation: boolean);
var
  i,j:Integer;
  R: TPaperRect;
begin
  EnablePreview:= AEnablePreview;
  EnableMargins:= AEnableMargins;
  EnablePapers:= AEnablePapers;
  EnableOrientation:= AEnableOrientation;

  gpPaper.Enabled := EnablePapers;
  if EnablePapers then
  begin
    SetupCupsCombo(cbSource, nil, 'InputSlot');
    SetupCupsCombo(cbPaper, nil, 'PageSize');
    if (cbPaper.Items.Count=0) then
    begin
      // no cups printer papers, use default ones
      cbPaper.Items := Printer.PaperSize.SupportedPapers;
      cbPaper.ItemIndex:= cbPaper.Items.IndexOf(Printer.PaperSize.PaperName);
      cbPaper.Enabled:=true;
    end;
  end;

  //TODO: support reverse variants too?
  gpOrientation.Enabled := EnableOrientation;
  case Printer.Orientation of
    poPortrait,poReversePortrait:
      radPortrait.Checked := true;
    poLandscape,poReverseLandscape:
      radLandscape.Checked := true;
  end;

  gpMargins.Enabled := EnableMargins;
  panPreview.Visible:= EnablePreview;

  if EnablePreview then
  begin
    // assume 100 pix = 8.5 inch (IOW, letter size width = 100 pixels)
    with ScreenInfo do
    begin
      FFactorX := (100/8.5)/Printer.XDPI;
      FFactorY := (100/8.5)*(PixelsPerInchY/PixelsPerInchX)/Printer.YDPI;
    end;

    // find the tallest paper
    FHeightTallest := 0;
    j := -1;
    if cbPaper.Enabled then
    for i:=0 to cbPaper.Items.Count-1 do
    begin
      if Printer.PaperSize.DefaultPapers then
        R := Printer.PaperSize.PaperRectOf[cbPaper.Items[i]]
      else
        R := Printer.PaperSize.PaperRectOf[GetCupsComboKeyValue(cbPaper, i)];
      with R.PhysicalRect do
      if FHeightTallest<(Bottom-Top) then
      begin
        FHeightTallest := (Bottom-Top);
        j := i;
      end;
    end;

    if j>=0 then
    begin
      {$IFDEF DebugCUPS}
      DebugLn(' Tallest Paper is: %s Height=%d %.2f Inch',
       [cbPaper.Items[j],FHeightTallest,FHeightTallest/Printer.YDPI]);
      {$ENDIF}
    end;

    // zoom factor
    FZoom := 1.0;
    UpdatePageSize;
  end;
end;

end.

