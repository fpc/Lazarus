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
    FFactorX, FFactorY, FZoom: Double;
    function NToInches: double;
    procedure RotateMargins(AOrder: boolean);
    procedure UpdateMaxValues;
  public
    UnitInches: boolean;
    EnablePreview: boolean;
    EnableMargins: boolean;
    EnablePapers: boolean;
    EnableOrientation: boolean;
    CurPageWidth: double;
    CurPageHeight: double;
    procedure Initialize(AEnablePreview, AEnableMargins, AEnablePapers,
      AEnableOrientation: boolean);
    procedure UpdatePageSize;
    procedure SetDefaultMinMargins;
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

  // same way must change MinValues
  m_l:= txtLeft.MinValue;
  m_t:= txtTop.MinValue;
  m_r:= txtRight.MinValue;
  m_b:= txtBottom.MinValue;

  if AOrder then
  begin
    txtLeft.MinValue:= m_b;
    txtTop.MinValue:= m_l;
    txtRight.MinValue:= m_t;
    txtBottom.MinValue:= m_r;
  end
  else
  begin
    txtLeft.MinValue:= m_t;
    txtTop.MinValue:= m_r;
    txtRight.MinValue:= m_b;
    txtBottom.MinValue:= m_l;
  end;
end;

procedure TframePageSetup.pbPreviewPaint(Sender: TObject);
var
  R: TRect;
  NLeft, NTop, NRight, NBottom: integer;
begin
  if not EnablePreview then
    exit;

  with pbPreview do
  begin
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Color := clBlack;
    Canvas.Brush.Color := clWhite;
    Canvas.Rectangle(0, 0, Width, Height);

    //if EnableMargins then // EnableMargins is for SpinEdits only
    begin
      NLeft := Round(txtLeft.Value * NToInches * Printer.XDPI * FFactorX * FZoom);
      NTop := Round(txtTop.Value * NToInches * Printer.YDPI * FFactorY * FZoom);
      NRight := Round(txtRight.Value * NToInches * Printer.XDPI * FFactorX * FZoom);
      NBottom := Round(txtBottom.Value * NToInches * Printer.YDPI * FFactorY * FZoom);

      R.Left := NLeft;
      R.Top := NTop;
      R.Right := Width-1-NRight;
      R.Bottom := Height-1-NBottom;

      Canvas.Pen.Color := clMedGray;
      //Canvas.Pen.Style := psDash; // AT: setting line style don't work, line is solid
      Canvas.Rectangle(R);
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
    CurPageWidth := (PhysicalRect.Right-PhysicalRect.Left)/Printer.XDPI/NToInches;
    CurPageHeight := (PhysicalRect.Bottom-PhysicalRect.Top)/Printer.YDPI/NToInches;
  end;

  UpdateMaxValues;
end;

procedure TframePageSetup.UpdateMaxValues;
const
  cMul = 0.45; // max margin is almost 1/2 of page size
begin
  txtLeft.MaxValue := CurPageWidth * cMul;
  txtRight.MaxValue := CurPageWidth * cMul;
  txtTop.MaxValue := CurPageHeight * cMul;
  txtBottom.MaxValue := CurPageHeight * cMul;
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

  cbPaper.Items.Clear;
  cbSource.Items.Clear;
  cbPaper.ItemIndex := -1;
  cbSource.ItemIndex := -1;

  if EnablePapers then
  begin
    SetupCupsCombo(cbSource, nil, 'InputSlot');
    SetupCupsCombo(cbPaper, nil, 'PageSize');
    if (cbPaper.Items.Count=0) then
    begin
      // no cups printer papers, use default ones
      cbPaper.Items := Printer.PaperSize.SupportedPapers;
      cbPaper.ItemIndex:= cbPaper.Items.IndexOf(Printer.PaperSize.PaperName);
    end;
  end;

  cbPaper.Enabled := cbPaper.Items.Count>0;
  cbSource.Enabled := cbSource.Items.Count>0;

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

procedure TframePageSetup.SetDefaultMinMargins;
begin
  with Printer.PaperSize.PaperRect do
  begin
    txtLeft.MinValue := (WorkRect.Left-PhysicalRect.Left)/Printer.XDPI/NToInches;
    txtTop.MinValue := (WorkRect.Top-PhysicalRect.Top)/Printer.YDPI/NToInches;
    txtRight.MinValue := (PhysicalRect.Right-WorkRect.Right)/Printer.XDPI/NToInches;
    txtBottom.MinValue := (PhysicalRect.Bottom-WorkRect.Bottom)/Printer.YDPI/NToInches;
  end;
end;

end.

