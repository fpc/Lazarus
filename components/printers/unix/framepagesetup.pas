unit framePageSetup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, FileUtil, LResources, Forms, ExtCtrls, StdCtrls,
  Printers, OsPrinters, LCLIntf, LCLProc, Controls, CupsLCL;

type
  TPageSetupMode = (psmFull, psmPapers, psmMargins);
  TPageSetupOption = (
    psoMargins,         // margins and preview are visible
    psoPapers,          // papers group visible
    psoOrientation      // orientation group visible
  );
  TPageSetupOptions = set of TPageSetupOption;

  { TframePageSetup }

  TframePageSetup = class(TFrame)
    cbPaper: TComboBox;
    cbSource: TComboBox;
    panMargins: TPanel;
    txtLeft: TEdit;
    txtRight: TEdit;
    txtTop: TEdit;
    txtBottom: TEdit;
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
      Shift: TShiftState; X, Y: Integer);
    procedure pbPreviewMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure pbPreviewMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure pbPreviewPaint(Sender: TObject);
    procedure radPortraitClick(Sender: TObject);
  private
    { private declarations }
    FHeightTallest: Integer;
    FHardMargins: TRect;
    FKw,FKh,FZoom: Double;
    FOptions: TPageSetupOptions;
  public
    { public declarations }
    procedure Initialize(AMode: TPageSetupMode);
    procedure UpdatePageSize;
  end;

implementation

{ TframePageSetup }

procedure TframePageSetup.pbPreviewPaint(Sender: TObject);
var
  R: TRect;
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

  if Sender=nil then ;

  if not (psoMargins in FOptions) then
    exit;

  with pbPreview do
  begin

    // page frame
    R := Rect(0,0,Width,Height);
    Canvas.Pen.Color := clBlack;
    Canvas.Brush.Color:=clWhite;
    Canvas.Rectangle(R);

    // hard margins
    Canvas.Pen.Color := RGBToColor(255,204,204);
    DrawMargin(0, FHardMargins.Left  );
    DrawMargin(1, FHardMargins.Top   );
    DrawMargin(2, FHardMargins.Right );
    DrawMargin(3, FHardMargins.Bottom);
  end;
end;

procedure TframePageSetup.radPortraitClick(Sender: TObject);
begin
  if sender=nil then ;

  if radPortrait.Checked then
    Printer.Orientation := poPortrait
  else
    Printer.Orientation := poLandsCape;
  UpdatePageSize;
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
  if not (psoMargins in FOptions) then
    exit;

  TallH := Round(FheightTallest * FKh);

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
  if not (psoMargins in FOptions) then
    exit;

  with Printer.PaperSize.PaperRect.PhysicalRect do
  begin
    PbPreview.Width := Round(Fkw * (Right - Left) * FZoom) + 2;
    PbPreview.Height := Round(FKh * (Bottom - Top) * FZoom) + 2;
  end;

  with Printer.PaperSize.PaperRect do
  begin
    FHardMargins.Left := Round(Fkw * (WorkRect.Left-PhysicalRect.Left) * FZoom);
    FHardMargins.Right := Round(Fkw * (Physicalrect.Right-WorkRect.Right) * FZoom);
    FHardMargins.Top := Round(FkH * (WorkRect.Top-PhysicalRect.Top) * FZoom);
    FHardMargins.Bottom := Round(FkH * (PhysicalRect.Bottom-WorkRect.Bottom) * FZoom);
  end;

  {$IFDEF DebugCUPS}
  with FHardMargins do
  begin
    DebugLn(' Kh=%.2f Kw=%.2f',[FKh,FKw]);
    DebugLn(' BoxLimits L=0 T=0 R=%d B=%d',[PbPreview.Width-1,PbPreview.Height-1]);
    DebugLn('OrgMargins L=%d T=%d R=%d B=%d',[Left,Top,Right,Bottom]);
  end;
  {$ENDIF}
end;

procedure TframePageSetup.Initialize(AMode: TPageSetupMode);
var
  i,j:Integer;
  R: TPaperRect;
begin
  case AMode of
    psmMargins:
      FOptions := [psoMargins];
    psmPapers:
      FOptions := [psoPapers,psoOrientation];
    else
      FOptions := [psoMargins,psoPapers,psoOrientation];
  end;

  if [psoMargins,psoPapers]*FOptions<>[] then
  begin
    SetupCupsCombo(cbPaper, nil, 'PageSize');
    if (cbPaper.Items.Count=0) then
    begin
      // no cups printer papers, use default ones
      cbPaper.Items := Printer.PaperSize.SupportedPapers;
      cbPaper.ItemIndex:= cbPaper.Items.IndexOf(Printer.PaperSize.PaperName);
      cbPaper.Enabled:=true;
    end;
  end;

  if psoPapers in FOptions then
    SetupCupsCOmbo(cbSource, nil, 'InputSlot')
  else
    gpPaper.Visible := false;

  //TODO: support reverse variants too?
  gpOrientation.Visible := (psoOrientation in FOptions);
  case Printer.Orientation of
    poPortrait,poReversePortrait:
      radPortrait.Checked := true;
    poLandscape,poReverseLandscape:
      radLandscape.Checked := true;
  end;

  if psoMargins in FOptions then
  begin
    // assume 100 pix = 8.5 inch (IOW, letter size width = 100 pixels)
    with ScreenInfo do
    begin
      FKw := (100/8.5)/Printer.XDPI;
      FKh := (100/8.5)*(PixelsPerInchY/PixelsPerInchX)/Printer.YDPI;
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

  end else
  begin
    panPreview.Visible:=false;
    gpMargins.Visible:=false;
  end;

  if AMode=psmPapers then
  begin
    gpOrientation.Anchors:=[akTop,akRight,akBottom];
    gpOrientation.Align:=alRight;
    gpPaper.Anchors:=[akTop,akLeft];
    gpPaper.Align:=alClient;
    PanSetup.Align:=alClient;
  end else
  if AMode=psmMargins then
    PanSetup.Height:=gpMargins.Height+C_BOTHSPACES;

end;

initialization
  {$I framepagesetup.lrs}

end.

