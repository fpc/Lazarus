unit svMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, Spin, ShellCtrls, Buttons, fpVectorial, SynEdit,
  SynHighlighterXML, types;

type

  { TMainForm }

  TMainForm = class(TForm)
    BtnExternalViewer: TButton;
    CbAdjustPenColorToBackground: TCheckBox;
    CbHistory: TComboBox;
    EdMargin: TSpinEdit;
    Image: TImage;
    ImageList1: TImageList;
    Label3: TLabel;
    LeftPanel: TPanel;
    PageControl: TPageControl;
    ImagePanel: TPanel;
    MouseWheelTimer: TTimer;
    ControlPanel: TPanel;
    Panel2: TPanel;
    ShellListView: TShellListView;
    ShellTreeView: TShellTreeView;
    ShellSplitter: TSplitter;
    MainSplitter: TSplitter;
    StatusBar: TStatusBar;
    SynEdit: TSynEdit;
    SynXMLSyn1: TSynXMLSyn;
    PgImage: TTabSheet;
    PgSource: TTabSheet;
    PgDebugTree: TTabSheet;
    TokensTreeView: TTreeView;
    procedure BtnExternalViewerClick(Sender: TObject);
    procedure CbAdjustPenColorToBackgroundChange(Sender: TObject);
    procedure CbHistoryCloseUp(Sender: TObject);
    procedure CbHistoryDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure CbHistoryEditingDone(Sender: TObject);
    procedure EdOffsetXChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ImageMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure ImagePanelMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure MouseWheelTimerTimer(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure ShellListViewCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure ShellListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ShellTreeViewGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure ShellTreeViewGetSelectedIndex(Sender: TObject; Node: TTreeNode);
    function ShellTreeViewSortCompare(Item1, Item2: TFileItem): integer;
  private
    { private declarations }
    FVec: TvVectorialDocument;
    FScale: Double;
    FScaleEff: Double;
    FImgSize: TSize;
    FZoomPos: TPoint;
    FMousePos: TPoint;
    FImgPos: TPoint;
    FFormActivated: boolean;
    FFileName: string;
    function  AddToDebugTreeProc(AStr: string; AParent: Pointer): Pointer;
    procedure DrawSVG(ACanvas: TCanvas; ACanvasSize: TSize);
    procedure LoadSVG(AFilename: String);
    procedure PrepareCanvas(ACanvas: TCanvas);
    procedure RedrawSVG;
    procedure UpdateHistory(AFileName: String);
    procedure UpdateStatusInfo;

    procedure ReadIni;
    procedure WriteIni;
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  LCLIntf, LCLType, LCLProc, IniFiles, LazFileUtils, FileCtrl,
  svgVectorialReader;

const
  PROGRAM_NAME = 'svgViewer';
  ZOOM_FACTOR = 1.05;
  DO_NOT_DRAW = false;
  INCH = 25.4;

function mm2px(mm: Double): Integer;
begin
  Result := round(mm/INCH * Screen.PixelsPerInch);
end;

{ TMainForm }

function TMainForm.AddToDebugTreeProc(AStr: string; AParent: Pointer): Pointer;
begin
  Result := TokensTreeView.Items.AddChild(TTreeNode(AParent), AStr);
end;

procedure TMainForm.CbAdjustPenColorToBackgroundChange(Sender: TObject);
begin
  RedrawSVG;
end;

procedure TMainForm.BtnExternalViewerClick(Sender: TObject);
begin
  OpenDocument(FFileName);
end;

procedure TMainForm.CbHistoryCloseUp(Sender: TObject);
var
  dir: String;
begin
  if CbHistory.ItemIndex = -1 then
    exit;
  dir := CbHistory.Items[CbHistory.ItemIndex];
  ShellTreeView.Path := dir;
end;

procedure TMainForm.CbHistoryDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  s: String;
  combobox: TComboBox;
begin
  combobox := Control as TComboBox;
  s := MinimizeName(combobox.Items[Index], combobox.Canvas, combobox.ClientWidth);
  combobox.Canvas.TextOut(ARect.Left, ARect.Top, s);
end;

procedure TMainForm.CbHistoryEditingDone(Sender: TObject);
begin
  UpdateHistory(AppendPathDelim(CbHistory.Text)+'.');
  ShellTreeView.Path := CbHistory.Text;
  ShellTreeView.MakeSelectionVisible;
end;

{ Paint the svg image into the provided canvas. It is the canvas of the
  Image's bitmap }
procedure TMainForm.DrawSVG(ACanvas: TCanvas; ACanvasSize: TSize);
var
  page: TvPage;
  dx, dy: Integer;
begin
  // Erase background of the image
  ACanvas.Brush.Color := clWhite;
  ACanvas.FillRect(0, 0, ACanvasSize.cx, ACanvasSize.cy);

  if FVec = nil then
    exit;

  // Get page of vectorial file
  page := FVec.GetPage(0);
  page.AdjustPenColorToBackground := CbAdjustPenColorToBackground.Checked;

  // Draw background as defined in the file
  page.DrawBackground(ACanvas);

  // Draw the page
  dx := round(FScale*EdMargin.Value) - page.RenderInfo.EntityCanvasMinXY.X;
  dy := round(FScale*EdMargin.Value) - page.RenderInfo.EntityCanvasMinXY.Y;
  page.Render(ACanvas, dx, dy, FScaleEff, FScaleEff);

  // Prepare debug tree
  TokensTreeView.Items.Clear;
  FVec.GenerateDebugTree(@AddToDebugTreeProc);
end;

procedure TMainForm.EdOffsetXChange(Sender: TObject);
begin
  RedrawSVG;
end;

procedure TMainForm.FormActivate(Sender: TObject);
var
  fn: string;
begin
  if FFormActivated then
    exit;
  FFormActivated := true;

  if (ParamCount > 0) then
    fn := ExpandfileName(ParamStr(1))
  else
    fn := FFileName;

  if (fn <> '') and FileExists(fn) then
  begin
    ShellTreeView.Path := ExtractFilePath(fn);
    ShellListView.Selected := ShellListView.Items.FindCaption(0, ExtractFileName(fn), false, true, false);
    LoadSVG(fn);
  end;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if CanClose then
    try
      WriteIni;
    finally
    end;
end;

{ Paint the svg file if specified as a command line parameter }
procedure TMainForm.FormCreate(Sender: TObject);
begin
  Caption := PROGRAM_NAME;
  PageControl.PageIndex := 0;
  {$IFNDEF MSWINDOWS}
  ShellTreeView.Images := ImageList1;
  ShellTreeView.OnGetImageIndex := @ShellTreeViewGetImageIndex;
  ShellTreeView.OnGetSelectedIndex := @ShellTreeViewGetSelectedIndex;
  {$ENDIF}
  ShellListview.Mask := '*.svg';
  ReadIni;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FVec);
end;

{ Remember the mouse-down position for panning }
procedure TMainForm.ImageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Shift = [ssLeft] then
    FMousePos := Point(X, Y);
end;

{ Pan the svg bitmap image while dragging the mouse }
procedure TMainForm.ImageMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if Shift = [ssLeft] then
  begin
    Image.Left := Image.Left + X - FMousePos.X;
    Image.Top := Image.Top + Y - FMousePos.Y;
    FImgPos.X := Image.Left;
    FImgPos.Y := Image.Top;
  end;
end;

{ Mouse wheel is for zooming into the svg bitmap image }
procedure TMainForm.ImageMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  f: Double;
begin
  // Do not paint the svg here, this will take too long in case of large
  // zoom factors. Instead, trigger a timer. Only the last mouse wheel event
  // will cause a repaint. But after each mouse wheel event, we must
  // calculate the new image position, because catching the zoom point is
  // cumulative.
  if WheelDelta > 0 then
    f := ZOOM_FACTOR
  else
    f := 1.0 / ZOOM_FACTOR;
  FScale := f * FScale;
  FScaleEff := FScaleEff * f;

  FZoomPos.X := MousePos.X;
  FZoomPos.Y := MousePos.Y;
  if Sender = Image then
  begin
    inc(FZoomPos.X, Image.Left);
    inc(FZoomPos.Y, Image.Top);
  end;
  FImgPos.X := round(FZoomPos.X - f * (FZoomPos.X - FImgPos.X));
  FImgPos.Y := round(FZoomPos.Y - f * (FZoomPos.Y - FImgPos.Y));

  MouseWheelTimer.Enabled := true;
end;

procedure TMainForm.ImagePanelMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  ImageMouseWheel(Sender, Shift, WheelDelta, MousePos, Handled);
end;

{ Loads an svg image. Zooms and centers it into the image. }
procedure TMainForm.LoadSVG(AFileName: String);
var
  page: TvPage;
  bmp: TBitmap;
  rimg, rsvg, factor: Double;
  m2: Integer;
begin
  if AFilename = '' then
    exit;

  if not FileExists(AFileName) then
  begin
    MessageDlg(Format('File "%s" not found.', [AFileName]), mtError, [mbOK], 0);
    exit;
  end;

  FFileName := AFileName;

  // Load the file into the memo
  SynEdit.Lines.LoadFromFile(AFileName);

  // Load the svg file into a TvVectorialDocument
  FreeAndNil(FVec);
  FVec := TvVectorialDocument.Create;
  FVec.ReadFromFile(AFilename);
  if FVec.GetPageCount = 0 then
  begin
    MessageDlg('Document has no pages.', mtError, [mbOK], 0);
    FreeAndNil(FVec);
    exit;
  end;

  // Get used page size of svg file
  page := FVec.GetPage(0);

  // svg units are millimeters.
  factor := mm2px(1.0);
  FScale := 1.0;

  // Calculate image size by using a temporary bitmap since we need a canvas
  bmp := TBitmap.Create;
  try
    bmp.SetSize(1, 1);
    page.Render(bmp.Canvas, 0, 0, factor, factor, DO_NOT_DRAW);
    FImgSize.cx := page.RenderInfo.EntityCanvasMaxXY.X - page.RenderInfo.EntityCanvasMinXY.X;
    FImgSize.cy := page.RenderInfo.EntityCanvasMaxXY.Y - page.RenderInfo.EntityCanvasMinXY.Y;
  finally
    bmp.Free;
  end;

  // Calculate initial zoom factor and offset coordinates such that drawing
  // fits into the image area
  m2 := EdMargin.Value * 2;
  if (FImgSize.cx + m2 <= Image.Parent.Width) and
     (FImgSize.cy + m2 <= Image.Parent.Height) then
  begin
    FImgPos.X := (Image.Parent.Width - m2 - FImgSize.cx) div 2;
    FImgPos.Y := (Image.Parent.Height - m2 - FImgSize.cy) div 2;
    FScale := 1.0;
  end else
  begin
    rimg := Image.Parent.Height / Image.Parent.Width;
    rsvg := (FImgSize.cy + m2) / (FImgSize.cx + m2);
    if rimg > rsvg then
    begin
      FScale := Image.Parent.Width / (FImgSize.cx + m2);
      FImgPos.X := -m2 div 2;
      FImgPos.Y := round((Image.Parent.Height - FScale * FImgSize.cy - m2) / 2);
//      FImgPos.Y := 0;
    end else
    begin
      FScale := Image.Parent.Height / (FImgSize.cy + m2);
      FImgPos.X := round((Image.Parent.Width - FScale * FImgSize.cx - m2) / 2);
      FImgPos.Y := -m2 div 2;
    end;
  end;

  // Overall magnification factor
  FScaleEff := FScale * factor;

  // Paint the initial version of the file
  PrepareCanvas(Image.Picture.Bitmap.Canvas);
  DrawSVG(Image.Picture.Bitmap.Canvas, FImgSize);

  // Update information in form
  Caption := Format('%s - %s', [PROGRAM_NAME, ExpandFilename(AFilename)]);
  UpdateStatusInfo;
end;

procedure TMainForm.MouseWheelTimerTimer(Sender: TObject);
begin
  // Stop the timer
  MouseWheelTimer.Enabled := false;

  // Repaint the svg image.
  RedrawSVG;
end;

procedure TMainForm.PageControlChange(Sender: TObject);
begin
  if PageControl.ActivePage = PgImage then
    UpdateStatusInfo
  else
    Statusbar.Panels[0].Text := '';
end;

procedure TMainForm.ShellListViewCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
var
  fn1, fn2: String;
begin
  fn1 := Item1.Caption;
  fn2 := Item1.Caption;
  if (fn1[1] = '_') or (fn2[1] = '_') then
    Compare := AnsiCompareText(fn1, fn2)
  else
    Compare := CompareText(fn1, fn2);
end;

procedure TMainForm.ShellListViewSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  fn: String;
begin
  if Selected then
  begin
    fn := ShellListview.GetPathFromItem(Item);
    ShellTreeview.MakeSelectionVisible;
    LoadSVG(fn);
    UpdateHistory(fn);
  end;
end;

procedure TMainForm.ShellTreeViewGetImageIndex(Sender: TObject;
  Node: TTreeNode);
begin
  Node.ImageIndex := 0;
end;

procedure TMainForm.ShellTreeViewGetSelectedIndex(Sender: TObject;
  Node: TTreeNode);
begin
  Node.SelectedIndex := 1;
end;

// https://forum.lazarus.freepascal.org/index.php/topic,61347.msg462091.html#msg462091
function TMainForm.ShellTreeViewSortCompare(Item1, Item2: TFileItem): integer;
var
  fn1, fn2: String;
begin
  // Make sure that folders are moved to the top
  Result := ord(Item2.isFolder) - ord(Item1.isFolder);
  if Result = 0 then
  begin
    fn1 := Item1.FileInfo.Name;
    fn2 := Item2.FileInfo.Name;
    // Move names with leading underscors to the top
    if (fn1[1] = '_') or (fn2[1] = '_') then
      Result := AnsiCompareText(fn1, fn2)
    else
      Result := CompareText(fn1, fn2);
  end;
end;

procedure TMainForm.PrepareCanvas(ACanvas: TCanvas);
var
  page: TvPage;
  delta: Integer;
begin
  page := FVec.GetPage(0);
  page.Render(ACanvas, 0, 0, FScaleEff, FScaleEff, DO_NOT_DRAW);

  delta := 2 * round(FScale * EdMargin.Value);
  FImgSize.cx := page.RenderInfo.EntityCanvasMaxXY.X - page.RenderInfo.EntityCanvasMinXY.X + delta;
  FImgSize.cy := page.RenderInfo.EntityCanvasMaxXY.Y - page.RenderInfo.EntityCanvasMinXY.Y + delta;
  Image.SetBounds(FImgPos.X, FImgPos.Y, FImgSize.cx, FImgSize.cy);
  Image.Picture.Bitmap.SetSize(FImgSize.cx, FImgSize.cy);
end;

procedure TMainForm.ReadIni;
var
  ini: TIniFile;
  L, T, W, H, p: Integer;
  R: TRect;
  List: TStrings;
  i: Integer;
  s: String;
begin
  ini := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    T := Ini.ReadInteger('Position', 'Top', Top);
    L := Ini.ReadInteger('Position', 'Left', Left);
    W := Ini.ReadInteger('Position', 'Width', Width);
    H := Ini.ReadInteger('Position', 'Height', Height);
    R := Screen.WorkAreaRect;
    if W > R.Width then W := R.Width;
    if H > R.Height then H := R.Height;
    if L < R.Left then L := R.Left;
    if T < R.Top then T := R.Top;
    if L + W > R.Right then L := R.Right - W - GetSystemMetrics(SM_CXSIZEFRAME);
    if T + H > R.Bottom then T := R.Bottom - H - GetSystemMetrics(SM_CYCAPTION) - GetSystemMetrics(SM_CYSIZEFRAME);
    SetBounds(L, T, W, H);
    WindowState := wsNormal;
    Application.ProcessMessages;
    WindowState := TWindowState(ini.ReadInteger('Position', 'WindowState', 0));

    p := ini.ReadInteger('Settings', 'LeftSplitter', LeftPanel.Width);
    if p > ClientWidth then p := ClientWidth div 4;
    LeftPanel.Width := p;
    p := ini.ReadInteger('Settings', 'ShellSplitter', ShellTreeView.Height);
    if p > ClientHeight then p := ClientHeight div 2;
    ShellTreeView.Height := p;

    FFileName := ini.ReadString('Settings', 'Filename', '');

    CbHistory.Items.BeginUpdate;
    List := TStringList.Create;
    try
      ini.ReadSection('History', List);
      CbHistory.Items.Clear;
      for i := 0 to List.Count-1 do
      begin
        s := ini.ReadString('History', Format('Item%d', [i+1]), '');
        if (s <> '') and DirectoryExists(s) then
          CbHistory.Items.Add(s);
      end;
      if CbHistory.Items.Count > 0 then
        CbHistory.ItemIndex := 0;
    finally
      List.Free;
      CbHistory.Items.EndUpdate;
    end;
  finally
    ini.Free;
  end;
end;

procedure TMainForm.RedrawSVG;
begin
  // Repaint the svg image.
  PrepareCanvas(Image.Picture.Bitmap.Canvas);
  DrawSVG(Image.Picture.Bitmap.Canvas, FImgSize);

  // Update some status information
  UpdateStatusInfo;
end;

procedure TMainForm.UpdateHistory(AFileName: String);
var
  dir: String;
  idx: Integer;
begin
  if AFileName = '' then
    exit;
  dir := ExtractFilePath(AFileName);
  idx := CbHistory.Items.IndexOf(dir);
  if idx = -1 then
    CbHistory.Items.Insert(0, dir)
  else
    CbHistory.Items.Move(idx, 0);
  while CbHistory.Items.Count > 10 do
    CbHistory.Items.Delete(CbHistory.Items.Count-1);
  CbHistory.ItemIndex := 0;
end;

procedure TMainForm.UpdateStatusInfo;
var
  page: TvPage;
begin
  page := FVec.GetPage(0);
  Statusbar.Panels[0].Text := Format('page.width=%.1f page.height=%.1f w=%d h=%d scale=%.2f', [
    page.Width, page.Height, Image.Width, Image.Height, FScale
  ]);
end;

procedure TMainForm.WriteIni;
var
  ini: TIniFile;
  i: Integer;
begin
  ini := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    ini.WriteInteger('Position', 'Top', RestoredTop);
    ini.WriteInteger('Position', 'Left', RestoredLeft);
    ini.WriteInteger('Position', 'Width', RestoredWidth);
    ini.WriteInteger('Position', 'Height', RestoredHeight);
    ini.WriteInteger('Position', 'WindowState', Integer(WindowState));
    ini.WriteInteger('Settings', 'LeftSplitter', LeftPanel.Width);
    ini.WriteInteger('Settings', 'ShellSplitter', ShellTreeView.Height);
    ini.WriteString('Settings', 'FileName', FFileName);
    ini.EraseSection('History');
    for i := 0 to CbHistory.Items.Count-1 do
      ini.WriteString('History', Format('Item%d', [i+1]), CbHistory.Items[i]);
  finally
    ini.Free;
  end;
end;

end.

