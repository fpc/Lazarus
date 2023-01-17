unit wmf_mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ShellCtrls,
  ExtCtrls, ComCtrls, StdCtrls, fpvectorial, Types;

type

  { TMainForm }

  TMainForm = class(TForm)
    BtnSaveAsWMF: TButton;
    CbHistory: TComboBox;
    Image1: TImage;
    ImageList: TImageList;
    ImageInfo: TLabel;
    LeftPanel: TPanel;
    Panel1: TPanel;
    ImagePanel: TPanel;
    RbMaxSize: TRadioButton;
    RbOrigSize: TRadioButton;
    ScrollBox1: TScrollBox;
    ShellListView: TShellListView;
    ShellTreeView: TShellTreeView;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    procedure BtnSaveAsWMFClick(Sender: TObject);
    procedure CbHistoryCloseUp(Sender: TObject);
    procedure CbHistoryDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure CbHistoryEditingDone(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure RbMaxSizeChange(Sender: TObject);
    procedure RbOrigSizeChange(Sender: TObject);
    procedure ScrollBox1Resize(Sender: TObject);
    procedure ShellListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ShellTreeViewExpanded(Sender: TObject; Node: TTreeNode);
    procedure ShellTreeViewGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure ShellTreeViewGetSelectedIndex(Sender: TObject; Node: TTreeNode);
  private
    { private declarations }
    FVec: TvVectorialDocument;
    FFileName: String;
    FFormActivated: Boolean;
    procedure LoadImage(const AFileName: String);
    procedure PaintImage(APage: TvPage);
    procedure ReadIni;
    procedure UpdateHistory(const AFileName: String);
    procedure WriteIni;
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  LCLType, LCLIntf,
  IniFiles, LazFileUtils, FileCtrl, fpvUtils;

const
  PROGRAM_NAME = 'wmfViewer';
  INCH = 25.4;


{ TMainForm }

procedure TMainForm.BtnSaveAsWMFClick(Sender: TObject);
var
  fn: String;
begin
  if ShellListView.Selected = nil then
    exit;
  fn := ShellListview.GetPathFromItem(ShellListview.Selected);
  fn := ChangeFileExt(fn, '') + '-saved.wmf';
  if FileExistsUTF8(fn) then begin
    if MessageDlg(Format('File "%s" already exists. Overwrite?', [fn]),
      mtConfirmation, [mbYes, mbNo], 0) <> mrYes then exit;
    DeleteFileUTF8(fn);
  end;
  FVec.WriteToFile(fn, vfWindowsMetafileWMF);
  ShowMessage(Format('Saved as "%s"', [fn]));
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
    LoadImage(fn);
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Set correct dpi for scaling by wmf reader
  ScreenDPIX := ScreenInfo.PixelsPerInchX;
  ScreenDPIY := ScreenInfo.PixelsPerInchY;

  Caption := PROGRAM_NAME;
  {$IFNDEF MSWINDOWS}
  ShellTreeView.Images := ImageList1;
  ShellTreeView.OnGetImageIndex := @ShellTreeViewGetImageIndex;
  ShellTreeView.OnGetSelectedIndex := @ShellTreeViewGetSelectedIndex;
  {$ENDIF}
  ShellListview.Mask := '*.wmf';

  ReadIni;

  if ParamCount > 0 then begin
    ShellTreeview.Path := ExtractFilepath(ParamStr(1));
    ShellTreeview.MakeSelectionVisible;
    LoadImage(ParamStr(1));
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  WriteIni;
  FreeAndNil(FVec);
end;

procedure TMainForm.LoadImage(const AFileName: String);
var
  page: TvPage;
begin
  FreeAndNil(FVec);
  try
    FVec := TvVectorialDocument.Create;
    // Load the image file into a TvVectorialDocument
    FVec.ReadFromFile(AFilename);
    // Draw the image
    FVec.GuessDocumentSize;
    page := FVec.GetPageAsVectorial(0);
    if (page.Width = 0) or (page.Height = 0) then
      page.CalculateDocumentSize;
    PaintImage(page);
    // Misc
    Caption := Format('%s - "%s"', [PROGRAM_NAME, AFileName]);
    ImageInfo.Caption := Format('%.0f x %.0f', [page.Width, page.Height]);
    FFileName := AFileName;
  except
    on E:Exception do
      MessageDlg(E.Message, mtError, [mbOK], 0);
  end;
end;

procedure TMainForm.PaintImage(APage: TvPage);
var
  bmp: TBitmap;
  multiplierX, multiplierY: Double;
  wimg, himg: Integer;
  dx, dy: Integer;
  zoom: Double;
begin
  if APage = nil then
    exit;

  // For conversion of the mm returned by the wmf reader to screen pixels
  multiplierX := 1.0; //ScreenInfo.PixelsPerInchX / INCH;
  multiplierY := 1.0; //ScreenInfo.PixelsPerInchY / INCH;

  // Calc image size
  wimg := round(APage.Width * multiplierX);   // image size in pixels
  himg := round(APage.Height * multiplierY);
  if (wimg = 0) or (himg = 0) then
    exit;

  // Create a temporary bitmap onto which the image file will be drawn
  bmp := TBitmap.Create;
  try
    if RbMaxSize.Checked then begin
      if himg/wimg > Scrollbox1.ClientHeight / Scrollbox1.ClientWidth then
      begin
        bmp.Height := Scrollbox1.ClientHeight;
        bmp.Width := round(wimg/himg * bmp.Height);
        multiplierX := multiplierX * Scrollbox1.ClientHeight / himg;
        multiplierY := multiplierY * Scrollbox1.ClientHeight / himg;
      end else begin
        bmp.Width := Scrollbox1.ClientWidth;
        bmp.Height := round(himg/wimg * bmp.Width);
        multiplierX := multiplierX * Scrollbox1.ClientWidth / wimg;
        multiplierY := multiplierY * Scrollbox1.ClientWidth / wimg;
      end;
    end else begin
      bmp.SetSize(wimg, himg);
      multiplierX := 1.0;
      multiplierY := 1.0;
    end;

    bmp.Canvas.Brush.Color := clWindow;
    bmp.Canvas.FillRect(0, 0, bmp.Width, bmp.Height);

   // APage.AutoFit(bmp.Canvas, wimg, wimg, wimg, dx, dy, zoom);

    if APage.UseTopLeftCoordinates then
      APage.Render(bmp.Canvas, 0, 0, multiplierX, multiplierY) else
      APage.Render(bmp.Canvas, 0, himg, multiplierX, -multiplierY);

    {
    if APage.UseTopLeftCoordinates then
      APage.Render(bmp.Canvas, dx, dy, zoom, zoom) else
      APage.Render(bmp.Canvas, dx, himg - dy, zoom, -zoom);
      }

    {
    if SameText(ExtractFileExt(FFileName), '.wmf') then
      APage.Render(bmp.Canvas, dx, dy, zoom, zoom) else
      APage.Render(bmp.Canvas, dx, himg - dy, zoom, -zoom);
     }

//    APage.Render(bmp.Canvas, 0, 0, multiplierX, multiplierY);
    // Assign the bitmap to the image's picture.
    Image1.Picture.Assign(bmp);
    Image1.Width := bmp.Width;
    Image1.Height := bmp.Height;
  finally
    bmp.Free;
  end;
end;

procedure TMainForm.RbMaxSizeChange(Sender: TObject);
begin
  if FVec <> nil then
    PaintImage(FVec.GetPageAsVectorial(0));
end;

procedure TMainForm.RbOrigSizeChange(Sender: TObject);
begin
  if FVec <> nil then
    PaintImage(FVec.GetPageAsVectorial(0));
end;

procedure TMainForm.ShellListViewSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  fn: String;
begin
  if Selected then
  begin
    fn := ShellListview.GetPathFromItem(ShellListview.Selected);
    ShellTreeview.MakeSelectionVisible;
    LoadImage(fn);
    UpdateHistory(fn);
  end;
end;

procedure TMainForm.ShellTreeViewExpanded(Sender: TObject; Node: TTreeNode);
begin
  ShellTreeView.AlphaSort;
end;

procedure TMainForm.ShellTreeViewGetImageIndex(Sender: TObject; Node: TTreeNode);
begin
  if Node.Level = 0 then
    Node.ImageIndex := 2 else
    Node.ImageIndex := 0;
end;

procedure TMainForm.ShellTreeViewGetSelectedIndex(Sender: TObject; Node: TTreeNode);
begin
  Node.SelectedIndex := 1;
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

(*
procedure TMainForm.ReadFromIni;
var
  ini: TCustomIniFile;
  L, T, W, H: Integer;
  R: TRect;
begin
  ini := TMemIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    L := ini.ReadInteger('MainForm', 'Left', Left);
    T := ini.ReadInteger('MainForm', 'Top', Top);
    W := ini.ReadInteger('MainForm', 'Width', Width);
    H := ini.ReadInteger('MainForm', 'Height', Height);
    R := Screen.DesktopRect;
    if L+W > R.Right then L := R.Right - W;
    if L < R.Left then L := R.Left;
    if T+H > R.Bottom then T := R.Bottom - H;
    if T < R.Top then T := R.Top;
    SetBounds(L, T, W, H);
    ShellTreeView.Height := ini.ReadInteger('MainForm', 'ShellTreeViewHeight', ShellTreeView.Height);
    LeftPanel.Width := ini.ReadInteger('MainForm', 'ShellControlsWidth', LeftPanel.Width);
    ShellTreeview.Path := ini.ReadString('Settings', 'InitialDir', '');
  finally
    ini.Free;
  end;
end;
*)
procedure TMainForm.ScrollBox1Resize(Sender: TObject);
begin
  if FVec <> nil then
    PaintImage(FVec.GetPageAsVectorial(0));
end;

procedure TMainForm.UpdateHistory(const AFileName: String);
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
           {
procedure TMainForm.WriteToIni;
var
  ini: TCustomIniFile;
begin
  ini := TMemIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    if WindowState = wsNormal then begin
      ini.WriteInteger('MainForm', 'Left', Left);
      ini.WriteInteger('MainForm', 'Top', Top);
      ini.WriteInteger('MainForm', 'Width', Width);
      ini.WriteInteger('MainForm', 'Height', Height);
      ini.WriteInteger('MainForm', 'ShellTreeViewHeight', ShellTreeView.Height);
      ini.WriteInteger('MainForm', 'ShellControlsWidth', LeftPanel.Width);
    end;
    ini.WriteString('Settings', 'InitialDir', ShellTreeview.Path);
  finally
    ini.Free;
  end;
end;
            }

end.

