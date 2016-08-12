unit wmf_mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ShellCtrls,
  ExtCtrls, ComCtrls, fpvectorial;

type

  { TForm1 }

  TForm1 = class(TForm)
    Image1: TImage;
    ImageList: TImageList;
    LeftPanel: TPanel;
    ScrollBox1: TScrollBox;
    ShellListView: TShellListView;
    ShellTreeView: TShellTreeView;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ShellListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ShellTreeViewGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure ShellTreeViewGetSelectedIndex(Sender: TObject; Node: TTreeNode);
  private
    { private declarations }
    FVec: TvVectorialDocument;
    procedure LoadImage(const AFileName: String);
    procedure ReadFromIni;
    procedure WriteToIni;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  IniFiles, fpvUtils;

const
  PROGRAM_NAME = 'wmfViewer';


{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Set correct dpi for scaling of mapmode MM_TEXT.
  ScreenDPIX := ScreenInfo.PixelsPerInchX;
  ScreenDPIY := ScreenInfo.PixelsPerInchY;

  Caption := PROGRAM_NAME;
  ShellListview.Mask := '*.svg;*.wmf';

  ReadFromIni;

  if ParamCount > 0 then begin
    ShellTreeview.Path := ExtractFilepath(ParamStr(1));
    ShellTreeview.MakeSelectionVisible;
    LoadImage(ParamStr(1));
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  WriteToIni;
  FreeAndNil(FVec);
end;

procedure TForm1.LoadImage(const AFileName: String);
const
  INCH = 25.4;
var
  bmp: TBitmap;
  page: TvPage;
  multiplierX, multiplierY: Double;
begin
  // For conversion of the mm returned by the wmf reader to screen pixels
  multiplierX := ScreenInfo.PixelsPerInchX / INCH;
  multiplierY := ScreenInfo.PixelsPerInchY / INCH;

  // Load the image file into a TvVectorialDocument
  FreeAndNil(FVec);
  try
    FVec := TvVectorialDocument.Create;
    FVec.ReadFromFile(AFilename);
  except
    on E:Exception do
      MessageDlg(E.Message, mtError, [mbOK], 0);
  end;

  if FVec = nil then
    exit;

  // Create a temporary bitmap onto which the image file will be drawn
  bmp := TBitmap.Create;
  try
    page := FVec.GetPage(0);
    bmp.SetSize(
      round(FVec.Width * multiplierX),    // Convert mm to pixels
      round(FVec.Height * multiplierY)
    );
    bmp.Canvas.Brush.Color := clWindow;
    bmp.Canvas.FillRect(0, 0, bmp.Width, bmp.Height);
    page.Render(bmp.Canvas, 0, 0, multiplierX, multiplierY);
    // Assign the bitmap to the image's picture.
    Image1.Picture.Assign(bmp);
    Image1.Width := bmp.Width;
    Image1.Height := bmp.Height;
    // Misc
    Caption := Format('%s - "%s"', [PROGRAM_NAME, AFileName]);
  finally
    bmp.Free;
  end;
end;

procedure TForm1.ShellListViewSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  fn: String;
begin
  if Selected then
  begin
    fn := ShellListview.GetPathFromItem(ShellListview.Selected);
    ShellTreeview.MakeSelectionVisible;
    LoadImage(fn);
  end;
end;

procedure TForm1.ShellTreeViewGetImageIndex(Sender: TObject; Node: TTreeNode);
begin
  if Node.Level = 0 then
    Node.ImageIndex := 2 else
    Node.ImageIndex := 0;
end;

procedure TForm1.ShellTreeViewGetSelectedIndex(Sender: TObject; Node: TTreeNode);
begin
  Node.SelectedIndex := 1;
end;

procedure TForm1.ReadFromIni;
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

procedure TForm1.WriteToIni;
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


end.

