unit reMainUnit;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Math, resource, TypInfo, fpImage, fpReadPNG, fpReadJPEG,
  LazUTF8, Graphics, SynEdit, Forms, Controls, Dialogs, ComCtrls, ActnList, Menus,
  ExtCtrls, Grids, Buttons, StdCtrls,
  BitmapResource, VersionResource, GroupIconResource, GroupCursorResource;

type

  { TreMainForm }

  TreMainForm = class(TForm)
    Bevel1: TBevel;
    fileSave: TAction;
    HeaderControl1: THeaderControl;
    hlpAbout: TAction;
    fileExit: TAction;
    fileOpen: TAction;
    ActionList1: TActionList;
    Image1: TImage;
    ImageList1: TImageList;
    lblWidth: TLabel;
    lblHeight: TLabel;
    lblPixelformat: TLabel;
    lblImageIndexCount: TLabel;
    infoWidth: TLabel;
    infoHeight: TLabel;
    infoPixelformat: TLabel;
    infoImageIndexCount: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    IconNavigatorPanel: TPanel;
    ImagePropsPanel: TPanel;
    btnPrevImage: TSpeedButton;
    btnNextImage: TSpeedButton;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    StringGrid1: TStringGrid;
    SynEdit1: TSynEdit;
    tabBinary: TTabSheet;
    tabString: TTabSheet;
    tabImage: TTabSheet;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    TreeView1: TTreeView;
    procedure fileExitExecute(Sender: TObject);
    procedure fileOpenExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HeaderControl1SectionResize(HeaderControl: TCustomHeaderControl;
      Section: THeaderSection);
    procedure hlpAboutExecute(Sender: TObject);
    procedure btnPrevImageClick(Sender: TObject);
    procedure btnNextImageClick(Sender: TObject);
    procedure Splitter1ChangeBounds(Sender: TObject);
    procedure TreeView1SelectionChanged(Sender: TObject);
  private
    Res:TResources;
    FActivated: Boolean;
    procedure ClearDisplay;
    procedure LoadVersionResource(V:TVersionResource);
    procedure LoadBitmapResource(B:TBitmapResource);
    procedure LoadGroupCursorResource(G: TGroupCursorResource);
    procedure LoadGroupIconResource(G:TGroupIconResource);
    procedure LoadResourceAsBinary(R: TAbstractResource);
    function ResourceIsImage(S: TStream): Boolean;
    procedure UpdatePictureInfo(Pic: TPicture; IsIcon: Boolean);
  public
    procedure OpenFile(const AFileName:string);
  end;

var
  reMainForm: TreMainForm;

implementation

{$R *.lfm}

uses
  winpeimagereader, elfreader, resreader, reAboutUnit, reConstsUnit;

{ TreMainForm }

procedure TreMainForm.fileExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TreMainForm.fileOpenExecute(Sender: TObject);
begin
  if OpenDialog1.Execute then
    OpenFile(UTF8ToSys(OpenDialog1.FileName));
end;

procedure TreMainForm.FormActivate(Sender: TObject);
var
  wmax: Integer;
begin
  if not FActivated then
  begin
    FActivated := true;
    wmax := Max(lblWidth.Width, lblHeight.Width);
    wmax := Max(lblPixelFormat.Width, Max(lblImageIndexCount.Width, wmax));
    infoWidth.BorderSpacing.Left := wmax; // + 16;
  end;
end;

procedure TreMainForm.FormCreate(Sender: TObject);
begin
  Caption:=sResourceExplorer;

  fileSave.Caption:=sSaveResource; //'Save resource...'
  hlpAbout.Caption:=sAbout; //'About...'
  fileExit.Caption:=sExit; //'Exit'
  fileOpen.Caption:=sOpen; //'Open...'
  MenuItem1.Caption:=sFile;
  MenuItem2.Caption:=sHelp;
  tabString.Caption:=sStrings;
  tabImage.Caption:=sImage;
  HeaderControl1.Sections[0].Text := sResources;
  lblWidth.Caption := sWidth;
  lblHeight.Caption := sHeight;
  lblPixelFormat.Caption := sPixelFormat;
  lblImageIndexCount.Caption := sImageIndex;
  btnPrevImage.Hint := sPrevImage;
  btnNextImage.Hint := sNextImage;

  {$IFDEF Windows}
  OpenDialog1.Filter := sAllFilesExcutableFilesExeExeDLLDllDll;
  {$ELSE}
  OpenDialog1.Filter := sAllFilesSharedLibSoSo;
  {$ENDIF}
  Splitter1ChangeBounds(nil);
  SynEdit1.Gutter.Visible := False;
  SynEdit1.Font.Quality := fqClearType;
  ClearDisplay;

  if ParamCount > 0 then
    OpenFile(ParamStr(1));
end;

procedure TreMainForm.FormDestroy(Sender: TObject);
begin
  if Assigned(Res) then Res.Free;
end;

procedure TreMainForm.HeaderControl1SectionResize(
  HeaderControl: TCustomHeaderControl; Section: THeaderSection);
begin
  TreeView1.Width := HeaderControl1.Sections[0].Width;
  HeaderControl1.Sections[1].Width := Width - TreeView1.Width;
end;

procedure TreMainForm.hlpAboutExecute(Sender: TObject);
begin
  reAboutForm:=TreAboutForm.Create(Application);
  reAboutForm.ShowModal;
  reAboutForm.Free;
end;

procedure TreMainForm.btnPrevImageClick(Sender: TObject);
begin
  if Image1.Picture.Graphic is TIcon then
  begin
    if Image1.Picture.Icon.Current > 0 then
    begin
      Image1.Picture.Icon.Current := Image1.Picture.Icon.Current - 1;
      UpdatePictureInfo(Image1.Picture, true);
    end;
  end;
end;

procedure TreMainForm.btnNextImageClick(Sender: TObject);
begin
  if Image1.Picture.Graphic is TIcon then
  begin
    if Image1.Picture.Icon.Current < Image1.Picture.Icon.Count-1 then
    begin
      Image1.Picture.Icon.Current := Image1.Picture.Icon.Current + 1;
      UpdatePictureInfo(Image1.Picture, true);
    end;
  end;
end;

procedure TreMainForm.Splitter1ChangeBounds(Sender: TObject);
begin
  HeaderControl1.Sections[0].Width:=TreeView1.Width;
  HeaderControl1.Sections[1].Width:=Width -  TreeView1.Width;
end;

procedure TreMainForm.TreeView1SelectionChanged(Sender: TObject);
var
  ResItem:TAbstractResource;
begin
  if Assigned(TreeView1.Selected) then ClearDisplay;
  if Assigned(TreeView1.Selected) and Assigned(TreeView1.Selected.Data) then
  begin
    ResItem:=TAbstractResource(TreeView1.Selected.Data);
    HeaderControl1.Sections[1].Text:=ResItem.ClassName + ' : ' + ResItem.Name.Name;
    if ResItem is TVersionResource then
      LoadVersionResource(ResItem as TVersionResource)
    else
    if ResItem is TBitmapResource then
      LoadBitmapResource(ResItem as TBitmapResource)
    else
    if ResItem is TGroupIconResource then
      LoadGroupIconResource(ResItem as TGroupIconResource)
    else
    if ResItem is TGroupCursorResource then
      LoadGroupCursorResource(ResItem as TGroupCursorResource)
    else
      LoadResourceAsBinary(ResItem);
  end;
end;


procedure TreMainForm.ClearDisplay;
begin
  StringGrid1.Clean;
  StringGrid1.Visible := False;
  Image1.Picture.Clear;
  SynEdit1.Lines.Clear;
end;

procedure TreMainForm.LoadVersionResource(V: TVersionResource);
var
  i,j, k:integer;
begin
  PageControl1.ActivePage:=tabString;
  StringGrid1.Visible := True;
  k:=0;
  StringGrid1.RowCount:=0;
  StringGrid1.ColCount:=2;
  for i:=0 to V.StringFileInfo.Count-1 do
  begin
    for j:=0 to V.StringFileInfo[i].Count-1 do
    begin
      StringGrid1.RowCount:=StringGrid1.RowCount + 1;
      StringGrid1.Cells[0, k]:=SysToUTF8(V.StringFileInfo[i].Keys[j]);
      StringGrid1.Cells[1, k]:=SysToUTF8(V.StringFileInfo[i].ValuesByIndex[j]);
      inc(k);
    end;
  end;
end;

procedure TreMainForm.LoadBitmapResource(B: TBitmapResource);
begin
  PageControl1.ActivePage:=tabImage;
  B.BitmapData.Position:=0;
  Image1.Picture.Bitmap.LoadFromStream(B.BitmapData);
  UpdatePictureInfo(Image1.Picture, false);
end;

procedure TreMainForm.LoadGroupCursorResource(G: TGroupCursorResource);
var
  img: TCursorImage;
begin
  PageControl1.ActivePage := tabImage;
  G.ItemData.Position := 0;
  img := TCursorImage.Create;
  img.LoadFromStream(G.ItemData);
  Image1.Picture.Assign(img);
  img.Free;
  UpdatePictureInfo(Image1.Picture, false);
end;

procedure TreMainForm.LoadGroupIconResource(G: TGroupIconResource);
begin
  PageControl1.ActivePage := tabImage;
  G.ItemData.Position := 0;
  Image1.Picture.Icon.LoadFromStream(G.ItemData);
  UpdatePictureInfo(Image1.Picture, true);
end;

procedure TreMainForm.UpdatePictureInfo(Pic: TPicture; IsIcon: Boolean);
begin
  if Pic = nil then
    ImagePropsPanel.Hide
  else
  begin
    ImagePropsPanel.Show;
    infoWidth.Caption := IntToStr(Pic.Width);
    infoHeight.Caption := IntToStr(Pic.Height);
    infoPixelFormat.Caption := GetEnumName(TypeInfo(TPixelformat), Integer(TRasterImage(Pic.Graphic).Pixelformat));
    if IsIcon then
      infoImageIndexCount.Caption := Format(sIndexOfCount, [Pic.Icon.Current + 1, Pic.Icon.Count]);
    IconNavigatorPanel.Visible := isIcon;
    infoImageIndexCount.Visible := isIcon;
    lblImageIndexCount.Visible := isIcon;
  end;
end;

function TreMainForm.ResourceIsImage(S: TStream): Boolean;
var
  readerClass: TFPCustomImageReaderClass;
begin
  Result := true;

  S.Position := 0;
  readerClass := TFPCustomImage.FindReaderFromStream(S);
  if readerClass <> nil then
    exit;

  Result := false;
end;

procedure TreMainForm.LoadResourceAsBinary(R: TAbstractResource);
var
  Offset, Size, BytesRead: Int64;
  Buf: Array[0..15] of Byte;
  Line, BinLine, AscLine: ShortString;
  i: Integer;
begin
  PageControl1.ActivePage:=tabBinary;
  //writeln('TreMainForm.LoadResourceAsBinary');
  //writeln('  ClassName      = ',R.ClassName);
  //writeln('  DataSize       = ',R.DataSize);
  //writeln('  R.RawData.Size = ',R.RawData.Size);
  //writeln('  DataOffset     = ',R.DataOffset);
  //writeln('  DataVersion    = ',R.DataVersion);
  SynEdit1.Lines.BeginUpdate;
  try
    Offset := 0;
    R.RawData.Position := 0;
    Size := R.RawData.Size;
    while (Offset < Size) do
    begin
      FillChar(Buf{%H-}, SizeOf(Buf), #0);
      BytesRead := R.RawData.Read(Buf[0], SizeOf(Buf));
      //writeln('    BytesRead = ',BytesRead);
      Line := '';
      BinLine := '';
      Ascline := '';
      for i := 0 to BytesRead - 1 do
      begin
        BinLine := BinLine + IntToHex(Buf[i],2) + #32;
        if (Buf[i] in [32..127]) then
          AscLine := AscLine + Char(Buf[i])
        else
          AscLine := AscLine + #32;
      end;
      for i := (BytesRead + 1) to SizeOf(Buf) do
      begin
        BinLine := BinLine + '   ';
        AscLine := AscLine + ' ';
      end;
      Line := IntToHex(Offset, 8) + #32;
      Line := Line + BinLine + #32 + AscLine + LineEnding;
      //writeln('    Line = ',Line);
      SynEdit1.Lines.Add(Line);
      Inc(Offset, SizeOf(Buf));
    end;
  finally
    SynEdit1.Lines.EndUpdate;
  end;

  // Check whether the resource is a non-RT_BITMAP image and try to display it
  // in the "Image" tab.
  if ResourceIsImage(R.RawData) then
  begin
    Image1.Picture.LoadFromStream(R.RawData);
    UpdatePictureInfo(Image1.Picture, false);
  end;
end;

procedure TreMainForm.OpenFile(const AFileName: string);
var
  Ext:string;
  Reader:TAbstractResourceReader;
  i:integer;
  Root, ResNode:TTreeNode;
begin
  if not FileExists(AFileName) then exit;
  Ext:=LowerCase(ExtractFileExt(AFileName));
  if (Ext = '.exe') or (Ext = '.dll') then
    Reader:=TWinPEImageResourceReader.Create
  else
  if (Ext = '.res')then
    Reader:=TResResourceReader.Create
  else
  if (Ext = '.o')then
    Reader:=TResResourceReader.Create
  else
  if (Ext = '')then
    Reader:=TElfResourceReader.Create;

  TreeView1.Items.Clear;
  ClearDisplay;
  if Assigned(Res) then
    FreeAndNil(Res);

  if not Assigned(Reader) then exit;

  Res:=TResources.Create;
  try
    Res.LoadFromFile(AFileName, Reader);

    for i:=0 to Res.Count-1 do
    begin
      Root:=TreeView1.Items.FindTopLvlNode(Res[i].ClassName);
      if not Assigned(Root) then
        Root:=TreeView1.Items.AddChild(nil, Res[i].ClassName);
      ResNode:=TreeView1.Items.AddChild(Root, Res[i].Name.Name);
      ResNode.Data:=Res[i];
    end;
  finally
    Reader.Free;
  end;

  StatusBar1.SimpleText:=AFileName;
end;

end.

