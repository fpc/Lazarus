unit reMainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, ActnList, Menus, ExtCtrls, Grids, resource,
  bitmapresource, versionresource, groupcursorresource, groupiconresource;

type

  { TreMainForm }

  TreMainForm = class(TForm)
    fileSave: TAction;
    HeaderControl1: THeaderControl;
    hlpAbout: TAction;
    fileExit: TAction;
    fileOpen: TAction;
    ActionList1: TActionList;
    Image1: TImage;
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    StringGrid1: TStringGrid;
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
    procedure FormCreate(Sender: TObject);
    procedure HeaderControl1SectionResize(HeaderControl: TCustomHeaderControl;
      Section: THeaderSection);
    procedure hlpAboutExecute(Sender: TObject);
    procedure Splitter1ChangeBounds(Sender: TObject);
    procedure TreeView1Click(Sender: TObject);
  private
    Res:TResources;
    procedure LoadVersionResource(V:TVersionResource);
    procedure LoadBitmapResource(B:TBitmapResource);
    procedure LoadGroupIconResource(G:TGroupIconResource);
  public
    procedure OpenFile(const AFileName:string);
  end;

var
  reMainForm: TreMainForm;

implementation

{$R *.lfm}

uses
  winpeimagereader, elfreader, coffreader, resreader, reAboutUnit,
  stringtableresource, acceleratorsresource, reConstsUnit;

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

  Splitter1ChangeBounds(nil);
end;

procedure TreMainForm.HeaderControl1SectionResize(
  HeaderControl: TCustomHeaderControl; Section: THeaderSection);
begin
  TreeView1.Width:=HeaderControl1.Sections[0].Width;
  HeaderControl1.Sections[1].Width:=Width -  TreeView1.Width;
end;

procedure TreMainForm.hlpAboutExecute(Sender: TObject);
begin
  reAboutForm:=TreAboutForm.Create(Application);
  reAboutForm.ShowModal;
  reAboutForm.Free;
end;

procedure TreMainForm.Splitter1ChangeBounds(Sender: TObject);
begin
  HeaderControl1.Sections[0].Width:=TreeView1.Width;
  HeaderControl1.Sections[1].Width:=Width -  TreeView1.Width;
end;

procedure TreMainForm.TreeView1Click(Sender: TObject);
var
  ResItem:TAbstractResource;
begin
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
      ;
  end;
end;

procedure TreMainForm.LoadVersionResource(V: TVersionResource);
var
  i,j, k:integer;
begin
  PageControl1.ActivePage:=tabString;
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
  //
end;

procedure TreMainForm.LoadGroupIconResource(G: TGroupIconResource);
begin

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
  if (Ext = '.res')then
    Reader:=TResResourceReader.Create
    else
  if (Ext = '.o')then
    Reader:=TResResourceReader.Create
  else
  if (Ext = '')then
    Reader:=TElfResourceReader.Create;

  TreeView1.Items.Clear;
  if Assigned(Res) then
    Res.Free;

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

