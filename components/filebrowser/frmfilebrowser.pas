unit frmFileBrowser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  LCLType, Forms, Controls, Dialogs, FileCtrl, ComCtrls, StdCtrls, ExtCtrls,
  // LazUtils
  FileUtil, LazFileUtils, LazUTF8,
  // Files
  FileBrowserTypes, Masks;

type
  TOpenFileEvent = procedure(Sender: TObject; const AFileName: string) of object;


  { TFileBrowserForm }

  TFileBrowserForm = class(TForm)
    btnConfigure: TButton;
    btnReload: TButton;
    cbHidden: TCheckBox;
    FileListBox: TFileListBox;
    cbFilePanel: TFilterComboBox;
    cbTreeFilter: TFilterComboBox;
    ilTreeview: TImageList;
    Panel1: TPanel;
    pnlFiles: TPanel;
    Splitter1: TSplitter;
    TV: TTreeView;
    procedure btnConfigureClick(Sender: TObject);
    procedure btnReloadClick(Sender: TObject);
    procedure cbHiddenChange(Sender: TObject);
    procedure cbTreeFilterChange(Sender: TObject);
    procedure FileListBoxDblClick(Sender: TObject);
    procedure cbFilePanelChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TVDblClick(Sender: TObject);
    procedure TVExpanded(Sender: TObject; Node: TTreeNode);
    procedure TVSelectionChanged(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure cbFilePanelSelect(Sender: TObject);
    procedure FileListBoxKeyPress(Sender: TObject; var Key: char);
  private
    FDirectoriesBeforeFiles: Boolean;
    FFilesInTree: Boolean;
    FOnConfigure: TNotifyEvent;
    FOnOpenFile: TOpenFileEvent;
    FOnSelectDir: TNotifyEvent;
    FRoot : TFileSystemEntry;
    FCurrentDir: string;
    FRootDir: string;
    FShowHidden: Boolean;
    FSelectedMask : TMaskList;
    procedure AddEntries(Node: TTreeNode);
    function AddNode(aSibling: TTreeNode; aEntry: TFileSystemEntry): TTreeNode;
    function AddChildNode(aParent: TTreeNode; aEntry: TFileSystemEntry): TTreeNode;
    procedure ConfigNode(aNode: TTreeNode; aEntry: TFileSystemEntry);
    function FindNode(aNodePath: String): TTreeNode;
    function GetAbsolutePath(Node: TTreeNode): string;
    function GetCurrentFile: string;
    function GetTreeFileMask: String;
    function NodeToEntry(aNode: TTreeNode): TFileSystemEntry;
    procedure SetCurrentFile(AValue: string);
    procedure SetDir(const Value: string);
    procedure SetDirectoriesBeforeFiles(AValue: Boolean);
    procedure SetFilesIntree(AValue: Boolean);
    procedure SetRootDir(const Value: string);
    procedure InitializeTreeview;
    procedure SetTreeFileMask(AValue: String);
    function ShowEntry(aEntry: TFilesystemEntry): boolean;
    {$IFDEF MSWINDOWS}
    procedure AddWindowsDriveLetters;
    {$ENDIF}
  public
    procedure ShowFiles;
    { Show the start directory }
    procedure ShowStartDir;
    { return the selected directory }
    function SelectedDir: string;
    { The selected/opened directory }
    property CurrentFile: string read GetCurrentFile write SetCurrentFile;
    { The selected/opened directory }
    property CurrentDirectory: string read FCurrentDir write SetDir;
    { Directory the treeview starts from }
    property RootDirectory: string read FRootDir write SetRootDir;
    { Must we show hidden directories - not working on unix type systems }
    property ShowHidden: Boolean read FShowHidden write FShowHidden default False;
    { Called when user double-clicks file name }
    property OnOpenFile: TOpenFileEvent read FOnOpenFile write FOnOpenFile;
    { Called when user clicks configure button }
    property OnConfigure: TNotifyEvent read FOnConfigure write FOnConfigure;
    { Called when a new directory is selected }
    property OnSelectDir: TNotifyEvent read FOnSelectDir write FOnSelectDir;
    { Show files in tree }
    property FilesInTree : Boolean Read FFilesInTree Write SetFilesIntree;
    { Show directories before files in tree }
    property DirectoriesBeforeFiles : Boolean Read FDirectoriesBeforeFiles Write SetDirectoriesBeforeFiles;
    { selected tree mask }
    property TreeFileMask : String Read GetTreeFileMask Write SetTreeFileMask;
  end;

var
  FileBrowserForm: TFileBrowserForm;


implementation

{$R frmfilebrowser.lfm}

{$IFDEF MSWINDOWS}
uses
  Windows;
{$ENDIF}

const
  cFilter = 'All Files (' + AllFilesMask + ')|' + AllFilesMask +
            '|Source(*.pas;*.pp)|*.pas;*.pp' +
            '|Projectfiles(*.pas;*.pp;*.inc;*.lfm;*.lpr;*.lrs;*.lpi;*.lpk)|' +
            '*.pas;*.pp;*.inc;*.lfm;*.lpr;*.lrs;*.lpi;*.lpk;|';


{ TFileBrowserForm }

procedure TFileBrowserForm.TVExpanded(Sender: TObject; Node: TTreeNode);
begin
  if Node.Count = 0 then
    AddEntries(Node);
end;

procedure TFileBrowserForm.TVSelectionChanged(Sender: TObject);

var
  Entry : TFileSystemEntry;

begin
  Entry:=NodeToEntry(TV.Selected);
  if Entry=Nil then
    Exit;
  if Entry.EntryType=etDirectory then
    begin
    FileListBox.Directory := ChompPathDelim(Entry.AbsolutePath);
    if Assigned(OnSelectDir) then
      OnselectDir(Self);
    end;
end;

procedure TFileBrowserForm.FormActivate(Sender: TObject);
begin
  { for some reason this does not work in FormShow }
  TV.MakeSelectionVisible;
end;

procedure TFileBrowserForm.cbFilePanelSelect(Sender: TObject);
begin
  FileListBox.Mask := cbFilePanel.Mask;
end;

procedure TFileBrowserForm.FileListBoxKeyPress(Sender: TObject; var Key: char);
begin
  if Key = Char(VK_RETURN) then
   FileListBoxDblClick(Sender);
end;

procedure TFileBrowserForm.btnConfigureClick(Sender: TObject);
begin
  if Assigned(FOnConfigure) then
    FOnConfigure(Self);
end;

procedure TFileBrowserForm.btnReloadClick(Sender: TObject);
var
  d: string;
begin
  // save current directory location
  d := ChompPathDelim(SelectedDir);
  InitializeTreeview;
  ShowFiles;
  // restore directory
  CurrentDirectory := d;
end;

procedure TFileBrowserForm.cbHiddenChange(Sender: TObject);
begin
  ShowHidden := cbHidden.Checked;
  if ShowHidden then
    FileListBox.FileType := FileListBox.FileType + [ftHidden]
  else
    FileListBox.FileType := FileListBox.FileType - [ftHidden];
end;

procedure TFileBrowserForm.cbTreeFilterChange(Sender: TObject);
begin
  TreeFileMask:=cbTreeFilter.Mask;
end;

procedure TFileBrowserForm.FileListBoxDblClick(Sender: TObject);
begin
  if Assigned(FOnOpenFile) then
    FOnOpenFile(Self, FileListBox.FileName);
end;

procedure TFileBrowserForm.cbFilePanelChange(Sender: TObject);
begin
  FileListBox.Mask := cbFilePanel.Text;

end;

procedure TFileBrowserForm.FormCreate(Sender: TObject);
begin
  FShowHidden := False;
  InitializeTreeview;
  cbFilePanel.Filter := cFilter;
  cbTreeFilter.Filter := cFilter;
end;

procedure TFileBrowserForm.FormShow(Sender: TObject);
begin
  if TV.Selected <> nil then
    TV.Selected.Expand(False);
end;

procedure TFileBrowserForm.TVDblClick(Sender: TObject);

var
  Entry : TFileSystemEntry;

begin
  Entry:=NodeToEntry(TV.Selected);
  if Entry=nil then
    exit;
  if (Entry.EntryType=etFile) then
    FOnOpenFile(Self, Entry.AbsolutePath);
end;

{ Adds Subdirectories to a passed node if they exist }

function TFileBrowserForm.ShowEntry(aEntry : TFilesystemEntry) : boolean;

begin
  Result:=(aEntry.EntryType=etDirectory);
  if Not Result then
    Result:=(FSelectedMask=Nil) or FSelectedMask.Matches(aEntry.Name);
end;

procedure TFileBrowserForm.AddEntries(Node: TTreeNode);

  procedure ShowTypes(NodeEntry : TFileSystemEntry; aTypes : TEntryTypes);
  var
    Entry : TFileSystemEntry;
    SortList :  TStringList;
    I : Integer;
  begin
    SortList := TStringList.Create;
    try
      For I:=0 to NodeEntry.EntryCount-1 do
        begin
        Entry:=NodeEntry.Entries[I];
        if Entry.EntryType in aTypes then
          SortList.AddObject(Entry.Name,Entry);
        end;
      SortList.Sort;
      For I:=0 to SortList.Count-1 do
        begin
        Entry:=TFileSystemEntry(SortList.Objects[i]);
        if ShowEntry(Entry) then
          AddChildNode(Node,Entry);
        end;
    finally
      SortList.Free;
    end;
  end;

var
  NodeEntry : TFileSystemEntry;
  lTypes : TEntryTypes;
  rOptions : TReadEntryOptions;

begin
  NodeEntry:=NodeToEntry(Node);
  if NodeEntry=Nil then
    exit;
  lTypes:=[etDirectory];
  if FilesInTree then
    Include(lTypes,etFile);
  rOptions:=[];
  if ShowHidden then
    Include(rOptions,reoHidden);
  if NodeEntry.HasEntries(ShowHidden,lTypes) and (NodeEntry.EntryCount=0) then
    NodeEntry.ReadEntries(rOptions);
  if DirectoriesBeforeFiles then
    begin
    ShowTypes(NodeEntry,[etDirectory]);
    ShowTypes(NodeEntry,[etFile,etSymlink]);
    end
  else
    ShowTypes(NodeEntry,[etDirectory,etFile,etSymlink]);
end;

function TFileBrowserForm.GetAbsolutePath(Node: TTreeNode): string;
begin
  Result := '';
  while Node <> nil do
  begin
    if Node.Text = PathDelim then
      Result := Node.Text + Result
    else
      Result := Node.Text + PathDelim + Result;
    Node := Node.Parent;
  end;
end;

function TFileBrowserForm.GetCurrentFile: string;

var
  N : TTreeNode;
  E : TFileSystemEntry;

begin
  Result:='';
  if FilesInTree then
    begin
    N:=TV.Selected;
    if Assigned(N) then
      E:=NodeToEntry(N);
    if Assigned(E) and (E.EntryType=etFile) then
      Result:=E.AbsolutePath;
    end
  else
    With FileListBox do
      if FileName<>'' then
        Result:=IncludeTrailingPathDelimiter(Directory)+FileName;
end;

function TFileBrowserForm.GetTreeFileMask: String;
begin
  if Assigned(FSelectedMask) then
    Result:=FSelectedMask.Mask
  else
    Result:='';
end;

function TFileBrowserForm.FindNode(aNodePath : String) : TTreeNode;

var
  StartDir: string;
  Parts : TStringArray;
  i, p, len: integer;
  SubDir: String;
  Node : TTreeNode;


begin
  Result:=nil;
  Node := TV.Items.GetFirstNode;
  if Node=Nil then
    exit;
  StartDir:=aNodePath;
  if Not FilesInTree then
    StartDir:=IncludeTrailingPathDelimiter(aNodePath);
  p := AnsiPos(RootDirectory, StartDir);
  if p = 1 then
    Delete(StartDir, P, Length(RootDirectory));
  Parts:=StartDir.Split(['/'],TStringSplitOptions.ExcludeEmpty);
  Len:=Length(Parts);
  I:=0;
  While I<Len do
    begin
    SubDir:=Parts[i];
    Node := Node.GetFirstChild;
    while (Node <> nil) and (AnsiCompareStr(Node.Text, SubDir) <> 0) do
      Node := Node.GetNextSibling;
    if Node = nil then
      begin
      break
      end
    else
      Node.Expand(False);
    Inc(I);
    end;
  Result:=Node;
end;

procedure TFileBrowserForm.SetDir(const Value: string);

begin
  FCurrentDir     := Value;
  ShowStartDir;
end;

procedure TFileBrowserForm.ShowStartDir;

var
  StartDir: string;
  Node: TTreeNode;

begin
  StartDir := FCurrentDir;
  if TV.Items.Count = 0 then
    Exit;
  Node:=FindNode(StartDir);
  TV.Selected := Node;
  TV.MakeSelectionVisible;
end;

procedure TFileBrowserForm.SetDirectoriesBeforeFiles(AValue: Boolean);
begin
  if FDirectoriesBeforeFiles=AValue then Exit;
  FDirectoriesBeforeFiles:=AValue;
  InitializeTreeview;
end;

procedure TFileBrowserForm.SetFilesIntree(AValue: Boolean);
begin
  if FFilesInTree=AValue then Exit;
  FFilesInTree:=AValue;
  InitializeTreeview;
end;

procedure TFileBrowserForm.SetRootDir(const Value: string);
var
  DoShowFiles : Boolean;

begin
  if (FRootDir=Value) then exit;
  FRootDir:=Value;
  DoShowFiles:=TV.Items.Count>0;
  InitializeTreeView;
  If DoShowFiles then
    ShowFiles;
end;

procedure TFileBrowserForm.InitializeTreeview;
begin
  TV.Items.Clear;
  pnlFiles.Visible:=not FilesInTree;
  Splitter1.Visible:=Not FilesInTree;
  if FilesInTree then
    TV.Align:=alClient;
end;

procedure TFileBrowserForm.SetTreeFileMask(AValue: String);

begin
  if aValue=GetTreeFileMask then exit;
  FreeAndNil(FSelectedMask);
  if AValue<>'' then
    FselectedMask:=TMaskList.Create(aValue);
  if TV.Items.Count>0 then
    ShowFiles;
end;

procedure TFileBrowserForm.ShowFiles;

Var
  RootNode: TTreeNode;
  lNode: TTreeNode;
  Dir : String;

begin
  TV.Items.Clear;
  {$IFDEF MSWINDOWS}
  { Add Windows drive letters }
  AddWindowsDriveLetters;
  {$ENDIF}
  Dir:=RootDirectory;
  if Dir='' then
    Dir:=PathDelim;
  if Assigned(FRoot) then
    FreeAndNil(FRoot);
  FRoot:=TDirectoryEntry.Create(Nil,ExcludeTrailingPathDelimiter(Dir));
  { Remove the path delimiter unless this is root. }
  if (Dir<>PathDelim) and (Dir[length(Dir)] = PathDelim) then
    SetLength(Dir, length(Dir)-1);
  { Find or Create the root node and add it to the Tree View. }
  RootNode := TV.Items.FindTopLvlNode(Dir + PathDelim);
  if RootNode = nil then
    RootNode := AddNode(Nil,FRoot);

  { Add the Subdirectories to Root nodes }
  lNode := TV.Items.GetFirstNode;
  while lNode <> nil do
  begin
    AddEntries(lNode);
    lNode := lNode.GetNextSibling;
  end;

  { Set the original root node as the selected node. }
  TV.Selected := RootNode;
  RootNode.Expand(False);
end;

procedure TFileBrowserForm.ConfigNode(aNode : TTreeNode; aEntry : TFileSystemEntry);

var
  Idx : Integer;

begin
  aNode.Data:=aEntry;
  Case aEntry.EntryType of
    etDirectory : Idx:=0;
    etFile : Idx:=1;
    etSymlink : Idx:=2;
  end;
  aNode.ImageIndex:=Idx;
  aNode.SelectedIndex:=Idx;
  aNode.HasChildren:=aEntry.HasEntries(ShowHidden);
end;

function TFileBrowserForm.AddNode(aSibling : TTreeNode; aEntry : TFileSystemEntry) : TTreeNode;

begin
  Result:=TV.Items.Add(nil, aEntry.Name);
  ConfigNode(Result,aEntry);
end;

function TFileBrowserForm.AddChildNode(aParent: TTreeNode; aEntry: TFileSystemEntry): TTreeNode;
begin
  Result:=TV.Items.AddChild(aParent, aEntry.Name);
  ConfigNode(Result,aEntry);
end;

{$IFDEF MSWINDOWS}
procedure TFileBrowserForm.AddWindowsDriveLetters;
const
  MAX_DRIVES = 25;
var
  n: integer;
  drvs: string;
  DriveEntry : TDirectoryEntry;

begin
  // making drive list, skipping drives A: and B: and Removable Devices without media
  n := 2;
  while n <= MAX_DRIVES do
  begin
    drvs := chr(n + Ord('A')) + ':\';
    if (Windows.GetDriveType(PChar(drvs)) <> 1) and
    (GetDiskFreeSpaceEx(PChar(drvs), nil, nil, nil)) then
      begin
      DriveEntry:=TDirectoryEntry.Create(Nil,drvs);
      AddNode(Nil,DriveEntry);
      end;
    Inc(n);
  end;
end;
{$ENDIF}

function TFileBrowserForm.NodeToEntry(aNode : TTreeNode) : TFileSystemEntry;

begin
  Result:=Nil;
  if Assigned(aNode) then
    Result:=TFileSystemEntry(aNode.Data);
end;

procedure TFileBrowserForm.SetCurrentFile(AValue: string);

var
  Dir : String;
  Node : TTreeNode;

begin
  if FilesInTree then
    begin
    Node:=FindNode(aValue);
    if Assigned(Node) then
      TV.Selected:=Node;
    end
  else
    begin
    Dir:=ExtractFilePath(aValue);
    CurrentDirectory:=Dir;
    FileListBox.Directory:=Dir;
    FileListBox.FileName:=ExtractFileName(aValue);
    end
end;

function TFileBrowserForm.SelectedDir: string;

var
  Entry : TFileSystemEntry;

begin
  Result := '';
  Entry:=NodeToEntry(TV.Selected);
  if Not Assigned(Entry) then
    exit;
  if Entry.EntryType<>etDirectory then
    Entry:=Entry.Parent;
  if Not Assigned(Entry) then
    exit;
  Result := Entry.AbsolutePath;
end;

end.

