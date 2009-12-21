unit frmFileBrowser;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  FileUtil,
  LResources,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  EditBtn,
  FileCtrl,
  ComCtrls,
  StdCtrls,
  ExtCtrls;

type
  TOpenFileEvent = procedure(Sender: TObject; const AFileName: string) of object;

  { TFileBrowserForm }

  TFileBrowserForm = class(TForm)
    btnConfigure: TButton;
    btnReload: TButton;
    cbHidden: TCheckBox;
    FileListBox: TFileListBox;
    FilterComboBox: TFilterComboBox;
    Panel1: TPanel;
    Splitter1: TSplitter;
    TV: TTreeView;
    procedure btnConfigureClick(Sender: TObject);
    procedure btnReloadClick(Sender: TObject);
    procedure cbHiddenChange(Sender: TObject);
    procedure FileListBoxDblClick(Sender: TObject);
    procedure FilterComboBoxChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TVExpanded(Sender: TObject; Node: TTreeNode);
    procedure TVSelectionChanged(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    FOnConfigure: TNotifyEvent;
    FOnOpenFile: TOpenFileEvent;
    FOnSaveLayout: TNotifyEvent;
    FOnSelectDir: TNotifyEvent;
    FRootDir: string;
    FDir: string;
    FShowHidden: Boolean;
    FOnLoadLayout: TNotifyEvent;
    procedure AddDirectories(Node: TTreeNode; Dir: string);
    function GetAbsolutePath(Node: TTreeNode): string;
    procedure SetDir(const Value: string);
    procedure SetRootDir(const Value: string);
    procedure InitializeTreeview;
    {$IFDEF MSWINDOWS}
    procedure AddWindowsDriveLetters;
    {$ENDIF}
  public
    { return the selected directory }
    function SelectedDir: string;
    { The selected/opened directory }
    property Directory: string read FDir write SetDir;
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
    { OnLoadLayout }
    property OnSaveLayout: TNotifyEvent read FOnSaveLayout write FOnSaveLayout;
    property OnLoadLayout: TNotifyEvent read FOnLoadLayout write FOnLoadLayout;
  end;

var
  FileBrowserForm: TFileBrowserForm;


implementation

{$IFDEF MSWINDOWS}
uses
  Windows;
{$ENDIF}

const
  cFilter = 'All Files (' + AllFilesMask + ')|' + AllFilesMask +
            '|Source(*.pas;*.pp)|*.pas;*.pp' +
            '|Projectfiles(*.pas;*.pp;*.inc;*.lfm;*.lpr;*.lrs;*.lpi;*.lpk)|' +
            '*.pas;*.pp;*.inc;*.lfm;*.lpr;*.lrs;*.lpi;*.lpk;|';


{function HasSubDirs returns True if the directory passed has subdirectories}
function HasSubDirs(const Dir: string; AShowHidden: Boolean): Boolean;
var
  FileInfo: TSearchRec;
  FCurrentDir: string;
begin
  //Assume No
  Result := False;
  if Dir <> '' then
  begin
    FCurrentDir := AppendPathDelim(Dir);
    FCurrentDir := FCurrentDir + GetAllFilesMask;
    try
      if SysUtils.FindFirst(FCurrentDir, faAnyFile or $00000080, FileInfo) = 0 then
        repeat
          if FileInfo.Name = '' then
            Continue;

            // check if special file
          if ((FileInfo.Name = '.') or (FileInfo.Name = '..')) or
            // unix dot directories (aka hidden directories)
            ((FileInfo.Name[1] in ['.']) and AShowHidden) or
            // check Hidden attribute
            (((faHidden and FileInfo.Attr) > 0) and AShowHidden) then
            Continue;

          Result := ((faDirectory and FileInfo.Attr) > 0);

          //We found at least one non special dir, that's all we need.
          if Result then
            break;
        until SysUtils.FindNext(FileInfo) <> 0;
    finally
      SysUtils.FindClose(FileInfo);
    end;
  end;
end;


{ TFileBrowserForm }

procedure TFileBrowserForm.TVExpanded(Sender: TObject; Node: TTreeNode);
begin
  if Node.Count = 0 then
    AddDirectories(Node, GetAbsolutePath(Node));
end;

procedure TFileBrowserForm.TVSelectionChanged(Sender: TObject);
begin
  FileListBox.Directory := ChompPathDelim(SelectedDir);
  if Assigned(OnSelectDir) then
    OnselectDir(Self);
end;

procedure TFileBrowserForm.FormActivate(Sender: TObject);
begin
  { for some reason this does not work in FormShow }
  TV.MakeSelectionVisible;
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
  // rebuild tree
  TV.Items.Clear;
  InitializeTreeview;
  // restore directory
  Directory := d;
end;

procedure TFileBrowserForm.cbHiddenChange(Sender: TObject);
begin
  ShowHidden := cbHidden.Checked;
  if ShowHidden then
    FileListBox.FileType := FileListBox.FileType + [ftHidden]
  else
    FileListBox.FileType := FileListBox.FileType - [ftHidden];
end;

procedure TFileBrowserForm.FileListBoxDblClick(Sender: TObject);
begin
  if Assigned(FOnOpenFile) then
    FOnOpenFile(Self, FileListBox.FileName);
end;

procedure TFileBrowserForm.FilterComboBoxChange(Sender: TObject);
begin
  FileListBox.Mask := FilterComboBox.Mask;
end;

procedure TFileBrowserForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Assigned(FOnSaveLayout) then
    FOnSaveLayout(Self);
  CloseAction := caFree;
end;

procedure TFileBrowserForm.FormCreate(Sender: TObject);
begin
  FShowHidden := False;
  InitializeTreeview;
  FilterComboBox.Filter := cFilter;
end;

procedure TFileBrowserForm.FormShow(Sender: TObject);
begin
  if Assigned(FOnLoadLayout) then
    FOnLoadLayout(Self);
  if TV.Selected <> nil then
    TV.Selected.Expand(False);
end;

{ Adds Subdirectories to a passed node if they exist }
procedure TFileBrowserForm.AddDirectories(Node: TTreeNode; Dir: string);
var
  FileInfo: TSearchRec;
  NewNode: TTreeNode;
  i: integer;
  FCurrentDir: string;
  //used to sort the directories.
  SortList: TStringList;
begin
  if Dir <> '' then
  begin
    FCurrentDir := Dir;
    FCurrentDir := AppendPathDelim(FCurrentDir);
    i           := length(FCurrentDir);
    FCurrentDir := FCurrentDir + GetAllFilesMask;
    try
      if SysUtils.FindFirst(FCurrentDir, faAnyFile or $00000080, FileInfo) = 0 then
      begin
        try
          SortList        := TStringList.Create;
          SortList.Sorted := True;
          repeat
            // check if special file
            if (FileInfo.Name = '.') or (FileInfo.Name = '..') or (FileInfo.Name = '') then
              Continue;
            // if hidden files or directories must be filtered, we test for
            // dot files, considered hidden under unix type OS's.
            if not FShowHidden then
              if (FileInfo.Name[1] in ['.']) then
                Continue;

            // if this is a directory then add it to the tree.
            if ((faDirectory and FileInfo.Attr) > 0) then
            begin
              //if this is a hidden file and we have not been requested to show
              //hidden files then do not add it to the list.
              if ((faHidden and FileInfo.Attr) > 0) and not FShowHidden then
                continue;

              SortList.Add(FileInfo.Name);
            end;
          until SysUtils.FindNext(FileInfo) <> 0;
          for i := 0 to SortList.Count - 1 do
          begin
            NewNode := TV.Items.AddChild(Node, SortList[i]);
            // if subdirectories then indicate so.
            NewNode.HasChildren := HasSubDirs(AppendPathDelim(Dir) + NewNode.Text, FShowHidden);
          end;
        finally
          SortList.Free;
        end;
      end;  { if FindFirst... }
    finally
      SysUtils.FindClose(FileInfo);
    end;
  end;  { if Dir... }
  if Node.Level = 0 then
    Node.Text := Dir;
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

procedure TFileBrowserForm.SetDir(const Value: string);
var
  StartDir: string;
  Node: TTreeNode;
  i, p: integer;
  SubDir: PChar;
begin
  FDir     := Value;
  StartDir := Value;
  if TV.Items.Count = 0 then
    Exit;
  p := AnsiPos(FRootDir, StartDir);
  if p = 1 then
    Delete(StartDir, P, Length(FRootDir));
  for i := 1 to Length(StartDir) do
    if (StartDir[i] = PathDelim) then
      StartDir[i] := #0;
  SubDir := PChar(StartDir);
  if SubDir[0] = #0 then
    SubDir := @SubDir[1];
  Node := TV.Items.GetFirstNode;
  while SubDir[0] <> #0 do
  begin
    Node := Node.GetFirstChild;
    while (Node <> nil) and (AnsiCompareStr(Node.Text, SubDir) <> 0) do
      Node := Node.GetNextSibling;
    if Node = nil then
      break
    else
      Node.Expand(False);
    SubDir := SubDir + StrLen(SubDir) + 1;
  end;
  TV.Selected := Node;
  TV.MakeSelectionVisible;
end;

procedure TFileBrowserForm.SetRootDir(const Value: string);
var
  RootNode: TTreeNode;
  lNode: TTreeNode;
begin
  { Clear the list }
  TV.Items.Clear;
  FRootDir := Value;

  {$IFDEF MSWINDOWS}
  { Add Windows drive letters }
  AddWindowsDriveLetters;
  {$ENDIF}

  { Remove the path delimiter unless this is root. }
  if FRootDir = '' then
    FRootDir := PathDelim;
  if (FRootDir <> PathDelim) and (FRootDir[length(FRootDir)] = PathDelim) then
    FRootDir := copy(FRootDir, 1, length(FRootDir) - 1);
  { Find or Create the root node and add it to the Tree View. }
  RootNode := TV.Items.FindTopLvlNode(FRootDir + PathDelim);
  if RootNode = nil then
    RootNode := TV.Items.Add(nil, FRootDir);

  { Add the Subdirectories to Root nodes }
  lNode := TV.Items.GetFirstNode;
  while lNode <> nil do
  begin
    AddDirectories(lNode, lNode.Text);
    lNode := lNode.GetNextSibling;
  end;

  { Set the original root node as the selected node. }
  TV.Selected := RootNode;
end;

procedure TFileBrowserForm.InitializeTreeview;
begin
  { I'm not sure what we should set these to. Maybe another Config option? }
  {$IFDEF UNIX}
  RootDirectory := '/';
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  RootDirectory := 'C:\';
  {$ENDIF}
end;

{$IFDEF MSWINDOWS}
procedure TFileBrowserForm.AddWindowsDriveLetters;
const
  MAX_DRIVES = 25;
var
  n: integer;
  drvs: string;
begin
  // making drive list, skipping drives A: and B: and Removable Devices without media
  n := 2;
  while n <= MAX_DRIVES do
  begin
    drvs := chr(n + Ord('A')) + ':\';
    if (Windows.GetDriveType(PChar(drvs)) <> 1) and
    (GetDiskFreeSpaceEx(PChar(drvs), nil, nil, nil)) then
      TV.Items.Add(nil, drvs);
    Inc(n);
  end;
end;
{$ENDIF}

function TFileBrowserForm.SelectedDir: string;
begin
  Result := '';
  if TV.Selected <> nil then
    Result := GetAbsolutePath(TV.Selected);
end;

initialization
  {$I frmfilebrowser.lrs}

end.

