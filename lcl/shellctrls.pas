{
 /***************************************************************************
                                   ShellCtrls.pas
                                   ------------


 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit ShellCtrls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Graphics,
  ComCtrls, FileUtil;

type

  { TObjectTypes }

  TObjectType = (otFolders, otNonFolders, otHidden);

  TObjectTypes = set of TObjectType;

  { Forward declaration of the classes }

  TCustomShellTreeView = class;
  TCustomShellListView = class;

  { TCustomShellTreeView }

  TCustomShellTreeView = class(TCustomTreeView)
  private
    FObjectTypes: TObjectTypes;
    FShellListView: TCustomShellListView;
    { Setters and getters }
    procedure SetShellListView(const Value: TCustomShellListView);
    { Other internal methods }
    procedure HandleOnExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
    procedure HandleSelectionChanged(Sender: TObject);
  protected
    { Other methods specific to Lazarus }
    function  PopulateTreeNodeWithFiles(
      ANode: TTreeNode; ANodePath: string): Boolean;
    procedure PopulateWithBaseFiles;
  public
    { Basic methods }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Methods specific to Lazarus - useful for other classes }
    class function  GetBasePath: string;
    class procedure GetFilesInDir(const ABaseDir: string;
      AMask: string; AObjectTypes: TObjectTypes; AResult: TStrings);
    { Other methods specific to Lazarus }
    function  GetPathFromNode(ANode: TTreeNode): string;

    { Properties }
    property ObjectTypes: TObjectTypes read FObjectTypes write FObjectTypes;
    property ShellListView: TCustomShellListView read FShellListView write SetShellListView;
  end;

  { TShellTreeView }

  TShellTreeView = class(TCustomShellTreeView)
  published
    { TCustomTreeView properties }
    property Align;
    property Anchors;
    property AutoExpand;
    property BorderSpacing;
    //property BiDiMode;
    property BackgroundColor;
    property BorderStyle;
    property BorderWidth;
    property Color;
    property Constraints;
    property Enabled;
    property ExpandSignType;
    property Font;
    //property ParentBiDiMode;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property RightClickSelect;
    property RowSelect;
    property ScrollBars;
    property SelectionColor;
    property ShowButtons;
    property ShowHint;
    property ShowLines;
    property ShowRoot;
    property TabOrder;
    property TabStop default True;
    property Tag;
    property ToolTips;
    property Visible;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnSelectionChanged;
    property OnShowHint;
    property OnUTF8KeyPress;
    property Options;
    property TreeLineColor;
    property TreeLinePenStyle;
    property ExpandSignColor;
    { TCustomShellTreeView properties }
    property ObjectTypes;
    property ShellListView;
  end;

  { TCustomShellListView }

  TCustomShellListView = class(TCustomListView)
  private
    FMask: string;
    FObjectTypes: TObjectTypes;
    FRoot: string;
    FShellTreeView: TCustomShellTreeView;
    { Setters and getters }
    procedure SetMask(const AValue: string);
    procedure SetShellTreeView(const Value: TCustomShellTreeView);
    procedure SetRoot(const Value: string);
    { Other internal methods }
    procedure HandleResize(Sender: TObject);
  protected
    { Methods specific to Lazarus }
    procedure PopulateWithRoot();
  public
    { Basic methods }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Methods specific to Lazarus }
    function GetPathFromItem(ANode: TListItem): string;
    { Properties }
    property Mask: string read FMask write SetMask; // Can be used to conect to other controls
    property ObjectTypes: TObjectTypes read FObjectTypes write FObjectTypes;
    property Root: string read FRoot write SetRoot;
    property ShellTreeView: TCustomShellTreeView read FShellTreeView write SetShellTreeView;
  end;

  { TShellListView }

  TShellListView = class(TCustomShellListView)
  published
    { TCustomListView properties
      The same as TListView excluding data properties }
    property Align;
    property Anchors;
    property BorderSpacing;
    property BorderStyle;
    property BorderWidth;
//    property Checkboxes;
    property Color default clWindow;
//    property Columns;
//    property ColumnClick;
    property Constraints;
    property DragCursor;
    property DragMode;
//    property DefaultItemHeight;
//    property DropTarget;
    property Enabled;
//    property FlatScrollBars;
    property Font;
//    property FullDrag;
//    property GridLines;
    property HideSelection;
//    property HotTrack;
//    property HotTrackStyles;
//    property HoverTime;
//    property Items;
    property LargeImages;
    property MultiSelect;
//    property OwnerData;
//    property OwnerDraw;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property RowSelect;
    property ScrollBars;
    property ShowColumnHeaders;
    property ShowHint;
//    property ShowWorkAreas;
    property SmallImages;
    property SortColumn;
    property SortType;
    property StateImages;
    property TabStop;
    property TabOrder;
    property ToolTips;
    property Visible;
    property ViewStyle;
//    property OnAdvancedCustomDraw;
//    property OnAdvancedCustomDrawItem;
//    property OnAdvancedCustomDrawSubItem;
    property OnChange;
    property OnClick;
    property OnColumnClick;
    property OnCompare;
    property OnContextPopup;
//    property OnCustomDraw;
//    property OnCustomDrawItem;
//    property OnCustomDrawSubItem;
    property OnDblClick;
    property OnDeletion;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnSelectItem;
    property OnStartDrag;
    property OnUTF8KeyPress;
    { TCustomShellListView properties }
    property ObjectTypes;
    property Root;
    property ShellTreeView;
  end;

procedure Register;

implementation

{$ifdef windows}
uses Windows;
{$endif}

{
uses ShlObj;

//  $I shellctrlswin32.inc

procedure PopulateTreeViewWithShell(ATreeView: TCustomShellTreeView);
var
  ShellFolder: IShellFolder = nil;
  Win32ObjectTypes: Integer;
//  pidl: LPITEMIDLIST;
  pidlParent: LPITEMIDLIST;
begin
  SHGetSpecialFolderLocation(0, CSIDL_DESKTOP, @pidl);

  SHGetDesktopFolder(ShellFolder);

  if ShellFolder = nil then Exit;

  // Converts the control data into Windows constants

  Win32ObjectTypes := 0;

  if otFolders in ATreeView.ObjectTypes then
    Win32ObjectTypes := Win32ObjectTypes or SHCONTF_FOLDERS;

  if otNonFolders in ATreeView.ObjectTypes then
    Win32ObjectTypes := Win32ObjectTypes or SHCONTF_NONFOLDERS;

  if otHidden in ATreeView.ObjectTypes then
    Win32ObjectTypes := Win32ObjectTypes or SHCONTF_INCLUDEHIDDEN;

  // Now gets the name of the desktop folder
}

{ TCustomShellTreeView }

procedure TCustomShellTreeView.SetShellListView(
  const Value: TCustomShellListView);
begin
  FShellListView := Value;

  // Update the pair, it will then update itself
  // in the setter of this property
  // Updates only if necessary to avoid circular calls of the setters
  if Value.ShellTreeView <> Self then
    Value.ShellTreeView := Self;
end;

procedure TCustomShellTreeView.HandleOnExpanding(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: Boolean);
begin
  Node.DeleteChildren;
  AllowExpansion := PopulateTreeNodeWithFiles(Node, GetPathFromNode(Node));
end;

procedure TCustomShellTreeView.HandleSelectionChanged(Sender: TObject);
begin
  if Assigned(FShellListView) then
  begin
    FShellListView.Root := GetPathFromNode(Selected);
    FShellListView.Refresh; // Repaint
  end;
end;

constructor TCustomShellTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Initial property values

  ObjectTypes:= [otFolders];

  // Necessary event handlers

  OnExpanding := @HandleOnExpanding;
  OnSelectionChanged := @HandleSelectionChanged;

  // Populates the base dirs

  PopulateWithBaseFiles();
end;

destructor TCustomShellTreeView.Destroy;
begin
  inherited Destroy;
end;

{ Helper routine.
  Finds all files/directories directly inside a directory.
  Does not recurse inside subdirectories. }
class procedure TCustomShellTreeView.GetFilesInDir(const ABaseDir: string;
  AMask: string; AObjectTypes: TObjectTypes; AResult: TStrings);
var
  DirInfo: TSearchRec;
  FindResult: Integer;
  IsDirectory, IsValidDirectory, IsHidden, AddFile: Boolean;
  ObjectData: TObject;
  SearchStr: string;
  MaskStr: string;
begin
  if Trim(AMask) = '' then MaskStr := AllFilesMask
  else MaskStr := AMask;

  SearchStr := IncludeTrailingPathDelimiter(ABaseDir) + MaskStr;

  FindResult := FindFirst(SearchStr, faAnyFile, DirInfo);

  while FindResult = 0 do
  begin
    Application.ProcessMessages;

    IsDirectory := (DirInfo.Attr and FaDirectory = FaDirectory);

    IsValidDirectory := (DirInfo.Name <> '.') and (DirInfo.Name <> '..');

    IsHidden := (DirInfo.Attr and faHidden = faHidden);

    // First check if we show hidden files
    if IsHidden then AddFile := (otHidden in AObjectTypes)
    else AddFile := True;

    // If it is a directory, check if it is a valid one
    if IsDirectory then
      AddFile := AddFile and ((otFolders in AObjectTypes) and IsValidDirectory)
    else
      AddFile := AddFile and (otNonFolders in AObjectTypes);

    // Mark if it is a directory (ObjectData <> nil)
    if IsDirectory then ObjectData := AResult
    else ObjectData := nil;

    // AddFile identifies if the file is valid or not
    if AddFile then AResult.AddObject(DirInfo.Name, ObjectData);

    FindResult := FindNext(DirInfo);
  end;

  SysUtils.FindClose(DirInfo);
end;

class function TCustomShellTreeView.GetBasePath: string;
begin
  {$if defined(windows) and not defined(wince)}
  Result := '';
  {$endif}
  {$ifdef wince}
  Result := '\';
  {$endif}
  {$ifdef unix}
  Result := '/';
  {$endif}
end;

{ Returns true if at least one item was added, false otherwise }
function TCustomShellTreeView.PopulateTreeNodeWithFiles(
  ANode: TTreeNode; ANodePath: string): Boolean;
var
  i: Integer;
  Files: TStringList;
  NewNode: TTreeNode;
begin
  Files := TStringList.Create;
  try
    GetFilesInDir(ANodePath, AllFilesMask, FObjectTypes, Files);

    Result := Files.Count > 0;

    for i := 0 to Files.Count - 1 do
    begin
      NewNode := Items.AddChildObject(ANode, Files.Strings[i], nil); //@Files.Strings[i]);
      NewNode.HasChildren := Files.Objects[i] <> nil; // This marks if the node is a directory
    end;
  finally
    Files.Free;
  end;
end;

procedure TCustomShellTreeView.PopulateWithBaseFiles;
  {$if defined(windows) and not defined(wince)}
const
  DRIVE_UNKNOWN = 0;
  DRIVE_NO_ROOT_DIR = 1;
  DRIVE_REMOVABLE = 2;
  DRIVE_FIXED = 3;
  DRIVE_REMOTE = 4;
  DRIVE_CDROM = 5;
  DRIVE_RAMDISK = 6;
var
  r: LongWord;
  Drives: array[0..128] of char;
  pDrive: PChar;
  NewNode: TTreeNode;
begin
  // avoids crashes in the IDE by not populating during design
  if (csDesigning in ComponentState) then Exit;

  r := GetLogicalDriveStrings(SizeOf(Drives), Drives);
  if r = 0 then Exit;
  if r > SizeOf(Drives) then Exit;
//    raise Exception.Create(SysErrorMessage(ERROR_OUTOFMEMORY));

  pDrive := Drives;
  while pDrive^ <> #0 do
  begin
//    r := GetDriveType(pDrive);

    NewNode := Items.AddChildObject(nil, pDrive, pDrive);
    NewNode.HasChildren := True;

    Inc(pDrive, 4);
  end;
end;
  {$else}
begin
  // avoids crashes in the IDE by not populating during design
  if (csDesigning in ComponentState) then Exit;

  PopulateTreeNodeWithFiles(nil, GetBasePath());
end;
  {$endif}

function TCustomShellTreeView.GetPathFromNode(ANode: TTreeNode): string;
var
  rootDir : String;
begin
  // If nothing is selected, then the base is selected
  if ANode = nil then Exit(GetBasePath());

  // In the future use ANode.Data instead of ANode.Text
  rootDir := PChar(ANode.Text);
  while (ANode.Parent <> nil)do
  begin
    ANode := ANode.Parent;
    if (PChar(ANode.Text) <> PathDelim) then
      rootDir := PChar(ANode.Text) + PathDelim + rootDir
    else
      rootDir := PChar(ANode.Text) + rootDir;
  end;
  // Check, maybe the base path won't be necessary in the future
  // if the base directory is added to the items list
  Result := GetBasePath + rootDir;
end;

{ TCustomShellListView }

procedure TCustomShellListView.SetShellTreeView(
  const Value: TCustomShellTreeView);
begin
  if FShellTreeView <> Value then
  begin
    FShellTreeView := Value;

    Clear;

    if Value <> nil then
    begin
      FRoot := Value.GetPathFromNode(Value.Selected);
      PopulateWithRoot();
    end;
  end;

  // Also update the pair, but only if necessary to avoid circular calls of the setters
  if Value.ShellListView <> Self then Value.ShellListView := Self;
end;

procedure TCustomShellListView.SetMask(const AValue: string);
begin
  if AValue <> FMask then
  begin
    FMask := AValue;
    Clear;
    Items.Clear;
    PopulateWithRoot();
  end;
end;

procedure TCustomShellListView.SetRoot(const Value: string);
begin
  if FRoot <> Value then
  begin
    FRoot := Value;
    Clear;
    Items.Clear;
    PopulateWithRoot();
  end;
end;

procedure TCustomShellListView.HandleResize(Sender: TObject);
begin
  {$ifdef DEBUG_SHELLCTRLS}
    WriteLn(':>TCustomShellListView.HandleResize');
  {$endif}

  // The correct check is with count,
  // if Column[0] <> nil then
  // will raise an exception
  if Self.Columns.Count < 3 then Exit;

  Column[0].Width := (70 * Width) div 100;
  Column[1].Width := (15 * Width) div 100;
  Column[2].Width := (15 * Width) div 100;

  {$ifdef DEBUG_SHELLCTRLS}
    WriteLn(':<TCustomShellListView.HandleResize C0.Width=',
     Column[0].Width, ' C1.Width=', Column[1].Width,
     ' C2.Width=', Column[2].Width);
  {$endif}
end;

constructor TCustomShellListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Initial property values
  ViewStyle := vsReport;
  ObjectTypes := [otNonFolders];

  Self.Columns.Add;
  Self.Columns.Add;
  Self.Columns.Add;
  Self.Column[0].Caption := 'Name';
  Self.Column[1].Caption := 'Size';
  Self.Column[2].Caption := 'Type';
  // Initial sizes, necessary under Windows CE
  HandleResize(Self);

  // Internal event handlers
  OnResize := @HandleResize;
end;

destructor TCustomShellListView.Destroy;
begin

  inherited Destroy;
end;

procedure TCustomShellListView.PopulateWithRoot();
var
  i: Integer;
  Files: TStringList;
  NewItem: TListItem;
  CurFileName, CurFilePath: string;
  CurFileSize: Int64;
begin
  // avoids crashes in the IDE by not populating during design
  if (csDesigning in ComponentState) then Exit;

  // Check inputs
  if Trim(FRoot) = '' then Exit;

  Files := TStringList.Create;
  try
    TCustomShellTreeView.GetFilesInDir(FRoot, FMask, FObjectTypes, Files);

    for i := 0 to Files.Count - 1 do
    begin
      NewItem := Items.Add;
      CurFileName := Files.Strings[i];
      CurFilePath := IncludeTrailingPathDelimiter(FRoot) + CurFileName;
      // First column - Name
      NewItem.Caption := CurFileName;
      // Second column - Size
      CurFileSize := FileSize(CurFilePath); // in Bytes
      if CurFileSize < 1024 then
        NewItem.SubItems.Add(IntToStr(CurFileSize) + ' bytes')
      else if CurFileSize < 1024 * 1024 then
        NewItem.SubItems.Add(IntToStr(CurFileSize div 1024) + ' kB')
      else
        NewItem.SubItems.Add(IntToStr(CurFileSize div (1024 * 1024)) + ' MB');
      // Third column - Type
      NewItem.SubItems.Add(ExtractFileExt(CurFileName));
    end;
  finally
    Files.Free;
  end;
end;

function TCustomShellListView.GetPathFromItem(ANode: TListItem): string;
begin
  Result := IncludeTrailingPathDelimiter(FRoot) + ANode.Caption;
end;

procedure Register;
begin
  RegisterComponents('Misc',[TShellTreeView, TShellListView]);
end;

end.
