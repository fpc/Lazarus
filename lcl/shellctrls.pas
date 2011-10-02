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
  Classes, SysUtils, Forms, Graphics, LCLType,
  ComCtrls, FileUtil;

type

  { TObjectTypes }

  TObjectType = (otFolders, otNonFolders, otHidden);

  TObjectTypes = set of TObjectType;

  TFileSortType = (fstNone, fstAlphabet, fstFoldersFirst);

  { Forward declaration of the classes }

  TCustomShellTreeView = class;
  TCustomShellListView = class;

  { TCustomShellTreeView }

  TCustomShellTreeView = class(TCustomTreeView)
  private
    FObjectTypes: TObjectTypes;
    FRoot: string;
    FShellListView: TCustomShellListView;
    FFileSortType: TFileSortType;
    { Setters and getters }
    procedure SetFileSortType(const AValue: TFileSortType);
    procedure SetRoot(const AValue: string);
    procedure SetShellListView(const Value: TCustomShellListView);
  protected
    { Other methods specific to Lazarus }
    function  PopulateTreeNodeWithFiles(
      ANode: TTreeNode; ANodePath: string): Boolean;
    procedure PopulateWithBaseFiles;
    procedure DoSelectionChanged; override;
    function CanExpand(Node: TTreeNode): Boolean; override;
  public
    { Basic methods }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Methods specific to Lazarus - useful for other classes }
    class function  GetBasePath: string;
    class procedure GetFilesInDir(const ABaseDir: string;
      AMask: string; AObjectTypes: TObjectTypes; AResult: TStrings; AFileSortType: TFileSortType = fstNone);
    { Other methods specific to Lazarus }
    function  GetPathFromNode(ANode: TTreeNode): string;
    function  GetSelectedNodePath(): string;

    { Properties }
    property ObjectTypes: TObjectTypes read FObjectTypes write FObjectTypes;
    property ShellListView: TCustomShellListView read FShellListView write SetShellListView;
    property FileSortType: TFileSortType read FFileSortType write SetFileSortType;
    property Root: string read FRoot write SetRoot;

    { Protected properties which users may want to access, see bug 15374 }
    property Items;
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
    property FileSortType;
    property HideSelection;
    property Indent;
    //property ParentBiDiMode;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property RightClickSelect;
    property Root;
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
  protected
    { Methods specific to Lazarus }
    procedure PopulateWithRoot();
    procedure Resize; override;
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
    { Protected properties which users may want to access, see bug 15374 }
    property Items;
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
var
  Tmp: TCustomShellListView;
begin
  if FShellListView = Value then Exit;

  if Assigned(FShellListView) then
  begin
    Tmp := FShellListView;
    FShellListView := nil;
    Tmp.ShellTreeView := nil;
  end;

  FShellListView := Value;

  // Update the pair, it will then update itself
  // in the setter of this property
  // Updates only if necessary to avoid circular calls of the setters
  if Assigned(Value) and (Value.ShellTreeView <> Self) then
    Value.ShellTreeView := Self;
end;

procedure TCustomShellTreeView.SetRoot(const AValue: string);
begin
  if FRoot=AValue then exit;
  FRoot:=AValue;
  Items.Clear;
  if FRoot = '' then
    PopulateWithBaseFiles()
  else
    PopulateTreeNodeWithFiles(nil, AValue);
  if Assigned(ShellListView) then
    ShellListView.Root := FRoot;
end;

// ToDo: Optimize, now the tree is populated in constructor, SetRoot and SetFileSortType.
// For some reason it does not show in performance really.
procedure TCustomShellTreeView.SetFileSortType(const AValue: TFileSortType);
begin
  if FFileSortType=AValue then exit;
  FFileSortType:=AValue;
  Items.Clear;
  if FRoot = '' then
    PopulateWithBaseFiles()
  else
    PopulateTreeNodeWithFiles(nil, FRoot);
end;

function TCustomShellTreeView.CanExpand(Node: TTreeNode): Boolean;
var
  OldAutoExpand: Boolean;
begin
  Result:=inherited CanExpand(Node);
  if not Result then exit;
  OldAutoExpand:=AutoExpand;
  AutoExpand:=False;
  Node.DeleteChildren;
  Result := PopulateTreeNodeWithFiles(Node, GetPathFromNode(Node));
  AutoExpand:=OldAutoExpand;
end;

constructor TCustomShellTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Initial property values

  ObjectTypes:= [otFolders];

  // Populates the base dirs

  PopulateWithBaseFiles();
end;

destructor TCustomShellTreeView.Destroy;
begin
  ShellListView := nil;
  inherited Destroy;
end;

type
  { TFileItem }
  TFileItem = class(TObject)
    Name: string;
    isFolder: Boolean;
    //more data to sort by size, date... etc
    constructor Create(const DirInfo: TSearchRec);
  end;

{ TFileItem }

constructor TFileItem.Create(const DirInfo:TSearchRec);
begin
  Name:=DirInfo.Name;
  isFolder:=DirInfo.Attr and FaDirectory > 0;
end;

function FilesSortAlphabet(p1, p2: Pointer): Integer;
var
  f1, f2: TFileItem;
begin
  f1:=TFileItem(p1);
  f2:=TFileItem(p2);
  Result:=CompareText(f1.Name, f2.Name);
end;

function FilesSortFoldersFirst(p1,p2: Pointer): Integer;
var
  f1, f2: TFileItem;
begin
  f1:=TFileItem(p1);
  f2:=TFileItem(p2);
  if f1.isFolder=f2.isFolder then
    Result:=FilesSortAlphabet(p1,p2)
  else begin
    if f1.isFolder then Result:=-1
    else Result:=1;
  end;

end;

{ Helper routine.
  Finds all files/directories directly inside a directory.
  Does not recurse inside subdirectories.

  AMask may contain multiple file masks separated by ;
  Don't add a final ; after the last mask.
}
class procedure TCustomShellTreeView.GetFilesInDir(const ABaseDir: string;
  AMask: string; AObjectTypes: TObjectTypes; AResult: TStrings; AFileSortType: TFileSortType);
var
  DirInfo: TSearchRec;
  FindResult: Integer;
  IsDirectory, IsValidDirectory, IsHidden, AddFile: Boolean;
  ObjectData: TObject;
  SearchStr: string;
  MaskStr: string;
  Files: TList;
  FileItem: TFileItem;
  i: Integer;
  MaskStrings: TStringList;
  {$if defined(windows) and not defined(wince)}
  ErrMode : LongWord;
  {$endif}
begin
  {$if defined(windows) and not defined(wince)}
  // disables the error dialog, while enumerating not-available drives
  // for example listing A: path, without diskette present.
  // WARNING: Since Application.ProcessMessages is called, it might effect some operations!
  ErrMode:=SetErrorMode(SEM_FAILCRITICALERRORS or SEM_NOALIGNMENTFAULTEXCEPT or SEM_NOGPFAULTERRORBOX or SEM_NOOPENFILEERRORBOX);
  try
  {$endif}

  if Trim(AMask) = '' then MaskStr := AllFilesMask
  else MaskStr := AMask;

  // The string list implements support for multiple masks separated
  // by semi-comma ";"
  MaskStrings := TStringList.Create;
  try
    MaskStrings.Delimiter := ';';
    MaskStrings.DelimitedText := MaskStr;

    if AFileSortType=fstNone then Files:=nil
    else Files:=TList.Create;

    for i := 0 to MaskStrings.Count - 1 do
    begin
      if MaskStrings.IndexOf(MaskStrings[i]) < i then Continue; // From patch from bug 17761: TShellListView Mask: duplicated items if mask is " *.ext;*.ext "
      SearchStr := IncludeTrailingPathDelimiter(ABaseDir) + MaskStrings.Strings[i];

      FindResult := FindFirstUTF8(SearchStr, faAnyFile, DirInfo);

      while FindResult = 0 do
      begin
        Application.ProcessMessages;

        IsDirectory := (DirInfo.Attr and FaDirectory = FaDirectory);

        IsValidDirectory := (DirInfo.Name <> '.') and (DirInfo.Name <> '..');

        IsHidden := (DirInfo.Attr and faHidden = faHidden);
        {$IFDEF Unix}
        if (DirInfo.Name<>'') and (DirInfo.Name[1]='.') then
          IsHidden:=true;
        {$ENDIF}

        // First check if we show hidden files
        if IsHidden then AddFile := (otHidden in AObjectTypes)
        else AddFile := True;

        // If it is a directory, check if it is a valid one
        if IsDirectory then
          AddFile := AddFile and ((otFolders in AObjectTypes) and IsValidDirectory)
        else
          AddFile := AddFile and (otNonFolders in AObjectTypes);

        // AddFile identifies if the file is valid or not
        if AddFile then
        begin
          if not Assigned(Files) then begin
            // Mark if it is a directory (ObjectData <> nil)
            if IsDirectory then ObjectData := AResult
            else ObjectData := nil;
            if AResult.IndexOf(DirInfo.Name) < 0 then // From patch from bug 17761: TShellListView Mask: duplicated items if mask is " *.ext;*.ext "
              AResult.AddObject(DirInfo.Name, ObjectData)
          end else
            Files.Add ( TFileItem.Create(DirInfo));
        end;

        FindResult := FindNextUTF8(DirInfo);
      end;

      FindCloseUTF8(DirInfo);
    end;
  finally
    MaskStrings.Free;
  end;

  if Assigned(Files) then begin
    Objectdata:=AResult;

    case AFileSortType of
      fstAlphabet:     Files.Sort(@FilesSortAlphabet);
      fstFoldersFirst: Files.Sort(@FilesSortFoldersFirst);
    end;

    for i:=0 to Files.Count-1 do begin
      FileItem:=TFileItem(Files[i]);
      if (i > 0) and (TFileItem(Files[i]).Name = TFileItem(Files[i - 1]).Name) then
        Continue; // cause Files is sorted // From patch from bug 17761: TShellListView Mask: duplicated items if mask is " *.ext;*.ext "
      if FileItem.isFolder then
        AResult.AddObject(FileItem.Name, ObjectData)
      else
        AResult.AddObject(FileItem.Name, nil);
      FileItem.Free;
    end;
    Files.Free;
  end;

  {$if defined(windows) and not defined(wince)}
  finally
     SetErrorMode(ErrMode);
  end;
  {$endif}
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
  // avoids crashes in the IDE by not populating during design
  if (csDesigning in ComponentState) then Exit;

  Files := TStringList.Create;
  try
    GetFilesInDir(ANodePath, AllFilesMask, FObjectTypes, Files, FFileSortType);

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

    NewNode := Items.AddChildObject(nil, ExcludeTrailingBackslash(pDrive), pDrive);
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

procedure TCustomShellTreeView.DoSelectionChanged;
begin
  inherited DoSelectionChanged;
  if Assigned(FShellListView) then
    FShellListView.Root := GetPathFromNode(Selected);
end;

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
  // Check if the base directory should be taken into account
  if FRoot = '' then
    Result := GetBasePath + rootDir
  else
    Result := IncludeTrailingPathDelimiter(FRoot) + rootDir;
end;

function TCustomShellTreeView.GetSelectedNodePath(): string;
begin
  Result := GetPathFromNode(Selected);
end;

{ TCustomShellListView }

procedure TCustomShellListView.SetShellTreeView(
  const Value: TCustomShellTreeView);
var
  Tmp: TCustomShellTreeView;
begin
  if FShellTreeView = Value then Exit;
  if FShellTreeView <> nil then
  begin
    Tmp := FShellTreeView;
    FShellTreeView := nil;
    Tmp.ShellListView := nil;
  end;

  FShellTreeView := Value;

  if not (csDestroying in ComponentState) then
    Clear;

  if Value <> nil then
  begin
    FRoot := Value.GetPathFromNode(Value.Selected);
    PopulateWithRoot();

    // Also update the pair, but only if necessary to avoid circular calls of the setters
    if Value.ShellListView <> Self then Value.ShellListView := Self;
  end;

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
  Resize;
end;

destructor TCustomShellListView.Destroy;
begin
  ShellTreeView := nil;
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
      // The raw size in bytes is stored in the data part of the item
      CurFileSize := FileSize(CurFilePath); // in Bytes
      NewItem.Data := Pointer(PtrInt(CurFileSize));
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

procedure TCustomShellListView.Resize;
begin
  inherited Resize;
  {$ifdef DEBUG_SHELLCTRLS}
    WriteLn(':>TCustomShellListView.HandleResize');
  {$endif}

  // The correct check is with count,
  // if Column[0] <> nil then
  // will raise an exception
  if Self.Columns.Count < 3 then Exit;

  // If the space available is small,
  // alloc a larger percentage to the secondary
  // fields
  if Width < 400 then
  begin
    Column[0].Width := (50 * Width) div 100;
    Column[1].Width := (25 * Width) div 100;
    Column[2].Width := (25 * Width) div 100;
  end
  else
  begin
    Column[0].Width := (70 * Width) div 100;
    Column[1].Width := (15 * Width) div 100;
    Column[2].Width := (15 * Width) div 100;
  end;

  {$ifdef DEBUG_SHELLCTRLS}
    WriteLn(':<TCustomShellListView.HandleResize C0.Width=',
     Column[0].Width, ' C1.Width=', Column[1].Width,
     ' C2.Width=', Column[2].Width);
  {$endif}
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
