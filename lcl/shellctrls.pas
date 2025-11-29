{
 /***************************************************************************
                                   ShellCtrls.pas
                                   ------------


 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit ShellCtrls;

{$mode objfpc}{$H+}

{.$define debug_shellctrls}

interface

uses
  Classes, SysUtils, Types, Math, AVL_Tree,
  // LCL
  Forms, Graphics, Controls, ComCtrls, LCLStrConsts, LCLProc,
  // LazUtils
  LazFileUtils, LazUTF8, Masks;

{$if defined(Windows) or defined(darwin) or defined(HASAMIGA))}
{$define CaseInsensitiveFilenames}
{$endif}
{$IF defined(CaseInsensitiveFilenames)}
{$DEFINE NotLiteralFilenames}
{$ENDIF}

type

  TObjectType = (otFolders, otNonFolders, otHidden);

  TObjectTypes = set of TObjectType;

  TFileSortType = (fstNone, fstAlphabet, fstFoldersFirst, fstCustom);

  TMaskCaseSensitivity = (mcsPlatformDefault, mcsCaseInsensitive, mcsCaseSensitive);

  TExpandCollapseMode = (
    ecmRefreshedExpanding,  // Clear already existing children before expanding
    ecmKeepChildren,        // Do not clear children of already-expanded, but collapsed nodes
    ecmCollapseAndClear     // Clear children when a node is collapsed
  );

  { Forward declaration of the classes }

  TCustomShellTreeView = class;
  TCustomShellListView = class;

  { TFileItem }

  TFileItem = class(TObject)
  private
    FFileInfo: TSearchRec;
    FBasePath: String;
  public
    //more data to sort by size, date... etc
    isFolder: Boolean;
    constructor Create(const DirInfo: TSearchRec; ABasePath: String);
    property BasePath: String read FBasePath;
    property FileInfo: TSearchRec read FFileInfo write FFileInfo;
  end;

  TFileItemCompareEvent = function(Item1, Item2: TFileItem): integer of object;

  { TCustomShellTreeView }

  TAddItemEvent = procedure(Sender: TObject; const ABasePath: String;
                            const AFileInfo: TSearchRec; var CanAdd: Boolean) of object;

  TCustomShellTreeView = class(TCustomTreeView)
  private
    FObjectTypes: TObjectTypes;
    FRoot: string;
    FShellListView: TCustomShellListView;
    FExpandCollapseMode: TExpandCollapseMode;
    FFileSortType: TFileSortType;
    FInitialRoot: String;
    FUpdateLock: Integer;
    FUseBuiltinIcons: Boolean;
    FOnAddItem: TAddItemEvent;
    FOnSortCompare: TFileItemCompareEvent;
    function CreateRootNode(const APath: String): TTreeNode;
    { Setters and getters }
    function GetPath: string;
    procedure SetFileSortType(const AValue: TFileSortType);
    procedure SetObjectTypes(AValue: TObjectTypes);
    procedure SetOnSortCompare(AValue: TFileItemCompareEvent);
    procedure SetPath(AValue: string);
    procedure SetRoot(const AValue: string);
    procedure SetShellListView(const Value: TCustomShellListView);
    procedure SetUseBuiltinIcons(const AValue: Boolean);
  protected
    class procedure WSRegisterClass; override;
    procedure DoCreateNodeClass(var NewNodeClass: TTreeNodeClass); override;
    procedure Loaded; override;
    function CreateNode: TTreeNode; override;
    { Other methods specific to Lazarus }
    function  PopulateTreeNodeWithFiles(
      ANode: TTreeNode; ANodePath: string): Boolean;
    procedure DoSelectionChanged; override;
    procedure DoAddItem(const ABasePath: String; const AFileInfo: TSearchRec; var CanAdd: Boolean);
    function CanExpand(Node: TTreeNode): Boolean; override;
    procedure Collapse(Node: TTreeNode); override;
    function DrawBuiltInIcon(ANode: TTreeNode; ARect: TRect): TSize; override;
    function ExistsAndIsValid(APath: String): Boolean;
    function GetBuiltinIconSize: TSize; override;
    function NodeHasChildren(Node: TTreeNode): Boolean; override;
    property ExpandCollapseMode: TExpandCollapseMode read FExpandCollapseMode write FExpandCollapseMode default ecmRefreshedExpanding;
  public
    { Basic methods }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Methods specific to Lazarus - useful for other classes }
    class function GetBasePath: string;
    function GetRootPath: string;
    class procedure GetFilesInDir(const ABaseDir: string; AMask: string;
      AObjectTypes: TObjectTypes; AResult: TStrings; AFileSortType: TFileSortType = fstNone;
      ACaseSensitivity: TMaskCaseSensitivity = mcsPlatformDefault);
    { Other methods specific to Lazarus }
    function  GetPathFromNode(ANode: TTreeNode): string;
    procedure PopulateWithBaseFiles;
    procedure Refresh(ANode: TTreeNode); overload;
    procedure UpdateView(AStartDir: String = '');
    property UseBuiltinIcons: Boolean read FUseBuiltinIcons write SetUseBuiltinIcons default true;

    { Properties }
    property ObjectTypes: TObjectTypes read FObjectTypes write SetObjectTypes default [otFolders];
    property ShellListView: TCustomShellListView read FShellListView write SetShellListView;
    property FileSortType: TFileSortType read FFileSortType write SetFileSortType default fstNone;
    property Root: string read FRoot write SetRoot;
    property Path: string read GetPath write SetPath;
    property OnAddItem: TAddItemEvent read FOnAddItem write FOnAddItem;
    property OnSortCompare: TFileItemCompareEvent read FOnSortCompare write SetOnSortCompare;
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
    property BackgroundColor;
    property BorderStyle;
    property BorderWidth;
    property Color;
    property Constraints;
    property Enabled;
    property ExpandCollapseMode;
    property ExpandSignType;
    property Font;
    property FileSortType;
    property HideSelection;
    property HotTrack;
    property Images;
    property Indent;
    property MultiSelectStyle;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly default True;
    property RightClickSelect;
    property Root;
    property RowSelect;
    property ScrollBars;
    property SelectionColor;
    property ShowButtons;
    property ShowHint;
    property ShowLines;
    property ShowRoot;
    property StateImages;
    property TabOrder;
    property TabStop default True;
    property Tag;
    property ToolTips;
    property Visible;
    property Options;
    property TreeLineColor;
    property TreeLinePenStyle;
    property ExpandSignColor;
    { TCustomShellTreeView properties }
    property ObjectTypes;
    property ShellListView;

    property OnAddItem;
    property OnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnCollapsed;
    property OnCollapsing;
    property OnCustomDraw;
    property OnCustomDrawItem;
    property OnDblClick;
    property OnEdited;
    property OnEditing;
    property OnEnter;
    property OnExit;
    property OnExpanded;
    property OnExpanding;
    property OnGetImageIndex;
    property OnGetSelectedIndex;
    property OnHasChildren;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnMouseWheelHorz;
    property OnMouseWheelLeft;
    property OnMouseWheelRight;
    property OnSelectionChanged;
    property OnShowHint;
    property OnSortCompare;
    property OnUTF8KeyPress;
  end;


  { TShellListItem }

  TShellListItem = class(TListItem)
  private
    FFileInfo: TSearchRec;
  public
    function IsFolder: Boolean;
    property FileInfo: TSearchRec read FFileInfo write FFileInfo;
  end;


  { TShellListColumn }

  TSLVColumnID = (cidCustom, cidFileName, cidSize, cidType, cidAttr, cidDateModified);
  TSLVSizeUnits = (suDefault, suBytes, suKB, suMB, suGB);

  TShellListColumn = class(TListColumn)
  private
    FColumnID: TSLVColumnID;
    FCustomID: Integer;
    FFormat: String;
    FSizeUnits: TSLVSizeUnits;
    procedure SetColumnID(const AValue: TSLVColumnID);
    procedure SetCustomID(const AValue: Integer);
    procedure SetFormat(const AValue: String);
    procedure SetSizeUnits(const AValue: TSLVSizeUnits);
  private
    procedure Assign(ASource: TPersistent); override;
  protected
    function GetListView: TCustomShellListView;
    function GetSizeFormat: String;
  published
    property ColumnID: TSLVColumnID read FColumnID write SetColumnID default cidCustom;
    property CustomID: Integer read FCustomID write SetCustomID default 0;
    property Format: String read FFormat write SetFormat;
    property SizeUnits: TSLVSizeUnits read FSizeUnits write SetSizeUnits default suDefault;
  end;

  TShellListColumns = class(TListColumns)
  private
    function GetItem(const AIndex: Integer): TShellListColumn;
    procedure SetItem(const AIndex: Integer; const AValue: TShellListColumn);
  public
    function Add: TShellListColumn;
    property Items[const AIndex: Integer]: TShellListColumn read GetItem write SetItem; default;
  end;


  { TCustomShellListView }

  TSLVSizeFunc = function(ASize: Int64; AFormat: String): String;

  TSLVFileAddedEvent = procedure (Sender: TObject; Item: TListItem) of object;

  TSLVGetCellTextEvent = procedure (Sender: TObject; Item: TShellListItem;
    AColumn: TShellListColumn; var AText: String) of object;

  TCSLVFileAddedEvent = procedure(Sender: TObject; Item: TListItem) of object; deprecated;

  TCustomShellListView = class(TCustomListView)
  private
    FAutoSizeColumns: Boolean;
    FFileSortType: TFileSortType;
    FMask: string;
    FMaskCaseSensitivity: TMaskCaseSensitivity;
    FObjectTypes: TObjectTypes;
    FOnSortCompare: TFileItemCompareEvent;
    FPopulateDelayed: Boolean;
    FRoot: string;
    FShellTreeView: TCustomShellTreeView;
    FUseBuiltInIcons: Boolean;
    FLockUpdate: Integer;
    FOnAddItem: TAddItemEvent;
    FOnFileAdded: TSLVFileAddedEvent;
    FOnGetCellText: TSLVGetCellTextEvent;
    { Setters and getters }
    function IsColumnsStored: Boolean;
    procedure SetFileSortType(AValue: TFileSortType);
    procedure SetMask(const AValue: string);
    procedure SetMaskCaseSensitivity(AValue: TMaskCaseSensitivity);
    procedure SetOnSortCompare(AValue: TFileItemCompareEvent);
    procedure SetShellTreeView(const Value: TCustomShellTreeView);
    procedure SetRoot(const Value: string);
    procedure SetObjectTypes(const Value: TObjectTypes);
  protected
    FSizeFunc: TSLVSizeFunc;
    { Methods specific to Lazarus }
    class procedure WSRegisterClass; override;
    procedure AdjustColWidths;
    procedure CreateHandle; override;
    function CreateListColumns: TListColumns; override;
    function CreateListItem: TListItem; override;
    procedure PopulateWithRoot();
    procedure DoOnResize; override;
    function GetCellText(AItem: TShellListItem; AColumn: TShellListColumn; AFileName: String): String;
    procedure InitColumns; virtual;
    procedure Loaded; override;
    procedure SetAutoSizeColumns(const Value: Boolean); virtual;
    procedure DoAddItem(const ABasePath: String; const AFileInfo: TSearchRec; var CanAdd: Boolean);
    function GetBuiltinImageIndex(const AFileName: String; ALargeImage: Boolean): Integer;
    function GetListColumnClass: TListColumnClass; override;
    property OnFileAdded: TCSLVFileAddedEvent read FOnFileAdded write FOnFileAdded;
  public
    { Basic methods }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Methods specific to Lazarus }
    function FindColumn(AColumnID: TSLVColumnID; ACustomID: Integer = 0): TShellListColumn;
    function GetPathFromItem(ANode: TListItem): string;
    procedure UpdateColumn(AColumn: TShellListColumn);
    procedure UpdateView;
    { Properties }
    property AutoSizeColumns: Boolean read FAutoSizeColumns write SetAutoSizeColumns default true;
    property Mask: string read FMask write SetMask; // Can be used to conect to other controls
    property MaskCaseSensitivity: TMaskCaseSensitivity read FMaskCaseSensitivity write SetMaskCaseSensitivity default mcsPlatformDefault;
    property ObjectTypes: TObjectTypes read FObjectTypes write SetObjectTypes default [otNonFolders];
    property FileSortType: TFileSortType read FFileSortType write SetFileSortType default fstNone;
    property Root: string read FRoot write SetRoot;
    property ShellTreeView: TCustomShellTreeView read FShellTreeView write SetShellTreeView;
    property UseBuiltInIcons: Boolean read FUseBuiltinIcons write FUseBuiltInIcons default true;
    property OnAddItem: TAddItemEvent read FOnAddItem write FOnAddItem;
    property OnGetCellText: TSLVGetCellTextEvent read FOnGetCellText write FOnGetCellText;
    property OnSortCompare: TFileItemCompareEvent read FOnSortCompare write SetOnSortCompare;
    { Protected properties which users may want to access, see bug 15374 }
    property Items;
  end;

  { TShellListView }

  TShellListView = class(TCustomShellListView)
  published
    { TCustomListView properties
      The same as TListView excluding data properties }

    property Align;
    property AutoSizeColumns;
    property Anchors;
    property BorderSpacing;
    property BorderStyle;
    property BorderWidth;
    property Color default clWindow;
    property Columns stored IsColumnsStored;
    property Constraints;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property LargeImages;
    property LargeImagesWidth;
    property Mask;
    property MaskCaseSensitivity;
    property MultiSelect;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly default True;
    property RowSelect;
    property ScrollBars;
    property ShowColumnHeaders;
    property ShowHint;
    property SmallImages;
    property SmallImagesWidth;
    property SortColumn;
    property SortType;
    property StateImages;
    property TabStop;
    property TabOrder;
    property ToolTips;
    property Visible;
    property ViewStyle default vsReport;
    { TCustomShellListView properties }
    property ObjectTypes;
    property Root;
    property ShellTreeView;
    property FileSortType;

    property OnChange;
    property OnClick;
    property OnColumnClick;
    property OnCompare;
    property OnContextPopup;
    property OnDblClick;
    property OnDeletion;
    property OnDragDrop;
    property OnDragOver;
    property OnEdited;
    property OnEditing;
    property OnEndDrag;
    property OnGetCellText;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnMouseWheelHorz;
    property OnMouseWheelLeft;
    property OnMouseWheelRight;
    property OnResize;
    property OnSelectItem;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property OnAddItem;
    property OnFileAdded;
    property OnSortCompare;
  end;

  { TShellTreeNode }

  TShellTreeNode = class(TTreeNode)
  private
    FBasePath: String;
  protected
    FFileInfo: TSearchRec;
    procedure SetBasePath(ABasePath: String);
  public
    function ShortFilename: String;
    function FullFilename: String;
    function IsDirectory: Boolean;

    property BasePath: String read FBasePath;
  end;

  EShellCtrl = class(Exception);
  EInvalidPath = class(EShellCtrl);

function DbgS(OT: TObjectTypes): String; overload;
function DbgS(CS: TMaskCaseSensitivity): String; overload;

procedure Register;

implementation

uses
 WSShellCtrls, LazMethodList
{$ifdef windows}
  ,Windows, ShellApi
{$endif};

const
  //no need to localize, it's a message for the programmer
  sShellTreeViewIncorrectNodeType = 'TShellTreeView: the newly created node is not a TShellTreeNode!';
  MaskCaseSensitivityStrings: array[TMaskCaseSensitivity] of String = ('mcsPlatformDefault', 'mcsCaseInsensitive', 'mcsCaseSensitive');

  DateColumnFormat = 'ddddd t';  // short date + short time

  FileSizeFormat: array[TSLVSizeUnits] of string = ('', '%.0n', '%.1n', '%.1n', '%.1n');

function DbgS(OT: TObjectTypes): String; overload;
begin
  Result := '[';
  if (otFolders in OT) then Result := Result + 'otFolders,';
  if (otNonFolders in OT) then Result := Result + 'otNonFolders,';
  if (otHidden in OT) then Result := Result + 'otHidden';
  if Result[Length(Result)] = ',' then System.Delete(Result, Length(Result), 1);
  Result := Result + ']';
end;

function DbgS(CS: TMaskCaseSensitivity): String;
begin
  Result := MaskCaseSensitivityStrings[CS];
end;

function AttrToStr(Attr: LongInt): String;
begin
  Result := '';
  if faReadOnly and Attr <> 0 then Result := Result + 'R';
  if faHidden and Attr <> 0 then Result := Result + 'H';
  if faSysFile and Attr <> 0 then Result := Result + 'S';
  if faArchive and Attr <> 0 then Result := Result + 'A';
end;

const
  ONE_KB = 1024;
  ONE_MB = 1024 * 1024;
  ONE_GB = 1024 * 1024 * 1024;

function FileSizeToBytesStr(AFileSize: Int64; AFormat: String = '%.0n'): String;
begin
  Result := Format(AFormat, [1.0*AFileSize]);
end;

function FileSizeToKBytesStr(AFileSize: Int64; AFormat: String = '%.1n'): String;
begin
  Result := Format(sShellCtrlsKB, [Format(AFormat, [AFileSize / ONE_KB])]);
end;

function FileSizeToMBytesStr(AFileSize: Int64; AFormat: String = '%.1n'): String;
begin
  Result := Format(sShellCtrlsMB, [Format(AFormat, [AFileSize / ONE_MB])]);
end;

function FileSizeToGBytesStr(AFileSize: Int64; AFormat: String = '%.1n'): String;
begin
  Result := Format(sShellCtrlsGB, [Format(AFormat, [AFileSize / ONE_GB])]);
end;

function FileSizeToStr(AFileSize: Int64; AFormat: String = '%.1n'): String;
begin
  if AFileSize < ONE_KB then
    Result := Format(sShellCtrlsBytes, [IntToStr(AFileSize)])
  else
  if AFileSize < ONE_MB then
    Result := Format(sShellCtrlsKB, [IntToStr(AFileSize div ONE_KB)])
  else
  if AFileSize < ONE_GB then
    Result := Format(sShellCtrlsMB, [IntToStr(AFileSize div ONE_MB)])
  else
    Result := Format(sShellCtrlsGB, [Format(AFormat, [AFileSize / ONE_GB])]);
end;


{ TShellListItem }

function TShellListItem.IsFolder: Boolean;
begin
  Result := (FFileInfo.Attr and faDirectory) = faDirectory;
end;


{ TShellListColumn }

procedure TShellListColumn.Assign(ASource: TPersistent);
begin
  if ASource is TShellListColumn then
  begin
    FColumnID := TShellListColumn(ASource).ColumnID;
    FCustomID := TShellListColumn(ASource).CustomID;
    FFormat := TShellListColumn(ASource).Format;
    FSizeUnits := TShellListColumn(ASource).SizeUnits;
  end;
  inherited;
end;

function TShellListColumn.GetListView: TCustomShellListView;
begin
  Result := TCustomShellListView(TListColumns(Collection).Owner);
end;

function TShellListColumn.GetSizeFormat: String;
begin
  if FFormat = '' then
    Result := FileSizeFormat[FSizeUnits]
  else
    Result := FFormat;
end;

procedure TShellListColumn.SetColumnID(const AValue: TSLVColumnID);
begin
  if AValue = FColumnID then exit;
  FColumnID := AValue;
  case FColumnID of
    cidFileName: Caption := sShellCtrlsName;
    cidSize: Caption := sShellCtrlsSize;
    cidType: Caption := sShellCtrlsType;
    cidAttr: Caption := sShellCtrlsAttributes;
    cidDateModified: Caption := sShellCtrlsModificationDate;
  end;
  Changed(true);
end;

procedure TShellListColumn.SetCustomID(const AValue: Integer);
begin
  if AValue = FCustomID then exit;
  FCustomID := AValue;
  Changed(true);
end;

procedure TShellListColumn.SetFormat(const AValue: String);
begin
  if AValue = FFormat then exit;
  FFormat := AValue;
  GetListView.UpdateColumn(Self);
  Changed(false);
end;

procedure TShellListColumn.SetSizeUnits(const AValue: TSLVSizeUnits);
begin
  if AValue = FSizeUnits then exit;
  FSizeUnits := AValue;
  GetListView.UpdateColumn(Self);
  Changed(false);
end;


{ TShellListColumns }

function TShellListColumns.Add: TShellListColumn;
begin
  Result := TShellListColumn(inherited Add);
  if (Owner <> nil) and
     ([csDesigning,csLoading,csReading]*Owner.ComponentState = [csDesigning])
  then
    OwnerFormDesignerModified(Owner);
end;

function TShellListColumns.GetItem(const AIndex: Integer): TShellListColumn;
begin
  Result := TShellListColumn(inherited GetItem(AIndex));
end;

procedure TShellListColumns.SetItem(const AIndex: Integer; const AValue: TShellListColumn);
begin
  inherited SetItem(AIndex, AValue);
end;


{ TFileItem : internal helper class used for temporarily storing info in an internal TStrings component}

constructor TFileItem.Create(const DirInfo:TSearchRec; ABasePath: String);
begin
  FFileInfo := DirInfo;
  FBasePath:= ABasePath;
  isFolder:=DirInfo.Attr and FaDirectory > 0;
end;


{ TFileItemAVLTree

  Specialized TAVLTree descendant for sorting the TFileItems found by the
  helper function GetFilesInDirectory such that a user-friendly compare function
  can be applied. }

type
  TFileItemAVLTree = class(TAVLTree)
  private
    FFileItemCompare: TFileItemCompareEvent;
    function InternalFileItemCompare(ATree: TAvlTree; Item1, Item2: Pointer): Integer;
  public
    constructor CreateFileItemCompare(ACompare: TFileItemCompareEvent);
  end;

constructor TFileItemAVLTree.CreateFileItemCompare(ACompare: TFileItemCompareEvent);
begin
  FFileItemCompare := ACompare;
  inherited CreateObjectCompare(@InternalFileItemCompare);
end;

function TFileItemAVLTree.InternalFileItemCompare(ATree: TAvlTree; Item1, Item2: Pointer): Integer;
begin
  Result := FFileItemCompare(TFileItem(Item1), TFileItem(Item2));
end;

{ TShellTreeNode }

procedure TShellTreeNode.SetBasePath(ABasePath: String);
begin
  FBasePath := ABasePath;
end;


function TShellTreeNode.ShortFilename: String;
begin
  Result := Text;
end;

function TShellTreeNode.FullFilename: String;
begin
  if (FBasePath <> '') then
    Result := AppendPathDelim(FBasePath) + Text 
  else
    //root nodes
    Result := Text;
  {$if defined(windows) and not defined(wince)}
  if (Length(Result) = 2) and (Result[2] = DriveSeparator) then
    Result := Result + PathDelim;
  {$endif}
end;

function TShellTreeNode.IsDirectory: Boolean;
begin
  Result := ((FFileInfo.Attr and faDirectory) > 0);
end;


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

procedure TCustomShellTreeView.SetUseBuiltinIcons(const AValue: Boolean);
begin
  if FUseBuiltinIcons = AValue then exit;
  FUseBuiltinIcons := AValue;
  Invalidate;
end;

procedure TCustomShellTreeView.DoCreateNodeClass(
  var NewNodeClass: TTreeNodeClass);
begin
  NewNodeClass := TShellTreeNode;
  inherited DoCreateNodeClass(NewNodeClass);
end;

procedure TCustomShellTreeView.Loaded;
begin
  inherited Loaded;
  if (FInitialRoot = '') then
    PopulateWithBaseFiles()
  else
    SetRoot(FInitialRoot);
end;

function TCustomShellTreeView.CreateNode: TTreeNode;
begin
  Result := inherited CreateNode;
  //just in case someone attaches a new OnCreateNodeClass which does not return a TShellTreeNode (sub)class
  if not (Result is TShellTreeNode) then
    Raise EShellCtrl.Create(sShellTreeViewIncorrectNodeType);
end;

function TCustomShellTreeView.CreateRootNode(const APath: string): TTreeNode;
var
  dirInfo: TSearchRec;
begin
  Result := Items.AddChild(nil, APath);
  TShellTreeNode(Result).SetBasePath('');
  FindFirstUTF8(APath, faAnyFile, dirInfo);
  TShellTreeNode(Result).FFileInfo := dirInfo;
  FindCloseUTF8(dirInfo);
  Result.HasChildren := True;
  Result.Expand(False);
end;

procedure TCustomShellTreeView.SetRoot(const AValue: string);
var
  RootNode: TTreeNode;
begin
  if FRoot=AValue then exit;
  if (csLoading in ComponentState) then
  begin
    FInitialRoot := AValue;
    Exit;
  end;
  //Delphi raises an exception in this case, but don't crash the IDE at designtime
  if not (csDesigning in ComponentState)
     and (AValue <> '')
     and not DirectoryExistsUtf8(ExpandFilenameUtf8(AValue)) then
     Raise EInvalidPath.CreateFmt(sShellCtrlsInvalidRoot,[ExpandFileNameUtf8(AValue)]);
  if (AValue = '') then
    FRoot := GetBasePath
  else
    FRoot:=AValue;
  Items.Clear;
  if FRoot = '' then
  begin
    PopulateWithBaseFiles()
  end
  else
  begin
    //Add a node for Root and expand it (issue #0024230)
    //Make FRoot contain fully qualified pathname, we need it later in GetPathFromNode()
    FRoot := ExpandFileNameUtf8(FRoot);
    //Set RootNode.Text to AValue so user can choose if text is fully qualified path or not
    RootNode := CreateRootNode(AValue);
  end;
  if Assigned(ShellListView) then
    ShellListView.Root := FRoot;
end;

// ToDo: Optimize, now the tree is populated in constructor, SetRoot and SetFileSortType.
// For some reason it does not show in performance really.
procedure TCustomShellTreeView.SetFileSortType(const AValue: TFileSortType);
var
  RootNode: TTreeNode;
  CurrPath: String;
begin
  if FFileSortType=AValue then exit;
  FFileSortType:=AValue;
  if (([csLoading, csDesigning] * ComponentState) <> []) then Exit;
  CurrPath := GetPath;
  try
    BeginUpdate;
    Items.Clear;
    if FRoot = '' then
      PopulateWithBaseFiles()
    else
    begin
      RootNode := CreateRootNode(FRoot);
      if ExistsAndIsValid(CurrPath) then
        SetPath(CurrPath);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TCustomShellTreeView.SetObjectTypes(AValue: TObjectTypes);
var
  CurrPath: String;
begin
  if FObjectTypes = AValue then Exit;
  FObjectTypes := AValue;
  if (csLoading in ComponentState) then Exit;
  CurrPath := GetPath;
  try
    BeginUpdate;
    //Refresh(nil);
    //if ExistsAndIsValid(CurrPath) then
    //  SetPath(CurrPath);
    UpdateView;
  finally
    EndUpdate;
  end;
end;

procedure TCustomShellTreeView.SetOnSortCompare(AValue: TFileItemCompareEvent);
var
  RootNode: TTreeNode;
  CurrPath: String;
begin
  if SameMethod(TMethod(AValue), TMethod(FOnSortCompare)) then
    Exit;

  FOnSortCompare := AValue;

  if (([csLoading,csDesigning] * ComponentState) <> []) or (FFileSortType <> fstCustom) then
    Exit;

  CurrPath := GetPath;
  try
    BeginUpdate;
    Items.Clear;
    if FRoot = '' then
      PopulateWithBaseFiles()
    else
    begin
      RootNode := CreateRootNode(FRoot);
      if ExistsAndIsValid(Currpath) then
        SetPath(CurrPath);
    end;
  finally
    EndUpdate;
  end;
end;

function TCustomShellTreeView.CanExpand(Node: TTreeNode): Boolean;
var
  OldAutoExpand: Boolean;
begin
  Result:=inherited CanExpand(Node);
  if not Result then exit;
  OldAutoExpand:=AutoExpand;
  AutoExpand:=False;
  BeginUpdate;
  try
    case FExpandCollapseMode of
      ecmRefreshedExpanding:
        begin
          Node.DeleteChildren;
          Result := PopulateTreeNodeWithFiles(Node, GetPathFromNode(Node));
        end;
      ecmKeepChildren:
        if Node.Count = 0 then
          Result := PopulateTreeNodeWithFiles(Node, GetPathFromNode(Node))
        else
          Result := true;
      ecmCollapseAndClear:
        Result := PopulateTreeNodeWithFiles(Node, GetPathFromNode(Node));
    end;
    AutoExpand:=OldAutoExpand;
  finally
    EndUpdate;
  end;
end;

procedure TCustomShellTreeView.Collapse(Node: TTreeNode);
var
  hadChildren: Boolean;
begin
  if csDestroying in ComponentState then
    exit;
  if ExpandCollapseMode = ecmCollapseAndClear then
  begin
    BeginUpdate;
    try
      hadChildren := Node.HasChildren;
      Node.DeleteChildren;
      Node.HasChildren := hadChildren;
    finally
      EndUpdate;
    end;
  end;
  inherited;
end;

constructor TCustomShellTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInitialRoot := '';
  FUseBuiltinIcons := true;
  PathDelimiter := SysUtils.PathDelim;
  {$IFDEF CaseInsensitiveFilenames}
  FFindOptions := [foFindExpands, foFindIgnoresCase];
  {$ELSE}
  FFindOptions := [foFindExpands];
  {$ENDIF}
  Options := Options + [tvoReadOnly];

  // Initial property values
  FObjectTypes:= [otFolders];

  // Populating the base dirs is done in Loaded
end;

destructor TCustomShellTreeView.Destroy;
begin
  ShellListView := nil;
  inherited Destroy;
end;

function FilesSortAlphabet(p1, p2: Pointer): Integer;
var
  f1, f2: TFileItem;
begin
  f1:=TFileItem(p1);
  f2:=TFileItem(p2);
  Result:=CompareText(f1.FileInfo.Name, f2.FileInfo.Name);
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

  AResult will contain TFileItem objects upon return, make sure to free them in the calling routine

  AMask may contain multiple file masks separated by ;
}
procedure GetFilesInDirectory(const ABaseDir: string; AMask: string;
  AObjectTypes: TObjectTypes; AResult: TStrings; AFileSortType: TFileSortType;
  ACaseSensitivity: TMaskCaseSensitivity = mcsPlatformDefault;
  ASortCompare: TFileItemCompareEvent = nil);
var
  DirInfo: TSearchRec;
  FindResult, i: Integer;
  IsDirectory, IsValidDirectory, IsHidden, AddFile, UseMaskList, CaseSens: Boolean;
  SearchStr, ShortFilename: string;
  MaskList: TMaskList = nil;
  Files: TFileItemAVLTree;
  FileItem: TFileItem;
  avlNode: TAVLTreeNode;
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
    while (Length(AMask) > 0) and (AMask[Length(AMask)] = ';') do
      Delete(AMask, Length(AMask), 1);
    if Trim(AMask) = '' then
      AMask := AllFilesMask;
    //Use a TMaskList if more than 1 mask is specified or if MaskCaseSensitivity differs from the platform default behaviour
    UseMaskList := (Pos(';', AMask) > 0) or
                   {$ifdef NotLiteralFilenames}
                   (ACaseSensitivity = mcsCaseSensitive)
                   {$else}
                   (ACaseSensitivity = mcsCaseInsensitive)
                   {$endif}
                   ;
    if UseMaskList then
    begin
      // Disable sets and ranges in the MaskList. [...] is interpreted as literal chars.
      // Otherwise this would be incompatible with the situation if no MaskList was used
      // and would break backwards compatibilty and could raise unexpected EConvertError.
      // If you need sets/ranges in the MaskList, use the OnAddItem event for that. (BB)
      {$ifdef NotLiteralFilenames}
      CaseSens := ACaseSensitivity = mcsCaseSensitive;
      {$else}
      CaseSens := ACaseSensitivity <> mcsCaseInsensitive;
      {$endif}
      MaskList := TMaskList.Create(AMask, ';', CaseSens, MaskOpCodesDisableRange);
    end;

    try
      Files := nil;
      case AFileSortType of
        fstAlphabet:
          Files := TFileItemAVLTree.Create(@FilesSortAlphabet);
        fstFoldersFirst:
          Files := TFileItemAVLTree.Create(@FilesSortFoldersFirst);
        fstCustom:
          if ASortCompare <> nil then
            Files := TFileItemAVLTree.CreateFileItemCompare(ASortCompare);
      end;

      i := 0;
      if UseMaskList then
        SearchStr := IncludeTrailingPathDelimiter(ABaseDir) + AllFilesMask
      else
        SearchStr := IncludeTrailingPathDelimiter(ABaseDir) + AMask; //single mask, let FindFirst/FindNext handle matching

      FindResult := FindFirstUTF8(SearchStr, faAnyFile, DirInfo);
      while (FindResult = 0) do
      begin
        ShortFilename := DirInfo.Name;
        IsValidDirectory := (ShortFilename <> '.') and (ShortFilename <> '..');
        //no need to call MaskListMatches (which loops through all masks) if ShortFileName is '.' or '..' since we never process this
        if ((not UseMaskList) or MaskList.Matches(DirInfo.Name)) and IsValidDirectory  then
        begin
          inc(i);
          if i = 100 then
          begin
            Application.ProcessMessages;
            i := 0;
          end;
          IsDirectory := (DirInfo.Attr and FaDirectory = FaDirectory);
          IsHidden := (DirInfo.Attr and faHidden{%H-} = faHidden{%H-});

          // First check if we show hidden files
          if IsHidden then
            AddFile := (otHidden in AObjectTypes)
          else
            AddFile := True;

          // If it is a directory, check if it is a valid one
          if IsDirectory then
            AddFile := AddFile and ((otFolders in AObjectTypes) and IsValidDirectory)
          else
            AddFile := AddFile and (otNonFolders in AObjectTypes);

          // AddFile identifies if the file is valid or not
          if AddFile then
          begin
            if Assigned(Files) then
              Files.Add(TFileItem.Create(DirInfo, ABaseDir))
            else
              AResult.AddObject(ShortFilename, TFileItem.Create(DirInfo, ABaseDir));
          end;
        end;// Filename matches the mask
        FindResult := FindNextUTF8(DirInfo);
      end; //FindResult = 0

      FindCloseUTF8(DirInfo);
    finally
      MaskList.Free;
    end;

    if Assigned(Files) then
    begin
      avlNode := Files.FindLowest;
      while Assigned(avlNode) do
      begin
        FileItem := TFileItem(avlNode.Data);
        AResult.AddObject(FileItem.FileInfo.Name, FileItem);
        avlNode := Files.FindSuccessor(avlNode);
      end;

      //don't free the TFileItems here, they will freed by the calling routine
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
  {$ifdef HASAMIGA}
  Result := '';
  {$endif}
end;

function TCustomShellTreeView.GetRootPath: string;
begin
  if FRoot <> '' then
    Result := FRoot
  else
    Result := GetBasePath();
  if Result <> '' then
    Result := IncludeTrailingPathDelimiter(Result);
end;

class procedure TCustomShellTreeView.GetFilesInDir(const ABaseDir: string;
  AMask: string; AObjectTypes: TObjectTypes; AResult: TStrings;
  AFileSortType: TFileSortType; ACaseSensitivity: TMaskCaseSensitivity);
begin
  GetFilesInDirectory(ABaseDir, AMask, AObjectTypes, AResult,
                      AFileSortType, ACaseSensitivity);
end;

function TCustomShellTreeView.NodeHasChildren(Node: TTreeNode): Boolean;

  function HasSubDir(Const ADir: String): Boolean;
  var
    SR: TSearchRec;
    FindRes: LongInt;
    Attr: Longint;
    IsHidden: Boolean;
  begin
    Result:=False;
    try
      Attr := faDirectory;
      if (otHidden in fObjectTypes) then Attr := Attr or faHidden{%H-};
      FindRes := FindFirstUTF8(AppendPathDelim(ADir) + AllFilesMask, Attr , SR);
      while (FindRes = 0) do
      begin
        if ((SR.Attr and faDirectory <> 0) and (SR.Name <> '.') and
           (SR.Name <> '..')) then
        begin
          IsHidden := ((Attr and faHidden{%H-}) > 0);
          if not (IsHidden and (not ((otHidden in fObjectTypes)))) then
          begin
            Result := True;
            Break;
          end;
        end;
        FindRes := FindNextUtf8(SR);
      end;
    finally
      FindCloseUTF8(SR);
    end; //try
  end;

var
  NodePath: String;
begin
  if Assigned(OnHasChildren) then
    Result := OnHasChildren(Self, Node)
  else
  begin
    NodePath := GetPathFromNode(Node);
    if (fObjectTypes * [otNonFolders] = []) then
      Result := TShellTreeNode(Node).IsDirectory and HasSubDir(NodePath)
    else
      Result := TShellTreeNode(Node).IsDirectory;
  end;
end;

{ Returns true if at least one item was added, false otherwise }
function TCustomShellTreeView.PopulateTreeNodeWithFiles(
  ANode: TTreeNode; ANodePath: string): Boolean;
var
  i: Integer;
  Files: TStringList;
  NewNode: TTreeNode;
  CanAdd: Boolean;
begin
  Result := False;
  // avoids crashes in the IDE by not populating during design
  if (csDesigning in ComponentState) then Exit;

  Files := TStringList.Create;
  Items.BeginUpdate;
  try
    Files.OwnsObjects := True;
    GetFilesInDirectory(ANodePath, AllFilesMask, FObjectTypes, Files,
                        FFileSortType, mcsPlatformDefault, FOnSortCompare);
    Result := Files.Count > 0;

    for i := 0 to Files.Count - 1 do
    begin
      CanAdd := True;
      with TFileItem(Files.Objects[i]) do DoAddItem(FBasePath, FileInfo, CanAdd);
      if CanAdd then
      begin
        NewNode := Items.AddChildObject(ANode, Files[i], nil);
        TShellTreeNode(NewNode).FFileInfo := TFileItem(Files.Objects[i]).FileInfo;
        TShellTreeNode(NewNode).SetBasePath(TFileItem(Files.Objects[i]).FBasePath);
        // NewNode.HasChildren will be set later when needed to avoid opening
        // all subdirectories (--> NodeHasChildren).
      end;
    end;
  finally
    Files.Free;
    Items.EndUpdate;
  end;
end;

procedure TCustomShellTreeView.PopulateWithBaseFiles;
{$if defined(windows) and not defined(wince)}
var
  r: LongWord;
  Drives: array[0..128] of char;
  pDrive: PChar;
  NewNode: TTreeNode;
begin
  // avoids crashes in the IDE by not populating during design
  if (csDesigning in ComponentState) then Exit;
  Items.BeginUpdate;
  try
    Items.Clear;
    r := GetLogicalDriveStrings(SizeOf(Drives), Drives);
    if r = 0 then Exit;
    if r > SizeOf(Drives) then Exit;
  //    raise Exception.Create(SysErrorMessage(ERROR_OUTOFMEMORY));
    pDrive := Drives;
    while pDrive^ <> #0 do
    begin
      NewNode := Items.AddChildObject(nil, ExcludeTrailingBackslash(pDrive), pDrive);
      //Yes, we want to remove the backslash,so don't use ChompPathDelim here
      TShellTreeNode(NewNode).FFileInfo.Name := ExcludeTrailingBackslash(pDrive);
      //On NT platforms drive-roots really have these attributes
      TShellTreeNode(NewNode).FFileInfo.Attr := faDirectory + faSysFile{%H-} + faHidden{%H-};
      TShellTreeNode(NewNode).SetBasePath('');
      NewNode.HasChildren := True;
      Inc(pDrive, 4);
    end;
  finally
    Items.EndUpdate;
  end;
end;
{$else}
var
  NewNode: TTreeNode;
begin
  // avoids crashes in the IDE by not populating during design
  // also do not populate before loading is done
  if ([csDesigning, csLoading] * ComponentState <> []) then Exit;
  Items.Clear;

  // This allows showing "/" in Linux, but in Windows it makes no sense to show the base
  if GetBasePath() <> '' then
  begin
    NewNode := Items.AddChild(nil, GetBasePath());
    NewNode.HasChildren := True;
    PopulateTreeNodeWithFiles(NewNode, GetBasePath());
    NewNode.Expand(False);
  end
  else
    PopulateTreeNodeWithFiles(nil, GetBasePath());
end;
{$endif}

procedure TCustomShellTreeView.DoSelectionChanged;
var
  ANode: TTreeNode;
  CurrentNodePath: String;
begin
  inherited DoSelectionChanged;
  ANode := Selected;
  if Assigned(FShellListView) and Assigned(ANode) then
  begin
    //You cannot rely on HasChildren here, because it can become FALSE when user
    //clicks the expand sign and folder is empty
    //Issue 0027571
    CurrentNodePath := ChompPathDelim(GetPathFromNode(ANode));
    if TShellTreeNode(ANode).IsDirectory then
    begin
      //Note: the folder may have been deleted in the mean time
      //an exception will be raised by the next line in that case
      FShellListView.Root := GetPathFromNode(ANode)
    end
    else
    begin
      if not FileExistsUtf8(CurrentNodePath) then
        Raise EShellCtrl.CreateFmt(sShellCtrlsSelectedItemDoesNotExists,[CurrentNodePath]);
      if Assigned(Anode.Parent) then
        FShellListView.Root := GetPathFromNode(ANode.Parent)
      else
        FShellListView.Root := '';
    end;
  end;
end;

procedure TCustomShellTreeView.DoAddItem(const ABasePath: String;
  const AFileInfo: TSearchRec; var CanAdd: Boolean);
begin
  if Assigned(FOnAddItem) then
    FOnAddItem(Self, ABasePath, AFileInfo, CanAdd);
end;

function TCustomShellTreeView.DrawBuiltInIcon(ANode: TTreeNode; ARect: TRect): TSize;
begin
  if FUseBuiltinIcons then
    Result := TWSCustomShellTreeViewClass(WidgetSetClass).DrawBuiltInIcon(Self, ANode, ARect)
  else
    Result := inherited;
end;

function TCustomShellTreeView.GetBuiltinIconSize: TSize;
begin
  if FUseBuiltinIcons then
    Result := TWSCustomShellTreeViewClass(WidgetsetClass).GetBuiltinIconSize
  else
    Result := inherited;
end;

function TCustomShellTreeView.GetPathFromNode(ANode: TTreeNode): string;
begin
  if Assigned(ANode) then
  begin
    Result := TShellTreeNode(ANode).FullFilename;
    if TShellTreeNode(ANode).IsDirectory then
      Result := AppendPathDelim(Result);
    if not FilenameIsAbsolute(Result) then
      Result := GetRootPath() + Result;    // Include root directory
  end
  else
    Result := '';
end;

procedure TCustomShellTreeView.Refresh(ANode: TTreeNode);
//nil will refresh root
var
  RootNodeText: String;
  IsRoot: Boolean;
begin
  if (Items.Count = 0) then Exit;
  {$ifdef debug_shellctrls}
  debugln(['TCustomShellTreeView.Refresh: GetFirstVisibleNode.Text = "',Items.GetFirstVisibleNode.Text,'"']);
  {$endif}

  IsRoot := (ANode = nil) or ((ANode = Items.GetFirstVisibleNode) and (GetRootPath <> ''));
  {$ifdef debug_shellctrls}
  debugln(['IsRoot = ',IsRoot]);
  {$endif}


  if (ANode = nil) and (GetRootPath <> '') then ANode := Items.GetFirstVisibleNode;
  if IsRoot then
  begin
    if Assigned(ANode) then
      RootNodeText := ANode.Text  //this may differ from FRoot, so don't use FRoot here
    else
      RootNodeText := GetRootPath;
    {$ifdef debug_shellctrls}
    debugln(['IsRoot = TRUE, RootNodeText = "',RootNodeText,'"']);
    {$endif}


    FRoot := #0; //invalidate FRoot
    SetRoot(RootNodeText); //re-initialize the entire tree
  end
  else
  begin
    ANode.Expand(False);
  end;
end;

{ Rebuilds the tree for all expanded nodes from the node corresponding to
  AStartDir (or from root if AStartDir is empty) to react on changes in the
  file system. Collapsed nodes will be updated anyway when they are expanded. }
procedure TCustomShellTreeView.UpdateView(AStartDir: String = '');

  function FindExistingSubPath(APath: String): String;
  var
    path: String;
    i: Integer;
  begin
    APath := AppendPathDelim(APath);
    Result := APath;
    for i := 1 to Length(APath) do
    begin
      if APath[i] = PathDelimiter then
      begin
        path := Copy(APath, 1, i);
        if ExistsAndIsValid(path) then
          Result := path
        else
          break;
      end;
    end;
    Result := ChompPathDelim(Result);
  end;

  procedure RecordNodeState(const ANode: TTreeNode; const AExpandedPaths: TStringList);
  var
    currentNode: TTreeNode;
    firstChild: TTreeNode;
  begin
    currentNode := ANode;
    while currentNode <> nil do
    begin
      if currentNode.Expanded then
      begin
        AExpandedPaths.Add(GetPathFromNode(currentNode));
        firstChild := currentNode.GetFirstChild();
        if firstChild <> nil then
          RecordNodeState(firstChild, AExpandedPaths);
      end;
      currentNode := currentNode.GetNextSibling();
    end;
  end;

  procedure RestoreNodeState(const ANode: TTreeNode; const ARefresh: boolean;
    const AExpandedPaths: TStringList);
  var
    currentNode: TTreeNode;
    firstChild: TTreeNode;
  begin
    currentNode := ANode;
    while currentNode <> nil do
    begin
      if AExpandedPaths.IndexOf(GetPathFromNode(currentNode)) >= 0 then
      begin
        currentNode.Expanded := True;
        if ARefresh then
          Refresh(currentNode);
        firstChild := currentNode.GetFirstChild();
        if firstChild <> nil then
          RestoreNodeState(firstChild, ARefresh, AExpandedPaths);
      end
      else
        currentNode.Expanded := False;
      currentNode := currentNode.GetNextSibling();
    end;
  end;

var
  node: TTreeNode;
  firstNode: TTreeNode;
  startNode: TTreeNode;
  topNodePath: String;
  selectedPath: String;
  selectedWasExpanded: Boolean = false;
  expandedPaths: TStringList;
  listviewRefreshNeeded: Boolean;
begin
  if FUpdateLock <> 0 then
    exit;

  expandedPaths := TStringList.Create;
  Items.BeginUpdate;
  try
    topNodePath := ChompPathDelim(GetPathFromNode(TopItem));
    selectedPath := GetPathFromNode(Selected);
    if Assigned(Selected) then
      selectedWasExpanded := Selected.Expanded;

    firstNode := Items.GetFirstNode;
    if AStartDir = '' then
    begin
      startNode := firstNode;
      listViewRefreshNeeded := true;
    end else
    begin
      // Make sure that AStartDir is a valid, existing path. If not, go back in
      // hierarchy until a valid subpath is found.
      startNode := Items.FindNodeWithTextPath(FindExistingSubPath(AStartDir));
      // Set a flag to refresh the ShellListView if affected by the refresh.
      listViewRefreshNeeded := (AStartDir = '') or (startNode = Selected);
      if (Selected = nil) and Assigned(FShellListView) then
         listViewRefreshNeeded := (FShellListView.Items.Count> 0);
    end;

    RecordNodeState(startNode, expandedPaths);
    RestoreNodeState(startNode, true, expandedPaths);

    if ExistsAndIsValid(selectedPath) then
    begin
      Path := selectedPath;
      // Setting the path expands the selected node --> apply the stored state.
      Selected.Expanded := selectedWasExpanded;
      Selected.HasChildren := NodeHasChildren(Selected);
      // Avoid selected node to scroll away.
      TopItem := Items.FindNodeWithTextPath(topNodePath);
    end;

    // Force synchronization of associated ShellListView, but only if the
    // refresh affects the selected tree node.
    if Assigned(FShellListView) and listViewRefreshNeeded then
    begin
      inc(FUpdateLock);
      try
        FShellListView.UpdateView;
      finally
        dec(FUpdateLock);
      end;
    end;
  finally
    Items.EndUpdate;
    expandedPaths.Free;
  end;
end;

function TCustomShellTreeView.GetPath: string;
begin
  Result := GetPathFromNode(Selected);
end;

function TCustomShellTreeView.ExistsAndIsValid(APath: String): Boolean;
// APath should be fully qualified
var
  Attr: LongInt;
begin
  Result := False;
  Attr := FileGetAttrUtf8(APath);
  {$ifdef debug_shellctrls}
  debugln(['TCustomShellTreeView.SetPath.Exists: Attr = ', Attr]);
  {$endif}
  if (Attr = -1) then Exit;
  if not (otNonFolders in FObjectTypes) then
    Result := ((Attr and faDirectory) > 0)
  else
  begin
    if not (otHidden in FObjectTypes) then
      Result := ((Attr and faHidden) = 0)
    else
      Result := True;
  end;
  {$ifdef debug_shellctrls}
  debugln(['TCustomShellTreeView.SetPath.Exists: Result = ',Result]);
  {$endif}
end;

{
SetPath: Path can be
- Absolute like '/usr/lib'
- Relative like 'foo/bar'
  This can be relative to:
  - Self.Root (which takes precedence over)
  - Current directory
}
procedure TCustomShellTreeView.SetPath(AValue: string);
var
  sl: TStringList;
  Node: TTreeNode;
  i: integer;
  FQRootPath, RelPath: String;
  RootIsAbsolute: Boolean;
  IsRelPath: Boolean;

  function GetAdjustedNodeText(ANode: TTreeNode): String;
  begin
    if (ANode = Items.GetFirstVisibleNode) and (FQRootPath <> '') then
    begin
      if not RootIsAbsolute then
        Result := ''
      else
        Result := FQRootPath;
    end
    else Result := ANode.Text;
  end;

  function PathIsDriveRoot({%H-}Path: String): Boolean;  {$if not (defined(windows) and not defined(wince))}inline;{$endif}
  //WinNT filesystem reports faHidden on all physical drive-roots (e.g. C:\)
  begin
    {$if defined(windows) and not defined(wince)}
    Result := (Length(Path) = 3) and
              (Upcase(Path[1]) in ['A'..'Z']) and
              (Path[2] = DriveSeparator) and
              (Path[3] in AllowDirectorySeparators);
    {$else}
    Result := False;
    {$endif windows}
  end;

  function ContainsHiddenDir(Fn: String): Boolean;
  var
    i: Integer;
    Attr: LongInt;
    Dirs: TStringList;
    RelPath: String;
  begin
    //if fn=root then always return false
    if (CompareFileNames(Fn, FQRootPath) = 0) then
      Result := False
    else
    begin
      Attr := FileGetAttrUtf8(Fn);
      Result := ((Attr and faHidden{%H-}) = faHidden{%H-}) and not PathIsDriveRoot(Fn);
      if not Result then
      begin
        //it also is not allowed that any folder above is hidden
        Fn := ChompPathDelim(Fn);
        Fn := ExtractFileDir(Fn);
        Dirs := TStringList.Create;
        try
          Dirs.StrictDelimiter := True;
          Dirs.Delimiter := PathDelim;
          Dirs.DelimitedText := Fn;
          Fn := '';
          for i := 0 to Dirs.Count - 1 do
          begin
            if (i = 0) then
              Fn := Dirs.Strings[i]
            else
              Fn := Fn + PathDelim + Dirs.Strings[i];
            if (Fn = '') then Continue;
            RelPath := CreateRelativePath(Fn, FQRootPath, False, True);
            //don't check if Fn now is "higher up the tree" than the current root
            if (RelPath = '') or ((Length(RelPath) > 1) and (RelPath[1] = '.') and (RelPath[2] = '.')) then
            begin
              {$ifdef debug_shellctrls}
              debugln(['TCustomShellTreeView.SetPath.ContainsHidden: Fn is higher: ',Fn]);
              {$endif}
              Continue;
            end;
            {$if defined(windows) and not defined(wince)}
            if (Length(Fn) = 2) and (Fn[2] = ':') then Continue;
            {$endif}
            Attr := FileGetAttrUtf8(Fn);
            if (Attr <> -1) and ((Attr and faHidden{%H-}) > 0) and not PathIsDriveRoot(Fn) then
            begin
              Result := True;
              {$ifdef debug_shellctrls}
              debugln(['TCustomShellTreeView.SetPath.Exists: a subdir is hidden: Result := False']);
              {$endif}
              Break;
            end;
          end;
        finally
          Dirs.Free;
        end;
      end;
    end;
  end;

begin
  RelPath := '';

  {$ifdef debug_shellctrls}
  debugln(['SetPath: GetRootPath = "',getrootpath,'"',' AValue=',AValue]);
  {$endif}
  if (GetRootPath <> '') then
    //FRoot is already Expanded in SetRoot, just add PathDelim if needed
    FQRootPath := AppendPathDelim(GetRootPath)
  else
    FQRootPath := '';
  RootIsAbsolute := (FQRootPath = '') or (FQRootPath = PathDelim)
                    {$ifdef mswindows}
                    or ((Length(FQRootPath) = 3) and (FQRootPath[2] = ':') and (FQRootPath[3] = PathDelim))
                    {$endif};

  {$ifdef debug_shellctrls}
  debugln(['SetPath: FQRootPath = ',fqrootpath]);
  debugln(['SetPath: RootIsAbsolute = ',RootIsAbsolute]);
  debugln(['SetPath: FilenameIsAbsolute = ',FileNameIsAbsolute(AValue)]);
  {$endif}

  if not FileNameIsAbsolute(AValue) then
  begin
    if ExistsAndIsValid(FQRootPath + AValue) then
    begin
      //Expand it, since it may be in the form of ../../foo
      AValue := ExpandFileNameUtf8(FQRootPath + AValue);
    end
    else
    begin
      //don't expand Avalue yet, we may need it in error message
      if not ExistsAndIsValid(ExpandFileNameUtf8(AValue)) then
        Raise EInvalidPath.CreateFmt(sShellCtrlsInvalidPath,[ExpandFileNameUtf8(FQRootPath + AValue)]);
      //Directory (or file) ExistsAndIsValid
      //Make it fully qualified
      AValue := ExpandFileNameUtf8(AValue);
    end;
  end
  else
  begin
    //AValue is an absoulte path to begin with , but still needs  expanding (because TryCreateRelativePath requires this)
    AValue := ExpandFilenameUtf8(AValue);
    //if not DirectoryExistsUtf8(AValue) then
    if not ExistsAndIsValid(AValue) then
      Raise EInvalidPath.CreateFmt(sShellCtrlsInvalidPath,[AValue]);
  end;

  //AValue now is a fully qualified path and it ExistsAndIsValid
  //Now check if it is a subdirectory of FQRootPath
  //RelPath := CreateRelativePath(AValue, FQRootPath, False);
  IsRelPath := (FQRootPath = '') or TryCreateRelativePath(AValue, FQRootPath, False, True, RelPath);

  {$ifdef debug_shellctrls}
  debugln('TCustomShellTreeView.SetPath: ');
  debugln(['  IsRelPath = ',IsRelPath]);
  debugln(['  RelPath = "',RelPath,'"']);
  debugln(['  FQRootPath = "',FQRootPath,'"']);
  {$endif}

  if (not IsRelpath) or ((RelPath <> '') and ((Length(RelPath) > 1) and (RelPath[1] = '.') and (RelPath[2] = '.'))) then
  begin
    // CreateRelativePath returns a string beginning with ..
    // so AValue is not a subdirectory of FRoot
    Raise EInvalidPath.CreateFmt(sShellCtrlsInvalidPathRelative,[AValue, FQRootPath]);
  end;

  if (RelPath = '') and (FQRootPath = '') then
    RelPath := AValue;
  {$ifdef debug_shellctrls}
  debugln(['RelPath = ',RelPath]);
  {$endif}

  if (RelPath = '') then
  begin
    {$ifdef debug_shellctrls}
    debugln('Root selected');
    {$endif}
    Node := Items.GetFirstVisibleNode;
    if Assigned(Node) then
    begin
      Node.Expanded := True;
      Node.Selected := True;
    end;
    Exit;
  end;

  if not (otHidden in FObjectTypes) and ContainsHiddenDir(AValue) then
    Raise EInvalidPath.CreateFmt(sShellCtrlsInvalidPath,[AValue, FQRootPath]);

  sl := TStringList.Create;
  sl.Delimiter := PathDelim;
  sl.StrictDelimiter := True;
  sl.DelimitedText := RelPath;
  if (sl.Count > 0) and (sl[0] = '') then  // This happens when root dir is empty
    sl[0] := PathDelim;                    //  and PathDelim was the first char
  if (sl.Count > 0) and (sl[sl.Count-1] = '') then sl.Delete(sl.Count-1); //remove last empty string
  if (sl.Count = 0) then
  begin
    sl.Free;
    Exit;
  end;

  {$ifdef debug_shellctrls}
  for i := 0 to sl.Count - 1 do debugln(['sl[',i,']="',sl[i],'"']);
  {$endif}

  BeginUpdate;
  try
    Node := Items.GetFirstVisibleNode;
    {$ifdef debug_shellctrls}
    if assigned(node) then debugln(['GetFirstVisibleNode = ',GetAdjustedNodeText(Node)]);
    {$endif}
    //Root node doesn't have Siblings in this case, we need one level down the tree
    if (GetRootPath <> '') and Assigned(Node) then
    begin
      {$ifdef debug_shellctrls}
      debugln('Root node doesn''t have Siblings');
      {$endif}
      Node := Node.GetFirstVisibleChild;
      {$ifdef debug_shellctrls}
      debugln(['Node = ',GetAdjustedNodeText(Node)]);
      {$endif}
      //I don't know why I wrote this in r44893, but it seems to be wrong so I comment it out
      //for the time being (2015-12-05: BB)
      //if RootIsAbsolute then sl.Delete(0);
    end;

    for i := 0 to sl.Count-1 do
    begin
      {$ifdef debug_shellctrls}
      DbgOut(['i=',i,' sl[',i,']=',sl[i],' ']);
      if Node <> nil then DbgOut(['GetAdjustedNodeText = ',GetAdjustedNodeText(Node)])
      else  DbgOut('Node = NIL');
      debugln;
      {$endif}
      while (Node <> Nil) and
            {$IF defined(CaseInsensitiveFilenames) or defined(NotLiteralFilenames)}
            (Utf8LowerCase(GetAdjustedNodeText(Node)) <> Utf8LowerCase(sl[i]))
            {$ELSE}
            (GetAdjustedNodeText(Node) <> sl[i])
            {$ENDIF}
            do
            begin
              {$ifdef debug_shellctrls}
              DbgOut(['  i=',i,' "',GetAdjustedNodeText(Node),' <> ',sl[i],' -> GetNextVisibleSibling -> ']);
              {$endif}
              Node := Node.GetNextVisibleSibling;
              {$ifdef debug_shellctrls}
              if Node <> nil then DbgOut(['GetAdjustedNodeText = ',GetAdjustedNodeText(Node)])
              else DbgOut('Node = NIL');
              debugln;
              {$endif}
            end;
      if Node <> Nil then
      begin
        Node.Expanded := True;
        Node.Selected := True;
        Node := Node.GetFirstVisibleChild;
      end
      else
        Break;
    end;
  finally
    sl.free;
    EndUpdate;
  end;
end;

class procedure TCustomShellTreeView.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterCustomShellTreeView;
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

procedure TCustomShellListView.SetFileSortType(AValue: TFileSortType);
begin
  if FFileSortType=AValue then Exit;
  FFileSortType:=AValue;
  Clear;
  Items.Clear;
  PopulateWithRoot();
end;

procedure TCustomShellListView.SetMaskCaseSensitivity(
  AValue: TMaskCaseSensitivity);
var
  OldMask: String;
  NeedRefresh: Boolean;
begin
  if FMaskCaseSensitivity = AValue then Exit;
  {$ifdef NotLiteralFilenames}
  if (FMaskCaseSensitivity in [mcsPlatformDefault, mcsCaseInsensitive]) then
    NeedRefresh := (AValue = mcsCaseSensitive)
  else
    NeedRefresh := True;
  {$else}
  if (FMaskCaseSensitivity in [mcsPlatformDefault, mcsCaseSensitive]) then
    NeedRefresh := (AValue = mcsCaseInsensitive)
  else
    NeedRefresh :=True;
  {$endif}
  FMaskCaseSensitivity := AValue;
  if NeedRefresh then
  begin
    //Trick SetMask to believe a refresh is needed.
    OldMask := FMask;
    FMask := #0 + FMask;
    SetMask(OldMask);
  end;
end;

procedure TCustomShellListView.SetOnSortCompare(AValue: TFileItemCompareEvent);
begin
  if SameMethod(TMethod(AValue), TMethod(FOnSortCompare)) then
    Exit;
  FOnSortCompare:=AValue;
  Clear;
  Items.Clear;
  PopulateWithRoot();
end;

procedure TCustomShellListView.SetObjectTypes(const Value: TObjectTypes);
var
  sel: String = '';
begin
  if FObjectTypes = Value then Exit;
  FObjectTypes := Value;

  BeginUpdate;
  try
    if Assigned(Selected) then
      sel := Selected.Caption;
    Clear;
    PopulateWithRoot();
    if Sel <> '' then
      Selected := FindCaption(0, sel, false, true, false);
  finally
    EndUpdate;
  end;
end;

procedure TCustomShellListView.SetRoot(const Value: string);
begin
  if FRoot <> Value then
  begin
    //Delphi raises an unspecified exception in this case, but don't crash the IDE at designtime
    if not (csDesigning in ComponentState)
       and (Value <> '')
       and not DirectoryExistsUtf8(ExpandFilenameUtf8(Value)) then
       Raise EInvalidPath.CreateFmt(sShellCtrlsInvalidRoot,[Value]);
    FRoot := Value;
    BeginUpdate;
    try
      Clear;
      Items.Clear;
      PopulateWithRoot();
    finally
      EndUpdate;
    end;
  end;
end;

function TCustomShellListView.FindColumn(AColumnID: TSLVColumnID;
  ACustomID: Integer = 0): TShellListColumn;
var
  i: Integer;
  col: TShellListColumn;
begin
  if AColumnID <> cidCustom then
    for i := 0 to ColumnCount-1 do
    begin
      col := TShellListColumn(Columns[i]);
      if (col.ColumnID = AColumnID) then
      begin
        Result := col;
        exit;
      end;
    end
  else
    for i := 0 to ColumnCount-1 do
    begin
      col := TShellListColumn(Columns[i]);
      if col.CustomID = ACustomID then
      begin
        Result := col;
        exit;
      end;
    end;
  Result := nil;
end;

procedure TCustomShellListView.UpdateColumn(AColumn: TShellListColumn);
var
  i: Integer;
  item: TShellListItem;
  newText: String;
begin
  if AColumn = nil then Exit;

  if AColumn.ColumnID = cidSize then
    case AColumn.SizeUnits of
      suDefault: FSizeFunc := @FileSizeToStr;
      suBytes: FSizeFunc := @FileSizeToBytesStr;
      suKB: FSizeFunc := @FileSizeToKBytesStr;
      suMB: FSizeFunc := @FileSizeToMBytesStr;
      suGB: FSizeFunc := @FileSizeToGBytesStr;
    end;

  BeginUpdate;
  try
    for i := 0 to Items.Count-1 do
    begin
      item := TShellListItem(Items[i]);
      newText := GetCellText(item, AColumn, item.Caption);
      if AColumn.ColumnID = cidFileName then
        item.Caption := newText
      else
        item.SubItems[AColumn.Index - 1] := newText;
    end;
    if AColumn.ColumnID = cidDateModified then
      AdjustColWidths;
  finally
    EndUpdate;
  end;
end;

constructor TCustomShellListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FUseBuiltInIcons := true;

  // Initial property values
  ViewStyle := vsReport;
  ObjectTypes := [otNonFolders];
  FMaskCaseSensitivity := mcsPlatformDefault;
  FAutoSizeColumns := true;
  ReadOnly := true;
  FSizeFunc := @FileSizeToStr;

  InitColumns;
end;

destructor TCustomShellListView.Destroy;
begin
  ShellTreeView := nil;
  inherited Destroy;
end;

function TCustomShellListView.GetBuiltinImageIndex(const AFileName: String;
  ALargeImage: Boolean): Integer;
begin
  Result := TWSCustomShellListViewClass(WidgetsetClass).GetBuiltInImageIndex(
    self, AFileName, ALargeImage
  );
end;

function TCustomShellListView.GetListColumnClass: TListColumnClass;
begin
  Result := TShellListColumn;
end;

procedure TCustomShellListView.PopulateWithRoot();
var
  i, j: Integer;
  Files: TStringList;
  NewItem: TShellListItem;
  FileItem: TFileItem;
  CurFileName, CurFilePath: string;
  CurFileSize: Int64;
  CanAdd: Boolean;
  col: TShellListColumn;
begin
  // csDesigning: avoids crashes in the IDE by not populating during design
  // csLoading: prevents multiple execution during loading
  if (([csLoading, csDesigning] * ComponentState) <> []) then Exit;

  // Check inputs
  if Trim(FRoot) = '' then Exit;

  // Check handle
  if not HandleAllocated then
  begin
    FPopulateDelayed := true;
    Exit;
  end;

  Items.BeginUpdate;
  Files := TStringList.Create;
  try
    Files.OwnsObjects := True;
    GetFilesInDirectory(FRoot, Trim(FMask), FObjectTypes, Files, FFileSortType,
                        FMaskCaseSensitivity, FOnSortCompare);

    for i := 0 to Files.Count - 1 do
    begin
      FileItem := TFileItem(Files.Objects[i]);
      CanAdd := True;
      with FileItem do DoAddItem(FBasePath, FileInfo, CanAdd);
      if CanAdd then
      begin
        NewItem := TShellListItem(Items.Add);
        NewItem.FileInfo := FileItem.FileInfo;
        CurFileName := Files.Strings[i];
        CurFilePath := IncludeTrailingPathDelimiter(FRoot) + CurFileName;

        for j := 0 to ColumnCount-1 do
        begin
          col := TShellListColumn(Columns[j]);
          if col.ColumnID = cidFileName then
            NewItem.Caption := CurFileName
          else
            NewItem.SubItems.Add(GetCellText(NewItem, col, CurFileName));
        end;
        // Image index
        if FUseBuiltInIcons then
        begin
          if (ViewStyle = vsIcon) and (LargeImages = nil) then
            NewItem.ImageIndex := GetBuiltInImageIndex(CurFilePath, true)
          else if (ViewStyle <> vsIcon) and (SmallImages = nil) then
            NewItem.ImageIndex := GetBuiltinImageIndex(CurFilePath, false);
        end;
        if Assigned(FOnFileAdded) then FOnFileAdded(Self,NewItem);
      end;
    end;
    Sort;
  finally
    Files.Free;
    Items.EndUpdate;
  end;
end;

function TCustomShellListView.GetCellText(AItem: TShellListItem;
  AColumn: TShellListColumn; AFileName: String): String;
var
  fmt: String;
begin
  Result := '';
  case AColumn.ColumnID of
    cidFileName:
      Result := AFileName;
    cidSize:
      if not AItem.IsFolder then  // no size display for folders.
        Result := FSizeFunc(AItem.FileInfo.Size, AColumn.GetSizeFormat)
      else
        Result := '';
    cidType:
      if not AItem.IsFolder then
        Result := ExtractFileExt(AFileName)
      else
        Result := sShellCtrlsFolder;
    cidAttr:
      Result := AttrToStr(AItem.FileInfo.Attr);
    cidDateModified:
      begin
        if AColumn.Format = '' then
          fmt := DateColumnFormat
        else
          fmt := AColumn.Format;
        Result := FormatDateTime(fmt, AItem.FileInfo.TimeStamp);
      end;
    cidCustom:
      ;  // Get text only from event handler
  end;
  if Assigned(FOnGetCellText) then
    FOnGetCellText(self, AItem, AColumn, Result);
end;

procedure TCustomShellListView.InitColumns;
begin
  Columns.BeginUpdate;
  try
    Columns.Clear;
    with TShellListColumn(Columns.Add) do
    begin
      ColumnID := cidFileName;
    end;
    with TShellListColumn(Columns.Add) do
    begin
      ColumnID := cidSize;
      Alignment := taRightJustify;
    end;
    with TShellListcolumn(Columns.Add) do
    begin
      ColumnID := cidType;
    end;
    AdjustColWidths;
  finally
    Columns.EndUpdate;
  end;
end;

function TCustomShellListView.IsColumnsStored: Boolean;
begin
  Result := not (
    (ColumnCount = 3) and
    (TShellListcolumn(Columns[0]).ColumnID = cidFileName) and
    (TShellListcolumn(Columns[1]).ColumnID = cidSize) and
    (TShellListcolumn(Columns[2]).ColumnID = cidType)
  );
end;

procedure TCustomShellListView.Loaded;

  procedure FixColumnIDs;
  var
    IDsMissing: Boolean;
    i: Integer;
  begin
    if ColumnCount > 3 then exit;
    for i := 0 to ColumnCount-1 do
      if TShellListColumn(Columns[i]).ColumnID <> cidCustom then
        exit;

    // We get here when the lfm file has been written by an old Laz version
    // which did not write the ColumnIDs --> the columns must be filename, size, type
    if ColumnCount > 0 then
      TShellListColumn(Columns[0]).ColumnID := cidFileName;
    if ColumnCount > 1 then
      TShellListColumn(Columns[1]).ColumnID := cidSize;
    if ColumnCount > 2 then
      TShellListColumn(Columns[2]).ColumnID := cidType;
  end;

begin
  inherited Loaded;
  FixColumnIDs;
  PopulateWithRoot;
end;

procedure TCustomShellListView.AdjustColWidths;
var
  colWidth: Integer;
  sumOfWidths: Integer;
  widthAvail: Integer;
  c: Integer;
  col: TShellListColumn;
  testDate: TDateTime;
  nCol: Integer;
  colWidths: array of Integer = nil;
  weightName: Integer;
  canv: TControlCanvas;
  fmt: String;
begin
  if Self.Columns.Count < 3 then
    Exit;
  if (Column[0].Width <> 0) and (not AutoSizeColumns) then
    Exit;

  SetLength(colWidths, ColumnCount);  // all colWidths are initialized with 0
  widthAvail := ClientWidth;   // Width to be distributed
  nCol := ColumnCount;         // number of columns to receive  distributed space

  // Calculate fixed width of date column, if available.
  canv := TControlCanvas.Create;
  try
    canv.Control := Self;
    testDate := EncodeDate(2000,12,29) + EncodeTime(12,0,0,0);
    for c := 0 to ColumnCount-1 do
    begin
      col := TShellListColumn(Columns[c]);
      if col.ColumnID = cidDateModified then
      begin
        if col.Format = '' then
          fmt := DateColumnFormat
        else
          fmt := col.Format;
        colWidths[c] := canv.TextWidth(FormatDateTime(fmt, testDate) + 'MM');   //'MM' to simulate the cell padding
        dec(widthAvail, colWidths[c]);
        dec(nCol);
      end;
    end;
  finally
    canv.Free;
  end;

  // If the space available is small, alloc a larger percentage to the secondary fields
  if ClientWidth < 400 then
    weightName := 2
  else
    weightName := 4;

  colWidth := widthAvail div (nCol + weightName - 1);
  colWidths[0] := weightName * colWidth;
  sumOfWidths := 0;
  for c := 0 to ColumnCount-1 do
  begin
    col := TShellListColumn(Column[c]);
    if colWidths[c] = 0 then
      colWidths[c] := colWidth;
    inc(sumOfWidths, colWidths[c]);
  end;
  inc(colWidths[0], ClientWidth - sumOfWidths); // distribute remaining space

  // Apply calculated colwidths to Columns
  BeginUpdate;
  try
    for c := 0 to ColumnCount-1 do
      Column[c].Width := colWidths[c];
  finally
    EndUpdate;
  end;
end;

procedure TCustomShellListView.CreateHandle;
begin
  inherited;
  if FPopulateDelayed then
  begin
    PopulateWithRoot;
    FPopulateDelayed := false;
  end;
end;

function TCustomShellListView.CreateListColumns: TListColumns;
begin
  Result := TShellListColumns.Create(Self);
end;

function TCustomShellListView.CreateListItem: TListItem;
begin
  if Assigned(OnCreateItemClass) then
    Result := inherited CreateListItem
  else
    Result := TShellListItem.Create(Items);
end;

procedure TCustomShellListView.DoOnResize;
begin
  inherited;
  AdjustColWidths;
end;

procedure TCustomShellListView.SetAutoSizeColumns(const Value: Boolean);
begin
  if Value = FAutoSizeColumns then
    Exit;
  FAutoSizeColumns := Value;
  if Value then
    AdjustColWidths;
end;

procedure TCustomShellListView.DoAddItem(const ABasePath: String;
  const AFileInfo: TSearchRec; var CanAdd: Boolean);
begin
  if Assigned(FOnAddItem) then
    FOnAddItem(Self, ABasePath, AFileInfo, CanAdd);
end;

function TCustomShellListView.GetPathFromItem(ANode: TListItem): string;
begin
  Result := IncludeTrailingPathDelimiter(FRoot) + ANode.Caption;
end;

{ Re-reads the list to react on changes in the file system. }
procedure TCustomShellListView.UpdateView;
var
  selectedItem: String = '';
begin
  if (FLockUpdate = 0) then
  begin
    if Assigned(Selected) then
      selectedItem := Selected.Caption;
    Clear;
    PopulateWithRoot;
    if selectedItem <> '' then
      Selected := FindCaption(0, selectedItem, false, true, false);

    if Assigned(ShellTreeView) then
    begin
      inc(FLockUpdate);
      try
        ShellTreeView.UpdateView(FRoot);
      finally
        dec(FLockUpdate);
      end;
    end;
  end;
end;

class procedure TCustomShellListView.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterCustomShellListView;
end;

procedure Register;
begin
  RegisterComponents('Misc',[TShellTreeView, TShellListView]);
end;

end.
