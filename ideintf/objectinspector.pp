{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
   This unit defines the TObjectInspector.
   It uses TOIPropertyGrid and TOIPropertyGridRow which are also defined in this
   unit. The object inspector uses property editors (see TPropertyEditor) to
   display and control properties, thus the object inspector is merely an
   object viewer than an editor. The property editors do the real work.


  ToDo:
   - backgroundcolor=clNone
   - replace pair splitter with splitter
   - Define Init values
   - Set to init value
}
unit ObjectInspector;

{$MODE OBJFPC}{$H+}

{off $DEFINE DoNotCatchOIExceptions}

interface

uses
  Forms, SysUtils, Buttons, Classes, Graphics, GraphType, StdCtrls, LCLType,
  LCLIntf, LCLProc, Controls, ComCtrls, ExtCtrls, TypInfo, Messages,
  LResources, PairSplitter, Laz_XMLCfg, Menus, Dialogs, ObjInspStrConsts,
  PropEdits, GraphPropEdits, ListViewPropEdit, ImageListEditor,
  ComponentTreeView;


type
  EObjectInspectorException = class(Exception);
  
  TObjectInspector = class;


  { TOIOptions }

  TOIOptions = class
  private
    FComponentTreeHeight: integer;
    FCustomXMLCfg: TXMLConfig;
    FDefaultItemHeight: integer;
    FFilename:string;
    FFileAge: longint;
    FShowComponentTree: boolean;
    FXMLCfg: TXMLConfig;
    FFileHasChangedOnDisk: boolean;

    FSaveBounds: boolean;
    FLeft: integer;
    FTop: integer;
    FWidth: integer;
    FHeight: integer;
    FPropertyGridSplitterX: integer;
    FEventGridSplitterX: integer;

    FGridBackgroundColor: TColor;
    FShowHints: boolean;
    procedure SetFilename(const NewFilename: string);
    function FileHasChangedOnDisk: boolean;
    function GetXMLCfg: TXMLConfig;
    procedure FileUpdated;
  public
    constructor Create;
    destructor Destroy;  override;
    function Load: boolean;
    function Save: boolean;
    procedure Assign(AnObjInspector: TObjectInspector);
    procedure AssignTo(AnObjInspector: TObjectInspector);
  public
    property Filename:string read FFilename write SetFilename;
    property CustomXMLCfg: TXMLConfig read FCustomXMLCfg write FCustomXMLCfg;

    property SaveBounds:boolean read FSaveBounds write FSaveBounds;
    property Left:integer read FLeft write FLeft;
    property Top:integer read FTop write FTop;
    property Width:integer read FWidth write FWidth;
    property Height:integer read FHeight write FHeight;
    property PropertyGridSplitterX:integer read FPropertyGridSplitterX
                                           write FPropertyGridSplitterX;
    property EventGridSplitterX:integer read FEventGridSplitterX
                                        write FEventGridSplitterX;
    property DefaultItemHeight: integer read FDefaultItemHeight
                                        write FDefaultItemHeight;
    property ShowComponentTree: boolean read FShowComponentTree
                                        write FShowComponentTree;
    property ComponentTreeHeight: integer read FComponentTreeHeight
                                          write FComponentTreeHeight;

    property GridBackgroundColor: TColor read FGridBackgroundColor
                                         write FGridBackgroundColor;
    property ShowHints: boolean read FShowHints
                                write FShowHints;
  end;

  TOICustomPropertyGrid = class;


  { TOIPropertyGridRow }

  TOIPropertyGridRow = class
  private
    FTop:integer;
    FHeight:integer;
    FLvl:integer;
    FName:string;
    FExpanded: boolean;
    FTree:TOICustomPropertyGrid;
    FChildCount:integer;
    FPriorBrother,
    FFirstChild,
    FLastChild,
    FNextBrother,
    FParent:TOIPropertyGridRow;
    FEditor: TPropertyEditor;
    procedure GetLvl;
  public
    constructor Create(PropertyTree:TOICustomPropertyGrid;  PropEditor:TPropertyEditor;
       ParentNode:TOIPropertyGridRow);
    destructor Destroy; override;
    function ConsistencyCheck: integer;
    function HasChild(Row: TOIPropertyGridRow): boolean;
  public
    Index:integer;
    LastPaintedValue:string;
    function GetBottom:integer;
    function IsReadOnly: boolean;
    function IsDisabled: boolean;
    procedure MeasureHeight(ACanvas: TCanvas);
  public
    property Editor:TPropertyEditor read FEditor;
    property Top:integer read FTop write FTop;
    property Height:integer read FHeight write FHeight;
    property Bottom: integer read GetBottom;
    property Lvl:integer read FLvl;
    property Name: string read FName;
    property Expanded:boolean read FExpanded;
    property Tree:TOICustomPropertyGrid read FTree;
    property Parent:TOIPropertyGridRow read FParent;
    property ChildCount:integer read FChildCount;
    property FirstChild:TOIPropertyGridRow read FFirstChild;
    property LastChild:TOIPropertyGridRow read FFirstChild;
    property NextBrother:TOIPropertyGridRow read FNextBrother;
    property PriorBrother:TOIPropertyGridRow read FPriorBrother;
  end;

  //----------------------------------------------------------------------------
  TOIPropertyGridState = (pgsChangingItemIndex, pgsApplyingValue);
  TOIPropertyGridStates = set of TOIPropertyGridState;
  
  TOICustomPropertyGrid = class(TCustomControl)
  private
    FBackgroundColor:TColor;
    FChangeStep: integer;
    FCurrentButton: TWinControl; // nil or ValueButton
    FCurrentEdit: TWinControl;  // nil or ValueEdit or ValueComboBox
    FCurrentEditorLookupRoot: TPersistent;
    FDefaultItemHeight:integer;
    FDragging:boolean;
    FExpandedProperties:TStringList;
    FExpandingRow:TOIPropertyGridRow;
    FFilter: TTypeKinds;
    FIndent:integer;
    FItemIndex:integer;
    FNameFont,FDefaultValueFont,FValueFont:TFont;
    FOnModified: TNotifyEvent;
    FPreferredSplitterX: integer; // best splitter position
    FPropertyEditorHook: TPropertyEditorHook;
    FRows:TList;
    FSelection: TPersistentSelectionList;
    FSplitterX:integer; // current splitter position
    FStates: TOIPropertyGridStates;
    FTopY:integer;

    // hint stuff
    FHintTimer : TTimer;
    FHintWindow : THintWindow;
    Procedure HintTimer(Sender: TObject);
    Procedure ResetHintTimer;
    procedure OnUserInput(Sender: TObject; Msg: Cardinal);

    procedure IncreaseChangeStep;

    function GetRow(Index:integer):TOIPropertyGridRow;
    function GetRowCount:integer;
    procedure ClearRows;
    function GetCurrentEditValue: string;
    procedure SetCurrentEditValue(const AValue: string);
    procedure SetItemIndex(NewIndex:integer);

    procedure SetItemsTops;
    procedure AlignEditComponents;
    procedure EndDragSplitter;
    procedure SetSplitterX(const NewValue:integer);
    procedure SetTopY(const NewValue:integer);

    function GetTreeIconX(Index:integer):integer;
    function RowRect(ARow:integer):TRect;
    procedure PaintRow(ARow:integer);
    procedure DoPaint(PaintOnlyChangedValues:boolean);

    procedure SetSelection(const ASelection:TPersistentSelectionList);
    procedure SetPropertyEditorHook(NewPropertyEditorHook:TPropertyEditorHook);

    procedure AddPropertyEditor(PropEditor: TPropertyEditor);
    procedure AddStringToComboBox(const s:string);
    procedure ExpandRow(Index:integer);
    procedure ShrinkRow(Index:integer);
    procedure AddSubEditor(PropEditor:TPropertyEditor);

    procedure SetRowValue;
    procedure DoCallEdit;
    procedure RefreshValueEdit;
    Procedure ValueEditDblClick(Sender : TObject);
    procedure ValueEditMouseDown(Sender: TObject; Button:TMouseButton;
      Shift: TShiftState; X,Y:integer);
    procedure ValueEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ValueEditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ValueEditExit(Sender: TObject);
    procedure ValueEditChange(Sender: TObject);
    procedure ValueComboBoxExit(Sender: TObject);
    procedure ValueComboBoxChange(Sender: TObject);
    procedure ValueComboBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ValueComboBoxKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ValueComboBoxCloseUp(Sender: TObject);
    procedure ValueComboBoxDropDown(Sender: TObject);
    procedure ValueButtonClick(Sender: TObject);
    procedure ValueComboBoxDrawItem(Control: TWinControl; Index: Integer;
          ARect: TRect; State: TOwnerDrawState);

    procedure WMVScroll(var Msg: TWMScroll); message WM_VSCROLL;
    procedure SetBackgroundColor(const AValue: TColor);
    procedure UpdateScrollBar;
  protected
    procedure CreateParams(var Params: TCreateParams); override;

    procedure MouseDown(Button:TMouseButton; Shift:TShiftState; X,Y:integer); override;
    procedure MouseMove(Shift:TShiftState; X,Y:integer);  override;
    procedure MouseUp(Button:TMouseButton; Shift:TShiftState; X,Y:integer); override;
    
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure HandleStandardKeys(var Key: Word; Shift: TShiftState); virtual;
    procedure HandleKeyUp(var Key: Word; Shift: TShiftState); virtual;

    procedure EraseBackground(DC: HDC); override;
    
    procedure DoSetBounds(ALeft, ATop, AWidth, AHeight: integer); override;
  public
    ValueEdit:TEdit;
    ValueComboBox:TComboBox;
    ValueButton:TButton;

    constructor Create(TheOwner: TComponent); override;
    constructor CreateWithParams(AnOwner: TComponent;
                                 APropertyEditorHook: TPropertyEditorHook;
                                 TypeFilter: TTypeKinds;
                                 DefItemHeight: integer);
    destructor Destroy;  override;
    function CanEditRowValue: boolean;
    function ConsistencyCheck: integer;
    function GetActiveRow: TOIPropertyGridRow;
    function GetHintTypeAt(RowIndex: integer; X: integer): TPropEditHint;

    function GetRowByPath(const PropPath:string): TOIPropertyGridRow;
    function GridHeight:integer;
    function MouseToIndex(y:integer;MustExist:boolean):integer;
    function PropertyPath(Index:integer):string;
    function TopMax:integer;
    procedure BuildPropertyList;
    procedure Clear;
    procedure Paint;  override;
    procedure PropEditLookupRootChange;
    procedure RefreshPropertyValues;
    procedure SetBounds(aLeft,aTop,aWidth,aHeight:integer); override;
    procedure SetCurrentRowValue(const NewValue: string);
  public
    property BackgroundColor: TColor read FBackgroundColor
                                     write SetBackgroundColor default clBtnFace;
    property BorderStyle default bsSingle;
    property CurrentEditValue: string read GetCurrentEditValue
                                      write SetCurrentEditValue;
    property DefaultItemHeight:integer read FDefaultItemHeight
                                       write FDefaultItemHeight default 25;
    property DefaultValueFont:TFont read FDefaultValueFont write FDefaultValueFont;
    property ExpandedProperties:TStringList read FExpandedProperties
                                            write FExpandedProperties;
    property Indent:integer read FIndent write FIndent default 9;
    property ItemIndex:integer read FItemIndex write SetItemIndex;
    property NameFont:TFont read FNameFont write FNameFont;
    property OnModified: TNotifyEvent read FOnModified write FOnModified;
    property PrefferedSplitterX: integer read FPreferredSplitterX
                                         write FPreferredSplitterX default 100;
    property PropertyEditorHook: TPropertyEditorHook read FPropertyEditorHook
                                                    write SetPropertyEditorHook;
    property RowCount:integer read GetRowCount;
    property Rows[Index:integer]:TOIPropertyGridRow read GetRow;
    property Selection: TPersistentSelectionList read FSelection
                                                 write SetSelection;
    property SplitterX:integer read FSplitterX write SetSplitterX default 100;
    property TopY:integer read FTopY write SetTopY default 0;
    property ValueFont:TFont read FValueFont write FValueFont;
  end;
  
  
  { TOIPropertyGrid }
  
  TOIPropertyGrid = class(TOICustomPropertyGrid)
  published
    property Align;
    property Anchors;
    property BackgroundColor;
    property BorderStyle;
    property Constraints;
    property DefaultItemHeight;
    property DefaultValueFont;
    property Indent;
    property NameFont;
    property OnChangeBounds;
    property OnClick;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnModified;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property PopupMenu;
    property PrefferedSplitterX;
    property SplitterX;
    property Tabstop;
    property ValueFont;
    property Visible;
  end;
  

  { TCustomPropertiesGrid }

  TCustomPropertiesGrid = class(TOICustomPropertyGrid)
  private
    FAutoFreeHook: boolean;
    function GetTIObject: TPersistent;
    procedure SetAutoFreeHook(const AValue: boolean);
    procedure SetTIObject(const AValue: TPersistent);
  public
    constructor Create(TheOwner: TComponent); override;
    property TIObject: TPersistent read GetTIObject write SetTIObject;
    property AutoFreeHook: boolean read FAutoFreeHook write SetAutoFreeHook;
  end;


  //============================================================================
  
  
  { TObjectInspector }
  
  TOnAddAvailablePersistent = procedure(APersistent: TPersistent;
    var Allowed: boolean) of object;

  TOIFlag = (
    oifRebuildPropListsNeeded
    );
  TOIFlags = set of TOIFlag;

  TObjectInspector = class (TForm)
    AvailPersistentComboBox: TComboBox;
    PairSplitter1: TPairSplitter;
    ComponentTree: TComponentTreeView;
    NoteBook: TNoteBook;
    PropertyGrid: TOICustomPropertyGrid;
    EventGrid: TOICustomPropertyGrid;
    StatusBar: TStatusBar;
    MainPopupMenu: TPopupMenu;
    ColorsPopupMenuItem: TMenuItem;
    SetDefaultPopupMenuItem: TMenuItem;
    UndoPropertyPopupMenuItem: TMenuItem;
    BackgroundColPopupMenuItem: TMenuItem;
    ShowHintsPopupMenuItem: TMenuItem;
    ShowComponentTreePopupMenuItem: TMenuItem;
    ShowOptionsPopupMenuItem: TMenuItem;
    procedure AvailComboBoxCloseUp(Sender: TObject);
    procedure ComponentTreeSelectionChanged(Sender: TObject);
    procedure ObjectInspectorResize(Sender: TObject);
    procedure OnGriddKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OnSetDefaultPopupmenuItemClick(Sender: TObject);
    procedure OnUndoPopupmenuItemClick(Sender: TObject);
    procedure OnBackgroundColPopupMenuItemClick(Sender: TObject);
    procedure OnShowHintPopupMenuItemClick(Sender: TObject);
    procedure OnShowOptionsPopupMenuItemClick(Sender: TObject);
    procedure OnShowComponentTreePopupMenuItemClick(Sender: TObject);
    procedure OnMainPopupMenuPopup(Sender: TObject);
    procedure HookRefreshPropertyValues;
  private
    FOnRemainingKeyUp: TKeyEvent;
    FSelection: TPersistentSelectionList;
    FComponentTreeHeight: integer;
    FDefaultItemHeight: integer;
    FFlags: TOIFlags;
    FOnShowOptions: TNotifyEvent;
    FPropertyEditorHook:TPropertyEditorHook;
    FOnAddAvailablePersistent: TOnAddAvailablePersistent;
    FOnSelectPersistentsInOI: TNotifyEvent;
    FOnModified: TNotifyEvent;
    FShowComponentTree: boolean;
    FUpdateLock: integer;
    FUpdatingAvailComboBox: boolean;
    FUsePairSplitter: boolean;
  protected
    function PersistentToString(APersistent: TPersistent): string;
    procedure SetComponentTreeHeight(const AValue: integer);
    procedure SetDefaultItemHeight(const AValue: integer);
    procedure SetOnShowOptions(const AValue: TNotifyEvent);
    procedure SetPropertyEditorHook(NewValue: TPropertyEditorHook);
    procedure SetSelection(const ASelection: TPersistentSelectionList);
    procedure AddPersistentToList(APersistent: TPersistent; List: TStrings);
    procedure HookLookupRootChange;
    procedure OnGridModified(Sender: TObject);
    procedure SetAvailComboBoxText;
    procedure HookGetSelection(const ASelection: TPersistentSelectionList);
    procedure HookSetSelection(const ASelection: TPersistentSelectionList);
    procedure SetShowComponentTree(const AValue: boolean);
    procedure SetUsePairSplitter(const AValue: boolean);
    procedure CreatePairSplitter;
    procedure DestroyNoteBook;
    procedure CreateNoteBook;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
    procedure RefreshSelection;
    procedure RefreshPropertyValues;
    procedure RebuildPropertyLists;
    procedure FillPersistentComboBox;
    procedure BeginUpdate;
    procedure EndUpdate;
    function GetActivePropertyGrid: TOICustomPropertyGrid;
    function GetActivePropertyRow: TOIPropertyGridRow;
    function GetCurRowDefaultValue(var DefaultStr: string): boolean;
  public
    property DefaultItemHeight: integer read FDefaultItemHeight
                                        write SetDefaultItemHeight;
    property Selection: TPersistentSelectionList
                                        read FSelection write SetSelection;
    property OnAddAvailPersistent: TOnAddAvailablePersistent
                 read FOnAddAvailablePersistent write FOnAddAvailablePersistent;
    property OnSelectPersistentsInOI: TNotifyEvent
                   read FOnSelectPersistentsInOI write FOnSelectPersistentsInOI;
    property PropertyEditorHook: TPropertyEditorHook
                           read FPropertyEditorHook write SetPropertyEditorHook;
    property OnModified: TNotifyEvent read FOnModified write FOnModified;
    property OnShowOptions: TNotifyEvent read FOnShowOptions write SetOnShowOptions;
    property OnRemainingKeyUp: TKeyEvent read FOnRemainingKeyUp write FOnRemainingKeyUp;
    property ShowComponentTree: boolean read FShowComponentTree write SetShowComponentTree;
    property ComponentTreeHeight: integer read FComponentTreeHeight write SetComponentTreeHeight;
    property UsePairSplitter: boolean read FUsePairSplitter write SetUsePairSplitter;
  end;

const
  DefaultObjectInspectorName: string = 'ObjectInspector';


//******************************************************************************


implementation

const
  ScrollBarWidth=0;

function SortGridRows(Item1, Item2 : pointer) : integer; 
begin
  Result:= AnsiCompareText(TOIPropertyGridRow(Item1).Name, TOIPropertyGridRow(Item2).Name);
end;


{ TOICustomPropertyGrid }

constructor TOICustomPropertyGrid.CreateWithParams(AnOwner:TComponent;
  APropertyEditorHook:TPropertyEditorHook; TypeFilter:TTypeKinds;
  DefItemHeight: integer);
begin
  inherited Create(AnOwner);
  FSelection:=TPersistentSelectionList.Create;
  FPropertyEditorHook:=APropertyEditorHook;
  FFilter:=TypeFilter;
  FItemIndex:=-1;
  FStates:=[];
  FRows:=TList.Create;
  FExpandingRow:=nil;
  FDragging:=false;
  FExpandedProperties:=TStringList.Create;
  FCurrentEdit:=nil;
  FCurrentButton:=nil;

  // visible values
  FTopY:=0;
  FSplitterX:=100;
  FPreferredSplitterX:=FSplitterX;
  FIndent:=9;
  FBackgroundColor:=clBtnFace;
  FNameFont:=TFont.Create;
  FNameFont.Color:=clWindowText;
  FValueFont:=TFont.Create;
  FValueFont.Color:=clMaroon;
  FDefaultValueFont:=TFont.Create;
  FDefaultValueFont.Color:=clActiveCaption;

  SetInitialBounds(0,0,200,130);
  ControlStyle:=ControlStyle+[csAcceptsControls,csOpaque];
  BorderWidth:=0;
  BorderStyle := bsSingle;

  // create sub components
  ValueEdit:=TEdit.Create(Self);
  with ValueEdit do begin
    Name:='ValueEdit';
    Visible:=false;
    Enabled:=false;
    SetBounds(0,-30,80,25); // hidden
    Parent:=Self;
    OnMouseDown := @ValueEditMouseDown;
    OnDblClick := @ValueEditDblClick;
    OnExit:=@ValueEditExit;
    OnChange:=@ValueEditChange;
    OnKeyDown:=@ValueEditKeyDown;
    OnKeyUp:=@ValueEditKeyUp;
  end;

  ValueComboBox:=TComboBox.Create(Self);
  with ValueComboBox do begin
    Name:='ValueComboBox';
    Visible:=false;
    Enabled:=false;
    SetBounds(0,-30,80,25); // hidden
    Parent:=Self;
    OnMouseDown := @ValueEditMouseDown;
    OnDblClick := @ValueEditDblClick;
    OnExit:=@ValueComboBoxExit;
    //OnChange:=@ValueComboBoxChange; the on change event is called even,
                                   // if the user is editing
    OnKeyDown:=@ValueComboBoxKeyDown;
    OnKeyUp:=@ValueComboBoxKeyUp;
    OnDropDown:=@ValueComboBoxDropDown;
    OnCloseUp:=@ValueComboBoxCloseUp;
    OnDrawItem:=@ValueComboBoxDrawItem;
  end;

  ValueButton:=TButton.Create(Self);
  with ValueButton do begin
    Name:='ValueButton';
    Visible:=false;
    Enabled:=false;
    OnClick:=@ValueButtonClick;
    Caption := '...';
    SetBounds(0,-30,25,25); // hidden
    Parent:=Self;
  end;

  if DefItemHeight<3 then
    FDefaultItemHeight:=ValueComboBox.Height-3
  else
    FDefaultItemHeight:=DefItemHeight;

  BuildPropertyList;
  
  FHintTimer := TTimer.Create(nil);
  FHintTimer.Interval := 500;
  FHintTimer.Enabled := False;
  FHintTimer.OnTimer := @HintTimer;

  FHintWindow := THintWindow.Create(nil);

  FHIntWindow.Visible := False;
  FHintWindow.Caption := 'This is a hint window'#13#10'Neat huh?';
  FHintWindow.HideInterval := 4000;
  FHintWindow.AutoHide := True;
  
  Application.AddOnUserInputHandler(@OnUserInput,true);
end;

procedure TOICustomPropertyGrid.UpdateScrollBar;
var
  ScrollInfo: TScrollInfo;
begin
  if HandleAllocated then begin
    ScrollInfo.cbSize := SizeOf(ScrollInfo);
    ScrollInfo.fMask := SIF_ALL or SIF_DISABLENOSCROLL;
    ScrollInfo.nMin := 0;
    ScrollInfo.nTrackPos := 0;
    ScrollInfo.nMax := TopMax+ClientHeight;
    ScrollInfo.nPage := ClientHeight;
    if ScrollInfo.nPage<1 then ScrollInfo.nPage:=1;
    ScrollInfo.nPos := TopY;
    ShowScrollBar(Handle, SB_VERT, True);
    SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
  end;
end;

procedure TOICustomPropertyGrid.CreateParams(var Params: TCreateParams);
const
  ClassStylesOff = CS_VREDRAW or CS_HREDRAW;
begin
  inherited CreateParams(Params);
  with Params do begin
    {$R-}
    WindowClass.Style := WindowClass.Style and not ClassStylesOff;
    Style := Style or WS_VSCROLL or WS_CLIPCHILDREN;
    {$R+}
    ExStyle := ExStyle or WS_EX_CLIENTEDGE;
  end;
end;

procedure TOICustomPropertyGrid.WMVScroll(var Msg: TWMScroll);
begin
  case Msg.ScrollCode of
      // Scrolls to start / end of the text
    SB_TOP:        TopY := 0;
    SB_BOTTOM:     TopY := TopMax;
      // Scrolls one line up / down
    SB_LINEDOWN:   TopY := TopY + DefaultItemHeight div 2;
    SB_LINEUP:     TopY := TopY - DefaultItemHeight div 2;
      // Scrolls one page of lines up / down
    SB_PAGEDOWN:   TopY := TopY + ClientHeight - DefaultItemHeight;
    SB_PAGEUP:     TopY := TopY - ClientHeight + DefaultItemHeight;
      // Scrolls to the current scroll bar position
    SB_THUMBPOSITION,
    SB_THUMBTRACK: TopY := Msg.Pos;
      // Ends scrolling
    SB_ENDSCROLL: ;
  end;
end;

destructor TOICustomPropertyGrid.Destroy;
var a:integer;
begin
  Application.RemoveOnUserInputHandler(@OnUserInput);
  FItemIndex:=-1;
  for a:=0 to FRows.Count-1 do Rows[a].Free;
  FreeAndNil(FRows);
  FreeAndNil(FSelection);
  FreeAndNil(FValueFont);
  FreeAndNil(FDefaultValueFont);
  FreeAndNil(FNameFont);
  FreeAndNil(FExpandedProperties);
  FreeAndNil(FHintTimer);
  FreeAndNil(FHintWindow);
  inherited Destroy;
end;

function TOICustomPropertyGrid.ConsistencyCheck: integer;
var
  i: integer;
begin
  for i:=0 to FRows.Count-1 do begin
    if Rows[i]=nil then begin
      Result:=-1;
      exit;
    end;
    if Rows[i].Index<>i then begin
      Result:=-2;
      exit;
    end;
    Result:=Rows[i].ConsistencyCheck;
    if Result<>0 then begin
      dec(Result,100);
      exit;
    end;
  end;
  Result:=0;
end;

procedure TOICustomPropertyGrid.SetSelection(
  const ASelection: TPersistentSelectionList);
var
  CurRow:TOIPropertyGridRow;
  OldSelectedRowPath:string;
begin
  OldSelectedRowPath:=PropertyPath(ItemIndex);
  ItemIndex:=-1;
  ClearRows;
  FSelection.Assign(ASelection);
  BuildPropertyList;
  CurRow:=GetRowByPath(OldSelectedRowPath);
  if CurRow<>nil then
    ItemIndex:=CurRow.Index;
end;

procedure TOICustomPropertyGrid.SetPropertyEditorHook(
  NewPropertyEditorHook:TPropertyEditorHook);
begin
  if FPropertyEditorHook=NewPropertyEditorHook then exit;
  FPropertyEditorHook:=NewPropertyEditorHook;
  IncreaseChangeStep;
  SetSelection(FSelection);
end;

function TOICustomPropertyGrid.PropertyPath(Index:integer):string;
var CurRow:TOIPropertyGridRow;
begin
  if (Index>=0) and (Index<FRows.Count) then begin
    CurRow:=Rows[Index];
    Result:=CurRow.Name;
    CurRow:=CurRow.Parent;
    while CurRow<>nil do begin
      Result:=CurRow.Name+'.'+Result;
      CurRow:=CurRow.Parent;
    end;
  end else Result:='';
end;

function TOICustomPropertyGrid.GetRowByPath(
  const PropPath: string): TOIPropertyGridRow;
// searches PropPath. Expands automatically parent rows
var CurName:string;
  s,e:integer;
  CurParentRow:TOIPropertyGridRow;
begin
  Result:=nil;
  if FRows.Count=0 then exit;
  CurParentRow:=nil;
  s:=1;
  while (s<=length(PropPath)) do begin
    e:=s;
    while (e<=length(PropPath)) and (PropPath[e]<>'.') do inc(e);
    CurName:=uppercase(copy(PropPath,s,e-s));
    s:=e+1;
    // search name in childs
    if CurParentRow=nil then
      Result:=Rows[0]
    else
      Result:=CurParentRow.FirstChild;
    while (Result<>nil) and (uppercase(Result.Name)<>CurName) do
      Result:=Result.NextBrother;
    if Result=nil then begin
      exit;
    end else begin
      // expand row
      CurParentRow:=Result;
      ExpandRow(CurParentRow.Index);
    end;
  end;
  if s<=length(PropPath) then Result:=nil;
end;

procedure TOICustomPropertyGrid.SetRowValue;
var
  CurRow: TOIPropertyGridRow;
  NewValue: string;
  OldExpanded: boolean;
  OldChangeStep: integer;
begin
  if not CanEditRowValue then exit;
  OldChangeStep:=fChangeStep;
  CurRow:=Rows[FItemIndex];
  if FCurrentEdit=ValueEdit then
    NewValue:=ValueEdit.Text
  else
    NewValue:=ValueComboBox.Text;
  if length(NewValue)>CurRow.Editor.GetEditLimit then
    NewValue:=LeftStr(NewValue,CurRow.Editor.GetEditLimit);
  if NewValue<>CurRow.Editor.GetVisualValue then begin
    Include(FStates,pgsApplyingValue);
    try
      {$IFNDEF DoNotCatchOIExceptions}
      try
      {$ENDIF}
        //writeln('TOICustomPropertyGrid.SetRowValue B ClassName=',CurRow.Editor.ClassName,' Visual=',CurRow.Editor.GetVisualValue,' NewValue=',NewValue,' AllEqual=',CurRow.Editor.AllEqual);
        CurRow.Editor.SetValue(NewValue);
        //writeln('TOICustomPropertyGrid.SetRowValue C ClassName=',CurRow.Editor.ClassName,' Visual=',CurRow.Editor.GetVisualValue,' NewValue=',NewValue,' AllEqual=',CurRow.Editor.AllEqual);
      {$IFNDEF DoNotCatchOIExceptions}
      except
        on E: Exception do begin
          MessageDlg(oisError, E.Message, mtError, [mbOk], 0);
        end;
      end;
      {$ENDIF}
      if (OldChangeStep<>FChangeStep) then begin
        // the selection has changed
        // => CurRow does not exist any more
        exit;
      end;
      
      // set value in edit control
      if FCurrentEdit=ValueEdit then
        ValueEdit.Text:=CurRow.Editor.GetVisualValue
      else
        ValueComboBox.Text:=CurRow.Editor.GetVisualValue;
        
      // update volatile sub properties
      if (paVolatileSubProperties in CurRow.Editor.GetAttributes)
      and ((CurRow.Expanded) or (CurRow.ChildCount>0)) then begin
        OldExpanded:=CurRow.Expanded;
        ShrinkRow(FItemIndex);
        if OldExpanded then
          ExpandRow(FItemIndex);
      end;
      //writeln('TOICustomPropertyGrid.SetRowValue D ClassName=',CurRow.Editor.ClassName,' Visual=',CurRow.Editor.GetVisualValue,' NewValue=',NewValue,' AllEqual=',CurRow.Editor.AllEqual);
    finally
      Exclude(FStates,pgsApplyingValue);
    end;
    DoPaint(true);
    if Assigned(FOnModified) then FOnModified(Self);
  end;
end;

procedure TOICustomPropertyGrid.DoCallEdit;
var
  CurRow:TOIPropertyGridRow;
  OldChangeStep: integer;
begin
  //writeln('#################### TOICustomPropertyGrid.DoCallEdit ...');
  if (FStates*[pgsChangingItemIndex,pgsApplyingValue]<>[])
  or (FCurrentEdit=nil)
  or (FItemIndex<0)
  or (FItemIndex>=FRows.Count)
  or ((FCurrentEditorLookupRoot<>nil)
    and (FPropertyEditorHook<>nil)
    and (FPropertyEditorHook.LookupRoot<>FCurrentEditorLookupRoot))
  then begin
    exit;
  end;

  OldChangeStep:=fChangeStep;
  CurRow:=Rows[FItemIndex];
  if paDialog in CurRow.Editor.GetAttributes then begin
    {$IFNDEF DoNotCatchOIExceptions}
    try
    {$ENDIF}
      DebugLn('#################### TOICustomPropertyGrid.DoCallEdit for ',CurRow.Editor.ClassName);
      CurRow.Editor.Edit;
    {$IFNDEF DoNotCatchOIExceptions}
    except
      on E: Exception do begin
        MessageDlg(oisError, E.Message, mtError, [mbOk], 0);
      end;
    end;
    {$ENDIF}
    if (OldChangeStep<>FChangeStep) then begin
      // the selection has changed
      // => CurRow does not exist any more
      exit;
    end;

    if FCurrentEdit=ValueEdit then
      ValueEdit.Text:=CurRow.Editor.GetVisualValue
    else
      ValueComboBox.Text:=CurRow.Editor.GetVisualValue;
  end;
end;

procedure TOICustomPropertyGrid.RefreshValueEdit;
var
  CurRow: TOIPropertyGridRow;
  NewValue, OldValue: string;
begin
  if (FStates*[pgsChangingItemIndex,pgsApplyingValue]=[])
  and (FCurrentEdit<>nil)
  and (FItemIndex>=0) and (FItemIndex<FRows.Count) then begin
    CurRow:=Rows[FItemIndex];
    if FCurrentEdit=ValueEdit then
      OldValue:=ValueEdit.Text
    else
      OldValue:=ValueComboBox.Text;
    NewValue:=CurRow.Editor.GetVisualValue;
    if OldValue<>NewValue then begin
      if FCurrentEdit=ValueEdit then
        ValueEdit.Text:=NewValue
      else
        ValueComboBox.Text:=NewValue;
    end;
  end;
end;

procedure TOICustomPropertyGrid.ValueEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  HandleStandardKeys(Key,Shift);
end;

procedure TOICustomPropertyGrid.ValueEditKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  HandleKeyUp(Key,Shift);
end;

procedure TOICustomPropertyGrid.ValueEditExit(Sender: TObject);
begin
  SetRowValue;
end;

procedure TOICustomPropertyGrid.ValueEditChange(Sender: TObject);
var CurRow:TOIPropertyGridRow;
begin
  if (FCurrentEdit<>nil) and (FItemIndex>=0) and (FItemIndex<FRows.Count) then
  begin
    CurRow:=Rows[FItemIndex];
    if paAutoUpdate in CurRow.Editor.GetAttributes then
      SetRowValue;
  end;
end;

procedure TOICustomPropertyGrid.ValueComboBoxExit(Sender: TObject);
begin
  SetRowValue;
end;

procedure TOICustomPropertyGrid.ValueComboBoxChange(Sender: TObject);
var i:integer;
begin
  i:=TComboBox(Sender).Items.IndexOf(TComboBox(Sender).Text);
  if i>=0 then SetRowValue;
end;

procedure TOICustomPropertyGrid.ValueComboBoxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  HandleStandardKeys(Key,Shift);
end;

procedure TOICustomPropertyGrid.ValueComboBoxKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  HandleKeyUp(Key,Shift);
end;

procedure TOICustomPropertyGrid.ValueButtonClick(Sender: TObject);
begin
  DoCallEdit;
end;

procedure TOICustomPropertyGrid.SetItemIndex(NewIndex:integer);
var NewRow:TOIPropertyGridRow;
  NewValue:string;
begin
  if (FStates*[pgsChangingItemIndex,pgsApplyingValue]<>[])
  or (FItemIndex=NewIndex) then
    exit;
    
  // save old edit value
  SetRowValue;
  
  Include(FStates,pgsChangingItemIndex);
  if (FItemIndex>=0) and (FItemIndex<FRows.Count) then
    Rows[FItemIndex].Editor.Deactivate;
  SetCaptureControl(nil);
  
  FItemIndex:=NewIndex;
  if FCurrentEdit<>nil then begin
    FCurrentEdit.Visible:=false;
    FCurrentEdit.Enabled:=false;
    FCurrentEdit:=nil;
  end;
  if FCurrentButton<>nil then begin
    FCurrentButton.Visible:=false;
    FCurrentButton.Enabled:=false;
    FCurrentButton:=nil;
  end;
  FCurrentEditorLookupRoot:=nil;
  if (NewIndex>=0) and (NewIndex<FRows.Count) then begin
    NewRow:=Rows[NewIndex];
    if NewRow.Bottom>=TopY+(ClientHeight-2*BorderWidth) then
      TopY:=NewRow.Bottom-(ClientHeight-2*BorderWidth)+1
    else if NewRow.Top<TopY then
      TopY:=NewRow.Top;
    NewRow.Editor.Activate;
    if paDialog in NewRow.Editor.GetAttributes then begin
      FCurrentButton:=ValueButton;
      FCurrentButton.Visible:=true;
    end;
    NewValue:=NewRow.Editor.GetVisualValue;
    if paValueList in NewRow.Editor.GetAttributes then begin
      FCurrentEdit:=ValueComboBox;
      ValueComboBox.MaxLength:=NewRow.Editor.GetEditLimit;
      ValueComboBox.Items.BeginUpdate;
      ValueComboBox.Items.Text:='';
      ValueComboBox.Items.Clear;
      ValueComboBox.Sorted:=paSortList in NewRow.Editor.GetAttributes;
      ValueComboBox.Enabled:=not NewRow.IsReadOnly;
      NewRow.Editor.GetValues(@AddStringToComboBox);
      ValueComboBox.Text:=NewValue;
      ValueComboBox.Items.EndUpdate;
    end else begin
      FCurrentEdit:=ValueEdit;
      ValueEdit.ReadOnly:=NewRow.IsReadOnly;
      ValueEdit.Enabled:=true;
      ValueEdit.MaxLength:=NewRow.Editor.GetEditLimit;
      ValueEdit.Text:=NewValue;
    end;
    AlignEditComponents;
    if FCurrentEdit<>nil then begin
      if FPropertyEditorHook<>nil then
        FCurrentEditorLookupRoot:=FPropertyEditorHook.LookupRoot;
      FCurrentEdit.Visible:=true;
      if (FDragging=false) and (FCurrentEdit.Showing)
      and FCurrentEdit.Enabled
      and (not NewRow.IsReadOnly) then begin
        FCurrentEdit.SetFocus;
      end;
    end;
    if FCurrentButton<>nil then
      FCurrentButton.Enabled:=not NewRow.IsDisabled;
  end;
  Exclude(FStates,pgsChangingItemIndex);
end;

function TOICustomPropertyGrid.GetRowCount:integer;
begin
  Result:=FRows.Count;
end;

procedure TOICustomPropertyGrid.BuildPropertyList;
var a:integer;
  CurRow:TOIPropertyGridRow;
  OldSelectedRowPath:string;
begin
  OldSelectedRowPath:=PropertyPath(ItemIndex);
  // unselect
  ItemIndex:=-1;
  // clear
  for a:=0 to FRows.Count-1 do Rows[a].Free;
  FRows.Clear;
  // get properties
  GetPersistentProperties(FSelection, FFilter, FPropertyEditorHook,
    @AddPropertyEditor,nil);
  // sort
  FRows.Sort(@SortGridRows);
  for a:=0 to FRows.Count-1 do begin
    if a>0 then
      Rows[a].FPriorBrother:=Rows[a-1]
    else
      Rows[a].FPriorBrother:=nil;
    if a<FRows.Count-1 then
      Rows[a].FNextBrother:=Rows[a+1]
    else
      Rows[a].FNextBrother:=nil;
  end;
  // set indices and tops
  SetItemsTops;
  // restore expands
  for a:=FExpandedProperties.Count-1 downto 0 do begin
    CurRow:=GetRowByPath(FExpandedProperties[a]);
    if CurRow<>nil then
      ExpandRow(CurRow.Index);
  end;
  // reselect
  CurRow:=GetRowByPath(OldSelectedRowPath);
  if CurRow<>nil then begin
    ItemIndex:=CurRow.Index;
  end;
  // update scrollbar
  FTopY:=0;
  UpdateScrollBar;
  // paint
  Invalidate;
end;

procedure TOICustomPropertyGrid.AddPropertyEditor(PropEditor: TPropertyEditor);
var NewRow:TOIPropertyGridRow;
begin
  NewRow:=TOIPropertyGridRow.Create(Self,PropEditor,nil);
  FRows.Add(NewRow);
  if FRows.Count>1 then begin
    NewRow.FPriorBrother:=Rows[FRows.Count-2];
    NewRow.FPriorBrother.FNextBrother:=NewRow;
  end;
end;

procedure TOICustomPropertyGrid.AddStringToComboBox(const s:string);
var NewIndex:integer;
begin
  NewIndex:=ValueComboBox.Items.Add(s);
  if ValueComboBox.Text=s then
    ValueComboBox.ItemIndex:=NewIndex;
end;

procedure TOICustomPropertyGrid.ExpandRow(Index:integer);
var a:integer;
  CurPath:string;
  AlreadyInExpandList:boolean;
begin
  FExpandingRow:=Rows[Index];
  if (FExpandingRow.Expanded)
  or (not (paSubProperties in FExpandingRow.Editor.GetAttributes))
  then begin
    FExpandingRow:=nil;
    exit;
  end;
  FExpandingRow.Editor.GetProperties(@AddSubEditor);
  SetItemsTops;
  FExpandingRow.FExpanded:=true;
  a:=0;
  CurPath:=uppercase(PropertyPath(FExpandingRow.Index));
  AlreadyInExpandList:=false;
  while a<FExpandedProperties.Count do begin
    if FExpandedProperties[a]=copy(CurPath,1,length(FExpandedProperties[a]))
    then begin
      if length(FExpandedProperties[a])=length(CurPath) then begin
        AlreadyInExpandList:=true;
        inc(a);
      end else begin
        FExpandedProperties.Delete(a);
      end;
    end else begin
      inc(a);
    end;
  end;
  if not AlreadyInExpandList then
    FExpandedProperties.Add(CurPath);
  FExpandingRow:=nil;
  UpdateScrollBar;
  Invalidate;
end;

procedure TOICustomPropertyGrid.ShrinkRow(Index:integer);
var CurRow, ARow:TOIPropertyGridRow;
  StartIndex,EndIndex,a:integer;
  CurPath:string;
begin
  CurRow:=Rows[Index];
  if (not CurRow.Expanded) then exit;
  // calculate all childs (between StartIndex..EndIndex)
  StartIndex:=CurRow.Index+1;
  EndIndex:=FRows.Count-1;
  ARow:=CurRow;
  while ARow<>nil do begin
    if ARow.NextBrother<>nil then begin
      EndIndex:=ARow.NextBrother.Index-1;
      break;
    end;
    ARow:=ARow.Parent;
  end;
  if (FItemIndex>=StartIndex) and (FItemIndex<=EndIndex) then
    ItemIndex:=0;
  for a:=EndIndex downto StartIndex do begin
    Rows[a].Free;
    FRows.Delete(a);
  end;
  SetItemsTops;
  CurRow.FExpanded:=false;
  CurPath:=uppercase(PropertyPath(CurRow.Index));
  a:=0;
  while a<FExpandedProperties.Count do begin
    if copy(FExpandedProperties[a],1,length(CurPath))=CurPath then
      FExpandedProperties.Delete(a)
    else
      inc(a);
  end;
  if CurRow.Parent<>nil then
    FExpandedProperties.Add(PropertyPath(CurRow.Parent.Index));
  UpdateScrollBar;
  Invalidate;
end;

procedure TOICustomPropertyGrid.AddSubEditor(PropEditor:TPropertyEditor);
var NewRow:TOIPropertyGridRow;
  NewIndex:integer;
begin
  NewRow:=TOIPropertyGridRow.Create(Self,PropEditor,FExpandingRow);
  NewIndex:=FExpandingRow.Index+1+FExpandingRow.ChildCount;
  FRows.Insert(NewIndex,NewRow);
  if FExpandingRow.FFirstChild=nil then
    FExpandingRow.FFirstChild:=NewRow;
  NewRow.FPriorBrother:=FExpandingRow.FLastChild;
  FExpandingRow.FLastChild:=NewRow;
  if NewRow.FPriorBrother<>nil then
    NewRow.FPriorBrother.FNextBrother:=NewRow;
  inc(FExpandingRow.FChildCount);
end;

function TOICustomPropertyGrid.MouseToIndex(y:integer;MustExist:boolean):integer;
var l,r,m:integer;
begin
  l:=0;
  r:=FRows.Count-1;
  inc(y,FTopY);
  while (l<=r) do begin
    m:=(l+r) shr 1;
    if Rows[m].Top>y then begin
      r:=m-1;
    end else if Rows[m].Bottom<=y then begin
      l:=m+1;
    end else begin
      Result:=m;  exit;
    end;
  end;
  if (MustExist=false) and (FRows.Count>0) then begin
    if y<0 then Result:=0
    else Result:=FRows.Count-1;
  end else Result:=-1;
end;

function TOICustomPropertyGrid.GetActiveRow: TOIPropertyGridRow;
begin
  Result:=nil;
  if ItemIndex<0 then exit;
  Result:=Rows[ItemIndex];
end;

procedure TOICustomPropertyGrid.SetCurrentRowValue(const NewValue: string);
begin
  if not CanEditRowValue then exit;
  if FCurrentEdit is TComboBox then
    TComboBox(FCurrentEdit).Text:=NewValue
  else if FCurrentEdit is TEdit then
    TEdit(FCurrentEdit).Text:=NewValue;
  SetRowValue;
end;

function TOICustomPropertyGrid.CanEditRowValue: boolean;
begin
  if (FStates*[pgsChangingItemIndex,pgsApplyingValue]<>[])
  or (FCurrentEdit=nil)
  or (FItemIndex<0)
  or (FItemIndex>=FRows.Count)
  or ((FCurrentEditorLookupRoot<>nil)
    and (FPropertyEditorHook<>nil)
    and (FPropertyEditorHook.LookupRoot<>FCurrentEditorLookupRoot))
  then begin
    Result:=false;
  end else begin
    Result:=true;
  end;
end;

function TOICustomPropertyGrid.GetHintTypeAt(RowIndex: integer; X: integer
  ): TPropEditHint;
var
  IconX: integer;
begin
  Result:=pehNone;
  if (RowIndex<0) or (RowIndex>=RowCount) then exit;
  if SplitterX>=X then begin
    if (FCurrentButton<>nil)
    and (FCurrentButton.Left<=X) then
      Result:=pehEditButton
    else
      Result:=pehValue;
  end else begin
    IconX:=GetTreeIconX(RowIndex);
    if IconX+Indent>X then
      Result:=pehTree
    else
      Result:=pehName;
  end;
end;

procedure TOICustomPropertyGrid.MouseDown(Button:TMouseButton;  Shift:TShiftState;
  X,Y:integer);
begin
  //ShowMessageDialog('X'+IntToStr(X)+',Y'+IntToStr(Y));
  inherited MouseDown(Button,Shift,X,Y);

  //hide the hint
  FHintWindow.Visible := False;
  
  if Button=mbLeft then begin
    if Cursor=crHSplit then begin
      FDragging:=true;
    end;
  end;
end;

procedure TOICustomPropertyGrid.MouseMove(Shift:TShiftState;  X,Y:integer);
var
  SplitDistance:integer;
  Index: Integer;
  fPropRow: TOIPropertyGridRow;
  fHint: String;
  fpoint: TPoint;
  fHintRect: TRect;
begin
  inherited MouseMove(Shift,X,Y);
  SplitDistance:=X-SplitterX;
  if FDragging then begin
    if ssLeft in Shift then begin
      SplitterX:=SplitterX+SplitDistance;
    end else begin
      EndDragSplitter;
    end;
  end else begin
    if (abs(SplitDistance)<=2) then begin
      Cursor:=crHSplit;
    end else begin
      Cursor:=crDefault;
    end;
    // to check if the property text fits in its box, if not show a hint
    if ShowHint then begin
      Index := MouseToIndex(y,false);
      if Index > -1 then begin
        fPropRow := GetRow(Index);
        if X < SplitterX then begin
          // Mouse is over property name...
          fHint := fPropRow.Name;
          if (Canvas.TextWidth(fHint)+BorderWidth+GetTreeIconX(Index)+Indent)
          >= SplitterX
          then begin
            fHintRect := FHintWindow.CalcHintRect(0,fHint,nil);
            fpoint := ClientToScreen(
                                   Point(BorderWidth+GetTreeIconX(Index)+Indent,
                                   fPropRow.Top - TopY-1));
            MoveRect(fHintRect,fPoint.x,fPoint.y);
            FHintWindow.ActivateHint(fHintRect,fHint);
          end;
        end
        else begin
          // Mouse is over property value...
          fHint := fPropRow.LastPaintedValue;
          if length(fHint)>100 then fHint:=copy(fHint,1,100)+'...';
          if Canvas.TextWidth(fHint) > (ClientWidth - BorderWidth - SplitterX)
          then begin
            fHintRect := FHintWindow.CalcHintRect(0,fHint,nil);
            fpoint := ClientToScreen(Point(SplitterX,fPropRow.Top - TopY-1));
            MoveRect(fHintRect,fPoint.x,fPoint.y);
            FHintWindow.ActivateHint(fHintRect,fHint);
          end;
        end;
      end;
    end;
  end;
end;

procedure TOICustomPropertyGrid.MouseUp(Button:TMouseButton;  Shift:TShiftState;
  X,Y:integer);
var
  IconX,Index:integer;
  PointedRow:TOIpropertyGridRow;
  WasDragging: boolean;
begin
  WasDragging:=FDragging;
  if FDragging then EndDragSplitter;
  inherited MouseUp(Button,Shift,X,Y);

  if Button=mbLeft then begin
    if not WasDragging then begin
      Index:=MouseToIndex(Y,false);
      if (Index>=0) and (Index<FRows.Count) then begin
        IconX:=GetTreeIconX(Index);
        if (X>=IconX) and (X<=IconX+FIndent) then begin
          PointedRow:=Rows[Index];
          if paSubProperties in PointedRow.Editor.GetAttributes then begin
            if PointedRow.Expanded then
              ShrinkRow(Index)
            else
              ExpandRow(Index);
            ItemIndex:=Index;
          end;
        end else begin
          ItemIndex:=Index;
        end;
      end;
    end;
  end;
end;

procedure TOICustomPropertyGrid.KeyDown(var Key: Word; Shift: TShiftState);
begin
  HandleStandardKeys(Key,Shift);
  inherited KeyDown(Key, Shift);
end;

procedure TOICustomPropertyGrid.HandleStandardKeys(var Key: Word; Shift: TShiftState
  );
var
  Handled: Boolean;
begin
  Handled:=true;
  case Key of
  
  VK_UP:
    if (ItemIndex>0) then ItemIndex:=ItemIndex-1;

  VK_Down:
    if (ItemIndex<FRows.Count-1) then ItemIndex:=ItemIndex+1;
    
  VK_TAB:
    // ToDo: implement completion
    if (ItemIndex<FRows.Count-1) then ItemIndex:=ItemIndex+1;
    
  VK_LEFT:
    if (FCurrentEdit=nil)
    and (ItemIndex>=0) and (Rows[ItemIndex].Expanded) then
      ShrinkRow(ItemIndex)
    else
      Handled:=false;
    
  VK_RIGHT:
    if (FCurrentEdit=nil)
    and (ItemIndex>=0) and (not Rows[ItemIndex].Expanded)
    and (paSubProperties in Rows[ItemIndex].Editor.GetAttributes) then
      ExpandRow(ItemIndex)
    else
      Handled:=false;

  VK_RETURN:
    SetRowValue;

  else
    Handled:=false;
  end;
  if Handled then Key:=VK_UNKNOWN;
end;

procedure TOICustomPropertyGrid.HandleKeyUp(var Key: Word; Shift: TShiftState);
begin
  if (Key<>VK_UNKNOWN) and Assigned(OnKeyUp) then OnKeyUp(Self,Key,Shift);
end;

procedure TOICustomPropertyGrid.EraseBackground(DC: HDC);
begin
  // everything is painted, so erasing the background is not needed
end;

procedure TOICustomPropertyGrid.DoSetBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  inherited DoSetBounds(ALeft, ATop, AWidth, AHeight);
  UpdateScrollBar;
end;

constructor TOICustomPropertyGrid.Create(TheOwner: TComponent);
begin
  CreateWithParams(TheOwner,nil,AllTypeKinds,25);
end;

procedure TOICustomPropertyGrid.OnUserInput(Sender: TObject; Msg: Cardinal);
begin
  ResetHintTimer;
end;

procedure TOICustomPropertyGrid.EndDragSplitter;
begin
  if FDragging then begin
    Cursor:=crDefault;
    FDragging:=false;
    FPreferredSplitterX:=FSplitterX;
    if FCurrentEdit<>nil then begin
      SetCaptureControl(nil);
      FCurrentEdit.SetFocus;
    end;
  end;
end;

procedure TOICustomPropertyGrid.SetSplitterX(const NewValue:integer);
var AdjustedValue:integer;
begin
  AdjustedValue:=NewValue;
  if AdjustedValue>ClientWidth then AdjustedValue:=ClientWidth;
  if AdjustedValue<1 then AdjustedValue:=1;
  if FSplitterX<>AdjustedValue then begin
    FSplitterX:=AdjustedValue;
    AlignEditComponents;
    Invalidate;
  end;
end;

procedure TOICustomPropertyGrid.SetTopY(const NewValue:integer);
begin
  if FTopY<>NewValue then begin
    FTopY:=NewValue;
    if FTopY<0 then FTopY:=0;
    UpdateScrollBar;
    ItemIndex:=-1;
    Invalidate;
  end;
end;

procedure TOICustomPropertyGrid.SetBounds(aLeft,aTop,aWidth,aHeight:integer);
begin
//writeln('[TOICustomPropertyGrid.SetBounds] ',Name,' ',aLeft,',',aTop,',',aWidth,',',aHeight,' Visible=',Visible);
  inherited SetBounds(aLeft,aTop,aWidth,aHeight);
  if Visible then begin
    if not FDragging then begin
      if (SplitterX<5) and (aWidth>20) then
        SplitterX:=100
      else
        SplitterX:=FPreferredSplitterX;
    end;
    AlignEditComponents;
  end;
end;

function TOICustomPropertyGrid.GetTreeIconX(Index:integer):integer;
begin
  Result:=Rows[Index].Lvl*Indent+2;
end;

function TOICustomPropertyGrid.TopMax:integer;
begin
  Result:=GridHeight-ClientHeight+2*BorderWidth;
  if Result<0 then Result:=0;
end;

function TOICustomPropertyGrid.GridHeight:integer;
begin
  if FRows.Count>0 then
    Result:=Rows[FRows.Count-1].Bottom
  else
    Result:=0;
end;

procedure TOICustomPropertyGrid.AlignEditComponents;
var RRect,EditCompRect,EditBtnRect:TRect;

  function CompareRectangles(r1,r2:TRect):boolean;
  begin
    Result:=(r1.Left=r2.Left) and (r1.Top=r2.Top) and (r1.Right=r2.Right)
       and (r1.Bottom=r2.Bottom);
  end;

// AlignEditComponents
begin
  if ItemIndex>=0 then begin
    RRect:=RowRect(ItemIndex);
    EditCompRect:=RRect;
    EditCompRect.Top:=EditCompRect.Top-1;
    EditCompRect.Left:=RRect.Left+SplitterX;
    if FCurrentButton<>nil then begin
      // edit dialog button
      with EditBtnRect do begin
        Top:=RRect.Top;
        Left:=RRect.Right-20;
        Bottom:=RRect.Bottom;
        Right:=RRect.Right;
        EditCompRect.Right:=Left;
      end;
      if not CompareRectangles(FCurrentButton.BoundsRect,EditBtnRect) then begin
        FCurrentButton.BoundsRect:=EditBtnRect;
        //FCurrentButton.Invalidate;
      end;
    end;
    if FCurrentEdit<>nil then begin
      // resize the edit component
      EditCompRect.Left:=EditCompRect.Left-1;
      if not CompareRectangles(FCurrentEdit.BoundsRect,EditCompRect) then begin
        FCurrentEdit.BoundsRect:=EditCompRect;
        FCurrentEdit.Invalidate;
      end;
    end;
  end;
end;

procedure TOICustomPropertyGrid.PaintRow(ARow:integer);
var ARowRect,NameRect,NameIconRect,NameTextRect,ValueRect:TRect;
  IconX,IconY:integer;
  CurRow:TOIPropertyGridRow;
  DrawState:TPropEditDrawState;
  OldFont:TFont;

  procedure DrawTreeIcon(x,y:integer;Plus:boolean);
  begin
    with Canvas do begin
      Brush.Color:=clWhite;
      Pen.Color:=clBlack;
      Rectangle(x,y,x+8,y+8);
      MoveTo(x+2,y+4);
      LineTo(x+7,y+4);
      if Plus then begin
        MoveTo(x+4,y+2);
        LineTo(x+4,y+7);
      end;
    end;
  end;

// PaintRow
begin
  CurRow:=Rows[ARow];
  ARowRect:=RowRect(ARow);
  NameRect:=ARowRect;
  ValueRect:=ARowRect;
  NameRect.Right:=SplitterX;
  ValueRect.Left:=SplitterX;
  IconX:=GetTreeIconX(ARow);
  IconY:=((NameRect.Bottom-NameRect.Top-9) div 2)+NameRect.Top;
  NameIconRect:=NameRect;
  NameIconRect.Right:=IconX+Indent;
  NameTextRect:=NameRect;
  NameTextRect.Left:=NameIconRect.Right;
  DrawState:=[];
  if ARow=FItemIndex then Include(DrawState,pedsSelected);
  with Canvas do begin
    // draw name background
    if FBackgroundColor<>clNone then begin
      Brush.Color:=FBackgroundColor;
      FillRect(NameIconRect);
      FillRect(NameTextRect);
    end;
    // draw icon
    if paSubProperties in CurRow.Editor.GetAttributes then begin
      DrawTreeIcon(IconX,IconY,not CurRow.Expanded);
    end;
    // draw name
    OldFont:=Font;
    Font:=FNameFont;
    CurRow.Editor.PropDrawName(Canvas,NameTextRect,DrawState);
    Font:=OldFont;
    // draw frame
    Pen.Color:=cl3DDkShadow;
    MoveTo(NameRect.Left,NameRect.Bottom-1);
    LineTo(NameRect.Right-1,NameRect.Bottom-1);
    LineTo(NameRect.Right-1,NameRect.Top-1);
    if ARow=FItemIndex then begin
      Pen.Color:=cl3DDkShadow;
      MoveTo(NameRect.Left,NameRect.Bottom-1);
      LineTo(NameRect.Left,NameRect.Top);
      LineTo(NameRect.Right-1,NameRect.Top);
      Pen.Color:=cl3DLight;
      MoveTo(NameRect.Left+1,NameRect.Bottom-2);
      LineTo(NameRect.Right-1,NameRect.Bottom-2);
    end;
    // draw value background
    if FBackgroundColor<>clNone then begin
      Brush.Color:=FBackgroundColor;
      FillRect(ValueRect);
    end;
    // draw value
    if ARow<>ItemIndex then begin
      OldFont:=Font;
      if CurRow.Editor.IsNotDefaultValue then
        Font:=FValueFont
      else
        Font:=FDefaultValueFont;
      CurRow.Editor.PropDrawValue(Canvas,ValueRect,DrawState);
      Font:=OldFont;
    end;
    CurRow.LastPaintedValue:=CurRow.Editor.GetVisualValue;
    // draw frame
    Pen.Color:=cl3DDkShadow;
    MoveTo(ValueRect.Left-1,ValueRect.Bottom-1);
    LineTo(ValueRect.Right,ValueRect.Bottom-1);
    Pen.Color:=cl3DLight;
    MoveTo(ValueRect.Left,ValueRect.Bottom-1);
    LineTo(ValueRect.Left,ValueRect.Top);
    if ARow=FItemIndex then begin
      MoveTo(ValueRect.Left,ValueRect.Bottom-2);
      LineTo(ValueRect.Right,ValueRect.Bottom-2);
    end;
  end;
end;

procedure TOICustomPropertyGrid.DoPaint(PaintOnlyChangedValues:boolean);
var a:integer;
  SpaceRect:TRect;
begin
  if not PaintOnlyChangedValues then begin
    with Canvas do begin
      // draw properties
      for a:=0 to FRows.Count-1 do begin
        PaintRow(a);
      end;
      // draw unused space below rows
      SpaceRect:=Rect(BorderWidth,BorderWidth,
                      ClientWidth-BorderWidth+1,ClientHeight-BorderWidth+1);
      if FRows.Count>0 then
        SpaceRect.Top:=Rows[FRows.Count-1].Bottom-FTopY+BorderWidth;
// TWinControl(Parent).InvalidateRect(Self,SpaceRect,true);
      if FBackgroundColor<>clNone then begin
        Brush.Color:=FBackgroundColor;
        FillRect(SpaceRect);
      end;
      // don't draw border: borderstyle=bsSingle
    end;
  end else begin
    for a:=0 to FRows.Count-1 do begin
      if Rows[a].Editor.GetVisualValue<>Rows[a].LastPaintedValue then
        PaintRow(a);
    end;
  end;
end;

procedure TOICustomPropertyGrid.Paint;
begin
  inherited Paint;
  DoPaint(false);
end;

procedure TOICustomPropertyGrid.RefreshPropertyValues;
begin
  RefreshValueEdit;
  DoPaint(true);
end;

procedure TOICustomPropertyGrid.PropEditLookupRootChange;
begin
  // When the LookupRoot changes, no changes can be stored
  // -> undo the value editor changes
  RefreshValueEdit;
end;

function TOICustomPropertyGrid.RowRect(ARow:integer):TRect;
begin
  Result.Left:=BorderWidth;
  Result.Top:=Rows[ARow].Top-FTopY+BorderWidth;
  Result.Right:=ClientWidth-ScrollBarWidth;
  Result.Bottom:=Rows[ARow].Bottom-FTopY+BorderWidth;
end;

procedure TOICustomPropertyGrid.SetItemsTops;
// compute row tops from row heights
// set indices of all rows
var a,scrollmax:integer;
begin
  for a:=0 to FRows.Count-1 do begin
    Rows[a].Index:=a;
    Rows[a].MeasureHeight(Canvas);
  end;
  if FRows.Count>0 then
    Rows[0].Top:=0;
  for a:=1 to FRows.Count-1 do
    Rows[a].FTop:=Rows[a-1].Bottom;
  if FRows.Count>0 then
    scrollmax:=Rows[FRows.Count-1].Bottom-Height
  else
    scrollmax:=0;
  // always show something
  if scrollmax<10 then scrollmax:=10;
end;

procedure TOICustomPropertyGrid.ClearRows;
var a:integer;
begin
  IncreaseChangeStep;
  for a:=0 to FRows.Count-1 do begin
    Rows[a].Free;
  end;
  FRows.Clear;
end;

function TOICustomPropertyGrid.GetCurrentEditValue: string;
begin
  if FCurrentEdit=ValueEdit then
    Result:=ValueEdit.Text
  else if FCurrentEdit=ValueComboBox then
    Result:=ValueComboBox.Text
  else
    Result:='';
end;

procedure TOICustomPropertyGrid.SetCurrentEditValue(const AValue: string);
begin
  if FCurrentEdit=ValueEdit then
    ValueEdit.Text:=AValue
  else if FCurrentEdit=ValueComboBox then
    ValueComboBox.Text:=AValue;
end;

procedure TOICustomPropertyGrid.Clear;
begin
  ClearRows;
end;

function TOICustomPropertyGrid.GetRow(Index:integer):TOIPropertyGridRow;
begin
  Result:=TOIPropertyGridRow(FRows[Index]);
end;

procedure TOICustomPropertyGrid.ValueComboBoxCloseUp(Sender: TObject);
begin
  SetRowValue;
end;

procedure TOICustomPropertyGrid.ValueComboBoxDropDown(Sender: TObject);
var
  CurRow: TOIPropertyGridRow;
  MaxItemWidth, CurItemWidth, i, Cnt: integer;
  ItemValue: string;
begin
  if (FItemIndex>=0) and (FItemIndex<FRows.Count) then begin
    CurRow:=Rows[FItemIndex];
    MaxItemWidth:=ValueComboBox.Width;
    Cnt:=ValueComboBox.Items.Count;
    for i:=0 to Cnt-1 do begin
      ItemValue:=ValueComboBox.Items[i];
      CurItemWidth:=ValueComboBox.Canvas.TextWidth(ItemValue);
      CurRow.Editor.ListMeasureWidth(ItemValue,i,ValueComboBox.Canvas,
                                     CurItemWidth);
      if MaxItemWidth<CurItemWidth then
        MaxItemWidth:=CurItemWidth;
    end;
    ValueComboBox.ItemWidth:=MaxItemWidth;
  end;
end;

procedure TOICustomPropertyGrid.ValueComboBoxDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  CurRow: TOIPropertyGridRow;
  ItemValue: string;
  AState: TPropEditDrawState;
begin
  if (FItemIndex>=0) and (FItemIndex<FRows.Count) then begin
    CurRow:=Rows[FItemIndex];
    if (Index>=0) and (Index<ValueComboBox.Items.Count) then
      ItemValue:=ValueComboBox.Items[Index]
    else
      ItemValue:='';
    AState:=[];
    if odPainted in State then Include(AState,pedsPainted);
    if odSelected in State then Include(AState,pedsSelected);
    if odFocused in State then Include(AState,pedsFocused);
    if odComboBoxEdit in State then
      Include(AState,pedsInEdit)
    else
      Include(AState,pedsInComboList);
      
    // clear background
    with ValueComboBox.Canvas do begin
      Brush.Color:=clWhite;
      Pen.Color:=clBlack;
      Font.Color:=Pen.Color;
      FillRect(ARect);
    end;
    CurRow.Editor.ListDrawValue(ItemValue,Index,ValueComboBox.Canvas,ARect,
                                AState);
  end;
end;

Procedure TOICustomPropertyGrid.HintTimer(sender : TObject);
var
  Rect : TRect;
  AHint : String;
  Position : TPoint;
  Index: integer;
  PointedRow:TOIpropertyGridRow;
  Window: TWinControl;
  HintType: TPropEditHint;
begin
  // ToDo: use LCL hintsystem
  FHintTimer.Enabled := False;
  if not ShowHint then exit;

  Position := Mouse.CursorPos;

  Position := ScreenToClient(Position);
  if ((Position.X <=0) or (Position.X >= Width) or (Position.Y <= 0)
  or (Position.Y >= Height)) then
    Exit;

  Window := FindLCLWindow(Position);
  if not(Assigned(Window)) then Exit;
  If (Window<>Self) and (not IsParentOf(Window)) then exit;

  AHint := '';
  Index:=MouseToIndex(Position.Y,false);
  if (Index>=0) and (Index<FRows.Count) then
  begin
    //IconX:=GetTreeIconX(Index);
    PointedRow:=Rows[Index];
    if Assigned(PointedRow) then
    Begin
      if Assigned(PointedRow.Editor) then begin
        HintType := GetHintTypeAt(Index,Position.X);
        AHint := PointedRow.Editor.GetHint(HintType,Position.X,Position.Y);
      end;
    end;
  end;
  if AHint = '' then Exit;
  Rect := FHintWindow.CalcHintRect(0,AHint,nil);  //no maxwidth
  Position := Mouse.CursorPos;
  Rect.Left := Position.X+10;
  Rect.Top := Position.Y+10;
  Rect.Right := Rect.Left + Rect.Right+3;
  Rect.Bottom := Rect.Top + Rect.Bottom+3;

  FHintWindow.ActivateHint(Rect,AHint);
end;

Procedure TOICustomPropertyGrid.ResetHintTimer;
begin
  if FHintWIndow.Visible then
    FHintWindow.Visible := False;
     
  FHintTimer.Enabled := False;
  if RowCount > 0 then
    FHintTimer.Enabled := not FDragging;
end;

procedure TOICustomPropertyGrid.ValueEditMouseDown(Sender : TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  //hide the hint window!
  if FHintWindow.Visible then FHintWindow.Visible := False;
end;

procedure TOICustomPropertyGrid.IncreaseChangeStep;
begin
  if FChangeStep<>$7fffffff then
    inc(FChangeStep)
  else
    FChangeStep:=-$7fffffff;
end;

PRocedure TOICustomPropertyGrid.ValueEditDblClick(Sender : TObject);
var
  CurRow: TOIPropertyGridRow;
  TypeKind : TTypeKind;
  CurValue: string;
begin
  if (FStates*[pgsChangingItemIndex,pgsApplyingValue]<>[])
  or (FCurrentEdit=nil)
  or (FItemIndex<0)
  or (FItemIndex>=FRows.Count)
  or ((FCurrentEditorLookupRoot<>nil)
    and (FPropertyEditorHook<>nil)
    and (FPropertyEditorHook.LookupRoot<>FCurrentEditorLookupRoot))
  then begin
    exit;
  end;

  FHintTimer.Enabled := False;

  if FCurrentEdit=ValueEdit then
    CurValue:=ValueEdit.Text
  else
    CurValue:=ValueComboBox.Text;
  if CurValue='' then begin
    DoCallEdit;
    exit;
  end;

  if (FCurrentEdit=ValueComboBox) then Begin
    //either an Event or an enumeration or Boolean
    CurRow:=Rows[FItemIndex];
    TypeKind := CurRow.Editor.GetPropType^.Kind;
    if TypeKind in [tkEnumeration,tkBool] then begin
      // set value to next value in list
      if ValueComboBox.Items.Count = 0 then Exit;
      if ValueComboBox.ItemIndex < (ValueComboBox.Items.Count-1) then
        ValueComboBox.ItemIndex := ValueComboBox.ItemIndex +1
      else
        ValueComboBox.ItemIndex := 0;
    end else if TypeKind=tkMethod then begin
      DoCallEdit;
    end;
  end;
end;

procedure TOICustomPropertyGrid.SetBackgroundColor(const AValue: TColor);
begin
  if FBackgroundColor=AValue then exit;
  FBackgroundColor:=AValue;
  Invalidate;
end;

//------------------------------------------------------------------------------

{ TOIPropertyGridRow }

constructor TOIPropertyGridRow.Create(PropertyTree: TOICustomPropertyGrid;
  PropEditor:TPropertyEditor; ParentNode:TOIPropertyGridRow);
begin
  inherited Create;
  // tree pointer
  FTree:=PropertyTree;
  FParent:=ParentNode;
  FNextBrother:=nil;
  FPriorBrother:=nil;
  FExpanded:=false;
  // child nodes
  FChildCount:=0;
  FFirstChild:=nil;
  FLastChild:=nil;
  // director
  FEditor:=PropEditor;
  GetLvl;
  FName:=FEditor.GetName;
  FTop:=0;
  FHeight:=FTree.DefaultItemHeight;
  Index:=-1;
  LastPaintedValue:='';
end;

destructor TOIPropertyGridRow.Destroy;
begin
  if FPriorBrother<>nil then FPriorBrother.FNextBrother:=FNextBrother;
  if FNextBrother<>nil then FNextBrother.FPriorBrother:=FPriorBrother;
  if FParent<>nil then begin
    if FParent.FFirstChild=Self then FParent.FFirstChild:=FNextBrother;
    if FParent.FLastChild=Self then FParent.FLastChild:=FPriorBrother;
    dec(FParent.FChildCount);
  end;
  if FEditor<>nil then FEditor.Free;
  inherited Destroy;
end;

function TOIPropertyGridRow.ConsistencyCheck: integer;
var
  OldLvl, RealChildCount: integer;
  AChild: TOIPropertyGridRow;
begin
  if Top<0 then begin
    Result:=-1;
    exit;
  end;
  if Height<0 then begin
    Result:=-2;
    exit;
  end;
  if Lvl<0 then begin
    Result:=-3;
    exit;
  end;
  OldLvl:=Lvl;
  GetLvl;
  if Lvl<>OldLvl then begin
    Result:=-4;
    exit;
  end;
  if Name='' then begin
    Result:=-5;
    exit;
  end;
  if NextBrother<>nil then begin
    if NextBrother.PriorBrother<>Self then begin
      Result:=-6;
      exit;
    end;
    if NextBrother.Index<Index+1 then begin
      Result:=-7;
      exit;
    end;
  end;
  if PriorBrother<>nil then begin
    if PriorBrother.NextBrother<>Self then begin
      Result:=-8;
      exit;
    end;
    if PriorBrother.Index>Index-1 then begin
      Result:=-9
    end;
  end;
  if (Parent<>nil) then begin
    // has parent
    if (not Parent.HasChild(Self)) then begin
      Result:=-10;
      exit;
    end;
  end else begin
    // no parent
  end;
  if FirstChild<>nil then begin
    if Expanded then begin
      if (FirstChild.Index<>Index+1) then begin
        Result:=-11;
        exit;
      end;
    end;
  end else begin
    if LastChild<>nil then begin
      Result:=-12;
      exit;
    end;
  end;
  RealChildCount:=0;
  AChild:=FirstChild;
  while AChild<>nil do begin
    if AChild.Parent<>Self then begin
      Result:=-13;
      exit;
    end;
    inc(RealChildCount);
    AChild:=AChild.NextBrother;
  end;
  if RealChildCount<>ChildCount then begin
    Result:=-14;
    exit;
  end;
  Result:=0;
end;

function TOIPropertyGridRow.HasChild(Row: TOIPropertyGridRow): boolean;
var
  ChildRow: TOIPropertyGridRow;
begin
  ChildRow:=FirstChild;
  while ChildRow<>nil do begin
    if ChildRow=Row then begin
      Result:=true;
      exit;
    end;
  end;
  Result:=false;
end;

procedure TOIPropertyGridRow.GetLvl;
var n:TOIPropertyGridRow;
begin
  FLvl:=0;
  n:=FParent;
  while n<>nil do begin
    inc(FLvl);
    n:=n.FParent;
  end;
end;

function TOIPropertyGridRow.GetBottom:integer;
begin
  Result:=FTop+FHeight;
end;

function TOIPropertyGridRow.IsReadOnly: boolean;
begin
  Result:=Editor.IsReadOnly or IsDisabled;
end;

function TOIPropertyGridRow.IsDisabled: boolean;
var
  ParentRow: TOIPropertyGridRow;
begin
  Result:=false;
  ParentRow:=Parent;
  while (ParentRow<>nil) do begin
    if paDisableSubProperties in ParentRow.Editor.GetAttributes then begin
      Result:=true;
      exit;
    end;
    ParentRow:=ParentRow.Parent;
  end;
end;

procedure TOIPropertyGridRow.MeasureHeight(ACanvas: TCanvas);
begin
  FHeight:=FTree.DefaultItemHeight;
  Editor.PropMeasureHeight(Name,ACanvas,FHeight);
end;

//==============================================================================


{ TOIOptions }

procedure TOIOptions.SetFilename(const NewFilename: string);
begin
  if FFilename=NewFilename then exit;
  FFilename:=NewFilename;
  FFileHasChangedOnDisk:=true;
end;

function TOIOptions.FileHasChangedOnDisk: boolean;
begin
  Result:=FFileHasChangedOnDisk
      or ((FFilename<>'') and (FFileAge<>0) and (FileAge(FFilename)<>FFileAge));
  FFileHasChangedOnDisk:=Result;
end;

function TOIOptions.GetXMLCfg: TXMLConfig;
begin
  if CustomXMLCfg<>nil then begin
    Result:=CustomXMLCfg;
  end else begin
    if FileHasChangedOnDisk or (FXMLCfg=nil) then begin
      FXMLCfg.Free;
      FXMLCfg:=TXMLConfig.Create(FFilename);
    end;
    Result:=FXMLCfg;
  end;
end;

procedure TOIOptions.FileUpdated;
begin
  FFileHasChangedOnDisk:=false;
  if FFilename<>'' then
    FFileAge:=FileAge(FFilename)
  else
    FFileAge:=0;
end;

constructor TOIOptions.Create;
begin
  inherited Create;
  FFilename:='';

  FSaveBounds:=false;
  FLeft:=0;
  FTop:=0;
  FWidth:=250;
  FHeight:=400;
  FPropertyGridSplitterX:=110;
  FEventGridSplitterX:=110;
  FDefaultItemHeight:=20;
  FShowComponentTree:=true;
  FComponentTreeHeight:=100;

  FGridBackgroundColor:=clBtnFace;
end;

destructor TOIOptions.Destroy;
begin
  FXMLCfg.Free;
  inherited Destroy;
end;

function TOIOptions.Load:boolean;
var XMLConfig: TXMLConfig;
begin
  Result:=false;
  if not FileExists(FFilename) then exit;
  try
    XMLConfig:=GetXMLCfg;

    FSaveBounds:=XMLConfig.GetValue('ObjectInspectorOptions/Bounds/Valid'
                     ,false);
    if FSaveBounds then begin
      FLeft:=XMLConfig.GetValue('ObjectInspectorOptions/Bounds/Left',0);
      FTop:=XMLConfig.GetValue('ObjectInspectorOptions/Bounds/Top',0);
      FWidth:=XMLConfig.GetValue('ObjectInspectorOptions/Bounds/Width',250);
      FHeight:=XMLConfig.GetValue('ObjectInspectorOptions/Bounds/Height',400);
    end;
    FPropertyGridSplitterX:=XMLConfig.GetValue(
       'ObjectInspectorOptions/Bounds/PropertyGridSplitterX',110);
    if FPropertyGridSplitterX<10 then FPropertyGridSplitterX:=10;
    FEventGridSplitterX:=XMLConfig.GetValue(
       'ObjectInspectorOptions/Bounds/EventGridSplitterX',110);
    if FEventGridSplitterX<10 then FEventGridSplitterX:=10;
    FDefaultItemHeight:=XMLConfig.GetValue(
       'ObjectInspectorOptions/Bounds/DefaultItemHeight',20);
    if FDefaultItemHeight<0 then FDefaultItemHeight:=20;
    FShowComponentTree:=XMLConfig.GetValue(
       'ObjectInspectorOptions/ComponentTree/Show/Value',true);
    FComponentTreeHeight:=XMLConfig.GetValue(
       'ObjectInspectorOptions/ComponentTree/Height/Value',100);

    FGridBackgroundColor:=XMLConfig.GetValue(
         'ObjectInspectorOptions/GridBackgroundColor',clBtnFace);
    FShowHints:=XMLConfig.GetValue(
         'ObjectInspectorOptions/ShowHints',false);
  except
    on E: Exception do begin
      DebugLn('ERROR: TOIOptions.Load: ',E.Message);
      exit;
    end;
  end;
  Result:=true;
end;

function TOIOptions.Save:boolean;
var XMLConfig: TXMLConfig;
begin
  Result:=false;
  try
    XMLConfig:=GetXMLCfg;

    XMLConfig.SetDeleteValue('ObjectInspectorOptions/Bounds/Valid',FSaveBounds,
                             false);
    if FSaveBounds then begin
      XMLConfig.SetValue('ObjectInspectorOptions/Bounds/Left',FLeft);
      XMLConfig.SetValue('ObjectInspectorOptions/Bounds/Top',FTop);
      XMLConfig.SetValue('ObjectInspectorOptions/Bounds/Width',FWidth);
      XMLConfig.SetValue('ObjectInspectorOptions/Bounds/Height',FHeight);
    end;
    XMLConfig.SetDeleteValue(
      'ObjectInspectorOptions/Bounds/PropertyGridSplitterX',
      FPropertyGridSplitterX, 110);
    XMLConfig.SetDeleteValue(
      'ObjectInspectorOptions/Bounds/EventGridSplitterX',
      FEventGridSplitterX, 110);
    XMLConfig.SetDeleteValue('ObjectInspectorOptions/Bounds/DefaultItemHeight',
                             FDefaultItemHeight,20);
    XMLConfig.SetDeleteValue('ObjectInspectorOptions/ComponentTree/Show/Value',
                             FShowComponentTree,true);
    XMLConfig.SetDeleteValue('ObjectInspectorOptions/ComponentTree/Height/Value',
                             FComponentTreeHeight,100);

    XMLConfig.SetDeleteValue('ObjectInspectorOptions/GridBackgroundColor',
                             FGridBackgroundColor,clBackground);
    XMLConfig.SetDeleteValue('ObjectInspectorOptions/ShowHints',FShowHints,
                             false);

    if XMLConfig<>CustomXMLCfg then XMLConfig.Flush;
  except
    on E: Exception do begin
      DebugLn('ERROR: TOIOptions.Save: ',E.Message);
      exit;
    end;
  end;
  Result:=true;
end;

procedure TOIOptions.Assign(AnObjInspector: TObjectInspector);
begin
  FLeft:=AnObjInspector.Left;
  FTop:=AnObjInspector.Top;
  FWidth:=AnObjInspector.Width;
  FHeight:=AnObjInspector.Height;
  FPropertyGridSplitterX:=AnObjInspector.PropertyGrid.PrefferedSplitterX;
  FEventGridSplitterX:=AnObjInspector.EventGrid.PrefferedSplitterX;
  FDefaultItemHeight:=AnObjInspector.DefaultItemHeight;
  FShowComponentTree:=AnObjInspector.ShowComponentTree;
  FComponentTreeHeight:=AnObjInspector.ComponentTreeHeight;
  FGridBackgroundColor:=AnObjInspector.PropertyGrid.BackgroundColor;
  FShowHints:=AnObjInspector.PropertyGrid.ShowHint;
end;

procedure TOIOptions.AssignTo(AnObjInspector: TObjectInspector);
begin
  if FSaveBounds then begin
    AnObjInspector.SetBounds(FLeft,FTop,FWidth,FHeight);
  end;
  AnObjInspector.PropertyGrid.PrefferedSplitterX:=FPropertyGridSplitterX;
  AnObjInspector.PropertyGrid.SplitterX:=FPropertyGridSplitterX;
  AnObjInspector.EventGrid.PrefferedSplitterX:=FEventGridSplitterX;
  AnObjInspector.EventGrid.SplitterX:=FEventGridSplitterX;
  AnObjInspector.DefaultItemHeight:=FDefaultItemHeight;
  AnObjInspector.ShowComponentTree:=FShowComponentTree;
  AnObjInspector.ComponentTreeHeight:=FComponentTreeHeight;
  AnObjInspector.PropertyGrid.BackgroundColor:=FGridBackgroundColor;
  AnObjInspector.PropertyGrid.ShowHint:=FShowHints;
  AnObjInspector.EventGrid.BackgroundColor:=FGridBackgroundColor;
  AnObjInspector.EventGrid.ShowHint:=FShowHints;
end;


//==============================================================================


{ TObjectInspector }

constructor TObjectInspector.Create(AnOwner: TComponent);

  procedure AddPopupMenuItem(var NewMenuItem: TMenuItem;
    ParentMenuItem: TMenuItem; const AName, ACaption, AHint: string;
    AnOnClick: TNotifyEvent; CheckedFlag, EnabledFlag, VisibleFlag: boolean);
  begin
    NewMenuItem:=TMenuItem.Create(Self);
    with NewMenuItem do begin
      Name:=AName;
      Caption:=ACaption;
      Hint:=AHint;
      OnClick:=AnOnClick;
      Checked:=CheckedFlag;
      Enabled:=EnabledFlag;
      Visible:=VisibleFlag;
    end;
    if ParentMenuItem<>nil then
      ParentMenuItem.Add(NewMenuItem)
    else
      MainPopupMenu.Items.Add(NewMenuItem);
  end;

  procedure AddSeparatorMenuItem(ParentMenuItem: TMenuItem;
    const AName: string; VisibleFlag: boolean);
  var
    NewMenuItem: TMenuItem;
  begin
    NewMenuItem:=TMenuItem.Create(Self);
    with NewMenuItem do begin
      Name:=AName;
      Caption:='-';
      Visible:=VisibleFlag;
    end;
    if ParentMenuItem<>nil then
      ParentMenuItem.Add(NewMenuItem)
    else
      MainPopupMenu.Items.Add(NewMenuItem);
  end;

begin
  inherited Create(AnOwner);
  FPropertyEditorHook:=nil;
  FSelection:=TPersistentSelectionList.Create;
  FUpdatingAvailComboBox:=false;
  FDefaultItemHeight := 22;
  FComponentTreeHeight:=100;
  FShowComponentTree:=true;
  FUsePairSplitter:=TPairSplitter.IsSupportedByInterface;

  Caption := oisObjectInspector;
  Name := DefaultObjectInspectorName;
  KeyPreview:=true;

  // StatusBar
  StatusBar:=TStatusBar.Create(Self);
  with StatusBar do begin
    Name:='StatusBar';
    Parent:=Self;
    SimpleText:=oisAll;
    Align:= alBottom;
  end;

  // PopupMenu
  MainPopupMenu:=TPopupMenu.Create(Self);
  with MainPopupMenu do begin
    Name:='MainPopupMenu';
    OnPopup:=@OnMainPopupMenuPopup;
    AutoPopup:=true;
  end;
  AddPopupMenuItem(SetDefaultPopupmenuItem,nil,'SetDefaultPopupMenuItem',
     'Set to Default Value','Set property value to Default',
     @OnSetDefaultPopupmenuItemClick,false,true,true);
  AddPopupMenuItem(UndoPropertyPopupMenuItem,nil,'UndoPropertyPopupMenuItem',
     'Undo','Set property value to last valid value',
     @OnUndoPopupmenuItemClick,false,true,true);
  AddSeparatorMenuItem(nil,'OptionsSeparatorMenuItem',true);
  AddPopupMenuItem(ColorsPopupmenuItem,nil,'ColorsPopupMenuItem','Set Colors',''
     ,nil,false,true,true);
  AddPopupMenuItem(BackgroundColPopupMenuItem,ColorsPopupMenuItem
     ,'BackgroundColPopupMenuItem','Background','Grid background color'
     ,@OnBackgroundColPopupMenuItemClick,false,true,true);
  AddPopupMenuItem(ShowHintsPopupMenuItem,nil
     ,'ShowHintPopupMenuItem','Show Hints','Grid hints'
     ,@OnShowHintPopupMenuItemClick,false,true,true);
  ShowHintsPopupMenuItem.ShowAlwaysCheckable:=true;
  AddPopupMenuItem(ShowComponentTreePopupMenuItem,nil
     ,'ShowComponentTreePopupMenuItem','Show Component Tree',''
     ,@OnShowComponentTreePopupMenuItemClick,FShowComponentTree,true,true);
  ShowComponentTreePopupMenuItem.ShowAlwaysCheckable:=true;
  AddPopupMenuItem(ShowOptionsPopupMenuItem,nil
     ,'ShowOptionsPopupMenuItem','Options',''
     ,@OnShowOptionsPopupMenuItemClick,false,true,FOnShowOptions<>nil);

  PopupMenu:=MainPopupMenu;

  // combobox at top (filled with available persistents)
  AvailPersistentComboBox := TComboBox.Create (Self);
  with AvailPersistentComboBox do begin
    Name:='AvailPersistentComboBox';
    Parent:=Self;
    Style:=csDropDown;
    Text:='';
    OnCloseUp:=@AvailComboBoxCloseUp;
    //Sorted:=true;
    Align:= alTop;
    Visible:=not FShowComponentTree;
  end;
  
  if FUsePairSplitter and ShowComponentTree then
    CreatePairSplitter;

  // Component Tree at top (filled with available components)
  ComponentTree:=TComponentTreeView.Create(Self);
  with ComponentTree do begin
    Name:='ComponentTree';
    Height:=ComponentTreeHeight;
    if PairSplitter1<>nil then begin
      Parent:=PairSplitter1.Sides[0];
      Align:=alClient;
    end else begin
      Parent:=Self;
      Align:=alTop;
    end;
    OnSelectionChanged:=@ComponentTreeSelectionChanged;
    Visible:=FShowComponentTree;
  end;

  CreateNoteBook;
  
  OnResize:=@ObjectInspectorResize;
end;

destructor TObjectInspector.Destroy;
begin
  FreeAndNil(FSelection);
  inherited Destroy;
end;

procedure TObjectInspector.SetPropertyEditorHook(NewValue:TPropertyEditorHook);
begin
  if FPropertyEditorHook=NewValue then exit;
  if FPropertyEditorHook<>nil then begin
    FPropertyEditorHook.RemoveAllHandlersForObject(Self);
  end;
  FPropertyEditorHook:=NewValue;
  if FPropertyEditorHook<>nil then begin
    FPropertyEditorHook.AddHandlerChangeLookupRoot(@HookLookupRootChange);
    FPropertyEditorHook.AddHandlerRefreshPropertyValues(
                                                @HookRefreshPropertyValues);
    FPropertyEditorHook.AddHandlerGetSelection(@HookGetSelection);
    FPropertyEditorHook.AddHandlerSetSelection(@HookSetSelection);
    // select root component
    FSelection.Clear;
    if (FPropertyEditorHook<>nil) and (FPropertyEditorHook.LookupRoot<>nil)
    and (FPropertyEditorHook.LookupRoot is TComponent) then
      FSelection.Add(TComponent(FPropertyEditorHook.LookupRoot));
    FillPersistentComboBox;
    PropertyGrid.PropertyEditorHook:=FPropertyEditorHook;
    EventGrid.PropertyEditorHook:=FPropertyEditorHook;
    ComponentTree.PropertyEditorHook:=FPropertyEditorHook;
    RefreshSelection;
  end;
end;

function TObjectInspector.PersistentToString(APersistent: TPersistent): string;
begin
  if APersistent is TComponent then
    Result:=TComponent(APersistent).GetNamePath+': '+APersistent.ClassName
  else
    Result:=APersistent.ClassName;
end;

procedure TObjectInspector.SetComponentTreeHeight(const AValue: integer);
begin
  if FComponentTreeHeight=AValue then exit;
  FComponentTreeHeight:=AValue;
end;

procedure TObjectInspector.SetDefaultItemHeight(const AValue: integer);
var
  NewValue: Integer;
begin
  NewValue:=AValue;
  if NewValue<0 then
    NewValue:=0
  else if NewValue=0 then
    NewValue:=22
  else if (NewValue>0) and (NewValue<10) then
    NewValue:=10
  else if NewValue>100 then NewValue:=100;
  if FDefaultItemHeight=NewValue then exit;
  FDefaultItemHeight:=NewValue;
  PropertyGrid.DefaultItemHeight:=FDefaultItemHeight;
  EventGrid.DefaultItemHeight:=FDefaultItemHeight;
  RebuildPropertyLists;
end;

procedure TObjectInspector.SetOnShowOptions(const AValue: TNotifyEvent);
begin
  if FOnShowOptions=AValue then exit;
  FOnShowOptions:=AValue;
  ShowOptionsPopupMenuItem.Visible:=FOnShowOptions<>nil;
end;

procedure TObjectInspector.AddPersistentToList(APersistent: TPersistent;
  List: TStrings);
var
  Allowed: boolean;
begin
  if (APersistent is TComponent)
  and (csDestroying in TComponent(APersistent).ComponentState) then exit;
  Allowed:=true;
  if Assigned(FOnAddAvailablePersistent) then
    FOnAddAvailablePersistent(APersistent,Allowed);
  if Allowed then
    List.AddObject(PersistentToString(APersistent),APersistent);
end;

procedure TObjectInspector.HookLookupRootChange;
begin
  PropertyGrid.PropEditLookupRootChange;
  EventGrid.PropEditLookupRootChange;
  FillPersistentComboBox;
end;

procedure TObjectInspector.FillPersistentComboBox;
var a:integer;
  Root:TComponent;
  OldText:AnsiString;
  NewList: TStringList;
begin
//writeln('[TObjectInspector.FillComponentComboBox] A ',FUpdatingAvailComboBox
//,' ',FPropertyEditorHook<>nil,'  ',FPropertyEditorHook.LookupRoot<>nil);
  if FUpdatingAvailComboBox then exit;
  FUpdatingAvailComboBox:=true;
  if ComponentTree<>nil then
    ComponentTree.Selection:=FSelection;
  NewList:=TStringList.Create;
  try
    if (FPropertyEditorHook<>nil)
    and (FPropertyEditorHook.LookupRoot<>nil) then begin
      AddPersistentToList(FPropertyEditorHook.LookupRoot,NewList);
      if FPropertyEditorHook.LookupRoot is TComponent then begin
        Root:=TComponent(FPropertyEditorHook.LookupRoot);
  //writeln('[TObjectInspector.FillComponentComboBox] B  ',Root.Name,'  ',Root.ComponentCount);
        for a:=0 to Root.ComponentCount-1 do
          AddPersistentToList(Root.Components[a],NewList);
      end;
    end;

    if AvailPersistentComboBox.Items.Equals(NewList) then exit;

    AvailPersistentComboBox.Items.BeginUpdate;
    if AvailPersistentComboBox.Items.Count=1 then
      OldText:=AvailPersistentComboBox.Text
    else
      OldText:='';
    AvailPersistentComboBox.Items.Assign(NewList);
    AvailPersistentComboBox.Items.EndUpdate;
    a:=AvailPersistentComboBox.Items.IndexOf(OldText);
    if (OldText='') or (a<0) then
      SetAvailComboBoxText
    else
      AvailPersistentComboBox.ItemIndex:=a;

  finally
    NewList.Free;
    FUpdatingAvailComboBox:=false;
  end;
end;

procedure TObjectInspector.BeginUpdate;
begin
  inc(FUpdateLock);
end;

procedure TObjectInspector.EndUpdate;
begin
  dec(FUpdateLock);
  if FUpdateLock<0 then begin
    DebugLn('ERROR TObjectInspector.EndUpdate');
  end;
  if FUpdateLock=0 then begin
    if oifRebuildPropListsNeeded in FFLags then
      RebuildPropertyLists;
  end;
end;

function TObjectInspector.GetActivePropertyGrid: TOICustomPropertyGrid;
begin
  Result:=nil;
  if NoteBook=nil then exit;
  if NoteBook.PageIndex=0 then
    Result:=PropertyGrid
  else if NoteBook.PageIndex=1 then
    Result:=EventGrid;
end;

function TObjectInspector.GetActivePropertyRow: TOIPropertyGridRow;
var
  CurGrid: TOICustomPropertyGrid;
begin
  Result:=nil;
  CurGrid:=GetActivePropertyGrid;
  if CurGrid=nil then exit;
  Result:=CurGrid.GetActiveRow;
end;

function TObjectInspector.GetCurRowDefaultValue(var DefaultStr: string): boolean;
var
  CurRow: TOIPropertyGridRow;
begin
  Result:=false;
  DefaultStr:='';
  CurRow:=GetActivePropertyRow;
  if (CurRow=nil) or (not (paHasDefaultValue in CurRow.Editor.GetAttributes))
  then exit;
  try
    DefaultStr:=CurRow.Editor.GetDefaultValue;
    Result:=true;
  except
    DefaultStr:='';
  end;
end;

procedure TObjectInspector.SetSelection(
  const ASelection:TPersistentSelectionList);
begin
  if FSelection.IsEqual(ASelection) then exit;
  //if (FSelection.Count=1) and (FSelection[0] is TCollectionItem)
  //and (ASelection.Count=0) then RaiseGDBException('');
  FSelection.Assign(ASelection);
  SetAvailComboBoxText;
  RefreshSelection;
end;

procedure TObjectInspector.RefreshSelection;
begin
  PropertyGrid.Selection := FSelection;
  EventGrid.Selection := FSelection;
  ComponentTree.Selection := FSelection;
  ComponentTree.MakeSelectionVisible;
  if (not Visible) and (FSelection.Count>0) then
    Visible:=true;
end;

procedure TObjectInspector.RefreshPropertyValues;
begin
  PropertyGrid.RefreshPropertyValues;
  EventGrid.RefreshPropertyValues;
end;

procedure TObjectInspector.RebuildPropertyLists;
begin
  if FUpdateLock>0 then
    Include(FFLags,oifRebuildPropListsNeeded)
  else begin
    Exclude(FFLags,oifRebuildPropListsNeeded);
    PropertyGrid.BuildPropertyList;
    EventGrid.BuildPropertyList;
  end;
end;

procedure TObjectInspector.AvailComboBoxCloseUp(Sender:TObject);
var NewComponent,Root:TComponent;
  a:integer;

  procedure SetSelectedPersistent(c:TPersistent);
  begin
    if (FSelection.Count=1) and (FSelection[0]=c) then exit;
    FSelection.Clear;
    FSelection.Add(c);
    RefreshSelection;
    if Assigned(FOnSelectPersistentsInOI) then
      FOnSelectPersistentsInOI(Self);
  end;

// AvailComboBoxChange
begin
  if FUpdatingAvailComboBox then exit;
  if (FPropertyEditorHook=nil) or (FPropertyEditorHook.LookupRoot=nil) then
    exit;
  if not (FPropertyEditorHook.LookupRoot is TComponent) then begin
    // not a TComponent => no childs => select always only the root
    SetSelectedPersistent(FPropertyEditorHook.LookupRoot);
    exit;
  end;
  Root:=TComponent(FPropertyEditorHook.LookupRoot);
  if (AvailPersistentComboBox.Text=PersistentToString(Root)) then begin
    SetSelectedPersistent(Root);
  end else begin
    for a:=0 to Root.ComponentCount-1 do begin
      NewComponent:=Root.Components[a];
      if AvailPersistentComboBox.Text=PersistentToString(NewComponent) then
      begin
        SetSelectedPersistent(NewComponent);
        break;
      end;
    end;
  end;
end;

procedure TObjectInspector.ComponentTreeSelectionChanged(Sender: TObject);
begin
  if (PropertyEditorHook=nil) or (PropertyEditorHook.LookupRoot=nil) then exit;
  if FSelection.IsEqual(ComponentTree.Selection) then exit;
  Fselection.Assign(ComponentTree.Selection);
  RefreshSelection;
  if Assigned(FOnSelectPersistentsInOI) then
    FOnSelectPersistentsInOI(Self);
end;

procedure TObjectInspector.ObjectInspectorResize(Sender: TObject);
begin
  if (ComponentTree<>nil) and (ComponentTree.Visible)
  and (ComponentTree.Parent=Self) then
    ComponentTree.Height:=ClientHeight div 4;
end;

procedure TObjectInspector.OnGriddKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(OnRemainingKeyUp) then OnRemainingKeyUp(Self,Key,Shift);
end;

procedure TObjectInspector.OnSetDefaultPopupmenuItemClick(Sender: TObject);
var
  CurGrid: TOICustomPropertyGrid;
  DefaultStr: string;
begin
  if not GetCurRowDefaultValue(DefaultStr) then exit;
  CurGrid:=GetActivePropertyGrid;
  if CurGrid=nil then exit;
  CurGrid.SetCurrentRowValue(DefaultStr);
  RefreshPropertyValues;
end;

procedure TObjectInspector.OnUndoPopupmenuItemClick(Sender: TObject);
var
  CurGrid: TOICustomPropertyGrid;
  CurRow: TOIPropertyGridRow;
begin
  CurGrid:=GetActivePropertyGrid;
  CurRow:=GetActivePropertyRow;
  if CurRow=nil then exit;
  CurGrid.CurrentEditValue:=CurRow.Editor.GetVisualValue;
end;

procedure TObjectInspector.OnBackgroundColPopupMenuItemClick(Sender :TObject);
var ColorDialog:TColorDialog;
begin
  ColorDialog:=TColorDialog.Create(Application);
  try
    with ColorDialog do begin
      Color:=PropertyGrid.BackgroundColor;
      if Execute then begin
        PropertyGrid.BackgroundColor:=Color;
        EventGrid.BackgroundColor:=Color;
      end;
    end;
  finally
    ColorDialog.Free;
  end;
end;

procedure TObjectInspector.OnGridModified(Sender: TObject);
begin
  if Assigned(FOnModified) then FOnModified(Self);
end;

procedure TObjectInspector.SetAvailComboBoxText;
begin
  case FSelection.Count of
    0: // none selected
       AvailPersistentComboBox.Text:='';
    1: // single selection
       AvailPersistentComboBox.Text:=PersistentToString(FSelection[0]);
  else
    // multi selection
    AvailPersistentComboBox.Text:=Format(oisItemsSelected, [FSelection.Count]);
  end;
end;

procedure TObjectInspector.HookGetSelection(
  const ASelection: TPersistentSelectionList);
begin
  if ASelection=nil then exit;
  ASelection.Assign(FSelection);
end;

procedure TObjectInspector.HookSetSelection(
  const ASelection: TPersistentSelectionList);
begin
  if ASelection=nil then exit;
  if FSelection.IsEqual(ASelection) then exit;
  Selection:=ASelection;
  if Assigned(FOnSelectPersistentsInOI) then
    FOnSelectPersistentsInOI(Self);
end;

procedure TObjectInspector.SetShowComponentTree(const AValue: boolean);
begin
  if FShowComponentTree=AValue then exit;
  FShowComponentTree:=AValue;
  BeginUpdate;
  ShowComponentTreePopupMenuItem.Checked:=FShowComponentTree;
  // hide controls while rebuilding
  if PairSplitter1<>nil then
    PairSplitter1.Visible:=false;
  DestroyNoteBook;
  ComponentTree.Visible:=false;
  AvailPersistentComboBox.Visible:=false;
  // rebuild controls
  if FUsePairSplitter and FShowComponentTree then begin
    CreatePairSplitter;
    ComponentTree.Parent:=PairSplitter1.Sides[0];
    ComponentTree.Align:=alClient;
  end else begin
    ComponentTree.Parent:=Self;
    ComponentTree.Align:=alTop;
    ComponentTree.Height:=ComponentTreeHeight;
    if PairSplitter1<>nil then begin
      PairSplitter1.Free;
      PairSplitter1:=nil;
    end;
  end;
  ComponentTree.Visible:=FShowComponentTree;
  AvailPersistentComboBox.Visible:=not FShowComponentTree;
  CreateNoteBook;
  EndUpdate;
end;

procedure TObjectInspector.SetUsePairSplitter(const AValue: boolean);
begin
  if FUsePairSplitter=AValue then exit;
  FUsePairSplitter:=AValue;
end;

procedure TObjectInspector.CreatePairSplitter;
begin
  // pair splitter between component tree and notebook
  PairSplitter1:=TPairSplitter.Create(Self);
  with PairSplitter1 do begin
    Name:='PairSplitter1';
    Parent:=Self;
    SplitterType:=pstVertical;
    Align:=alClient;
    Position:=ComponentTreeHeight;
    Sides[0].Name:=Name+'Side1';
    Sides[1].Name:=Name+'Side2';
  end;
end;

procedure TObjectInspector.DestroyNoteBook;
begin
  if NoteBook<>nil then
    NoteBook.Visible:=false;
  if PropertyGrid<>nil then begin
    PropertyGrid.Free;
    PropertyGrid:=nil;
  end;
  if EventGrid<>nil then begin
    EventGrid.Free;
    EventGrid:=nil;
  end;
  if NoteBook<>nil then begin
    NoteBook.Free;
    NoteBook:=nil;
  end;
end;

procedure TObjectInspector.CreateNoteBook;
begin
  DestroyNoteBook;

  // NoteBook
  NoteBook:=TNoteBook.Create(Self);
  with NoteBook do begin
    Name:='NoteBook';
    if PairSplitter1<>nil then begin
      Parent:=PairSplitter1.Sides[1];
    end else begin
      Parent:=Self;
    end;
    Align:= alClient;
    if PageCount>0 then
      Pages.Strings[0]:=oisProperties
    else
      Pages.Add(oisProperties);
    Page[0].Name:='PropertyPage';
    Pages.Add(oisEvents);
    Page[1].Name:='EventPage';
    PageIndex:=0;
    PopupMenu:=MainPopupMenu;
  end;

  // property grid
  PropertyGrid:=TOICustomPropertyGrid.CreateWithParams(Self,PropertyEditorHook
      ,[tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkSet{, tkMethod}
      , tkSString, tkLString, tkAString, tkWString, tkVariant
      {, tkArray, tkRecord, tkInterface}, tkClass, tkObject, tkWChar, tkBool
      , tkInt64, tkQWord],
      FDefaultItemHeight);
  with PropertyGrid do begin
    Name:='PropertyGrid';
    Parent:=NoteBook.Page[0];
{    
    ValueEdit.Parent:=Parent;
    ValueComboBox.Parent:=Parent;
    ValueButton.Parent:=Parent;
}    
    Selection:=Self.FSelection;
    Align:=alClient;
    PopupMenu:=MainPopupMenu;
    OnModified:=@OnGridModified;
    OnKeyUp:=@OnGriddKeyUp;
  end;

  // event grid
  EventGrid:=TOICustomPropertyGrid.CreateWithParams(Self,PropertyEditorHook,
                                              [tkMethod],FDefaultItemHeight);
  with EventGrid do begin
    Name:='EventGrid';
    Parent:=NoteBook.Page[1];
{    
    ValueEdit.Parent:=Parent;
    ValueComboBox.Parent:=Parent;
    ValueButton.Parent:=Parent;
}
    Selection:=Self.FSelection;
    Align:=alClient;
    PopupMenu:=MainPopupMenu;
    OnModified:=@OnGridModified;
    OnKeyUp:=@OnGriddKeyUp;
  end;
end;

procedure TObjectInspector.KeyDown(var Key: Word; Shift: TShiftState);
var
  CurGrid: TOICustomPropertyGrid;
begin
  CurGrid:=GetActivePropertyGrid;
  if CurGrid<>nil then begin
    CurGrid.HandleStandardKeys(Key,Shift);
    if Key=VK_UNKNOWN then exit;
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TObjectInspector.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  if (Key<>VK_UNKNOWN) and Assigned(OnRemainingKeyUp) then
    OnRemainingKeyUp(Self,Key,Shift);
end;

procedure TObjectInspector.OnShowHintPopupMenuItemClick(Sender : TObject);
begin
  PropertyGrid.ShowHint:=not PropertyGrid.ShowHint;
  EventGrid.ShowHint:=not EventGrid.ShowHint;
end;

procedure TObjectInspector.OnShowOptionsPopupMenuItemClick(Sender: TObject);
begin
  if Assigned(FOnShowOptions) then FOnShowOptions(Sender);
end;

procedure TObjectInspector.OnShowComponentTreePopupMenuItemClick(Sender: TObject
  );
begin
  ShowComponentTree:=not ShowComponentTree;
end;

procedure TObjectInspector.OnMainPopupMenuPopup(Sender: TObject);
var
  DefaultStr: String;
  CurGrid: TOICustomPropertyGrid;
  CurRow: TOIPropertyGridRow;
begin
  SetDefaultPopupMenuItem.Enabled:=GetCurRowDefaultValue(DefaultStr);
  if SetDefaultPopupMenuItem.Enabled then
    SetDefaultPopupMenuItem.Caption:=Format(oisSetToDefault, [DefaultStr])
  else
    SetDefaultPopupMenuItem.Caption:=oisSetToDefaultValue;
    
  CurGrid:=GetActivePropertyGrid;
  CurRow:=GetActivePropertyRow;
  if (CurRow<>nil) and (CurRow.Editor.GetVisualValue<>CurGrid.CurrentEditValue)
  then
    UndoPropertyPopupMenuItem.Enabled:=true
  else
    UndoPropertyPopupMenuItem.Enabled:=false;
  ShowHintsPopupMenuItem.Checked:=PropertyGrid.ShowHint;
end;

procedure TObjectInspector.HookRefreshPropertyValues;
begin
  RefreshPropertyValues;
end;

{ TCustomPropertiesGrid }

function TCustomPropertiesGrid.GetTIObject: TPersistent;
begin
  if PropertyEditorHook<>nil then Result:=PropertyEditorHook.LookupRoot;
end;

procedure TCustomPropertiesGrid.SetAutoFreeHook(const AValue: boolean);
begin
  if FAutoFreeHook=AValue then exit;
  FAutoFreeHook:=AValue;
end;

procedure TCustomPropertiesGrid.SetTIObject(const AValue: TPersistent);
var
  NewSelection: TPersistentSelectionList;
begin
  if (TIObject=AValue) then begin
    if ((AValue<>nil) and (Selection.Count=1) and (Selection[0]=AValue))
    or (AValue=nil) then
      exit;
  end;
  if PropertyEditorHook=nil then
    PropertyEditorHook:=TPropertyEditorHook.Create;
  PropertyEditorHook.LookupRoot:=AValue;
  if (AValue<>nil) and ((Selection.Count<>1) or (Selection[0]<>AValue)) then
  begin
    NewSelection:=TPersistentSelectionList.Create;
    try
      if AValue<>nil then
        NewSelection.Add(AValue);
      Selection:=NewSelection;
    finally
      NewSelection.Free;
    end;
  end;
end;

constructor TCustomPropertiesGrid.Create(TheOwner: TComponent);
var
  Hook: TPropertyEditorHook;
begin
  Hook:=TPropertyEditorHook.Create;
  AutoFreeHook:=true;
  CreateWithParams(TheOwner,Hook,AllTypeKinds,25);
end;

end.

