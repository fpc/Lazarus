{ $Id: objectinspector.pp 17395 2008-11-15 03:53:22Z paul $}
{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
   This unit defines the TObjectInspectorDlg.
   It uses TOIPropertyGrid and TOIPropertyGridRow which are also defined in this
   unit. The object inspector uses property editors (see TPropertyEditor) to
   display and control properties, thus the object inspector is merely an
   object viewer than an editor. The property editors do the real work.

  ToDo:
   - backgroundcolor=clNone
   - Define Init values
   - Set to init value
}
unit ObjectInspector;

{$MODE OBJFPC}{$H+}

{off $DEFINE DoNotCatchOIExceptions}

interface

uses
  // IMPORTANT: the object inspector is a tool and can be used in other programs
  //            too. Don't put Lazarus IDE specific things here.
  // FCL
  SysUtils, Types, Classes, TypInfo,
  // LCL
  InterfaceBase, Forms, Buttons, Graphics, GraphType, LCLProc,
  StdCtrls, LCLType, LCLIntf, Controls, ComCtrls, ExtCtrls,
  LMessages, LResources, LazConfigStorage, Menus, Dialogs, Themes,
  ObjInspStrConsts,
  PropEdits, GraphPropEdits, ListViewPropEdit, ImageListEditor,
  ComponentTreeView, ComponentEditors, IDEImagesIntf, OIFavouriteProperties;

const
  OIOptionsFileVersion = 3;

  DefBackgroundColor = clBtnFace;
  DefReferencesColor = clMaroon;
  DefSubPropertiesColor = clGreen;
  DefNameColor = clWindowText;
  DefDefaultValueColor = clWindowText;
  DefValueColor = clMaroon;
  DefReadOnlyColor = clGrayText;
  DefHighlightColor = clHighlight;
  DefHighlightFontColor = clHighlightText;
  DefGutterColor = DefBackgroundColor;
  DefGutterEdgeColor = cl3DShadow;

  ObjectInspectorDlgName = 'ObjectInspectorDlg';

type
  EObjectInspectorException = class(Exception);

  TObjectInspectorDlg = class;
  TOICustomPropertyGrid = class;

  // standard ObjectInspector pages
  TObjectInspectorPage = (
    oipgpProperties,
    oipgpEvents,
    oipgpFavourite,
    oipgpRestricted
    );
  TObjectInspectorPages = set of TObjectInspectorPage;


  { TOIOptions }

  TOIOptions = class
  private
    FComponentTreeHeight: integer;
    FConfigStore: TConfigStorage;
    FDefaultItemHeight: integer;
    FGutterColor: TColor;
    FGutterEdgeColor: TColor;
    FShowComponentTree: boolean;

    FSaveBounds: boolean;
    FLeft: integer;
    FShowGutter: boolean;
    FShowInfoBox: boolean;
    FInfoBoxHeight: integer;
    FShowStatusBar: boolean;
    FTop: integer;
    FWidth: integer;
    FHeight: integer;
    FGridSplitterX: array[TObjectInspectorPage] of integer;

    FPropertyNameColor: TColor;
    FDefaultValueColor: TColor;
    FSubPropertiesColor: TColor;
    FValueColor: TColor;
    FReadOnlyColor: TColor;
    FReferencesColor: TColor;
    FGridBackgroundColor: TColor;
    FHighlightColor: TColor;
    FHighlightFontColor: TColor;

    FShowHints: boolean;
    FAutoShow: Boolean;
    FBoldNonDefaultValues: Boolean;
    FDrawGridLines: Boolean;
    function FPropertyGridSplitterX(Page: TObjectInspectorPage): integer;
    procedure FPropertyGridSplitterX(Page: TObjectInspectorPage;
      const AValue: integer);
  public
    constructor Create;
    function Load: boolean;
    function Save: boolean;
    procedure Assign(AnObjInspector: TObjectInspectorDlg);
    procedure AssignTo(AnObjInspector: TObjectInspectorDlg); overload;
    procedure AssignTo(AGrid: TOICustomPropertyGrid); overload;

    property ConfigStore: TConfigStorage read FConfigStore write FConfigStore;

    property SaveBounds:boolean read FSaveBounds write FSaveBounds;
    property Left:integer read FLeft write FLeft;
    property Top:integer read FTop write FTop;
    property Width:integer read FWidth write FWidth;
    property Height:integer read FHeight write FHeight;
    property GridSplitterX[Page: TObjectInspectorPage]:integer
                       read FPropertyGridSplitterX write FPropertyGridSplitterX;
    property DefaultItemHeight: integer read FDefaultItemHeight
                                        write FDefaultItemHeight;
    property ShowComponentTree: boolean read FShowComponentTree
                                        write FShowComponentTree;
    property ComponentTreeHeight: integer read FComponentTreeHeight
                                          write FComponentTreeHeight;

    property GridBackgroundColor: TColor read FGridBackgroundColor write FGridBackgroundColor;
    property SubPropertiesColor: TColor read FSubPropertiesColor write FSubPropertiesColor;
    property ReferencesColor: TColor read FReferencesColor write FReferencesColor;
    property ValueColor: TColor read FValueColor write FValueColor;
    property ReadOnlyColor: TColor read FReadOnlyColor write FReadOnlyColor;
    property DefaultValueColor: TColor read FDefaultValueColor write FDefaultValueColor;
    property PropertyNameColor: TColor read FPropertyNameColor write FPropertyNameColor;
    property HighlightColor: TColor read FHighlightColor write FHighlightColor;
    property HighlightFontColor: TColor read FHighlightFontColor write FHighlightFontColor;
    property GutterColor: TColor read FGutterColor write FGutterColor;
    property GutterEdgeColor: TColor read FGutterEdgeColor write FGutterEdgeColor;

    property ShowHints: boolean read FShowHints
                                write FShowHints;
    property AutoShow: boolean read FAutoShow write FAutoShow;
    property BoldNonDefaultValues: boolean read FBoldNonDefaultValues write FBoldNonDefaultValues;
    property DrawGridLines: boolean read FDrawGridLines write FDrawGridLines;
    property ShowGutter: boolean read FShowGutter write FShowGutter;
    property ShowStatusBar: boolean read FShowStatusBar write FShowStatusBar;
    property ShowInfoBox: boolean read FShowInfoBox write FShowInfoBox;
    property InfoBoxHeight: integer read FInfoBoxHeight write FInfoBoxHeight;
  end;

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
    FParent: TOIPropertyGridRow;
    FEditor: TPropertyEditor;
    FWidgetSets: TLCLPlatforms;

    FIndex:integer;
    LastPaintedValue: string;

    procedure GetLvl;
  public
    constructor Create(PropertyTree: TOICustomPropertyGrid;
       PropEditor:TPropertyEditor; ParentNode:TOIPropertyGridRow; WidgetSets: TLCLPlatforms);
    destructor Destroy; override;
    function ConsistencyCheck: integer;
    function HasChild(Row: TOIPropertyGridRow): boolean;
    procedure WriteDebugReport(const Prefix: string);

    function GetBottom: integer;
    function IsReadOnly: boolean;
    function IsDisabled: boolean;
    procedure MeasureHeight(ACanvas: TCanvas);
    function Sort(const Compare: TListSortCompare): boolean;// true if changed
    function IsSorted(const Compare: TListSortCompare): boolean;
    function Next: TOIPropertyGridRow;
    function NextSkipChilds: TOIPropertyGridRow;

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
    property LastChild:TOIPropertyGridRow read FLastChild;
    property NextBrother:TOIPropertyGridRow read FNextBrother;
    property PriorBrother:TOIPropertyGridRow read FPriorBrother;
    property Index: integer read FIndex;
  end;

  //----------------------------------------------------------------------------
  TOIPropertyGridState = (
    pgsChangingItemIndex,
    pgsApplyingValue,
    pgsUpdatingEditControl,
    pgsBuildPropertyListNeeded,
    pgsGetComboItemsCalled,
    pgsIdleEnabled
    );
  TOIPropertyGridStates = set of TOIPropertyGridState;

  { TOICustomPropertyGrid }

  TOICustomPropertyGridColumn = (
    oipgcName,
    oipgcValue
  );

  TOILayout = (
   oilHorizontal,
   oilVertical
  );
  
  TOIQuickEdit = (
    oiqeEdit,
    oiqeShowValue
  );

  TOIPropertyHint = function(Sender: TObject; PointedRow: TOIPropertyGridRow;
            ScreenPos: TPoint; aHintWindow: THintWindow;
            out HintWinRect: TRect; out AHint: string
             ): boolean of object;

  TOICustomPropertyGrid = class(TCustomControl)
  private
    FBackgroundColor: TColor;
    FColumn: TOICustomPropertyGridColumn;
    FGutterColor: TColor;
    FGutterEdgeColor: TColor;
    FHighlightColor: TColor;
    FLayout: TOILayout;
    FOnOIKeyDown: TKeyEvent;
    FOnPropertyHint: TOIPropertyHint;
    FOnSelectionChange: TNotifyEvent;
    FReferencesColor: TColor;
    FReadOnlyColor: TColor;
    FRowSpacing: integer;
    FShowGutter: Boolean;
    FSubPropertiesColor: TColor;
    FChangeStep: integer;
    FCurrentButton: TControl; // nil or ValueButton
    FCurrentEdit: TWinControl;  // nil or ValueEdit or ValueComboBox or ValueCheckBox
    FCurrentEditorLookupRoot: TPersistent;
    FDefaultItemHeight:integer;
    FDragging: boolean;
    FExpandedProperties: TStringList;// used to restore expanded state when switching selected component(s)
    FExpandingRow: TOIPropertyGridRow;
    FFavourites: TOIFavouriteProperties;
    FFilter: TTypeKinds;
    FIndent: integer;
    FItemIndex: integer;
    FNameFont, FDefaultValueFont, FValueFont, FHighlightFont: TFont;
    FNewComboBoxItems: TStringList;
    FOnModified: TNotifyEvent;
    FPreferredSplitterX: integer; // best splitter position
    FPropertyEditorHook: TPropertyEditorHook;
    FRows: TFPList;// list of TOIPropertyGridRow
    FSelection: TPersistentSelectionList;
    FNotificationComponents: TFPList;
    FSplitterX: integer; // current splitter position
    FStates: TOIPropertyGridStates;
    FTopY: integer;
    FDrawHorzGridLines: Boolean;
    FActiveRowBmp: TCustomBitmap;
    FFirstClickTime: TDateTime;

    // hint stuff
    FHintTimer: TTimer;
    FHintWindow: THintWindow;
    FHintIndex: integer;
    FShowingLongHint: boolean; // last hint was activated by the hinttimer

    ValueEdit: TEdit;
    ValueComboBox: TComboBox;
    ValueCheckBox: TCheckBox;
    ValueButton: TSpeedButton;

    procedure HintTimer(Sender: TObject);
    procedure ResetHintTimer;
    procedure HideHint;
    procedure OnUserInput(Sender: TObject; Msg: Cardinal);
    procedure HintMouseDown(Sender: TObject; Button: TMouseButton;
                            Shift: TShiftState; X, Y: Integer);

    procedure IncreaseChangeStep;
    function GridIsUpdating: boolean;

    function GetRow(Index:integer):TOIPropertyGridRow;
    function GetRowCount:integer;
    procedure ClearRows;
    function GetCurrentEditValue: string;
    procedure SetActiveControl(const AControl: TWinControl);
    procedure SetColumn(const AValue: TOICustomPropertyGridColumn);
    procedure SetCurrentEditValue(const NewValue: string);
    procedure SetDrawHorzGridLines(const AValue: Boolean);
    procedure SetFavourites(const AValue: TOIFavouriteProperties);
    procedure SetFilter(const AValue: TTypeKinds);
    procedure SetGutterColor(const AValue: TColor);
    procedure SetGutterEdgeColor(const AValue: TColor);
    procedure SetHighlightColor(const AValue: TColor);
    procedure SetItemIndex(NewIndex:integer);
    function IsCurrentEditorAvailable: Boolean;

    function GetNameRowHeight: Integer; // temp solution untill TFont.height returns its actual value

    procedure SetItemsTops;
    procedure AlignEditComponents;
    procedure EndDragSplitter;
    procedure SetRowSpacing(const AValue: integer);
    procedure SetShowGutter(const AValue: Boolean);
    procedure SetSplitterX(const NewValue:integer);
    procedure SetTopY(const NewValue:integer);

    function GetPropNameColor(ARow: TOIPropertyGridRow):TColor;
    function GetTreeIconX(Index: integer):integer;
    function RowRect(ARow: integer):TRect;
    procedure PaintRow(ARow: integer);
    procedure DoPaint(PaintOnlyChangedValues: boolean);

    procedure SetSelection(const ASelection:TPersistentSelectionList);
    procedure SetPropertyEditorHook(NewPropertyEditorHook:TPropertyEditorHook);
    procedure UpdateSelectionNotifications;

    procedure AddPropertyEditor(PropEditor: TPropertyEditor);
    procedure AddStringToComboBox(const s: string);
    procedure ExpandRow(Index: integer);
    procedure ShrinkRow(Index: integer);
    procedure AddSubEditor(PropEditor: TPropertyEditor);
    procedure SortSubEditors(ParentRow: TOIPropertyGridRow);
    function CanExpandRow(Row: TOIPropertyGridRow): boolean;

    procedure SetRowValue;
    procedure DoCallEdit(Edit: TOIQuickEdit = oiqeEdit);
    procedure RefreshValueEdit;
    procedure ToggleRow;
    procedure ValueEditDblClick(Sender : TObject);
    procedure ValueControlMouseDown(Sender: TObject; Button:TMouseButton;
      Shift: TShiftState; X,Y:integer);
    procedure ValueControlMouseMove(Sender: TObject; Shift: TShiftState;
      X,Y:integer);
    procedure ValueEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ValueEditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ValueEditExit(Sender: TObject);
    procedure ValueEditChange(Sender: TObject);
    procedure ValueEditMouseUp(Sender: TObject; Button: TMouseButton;
                               Shift: TShiftState; X, Y: Integer);
    procedure ValueCheckBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ValueCheckBoxKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ValueCheckBoxExit(Sender: TObject);
    procedure ValueCheckBoxClick(Sender: TObject);
    procedure ValueComboBoxExit(Sender: TObject);
    procedure ValueComboBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ValueComboBoxKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ValueComboBoxMouseUp(Sender: TObject; Button: TMouseButton;
                                   Shift: TShiftState; X, Y: Integer);
    procedure ValueComboBoxCloseUp(Sender: TObject);
    procedure ValueComboBoxGetItems(Sender: TObject);
    procedure ValueButtonClick(Sender: TObject);
    procedure ValueComboBoxMeasureItem(Control: TWinControl; Index: Integer;
          var AHeight: Integer);
    procedure ValueComboBoxDrawItem(Control: TWinControl; Index: Integer;
          ARect: TRect; State: TOwnerDrawState);
    procedure OnIdle(Sender: TObject; var Done: Boolean);
    procedure SetIdleEvent(Enable: boolean);

    procedure WMVScroll(var Msg: TLMScroll); message LM_VSCROLL;
    procedure WMMouseWheel(var Message: TLMMouseEvent); message
                   LM_MOUSEWHEEL;
    procedure SetBackgroundColor(const AValue: TColor);
    procedure SetReferences(const AValue: TColor);
    procedure SetSubPropertiesColor(const AValue: TColor);
    procedure SetReadOnlyColor(const AValue: TColor);
    procedure UpdateScrollBar;
    function FillComboboxItems: boolean; // true if something changed
    function EditorFilter(const AEditor: TPropertyEditor): Boolean;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure MouseDown(Button:TMouseButton; Shift:TShiftState; X,Y:integer); override;
    procedure MouseMove(Shift:TShiftState; X,Y:integer);  override;
    procedure MouseUp(Button:TMouseButton; Shift:TShiftState; X,Y:integer); override;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure HandleStandardKeys(var Key: Word; Shift: TShiftState); virtual;
    procedure HandleKeyUp(var Key: Word; Shift: TShiftState); virtual;
    procedure DoTabKey; virtual;
    procedure DoSetBounds(ALeft, ATop, AWidth, AHeight: integer); override;
    procedure DoSelectionChange;
  public
    constructor Create(TheOwner: TComponent); override;
    constructor CreateWithParams(AnOwner: TComponent;
                                 APropertyEditorHook: TPropertyEditorHook;
                                 TypeFilter: TTypeKinds;
                                 DefItemHeight: integer);
    destructor Destroy;  override;
    function InitHints: boolean;
    function CanEditRowValue: boolean;
    procedure SaveChanges;
    function ConsistencyCheck: integer;
    procedure EraseBackground(DC: HDC); override;
    function GetActiveRow: TOIPropertyGridRow;
    function GetHintTypeAt(RowIndex: integer; X: integer): TPropEditHint;

    function GetRowByPath(const PropPath: string): TOIPropertyGridRow;
    function GridHeight: integer;
    function MouseToIndex(y: integer; MustExist: boolean):integer;
    function PropertyPath(Index: integer):string;
    function PropertyPath(Row: TOIPropertyGridRow):string;
    function TopMax: integer;
    procedure BuildPropertyList(OnlyIfNeeded: boolean = false);
    procedure Clear;
    procedure Paint;  override;
    procedure PropEditLookupRootChange;
    procedure RefreshPropertyValues;
    procedure ScrollToActiveItem;
    procedure ScrollToItem(NewIndex: Integer);
    procedure SetBounds(aLeft, aTop, aWidth, aHeight: integer); override;
    procedure SetCurrentRowValue(const NewValue: string);
    procedure SetItemIndexAndFocus(NewItemIndex: integer);

    property BackgroundColor: TColor read FBackgroundColor
                                     write SetBackgroundColor default DefBackgroundColor;
    property GutterColor: TColor read FGutterColor write SetGutterColor default DefGutterColor;
    property GutterEdgeColor: TColor read FGutterEdgeColor write SetGutterEdgeColor default DefGutterEdgeColor;
    property HighlightColor: TColor read FHighlightColor write SetHighlightColor default DefHighlightColor;
    property ReferencesColor: TColor read FReferencesColor
                                     write SetReferences default DefReferencesColor;
    property SubPropertiesColor: TColor read FSubPropertiesColor
                                     write SetSubPropertiesColor default DefSubPropertiesColor;
    property ReadOnlyColor: TColor read FReadOnlyColor
                                     write SetReadOnlyColor default DefReadOnlyColor;
    property NameFont: TFont read FNameFont write FNameFont;
    property DefaultValueFont: TFont read FDefaultValueFont write FDefaultValueFont;
    property ValueFont: TFont read FValueFont write FValueFont;
    property HighlightFont: TFont read FHighlightFont write FHighlightFont;

    property BorderStyle default bsSingle;
    property Column: TOICustomPropertyGridColumn read FColumn write SetColumn;
    property CurrentEditValue: string read GetCurrentEditValue
                                      write SetCurrentEditValue;
    property DefaultItemHeight:integer read FDefaultItemHeight
                                       write FDefaultItemHeight default 25;
    property DrawHorzGridLines: Boolean read FDrawHorzGridLines write
      SetDrawHorzGridLines default True;
    property ExpandedProperties: TStringList read FExpandedProperties
                                            write FExpandedProperties;
    property Indent: integer read FIndent write FIndent;
    property ItemIndex: integer read FItemIndex write SetItemIndex;
    property Layout: TOILayout read FLayout write FLayout default oilHorizontal;
    property OnModified: TNotifyEvent read FOnModified write FOnModified;
    property OnOIKeyDown: TKeyEvent read FOnOIKeyDown write FOnOIKeyDown;
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
    property OnPropertyHint: TOIPropertyHint read FOnPropertyHint write FOnPropertyHint;
    property PrefferedSplitterX: integer read FPreferredSplitterX
                                         write FPreferredSplitterX default 100;
    property PropertyEditorHook: TPropertyEditorHook read FPropertyEditorHook
                                                    write SetPropertyEditorHook;
    property RowCount: integer read GetRowCount;
    property Rows[Index: integer]: TOIPropertyGridRow read GetRow;
    property RowSpacing: integer read FRowSpacing write SetRowSpacing;
    property Selection: TPersistentSelectionList read FSelection
                                                 write SetSelection;
    property ShowGutter: Boolean read FShowGutter write SetShowGutter default True;
    property SplitterX: integer read FSplitterX write SetSplitterX default 100;
    property TopY: integer read FTopY write SetTopY default 0;
    property Favourites: TOIFavouriteProperties read FFavourites
                                                write SetFavourites;
    property Filter : TTypeKinds read FFilter write SetFilter;
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
    property OnDblClick;
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
    property OnSelectionChange;
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
    FSaveOnChangeTIObject: boolean;
    function GetTIObject: TPersistent;
    procedure SetAutoFreeHook(const AValue: boolean);
    procedure SetTIObject(const AValue: TPersistent);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property TIObject: TPersistent read GetTIObject write SetTIObject;
    property AutoFreeHook: boolean read FAutoFreeHook write SetAutoFreeHook;
    property SaveOnChangeTIObject: boolean read FSaveOnChangeTIObject
                                           write FSaveOnChangeTIObject
                                           default true;
  end;


  //============================================================================


  { TObjectInspectorDlg }

  TOnAddAvailablePersistent = procedure(APersistent: TPersistent;
    var Allowed: boolean) of object;

  TOIFlag = (
    oifRebuildPropListsNeeded
    );
  TOIFlags = set of TOIFlag;

  { TObjectInspectorDlg }

  TObjectInspectorDlg = class(TForm)
    AddToFavoritesPopupMenuItem: TMenuItem;
    ViewRestrictedPropertiesPopupMenuItem: TMenuItem;
    AvailPersistentComboBox: TComboBox;
    ComponentTree: TComponentTreeView;
    InfoPanel: TPanel;
    CopyPopupmenuItem: TMenuItem;
    CutPopupmenuItem: TMenuItem;
    DeletePopupmenuItem: TMenuItem;
    EventGrid: TOICustomPropertyGrid;
    FavouriteGrid: TOICustomPropertyGrid;
    RestrictedGrid: TOICustomPropertyGrid;
    RestrictedPanel: TPanel;
    RestrictedInnerPanel: TPanel;
    WidgetSetsRestrictedLabel: TLabel;
    WidgetSetsRestrictedBox: TPaintBox;
    ComponentRestrictedLabel: TLabel;
    ComponentRestrictedBox: TPaintBox;
    FindDeclarationPopupmenuItem: TMenuItem;
    OptionsSeparatorMenuItem: TMenuItem;
    MainPopupMenu: TPopupMenu;
    NoteBook: TPageControl;
    OptionsSeparatorMenuItem2: TMenuItem;
    PastePopupmenuItem: TMenuItem;
    PropertyGrid: TOICustomPropertyGrid;
    RemoveFromFavoritesPopupMenuItem: TMenuItem;
    SetDefaultPopupMenuItem: TMenuItem;
    ShowComponentTreePopupMenuItem: TMenuItem;
    ShowHintsPopupMenuItem: TMenuItem;
    ShowOptionsPopupMenuItem: TMenuItem;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    StatusBar: TStatusBar;
    UndoPropertyPopupMenuItem: TMenuItem;
    procedure AvailComboBoxCloseUp(Sender: TObject);
    procedure ComponentTreeDblClick(Sender: TObject);
    procedure ComponentTreeKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ComponentTreeSelectionChanged(Sender: TObject);
    procedure OnGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OnGridKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OnGridDblClick(Sender: TObject);
    procedure OnSetDefaultPopupmenuItemClick(Sender: TObject);
    procedure OnAddToFavoritesPopupmenuItemClick(Sender: TObject);
    procedure OnRemoveFromFavoritesPopupmenuItemClick(Sender: TObject);
    procedure OnViewRestrictionsPopupmenuItemClick(Sender: TObject);
    procedure OnUndoPopupmenuItemClick(Sender: TObject);
    procedure OnFindDeclarationPopupmenuItemClick(Sender: TObject);
    procedure OnCutPopupmenuItemClick(Sender: TObject);
    procedure OnCopyPopupmenuItemClick(Sender: TObject);
    procedure OnPastePopupmenuItemClick(Sender: TObject);
    procedure OnDeletePopupmenuItemClick(Sender: TObject);
    procedure OnShowHintPopupMenuItemClick(Sender: TObject);
    procedure OnShowOptionsPopupMenuItemClick(Sender: TObject);
    procedure OnShowComponentTreePopupMenuItemClick(Sender: TObject);
    procedure OnMainPopupMenuPopup(Sender: TObject);
    procedure RestrictedPageShow(Sender: TObject);
    procedure WidgetSetRestrictedPaint(Sender: TObject);
    procedure ComponentRestrictedPaint(Sender: TObject);
    procedure DoUpdateRestricted;
    procedure DoViewRestricted;
  private
    FAutoShow: Boolean;
    FFavourites: TOIFavouriteProperties;
    FInfoBoxHeight: integer;
    FOnPropertyHint: TOIPropertyHint;
    FOnSelectionChange: TNotifyEvent;
    FRestricted: TOIRestrictedProperties;
    FOnAddToFavourites: TNotifyEvent;
    FOnFindDeclarationOfProperty: TNotifyEvent;
    FOnOIKeyDown: TKeyEvent;
    FOnRemainingKeyDown: TKeyEvent;
    FOnRemainingKeyUp: TKeyEvent;
    FOnRemoveFromFavourites: TNotifyEvent;
    FOnUpdateRestricted: TNotifyEvent;
    FOnViewRestricted: TNotifyEvent;
    FSelection: TPersistentSelectionList;
    FComponentTreeHeight: integer;
    FDefaultItemHeight: integer;
    FFlags: TOIFlags;
    FOnShowOptions: TNotifyEvent;
    FPropertyEditorHook: TPropertyEditorHook;
    FOnAddAvailablePersistent: TOnAddAvailablePersistent;
    FOnSelectPersistentsInOI: TNotifyEvent;
    FOnModified: TNotifyEvent;
    FShowComponentTree: boolean;
    FShowFavorites: Boolean;
    FShowInfoBox: Boolean;
    FShowRestricted: Boolean;
    FShowStatusBar: Boolean;
    FUpdateLock: integer;
    FUpdatingAvailComboBox: boolean;
    FComponentEditor: TBaseComponentEditor;
    function GetGridControl(Page: TObjectInspectorPage): TOICustomPropertyGrid;
    procedure SetComponentEditor(const AValue: TBaseComponentEditor);
    procedure SetFavourites(const AValue: TOIFavouriteProperties);
    procedure SetComponentTreeHeight(const AValue: integer);
    procedure SetDefaultItemHeight(const AValue: integer);
    procedure SetInfoBoxHeight(const AValue: integer);
    procedure SetRestricted(const AValue: TOIRestrictedProperties);
    procedure SetOnShowOptions(const AValue: TNotifyEvent);
    procedure SetPropertyEditorHook(NewValue: TPropertyEditorHook);
    procedure SetSelection(const ASelection: TPersistentSelectionList);
    procedure SetShowComponentTree(const AValue: boolean);
    procedure SetShowFavorites(const AValue: Boolean);
    procedure SetShowInfoBox(const AValue: Boolean);
    procedure SetShowRestricted(const AValue: Boolean);
    procedure SetShowStatusBar(const AValue: Boolean);
    procedure ShowNextPage(Delta: integer);
    procedure RestrictedPaint(
      ABox: TPaintBox; const ARestrictions: TWidgetSetRestrictionsArray);
    procedure DoComponentEditorVerbMenuItemClick(Sender: TObject);
    procedure DoCollectionAddItem(Sender: TObject);
    procedure DoZOrderItemClick(Sender: TObject);
  private
    FInSelection: Boolean;
    FOnAutoShow: TNotifyEvent;
    function GetComponentTreeHeight: integer;
    function GetInfoBoxHeight: integer;
  protected
    function PersistentToString(APersistent: TPersistent): string;
    procedure AddPersistentToList(APersistent: TPersistent; List: TStrings);
    procedure HookLookupRootChange;
    procedure OnGridModified(Sender: TObject);
    procedure OnGridSelectionChange(Sender: TObject);
    function OnGridPropertyHint(Sender: TObject; PointedRow: TOIPropertyGridRow;
      ScreenPos: TPoint; aHintWindow: THintWindow;
      out HintWinRect: TRect; out AHint: string): boolean;
    procedure SetAvailComboBoxText;
    procedure HookGetSelection(const ASelection: TPersistentSelectionList);
    procedure HookSetSelection(const ASelection: TPersistentSelectionList);
    procedure CreateSplitter(TopSplitter: Boolean);
    procedure DestroyNoteBook;
    procedure CreateNoteBook;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure DoModified(Sender: TObject);
    function GetSelectedPersistent: TPersistent;
    function GetComponentEditorForSelection: TBaseComponentEditor;
    property ComponentEditor: TBaseComponentEditor read FComponentEditor write SetComponentEditor;
  public
    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
    procedure RefreshSelection;
    procedure RefreshComponentTreeSelection;
    procedure SaveChanges;
    procedure RefreshPropertyValues;
    procedure RebuildPropertyLists;
    procedure FillPersistentComboBox;
    procedure BeginUpdate;
    procedure EndUpdate;
    function GetActivePropertyGrid: TOICustomPropertyGrid;
    function GetActivePropertyRow: TOIPropertyGridRow;
    function GetCurRowDefaultValue(var DefaultStr: string): boolean;
    procedure HookRefreshPropertyValues;
    procedure ActivateGrid(Grid: TOICustomPropertyGrid);
    procedure FocusGrid(Grid: TOICustomPropertyGrid = nil);

    property AutoShow: Boolean read FAutoShow write FAutoShow;
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
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
    property OnPropertyHint: TOIPropertyHint read FOnPropertyHint write FOnPropertyHint;
    property OnShowOptions: TNotifyEvent read FOnShowOptions
                                         write SetOnShowOptions;
    property OnRemainingKeyUp: TKeyEvent read FOnRemainingKeyUp
                                         write FOnRemainingKeyUp;
    property OnRemainingKeyDown: TKeyEvent read FOnRemainingKeyDown
                                         write FOnRemainingKeyDown;
    property OnUpdateRestricted: TNotifyEvent read FOnUpdateRestricted
                                         write FOnUpdateRestricted;
    property ShowComponentTree: boolean read FShowComponentTree
                                        write SetShowComponentTree;
    property ShowFavorites: Boolean read FShowFavorites write SetShowFavorites;
    property ShowRestricted: Boolean read FShowRestricted write SetShowRestricted;
    property ComponentTreeHeight: integer read GetComponentTreeHeight
                                          write SetComponentTreeHeight;
    property InfoBoxHeight: integer read GetInfoBoxHeight write SetInfoBoxHeight;
    property ShowStatusBar: Boolean read FShowStatusBar write SetShowStatusBar;
    property ShowInfoBox: Boolean read FShowInfoBox write SetShowInfoBox;
    property GridControl[Page: TObjectInspectorPage]: TOICustomPropertyGrid
                                                            read GetGridControl;
    property Favourites: TOIFavouriteProperties read FFavourites write SetFavourites;
    property RestrictedProps: TOIRestrictedProperties read FRestricted write SetRestricted;
    property OnAddToFavourites: TNotifyEvent read FOnAddToFavourites
                                             write FOnAddToFavourites;
    property OnRemoveFromFavourites: TNotifyEvent read FOnRemoveFromFavourites
                                                  write FOnRemoveFromFavourites;
    property OnViewRestricted: TNotifyEvent read FOnViewRestricted write FOnViewRestricted;
    property OnOIKeyDown: TKeyEvent read FOnOIKeyDown write FOnOIKeyDown;
    property OnFindDeclarationOfProperty: TNotifyEvent
           read FOnFindDeclarationOfProperty write FOnFindDeclarationOfProperty;
    property OnAutoShow: TNotifyEvent read FOnAutoShow write FOnAutoShow;
  end;

const
  DefaultObjectInspectorName: string = 'ObjectInspectorDlg';

// the ObjectInspector of the IDE can be found in FormEditingIntf

implementation

{$R *.lfm}

uses
  math;

type
  TOIHintWindow = class(THintWindow)
  public
    property OnMouseDown;
  end;

const
  DefaultOIPageNames: array[TObjectInspectorPage] of shortstring = (
    'PropertyPage',
    'EventPage',
    'FavouritePage',
    'RestrictedPage'
    );
  DefaultOIGridNames: array[TObjectInspectorPage] of shortstring = (
    'PropertyGrid',
    'EventGrid',
    'FavouriteGrid',
    'RestrictedGrid'
    );

function SortGridRows(Item1, Item2 : pointer) : integer;
begin
  Result:=SysUtils.CompareText(TOIPropertyGridRow(Item1).Name,
                               TOIPropertyGridRow(Item2).Name);
end;

{ TOICustomPropertyGrid }

constructor TOICustomPropertyGrid.CreateWithParams(AnOwner:TComponent;
  APropertyEditorHook:TPropertyEditorHook; TypeFilter:TTypeKinds;
  DefItemHeight: integer);
var
  Details: TThemedElementDetails;
begin
  inherited Create(AnOwner);
  FLayout := oilHorizontal;

  FSelection:=TPersistentSelectionList.Create;
  FNotificationComponents:=TFPList.Create;
  FPropertyEditorHook:=APropertyEditorHook;
  FFilter:=TypeFilter;
  FItemIndex:=-1;
  FStates:=[];
  FColumn := oipgcValue;
  FRows:=TFPList.Create;
  FExpandingRow:=nil;
  FDragging:=false;
  FExpandedProperties:=TStringList.Create;
  FCurrentEdit:=nil;
  FCurrentButton:=nil;

  // visible values
  FTopY:=0;
  FSplitterX:=100;
  FPreferredSplitterX:=FSplitterX;
  Details := ThemeServices.GetElementDetails(ttGlyphOpened);
  FIndent := ThemeServices.GetDetailSize(Details).cx;

  FBackgroundColor:=DefBackgroundColor;
  FReferencesColor:=DefReferencesColor;
  FSubPropertiesColor:=DefSubPropertiesColor;
  FReadOnlyColor:=DefReadOnlyColor;
  FHighlightColor:=DefHighlightColor;
  FGutterColor:=DefGutterColor;
  FGutterEdgeColor:=DefGutterEdgeColor;

  FNameFont:=TFont.Create;
  FNameFont.Color:=DefNameColor;
  FValueFont:=TFont.Create;
  FValueFont.Color:=DefValueColor;
  FDefaultValueFont:=TFont.Create;
  FDefaultValueFont.Color:=DefDefaultValueColor;
  FHighlightFont:=TFont.Create;
  FHighlightFont.Color:=DefHighlightFontColor;

  FDrawHorzGridLines := True;
  FShowGutter := True;

  SetInitialBounds(0,0,200,130);
  ControlStyle:=ControlStyle+[csAcceptsControls,csOpaque];
  BorderWidth:=0;
  BorderStyle := bsSingle;

  // create sub components
  ValueEdit:=TEdit.Create(Self);
  with ValueEdit do
  begin
    Name:='ValueEdit';
    Visible:=false;
    Enabled:=false;
    AutoSize:=false;
    SetBounds(0,-30,80,25); // hidden
    Parent:=Self;
    OnMouseDown := @ValueControlMouseDown;
    OnMouseMove := @ValueControlMouseMove;
    OnDblClick := @ValueEditDblClick;
    OnExit:=@ValueEditExit;
    OnChange:=@ValueEditChange;
    OnKeyDown:=@ValueEditKeyDown;
    OnKeyUp:=@ValueEditKeyUp;
    OnMouseUp:=@ValueEditMouseUp;
  end;

  ValueComboBox:=TComboBox.Create(Self);
  with ValueComboBox do
  begin
    Name:='ValueComboBox';
    Sorted:=true;
    AutoSelect:=true;
    AutoComplete:=true;
    Visible:=false;
    Enabled:=false;
    AutoSize:=false;
    SetBounds(0,-30,Width,Height); // hidden
    DropDownCount:=20;
    Parent:=Self;
    OnMouseDown := @ValueControlMouseDown;
    OnMouseMove := @ValueControlMouseMove;
    OnDblClick := @ValueEditDblClick;
    OnExit:=@ValueComboBoxExit;
    //OnChange:=@ValueComboBoxChange; the on change event is called even,
                                   // if the user is still editing
    OnKeyDown:=@ValueComboBoxKeyDown;
    OnKeyUp:=@ValueComboBoxKeyUp;
    OnMouseUp:=@ValueComboBoxMouseUp;
    OnGetItems:=@ValueComboBoxGetItems;
    OnCloseUp:=@ValueComboBoxCloseUp;
    OnMeasureItem:=@ValueComboBoxMeasureItem;
    OnDrawItem:=@ValueComboBoxDrawItem;
  end;

  ValueCheckBox:=TCheckBox.Create(Self);
  with ValueCheckBox do
  begin
    Name:='ValueCheckBox';
    Visible:=false;
    AutoSize:=false;
    Enabled:=false;
    SetBounds(0,-30,Width,Height); // hidden
    Parent:=Self;
    OnMouseDown := @ValueControlMouseDown;
    OnMouseMove := @ValueControlMouseMove;
    OnExit:=@ValueCheckBoxExit;
    OnKeyDown:=@ValueCheckBoxKeyDown;
    OnKeyUp:=@ValueCheckBoxKeyUp;
  end;

  ValueButton:=TSpeedButton.Create(Self);
  with ValueButton do
  begin
    Name:='ValueButton';
    Visible:=false;
    Enabled:=false;
    Transparent:=false;
    OnClick:=@ValueButtonClick;
    Caption := '...';
    SetBounds(0,-30,Width,Height); // hidden
    Parent:=Self;
  end;

  FActiveRowBmp := CreateBitmapFromLazarusResource('pg_active_row');

  if DefItemHeight<3 then
    FDefaultItemHeight:=ValueComboBox.Height-3
  else
    FDefaultItemHeight:=DefItemHeight;

  BuildPropertyList;

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
    ScrollInfo.nMax := TopMax+ClientHeight-1;
    ScrollInfo.nPage := ClientHeight;
    if ScrollInfo.nPage<1 then ScrollInfo.nPage:=1;
    if TopY > ScrollInfo.nMax then TopY:=ScrollInfo.nMax;
    ScrollInfo.nPos := TopY;
    ShowScrollBar(Handle, SB_VERT, True);
    SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
  end;
end;

function TOICustomPropertyGrid.FillComboboxItems: boolean;
var
  ExcludeUpdateFlag: boolean;
  CurRow: TOIPropertyGridRow;
begin
  Result:=false;
  ExcludeUpdateFlag:=not (pgsUpdatingEditControl in FStates);
  Include(FStates,pgsUpdatingEditControl);
  ValueComboBox.Items.BeginUpdate;
  try
    CurRow:=Rows[FItemIndex];
    if FNewComboBoxItems<>nil then FNewComboBoxItems.Clear;
    CurRow.Editor.GetValues(@AddStringToComboBox);
    if FNewComboBoxItems<>nil then begin
      FNewComboBoxItems.Sorted:=paSortList in CurRow.Editor.GetAttributes;
      if ValueComboBox.Items.Equals(FNewComboBoxItems) then exit;
      ValueComboBox.Items.Assign(FNewComboBoxItems);
      //debugln('TOICustomPropertyGrid.FillComboboxItems "',FNewComboBoxItems.Text,'" Cur="',ValueComboBox.Items.Text,'" ValueComboBox.Items.Count=',dbgs(ValueComboBox.Items.Count));
    end else if ValueComboBox.Items.Count=0 then begin
      exit;
    end else begin
      ValueComboBox.Items.Text:='';
      ValueComboBox.Items.Clear;
      //debugln('TOICustomPropertyGrid.FillComboboxItems FNewComboBoxItems=nil Cur="',ValueComboBox.Items.Text,'" ValueComboBox.Items.Count=',dbgs(ValueComboBox.Items.Count));
    end;
    Result:=true;
    //debugln(['TOICustomPropertyGrid.FillComboboxItems CHANGED']);
  finally
    FreeAndNil(FNewComboBoxItems);
    ValueComboBox.Items.EndUpdate;
    if ExcludeUpdateFlag then
      Exclude(FStates,pgsUpdatingEditControl);
  end;
end;

procedure TOICustomPropertyGrid.CreateParams(var Params: TCreateParams);
const
  ClassStylesOff = CS_VREDRAW or CS_HREDRAW;
begin
  inherited CreateParams(Params);
  with Params do begin
    {$IFOPT R+}{$DEFINE RangeChecking}{$ENDIF}
    {$R-}
    WindowClass.Style := WindowClass.Style and not ClassStylesOff;
    Style := Style or WS_VSCROLL or WS_CLIPCHILDREN;
    {$IFDEF RangeChecking}{$R+}{$UNDEF RangeChecking}{$ENDIF}
    ExStyle := ExStyle or WS_EX_CLIENTEDGE;
  end;
end;

procedure TOICustomPropertyGrid.CreateWnd;
begin
  inherited CreateWnd;
  // handle just created, set scrollbar
  UpdateScrollBar;
end;

procedure TOICustomPropertyGrid.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  i: LongInt;
begin
  if (Operation=opRemove) and (FNotificationComponents<>nil) then begin
    FNotificationComponents.Remove(AComponent);
    i:=FSelection.IndexOf(AComponent);
    if i>=0 then begin
      FSelection.Delete(i);
      Include(FStates,pgsBuildPropertyListNeeded);
    end;
  end;
  inherited Notification(AComponent, Operation);
end;

procedure TOICustomPropertyGrid.WMVScroll(var Msg: TLMScroll);
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
    SB_ENDSCROLL:  SetCaptureControl(nil); // release scrollbar capture
  end;
end;

procedure TOICustomPropertyGrid.WMMouseWheel(var Message: TLMMouseEvent);
begin
  // -1 : scroll by page
  if Mouse.WheelScrollLines=-1
    then TopY := TopY -
              (Message.WheelDelta * (ClientHeight - DefaultItemHeight)) div 120
    // scrolling one line -> scroll half an item, see SB_LINEDOWN and SB_LINEUP
    // handler in WMVScrol
    else TopY := TopY -
              (Message.WheelDelta * Mouse.WheelScrollLines*DefaultItemHeight) div 240;
  Message.Result := 1;
end;

destructor TOICustomPropertyGrid.Destroy;
var
  a: integer;
begin
  SetIdleEvent(false);
  Application.RemoveOnUserInputHandler(@OnUserInput);
  FItemIndex := -1;
  for a := 0 to FRows.Count - 1 do
    Rows[a].Free;
  FreeAndNil(FRows);
  FreeAndNil(FSelection);
  FreeAndNil(FNotificationComponents);
  FreeAndNil(FValueFont);
  FreeAndNil(FDefaultValueFont);
  FreeAndNil(FNameFont);
  FreeAndNil(FHighlightFont);
  FreeAndNil(FExpandedProperties);
  FreeAndNil(FHintTimer);
  FreeAndNil(FHintWindow);
  FreeAndNil(FNewComboBoxItems);
  FreeAndNil(FActiveRowBmp);
  inherited Destroy;
end;

function TOICustomPropertyGrid.InitHints: boolean;
begin
  if not ShowHint then exit(false);

  Result := true;
  if FHintTimer = nil then 
  begin
    FHintIndex := -1;
    FShowingLongHint := False;
    FHintTimer := TTimer.Create(nil);
    FHintTimer.Interval := 500;
    FHintTimer.Enabled := False;
    FHintTimer.OnTimer := @HintTimer;

    FHintWindow := TOIHintWindow.Create(Self);

    TOIHintWindow(FHintWindow).OnMouseDown := @HintMouseDown;
    FHIntWindow.Visible := False;
    FHintWindow.Caption := 'This is a hint window'#13#10'Neat huh?';
    FHintWindow.HideInterval := 4000;
    FHintWindow.AutoHide := True;
  end
end;

function TOICustomPropertyGrid.IsCurrentEditorAvailable: Boolean;
begin
  Result := (FCurrentEdit <> nil) and InRange(FItemIndex, 0, FRows.Count - 1);
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
  if ASelection=nil then exit;
  if (not ASelection.ForceUpdate) and FSelection.IsEqual(ASelection) then exit;

  OldSelectedRowPath:=PropertyPath(ItemIndex);
  ItemIndex:=-1;
  ClearRows;
  FSelection.Assign(ASelection);
  UpdateSelectionNotifications;
  BuildPropertyList;
  CurRow:=GetRowByPath(OldSelectedRowPath);
  if CurRow<>nil then
    ItemIndex:=CurRow.Index;
  Column := oipgcValue;
end;

procedure TOICustomPropertyGrid.SetPropertyEditorHook(
  NewPropertyEditorHook:TPropertyEditorHook);
begin
  if FPropertyEditorHook=NewPropertyEditorHook then exit;
  FPropertyEditorHook:=NewPropertyEditorHook;
  IncreaseChangeStep;
  SetSelection(FSelection);
end;

procedure TOICustomPropertyGrid.UpdateSelectionNotifications;
var
  i: Integer;
  AComponent: TComponent;
begin
  for i:=0 to FSelection.Count-1 do begin
    if FSelection[i] is TComponent then begin
      AComponent:=TComponent(FSelection[i]);
      if FNotificationComponents.IndexOf(AComponent)<0 then begin
        FNotificationComponents.Add(AComponent);
        AComponent.FreeNotification(Self);
      end;
    end;
  end;
  for i:=FNotificationComponents.Count-1 downto 0 do begin
    AComponent:=TComponent(FNotificationComponents[i]);
    if FSelection.IndexOf(AComponent)<0 then begin
      FNotificationComponents.Delete(i);
      AComponent.RemoveFreeNotification(Self);
    end;
  end;
  //DebugLn(['TOICustomPropertyGrid.UpdateSelectionNotifications FNotificationComponents=',FNotificationComponents.Count,' FSelection=',FSelection.Count]);
end;

function TOICustomPropertyGrid.PropertyPath(Index:integer):string;
begin
  if (Index>=0) and (Index<FRows.Count) then begin
    Result:=PropertyPath(Rows[Index]);
  end else
    Result:='';
end;

function TOICustomPropertyGrid.PropertyPath(Row: TOIPropertyGridRow): string;
begin
  if Row=nil then begin
    Result:='';
    exit;
  end;
  Result:=Row.Name;
  Row:=Row.Parent;
  while Row<>nil do begin
    Result:=Row.Name+'.'+Result;
    Row:=Row.Parent;
  end;
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
    // search name in children
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
      if s<=length(PropPath) then
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
  //debugln(['TOICustomPropertyGrid.SetRowValue A ',dbgs(FStates*[pgsChangingItemIndex,pgsApplyingValue]<>[]),' FItemIndex=',dbgs(FItemIndex),' CanEditRowValue=',CanEditRowValue]);
  if not CanEditRowValue or Rows[FItemIndex].IsReadOnly then exit;

  NewValue:=GetCurrentEditValue;

  CurRow:=Rows[FItemIndex];
  if length(NewValue)>CurRow.Editor.GetEditLimit then
    NewValue:=LeftStr(NewValue,CurRow.Editor.GetEditLimit);

  //DebugLn(['TOICustomPropertyGrid.SetRowValue Old="',CurRow.Editor.GetVisualValue,'" New="',NewValue,'"']);
  if CurRow.Editor.GetVisualValue=NewValue then exit;

  OldChangeStep:=fChangeStep;
  Include(FStates,pgsApplyingValue);
  try
    {$IFNDEF DoNotCatchOIExceptions}
    try
    {$ENDIF}
      //debugln(['TOICustomPropertyGrid.SetRowValue B ClassName=',CurRow.Editor.ClassName,' Visual=',CurRow.Editor.GetVisualValue,' NewValue=',NewValue,' AllEqual=',CurRow.Editor.AllEqual]);
      CurRow.Editor.SetValue(NewValue);
      //debugln(['TOICustomPropertyGrid.SetRowValue C ClassName=',CurRow.Editor.ClassName,' Visual=',CurRow.Editor.GetVisualValue,' NewValue=',NewValue,' AllEqual=',CurRow.Editor.AllEqual]);
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
    SetCurrentEditValue(CurRow.Editor.GetVisualValue);

    // update volatile sub properties
    if (paVolatileSubProperties in CurRow.Editor.GetAttributes)
    and ((CurRow.Expanded) or (CurRow.ChildCount>0)) then begin
      OldExpanded:=CurRow.Expanded;
      ShrinkRow(FItemIndex);
      if OldExpanded then
        ExpandRow(FItemIndex);
    end;
    //debugln(['TOICustomPropertyGrid.SetRowValue D ClassName=',CurRow.Editor.ClassName,' Visual=',CurRow.Editor.GetVisualValue,' NewValue=',NewValue,' AllEqual=',CurRow.Editor.AllEqual]);
  finally
    Exclude(FStates,pgsApplyingValue);
  end;
  if FPropertyEditorHook=nil then
    DoPaint(true)
  else
    FPropertyEditorHook.RefreshPropertyValues;
  //DebugLn(['TOICustomPropertyGrid.SetRowValue ',CurRow.Name,' ',CurRow.Editor.GetVisualValue,' ',Assigned(FOnModified)]);
  if Assigned(FOnModified) then FOnModified(Self);
end;

procedure TOICustomPropertyGrid.DoCallEdit(Edit: TOIQuickEdit);
var
  CurRow:TOIPropertyGridRow;
  OldChangeStep: integer;
begin
  //writeln('#################### TOICustomPropertyGrid.DoCallEdit ...');
  if not CanEditRowValue then exit;

  OldChangeStep:=fChangeStep;
  CurRow:=Rows[FItemIndex];
  if paDialog in CurRow.Editor.GetAttributes then begin
    {$IFNDEF DoNotCatchOIExceptions}
    try
    {$ENDIF}
      DebugLn(['#################### TOICustomPropertyGrid.DoCallEdit for ',CurRow.Editor.ClassName,' Edit=',Edit=oiqeEdit]);
      Include(FStates,pgsApplyingValue);
      try
        if Edit=oiqeShowValue then
          CurRow.Editor.ShowValue
        else
          CurRow.Editor.Edit;
      finally
        Exclude(FStates,pgsApplyingValue);
      end;
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
      RefreshPropertyValues;
      exit;
    end;

    // update value
    RefreshValueEdit;

    //invalidate changed subproperties
    DoPaint(True);
  end;
end;

procedure TOICustomPropertyGrid.RefreshValueEdit;
var
  CurRow: TOIPropertyGridRow;
  NewValue: string;
begin
  if not GridIsUpdating and IsCurrentEditorAvailable then begin
    CurRow:=Rows[FItemIndex];
    NewValue:=CurRow.Editor.GetVisualValue;
    SetCurrentEditValue(NewValue);
  end;
end;

procedure TOICustomPropertyGrid.ValueEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  ScrollToActiveItem;
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
var CurRow: TOIPropertyGridRow;
begin
  if (pgsUpdatingEditControl in FStates) or not IsCurrentEditorAvailable then exit;
  CurRow:=Rows[FItemIndex];
  if paAutoUpdate in CurRow.Editor.GetAttributes then
    SetRowValue;
end;

procedure TOICustomPropertyGrid.ValueEditMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button=mbLeft) and (Shift=[ssCtrl,ssLeft]) then
    DoCallEdit(oiqeShowValue);
end;

procedure TOICustomPropertyGrid.ValueCheckBoxKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  ScrollToActiveItem;
  HandleStandardKeys(Key,Shift);
end;

procedure TOICustomPropertyGrid.ValueCheckBoxKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  HandleKeyUp(Key,Shift);
end;

procedure TOICustomPropertyGrid.ValueCheckBoxExit(Sender: TObject);
begin
  SetRowValue;
end;

procedure TOICustomPropertyGrid.ValueCheckBoxClick(Sender: TObject);
var
  CurRow: TOIPropertyGridRow;
begin
  if (pgsUpdatingEditControl in FStates) or not IsCurrentEditorAvailable then exit;
  CurRow:=Rows[FItemIndex];
  if paAutoUpdate in CurRow.Editor.GetAttributes then
    SetRowValue;
end;

procedure TOICustomPropertyGrid.ValueComboBoxExit(Sender: TObject);
begin
  if pgsUpdatingEditControl in FStates then exit;
  SetRowValue;
end;

procedure TOICustomPropertyGrid.ValueComboBoxKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  ScrollToActiveItem;
  HandleStandardKeys(Key,Shift);
end;

procedure TOICustomPropertyGrid.ValueComboBoxKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  HandleKeyUp(Key,Shift);
end;

procedure TOICustomPropertyGrid.ValueComboBoxMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button=mbLeft) then begin
    if (Shift=[ssCtrl,ssLeft]) then
      DoCallEdit(oiqeShowValue)
    else if (FFirstClickTime<>0) and (Now-FFirstClickTime<(1/86400*0.4)) then
      ValueEditDblClick(Sender);
  end;
end;

procedure TOICustomPropertyGrid.ValueButtonClick(Sender: TObject);
begin
  ScrollToActiveItem;
  DoCallEdit;
end;

procedure TOICustomPropertyGrid.ValueComboBoxMeasureItem(Control: TWinControl;
  Index: Integer; var AHeight: Integer);
var
  CurRow: TOIPropertyGridRow;
begin
  if (FItemIndex >= 0) and (FItemIndex < FRows.Count) then
  begin
    CurRow := Rows[FItemIndex];
    CurRow.Editor.ListMeasureHeight('Fj', Index, ValueComboBox.Canvas, AHeight);
    AHeight := Max(AHeight, ValueComboBox.ItemHeight);
  end;
end;

procedure TOICustomPropertyGrid.SetItemIndex(NewIndex:integer);
var
  NewRow: TOIPropertyGridRow;
  NewValue: string;
  EditorAttributes: TPropertyAttributes;
begin
  if GridIsUpdating or (FItemIndex = NewIndex) then
    exit;

  // save old edit value
  SetRowValue;

  Include(FStates, pgsChangingItemIndex);
  if (FItemIndex >= 0) and (FItemIndex < FRows.Count) then
    Rows[FItemIndex].Editor.Deactivate;
  if CanFocus then
    SetCaptureControl(nil);

  FItemIndex := NewIndex;
  if FCurrentEdit <> nil then
  begin
    FCurrentEdit.Visible:=false;
    FCurrentEdit.Enabled:=false;
    FCurrentEdit:=nil;
  end;
  if FCurrentButton<>nil then
  begin
    FCurrentButton.Visible:=false;
    FCurrentButton.Enabled:=false;
    FCurrentButton:=nil;
  end;
  FCurrentEditorLookupRoot:=nil;
  if (NewIndex>=0) and (NewIndex<FRows.Count) then
  begin
    NewRow:=Rows[NewIndex];

    ScrollToItem(NewIndex);

    if CanFocus then
      NewRow.Editor.Activate;
    EditorAttributes:=NewRow.Editor.GetAttributes;
    if paDialog in EditorAttributes then begin
      FCurrentButton:=ValueButton;
      FCurrentButton.Visible:=true;
      //DebugLn(['TOICustomPropertyGrid.SetItemIndex FCurrentButton.BoundsRect=',dbgs(FCurrentButton.BoundsRect)]);
    end;
    NewValue:=NewRow.Editor.GetVisualValue;
    {$IFDEF UseOICheckBox}
    if (NewRow.Editor is TBoolPropertyEditor) then begin
      FCurrentEdit:=ValueCheckBox;
      ValueCheckBox.Enabled:=not NewRow.IsReadOnly;
      ValueCheckBox.Caption:=NewValue;
      ValueCheckBox.Checked:=(NewValue='True');
    end else
    {$ENDIF}
    if paValueList in EditorAttributes then begin
      FCurrentEdit:=ValueComboBox;
      if paPickList in EditorAttributes then begin
        // text field should be readonly
        if paCustomDrawn in EditorAttributes then
          ValueComboBox.Style:=csOwnerDrawVariable
        else
          ValueComboBox.Style:=csDropDownList;
      end else begin
        if paCustomDrawn in EditorAttributes then
          ValueComboBox.Style:=csOwnerDrawVariable
        else
          ValueComboBox.Style:=csDropDown;
      end;
      ValueComboBox.MaxLength:=NewRow.Editor.GetEditLimit;
      ValueComboBox.Sorted:=paSortList in NewRow.Editor.GetAttributes;
      ValueComboBox.Enabled:=not NewRow.IsReadOnly;
      // Do not fill the items here, because it can be very slow.
      // Just fill in some values and update the values, before the combobox
      // popups
      ValueComboBox.Items.Text:=NewValue;
      Exclude(FStates,pgsGetComboItemsCalled);
      SetIdleEvent(true);
      ValueComboBox.Text:=NewValue;
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
      and (not NewRow.IsReadOnly)
      and CanFocus then begin
        if (Column=oipgcValue) then
          SetActiveControl(FCurrentEdit);
      end;
    end;
    if FCurrentButton<>nil then
      FCurrentButton.Enabled:=not NewRow.IsDisabled;
  end;
  //DebugLn(['TOICustomPropertyGrid.SetItemIndex Vis=',ValueComboBox.Visible,' Ena=',ValueComboBox.Enabled,' Items.Count=',ValueComboBox.Items.Count ,' Text=',ValueComboBox.Text]);
  Exclude(FStates, pgsChangingItemIndex);
  DoSelectionChange;
  Invalidate;
end;

function TOICustomPropertyGrid.GetNameRowHeight: Integer;
begin
  Result := Abs(FNameFont.Height);
  if Result = 0
  then Result := 16;
  Inc(Result, 2); // margin
end;

function TOICustomPropertyGrid.GetRowCount:integer;
begin
  Result:=FRows.Count;
end;

procedure TOICustomPropertyGrid.BuildPropertyList(OnlyIfNeeded: boolean);
var
  a: integer;
  CurRow: TOIPropertyGridRow;
  OldSelectedRowPath: string;
begin
  if OnlyIfNeeded and (not (pgsBuildPropertyListNeeded in FStates)) then exit;
  Exclude(FStates,pgsBuildPropertyListNeeded);

  OldSelectedRowPath:=PropertyPath(ItemIndex);
  // unselect
  ItemIndex:=-1;
  // clear
  for a:=0 to FRows.Count-1 do Rows[a].Free;
  FRows.Clear;
  // get properties
  GetPersistentProperties(
    FSelection, FFilter + [tkClass], FPropertyEditorHook, @AddPropertyEditor,
    @EditorFilter);
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
  // update scrollbar
  FTopY:=0;
  UpdateScrollBar;
  // reselect
  CurRow:=GetRowByPath(OldSelectedRowPath);
  if CurRow<>nil then begin
    ItemIndex:=CurRow.Index;
  end;
  // paint
  Invalidate;
end;

procedure TOICustomPropertyGrid.AddPropertyEditor(PropEditor: TPropertyEditor);
var
  NewRow: TOIPropertyGridRow;
  WidgetSets: TLCLPlatforms;
begin
  WidgetSets := [];
  if Favourites<>nil then begin
    //debugln('TOICustomPropertyGrid.AddPropertyEditor A ',PropEditor.GetName);
    if Favourites is TOIRestrictedProperties then
    begin
      WidgetSets := (Favourites as TOIRestrictedProperties).AreRestricted(
                                                  Selection,PropEditor.GetName);
      if WidgetSets = [] then
      begin
        PropEditor.Free;
        Exit;
      end;
    end
    else
      if not Favourites.AreFavourites(Selection,PropEditor.GetName) then begin
        PropEditor.Free;
        exit;
      end;
  end;
  if PropEditor is TClassPropertyEditor then
    (PropEditor as TClassPropertyEditor).SubPropsTypeFilter := FFilter;
  NewRow := TOIPropertyGridRow.Create(Self, PropEditor, nil, WidgetSets);
  FRows.Add(NewRow);
  if FRows.Count>1 then begin
    NewRow.FPriorBrother:=Rows[FRows.Count-2];
    NewRow.FPriorBrother.FNextBrother:=NewRow;
  end;
end;

procedure TOICustomPropertyGrid.AddStringToComboBox(const s: string);
begin
  if FNewComboBoxItems=nil then
    FNewComboBoxItems:=TStringList.Create;
  FNewComboBoxItems.Add(s);
end;

procedure TOICustomPropertyGrid.ExpandRow(Index:integer);
var
  a: integer;
  CurPath: string;
  AlreadyInExpandList: boolean;
  ActiveRow: TOIPropertyGridRow;
begin
  // Save ItemIndex
  if ItemIndex <> -1 then
    ActiveRow := Rows[ItemIndex]
  else
    ActiveRow := nil;
  FExpandingRow := Rows[Index];
  if (FExpandingRow.Expanded) or (not CanExpandRow(FExpandingRow)) then
  begin
    FExpandingRow := nil;
    Exit;
  end;
  FExpandingRow.Editor.GetProperties(@AddSubEditor);
  SortSubEditors(FExpandingRow);
  SetItemsTops;
  FExpandingRow.FExpanded := True;
  a := 0;
  CurPath:=uppercase(PropertyPath(FExpandingRow.Index));
  AlreadyInExpandList:=false;
  while a < FExpandedProperties.Count do
  begin
    if FExpandedProperties[a]=copy(CurPath,1,length(FExpandedProperties[a])) then
    begin
      if Length(FExpandedProperties[a]) = Length(CurPath) then
      begin
        AlreadyInExpandList := True;
        inc(a);
      end
      else
        FExpandedProperties.Delete(a);
    end
    else
      inc(a);
  end;
  if not AlreadyInExpandList then
    FExpandedProperties.Add(CurPath);
  FExpandingRow := nil;
  // restore ItemIndex
  if ActiveRow <> nil then
    FItemIndex := ActiveRow.Index
  else
    FItemIndex := -1;
  UpdateScrollBar;
  Invalidate;
end;

procedure TOICustomPropertyGrid.ShrinkRow(Index:integer);
var
  CurRow, ARow: TOIPropertyGridRow;
  StartIndex, EndIndex, a: integer;
  CurPath: string;
begin
  CurRow := Rows[Index];
  if (not CurRow.Expanded) then
    Exit;
  // calculate all children (between StartIndex..EndIndex)
  StartIndex := CurRow.Index + 1;
  EndIndex := FRows.Count - 1;
  ARow := CurRow;
  while ARow <> nil do
  begin
    if ARow.NextBrother <> nil then
    begin
      EndIndex := ARow.NextBrother.Index - 1;
      break;
    end;
    ARow := ARow.Parent;
  end;
  if (FItemIndex >= StartIndex) and (FItemIndex <= EndIndex) then
    // current row delete, set new current row
    ItemIndex:=0
  else
  if FItemIndex > EndIndex then
    // adjust current index for deleted rows
    FItemIndex := FItemIndex - (EndIndex - StartIndex + 1);
  for a := EndIndex downto StartIndex do
  begin
    Rows[a].Free;
    FRows.Delete(a);
  end;
  SetItemsTops;
  CurRow.FExpanded := False;
  CurPath := UpperCase(PropertyPath(CurRow.Index));
  a := 0;
  while a < FExpandedProperties.Count do
  begin
    if copy(FExpandedProperties[a], 1, length(CurPath)) = CurPath then
      FExpandedProperties.Delete(a)
    else
      inc(a);
  end;
  if CurRow.Parent <> nil then
    FExpandedProperties.Add(PropertyPath(CurRow.Parent.Index));
  UpdateScrollBar;
  Invalidate;
end;

procedure TOICustomPropertyGrid.AddSubEditor(PropEditor:TPropertyEditor);
var NewRow:TOIPropertyGridRow;
  NewIndex:integer;
begin
  if PropEditor is TClassPropertyEditor then
    (PropEditor as TClassPropertyEditor).SubPropsTypeFilter := FFilter;
  NewRow:=TOIPropertyGridRow.Create(Self,PropEditor,FExpandingRow, []);
  NewIndex:=FExpandingRow.Index+1+FExpandingRow.ChildCount;
  NewRow.FIndex:=NewIndex;
  FRows.Insert(NewIndex,NewRow);
  if NewIndex<FItemIndex
    then inc(FItemIndex);
  if FExpandingRow.FFirstChild=nil then
    FExpandingRow.FFirstChild:=NewRow;
  NewRow.FPriorBrother:=FExpandingRow.FLastChild;
  FExpandingRow.FLastChild:=NewRow;
  if NewRow.FPriorBrother<>nil then
    NewRow.FPriorBrother.FNextBrother:=NewRow;
  inc(FExpandingRow.FChildCount);
end;

procedure TOICustomPropertyGrid.SortSubEditors(ParentRow: TOIPropertyGridRow);
var
  Item: TOIPropertyGridRow;
  Index: Integer;
  Next: TOIPropertyGridRow;
begin
  if not ParentRow.Sort(@SortGridRows) then exit;
  // update FRows
  Item:=ParentRow.FirstChild;
  Index:=ParentRow.Index+1;
  Next:=ParentRow.NextSkipChilds;
  while (Item<>nil) and (Item<>Next) do begin
    FRows[Index]:=Item;
    Item.FIndex:=Index;
    Item:=Item.Next;
    inc(Index);
  end;
end;

function TOICustomPropertyGrid.CanExpandRow(Row: TOIPropertyGridRow): boolean;
var
  AnObject: TPersistent;
  ParentRow: TOIPropertyGridRow;
begin
  Result:=false;
  if (Row=nil) or (Row.Editor=nil) then exit;
  if (not (paSubProperties in Row.Editor.GetAttributes)) then exit;
  // check if circling
  if (Row.Editor is TPersistentPropertyEditor) then begin
    AnObject:=TPersistent(Row.Editor.GetObjectValue);
    if FSelection.IndexOf(AnObject)>=0 then exit;
    ParentRow:=Row.Parent;
    while ParentRow<>nil do begin
      if (ParentRow.Editor is TPersistentPropertyEditor)
      and (ParentRow.Editor.GetObjectValue=AnObject) then
        exit;
      ParentRow:=ParentRow.Parent;
    end;
  end;
  Result:=true;
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
  if InRange(ItemIndex,0,FRows.Count-1) then
    Result:=Rows[ItemIndex]
  else
    Result:=nil;
end;

procedure TOICustomPropertyGrid.SetCurrentRowValue(const NewValue: string);
begin
  if not CanEditRowValue or Rows[FItemIndex].IsReadOnly then exit;
  // SetRowValue reads the value from the current edit control and writes it
  // to the property editor
  // -> set the text in the current edit control without changing FLastEditValue
  SetCurrentEditValue(NewValue);
  SetRowValue;
end;

procedure TOICustomPropertyGrid.SetItemIndexAndFocus(NewItemIndex: integer);
begin
  if not InRange(NewItemIndex, 0, FRows.Count - 1) then exit;
  ItemIndex:=NewItemIndex;
  if FCurrentEdit<>nil then
  begin
    SetActiveControl(FCurrentEdit);
    if (FCurrentEdit is TCustomEdit) then
      TCustomEdit(FCurrentEdit).SelectAll;
  end;
end;

function TOICustomPropertyGrid.CanEditRowValue: boolean;
begin
  Result:=
    not GridIsUpdating and IsCurrentEditorAvailable
    and ((FCurrentEditorLookupRoot = nil)
      or (FPropertyEditorHook = nil)
      or (FPropertyEditorHook.LookupRoot = FCurrentEditorLookupRoot));
  if Result then begin
    {DebugLn(['TOICustomPropertyGrid.CanEditRowValue',
      ' pgsChangingItemIndex=',pgsChangingItemIndex in FStates,
      ' pgsApplyingValue=',pgsApplyingValue in FStates,
      ' pgsUpdatingEditControl=',pgsUpdatingEditControl in FStates,
      ' FCurrentEdit=',dbgsName(FCurrentEdit),
      ' FItemIndex=',FItemIndex,
      ' FCurrentEditorLookupRoot=',dbgsName(FCurrentEditorLookupRoot),
      ' FPropertyEditorHook.LookupRoot=',dbgsName(FPropertyEditorHook.LookupRoot)
      ]);}
  end;
end;

procedure TOICustomPropertyGrid.SaveChanges;
begin
  SetRowValue;
end;

function TOICustomPropertyGrid.GetHintTypeAt(RowIndex: integer; X: integer): TPropEditHint;
var
  IconX: integer;
begin
  Result := pehNone;
  if (RowIndex < 0) or (RowIndex >= RowCount) then 
    Exit;
  if SplitterX <= X then 
  begin
    if (FCurrentButton <> nil) and (FCurrentButton.Left <= X) then
      Result := pehEditButton
    else
      Result := pehValue;
  end else 
  begin
    IconX := GetTreeIconX(RowIndex);
    if IconX + Indent > X then
      Result := pehTree
    else
      Result := pehName;
  end;
end;

procedure TOICustomPropertyGrid.MouseDown(Button:TMouseButton;  Shift:TShiftState;
  X,Y:integer);
var
  IconX,Index:integer;
  PointedRow:TOIpropertyGridRow;
begin
  //ShowMessageDialog('X'+IntToStr(X)+',Y'+IntToStr(Y));
  inherited MouseDown(Button,Shift,X,Y);

  HideHint;

  if Button=mbLeft then begin
    FFirstClickTime:=Now;
    if Cursor=crHSplit then begin
      FDragging:=true;
    end
    else
    begin
      Index:=MouseToIndex(Y,false);
      if (Index>=0) and (Index<FRows.Count) then
      begin
        PointedRow:=Rows[Index];
        if CanExpandRow(PointedRow) then
        begin
          IconX:=GetTreeIconX(Index);
          if ((X>=IconX) and (X<=IconX+FIndent)) or (ssDouble in Shift) then
          begin
            if PointedRow.Expanded then
              ShrinkRow(Index)
            else
              ExpandRow(Index);
          end;
        end;

        SetItemIndexAndFocus(Index);
        SetCaptureControl(Self);
        Column := oipgcValue;
      end;
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
  end
  else
  begin
    if (abs(SplitDistance)<=2) then begin
      Cursor:=crHSplit;
    end else begin
      Cursor:=crDefault;
    end;

    if ssLeft in Shift then
    begin
      Index := MouseToIndex(Y, False);
      SetItemIndexAndFocus(Index);
      SetCaptureControl(Self);
    end;

    // to check if the property text fits in its box, if not show a hint
    if ShowHint then 
    begin
      Index := MouseToIndex(y,false);
      if (Index > -1)
      and (not FShowingLongHint)
      and ((FHintWindow=nil) or (not FHintWindow.Visible)
           or (Index<>FHintIndex))
      then begin
        FHintIndex:=Index;
        FShowingLongHint:=false;
        fPropRow := GetRow(Index);
        if X < SplitterX then 
        begin
          // Mouse is over property name...
          fHint := fPropRow.Name;
          if InitHints and ((Canvas.TextWidth(fHint) + BorderWidth + GetTreeIconX(Index) + Indent) >= SplitterX) then
          begin
            fHintRect := FHintWindow.CalcHintRect(0,fHint,nil);
            fPoint := ClientToScreen(
                                   Point(BorderWidth+GetTreeIconX(Index)+Indent,
                                   fPropRow.Top - TopY-1));
            MoveRect(fHintRect,fPoint.x,fPoint.y);
            FHintWindow.ActivateHint(fHintRect,fHint);
          end;
        end
        else 
        begin
          // Mouse is over property value...
          fHint := fPropRow.LastPaintedValue;
          if length(fHint) > 100 then fHint := copy(fHint, 1, 100) + '...';
          if (Canvas.TextWidth(fHint) > (ClientWidth - BorderWidth - SplitterX)) and 
             InitHints then 
          begin
            fHintRect := FHintWindow.CalcHintRect(0,fHint,nil);
            fpoint := ClientToScreen(Point(SplitterX, fPropRow.Top - TopY - 1));
            MoveRect(fHintRect, fPoint.x, fPoint.y);
            FHintWindow.ActivateHint(fHintRect, fHint);
          end;
        end;
      end;
    end;
  end;
end;

procedure TOICustomPropertyGrid.MouseUp(Button:TMouseButton;  Shift:TShiftState;
  X,Y:integer);
begin
  if FDragging then EndDragSplitter;
  SetCaptureControl(nil);
  inherited MouseUp(Button,Shift,X,Y);
end;

procedure TOICustomPropertyGrid.KeyDown(var Key: Word; Shift: TShiftState);
begin
  HandleStandardKeys(Key,Shift);
  inherited KeyDown(Key, Shift);
end;

procedure TOICustomPropertyGrid.HandleStandardKeys(
  var Key: Word; Shift: TShiftState);

var
  Handled: Boolean;

  procedure FindPropertyByFirstLetter;
  var
    i: Integer;
  begin
    if Column = oipgcName then
      for i := 0 to RowCount - 1 do
        if (Rows[i].Lvl = Rows[ItemIndex].Lvl) and
          (Ord(upCase(Rows[i].Name[1])) = Key) then
        begin
          SetItemIndexAndFocus(i);
          exit;
        end;
    Handled := false;
  end;

  procedure HandleUnshifted;
  const
    Page = 20;
  begin
    Handled := true;
    case Key of
      VK_UP   : SetItemIndexAndFocus(ItemIndex - 1);
      VK_DOWN : SetItemIndexAndFocus(ItemIndex + 1);
      VK_PRIOR: SetItemIndexAndFocus(Max(ItemIndex - Page, 0));
      VK_NEXT : SetItemIndexAndFocus(Min(ItemIndex + Page, FRows.Count - 1));

      VK_TAB: DoTabKey;

      VK_RETURN:
        begin
          SetRowValue;
          if FCurrentEdit is TCustomEdit then
            TCustomEdit(FCurrentEdit).SelectAll;
        end;

      VK_ESCAPE: RefreshValueEdit;

      Ord('A')..Ord('Z'): FindPropertyByFirstLetter;

      else
        Handled := false;
    end;
  end;

begin
  //writeln('TOICustomPropertyGrid.HandleStandardKeys ',Key);
  Handled := false;
  if (Shift = []) or (Shift = [ssShift]) then
  begin
    if not (FCurrentEdit is TCustomCombobox) or
       not TCustomCombobox(FCurrentEdit).DroppedDown then
      HandleUnshifted;
  end
  else
  if Shift = [ssCtrl] then
  begin
    case Key of
      VK_RETURN:
        begin
          ToggleRow;
          Handled := true;
        end;
    end;
  end
  else 
  if Shift = [ssAlt] then
    case Key of
      VK_LEFT:
        begin
          Handled := (ItemIndex >= 0) and Rows[ItemIndex].Expanded;
          if Handled then ShrinkRow(ItemIndex);
        end;

      VK_RIGHT:
        begin
          Handled := (ItemIndex >= 0) and not Rows[ItemIndex].Expanded and
            CanExpandRow(Rows[ItemIndex]);
          if Handled then ExpandRow(ItemIndex)
        end;
    end;


  if not Handled and Assigned(OnOIKeyDown) then
  begin
    OnOIKeyDown(Self, Key, Shift);
    Handled := Key = VK_UNKNOWN;
  end;

  //writeln('TOICustomPropertyGrid.HandleStandardKeys ',Key,' Handled=',Handled);
  if Handled then
    Key := VK_UNKNOWN;
end;

procedure TOICustomPropertyGrid.HandleKeyUp(var Key: Word; Shift: TShiftState);
begin
  if (Key<>VK_UNKNOWN) and Assigned(OnKeyUp) then OnKeyUp(Self,Key,Shift);
end;

procedure TOICustomPropertyGrid.DoTabKey;
begin
  if Column = oipgcValue then 
  begin
    Column := oipgcName;
    Self.SetFocus;
  end else 
  begin
    Column := oipgcValue;
    if FCurrentEdit <> nil then
      FCurrentEdit.SetFocus;
  end;
end;

function TOICustomPropertyGrid.EditorFilter(
  const AEditor: TPropertyEditor): Boolean;
begin
  Result := IsInteresting(AEditor, FFilter);
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

procedure TOICustomPropertyGrid.DoSelectionChange;
begin
  if Assigned(FOnSelectionChange) then
    OnSelectionChange(Self);
end;

constructor TOICustomPropertyGrid.Create(TheOwner: TComponent);
begin
  CreateWithParams(TheOwner,nil,AllTypeKinds,25);
end;

procedure TOICustomPropertyGrid.OnUserInput(Sender: TObject; Msg: Cardinal);
begin
  ResetHintTimer;
end;

procedure TOICustomPropertyGrid.HintMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pos: TPoint;
begin
  pos := ScreenToClient(FHintWindow.ClientToScreen(Point(X, Y)));
  MouseDown(Button, Shift, pos.X, pos.Y);
end;

procedure TOICustomPropertyGrid.EndDragSplitter;
begin
  if FDragging then begin
    Cursor:=crDefault;
    FDragging:=false;
    FPreferredSplitterX:=FSplitterX;
    if FCurrentEdit<>nil then begin
      SetCaptureControl(nil);
      if Column=oipgcValue then
        FCurrentEdit.SetFocus
      else
        Self.SetFocus;
    end;
  end;
end;

procedure TOICustomPropertyGrid.SetReadOnlyColor(const AValue: TColor);
begin
  if FReadOnlyColor = AValue then Exit;
  FReadOnlyColor := AValue;
  Invalidate;
end;

procedure TOICustomPropertyGrid.SetRowSpacing(const AValue: integer);
begin
  if FRowSpacing = AValue then exit;
  FRowSpacing := AValue;
  SetItemsTops;
end;

procedure TOICustomPropertyGrid.SetShowGutter(const AValue: Boolean);
begin
  if FShowGutter=AValue then exit;
  FShowGutter:=AValue;
  invalidate;
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
var
  NewTopY: integer;
begin
  NewTopY := TopMax;
  if NewValue < NewTopY then
    NewTopY := NewValue;
  if NewTopY < 0 then
    NewTopY := 0;
  if FTopY<>NewTopY then begin
    FTopY:=NewTopY;
    UpdateScrollBar;
    AlignEditComponents;
    Invalidate;
  end;
end;

function TOICustomPropertyGrid.GetPropNameColor(ARow:TOIPropertyGridRow):TColor;

 function HasWriter(APropInfo: PPropInfo): Boolean; inline;
 begin
   Result := Assigned(APropInfo) and Assigned(APropInfo^.SetProc);
 end;

var
  ParentRow:TOIPropertyGridRow;
  IsObjectSubProperty:Boolean;
begin
  // Try to guest if ARow, or one of its parents, is a subproperty
  // of an object (and not an item of a set)
  IsObjectSubProperty:=false;
  ParentRow:=ARow.Parent;
  while Assigned(ParentRow) do
  begin
    if ParentRow.Editor is TPersistentPropertyEditor then
      IsObjectSubProperty:=true;
    ParentRow:=ParentRow.Parent;
  end;

  if (ItemIndex <> -1) and (ItemIndex = ARow.Index) then
    Result := FHighlightFont.Color
  else
  if not HasWriter(ARow.Editor.GetPropInfo) then
    Result := FReadOnlyColor
  else
  if ARow.Editor is TPersistentPropertyEditor then
    Result := FReferencesColor
  else
  if IsObjectSubProperty then
    Result := FSubPropertiesColor
  else
    Result := FNameFont.Color;
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
  Result:=GridHeight-ClientHeight+2*integer(BorderWidth);
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
  if ItemIndex>=0
  then begin
    RRect := RowRect(ItemIndex);
    EditCompRect := RRect;
    EditCompRect.Bottom := EditCompRect.Bottom - 1;

    if Layout = oilHorizontal
    then begin
      EditCompRect.Left := RRect.Left + SplitterX;
    end
    else begin
      EditCompRect.Top := RRect.Top + GetNameRowHeight;
      EditCompRect.Left := RRect.Left + GetTreeIconX(ItemIndex) + Indent;
    end;

    if FCurrentButton<>nil then begin
      // edit dialog button
      with EditBtnRect do begin
        Top := EditCompRect.Top;
        Left := EditCompRect.Right - 20;
        Bottom := EditCompRect.Bottom;
        Right := EditCompRect.Right;
        EditCompRect.Right := Left;
      end;
      if not CompareRectangles(FCurrentButton.BoundsRect,EditBtnRect) then begin
        FCurrentButton.BoundsRect:=EditBtnRect;
      end;
      //DebugLn(['TOICustomPropertyGrid.AlignEditComponents FCurrentButton.BoundsRect=',dbgs(FCurrentButton.BoundsRect),' EditBtnRect=',dbgs(EditBtnRect)]);
    end;
    if FCurrentEdit<>nil then begin
      // resize the edit component
      Dec(EditCompRect.Left);
      Dec(EditCompRect.Top);
      Inc(EditCompRect.Bottom);
      //debugln('TOICustomPropertyGrid.AlignEditComponents A ',dbgsName(FCurrentEdit),' ',dbgs(EditCompRect));
      if not CompareRectangles(FCurrentEdit.BoundsRect,EditCompRect) then begin
        FCurrentEdit.BoundsRect:=EditCompRect;
        if FCurrentEdit is TComboBox then
          TComboBox(FCurrentEdit).ItemHeight:=
                                         EditCompRect.Bottom-EditCompRect.Top-6;
        FCurrentEdit.Invalidate;
      end;
    end;
  end;
end;

procedure TOICustomPropertyGrid.PaintRow(ARow: integer);
var
  FullRect,NameRect,NameIconRect,NameTextRect,ValueRect, ParentRect:TRect;
  IconX,IconY:integer;
  CurRow:TOIPropertyGridRow;
  DrawState:TPropEditDrawState;
  OldFont:TFont;
  lclPlatform: TLCLPlatform;
  X, Y: Integer;
  NameBgColor: TColor;

  procedure DrawTreeIcon(X, Y: Integer; Minus: Boolean);
  const
    PlusMinusDetail: array[Boolean] of TThemedTreeview =
    (
      ttGlyphClosed,
      ttGlyphOpened
    );
  var
    Details: TThemedElementDetails;
    Size: TSize;
  begin
    Details := ThemeServices.GetElementDetails(PlusMinusDetail[Minus]);
    Size := ThemeServices.GetDetailSize(Details);
    ThemeServices.DrawElement(Canvas.Handle, Details, Rect(X, Y, X + Size.cx, Y + Size.cy), nil);
  end;

  procedure DrawActiveRow(X, Y: Integer);
  begin
    Canvas.Draw(X, Y, FActiveRowBmp);
  end;

// PaintRow
begin
  CurRow := Rows[ARow];
  FullRect := RowRect(ARow);
  NameRect := FullRect;
  ValueRect := FullRect;
  Inc(FullRect.Bottom, FRowSpacing);

  if ARow = FItemIndex then 
  begin
    if Layout = oilHorizontal then
    begin
      if Assigned(FCurrentButton) and (FCurrentButton.Visible) then
        Dec(FullRect.Right, FCurrentButton.Width);

      if Assigned(FCurrentEdit) and (FCurrentEdit.Visible) then
        Dec(FullRect.Right, FCurrentEdit.Width);
    end;
  end;

  if Layout = oilHorizontal
  then begin
    NameRect.Right:=SplitterX;
    ValueRect.Left:=SplitterX;
  end
  else begin
    NameRect.Bottom := NameRect.Top + GetNameRowHeight;
    ValueRect.Top := NameRect.Bottom;
  end;

  IconX:=GetTreeIconX(ARow);
  IconY:=((NameRect.Bottom-NameRect.Top-9) div 2)+NameRect.Top;
  NameIconRect := NameRect;
  NameIconRect.Right := IconX + Indent;
  NameTextRect := NameRect;
  NameTextRect.Left := NameIconRect.Right;

  if Layout = oilVertical then
    ValueRect.Left := NameTextRect.Left
  else
  begin
    inc(NameIconRect.Right, 2 + Ord(ShowGutter));
    inc(NameTextRect.Left, 3 +  + Ord(ShowGutter));
  end;

  DrawState:=[];
  if ARow = FItemIndex then
    Include(DrawState, pedsSelected);

  with Canvas do
  begin
    // clear background in one go

    if (ARow = FItemIndex) and (FHighlightColor <> clNone) then
      NameBgColor := FHighlightColor
    else
      NameBgColor := FBackgroundColor;

    if FBackgroundColor <> clNone then
    begin
      Brush.Color := FBackgroundColor;
      FillRect(FullRect);
    end;

    if ShowGutter and (Layout = oilHorizontal) and
       (FGutterColor <> FBackgroundColor) and (FGutterColor <> clNone) then
    begin
      Brush.Color := FGutterColor;
      FillRect(NameIconRect);
    end;

    // draw icon
    if CanExpandRow(CurRow) then
      DrawTreeIcon(IconX, IconY, CurRow.Expanded)
    else
    if (ARow = FItemIndex) then
      DrawActiveRow(IconX, IconY);

    // draw name
    OldFont:=Font;
    Font:=FNameFont;
    Font.Color := GetPropNameColor(CurRow);
    // set bg color to highlight if needed
    if (NameBgColor <> FBackgroundColor) and (NameBgColor <> clNone) then
    begin
      Brush.Color := NameBgColor;
      FillRect(NameTextRect);
    end;
    CurRow.Editor.PropDrawName(Canvas, NameTextRect, DrawState);
    Font := OldFont;

    if (FBackgroundColor <> clNone) then // return color back to background
      Brush.Color := FBackgroundColor;
    
    // draw widgetsets
    X := NameRect.Right - 2;
    Y := (NameRect.Top + NameRect.Bottom - IDEImages.Images_16.Height) div 2;
    OldFont:=Font;
    Font:=FNameFont;
    Font.Color := clRed;
    for lclPlatform := High(TLCLPlatform) downto Low(TLCLPlatform) do
    begin
      if lclPlatform in CurRow.FWidgetSets then
      begin
        Dec(X, IDEImages.Images_16.Width);
        IDEImages.Images_16.Draw(Canvas, X, Y,
          IDEImages.LoadImage(16, 'issue_'+LCLPlatformDirNames[lclPlatform]));
      end;
    end;
    Font:=OldFont;

    // draw value
    if ARow<>ItemIndex
    then begin
      OldFont:=Font;
      if CurRow.Editor.IsNotDefaultValue then
        Font:=FValueFont
      else
        Font:=FDefaultValueFont;
      CurRow.Editor.PropDrawValue(Canvas,ValueRect,DrawState);
      Font:=OldFont;
    end;
    CurRow.LastPaintedValue:=CurRow.Editor.GetVisualValue;


    // -----------------
    // frames
    // -----------------

    if Layout = oilHorizontal then
    begin
      // Row Divider

      if DrawHorzGridLines then
      begin
        Pen.Style := psDot;
        Pen.EndCap := pecFlat;
        Pen.Cosmetic := False;
        Pen.Color := cl3DShadow;
        if FRowSpacing <> 0 then
        begin
          MoveTo(NameTextRect.Left, NameRect.Top - 1);
          LineTo(ValueRect.Right, NameRect.Top - 1);
        end;
        MoveTo(NameTextRect.Left, NameRect.Bottom - 1);
        LineTo(ValueRect.Right, NameRect.Bottom - 1);
      end;

      // Split lines between: icon and name, name and value
      Pen.Style := psSolid;
      Pen.Cosmetic := True;
      Pen.Color := cl3DHiLight;
      MoveTo(NameRect.Right - 1, NameRect.Bottom - 1);
      LineTo(NameRect.Right - 1, NameRect.Top - 1 - FRowSpacing);
      Pen.Color := cl3DShadow;
      MoveTo(NameRect.Right - 2, NameRect.Bottom - 1);
      LineTo(NameRect.Right - 2, NameRect.Top - 1 - FRowSpacing);

      // draw gutter line
      if ShowGutter then
      begin
        Pen.Color := GutterEdgeColor;
        MoveTo(NameIconRect.Right, NameRect.Bottom - 1);
        LineTo(NameIconRect.Right, NameRect.Top - 1 - FRowSpacing);

        if CurRow.Lvl > 0 then
        begin
          // draw to parent
          if ARow > 0 then
          begin
            ParentRect := RowRect(ARow - 1);
            X := ParentRect.Left + GetTreeIconX(ARow - 1) + Indent + 3;
            if X <> NameIconRect.Right then
            begin
              MoveTo(NameIconRect.Right, NameRect.Top - 1 - FRowSpacing);
              LineTo(X - 1, NameRect.Top - 1 - FRowSpacing);
            end;
          end;
          // to to parent next sibling
          if ARow < FRows.Count - 1 then
          begin
            ParentRect := RowRect(ARow + 1);
            X := ParentRect.Left + GetTreeIconX(ARow + 1) + Indent + 3;
            if X <> NameIconRect.Right then
            begin
              MoveTo(NameIconRect.Right, NameRect.Bottom - 1);
              LineTo(X - 1, NameRect.Bottom - 1);
            end;
          end;
        end;
      end;
    end
    else begin
      Pen.Style := psSolid;
      Pen.Color := cl3DLight;
      MoveTo(ValueRect.Left, ValueRect.Bottom - 1);
      LineTo(ValueRect.Left, NameTextRect.Top);
      LineTo(ValueRect.Right - 1, NameTextRect.Top);
      Pen.Color:=cl3DHiLight;
      LineTo(ValueRect.Right - 1, ValueRect.Bottom - 1);
      LineTo(ValueRect.Left, ValueRect.Bottom - 1);

      MoveTo(NameTextRect.Left + 1, NametextRect.Bottom);
      LineTo(NameTextRect.Left + 1, NameTextRect.Top + 1);
      LineTo(NameTextRect.Right - 2, NameTextRect.Top + 1);
      Pen.Color:=cl3DLight;
      LineTo(NameTextRect.Right - 2, NameTextRect.Bottom - 1);
      LineTo(NameTextRect.Left + 2, NameTextRect.Bottom - 1);
    end;
  end;
end;

procedure TOICustomPropertyGrid.DoPaint(PaintOnlyChangedValues: boolean);
var
  a: integer;
  SpaceRect: TRect;
  GutterX: Integer;
begin
  BuildPropertyList(true);
  if not PaintOnlyChangedValues then
  begin
    with Canvas do
    begin
      // draw properties
      for a := 0 to FRows.Count - 1 do
        PaintRow(a);
      // draw unused space below rows
      SpaceRect := Rect(BorderWidth, BorderWidth,
                        ClientWidth - BorderWidth + 1, ClientHeight - BorderWidth + 1);
      if FRows.Count > 0 then
        SpaceRect.Top := Rows[FRows.Count - 1].Bottom - FTopY + BorderWidth;
      if FBackgroundColor <> clNone then
      begin
        Brush.Color := FBackgroundColor;
        FillRect(SpaceRect);
      end;

      // draw gutter if needed
      if ShowGutter and (Layout = oilHorizontal) then
      begin
        if FRows.Count > 0 then
          GutterX := RowRect(FRows.Count - 1).Left + GetTreeIconX(FRows.Count - 1)
        else
          GutterX := BorderWidth + 2;
        inc(GutterX, Indent + 3);
        SpaceRect.Right := GutterX;
        if GutterColor <> clNone then
        begin
          Brush.Color := GutterColor;
          FillRect(SpaceRect);
        end;
        MoveTo(GutterX, SpaceRect.Top);
        LineTo(GutterX, SpaceRect.Bottom);
      end;
      // don't draw border: borderstyle=bsSingle
    end;
  end else
  begin
    for a := 0 to FRows.Count-1 do
    begin
      if Rows[a].Editor.GetVisualValue <> Rows[a].LastPaintedValue then
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

procedure TOICustomPropertyGrid.ScrollToActiveItem;
begin
  ScrollToItem(FItemIndex);
end;

procedure TOICustomPropertyGrid.ScrollToItem(NewIndex: Integer);
var
  NewRow: TOIPropertyGridRow;
begin
  if (NewIndex >= 0) and (NewIndex < FRows.Count) then
  begin
    NewRow := Rows[NewIndex];
    if NewRow.Bottom >= TopY + (ClientHeight - 2*BorderWidth) then
      TopY := NewRow.Bottom- (ClientHeight - 2*BorderWidth) + 1
    else
      if NewRow.Top < TopY then TopY := NewRow.Top;
  end;
end;

procedure TOICustomPropertyGrid.PropEditLookupRootChange;
begin
  // When the LookupRoot changes, no changes can be stored
  // -> undo the value editor changes
  RefreshValueEdit;
  if PropertyEditorHook<>nil then
    FCurrentEditorLookupRoot:=PropertyEditorHook.LookupRoot;
end;

function TOICustomPropertyGrid.RowRect(ARow:integer):TRect;
const
  ScrollBarWidth=0;
begin
  Result.Left:=BorderWidth;
  Result.Top:=Rows[ARow].Top-FTopY+BorderWidth;
  Result.Right:=ClientWidth-ScrollBarWidth;
  Result.Bottom:=Rows[ARow].Bottom-FTopY+BorderWidth;
end;

procedure TOICustomPropertyGrid.SetItemsTops;
// compute row tops from row heights
// set indices of all rows
var a:integer;
begin
  for a:=0 to FRows.Count-1 do begin
    Rows[a].FIndex:=a;
    Rows[a].MeasureHeight(Canvas);
  end;
  if FRows.Count>0 then
    Rows[0].Top:=0;
  for a:=1 to FRows.Count-1 do
    Rows[a].FTop:=Rows[a-1].Bottom + FRowSpacing;
end;

procedure TOICustomPropertyGrid.ClearRows;
var i:integer;
begin
  IncreaseChangeStep;
  // reverse order to make sure child rows are freed before parent rows
  for i:=FRows.Count-1 downto 0 do begin
    //debugln(['TOICustomPropertyGrid.ClearRows ',i,' ',FRows.Count,' ',dbgs(frows[i])]);
    Rows[i].Free;
    FRows[i]:=nil;
  end;
  FRows.Clear;
end;

function TOICustomPropertyGrid.GetCurrentEditValue: string;
begin
  if FCurrentEdit=ValueEdit then
    Result:=ValueEdit.Text
  else if FCurrentEdit=ValueComboBox then
    Result:=ValueComboBox.Text
  else if FCurrentEdit=ValueCheckBox then
    Result:=ValueCheckBox.Caption
  else
    Result:='';
end;

procedure TOICustomPropertyGrid.SetActiveControl(const AControl: TWinControl);
var
  F: TCustomForm;
begin
  F := GetParentForm(Self);
  if F <> nil then
    F.ActiveControl := AControl;
end;

procedure TOICustomPropertyGrid.SetColumn(
  const AValue: TOICustomPropertyGridColumn);
begin
  if FColumn <> AValue then
  begin
    FColumn := AValue;
    // TODO: indication
  end;
end;

procedure TOICustomPropertyGrid.SetCurrentEditValue(const NewValue: string);
begin
  if FCurrentEdit=ValueEdit then
    ValueEdit.Text:=NewValue
  else if FCurrentEdit=ValueComboBox then
  begin
    ValueComboBox.Text:=NewValue;
    if ValueComboBox.Style=csOwnerDrawVariable then
       Exclude(FStates,pgsGetComboItemsCalled);
  end
  else if FCurrentEdit=ValueCheckBox then begin
    ValueCheckBox.Caption:=NewValue;
    ValueCheckBox.Checked:=NewValue='True';
  end;
end;

procedure TOICustomPropertyGrid.SetDrawHorzGridLines(const AValue: Boolean);
begin
  if FDrawHorzGridLines = AValue then Exit;
  FDrawHorzGridLines := AValue;
  Invalidate;
end;

procedure TOICustomPropertyGrid.SetFavourites(
  const AValue: TOIFavouriteProperties);
begin
  //debugln('TOICustomPropertyGrid.SetFavourites ',dbgsName(Self));
  if FFavourites=AValue then exit;
  FFavourites:=AValue;
  BuildPropertyList;
end;

procedure TOICustomPropertyGrid.SetFilter(const AValue: TTypeKinds);
begin
  if (AValue<>FFilter) then
  begin
    FFilter:=AValue;
    BuildPropertyList;
  end;
end;

procedure TOICustomPropertyGrid.SetGutterColor(const AValue: TColor);
begin
  if FGutterColor=AValue then exit;
  FGutterColor:=AValue;
  invalidate;
end;

procedure TOICustomPropertyGrid.SetGutterEdgeColor(const AValue: TColor);
begin
  if FGutterEdgeColor=AValue then exit;
  FGutterEdgeColor:=AValue;
  invalidate;
end;

procedure TOICustomPropertyGrid.SetHighlightColor(const AValue: TColor);
begin
  if FHighlightColor=AValue then exit;
  FHighlightColor:=AValue;
  Invalidate;
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

procedure TOICustomPropertyGrid.ValueComboBoxGetItems(Sender: TObject);
{ This event is called whenever the widgetset updates the list.
  On gtk the list is updated just before the user popups the list.
  Other widgetsets need the list always, which is bad, as this means collecting
  all items even if the dropdown never happens.
}
var
  CurRow: TOIPropertyGridRow;
  MaxItemWidth, CurItemWidth, i, Cnt: integer;
  ItemValue, CurValue: string;
  NewItemIndex: LongInt;
  ExcludeUpdateFlag: boolean;
begin
  Include(FStates,pgsGetComboItemsCalled);
  if (FItemIndex>=0) and (FItemIndex<FRows.Count) then begin
    ExcludeUpdateFlag:=not (pgsUpdatingEditControl in FStates);
    Include(FStates,pgsUpdatingEditControl);
    ValueComboBox.Items.BeginUpdate;
    try
      CurRow:=Rows[FItemIndex];

      // Items
      if not FillComboboxItems then exit;

      // Text and ItemIndex
      CurValue:=CurRow.Editor.GetVisualValue;
      ValueComboBox.Text:=CurValue;
      NewItemIndex:=ValueComboBox.Items.IndexOf(CurValue);
      if NewItemIndex>=0 then
        ValueComboBox.ItemIndex:=NewItemIndex;

      // ItemWidth
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
    finally
      ValueComboBox.Items.EndUpdate;
      if ExcludeUpdateFlag then
        Exclude(FStates,pgsUpdatingEditControl);
    end;
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

procedure TOICustomPropertyGrid.OnIdle(Sender: TObject; var Done: Boolean);
begin
  if (not (pgsGetComboItemsCalled in FStates))
  and (FCurrentEdit=ValueComboBox)
  and ValueComboBox.Enabled
  then begin
    ValueComboBoxGetItems(Self);
  end;
end;

procedure TOICustomPropertyGrid.SetIdleEvent(Enable: boolean);
begin
  if (pgsIdleEnabled in FStates)=Enable then exit;
  if Enable then begin
    Application.AddOnIdleHandler(@OnIdle);
    Include(FStates,pgsIdleEnabled);
  end else begin
    Application.RemoveOnIdleHandler(@OnIdle);
    Exclude(FStates,pgsIdleEnabled);
  end;
end;

procedure TOICustomPropertyGrid.HintTimer(sender : TObject);
var
  HintRect : TRect;
  AHint : String;
  Position : TPoint;
  Index: integer;
  PointedRow:TOIpropertyGridRow;
  Window: TWinControl;
  HintType: TPropEditHint;
  ClientPosition: TPoint;
begin
  if FHintTimer <> nil then
    FHintTimer.Enabled := False;
  if (not InitHints) then exit;

  Position := Mouse.CursorPos;

  Window := FindLCLWindow(Position);
  if not Assigned(Window) then Exit;
  If (Window <> Self) and (not IsParentOf(Window)) then exit;

  ClientPosition := ScreenToClient(Position);
  if ((ClientPosition.X <=0) or (ClientPosition.X >= Width) or
     (ClientPosition.Y <= 0) or (ClientPosition.Y >= Height)) then
    Exit;

  AHint := '';
  Index := MouseToIndex(ClientPosition.Y, False);
  if (Index >= 0) and (Index < FRows.Count) then
  begin
    //IconX:=GetTreeIconX(Index);
    PointedRow := Rows[Index];
    if Assigned(PointedRow) and Assigned(PointedRow.Editor) then 
    begin
      if Index <> ItemIndex then 
      begin
        HintType := GetHintTypeAt(Index,Position.X);
        if (HintType = pehName) and Assigned(OnPropertyHint) then
        begin
          if OnPropertyHint(Self, PointedRow, Position, FHintWindow, HintRect, AHint)
          then begin
            FHintIndex := Index;
            FShowingLongHint := True;
            //DebugLn(['TOICustomPropertyGrid.HintTimer ',dbgs(HintRect),' ',AHint,' ',dbgs(Position)]);
            FHintWindow.ActivateHint(HintRect, AHint);
          end;
          exit;
        end;
        AHint := PointedRow.Editor.GetHint(HintType, Position.X, Position.Y);
      end;
    end;
  end;

  if AHint = '' then Exit;
  FHintIndex := Index;
  FShowingLongHint := True;
  HintRect := FHintWindow.CalcHintRect(0, AHint, nil);  //no maxwidth
  HintRect.Left := Position.X + 10;
  HintRect.Top := Position.Y + 10;
  HintRect.Right := HintRect.Left + HintRect.Right + 3;
  HintRect.Bottom := HintRect.Top + HintRect.Bottom + 3;
  //DebugLn(['TOICustomPropertyGrid.HintTimer ',dbgs(Rect),' ',AHint,' ',dbgs(Position)]);

  FHintWindow.ActivateHint(HintRect, AHint);
end;

Procedure TOICustomPropertyGrid.ResetHintTimer;
begin
  if FHintWindow = nil then exit;

  HideHint;

  FHintTimer.Enabled := False;
  if RowCount > 0 then
    FHintTimer.Enabled := not FDragging;
end;

procedure TOICustomPropertyGrid.HideHint;
begin
  if FHintWindow = nil then Exit;
  FHintWindow.Visible := False;
  FHintIndex := -1;
  FShowingLongHint := False;
  while FHintWindow.ControlCount > 0 do
    FHintWindow.Controls[0].Free;
end;

procedure TOICustomPropertyGrid.ValueControlMouseDown(Sender : TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  HideHint;
  ScrollToActiveItem;
end;

procedure TOICustomPropertyGrid.ValueControlMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: integer);
begin
  // when the cursor is divider change it to default
  if (Sender as TControl).Parent.Cursor <> crDefault then
    (Sender as TControl).Parent.Cursor := crDefault;
end;

procedure TOICustomPropertyGrid.IncreaseChangeStep;
begin
  if FChangeStep<>$7fffffff then
    inc(FChangeStep)
  else
    FChangeStep:=-$7fffffff;
end;

function TOICustomPropertyGrid.GridIsUpdating: boolean;
begin
  Result:=(FStates*[pgsChangingItemIndex,pgsApplyingValue,
                    pgsBuildPropertyListNeeded]<>[])
end;

procedure TOICustomPropertyGrid.ToggleRow;
var
  CurRow: TOIPropertyGridRow;
  TypeKind : TTypeKind;
begin
  if not CanEditRowValue then exit;

  if FHintTimer <> nil then
    FHintTimer.Enabled := False;

  if (FCurrentEdit = ValueComboBox) then 
  begin
    //either an Event or an enumeration or Boolean
    CurRow := Rows[FItemIndex];
    TypeKind := CurRow.Editor.GetPropType^.Kind;
    if TypeKind in [tkEnumeration, tkBool, tkSet] then 
    begin
      // set value to next value in list
      if ValueComboBox.Items.Count = 0 then Exit;
      if ValueComboBox.ItemIndex < (ValueComboBox.Items.Count - 1) then
        ValueComboBox.ItemIndex := ValueComboBox.ItemIndex + 1
      else
        ValueComboBox.ItemIndex := 0;
      SetRowValue;
      exit;
    end;
  end;
  DoCallEdit;
end;

procedure TOICustomPropertyGrid.ValueEditDblClick(Sender: TObject);
begin
  FFirstClickTime:=0;
  ToggleRow;
end;

procedure TOICustomPropertyGrid.SetBackgroundColor(const AValue: TColor);
begin
  if FBackgroundColor=AValue then exit;
  FBackgroundColor:=AValue;
  Invalidate;
end;

procedure TOICustomPropertyGrid.SetReferences(const AValue: TColor);
begin
  if FReferencesColor=AValue then exit;
  FReferencesColor:=AValue;
  Invalidate;
end;

procedure TOICustomPropertyGrid.SetSubPropertiesColor(const AValue: TColor);
begin
  if FSubPropertiesColor=AValue then exit;
  FSubPropertiesColor:=AValue;
  Invalidate;
end;

//------------------------------------------------------------------------------

{ TOIPropertyGridRow }

constructor TOIPropertyGridRow.Create(PropertyTree: TOICustomPropertyGrid;
  PropEditor:TPropertyEditor; ParentNode:TOIPropertyGridRow; WidgetSets: TLCLPlatforms);
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
  FIndex:=-1;
  LastPaintedValue:='';
  FWidgetSets := WidgetSets;
end;

destructor TOIPropertyGridRow.Destroy;
begin
  //debugln(['TOIPropertyGridRow.Destroy ',fname,' ',dbgs(Pointer(Self))]);
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

procedure TOIPropertyGridRow.WriteDebugReport(const Prefix: string);
var
  i: Integer;
  Item: TOIPropertyGridRow;
begin
  DebugLn([Prefix+'TOIPropertyGridRow.WriteDebugReport ',Name]);
  i:=0;
  Item:=FirstChild;
  while Item<>nil do begin
    DebugLn([Prefix+'  ',i,' ',Item.Name]);
    inc(i);
    Item:=Item.NextBrother;
  end;
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
  if FTree.Layout = oilVertical
  then Inc(Result, FTree.GetNameRowHeight);
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

function TOIPropertyGridRow.Sort(const Compare: TListSortCompare): boolean;
var
  List: TFPList;
  Item: TOIPropertyGridRow;
  i: Integer;
begin
  if IsSorted(Compare) then exit(false);
  List:=TFPList.Create;
  try
    // create a TFPList of the children
    List.Capacity:=ChildCount;
    Item:=FirstChild;
    while Item<>nil do begin
      List.Add(Item);
      Item:=Item.NextBrother;
    end;
    // sort the TFPList
    List.Sort(Compare);
    // sort in double linked list
    for i:=0 to List.Count-1 do begin
      Item:=TOIPropertyGridRow(List[i]);
      if i=0 then begin
        FFirstChild:=Item;
        Item.FPriorBrother:=nil;
      end else
        Item.FPriorBrother:=TOIPropertyGridRow(List[i-1]);
      if i=List.Count-1 then begin
        FLastChild:=Item;
        Item.FNextBrother:=nil;
      end else
        Item.FNextBrother:=TOIPropertyGridRow(List[i+1]);
    end;
  finally
    List.Free;
  end;
  Result:=true;
end;

function TOIPropertyGridRow.IsSorted(const Compare: TListSortCompare): boolean;
var
  Item1: TOIPropertyGridRow;
  Item2: TOIPropertyGridRow;
begin
  if ChildCount<2 then exit(true);
  Item1:=FirstChild;
  while true do begin
    Item2:=Item1.NextBrother;
    if Item2=nil then break;
    if Compare(Item1,Item2)>0 then exit(false);
    Item1:=Item2;
  end;
  Result:=true;
end;

function TOIPropertyGridRow.Next: TOIPropertyGridRow;
begin
  if fFirstChild<>nil then
    Result:=fFirstChild
  else
    Result:=NextSkipChilds;
end;

function TOIPropertyGridRow.NextSkipChilds: TOIPropertyGridRow;
begin
  Result:=Self;
  while (Result<>nil) do begin
    if Result.NextBrother<>nil then begin
      Result:=Result.NextBrother;
      exit;
    end;
    Result:=Result.Parent;
  end;
end;

//==============================================================================


{ TOIOptions }

function TOIOptions.FPropertyGridSplitterX(Page: TObjectInspectorPage): integer;
begin
  Result:=FGridSplitterX[Page];
end;

procedure TOIOptions.FPropertyGridSplitterX(Page: TObjectInspectorPage;
  const AValue: integer);
begin
  FGridSplitterX[Page]:=AValue;
end;

constructor TOIOptions.Create;
var
  p: TObjectInspectorPage;
begin
  inherited Create;

  FSaveBounds:=false;
  FLeft:=0;
  FTop:=0;
  FWidth:=250;
  FHeight:=400;
  for p:=Low(TObjectInspectorPage) to High(TObjectInspectorPage) do
    FGridSplitterX[p]:=110;
  FDefaultItemHeight:=20;
  FShowComponentTree:=true;
  FComponentTreeHeight:=100;
  FInfoBoxHeight:=80;

  FGridBackgroundColor := DefBackgroundColor;
  FDefaultValueColor := DefDefaultValueColor;
  FSubPropertiesColor := DefSubPropertiesColor;
  FValueColor := DefValueColor;
  FReadOnlyColor := DefReadOnlyColor;
  FReferencesColor := DefReferencesColor;
  FPropertyNameColor := DefNameColor;
  FHighlightColor := DefHighlightColor;
  FHighlightFontColor := DefHighlightFontColor;
  FGutterColor := DefGutterColor;
  FGutterEdgeColor := DefGutterEdgeColor;

  FBoldNonDefaultValues := True;
  FDrawGridLines := True;
  FShowGutter := True;
  FShowStatusBar := True;
  FShowInfoBox := False;
end;

function TOIOptions.Load: boolean;
var
  Path: String;
  FileVersion: integer;
  Page: TObjectInspectorPage;
begin
  Result:=false;
  if ConfigStore=nil then exit;
  try
    Path:='ObjectInspectorOptions/';
    FileVersion:=ConfigStore.GetValue(Path+'Version/Value',0);

    FSaveBounds:=ConfigStore.GetValue(Path+'Bounds/Valid'
                                      ,false);
    if FSaveBounds then begin
      FLeft:=ConfigStore.GetValue(Path+'Bounds/Left',0);
      FTop:=ConfigStore.GetValue(Path+'Bounds/Top',0);
      FWidth:=ConfigStore.GetValue(Path+'Bounds/Width',250);
      FHeight:=ConfigStore.GetValue(Path+'Bounds/Height',400);
    end;

    if FileVersion>=2 then begin
      for Page:=Low(TObjectInspectorPage) to High(TObjectInspectorPage) do
        FGridSplitterX[Page]:=ConfigStore.GetValue(
           Path+'Bounds/'+DefaultOIPageNames[Page]+'/SplitterX',110);
    end else begin
      FGridSplitterX[oipgpProperties]:=ConfigStore.GetValue(
         Path+'Bounds/PropertyGridSplitterX',110);
      FGridSplitterX[oipgpEvents]:=ConfigStore.GetValue(
         Path+'Bounds/EventGridSplitterX',110);
    end;
    for Page:=Low(TObjectInspectorPage) to High(TObjectInspectorPage) do
      if FGridSplitterX[Page]<10 then FGridSplitterX[Page]:=10;

    FDefaultItemHeight:=ConfigStore.GetValue(
       Path+'Bounds/DefaultItemHeight',20);
    if FDefaultItemHeight<0 then FDefaultItemHeight:=20;
    FShowComponentTree:=ConfigStore.GetValue(
       Path+'ComponentTree/Show/Value',true);
    FComponentTreeHeight:=ConfigStore.GetValue(
       Path+'ComponentTree/Height/Value',100);

    FGridBackgroundColor:=ConfigStore.GetValue(
         Path+'Color/GridBackground',DefBackgroundColor);
    FDefaultValueColor:=ConfigStore.GetValue(
         Path+'Color/DefaultValue', DefDefaultValueColor);
    FSubPropertiesColor:=ConfigStore.GetValue(
         Path+'Color/SubProperties', DefSubPropertiesColor);
    FValueColor:=ConfigStore.GetValue(
         Path+'Color/Value', DefValueColor);
    FReadOnlyColor:=ConfigStore.GetValue(
         Path+'Color/ReadOnly', DefReadOnlyColor);
    FReferencesColor:=ConfigStore.GetValue(
         Path+'Color/References',DefReferencesColor);
    FPropertyNameColor:=ConfigStore.GetValue(
         Path+'Color/PropertyName',DefNameColor);
    FHighlightColor:=ConfigStore.GetValue(
         Path+'Color/Highlight',DefHighlightColor);
    FHighlightFontColor:=ConfigStore.GetValue(
         Path+'Color/HighlightFont',DefHighlightFontColor);
    FGutterColor:=ConfigStore.GetValue(
         Path+'Color/Gutter',DefGutterColor);
    FGutterEdgeColor:=ConfigStore.GetValue(
         Path+'Color/GutterEdge',DefGutterEdgeColor);

    FShowHints:=ConfigStore.GetValue(
         Path+'ShowHints',FileVersion>=3);
    FAutoShow := ConfigStore.GetValue(
         Path+'AutoShow',true);
    FBoldNonDefaultValues := ConfigStore.GetValue(
         Path+'BoldNonDefaultValues',true);
    FDrawGridLines := ConfigStore.GetValue(
         Path+'DrawGridLines',true);
    FShowGutter := ConfigStore.GetValue(
         Path+'ShowGutter',true);
    FShowStatusBar := ConfigStore.GetValue(
         Path+'ShowStatusBar',true);
    FShowInfoBox := ConfigStore.GetValue(
         Path+'ShowInfoBox',false);
    FInfoBoxHeight := ConfigStore.GetValue(
       Path+'InfoBoxHeight',80);
  except
    on E: Exception do begin
      DebugLn('ERROR: TOIOptions.Load: ',E.Message);
      exit;
    end;
  end;
  Result:=true;
end;

function TOIOptions.Save: boolean;
var
  Page: TObjectInspectorPage;
  Path: String;
begin
  Result:=false;
  if ConfigStore=nil then exit;
  try
    Path:='ObjectInspectorOptions/';
    ConfigStore.SetValue(Path+'Version/Value',OIOptionsFileVersion);

    ConfigStore.SetDeleteValue(Path+'Bounds/Valid',FSaveBounds,
                             false);

    ConfigStore.SetDeleteValue(Path+'Bounds/Valid',FSaveBounds,
                             false);
    if FSaveBounds then begin
      ConfigStore.SetValue(Path+'Bounds/Left',FLeft);
      ConfigStore.SetValue(Path+'Bounds/Top',FTop);
      ConfigStore.SetValue(Path+'Bounds/Width',FWidth);
      ConfigStore.SetValue(Path+'Bounds/Height',FHeight);
    end;
    for Page:=Low(TObjectInspectorPage) to High(TObjectInspectorPage) do
      ConfigStore.SetDeleteValue(
         Path+'Bounds/'+DefaultOIPageNames[Page]+'/SplitterX',
         FGridSplitterX[Page],110);
    ConfigStore.SetDeleteValue(Path+'Bounds/DefaultItemHeight',
                             FDefaultItemHeight,20);
    ConfigStore.SetDeleteValue(Path+'ComponentTree/Show/Value',
                             FShowComponentTree,true);
    ConfigStore.SetDeleteValue(Path+'ComponentTree/Height/Value',
                             FComponentTreeHeight,100);

    ConfigStore.SetDeleteValue(Path+'Color/GridBackground',
                             FGridBackgroundColor,DefBackgroundColor);
    ConfigStore.SetDeleteValue(Path+'Color/DefaultValue',
                             FDefaultValueColor,DefDefaultValueColor);
    ConfigStore.SetDeleteValue(Path+'Color/SubProperties',
                             FSubPropertiesColor,DefSubPropertiesColor);
    ConfigStore.SetDeleteValue(Path+'Color/Value',
                             FValueColor,DefValueColor);
    ConfigStore.SetDeleteValue(Path+'Color/ReadOnly',
                             FReadOnlyColor,DefReadOnlyColor);
    ConfigStore.SetDeleteValue(Path+'Color/References',
                             FReferencesColor,DefReferencesColor);
    ConfigStore.SetDeleteValue(Path+'Color/PropertyName',
                              FPropertyNameColor,DefNameColor);
    ConfigStore.SetDeleteValue(Path+'Color/Highlight',
                              FHighlightColor,DefHighlightColor);
    ConfigStore.SetDeleteValue(Path+'Color/HighlightFont',
                              FHighlightFontColor,DefHighlightFontColor);
    ConfigStore.SetDeleteValue(Path+'Color/Gutter',
                              FGutterColor,DefGutterColor);
    ConfigStore.SetDeleteValue(Path+'Color/GutterEdge',
                              FGutterEdgeColor,DefGutterEdgeColor);

    ConfigStore.SetDeleteValue(Path+'ShowHints',FShowHints, True);
    ConfigStore.SetDeleteValue(Path+'AutoShow',FAutoShow, True);
    ConfigStore.SetDeleteValue(Path+'BoldNonDefaultValues',FBoldNonDefaultValues, True);
    ConfigStore.SetDeleteValue(Path+'DrawGridLines',FDrawGridLines, True);
    ConfigStore.SetDeleteValue(Path+'ShowGutter',FShowGutter, True);
    ConfigStore.SetDeleteValue(Path+'ShowStatusBar',FShowStatusBar, True);
    ConfigStore.SetDeleteValue(Path+'ShowInfoBox',FShowInfoBox, False);
    ConfigStore.SetDeleteValue(Path+'InfoBoxHeight',FInfoBoxHeight,80);
  except
    on E: Exception do begin
      DebugLn('ERROR: TOIOptions.Save: ',E.Message);
      exit;
    end;
  end;
  Result:=true;
end;

procedure TOIOptions.Assign(AnObjInspector: TObjectInspectorDlg);
var
  Page: TObjectInspectorPage;
begin
  FLeft:=AnObjInspector.Left;
  FTop:=AnObjInspector.Top;
  FWidth:=AnObjInspector.Width;
  FHeight:=AnObjInspector.Height;
  for Page:=Low(TObjectInspectorPage) to High(TObjectInspectorPage) do
    if AnObjInspector.GridControl[Page]<>nil then
      FGridSplitterX[Page]:=AnObjInspector.GridControl[Page].PrefferedSplitterX;
  FDefaultItemHeight:=AnObjInspector.DefaultItemHeight;
  FShowComponentTree:=AnObjInspector.ShowComponentTree;
  FComponentTreeHeight:=AnObjInspector.ComponentTreeHeight;

  FGridBackgroundColor:=AnObjInspector.PropertyGrid.BackgroundColor;
  FSubPropertiesColor:=AnObjInspector.PropertyGrid.SubPropertiesColor;
  FReferencesColor:=AnObjInspector.PropertyGrid.ReferencesColor;
  FValueColor:=AnObjInspector.PropertyGrid.ValueFont.Color;
  FDefaultValueColor:=AnObjInspector.PropertyGrid.DefaultValueFont.Color;
  FReadOnlyColor:=AnObjInspector.PropertyGrid.ReadOnlyColor;
  FPropertyNameColor:=AnObjInspector.PropertyGrid.NameFont.Color;
  FHighlightColor:=AnObjInspector.PropertyGrid.HighlightColor;
  FHighlightFontColor:=AnObjInspector.PropertyGrid.HighlightFont.Color;
  FGutterColor:=AnObjInspector.PropertyGrid.GutterColor;
  FGutterEdgeColor:=AnObjInspector.PropertyGrid.GutterEdgeColor;

  FShowHints := AnObjInspector.PropertyGrid.ShowHint;
  FAutoShow := AnObjInspector.AutoShow;
  FBoldNonDefaultValues := fsBold in AnObjInspector.PropertyGrid.ValueFont.Style;
  FDrawGridLines := AnObjInspector.PropertyGrid.DrawHorzGridLines;
  FShowGutter := AnObjInspector.PropertyGrid.ShowGutter;
  FShowStatusBar := AnObjInspector.ShowStatusBar;
  FShowInfoBox := AnObjInspector.ShowInfoBox;
  FInfoBoxHeight := AnObjInspector.InfoBoxHeight;
end;

procedure TOIOptions.AssignTo(AnObjInspector: TObjectInspectorDlg);
var
  Page: TObjectInspectorPage;
  Grid: TOICustomPropertyGrid;
begin
  if FSaveBounds then
  begin
    AnObjInspector.SetBounds(FLeft,FTop,FWidth,FHeight);
  end;

  for Page := Low(TObjectInspectorPage) to High(TObjectInspectorPage) do
  begin
    Grid := AnObjInspector.GridControl[Page];
    if Grid = nil then
      Continue;
    Grid.PrefferedSplitterX := FGridSplitterX[Page];
    Grid.SplitterX := FGridSplitterX[Page];
    AssignTo(Grid);
  end;
  AnObjInspector.DefaultItemHeight := DefaultItemHeight;
  AnObjInspector.ShowComponentTree := ShowComponentTree;
  AnObjInspector.ShowInfoBox := ShowInfoBox;
  AnObjInspector.ComponentTreeHeight := ComponentTreeHeight;
  AnObjInspector.InfoBoxHeight := InfoBoxHeight;
  AnObjInspector.AutoShow := AutoShow;
  AnObjInspector.ShowStatusBar := ShowStatusBar;
end;

procedure TOIOptions.AssignTo(AGrid: TOICustomPropertyGrid);
begin
  AGrid.BackgroundColor := FGridBackgroundColor;
  AGrid.SubPropertiesColor := FSubPropertiesColor;
  AGrid.ReferencesColor := FReferencesColor;
  AGrid.ReadOnlyColor := FReadOnlyColor;
  AGrid.ValueFont.Color := FValueColor;
  if FBoldNonDefaultValues then
    AGrid.ValueFont.Style := [fsBold]
  else
    AGrid.ValueFont.Style := [];
  AGrid.DefaultValueFont.Color := FDefaultValueColor;
  AGrid.NameFont.Color := FPropertyNameColor;
  AGrid.HighlightColor := FHighlightColor;
  AGrid.HighlightFont.Color := FHighlightFontColor;
  AGrid.GutterColor := FGutterColor;
  AGrid.GutterEdgeColor := FGutterEdgeColor;
  AGrid.ShowHint := FShowHints;
  AGrid.DrawHorzGridLines := FDrawGridLines;
  AGrid.ShowGutter := FShowGutter;
end;


//==============================================================================


{ TObjectInspectorDlg }

constructor TObjectInspectorDlg.Create(AnOwner: TComponent);

  procedure AddPopupMenuItem(var NewMenuItem: TMenuItem;
    ParentMenuItem: TMenuItem; const AName, ACaption, AHint, AResourceName: string;
    AnOnClick: TNotifyEvent; CheckedFlag, EnabledFlag, VisibleFlag: boolean);
  begin
    NewMenuItem:=TMenuItem.Create(Self);
    with NewMenuItem do 
    begin
      Name:=AName;
      Caption:=ACaption;
      Hint:=AHint;
      OnClick:=AnOnClick;
      Checked:=CheckedFlag;
      Enabled:=EnabledFlag;
      Visible:=VisibleFlag;
      if AResourceName <> '' then
        ImageIndex := IDEImages.LoadImage(16, AResourceName);
    end;
    if ParentMenuItem<>nil then
      ParentMenuItem.Add(NewMenuItem)
    else
      MainPopupMenu.Items.Add(NewMenuItem);
  end;

  function AddSeparatorMenuItem(ParentMenuItem: TMenuItem; const AName: string; VisibleFlag: boolean): TMenuItem;
  begin
    Result := TMenuItem.Create(Self);
    with Result do
    begin
      Name := AName;
      Caption := cLineCaption;
      Visible := VisibleFlag;
    end;
    if ParentMenuItem <> nil then
      ParentMenuItem.Add(Result)
    else
      MainPopupMenu.Items.Add(Result);
  end;

begin
  inherited Create(AnOwner);
  FPropertyEditorHook:=nil;
  FInSelection := False;
  FSelection:=TPersistentSelectionList.Create;
  FAutoShow := True;
  FUpdatingAvailComboBox:=false;
  FDefaultItemHeight := 22;
  FComponentTreeHeight:=100;
  FShowComponentTree := True;
  FShowFavorites := False;
  FShowRestricted := False;
  FShowStatusBar := True;
  FInfoBoxHeight := 80;
  FShowInfoBox := False;
  FComponentEditor := nil;

  Caption := oisObjectInspector;
  StatusBar.SimpleText := oisAll;

  MainPopupMenu.Images := IDEImages.Images_16;

  AddPopupMenuItem(SetDefaultPopupmenuItem,nil,'SetDefaultPopupMenuItem',
     'Set to Default value','Set property value to Default', '',
     @OnSetDefaultPopupmenuItemClick,false,true,true);
  AddPopupMenuItem(AddToFavoritesPopupMenuItem,nil,'AddToFavoritePopupMenuItem',
     oisAddtofavorites,'Add property to favorites properties', '',
     @OnAddToFavoritesPopupmenuItemClick,false,true,true);
  AddPopupMenuItem(RemoveFromFavoritesPopupMenuItem,nil,
     'RemoveFromFavoritesPopupMenuItem',
     oisRemovefromfavorites,'Remove property from favorites properties', '',
     @OnRemoveFromFavoritesPopupmenuItemClick,false,true,true);
  AddPopupMenuItem(ViewRestrictedPropertiesPopupMenuItem,nil,
     'ViewRestrictedPropertiesPopupMenuItem',
     oisViewRestrictedProperties,'View restricted property descriptions', '',
     @OnViewRestrictionsPopupmenuItemClick,false,true,true);
  AddPopupMenuItem(UndoPropertyPopupMenuItem,nil,'UndoPropertyPopupMenuItem',
     oisUndo,'Set property value to last valid value', '',
     @OnUndoPopupmenuItemClick,false,true,true);
  AddPopupMenuItem(FindDeclarationPopupmenuItem,nil,'FindDeclarationPopupmenuItem',
     oisFinddeclaration,'Jump to declaration of property', '',
     @OnFindDeclarationPopupmenuItemClick,false,true,false);
  OptionsSeparatorMenuItem := AddSeparatorMenuItem(nil, 'OptionsSeparatorMenuItem', true);
  AddPopupMenuItem(CutPopupMenuItem,nil,'CutPopupMenuItem',
     oisCutComponents,'Cut selected item', 'laz_cut',
     @OnCutPopupmenuItemClick,false,true,true);
  AddPopupMenuItem(CopyPopupMenuItem,nil,'CopyPopupMenuItem',
     oisCopyComponents,'Copy selected item', 'laz_copy',
     @OnCopyPopupmenuItemClick,false,true,true);
  AddPopupMenuItem(PastePopupMenuItem,nil,'PastePopupMenuItem',
     oisPasteComponents,'Paste selected item', 'laz_paste',
     @OnPastePopupmenuItemClick,false,true,true);
  AddPopupMenuItem(DeletePopupMenuItem,nil,'DeletePopupMenuItem',
     oisDeleteComponents,'Delete selected item', 'delete_selection',
     @OnDeletePopupmenuItemClick,false,true,true);
  OptionsSeparatorMenuItem2 := AddSeparatorMenuItem(nil, 'OptionsSeparatorMenuItem2', true);
  AddPopupMenuItem(ShowHintsPopupMenuItem,nil
     ,'ShowHintPopupMenuItem',oisShowHints,'Grid hints', ''
     ,@OnShowHintPopupMenuItemClick,false,true,true);
  ShowHintsPopupMenuItem.ShowAlwaysCheckable:=true;
  AddPopupMenuItem(ShowComponentTreePopupMenuItem,nil
     ,'ShowComponentTreePopupMenuItem',oisShowComponentTree, '', ''
     ,@OnShowComponentTreePopupMenuItemClick,FShowComponentTree,true,true);
  ShowComponentTreePopupMenuItem.ShowAlwaysCheckable:=true;
  AddPopupMenuItem(ShowOptionsPopupMenuItem,nil
     ,'ShowOptionsPopupMenuItem',oisOptions,'', 'oi_options'
     ,@OnShowOptionsPopupMenuItemClick,false,true,FOnShowOptions<>nil);

  // combobox at top (filled with available persistents)
  with AvailPersistentComboBox do
  begin
    Sorted := true;
    AutoSelect := true;
    AutoComplete := true;
    DropDownCount := 12;
    Visible := not FShowComponentTree;
  end;

  // Component Tree at top (filled with available components)
  ComponentTree := TComponentTreeView.Create(Self);
  with ComponentTree do
  begin
    Name := 'ComponentTree';
    Constraints.MinHeight := 16;
    Height := ComponentTreeHeight;
    Parent := Self;
    Align := alTop;
    OnDblClick := @ComponentTreeDblClick;
    OnKeyDown := @ComponentTreeKeyDown;
    OnSelectionChanged := @ComponentTreeSelectionChanged;
    OnModified := @DoModified;
    Visible := FShowComponentTree;
    Scrollbars := ssAutoBoth;
    PopupMenu := MainPopupMenu;
  end;

  InfoPanel := TPanel.Create(Self);
  with InfoPanel do
  begin
    Name := 'InfoPanel';
    Caption := '';
    Height := InfoBoxHeight;
    Parent := Self;
    BevelOuter := bvLowered;
    Align := alBottom;
    PopupMenu := MainPopupMenu;
    Visible := FShowInfoBox;
  end;

  if ShowComponentTree then
    CreateSplitter(True);

  if ShowInfoBox then
    CreateSplitter(False);

  CreateNoteBook;
end;

destructor TObjectInspectorDlg.Destroy;
begin
  FreeAndNil(FSelection);
  FreeAndNil(FComponentEditor);
  inherited Destroy;
  FreeAndNil(FFavourites);
end;

procedure TObjectInspectorDlg.SetPropertyEditorHook(NewValue:TPropertyEditorHook);
var
  Page: TObjectInspectorPage;
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
    for Page:=Low(TObjectInspectorPage) to High(TObjectInspectorPage) do
      if GridControl[Page]<>nil then
        GridControl[Page].PropertyEditorHook:=FPropertyEditorHook;
    ComponentTree.PropertyEditorHook:=FPropertyEditorHook;
    RefreshSelection;
  end;
end;

function TObjectInspectorDlg.PersistentToString(APersistent: TPersistent): string;
begin
  if APersistent is TComponent then
    Result:=TComponent(APersistent).GetNamePath+': '+APersistent.ClassName
  else
    Result:=APersistent.ClassName;
end;

procedure TObjectInspectorDlg.SetComponentTreeHeight(const AValue: integer);
begin
  if FComponentTreeHeight <> AValue then
  begin
    FComponentTreeHeight := AValue;
    if Assigned(ComponentTree) then
      ComponentTree.Height := AValue;
  end;
end;

procedure TObjectInspectorDlg.SetDefaultItemHeight(const AValue: integer);
var
  NewValue: Integer;
  Page: TObjectInspectorPage;
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
  for Page:=Low(TObjectInspectorPage) to High(TObjectInspectorPage) do
    if GridControl[Page]<>nil then
      GridControl[Page].DefaultItemHeight:=FDefaultItemHeight;
  RebuildPropertyLists;
end;

procedure TObjectInspectorDlg.SetInfoBoxHeight(const AValue: integer);
begin
  if FInfoBoxHeight <> AValue then
  begin
    FInfoBoxHeight := AValue;
    if Assigned(InfoPanel) then
      InfoPanel.Height := AValue;
  end;
end;

procedure TObjectInspectorDlg.SetRestricted(const AValue: TOIRestrictedProperties);
begin
  if FRestricted = AValue then exit;
  //DebugLn('TObjectInspectorDlg.SetRestricted Count: ', DbgS(AValue.Count));
  FRestricted := AValue;
  RestrictedGrid.Favourites := FRestricted;
end;

procedure TObjectInspectorDlg.SetOnShowOptions(const AValue: TNotifyEvent);
begin
  if FOnShowOptions=AValue then exit;
  FOnShowOptions:=AValue;
  ShowOptionsPopupMenuItem.Visible:=FOnShowOptions<>nil;
end;

procedure TObjectInspectorDlg.AddPersistentToList(APersistent: TPersistent;
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

procedure TObjectInspectorDlg.HookLookupRootChange;
var
  Page: TObjectInspectorPage;
begin
  for Page:=Low(TObjectInspectorPage) to High(TObjectInspectorPage) do
    if GridControl[Page]<>nil then
      GridControl[Page].PropEditLookupRootChange;
  FillPersistentComboBox;
end;

procedure TObjectInspectorDlg.FillPersistentComboBox;
var a:integer;
  Root:TComponent;
  OldText:AnsiString;
  NewList: TStringList;
begin
//writeln('[TObjectInspectorDlg.FillComponentComboBox] A ',FUpdatingAvailComboBox
//,' ',FPropertyEditorHook<>nil,'  ',FPropertyEditorHook.LookupRoot<>nil);
  if FUpdatingAvailComboBox then exit;
  FUpdatingAvailComboBox:=true;
  if ComponentTree<>nil then
    ComponentTree.RebuildComponentNodes;
  NewList:=TStringList.Create;
  try
    if (FPropertyEditorHook<>nil)
    and (FPropertyEditorHook.LookupRoot<>nil) then begin
      AddPersistentToList(FPropertyEditorHook.LookupRoot,NewList);
      if FPropertyEditorHook.LookupRoot is TComponent then begin
        Root:=TComponent(FPropertyEditorHook.LookupRoot);
  //writeln('[TObjectInspectorDlg.FillComponentComboBox] B  ',Root.Name,'  ',Root.ComponentCount);
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

procedure TObjectInspectorDlg.BeginUpdate;
begin
  inc(FUpdateLock);
end;

procedure TObjectInspectorDlg.EndUpdate;
begin
  dec(FUpdateLock);
  if FUpdateLock<0 then begin
    DebugLn('ERROR TObjectInspectorDlg.EndUpdate');
  end;
  if FUpdateLock=0 then begin
    if oifRebuildPropListsNeeded in FFLags then
      RebuildPropertyLists;
  end;
end;

function TObjectInspectorDlg.GetActivePropertyGrid: TOICustomPropertyGrid;
begin
  Result:=nil;
  if NoteBook=nil then exit;
  case NoteBook.PageIndex of
  0: Result:=PropertyGrid;
  1: Result:=EventGrid;
  2: Result:=FavouriteGrid;
  3: Result:=RestrictedGrid;
  end;
end;

function TObjectInspectorDlg.GetActivePropertyRow: TOIPropertyGridRow;
var
  CurGrid: TOICustomPropertyGrid;
begin
  Result:=nil;
  CurGrid:=GetActivePropertyGrid;
  if CurGrid=nil then exit;
  Result:=CurGrid.GetActiveRow;
end;

function TObjectInspectorDlg.GetCurRowDefaultValue(var DefaultStr: string): boolean;
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

procedure TObjectInspectorDlg.SetSelection(const ASelection: TPersistentSelectionList);
begin
  if (not Assigned(ASelection)) then
    exit;
  if FInSelection and FSelection.IsEqual(ASelection) then
    exit; // prevent endless loops
  if (not ASelection.ForceUpdate) and FSelection.IsEqual(ASelection) then
    exit; // nothing changed
  FInSelection := True;
  try
    FSelection.Assign(ASelection);
    SetAvailComboBoxText;
    RefreshSelection;
    if Assigned(FOnSelectPersistentsInOI) then
      FOnSelectPersistentsInOI(Self);
  finally
    FInSelection := False;
  end;
end;

procedure TObjectInspectorDlg.RefreshSelection;
var
  Page: TObjectInspectorPage;
begin
  if NoteBook.Page[3].Visible then
  begin
    DoUpdateRestricted;
    
    // invalidate RestrictedProps
    WidgetSetsRestrictedBox.Invalidate;
    ComponentRestrictedBox.Invalidate;
  end;
  
  for Page := Low(TObjectInspectorPage) to High(TObjectInspectorPage) do
    if GridControl[Page] <> nil then
      GridControl[Page].Selection := FSelection;
  RefreshComponentTreeSelection;
  if (not Visible) and AutoShow and (FSelection.Count > 0) then
    if Assigned(OnAutoShow) then
      OnAutoShow(Self)
    else
      Visible := True;
end;

procedure TObjectInspectorDlg.RefreshComponentTreeSelection;
begin
  ComponentTree.Selection := FSelection;
  ComponentTree.MakeSelectionVisible;
end;

procedure TObjectInspectorDlg.SaveChanges;
var
  Page: TObjectInspectorPage;
begin
  for Page:=Low(TObjectInspectorPage) to High(TObjectInspectorPage) do
    if GridControl[Page]<>nil then
      GridControl[Page].SaveChanges;
end;

procedure TObjectInspectorDlg.RefreshPropertyValues;
var
  Page: TObjectInspectorPage;
begin
  for Page:=Low(TObjectInspectorPage) to High(TObjectInspectorPage) do
    if GridControl[Page]<>nil then
      GridControl[Page].RefreshPropertyValues;
end;

procedure TObjectInspectorDlg.RebuildPropertyLists;
var
  Page: TObjectInspectorPage;
begin
  if FUpdateLock>0 then
    Include(FFLags,oifRebuildPropListsNeeded)
  else begin
    Exclude(FFLags,oifRebuildPropListsNeeded);
    for Page:=Low(TObjectInspectorPage) to High(TObjectInspectorPage) do
      if GridControl[Page]<>nil then
        GridControl[Page].BuildPropertyList;
  end;
end;

procedure TObjectInspectorDlg.AvailComboBoxCloseUp(Sender:TObject);
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
    // not a TComponent => no children => select always only the root
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

function TObjectInspectorDlg.GetComponentEditorForSelection: TBaseComponentEditor;
var
  APersistent: TPersistent;
  AComponent: TComponent absolute APersistent;
  ADesigner: TIDesigner;
begin
  APersistent := GetSelectedPersistent;
  if not (APersistent is TComponent) then
    Exit(nil);
  ADesigner := FindRootDesigner(AComponent);
  if not (ADesigner is TComponentEditorDesigner) then
    Exit(nil);
  Result := GetComponentEditor(AComponent, TComponentEditorDesigner(ADesigner));
end;

procedure TObjectInspectorDlg.ComponentTreeDblClick(Sender: TObject);
var
  CompEditor: TBaseComponentEditor;
begin
  if (PropertyEditorHook = nil) or (PropertyEditorHook.LookupRoot = nil) then
    Exit;
  if not FSelection.IsEqual(ComponentTree.Selection) then
    ComponentTreeSelectionChanged(Sender);
  CompEditor := GetComponentEditorForSelection;
  if Assigned(CompEditor) then
  begin
    try
      CompEditor.Edit;
    finally
      CompEditor.Free;
    end;
  end;
end;

procedure TObjectInspectorDlg.ComponentTreeKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (Shift = []) and (Key = VK_DELETE) and
     (Selection.Count > 0) and
     (MessageDlg(oiscDelete, mtConfirmation,[mbYes, mbNo],0) = mrYes) then
  begin
    OnDeletePopupmenuItemClick(nil);
  end;
end;        

procedure TObjectInspectorDlg.ComponentTreeSelectionChanged(Sender: TObject);
begin
  if (PropertyEditorHook=nil) or (PropertyEditorHook.LookupRoot=nil) then exit;
  if FSelection.IsEqual(ComponentTree.Selection) then exit;
  Fselection.Assign(ComponentTree.Selection);
  RefreshSelection;
  if Assigned(FOnSelectPersistentsInOI) then
    FOnSelectPersistentsInOI(Self);
end;

procedure TObjectInspectorDlg.OnGridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Handled: Boolean;
begin
  Handled := false;

  //CTRL-[Shift]-TAB will select next or previous notebook tab
  if Key=VK_TAB then
  begin
    if Shift = [ssCtrl] then
    begin
      Handled := true;
      ShowNextPage(1);
    end else if Shift = [ssCtrl, ssShift] then
    begin
      Handled := true;
      ShowNextPage(-1);
    end;
  end;

  //Allow combobox navigation while it has focus
  if not Handled
  then Handled := AvailPersistentComboBox.Focused;

  if not Handled then
  begin
    //CTRL-ArrowDown will dropdown the component combobox
    if (Key=VK_DOWN) and (ssCtrl in Shift) then
    begin
      Handled := true;
      if AvailPersistentComboBox.Canfocus
      then AvailPersistentComboBox.SetFocus;
      AvailPersistentComboBox.DroppedDown := true;
    end;
  end;

  if not Handled then
  begin
    if Assigned(OnOIKeyDown) then 
      OnOIKeyDown(Self,Key,Shift);
    if (Key<>VK_UNKNOWN) and Assigned(OnRemainingKeyDown) then
      OnRemainingKeyDown(Self,Key,Shift);
  end
  else
    Key := VK_UNKNOWN;
end;

procedure TObjectInspectorDlg.OnGridKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(OnRemainingKeyUp) then OnRemainingKeyUp(Self,Key,Shift);
end;

procedure TObjectInspectorDlg.OnGridDblClick(Sender: TObject);
begin
  //
end;

procedure TObjectInspectorDlg.OnSetDefaultPopupmenuItemClick(Sender: TObject);
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

procedure TObjectInspectorDlg.OnAddToFavoritesPopupmenuItemClick(Sender: TObject);
begin
  //debugln('TObjectInspectorDlg.OnAddToFavouritePopupmenuItemClick');
  if Assigned(OnAddToFavourites) then OnAddToFavourites(Self);
end;

procedure TObjectInspectorDlg.OnRemoveFromFavoritesPopupmenuItemClick(
  Sender: TObject);
begin
  if Assigned(OnRemoveFromFavourites) then OnRemoveFromFavourites(Self);
end;

procedure TObjectInspectorDlg.OnViewRestrictionsPopupmenuItemClick(Sender: TObject);
begin
  DoViewRestricted;
end;

procedure TObjectInspectorDlg.OnUndoPopupmenuItemClick(Sender: TObject);
var
  CurGrid: TOICustomPropertyGrid;
  CurRow: TOIPropertyGridRow;
begin
  CurGrid:=GetActivePropertyGrid;
  CurRow:=GetActivePropertyRow;
  if CurRow=nil then exit;
  CurGrid.CurrentEditValue:=CurRow.Editor.GetVisualValue;
end;

procedure TObjectInspectorDlg.OnFindDeclarationPopupmenuItemClick(Sender: TObject);
begin
  if Assigned(OnFindDeclarationOfProperty) then
    OnFindDeclarationOfProperty(Self);
end;

procedure TObjectInspectorDlg.OnCutPopupmenuItemClick(Sender: TObject);
var
  ADesigner: TIDesigner;
begin
  if (Selection.Count > 0) and (Selection[0] is TComponent) then
  begin
    ADesigner := FindRootDesigner(Selection[0]);
    if ADesigner is TComponentEditorDesigner then
      TComponentEditorDesigner(ADesigner).CutSelection;
  end;
end;

procedure TObjectInspectorDlg.OnCopyPopupmenuItemClick(Sender: TObject);
var
  ADesigner: TIDesigner;
begin
  if (Selection.Count > 0) and (Selection[0] is TComponent) then
  begin
    ADesigner := FindRootDesigner(Selection[0]);
    if ADesigner is TComponentEditorDesigner then
      TComponentEditorDesigner(ADesigner).CopySelection;
  end;
end;

procedure TObjectInspectorDlg.OnPastePopupmenuItemClick(Sender: TObject);
var
  ADesigner: TIDesigner;
begin
  if Selection.Count > 0 then
  begin
    ADesigner := FindRootDesigner(Selection[0]);
    if ADesigner is TComponentEditorDesigner then
      TComponentEditorDesigner(ADesigner).PasteSelection([]);
  end;
end;

procedure TObjectInspectorDlg.OnDeletePopupmenuItemClick(Sender: TObject);
var
  ADesigner: TIDesigner;
  ACollection: TCollection;
  i: integer;
begin
  if (Selection.Count > 0) then
  begin
    ADesigner := FindRootDesigner(Selection[0]);
    if ADesigner is TComponentEditorDesigner then
    begin
      if Selection[0] is TCollection then
      begin
        ACollection := TCollection(Selection[0]);
        Selection.BeginUpdate;
        Selection.Clear;
        for i := 0 to ACollection.Count - 1 do
          Selection.Add(ACollection.Items[i]);
        Selection.EndUpdate;
        if Assigned(FOnSelectPersistentsInOI) then
          FOnSelectPersistentsInOI(Self);
      end;
      TComponentEditorDesigner(ADesigner).DeleteSelection;
    end;
  end;
end;

procedure TObjectInspectorDlg.OnGridModified(Sender: TObject);
begin
  DoModified(Self);
end;

procedure TObjectInspectorDlg.OnGridSelectionChange(Sender: TObject);
begin
  if Assigned(FOnSelectionChange) then OnSelectionChange(Self);
end;

function TObjectInspectorDlg.OnGridPropertyHint(Sender: TObject;
  PointedRow: TOIPropertyGridRow; ScreenPos: TPoint; aHintWindow: THintWindow;
  out HintWinRect: TRect; out AHint: string): boolean;
begin
  Result := False;
  if Assigned(FOnPropertyHint) then
    Result := FOnPropertyHint(Sender, PointedRow, ScreenPos,
       aHintWindow, HintWinRect, AHint);
end;

procedure TObjectInspectorDlg.SetAvailComboBoxText;
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

procedure TObjectInspectorDlg.HookGetSelection(const ASelection: TPersistentSelectionList);
begin
  if ASelection=nil then exit;
  ASelection.Assign(FSelection);
end;

procedure TObjectInspectorDlg.HookSetSelection(const ASelection: TPersistentSelectionList);
begin
  Selection := ASelection;
end;

procedure TObjectInspectorDlg.SetShowComponentTree(const AValue: boolean);
begin
  if FShowComponentTree = AValue then Exit;
  FShowComponentTree := AValue;
  BeginUpdate;
  try
    ShowComponentTreePopupMenuItem.Checked := FShowComponentTree;
    // hide controls while rebuilding
    if Splitter1 <> nil then
      Splitter1.Visible := False;
    ComponentTree.Visible := False;
    AvailPersistentComboBox.Visible := False;
    // rebuild controls
    ComponentTree.Parent := Self;
    ComponentTree.Align := alTop;
    if FShowComponentTree then
      CreateSplitter(True)
    else
    begin
      ComponentTree.Height := ComponentTreeHeight;
      FreeAndNil(Splitter1);
    end;
    ComponentTree.Visible := FShowComponentTree;
    AvailPersistentComboBox.Visible := not FShowComponentTree;
  finally
    EndUpdate;
  end;
end;

procedure TObjectInspectorDlg.SetShowFavorites(const AValue: Boolean);
begin
  if FShowFavorites = AValue then exit;
  FShowFavorites := AValue;
  NoteBook.Page[2].TabVisible := AValue;
end;

procedure TObjectInspectorDlg.SetShowInfoBox(const AValue: Boolean);
begin
  if FShowInfoBox = AValue then exit;
  FShowInfoBox := AValue;

  InfoPanel.Visible := AValue;

  if AValue then
    CreateSplitter(False)
  else
    FreeAndNil(Splitter2);
end;

procedure TObjectInspectorDlg.SetShowRestricted(const AValue: Boolean);
begin
  if FShowRestricted = AValue then exit;
  FShowRestricted := AValue;
  NoteBook.Page[3].TabVisible := AValue;
end;

procedure TObjectInspectorDlg.SetShowStatusBar(const AValue: Boolean);
begin
  if FShowStatusBar = AValue then exit;
  FShowStatusBar := AValue;
  StatusBar.Visible := AValue;
end;

procedure TObjectInspectorDlg.ShowNextPage(Delta: integer);
var
  NewPageIndex: Integer;
begin
  NewPageIndex := NoteBook.PageIndex;
  repeat
    NewPageIndex := NewPageIndex + Delta;
    if NewPageIndex >= NoteBook.PageCount then
      NewPageIndex := 0;
    if NewPageIndex < 0 then
      NewPageIndex := NoteBook.PageCount - 1;
    if NoteBook.Page[NewPageIndex].TabVisible then
    begin
      NoteBook.PageIndex := NewPageIndex;
      break;
    end;
  until NewPageIndex = NoteBook.PageIndex;
end;


procedure TObjectInspectorDlg.RestrictedPageShow(Sender: TObject);
begin
  //DebugLn('RestrictedPageShow');
  DoUpdateRestricted;
end;

procedure TObjectInspectorDlg.RestrictedPaint(
  ABox: TPaintBox; const ARestrictions: TWidgetSetRestrictionsArray);

  function OutVertCentered(AX: Integer; const AStr: String): TSize;
  begin
    Result := ABox.Canvas.TextExtent(AStr);
    ABox.Canvas.TextOut(AX, (ABox.Height - Result.CY) div 2, AStr);
  end;

var
  X, Y: Integer;
  lclPlatform: TLCLPlatform;
  None: Boolean;
  OldStyle: TBrushStyle;
begin
  X := 0;
  Y := (ABox.Height - IDEImages.Images_16.Height) div 2;
  OldStyle := ABox.Canvas.Brush.Style;
  try
    ABox.Canvas.Brush.Style := bsClear;
    None := True;
    for lclPlatform := Low(TLCLPlatform) to High(TLCLPlatform) do
    begin
      if ARestrictions[lclPlatform] = 0 then continue;
      None := False;
      IDEImages.Images_16.Draw(
        ABox.Canvas, X, Y,
        IDEImages.LoadImage(16, 'issue_' + LCLPlatformDirNames[lclPlatform]));
      Inc(X, 16);
      Inc(X, OutVertCentered(X, IntToStr(ARestrictions[lclPlatform])).CX);
    end;

    if None then
      OutVertCentered(4, oisNone);
  finally
    ABox.Canvas.Brush.Style := OldStyle;
  end;
end;

procedure TObjectInspectorDlg.WidgetSetRestrictedPaint(Sender: TObject);
begin
  if RestrictedProps <> nil then
    RestrictedPaint(
      WidgetSetsRestrictedBox, RestrictedProps.WidgetSetRestrictions);
end;

procedure TObjectInspectorDlg.ComponentRestrictedPaint(Sender: TObject);
var
  I, J: Integer;
  WidgetSetRestrictions: TWidgetSetRestrictionsArray;
begin
  if (RestrictedProps = nil) or (Selection = nil) then exit;

  FillChar(WidgetSetRestrictions, SizeOf(WidgetSetRestrictions), 0);
  for I := 0 to RestrictedProps.Count - 1 do
  begin
    if RestrictedProps.Items[I] is TOIRestrictedProperty then
      for J := 0 to Selection.Count - 1 do
        with RestrictedProps.Items[I] as TOIRestrictedProperty do
          CheckRestrictions(Selection[J].ClassType, WidgetSetRestrictions);
  end;

  RestrictedPaint(ComponentRestrictedBox, WidgetSetRestrictions);
end;

procedure TObjectInspectorDlg.CreateSplitter(TopSplitter: Boolean);
begin
  // vertical splitter between component tree and notebook
  if TopSplitter then
  begin
    Splitter1 := TSplitter.Create(Self);
    with Splitter1 do
    begin
      Name := 'Splitter1';
      Parent := Self;
      Align := alTop;
      Top := ComponentTreeHeight;
      Height := 5;
    end;
  end
  else
  begin
    Splitter2 := TSplitter.Create(Self);
    with Splitter2 do
    begin
      Name := 'Splitter2';
      Parent := Self;
      Align := alBottom;
      Top := InfoPanel.Top - 1;
      Height := 5;
    end;
  end;
end;

procedure TObjectInspectorDlg.DestroyNoteBook;
begin
  if NoteBook<>nil then
    NoteBook.Visible:=false;
  FreeAndNil(PropertyGrid);
  FreeAndNil(EventGrid);
  FreeAndNil(FavouriteGrid);
  FreeAndNil(RestrictedGrid);
  FreeAndNil(NoteBook);
end;

procedure TObjectInspectorDlg.CreateNoteBook;

  function CreateGrid(
    ATypeFilter: TTypeKinds; AOIPage: TObjectInspectorPage;
    ANotebookPage: Integer): TOICustomPropertyGrid;
  begin
    Result:=TOICustomPropertyGrid.CreateWithParams(
      Self, PropertyEditorHook, ATypeFilter, FDefaultItemHeight);
    with Result do
    begin
      Name := DefaultOIGridNames[AOIPage];
      Selection := Self.FSelection;
      Align := alClient;
      PopupMenu := MainPopupMenu;
      OnModified := @OnGridModified;
      OnSelectionChange := @OnGridSelectionChange;
      OnPropertyHint := @OnGridPropertyHint;
      OnOIKeyDown := @OnGridKeyDown;
      OnKeyUp := @OnGridKeyUp;
      OnDblClick := @OnGridDblClick;

      Parent := NoteBook.Page[ANotebookPage];
    end;
  end;

const
  PROPS = [
    tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkSet,{ tkMethod,}
    tkSString, tkLString, tkAString, tkWString, tkVariant,
    {tkArray, tkRecord, tkInterface,} tkClass, tkObject, tkWChar, tkBool,
    tkInt64, tkQWord];
  function AddPage(PageName, TabCaption: string): TTabSheet;
  begin
    Result:=TTabSheet.Create(Self);
    Result.Name:=PageName;
    Result.Caption:=TabCaption;
    Result.Parent:=NoteBook;
  end;
var
  APage: TTabSheet;
begin
  DestroyNoteBook;

  // NoteBook
  NoteBook:=TPageControl.Create(Self);
  with NoteBook do
  begin
    Name := 'NoteBook';
    Parent := Self;
    Align := alClient;
    PopupMenu := MainPopupMenu;
  end;

  AddPage(DefaultOIPageNames[oipgpProperties],oisProperties);

  AddPage(DefaultOIPageNames[oipgpEvents],oisEvents);

  APage:=AddPage(DefaultOIPageNames[oipgpFavourite],oisFavorites);
  APage.TabVisible := ShowFavorites;

  APage:=AddPage(DefaultOIPageNames[oipgpRestricted],oisRestricted);
  APage.TabVisible := ShowRestricted;
  APage.OnShow := @RestrictedPageShow;
    
  NoteBook.PageIndex:=0;

  PropertyGrid := CreateGrid(PROPS, oipgpProperties, 0);
  EventGrid := CreateGrid([tkMethod], oipgpEvents, 1);
  FavouriteGrid := CreateGrid(PROPS + [tkMethod], oipgpFavourite, 2);
  FavouriteGrid.Favourites := FFavourites;
  RestrictedGrid := CreateGrid(PROPS + [tkMethod], oipgpRestricted, 3);

  RestrictedPanel := TPanel.Create(Self);
  with RestrictedPanel do
  begin
    Align := alTop;
    BevelOuter := bvNone;
    Parent := NoteBook.Page[3];
  end;
  
  RestrictedInnerPanel := TPanel.Create(Self);
  with RestrictedInnerPanel do
  begin
    BevelOuter := bvNone;
    BorderSpacing.Around := 6;
    Parent := RestrictedPanel;
  end;
  
  WidgetSetsRestrictedLabel := TLabel.Create(Self);
  with WidgetSetsRestrictedLabel do
  begin
    Caption := oisWidgetSetRestrictions;
    Top := 1;
    Align := alTop;
    AutoSize := True;
    Parent := RestrictedInnerPanel;
  end;
  
  WidgetSetsRestrictedBox := TPaintBox.Create(Self);
  with WidgetSetsRestrictedBox do
  begin
    Top := 2;
    Align := alTop;
    Height := 24;
    OnPaint := @WidgetSetRestrictedPaint;
    Parent := RestrictedInnerPanel;
  end;
  
  ComponentRestrictedLabel := TLabel.Create(Self);
  with ComponentRestrictedLabel do
  begin
    Caption := oisComponentRestrictions;
    Top := 3;
    Align := alTop;
    AutoSize := True;
    Parent := RestrictedInnerPanel;
  end;
  
  ComponentRestrictedBox := TPaintBox.Create(Self);
  with ComponentRestrictedBox do
  begin
    Top := 4;
    Align := alTop;
    Height := 24;
    OnPaint := @ComponentRestrictedPaint;
    Parent := RestrictedInnerPanel;
  end;
  
  RestrictedInnerPanel.AutoSize := True;
  RestrictedPanel.AutoSize := True;
end;

procedure TObjectInspectorDlg.KeyDown(var Key: Word; Shift: TShiftState);
var
  CurGrid: TOICustomPropertyGrid;
begin
  CurGrid:=GetActivePropertyGrid;
  //Do not disturb the combobox navigation while it has focus
  if not AvailPersistentComboBox.DroppedDown then begin
    if CurGrid<>nil then begin
      CurGrid.HandleStandardKeys(Key,Shift);
      if Key=VK_UNKNOWN then exit;
    end;
  end;
  inherited KeyDown(Key, Shift);
  if (Key<>VK_UNKNOWN) and Assigned(OnRemainingKeyDown) then
    OnRemainingKeyDown(Self,Key,Shift);
end;

procedure TObjectInspectorDlg.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  if (Key<>VK_UNKNOWN) and Assigned(OnRemainingKeyUp) then
    OnRemainingKeyUp(Self,Key,Shift);
end;

procedure TObjectInspectorDlg.DoModified(Sender: TObject);
begin
  if Assigned(FOnModified) then
    FOnModified(Sender)
end;

function TObjectInspectorDlg.GetSelectedPersistent: TPersistent;
begin
  if ComponentTree.Selection.Count = 1 then
    Result := ComponentTree.Selection[0]
  else
    Result := nil;
end;

procedure TObjectInspectorDlg.OnShowHintPopupMenuItemClick(Sender : TObject);
var
  Page: TObjectInspectorPage;
begin
  for Page := Low(TObjectInspectorPage) to High(TObjectInspectorPage) do
    if GridControl[Page] <> nil then
      GridControl[Page].ShowHint := not GridControl[Page].ShowHint;
end;

procedure TObjectInspectorDlg.OnShowOptionsPopupMenuItemClick(Sender: TObject);
begin
  if Assigned(FOnShowOptions) then FOnShowOptions(Sender);
end;

procedure TObjectInspectorDlg.OnShowComponentTreePopupMenuItemClick(Sender: TObject);
begin
  ShowComponentTree:=not ShowComponentTree;
end;

procedure TObjectInspectorDlg.OnMainPopupMenuPopup(Sender: TObject);
var
  EditorVerbSeparator: TMenuItem;

  procedure RemoveComponentEditorMenuItems;
  var
    I: Integer;
  begin
    for I := MainPopupMenu.Items.Count - 1 downto 0 do
      if Pos('ComponentEditorVerbMenuItem', MainPopupMenu.Items[I].Name) = 1 then
        MainPopupMenu.Items[I].Free;
  end;

  procedure AddComponentEditorMenuItems;
  var
    I, VerbCount: Integer;
    Item: TMenuItem;
  begin
    VerbCount := ComponentEditor.GetVerbCount;
    for I := 0 to VerbCount - 1 do
    begin
      Item := NewItem(ComponentEditor.GetVerb(I), 0, False, True,
        @DoComponentEditorVerbMenuItemClick, 0, 'ComponentEditorVerbMenuItem' + IntToStr(i));
      ComponentEditor.PrepareItem(I, Item);
      MainPopupMenu.Items.Insert(I, Item);
    end;
    // insert the separator
    if VerbCount > 0 then
    begin
      EditorVerbSeparator := NewLine;
      EditorVerbSeparator.Name := 'ComponentEditorVerbMenuItem' + IntToStr(VerbCount);
      MainPopupMenu.Items.Insert(VerbCount, EditorVerbSeparator);
    end;
  end;

  procedure AddCollectionEditorMenuItems(ACollection: TCollection);
  var
    Item: TMenuItem;
  begin
    Item := NewItem(oisAddCollectionItem, 0, False, True,
      @DoCollectionAddItem, 0, 'ComponentEditorVerbMenuItem0');
    MainPopupMenu.Items.Insert(0, Item);
    EditorVerbSeparator := NewLine;
    EditorVerbSeparator.Name := 'ComponentEditorVerbMenuItem1';
    MainPopupMenu.Items.Insert(1, EditorVerbSeparator);
  end;

  procedure AddZOrderMenuItems;
  var
    ZItem, Item: TMenuItem;
  begin
    ZItem := NewSubMenu(oisZOrder, 0, 'ComponentEditorVerbMenuItemZOrder', [], True);
    Item := NewItem(oisOrderMoveToFront, 0, False, True, @DoZOrderItemClick, 0, '');
    Item.ImageIndex := IDEImages.LoadImage(16, 'Order_move_front');
    Item.Tag := 0;
    ZItem.Add(Item);
    Item := NewItem(oisOrderMoveToBack, 0, False, True, @DoZOrderItemClick, 0, '');
    Item.ImageIndex := IDEImages.LoadImage(16, 'Order_move_back');
    Item.Tag := 1;
    ZItem.Add(Item);
    Item := NewItem(oisOrderForwardOne, 0, False, True, @DoZOrderItemClick, 0, '');
    Item.ImageIndex := IDEImages.LoadImage(16, 'Order_forward_one');
    Item.Tag := 2;
    ZItem.Add(Item);
    Item := NewItem(oisOrderBackOne, 0, False, True, @DoZOrderItemClick, 0, '');
    Item.ImageIndex := IDEImages.LoadImage(16, 'Order_back_one');
    Item.Tag := 3;
    ZItem.Add(Item);
    if EditorVerbSeparator <> nil then
      MainPopupMenu.Items.Insert(EditorVerbSeparator.MenuIndex + 1, ZItem)
    else
      MainPopupMenu.Items.Insert(0, ZItem);
    Item := NewLine;
    Item.Name := 'ComponentEditorVerbMenuItemZOrderSeparator';
    MainPopupMenu.Items.Insert(ZItem.MenuIndex + 1, Item);
  end;

var
  DefaultStr: String;
  CurGrid: TOICustomPropertyGrid;
  CurRow: TOIPropertyGridRow;
  Persistent: TPersistent;
begin
  RemoveComponentEditorMenuItems;
  ShowHintsPopupMenuItem.Checked := PropertyGrid.ShowHint;
  // show component editors only for component treeview
  if MainPopupMenu.PopupComponent = ComponentTree then
  begin
    EditorVerbSeparator := nil;
    ComponentEditor := GetComponentEditorForSelection;
    if ComponentEditor <> nil then
      AddComponentEditorMenuItems
    else
    begin
      // check if it is a TCollection
      Persistent := GetSelectedPersistent;
      if Persistent is TCollection then
        AddCollectionEditorMenuItems(TCollection(Persistent))
      else
      if Persistent is TCollectionItem then
        AddCollectionEditorMenuItems(TCollectionItem(Persistent).Collection);
    end;

    // add Z-Order menu

    if (Selection.Count = 1) and (Selection[0] is TControl) then
      AddZOrderMenuItems;

    CutPopupMenuItem.Visible := (Selection.Count > 0) and (Selection[0] is TComponent);
    CopyPopupMenuItem.Visible := (Selection.Count > 0) and (Selection[0] is TComponent);
    PastePopupMenuItem.Visible := (Selection.Count > 0) and (Selection[0] is TComponent);
    DeletePopupMenuItem.Visible := True;
    OptionsSeparatorMenuItem2.Visible := True;
  end
  else
  begin
    CutPopupMenuItem.Visible := False;
    CopyPopupMenuItem.Visible := False;
    PastePopupMenuItem.Visible := False;
    DeletePopupMenuItem.Visible := False;
    OptionsSeparatorMenuItem2.Visible := False;
  end;


  if (MainPopupMenu.PopupComponent is TOICustomPropertyGrid) then
  begin
    SetDefaultPopupMenuItem.Visible := True;

    SetDefaultPopupMenuItem.Enabled := GetCurRowDefaultValue(DefaultStr);
    if SetDefaultPopupMenuItem.Enabled then
      SetDefaultPopupMenuItem.Caption := Format(oisSetToDefault, [DefaultStr])
    else
      SetDefaultPopupMenuItem.Caption := oisSetToDefaultValue;

    AddToFavoritesPopupMenuItem.Visible :=
      (Favourites <> nil) and
      ShowFavorites and
      (GetActivePropertyGrid <> FavouriteGrid) and
      Assigned(OnAddToFavourites) and
      (GetActivePropertyRow <> nil);

    RemoveFromFavoritesPopupMenuItem.Visible :=
      (Favourites<>nil) and
      ShowFavorites and
      (GetActivePropertyGrid = FavouriteGrid) and
      Assigned(OnRemoveFromFavourites) and
      (GetActivePropertyRow <> nil);

    CurGrid := GetActivePropertyGrid;
    CurRow := GetActivePropertyRow;
    UndoPropertyPopupMenuItem.Visible := True;
    UndoPropertyPopupMenuItem.Enabled := (CurRow<>nil) and (CurRow.Editor.GetVisualValue <> CurGrid.CurrentEditValue);
    if CurRow=nil then begin
      FindDeclarationPopupmenuItem.Visible := False;
    end
    else begin
      FindDeclarationPopupmenuItem.Visible := true;
      FindDeclarationPopupmenuItem.Caption:=Format(oisJumpToDeclarationOf, [
        CurRow.Name]);
      FindDeclarationPopupmenuItem.Hint:=Format(oisJumpToDeclarationOf, [CurRow.
        Editor.GetPropertyPath(0)]);
    end;
    ViewRestrictedPropertiesPopupMenuItem.Visible := True;
    OptionsSeparatorMenuItem.Visible := True;
  end
  else
  begin
    SetDefaultPopupMenuItem.Visible := False;
    AddToFavoritesPopupMenuItem.Visible := False;
    RemoveFromFavoritesPopupMenuItem.Visible := False;
    UndoPropertyPopupMenuItem.Visible := False;
    FindDeclarationPopupmenuItem.Visible := False;
    ViewRestrictedPropertiesPopupMenuItem.Visible := False;
    OptionsSeparatorMenuItem.Visible := False;
  end;
  //debugln(['TObjectInspectorDlg.OnMainPopupMenuPopup ',FindDeclarationPopupmenuItem.Visible]);
end;

procedure TObjectInspectorDlg.DoUpdateRestricted;
begin
  if Assigned(FOnUpdateRestricted) then FOnUpdateRestricted(Self);
end;

procedure TObjectInspectorDlg.DoViewRestricted;
begin
  if Assigned(FOnViewRestricted) then FOnViewRestricted(Self);
end;

procedure TObjectInspectorDlg.DoComponentEditorVerbMenuItemClick(Sender: TObject);
var
  Verb: integer;
  AMenuItem: TMenuItem;
begin
  if Sender is TMenuItem then
    AMenuItem := TMenuItem(Sender)
  else
    Exit;
  // component menu items start from the start of menu
  Verb := AMenuItem.MenuIndex;
  ComponentEditor.ExecuteVerb(Verb);
end;

procedure TObjectInspectorDlg.DoCollectionAddItem(Sender: TObject);
var
  Persistent: TPersistent;
  Collection: TCollection absolute Persistent;
  ci: TCollectionItem;
begin
  Persistent := GetSelectedPersistent;
  if Persistent = nil then
    Exit;
  if Persistent is TCollectionItem then
    Persistent := TCollectionItem(Persistent).Collection;
  if not (Persistent is TCollection) then
    Exit;
  ci:=Collection.Add;
  GlobalDesignHook.PersistentAdded(ci,false);
  DoModified(Self);
  Selection.ForceUpdate := True;
  try
    SetSelection(Selection);
  finally
    Selection.ForceUpdate := False;
  end;
end;

procedure TObjectInspectorDlg.DoZOrderItemClick(Sender: TObject);
var
  Control: TControl;
begin
  if not (Sender is TMenuItem) then Exit;
  if (Selection.Count <> 1) or
     not (Selection[0] is TControl) then Exit;
  Control := TControl(Selection[0]);
  if Control.Parent = nil then Exit;

  case TMenuItem(Sender).Tag of
    0: Control.BringToFront;
    1: Control.SendToBack;
    2: Control.Parent.SetControlIndex(Control, Control.Parent.GetControlIndex(Control) + 1);
    3: Control.Parent.SetControlIndex(Control, Control.Parent.GetControlIndex(Control) - 1);
  end;
  DoModified(Self);
end;

function TObjectInspectorDlg.GetComponentTreeHeight: integer;
begin
  if Assigned(ComponentTree) then
    Result := ComponentTree.Height
  else
    Result := FComponentTreeHeight;
end;

function TObjectInspectorDlg.GetInfoBoxHeight: integer;
begin
  if Assigned(InfoPanel) then
    Result := InfoPanel.Height
  else
    Result := FInfoBoxHeight;
end;

procedure TObjectInspectorDlg.HookRefreshPropertyValues;
begin
  RefreshPropertyValues;
end;

procedure TObjectInspectorDlg.ActivateGrid(Grid: TOICustomPropertyGrid);
begin
  if Grid=PropertyGrid then NoteBook.PageIndex:=0
  else if Grid=EventGrid then NoteBook.PageIndex:=1
  else if Grid=FavouriteGrid then NoteBook.PageIndex:=2
  else if Grid=RestrictedGrid then NoteBook.PageIndex:=3;
end;

procedure TObjectInspectorDlg.FocusGrid(Grid: TOICustomPropertyGrid);
var
  Index: Integer;
begin
  if Grid=nil then
    Grid := GetActivePropertyGrid
  else
    ActivateGrid(Grid);
  if Grid <> nil then
  begin
    Index := Grid.ItemIndex;
    if Index < 0 then
      Index := 0;
    Grid.SetItemIndexAndFocus(Index);
  end;
end;

function TObjectInspectorDlg.GetGridControl(Page: TObjectInspectorPage
  ): TOICustomPropertyGrid;
begin
  case Page of
  oipgpFavourite: Result:=FavouriteGrid;
  oipgpEvents: Result:=EventGrid;
  oipgpRestricted: Result:=RestrictedGrid;
  else  Result:=PropertyGrid;
  end;
end;

procedure TObjectInspectorDlg.SetComponentEditor(
  const AValue: TBaseComponentEditor);
begin
  if FComponentEditor <> AValue then
  begin
    FComponentEditor.Free;
    FComponentEditor := AValue;
  end;
end;

procedure TObjectInspectorDlg.SetFavourites(const AValue: TOIFavouriteProperties);
begin
  //debugln('TObjectInspectorDlg.SetFavourites ',dbgsName(Self));
  if FFavourites=AValue then exit;
  FFavourites:=AValue;
  FavouriteGrid.Favourites:=FFavourites;
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
  if SaveOnChangeTIObject then
    SaveChanges;
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
  FSaveOnChangeTIObject:=true;
  FAutoFreeHook:=true;
  CreateWithParams(TheOwner,Hook,AllTypeKinds,25);
end;

destructor TCustomPropertiesGrid.Destroy;
begin
  if FAutoFreeHook then
    FPropertyEditorHook.Free;
  inherited Destroy;
end;

initialization
  {$I images/ideintf_images.lrs}
  
finalization

end.

