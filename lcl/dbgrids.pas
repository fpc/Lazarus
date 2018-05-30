
{ $Id$}
{
 /***************************************************************************
                               DBGrids.pas
                               -----------
                     An interface to DB aware Controls
                     Initial Revision : Sun Sep 14 2003


 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
{
TDBGrid and TComponentDataLink for Lazarus
Copyright (C) 2003  Jesus Reyes Aguilar.
email: jesusrmx@yahoo.com.mx

TComponentDatalink idea was taken from Joanna Carter's article
"The Ultimate Datalink?" Delphi Magazine Issue #30 February 1998
}
unit DBGrids;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, FileUtil, DB, LazUTF8, LazLoggerBase, LCLStrConsts,
  LCLIntf, LCLType, LMessages, LResources, Controls, StdCtrls, Graphics, Grids,
  Dialogs, Themes, Variants, Clipbrd, ImgList, Laz2_XMLCfg;

{$if FPC_FULLVERSION<20701}
  {$DEFINE noautomatedbookmark}
{$endif}

const
  DEFINDICATORCOLWIDTH = 12;

type
  TCustomDbGrid = class;
  TColumn = class;
  EInvalidGridOperation = class(Exception);


  TDBGridOption = (
    dgEditing,                          // Enable or disable editing data
    dgTitles,                           // Show column titles
    dgIndicator,                        // Show current row indicator
    dgColumnResize,
    dgColumnMove,
    dgColLines,                         // Show vertical lines between columns
    dgRowLines,                         // Show horizontal lines between rows
    dgTabs,                             // Allow using TAB key to navigate grid
    dgAlwaysShowEditor,
    dgRowSelect,
    dgAlwaysShowSelection,
    dgConfirmDelete,
    dgCancelOnExit,
    dgMultiselect,                      // Allow selection of multiple nonadjacent rows
    dgHeaderHotTracking,
    dgHeaderPushedLook,
    dgPersistentMultiSelect,
    dgAutoSizeColumns,
    dgAnyButtonCanSelect,               // any mouse button can move selection
    dgDisableDelete,                    // disable deleting records with Ctrl+Delete
    dgDisableInsert,                    // disable inserting (or append) records
    dgCellHints,                        // show individual cell hints
    dgTruncCellHints,                   // show cell hints if cell text is too long
    dgCellEllipsis,                     // show ... if cell text is truncated
    dgRowHighlight,                     // Highlight current row
    dgThumbTracking,
    dgDblClickAutoSize,                 // dblclicking columns borders (on hdrs) resize col.
    dgDisplayMemoText                   // show memo content instead of (memo) for ftMemo fields
  );
  TDbGridOptions = set of TDbGridOption;

  TDbGridExtraOption = (
    dgeAutoColumns,       // if uncustomized columns, add them anyway?
    dgeCheckboxColumn     // enable the use of checkbox in columns
  );
  TDbGridExtraOptions = set of TDbGridExtraOption;

  TDbGridStatusItem = (gsUpdatingData, gsAddingAutoColumns, gsRemovingAutoColumns,
                       gsAutoSized, gsStartEditing, gsLoadingGrid);
  TDbGridStatus = set of TDbGridStatusItem;

  TDataSetScrolledEvent =
    procedure(DataSet: TDataSet; Distance: Integer) of object;

  TFocusControlEvent =
    procedure(aField: TFieldRef) of object;

  TDBGridClickEvent =
    procedure(Column: TColumn) of object;

  TMovedEvent =
    procedure(Sender: TObject; FromIndex, ToIndex: Integer) of object;

  TDrawColumnCellEvent =
    procedure(Sender: TObject; const Rect: TRect; DataCol: Integer;
              Column: TColumn; State: TGridDrawState) of object;

  TGetDbEditMaskEvent =
    procedure (Sender: TObject; const Field: TField;
               var Value: string) of object;

  TDbGridSelEditorEvent =
    procedure(Sender: TObject; Column: TColumn;
              var Editor: TWinControl) of object;

  TPrepareDbGridCanvasEvent =
    procedure(sender: TObject; DataCol: Integer;
              Column: TColumn; AState: TGridDrawState) of object;

  TDbGridCheckBoxBitmapEvent =
    procedure(Sender: TObject; const CheckedState: TCheckboxState;
              var ABitmap: TBitmap) of object;

  TDbGridCheckboxStateEvent =
    procedure(Sender: TObject; Column: TColumn;
              var AState: TCheckboxState) of object;

  TDbGridCellHintEvent =
    procedure(Sender: TObject; Column: TColumn; var AText: String) of object;

type

  TBookmarkList = class;
  TBookmarkedRecordEnumeratorOptions = set of
      (
        breDisableDataset,
        breStopOnInvalidBookmark,
        breRestoreCurrent
      );

  { TBookmarkedRecordEnumerator }

  TBookmarkedRecordEnumerator = class
  private
    fBookmarkList: TBookmarkList;
    fBookmarkIndex: Integer;
    fCurrent, fBook: TBookmark;
    fDataset: TDataset;
    fOptions: TBookmarkedRecordEnumeratorOptions;
  public
    constructor Create(bookList: TBookmarkList; aGrid: TCustomDbGrid;
        anOptions: TBookmarkedRecordEnumeratorOptions);
    destructor Destroy; override;
    function MoveNext: boolean;
    function GetEnumerator: TBookmarkedRecordEnumerator;
    property Current: TBookmark read fCurrent;
    property Options: TBookmarkedRecordEnumeratorOptions read fOptions write fOptions;
  end;

  { TBookmarkList }

  TBookmarkList = class
  private
    FList: TFPList; // list of TBookmark
    FGrid: TCustomDbGrid;
    FDataset: TDataset;
    FUseCompareBookmarks: boolean;
    FCanDoBinarySearch: boolean;
    function GetCount: integer;
    function GetCurrentRowSelected: boolean;
    function GetItem(AIndex: Integer): TBookmark;
    procedure SetCurrentRowSelected(const AValue: boolean);
    procedure CheckActive;
  public
    constructor Create(AGrid: TCustomDbGrid);
    destructor Destroy; override;

    procedure Clear;
    procedure Delete;
    function  Find(const Item: TBookmark; var AIndex: Integer): boolean;
    function  IndexOf(const Item: TBookmark): Integer;
    function  Refresh: boolean;
    function  GetEnumerator(opt: TBookmarkedRecordEnumeratorOptions =
                [breDisableDataset, breRestoreCurrent]): TBookmarkedRecordEnumerator;

    property Count: integer read GetCount;
    property CurrentRowSelected: boolean
      read GetCurrentRowSelected write SetCurrentRowSelected;
    property Items[AIndex: Integer]: TBookmark read GetItem; default;
  end;

  { TComponentDataLink }

  TComponentDataLink=class(TDatalink)
  private
    FDataSet: TDataSet;
    FDataSetName: string;
    FModified: Boolean;
    FOnDatasetChanged: TDatasetNotifyEvent;
    fOnDataSetClose: TDataSetNotifyEvent;
    fOnDataSetOpen: TDataSetNotifyEvent;
    FOnDataSetScrolled: TDataSetScrolledEvent;
    FOnEditingChanged: TDataSetNotifyEvent;
    fOnFocusControl: TFocusControlEvent;
    fOnInvalidDataSet: TDataSetNotifyEvent;
    fOnInvalidDataSource: TDataSetNotifyEvent;
    FOnLayoutChanged: TDataSetNotifyEvent;
    fOnNewDataSet: TDataSetNotifyEvent;
    FOnRecordChanged: TFieldNotifyEvent;
    FOnUpdateData: TDataSetNotifyEvent;

    function GetDataSetName: string;
    function GetFields(Index: Integer): TField;
    procedure SetDataSetName(const AValue: string);
  protected
    procedure RecordChanged(Field: TField); override;
    procedure DataSetChanged; override;
    procedure ActiveChanged; override;
    procedure LayoutChanged; override;
    procedure DataSetScrolled(Distance: Integer); override;
    procedure FocusControl(Field: TFieldRef); override;
    // Testing Events
    procedure CheckBrowseMode; override;
    procedure EditingChanged; override;
    procedure UpdateData; override;
    function  MoveBy(Distance: Integer): Integer; override;
    property  Modified: Boolean read FModified write FModified;
  public
    property OnRecordChanged: TFieldNotifyEvent read FOnRecordChanged write FOnRecordChanged;
    property OnDataSetChanged: TDatasetNotifyEvent read FOnDatasetChanged write FOnDataSetChanged;
    property OnNewDataSet: TDataSetNotifyEvent read fOnNewDataSet write fOnNewDataSet;
    property OnDataSetOpen: TDataSetNotifyEvent read fOnDataSetOpen write fOnDataSetOpen;
    property OnInvalidDataSet: TDataSetNotifyEvent read fOnInvalidDataSet write fOnInvalidDataSet;
    property OnInvalidDataSource: TDataSetNotifyEvent read fOnInvalidDataSource write fOnInvalidDataSource;
    property OnFocusControl: TFocusControlEvent read fOnFocusControl write fOnFocusControl;
    property OnLayoutChanged: TDataSetNotifyEvent read FOnLayoutChanged write FOnLayoutChanged;
    property OnDataSetClose: TDataSetNotifyEvent read fOnDataSetClose write fOnDataSetClose;
    property OnDataSetScrolled: TDataSetScrolledEvent read FOnDataSetScrolled write FOnDataSetScrolled;
    property OnEditingChanged: TDataSetNotifyEvent read FOnEditingChanged write FOnEditingChanged;
    property OnUpdateData: TDataSetNotifyEvent read FOnUpdateData write FOnUpdateData;
    property DataSetName:string read GetDataSetName write SetDataSetName;
    property Fields[Index: Integer]: TField read GetFields;
    property VisualControl;
  end;

  { TColumn }

  TColumnTitle = class(TGridColumnTitle)
  protected
    function  GetDefaultCaption: string; override;
  end;

  { TColumn }

  TColumn = class(TGridColumn)
  private
    FDisplayFormat: String;
    FDisplayFormatChanged: boolean;
    FFieldName: String;
    FField: TField;
    FIsAutomaticColumn: boolean;
    FDesignIndex: Integer;
    procedure ApplyDisplayFormat;
    function  GetDataSet: TDataSet;
    function  GetDisplayFormat: string;
    function  GetField: TField;
    function  GetIsDesignColumn: boolean;
    function  IsDisplayFormatStored: boolean;
    procedure SetDisplayFormat(const AValue: string);
    procedure SetField(const AValue: TField);
    procedure SetFieldName(const AValue: String);
  protected
    function  CreateTitle: TGridColumnTitle; override;
    function  GetDefaultAlignment: TAlignment; override;
    function  GetDefaultDisplayFormat: string;
    function  GetDefaultValueChecked: string; override;
    function  GetDefaultValueUnchecked: string; override;
    function  GetDefaultVisible: boolean; override;
    function  GetDisplayName: string; override;
    function  GetDefaultReadOnly: boolean; override;
    function  GetDefaultWidth: Integer; override;
    function  GetPickList: TStrings; override;
    property  IsAutomaticColumn: boolean read FIsAutomaticColumn;
    property  IsDesignColumn: boolean read GetIsDesignColumn;
    procedure LinkField;
  public
    constructor Create(ACollection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    function  IsDefault: boolean; override;
    property  DesignIndex: integer read FDesignIndex;
    property  Field: TField read GetField write SetField;
  published
    property  FieldName: String read FFieldName write SetFieldName;
    property DisplayFormat: string read GetDisplayFormat write SetDisplayFormat
      stored IsDisplayFormatStored;
  end;

  TColumnOrder = (coDesignOrder, coFieldIndexOrder);

  { TDBGridColumns }
  TDBGridColumns = class(TGridColumns)
  private
    function GetColumn(Index: Integer): TColumn;
    procedure SetColumn(Index: Integer; Value: TColumn);
  protected
    procedure Update(Item: TCollectionItem); override;
    function ColumnFromField(Field: TField): TColumn;
    function  HasAutomaticColumns: boolean;
    function  HasDesignColumns: boolean;
    procedure RemoveAutoColumns;
  public
    function  Add: TColumn;
    procedure LinkFields;
    procedure ResetColumnsOrder(ColumnOrder: TColumnOrder);
    property Items[Index: Integer]: TColumn read GetColumn write SetColumn; default;
  end;

  { TCustomDBGrid }

  TCustomDBGrid=class(TCustomGrid)
  private
    FDataLink: TComponentDataLink;
    FExtraOptions: TDBGridExtraOptions;
    FOnCellClick: TDBGridClickEvent;
    FOnColEnter,FOnColExit: TNotifyEvent;
    FOnColumnMoved: TMovedEvent;
    FOnColumnSized: TNotifyEvent;
    FOnDrawColumnCell: TDrawColumnCellEvent;
    FOnDrawColumnTitle: TDrawColumnCellEvent;
    FOnFieldEditMask: TGetDbEditMaskEvent;
    FOnTitleClick: TDBGridClickEvent;
    FOnSelectEditor: TDbGridSelEditorEvent;
    FOnCheckboxBitmap: TDbGridCheckBoxBitmapEvent;
    FOnCheckboxState: TDbGridCheckboxStateEvent;
    FOptions: TDBGridOptions;
    FReadOnly: Boolean;
    FColEnterPending: Boolean;
    FLayoutChangedCount: integer;
    FTempText : string;
    FDrawingActiveRecord: Boolean;
    FDrawingMultiSelRecord: Boolean;
    FDrawingEmptyDataset: Boolean;
    FEditingColumn: Integer;
    FOldPosition: Integer;
    FDefaultColWidths: boolean;
    FGridStatus: TDBGridStatus;
    FOldControlStyle: TControlStyle;
    FSelectedRows: TBookmarkList;
    FOnPrepareCanvas: TPrepareDbGridCanvasEvent;
    FKeyBookmark: TBookmark;
    FKeySign: Integer;
    FSavedRecord: Integer;
    FOnGetCellHint: TDbGridCellHintEvent;
    FOnRowMoved: TMovedEvent;
    procedure EmptyGrid;
    function GetColumns: TDBGridColumns;
    function GetCurrentColumn: TColumn;
    function GetCurrentField: TField;
    function GetDataSource: TDataSource;
    function GetFirstColumn: TColumn;
    function GetLastColumn: TColumn;
    function GetRecordCount: Integer;
    function GetSelectedFieldRect: TRect;
    function GetSelectedIndex: Integer;
    procedure OnRecordChanged(Field:TField);
    procedure OnDataSetChanged(aDataSet: TDataSet);
    procedure OnDataSetOpen(aDataSet: TDataSet);
    procedure OnDataSetClose(aDataSet: TDataSet);
    procedure OnEditingChanged(aDataSet: TDataSet);
    procedure OnInvalidDataSet(aDataSet: TDataSet);
    procedure OnInvalidDataSource(aDataSet: TDataset);
    procedure OnFocusControl(aField: TFieldRef);
    procedure OnLayoutChanged(aDataSet: TDataSet);
    procedure OnNewDataSet(aDataSet: TDataset);
    procedure OnDataSetScrolled(aDataSet:TDataSet; Distance: Integer);
    procedure OnUpdateData(aDataSet: TDataSet);
    procedure SetColumns(const AValue: TDBGridColumns);
    //procedure ReadColumns(Reader: TReader);
    //procedure SetColumns(const AValue: TDBGridColumns);
    procedure SetCurrentField(const AValue: TField);
    procedure SetDataSource(const AValue: TDataSource);
    procedure SetExtraOptions(const AValue: TDBGridExtraOptions);
    procedure SetOptions(const AValue: TDBGridOptions);
    procedure SetRowMoved(AValue: TMovedEvent);
    procedure SetSelectedIndex(const AValue: Integer);
    procedure UpdateBufferCount;

    // Temporal
    function GetColumnCount: Integer;

    function DefaultFieldColWidth(F: TField): Integer;

    procedure UpdateGridColumnSizes;
    procedure UpdateScrollbarRange;
    procedure DoLayoutChanged;
    //procedure WriteColumns(Writer: TWriter);

    procedure RestoreEditor;
    function  ISEOF: boolean;
    function  ValidDataSet: boolean;
    function  InsertCancelable: boolean;
    procedure StartUpdating;
    procedure EndUpdating;
    function  UpdatingData: boolean;
    procedure SwapCheckBox;
    procedure ToggleSelectedRow;
    procedure SelectRecord(AValue: boolean);
    procedure GetScrollbarParams(out aRange, aPage, aPos: Integer);
    procedure CMGetDataLink(var Message: TLMessage); message CM_GETDATALINK;
    procedure ClearSelection(selCurrent:boolean=false);
    function  NeedAutoSizeColumns: boolean;
    procedure RenewColWidths;
    procedure InternalAutoSizeColumn(aCol: Integer; aCanvas: TCanvas; aDatalinkActive: Boolean);
    procedure DoHeaderClick(Index: Integer);
  protected
    procedure AddAutomaticColumns;
    procedure AssignTo(Dest: TPersistent); override;
    procedure AutoAdjustColumn(aCol: Integer); override;
    procedure BeforeMoveSelection(const DCol,DRow: Integer); override;
    procedure BeginLayout;
    procedure CellClick(const aCol,aRow: Integer; const Button:TMouseButton); override;
    function  CheckDisplayMemo(aField: TField): boolean;
    procedure InvalidateSizes;
    procedure ColRowMoved(IsColumn: Boolean; FromIndex,ToIndex: Integer); override;
    function  ColumnEditorStyle(aCol: Integer; F: TField): TColumnButtonStyle;
    function  CreateColumns: TGridColumns; override;
    procedure CreateWnd; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DefaultDrawCell(aCol,aRow: Integer; aRect: TRect; aState: TGridDrawState);
    function  DefaultEditorStyle(const Style:TColumnButtonStyle; const F:TField): TColumnButtonStyle;
    procedure DoCopyToClipboard; override;
    procedure DoExit; override;
    function  DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function  DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure DoOnChangeBounds; override;
    procedure DoPrepareCanvas(aCol,aRow:Integer; aState: TGridDrawState); override;
    procedure DoLoadColumn(sender: TCustomGrid; aColumn: TGridColumn; aColIndex: Integer;
                            aCfg: TXMLConfig; aVersion: Integer; aPath: string); override;
    procedure DoSaveColumn(sender: TCustomGrid; aColumn: TGridColumn; aColIndex: Integer;
                            aCfg: TXMLConfig; aVersion: Integer; aPath: string); override;
    procedure DrawAllRows; override;
    procedure DrawFocusRect(aCol,aRow:Integer; ARect:TRect); override;
    procedure DrawRow(ARow: Integer); override;
    procedure DrawCell(aCol,aRow: Integer; aRect: TRect; aState:TGridDrawState); override;
    procedure DrawCellBackground(aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure DrawCheckboxBitmaps(aCol: Integer; aRect: TRect; F: TField);
    procedure DrawFixedText(aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure DrawColumnText(aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState); override;
    procedure DrawIndicator(ACanvas: TCanvas; R: TRect; Opt: TDataSetState; MultiSel: boolean); virtual;
    procedure EditingColumn(aCol: Integer; Ok: boolean);
    procedure EditorCancelEditing;
    procedure EditorDoGetValue; override;
    function  EditorCanAcceptKey(const ch: TUTF8Char): boolean; override;
    function  EditorIsReadOnly: boolean; override;
    procedure EditorTextChanged(const aCol,aRow: Integer; const aText:string); override;
    procedure EndLayout;
    function  FieldIndexFromGridColumn(AGridCol: Integer): Integer;
    function  FirstGridColumn: Integer; override;
    function  GetBufferCount: integer; virtual;
    function  GetCellHintText(aCol, aRow: Integer): String; override;
    function  GetDefaultColumnAlignment(Column: Integer): TAlignment; override;
    function  GetDefaultColumnWidth(Column: Integer): Integer; override;
    function  GetDefaultColumnReadOnly(Column: Integer): boolean; override;
    function  GetDefaultColumnTitle(Column: Integer): string; override;
    function  GetDefaultRowHeight: integer; override;
    function  GetDsFieldFromGridColumn(Column: Integer): TField;
    function  GetEditMask(aCol, aRow: Longint): string; override;
    function  GetEditText(aCol, aRow: Longint): string; override;
    function  GetFieldFromGridColumn(Column: Integer): TField;
    function  GetGridColumnFromField(F: TField): Integer;
    procedure GetImageForCheckBox(const aCol, aRow: Integer;
      CheckBoxView: TCheckBoxState; var ImageList: TCustomImageList;
      var ImageIndex: TImageIndex; var Bitmap: TBitmap); override;
    function  GetIsCellSelected(aCol, aRow: Integer): boolean; override;
    function  GetIsCellTitle(aCol,aRow: Integer): boolean; override;
    procedure GetSelectedState(AState: TGridDrawState; out IsSelected:boolean); override;
    function  GetSmoothScroll(Which: Integer): Boolean; override;
    function  GetTruncCellHintText(aCol, aRow: Integer): string; override;
    function  GridCanModify: boolean;
    procedure GetSBVisibility(out HsbVisible,VsbVisible:boolean);override;
    procedure GetSBRanges(const HsbVisible,VsbVisible: boolean;
                  out HsbRange,VsbRange,HsbPage,VsbPage,HsbPos,VsbPos:Integer); override;
    procedure HeaderClick(IsColumn: Boolean; index: Integer); override;
    procedure HeaderSized(IsColumn: Boolean; Index: Integer); override;
    function  IsColumnVisible(aCol: Integer): boolean;
    function  IsValidChar(AField: TField; AChar: TUTF8Char): boolean;
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
    procedure LinkActive(Value: Boolean); virtual;
    procedure LayoutChanged; virtual;
    procedure Loaded; override;
    procedure LoadGridOptions(cfg: TXMLConfig; Version: Integer); override;
    procedure MoveSelection; override;
    function  MouseButtonAllowed(Button: TMouseButton): boolean; override;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure PrepareCanvas(aCol,aRow: Integer; aState:TGridDrawState); override;
    procedure PrepareCellHints(aCol,aRow: Integer); override;
    procedure RemoveAutomaticColumns;
    procedure ResetSizes; override;
    procedure SaveGridOptions(Cfg: TXMLConfig); override;
    procedure SelectEditor; override;
    procedure SetEditText(ACol, ARow: Longint; const Value: string); override;
    procedure SetFixedCols(const AValue: Integer); override;
    function  SelectCell(aCol, aRow: Integer): boolean; override;
    procedure UnprepareCellHints; override;
    procedure UpdateActive; virtual;
    procedure UpdateAutoSizeColumns;
    procedure UpdateData; virtual;
    function  UpdateGridCounts: Integer;
    procedure WMVScroll(var Message : TLMVScroll); message LM_VScroll;
    procedure WndProc(var TheMessage : TLMessage); override;

    property Columns: TDBGridColumns read GetColumns write SetColumns;
    property GridStatus: TDBGridStatus read FGridStatus write FGridStatus;
    property Datalink: TComponentDataLink read FDatalink;
    property Options: TDBGridOptions read FOptions write SetOptions default
              [dgColumnResize, dgColumnMove, dgTitles, dgIndicator, dgRowLines,
               dgColLines, dgConfirmDelete, dgCancelOnExit, dgTabs, dgEditing,
               dgAlwaysShowSelection];
    property OptionsExtra: TDBGridExtraOptions read FExtraOptions
              write SetExtraOptions default [dgeAutoColumns, dgeCheckboxColumn];
    property ReadOnly: Boolean read FReadOnly write FReadOnly default false;
    property SelectedRows: TBookmarkList read FSelectedRows;

    property OnCellClick: TDBGridClickEvent read FOnCellClick write FOnCellClick;
    property OnColEnter: TNotifyEvent read FOnColEnter write FOnColEnter;
    property OnColExit: TNotifyEvent read FOnColExit write FOnColExit;
    property OnColumnMoved: TMovedEvent read FOnColumnMoved write FOnColumnMoved;
    property OnColumnSized: TNotifyEvent read FOnColumnSized write FOnColumnSized;
    property OnDrawColumnCell: TDrawColumnCellEvent read FOnDrawColumnCell write FOnDrawColumnCell;
    property OnDrawColumnTitle: TDrawColumnCellEvent read FOnDrawColumnTitle write FOnDrawColumnTitle;
    property OnFieldEditMask: TGetDbEditMaskEvent read FOnFieldEditMask write FOnFieldEditMask;
    property OnGetCellHint: TDbGridCellHintEvent read FOnGetCellHint write FOnGetCellHint;
    property OnPrepareCanvas: TPrepareDbGridCanvasEvent read FOnPrepareCanvas write FOnPrepareCanvas;
    property OnSelectEditor: TDbGridSelEditorEvent read FOnSelectEditor write FOnSelectEditor;
    property OnTitleClick: TDBGridClickEvent read FOnTitleClick write FOnTitleClick;
    property OnUserCheckboxBitmap: TDbGridCheckboxBitmapEvent read FOnCheckboxBitmap write FOnCheckboxBitmap;
    property OnUserCheckboxState: TDbGridCheckboxStateEvent read FOnCheckboxState write FOnCheckboxState;
    property OnRowMoved: TMovedEvent read FOnRowMoved write SetRowMoved;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AutoAdjustColumns; override;
    procedure InitiateAction; override;
    procedure DefaultDrawColumnCell(const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
    function  EditorByStyle(Style: TColumnButtonStyle): TWinControl; override;
    procedure ResetColWidths;
    destructor Destroy; override;
    function MouseToRecordOffset(const x,y: Integer; out Column: TColumn; out RecordOffset: Integer): TGridZone;
    function ExecuteAction(AAction: TBasicAction): Boolean; override;
    function UpdateAction(AAction: TBasicAction): Boolean; override;

    procedure SaveToFile(FileName: string); override;
    procedure SaveToStream(AStream: TStream); override;
    procedure LoadFromFile(FileName: string); override;
    procedure LoadFromStream(AStream: TStream); override;

    property AllowOutboundEvents;
    property SelectedField: TField read GetCurrentField write SetCurrentField;
    property SelectedIndex: Integer read GetSelectedIndex write SetSelectedIndex;
    property SelectedColumn: TColumn read GetCurrentColumn;
    property SelectedFieldRect: TRect read GetSelectedFieldRect;
    property LastColumn: TColumn read GetLastColumn;
    property FirstColumn: TColumn read GetFirstColumn;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
  end;

  TDBGrid=class(TCustomDBGrid)
  public
    property BorderColor;
    property Canvas;
    property DefaultTextStyle;
    property EditorBorderStyle;
    property EditorMode;
    property ExtendedColSizing;
    property FastEditing;
    property FocusColor;
    property FocusRectVisible;
    property GridLineColor;
    property GridLineStyle;
    property InplaceEditor;
    property SelectedColor;
    property SelectedRows;
    property OnRowMoved;
  published
    property Align;
    property AlternateColor;
    property Anchors;
    property AutoAdvance default aaRightDown;
    property AutoEdit;
    property AutoFillColumns;
    property BiDiMode;
    property BorderSpacing;
    property BorderStyle;
    property CellHintPriority;
    property Color;
    property Columns; // stored false;
    property Constraints;
    property DataSource;
    property DefaultDrawing;
    property DefaultRowHeight;
    property DragCursor;
    //property DragKind;
    property DragMode;
    property Enabled;
    property FixedColor;
    property FixedCols;
    property FixedHotColor;
    property Flat;
    property Font;
    property HeaderHotZones;
    property HeaderPushZones;
    //property ImeMode;
    //property ImeName;
    property Options;
    property Options2;
    property OptionsExtra;
    property ParentBiDiMode;
    property ParentColor default false;
    property ParentFont;
    //property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property Scrollbars default ssBoth;
    property ShowHint;
    property TabAdvance;
    property TabOrder;
    property TabStop;
    property TitleFont;
    property TitleImageList;
    property TitleStyle;
    property UseXORFeatures;
    property Visible;
    property OnCellClick;
    property OnColEnter;
    property OnColExit;
    property OnColumnMoved;
    property OnColumnSized;
    property OnContextPopup;
    property OnDrawColumnCell;
    property OnDrawColumnTitle;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditButtonClick;
    property OnEditingDone;
    //property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnFieldEditMask;
    property OnGetCellHint;
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
    property OnPrepareCanvas;
    property OnSelectEditor;
    //property OnStartDock;
    property OnStartDrag;
    property OnTitleClick;
    property OnUserCheckboxBitmap;
    property OnUserCheckboxState;
    property OnUTF8KeyPress;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Data Controls',[TDBGrid]);
end;

function CalcCanvasCharWidth(Canvas:TCanvas): integer;
begin
  {$ifdef dbgDBGridExtra}
  DebugLnEnter('CalcCanvasCharWidth INIT');
  {$endif}
  if Canvas.HandleAllocated then
    result := Canvas.TextWidth('MX') div 2
  else
    result := 8;
  {$ifdef dbgDBGridExtra}
  DebugLnExit('CalcCanvasCharWidth DONE result=%d', [result]);
  {$endif}
end;

function CalcColumnFieldWidth(Canvas: TCanvas; hasTitle: boolean;
  aTitle: String; aTitleFont: TFont; Field: TField): Integer;
var
  aCharWidth, aTitleWidth: Integer;
  aFont: TFont;
  UseTitleFont: boolean;
begin
  {$ifdef dbgDBGridExtra}
  DebugLnEnter('CalcColumnFieldWidth INIT');
  {$endif}
  if (Field=nil) or (Field.DisplayWidth=0) then
    Result := DEFCOLWIDTH
  else begin

    aCharWidth := CalcCanvasCharWidth(Canvas);
    aTitleWidth := UTF8Length(aTitle);
    if Field.DisplayWidth > aTitleWidth then
      result := aCharWidth * Field.DisplayWidth
    else
      result := aCharWidth * aTitleWidth;

    if HasTitle then begin
      UseTitleFont :=
        (Canvas.Font.Size<>aTitleFont.Size) or
        (Canvas.Font.Style<>aTitleFont.Style) or
        (Canvas.Font.CharSet<>aTitleFont.CharSet) or
        (Canvas.Font.Name<>aTitleFont.Name);
      if UseTitleFont then begin
        aFont := TFont.Create;
        aFont.Assign(Canvas.Font);
        Canvas.Font := aTitleFont;
      end;
      try
        aCharWidth := Canvas.TextWidth(ATitle)+6;
        if aCharWidth>Result then
          Result := aCharWidth;
      finally
        if UseTitleFont then begin
          Canvas.Font := aFont;
          aFont.Free;
        end;
      end;
    end; // if HasTitle ...
  end; // if (Field=nil) or (Field.DisplayWidth=0)
  {$ifdef dbgDBGridExtra}
  DebugLnExit('CalcColumnFieldWidth DONE result=%d', [result]);
  {$endif}
end;

var
  LookupTmpSetActive: Boolean;
  LookupBookMark: TBookmark;

procedure LookupGetBookMark(ALookupField: TField);
begin
  {$ifdef dbgDBGrid}
  DebugLn('LookupGetBookMark');
  {$endif}
  LookupTmpSetActive := not ALookupField.LookupDataSet.Active;
  if LookupTmpSetActive then
    ALookupField.LookupDataSet.Active := True
  else
  begin
    LookupBookMark := ALookupField.LookupDataSet.GetBookmark;
    ALookupField.LookupDataSet.DisableControls;
  end;
end;

procedure LookupGotoBookMark(ALookupField: TField);
begin
  {$ifdef dbgDBGrid}
  DebugLn('LookupGotoBookMark');
  {$endif}
  if LookupTmpSetActive then
  begin
    ALookupField.LookupDataSet.Active := False;
    LookupTmpSetActive := False;
  end
  else
  try
    ALookupField.LookupDataSet.GotoBookmark(LookupBookMark);
    ALookupField.LookupDataSet.FreeBookmark(LookupBookMark);
  finally
    ALookupField.LookupDataSet.EnableControls;
  end;
end;

{ TBookmarkedRecordEnumerator }

constructor TBookmarkedRecordEnumerator.Create(bookList: TBookmarkList;
  aGrid: TCustomDbGrid; anOptions: TBookmarkedRecordEnumeratorOptions);
begin
  inherited Create;
  fBookmarkList := bookList;
  fBookmarkIndex := -1;
  fDataset := aGrid.Datasource.dataset;
  fOptions := anOptions;
end;

destructor TBookmarkedRecordEnumerator.Destroy;
begin
  if breRestoreCurrent in fOptions then begin
    if fDataset.BookmarkValid(fBook) then
      fDataset.GotoBookmark(fBook);
    fDataset.FreeBookmark(fBook);
  end;
  if breDisableDataset in fOptions then
    fDataset.EnableControls;
  inherited Destroy;
end;

function TBookmarkedRecordEnumerator.MoveNext: boolean;
begin
  inc(fBookmarkIndex);

  if fBookmarkIndex=0 then begin
    if breDisableDataset in fOptions then
      fDataset.DisableControls;
    if breRestoreCurrent in fOptions then
      fBook := fDataset.GetBookmark;
  end;

  result := fBookmarkIndex<fBookmarkList.Count;
  if result then begin
    fCurrent := fBookmarkList[fBookmarkIndex];
    if fDataset.BookmarkValid(fCurrent) then
      fDataSet.GotoBookmark(fCurrent)
    else if breStopOnInvalidBookmark in fOptions then
      result := false;
  end;
end;

function TBookmarkedRecordEnumerator.GetEnumerator: TBookmarkedRecordEnumerator;
begin
  result := self;
end;

{ TCustomDBGrid }

procedure TCustomDBGrid.OnRecordChanged(Field: TField);
var
  c: Integer;
begin
  {$ifdef dbgDBGrid}
  DbgOut(ClassName,'.OnRecordChanged(Field=');
  if Field=nil then DebugLn('nil)')
  else              DebugLn(Field.FieldName,')');
  {$endif}
  if Field=nil then
    UpdateActive
  else begin
    c := GetGridColumnFromField(Field);
    if c>0 then begin
      if EditorMode and (Field=SelectedField) then
        EditorDoSetValue
      else
        InvalidateCell(C, Row)
    end else
      UpdateActive;
  end;
end;

function TCustomDBGrid.GetDataSource: TDataSource;
begin
  {$ifdef dbgDBGrid}
  DebugLn('%s.GetDataSource', [ClassName]);
  {$endif}
  Result:= FDataLink.DataSource;
end;

function TCustomDBGrid.GetFirstColumn: TColumn;
var
  i: Integer;
begin
  i := ColumnIndexFromGridColumn(GetFirstVisibleColumn);
  if i>=0 then
    Result := Columns[i]
  else
    Result := nil;
end;

function TCustomDBGrid.GetLastColumn: TColumn;
var
  i: Integer;
begin
  i := ColumnIndexFromGridColumn(GetLastVisibleColumn);
  if i>=0 then
    Result := Columns[i]
  else
    Result := nil;
end;

function TCustomDBGrid.GetRecordCount: Integer;
begin
  {$ifdef dbgDBGrid}
  DebugLnEnter('%s.GetRecordCount INIT', [ClassName]);
  {$endif}
  result := FDataLink.DataSet.RecordCount;
  {$ifdef dbgDBGrid}
  DebugLnExit('%s.GetRecordCount DONE RecordCount=%d', [ClassName, result]);
  {$endif}
end;

function TCustomDBGrid.GetSelectedFieldRect: TRect;
begin
  result := CellRect(Col,Row);
end;

function TCustomDBGrid.GetSelectedIndex: Integer;
begin
  if Columns.Enabled then
    Result := ColumnIndexFromGridColumn( Col )
  else
    Result := FieldIndexFromGridColumn( Col );
end;

procedure TCustomDBGrid.EmptyGrid;
var
  OldFixedCols, OldFixedRows: Integer;
begin
  {$ifdef dbgDBGrid}
  DebugLn('%s.EmptyGrid', [ClassName]);
  {$endif}
  OldFixedCols := FixedCols;
  OldFixedRows := FixedRows;
  Clear;
  RowCount := OldFixedRows + 1;
  ColCount := OldFixedCols + 1;
  if dgIndicator in Options then
    ColWidths[0]:=Scale96ToFont(DEFINDICATORCOLWIDTH);
end;

procedure TCustomDBGrid.DoHeaderClick(Index: Integer);
var
  Column: TColumn;
begin
  if Assigned(OnTitleClick) then begin
    Column := TColumn(ColumnFromGridColumn(Index));
    if Column <> nil then
      OnTitleClick(Column);
  end;
end;

function TCustomDBGrid.GetColumns: TDBGridColumns;
begin
  result := TDBGridColumns( inherited Columns );
end;

procedure TCustomDBGrid.InvalidateSizes;
begin
  {$ifdef dbgDBGrid}
  DebugLn('%s.InvalidateSizes', [ClassName]);
  {$endif}
  GridFlags := GridFlags + [gfVisualChange];
end;

function TCustomDBGrid.GetCurrentColumn: TColumn;
begin
  if Columns.Enabled then
    Result := TColumn(Columns[SelectedIndex])
  else
    Result := nil;
end;

function TCustomDBGrid.GetCurrentField: TField;
begin
  result := GetFieldFromGridColumn( Col );
end;

procedure TCustomDBGrid.OnDataSetChanged(aDataSet: TDataSet);
begin
  {$ifdef dbgDBGrid}
  DebugLnEnter('%s.OnDataSetChanged INIT name=%s aDataSet=%s',
  	[ClassName,name,dbgsname(ADataset)]);
  {$endif}
  if not (gsStartEditing in FGridStatus) then begin
    GridFlags := GridFlags + [gfEditingDone];
    if EditorMode then
      EditorMode := False;
    GridFlags := GridFlags - [gfEditingDone];
    LayoutChanged;
  end;
  UpdateActive;
  if not (gsStartEditing in FGridStatus) then begin
    SelectEditor;
    if (dgAlwaysShowEditor in Options) and not EditorMode then
      EditorMode := true;
  end;
  {$ifdef dbgDBGrid}
  DebugLnExit('%s.OnDataSetChanged DONE name=%s aDataSet=%s',
  	[ClassName,name,dbgsname(ADataset)]);
  {$endif}
end;

procedure TCustomDBGrid.OnDataSetOpen(aDataSet: TDataSet);
begin
  {$ifdef dbgDBGrid}
  DebugLnEnter('%s.OnDataSetOpen INIT', [ClassName]);
  {$endif}
  RenewColWidths;
  LinkActive(True);
  UpdateActive;
  SelectEditor;
  {$ifdef dbgDBGrid}
  DebugLnExit('%s.OnDataSetOpen DONE', [ClassName]);
  {$endif}
end;

procedure TCustomDBGrid.OnDataSetClose(aDataSet: TDataSet);
begin
  {$ifdef dbgDBGrid}
  DebugLn('%s.OnDataSetClose', [ClassName]);
  {$endif}
  LinkActive(False);
end;

procedure TCustomDBGrid.OnEditingChanged(aDataSet: TDataSet);
begin
  {$ifdef dbgDBGrid}
  DebugLn('%s.OnEditingChanged', [ClassName]);
  if aDataSet<>nil then begin
    DebugLn(['Editing=', dsEdit = aDataSet.State]);
    DebugLn(['Inserting=',dsInsert = aDataSet.State]);
  end else
    DebugLn('Dataset=nil');
  {$endif}
  FDataLink.Modified := False;
  UpdateActive;
end;

procedure TCustomDBGrid.OnInvalidDataSet(aDataSet: TDataSet);
begin
  {$ifdef dbgDBGrid}
  DebugLn('%s.OnInvalidDataSet', [ClassName]);
  {$endif}
  LinkActive(False);
end;

procedure TCustomDBGrid.OnInvalidDataSource(aDataSet: TDataset);
begin
  {$ifdef dbgDBGrid}
  DebugLn('%s.OnInvalidDataSource', [ClassName]);
  {$endif}
  LinkActive(False);
end;

procedure TCustomDBGrid.OnFocusControl(aField: TFieldRef);
var
  aIndex: Integer;
begin
  if CanFocus and (aField<>nil) and (aField^<>nil) then begin
    aIndex := GetGridColumnFromField(aField^);
    if aIndex>=FirstGridColumn then begin
      SelectedField := aField^;
      aField^ := nil;
      SetFocus;
    end;
  end;
end;

procedure TCustomDBGrid.OnLayoutChanged(aDataSet: TDataSet);
begin
  {$ifdef dbgDBGrid}
  DebugLn('%s.OnLayoutChanged', [ClassName]);
  {$endif}
  LayoutChanged;
end;

procedure TCustomDBGrid.OnNewDataSet(aDataSet: TDataset);
begin
  {$ifdef dbgDBGrid}
  DebugLnEnter('%s.OnNewDataSet INIT', [ClassName]);
  {$endif}
  RenewColWidths;
  LinkActive(True);
  UpdateActive;
  SelectEditor;
  {$ifdef dbgDBGrid}
  DebugLnExit('%s.OnNewDataSet DONE', [ClassName]);
  {$endif}
end;

procedure TCustomDBGrid.OnDataSetScrolled(aDataSet: TDataSet; Distance: Integer
  );
var
  OldEditorMode: boolean;
  OldRow: Integer;
begin
  {$ifdef dbgDBGrid}
  DebugLn('%s.OnDataSetScrolled Distance=%d ds.RecordCount=%d',[ClassName, Distance, aDataSet.RecordCount]);
  {$endif}
  UpdateScrollBarRange;
  // todo: Use a fast interface method to scroll a rectangular section of window
  //       if distance=+, Row[Distance] to Row[RowCount-2] UP
  //       if distance=-, Row[FixedRows+1] to Row[RowCount+Distance] DOWN

  OldEditorMode := EditorMode;
  if OldEditorMode then
    EditorMode := False;

  if Distance<>0 then begin

    OldRow := Row;
    Row := FixedRows + FDataLink.ActiveRecord;
    if OldRow=Row then  // if OldRow<>NewRow SelectEditor will be called by MoveExtend
      SelectEditor;     // if OldRow=NewRow we need to manually call SelectEditor

    Invalidate;
  end else
    UpdateActive;

  if OldEditorMode and (dgAlwaysShowEditor in Options) then
    EditorMode := True;
end;

procedure TCustomDBGrid.OnUpdateData(aDataSet: TDataSet);
begin
  {$ifdef dbgDBGrid}
  DebugLn('%s.OnUpdateData', [ClassName]);
  {$endif}
  UpdateData;
end;

procedure TCustomDBGrid.SetColumns(const AValue: TDBGridColumns);
begin
  {$ifdef dbgDBGrid}
  DebugLn('%s.SetColumns', [ClassName]);
  {$endif}
  inherited Columns := TGridColumns(AValue);
end;

{
procedure TCustomDBGrid.ReadColumns(Reader: TReader);
begin
  Columns.Clear;
  Reader.ReadValue;
  Reader.ReadCollection(Columns);
end;
procedure TCustomDBGrid.SetColumns(const AValue: TDBGridColumns);
begin
  Columns.Assign(AValue);
end;
}
procedure TCustomDBGrid.SetCurrentField(const AValue: TField);
var
  i: Integer;
begin
  if Avalue<>SelectedField then begin
    i := GetGridColumnFromField( AValue );
    if (i>=FirstGridColumn) and (i>=FixedCols) then
      Col := i;
  end;
end;

procedure TCustomDBGrid.SetDataSource(const AValue: TDataSource);
begin
  {$ifdef dbgDBGrid}
  DebugLn('%s.SetDataSource', [ClassName]);
  {$endif}
  if AValue = FDatalink.Datasource then Exit;
  RenewColWidths;
  FDataLink.DataSource := AValue;
  UpdateActive;
end;

procedure TCustomDBGrid.SetExtraOptions(const AValue: TDBGridExtraOptions);
var
  OldOptions: TDBGridExtraOptions;
  function IsOptionChanged(Op: TDBGridExtraOption): boolean;
  begin
    result := ((op in OldOptions) and not (op in AValue)) or
      (not (op in OldOptions) and (op in AValue));
  end;
begin
  {$ifdef dbgDBGrid}
  DebugLn('%s.SetExtraOptions', [ClassName]);
  {$endif}
  if FExtraOptions=AValue then exit;
  OldOptions := FExtraOptions;
  FExtraOptions := AValue;

  if IsOptionChanged(dgeCheckboxColumn) then
    Invalidate;

  if IsOptionChanged(dgeAutoColumns) then begin
    if dgeAutoColumns in aValue then
      AddAutomaticColumns
    else if TDBGridColumns(Columns).HasAutomaticColumns then
      RemoveAutomaticColumns;
    UpdateActive;
  end;

end;

procedure TCustomDBGrid.SetOptions(const AValue: TDBGridOptions);
var
  OldOptions: TGridOptions;
  ChangedOptions: TDbGridOptions;
  MultiSel: boolean;
begin
  {$ifdef dbgDBGrid}
  DebugLnEnter('%s.SetOptions INIT', [ClassName]);
  {$endif}
  if FOptions<>AValue then begin
    MultiSel := dgMultiSelect in FOptions;
    ChangedOptions := (FOptions-AValue) + (AValue-FOptions);
    FOptions:=AValue;
    OldOptions := inherited Options;

   if dgRowSelect in FOptions then
    FOptions := FOptions - [dgEditing, dgAlwaysShowEditor, dgRowHighlight];

    BeginLayout;

    if dgRowLines in fOptions then
      Include(OldOptions, goHorzLine)
    else
      Exclude(OldOptions, goHorzLine);

    if dgColLines in fOptions then
      Include(OldOptions, goVertLine)
    else
      Exclude(OldOptions, goVertLine);

    if dgColumnResize in fOptions then
      Include(OldOptions, goColSizing)
    else
      Exclude(OldOptions, goColSizing);

    if dgColumnMove in fOptions then
      Include(OldOptions, goColMoving)
    else
      Exclude(OldOptions, goColMoving);

    if dgAlwaysShowEditor in FOptions then
      Include(OldOptions, goAlwaysShowEditor)
    else
      Exclude(OldOptions, goAlwaysShowEditor);

    if dgRowSelect in FOptions then
      Include(OldOptions, goRowSelect)
    else
      Exclude(OldOptions, goRowSelect);

    if dgEditing in FOptions then
      Include(OldOptions, goEditing)
    else
      Exclude(OldOptions, goediting);

    if dgTabs in FOptions then
      Include(OldOptions, goTabs)
    else
      Exclude(OldOptions, goTabs);

    if dgHeaderHotTracking in FOptions then
      Include(OldOptions, goHeaderHotTracking)
    else
      Exclude(OldOptions, goHeaderHotTracking);

    if dgHeaderPushedLook in FOptions then
      Include(OldOptions, goHeaderPushedLook)
    else
      Exclude(OldOptions, goHeaderPushedLook);

    if dgCellHints in FOptions then
      Include(OldOptions, goCellHints)
    else
      Exclude(OldOptions, goCellHints);

    if dgTruncCellHints in FOptions then
      Include(OldOptions, goTruncCellHints)
    else
      Exclude(OldOptions, goTruncCellHints);

    if dgCellEllipsis in FOptions then
      Include(OldOptions, goCellEllipsis)
    else
      Exclude(OldOptions, goCellEllipsis);

    if dgRowHighlight in FOptions then
      Include(OldOptions, goRowHighlight)
    else
      Exclude(OldOptions, goRowHighlight);

    if dgDblClickAutoSize in FOptions then
      Include(OldOptions, goDblClickAutoSize)
    else
      Exclude(OldOptions, goDblClickAutoSize);

    if (dgIndicator in ChangedOptions) then begin
      if (dgIndicator in FOptions) then
        FixedCols := FixedCols + 1
      else
        FixedCols := Max(FixedCols - 1, 0);
    end;

    if (dgTitles in ChangedOptions) then begin
      if dgTitles in FOptions then
        FixedRows := FixedRows + 1
      else
        FixedRows := Max(FixedRows - 1, 0);
    end;

    if (dgAutoSizeColumns in ChangedOptions) then begin
      Exclude(FGridStatus, gsAutoSized);
    end;

    if dgThumbTracking in ChangedOptions then begin
      if dgThumbTracking in FOptions then
        Include(OldOptions, goThumbTracking)
      else
        Exclude(OldOptions, goThumbTracking);
    end;

    inherited Options := OldOptions;

    if MultiSel and not (dgMultiSelect in FOptions) then begin
      FSelectedRows.Clear;
      if FKeyBookmark<>nil then begin
        FDatalink.DataSet.FreeBookmark(FKeyBookmark);
        FKeyBookmark:=nil;
      end;
    end;

    EndLayout;
  end;
  {$ifdef dbgDBGrid}
  DebugLnExit('%s.SetOptions DONE', [ClassName]);
  {$endif}
end;

procedure TCustomDBGrid.SetRowMoved(AValue: TMovedEvent);
begin
  if FOnRowMoved = AValue then
    Exit;
  FOnRowMoved := AValue;
  if assigned(OnRowMoved) then
    inherited Options := inherited Options + [goRowMoving]
  else
    inherited Options := inherited Options - [goRowMoving];
end;

procedure TCustomDBGrid.SetSelectedIndex(const AValue: Integer);
begin
  Col := FirstGridColumn + AValue;
end;

procedure TCustomDBGrid.UpdateBufferCount;
var
  BCount: Integer;
begin
  {$ifdef dbgDBGrid}
  DebugLnEnter('%s.UpdateBufferCount INIT', [ClassName]);
  {$endif}
  if FDataLink.Active then begin
    BCount := GetBufferCount;
    if BCount<1 then
      BCount := 1;
    FDataLink.BufferCount:= BCount;
  end;
  {$ifdef dbgDBGrid}
  DebugLnExit('%s.UpdateBufferCount DONE BufferCount=%d', [ClassName, FDataLink.BufferCount]);
  {$endif}
end;

procedure TCustomDBGrid.UpdateData;
var
  selField,edField: TField;
  LookupKeyValues: Variant;
begin
  // get Editor text and update field content
  if not UpdatingData and (FEditingColumn>-1) and FDatalink.Editing then begin
    SelField := SelectedField;
    edField := GetFieldFromGridColumn(FEditingColumn);

    if (edField<>nil) and (edField = SelField) then begin
      {$ifdef dbgDBGrid}
      DebugLnEnter('%s.UpdateData INIT Field[%s(%s)]=%s',
                   [ClassName, edField.Fieldname ,edField.AsString, FTempText]);
      {$endif}

      StartUpdating;
      try
        edField.Text := FTempText;
        if edField.FieldKind = fkLookup then
        begin
          LookupKeyValues := Null;
          if edField.LookupCache then
            LookupKeyValues := edField.LookupList.FirstKeyByValue(FTempText)
          else
          begin
            LookupGetBookMark(edField);
            try
              if edField.LookupDataSet.Locate(edField.LookupResultField,
                VarArrayOf([FTempText]), []) then
                  LookupKeyValues :=
                    edField.LookupDataSet.FieldValues[edField.LookupKeyFields];
            finally
              LookupGotoBookMark(edField);
            end;
          end;
          edField.DataSet.FieldValues[edField.KeyFields] := LookupKeyValues;
        end;
      finally
        EndUpdating;
      end;
      EditingColumn(FEditingColumn, False);
      {$ifdef dbgDBGrid}
      DebugLnExit('%s.UpdateData DONE Field=%s',[ClassName, edField.ASString]);
      {$endif}
    end;

  end;
end;

{$ifdef dbgDBGrid}
function SBCodeToStr(Code: Integer): String;
begin
  Case Code of
    SB_LINEUP : result := 'SB_LINEUP';
    SB_LINEDOWN: result := 'SB_LINEDOWN';
    SB_PAGEUP: result := 'SB_PAGEUP';
    SB_PAGEDOWN: result := 'SB_PAGEDOWN';
    SB_THUMBTRACK: result := 'SB_THUMBTRACK';
    SB_THUMBPOSITION: result := 'SB_THUMBPOSITION';
    SB_ENDSCROLL: result := 'SB_SCROLLEND';
    SB_TOP: result := 'SB_TOP';
    SB_BOTTOM: result := 'SB_BOTTOM';
    else result :=IntToStr(Code)+ ' -> ?';
  end;
end;
{$endif}

procedure TCustomDBGrid.WMVScroll(var Message: TLMVScroll);
var
  IsSeq: boolean;
  aPos, aRange, aPage: Integer;
  DeltaRec: integer;

  function MaxPos: Integer;
  begin
    if IsSeq then
      result := GetRecordCount - 1
    else
      result := 4;
  end;

  procedure DsMoveBy(Delta: Integer);
  begin
    FDataLink.MoveBy(Delta);
    GetScrollbarParams(aRange, aPage, aPos);
  end;

  procedure DsGoto(BOF: boolean);
  begin
    if BOF then FDatalink.DataSet.First
    else        FDataLink.DataSet.Last;
    GetScrollbarParams(aRange, aPage, aPos);
  end;

  function DsPos: boolean;
  var
    oldMaxPos: Integer;
  begin
    result := false;
    aPos := Message.Pos;
    if aPos=FOldPosition then begin
      result := true;
      exit;
    end;
    oldMaxPos := MaxPos;
    if aPos>=oldMaxPos then
      dsGoto(False)
    else if aPos<=0 then
      dsGoto(True)
    else if IsSeq then begin
      FDatalink.DataSet.RecNo := aPos + 1;
      {$IFDEF MSWINDOWS}
      // Workaround for scrollbar range not being updated
      // probably only needed under windows, issue 33799
      if oldMaxPos<>MaxPos then begin
        ScrollBarShow(SB_VERT, false);
        ScrollBarShow(SB_VERT, true);
      end;
      {$ENDIF}
    end
    else begin
      DeltaRec := Message.Pos - FOldPosition;
      if DeltaRec=0 then begin
        result := true;
        exit
      end
      else if DeltaRec<-1 then
        DsMoveBy(-VisibleRowCount)
      else if DeltaRec>1 then
        DsMoveBy(VisibleRowCount)
      else
        DsMoveBy(DeltaRec);
    end;
  end;

begin
  if not FDatalink.Active then exit;

  {$ifdef dbgDBGrid}
  DebugLnEnter('%s.WMVScroll INIT Code=%s Position=%s OldPos=%s',
  			[ClassName, SbCodeToStr(Message.ScrollCode), dbgs(Message.Pos), Dbgs(FOldPosition)]);
  {$endif}

  aPos := 0;
  IsSeq := FDatalink.DataSet.IsSequenced and not FDataLink.DataSet.Filtered;
  case Message.ScrollCode of
    SB_TOP:
      DsGoto(True);
    SB_BOTTOM:
      DsGoto(False);
    SB_PAGEUP:
      DsMoveBy(-VisibleRowCount);
    SB_LINEUP:
      DsMoveBy(-1);
    SB_LINEDOWN:
      DsMoveBy(1);
    SB_PAGEDOWN:
      DsMoveBy(VisibleRowCount);
    SB_THUMBPOSITION:
      if DsPos then
        exit;
    SB_THUMBTRACK:
      if dgThumbTracking in Options then begin
        if not (FDatalink.DataSet.IsSequenced) or DsPos then begin
          {$ifdef dbgDBGrid}
          DebugLnExit('%s.WMVScroll EXIT: SB_THUMBTRACK: DsPos or not sequenced', [ClassName]);
          {$endif}
          exit;
        end;
      end else begin
        {$ifdef dbgDBGrid}
        DebugLnExit('%s.WMVScroll EXIT: SB_THUMBTRACK: not using dgThumbTracking', [ClassName]);
        {$endif}
        Exit;
      end;
    else begin
      {$ifdef dbgDBGrid}
      DebugLnExit('%s.WMVScroll EXIT: invalid ScrollCode: %d', [ClassName, message.ScrollCode]);
      {$endif}
      Exit;
    end;
  end;

  ScrollBarPosition(SB_VERT, aPos);
  FOldPosition:=aPos;

  if EditorMode then
    RestoreEditor;
  {$ifdef dbgDBGrid}
  DebugLnExit('%s.WMVScroll DONE Diff=%s FinalPos=%s', [ClassName, dbgs(DeltaRec), dbgs(aPos)]);
  {$endif}
end;

procedure TCustomDBGrid.WndProc(var TheMessage: TLMessage);
begin
  if (TheMessage.Msg=LM_SETFOCUS) and (gsUpdatingData in FGridStatus) then begin
    {$ifdef dbgGrid}DebugLn('%s.LM_SETFOCUS while updating', [ClassName]);{$endif}
    if EditorMode then begin
      LCLIntf.SetFocus(Editor.Handle);
      EditorSelectAll;
    end;
    exit;
  end;
  inherited WndProc(TheMessage);
end;


function TCustomDBGrid.DefaultFieldColWidth(F: TField): Integer;
begin
  if not HandleAllocated or (F=nil) then
    result:=DefaultColWidth
  else begin
    if F.DisplayWidth = 0 then
      if Canvas.HandleAllocated then
        result := Canvas.TextWidth( F.DisplayName ) + 3
      else
        Result := DefaultColWidth
    else
      result := F.DisplayWidth * CalcCanvasCharWidth(Canvas);
  end;
end;

function TCustomDBGrid.GetColumnCount: Integer;
var
  i: integer;
  F: TField;
begin
  result := 0;
  if Columns.Enabled then
    result := Columns.VisibleCount
  else
    if (dgeAutoColumns in OptionsExtra) and FDataLink.Active then
      for i:=0 to FDataLink.DataSet.FieldCount-1 do begin
        F:= FDataLink.DataSet.Fields[i];
        if (F<>nil) and F.Visible then
          Inc(Result);
      end;
end;

// Get the visible field (from dataset fields) that corresponds to given column
function TCustomDBGrid.GetDsFieldFromGridColumn(Column: Integer): TField;
var
  i: Integer;
begin
  i := FieldIndexFromGridColumn( Column );
  if i>=0 then
    Result := FDataLink.DataSet.Fields[i]
  else
    Result := nil;
end;

function TCustomDBGrid.FirstGridColumn: Integer;
begin
  if (dgIndicator in Options) then
    Result := 1
  else
    Result := 0;
end;

procedure TCustomDBGrid.PrepareCellHints(aCol, aRow: Integer);
begin
  if not DataLink.Active then Exit;
  FSavedRecord := DataLink.ActiveRecord;
  DataLink.ActiveRecord := ARow - FixedRows;
end;

procedure TCustomDBGrid.UnprepareCellHints;
begin
  if not DataLink.Active then Exit;
  DataLink.ActiveRecord := FSavedRecord;
end;

function TCustomDBGrid.GetCellHintText(aCol, aRow: Integer): String;
var
  C: TColumn;
begin
  Result := '';
  if (ARow < FixedRows) then
    exit;
  if Assigned(FOnGetCellHint) then begin
    C := ColumnFromGridColumn(ACol) as TColumn;
    FOnGetCellHint(self, C, Result);
  end;
end;

function TCustomDBGrid.GetTruncCellHintText(aCol, aRow: Integer): string;
var
  F: TField;
  C: TColumn;
begin
  Result := '';
  if ARow < FixedRows then
    exit;
  F := GetFieldFromGridColumn(ACol);
  if (F <> nil) then
    if CheckDisplayMemo(f) then
      result := F.AsString
    else
    if (F.DataType <> ftBlob) then
      Result := F.DisplayText
    else
      Result := '(blob)';
  if Assigned(OnGetCellHint) then begin
    C := ColumnFromGridColumn(ACol) as TColumn;
    FOnGetCellHint(self, C, Result);
  end;
end;

// obtain the field either from a Db column or directly from dataset fields
function TCustomDBGrid.GetFieldFromGridColumn(Column: Integer): TField;
var
  i: integer;
begin
  if Columns.Enabled then begin
    i := ColumnIndexFromGridColumn( Column );
    if i>=0 then
      result := TDBGridColumns(Columns)[i].FField
    else
      result := nil;
  end else
    result := GetDsFieldFromGridColumn(Column);
end;

// obtain the corresponding grid column for the given field
function TCustomDBGrid.GetGridColumnFromField(F: TField): Integer;
var
  i: Integer;
begin
  result := -1;
  for i:=FirstGridColumn to ColCount-1 do begin
    if GetFieldFromGridColumn(i) = F then begin
      result := i;
      break;
    end;
  end;
end;

procedure TCustomDBGrid.GetImageForCheckBox(const aCol, aRow: Integer;
  CheckBoxView: TCheckBoxState; var ImageList: TCustomImageList;
  var ImageIndex: TImageIndex; var Bitmap: TBitmap);
begin
  inherited GetImageForCheckBox(aCol, aRow, CheckBoxView, ImageList, ImageIndex, Bitmap);
  if Assigned(OnUserCheckboxBitmap) then
    OnUserCheckboxBitmap(Self, CheckBoxView, Bitmap);
end;

// obtain the visible field index corresponding to the grid column index
function TCustomDBGrid.FieldIndexFromGridColumn(AGridCol: Integer): Integer;
var
  i: Integer;
  Column: TColumn;
begin
  result := -1;
  if not FDatalink.Active then
    exit;

  if Columns.Enabled then begin
    Column := TColumn(ColumnFromGridColumn(AGridCol));
    if (Column<>nil) and (Column.Field<>nil) and Column.Field.Visible then
      Result := FDatalink.Dataset.Fields.IndexOf(Column.Field)
  end else begin
    AGridCol := AGridCol - FirstGridColumn;
    i := 0;
    while (AGridCol>=0) and (i<FDatalink.DataSet.FieldCount) do begin
      if FDatalink.Fields[i].Visible then begin
        Dec(AGridCol);
        if AGridCol<0 then begin
          Result := i;
          break;
        end;
      end;
      inc(i);
    end;
  end;
end;

function TCustomDBGrid.GetBufferCount: integer;
begin
  {$ifdef dbgDBGrid}
  DebugLn('%s.GetBufferCount', [ClassName]);
  {$endif}
  Result := ClientHeight div DefaultRowHeight;
  if dgTitles in Options then
    Dec(Result, 1);
end;

procedure TCustomDBGrid.UpdateGridColumnSizes;
var
  i: Integer;
begin
  {$ifdef dbgDBGrid}
  DebugLn('%s.UpdateGridColumnSizes', [ClassName]);
  {$endif}
  if FDefaultColWidths then begin
    if dgIndicator in Options then
      ColWidths[0]:=Scale96ToFont(12);
    if NeedAutoSizeColumns then
      UpdateAutoSizeColumns;
  end;
end;

procedure TCustomDBGrid.UpdateScrollbarRange;
var
  aRange, aPage, aPos: Integer;
  ScrollInfo: TScrollInfo;
begin
  if not HandleAllocated then exit;

  {$ifdef dbgDBGrid}
  DebugLnEnter('%s.UpdateScrollbarRange INIT', [ClassName]);
  {$endif}

  GetScrollBarParams(aRange, aPage, aPos);

  if (ScrollBars in [ssBoth, ssVertical])
  or ((Scrollbars in [ssAutoVertical, ssAutoBoth]) and (aRange>aPage)) then
  begin
    FillChar(ScrollInfo, SizeOf(ScrollInfo), 0);
    ScrollInfo.cbSize := SizeOf(ScrollInfo);

    {TODO: try to move this out}
    {$ifdef WINDOWS}
    ScrollInfo.fMask := SIF_ALL or SIF_DISABLENOSCROLL;
    ScrollInfo.ntrackPos := 0;
    {$else}
    ScrollInfo.fMask := SIF_ALL or SIF_UPDATEPOLICY;
    //ScrollInfo.ntrackPos := SB_POLICY_CONTINUOUS;
    ScrollInfo.ntrackPos := SB_POLICY_DISCONTINUOUS;
    {$endif}
    ScrollInfo.nMin := 0;
    ScrollInfo.nMax := aRange;
    ScrollInfo.nPos := Min(aPos,aRange-aPage);
    ScrollInfo.nPage := aPage;
    SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
  end;

  FOldPosition := aPos;
  {$ifdef dbgDBGrid}
  DebugLnExit('%s.UpdateScrollBarRange DONE Handle=%d aRange=%d aPage=%d aPos=%d',
    [ClassName, Handle, aRange, aPage, aPos]);
  {$endif}
end;

procedure TCustomDBGrid.DoLayoutChanged;
begin
  if csDestroying in ComponentState then
    exit;
  {$ifdef dbgDBGrid}DebugLnEnter('%s.doLayoutChanged INIT', [ClassName]);{$endif}
  BeginUpdate;
  if UpdateGridCounts=0 then
    EmptyGrid;
  EndUpdate;
  UpdateScrollbarRange;
  {$ifdef dbgDBGrid}DebugLnExit('%s.doLayoutChanged DONE', [ClassName]);{$endif}
end;
{
procedure TCustomDBGrid.WriteColumns(Writer: TWriter);
begin
  if Columns.IsDefault then
    Writer.WriteCollection(nil)
  else
    Writer.WriteCollection(Columns);
end;
}
procedure TCustomDBGrid.RestoreEditor;
begin
  if EditorMode then begin
    EditorMode := False;
    EditorMode := True;
  end;
end;

function TCustomDBGrid.ISEOF: boolean;
begin
  {$ifdef dbgDBGrid}
  DebugLn('%s.IsEOF', [ClassName]);
  {$endif}
  Result := FDatalink.Active and FDatalink.DataSet.EOF;
end;

function TCustomDBGrid.ValidDataSet: boolean;
begin
  {$ifdef dbgDBGrid}
  DebugLn('%s.ValidDataSet', [ClassName]);
  {$endif}
  Result := FDatalink.Active And (FDatalink.DataSet<>nil)
end;

function TCustomDBGrid.InsertCancelable: boolean;
begin
  with FDatalink.DataSet do
    Result := (State=dsInsert) and not (Modified or FDataLink.FModified);
end;

procedure TCustomDBGrid.StartUpdating;
begin
  if not UpdatingData then begin
    {$ifdef dbgDBGrid}DebugLn('%s.StartUpdating', [ClassName]);{$endif}
    Include(FGridStatus, gsUpdatingData);
    FOldControlStyle := ControlStyle;
    ControlStyle := ControlStyle + [csActionClient];
    LockEditor;
  end
  else
    {$ifdef dbgDBGrid}DebugLn('WARNING: multiple calls to StartUpdating');{$endif}
end;

procedure TCustomDBGrid.EndUpdating;
begin
  {$ifdef dbgDBGrid}DebugLn('%s.EndUpdating', [ClassName]);{$endif}
  Exclude(FGridStatus, gsUpdatingData);
  ControlStyle := FOldControlStyle;
  UnLockEditor;
  if csActionClient in ControlStyle then
    DebugLn('WARNING: still got csActionClient');
end;

function TCustomDBGrid.UpdatingData: boolean;
begin
  result := gsUpdatingData in FGridStatus;
end;

procedure TCustomDBGrid.AddAutomaticColumns;
var
  i: Integer;
  F: TField;
begin
  // add as many columns as there are fields in the dataset
  // do this only at runtime.
  if (csDesigning in ComponentState) or not FDatalink.Active or
    (gsRemovingAutoColumns in FGridStatus) or  (gsLoadingGrid in FGridStatus) or
    not (dgeAutoColumns in OptionsExtra)
  then
    exit;
  Include(FGridStatus, gsAddingAutoColumns);
  try
    for i:=0 to FDataLink.DataSet.FieldCount-1 do begin

      F:= FDataLink.DataSet.Fields[i];

      if TDBGridColumns(Columns).ColumnFromField(F) <> nil then
        // this field is already in the collection. This could only happen
        // if AddAutomaticColumns was called out of LayoutChanged.
        // to avoid duplicate columns skip this field.
        continue;

      if (F<>nil) then begin
        with TDBGridColumns(Columns).Add do begin
          FIsAutomaticColumn := True;
          Field := F;
          Visible := F.Visible;
        end;
      end;

    end;
    // honor the field.index
    TDBGridColumns(Columns).ResetColumnsOrder(coFieldIndexOrder);
  finally
    Exclude(FGridStatus, gsAddingAutoColumns);
  end;
end;

procedure TCustomDBGrid.AssignTo(Dest: TPersistent);
begin
  if Dest is TCustomDbGrid then begin
    // TODO
  end else
    inherited AssignTo(Dest);
end;

procedure TCustomDBGrid.AutoAdjustColumn(aCol: Integer);
var
  DatalinkActive: Boolean;
  CurActiveRecord: Integer;
  tmpCanvas: TCanvas;
begin
  BeginLayout;

  DatalinkActive := FDatalink.Active;
  if DatalinkActive then
    CurActiveRecord := FDatalink.ActiveRecord;

  tmpCanvas := GetWorkingCanvas(Canvas);
  try

    InternalAutoSizeColumn(aCol,tmpCanvas,DatalinkActive);

  finally
    if TmpCanvas<>Canvas then
      FreeWorkingCanvas(tmpCanvas);

    if DatalinkActive then
      FDatalink.ActiveRecord := CurActiveRecord;

    EndLayout;
  end;
end;

procedure TCustomDBGrid.UpdateAutoSizeColumns;
var
  ACol: Integer;
  DatalinkActive: boolean;
  CurActiveRecord: Integer;
  tmpCanvas: TCanvas;
begin
  if gsAutoSized in GridStatus then
    exit;

  BeginLayout;

  DatalinkActive := FDatalink.Active;
  if DatalinkActive then
    CurActiveRecord := FDatalink.ActiveRecord;

  tmpCanvas := GetWorkingCanvas(Canvas);
  try

    for aCol:=FixedCols to ColCount-1 do
      InternalAutoSizeColumn(ACol,tmpCanvas,DatalinkActive);

  finally
    if TmpCanvas<>Canvas then
      FreeWorkingCanvas(tmpCanvas);

    if DatalinkActive then
      FDatalink.ActiveRecord := CurActiveRecord;

    include(FGridStatus, gsAutoSized);

    EndLayout;
  end;

end;

procedure TCustomDBGrid.SwapCheckBox;
var
  SelField: TField;
  TempColumn: TColumn;
begin
  if not GridCanModify then
    exit;

  SelField := SelectedField;
  TempColumn := TColumn(ColumnFromGridColumn(Col));
  if (SelField<>nil) and (TempColumn<>nil) and not TempColumn.ReadOnly and
     FDatalink.Edit then
  begin
    if SelField.DataType=ftBoolean then
      SelField.AsBoolean := not SelField.AsBoolean
    else
    begin
      if TempColumn.ValueChecked=SelField.AsString then
        SelField.AsString := TempColumn.ValueUnchecked
      else
        SelField.AsString := TempColumn.ValueChecked;
    end;
  end;
end;

procedure TCustomDBGrid.ToggleSelectedRow;
begin
  SelectRecord(not FSelectedRows.CurrentRowSelected);
end;

procedure TCustomDBGrid.LinkActive(Value: Boolean);
begin
  {$ifdef dbgDBGrid}
  DebugLn('%s.LinkActive', [ClassName]);
  {$endif}
  if not Value then begin
    FSelectedRows.Clear;
    if FKeyBookmark<>nil then begin
      FDatalink.DataSet.FreeBookmark(FKeyBookmark);
      FKeyBookmark:=nil;
    end;
    RemoveAutomaticColumns;
  end;
  LayoutChanged;
end;

procedure TCustomDBGrid.LayoutChanged;
begin
  {$ifdef dbgDBGrid}
  DebugLn('%s.LayoutChanged', [ClassName]);
  {$endif}
  if csDestroying in ComponentState then
    exit;
  if FLayoutChangedCount=0 then begin
    BeginLayout;
    if Columns.Count>0 then
      TDBGridColumns(Columns).LinkFields
    else if not FDataLink.Active then
      FDataLink.BufferCount := 0
    else
      AddAutomaticColumns;
    EndLayout;
  end;
end;

procedure TCustomDBGrid.Loaded;
begin
  {$ifdef dbgDBGrid}
  DebugLn('%s.Loaded', [ClassName]);
  {$endif}
  LayoutChanged;
  inherited Loaded;
end;

procedure TCustomDBGrid.LoadGridOptions(cfg: TXMLConfig; Version: Integer);
var
  Opt: TDBGridOptions;
  Path: string;
  procedure GetValue(optStr:string; aOpt:TDBGridOption);
  begin
    if Cfg.GetValue(Path+OptStr+'/value', False) then Opt:=Opt+[aOpt];
  end;
begin
  Opt:=[];
  Path:='grid/design/options/';
  GetValue('dgEditing', dgEditing);
  GetValue('dgTitles', dgTitles);
  GetValue('dgIndicator', dgIndicator);
  GetValue('dgColumnResize', dgColumnResize);
  GetValue('dgColumnMove', dgColumnMove);
  GetValue('dgColLines', dgColLines);
  GetValue('dgRowLines', dgRowLines);
  GetValue('dgTabs', dgTabs);
  GetValue('dgAlwaysShowEditor', dgAlwaysShowEditor);
  GetValue('dgRowSelect', dgRowSelect);
  GetValue('dgAlwaysShowSelection', dgAlwaysShowSelection);
  GetValue('dgConfirmDelete', dgConfirmDelete);
  GetValue('dgCancelOnExit', dgCancelOnExit);
  GetValue('dgMultiselect', dgMultiselect);
  GetValue('dgHeaderHotTracking', dgHeaderHotTracking);
  GetValue('dgHeaderPushedLook', dgHeaderPushedLook);
  GetValue('dgPersistentMultiSelect', dgPersistentMultiSelect);
  GetValue('dgAutoSizeColumns', dgAutoSizeColumns);
  GetValue('dgAnyButtonCanSelect', dgAnyButtonCanSelect);
  GetValue('dgDisableDelete', dgDisableDelete);
  GetValue('dgDisableInsert', dgDisableInsert);
  GetValue('dgCellHints', dgCellHints);
  GetValue('dgTruncCellHints', dgTruncCellHints);
  GetValue('dgCellEllipsis', dgCellEllipsis);
  GetValue('dgRowHighlight', dgRowHighlight);
  GetValue('dgThumbTracking', dgThumbTracking);
  Options:=Opt;
end;

type
  TProtFields=class(TFields)
  end;

procedure TCustomDBGrid.ColRowMoved(IsColumn: Boolean; FromIndex,
  ToIndex: Integer);
var
  F: TField;
begin
  if IsColumn then begin
    if Columns.Enabled then
      inherited ColRowMoved(IsColumn, FromIndex, ToIndex)
    else if FDatalink.Active and (FDataLink.DataSet<>nil) then begin
      F := GetDsFieldFromGridColumn(FromIndex);
      if F<>nil then begin
        TProtFields(FDatalink.DataSet.Fields).SetFieldIndex( F, ToIndex - FirstGridColumn );
      end;
    end;
    if Assigned(OnColumnMoved) then
      OnColumnMoved(Self, FromIndex, ToIndex);
  end
  else if Assigned(OnRowMoved) then
    OnRowMoved(Self, FromIndex, ToIndex);
end;

function TCustomDBGrid.ColumnEditorStyle(aCol: Integer; F: TField): TColumnButtonStyle;
var
  gridcol: TGridColumn;
begin
  result := cbsAuto;
  gridcol := ColumnFromGridColumn(aCol);
  if Columns.Enabled and assigned(gridcol) then
    result := gridcol.ButtonStyle;

  result := DefaultEditorStyle(result, F);
end;

function TCustomDBGrid.CreateColumns: TGridColumns;
begin
  {$ifdef dbgDBGrid}
  DebugLn('%s.CreateColumns', [ClassName]);
  {$endif}
  result := TDBGridColumns.Create(Self, TColumn);
end;

procedure TCustomDBGrid.CreateWnd;
begin
  {$ifdef dbgDBGrid}
  DebugLn('%s.CreateWnd', [ClassName]);
  {$endif}
  inherited CreateWnd;
  LayoutChanged;
  if Scrollbars in [ssBoth, ssVertical, ssAutoBoth, ssAutoVertical] then
    ScrollBarShow(SB_VERT, True);
end;

procedure TCustomDBGrid.DefineProperties(Filer: TFiler);
  {
  function HasColumns: boolean;
  var
    C: TGridColumns;
  begin
    if Filer.Ancestor <> nil then
      C := TCustomGrid(Filer.Ancestor).Columns
    else
      C := Columns;
    if C<>nil then
      result := not C.IsDefault
    else
      result := false;
  end;
  }
begin
  // simply avoid to call TCustomGrid.DefineProperties method
  // which defines ColWidths,Rowheights,Cells
  //Filer.DefineProperty('Columns',  @ReadColumns,  @WriteColumns,  HasColumns);
end;

procedure TCustomDBGrid.DefaultDrawCell(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState);

var
  S: string;
  F: TField;
  cbs: TColumnButtonStyle;
begin

  DrawCellBackground(aCol, aRow, aRect, aState);

  if gdFixed in aState then
    DrawFixedText(aCol, aRow, aRect, aState)
  else
  if not FDrawingEmptyDataset then begin

    F := GetFieldFromGridColumn(aCol);
    cbs := ColumnEditorStyle(aCol, F);
    case cbs of
      cbsCheckBoxColumn:
        DrawCheckBoxBitmaps(aCol, aRect, F);
      else
      begin

        if cbs=cbsButtonColumn then
          DrawButtonCell(aCol, aRow, aRect, aState);

        {$ifdef dbggridpaint}
        DbgOut(' Col=%d',[ACol]);
        {$endif}
        if F<>nil then begin
          {$ifdef dbgGridPaint}
          DbgOut(' Field=%s',[F.FieldName]);
          {$endif}
          if CheckDisplayMemo(F) then
            S := F.AsString
          else
            S := F.DisplayText;
        end else
          S := '';
        {$ifdef dbggridpaint}
        DbgOut(' Value=%s ',[S]);
        {$endif}
        DrawCellText(aCol,aRow,aRect,aState,S);
      end;
    end;
  end;
end;

function TCustomDBGrid.DefaultEditorStyle(const Style: TColumnButtonStyle;
  const F: TField): TColumnButtonStyle;
begin
  result := Style;
  if (Result=cbsAuto) and (F<>nil) then
    case F.DataType of
      ftBoolean: Result := cbsCheckboxColumn;
    end;
  if (result = cbsCheckBoxColumn) and not (dgeCheckboxColumn in FExtraOptions) then
    Result := cbsAuto;
end;

procedure TCustomDBGrid.DoCopyToClipboard;
var
  F: TField;
begin
  // copy current field to clipboard
  if not FDatalink.Active then
    exit;
  F := GetFieldFromGridColumn(Col);
  if F<>nil then
    Clipboard.AsText := F.AsString;
end;

procedure TCustomDBGrid.DoOnChangeBounds;
begin
  BeginUpdate;
  inherited DoOnChangeBounds;
  if HandleAllocated then
    LayoutChanged;
  EndUpdate;
end;

procedure TCustomDBGrid.DoPrepareCanvas(aCol, aRow: Integer;
  aState: TGridDrawState);
var
  DataCol: Integer;
  IsSelected: boolean;
begin
  if (ARow>=FixedRows) then begin

    if not DefaultDrawing then begin
      GetSelectedState(aState, IsSelected);
      if IsSelected then begin
        Canvas.Brush.Color := SelectedColor;
        Canvas.Font.Color := clHighlightText;
      end;
    end;

    if Assigned(OnPrepareCanvas) then begin
      DataCol := ColumnIndexFromGridColumn(aCol);
      if DataCol>=0 then
        OnPrepareCanvas(Self, DataCol, TColumn(Columns[DataCol]), aState);
    end;

  end;
end;

procedure TCustomDBGrid.DoLoadColumn(sender: TCustomGrid; aColumn: TGridColumn;
  aColIndex: Integer; aCfg: TXMLConfig; aVersion: Integer; aPath: string);
var
  c: TColumn;
  s: string;
begin
  c:=TColumn(aColumn);
  s := aCfg.GetValue(aPath + '/fieldname/value', '');
  if s<>'' then
    c.FieldName := s;
  s := aCfg.GetValue(aPath + '/displayformat/value', '');
  if s<>'' then
    c.DisplayFormat := s;
  inherited DoLoadColumn(sender, aColumn, aColIndex, aCfg, aVersion, aPath);
end;

procedure TCustomDBGrid.DoSaveColumn(sender: TCustomGrid; aColumn: TGridColumn;
  aColIndex: Integer; aCfg: TXMLConfig; aVersion: Integer; aPath: string);
var
  c: TColumn;
begin
  c:=TColumn(aColumn);
  aCfg.SetValue(aPath + '/fieldname/value', c.FieldName);
  aCfg.SetValue(aPath + '/displayformat/value', c.DisplayFormat);
  inherited DoSaveColumn(sender, aColumn, aColIndex, aCfg, aVersion, aPath);
end;

procedure TCustomDBGrid.BeforeMoveSelection(const DCol,DRow: Integer);
begin
  {$ifdef dbgDBGrid}DebugLnEnter('%s.BeforeMoveSelection INIT', [ClassName]);{$endif}
  inherited BeforeMoveSelection(DCol, DRow);
  if DCol<>Col then begin
    if assigned(OnColExit) then
      OnColExit(Self);
    FColEnterPending:=True;
  end;
{$ifdef dbgDBGrid}DebugLnExit('%s.BeforeMoveSelection DONE', [ClassName]);{$endif}
end;

procedure TCustomDBGrid.HeaderClick(IsColumn: Boolean; index: Integer);
begin
  {$ifdef dbgDBGrid}
  DebugLn('%s.HeaderClick', [ClassName]);
  {$endif}
  if IsColumn then
    DoHeaderClick(Index);
end;

procedure TCustomDBGrid.KeyDown(var Key: Word; Shift: TShiftState);
type
  TOperation=(opMoveBy,opCancel,opAppend,opInsert,opDelete);
var
  DeltaCol,DeltaRow: Integer;

  procedure DoOnKeyDown;
  begin
    {$ifdef dbgGrid}DebugLnEnter('DoOnKeyDown INIT');{$endif}
    if Assigned(OnKeyDown) then
      OnKeyDown(Self, Key, Shift);
    {$ifdef dbgGrid}DebugLnExit('DoOnKeyDown DONE');{$endif}
  end;

  {$ifdef dbgGrid}
  function OperToStr(AOper: TOperation): string;
  begin
    case AOper of
      opMoveBy: result := 'opMoveBy';
      opCancel: result := 'opCancel';
      opAppend: result := 'opAppend';
      opInsert: result := 'opInsert';
      opDelete: result := 'opDelete';
    end;
  end;
  {$endif}

  procedure DoOperation(AOper: TOperation; Arg: Integer = 0);
  begin
    {$ifdef dbgGrid}DebugLnEnter('KeyDown.DoOperation(%s,%d) INIT',[OperToStr(AOper),arg]);{$endif}
    GridFlags := GridFlags + [gfEditingDone];
    case AOper of
      opMoveBy:
        FDatalink.MoveBy(Arg);
      opCancel:
        begin
          if EditorMode then
            EditorCancelEditing;
          FDatalink.Dataset.Cancel;
        end;
      opAppend:
        FDatalink.Dataset.Append;
      opInsert:
        FDatalink.Dataset.Insert;
      opDelete:
        FDatalink.Dataset.Delete;
    end;
    GridFlags := GridFlags - [gfEditingDone];
    {$ifdef dbgGrid}DebugLnExit('KeyDown.DoOperation(%s,%d) DONE',[OperToStr(AOper),arg]);{$endif}
  end;

  procedure SelectNext(const AStart,ADown:Boolean);
  var
    N: Integer;
    CurBookmark: TBookmark;
  begin
    if dgPersistentMultiSelect in Options then
      exit;

    if (ssShift in Shift) then begin

      CurBookmark := FDatalink.DataSet.GetBookmark;
      if FKeyBookmark=nil then
        FKeyBookmark:=CurBookmark;

      if (FKeyBookmark=CurBookmark) then begin
        if AStart then begin
          SelectRecord(true);
          if ADown then
            FKeySign := 1
          else
            FKeySign := -1;
          exit;
        end;
        FKeySign := 0;
      end else
        FDatalink.DataSet.FreeBookmark(CurBookmark);

      n := 4*Ord(FKeySign>=0) + 2*Ord(ADown) + 1*Ord(AStart);
      case n of
        0,6,8..11:
          begin
            SelectRecord(True);
          end;
        3,5:
          begin
            SelectRecord(False);
          end;
      end;
    end else
      ClearSelection(true);
  end;

  function doVKDown: boolean;
  begin
    {$ifdef dbgGrid}DebugLnEnter('DoVKDown INIT');{$endif}
    if InsertCancelable then
    begin
      if IsEOF then
        result:=true
      else begin
        doOperation(opCancel);
        result := false;
      end;
    end else begin
      result:=false;
      SelectNext(true,true);
      doOperation(opMoveBy, 1);
      if GridCanModify and FDataLink.EOF then begin
        if not (dgDisableInsert in Options) then
          doOperation(opAppend);
      end else
        SelectNext(false,true);
    end;
    {$ifdef dbgGrid}DebugLnExit('DoVKDown DONE');{$endif}
  end;

  function DoVKUP: boolean;
  begin
    {$ifdef dbgGrid}DebugLnEnter('DoVKUP INIT');{$endif}
    if InsertCancelable then
      doOperation(opCancel)
    else begin
      SelectNext(true, false);
      doOperation(opMoveBy, -1);
      SelectNext(false, false);
    end;
    result := FDatalink.DataSet.BOF;
    {$ifdef dbgGrid}DebugLnExit('DoVKUP DONE');{$endif}
  end;

  procedure MoveSel(AReset: boolean);
  var
    ACol: Integer;
  begin
    if (DeltaCol<>0) or (DeltaRow<>0) then begin
      if DeltaRow > 0 then begin
        if doVKDown then
          //DeltaCol:=0; // tochk: strict? already in EOF, don't change column
      end else
      if DeltaRow < 0 then begin
        if doVKUP then
          //DeltaCol:=0; // tochk: strict? already in BOF, don't change column
      end;
      GridFlags := GridFlags + [gfEditingDone];
      if (DeltaCol<>0) then
        if Col + DeltaCol < FixedCols then
          Col := FixedCols
        else if Col + DeltaCol >= ColCount then
          Col := ColCount - 1
        else
        begin
          ACol := Col + DeltaCol;
          if ColWidths[ACol] > 0 then
            Col := ACol
          else
            if DeltaCol < 0 then
              Col := GetFirstVisibleColumn
            else
              Col := GetLastVisibleColumn;
        end;
      GridFlags := GridFlags - [gfEditingDone];
    end else
    if AReset then
      ResetEditor;
  end;

begin
  {$ifdef dbgGrid}DebugLnEnter('%s.KeyDown %s INIT Key=%d',[ClassName, Name,Key]);{$endif}
  case Key of

    VK_TAB:
      begin
        doOnKeyDown;
        if (Key<>0) and ValidDataset then begin
          if (dgTabs in Options) then begin

            if ((ssShift in shift) and
               (Col<=GetFirstVisibleColumn) and (Row<=GetFirstVisibleRow)) then begin
              if EditorKey then
                GridFlags := GridFlags + [gfRevEditorTab];
              {$ifdef dbgGrid}DebugLnExit('%s.KeyDown Exit: Tab: Shift',[ClassName]);{$endif}
              exit;
            end;

            GetDeltaMoveNext(ssShift in Shift, DeltaCol, DeltaRow, TabAdvance);

            if (not (ssShift in Shift)) and (Row>=GetLastVisibleRow) and
               (DeltaRow>0) and (Col=GetLastVisibleColumn) and
               (FDatalink.Editing or not GridCanModify) then begin
              {$ifdef dbgGrid}DebugLnExit('%s.KeyDown Exit: Tab: not shift',[ClassName]);{$endif}
              exit;
            end;

            MoveSel(false);
            Key := 0;
          end;
        end;
      end;

    VK_RETURN:
      begin
        doOnKeyDown;
        if (Key<>0) and ValidDataset then begin
          key:=0;
          if (dgEditing in Options) and not EditorMode then
            EditorMode:=true
          else begin
            GetDeltaMoveNext(ssShift in Shift, DeltaCol, DeltaRow, AutoAdvance);
            MoveSel(True);
          end;
        end;
      end;

    VK_DELETE:
      begin
        doOnKeyDown;
        if (Key<>0) and (ssCtrl in Shift) and GridCanModify and
           (not (dgDisableDelete in Options)) and
           not FDataLink.DataSet.IsEmpty then begin

          if not (dgConfirmDelete in Options) or
            (MessageDlg(rsDeleteRecord, mtConfirmation, mbOKCancel, 0 )<>mrCancel)
          then begin
            doOperation(opDelete);
            key := 0;
          end;

        end;
      end;

    VK_DOWN:
      begin
        DoOnKeyDown;
        if (Key<>0) and ValidDataset then begin
          doVKDown;
          Key := 0;
        end;
      end;

    VK_UP:
      begin
        doOnKeyDown;
        if (Key<>0) and ValidDataset then begin
          doVKUp;
          key := 0;
         end;
      end;

    VK_NEXT:
      begin
        doOnKeyDown;
        if (Key<>0) and ValidDataset then begin
          doOperation(opMoveBy, VisibleRowCount);
          ClearSelection(true);
          Key := 0;
        end;
      end;

    VK_PRIOR:
      begin
        doOnKeyDown;
        if (Key<>0) and ValidDataset then begin
          doOperation(opMoveBy, -VisibleRowCount);
          ClearSelection(true);
          key := 0;
        end;
      end;

    VK_ESCAPE:
      begin
        doOnKeyDown;
        if (Key<>0) and ValidDataset then begin
          if EditorMode then begin
            EditorCancelEditing;
            if FDatalink.Active and not FDatalink.Dataset.Modified then
              FDatalink.Modified := False;
            Key := 0;
          end else
            if FDataLink.Active then
              doOperation(opCancel);
        end;
      end;

    VK_INSERT:
      begin
        doOnKeyDown;
        if Key<>0 then
          if not (dgDisableInsert in Options) and GridCanModify then begin
            doOperation(opInsert);
            Key:=0;
          end;
      end;

    VK_HOME:
      begin
        doOnKeyDown;
        if Key<>0 then begin
          if FDatalink.Active then begin
            GridFlags := GridFlags + [gfEditingDone];
            if ssCTRL in Shift then
              FDataLink.DataSet.First
            else
              MoveNextSelectable(False, FixedCols, Row);
            GridFlags := GridFlags - [gfEditingDone];
            ClearSelection(true);
            Key:=0;
          end;
        end;
      end;

    VK_END:
      begin
        doOnKeyDown;
        if Key<>0 then begin
          if FDatalink.Active then begin
            GridFlags := GridFlags + [gfEditingDone];
            if ssCTRL in shift then
              FDatalink.DataSet.Last
            else begin
              DeltaCol := GetLastVisibleColumn;
              if DeltaCol>=0 then
                MoveNextSelectable(False, DeltaCol, Row);
            end;
            GridFlags := GridFlags - [gfEditingDone];
            ClearSelection(true);
            Key:=0;
          end;
        end;
      end;

    VK_SPACE:
      begin
        doOnKeyDown;
        if (Key<>0) and ValidDataset then begin
          if ColumnEditorStyle(Col, SelectedField) = cbsCheckboxColumn then begin
            SwapCheckBox;
            Key:=0;
          end;
        end;
      end;

    VK_MULTIPLY:
      begin
        doOnKeyDown;
        if (Key<>0) and ValidDataset and (ssCtrl in Shift) then
          ToggleSelectedRow;
      end;

    else
      inherited KeyDown(Key, Shift);
  end;
  {$ifdef dbgGrid}DebugLnExit('%s.KeyDown DONE Key= %d',[ClassName, Key]);{$endif}
end;

procedure TCustomDBGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  Gz: TGridZone;
  P: TPoint;
  procedure doMouseDown;
  begin
    if not Focused and not(csNoFocus in ControlStyle) then
      SetFocus;
    if assigned(OnMouseDown) then
      OnMouseDown(Self, Button, Shift, X, Y);
  end;
  procedure doInherited;
  begin
    inherited MouseDown(Button, Shift, X, Y);
  end;
  procedure doMoveBy;
  begin
    {$ifdef dbgGrid}DebugLnEnter('%s.MouseDown MoveBy INIT', [ClassName]); {$endif}
    FDatalink.MoveBy(P.Y - Row);
    {$ifdef dbgGrid}DebugLnExit('%s.MouseDown MoveBy DONE', [ClassName]); {$endif}
  end;
  procedure doMoveToColumn;
  begin
    {$ifdef dbgGrid}DebugLnEnter('%s.MouseDown MoveToCol INIT Col=%d', [ClassName, P.X]); {$endif}
    Col := P.X;
    {$ifdef dbgGrid}DebugLnExit('%s.MouseDown MoveToCol DONE', [ClassName]); {$endif}
  end;
  procedure DoCancel;
  begin
    {$ifdef dbgGrid}DebugLnEnter('%s.MouseDown Dataset.CANCEL INIT', [ClassName]);{$endif}
    if EditorMode then
      EditorCancelEditing;
    FDatalink.Dataset.cancel;
    {$ifdef dbgGrid}DebugLnExit('%s.MouseDown Dataset.CANCEL DONE', [ClassName]);{$endif}
  end;
  procedure DoAcceptValue;
  begin
    if EditorMode and FDatalink.FModified then
      EditorMode := False;
  end;
begin

  if (csDesigning in componentState) {or not GCache.ValidGrid }then begin
    {$ifdef dbgDBGrid}DebugLn('%s.MouseDown - checkDesigning', [ClassName]);{$endif}
    exit;
  end;

  if UpdatingData then begin
    {$ifdef dbgDBGrid}DebugLn('%s.MouseDown - UpdatingData', [ClassName]);{$endif}
    exit;
  end;

  if not MouseButtonAllowed(Button) then begin
    {$ifdef dbgDBGrid}DebugLn('%s.MouseDown - no mouse allowed', [ClassName]);{$endif}
    doInherited;
    exit;
  end;

  {$ifdef dbgGrid}DebugLnEnter('%s.MouseDown INIT', [ClassName]); {$endif}
  Gz:=MouseToGridZone(X,Y);
  CacheMouseDown(X,Y);
  case Gz of
    gzInvalid:
      doMouseDown;

    gzFixedCells, gzFixedCols:
      doInherited;
    else
      begin

        if FKeyBookmark<>nil then begin
          FDatalink.DataSet.FreeBookmark(FKeyBookmark);
          FKeyBookmark:=nil; // force new keyboard selection start
        end;

        P:=MouseToCell(Point(X,Y));
        if Gz=gzFixedRows then
          P.X := Col;

        if P.Y=Row then begin      // The current active row was clicked again.
          //doAcceptValue;

          if ssCtrl in Shift then begin
            doMouseDown;
            // Don't unselect the row if Right-click was for PopupMenu.
            if (Button<>mbRight) or (PopupMenu=Nil) then
              ToggleSelectedRow;
          end
          else begin
            if Button=mbLeft then
              ClearSelection(true);
            if gz=gzFixedRows then begin
              fGridState:=gsRowMoving;
              ResetLastMove;
              doMouseDown;
            end
            else
              doInherited;
          end;

        end else begin
          doMouseDown;
          if ValidDataSet then begin
            if InsertCancelable and IsEOF then
              doCancel;
            doMoveBy;
            if IsMouseOverCellButton(X, Y) then
              StartPushCell;
          end;
          if ssCtrl in Shift then
            ToggleSelectedRow
          else begin
            if Button=mbLeft then
              ClearSelection(true)
            // Select row before popupmenu
            else if (Button=mbRight) and Assigned(PopupMenu) and not FSelectedRows.CurrentRowSelected then
              ToggleSelectedRow;
            doMoveToColumn;
          end;
        end;
      end;
  end;
  {$ifdef dbgGrid}DebugLnExit('%s.MouseDown DONE', [ClassName]); {$endif}
end;

procedure TCustomDBGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if (fGridState=gsSelecting) and not Dragging then
    exit
  else
    inherited MouseMove(Shift, X, Y);
end;

procedure TCustomDBGrid.PrepareCanvas(aCol, aRow: Integer;
  aState: TGridDrawState);
begin
  inherited PrepareCanvas(aCol, aRow, aState);

  if gdFixed in aState then begin
    if gdHot in aState then
      Canvas.Brush.Color := FixedHotColor
    else
      Canvas.Brush.Color := GetColumnColor(aCol, gdFixed in AState);
  end;

  if (not FDatalink.Active) and ((gdSelected in aState) or (gdFocused in aState)) then
    Canvas.Brush.Color := Self.Color;
end;

procedure TCustomDBGrid.RemoveAutomaticColumns;
begin
  if not (csDesigning in ComponentState) then
    TDBGridColumns(Columns).RemoveAutoColumns;
end;

procedure TCustomDBGrid.ResetSizes;
begin
  LayoutChanged;
  inherited ResetSizes;
end;

procedure TCustomDBGrid.SaveGridOptions(Cfg: TXMLConfig);
var
  Path: string;
begin
  Path:='grid/design/options/';
  Cfg.SetValue(Path+'dgEditing/value', dgEditing in Options);
  Cfg.SetValue(Path+'dgTitles/value', dgTitles in Options);
  Cfg.SetValue(Path+'dgIndicator/value', dgIndicator in Options);
  Cfg.SetValue(Path+'dgColumnResize/value', dgColumnResize in Options);
  Cfg.SetValue(Path+'dgColumnMove/value', dgColumnMove in Options);
  Cfg.SetValue(Path+'dgColLines/value', dgColLines in Options);
  Cfg.SetValue(Path+'dgRowLines/value', dgRowLines in Options);
  Cfg.SetValue(Path+'dgTabs/value', dgTabs in Options);
  Cfg.SetValue(Path+'dgAlwaysShowEditor/value', dgAlwaysShowEditor in Options);
  Cfg.SetValue(Path+'dgRowSelect/value', dgRowSelect in Options);
  Cfg.SetValue(Path+'dgAlwaysShowSelection/value', dgAlwaysShowSelection in Options);
  Cfg.SetValue(Path+'dgConfirmDelete/value', dgConfirmDelete in Options);
  Cfg.SetValue(Path+'dgCancelOnExit/value', dgCancelOnExit in Options);
  Cfg.SetValue(Path+'dgMultiselect/value', dgMultiselect in Options);
  Cfg.SetValue(Path+'dgHeaderHotTracking/value', dgHeaderHotTracking in Options);
  Cfg.SetValue(Path+'dgHeaderPushedLook/value', dgHeaderPushedLook in Options);
  Cfg.SetValue(Path+'dgPersistentMultiSelect/value', dgPersistentMultiSelect in Options);
  cfg.SetValue(Path+'dgAutoSizeColumns/value', dgAutoSizeColumns in Options);
  cfg.SetValue(Path+'dgAnyButtonCanSelect/value', dgAnyButtonCanSelect in Options);
  Cfg.SetValue(Path+'dgDisableDelete/value', dgDisableDelete in Options);
  Cfg.SetValue(Path+'dgDisableInsert/value', dgDisableInsert in Options);
  Cfg.SetValue(Path+'dgCellHints/value', dgCellHints in Options);
  cfg.SetValue(Path+'dgTruncCellHints/value', dgTruncCellHints in Options);
  Cfg.SetValue(Path+'dgCellEllipsis/value', dgCellEllipsis in Options);
  Cfg.SetValue(Path+'dgRowHighlight/value', dgRowHighlight in Options);
  Cfg.SetValue(Path+'dgThumbTracking/value', dgThumbTracking in Options);
end;

procedure TCustomDBGrid.SelectEditor;
var
  aEditor: TWinControl;
  aMaxLen: integer;
begin
  {$ifdef dbgDBGrid}
  DebugLnEnter('%s.SelectEditor INIT Editor=%s',[ClassName, dbgsname(editor)]);
  {$endif}
  if (FDatalink<>nil) and FDatalink.Active then begin
    inherited SelectEditor;

    if (SelectedField is TStringField) then
      aMaxLen := SelectedField.Size
    else
      aMaxLen := 0;

    if (Editor is TCustomEdit) then
      TCustomEdit(Editor).MaxLength := aMaxLen
    else
    if (Editor is TCompositeCellEditor) then
      TCompositeCellEditor(Editor).MaxLength := aMaxLen;

    if Assigned(OnSelectEditor) then begin
      aEditor:=Editor;
      OnSelectEditor(Self, SelectedColumn, aEditor);
      Editor:=aEditor;
    end;
  end else
    Editor := nil;
  {$ifdef dbgDBGrid}
  DebugLnExit('%s.SelectEditor DONE Editor=%s',[ClassName, dbgsname(editor)]);
  {$endif}
end;

procedure TCustomDBGrid.SetEditText(ACol, ARow: Longint; const Value: string);
begin
  FTempText := Value;
end;

procedure TCustomDBGrid.SetFixedCols(const AValue: Integer);
begin
  if (FixedCols=AValue) or (AValue<FirstGridColumn) then
    exit;
  inherited SetFixedCols(AValue);
end;

function TCustomDBGrid.SelectCell(aCol, aRow: Integer): boolean;
begin
  Result:= (ColWidths[aCol] > 0) and (RowHeights[aRow] > 0);
end;

procedure TCustomDBGrid.BeginLayout;
begin
  inc(FLayoutChangedCount);
end;

procedure TCustomDBGrid.EditingColumn(aCol: Integer; Ok: boolean);
begin
  {$ifdef dbgDBGrid}DebugLnEnter('%s.EditingColumn INIT aCol=%d Ok=%s',
  	[ClassName, aCol, BoolToStr(ok, true)]); {$endif}
  if Ok then begin
    FEditingColumn := aCol;
    FDatalink.Modified := True;
  end
  else
    FEditingColumn := -1;
  {$ifdef dbgDBGrid} DebugLnExit('%s.EditingColumn DONE', [ClassName]); {$endif}
end;

procedure TCustomDBGrid.EditorCancelEditing;
begin
  EditingColumn(FEditingColumn, False); // prevents updating the value
  if EditorMode then begin
    EditorMode := False;
    if dgAlwaysShowEditor in Options then
      EditorMode := True;
  end;
end;

procedure TCustomDBGrid.EditorDoGetValue;
begin
  {$ifdef dbgDBGrid}DebugLnEnter('%s.EditorDoGetValue INIT', [ClassName]);{$endif}
  inherited EditordoGetValue;
  UpdateData;
  {$ifdef dbgDBGrid}DebugLnExit('%s.EditorDoGetValue DONE', [ClassName]);{$endif}
end;

procedure TCustomDBGrid.CellClick(const aCol, aRow: Integer; const Button:TMouseButton);
begin
  {$ifdef dbgGrid}DebugLn('%s.CellClick', [ClassName]); {$endif}
  if Button<>mbLeft then
    exit;

  if (aCol>=FirstGridColumn) then begin
    if (aRow>=FixedRows) then begin
      if IsColumnVisible(aCol) and
         (ColumnEditorStyle(ACol, SelectedField) = cbsCheckboxColumn) then begin
        // react only if overriden editor is hidden
        if (Editor=nil) or not EditorMode then
          SwapCheckBox
      end;
      if Assigned(OnCellClick) then
        OnCellClick(TColumn(ColumnFromGridColumn(aCol)));
    end else
      DoHeaderClick(aCol)
  end;
end;

function TCustomDBGrid.CheckDisplayMemo(aField: TField): boolean;
begin
  // note that this assumes that aField is not nil
  result := (aField.DataType=ftMemo) and (dgDisplayMemoText in Options);
end;

procedure TCustomDBGrid.EndLayout;
begin
  dec(FLayoutChangedCount);
  if FLayoutChangedCount = 0 then
    DoLayoutChanged;
end;

function TCustomDBGrid.GetDefaultColumnAlignment(Column: Integer): TAlignment;
var
  F: TField;
begin
  F := GetDsFieldFromGridColumn(Column);
  if F<>nil then
    result := F.Alignment
  else
    result := taLeftJustify;
end;

function TCustomDBGrid.GetDefaultColumnWidth(Column: Integer): Integer;
begin
  Result := DefaultFieldColWidth(GetDsFieldFromGridColumn(Column));
end;

function TCustomDBGrid.GetDefaultColumnReadOnly(Column: Integer): boolean;
var
  F: Tfield;
begin
  result := true;
  if not Self.ReadOnly and (FDataLink.Active and not FDatalink.ReadOnly) then begin
    F := GetDsFieldFromGridColumn(Column);
    result := (F=nil) or F.ReadOnly;
  end;
end;

function TCustomDBGrid.GetDefaultColumnTitle(Column: Integer): string;
var
  F: Tfield;
begin
  F := GetDsFieldFromGridColumn(Column);
  if F<>nil then
    Result := F.DisplayName
  else
    Result := '';
end;

function TCustomDBGrid.GetDefaultRowHeight: integer;
begin
  result := inherited GetDefaultRowHeight;
  Dec(Result, 2); // a litle smaller for dbgrid
end;

procedure TCustomDBGrid.DoExit;
begin
  {$ifdef dbgDBGrid}DebugLnEnter('%s.DoExit INIT', [ClassName]);{$endif}
  if ValidDataSet and (dgCancelOnExit in Options) and
    InsertCancelable then
  begin
    FDataLink.DataSet.Cancel;
    EditingColumn(FEditingColumn, False);
  end;
  inherited DoExit;
  {$ifdef dbgDBGrid}DebugLnExit('%s.DoExit DONE', [ClassName]);{$endif}
end;

function TCustomDBGrid.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint
  ): Boolean;
begin
  Result := False;
  if Assigned(OnMouseWheelDown) then
    OnMouseWheelDown(Self, Shift, MousePos, Result);
  if not Result and FDatalink.Active then begin
    FDatalink.MoveBy(1);
    Result := True;
  end;
end;

function TCustomDBGrid.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint
  ): Boolean;
begin
  Result := False;
  if Assigned(OnMouseWheelUp) then
    OnMouseWheelUp(Self, Shift, MousePos, Result);
  if not Result and FDatalink.Active then begin
    FDatalink.MoveBy(-1);
    Result := True;
  end;
end;

function TCustomDBGrid.GetEditMask(aCol, aRow: Longint): string;
var
  aField: TField;
begin
  Result := '';
  if FDataLink.Active then begin
    aField := GetFieldFromGridColumn(aCol);
    if (aField<>nil) then begin
      Result := aField.EditMask;
      if assigned(OnFieldEditMask) then
        OnFieldEditMask(Self, AField, Result);
    end;
  end;
end;

function TCustomDBGrid.GetEditText(aCol, aRow: Longint): string;
var
  aField: TField;
begin
  Result := '';
  if FDataLink.Active then begin
    aField := GetFieldFromGridColumn(aCol);
    if aField<>nil then begin
      if CheckDisplayMemo(aField) then
        Result := aField.AsString
      else
        Result := aField.Text;
    end;
  end;
end;

function TCustomDBGrid.GetIsCellSelected(aCol, aRow: Integer): boolean;
begin
  Result:=inherited GetIsCellSelected(aCol, aRow) or
    FDrawingMultiSelRecord;
end;

function TCustomDBGrid.GetIsCellTitle(aCol, aRow: Integer): boolean;
begin
  result := (FixedRows>0) and (aRow=0);
  if result and Columns.Enabled then
    result := (aCol>=FirstGridColumn);
end;

procedure TCustomDBGrid.GetSelectedState(AState: TGridDrawState; out
  IsSelected: boolean);
begin
  inherited GetSelectedState(AState, IsSelected);
  if IsSelected and not Self.Focused and not(dgAlwaysShowSelection in Options) then
    IsSelected := false;
end;

function TCustomDBGrid.GetSmoothScroll(Which: Integer): Boolean;
begin
  if Which=SB_Vert then
    Result := False
  else
    Result := inherited GetSmoothScroll(Which);
end;

function TCustomDBGrid.GridCanModify: boolean;
begin
  result := not ReadOnly and (dgEditing in Options) and not FDataLink.ReadOnly
    and FDataLink.Active and FDatalink.DataSet.CanModify;
end;

procedure TCustomDBGrid.GetSBVisibility(out HsbVisible, VsbVisible: boolean);
var
  aRange,aPage,aPos: Integer;
begin
  inherited GetSBVisibility(HsbVisible, VsbVisible);
  VSbVisible := (ScrollBars in [ssVertical, ssBoth]);
  if not VSbVisible and ScrollBarAutomatic(ssVertical) then begin
    GetScrollbarParams(aRange,aPage, aPos);
    if ARange>aPage then
      VSbVisible:=True;
  end;
end;

procedure TCustomDBGrid.GetSBRanges(const HsbVisible, VsbVisible: boolean; out
  HsbRange, VsbRange, HsbPage, VsbPage, HsbPos, VsbPos: Integer);
begin
  inherited GetSBRanges(HsbVisible, VsbVisible, HsbRange, VsbRange, HsbPage, VsbPage, HsbPos, VsbPos);
  if VSbVisible then
    GetScrollbarParams(VsbRange, VsbPage, VsbPos)
  else begin
    VsbRange := 0;
    VsbPage := 0;
    VsbPos := 0;
  end;
end;

procedure TCustomDBGrid.MoveSelection;
begin
  {$ifdef dbgDBGrid}DebugLnEnter('%s.MoveSelection INIT', [ClassName]);{$endif}
  inherited MoveSelection;
  if FColEnterPending and Assigned(OnColEnter) then begin
    OnColEnter(Self);
  end;
  FColEnterPending:=False;
  UpdateActive;
  {$ifdef dbgDBGrid}DebugLnExit('%s.MoveSelection DONE', [ClassName]);{$endif}
end;

function TCustomDBGrid.MouseButtonAllowed(Button: TMouseButton): boolean;
begin
  Result:= FDataLink.Active and ((Button=mbLeft) or (dgAnyButtonCanSelect in Options));
end;

procedure TCustomDBGrid.DrawAllRows;
var
  CurActiveRecord: Integer;
begin
  if FDataLink.Active then begin
    {$ifdef dbgGridPaint}
    DebugLnEnter('%s DrawAllRows INIT Link.ActiveRecord=%d, Row=%d',[Name, FDataLink.ActiveRecord, Row]);
    {$endif}
    CurActiveRecord:=FDataLink.ActiveRecord;
    FDrawingEmptyDataset:=FDatalink.DataSet.IsEmpty;
  end else
    FDrawingEmptyDataset:=True;
  try
    inherited DrawAllRows;
  finally
    if FDataLink.Active then begin
      FDataLink.ActiveRecord:=CurActiveRecord;
      {$ifdef dbgGridPaint}
      DebugLnExit('%s DrawAllRows DONE Link.ActiveRecord=%d, Row=%d',[Name, FDataLink.ActiveRecord, Row]);
      {$endif}
    end;
  end;
end;

procedure TCustomDBGrid.DrawFocusRect(aCol, aRow: Integer; ARect: TRect);
begin
  // Draw focused cell if we have the focus
  if Self.Focused and (dgAlwaysShowSelection in Options) and
    FDatalink.Active and DefaultDrawing then
  begin
    CalcFocusRect(aRect);
    DrawRubberRect(Canvas, aRect, FocusColor);
  end;
end;

//
procedure TCustomDBGrid.DrawRow(ARow: Integer);
begin
  if (ARow>=FixedRows) and FDataLink.Active then begin
    //if (Arow>=FixedRows) and FCanBrowse then
    FDataLink.ActiveRecord:=ARow-FixedRows;
    FDrawingActiveRecord := ARow = Row;
    FDrawingMultiSelRecord := (dgMultiSelect in Options) and
      SelectedRows.CurrentRowSelected
  end else begin
    FDrawingActiveRecord := False;
    FDrawingMultiSelRecord := False;
  end;
  {$ifdef dbgGridPaint}
  DbgOut('DrawRow Row=', IntToStr(ARow), ' Act=', dbgs(FDrawingActiveRecord));
  {$endif}
  inherited DrawRow(ARow);
  {$ifdef dbgGridPaint}
  DebugLn('End Row')
  {$endif}
end;

procedure TCustomDBGrid.DrawCell(aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  DataCol: Integer;
begin
  PrepareCanvas(aCol, aRow, aState);

  {$ifdef dbgGridPaint}
  DbgOut(' ',IntToStr(aCol));
  if gdSelected in aState then DbgOut('S');
  if gdFocused in aState then DbgOut('*');
  if gdFixed in aState then DbgOut('F');
  {$endif dbgGridPaint}

  if (gdFixed in aState) or DefaultDrawing then
    DefaultDrawCell(aCol, aRow, aRect, aState)
  else
  if not DefaultDrawing then
    DrawCellBackground(aCol, aRow, aRect, aState);

  if not (csDesigning in ComponentState) then
  begin
    if (ARow>=FixedRows) and Assigned(OnDrawColumnCell) then begin
      DataCol := ColumnIndexFromGridColumn(aCol);
      if DataCol>=0 then
        OnDrawColumnCell(Self, aRect, DataCol, TColumn(Columns[DataCol]), aState);
    end;
    if (ARow<FixedRows) and Assigned(OnDrawColumnTitle) then begin
      DataCol := ColumnIndexFromGridColumn(aCol);
      if DataCol>=0 then
        OnDrawColumnTitle(Self, aRect, DataCol, TColumn(Columns[DataCol]), aState);
    end;
  end;

  DrawCellGrid(aCol, aRow, aRect, aState);
end;

procedure TCustomDBGrid.DrawCellBackground(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState);
begin
  // background
  if (gdFixed in aState) and (TitleStyle=tsNative) then
    DrawThemedCell(aCol, aRow, aRect, aState)
  else
    Canvas.FillRect(aRect);
end;

procedure TCustomDBGrid.DrawCheckboxBitmaps(aCol: Integer; aRect: TRect;
  F: TField);
var
  AState: TCheckboxState;
begin
  if (aCol=Col) and FDrawingActiveRecord then begin
    // show checkbox only if overriden editor is hidden
    if EditorMode then
      exit;
  end;

  // by SSY
  if (F<>nil) then
    if F.DataType=ftBoolean then
      if F.IsNull then
        AState := cbGrayed
      else
      if F.AsBoolean then
        AState := cbChecked
      else
        AState := cbUnChecked
    else
      if F.AsString=ColumnFromGridColumn(aCol).ValueChecked then
        AState := cbChecked
      else

      if F.AsString=ColumnFromGridColumn(aCol).ValueUnChecked then
        AState := cbUnChecked
      else
        AState := cbGrayed
  else
    AState := cbGrayed;

  if assigned(OnUserCheckboxState) then
    OnUserCheckboxState(Self, TColumn(ColumnFromGridColumn(aCol)), AState);

  DrawGridCheckboxBitmaps(aCol, Row{dummy}, ARect, AState);
end;

procedure TCustomDBGrid.DrawFixedText(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState);

  function GetDatasetState: TDataSetState;
  begin
    if FDatalink.Active then
      result := FDataLink.DataSet.State
    else
      result := dsInactive;
  end;

begin
  if (ACol=0) and (dgIndicator in Options) and FDrawingActiveRecord then begin
    DrawIndicator(Canvas, aRect, GetDataSetState, FDrawingMultiSelRecord);
    {$ifdef dbgGridPaint}
    dbgOut('>');
    {$endif}
  end else
  if (ACol=0) and (dgIndicator in Options) and FDrawingMultiSelRecord then
    DrawIndicator(Canvas, aRect, dsCurValue{dummy}, True)
  else
    DrawColumnText(aCol, aRow, aRect, aState);
end;

procedure TCustomDBGrid.DrawColumnText(aCol, aRow: Integer; aRect: TRect;
 aState: TGridDrawState);
var
  F: TField;
  s: String;
begin
  if GetIsCellTitle(aCol, aRow) then
    inherited DrawColumnText(aCol, aRow, aRect, aState)
  else begin
    F := GetFieldFromGridColumn(aCol);
    if F<>nil then begin
      if CheckDisplayMemo(F) then
        s := F.AsString
      else
        s := F.DisplayText;
      DrawCellText(aCol, aRow, aRect, aState, s)
    end;
  end;
end;

procedure TCustomDBGrid.DrawIndicator(ACanvas: TCanvas; R: TRect;
  Opt: TDataSetState; MultiSel: boolean);
var
  dx, dy, x, y: Integer;

  procedure CenterY;
  begin
    y := R.Top + (R.Bottom-R.Top) div 2;
  end;

  procedure CenterX;
  begin
    X := R.Left + (R.Right-R.Left) div 2;
  end;

  procedure DrawEdit(clr: Tcolor);
  begin
    ACanvas.Pen.Color := clr;
    CenterY;
    CenterX;
    ACanvas.MoveTo(X-2, Y-Dy);
    ACanvas.LineTo(X+3, Y-Dy);
    ACanvas.MoveTo(X, Y-Dy);
    ACanvas.LineTo(X, Y+Dy);
    ACanvas.MoveTo(X-2, Y+Dy);
    ACanvas.LineTo(X+3, Y+Dy);
  end;

begin
  dx := 6;
  dy := 6;
  x := 0;
  y := 0;
  case Opt of
    dsBrowse:
      begin //
        ACanvas.Brush.Color:=clBlack;
        ACanvas.Pen.Color:=clBlack;
        CenterY;
        x:= R.Left+3;
        if MultiSel then begin
          if BiDiMode = bdRightToLeft then begin
            ACanvas.Polyline([point(x+dx,y-dy),  point(x,y),point(x+dx,y+dy), point(x+dx,y+dy-1)]);
            ACanvas.Polyline([point(x+dx,y-dy+1),  point(x+1,y),point(x+dx,y+dy-1), point(x+dx,y+dy-2)]);
            CenterX;
            Dec(X,3);
            ACanvas.Ellipse(Rect(X+dx-2,Y-2,X+dx+2,Y+2));
          end else begin
            ACanvas.Polyline([point(x,y-dy),  point(x+dx,y),point(x,y+dy), point(x,y+dy-1)]);
            ACanvas.Polyline([point(x,y-dy+1),point(x+dx-1,y),point(x, y+dy-1), point(x,y+dy-2)]);
            CenterX;
            Dec(X,3);
            ACanvas.Ellipse(Rect(X-2,Y-2,X+2,Y+2));
          end;
        end else begin
          if BiDiMode = bdRightToLeft then
            ACanvas.Polygon([point(x,y),point(x+dx,y-dy),point(x+dx, y+dy),point(x,y)])
          else
            ACanvas.Polygon([point(x,y-dy),point(x+dx,y),point(x, y+dy),point(x,y-dy)]);
        end;
      end;
    dsEdit:
      DrawEdit(clBlack);
    dsInsert:
      DrawEdit(clGreen);
    else
    if MultiSel then begin
      ACanvas.Brush.Color:=clBlack;
      ACanvas.Pen.Color:=clBlack;
      CenterX;
      CenterY;
      ACanvas.Ellipse(Rect(X-3,Y-3,X+3,Y+3));
    end;
  end;
end;

function TCustomDBGrid.EditorCanAcceptKey(const ch: TUTF8Char): boolean;
var
  aField: TField;
begin
  result := False;
  if FDataLink.Active then begin
    aField := SelectedField;
    if aField<>nil then begin
      Result := IsValidChar(AField, Ch) and not aField.Calculated and
        (aField.DataType<>ftAutoInc) and (aField.FieldKind<>fkLookup) and
        (not aField.IsBlob or CheckDisplayMemo(aField));
    end;
  end;
end;

function TCustomDBGrid.EditorIsReadOnly: boolean;
var
  AField : TField;
  FieldList: TList;
  I: Integer;
begin
  Result := inherited EditorIsReadOnly;
  if not Result then begin

    AField := GetFieldFromGridColumn(Col);
    if assigned(AField) then begin

      // if field can't be modified, it's assumed readonly
      result := not AField.CanModify;

      // if field is readonly, check if it's a lookup field
      if result and (AField.FieldKind = fkLookup) then begin
        FieldList := TList.Create;
        try
          AField.DataSet.GetFieldList(FieldList, AField.KeyFields);
          // check if any keyfields are there
          result := (FieldList.Count=0); // if not simply is still readonly
                                         // if yes assumed keyfields are modifiable
          for I := 0 to FieldList.Count-1 do
            if not TField(FieldList[I]).CanModify then begin
              result := true; // at least one keyfield is readonly
              break;
            end;
        finally
          FieldList.Free;
        end;
      end;

      // if it's not readonly and is not already editing, start editing.
      if not result and not FDatalink.Editing then begin
        Include(FGridStatus, gsStartEditing);
        Result := not FDataLink.Edit;
        Exclude(FGridStatus, gsStartEditing);
      end;

    end
    else
      result := true;  // field is nil so it's readonly

  end;
end;

procedure TCustomDBGrid.EditorTextChanged(const aCol, aRow: Integer;
  const aText: string);
var
  isReadOnly: Boolean;
begin
  isReadOnly := EditorIsReadonly;
  if not isReadOnly then
    SetEditText(aCol, aRow, aText);
  EditingColumn(Col, not isReadOnly);
end;

procedure TCustomDBGrid.HeaderSized(IsColumn: Boolean; Index: Integer);
var
  i: Integer;
begin
  if IsColumn then begin
    if Columns.Enabled then begin
      i := ColumnIndexFromGridColumn(Index);
      if i>=0 then
        Columns[i].Width := ColWidths[Index];
    end;
    FDefaultColWidths := False;
    if Assigned(OnColumnSized) then
      OnColumnSized(Self);
  end;
end;

function TCustomDBGrid.IsColumnVisible(aCol: Integer): boolean;
var
  gridcol: TGridColumn;
begin
  if Columns.Enabled then begin
    gridcol := ColumnFromGridColumn(aCol);
    result := (gridcol<>nil) and gridCol.Visible;
  end else
    result := (aCol>=FirstGridColumn) and (ColWidths[aCol]>0);
end;

function TCustomDBGrid.IsValidChar(AField: TField; AChar: TUTF8Char): boolean;
begin
  result := False;

  if Length(AChar)>1 then begin
    // problem: AField should validate a unicode char, but AField has no
    //          such facility, ask the user, if user is not interested
    //          do ansi convertion and try with field.

    { TODO: is this really necessary?
    if Assigned(FOnValidateUTF8Char) then begin
      result := true;
      OnValidateUT8Char(Self, AField, AChar, Result)
      exit;
    end else
    }
      AChar := UTF8ToSys(AChar);
  end else
  if Length(AChar)=0 then
    exit;

  Result := (AChar[1]=#8) or AField.IsValidChar(AChar[1])
end;

procedure TCustomDBGrid.UpdateActive;
var
  PrevRow: Integer;
  NewRow: Integer;
begin
  if (csDestroying in ComponentState) or
    (FDatalink=nil) or (not FDatalink.Active) or
    (FDatalink.ActiveRecord<0) then
    exit;
  {$ifdef dbgDBGrid}
  DebugLn('%s.UpdateActive (%s): ActiveRecord=%d FixedRows=%d Row=%d',
  		[ClassName, Name, FDataLink.ActiveRecord, FixedRows, Row]);
  {$endif}
  PrevRow := Row;
  NewRow:= FixedRows + FDataLink.ActiveRecord;
  if NewRow>RowCount-1 then
    NewRow := RowCount-1;
  Row := NewRow;
  if PrevRow<>Row then
    InvalidateCell(0, PrevRow);//(InvalidateRow(PrevRow);
  InvalidateRow(Row);
end;

function TCustomDBGrid.UpdateGridCounts: Integer;
var
  RecCount: Integer;
  FRCount, FCCount: Integer;
begin
  // find out the column count, if result=0 then
  // there are no visible columns defined or dataset is inactive
  // or there are no visible fields, ie the grid is blank
  {$ifdef dbgDBGrid}DebugLnEnter('%s.UpdateGridCounts INIT', [ClassName]);{$endif}
  BeginUpdate;
  try
    Result := GetColumnCount;
    if Result > 0 then begin
      if dgTitles in Options then FRCount := 1 else FRCount := 0;
      if dgIndicator in Options then FCCount := 1 else FCCount := 0;
      InternalSetColCount(Result + FCCount);
      if FDataLink.Active then begin
        UpdateBufferCount;
        RecCount := FDataLink.RecordCount;
        if RecCount<1 then
          RecCount := 1;
      end else begin
        RecCount := 0;
        if FRCount=0 then
          // need to be large enough to hold indicator
          // if there is one, and if there are no titles
          RecCount := FCCount;
      end;
      Inc(RecCount, FRCount);
      RowCount := RecCount;
      FixedRows := FRCount;
      UpdateGridColumnSizes;
      if FDatalink.Active and (FDatalink.ActiveRecord>=0) then
        AdjustEditorBounds(Col, FixedRows + FDatalink.ActiveRecord);
    end;
  finally
    EndUpdate;
  end;
  {$ifdef dbgDBGrid}DebugLnExit('%s.UpdateGridCounts DONE', [ClassName]);{$endif}
end;

constructor TCustomDBGrid.Create(AOwner: TComponent);
begin
  FEditingColumn:=-1;
  DragDx:=5;
  inherited Create(AOwner);

  FDataLink := TComponentDataLink.Create;//(Self);
  FDataLink.OnRecordChanged:=@OnRecordChanged;
  FDataLink.OnDatasetChanged:=@OnDataSetChanged;
  FDataLink.OnDataSetOpen:=@OnDataSetOpen;
  FDataLink.OnDataSetClose:=@OnDataSetClose;
  FDataLink.OnNewDataSet:=@OnNewDataSet;
  FDataLink.OnInvalidDataSet:=@OnInvalidDataset;
  FDataLink.OnInvalidDataSource:=@OnInvalidDataSource;
  FDataLink.OnDataSetScrolled:=@OnDataSetScrolled;
  FDataLink.OnLayoutChanged:=@OnLayoutChanged;
  FDataLink.OnEditingChanged:=@OnEditingChanged;
  FDataLink.OnUpdateData:=@OnUpdateData;
  FDatalink.OnFocusControl := @OnFocusControl;
  FDataLink.VisualControl:= True;

  FSelectedRows := TBookmarkList.Create(Self);

  RenewColWidths;

  FOptions := [dgColumnResize, dgColumnMove, dgTitles, dgIndicator, dgRowLines,
    dgColLines, dgConfirmDelete, dgCancelOnExit, dgTabs, dgEditing,
    dgAlwaysShowSelection];

  inherited Options :=
    [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine,
     goSmoothScroll, goColMoving, goTabs, goEditing, goDrawFocusSelected,
     goColSizing ];

  FExtraOptions := [dgeAutoColumns, dgeCheckboxColumn];

  AutoAdvance := aaRightDown;

  // What a dilema!, we need ssAutoHorizontal and ssVertical!!!
  ScrollBars:=ssBoth;
  AllowOutboundEvents := false;
end;

procedure TCustomDBGrid.AutoAdjustColumns;
begin
  Exclude(FGridStatus, gsAutoSized);
  UpdateAutoSizeColumns;
end;

procedure TCustomDBGrid.InitiateAction;
begin
  {$ifdef dbgDBGrid}DebugLnEnter('%s.InitiateAction INIT', [ClassName]);{$endif}
  inherited InitiateAction;
  if (gsUpdatingData in FGridStatus) then begin
    EndUpdating;
    {
    if EditorMode then begin
      Editor.SetFocus;
      EditorSelectAll;
    end;
    }
  end;
  {$ifdef dbgDBGrid}DebugLnExit('%s.InitiateAction DONE', [ClassName]);{$endif}
end;

procedure TCustomDBGrid.DefaultDrawColumnCell(const Rect: TRect;
  DataCol: Integer; Column: TColumn; State: TGridDrawState);
var
  S: string;
  F: TField;
  DataRow: Integer;
begin
  F := Column.Field;

  DataCol := GridColumnFromColumnIndex(DataCol);
  if FDataLink.Active then
    DataRow := FixedRows + FDataLink.ActiveRecord
  else
    DataRow := 0;

  if DataCol>=FirstGridColumn then
    case ColumnEditorStyle(DataCol, F) of

      cbsCheckBoxColumn:
        DrawCheckBoxBitmaps(DataCol, Rect, F);

      else begin
        if F<>nil then begin
          if CheckDisplayMemo(F) then
            S := F.AsString
          else
          if F.dataType <> ftBlob then
            S := F.DisplayText
          else
            S := '(blob)';
        end else
          S := '';
        DrawCellText(DataCol, DataRow, Rect, State, S);
      end;

    end;
end;

function TCustomDBGrid.EditorByStyle(Style: TColumnButtonStyle): TWinControl;
begin
  // we want override the editor style if it is cbsAuto because
  // field.datatype might be ftBoolean or some other cases
  if Style=cbsAuto then
    Style := ColumnEditorStyle(Col, SelectedField);

  Result:=inherited EditorByStyle(Style);
end;

procedure TCustomDBGrid.ResetColWidths;
begin
  if not FDefaultColWidths then begin
    RenewColWidths;
    LayoutChanged;
  end;
end;

procedure TCustomDBGrid.SelectRecord(AValue: boolean);
begin
  {$ifdef dbgGrid}DebugLn('%s.SelectRecord', [ClassName]); {$endif}
  if dgMultiSelect in Options then
    FSelectedRows.CurrentRowSelected := AValue;
end;

procedure TCustomDBGrid.GetScrollbarParams(out aRange, aPage, aPos: Integer);
begin
  if (FDatalink<>nil) and FDatalink.Active then begin
    if FDatalink.dataset.IsSequenced then begin
      aRange := GetRecordCount + VisibleRowCount - 1;
      aPage := VisibleRowCount;
      if aPage<1 then aPage := 1;
      if FDatalink.BOF then aPos := 0 else
      if FDatalink.EOF then aPos := aRange
      else
      begin
        aPos := FDataLink.DataSet.RecNo - 1; // RecNo is 1 based
        FDataLink.DataSet.UpdateCursorPos; // FPC 3 bug #31532 workaround
      end;
      if aPos<0 then aPos:=0;
      if aRange=0 then aRange:=1; // there's always 1 (new) row
    end else begin
      aRange := 6;
      aPage := 2;
      if FDatalink.EOF then aPos := 4 else
      if FDatalink.BOF then aPos := 0
      else aPos := 2;
    end;
  end else begin
    aRange := 0;
    aPage := 0;
    aPos := 0;
  end;
end;

procedure TCustomDBGrid.CMGetDataLink(var Message: TLMessage);
begin
  Message.Result := PtrUInt(FDataLink);
end;

procedure TCustomDBGrid.ClearSelection(selCurrent:boolean=false);
begin
  if [dgMultiSelect,dgPersistentMultiSelect]*Options=[dgMultiSelect] then begin
    if SelectedRows.Count>0 then
      SelectedRows.Clear;
    if SelCurrent then
      SelectRecord(true);
  end;
  if FKeyBookmark<>nil then begin
    FDatalink.DataSet.FreeBookmark(FKeyBookmark);
    FKeyBookmark:=nil;
  end;
end;

function TCustomDBGrid.NeedAutoSizeColumns: boolean;
begin
  result := (dgAutoSizeColumns in Options)
            //and (HandleAllocated)
            ;
end;

procedure TCustomDBGrid.RenewColWidths;
begin
  FDefaultColWidths := True;
  exclude(FGridStatus, gsAutoSized);
end;

procedure TCustomDBGrid.InternalAutoSizeColumn(aCol: Integer; aCanvas: TCanvas; aDatalinkActive: Boolean);
var
  Field: TField;
  C: TGridColumn;
  ColWidth: Integer;
  ARow,w: Integer;
  s: string;

begin
  Field := GetFieldFromGridColumn(ACol);
  C := ColumnFromGridColumn(ACol);

  if (C<>nil) and (C.Title<>nil) then begin
    aCanvas.Font := C.Title.Font;
    ColWidth := aCanvas.TextWidth(trim(C.Title.Caption));
    aCanvas.Font := C.Font;
  end else begin
    if (Field<>nil) then begin
      aCanvas.Font := TitleFont;
      ColWidth := aCanvas.TextWidth(Field.FieldName);
    end
    else
      ColWidth := 0;
    aCanvas.Font := Font;
  end;

  if (Field<>nil) and aDatalinkActive then
    for ARow := FixedRows to RowCount-1 do begin

      FDatalink.ActiveRecord := ARow - FixedRows;

      if Field.dataType<>ftBlob then
        s := trim(Field.DisplayText)
      else
        s := '(blob)';
      w := aCanvas.TextWidth(s);
      if w>ColWidth then
        ColWidth := w;

    end;

  if ColWidth=0 then
    ColWidth := GetColumnWidth(ACol);

  ColWidths[ACol] := ColWidth + 15;
end;

destructor TCustomDBGrid.Destroy;
begin
  {$ifdef dbgGrid}DebugLn('%s.Destroy', [ClassName]); {$endif}
  FSelectedRows.Free;
  FDataLink.OnDataSetChanged:=nil;
  FDataLink.OnRecordChanged:=nil;
  FDataLink.Free;
  inherited Destroy;
end;

function TCustomDBGrid.MouseToRecordOffset(const x, y: Integer; out
  Column: TColumn; out RecordOffset: Integer): TGridZone;
var
  aCol,aRow: Integer;
begin
  Result := MouseToGridZone(x, y);

  Column := nil;
  RecordOffset := 0;

  if (Result=gzInvalid) or (Result=gzFixedCells) then
    exit;

  MouseToCell(x, y, aCol, aRow);

  if (Result=gzFixedRows) or (Result=gzNormal) then
    RecordOffset := aRow - Row;

  if (Result=gzFixedCols) or (Result=gzNormal) then begin
    aRow := ColumnIndexFromGridColumn(aCol);
    if aRow>=0 then
      Column := Columns[aRow];
  end;
end;

function TCustomDBGrid.ExecuteAction(AAction: TBasicAction): Boolean;
begin
    Result := (DataLink <> nil)
              and DataLink.ExecuteAction(AAction);
end;

function TCustomDBGrid.UpdateAction(AAction: TBasicAction): Boolean;
begin
  Result := (DataLink <> nil)
            and DataLink.UpdateAction(AAction);
end;

procedure TCustomDBGrid.SaveToFile(FileName: string);
begin
  SaveOptions:=[ soDesign ];
  inherited SaveToFile(Filename);
end;

procedure TCustomDBGrid.SaveToStream(AStream: TStream);
begin
  SaveOptions:=[ soDesign ];
  inherited SaveToStream(AStream);
end;

procedure TCustomDBGrid.LoadFromFile(FileName: string);
begin
  SaveOptions:=[ soDesign ];
  Include(FGridStatus, gsLoadingGrid);
  inherited LoadFromFile(Filename);
  Exclude(FGridStatus, gsLoadingGrid);
end;

procedure TCustomDBGrid.LoadFromStream(AStream: TStream);
begin
  SaveOptions:=[ soDesign ];
  Include(FGridStatus, gsLoadingGrid);
  inherited LoadFromStream(AStream);
  Exclude(FGridStatus, gsLoadingGrid);
end;

{ TComponentDataLink }

function TComponentDataLink.GetFields(Index: Integer): TField;
begin
  {$ifdef dbgGrid}DebugLn('%s.GetFields Index=%d',[ClassName, Index]); {$endif}
  if (index>=0) and (index<DataSet.FieldCount) then
    result:=DataSet.Fields[index]
  else
    result:=nil;
end;

function TComponentDataLink.GetDataSetName: string;
begin
  {$ifdef dbgDBGrid}
  DebugLn('%s.GetDataSetName', [ClassName]);
  {$endif}
  Result:=FDataSetName;
  if DataSet<>nil then Result:=DataSet.Name;
end;

procedure TComponentDataLink.SetDataSetName(const AValue: string);
begin
  {$ifdef dbgDBGrid}
  DebugLn('%s.SetDataSetName', [ClassName]);
  {$endif}
  if FDataSetName<>AValue then FDataSetName:=AValue;
end;

procedure TComponentDataLink.RecordChanged(Field: TField);
begin
  {$ifdef dbgDBGrid}
  DebugLn('%s.RecordChanged', [ClassName]);
  {$endif}
  if Assigned(OnRecordChanged) then
    OnRecordChanged(Field);
end;

procedure TComponentDataLink.DataSetChanged;
begin
  {$ifdef dbgDBGrid}
  DebugLn('%s.DataSetChanged FirstRecord=%d', [ClassName, FirstRecord]);
  {$endif}
  if Assigned(OnDataSetChanged) then
    OnDataSetChanged(DataSet);
end;

procedure TComponentDataLink.ActiveChanged;
begin
  {$ifdef dbgDBGrid}
  DebugLnEnter('%s.ActiveChanged INIT', [ClassName]);
  {$endif}
  if Active then begin
    fDataSet := DataSet;
    if DataSetName <> fDataSetName then begin
      fDataSetName := DataSetName;
      if Assigned(fOnNewDataSet) then fOnNewDataSet(DataSet);
    end else
      if Assigned(fOnDataSetOpen) then fOnDataSetOpen(DataSet);
  end else begin
    BufferCount := 0;
    if (DataSource = nil)then begin
      if Assigned(fOnInvalidDataSource) then fOnInvalidDataSource(fDataSet);
      fDataSet := nil;
      fDataSetName := '[???]';
    end else begin
      if (DataSet=nil)or(csDestroying in DataSet.ComponentState) then begin
        if Assigned(fOnInvalidDataSet) then fOnInvalidDataSet(fDataSet);
        fDataSet := nil;
        fDataSetName := '[???]';
      end else begin
        if Assigned(FOnDataSetClose) then begin
          FOnDataSetClose(DataSet);
          {$ifdef dbgDBGrid} DebugLn('%s.ActiveChanged OnDataSetClose Called', [ClassName]); {$endif}
        end;
        if DataSet <> nil then FDataSetName := DataSetName;
      end;
    end;
  end;
  {$ifdef dbgDBGrid}
  DebugLnExit('%s.ActiveChanged DONE', [ClassName]);
  {$endif}
end;

procedure TComponentDataLink.LayoutChanged;
begin
  {$ifdef dbgDBGrid}
  DebugLnEnter('%s.LayoutChanged INIT', [ClassName]);
  {$endif}
  if Assigned(OnLayoutChanged) then
    OnLayoutChanged(DataSet);
  {$ifdef dbgDBGrid}
  DebugLnExit('%s.LayoutChanged DONE', [ClassName]);
  {$endif}
end;

procedure TComponentDataLink.DataSetScrolled(Distance: Integer);
begin
  {$ifdef dbgDBGrid}
  DebugLn('%s.DataSetScrolled Distance=%d',[ClassName, Distance]);
  {$endif}
  if Assigned(OnDataSetScrolled) then
    OnDataSetScrolled(DataSet, Distance);
end;

procedure TComponentDataLink.FocusControl(Field: TFieldRef);
begin
  {$ifdef dbgDBGrid}
  DebugLn('%s.FocusControl', [ClassName]);
  {$endif}
  if Assigned(OnFocusControl) then
    OnFocusControl(Field);
end;

procedure TComponentDataLink.CheckBrowseMode;
begin
  {$ifdef dbgDBGrid}
  DebugLn('%s.CheckBrowseMode', [ClassName]);
  {$endif}
  inherited CheckBrowseMode;
end;

procedure TComponentDataLink.EditingChanged;
begin
  {$ifdef dbgDBGrid}
  DebugLn('%s.EditingChanged', [ClassName]);
  {$endif}
  if Assigned(OnEditingChanged) then
    OnEditingChanged(DataSet);
end;

procedure TComponentDataLink.UpdateData;
begin
  {$ifdef dbgDBGrid}
  DebugLn('%s.UpdateData', [ClassName]);
  {$endif}
  if Assigned(OnUpdatedata) then
    OnUpdateData(DataSet);
end;

function TComponentDataLink.MoveBy(Distance: Integer): Integer;
begin
  (*
  {$ifdef dbgDBGrid}
  DebugLnEnter('%s.MoveBy INIT Distance=%d',[ClassName, Distance]);
  {$endif}
  *)
  Result:=inherited MoveBy(Distance);
  (*
  {$ifdef dbgDBGrid}
  DebugLnExit('%s.MoveBy DONE Result=%d',[ClassName, Result]);
  {$endif}
  *)
end;

{ TDBGridColumns }

function TDBGridColumns.GetColumn(Index: Integer): TColumn;
begin
  result := TColumn( inherited Items[Index] );
end;

procedure TDBGridColumns.SetColumn(Index: Integer; Value: TColumn);
begin
  Items[Index].Assign( Value );
end;

procedure TDBGridColumns.Update(Item: TCollectionItem);
begin
  if (Grid<>nil) and not (csLoading in Grid.ComponentState) then
    TCustomDBGrid(Grid).LayoutChanged;
end;

function TDBGridColumns.ColumnFromField(Field: TField): TColumn;
var
  i: Integer;
begin
  if Field<>nil then
  for i:=0 to Count-1 do begin
    result := Items[i];
    if (result<>nil)and(result.Field=Field) then
      exit;
  end;
  result:=nil;
end;

function TDBGridColumns.HasAutomaticColumns: boolean;
var
  i: Integer;
begin
  Result := False;
  for i:=0 to Count-1 do
    if Items[i].IsAutomaticColumn then begin
      Result := true;
      break;
    end;
end;

function TDBGridColumns.HasDesignColumns: boolean;
var
  i: Integer;
begin
  Result := False;
  for i:=0 to Count-1 do
    if Items[i].IsDesignColumn then begin
      Result := true;
      break;
    end;
end;

procedure TDBGridColumns.RemoveAutoColumns;
var
  i: Integer;
  G: TCustomDBGrid;
begin
  if HasAutomaticColumns then begin
    G := TCustomDBGrid(Grid);
    G.GridStatus := G.GridStatus + [gsRemovingAutoColumns];
    BeginUpdate;
    try
      for i:=Count-1 downto 0 do
        if Items[i].IsAutomaticColumn then
          Delete(i);
    finally
      EndUpdate;
      G.GridStatus := G.GridStatus - [gsRemovingAutoColumns];
    end;
  end;
end;

function CompareFieldIndex(P1,P2:Pointer): integer;
begin
  if P1=P2 then
    Result := 0
  else if (P1=nil) or (TColumn(P1).Field=nil) then
    Result := 1
  else if (P2=nil) or (TColumn(P2).Field=nil) then
    Result := -1
  else
    Result := TColumn(P1).Field.Index - TColumn(P2).Field.Index;
end;

function CompareDesignIndex(P1,P2:Pointer): integer;
begin
  result := TColumn(P1).DesignIndex - TColumn(P2).DesignIndex;
end;

procedure TDBGridColumns.ResetColumnsOrder(ColumnOrder: TColumnOrder);
var
  L: TList;
  i: Integer;
begin
  L := TList.Create;
  try

    for i:=0 to Count-1 do
      L.Add(Items[i]);

    case ColumnOrder of
      coDesignOrder:
        begin
          if not HasDesignColumns then
            exit;
          L.Sort(@CompareDesignIndex)
        end;
      coFieldIndexOrder:
        L.Sort(@CompareFieldIndex);
      else
        exit;
    end;

    for i:=0 to L.Count-1 do
      TColumn(L.Items[i]).Index := i;

  finally
    L.Free;
  end;
end;

function TDBGridColumns.Add: TColumn;
var
  G: TCustomDBGrid;
begin
  {$ifdef dbgDBGrid}
  DebugLn('%s.Add', [ClassName]);
  {$endif}
  G := TCustomDBGrid(Grid);
  if G<>nil then begin
    // remove automatic columns before adding user columns
    if not (gsAddingAutoColumns in G.GridStatus) then
      RemoveAutoColumns;
  end;
  result := TColumn( inherited add );
end;

procedure TDBGridColumns.LinkFields;
var
  i: Integer;
  G: TCustomDBGrid;
begin
  G := TCustomDBGrid(Grid);
  if G<>nil then
    G.BeginLayout;
  for i:=0 to Count-1 do
    Items[i].LinkField;
  if G<>nil then
    G.EndLayout;
end;

{ TColumn }

function TColumn.GetField: TField;
begin
  if (FFieldName<>'') and (FField<>nil) then
    LinkField;
  result := FField;
end;

function TColumn.GetIsDesignColumn: boolean;
begin
  result := (DesignIndex>=0) and (DesignIndex<10000);
end;

function TColumn.GetPickList: TStrings;
begin
  Result := inherited GetPickList;
  if (Field<>nil) and (FField.FieldKind=fkLookup) then
  begin
    if FField.LookupCache then
      FField.LookupList.ValuesToStrings(Result)
    else
    begin
      Result.Clear;
      LookupGetBookMark(FField);
      try
      with FField.LookupDataSet do
      begin
        First;
        while not EOF do
        begin
          Result.Add(FieldbyName(FField.LookupResultField).AsString);
          Next;
        end;
      end;
      finally
        LookupGotoBookMark(FField);
      end;
    end;
  end;
end;

procedure TColumn.ApplyDisplayFormat;
begin
  if (FField <> nil) and FDisplayFormatChanged then begin
    if (FField is TNumericField) then
      TNumericField(FField).DisplayFormat := DisplayFormat
    else if (FField is TDateTimeField) then
      TDateTimeField(FField).DisplayFormat := DisplayFormat;
  end;
end;

function TColumn.GetDisplayFormat: string;
begin
  if not FDisplayFormatChanged then
    Result := GetDefaultDisplayFormat
  else
    result := FDisplayFormat;
end;

function TColumn.IsDisplayFormatStored: boolean;
begin
  Result := FDisplayFormatChanged;
end;

procedure TColumn.SetDisplayFormat(const AValue: string);
begin
  if (not FDisplayFormatChanged)or(CompareText(AValue, FDisplayFormat)<>0) then begin
    FDisplayFormat := AValue;
    FDisplayFormatChanged:=True;
    ColumnChanged;
  end;
end;

procedure TColumn.SetField(const AValue: TField);
begin
  if FField <> AValue then begin
    FField := AValue;
    if FField<>nil then
      FFieldName := FField.FieldName;
    ColumnChanged;
  end;
end;

procedure TColumn.SetFieldName(const AValue: String);
begin
  if FFieldName=AValue then exit;
  FFieldName:=AValue;
  LinkField;
  ColumnChanged;
end;

function TColumn.GetDataSet: TDataSet;
var
  AGrid: TCustomDBGrid;
begin
  AGrid := TCustomDBGrid(Grid);
  if (AGrid<>nil) then
    result := AGrid.FDataLink.DataSet
  else
    result :=nil;
end;

procedure TColumn.Assign(Source: TPersistent);
begin
  if Source is TColumn then begin
    //DebugLn('Assigning TColumn[',dbgs(Index),'] a TColumn')
    Collection.BeginUpdate;
    try
      inherited Assign(Source);
      FieldName := TColumn(Source).FieldName;
      DisplayFormat := TColumn(Source).DisplayFormat;
      ValueChecked := TColumn(Source).ValueChecked;
      ValueUnchecked := TColumn(Source).ValueUnchecked;
    finally
      Collection.EndUpdate;
    end;
  end else
    inherited Assign(Source);
end;

function TColumn.GetDefaultWidth: Integer;
var
  AGrid: TCustomDBGrid;
  tmpCanvas: TCanvas;
begin
  AGrid := TCustomDBGrid(Grid);
  if AGrid<>nil then begin

    tmpCanvas := GetWorkingCanvas(aGrid.Canvas);
    tmpCanvas.Font := aGrid.Font;

    if FField<>nil then
      result := CalcColumnFieldWidth(
        tmpCanvas,
        dgTitles in aGrid.Options,
        Title.Caption,
        Title.Font,
        FField)
    else
      result := AGrid.DefaultColWidth;

    if tmpCanvas<>AGrid.Canvas then
      FreeWorkingCanvas(tmpCanvas);

  end else
    result := -1;
end;

function TColumn.CreateTitle: TGridColumnTitle;
begin
  Result := TColumnTitle.Create(Self);
end;

constructor TColumn.Create(ACollection: TCollection);
var
  AGrid: TCustomGrid;
begin
  {$ifdef dbgDBGrid}
  DebugLn('%s.Create', [ClassName]);
  {$endif}
  inherited Create(ACollection);
  if ACollection is TDBGridColumns then begin
    AGrid := TDBGridColumns(ACollection).Grid;
    if (AGrid<>nil) and (csLoading in AGrid.ComponentState) then
      FDesignIndex := Index
    else
      FDesignIndex := 10000;
  end;
end;

function TColumn.IsDefault: boolean;
begin
  result := not FDisplayFormatChanged and (inherited IsDefault());
end;

procedure TColumn.LinkField;
var
  AGrid: TCustomDBGrid;
begin
  AGrid:= TCustomDBGrid(Grid);
  if (AGrid<>nil) and AGrid.FDatalink.Active then begin
    Field := AGrid.FDataLink.DataSet.FindField(FFieldName);
    ApplyDisplayFormat;
  end else
    Field := nil;
end;

function TColumn.GetDefaultDisplayFormat: string;
begin
  Result := '';
  if FField<>nil then begin
    if FField is TNumericField then
      Result := TNumericField(FField).DisplayFormat
    else if FField is TDateTimeField then
      Result := TDateTimeField(FField).DisplayFormat
  end;
end;

function TColumn.GetDefaultValueChecked: string;
begin
  if (FField<>nil) and (FField.Datatype=ftBoolean) then
    Result := BoolToStr(True)
  else
    Result := '1';
end;

function TColumn.GetDefaultValueUnchecked: string;
begin
  if (FField<>nil) and (FField.DataType=ftBoolean) then
    Result := BoolToStr(False)
  else
    Result := '0';
end;

function TColumn.GetDefaultReadOnly: boolean;
var
  AGrid: TCustomDBGrid;
begin
  AGrid := TCustomDBGrid(Grid);
  Result := ((AGrid<>nil)and(AGrid.ReadOnly)) or ((FField<>nil)And(FField.ReadOnly))
end;

function TColumn.GetDefaultVisible: boolean;
begin
  if FField<>nil then
    result := FField.Visible
  else
    result := True;
end;

function TColumn.GetDisplayName: string;
begin
  if FFieldName<>'' then
    Result:=FFieldName
  else
    Result:=inherited GetDisplayName;
end;

function TColumn.GetDefaultAlignment: TAlignment;
var
  Bs: set of TColumnButtonStyle;
begin
  bs := [buttonStyle];
  if Grid<>nil then
    Include(bs, TCustomDbGrid(Grid).DefaultEditorStyle(ButtonStyle, FField));
  if bs*[cbsCheckboxColumn,cbsButtonColumn]<>[] then
    result := taCenter
  else
  if FField<>nil then
    result := FField.Alignment
  else
    Result := taLeftJustify;
end;

{ TColumnTitle }

function TColumnTitle.GetDefaultCaption: string;
begin
  with (Column as TColumn) do begin
    if FieldName<>'' then begin
      if FField<>nil then
        Result := FField.DisplayName
      else
        Result := Fieldname;
    end else
      Result := inherited GetDefaultCaption;
  end;
end;

{ TBookmarkList }

function TBookmarkList.GetCount: integer;
begin
  {$ifdef dbgDBGrid}
  DebugLn('%s.GetCount FList.Count=%d',[ClassName, FList.Count]);
  {$endif}
  result := FList.Count;
end;

function TBookmarkList.GetCurrentRowSelected: boolean;
var
  Bookmark: TBookmark;
begin
  CheckActive;
  Bookmark := FDataset.GetBookmark;
  Result := IndexOf(Bookmark)>=0;
  FDataset.FreeBookmark(Bookmark);
end;

function TBookmarkList.GetItem(AIndex: Integer): TBookmark;
begin
  Result := TBookmark(FList[AIndex]);
end;

procedure TBookmarkList.SetCurrentRowSelected(const AValue: boolean);
var
  Bookmark: pointer;
  Index: Integer;
begin
  CheckActive;

  Bookmark := nil;
  TBookmark(Bookmark) := FDataset.GetBookmark; // fetch and increase reference count
  if Bookmark = nil then
    Exit;

  if Find(Bookmark, Index) then begin
    FDataset.FreeBookmark(Bookmark);
    {$ifndef noautomatedbookmark}
    SetLength(TBookmark(Bookmark),0); // decrease reference count
    {$endif noautomatedbookmark}
    if not AValue then begin
      FDataset.FreeBookmark(Pointer(Items[Index]));
      {$ifndef noautomatedbookmark}
      Bookmark := FList[Index];
      SetLength(TBookmark(Bookmark),0); // decrease reference count
      {$endif noautomatedbookmark}
      FList.Delete(Index);
      FGrid.Invalidate;
    end;
  end else begin
    if AValue then begin
      // the reference count of Bookmark was increased above, so it is save to
      // store it here as pointer
      FList.Insert(Index, Bookmark);
      FGrid.Invalidate;
    end else
      FDataset.FreeBookmark(Bookmark);
  end;
end;

procedure TBookmarkList.CheckActive;
begin
  {$ifdef dbgDBGrid}
  DebugLn('%s.CheckActive', [ClassName]);
  {$endif}
  if not FGrid.FDataLink.Active then
    raise EInvalidGridOperation.Create('Dataset Inactive');

  if FGrid.DataSource.DataSet=FDataset then
    exit;
  FDataset := FGrid.DataSource.DataSet;

  // Note.
  //
  // Some dataset descendants do not implement CompareBookmarks, for these we
  // use MyCompareBookmarks in the hope the allocated bookmark memory is used
  // to hold some kind of record index.
  FUseCompareBookmarks := TMethod(@FDataset.CompareBookmarks).Code<>pointer(@TDataset.CompareBookmarks);

  // Note.
  //
  // fpc help say CompareBookmarks should return -1, 0 or 1 ... which imply that
  // bookmarks should be a sorted array (or list). In this scenario binary search
  // is the prefered method for finding a bookmark.
  //
  // The problem here is that TBufDataset and TSQLQuery (and thus TCustomSQLQuery
  // and TCustomBufDataset) CompareBookmarks only return 0 or -1 (some kind of
  // is this a valid bookmark or not), the result is that it appears as an unsorted
  // list (or array) and binary search should not be used.
  //
  // The weird thing is that if we use MyCompareBookmarks which deals with comparing
  // the memory reserved for bookmarks in the hope bookmarks are just some kind of
  // reocord indexes, currently work fine for TCustomBufDataset derived datasets.
  // however using CompareBookmarks is always the right thing to use where implemented.
  //
  // As Dbgrid should be TDataset implementation agnostic this is a way I found
  // to know if the dataset is derived from TCustomBufDataset or not.
  // Once TCustomBufDataset is fixed, remove this ugly note & hack.
  case FDataset.ClassName of
    'TSQLQuery','TBufDataset','TCustomSQLQuery','TCustomBufDataset':
      FCanDoBinarySearch := false;
    else
      FCanDoBinarySearch := true;
  end;
end;

function TBookmarkList.GetEnumerator(opt: TBookmarkedRecordEnumeratorOptions
  ): TBookmarkedRecordEnumerator;
begin
  result := TBookmarkedRecordEnumerator.Create(self, fGrid, opt);
end;

constructor TBookmarkList.Create(AGrid: TCustomDbGrid);
begin
  inherited Create;
  FGrid := AGrid;
  FList := TFPList.Create;
end;

destructor TBookmarkList.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TBookmarkList.Clear;
var
  i: Integer;
  {$ifndef noautomatedbookmark}
  Bookmark: Pointer;
  {$endif}
begin
  for i:=0 to FList.Count-1 do
  begin
    {$ifdef dbgDBGrid}
    DebugLn('%s.Clear', [ClassName]);
    {$endif}
    FDataset.FreeBookmark(Items[i]);
    {$ifndef noautomatedbookmark}
    Bookmark := FList[i];
    SetLength(TBookmark(Bookmark),0); // decrease reference count
    {$endif noautomatedbookmark}
  end;
  FList.Clear;
  FGrid.Invalidate;
end;

procedure TBookmarkList.Delete;
var
  i: Integer;
  {$ifndef noautomatedbookmark}
  Bookmark: Pointer;
  {$endif}
begin
  {$ifdef dbgDBGrid}
  DebugLn('%s.Delete', [ClassName]);
  {$endif}
  for i := FList.Count-1 downto 0 do begin
    FDataset.GotoBookmark(Items[i]);
    {$ifndef noautomatedbookmark}
    Bookmark := FList[i];
    SetLength(TBookmark(Bookmark),0); // decrease reference count
    {$else}
    FDataset.FreeBookmark(Items[i]);
    {$endif noautomatedbookmark}
    FDataset.Delete;
    FList.Delete(i);
  end;
end;

type
  TDs=class(TDataset)
  end;

function MyCompareBookmarks(ds:Tdataset; b1,b2:pointer): Integer;
begin
  if b1=b2 then
    result := 0
  else
  if b1=nil then
    result := 1
  else
  if b2=nil then
    result := -1
  else begin
    // Note: Tds(ds).bookmarksize is set at creation of TDataSet and does not change
    result := CompareMemRange(b1,b2,Tds(ds).bookmarksize);
  end;
end;

function TBookmarkList.Find(const Item: TBookmark; var AIndex: Integer): boolean;
var
  L, R, I: Integer;
  CompareRes: Integer;

  procedure BinarySearch;
  begin
    L := 0;
    R := FList.Count - 1;
    while (L <= R) do
    begin
      I := L + (R - L) div 2;
      if FUseCompareBookmarks then
        CompareRes := FDataset.CompareBookmarks(Item, TBookmark(FList[I]))
      else
        CompareRes := MyCompareBookmarks(FDataset, pointer(Item), FList[I]);
      if (CompareRes > 0) then
        L := I + 1
      else
      begin
        R := I - 1;
        if (CompareRes = 0) then
        begin
           Result := True;
           L := I;
        end;
      end;
    end;
    AIndex := L;
  end;

  procedure VisitAll;
  begin
    AIndex := 0;
    i := 0;
    while i<FList.Count do begin
      if FUseCompareBookmarks then
        CompareRes := FDataset.CompareBookmarks(Item, TBookmark(FList[I]))
      else
        CompareRes := MyCompareBookmarks(FDataset, pointer(Item), FList[I]);
      if CompareRes=0 then begin
        result := true;
        AIndex := i;
        exit;
      end;
      inc(i);
    end;
  end;

begin
  {$ifdef dbgDBGrid}
  DebugLn('%s.Find', [ClassName]);
  {$endif}

  Result := False;
  if Item=nil then
    Exit;
  if FCanDoBinarySearch then
    BinarySearch
  else
    VisitAll;
end;

function TBookmarkList.IndexOf(const Item: TBookmark): Integer;
begin
  {$ifdef dbgDBGrid}
  DebugLn('%s.IndexOf', [ClassName]);
  {$endif}
  if not Find(Item, Result) then
    Result := -1;
end;

function TBookmarkList.Refresh: boolean;
var
  i: LongInt;
  {$ifndef noautomatedbookmark}
  Bookmark: Pointer;
  {$endif}
begin
  {$ifdef dbgDBGrid}
  DebugLn('%s.Refresh', [ClassName]);
  {$endif}
  Result := False;
  for i := FList.Count - 1 downto 0 do
    if not FDataset.BookmarkValid(TBookMark(Items[i])) then begin
      Result := True;
      FDataset.FreeBookmark(Items[i]);
      {$ifndef noautomatedbookmark}
      Bookmark := FList[i];
      SetLength(TBookmark(Bookmark),0); // decrease reference count
      {$endif noautomatedbookmark}
      Flist.Delete(i);
    end;
  if Result then
    FGrid.Invalidate;
end;

end.
