{

TCustomGrid, TDrawGrid and TStringGrid for Lazarus
Copyright (C) 2002  Jesus Reyes Aguilar.
email: jesusrmx@yahoo.com.mx

THIS CONTROL IS FREEWARE - USE AS YOU WILL

if you release sourcecode that uses this control, please credit me
or leave this header intact.  if you release a compiled application
that uses this code, please credit me somewhere in a little bitty
location so I can at least get bragging rights!
(Extract: from Tony's checkbook tracker, http://tony.maro.net)

This code is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

Cur version: 0.8.4
The log was moved to end of file, search for: The_Log

}

unit Grids;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLStrConsts, LCLProc, LCLType, LCLIntf, Controls,
  GraphType, Graphics, Forms, VCLGlobals, DynamicArray, LMessages, Messages,
  XMLCfg, StdCtrls, LResources, MaskEdit;

const
  //GRIDFILEVERSION = 1; // Original
  //GRIDFILEVERSION = 2; // Introduced goSmoothScroll
  GRIDFILEVERSION = 3; // Introduced Col/Row FixedAttr and NormalAttr
  

const
  GM_SETVALUE   = LM_USER + 100;
  GM_GETVALUE   = LM_USER + 101;
  GM_SETGRID    = LM_USER + 102;
  GM_SETPOS     = LM_USER + 103;
  GM_SELECTALL  = LM_USER + 104;

const
  CA_LEFT     =   $1;
  CA_CENTER   =   $2;
  CA_RIGHT    =   $4;
  CL_TOP      =   $8;
  CL_CENTER   =   $10;
  CL_BOTTOM   =   $20;
  
const
  EO_AUTOSIZE   =   $1;
  EO_HOOKKEYS   =   $2;
  EO_HOOKEXIT   =   $4;
  EO_SELECTALL  =   $8;
  EO_WANTCHAR   =   $10;
  
type
  EGridException = class(Exception);

type
  TGridOption = (
    goFixedVertLine,      // Ya
    goFixedHorzLine,      // Ya
    goVertLine,           // Ya
    goHorzLine,           // Ya
    goRangeSelect,        // Ya
    goDrawFocusSelected,  // Ya
    goRowSizing,          // Ya
    goColSizing,          // Ya
    goRowMoving,          // Ya
    goColMoving,          // Ya
    goEditing,            // Ya
    goTabs,               // Ya
    goRowSelect,          // Ya
    goAlwaysShowEditor,   // Ya
    goThumbTracking,      // ya
    // Additional Options
    goColSpanning,        // Enable cellextent calcs
    goRelaxedRowSelect,   // User can see focused cell on goRowSelect
    goDblClickAutoSize,   // dblclicking columns borders (on hdrs) resize col.
    goSmoothScroll        // Switch scrolling mode (pixel scroll is by default)
  );
  TGridSaveOptions = (
    soDesign,
    soAttributes,
    soContent,
    soPosition
  );

  TGridOptions = set of TGridOption;
  TGridDrawState = set of (gdSelected, gdFocused, gdFixed);
  TGridState =
    (gsNormal, gsSelecting, gsRowSizing, gsColSizing,gsRowMoving,gsColMoving);
  TGridZone = (gzNormal, gzFixedCols, gzFixedRows, gzFixedCells);
  TSaveOptions = Set of TGridSaveOptions;
  
  TUpdateOption = (uoNone, uoQuick, uoFull);
  TAutoAdvance = (aaDown,aaRight);
  
  TGridStatus = (stNormal, stEditorHiding, stEditorShowing, stFocusing);
  

type
  PCellFontData=^TCellFontData;
  TCellFontData=record
    Pitch: TFontPitch;
    Styles: TFontStyles;
    Size: Integer;
    CharSet: TFontCharSet;
    Face: PChar;
  end;

  PCellAttr=^TCellAttr;
  TCellAttr=record
    Color: TColor;
    FontColor: TColor;
    FontData: PCellFontData;
    TextStyle: TTextStyle;
  end;
      
  PCellProps= ^TCellProps;
  TCellProps=record
    Attr: PCellAttr;
    Data: TObject;
    Text: pchar;
  end;
  
  PColRowProps= ^TColRowProps;
  TColRowProps=record
    Size: Integer;
    FixedAttr: PCellAttr;
    NormalAttr: PCellAttr;
  end;
  
  
type
  TCustomGrid = class;
  
  PGridMessage=^TGridMessage;
  TGridMessage=record
    MsgID: Cardinal;
    Grid: TCustomGrid;
    Col,Row: Integer;
    Value: string;
    CellRect: TRect;
    Options: Integer;
  end;

  { Default cell editor for TStringGrid }
  TStringCellEditor=class(TCustomEdit)
  private
    FGrid: TCustomGrid;
  protected
    //procedure WndProc(var TheMessage : TLMessage); override;
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
    procedure msg_SetValue(var Msg: TGridMessage); Message GM_SETVALUE;
    procedure msg_GetValue(var Msg: TGridMessage); Message GM_GETVALUE;
    procedure msg_SetGrid(var Msg: TGridMessage); Message GM_SETGRID;
    procedure msg_SelectAll(var Msg: TGridMessage); MEssage GM_SELECTALL;
  end;
  
  TOnDrawCell =
    procedure(Sender: TObject; Col, Row: Integer; aRect: TRect;
              aState:TGridDrawState) of Object;
              
  TOnCanSelectEvent =
    procedure(Sender: TObject; Col, Row: Integer;
              var CanSelect: Boolean) of Object;
  TOnSelectEvent =
    procedure(Sender: TObject; Col,Row: Integer) of Object;
    
  TOnCellAttrEvent =
    procedure(Sender:TObject; const Col, Row:Integer; State: TGridDrawState;
              var Attr: TCellAttr) of Object;
  TGridOperationEvent =
    procedure (Sender: TObject; IsColumn:Boolean;
               sIndex,tIndex: Integer) of object;
  THdrEvent =
    procedure(Sender: TObject; IsColumn: Boolean; Index: Integer) of Object;
    
  TOnCompareCells =
    function (Sender: TObject; Acol,ARow,Bcol,BRow: Integer): Integer of Object;
    
  TSelectEditorEvent =
    procedure(Sender: TObject; Col,Row: Integer;
              var Editor: TWinControl) of Object;

  TVirtualGrid=class
    private
      FColCount: Integer;
      FRowCount: Integer;
      FCells, FCols, FRows: TArray;
      function GetCells(Col, Row: Integer): PCellProps;
      function Getrows(Row: Integer): PColRowprops;
      function Getcols(Col: Integer): PColRowprops;
      procedure SetCells(Col, Row: Integer; const AValue: PCellProps);
      procedure Setrows(Row: Integer; const Avalue: PColRowprops);
      procedure Setcolcount(const Avalue: Integer);
      procedure Setrowcount(const Avalue: Integer);
      procedure Setcols(Col: Integer; const Avalue: PColRowprops);
    protected
      function GetDefaultCell: PcellProps;
      function GetDefaultColRow: PColRowProps;
      procedure doDestroyItem(Sender: TObject; Col,Row:Integer; var Item: Pointer);
      procedure doNewItem(Sender: TObject; Col,Row:Integer; var Item: Pointer);
      procedure DeleteColRow(IsColumn: Boolean; Index: Integer);
      procedure MoveColRow(IsColumn: Boolean; FromIndex, ToIndex: Integer);
      procedure ExchangeColRow(IsColumn:Boolean; Index,WithIndex: Integer);
      procedure DisposeCell(Var P: PCellProps);
      procedure DisposeColRow(var p: PColRowProps);
    public
      constructor Create;
      destructor Destroy; override;
      procedure Clear;
      
      property ColCount: Integer read FColCount write SetColCount;
      Property RowCount: Integer read FRowCount write SetRowCount;
      
      property Celda[Col,Row: Integer]: PCellProps read GetCells write SetCells;
      Property Cols[Col: Integer]: PColRowProps read GetCols write SetCols;
      property Rows[Row: Integer]: PColRowProps read GetRows write SetRows;
  end;

  type
    TGridCoord = {$IFNDEF NEW1_1}type{$ENDIF} TPoint;
    TGridRect  = {$IFNDEF NEW1_1}type{$ENDIF} TRect;

    TGridDataCache=record
      FixedWidth: Integer;    // Sum( Fixed ColsWidths[i] )
      FixedHeight: Integer;   // Sum( Fixed RowsHeights[i] )
      GridWidth: Integer;     // Sum( ColWidths[i] )
      GridHeight: Integer;    // Sum( RowHeights[i] )
      ClientWidth: Integer;   // Width-VertScrollbar.Size
      ClientHeight: Integer;  // Height-HorzScrollbar.Size
      ScrollWidth: Integer;   // ClientWidth-FixedWidth
      ScrollHeight: Integer;  // ClientHeight-FixedHeight
      VisibleGrid: TRect;     // Visible non fixed rectagle of cells
      MaxClientXY: Tpoint;    // VisibleGrid.BottomRight (pixel) coordinates
      ValidGrid: Boolean;     // true if there is something to show
      AccumWidth: TList;       // Accumulated width per column
      AccumHeight: TList;     // Accumulated Height per row
      HScrDiv,VScrDiv: Double;      // Transform const for ThumbTracking
      TLColOff,TLRowOff: Integer;   // TopLeft Offset in pixels
    end;

type
  //TCustomGrid=class(TScrollBox)
  //TCustomGrid=class(TCustomControl)
  TCustomGrid=class(TScrollingWinControl)
  private
    FAutoAdvance: TAutoAdvance;
    FDefaultDrawing: Boolean;
    FEditor: TWinControl;
    FOnCompareCells: TOnCompareCells;
    FGridLineStyle: TPenStyle;
    FGridLineWidth: Integer;
    FDefColWidth, FDefRowHeight: Integer;
    FCol,FRow, FFixedCols, FFixedRows: Integer;
    FOnSelectEditor: TSelectEditorEvent;
    FGridLineColor: TColor;
    
    FFocusColor: TColor;
    FCols,FRows: TList;
    
    FsaveOptions: TSaveOptions;
    FScrollBars: TScrollStyle;
    FSelectActive: Boolean;
    FTopLeft: TPoint;
    FSplitter, FPivot: TPoint;
    FRange: TRect;
    FDragDx: Integer;
    FMoveLast: TPoint;
    
    FUpdateCount: Integer;
    FUpdateScrollBarsCount: Integer;

    
    // Cached Values
    FGCache: TGridDataCache;
    // Options
    FOptions: TGridOptions;

    FOnDrawCell: TOnDrawcell;
    FOnCanSelect: TOnCanSelectEvent;
    FOnBeforeSelection: TOnSelectEvent;
    FOnSelection: TOnSelectEvent;
    FOnTopLeftChange: TNotifyEvent;
    FSkipUnselectable: Boolean;
    
    procedure AdjustCount(IsColumn:Boolean; OldValue, NewValue:Integer);
    procedure CheckFixedCount(aCol,aRow,aFCol,aFRow: Integer);
    procedure CacheVisibleGrid;
    function doColSizing(X,Y: Integer): Boolean;
    function doRowSizing(X,Y: Integer): Boolean;
    procedure doColMoving(X,Y: Integer);
    procedure doRowMoving(X,Y: Integer);
    procedure doTopleftChange(DimChg: Boolean);

    function OffsetToColRow(IsCol,Fisical:Boolean; Offset:Integer; var Rest:Integer): Integer;
    function ColRowToOffset(IsCol,Fisical:Boolean; Index: Integer; var Ini,Fin:Integer): Boolean;

    function GetLeftCol: Integer;
    function GetTopRow: Longint;
    function GetVisibleColCount: Integer;
    function GetVisibleRowCount: Integer;
    function GetColCount: Integer;
    function GetRowCount: Integer;
    function GetRowHeights(Arow: Integer): Integer;
    function GetSelection: TGridRect;
    function GetColWidths(Acol: Integer): Integer;
    function GetVisibleGrid: TRect;
    procedure MyTextRect(R: TRect; Offx,Offy:Integer; S:string; Ts: TTextStyle);
    function ScrollToCell(const aCol,aRow: Integer): Boolean;
    function ScrollGrid(Relative:Boolean; DCol,DRow: Integer): TPoint;
    procedure SetDefaultDrawing(const AValue: Boolean);
    procedure SetEditor(AValue: TWinControl);
    procedure SetFocusColor(const AValue: TColor);
    procedure SetGridLineStyle(const AValue: TPenStyle);
    procedure SetSelectActive(const AValue: Boolean);
    procedure SetSelection(const AValue: TGridRect);
    procedure SetFixedCols(const AValue: Integer);
    procedure SetFixedRows(const AValue: Integer);
    procedure SetGridLineColor(const AValue: TColor);
    procedure SetGridLineWidth(const AValue: Integer);
    procedure SetLeftCol(const AValue: Integer);
    procedure SetOptions(const AValue: TGridOptions);
    procedure SetScrollBars(const AValue: TScrollStyle);
    procedure SetTopRow(const AValue: Integer);
    procedure Setrowheights(Arow: Integer; Avalue: Integer);
    procedure Setcolwidths(Acol: Integer; Avalue: Integer);
    procedure SetColCount(Valor: Integer);
    procedure SetRowCount(Valor: Integer);
    procedure SetDefColWidth(Valor: Integer);
    procedure SetDefRowHeight(Valor: Integer);
    procedure SetCol(Valor: Integer);
    procedure SetRow(Valor: Integer);
    procedure TryScrollTo(aCol,aRow: integer);
    procedure UpdateScrollBarPos(Which: TControlScrollbar);
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure WMChar(var Message: TLMChar); message LM_CHAR;
    
    
  protected
    fGridState: TGridState;
    
    procedure WndProc(var TheMessage : TLMessage); override;
    
    procedure AutoAdjustColumn(aCol: Integer); virtual;
    function  CellRect(ACol, ARow: Integer): TRect;
    
    procedure ColRowDeleted(IsColumn: Boolean; Index: Integer); Dynamic;
    procedure ColRowExchanged(IsColumn: Boolean; Index,WithIndex: Integer); Dynamic;
    procedure ColRowMoved(IsColumn: Boolean; FromIndex,ToIndex: Integer); Dynamic;

    procedure ColWidthsChanged; dynamic;
    procedure DblClick; override;
    procedure DestroyHandle; override;
    procedure doExit; override;
    procedure doEnter; override;

    procedure DrawEdges;
    procedure DrawBackGround; virtual;
    //procedure DrawFixedCells; virtual;
    //procedure DrawInteriorCells; virtual;
    procedure DrawFocused; virtual;
    procedure DrawFocusRect(aCol,aRow:Integer; ARect:TRect; aState:TGridDrawstate); virtual;
    procedure DrawColRowMoving;
    procedure DrawCell(aCol,aRow:Integer; aRect:TRect; aState:TGridDrawState); virtual;
    
    procedure DrawByRows; virtual;
    procedure DrawRow(aRow: Integer); virtual;

    procedure HeaderClick(IsColumn: Boolean; Index: Integer); Dynamic;
    procedure InvalidateCol(ACol: Integer);
    procedure InvalidateRow(ARow: Integer);
    procedure InvalidateCell(aCol, aRow: Integer); overload;
    procedure InvalidateCell(aCol, aRow: Integer; Redraw: Boolean); overload;
    procedure InvalidateGrid;
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
    procedure KeyUp(var Key : Word; Shift : TShiftState); override;
    procedure LoadContent(cfg: TXMLConfig; Version: Integer); virtual;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer);override;
    procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    function  MoveExtend(Relative: Boolean; DCol, DRow: Integer): Boolean;
    function  MoveNextSelectable(Relative:Boolean; DCol, DRow: Integer): Boolean;
    function  TryMoveSelection(Relative: Boolean; var DCol, DRow: Integer): Boolean;
    procedure ProcessEditor(LastEditor:TWinControl; DCol,DRow: Integer; WasVis: Boolean);


    procedure BeforeMoveSelection(const DCol,DRow: Integer); virtual;
    procedure MoveSelection; virtual;
    function  CanSelect(const DCol,DRow: Integer): Boolean;
    procedure DrawCellGrid(Rect: TRect; aCol,aRow: Integer; astate: TGridDrawState);
    procedure Paint; override;
    procedure ResetOffset(chkCol, ChkRow: Boolean);
    procedure RowHeightsChanged; dynamic;
    function  SelectCell(ACol, ARow: Integer): Boolean; virtual;
    procedure SizeChanged(OldColCount, OldRowCount: Integer); dynamic;
    procedure Sort(ColSorting: Boolean; Index,IndxFrom,IndxTo:Integer); virtual;
    procedure TopLeftChanged; dynamic;
    procedure SaveContent(cfg: TXMLConfig); virtual;
    procedure VisualChange; virtual;

    procedure WMHScroll(var Message : TLMHScroll); message LM_HScroll;
    procedure WMVScroll(var Message : TLMVScroll); message LM_VScroll;
    
    // Editor support
  private
    FEditorHiding: Boolean;
    FEditorShowing: Boolean;
    FEditorKey: Boolean;
    FEditorOptions: Integer;
    procedure EditorGetValue;
    procedure EditorHide;
    procedure EditorSetValue;
    procedure EditorShow;
    procedure EditorPos;
    procedure EditorReset;
    procedure EditorSelectAll;
    procedure EditorShowChar(Ch: Char);
    function ShouldEdit: Boolean;
  protected
    procedure EditorCancel; virtual;
    procedure doEditorGetValue; virtual;
    procedure doEditorSetValue; virtual;
    procedure SelectEditor; virtual;
  public
    procedure EditorExit(Sender: TObject);
    procedure EditorKeyDown(Sender: TObject; var Key:Word; Shift:TShiftState);

  protected
    Property AutoAdvance: TAutoAdvance read FAutoAdvance write FAutoAdvance default aaRight;
    Property ColWidths[aCol: Integer]: Integer read GetColWidths write SetColWidths;
    Property ColCount: Integer read GetColCount write SetColCount;
    Property Col: Integer read FCol write SetCol;
    Property DefaultColWidth: Integer read FDefColWidth write SetDefColWidth;
    Property DefaultRowHeight: Integer read FDefRowHeight write SetDefRowHeight;
    property DefaultDrawing: Boolean read FDefaultDrawing write SetDefaultDrawing default True;
    Property DragDx: Integer read FDragDx write FDragDx;
    Property Editor: TWinControl read FEditor write SetEditor;
    Property FixedCols: Integer read FFixedCols write SetFixedCols;
    Property FixedRows: Integer read FFixedRows write SetFixedRows;
    Property FocusColor: TColor read FFocusColor write SetFocusColor;
    Property GridLineColor: TColor read FGridLineColor write SetGridLineColor;
    Property GridLineStyle: TPenStyle read FGridLineStyle write SetGridLineStyle;
    property GridWidth: Integer read FGCache.GridWidth;
    property GridHeight: Integer read FGCache.GridHeight;
    property GridLineWidth: Integer read FGridLineWidth write SetGridLineWidth default 1;
    property LeftCol:Integer read GetLeftCol write SetLeftCol;
    property Options: TGridOptions read FOptions write SetOptions;
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars;
    Property SkipUnselectable: Boolean read FSkipUnselectable write FSkipUnselectable;
    Property RowCount: Integer read GetRowCount write SetRowCount;
    Property Row: Integer read FRow write SetRow;
    Property SaveOptions: TSaveOptions read FsaveOptions write FSaveOptions;
    Property SelectActive: Boolean read FSelectActive write SetSelectActive;
    property Selection: TGridRect read GetSelection write SetSelection;
    property TopRow: Integer read GetTopRow write SetTopRow;
    Property RowHeights[aRow: Integer]: Integer read GetRowHeights write SetRowHeights;
    property VisibleColCount: Integer read GetVisibleColCount;
    property VisibleRowCount: Integer read GetVisibleRowCount;

    Property OnDrawCell: TOnDrawCell read FOnDrawCell write FOnDrawCell;
    Property OnCanSelect: TOnCanSelectEvent read fOnCanSelect write fOnCanSelect;
    Property OnBeforeSelection: TOnSelectEvent read FOnBeforeSelection write FOnBeforeSelection;
    
    Property OnSelection: TOnSelectEvent read fOnSelection write fOnSelection;
    Property OnTopLeftChange: TNotifyEvent read FOnTopLeftChange write FOnTopLeftChange;
    Property OnCompareCells: TOnCompareCells read FOnCompareCells write FOnCompareCells;
    Property OnSelectEditor: TSelectEditorEvent read FOnSelectEditor write FOnSelectEditor;
    Property GCache: TGridDataCache read FGCAChe;
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    procedure Invalidate; override;
    
    { Exposed procs }
    procedure DeleteColRow(IsColumn: Boolean; Index: Integer);
    procedure ExchangeColRow(IsColumn: Boolean; Index, WithIndex: Integer);
    procedure MoveColRow(IsColumn: Boolean; FromIndex, ToIndex: Integer);
    procedure SortColRow(IsColumn: Boolean; Index:Integer); overload;
    procedure SortColRow(IsColumn: Boolean; Index,FromIndex,ToIndex: Integer); overload;
    
    procedure BeginUpdate;
    procedure AutoAdjustColumns;
    procedure Clear;
    procedure EndUpdate(UO: TUpdateOption); overload;
    procedure EndUpdate(FullUpdate: Boolean); overload;
    procedure LoadFromFile(FileName: string);
    procedure SaveToFile(FileName: string);

    function ColRowToClientCellRect(aCol, aRow: Integer): TRect;
    function MouseToCell(Mouse: TPoint): TPoint;
    function MouseToLogcell(Mouse: TPoint): TPoint;
    function MouseToGridZone(X,Y: Integer; CellCoords: Boolean): TGridZone;
    function IsCellVisible(aCol, aRow: Integer): Boolean;
    function IscellSelected(aCol,aRow: Integer): Boolean;
  end;
      
  TTextStyleAdapter=class
  private
    FTextStyle: TTextStyle;
  protected
    property TextStyle: TTextStyle read FTextStyle write FTextStyle;
  public
    property Alignment: TAlignment read FTextStyle.Alignment write FTextStyle.Alignment;
    property Layout: TTextLayout read FTextStyle.Layout write FTextStyle.Layout;
    property WordBreak: Boolean read FTextStyle.WordBreak write FTextStyle.WordBreak;
    property Clipping: Boolean read FTextStyle.Clipping write FTextStyle.Clipping;
    property ExpandTabs: Boolean read FTextStyle.ExpandTabs write FTextStyle.ExpandTabs;
    property ShowPrefix: Boolean read FTextStyle.ShowPrefix write FTextStyle.ShowPrefix;
    property Opaque: Boolean read FTextStyle.Opaque write FTextStyle.Opaque;
    property SystemFont: Boolean read FTextStyle.SystemFont write FTextStyle.SystemFont;
  end;
  
  TGridPropertyAdapter=class
  private
    FGrid: TCustomGrid;
    FFont: TFont;
    FColor: TColor;
    FAlign: Integer;
    FTextStyleAdapter: TTextStyleAdapter;

    procedure setAlign(const AValue: Integer);
    procedure setColor(const AValue: TColor);
    procedure setFont(const AValue: TFont);
    Procedure OnFontChange(Sender: TObject);
  protected
    procedure setAttr(Attr: TCellAttr);
    property Grid: TCustomGrid read FGrid write FGrid;
  public
    constructor create;
    destructor destroy; override;
    
    property Color: TColor read FColor write setColor;
    property Font: TFont read FFont write setFont;
    property Align: Integer read FAlign write setAlign;
  end;


  TDrawGrid=class(TCustomGrid)
  private
    FCellAttr: TCellAttr; // Attibute used to render Cell
    FOnColRowDeleted: TgridOperationEvent;
    FOnColRowExchanged: TgridOperationEvent;
    FOnColRowMoved: TgridOperationEvent;
    FOnHeaderClick: THdrEvent;
    FGrid: TVirtualGrid;
    FDefCellAttr, FdefSelCellAttr, FdefFixedCellAttr: TCellAttr;
    FOnCellAttr: TOnCellAttrEvent;


    function GetCellAlign(ACol, ARow: Integer): Integer;
    function GetCellAttr(ACol, ARow: Integer): TCellAttr;
    function GetCellColor(ACol, ARow: Integer): TColor;
    function GetCellFontCOlor(ACol, ARow: Integer): TColor;
    function GetColAlign(aCol: Integer): Integer;
    function GetColAttr(aCol: Integer): TCellAttr;
    function GetColColor(aCol: Integer): TColor;
    function GetColFontColor(aCol: Integer): TColor;
    function GetFixedColor: TColor;
    function GetRowAlign(aRow: Integer): Integer;
    function GetRowAttr(aRow: Integer): TCellAttr;
    function GetRowColor(aRow: Integer): TColor;
    function GetRowFontColor(aRow: Integer): TColor;
    function getFixedColAlign(aCol: Integer): Integer;
    function getFixedColAttr(aCol: Integer): TCellAttr;
    function getfixedColColor(aCol: Integer): TColor;
    function getFixedColFontColor(aCol: Integer): TColor;
    function getFixedRowAlign(aRow: Integer): Integer;
    function getFixedRowAttr(aRow: Integer): TCellAttr;
    function getfixedRowColor(aRow: Integer): TColor;
    function getFixedRowFontColor(aRow: Integer): TColor;
    procedure SetCellAlign(ACol, ARow: Integer; const AValue: Integer);
    procedure SetCellAttr(ACol, ARow: Integer; const AValue: TCellAttr);
    procedure SetCellColor(ACol, ARow: Integer; const AValue: TColor);
    procedure SetCellFontCOlor(ACol, ARow: Integer; const AValue: TColor);
    procedure SetColAlign(aCol: Integer; const AValue: Integer);
    procedure SetColAttr(aCol: Integer; const AValue: TCellAttr);
    procedure SetColColor(aCol: Integer; const AValue: TColor);
    procedure SetColFontColor(aCol: Integer; const AValue: TColor);
    procedure SetDefaultCellAttr(const AValue: TCellAttr);
    procedure SetFixedColor(const AValue: TColor);
    procedure SetRowAlign(aRow: Integer; const AValue: Integer);
    procedure SetRowAttr(aRow: Integer; const AValue: TCellAttr);
    procedure SetRowColor(aRow: Integer; const AValue: TColor);
    procedure SetRowFontColor(aRow: Integer; const AValue: TColor);
    procedure setFixedColAlign(aCol: Integer; const AValue: Integer);
    procedure setFixedColAttr(aCol: Integer; const AValue: TCellAttr);
    procedure setfixedColColor(aCol: Integer; const AValue: TColor);
    procedure setFixedcolFontColor(aCol: Integer; const AValue: TColor);
    procedure setFixedRowAlign(aRow: Integer; const AValue: Integer);
    procedure setFixedRowAttr(aRow: Integer; const AValue: TCellAttr);
    procedure setFixedRowColor(aRow: Integer; const AValue: TColor);
    procedure setFixedRowFontColor(aRow: Integer; const AValue: TColor);
    procedure SetDefFixedCellAttr(const AValue: TCellAttr);
    procedure SetDefSelCellAttr(const AValue: TCellAttr);

  protected
    procedure CalcCellExtent(acol, aRow: Integer; var aRect: TRect); virtual;
    procedure DrawCell(aCol,aRow: Integer; aRect: TRect; aState:TGridDrawState); override;
    procedure DrawFocusRect(aCol,aRow: Integer; ARect: TRect; aState: TGridDrawstate); override;
    procedure ColRowExchanged(IsColumn: Boolean; Index,WithIndex: Integer); override;
    procedure ColRowDeleted(IsColumn: Boolean; Index: Integer); override;
    procedure ColRowMoved(IsColumn: Boolean; FromIndex,ToIndex: Integer); override;
    procedure HeaderClick(IsColumn: Boolean; Index: Integer); override;
    procedure SizeChanged(OldColCount, OldRowCount: Integer); override;
    procedure SetColor(Value : TColor); override;
    procedure SaveContent(cfg: TXMLConfig); override;
    procedure LoadContent(Cfg: TXMLConfig; Version:Integer); override;
    
  public
    // to easy user call
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    procedure DefaultDrawCell(aCol,aRow: Integer; var aRect: TRect; aState:TGridDrawState);
    
    procedure RemoveCellAttr(aCol,aRow: Integer);
    procedure RemoveColRowAttr(Index: Integer; IsCol,IsFixed: Boolean);

    property Canvas;


    property ColAttr[aCol: Integer]: TCellAttr read GetColAttr write SetColAttr;
    Property ColColor[aCol: Integer]: TColor read GetColColor write SetColColor;
    Property ColFontColor[aCol: Integer]: TColor read GetColFontColor write SetColFontColor;
    Property ColAlign[aCol: Integer]: Integer read GetColAlign write SetColAlign;

    property RowAttr[aRow: Integer]: TCellAttr read GetRowAttr write SetRowAttr;
    property RowColor[aRow: Integer]: TColor read GetRowColor write SetRowColor;
    property RowFontColor[aRow: Integer]: TColor read GetRowFontColor write SetRowFontColor;
    property RowAlign[aRow: Integer]: Integer read GetRowAlign write SetRowAlign;

    property CellAttr[ACol,ARow:Integer]: TCellAttr read GetCellAttr write SetCellAttr;
    property CellColor[ACol, ARow:Integer]: TColor read GetCellColor write SetCellColor;
    Property CellFontColor[ACol,ARow:Integer]: TColor read GetCellFontColor write SetCellFontCOlor;
    Property CellAlign[ACol,ARow: Integer]: Integer read GetCellAlign write SetCellAlign;
    
    property DefaultCellAttr: TCellAttr read fDefCellAttr write SetDefaultCellAttr;
    property SelectedcellAttr: TCellAttr read FDefSelCellAttr write SetDefSelCellAttr;
    property FixedCellAttr: TCellAttr read FDefFixedCellAttr write SetDefFixedCellAttr;

    property FixedColAttr[aCol: Integer]: TCellAttr read getFixedColAttr write setFixedColAttr;
    property FixedColColor[aCol: Integer]: TColor read getFixedColColor write setFixedColColor;
    property FixedColFontColor[aCol: Integer]: TColor read getFixedColFontColor write setFixedcolFontColor;
    property FixedColAlign[aCol: Integer]: Integer read getFixedColAlign write setFixedColAlign;
    
    property FixedRowAttr[aRow: Integer]: TCellAttr read getFixedRowAttr write setFixedRowAttr;
    property FixedRowColor[aRow: Integer]: TColor read getFixedRowColor write setFixedRowColor;
    property FixedRowFontColor[aRow: Integer]: TColor read getFixedRowFontColor write setFixedRowFontColor;
    property FixedRowAlign[aRow: Integer]: Integer read getFixedRowAlign write setFixedRowAlign;

    property Editor;

    property Col;
    property ColWidths;
    //property EditorMode;
    property GridHeight;
    property GridWidth;
    property LeftCol;
    property Selection;
    property Row;
    property RowHeights;
    Property GridLineColor;
    Property GridLineStyle;
    Property FocusColor;
    Property SaveOptions;
    Property SkipUnselectable;
    //property TabStops;
    property TopRow;
  Published
    property Align;
    property Anchors;
    Property AutoAdvance;
    //property BiDiMode;
    //property BorderStyle;
    property Color default clWindow;
    property ColCount;
    //property Constraints;
    property Ctl3D;
    property DefaultColWidth;
    property DefaultRowHeight;
    property DefaultDrawing;
    //property DragCursor;
    //property DragKind;
    //property DragMode;
    property Enabled;
    property FixedColor: TColor read GetFixedColor write SetFixedColor default clBtnFace;
    property FixedCols;
    property RowCount;
    property FixedRows;
    property Font;
    property GridLineWidth;
    property Options;
    //property ParentBiDiMode;
    //property ParentColor;
    property ParentCtl3D;
    //property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property VisibleColCount;
    property VisibleRowCount;


    Property OnBeforeSelection;
    Property OnCanSelect;
    Property OnCellAttr: TOnCellAttrEvent read fonCellAttr write fOnCellAttr;
    Property OnCompareCells;
    Property OnColRowDeleted: TgridOperationEvent read FOnColRowDeleted write FOnColRowDeleted;
    Property OnColRowExchanged: TgridOperationEvent read FOnColRowExchanged write FOnColRowExchanged;
    Property OnColRowMoved: TgridOperationEvent read FOnColRowMoved write FOnColRowMoved;
    Property OnDrawCell;
    Property OnHeaderClick: THdrEvent read FOnHeaderClick write FOnHeaderClick;
    Property OnSelectEditor;
    Property OnSelection;
    Property OnTopleftChange;
  end;
  
  TStringGrid = class(TDrawGrid)
    private
      FDefEditor: TStringCellEditor;
      function GetCells(ACol, ARow: Integer): string;
      function GetCols(Index: Integer): TStrings;
      function GetObjects(ACol, ARow: Integer): TObject;
      function GetRows(Index: Integer): TStrings;
      procedure SetCells(ACol, ARow: Integer; const AValue: string);
      procedure SetCols(Index: Integer; const AValue: TStrings);
      procedure SetObjects(ACol, ARow: Integer; AValue: TObject);
      procedure SetRows(Index: Integer; const AValue: TStrings);
    protected
      procedure AutoAdjustColumn(aCol: Integer); override;
      procedure CalcCellExtent(acol, aRow: Integer; var aRect: TRect); override;
      procedure DrawCell(aCol,aRow: Integer; aRect: TRect; aState:TGridDrawState); override;
      procedure doEditorGetValue; override;
      procedure doEditorSetValue; override;
      procedure SaveContent(cfg: TXMLConfig); override;
      procedure LoadContent(cfg: TXMLConfig; Version: Integer); override;
      //procedure DrawInteriorCells; override;
      procedure SelectEditor; override;
    public
      Constructor Create(AOWner: TComponent); override;
      Destructor Destroy; override;
      property Cells[ACol, ARow: Integer]: string read GetCells write SetCells;
      property Cols[Index: Integer]: TStrings read GetCols write SetCols;
      property Objects[ACol, ARow: Integer]: TObject read GetObjects write SetObjects;
      property Rows[Index: Integer]: TStrings read GetRows write SetRows;
  end;
  
  procedure DebugRect(S:string; R:TRect);
  procedure DebugPoint(S:string; P:TPoint);
      
  procedure CellAlignToAttr(Align: Integer; var Attr: TCellAttr);
  procedure AttrToCellAlign(Attr: TCellAttr; var Align: Integer);

procedure Register;

implementation

{
// Dibujar una linea en el borde izquierdo de esta celda
Dc:=GetDC(handle);
Pen:=CreatePen(PS_SOLID, 3, clRed);
OldPen:=SelectObject(Dc, Pen);
MoveToEx(Dc, R.left, 0, nil);
LineTo(Dc, R.Left, FGCache.MaxClientXY.Y);
SelectObject(Dc, OldPen);
DeleteObject(Pen);
ReleaseDC(Handle, Dc);
FMoveLast:=P;
}

{function RndStr:string;
var
  i: Integer;
begin
  Result:='';
  For i:=1 to 10 do begin
    Result:=Result+ Char(Ord('A')+Random(20));
  end;
end;}
function PointIgual(const P1,P2: TPoint): Boolean;
begin
  result:=(P1.X=P2.X)and(P1.Y=P2.Y);
end;
{function RectIgual(const R1,R2: TRect): Boolean;
begin
  Result:=CompareMem(@R1,@R2, SizeOf(R1));
end;}
function Min(const I,J: Integer): Integer;
begin
  if I<J then Result:=I

  else        Result:=J;
end;
function Max(const I,J: Integer): Integer;
begin
  if I>J then Result:=I

  else        Result:=J;
end;
function NormalizarRect(const R:TRect): TRect;
begin
  Result.Left:=Min(R.Left, R.Right);

  Result.Top:=Min(R.Top, R.Bottom);
  Result.Right:=Max(R.Left, R.Right);
  Result.Bottom:=Max(R.Top, R.Bottom);
end;

procedure SwapInt(var I1,I2: Integer);
var
  Tmp: Integer;
begin
  Tmp:=I1;
  I1:=I2;
  I2:=Tmp;
end;

function GetDefaultCellAttr: TCellAttr;
begin
  with Result do begin
    FontColor:=clBlack;
    Color:=clWindow;
    FontData:=nil;
    with TextStyle do begin
      Alignment:=taLeftJustify;
      Layout:=tlCenter;
      SingleLine:=False;
      WordBreak:=False;
      Opaque:=False;
      Clipping:=False;
    end;
  end;
end;

{procedure DebugAttr(Msg: string; Attr: TCellAttr);
begin
  with Attr do begin
    WriteLn(Msg);
    WriteLn('Color=',ColorToString(Attr.Color));
    WriteLn('FontColor=',ColorToString(Attr.FontColor));
    with TextStyle do begin
      WriteLn('Textstyle.Alignment=', Ord(Alignment));
      WriteLn('TextStyle.Layout=',Ord(Layout));
      WriteLn('TextStyle.SingleLine=',Singleline);
      WriteLn('TextStyle.Clipping=',Clipping);
      WriteLn('TextStyle.Wordbreak=',WordBreak);
      WriteLn('TextStyle.Opaque=',Opaque);
      WriteLn('TextStyle.SystemFont',systemFont);
    end;
  end;
end;}

function LoadCellAttrFromXMLPath(Cfg: TXMLConfig; Path: string): TCellAttr;
begin
  Result:=GetDefaultCellAttr;

  with Result do begin
    Color:=StringToColor(Cfg.GetValue(Path+'/color', ColorToString(Color)));
    FontColor:=
      StringToColor(Cfg.GetValue(Path+'/fontcolor',ColorToString(FontColor)));
    with TextStyle do begin
      Alignment:=
        TAlignment(cfg.GetValue(Path+'/textstyle/alignment/value',
          Integer(Alignment)));
      Layout:=
        TTextLayout(    cfg.GetValue(Path+'/textstyle/layout/value',
          Integer(layout)));
      SingleLine:= cfg.GetValue(Path+'/textstyle/SingleLine/value',SingleLine);
      Clipping:= cfg.GetValue(Path+'/textstyle/clipping/value',Clipping);
      WordBreak:= cfg.GetValue(Path+'/textStyle/wordbreak/value',WordBreak);
      Opaque:= cfg.GetValue(Path+'/textstyle/opaque/value',Opaque);
      SystemFont:= cfg.GetValue(Path+'/textstyle/systemfont/value',SystemFont);
    end;
  end;
end;

function CellAttrIgual(const Ca1,Ca2: TCellAttr): Boolean;
begin
  Result:=CompareMem(@Ca1,@Ca2,SizeOf(Ca1));
end;

procedure CellAlignToAttr(Align: Integer; var Attr: TCellAttr);
begin
  with Attr.TextStyle do begin
    if Align and CA_LEFT    = CA_LEFT     then Alignment:=taLeftJustify else
    if Align and CA_CENTER  = CA_CENTER   then Alignment:=taCenter
    else                                       Alignment:=taRightJustify;
    if Align and CL_TOP     = CL_TOP      then Layout:=tlTop else
    if Align AND CL_CENTER  = CL_CENTER   then Layout:=tlCenter
    else                                       Layout:=tlBottom;
  end;
end;

procedure AttrToCellAlign(Attr: TCellAttr; var Align: Integer);
begin
  with Attr.TextStyle do begin
    Align:=0;
    case Alignment of
      taCenter: Align:=CA_CENTER;
      taRightJustify: Align:=CA_RIGHT;
      else Align:=CA_LEFT;
    end;
    case Layout of
      tlTop: Align:=Align or CL_TOP;
      tlBottom: Align:=Align or CL_BOTTOM;
      else Align:=ALign or CL_CENTER;
    end;
  end;
end;

procedure DisposeCellAttr(Attr: PCellAttr);
begin
  If Attr^.FontData<>nil Then begin
    if attr^.Fontdata^.Face<>nil then
      StrDispose(Attr^.FontData^.Face);
    Dispose(Attr^.FontData);
  End;
  Dispose(Attr);
end;

{ TCustomGrid }

function TCustomGrid.Getrowheights(Arow: Integer): Integer;
begin
  Result:=Integer(FRows[aRow]);
  if Result<0 then Result:=fDefRowHeight;
end;

function TCustomGrid.GetTopRow: Longint;
begin
  Result:=fTopLeft.y;
end;

function TCustomGrid.GetVisibleColCount: Integer;
var
  R: TRect;
begin
  R:=FGCache.VisibleGrid;
  Result:=r.Right-r.left+1;//+FFixedCols;
end;

function TCustomGrid.GetVisibleRowCount: Integer;
var
  R: TRect;
begin
  R:=FGCache.VisibleGrid;
  Result:=r.bottom-r.top+1;//+FFixedRows;
end;

function TCustomGrid.GetLeftCol: Integer;
begin
  result:=fTopLeft.x;
end;

function TCustomGrid.Getcolcount: Integer;
begin
  Result:=FCols.Count;
end;

function TCustomGrid.Getrowcount: Integer;
begin
  Result:=FRows.Count;
end;

function TCustomGrid.Getcolwidths(Acol: Integer): Integer;
begin
  Result:=Integer(FCols[aCol]);
  if result<0 then Result:=fDefColWidth;
end;

procedure TCustomGrid.SetEditor(AValue: TWinControl);
var
  Msg: TGridMessage;
begin
  if FEditor=AValue then exit;
  FEditor:=AValue;
  if FEditor<>nil then begin

    if FEditor.Parent=nil then FEditor.Visible:=False;
    if FEditor.Parent<>Self then FEditor.Parent:=Self;
    FEditor.TabStop:=False;
    
    Msg.MsgID:=GM_SETGRID;
    Msg.Grid:=Self;
    Msg.Options:=0;
    FEditor.Dispatch(Msg);
    FEditorOptions:=Msg.Options;

    if Msg.Options and EO_HOOKKEYS = EO_HOOKKEYS then begin
      FEditor.OnKeyDown:=@EditorKeyDown;
    end;
    
    if Msg.Options and EO_HOOKEXIT = EO_HOOKEXIT then begin
      FEditor.OnExit:=@EditorExit;
    end;
    
    {$IfDef EditorDbg}
    write('SetEditor-> Editor=',FEditor.Name,' ');
    if FEditorOptions and EO_AUTOSIZE = EO_AUTOSIZE then write('EO_AUTOSIZE ');
    if FEditorOptions and EO_HOOKKEYS = EO_HOOKKEYS then write('EO_HOOKKEYS ');
    if FEditorOptions and EO_HOOKEXIT = EO_HOOKEXIT then write('EO_HOOKEXIT ');
    if FEditorOptions and EO_SELECTALL= EO_SELECTALL then write('EO_SELECTALL ');
    if FEditorOptions and EO_WANTCHAR = EO_WANTCHAR then write('EO_WANTCHAR ');
    WriteLn;
    {$Endif}
  end;
end;

procedure TCustomGrid.SetFixedCols(const AValue: Integer);
begin
  if FFixedCols=AValue then exit;
  CheckFixedCount(ColCount, RowCount, AValue, FFixedRows);
  FFixedCols:=AValue;
  fTopLeft.x:=AValue;
  fCol:=Avalue;
  doTopleftChange(true);
end;

procedure TCustomGrid.SetFixedRows(const AValue: Integer);
begin
  if FFixedRows=AValue then exit;
  CheckFixedCount(ColCount, RowCount, FFixedCols, AValue);
  FFixedRows:=AValue;
  fTopLeft.y:=AValue;
  FRow:=AValue;
  doTopleftChange(true);
end;

procedure TCustomGrid.SetGridLineColor(const AValue: TColor);
begin
  if FGridLineColor=AValue then exit;
  FGridLineColor:=AValue;
  Invalidate;
end;

procedure TCustomGrid.SetLeftCol(const AValue: Integer);
begin
  TryScrollTo(AValue, FTopLeft.Y);
end;

procedure TCustomGrid.SetOptions(const AValue: TGridOptions);
begin
  if FOptions=AValue then exit;
  FOptions:=AValue;
  if goRowSelect in Options then begin
    FRange:=Rect(FFixedCols, FRow, ColCount-1, FRow);
    FOptions:=FOptions - [goAlwaysShowEditor];
  end
  else                           FRange:=Rect(FCol,FRow,FCol,FRow);
  if goAlwaysShowEditor in Options then begin
    EditorShow;
  end else begin
    EditorHide;
  end;
  VisualChange;
end;

procedure TCustomGrid.SetScrollBars(const AValue: TScrollStyle);
begin
  if FScrollBars=AValue then exit;
  FScrollBars:=AValue;
  VisualChange;
end;

procedure TCustomGrid.SetTopRow(const AValue: Integer);
begin
  TryScrollTo(FTopLeft.X, Avalue);
end;

procedure TCustomGrid.Setrowheights(Arow: Integer; Avalue: Integer);
begin
  if AValue<0 then AValue:=-1;
  if AValue<>Integer(FRows[ARow]) then begin
    FRows[ARow]:=Pointer(AValue);
    VisualChange;
    if (FEditor<>nil)and(Feditor.Visible)and(ARow<=FRow) then EditorPos;
    RowHeightsChanged;
  end;
end;

procedure TCustomGrid.Setcolwidths(Acol: Integer; Avalue: Integer);
begin
  if AValue<0 then Avalue:=-1;
  if Avalue<>Integer(FCols[ACol]) then begin
    FCols[ACol]:=Pointer(AValue);
    VisualChange;
    if (FEditor<>nil)and(Feditor.Visible)and(ACol<=FCol) then EditorPos;
    ColWidthsChanged;
  end;
end;

procedure TCustomGrid.AdjustCount(IsColumn: Boolean; OldValue, newValue: Integer);
  procedure AddDel(Lst: TList; aCount: Integer);
  begin
    while lst.Count<aCount do Lst.Add(Pointer(-1)); // default width/height
    Lst.Count:=aCount;
  end;
  
var
  OldCount: integer;
begin
  if IsColumn then begin
    AddDel(FCols, NewValue);
    FGCache.AccumWidth.Count:=NewValue;
    OldCount:=RowCount;
    if (OldValue=0)and(NewValue>=0) then begin
      FTopLeft.X:=FFixedCols;
      if RowCount=0 then begin
        FFixedRows:=0;
        FTopLeft.Y:=0;
        AddDel(FRows, 1); FGCache.AccumHeight.Count:=1;
      end;
    end;
    SizeChanged(OldValue, OldCount);
  end else begin
    AddDel(FRows, NewValue);
    FGCache.AccumHeight.Count:=NewValue;
    OldCount:=ColCount;
    if (OldValue=0)and(NewValue>=0) then begin
      FTopleft.Y:=FFixedRows;
      if FCols.Count=0 then begin
        FFixedCols:=0;
        FTopLeft.X:=0;
        AddDel(FCols, 1); FGCache.AccumWidth.Count:=1;
      end;
    end;
    SizeChanged(OldCount, OldValue);
  end;
  VisualChange;
end;

procedure TCustomGrid.SetColCount(Valor: Integer);
var
  OldC: Integer;
begin
  if Valor=FCols.Count then Exit;
  if Valor<1 then
    Clear
  else begin
    OldC:=FCols.Count;
    CheckFixedCount(Valor, RowCount, FFixedCols, FFixedRows);
    AdjustCount(True, OldC, Valor);
  end;
end;

procedure TCustomGrid.SetRowCount(Valor: Integer);
var
  OldR: Integer;
begin
  if Valor=FRows.Count then Exit;
  if Valor<1 then
    clear
  else begin
    OldR:=FRows.Count;
    CheckFixedCount(ColCount, Valor, FFixedCols, FFixedRows);
    AdjustCount(False, OldR, Valor);
  end;
end;

procedure TCustomGrid.SetDefColWidth(Valor: Integer);
begin
  if Valor=fDefColwidth then Exit;
  FDefColWidth:=Valor;
  VisualChange;
end;

procedure TCustomGrid.SetDefRowHeight(Valor: Integer);
begin
  if Valor=fDefRowHeight then Exit;
  FDefRowheight:=Valor;
  VisualChange;
end;

procedure TCustomGrid.SetCol(Valor: Integer);
begin
  if Valor=FCol then Exit;
  MoveExtend(False, Valor, FRow);
end;

procedure TCustomGrid.SetRow(Valor: Integer);
begin
  if Valor=FRow then Exit;
  MoveExtend(False, FCol, Valor);
end;

procedure TCustomGrid.Sort(ColSorting: Boolean; Index, IndxFrom, IndxTo: Integer);
  procedure QuickSort(L,R: Integer);
  var
    i,j: Integer;
    P{,Q}: Integer;
  begin
    repeat
      I:=L;
      J:=R;
      P:=(L+R)Div 2;
      repeat
        if ColSorting then begin
          while OnCompareCells(Self, Index, P, Index, i)>0 do I:=I+1;
          while OnCompareCells(Self, Index, P, Index, j)<0 do J:=J-1;
        end else begin
          while OnCompareCells(Self, P, Index, i, Index)>0 do I:=I+1;
          while OnCompareCells(Self, P, Index, j, Index)<0 do J:=J-1;
        end;
        if I<=J then begin
          ExchangeColRow(not ColSorting, i,j);
          I:=I+1;
          J:=j-1;
        end;
      until I>J;
      if L<J then QuickSort(L,J);
      L:=I;
    until I>=R;
  end;
begin
  BeginUpdate;
  QuickSort(IndxFrom, IndxTo);
  EndUpdate(True);
end;

procedure TCustomGrid.doTopleftChange(dimChg: Boolean);
begin
  TopLeftChanged;
  if dimchg then begin
    VisualChange;
  end else begin
    CacheVisibleGrid;
    Invalidate;
  end;
  UpdateScrollBarPos(nil);
end;

procedure TCustomGrid.VisualChange;
var
  Tw,Th: Integer;
  Dh,DV: Integer;
  
  function MaxTopLeft: TPoint;
  var
    i: Integer;
    W,H: Integer;
  begin
    Result:=Point(ColCount-1, RowCount-1);
    W:=0;
    For i:=ColCount-1 downTo FFixedCols Do begin
      W:=W+GetColWidths(i);
      if W<FGCache.ScrollWidth then Result.x:=i
      else         Break;
    end;
    H:=0;
    For i:=RowCount-1 downto FFixedRows do begin
      H:=H+GetRowHeights(i);
      if H<FGCache.ScrollHeight then Result.y:=i
      else         Break;
    end;
  end;
var
  Mtl: TPoint;
  {$Ifdef TestSbars} vs,hs: Boolean; {$Endif}
begin
  // Calculate New Cached Values
  FGCache.GridWidth:=0;
  FGCache.FixedWidth:=0;
  For Tw:=0 To ColCount-1 do begin
    FGCache.AccumWidth[Tw]:=Pointer(FGCache.GridWidth);
    FGCache.GridWidth:=FGCache.GridWidth + GetColWidths(Tw);
    if Tw<FixedCols then FGCache.FixedWidth:=FGCache.GridWidth;
    {$IfDef dbgScroll}
    WriteLn('FGCache.AccumWidth[',Tw,']=',Integer(FGCache.AccumWidth[Tw]));
    {$Endif}
  end;
  FGCache.Gridheight:=0;
  FGCache.FixedHeight:=0;
  For Tw:=0 To RowCount-1 do begin
    FGCache.AccumHeight[Tw]:=Pointer(FGCache.Gridheight);
    FGCache.Gridheight:=FGCache.Gridheight+GetRowHeights(Tw);
    if Tw<FixedRows then FGCache.FixedHeight:=FGCache.GridHeight;
    {$IfDef dbgScroll}
    WriteLn('FGCache.AccumHeight[',Tw,']=',Integer(FGCache.AccumHeight[Tw]));
    {$Endif}
  end;
  
  Dh:=18{GetSystemMetrics(SM_CYHSCROLL)};
  DV:=18{GetSystemMetrics(SM_CXVSCROLL)};
  TW:=FGCache.GridWidth;
  TH:=FGCache.GridHeight;

  if not(goSmoothScroll in Options) then begin
    FGCache.TLColOff:=0;
    FGCache.TLRowOff:=0;
  end;

  {$Ifdef TestSBars}
  vs:=VertScrollBar.Visible;
  hs:=HorzScrollBar.Visible;
  {$Endif}

  HorzScrollBar.Visible:=
     (FScrollbars in [ssHorizontal, ssBoth]) or
    ((FScrollbars in [ssAutoHorizontal,ssAutoBoth]) and (TW>Width-Dv));
    
  VertScrollBar.Visible:=
     (FScrollbars in [ssVertical, ssBoth]) or
    ((FScrollbars in [ssAutoVertical, ssAutoBoth]) and (TH>height-Dh));
  
  if not HorzScrollBar.Visible then DH:=0;
  if not VertScrollBar.Visible then DV:=0;
  
  {$IfDef TestSBars}
  if (vs<VertScrollBar.Visible) then
    WriteLn('Vertical Scrollbar Visible=', VertScrollBar.visible);
  if (hs<>HorzScrollBar.Visible) then
    WriteLn('Horizontal Scrollbar Visible=', HorzScrollBar.Visible );
  {$Endif}

  FGCache.ClientWidth:=Width - DV;
  FGCache.ClientHeight:=Height - DH;
  FGCache.ScrollWidth:=FGCache.ClientWidth-FGCache.FixedWidth;
  FGCache.ScrollHeight:=FGCache.ClientHeight-FGCache.FixedHeight;
  
  Mtl:=MaxTopLeft;
  {$Ifdef DbgScroll}
  DebugPoint('MaxTopLeft',MaxTopLeft);
  {$Endif}
  FGCache.HScrDiv:=0;
  FGCache.VScrDiv:=0;

  with FGCache do
  if FScrollBars in [ssAutoHorizontal, ssAutoBoth] then begin
    if HorzScrollBar.Visible then begin
      HorzScrollBar.Range:= GridWidth+2;
      
      if NOt (goSmoothScroll in Options) then begin
        TW:= Integer(AccumWidth[Mtl.X])-(HorzScrollBar.Range-ClientWidth);
        HorzScrollBar.Range:= HorzScrollBar.Range + TW - FixedWidth + 1;
      end;
      
      if HorzScrollBar.Range>ClientWidth then
        HScrDiv:= Double(ColCount-FixedRows-1)/(HorzScrollBar.range-ClientWidth);

      {$Ifdef dbgScroll}
      Writeln('TotWidth=',GridWidth,'ClientWidth=',ClientWidth,' Horz Range=',HorzScrolLBar.Range);
      {$Endif}
    end
  end else
  if FScrollBars in [ssHorizontal, ssBoth] then HorzScrolLBar.Range:=0;
  
  with FGCache do
  if FScrollBars in [ssAutoVertical, ssAutoBoth] then begin
    if VertScrollBar.Visible then begin
      VertScrollBar.Range:=GridHeight + 2;

      if not (goSmoothScroll in Options) then begin
        TH:= Integer(accumHeight[Mtl.Y])-(VertScrollBar.Range-ClientHeight);
        VertScrollBar.Range:= VertScrollBar.Range + TH - FixedHeight + 1;
      end;

      if VertScrolLBar.Range>ClientHeight then
        VScrDiv:= Double(RowCount-FixedRows-1)/(VertScrollBar.Range-ClientHeight);

      {$Ifdef dbgScroll}
      Writeln('TotHeight=',GridHeight,'ClientHeight=',ClientHeight,' Vert Range=',VertScrolLBar.Range);
      {$Endif}
    end
  end else
  if FScrollBars in [ssVertical, ssBoth] then VertScrollbar.Range:=0;

  CacheVisibleGrid;
  Invalidate;
end;

function TCustomGrid.CellRect(ACol, ARow: Integer): TRect;
begin
  Result:=ColRowToClientCellRect(aCol,aRow);
end;

// The visible grid Depends on  TopLeft and ClientWidht,ClientHeight,
// Col/Row Count, So it Should be called inmediately after any change
// like that
function TCustomGrid.GetVisibleGrid: TRect;
var
  w: Integer;
  MaxRight: Integer;
  MaxBottom: Integer;
begin
  if (FTopLeft.X<0)or(FTopLeft.y<0) then begin
    Result:=Rect(-1,-1,-1,-1);
    Exit;
  end;
  // visible TopLeft Cell
  Result.TopLeft:=fTopLeft;
  Result.BottomRight:=Result.TopLeft;
  
  // Max visible coordinates
  MaxRight:= FGCache.ClientWidth;
  MaxBottom:=FGCache.ClientHeight;
    
  // Left Margin of next visible Column and Rightmost visible cell
  w:=GetColWidths(Result.Left) + FGCache.FixedWidth- FGCache.TLColOff;
  while (Result.Right<ColCount-1)and(W<MaxRight) do begin
    Inc(Result.Right);
    W:=W+GetColWidths(Result.Right);
  end;

  // Top Margin of next visible Row and Bottom most visible cell
  w:=GetRowheights(Result.Top) + FGCache.FixedHeight - FGCache.TLRowOff;
  while (Result.Bottom<RowCount-1)and(W<MaxBottom) do begin
    Inc(Result.Bottom);
    W:=W+GetRowHeights(Result.Bottom);
  end;
end;

{Calculate the TopLeft needed to show cell[aCol,aRow]}
function TCustomGrid.ScrollToCell(const aCol,aRow: Integer): Boolean;
var
  RNew: TRect;
  Fw,Fh,Cw,Ch: Integer;
  OldTopLeft:TPoint;
  Xinc,YInc: Integer;
begin
  Cw:=FGCache.ClientWidth; //ClientWidth;
  Ch:=FGCache.ClientHeight; //ClientHeight;

  Fw:=FGCache.FixedWidth; //GetFixedWidth;
  Fh:=FGcache.FixedHeight; //GetFixedHeight;
  
  OldTopLeft:=fTopLeft;
  while (fTopLeft.x>=0) and
        (fTopLeft.x<ColCount)and
        (fTopLeft.y>=0) and
        (fTopLeft.y<RowCount) do begin

    RNew:=ColRowToClientCellRect(aCol,aRow);
    
    Xinc:=0;
    if Rnew.Left + FGCache.TLColOff < fw then Xinc:=-1
    else if RNew.Right  + FGCache.TLColOff > Cw then XInc:=1;
    Yinc:=0;
    if RNew.Top  + FGCAche.TLRowOff < fh then Yinc:=-1
    else if RNew.Bottom + FGCache.TLRowOff >Ch then YInc:=1;
    
    with FTopLeft do
    if ((XInc=0)and(Yinc=0)) or
       ((X=aCol)and(y=aRow)) Or // Only Perfect fit !
       ((X+XInc>=ColCount)or(Y+Yinc>=RowCount)) Or // Last Posible
       ((X+XInc<0)Or(Y+Yinc<0)) // Least Posible
    then Break;

    Inc(FTopLeft.x, XInc);
    Inc(FTopLeft.y, YInc);
  end;

  Result:=not PointIgual(OldTopleft,FTopLeft);
  if result then doTopleftChange(False)
  else ResetOffset(True, True);
end;

{Returns a valid TopLeft from a proposed TopLeft[DCol,DRow] which are
 relative or absolute coordinates }
function TCustomGrid.ScrollGrid(Relative: Boolean; DCol, DRow: Integer): TPoint;
begin
  Result:=FTopLeft;
  if not Relative then begin
    DCol:=DCol-Result.x;
    DRow:=DRow-Result.y;
  end;
  
  if DCol+Result.x<FFixedCols then DCol:=Result.x-FFixedCols else
  if DCol+Result.x>ColCount-1 then DCol:=ColCount-1-Result.x;
  if DRow+Result.y<FFixedRows then DRow:=Result.y-FFixedRows else
  if DRow+Result.y>RowCount-1 then DRow:=RowCount-1-Result.y;

  Inc(Result.x, DCol);
  Inc(Result.y, DRow);
end;

procedure TCustomGrid.TopLeftChanged;
begin
  if Assigned(OnTopLeftChange) and not (csDesigning in ComponentState) then
    OnTopLeftChange(Self);
end;

procedure TCustomGrid.HeaderClick(IsColumn: Boolean; Index: Integer);
begin
end;
procedure TCustomGrid.ColRowMoved(IsColumn: Boolean; FromIndex,ToIndex: Integer);
begin
end;
procedure TCustomGrid.ColRowExchanged(isColumn: Boolean; Index, WithIndex: Integer);
begin
end;
procedure TCustomGrid.DrawFocusRect(aCol, aRow: Integer; ARect: TRect;
  aState: TGridDrawstate);
begin
end;
procedure TCustomGrid.AutoAdjustColumn(aCol: Integer);
begin
end;
procedure TCustomGrid.SizeChanged(OldColCount, OldRowCount: Integer);
begin
end;
procedure TCustomGrid.ColRowDeleted(IsColumn: Boolean; Index: Integer);
begin
end;

procedure TCustomGrid.Paint;
begin
  Inherited Paint;
  if FUpdateCount=0 then begin
    //WriteLn('Paint: FGCache.ValidGrid=',FGCache.ValidGrid );
    DebugRect('Paint.ClipRect=',Canvas.ClipRect);
    DrawEdges;
    DrawBackGround;
    if FGCache.ValidGrid then begin
      {
      DrawFixedCells;
      DrawInteriorCells;
      DrawFocused;
      }
      DrawByRows;
      DrawColRowMoving;
    end;
  end;
end;

procedure TCustomGrid.ResetOffset(chkCol, ChkRow: Boolean);
begin
  with FGCache do begin
    if ChkCol then ChkCol:=TLColOff<>0;
    if ChkCol then TlColOff:=0;
    if ChkRow then ChkRow:=TLRowOff<>0;
    if ChkRow then TlRowOff:=0;
    if ChkRow or ChkCol then begin
      CacheVisibleGrid;
      Invalidate;
      if ChkCol then UpdateScrollBarPos(HorzScrollBar);
      if ChkRow then UpdateScrolLBarPos(VertScrollBar);
    end;
  end;
end;


function TCustomGrid.SelectCell(ACol, ARow: Integer): Boolean;
begin
  Result:=MoveExtend(False, aCol, aRow);
end;

procedure TCustomGrid.DrawBackGround;
begin
  {
    The user can draw a something here :)
    
  Canvas.Brush.Color:=Color;
  Canvas.FillRect(Parent.ClientRect);
  }
end;
(*
procedure TCustomGrid.DrawFixedCells;
var
  Gds: TGridDrawState;
  i,j: Integer;
begin
  Gds:=[gdFixed];
  // Draw fixed fixed Cells
  For i:=0 to FFixedCols-1 do
    For j:=0 to fFixedRows-1 do
      DrawCell(i,j, ColRowToClientCellRect(i,j), gds);
      
  with FGCache.VisibleGrid do begin
    // Draw fixed column headers
    For i:=left to Right do
      For j:=0 to fFixedRows-1 do
        DrawCell(i,j, ColRowToClientCellRect(i,j), gds);
    // Draw fixed row headers
    For i:=0 to FFixedCols-1 do
      For j:=Top to Bottom do
        DrawCell(i,j, ColRowToClientCellRect(i,j), gds);
  end;
end;

procedure TCustomGrid.DrawInteriorCells;
var
  Gds: TGridDrawState;
  i,j: Integer;
begin
  with FGCache.VisibleGrid do begin
    For i:=Left to Right do
      For j:=Top to Bottom do begin
        Gds:=[];
        if (i=FCol)and(J=FRow) then Continue;
        if IsCellSelected(i,j) then Include(gds, gdSelected);
        DrawCell(i,j, ColRowToClientCellRect(i,j), gds);
      end;
  end;
end;
*)
procedure TCustomGrid.DrawColRowMoving;
begin
  if (FGridState=gsColMoving)and(fMoveLast.x>=0) then begin
    Canvas.Pen.Width:=3;
    Canvas.Pen.Color:=clRed;
    Canvas.MoveTo(fMoveLast.y, 0);
    Canvas.Lineto(fMovelast.y, FGCache.MaxClientXY.Y);
    Canvas.Pen.Width:=1;
  end else
  if (FGridState=gsRowMoving)and(FMoveLast.y>=0) then begin
    Canvas.Pen.Width:=3;
    Canvas.Pen.Color:=clRed;
    Canvas.MoveTo(0, FMoveLast.X);
    Canvas.LineTo(FGCache.MaxClientXY.X, FMoveLast.X);
    Canvas.Pen.Width:=1;
  end;
end;

procedure TCustomGrid.DrawCell(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState);
begin
  if gdFixed in aState then Canvas.Brush.Color:=clBtnFace
  else                      Canvas.Brush.Color:=Color;
  Canvas.FillRect(aRect);
  DrawCellGrid(aRect,aCol,aRow,aState);
end;

procedure TCustomGrid.DrawByRows;
var
  i: Integer;
begin
  // Draw Rows
  with FGCache.VisibleGrid do
    For i:=Top To Bottom do DrawRow(i);
  // Draw Fixed Rows
  For i:=0 to FFixedRows-1 Do DrawRow(i);
end;

procedure TCustomGrid.DrawRow(aRow: Integer);
var
  Gds: TGridDrawState;
  i: Integer;
  Rs: Boolean;
  R: TRect;
begin

  ColRowToOffSet(False, True, aRow, R.Top, R.Bottom);
  
  // Draw columns in this row
  with FGCache.VisibleGrid do
    if ARow<FFixedRows then begin
      gds:=[gdFixed];
      For i:=Left to Right do begin
        ColRowToOffset(true, True, i, R.Left, R.Right);
        DrawCell(i,aRow, R{ColRowToClientCellRect(i,aRow)},gds)
      end;
    end else begin
      Rs:=(goRowSelect in Options);
      For i:=Left To Right do begin
        Gds:=[];
        if (i=Fcol)and(FRow=ARow) then begin
          // Focused Cell
          Include(gds, gdFocused);
          // Check if need to be selected
          if (goDrawFocusSelected in Options) or
             (Rs and not(goRelaxedRowSelect in Options)) then Include(gds, gdSelected);
        end else
        if IsCellSelected(i, ARow) then Include(gds, gdSelected);
        ColRowToOffset(True, True, i, R.Left, R.Right);
        DrawCell(i,aRow, R{ColRowToClientCellRect(i,aRow)}, gds);
      end;
      // Draw the focus Rect
      if (ARow=FRow) and
         (IsCellVisible(FCol,ARow) Or (Rs and (ARow>=Top) and (ARow<=Bottom)))
      then begin
        if ShouldEdit and (FEditor<>nil)and(FEditor.Visible) then begin
          //WriteLn('No Draw Focus Rect');
        end else begin
          ColRowToOffset(True, True, FCol, R.Left, R.Right);
          DrawFocusRect(FCol,FRow, R{ColRowToClienTCellRect(FCol,FRow)}, [gdFocused]);
        end;
      end;
    end; // else begin

  // Draw Fixed Columns
  gds:=[gdFixed];
  For i:=0 to FFixedCols-1 do begin
    ColRowToOffset(True, True, i, R.Left, R.Right);
    DrawCell(i,aRow, R{ColRowToClientCellRect(i,aRow)},gds);
  end;
end;

procedure TCustomGrid.DrawEdges;
var
  P:  TPoint;
  Cr: TRect;
begin
  P:=FGCache.MaxClientXY;
  Cr:=Bounds(0,0, FGCache.ClientWidth, FGCache.ClientHeight);
  if P.x<Cr.Right then begin
    Cr.Left:=P.x;
    Canvas.Brush.Color:=Color;
    Canvas.FillRect(cr);
    Cr.Left:=0;
    Cr.Right:=p.x;
  end;
  if P.y<Cr.Bottom then begin
    Cr.Top:=p.y;
    Canvas.Brush.Color:=Color;
    Canvas.FillRect(cr);
  end;
end;

procedure TCustomGrid.DrawFocused;
var
  R: TRect;
  gds: TGridDrawState;
begin
  gds:=[gdFocused];
  if IsCellVisible(FCol,FRow) then begin
    if goDrawFocusSelected in Options then Include(gds,gdSelected);
    if (goRowSelect in Options) and not (goRelaxedRowSelect in Options) then
      Include(gds, gdSelected);
    R:=colrowToClientCellRect(fCol,fRow);
    DrawCell(fCol,fRow,R, gds);
    DrawFocusRect(fCol,fRow, R, gds);
  end else
    if  ((goRowSelect in Options) and
        (Frow>=FGCache.VisibleGrid.Top) and
        (Frow<=FGCache.VisibleGrid.Bottom))
    then begin
      R:=colrowToClientCellRect(fCol,fRow);
      DrawFocusRect(fcol,fRow, R, gds);
    end;
end;

procedure DebugRect(S:string; R:TRect);
begin
  WriteLn(S, 'L=',R.Left, ' T=',R.Top, ' R=',R.Right,' B=',R.Bottom);
end;
procedure DebugPoint(S:string; P:TPoint);
begin
  WriteLn(S, 'X=',P.X,' Y=',P.Y);
end;

procedure Register;
begin
  RegisterComponents('Additional',[TStringGrid,TDrawGrid]);
end;

procedure TCustomGrid.DrawCellGrid(Rect: TRect; aCol,aRow: Integer; aState: TGridDrawState);
var
  dv,dh: Boolean;
begin
  // Draw Cell Grid or Maybe in the future Borders..
  Dv:= goVertLine in Options;
  Dh:= goHorzLine in Options;
  if (gdFixed in aState) then begin
    with Canvas, Rect do begin
      Pen.Style:=psSolid;
      Pen.Color:=cl3DDkShadow;
        MoveTo(Left,Bottom-1);
        LineTo(Right-1,Bottom-1);
        LineTo(Right-1,Top-1);
      //if ARow=FItemIndex then begin
          Pen.Color:=cl3DDkShadow;
          MoveTo(Left,Bottom-1);
          LineTo(Left,Top);
          LineTo(Right-1,Top);
          Pen.Color:=cl3DLight;
          MoveTo(Left+1,Bottom-2);
          LineTo(Right-1,Bottom-2);
      //end;
    end;
    //Canvas.Frame3d(Rect, 1, bvLowered{bvNone}{bvRaised});
    Dh:=Dh and (goFixedHorzLine in Options);
    Dv:=Dv and (goFixedVertLine in Options);
  end;
  
  Canvas.Pen.Style:=fGridLineStyle;
  Canvas.Pen.Color:=fGridLineColor;
  if Dh then begin
  //if fDrawHorzGrid then begin
    Canvas.MoveTo(Rect.Left,Rect.Top);
    Canvas.LineTo(Rect.Right,Rect.Top);
    if aRow=RowCount-1 then begin
      Canvas.MoveTo(Rect.Left,Rect.Bottom);
      Canvas.LineTo(Rect.Right,Rect.Bottom);
    end;
  end;
  
  if Dv then begin
  //if FDrawVertGrid then begin
    Canvas.MoveTo(Rect.Left,Rect.Top);
    Canvas.LineTo(Rect.Left,Rect.Bottom);
    if aCol=ColCount-1 then begin
      Canvas.Moveto(Rect.Right, Rect.Top);
      Canvas.LineTo(Rect.Right, Rect.bottom);
    end;
  end;
end;


procedure TCustomGrid.MyTextRect(R: TRect; Offx, Offy: Integer; S: string;
  Ts: TTextStyle);
var
  Rorg: TRect;
  tmpRgn: HRGN;
begin
  if Ts.Clipping then begin
    //IntersectClipRect(Canvas.handle, R.Left,R.Top,R.Right,R.Bottom);
  
    GetClipBox(Canvas.Handle, @ROrg);
    //DebugRect('Ini Rect = ', ROrg);
    tmpRGN:=CreateRectRgn(R.Left, R.Top, R.Right, R.Bottom);
    SelectClipRgn(Canvas.Handle, tmpRGN);
    //GetClipBox(Canvas.Handle, @Rtmp);
    //DebugRect('Set Rect = ', Rtmp);
    DeleteObject(tmpRGN);
  end;

  //if Ts.Opaque then Canvas.FillRect(R);
  Canvas.TextOut(R.Left+Offx, R.Top+Offy,  S);

  if Ts.Clipping then begin
    tmpRGN:=CreateRectRgn(Rorg.Left, Rorg.Top, Rorg.Right, Rorg.Bottom);
    SelectClipRgn(Canvas.Handle, tmpRGN);
    //GetClipBox(Canvas.Handle, @Rtmp);
    //DebugRect('end Rect = ', Rtmp);
    DeleteObject(tmpRGN);
  end;
end;

procedure TCustomGrid.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  Message.Result:=1;
  WriteLn('TCustomGrid.WMEraseBkgnd');
end;


//
// NOTE: WMHScroll and VMHScroll
// This methods are used to pre-calculate the scroll position
//
procedure TCustomGrid.WMHScroll(var Message: TLMHScroll);
var
  C,Tl: Integer;
  R: TRect;
begin
  // Avoid invalidating right know, just let the scrollbar
  // calculate its position
  BeginUpdate;
  Inherited;
  Message.Result:=1;
  EndUpdate(uoNone);

  {$IfDef dbgScroll}
  WriteLn('HSCROLL: Code=',Message.ScrollCode,' Position=', Message.Pos);
  {$Endif}
  
  if FGCache.HScrDiv<=0 then Exit;
  if FEditor<>nil then EditorGetValue;

  if goThumbTracking in Options then begin
    C:=FFixedCols + Round( Message.Pos * FGCache.HScrDiv );
    if (FCol<>C) then begin
      Inc(FUpdateScrollBarsCount);
      MoveExtend(False, C, FRow);
      Dec(FUpdateScrollBarsCount);
    end;
  end else begin

    C:=Message.Pos+FGCache.FixedWidth;
    TL:=OffsetToColRow(True, False, C, FGCache.TLColOff);
    {$Ifdef dbgScroll}
    WriteLn('---- Offset=',C, ' TL=',TL, ' TLColOFf=', FGCache.TLColOff);
    {$Endif}
    if not (goSmoothScroll in Options) then FGCache.TLColOff:=0;

    if TL<>FTopLeft.X then begin
      Inc(FUpdateScrollBarsCount);
      TryScrollTo(Tl, FTopLeft.Y);
      Dec(FUpdateScrollBarsCount);
    end else
    if goSmoothScroll in Options then begin
      //WriteLn('This Way ==');
      CacheVisibleGrid;
      With FGCache do begin
        R.Topleft:=Point(FixedWidth, 0);
        R.Right:=ClientWidth;
        R.Bottom:=ClientHeight;
      end;
      InvalidateRect(Handle, @R, false);
      //Invalidate;
    end;

  end;
end;

procedure TCustomGrid.WMVScroll(var Message: TLMVScroll);
var
  C: Integer;
  TL: Integer;
  R: TRect;
begin
  // Avoid invalidating right know, just let the scrollbar
  // calculate its position
  BeginUpdate;
  Inherited;
  Message.Result:=1;
  EndUpdate(uoNone);

  {$IfDef dbgScroll}
  WriteLn('VSCROLL: Code=',Message.ScrollCode,' Position=', Message.Pos);
  {$Endif}

  if FGCache.VScrDiv<=0 then Exit;
  if FEditor<>nil then EditorGetValue;
  if goThumbTracking in Options then begin
    C:=FFixedRows + Round( Message.Pos * FGCache.VScrDiv );
    if (C<>FRow) then begin
      Inc(FUpdateScrollBarsCount);
      MoveExtend(False, FCol, C);
      Dec(FUpdateScrollBarsCount);
    end;
  end else begin
    C:=Message.Pos+FGCache.Fixedheight;
    TL:=OffsetToColRow(False, False, C, FGCache.TLRowOff);

    {$Ifdef dbgScroll}
    WriteLn('---- Offset=',C, ' TL=',TL, ' TLRowOFf=', FGCache.TLRowOff);
    {$Endif}
    if not (goSmoothScroll in Options) then FGCache.TLRowOff:=0;

    if TL<>FTopLeft.Y then begin
      Inc(FUpdateScrollBarsCount);
      TryScrollTo(FTopLeft.X, Tl);
      Dec(FUpdateScrollBarsCount);
    end else
    if goSmoothScroll in Options then begin
      CacheVisibleGrid;
      With FGCache do begin
        R.Topleft:=Point(0, FixedHeight);
        R.Right:=ClientWidth;
        R.Bottom:=ClientHeight;
      end;
      InvalidateRect(Handle, @R, false);
      //Invalidate;
    end;
  end;
end;

procedure TCustomGrid.WMSize(var Msg: TWMSize);
begin
  Inherited;
  visualChange;
end;

procedure TCustomGrid.WMChar(var Message: TLMChar);
var
  Ch: Char;
begin
  Ch:=Char(Message.CharCode);
  WriteLn(ClassName,'.WMchar CharCode= ',Message.CharCode);
  if (goEditing in Options) and (Ch in [^H, #32..#255]) then EditorShowChar(Ch)
  else inherited;
end;


procedure TCustomGrid.WndProc(var TheMessage: TLMessage);
begin
  {
  case TheMessage.Msg of
    LM_SETFOCUS:
      begin
        (*
        write('LM_SETFOCUS RECIBIDO');
        if FExchangingFocus then begin
          WriteLn(' - AVOIDING');
          Exit;
        end else WriteLn;
        *)
      end;

    LM_LBUTTONDOWN: WriteLn('LM_MOUSEDOWN');
    CM_MOUSEENTER:  WriteLn('CM_MOUSEENTER');
    CM_MOUSELEAVE:  WriteLn('CM_MOUSELEAVE');
    LM_MOUSEMOVE:   WriteLn('LM_MOUSEMOVE');
  end;
  }
  inherited WndProc(TheMessage);
end;

{ Scroll grid to the given Topleft[aCol,aRow] as needed }
procedure TCustomGrid.TryScrollTo(aCol, aRow: Integer);
var
  TryTL: TPoint;
begin
  TryTL:=ScrollGrid(False,aCol, aRow);
  if not PointIgual(TryTL, FTopLeft) then begin
    FTopLeft:=TryTL;
    doTopleftChange(False);
  end;
end;

procedure TCustomGrid.SetGridLineWidth(const AValue: Integer);
begin
  // Todo
  if FGridLineWidth=AValue then exit;
  FGridLineWidth:=AValue;
  Invalidate;
end;

{ Reposition the scrollbars according to the current TopLeft }
procedure TCustomGrid.UpdateScrollbarPos(Which: TControlScrollbar);
begin
  // Adjust ScrollBar Positions
  // Special condition only When scrolling by draging
  // the scrollbars see: WMHScroll and WVHScroll
  if FUpdateScrollBarsCount=0 then begin
  
    if (Which=HorzScrollBar)or(Which=nil) then
      if (FScrollBars in [ssAutoHorizontal, ssAutoBoth]) and
          HorzScrolLBar.Visible then begin
          with FGCache do
            HorzScrollBar.Position:=
              Integer(AccumWidth[FTopLeft.x])-TLColOff-FixedWidth;
      end;

    if (Which=VertScrollBar)Or(Which=nil) then
      if (FScrolLBars in [ssAutoVertical, ssAutoBoth]) and
          VertScrolLBar.Visible then begin
          with FGCache do
            VertScrollBar.Position:=
              Integer(AccumHeight[FTopLeft.y])-TLRowOff-FixedHeight;
      end;
  end; {if FUpd...}
end;

procedure TCustomGrid.CheckFixedCount(aCol,aRow,aFCol,aFRow: Integer);
begin
  if AFRow<0 then
    Raise EGridException.Create('FixedRows<0');
  if AFCol<0 then
    Raise EGridException.Create('FixedCols<0');
    
  if (aCol=0)And(aFCol=0) then // invalid grid, ok
  else if (aFCol>=aCol) Then
    raise EGridException.Create(rsFixedColsTooBig);
  if (aRow=0)and(aFRow=0) then // Invalid grid, ok
  else if (aFRow>=aRow) then
    raise EGridException.Create(rsFixedRowsTooBig);
end;

{ Save to the cache the current visible grid (excluding fixed cells) }
procedure TCustomGrid.CacheVisibleGrid;
var
  R: TRect;
begin
  with FGCache do begin
    VisibleGrid:=GetVisibleGrid;
    with VisibleGrid do
      ValidGrid:=(Left>=0)and(Top>=0)and(Right>=Left)and(Bottom>=Top);
    if not ValidGrid then MaxClientXY:=Point(0,0)
    else begin
      R:=ColRowToClientCellrect(VisibleGrid.Right, VisibleGrid.Bottom);
      MaxClientXY:=R.BottomRight;
    end;
  end;
end;

function TCustomGrid.GetSelection: TGridRect;
begin
  Result:=FRange;
end;

procedure TCustomGrid.SetDefaultDrawing(const AValue: Boolean);
begin
  if FDefaultDrawing=AValue then exit;
  FDefaultDrawing:=AValue;
  Invalidate;
end;

procedure TCustomGrid.SetFocusColor(const AValue: TColor);
begin
  if FFocusColor=AValue then exit;
  FFocusColor:=AValue;
  InvalidateCell(FCol,FRow);
end;

procedure TCustomGrid.SetGridLineStyle(const AValue: TPenStyle);
begin
  if FGridLineStyle=AValue then exit;
  FGridLineStyle:=AValue;
  Invalidate;
end;

procedure TCustomGrid.SetSelectActive(const AValue: Boolean);
begin
  if FSelectActive=AValue then exit;
  FSelectActive:=AValue and not(goEditing in Options);
  if FSelectActive then FPivot:=Point(FCol,FRow);
end;

procedure TCustomGrid.SetSelection(const AValue: TGridRect);
begin
  if goRangeSelect in Options then begin
    fRange:=NormalizarRect(aValue);
    Invalidate;
  end;
end;

function TCustomGrid.doColSizing(X, Y: Integer): Boolean;
var
  R: TRect;
  Loc: Integer;
begin
  Result:=False;
  if gsColSizing = fGridState then begin
    if x>FSplitter.y then
      ColWidths[FSplitter.x]:=x-FSplitter.y
    else
      if ColWidths[FSplitter.x]>0 then ColWidths[FSplitter.X]:=0;
    Result:=True;
  end else
  if (fGridState=gsNormal)and(Y<FGCache.FixedHeight)and(X>FGCache.FixedWidth) then
  begin
    FSplitter.X:= OffsetToColRow(True, True, X, Loc);
    FSplitter.Y:=0;
    if FSplitter.X>=0 then begin
      R:=ColRowToClientCellRect(FSplitter.x, FSplitter.y);
      FSplitter.y:=X;                       // Resizing X reference
      if (R.Right-X)<(X-R.Left) then Loc:=R.Right
      else begin
        Loc:=R.Left;
        Dec(FSplitter.x);                   // Resizing col is the previous
      end;
      IF (Abs(Loc-x)<=2)and(FSplitter.X>=FFixedCols) then Cursor:=crHSplit
      else                                                Cursor:=crDefault;
      Result:=True;
    end;
  end
    else
      if (cursor=crHSplit) then Cursor:=crDefault;
end;

function TCustomGrid.doRowSizing(X, Y: Integer): Boolean;
var
  OffTop,OffBottom: Integer;
begin
  Result:=False;
  if gsRowSizing = fGridState then begin
    if y>FSplitter.x then
      RowHeights[FSplitter.y]:=y-FSplitter.x
    else
      if RowHeights[FSplitter.y]>0 then RowHeights[FSplitter.Y]:=0;
    Result:=True;
  end else
  if (fGridState=gsNormal)and(X<FGCache.FixedWidth)and(Y>FGCache.FixedHeight) then
  begin
    fSplitter.Y:=OffsetToColRow(False, True, Y, OffTop{dummy});
    if Fsplitter.Y>=0 then begin
      ColRowToOffset(False, True, FSplitter.Y, OffTop, OffBottom);
      FSplitter.X:=Y;
      if (OffBottom-Y)<(Y-OffTop) then SwapInt(OffTop, OffBottom)
      else Dec(FSplitter.y);
      IF (Abs(OffTop-y)<=2)and(FSplitter.Y>=FFixedRows) then Cursor:=crVSplit
      else                                                   Cursor:=crDefault;
      Result:=True;
    end;
  end
    else
      if Cursor=crVSplit then Cursor:=crDefault;
end;

procedure TCustomGrid.doColMoving(X, Y: Integer);
var
  P: TPoint;
  R: TRect;
begin
  P:=MouseToCell(Point(X,Y));
  if (Abs(FSplitter.Y-X)>fDragDx)and(Cursor<>crMultiDrag) then begin
    Cursor:=crMultiDrag;
    FMoveLast:=Point(-1,-1);
    ResetOffset(True, False);
  end;
  if (Cursor=crMultiDrag)and
     (P.x>=FFixedCols) and
     ((P.X<=FSplitter.X)or(P.X>FSplitter.X))and
     (P.X<>FMoveLast.X) then begin
      R:=ColRowToClientCellRect(P.x, P.y);
      if P.x<=FSplitter.X then fMoveLast.Y:=R.left
      else                     FMoveLast.Y:=R.Right;
      fMoveLast.X:=P.X;
      Invalidate;
  end;
end;

procedure TCustomGrid.doRowMoving(X, Y: Integer);
var
  P: TPoint;
  R: TRect;
begin
  P:=MouseToCell(Point(X,Y));
  if (Cursor<>crMultiDrag)and(Abs(FSplitter.X-Y)>fDragDx) then begin
    Cursor:=crMultiDrag;
    FMoveLast:=Point(-1,-1);
    ResetOffset(False, True);
  end;
  if (Cursor=crMultiDrag)and
     (P.y>=FFixedRows) and
     ((P.y<=FSplitter.Y)or(P.Y>FSplitter.Y))and
     (P.y<>FMoveLast.Y) then begin
      R:=ColRowToClientCellRect(P.x, P.y);
      if P.y<=FSplitter.y then fMoveLast.X:=R.Top
      else                     FMoveLast.X:=R.Bottom;
      fMoveLast.Y:=P.Y;
      Invalidate;
  end;
end;


function TCustomGrid.OffsetToColRow(IsCol, Fisical: Boolean; Offset: Integer;
  var Rest: Integer): Integer;
begin
  Result:=0; //Result:=-1;
  Rest:=0;
  if Offset<0 then Exit; // Out of Range;
  
  with FGCache do
  if IsCol then begin
  
    // begin to count Cols from 0 but ...
    if Fisical and (Offset>FixedWidth-1) then begin
      Result:=FTopLeft.X;  // In scrolled view, then begin from FtopLeft col
      Offset:=Offset-FixedWidth+Integer(AccumWidth[Result])+TLColOff;
      if Offset>GridWidth-1 then begin
        Result:=ColCount-1;
        Exit;
      end;
    end;
    while Offset>(Integer(AccumWidth[Result])+GetColWidths(Result)-1) do Inc(Result);
    Rest:=Offset;
    if Result<>0 then Rest:=Offset-Integer(AccumWidth[Result]);

  end else begin
  
    if Fisical and (Offset>FixedHeight-1) then begin
      Result:=FTopLeft.Y;
      Offset:=Offset-FixedHeight+Integer(AccumHeight[Result])+TLRowOff;
      if Offset>GridHeight-1 then begin
        Result:=RowCount-1;
        Exit; // Out of Range
      end;
    end;
    while Offset>(Integer(AccumHeight[Result])+GetRowHeights(Result)-1) do Inc(Result);
    Rest:=Offset;
    if Result<>0 then Rest:=Offset-Integer(AccumHeight[Result]);
    
  end;
end;

function TCustomGrid.ColRowToOffset(IsCol,Fisical:Boolean; Index:Integer; var Ini,Fin:Integer): Boolean;
var
  Dim: Integer;
begin
  with FGCache do begin
    if IsCol then begin
      Ini:=Integer(AccumWidth[Index]);
      Dim:=GetColWidths(Index);
    end else begin
      Ini:=Integer(AccumHeight[Index]);
      Dim:= GetRowheights(Index);
    end;
    if not Fisical then begin
      Fin:=Ini + Dim;
      Exit;
    end;
    if IsCol then begin
      if index>=FFixedCols then
        Ini:=Ini-Integer(AccumWidth[FTopLeft.X]) + FixedWidth -  TLColOff;
    end else begin
      if Index>=FFixedRows then
        Ini:=Ini-Integer(AccumHeight[FTopLeft.Y]) + FixedHeight - TLRowOff;
    end;
    Fin:=Ini + Dim;
  end;
  Result:=true;
end;

function TCustomGrid.MouseToGridZone(X, Y: Integer; CellCoords: Boolean): TGridZone;
begin
  Result:=gzNormal;
  if CellCoords then begin
    if (X<fFixedCols) then
      if Y<FFixedRows then  Result:= gzFixedCells
      else                  Result:= gzFixedRows
    else
    if (Y<fFixedRows) then
      if X<FFixedCols then  Result:= gzFixedCells
      else                  Result:= gzFixedCols;
  end else begin
    if X<=FGCache.FixedWidth then
      if Y<=FGcache.FixedHeight then  Result:=gzFixedCells
      else                            Result:=gzFixedRows
    else
    if Y<=FGCache.FixedHeight then
      if X<=FGCache.FixedWidth then   Result:=gzFixedCells
      else                            Result:=gzFixedCols;
  end;
end;

procedure TCustomGrid.ExchangeColRow(IsColumn: Boolean; Index, WithIndex: Integer
  );
begin
  if IsColumn then FCols.Exchange(Index, WithIndex)
  else             FRows.Exchange(Index, WithIndex);
  ColRowExchanged(IsColumn, Index, WithIndex);
  VisualChange;
end;

procedure TCustomGrid.MoveColRow(IsColumn: Boolean; FromIndex, ToIndex: Integer);
begin
  if IsColumn then FCols.Move(FromIndex, ToIndex)
  else             FRows.Move(FromIndex, ToIndex);
  ColRowMoved(IsColumn, FromIndex, ToIndex);
  VisualChange;
end;

procedure TCustomGrid.SortColRow(IsColumn: Boolean; Index: Integer);
begin
  if IsColumn then SortColRow(IsColumn, Index, FFixedRows, RowCount-1)
  else             SortColRow(IsColumn, Index, FFixedCols, ColCount-1);
end;

procedure TCustomGrid.SortColRow(IsColumn: Boolean; Index, FromIndex,
  ToIndex: Integer);
begin
  if Assigned(OnCompareCells) then begin
    BeginUpdate;
    Sort(IsColumn, Index, FromIndex, ToIndex);
    EndUpdate(true);
  end;
end;

procedure TCustomGrid.DeleteColRow(IsColumn: Boolean; Index: Integer);
begin
  if IsColumn then FCols.Delete(Index)
  else             FRows.Delete(Index);
  ColRowDeleted(IsColumn, Index);
  VisualChange;
end;



procedure TCustomGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  Gz: TGridZone;
  R: TRect;
begin
  inherited MouseDown(Button, Shift, X, Y);

  if not FGCache.ValidGrid then Exit;
  if not (ssLeft in Shift) then Exit;
  if csDesigning in componentState then Exit;

  {$IfDef dbgFocus} WriteLn('MouseDown INIT'); {$Endif}
  
  Gz:=MouseToGridZone(X,Y, False);
  case Gz of
    gzFixedCols:
      begin
        if (goColSizing in Options)and(Cursor=crHSplit) then begin
          R:=ColRowToClientCellRect(FSplitter.x, FTopLeft.y);
          FSplitter.y:=R.Left;
          fGridState:= gsColSizing;
        end else begin
          // ColMoving or Clicking
          fGridState:=gsColMoving;
          FSplitter:=MouseToCell(Point(X,Y));
          FMoveLast:=Point(-1,-1);
          FSplitter.Y:=X;
        end;
      end;
    gzFixedRows:
      if (goRowSizing in Options)and(Cursor=crVSplit) then begin
        R:=ColRowToClientcellRect(FTopLeft.X, FSplitter.y);
        FSplitter.x:=R.top;
        fGridState:= gsRowSizing;
      end else begin
        // RowMoving or Clicking
        fGridState:=gsRowMoving;
        fSplitter:=MouseToCell(Point(X,Y));
        FMoveLast:=Point(-1,-1);
        FSplitter.X:=Y;
      end;
    gzNormal:
      begin
        fGridState:=gsSelecting;
        FSplitter:=MouseToCell(Point(X,Y));

        if not (goEditing in Options) then begin
          if ssShift in Shift then begin
            SelectActive:=(goRangeSelect in Options);
          end else begin
            if not SelectACtive then begin
              FPivot:=FSplitter;
              FSelectActive:=true;
            end;
          end;
        end;
        
        if not MoveExtend(False, FSplitter.X, FSplitter.Y) then begin
          if ShouldEdit then begin
            SelectEditor;
            EditorShow;
          end;
          // user clicked on selected cell
          // -> fire an OnSelection event
          MoveSelection;
          // Click();
        end;
        
        if (GoEditing in Options)and(FEditor=nil) and not Focused then begin
          {$IfDef dbgFocus} WriteLn('  AUTO-FOCUSING '); {$Endif}
          LCLIntf.SetFocus(Self.Handle);
        end;
        
      end;
  end;
  {$ifDef dbgFocus} WriteLn('MouseDown END'); {$Endif}
end;

procedure TCustomGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  p: TPoint;
begin
  inherited MouseMove(Shift, X, Y);
  if not FGCache.ValidGrid then Exit;

  case fGridState of
    gsSelecting:
      begin
        if not (goEditing in Options) then begin
          P:=MouseToLogcell(Point(X,Y));
          MoveExtend(False, P.x, P.y);
        end;
      end;
    gsColMoving: if goColMoving in Options then doColMoving(X,Y);
    gsRowMoving: if goRowMoving in Options then doRowMoving(X,Y);
    else
      begin
        if goColSizing in Options then doColSizing(X,Y);
        if goRowSizing in Options then doRowSizing(X,Y);
      end;
  end;
end;

procedure TCustomGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
   Cur: TPoint;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if not FGCache.ValidGrid then Exit;
  {$IfDef dbgFocus}WriteLn('MouseUP INIT');{$Endif}
  Cur:=MouseToCell(Point(x,y));
  case fGridState of
    gsSelecting:
      begin
        if SelectActive then begin
          MoveExtend(False, Cur.x, Cur.y);
          SelectActive:=False;
        end;
      end;
    gsColMoving:
      begin
        //WriteLn('Move Col From ',Fsplitter.x,' to ', FMoveLast.x);
        if FMoveLast.X>=0 then begin
          MoveColRow(True, Fsplitter.X, FMoveLast.X);
          Cursor:=crDefault;
        end else
          if Cur.X=FSplitter.X then HeaderClick(True, FSplitter.X);
      end;
    gsRowMoving:
      begin
        //WriteLn('Move Row From ',Fsplitter.Y,' to ', FMoveLast.Y);
        if FMoveLast.Y>=0 then begin
          MoveColRow(False, Fsplitter.Y, FMoveLast.Y);
          Cursor:=crDefault;
        end else
          if Cur.Y=FSplitter.Y then HeaderClick(False, FSplitter.Y);
      end;
  end;
  fGridState:=gsNormal;
  {$IfDef dbgFocus}WriteLn('MouseUP  END  RND=',Random);{$Endif}
end;

procedure TCustomGrid.DblClick;
begin
  if (goColSizing in Options) and (Cursor=crHSplit) then begin
    if (goDblClickAutoSize in Options) then begin
      AutoAdjustColumn( FSplitter.X );
    end else
      WriteLn('Got Doubleclick on Col Resizing: AutoAdjust?');
  end else
  if  (goDblClickAutoSize in Options) and
      (goRowSizing in Options) and
      (Cursor=crVSplit) then begin
        WriteLn('Got DoubleClick on Row Resizing: AutoAdjust?');
  end
  else
    Inherited DblClick;
end;

procedure TCustomGrid.DestroyHandle;
begin
  editorGetValue;
  inherited DestroyHandle;
end;

procedure TCustomGrid.doExit;
begin
  if FEditorShowing then begin
    {$IfDef dbgFocus}WriteLn('DoExit - EditorShowing');{$Endif}
  end else begin
    {$IfDef dbgFocus}WriteLn('DoExit - Ext');{$Endif}
    Invalidate;
  end;
  inherited doExit;
end;

procedure TCustomGrid.doEnter;
begin
  inherited doEnter;
  if FEditorHiding then begin
    {$IfDef dbgFocus}WriteLn('DoEnter - EditorHiding');{$Endif}
  end else begin
    {$IfDef dbgFocus}WriteLn('DoEnter - Ext');{$Endif}
    if ShouldEdit then begin
      SelectEditor;
      if Feditor=nil then Invalidate
      else                EditorShow;
    end else Invalidate;
  end;
end;

procedure TCustomGrid.KeyDown(var Key: Word; Shift: TShiftState);
var
  Sh: Boolean;
  
  procedure MoveSel(Rel: Boolean; aCol,aRow: Integer);
  //var SmallMove: Boolean;
  begin
    // Always reset Offset in kerboard Events
    FGCache.TLColOff:=0;
    FGCache.TLRowOff:=0;
    SelectActive:=Sh;
    MoveNextSelectable(Rel, aCol, aRow);
    {
    SmallMove:= Rel and (Abs(ACol)<2)and(Abs(Arow)<2);
    if SmallMove and SkipUnSelectable then MoveNextSelectable(acol,aRow)
    else                                   MoveExtend(Rel,aCol,aRow);
    }
    Key:=0;
  end;
var
  R: TRect;
  Relaxed: Boolean;
  //PF: TCustomForm;
begin
  inherited KeyDown(Key, Shift);
  if not FGCache.ValidGrid then Exit;
  Sh:=(ssShift in Shift);
  Relaxed:=not (goRowSelect in Options) or (goRelaxedRowSelect in Options);
  
  if (Key=Vk_TAB) then begin
    if (goTabs in Options) then begin
      case FAutoAdvance of
        aaRight:
          if Sh then Key:=VK_LEFT
          else       Key:=VK_RIGHT;
        aaDown:
          if Sh then Key:=VK_UP
          else       Key:=VK_DOWN;
      end;
    end else begin
      // TODO
      (*
      Pf:=GetParentForm(Self);
      if (Pf<>nil) then Pf.FocusControl(Self);
      PerformTab;
      *)
    end;
  end;
  
  case Key of
    VK_LEFT:
      begin
        if Relaxed then MoveSel(True,-1, 0)
        else            MoveSel(true, 0,-1);
      end;
    VK_RIGHT:
      begin
        if Relaxed then MoveSel(True, 1, 0)
        else            MoveSel(True, 0, 1);
      end;
    VK_UP:
      begin
        MoveSel(True, 0, -1);
      end;
    VK_DOWN:
      begin
        MoveSel(True, 0, 1);
      end;
    VK_PRIOR:
      begin
        R:=FGCache.Visiblegrid;
        MoveSel(True, 0, R.Top-R.Bottom);
      end;
    VK_NEXT:
      begin
        R:=FGCache.VisibleGrid;
        MoveSel(True, 0, R.Bottom-R.Top);
      end;
    VK_HOME:
      begin
        if ssCtrl in Shift then MoveSel(False, FCol, FFixedRows)
        else
          if Relaxed then MoveSel(False, FFixedCols, FRow)
          else            MoveSel(False, FCol, FFixedRows);
      end;
    VK_END:
      begin
        if ssCtrl in Shift then MoveSel(False, FCol, RowCount-1)
        else
          if Relaxed then MoveSel(False, ColCount-1, FRow)
          else            MoveSel(False, FCol, RowCount-1);
      end;
    VK_F2, VK_RETURN:
      begin
        EditorShow;
        if Key=VK_RETURN then EditorSelectAll;
        Key:=0;
      end;
    VK_BACK:
      begin
        // Workaround: LM_CHAR doesnt trigger with BACKSPACE
        EditorShowChar(^H);
        key:=0;
      end;

    {$IfDef Dbg}
    else WriteLn(ClassName,'.KeyDown: ', Key);
    {$Endif}
  end;
end;


procedure TCustomGrid.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
end;


{ Returns a reactagle corresponding to a fisical cell[aCol,aRow] }
function TCustomGrid.ColRowToClientCellRect(aCol, aRow: Integer): TRect;
begin
  ColRowToOffset(True, True, ACol, Result.Left, Result.Right);
  ColRowToOffSet(False,True, ARow, Result.Top, Result.Bottom);
end;

{ Convert a fisical Mouse coordinate into fisical a cell coordinate }
function TCustomGrid.MouseToCell(Mouse: TPoint): TPoint;
var
   d: Integer;
begin
  Result.X:= OffsetToColRow(True, True, Mouse.x, d);
  Result.Y:= OffsetToColRow(False,True, Mouse.y, d);
end;

{ Convert a fisical Mouse coordinate into logical a cell coordinate }
function TCustomGrid.MouseToLogcell(Mouse: TPoint): TPoint;
var
  gz: TGridZone;
begin
  Gz:=MouseToGridZone(Mouse.x, Mouse.y, False);
  if gz=gzNormal then Result:=MouseToCell(Mouse)
  else begin
    Result:=MouseToCell(Mouse);
    if (gz=gzFixedRows)or(gz=gzFixedCells) then begin
      Result.x:= fTopLeft.x-1;
      if Result.x<FFixedCols then Result.x:=FFixedCols;
    end;
    if (gz=gzFixedCols)or(gz=gzFixedCells) then begin
      Result.y:=fTopleft.y-1;
      if Result.y<fFixedRows then Result.y:=FFixedRows;
    end;
  end;
end;

function TCustomGrid.ISCellVisible(aCol, aRow: Integer): Boolean;
begin
  with FGCache.VisibleGrid do
    Result:= (Left<=ACol)and(aCol<=Right)and(Top<=aRow)and(aRow<=Bottom);
end;

procedure TCustomGrid.InvalidateCol(ACol: Integer);
var
  R: TRect;
begin
  {$ifdef dbg} WriteLn('InvalidateCol  Col=',aCol); {$Endif}
  R:=ColRowToClientCellRect(aCol, FTopLeft.y);
  R.Top:=0; // Full Column
  R.Bottom:=FGCache.MaxClientXY.Y;
  InvalidateRect(Handle, @R, True);
end;

procedure TCustomGrid.InvalidateRow(ARow: Integer);
var
  R: TRect;
begin
  {$ifdef dbg} WriteLn('InvalidateRow  Row=',aRow); {$Endif}
  R:=ColRowToClientCellRect(fTopLeft.x, aRow);
  R.Left:=0; // Full row
  R.Right:=FGCache.MaxClientXY.X;
  InvalidateRect(Handle, @R, True);
end;

function TCustomGrid.MoveExtend(Relative: Boolean; DCol, DRow: Integer): Boolean;
var
  InvalidateAll: Boolean;
  LastEditor: TWinControl;
  WasVis: Boolean;
begin
  Result:=TryMoveSelection(Relative,DCol,DRow);
  if (not Result) then Exit;
  
  BeforeMoveSelection(DCol,DRow);
  {$IfDef dbgFocus}WriteLn(' MoveExtend INIT FCol= ',FCol, ' FRow= ',FRow);{$Endif}

  LastEditor:=Editor;
  WasVis:=(LastEditor<>nil)and(LastEditor.Visible);
  // default range
  if goRowSelect in Options then FRange:=Rect(FFixedCols, DRow, Colcount-1, DRow)
  else                           FRange:=Rect(DCol,DRow,DCol,DRow);
  
  InvalidateAll:=False;
  if SelectActive then
    if goRangeSelect in Options then begin
      if goRowSelect in Options then begin
        FRange.Top:=Min(fPivot.y, DRow);
        FRange.Bottom:=Max(fPivot.y, DRow);
      end else begin
        FRange:=NormalizarRect(Rect(Fpivot.x,FPivot.y, DCol, DRow));
      end;
      InvalidateAll:=True;
    end;

  if not ScrollToCell(DCol, DRow) then
    if InvalidateAll then begin
      //InvalidateSelection;
      Invalidate
    end else
    if goRowSelect in Options then begin
      InvalidateRow(FRow);
      InvalidateRow(DRow);
    end else begin
      InvalidateCell(FCol, FRow);
      InvalidateCell(DCol, DRow);
    end;
    

  SwapInt(DCol,FCol);
  SwapInt(DRow,FRow);

  MoveSelection;
  SelectEditor;

  ProcessEditor(LastEditor,DCol,DRow,WasVis);

  {$IfDef dbgFocus}WriteLn(' MoveExtend FIN FCol= ',FCol, ' FRow= ',FRow);{$Endif}
end;

function TCustomGrid.MoveNextSelectable(Relative: Boolean; DCol, DRow: Integer
  ): Boolean;
var
  CInc,RInc: Integer;
  NCol,NRow: Integer;
  SelOk: Boolean;
begin
  // Reference
  if not Relative then begin
    NCol:=DCol;
    NRow:=DRow;
    DCol:=NCol-FCol;
    DRow:=NRow-FRow;
  end else begin
    NCol:=FCol + DCol;
    NRow:=FRow + DRow;
  end;
  // Increment
  if DCol<0 then CInc:=-1 else
  if DCol>0 then CInc:= 1
  else           CInc:= 0;
  if DRow<0 then RInc:=-1 else
  if DRow>0 then RInc:= 1
  else           RInc:= 0;
  // Calculation
  SelOk:=CanSelect(NCol,NRow);
  Result:=False;
  while not SelOk do begin
    if  (NRow>RowCount-1)or(NRow<FFixedRows) or
        (NCol>ColCount-1)or(NCol<FFixedCols) then Exit;
    Inc(NCol, CInc);
    Inc(NRow, RInc);
    SelOk:=CanSelect(NCol, NRow);
  end;
  Result:=MoveExtend(False, NCol, NRow);
end;

function TCustomGrid.TryMoveSelection(Relative: Boolean; var DCol, DRow: Integer
  ): Boolean;
begin

  Result:=False;
  
  dCol:=FCol*(1-Byte(not Relative))+DCol;
  dRow:=FRow*(1-Byte(not Relative))+DRow;
  if dCol<FFixedCols then dCol:=FFixedCols else
  if dCol>ColCount-1 then dcol:=ColCount-1;
  if dRow<FFixedRows then dRow:=FFixedRows else
  if dRow>RowCount-1 then dRow:=RowCount-1;

  // Change on Focused cell?
  if (Dcol=FCol)and(DRow=FRow) then begin
  end else begin
    Result:=CanSelect(DCol,DRow);
  end;
end;

procedure TCustomGrid.ProcessEditor(LastEditor: TWinControl; DCol, DRow: Integer; WasVis: Boolean);
  procedure RestoreEditor;
  begin
    SwapInt(Integer(FEditor),Integer(LastEditor));
    SwapInt(FCol,DCol);
    SwapInt(FRow,DRow);
  end;
  procedure HideLastEditor;
  begin
    RestoreEditor;
    EditorGetValue;
    RestoreEditor;
  end;
var
  WillVis: Boolean;
begin
  WillVis:=(FEditor<>nil)and ShouldEdit;

  if WillVis or WasVis then begin
    if not WillVis then HideLastEditor else
    if not WasVis then  EditorShow
    else begin
      if LastEditor=FEditor then begin
        RestoreEditor;
        doEditorGetValue;
        RestoreEditor;
        EditorPos;
        doEditorSetValue;
      end else begin
        LastEditor.Visible:=False;
        lastEditor.Parent:=nil;
        EditorShow;
      end;
    end;
  end;
end;

procedure TCustomGrid.BeforeMoveSelection(const DCol,DRow: Integer);
begin
  if Assigned(OnBeforeSelection) then OnBeforeSelection(Self, DCol, DRow);
end;

function TCustomGrid.CanSelect(const DCol,DRow: Integer): Boolean;
begin
  Result:=true;
  if Assigned(OnCanSelect) then OnCanSelect(Self, DCol, DRow, Result);
end;

procedure TCustomGrid.MoveSelection;
begin
  if Assigned(OnSelection) then OnSelection(Self, FCol, FRow);
end;

procedure TCustomGrid.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TCustomGrid.EndUpdate(UO: TUpdateOption);
begin
  Dec(FUpdateCount);
  if FUpdateCount=0 then
    case UO of
      uoQuick: Invalidate;
      uoFull: VisualChange;
    end;
end;

procedure TCustomGrid.EndUpdate(FullUpdate: Boolean);
begin
  EndUpdate(uoFull);
end;

function TCustomGrid.IsCellSelected(aCol, aRow: Integer): Boolean;
begin
  Result:=  (FRange.Left<=aCol)   and
            (aCol<=FRange.Right)  and
            (FRange.Top<=aRow)    and
            (aRow<=FRange.Bottom);
end;

procedure TCustomGrid.InvalidateCell(aCol, aRow: Integer);
begin
  InvalidateCell(ACol,ARow, False);
end;

procedure TCustomGrid.InvalidateCell(aCol, aRow: Integer; Redraw: Boolean);
var
  R: TRect;
begin
  {$IfDef dbgPaint}
    WriteLn('InvalidateCell  Col=',aCol, ' Row=',aRow,' Redraw=',Redraw);
  {$Endif}
  R:=ColRowToClientCellRect(aCol, aRow);
  InvalidateRect(Handle, @R, Redraw);
end;

procedure TCustomGrid.InvalidateGrid;
begin
  if FUpdateCount=0 then Invalidate;
end;

procedure TCustomGrid.Invalidate;
begin
  if FUpdateCount=0 then
    inherited Invalidate;
end;

procedure TCustomGrid.EditorGetValue;
begin
  if not (csDesigning in ComponentState) then begin
    doEditorGetValue;
    EditorHide;
  end;
end;

procedure TCustomGrid.EditorSetValue;
begin
  if not (csDesigning in ComponentState) then begin
    doEditorSetValue;
    EditorPos;
  end;
end;

procedure TCustomGrid.EditorHide;
begin
  if (Editor<>nil) and Editor.HandleAllocated and Editor.Visible then begin
    if not FEditorHiding then begin
      {$IfDef dbgFocus} WriteLn('EditorHide INIT FCol=',FCol,' FRow=',FRow);{$Endif}
      FEditorHiding:=True;
      Editor.Visible:=False;
      Editor.Parent:=nil;
      LCLIntf.SetFocus(Self.Handle);
      FEDitorHiding:=False;
      {$IfDef dbgFocus} WriteLn('EditorHide FIN'); {$Endif}
    end;
  end;
end;

procedure TCustomGrid.EditorShow;
begin
  if csDesigning in ComponentState then Exit;
  
  if (goEditing in Options) then
    if (Editor<>nil) and not Editor.Visible then begin
      if not FEditorShowing then begin
        {$IfDef dbgFocus} WriteLn('EditorShow INIT FCol=',FCol,' FRow=',FRow);{$Endif}
        FEditorShowing:=True;
        ScrollToCell(FCol,FRow);
        EditorReset;
        LCLIntf.SetFocus(Editor.Handle);
        FEditorShowing:=False;
        InvalidateCell(FCol,FRow,True);
        {$IfDef dbgFocus} WriteLn('EditorShow FIN');{$Endif}
      end;
    end;
end;

procedure TCustomGrid.EditorPos;
var
  msg: TGridMessage;
begin
  if FEditor<>nil then begin
    Msg.CellRect:=ColRowToClientCellRect(FCol,FRow);
    if FEditorOptions and EO_AUTOSIZE = EO_AUTOSIZE then begin
      with Msg.CellRect do begin
        Right:=Right-Left;
        Bottom:=Bottom-Top;
        FEditor.SetBounds(Left, Top, Right, Bottom);
      end;
    end else begin
      Msg.MsgID:=GM_SETPOS;
      Msg.Grid:=Self;
      Msg.Col:=FCol;
      Msg.Row:=FRow;
      FEditor.Dispatch(Msg);
    end;
  end;
end;

procedure TCustomGrid.EditorReset;
begin
  EditorSetValue;
  Editor.Parent:=Self;
  Editor.Visible:=True;
end;

procedure TCustomGrid.EditorSelectAll;
var
  Msg: TGridMessage;
begin
  if FEditor<>nil then
    if FEditorOptions and EO_SELECTALL = EO_SELECTALL then begin
      Msg.MsgID:=GM_SELECTALL;
      FEditor.Dispatch(Msg);
    end;
end;

procedure TCustomGrid.doEditorGetValue;
begin
  //
end;

procedure TCustomGrid.doEditorSetValue;
begin
  //
end;

procedure TCustomGrid.EditorExit(Sender: TObject);
begin
  if not FEditorHiding then begin
    {$IfDef dbgFocus} WriteLn('EditorExit INIT');{$Endif}
    FEditorHiding:=True;
    EditorGetValue;
    if Editor<>nil then begin
      Editor.Visible:=False;
      Editor.Parent:=nil;
      //InvalidateCell(FCol,FRow, True);
    end;
    FEditorHiding:=False;
    {$IfDef dbgFocus} WriteLn('EditorExit FIN'); {$Endif}
  end;
end;

procedure TCustomGrid.EditorKeyDown(Sender: TObject; var Key:Word; Shift:TShiftState);
begin
  FEditorKey:=True; // Just a flag to see from where the event comes
  case Key of
    VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN,
    VK_PRIOR, VK_NEXT:
    begin
      if not(ssShift in Shift) then KeyDown(Key, Shift);
    end;
    VK_RETURN, VK_TAB:
    begin
      if (Key=VK_TAB) and not (goTabs in Options) then begin
          // let the focus go
          KeyDown(Key, Shift);
          //WriteLn('Editor KeyTab Pressed, Focus Should leave the grid');
          Exit;
      end;
      Key:=0;
      case FAutoAdvance of
        aaRight: Key:=VK_RIGHT * Integer( FCol<ColCount-1 );
        aaDown : Key:=VK_DOWN * Integer( FRow<RowCount-1 );
      end;
      if Key=0 then begin
        EditorGetValue;
        EditorShow;
        // Select All !
      end else KeyDown(Key, Shift);
    end;
  end;
  FEditorKey:=False;
end;

procedure TCustomGrid.SelectEditor;
var
  aEditor: TWinControl;
begin
  aEditor:= Editor;
  if (goEditing in Options) and Assigned(OnSelectEditor) then
    OnSelectEditor(Self, fCol,FRow, aEditor);
  if aEditor<>Editor then Editor:=aEditor;
end;

function TCustomGrid.ShouldEdit: Boolean;
begin
  Result:=(goEditing in Options)and(goAlwaysShowEditor in Options);
end;

procedure TCustomGrid.EditorShowChar(Ch: Char);
var
  msg: TGridMessage;
begin
  SelectEditor;
  if FEditor<>nil then begin
    EditorShow;
    EditorSelectAll;
    //PostMessage(FEditor.Handle, LM_CHAR, Word(Ch), 0);
    //
    // Note. this is a workaround because the call above doesn't work
    ///
    Msg.MsgID:=GM_SETVALUE;
    Msg.Grid:=Self;
    Msg.Col:=FCol;
    Msg.Row:=FRow;
    if Ch=^H then Msg.Value:=''
    else          Msg.Value:=ch;
    FEditor.Dispatch(Msg);
  end;
end;

procedure TCustomGrid.EditorCancel;
begin
  EditorHide;
  SetFocus;
end;

procedure TCustomGrid.ColWidthsChanged;
begin
  //
end;
procedure TCustomGrid.RowHeightsChanged;
begin
  //
end;

procedure TCustomGrid.SaveContent(cfg: TXMLConfig);
var
  i,j,k: Integer;
  Path: string;
begin
  cfg.SetValue('grid/version', GRIDFILEVERSION);
  
  Cfg.SetValue('grid/saveoptions/create', soDesign in SaveOptions);
  if soDesign in SaveOptions then begin
    Cfg.SetValue('grid/design/columncount',  ColCount);
    Cfg.SetValue('grid/design/rowcount',  RowCount);
    Cfg.SetValue('grid/design/fixedcols', FixedCols);
    Cfg.SetValue('grid/design/fixedrows', Fixedrows);
    Cfg.SetValue('grid/design/defaultcolwidth', DefaultColWidth);
    Cfg.SetValue('grid/design/defaultRowHeight',DefaultRowHeight);

    j:=0;
    For i:=0 to ColCount-1 do begin
      k:=Integer(FCols[i]);
      if (k>=0)and(k<>DefaultColWidth) then begin
        inc(j);
        cfg.SetValue('grid/design/columns/columncount',j);
        cfg.SetValue('grid/design/columns/column'+IntToStr(j)+'/index', i);
        cfg.SetValue('grid/design/columns/column'+IntToStr(j)+'/width', k);
      end;
    end;
    j:=0;
    For i:=0 to RowCount-1 do begin
      k:=Integer(FRows[i]);
      if (k>=0)and(k<>DefaultRowHeight) then begin
        inc(j);
        cfg.SetValue('grid/design/rows/rowcount',j);
        cfg.SetValue('grid/design/rows/row'+IntToStr(j)+'/index', i);
        cfg.SetValue('grid/design/rows/row'+IntToStr(j)+'/height',k);
      end;
    end;


    Path:='grid/design/options/';
    Cfg.SetValue(Path+'goFixedVertLine/value', goFixedVertLine in options);
    Cfg.SetValue(Path+'goFixedHorzLine/value', goFixedHorzLine in options);
    Cfg.SetValue(Path+'goVertLine/value',  goVertLine in options);
    Cfg.SetValue(Path+'goHorzLine/value',  goHorzLine in options);
    Cfg.SetValue(Path+'goRangeSelect/value', goRangeSelect in options);
    Cfg.SetValue(Path+'goDrawFocusSelected/value', goDrawFocusSelected in options);
    Cfg.SetValue(Path+'goRowSizing/value', goRowSizing in options);
    Cfg.SetValue(Path+'goColSizing/value', goColSizing in options);
    Cfg.SetValue(Path+'goRowMoving/value', goRowMoving in options);
    Cfg.SetValue(Path+'goColMoving/value', goColMoving in options);
    Cfg.SetValue(Path+'goEditing/value', goEditing in options);
    Cfg.SetValue(Path+'goTabs/value', goTabs in options);
    Cfg.SetValue(Path+'goRowSelect/value', goRowSelect in options);
    Cfg.SetValue(Path+'goAlwaysShowEditor/value', goAlwaysShowEditor in options);
    Cfg.SetValue(Path+'goThumbTracking/value', goThumbTracking in options);
    Cfg.SetValue(Path+'goColSpanning/value', goColSpanning in options);
    cfg.SetValue(Path+'goRelaxedRowSelect/value', goRelaxedRowSelect in options);
    cfg.SetValue(Path+'goDblClickAutoSize/value', goDblClickAutoSize in options);
    Cfg.SetValue(Path+'goSmoothScroll/value', goSmoothScroll in Options);
  end;
  
  Cfg.SetValue('grid/saveoptions/position', soPosition in SaveOptions);
  if soPosition in SaveOptions then begin
    Cfg.SetValue('grid/position/topleftcol',ftopleft.x);
    Cfg.SetValue('grid/position/topleftrow',ftopleft.y);
    Cfg.SetValue('grid/position/col',fCol);
    Cfg.SetValue('grid/position/row',fRow);
    if goRangeSelect in Options then begin
      Cfg.SetValue('grid/position/selection/left',Selection.left);
      Cfg.SetValue('grid/position/selection/top',Selection.top);
      Cfg.SetValue('grid/position/selection/right',Selection.right);
      Cfg.SetValue('grid/position/selection/bottom',Selection.bottom);
    end;
  end;
end;


procedure TCustomGrid.LoadContent(cfg: TXMLConfig; Version: Integer);
var
  CreateSaved: Boolean;
  Opt: TGridOptions;
  i,j,k: Integer;
  path: string;
  
    procedure GetValue(optStr:string; aOpt:TGridOption);
    begin
      if Cfg.GetValue(Path+OptStr+'/value', False) then Opt:=Opt+[aOpt];
    end;
    
begin
  if soDesign in FSaveOptions then begin
    CreateSaved:=Cfg.GetValue('grid/saveoptions/create', false);
    if CreateSaved then begin
      Clear;
      FixedCols:=0;
      FixedRows:=0;
      ColCount:=Cfg.GetValue('grid/design/columncount', 5);
      RowCount:=Cfg.GetValue('grid/design/rowcount', 5);
      FixedCols:=Cfg.GetValue('grid/design/fixedcols', 1);
      FixedRows:=Cfg.GetValue('grid/design/fixedrows', 1);
      DefaultRowheight:=Cfg.GetValue('grid/design/defaultrowheight', 24);
      DefaultColWidth:=Cfg.getValue('grid/design/defaultcolwidth', 64);

      Path:='grid/design/columns/';
      k:=cfg.getValue(Path+'columncount',0);
      For i:=1 to k do begin
        j:=cfg.getValue(Path+'column'+IntToStr(i)+'/index',-1);
        if (j>=0)and(j<=ColCount-1) then begin
          ColWidths[j]:=cfg.getValue(Path+'column'+IntToStr(i)+'/width',-1);
        end;
      end;
      Path:='grid/design/rows/';
      k:=cfg.getValue(Path+'rowcount',0);
      For i:=1 to k do begin
        j:=cfg.getValue(Path+'row'+IntToStr(i)+'/index',-1);
        if (j>=0)and(j<=ColCount-1) then begin
          RowHeights[j]:=cfg.getValue(Path+'row'+IntToStr(i)+'/height',-1);
        end;
      end;

      Opt:=[];
      Path:='grid/design/options/';
      GetValue('goFixedVertLine', goFixedVertLine);
      GetValue('goFixedHorzLine', goFixedHorzLine);
      GetValue('goVertLine',goVertLine);
      GetValue('goHorzLine',goHorzLine);
      GetValue('goRangeSelect',goRangeSelect);
      GetValue('goDrawFocusSelected',goDrawFocusSelected);
      GetValue('goRowSizing',goRowSizing);
      GetValue('goColSizing',goColSizing);
      GetValue('goRowMoving',goRowMoving);
      GetValue('goColMoving',goColMoving);
      GetValue('goEditing',goEditing);
      GetValue('goRowSelect',goRowSelect);
      GetValue('goTabs',goTabs);
      GetValue('goAlwaysShowEditor',goAlwaysShowEditor);
      GetValue('goThumbTracking',goThumbTracking);
      GetValue('goColSpanning', goColSpanning);
      GetValue('goRelaxedRowSelect',goRelaxedRowSelect);
      GetValue('goDblClickAutoSize',goDblClickAutoSize);
      if Version>=2 then begin
        GetValue('goSmoothScroll',goSmoothScroll);
      end;
      
      Options:=Opt;
    end;

    CreateSaved:=Cfg.GetValue('grid/saveoptions/position', false);
    if CreateSaved then begin
      i:=Cfg.GetValue('grid/position/topleftcol',-1);
      j:=Cfg.GetValue('grid/position/topleftrow',-1);
      if MouseToGridZone(i,j,true)=gzNormal then begin
        tryScrollto(i,j);
      end;
      i:=Cfg.GetValue('grid/position/col',-1);
      j:=Cfg.GetValue('grid/position/row',-1);
      if (i>=FFixedCols)and(i<=ColCount-1) and
         (j>=FFixedRows)and(j<=RowCount-1) then begin
        MoveExtend(false, i,j);
      end;
      if goRangeSelect in Options then begin
        FRange.left:=Cfg.getValue('grid/position/selection/left',FCol);
        FRange.Top:=Cfg.getValue('grid/position/selection/top',FRow);
        FRange.Right:=Cfg.getValue('grid/position/selection/right',FCol);
        FRange.Bottom:=Cfg.getValue('grid/position/selection/bottom',FRow);
      end;
    end;
  end;
end;

procedure TCustomGrid.Loaded;
begin
  inherited Loaded;
  VisualChange;
end;

constructor TCustomGrid.Create(AOwner: TComponent);
begin
  // Inherited create Calls SetBounds->WM_SIZE->VisualChange so
  // fGrid needs to be created before that
  FCols:=TList.Create;
  FRows:=TList.Create;
  FGCache.AccumWidth:=TList.Create;
  FGCache.AccumHeight:=TList.Create;
  inherited Create(AOwner);
  AutoScroll:=False;
  FOptions:=
    [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect,
     goSmoothScroll ];
  FScrollbars:=ssAutoBoth;
  fGridState:=gsNormal;
  fDefColWidth:=64;//40;
  fDefRowHeight:=24;//18;
  fGridLineColor:=clGray;
  FGridLineStyle:=psSolid;
  fFocusColor:=clRed;
  FSkipUnSelectable:=True;

  FRange:=Rect(-1,-1,-1,-1);
  FDragDx:=3;

  SetBounds(0,0,200,100);
  ColCount:=5;
  RowCount:=5;
  FixedCols:=1;
  FixedRows:=1;
  Editor:=nil;
  
  Color:=clWindow;
end;

destructor TCustomGrid.Destroy;
begin
  {$Ifdef dbg}WriteLn('TCustomGrid.Destroy');{$Endif}
  FreeThenNil(FGCache.AccumWidth);
  FreeThenNil(FGCache.AccumHeight);
  FreeThenNil(FCols);
  FreeThenNil(FRows);
  inherited Destroy;
end;

procedure TCustomGrid.SaveToFile(FileName: string);
var
  Cfg: TXMLConfig;
begin
  if FileExists(FileName) then DeleteFile(FileName);

  Cfg:=TXMLConfig.Create(FileName);
  Try
    SaveContent(Cfg);
  Finally
    Cfg.Flush;
    FreeThenNil(Cfg);
  end;
end;

procedure TCustomGrid.LoadFromFile(FileName: string);
var
  Cfg: TXMLConfig;
  Version: Integer;
begin
  if not FileExists(FileName) then
    raise Exception.Create(rsGridFileDoesNotExists);
    
  Cfg:=TXMLConfig.Create(FileName);
  Try
    Version:=cfg.GetValue('grid/version',-1);
    if Version=-1 then raise Exception.Create(rsNotAValidGridFile);
    BeginUpdate;
    LoadContent(Cfg, Version);
    EndUpdate(True);
  Finally
    FreeThenNil(Cfg);
  end;
end;

procedure TCustomGrid.Clear;
var
  OldR,OldC: Integer;
begin
  OldR:=RowCount;
  OldC:=ColCount;
  FFixedCols:=0;
  FFixedRows:=0;
  FRows.Count:=0;
  FCols.Count:=0;
  FTopLeft:=Point(-1,-1);
  VisualChange;
  SizeChanged(OldR,OldC);
end;

procedure TCustomGrid.AutoAdjustColumns;
var
  i: Integer;
begin
  For i:=0 to ColCount do
    AutoAdjustColumn(i);
end;

{ TVirtualGrid }

function TVirtualGrid.GetCells(Col, Row: Integer): PCellProps;
begin
  // todo: Check range
  Result:=nil;
  if (Col<0) or (Row<0) or (Col>=ColCount) or (Row>=RowCount) then
    raise EGridException.CreateFmt(rsIndexOutOfRange, [Col, Row]);
  Result:=FCells[Col,Row];
end;

function Tvirtualgrid.Getrows(Row: Integer): PColRowprops;
begin
  Result:= FRows[Row, 0];
end;

function Tvirtualgrid.Getcols(Col: Integer): PColRowProps;
begin
  result:=FCols[Col, 0];
end;

procedure TVirtualGrid.SetCells(Col, Row: Integer; const AValue: PCellProps);
var
   Cell: PCellProps;
begin
  // todo: Check range
  Cell:=FCells[Col,Row];
  if Cell<>nil then DisposeCell(Cell);
  Cell:=AValue;
  FCells[Col,Row]:=Cell;
end;

procedure Tvirtualgrid.Setrows(Row: Integer; const Avalue: PColRowProps);
var
   C: PColRowProps;
begin
  // todo: Check range
  C:=FRows[Row,0];
  if C<>nil then DisposeColRow(C);
  FRows[Row,0]:=AValue;
end;

procedure Tvirtualgrid.Setcolcount(const Avalue: Integer);
begin
  if FColCount=Avalue then Exit;
  {$Ifdef dbgMem}
    WriteLn('TVirtualGrid.SetColCount Value=',AValue);
  {$Endif}
  FColCount:=AValue;
  {$Ifdef dbgMem}
    write('TVirtualGrid.SetColCount->FCOLS: ');
  {$Endif}
  FCols.SetLength(FColCount, 1);
  {$Ifdef dbgMem}
    write('TVirtualGrid.SetColCount->FCELLS(',FColCount,',',FRowCount,'): ');
  {$Endif}
  FCells.SetLength(FColCount, FRowCount);
end;


procedure Tvirtualgrid.Setrowcount(const Avalue: Integer);
begin
  if FRowCount=AValue then Exit;
  {$Ifdef dbgMem}
    WriteLn('TVirtualGrid.SetRowCount Value=',AValue);
  {$Endif}
  FRowCount:=AValue;
  {$Ifdef dbgMem}
    write('TVirtualGrid.SetRowCount->FROWS: ');
  {$Endif}
  FRows.SetLength(FRowCount,1);
  {$Ifdef dbgMem}
    write('TVirtualGrid.SetRowCount->FCELLS(',FColCount,',',FRowCount,'): ');
  {$Endif}
  FCells.SetLength(FColCount, FRowCount);
end;

procedure Tvirtualgrid.Setcols(Col: Integer; const Avalue: PColRowProps);
var
   C: PColRowProps;
begin
  // todo: Check range
  C:=FCols[Col,0];
  if C<>nil then DisposeColRow(C);
  FCols[Col,0]:=AValue;
end;

procedure Tvirtualgrid.Clear;
begin
  {$Ifdef dbgMem}write('FROWS: ');{$Endif}FRows.Clear;
  {$Ifdef dbgMem}write('FCOLS: ');{$Endif}FCols.Clear;
  {$Ifdef dbgMem}write('FCELLS: ');{$Endif}FCells.Clear;
  FColCount:=0;
  FRowCount:=0;
end;

procedure Tvirtualgrid.Disposecell(var P: Pcellprops);
begin
  if P<>nil then begin
    if P^.Text<>nil then StrDispose(P^.Text);
    if P^.Attr<>nil then DisposeCellAttr(P^.Attr);
    Dispose(P);
    P:=nil;
  end;
end;

procedure TVirtualGrid.DisposeColRow(var p: PColRowProps);
begin
  if P<>nil then begin
    if P^.FixedAttr<>nil then DisposeCellAttr(P^.FixedAttr);
    if P^.NormalAttr<>nil then DisposeCellAttr(P^.NormalAttr);
    Dispose(P);
    P:=nil;
  end;
end;

function TVirtualGrid.GetDefaultCell: PcellProps;
begin
  New(Result);
  Result^.Text:=nil;
  Result^.Attr:=nil;
end;

function TVirtualGrid.GetDefaultColRow: PColRowProps;
begin
  New(Result);
  Result^.FixedAttr:=nil;
  Result^.NormalAttr:=nil;
  Result^.Size:=-1;
end;

procedure Tvirtualgrid.Dodestroyitem (Sender: Tobject; Col,Row: Integer;
  var Item: Pointer);
begin
  {$Ifdef dbgMem}
    WriteLn('TVirtualGrid.doDestroyItem Col=',Col,' Row= ',
            Row,' Item=',Integer(Item));
  {$endif}
  if Item<>nil then begin
    if (Sender=FCols)or(Sender=FRows) then begin
      DisposeColRow(PColRowProps(Item));
    end else begin
      DisposeCell(PCellProps(Item));
    end;
    Item:=nil;
  end;
end;

procedure Tvirtualgrid.doNewitem(Sender: Tobject; Col,Row:Integer;
  var Item: Pointer);
begin
  {$Ifdef dbgMem}
    WriteLn('TVirtualGrid.doNewItem Col=',Col,' Row= ',
            Row,' Item=',Integer(Item));
  {$endif}
  if Sender=FCols then begin
    // Procesar Nueva Columna
    Item:=GetDefaultColRow;
  end else
  if Sender=FRows then begin
    // Procesar Nuevo Renglon
    Item:=GetDefaultColRow;
  end else begin
    // Procesar Nueva Celda
    Item:=nil;
  end;
end;

constructor TVirtualGrid.Create;
begin
  Inherited Create;
  {$Ifdef dbg}WriteLn('TVirtualGrid.Create');{$Endif}
  FCells:=TArray.Create;
  FCells.OnDestroyItem:=@doDestroyItem;
  FCells.OnNewItem:=@doNewItem;
  FCols:= TArray.Create;
  FCols.OnDestroyItem:=@doDestroyItem;
  FCols.OnNewItem:=@doNewItem;
  FRows:=TArray.Create;
  FRows.OnDestroyItem:=@doDestroyItem;
  FRows.OnNewItem:=@doNewItem;
  RowCount:=4;
  ColCount:=4;
end;

destructor TVirtualGrid.Destroy;
begin
  {$Ifdef dbg}WriteLn('TVirtualGrid.Destroy');{$Endif}
  Clear;
  FreeThenNil(FRows);
  FreeThenNil(FCols);
  FreeThenNil(FCells);
  inherited Destroy;
end;
procedure TVirtualGrid.DeleteColRow(IsColumn: Boolean; Index: Integer);
begin
  FCells.DeleteColRow(IsColumn, Index);
  if IsColumn then begin
    FCols.DeleteColRow(True, Index);
    Dec(FColCount);
  end else begin
    FRows.DeleteColRow(True, Index);
    Dec(fRowCount);
  end;
end;

procedure TVirtualGrid.MoveColRow(IsColumn: Boolean; FromIndex, ToIndex: Integer
  );
begin
  FCells.MoveColRow(IsColumn, FromIndex, ToIndex);
  if IsColumn then FCols.MoveColRow(True, FromIndex, ToIndex)
  else             FRows.MoveColRow(True, FromIndex, ToIndex);
end;

procedure TVirtualGrid.ExchangeColRow(IsColumn: Boolean; Index,
  WithIndex: Integer);
begin
  FCells.ExchangeColRow(IsColumn, Index, WithIndex);
  if IsColumn then FCols.ExchangeColRow(true, Index, WithIndex)
  else             FRows.ExchangeColRow(True, Index, WithIndex);
end;

{
procedure TStringCellEditor.WndProc(var TheMessage: TLMessage);
begin
  write(Name,'.WndProc msg= ');
  case TheMessage.Msg of
    LM_SHOWWINDOW: WriteLn('LM_SHOWWINDOW');
    LM_SETFOCUS: WriteLn('LM_SETFOCUS');
    LM_PAINT: WriteLn('LM_PAINT');
    LM_KEYUP: WriteLn('LM_KEYUP');
    LM_WINDOWPOSCHANGED: WriteLn('LM_WINDOWPOSCHANGED');
    LM_MOVE: WriteLn('LM_MOVE');
    LM_KILLFOCUS: WriteLn('LM_KILLFOCUS');
    CM_BASE..CM_MOUSEWHEEL:
      begin
        case TheMessage.Msg of
          CM_MOUSEENTER: WriteLn('CM_MOUSEENTER');
          CM_MOUSELEAVE: WriteLn('CM_MOUSELEAVE');
          CM_VISIBLECHANGED: WriteLn('CM_VISIBLECHANGED');
          CM_TEXTCHANGED: WriteLn('CM_TEXTCHANGED');
          CM_SHOWINGCHANGED: WriteLn('CM_SHOWINGCHANGED');
          else WriteLn('CM_BASE + ',TheMessage.Msg-CM_BASE);
        end

      end;
    CN_BASE..CN_NOTIFY:
      begin
        WriteLn('CN_BASE + ',TheMessage.Msg-CN_BASE);
      end;
    else
      WriteLn(TheMessage.Msg,' (',IntToHex(TheMessage.Msg, 4),')');
  end;
  inherited WndProc(TheMessage);
end;
}
{ TStringCellEditor }
procedure TStringCellEditor.KeyDown(var Key: Word; Shift: TShiftState);
  procedure doInherited;
  begin
    inherited keyDown(key, shift);
    key:=0;
  end;
  function AtStart: Boolean;
  begin
    Result:= (SelStart=0);
  end;
  function AtEnd: Boolean;
  begin
    Result:= (SelStart+1)>Length(Text);
  end;
begin
  {$IfDef dbg}
  WriteLn('INI: Key=',Key,' SelStart=',SelStart,' SelLenght=',SelLength);
  {$Endif}
  {
  case Key of
    VK_LEFT:  if AtStart then doInherited;
    VK_RIGHT: if AtEnd then doInherited;
  end;
  }
  if FGrid<>nil then begin
    Fgrid.EditorKeyDown(Self, Key, Shift);
  end;
  inherited keyDown(key, shift);
  {$IfDef dbg}
  WriteLn('FIN: Key=',Key,' SelStart=',SelStart,' SelLenght=',SelLength);
  {$Endif}
end;


procedure TStringCellEditor.msg_SetValue(var Msg: TGridMessage);
begin
  Text:=Msg.Value;
end;

procedure TStringCellEditor.msg_GetValue(var Msg: TGridMessage);
begin
  Msg.Value:=Text;
end;

procedure TStringCellEditor.msg_SetGrid(var Msg: TGridMessage);
begin
  FGrid:=Msg.Grid;
  Msg.Options:=EO_AUTOSIZE or EO_HOOKEXIT or EO_SELECTALL;
end;

procedure TStringCellEditor.msg_SelectAll(var Msg: TGridMessage);
begin
  SelectAll;
end;

{ TDrawGrid }

function TDrawGrid.GetCellAttr(ACol, ARow: Integer): TCellAttr;
var
  c: PCellProps;
begin
  C:=FGrid.Celda[ACol,ARow];
  if (C<>nil)and(C^.Attr<>nil) then Result:=C^.Attr^
  else                              Result:=FDefCellAttr;
end;

function TDrawGrid.GetCellAlign(ACol, ARow: Integer): Integer;
var
  Attr: TCellAttr;
begin
  Attr:=GetCellAttr(Acol,ARow);
  AttrToCellAlign(Attr, Result);
end;

function TDrawGrid.GetCellColor(ACol, ARow: Integer): TColor;
var
  Attr: TCellAttr;
begin
  Attr:=GetCellAttr(ACol,ARow);
  Result:=Attr.Color;
end;

function TDrawGrid.GetCellFontCOlor(ACol, ARow: Integer): TColor;
var
  Attr: TCellAttr;
begin
  Attr:=GetCellAttr(ACol,ARow);
  Result:=Attr.FontColor;
end;

function TDrawGrid.GetColAlign(aCol: Integer): Integer;
var
  Attr: TCellAttr;
begin
  Attr:=GetColAttr(aCol);
  AttrToCellAlign(Attr, Result);
end;

function TDrawGrid.GetColAttr(aCol: Integer): TCellAttr;
var
  c: PColRowProps;
begin
  C:=FGrid.Cols[ACol];
  if (C<>nil)and(C^.NormalAttr<>nil) then Result:=C^.NormalAttr^
  else                                    Result:=FDefCellAttr;
end;

function TDrawGrid.GetColColor(aCol: Integer): TColor;
var
  Attr: TCellAttr;
begin
  Attr:=GetColAttr(ACol);
  Result:=Attr.Color;
end;

function TDrawGrid.GetColFontColor(aCol: Integer): TColor;
var
  Attr: TCellAttr;
begin
  Attr:=GetColAttr(ACol);
  Result:=Attr.FontColor;
end;

function TDrawGrid.GetFixedColor: TColor;
begin
  Result:=fDefFixedCellAttr.Color;
end;

function TDrawGrid.GetRowAlign(aRow: Integer): Integer;
var
  Attr: TCellAttr;
begin
  Attr:=GetRowAttr(aRow);
  AttrToCellAlign(Attr, Result);
end;

function TDrawGrid.GetRowAttr(aRow: Integer): TCellAttr;
var
  c: PColRowProps;
begin
  C:=FGrid.Rows[ARow];
  if (C<>nil)and(C^.NormalAttr<>nil) then Result:=C^.NormalAttr^
  else                                    Result:=FDefCellAttr;
end;

function TDrawGrid.GetRowColor(aRow: Integer): TColor;
var
  Attr: TCellAttr;
begin
  Attr:=GetRowAttr(ARow);
  Result:=Attr.Color;
end;

function TDrawGrid.GetRowFontColor(aRow: Integer): TColor;
var
  Attr: TCellAttr;
begin
  Attr:=GetRowAttr(ARow);
  Result:=Attr.FontColor;
end;

procedure TDrawGrid.SetDefFixedCellAttr(const AValue: TCellAttr);
begin
  if CellAttrIgual(FDefFixedCellAttr, AValue) then Exit;
  FDefFixedCellAttr:=AValue;
  Invalidate;
end;

procedure TDrawGrid.SetDefSelCellAttr(const AValue: TCellAttr);
begin
  if CellAttrIgual(FDefSelCellAttr, AValue) then Exit;
  FDefSelCellAttr:=AValue;
  Invalidate;
end;

procedure TDrawGrid.SetCellAlign(ACol, ARow: Integer; const AValue: Integer);
var
  Attr: TCellAttr;
begin
  Attr:=GetCellAttr(ACol, Arow);
  CellAlignToAttr(aValue, Attr);
  SetCellAttr(ACol,ARow,Attr);
end;

procedure TDrawGrid.SetCellAttr(ACol, ARow: Integer; const AValue: TCellAttr);
var
  c: PCellProps;
  IsNew: Boolean;
begin
  C:=FGrid.Celda[ACol,ARow];
  IsNew:=C=nil;
  if IsNew then C:=FGrid.GetDefaultCell;
  if C^.Attr=nil then New(C^.Attr);
  C^.Attr^:=Avalue;
  if IsNew then FGrid.Celda[aCol,ARow]:=C; // Celda takes care
  InvalidateCell(aCol,aRow);
end;

procedure TDrawGrid.SetCellColor(ACol, ARow: Integer; const AValue: TColor);
var
  Attr: TCellAttr;
begin
  Attr:=GetCellAttr(ACol, Arow);
  attr.Color:=AValue;
  SetCellAttr(ACol,ARow,Attr);
end;

procedure TDrawGrid.SetCellFontCOlor(ACol, ARow: Integer; const AValue: TColor);
var
  Attr: TCellAttr;
begin
  Attr:=GetCellAttr(ACol, Arow);
  Attr.FontColor:=Avalue;
  SetCellAttr(ACol,ARow,Attr);
end;

procedure TDrawGrid.SetColAlign(aCol: Integer; const AValue: Integer);
var
  Attr: TCellAttr;
begin
  Attr:=GetColAttr(ACol);
  CellAlignToAttr(aValue, Attr);
  SetColAttr(aCol, Attr);
end;

procedure TDrawGrid.SetColAttr(aCol: Integer; const AValue: TCellAttr);
var
  c: PColRowProps;
  IsNew: Boolean;
begin
  C:=FGrid.Cols[ACol];
  IsNew:=C=nil;
  if IsNew then C:=FGrid.GetDefaultColRow;
  if C^.NormalAttr=nil then New(C^.NormalAttr);
  C^.NormalAttr^:=Avalue;
  if IsNew then FGrid.Cols[aCol]:=C; // Celda takes care
  InvalidateCol(aCol);
end;

procedure TDrawGrid.SetColColor(aCol: Integer; const AValue: TColor);
var
  Attr: TCellAttr;
begin
  Attr:=GetColAttr(ACol);
  Attr.Color:=AValue;
  SetColAttr(aCol, Attr);
end;

procedure TDrawGrid.SetColFontColor(aCol: Integer; const AValue: TColor);
var
  Attr: TCellAttr;
begin
  Attr:=GetColAttr(ACol);
  Attr.FontColor:=AValue;
  SetColAttr(aCol, Attr);
end;

procedure TDrawGrid.SetDefaultCellAttr(const AValue: TCellAttr);
begin
  if CellAttrIgual(FDefCellAttr, AValue) then Exit;
  FDefCellAttr:=AValue;
  Invalidate;
end;

procedure TDrawGrid.SetFixedColor(const AValue: TColor);
begin
  if fDefFixedCellAttr.Color=AValue then exit;
  fDefFixedCellAttr.Color:=aValue;
  Invalidate;
end;

procedure TDrawGrid.SetRowAlign(aRow: Integer; const AValue: Integer);
var
  Attr: TCellAttr;
begin
  Attr:=GetRowAttr(ARow);
  CellAlignToAttr(Avalue, Attr);
  SetRowAttr(aRow, Attr);
end;

procedure TDrawGrid.SetRowAttr(aRow: Integer; const AValue: TCellAttr);
var
  c: PColRowProps;
  IsNew: Boolean;
begin
  C:=FGrid.Rows[aRow];
  IsNew:=C=nil;
  if IsNew then C:=FGrid.GetDefaultColRow;
  if C^.NormalAttr=nil then New(C^.NormalAttr);
  C^.NormalAttr^:=Avalue;
  if IsNew then FGrid.Rows[aRow]:=C; // Celda takes care
  InvalidateRow(aRow);
end;

procedure TDrawGrid.SetRowColor(aRow: Integer; const AValue: TColor);
var
  Attr: TCellAttr;
begin
  Attr:=GetRowAttr(ARow);
  Attr.Color:=AValue;
  SetRowAttr(aRow, Attr);
end;

procedure TDrawGrid.SetRowFontColor(aRow: Integer; const AValue: TColor);
var
  Attr: TCellAttr;
begin
  Attr:=GetRowAttr(ARow);
  Attr.FontColor:=AValue;
  SetRowAttr(aRow, Attr);
end;

function TDrawGrid.getFixedColAlign(aCol: Integer): Integer;
var
  Attr: TCellAttr;
begin
  Attr:=GetFixedColAttr(aCol);
  AttrToCellAlign(Attr, Result);
end;

function TDrawGrid.getFixedColAttr(aCol: Integer): TCellAttr;
var
  c: PColRowProps;
begin
  C:=FGrid.Cols[ACol];
  if (C<>nil)and(C^.FixedAttr<>nil) then Result:=C^.FixedAttr^
  else                                   Result:=FDefFixedCellAttr;
end;

function TDrawGrid.getFixedColFontColor(aCol: Integer): TColor;
var
  Attr: TCellAttr;
begin
  Attr:=getFixedColAttr(ACol);
  Result:=Attr.FontColor;
end;

function TDrawGrid.getFixedRowAlign(aRow: Integer): Integer;
var
  Attr: TCellAttr;
begin
  Attr:=getFixedRowAttr(aRow);
  AttrToCellAlign(Attr, Result);
end;

function TDrawGrid.getFixedRowAttr(aRow: Integer): TCellAttr;
var
  c: PColRowProps;
begin
  C:=FGrid.Rows[ARow];
  if (C<>nil)and(C^.FixedAttr<>nil) then Result:=C^.FixedAttr^
  else                                   Result:=FDefFixedCellAttr;
end;

function TDrawGrid.getFixedRowFontColor(aRow: Integer): TColor;
var
  Attr: TCellAttr;
begin
  Attr:=getFixedRowAttr(ARow);
  Result:=Attr.FontColor;
end;

function TDrawGrid.getfixedColColor(aCol: Integer): TColor;
var
  Attr: TCellAttr;
begin
  Attr:=getFixedColAttr(ACol);
  Result:=Attr.Color;
end;

function TDrawGrid.getfixedRowColor(aRow: Integer): TColor;
var
  Attr: TCellAttr;
begin
  Attr:=getFixedRowAttr(ARow);
  Result:=Attr.Color;
end;

procedure TDrawGrid.setFixedColAlign(aCol: Integer; const AValue: Integer);
var
  Attr: TCellAttr;
begin
  Attr:=getFixedColAttr(aCol);
  CellAlignToAttr(aValue, Attr);
  setFixedColAttr(aCol, Attr);
end;

procedure TDrawGrid.setFixedColAttr(aCol: Integer; const AValue: TCellAttr);
var
  c: PColRowProps;
  IsNew: Boolean;
begin
  C:=FGrid.Cols[ACol];
  IsNew:=C=nil;
  if IsNew then C:=FGrid.GetDefaultColRow;
  if C^.FixedAttr=nil then New(C^.FixedAttr);
  C^.FixedAttr^:=Avalue;
  if IsNew then FGrid.Cols[aCol]:=C; // Celda takes care
  InvalidateCol(aCol);
end;

procedure TDrawGrid.setFixedcolFontColor(aCol: Integer; const AValue: TColor);
var
  Attr: TCellAttr;
begin
  Attr:=GetFixedColAttr(ACol);
  Attr.FontColor:=AValue;
  SetFixedColAttr(aCol, Attr);
end;

procedure TDrawGrid.setFixedRowAlign(aRow: Integer; const AValue: Integer);
var
  Attr: TCellAttr;
begin
  Attr:=getFixedRowAttr(ARow);
  CellAlignToAttr(Avalue, Attr);
  setFixedRowAttr(aRow, Attr);
end;

procedure TDrawGrid.setFixedRowAttr(aRow: Integer; const AValue: TCellAttr);
var
  c: PColRowProps;
  IsNew: Boolean;
begin
  C:=FGrid.Rows[aRow];
  IsNew:=C=nil;
  if IsNew then C:=FGrid.GetDefaultColRow;
  if C^.FixedAttr=nil then New(C^.FixedAttr);
  C^.FixedAttr^:=Avalue;
  if IsNew then FGrid.Rows[aRow]:=C; // Celda takes care
  InvalidateRow(ARow);
end;

procedure TDrawGrid.setFixedRowFontColor(aRow: Integer; const AValue: TColor);
var
  Attr: TCellAttr;
begin
  Attr:=getFixedRowAttr(ARow);
  Attr.FontColor:=AValue;
  setFixedRowAttr(aRow, Attr);
end;

procedure TDrawGrid.setfixedColColor(aCol: Integer; const AValue: TColor);
var
  Attr: TCellAttr;
begin
  Attr:=getFixedColAttr(ACol);
  Attr.Color:=AValue;
  setFixedColAttr(aCol, Attr);
end;

procedure TDrawGrid.setFixedRowColor(aRow: Integer; const AValue: TColor);
var
  Attr: TCellAttr;
begin
  Attr:=getFixedRowAttr(ARow);
  Attr.Color:=AValue;
  setFixedRowAttr(aRow, Attr);
end;


procedure TDrawGrid.CalcCellExtent(acol, aRow: Integer; var aRect: TRect);
begin
  //
end;

procedure TDrawGrid.DrawCell(aCol,aRow: Integer; aRect: TRect;
  aState:TGridDrawState);
begin
  if Assigned(OnDrawCell) and not(CsDesigning in ComponentState) then
    OnDrawCell(Self,aCol,aRow,aRect,aState)
  else
    DefaultDrawCell(aCol,aRow,aRect,aState);
  Inherited DrawCellGrid(aRect,aCol,aRow,aState); // Draw the grid
end;

procedure TDrawGrid.DrawFocusRect(aCol, aRow: Integer; ARect: TRect;
  aState: TGridDrawstate);
begin
  // Draw focused cell if we have the focus
  if Self.Focused Or (ShouldEdit and ((Feditor=nil)or not Feditor.Focused)) then begin
    if (gdFocused in aState)then begin
      Canvas.Pen.Color:=FFocusColor;
      Canvas.Pen.Style:=psDot;
      if goRowSelect in Options then begin
        Canvas.MoveTo(FGCache.FixedWidth+1, aRect.Top+1);
        Canvas.LineTo(FGCache.MaxClientXY.x-1, aRect.Top+1);
        Canvas.LineTo(FGCache.MaxClientXY.x-1, aRect.Bottom-1);
        Canvas.LineTo(FGCache.FixedWidth+1, aRect.Bottom-1);
        Canvas.LineTo(FGCache.FixedWidth+1, aRect.Top+1);
      end else begin
        Canvas.MoveTo(aRect.Left+1, aRect.Top+1);
        Canvas.LineTo(ARect.Right-1, ARect.Top+1);
        Canvas.LineTo(aRect.Right-1, aRect.bottom-1);
        Canvas.LineTo(aRect.Left+1, aRect.Bottom-1);
        Canvas.Lineto(aRect.left+1, aRect.top+1);
      end;
      Canvas.Pen.Style:=psSolid;
    end;
  end;
end;

procedure TDrawGrid.ColRowExchanged(IsColumn:Boolean; Index, WithIndex: Integer);
begin
  Fgrid.ExchangeColRow(IsColumn, Index, WithIndex);
  if Assigned(OnColRowExchanged) then
    OnColRowExchanged(Self, IsColumn, Index, WithIndex);
end;

procedure TDrawGrid.ColRowDeleted(IsColumn: Boolean; Index: Integer);
begin
  FGrid.DeleteColRow(IsColumn, Index);
  if Assigned(OnColRowDeleted) then
    OnColRowDeleted(Self, IsColumn, Index, Index);
end;

procedure TDrawGrid.ColRowMoved(IsColumn: Boolean; FromIndex, ToIndex: Integer);
begin
  FGrid.MoveColRow(IsColumn, FromIndex, ToIndex);
  if Assigned(OnColRowMoved) then
    OnColRowMoved(Self, IsColumn, FromIndex, toIndex);
end;

procedure TDrawGrid.HeaderClick(IsColumn: Boolean; Index: Integer);
begin
  inherited HeaderClick(IsColumn, Index);
  if Assigned(OnHeaderClick) then OnHeaderClick(Self, IsColumn, Index);
end;

procedure TDrawGrid.SizeChanged(OldColCount, OldRowCount: Integer);
begin
  if OldColCount<>ColCount then fGrid.ColCount:=ColCOunt;
  if OldRowCount<>RowCount then fGrid.RowCount:=RowCount;
end;

procedure TDrawGrid.SetColor(Value: TColor);
begin
  FDefCellAttr.Color:=Value;
  inherited SetColor(Value);
  Invalidate;
  WriteLn('TDrawGrid.SetColor Changed');
end;

procedure TDrawGrid.SaveContent(cfg: TXMLConfig);
var
  i,j,k: Integer;
  c: PCellProps;
  cr: PColRowProps;
  path: string;

  procedure SaveAttr(Attr: PCellAttr; FNStr: String);
  begin
    with Attr^ do begin
      Cfg.SetValue(Path+FNStr+'/color', ColorToString(Color));
      Cfg.SetValue(Path+FNStr+'/fontcolor',ColorToString(FontColor));
      Cfg.SetValue(Path+FNStr+'/textstyle/alignment/value', Ord(TextStyle.Alignment));
      cfg.SetValue(Path+FNStr+'/textstyle/layout/value',Ord(TextStyle.Layout));
      cfg.SetValue(Path+FNStr+'/textstyle/singleLine/value',TextStyle.SingleLine);
      cfg.SetValue(Path+FNStr+'/textstyle/clipping/value',TextStyle.Clipping);
      cfg.SetValue(Path+FNStr+'/textstyle/wordbreak/value',TextStyle.WordBreak);
      cfg.SetValue(Path+FNStr+'/textstyle/opaque/value',TextStyle.Opaque);
      cfg.SetValue(Path+FNStr+'/textstyle/systemfont/value',TextStyle.SystemFont);
    end;
  end;
begin
  Inherited SaveContent(cfg);
  Cfg.SetValue('grid/saveoptions/attributes', soAttributes in SaveOptions);
  if not (soAttributes in SaveOptions) then Exit;
  
  // Save Columns
  j:=0;
  For i:=0 to ColCount-1 do begin
    cr:=fGrid.Cols[i];
    if (cr<>nil) then
      if (cr^.NormalAttr<>nil)or(cr^.FixedAttr<>nil) then begin
        Inc(j);
        Cfg.SetValue('grid/attributes/columns/columncount', j);
        path:='grid/attributes/columns/column'+IntToStr(j);
        if Cr^.NormalAttr<>nil then begin
          Cfg.SetValue(Path+'/normal/index', i);
          SaveAttr(cr^.NormalAttr, '/normal');
        end;
        if Cr^.FixedAttr<>nil then begin
          Cfg.SetValue(Path+'/fixed/index', i);
          SaveAttr(cr^.FixedAttr, '/fixed');
        end;
      end;
  end;
  
  // Save Rows
  j:=0;
  For i:=0 to RowCount-1 do begin
    cr:=fGrid.Rows[i];
    if (cr<>nil) then
      if (cr^.NormalAttr<>nil)or(cr^.FixedAttr<>nil) then begin
        Inc(j);
        Cfg.SetValue('grid/attributes/rows/rowcount', j);
        Path:='grid/attributes/rows/row'+IntToStr(j);
        if cr^.NormalAttr<>nil then begin
          cfg.SetValue(Path+'/normal/index',i);
          SaveAttr(cr^.NormalAttr, '/normal');
        end;
        if Cr^.FixedAttr<>nil then begin
          cfg.SetValue(Path+'/fixed/index',i);
          SaveAttr(cr^.fixedAttr, '/fixed');
        end;
      end;
  end;
  
  // Save attributtes of Cells
  k:=0;
  For i:=0 to ColCount-1 do
    For j:=0 to RowCount-1 do begin
      C:=fGrid.Celda[i,j];
      if (c<>nil)and(c^.Attr<>nil) then begin
        Inc(k);
        Cfg.SetValue('grid/attributes/cells/cellcount',k);
        Path:='grid/attributes/cells/cell'+IntToStr(k);
        cfg.SetValue(Path+'/column',i);
        cfg.SetValue(Path+'/row',j);
        SaveAttr(C^.Attr, '');
      end;
    end;
end;

procedure TDrawGrid.LoadContent(Cfg: TXMLConfig; Version: Integer);

  Procedure LoadAttr(path:String; IsColumn,IsFixed:Boolean);
  Var
    j: Integer;
  begin
    j:=cfg.getValue(Path+'/index', -1);
    if (j>=0)and(j<=Colcount-1) then begin
      if IsFixed Then begin
        If IsColumn Then FixedColAttr[j]:=LoadCellAttrFromXMLPath(cfg, Path)
        Else             FixedRowAttr[j]:=LoadCellAttrFromXMLPath(cfg, Path);
      End Else begin
        If IsColumn Then ColAttr[j]:=LoadCellAttrFromXMLPath(cfg, Path)
        Else             RowAttr[j]:=LoadCellAttrFromXMLPath(cfg, Path);
      End;
    End;
  End;
var
  i,j,k: Integer;
  B: Boolean;
  path: string;
begin

  Inherited LoadContent(Cfg, Version);

  if not (soAttributes in SaveOptions) then Exit;
  B:=Cfg.GetValue('grid/saveoptions/attributes',false);
  if not B then Exit;

  // Load Columns
  k:=cfg.getValue('grid/attributes/columns/columncount',0);
  For i:=1 to k do begin
    // Normal
    Path:='grid/attributes/columns/column'+IntToStr(i);
    if Version<3 then begin
      LoadAttr(Path, true, false);
    end else begin
      LoadAttr(Path+'/normal', true, false);
      LoadAttr(Path+'/fixed',  true, true);
    End;
  end;
  
  // Load Rows
  k:=cfg.getValue('grid/attributes/rows/rowcount',0);
  For i:=1 to k do begin
    Path:='grid/attributes/rows/row'+IntToStr(i);
    if Version<3 then begin
      LoadAttr(Path, false, false);
    end else begin
      LoadAttr(Path+'/normal', false, false);
      LoadAttr(Path+'/fixed',  false, true);
    end;
  end;
  
  // Load Cells
  Path:='grid/attributes/cells/';
  k:=cfg.getValue(Path+'cellcount',0);
  while k>0 do begin
    i:=cfg.getValue(Path+'cell'+inttoStr(k)+'/column', -1);
    j:=cfg.getValue(Path+'cell'+inttostr(k)+'/row', -1);
    if (j>=0)and(j<=rowcount-1)and(i>=0)and(i<=Colcount-1) then begin
      CellAttr[i,j]:=LoadCellAttrFromXMLPath(cfg, Path+'cell'+IntToStr(k));
    end;
    dec(k);
  end;
end;

constructor TDrawGrid.Create(AOwner: TComponent);
begin
  fGrid:=TVirtualGrid.Create;

  FDefCellAttr:=GetDefaultCellAttr;
  FDefSelCellAttr:=FDefCellAttr;
  with FDefSelCellAttr do begin
    Color:=clBlack;
    FontColor:=clWhite;
  end;
  fdefFixedCellAttr:=FDefCelLAttr;
  fdefFixedCellAttr.Color:=clBtnFace;

  inherited Create(AOwner);
end;

destructor TDrawGrid.Destroy;
begin
  {$Ifdef dbg}WriteLn('TDrawGrid.Destroy');{$Endif}
  
  FreeThenNil(FGrid);
  inherited Destroy;
end;

procedure TDrawGrid.DefaultDrawCell(aCol, aRow: Integer; var aRect: TRect;
  aState: TGridDrawState);
var
   c: PcellProps;
   cr: PColRowProps;
   attr: pcellattr;
begin
  // Set draw Cell Attributes
  if gdSelected in aState then FCellAttr:=FDefSelCellAttr
  else if gdFixed in aState then FCellAttr:=FDefFixedCellAttr
  else FCellAttr:=FDefcellAttr;
  
  if DefaultDrawing or (csDesigning in ComponentState) then
    FCellAttr.TextStyle.Clipping:=False
  else
    if not (gdSelected in aState) then begin
      C:= FGrid.Celda[aCol,aRow];
      if C = nil then Attr:=nil
      else            Attr:=C^.attr;
      
      if Attr=nil then
        if gdFixed in aState then begin
          case MouseToGridZone(aCol, aRow, true) of
            gzFixedRows:
              cr:=FGrid.Rows[aRow];
            gzFixedCols:
              cr:=FGrid.Cols[aCol];
            gzFixedCells:
              begin
                cr:=FGrid.Cols[aCol];
                if (cr=nil)or(cr^.fixedAttr=nil) then cr:=FGrid.Rows[aRow];
              end;
            else cr:=nil;
          end;
          if Cr<>nil then Attr:=Cr^.FixedAttr;
        end else begin
          cr:=FGrid.Cols[aCol];
          if (Cr=nil)or(Cr^.NormalAttr=nil) then Cr:=FGrid.Rows[aRow];
          if Cr<>nil then Attr:=Cr^.NormalAttr;
        end;
        
      if Attr<>nil then FCellAttr:=Attr^;
    end;
    
    if Assigned(fonCellAttr) then fonCellAttr(Self, aCol,aRow, aState, FCellAttr);
    if goColSpanning in Options then CalcCellExtent(acol, arow, aRect);
  
  Canvas.Brush.Color:=fCellAttr.Color;
  Canvas.Font.Color:=fCellAttr.FontColor;
  Canvas.FillRect(aRect);
end;

procedure TDrawGrid.RemoveCellAttr(aCol, aRow: Integer);
var
  c: PCellProps;
begin
  c:=FGrid.Celda[aCol,aRow];
  if c<>nil then begin
    if c^.Attr<>nil then begin
      Dispose(c^.Attr);
      c^.Attr:=nil;
      InvalidateCell(aCol,aRow);
    End;
  End;
end;

procedure TDrawGrid.RemoveColRowAttr(Index: Integer; IsCol, IsFixed: Boolean);
  procedure RemoveAttr(Var attr: PCellAttr);
  begin
    If Attr<>nil then begin
      Dispose(Attr);
      Attr:=nil;
      If IsCol Then InvalidateCol(Index)
      Else          InvalidateRow(Index);
    End;
  End;
var
  Cr: PColRowProps;
begin
  If IsCol Then Cr:=FGrid.Cols[Index]
  Else          Cr:=FGrid.Rows[Index];
  if Cr<>nil then begin
    If IsFixed Then RemoveAttr(Cr^.FixedAttr)
    Else            RemoveAttr(Cr^.NormalAttr);
  End;
end;

{ TStringGrid }

function TStringGrid.Getcells(aCol, aRow: Integer): string;
var
   C: PCellProps;
begin
  Result:='';
  C:=FGrid.Celda[aCol,aRow];
  if C<>nil then Result:=C^ .Text;
end;

function TStringGrid.GetCols(Index: Integer): TStrings;
var
  i,j: Integer;
begin
  Result:=nil;
  if (ColCount>0)and(index>=0)and(Index<ColCount) then begin
    Result:=TStringList.Create;
    For i:=0 to RowCount-1 do begin
      j:=Result.Add( Cells[Index, i] );
      Result.Objects[j]:=Objects[Index, i];
    end;
  end;
end;

function TStringGrid.GetObjects(ACol, ARow: Integer): TObject;
var
  C: PCellProps;
begin
  Result:=nil;
  C:=Fgrid.Celda[aCol,aRow];
  if C<>nil then Result:=C^.Data;
end;

function TStringGrid.GetRows(Index: Integer): TStrings;
var
  i,j: Integer;
begin
  Result:=nil;
  if (RowCount>0)and(index>=0)and(Index<RowCount) then begin
    Result:=TStringList.Create;
    For i:=0 to ColCount-1 do begin
      j:=Result.Add( Cells[i, Index] );
      Result.Objects[j]:=Objects[i, Index];
    end;
  end;
end;

procedure TStringGrid.Setcells(aCol, aRow: Integer; const Avalue: string);
var
  C: PCellProps;
begin
  C:= FGrid.Celda[aCol,aRow];
  if C<>nil then begin
    if C^.Text<>nil then StrDispose(C^.Text);
    C^.Text:=StrNew(pchar(aValue));
    InvalidateCell(aCol, aRow);
  end else begin
    if AValue<>'' then begin
      New(C);
      C^.Text:=StrNew(pchar(Avalue));
      C^.Attr:=nil;
      FGrid.Celda[aCol,aRow]:=C;
      InvalidateCell(aCol, aRow);
    end;
  end;
end;

procedure TStringGrid.SetCols(Index: Integer; const AValue: TStrings);
begin

end;

procedure TStringGrid.SetObjects(ACol, ARow: Integer; AValue: TObject);
var
  c: PCellProps;
begin
  C:=FGrid.Celda[aCol,aRow];
  if c<>nil then C^.Data:=AValue
  else begin
    c:=fGrid.GetDefaultCell;
    c^.Data:=Avalue;
    FGrid.Celda[aCol,aRow]:=c;
  end;
end;

procedure TStringGrid.SetRows(Index: Integer; const AValue: TStrings);
begin

end;

procedure TStringGrid.AutoAdjustColumn(aCol: Integer);
var
  i,W: Integer;
  Ts: TSize;
begin
  if (aCol<0)or(aCol>ColCount-1) then Exit;
  W:=0;
  For i:=0 to RowCount-1 do begin
    Ts:=Canvas.TextExtent(Cells[aCol, i]);
    if Ts.Cx>W then W:=Ts.Cx;
  end;
  if W=0 then W:=DefaultColWidth
  else        W:=W + 8;
  ColWidths[aCol]:=W;
end;

procedure TStringGrid.CalcCellExtent(acol, aRow: Integer; var aRect: TRect);
var
  S: string;
  Ts: Tsize;
  nc: PcellProps;
  i: integer;
begin
  inherited CalcCellExtent(acol,arow, aRect);
  S:=Cells[aCol,aRow];
  if not FCellAttr.TextStyle.Clipping then begin
    // Calcular el numero de celdas necesarias para contener todo
    // El Texto
    Ts:=Canvas.TextExtent(S);
    i:=aCol;
    while (Ts.Cx>(aRect.Right-aRect.Left))and(i<ColCount) do begin
      inc(i);
      Nc:=FGrid.Celda[i, aRow];
      if (nc<>nil)and(Nc^.Text<>'')then Break;
      aRect.Right:=aRect.Right + getColWidths(i);
    end;
    fcellAttr.TextStyle.Clipping:=i<>aCol;
  end;
end;

procedure TStringGrid.DrawCell(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState);
var
  S: string;
begin
  inherited DrawCell(aCol, aRow, aRect, aState);
  S:=Cells[aCol,aRow];
  if S<>'' then begin
    Canvas.TextRect(aRect, 3, 0, S, FCellAttr.TextStyle);
  end;
end;

procedure TStringGrid.doEditorGetValue;
var
  msg: TGridMessage;
begin
  if (FEditor<>nil) and FEditor.Visible then begin
    Msg.MsgID:=GM_GETVALUE;
    Msg.grid:=Self;
    Msg.Col:=FCol;
    msg.Row:=FRow;
    msg.Value:=Cells[FCol,FRow];
    FEditor.Dispatch(Msg);
    Cells[FCol,FRow]:=msg.Value;
    //FEditor.Perform(GM_GETVALUE, Integer(Self), Integer(@Msg));
  end;
  //inherited EditorGetValue;
end;

procedure TStringGrid.doEditorSetValue;
var
  msg: TGridMessage;
begin
  if FEditor<>nil then begin
    Msg.MsgID:=GM_SETVALUE;
    Msg.Grid:=Self;
    Msg.Col:=FCol;
    Msg.Row:=FRow;
    Msg.Value:=Cells[FCol,FRow];
    FEditor.Dispatch(Msg);
  end;
  //inherited EditorSetValue;
end;

procedure TStringGrid.SaveContent(cfg: TXMLConfig);
var
  i,j,k: Integer;
  c: PCellProps;
begin
  inherited SaveContent(cfg);
  cfg.SetValue('grid/saveoptions/content', soContent in SaveOptions);
  if soContent in SaveOptions then begin
    // Save Cell Contents
    k:=0;
    For i:=0 to ColCount-1 do
      For j:=0 to RowCount-1 do begin
        C:=fGrid.Celda[i,j];
        if (c<>nil) and (C^.Text<>'') then begin
          Inc(k);
          Cfg.SetValue('grid/content/cells/cellcount',k);
          cfg.SetValue('grid/content/cells/cell'+IntToStr(k)+'/column',i);
          cfg.SetValue('grid/content/cells/cell'+IntToStr(k)+'/row',j);
          cfg.SetValue('grid/content/cells/cell'+IntToStr(k)+'/text', c^.Text);
        end;
      end;
   end;
end;

procedure TStringGrid.LoadContent(Cfg: TXMLConfig; Version:Integer);
var
  ContentSaved: Boolean;
  i,j,k: Integer;
begin
  inherited LoadContent(Cfg, Version);
  if soContent in FSaveOptions then begin
    ContentSaved:=Cfg.GetValue('grid/saveoptions/content', false);
    if ContentSaved then begin
      k:=cfg.getValue('grid/content/cells/cellcount', 0);
      while k>0 do begin
        i:=cfg.GetValue('grid/content/cells/cell'+IntToStr(k)+'/column', -1);
        j:=cfg.GetValue('grid/content/cells/cell'+IntTostr(k)+'/row',-1);
        if (j>=0)and(j<=rowcount-1)and(i>=0)and(i<=Colcount-1) then
          Cells[i,j]:=cfg.GetValue('grid/content/cells/cell'+IntToStr(k)+'/text','');
        Dec(k);
      end;
    end;
  end;
end;
(*
procedure TStringGrid.DrawInteriorCells;
var
  i,j: Integer;
  gds: TGridDrawState;
  c: PCellProps;
begin
  with FGCache.VisibleGrid do
  if goColSpanning in Options then begin
    //
    // Ordered draw should be done in order to this work
    //
    Gds:=[];
    // Draw Empty (nil) cells First
    For i:=Left to Right do
      For j:=Top to Bottom do begin
        if IsCellSelected(i,j) then Continue;
        C:=Fgrid.Celda[i,j];
        if (c=nil) then DrawCell(i,j, ColRowToClientCellRect(i,j), gds);
      end;
     // Draw Cells Empty Cells (Text='') with Attribute
    For i:=Left to Right do
      For j:=Top to Bottom do begin
        if IsCellSelected(i,j) then Continue;
        if (i=FCol)or(j=FRow) then Continue;
        C:=Fgrid.Celda[i,j];
        if (c<>nil)and(C^.Text='') then
          DrawCell(i,j, ColRowToClientCellRect(i,j), gds);
      end;
    // Draw Cells not Empty (Text<>'')
    For i:=Left to Right do
      For j:=Top to Bottom do begin
        if IsCellSelected(i,j) then Continue;
        C:=Fgrid.Celda[i,j];
        if (C<>nil)and(C^.Text<>'') then
          DrawCell(i,j, ColRowToClientCellRect(i,j), gds);
      end;

    gds:=[gdSelected];
    For i:=Left To Right do
      For j:=Top to Bottom do
        if IsCellSelected(i,j) then begin
          DrawCell(i,j, colRowToClientCellRect(i,j), gds);
        end;

  end else inherited DrawInteriorCells;
end;
*)
procedure TStringGrid.SelectEditor;
begin
  if goEditing in Options then Editor:=fDefEditor;
  inherited SelectEditor;
end;

constructor TStringGrid.Create(AOWner: TComponent);
begin
  inherited Create(AOWner);
  if not (csDesigning in componentState) then begin
    FDefEditor:=TStringCellEditor.Create(nil);
    FDefEditor.Name:='Default_StringCellEditor';
    FDefEditor.Text:='';
    FDefEditor.Visible:=False;
    FDefEditor.Align:=alNone;
  end else begin
    FDefEditor:=nil;
  end;
end;

destructor TStringGrid.Destroy;
begin
  {$Ifdef dbg}WriteLn('TStringGrid.Destroy');{$Endif}
  if FdefEditor<>nil then begin
    FDefEDitor.Parent:=nil;
    FreeThenNil(FDefEditor);
  end;
  inherited Destroy;
end;

{ TGridPropertyAdapter }

{
procedure TGridPropertyAdapter.setAlign(const AValue: getAlign);
begin

end;
}
procedure TGridPropertyAdapter.setColor(const AValue: TColor);
begin
  FColor:=AValue;
end;

procedure TGridPropertyAdapter.setAlign(const AValue: Integer);
begin
  FAlign:=AValue;
end;

procedure TGridPropertyAdapter.setFont(const AValue: TFont);
begin
  FFont.Assign(aValue);
end;

procedure TGridPropertyAdapter.OnFontChange(Sender: TObject);
begin
  //
end;

procedure TGridPropertyAdapter.setAttr(Attr: TCellAttr);
begin
  // load font attributes with Attr data where apply
  FFont.Color:=Attr.Color;
  if Attr.FontData <> nil then
    with Attr.FontData^ do begin
      FFont.Name:= Face;
      FFont.Size:= Size;
      FFont.CharSet:=CharSet;
      FFont.Style:=Styles;
      FFont.Pitch:=Pitch;
    End;
  FTextStyleAdapter.TextStyle:=Attr.TextStyle;
end;

constructor TGridPropertyAdapter.create;
begin
  inherited create;
  FFont:=TFont.Create;
  FFont.OnChange:=@OnFontChange;
  FTextStyleAdapter:=TTextStyleAdapter.Create;
end;

destructor TGridPropertyAdapter.destroy;
begin
  FTextStyleAdapter.Free;
  FFont.Free;
  inherited destroy;
end;

end.

{  The_Log

VERSION: 0.8.4:
---------------
Date: 21-JAN-2003
- Moved log to the end of file
- Editor should be set in OnSelectEditor or SelectEditor in descendants.
- Added SkipUnselectable, this allow the seleccion [using UP,DOWN,LEFT,TOP,
  TABS (if goTabs)] select the next selectable cell.
- Fixed goAlwaysShowEditor
- Fixed bug (gtk-CRITICAL) when destroying the grid and the editor is visible
- Fixed bug selecting a partial visible cell while the grid is scrolled
- missing: tabb from the grid, and Shift-Tab in goTabs mode.



VERSION: 0.8.3
---------------
CHANGES   - Better Editor Support
            Renamed Editor functions
            Editors uses .Dispatch instead of .Perform
            Introduced EditorOptions:
            EO_AUTOSIZE  = Let the grid automatically resize the editor
            EO_HOOKKEYS  = Let the grid process known keydows first
            EO_HOOKEXIT  = Let the grid handle the focus
            EO_SELECTALL = Editor wants to receive SelectAll msg on Key RETURN
            EO_WANTCHAR  = Editor wants to Preview Keys on the grid (soon)
            EO_GETSETVAL = Editor wants to receive GetValue,SetValue msgs (soon)
            This Options should be set in GM_SETGRID message (msg.Options:= ..)

          - Deleted Scr1 Conditional

FIXES     Painting and Crashes at desing time

TODOS     Better editor Support
          TCustomgrid Inherited from TCustomControl to get rid of
            - published VertScrollBar
            - published HorzScrollBar
            - published AutoScroll
            - translucid look at design time?
          Detect ReadOnly grid in editors
          Detect changes in the grid.
          Column Resizing at design time
          ...


VERSION: 0.8.2
---------------
CHANGES		Demo Program

			Too many internal changes to be listed, scrollbars are now
			proportional to client/grid sizes (with goSmoothScroll option
			and almost proptional without it), removed OnEditor, etc.

ADDED		goSmoothScroll. (default) allows scroll the grid by pixel basis
            goThumbTracking. The grid acts always as if this is set, the
			  value is ignored due to current implementation, however if
			  the user set it explicitly then, when the user is scrolling,
			  the focused cell will be following the scroll position.
			goTabs.
			goAlwaysShowEditor. Still need some working

NEW			AutoAdvance. Choose where the next cell position should go
              if a RETURN or TABS(if enabled) is pressed

			  aaRight. Selected cell will go to the right
			  aaDown.  Selected cell will go to down

BUGS		goEditing:
			  - pressing RETURN doesn't edit the current cell
			  - pressing other keys doesn't start editing (need F2)
			goTabs:
			  - Shift-TAB doesn't work
			goAlwaysShowEditor:
			  - Still working :)
			...


VERSION: 0.8.1
---------------
DATE: 28-DEC-2002

CHANGES -- Continued migrating properties from TCustomGrid to TDrawGrid
	   (onCellAttr, DefaultCellAttr, FixedColor, etc.)

FIXES   -- FGrid in TDrawGrid was not destroyed
        -- goEditing now works. I mean, you can now stop showing the
	   editor at F2 (although editor needs more work)
           Default cell editor
	-- DefaultEditor parent is now TStringGrid
	-- Some fpc 1.1 issues (Mattias)


VERSION: 0.8.0
---------------
DATE: 20-DEC-2002

CHANGES Many internal changes (width,height removed from pcellsprop,
        fgrid removed from tcustomgrid, colRowToClientCellRect now
        uses col,row instead of point(col,row), cleaned DynamicArray,
        drawcells splitted in DrawFixedCells, DrawInteriorCells, DrawFocused
        so TStringGrid can implement ordered cell drawin and TCustomGrid
        draw cells is simpler, etc).

ADDED   ExchangeColRow(IsColumn: Boolean; Index, WithIndex: Integer);
        DeleteColRow(IsColumn:Boolea; Index:Integer);
        MoveColRow(IsColumn: Boolean; FromIndex, ToIndex: Integer);
        SortColRow(IsColumn: Boolean; Index: Integer);
        SortColRow(IsColumn: Boolean; Index,FromIndex,ToIndex: Integer);
        Property OnColRowMoved: TgridOperationEvent
        Property OnColRowDeleted: TgridOperationEvents
        Property OnColRowExchanged: TgridOperationEvents

ADDED   TcustomGrid derivatives can now replace sort algorithm overriding
        Sort method and using exchangeColRow as needed.


VERSION:  0.7.3
-----------------
DATE: 10-DIC-2002

ADDED goDblClickAutoSize to grid Options, Doubleclicking col's right edge
      automatically adjust column width (in TStringGrid).
      Implemented AutoAdjustColumn() and AutoAdjustColumns.

FIXED col, row increment after grid.clear don't show the grid ( if
      fixed rows-cols = 0 )

ADDED version info to saved grid files.

ADDED NEW DEMO: mysql_query. A program that connects to MySQL and shows query
      results in a grid which you can save and load.


VERSION:  0.7.2
-----------------
DATE: 5-DIC-2002
FIXED a bug that prevents col, and row sizing. MouseDown uses only Left clicks

VERSION:  0.7.1
-----------------
DATE: 3-DIC-2002
ADDED LoadFromFile and SaveToFile to XML file.
  SaveOptions   (soDesign,soPosition,soAttributes,soContent);
  soDesign:     Save & Load ColCount,RowCount,FixedCols,FixedRows,
                ColWidths, RowHeights and Options (TCustomGrid)
  soPosition:   Save & Load Scroll Position, Row, Col and Selection (TCustomGrid)
  soAttributes: Save & Load Colors, Text Alignment & Layout, etc. (TDrawGrid)
  soContent:    Save & Load Text (TStringGrid)

ADDED TCustomgrid.Clear.
                Wipe completly the grid.
ADDED goRelaxedRowSelect option
                You can see focused cell and navigate freely if goRowSelect is
                set.
FIXED Crash on reducing Rowcount


VERSION:  0.7.0
-----------------
RELEASE DATE: 30-NOV-2002

This unit version provides TCustomGrid, TDrawGrid and TStringGrid for lazarus
from the component user perpective there should be to much differences.
This release has only basic editing support.

Old Features:
  Almost all that T*Grid can do.

New Features :

  OnHeaderClick:
              Detect clicks on Row(Column) Headers, it uses a property: DragDx
              as a threshold in order to detect Col(Row) moving or clicking.

  OnCellAttr: In this Event You can easily customize the grid.
  OnDrawCell: Draw your specific cells here and then call .DefaultDrawCell
              to let the grid draw other cells.
  SortColumn,
  SortRow:    Sorting capabilities are built! you need only write one
              OnCompareCells handler to do your custom sorting needs.

  Exposed: DeleteColumn, DeleteRow, MoveColumn, MoveRow.

  RowAttr[],RowColor[],RowFontColor[],RowAlign[]
  ColAttr[],ColColor[],ColFontColor[],ColAlign[]
  CellAttr[],CellColor[],CellFontColor[],CellAlign[]

  GridLineStyle, FocusColor, etc.

Bugs:

  + Editor: it has a unneeded feature "auto cell filling" :)

  others.
}


