{

TCustomGrid, TDrawGrid and TStringGrid for Lazarus
Copyright (C) 2002  Jesus Reyes Aguilar.
email: jesusrmx@yahoo.com.mx

THIS CONTROL IS FREEWARE - USE AS YOU WILL

If you release sourcecode that uses this control, please credit me
or leave this header intact.  If you release a compiled application
that uses this code, please credit me somewhere in a little bitty
location so I can at least get bragging rights!
(Extract: from Tony's checkbook tracker, http://tony.maro.net)

This code is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

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

{.$Define dbgScroll}

unit Grids;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LCLType, LCLLinux, Controls, GraphType, Graphics,
  Forms, VCLGlobals, DynamicArray, LMessages, Messages, XMLCfg, StdCtrls,
  LResources;

Const
  //GRIDFILEVERSION = 1; // Original
  GRIDFILEVERSION = 2; // Introduced goSmoothScroll

Const
  GM_SETVALUE   = LM_USER + 100;
  GM_GETVALUE   = LM_USER + 101;
  GM_SETGRID    = LM_USER + 102;
  GM_SETPOS     = LM_USER + 103;
  GM_SELECTALL  = LM_USER + 104;

Const
  CA_LEFT     =   $1;
  CA_CENTER   =   $2;
  CA_RIGHT    =   $4;
  CL_TOP      =   $8;
  CL_CENTER   =   $10;
  CL_BOTTOM   =   $20;
  
Const
  EO_AUTOSIZE   =   $1;
  EO_HOOKKEYS   =   $2;
  EO_HOOKEXIT   =   $4;
  EO_SELECTALL  =   $8;
  EO_WANTCHAR   =   $10;
  
Type
  EGridException = class(Exception);


Type
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
  

Type
  PCellAttr=^TCellAttr;
  TCellAttr=Record
    Color: TColor;
    FontColor: TColor;
    FontFace: Pchar;
    TextStyle: TTextStyle;
  End;
      
  PCellProps= ^TCellProps;
  TCellProps=Record
    Attr: PCellAttr;
    Data: TObject;
    Text: pchar;
  End;
  
Type
  TCustomGrid = Class;
      
  PGridMessage=^TGridMessage;
  TGridMessage=Record
    MsgID: Cardinal;
    Grid: TCustomGrid;
    Col,Row: Integer;
    Value: String;
    CellRect: TRect;
    Options: Integer;
  End;

  { Default cell editor for TStringGrid }
  TStringCellEditor=Class(TCustomEdit)
  Private
    FGrid: TCustomGrid;
  Protected
    procedure KeyDown(var Key : Word; Shift : TShiftState); Override;
    Procedure msg_SetValue(Var Msg: TGridMessage); Message GM_SETVALUE;
    Procedure msg_GetValue(Var Msg: TGridMessage); Message GM_GETVALUE;
    Procedure msg_SetGrid(Var Msg: TGridMessage); Message GM_SETGRID;
    Procedure msg_SelectAll(Var Msg: TGridMessage); MEssage GM_SELECTALL;
  End;
  

  TOnDrawCell =
    Procedure(Sender: TObject; Col, Row: Integer; Rect: TRect;
              aState:TGridDrawState) of Object;
  TOnBeforeSelectionEvent =
    Procedure(Sender: TObject; Col, Row: Integer;
              Var CanChange: Boolean) of Object;
  TOnSelectEvent =
    Procedure(Sender: TObject; Col,Row: Integer) of Object;
  TOnCellAttrEvent =
    Procedure(Sender:TObject; const Col, Row:Integer; aState: TGridDrawState;
              Var CellProps: TCellAttr) of Object;
  TGridOperationEvent =
    Procedure (Sender: TObject; IsColumn:Boolean;
               sIndex,tIndex: Integer) of object;
  THdrEvent =
    Procedure(Sender: TObject; IsColumn: Boolean; Index: Integer) of Object;
  TOnCompareCells =
    Function (Sender: TObject; Acol,ARow,Bcol,BRow: Integer): Integer of Object;

  TVirtualGrid=Class
    Private
      FColCount: Integer;
      FRowCount: Integer;
      FCells, FCols, FRows: TArray;
      function GetCells(Col, Row: Integer): PCellProps;
      Function Getrows(Row: Integer): Pcellprops;
      Function Getcols(Col: Integer): Pcellprops;
      procedure SetCells(Col, Row: Integer; const AValue: PCellProps);
      Procedure Setrows(Row: Integer; Const Avalue: Pcellprops);
      Procedure Setcolcount(Const Avalue: Integer);
      Procedure Setrowcount(Const Avalue: Integer);
      Procedure Setcols(Col: Integer; Const Avalue: Pcellprops);
      Procedure DisposeCell(Var P: PcellProps);
    Protected
      Function GetDefaultCell: PcellProps;
      Procedure doDestroyItem(Sender: TObject; Col,Row:Integer; Var Item: Pointer);
      Procedure doNewItem(Sender: TObject; Col,Row:Integer; Var Item: Pointer);
      Procedure DeleteColRow(IsColumn: Boolean; Index: Integer);
      Procedure MoveColRow(IsColumn: Boolean; FromIndex, ToIndex: Integer);
      Procedure ExchangeColRow(IsColumn:Boolean; Index,WithIndex: Integer);
    Public
      Constructor Create;
      Destructor Destroy; Override;
      Procedure Clear;
      Property ColCount: Integer Read FColCount Write SetColCount;
      Property RowCount: Integer Read FRowCount Write SetRowCount;
      
      Property Celda[Col,Row: Integer]: PCellProps Read GetCells Write SetCells;
      Property Cols[Col: Integer]: PCellProps Read GetCols Write SetCols;
      property Rows[Row: Integer]: PCellProps Read GetRows Write SetRows;
  End;

      
  Type
    TGridCoord = Type TPoint;
    TGridRect  = Type TRect;

    TGridDataCache=Record
      FixedWidth: Integer;    // Sum( Fixed ColsWidths[i] )
      FixedHeight: Integer;   // Sum( Fixed RowsHeights[i] )
      GridWidth: Integer;     // Sum( ColWidths[i] )
      GridHeight: Integer;    // Sum( RowHeights[i] )
      ClientWidth: Integer;   // Width-VertScrollbar.Size
      ClientHeight: Integer;  // Height-HorzScrollbar.Size
      ScrollWidth: Integer;   // ClientWidth-FixedWidth
      ScrollHeight: Integer;  // ClientHeight-FixedHeight
      VisibleGrid: TRect;     // Visible non fixed rectagle of cells
      MaxClientXY: Tpoint;    // VisibleGrid.BottomRight coordinates
      ValidGrid: Boolean;     // true if there is something to show
      AccumWidth: TList;       // Accumulated width per column
      AccumHeight: TList;     // Accumulated Height per column
      HScrDiv,VScrDiv: Double;      // Tx const for ThumbTracking
      TLColOff,TLRowOff: Integer;   // TopLeft Offset in pixels
    End;


Type
  //TCustomGrid=Class(TScrollBox)
  //TCustomGrid=Class(TCustomControl)
  TCustomGrid=Class(TScrollingWinControl)
  Private
    FAutoAdvance: TAutoAdvance;
    FDefaultDrawing: Boolean;
    FEditor: TWinControl;
    FOnCompareCells: TOnCompareCells;
    FGridLineStyle: TPenStyle;
    FGridLineWidth: Integer;
    FDefColWidth, FDefRowHeight: Integer;
    FCol,FRow, FFixedCols, FFixedRows: Integer;
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
    FFocusing: Boolean;

    
    // Cached Values
    FGCache: TGridDataCache;
    // Options
    FOptions: TGridOptions;

    FOnDrawCell: TOnDrawcell;
    FOnBeforeSelection: TOnBeforeSelectionEvent;
    FOnSelection: TOnSelectEvent;
    FOnTopLeftChange: TNotifyEvent;
    
    Procedure AdjustCount(IsColumn:Boolean; OldValue, NewValue:Integer);
    Procedure CheckFixedCount(aCol,aRow,aFCol,aFRow: Integer);
    Procedure CacheVisibleGrid;
    function GetSelection: TGridRect;
    Function doColSizing(X,Y: Integer): Boolean;
    Function doRowSizing(X,Y: Integer): Boolean;
    Procedure doColMoving(X,Y: Integer);
    Procedure doRowMoving(X,Y: Integer);
    
    Function OffsetToColRow(IsCol,Fisical:Boolean; Offset:Integer; Var Rest:Integer): Integer;
    Function ColRowToOffset(IsCol,Fisical:Boolean; Index: Integer; Var Ini,Fin:Integer): Boolean;

    function GetLeftCol: Integer;
    function GetTopRow: Longint;
    function GetVisibleColCount: Integer;
    function GetVisibleRowCount: Integer;
    Function Getrowheights(Arow: Integer): Integer;
    Function Getcolcount: Integer;
    Function Getrowcount: Integer;
    Function Getcolwidths(Acol: Integer): Integer;
    Function GetVisibleGrid: TRect;
    Procedure MyTextRect(R: TRect; Offx,Offy:Integer; S:String; Ts: TTextStyle);
    Function ScrollToCell(Const aCol,aRow: Integer): Boolean;
    Function ScrollGrid(Relative:Boolean; DCol,DRow: Integer): TPoint;
    procedure SetDefaultDrawing(const AValue: Boolean);
    procedure SetEditor(const AValue: TWinControl);
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
    Procedure Setrowheights(Arow: Integer; Avalue: Integer);
    Procedure Setcolwidths(Acol: Integer; Avalue: Integer);
    Procedure SetColCount(Valor: Integer);
    Procedure SetRowCount(Valor: Integer);
    Procedure SetDefColWidth(Valor: Integer);
    Procedure SetDefRowHeight(Valor: Integer);
    Procedure SetCol(Valor: Integer);
    Procedure SetRow(Valor: Integer);
    Procedure doTopleftChange(DimChg: Boolean);
    Procedure TryScrollTo(aCol,aRow: integer);
    Procedure UpdateScrollBarPos(Which: TControlScrollbar);
    procedure VisualChange;
    Procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
    Procedure WMHScroll(var Message : TLMHScroll); message LM_HScroll;
    Procedure WMVScroll(var Message : TLMVScroll); message LM_VScroll;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;

  protected
    fGridState: TGridState;
    Procedure AutoAdjustColumn(aCol: Integer); Virtual;
    function  CellRect(ACol, ARow: Integer): TRect;
    
    Procedure ColRowDeleted(IsColumn: Boolean; Index: Integer); Dynamic;
    Procedure ColRowExchanged(IsColumn: Boolean; Index,WithIndex: Integer); Dynamic;
    Procedure ColRowMoved(IsColumn: Boolean; FromIndex,ToIndex: Integer); Dynamic;

    procedure ColWidthsChanged; dynamic;
    procedure DblClick; Override;
    Procedure doExit; Override;
    Procedure doEnter; Override;

    Procedure DrawEdges;
    Procedure DrawBackGround; Virtual;
    Procedure DrawFixedCells; Virtual;
    Procedure DrawInteriorCells; Virtual;
    Procedure DrawFocused; Virtual;
    Procedure DrawFocusRect(aCol,aRow:Integer; ARect:TRect; aState:TGridDrawstate); Virtual;
    Procedure DrawColRowMoving;
    procedure DrawCell(aCol,aRow:Integer; aRect:TRect; aState:TGridDrawState); Virtual;
    
    Procedure DrawByRows; Virtual;
    Procedure DrawRow(aRow: Integer); Virtual;

    Procedure HeaderClick(IsColumn: Boolean; Index: Integer); Dynamic;
    procedure InvalidateCol(ACol: Integer);
    procedure InvalidateRow(ARow: Integer);
    Procedure InvalidateCell(aCol, aRow: Integer);
    Procedure InvalidateGrid;
    procedure KeyDown(var Key : Word; Shift : TShiftState); Override;
    procedure KeyUp(var Key : Word; Shift : TShiftState); Override;
    Procedure LoadContent(cfg: TXMLConfig); Virtual;
    procedure Loaded; override;
    Procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    Procedure MouseMove(Shift: TShiftState; X,Y: Integer);Override;
    Procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    Function  MoveExtend(Relative: Boolean; DCol, DRow: Integer): Boolean;
    Procedure MoveSelection; Virtual;
    Procedure DrawCellGrid(Rect: TRect; aCol,aRow: Integer; astate: TGridDrawState);
    Procedure Paint; override;
    Procedure ResetOffset(chkCol, ChkRow: Boolean);
    procedure RowHeightsChanged; dynamic;
    function  SelectCell(ACol, ARow: Integer): Boolean; virtual;
    procedure SizeChanged(OldColCount, OldRowCount: Integer); dynamic;
    Procedure Sort(ColSorting: Boolean; Index,IndxFrom,IndxTo:Integer); virtual;
    Procedure TopLeftChanged; dynamic;
    Procedure SaveContent(cfg: TXMLConfig); Virtual;
    
    // Editor support
  Protected
    FEditorHiding: Boolean;
    FEditorKey: Boolean;
    FEditorOptions: Integer;
    Procedure EditorCancel; Virtual;
    Procedure EditorGetValue;
    Procedure EditorHide;
    Procedure EditorSetValue;
    Procedure EditorShow;
    Procedure EditorPos;
    Procedure EditorReset;
    Procedure EditorSelectAll;
    Procedure doEditorGetValue; Virtual;
    Procedure doEditorSetValue; Virtual;
  Public
    Procedure EditorExit(Sender: TObject);
    procedure EditorKeyDown(Sender: TObject; var Key:Word; Shift:TShiftState);

  Protected
    Property AutoAdvance: TAutoAdvance Read FAutoAdvance Write FAutoAdvance default aaRight;
    Property ColWidths[aCol: Integer]: Integer Read GetColWidths Write SetColWidths;
    Property ColCount: Integer Read GetColCount Write SetColCount;
    Property Col: Integer Read FCol Write SetCol;
    Property DefaultColWidth: Integer Read FDefColWidth Write SetDefColWidth;
    Property DefaultRowHeight: Integer Read FDefRowHeight write SetDefRowHeight;
    property DefaultDrawing: Boolean read FDefaultDrawing write SetDefaultDrawing default True;
    Property DragDx: Integer read FDragDx Write FDragDx;
    Property Editor: TWinControl Read FEditor Write SetEditor;
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
    Property RowCount: Integer Read GetRowCount Write SetRowCount;
    Property Row: Integer Read FRow Write SetRow;
    Property SaveOptions: TSaveOptions Read FsaveOptions Write FSaveOptions;
    Property SelectActive: Boolean read FSelectActive write SetSelectActive;
    property Selection: TGridRect read GetSelection write SetSelection;
    property TopRow: Integer read GetTopRow write SetTopRow;
    Property RowHeights[aRow: Integer]: Integer Read GetRowHeights Write SetRowHeights;
    property VisibleColCount: Integer read GetVisibleColCount;
    property VisibleRowCount: Integer read GetVisibleRowCount;

    Property OnDrawCell: TOnDrawCell Read FOnDrawCell Write FOnDrawCell;
    Property OnBeforeSelection: TOnBeforeSelectionEvent Read fOnBeforeSelection Write fOnBeforeSelection;
    Property OnSelection: TOnSelectEvent Read fOnSelection Write fOnSelection;
    Property OnTopLeftChange: TNotifyEvent Read FOnTopLeftChange Write FOnTopLeftChange;
    Property OnCompareCells: TOnCompareItem Read FOnCompareCells Write FOnCompareCells;

  Public
    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; override;
    Procedure Invalidate; Override;
    
    { Exposed procs }
    Procedure DeleteColRow(IsColumn: Boolean; Index: Integer);
    Procedure ExchangeColRow(IsColumn: Boolean; Index, WithIndex: Integer);
    Procedure MoveColRow(IsColumn: Boolean; FromIndex, ToIndex: Integer);
    Procedure SortColRow(IsColumn: Boolean; Index:Integer); Overload;
    Procedure SortColRow(IsColumn: Boolean; Index,FromIndex,ToIndex: Integer); Overload;
    
    Procedure BeginUpdate;
    Procedure AutoAdjustColumns;
    Procedure Clear;
    Procedure EndUpdate(UO: TUpdateOption); Overload;
    Procedure EndUpdate(FullUpdate: Boolean); Overload;
    Procedure LoadFromFile(FileName: String);
    Procedure SaveToFile(FileName: String);

    Function ColRowToClientCellRect(aCol, aRow: Integer): TRect;
    Function MouseToCell(Mouse: TPoint): TPoint;
    Function MouseToLogcell(Mouse: TPoint): TPoint;
    Function MouseToGridZone(X,Y: Integer; CellCoords: Boolean): TGridZone;
    Function IsCellVisible(aCol, aRow: Integer): Boolean;
    Function IscellSelected(aCol,aRow: Integer): Boolean;
  End;
      

  TDrawGrid=Class(TCustomGrid)
  Private
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

  Protected
    Procedure CalcCellExtent(acol, aRow: Integer; Var aRect: TRect); Virtual;
    Procedure DrawCell(aCol,aRow: Integer; aRect: TRect; aState:TGridDrawState); Override;
    Procedure DrawFocusRect(aCol,aRow: Integer; ARect: TRect; aState: TGridDrawstate); Override;
    Procedure ColRowExchanged(IsColumn: Boolean; Index,WithIndex: Integer); Override;
    Procedure ColRowDeleted(IsColumn: Boolean; Index: Integer); Override;
    Procedure ColRowMoved(IsColumn: Boolean; FromIndex,ToIndex: Integer); Override;
    Procedure HeaderClick(IsColumn: Boolean; Index: Integer); Override;
    procedure SizeChanged(OldColCount, OldRowCount: Integer); Override;
    Procedure SaveContent(cfg: TXMLConfig); Override;
    Procedure LoadContent(Cfg: TXMLConfig); Override;
  Public
    // to easy user call
    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; override;

    Procedure DefaultDrawCell(aCol,aRow: Integer; Var aRect: TRect; aState:TGridDrawState);
    property Canvas;

    Property ColAttr[aCol: Integer]: TCellAttr read GetColAttr write SetColAttr;
    Property ColColor[aCol: Integer]: TColor read GetColColor write SetColColor;
    Property ColFontColor[aCol: Integer]: TColor read GetColFontColor write SetColFontColor;
    Property ColAlign[aCol: Integer]: Integer read GetColAlign write SetColAlign;

    property FixedColor: TColor read GetFixedColor write SetFixedColor default clBtnFace;

    Property RowAttr[aRow: Integer]: TCellAttr read GetRowAttr write SetRowAttr;
    Property RowColor[aRow: Integer]: TColor read GetRowColor write SetRowColor;
    Property RowFontColor[aRow: Integer]: TColor read GetRowFontColor write SetRowFontColor;
    Property RowAlign[aRow: Integer]: Integer read GetRowAlign write SetRowAlign;

    Property CellAttr[ACol,ARow:Integer]: TCellAttr read GetCellAttr write SetCellAttr;
    property CellColor[ACol, ARow:Integer]: TColor read GetCellColor write SetCellColor;
    Property CellFontCOlor[ACol,ARow:Integer]: TColor read GetCellFontCOlor write SetCellFontCOlor;
    Property CellAlign[ACol,ARow: Integer]: Integer read GetCellAlign write SetCellAlign;
    Property DefaultCellAttr: TCellAttr read fDefCellAttr write SetDefaultCellAttr;
    
    Property Editor;

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
    property FixedCols;
    property RowCount;
    property FixedRows;
    property Font;
    property GridLineWidth;
    property Options;
    //property ParentBiDiMode;
    //property ParentColor;
    //property ParentCtl3D;
    //property ParentFont;
    //property ParentShowHint;
    //property PopupMenu;
    //property ScrollBars;
    //property ShowHint;
    //property TabOrder;
    //property TabStop;
    property Visible;
    property VisibleColCount;
    property VisibleRowCount;

    Property OnDrawCell;
    Property OnBeforeSelection;
    Property OnSelection;
    Property OnCellAttr: TOnCellAttrEvent read fonCellAttr Write fOnCellAttr;
    Property OnTopleftChange;
    Property OnCompareCells;
    Property OnColRowMoved: TgridOperationEvent Read FOnColRowMoved Write FOnColRowMoved;
    Property OnColRowDeleted: TgridOperationEvent Read FOnColRowDeleted Write FOnColRowDeleted;
    Property OnColRowExchanged: TgridOperationEvent Read FOnColRowExchanged Write FOnColRowExchanged;
    Property OnHeaderClick: THdrEvent Read FOnHeaderClick Write FOnHeaderClick;
  End;
  
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
      Procedure AutoAdjustColumn(aCol: Integer); Override;
      Procedure CalcCellExtent(acol, aRow: Integer; Var aRect: TRect); Override;
      Procedure DrawCell(aCol,aRow: Integer; aRect: TRect; aState:TGridDrawState); Override;
      Procedure doEditorGetValue; Override;
      Procedure doEditorSetValue; Override;
      Procedure SaveContent(cfg: TXMLConfig); Override;
      Procedure LoadContent(cfg: TXMLConfig); Override;
      Procedure DrawInteriorCells; Override;
      Procedure MoveSelection; Override;
    public
      Constructor Create(AOWner: TComponent); Override;
      Destructor Destroy; Override;
      property Cells[ACol, ARow: Integer]: string read GetCells write SetCells;
      property Cols[Index: Integer]: TStrings read GetCols write SetCols;
      property Objects[ACol, ARow: Integer]: TObject read GetObjects write SetObjects;
      property Rows[Index: Integer]: TStrings read GetRows write SetRows;
  End;
  
  Procedure DebugRect(S:String; R:TRect);
  Procedure DebugPoint(S:String; P:TPoint);
      

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

Function PointIgual(Const P1,P2: TPoint): Boolean;
begin
  result:=(P1.X=P2.X)And(P1.Y=P2.Y);
End;
Function RectIgual(Const R1,R2: TRect): Boolean;
begin
  Result:=CompareMem(@R1,@R2, SizeOf(R1));

End;
Function Min(Const I,J: Integer): Integer;
begin
  If I<J then Result:=I

  Else        Result:=J;
End;
Function Max(Const I,J: Integer): Integer;
begin
  If I>J then Result:=I

  Else        Result:=J;
End;
Function NormalizarRect(Const R:TRect): TRect;
Begin
  Result.Left:=Min(R.Left, R.Right);

  Result.Top:=Min(R.Top, R.Bottom);
  Result.Right:=Max(R.Left, R.Right);
  Result.Bottom:=Max(R.Top, R.Bottom);
End;

function GetDefaultCellAttr: TCellAttr;
begin
  With Result do begin
    FontColor:=clBlack;
    Color:=clWindow;
    FontFace:=nil;
    With TextStyle do Begin
      Alignment:=taLeftJustify;
      Layout:=tlCenter;
      SingleLine:=False;
      WordBreak:=False;
      Opaque:=False;
      Clipping:=False;
    End;
  End;
end;

procedure DebugAttr(Msg: String; Attr: TCellAttr);
Begin
  With Attr do begin
    WriteLn(Msg);
    WriteLn('Color=',ColorToString(Attr.Color));
    WriteLn('FontColor=',ColorToString(Attr.FontColor));
    With TextStyle do begin
      WriteLn('Textstyle.Alignment=', Ord(Alignment));
      WriteLn('TextStyle.Layout=',Ord(Layout));
      WriteLn('TextStyle.SingleLine=',Singleline);
      WriteLn('TextStyle.Clipping=',Clipping);
      WriteLn('TextStyle.Wordbreak=',WordBreak);
      WriteLn('TextStyle.Opaque=',Opaque);
      WriteLn('TextStyle.SystemFont',systemFont);
    End;
  End;
End;
Function LoadCellAttrFromXMLPath(Cfg: TXMLConfig; Path: String): TCellAttr;
begin
  Result:=GetDefaultCellAttr;

  With Result do begin
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
    End;
  End;
End;

Function CellAttrIgual(Const Ca1,Ca2: TCellAttr): Boolean;
begin
  Result:=CompareMem(@Ca1,@Ca2,SizeOf(Ca1));
End;

Procedure CellAlignToAttr(Align: Integer; Var Attr: TCellAttr);
begin
  With Attr.TextStyle do Begin
    If Align And CA_LEFT    = CA_LEFT     Then Alignment:=taLeftJustify Else
    If Align And CA_CENTER  = CA_CENTER   Then Alignment:=taCenter
    Else                                       Alignment:=taRightJustify;
    If Align And CL_TOP     = CL_TOP      Then Layout:=tlTop Else
    If Align AND CL_CENTER  = CL_CENTER   Then Layout:=tlCenter
    Else                                       Layout:=tlBottom;
  End;
End;

Procedure AttrToCellAlign(Attr: TCellAttr; Var Align: Integer);
begin
  With Attr.TextStyle do Begin
    Align:=0;
    Case Alignment of
      taCenter: Align:=CA_CENTER;
      taRightJustify: Align:=CA_RIGHT;
      Else Align:=CA_LEFT;
    End;
    Case Layout of
      tlTop: Align:=Align or CL_TOP;
      tlBottom: Align:=Align or CL_BOTTOM;
      Else Align:=ALign or CL_CENTER;
    end;
  End;
End;


Function TCustomGrid.Getrowheights(Arow: Integer): Integer;
Begin
  Result:=Integer(FRows[aRow]);
  if Result<0 Then Result:=fDefRowHeight;
End;

function TCustomGrid.GetTopRow: Longint;
begin
  Result:=fTopLeft.y;
end;

function TCustomGrid.GetVisibleColCount: Integer;
Var
  R: TRect;
begin
  R:=FGCache.VisibleGrid;
  Result:=r.Right-r.left+1+FFixedCols;
end;

function TCustomGrid.GetVisibleRowCount: Integer;
Var
  R: TRect;
begin
  R:=FGCache.VisibleGrid;
  Result:=r.bottom-r.top+1+FFixedRows;
end;

function TCustomGrid.GetLeftCol: Integer;
begin
  result:=fTopLeft.x;
end;

Function TCustomGrid.Getcolcount: Integer;
Begin
  Result:=FCols.Count;
End;

Function TCustomGrid.Getrowcount: Integer;
Begin
  Result:=FRows.Count;
End;

Function TCustomGrid.Getcolwidths(Acol: Integer): Integer;
Begin
  Result:=Integer(FCols[aCol]);
  if result<0 then Result:=fDefColWidth;
End;

procedure TCustomGrid.SetEditor(const AValue: TWinControl);
Var
  Msg: TGridMessage;
begin
  if Not(goEditing in Options) or (FEditor=AValue) then exit;
  FEditor:=AValue;
  if FEditor<>nil Then Begin
    Msg.MsgID:=GM_SETGRID;
    Msg.Grid:=Self;
    Msg.Options:=0;
    FEditor.Dispatch(Msg);
    FEditorOptions:=Msg.Options;
    If Msg.Options And EO_HOOKKEYS = EO_HOOKKEYS Then begin
      FEditor.OnKeyDown:=@EditorKeyDown;
    End;
    If Msg.Options And EO_HOOKEXIT = EO_HOOKEXIT Then begin
      FEditor.OnExit:=@EditorExit;
    End;
    {$IfDef EditorDbg}
    Write('SetEditor-> Editor=',FEditor.Name,' ');
    If FEditorOptions And EO_AUTOSIZE = EO_AUTOSIZE Then Write('EO_AUTOSIZE ');
    If FEditorOptions And EO_HOOKKEYS = EO_HOOKKEYS Then Write('EO_HOOKKEYS ');
    If FEditorOptions And EO_HOOKEXIT = EO_HOOKEXIT Then Write('EO_HOOKEXIT ');
    if FEditorOptions And EO_SELECTALL= EO_SELECTALL Then Write('EO_SELECTALL ');
    if FEditorOptions And EO_WANTCHAR = EO_WANTCHAR Then Write('EO_WANTCHAR ');
    WriteLn;
    {$Endif}
  End;
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
  If goRowSelect in Options Then Begin
    FRange:=Rect(FFixedCols, FRow, ColCount-1, FRow);
    FOptions:=FOptions - [goAlwaysShowEditor];
  End
  Else                           FRange:=Rect(FCol,FRow,FCol,FRow);
  If goAlwaysShowEditor in Options Then begin
    EditorShow;
  End Else begin
    EditorHide;
  End;
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

Procedure TCustomGrid.Setrowheights(Arow: Integer; Avalue: Integer);
Begin
  If AValue<0 Then AValue:=-1;
  if AValue<>Integer(FRows[ARow]) Then begin
    FRows[ARow]:=Pointer(AValue);
    VisualChange;
    RowHeightsChanged;
  End;
End;

Procedure TCustomGrid.Setcolwidths(Acol: Integer; Avalue: Integer);
Begin
  If AValue<0 Then Avalue:=-1;
  If Avalue<>Integer(FCols[ACol]) Then begin
    FCols[ACol]:=Pointer(AValue);
    VisualChange;
    ColWidthsChanged;
  end;
End;

Procedure TCustomGrid.AdjustCount(IsColumn: Boolean; OldValue, newValue: Integer);
  Procedure AddDel(Lst: TList; aCount: Integer);
  Begin
    While lst.Count<aCount do Lst.Add(Pointer(-1)); // default width/height
    Lst.Count:=aCount;
  End;
Begin
  If IsColumn Then begin
    AddDel(FCols, NewValue);
    FGCache.AccumWidth.Count:=NewValue;
    If (OldValue=0)And(NewValue>=0) Then Begin
      FTopLeft.X:=FFixedCols;
      If RowCount=0 then begin
        FFixedRows:=0;
        FTopLeft.Y:=0;
        AddDel(FRows, 1); FGCache.AccumHeight.Count:=1;
      End;
    End;
    SizeChanged(OldValue, FRows.Count);
  End else begin
    AddDel(fRows, NewValue);
    FGCache.AccumHeight.Count:=NewValue;
    If (OldValue=0)And(NewValue>=0) then Begin
      FTopleft.Y:=FFixedRows;
      If FCols.Count=0 then begin
        FFixedCols:=0;
        FTopLeft.X:=0;
        AddDel(FCols, 1); FGCache.AccumWidth.Count:=1;
      End;
    End;
    SizeChanged(FCols.Count, OldValue);
  End;
  VisualChange;
End;

Procedure TCustomGrid.SetColCount(Valor: Integer);
Var
  OldC: Integer;
begin
  If Valor=FCols.Count Then Exit;
  OldC:=FCols.Count;
  CheckFixedCount(Valor, RowCount, FFixedCols, FFixedRows);
  AdjustCount(True, OldC, Valor);
End;

Procedure TCustomGrid.SetRowCount(Valor: Integer);
Var
  OldR: Integer;
begin
  If Valor=FRows.Count Then Exit;
  OldR:=FRows.Count;
  CheckFixedCount(ColCount, Valor, FFixedCols, FFixedRows);
  AdjustCount(False, OldR, Valor);
End;

Procedure TCustomGrid.SetDefColWidth(Valor: Integer);
begin
  If Valor=fDefColwidth Then Exit;
  FDefColWidth:=Valor;
  VisualChange;
End;

Procedure TCustomGrid.SetDefRowHeight(Valor: Integer);
begin
  If Valor=fDefRowHeight Then Exit;
  FDefRowheight:=Valor;
  VisualChange;
End;

procedure TCustomGrid.SetCol(Valor: Integer);
begin
  If Valor=FCol Then Exit;
  MoveExtend(False, Valor, FRow);
end;

procedure TCustomGrid.SetRow(Valor: Integer);
begin
  If Valor=FRow Then Exit;
  MoveExtend(False, FCol, Valor);
end;

procedure TCustomGrid.Sort(ColSorting: Boolean; Index, IndxFrom, IndxTo: Integer);
  Procedure QuickSort(L,R: Integer);
  Var
    i,j: Integer;
    P{,Q}: Integer;
  begin
    Repeat
      I:=L;
      J:=R;
      P:=(L+R)Div 2;
      Repeat
        If ColSorting Then begin
          While OnCompareCells(Self, Index, P, Index, i)>0 do I:=I+1;
          While OnCompareCells(Self, Index, P, Index, j)<0 do J:=J-1;
        end Else begin
          While OnCompareCells(Self, P, Index, i, Index)>0 do I:=I+1;
          While OnCompareCells(Self, P, Index, j, Index)<0 do J:=J-1;
        End;
        If I<=J Then Begin
          ExchangeColRow(Not ColSorting, i,j);
          I:=I+1;
          J:=j-1;
        End;
      Until I>J;
      If L<J Then QuickSort(L,J);
      L:=I;
    Until I>=R;
  End;
begin
  BeginUpdate;
  QuickSort(IndxFrom, IndxTo);
  EndUpdate(True);
end;

procedure TCustomGrid.doTopleftChange(dimChg: Boolean);
begin
  TopLeftChanged;
  If dimchg then Begin
    VisualChange;
  End Else begin
    CacheVisibleGrid;
    Invalidate;
  End;
  UpdateScrollBarPos(nil);
end;

procedure TCustomGrid.VisualChange;
Var
  Tw,Th: Integer;
  Dh,DV: Integer;
  
  Function MaxTopLeft: TPoint;
  Var
    i: Integer;
    W,H: Integer;
  begin
    Result:=Point(ColCount-1, RowCount-1);
    W:=0;
    For i:=ColCount-1 downTo FFixedCols Do begin
      W:=W+GetColWidths(i);
      If W<FGCache.ScrollWidth Then Result.x:=i
      Else         Break;
    End;
    H:=0;
    For i:=RowCount-1 downto FFixedRows do begin
      H:=H+GetRowHeights(i);
      If H<FGCache.ScrollHeight Then Result.y:=i
      Else         Break;
    End;
  End;
Var
  Mtl: TPoint;
  {$Ifdef TestSbars} vs,hs: Boolean; {$Endif}
begin
  // Calculate New Cached Values
  FGCache.GridWidth:=0;
  FGCache.FixedWidth:=0;
  For Tw:=0 To ColCount-1 do begin
    FGCache.AccumWidth[Tw]:=Pointer(FGCache.GridWidth);
    FGCache.GridWidth:=FGCache.GridWidth + GetColWidths(Tw);
    If Tw<FixedCols Then FGCache.FixedWidth:=FGCache.GridWidth;
    {$IfDef dbgScroll}
    WriteLn('FGCache.AccumWidth[',Tw,']=',Integer(FGCache.AccumWidth[Tw]));
    {$Endif}
  End;
  FGCache.Gridheight:=0;
  FGCache.FixedHeight:=0;
  For Tw:=0 To RowCount-1 do begin
    FGCache.AccumHeight[Tw]:=Pointer(FGCache.Gridheight);
    FGCache.Gridheight:=FGCache.Gridheight+GetRowHeights(Tw);
    If Tw<FixedRows Then FGCache.FixedHeight:=FGCache.GridHeight;
    {$IfDef dbgScroll}
    WriteLn('FGCache.AccumHeight[',Tw,']=',Integer(FGCache.AccumHeight[Tw]));
    {$Endif}
  End;
  
  Dh:=18{GetSystemMetrics(SM_CYHSCROLL)};
  DV:=18{GetSystemMetrics(SM_CXVSCROLL)};
  TW:=FGCache.GridWidth;
  TH:=FGCache.GridHeight;

  If Not(goSmoothScroll in Options) Then begin
    FGCache.TLColOff:=0;
    FGCache.TLRowOff:=0;
  End;

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
  
  If Not HorzScrollBar.Visible Then DH:=0;
  If Not VertScrollBar.Visible Then DV:=0;
  
  {$IfDef TestSBars}
  If (vs<VertScrollBar.Visible) Then
    WriteLn('Vertical Scrollbar Visible=', VertScrollBar.visible);
  if (hs<>HorzScrollBar.Visible) Then
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

  With FGCache do
  If FScrollBars in [ssAutoHorizontal, ssAutoBoth] then begin
    If HorzScrollBar.Visible Then begin
      HorzScrollBar.Range:= GridWidth+2;
      
      If NOt (goSmoothScroll in Options) Then Begin
        TW:= Integer(AccumWidth[Mtl.X])-(HorzScrollBar.Range-ClientWidth);
        HorzScrollBar.Range:= HorzScrollBar.Range + TW - FixedWidth + 1;
      End;
      
      If HorzScrollBar.Range>ClientWidth Then
        HScrDiv:= (ColCount-FixedRows-1)/(HorzScrollBar.range-ClientWidth);

      {$Ifdef dbgScroll}
      Writeln('TotWidth=',GridWidth,'ClientWidth=',ClientWidth,' Horz Range=',HorzScrolLBar.Range);
      {$Endif}
    End
  End Else
  If FScrollBars in [ssHorizontal, ssBoth] Then HorzScrolLBar.Range:=0;
  
  With FGCache do
  If FScrollBars in [ssAutoVertical, ssAutoBoth] Then Begin
    If VertScrollBar.Visible Then begin
      VertScrollBar.Range:=GridHeight + 2;

      If Not (goSmoothScroll in Options) Then begin
        TH:= Integer(accumHeight[Mtl.Y])-(VertScrollBar.Range-ClientHeight);
        VertScrollBar.Range:= VertScrollBar.Range + TH - FixedHeight + 1;
      End;

      If VertScrolLBar.Range>ClientHeight Then
        VScrDiv:= (RowCount-FixedRows-1)/(VertScrollBar.Range-ClientHeight);

      {$Ifdef dbgScroll}
      Writeln('TotHeight=',GridHeight,'ClientHeight=',ClientHeight,' Vert Range=',VertScrolLBar.Range);
      {$Endif}
    End
  End Else
  If FScrollBars in [ssVertical, ssBoth] then VertScrollbar.Range:=0;

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
Var
  w: Integer;
  MaxRight: Integer;
  MaxBottom: Integer;
begin
  If (FTopLeft.X<0)or(FTopLeft.y<0) then begin
    Result:=Rect(-1,-1,-1,-1);
    Exit;
  End;
  // visible TopLeft Cell
  Result.TopLeft:=fTopLeft;
  Result.BottomRight:=Result.TopLeft;
  
  // Max visible coordinates
  MaxRight:= FGCache.ClientWidth;
  MaxBottom:=FGCache.ClientHeight;
    
  // Left Margin of next visible Column and Rightmost visible cell
  w:=GetColWidths(Result.Left) + FGCache.FixedWidth- FGCache.TLColOff;
  While (Result.Right<ColCount-1)And(W<MaxRight) do begin
    Inc(Result.Right);
    W:=W+GetColWidths(Result.Right);
  End;

  // Top Margin of next visible Row and Bottom most visible cell
  w:=GetRowheights(Result.Top) + FGCache.FixedHeight - FGCache.TLRowOff;
  While (Result.Bottom<RowCount-1)And(W<MaxBottom) do begin
    Inc(Result.Bottom);
    W:=W+GetRowHeights(Result.Bottom);
  End;
end;

{Calculate the TopLeft needed to show cell[aCol,aRow]}
Function TCustomGrid.ScrollToCell(Const aCol,aRow: Integer): Boolean;
Var
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
  While (fTopLeft.x>=0) And
        (fTopLeft.x<ColCount)And
        (fTopLeft.y>=0) And
        (fTopLeft.y<RowCount) do begin

    RNew:=ColRowToClientCellRect(aCol,aRow);
    
    Xinc:=0;
    if Rnew.Left<fw then Xinc:=-1
    Else If RNew.Right>Cw Then XInc:=1;
    Yinc:=0;
    if RNew.Top<fh Then Yinc:=-1
    Else If RNew.Bottom>Ch Then YInc:=1;
    
    With FTopLeft do
    If ((XInc=0)And(Yinc=0)) or
       ((X=aCol)and(y=aRow)) Or // Only Perfect fit !
       ((X+XInc>=ColCount)or(Y+Yinc>=RowCount)) Or // Last Posible
       ((X+XInc<0)Or(Y+Yinc<0)) // Least Posible
    Then Break;

    Inc(FTopLeft.x, XInc);
    Inc(FTopLeft.y, Yinc);
  end;

  Result:=Not PointIgual(OldTopleft,FTopLeft);
  If result Then doTopleftChange(False);
End;

{Returns a valid TopLeft from a proposed TopLeft[DCol,DRow] which are
 relative or absolute coordinates }
function TCustomGrid.ScrollGrid(Relative: Boolean; DCol, DRow: Integer): TPoint;
begin
  Result:=FTopLeft;
  If Not Relative Then begin
    DCol:=DCol-Result.x;
    DRow:=DRow-Result.y;
  End;
  
  If DCol+Result.x<FFixedCols Then DCol:=Result.x-FFixedCols Else
  If DCol+Result.x>ColCount-1 Then DCol:=ColCount-1-Result.x;
  If DRow+Result.y<FFixedRows Then DRow:=Result.y-FFixedRows Else
  If DRow+Result.y>RowCount-1 Then DRow:=RowCount-1-Result.y;

  Inc(Result.x, DCol);
  Inc(Result.y, DRow);
end;

procedure TCustomGrid.TopLeftChanged;
begin
  If Assigned(OnTopLeftChange) And Not (csDesigning in ComponentState) Then
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
  If FUpdateCount=0 Then begin
    //WriteLn('Paint: FGCache.ValidGrid=',FGCache.ValidGrid );
    DrawEdges;
    DrawBackGround;
    If FGCache.ValidGrid Then begin
      {
      DrawFixedCells;
      DrawInteriorCells;
      DrawFocused;
      }
      DrawByRows;
      DrawColRowMoving;
    End;
  End;
End;

procedure TCustomGrid.ResetOffset(chkCol, ChkRow: Boolean);
begin
  With FGCache do begin
    If ChkCol Then ChkCol:=TLColOff<>0;
    If ChkCol Then TlColOff:=0;
    If ChkRow Then ChkRow:=TLRowOff<>0;
    If ChkRow Then TlRowOff:=0;
    If ChkRow or ChkCol Then begin
      CacheVisibleGrid;
      Invalidate;
      If ChkCol Then UpdateScrollBarPos(HorzScrollBar);
      If ChkRow Then UpdateScrolLBarPos(VertScrollBar);
    End;
  End;
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

procedure TCustomGrid.DrawFixedCells;
Var
  Gds: TGridDrawState;
  i,j: Integer;
begin
  Gds:=[gdFixed];
  // Draw fixed fixed Cells
  For i:=0 to FFixedCols-1 do
    For j:=0 to fFixedRows-1 do
      DrawCell(i,j, ColRowToClientCellRect(i,j), gds);
      
  With FGCache.VisibleGrid do begin
    // Draw fixed column headers
    For i:=left to Right do
      For j:=0 to fFixedRows-1 do
        DrawCell(i,j, ColRowToClientCellRect(i,j), gds);
    // Draw fixed row headers
    For i:=0 to FFixedCols-1 do
      For j:=Top to Bottom do
        DrawCell(i,j, ColRowToClientCellRect(i,j), gds);
  End;
end;

procedure TCustomGrid.DrawInteriorCells;
Var
  Gds: TGridDrawState;
  i,j: Integer;
begin
  With FGCache.VisibleGrid do Begin
    For i:=Left to Right do
      For j:=Top to Bottom do begin
        Gds:=[];
        if (i=FCol)And(J=FRow) Then Continue;
        If IsCellSelected(i,j) Then Include(gds, gdSelected);
        DrawCell(i,j, ColRowToClientCellRect(i,j), gds);
      End;
  End;
end;

procedure TCustomGrid.DrawColRowMoving;
begin
  If (FGridState=gsColMoving)And(fMoveLast.x>=0) Then begin
    Canvas.Pen.Width:=3;
    Canvas.Pen.Color:=clRed;
    Canvas.MoveTo(fMoveLast.y, 0);
    Canvas.Lineto(fMovelast.y, FGCache.MaxClientXY.Y);
    Canvas.Pen.Width:=1;
  end Else
  If (FGridState=gsRowMoving)And(FMoveLast.y>=0) then begin
    Canvas.Pen.Width:=3;
    Canvas.Pen.Color:=clRed;
    Canvas.MoveTo(0, FMoveLast.X);
    Canvas.LineTo(FGCache.MaxClientXY.X, FMoveLast.X);
    Canvas.Pen.Width:=1;
  End;
end;

procedure TCustomGrid.DrawCell(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState);
begin
  DrawCellGrid(aRect,aCol,aRow,aState);
end;

procedure TCustomGrid.DrawByRows;
Var
  i: Integer;
begin
  // Draw Rows
  With FGCache.VisibleGrid do
    For i:=Top To Bottom do DrawRow(i);
  // Draw Fixed Rows
  For i:=0 to FFixedRows-1 Do DrawRow(i);
end;

procedure TCustomGrid.DrawRow(aRow: Integer);
Var
  Gds: TGridDrawState;
  i: Integer;
  Rs: Boolean;
  R: TRect;
begin

  ColRowToOffSet(False, True, aRow, R.Top, R.Bottom);
  
  // Draw columns in this row
  With FGCache.VisibleGrid do
    If ARow<FFixedRows Then Begin
      gds:=[gdFixed];
      For i:=Left to Right do Begin
        ColRowToOffset(true, True, i, R.Left, R.Right);
        DrawCell(i,aRow, R{ColRowToClientCellRect(i,aRow)},gds)
      End;
    End Else begin
      Rs:=(goRowSelect in Options);
      For i:=Left To Right do begin
        Gds:=[];
        If (i=Fcol)And(FRow=ARow) Then begin
          // Focused Cell
          Include(gds, gdFocused);
          // Check if need to be selected
          If (goDrawFocusSelected in Options) or
             (Rs And Not(goRelaxedRowSelect in Options)) Then Include(gds, gdSelected);
        End Else
        If IsCellSelected(i, ARow) Then Include(gds, gdSelected);
        ColRowToOffset(True, True, i, R.Left, R.Right);
        DrawCell(i,aRow, R{ColRowToClientCellRect(i,aRow)}, gds);
      End;
      // Draw the focus Rect
      If (ARow=FRow) And
         (IsCellVisible(FCol,ARow) Or (Rs And (ARow>=Top) And (ARow<=Bottom)))
      Then Begin
        If (goEditing in Options)And(goAlwaysShowEditor in Options)And
           (Editor<>nil) Then begin
           //WriteLn('No Draw Focus Rect');
        End Else begin
          ColRowToOffset(True, True, FCol, R.Left, R.Right);
          DrawFocusRect(FCol,FRow, R{ColRowToClienTCellRect(FCol,FRow)}, [gdFocused]);
        End;
      End;
    End; // Else Begin

  // Draw Fixed Columns
  gds:=[gdFixed];
  For i:=0 to FFixedCols-1 do begin
    ColRowToOffset(True, True, i, R.Left, R.Right);
    DrawCell(i,aRow, R{ColRowToClientCellRect(i,aRow)},gds);
  End;
end;

procedure TCustomGrid.DrawEdges;
Var
  P:  TPoint;
  Cr: TRect;
begin
  P:=FGCache.MaxClientXY;
  Cr:=Bounds(0,0, FGCache.ClientWidth, FGCache.ClientHeight);
  If P.x<Cr.Right Then begin
    Cr.Left:=P.x;
    Canvas.Brush.Color:=Color;
    Canvas.FillRect(cr);
    Cr.Left:=0;
    Cr.Right:=p.x;
  End;
  If P.y<Cr.Bottom Then begin
    Cr.Top:=p.y;
    Canvas.Brush.Color:=Color;
    Canvas.FillRect(cr);
  End;
end;

procedure TCustomGrid.DrawFocused;
Var
  R: TRect;
  gds: TGridDrawState;
begin
  gds:=[gdFocused];
  If IsCellVisible(FCol,FRow) Then begin
    If goDrawFocusSelected in Options Then Include(gds,gdSelected);
    If (goRowSelect in Options) And Not (goRelaxedRowSelect in Options) Then
      Include(gds, gdSelected);
    R:=colrowToClientCellRect(fCol,fRow);
    DrawCell(fCol,fRow,R, gds);
    DrawFocusRect(fCol,fRow, R, gds);
  End Else
    If  ((goRowSelect in Options) And
        (Frow>=FGCache.VisibleGrid.Top) And
        (Frow<=FGCache.VisibleGrid.Bottom))
    Then begin
      R:=colrowToClientCellRect(fCol,fRow);
      DrawFocusRect(fcol,fRow, R, gds);
    End;
end;

Procedure DebugRect(S:String; R:TRect);
begin
  WriteLn(S, 'L=',R.Left, ' T=',R.Top, ' R=',R.Right,' B=',R.Bottom);
End;
Procedure DebugPoint(S:String; P:TPoint);
begin
  WriteLn(S, 'X=',P.X,' Y=',P.Y);
End;

procedure TCustomGrid.DrawCellGrid(Rect: TRect; aCol,aRow: Integer; aState: TGridDrawState);
Var
  dv,dh: Boolean;
begin
  // Draw Cell Grid or Maybe in the future Borders..
  Dv:= goVertLine in Options;
  Dh:= goHorzLine In Options;
  If (gdFixed in aState) Then begin
    With Canvas, Rect do Begin
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
      //End;
    End;
    //Canvas.Frame3d(Rect, 1, bvLowered{bvNone}{bvRaised});
    Dh:=Dh and (goFixedHorzLine in Options);
    Dv:=Dv and (goFixedVertLine in Options);
  End;
  
  Canvas.Pen.Style:=fGridLineStyle;
  Canvas.Pen.Color:=fGridLineColor;
  If Dh Then begin
  //If fDrawHorzGrid then begin
    Canvas.MoveTo(Rect.Left,Rect.Top);
    Canvas.LineTo(Rect.Right,Rect.Top);
    If aRow=RowCount-1 Then begin
      Canvas.MoveTo(Rect.Left,Rect.Bottom);
      Canvas.LineTo(Rect.Right,Rect.Bottom);
    End;
  End;
  
  If Dv Then begin
  //If FDrawVertGrid Then begin
    Canvas.MoveTo(Rect.Left,Rect.Top);
    Canvas.LineTo(Rect.Left,Rect.Bottom);
    If aCol=ColCount-1 Then begin
      Canvas.Moveto(Rect.Right, Rect.Top);
      Canvas.LineTo(Rect.Right, Rect.bottom);
    End;
  End;
end;


procedure TCustomGrid.MyTextRect(R: TRect; Offx, Offy: Integer; S: String;
  Ts: TTextStyle);
Var
  Rorg: TRect;
  tmpRgn: HRGN;
begin
  If Ts.Clipping Then begin
    //IntersectClipRect(Canvas.handle, R.Left,R.Top,R.Right,R.Bottom);
  
    GetClipBox(Canvas.Handle, @ROrg);
    //DebugRect('Ini Rect = ', ROrg);
    tmpRGN:=CreateRectRgn(R.Left, R.Top, R.Right, R.Bottom);
    SelectClipRgn(Canvas.Handle, tmpRGN);
    //GetClipBox(Canvas.Handle, @Rtmp);
    //DebugRect('Set Rect = ', Rtmp);
    DeleteObject(tmpRGN);
  End;

  //If Ts.Opaque Then Canvas.FillRect(R);
  Canvas.TextOut(R.Left+Offx, R.Top+Offy,  S);

  If Ts.Clipping Then begin
    tmpRGN:=CreateRectRgn(Rorg.Left, Rorg.Top, Rorg.Right, Rorg.Bottom);
    SelectClipRgn(Canvas.Handle, tmpRGN);
    //GetClipBox(Canvas.Handle, @Rtmp);
    //DebugRect('End Rect = ', Rtmp);
    DeleteObject(tmpRGN);
  End;
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
Var
  C,Tl: Integer;
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
  
  If FGCache.HScrDiv<=0 Then Exit;
  If FEditor<>nil then EditorGetValue;

  If goThumbTracking in Options Then Begin
    C:=FFixedCols + Round( Message.Pos * FGCache.HScrDiv );
    If (FCol<>C) Then begin
      Inc(FUpdateScrollBarsCount);
      MoveExtend(False, C, FRow);
      Dec(FUpdateScrollBarsCount);
    End;
  End Else begin

    C:=Message.Pos+FGCache.FixedWidth;
    TL:=OffsetToColRow(True, False, C, FGCache.TLColOff);
    {$Ifdef dbgScroll}
    WriteLn('---- Offset=',C, ' TL=',TL, ' TLColOFf=', FGCache.TLColOff);
    {$Endif}
    If Not (goSmoothScroll in Options) then FGCache.TLColOff:=0;

    If TL<>FTopLeft.X Then begin
      Inc(FUpdateScrollBarsCount);
      TryScrollTo(Tl, FTopLeft.Y);
      Dec(FUpdateScrollBarsCount);
    End Else
    If goSmoothScroll in Options Then begin
      CacheVisibleGrid;
      Invalidate;
    End;

  End;
end;

procedure TCustomGrid.WMVScroll(var Message: TLMVScroll);
Var
  C: Integer;
  TL: Integer;
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

  If FGCache.VScrDiv<=0 Then Exit;
  If FEditor<>nil then EditorGetValue;
  If goThumbTracking in Options Then begin
    C:=FFixedRows + Round( Message.Pos * FGCache.VScrDiv );
    If (C<>FRow) Then begin
      Inc(FUpdateScrollBarsCount);
      MoveExtend(False, FCol, C);
      Dec(FUpdateScrollBarsCount);
    End;
  End Else begin
    C:=Message.Pos+FGCache.Fixedheight;
    TL:=OffsetToColRow(False, False, C, FGCache.TLRowOff);

    {$Ifdef dbgScroll}
    WriteLn('---- Offset=',C, ' TL=',TL, ' TLRowOFf=', FGCache.TLRowOff);
    {$Endif}
    If Not (goSmoothScroll in Options) Then FGCache.TLRowOff:=0;

    If TL<>FTopLeft.Y Then begin
      Inc(FUpdateScrollBarsCount);
      TryScrollTo(FTopLeft.X, Tl);
      Dec(FUpdateScrollBarsCount);
    End Else
    If goSmoothScroll in Options Then begin
      CacheVisibleGrid;
      Invalidate;
    End;
  End;
end;

procedure TCustomGrid.WMSize(var Msg: TWMSize);
begin
  Inherited;
  visualChange;
end;

{ Scroll grid to the given Topleft[aCol,aRow] as needed }
procedure TCustomGrid.TryScrollTo(aCol, aRow: Integer);
Var
  TryTL: TPoint;
begin
  TryTL:=ScrollGrid(False,aCol, aRow);
  if Not PointIgual(TryTL, FTopLeft) Then begin
    FTopLeft:=TryTL;
    doTopleftChange(False);
  End;
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
  If FUpdateScrollBarsCount=0 Then begin
  
    if (Which=HorzScrollBar)or(Which=nil) Then
      If (FScrollBars in [ssAutoHorizontal, ssAutoBoth]) And
          HorzScrolLBar.Visible Then begin
          With FGCache do
            HorzScrollBar.Position:=
              Integer(AccumWidth[FTopLeft.x])-TLColOff-FixedWidth;
      End;

    If (Which=VertScrollBar)Or(Which=nil) Then
      If (FScrolLBars in [ssAutoVertical, ssAutoBoth]) And
          VertScrolLBar.Visible Then begin
          With FGCache do
            VertScrollBar.Position:=
              Integer(AccumHeight[FTopLeft.y])-TLRowOff-FixedHeight;
      End;
  End; {If FUpd...}
end;

procedure TCustomGrid.CheckFixedCount(aCol,aRow,aFCol,aFRow: Integer);
begin
  If AFRow<0 Then Raise EGridException.Create('FixedRows<0');
  If AFCol<0 Then Raise EGridException.Create('FixedCols<0');
  If (ACol>0)And(aFCol>=ACol) Then
    Raise EGridException.Create('FixedCols can''t be >= ColCount');
  If (ARow>0)And(aFRow>=ARow) Then
    Raise EGridException.Create('FixedRows can''t be >= RowCount');
end;

{ Save to the cache the current visible grid (excluding fixed cells) }
procedure TCustomGrid.CacheVisibleGrid;
Var
  R: TRect;
begin
  With FGCache do begin
    VisibleGrid:=GetVisibleGrid;
    With VisibleGrid do
      ValidGrid:=(Left>=0)And(Top>=0)And(Right>=Left)And(Bottom>=Top);
    If Not ValidGrid Then MaxClientXY:=Point(0,0)
    Else begin
      R:=ColRowToClientCellrect(VisibleGrid.Right, VisibleGrid.Bottom);
      MaxClientXY:=R.BottomRight;
    End;
  End;
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
  FSelectActive:=AValue;
  If FSelectActive Then FPivot:=Point(FCol,FRow);
end;

procedure TCustomGrid.SetSelection(const AValue: TGridRect);
begin
  If goRangeSelect in Options Then begin
    fRange:=NormalizarRect(aValue);
    Invalidate;
  End;
end;

function TCustomGrid.doColSizing(X, Y: Integer): Boolean;
Var
  R: TRect;
  Loc: Integer;
begin
  Result:=False;
  If gsColSizing = fGridState Then begin
    If x>FSplitter.y Then
      ColWidths[FSplitter.x]:=x-FSplitter.y
    Else
      if ColWidths[FSplitter.x]>0 Then ColWidths[FSplitter.X]:=0;
    Result:=True;
  End Else
  If (fGridState=gsNormal)And(Y<FGCache.FixedHeight)And(X>FGCache.FixedWidth) Then
  begin
    FSplitter.X:= OffsetToColRow(True, True, X, Loc);
    FSplitter.Y:=0;
    If FSplitter.X>=0 Then begin
      R:=ColRowToClientCellRect(FSplitter.x, FSplitter.y);
      FSplitter.y:=X;                       // Resizing X reference
      If (R.Right-X)<(X-R.Left) then Loc:=R.Right
      Else begin
        Loc:=R.Left;
        Dec(FSplitter.x);                   // Resizing col is the previous
      End;
      IF (Abs(Loc-x)<=2)And(FSplitter.X>=FFixedCols) then Cursor:=crHSplit
      Else                                                Cursor:=crDefault;
      Result:=True;
    End;
  End
    Else
      If (cursor=crHSplit) Then Cursor:=crDefault;
end;

function TCustomGrid.doRowSizing(X, Y: Integer): Boolean;
Var
  R: TRect;
  Loc: Integer;
begin
  Result:=False;
  If gsRowSizing = fGridState Then begin
    If y>FSplitter.x Then
      RowHeights[FSplitter.y]:=y-FSplitter.x
    Else
      if RowHeights[FSplitter.y]>0 Then RowHeights[FSplitter.Y]:=0;
    Result:=True;
  End Else
  If (fGridState=gsNormal)And(X<FGCache.FixedWidth)And(Y>FGCache.FixedHeight) Then
  begin
    fSplitter.Y:=OffsetToColRow(False, True, Y, Loc);
    If Fsplitter.Y>=0 Then begin
      ColRowToOffset(False, True, FSplitter.Y, R.Top, R.Bottom);
      FSplitter.X:=Y;
      If (R.Bottom-Y)<(Y-R.Top) Then Loc:=R.Bottom
      Else begin
        Loc:=R.Top;
        Dec(FSplitter.y);
      End;
      IF (Abs(Loc-y)<=2)And(FSplitter.Y>=FFixedRows) then Cursor:=crVSplit
      Else                                                Cursor:=crDefault;
      Result:=True;
    End;
    {
    FSplitter:=MouseToCell(Point(X,Y)); // Resizing Row
    R:=ColRowToClientCellRect(FSplitter.x, FSplitter.y);
    Fsplitter.x:=y;                       // Resizing y reference
    If (R.Bottom-Y)<(Y-R.Top) Then Loc:=R.Bottom
    Else Begin
      Loc:=R.Top;
      Dec(FSplitter.y);                   // Resizing row is the previous
    End;
    IF (Abs(Loc-y)<=2)And(FSplitter.Y>=FFixedRows) then Cursor:=crVSplit
    Else                                                Cursor:=crDefault;
    Result:=True;
    }
  End
    Else
      If Cursor=crVSplit Then Cursor:=crDefault;
end;

procedure TCustomGrid.doColMoving(X, Y: Integer);
Var
  P: TPoint;
  R: TRect;
begin
  P:=MouseToCell(Point(X,Y));
  If (Abs(FSplitter.Y-X)>fDragDx)And(Cursor<>crMultiDrag) Then begin
    Cursor:=crMultiDrag;
    FMoveLast:=Point(-1,-1);
    ResetOffset(True, False);
  End;
  if (Cursor=crMultiDrag)And
     (P.x>=FFixedCols) And
     ((P.X<=FSplitter.X)or(P.X>FSplitter.X))And
     (P.X<>FMoveLast.X) Then begin
      R:=ColRowToClientCellRect(P.x, P.y);
      If P.x<=FSplitter.X Then fMoveLast.Y:=R.left
      Else                     FMoveLast.Y:=R.Right;
      fMoveLast.X:=P.X;
      Invalidate;
  End;
end;

procedure TCustomGrid.doRowMoving(X, Y: Integer);
Var
  P: TPoint;
  R: TRect;
begin
  P:=MouseToCell(Point(X,Y));
  If (Cursor<>crMultiDrag)And(Abs(FSplitter.X-Y)>fDragDx) Then begin
    Cursor:=crMultiDrag;
    FMoveLast:=Point(-1,-1);
    ResetOffset(False, True);
  End;
  if (Cursor=crMultiDrag)And
     (P.y>=FFixedRows) And
     ((P.y<=FSplitter.Y)or(P.Y>FSplitter.Y))And
     (P.y<>FMoveLast.Y) Then begin
      R:=ColRowToClientCellRect(P.x, P.y);
      If P.y<=FSplitter.y Then fMoveLast.X:=R.Top
      Else                     FMoveLast.X:=R.Bottom;
      fMoveLast.Y:=P.Y;
      Invalidate;
  End;
end;


Function TCustomGrid.OffsetToColRow(IsCol, Fisical: Boolean; Offset: Integer;
  var Rest: Integer): Integer;
begin
  Result:=0; //Result:=-1;
  Rest:=0;
  If Offset<0 Then Exit; // Out of Range;
  With FGCache do
  If IsCol Then begin

    // Begin to count Cols from 0 but ...
    If Fisical And (Offset>FixedWidth-1) Then begin
      Result:=FTopLeft.X;  // In scrolled view, then Begin from FtopLeft col
      Offset:=Offset-FixedWidth+Integer(AccumWidth[Result])+TLColOff;
      If Offset>GridWidth-1 Then Begin
        Result:=ColCount-1;
        Exit;
      End;
    End;
    While Offset>(Integer(AccumWidth[Result])+GetColWidths(Result)-1) do Inc(Result);
    Rest:=Offset;
    If Result<>0 Then Rest:=Offset-Integer(AccumWidth[Result]);

  End Else Begin
  
    If Fisical And (Offset>FixedHeight-1) Then begin
      Result:=FTopLeft.Y;
      Offset:=Offset-FixedHeight+Integer(AccumHeight[Result])+TLRowOff;
      If Offset>GridHeight-1 Then Begin
        Result:=RowCount-1;
        Exit; // Out of Range
      End;
    End;
    While Offset>(Integer(AccumHeight[Result])+GetRowHeights(Result)-1) do Inc(Result);
    Rest:=Offset;
    If Result<>0 Then Rest:=Offset-Integer(AccumHeight[Result]);
    
  End;
end;

function TCustomGrid.ColRowToOffset(IsCol,Fisical:Boolean; Index:Integer; Var Ini,Fin:Integer): Boolean;
Var
  Dim: Integer;
begin
  With FGCache do begin
    If IsCol Then begin
      Ini:=Integer(AccumWidth[Index]);
      Dim:=GetColWidths(Index);
    End Else begin
      Ini:=Integer(AccumHeight[Index]);
      Dim:= GetRowheights(Index);
    End;
    if Not Fisical Then begin
      Fin:=Ini + Dim;
      Exit;
    End;
    If IsCol Then Begin
      If index>=FFixedCols Then
        Ini:=Ini-Integer(AccumWidth[FTopLeft.X]) + FixedWidth -  TLColOff;
    End Else begin
      if Index>=FFixedRows then
        Ini:=Ini-Integer(AccumHeight[FTopLeft.Y]) + FixedHeight - TLRowOff;
    End;
    Fin:=Ini + Dim;
  End;
  Result:=true;
end;

{
Function TCustomGrid.GetColRowOffset(IsCol, Fisical: Boolean; Offset: Integer;
  var Rest: Integer): Integer;
Var
  i,j,k: Integer;
  L: TList;
begin
  Result:=-1;
  If Offset<0 Then Exit; // Out of Range;
  j:=0; k:=0;
  If IsCol Then begin
    If Offset>FGCache.GridWidth Then Exit; // Out of Range;
    If Fisical Then
      If Offset>FGCache.FixedWidth Then begin
        k:=FTopLeft.X;
        Offset:=Offset - FGCache.FixedWidth;
      End;
    for i:=k to ColCount-1 do begin
      if Offset<j Then Break;
      j:=j+getColWidths(i);
      Result:=i;
    End;
  End  Else Begin
    If Offset>FGCache.GridHeight Then Exit; // Out of Range
    If Fisical Then
      If Offset>FGCache.FixedHeight Then begin
        k:=FTopLeft.Y;
        Offset:=Offset - FGCache.FixedHeight;
      End;
    For i:=k to RowCount-1 do begin
      If Offset<j Then Break;
      j:=j+GetRowheights(i);
      Result:=i;
    End;

  End;
  Rest:=Offset;
end;
}
function TCustomGrid.MouseToGridZone(X, Y: Integer; CellCoords: Boolean): TGridZone;
begin
  Result:=gzNormal;
  If CellCoords Then Begin
    If (X<fFixedCols) then
      If Y<FFixedRows Then  Result:= gzFixedCells
      Else                  Result:= gzFixedRows
    Else
    If (Y<fFixedRows) Then
      If X<FFixedCols Then  Result:= gzFixedCells
      Else                  Result:= gzFixedCols;
  End Else begin
    If X<=FGCache.FixedWidth Then
      If Y<=FGcache.FixedHeight Then  Result:=gzFixedCells
      Else                            Result:=gzFixedRows
    Else
    If Y<=FGCache.FixedHeight Then
      if X<=FGCache.FixedWidth Then   Result:=gzFixedCells
      Else                            Result:=gzFixedCols;
  End;
end;

procedure TCustomGrid.ExchangeColRow(IsColumn: Boolean; Index, WithIndex: Integer
  );
begin
  If IsColumn Then FCols.Exchange(Index, WithIndex)
  Else             FRows.Exchange(Index, WithIndex);
  ColRowExchanged(IsColumn, Index, WithIndex);
  VisualChange;
end;

procedure TCustomGrid.MoveColRow(IsColumn: Boolean; FromIndex, ToIndex: Integer);
begin
  If IsColumn then FCols.Move(FromIndex, ToIndex)
  Else             FRows.Move(FromIndex, ToIndex);
  ColRowMoved(IsColumn, FromIndex, ToIndex);
  VisualChange;
End;

procedure TCustomGrid.SortColRow(IsColumn: Boolean; Index: Integer);
begin
  If IsColumn Then SortColRow(IsColumn, Index, FFixedRows, RowCount-1)
  Else             SortColRow(IsColumn, Index, FFixedCols, ColCount-1);
end;

procedure TCustomGrid.SortColRow(IsColumn: Boolean; Index, FromIndex,
  ToIndex: Integer);
begin
  If Assigned(OnCompareCells) Then begin
    BeginUpdate;
    Sort(IsColumn, Index, FromIndex, ToIndex);
    EndUpdate(true);
  End;
end;

procedure TCustomGrid.DeleteColRow(IsColumn: Boolean; Index: Integer);
begin
  If IsColumn Then FCols.Delete(Index)
  Else             FRows.Delete(Index);
  ColRowDeleted(IsColumn, Index);
  VisualChange;
end;

procedure TCustomGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
Var
  Gz: TGridZone;
  R: TRect;
begin
  inherited MouseDown(Button, Shift, X, Y);
  If Not FGCache.ValidGrid Then Exit;
  If Not (ssLeft in Shift) Then Exit;
  Gz:=MouseToGridZone(X,Y, False);
  Case Gz of
    gzFixedCols:
      begin
        if (goColSizing in Options)And(Cursor=crHSplit) then Begin
          R:=ColRowToClientCellRect(FSplitter.x, FTopLeft.y);
          FSplitter.y:=R.Left;
          fGridState:= gsColSizing;
        End Else begin
          // ColMoving or Clicking
          fGridState:=gsColMoving;
          FSplitter:=MouseToCell(Point(X,Y));
          FMoveLast:=Point(-1,-1);
          FSplitter.Y:=X;
        End;
      End;
    gzFixedRows:
      If (goRowSizing in Options)And(Cursor=crVSplit) Then begin
        R:=ColRowToClientcellRect(FTopLeft.X, FSplitter.y);
        FSplitter.x:=R.top;
        fGridState:= gsRowSizing;
      End Else begin
        // RowMoving or Clicking
        fGridState:=gsRowMoving;
        fSplitter:=MouseToCell(Point(X,Y));
        FMoveLast:=Point(-1,-1);
        FSplitter.X:=Y;
      End;
    gzNormal:
      Begin
        if csDesigning in ComponentState Then Exit;

        // is user clicking on a selection?
        // is user dragging the selection?
        // is user only selecting a new cell range?
        fGridState:=gsSelecting;
        FSplitter:=MouseToCell(Point(X,Y));

        If Not (goEditing in Options) Then begin
          If ssShift in Shift Then begin
            SelectActive:=(goRangeSelect in Options);
          End Else begin
            If Not SelectACtive Then Begin
              FPivot:=FSplitter;
              FSelectActive:=true;
            End;
          End;
          MoveExtend(False, fsplitter.X, fSplitter.y);
        End;
      End;
  End;
end;

procedure TCustomGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
Var
  p: TPoint;
begin
  inherited MouseMove(Shift, X, Y);
  If Not FGCache.ValidGrid Then Exit;

  Case fGridState of
    gsSelecting:
      begin
        If Not (goEditing in Options) Then begin
          P:=MouseToLogcell(Point(X,Y));
          MoveExtend(False, P.x, P.y);
        End;
      End;
    gsColMoving: If goColMoving in Options Then doColMoving(X,Y);
    gsRowMoving: If goRowMoving in Options Then doRowMoving(X,Y);
    Else
      begin
        If goColSizing in Options Then doColSizing(X,Y);
        If goRowSizing in Options Then doRowSizing(X,Y);
      End;
  End;
end;

procedure TCustomGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
   Cur: TPoint;
begin
  inherited MouseUp(Button, Shift, X, Y);
  If Not FGCache.ValidGrid Then Exit;
  Cur:=MouseToCell(Point(x,y));
  Case fGridState of
    gsSelecting:
      Begin
        MoveExtend(False, Cur.x, Cur.y);
        SelectActive:=False;
      End;
    gsColMoving:
      begin
        //WriteLn('Move Col From ',Fsplitter.x,' to ', FMoveLast.x);
        If FMoveLast.X>=0 Then Begin
          MoveColRow(True, Fsplitter.X, FMoveLast.X);
          Cursor:=crDefault;
        End Else
          If Cur.X=FSplitter.X Then HeaderClick(True, FSplitter.X);
      End;
    gsRowMoving:
      Begin
        //WriteLn('Move Row From ',Fsplitter.Y,' to ', FMoveLast.Y);
        If FMoveLast.Y>=0 Then begin
          MoveColRow(False, Fsplitter.Y, FMoveLast.Y);
          Cursor:=crDefault;
        End Else
          If Cur.Y=FSplitter.Y Then HeaderClick(False, FSplitter.Y);
      End;
  End;
  fGridState:=gsNormal;
end;

procedure TCustomGrid.DblClick;
begin
  If (goColSizing in Options) And (Cursor=crHSplit) Then begin
    If (goDblClickAutoSize in Options) Then begin
      AutoAdjustColumn( FSplitter.X );
    End Else
      WriteLn('Got Doubleclick on Col Resizing: AutoAdjust?');
  End Else
  If  (goDblClickAutoSize in Options) And
      (goRowSizing in Options) And
      (Cursor=crVSplit) Then begin
        WriteLn('Got DoubleClick on Row Resizing: AutoAdjust?');
  End
  Else
    Inherited DblClick;
end;

procedure TCustomGrid.doExit;
begin
  inherited doExit;
  Invalidate;
end;

procedure TCustomGrid.doEnter;
begin
  inherited doEnter;
  If FEditorHiding Then begin
    // Self generated doEnter
  End Else begin
    // Got the focus for some other reason
    Invalidate; // redraw the focused cell
    // Handle click on focused cell, still needs to show the editor
    // when focusing from other way than mouse clicks
    FFocusing:=True;
  End;
end;

procedure TCustomGrid.KeyDown(var Key: Word; Shift: TShiftState);
Var
  Sh: Boolean;
  
  Procedure MoveSel(Rel: Boolean; aCol,aRow: Integer);
  begin
    // Always reset Offset in kerboard Events
    FGCache.TLColOff:=0; FGCache.TLRowOff:=0;
    SelectActive:=Sh;
    MoveExtend(Rel,aCol,aRow);
    Key:=0;
  End;
Var
  R: TRect;
  Relaxed: Boolean;
begin
  inherited KeyDown(Key, Shift);
  If Not FGCache.ValidGrid Then Exit;
  Sh:=(ssShift in Shift);
  Relaxed:=Not (goRowSelect in Options) or (goRelaxedRowSelect in Options);
  
  If (Key=Vk_TAB)And(goTabs in Options) Then begin
    Case FAutoAdvance of
      aaRight:
        If Sh Then Key:=VK_LEFT
        Else       Key:=VK_RIGHT;
      aaDown:
        If Sh Then Key:=VK_UP
        Else       Key:=VK_DOWN;
    End;
  End;
  
  Case Key of
    VK_LEFT:
      Begin
        If Relaxed Then MoveSel(True,-1, 0)
        Else            MoveSel(true, 0,-1);
      End;
    VK_RIGHT:
      begin
        If Relaxed Then MoveSel(True, 1, 0)
        Else            MoveSel(True, 0, 1);
      End;
    VK_UP:
      Begin
        MoveSel(True, 0, -1);
      End;
    VK_DOWN:
      begin
        MoveSel(True, 0, 1);
      End;
    VK_PRIOR:
      begin
        R:=FGCache.Visiblegrid;
        MoveSel(True, 0, R.Top-R.Bottom);
      End;
    VK_NEXT:
      Begin
        R:=FGCache.VisibleGrid;
        MoveSel(True, 0, R.Bottom-R.Top);
      End;
    VK_HOME:
      begin
        If ssCtrl in Shift Then MoveSel(False, FCol, FFixedRows)
        Else
          if Relaxed Then MoveSel(False, FFixedCols, FRow)
          Else            MoveSel(False, FCol, FFixedRows);
      End;
    VK_END:
      Begin
        If ssCtrl in Shift Then MoveSel(False, FCol, RowCount-1)
        Else
          if Relaxed Then MoveSel(False, ColCount-1, FRow)
          Else            MoveSel(False, FCol, RowCount-1);
      End;
    VK_F2, VK_RETURN:
      begin
        EditorShow;
        If Key=VK_RETURN Then EditorSelectAll;
        Key:=0;
      End;

    {$IfDef Dbg}
    Else WriteLn('KeyDown: ', Key);
    {$Endif}
  End;
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

Var
   //i,j: Integer;
   //P: TPoint;
   d: Integer;
begin
  Result.X:= OffsetToColRow(True, True, Mouse.x, d);
  Result.Y:= OffsetToColRow(False,True, Mouse.y, d);
{  Exit;
  P:=Point(0,0);
  If Mouse.x>FGCache.FixedWidth Then begin
    P.x:=fTopLeft.x;
    Mouse.X:=Mouse.X-FGCache.FixedWidth;
  End;
  If Mouse.y>FGCache.FixedHeight Then begin
    p.y:=fTopLeft.y;
    Mouse.Y:=Mouse.Y-FGCache.Fixedheight;
  End;

  Result.x:=-1; Result.y:=-1;
  j:=0;
  for i:=P.x to ColCount-1 do begin
    if Mouse.x<j Then Break;
    j:=j+getColWidths(i);
    Result.x:=i;
  End;
  j:=0;
  For i:=P.y to RowCount-1 do begin
    if Mouse.y<j then break;
    j:=j+getRowheights(i);
    Result.y:=i;
  End;
}
end;

{ Convert a fisical Mouse coordinate into logical a cell coordinate }
function TCustomGrid.MouseToLogcell(Mouse: TPoint): TPoint;
Var
  gz: TGridZone;
begin
  Gz:=MouseToGridZone(Mouse.x, Mouse.y, False);
  If gz=gzNormal Then Result:=MouseToCell(Mouse)
  Else begin
    Result:=MouseToCell(Mouse);
    If (gz=gzFixedRows)or(gz=gzFixedCells) then begin
      Result.x:= fTopLeft.x-1;
      If Result.x<FFixedCols Then Result.x:=FFixedCols;
    End;
    If (gz=gzFixedCols)or(gz=gzFixedCells) Then begin
      Result.y:=fTopleft.y-1;
      If Result.y<fFixedRows Then Result.y:=FFixedRows;
    End;
  End;
end;

function TCustomGrid.ISCellVisible(aCol, aRow: Integer): Boolean;
begin
  With FGCache.VisibleGrid do
    Result:= (Left<=ACol)And(aCol<=Right)And(Top<=aRow)And(aRow<=Bottom);
end;

procedure TCustomGrid.InvalidateCol(ACol: Integer);
Var
  R: TRect;
begin
  {$ifdef dbg} WriteLn('InvalidateCol  Col=',aCol); {$Endif}
  R:=ColRowToClientCellRect(aCol, FTopLeft.y);
  R.Bottom:=FGCache.MaxClientXY.Y;
  InvalidateRect(Handle, @R, True);
End;

procedure TCustomGrid.InvalidateRow(ARow: Integer);
Var
  R: TRect;
begin
  {$ifdef dbg} WriteLn('InvalidateRow  Row=',aRow); {$Endif}
  R:=ColRowToClientCellRect(fTopLeft.x, aRow);
  R.Right:=FGCache.MaxClientXY.X;
  InvalidateRect(Handle, @R, True);
End;


Function TCustomGrid.MoveExtend(Relative: Boolean; DCol, DRow: Integer): Boolean;
Var
  InvalidateAll: Boolean;
begin
  Result:=False;
  
  dCol:=FCol*(1-Byte(Not Relative))+DCol;
  dRow:=FRow*(1-Byte(Not Relative))+DRow;
  If dCol<FFixedCols Then dCol:=FFixedCols Else
  If dCol>ColCount-1 Then dcol:=ColCount-1;
  If dRow<FFixedRows Then dRow:=FFixedRows Else
  If dRow>RowCount-1 Then dRow:=RowCount-1;

  // Change on Focused cell?
  If (Dcol=FCol)And(DRow=FRow) Then begin
    If FFocusing
      And(goEditing in Options)
      And(goAlwaysShowEditor In Options)
      And(EDitor<>nil) Then Begin
        EditorShow;
    End;
    FFocusing:=False;
    Exit;
  End;

  FFocusing:=False;
  Result:=True;
  if Assigned(OnBeforeSelection) Then OnBeforeSelection(Self, DCol, DRow, Result);
  If Not Result Then Exit;
  
  // Going to change selection, get editor value before that
  EditorGetValue;
  
  // default range
  If goRowSelect in Options Then FRange:=Rect(FFixedCols, DRow, Colcount-1, DRow)
  Else                           FRange:=Rect(DCol,DRow,DCol,DRow);
  
  InvalidateAll:=False;
  If SelectActive Then
    if goRangeSelect in Options Then begin
      If goRowSelect in Options Then Begin
        FRange.Top:=Min(fPivot.y, DRow);
        FRange.Bottom:=Max(fPivot.y, DRow);
      End Else begin
        FRange:=NormalizarRect(Rect(Fpivot.x,FPivot.y, DCol, DRow));
      End;
      InvalidateAll:=True;
    End;

  If Not ScrollToCell(DCol, DRow) Then
    If InvalidateAll Then Begin
      //InvalidateSelection;
      Invalidate
    End Else begin
      //InvalidateCell(FCol, FRow);
      InvalidateCell(DCol, DRow);
    End;
    
  fCol:=DCol;
  fRow:=DRow;
  Editor:=nil;
  
  MoveSelection;
  
  if //Not SelectActive And
    (goEditing in Options) And
    (goAlwaysShowEditor in Options) And
    Not(goRowSelect in Options) Then EditorShow;
end;

procedure TCustomGrid.MoveSelection;
begin
  if Assigned(onSelection) Then OnSelection(Self, FCol, FRow);
end;

procedure TCustomGrid.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TCustomGrid.EndUpdate(UO: TUpdateOption);
begin
  Dec(FUpdateCount);
  If FUpdateCount=0 Then
    Case UO of
      uoQuick: Invalidate;
      uoFull: VisualChange;
    End;
end;

procedure TCustomGrid.EndUpdate(FullUpdate: Boolean);
begin
  EndUpdate(uoFull);
end;

function TCustomGrid.IsCellSelected(aCol, aRow: Integer): Boolean;
begin
  Result:=  (FRange.Left<=aCol)   And
            (aCol<=FRange.Right)  And
            (FRange.Top<=aRow)    And
            (aRow<=FRange.Bottom);
end;

procedure TCustomGrid.InvalidateCell(aCol, aRow: Integer);
Var
  R: TRect;
begin
  {$IfDef dbgPaint}
    WriteLn('InvalidateCell  Col=',aCol, ' Row=',aRow);
  {$Endif}
  R:=ColRowToClientCellRect(aCol, aRow);
  InvalidateRect(Handle, @R, False);
  //InvalidateRect(Handle, @R, True);
End;

procedure TCustomGrid.InvalidateGrid;
begin
  If FUpdateCount=0 Then Invalidate;
end;

procedure TCustomGrid.Invalidate;
begin
  If FUpdateCount=0 Then
    inherited Invalidate;
end;

procedure TCustomGrid.EditorGetValue;
begin
  If Not (csDesigning in ComponentState) Then begin
    EditorHide;
    doEditorGetValue;
  End;
end;

procedure TCustomGrid.EditorSetValue;
begin
  If Not (csDesigning in ComponentState) Then begin
    EditorPos;
    doEditorSetValue;
  End;
end;

procedure TCustomGrid.EditorHide;
begin
  if (Editor<>nil) And Editor.HandleAllocated And Editor.Visible Then begin
    If Not FEditorHiding Then begin
      FEditorHiding:=True;
      Editor.Visible:=False;
      Editor.Parent:=nil;
      LCLLinux.SetFocus(Self.Handle);
      FEDitorHiding:=False;
    End;
  End;
end;

procedure TCustomGrid.EditorShow;
begin
  If Not (csDesigning in ComponentState)And(goEditing in Options) Then
    If (Editor<>nil) And Not Editor.Visible Then Begin
      ResetOffset(True, True);
      EditorReset;
      LCLLinux.SetFocus(Editor.Handle);
    End;
end;

procedure TCustomGrid.EditorPos;
Var
  msg: TGridMessage;
begin
  if FEditor<>nil Then begin
    Msg.CellRect:=ColRowToClientCellRect(FCol,FRow);
    If FEditorOptions And EO_AUTOSIZE = EO_AUTOSIZE Then begin
      With Msg.CellRect do
        FEditor.SetBounds(Left, Top, Right-Left, Bottom-Top);
    End Else Begin
      Msg.MsgID:=GM_SETPOS;
      Msg.Grid:=Self;
      Msg.Col:=FCol;
      Msg.Row:=FRow;
      FEditor.Dispatch(Msg);
    End;
  End;
end;

procedure TCustomGrid.EditorReset;
begin
  Editor.Parent:=Self;
  EditorSetValue;
  Editor.Visible:=True;
end;

procedure TCustomGrid.EditorSelectAll;
Var
  Msg: TGridMessage;
begin
  If FEditor<>nil Then
    If FEditorOptions And EO_SELECTALL = EO_SELECTALL Then begin
      Msg.MsgID:=GM_SELECTALL;
      FEditor.Dispatch(Msg);
    End;
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
  //WriteLn('Editor is losing the focus..');
  If Not FEditorHiding Then begin
    // Editor losing focus for any reason
    //WriteLn('Hey, What is happening here?');
    FEditorHiding:=True;
    EditorGetValue;
    If Editor<>nil Then Begin
      Editor.Visible:=False;
      Editor.Parent:=nil;
    End;
    FEditorHiding:=False;
  End;
end;

procedure TCustomGrid.EditorKeyDown(Sender: TObject; var Key:Word; Shift:TShiftState);
begin
  FEditorKey:=True; // Just a flag to see from where the event comes
  Case Key of
    VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN,
    VK_PRIOR, VK_NEXT:
    Begin
      If Not(ssShift in Shift) Then KeyDown(Key, Shift);
    End;
    VK_RETURN, VK_TAB:
    Begin
      If (Key=VK_TAB) And Not (goTabs in Options) Then begin
          // let the focus go
          Exit;
      End;
      Key:=0;
      Case FAutoAdvance of
        aaRight: Key:=VK_RIGHT * Integer( FCol<ColCount-1 );
        aaDown : Key:=VK_DOWN * Integer( FRow<RowCount-1 );
      End;
      If Key=0 Then begin
        EditorGetValue;
        EditorShow;
        // Select All !
      End Else KeyDown(Key, Shift);
    End;
  End;
  FEditorKey:=False;
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
Var
  i,j,k: Integer;
  Path: String;
begin
  cfg.SetValue('grid/version', GRIDFILEVERSION);
  
  Cfg.SetValue('grid/saveoptions/create', soDesign in SaveOptions);
  If soDesign in SaveOptions then begin
    Cfg.SetValue('grid/design/columncount',  ColCount);
    Cfg.SetValue('grid/design/rowcount',  RowCount);
    Cfg.SetValue('grid/design/fixedcols', FixedCols);
    Cfg.SetValue('grid/design/fixedrows', Fixedrows);
    Cfg.SetValue('grid/design/defaultcolwidth', DefaultColWidth);
    Cfg.SetValue('grid/design/defaultRowHeight',DefaultRowHeight);

    j:=0;
    For i:=0 to ColCount-1 do begin
      k:=Integer(FCols[i]);
      If (k>=0)And(k<>DefaultColWidth) Then Begin
        inc(j);
        cfg.SetValue('grid/design/columns/columncount',j);
        cfg.SetValue('grid/design/columns/column'+IntToStr(j)+'/index', i);
        cfg.SetValue('grid/design/columns/column'+IntToStr(j)+'/width', k);
      End;
    End;
    j:=0;
    For i:=0 to RowCount-1 do begin
      k:=Integer(FRows[i]);
      If (k>=0)And(k<>DefaultRowHeight) Then begin
        inc(j);
        cfg.SetValue('grid/design/rows/rowcount',j);
        cfg.SetValue('grid/design/rows/row'+IntToStr(j)+'/index', i);
        cfg.SetValue('grid/design/rows/row'+IntToStr(j)+'/height',k);
      End;
    End;


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
  End;
  
  Cfg.SetValue('grid/saveoptions/position', soPosition in SaveOptions);
  If soPosition in SaveOptions then begin
    Cfg.SetValue('grid/position/topleftcol',ftopleft.x);
    Cfg.SetValue('grid/position/topleftrow',ftopleft.y);
    Cfg.SetValue('grid/position/col',fCol);
    Cfg.SetValue('grid/position/row',fRow);
    if goRangeSelect in Options Then begin
      Cfg.SetValue('grid/position/selection/left',Selection.left);
      Cfg.SetValue('grid/position/selection/top',Selection.top);
      Cfg.SetValue('grid/position/selection/right',Selection.right);
      Cfg.SetValue('grid/position/selection/bottom',Selection.bottom);
    End;
  End;
end;

procedure TCustomGrid.LoadContent(cfg: TXMLConfig);
Var
  CreateSaved: Boolean;
  Opt: TGridOptions;
  i,j,k: Integer;
  path: String;
  
    Procedure GetValue(optStr:String; aOpt:TGridOption);
    begin
      If Cfg.GetValue(Path+OptStr+'/value', False) Then Opt:=Opt+[aOpt];
    End;
    
begin
  If soDesign in FSaveOptions Then begin
    CreateSaved:=Cfg.GetValue('grid/saveoptions/create', false);
    If CreateSaved Then begin
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
        If (j>=0)And(j<=ColCount-1) Then begin
          ColWidths[j]:=cfg.getValue(Path+'column'+IntToStr(i)+'/width',-1);
        End;
      End;
      Path:='grid/design/rows/';
      k:=cfg.getValue(Path+'rowcount',0);
      For i:=1 to k do begin
        j:=cfg.getValue(Path+'row'+IntToStr(i)+'/index',-1);
        If (j>=0)And(j<=ColCount-1) Then begin
          RowHeights[j]:=cfg.getValue(Path+'row'+IntToStr(i)+'/height',-1);
        End;
      End;

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
      If GRIDFILEVERSION>=2 Then begin
        GetValue('goSmoothScroll',goSmoothScroll);
      End;
      
      Options:=Opt;
    End;

    CreateSaved:=Cfg.GetValue('grid/saveoptions/position', false);
    If CreateSaved Then begin
      i:=Cfg.GetValue('grid/position/topleftcol',-1);
      j:=Cfg.GetValue('grid/position/topleftrow',-1);
      if MouseToGridZone(i,j,true)=gzNormal Then Begin
        tryScrollto(i,j);
      End;
      i:=Cfg.GetValue('grid/position/col',-1);
      j:=Cfg.GetValue('grid/position/row',-1);
      If (i>=FFixedCols)And(i<=ColCount-1) And
         (j>=FFixedRows)And(j<=RowCount-1) Then Begin
        MoveExtend(false, i,j);
      End;
      if goRangeSelect in Options Then begin
        FRange.left:=Cfg.getValue('grid/position/selection/left',FCol);
        FRange.Top:=Cfg.getValue('grid/position/selection/top',FRow);
        FRange.Right:=Cfg.getValue('grid/position/selection/right',FCol);
        FRange.Bottom:=Cfg.getValue('grid/position/selection/bottom',FRow);
      End;
    End;
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

  FRange:=Rect(-1,-1,-1,-1);
  FDragDx:=3;

  SetBounds(0,0,200,100);
  ColCount:=5;
  RowCount:=5;
  FixedCols:=1;
  FixedRows:=1;
  Editor:=nil;
end;

destructor TCustomGrid.Destroy;
begin
  {$Ifdef dbg}WriteLn('TCustomGrid.Destroy');{$Endif}
  FreeThenNil(FGCache.AccumWidth);
  FreeThenNil(FGCache.AccumHeight);
  FreeThenNil(FCols);
  FreeThenNil(FRows);
  inherited Destroy;
End;

procedure TCustomGrid.SaveToFile(FileName: String);
Var
  Cfg: TXMLConfig;
begin
  If FileExists(FileName) then DeleteFile(FileName);

  Cfg:=TXMLConfig.Create(FileName);
  Try
    SaveContent(Cfg);
  Finally
    Cfg.Flush;
    FreeThenNil(Cfg);
  End;
end;

procedure TCustomGrid.LoadFromFile(FileName: String);
Var
  Cfg: TXMLConfig;
  Version: Integer;
begin
  If Not FileExists(FileName) Then
    Raise Exception.Create('Grid file doesn''t exists');
    
  Cfg:=TXMLConfig.Create(FileName);
  Try
    Version:=cfg.GetValue('grid/version',-1);
    if Version=-1 Then Raise Exception.Create('Not a valid grid file');
    BeginUpdate;
    LoadContent(Cfg);
    EndUpdate(True);
  Finally
    FreeThenNil(Cfg);
  End;
end;

procedure TCustomGrid.Clear;
Var
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
Var
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
  try
    Result:=FCells[Col,Row];
  Except
    WriteLn('GetCell: Index Out of range Cell[Col=',Col,' Row=',Row,']');
  End;
end;

Function Tvirtualgrid.Getrows(Row: Integer): Pcellprops;
Begin
  Result:= FRows[Row, 0];
End;

Function Tvirtualgrid.Getcols(Col: Integer): Pcellprops;
Begin
  result:=FCols[Col, 0];
End;

procedure TVirtualGrid.SetCells(Col, Row: Integer; const AValue: PCellProps);
Var
   Cell: PCellProps;
begin
  // todo: Check range
  Cell:=FCells[Col,Row];
  If Cell<>nil Then DisposeCell(Cell);
  Cell:=AValue;
  FCells[Col,Row]:=Cell;
end;

Procedure Tvirtualgrid.Setrows(Row: Integer; Const Avalue: Pcellprops);
Var
   Cell: PCellProps;
begin
  // todo: Check range
  Cell:=FRows[Row,0];
  If Cell<>nil Then DisposeCell(Cell);
  FCells[Row,0]:=AValue;
end;

Procedure Tvirtualgrid.Setcolcount(Const Avalue: Integer);
Begin
  If FColCount=Avalue then Exit;
  {$Ifdef dbgMem}
    WriteLn('TVirtualGrid.SetColCount Value=',AValue);
  {$Endif}
  FColCount:=AValue;
  {$Ifdef dbgMem}
    Write('TVirtualGrid.SetColCount->FCOLS: ');
  {$Endif}
  FCols.SetLength(FColCount, 1);
  {$Ifdef dbgMem}
    Write('TVirtualGrid.SetColCount->FCELLS(',FColCount,',',FRowCount,'): ');
  {$Endif}
  FCells.SetLength(FColCount, FRowCount);
End;


Procedure Tvirtualgrid.Setrowcount(Const Avalue: Integer);
Begin
  If FRowCount=AValue Then Exit;
  {$Ifdef dbgMem}
    WriteLn('TVirtualGrid.SetRowCount Value=',AValue);
  {$Endif}
  FRowCount:=AValue;
  {$Ifdef dbgMem}
    Write('TVirtualGrid.SetRowCount->FROWS: ');
  {$Endif}
  FRows.SetLength(FRowCount,1);
  {$Ifdef dbgMem}
    Write('TVirtualGrid.SetRowCount->FCELLS(',FColCount,',',FRowCount,'): ');
  {$Endif}
  FCells.SetLength(FColCount, FRowCount);
End;

Procedure Tvirtualgrid.Setcols(Col: Integer; Const Avalue: Pcellprops);
Var
   Cell: PCellProps;
begin
  // todo: Check range
  Cell:=FCols[Col,0];
  If Cell<>nil Then DisposeCell(Cell);
  FCols[Col,0]:=AValue;
End;

Procedure Tvirtualgrid.Clear;
Begin
  {$Ifdef dbgMem}Write('FROWS: ');{$Endif}FRows.Clear;
  {$Ifdef dbgMem}Write('FCOLS: ');{$Endif}FCols.Clear;
  {$Ifdef dbgMem}Write('FCELLS: ');{$Endif}FCells.Clear;
  FColCount:=0;
  FRowCount:=0;
End;

Procedure Tvirtualgrid.Disposecell(Var P: Pcellprops);
Begin
  If P<>nil then begin
    If P^.Text<>nil Then begin
      StrDispose(P^.Text);
    End;
    If P^.Attr<>nil Then Dispose(P^.Attr);
    Dispose(P);
    P:=nil;
  End;
End;

function TVirtualGrid.GetDefaultCell: PcellProps;
begin
  New(Result);
  Result^.Text:=nil;
  Result^.Attr:=nil;
end;

Procedure Tvirtualgrid.Dodestroyitem (Sender: Tobject; Col,Row: Integer;
  Var Item: Pointer);
Begin
  {$IFNDef LogNil} If Item<>nil then {$Endif}
  {$Ifdef dbgMem}
    WriteLn('TVirtualGrid.doDestroyItem Col=',Col,' Row= ',
            Row,' Item=',Integer(Item));
  {$endif}
  If Item<>nil Then begin
    DisposeCell(pCellProps(Item));
    Item:=nil;
  End;
End;

Procedure Tvirtualgrid.Donewitem(Sender: Tobject; Col,Row:Integer;
  Var Item: Pointer);
Begin
  {$Ifdef dbgMem}
    WriteLn('TVirtualGrid.doNewItem Col=',Col,' Row= ',
            Row,' Item=',Integer(Item));
  {$endif}
  If Sender=FCols Then begin
    // Procesar Nueva Columna
    Item:=GetDefaultCell;
  End Else
  if Sender=FRows Then begin
    // Procesar Nuevo Renglon
    Item:=GetDefaultCell;
  End Else begin
    // Procesar Nueva Celda
    Item:=nil;
  End;
End;

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
  If IsColumn Then begin
    FCols.DeleteColRow(True, Index);
    Dec(FColCount);
  End Else begin
    FRows.DeleteColRow(True, Index);
    Dec(fRowCount);
  End;
end;

procedure TVirtualGrid.MoveColRow(IsColumn: Boolean; FromIndex, ToIndex: Integer
  );
begin
  FCells.MoveColRow(IsColumn, FromIndex, ToIndex);
  If IsColumn then FCols.MoveColRow(True, FromIndex, ToIndex)
  Else             FRows.MoveColRow(True, FromIndex, ToIndex);
end;

procedure TVirtualGrid.ExchangeColRow(IsColumn: Boolean; Index,
  WithIndex: Integer);
begin
  FCells.ExchangeColRow(IsColumn, Index, WithIndex);
  If IsColumn Then FCols.ExchangeColRow(true, Index, WithIndex)
  Else             FRows.ExchangeColRow(True, Index, WithIndex);
end;

{ TStringCellEditor }
procedure TStringCellEditor.KeyDown(var Key: Word; Shift: TShiftState);
begin
  {$IfDef dbg}
  WriteLn('INI: Key=',Key,' SelStart=',SelStart,' SelLenght=',SelLength);
  {$Endif}
  If FGrid<>nil then Fgrid.EditorKeyDown(Self, Key, Shift);
  {$IfDef dbg}
  WriteLn('FIN: Key=',Key,' SelStart=',SelStart,' SelLenght=',SelLength);
  {$Endif}
  inherited KeyDown(Key, Shift);
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
Var
  c: PCellProps;
begin
  C:=FGrid.Celda[ACol,ARow];
  If (C<>nil)And(C^.Attr<>nil) then Result:=C^.Attr^
  Else                              Result:=FDefCellAttr;
end;

function TDrawGrid.GetCellAlign(ACol, ARow: Integer): Integer;
Var
  Attr: TCellAttr;
begin
  Attr:=GetCellAttr(Acol,ARow);
  AttrToCellAlign(Attr, Result);
end;

function TDrawGrid.GetCellColor(ACol, ARow: Integer): TColor;
Var
  Attr: TCellAttr;
begin
  Attr:=GetCellAttr(ACol,ARow);
  Result:=Attr.Color;
end;

function TDrawGrid.GetCellFontCOlor(ACol, ARow: Integer): TColor;
Var
  Attr: TCellAttr;
begin
  Attr:=GetCellAttr(ACol,ARow);
  Result:=Attr.FontColor;
end;

function TDrawGrid.GetColAlign(aCol: Integer): Integer;
Var
  Attr: TCellAttr;
begin
  Attr:=GetColAttr(aCol);
  AttrToCellAlign(Attr, Result);
end;

function TDrawGrid.GetColAttr(aCol: Integer): TCellAttr;
Var
  c: PCellProps;
begin
  C:=FGrid.Cols[ACol];
  If (C<>nil)And(C^.Attr<>nil) then Result:=C^.Attr^
  Else                              Result:=FDefCellAttr;
end;

function TDrawGrid.GetColColor(aCol: Integer): TColor;
Var
  Attr: TCellAttr;
begin
  Attr:=GetColAttr(ACol);
  Result:=Attr.Color;
end;

function TDrawGrid.GetColFontColor(aCol: Integer): TColor;
Var
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
Var
  Attr: TCellAttr;
begin
  Attr:=GetRowAttr(aRow);
  AttrToCellAlign(Attr, Result);
end;

function TDrawGrid.GetRowAttr(aRow: Integer): TCellAttr;
Var
  c: PCellProps;
begin
  C:=FGrid.Rows[ARow];
  If (C<>nil)And(C^.Attr<>nil) then Result:=C^.Attr^
  Else                              Result:=FDefCellAttr;
end;

function TDrawGrid.GetRowColor(aRow: Integer): TColor;
Var
  Attr: TCellAttr;
begin
  Attr:=GetColAttr(ARow);
  Result:=Attr.Color;
end;

function TDrawGrid.GetRowFontColor(aRow: Integer): TColor;
Var
  Attr: TCellAttr;
begin
  Attr:=GetColAttr(ARow);
  Result:=Attr.FontColor;
end;

procedure TDrawGrid.SetCellAlign(ACol, ARow: Integer; const AValue: Integer);
Var
  Attr: TCellAttr;
begin
  Attr:=GetCellAttr(ACol, Arow);
  CellAlignToAttr(aValue, Attr);
  SetCellAttr(ACol,ARow,Attr);
end;

procedure TDrawGrid.SetCellAttr(ACol, ARow: Integer; const AValue: TCellAttr);
Var
  c: PCellProps;
  IsNew: Boolean;
begin
  C:=FGrid.Celda[ACol,ARow];
  IsNew:=C=nil;
  If IsNew Then C:=FGrid.GetDefaultCell;
  if C^.Attr=nil Then New(C^.Attr);
  C^.Attr^:=Avalue;
  If IsNew then FGrid.Celda[aCol,ARow]:=C; // Celda takes care
end;

procedure TDrawGrid.SetCellColor(ACol, ARow: Integer; const AValue: TColor);
Var
  Attr: TCellAttr;
begin
  Attr:=GetCellAttr(ACol, Arow);
  attr.Color:=AValue;
  SetCellAttr(ACol,ARow,Attr);
end;

procedure TDrawGrid.SetCellFontCOlor(ACol, ARow: Integer; const AValue: TColor);
Var
  Attr: TCellAttr;
begin
  Attr:=GetCellAttr(ACol, Arow);
  Attr.FontColor:=Avalue;
  SetCellAttr(ACol,ARow,Attr);
end;

procedure TDrawGrid.SetColAlign(aCol: Integer; const AValue: Integer);
Var
  Attr: TCellAttr;
begin
  Attr:=GetColAttr(ACol);
  CellAlignToAttr(aValue, Attr);
  SetColAttr(aCol, Attr);
end;

procedure TDrawGrid.SetColAttr(aCol: Integer; const AValue: TCellAttr);
Var
  c: PCellProps;
  IsNew: Boolean;
begin
  C:=FGrid.Cols[ACol];
  IsNew:=C=nil;
  If IsNew Then C:=FGrid.GetDefaultCell;
  if C^.Attr=nil Then New(C^.Attr);
  C^.Attr^:=Avalue;
  If IsNew then FGrid.Cols[aCol]:=C; // Celda takes care
end;

procedure TDrawGrid.SetColColor(aCol: Integer; const AValue: TColor);
Var
  Attr: TCellAttr;
begin
  Attr:=GetColAttr(ACol);
  Attr.Color:=AValue;
  SetColAttr(aCol, Attr);
end;

procedure TDrawGrid.SetColFontColor(aCol: Integer; const AValue: TColor);
Var
  Attr: TCellAttr;
begin
  Attr:=GetColAttr(ACol);
  Attr.FontColor:=AValue;
  SetColAttr(aCol, Attr);
end;

procedure TDrawGrid.SetDefaultCellAttr(const AValue: TCellAttr);
begin
  If CellAttrIgual(FDefCellAttr, AValue) Then Exit;
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
Var
  Attr: TCellAttr;
begin
  Attr:=GetRowAttr(ARow);
  CellAlignToAttr(Avalue, Attr);
  SetRowAttr(aRow, Attr);
end;

procedure TDrawGrid.SetRowAttr(aRow: Integer; const AValue: TCellAttr);
Var
  c: PCellProps;
  IsNew: Boolean;
begin
  C:=FGrid.Rows[aRow];
  IsNew:=C=nil;
  If IsNew Then C:=FGrid.GetDefaultCell;
  if C^.Attr=nil Then New(C^.Attr);
  C^.Attr^:=Avalue;
  If IsNew then FGrid.Rows[aRow]:=C; // Celda takes care
end;

procedure TDrawGrid.SetRowColor(aRow: Integer; const AValue: TColor);
Var
  Attr: TCellAttr;
begin
  Attr:=GetRowAttr(ARow);
  Attr.Color:=AValue;
  SetRowAttr(aRow, Attr);
end;

procedure TDrawGrid.SetRowFontColor(aRow: Integer; const AValue: TColor);
Var
  Attr: TCellAttr;
begin
  Attr:=GetRowAttr(ARow);
  Attr.FontColor:=AValue;
  SetRowAttr(aRow, Attr);
end;


procedure TDrawGrid.CalcCellExtent(acol, aRow: Integer; var aRect: TRect);
begin
  //
end;

procedure TDrawGrid.DrawCell(aCol,aRow: Integer; aRect: TRect;
  aState:TGridDrawState);
Begin
  If Assigned(OnDrawCell) And Not(CsDesigning in ComponentState) Then
    OnDrawCell(Self,aCol,aRow,aRect,aState)
  Else
    DefaultDrawCell(aCol,aRow,aRect,aState);
  Inherited DrawCellGrid(aRect,aCol,aRow,aState); // Draw the grid
End;

procedure TDrawGrid.DrawFocusRect(aCol, aRow: Integer; ARect: TRect;
  aState: TGridDrawstate);
begin
  // Draw focused cell if we have the focus
  If Self.Focused Then begin
    If (gdFocused in aState)Then begin
      Canvas.Pen.Color:=FFocusColor;
      Canvas.Pen.Style:=psDot;
      If goRowSelect in Options Then begin
        Canvas.MoveTo(FGCache.FixedWidth+1, aRect.Top+1);
        Canvas.LineTo(FGCache.MaxClientXY.x-1, aRect.Top+1);
        Canvas.LineTo(FGCache.MaxClientXY.x-1, aRect.Bottom-1);
        Canvas.LineTo(FGCache.FixedWidth+1, aRect.Bottom-1);
        Canvas.LineTo(FGCache.FixedWidth+1, aRect.Top+1);
      End Else begin
        Canvas.MoveTo(aRect.Left+1, aRect.Top+1);
        Canvas.LineTo(ARect.Right-1, ARect.Top+1);
        Canvas.LineTo(aRect.Right-1, aRect.bottom-1);
        Canvas.LineTo(aRect.Left+1, aRect.Bottom-1);
        Canvas.Lineto(aRect.left+1, aRect.top+1);
      End;
      Canvas.Pen.Style:=psSolid;
    End;
  End;
end;

procedure TDrawGrid.ColRowExchanged(IsColumn:Boolean; Index, WithIndex: Integer);
begin
  Fgrid.ExchangeColRow(IsColumn, Index, WithIndex);
  if Assigned(OnColRowExchanged) Then
    OnColRowExchanged(Self, IsColumn, Index, WithIndex);
end;

procedure TDrawGrid.ColRowDeleted(IsColumn: Boolean; Index: Integer);
begin
  FGrid.DeleteColRow(IsColumn, Index);
  If Assigned(OnColRowDeleted) Then
    OnColRowDeleted(Self, IsColumn, Index, Index);
end;

procedure TDrawGrid.ColRowMoved(IsColumn: Boolean; FromIndex, ToIndex: Integer);
begin
  FGrid.MoveColRow(IsColumn, FromIndex, ToIndex);
  If Assigned(OnColRowMoved) Then
    OnColRowMoved(Self, IsColumn, FromIndex, toIndex);
end;

procedure TDrawGrid.HeaderClick(IsColumn: Boolean; Index: Integer);
begin
  inherited HeaderClick(IsColumn, Index);
  If Assigned(OnHeaderClick) Then OnHeaderClick(Self, IsColumn, Index);
end;

procedure TDrawGrid.SizeChanged(OldColCount, OldRowCount: Integer);
begin
  If OldColCount<>ColCount Then fGrid.ColCount:=ColCOunt;
  If OldRowCount<>RowCount Then fGrid.RowCount:=RowCount;
end;

procedure TDrawGrid.SaveContent(cfg: TXMLConfig);
Var
  i,j,k: Integer;
  c: PCellProps;
  path: String;

  procedure SaveAttr;
  begin
    With c^.Attr^ do begin
      Cfg.SetValue(Path+'/color', ColorToString(Color));
      Cfg.SetValue(Path+'/fontcolor',ColorToString(FontColor));
      Cfg.SetValue(Path+'/textstyle/alignment/value', Ord(TextStyle.Alignment));
      cfg.SetValue(Path+'/textstyle/layout/value',Ord(TextStyle.Layout));
      cfg.SetValue(Path+'/textstyle/singleLine/value',TextStyle.SingleLine);
      cfg.SetValue(Path+'/textstyle/clipping/value',TextStyle.Clipping);
      cfg.SetValue(Path+'/textstyle/wordbreak/value',TextStyle.WordBreak);
      cfg.SetValue(Path+'/textstyle/opaque/value',TextStyle.Opaque);
      cfg.SetValue(Path+'/textstyle/systemfont/value',TextStyle.SystemFont);
    End;
  End;
begin
  Inherited SaveContent(cfg);
  Cfg.SetValue('grid/saveoptions/attributes', soAttributes in SaveOptions);
  if Not (soAttributes in SaveOptions) then Exit;
  
  // Save Columns
  j:=0;
  For i:=0 to ColCount-1 do begin
    c:=fGrid.Cols[i];
    If (c<>nil)And(c^.Attr<>nil) Then begin
      Inc(j);
      Cfg.SetValue('grid/attributes/columns/columncount', j);
      path:='grid/attributes/columns/column'+IntToStr(j);
      Cfg.SetValue(Path+'/index', i);
      SaveAttr;
    End;
  End;
  // Save Rows
  j:=0;
  For i:=0 to RowCount-1 do begin
    c:=fGrid.Rows[i];
    If (c<>nil)And(c^.Attr<>nil) Then begin
      Inc(j);
      Path:='grid/attributes/rows/row'+IntToStr(j);
      Cfg.SetValue(Path+'/index', i);
      SaveAttr;
    End;
  End;
  // Save attributtes of Cells
  k:=0;
  For i:=0 to ColCount-1 do
    For j:=0 to RowCount-1 do begin
      C:=fGrid.Celda[i,j];
      If (c<>nil)And(c^.Attr<>nil) Then begin
        Inc(k);
        Cfg.SetValue('grid/attributes/cells/cellcount',k);
        Path:='grid/attributes/cells/cell'+IntToStr(k);
        cfg.SetValue(Path+'/column',i);
        cfg.SetValue(Path+'/row',j);
        SaveAttr;
      End;
    End;
end;

procedure TDrawGrid.LoadContent(Cfg: TXMLConfig);
Var
  i,j,k: Integer;
  B: Boolean;
  path: String;
begin

  Inherited LoadContent(Cfg);

  If Not (soAttributes in SaveOptions) Then Exit;
  B:=Cfg.GetValue('grid/saveoptions/attributes',false);
  If Not B Then Exit;

  // Load Columns
  Path:='grid/attributes/columns/';
  k:=cfg.getValue(Path+'columncount',0);
  For i:=1 to k do begin
    j:=cfg.getValue(Path+'column'+IntToStr(i)+'/index', -1);
    if (j>=0)And(j<=Colcount-1) Then begin
      ColAttr[j]:=LoadCellAttrFromXMLPath(cfg, Path+'column'+IntToStr(i));
    End;
  End;
  // Load Rows
  Path:='grid/attributes/rows/';
  k:=cfg.getValue(Path+'rowcount',0);
  For i:=1 to k do begin
    j:=cfg.getValue(Path+'row'+IntToStr(i)+'/index', -1);
    if (j>=0)and(j<=RowCount-1) Then begin
      RowAttr[j]:=LoadCellAttrFromXMLPath(cfg, Path+'row'+IntToStr(i));
    End;
  End;
  // Load Cells
  Path:='grid/attributes/cells/';
  k:=cfg.getValue(Path+'cellcount',0);
  While k>0 do begin
    i:=cfg.getValue(Path+'cell'+inttoStr(k)+'/column', -1);
    j:=cfg.getValue(Path+'cell'+inttostr(k)+'/row', -1);
    if (j>=0)And(j<=rowcount-1)and(i>=0)and(i<=Colcount-1) Then begin
      CellAttr[i,j]:=LoadCellAttrFromXMLPath(cfg, Path+'cell'+IntToStr(k));
    End;
    dec(k);
  End;
end;

constructor TDrawGrid.Create(AOwner: TComponent);
begin
  fGrid:=TVirtualGrid.Create;
  FDefCellAttr:=GetDefaultCellAttr;
  FDefSelCellAttr:=FDefCellAttr;
  With FDefSelCellAttr do begin
    Color:=clBlack;
    FontColor:=clWhite;
  End;
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

procedure TDrawGrid.DefaultDrawCell(aCol, aRow: Integer; Var aRect: TRect;
  aState: TGridDrawState);
Var
   c: PcellProps;
begin
  // Set draw Cell Attributes
  if DefaultDrawing or (csDesigning in ComponentState) then Begin
    if gdFixed in aState Then FCellAttr.Color:=clBtnFace
    Else fCellAttr.Color:=Self.Color;
    FCellAttr.FontColor:=Self.Font.Color;
    FCellAttr.TextStyle.Clipping:=False;
  End Else begin
    // Column -> Row -> Cell Specific Attributtes
    fCellAttr:=fDefCellAttr;
    If gdSelected in aState then FCellAttr:=FDefSelCellAttr
    Else begin
      If gdFixed in aState Then begin
        FCellAttr:=FDefFixedCellAttr;
        Case MouseToGridZone(aCol, aRow, true) of
          gzFixedRows: C:=FGrid.Cols[aCol];
          gzFixedCols: C:=FGrid.Rows[aRow];
          Else c:=nil;
        End;
        If (c<>nil)and(C^.Attr<>nil) Then FCellAttr:=C^.Attr^;
      End
      Else begin
        C:=FGrid.Cols[aCol]; If (c<>nil)and(C^.Attr<>nil) Then FCellAttr:=C^.Attr^;
        C:=FGrid.Rows[aRow]; If (c<>nil)And(C^.Attr<>nil) Then FCellAttr:=C^.Attr^;
      End;
      C:= FGrid.Celda[aCol,aRow];If (C<>nil)And(C^.Attr<>nil) Then FCellAttr:=C^.Attr^;
    End;
    if Assigned(fonCellAttr) Then fonCellAttr(Self, aCol,aRow, aState, FCellAttr);
    If goColSpanning in Options Then CalcCellExtent(acol, arow, aRect);
  End;
  Canvas.Brush.Color:=fCellAttr.Color;
  Canvas.Font.Color:=fCellAttr.FontColor;
  Canvas.FillRect(aRect);
end;

{ TStringGrid }

Function TStringGrid.Getcells(aCol, aRow: Integer): String;
Var
   C: PCellProps;
Begin
  Result:='';
  C:=FGrid.Celda[aCol,aRow];
  If C<>nil Then Result:=C^ .Text;
End;

function TStringGrid.GetCols(Index: Integer): TStrings;
Var
  i,j: Integer;
begin
  Result:=nil;
  If (ColCount>0)And(index>=0)and(Index<ColCount) Then begin
    Result:=TStringList.Create;
    For i:=0 to RowCount-1 do Begin
      j:=Result.Add( Cells[Index, i] );
      Result.Objects[j]:=Objects[Index, i];
    End;
  End;
end;

function TStringGrid.GetObjects(ACol, ARow: Integer): TObject;
Var
  C: PCellProps;
begin
  Result:=nil;
  C:=Fgrid.Celda[aCol,aRow];
  If C<>nil Then Result:=C^.Data;
end;

function TStringGrid.GetRows(Index: Integer): TStrings;
Var
  i,j: Integer;
begin
  Result:=nil;
  If (RowCount>0)And(index>=0)and(Index<RowCount) Then begin
    Result:=TStringList.Create;
    For i:=0 to ColCount-1 do Begin
      j:=Result.Add( Cells[i, Index] );
      Result.Objects[j]:=Objects[i, Index];
    End;
  End;
end;

Procedure TStringGrid.Setcells(aCol, aRow: Integer; Const Avalue: String);
Var
  C: PCellProps;
Begin
  C:= FGrid.Celda[aCol,aRow];
  If C<>nil Then begin
    If C^.Text<>nil Then StrDispose(C^.Text);
    C^.Text:=StrNew(pchar(aValue));
    InvalidateCell(aCol, aRow);
  End Else Begin
    If AValue<>'' Then Begin
      New(C);
      C^.Text:=StrNew(pchar(Avalue));
      C^.Attr:=nil;
      FGrid.Celda[aCol,aRow]:=C;
      InvalidateCell(aCol, aRow);
    End;
  End;
End;

procedure TStringGrid.SetCols(Index: Integer; const AValue: TStrings);
begin

end;

procedure TStringGrid.SetObjects(ACol, ARow: Integer; AValue: TObject);
Var
  c: PCellProps;
begin
  C:=FGrid.Celda[aCol,aRow];
  If c<>nil Then C^.Data:=AValue
  Else begin
    c:=fGrid.GetDefaultCell;
    c^.Data:=Avalue;
    FGrid.Celda[aCol,aRow]:=c;
  End;
end;

procedure TStringGrid.SetRows(Index: Integer; const AValue: TStrings);
begin

end;

procedure TStringGrid.AutoAdjustColumn(aCol: Integer);
Var
  i,W: Integer;
  Ts: TSize;
begin
  if (aCol<0)or(aCol>ColCount-1) Then Exit;
  W:=0;
  For i:=0 to RowCount-1 do begin
    Ts:=Canvas.TextExtent(Cells[aCol, i]);
    If Ts.Cx>W Then W:=Ts.Cx;
  End;
  If W=0 Then W:=DefaultColWidth
  Else        W:=W + 8;
  ColWidths[aCol]:=W;
end;

procedure TStringGrid.CalcCellExtent(acol, aRow: Integer; var aRect: TRect);
Var
  S: String;
  Ts: Tsize;
  nc: PcellProps;
  i: integer;
begin
  inherited CalcCellExtent(acol,arow, aRect);
  S:=Cells[aCol,aRow];
  If Not FCellAttr.TextStyle.Clipping Then begin
    // Calcular el numero de celdas necesarias para contener todo
    // El Texto
    Ts:=Canvas.TextExtent(S);
    i:=aCol;
    While (Ts.Cx>(aRect.Right-aRect.Left))and(i<ColCount) do begin
      inc(i);
      Nc:=FGrid.Celda[i, aRow];
      if (nc<>nil)And(Nc^.Text<>'')Then Break;
      aRect.Right:=aRect.Right + getColWidths(i);
    End;
    fcellAttr.TextStyle.Clipping:=i<>aCol;
  End;
end;

procedure TStringGrid.DrawCell(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState);
Var
  S: String;
begin
  inherited DrawCell(aCol, aRow, aRect, aState);
  S:=Cells[aCol,aRow];
  If S<>'' Then begin
    Canvas.TextRect(aRect, 3, 0, S, FCellAttr.TextStyle);
  End;
end;

procedure TStringGrid.doEditorGetValue;
Var
  msg: TGridMessage;
begin
  If (FEditor<>nil) And FEditor.Visible Then Begin
    Msg.MsgID:=GM_GETVALUE;
    Msg.grid:=Self;
    Msg.Col:=FCol;
    msg.Row:=FRow;
    msg.Value:=Cells[FCol,FRow];
    FEditor.Dispatch(Msg);
    Cells[FCol,FRow]:=msg.Value;
    //FEditor.Perform(GM_GETVALUE, Integer(Self), Integer(@Msg));
  End;
  //inherited EditorGetValue;
end;

procedure TStringGrid.doEditorSetValue;
Var
  msg: TGridMessage;
begin
  if FEditor<>nil Then begin
    Msg.MsgID:=GM_SETVALUE;
    Msg.Grid:=Self;
    Msg.Col:=FCol;
    Msg.Row:=FRow;
    Msg.Value:=Cells[FCol,FRow];
    FEditor.Dispatch(Msg);
  End;
  //inherited EditorSetValue;
end;

procedure TStringGrid.SaveContent(cfg: TXMLConfig);
Var
  i,j,k: Integer;
  c: PCellProps;
begin
  inherited SaveContent(cfg);
  cfg.SetValue('grid/saveoptions/content', soContent in SaveOptions);
  If soContent in SaveOptions Then begin
    // Save Cell Contents
    k:=0;
    For i:=0 to ColCount-1 do
      For j:=0 to RowCount-1 do begin
        C:=fGrid.Celda[i,j];
        If (c<>nil) And (C^.Text<>'') Then begin
          Inc(k);
          Cfg.SetValue('grid/content/cells/cellcount',k);
          cfg.SetValue('grid/content/cells/cell'+IntToStr(k)+'/column',i);
          cfg.SetValue('grid/content/cells/cell'+IntToStr(k)+'/row',j);
          cfg.SetValue('grid/content/cells/cell'+IntToStr(k)+'/text', c^.Text);
        End;
      End;
   End;
end;

procedure TStringGrid.LoadContent(Cfg: TXMLConfig);
Var
  ContentSaved: Boolean;
  i,j,k: Integer;
begin
  inherited LoadContent(Cfg);
  If soContent in FSaveOptions Then begin
    ContentSaved:=Cfg.GetValue('grid/saveoptions/content', false);
    If ContentSaved Then begin
      k:=cfg.getValue('grid/content/cells/cellcount', 0);
      While k>0 do begin
        i:=cfg.GetValue('grid/content/cells/cell'+IntToStr(k)+'/column', -1);
        j:=cfg.GetValue('grid/content/cells/cell'+IntTostr(k)+'/row',-1);
        if (j>=0)And(j<=rowcount-1)and(i>=0)and(i<=Colcount-1) Then
          Cells[i,j]:=cfg.GetValue('grid/content/cells/cell'+IntToStr(k)+'/text','');
        Dec(k);
      End;
    End;
  End;
end;

procedure TStringGrid.DrawInteriorCells;
Var
  i,j: Integer;
  gds: TGridDrawState;
  c: PCellProps;
begin
  With FGCache.VisibleGrid do
  if goColSpanning in Options Then begin
    //
    // Ordered draw should be done in order to this work
    //
    Gds:=[];
    // Draw Empty (nil) cells First
    For i:=Left to Right do
      For j:=Top to Bottom do begin
        if IsCellSelected(i,j) Then Continue;
        C:=Fgrid.Celda[i,j];
        If (c=nil) Then DrawCell(i,j, ColRowToClientCellRect(i,j), gds);
      End;
     // Draw Cells Empty Cells (Text='') With Attribute
    For i:=Left to Right do
      For j:=Top to Bottom do begin
        if IsCellSelected(i,j) Then Continue;
        If (i=FCol)or(j=FRow) Then Continue;
        C:=Fgrid.Celda[i,j];
        If (c<>nil)And(C^.Text='') Then
          DrawCell(i,j, ColRowToClientCellRect(i,j), gds);
      End;
    // Draw Cells Not Empty (Text<>'')
    For i:=Left to Right do
      For j:=Top to Bottom do begin
        if IsCellSelected(i,j) Then Continue;
        C:=Fgrid.Celda[i,j];
        If (C<>nil)And(C^.Text<>'') Then
          DrawCell(i,j, ColRowToClientCellRect(i,j), gds);
      End;

    gds:=[gdSelected];
    For i:=Left To Right do
      For j:=Top to Bottom do
        If IsCellSelected(i,j) Then begin
          DrawCell(i,j, colRowToClientCellRect(i,j), gds);
        End;

  End else inherited DrawInteriorCells;
end;

procedure TStringGrid.MoveSelection;
begin
  If goEditing in Options Then Editor:=fDefEditor;
  inherited MoveSelection;
end;

constructor TStringGrid.Create(AOWner: TComponent);
begin
  inherited Create(AOWner);
  if Not (csDesigning in componentState) Then begin
    FDefEditor:=TStringCellEditor.Create(nil);
    FDefEditor.Name:='Default_StringCellEditor';
    FDefEditor.Visible:=False;
    FDefEditor.Align:=alNone;
  End Else Begin
    FDefEditor:=nil;
  End;
end;

destructor TStringGrid.Destroy;
begin
  {$Ifdef dbg}WriteLn('TStringGrid.Destroy');{$Endif}
  if FdefEditor<>nil Then begin
    FDefEDitor.Parent:=nil;
    FreeThenNil(FDefEditor);
  End;
  inherited Destroy;
end;



initialization

LazarusResources.Add('tdrawgrid','XPM',[
  '/* XPM */'#10'static char *tdrawgrid[]={'#10'"23 23 8 1",'#10'"# c #000000",'
  +#10'"f c #0000c0",'#10'"b c #808080",'#10'". c #a0a0a0",'#10'"c c #ff0000",'
  +#10'"d c #ff00ff",'#10'"a c #ffffc0",'#10'"e c #ffffff",'#10'"....#.....#...'
  +'..#.....#",'#10'"....#.....#.....#.....#",'#10'"....#.....#.....#.....#",'
  +#10'"....#.....#.....#.....#",'#10'"#######################",'#10'"....#aaaa'
  +'abaaaaabaaaaaa",'#10'"....#aaaaabaaaaabaaaaaa",'#10'"....#aaacccccccabaaaaa'
  +'a",'#10'"....#aaacccccccabaadaaa",'#10'"#####bbbcccccccbbbdddbb",'#10'"....'
  +'#eeecccccccebedddee",'#10'"....#eeecccccccebddddde",'#10'"....#eeecccccffff'
  +'ddddde",'#10'"....#eeeccccffffffddddd",'#10'"#####bbbbbbbffffffddddd",'#10
  +'"....#aaaaabaffffffaaaaa",'#10'"....#aaaaabaffffffaaaaa",'#10'"....#aaaaaba'
  +'affffaaaaaa",'#10'"....#aaaaabaaaaabaaaaaa",'#10'"#####bbbbbbbbbbbbbbbbbb",'
  +#10'"....#eeeeebeeeeebeeeeee",'#10'"....#eeeeebeeeeebeeeeee",'#10'"....#eeee'
  +'ebeeeeebeeeeee"};'#10
]);
LazarusResources.Add('tstringgrid','XPM',[
  '/* XPM */'#10'static char *tstringgrid[]={'#10'"23 23 5 1",'#10'"# c #000000'
  +'",'#10'"b c #808080",'#10'". c #a0a0a0",'#10'"a c #ffffc0",'#10'"c c #fffff'
  +'f",'#10'"....#.....#.....#.....#",'#10'"....#.....#.....#.....#",'#10'"....'
  +'#.....#.....#.....#",'#10'"....#.....#.....#.....#",'#10'"#################'
  +'######",'#10'"....#aaaaabaaaaabaaaaaa",'#10'"....#aaaaabaaaaabaaaaaa",'#10
  +'"....#aaaaabaaaaabaaaaaa",'#10'"....#aaaaabaaaaabaaaaaa",'#10'"#####bbbbbbb'
  +'bbbbbbbbbbb",'#10'"....#cccccbcccccbcccccc",'#10'"....#cccccbcccccbcccccc",'
  +#10'"....#cccccbcccccbcccccc",'#10'"....#cccccbcccccbcccccc",'#10'"#####bbbb'
  +'bbbbbbbbbbbbbb",'#10'"....#aaaaabaaaaabaaaaaa",'#10'"....#aaaaabaaaaabaaaaa'
  +'a",'#10'"....#aaaaabaaaaabaaaaaa",'#10'"....#aaaaabaaaaabaaaaaa",'#10'"####'
  +'#bbbbbbbbbbbbbbbbbb",'#10'"....#cccccbcccccbcccccc",'#10'"....#cccccbcccccb'
  +'cccccc",'#10'"....#cccccbcccccbcccccc"};'#10
]);

end.
