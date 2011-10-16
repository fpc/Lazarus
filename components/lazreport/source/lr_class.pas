
{*****************************************}
{                                         }
{             FastReport v2.3             }
{             Report classes              }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit LR_Class;

interface

{$I LR_Vers.inc}

uses
  SysUtils, {$IFDEF UNIX}CLocale,{$ENDIF} Classes, MaskUtils, Controls, FileUtil,
  Forms, ComCtrls, Dialogs, Menus, Variants, DB, Graphics, Printers, osPrinters,
  DOM, XMLRead, XMLConf, LCLType, LCLIntf, TypInfo, LCLProc, LR_View, LR_Pars,
  LR_Intrp, LR_DSet, LR_DBSet, LR_DBRel, LR_Const;

const
// object flags
  flStretched              = 1;
  flWordWrap               = 2;
  flWordBreak              = 4;
  flAutoSize               = 8;
  flHideDuplicates         = $10;
  flStartRecord            = $20;
  flEndRecord              = $40;

  flBandNewPageAfter       = 2;
  flBandPrintifSubsetEmpty = 4;
  flBandPageBreak          = 8;
  flBandOnFirstPage        = $10;
  flBandOnLastPage         = $20;
  flBandRepeatHeader       = $40;

  flPictCenter             = 2;
  flPictRatio              = 4;
  flWantHook               = $8000;
  flIsDuplicate            = $4000;

// object types
  gtMemo                   = 0;
  gtPicture                = 1;
  gtBand                   = 2;
  gtSubReport              = 3;
  gtLine                   = 4;
  gtAddIn                  = 10;

//format type
  fmtText                  = 0;
  fmtNumber                = 1;
  fmtDate                  = 2;
  fmtTime                  = 3;
  fmtBoolean               = 4;
  
type
  TfrDrawMode = (drAll, drCalcHeight, drAfterCalcHeight, drPart);
  TfrBandType = (btReportTitle, btReportSummary,
                 btPageHeader, btPageFooter,
                 btMasterHeader, btMasterData, btMasterFooter,
                 btDetailHeader, btDetailData, btDetailFooter,
                 btSubDetailHeader, btSubDetailData, btSubDetailFooter,
                 btOverlay, btColumnHeader, btColumnFooter,
                 btGroupHeader, btGroupFooter,
                 btCrossHeader, btCrossData, btCrossFooter, btNone);
  TfrBandTypes = set of TfrBandType;
  TfrDataSetPosition = (psLocal, psGlobal);
  TfrValueType = (vtNotAssigned, vtDBField, vtOther, vtFRVar);
  TfrPageMode = (pmNormal, pmBuildList);
  TfrBandRecType = (rtShowBand, rtFirst, rtNext);
  TfrRgnType = (rtNormal, rtExtended);
  TfrReportType = (rtSimple, rtMultiple);
  TfrStreamMode = (smDesigning, smPrinting);
  TfrFrameBorder = (frbLeft, frbTop, frbRight, frbBottom);
  TfrFrameBorders = set of TfrFrameBorder;
  TfrFrameStyle = (frsSolid,frsDash, frsDot, frsDashDot, frsDashDotDot,frsDouble);
  TfrPageType = (ptReport, ptDialog);
  TfrReportOption = (roIgnoreFieldNotFound);
  TfrReportOptions = set of TfrReportOption;

  TfrView = class;
  TfrBand = class;
  TfrPage = class;
  TfrReport = class;
  TfrExportFilter = class;

  TDetailEvent = procedure(const ParName: String; var ParValue: Variant) of object;
  TEnterRectEvent = procedure(Memo: TStringList; View: TfrView) of object;
  TBeginDocEvent = procedure of object;
  TEndDocEvent = procedure of object;
  TBeginPageEvent = procedure(pgNo: Integer) of object;
  TEndPageEvent = procedure(pgNo: Integer) of object;
  TBeginBandEvent = procedure(Band: TfrBand) of object;
  TEndBandEvent = procedure(Band: TfrBand) of object;
  TfrProgressEvent = procedure(n: Integer) of object;
  TBeginColumnEvent = procedure(Band: TfrBand) of object;
  TPrintColumnEvent = procedure(ColNo: Integer; var Width: Integer) of object;
  TManualBuildEvent = procedure(Page: TfrPage) of object;

  TfrHighlightAttr = packed record
    FontStyle: Word;
    FontColor, FillColor: TColor;
  end;

  // print info about page size, margins e.t.c
  TfrPrnInfo = record
    PPgw, PPgh, Pgw, Pgh : Integer; // page width/height (printer/screen)
    POfx, POfy, Ofx, Ofy : Integer; // offset x/y
    PPw, PPh, Pw, Ph     : Integer; // printable width/height
  end;

  PfrPageInfo = ^TfrPageInfo;
  TfrPageInfo = packed record // pages of a preview
    R         : TRect;
    pgSize    : Word;
    pgWidth   : Integer;
    pgHeight  : Integer;
    pgOr      : TPrinterOrientation;
    pgMargins : Boolean;
    PrnInfo   : TfrPrnInfo;
    Visible   : Boolean;
    Stream    : TMemoryStream;
    Page      : TfrPage;
  end;

  PfrBandRec = ^TfrBandRec;
  TfrBandRec = packed record
    Band   : TfrBand;
    Action : TfrBandRecType;
  end;
  
  TLayoutOrder = (loColumns, loRows);

  TfrMemoStrings  =Class(TStringList);
  TfrScriptStrings=Class(TStringList);
  
  TfrDialogForm = Class(TForm);
  
  { TLrXMLConfig }

  TLrXMLConfig = class (TXMLConfig)
  public
    procedure LoadFromStream(const Stream: TStream);
    procedure SetValue(const APath: string; const AValue: string); overload;
    function  GetValue(const APath: string; const ADefault: string): string; overload;
  end;

  { TfrObject }

  TfrObject = Class(TPersistent)
  private
    fMemo   : TfrMemoStrings;
    fName   : string;
    fScript : TfrScriptStrings;
    fVisible: Boolean;
    fUpdate : Integer;
    
    procedure SetMemo(const AValue: TfrMemoStrings);
    procedure SetName(const AValue: string);
    procedure SetScript(const AValue: TfrScriptStrings);
  protected
    BaseName  : String;
    
    function GetSaveProperty(const Prop : String; aObj : TPersistent=nil) : string;
    procedure RestoreProperty(const Prop,aValue : String; aObj : TPersistent=nil);
  public
    x, y, dx, dy: Integer;

    constructor Create; virtual;
    destructor Destroy; override;
    
    procedure Assign(From: TfrView); virtual; overload;

    procedure BeginUpdate;
    procedure EndUpdate;
    
    procedure CreateUniqueName;

    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); virtual;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); virtual;

    property Memo   : TfrMemoStrings read fMemo write SetMemo;
    property Script : TfrScriptStrings read fScript write SetScript;
    property Left   : Integer read x write x;
    property Top    : Integer read y write y;
    property Width  : Integer read dx write dx;
    property Height : Integer read dy write dy;

  published
    property Name   : string read fName write SetName;
    property Visible: Boolean read fVisible write fVisible;
  end;
  
  { TfrView }

  TfrView = class(TfrObject)
  private
    fFillColor : TColor;
    fCanvas    : TCanvas;
    fFrameColor: TColor;
    fFrames    : TfrFrameBorders;
    fFrameStyle: TfrFrameStyle;
    fFrameWidth: Double;
    fStreamMode: TfrStreamMode;
    fFormat    : Integer;
    fFormatStr : string;
    function GetHeight: Double;
    function GetLeft: Double;
    function GetStretched: Boolean;
    function GetTop: Double;
    function GetWidth: Double;
    procedure P1Click(Sender: TObject);
    procedure SetFillColor(const AValue: TColor);
    procedure SetFormat(const AValue: Integer);
    procedure SetFormatStr(const AValue: String);
    procedure SetFrameColor(const AValue: TColor);
    procedure SetFrames(const AValue: TfrFrameBorders);
    procedure SetFrameStyle(const AValue: TfrFrameStyle);
    procedure SetFrameWidth(const AValue: Double);
    procedure SetHeight(const AValue: Double);
    procedure SetLeft(const AValue: Double);
    procedure SetStretched(const AValue: Boolean);
    procedure SetTop(const AValue: Double);
    procedure SetWidth(const AValue: Double);
  protected
    SaveX, SaveY, SaveDX, SaveDY: Integer;
    SaveFW: Double;

    gapx, gapy: Integer;
    Memo1: TStringList;
    FDataSet: TfrTDataSet;
    FField: String;
    olddy: Integer;

    procedure ShowBackGround; virtual;
    procedure ShowFrame; virtual;
    procedure BeginDraw(ACanvas: TCanvas);
    procedure GetBlob(b: TfrTField); virtual;
    procedure OnHook(View: TfrView); virtual;
    procedure BeforeChange;
    procedure AfterChange;
    procedure ResetLastValue; virtual;
    function GetFrames: TfrFrameBorders; virtual;
  public
    Parent: TfrBand;
    ID: Integer;
    Typ: Byte;
    Selected: Boolean;
    OriginalRect: TRect;
    ScaleX, ScaleY: Double;   // used for scaling objects in preview
    OffsX, OffsY: Integer;    //
    IsPrinting: Boolean;
    Flags: Word;
    DRect: TRect;
    ParentBandType: TfrBandType; // identify parent band type on exporting view

    constructor Create; override;
    destructor Destroy; override;
    
    procedure Assign(From: TfrView); override;
    procedure CalcGaps; virtual;
    procedure RestoreCoord; virtual;
    procedure Draw(aCanvas: TCanvas); virtual; abstract;
    procedure Print(Stream: TStream); virtual;
    procedure ExportData; virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure SaveToStream(Stream: TStream); virtual;

    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); override;

    procedure Resized; virtual;
    procedure DefinePopupMenu(Popup: TPopupMenu); virtual;
    function GetClipRgn(rt: TfrRgnType): HRGN; virtual;
    procedure SetBounds(aLeft, aTop, aWidth, aHeight: Integer);

    function PointInView(aX,aY : Integer) : Boolean; virtual;
    procedure Invalidate;

    property Canvas : TCanvas read fCanvas write fCanvas;

    property FillColor : TColor read fFillColor write SetFillColor;
    property Stretched : Boolean read GetStretched write SetStretched;

    property Frames : TfrFrameBorders read GetFrames write SetFrames;
    property FrameColor : TColor read fFrameColor write SetFrameColor;
    property FrameStyle : TfrFrameStyle read fFrameStyle write SetFrameStyle;
    property FrameWidth : Double read fFrameWidth write SetFrameWidth;
    property Format     : Integer read fFormat write SetFormat;
    property FormatStr  : String read fFormatStr write SetFormatStr;

    property StreamMode: TfrStreamMode read fStreamMode write fStreamMode;

  published
    property Left: double read GetLeft write SetLeft;
    property Top: double read GetTop write SetTop;
    property Width: double read GetWidth write SetWidth;
    property Height: double read GetHeight write SetHeight;
  end;
  TfrViewClass = Class of TFRView;

  TfrStretcheable = class(TfrView)
  protected
    ActualHeight: Integer;
    DrawMode: TfrDrawMode;

    function CalcHeight: Integer; virtual; abstract;
    function MinHeight: Integer; virtual; abstract;
    function RemainHeight: Integer; virtual; abstract;
  published
    property Stretched;
  end;

  { TfrMemoView }

  TfrMemoView = class(TfrStretcheable)
  private
    fAngle       : Byte;
    fFont        : TFont;
    fLastValue   : TStringList;

    function GetAlignment: TAlignment;
    function GetAutoSize: Boolean;
    function GetHideDuplicates: Boolean;
    function GetIsLastValueSet: boolean;
    function GetLayout: TTextLayout;
    function GetWordWrap: Boolean;
    procedure P1Click(Sender: TObject);
    procedure P2Click(Sender: TObject);
    procedure P3Click(Sender: TObject);
    procedure P4Click(Sender: TObject);
    procedure P5Click(Sender: TObject);
    procedure SetAlignment(const AValue: TAlignment);
    procedure SetAutoSize(const AValue: Boolean);
    procedure SetFont(Value: TFont);
    procedure SetHideDuplicates(const AValue: Boolean);
    procedure SetIsLastValueSet(const AValue: boolean);
    procedure SetLayout(const AValue: TTextLayout);
    procedure SetWordWrap(const AValue: Boolean);
  protected
    Streaming: Boolean;
    TextHeight: Integer;
    CurStrNo: Integer;
    Exporting: Boolean;

    procedure ExpandVariables;
    procedure AssignFont(aCanvas: TCanvas);
    procedure WrapMemo;
    procedure ShowMemo;
    function CalcWidth(aMemo: TStringList): Integer;
    function CalcHeight: Integer; override;
    function MinHeight: Integer; override;
    function RemainHeight: Integer; override;
    procedure GetBlob(b: TfrTField); override;
    procedure FontChange(sender: TObject);
    procedure ResetLastValue; override;

    property IsLastValueSet: boolean read GetIsLastValueSet write SetIsLastValueSet;
  public
    Adjust: Integer; // bit format xxxLLRAA: LL=Layout, R=Rotated, AA=Alignment
    Highlight: TfrHighlightAttr;
    HighlightStr: String;
    LineSpacing, CharacterSpacing: Integer;
    
    constructor Create; override;
    destructor Destroy; override;
    
    procedure Assign(From: TfrView); override;
    procedure Draw(aCanvas: TCanvas); override;
    procedure Print(Stream: TStream); override;
    procedure ExportData; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); override;
    procedure DefinePopupMenu(Popup: TPopupMenu); override;

    procedure MonitorFontChanges;
    
  published
    property Font      : TFont read fFont write SetFont;
    property Alignment : TAlignment read GetAlignment write SetAlignment;
    property Layout    : TTextLayout read GetLayout write SetLayout;
    property Angle     : Byte read fAngle write fAngle;
    property WordWrap  : Boolean read GetWordWrap write SetWordWrap;
    property AutoSize  : Boolean read GetAutoSize write SetAutoSize;
    property HideDuplicates: Boolean read GetHideDuplicates write SetHideDuplicates;
    
    property FillColor;
    property Memo;
    property Script;
    property Frames;
    property FrameColor;
    property FrameStyle;
    property FrameWidth;
    property Format;
    property FormatStr;
  end;

  { TfrBandView }

  TfrBandView = class(TfrView)
  private
    fDataSetStr : String;
    fBandType   : TfrBandType;
    fCondition  : String;
    
    procedure P1Click(Sender: TObject);
    procedure P2Click(Sender: TObject);
    procedure P3Click(Sender: TObject);
    procedure P4Click(Sender: TObject);
    procedure P5Click(Sender: TObject);
    procedure P6Click(Sender: TObject);
    function  GetTitleRect: TRect;
    function  TitleSize: Integer;
    procedure CalcTitleSize;
  public
    constructor Create; override;

    procedure Assign(From: TfrView); override;
    
    procedure LoadFromStream(Stream: TStream); override;
    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); override;
    

    procedure Draw(aCanvas: TCanvas); override;
    procedure DefinePopupMenu(Popup: TPopupMenu); override;
    function GetClipRgn(rt: TfrRgnType): HRGN; override;
    
    function PointInView(aX,aY : Integer) : Boolean; override;


  published
    property DataSet: String read fDataSetStr write fDataSetStr;
    property GroupCondition: String read fCondition write fCondition;

    property BandType: TfrBandType read fBandType write fBandType;

    property Script;
    property Stretched;
  end;

  { TfrSubReportView }

  TfrSubReportView = class(TfrView)
  public
    SubPage: Integer;
    constructor Create; override;
    procedure Assign(From: TfrView); override;
    procedure Draw(aCanvas: TCanvas); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); override;
    procedure DefinePopupMenu(Popup: TPopupMenu); override;
  end;

  { TfrPictureView }

  TfrPictureView = class(TfrView)
  private
    fPicture: TPicture;
    FSharedName: string;
    
    procedure P1Click(Sender: TObject);
    procedure P2Click(Sender: TObject);
    function GetPictureType: byte;
    function PictureTypeToGraphic(b: Byte): TGraphic;
    function ExtensionToGraphic(const Ext: string): TGraphic;
    function StreamToGraphic(M: TMemoryStream): TGraphic;
    procedure SetPicture(const AValue: TPicture);
  protected
    procedure GetBlob(b: TfrTField); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    
    procedure Assign(From: TfrView); override;
    procedure Draw(aCanvas: TCanvas); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); override;
    procedure DefinePopupMenu(Popup: TPopupMenu); override;
  published
    property Picture : TPicture read fPicture write SetPicture;

    property Memo;
    property Script;
    property Frames;
    property FrameColor;
    property FrameStyle;
    property FrameWidth;
    property Stretched;
    property SharedName: string read FSharedName write FSharedName;
  end;

  { TfrLineView }

  TfrLineView = class(TfrView)
  protected
    function GetFrames: TfrFrameBorders; override;
  public
    constructor Create; override;

    procedure Draw(aCanvas: TCanvas); override;
    procedure DefinePopupMenu(Popup: TPopupMenu); override;
    function GetClipRgn(rt: TfrRgnType): HRGN; override;
    function PointInView(aX,aY: Integer): Boolean; override;
    
  published
    property FrameColor;
    property FrameStyle;
    property FrameWidth;
  end;

  TfrRect = Class(TPersistent)
  private
    fBottom: Integer;
    fLeft: Integer;
    fRight: Integer;
    fTop: Integer;
    function GetRect: TRect;
    procedure SetRect(const AValue: TRect);
  public
    property AsRect : TRect read GetRect write SetRect;
    
  published
    property Left : Integer read fLeft write fLeft;
    property Top  : Integer read fTop  write fTop;
    property Right: Integer read fRight write fRight;
    property Bottom : Integer read fBottom write fBottom;
  end;
  
  TfrBand = class(TfrObject)
  private
    Parent: TfrPage;
    View: TfrView;
    Flags: Word;
    Next, Prev: TfrBand;
    SubIndex, MaxY: Integer;
    EOFReached: Boolean;
    EOFArr: Array[0..31] of Boolean;
    Positions: Array[TfrDatasetPosition] of Integer;
    LastGroupValue: Variant;
    HeaderBand, FooterBand, LastBand: TfrBand;
    Values: TStringList;
    Count: Integer;
    DisableInit: Boolean;
    CalculatedHeight: Integer;

    procedure InitDataSet(const Desc: String);
    procedure DoError;
    function CalcHeight: Integer;
    procedure StretchObjects(MaxHeight: Integer);
    procedure UnStretchObjects;
    procedure DrawObject(t: TfrView);
    procedure PrepareSubReports;
    procedure DoSubReports;
    function DrawObjects: Boolean;
    procedure DrawCrossCell(Parnt: TfrBand; CurX: Integer);
    procedure DrawCross;
    function CheckPageBreak(ay, ady: Integer; PBreak: Boolean): Boolean;
    function CheckNextColumn: boolean;
    procedure DrawPageBreak;
    function HasCross: Boolean;
    function DoCalcHeight: Integer;
    procedure DoDraw;
    function Draw: Boolean;
    procedure InitValues;
    procedure DoAggregate;
    procedure ResetLastValues;
  public
    maxdy: Integer;

    Typ: TfrBandType;
    PrintIfSubsetEmpty, NewPageAfter, Stretched, PageBreak: Boolean;
    Objects: TFpList;
    DataSet: TfrDataSet;
    IsVirtualDS: Boolean;
    VCDataSet: TfrDataSet;
    IsVirtualVCDS: Boolean;
    GroupCondition: String;
    ForceNewPage, ForceNewColumn: Boolean;

    constructor Create(ATyp: TfrBandType; AParent: TfrPage); overload;
    destructor Destroy; override;
    function IsDataBand: boolean;
  end;

  TfrValue = class
  public
    Typ       : TfrValueType;
    OtherKind : Integer;     // for vtOther - typ, for vtDBField - format
    DataSet   : String;      // for vtDBField
    Field     : String;      // here is an expression for vtOther
    DSet      : TfrTDataSet;
  end;

  { TfrValues }

  TfrValues = class(TPersistent)
  private
    FItems: TStringList;
    function GetValue(Index: Integer): TfrValue;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function AddValue: Integer;
    function FindVariable(const s: String): TfrValue;
    procedure ReadBinaryData(Stream: TStream);
    procedure ReadBinaryDataFromXML(XML: TLrXMLConfig; const Path: String);
    procedure WriteBinaryData(Stream: TStream);
    procedure WriteBinaryDataToXML(XML: TLrXMLConfig; const Path: String);
    procedure Clear;

    property Items: TStringList read FItems write FItems;
    property Objects[Index: Integer]: TfrValue read GetValue;
  end;

  { TfrPage }

  TfrPage = class(TfrObject)
  private
    Bands             : Array[TfrBandType] of TfrBand;
    fColCount         : Integer;
    fColGap           : Integer;
    fColWidth         : Integer;
    fLastBandType     : TfrBandType;
    fLastRowHeight    : Integer;
    fMargins          : TfrRect;
    fOrientation      : TPrinterOrientation;
    fPrintToPrevPage  : Boolean;
    fRowStarted       : boolean;
    fUseMargins       : Boolean;
    Skip              : Boolean;
    InitFlag          : Boolean;
    CurColumn         : Integer;
    LastStaticColumnY : Integer;
    XAdjust           : Integer;
    List              : TFpList;
    Mode              : TfrPageMode;
    PlayFrom          : Integer;
    LastBand          : TfrBand;
    ColPos            : Integer;
    CurPos            : Integer;
    PageType          : TfrPageType;
    fLayoutOrder      : TLayoutOrder;
    procedure DoAggregate(a: Array of TfrBandType);
    procedure AddRecord(b: TfrBand; rt: TfrBandRecType);
    procedure ClearRecList;
    function PlayRecList: Boolean;
    procedure DrawPageFooters;
    function BandExists(b: TfrBand): Boolean;
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);

    procedure ShowBand(b: TfrBand);
  protected
    procedure InitReport; virtual;
    procedure DoneReport; virtual;
    procedure TossObjects; virtual;
    procedure PrepareObjects; virtual;
    procedure FormPage; virtual;
    procedure AfterPrint; virtual;

  public
    pgSize    : Integer;
    PrnInfo   : TfrPrnInfo;
    Objects   : TFpList;
    RTObjects : TFpList;
    CurY      : Integer;
    CurBottomY: Integer;
    
    constructor Create; override;
    destructor Destroy; override;

    constructor Create(ASize, AWidth, AHeight: Integer; AOr: TPrinterOrientation);
    constructor CreatePage; virtual;
    
    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SavetoXML(XML: TLrXMLConfig; const Path: String); override;

    function TopMargin: Integer;
    function BottomMargin: Integer;
    function LeftMargin: Integer;
    function RightMargin: Integer;
    procedure Clear;
    procedure Delete(Index: Integer);
    function FindObjectByID(ID: Integer): Integer;
    function FindObject(const aName: String): TfrObject;
    function FindRTObject(const aName: String): TfrObject;
    procedure ChangePaper(ASize, AWidth, AHeight: Integer; AOr: TPrinterOrientation);
    procedure ShowBandByName(const s: String);
    procedure ShowBandByType(bt: TfrBandType);
    procedure NewPage;
    procedure NewColumn(Band: TfrBand);
    procedure NextColumn(Band: TFrBand);
    function RowsLayout: boolean;
    procedure StartColumn;
    procedure StartRowsLayoutNonDataBand(Band: TfrBand);
    function  AdvanceRow(Band: TfrBand): boolean;
    
    property ColCount : Integer read fColCount write fColCount;
    property ColWidth : Integer read fColWidth write fColWidth;
    property ColGap   : Integer read fColGap write fColGap;
    property UseMargins : Boolean read fUseMargins write fUseMargins;
    property Margins    : TfrRect read fMargins write fMargins;
    property PrintToPrevPage : Boolean read fPrintToPrevPage write fPrintToPrevPage;
    property Orientation : TPrinterOrientation read fOrientation write fOrientation;
    property LayoutOrder: TLayoutOrder read fLayoutOrder write fLayoutOrder;
    property LastRowHeight: Integer read fLastRowHeight write fLastRowHeight;
    property RowStarted: boolean read fRowStarted write fRowStarted;
    property LastBandType: TfrBandType read fLastBandType write fLastbandType;

  published
    property Script;
    property Height;
    property Width;
  end;

  TFrPageClass = Class of TfrPage;
  
  { TfrPageReport }

  TfrPageReport = Class(TfrPage)
  public
    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SavetoXML(XML: TLrXMLConfig; const Path: String); override;
    
    constructor CreatePage; override;
  published
    property ColCount;
    property ColWidth;
    property ColGap;
    property UseMargins;
    property Margins;
    property PrintToPrevPage;
    property Orientation;
    property LayoutOrder;
  end;

  { TfrPageDialog }

  TfrPageDialog = Class(TfrPage)
  private
    fHasVisibleControls : Boolean;
    fForm               : TfrDialogForm;
    fCaption            : String;
    
  protected
    procedure PrepareObjects; override;
    procedure InitReport; override;
  public
    constructor Create; override;

    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SavetoXML(XML: TLrXMLConfig; const Path: String); override;
  published
    property Caption : string read fCaption write fCaption;
  end;

  { TfrPages }

  TfrPages = class(TObject)
  private
    FPages: TFpList;
    Parent: TfrReport;

    function GetCount: Integer;
    function GetPages(Index: Integer): TfrPage;
  public
    constructor Create(AParent: TfrReport);
    destructor Destroy; override;

    procedure Clear;
    procedure Add(const aClassName : string='TfrPageReport');
    procedure Delete(Index: Integer);
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String);
    procedure SaveToStream(Stream: TStream);
    procedure SavetoXML(XML: TLrXMLConfig; const Path: String);

    property Pages[Index: Integer]: TfrPage read GetPages; default;
    property Count: Integer read GetCount;
  end;

  { TfrEMFPages }

  TfrEMFPages = class(TObject)
  private
    FPages: TFpList;
    Parent: TfrReport;
    function GetCount: Integer;
    function GetPages(Index: Integer): PfrPageInfo;
    procedure ExportData(Index: Integer);
    procedure PageToObjects(Index: Integer);
    procedure ObjectsToPage(Index: Integer);
  public
    constructor Create(AParent: TfrReport);
    destructor Destroy; override;
    procedure Clear;
    procedure Draw(Index: Integer; Canvas: TCanvas; DrawRect: TRect);
    procedure Add(APage: TfrPage);
    procedure Insert(Index: Integer; APage: TfrPage);
    procedure Delete(Index: Integer);
    procedure LoadFromStream(AStream: TStream);
    procedure AddPagesFromStream(AStream: TStream; AReadHeader: boolean=true);
    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String);
    procedure SaveToStream(AStream: TStream);
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String);
    property Pages[Index: Integer]: PfrPageInfo read GetPages; default;
    property Count: Integer read GetCount;
  end;

  { TfrExportFilter }

  TExportFilterSetup = procedure(Sender: TfrExportFilter) of object;

  TfrExportFilter = class(TObject)
  private
    FOnSetup: TExportFilterSetup;
    FBandTypes: TfrBandTypes;
    FUseProgressBar: boolean;
    FLineIndex: Integer;
  protected
    Stream: TStream;
    Lines: TFpList;
    procedure ClearLines;
    procedure Setup; virtual;
    function  AddData(x, y: Integer; view: TfrView): pointer; virtual;
    procedure NewRec(View: TfrView; const AText:string; var P:Pointer); virtual;
    procedure AddRec(ALineIndex: Integer; ARec: Pointer); virtual;
    function  GetviewText(View:TfrView): string; virtual;
    function  CheckView(View:TfrView): boolean; virtual;
  public
    constructor Create(AStream: TStream); virtual;
    destructor Destroy; override;
    procedure OnBeginDoc; virtual;
    procedure OnEndDoc; virtual;
    procedure OnBeginPage; virtual;
    procedure OnEndPage; virtual;
    procedure OnData(x, y: Integer; View: TfrView); virtual;
    procedure OnText(x, y: Integer; const text: String; View: TfrView); virtual;

    property BandTypes: TfrBandTypes read FBandTypes write FBandTypes;
    property UseProgressbar: boolean read FUseProgressBar write FUseProgressBar;
    property OnSetup: TExportFilterSetup read FOnSetup write FOnSetup;
  end;

  TfrExportFilterClass = class of TfrExportFilter;

  TfrDataType = (dtDataSet,dtDataSource);

  { TfrReport }

  TfrReport = class(TComponent)
  private
    FDataType: TfrDataType;
    FOnExportFilterSetup: TExportFilterSetup;
    FPages: TfrPages;
    FEMFPages: TfrEMFPages;
    FReportAutor: string;
    FReportCreateDate: TDateTime;
    FReportLastChange: TDateTime;
    FReportOptions: TfrReportOptions;
    FReportVersionBuild: string;
    FReportVersionMajor: string;
    FReportVersionMinor: string;
    FReportVersionRelease: string;
    FVars: TStrings;
    FVal: TfrValues;
    FDataset: TfrDataset;
    FGrayedButtons: Boolean;
    FReportType: TfrReportType;
    FShowProgress: Boolean;
    FModalPreview: Boolean;
    FModifyPrepared: Boolean;
    FStoreInDFM: Boolean;
    FPreview: TfrPreview;
    FPreviewButtons: TfrPreviewButtons;
    FInitialZoom: TfrPreviewZoom;
    FOnBeginDoc: TBeginDocEvent;
    FOnEndDoc: TEndDocEvent;
    FOnBeginPage: TBeginPageEvent;
    FOnEndPage: TEndPageEvent;
    FOnBeginBand: TBeginBandEvent;
    FOnEndBand: TEndBandEvent;
    FOnGetValue: TDetailEvent;
    FOnEnterRect: TEnterRectEvent;
    FOnProgress: TfrProgressEvent;
    FOnFunction: TFunctionEvent;
    FOnBeginColumn: TBeginColumnEvent;
    FOnPrintColumn: TPrintColumnEvent;
    FOnManualBuild: TManualBuildEvent;
    FCurrentFilter: TfrExportFilter;
    FPageNumbers  : String;
    FCopies       : Integer;
    FCurPage      : TfrPage;
    
//    FDefaultTitle : String;
    FTitle        : String;
    FSubject      : string;
    FKeyWords     : string;
    FComments     : TStringList;
    FDFMStream    : TStream;

    function FormatValue(V: Variant; AFormat: Integer; const AFormatStr: String): String;
//    function GetLRTitle: String;

    procedure OnGetParsFunction(const aName: String; p1, p2, p3: Variant;
                                var val: Variant);
    procedure PrepareDataSets;
    procedure BuildBeforeModal(Sender: TObject);
    procedure ExportBeforeModal(Sender: TObject);
    procedure PrintBeforeModal(Sender: TObject);
    function DoPrepareReport: Boolean;
    procedure DoBuildReport; virtual;
    procedure DoPrintReport(const PageNumbers: String; Copies: Integer);
    procedure SetComments(const AValue: TStringList);
    procedure SetPrinterTo(const PrnName: String);
    procedure SetVars(Value: TStrings);
    procedure ClearAttribs;
    procedure Loaded; override;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadBinaryData(Stream: TStream);
    procedure WriteBinaryData(Stream: TStream);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    CanRebuild                : Boolean;            // true, if report can be rebuilded
    Terminated                : Boolean;
    PrintToDefault, DoublePass: WordBool;
    FinalPass                 : Boolean;
    FileName                  : String;
    ExportFilename            : string;   // filename used when exporting a report

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    // service methods
    function FindVariable(Variable: String): Integer;
    procedure GetVariableValue(const s: String; var aValue: Variant);
    procedure GetVarList(CatNo: Integer; List: TStrings);
    procedure GetCategoryList(List: TStrings);
    function FindObject(const aName: String): TfrObject;
    // internal events used through report building
    procedure InternalOnEnterRect(Memo: TStringList; View: TfrView);
    procedure InternalOnExportData(View: TfrView);
    procedure InternalOnExportText(x, y: Integer; const text: String; View: TfrView);
    procedure InternalOnGetValue(ParName: String; var ParValue: String);
    procedure InternalOnProgress(Percent: Integer);
    procedure InternalOnBeginColumn(Band: TfrBand);
    procedure InternalOnPrintColumn(ColNo: Integer; var ColWidth: Integer);
    procedure FillQueryParams;
    // load/save methods
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String);
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromFile(const FName: String);
    procedure LoadFromXMLFile(const Fname: String);
    procedure LoadFromXMLStream(const Stream: TStream);
    procedure SaveToFile(FName: String);
    procedure SavetoXML(XML: TLrXMLConfig; const Path: String);
    procedure SaveToXMLFile(const FName: String);

    procedure LoadFromDB(Table: TDataSet; DocN: Integer);
    procedure SaveToDB(Table: TDataSet; DocN: Integer);

    procedure LoadTemplate(const fname: String; comm: TStrings;
      Bmp: TBitmap; Load: Boolean);
    procedure SaveTemplate(const fname: String; comm: TStrings; Bmp: TBitmap);
    procedure LoadPreparedReport(const FName: String);
    procedure SavePreparedReport(const FName: String);
    // report manipulation methods
    procedure DesignReport;
    function PrepareReport: Boolean;
    procedure ExportTo(FilterClass: TfrExportFilterClass; const aFileName: String);
    procedure ShowReport;
    procedure ShowPreparedReport;
    procedure PrintPreparedReport(const PageNumbers: String; Copies: Integer);
    function ChangePrinter(OldIndex, NewIndex: Integer): Boolean;
    procedure EditPreparedReport(PageIndex: Integer);
    //
    property Subject : string read FSubject write FSubject;
    property KeyWords : string read FKeyWords write FKeyWords;
    property Comments : TStringList read FComments write SetComments;
    property ReportAutor : string read FReportAutor write FReportAutor;
    property ReportVersionMajor : string read FReportVersionMajor write FReportVersionMajor;
    property ReportVersionMinor : string read FReportVersionMinor write FReportVersionMinor;
    property ReportVersionRelease : string read FReportVersionRelease write FReportVersionRelease;
    property ReportVersionBuild : string read FReportVersionBuild write FReportVersionBuild;
    property ReportCreateDate : TDateTime read FReportCreateDate write FReportCreateDate;
    property ReportLastChange : TDateTime read FReportLastChange write FReportLastChange;
    //
    property Pages: TfrPages read FPages;
    property EMFPages: TfrEMFPages read FEMFPages write FEMFPages;
    property Variables: TStrings read FVars write SetVars;
    property Values: TfrValues read FVal write FVal;
    
  published
    property Dataset: TfrDataset read FDataset write FDataset;
    property GrayedButtons: Boolean read FGrayedButtons write FGrayedButtons default False;
    property InitialZoom: TfrPreviewZoom read FInitialZoom write FInitialZoom;
    property ModalPreview: Boolean read FModalPreview write FModalPreview default True;
    property ModifyPrepared: Boolean read FModifyPrepared write FModifyPrepared default True;
    property Options: TfrReportOptions read FReportOptions write FReportOptions;
    property Preview: TfrPreview read FPreview write FPreview;
    property PreviewButtons: TfrPreviewButtons read FPreviewButtons write FPreviewButtons;
    property ReportType: TfrReportType read FReportType write FReportType default rtSimple;
    property ShowProgress: Boolean read FShowProgress write FShowProgress default True;
    property StoreInDFM: Boolean read FStoreInDFM write FStoreInDFM default False;
    property DataType : TfrDataType read FDataType write FDataType;

    property Title: String read FTitle write FTitle;

    property OnBeginDoc: TBeginDocEvent read FOnBeginDoc write FOnBeginDoc;
    property OnEndDoc: TEndDocEvent read FOnEndDoc write FOnEndDoc;
    property OnBeginPage: TBeginPageEvent read FOnBeginPage write FOnBeginPage;
    property OnEndPage: TEndPageEvent read FOnEndPage write FOnEndPage;
    property OnBeginBand: TBeginBandEvent read FOnBeginBand write FOnBeginBand;
    property OnEndBand: TEndBandEvent read FOnEndBand write FOnEndBand;
    property OnGetValue: TDetailEvent read FOnGetValue write FOnGetValue;
    property OnEnterRect: TEnterRectEvent read FOnEnterRect write FOnEnterRect;
    property OnUserFunction: TFunctionEvent read FOnFunction write FOnFunction;
    property OnProgress: TfrProgressEvent read FOnProgress write FOnProgress;
    property OnBeginColumn: TBeginColumnEvent read FOnBeginColumn write FOnBeginColumn;
    property OnPrintColumn: TPrintColumnEvent read FOnPrintColumn write FOnPrintColumn;
    property OnManualBuild: TManualBuildEvent read FOnManualBuild write FOnManualBuild;
    property OnExportFilterSetup: TExportFilterSetup read FOnExportFilterSetup write FOnExportFilterSetup;
  end;

  TfrCompositeReport = class(TfrReport)
  private
    procedure DoBuildReport; override;
  public
    Reports: TFpList;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TfrReportDesigner = class(TForm)
  public
    Page: TfrPage;
    Modified: Boolean;
    procedure RegisterObject(ButtonBmp: TBitmap; const ButtonHint: String;
      ButtonTag: Integer); virtual; abstract;
    procedure RegisterTool(const MenuCaption: String; ButtonBmp: TBitmap;
      NotifyOnClick: TNotifyEvent); virtual; abstract;
    procedure BeforeChange; virtual; abstract;
    procedure AfterChange; virtual; abstract;
    procedure RedrawPage; virtual; abstract;
    //
    function PointsToUnits(x: Integer): Double;  virtual; abstract;
    function UnitsToPoints(x: Double): Integer;  virtual; abstract;
  end;

  TfrDataManager = class(TObject)
  public
    procedure Clear; virtual; abstract;
    procedure LoadFromStream(Stream: TStream); virtual; abstract;
    procedure LoadFromXML(XML:TLrXMLConfig; const Path: String); virtual; abstract;
    procedure SaveToStream(Stream: TStream); virtual; abstract;
    procedure SaveToXML(XML:TLrXMLConfig; const Path: String); virtual; abstract;
    procedure BeforePreparing; virtual; abstract;
    procedure AfterPreparing; virtual; abstract;
    procedure PrepareDataSet(ds: TfrTDataSet); virtual; abstract;
    function ShowParamsDialog: Boolean; virtual; abstract;
    procedure AfterParamsDialog; virtual; abstract;
  end;

  TfrObjEditorForm = class(TForm)
  public
    procedure ShowEditor(t: TfrView); virtual;
  end;

  TfrFunctionDescription = class(TObject)
    funName:string;
    funGroup:string;
    funDescription:string;
  end;
  
  { TfrFunctionLibrary }

  TfrFunctionLibrary = class(TObject)
  private
    List, Extra: TStringList;
    function GetCount: integer;
    function GetDescription(AIndex: Integer): TfrFunctionDescription;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function OnFunction(const FName: String; p1, p2, p3: Variant;
      var val: Variant): Boolean;
    procedure DoFunction(FNo: Integer; p1, p2, p3: Variant; var val: Variant);
      virtual; abstract;
    procedure UpdateDescriptions; virtual;
    procedure Add(const funName:string; IsExtra:boolean=false);
    procedure AddFunctionDesc(const funName, funGroup, funDescription:string);
    property FunctionCount:integer read GetCount;
    property Description[AIndex:Integer]:TfrFunctionDescription read GetDescription;
  end;

  TfrCompressor = class(TObject)
  public
    Enabled: Boolean;
    procedure Compress(StreamIn, StreamOut: TStream); virtual;
    procedure DeCompress(StreamIn, StreamOut: TStream); virtual;
  end;


function frCreateObject(Typ: Byte; const ClassName: String): TfrView;
procedure frRegisterObject(ClassRef: TFRViewClass; ButtonBmp: TBitmap;
  const ButtonHint: String; EditorForm: TfrObjEditorForm);
procedure frRegisterExportFilter(ClassRef: TfrExportFilterClass;
  const FilterDesc, FilterExt: String);
procedure frRegisterFunctionLibrary(ClassRef: TClass);
procedure frRegisterTool(const MenuCaption: String; ButtonBmp: TBitmap; OnClick: TNotifyEvent);
function GetDefaultDataSet: TfrTDataSet;
procedure SetBit(var w: Word; e: Boolean; m: Integer);
function frGetBandName(BandType: TfrBandType): string;

const
  frCurrentVersion = 25;
    // version 2.5: lazreport: added to binary stream ParentBandType variable
    //                         on TfrView, used to extend export facilities

  frSpecCount = 9;
  frSpecFuncs: Array[0..frSpecCount - 1] of String = ('PAGE#', '',
    'DATE', 'TIME', 'LINE#', 'LINETHROUGH#', 'COLUMN#', 'CURRENT#', 'TOTALPAGES');
  frColors: Array[0..15] of TColor =
    (clWhite, clBlack, clMaroon, clGreen, clOlive, clNavy, clPurple, clTeal,
     clGray, clSilver, clRed, clLime, clYellow, clBlue, clFuchsia, clAqua);
  frAllFrames=[frbLeft, frbTop, frbRight, frbBottom];

  frUnwrapRead: boolean = false; // TODO: remove this for 0.9.28
  
type
  PfrTextRec = ^TfrTextRec;
  TfrTextRec = record
    Next: PfrTextRec;
    X: Integer;
    W: Integer;
    Text: string;
    FontName: String[32];
    FontSize, FontStyle, FontColor, FontCharset, FillColor: Integer;
    Alignment: TAlignment;
    Borders: TfrFrameBorders;
    BorderColor: TColor;
    BorderStyle: TfrFrameStyle;
    BorderWidth: Integer;
    Typ: Byte;
  end;

  TfrAddInObjectInfo = record
    ClassRef: TfrViewClass;
    EditorForm: TfrObjEditorForm;
    ButtonBmp: TBitmap;
    ButtonHint: String;
  end;

  TfrExportFilterInfo = record
    ClassRef: TfrExportFilterClass;
    FilterDesc, FilterExt: String;
  end;

  TfrFunctionInfo = record
    FunctionLibrary: TfrFunctionLibrary;
  end;

  TfrToolsInfo = record
    Caption: String;
    ButtonBmp: TBitmap;
    OnClick: TNotifyEvent;
  end;

var
  frDesigner: TfrReportDesigner;                  // designer reference
  frDataManager: TfrDataManager;                  // data manager reference
  frParser: TfrParser;                            // parser reference
  frInterpretator: TfrInterpretator;              // interpretator reference
  frVariables: TfrVariables;                      // report variables reference
  frCompressor: TfrCompressor;                    // compressor reference
  CurReport: TfrReport;                           // currently proceeded report
  MasterReport: TfrReport;                        // reference to main composite report
  CurView: TfrView;                               // currently proceeded view
  CurBand: TfrBand;                               // currently proceeded band
  CurPage: TfrPage;                               // currently proceeded page
  DocMode: (dmDesigning, dmPrinting);             // current mode
  DisableDrawing: Boolean;
  frAddIns: Array[0..31] of TfrAddInObjectInfo;   // add-in objects
  frAddInsCount: Integer;
  frFilters: Array[0..31] of TfrExportFilterInfo; // export filters
  frFiltersCount: Integer;
  frFunctions: Array[0..31] of TfrFunctionInfo;   // function libraries
  frFunctionsCount: Integer;
  frTools: Array[0..31] of TfrToolsInfo;          // tools
  frToolsCount: Integer;
  PageNo: Integer;                                // current page number in Building mode
  frCharset: 0..255;
  frBandNames: Array[btReportTitle..btNone] of String;
  frSpecArr: Array[0..frSpecCount - 1] of String;
  frDateFormats, frTimeFormats: Array[0..3] of String;
  frVersion: Byte;                       // version of currently loaded report
  SMemo: TStringList;          // temporary memo used during TfrView drawing
  ShowBandTitles: Boolean = True;
  ProcedureInitDesigner : Procedure = nil;
  
(*
  FRE_COMPATIBLEREAD variable added for migrating from older versions 
  of FreeReport and will be removed in next releases as soon as possible.
*)
{$IFDEF FREEREP2217READ}
  FRE_COMPATIBLE_READ: Boolean = False;
{$ENDIF}

implementation

uses
  LR_Fmted, LR_Prntr, LR_Progr, LR_Utils, DateUtils
  {$IFDEF JPEG}, JPEG {$ENDIF};

type

  { TfrStdFunctionLibrary }

  TfrStdFunctionLibrary = class(TfrFunctionLibrary)
  public
    constructor Create; override;
    procedure UpdateDescriptions; override;
    procedure DoFunction(FNo: Integer; p1, p2, p3: Variant; var val: Variant); override;
  end;

  TInterpretator = class(TfrInterpretator)
  public
    procedure GetValue(const Name: String; var Value: Variant); override;
    procedure SetValue(const Name: String; Value: Variant); override;
    procedure DoFunction(const name: String; p1, p2, p3: Variant;
                         var val: Variant); override;
  end;


var
  VHeight: Integer;            // used for height calculation of TfrMemoView
  SBmp: TBitmap;               // small bitmap used by TfrBandView drawing
  TempBmp: TBitmap;            // temporary bitmap used by TfrMemoView
  CurDate, CurTime: TDateTime; // date/time of report starting
  CurValue: Variant;           // used for highlighting
  AggrBand: TfrBand;           // used for aggregate functions
  CurVariable: String;
  IsColumns: Boolean;
  SavedAllPages: Integer;      // number of pages in entire report
  ErrorFlag: Boolean;          // error occured through TfrView drawing
  ErrorStr: String;            // error description
  SubValue: String;            // used in GetValue event handler
  ObjID: Integer = 0;
  BoolStr: Array[0..3] of String;
  HookList: TFpList;
  FRInitialized: Boolean = False;

  // variables used through report building
  PrevY, PrevBottomY, ColumnXAdjust: Integer;
  Append, WasPF: Boolean;
  CompositeMode: Boolean;
  MaxTitleSize: Integer = 0;
  
{$IFDEF DebugLR}
function Bandtyp2str(typ: TfrBandType): string;
begin
  case typ of
    btReportTitle: result := 'btReportTitle';
    btReportSummary: result := 'btReportSummary';
    btPageHeader: result := 'btPageHeader';
    btPageFooter: result := 'btPageFooter';
    btMasterHeader: result := 'btMasterHeader';
    btMasterData: result := 'btMasterData';
    btMasterFooter: result := 'btMasterFooter';
    btDetailHeader: result := 'btDetailHeader';
    btDetailData: result := 'btDetailData';
    btDetailFooter: result := 'btDetailFooter';
    btSubDetailHeader: result := 'btSubDetailHeader';
    btSubDetailData: result := 'btSubDetailData';
    btSubDetailFooter: result := 'btSubDetailFooter';
    btOverlay: result := 'btOverlay';
    btColumnHeader: result := 'btColumnHeader';
    btColumnFooter: result := 'btColumnFooter';
    btGroupHeader: result := 'btGroupHeader';
    btGroupFooter: result := 'btGroupFooter';
    btCrossHeader: result := 'btCrossHeader';
    btCrossData: result := 'btCrossData';
    btCrossFooter: result := 'btCrossFooter';
    btNone: result:='btNone';
  end;

end;

function BandInfo(Band: TfrBand): string;
begin
  result := format('"%s":%s typ=%s',[Band.Name, dbgsname(band), BandTyp2str(Band.typ)]);
end;

function ViewInfo(View: TfrView): string;
begin
  result := format('"%s":%s typ=%s',[View.Name, dbgsname(View), frTypeObjectToStr(View.Typ)]);
end;

function VarStr(V:Variant): string;
begin
  if VarIsNull(v) then
    result := '{null}'
  else
  if VarIsEmpty(v) then
    result := '{empty}'
  else begin
    if VarIsStr(v) then
      result := quotedstr(v)
    else
      result := v;
  end;
end;

{$ENDIF}

procedure UpdateLibraryDescriptions;
var
  i: integer;
begin
  for i:=0 to frFunctionsCount-1 do
    frFunctions[i].FunctionLibrary.UpdateDescriptions;
end;

procedure UpdateObjectStringResources;
begin
  frCharset := StrToInt(sCharset);

  frBandNames[btReportTitle] := sBand1;
  frBandNames[btReportSummary] := sBand2;
  frBandNames[btPageHeader] := sBand3;
  frBandNames[btPageFooter] := sBand4;
  frBandNames[btMasterHeader] := sBand5;
  frBandNames[btMasterData] := sBand6;
  frBandNames[btMasterFooter] := sBand7;
  frBandNames[btDetailHeader] := sBand8;
  frBandNames[btDetailData] := sBand9;
  frBandNames[btDetailFooter] := sBand10;
  frBandNames[btSubDetailHeader] := sBand11;
  frBandNames[btSubDetailData] := sBand12;
  frBandNames[btSubDetailFooter] := sBand13;
  frBandNames[btOverlay] := sBand14;
  frBandNames[btColumnHeader] := sBand15;
  frBandNames[btColumnFooter] := sBand16;
  frBandNames[btGroupHeader] := sBand17;
  frBandNames[btGroupFooter] := sBand18;
  frBandNames[btCrossHeader] := sBand19;
  frBandNames[btCrossData] := sBand20;
  frBandNames[btCrossFooter] := sBand21;
  frBandNames[btNone] := sBand22;

  frSpecArr[0] := sVar1;
  frSpecArr[1] := sVar2;
  frSpecArr[2] := sVar3;
  frSpecArr[3] := sVar4;
  frSpecArr[4] := sVar5;
  frSpecArr[5] := sVar6;
  frSpecArr[6] := sVar7;
  frSpecArr[7] := sVar8;
  frSpecArr[8] := sVar9;

  BoolStr[0] :=SFormat51;
  BoolStr[1] :=SFormat52;
  BoolStr[2] :=SFormat53;
  BoolStr[3] :=SFormat54;

  frDateFormats[0] :=sDateFormat1;
  frDateFormats[1] :=sDateFormat2;
  frDateFormats[2] :=sDateFormat3;
  frDateFormats[3] :=sDateFormat4;

  frTimeFormats[0] :=sTimeFormat1;
  frTimeFormats[1] :=sTimeFormat2;
  frTimeFormats[2] :=sTimeFormat3;
  frTimeFormats[3] :=sTimeFormat4;

  UpdateLibraryDescriptions;
end;

{----------------------------------------------------------------------------}
function frCreateObject(Typ: Byte; const ClassName: String): TfrView;
var
  i: Integer;
begin
  Result := nil;
  case Typ of
    gtMemo:      Result := TfrMemoView.Create;
    gtPicture:   Result := TfrPictureView.Create;
    gtBand:      Result := TfrBandView.Create;
    gtSubReport: Result := TfrSubReportView.Create;
    gtLine:      Result := TfrLineView.Create;
    gtAddIn:
      begin
        for i := 0 to frAddInsCount - 1 do
        begin
          {$IFDEF DebugLR}
          DebugLn('frCreateObject classname compare %s=%s',[frAddIns[i].ClassRef.ClassName,ClassName]);
          {$ENDIF}

          if frAddIns[i].ClassRef.ClassName = ClassName then
          begin
            Result := frAddIns[i].ClassRef.Create;
//            Result.Create;
            Result.Typ := gtAddIn;
            break;
          end;
        end;
        if Result = nil then
          raise EClassNotFound.Create(Format(sClassObjectNotFound,[ClassName]));
      end;
  end;
  
  if Result <> nil then
  begin
    {$IFDEF DebugLR}
    DebugLn('frCreateObject instance classname=%s',[ClassName]);
    {$ENDIF}

    Result.ID := ObjID;
    Inc(ObjID);
  end;
end;

procedure frRegisterObject(ClassRef: TfrViewClass; ButtonBmp: TBitmap;
  const ButtonHint: String; EditorForm: TfrObjEditorForm);
begin
  frAddIns[frAddInsCount].ClassRef := ClassRef;
  frAddIns[frAddInsCount].EditorForm := EditorForm;
  frAddIns[frAddInsCount].ButtonBmp := ButtonBmp;
  frAddIns[frAddInsCount].ButtonHint := ButtonHint;
  if frDesigner <> nil then
    frDesigner.RegisterObject(ButtonBmp, ButtonHint,
      Integer(gtAddIn) + frAddInsCount);
  Inc(frAddInsCount);
end;

procedure frRegisterExportFilter(ClassRef: TfrExportFilterClass;
  const FilterDesc, FilterExt: String);
begin
  frFilters[frFiltersCount].ClassRef := ClassRef;
  frFilters[frFiltersCount].FilterDesc := FilterDesc;
  frFilters[frFiltersCount].FilterExt := FilterExt;
  Inc(frFiltersCount);
end;

procedure frRegisterFunctionLibrary(ClassRef: TClass);
begin
  frFunctions[frFunctionsCount].FunctionLibrary :=
    TfrFunctionLibrary(ClassRef.NewInstance);
  frFunctions[frFunctionsCount].FunctionLibrary.Create;
  Inc(frFunctionsCount);
end;

procedure frRegisterTool(const MenuCaption: String; ButtonBmp: TBitmap; OnClick: TNotifyEvent);
begin
  frTools[frToolsCount].Caption := MenuCaption;
  frTools[frToolsCount].ButtonBmp := ButtonBmp;
  frTools[frToolsCount].OnClick := OnClick;
  if frDesigner <> nil then
    frDesigner.RegisterTool(MenuCaption, ButtonBmp, OnClick);
  Inc(frToolsCount);
end;

function Create90Font(Font: TFont): HFont;
var
  F: TLogFont;
begin
  GetObject(Font.Handle, SizeOf(TLogFont), @F);
  F.lfEscapement := 900;
  F.lfOrientation := 900;
  Result := CreateFontIndirect(F);
end;

function GetDefaultDataSet: TfrTDataSet;
var
  Res: TfrDataset;
begin
  Result := nil; Res := nil;
  if CurBand <> nil then
    case CurBand.Typ of
      btMasterData, btReportSummary, btMasterFooter,
      btGroupHeader, btGroupFooter:
        Res := CurPage.Bands[btMasterData].DataSet;
      btDetailData, btDetailFooter:
        Res := CurPage.Bands[btDetailData].DataSet;
      btSubDetailData, btSubDetailFooter:
        Res := CurPage.Bands[btSubDetailData].DataSet;
      btCrossData, btCrossFooter:
        Res := CurPage.Bands[btCrossData].DataSet;
    end;
  if (Res <> nil) and (Res is TfrDBDataset) then
    Result := TfrDBDataSet(Res).GetDataSet;
end;

function ReadString(Stream: TStream): String;
begin
  if frVersion >= 23 then
{$IFDEF FREEREP2217READ}
      Result := frReadString(Stream) // load in current format
  else
    if (frVersion = 22) and FRE_COMPATIBLE_READ then
      Result := frReadString2217(Stream) // load in bad format
    else
{$ELSE}
    Result := frReadString(Stream) else
{$ENDIF}
    Result := frReadString22(Stream);
end;

procedure ReadMemo(Stream: TStream; Memo: TStrings);
begin
  if frVersion >= 23 then
{$IFDEF FREEREP2217READ}
      frReadMemo(Stream, Memo) // load in current format
  else
    if (frVersion = 22) and FRE_COMPATIBLE_READ then
      Memo.Text := frReadMemoText2217(Stream) // load in bad format
    else
{$ELSE}
    frReadMemo(Stream, Memo) else
{$ENDIF}
    frReadMemo22(Stream, Memo);
end;

procedure CreateDS(const Desc: String; var DataSet: TfrDataSet; var IsVirtualDS: Boolean);
begin
  if (Desc <> '') and (Desc[1] in ['1'..'9']) then
  begin
    DataSet := TfrUserDataSet.Create(nil);
    DataSet.RangeEnd := reCount;
    DataSet.RangeEndCount := StrToInt(Desc);
    IsVirtualDS := True;
  end
  else
    DataSet := frFindComponent(CurReport.Owner, Desc) as TfrDataSet;
  if DataSet <> nil then
    DataSet.Init;
end;

// locale neutral StrToFloatDef
function StringToFloatDef(const S:String; const ADefault:Double): Double;
var
  Code: Integer;
begin
  if S='' then
    Code:=1
  else
    Val(S, Result, Code);
  if Code>0 then
    Result:=ADefault;
end;

procedure SetBit(var w: Word; e: Boolean; m: Integer);
begin
  if e then
    w:=w or m
  else
    w:=w and not m;
end;

function frGetBandName(BandType: TfrBandType): string;
begin
  result := GetEnumName(TypeInfo(TFrBandType), ord(BandType));
  result := copy(result, 3, Length(result));
end;

{----------------------------------------------------------------------------}
constructor TfrView.Create;
begin
  inherited Create;
  Parent := nil;
  Memo1 := TStringList.Create;
  fFrameWidth := 1;
  fFrameColor := clBlack;
  fFillColor := clNone;
  fFormat := 2*256 + Ord(DecimalSeparator);
  BaseName := 'View';
  Visible := True;
  StreamMode := smDesigning;
  ScaleX := 1;
  ScaleY := 1;
  OffsX := 0;
  OffsY := 0;
  Flags := flStretched;
  
  fFrames:=[]; //No frame
end;

destructor TfrView.Destroy;
begin
  Memo1.Free;
  inherited Destroy;
end;

procedure TfrView.Assign(From: TfrView);
begin
  inherited Assign(From);
  
  fName := From.Name;
  Typ := From.Typ;
  Selected := From.Selected;

  Flags := From.Flags;
  fFrameWidth := From.FrameWidth;
  fFrameColor := From.FrameColor;
  fFrameStyle := From.FrameStyle;
  fFillColor := From.FillColor;
  fFormat := From.Format;
  fFormatStr := From.FormatStr;
  fVisible := From.Visible;
  fFrames:=From.Frames;
end;

procedure TfrView.CalcGaps;
var
  bx, by, bx1, by1, wx1, wx2, wy1, wy2: Integer;
begin
  SaveX := x;
  SaveY := y;
  SaveDX := dx;
  SaveDY := dy;
  SaveFW := FrameWidth;
  if DocMode = dmDesigning then
  begin
    ScaleX := 1;
    ScaleY := 1;
    OffsX := 0;
    OffsY := 0;
  end;

  x := Round(x*ScaleX)+OffsX;
  y := Round(y* ScaleY)+OffsY;
  dx:= Round(dx*ScaleX);
  dy:= Round(dy*ScaleY);

  wx1 := Round((FrameWidth * ScaleX - 1) / 2);
  wx2 := Round(FrameWidth * ScaleX / 2);
  wy1 := Round((FrameWidth * ScaleY - 1) / 2);
  wy2 := Round(FrameWidth * ScaleY / 2);
  {$IFDEF DebugLR}
  DebugLn('CalcGaps: dx=%d ScaleX=%f',[dx,ScaleX]);
  {$ENDIF}
  fFrameWidth := FrameWidth * ScaleX;
  gapx := wx2 + 2;
  gapy := wy2 div 2 + 1;
  bx := x;
  by := y;
  bx1 := Round((SaveX + SaveDX) * ScaleX + OffsX);
  by1 := Round((SaveY + SaveDY) * ScaleY + OffsY);
  
  if frbTop in Frames    then Dec(bx1, wx2);
  if frbLeft in Frames   then Dec(by1, wy2);
  if frbBottom in Frames then Inc(bx, wx1);
  if frbRight in Frames  then Inc(by, wy1);
  DRect := Rect(bx, by, bx1 + 1, by1 + 1);
end;

procedure TfrView.RestoreCoord;
begin
  x  := SaveX;
  y  := SaveY;
  dx := SaveDX;
  dy := SaveDY;
  fFrameWidth := SaveFW;
end;

procedure TfrView.ShowBackground;
var
  fp: TColor;
begin
  if DisableDrawing then Exit;
  if (DocMode = dmPrinting) and (FillColor = clNone) then Exit;
  fp := FillColor;
  if (DocMode = dmDesigning) and (fp = clNone) then
    fp := clWhite;
  Canvas.Brush.Bitmap := nil;
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := fp;
  if DocMode = dmDesigning then
    Canvas.FillRect(DRect)
  else
    Canvas.FillRect(Rect(x, y,
   //use calculating coords instead of dx, dy - for best view
   Round((SaveX + SaveDX) * ScaleX + OffsX), Round((SaveY + SaveDY) * ScaleY + OffsY)));
end;

procedure TfrView.ShowFrame;
var
  x1, y1: Integer;

  procedure IntLine(X11, Y11, DX11, DY11: Integer);
  begin
    Canvas.MoveTo(X11, Y11);
    Canvas.LineTo(X11+DX11, Y11+Dy11);
  end;
  
  procedure Line1(x, y, x1, y1: Integer);
  var
    i, w: Integer;
  begin
    {$IFDEF DebugLR}
    DebugLn('Line1(',InttoStr(x),',',IntToStr(y),',',IntToStr(x1),',',IntToStr(y1),')');
    {$ENDIF}

    if Canvas.Pen.Style = psSolid then
    begin
      if FrameStyle<>frsDouble then
      begin
        Canvas.MoveTo(x, y);
        Canvas.LineTo(x1, y1);
      end
      else
      begin
        if x = x1 then
        begin
          Canvas.MoveTo(x - Round(FrameWidth), y);
          Canvas.LineTo(x1 - Round(FrameWidth), y1);
          Canvas.Pen.Color := FillColor;
          Canvas.MoveTo(x, y);
          Canvas.LineTo(x1, y1);
          Canvas.Pen.Color := FrameColor;
          Canvas.MoveTo(x + Round(FrameWidth), y);
          Canvas.LineTo(x1 + Round(FrameWidth), y1);
        end
        else
        begin
          Canvas.MoveTo(x, y - Round(FrameWidth));
          Canvas.LineTo(x1, y1 - Round(FrameWidth));
          Canvas.Pen.Color := FillColor;
          Canvas.MoveTo(x, y);
          Canvas.LineTo(x1, y1);
          Canvas.Pen.Color := FrameColor;
          Canvas.MoveTo(x, y + Round(FrameWidth));
          Canvas.LineTo(x1, y1 + Round(FrameWidth));
        end;
      end
    end
    else
    begin
      Canvas.Brush.Color:=FillColor;
      w := Canvas.Pen.Width;
      Canvas.Pen.Width := 1;
      if x = x1 then
      begin
        for i := 0 to w - 1 do
        begin
          Canvas.MoveTo(x - w div 2 + i, y);
          Canvas.LineTo(x - w div 2 + i, y1);
        end
      end
      else
      begin
        for i := 0 to w - 1 do
        begin
          Canvas.MoveTo(x, y - w div 2 + i);
          Canvas.LineTo(x1, y - w div 2 + i);
        end;
      end;
      Canvas.Pen.Width := w;
    end;
  end;
begin
  if DisableDrawing then Exit;
  if (DocMode = dmPrinting) and (Frames=[]) then Exit;

  with Canvas do
  begin
    Brush.Style:= bsClear;
    Pen.Style:=psSolid;
    if (dx>0) and (dy>0) and (DocMode = dmDesigning) then
    begin
      Pen.Color := clBlack;
      Pen.Width := 1;
      IntLine(x,y+3,0,-3);
      IntLine(x,y, 4, 0);
      IntLine(x,y+dy-3, 0, 3);
      IntLine(x,y+dy, 4, 0);
      IntLine(x+dx-3,y,3,0);
      IntLine(x+dx,y,0,4);
      IntLine(x+dx-3,y+dy,3,0);
      IntLine(x+dx,y+dy,0,-4);
    end;

    Pen.Color := FrameColor;
    Pen.Width := Round(FrameWidth);
    if FrameStyle<>frsDouble then
      Pen.Style := TPenStyle(FrameStyle);

    // use calculating coords instead of dx, dy - for best view
    x1 := Round((SaveX + SaveDX) * ScaleX + OffsX);
    y1 := Round((SaveY + SaveDY) * ScaleY + OffsY);
    
    { // todo: Frame is not implemented in Win32
    if ((frbTop in Frames) and (frbLeft in Frames) and
        (frbBottom   in Frames) and (frbRight  in Frames)) and (FrameStyle=frsSolid) then
          Frame(x,y, x1 + 1, y1 + 1)
    else
    }
    begin
      if (frbRight in Frames) then Line1(x1, y, x1, y1);
      if (frbLeft   in Frames) then Line1(x, y, x, y1);
      if (frbBottom in Frames) then Line1(x, y1, x1, y1);
      if (frbTop  in Frames) then Line1(x, y, x1, y);
    end;
    
    Brush.Style := bsSolid;
  end;
end;

procedure TfrView.BeginDraw(ACanvas: TCanvas);
begin
  fCanvas := ACanvas;
  CurView := Self;
end;

procedure TfrView.Print(Stream: TStream);
begin
  {$IFDEF DebugLR}
  DebugLn('%s.TfrView.Print()',[name]);
  {$ENDIF}
  BeginDraw(Canvas);
  Memo1.Assign(Memo);
  CurReport.InternalOnEnterRect(Memo1, Self);
  frInterpretator.DoScript(Script);
  if not Visible then Exit;

  Stream.Write(Typ, 1);
  if Typ = gtAddIn then
    frWriteString(Stream, ClassName);
  SaveToStream(Stream);
  {$IFDEF DebugLR}
  DebugLn('%s.TfrView.Print() end',[name]);
  {$ENDIF}
end;

procedure TfrView.ExportData;
begin
  CurReport.InternalOnExportData(Self);
end;

procedure TfrView.LoadFromStream(Stream: TStream);
var
  wb : Word;
  S  : Single;
  i  : Integer;
begin
  {$IFDEF DebugLR}
  DebugLn('%s.TfrView.LoadFromStream begin StreamMode=%d ClassName=%s',
    [name,Ord(StreamMode),ClassName]);
  {$ENDIF}
  with Stream do
  begin
    if StreamMode = smDesigning then
    begin
      if frVersion >= 23 then
        Name := ReadString(Stream)
      else
        CreateUniqueName;
    end;
    
    //Read(x, 18); // this is equal to, but much faster:
    Read(x, 4);
    Read(y, 4);
    Read(dx, 4);
    Read(dy, 4);
    Read(Flags, 2);

    Read(S, SizeOf(S)); fFrameWidth := S;
    Read(fFrameColor, SizeOf(fFrameColor));
    Read(fFrames,SizeOf(fFrames));
    Read(fFrameStyle, SizeOf(fFrameStyle));

    Read(fFillColor, 4);

    if StreamMode = smDesigning then
    begin
      Read(fFormat, 4);
      fFormatStr := ReadString(Stream);
    end;
    ReadMemo(Stream, Memo);

    if (frVersion >= 23) and (StreamMode = smDesigning) then
    begin
      ReadMemo(Stream, Script);
      Read(wb,2);
      Visible:=(Wb<>0);
    end;

    if (frVersion >= 25) then begin
      Read(I, 4);
      ParentBandType := TfrBandType(I);
    end;

  end;
  {$IFDEF DebugLR}
  DebugLn('%s.TfrView.LoadFromStream end',[name]);
  {$ENDIF}
end;

procedure TfrView.LoadFromXML(XML: TLrXMLConfig; const Path: String);
var
  S:string;
begin
  inherited LoadFromXML(XML,Path);
  StreamMode := TfrStreamMode(XML.GetValue(Path+'StreamMode/Value', 0)); // TODO Check default
  if StreamMode = smDesigning then
  begin
    if frVersion >= 23 then
      Name := XML.GetValue(Path+'Name/Value', 'checkthis!') // TODO Check default
    else
      CreateUniqueName;
  end;

  x  := XML.GetValue(Path + 'Size/Left/Value', 0);
  y  := XML.GetValue(Path + 'Size/Top/Value', 0);
  dx := XML.GetValue(Path + 'Size/Width/Value', 100);
  dy := XML.GetValue(Path + 'Size/Height/Value', 100);
  Flags := Word(XML.GetValue(Path + 'Flags/Value', 0)); // TODO Check default

  FFrameWidth := StringToFloatDef(XML.GetValue(Path+'Frames/FrameWidth/Value', ''), 1.0);
  FFramecolor := StringToColor(XML.GetValue(Path+'Frames/FrameColor/Value', 'clBlack')); // TODO Check default

  S:=XML.GetValue(Path+'Frames/FrameBorders/Value','');
  if S<>'' then
    RestoreProperty('Frames',S);

  S:=XML.GetValue(Path+'Frames/FrameStyle/Value','');
  if S<>'' then
    RestoreProperty('FrameStyle',S);

  FFillColor := StringToColor(XML.GetValue(Path+'FillColor/Value', 'clWindow')); // TODO Check default
  if StreamMode = smDesigning then
  begin
    fFormat     := XML.GetValue(Path+'Data/Format/Value', Format); // TODO Check default
    fFormatStr  := XML.GetValue(Path+'Data/FormatStr/Value', FormatStr);
    Memo.Text  := XML.GetValue(Path+'Data/Memo/Value', '');   // TODO Check default
    Script.Text:= XML.GetValue(Path+'Data/Script/Value', '');   // TODO Check default
  end
    else
      memo1.text := XML.GetValue(Path+'Data/Memo1/Value', ''); // TODO Check default
end;

procedure TfrView.SaveToStream(Stream: TStream);
var
  S: Single;
  B: Integer;
begin
  {$IFDEF DebugLR}
  DebugLn('%s.SaveToStream begin',[name]);
  {$ENDIF}

  with Stream do
  begin
    if StreamMode = smDesigning then
      frWriteString(Stream, Name);
//    Write(x, 18); // this is equal to, but much faster:
    Write(x, 4);
    Write(y, 4);
    Write(dx, 4);
    Write(dy, 4);
    Write(Flags, 2);

    S := fFrameWidth; Write(s,SizeOf(s));
    Write(fFrameColor, SizeOf(fFrameColor));
    Write(fFrames,SizeOf(fFrames));
    Write(fFrameStyle, SizeOf(fFrameStyle));

    Write(fFillColor, 4);

    if StreamMode = smDesigning then
    begin
      Write(fFormat, 4);
      frWriteString(Stream, fFormatStr);
      frWriteMemo(Stream, Memo);
      frWriteMemo(Stream, Script);
      Write(Visible, 2);
    end
    else
      frWriteMemo(Stream, Memo1);

    // parent band type new in stream format 25
    B := 0;
    if Parent<>nil then
      B := ord(Parent.Typ);
    Write(B, 4);

  end;
  {$IFDEF DebugLR}
  Debugln('%s.SaveToStream end',[name]);
  {$ENDIF}
end;

procedure TfrView.SaveToXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited SaveToXML(XML,Path);
  XML.SetValue(Path+'Typ/Value', frTypeObjectToStr(Typ));
  XML.SetValue(Path+'StreamMode/Value', Ord(StreamMode)); //todo: use symbolic valuess
  XML.SetValue(Path+'Size/Left/Value', x);
  XML.SetValue(Path+'Size/Top/Value', y);
  XML.SetValue(Path+'Size/Width/Value', dx);
  XML.SetValue(Path+'Size/Height/Value', dy);
  XML.SetValue(Path+'Flags/Value', flags);
  
  if IsPublishedProp(self,'FillColor') then
    XML.SetValue(Path+'FillColor/Value', GetSaveProperty('FillColor'));

  if IsPublishedProp(self,'FrameColor') then
    XML.SetValue(Path+'Frames/FrameColor/Value', GetSaveProperty('FrameColor'));

  if IsPublishedProp(self,'FrameStyle') then
    XML.SetValue(Path+'Frames/FrameStyle/Value', GetSaveProperty('FrameStyle'));

  if IsPublishedProp(self,'FrameWidth') then
    XML.SetValue(Path+'Frames/FrameWidth/Value', GetSaveProperty('FrameWidth'));

  if IsPublishedProp(self,'Frames') then
    XML.SetValue(Path+'Frames/FrameBorders/Value', GetSaveProperty('Frames'));

  if StreamMode = smDesigning then
  begin
    if IsPublishedProp(self,'Format') then
      XML.SetValue(Path+'Data/Format/Value', Format);
    if IsPublishedProp(self,'FormatStr') then
       XML.SetValue(Path+'Data/FormatStr/Value', FormatStr);
    if IsPublishedProp(self,'Memo') then
      XML.SetValue(Path+'Data/Memo/Value', TStrings(Memo).Text);
    if IsPublishedProp(self,'Script') then
      XML.SetValue(Path+'Data/Script/Value', TStrings(Script).Text);

  end
    else
      XML.SetValue(Path+'Data/Memo1/Value', Memo1.Text);
end;

procedure TfrView.Resized;
begin
end;

procedure TfrView.GetBlob(b: TfrTField);
begin
  if b=nil then;
end;

procedure TfrView.OnHook(View: TfrView);
begin
  if view=nil then;
end;

procedure TfrView.BeforeChange;
begin
  if (frDesigner<>nil) and (fUpdate=0) then
    frDesigner.BeforeChange;
end;

procedure TfrView.AfterChange;
begin
  if (frDesigner<>nil) and (fUpdate=0) then
    frDesigner.AfterChange;
end;

procedure TfrView.ResetLastValue;
begin
  // to be overriden in TfrMemoView
end;

function TfrView.GetClipRgn(rt: TfrRgnType): HRGN;
var
  bx, by, bx1, by1, w1, w2: Integer;
begin
  if FrameStyle=frsDouble then
  begin
    w1 := Round(FrameWidth * 1.5);
    w2 := Round((FrameWidth - 1) / 2 + FrameWidth);
  end
  else
  begin
    w1 := Round(FrameWidth / 2);
    w2 := Round((FrameWidth - 1) / 2);
  end;
  bx:=x;
  by:=y;
  bx1:=x+dx+1;
  by1:=y+dy+1;
  
  if (frbTop  in Frames) then Inc(bx1, w2);
  if (frbLeft  in Frames) then Inc(by1, w2);
  if (frbBottom  in Frames) then Dec(bx, w1);
  if (frbRight  in Frames) then Dec(by, w1);
  if rt = rtNormal then
    Result := CreateRectRgn(bx, by, bx1, by1)
  else
    Result := CreateRectRgn(bx - 10, by - 10, bx1 + 10, by1 + 10);
end;

procedure TfrView.SetBounds(aLeft, aTop, aWidth, aHeight: Integer);
begin
  Self.x  := aLeft;
  Self.y   := aTop;
  Self.dx := aWidth;
  Self.dy:= aHeight;
end;

function TfrView.PointInView(aX,aY: Integer): Boolean;
Var Rc : TRect;
    bx, by, bx1, by1, w1, w2: Integer;
begin
  if FrameStyle=frsDouble then
  begin
    w1 := Round(FrameWidth * 1.5);
    w2 := Round((FrameWidth - 1) / 2 + FrameWidth);
  end
  else
  begin
    w1 := Round(FrameWidth / 2);
    w2 := Round((FrameWidth - 1) / 2);
  end;
  bx:=x;
  by:=y;
  bx1:=dx+1;
  by1:=dy+1;

  if (frbTop  in Frames) then Inc(bx1, w2);
  if (frbLeft  in Frames) then Inc(by1, w2);
  if (frbBottom in Frames) then Dec(bx, w1);
  if (frbRight  in Frames) then Dec(by, w1);
  Rc:=Bounds(bx, by, bx1, by1);

  Result:=((aX>Rc.Left) and (aX<Rc.Right) and (aY>Rc.Top) and (aY<Rc.Bottom));
end;

procedure TfrView.Invalidate;
begin
  if Assigned(Canvas) and (fUpdate=0) then
    Draw(Canvas);
end;

procedure TfrView.DefinePopupMenu(Popup: TPopupMenu);
var
  m: TMenuItem;
begin
  m := TMenuItem.Create(Popup);
  m.Caption := '-';
  Popup.Items.Add(m);

  m := TMenuItem.Create(Popup);
  m.Caption := sStretched;
  m.OnClick := @P1Click;
  m.Checked := Stretched;
  Popup.Items.Add(m);
end;

procedure TfrView.P1Click(Sender: TObject);
var
  i: Integer;
  t: TfrView;
begin
  frDesigner.BeforeChange;
  with Sender as TMenuItem do
  begin
    Checked := not Checked;
    for i := 0 to frDesigner.Page.Objects.Count-1 do
    begin
      t := TfrView(frDesigner.Page.Objects[i]);
      if t.Selected then
        SetBit(t.Flags, Checked, flStretched);
    end;
  end;
  frDesigner.AfterChange;
end;

function TfrView.GetLeft: Double;
begin
  if frDesigner<>nil then
    result := frDesigner.PointsToUnits(x)
  else
    result := x;
end;

function TfrView.GetStretched: Boolean;
begin
  Result:=((Flags and flStretched)<>0);
end;

function TfrView.GetHeight: Double;
begin
  if frDesigner<>nil then
    result := frDesigner.PointsToUnits(dy)
  else
    result := dy;
end;

function TfrView.GetFrames: TfrFrameBorders;
begin
  result :=  fFrames;
end;

function TfrView.GetTop: Double;
begin
  if frDesigner<>nil then
    result := frDesigner.PointsToUnits(y)
  else
    result := y;
end;

function TfrView.GetWidth: Double;
begin
  if frDesigner<>nil then
    result := frDesigner.PointsToUnits(dx)
  else
    result := dx;
end;

procedure TfrView.SetFillColor(const AValue: TColor);
begin
  if (aValue<>fFillColor) and (fUpdate=0) then
  begin
    BeforeChange;
    fFillColor:=aValue;
    AfterChange;
  end;
end;

procedure TfrView.SetFormat(const AValue: Integer);
begin
  if fFormat<>AValue then
  begin
    BeforeChange;
    fFormat := AValue;
    AfterChange;
  end;
end;

procedure TfrView.SetFormatStr(const AValue: String);
begin
  if fFormatStr<>AValue then
  begin
    BeforeChange;
    fFormatStr := AValue;
    AFterChange;
  end;
end;

procedure TfrView.SetFrameColor(const AValue: TColor);
begin
  if fFramecolor<>AValue then
  begin
    BeforeChange;
    fFrameColor := AValue;
    AfterChange;
  end;
end;

procedure TfrView.SetFrames(const AValue: TfrFrameBorders);
begin
  if (aValue<>fFrames) and (fUpdate=0) then
  begin
    BeforeChange;
    fFrames:=AValue;
    AfterChange;
  end;
end;

procedure TfrView.SetFrameStyle(const AValue: TfrFrameStyle);
begin
  if fFrameStyle<>AValue then
  begin
    BeforeChange;
    fFrameStyle := AValue;
    AfterChange;
  end;
end;

procedure TfrView.SetFrameWidth(const AValue: Double);
begin
  if fFrameWidth<>AValue then
  begin
    BeforeChange;
    fFrameWidth := AValue;
    AfterChange;
  end;
end;

procedure TfrView.SetHeight(const AValue: Double);
begin
  if frDesigner<>nil then begin
    BeforeChange;
    dy := frDesigner.UnitsToPoints(AValue);
    AfterChange;
  end else
    dy := round(Avalue);
end;

procedure TfrView.SetLeft(const AValue: Double);
begin
  if frDesigner<>nil then begin
    BeforeChange;
    x := frDesigner.UnitsToPoints(AValue);
    AfterChange;
  end else
    x := round(AValue);
end;

procedure TfrView.SetStretched(const AValue: Boolean);
begin
  if Stretched<>AValue then
  begin
    BeforeChange;
    SetBit(Flags, AValue, flStretched);
    AfterChange;
  end;
end;

procedure TfrView.SetTop(const AValue: Double);
begin
  if frDesigner<>nil then begin
    BeforeChange;
    y := frDesigner.UnitsToPoints(AValue);
    AfterChange;
  end else
    y := round(AValue);
end;

procedure TfrView.SetWidth(const AValue: Double);
begin
  if frDesigner<>nil then begin
    BeforeChange;
    dx := frDesigner.UnitsToPoints(AValue);
    AfterChange;
  end else
    dx := round(AValue);
end;

{----------------------------------------------------------------------------}
constructor TfrMemoView.Create;
begin
  inherited Create;
  Typ := gtMemo;
  FFont := TFont.Create;
  FFont.Name := 'Arial';
  FFont.Size := 10;
  FFont.Color := clBlack;
  FFont.Charset := frCharset;
  Highlight.FontColor := clBlack;
  Highlight.FillColor := clWhite;
  Highlight.FontStyle := 2; // fsBold
  BaseName := 'Memo';
  Flags := flStretched + flWordWrap;
  LineSpacing := 2;
  CharacterSpacing := 0;
  Adjust := 0;
  fAngle    :=0;
end;

destructor TfrMemoView.Destroy;
begin
  FFont.Free;
  if FLastValue<>nil then
    FLastValue.Free;
  inherited Destroy;
end;

procedure TfrMemoView.SetFont(Value: TFont);
begin
  BeforeChange;
  fFont.Assign(Value);
  AfterChange;
end;

procedure TfrMemoView.SetHideDuplicates(const AValue: Boolean);
begin
  if HideDuplicates<>AValue then
  begin
    BeforeChange;
    SetBit(Flags, AValue, flHideDuplicates);
    AfterChange;
  end;
end;

procedure TfrMemoView.SetIsLastValueSet(const AValue: boolean);
begin
  if AValue then begin
    if FLastValue=nil then
      FLastValue := TStringList.Create;
    FLastValue.Assign(Memo1);
  end else
  if FLastValue<>nil then begin
    FLastValue.Free;
    FLastValue:=nil;
  end;
end;

procedure TfrMemoView.SetLayout(const AValue: TTextLayout);
begin
  if Layout<>AValue then
  begin
    BeforeChange;
    Adjust := (Adjust and %11100111) or (ord(AValue) shl 3);
    AfterChange;
  end;
end;

procedure TfrMemoView.SetWordWrap(const AValue: Boolean);
begin
  if WordWrap<>AValue then
  begin
    BeforeChange;
    SetBit(Flags, AValue, flWordWrap);
    AfterChange;
  end;
end;

procedure TfrMemoView.Assign(From: TfrView);
begin
  inherited Assign(From);
  FFont.Assign(TfrMemoView(From).Font);
  Adjust := TfrMemoView(From).Adjust;
  Highlight := TfrMemoView(From).Highlight;
  HighlightStr := TfrMemoView(From).HighlightStr;
  LineSpacing := TfrMemoView(From).LineSpacing;
end;

procedure TfrMemoView.ExpandVariables;
var
  i: Integer;
  procedure GetData(var s: String);
  var
    i, j: Integer;
    s1, s2: String;
  begin
    i := 1;
    repeat
      while (i < Length(s)) and (s[i] <> '[') do Inc(i);
      s1 := GetBrackedVariable(s, i, j);
      if i <> j then
      begin
        Delete(s, i, j - i + 1);
        s2 := '';
        CurReport.InternalOnGetValue(s1, s2);
        Insert(s2, s, i);
        Inc(i, Length(s2));
        j := 0;
      end;
    until i = j;
  end;
  
var
  s: string;
begin
  Memo1.Clear;
  for i := 0 to Memo.Count - 1 do
  begin
    s := Memo[i];
    if Length(s) > 0 then
    begin
      GetData(s);
      Memo1.Add(s)
    end
    else
      Memo1.Add('');
  end;
end;

procedure TfrMemoView.AssignFont(aCanvas: TCanvas);
begin
  {$IFDEF DebugLR}
  DebugLnEnter('AssignFont (%s) INIT: Self.Font.Size=%d aCanvas.Font.Size=%d',
    [self.Font.Name,Self.Font.Size,ACanvas.Font.Size]);
  {$ENDIF}
  //**    Brush.Style := bsClear;
  aCanvas.Font.Assign(Self.Font);
  if Self.Font.Name='' then
    aCanvas.Font.Name := 'default';
  //Font := Self.Font;
  if not IsPrinting and (ScaleY<>0) then
    ACanvas.Font.Height := -Round(Self.Font.Size * 96 / 72 * ScaleY);
  {$IFDEF DebugLR}
  DebugLnExit('AssignFont (%s) DONE: Self.Font.Size=%d aCanvas.Font.Size=%d',
    [self.Font.Name,Self.Font.Size,ACanvas.Font.Size]);
  {$ENDIF}
end;

type
  TWordBreaks = string;

const
  gl:string='ÀÅ¨ÈÎÓÛÝÞßàåèîóûýþ';
  r_sogl:string='ÚÜúü';

function BreakWord(s: string): TWordBreaks;

  function IsCharIn(i:integer; target:string):boolean;
  begin
    result := Pos(UTF8Copy(s, i, 1), target)>0;
  end;

var
  i,len: Integer;
  IsCh1,IsCh2,CanBreak: Boolean;
begin
  Result := '';
  Len := UTF8Length(s);
  if  Len > 4 then
  begin
    i := 2;
    repeat
      CanBreak := False;
      IsCh1 := IsCharIn(i + 1,gl);
      IsCh2 := IsCharIn(i + 2,gl);
      if IsCharIn(i,gl) then
      begin
        if IsCh1 or IsCh2 then
          CanBreak := True;
      end
      else
      begin
        if not IsCh1 and not IsCharIn(i + 1,r_sogl) and IsCh2 then
          CanBreak := True;
      end;
      if CanBreak then
        Result := Result + Chr(i);
      Inc(i);
    until i > Len - 2;
  end;
  {$IFDEF DebugLR}
  DebugLnEnter('');
  debugLn('breakword: s=%s result=%s',[dbgstr(s),dbgstr(result)]);
  DebugLnExit('');
  {$ENDIF}
end;

procedure TfrMemoView.WrapMemo;
var
  size, size1, maxwidth: Integer;
  b: TWordBreaks;
  WCanvas: TCanvas;
  desc: string;

  procedure OutLine(const str: String);
  var
    n, w: Word;
  begin
    n := Length(str);
    if (n > 0) and (str[n] = #1) then
      w := WCanvas.TextWidth(Copy(str, 1, n - 1)) else
      w := WCanvas.TextWidth(str);
    {$IFDEF DebugLR}
    debugLn('Outline: str="%s" w/=%d w%%=%d',[str,w div 256, w mod 256]);
    {$ENDIF}
    SMemo.Add(str + Chr(w div 256) + Chr(w mod 256));
    Inc(size, size1);
  end;

  procedure WrapLine(const s: String);
  var
    i, cur, beg, last, len: Integer;
    WasBreak, CRLF: Boolean;
    ch: TUTF8char;
  begin

    CRLF := False;
    for i := 1 to Length(s) do
    begin
      if s[i] in [#10, #13] then
      begin
        CRLF := True;
        break;
      end;
    end;

    last := 1; beg := 1;
    if not CRLF and ((Length(s) <= 1) or (WCanvas.TextWidth(s) <= maxwidth)) then
    begin
      OutLine(s + #1)
    end else
    begin

      cur := 1;
      Len := UTF8Desc(S, Desc);

      while cur <= Len do
      begin
        Ch := UTF8Char(s, cur, Desc);

        // check for items with soft-breaks
        if (Ch=#10) or (Ch=#13) then
        begin
          OutLine(UTF8Range(s, beg, cur - beg, Desc) + #1);
          while (cur<Len) and UTF8CharIn(ch, [#10,#13]) do
          begin
            inc(cur);
            if cur<=len then
              ch := UTF8Char(s, cur, desc);
          end;

          beg := cur; last := beg;
          if UTF8CharIn(Ch,[#13, #10]) then
            exit
          else
            continue;
        end;

        if ch <> ' ' then
        if WCanvas.TextWidth(UTF8Range(s, beg, cur - beg + 1, Desc)) > maxwidth then
        begin

          WasBreak := False;
          if (Flags and flWordBreak) <> 0 then
          begin

            // in case of breaking in the middle, get the full word
            i := cur;
            while (i <= Len) and not UTF8CharIn(ch, [' ', '.', ',', '-']) do
            begin
              Inc(i);
              if i<=len then
                ch := UTF8Char(s, i, Desc);
            end;

            // find word's break points using some simple hyphenator algorithm
            // TODO: implement interface so users can use their own hyphenator
            //       algorithm
            b := BreakWord(UTF8Range(s, last, i - last, Desc));

            // if word can be broken in many segments, find the last segment that
            // fits within maxwidth
            if Length(b) > 0 then
            begin
              i := 1;
              while (i <= Length(b)) and
                (WCanvas.TextWidth(UTF8Range(s, beg, last - beg + Ord(b[i]), Desc) + '-') <= maxwidth) do
              begin
                WasBreak := True;
                cur := last + Ord(b[i]);  // cur now points to next char after breaking word
                Inc(i);
              end;
            end;

            // last now points to nex char to be processed
            last := cur;
          end
          else
          begin
            if last = beg then
              last := cur;
          end;

          if WasBreak then
          begin
            // if word has been broken, output the partial word plus an hyphen
            OutLine(UTF8Range(s, beg, last - beg, Desc) + '-');
          end else
          begin
            // output the portion of word that fits maxwidth
            OutLine(UTF8Range(s, beg, last - beg, Desc));
            // if space was found, advance to next no space char
            if s[last] = ' ' then
              last := last + 1;
          end;

          beg := last;
        end;

        if UTF8CharIn(Ch, [' ', '.', ',', '-']) then
          last := cur;
        Inc(cur);
      end;

      if beg <> cur then
        OutLine(UTF8Range(s, beg, cur - beg + 1, Desc) + #1);
    end;
  end;

  procedure OutMemo;
  var
    i: Integer;
  begin
    size := y + gapy;
    size1 := -WCanvas.Font.Height + LineSpacing;
    maxWidth := dx - gapx - gapx;
    {$IFDEF DebugLR}
    DebugLn('OutMemo: Size=%d Size1=%d MaxWidth=%d dx=%d gapx=%d',[Size,Size1,MaxWidth,dx,gapx]);
    {$ENDIF}
    for i := 0 to Memo1.Count - 1 do
    begin
      if (Flags and flWordWrap) <> 0 then
        WrapLine(Memo1[i])
      else
        OutLine(Memo1[i] + #1);
    end;
    VHeight := size - y + gapy;
    TextHeight := size1;
  end;

  procedure OutMemo90;
  var
    i: Integer;
    h, oldh: HFont;
  begin
    h := Create90Font(WCanvas.Font);
    oldh := SelectObject(WCanvas.Handle, h);
    size := x + gapx;
    size1 := -WCanvas.Font.Height + LineSpacing;
    maxwidth := dy - gapy - gapy;
    for i := 0 to Memo1.Count - 1 do
    begin
      if (Flags and flWordWrap) <> 0 then
        WrapLine(Memo1[i])
      else
        OutLine(Memo1[i]);
    end;
    
    SelectObject(WCanvas.Handle, oldh);
    DeleteObject(h);
    VHeight := size - x + gapx;
    TextHeight := size1;
  end;

begin
  WCanvas := TempBmp.Canvas;
  WCanvas.Font.Assign(Font);
  WCanvas.Font.Height := -Round(Font.Size * 96 / 72);
  {$IFDEF DebugLR}
  DebugLnEnter('TfrMemoView.WrapMemo INI Font.PPI=%d Font.Size=%d Canvas.Font.PPI=%d WCanvas.Font.Size=%d',
    [Font.PixelsPerInch, Font.Size,Canvas.Font.PixelsPerInch,WCanvas.Font.Size]);
  {$ENDIF}

  SetTextCharacterExtra(WCanvas.Handle, CharacterSpacing);
  SMemo.Clear;
  if Angle<>0 then
    OutMemo90
  else
    OutMemo;
  {$IFDEF DebugLR}
  DebugLnExit('TfrMemoView.WrapMemo DONE',[]);
  {$ENDIF}
end;

procedure TfrMemoView.ShowMemo;
var
  DR         : TRect;
  SavX,SavY  : Integer;
  
  procedure OutMemo;
  var
    i, cury, th: Integer;

    function OutLine(st: String): Boolean;
    var
      {$IFDEF DebugLR}
      aw: Integer;
      {$ENDIF}
      n, nw, w, curx: Integer;
      ParaEnd: Boolean;
      Ts: TTextStyle;
    begin
      if not Streaming and (cury + th < DR.Bottom) then
      begin
        n := Length(St);
        w := Ord(St[n - 1]) * 256 + Ord(St[n]);
        SetLength(St, n - 2);
        ParaEnd := True;
        if Length(St) > 0 then
        begin
          if St[Length(St)] = #1 then
            SetLength(St, Length(St) - 1)
          else
            ParaEnd := False;
        end;

        // handle any alignment with same code
        Ts := Canvas.TextStyle;
        Ts.Layout    :=tlTop;
        Ts.Alignment :=self.Alignment;
        Ts.Wordbreak :=false;
        Ts.SingleLine:=True;
        Ts.Clipping  :=True;
        Canvas.TextStyle := Ts;
        
        nw := Round(w * ScaleX);                    // needed width
        {$IFDEF DebugLR}
        DebugLn('Canvas.Font.Size=%d TextWidth=%d',[Canvas.Font.Size,Canvas.TextWidth(St)]);
        {$ENDIF}
        (*
        while (Canvas.TextWidth(St) > nw) and (Canvas.Font.Size>1) do
        begin
          Canvas.Font.Size := Canvas.Font.Size-1;
          {$IFDEF DebugLR}
          DebugLn('Rescal font %d',[Canvas.Font.Size]);
          {$ENDIF}
        end;
        *)
        {$IFDEF DebugLR}
        DebugLn('Th=%d Canvas.TextHeight(H)=%d',[Th,Canvas.TextHeight('H')]);
        Debugln('Canvas.Font.Size=%d TextWidth=%d',[Canvas.Font.Size,Canvas.TextWidth(St)]);
        aw := Canvas.TextWidth(St);                // actual width
        DebugLn('nw=%d  aw=%d',[nw,aw]);
        {$ENDIF}
        case Alignment of
          Classes.taLeftJustify : CurX :=x+gapx;
          Classes.taRightJustify: CurX :=x+dx-1-gapx-nw;
          Classes.taCenter      : CurX :=x+gapx+(dx-gapx-gapx-nw) div 2;
        end;

        if not Exporting then
          Canvas.TextRect(DR, CurX, CurY, St)
        else
          CurReport.InternalOnExportText(curx, cury, St, Self);
          
        Inc(CurStrNo);
        Result := False;
      end
      else  Result := True;

      cury := cury + th;
    end;

  begin {OutMemo}
    cury := y + gapy;

    th := -Canvas.Font.Height+Round(LineSpacing * ScaleY);
    {$IFDEF DebugLR}
    DebugLn('Th=%d Canvas.TextHeight(H)=%d DR=%s',[Th,Canvas.TextHeight('H'),dbgs(DR)]);
    {$ENDIF}

    CurStrNo := 0;
    for i := 0 to Memo1.Count - 1 do
      if OutLine(Memo1[i]) then
        break;
  end;

  procedure OutMemo90;
  var
    i, th, curx: Integer;
    h, oldh: HFont;

    procedure OutLine(str: String);
    var
      i, n, cury: Integer;
      ParaEnd: Boolean;
    begin
      SetLength(str, Length(str) - 2);
      if str[Length(str)] = #1 then
      begin
        ParaEnd := True;
        SetLength(str, Length(str) - 1);
      end
      else
        ParaEnd := False;
      cury := 0;
      if Adjust = 4 then
        cury:=y + dy-gapy
      else if Adjust = 5 then
        cury := y + gapy + Canvas.TextWidth(str)
      else if Adjust = 6 then
        cury := y + dy - 1 - gapy - (dy - gapy - gapy - Canvas.TextWidth(str)) div 2
      else if not Exporting then
      begin
        cury := y + dy - gapy;
        n := 0;
        for i := 1 to Length(str) do
          if str[i] = ' ' then Inc(n);
        //**
        {if (n <> 0) and not ParaEnd then
          SetTextJustification(Canvas.Handle,
            dy - gapy - gapy - Canvas.TextWidth(str), n);}
      end;
      if not Exporting then
      begin
       //**
       { ExtTextOut(Canvas.Handle, curx, cury, ETO_CLIPPED, @DR,
          PChar(str), Length(str), nil);
        if Adjust <> 7 then
          SetTextJustification(Canvas.Handle, 0, 0);
        }
      end;
      if Exporting then
        CurReport.InternalOnExportText(curx, cury, str, Self);
      Inc(CurStrNo);
      curx := curx + th;
    end;
  begin {OutMemo90}
    h := Create90Font(Canvas.Font);
    oldh := SelectObject(Canvas.Handle,h);
    curx := x + gapx;
    th := -Canvas.Font.Height + Round(LineSpacing * ScaleY);
    CurStrNo := 0;
    for i := 0 to Memo1.Count - 1 do
      OutLine(Memo1[i]);
    SelectObject(Canvas.Handle, oldh);
    DeleteObject(h);
  end;

begin
  {$IFDEF DebugLR}
  DebugLnEnter('TfrMemoView.ShowMemo INIT Font.Size=%d Canvas.Font.Size=%d',
    [Font.Size, Canvas.Font.Size]);
  {$ENDIF}
  AssignFont(Canvas);
  SavX:=X;
  SavY:=Y;
  Try
    SetTextCharacterExtra(Canvas.Handle, Round(CharacterSpacing * ScaleX));
    DR:=Rect(DRect.Left + 1, DRect.Top, DRect.Right - 2, DRect.Bottom - 1);
    VHeight:=Round(VHeight*ScaleY);
    if Alignment in [Classes.taLeftJustify..Classes.taCenter] then
    begin
      if Layout=tlCenter then
        y:=y+(dy-VHeight) div 2
      else if Layout=tlBottom then
              y:=y+dy-VHeight;
    end;
    
    OutMemo;

  finally
    X:=SavX;
    Y:=SavY;
    {$IFDEF DebugLR}
    DebugLnExit('TfrMemoView.ShowMemo DONE Font.Size=%d Canvas.Font.Size=%d',[Font.Size, Canvas.Font.Size]);
    {$ENDIF}
  end;
  (*
  if (Adjust and $18) <> 0 then
  begin
    ad := Adjust;
    ox := x;
    oy := y;
    Adjust := Adjust and $7;
    if (ad and $4) <> 0 then
    begin
      if (ad and $18) = $8 then
        x := x + (dx - VHeight) div 2
      else if (ad and $18) = $10 then
               x := x + dx - VHeight;
      OutMemo90;
    end
    else
    begin
      if (ad and $18) = $8 then
        y := y + (dy - VHeight) div 2
      else if (ad and $18) = $10 then
        y := y + dy - VHeight;
      OutMemo;
    end;
    Adjust := ad;
    x := ox; y := oy;
  end
  else if (Adjust and $4) <> 0 then
          OutMemo90
       else
         OutMemo;
  *)
end;

function TfrMemoView.CalcWidth(aMemo: TStringList): Integer;
var
  CalcRect: TRect;
  s: String;
  n: Integer;
  DTFlags: Cardinal;
begin
  {$IFDEF DebugLR}
  DebugLnEnter('TfrMemoView.CalcWidth INIT text=%s Font.PPI=%d Font.Size=%d dx=%d dy=%d',
    [aMemo.Text,Font.PixelsPerInch,Font.Size,Dx,dy]);
  {$ENDIF}
  CalcRect := Rect(0, 0, dx, dy);
  Canvas.Font.Assign(Font);
  Canvas.Font.Height := -Round(Font.Size * 96 / 72);
  {$IFDEF DebugLR}
  DebugLn('Canvas.Font.PPI=%d Canvas.Font.Size=%d',[Canvas.Font.PixelsPerInch,Canvas.Font.Size]);
  {$ENDIF}
  DTFlags := DT_CALCRECT;
  if Flags and flWordBreak <> 0 then
    DTFlags := DT_CALCRECT or DT_WORDBREAK;

  s := aMemo.Text;
  n := Length(s);
  if n > 2 then
    if (s[n - 1] = #13) and (s[n] = #10) then
      SetLength(s, n - 2);
  SetTextCharacterExtra(Canvas.Handle, Round(CharacterSpacing * ScaleX));
  DrawText(Canvas.Handle, PChar(s), Length(s), CalcRect, DTFlags);
  Result := CalcRect.Right + Round(2 * FrameWidth) + 2;
  {$IFDEF DebugLR}
  DebugLnExit('TfrMemoView.CalcWidth DONE Width=%d Rect=%s',[Result,dbgs(CalcRect)]);
  {$ENDIF}
end;

procedure TfrMemoView.Draw(aCanvas: TCanvas);
var
  NeedWrap: Boolean;
  newdx: Integer;
  OldScaleX, OldScaleY: Double;
  IsVisible: boolean;
begin
  BeginDraw(aCanvas);
  {$IFDEF DebugLR}
  if IsPrinting then begin
    DebugLn('');
    Debugln('TfrMemoView.Draw: Name=%s Printing=%s Canvas.Font.PPI=%d',
      [Name,dbgs(IsPrinting),Canvas.Font.PixelsPerInch]);
  end;
  NewDx := 0;
  {$ENDIF}
  if ((Flags and flAutoSize) <> 0) and (Memo.Count > 0) and  (DocMode <> dmDesigning) then
  begin
    newdx := CalcWidth(Memo);

    if Alignment=Classes.taRightJustify then
    begin
      x := x + dx - newdx;
      dx := newdx;
    end
    else
      dx := newdx;
  end;
  {$IFDEF DebugLR}
  DebugLn('NewDx=%d Dx=%d',[NewDx,dx]);
  {$ENDIF}
  Streaming := False;
  Memo1.Assign(Memo);

  OldScaleX := ScaleX;
  OldScaleY := ScaleY;
  ScaleX := 1;
  ScaleY := 1;
  CalcGaps;
  ScaleX := OldScaleX;
  ScaleY := OldScaleY;
  RestoreCoord;
  if Memo1.Count > 0 then
  begin
    NeedWrap := Pos(#1, Memo1.Text) = 0;
    {$IFDEF DebugLR}
    DebugLn('Memo1: Count=%d Text=%s NeedWrap=%s', [Memo1.Count,dbgstr(Memo1.text),dbgs(needwrap)]);
    {$ENDIF}
    if Memo1[Memo1.Count - 1] = #1 then
      Memo1.Delete(Memo1.Count - 1);

    if NeedWrap then
    begin
      WrapMemo;
      Memo1.Assign(SMemo);
    end;
  end;

  CalcGaps;

  if Flags and flHideDuplicates <> 0 then
    IsVisible := (flIsDuplicate and Flags = 0)
  else
    IsVisible := true;

  if IsVisible then
  begin
    if not Exporting then ShowBackground;
    if not Exporting then ShowFrame;
    if Memo1.Count > 0 then
      ShowMemo;
  end;

  RestoreCoord;
end;

procedure TfrMemoView.Print(Stream: TStream);
var
  St: String;
  CanExpandVar: Boolean;
  OldFont: TFont;
  OldFill: Integer;
  i: Integer;
begin
  {$IFDEF DebugLR}
  DebugLn('TfrMemoView.Print %s',[Name]);
  {$ENDIF}
  BeginDraw(TempBmp.Canvas);
  Streaming := True;
  if DrawMode = drAll then
    frInterpretator.DoScript(Script);

  CanExpandVar := True;
  if (DrawMode = drAll) and (Assigned(CurReport.OnEnterRect) or
     ((FDataSet <> nil) and frIsBlob(TfrTField(FDataSet.FindField(FField))))) then
  begin
    Memo1.Assign(Memo);
    St:=Memo1.Text;
    CurReport.InternalOnEnterRect(Memo1, Self);
    if St<>Memo1.Text then
       CanExpandVar:= False;
  end
  else if DrawMode = drAfterCalcHeight then
           CanExpandVar := False;
  if DrawMode <> drPart then
    if CanExpandVar then ExpandVariables;

  if HideDuplicates then begin
    if IsLastValueSet then
      SetBit(Flags, FLastValue.Equals(Memo1), flIsDuplicate)
    else
      SetBit(Flags, false, flIsDuplicate);
    IsLastValueSet := True;
  end;

  if not Visible then
  begin
    DrawMode := drAll;
    Exit;
  end;

  OldFont := TFont.Create;
  OldFont.Assign(Font);
  OldFill := FillColor;
  if Length(HighlightStr) <> 0 then
  begin
    if frParser.Calc(HighlightStr) <> 0 then
    begin
      Font.Style:= frSetFontStyle(Highlight.FontStyle);
      Font.Color:= Highlight.FontColor;
      fFillColor := Highlight.FillColor;
    end;
  end;
  
  if (DrawMode = drPart) then
  begin
    CalcGaps;
    Streaming:=False;
    ShowMemo;
    SMemo.Assign(Memo1);
    while Memo1.Count > CurStrNo do
      Memo1.Delete(CurStrNo);
    if (Memo1.Count>0) and (Pos(#1, Memo1.Text) = 0) then
      Memo1.Add(#1);
  end;

  Stream.Write(Typ, 1);
  if Typ = gtAddIn then
    frWriteString(Stream, ClassName);
    
  SaveToStream(Stream);
  
  if DrawMode = drPart then
  begin
    Memo1.Assign(SMemo);
    for i := 0 to CurStrNo - 1 do
      Memo1.Delete(0);
  end;

  Font.Assign(OldFont);
  OldFont.Free;
  fFillColor := OldFill;
  DrawMode := drAll;
end;

procedure TfrMemoView.ExportData;
begin
  inherited;
  Exporting := True;
  Draw(TempBmp.Canvas);
  Exporting := False;
end;

function TfrMemoView.CalcHeight: Integer;
var
  s: String;
  CanExpandVar: Boolean;
  OldFont: TFont;
  OldFill: Integer;
begin
  Result := 0;
  DrawMode := drAfterCalcHeight;
  BeginDraw(TempBmp.Canvas);
  frInterpretator.DoScript(Script);
  if not Visible then Exit;

  CanExpandVar := True;
  Memo1.Assign(Memo);
  s := Memo1.Text;
  CurReport.InternalOnEnterRect(Memo1, Self);
  if s <> Memo1.Text then CanExpandVar := False;
  if CanExpandVar then ExpandVariables;

  OldFont := TFont.Create;
  OldFont.Assign(Font);
  OldFill := FillColor;
  if Length(HighlightStr) <> 0 then
    if frParser.Calc(HighlightStr) <> 0 then
    begin
      Font.Style := frSetFontStyle(Highlight.FontStyle);
      Font.Color := Highlight.FontColor;
      fFillColor := Highlight.FillColor;
    end;
  if ((Flags and flAutoSize) <> 0) and (Memo1.Count > 0) and
     (DocMode <> dmDesigning) then
    dx := CalcWidth(Memo1);

  CalcGaps;
  if Memo1.Count <> 0 then
  begin
    WrapMemo;
    Result := VHeight;
  end;
  Font.Assign(OldFont);
  OldFont.Free;
  fFillColor := OldFill;
end;

function TfrMemoView.MinHeight: Integer;
begin
  Result := TextHeight;
end;

function TfrMemoView.RemainHeight: Integer;
begin
  Result := Memo1.Count * TextHeight;
end;

procedure TfrMemoView.LoadFromStream(Stream: TStream);
var
  w: Word;
  i: Integer;
  TmpAlign: TAlignment;
  TmpLayout: TTextLayout;
begin
  {$IFDEF DebugLR}
  DebugLn('Stream.Position=%d Stream.Size=%d',[Stream.Position,Stream.Size]);
  {$ENDIF}

  inherited LoadFromStream(Stream);
  Font.Name := ReadString(Stream);
  with Stream do
  begin
    Read(i, 4);
    Font.Size := i;
    Read(w, 2);
    Font.Style := frSetFontStyle(w);
    Read(i, 4);
    Font.Color := i;
    Read(w, 2);
    if frVersion < 23 then
      w := frCharset;
    Font.Charset := w;
    if StreamMode = smDesigning then
    begin
      Read(Highlight, 10);
      HighlightStr := ReadString(Stream);
    end;
    

    Read(TmpAlign,SizeOf(TmpAlign));
    Read(TmpLayout,SizeOf(TmpLayout));
    Read(fAngle,SizeOf(fAngle));

    BeginUpdate;
    Alignment := tmpAlign;
    Layout := tmpLayout;
    EndUpdate;
  end;

  if frVersion = 21 then
    Flags := Flags or flWordWrap;
end;

procedure TfrMemoView.LoadFromXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited LoadFromXML(XML, Path);

  Font.Name := XML.GetValue(Path+'Font/Name/Value', 'Arial'); // todo chk
  Font.Size := XML.GetValue(Path+'Font/Size/Value', 10); // todo chk
  RestoreProperty('CharSet',XML.GetValue(Path+'Font/Charset/Value',''),Font);
  RestoreProperty('Style',XML.GetValue(Path+'Font/Style/Value',''),Font);
  Font.Color := StringToColor(XML.GetValue(Path+'Font/Color/Value','clBlack')); // todo chk

  if StreamMode = smDesigning then begin
    Highlight.FontStyle := XML.GetValue(Path+'Highlight/FontStyle/Value', 0); // todo chk
    Highlight.FontColor := StringToColor(XML.GetValue(Path+'Highlight/FontColor/Value', 'clBlack'));
    Highlight.FillColor := StringToColor(XML.GetValue(Path+'Highlight/FillColor/Value', 'clWhite'));
    HighlightStr := XML.GetValue(Path+'Highlight/HighlightStr/Value', HighlightStr);
  end;
  
  RestoreProperty('Alignment',XML.GetValue(Path+'Alignment/Value',''));
  RestoreProperty('Layout',XML.GetValue(Path+'Layout/Value',''));
  fAngle := XML.GetValue(Path+'Angle/Value', 0);
end;

procedure TfrMemoView.SaveToStream(Stream: TStream);
var
  i: Integer;
  w: Word;
  tmpAlign: TAlignment;
  tmpLayout: TTextLayout;
begin
  inherited SaveToStream(Stream);
  frWriteString(Stream, Font.Name);
  with Stream do
  begin
    i := Font.Size;
    Write(i, 4);
    w := frGetFontStyle(Font.Style);
    Write(w, 2);
    i := Font.Color;
    Write(i, 4);
    w := Font.Charset;
    Write(w, 2);
    if StreamMode = smDesigning then
    begin
      Write(Highlight, 10);
      frWriteString(Stream, HighlightStr);
    end;
    
    if (Adjust and %11 = %11) then
      tmpAlign := taLeftJustify
    else
      tmpAlign := Alignment;
    tmpLayout := Layout;
    Write(tmpAlign,SizeOf(tmpAlign));
    Write(tmpLayout,SizeOf(tmpLayout));
    Write(fAngle,SizeOf(fAngle));
  end;
end;

procedure TfrMemoView.SaveToXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited SaveToXML(XML, Path);
  XML.SetValue(Path+'Font/Name/Value', Font.name);
  XML.SetValue(Path+'Font/Size/Value', Font.Size);
  XML.SetValue(Path+'Font/Color/Value', ColorToString(Font.Color));
  XML.SetValue(Path+'Font/Charset/Value', GetSaveProperty('CharSet',Font));
  XML.SetValue(Path+'Font/Style/Value', GetSaveProperty('Style',Font));

  if StreamMode=smDesigning then
  begin
    XML.SetValue(Path+'Highlight/FontStyle/Value', HighLight.FontStyle);
    XML.SetValue(Path+'Highlight/FontColor/Value', ColorToString(Highlight.FontColor));
    XML.SetValue(Path+'Highlight/FillColor/Value', ColorToString(Highlight.FillColor));
    XML.SetValue(Path+'Highlight/HighlightStr/Value', HighlightStr);
  end;
  XML.SetValue(Path+'Alignment/Value',GetSaveProperty('Alignment'));
  XML.SetValue(Path+'Layout/Value', GetSaveProperty('Layout'));
  XML.SetValue(Path+'Angle/Value', FAngle);
end;

procedure TfrMemoView.GetBlob(b: TfrTField);
var
  M: TMemoryStream;
begin
  // todo: TBLobField.AssignTo is not implemented yet
  //       even if I supply a patch for 2.0.4 it will
  //       not be integrated because it's in RC1 now
  //       (I guess)
  //
  //Memo1.Assign(b);
  M := TMemoryStream.Create;
  try
    TBlobField(B).SaveToStream(M);
    M.Position := 0;
    Memo1.LoadFromStream(M);
  finally
    M.Free;
  end;
end;

procedure TfrMemoView.FontChange(sender: TObject);
begin
  AfterChange;
end;

procedure TfrMemoView.ResetLastValue;
begin
  IsLastValueSet := False;
end;

procedure TfrMemoView.DefinePopupMenu(Popup: TPopupMenu);
var
  m: TMenuItem;
begin
  m := TMenuItem.Create(Popup);
  m.Caption := sVarFormat;
  m.OnClick := @P1Click;
  Popup.Items.Add(m);

  m := TMenuItem.Create(Popup);
  m.Caption := sFont;
  m.OnClick := @P4Click;
  Popup.Items.Add(m);
  inherited DefinePopupMenu(Popup);

  m := TMenuItem.Create(Popup);
  m.Caption := sWordWrap;
  m.OnClick := @P2Click;
  m.Checked := WordWrap;
  Popup.Items.Add(m);

  m := TMenuItem.Create(Popup);
  m.Caption := sWordBreak;
  m.OnClick := @P3Click;
  m.Enabled := WordWrap;
  if m.Enabled then
     m.Checked := (Flags and flWordBreak) <> 0;
  Popup.Items.Add(m);

  m := TMenuItem.Create(Popup);
  m.Caption := sAutoSize;
  m.OnClick := @P5Click;
  m.Checked := AutoSize;
  Popup.Items.Add(m);
end;

procedure TfrMemoView.MonitorFontChanges;
begin
  FFont.OnChange:= @FontChange;
end;

procedure TfrMemoView.P1Click(Sender: TObject);
var
  t: TfrView;
  i: Integer;
begin
  BeforeChange;
  frFmtForm := TfrFmtForm.Create(nil);
  try
    with frFmtForm do
    begin
      EdFormat := Self.Format;
      EdFormatStr := Self.FormatStr;
      if ShowModal = mrOk then
      begin
        for i := 0 to frDesigner.Page.Objects.Count - 1 do
        begin
          t := TfrView(frDesigner.Page.Objects[i]);
          if t.Selected then
          begin
            (t as TfrMemoView).Format := EdFormat;
            (t as TfrMemoView).FormatStr := EdFormatStr;
          end;
        end;
      end;
    end;
  finally
    frFmtForm.Free;
    AfterChange
  end;
end;

function TfrMemoView.GetAutoSize: Boolean;
begin
  Result:=((Flags and flAutoSize)<>0);
end;

function TfrMemoView.GetHideDuplicates: Boolean;
begin
  result:=((Flags and flHideDuplicates)<>0);
end;

function TfrMemoView.GetIsLastValueSet: boolean;
begin
  result := FLastValue<>nil;
end;

function TfrMemoView.GetLayout: TTextLayout;
begin
  result := TTextLayout((adjust shr 3) and %11);
end;

function TfrMemoView.GetAlignment: TAlignment;
begin
  Result:=Classes.TAlignment(Adjust and %11);
end;

function TfrMemoView.GetWordWrap: Boolean;
begin
  Result:=((Flags and flWordWrap)<>0);
end;

procedure TfrMemoView.P2Click(Sender: TObject);
var
  i: Integer;
  t: TfrView;
begin
  frDesigner.BeforeChange;
  with Sender as TMenuItem do
  begin
    Checked := not Checked;
    for i := 0 to frDesigner.Page.Objects.Count - 1 do
    begin
      t :=TfrView(frDesigner.Page.Objects[i]);
      if t.Selected then
        SetBit(t.Flags, Checked, flWordWrap);
    end;
  end;
  frDesigner.AfterChange;
end;

procedure TfrMemoView.P3Click(Sender: TObject);
var
  i: Integer;
  t: TfrView;
begin
  frDesigner.BeforeChange;
  with Sender as TMenuItem do
  begin
    Checked := not Checked;
    for i := 0 to frDesigner.Page.Objects.Count - 1 do
    begin
      t :=TfrView(frDesigner.Page.Objects[i]);
      if t.Selected then
        t.Flags := (t.Flags and not flWordBreak) + Word(Checked) * flWordBreak;
    end;
  end;
  frDesigner.AfterChange;
end;

procedure TfrMemoView.P4Click(Sender: TObject);
var
  t: TfrView;
  i: Integer;
  fd: TFontDialog;
begin
  frDesigner.BeforeChange;
  fd := TFontDialog.Create(nil);
  with fd do
  begin
    Font.Assign(Self.Font);
    if Execute then
      for i := 0 to frDesigner.Page.Objects.Count - 1 do
      begin
        t :=TfrView(frDesigner.Page.Objects[i]);
        if t.Selected then
        begin
          if Font.Name <> Self.Font.Name then
            TfrMemoView(t).Font.Name := Font.Name;
          if Font.Size <> Self.Font.Size then
            TfrMemoView(t).Font.Size := Font.Size;
          if Font.Color <> Self.Font.Color then
            TfrMemoView(t).Font.Color := Font.Color;
          if Font.Style <> Self.Font.Style then
            TfrMemoView(t).Font.Style := Font.Style;
          if Font.Charset <> Self.Font.Charset then
            TfrMemoView(t).Font.Charset := Font.Charset;
        end;
      end;
  end;
  fd.Free;
  frDesigner.AfterChange;
end;

procedure TfrMemoView.P5Click(Sender: TObject);
var
  i: Integer;
  t: TfrView;
begin
  frDesigner.BeforeChange;
  with Sender as TMenuItem do
  begin
    Checked := not Checked;
    for i := 0 to frDesigner.Page.Objects.Count - 1 do
    begin
      t :=TfrView(frDesigner.Page.Objects[i]);
      if t.Selected then
        t.Flags := (t.Flags and not flAutoSize) + Word(Checked) * flAutoSize;
    end;
  end;
  frDesigner.AfterChange;
end;

procedure TfrMemoView.SetAlignment(const AValue: TAlignment);
begin
  if Alignment<>AValue then
  begin
    BeforeChange;
    Adjust := (Adjust and not 3) or ord(AValue);
    AfterChange;
  end;
end;

procedure TfrMemoView.SetAutoSize(const AValue: Boolean);
begin
  if AutoSize<>AValue then
  begin
    BeforeChange;
    SetBit(Flags, AValue, flAutoSize);
    AfterChange;
  end;
end;

{----------------------------------------------------------------------------}
constructor TfrBandView.Create;
begin
  inherited Create;
  Typ := gtBand;
  fFormat := 0;
  BaseName := 'Band';
  Flags := flBandOnFirstPage + flBandOnLastPage;
end;

procedure TfrBandView.Assign(From: TfrView);
begin
  inherited Assign(From);
  if From is TfrBandView then
  begin
    BandType := TFrBandView(From).BandType;
    DataSet  := TFrBandView(From).DataSet;
    GroupCondition:=TFrBandView(From).GroupCondition;
  end;
end;

procedure TfrBandView.LoadFromStream(Stream: TStream);
begin
  inherited LoadFromStream(Stream);
  
  With Stream do
  begin
    Read(fBandType,SizeOf(BandType));
    fCondition :=ReadString(Stream);
    fDataSetStr:=ReadString(Stream);
  end;
end;

procedure TfrBandView.LoadFromXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited LoadFromXML(XML, Path);
  RestoreProperty('BandType',XML.GetValue(Path+'BandType/Value','')); // todo chk
  FCondition := XML.GetValue(Path+'Condition/Value', ''); // todo chk
  FDatasetStr := XML.GetValue(Path+'DatasetStr/Value', ''); // todo chk
end;

procedure TfrBandView.SaveToStream(Stream: TStream);
begin
  inherited SaveToStream(Stream);
  
  with Stream do
  begin
    Write(fBandType,SizeOf(fBandType));
    frWriteString(Stream, fCondition);
    frWriteString(Stream, fDataSetStr);
  end;
end;

procedure TfrBandView.SaveToXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited SaveToXML(XML, Path);
  XML.SetValue(Path+'BandType/Value', GetSaveProperty('BandType')); //Ord(FBandType)); // todo: use symbolic values
  XML.SetValue(Path+'Condition/Value', FCondition);
  XML.SetValue(Path+'DatasetStr/Value', FDatasetStr);
end;

procedure TfrBandView.Draw(aCanvas: TCanvas);
var
  h, oldh: HFont;
  St     : String;
  R      : TRect;
begin
  fFrameWidth := 1;
  if BandType in [btCrossHeader..btCrossFooter] then
  begin
    y := 0;
    dy := frDesigner.Page.PrnInfo.Pgh;
  end
  else
  begin
    x := 0;
    dx := frDesigner.Page.PrnInfo.Pgw;
  end;
  BeginDraw(aCanvas);
  CalcGaps;
  with Canvas do
  begin
    //Brush.Bitmap := SBmp;
    Brush.Bitmap := nil;
    Brush.Style := bsSolid;
    Brush.Color:=clBtnFace;
    FillRect(DRect);
    Brush.Color:=clLtGray;
    Brush.Style:=bsDiagCross;
    FillRect(DRect);
    frInitFont(Font,clBlack,8,[]);
    Pen.Width := 1;
    Pen.Color := clBtnFace;
    Pen.Style := psSolid;
    Brush.Style := bsClear;
    Rectangle(x, y, x + dx + 1, y + dy + 1);
    Brush.Color := clBtnFace;
    Brush.Style := bsSolid;
    CalcTitleSize;
    R := GetTitleRect;
    if ShowBandTitles then
    begin
      FillRect(R);
      if BandType in [btCrossHeader..btCrossFooter] then
      begin
        Pen.Color := clBtnShadow;
        MoveTo(r.left, r.Bottom-2); LineTo(r.right, r.Bottom-2);
        Pen.Color := clBlack;
        MoveTo(r.left, r.Bottom-1); LineTo(r.right, r.Bottom-1);
        Pen.Color := clBtnHighlight;
        MoveTo(r.left, r.bottom-1); lineto(r.left, r.top);
        h := Create90Font(Font);
        oldh := SelectObject(Handle, h);
        Brush.Color:=clBtnFace;
        TextOut(r.Left + 3, r.bottom-6, frBandNames[BandType]);
        SelectObject(Handle, oldh);
        DeleteObject(h);
      end
      else
      begin
        Pen.Color := clBtnShadow;
        MoveTo(r.Right-2, r.Top);
        LineTo(r.Right-2, r.Bottom);
        Pen.Color := clBlack;
        MoveTo(r.Right-1, r.Top);
        LineTo(r.Right-1, r.Bottom);
        st:=frBandNames[BandType];
        Brush.Color:=clBtnFace;
        TextOut(r.left+5, r.top+1, frBandNames[BandType]);
      end;
    end
    else
    begin
      Brush.Style := bsClear;
      if BandType in [btCrossHeader..btCrossFooter] then
      begin
        h := Create90Font(Font);
        oldh := SelectObject(Handle, h);
        Brush.Color:=clBtnFace;
        TextOut(x + 2, r.bottom-6, frBandNames[BandType]);
        SelectObject(Handle, oldh);
        DeleteObject(h);
      end
      else
      begin
        Brush.Color:=clBtnFace;
        TextOut(x + 4, y + 2, frBandNames[BandType]);
      end;
    end;
  end;
end;

function TfrBandView.GetClipRgn(rt: TfrRgnType): HRGN;
var
  R,R1,R2: HRGN;
  RR : LongInt;
begin
  if not ShowBandTitles then
  begin
    Result := inherited GetClipRgn(rt);
    Exit;
  end;

  if rt = rtNormal then
    R1 := CreateRectRgn(x, y, x + dx + 1, y + dy + 1)
  else
    R1 := CreateRectRgn(x - 10, y - 10, x + dx + 10, y + dy + 10);

  with GetTitleRect do
    R := CreateRectRgn(Left,Top,Right,Bottom);

  R2:=CreateRectRgn(0,0,0,0);

  RR:=CombineRgn(R2, R, R1, RGN_OR);
  Result:=R2;

  
  DeleteObject(R);
  DeleteObject(R1);
end;

function TfrBandView.PointInView(aX,aY: Integer): Boolean;
var
    Rc : TRect;
begin
  Rc:=Bounds(x, y,dx+1,dy + 1);
  Result:=((aX>Rc.Left) and (aX<Rc.Right) and (aY>Rc.Top) and (aY<Rc.Bottom));
  {$IFDEF DebugLR}
  DebugLn('PointInView, Bounds=%s Point=%d,%d Res=%s',[dbgs(rc),ax,ay,BoolToStr(result)]);
  {$ENDIF}

  if not Result and ShowBandTitles then
  begin
    Rc := GetTitleRect;
    Result := PtInRect(Rc, Point(Ax,Ay));
    {$IFDEF DebugLR}
    DebugLn('PointInView, TitleRect=%s Point=%d,%d Res=%s',[dbgs(rc),ax,ay,BoolToStr(result)]);
    {$ENDIF}
  end;
end;

procedure TfrBandView.DefinePopupMenu(Popup: TPopupMenu);
var
  m: TMenuItem;
begin
  if BandType in [btReportTitle, btReportSummary, btPageHeader, btCrossHeader,
    btMasterHeader..btSubDetailFooter, btGroupHeader, btGroupFooter] then
    inherited DefinePopupMenu(Popup);

  if BandType in [btReportTitle, btReportSummary, btMasterData, btDetailData,
    btSubDetailData, btMasterFooter, btDetailFooter,
    btSubDetailFooter, btGroupHeader] then
  begin
    m := TMenuItem.Create(Popup);
    m.Caption := sFormNewPage;
    m.OnClick := @P1Click;
    m.Checked := (Flags and flBandNewPageAfter) <> 0;
    Popup.Items.Add(m);
  end;

  if BandType in [btMasterData, btDetailData] then
  begin
    m := TMenuItem.Create(Popup);
    m.Caption := sPrintIfSubsetEmpty;
    m.OnClick := @P2Click;
    m.Checked := (Flags and flBandPrintIfSubsetEmpty) <> 0;
    Popup.Items.Add(m);
  end;

  if BandType in [btReportTitle, btReportSummary, btMasterHeader..btSubDetailFooter,
    btGroupHeader, btGroupFooter] then
  begin
    m := TMenuItem.Create(Popup);
    m.Caption := sBreaked;
    m.OnClick := @P3Click;
    m.Checked := (Flags and flBandPageBreak) <> 0;
    Popup.Items.Add(m);
  end;

  if BandType in [btPageHeader, btPageFooter] then
  begin
    m := TMenuItem.Create(Popup);
    m.Caption := sOnFirstPage;
    m.OnClick := @P4Click;
    m.Checked := (Flags and flBandOnFirstPage) <> 0;
    Popup.Items.Add(m);
  end;

  if BandType = btPageFooter then
  begin
    m := TMenuItem.Create(Popup);
    m.Caption := sOnLastPage;
    m.OnClick := @P5Click;
    m.Checked := (Flags and flBandOnLastPage) <> 0;
    Popup.Items.Add(m);
  end;

  if BandType in [btMasterHeader, btDetailHeader, btSubDetailHeader,
    btCrossHeader, btGroupHeader] then
  begin
    m := TMenuItem.Create(Popup);
    m.Caption := sRepeatHeader;
    m.OnClick := @P6Click;
    m.Checked := (Flags and flBandRepeatHeader) <> 0;
    Popup.Items.Add(m);
  end;
end;

procedure TfrBandView.P1Click(Sender: TObject);
var
  i: Integer;
  t: TfrView;
begin
  frDesigner.BeforeChange;
  with Sender as TMenuItem do
  begin
    Checked := not Checked;
    for i := 0 to frDesigner.Page.Objects.Count - 1 do
    begin
      t :=TfrView(frDesigner.Page.Objects[i]);
      if t.Selected then
        t.Flags := (t.Flags and not flBandNewPageAfter) +
          Word(Checked) * flBandNewPageAfter;
    end;
  end;
end;

procedure TfrBandView.P2Click(Sender: TObject);
var
  i: Integer;
  t: TfrView;
begin
  frDesigner.BeforeChange;
  with Sender as TMenuItem do
  begin
    Checked := not Checked;
    for i := 0 to frDesigner.Page.Objects.Count - 1 do
    begin
      t :=TfrView(frDesigner.Page.Objects[i]);
      if t.Selected then
        t.Flags := (t.Flags and not flBandPrintifSubsetEmpty) +
          Word(Checked) * flBandPrintifSubsetEmpty;
    end;
  end;
end;

procedure TfrBandView.P3Click(Sender: TObject);
var
  i: Integer;
  t: TfrView;
begin
  frDesigner.BeforeChange;
  with Sender as TMenuItem do
  begin
    Checked := not Checked;
    for i := 0 to frDesigner.Page.Objects.Count - 1 do
    begin
      t :=TfrView(frDesigner.Page.Objects[i]);
      if t.Selected then
        t.Flags := (t.Flags and not flBandPageBreak) + Word(Checked) * flBandPageBreak;
    end;
  end;
end;

procedure TfrBandView.P4Click(Sender: TObject);
begin
  frDesigner.BeforeChange;
  with Sender as TMenuItem do
  begin
    Checked := not Checked;
    Flags := (Flags and not flBandOnFirstPage) + Word(Checked) * flBandOnFirstPage;
  end;
end;

procedure TfrBandView.P5Click(Sender: TObject);
begin
  frDesigner.BeforeChange;
  with Sender as TMenuItem do
  begin
    Checked := not Checked;
    Flags := (Flags and not flBandOnLastPage) + Word(Checked) * flBandOnLastPage;
  end;
end;

procedure TfrBandView.P6Click(Sender: TObject);
begin
  frDesigner.BeforeChange;
  with Sender as TMenuItem do
  begin
    Checked := not Checked;
    Flags := (Flags and not flBandRepeatHeader) + Word(Checked) * flBandRepeatHeader;
  end;
end;

function TfrBandView.GetTitleRect: TRect;
begin
  if BandType in [btCrossHeader..btCrossFooter] then
    result := rect(x - 18, y, x, y + TitleSize + 10)
  else
    result := rect(x, y-18, x + TitleSize + 10, y);
end;

function TfrBandView.TitleSize: Integer;
begin
  if MaxTitleSize<100 then
    result := 100
  else
    result := MaxTitleSize;
end;

procedure TfrBandView.CalcTitleSize;
var
  Bt: TfrBandType;
  W: Integer;
begin
  if MaxTitleSize=0 then begin
    MaxTitleSize := Canvas.TextWidth('-'); // work around gtk2 first calc is not right
    for bt := btReportTitle to btNone do begin
      W := Canvas.TextWidth(frBandNames[bt]);
      if W>MaxTitleSize then
        MaxTitleSize := W;
    end;
  end;
end;

{----------------------------------------------------------------------------}
constructor TfrSubReportView.Create;
begin
  inherited Create;
  Typ := gtSubReport;
  BaseName := 'SubReport';
end;

procedure TfrSubReportView.Assign(From: TfrView);
begin
  inherited Assign(From);
  SubPage := (From as TfrSubReportView).SubPage;
end;

procedure TfrSubReportView.Draw(aCanvas: TCanvas);
begin
  BeginDraw(aCanvas);
  fFrameWidth := 1;
  CalcGaps;
  with aCanvas do
  begin
    Font.Name := 'Arial';
    Font.Style := [];
    Font.Size := 8;
    Font.Color := clBlack;
    Font.Charset := frCharset;
    Pen.Width := 1;
    Pen.Color := clBlack;
    Pen.Style := psSolid;
    Brush.Color := clWhite;
    Rectangle(x, y, x + dx + 1, y + dy + 1);
    Brush.Style := bsClear;
    TextRect(DRect, x + 2, y + 2, sSubReportOnPage + ' ' +
      IntToStr(SubPage + 1));
  end;
  RestoreCoord;
end;

procedure TfrSubReportView.DefinePopupMenu(Popup: TPopupMenu);
begin
  // no specific items in popup menu
end;

procedure TfrSubReportView.LoadFromStream(Stream: TStream);
begin
  inherited LoadFromStream(Stream);
  Stream.Read(SubPage, 4);
end;

procedure TfrSubReportView.LoadFromXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited LoadFromXML(XML, Path);
  SubPage := XML.GetValue(Path+'SubPage/Value', 0); // todo chk
end;

procedure TfrSubReportView.SaveToStream(Stream: TStream);
begin
  inherited SaveToStream(Stream);
  Stream.Write(SubPage, 4);
end;

procedure TfrSubReportView.SaveToXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited SaveToXML(XML, Path);
  XML.SetValue(Path+'SubPage/Value', SubPage);
end;

{----------------------------------------------------------------------------}
constructor TfrPictureView.Create;
begin
  inherited Create;
  Typ := gtPicture;
  fPicture := TPicture.Create;
  Flags := flStretched + flPictRatio;
  BaseName := 'Picture';
end;

destructor TfrPictureView.Destroy;
begin
  Picture.Free;
  inherited Destroy;
end;

procedure TfrPictureView.Assign(From: TfrView);
begin
  inherited Assign(From);
  Picture.Assign(TfrPictureView(From).Picture);
  FSharedName := TFrPictureView(From).SharedName;
end;

procedure TfrPictureView.Draw(aCanvas: TCanvas);
var
  r: TRect;
  kx, ky: Double;
  w, h, w1, h1: Integer;

  procedure PrintBitmap(DestRect: TRect; Bitmap: TBitmap);
  var
    BitmapHeader: pBitmapInfo;
    BitmapImage: Pointer;
    HeaderSize: DWord;
    ImageSize: DWord;
  begin
    aCanvas.StretchDraw(DestRect, Bitmap);
    //**
    {GetDIBSizes(Bitmap.Handle, HeaderSize, ImageSize);
    GetMem(BitmapHeader, HeaderSize);
    GetMem(BitmapImage, ImageSize);
    try
      GetDIB(Bitmap.Handle, Bitmap.Palette, BitmapHeader^, BitmapImage^);
      StretchDIBits(
        aCanvas.Handle,
        DestRect.Left, DestRect.Top,     // Destination Origin
        DestRect.Right - DestRect.Left,  // Destination Width
        DestRect.Bottom - DestRect.Top,  // Destination Height
        0, 0,                            // Source Origin
        Bitmap.Width, Bitmap.Height,     // Source Width & Height
        BitmapImage,
        TBitmapInfo(BitmapHeader^),
        DIB_RGB_COLORS,
        SRCCOPY)
    finally
      FreeMem(BitmapHeader);
      FreeMem(BitmapImage)
    end;
    }
  end;

begin
  {$IFDEF DebugLR}
  DebugLnEnter('TfrPictureView.Draw INI');
  {$ENDIF}
  BeginDraw(aCanvas);
  CalcGaps;
  with aCanvas do
  begin
    ShowBackground;
    if ((Picture.Graphic = nil) or Picture.Graphic.Empty) and (DocMode = dmDesigning) then
    begin
      Font.Name := 'Arial';
      Font.Size := 8;
      Font.Style := [];
      Font.Color := clBlack;
      Font.Charset := frCharset;
      TextOut(x + 2, y + 2, sPicture);
    end
    else if not ((Picture.Graphic = nil) or Picture.Graphic.Empty) then
    begin
      if (Flags and flStretched) <> 0 then
      begin
        r := DRect;
        if (Flags and flPictRatio) <> 0 then
        begin
          kx := dx / Picture.Width;
          ky := dy / Picture.Height;
          if kx < ky then
            r := Rect(DRect.Left, DRect.Top,
              DRect.Right, DRect.Top + Round(Picture.Height * kx))
          else
            r := Rect(DRect.Left, DRect.Top,
              DRect.Left + Round(Picture.Width * ky), DRect.Bottom);
          w := DRect.Right - DRect.Left;
          h := DRect.Bottom - DRect.Top;
          w1 := r.Right - r.Left;
          h1 := r.Bottom - r.Top;
          if (Flags and flPictCenter) <> 0 then
            OffsetRect(r, (w - w1) div 2, (h - h1) div 2);
        end;

        if IsPrinting and (Picture.Graphic is TBitmap) then
          PrintBitmap(r, Picture.Bitmap)
        else
          StretchDraw(r, Picture.Graphic);
      end
      else
      begin
        r := DRect;
        if (Flags and flPictCenter) <> 0 then
        begin
          w := DRect.Right - DRect.Left;
          h := DRect.Bottom - DRect.Top;
          OffsetRect(r, (w - Picture.Width) div 2, (h - Picture.Height) div 2);
        end;
        Draw(r.Left, r.Top, Picture.Graphic)
      end;
    end;
    ShowFrame;
  end;
  RestoreCoord;
  {$IFDEF DebugLR}
  DebugLnExit('TfrPictureView.Draw DONE');
  {$ENDIF}
end;

const
  pkNone = 0;
  pkBitmap = 1;
//**  pkMetafile = 2;
  pkIcon = 3;
  pkJPEG = 4;
  pkPNG  = 5;
  pkAny  = 255;

procedure StreamToXML(XML: TLrXMLConfig; Path: String; Stream: TStream);
var
  Buf: array[0..1023] of byte;
  S: string;
  i,c: integer;
  procedure WriteBuf(Count: Integer);
  var
    j: Integer;
    St: string[3];
  begin
    for j:=0 to Count-1 do begin
      St := IntToHex(Buf[j], 2);
      Move(St[1], S[C], 2);
      inc(c,2);
    end;
  end;
begin
  XML.SetValue(Path+'Size/Value', Stream.Size);
  SetLength(S, Stream.Size*2);
  c := 1;
  for i:=1 to Stream.Size div SizeOf(Buf) do begin
    Stream.Read(Buf, SizeOf(buf));
    WriteBuf(SizeOf(Buf));
  end;
  i := Stream.Size mod SizeOf(Buf);
  if i>0 then begin
    Stream.Read(Buf, i);
    Writebuf(i);
  end;
  XML.SetValue(Path+'Data/Value', S);
end;

procedure XMLToStream(XML: TLrXMLConfig; Path: String; Stream: TStream);
var
  S: String;
  i,Size,cd: integer;
  B: Byte;
begin
  Size := XML.GetValue(Path+'Size/Value', 0);
  if Size>0 then begin
    S := XML.GetValue(Path+'Data/Value', '');
    if S<>'' then
      for i:=1 to Size do begin
        Val('$'+S[i*2-1]+S[i*2], B, cd);
        Stream.Write(B, 1);
      end;
  end;
end;

procedure TfrPictureView.LoadFromStream(Stream: TStream);
var
  b: Byte;
  n: Integer;
  AGraphicClass: TGraphicClass;
  Graphic: TGraphic;
  Ext: string;
begin
  inherited LoadFromStream(Stream);
  Stream.Read(b, 1);

  if b=pkAny then
    Graphic := ExtensionToGraphic(Stream.ReadAnsiString)
  else
    Graphic := PictureTypeToGraphic(b);

  FSharedName := Stream.ReadAnsiString;

  Stream.Read(n, 4);

  Picture.Graphic := Graphic;
  if Graphic <> nil then
  begin
    Graphic.Free;
    Picture.Graphic.LoadFromStream(Stream);
  end;
  Stream.Seek(n, soFromBeginning);
end;

procedure TfrPictureView.LoadFromXML(XML: TLrXMLConfig; const Path: String);
var
  b: Byte;
  m: TMemoryStream;
  Graphic: TGraphic;
  Ext: string;

  procedure GetPictureStream;
  begin
    M := TMemoryStream.Create;
    try
      XMLToStream(XML, Path+'Picture/', M);
    except
      M.Free;
      M := nil;
    end;
  end;

begin
  inherited LoadFromXML(XML, Path);

  SharedName := XML.GetValue(Path+'Picture/SharedName/Value','');
  b := XML.GetValue(Path+'Picture/Type/Value', pkNone);
  Ext := XML.GetValue(Path+'Picture/Type/Ext', '');

  M := nil;
  if (b=pkAny) and (Ext<>'') then
    Graphic := ExtensionToGraphic(Ext)
  else
  if (b>pkBitmap) and (b<pkAny) then
    Graphic := PictureTypeToGraphic(b)
  else begin
    GetPictureStream;
    Graphic := StreamToGraphic(M);
  end;

  Picture.Graphic := Graphic;
  if Graphic <> nil then
  begin
    Graphic.Free;
    if M=nil then
      GetPictureStream;
    try
      try
        M.Position := 0;
        Picture.Graphic.LoadFromStream(M);
      except
        ShowMessage('Unknown Image Format!');
      end;
    finally
      M.Free;
    end;
  end;
end;

procedure TfrPictureView.SaveToStream(Stream: TStream);
var
  b: Byte;
  n, o: Integer;
  ext: string;
begin
  inherited SaveToStream(Stream);

  b := GetPictureType;
  Stream.Write(b, 1);
  if b<>pkNone then
  begin
    ext := GraphicExtension(TGraphicClass(Picture.Graphic.ClassType));
    Stream.WriteAnsiString(ext);
  end;
  Stream.WriteAnsiString(FSharedName);
  n := Stream.Position;
  Stream.Write(n, 4);
  if b <> pkNone then
    Picture.Graphic.SaveToStream(Stream);
  o := Stream.Position;
  Stream.Seek(n, soFromBeginning);
  Stream.Write(o, 4);
  Stream.Seek(0, soFromEnd);
end;

procedure TfrPictureView.SaveToXML(XML: TLrXMLConfig; const Path: String);
var
  b: Byte;
  n, o: Integer;
  m: TMemoryStream;
begin
  inherited SaveToXML(XML, Path);
  b := GetPictureType;

  XML.SetValue(Path+'Picture/SharedName/Value', SharedName);
  XML.SetValue(Path+'Picture/Type/Value', b);
  if b <> pkNone then
  begin
    XML.SetValue(Path+'Picture/Type/Ext',
                 GraphicExtension(TGraphicClass(Picture.Graphic.ClassType)));
    M := TMemoryStream.Create;
    try
      Picture.Graphic.SaveToStream(M);
      M.Position:=0;
      StreamToXML(XML, Path+'Picture/', M);
    finally
      M.Free;
    end;
  end;
end;

procedure TfrPictureView.GetBlob(b: TfrTField);
var
  s: TStream;
  GraphExt: string;
  gc: TGraphicClass;
  AGraphic: TGraphic;
begin
  if b.IsNull then
    Picture.Assign(nil)
  else begin
    // todo: TBlobField.AssignTo is not implemented yet
    s := TDataset(FDataSet).CreateBlobStream(TField(b),bmRead);
    if s.Size = 0 then
      begin
        Picture.Clear;
        s.Free;
      end
    else
    begin
      try
        GraphExt :=  s.ReadAnsiString;
        gc := GetGraphicClassForFileExtension(GraphExt);
        if assigned(gc) then
        begin
          AGraphic := gc.Create;
          AGraphic.LoadFromStream(s);
          Picture.Assign(AGraphic);
        end;
      finally
        if assigned(AGraphic) then
          AGraphic.Free;
        s.Free;
      end
    end;
  end;
end;

procedure TfrPictureView.DefinePopupMenu(Popup: TPopupMenu);
var
  m: TMenuItem;
begin
  inherited DefinePopupMenu(Popup);
  m := TMenuItem.Create(Popup);
  m.Caption := sPictureCenter;
  m.OnClick := @P1Click;
  m.Checked := (Flags and flPictCenter) <> 0;
  Popup.Items.Add(m);

  m := TMenuItem.Create(Popup);
  m.Caption := sKeepAspectRatio;
  m.OnClick := @P2Click;
  m.Enabled := Stretched;
  if m.Enabled then
    m.Checked := (Flags and flPictRatio) <> 0;
  Popup.Items.Add(m);
end;

procedure TfrPictureView.P1Click(Sender: TObject);
var
  i: Integer;
  t: TfrView;
begin
  frDesigner.BeforeChange;
  with Sender as TMenuItem do
  begin
    Checked := not Checked;
    for i := 0 to frDesigner.Page.Objects.Count - 1 do
    begin
      t := TfrView(frDesigner.Page.Objects[i]);
      if t.Selected then
        t.Flags := (t.Flags and not flPictCenter) + Word(Checked) * flPictCenter;
    end;
  end;
  frDesigner.AfterChange;
end;

procedure TfrPictureView.P2Click(Sender: TObject);
var
  i: Integer;
  t: TfrView;
begin
  frDesigner.BeforeChange;
  with Sender as TMenuItem do
  begin
    Checked := not Checked;
    for i := 0 to frDesigner.Page.Objects.Count - 1 do
    begin
      t :=TfrView(frDesigner.Page.Objects[i]);
      if t.Selected then
        t.Flags := (t.Flags and not flPictRatio) + Word(Checked) * flPictRatio;
    end;
  end;
  frDesigner.AfterChange;
end;

function TfrPictureView.GetPictureType: byte;
begin
  result := pkNone;
  if Picture.Graphic <> nil then
    result := pkAny;
end;

function TfrPictureView.PictureTypeToGraphic(b: Byte): TGraphic;
begin
  result := nil;
  case b of
    pkBitmap:   result := TBitmap.Create;
    pkIcon:     result := TIcon.Create;
    pkJPEG:     result := TJPEGImage.Create;
    pkPNG:      result := TPortableNetworkGraphic.Create;
  end;
end;

function TfrPictureView.ExtensionToGraphic(const Ext: string): TGraphic;
var
  AGraphicClass: TGraphicClass;
begin
  AGraphicClass := GetGraphicClassForFileExtension(Ext);
  if AGraphicClass<>nil then
    result := AGraphicClass.Create
  else
    result := nil;
end;

function TfrPictureView.StreamToGraphic(M: TMemoryStream): TGraphic;

  function ReadString(Len: Integer): string;
  begin
    SetLength(result, Len);
    M.Read(result[1], Len);
  end;

  function TestStreamIsPNG: boolean;
  begin
    result := ReadString(8) = #137'PNG'#13#10#26#10;
    M.Position := 0;
  end;

  function TestStreamIsJPEG: boolean;
  begin
    Result := ReadString(4) = #$FF#$D8#$FF#$E0;
    if result then begin
      M.Position := 6;
      result := ReadString(5) = 'JFIF'#0
    end;
    M.Position := 0;
  end;

begin

  if M=nil then begin
    result := nil;
    exit;
  end;

  M.Position := 0;

  if TestStreamIsBMP(M) then
  begin
    result := PictureTypeToGraphic(pkBitmap);
    exit;
  end;

  if TestStreamIsIcon(M) then begin
    result := PictureTypeToGraphic(pkIcon);
    exit;
  end;

  if TestStreamIsXPM(M) then
  begin
    result := TPixmap.Create;
    exit;
  end;

  if TestStreamIsPNG then
  begin
    result := PictureTypeToGraphic(pkPNG);
    exit;
  end;

  if TestStreamIsJPEG then
  begin
    result := PictureTypeToGraphic(pkJPEG);
    exit;
  end;

  result := nil;
end;

procedure TfrPictureView.SetPicture(const AValue: TPicture);
begin
  BeforeChange;
  fPicture := AValue;
  AfterChange;
end;

function TfrLineView.GetFrames: TfrFrameBorders;
begin
  if dx > dy then
  begin
    dy := 0;
    fFrames:=[frbTop];
  end
  else
  begin
    dx := 0;
    fFrames:=[frbLeft];
  end;
  Result:=fFrames;
end;

{----------------------------------------------------------------------------}
constructor TfrLineView.Create;
begin
  inherited Create;
  Typ := gtLine;
  fFrames:=[frbLeft];
  BaseName := 'Line';
end;

procedure TfrLineView.Draw(aCanvas: TCanvas);
begin
  BeginDraw(aCanvas);
  GetFrames;
  CalcGaps;
  ShowFrame;
  RestoreCoord;
end;

procedure TfrLineView.DefinePopupMenu(Popup: TPopupMenu);
begin
  // no specific items in popup menu
end;

function TfrLineView.GetClipRgn(rt: TfrRgnType): HRGN;
var
  bx, by, bx1, by1, dd: Integer;
begin
  bx := x; by := y; bx1 := x + dx + 1; by1 := y + dy + 1;
  if FrameStyle<>frsDouble then
    dd := Round(FrameWidth / 2)
  else
    dd := Round(FrameWidth * 1.5);
  if Frames=[frbLeft] then
  begin
    Dec(bx, dd);
    Inc(bx1, dd);
  end
  else
  begin
    Dec(by, dd);
    Inc(by1, dd);
  end;
  if rt = rtNormal then
    Result := CreateRectRgn(bx, by, bx1, by1)
  else
    Result := CreateRectRgn(bx - 10, by - 10, bx1 + 10, by1 + 10);
end;

function TfrLineView.PointInView(aX, aY: Integer): Boolean;
var
  bx, by, bx1, by1, w1, w2: Integer;
begin
  if FrameStyle=frsDouble then
    w1 := Round(FrameWidth * 1.5)
  else
    w1 := Round(FrameWidth);

  bx:=x-w1;
  by:=y-w1;
  bx1:=x+dx+w1;
  by1:=y+dy+w1;

  Result:=(ax>=bx) and (ax<=bx1) and (ay>=by) and (ay<=by1);
end;

{----------------------------------------------------------------------------}
constructor TfrBand.Create(ATyp: TfrBandType; AParent: TfrPage);
begin
  inherited Create;
  Typ := ATyp;
  Parent := AParent;
  Objects := TFpList.Create;
  Values := TStringList.Create;
  Next := nil;
  Positions[psLocal] := 1;
  Positions[psGlobal] := 1;
  Visible:=True;
end;

destructor TfrBand.Destroy;
begin
  if Next <> nil then
    Next.Free;
  Objects.Free;
  Values.Free;
  if DataSet <> nil then
    DataSet.Exit;
  if IsVirtualDS then
    DataSet.Free;
  if VCDataSet <> nil then
    VCDataSet.Exit;
  if IsVirtualVCDS then
    VCDataSet.Free;
  inherited Destroy;
end;

function TfrBand.IsDataBand: boolean;
begin
  result := (typ in [btMasterData, btDetailData, btSubDetailData]);
end;

procedure TfrBand.InitDataSet(const Desc: String);
begin
  if Typ = btGroupHeader then
    GroupCondition := Desc
  else
    if Pos(';', Desc) = 0 then
      CreateDS(Desc, DataSet, IsVirtualDS);
  if (Typ = btMasterData) and (Dataset = nil) and
     (CurReport.ReportType = rtSimple) then
    DataSet := CurReport.Dataset;
end;

procedure TfrBand.DoError;
var
  i: Integer;
begin
  ErrorFlag := True;
  ErrorStr := sErrorOccured;
  for i := 0 to CurView.Memo.Count - 1 do
    ErrorStr := ErrorStr + #13 + CurView.Memo[i];
  ErrorStr := ErrorStr + #13 + sDoc + ' ' + CurReport.Name +
    #13 + sBand; // + ' ' + frBandNames[Integer(CurView.Parent.Typ)];
  MasterReport.Terminated := True;
end;

function TfrBand.CalcHeight: Integer;
var
  Bnd: TfrBand;
  DS : TfrDataSet;
  ddx: Integer;
  BM : Pointer;
  
  function SubDoCalcHeight(CheckAll: Boolean): Integer;
  var
    i, h: Integer;
    t: TfrView;
  begin
    CurBand := Self;
    AggrBand := Self;
    Result := dy;
    for i := 0 to Objects.Count - 1 do
    begin
      t :=TfrView(Objects[i]);
      t.olddy := t.dy;
      if t is TfrStretcheable then
        if (t.Parent = Self) or CheckAll then
        begin
          h := TfrStretcheable(t).CalcHeight + t.y;
          if h > Result then
            Result := h;
          if CheckAll then
            TfrStretcheable(t).DrawMode := drAll;
        end
    end;
  end;
begin
  Result := dy;
  if HasCross and (Typ <> btPageFooter) then
  begin
    Parent.ColPos := 1;
    CurReport.InternalOnBeginColumn(Self);
    if Parent.BandExists(Parent.Bands[btCrossData]) then
    begin
      Bnd := Parent.Bands[btCrossData];
      if Bnd.DataSet <> nil then
        DS := Bnd.DataSet
      else
        DS := VCDataSet;

      BM:=DS.GetBookMark;
      DS.DisableControls;
      try
        DS.First;
        while not DS.Eof do
        begin
          ddx := 0;
          CurReport.InternalOnPrintColumn(Parent.ColPos, ddx);
          CalculatedHeight := SubDoCalcHeight(True);
          if CalculatedHeight > Result then
            Result := CalculatedHeight;
          Inc(Parent.ColPos);
          DS.Next;
          if MasterReport.Terminated then break;
        end;
      finally
        DS.GotoBookMark(BM);
        DS.FreeBookMark(BM);
        DS.EnableControls;
      end;
    end;
  end
  else
    Result := SubDoCalcHeight(False);
  CalculatedHeight := Result;
end;

procedure TfrBand.StretchObjects(MaxHeight: Integer);
var
  i: Integer;
  t: TfrView;
begin
  for i := 0 to Objects.Count - 1 do
  begin
    t :=TfrView(Objects[i]);
    if (t is TfrStretcheable) or (t is TfrLineView) then
      if (t.Flags and flStretched) <> 0 then
        t.dy := MaxHeight - t.y;
  end;
end;

procedure TfrBand.UnStretchObjects;
var
  i: Integer;
  t: TfrView;
begin
  for i := 0 to Objects.Count - 1 do
  begin
    t :=TfrView(Objects[i]);
    t.dy := t.olddy;
  end;
end;

procedure TfrBand.DrawObject(t: TfrView);
var
  ox,oy: Integer;
begin
  {$IFDEF DebugLR}
  DebugLnEnter('TfrBand.DrawObject INI t=%s:%s Xadj=%d Margin=%d DiableDrawing=%s',
  [dbgsname(t),t.name,Parent.XAdjust,Parent.LeftMargin,BoolToStr(DisableDrawing,true)]);
  {$ENDIF}
  CurPage := Parent;
  CurBand := Self;
  AggrBand := Self;
  try
    if (t.Parent = Self) and not DisableDrawing then
    begin
      ox := t.x; Inc(t.x, Parent.XAdjust - Parent.LeftMargin);
      oy := t.y; Inc(t.y, y);
      {$IFDEF DebugLR}
      DebugLnEnter('Printing view %s x=%d y=%d dx=%d dy=%d',[ViewInfo(t),t.x,t.y,t.dx,t.dy]);
      {$ENDIF}
      t.Print(MasterReport.EMFPages[PageNo]^.Stream);
      {$IFDEF DebugLR}
      DebugLnExit('');
      {$ENDIF}
      t.x := ox; t.y := oy;
      if (t is TfrMemoView) and
         (TfrMemoView(t).DrawMode in [drAll, drAfterCalcHeight]) then
        Parent.AfterPrint;
    end;
  except
    on exception do DoError;
  end;
  {$IFDEF DebugLR}
  DebugLnExit('TfrBand.DrawObject DONE t=%s:%s',[dbgsname(t),t.name]);
  {$ENDIF}
end;

procedure TfrBand.PrepareSubReports;
var
  i: Integer;
  t: TfrView;
  Page: TfrPage;
begin
  for i := SubIndex to Objects.Count - 1 do
  begin
    t :=TfrView(Objects[i]);
    Page := CurReport.Pages[(t as TfrSubReportView).SubPage];
    Page.Mode := pmBuildList;
    Page.FormPage;
    Page.CurY := y + t.y;
    Page.CurBottomY := Parent.CurBottomY;
    Page.XAdjust := Parent.XAdjust + t.x;
    Page.ColCount := 1;
    Page.PlayFrom := 0;
    EOFArr[i - SubIndex] := False;
  end;
  Parent.LastBand := nil;
end;

procedure TfrBand.DoSubReports;
var
  i: Integer;
  t: TfrView;
  Page: TfrPage;
begin
  repeat
    if not EOFReached then
      for i := SubIndex to Objects.Count - 1 do
      begin
        t :=TfrView(Objects[i]);
        Page := CurReport.Pages[(t as TfrSubReportView).SubPage];
        Page.CurY := Parent.CurY;
        Page.CurBottomY := Parent.CurBottomY;
      end;
    EOFReached := True;
    MaxY := Parent.CurY;
    for i := SubIndex to Objects.Count - 1 do
    begin
      if not EOFArr[i - SubIndex] then
      begin
        t :=TfrView(Objects[i]);
        Page := CurReport.Pages[(t as TfrSubReportView).SubPage];
        if Page.PlayRecList then
          EOFReached := False
        else
        begin
          EOFArr[i - SubIndex] := True;
          if Page.CurY > MaxY then MaxY := Page.CurY;
        end;
      end;
    end;
    
    if not EOFReached then
    begin
      if Parent.Skip then
      begin
        Parent.LastBand := Self;
        Exit;
      end
      else
        Parent.NewPage;
    end;
    
  until EOFReached or MasterReport.Terminated;
  
  for i := SubIndex to Objects.Count - 1 do
  begin
    t :=TfrView(Objects[i]);
    Page := CurReport.Pages[(t as TfrSubReportView).SubPage];
    Page.ClearRecList;
  end;
  Parent.CurY := MaxY;
  Parent.LastBand := nil;
end;

function TfrBand.DrawObjects: Boolean;
var
  i: Integer;
  t: TfrView;
begin
  {$ifdef DebugLR}
  DebugLnEnter('DrawObjects INIT');
  {$endif}
  Result := False;
  for i := 0 to Objects.Count - 1 do
  begin
    t :=TfrView(Objects[i]);
    if t.Typ = gtSubReport then
    begin
      SubIndex := i;
      Result := True;
      PrepareSubReports;
      DoSubReports;
      break;
    end;

    t.Flags:=t.Flags and not (flStartRecord or flEndRecord);
    if i=0 then               t.Flags := t.Flags or flStartRecord;
    if i=Objects.Count-1 then t.Flags := t.Flags or flEndRecord;

    DrawObject(t);
    if MasterReport.Terminated then break;
  end;
  {$ifdef DebugLR}
  DebugLnExit('DrawObjects DONE result=%s',[BoolToStr(result,true)]);
  {$endif}
end;

procedure TfrBand.DrawCrossCell(Parnt: TfrBand; CurX: Integer);
var
  i, sfx, sfy: Integer;
  t: TfrView;
begin
  CurBand := Self;
  CurBand.Positions[psGlobal] := Parnt.Positions[psGlobal];
  CurBand.Positions[psLocal] := Parnt.Positions[psLocal];
  if Typ = btCrossData then
    AggrBand := Parnt;
  try
    for i := 0 to Objects.Count - 1 do
    begin
      t := TfrView(Objects[i]);
      if Parnt.Objects.IndexOf(t) <> -1 then
        if not DisableDrawing then
        begin
          sfx := t.x; Inc(t.x, CurX);
          sfy := t.y; Inc(t.y, Parnt.y);
    	  t.Print(MasterReport.EMFPages[PageNo]^.Stream);
	  if (t is TfrMemoView) and
             (TfrMemoView(t).DrawMode in [drAll, drAfterCalcHeight]) then
            Parent.AfterPrint;
          t.Parent := Self;
          t.x := sfx;
          t.y := sfy;
        end
        else
        begin
          CurView := t;
          frInterpretator.DoScript(t.Script);
        end;
    end;
  except
    on E: exception do DoError; //(E);
  end;
end;

procedure TfrBand.DrawCross;
var
  Bnd       : TfrBand;
  sfpage    : Integer;
  CurX, ddx : Integer;
  DS        : TfrDataSet;
  BM        : Pointer;
  
  procedure CheckColumnPageBreak(ddx: Integer);
  var
    sfy: Integer;
    b: TfrBand;
  begin
    if CurX + ddx > Parent.RightMargin then
    begin
      Inc(ColumnXAdjust, CurX - Parent.LeftMargin);
      CurX := Parent.LeftMargin;
      Inc(PageNo);
      if PageNo >= MasterReport.EMFPages.Count then
      begin
        MasterReport.EMFPages.Add(Parent);
        sfy := Parent.CurY;
        Parent.ShowBand(Parent.Bands[btOverlay]);
        Parent.CurY := Parent.TopMargin;
        if (sfPage <> 0) or
          ((Parent.Bands[btPageHeader].Flags and flBandOnFirstPage) <> 0) then
          Parent.ShowBand(Parent.Bands[btPageHeader]);
        Parent.CurY := sfy;
        CurReport.InternalOnProgress(PageNo);
      end;
      if Parent.BandExists(Parent.Bands[btCrossHeader]) then
        if (Parent.Bands[btCrossHeader].Flags and flBandRepeatHeader) <> 0 then
        begin
          b := Parent.Bands[btCrossHeader];
          b.DrawCrossCell(Self, Parent.LeftMargin);
          CurX := Parent.LeftMargin + b.dx;
        end;
    end;
  end;
begin
  ColumnXAdjust := 0;
  Parent.ColPos := 1;
  CurX := 0;
  sfpage := PageNo;
  if Typ = btPageFooter then Exit;
  IsColumns := True;
  CurReport.InternalOnBeginColumn(Self);

  if Parent.BandExists(Parent.Bands[btCrossHeader]) then
  begin
    Bnd := Parent.Bands[btCrossHeader];
    Bnd.DrawCrossCell(Self, Bnd.x);
    CurX := Bnd.x + Bnd.dx;
  end;

  if Parent.BandExists(Parent.Bands[btCrossData]) then
  begin
    Bnd := Parent.Bands[btCrossData];
    if CurX = 0 then CurX := Bnd.x;
    if Bnd.DataSet <> nil then
      DS := Bnd.DataSet
    else
      DS := VCDataSet;
      
    if DS <> nil then
    begin
      BM:=DS.GetBookMark;
      DS.DisableControls;
      try
        DS.First;
        while not DS.Eof do
        begin
          ddx := Bnd.dx;
          CurReport.InternalOnPrintColumn(Parent.ColPos, ddx);
          CheckColumnPageBreak(ddx);
          Bnd.DrawCrossCell(Self, CurX);

          if Typ in [btMasterData, btDetailData, btSubdetailData] then
            Parent.DoAggregate([btPageFooter, btMasterFooter, btDetailFooter,
               btSubDetailFooter, btGroupFooter, btCrossFooter, btReportSummary]);

          Inc(CurX, ddx);
          Inc(Parent.ColPos);
          DS.Next;
          if MasterReport.Terminated then break;
        end;
      finally
        DS.GotoBookMark(BM);
        DS.FreeBookMark(BM);
        DS.EnableControls;
      end;
    end;
  end;
  
  if Parent.BandExists(Parent.Bands[btCrossFooter]) then
  begin
    Bnd := Parent.Bands[btCrossFooter];
    if CurX = 0 then CurX := Bnd.x;
    CheckColumnPageBreak(Bnd.dx);
    AggrBand := Bnd;
    Bnd.DrawCrossCell(Self, CurX);
    Bnd.InitValues;
  end;
  PageNo := sfpage;
  ColumnXAdjust := 0;
  IsColumns := False;
end;

function TfrBand.CheckPageBreak(ay, ady: Integer; PBreak: Boolean): Boolean;
begin
  {$IFDEF DebugLR}
  DebugLnEnter('TfrBand.CheckPageBreak INI ay=%d ady=%d Pbreak=%d',[ay,ady,ord(pbreak)]);
  {$ENDIF}
  Result := False;
  with Parent do begin
    {$IFDEF DebugLR}
    DebugLn('say+dy+ady=%d CurBottomY=%d',[ay+Bands[btColumnFooter].dy+ady,CurBottomY]);
    {$ENDIF}
    if not RowsLayout then begin
      if ay + Bands[btColumnFooter].dy + ady > CurBottomY then
      begin
        if not PBreak then
          NewColumn(Self);
        Result := True;
      end;
    end;
  end;
  {$IFDEF DebugLR}
  DebugLnExit('TfrBand.CheckPageBreak END ay=%d ady=%d Result=%d',[ay,ady,ord(Result)]);
  {$ENDIF}
end;

function TfrBand.CheckNextColumn: boolean;
var
  BandHeight: Integer;
begin
  with Parent do begin
    if (CurColumn=0) and (typ=btMasterData) then begin
      BandHeight := DoCalcHeight;
      {$IFDEF DebugLR}
      DebugLn('TfrBand.CheckNextColumn INI CurY=%d BHeight=%d CurY+BH=%d CurBottomY=%d',
        [CurY,BandHeight,CurY+BandHeight,CurBottomY]);
      {$ENDIF}
      // check left height space when on last column
      if CurY + BandHeight>CurBottomY then
        NewPage;
      {$IFDEF DebugLR}
      DebugLn('TfrBand.CheckNextColumn END CurY=%d BHeight=%d CurY+BH=%d CurBottomY=%d',
        [CurY,BandHeight,CurY+BandHeight,CurBottomY]);
      {$ENDIF}
    end;
  end;
end;

procedure TfrBand.DrawPageBreak;
var
  i: Integer;
  newDy, oldy, olddy, aMaxy: Integer;
  t: TfrView;
  Flag: Boolean;

  procedure CorrY(t: TfrView; dy: Integer);
  var
    i: Integer;
    t1: TfrView;
  begin
    for i := 0 to Objects.Count - 1 do
    begin
      t1 :=TfrView(Objects[i]);
      if t1 <> t then
        if (t1.y > t.y + t.dy) and (t1.x >= t.x) and (t1.x <= t.x + t.dx) then
          Inc(t1.y, dy);
    end;
  end;

begin
  {$IFDEF DebugLR}
  DebugLnEnter('DrawPageBreak INI y=%d Maxdy=%d',[y,maxdy]);
  {$ENDIF}
  for i := 0 to Objects.Count - 1 do
  begin
    t :=TfrView(Objects[i]);
    t.Selected := True;
    t.OriginalRect := Rect(t.x, t.y, t.dx, t.dy);
  end;
  if not CheckPageBreak(y, maxdy, True) then
    DrawObjects
  else
  begin
    for i := 0 to Objects.Count - 1 do
    begin
      t :=TfrView(Objects[i]);
      if t is TfrStretcheable then
        TfrStretcheable(t).ActualHeight := 0;
      if t is TfrMemoView then
      begin
        {$IFDEF DebugLR}
        DebugLnEnter('CalcHeight Memo INI');
        {$ENDIF}
        TfrMemoView(t).CalcHeight; // wraps a memo onto separate lines
        t.Memo1.Assign(SMemo);
        {$IFDEF DebugLR}
        DebugLnExit('CalcHeight Memo DONE');
        {$ENDIF}
      end;
    end;
    repeat
      newDy := Parent.CurBottomY - Parent.Bands[btColumnFooter].dy - y - 2;
      aMaxy := 0;
      for i := 0 to Objects.Count - 1 do
      begin
        t :=TfrView(Objects[i]);
        if t.Selected then
        if (t.y >= 0) and (t.y < newdy) then
          if (t.y + t.dy < newdy) then
          begin
            if aMaxy < t.y + t.dy then
              aMaxy := t.y + t.dy;
            DrawObject(t);
            t.Selected := False;
          end
          else
          begin
            if t is TfrStretcheable then
            begin
              olddy := t.dy;
              t.dy := newdy - t.y + 1;
              Inc(TfrStretcheable(t).ActualHeight, t.dy);
              if t.dy > TfrStretcheable(t).MinHeight then
              begin
                TfrStretcheable(t).DrawMode := drPart;
                DrawObject(t);
              end;
              t.dy := olddy;
            end
            else
              t.y := newdy
          end
        else if t is TfrStretcheable then
          if (t.y < 0) and (t.y + t.dy >= 0) then
            if t.y + t.dy < dy then
            begin
              oldy := t.y; olddy := t.dy;
              t.dy := t.y + t.dy;
              t.y := 0;
              if t.dy > TfrStretcheable(t).MinHeight div 2 then
              begin
                t.dy := TfrStretcheable(t).RemainHeight + t.gapy * 2 + 1;
                Inc(TfrStretcheable(t).ActualHeight, t.dy - 1);
                if aMaxy < t.y + t.dy then
                  aMaxy := t.y + t.dy;
                TfrStretcheable(t).DrawMode := drPart;
                DrawObject(t);
              end;
              t.y := oldy; t.dy := olddy;
              CorrY(t, TfrStretcheable(t).ActualHeight - t.dy);
              t.Selected := False;
            end
            else
            begin
              oldy := t.y; olddy := t.dy;
              t.y := 0; t.dy := newdy;
              Inc(TfrStretcheable(t).ActualHeight, t.dy);
              TfrStretcheable(t).DrawMode := drPart;
              DrawObject(t);
              t.y := oldy; t.dy := olddy;
              t.Selected := False;
            end;
      end;
      Flag := False;
      for i := 0 to Objects.Count - 1 do
      begin
        t :=TfrView(Objects[i]);
        if t.Selected then Flag := True;
        Dec(t.y, newdy);
      end;
      if Flag then CheckPageBreak(y, 10000, False);
      y := Parent.CurY;
      if MasterReport.Terminated then break;
    until not Flag;
    maxdy := aMaxy;
  end;
  for i := 0 to Objects.Count - 1 do
  begin
    t :=TfrView(Objects[i]);
    t.y := t.OriginalRect.Top;
    t.dy := t.OriginalRect.Bottom;
  end;
  Inc(Parent.CurY, maxdy);
  {$IFDEF DebugLR}
  DebugLnExit('DrawPageBreak END Parent.CurY=%d',[Parent.CurY]);
  {$ENDIF}
end;

function TfrBand.HasCross: Boolean;
var
  i: Integer;
  t: TfrView;
begin
  Result := False;
  for i := 0 to Objects.Count - 1 do
  begin
    t :=TfrView(Objects[i]);
    if t.Parent <> Self then
    begin
      Result := True;
      break;
    end;
  end;
end;

procedure TfrBand.DoDraw;
var
  sfy, sh: Integer;
  UseY, WasSub: Boolean;

begin
  if Objects.Count = 0 then Exit;
  sfy := y;
  UseY := not (Typ in [btPageFooter, btOverlay, btNone]);
  if UseY then
    y := Parent.CurY;
  {$IFDEF DebugLR}
  DebugLnEnter('TfrBand.DoDraw INI Band=%s sfy=%d y=%d dy=%d XAdjust=%d CurY=%d Stretch=%d PageBreak=%d',
    [bandInfo(self), sfy, y, dy, Parent.XAdjust, parent.cury, Ord(Stretched), Ord(PageBreak)]);
  {$ENDIF}

  Parent.RowStarted := True;
    
  if Stretched then
  begin
    sh := CalculatedHeight;
//    sh := CalcHeight;
    if sh > dy then
      StretchObjects(sh);
    maxdy := sh;
    if not PageBreak then
      CheckPageBreak(y, sh, False);
    y := Parent.CurY;
    WasSub := False;
    if PageBreak then
    begin
      DrawPageBreak;
      sh := 0;
    end
    else
    begin
      WasSub := DrawObjects;
      if HasCross then
        DrawCross;
    end;
    UnStretchObjects;

    Parent.LastRowHeight := sh;

    if not WasSub then
      Inc(Parent.CurY, sh);
  end
  else
  begin

    if UseY then
    begin
      if not PageBreak then
        CheckPageBreak(y, dy, False);
      y := Parent.CurY;
    end;

    if PageBreak then
    begin
      maxdy := CalculatedHeight;
      DrawPageBreak;
      Parent.LastRowHeight := maxdy;
    end
    else
    begin
      WasSub := DrawObjects;
      if HasCross then
        DrawCross;
      if UseY and not WasSub then begin

        Parent.LastRowHeight := dy;

        if Parent.AdvanceRow(Self) then
          Inc(Parent.CurY, dy);
      end;
    end;
  end;
  y := sfy;
  if Typ in [btMasterData, btDetailData, btSubDetailData] then
    Parent.DoAggregate([btPageFooter, btMasterFooter, btDetailFooter,
                 btSubDetailFooter, btGroupFooter, btReportSummary]);
  {$IFDEF DebugLR}
  DebugLnExit('TfrBand.DoDraw END sfy=%d y=%d dy=%d xadjust=%d CurY=%d',
    [sfy, y, dy, parent.xadjust, parent.cury]);
  {$ENDIF}
end;

function TfrBand.DoCalcHeight: Integer;
var
  b: TfrBand;
begin
  if (Typ in [btMasterData, btDetailData, btSubDetailData]) and
    (Next <> nil) and (Next.Dataset = nil) then
  begin
    b := Self;
    Result := 0;
    repeat
      Result := Result + b.CalcHeight;
      b := b.Next;
    until b = nil;
  end
  else
  begin
    Result := dy;
    CalculatedHeight := dy;
    if Stretched then Result := CalcHeight;
  end;
end;

function TfrBand.Draw: Boolean;
var
  b: TfrBand;
begin
  {$IFDEF debugLr}
  DebugLnEnter('TFrBand.Draw INI Band=%s y=%d vis=%s',[BandInfo(self),y,BoolToStr(Visible,true)]);
  {$endif}
  Result := False;
  CurView := View;
  CurBand := Self;
  AggrBand := Self;
  CalculatedHeight := -1;
  ForceNewPage := False;
  ForceNewColumn := False;
  if Assigned(CurReport.FOnBeginBand) then
    CurReport.FOnBeginBand(Self);
  frInterpretator.DoScript(Script);

  if Parent.RowsLayout and IsDataBand then begin
  
    if Visible then
    begin
      if Objects.Count > 0 then
      begin
        if not (Typ in [btPageFooter, btOverlay, btNone]) then begin
          if Parent.Skip then
            exit
          else
            CheckNextColumn;
        end;
        EOFReached := True;
        // only masterdata band supported in RowsLayout columns report
        if typ=btMasterData then begin
          DoDraw;
          Parent.NextColumn(Self);
        end;
        if not EOFReached then
          Result := True;
      end;
    end;

  end else begin

    if Parent.RowsLayout and (typ<>btColumnHeader) then

      Parent.StartRowsLayoutNonDataBand(Self)

    else
    // new page was requested in script
    if ForceNewPage then
    begin
      Parent.CurColumn := Parent.ColCount - 1;
      Parent.NewColumn(Self);
    end;
    if ForceNewColumn then
      Parent.NewColumn(Self);

    if Visible then
    begin
      if Typ = btColumnHeader then
        Parent.LastStaticColumnY := Parent.CurY;
      if Typ = btPageFooter then
        y := Parent.CurBottomY;
      if Objects.Count > 0 then
      begin
        if not (Typ in [btPageFooter, btOverlay, btNone]) then
          if (Parent.CurY + DoCalcHeight > Parent.CurBottomY) and not PageBreak then
          begin
            Result := True;
            if Parent.Skip then
              Exit
            else
              CheckPageBreak(0, 10000, False);
          end;
        EOFReached := True;

        // dealing with multiple bands
        if (Typ in [btMasterData, btDetailData, btSubDetailData]) and
          (Next <> nil) and (Next.Dataset = nil) and (DataSet <> nil) then
        begin
          b := Self;
          repeat
            b.DoDraw;
            b := b.Next;
          until b = nil;
        end
        else
        begin
          DoDraw;
          if not (Typ in [btMasterData, btDetailData, btSubDetailData, btGroupHeader]) and
            NewPageAfter then
            Parent.NewPage;
        end;
        if not EOFReached then Result := True;
      end;
    end
    // if band is not visible, just performing aggregate calculations
    // relative to it
    else
    if Typ in [btMasterData, btDetailData, btSubDetailData] then
      Parent.DoAggregate([btPageFooter, btMasterFooter, btDetailFooter,
                          btSubDetailFooter, btGroupFooter, btReportSummary]);

    // check if multiple pagefooters (in cross-tab report) - resets last of them
    if not DisableInit then
      if (Typ <> btPageFooter) or (PageNo = MasterReport.EMFPages.Count - 1) then
        InitValues;

    // if in rows layout, reset starting column after non-data band
    if Parent.RowsLayout and (typ<>btColumnHeader) then
      Parent.StartColumn;

  end;
  
  if Assigned(CurReport.FOnEndBand) then
    CurReport.FOnEndBand(Self);

  Parent.LastBandType := typ;

  {$IFDEF debugLr}
  DebugLnExit('TFrBand.Draw END %s y=%d PageNo=%d EOFReached=',[dbgsname(self),y, PageNo]);
  {$endif}
end;

procedure TfrBand.InitValues;
var
  b: TfrBand;
begin
  if Typ = btGroupHeader then
  begin
    b := Self;
    while b <> nil do
    begin
      if b.FooterBand <> nil then
      begin
        b.FooterBand.Values.Clear;
        b.FooterBand.Count := 0;
      end;
      b.LastGroupValue := frParser.Calc(b.GroupCondition);
      b := b.Next;
    end;
  end
  else
  begin
    Values.Clear;
    Count := 0;
  end
end;

{$ifdef DebugLR}
function DecodeValue(s:string):string;
var
  p: Integer;
begin
  result := s;
  p := pos('=',result) + 2;
  if result<>'' then
    insert('|',result,p);
end;
{$endif}
procedure TfrBand.DoAggregate;
var
  i: Integer;
  t: TfrView;
  s: String;
  v: Boolean;
begin
  {$ifdef DebugLR}
  DebugLnEnter('TfrBand.DoAggregate INIT Band=%s',[BandInfo(self)]);
  {$endif}
 for i := 0 to Values.Count - 1 do
  begin
    s := Values[i];
    {$ifdef DebugLR}
    DbgOut('Mangling Values[',dbgs(i),']=',QuotedStr(DecodeValue(s)),' ==> ');
    {$endif}
    Values[i] := Copy(s, 1, Pos('=', s) - 1) + '=0' + Copy(s, Pos('=', s) + 2, 255);
    {$ifdef DebugLR}
    DebugLn(QuotedStr(DecodeValue(Values[i])));
    {$endif}
  end;

  v := Visible;
  Visible := False;
  AggrBand := Self;
  for i := 0 to Objects.Count - 1 do
  begin
    t :=TfrView(Objects[i]);
    CurView := t;
    if t is TfrMemoView then
      TfrMemoView(t).ExpandVariables;
  end;
  Visible := v;
  Inc(Count);
  {$ifdef DebugLR}
  DebugLnExit('TfrBand.DoAggregate DONE Band=%s',[BandInfo(self)]);
  {$endif}
end;

procedure TfrBand.ResetLastValues;
var
  i: Integer;
  t: TfrView;
begin
  for i := 0 to Objects.Count - 1 do
  begin
    t :=TfrView(Objects[i]);
    t.ResetLastValue;
  end;
end;

{----------------------------------------------------------------------------}
type
  TfrBandParts = (bpHeader, bpData, bpFooter);
const
  MAXBNDS = 3;
  Bnds: Array[1..MAXBNDS, TfrBandParts] of TfrBandType =
   ((btMasterHeader, btMasterData, btMasterFooter),
    (btDetailHeader, btDetailData, btDetailFooter),
    (btSubDetailHeader, btSubDetailData, btSubDetailFooter));


constructor TfrPage.Create(ASize, AWidth, AHeight: Integer;
  AOr: TPrinterOrientation);
begin
  Self.Create;
  
  ChangePaper(ASize, AWidth, AHeight, AOr);
  PrintToPrevPage := False;
  UseMargins := True;
end;

constructor TfrPage.CreatePage;
begin
  self.Create;
end;

destructor TfrPage.Destroy;
begin
  Clear;
  Objects.Free;
  RTObjects.Free;
  List.Free;
  fMargins.Free;
  
  inherited Destroy;
end;

procedure TfrPage.ChangePaper(ASize, AWidth, AHeight: Integer;
  AOr: TPrinterOrientation);
begin
  try
    Prn.SetPrinterInfo(ASize, AWidth, AHeight, AOr);
    Prn.FillPrnInfo(PrnInfo);
  except
    on E:exception do
    begin
      Prn.SetPrinterInfo($100, AWidth, AHeight, AOr);
      Prn.FillPrnInfo(PrnInfo);
    end;
  end;
  pgSize := Prn.PaperSize;
  Width := Prn.PaperWidth;
  Height := Prn.PaperHeight;
  Orientation:= Prn.Orientation;
end;

procedure TfrPage.Clear;
begin
  while Objects.Count > 0 do
    Delete(0);
end;

procedure TfrPage.Delete(Index: Integer);
begin
  TfrView(Objects[Index]).Free;
  Objects.Delete(Index);
end;

function TfrPage.FindObjectByID(ID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Objects.Count - 1 do
  begin
    if TfrView(Objects[i]).ID = ID then
    begin
      Result := i;
      break;
    end;
  end;
end;

function TfrPage.FindObject(const aName: String): TfrObject;
var
  i: Integer;
begin
  Result := nil;
  if AnsiCompareText(Self.Name, aName) = 0 then
    Result:=Self
  else
  begin
    for i := 0 to Objects.Count - 1 do
    begin
      if AnsiCompareText(TfrObject(Objects[i]).Name, aName) = 0 then
      begin
        Result :=TfrObject(Objects[i]);
        Exit;
      end;
    end;
  end;
end;

function TfrPage.FindRTObject(const aName: String): TfrObject;
var
  i: Integer;
begin
  Result := nil;
  if AnsiCompareText(Self.Name, aName) = 0 then
    Result:=Self
  else
    for i := 0 to RTObjects.Count - 1 do
    begin
      if AnsiCompareText(TfrObject(RTObjects[i]).Name, aName) = 0 then
      begin
        Result :=TfrObject(RTObjects[i]);
        Exit;
      end;
    end;
end;

procedure TfrPage.InitReport;
var
  b: TfrBandType;
begin
  for b := btReportTitle to btNone do
    Bands[b] := TfrBand.Create(b, Self);
  while RTObjects.Count > 0 do
  begin
    TfrView(RTObjects[0]).Free;
    RTObjects.Delete(0);
  end;
  TossObjects;
  InitFlag := True;
  CurPos := 1; ColPos := 1;
end;

procedure TfrPage.DoneReport;
var
  b: TfrBandType;
begin
  if InitFlag then
  begin
    for b := btReportTitle to btNone do
      Bands[b].Free;
    while RTObjects.Count > 0 do
    begin
      TfrView(RTObjects[0]).Free;
      RTObjects.Delete(0);
    end;
  end;
  InitFlag := False;
end;

function TfrPage.TopMargin: Integer;
begin
  if UseMargins then
  begin
    if Margins.Top = 0 then
      Result := PrnInfo.Ofy
    else
      Result := Margins.Top;
  end
  else Result := 0;
end;

function TfrPage.BottomMargin: Integer;
begin
  with PrnInfo do
    if UseMargins then
      if Margins.Bottom = 0 then
        Result:=Ofy+Ph
      else
        Result:=Pgh-Margins.Bottom
    else
      Result:=Pgh;
  if (DocMode <> dmDesigning) and BandExists(Bands[btPageFooter]) then
    Result := Result - Bands[btPageFooter].dy;
end;

function TfrPage.LeftMargin: Integer;
begin
  if UseMargins then
  begin
    if Margins.Left = 0 then
      Result := PrnInfo.Ofx
    else
      Result := Margins.Left;
  end
  else Result := 0;
end;

function TfrPage.RightMargin: Integer;
begin
  with PrnInfo do
  begin
    if UseMargins then
    begin
      if Margins.Right = 0 then
        Result := Ofx + Pw
      else
        Result := Pgw - Margins.Right;
    end
    else Result := Pgw;
  end;
end;

procedure TfrPage.TossObjects;
var
  i, j, n, last, miny: Integer;
  b: TfrBandType;
  bt, t: TfrView;
  Bnd, Bnd1: TfrBand;
  FirstBand, Flag: Boolean;
  BArr: Array[0..31] of TfrBand;
  s: String;
begin
  for i := 0 to Objects.Count - 1 do
  begin
    bt :=TfrView(Objects[i]);
    t := frCreateObject(bt.Typ, bt.ClassName);
    t.Assign(bt);
    t.StreamMode := smPrinting;
    RTObjects.Add(t);
    if (t.Flags and flWantHook) <> 0 then
      HookList.Add(t);
  end;

  for i := 0 to RTObjects.Count - 1 do // select all objects exclude bands
  begin
    t :=TfrView(RTObjects[i]);
    t.Selected := t.Typ <> gtBand;
    t.Parent := nil;
    frInterpretator.PrepareScript(t.Script, t.Script, SMemo);
    if t.Typ = gtSubReport then
      CurReport.Pages[(t as TfrSubReportView).SubPage].Skip := True;
  end;
  Flag := False;
  for i := 0 to RTObjects.Count - 1 do // search for btCrossXXX bands
  begin
    bt :=TfrView(RTObjects[i]);
    if (bt.Typ = gtBand) and
       (TfrBandView(bt).BandType in [btCrossHeader..btCrossFooter]) then
    with Bands[TfrBandView(bt).BandType] do
    begin
      Memo.Assign(bt.Memo);
      Script.Assign(bt.Script);
      x := bt.x; dx := bt.dx;
      InitDataSet(TfrBandView(bt).DataSet);
      View := bt;
      Flags := bt.Flags;
      Visible := bt.Visible;
      bt.Parent := Bands[TfrBandView(bt).BandType];
      Flag := True;
    end;
  end;

  if Flag then // fill a ColumnXXX bands at first
    for b := btCrossHeader to btCrossFooter do
    begin
      Bnd := Bands[b];
      for i := 0 to RTObjects.Count - 1 do
      begin
        t :=TfrView(RTObjects[i]);
        if t.Selected then
         if (t.x >= Bnd.x) and (t.x + t.dx <= Bnd.x + Bnd.dx) then
         begin
           t.x := t.x - Bnd.x;
           t.Parent := Bnd;
           Bnd.Objects.Add(t);
         end;
      end;
    end;

  for b := btReportTitle to btGroupFooter do // fill other bands
  begin
    FirstBand := True;
    Bnd := Bands[b];
    BArr[0] := Bnd;
    Last := 1;
    for i := 0 to RTObjects.Count - 1 do // search for specified band
    begin
      bt :=TfrView(RTObjects[i]);
      if (bt.Typ = gtBand) and (TfrBandView(bt).BandType=b) then
      begin
        if not FirstBand then
        begin
          Bnd.Next := TfrBand.Create(b,Self);
          Bnd := Bnd.Next;
          BArr[Last] := Bnd;
          Inc(Last);
        end;
        FirstBand := False;
        Bnd.Memo.Assign(bt.Memo);
        Bnd.Script.Assign(bt.Script);
        Bnd.y := bt.y;
        Bnd.dy := bt.dy;
        Bnd.View := bt;
        Bnd.Flags := bt.Flags;
        Bnd.Visible := bt.Visible;
        bt.Parent := Bnd;
        with bt as TfrBandView, Bnd do
        begin
          if Typ = btGroupHeader then
            InitDataSet(TfrBandView(Bt).fCondition)
          else
            InitDataSet(TfrBandView(Bt).DataSet);
          Stretched := (Flags and flStretched) <> 0;
          PrintIfSubsetEmpty := (Flags and flBandPrintIfSubsetEmpty) <> 0;
          if Skip then
          begin
            NewPageAfter := False;
            PageBreak := False;
          end
          else
          begin
            NewPageAfter := (Flags and flBandNewPageAfter) <> 0;
            PageBreak := (Flags and flBandPageBreak) <> 0;
          end;
        end;
        for j := 0 to RTObjects.Count - 1 do // placing objects over band
        begin
          t :=TfrView(RTObjects[j]);
          if (t.Parent = nil) and (t.Typ <> gtSubReport) then
           if t.Selected then
            if (t.y >= Bnd.y) and (t.y <= Bnd.y + Bnd.dy) then
            begin
              t.Parent := Bnd;
              t.y := t.y - Bnd.y;
              t.Selected := False;
              Bnd.Objects.Add(t);
            end;
        end;
        for j := 0 to RTObjects.Count - 1 do // placing ColumnXXX objects over band
        begin
          t :=TfrView(RTObjects[j]);
          if t.Parent <> nil then
           if t.Selected then
            if (t.y >= Bnd.y) and (t.y <= Bnd.y + Bnd.dy) then
            begin
              t.y := t.y - Bnd.y;
              t.Selected := False;
              Bnd.Objects.Add(t);
            end;
        end;
        for j := 0 to RTObjects.Count - 1 do // placing subreports over band
        begin
          t :=TfrView(RTObjects[j]);
          if (t.Parent = nil) and (t.Typ = gtSubReport) then
           if t.Selected then
            if (t.y >= Bnd.y) and (t.y <= Bnd.y + Bnd.dy) then
            begin
              t.Parent := Bnd;
              t.y := t.y - Bnd.y;
              t.Selected := False;
              Bnd.Objects.Add(t);
            end;
        end;
      end;
    end;
    for i := 0 to Last - 1 do // sorting bands
    begin
      miny := BArr[i].y; n := i;
      for j := i + 1 to Last - 1 do
        if BArr[j].y < miny then
        begin
          miny := BArr[j].y;
          n := j;
        end;
      Bnd := BArr[i]; BArr[i] := BArr[n]; BArr[n] := Bnd;
    end;
    Bnd := BArr[0]; Bands[b] := Bnd;
    Bnd.Prev := nil;
    for i := 1 to Last - 1 do  // finally ordering
    begin
      Bnd.Next := BArr[i];
      Bnd := Bnd.Next;
      Bnd.Prev := BArr[i - 1];
    end;
    Bnd.Next := nil;
    Bands[b].LastBand := Bnd;
  end;

  for i := 0 to RTObjects.Count - 1 do // place other objects on btNone band
  begin
    t :=TfrView(RTObjects[i]);
    if t.Selected then
    begin
      t.Parent := Bands[btNone];
      Bands[btNone].y := 0;
      Bands[btNone].Objects.Add(t);
    end;
  end;

  for i := 1 to MAXBNDS do  // connect header & footer to each data-band
  begin
    Bnd := Bands[Bnds[i, bpHeader]];
    while Bnd <> nil do
    begin
      Bnd1 := Bands[Bnds[i, bpData]];
      while Bnd1 <> nil do
      begin
        if Bnd1.y > Bnd.y + Bnd.dy then break;
        Bnd1 := Bnd1.Next;
      end;
      if (Bnd1 <> nil) and (Bnd1.HeaderBand = nil) then
        Bnd1.HeaderBand := Bnd;

      Bnd := Bnd.Next;
    end;

    Bnd := Bands[Bnds[i, bpFooter]];
    while Bnd <> nil do
    begin
      Bnd1 := Bands[Bnds[i, bpData]];
      while Bnd1 <> nil do
      begin
        if Bnd1.y + Bnd1.dy > Bnd.y then
        begin
          Bnd1 := Bnd1.Prev;
          break;
        end;
        if Bnd1.Next = nil then break;
        Bnd1 := Bnd1.Next;
      end;
      if (Bnd1 <> nil) and (Bnd1.FooterBand = nil) then
        Bnd1.FooterBand := Bnd;

      Bnd := Bnd.Next;
    end;
  end;

  Bnd := Bands[btGroupHeader].LastBand;
  Bnd1 := Bands[btGroupFooter];
  repeat
    Bnd.FooterBand := Bnd1;
    Bnd := Bnd.Prev;
    Bnd1 := Bnd1.Next;
  until (Bnd = nil) or (Bnd1 = nil);

  if BandExists(Bands[btCrossData]) and (Pos(';', TfrBandView(Bands[btCrossData].View).DataSet) <> 0) then
  begin
    s := TfrBandView(Bands[btCrossData].View).DataSet;

    i := 1;
    while i < Length(s) do
    begin
      j := i;
      while s[j] <> '=' do Inc(j);
      n := j;
      while s[n] <> ';' do Inc(n);
      for b := btMasterHeader to btGroupFooter do
      begin
        Bnd := Bands[b];
        while Bnd <> nil do
        begin
          if Bnd.View <> nil then
            if AnsiCompareText(Bnd.View.Name, Copy(s, i, j - i)) = 0 then
              CreateDS(Copy(s, j + 1, n - j - 1), Bnd.VCDataSet, Bnd.IsVirtualVCDS);
          Bnd := Bnd.Next;
        end;
      end;
      i := n + 1;
    end;
  end;

  if ColCount = 0 then ColCount := 1;
  ColWidth := (RightMargin - LeftMargin) div ColCount;
end;

procedure TfrPage.PrepareObjects;
var
  i, j: Integer;
  t: TfrView;
  Value: TfrValue;
  s: String;
  DSet: TfrTDataSet;
  Field: TfrTField;
begin
  {$ifdef DebugLR}
  DebugLnEnter('TfrPage.PrepareObjects INIT');
  {$endif}

  CurPage := Self;
  for i := 0 to RTObjects.Count - 1 do
  begin
    t :=TfrView(RTObjects[i]);
    t.FField := '';
    if t.Memo.Count > 0 then
      s := t.Memo[0];
    j := Length(s);
    if (j > 2) and (s[1] = '[') then
    begin
      while (j > 0) and (s[j] <> ']') do Dec(j);
      s := Copy(s, 2, j - 2);
      t.FDataSet := nil;
      t.FField := '';
      Value := CurReport.Values.FindVariable(s);
      if Value = nil then
      begin
        CurBand := t.Parent;
        DSet := GetDefaultDataset;
        frGetDatasetAndField(s, DSet, Field);
        if Field <> nil then
        begin
          {$ifdef DebugLR}
          DebugLn('For View=%s found Field=%s',[ViewInfo(t),Field.FieldName]);
          {$endif}
          t.FDataSet := DSet;
          t.FField := Field.FieldName;
        end;
      end
      else if Value.Typ = vtDBField then
        if Value.DSet <> nil then
        begin
          t.FDataSet := Value.DSet;
          t.FField := Value.Field;
        end;
    end;
  end;
  {$ifdef DebugLR}
  DebugLnExit('TfrPage.PrepareObjects DONE');
  {$endif}
end;

procedure TfrPage.ShowBand(b: TfrBand);
begin
  if b <> nil then
  begin
    {$IFDEF DebugLR}
    DebugLn;
    DebugLnEnter('TfrPage.ShowBand INI Band=%s',[BandInfo(b)]);
    {$ENDIF}
    if Mode = pmBuildList then
      AddRecord(b, rtShowBand) else
      b.Draw;
    {$IFDEF DebugLR}
    DebugLnExit('TfrPage.ShowBand END Band=%s',[BandInfo(b)]);
    {$ENDIF}
  end;
end;

constructor TfrPage.Create;
begin
  inherited Create;
  fMargins:=TfrRect.Create;
  BaseName:='Page';
  
  List := TFpList.Create;
  Objects := TFpList.Create;
  RTObjects := TFpList.Create;
  PageType:=ptReport;
end;

procedure TfrPage.ShowBandByName(const s: String);
var
  bt: TfrBandType;
  b: TfrBand;
begin
  for bt := btReportTitle to btNone do
  begin
    b := Bands[bt];
    while b <> nil do
    begin
      if b.View <> nil then
        if AnsiCompareText(b.View.Name, s) = 0 then
        begin
          b.Draw;
          Exit;
        end;
      b := b.Next;
    end;
  end;
end;

procedure TfrPage.ShowBandByType(bt: TfrBandType);
var
  b: TfrBand;
begin
  b := Bands[bt];
  if b <> nil then
    b.Draw;
end;

procedure TfrPage.AddRecord(b: TfrBand; rt: TfrBandRecType);
var
  p: PfrBandRec;
begin
  GetMem(p, SizeOf(TfrBandRec));
  p^.Band := b;
  p^.Action := rt;
  List.Add(p);
end;

procedure TfrPage.ClearRecList;
var
  i: Integer;
begin
  for i := 0 to List.Count - 1 do
    FreeMem(PfrBandRec(List[i]), SizeOf(TfrBandRec));
  List.Clear;
end;

function TfrPage.PlayRecList: Boolean;
var
  p: PfrBandRec;
  b: TfrBand;
begin
  Result := False;
  while PlayFrom < List.Count do
  begin
    p := List[PlayFrom];
    b := p^.Band;
    case p^.Action of
      rtShowBand:
        begin
          if LastBand <> nil then
          begin
            LastBand.DoSubReports;
            if LastBand <> nil then
            begin
              Result := True;
              Exit;
            end;
          end
          else
            if b.Draw then
            begin
              Result := True;
              Exit;
            end;
        end;
      rtFirst:
        begin
          b.DataSet.First;
          b.Positions[psLocal] := 1;
        end;
      rtNext:
        begin
          b.DataSet.Next;
          Inc(CurPos);
          Inc(b.Positions[psGlobal]);
          Inc(b.Positions[psLocal]);
        end;
    end;
    Inc(PlayFrom);
  end;
end;

procedure TfrPage.DrawPageFooters;
begin
  {$IFDEF DebugLR}
  DebugLn('TFrPage.DrawPageFootersPage INI PageNo=%d XAdjust=%d CurColumn=%d',
    [PageNo, XAdjust, CurColumn]);
  {$ENDIF}
  CurColumn := 0;
  XAdjust := LeftMargin;
  if (PageNo <> 0) or ((Bands[btPageFooter].Flags and flBandOnFirstPage) <> 0) then
    while PageNo < MasterReport.EMFPages.Count do
    begin
      if not (Append and WasPF) then
      begin
        if (CurReport <> nil) and Assigned(CurReport.FOnEndPage) then
          CurReport.FOnEndPage(PageNo);
        if (MasterReport <> CurReport) and (MasterReport <> nil) and
          Assigned(MasterReport.FOnEndPage) then
          MasterReport.FOnEndPage(PageNo);
        if not RowsLayout then
          ShowBand(Bands[btPageFooter]);
      end;
      Inc(PageNo);
    end;
  PageNo := MasterReport.EMFPages.Count;
  {$IFDEF DebugLR}
  DebugLn('TFrPage.DrawPageFootersPage FIN PageNo=%d XAdjust=%d CurColumn=%d',
    [PageNo, XAdjust, CurColumn]);
  {$ENDIF}
end;

procedure TfrPage.NewPage;
begin
  {$IFDEF DebugLR}
  DebugLnEnter('TFrPage.NewPage INI PageNo=%d CurBottomY=%d CurY=%d XAdjust=%d',
    [PageNo, CurBottomY, CurY, XAdjust]);
  {$ENDIF}

  CurReport.InternalOnProgress(PageNo + 1);
  if not RowsLayout then
    ShowBand(Bands[btColumnFooter]);
  DrawPageFooters;
  CurBottomY := BottomMargin;
  MasterReport.EMFPages.Add(Self);
  Append := False;
  {$IFDEF DebugLR}
  DebugLn('---- Start of new page ----');
  {$ENDIF}
  ShowBand(Bands[btOverlay]);
  CurY := TopMargin;
  ShowBand(Bands[btPageHeader]);
  if not RowsLayout then
    ShowBand(Bands[btColumnHeader]);
  {$IFDEF DebugLR}
  DebugLnExit('TFrPage.NewPage END PageNo=%d CurBottomY=%d CurY=%d XAdjust=%d',
    [PageNo, CurBottomY, CurY, XAdjust]);
  {$ENDIF}
end;

procedure TfrPage.NewColumn(Band: TfrBand);
var
  b: TfrBand;
begin
  {$IFDEF DebugLR}
  DebugLnEnter('TfrPage.NewColumn INI CurColumn=%d ColCount=%d CurY=%d XAdjust=%d',
    [CurColumn, ColCount, CurY, XAdjust]);
  {$ENDIF}
  if CurColumn < ColCount - 1 then
  begin
    ShowBand(Bands[btColumnFooter]);
    Inc(CurColumn);
    Inc(XAdjust, ColWidth + ColGap);
    CurY := LastStaticColumnY;
    ShowBand(Bands[btColumnHeader]);
  end
  else
    NewPage;
  b := Bands[btGroupHeader];
  if b <> nil then
    while (b <> nil) and (b <> Band) do
    begin
      b.DisableInit := True;
      if (b.Flags and flBandRepeatHeader) <> 0 then
        ShowBand(b);
      b.DisableInit := False;
      b := b.Next;
    end;
  if Band.Typ in [btMasterData, btDetailData, btSubDetailData] then
  begin
    if (Band.HeaderBand <> nil) and
      ((Band.HeaderBand.Flags and flBandRepeatHeader) <> 0) then
      ShowBand(Band.HeaderBand);
    Band.ResetLastValues;
  end;
  {$IFDEF DebugLR}
  DebugLnExit('TfrPage.NewColumn END CurColumn=%d ColCount=%d CurY=%d XAdjust=%d',
    [CurColumn, ColCount, CurY, XAdjust]);
  {$ENDIF}
end;

procedure TfrPage.NextColumn(Band: TFrBand);
begin
  {$IFDEF DebugLR}
  DebugLnEnter('TfrPage.NextColumn INI CurColumn=%d ColCount=%d CurY=%d XAdjust=%d',
    [CurColumn, ColCount, CurY, XAdjust]);
  {$ENDIF}
  if CurColumn < ColCount - 1 then
  begin
    Inc(CurColumn);
    Inc(XAdjust, ColWidth + ColGap);
    Inc(ColPos);
  end
  else
    StartColumn;
  {$IFDEF DebugLR}
  DebugLnExit('TfrPage.NextColumn END CurColumn=%d ColCount=%d CurY=%d XAdjust=%d',
    [CurColumn, ColCount, CurY, XAdjust]);
  {$ENDIF}
end;

function TfrPage.RowsLayout: boolean;
begin
  result := (ColCount>1) and (LayoutOrder=loRows)
end;

procedure TfrPage.StartColumn;
begin
  CurColumn := 0;
  ColPos:=1;
  XAdjust := LeftMargin;
end;

procedure TfrPage.StartRowsLayoutNonDataBand(Band: TfrBand);
begin

  // reset starting column
  if Band.ForceNewPage then begin
    CurColumn := ColCount - 1;
    NewColumn(Band);
  end else
    StartColumn;

  // check for partial rows
  if LastBandType in [btMasterData, btDetailData, btSubdetailData] then
  begin
    if not RowStarted then
      Inc(CurY, LastRowHeight);
  end;

end;

function TfrPage.AdvanceRow(Band: TfrBand): boolean;
begin
  result := not RowsLayout or (not Band.IsDataBand) or (CurColumn=ColCount-1);
  RowStarted := result;
end;

procedure TfrPage.DoAggregate(a: Array of TfrBandType);
var
  i: Integer;
  procedure DoAggregate1(bt: TfrBandType);
  var
    b: TfrBand;
  begin
    b := Bands[bt];
    while b <> nil do
    begin
      b.DoAggregate;
      b := b.Next;
    end;
  end;
begin
  for i := Low(a) to High(a) do
    DoAggregate1(a[i]);
end;

procedure TfrPage.FormPage;
type
  TBookRecord = record
    Dataset: TfrDataset;
    Bookmark: Pointer;
  end;
var
  BndStack: Array[1..MAXBNDS * 3] of TfrBand;
  MaxLevel, BndStackTop: Integer;
  i, sfPage            : Integer;
  HasGroups            : Boolean;
  DetailCount          : Integer;
  BooksBkUp            : array of TBookRecord;
  CurGroupValue        : variant;
  BookPrev             : pointer;
  
  procedure AddToStack(b: TfrBand);
  begin
    if b <> nil then
    begin
      Inc(BndStackTop);
      BndStack[BndStackTop] := b;
    end;
  end;

  procedure BackupBookmark(b: TfrBand);
  var
    n: Integer;
  begin
    if b.Dataset <> nil then
    begin
      n := Length(BooksBkUp);
      SetLength(BooksBkUp, n+1);
      BooksBkUp[n].Dataset := b.Dataset;
      BooksBkUp[n].Bookmark := b.Dataset.GetBookmark;
    end;
    if b.Typ in [btDetailData,btSubDetailData] then
      inc(DetailCount);
  end;

  procedure RestoreBookmarks;
  var
    n: Integer;
  begin
    for n:=0 to Length(BooksBkUp)-1 do
    with BooksBkUp[n] do begin
      Dataset.GotoBookMark(Bookmark);
      Dataset.FreeBookMark(Bookmark);
      if DetailCount=0 then
        Dataset.EnableControls;
    end;
    SetLength(BooksBkUp, 0);
  end;

  procedure DisableControls;
  var
    n: Integer;
  begin
    if DetailCount=0 then
    for n:=0 to Length(BooksBkUp)-1 do
      BooksBkUp[n].Dataset.DisableControls;
  end;

  procedure ShowStack;
  var
    i: Integer;
  begin
    {$IFDEF DebugLR}
    DebugLnEnter('ShowStack INI');
    {$ENDIF}
    for i := 1 to BndStackTop do
      if BandExists(BndStack[i]) then
        ShowBand(BndStack[i]);
    BndStackTop := 0;
    {$IFDEF DebugLR}
    DebugLnExit('ShowStack END');
    {$ENDIF}
  end;

  procedure DoLoop(Level: Integer);
  var
    WasPrinted: Boolean;
    b, b1, b2: TfrBand;
    BM       : Pointer;

    procedure InitGroups(b: TfrBand);
    begin
      while b <> nil do
      begin
        Inc(b.Positions[psLocal]);
        Inc(b.Positions[psGlobal]);
        ShowBand(b);
        b := b.Next;
      end;
    end;

  begin
    b := Bands[Bnds[Level, bpData]];
    {$IFDEF DebugLR}
    DebugLnEnter('Doop(Level=%d) INI b=%s mode=',[Level,bandinfo(b)]);
    {$ENDIF}
    while (b <> nil) and (b.Dataset <> nil) do
    begin
      b.ResetLastValues;
      try
        b.DataSet.First;

        //if Level<>1 then begin
        //  b.Dataset.Refresh;
        //end;

        if Mode = pmBuildList then
          AddRecord(b, rtFirst)
        else
          b.Positions[psLocal] := 1;

        b1 := Bands[btGroupHeader];
        while b1 <> nil do
        begin
          b1.Positions[psLocal] := 0;
          b1.Positions[psGlobal] := 0;
          b1 := b1.Next;
        end;

        if not b.DataSet.Eof then
        begin
          if (Level = 1) and HasGroups then
            InitGroups(Bands[btGroupHeader]);
          if b.HeaderBand <> nil then
            AddToStack(b.HeaderBand);
          if b.FooterBand <> nil then
            b.FooterBand.InitValues;

          while not b.DataSet.Eof do
          begin
            Application.ProcessMessages;
            if MasterReport.Terminated then
              break;
            AddToStack(b);
            WasPrinted := True;
            if Level < MaxLevel then
            begin
              DoLoop(Level + 1);
              if BndStackTop > 0 then
                if b.PrintIfSubsetEmpty then
                  ShowStack
                else
                begin
                  Dec(BndStackTop);
                  WasPrinted := False;
                end;
            end
            else
              ShowStack;

            if (Level = 1) and HasGroups then
            begin
              // get a bookmark to current record it will be used in case
              // a group change is detected and there are remaining group
              // footers.
              BookPrev := b.DataSet.GetBookMark;
              try
                b.DataSet.Next;
                b1 := Bands[btGroupHeader];
                while b1 <> nil do
                begin
                  curGroupValue := frParser.Calc(b1.GroupCondition);
                  {$IFDEF DebugLR}
                  DebugLn('GroupCondition=%s LastGroupValue=%s curGroupValue=%s',
                    [b1.GroupCondition,varstr(b1.LastGroupValue),varstr(curGroupValue)]);
                  {$ENDIF}
                  if (curGroupValue <> b1.LastGroupValue) or
                    b.Dataset.Eof then
                  begin
                    // next bands should be printed on the previous record context
                    b.DataSet.GotoBookMark(BookPrev);
                    ShowBand(b.FooterBand);
                    b2 := Bands[btGroupHeader].LastBand;
                    while b2 <> b1 do
                    begin
                      ShowBand(b2.FooterBand);
                      b2.Positions[psLocal] := 0;
                      b2 := b2.Prev;
                    end;
                    ShowBand(b1.FooterBand);
                    // advance to the actual current record
                    b.DataSet.Next;
                    if not b.Dataset.Eof then
                    begin
                      if b1.NewPageAfter then NewPage;
                      InitGroups(b1);
                      ShowBand(b.HeaderBand);
                      b.Positions[psLocal] := 0;
                    end;
                    b.ResetLastValues;
                    break;
                  end;
                  b1 := b1.Next;
                end;
              finally
                b.DataSet.FreeBookMark(BookPrev);
              end;
            end else
              b.DataSet.Next;

            if Mode = pmBuildList then
              AddRecord(b, rtNext)
            else if WasPrinted then
            begin
              Inc(CurPos);
              Inc(b.Positions[psGlobal]);
              Inc(b.Positions[psLocal]);
              if not b.DataSet.Eof and b.NewPageAfter then
              begin
                NewPage;
                b.ResetLastValues;
              end;
            end;
            if MasterReport.Terminated then
              break;
          end;
          if BndStackTop = 0 then
            ShowBand(b.FooterBand) else
            Dec(BndStackTop);
        end else
        if b.PrintIfSubsetEmpty then begin
          if b.HeaderBand <> nil then
            ShowBand(b.HeaderBand);
          if b.FooterBand <> nil then begin
            b.FooterBand.InitValues;
            ShowBand(b.FooterBand);
          end;
        end;
      finally
      end;
      b := b.Next;
    end;
    {$IFDEF DebugLR}
    DebugLnExit('Doop(Level=%d) END',[Level]);
    {$ENDIF}
  end;

begin
  {$IFDEF DebugLR}
  DebugLnEnter('TfrPage.FormPage INI Mode=%d',[ord(mode)]);
  {$ENDIF}
  if Mode = pmNormal then
  begin
    if Append then
    begin
      if PrevY = PrevBottomY then
      begin
        Append := False;
        WasPF := False;
        PageNo := MasterReport.EMFPages.Count;
      end;
    end;
    
    if Append and WasPF then
      CurBottomY := PrevBottomY
    else
      CurBottomY := BottomMargin;
      
    CurColumn := 0;
    XAdjust := LeftMargin;
    {$IFDEF DebugLR}
    DebugLn('XAdjust=%d CurBottomY=%d PrevY=%d',[XAdjust,CurBottomY,PrevY]);
    {$ENDIF}
    if not Append then
    begin
      MasterReport.EMFPages.Add(Self);
      CurY := TopMargin;
      ShowBand(Bands[btOverlay]);
      ShowBand(Bands[btNone]);
    end
    else
      CurY := PrevY;
    sfPage := PageNo;
    {$IFDEF DebugLR}
    DebugLn('XAdjust=%d CurY=%d sfPage=%d',[XAdjust,CurY,sfpage]);
    {$ENDIF}
    ShowBand(Bands[btReportTitle]);
    if PageNo = sfPage then // check if new page was formed
    begin
      if BandExists(Bands[btPageHeader]) and
        ((Bands[btPageHeader].Flags and flBandOnFirstPage) <> 0) then
        ShowBand(Bands[btPageHeader]);
      if not RowsLayout then
        ShowBand(Bands[btColumnHeader]);
    end;
  end;

  BndStackTop := 0;
  SetLength(BooksBkUp, 0);
  DetailCount := 0;
  for i := 1 to MAXBNDS do
  begin
    if BandExists(Bands[Bnds[i, bpData]]) then
    begin
      MaxLevel := i;
      BackupBookmark(Bands[Bnds[i, bpData]]);
    end;
  end;
  HasGroups := Bands[btGroupHeader].Objects.Count > 0;
  {$IFDEF DebugLR}
  DebugLn('GroupsCount=%d MaxLevel=%d doing DoLoop(1)',[
    Bands[btGroupHeader].Objects.Count, MaxLevel]);
  {$ENDIF}
  DisableControls;
  DoLoop(1);
  RestoreBookmarks; // this also enablecontrols
  if Mode = pmNormal then
  begin
    if not RowsLayout then
      ShowBand(Bands[btColumnFooter]);
    ShowBand(Bands[btReportSummary]);
    PrevY := CurY;
    PrevBottomY := CurBottomY;
    if CurColumn > 0 then
      PrevY := BottomMargin;
    CurColumn := 0;
    XAdjust := LeftMargin;
    sfPage := PageNo;
    WasPF := False;
    if (Bands[btPageFooter].Flags and flBandOnLastPage) <> 0 then
    begin
      WasPF := BandExists(Bands[btPageFooter]);
      if WasPF then
         DrawPageFooters;
    end;
    PageNo := sfPage + 1;
  end;
  {$IFDEF DebugLR}
  DebugLnExit('TfrPage.FormPage END PrevY=%d PrevBottomY=%d PageNo=%d XAdjust=%d',
    [PrevY,PrevBottomY,PageNo,XAdjust]);
  {$ENDIF}
end;

function TfrPage.BandExists(b: TfrBand): Boolean;
begin
  Result := b.Objects.Count > 0;
end;

procedure TfrPage.AfterPrint;
var
  i: Integer;
begin
  for i := 0 to HookList.Count - 1 do
    TfrView(HookList[i]).OnHook(CurView);
end;

procedure TfrPage.LoadFromStream(Stream: TStream);
var
  b: Byte;
  s: String[6];
  Bool : WordBool;
  Rc   : TRect;
begin
  with Stream do
  begin
    Read(pgSize, 4);
    Read(dx, 4); //Width
    Read(dy, 4); //Height
    Read(Rc, Sizeof(Rc));
    Margins.AsRect:=Rc;
    Read(b, 1);
    Orientation:=TPrinterOrientation(b);
    if frVersion < 23 then
      Read(s[1], 6);
    Read(Bool, 2);
    PrintToPrevPage:=Bool;
    Read(Bool, 2);
    UseMargins:=Bool;
    Read(fColCount, 4);
    Read(fColGap, 4);
    if frVersion>23 then
      Read(ord(PageType), SizeOf(TfrPageType));
    Read(fLayoutOrder, 4);
  end;
  ChangePaper(pgSize, Width, Height, Orientation);
end;

procedure TfrPage.LoadFromXML(XML: TLrXMLConfig; const Path: String);
var
  b:byte;
begin
  inherited LoadFromXML(XML,Path);
  
  dx := XML.GetValue(Path+'Width/Value', 0); // TODO chk
  dy := XML.GetValue(Path+'Height/Value', 0); // TODO chk
  b := XML.GetValue(Path+'Height/PageType', ord(PageType));
  PageType:=TfrPageType(b);
end;

procedure TfrPage.SaveToStream(Stream: TStream);
var
  b: Byte;
  Bool : WordBool;
  Rc   : TRect;
begin
  with Stream do
  begin
    Write(pgSize, 4);
    Write(Width, 4);
    Write(Height, 4);
    Rc:=Margins.AsRect;
    Write(Rc, Sizeof(Rc));
    b := Byte(Orientation);
    Write(b, 1);
    Bool:=PrintToPrevPage;
    Write(Bool, 2);
    Bool:=UseMargins;
    Write(Bool, 2);
    Write(ColCount, 4);
    Write(ColGap, 4);
    Write(ord(PageType), SizeOf(TfrPageType));
    Write(LayoutOrder, 4);
  end;
end;

procedure TfrPage.SavetoXML(XML: TLrXMLConfig; const Path: String);
begin
  Inherited SavetoXML(XML,Path);
  XML.SetValue(Path+'Width/Value', Width);
  XML.SetValue(Path+'Height/Value', Height);
  XML.SetValue(Path+'Height/PageType', ord(PageType));
end;

{-----------------------------------------------------------------------}
constructor TfrPages.Create(AParent: TfrReport);
begin
  inherited Create;
  Parent := AParent;
  FPages := TFpList.Create;
end;

destructor TfrPages.Destroy;
begin
  Clear;
  FPages.Free;
  inherited Destroy;
end;

function TfrPages.GetCount: Integer;
begin
  Result := FPages.Count;
end;

function TfrPages.GetPages(Index: Integer): TfrPage;
begin
  Result :=TfrPage(FPages[Index]);
end;

procedure TfrPages.Clear;
var
  i: Integer;
begin
  for i := 0 to FPages.Count - 1 do
    Pages[i].Free;
  FPages.Clear;
end;

procedure TfrPages.Add(const aClassName : string='TfrPageReport');
Var Pg : TFrPage;
    Rf : TFrPageClass;
begin
  Pg:=nil;
  
  Rf:=TFrPageClass(GetClass(aClassName));
  if Assigned(Rf) then
  begin
    Pg:=Rf.CreatePage;
    
    if Assigned(Pg) then
    begin
      Pg.CreateUniqueName;
      FPages.Add(Pg);
    end;
  end
  else showMessage(Format('Class %s not found',[aClassName]))
end;

procedure TfrPages.Delete(Index: Integer);
begin
  Pages[Index].Free;
  FPages.Delete(Index);
end;

procedure TfrPages.LoadFromStream(Stream: TStream);
var
  b: Byte;
  t: TfrView;
  s: String;
  buf: String[8];

  procedure AddObject(ot: Byte; clname: String);
  begin
    Stream.Read(b, 1);
    Pages[b].Objects.Add(frCreateObject(ot, clname));
    t :=TfrView(Pages[b].Objects.Items[Pages[b].Objects.Count - 1]);
  end;

begin
  Clear;
  Stream.Read(Parent.PrintToDefault, 2);
  Stream.Read(Parent.DoublePass, 2);
  Parent.SetPrinterTo(ReadString(Stream));
  while Stream.Position < Stream.Size do
  begin
    {$IFDEF DebugLR}
    DebugLn('TfrPages.LoadFromStream');
    {$ENDIF}

    Stream.Read(b, 1);
    if b = $FF then  // page info
    begin
      if frVersion>23 then
      begin
        S:=ReadString(Stream);
        Add(S);
      end
      else
        Add;
      Pages[Count - 1].LoadFromStream(Stream);
    end
    else if b = $FE then // values
    begin
      Parent.FVal.ReadBinaryData(Stream);
      ReadMemo(Stream, SMemo);
      Parent.Variables.Assign(SMemo);
    end
    else if b = $FD then // datasets
    begin
      if frDataManager <> nil then
        frDataManager.LoadFromStream(Stream);
      break;
    end
    else
    begin
      if b > Integer(gtAddIn) then
      begin
        raise Exception.Create('');
        break;
      end;
      s := '';
      if b = gtAddIn then
      begin
        s := ReadString(Stream);
        if AnsiUpperCase(s) = 'TFRFRAMEDMEMOVIEW' then
          AddObject(gtMemo, '')
        else
          AddObject(gtAddIn, s);
      end
      else
        AddObject(b, '');
      t.LoadFromStream(Stream);
      if AnsiUpperCase(s) = 'TFRFRAMEDMEMOVIEW' then
        Stream.Read(buf[1], 8);
    end;
  end;
end;

procedure TfrPages.LoadFromXML(XML: TLrXMLConfig; const Path: String);
var
  b: Byte;
  t: TfrView;
  s: string;
  procedure AddObject(aPage: TFrPage; ot: Byte; clname: String);
  begin
    aPage.Objects.Add(frCreateObject(ot, clname));
    t :=TfrView(aPage.Objects.Items[aPage.Objects.Count - 1]);
  end;
var
  i,j,aCount,oCount: Integer;
  aTyp: byte;
  aPath,aSubPath,clName: string;
begin
  Clear;
  {$IFDEF DebugLR}
  DebugLn('TfrPages.LoadFromXML: LoadingFrom: ', Path);
  {$ENDIF}
  Parent.PrintToDefault:= XML.GetValue(Path+'PrintToDefault/Value', True);
  Parent.DoublePass :=    XML.GetValue(Path+'DoublePass/Value', False); // TODO: check default
  clName :=               XML.GetValue(Path+'SelectedPrinter/Value','');
  Parent.SetPrinterTo(clName); // TODO: check default
  aCount := XML.GetValue(Path+'PageCount/Value', 0);
  for i := 0 to aCount - 1 do // adding pages at first
  begin
    aPath := Path+'Page'+IntToStr(i+1)+'/';
    clname:= XML.GetValue(aPath+'ClassName/Value', 'TFRPAGEREPORT');
    add(clName);
    
    Pages[i].LoadFromXML(XML, aPath);
    oCount := XML.GetValue(aPath+'ObjectCount/Value', 0);
    for j:=0 to oCount - 1 do
    begin
      aSubPath := aPath + 'Object'+IntTostr(j+1)+'/';
      aTyp := StrTofrTypeObject(XML.GetValue(aSubPath+'Typ/Value', '0'));
      if aTyp>gtAddin then
        raise Exception.Create('');
      clname := XML.GetValue(aSubPath+'ClassName/Value', 'TFRVIEW'); // TODO: Check default
      if aTyp=gtAddin then
      begin
        if ansiuppercase(clname)='TFRFRAMEDMEMOVIEW' then
          addObject(Pages[i], gtMemo, '')
        else
          addObject(Pages[i], gtAddin, clName)
      end else
        AddObject(Pages[i], aTyp, '');
      t.LoadFromXML(XML, aSubPath);
    end;
  end;
  Parent.FVal.ReadBinaryDataFromXML(XML, Path+'FVal/');
  Parent.Variables.Text:= XML.GetValue(Path+'ParentVars/Value', '' );
  if frDataManager<>nil then
    frDatamanager.LoadFromXML(XML, Path+'Datamanager/');
end;

procedure TfrPages.SaveToStream(Stream: TStream);
var
  b: Byte;
  i, j: Integer;
  t: TfrView;
  S:string;
begin
  Stream.Write(Parent.PrintToDefault, 2);
  Stream.Write(Parent.DoublePass, 2);
  frWriteString(Stream, Prn.Printers[Prn.PrinterIndex]);
  for i := 0 to Count - 1 do // adding pages at first
  begin
    b := $FF;
    Stream.Write(b, 1);      // page info
    frWriteString(Stream, Pages[i].Classname);
    Pages[i].SaveToStream(Stream);
  end;
  for i := 0 to Count - 1 do
  begin
    for j := 0 to Pages[i].Objects.Count - 1 do // then adding objects
    begin
      t :=TfrView(Pages[i].Objects[j]);
      b := Byte(t.Typ);
      Stream.Write(b, 1);
      if t.Typ = gtAddIn then
        frWriteString(Stream, t.ClassName);
      Stream.Write(i, 1);
      t.SaveToStream(Stream);
    end;
  end;
  b := $FE;
  Stream.Write(b, 1);
  Parent.FVal.WriteBinaryData(Stream);
  SMemo.Assign(Parent.Variables);
  frWriteMemo(Stream, SMemo);
  if frDataManager <> nil then
  begin
    b := $FD;
    Stream.Write(b, 1);
    frDataManager.SaveToStream(Stream);
  end;
end;

procedure TfrPages.SavetoXML(XML: TLrXMLConfig; const Path: String);
var
  b: Byte;
  i, j: Integer;
  t: TfrView;
  aPath,aSubPath: String;
begin
  XML.SetValue(Path+'PrintToDefault/Value', Parent.PrintToDefault);
  XML.SetValue(Path+'DoublePass/Value', Parent.DoublePass);
  XML.SetValue(Path+'SelectedPrinter/Value', Prn.Printers[Prn.PrinterIndex]);
  XML.SetValue(Path+'PageCount/Value', Count);
  for i := 0 to Count - 1 do // adding pages at first
  begin
    aPath := Path+'Page'+IntToStr(i+1)+'/';
    Pages[i].SaveToXML(XML, aPath);
    XML.SetValue(aPath+'ObjectCount/Value', Pages[i].Objects.count);
    for j:=0 to Pages[i].Objects.count - 1 do
    begin
      aSubPath := aPath + 'Object'+IntTostr(j+1)+'/';
      T := TfrView(Pages[i].Objects[j]);
      T.SaveToXML(XML, aSubPath);
    end;
  end;
  Parent.FVal.WriteBinaryDataToXML(XML, Path+'FVal/');
  XML.SetValue(Path+'ParentVars/Value',Parent.Variables.Text);
  if frDataManager <> nil then
  begin
    frDataManager.SaveToXML(XML, Path+'Datamanager/');
  end;
end;

{-----------------------------------------------------------------------}
constructor TfrEMFPages.Create(AParent: TfrReport);
begin
  inherited Create;
  Parent := AParent;
  FPages := TFpList.Create;
end;

destructor TfrEMFPages.Destroy;
begin
  Clear;
  FPages.Free;
  inherited Destroy;
end;

function TfrEMFPages.GetCount: Integer;
begin
  Result := FPages.Count;
end;

function TfrEMFPages.GetPages(Index: Integer): PfrPageInfo;
begin
  Result := FPages[Index];
end;

procedure TfrEMFPages.Clear;
begin
  while FPages.Count > 0 do
    Delete(0);
end;

procedure TfrEMFPages.Draw(Index: Integer; Canvas: TCanvas; DrawRect: TRect);
var
  p: PfrPageInfo;
  t: TfrView;
  i: Integer;
  sx, sy: Double;
  v, IsPrinting: Boolean;
  h: THandle;
begin
  IsPrinting := Printer.Printing and (Canvas is TPrinterCanvas);
  {$IFDEF DebugLR}
  DebugLn('TfrEMFPages.Draw IsPrinting=%d PageIndex=%d Canvas.ClassName=%s '+
          'CanvasPPI=%d',[ord(IsPrinting), Index, Canvas.ClassName,
          Canvas.Font.pixelsPerInch]);
  {$ENDIF}

  DocMode := dmPrinting;
  p := FPages[Index];
  with p^ do
  begin
    if Visible then
    begin
      if Page = nil then
        ObjectsToPage(Index);
        
      sx:=(DrawRect.Right-DrawRect.Left)/PrnInfo.PgW;
      sy:=(DrawRect.Bottom-DrawRect.Top)/PrnInfo.PgH;
      h:= Canvas.Handle;

      for i := 0 to Page.Objects.Count - 1 do
      begin
        t :=TfrView(Page.Objects[i]);
        v := True;

        if not IsPrinting then
        begin
          with t, DrawRect do
          begin
            v := RectVisible(h, Rect(Round(x * sx) + Left - 10,
                                     Round(y * sy) + Top - 10,
                                     Round((x + dx) * sx) + Left + 10,
                                     Round((y + dy) * sy) + Top + 10));
          end;
        end;

        if v then
        begin
          t.ScaleX := sx;
          t.ScaleY := sy;
          t.OffsX := DrawRect.Left;
          t.OffsY := DrawRect.Top;
          t.IsPrinting := IsPrinting;
          t.Draw(Canvas);
        end;
      end;
    end
    else
    begin
      Page.Free;
      Page := nil;
    end;
  end;
end;

procedure TfrEMFPages.ExportData(Index: Integer);
var
  p: PfrPageInfo;
  b: Byte;
  t: TfrView;
  s: String;
begin
  p := FPages[Index];
  with p^ do
  begin
    Stream.Position := 0;
    Stream.Read(frVersion, 1);
    while Stream.Position < Stream.Size do
    begin
      Stream.Read(b, 1);
      if b = gtAddIn then
        s := ReadString(Stream) else
        s := '';
      t := frCreateObject(b, s);
      t.StreamMode := smPrinting;
      t.LoadFromStream(Stream);
      t.ExportData;
      t.Free;
    end;
  end;
end;

procedure TfrEMFPages.ObjectsToPage(Index: Integer);
var
  p: PfrPageInfo;
  b: Byte;
  t: TfrView;
  s: String;
begin
  p := FPages[Index];
  with p^ do
  begin
    if Page <> nil then
      Page.Free;
    Page := TfrPageReport.Create(pgSize, pgWidth, pgHeight, pgOr);
    Stream.Position := 0;
    Stream.Read(frVersion, 1);
    while Stream.Position < Stream.Size do
    begin
      Stream.Read(b, 1);
      if b = gtAddIn then
        s := ReadString(Stream)
      else
        s := '';
      t := frCreateObject(b, s);
      t.StreamMode := smPrinting;
      t.LoadFromStream(Stream);
      t.StreamMode := smDesigning;
      Page.Objects.Add(t);
    end;
  end;
end;

procedure TfrEMFPages.PageToObjects(Index: Integer);
var
  i: Integer;
  p: PfrPageInfo;
  t: TfrView;
begin
  p := FPages[Index];
  with p^ do
  begin
    Stream.Clear;
    frVersion := frCurrentVersion;
    Stream.Write(frVersion, 1);
    for i := 0 to Page.Objects.Count - 1 do
    begin
      t :=TfrView(Page.Objects[i]);
      t.StreamMode := smPrinting;
      Stream.Write(t.Typ, 1);
      if t.Typ = gtAddIn then
        frWriteString(Stream, t.ClassName);
      t.Memo1.Assign(t.Memo);
      t.SaveToStream(Stream);
    end;
  end;
end;

procedure TfrEMFPages.Insert(Index: Integer; APage: TfrPage);
var
  p: PfrPageInfo;
begin
  GetMem(p, SizeOf(TfrPageInfo));
  FillChar(p^, SizeOf(TfrPageInfo), 0);
  if Index >= FPages.Count then
    FPages.Add(p)
  else
    FPages.Insert(Index, p);
    
  with p^ do
  begin
    Stream := TMemoryStream.Create;
    frVersion := frCurrentVersion;
    Stream.Write(frVersion, 1);
    pgSize := APage.pgSize;
    pgWidth := APage.Width;
    pgHeight := APage.Height;
    pgOr := APage.Orientation;
    pgMargins := APage.UseMargins;
    PrnInfo := APage.PrnInfo;
  end;
end;

procedure TfrEMFPages.Add(APage: TfrPage);
begin
  Insert(FPages.Count, APage);
  if (CurReport <> nil) and Assigned(CurReport.FOnBeginPage) then
    CurReport.FOnBeginPage(PageNo);
  if (MasterReport <> CurReport) and (MasterReport <> nil) and
    Assigned(MasterReport.FOnBeginPage) then
    MasterReport.FOnBeginPage(PageNo);
end;

procedure TfrEMFPages.Delete(Index: Integer);
begin
  if Pages[Index]^.Page <> nil then Pages[Index]^.Page.Free;
  if Pages[Index]^.Stream <> nil then Pages[Index]^.Stream.Free;
  FreeMem(Pages[Index], SizeOf(TfrPageInfo));
  FPages.Delete(Index);
end;

procedure TfrEMFPages.LoadFromStream(AStream: TStream);
var
  i, o, c: Integer;
  b, compr: Byte;
  p: PfrPageInfo;
  s: TMemoryStream;

  procedure ReadVersion22;
  var
    Pict: TfrPictureView;
  begin
    frReadMemo22(AStream, SMemo);
    if SMemo.Count > 0 then
      Parent.SetPrinterTo(SMemo[0]);
    AStream.Read(c, 4);
    i := 0;
    repeat
      AStream.Read(o, 4);
      GetMem(p, SizeOf(TfrPageInfo));
      FillChar(p^, SizeOf(TfrPageInfo), 0);
      FPages.Add(p);
      with p^ do
      begin
        AStream.Read(pgSize, 2);
        AStream.Read(pgWidth, 4);
        AStream.Read(pgHeight, 4);
        AStream.Read(b, 1);
        pgOr := TPrinterOrientation(b);
        AStream.Read(b, 1);
        pgMargins := Boolean(b);
        Prn.SetPrinterInfo(pgSize, pgWidth, pgHeight, pgOr);
        Prn.FillPrnInfo(PrnInfo);

        Pict := TfrPictureView.Create;
        Pict.SetBounds(0, 0, PrnInfo.PgW, PrnInfo.PgH);
        Pict.Picture.Graphic.LoadFromStream(AStream);

        Stream := TMemoryStream.Create;
        b := frCurrentVersion;
        Stream.Write(b, 1);
        Pict.StreamMode := smPrinting;
        Stream.Write(Pict.Typ, 1);
        Pict.SaveToStream(Stream);
        Pict.Free;
      end;
      AStream.Seek(o, soFromBeginning);
      Inc(i);
    until i >= c;
  end;

begin
  Clear;
  AStream.Read(compr, 1);
  if not (compr in [0, 1, 255]) then
  begin
    AStream.Seek(0, soFromBeginning);
    ReadVersion22;
    Exit;
  end;
  AddPagesFromStream(AStream, false);
end;

procedure TfrEMFPages.AddPagesFromStream(AStream: TStream;
  AReadHeader: boolean=true);
var
  i, o, c: Integer;
  b, compr: Byte;
  p: PfrPageInfo;
  s: TMemoryStream;

begin
  if AReadHeader then begin
    AStream.Read(compr, 1);
    if not (compr in [0, 1, 255]) then
    begin
      Exit;
    end;
  end;
  Parent.SetPrinterTo(frReadString(AStream));
  AStream.Read(c, 4);
  i := 0;
  repeat
    AStream.Read(o, 4);
    GetMem(p, SizeOf(TfrPageInfo));
    FillChar(p^, SizeOf(TfrPageInfo), #0);
    FPages.Add(p);
    with p^ do
    begin
      AStream.Read(pgSize, 2);
      AStream.Read(pgWidth, 4);
      AStream.Read(pgHeight, 4);
      AStream.Read(b, 1);
      pgOr := TPrinterOrientation(b);
      AStream.Read(b, 1);
      pgMargins := Boolean(b);
      if compr <> 0 then
      begin
        s := TMemoryStream.Create;
        s.CopyFrom(AStream, o - AStream.Position);
        Stream := TMemoryStream.Create;
        frCompressor.DeCompress(s, Stream);
        s.Free;
      end
      else
      begin
        Stream := TMemoryStream.Create;
        Stream.CopyFrom(AStream, o - AStream.Position);
      end;
      Prn.SetPrinterInfo(pgSize, pgWidth, pgHeight, pgOr);
      Prn.FillPrnInfo(PrnInfo);
    end;
    AStream.Seek(o, soFromBeginning);
    Inc(i);
  until i >= c;
end;

procedure TfrEMFPages.LoadFromXML(XML: TLrXMLConfig; const Path: String);
begin
  // todo
end;

procedure TfrEMFPages.SaveToStream(AStream: TStream);
var
  i, o, n: Integer;
  b: Byte;
  s: TMemoryStream;
begin
  b := Byte(frCompressor.Enabled);
  AStream.Write(b, 1);
  frWriteString(AStream, Prn.Printers[Prn.PrinterIndex]);
  n := Count;
  AStream.Write(n, 4);
  i := 0;
  repeat
    o := AStream.Position;
    AStream.Write(o, 4); // dummy write
    with Pages[i]^ do
    begin
      AStream.Write(pgSize, 2);
      AStream.Write(pgWidth, 4);
      AStream.Write(pgHeight, 4);
      b := Byte(pgOr);
      AStream.Write(b, 1);
      b := Byte(pgMargins);
      AStream.Write(b, 1);
      Stream.Position := 0;
      if frCompressor.Enabled then
      begin
        s := TMemoryStream.Create;
        frCompressor.Compress(Stream, s);
        AStream.CopyFrom(s, s.Size);
        s.Free;
      end
      else
        AStream.CopyFrom(Stream, Stream.Size);
    end;
    n := AStream.Position;
    AStream.Seek(o, soFromBeginning);
    AStream.Write(n, 4);
    AStream.Seek(0, soFromEnd);
    Inc(i);
  until i >= Count;
end;

procedure TfrEMFPages.SaveToXML(XML: TLrXMLConfig; const Path: String);
begin
  // Todo
end;

{-----------------------------------------------------------------------}
constructor TfrValues.Create;
begin
  inherited Create;
  FItems := TStringList.Create;
end;

destructor TfrValues.Destroy;
begin
  Clear;
  FItems.Free;
  inherited Destroy;
end;

procedure TfrValues.WriteBinaryData(Stream: TStream);
var
  i, n: Integer;

  procedure WriteStr(s: String);
  var
    n: Byte;
  begin
    n := Length(s);
    Stream.Write(n, 1);
    Stream.Write(s[1], n);
  end;

begin
  with Stream do
  begin
    n := FItems.Count;
    WriteBuffer(n, SizeOf(n));
    for i := 0 to n - 1 do
    with Objects[i] do
    begin
      WriteBuffer(Typ, SizeOf(Typ));
      WriteBuffer(OtherKind, SizeOf(OtherKind));
      WriteStr(DataSet);
      WriteStr(Field);
      WriteStr(FItems[i]);
    end;
  end;
end;

procedure TfrValues.WriteBinaryDataToXML(XML: TLrXMLConfig; const Path: String);
var
  i: integer;
  aSubPath: String;
begin
  XML.SetValue(Path+'Count/Value', FItems.Count);
  for i:= 0 to FItems.Count-1 do
  with Objects[i] do
  begin
    aSubPath := Path+'Objects'+InttoStr(i+1)+'/';
    XML.SetValue(aSubPath+'Typ/Value', Ord(Typ));
    XML.SetValue(aSubPath+'OtherKind/Value', OtherKind);
    XML.SetValue(aSubPath+'Dataset/Value', DataSet);
    XML.SetValue(aSubPath+'Field/Value', Field);
    XML.SetValue(aSubPath+'Item/Value', FItems[i]);
  end;
end;

procedure TfrValues.ReadBinaryData(Stream: TStream);
var
  i, j, n: Integer;

  function ReadStr: String;
  var
    n: Byte;
  begin
    Stream.Read(n, 1);
    SetLength(Result, n);
    Stream.Read(Result[1], n);
  end;

begin
  Clear;
  FItems.Sorted := False;
  with Stream do
  begin
    ReadBuffer(n, SizeOf(n));
    for i := 0 to n - 1 do
    begin
      j := AddValue;
      with Objects[j] do
      begin
        ReadBuffer(Typ, SizeOf(Typ));
        ReadBuffer(OtherKind, SizeOf(OtherKind));
        DataSet := ReadStr;
        Field := ReadStr;
        FItems[j] := ReadStr;
      end;
    end;
  end;
end;

procedure TfrValues.ReadBinaryDataFromXML(XML: TLrXMLConfig; const Path: String);
var
  i,j,n: Integer;
  aSubPath: String;
begin
  clear;
  FItems.Sorted := False;
  n := XML.GetValue(Path+'Count/Value', 0);
  for i:= 0 to n - 1 do
  begin
    j := AddValue;
    with Objects[j] do
    begin
      aSubPath := Path+'Objects'+InttoStr(i+1)+'/';
      Typ := TfrValueType(XML.GetValue(aSubPath+'Typ/Value', 0)); // TODO check default value
      OtherKind := XML.GetValue( aSubPath+'OtherKind/Value', 0); // TODO check default value
      DataSet := XML.GetValue(aSubPath+'Dataset/Value', ''); // TODO check default value
      Field := XML.GetValue(aSubPath+'Field/Value', ''); // TODO check default value
      FItems[j] := XML.GetValue(aSubPath+'Item/Value', ''); // TODO check default value
    end;
  end;
end;

function TfrValues.GetValue(Index: Integer): TfrValue;
begin
  Result := TfrValue(FItems.Objects[Index]);
end;

function TfrValues.AddValue: Integer;
begin
  Result := FItems.AddObject('', TfrValue.Create);
end;

procedure TfrValues.Clear;
var
  i: Integer;
begin
  for i := 0 to FItems.Count - 1 do
    TfrValue(FItems.Objects[i]).Free;
  FItems.Clear;
end;

function TfrValues.FindVariable(const s: String): TfrValue;
var
  i: Integer;
begin
  Result := nil;
  i := FItems.IndexOf(s);
  if i <> -1 then
    Result := Objects[i];
end;

{----------------------------------------------------------------------------}
constructor TfrReport.Create(AOwner: TComponent);
const
  Clr: Array[0..1] of TColor = (clWhite, clSilver);

var
  j: Integer;
  i: Integer;
begin
  inherited Create(AOwner);
  if not FRInitialized then
  begin
    FRInitialized := True;
    SBmp := TBitmap.Create;
    TempBmp := TBitmap.Create;
    SBmp.Width := 8;
    SBmp.Height := 8;
    TempBmp.Width := 8;
    TempBmp.Height := 8;
    for j := 0 to 7 do
    begin
      for i := 0 to 7 do
        SBmp.Canvas.Pixels[i, j] := Clr[(j + i) mod 2];
    end;
    frProgressForm := TfrProgressForm.Create(nil);
  end;

  FPages := TfrPages.Create(Self);
  FEMFPages := TfrEMFPages.Create(Self);
  FVars := TStringList.Create;
  FVal := TfrValues.Create;
  FShowProgress := True;
  FModalPreview := True;
  FModifyPrepared := True;
  FPreviewButtons := [pbZoom, pbLoad, pbSave, pbPrint, pbFind, pbHelp, pbExit];
  FInitialZoom := pzDefault;
  FileName := sUntitled;
  FComments:=TStringList.Create;
  UpdateObjectStringResources;
end;

destructor TfrReport.Destroy;
begin
  if CurReport=Self then
    CurReport:=nil;
  FVal.Free;
  FVars.Free;
  FEMFPages.Free;
  FEMFPages := nil;
  FPages.Free;
  FComments.Free;
  inherited Destroy;
end;

procedure TfrReport.Clear;
begin
  Pages.Clear;
  if frDataManager <> nil then
    frDataManager.Clear;
  DoublePass := False;
  ClearAttribs;
  DocMode := dmDesigning;
end;

procedure TfrReport.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('ReportForm', @ReadBinaryData, @WriteBinaryData, True);
end;

procedure TfrReport.WriteBinaryData(Stream: TStream);
var
  n: Integer;
  Stream1: TMemoryStream;
begin
  n := frCurrentVersion;
  Stream.Write(n, 4);
  if FStoreInDFM then
  begin
    Stream1 := TMemoryStream.Create;
    SaveToStream(Stream1);
    Stream1.Position := 0;
    n := Stream1.Size;
    Stream.Write(n, 4);
    Stream.CopyFrom(Stream1, n);
    Stream1.Free;
  end;
end;

procedure TfrReport.ReadBinaryData(Stream: TStream);
var
  n: Integer;
begin
  Stream.Read(n, 4); // version
  if FStoreInDFM then
  begin
    Stream.Read(n, 4);
    FDFMStream := TMemoryStream.Create;
    FDFMStream.CopyFrom(Stream, n);
    FDFMStream.Position := 0;
  end;
end;

procedure TfrReport.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Dataset) then
    Dataset := nil;
  if (Operation = opRemove) and (AComponent = Preview) then
    Preview := nil;
end;

// report building events
procedure TfrReport.InternalOnProgress(Percent: Integer);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Percent)
  else
    if fShowProgress then
    begin
      with frProgressForm do
      begin
        if (MasterReport.DoublePass and MasterReport.FinalPass) or
            (FCurrentFilter <> nil) then
          Label1.Caption:=Format('%s %d %s %d',[FirstCaption,Percent,sFrom,SavedAllPages])
        else
          Label1.Caption:=Format('%s %d',[FirstCaption,Percent]);
          
        Application.ProcessMessages;
      end;
    end;
end;

function CopyVarString(V: Variant): String;
begin
  Result := pchar(TVarData(V).VString)
end;

procedure TfrReport.InternalOnGetValue(ParName: String; var ParValue: String);
var
  i, j, AFormat: Integer;
  AFormatStr: String;
  V : Variant;
  ValStr: String;
begin
  SubValue := '';
  AFormat := CurView.Format;
  AFormatStr := CurView.FormatStr;
  i := Pos(' #', ParName);
  if i <> 0 then
  begin
    AFormatStr := Copy(ParName, i + 2, Length(ParName) - i - 1);
    ParName := Copy(ParName, 1, i - 1);

    if AFormatStr[1] in ['0'..'9', 'N', 'n'] then
    begin
      if AFormatStr[1] in ['0'..'9'] then
        AFormatStr := 'N' + AFormatStr;
      AFormat := $01000000;
      if AFormatStr[2] in ['0'..'9'] then
        AFormat := AFormat + $00010000;
      i := Length(AFormatStr);
      while i > 1 do
      begin
        if AFormatStr[i] in ['.', ',', '-'] then
        begin
          AFormat := AFormat + Ord(AFormatStr[i]);
          AFormatStr[i] := '.';
          if AFormatStr[2] in ['0'..'9'] then
          begin
            Inc(i);
            j := i;
            while (i <= Length(AFormatStr)) and (AFormatStr[i] in ['0'..'9']) do
              Inc(i);
            AFormat := AFormat + 256 * StrToInt(Copy(AFormatStr, j, i - j));
          end;
          break;
        end;
        Dec(i);
      end;
      if not (AFormatStr[2] in ['0'..'9']) then
      begin
        AFormatStr := Copy(AFormatStr, 2, 255);
        AFormat := AFormat + $00040000;
      end;
    end
    else if AFormatStr[1] in ['D', 'T', 'd', 't'] then
    begin
      AFormat := $02040000;
      AFormatStr := Copy(AFormatStr, 2, 255);
    end
    else if AFormatStr[1] in ['B', 'b'] then
    begin
      AFormat := $04040000;
      AFormatStr := Copy(AFormatStr, 2, 255);
    end;
  end;

  CurVariable := ParName;
  CurValue := 0;
  GetVariableValue(ParName, CurValue);
  ParValue := FormatValue(CurValue, AFormat, AFormatStr);
  {
  if TVarData(CurValue).VType=varString then
    ValStr := CopyVarString(CurValue)
  else
    ValStr := CurValue;
  ParValue := FormatValueStr(ValStr, Format, FormatStr);
  }
  {$IFDEF DebugLR}
  DebugLn('TfrReport.InternalOnGetValue(%s) Value=%s',[ParName,ParValue]);
  {$ENDIF}
end;

procedure TfrReport.InternalOnEnterRect(Memo: TStringList; View: TfrView);
begin
  {$IFDEF DebugLR}
  DebugLn('TfrReport.InternalOnEnterRect View=%s',[ViewInfo(View)]);
  {$ENDIF}
  with View do
    if (FDataSet <> nil) and frIsBlob(TfrTField(FDataSet.FindField(FField))) then
      GetBlob(TfrTField(FDataSet.FindField(FField)));
  if Assigned(FOnEnterRect) then
     FOnEnterRect(Memo, View);
end;

procedure TfrReport.InternalOnExportData(View: TfrView);
begin
  FCurrentFilter.OnData(View.x, View.y, View);
end;

procedure TfrReport.InternalOnExportText(x, y: Integer; const text: String;
  View: TfrView);
begin
  FCurrentFilter.OnText(x, y, text, View);
end;

procedure TfrReport.InternalOnBeginColumn(Band: TfrBand);
begin
  if Assigned(FOnBeginColumn) then FOnBeginColumn(Band);
end;

procedure TfrReport.InternalOnPrintColumn(ColNo: Integer; var ColWidth: Integer);
begin
  if Assigned(FOnPrintColumn) then FOnPrintColumn(ColNo, ColWidth);
end;

function TfrReport.FormatValue(V: Variant;
  AFormat: Integer;
  const AFormatStr: String): String;
var
  f1, f2: Integer;
  c: Char;
  s: String;
begin
  if (TVarData(v).VType = varEmpty) {VarIsEmpty(v)} or VarIsNull(v) then
  begin
    Result := ' ';
    Exit;
  end;
  
  c := DecimalSeparator;
  f1 := (AFormat div $01000000) and $0F;
  f2 := (AFormat div $00010000) and $FF;
  try
    case f1 of
      fmtText:
        begin
          if VarIsType(v, varDate) and (trunc(Extended(v))=0) then
          begin
            Result := TimeToStr(v);
            if Result='' then
              Result := FormatDateTime('hh:nn:ss', v);
          end
          else
            Result := v;
        end;
      fmtNumber:
        begin
          DecimalSeparator := Chr(AFormat and $FF);
          case f2 of
            0: Result := FormatFloat('###.##', v);
            1: Result := FloatToStrF(Extended(v), ffFixed, 15, (AFormat div $0100) and $FF);
            2: Result := FormatFloat('#,###.##', v);
            3: Result := FloatToStrF(Extended(v), ffNumber, 15, (AFormat div $0100) and $FF);
            4: Result := FormatFloat(AFormatStr, v);
          end;
        end;
      fmtDate:
         if f2 = 4 then
           Result := SysToUTF8(FormatDateTime(AFormatStr, v))
         else
           Result := FormatDateTime(frDateFormats[f2], v);
      fmtTime:
         if f2 = 4 then
           Result := FormatDateTime(AFormatStr, v)
         else
           Result := FormatDateTime(frTimeFormats[f2], v);
      fmtBoolean :
         begin
           if f2 = 4 then
             s := AFormatStr
           else
             s := BoolStr[f2];
           if Integer(v) = 0 then
             Result := Copy(s, 1, Pos(';', s) - 1)
           else
             Result := Copy(s, Pos(';', s) + 1, 255);
         end;
    end;
  except
    on e:exception do
      Result := v;
  end;
  DecimalSeparator := c;
end;

{
function TfrReport.GetLRTitle: String;
begin
  if csDesigning in ComponentState then
    Result:=fDefaultTitle
  else
  begin
    if fTitle<>'' then
      Result:=fTitle
    else
      Result:=fDefaultTitle;
  end;
end;
}
procedure TfrReport.GetVariableValue(const s: String; var aValue: Variant);
var
  Value: TfrValue;
  D: TfrTDataSet;
  F: TfrTField;

  function MasterBand: TfrBand;
  begin
    Result := CurBand;
    if Result.DataSet = nil then
      while Result.Prev <> nil do
        Result := Result.Prev;
  end;

begin
  TVarData(aValue).VType := varEmpty;

  if Assigned(FOnGetValue) then
     FOnGetValue(s,aValue);
     
  if TVarData(aValue).VType = varEmpty then
  begin
    Value := Values.FindVariable(s);
    if Assigned(Value) then
    begin
      with Value do
      begin
         case Typ of
          vtNotAssigned: aValue := '';
          vtDBField    : begin
                            F := TfrTField(DSet.FindField(Field));
                            if not F.DataSet.Active then
                              F.DataSet.Open;
                            if Assigned(F.OnGetText) then
                              aValue:=F.DisplayText
                            else
                              aValue:=F.AsVariant;
                          end;
          vtFRVar       : aValue := frParser.Calc(Field);
          vtOther       : begin
                            if OtherKind = 1 then
                              aValue:=frParser.Calc(Field)
                            else
                              aValue:=frParser.Calc(frSpecFuncs[OtherKind]);
                          end;
         end;
      end;
    end
    else
    begin
      D := GetDefaultDataSet;
      frGetDataSetAndField(s, D, F);
      if F <> nil then
      begin
        if not F.DataSet.Active then
          F.DataSet.Open;
        if Assigned(F.OnGetText) then
           aValue:=F.DisplayText
        else
           aValue:=F.AsVariant
      end
      else if (D<>nil) and (roIgnoreFieldNotFound in FReportOptions) and
              lrValidFieldReference(s) then
        aValue := Null
      else if s = 'VALUE' then
        aValue:= CurValue
      else if s = frSpecFuncs[0] then
        aValue:= PageNo + 1
      else if s = frSpecFuncs[2] then
        aValue := CurDate
      else if s = frSpecFuncs[3] then
        aValue:= CurTime
      else if s = frSpecFuncs[4] then
        aValue:= MasterBand.Positions[psLocal]
      else if s = frSpecFuncs[5] then
        aValue:= MasterBand.Positions[psGlobal]
      else if s = frSpecFuncs[6] then
        aValue:= CurPage.ColPos
      else if s = frSpecFuncs[7] then
        aValue:= CurPage.CurPos
      else if s = frSpecFuncs[8] then
        aValue:= SavedAllPages
      else
      begin
        if frVariables.IndexOf(s) <> -1 then
        begin
          aValue:= frVariables[s];
          Exit;
        end else
        if CompareText(s,'REPORTTITLE')=0 then
        begin
          aValue := Title;
          Exit;
        end;
        if s <> SubValue then
        begin
          SubValue := s;
          aValue:= frParser.Calc(s);
          SubValue := '';
        end
        else raise(EParserError.Create('Undefined symbol: ' + SubValue));
      end;
    end;
  end;
end;

procedure TfrReport.OnGetParsFunction(const aName: String; p1, p2, p3: Variant;
   var val: Variant);
var
  i: Integer;
begin
//  val := '0';
  val := varempty;
  {$ifdef DebugLR}
  DebugLn('OnGetParsFunction aName=%s p1=%s p2=%s p3=%s',[aName,p1,p2,p3]);
  {$endif}
  for i := 0 to frFunctionsCount - 1 do
    if frFunctions[i].FunctionLibrary.OnFunction(aName, p1, p2, p3, val) then
      exit;
  if AggrBand.Visible then
    if Assigned(FOnFunction) then
       FOnFunction(aName, p1, p2, p3, val);
end;

// load/save methods
procedure TfrReport.LoadFromStream(Stream: TStream);
begin
  CurReport := Self;
  Stream.Read(frVersion, 1);
  if frVersion < 21 then
  begin
    frVersion := 21;
    Stream.Position := 0;
  end;
  if frVersion <= frCurrentVersion then
  try
{$IFDEF FREEREP2217READ}
    if FRE_COMPATIBLE_READ and (frVersion >= 23) then
      frVersion := 22;
{$ENDIF}
    Pages.LoadFromStream(Stream);
  except
    Pages.Clear;
    Pages.Add;
    MessageDlg(sFRFError,mtError,[mbOk],0)
  end
  else
    MessageDlg(sFRFError,mtError,[mbOk],0);
end;

procedure TfrReport.LoadFromXML(XML: TLrXMLConfig; const Path: String);
var
  ATitle: string;
begin
  CurReport := Self;
  frVersion := XML.GetValue(Path+'Version/Value', 21);
  fComments.Text := XML.GetValue(Path+'Comments/Value', '');
  fKeyWords := XML.GetValue(Path+'KeyWords/Value', '');
  fSubject  := XML.GetValue(Path+'Subject/Value', '');
  ATitle    := XML.GetValue(Path+'Title/Value', '');
  if ATitle<>'' then
    fTitle := ATitle;

//  XML.SetValue(Path+'ReportCreateDate/Value', FReportCreateDate);
//  XML.SetValue(Path+'ReportCreateDate/Value', FReportLastChange);
  FReportVersionBuild:=XML.GetValue(Path+'ReportVersionBuild/Value', '');
  FReportVersionMajor:=XML.GetValue(Path+'ReportVersionMajor/Value', '');
  FReportVersionMinor:=XML.GetValue(Path+'ReportVersionMinor/Value', '');
  FReportVersionRelease:=XML.GetValue(Path+'ReportVersionRelease/Value', '');
  FReportAutor:=XML.GetValue(Path+'ReportAutor/Value', '');

  if frVersion < 21 then
    frVersion := 21;
  if frVersion <= frCurrentVersion then
  try
{$IFDEF FREEREP2217READ}
    if FRE_COMPATIBLE_READ and (frVersion >= 23) then
      frVersion := 22;
{$ENDIF}
    pages.LoadFromXML(XML, Path+'Pages/');
  except
    Pages.Clear;
    Pages.Add;
    MessageDlg(sFRFError,mtError,[mbOk],0)
  end
  else
    MessageDlg(sFRFError,mtError,[mbOk],0);
end;

procedure TfrReport.SaveToStream(Stream: TStream);
begin
  CurReport := Self;
  frVersion := frCurrentVersion;
  Stream.Write(frVersion, 1);
  Pages.SaveToStream(Stream);
end;

procedure TfrReport.LoadFromFile(const FName: String);
var
  Stream: TFileStream;
  Ext   : String;
begin
  Ext:=ExtractFileExt(fName);
  if SameText('.lrf',Ext) then
    LoadFromXMLFile(fName)
  else
  begin
    Stream := TFileStream.Create(UTF8ToSys(FName), fmOpenRead);
    LoadFromStream(Stream);
    Stream.Free;
    FileName := FName;
  end;
end;

procedure TfrReport.LoadFromXMLFile(const Fname: String);
var
  XML: TLrXMLConfig;
begin
  XML := TLrXMLConfig.Create(nil);
  XML.Filename := UTF8ToSys(FName);
  try
    LoadFromXML(XML, 'LazReport/');
    FileName := FName;
  finally
    XML.Free;
  end;
end;

procedure TfrReport.LoadFromXMLStream(const Stream: TStream);
var
  XML: TLrXMLConfig;
begin
  XML := TLrXMLConfig.Create(nil);
  XML.LoadFromStream(Stream);
  try
    LoadFromXML(XML, 'LazReport/');
    FileName := '-stream-';
  finally
    XML.Free;
  end;
end;

procedure TfrReport.SaveToFile(FName: String);
var
  Stream: TFileStream;
  Ext   : string;
begin
  Ext:=ExtractFileExt(fName);
  if (Ext='') or (Ext='.') then
  begin
    Ext:='.lrf';
    fName:=ChangeFileExt(fName,Ext);
  end;
  
  if SameText('.lrf',Ext) then
    SaveToXMLFile(fName)
  else
  begin
    Stream := TFileStream.Create(UTF8ToSys(FName), fmCreate);
    SaveToStream(Stream);
    Stream.Free;
  end;
end;

procedure TfrReport.SavetoXML(XML: TLrXMLConfig; const Path: String);
begin
  CurReport := Self;
  frVersion := frCurrentVersion;
  XML.SetValue(Path+'Version/Value', frVersion);

  XML.SetValue(Path+'Title/Value', fTitle);
  XML.SetValue(Path+'Subject/Value', fSubject);
  XML.SetValue(Path+'KeyWords/Value', fKeyWords);
  XML.SetValue(Path+'Comments/Value', fComments.Text);

//  XML.SetValue(Path+'ReportCreateDate/Value', FReportCreateDate);
//  XML.SetValue(Path+'ReportCreateDate/Value', FReportLastChange);
  XML.SetValue(Path+'ReportVersionBuild/Value', FReportVersionBuild);
  XML.SetValue(Path+'ReportVersionMajor/Value', FReportVersionMajor);
  XML.SetValue(Path+'ReportVersionMinor/Value', FReportVersionMinor);
  XML.SetValue(Path+'ReportVersionRelease/Value', FReportVersionRelease);
  XML.SetValue(Path+'ReportAutor/Value', FReportAutor);

  Pages.SaveToXML(XML, Path+'Pages/');
end;

procedure TfrReport.SaveToXMLFile(const FName: String);
var
  XML: TLrXMLConfig;
begin
  XML := TLrXMLConfig.Create(nil);
  XML.StartEmpty := True;
  XML.Filename := UTF8ToSys(FName);
  try
    SaveToXML(XML, 'LazReport/');
    XML.Flush;
  finally
    XML.Free;
  end;
end;

procedure TfrReport.LoadFromDB(Table: TDataSet; DocN: Integer);
var
  Stream: TMemoryStream;
begin
  Table.First;
  while not Table.Eof do
  begin
    if Table.Fields[0].AsInteger = DocN then
    begin
      Stream := TMemoryStream.Create;
      TfrTBlobField(Table.Fields[1]).SaveToStream(Stream);
      Stream.Position := 0;
      LoadFromStream(Stream);
      Stream.Free;
      Exit;
    end;
    Table.Next;
  end;
end;

procedure TfrReport.SaveToDB(Table: TDataSet; DocN: Integer);
var
  Stream: TMemoryStream;
  Found: Boolean;
begin
  Found := False;
  Table.First;
  while not Table.Eof do
  begin
    if Table.Fields[0].AsInteger = DocN then
    begin
      Found := True;
      break;
    end;
    Table.Next;
  end;

  if Found then
    Table.Edit else
    Table.Append;
  Table.Fields[0].AsInteger := DocN;
  Stream := TMemoryStream.Create;
  SaveToStream(Stream);
  Stream.Position := 0;
  TfrTBlobField(Table.Fields[1]).LoadFromStream(Stream);
  Stream.Free;
  Table.Post;
end;

procedure TfrReport.LoadPreparedReport(const FName: String);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(UTF8ToSys(FName), fmOpenRead);
  EMFPages.LoadFromStream(Stream);
  Stream.Free;
  CanRebuild := False;
end;

procedure TfrReport.SavePreparedReport(const FName: String);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(UTF8ToSys(FName), fmCreate);
  EMFPages.SaveToStream(Stream);
  Stream.Free;
end;

procedure TfrReport.LoadTemplate(const FName: String; comm: TStrings;
  Bmp: TBitmap; Load: Boolean);
var
  Stream: TFileStream;
  b: Byte;
  fb: TBitmap;
  fm: TStringList;
  pos: Integer;
begin
  fb := TBitmap.Create;
  fm := TStringList.Create;
  Stream := TFileStream.Create(UTF8ToSys(FName), fmOpenRead);
  if Load then
  begin
    ReadMemo(Stream, fm);
    Stream.Read(pos, 4);
    Stream.Read(b, 1);
    if b <> 0 then
      fb.LoadFromStream(Stream);
    Stream.Position := pos;
    Pages.LoadFromStream(Stream);
  end
  else
  begin
    ReadMemo(Stream, Comm);
    Stream.Read(pos, 4);
    Bmp.Assign(nil);
    Stream.Read(b, 1);
    if b <> 0 then
      Bmp.LoadFromStream(Stream);
  end;
  fm.Free; fb.Free;
  Stream.Free;
end;

procedure TfrReport.SaveTemplate(const FName: String; Comm: TStrings; Bmp: TBitmap);
var
  Stream: TFileStream;
  b: Byte;
  pos, lpos: Integer;
begin
  Stream := TFileStream.Create(UTF8ToSys(FName), fmCreate);
  frWriteMemo(Stream, Comm);
  b := 0;
  pos := Stream.Position;
  lpos := 0;
  Stream.Write(lpos, 4);
  if Bmp.Empty then
    Stream.Write(b, 1)
  else
  begin
    b := 1;
    Stream.Write(b, 1);
    Bmp.SaveToStream(Stream);
  end;
  lpos := Stream.Position;
  Stream.Position := pos;
  Stream.Write(lpos, 4);
  Stream.Position := lpos;
  Pages.SaveToStream(Stream);
  Stream.Free;
end;

// report manipulation methods
procedure TfrReport.DesignReport;
var
  HF: String;
begin
  CurReport := Self;
  if Pages.Count = 0 then
    Pages.Add;
  HF := Application.HelpFile;
  Application.HelpFile := 'FRuser.hlp';
  if not Assigned(frDesigner)  and Assigned(ProcedureInitDesigner)  then
    ProcedureInitDesigner();
  if frDesigner <> nil then
    {$IFDEF MODALDESIGNER}
    frDesigner.ShowModal;
    {$ELSE}
    frDesigner.Show;
    {$ENDIF}
  Application.HelpFile := HF;
end;

var
  FirstPassTerminated, FirstTime: Boolean;

procedure TfrReport.BuildBeforeModal(Sender: TObject);
begin
  {$IFDEF DebugLR}
  DebugLnEnter('TfrReport.BuildBeforeModal INIT FinalPass=%s DoublePass=%s',[dbgs(FinalPass),dbgs(DoublePass)]);
  {$ENDIF}
  DoBuildReport;
  if FinalPass then
  begin
    if Terminated then
      frProgressForm.ModalDone(mrCancel)
    else
      frProgressForm.ModalDone(mrOk);
  end
  else
  begin
    FirstPassTerminated := Terminated;
    SavedAllPages := EMFPages.Count;
    DoublePass := False;
    FirstTime := False;
    DoPrepareReport; // do final pass
    DoublePass := True;
  end;
  {$IFDEF DebugLR}
  DebugLnExit('TfrReport.BuildBeforeModal DONE');
  {$ENDIF}
end;

function TfrReport.PrepareReport: Boolean;
var
  ParamOk: Boolean;
begin
  {$IFDEF DebugLR}
  DebugLnEnter('TfrReport.PrepareReport INIT');
  {$ENDIF}
  DocMode := dmPrinting;
  CurDate := Date;
  CurTime := Time;
  MasterReport := Self;
  CurReport := Self;
  Values.Items.Sorted := True;
  frParser.OnGetValue := @GetVariableValue;
  frParser.OnFunction := @OnGetParsFunction;
  if Assigned(FOnBeginDoc) then FOnBeginDoc;

  Result := False;
  ParamOk := True;
  if frDataManager <> nil then
  begin
    FillQueryParams;
    ParamOk := frDataManager.ShowParamsDialog;
  end;
  
  if ParamOk then
    Result := DoPrepareReport;
    
  FinalPass := False;
  if frDataManager <> nil then
    frDataManager.AfterParamsDialog;
    
  if Assigned(FOnEndDoc) then
    FOnEndDoc;
  {$IFDEF DebugLR}
  DebugLnExit('TfrReport.PrepareReport DONE');
  {$ENDIF}
end;

function TfrReport.DoPrepareReport: Boolean;
var
  s: String;
begin
  Result := True;
  Terminated := False;
  Append := False;
  DisableDrawing := False;
  FinalPass := True;
  FirstTime := True;
  PageNo := 0;
  EMFPages.Clear;

  {$IFDEF DebugLR}
  DebugLnEnter('DoPrepareReport INIT DoublePass=%s',[BoolToStr(DoublePass)]);
  {$ENDIF}

  s := sReportPreparing;
  if DoublePass then
  begin
    {$IFDEF DebugLR}
    DebugLnEnter('DoPrepareReport FirstPass Begin');
    {$ENDIF}

    DisableDrawing := True;
    FinalPass := False;
    if not Assigned(FOnProgress) and FShowProgress then
    begin
      with frProgressForm do
      begin
        if Title = '' then
          Caption := s
        else
          Caption := s + ' - ' + Title;
          
        FirstCaption := sFirstPass;
        Label1.Caption := FirstCaption + '  1';
        OnBeforeModal := @BuildBeforeModal;
        Show_Modal(Self);
      end;
      
      {$IFDEF DebugLR}
      DebugLnExit('DoPrepareReport FirstPass End');
      {$ENDIF}
    end
    else BuildBeforeModal(nil);
    {$IFDEF DebugLR}
    DebugLnExit('DoPrepareReport DONE');
    {$ENDIF}
    Exit;
  end;
  
  if not Assigned(FOnProgress) and FShowProgress then
  begin
    {$IFDEF DebugLR}
    DebugLnEnter('DoPrepareReport SecondPass begin');
    {$ENDIF}

    with frProgressForm do
    begin
      {$IFDEF DebugLR}
      DebugLn('1');
      {$ENDIF}
      if Title = '' then
        Caption := s
      else
        Caption := s + ' - ' + Title;
      FirstCaption := sPagePreparing;
      Label1.Caption := FirstCaption + '  1';
      OnBeforeModal:=@BuildBeforeModal;
      {$IFDEF DebugLR}
      DebugLn('2');
      {$ENDIF}
      if Visible then
      begin
        {$IFDEF DebugLR}
        DebugLn('3');
        {$ENDIF}
        if not FirstPassTerminated then
           DoublePass := True;
           
        BuildBeforeModal(nil);
        {$IFDEF DebugLR}
        DebugLn('4');
        {$ENDIF}
      end
      else
      begin
        {$IFDEF DebugLR}
        DebugLn('5');
        {$ENDIF}
        SavedAllPages := 0;
        if Show_Modal(Self) = mrCancel then
          Result := False;
        {$IFDEF DebugLR}
        DebugLn('6');
        {$ENDIF}
      end;
      
      {$IFDEF DebugLR}
      DebugLnExit('DoPrepareReport SecondPass End');
      {$ENDIF}
    end;
  end
  else BuildBeforeModal(nil);
  Terminated := False;
  {$IFDEF DebugLR}
  DebugLnExit('DoPrepareReport DONE');
  {$ENDIF}
end;

var
  ExportStream: TFileStream;

procedure TfrReport.ExportBeforeModal(Sender: TObject);
var
  i: Integer;
begin
  Application.ProcessMessages;
  for i := 0 to EMFPages.Count - 1 do
  begin
    FCurrentFilter.OnBeginPage;
    EMFPages.ExportData(i);
    InternalOnProgress(i + 1);
    Application.ProcessMessages;
    FCurrentFilter.OnEndPage;
  end;
  FCurrentFilter.OnEndDoc;
  frProgressForm.ModalResult := mrOk;
end;

procedure TfrReport.ExportTo(FilterClass: TfrExportFilterClass; const aFileName: String);
var
  s: String;
begin
  ExportStream := TFileStream.Create(UTF8ToSys(aFileName), fmCreate);
  FCurrentFilter := FilterClass.Create(ExportStream);

  CurReport := Self;
  MasterReport := Self;

  FCurrentFilter.OnSetup:=CurReport.OnExportFilterSetup;

  FCurrentFilter.Setup;
  FCurrentFilter.OnBeginDoc;

  SavedAllPages := EMFPages.Count;

  if FCurrentFilter.UseProgressbar then
  with frProgressForm do
  begin
    s := sReportPreparing;
    if Title = '' then
      Caption := s
    else
      Caption := s + ' - ' + Title;
    FirstCaption := sPagePreparing;
    Label1.Caption := FirstCaption + '  1';
    OnBeforeModal := @ExportBeforeModal;
    Show_Modal(Self);
  end else
    ExportBeforeModal(nil);

  FreeAndNil(FCurrentFilter);
  ExportStream.Free;
end;

procedure TfrReport.FillQueryParams;
var
  i, j: Integer;
  t: TfrView;
  procedure PrepareDS(ds: TfrDataSet);
  begin
    if (ds <> nil) and (ds is TfrDBDataSet) then
      frDataManager.PrepareDataSet(TfrTDataSet((ds as TfrDBDataSet).GetDataSet));
  end;
begin
  if frDataManager = nil then Exit;
  frDataManager.BeforePreparing;
  if Dataset <> nil then
    PrepareDS(DataSet);
  for i := 0 to Pages.Count - 1 do
    for j := 0 to Pages[i].Objects.Count-1 do
    begin
      t :=TfrView(Pages[i].Objects[j]);
      if t is TfrBandView then
        PrepareDS(frFindComponent(CurReport.Owner, TfrBandView(t).DataSet) as TfrDataSet);
    end;
  frDataManager.AfterPreparing;
end;

procedure TfrReport.DoBuildReport;
var
  i  : Integer;
  b  : Boolean;
  BM : Pointer;
begin
  {$IFDEF DebugLR}
  DebugLnEnter('TfrReport.DoBuildReport INIT');
  {$ENDIF}
  HookList.Clear;
  CanRebuild := True;
  DocMode := dmPrinting;
  CurReport := Self;
  Values.Items.Sorted := True;
  frParser.OnGetValue := @GetVariableValue;
  frParser.OnFunction := @OnGetParsFunction;
  ErrorFlag := False;
  b := (Dataset <> nil) and (ReportType = rtMultiple);
  if b then
  begin
    BM:=DataSet.GetBookMark;
    DataSet.DisableControls;
    Dataset.Init;
    Dataset.First;
  end;
  try
    for i := 0 to Pages.Count - 1 do
      Pages[i].Skip := False;
    for i := 0 to Pages.Count - 1 do
      Pages[i].InitReport;
    PrepareDataSets;
    for i := 0 to Pages.Count - 1 do
      if Pages[i].PageType = ptReport then
        Pages[i].PrepareObjects;

    repeat
      {$IFDEF DebugLR}
      DebugLn('p1');
      {$ENDIF}
      InternalOnProgress(PageNo + 1);
      {$IFDEF DebugLR}
      DebugLn('p2');
      {$ENDIF}

      for i := 0 to Pages.Count - 1 do
      begin
        if Pages[i].PageType = ptReport then
        begin
          FCurPage := Pages[i];
          if FCurPage.Skip then
            Continue;
          FCurPage.Mode := pmNormal;
          if Assigned(FOnManualBuild) then
            FOnManualBuild(FCurPage)
          else
            FCurPage.FormPage;

        {$IFDEF DebugLR}
        debugLn('p3');
        {$ENDIF}

          Append := False;
          if ((i = Pages.Count - 1) and CompositeMode and (not b or Dataset.Eof)) or
             ((i <> Pages.Count - 1) and Pages[i + 1].PrintToPrevPage) then
          begin
            Dec(PageNo);
            Append := True;
          end;
          if not Append then
          begin
            PageNo := MasterReport.EMFPages.Count;
            InternalOnProgress(PageNo);
          end;
          if MasterReport.Terminated then
            Break;
        end;
      end;
      {$IFDEF DebugLR}
      DebugLn('p4');
      {$ENDIF}

      InternalOnProgress(PageNo);
      if b then
        Dataset.Next;
    until MasterReport.Terminated or not b or Dataset.Eof;

    for i := 0 to Pages.Count - 1 do
      Pages[i].DoneReport;
  finally
    if b then
    begin
      Dataset.Exit;
      DataSet.GotoBookMark(BM);
      DataSet.FreeBookMark(BM);
      DataSet.EnableControls;
    end;
  end;
  if (frDataManager <> nil) and FinalPass then
    frDataManager.AfterPreparing;
  Values.Items.Sorted := False;
  {$IFDEF DebugLR}
  DebugLnExit('TfrReport.DoBuildReport DONE');
  {$ENDIF}
end;

procedure TfrReport.ShowReport;
begin
  PrepareReport;
  if ErrorFlag then
  begin
    MessageDlg(ErrorStr,mtError,[mbOk],0);
    EMFPages.Clear;
  end
  else
    ShowPreparedReport;
end;

procedure TfrReport.ShowPreparedReport;
var
  s: String;
  p: TfrPreviewForm;
begin
  {$IFDEF DebugLR}
  DebugLnEnter('TfrReport.ShowPreparedReport INIT');
  {$ENDIF}
  CurReport := Self;
  MasterReport := Self;
  DocMode := dmPrinting;
  if EMFPages.Count = 0 then Exit;
  s := sPreview;
  if Title <> '' then
    s := s + ' - ' + Title;
    
  if not (csDesigning in ComponentState) and Assigned(Preview) then
  begin
    Preview.Connect(Self);
  end
  else
  begin
    p := TfrPreviewForm.Create(nil);
    p.BorderIcons:=p.BorderIcons - [biMinimize];
    {$IFDEF DebugLR}
    DebugLn('1 TfrPreviewForm.visible=%s',[BooLToStr(p.Visible)]);
    {$ENDIF}
    p.Caption := s;
    {$IFDEF DebugLR}
    DebugLn('2 TfrPreviewForm.visible=%s',[BooLToStr(p.Visible)]);
    {$ENDIF}
    if ExportFilename<>'' then
      p.SaveDialog.FileName := ExportFilename;
    p.Show_Modal(Self);
  end;
  {$IFDEF DebugLR}
  DebugLnExit('TfrReport.ShowPreparedReport DONE');
  {$ENDIF}
end;

procedure TfrReport.PrintBeforeModal(Sender: TObject);
begin
  DoPrintReport(FPageNumbers, FCopies);
  frProgressForm.ModalResult := mrOk;
end;

procedure TfrReport.PrintPreparedReport(const PageNumbers: String; Copies: Integer);
var
  s: String;
begin
  CurReport:=Self;
  MasterReport:=Self;
  s:=sReportPreparing;
  Terminated:=False;
  FPageNumbers:=PageNumbers;
  FCopies:=Copies;
  
  if not Assigned(FOnProgress) and FShowProgress then
  begin
    with frProgressForm do
    begin
      if Title = '' then
        Caption := s
      else
        Caption := s + ' - ' + Title;
        
      FirstCaption := sPagePrinting;
      Label1.Caption := FirstCaption;
      OnBeforeModal := @PrintBeforeModal;

      Show_Modal(Self);
    end
  end
  else PrintBeforeModal(nil);

  Terminated := False;
end;

procedure TfrReport.DoPrintReport(const PageNumbers: String; Copies: Integer);
var
  i, j: Integer;
  f: Boolean;
  pgList: TStringList;

  procedure ParsePageNumbers;
  var
    i, j, n1, n2: Integer;
    s: String;
    IsRange: Boolean;
  begin
    s := PageNumbers;
    
    while Pos(' ', s) <> 0 do
      Delete(s, Pos(' ', s), 1);
    if s = '' then Exit;

    s := s + ',';
    i := 1; j := 1; n1 := 1;
    IsRange := False;
    while i <= Length(s) do
    begin
      if s[i] = ',' then
      begin
        n2 := StrToInt(Copy(s, j, i - j));
        j := i + 1;
        if IsRange then
        begin
          while n1 <= n2 do
          begin
            pgList.Add(IntToStr(n1));
            Inc(n1);
          end;
        end
        else
          pgList.Add(IntToStr(n2));
          
        IsRange := False;
      end
      else if s[i] = '-' then
           begin
             IsRange := True;
             n1 := StrToInt(Copy(s, j, i - j));
             j := i + 1;
           end;
           
      Inc(i);
    end;
  end;

  procedure PrintPage(n: Integer);
  begin
    with Printer, EMFPages[n]^ do
    begin
      if not Prn.IsEqual(pgSize, pgWidth, pgHeight, pgOr) then
      begin
        EndDoc;
        Prn.SetPrinterInfo(pgSize, pgWidth, pgHeight, pgOr);
        BeginDoc;
      end
      else if not f then
             NewPage;
             
      Prn.FillPrnInfo(PrnInfo);
      Visible := True;

      with PrnInfo do
      begin
        if pgMargins then
          EMFPages.Draw(n, Printer.Canvas, Rect(-POfx, -POfy, PPgw - POfx, PPgh - POfy))
        else
          EMFPages.Draw(n, Printer.Canvas, Rect(0, 0, PPw, PPh));
      end;
      
      Visible := False;
      EMFPages.Draw(n, Printer.Canvas, Rect(0, 0, 0, 0));
    end;
    InternalOnProgress(n + 1);
    Application.ProcessMessages;
    f := False;
  end;
  {$IFDEF DebugLR}
  procedure DebugPrnInfo(msg: string);
  var
    k: integer;
  begin
    DebugLn('--------------------------------------------------');
    DebugLn(Msg);
    for k:=0 to EMFPages.Count-1 do begin
      DebugLn('EMFPage ',dbgs(k));
      with EmfPages[k]^.PrnInfo do begin
        DebugLn(Format('  Ppgw=%d PPgh=%d Pgw=%d Pgh=%d',[PPgw,PPgh,Pgw,Pgh]));
        DebugLn(Format('  Pofx=%d POfy=%d Ofx=%d Ofy=%d',[POfx,POfy,Ofx,Ofy]));
        DebugLn(Format('   Ppw=%d  Pph=%d  Pw=%d  Ph=%d',[Ppw,Pph,Pw,Ph]));
      end;
    end;
  end;
  {$ENDIF}

begin
  {$IFDEF DebugLR}
  DebugLn('DoPrintReport INIT');
  DebugPrnInfo('=== INIT');
  {$ENDIF}
  Prn.Printer := Printer;
  pgList := TStringList.Create;

  ParsePageNumbers;
  if Copies <= 0 then
    Copies := 1;

  with EMFPages[0]^ do
  begin
    Prn.SetPrinterInfo(pgSize, pgWidth, pgHeight, pgOr);
    Prn.FillPrnInfo(PrnInfo);
  end;
  {$IFDEF DebugLR}
  DebugPrnInfo('=== AFTER EMFPages[0]^');
  {$ENDIF}
  if Title <> '' then
    Printer.Title:=Format('%s',[Title])
  else
    Printer.Title:=Format('LazReport : %s',[sUntitled]);

  Printer.BeginDoc;
  f:= True;
  for i := 0 to EMFPages.Count - 1 do
  begin
    if (pgList.Count = 0) or (pgList.IndexOf(IntToStr(i + 1)) <> -1) then
    begin
      for j := 0 to Copies - 1 do
      begin
        {$IFDEF DebugLR}
        DebugPrnInfo('=== Before PrintPage('+IntToStr(i)+')');
        {$ENDIF}
        PrintPage(i);
        
        if Terminated then
        begin
          Printer.Abort;
          pgList.Free;
          Exit;
        end;
      end;
    end;
  end;
  
  Printer.EndDoc;
  pgList.Free;
  {$IFDEF DebugLR}
  DebugPrnInfo('=== END');
  {$ENDIF}
end;

procedure TfrReport.SetComments(const AValue: TStringList);
begin
  FComments.Assign(AValue);
end;

// printer manipulation methods

procedure TfrReport.SetPrinterTo(const PrnName: String);
begin
  {$ifdef dbgPrinter}
  DebugLn('TfrReport.SetPrinterTo PrnName=%s PrintToDefault=%d PrnExist?=%d PrnIndex=%d PrinterIndex=%d Name=%s',
    [prnName, Ord(PrintToDefault), Ord(Prn.Printers.IndexOf(PrnName)>=0),
     prn.PrinterIndex, Prn.Printer.PrinterIndex, prn.Printer.Printers[prn.Printer.PrinterIndex]]);
  {$endif}
  if not PrintToDefault then
  begin
    if Prn.Printers.IndexOf(PrnName) <> -1 then
      Prn.PrinterIndex := Prn.Printers.IndexOf(PrnName)
    else
    if Prn.Printers.Count>0 then
      Prn.PrinterIndex := 0 // either the system default or
                            // own virtual default printer
  end;
end;

function TfrReport.ChangePrinter(OldIndex, NewIndex: Integer): Boolean;

  procedure ChangePages;
  var
    i: Integer;
  begin
    for i := 0 to Pages.Count - 1 do
    begin
      with Pages[i] do
        ChangePaper(pgSize, Width, Height, Orientation);
    end;
  end;
  
begin
  Result := True;
  try
    {$ifdef dbgPrinter}
    DebugLn('TfrReport.ChangePrinter CurIndex=%d OldIndex=%d NewIndex=%d',
      [Prn.PrinterIndex,OldIndex,NewIndex]);
    {$endif}
    Prn.PrinterIndex := NewIndex;
    Prn.PaperSize := -1;
    ChangePages;
  except
    on Exception do
    begin
      MessageDlg(sPrinterError,mtError,[mbOk],0);
      Prn.PrinterIndex := OldIndex;
      ChangePages;
      Result := False;
    end;
  end;
end;

procedure TfrReport.EditPreparedReport(PageIndex: Integer);
var
  p: PfrPageInfo;
  Stream: TMemoryStream;
  Designer: TfrReportDesigner;
  DesName: String;
begin
  if frDesigner = nil then Exit;
  Screen.Cursor := crHourGlass;
  Designer := frDesigner;
  DesName := Designer.Name;
  Designer.Name := DesName + '__';
  Designer.Page := nil;
  frDesigner := TfrReportDesigner(frDesigner.ClassType.NewInstance);
  frDesigner.Create(nil);
  Stream := TMemoryStream.Create;
  SaveToStream(Stream);
  Pages.Clear;
  EMFPages.ObjectsToPage(PageIndex);
  p := EMFPages[PageIndex];
  Pages.FPages.Add(p^.Page);
  CurReport := Self;
  Screen.Cursor := crDefault;
  try
    frDesigner.ShowModal;
    if frDesigner.Modified then
      if MessageDlg(sSaveChanges+' ?',mtConfirmation,[mbYes,mbNo],0)=mrYes then
        EMFPages.PageToObjects(PageIndex);
  finally
    Pages.FPages.Clear;
    Stream.Position := 0;
    LoadFromStream(Stream);
    Stream.Free;
    frDesigner.Free;
    frDesigner := Designer;
    frDesigner.Name := DesName;
    frDesigner.Page := Pages[0];
    frDesigner.RedrawPage;
  end;
end;


// miscellaneous methods
procedure TfrReport.PrepareDataSets;
var
  i: Integer;
begin
  with Values do
  for i := 0 to Items.Count - 1 do
    with Objects[i] do
    if Typ = vtDBField then
      DSet := frGetDataSet(DataSet);
end;

procedure TfrReport.SetVars(Value: TStrings);
begin
  FVars.Assign(Value);
end;

procedure TfrReport.ClearAttribs;
begin
//  FDefaultTitle:='';
  FTitle:='';
  FSubject:='';
  FKeyWords:='';
  FComments.Clear;

  ReportAutor := '';
  ReportVersionMajor := '';
  ReportVersionMinor := '';
  ReportVersionRelease := '';
  ReportVersionBuild := '';
  ReportCreateDate := Now;
  ReportLastChange := Now;
end;

procedure TfrReport.Loaded;
begin
  inherited Loaded;
  if assigned(FDFMStream) then
  begin
    LoadFromStream(FDFMStream);
    FreeAndNil(FDFMStream)
  end;
end;

procedure TfrReport.GetVarList(CatNo: Integer; List: TStrings);
var
  i, n: Integer;
  s: String;
begin
  List.Clear;
  i := 0; n := 0;
  if FVars.Count > 0 then
    repeat
      s := FVars[i];
      if Length(s) > 0 then
        if s[1] <> ' ' then Inc(n);
      Inc(i);
    until n > CatNo;
  while i < FVars.Count do
  begin
    s := FVars[i];
    if (s <> '') and (s[1] = ' ') then
      List.Add(Copy(s, 2, Length(s) - 1)) else
      break;
    Inc(i);
  end;
end;

procedure TfrReport.GetCategoryList(List: TStrings);
var
  i: Integer;
  s: String;
begin
  List.Clear;
  for i := 0 to FVars.Count - 1 do
  begin
    s := FVars[i];
    if (Length(s)>0) and (s[1]<>' ') then
       List.Add(s);
  end;
end;

function TfrReport.FindVariable(Variable: String): Integer;
var
  i: Integer;
begin
  Result := -1;
  Variable := ' ' + Variable;
  for i := 0 to FVars.Count - 1 do
    if Variable = FVars[i] then
    begin
      Result := i;
      break;
    end;
end;

function TfrReport.FindObject(const aName: String): TfrObject;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Pages.Count - 1 do
  begin
    Result:=Pages[i].FindObject(aName);
    if Assigned(Result) then
      Break;
  end;
end;


{----------------------------------------------------------------------------}
constructor TfrCompositeReport.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Reports := TFpList.Create;
end;

destructor TfrCompositeReport.Destroy;
begin
  Reports.Free;
  inherited Destroy;
end;

procedure TfrCompositeReport.DoBuildReport;
var
  i: Integer;
  Doc: TfrReport;
  ParamOk: Boolean;
begin
  CanRebuild := True;
  PageNo := 0;
  for i := 0 to Reports.Count - 1 do
  begin
    Doc := TfrReport(Reports[i]);
    CompositeMode := False;
    if i <> Reports.Count - 1 then
      if (TfrReport(Reports[i + 1]).Pages.Count > 0) and
        TfrReport(Reports[i + 1]).Pages[0].PrintToPrevPage then
        CompositeMode := True;
    CurReport := Doc;
    if Assigned(Doc.FOnBeginDoc) and FirstTime then
      Doc.FOnBeginDoc;
    ParamOk := True;
    if (frDataManager <> nil) and FirstTime then
    begin
      Doc.FillQueryParams;
      ParamOk := frDataManager.ShowParamsDialog;
    end;
    if ParamOk then
      Doc.DoBuildReport;
    if (frDataManager <> nil) and FinalPass then
      frDataManager.AfterParamsDialog;
    if Assigned(Doc.FOnEndDoc) and FinalPass then
      Doc.FOnEndDoc;
    Append := CompositeMode;
    CompositeMode := False;
    if Terminated then break;
  end;
end;


{----------------------------------------------------------------------------}
procedure TfrObjEditorForm.ShowEditor(t: TfrView);
begin
// abstract method
end;


{----------------------------------------------------------------------------}
constructor TfrExportFilter.Create(AStream: TStream);
begin
  inherited Create;
  Stream := AStream;
  Lines := TFpList.Create;
  FBandTypes := [btReportTitle..btNone];
end;

destructor TfrExportFilter.Destroy;
begin
  ClearLines;
  Lines.Free;
  inherited Destroy;
end;

procedure TfrExportFilter.ClearLines;
var
  i: Integer;
  p, p1: PfrTextRec;
begin
  for i := 0 to Lines.Count - 1 do
  begin
    p := PfrTextRec(Lines[i]);
    while p <> nil do
    begin
      p1 := p;
      p := p^.Next;
      SetLength(p1^.Text, 0);
      FreeMem(p1, SizeOf(TfrTextRec));
    end;
  end;
  Lines.Clear;
  FLineIndex := -1;
end;

procedure TfrExportFilter.Setup;
begin
  if assigned(FOnSetup) then
    FOnSetup(Self);
end;

function TfrExportFilter.AddData(x, y: Integer; view: TfrView):pointer;
var
  p: PfrTextRec;
  s: string;
begin
  result := nil;

  if (View = nil) or not (View.ParentBandType in BandTypes) then
    exit;

  if View.Flags and flStartRecord<>0 then
    Inc(FLineIndex);

  if CheckView(View) then
  begin
    s := GetViewText(View);
    NewRec(View, s, p);
    AddRec(FLineIndex, p);
    result := p;
  end;
end;

procedure TfrExportFilter.NewRec(View: TfrView; const AText: string;
  var p:pointer);
begin
  GetMem(p, SizeOf(TfrTextRec));
  FillChar(p^, SizeOf(TfrTextRec), 0);
  with PfrTextRec(p)^ do
  begin
    Next  := nil;
    X     := View.X;
    W     := round(View.Width);
    Typ   := View.Typ;
    Text  := AText;
    FillColor   := View.FillColor;
    Borders     := View.Frames;
    BorderColor := View.FrameColor;
    BorderStyle := View.FrameStyle;
    BorderWidth := Round(View.FrameWidth);
    if View is TfrMemoView then
      with View as TfrMemoView do
      begin
        FontName    := Font.Name;
        FontSize    := Font.Size;
        FontStyle   := frGetFontStyle(Font.Style);
        FontColor   := Font.Color;
        FontCharset := Font.Charset;
        Alignment   := Alignment;
      end;
  end;
end;

procedure TfrExportFilter.AddRec(ALineIndex: Integer; ARec: pointer);
var
  p, p1, p2: PfrTextRec;
begin

  p := ARec;
  p1 := Lines[ALineIndex];
  if p1 = nil then
    Lines[ALineIndex] := TObject(p)
  else
  begin
    p2 := p1;
    while (p1 <> nil) and (p1^.X <= p^.X) do
    begin
      p2 := p1;
      p1 := p1^.Next;
    end;
    if p2 <> p1 then
    begin
      p2^.Next := p;
      p^.Next := p1;
    end
    else
    begin
      Lines[ALineIndex] := TObject(p);
      p^.Next := p1;
    end;
  end;

end;

function TfrExportFilter.GetviewText(View: TfrView): string;
var
  i: Integer;
begin
  result := '';
  for i:=0 to View.Memo.Count-1 do begin
    result := result + View.Memo[i];
    if i<>View.Memo.Count-1 then
      result := result + LineEnding;
  end;
end;

function TfrExportFilter.CheckView(View: TfrView): boolean;
begin
  result := true;
end;

procedure TfrExportFilter.OnBeginDoc;
begin
// abstract method
end;

procedure TfrExportFilter.OnEndDoc;
begin
// abstract method
end;

procedure TfrExportFilter.OnBeginPage;
begin
// abstract method
end;

procedure TfrExportFilter.OnEndPage;
begin
// abstract method
end;

procedure TfrExportFilter.OnData(x, y: Integer; View: TfrView);
begin
// abstract method
end;

procedure TfrExportFilter.OnText(x, y: Integer; const text: String; View: TfrView);
begin
// abstract method
end;

function TfrFunctionLibrary.GetCount: integer;
begin
  result := List.Count + Extra.Count;
end;

function TfrFunctionLibrary.GetDescription(AIndex: Integer
  ): TfrFunctionDescription;
begin
  result := nil;
  if (AIndex>=0) and (AIndex<FunctionCount) then
  begin
    if AIndex<List.Count then
      result := TfrFunctionDescription(List.Objects[AIndex])
    else
      result := TfrFunctionDescription(Extra.Objects[AIndex-List.Count]);
  end;
end;

{----------------------------------------------------------------------------}
constructor TfrFunctionLibrary.Create;
begin
  inherited Create;
  List := TStringList.Create;
  Extra:= TStringList.Create;
  //List.Sorted := True;
end;

destructor TfrFunctionLibrary.Destroy;
  procedure FreeList(AList:TStringList);
  var
    i:integer;
  begin
    for i:=0 to AList.Count-1 do
      if Assigned(AList.Objects[i]) then
      begin
        AList.Objects[i].Free;
        AList.Objects[i]:=nil;
      end;
    AList.Free;
  end;

begin
  FreeList(List);
  FreeList(Extra);
  inherited Destroy;
end;

function TfrFunctionLibrary.OnFunction(const FName: String; p1, p2, p3: Variant;
  var val: Variant): Boolean;
var
  i: Integer;
begin
  Result := False;
//  if List.Find(FName, i) then
  I:=List.IndexOf(FName);
  if I>=0 then
  begin
    DoFunction(i, p1, p2, p3, val);
    Result := True;
  end;
end;

procedure TfrFunctionLibrary.UpdateDescriptions;
begin
end;

procedure TfrFunctionLibrary.Add(const funName: string; IsExtra:boolean=false);
begin
  if IsExtra then
    Extra.Add(funName)
  else
    List.Add(FunName);
end;

procedure TfrFunctionLibrary.AddFunctionDesc(const funName, funGroup,
  funDescription: string);
var
  i: Integer;

  procedure AddDesc(AList:TStringList);
  begin
    if not Assigned(AList.Objects[i]) then
      AList.Objects[i]:=TfrFunctionDescription.Create;
    TfrFunctionDescription(AList.Objects[i]).funName:=funName;
    TfrFunctionDescription(AList.Objects[i]).funGroup:=funGroup;
    TfrFunctionDescription(AList.Objects[i]).funDescription:=funDescription;
  end;

begin
  if List.Find(funName, i) then
    AddDesc(List)
  else
  begin
    i := Extra.IndexOf(funName);
    if i>=0 then
      AddDesc(Extra);
  end;
end;


{----------------------------------------------------------------------------}
constructor TfrStdFunctionLibrary.Create;
begin
  inherited Create;
  Add('AVG');               {0}
  Add('COUNT');             {1}
  Add('DAYOF');             {2}
  Add('FORMATDATETIME');    {3}
  Add('FORMATFLOAT');       {4}
  Add('FORMATTEXT');        {5}
  Add('INPUT');             {6}
  Add('LENGTH');            {7}
  Add('LOWERCASE');         {8}
  Add('MAX');               {9}
  Add('MAXNUM');            {10}
  Add('MESSAGEBOX');        {11}
  Add('MIN');               {12}
  Add('MINNUM');            {13}
  Add('MONTHOF');           {14}
  Add('NAMECASE');          {15}
  Add('POS');               {16}
  Add('STRTODATE');         {17}
  Add('STRTOTIME');         {18}
  Add('SUM');               {19}
  Add('TRIM');              {20}
  Add('UPPERCASE');         {21}
  Add('YEAROF');            {22}
  // internal functions/operators
  Add('COPY', true);
  Add('STR', true);
  Add('INT', true);
  Add('ROUND', true);
  Add('FRAC', true);
  Add('MOD', true);
end;

procedure TfrStdFunctionLibrary.UpdateDescriptions;
begin
  AddFunctionDesc('AVG', SAggregateCategory, SDescriptionAVG);
  AddFunctionDesc('COUNT', SAggregateCategory, SDescriptionCOUNT);
  AddFunctionDesc('MAX', SAggregateCategory, SDescriptionMAX);
  AddFunctionDesc('MIN', SAggregateCategory, SDescriptionMIN);
  AddFunctionDesc('SUM', SAggregateCategory, SDescriptionSUM);

  AddFunctionDesc('DAYOF', SDateTimeCategory, SDescriptionDAYOF);
  AddFunctionDesc('MONTHOF', SDateTimeCategory, SDescriptionMONTHOF);
  AddFunctionDesc('STRTODATE', SDateTimeCategory, SDescriptionSTRTODATE);
  AddFunctionDesc('STRTOTIME', SDateTimeCategory, SDescriptionSTRTOTIME);
  AddFunctionDesc('YEAROF', SDateTimeCategory, SDescriptionYEAROF);

  AddFunctionDesc('FORMATDATETIME', SStringCategory, SDescriptionFORMATDATETIME);
  AddFunctionDesc('FORMATFLOAT', SStringCategory, SDescriptionFORMATFLOAT);
  AddFunctionDesc('FORMATTEXT', SStringCategory, SDescriptionFORMATTEXT);
  AddFunctionDesc('LENGTH', SStringCategory, SDescriptionLENGTH);
  AddFunctionDesc('LOWERCASE', SStringCategory, SDescriptionLOWERCASE);
  AddFunctionDesc('NAMECASE', SStringCategory, SDescriptionNAMECASE);
  AddFunctionDesc('TRIM', SStringCategory, SDescriptionTRIM);
  AddFunctionDesc('UPPERCASE', SStringCategory, SDescriptionUPPERCASE);
  AddFunctionDesc('POS', SStringCategory, SDescriptionPOS);
  AddFunctionDesc('COPY', SStringCategory, SDescriptionCOPY);
  AddFunctionDesc('STR', SStringCategory, SDescriptionSTR);

  AddFunctionDesc('INPUT', SOtherCategory, SDescriptionINPUT);
  AddFunctionDesc('MESSAGEBOX', SOtherCategory, SDescriptionMESSAGEBOX);

  AddFunctionDesc('MAXNUM', SMathCategory, SDescriptionMAXNUM);
  AddFunctionDesc('MINNUM', SMathCategory, SDescriptionMINNUM);
  AddFunctionDesc('INT', SMathCategory, SDescriptionINT);
  AddFunctionDesc('ROUND', SMathCategory, SDescriptionROUND);
  AddFunctionDesc('FRAC', SMathCategory, SDescriptionFRAC);
end;

procedure TfrStdFunctionLibrary.DoFunction(FNo: Integer; p1, p2, p3: Variant;
  var val: Variant);
var
  DataSet: TfrTDataSet;
  Field: TfrTField;
  Obj: TFrObject;
  s1, s2, VarName: String;
  min, max, avg, sum, count, d, v: Double;
  dk: (dkNone, dkSum, dkMin, dkMax, dkAvg, dkCount);
  vv, v2, v1: Variant;
  BM : Pointer;
  {$IFDEF DebugLR}
  function FNoStr: string;
  begin
    if FNo<=List.Count then
      result := List[FNo]
    else
      result := '???';
  end;
  {$ENDIF}
begin
  {$IFDEF DebugLR}
  DebugLnEnter('TfrStdFunctionLibrary.DoFunction FNo=%d (%s) p1=%s p2=%s p3=%s val=%s',[FNo,FNoStr,p1,p2,p3,val]);
  {$ENDIF}
  dk := dkNone;
  val := '0';
  case FNo of
    0: dk := dkAvg;                                           //Add('AVG');               {0}
    1: dk := dkCount;                                         //Add('COUNT');             {1}
    2: val := DayOf(frParser.Calc(p1));                       //Add('DAYOF');             {2}
    3: val := FormatDateTime(frParser.Calc(p1), frParser.Calc(p2)); //Add('FORMATDATETIME');    {3}
    4: val := FormatFloat(frParser.Calc(p1), frParser.Calc(p2)); //Add('FORMATFLOAT');       {4}
    5: val := FormatMaskText(frParser.Calc(p1) + ';0; ', frParser.Calc(p2));  //Add('FORMATTEXT');        {5}
    6:begin                                                   //Add('INPUT');             {6}
        s1 := InputBox('', frParser.Calc(p1), frParser.Calc(p2));
        val := s1;
      end;
    7:val := Length(frParser.Calc(p1));                       //Add('LENGTH');            {7}
    8: val := AnsiLowerCase(frParser.Calc(p1)); //Add('LOWERCASE');         {8}
    9: dk := dkMax;                                           //Add('MAX');               {9}
   10:begin                                                   //Add('MAXNUM');            {10}
        v2 := frParser.Calc(p1);
        v1 := frParser.Calc(p2);
        if v2 > v1 then
          val := v2 else
          val := v1;
      end;
   11:val := Application.MessageBox(PChar(String(frParser.Calc(p1))), //Add('MESSAGEBOX');        {11}
          PChar(String(frParser.Calc(p2))), frParser.Calc(p3));
   12: dk := dkMin;                                           //Add('MIN');               {12}
   13:begin                                                   //Add('MINNUM');            {13}
        v2 := frParser.Calc(p1);
        v1 := frParser.Calc(p2);
        if v2 < v1 then
          val := v2 else
          val := v1;
      end;
   14: val := MonthOf(frParser.Calc(p1));                     //Add('MONTHOF');           {14}
   15:begin                                                   //Add('NAMECASE');          {15}
        s1 := AnsiLowerCase(frParser.Calc(p1));
        if Length(s1) > 0 then
          val := AnsiUpperCase(s1[1]) + Copy(s1, 2, Length(s1) - 1)
        else
          val := '';
      end;
   16:begin                                                   // Add('POS');               {16}
        S1:=frParser.Calc(p1);
        S2:=frParser.Calc(p2);
        val := Pos(S1, S2);
      end;
   17: val := StrToDate(frParser.Calc(p1));                   //Add('STRTODATE');         {17}
   18: val := StrToTime(frParser.Calc(p1));                   //Add('STRTOTIME');         {18}
   19: dk := dkSum;                                           //Add('SUM');               {19}
   20: begin                                                  //Add('TRIM');              {20}
         S1:=frParser.Calc(p1);
         val := Trim(S1);
       end;
   21: val := AnsiUpperCase(frParser.Calc(p1)); //Add('UPPERCASE');         {21}
   22: val := YearOf(frParser.Calc(p1));                      //Add('YEAROF');            {22}
  end;
  
  if dk <> dkNone then
  begin

    if dk = dkCount then
      DataSet := frGetDataSet(lrGetUnBrackedStr(p1))
    else
    begin
      // if bandname is provided if yes, don't try to use dataset/field
      Obj := curPage.FindObject(trim(P2));

      if (obj is TfrBandView) and
        (TfrBandView(Obj).BandType in [btMasterData,btDetailData,
          btSubDetailData,btCrossData])
      then
        DataSet := nil
      else begin
        Dataset := nil;
        frGetDataSetAndField(lrGetUnBrackedStr(p1), DataSet, Field);
      end;
    end;
      
    if (DataSet <> nil) and (Field <> nil) and AggrBand.Visible then
    begin
      min := 1e200; max := -1e200; sum := 0; count := 0; avg := 0;
      BM:=DataSet.GetBookMark;
      DataSet.DisableControls;
      try
        DataSet.First;
        while not DataSet.Eof do
        begin
          v := 0;
          if dk <> dkCount then
          begin
            if not Field.IsNull then
              v := Field.AsFloat
            else
              v := 0;
          end;

          if v > max then max := v;
          if v < min then min := v;
          sum := sum + v;
          count := count + 1;
          DataSet.Next;
        end;
      finally
        DataSet.GotoBookMark(BM);
        DataSet.FreeBookMark(BM);
        DataSet.EnableControls;
      end;
      
      if count > 0 then
        avg := sum / count;
      d := 0;
      case dk of
        dkSum: d := sum;
        dkMin: d := min;
        dkMax: d := max;
        dkAvg: d := avg;
        dkCount: d := count;
      end;
      val := d;
    end
    else if (CurBand.View<>nil) and ((DataSet = nil) or (Field = nil)) then
    begin
      {$IFDEF DebugLR}
      DebugLn('CurBand=%s CurBand.View=%s AggrBand=%s',
        [BandInfo(CurBand),dbgsName(CurBand.View),BandInfo(AggrBand)]);
      {$ENDIF}
      s1 := Trim(string(p2));
      if s1 = '' then begin
        if (dk=dkCount) and (p1+''<>'') then
          s1 := p1
        else
          s1 := CurBand.View.Name;
      end;
      if dk <> dkCount then
        s2 := Trim(string(p3)) else
        s2 := Trim(string(p2));
      if (AggrBand.Typ in [btPageFooter, btMasterFooter, btDetailFooter,
        btSubDetailFooter, btGroupFooter, btCrossFooter, btReportSummary]) and
         ((s2 = '1') or ((s2 <> '1') and CurBand.Visible)) then
      begin
        VarName := List[FNo] + p1;
        if IsColumns then
          if AggrBand.Typ = btCrossFooter then
            VarName := VarName + '00' else
            VarName := VarName + IntToStr(CurPage.ColPos);
        {$ifdef DebugLR}
        dbgOut('VarName=', QuotedStr(VarName));
        {$endif}
        if not AggrBand.Visible and (AnsiCompareText(CurBand.View.Name, s1) = 0) then
        begin
          s1 := AggrBand.Values.Values[VarName];
          {$IFDEF DebugLR}
          dbgOut(' values[',QuotedStr(VarName),']=',QuotedStr(DecodeValue(s1)));
          {$ENDIF}
          if (s1='') or ((s1 <> '') and (s1[1] <> '1')) then
          begin
            s1 := Copy(s1, 2, 255);
            vv := 0;
            if dk <> dkCount then
              vv := frParser.Calc(p1);
            if  VarIsNull(vv) or (TVarData(vv).VType=varEmpty)  then
              vv := 0;
            {$IFDEF DebugLR}
            dbgOut(' Calc(',QuotedStr(p1),')=',varstr(vv));
            {$ENDIF}
            d := vv;
            if s1 = '' then
              if dk = dkMin then s1 := '1e200'
              else if dk = dkMax then s1 := '-1e200'
              else s1 := '0';
            v := StrToFloat(s1);
            case dk of
              dkAvg: v := v + d;
              dkCount: v := v + 1;
              dkMax: if v < d then v := d;
              dkMin: if v > d then v := d;
              dkSum: v := v + d;
            end;
            AggrBand.Values.Values[VarName] := '1' + FloatToStr(v);
            {$IFDEF DebugLR}
            dbgOut(' NewVal=',dbgs(v),' values[',Quotedstr(VarName),']=',DecodeValue(AggrBand.Values.Values[VarName]));
            {$ENDIF}
          end;
          {$ifdef DebugLR}
          DebugLn('');
          {$endif}
        end
        else if AggrBand.Visible then
        begin
          val := StrToFloatDef(Copy(AggrBand.Values.Values[VarName], 2, 255),0);
          if dk = dkAvg then
            val := val / AggrBand.Count;
          {$ifdef DebugLR}
          DebugLn('Value=%s',[Val]);
          {$endif}
        end;
      end;
    end;
  end;
  {$IFDEF DebugLR}
  DebugLnExit('TfrStdFunctionLibrary.DoFunction DONE val=%s',[val]);
  {$ENDIF}
end;

{-----------------------------------------------------------------------------}
const
  PropCount = 6;
  PropNames: Array[0..PropCount - 1] of String =
    ('Text','FontName', 'FontSize', 'FontStyle', 'FontColor', 'Adjust');

  ColNames: Array[0..16] of String =
    ('clWhite', 'clBlack', 'clMaroon', 'clGreen', 'clOlive', 'clNavy',
     'clPurple', 'clTeal', 'clGray', 'clSilver', 'clRed', 'clLime',
     'clYellow', 'clBlue', 'clFuchsia', 'clAqua', 'clTransparent');

{$WARNINGS OFF}
procedure TInterpretator.GetValue(const Name: String; var Value: Variant);
var
  t         : TfrObject;
  Prop      : String;
  PropInfo  : PPropInfo;
  St        : String;
  i         : Integer;
begin
  {$IFDEF DebugLR}
  DebugLn('TInterpretator.GetValue(',Name,') INIT');
  {$ENDIF}
  //Value := 0;
  t := CurView;
  Prop := Name;

  if frVariables.IndexOf(Name) <> -1 then
  begin
    Value := frVariables[Name];
    Exit;
  end;

  if Name = 'FREESPACE' then
  begin
    Value:=IntToStr(CurPage.CurBottomY-CurPage.CurY);
    Exit;
  end;
  
  if Pos('.', Name) <> 0 then
  begin
    //Find Object
    t := CurPage.FindRTObject(Copy(Name, 1, Pos('.', Name) - 1));
    //Property of object
    Prop:=Copy(Name, Pos('.',Name)+1,255);
  end;
  
  if not Assigned(t) then
    frParser.OnGetValue(Name, Value)
  else
  begin
     //Retreive property informations
     PropInfo:=GetPropInfo(t,Prop);
     if Assigned(PropInfo) then
     begin
        {$IFDEF DebugLR}
        DebugLn('TInterpretator.GetValue(',Name,') Prop=',Prop,
        ' Kind=',InttoStr(Ord(PropInfo^.PropType^.Kind)));
        {$ENDIF}
        Case PropInfo^.PropType^.Kind of
          tkChar,tkAString,tkWString,
          tkSString,tkLString         : begin
                                          St:=GetStrProp(t,Prop);
                                          {$IFDEF DebugLR}
                                          DebugLn('St=',St);
                                          {$ENDIF}
                                          Value:=St;
                                        end;
          tkBool,tkInt64,tkQWord,
          tkInteger                   : Value:=GetOrdProp(t,PropInfo);
          tkSet                       : begin
                                          St:=GetSetProp(t,Prop);
                                          {$IFDEF DebugLR}
                                          DebugLn('St=',St);
                                          {$ENDIF}
                                          Value:=St;
                                        end;
          tkFloat                     : Value:=GetFloatProp(t,Prop);
          tkEnumeration               : begin
                                          St:=GetEnumProp(t,Prop);
                                          {$IFDEF DebugLR}
                                          DebugLn('St=',St);
                                          {$ENDIF}
                                          Value:=St;
                                        end;
        end;
     end else
     begin
      // it's not a property of t, try with known color names first
      for i := 0 to 16 do
        if AnsiCompareText(ColNames[i], Prop) = 0 then
        begin
          // color constant found.
          if i <> 16 then
            Value := frColors[i] else
            Value := clNone;
          exit;
        end;

      // it's not a color name, try with customized properties
      // not included directly in t
      if not (t is TfrBandView) then
      begin
        for i:=0 to propcount-1 do
          if CompareText(PropNames[i], Prop)=0 then
          begin
            {$IFDEF DebugLR}
            DbgOut('A CustomField was found ', Prop);
            if i=0 then
              DbgOut(', t.memo.text=',DbgStr(t.Memo.Text));
            DebugLn;
            {$ENDIF}
            case i of
              0: Value := t.Memo.Text;
              1: Value := TfrMemoView(t).Font.Name;
              2: Value := TfrMemoView(t).Font.Size;
              3: Value := frGetFontStyle(TfrMemoView(t).Font.Style);
              4: Value := TfrMemoView(t).Font.Color;
              5: Value := TfrMemoView(t).Adjust;
            end;
            exit;
          end;
      end;

      // no luck yet, try next if it's a custom variable
      if Assigned(frParser.OnGetValue) then
        frParser.OnGetValue(Name, Value);
     end;
     {$IFDEF DebugLR}
     DebugLn('TInterpretator.GetValue(',Name,') No Propinfo for Prop=',Prop);
     {$ENDIF}

     if VarIsNull(Value) or VarIsEmpty(Value) then
     begin
      {$IFDEF DebugLR}
      DebugLn('TInterpretator.GetValue(',Name,')=NULL >> Value="',Name,'"');
      {$ENDIF}
      Value:=Name;
     end
     {$IFDEF DebugLR}
     else
      DebugLn('TInterpretator.GetValue(',Name,')=',VarToStr(Value));
     {$ENDIF}
  end;
end;
{$WARNINGS ON}

procedure TInterpretator.SetValue(const Name: String; Value: Variant);
var
  t         : TfrObject;
  Prop      : String;
  PropInfo  : PPropInfo;
  St        : String;
  i         : Integer;
begin
  {$IFDEF DebugLR}
  DebugLn('TInterpretator.SetValue(',Name,',',Value,')');

  if VarIsNull(Value) or VarIsEmpty(Value) then
        DebugLn('Value=NULL');
  {$ENDIF}

  t := CurView;
  Prop := Name;
  if Pos('.', Name) <> 0 then
  begin
    St := Copy(Name, 1, Pos('.', Name) - 1);
    {$IFDEF DebugLR}
    DebugLn('Trying to find RT Object "',St,'"');
    {$ENDIF}
    t := CurPage.FindRTObject(St);
    Prop := Copy(Name, Pos('.', Name) + 1, 255);
  end;
  {$IFDEF DebugLR}
  DebugLn('t=', dbgsName(t),' Prop=',Prop);
  {$ENDIF}

  //Retreive property informations
  if t is TfrBandView then
    t := TfrBandView(t).Parent;

  PropInfo:=GetPropInfo(t,Prop);
  if Assigned(PropInfo) then
  begin
    St:=VarToStr(Value);
    {$IFDEF DebugLR}
    DebugLn('PropInfo for ',prop,' found, Setting Value=',St);
    {$ENDIF}

    Case PropInfo^.PropType^.Kind of
      tkChar,tkAString,tkWString,
      tkSString,tkLString         : SetStrProp(t,Prop,St);
      tkBool,tkInt64,tkQWord,
      tkInteger                   : begin
                                      if AnsiCompareText(PropInfo^.PropType^.Name,'TGraphicsColor')=0 then
                                        SetOrdProp(t,PropInfo,StringToColor(St))
                                      else
                                        SetOrdProp(t,PropInfo,Value)
                                    end;
      tkSet                       : SetSetProp(t,Prop,St);
      tkFloat                     : SetFloatProp(t,Prop,Value);
      tkEnumeration               : SetEnumProp(t,Prop,St);
    end;
  end
  else
  begin
    if not (t is TfrBandView) then
    begin
      // try with customized properties not included directly in t
      for i:=0 to propcount-1 do
        if CompareText(PropNames[i], Prop)=0 then
        begin
          {$IFDEF DebugLR}
          DbgOut('A CustomField was found ', Prop);
          if i=0 then
            DbgOut(', t.memo.text=',DbgStr(t.Memo.Text),' nuevo valor=',VarToStr(Value));
          DebugLn;
          {$ENDIF}
          case i of
            0: t.Memo.Text := Value;
            1: TfrMemoView(t).Font.Name := Value;
            2: TfrMemoView(t).Font.Size := Value;
            3: TfrMemoView(t).Font.Style := frSetFontStyle(Value);
            4: TfrMemoView(t).Font.Color := Value;
            5: TfrMemoView(t).Adjust := Value;
          end;
          exit;
        end;
    end;
    // not found, treat it as a variable
    {$IFDEF DebugLR}
    DebugLn('frVariables[',Name,'] := ',Value);
    {$ENDIF}
    frVariables[Name] := Value;
    Exit;
  end;
end;

procedure TInterpretator.DoFunction(const Name: String; p1, p2, p3: Variant;
  var val: Variant);
begin
  if Name = 'NEWPAGE' then
  begin
    CurBand.ForceNewPage := True;
    Val := '0';
  end
  else if Name = 'NEWCOLUMN' then
  begin
    CurBand.ForceNewColumn := True;
    Val := '0';
  end
  else
    frParser.OnFunction(Name, p1, p2, p3, val);
end;


{----------------------------------------------------------------------------}
procedure TfrCompressor.Compress(StreamIn, StreamOut: TStream);
begin
// abstract method
end;

procedure TfrCompressor.DeCompress(StreamIn, StreamOut: TStream);
begin
// abstract method
end;

{----------------------------------------------------------------------------}
procedure DoInit;
begin
  RegisterClasses([TfrPageReport,TfrPageDialog]);
  
  frDesigner:=nil;
  
  SMemo := TStringList.Create;

  frRegisterFunctionLibrary(TfrStdFunctionLibrary);

  frParser := TfrParser.Create;
  frInterpretator := TInterpretator.Create;
  frVariables := TfrVariables.Create;
  frCompressor := TfrCompressor.Create;
  HookList := TFpList.Create;
end;

procedure DoExit;
var
  i: Integer;
begin
  SBmp.Free;
  TempBmp.Free;
  SMemo.Free;
  frProgressForm.Free;
  for i := 0 to frFunctionsCount - 1 do
    frFunctions[i].FunctionLibrary.Free;
  frParser.Free;
  frInterpretator.Free;
  frVariables.Free;
  frCompressor.Free;
  HookList.Free;
end;


{ TfrObject }

procedure TfrObject.SetMemo(const AValue: TfrMemoStrings);
begin
  if fMemo=AValue then exit;
  fMemo.Assign(AValue);
end;

procedure TfrObject.SetName(const AValue: string);
begin
  if fName=AValue then exit;

  if (frDesigner<>nil) and (CurReport<>nil) then
  begin
    if CurReport.FindObject(AValue)<>nil then
    begin
      MessageDlg(format(sDuplicatedObjectName,[AValue]),mtError,[mbOk],0);
      exit;
    end;
  end;

  fName:=AValue;
end;

procedure TfrObject.SetScript(const AValue: TfrScriptStrings);
begin
  if fScript=AValue then exit;
  fScript.Assign(AValue);
end;

//Code from FormStorage
function TfrObject.GetSaveProperty(const Prop: String; aObj : TPersistent=nil): string;
Var PropInfo  : PPropInfo;
    Obj       : TObject;
    OldSep    : char;
begin
  Result:='';

  if not Assigned(aObj) then
    aObj:=Self;
    
  Try
    PropInfo:=GetPropInfo(aObj,Prop);
    if Assigned(PropInfo) then
    begin
      Case PropInfo^.PropType^.Kind of
        tkChar,tkAString,tkWString,
        tkSString,tkLString         : Result:=GetStrProp(aObj,Prop);
        tkBool,tkInt64,tkQWord,
        tkInteger                   : begin
                                        if PropInfo^.PropType^.Name='TGraphicsColor' then
                                          Result:=ColorToString(GetOrdProp(aObj,PropInfo))
                                        else
                                          Result:=IntToStr(GetOrdProp(aObj,PropInfo));
                                      end;
        tkSet                       : Result:=GetSetProp(aObj,Prop);
        tkFloat                     : begin
                                        OldSep := DecimalSeparator;
                                        DecimalSeparator := '.';
                                        Result := FloatToStr(GetFloatProp(aObj,Prop));
                                        DecimalSeparator := OldSep;
                                      end;
        tkEnumeration               : Result:=GetEnumProp(aObj,Prop);
        tkClass                     : Begin
                                        Obj:=GetObjectProp(aObj,Prop);
                                        if Obj Is TStrings then
                                          Result:=TStrings(Obj).CommaText
                                        else
                                          Result:=Format('Object "%s" not implemented',[PropInfo^.PropType^.Name]);
                                      end;
      end;
    end
    else Result:='??';
  Except
  End;
end;

//Code from formStorage
procedure TfrObject.RestoreProperty(const Prop, aValue: String;  aObj : TPersistent=nil);
Var PropInfo  : PPropInfo;
    Obj       : TObject;
begin
  Try
    if not Assigned(aObj) then
      aObj:=Self;
      
    PropInfo:=GetPropInfo(aObj,Prop);
    if Assigned(PropInfo) then
    begin
      Case PropInfo^.PropType^.Kind of
        tkChar,tkAString,tkWString,
        tkSString,tkLString         : SetStrProp(aObj,Prop,aValue);
        tkBool,tkInt64,tkQWord,
        tkInteger                   : begin
                                        if PropInfo^.PropType^.Name='TGraphicsColor' then
                                          SetOrdProp(aObj,PropInfo,StringToColor(aValue))
                                        else
                                          SetOrdProp(aObj,PropInfo,StrToInt(aValue))
                                      end;
        tkSet                       : SetSetProp(aObj,Prop,aValue);
        tkFloat                     : SetFloatProp(aObj,Prop,StrToFloat(aValue));
        tkEnumeration               : SetEnumProp(aObj,Prop,aValue);
        tkClass                     : Begin
                                        Obj:=GetObjectProp(aObj,Prop);
                                        if Obj Is TStrings then
                                          TStrings(Obj).CommaText:=aValue;
                                      end;
      end;
    end;
  Except
  End;
end;

constructor TfrObject.Create;
begin
  inherited Create;
  fUpdate:=0;
  BaseName:='LRObj';
  fVisible:=True;
  fMemo:=TfrMemoStrings.Create;
  fScript:=TfrScriptStrings.Create;
end;

destructor TfrObject.Destroy;
begin
  fmemo.Free;
  fScript.Free;
  
  inherited Destroy;
end;

procedure TfrObject.Assign(From: TfrView);
begin
  x  := From.x;
  y  := From.y;
  dx := From.dx;
  dy := From.dy;

  Memo.Assign(From.Memo);
  Script.Assign(From.Script);
  Visible:=From.Visible;
end;

procedure TfrObject.BeginUpdate;
begin
  Inc(fUpdate)
end;

procedure TfrObject.EndUpdate;
begin
  if fUpdate>0 then
    Dec(fUpdate)
end;

procedure TfrObject.CreateUniqueName;
var i: Integer;
begin
  fName := '';

  if Assigned(CurReport) then
  begin
    for i := 1 to 10000 do
    begin
      if CurReport.FindObject(BaseName + IntToStr(i)) = nil then
      begin
        fName := BaseName + IntToStr(i);
        Exit;
      end;
    end;
  end
  else fName := BaseName + '1';
end;

procedure TfrObject.LoadFromXML(XML: TLrXMLConfig; const Path: String);
begin
  //ClassName not read here.
  Name:=XML.GetValue(Path+'Name/Value','');
  if Name='' then
    CreateUniqueName;
  Visible:=XML.GetValue(Path+'Visible/Value', true);
end;

procedure TfrObject.SaveToXML(XML: TLrXMLConfig; const Path: String);
begin
  XML.SetValue(Path+'Name/Value', GetSaveProperty('Name'));
  XML.SetValue(Path+'ClassName/Value', self.Classname);
  
  XML.SetValue(Path+'Visible/Value', GetSaveProperty('Visible'));
end;

{ TfrRect }

function TfrRect.GetRect: TRect;
begin
  Result:=Rect(Left,Top,Right,Bottom);
end;

procedure TfrRect.SetRect(const AValue: TRect);
begin
  fLeft:=aValue.Left;
  fRight:=aValue.Right;
  fBottom:=aValue.Bottom;
  fTop:=aValue.Top;
end;

{ TfrPageReport }

procedure TfrPageReport.LoadFromXML(XML: TLrXMLConfig; const Path: String);
var
  Rc   : TRect;
begin
  inherited LoadFromXML(XML, Path);


  pgSize := XML.GetValue(Path+'PgSize/Value', 0); // TODO chk
  rc.left := XML.GetValue(Path+'Margins/left/Value', 0); // TODO chk
  rc.top := XML.GetValue(Path+'Margins/Top/Value', 0); // TODO chk
  rc.Right := XML.GetValue(Path+'Margins/Right/Value', 0); // TODO chk
  rc.Bottom := XML.GetValue(Path+'Margins/Bottom/Value', 0); // TODO chk
  Margins.AsRect := rc;
  RestoreProperty('Orientation',XML.GetValue(Path+'Orientation/Value',''));

  UseMargins := XML.GetValue(Path+'UseMargins/Value', True); // TODO chk
  PrintToPrevPage := XML.GetValue(Path+'PrintToPrevPage/Value', True); // TODO chk
  ColCount := XML.GetValue(Path+'ColCount/Value', 1); // TODO chk
  ColGap := XML.GetValue(Path+'ColGap/Value', 0);
  RestoreProperty('LayoutOrder',XML.GetValue(Path+'LayoutOrder/Value','loColumns'));
  ChangePaper(pgSize, Width, Height, Orientation);
end;

procedure TfrPageReport.SavetoXML(XML: TLrXMLConfig; const Path: String);
var
  Rc   : TRect;
begin
  inherited SavetoXML(XML, Path);
  
  Rc:=Margins.AsRect;
  XML.SetValue(Path+'PgSize/Value', PgSize);
  XML.SetValue(Path+'Margins/left/Value', Rc.Left);
  XML.SetValue(Path+'Margins/Top/Value', Rc.Top);
  XML.SetValue(Path+'Margins/Right/Value', Rc. Right);
  XML.SetValue(Path+'Margins/Bottom/Value', Rc.Bottom);
  XML.SetValue(Path+'Orientation/Value', GetSaveProperty('Orientation'));
  XML.SetValue(Path+'UseMargins/Value', UseMargins);
  XML.SetValue(Path+'PrintToPrevPage/Value', PrintToPrevPage);
  XML.SetValue(Path+'ColCount/Value', ColCount);
  XML.SetValue(Path+'ColGap/Value', ColGap);
  XML.SetValue(Path+'LayoutOrder/Value', GetSaveProperty('LayoutOrder'));
end;

constructor TfrPageReport.CreatePage;
begin
  self.Create(prn.DefaultPageSize, 0, 0, poPortrait);
end;

{ TfrPageDialog }

procedure TfrPageDialog.PrepareObjects;
begin
  //Do nothing
end;

procedure TfrPageDialog.InitReport;
begin
  //inherited InitReport;
  fHasVisibleControls:=False;
end;

constructor TfrPageDialog.Create;
begin
  inherited Create;
  
  fForm   :=nil;
  BaseName:='Dialog';
  
  Width :=200;
  Height:=150;
  PageType:=ptDialog;
end;

procedure TfrPageDialog.LoadFromXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited LoadFromXML(XML, Path);
  
  XML.GetValue(Path+'Caption/Value', '');
end;

procedure TfrPageDialog.SavetoXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited SavetoXML(XML, Path);
  
  XML.SetValue(Path+'Caption/Value', Caption);
end;

{ TLrXMLConfig }

procedure TLrXMLConfig.LoadFromStream(const Stream: TStream);
begin

  Flush;
  FreeAndNil(Doc);

  if csLoading in ComponentState then
    exit;

  if assigned(Stream) and not StartEmpty then
    ReadXMLFile(Doc, Stream);

  if not Assigned(Doc) then
    Doc := TXMLDocument.Create;

  if not Assigned(Doc.DocumentElement) then
    Doc.AppendChild(Doc.CreateElement(RootName))
  else
    if Doc.DocumentElement.NodeName <> RootName then
      raise EXMLConfigError.Create(SWrongRootName);
end;

procedure TLrXMLConfig.SetValue(const APath: string; const AValue: string);
begin
  inherited SetValue(UTF8Decode(APath), UTF8Decode(AValue));
end;

function TLrXMLConfig.GetValue(const APath: string; const ADefault: string
  ): string;
var
  wValue: widestring;
begin
  if frUnWrapRead then
    result := inherited GetValue(APath, ADefault)
  else
  begin
    WValue := inherited GetValue(UTF8Decode(APath), UTF8Decode(ADefault));
    Result := UTF8Encode(WValue);
  end;
end;

initialization
  DoInit;

finalization
  DoExit;

end.

