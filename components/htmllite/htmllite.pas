{Version 7.5}
{*********************************************************}
{*                     HTMLLITE.PAS                      *}
{*              Copyright (c) 1995-2002 by               *}
{*                   L. David Baldwin                    *}
{*                 All rights reserved.                  *}
{*********************************************************}

{$DEFINE HL_LAZARUS}

{$i LiteCons.inc}

unit HTMLLite;

interface

{$IFDEF HL_LAZARUS}
uses
  Classes, SysUtils, LCLLinux, LMessages, Messages, LCLType, VCLGlobals,
  GraphType, Graphics, Controls, StdCtrls, Forms, Dialogs, ExtCtrls, Menus,
  Clipbrd, LiteUn2, LiteSubs, LiteSbs1, LitePars, LiteReadThd;
{$ELSE not HL_LAZARUS}
uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls, StdCtrls,
  LiteUn2, Forms, Dialogs, ExtCtrls, LitePars, LiteSubs, LiteReadThd, Menus,
  Clipbrd;
{$ENDIF not HL_LAZARUS}

const
  wm_FormSubmit = wm_User+100;
  wm_MouseScroll = wm_User+102;
  wm_Suspend = wm_User+103;
  wm_Terminate = wm_User+104;


type
  THTMLBorderStyle = (htFocused, htNone, htSingle);
  TRightClickParameters = Class(TObject)
    URL, Target: string;
    Image: TImageObj;
    ImageX, ImageY: integer;
    ClickWord: string;
    end;
  TRightClickEvent = procedure(Sender: TObject; Parameters: TRightClickParameters) of Object;
  THotSpotEvent = procedure(Sender: TObject; const SRC: string) of Object;
  THotSpotClickEvent = procedure(Sender: TObject; const SRC: string;
                     var Handled: boolean) of Object;
  TProcessingEvent = procedure(Sender: TObject; ProcessingOn: boolean) of Object;
  TImageClickEvent = procedure(Sender, Obj: TObject; Button: TMouseButton;
                       Shift: TShiftState; X, Y: Integer) of Object;
  TImageOverEvent = procedure(Sender, Obj: TObject; Shift: TShiftState;
                       X, Y: Integer) of Object;
  TMetaRefreshType = procedure(Sender: TObject; Delay: integer; const URL: string) of Object;

  htOptionEnum = (htOverLinksActive,htNoLinkUnderline, htShowDummyCaret, htShowVScroll);   
  ThtmlViewerOptions = set of htOptionEnum;

  TPaintPanel = class(TCustomPanel)
  private
    FOnPaint: TNotifyEvent;
    FViewer: TComponent;
    Canvas2: TCanvas;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_EraseBkgnd;
    procedure WMLButtonDblClk(var Message: TWMMouse); message WM_LButtonDblClk;
    procedure DoBackground(ACanvas: TCanvas; WmErase: boolean);
    constructor CreateIt(AOwner: TComponent; Viewer: TComponent);
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  public
    procedure Paint; override;
  end;

  T32ScrollBar = Class(TScrollBar)   {a 32 bit scrollbar}
  private
    FPosition: integer;
    FMin, FMax, FPage: integer;
    procedure SetPosition(Value: integer);
    procedure SetMin(Value: Integer);
    procedure SetMax(Value: Integer);
    procedure CNVScroll(var Message: TWMVScroll); message CN_VSCROLL;
  public
    property Position: integer read FPosition write SetPosition;
    property Min: integer read FMin write SetMin;
    property Max: integer read FMax write SetMax;
    procedure SetParams(APosition, APage, AMin, AMax: Integer);
  end;

  ThtmlFileType = (HTMLType, TextType, ImgType, OtherType);

  ThtmlLite = class(TWinControl)
  private
    ParseThreadHasTerminated: boolean;
    SuspendException: boolean;
    FProcessingOnAtSuspend: boolean;
    FParseBatch: integer; {number of chars parsed before suspend}   
    procedure ParserTerminate(Sender: TObject);
    procedure AppendStr(const S: string; ViewBottom, Finish,
      ProcessOn: boolean);
    function AppendBatch(const St: string): string;
  protected
    hlParser: ThlParser;       
    FOnDragDrop: TDragDropEvent;
    FOnDragOver: TDragOverEvent;
    OldHeight, OldWidth, OldCount, OldCharPos: integer;  
    function GetDragDrop: TDragDropEvent;
    function GetDragOver: TDragOverEvent;
    procedure SetDragDrop(const Value: TDragDropEvent);
    procedure SetDragOver(const Value: TDragOverEvent);
    procedure HTMLDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure HTMLDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
  protected
    { Private declarations }
    DontDraw: boolean;
    FTitle: String;
    FURL: String;
    FTarget: String;
    FBase, FBaseEx: String;
    FBaseTarget: String;
    FCurrentFile: String;
    FNameList: TStringList;
    FCurrentFileType: ThtmlFileType;
    FOnHotSpotCovered: THotSpotEvent;
    FOnHotSpotClick: THotSpotClickEvent;
    FOnImageRequest: TGetImageEvent;
    FOnScript: TScriptEvent;
    FOnFormSubmit: TFormSubmitEvent;
    FOnHistoryChange: TNotifyEvent;
    FOnProcessing: TProcessingEvent;
    FOnInclude: TIncludeType;
    FOnSoundRequest: TSoundType;
    FOnMeta: TMetaType;
    FOnMetaRefresh: TMetaRefreshType;
    FRefreshURL: string;
    FRefreshDelay: Integer;
    FOnRightClick: TRightClickEvent;
    FOnImageClick: TImageClickEvent;
    FOnImageOver: TImageOverEvent;
    FOnObjectClick: TObjectClickEvent;
    FHistory, FTitleHistory: TStrings;
    FPositionHistory: TFreeList;
    FHistoryIndex: integer;
    FHistoryMaxCount: integer;
    FFontName: String;
    FPreFontName: String;
    FFontColor: TColor;
    FHotSpotColor, FVisitedColor, FOverColor: TColor;
    FVisitedMaxCount: integer;
    FBackGround: TColor;
    FFontSize: integer;
    FProcessing: boolean;
    FAction, FFormTarget, FEncType, FMethod: String;
    FStringList: TStringList;
    FImageCacheCount: integer;
    FNoSelect: boolean;
    FScrollBars: TScrollStyle;
    FBorderStyle: THTMLBorderStyle;
    FDither: boolean;
    FCaretPos: integer;
    FOptions: ThtmlViewerOptions;
    sbWidth: integer;
    ScrollWidth: integer;
    MaxVertical: integer;
    MouseScrolling: boolean;
    LeftButtonDown: boolean;
    MiddleScrollOn: boolean;
    MiddleY: integer;
    Hiliting: boolean;
    {$ifdef ver100_plus}  {Delphi 3,4,5, C++Builder 3, 4}
    FCharset: TFontCharset;
    {$endif}
    FPage: integer;
    FOnMouseDouble: TMouseEvent;
    HotSpotAction: boolean;
    FMarginHeight, FMarginWidth: integer;
    FServerRoot: string;
    FSectionList: TSectionList;
    FImageStream: TMemoryStream;
    FOnExpandName: TExpandNameEvent;
    FViewBottom: boolean;
    HTMLTimer: TTimer;

    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure ScrollTo(Y: integer);
    procedure Scroll(Sender: TObject; ScrollCode: TScrollCode;
           var ScrollPos: Integer);
    procedure Layout;
    procedure SetViewImages(Value: boolean);
    function GetViewImages: boolean;
    procedure SetColor(Value: TColor);
    function GetBase: string;
    procedure SetBase(Value: string);
    function GetBaseTarget: string;
    function GetFURL: string;
    function GetTitle: string;
    function GetCurrentFile: string;
    procedure SetBorderStyle(Value: THTMLBorderStyle);
    function GetPosition: integer;
    procedure SetPosition(Value: integer);
    function GetScrollPos: integer;
    procedure SetScrollPos(Value: integer);
    function GetScrollBarRange: integer;
    procedure SetHistoryIndex(Value: integer);
    function GetFontName: TFontName;
    procedure SetFontName(Value: TFontName);
    function GetPreFontName: TFontName;
    procedure SetPreFontName(Value: TFontName);
    procedure SetFontSize(Value: integer);
    procedure SetFontColor(Value: TColor);
    procedure SetHotSpotColor(Value: TColor);
    procedure SetActiveColor(Value: TColor);
    procedure SetVisitedColor(Value: TColor);
    procedure SetVisitedMaxCount(Value: integer);
    procedure SetOnImageRequest(Handler: TGetImageEvent);
    procedure SetOnScript(Handler: TScriptEvent);
    procedure SetOnFormSubmit(Handler: TFormSubmitEvent);
    function GetOurPalette: HPalette;
    procedure SetOurPalette(Value: HPalette);
    procedure SetDither(Value: boolean);
    procedure SetCaretPos(Value: integer);
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
    procedure BackgroundChange(Sender: TObject);
    procedure SubmitForm(Sender: TObject;
      const Action, TheTarget, EncType, Method: string; Results: TStringList);
    procedure SetImageCacheCount(Value: integer);
    procedure WMFormSubmit(var Message: TMessage); message WM_FormSubmit;
    procedure WMMouseScroll(var Message: TMessage); message WM_MouseScroll;
    procedure WMSuspend(var Message: TMessage); message WM_Suspend;
    procedure WMTerminate(var Message: TMessage); message WM_Terminate;
    procedure SetSelLength(Value: integer);
    procedure SetSelStart(Value: integer);
    function GetSelLength: integer;
    function GetSelText: string;
    procedure SetNoSelect(Value: boolean);
    procedure SetHistoryMaxCount(Value: integer);
    procedure DrawBorder;
    procedure DoHilite(X, Y: integer);
    procedure SetScrollBars(Value: TScrollStyle);
    procedure SetProcessing(Value: boolean);
    function GetTarget: String;
    {$ifdef ver100_plus}  {Delphi 3,4,5, C++Builder 3, 4}
    procedure SetCharset(Value: TFontCharset);
    {$endif}
    function GetFormControlList: TList;
    function GetNameList: TStringList;
    function GetLinkList: TList;
    procedure SetMarginWidth(Value: integer);
    procedure SetMarginHeight(Value: integer);
    procedure SetServerRoot(Value: string);
    procedure SetOnObjectClick(Handler: TObjectClickEvent);
    procedure FormControlEnterEvent(Sender: TObject);
    procedure HandleMeta(Sender: TObject; const HttpEq,
      {$IFDEF HL_LAZARUS}NewName{$ELSE}Name{$ENDIF}, Content: string);
    procedure SetOptions(Value: ThtmlViewerOptions);
    procedure SetOnExpandName(Handler: TExpandNameEvent);
    function GetWordAtCursor(X, Y: integer; var St, En: integer;
                                            var AWord: string): boolean;
    procedure HTMLTimerTimer(Sender: TObject);
    procedure InitLoad;   

  protected
    { Protected declarations }
    PaintPanel: TPaintPanel;
    BorderPanel: TPanel;
    VScrollBar: T32ScrollBar;
    HScrollBar: TScrollBar;
    Sel1: integer;
    Visited: TStringList;     {visited URLs}
    ParseThread: TParseThread;

    procedure DoLogic(StartY, StartScroll, StartCount, StartCharPos: integer);
    procedure DoScrollBars;
    procedure SetupAndLogic(StartY, StartScroll, StartCount, StartCharPos: integer);
    function GetURL(X, Y: integer; var UrlTarg: TUrlTarget;
             var FormControl: TImageFormControlObj): boolean;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function GetPalette: HPALETTE; override;
    procedure HTMLPaint(Sender: TObject); virtual;
    procedure HTMLMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
{$ifdef ver120_plus}
    procedure HTMLMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
{$endif}
    procedure HTMLMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer); virtual;
    procedure HTMLMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure HTMLMouseDblClk(Message: TWMMouse);
    procedure URLAction; virtual;
    function HotSpotClickHandled: boolean; dynamic;
    procedure LoadFile(const FileName: string; ft: ThtmlFileType);
    procedure LoadString(const Source, Reference: string; ft: ThtmlFileType);
    procedure PaintWindow(DC: HDC); override;
    procedure UpdateImageCache;
    procedure AddVisitedLink(const S: string);
    procedure CheckVisitedLinks;
    {$IFDEF HL_LAZARUS}
    procedure InitializeWnd; override;
    {$ENDIF}
  public
    { Public declarations }
    FrameOwner: TObject;
    FMarginHeightX, FMarginWidthX: integer;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function HTMLExpandFilename(const Filename: string): string; virtual;
    procedure LoadFromFile(const FileName: string);
    procedure LoadTextFile(const FileName: string);
    procedure LoadImageFile(const FileName: string);
    procedure LoadStrings(const Strings: TStrings; const Reference: string);
    procedure LoadTextStrings(const Strings: TStrings);
    procedure LoadFromString(const S, Reference: string);    
    procedure LoadTextFromString(const S: string);    
    procedure LoadFromStream(const AStream: TStream; const Reference: string);
    function PositionTo(Dest: string): boolean;
    function Find(const S: String; MatchCase: boolean): boolean;
    procedure Clear; virtual;
    procedure CopyToClipboard;
    procedure SelectAll;
    procedure ClearHistory;
    procedure Reload;
    procedure BumpHistory(const FileName, Title: string;
                 OldPos: integer; ft: ThtmlFileType);
    function GetSelTextBuf(Buffer: PChar; BufSize: integer): integer;
    function InsertImage(const Src: string; Stream: TMemoryStream): boolean;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure Repaint; override;
    function FindSourcePos(DisplayPos: integer): integer;
    function FindDisplayPos(SourcePos: integer; Prev: boolean): integer;
    function DisplayPosToXy(DisplayPos: integer; var X, Y: integer): boolean;
    function PtInObject(X, Y: integer; var Obj: TObject): boolean;  {X, Y, are client coord}
    procedure SignalSuspend;  
    procedure InitStr(ft: ThtmlFileType);        
    procedure InitiateAppend(Ref: string);
    procedure AppendString(const S: string; ViewBottom, Finish: boolean);

    property DocumentTitle: string read GetTitle;
    property URL: string read GetFURL;
    property Base: string read GetBase write SetBase;
    property BaseTarget: string read GetBaseTarget;
    property Position: integer read GetPosition write SetPosition;
    property VScrollBarPosition: integer read GetScrollPos write SetScrollPos;
    property VScrollBarRange: integer read GetScrollBarRange;
    property CurrentFile: string read GetCurrentFile;
    property History: TStrings read FHistory;
    property TitleHistory: TStrings read FTitleHistory;
    property HistoryIndex: integer read FHistoryIndex write SetHistoryIndex;
    property Processing: boolean read FProcessing;
    property SelStart: integer read FCaretPos write SetSelStart;
    property SelLength: integer read GetSelLength write SetSelLength;
    property SelText: string read GetSelText;
    property Target: string read GetTarget;
    property Palette: HPalette read GetOurPalette write SetOurPalette;
    property Dither: boolean read FDither write SetDither default True;
    property CaretPos: integer read FCaretPos write SetCaretPos;
    property FormControlList: TList read GetFormControlList;
    property NameList: TStringList read GetNameList;
    property LinkList: TList read GetLinkList;
    property SectionList: TSectionList read FSectionList;
    property OnExpandName: TExpandNameEvent read FOnExpandName write SetOnExpandName;

  published
    { Published declarations }
    property OnHotSpotCovered: THotSpotEvent read FOnHotSpotCovered
             write FOnHotSpotCovered;
    property OnHotSpotClick: THotSpotClickEvent read FOnHotSpotClick
             write FOnHotSpotClick;
    property OnImageRequest: TGetImageEvent read FOnImageRequest
             write SetOnImageRequest;
    property OnScript: TScriptEvent read FOnScript
             write SetOnScript;
    property OnFormSubmit: TFormSubmitEvent read FOnFormSubmit
             write SetOnFormSubmit;
    property OnHistoryChange: TNotifyEvent read FOnHistoryChange
             write FOnHistoryChange;
    property ViewImages: boolean read GetViewImages write SetViewImages default True;
    property Enabled;
    property TabStop;
    property TabOrder;
    property Align;
    property Name;
    property Tag;
    property PopupMenu;
    property ShowHint;
    property Height default 150;
    property Width default 150;
    property DefBackground: TColor read FBackground write SetColor default clBtnFace;
    property BorderStyle: THTMLBorderStyle read FBorderStyle write SetBorderStyle;
    property Visible;
    property HistoryMaxCount: integer read FHistoryMaxCount write SetHistoryMaxCount;
    property DefFontName: TFontName read GetFontName write SetFontName;
    property DefPreFontName: TFontName read GetPreFontName write SetPreFontName;
    property DefFontSize: integer read FFontSize write SetFontSize default 12;
    property DefFontColor: TColor read FFontColor write SetFontColor
             default clBtnText;
    property DefHotSpotColor: TColor read FHotSpotColor write SetHotSpotColor
             default clBlue;
    property DefVisitedLinkColor: TColor read FVisitedColor write SetVisitedColor
             default clPurple;
    property DefOverLinkColor: TColor read FOverColor write SetActiveColor
             default clBlue;
    property VisitedMaxCount: integer read FVisitedMaxCount write SetVisitedMaxCount default 50;
    property ImageCacheCount: integer read FImageCacheCount
                write SetImageCacheCount default 5;
    property NoSelect: boolean read FNoSelect write SetNoSelect;
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars default ssBoth;
    {$ifdef ver100_plus}  {Delphi 3,4,5, C++Builder 3, 4}
    property CharSet: TFontCharset read FCharSet write SetCharset;
    {$endif}
    property MarginHeight: integer read FMarginHeight write SetMarginHeight default 5;
    property MarginWidth: integer read FMarginWidth write SetMarginWidth default 10;
    property ServerRoot: string read FServerRoot write SetServerRoot;
    property htOptions: ThtmlViewerOptions read FOptions write SetOptions;

    property OnMouseMove;
    property OnMouseUp;
    property OnMouseDown;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnEnter;
    property OnProcessing: TProcessingEvent read FOnProcessing write FOnProcessing;
    property OnInclude: TIncludeType read FOnInclude write FOnInclude;
    property OnSoundRequest: TSoundType read FOnSoundRequest write FOnSoundRequest;
    property OnMeta: TMetaType read FOnMeta write FOnMeta;
    property OnMetaRefresh: TMetaRefreshType read FOnMetaRefresh write FOnMetaRefresh;
    property OnImageClick: TImageClickEvent read FOnImageClick write FOnImageClick;
    property OnImageOver: TImageOverEvent read FOnImageOver write FOnImageOver;
    property OnObjectClick: TObjectClickEvent read FOnObjectClick write SetOnObjectClick;
    property OnRightClick:  TRightClickEvent read FOnRightClick write FOnRightClick;
    property OnMouseDouble: TMouseEvent read FOnMouseDouble write FOnMouseDouble;
    property OnDragDrop: TDragDropEvent read GetDragDrop write SetDragDrop;
    property OnDragOver: TDragOverEvent read GetDragOver write SetDragOver;
    property ParseBatch: integer read FParseBatch write FParseBatch;
    end;


procedure Register;

implementation

const
  MaxHScroll = 6000;  {max horizontal display in pixels}
  VScale = 1;
  ScrollGap = 20;

type
  PositionObj = class(TObject)
    Pos: integer;
    FileType: ThtmlFileType;
    end;

constructor ThtmlLite.Create(AOwner: TComponent);
begin
inherited Create(AOwner);
ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
  csSetCaption, csDoubleClicks];
Height := 150;
Width := 150;
{$ifdef ver100_plus}  {Delphi 3,4,5, C++Builder 3, 4}
FCharset := DEFAULT_CHARSET;
{$endif}
FMarginHeight := 5;
FMarginWidth := 10;

hlParser := ThlParser.Create;  

BorderPanel := TPanel.Create(Self);
BorderPanel.BevelInner := bvNone;
BorderPanel.BevelOuter := bvNone;
BorderPanel.Ctl3D := False;
BorderPanel.ParentColor := True;
BorderPanel.Align := alClient;
BorderPanel.ParentCtl3D := False;

InsertControl(BorderPanel);

PaintPanel := TPaintPanel.CreateIt(Self, Self);
PaintPanel.ParentFont := False;
InsertControl(PaintPanel);
PaintPanel.Top := 1;
PaintPanel.Left := 1;
PaintPanel.BevelOuter := bvNone;
PaintPanel.BevelInner := bvNone;
PaintPanel.ctl3D := False;
PaintPanel.ParentColor := True;

PaintPanel.OnPaint := {$IFDEF HL_LAZARUS}@{$ENDIF}HTMLPaint;
PaintPanel.OnMouseDown := {$IFDEF HL_LAZARUS}@{$ENDIF}HTMLMouseDown;
PaintPanel.OnMouseMove := {$IFDEF HL_LAZARUS}@{$ENDIF}HTMLMouseMove;
PaintPanel.OnMouseUp := {$IFDEF HL_LAZARUS}@{$ENDIF}HTMLMouseUp;
{$ifdef ver120_plus}
OnMouseWheel := {$IFDEF HL_LAZARUS}@{$ENDIF}HTMLMouseWheel;
{$endif}

VScrollBar := T32ScrollBar.Create(Self);
VScrollBar.Kind := sbVertical;
VScrollBar.SmallChange := 16;
VScrollBar.Visible := False;
VScrollBar.TabStop := False;
sbWidth := VScrollBar.Width;
InsertControl(VScrollBar);

HScrollBar := TScrollBar.Create(Self);
HScrollBar.Kind := sbHorizontal;
HScrollBar.SmallChange := 15;
HScrollBar.OnScroll := {$IFDEF HL_LAZARUS}@{$ENDIF}Scroll;
HScrollBar.Visible := False;
HScrollBar.TabStop := False;
InsertControl(HScrollBar);

FScrollBars := ssBoth;

FSectionList := TSectionList.Create(Self, PaintPanel);
FSectionList.ControlEnterEvent := {$IFDEF HL_LAZARUS}@{$ENDIF}FormControlEnterEvent;
FSectionList.OnBackgroundChange := {$IFDEF HL_LAZARUS}@{$ENDIF}BackgroundChange;
FSectionList.ShowImages := True;
FSectionList.Parser := hlParser;    

FNameList := TStringList.Create;
FNameList.Sorted := True;
DefBackground := clBtnFace;
DefFontColor := clBtnText;
DefHotSpotColor := clBlue;
DefOverLinkColor := clBlue;
DefVisitedLinkColor := clPurple;
FVisitedMaxCount := 50;
DefFontSize := 12;
DefFontName := 'Times New Roman';
DefPreFontName := 'Courier New';
SetImageCacheCount(5);

FBase := '';
FBaseEx := '';
FBaseTarget := '';
FCurrentFile := '';
FTitle := '';
FURL := '';
FTarget := '';

FHistory := TStringList.Create;
FPositionHistory := TFreeList.Create;
FTitleHistory := TStringList.Create;
FDither := True;
FParseBatch := 0;  {number of chars parsed before suspend}   

Visited := TStringList.Create;

HTMLTimer := TTimer.Create(Self);
HTMLTimer.Enabled := False;
HTMLTimer.Interval := 200;
HTMLTimer.OnTimer := {$IFDEF HL_LAZARUS}@{$ENDIF}HTMLTimerTimer;
end;

destructor ThtmlLite.Destroy;
begin
FSectionList.Free;
FNameList.Free;
FHistory.Free;
FPositionHistory.Free;
FTitleHistory.Free;
Visited.Free;
HTMLTimer.Free;
hlParser.Free;
inherited Destroy;
end;

procedure ThtmlLite.SetupAndLogic(StartY, StartScroll, StartCount, StartCharPos: integer);
begin
FTitle := hlParser.Title;
if hlParser.Base <> '' then
  FBase :=  hlParser.Base
else FBase := FBaseEx;
FBaseTarget := hlParser.BaseTarget;
try
  DontDraw := True;
  {Load the background bitmap if any and if ViewImages set}
  FSectionList.GetBackgroundBitmap;

DoLogic(StartY, StartScroll, StartCount, StartCharPos);

finally
  DontDraw := False;
  end;
end;

function ThtmlLite.AppendBatch(const St: string): string;
var
  Tmp: string;
begin
if FParseBatch > 0 then
  begin
  Result := St;
  Tmp :=  Copy(Result, 1, FParseBatch);
  Delete(Result, 1, FParseBatch);
  end
else    {FParseBatch <= 0, Append all that remains}
  begin
  Tmp := St;
  Result := '';
  end;
AppendStr(Tmp, False, Result='', True);
end;

{----------------ThtmlLite.LoadTextFromString}
procedure ThtmlLite.LoadTextFromString(const S: string);    
begin
LoadString(S, '', TextType);
end;

{----------------ThtmlLite.LoadFromString}
procedure ThtmlLite.LoadFromString(const S, Reference: string);    
begin
LoadString(S, Reference, HTMLType);
end;

{----------------ThtmlLite.LoadString}
procedure ThtmlLite.LoadString(const Source, Reference: string; ft: ThtmlFileType);
var
  I: integer;
  Dest, FName, OldFile: string;
  St: string;

begin
FName := Reference;
I := Pos('#', FName);
if I > 0 then
  begin
  Dest := copy(FName, I+1, 255);  {positioning information}
  System.Delete(FName, I, 255);
  end
else Dest := '';
FRefreshDelay := 0;
try
  OldFile := FCurrentFile;
  FCurrentFile := ExpandFileName(FName);
  FCurrentFileType := ft;
  InitStr(ft);
  DontDraw := True;
  try
    St := Source;
    St := AppendBatch(St);
    DontDraw := False;
    while (St <> '') and not SuspendException do
      St := AppendBatch(St);
  finally
    DontDraw := False;  {make sure it's off}
    while not ParseThreadHasTerminated do
      begin
      Application.ProcessMessages;
      Sleep(0);
      end;
    CheckVisitedLinks;
    if (Dest <> '') and PositionTo(Dest) then  {change position, if applicable}
    else if FCurrentFile <> OldFile then
       begin
       ScrollTo(0);
       HScrollBar.Position := 0;
       end;
    {else if same file leave position alone}
    PaintPanel.Invalidate;
  end;
finally
  SetProcessing(False);
  end;
if (FRefreshDelay > 0) and Assigned(FOnMetaRefresh) then
  FOnMetaRefresh(Self, FRefreshDelay, FRefreshURL);
end;

procedure ThtmlLite.LoadFile(const FileName: string; ft: ThtmlFileType);
var
  I: integer;
  Dest, FName, OldFile: string;
  FS: TFileStream;
  St: string;

begin
IOResult;   {eat up any pending errors}
FName := FileName;
I := Pos('#', FName);
if I > 0 then
  begin
  Dest := copy(FName, I+1, 255);  {positioning information}
  System.Delete(FName, I, 255);
  end
else Dest := '';
FRefreshDelay := 0;
try
  if not FileExists(FName) then
    Raise(EInOutError.Create('Can''t locate file: '+FName));
  try
    OldFile := FCurrentFile;
    FCurrentFile := ExpandFileName(FName);
    FCurrentFileType := ft;
    if ft in [HTMLType, TextType] then
      begin
      FS := TFileStream.Create(FName, fmOpenRead or fmShareDenyWrite);
      try
        SetLength(St, FS.Size);
        FS.ReadBuffer(St[1], FS.Size);
      finally
        FS.Free;
        end;
      end
    else St := '';
    DontDraw := True;
    if ft in [HTMLType, TextType] then
      begin
      InitStr(ft);
      St := AppendBatch(St);
      end
    else
      begin
      St := '<img src="'+FName+'">';
      InitStr(HTMLType);
      AppendStr(St, False, True, True);
      St := '';
      end;
    DontDraw := False;  
    while (St <> '') and not SuspendException do
      St := AppendBatch(St);
  finally
    DontDraw := False;  
    while not ParseThreadHasTerminated do
      begin
      Application.ProcessMessages;
      Sleep(0);
      end;
    CheckVisitedLinks;
    if (Dest <> '') and PositionTo(Dest) then  {change position, if applicable}
    else if FCurrentFile <> OldFile then
       begin
       ScrollTo(0);
       HScrollBar.Position := 0;
       end;
    {else if same file leave position alone}
    PaintPanel.Invalidate;
  end;
finally
  SetProcessing(False);
  end;
if (FRefreshDelay > 0) and Assigned(FOnMetaRefresh) then
  FOnMetaRefresh(Self, FRefreshDelay, FRefreshURL);
end;

procedure ThtmlLite.LoadFromFile(const FileName: string);
var
  OldFile, OldTitle: string;
  OldPos: integer;
  OldType: ThtmlFileType;
begin
if FProcessing then Exit;
if Filename <> '' then
  begin
  OldFile := FCurrentFile;
  OldTitle := FTitle;
  OldPos := Position;
  OldType := FCurrentFileType;
  LoadFile(FileName, HTMLType);
  if (OldFile <> FCurrentFile) or (OldType <> FCurrentFileType) then
    BumpHistory(OldFile, OldTitle, OldPos, OldType);
  end;
end;

{----------------ThtmlLite.LoadTextFile}
procedure ThtmlLite.LoadTextFile(const FileName: string);
var
  OldFile, OldTitle: string;
  OldPos: integer;
  OldType: ThtmlFileType;
begin
if FProcessing then Exit;
if Filename <> '' then
  begin
  OldFile := FCurrentFile;
  OldTitle := FTitle;
  OldPos := Position;
  OldType := FCurrentFileType;
  LoadFile(FileName, TextType);
  if (OldFile <> FCurrentFile) or (OldType <> FCurrentFileType) then
    BumpHistory(OldFile, OldTitle, OldPos, OldType);
  end;
end;

{----------------ThtmlLite.LoadImageFile}
procedure ThtmlLite.LoadImageFile(const FileName: string);
var
  OldFile, OldTitle: string;
  OldPos: integer;
  OldType: ThtmlFileType;

begin
if FProcessing then Exit;
if Filename <> '' then
  begin
  OldFile := FCurrentFile;
  OldTitle := FTitle;
  OldPos := Position;
  OldType := FCurrentFileType;
  LoadFile(FileName, ImgType);
  if (OldFile <> FCurrentFile) or (OldType <> FCurrentFileType) then
    BumpHistory(OldFile, OldTitle, OldPos, OldType);
  end;
end;

{----------------ThtmlLite.LoadStrings}
procedure ThtmlLite.LoadStrings(const Strings: TStrings; const Reference: string);
begin
LoadString(Strings.Text, Reference, HTMLType);
end;

{----------------ThtmlLite.LoadTextStrings}
procedure ThtmlLite.LoadTextStrings(const Strings: TStrings);
begin
LoadString(Strings.Text, '', TextType);
end;

{----------------ThtmlLite.InitStr}
procedure ThtmlLite.InitStr(ft: ThtmlFileType);
begin
if FProcessing then Exit;
SetProcessing(True);
try
  if Assigned(ParseThread) then   
    begin
    ParseThreadHasTerminated := False;
    AppendStr('', False, True, False);
    While not ParseThreadHasTerminated do
      begin
      Application.ProcessMessages;
      Sleep(0);
      end;
    end;
  FRefreshDelay := 0;
  InitLoad;
  MaxVertical := 0;
  CaretPos := 0;
  Sel1 := -1;
  OldHeight := 0;
  OldWidth := 0;
  OldCount := 0;
  if Assigned(FOnSoundRequest) then
    FOnSoundRequest(Self, '', 0, True);
  ParseThread := TParseThread.Create(True);
  with ParseThread do
    begin
    FreeOnTerminate := True;  
    Parser := hlParser;
    ASectionList := FSectionList;
    AIncludeEvent := FOnInclude;
    ASoundEvent := FOnSoundRequest;
    AMetaEvent := {$IFDEF HL_LAZARUS}@{$ENDIF}HandleMeta;
    ANameList := FNameList;
    Text := ft = TextType;
    OnTerminate := {$IFDEF HL_LAZARUS}@{$ENDIF}ParserTerminate;
    ParseThreadHasTerminated := False;
    SuspendException := False;
    end;
  hlParser.ParseThread := ParseThread;
  hlParser.AllowSuspend := True;
  ParseThread.Resume;
except
  SetProcessing(False);
  Raise;
  end;
end;

{----------------ThtmlLite.AppendString}
procedure ThtmlLite.AppendString(const S: string; ViewBottom, Finish: boolean);
begin
AppendStr(S, ViewBottom, Finish, False);  {Processing off on Suspend}
end;

{----------------ThtmlLite.AppendStr}
procedure ThtmlLite.AppendStr(const S: string; ViewBottom, Finish, ProcessOn: boolean);
begin
If ((S = '') and not Finish) or ParseThreadHasTerminated or not Assigned(ParseThread) then
  Exit;
while not ParseThreadHasTerminated and not ParseThread.Suspended and not SuspendException do
  begin
  Application.ProcessMessages;
  Sleep(0);
  end;
if ParseThreadHasTerminated then
  Exit;
if not SuspendException then
  begin
  FViewBottom := ViewBottom;
  ParseThread.AddString(S);
  SetProcessing(True);
  FProcessingOnAtSuspend := ProcessOn;
  hlParser.AllowSuspend := not Finish;
  end
else hlParser.AllowSuspend := False;
ParseThread.Resume;
end;

procedure ThtmlLite.SignalSuspend;
begin
PostMessage(handle, wm_Suspend, 0, 0);
end;

{----------------ThtmlLite.WMSuspend}
procedure ThtmlLite.WMSuspend(var Message: TMessage);
{Parser is waiting for more text}
begin
if FSectionList.Count > 0 then
  begin
  try
    SetupAndLogic(OldHeight, OldWidth, OldCount, OldCharPos);
  except
    SuspendException:= True;
    if not FProcessingOnAtSuspend then
      SetProcessing(False);   {turn off Processing for AppendString}
    Raise;
    end;

  if FViewBottom then
    begin
    VScrollBarPosition := MaxVertical;
    HScrollBar.Position := 0;
    end;
  PaintPanel.Invalidate;
  if not FProcessingOnAtSuspend then
    SetProcessing(False);   {turn off Processing for AppendString}
  end;
end;

procedure ThtmlLite.WMTerminate(var Message: TMessage);
begin
if (FSectionList.Count > 0) and not SuspendException then
  begin
  SetupAndLogic(OldHeight, OldWidth, OldCount, OldCharPos);
  if FViewBottom then
    begin
    VScrollBarPosition := MaxVertical;
    HScrollBar.Position := 0;
    end;
  PaintPanel.Invalidate;
  end;
SetProcessing(False);
end;

procedure ThtmlLite.ParserTerminate(Sender: TObject);
begin
ParseThreadHasTerminated := True;
ParseThread := Nil;  {it's still there, though}
PostMessage(handle, wm_Terminate, 0, 0);
end;

{----------------ThtmlLite.InitiateAppend}
procedure ThtmlLite.InitiateAppend(Ref: string);
begin
InitStr(HTMLType);
end;

{----------------ThtmlLite.LoadFromStream}
procedure ThtmlLite.LoadFromStream(const AStream: TStream; const Reference: string);
var
  Stream: TMemoryStream;
  S: string;
begin
Stream := TMemoryStream.Create;
try
  Stream.LoadFromStream(AStream);
  SetLength(S, Stream.Size);
  Move(Stream.Memory^, S[1], Stream.Size);
finally
  Stream.Free;
  end;
LoadString(S, Reference, HTMLType);
ScrollTo(0);
HScrollBar.Position := 0;
end;

{----------------ThtmlLite.DoScrollBars}
procedure ThtmlLite.DoScrollBars;
var
  VBar, HBar: boolean;
  Wid, HWidth, WFactor: integer;
  ScrollInfo :TScrollInfo;

begin
VBar := False;
ScrollWidth := IntMin(ScrollWidth, MaxHScroll);
if FBorderStyle = htNone then
  begin
  WFactor := 0;
  PaintPanel.Top := 0;
  PaintPanel.Left := 0;
  BorderPanel.Visible := False;
  end
else
  begin
  WFactor := 1;
  PaintPanel.Top := 1;
  PaintPanel.Left := 1;
  BorderPanel.Visible := False;
  BorderPanel.Visible := True;
  end;
if FScrollBars in [ssBoth, ssVertical] then
  begin  {assume a vertical scrollbar}
  VBar := (MaxVertical >= Height-2) or
          ((FScrollBars in [ssBoth, ssHorizontal]) and
           (MaxVertical >= Height-2-sbWidth) and
           (ScrollWidth+2*FMarginWidthX > Width-sbWidth));
  HBar := (FScrollBars in [ssBoth, ssHorizontal]) and
          ((ScrollWidth+2*FMarginWidthX > Width) or
           ((VBar or (htShowVScroll in FOptions)) and   
               (ScrollWidth+2*FMarginWidthX > Width-sbWidth)));
  end
else
  begin  {there is no vertical scrollbar}
  HBar := (FScrollBars in [ssBoth, ssHorizontal]) and
          (ScrollWidth+2*FMarginWidthX > Width);
  end;
if VBar or ((htShowVScroll in FOptions) and (FScrollBars in [ssBoth, ssVertical])) then  
  Wid := Width - sbWidth
else
  Wid := Width;
PaintPanel.Width := Wid - 2*WFactor;
if HBar then
  PaintPanel.Height := Height - 2*WFactor - sbWidth
else
  PaintPanel.Height := Height - 2*WFactor;
HWidth := IntMax(ScrollWidth+2*FMarginWidthX, Wid-2*WFactor);
HScrollBar.Visible := HBar;
HScrollBar.LargeChange := IntMax(1, Wid - 20);
HScrollBar.SetBounds(WFactor, Height-sbWidth-WFactor, Wid -WFactor, sbWidth);
VScrollBar.SetBounds(Width-sbWidth-WFactor, WFactor, sbWidth, Height - 2*WFactor);
VScrollBar.LargeChange := PaintPanel.Height div VScale - VScrollBar.SmallChange;
if htShowVScroll in FOptions then
  begin
  VScrollBar.Visible := ( FScrollBars in [ssBoth, ssVertical] );
  VScrollBar.Enabled := VBar;
  end
else VScrollBar.Visible := VBar;

HScrollBar.Max := IntMax(0, HWidth);
VScrollBar.SetParams(VScrollBar.Position, PaintPanel.Height+1, 0, MaxVertical);
ScrollInfo.cbSize := SizeOf(ScrollInfo);
ScrollInfo.fMask := SIF_PAGE;
ScrollInfo.nPage := Wid;
SetScrollInfo(HScrollBar.Handle,SB_CTL,ScrollInfo,TRUE);
end;

{----------------ThtmlLite.DoLogic}
procedure ThtmlLite.DoLogic(StartY, StartScroll, StartCount, StartCharPos: integer);
var
  Curs: integer;
  Wid, WFactor: integer;
begin
ScrollWidth := StartScroll;
Curs := StartCharPos;
HandleNeeded;
try
  DontDraw := True;
  if FBorderStyle = htNone then WFactor := 1
    else WFactor := 3;
  if FScrollBars in [ssBoth, ssVertical] then
    begin  {assume a vertical scrollbar}
    Wid := Width - sbWidth - WFactor;
    OldHeight := FSectionList.DoLogic(PaintPanel.Canvas, FMarginHeightX,
            Wid-2*FMarginWidthX, ScrollWidth, Curs, StartY, StartCount);
    MaxVertical := OldHeight + 2*FMarginHeight;
    DoScrollBars;
    end
  else
    begin  {there is no vertical scrollbar}
    Wid := Width-WFactor;
    OldHeight := FSectionList.DoLogic(PaintPanel.Canvas, FMarginHeightX,
            Wid-2*FMarginWidthX, ScrollWidth, Curs, StartY, StartCount);
    MaxVertical := OldHeight + FMarginHeight;
    DoScrollBars;
    end;
  OldWidth := ScrollWidth;
  OldCount := FSectionList.Count;
  OldCharPos := Curs;
  if Cursor = crIBeam then
    Cursor := ThickIBeamCursor;
finally

  DontDraw := False;
  end;
end;

procedure ThtmlLite.HTMLPaint(Sender: TObject);
var
  ARect: TRect;
begin
if not DontDraw then
  begin
  ARect := Rect(FMarginWidthX, 1, PaintPanel.Width, PaintPanel.Height);
writeln('ThtmlLite.HTMLPaint A ',ARect.Left,',',ARect.Top,',',ARect.Right,',',ARect.Bottom);
  FSectionList.Draw(PaintPanel.Canvas2, ARect, MaxHScroll,
                        FMarginWidthX - HScrollBar.Position, FMarginHeightX);
  end;
end;

procedure ThtmlLite.WMSize(var Message: TWMSize);
begin
inherited;
if not FProcessing then
  Layout
else
  DoScrollBars;
if MaxVertical < PaintPanel.Height then
  Position := 0
else ScrollTo(VScrollBar.Position * integer(VScale));   {keep aligned to limits}
with HScrollBar do
  Position := IntMin(Position, Max - PaintPanel.Width);
end;

procedure ThtmlLite.Scroll(Sender: TObject; ScrollCode: TScrollCode;
       var ScrollPos: Integer);
{only the 32 bit horizontal scrollbar comes here}
begin
SetFocus;
ScrollPos := IntMin(ScrollPos, HScrollBar.Max - PaintPanel.Width);
PaintPanel.Invalidate;
end;

procedure ThtmlLite.ScrollTo(Y: integer);
begin
Y := IntMin(Y, MaxVertical - PaintPanel.Height);
Y := IntMax(Y, 0);
VScrollBar.Position := Y;
FSectionList.SetYOffset(Y);
Invalidate;
end;

procedure ThtmlLite.Layout;
var
  OldPos: integer;
begin
if FProcessing then Exit;
SetProcessing(True);
try
  OldPos := Position;
  DoLogic(0, 0, 0, 0);
  Position := OldPos;   {return to old position after width change}
finally
  SetProcessing(False);
  end;
end;

function ThtmlLite.HotSpotClickHandled: boolean;
var
  Handled: boolean;
begin
Handled := False;
if Assigned(FOnHotSpotClick) then
  FOnHotSpotClick(Self, URL, Handled);
Result := Handled;
end;

procedure ThtmlLite.URLAction;
var
  S, Dest: string;
  Ext: string[5];
  I: integer;
  OldPos: integer;

begin
if not HotSpotClickHandled then
  begin
  OldPos := Position;
  S := URL;
  I := Pos('#', S);  {# indicates a position within the document}
  if I = 1 then
    begin
    if PositionTo(S) then    {no filename with this one}
      begin
      BumpHistory(FCurrentFile, FTitle, OldPos, FCurrentFileType);
      AddVisitedLink(FCurrentFile+S);
      end;
    end
  else
    begin
    if I >= 1 then
      begin
      Dest := System.Copy(S, I, 255);  {local destination}
      S := System.Copy(S, 1, I-1);     {the file name}
      end
    else
      Dest := '';    {no local destination}
    S := HTMLExpandFileName(S);
    Ext := Uppercase(ExtractFileExt(S));
    if (Ext = '.HTM') or (Ext = '.HTML')  then
      begin              {an html file}
      if S <> FCurrentFile then
        begin
        LoadFromFile(S + Dest);
        AddVisitedLink(S+Dest);
        end
      else
        if PositionTo(Dest) then   {file already loaded, change position}
          begin
          BumpHistory(FCurrentFile, FTitle, OldPos, HTMLType);
          AddVisitedLink(S+Dest);
          end;
      end
    else if (Ext = '.BMP') or (Ext = '.GIF') or (Ext = '.JPG') or (Ext = '.JPEG')
                or (Ext = '.PNG') then
      LoadImageFile(S);
    end;
    {Note: Self may not be valid here}
  end;
end;

{----------------ThtmlLite.AddVisitedLink}
procedure ThtmlLite.AddVisitedLink(const S: string);
var
  I, J: integer;
  S1, UrlTmp: string;
begin
if Assigned(FrameOwner) or (FVisitedMaxCount = 0) then
  Exit;      {TFrameViewer will take care of visited links}
I := Visited.IndexOf(S);
if I = 0 then Exit
else if I < 0 then
  begin
  for J := 0 to SectionList.LinkList.Count-1 do
    with TFontObj(SectionList.LinkList[J]) do
      begin
      UrlTmp := Url;
      if Length(UrlTmp) > 0 then
        begin
        if Url[1] = '#' then
          S1 := FCurrentFile+UrlTmp
        else
          S1 := HTMLExpandFilename(UrlTmp);
        if CompareText(S, S1) = 0 then
          Visited := True;
        end;
      end;
  end
else Visited.Delete(I);   {thus moving it to the top}
Visited.Insert(0, S);
for I :=  Visited.Count-1 downto FVisitedMaxCount do
  Visited.Delete(I);
end;

{----------------ThtmlLite.CheckVisitedLinks}
procedure ThtmlLite.CheckVisitedLinks;
var
  I, J: integer;
  S, S1: string;
begin
if FVisitedMaxCount = 0 then
  Exit;
for I := 0 to Visited.Count-1 do
  begin
  S := Visited[I];
  for J := 0 to SectionList.LinkList.Count-1 do
    with TFontObj(SectionList.LinkList[J]) do
      begin
      if (Url <> '') and (Url[1] = '#') then
        S1 := FCurrentFile+Url
      else
        S1 := HTMLExpandFilename(Url);
      if CompareText(S, S1) = 0 then
        Visited := True;
      end;
  end;
end;

{$IFDEF HL_LAZARUS}
procedure ThtmlLite.InitializeWnd;
begin
  inherited InitializeWnd;
  DoScrollBars;
end;
{$ENDIF}

procedure ThtmlLite.HTMLMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  XR, CaretHt: integer;
  YR: integer;
  Cell1: TObject;
  InText: boolean;
begin
inherited MouseDown(Button, Shift, X, Y);

SetFocus;
HotSpotAction := False;
if MiddleScrollOn then
  begin
  MiddleScrollOn := False;
  PaintPanel.Cursor := Cursor;
  MouseScrolling := False;
  end
else if (Button = mbMiddle) then
  begin
  MiddleScrollOn := True;
  MiddleY := Y;
  PaintPanel.Cursor := UpDownCursor;
  end
else if (Button = mbLeft) then
  begin
  LeftButtonDown := True;
  HiLiting := True;
  with FSectionList do
    begin
    Sel1 := FindCursor(PaintPanel.Canvas, X, Y+YOff-FMarginHeightX, XR, YR, CaretHt, Cell1, InText);
    if Sel1 > -1 then
      begin
      if SelB <> SelE then
        InvalidateRect(PaintPanel.Handle, Nil, True);
      SelB := Sel1;
      SelE := Sel1;
      CaretPos := Sel1;
      end;
    end;
  end;
end;

procedure ThtmlLite.HTMLTimerTimer(Sender: TObject);
var
  Pt: TPoint;
begin
if GetCursorPos(Pt) and (WindowFromPoint(Pt) <> PaintPanel.Handle) then
  begin
  SectionList.CancelActives;
  HTMLTimer.Enabled := False;
  if FURL <> '' then
    begin
    FURL := '';
    FTarget := '';
    if Assigned(FOnHotSpotCovered) then FOnHotSpotCovered(Self, '');
    end;
  end;
end;

function ThtmlLite.PtInObject(X, Y: integer; var Obj: TObject): boolean;  {X, Y, are client coord}
var
  IX, IY: integer;
begin
Result := PtInRect(ClientRect, Point(X, Y)) and
            FSectionList.PtInObject(X, Y+FSectionList.YOff-FMarginHeightX, Obj, IX, IY);  
end;

procedure ThtmlLite.HTMLMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  UrlTarget : TUrlTarget;
  CurUrl, CurTarget: string;
  FormControl: TImageFormControlObj;
  Obj: TObject;
  IX, IY: integer;
  XR, CaretHt: integer;
  YR: integer;
  Cell1: TObject;
  InText: boolean;
  NextCursor: TCursor;
begin
Inherited MouseMove(Shift,X,Y);

if MiddleScrollOn then
  begin
  if not MouseScrolling and (Abs(Y-MiddleY) > ScrollGap) then
    begin
    MouseScrolling := True;
    PostMessage(Handle, wm_MouseScroll, 0, 0);
    end;
  Exit;
  end;

UrlTarget := Nil;
CurURL := '';
NextCursor := crArrow;
if GetURL(X, Y, UrlTarget, FormControl) then
  begin
  NextCursor := HandCursor;
  if not Assigned(FormControl) then
    begin
    CurUrl := UrlTarget.Url;
    CurTarget := UrlTarget.Target;
    UrlTarget.Free;
    end;
  end;
if (Assigned(FOnImageClick) or Assigned(FOnImageOver)) and
     FSectionList.PtInObject(X, Y+FSectionList.YOff-FMarginHeightX, Obj, IX, IY) then
  begin
  if NextCursor <> HandCursor then  {in case it's also a Link}
    NextCursor := crArrow;
  if Assigned(FOnImageOver) then FOnImageOver(Self, Obj, Shift, IX, IY);
  end
else if (FSectionList.FindCursor(PaintPanel.Canvas, X, Y+FSectionList.YOff-FMarginHeightX, XR, YR, CaretHt, Cell1, InText) >= 0)
          and InText and (NextCursor <> HandCursor) then
  NextCursor := Cursor;

PaintPanel.Cursor := NextCursor;
SetCursor(Screen.Cursors[NextCursor]);

if ((NextCursor = HandCursor) or (SectionList.ActiveImage <> Nil)) then
  HTMLTimer.Enabled := True
else HTMLTimer.Enabled := False;

if (CurURL <> FURL) or (CurTarget <> FTarget) then
  begin
  FURL := CurURL;
  FTarget := CurTarget;
  if Assigned(FOnHotSpotCovered) then FOnHotSpotCovered(Self, CurURL);
  end;
if (ssLeft in Shift) and not MouseScrolling and not FNoSelect
       and ((Y <= 0) or (Y >= Self.Height)) then
  begin
  MouseScrolling := True;
  PostMessage(Handle, wm_MouseScroll, 0, 0);
  end;
if (ssLeft in Shift) and not FNoSelect then
  DoHilite(X, Y);
inherited MouseMove(Shift, X, Y);
end;

procedure ThtmlLite.HTMLMouseUp(Sender: TObject; Button: TMouseButton;
            Shift: TShiftState; X, Y: Integer);
var
  UrlTarget: TUrlTarget;
  FormControl: TImageFormControlObj;
  Obj: TObject;
  IX, IY: integer;
  InImage: boolean;
  Parameters: TRightClickParameters;
  AWord: string;
  St, En: integer;
begin
if MiddleScrollOn then
  begin
  {cancel unless it's middle button and has moved}
  if (Button <> mbMiddle) or (Y <> MiddleY) then
    begin
    MiddleScrollOn := False;
    PaintPanel.Cursor := Cursor;
    end;
  Exit;
  end;

inherited MouseUp(Button, Shift, X, Y);

if Assigned(FOnImageClick) or Assigned(FOnRightClick) then
  begin
  InImage := FSectionList.PtInObject(X, Y+FSectionList.YOff-FMarginHeightX, Obj, IX, IY);
  if Assigned(FOnImageClick) and InImage then
    FOnImageClick(Self, Obj, Button, Shift, IX, IY);
  if (Button = mbRight) and Assigned(FOnRightClick) then
    begin
    Parameters := TRightClickParameters.Create;
    try
      if InImage then
        begin
        Parameters.Image := Obj as TImageObj;
        Parameters.ImageX := IX;
        Parameters.ImageY := IY;
        end;
      if GetURL(X, Y, UrlTarget, FormControl) and (UrlTarget <> Nil) then
        begin
        Parameters.URL := UrlTarget.URL;
        Parameters.Target := UrlTarget.Target;
        UrlTarget.Free;
        end;
      if GetWordAtCursor(X, Y, St, En, AWord) then
        Parameters.ClickWord := AWord;
      FOnRightClick(Self, Parameters);
    finally
      Parameters.Free;
      end;
    end;
  end;

if Button = mbLeft then
  begin
  MouseScrolling := False;
  DoHilite(X, Y);
  Hiliting := False;
  if LeftButtonDown and GetURL(X, Y, UrlTarget, FormControl) then
    begin
    LeftButtonDown := False;
    if Assigned(FormControl) then
      FormControl.ImageClick
    else if (FSectionList.SelE <= FSectionList.SelB) then
      begin
      FURL := UrlTarget.URL;
      FTarget := UrlTarget.Target;
      UrlTarget.Free;
      HotSpotAction := True;   {prevent double click action}
      URLAction;
      {Note:  Self pointer may not be valid after URLAction call (TFrameViewer, HistoryMaxCount=0)}
      end;
    end;
  LeftButtonDown := False;
  end;
end;

{----------------ThtmlLite.HTMLMouseWheel}
{$ifdef ver120_plus}
procedure ThtmlLite.HTMLMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
VScrollBarPosition := VScrollBarPosition - WheelDelta div 2;
Handled := True;
end;
{$endif}

{----------------ThtmlLite.GetWordAtCursor}
function ThtmlLite.GetWordAtCursor(X, Y: integer; var St, En: integer;
                                        var AWord: string): boolean;
const
  AlphNum = ['a'..'z', 'A'..'Z', '0'..'9', #192..#255];  {changed in 7.2}
var
  XR, X1, CaretHt: integer;
  YR, Y1: integer;
  Cell1: TObject;
  Obj: TObject;
  Ch: char;
  InText: boolean;

  function GetCh(Pos: integer): char;
  var
    Ch: char;
    Obj1: TObject;
  begin
  Result := ' ';
  if not FSectionList.GetChAtPos(Pos, Ch, Obj1) or (Obj1 <> Obj) then Exit;
  Result := Ch;
  end;

begin
Result := False;
AWord := '';
with FSectionList do
  begin
  InText := False;
  CaretPos := FindCursor(PaintPanel.Canvas, X,
         Y+YOff-FMarginHeightX, XR, YR, CaretHt, Cell1, InText);
  CursorToXy(PaintPanel.Canvas, CaretPos, X1, Y1);
  if InText then   {else cursor is past end of row}
    begin
    en := CaretPos;
    st := en-1;
    if GetChAtPos(en, Ch, Obj) and (Ch in AlphNum) then
      begin
      AWord := Ch;
      Result := True;
      Inc(en);
      Ch := GetCh(en);
      while Ch in AlphNum do
        begin
        AWord := AWord + Ch;
        Inc(en);
        Ch := GetCh(en);
        end;
      if St >= 0 then
        begin
        Ch := GetCh(st);
        while (st >= 0) and (Ch in AlphNum) do
          begin
          System.Insert(Ch, AWord, 1);
          Dec(st);
          if St >= 0 then
            Ch := GetCh(St);
          end;
        end;
      end;
    end;
  end;
end;

{----------------ThtmlLite.HTMLMouseDblClk}
procedure ThtmlLite.HTMLMouseDblClk(Message: TWMMouse);
var
  st, en: integer;
  AWord: string;
begin
if FProcessing or HotSpotAction then Exit;
if not FNoSelect and GetWordAtCursor(Message.XPos, Message.YPos, St, En, AWord) then
  begin
  FSectionList.SelB := st+1;
  FSectionList.SelE := en;
  FCaretPos := st+1;
  InvalidateRect(PaintPanel.Handle, Nil, True);
  end;
if Assigned(FOnMouseDouble) then
  with Message do
    FOnMouseDouble(Self, mbLeft, KeysToShiftState(Keys), XPos, YPos);
end;

procedure ThtmlLite.DoHilite(X, Y: integer);
var
  Curs, YR, YWin: integer;
  CursCell: TObject;
  XR, CaretHt: integer;
  InText: boolean;
begin
if Hiliting and (Sel1 >= 0) then
  with FSectionList do
    begin
    CursCell := Nil;
    YWin := IntMin(IntMax(0, Y), Height);
    Curs := FindCursor(PaintPanel.Canvas, X, YWin+YOff-FMarginHeightX, XR, YR, CaretHt, CursCell, InText);
    if (Curs >= 0) and not FNoSelect then
      begin
      if Curs > Sel1 then
        begin
        SelE := Curs;
        SelB := Sel1;
        end
      else
        begin
        SelB := Curs;
        SelE := Sel1;
        end;
      InvalidateRect(PaintPanel.Handle, Nil, True);
      end;
    CaretPos := Curs;
    end;
end;

{----------------ThtmlLite.WMMouseScroll}
procedure ThtmlLite.WMMouseScroll(var Message: TMessage);
const
    Ticks: DWord = 0;
var
  Pos: integer;
  Pt: TPoint;
begin
GetCursorPos(Pt);
Ticks := 0;
with VScrollBar do
  begin
  Pt := PaintPanel.ScreenToClient(Pt);
  while MouseScrolling and (LeftButtonDown and((Pt.Y <= 0) or (Pt.Y > Self.Height)))
                  or (MiddleScrollOn and (Abs(Pt.Y - MiddleY) > ScrollGap)) do
    begin
    if GetTickCount > Ticks +100 then
      begin
      Ticks := GetTickCount;
      Pos := Position;
      if LeftButtonDown then
        begin
        if Pt.Y < -15 then
          Pos := Position - SmallChange * 8
        else if Pt.Y <= 0 then
          Pos := Position - SmallChange
        else if Pt.Y > Self.Height+15 then
          Pos := Position + SmallChange * 8
        else
          Pos := Position + SmallChange;
        end
      else
        begin   {MiddleScrollOn}
        if Pt.Y-MiddleY < -3*ScrollGap then
          Pos := Position - 32
        else if Pt.Y-MiddleY < -ScrollGap then
          Pos := Position - 8
        else if Pt.Y-MiddleY > 3*ScrollGap then
          Pos := Position + 32
        else if Pt.Y-MiddleY > ScrollGap then
          Pos := Position + 8;
        if Pos < Position then
          PaintPanel.Cursor := UpOnlyCursor
        else if Pos > Position then
          PaintPanel.Cursor := DownOnlyCursor;
        end;
      Pos := IntMax(0, IntMin(Pos, MaxVertical - PaintPanel.Height));
      FSectionList.SetYOffset(Pos * integer(VScale));
      SetPosition(Pos);
      DoHilite(Pt.X, Pt.Y);
      PaintPanel.Invalidate;
      GetCursorPos(Pt);
      Pt := PaintPanel.ScreenToClient(Pt);
      end;
    Application.ProcessMessages;
    Application.ProcessMessages;
    Application.ProcessMessages;
    Application.ProcessMessages;
    end;
  end;
MouseScrolling := False;
if MiddleScrollOn then
  PaintPanel.Cursor := UpDownCursor;
end;

function ThtmlLite.PositionTo(Dest: string): boolean;
var
  I: integer;
begin
Result := False;
If Dest = '' then Exit;
if Dest[1] = '#' then
  System.Delete(Dest, 1, 1);
I := FNameList.IndexOf(UpperCase(Dest));
if I > -1 then
  begin
  ScrollTo(TSectionBase(FNameList.Objects[I]).YValue);
  HScrollBar.Position := 0;
  Result := True;
  AddVisitedLink(FCurrentFile+'#'+Dest);
  end;
end;

function ThtmlLite.GetURL(X, Y: integer; var UrlTarg: TUrlTarget;
          var FormControl: TImageFormControlObj): boolean;
begin
Result := FSectionList.GetURL(PaintPanel.Canvas, X, Y+FSectionList.YOff-FMarginHeightX,
          UrlTarg, FormControl);
end;

procedure ThtmlLite.SetViewImages(Value: boolean);
var
  OldPos: integer;
  OldCursor: TCursor;
begin
if (Value <> FSectionList.ShowImages) and not FProcessing then
  begin
  OldCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;
    SetProcessing(True);
    FSectionList.ShowImages := Value;
    if FSectionList.Count > 0 then
      begin
      FSectionList.GetBackgroundBitmap;    {load any background bitmap}
      OldPos := Position;
      DoLogic(0, 0, 0, 0);
      Position := OldPos;
      Invalidate;
      end;
  finally
    Screen.Cursor := OldCursor;
    SetProcessing(False);
    end;
  end;
end;

{----------------ThtmlLite.InsertImage}
function ThtmlLite.InsertImage(const Src: string; Stream: TMemoryStream): boolean;
var
  OldPos: integer;
  ReFormat: boolean;
begin
Result := False;
if FProcessing then Exit;
try
  SetProcessing(True);
  FSectionList.InsertImage(Src, Stream, Reformat);
  FSectionList.GetBackgroundBitmap;     {in case it's the one placed}
  if Reformat then
    if FSectionList.Count > 0 then
      begin
      FSectionList.GetBackgroundBitmap;    {load any background bitmap}
      OldPos := Position;
      DoLogic(0, 0, 0, 0);
      Position := OldPos;
      end;
  Invalidate;
finally
  SetProcessing(False);
  Result := True;
  end;
end;

function ThtmlLite.GetBase: string;
begin
Result := FBase;
end;

procedure ThtmlLite.SetBase(Value: string);
begin
FBase := Value;
FBaseEx := Value;
end;

function ThtmlLite.GetBaseTarget: string;
begin
Result := FBaseTarget;
end;

function ThtmlLite.GetTitle: string;
begin
Result := FTitle;
end;

function ThtmlLite.GetCurrentFile: string;
begin
Result := FCurrentFile;
end;

function ThtmlLite.GetFURL: string;
begin
Result := FURL;
end;

function ThtmlLite.GetTarget: string;
begin
Result := FTarget;
end;

function ThtmlLite.GetViewImages: boolean;
begin
Result := FSectionList.ShowImages;
end;

procedure ThtmlLite.SetColor(Value: TColor);
begin
if FProcessing then Exit;
FBackground := Value;
FSectionList.Background:= Value;
Color := Value;
Invalidate;
end;

procedure ThtmlLite.SetBorderStyle(Value: THTMLBorderStyle);
begin
if Value <> FBorderStyle then
  begin
  FBorderStyle := Value;
  DrawBorder;
  end;
end;

procedure ThtmlLite.KeyDown(var Key: Word; Shift: TShiftState);
var
  Pos: integer;
  OrigPos:    integer;
  TheChange: integer;
begin
inherited KeyDown(Key, Shift);
if MiddleScrollOn then    {v7.2}
  begin
  MiddleScrollOn := False;
  PaintPanel.Cursor := Cursor;
  Exit;
  end;
with VScrollBar do
  if Key in [VK_PRIOR, VK_NEXT, VK_UP, VK_DOWN, VK_HOME, VK_END] then
    begin
    Pos := Position;
    OrigPos := Pos;
    case Key of
      VK_PRIOR : Dec(Pos, LargeChange);
      VK_NEXT  : Inc(Pos, LargeChange);
      VK_UP    : Dec(Pos, SmallChange);
      VK_DOWN  : Inc(Pos, SmallChange);
      VK_Home  : Pos := 0;
      VK_End   : Pos := MaxVertical div VScale;
      end;
    if Pos < 0 then Pos := 0;
    Pos := IntMax(0, IntMin(Pos, MaxVertical - PaintPanel.Height));

    Position := Pos;
    FSectionList.SetYOffset(Pos * integer(VScale));
    TheChange := OrigPos-Pos;
    if abs(TheChange) = SmallChange then begin
       ScrollWindow(PaintPanel.Handle, 0, TheChange*VScale, NIL, NIL);
       PaintPanel.Update;
    end else
       PaintPanel.Invalidate;
    end;
with HScrollBar do
  if Key in [VK_LEFT, VK_RIGHT] then
    begin
    Pos := Position;
    case Key of
      VK_LEFT  : Dec(Pos, SmallChange);
      VK_RIGHT : Inc(Pos, SmallChange);
      end;
    if Pos < 0 then Pos := 0;
    Pos := IntMin(Pos, Max - PaintPanel.Width);
    Position := Pos;
    PaintPanel.Invalidate;
    end;
end;

procedure ThtmlLite.WMGetDlgCode(var Message: TMessage);
begin
Message.Result := DLGC_WantArrows;  {else don't get the arrow keys}
end;

function ThtmlLite.GetPosition: integer;
var
  Index: integer;
  TopPos, Pos: integer;
  S: TSectionBase;
begin
Pos := integer(VScrollBar.Position) * VScale;
S:= FSectionList.FindSectionAtPosition(Pos, TopPos, Index);
if Assigned(S) then
  Result := integer(Index) shl 16 + ((Pos - TopPos) and $FFFF)
else Result := 0;
{Hiword is section #, Loword is displacement from top of section}
end;

procedure ThtmlLite.SetPosition(Value: integer);
var
  TopPos: integer;
begin
if (Value >= 0) and (Hiword(Value) < FSectionList.Count) then
  begin
  TopPos := FSectionList.FindPositionByIndex(HiWord(Value));
  ScrollTo(TopPos + LoWord(Value));
  end;
end;

function ThtmlLite.GetScrollPos: integer;
begin
Result := VScrollBar.Position;
end;

procedure ThtmlLite.SetScrollPos(Value: integer);
begin
if Value < 0 then Value := 0;
Value := IntMin(Value, VScrollBar.Max);
if Value <> GetScrollPos then
  ScrollTo(integer(Value) * VScale);
end;

function ThtmlLite.GetScrollBarRange: integer;
begin
Result := MaxVertical - PaintPanel.Height;
end;

function ThtmlLite.GetPalette: HPALETTE;
begin
if ThePalette <> 0 then
  Result := ThePalette
else Result := inherited GetPalette;
Invalidate;
end;

function ThtmlLite.HTMLExpandFilename(const Filename: string): string;
begin
Result := HTMLServerToDos(Trim(Filename), FServerRoot);

if Pos('\', Result) = 1 then
  Result := ExpandFilename(Result)
else if (Pos(':', Result)<> 2) and (Pos('\\', Result) <> 1) then
  if CompareText(FBase, 'DosPath') = 0 then  {let Dos find the path}
  else if FBase <> '' then
    Result := ExpandFilename(HTMLToDos(FBase) + Result)
  else
    Result := ExpandFilename(ExtractFilePath(FCurrentFile) + Result);
end;

{----------------ThtmlLite.BumpHistory}
procedure ThtmlLite.BumpHistory(const FileName, Title: string;
              OldPos: integer; ft: ThtmlFileType);
var
  I: integer;
  PO: PositionObj;
begin
if (FHistoryMaxCount > 0) and  (FCurrentFile <> '') and
         ((FileName <> FCurrentFile) or (FCurrentFileType <> ft)
         or (OldPos <> Position)) then
  with FHistory do
    begin
    if (Count > 0) and (Filename <> '') then
      begin
      Strings[FHistoryIndex] := Filename;
      with PositionObj(FPositionHistory[FHistoryIndex]) do
        begin
        Pos := OldPos;
        FileType := ft;
        end;
      FTitleHistory[FHistoryIndex] := Title;
      for I := 0 to FHistoryIndex-1 do
        begin
        Delete(0);
        FTitleHistory.Delete(0);
        PositionObj(FPositionHistory[0]).Free;
        FPositionHistory.Delete(0);
        end;
      end;
    FHistoryIndex := 0;
    Insert(0, FCurrentFile);
    PO := PositionObj.Create;
    PO.Pos := Position;
    PO.FileType := FCurrentFileType;
    FPositionHistory.Insert(0, PO);
    FTitleHistory.Insert(0, FTitle);
    if Count > FHistoryMaxCount then
      begin
      Delete(FHistoryMaxCount);
      FTitleHistory.Delete(FHistoryMaxCount);
      PositionObj(FPositionHistory[FHistoryMaxCount]).Free;
      FPositionHistory.Delete(FHistoryMaxCount);
      end;
    if Assigned(FOnHistoryChange) then FOnHistoryChange(Self);
    end;
end;

procedure ThtmlLite.SetHistoryIndex(Value: integer);
begin
with FHistory do
  if (Value <> FHistoryIndex) and (Value >= 0) and (Value < Count)
            and not FProcessing then
    begin
    if FCurrentFile <> '' then
      begin
      Strings[FHistoryIndex] := FCurrentFile;
      with PositionObj(FPositionHistory[FHistoryIndex]) do
        begin
        Pos := Position;
        FileType := FCurrentFileType;
        end;
      FTitleHistory[FHistoryIndex] := FTitle;
      end;
    with PositionObj(FPositionHistory[Value]) do
      begin
      if (FCurrentFile <> Strings[Value]) or (FCurrentFileType <> FileType) then
        Self.LoadFile(Strings[Value], FileType);
      Position := Pos;
      end;
    FHistoryIndex := Value;
    if Assigned(FOnHistoryChange) then FOnHistoryChange(Self);
    end;
end;

procedure ThtmlLite.SetHistoryMaxCount(Value: integer);
begin
if (Value = FHistoryMaxCount) or (Value < 0) then Exit;
if Value < FHistoryMaxCount then
  ClearHistory;
FHistoryMaxCount := Value;
end;

procedure ThtmlLite.ClearHistory;
var
  CountWas: integer;
begin
CountWas := FHistory.Count;
FHistory.Clear;
FTitleHistory.Clear;
FPositionHistory.Clear;
FHistoryIndex := 0;
if (CountWas > 0) and Assigned(FOnHistoryChange) then
  FOnHistoryChange(Self);
end;

function ThtmlLite.GetFontName: TFontName;
begin
Result := FFontName;
end;

procedure ThtmlLite.SetFontName(Value: TFontName);
begin
if  CompareText(Value, FSectionList.FontName) <> 0 then
  begin
  FFontName := Value;
  FSectionList.FontName := Value;
  FSectionList.UpdateFonts;
  if FSectionList.Count > 0 then
    Layout;
  Invalidate;
  end;
end;

function ThtmlLite.GetPreFontName: TFontName;
begin
Result := FPreFontName;
end;

procedure ThtmlLite.SetPreFontName(Value: TFontName);
begin
if  CompareText(Value, FSectionList.PreFontName) <> 0 then
  begin
  FPreFontName := Value;
  FSectionList.PreFontName := Value;
  FSectionList.UpdateFonts;
  if FSectionList.Count > 0 then
    Layout;
  Invalidate;
  end;
end;

procedure ThtmlLite.SetFontSize(Value: integer);
begin
Value := IntMax(Value, 6);  {minimum value of 6 pts}
FFontSize := Value;
FSectionList.FontSize := Value;
FSectionList.UpdateFonts;
if FSectionList.Count > 0 then
  Layout;
Invalidate;
end;

{$ifdef ver100_plus}  {Delphi 3,4,5, C++Builder 3, 4}
procedure ThtmlLite.SetCharset(Value: TFontCharset);
begin
FCharset := Value;
FSectionList.Charset := Value;
FSectionList.UpdateFonts;
if FSectionList.Count > 0 then
  Layout;
Invalidate;
end;
{$endif}

function ThtmlLite.GetFormControlList: TList;
begin
Result := FSectionList.FormControlList;
end;

function ThtmlLite.GetNameList: TStringList;
begin
Result := FNameList;
end;

function ThtmlLite.GetLinkList: TList;
begin
Result := FSectionList.LinkList;
end;

procedure ThtmlLite.SetFontColor(Value: TColor);
begin
FFontColor := Value;
FSectionList.FontColor := Value;
FSectionList.UpdateFonts;
Invalidate;
end;

procedure ThtmlLite.SetHotSpotColor(Value: TColor);
begin
FHotSpotColor := Value;
FSectionList.HotSpotColor := Value;
FSectionList.UpdateFonts;
Invalidate;
end;

procedure ThtmlLite.SetVisitedColor(Value: TColor);
begin
FVisitedColor := Value;
FSectionList.LinkVisitedColor := Value;
FSectionList.UpdateFonts;
Invalidate;
end;

procedure ThtmlLite.SetActiveColor(Value: TColor);
begin
FOverColor := Value;
FSectionList.LinkActiveColor := Value;
FSectionList.UpdateFonts;
Invalidate;
end;

procedure ThtmlLite.SetVisitedMaxCount(Value: integer);
var
  I: integer;
begin
Value := IntMax(Value, 0);
if Value <> FVisitedMaxCount then
  begin
  FVisitedMaxCount := Value;
  if FVisitedMaxCount = 0 then
    begin
    Visited.Clear;
    for I := 0 to SectionList.LinkList.Count-1 do
      TFontObj(LinkList[I]).Visited := False;
    Invalidate;
    end
  else
    begin
    FVisitedMaxCount := Value;
    for I := Visited.Count-1 downto FVisitedMaxCount do
      Visited.Delete(I);
    end;
  end;
end;

procedure ThtmlLite.BackgroundChange(Sender: TObject);
begin
Color := (Sender as TSectionList).Background or $2000000;
end;

procedure ThtmlLite.SetOnImageRequest(Handler: TGetImageEvent);
begin
FOnImageRequest := Handler;
FSectionList.GetImage := Handler;
end;

procedure ThtmlLite.SetOnExpandName(Handler: TExpandNameEvent);
begin
FOnExpandName := Handler;
FSectionList.ExpandName := Handler;
end;

procedure ThtmlLite.SetOnScript(Handler: TScriptEvent);
begin
FOnScript := Handler;
FSectionList.ScriptEvent := Handler;
end;

procedure ThtmlLite.SetOnObjectClick(Handler: TObjectClickEvent);
begin
FOnObjectClick := Handler;
FSectionList.ObjectClick := Handler;
end;

procedure ThtmlLite.SetOnFormSubmit(Handler: TFormSubmitEvent);
begin
FOnFormSubmit := Handler;
if Assigned(Handler) then
  FSectionList.SubmitForm := {$IFDEF HL_LAZARUS}@{$ENDIF}SubmitForm
else FSectionList.SubmitForm := Nil;
end;

procedure ThtmlLite.SubmitForm(Sender: TObject;
  const Action, TheTarget, EncType, Method: string; Results: TStringList);
begin
if Assigned(FOnFormSubmit) then
  begin
  FAction := Action;
  FMethod := Method;
  FFormTarget := TheTarget;
  FEncType:= EncType;
  FStringList := Results;
  PostMessage(Handle, wm_FormSubmit, 0, 0);
  end;
end;

procedure ThtmlLite.WMFormSubmit(var Message: TMessage);
begin
FOnFormSubmit(Self, FAction, FFormTarget, FEncType, FMethod, FStringList);
end;     {user disposes of the TStringList}

function ThtmlLite.Find(const S: String; MatchCase: boolean): boolean;
var
  ChArray: array[0..256] of char;
  Curs: integer;
  X: integer;
  Y, Pos: integer;
begin
Result := False;
if S = '' then Exit;
StrPCopy(ChArray, S);
with FSectionList do
  begin
  Curs := FindString(CaretPos, ChArray, MatchCase);
  if Curs >= 0 then
    begin
    Result := True;
    SelB := Curs;
    SelE := Curs+Length(S);
    CaretPos := SelE;
    if CursorToXY(PaintPanel.Canvas, Curs, X, Y) then
      begin
      Pos := VScrollBarPosition * integer(VScale);
      if (Y < Pos) or
             (Y > Pos +ClientHeight-20) then
        VScrollBarPosition := (Y - ClientHeight div 2) div VScale;
      Invalidate;
      end;
    end;
  end;
end;

procedure ThtmlLite.FormControlEnterEvent(Sender: TObject);
var
  Y, Pos: integer;
begin
if Sender is TFormControlObj then
  begin
  Y := TFormControlObj(Sender).YValue;
  Pos := VScrollBarPosition * integer(VScale);
  if (Y < Pos) or (Y > Pos +ClientHeight-20) then
    begin
    VScrollBarPosition := (Y - ClientHeight div 2) div VScale;
    Invalidate;
    end;
  end;
end;

procedure ThtmlLite.SelectAll;
var
  SB: TSectionBase;
begin
with FSectionList do
  if (Count > 0) and not FNoSelect then
    begin
    SelB := 0;
    SB := TSectionBase(Items[Count-1]);
    with SB do
      SelE := StartCurs + Len;
    Invalidate;
    end;
end;

{----------------ThtmlLite.InitLoad}
procedure ThtmlLite.InitLoad;   
begin
FSectionList.Clear;
UpdateImageCache;
FSectionList.SetFonts(FFontName, FPreFontName, FFontSize, FFontColor,
                      FHotSpotColor, FVisitedColor, FOverColor, FBackground,
                        htOverLinksActive in FOptions);
FNameList.Clear;
FMarginWidthX := FMarginWidth;
FMarginHeightX := FMarginHeight;
end;

{----------------ThtmlLite.Clear}
procedure ThtmlLite.Clear;
{Note: because of Frames do not clear history list here}
begin
if FProcessing then Exit;
HTMLTimer.Enabled := False;
FSectionList.Clear;
FSectionList.SetFonts(FFontName, FPreFontName, FFontSize, FFontColor,
                      FHotSpotColor, FVisitedColor, FOverColor, FBackground,
                        htOverLinksActive in FOptions);
FNameList.Clear;
FBase := '';
FBaseEx := '';
FBaseTarget := '';
FTitle := '';
VScrollBar.Max := 0;
VScrollBar.Visible := False;
VScrollBar.Height := PaintPanel.Height;
HScrollBar.Visible := False;
CaretPos := 0;
Sel1 := -1;
if Assigned(FOnSoundRequest) then
  FOnSoundRequest(Self, '', 0, True);
Invalidate;
end;

procedure ThtmlLite.PaintWindow(DC: HDC);
begin
PaintPanel.RePaint;
VScrollbar.RePaint;
HScrollbar.RePaint;
end;

procedure ThtmlLite.CopyToClipboard;
begin
Clipboard.SetTextBuf(PAnsiChar(GetSelText));
end;

function ThtmlLite.GetSelTextBuf(Buffer: PChar; BufSize: integer): integer;
begin
if BufSize <= 0 then Result := 0
else Result := FSectionList.GetSelTextBuf(Buffer, BufSize);
end;

function ThtmlLite.GetSelText: string;
var
  Len: integer;
begin
Len := FSectionList.GetSelLength;
if Len > 0 then
  begin
  SetString(Result, Nil, Len);
  FSectionList.GetSelTextBuf(Pointer(Result), Len+1);
  end
else Result := '';
end;

function ThtmlLite.GetSelLength: integer;
begin
with FSectionList do
  if FCaretPos = SelB then
    Result := SelE - SelB
  else
    Result := SelB - SelE;
end;

procedure ThtmlLite.SetSelLength(Value: integer);
begin
with FSectionList do
  begin
  if Value >= 0 then
    begin
    SelB := FCaretPos;
    SelE := FCaretPos + Value;
    end
  else
    begin
    SelE := FCaretPos;
    SelB := FCaretPos + Value;
    end;
  Invalidate;
  end;
end;

procedure ThtmlLite.SetSelStart(Value: integer);
begin
with FSectionList do
  begin
  CaretPos := Value;
  SelB := Value;
  SelE := Value;
  Invalidate;
  end;
end;

procedure ThtmlLite.SetNoSelect(Value: boolean);
begin
if Value <> FNoSelect then
  begin
  FNoSelect := Value;
  if Value = True then
    begin
    FSectionList.SelB := -1;
    FSectionList.SelE := -1;
    RePaint;
    end;
  end;
end;

procedure ThtmlLite.UpdateImageCache;
begin
BitmapList.BumpAndCheck;
end;

procedure ThtmlLite.SetImageCacheCount(Value: integer);
begin
Value := IntMax(0, Value);
Value := IntMin(20, Value);
if Value <> FImageCacheCount then
  begin
  FImageCacheCount := Value;
  BitmapList.SetCacheCount(FImageCacheCount);
  end;
end;

procedure ThtmlLite.DrawBorder;
begin
if (Focused and (FBorderStyle = htFocused)) or (FBorderStyle = htSingle)
       or (csDesigning in ComponentState) then
  BorderPanel.BorderStyle := bsSingle
else
  BorderPanel.BorderStyle := bsNone;
BorderPanel.Invalidate;
end;

procedure ThtmlLite.DoEnter;
begin
inherited DoEnter;
DrawBorder;
end;

procedure ThtmlLite.DoExit;
begin
inherited DoExit;
DrawBorder;
end;

procedure ThtmlLite.SetScrollBars(Value: TScrollStyle);
begin
if (Value <> FScrollBars) then
  begin
  FScrollBars := Value;
  if not (csLoading in ComponentState) and HandleAllocated then
    begin
    SetProcessing(True);
    try
      DoLogic(0, 0, 0, 0);
    finally
      SetProcessing(False);
      end;
    Invalidate;
    end;
  end;
end;

{----------------ThtmlLite.Reload}
procedure ThtmlLite.Reload;     {reload the last file}
var
  Pos: integer;
begin
if FCurrentFile <> '' then
  begin
  Pos := Position;
  if FCurrentFileType = HTMLType then
    LoadFromFile(FCurrentFile)
  else if FCurrentFileType = TextType then
    LoadTextFile(FCurrentFile)
  else LoadImageFile(FCurrentFile);
  Position := Pos;
  end;
end;

{----------------ThtmlLite.GetOurPalette:}
function ThtmlLite.GetOurPalette: HPalette;
begin
if ColorBits = 8 then
  Result := CopyPalette(ThePalette)
else Result := 0;
end;

{----------------ThtmlLite.SetOurPalette}
procedure ThtmlLite.SetOurPalette(Value: HPalette);
var
  NewPalette: HPalette;
begin
if (Value <> 0) and (ColorBits = 8) then
  begin
  NewPalette := CopyPalette(Value);
  if NewPalette <> 0 then
    begin
    if ThePalette <> 0 then
      DeleteObject(ThePalette);
    ThePalette := NewPalette;
    if FDither then SetGlobalPalette(ThePalette);
    end;
  end;
end;

{----------------ThtmlLite.SetDither}
procedure ThtmlLite.SetDither(Value: boolean);
begin
if (Value <> FDither) and (ColorBits = 8) then
  begin
  FDither := Value;
  if Value then SetGlobalPalette(ThePalette)
  else SetGLobalPalette(0);
  end;
end;

procedure ThtmlLite.SetCaretPos(Value: integer);
begin
if Value >= 0 then
  FCaretPos := Value;
end;

function ThtmlLite.FindSourcePos(DisplayPos: integer): integer;
begin
Result := FSectionList.FindSourcePos(DisplayPos);
end;

function ThtmlLite.FindDisplayPos(SourcePos: integer; Prev: boolean): integer;
begin
Result := FSectionList.FindDocPos(SourcePos, Prev);
end;

function ThtmlLite.DisplayPosToXy(DisplayPos: integer; var X, Y: integer): boolean;
begin
Result := FSectionList.CursorToXY(PaintPanel.Canvas, DisplayPos, X, integer(Y));  {integer() req'd for delphi 2}
end;

{----------------ThtmlLite.SetProcessing}
procedure ThtmlLite.SetProcessing(Value: boolean);
begin
if FProcessing <> Value then
  begin
  FProcessing := Value;
  if Assigned(FOnProcessing) and not (csLoading in ComponentState) then
        FOnProcessing(Self, FProcessing);
  end;
end;

{----------------ThtmlLite.SetMarginWidth}
procedure ThtmlLite.SetMarginWidth(Value: integer);
var
  OldPos: integer;
  OldCursor: TCursor;
begin
if (Value <> FMarginWidth) and not FProcessing and (Value >= 0) then
  begin
  OldCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;
    SetProcessing(True);
    FMarginWidth := IntMin(Value, 200);
    FMarginWidthX := FMarginWidth;    
    if FSectionList.Count > 0 then
      begin
      OldPos := Position;
      DoLogic(0, 0, 0, 0);
      Position := OldPos;
      Invalidate;
      end;
  finally
    Screen.Cursor := OldCursor;
    SetProcessing(False);
    end;
  end;
end;

{----------------ThtmlLite.SetMarginHeight}
procedure ThtmlLite.SetMarginHeight(Value: integer);
var
  OldPos: integer;
  OldCursor: TCursor;
begin
if (Value <> FMarginHeight) and not FProcessing and (Value >= 0) then
  begin
  OldCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;
    SetProcessing(True);
    FMarginHeight := IntMin(Value, 200);
    FMarginHeightX := FMarginHeight;   
    if FSectionList.Count > 0 then
      begin
      OldPos := Position;
      DoLogic(0, 0, 0, 0);
      Position := OldPos;
      Invalidate;
      end;
  finally
    Screen.Cursor := OldCursor;
    SetProcessing(False);
    end;
  end;
end;

procedure ThtmlLite.SetServerRoot(Value: string);
begin
Value := Trim(Value);
if (Length(Value) >= 1) and (Value[Length(Value)] = '\') then
  SetLength(Value, Length(Value)-1);
FServerRoot := Value;
end;

procedure ThtmlLite.HandleMeta(Sender: TObject; const HttpEq,
  {$IFDEF HL_LAZARUS}NewName{$ELSE}Name{$ENDIF}, Content: string);
var
  DelTime, I: integer;
begin
if Assigned(FOnMeta) then FOnMeta(Self, HttpEq,
  {$IFDEF HL_LAZARUS}NewName{$ELSE}Name{$ENDIF}, Content);
if Assigned(FOnMetaRefresh) then
  if CompareText(Lowercase(HttpEq), 'refresh') = 0 then
    begin
    I := Pos(';', Content);
    if I > 0 then
      DelTime := StrToIntDef(copy(Content, 1, I-1), -1)
    else DelTime := StrToIntDef(Content, -1);
    if DelTime < 0 then Exit
    else if DelTime = 0 then DelTime := 1;
    I := Pos('url=', Lowercase(Content));
    if I > 0 then
      FRefreshURL := Copy(Content, I+4, Length(Content)-I-3)
    else FRefreshURL := '';
    FRefreshDelay := DelTime;
    end;
end;

procedure ThtmlLite.SetOptions(Value: ThtmlViewerOptions);
begin
if Value <> FOptions then
  begin
  FOptions := Value;
  if Assigned(FSectionList) then
    with FSectionList do
      begin
      LinksActive := htOverLinksActive in FOptions;
      if htNoLinkUnderline in FOptions then
        UnLine := []
      else UnLine := [fsUnderline];
      ShowDummyCaret := htShowDummyCaret in FOptions;
      end;
  end;
end;

procedure ThtmlLite.Repaint;
var
  I: integer;
begin
for I := 0 to FormControlList.count-1 do
  with TFormControlObj(FormControlList.Items[I]) do
    if Assigned(TheControl) then
      TheControl.Hide;
BorderPanel.BorderStyle := bsNone;
inherited Repaint;
end;

function ThtmlLite.GetDragDrop: TDragDropEvent;
begin
Result := FOnDragDrop;
end;

procedure ThtmlLite.SetDragDrop(const Value: TDragDropEvent);
begin
FOnDragDrop := Value;
if Assigned(Value) then
  PaintPanel.OnDragDrop := {$IFDEF HL_LAZARUS}@{$ENDIF}HTMLDragDrop
else PaintPanel.OnDragDrop := Nil;
end;

procedure ThtmlLite.HTMLDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
if Assigned(FOnDragDrop) then
  FOnDragDrop(Self, Source, X, Y);
end;

function ThtmlLite.GetDragOver: TDragOverEvent;
begin
Result := FOnDragOver;
end;

procedure ThtmlLite.SetDragOver(const Value: TDragOverEvent);
begin
FOnDragOver := Value;
if Assigned(Value) then
  PaintPanel.OnDragOver := {$IFDEF HL_LAZARUS}@{$ENDIF}HTMLDragOver
else PaintPanel.OnDragOver := Nil;
end;

procedure ThtmlLite.HTMLDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
if Assigned(FOnDragOver) then
  FOnDragOver(Self, Source, X, Y, State, Accept);
end;

{----------------TPaintPanel.CreateIt}
constructor TPaintPanel.CreateIt(AOwner: TComponent; Viewer: TComponent);

begin
  inherited Create(AOwner);
  FViewer := Viewer;
end;

{----------------TPaintPanel.Paint}
procedure TPaintPanel.Paint;
var
  MemDC: HDC;
  ABitmap: HBitmap;
  ARect: TRect;
  OldPal: HPalette;
begin
if (FViewer as ThtmlLite).DontDraw then Exit;
ThtmlLite(FViewer).DrawBorder;
OldPal := 0;
Canvas.Font := Font;
Canvas.Brush.Color := Color;
ARect := Canvas.ClipRect;
Canvas2 := TCanvas.Create;   {paint on a memory DC}
try
  {$IFDEF HL_LAZARUS}
  Canvas2.Handle := Canvas.Handle;
  {$ELSE}
  MemDC := CreateCompatibleDC(Canvas.Handle);
  ABitmap := 0;
  try
    with ARect do
      begin
      ABitmap := CreateCompatibleBitmap(Canvas.Handle, Right-Left, Bottom-Top);
      if (ABitmap = 0) and (Right-Left + Bottom-Top <> 0) then
             raise EOutOfResources.Create('Out of Resources');
      try
        SelectObject(MemDC, ABitmap);
        SetWindowOrgEx(memDC, Left, Top, Nil);
        Canvas2.Handle := MemDC;
  {$ENDIF}
        DoBackground(Canvas2, False);
        if Assigned(FOnPaint) then FOnPaint(Self);
  {$IFDEF HL_LAZARUS}
  {$ELSE}
        OldPal := SelectPalette(Canvas.Handle, ThePalette, False);
        RealizePalette(Canvas.Handle);
        BitBlt(Canvas.Handle, Left, Top, Right-Left, Bottom-Top,
                              MemDC, Left, Top, SrcCopy);
      finally
        if OldPal <> 0 then SelectPalette(MemDC, OldPal, False);
        Canvas2.Handle := 0;
        end;
      end;
  finally
    DeleteDC(MemDC);
    DeleteObject(ABitmap);
  end;
  {$ENDIF}
finally
  Canvas2.Free;
  end;
end;

procedure TPaintPanel.DoBackground(ACanvas: TCanvas; WmErase: boolean);
var
  Bitmap, Mask: TBitmap;
  H, W, WW, HH, HPos, BW, BH, DCx, DCy: integer;
  Pos: integer;
  OldBrush: HBrush;
  OldPal: HPalette;
  ARect: TRect;
  DC: HDC;
  CopyFromDC: boolean;
begin
DC := ACanvas.handle;
if DC <> 0 then
  begin
  Pos := (FViewer as ThtmlLite).VScrollBarPosition * integer(VScale);
  ARect := Canvas.ClipRect;

  OldPal := SelectPalette(DC, ThePalette, False);
  RealizePalette(DC);
  ACanvas.Brush.Color := Color or $2000000;
  OldBrush := SelectObject(DC, ACanvas.Brush.Handle);
  try
    with FViewer as ThtmlLite do
      begin
      if True then   {note:  this code for later use for Watermarks}
        HPos := HScrollBar.Position
      else
        begin
        HPos := 0;
        Pos := 0;
        end;
      Bitmap := FSectionList.BackgroundBitmap;
      Mask := FSectionList.BackgroundMask;
      if FSectionList.ShowImages and Assigned(Bitmap) then
        try
          DCx := 0;  DCy := 0;
          BW := Bitmap.Width;
          BH := Bitmap.Height;
          HH := (Pos div BH) * BH - Pos;
          WW := (HPos div BW) * BW - HPos;
          while HH < ARect.Top do
            Inc(HH, BH);
          while WW < ARect.Left do
            Inc(WW, BW);
          {to use the fast method, the bitmap must be drawn entirely within the
           viewable area}
          if ((HH+BH <= ARect.Bottom) and ((WW+BW <= ARect.Right)
                  or ((WW = ARect.Left) and (BW >= ARect.Right-ARect.Left)))) then
            begin
            CopyFromDC := True;   {fast}
            DCx := WW;
            DCy := HH;
            end
          else CopyFromDC := False;
          Dec(HH, BH);
          Dec(WW, BW);
          H := HH;
          if Mask <> Nil then
            ACanvas.FillRect(ARect);   {background for transparency}
          if CopyFromDC then
            if Mask = Nil then
              BitBlt(DC, DCx, DCy, BW, BH, Bitmap.Canvas.Handle, 0, 0, SRCCOPY)
            else
              begin
              BitBlt(dc, DCx, DCy, BW, BH, Bitmap.Canvas.Handle, 0, 0, SrcInvert);
              BitBlt(dc, DCx, DCy, BW, BH, Mask.Canvas.Handle, 0, 0, SrcAnd);
              BitBlt(dc, DCx, DCy, BW, BH, Bitmap.Canvas.Handle, 0, 0, SrcPaint);
              end;
          repeat
            W := WW;
            repeat
              if CopyFromDC then
                BitBlt(DC, W, H, BW, BH, DC, DCx, DCy, SRCCOPY)
              else if Mask = Nil then
                BitBlt(DC, W, H, BW, BH, Bitmap.Canvas.Handle, 0, 0, SRCCOPY)
              else
                begin
                BitBlt(dc, W, H, BW, BH, Bitmap.Canvas.Handle, 0, 0, SrcInvert);
                BitBlt(dc, W, H, BW, BH, Mask.Canvas.Handle, 0, 0, SrcAnd);
                BitBlt(dc, W, H, BW, BH, Bitmap.Canvas.Handle, 0, 0, SrcPaint);
                end;
              Inc(W, BW);
            until W >= ARect.Right;
            Inc(H, BH);
          until H >= ARect.Bottom;
        except
          ACanvas.FillRect(ARect);
        end
      else
        begin
        ACanvas.FillRect(ARect);
        end;
      end;
  finally
    SelectObject(DC, OldBrush);
    SelectPalette(DC, OldPal, False);
    RealizePalette(DC);
    end;
  end;
end;

procedure TPaintPanel.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
Message.Result := 1;   {it's erased}
end;

{----------------TPaintPanel.WMLButtonDblClk}
procedure TPaintPanel.WMLButtonDblClk(var Message: TWMMouse);
begin
if Message.Keys and MK_LButton <> 0 then
  ThtmlLite(FViewer).HTMLMouseDblClk(Message);
end;

{----------------T32ScrollBar.SetParams}
procedure T32ScrollBar.SetParams(APosition, APage, AMin, AMax: Integer);
var
  ScrollInfo: TScrollInfo;
begin
if (APosition <> FPosition) or (APage <> FPage) or (AMin <> FMin)
              or (AMax <> FMax) then
  with ScrollInfo do
    begin
    cbSize := SizeOf(ScrollInfo);
    fMask := SIF_ALL;
    if htShowVScroll in (Owner as ThtmlLite).FOptions then
      fMask := fMask or SIF_DISABLENOSCROLL;
    nPos := APosition;
    nPage := APage;
    nMin := AMin;
    nMax := AMax;
    SetScrollInfo(Handle, SB_CTL, ScrollInfo, True);
    FPosition := APosition;
    FPage := APage;
    FMin := AMin;
    FMax := AMax;
    end;
end;

procedure T32ScrollBar.SetPosition(Value: integer);
begin
SetParams(Value, FPage, FMin, FMax);
end;

procedure T32ScrollBar.SetMin(Value: Integer);
begin
  SetParams(FPosition, FPage, Value, FMax);
end;

procedure T32ScrollBar.SetMax(Value: Integer);
begin
  SetParams(FPosition, FPage, FMin, Value);
end;

procedure T32ScrollBar.CNVScroll(var Message: TWMVScroll);
var
  SPos: integer;
  ScrollInfo: TScrollInfo;
  OrigPos: integer;
  TheChange: integer;
begin
Parent.SetFocus;
with ThtmlLite(Parent) do
  begin
  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  ScrollInfo.fMask := SIF_ALL;
  GetScrollInfo(Self.Handle, SB_CTL, ScrollInfo);
  if TScrollCode(Message.ScrollCode) = scTrack then
    begin
    OrigPos := ScrollInfo.nPos;
    SPos := ScrollInfo.nTrackPos;
    end
  else
    begin
    SPos := ScrollInfo.nPos;
    OrigPos := SPos;
    case TScrollCode(Message.ScrollCode) of
      scLineUp:
        Dec(SPos, SmallChange);
      scLineDown:
        Inc(SPos, SmallChange);
      scPageUp:
        Dec(SPos, LargeChange);
      scPageDown:
        Inc(SPos, LargeChange);
      scTop:
        SPos := 0;
      scBottom:
        SPos := (MaxVertical - PaintPanel.Height) div VScale;
      end;
    end;
  SPos := IntMax(0, IntMin(SPos, (MaxVertical - PaintPanel.Height) div VScale));  

  Self.SetPosition(SPos);

  FSectionList.SetYOffset(SPos * VScale);
  TheChange := OrigPos-SPos;

  ScrollWindow(PaintPanel.Handle,0,TheChange,NIL,NIL);
  PaintPanel.Update;
  end;
end;

procedure Register;
begin
RegisterComponents('Samples', [ThtmlLite]);
end;

end.

