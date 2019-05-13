{
 /***************************************************************************
                               InterfaceBase.pp
                               ----------------
                   Initial Revision  : Fri Jul 23 20:00:00 PDT 1999


 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

unit InterfaceBase;

{$mode objfpc}
{$LONGSTRINGS ON}

interface

{$ifdef Trace}
  {$ASSERTIONS ON}
{$endif}

uses
  Types, Classes, SysUtils, Math, FPImage,
  // LazUtils
  LazUTF8, IntegerList,
  // LCL
  LCLType, LCLProc, LMessages, LCLPlatformDef,
  GraphType, GraphMath, IntfGraphics, Themes;

type
  PEventHandler = type Pointer;
  PProcessEventHandler = type Pointer;
  PPipeEventHandler = type Pointer;
  PSocketEventHandler = type Pointer;

  TChildExitReason = (cerExit, cerSignal);
  TPipeReason = (prDataAvailable, prBroken, prCanWrite);
  TPipeReasons = set of TPipeReason;

  TApplicationMainLoop = procedure of object;
  TWaitHandleEvent = procedure(AData: PtrInt; AFlags: dword) of object;
  TChildExitEvent = procedure(AData: PtrInt; AReason: TChildExitReason; AInfo: dword) of object;
  TPipeEvent = procedure(AData: PtrInt; AReasons: TPipeReasons) of object;
  TSocketEvent = procedure(AData: PtrInt; AFlags: dword) of object;

  TLCLWndMethod = procedure(var TheMessage: TLMessage) of Object;

  TLCLCapability = (
    lcAsyncProcess,             // Support for async process
    lcCanDrawOutsideOnPaint,    // Support for drawing outside OnPaint event of an control
    lcNeedMininimizeAppWithMainForm, // When main form is minimized, then minimize also app
    lcApplicationTitle,         // Can change application title in runtime
    lcApplicationWindow,        // Application has a special root window
    lcFormIcon,                 // Forms have icon
    lcModalWindow,              // native modal windows support
    lcDragDockStartOnTitleClick,// ability to start drag/dock events on title bar click
    lcAntialiasingEnabledByDefault, // is amDontCare = amOn for the widgetset
    lcLMHelpSupport,            // support for LM_HELP command
    lcReceivesLMClearCutCopyPasteReliably, // In Carbon we do not receive LM_CLEAR, CUT, COPY, PASTE, etc reliably, and this affects DB controls. See bug http://bugs.freepascal.org/view.php?id=20394
    lcSendsUTF8KeyPress,        // If the interface does not yet send UTF8KeyPress directly, then it will be emulated in TWinControl.CNChar
    lcAllowChildControlsInNativeControls, // Utilized by LCL-CustomDrawn so that it can inject child controls in native ones
    lcEmulatedMDI, // used for emulating MDI on widgetsets which does not provide native MDI handling
    lcAccessibilitySupport,     // Indicates that accessibility is implemented, mostly for TCustomControl descendents as native widgests should have in-built accessibility
    lcRadialGradientBrush,      // Indicates that the function CreateBrushWithRadialGradient is supported, i.e. we can create a brush with a radial gradient pattern
    lcTransparentWindow,        // ability to pass mouse messages through a window (on win32 LM_NCHITTEST with HTTRANSPARENT result)
    lcTextHint,                 // native TextHint support
    lcNativeTaskDialog          // task dialog under mswindows for widgetsets different than win32/wince. Used in LCLTaskDialog. eg Qt/Qt5 must set this option to false otherwise taskdialog segfaults.
  );

  { TDialogButton }

  TDialogButton = class(TCollectionItem)
  private
    FCaption: string;
    FModalResult: LongInt;
    function GetCancel: Boolean;
    function GetDefault: Boolean;
    procedure SetCancel(const AValue: Boolean);
    procedure SetDefault(const AValue: Boolean);
  protected
    function GetDisplayName: string; override;
    procedure SetCaption(const AValue: string); virtual;
  public
    constructor Create(ACollection: TCollection); override;
    property Caption: string read FCaption write SetCaption;
    property Cancel: Boolean read GetCancel write SetCancel;
    property Default: Boolean read GetDefault write SetDefault;
    property ModalResult: LongInt read FModalResult write FModalResult;
  end;

  { TDialogButtons }

  TDialogButtons = class(TCollection)
  protected
    FCancelButton: TDialogButton;
    FDefaultButton: TDialogButton;
    function GetItem(Index: Integer): TDialogButton;
    procedure SetCancelButton(const AValue: TDialogButton); virtual;
    procedure SetDefaultButton(const Value: TDialogButton); virtual;
    procedure SetItem(Index: Integer; const Value: TDialogButton);
  public
    destructor Destroy; override;
    function Add: TDialogButton;
    function FindButton(AModalResult: LongInt): TDialogButton;
    function FindButton(Order: array of LongInt): TDialogButton;
    property DefaultButton: TDialogButton read FDefaultButton write SetDefaultButton;
    property CancelButton: TDialogButton read FCancelButton write SetCancelButton;
    property Items[Index: Integer]: TDialogButton read GetItem write SetItem; default;
  end;

type
  TWSTimerProc = procedure of object;

  { TWidgetSet }

  TWidgetSet = class(TObject)
  protected
    FThemeServices: TThemeServices;
    procedure PassCmdLineOptions; virtual;
    function CreateThemeServices: TThemeServices; virtual;
    function GetAppHandle: THandle; virtual;
    procedure SetAppHandle(const AValue: THandle); virtual;
  public
    constructor Create; virtual;
    procedure BeforeDestruction;override;

    procedure AppInit(var ScreenInfo: TScreenInfo); virtual; abstract;
    procedure AppRun(const ALoop: TApplicationMainLoop); virtual;
    procedure AppWaitMessage; virtual; abstract;
    procedure AppProcessMessages; virtual; abstract;
    procedure AppTerminate; virtual; abstract;
    procedure AppMinimize; virtual; abstract;
    procedure AppRestore; virtual; abstract;
    procedure AppBringToFront; virtual; abstract;
    procedure AppSetIcon(const Small, Big: HICON); virtual;
    procedure AppSetTitle(const ATitle: string); virtual;
    procedure AppSetVisible(const AVisible: Boolean); virtual;
    function AppRemoveStayOnTopFlags(const ASystemTopAlso: Boolean = False): Boolean; virtual;
    function AppRestoreStayOnTopFlags(const ASystemTopAlso: Boolean = False): Boolean; virtual;
    procedure AppSetMainFormOnTaskBar(const DoSet: Boolean); virtual;
    
    function  LCLPlatform: TLCLPlatform; virtual; abstract;
    function  GetLCLCapability(ACapability: TLCLCapability): PtrUInt; virtual;

    function  DCGetPixel(CanvasHandle: HDC; X, Y: integer): TGraphicsColor; virtual; abstract;
    procedure DCSetPixel(CanvasHandle: HDC; X, Y: integer; AColor: TGraphicsColor); virtual; abstract;
    procedure DCRedraw(CanvasHandle: HDC); virtual; abstract;
    procedure DCSetAntialiasing(CanvasHandle: HDC; AEnabled: Boolean); virtual;
    procedure SetDesigning(AComponent: TComponent); virtual;

    function  InitStockFont(AFont: TObject; AStockFont: TStockFont): Boolean; virtual;
    function  IsHelpKey(Key: Word; Shift: TShiftState): Boolean; virtual;

    // create and destroy
    function CreateTimer(Interval: integer; TimerProc: TWSTimerProc): THandle; virtual; abstract;
    function DestroyTimer(TimerHandle: THandle): boolean; virtual; abstract;
    property AppHandle: THandle read GetAppHandle write SetAppHandle; platform;

    {$DEFINE IF_BASE_MEMBER}
    {$I winapih.inc}
    {$I lclintfh.inc}
    {$UNDEF IF_BASE_MEMBER}
    
    property ThemeServices: TThemeServices read FThemeServices;
  end;
  TWidgetSetClass = class of TWidgetSet;

  function GetDefaultLCLWidgetType: TLCLPlatform;
  function GetLCLWidgetTypeName: string;

const
  { Constants for the routine TWidgetSet.GetLCLCapability }
  LCL_CAPABILITY_NO = 0;
  LCL_CAPABILITY_YES = 1;

type
  EInterfaceException = class(Exception);
  EInterfaceError = class(EInterfaceException);
  EInterfaceCritical = class(EInterfaceException);
  EInterfaceWarning = class(EInterfaceException);

type
  TInputDialogFunction = function (const InputCaption, InputPrompt : String;
                             MaskInput : Boolean; var Value : String) : Boolean;
  TPromptDialogFunction = function(const DialogCaption, DialogMessage : String;
    DialogType : longint; Buttons : PLongint;
    ButtonCount, DefaultIndex, EscapeResult : Longint;
    UseDefaultPos: boolean;
    X, Y : Longint) : Longint;
  TQuestionDialogFunction = function(const aCaption, aMsg: string;
    DlgType: LongInt; Buttons: TDialogButtons; HelpCtx: Longint): LongInt;
  TLoadBitmapFunction = function(hInstance: THandle; lpBitmapName: PChar): HBitmap;
  TLoadCursorFunction = function(hInstance: THandle; lpCursorName: PChar): HCursor;
  TLoadIconFunction= function(hInstance: THandle; lpIconName: PChar): HIcon;

var
  InputDialogFunction: TInputDialogFunction = nil;
  PromptDialogFunction: TPromptDialogFunction = nil;
  QuestionDialogFunction: TQuestionDialogFunction = nil;
  LoadBitmapFunction: TLoadBitmapFunction = nil;
  LoadCursorFunction: TLoadCursorFunction = nil;
  LoadIconFunction: TLoadIconFunction = nil;

var
  WidgetSet: TWidgetSet = nil;

implementation

{function GetDefaultLCLWidgetType: TLCLPlatform;
begin
  Assert(Assigned(WidgetSet), 'GetDefaultLCLWidgetType: WidgetSet is not assigned.');
  if WidgetSet.LCLPlatform<>lpNoGUI then
    Result:=WidgetSet.LCLPlatform
end; }
function GetDefaultLCLWidgetType: TLCLPlatform;
begin
  if (WidgetSet<>nil) and (WidgetSet.LCLPlatform<>lpNoGUI) then
    Result:=WidgetSet.LCLPlatform
  else
    Result:=BuildLCLWidgetType;
end;

function GetLCLWidgetTypeName: string;
begin
  Result:=LCLPlatformDirNames[GetDefaultLCLWidgetType];
end;

{ TDialogButtons }

procedure TDialogButtons.SetCancelButton(const AValue: TDialogButton);
begin
  FCancelButton := AValue;
end;

function TDialogButtons.GetItem(Index: Integer): TDialogButton;
begin
  Result := TDialogButton(inherited GetItem(Index));
end;

procedure TDialogButtons.SetDefaultButton(const Value: TDialogButton);
begin
  FDefaultButton := Value;
end;

procedure TDialogButtons.SetItem(Index: Integer; const Value: TDialogButton);
begin
  inherited SetItem(Index, Value);
end;

destructor TDialogButtons.Destroy;
begin
  inherited Destroy;
end;

function TDialogButtons.Add: TDialogButton;
begin
  Result := TDialogButton(inherited Add);
end;

function TDialogButtons.FindButton(AModalResult: LongInt): TDialogButton;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].ModalResult = AModalResult then
      Exit(Items[i]);
  Result := nil;
end;

function TDialogButtons.FindButton(Order: array of LongInt): TDialogButton;
var
  i: Integer;
begin
  for i := Low(Order) to High(Order) do
  begin
    Result := FindButton(Order[i]);
    if Result <> nil then
      Exit;
  end;
  Result := nil;
end;

{ TDialogButton }

procedure TDialogButton.SetCaption(const AValue: string);
begin
  FCaption := AValue;
end;

function TDialogButton.GetDefault: Boolean;
begin
  Result := TDialogButtons(Collection).DefaultButton = Self;
end;

function TDialogButton.GetCancel: Boolean;
begin
  Result := TDialogButtons(Collection).CancelButton = Self;
end;

procedure TDialogButton.SetCancel(const AValue: Boolean);
begin
  if AValue then
    TDialogButtons(Collection).CancelButton := Self
  else
    TDialogButtons(Collection).CancelButton := nil;
end;

procedure TDialogButton.SetDefault(const AValue: Boolean);
begin
  if AValue then
    TDialogButtons(Collection).DefaultButton := Self
  else
    TDialogButtons(Collection).DefaultButton := nil;
end;

function TDialogButton.GetDisplayName: string;
begin
  Result := FCaption;
end;

constructor TDialogButton.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FCaption := '';
  FModalResult := 0;
end;

{$I interfacebase.inc}
{$I intfbasewinapi.inc}
{$I intfbaselcl.inc}

end.
