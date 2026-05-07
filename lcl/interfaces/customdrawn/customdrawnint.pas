{
 /***************************************************************************
                CustomDrawnInt.pas -  CustomDrawn Interface Object
                             -------------------

 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

unit CustomDrawnInt;

{$mode objfpc}{$H+}

{$I customdrawndefines.inc}

interface

uses
  // RTL
  // for CD_Cocoa Types needs to be after the platform-specif units or else Mac
  // will catch MacOSAll.Rect/Size/Point
  {$ifndef CD_Cocoa}Types,{$endif}Classes, SysUtils, Math,
  fpimage, fpcanvas, fpimgcanv, ctypes, dateutils,
  // XML
  XMLRead, Dom,
  // Platform specific
  {$ifdef CD_Windows}Windows, customdrawn_WinProc, customdrawn_winextra,{$endif}
  {$ifdef CD_Cocoa}MacOSAll, CocoaAll, customdrawn_cocoaproc, CocoaGDIObjects,Types,{$endif}
  {$ifdef CD_X11}X, XLib, XUtil, BaseUnix, customdrawn_x11proc,{$ifdef CD_UseNativeText}xft, fontconfig,{$endif}{$endif}
  {$ifdef CD_Android}
  customdrawn_androidproc, jni, bitmap, log, keycodes,
  {$endif}
  {$ifdef CD_Wayland}BaseUnix, Process, waylandwire, waylandcore, xdgshell, xdgactivation, cursorshape, wldatadevice, textinputv3, customdrawn_waylandproc,{$endif}
  {$ifdef WinCE}aygshell,{$endif}
  // LazUtils
  LazUtilities, LazFileUtils, lazutf8,
  {$ifndef CD_UseNativeText}
  // LazFreeType
  LazFreeTypeIntfDrawer, LazFreeType, EasyLazFreeType, IniFiles,
  {$endif}
  // Widgetset
  customdrawnproc, customdrawnthemes,
  // LCL
  customdrawn_common, customdrawncontrols, customdrawndrawers,
  lazcanvas, lazregions, lazdeviceapis,
  LCLPlatformDef, InterfaceBase, Themes, Dialogs, Buttons,
  Controls, StdCtrls, ComCtrls, Forms, lclproc, IntfGraphics, GraphType,
  LCLType, LMessages, Graphics, LCLStrConsts, Menus, LazLoggerBase;

type
  {$ifdef CD_Windows}
  TWinCETitlePolicy = (tpAlwaysUseOKButton, tpOKButtonOnlyOnDialogs, tpControlWithBorderIcons);

  PPPipeEventInfo = ^PPipeEventInfo;
  PPipeEventInfo = ^TPipeEventInfo;
  TPipeEventInfo = record
    Handle: THandle;
    UserData: PtrInt;
    OnEvent: TPipeEvent;
    Prev: PPipeEventInfo;
    Next: PPipeEventInfo;
  end;

  TWaitHandler = record
    ListIndex: pdword;
    UserData: PtrInt;
    OnEvent: TWaitHandleEvent;
  end;

  TSocketEvent = function(ASocket: THandle; Flags: dword): Integer of object;
  {$endif}
  {$ifdef CD_Cocoa}

  TCDTimerObject=objcclass(NSObject)
    func : TWSTimerProc;
    procedure timerEvent; message 'timerEvent';
    class function initWithFunc(afunc: TWSTimerProc): TCDTimerObject; message 'initWithFunc:';
  end;

  TCDAppDelegate = objcclass(NSObject, NSApplicationDelegateProtocol)
    function applicationShouldTerminate(sender: NSApplication): NSApplicationTerminateReply; message 'applicationShouldTerminate:';
  end;
  {$endif}
  {$ifdef CD_X11}
  // Just in case...
  {$endif}

  // Return true to disable the form background drawing
  TDisableFormBackgroundDrawingProc = function (AForm: TCustomForm): Boolean of object;

  { TLazCDCustomFont }

  TLazCDCustomFont = class(TFPCustomFont)
  public
    {$ifndef CD_UseNativeText}
    FTFont: TFreeTypeFont;
    {$endif}
    constructor Create; override;
    destructor Destroy; override;
  end;

  { TCDWidgetSet }

  TCDWidgetSet = class(TWidgetSet)
  private
    FTerminating: Boolean;

    // Clipboard support
    FClipBoardFormats: TStringList;

    {$ifdef CD_WINDOWS}
    // In win32 it is: The parent of all windows, represents the button of the taskbar
    // In wince it is just an invisible window, but retains the following functions:
    // * This window is also the owner of the clipboard.
    // * Assoc. windowproc also acts as handler for popup menus
    // * It is indispensable for popupmenus and thread synchronization
    FAppHandle: THandle;

    FMetrics: TNonClientMetrics;
    FMetricsFailed: Boolean;

    FStatusFont: HFONT;
    FMessageFont: HFONT;

    FWaitHandleCount: dword;
    FWaitHandles: array of HANDLE;
    FWaitHandlers: array of TWaitHandler;
    FWaitPipeHandlers: PPipeEventInfo;

    FOnAsyncSocketMsg: TSocketEvent;

    function WinRegister: Boolean;
    procedure CreateAppHandle;
    {$endif}
  public
    {$ifdef CD_X11}
    FDisplayName: string;
    FDisplay: PDisplay;
    FScreen: longint;
    FVisual: TVisual; // Visual from X11

    LeaderWindow: X.TWindow;
    ClientLeaderAtom: TAtom;

    FWMProtocols: TAtom;	  // Atom for "WM_PROTOCOLS"
    FWMDeleteWindow: TAtom;	  // Atom for "WM_DELETE_WINDOW"
    FWMHints: TAtom;		  // Atom for "_MOTIF_WM_HINTS"
    FWMPaint: TAtom;		  // Atom for "WM_PAINT"

    // For composing character events
    ComposeBuffer: string;
    ComposeStatus: TStatus;
    InputMethod: xlib.PXIM;
    InputContext: PXIC;
    LastKeySym: TKeySym; // Used for KeyRelease event
    LastKey: Word;       // Used for KeyRelease event

    ShiftState: TShiftState; // Keeps ShiftState from X

    // XConnections list
    XConnections: TFPList;
    // Windows Info List
    XWindowList: TStringList;
    // Timer queue head
    {$ifdef CD_X11_UseNewTimer}
    XTimerListHead: customdrawn_x11proc.TCDX11Timer;
    {$endif}

    // Functions to keep track of windows needing repaint
    function CheckInvalidateWindowForX(XWIndowID: X.TWindow): Boolean;
    procedure WindowUpdated(XWIndowID: X.TWindow);

    function FindWindowByXID(XWindowID: X.TWindow; out AWindowInfo: TX11WindowInfo): TWinControl;
    procedure AppProcessMessage;
    procedure AppProcessInvalidates;
    function XStateToLCLState(XKeyState: cuint): TShiftState;
    {$endif}
    {$ifdef CD_Android}
    CombiningAccent: Cardinal;
    procedure AndroidDebugLn(ASender: TObject; AStr: string; var AHandled: Boolean;
      Target: TLazLoggerWriteTarget; Data: Pointer);
    function AndroidKeyCodeToLCLKeyCode(AAndroidKeyCode: Integer): Word;
    function DoOpenURLWidgetsetImplementation(AURL: string): Boolean;
    function DoOpenDocumentWidgetsetImplementation(APath: string): Boolean;
    function DoOpenAndroidURI(AURI: JObject; AMimeType: string): Boolean;
    function GetMimeTypeFromFileName(AFileName: string): string;
    procedure ShowListViewDialog(ATitle: string; ATitles,
      ADescriptions: array of string;
      AColorOddRow: jint = $ff292C29; AColorEvenRow: jint = $ff424542);
    {$endif}
    {$ifdef CD_Cocoa}
    pool      : NSAutoreleasePool;
    NSApp     : NSApplication;
    delegate  : TCDAppDelegate;
    ScreenBitmapContext: CGContextRef;
    {$endif}
    {$ifdef CD_Wayland}
    FWlDisplay:        TWaylandDisplay;
    FWlRegistry:       TWaylandRegistry;
    FWlCompositor:     TWaylandCompositor;
    FWlShm:            TWaylandShm;
    FXdgWmBase:        TXdgWmBase;
    FXdgActivation:    TXdgActivationV1;
    FWlSeat:           TWaylandSeat;
    FWlPointer:        TWaylandPointer;
    FWlKeyboard:       TWaylandKeyboard;
    FWpCursorShapeMgr: TWpCursorShapeManagerV1;
    FWpCursorShapeDev: TWpCursorShapeDeviceV1;
    FWlPointerFocus:   TWaylandWindowInfo;  { window the pointer is currently over }
    FWlKeyboardFocus:  TWaylandWindowInfo;  { window with keyboard focus }
    FWlPointerX:       LongInt;
    FWlPointerY:       LongInt;
    FWlShiftHeld:      Boolean;
    FWlCtrlHeld:       Boolean;
    FWlAltHeld:        Boolean;
    FWlCapsLocked:     Boolean;       { from wl_keyboard.modifiers ModsLocked }
    FWlNumLocked:      Boolean;
    FWlWindowList:     TFPList;          { TWaylandWindowInfo }
    FWlTimerList:      TFPList;          { TWLTimer }
    FWlOutputs:        TFPList;          { TWaylandOutput, in advertise order }
    { Clipboard plumbing. ddmgr is the bound global; device is per-seat
      and bound when the seat advertises pointer/keyboard caps. Source
      / SourceFormats / SourceCallback hold the most recent
      ClipboardGetOwnerShip arguments so the OnSend handler can
      synthesize bytes for whichever mime type the receiver picked. }
    FWlDataDeviceManager: TWlDataDeviceManager;
    FWlDataDevice:        TWlDataDevice;
    FWlClipSource:        TWlDataSource;
    FWlClipFormats:       array of TClipboardFormat;
    FWlClipCallback:      TClipboardRequestEvent;
    FWlLastInputSerial:   LongWord;       { for set_selection }
    FWlLastPressSerial:   LongWord;       { for xdg_popup.grab; only press serials are valid grab triggers }
    { Currently-mapped THintWindow popup, or nil. Tooltips are kept at
      the leaf of the xdg_popup chain by construction:
       1) before any non-tooltip popup is created, the active tooltip
          is dismissed (WLCreatePopupForWindow tears it down up front);
       2) when a popup that's the parent of the active tooltip is torn
          down, the tooltip is dismissed first (WLTeardownPopup does
          so before the parent's wl objects go away).
      Without (1), a tooltip up before a menu opens becomes a sibling
      of that menu; without (2), the menu's destroy-popup happens with
      a live child popup -- both are protocol errors that disconnect
      us. Tooltips are also created without xdg_popup.grab so there's
      no compositor-driven dismissal racing with our LCL HintWindow
      auto-hide timer. }
    FWlActiveTooltip:     TWaylandWindowInfo;
    { zwp_text_input_v3 plumbing. ibus / fcitx5 IME path. The mgr is
      bound at registry time; the per-seat object is created after
      seat caps. FTextInputFocus is the form-level surface the
      compositor most recently delivered text-input.enter for; it
      stays set even between input events because text-input focus
      tracks keyboard focus and the LCL may not re-send Show on
      every redraw. The pending* fields buffer the state the spec
      tells us to apply atomically on the `done` event. }
    FZwpTextInputMgr:      TZwpTextInputManagerV3;
    FZwpTextInput:         TZwpTextInputV3;
    FTextInputFocus:       TWaylandWindowInfo;
    FTextInputEnabled:     Boolean;       { our local mirror of the enabled flag }
    FImeDesiredEdit:       TWinControl;   { LCL control that asked for IME; nil = none.
                                            Tracks intent independently from
                                            FTextInputFocus (set on text_input.enter)
                                            so we never send enable on an unfocused
                                            surface, which is unspecified per spec. }
    FTIPendingPreedit:     Boolean;       { preedit_string event buffered? }
    FTIPendingPreeditText: AnsiString;
    FTIPendingPreeditB:    LongInt;       { cursor_begin (byte offset) }
    FTIPendingPreeditE:    LongInt;       { cursor_end (byte offset) }
    FTIPendingCommit:      Boolean;       { commit_string event buffered? }
    FTIPendingCommitText:  AnsiString;
    FTIPendingDelete:      Boolean;       { delete_surrounding_text event buffered? }
    FTIPendingDeleteB:     LongWord;      { before_length (bytes) }
    FTIPendingDeleteA:     LongWord;      { after_length (bytes) }
    { Read-only IME state for demo / debug. Last applied commit_string
      and the live preedit; refreshed on every `done`. }
    FWlLastImeCommit:      AnsiString;
    FWlLastImePreedit:     AnsiString;
    procedure WLPaintAllPending;
    procedure WLDrawWindow(WI: TWaylandWindowInfo);
    function  WLFindWindowBySurface(Surface: TWaylandSurface): TWaylandWindowInfo;
    function  WLFindFormWindowInfo(LCLCtrl: TWinControl): TWaylandWindowInfo;
    function  WLPrimaryOutput: TWaylandOutput;
    { Wayland-protocol event handlers. Bound as method pointers on the
      registry / seat / pointer / keyboard / clipboard objects so they
      reach widget-set state via Self instead of a global. }
    procedure WLHandleGlobal(Name: LongWord; const Iface: AnsiString;
      Version: LongWord);
    procedure WLHandlePing(Serial: LongWord);
    procedure WLHandleSeatCaps(Sender: TWaylandSeat; Caps: LongWord);
    procedure WLHandlePointerEnter(Sender: TWaylandPointer; Serial: LongWord;
      Surface: TWaylandSurface; X, Y: LongInt);
    procedure WLHandlePointerLeave(Sender: TWaylandPointer; Serial: LongWord;
      Surface: TWaylandSurface);
    procedure WLHandlePointerMotion(Sender: TWaylandPointer; TimeMs: LongWord;
      X, Y: LongInt);
    procedure WLHandlePointerButton(Sender: TWaylandPointer;
      Serial, TimeMs, Button, State: LongWord);
    procedure WLHandlePointerAxis(Sender: TWaylandPointer;
      TimeMs, Axis: LongWord; Value: TWlFixed);
    procedure WLHandleKeymap(Sender: TWaylandKeyboard; Format: LongWord;
      Fd: cint; Size: LongWord);
    procedure WLHandleKeyboardEnter(Sender: TWaylandKeyboard; Serial: LongWord;
      Surface: TWaylandSurface);
    procedure WLHandleKeyboardLeave(Sender: TWaylandKeyboard; Serial: LongWord;
      Surface: TWaylandSurface);
    procedure WLHandleKey(Sender: TWaylandKeyboard;
      Serial, TimeMs, Key, State: LongWord);
    procedure WLHandleKeyMods(Sender: TWaylandKeyboard;
      Serial, ModsDepressed, ModsLatched, ModsLocked, Group: LongWord);
    procedure WLClipSourceSend(Source: TWlDataSource;
      const MimeType: AnsiString; Fd: cint);
    procedure WLClipSourceCancelled(Source: TWlDataSource);
    procedure WLHandleTextInputEnter(Sender: TZwpTextInputV3;
      Surface: TWaylandSurface);
    procedure WLHandleTextInputLeave(Sender: TZwpTextInputV3;
      Surface: TWaylandSurface);
    procedure WLHandleTextInputPreedit(Sender: TZwpTextInputV3;
      const Text: AnsiString; CursorBegin, CursorEnd: LongInt);
    procedure WLHandleTextInputCommit(Sender: TZwpTextInputV3;
      const Text: AnsiString);
    procedure WLHandleTextInputDelete(Sender: TZwpTextInputV3;
      BeforeLength, AfterLength: LongWord);
    procedure WLHandleTextInputDone(Sender: TZwpTextInputV3; Serial: LongWord);
    { LCL-side helpers used by Show/HideVirtualKeyboard and TCDIntfEdit
      focus / change hooks. LCLEdit is the LCL TCustomEdit; the
      injected TCDEdit lives at TCDWinControl(LCLEdit.Handle).CDControl. }
    procedure WLEnableTextInputForEdit(LCLEdit: TWinControl);
    procedure WLDisableTextInput;
    procedure WLPushTextInputEnable;
    procedure WLPushTextInputContext(LCLEdit: TWinControl);
    procedure WLHandleSurfaceConfigure(Sender: TXdgSurface; Serial: LongWord);
    procedure WLHandleToplevelConfigure(Sender: TXdgToplevel;
      W, H: LongInt; const States: array of LongWord);
    procedure WLHandleToplevelClose(Sender: TXdgToplevel);
    procedure WLHandlePopupConfigure(Sender: TXdgPopup;
      X, Y, W, H: LongInt);
    procedure WLHandlePopupDone(Sender: TXdgPopup);
    function  WLChoosePopupParent(LCLForm: TCustomForm): TWaylandWindowInfo;
    function  WLComputePopupAnchorRect(LCLForm: TCustomForm;
      ParentWI: TWaylandWindowInfo; out R: TRect): Boolean;
    procedure WLCreatePopupForWindow(WI, ParentWI: TWaylandWindowInfo;
      const Anchor: TRect);
    function  WLCurrentShiftState: TShiftState;
    {$endif}
  // For generic methods added in customdrawn
  // They are used internally in LCL-CustomDrawn, LCL app should not use them
  public
    {$ifndef CD_UseNativeText}
    // Font Path List
    FFontPaths: TstringList;
    FFontList: THashedStringList;
    // default fonts availability
    LiberationFont: Boolean;
    LuxiFont: Boolean;
    {$endif}
    // Stock objects
    FStockBlackBrush: TFPCustomBrush;
    FStockDKGrayBrush: TFPCustomBrush;
    FStockGrayBrush: TFPCustomBrush;
    FStockLtGrayBrush: TFPCustomBrush;
    FStockNullBrush: TFPCustomBrush;
    FStockWhiteBrush: TFPCustomBrush;

    FStockBlackPen: TFPCustomPen;
    FStockNullPen: TFPCustomPen;
    FStockWhitePen: TFPCustomPen;

    DefaultFontSize: Integer;
    FDefaultGUIFont: TFPCustomFont;
    //
    AccumulatedStr: string;
    // The currently focused control
    FocusedControl: TWinControl;
    FocusedIntfControl: TWinControl;
    // Default Fonts
    DefaultFont: TFPCustomFont;
    DefaultFontAndroidSize: Integer;
    // Mobile emulator and mobile mode
    MobileMainForm: TLCLHandle;
    // For unusual implementations of DebugLn/DebugOut
    procedure AccumulatingDebugOut(ASender: TObject; AStr: string; var AHandled: Boolean;
      Target: TLazLoggerWriteTarget; Data: Pointer);
    //
    procedure CDSetFocusToControl(ALCLControl, AIntfControl: TWinControl);
  //
  protected
    function CreateThemeServices: TThemeServices; override;
    function GetAppHandle: TLCLHandle; override; //BackendSpecific
    //procedure SetAppHandle(const AValue: TLCLHandle); override;
    //
    procedure BackendCreate;
    procedure BackendDestroy;
    //
    procedure GenericAppInit;
  public
    // ScreenDC and Image for doing Canvas operations outside the Paint event
    // and also for text drawing operations
    ScreenDC: TLazCanvas;
    ScreenBitmapRawImage: TRawImage;
    ScreenBitmapHeight: Integer;
    ScreenBitmapWidth: Integer;
    ScreenImage: TLazIntfImage;
    ScreenFormat: TLazCanvasImageFormat;

    // Android Activity callbacks
    ActivityOnCreate: TProcedure;
    ActivityClassName: string;

    constructor Create; override;
    destructor Destroy; override;

    function LCLPlatform: TLCLPlatform; override;
    function GetLCLCapability(ACapability: TLCLCapability): PtrUInt; override;

    { Initialize the API }
    procedure AppInit(var ScreenInfo: TScreenInfo); override;
    procedure AppRun(const ALoop: TApplicationMainLoop); override;
    procedure AppWaitMessage; override;
    procedure AppProcessMessages; override;
    procedure AppTerminate; override;
    procedure AppMinimize; override;
    procedure AppRestore; override;
    procedure AppBringToFront; override;
    procedure AppSetIcon(const Small, Big: HICON); override;
    procedure AppSetTitle(const ATitle: string); override;
    procedure AppSetVisible(const AVisible: Boolean); override;
    function AppRemoveStayOnTopFlags(const ASystemTopAlso: Boolean = False): Boolean; override;
    function AppRestoreStayOnTopFlags(const ASystemTopAlso: Boolean = False): Boolean; override;
    procedure AppSetMainFormOnTaskBar(const DoSet: Boolean); override;

    //function  InitStockFont(AFont: TObject; AStockFont: TStockFont): Boolean; override;

    procedure DCSetPixel(CanvasHandle: HDC; X, Y: integer; AColor: TGraphicsColor); override;
    function  DCGetPixel(CanvasHandle: HDC; X, Y: integer): TGraphicsColor; override;
    procedure DCRedraw(CanvasHandle: HDC); override;
    procedure DCSetAntialiasing(CanvasHandle: HDC; AEnabled: Boolean); override;
    procedure SetDesigning(AComponent: TComponent); override;

    // create and destroy
    function CreateTimer(Interval: integer; TimerFunc: TWSTimerProc): TLCLHandle; override;
    function DestroyTimer(TimerHandle: TLCLHandle): boolean; override;

    {$I customdrawnwinapih.inc}
    {$I customdrawnlclintfh.inc}
  public
    { Variables to be set by the user }
    {$ifdef WinCE}
    WinCETitlePolicy: TWinCETitlePolicy;
    {$endif}

    // This callback might be set to provide a routine which will select for
    // which forms the drawing of the background should be disabled
    // This is provided for speeding up the drawing
    //
    // Only use it if you are 100% sure that you are filling the entire buffer
    // in the form paint event. Note that the form might sometimes be smaller
    // then the buffer in Android so fill the entire Canvas area, not only the form size
    DisableFormBackgroundDrawingProc: TDisableFormBackgroundDrawingProc;
  end;

var
  CDWidgetSet: TCDWidgetSet absolute WidgetSet;

function CDMessageBoxFunction(Text, Caption : PChar; Flags : Longint) : Integer;

{$ifdef CD_WINDOWS}
function WindowProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
  LParam: Windows.LParam): LResult; {$ifdef WinCE}cdecl;{$else}stdcall;{$endif}
{$endif}
{$ifdef CD_X11}
procedure MyXConnectionWatchProc(display: PDisplay; client_data: TXPointer;
  fd: cint; opening: XLib.TBool; watch_data: PXPointer); cdecl;
{$endif}
{$ifdef CD_Android}
function Java_com_pascal_lclproject_LCLActivity_LCLOnTouch(env:PJNIEnv;this:jobject; x, y: single; action: jint): jint; cdecl;
function Java_com_pascal_lclproject_LCLActivity_LCLDrawToBitmap(
    env:PJNIEnv;this:jobject; width, height: jint; abitmap: jobject): jint; cdecl;
function Java_com_pascal_lclproject_LCLActivity_LCLOnCreate(
    env:PJNIEnv; this:jobject; alclactivity: jobject): jint; cdecl;
function Java_com_pascal_lclproject_LCLActivity_LCLOnMessageBoxFinished(
    env:PJNIEnv; this:jobject; AResult, ADialogType: jint): jint; cdecl;
function Java_com_pascal_lclproject_LCLActivity_LCLOnKey(
    env:PJNIEnv; this:jobject; AKind: jint; AKeyCode: jint;
    AEvent: jobject; AChar: jint): jint; cdecl;
function Java_com_pascal_lclproject_LCLActivity_LCLOnTimer(
    env:PJNIEnv; this:jobject; ATimer: jobject; ATimerIDIndex: jint): jint; cdecl;
function Java_com_pascal_lclproject_LCLActivity_LCLOnConfigurationChanged(
    env:PJNIEnv; this:jobject; ANewDPI, ANewWidth: jint): jint; cdecl;
function Java_com_pascal_lclproject_LCLActivity_LCLOnSensorChanged(
    env:PJNIEnv; this:jobject; ASensorKind: jint; AValues: JDoubleArray): jint; cdecl;
function Java_com_pascal_lclproject_LCLActivity_LCLOnMenuAction(
  env:PJNIEnv; this:jobject; kind, itemIndex: jint): jint; cdecl;
function JNI_OnLoad(vm:PJavaVM;reserved:pointer):jint; cdecl;
procedure JNI_OnUnload(vm:PJavaVM;reserved:pointer); cdecl;

var
  javaVMRef: PJavaVM=nil;
  javaEnvRef: PJNIEnv=nil;
  javaActivityClass: JClass = nil;
  javaActivityObject: jobject = nil;

  // The SDK Version
  android_os_Build_VERSION_SDK_INT: jint;

  // Other classes and objects
  javaAndroidAppActivityClass: JClass = nil;
  javaJavaLangSystemClass: JClass = nil;
  javaAndroidOSBuildClass: JClass = nil;
  javaAndroidOSVibratorClass: JClass = nil;
  javaAndroidContentContextClass: JClass = nil;
  javaJavaLangStringClass: JClass = nil;
  javaAndroidOSBuildVERSIONClass: JClass = nil;

  // Other fields
  javaField_VERSION_SDK_INT: JFieldID = nil;
  // Fields of our Activity
  // Strings
  javaField_lcltext: JfieldID=nil;
  javaField_lcltitle: JfieldID=nil;
  javaField_lclbutton1str: JfieldID=nil;
  javaField_lclbutton2str: JfieldID=nil;
  javaField_lclbutton3str: JfieldID=nil;
  // Integers
  javaField_lclwidth: JfieldID=nil;
  javaField_lclheight: JfieldID=nil;
  javaField_lclbutton1: JfieldID=nil;
  javaField_lclbutton2: JfieldID=nil;
  javaField_lclbutton3: JfieldID=nil;
  javaField_lclbitmap: JfieldID=nil;
  javaField_lcltextsize: JfieldID=nil;
  // Text metrics
  javaField_lcltextascent: JfieldID=nil;
  javaField_lcltextbottom: JfieldID=nil;
  javaField_lcltextdescent: JfieldID=nil;
  javaField_lcltextleading: JfieldID=nil;
  javaField_lcltexttop: JfieldID=nil;
  javaField_lclmaxwidth: JfieldID=nil;
  javaField_lclmaxcount: JfieldID=nil;
  javaField_lclpartialwidths: JfieldID=nil;
  // Timer
  javaField_lcltimerinterval: JfieldID=nil;
  javaField_lcltimerid: JfieldID=nil;
  // Screen Metrics
  javaField_lclxdpi: JfieldID=nil;
  javaField_lclydpi: JfieldID=nil;
  javaField_lclformwidth: JfieldID=nil;
  javaField_lclformheight: JfieldID=nil;
  javaField_lclscreenwidth: JfieldID=nil;
  javaField_lclscreenheight: JfieldID=nil;
  // For LazDeviceAPIs
  javaField_lcldestination: JfieldID=nil;
  javaField_lclkind: JfieldID=nil;

  // Methods of our Activity
  javaMethod_LCLDoGetTextBounds: jmethodid = nil;
  javaMethod_LCLDoGetTextPartialWidths: jmethodid = nil;
  javaMethod_LCLDoDrawText: jmethodid = nil;
  javaMethod_LCLDoShowMessageBox: jmethodid = nil;
  javaMethod_LCLDoCreateTimer: jmethodid = nil;
  javaMethod_LCLDoDestroyTimer: jmethodid = nil;
  javaMethod_LCLDoHideVirtualKeyboard: jmethodid = nil;
  javaMethod_LCLDoShowVirtualKeyboard: jmethodid = nil;
  javaMethod_LCLDoStartReadingAccelerometer: jmethodid = nil;
  javaMethod_LCLDoStopReadingAccelerometer: jmethodid = nil;
  javaMethod_LCLDoSendMessage: jmethodid = nil;
  javaMethod_LCLDoRequestPositionInfo: jmethodid = nil;
  // Methods from android.app.Activity
  javaMethod_Activity_finish: jmethodid = nil;
  // Methods from java.lang.System
  javaMethod_System_exit: jmethodid = nil;
  // Generic methods from Context
  javaMethod_getSystemService: jmethodid = nil;

  // This is utilized to store the information such as invalidate requests in events
  eventResult: jint;
{$endif}

implementation

uses
  WsControls, lclintf,
  CustomDrawnWSFactory,
  CustomDrawnWSForms,
{  Win32WSButtons,
  Win32WSMenus,
  Win32WSStdCtrls,
  Win32WSDialogs,
  Win32Themes,
////////////////////////////////////////////////////
  Win32Extra,}
  customdrawnprivate,
  LCLMessageGlue;

  {$ifdef CD_Windows}
const
  CDBackendNativeHandle = nhtWindowsHWND;
  {$define CD_HasNativeFormHandle}
  {$endif}
  {$ifdef CD_X11}
const
  CDBackendNativeHandle = nhtX11TWindow;
  {$define CD_HasNativeFormHandle}
  {$endif}
  {$ifdef CD_Cocoa}
const
  CDBackendNativeHandle = nhtCocoaNSWindow;
  {$define CD_HasNativeFormHandle}
  {$endif}

{ TLazCDCustomFont }

constructor TLazCDCustomFont.Create;
begin
  inherited Create;
  {$ifndef CD_UseNativeText}
  FTFont := TFreeTypeFont.Create;
  {$endif}
end;

destructor TLazCDCustomFont.Destroy;
begin
  {$ifndef CD_UseNativeText}
  FTFont.Free;
  {$endif}
  inherited Destroy;
end;

{$I customdrawnobject.inc}

{$I customdrawnwinapi.inc}
{$I customdrawnlclintf.inc}

{$ifdef CD_Windows}
  {$include wincallback.inc}
  {$I customdrawnobject_win.inc}
  {$I customdrawnwinapi_win.inc}
  {$I customdrawnlclintf_win.inc}
{$endif}
{$ifdef CD_Cocoa}
  {$I customdrawnobject_cocoa.inc}
  {$I customdrawnwinapi_cocoa.inc}
  {$I customdrawnlclintf_cocoa.inc}
{$endif}
{$ifdef CD_X11}
  {$I customdrawnobject_x11.inc}
  {$I customdrawnwinapi_x11.inc}
  {$I customdrawnlclintf_x11.inc}
{$endif}
{$ifdef CD_Android}
  {$I customdrawnobject_android.inc}
  {$I customdrawnwinapi_android.inc}
  {$I customdrawnlclintf_android.inc}
{$endif}
{$ifdef CD_Wayland}
  {$I customdrawnobject_wayland.inc}
  {$I customdrawnwinapi_wayland.inc}
  {$I customdrawnlclintf_wayland.inc}
{$endif}

end.
