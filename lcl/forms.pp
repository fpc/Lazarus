{
 /***************************************************************************
                                  forms.pp
                                  --------
                             Component Library Code


                   Initial Revision  : Sun Mar 28 23:15:32 CST 1999
                   Revised : Sat Jul 15 1999

 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

unit Forms;


{$mode objfpc}{$H+}

interface

{$ifdef Trace}
{$ASSERTIONS ON}
{$endif}


uses
  Classes, Controls, VCLGlobals, SysUtils, LCLType, LCLProc, LCLLinux,
  InterfaceBase, GraphType, Graphics, Menus, LMessages, CustomTimer, ActnList,
  ClipBrd;

type
  TProcedure = procedure;
  TProcedureOfObject = procedure of object;

  // form position policies:
  TPosition = (
    poDesigned,        // bounds from the designer
    poDefault,         // LCL decision (normally window manager)
    poDefaultPosOnly,  // designed size and LCL position
    poDefaultSizeOnly, // designed position and LCL size
    poScreenCenter,    // center form on screen
    poDesktopCenter,   // center form on desktop
    poMainFormCenter,  // center form on main form
    poOwnerFormCenter  // center form on owner form
    );

  TWindowState = (wsNormal, wsMinimized, wsMaximized);
  TCloseAction = (caNone, caHide, caFree, caMinimize);

  TScrollingWinControl = class;
  
  
  { TControlScrollBar }

  TScrollBarKind = (sbHorizontal, sbVertical);
  TScrollBarInc = 1..32768;
  TScrollBarStyle = (ssRegular, ssFlat, ssHotTrack);

  TControlScrollBar = class(TPersistent)
  private
    FControl: TScrollingWinControl;

    FAutoRange : Longint;

    FKind: TScrollBarKind;

    FIncrement: TScrollBarInc;
    FPage: TScrollBarInc;
    FPosition: Integer;
    FRange: Integer;
    FSmooth : Boolean;
    FVisible: Boolean;

    function GetSize: integer;
    procedure SetPosition(Value: Integer);
    procedure SetRange(Value: Integer);
    procedure SetSize(const AValue: integer);
    procedure SetSmooth(Value: Boolean);
    procedure SetVisible(Value: Boolean);
  protected
    procedure AutoCalcRange;
    Procedure UpdateScrollBar;
    procedure ScrollHandler(var Message: TLMScroll);
  public
    constructor Create(AControl: TScrollingWinControl; AKind: TScrollBarKind);
    procedure Assign(Source: TPersistent); override;
    function IsScrollBarVisible: Boolean;
    function ScrollPos: Integer;
    property Kind: TScrollBarKind read FKind;
  published
    property Increment: TScrollBarInc read FIncrement write FIncrement default 8;
    property Page: TScrollBarInc read FPage write FPage default 80;
    property Smooth : Boolean read FSmooth write SetSmooth;// default True
    property Position: Integer read FPosition write SetPosition default 0;
    property Range: Integer read FRange write SetRange default 0;
    property Size: integer read GetSize write SetSize stored false;
    property Visible: Boolean read FVisible write SetVisible;// default True;
  end;
  
  
  { TScrollingWinControl }

  TScrollingWinControl = class(TWinControl)
  private
    FHorzScrollBar : TControlScrollBar;
    FVertScrollBar : TControlScrollBar;
    FAutoScroll    : Boolean;

    FOnPaint: TNotifyEvent;

    FCanvas : TControlCanvas;

    IsUpdating : Boolean;

    procedure SetAutoScroll(Value: Boolean);
    procedure SetHorzScrollBar(Value: TControlScrollBar);
    procedure SetVertScrollBar(Value: TControlScrollBar);
    Function StoreScrollBars : Boolean;
  Protected
    procedure AlignControls(AControl: TControl; var ARect: TRect); override;
    procedure CreateWnd; override;
    function  GetClientScrollOffset: TPoint; override;
    Procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
    procedure WMPaint(var message: TLMPaint); message LM_PAINT;
    procedure WMSize(var Message: TLMSize); message LM_Size;
    Procedure WMHScroll(var Message : TLMHScroll); message LM_HScroll;
    Procedure WMVScroll(var Message : TLMVScroll); message LM_VScroll;
    procedure ScrollBy(DeltaX, DeltaY: Integer);
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  Public
    Constructor Create(AOwner : TComponent); Override;
    Destructor Destroy; Override;

    procedure Paint; dynamic;
    procedure PaintWindow(dc : Hdc); override;

    Procedure UpdateScrollbars;

    property Canvas: TControlCanvas read FCanvas;
  published
    property AutoScroll: Boolean read FAutoScroll write SetAutoScroll;
    property HorzScrollBar: TControlScrollBar
      read FHorzScrollBar write SetHorzScrollBar stored StoreScrollBars;
    property VertScrollBar: TControlScrollBar
      read FVertScrollBar write SetVertScrollBar stored StoreScrollBars;
  end;
  
  
  { TScrollBox }

  TScrollBox = class(TScrollingWinControl)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property Anchors;
    property AutoSize default True;
    //property AutoScroll;
    //property BiDiMode;
    //property BorderStyle: TBorderStyle
    //  read FBorderStyle write SetBorderStyle default bsSingle;
    property Constraints;
    //property DockSite;
    property DragCursor;
    property DragKind;

    property DragMode;
    property Enabled;
    property Color nodefault;
    property Ctl3D;
    property Font;
    //property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    //property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnDblClick;
    //property OnDockDrop;
    //property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    //property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    //property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    //property OnStartDock;
    property OnStartDrag;
    //property OnUnDock;
    property OnPaint;
  end;

  TIDesigner = class;


  { TCustomForm }

  TCloseEvent = procedure(Sender: TObject; var Action: TCloseAction) of object;
  TCloseQueryEvent = procedure(Sender : TObject; var CanClose : boolean) of object;

  TFormStateType = (
    fsCreating,  // initializing (form streaming)
    fsVisible,   // form should be shown
    fsShowing,
    fsModal,     // form is modal
    fsCreatedMDIChild
    );
  TFormState = set of TFormStateType;

  TModalResult = low(Integer)..high(Integer);

  TCustomForm = class(TScrollingWinControl)
  private
    FActive : Boolean;
    FActiveControl : TWinControl;
    FBorderStyle : TFormBorderStyle;
    FDesigner : TIDesigner;
    FFormStyle : TFormStyle;
    FFormState: TFormState;
    FFormUpdateCount: integer;
    FIcon: TIcon;
    FKeyPreview: Boolean;
    FMenu : TMainMenu;
    FModalResult : TModalResult;
    FOnActivate: TNotifyEvent;
    FOnCreate: TNotifyEvent;
    FOnDeactivate : TNotifyEvent;
    FOnDestroy: TNotifyEvent;
    FOnHide: TNotifyEvent;
    FOnShow: TNotifyEvent;
    FOnClose: TCloseEvent;
    FOnCloseQuery : TCloseQueryEvent;
    FPosition : TPosition;
    FWindowState : TWindowState;
    FDummyTextHeight : Longint;
    procedure ClientWndProc(var Message: TLMessage);
    procedure CloseModal;
    procedure DoCreate;
    procedure DoDestroy;
    procedure SetActive(AValue: Boolean);
    procedure SetActiveControl(AWinControl: TWinControl);
    procedure SetBorderStyle(Value : TFORMBorderStyle);
    procedure SetDesigner(Value : TIDesigner);
    procedure SetMenu(Value : TMainMenu);
    procedure SetFormStyle(Value : TFormStyle);
    procedure SetIcon(AValue: TIcon);
    procedure SetPosition(Value : TPosition);
    procedure SetVisible(Value: boolean);
    procedure SetWindowFocus;
    procedure SetWindowState(Value : TWIndowState);
    function IsForm : Boolean;
    procedure IconChanged(Sender: TObject);
    function IsIconStored: Boolean;
    { events }
    procedure WMActivate(var Message : TLMActivate); message LM_ACTIVATE;
    procedure WMDeactivate(var Message : TLMActivate); message LM_DEACTIVATE;
    procedure WMPaint(var message: TLMPaint); message LM_PAINT;
    procedure WMSize(var message: TLMSize); message LM_Size;
    procedure WMShowWindow(var message: TLMShowWindow); message LM_SHOWWINDOW;
    procedure WMCloseQuery(var message: TLMessage); message LM_CLOSEQUERY;
  protected
    procedure Activate; dynamic;
    procedure ActiveChanged; dynamic;
    function CloseQuery : boolean; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Loaded; override;
    procedure BeginFormUpdate;
    procedure Deactivate;dynamic;
    procedure DoClose(var Action: TCloseAction); dynamic;
    procedure DoHide; dynamic;
    procedure DoShow; dynamic;
    procedure EndFormUpdate;
    function FormUpdating: boolean;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    // Delphi needed GetClientRect for win32 specific things, LCL not
    // Function GetClientRect : TRect ; Override;
    procedure Notification(AComponent: TComponent; Operation : TOperation);override;
    procedure PaintWindow(dc : Hdc); override;
    procedure RequestAlign; override;
    procedure UpdateShowing; override;
    procedure UpdateWindowState;
    procedure ValidateRename(AComponent: TComponent;
                             const CurName, NewName: string);override;
    Procedure SetZOrder(Topmost: Boolean); override;
    procedure WndProc(var TheMessage : TLMessage); override;
  public
    // properties
    property ActiveControl : TWinControl read FActiveControl write SetActiveControl;
    property Icon: TIcon read FIcon write SetIcon stored IsIconStored;
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    property OnClose: TCloseEvent read FOnClose write FOnClose stored IsForm;
    property OnCloseQuery : TCloseQueryEvent
                     read FOnCloseQuery write FOnCloseQuery stored IsForm;
    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
    property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property OnHide: TNotifyEvent read FOnHide write FOnHide;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property OnResize stored IsForm;
    property Position : TPosition read FPosition write SetPosition default poDesigned;
    property TextHeight : Longint read FDummyTextHeight write FDummyTextHeight stored False;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateNew(AOwner: TComponent; Num : Integer); virtual;
    procedure BeforeDestruction; override;
    function GetIconHandle: HICON;
    destructor Destroy; override;
    procedure Close;
    procedure Release;
    procedure Hide;
    function WantChildKey(Child : TControl; var MEssage : TLMessage): Boolean; virtual;
    procedure SetFocus; override;
    function SetFocusedControl(Control : TWinControl): Boolean ; Virtual;
    procedure FocusControl(WinControl : TWinControl);
    function ShowModal : Integer;
    property Active : Boolean read FActive;
    property BorderStyle : TFormBorderStyle
      read FBorderStyle write SetBorderStyle default bsSizeable;
    //property Canvas: TControlCanvas read GetCanvas;
    property Caption stored IsForm;
    property Designer : TIDesigner read FDesigner write SetDesigner;
    property FormStyle : TFormStyle read FFormStyle write SetFormStyle default fsNormal;
    property FormState : TFormState read FFormState;
    property KeyPreview: Boolean read FKeyPreview write FKeyPreview;
    property Menu : TMainMenu read FMenu write SetMenu;
    property ModalResult : TModalResult read FModalResult write FModalResult;
    property Visible write SetVisible default False;
    property WindowState: TWindowState read FWindowState write SetWindowState default wsNormal;
  end;
  
  
  { TForm }

  TForm = class(TCustomForm)
  private
    FClientHandle: HWND;
    FDummyPPI : longint;
  public
    property ClientHandle: HWND read FClientHandle;
  published
    property PixelsPerInch : Longint read FDummyPPI write FDummyPPI stored False;
    property ActiveControl;
    property Align;
    property AutoSize;
    property BorderStyle;
    property Caption;
    property Color;
    property ClientHeight;
    property ClientWidth;
    property Constraints;
    property Enabled;
    property Font;
    property FormStyle;
    property Icon;
    property Menu;
    property ParentFont;
    property PopupMenu;
    property Position;
    property ShowHint;
    property TextHeight;
    property Visible;
    property WindowState;
    property OnActivate;
    property OnCreate;
    property OnClose;
    property OnCloseQuery;
    property OnDeactivate;
    property OnDestroy;
    property OnKeyPress;
    property OnKeyUp;
    property OnKeyDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseDown;
    property OnShow;
    property OnHide;
    property OnPaint;
    property OnResize;
  end;

  TFormClass = class of TForm;
  

  { THintWindow }
  
  THintWindow = class(TCustomForm)
  private
    FActivating: Boolean;
    FAutoHide : Boolean;
    FAutoHideTimer : TComponent;
    FHideInterval : Integer;
    procedure SetAutoHide(Value : Boolean);
    procedure AutoHideHint(Sender : TObject);
    procedure SetHideInterval(Value : Integer);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ActivateHint(ARect: TRect; const AHint: String); virtual;
    function CalcHintRect(MaxWidth: Integer; const AHint: String; AData: Pointer): TRect; virtual;
    property AutoHide : Boolean read FAutoHide write SetAutoHide;
    property HideInterval : Integer read FHideInterval write SetHideInterval;
  end;

  THintWindowClass = class of THintWindow;


  { TScreen }

  PCursorRec = ^TCursorRec;
  TCursorRec = record
    Next: PCursorRec;
    Index: Integer;
    Handle: HCURSOR;
  end;

  TScreen = class(TComponent)
  private
    FActiveControl: TWinControl;
    FActiveCustomForm: TCustomForm;
    FActiveForm: TForm;
    FCursor: TCursor;
    FCursorCount: integer;
    FCursorList: PCursorRec;
    FCustomForms: TList;
    FDefaultCursor: HCURSOR;
    FFocusedForm: TCustomForm;
    FFonts : TStrings;
    FFormList: TList;
    FHintFont : TFont;
    FLastActiveControl: TWinControl;
    FLastActiveCustomForm: TCustomForm;
    FOnActiveControlChange: TNotifyEvent;
    FOnActiveFormChange: TNotifyEvent;
    FPixelsPerInch : integer;
    FSaveFocusedList: TList;

    procedure CreateCursors;
    procedure DeleteCursor(Index: Integer);
    procedure DestroyCursors;
    function GetCursors(Index: Integer): HCURSOR;
    function GetCustomFormCount: Integer;
    function GetCustomForms(Index: Integer): TCustomForm;
    function GetFonts : TStrings;
    function GetFormCount: Integer;
    function GetForms(IIndex: Integer): TForm;
    function GetHeight : Integer;
    function GetWidth : Integer;
    procedure AddForm(AForm: TCustomForm);
    procedure RemoveForm(AForm: TCustomForm);
    procedure SetCursor(const AValue: TCursor);
    procedure SetCursors(Index: Integer; const AValue: HCURSOR);
    procedure UpdateLastActive;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; Override;
  public
    property ActiveControl: TWinControl read FActiveControl;
    property ActiveCustomForm: TCustomForm read FActiveCustomForm;
    property ActiveForm: TForm read FActiveForm;
    property Cursor: TCursor read FCursor write SetCursor;
    property Cursors[Index: Integer]: HCURSOR read GetCursors write SetCursors;
    property CustomFormCount: Integer read GetCustomFormCount;
    property CustomForms[Index: Integer]: TCustomForm read GetCustomForms;
    property FormCount: Integer read GetFormCount;
    property Forms[Index: Integer]: TForm read GetForms;
    property Fonts : TStrings read GetFonts;
    property PixelsPerInch : integer read FPixelsPerInch;
    property HintFont : TFont read FHintFont;
    property Height : Integer read Getheight;
    property Width : Integer read GetWidth;
    property OnActiveControlChange: TNotifyEvent
      read FOnActiveControlChange write FOnActiveControlChange;
    property OnActiveFormChange: TNotifyEvent
      read FOnActiveFormChange write FOnActiveFormChange;
  end;


  { TApplication }

  TExceptionEvent = procedure (Sender: TObject; E: Exception) of object;
  TIdleEvent = procedure (Sender: TObject; var Done: Boolean) of object;
  TOnUserInputEvent = procedure(Sender: TObject; Msg: Cardinal) of object;
  
  // application hint stuff
  PHintInfo = ^THintInfo;
  THintInfo = record
    HintControl: TControl;
    HintWindowClass: THintWindowClass;
    HintPos: TPoint;
    HintMaxWidth: Integer;
    HintColor: TColor;
    CursorRect: TRect;
    CursorPos: TPoint;
    ReshowTimeout: Integer;
    HideTimeout: Integer;
    HintStr: string;
    HintData: Pointer;
  end;

  TCMHintShow = record
    Msg: Cardinal;
    Reserved: Integer;
    HintInfo: PHintInfo;
    Result: Integer;
  end;

  TCMHintShowPause = record
    Msg: Cardinal;
    WasActive: Integer;
    Pause: PInteger;
    Result: Integer;
  end;

  TAppHintTimerType = (ahtNone, ahtShowHint, ahtHideHint, ahtReshowHint);

  TShowHintEvent = procedure (var HintStr: string; var CanShow: Boolean;
    var HintInfo: THintInfo) of object;

  THintInfoAtMouse = record
    MousePos: TPoint;
    Control: TControl;
    ControlHasHint: boolean;
  end;

  TApplicationFlag = (AppWaiting, AppIdleEndSent);
  TApplicationFlags = set of TApplicationFlag;

  TApplication = class(TComponent)
  private
    FFlag: TApplicationFlags;
    FHandle : THandle;
    FHint: string;
    FHintColor: TColor;
    FHintControl: TControl;
    FHintHidePause: Integer;
    FHintPause: Integer;
    FHintShortCuts: Boolean;
    FHintShortPause: Integer;
    FHintTimer: TCustomTimer;
    FHintTimerType: TAppHintTimerType;
    FHintWindow: THintWindow;
    FIcon: TIcon;
    FList: TList;
    FMainForm : TForm;
    FMouseControl: TControl;
    FOnException: TExceptionEvent;
    FOnHint: TNotifyEvent;
    FOnIdle: TIdleEvent;
    FOnIdleHandler: TMethodList;
    FOnIdleEnd: TNotifyEvent;
    FOnIdleEndHandler: TMethodList;
    FOnShowHint: TShowHintEvent;
    FOnUserInput: TOnUserInputEvent;
    FOnUserInputHandler: TMethodList;
    FShowHint: Boolean;
    FTerminate : Boolean;
    FTitle : String;
    procedure DoOnIdleEnd;
    function GetExename: String;
    function GetIconHandle: HICON;
    function GetTitle: string;
    procedure IconChanged(Sender: TObject);
    procedure Idle;
    procedure MouseIdle(const CurrentControl: TControl);
    procedure SetHint(const AValue: string);
    procedure SetHintColor(const AValue: TColor);
    procedure SetIcon(AValue: TIcon);
    procedure SetShowHint(const AValue: Boolean);
    procedure StopHintTimer;
    procedure WndProc(var AMessage : TLMessage);
  protected
    procedure NotifyIdleHandler;
    procedure NotifyIdleEndHandler;
    function IsHintMsg(var Msg: TMsg): Boolean;
    procedure DoOnMouseMove; virtual;
    procedure ShowHintWindow(const Info: THintInfoAtMouse);
    procedure StartHintTimer(Interval: integer; TimerType: TAppHintTimerType);
    procedure OnHintTimer(Sender: TObject);
    procedure UpdateVisible;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ControlDestroyed(AControl: TControl);
    Procedure BringToFront;
    procedure CreateForm(InstanceClass: TComponentClass; var Reference);
    function HandleAllocated: boolean;
    procedure HandleException(Sender: TObject);
    procedure HandleMessage;
    function IsWaiting: boolean;
    procedure CancelHint;
    procedure HideHint;
    procedure HintMouseMessage(Control : TControl; var AMessage: TLMessage);
    property Icon: TIcon read FIcon write SetIcon;
    procedure Initialize;
    function MessageBox(Text, Caption : PChar; Flags : Longint) : Integer;
    procedure Notification(AComponent : TComponent; Operation : TOperation); override;
    Procedure ProcessMessages;
    procedure Run;
    procedure ShowException(E: Exception);
    procedure Terminate;
    procedure NotifyUserInputHandler(Msg: Cardinal);
    procedure AddOnIdleHandler(AnOnIdleHandler: TNotifyEvent);
    procedure RemoveOnIdleHandler(AnOnIdleHandler: TNotifyEvent);
    procedure AddOnIdleEndHandler(AnOnIdleEndHandler: TNotifyEvent);
    procedure RemoveOnIdleEndHandler(AnOnIdleEndHandler: TNotifyEvent);
    procedure AddOnUserInputHandler(AnOnUserInputHandler: TOnUserInputEvent);
    procedure RemoveOnUserInputHandler(AnOnUserInputHandler: TOnUserInputEvent);
    procedure DoBeforeMouseMessage(CurMouseControl: TControl);
  public
    property Exename: String read GetExeName;
    property Handle: THandle read FHandle;
    property Hint: string read FHint write SetHint;
    property HintColor: TColor read FHintColor write SetHintColor;
    property HintHidePause: Integer read FHintHidePause write FHintHidePause;
    property HintPause: Integer read FHintPause write FHintPause;
    property HintShortCuts: Boolean read FHintShortCuts write FHintShortCuts;
    property HintShortPause: Integer read FHintShortPause write FHintShortPause;
    property MainForm: TForm read FMainForm;
    property Terminated: Boolean read FTerminate;
    property OnException: TExceptionEvent read FOnException write FOnException;
    property OnIdle: TIdleEvent read FOnIdle write FOnIdle;
    property OnIdleEnd: TNotifyEvent read FOnIdleEnd write FOnIdleEnd;
    property OnHint: TNotifyEvent read FOnHint write FOnHint;
    property OnShowHint: TShowHintEvent read FOnShowHint write FOnShowHint;
    property OnUserInput: TOnUserInputEvent read FOnUserInput write FOnUserInput;
    property ShowHint: Boolean read FShowHint write SetShowHint;
    property Title: String read GetTitle write FTitle;
  end;
  
  
  { TIDesigner }

  TIDesigner = class(TObject)
  public
    function IsDesignMsg(Sender: TControl; var Message: TLMessage): Boolean;
      virtual; abstract;
    procedure Modified; virtual; abstract;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); virtual; abstract;
    procedure PaintGrid; virtual; abstract;
    procedure ValidateRename(AComponent: TComponent;
      const CurName, NewName: string); virtual; abstract;
    function GetShiftState: TShiftState; virtual; abstract;
    Procedure SelectOnlyThisComponent(AComponent:TComponent); virtual; abstract;
  end;


function KeysToShiftState(Keys:Word): TShiftState;
function KeyDataToShiftState(KeyData: Longint): TShiftState;

type
  TFocusState = type Pointer;

function SaveFocusState: TFocusState;
procedure RestoreFocusState(FocusState: TFocusState);

function GetParentForm(Control:TControl): TCustomForm;
function FindRootDesigner(AComponent: TComponent): TIDesigner;

function IsAccel(VK : Word; const Str : ShortString): Boolean;
procedure NotifyApplicationUserInput(Msg: Cardinal);

function InitResourceComponent(Instance: TComponent;
  RootAncestor: TClass):Boolean;

function GetShortHint(const Hint: string): string;
function GetLongHint(const Hint: string): string;


var
  Application : TApplication;
  Screen : TScreen;
  ExceptionObject : TExceptObject;
  HintWindowClass: THintWindowClass;

type
  TMessageBoxFunction =
    function(Text, Caption : PChar; Flags : Longint) : Integer;
var
  MessageBoxFunction: TMessageBoxFunction;

procedure FreeInterfaceObject;


implementation


uses
  LResources, Math;

var
  FocusMessages: Boolean;
  FocusCount: Integer;

{------------------------------------------------------------------------------
  procedure NotifyApplicationUserInput;

 ------------------------------------------------------------------------------}
procedure NotifyApplicationUserInput(Msg: Cardinal);
begin
  if Application<>nil then
    Application.NotifyUserInputHandler(Msg);
end;


//------------------------------------------------------------------------------
procedure ExceptionOccurred(Sender : TObject; Addr,Frame : Pointer);
var
  Mess : String;
Begin
  Writeln('[FORMS.PP] ExceptionOccurred Procedure');
  Mess := 'Error occurred in '+Sender.ClassName+' at '#13#10'Address '+HexStr(Cardinal(Addr),8)+#13#10'Frame '+HexStr(Cardinal(Frame),8);
  if Application<>nil then
    Application.MessageBox(PChar(Mess),'Exception',mb_IconError+mb_Ok)
  else
    writeln(Mess);
end;

//------------------------------------------------------------------------------
// The focus state is just the focus count for now. To save having to allocate
// anything, I just map the Integer to the TFocusState.
function SaveFocusState: TFocusState;
begin
  Result := TFocusState(FocusCount);
end;

procedure RestoreFocusState(FocusState: TFocusState);
begin
  FocusCount := Integer(FocusState);
end;

function SendFocusMessage(Window: HWnd; Msg: Word): Boolean;
var
  Count: Integer;
begin
  Count := FocusCount;
  SendMessage(Window, Msg, 0, 0);
  Result := (FocusCount = Count);
end;

//------------------------------------------------------------------------------
function KeysToShiftState(Keys:Word): TShiftState;
begin
  Result := [];
  if Keys and MK_Shift <> 0 then Include(Result,ssShift);
  if Keys and MK_Control <> 0 then Include(Result,ssCtrl);
  if Keys and MK_LButton <> 0 then Include(Result,ssLeft);
  if Keys and MK_RButton <> 0 then Include(Result,ssRight);
  if Keys and MK_MButton <> 0 then Include(Result,ssMiddle);
  if GetKeyState(VK_MENU) < 0 then Include(Result, ssAlt);
end;

function KeyDataToShiftState(KeyData: Longint): TShiftState;
begin
  Result := [];

  if GetKeyState(VK_SHIFT) < 0 then Include(Result, ssShift);
  if GetKeyState(VK_CONTROL) < 0 then Include(Result, ssCtrl);
  if KeyData and $20000000 <> 0 then Include(Result, ssAlt);
end;

//------------------------------------------------------------------------------
function GetParentForm(Control:TControl): TCustomForm;
begin
  while Control.Parent <> nil do
    Control := Control.Parent;
  if Control is TCustomForm 
  then Result := TCustomForm(Control) 
  else Result := nil;
end;

//------------------------------------------------------------------------------
function IsAccel(VK : Word; const Str : ShortString): Boolean;
begin
  Result := true;
end;


//==============================================================================

function InitResourceComponent(Instance: TComponent;
  RootAncestor: TClass):Boolean;

  function InitComponent(ClassType: TClass): Boolean;
  
    procedure ApplyVisible;
    var
      i: integer;
      AControl: TControl;
    begin
      // The LCL has as default Visible=false. But for Delphi compatbility
      // loading control defaults to true.
      if Instance is TControl then
        for i:=0 to Instance.ComponentCount-1 do begin
          AControl:=TControl(Instance.Components[i]);
          if (AControl is TControl) then begin
            if (not (csVisibleSetInLoading in AControl.ControlState)) then
              AControl.Visible:=true
            else
              AControl.ControlState:=
                AControl.ControlState-[csVisibleSetInLoading];
          end;
        end;
    end;
  
  var
    CompResource:TLResource;
    MemStream: TMemoryStream;
  begin
//writeln('[InitComponent] ',ClassType.Classname,' ',Instance<>nil);
    Result:=false;
    if (ClassType=TComponent) or (ClassType=RootAncestor) then exit;
    if Assigned(ClassType.ClassParent) then
      Result:=InitComponent(ClassType.ClassParent);
    CompResource:=LazarusResources.Find(ClassType.ClassName);
    if (CompResource = nil) or (CompResource.Value='') then exit;
//writeln('[InitComponent] CompResource found for ',ClassType.Classname);
    if (ClassType.InheritsFrom(TForm))
    and (CompResource.ValueType<>'FORMDATA') then exit;
    MemStream:=TMemoryStream.Create;
    try
      MemStream.Write(CompResource.Value[1],length(CompResource.Value));
      MemStream.Position:=0;
      writeln('Form Stream "',ClassType.ClassName,'" Signature=',copy(CompResource.Value,1,4));
      try
        Instance:=MemStream.ReadComponent(Instance);
      except
        on E: Exception do begin
          writeln('Form streaming "',ClassType.ClassName,'" error: ',E.Message);
          exit;
        end;
      end;
    finally
      ApplyVisible;
      MemStream.Free;
    end;
    Result:=true;
  end;

// InitResourceComponent
//var LocalizedLoading: Boolean;
begin
  //GlobalNameSpace.BeginWrite; // hold lock across all ancestor loads (performance)
  try
    //LocalizedLoading:=(Instance.ComponentState * [csInline,csLoading])=[];
    //if LocalizedLoading then BeginGloabelLoading; // push new loadlist onto stack
    try
      Result:=InitComponent(Instance.ClassType);
      //if LocalizedLoading then NotifyGloablLoading; // call Loaded
    finally
      //if LocalizedLoading then EndGloablLoading; // pop loadlist off stack
    end;
  finally
    //GlobalNameSpace.EndWrite;
  end;
end;

function FindRootDesigner(AComponent: TComponent): TIDesigner;
var
  Form: TCustomForm;
begin
  Result:=nil;
  if AComponent=nil then exit;
  while (AComponent<>nil) do begin
    if (AComponent is TCustomForm) then begin
      Form:=TCustomForm(AComponent);
      if Form.Parent=nil then begin
        Result:=Form.Designer;
        exit;
      end;
    end;
    if AComponent is TControl then begin
      AComponent:=TControl(AComponent).Parent;
    end else begin
      exit;
    end;
  end;
end;

function SendApplicationMsg(Msg: Cardinal; WParam, LParam: Longint): Longint;
begin
  if (Application<>nil) and (Application.HandleAllocated) then
    Result := SendMessage(Application.Handle, Msg, WParam, LParam)
  else
    Result := 0;
end;

procedure IfOwnerIsFormThenDesignerModified(AComponent: TComponent);
begin
  if (AComponent<>nil) and (AComponent.Owner<>nil)
  and ([csDesigning,csLoading]*AComponent.ComponentState=[csDesigning])
  and (AComponent.Owner is TForm)
  and (TForm(AComponent.Owner).Designer <> nil) then
    TForm(AComponent.Owner).Designer.Modified;
end;

function GetShortHint(const Hint: string): string;
var
  I: Integer;
begin
  I := Pos('|', Hint);
  if I = 0 then
    Result := Hint else
    Result := Copy(Hint, 1, I - 1);
end;

function GetLongHint(const Hint: string): string;
var
  I: Integer;
begin
  I := Pos('|', Hint);
  if I = 0 then
    Result := Hint else
    Result := Copy(Hint, I + 1, Maxint);
end;

procedure FreeInterfaceObject;
begin
  Application.Free;
  Application:=nil;
  FreeAllClipBoards;
  InterfaceObject.Free;
  InterfaceObject:=nil;
end;

//==============================================================================


{$I scrollingwincontrol.inc}
{$I scrollbox.inc}
{$I customform.inc}
{$I screen.inc}
{$I application.inc}
{$I hintwindow.inc}


initialization
  FocusCount := 0;
  Focusmessages := True;
  HintWindowClass := THintWindow;
  LCLProc.OwnerFormDesignerModifiedProc:=@IfOwnerIsFormThenDesignerModified;
  Screen:= TScreen.Create(nil);
  Application:= TApplication.Create(nil);

finalization
  writeln('forms.pp - finalization section');
  LCLProc.OwnerFormDesignerModifiedProc:=nil;
  HintWindowClass:=THintWindow;
  FreeThenNil(Application);
  FreeThenNil(Screen);

end.


