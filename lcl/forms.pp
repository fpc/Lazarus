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
  Classes, Controls, LCLStrConsts, VCLGlobals, SysUtils, LCLType, LCLProc,
  LCLLinux, InterfaceBase, GraphType, Graphics, Menus, LMessages, CustomTimer,
  ActnList, ClipBrd;

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
  EScrollBar = class(Exception) end;

  TControlScrollBar = class(TPersistent)
  private
    FAutoRange : Longint;
    FIncrement: TScrollBarInc;
    FKind: TScrollBarKind;
    FPage: TScrollBarInc;
    FPosition: Integer;
    FRange: Integer;
    FSmooth : Boolean;
    FVisible: Boolean;
  protected
    FControl: TWinControl;
    function ControlAutoScroll: boolean; virtual;
    function ControlHandle: HWnd; virtual;
    function GetIncrement: TScrollBarInc; virtual;
    function GetPage: TScrollBarInc; virtual;
    function GetPosition: Integer; virtual;
    function GetRange: Integer; virtual;
    function GetSize: integer; virtual;
    function GetSmooth: Boolean; virtual;
    function GetVisible: Boolean; virtual;
    function HandleAllocated: boolean; virtual;
    function SmoothIsStored: boolean; virtual;
    function VisibleIsStored: boolean; virtual;
    procedure AutoCalcRange; virtual;
    procedure ControlUpdateScrollBars; virtual;
    procedure ScrollControlBy(DeltaX, DeltaY: integer); virtual;
    procedure ScrollHandler(var Message: TLMScroll);
    procedure SetIncrement(const AValue: TScrollBarInc); virtual;
    procedure SetPage(const AValue: TScrollBarInc); virtual;
    procedure SetPosition(const Value: Integer); virtual;
    procedure SetRange(const Value: Integer); virtual;
    procedure SetSize(const AValue: integer); virtual;
    procedure SetSmooth(const Value: Boolean); virtual;
    procedure SetVisible(const Value: Boolean); virtual;
    Procedure UpdateScrollBar; virtual;
  public
    constructor Create(AControl: TWinControl; AKind: TScrollBarKind);
    procedure Assign(Source: TPersistent); override;
    function IsScrollBarVisible: Boolean; virtual;
    function ScrollPos: Integer; virtual;
    property Kind: TScrollBarKind read FKind;
    function GetOtherScrollBar: TControlScrollBar;
    function GetHorzScrollBar: TControlScrollBar; virtual;
    function GetVertScrollBar: TControlScrollBar; virtual;
    property Size: integer read GetSize write SetSize stored false;
  published
    property Increment: TScrollBarInc read GetIncrement write SetIncrement default 8;
    property Page: TScrollBarInc read GetPage write SetPage default 80;
    property Smooth : Boolean read GetSmooth write SetSmooth stored SmoothIsStored;
    property Position: Integer read GetPosition write SetPosition default 0;
    property Range: Integer read GetRange write SetRange default 0;
    property Visible: Boolean read GetVisible write SetVisible stored VisibleIsStored;
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
  protected
    function GetClientScrollOffset: TPoint; override;
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


  { TCustomForm }

  TIDesigner = class;

  TCloseEvent = procedure(Sender: TObject; var Action: TCloseAction) of object;
  TCloseQueryEvent = procedure(Sender : TObject;
                               var CanClose : boolean) of object;
  THelpEvent = function(Command: Word; Data: Longint;
    var CallHelp: Boolean): Boolean of object;

  TFormStateType = (
    fsCreating,  // initializing (form streaming)
    fsVisible,   // form should be shown
    fsShowing,
    fsModal,     // form is modal
    fsCreatedMDIChild,
    fsBorderStyleChanged,
    fsFormStyleChanged
    );
  TFormState = set of TFormStateType;

  TModalResult = low(Integer)..high(Integer);

  TCustomForm = class(TScrollingWinControl)
  private
    FActive: Boolean;
    FActiveControl: TWinControl;
    FBorderStyle: TFormBorderStyle;
    FDesigner: TIDesigner;
    FDummyTextHeight: Longint;
    FFormState: TFormState;
    FFormStyle: TFormStyle;
    FFormUpdateCount: integer;
    FHelpFile: string;
    FIcon: TIcon;
    FKeyPreview: Boolean;
    FMenu: TMainMenu;
    FModalResult: TModalResult;
    FOnActivate: TNotifyEvent;
    FOnClose: TCloseEvent;
    FOnCloseQuery: TCloseQueryEvent;
    FOnCreate: TNotifyEvent;
    FOnDeactivate: TNotifyEvent;
    FOnDestroy: TNotifyEvent;
    FOnHelp: THelpEvent;
    FOnHide: TNotifyEvent;
    FOnShow: TNotifyEvent;
    FOnWindowStateChanged: TNotifyEvent;
    FPosition: TPosition;
    FWindowState: TWindowState;
    function IsForm : Boolean;
    function IsHelpFileStored: boolean;
    function IsIconStored: Boolean;
    procedure ClientWndProc(var Message: TLMessage);
    procedure CloseModal;
    procedure DoCreate;
    procedure DoDestroy;
    procedure IconChanged(Sender: TObject);
    function IsKeyPreviewStored: boolean;
    procedure SetActive(AValue: Boolean);
    procedure SetActiveControl(AWinControl: TWinControl);
    procedure SetBorderStyle(Value : TFormBorderStyle);
    procedure SetDesigner(Value : TIDesigner);
    procedure SetFormStyle(Value : TFormStyle);
    procedure SetIcon(AValue: TIcon);
    procedure SetMenu(Value : TMainMenu);
    procedure SetModalResult(const AValue: TModalResult);
    procedure SetPosition(Value : TPosition);
    procedure SetVisible(Value: boolean);
    procedure SetWindowFocus;
    procedure SetWindowState(Value : TWIndowState);
    procedure WMActivate(var Message : TLMActivate); message LM_ACTIVATE;
    procedure WMCloseQuery(var message: TLMessage); message LM_CLOSEQUERY;
    procedure WMDeactivate(var Message : TLMActivate); message LM_DEACTIVATE;
    procedure WMPaint(var message: TLMPaint); message LM_PAINT;
    procedure WMShowWindow(var message: TLMShowWindow); message LM_SHOWWINDOW;
    procedure WMSize(var message: TLMSize); message LM_Size;
  protected
    function CloseQuery : boolean; virtual;
    function FormUpdating: boolean;
    procedure Activate; dynamic;
    procedure ActiveChanged; dynamic;
    procedure BeginFormUpdate;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Deactivate;dynamic;
    procedure DoClose(var Action: TCloseAction); dynamic;
    procedure DoHide; dynamic;
    procedure DoShow; dynamic;
    procedure EndFormUpdate;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure Loaded; override;
    // Delphi needs GetClientRect for win32 specific things, LCL not
    // Function GetClientRect : TRect ; Override;
    procedure Notification(AComponent: TComponent; Operation : TOperation);override;
    procedure PaintWindow(dc : Hdc); override;
    procedure RequestAlign; override;
    Procedure SetZOrder(Topmost: Boolean); override;
    procedure UpdateShowing; override;
    procedure UpdateWindowState;
    procedure ValidateRename(AComponent: TComponent;
                             const CurName, NewName: string); override;
    procedure VisibleChanging; override;
    procedure WndProc(var TheMessage : TLMessage); override;
    function VisibleIsStored: boolean;
  public
    // properties
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateNew(AOwner: TComponent; Num : Integer); virtual;
    procedure BeforeDestruction; override;
    function GetIconHandle: HICON;
    destructor Destroy; override;
    procedure Close;
    procedure Release;
    procedure Hide;
    procedure Show;
    procedure ShowOnTop;
    function WantChildKey(Child : TControl;
                          var Message : TLMessage): Boolean; virtual;
    procedure SetFocus; override;
    function SetFocusedControl(Control: TWinControl): Boolean ; Virtual;
    procedure FocusControl(WinControl: TWinControl);
    function ShowModal : Integer;
  public
    property Active: Boolean read FActive;
    property ActiveControl: TWinControl read FActiveControl write SetActiveControl;
    property BorderStyle: TFormBorderStyle
                      read FBorderStyle write SetBorderStyle default bsSizeable;
    property Caption stored IsForm;
    property Designer: TIDesigner read FDesigner write SetDesigner;
    property FormState: TFormState read FFormState;
    property FormStyle: TFormStyle read FFormStyle write SetFormStyle
                                   default fsNormal;
    property HelpFile: string read FHelpFile write FHelpFile
                              stored IsHelpFileStored;
    property Icon: TIcon read FIcon write SetIcon stored IsIconStored;
    property KeyPreview: Boolean read FKeyPreview write FKeyPreview
                                 stored IsKeyPreviewStored;
    property Menu : TMainMenu read FMenu write SetMenu;
    property ModalResult : TModalResult read FModalResult write SetModalResult;
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    property OnClose: TCloseEvent read FOnClose write FOnClose stored IsForm;
    property OnCloseQuery : TCloseQueryEvent
                     read FOnCloseQuery write FOnCloseQuery stored IsForm;
    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
    property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property OnHelp: THelpEvent read FOnHelp write FOnHelp;
    property OnHide: TNotifyEvent read FOnHide write FOnHide;
    property OnResize stored IsForm;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property OnWindowStateChanged: TNotifyEvent
                         read fOnWindowStateChanged write fOnWindowStateChanged;
    property Position: TPosition read FPosition write SetPosition default poDesigned;
    property TextHeight: Longint read FDummyTextHeight write FDummyTextHeight
                                 stored False;
    property Visible write SetVisible stored VisibleIsStored default false;
    property WindowState: TWindowState read FWindowState write SetWindowState
                                       default wsNormal;
  end;
  
  
  { TForm }

  TForm = class(TCustomForm)
  private
    FClientHandle: HWND;
    FDummyPPI : longint;
  public
    property ClientHandle: HWND read FClientHandle;
  published
    property ActiveControl;
    property Align;
    property AutoSize;
    property BorderStyle;
    property Caption;
    property ClientHeight;
    property ClientWidth;
    property Color;
    property Constraints;
    property Enabled;
    property Font;
    property FormStyle;
    property HelpFile;
    property Icon;
    property KeyPreview;
    property Menu;
    property OnActivate;
    property OnClose;
    property OnCloseQuery;
    property OnCreate;
    property OnDeactivate;
    property OnDestroy;
    property OnHide;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnHelp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaint;
    property OnResize;
    property OnShow;
    property ParentFont;
    property PixelsPerInch : Longint read FDummyPPI write FDummyPPI stored False;
    property PopupMenu;
    property Position;
    property ShowHint;
    property TextHeight;
    property Visible;
    property WindowState;
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
    procedure ReleaseHandle;
  public
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
  
  TScreenFormEvent = procedure(Sender: TObject; Form: TCustomForm) of object;
  TScreenActiveFormChangedEvent = procedure(Sender: TObject;
                                            LastForm: TCustomForm) of object;
  TScreenControlEvent = procedure(Sender: TObject;
                                  LastControl: TControl) of object;

  TScreenNotification = (
    snFormAdded,
    snRemoveForm,
    snActiveControlChanged,
    snActiveFormChanged
    );

  TScreen = class(TComponent)
  private
    FActiveControl: TWinControl;
    FActiveCustomForm: TCustomForm;
    FActiveForm: TForm;
    FCursor: TCursor;
    FCursorCount: integer;
    FCursorList: PCursorRec;
    FCustomForms: TList;
    FCustomFormsZOrdered: TList;
    FDefaultCursor: HCURSOR;
    FFocusedForm: TCustomForm;
    FFonts : TStrings;
    FFormList: TList;
    FHandlers: array[TScreenNotification] of TMethodList;
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
    function GetCustomFormsZOrdered(Index: Integer): TCustomForm;
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
    procedure AddHandler(HandlerType: TScreenNotification;
                         const Handler: TMethod);
    procedure RemoveHandler(HandlerType: TScreenNotification;
                            const Handler: TMethod);
    function GetHandlerCount(HandlerType: TScreenNotification): integer;
    function GetNextHandlerIndex(HandlerType: TScreenNotification;
                                 var i: integer): boolean;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; Override;
    function CustomFormIndex(AForm: TCustomForm): integer;
    function FormIndex(AForm: TForm): integer;
    function CustomFormZIndex(AForm: TCustomForm): integer;
    procedure MoveFormToFocusFront(ACustomForm: TCustomForm);
    procedure MoveFormToZFront(ACustomForm: TCustomForm);
    // handler
    procedure AddHandlerFormAdded(OnFormAdded: TScreenFormEvent);
    procedure RemoveHandlerFormAdded(OnFormAdded: TScreenFormEvent);
    procedure AddHandlerRemoveForm(OnRemoveForm: TScreenFormEvent);
    procedure RemoveHandlerRemoveForm(OnRemoveForm: TScreenFormEvent);
    procedure AddHandlerActiveControlChanged(
                                   OnActiveControlChanged: TScreenControlEvent);
    procedure RemoveHandlerActiveControlChanged(
                                   OnActiveControlChanged: TScreenControlEvent);
    procedure AddHandlerActiveFormChanged(
                            OnActiveFormChanged: TScreenActiveFormChangedEvent);
    procedure RemoveHandlerActiveFormChanged(
                            OnActiveFormChanged: TScreenActiveFormChangedEvent);
  public
    property ActiveControl: TWinControl read FActiveControl;
    property ActiveCustomForm: TCustomForm read FActiveCustomForm;
    property ActiveForm: TForm read FActiveForm;
    property Cursor: TCursor read FCursor write SetCursor;
    property Cursors[Index: Integer]: HCURSOR read GetCursors write SetCursors;
    property CustomFormCount: Integer read GetCustomFormCount;
    property CustomForms[Index: Integer]: TCustomForm read GetCustomForms;
    property CustomFormsZOrdered[Index: Integer]: TCustomForm
                                                    read GetCustomFormsZOrdered;
    property FocusedForm: TCustomForm read FFocusedForm;
    property FormCount: Integer read GetFormCount;
    property Forms[Index: Integer]: TForm read GetForms;
    property Fonts : TStrings read GetFonts;
    property Height : Integer read Getheight;
    property HintFont : TFont read FHintFont;
    property Width : Integer read GetWidth;
    property OnActiveControlChange: TNotifyEvent read FOnActiveControlChange
                                                 write FOnActiveControlChange;
    property OnActiveFormChange: TNotifyEvent read FOnActiveFormChange
                                              write FOnActiveFormChange;
    property PixelsPerInch : integer read FPixelsPerInch;
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

  TApplicationFlag = (
    AppWaiting,
    AppIdleEndSent,
    AppHandlingException
    );
  TApplicationFlags = set of TApplicationFlag;

  TApplication = class(TComponent)
  private
    FFlags: TApplicationFlags;
    FHandle : THandle;
    //FHelpSystem : IHelpSystem;
    FHelpFile: string;
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
    FOnHelp: THelpEvent;
    FOnHint: TNotifyEvent;
    FOnIdle: TIdleEvent;
    FOnIdleHandler: TMethodList;
    FOnIdleEnd: TNotifyEvent;
    FOnIdleEndHandler: TMethodList;
    FOnKeyDownHandler: TMethodList;
    FOnShowHint: TShowHintEvent;
    FOnUserInput: TOnUserInputEvent;
    FOnUserInputHandler: TMethodList;
    FShowHint: Boolean;
    FTerminate : Boolean;
    FTitle : String;
    procedure DoOnIdleEnd;
    function GetCurrentHelpFile: string;
    function GetExename: String;
    function GetIconHandle: HICON;
    function GetTitle: string;
    procedure IconChanged(Sender: TObject);
    procedure Idle;
    function InvokeHelp(Command: Word; Data: Longint): Boolean;
    procedure MouseIdle(const CurrentControl: TControl);
    procedure SetHint(const AValue: string);
    procedure SetHintColor(const AValue: TColor);
    procedure SetIcon(AValue: TIcon);
    procedure SetShowHint(const AValue: Boolean);
    procedure StopHintTimer;
    function  ValidateHelpSystem: Boolean;
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
    function HelpCommand(Command: Integer; Data: Longint): Boolean;
    function HelpContext(Context: THelpContext): Boolean;
    function HelpJump(const JumpID: string): Boolean;
    function HelpKeyword(const Keyword: String): Boolean;
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
    procedure NotifyKeyDownHandler(Sender: TObject;
                                   var Key : Word; Shift : TShiftState);
    procedure AddOnIdleHandler(AnOnIdleHandler: TNotifyEvent);
    procedure RemoveOnIdleHandler(AnOnIdleHandler: TNotifyEvent);
    procedure AddOnIdleEndHandler(AnOnIdleEndHandler: TNotifyEvent);
    procedure RemoveOnIdleEndHandler(AnOnIdleEndHandler: TNotifyEvent);
    procedure AddOnUserInputHandler(AnOnUserInputHandler: TOnUserInputEvent);
    procedure RemoveOnUserInputHandler(AnOnUserInputHandler: TOnUserInputEvent);
    procedure AddOnKeyDownHandler(AnOnKeyDownHandler: TKeyEvent);
    procedure RemoveOnKeyDownHandler(AnOnKeyDownHandler: TKeyEvent);
    procedure DoBeforeMouseMessage(CurMouseControl: TControl);
  public
    property Exename: String read GetExeName;
    property Handle: THandle read FHandle;
    //property HelpSystem : IHelpSystem read FHelpSystem;
    property HelpFile: string read FHelpFile write FHelpFile;
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
    property OnHelp: THelpEvent read FOnHelp write FOnHelp;
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

{$IFNDEF VER1_0_8}
type
{ TDataModule }

  TDataModule = class(TComponent)
  private
    FDesignSize: TPoint;
    FDesignOffset: TPoint;
    FOnCreate: TNotifyEvent;
    FOnDestroy: TNotifyEvent;
    FOldCreateOrder: Boolean;
    procedure ReadHeight(Reader: TReader);
    procedure ReadHorizontalOffset(Reader: TReader);
    procedure ReadVerticalOffset(Reader: TReader);
    procedure ReadWidth(Reader: TReader);
    procedure WriteWidth(Writer: TWriter);
    procedure WriteHorizontalOffset(Writer: TWriter);
    procedure WriteVerticalOffset(Writer: TWriter);
    procedure WriteHeight(Writer: TWriter);
  protected
    procedure DoCreate; virtual;
    procedure DoDestroy; virtual;
    procedure DefineProperties(Filer: TFiler); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function HandleCreateException: Boolean; dynamic;
    procedure ReadState(Reader: TReader); override;
  public
    constructor Create(TheOwner: TComponent); override;
    constructor CreateNew(TheOwner: TComponent; CreateMode: Integer); virtual;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property DesignOffset: TPoint read FDesignOffset write FDesignOffset;
    property DesignSize: TPoint read FDesignSize write FDesignSize;
  published
    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property OldCreateOrder: Boolean read FOldCreateOrder write FOldCreateOrder;
  end;

const
  AddDataModule: procedure (DataModule: TDataModule) of object = nil;
  RemoveDataModule: procedure (DataModule: TDataModule) of object = nil;
  ApplicationHandleException: procedure (Sender: TObject) of object = nil;
  ApplicationShowException: procedure (E: Exception) of object = nil;
{$ENDIF}


function KeysToShiftState(Keys:Word): TShiftState;
function KeyDataToShiftState(KeyData: Longint): TShiftState;

type
  TFocusState = Pointer;

function SaveFocusState: TFocusState;
procedure RestoreFocusState(FocusState: TFocusState);

type
  TGetDesignerFormEvent =
    function(AComponent: TComponent): TCustomForm of object;

var
  OnGetDesignerForm: TGetDesignerFormEvent;

function GetParentForm(Control:TControl): TCustomForm;
function GetDesignerForm(AComponent: TComponent): TCustomForm;
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

procedure Register;

implementation


uses
  LResources, Math;

var
  FocusMessages: Boolean;
  FocusCount: Integer;

procedure Register;
begin
  RegisterComponents('Additional',[TScrollBox]);
end;

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
  Mess := Format(rsErrorOccurredInAtAddressFrame, [Sender.ClassName, #13#10,
    HexStr(Cardinal(Addr), 8), #13#10, HexStr(Cardinal(Frame), 8)]);
  if Application<>nil then
    Application.MessageBox(PChar(Mess), PChar(rsException), mb_IconError+mb_Ok)
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
      // The LCL has as default Visible=false. But for Delphi compatibility
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
      //writeln('Form Stream "',ClassType.ClassName,'" Signature=',copy(CompResource.Value,1,4));
      try
        if Instance.Name='' then begin

        end;
        Instance:=MemStream.ReadComponent(Instance);
      except
        on E: Exception do begin
          writeln(Format(rsFormStreamingError, [ClassType.ClassName, E.Message])
            );
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
  Form:=GetDesignerForm(AComponent);
  if Form<>nil then
    Result:=Form.Designer;
end;

function GetDesignerForm(AComponent: TComponent): TCustomForm;
var
  Owner: TComponent;
begin
  Result:=nil;
  if AComponent=nil then exit;
  if Assigned(OnGetDesignerForm) then
    Result:=OnGetDesignerForm(AComponent)
  else begin
    Owner:=AComponent.Owner;
    if Owner is TCustomForm then Result:=TCustomForm(Owner);
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


{$IFNDEF VER1_0_8}
{ TDataModule }

constructor TDataModule.Create(TheOwner: TComponent);
begin
  //GlobalNameSpace.BeginWrite;
  try
    CreateNew(TheOwner,0);
    if (ClassType <> TDataModule) and not (csDesigning in ComponentState) then
    begin
      if not InitResourceComponent(Self, TForm) then begin
        raise EResNotFound.CreateFmt(lisLCLResourceSNotFound, [ClassName]);
      end;
      if OldCreateOrder then DoCreate;
    end;
  finally
    //GlobalNameSpace.EndWrite;
  end;
end;

procedure TDataModule.AfterConstruction;
begin
  if not OldCreateOrder then DoCreate;
end;

constructor TDataModule.CreateNew(TheOwner: TComponent; CreateMode: Integer);
begin
  inherited Create(TheOwner);

  if Assigned(AddDataModule) and (CreateMode >= 0) then
    AddDataModule(Self);
end;

procedure TDataModule.BeforeDestruction;
begin
  //GlobalNameSpace.BeginWrite;
  Destroying;
  RemoveFixupReferences(Self, '');
  if not OldCreateOrder then DoDestroy;
end;

destructor TDataModule.Destroy;
begin
  if not (csDestroying in ComponentState) then
    ; //GlobalNameSpace.BeginWrite;
  try
    if OldCreateOrder then DoDestroy;
    if Assigned(RemoveDataModule) then
      RemoveDataModule(Self);
    inherited Destroy;
  finally
    //GlobalNameSpace.EndWrite;
  end;
end;

procedure TDataModule.DoCreate;
begin
  if Assigned(FOnCreate) then
  try
    FOnCreate(Self);
  except
    begin
      if not HandleCreateException then
        raise;
    end;
  end;
end;

procedure TDataModule.DoDestroy;
begin
  if Assigned(FOnDestroy) then
  try
    FOnDestroy(Self);
  except
    begin
      if Assigned(ApplicationHandleException) then
        ApplicationHandleException(Self);
    end;
  end;
end;

procedure TDataModule.DefineProperties(Filer: TFiler);
var
  Ancestor: TDataModule;

  function DoWriteWidth: Boolean;
  begin
    Result := True;
    if Ancestor <> nil then Result := FDesignSize.X <> Ancestor.FDesignSize.X;
  end;

  function DoWriteHorizontalOffset: Boolean;
  begin
    if Ancestor <> nil then
      Result := FDesignOffset.X <> Ancestor.FDesignOffset.X else
      Result := FDesignOffset.X <> 0;
  end;

  function DoWriteVerticalOffset: Boolean;
  begin
    if Ancestor <> nil then
      Result := FDesignOffset.Y <> Ancestor.FDesignOffset.Y else
      Result := FDesignOffset.Y <> 0;
  end;

  function DoWriteHeight: Boolean;
  begin
    Result := True;
    if Ancestor <> nil then Result := FDesignSize.Y <> Ancestor.FDesignSize.Y;
  end;

begin
  inherited DefineProperties(Filer);
  Ancestor := TDataModule(Filer.Ancestor);
  Filer.DefineProperty('Height', @ReadHeight, @WriteHeight, DoWriteHeight);
  Filer.DefineProperty('HorizontalOffset', @ReadHorizontalOffset,
                       @WriteHorizontalOffset, DoWriteHorizontalOffset);
  Filer.DefineProperty('VerticalOffset', @ReadVerticalOffset,
                       @WriteVerticalOffset, DoWriteVerticalOffset);
  Filer.DefineProperty('Width', @ReadWidth, @WriteWidth, DoWriteWidth);
end;

procedure TDataModule.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
  OwnedComponent: TComponent;
begin
  inherited GetChildren(Proc, Root);
  if Root = Self then begin
    for I := 0 to ComponentCount - 1 do
    begin
      OwnedComponent := Components[I];
      if not OwnedComponent.HasParent then Proc(OwnedComponent);
    end;
  end;
end;

function TDataModule.HandleCreateException: Boolean;
begin
  if Assigned(ApplicationHandleException) then
  begin
    ApplicationHandleException(Self);
    Result := True;
  end
  else
    Result := False;
end;

procedure TDataModule.ReadState(Reader: TReader);
begin
  FOldCreateOrder := false;
  inherited ReadState(Reader);
end;

procedure TDataModule.ReadWidth(Reader: TReader);
begin
  FDesignSize.X := Reader.ReadInteger;
end;

procedure TDataModule.ReadHorizontalOffset(Reader: TReader);
begin
  FDesignOffset.X := Reader.ReadInteger;
end;

procedure TDataModule.ReadVerticalOffset(Reader: TReader);
begin
  FDesignOffset.Y := Reader.ReadInteger;
end;

procedure TDataModule.ReadHeight(Reader: TReader);
begin
  FDesignSize.Y := Reader.ReadInteger;
end;

procedure TDataModule.WriteWidth(Writer: TWriter);
begin
  Writer.WriteInteger(FDesignSize.X);
end;

procedure TDataModule.WriteHorizontalOffset(Writer: TWriter);
begin
  Writer.WriteInteger(FDesignOffset.X);
end;

procedure TDataModule.WriteVerticalOffset(Writer: TWriter);
begin
  Writer.WriteInteger(FDesignOffset.Y);
end;

procedure TDataModule.WriteHeight(Writer: TWriter);
begin
  Writer.WriteInteger(FDesignSize.Y);
end;
{$ENDIF}

//==============================================================================

{$I controlscrollbar.inc}
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
  
  {$IFDEF VER1_0_8}
  RegisterInitComponentHandler(TComponent,@InitResourceComponent);
  {$ENDIF}
  // keep this comment, there is parser a bug in fpc 1.0.x

finalization
  //writeln('forms.pp - finalization section');
  LCLProc.OwnerFormDesignerModifiedProc:=nil;
  HintWindowClass:=THintWindow;
  FreeThenNil(Application);
  FreeThenNil(Screen);

end.


