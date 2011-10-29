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
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

unit Forms;

{$mode objfpc}{$H+}{$macro on}
{$I lcl_defines.inc}

interface

{$ifdef Trace}
  {$ASSERTIONS ON}
{$endif}

{$DEFINE HasDefaultValues}

uses
  Classes, SysUtils, Types, TypInfo, Math,
  AvgLvlTree, Maps, LCLVersion, LCLStrConsts, LCLType, LCLProc, LCLIntf,
  FileUtil, InterfaceBase, LResources, GraphType, Graphics, Menus, LMessages,
  CustomTimer, ActnList, ClipBrd, CustApp, HelpIntfs, LCLClasses, Controls,
  ImgList, Themes
  {$ifndef wince},gettext{$endif}// remove ifdefs when gettext is fixed and a new fpc is released
  ;

type
  // forward class declarations
  TIDesigner = class;
  TMonitor = class;
  TScrollingWinControl = class;

  TProcedure = procedure;
  TProcedureOfObject = procedure of object;

  // form position policies:
  TPosition = (
    poDesigned,        // use bounds from the designer (read from stream)
    poDefault,         // LCL decision (normally window manager decides)
    poDefaultPosOnly,  // designed size and LCL position
    poDefaultSizeOnly, // designed position and LCL size
    poScreenCenter,    // center form on screen (depends on DefaultMonitor)
    poDesktopCenter,   // center form on desktop (total of all screens)
    poMainFormCenter,  // center form on main form (depends on DefaultMonitor)
    poOwnerFormCenter  // center form on owner form (depends on DefaultMonitor)
    );

  TWindowState = (wsNormal, wsMinimized, wsMaximized, wsFullScreen);
  TCloseAction = (caNone, caHide, caFree, caMinimize);

  { Hint actions }

  TCustomHintAction = class(TCustomAction)
  published
    property Hint;
  end;


  { TControlScrollBar }

  TScrollBarKind = (sbHorizontal, sbVertical);
  TScrollBarInc = 1..32768;
  TScrollBarStyle = (ssRegular, ssFlat, ssHotTrack);
  EScrollBar = class(Exception) end;

  TControlScrollBar = class(TPersistent)
  private
    FAutoRange: Longint; // = FRange - ClientSize, >=0
    FIncrement: TScrollBarInc;
    FKind: TScrollBarKind;
    FPage: TScrollBarInc;
    FPosition: Integer;
    FRange: Integer;
    FSmooth: Boolean;
    FTracking: Boolean;
    FVisible: Boolean;
    FOldScrollInfo: TScrollInfo;
    FOldScrollInfoValid: Boolean;
  protected
    FControl: TWinControl;
    function ControlHandle: HWnd; virtual;
    function GetAutoScroll: boolean; virtual;
    function GetIncrement: TScrollBarInc; virtual;
    function GetPage: TScrollBarInc; virtual;
    function GetPosition: Integer; virtual;
    function GetRange: Integer; virtual;
    function GetSize: integer; virtual;
    function GetSmooth: Boolean; virtual;
    function HandleAllocated: boolean; virtual;
    function IsRangeStored: boolean; virtual;
    procedure AutoCalcRange; virtual;
    procedure ControlUpdateScrollBars; virtual;
    procedure InternalSetRange(const AValue: Integer); virtual;
    procedure ScrollHandler(var Message: TLMScroll);
    procedure SetAutoScroll(const AValue: Boolean); virtual;
    procedure SetIncrement(const AValue: TScrollBarInc); virtual;
    procedure SetPage(const AValue: TScrollBarInc); virtual;
    procedure SetPosition(const Value: Integer); virtual;
    procedure SetRange(const AValue: Integer); virtual;
    procedure SetSize(const AValue: integer); virtual;
    procedure SetSmooth(const AValue: Boolean); virtual;
    procedure SetTracking(const AValue: Boolean);
    procedure SetVisible(const AValue: Boolean); virtual;
    procedure UpdateScrollBar; virtual;
    procedure InvalidateScrollInfo;
  {$ifdef VerboseScrollingWinControl}
    function DebugCondition: Boolean;
  {$endif}
    function GetHorzScrollBar: TControlScrollBar; virtual;
    function GetVertScrollBar: TControlScrollBar; virtual;
  protected
    function ScrollBarShouldBeVisible: Boolean; virtual; // should the widget be made visible?
  public
    constructor Create(AControl: TWinControl; AKind: TScrollBarKind);
    procedure Assign(Source: TPersistent); override;
    function IsScrollBarVisible: Boolean; virtual; // returns current widget state
    function ScrollPos: Integer; virtual;
    property Kind: TScrollBarKind read FKind;
    function GetOtherScrollBar: TControlScrollBar;
    property Size: integer read GetSize write SetSize stored False;
    function ClientSize: integer; // return for vertical scrollbar the clientwidth
    function ClientSizeWithBar: integer; // return for vertical scrollbar the clientwidth with the bar, even if Visible=false
    function ClientSizeWithoutBar: integer; // return for vertical scrollbar the clientwidth without the bar, even if Visible=true
  published
    property Increment: TScrollBarInc read GetIncrement write SetIncrement default 8;
    property Page: TScrollBarInc read GetPage write SetPage default 80;
    property Smooth: Boolean read GetSmooth write SetSmooth default False;
    property Position: Integer read GetPosition write SetPosition default 0; // 0..Range-Page
    property Range: Integer read GetRange write SetRange stored IsRangeStored default 0; // >=Page
    property Tracking: Boolean read FTracking write SetTracking default False;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  { TScrollingWinControl }

  TScrollingWinControl = class(TCustomControl)
  private
    FHorzScrollBar: TControlScrollBar;
    FVertScrollBar: TControlScrollBar;
    FAutoScroll: Boolean;
    FIsUpdating: Boolean;
    procedure SetHorzScrollBar(Value: TControlScrollBar);
    procedure SetVertScrollBar(Value: TControlScrollBar);
  protected
    class procedure WSRegisterClass; override;
    procedure AlignControls(AControl: TControl; var ARect: TRect); override;
    procedure CreateWnd; override;
    function GetClientScrollOffset: TPoint; override;
    function GetLogicalClientRect: TRect; override;// logical size of client area
    procedure DoOnResize; override;
    procedure WMHScroll(var Message : TLMHScroll); message LM_HScroll;
    procedure WMVScroll(var Message : TLMVScroll); message LM_VScroll;
    function ComputeScrollbars: Boolean; virtual;
    procedure ScrollbarHandler(ScrollKind: TScrollBarKind;
                               OldPosition: Integer); virtual;
    procedure SetAutoScroll(Value: Boolean); virtual;
    procedure Loaded; override;
    property AutoScroll: Boolean read FAutoScroll write SetAutoScroll default False;// auto show/hide scrollbars
    procedure SetAutoSize(Value: Boolean); override;
  public
    constructor Create(TheOwner : TComponent); override;
    destructor Destroy; override;
    procedure UpdateScrollbars;
    class function GetControlClassDefaultSize: TSize; override;
    procedure ScrollBy(DeltaX, DeltaY: Integer); override;
  published
    property HorzScrollBar: TControlScrollBar read FHorzScrollBar write SetHorzScrollBar;
    property VertScrollBar: TControlScrollBar read FVertScrollBar write SetVertScrollBar;
  end;


  { TScrollBox }

  TScrollBox = class(TScrollingWinControl)
  protected
    class procedure WSRegisterClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property Anchors;
    property AutoSize;
    property AutoScroll default True;
    property BorderSpacing;
    property BiDiMode;
    property BorderStyle default bsSingle;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;

    property DragMode;
    property Enabled;
    property Color nodefault;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
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
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    property OnPaint;
  end;


  { TCustomFrame }

  TCustomFrame = class(TScrollingWinControl)
  private
    procedure AddActionList(ActionList: TCustomActionList);
    procedure RemoveActionList(ActionList: TCustomActionList);
    procedure ReadDesignLeft(Reader: TReader);
    procedure ReadDesignTop(Reader: TReader);
    procedure WriteDesignLeft(Writer: TWriter);
    procedure WriteDesignTop(Writer: TWriter);
  protected
    class procedure WSRegisterClass; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure SetParent(AParent: TWinControl); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure CalculatePreferredSize(var PreferredWidth,
           PreferredHeight: integer; WithThemeSpace: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    class function GetControlClassDefaultSize: TSize; override;
  end;

  TCustomFrameClass = class of TCustomFrame;


  { TFrame }

  TFrame = class(TCustomFrame)
  private
    FLCLVersion: string;
    function LCLVersionIsStored: boolean;
  public
    constructor Create(TheOwner: TComponent); override;
  published
    property Align;
    property Anchors;
    property AutoScroll;
    property AutoSize;
    property BiDiMode;
    property BorderSpacing;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Color nodefault;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property LCLVersion: string read FLCLVersion write FLCLVersion stored LCLVersionIsStored;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
  end;


  { TCustomForm }

  TBorderIcon = ( // Form title bar items
    biSystemMenu, // system menu
    biMinimize,   // minimize button
    biMaximize,   // maximize button
    biHelp        // help button
  );
  TBorderIcons = set of TBorderIcon;

  TDefaultMonitor = ( // monitor to place form
    dmDesktop,        // use full desktop
    dmPrimary,        // use primary monitor
    dmMainForm,       // use monitor of main form
    dmActiveForm      // use monitor of active form
  );

  TFormStateType = (
    fsCreating,          // initializing (form streaming)
    fsVisible,           // form should be shown
    fsShowing,           // form handling WM_SHOWWINDOW message
    fsModal,             // form is modal
    fsCreatedMDIChild,   // todo: not mplemented
    fsBorderStyleChanged,// border style is changed before window handle creation
    fsFormStyleChanged,  // form style is changed before window handle creation
    fsFirstShow,         // form is shown for the first time
    fsDisableAutoSize    // disable autosize
    );
  TFormState = set of TFormStateType;

  TModalResult = low(Integer)..high(Integer);
  PModalResult = ^TModalResult;

  TFormHandlerType = (
    fhtFirstShow,
    fhtClose,
    fhtCreate
    );

  TShowInTaskbar = (
    stDefault,  // use default rules for showing taskbar item
    stAlways,   // always show taskbar item for the form
    stNever     // never show taskbar item for the form
  );

  TPopupMode = (
    pmNone,     // default behavior - popup to mainform/taskbar window
    pmAuto,     // popup to active form and same as pmNone if no active form
    pmExplicit  // popup to PopupParent and same as pmNone if not exists
  );

  TCloseEvent = procedure(Sender: TObject; var CloseAction: TCloseAction) of object;
  TCloseQueryEvent = procedure(Sender : TObject; var CanClose : boolean) of object;
  TDropFilesEvent = procedure (Sender: TObject; const FileNames: Array of String) of object;
  THelpEvent = function(Command: Word; Data: PtrInt; var CallHelp: Boolean): Boolean of object;
  TShortCutEvent = procedure (var Msg: TLMKey; var Handled: Boolean) of object;

  TCustomForm = class(TScrollingWinControl)
  private
    FActive: Boolean;
    FActiveControl: TWinControl;
    FActiveDefaultControl: TControl;
    FAllowDropFiles: Boolean;
    FAlphaBlend: Boolean;
    FAlphaBlendValue: Byte;
    FBorderIcons: TBorderIcons;
    FDefaultControl: TControl;
    FCancelControl: TControl;
    FDefaultMonitor: TDefaultMonitor;
    FDesigner: TIDesigner;
    FFormStyle: TFormStyle;
    FFormUpdateCount: integer;
    FFormHandlers: array[TFormHandlerType] of TMethodList;
    FHelpFile: string;
    FIcon: TIcon;
    FPopupMode: TPopupMode;
    FPopupParent: TCustomForm;
    FSmallIconHandle: HICON;
    FBigIconHandle: HICON;
    FKeyPreview: Boolean;
    FMenu: TMainMenu;
    FModalResult: TModalResult;
    FLastFocusedControl: TWinControl;
    FOldBorderStyle: TFormBorderStyle;
    FOnActivate: TNotifyEvent;
    FOnClose: TCloseEvent;
    FOnCloseQuery: TCloseQueryEvent;
    FOnCreate: TNotifyEvent;
    FOnDeactivate: TNotifyEvent;
    FOnDestroy: TNotifyEvent;
    FOnDropFiles: TDropFilesEvent;
    FOnHelp: THelpEvent;
    FOnHide: TNotifyEvent;
    FOnShortcut: TShortCutEvent;
    FOnShow: TNotifyEvent;
    FOnWindowStateChange: TNotifyEvent;
    FPixelsPerInch: Longint;
    FPosition: TPosition;
    FRealizedShowInTaskBar: TShowInTaskbar;
    FRestoredLeft: integer;
    FRestoredTop: integer;
    FRestoredWidth: integer;
    FRestoredHeight: integer;
    FShowInTaskbar: TShowInTaskbar;
    FWindowState: TWindowState;
    function GetEffectiveShowInTaskBar: TShowInTaskBar;
    function GetMonitor: TMonitor;
    function GetPixelsPerInch: Longint;
    function GetRestoredLeft: integer;
    function GetRestoredTop: integer;
    function IsAutoScrollStored: Boolean;
    function IsForm: Boolean;
    function IsIconStored: Boolean;
    procedure CloseModal;
    procedure FreeIconHandles;
    procedure IconChanged(Sender: TObject);
    procedure SetActive(AValue: Boolean);
    procedure SetActiveControl(AWinControl: TWinControl);
    procedure SetActiveDefaultControl(AControl: TControl);
    procedure SetAllowDropFiles(const AValue: Boolean);
    procedure SetAlphaBlend(const AValue: Boolean);
    procedure SetAlphaBlendValue(const AValue: Byte);
    procedure SetBorderIcons(NewIcons: TBorderIcons);
    procedure SetFormBorderStyle(NewStyle: TFormBorderStyle);
    procedure SetCancelControl(NewControl: TControl);
    procedure SetDefaultControl(NewControl: TControl);
    procedure SetDesigner(Value : TIDesigner);
    procedure SetFormStyle(Value : TFormStyle);
    procedure SetIcon(AValue: TIcon);
    procedure SetMenu(Value: TMainMenu);
    procedure SetPopupMode(const AValue: TPopupMode);
    procedure SetPopupParent(const AValue: TCustomForm);
    procedure SetPosition(Value: TPosition);
    procedure SetShowInTaskbar(Value: TShowInTaskbar);
    procedure SetLastFocusedControl(AControl: TWinControl);
    procedure SetWindowFocus;
    procedure SetWindowState(Value : TWindowState);
    procedure WMActivate(var Message : TLMActivate); message LM_ACTIVATE;
    procedure WMCloseQuery(var message: TLMessage); message LM_CLOSEQUERY;
    procedure WMDeactivate(var Message : TLMActivate); message LM_DEACTIVATE;
    procedure WMHelp(var Message: TLMHelp); message LM_HELP;
    procedure WMShowWindow(var message: TLMShowWindow); message LM_SHOWWINDOW;
    procedure WMSize(var message: TLMSize); message LM_Size;
    procedure WMWindowPosChanged(var Message: TLMWindowPosChanged); message LM_WINDOWPOSCHANGED;
    procedure CMBiDiModeChanged(var Message: TLMessage); message CM_BIDIMODECHANGED;
    procedure CMParentBiDiModeChanged(var Message: TLMessage); message CM_PARENTBIDIMODECHANGED;
    procedure CMAppShowBtnGlyphChanged(var Message: TLMessage); message CM_APPSHOWBTNGLYPHCHANGED;
    procedure CMAppShowMenuGlyphChanged(var Message: TLMessage); message CM_APPSHOWMENUGLYPHCHANGED;
    procedure CMIconChanged(var Message: TLMessage); message CM_ICONCHANGED;
    procedure CMRelease(var Message: TLMessage); message CM_RELEASE;
    procedure CMActivate(var Message: TLMessage); message CM_ACTIVATE;
    procedure CMDeactivate(var Message: TLMessage); message CM_DEACTIVATE;
    procedure AddHandler(HandlerType: TFormHandlerType;
                         const Handler: TMethod; AsFirst: Boolean);
    procedure RemoveHandler(HandlerType: TFormHandlerType;
                            const Handler: TMethod);
    function FindDefaultForActiveControl: TWinControl;
    procedure UpdateMenu;
    procedure UpdateShowInTaskBar;
  protected
    FActionLists: TList; // keep this TList for Delphi compatibility
    FFormBorderStyle: TFormBorderStyle;
    FFormState: TFormState;
    class procedure WSRegisterClass; override;
    procedure CMShowingChanged(var Message: TLMessage); message CM_SHOWINGCHANGED;
    procedure DoShowWindow; virtual;
    procedure Activate; virtual;
    procedure ActiveChanged; virtual;
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure BeginFormUpdate;
    function ColorIsStored: boolean; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Deactivate; virtual;
    procedure DoClose(var CloseAction: TCloseAction); virtual;
    procedure DoCreate; virtual;
    procedure DoDestroy; virtual;
    procedure DoHide; virtual;
    procedure DoShow; virtual;
    procedure EndFormUpdate;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function HandleCreateException: Boolean; virtual;
    function HandleDestroyException: Boolean; virtual;
    function HandleShowHideException: Boolean; virtual;
    procedure InitializeWnd; override;
    procedure Loaded; override;
    procedure ChildHandlesCreated; override;
    procedure Notification(AComponent: TComponent; Operation : TOperation);override;
    procedure PaintWindow(dc : Hdc); override;
    procedure RequestAlign; override;
    procedure CalculatePreferredSize(var PreferredWidth,
           PreferredHeight: integer; WithThemeSpace: Boolean); override;
    procedure SetZOrder(Topmost: Boolean); override;
    procedure SetParent(NewParent: TWinControl); override;
    procedure MoveToDefaultPosition; virtual;
    procedure UpdateShowing; override;
    procedure SetVisible(Value: boolean); override;
    procedure AllAutoSized; override;
    procedure DoFirstShow; virtual;
    procedure UpdateWindowState;
    procedure VisibleChanging; override;
    procedure VisibleChanged; override;
    procedure WndProc(var TheMessage : TLMessage); override;
    function VisibleIsStored: boolean;
    procedure DoSendBoundsToInterface; override;
    procedure DoAutoSize; override;
    procedure SetAutoSize(Value: Boolean); override;
    procedure SetAutoScroll(Value: Boolean); override;
    procedure DoAddActionList(List: TCustomActionList);
    procedure DoRemoveActionList(List: TCustomActionList);
  protected
    // drag and dock
    procedure BeginAutoDrag; override;
    procedure DoDock(NewDockSite: TWinControl; var ARect: TRect); override;
    function GetFloating: Boolean; override;
    function GetDefaultDockCaption: String; override;
  protected
    // actions
    procedure CMActionExecute(var Message: TLMessage); message CM_ACTIONEXECUTE;
    procedure CMActionUpdate(var Message: TLMessage); message CM_ACTIONUPDATE;
    function DoExecuteAction(ExeAction: TBasicAction): boolean;
    function DoUpdateAction(TheAction: TBasicAction): boolean;
    procedure UpdateActions; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateNew(AOwner: TComponent; Num: Integer = 0); virtual;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    class function GetControlClassDefaultSize: TSize; override;

    function BigIconHandle: HICON;
    procedure Close;
    function CloseQuery: boolean; virtual;
    procedure DefocusControl(Control: TWinControl; Removing: Boolean);
    procedure DestroyWnd; override;
    procedure EnsureVisible(AMoveToTop: Boolean = True);
    procedure FocusControl(WinControl: TWinControl);
    function FormIsUpdating: boolean; override;
    function GetFormImage: TBitmap;
    function GetRolesForControl(AControl: TControl): TControlRolesForForm;
    procedure Hide;
    procedure IntfDropFiles(const FileNames: array of String);
    procedure IntfHelp(AComponent: TComponent);
    function IsShortcut(var Message: TLMKey): boolean; virtual;
    procedure MakeFullyVisible(AMonitor: TMonitor = nil; UseWorkarea: Boolean = False);
    function AutoSizeDelayedHandle: Boolean; override;
    procedure GetPreferredSize(var PreferredWidth, PreferredHeight: integer;
                               Raw: boolean = false;
                               WithThemeSpace: boolean = true); override;
    procedure Release;
    function CanFocus: Boolean; override;
    procedure SetFocus; override;
    function SetFocusedControl(Control: TWinControl): Boolean ; virtual;
    procedure SetRestoredBounds(ALeft, ATop, AWidth, AHeight: integer);
    procedure Show;
    function ShowModal: Integer; virtual;
    procedure ShowOnTop;
    function SmallIconHandle: HICON;
    function WantChildKey(Child : TControl;
                          var Message : TLMessage): Boolean; virtual;

    // handlers
    procedure RemoveAllHandlersOfObject(AnObject: TObject); override;
    procedure AddHandlerFirstShow(OnFirstShowHandler: TNotifyEvent;
                                  AsFirst: Boolean=true);
    procedure RemoveHandlerFirstShow(OnFirstShowHandler: TNotifyEvent);
    procedure AddHandlerClose(OnCloseHandler: TCloseEvent; AsFirst: Boolean=true);
    procedure RemoveHandlerClose(OnCloseHandler: TCloseEvent);
    procedure AddHandlerCreate(OnCreateHandler: TNotifyEvent; AsFirst: Boolean=true);
    procedure RemoveHandlerCreate(OnCreateHandler: TNotifyEvent);
  public
    // drag and dock
    procedure Dock(NewDockSite: TWinControl; ARect: TRect); override;
    procedure UpdateDockCaption(Exclude: TControl); override;
  public
    property Active: Boolean read FActive;
    property ActiveControl: TWinControl read FActiveControl write SetActiveControl;
    property ActiveDefaultControl: TControl read FActiveDefaultControl write SetActiveDefaultControl;
    property AllowDropFiles: Boolean read FAllowDropFiles write SetAllowDropFiles default False;
    property AlphaBlend: Boolean read FAlphaBlend write SetAlphaBlend;
    property AlphaBlendValue: Byte read FAlphaBlendValue write SetAlphaBlendValue;
    property AutoScroll stored IsAutoScrollStored;
    property BorderIcons: TBorderIcons read FBorderIcons write SetBorderIcons
      default [biSystemMenu, biMinimize, biMaximize];
    property BorderStyle: TFormBorderStyle
                      read FFormBorderStyle write SetFormBorderStyle default bsSizeable;
    property CancelControl: TControl read FCancelControl write SetCancelControl;
    property Caption stored IsForm;
    property Color default {$ifdef UseCLDefault}clDefault{$else}clBtnFace{$endif};
    property DefaultControl: TControl read FDefaultControl write SetDefaultControl;
    property DefaultMonitor: TDefaultMonitor read FDefaultMonitor
      write FDefaultMonitor default dmActiveForm;
    property Designer: TIDesigner read FDesigner write SetDesigner;
    property FormState: TFormState read FFormState;
    property FormStyle: TFormStyle read FFormStyle write SetFormStyle
                                   default fsNormal;
    property HelpFile: string read FHelpFile write FHelpFile;
    property Icon: TIcon read FIcon write SetIcon stored IsIconStored;
    property KeyPreview: Boolean read FKeyPreview write FKeyPreview default False;
    property Menu : TMainMenu read FMenu write SetMenu;
    property ModalResult : TModalResult read FModalResult write FModalResult;
    property Monitor: TMonitor read GetMonitor;
    property PopupMode: TPopupMode read FPopupMode write SetPopupMode default pmNone;
    property PopupParent: TCustomForm read FPopupParent write SetPopupParent;

    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    property OnClose: TCloseEvent read FOnClose write FOnClose stored IsForm;
    property OnCloseQuery : TCloseQueryEvent
                     read FOnCloseQuery write FOnCloseQuery stored IsForm;
    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
    property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property OnDropFiles: TDropFilesEvent read FOnDropFiles write FOnDropFiles;
    property OnHelp: THelpEvent read FOnHelp write FOnHelp;
    property OnHide: TNotifyEvent read FOnHide write FOnHide;
    property OnResize stored IsForm;
    property OnShortcut: TShortcutEvent read FOnShortcut write FOnShortcut;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property OnWindowStateChange: TNotifyEvent
                         read FOnWindowStateChange write FOnWindowStateChange;
    property ParentFont default False;
    property PixelsPerInch: Longint read GetPixelsPerInch write FPixelsPerInch stored False;
    property Position: TPosition read FPosition write SetPosition default poDesigned;
    property RestoredLeft: integer read GetRestoredLeft;
    property RestoredTop: integer read GetRestoredTop;
    property RestoredWidth: integer read FRestoredWidth;
    property RestoredHeight: integer read FRestoredHeight;
    property ShowInTaskBar: TShowInTaskbar read FShowInTaskbar write SetShowInTaskBar
                                    default stDefault;
    property Visible stored VisibleIsStored default false;
    property WindowState: TWindowState read FWindowState write SetWindowState
                                       default wsNormal;
  end;

  TCustomFormClass = class of TCustomForm;


  { TForm }

  TForm = class(TCustomForm)
  private
    FClientHandle: HWND;
    FLCLVersion: string;
    function LCLVersionIsStored: boolean;
  protected
    procedure CreateWnd; override;
    procedure Loaded; override;
  public
    constructor Create(TheOwner: TComponent); override;
    property ClientHandle: HWND read FClientHandle;
    property DockManager;
  published
    property Action;
    property ActiveControl;
    property Align;
    property AllowDropFiles;
    property AlphaBlend default False;
    property AlphaBlendValue default 255;
    property Anchors;
    property AutoScroll;
    property AutoSize;
    property BiDiMode;
    property BorderIcons;
    property BorderStyle;
    property BorderWidth;
    property Caption;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Color;
    property Constraints;
    property DefaultMonitor;
    property DockSite;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FormStyle;
    property HelpFile;
    property Icon;
    property KeyPreview;
    property Menu;
    property OnActivate;
    property OnChangeBounds;
    property OnClick;
    property OnClose;
    property OnCloseQuery;
    property OnContextPopup;
    property OnCreate;
    property OnDblClick;
    property OnDeactivate;
    property OnDestroy;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnDropFiles;
    property OnEndDock;
    property OnGetSiteInfo;
    property OnHelp;
    property OnHide;
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
    property OnPaint;
    property OnResize;
    property OnShortCut;
    property OnShow;
    property OnShowHint;
    property OnStartDock;
    property OnUnDock;
    property OnUTF8KeyPress;
    property OnWindowStateChange;
    property ParentBiDiMode;
    property ParentFont;
    property PixelsPerInch;
    property PopupMenu;
    property PopupMode;
    property PopupParent;
    property Position;
    property SessionProperties;
    property ShowHint;
    property ShowInTaskBar;
    property UseDockManager;
    property LCLVersion: string read FLCLVersion write FLCLVersion stored LCLVersionIsStored;
    property Visible;
    property WindowState;
  end;

  TFormClass = class of TForm;


  { TCustomDockForm }

  TCustomDockForm = class(TCustomForm)
  protected
    procedure DoAddDockClient(Client: TControl; const ARect: TRect); override;
    procedure DoRemoveDockClient(Client: TControl); override;
    procedure GetSiteInfo(Client: TControl; var InfluenceRect: TRect;
                          MousePos: TPoint; var CanDock: Boolean); override;
    procedure Loaded; override;
  public
    constructor Create(TheOwner: TComponent); override;
    property AutoScroll default False;
    property BorderStyle default bsSizeToolWin;
    property FormStyle default fsStayOnTop;
  published
    property PixelsPerInch;
  end;


  { THintWindow }

  THintWindow = class(TCustomForm)
  private
    FActivating: Boolean;
    FAlignment: TAlignment;
    FAutoHide: Boolean;
    FAutoHideTimer: TCustomTimer;
    FHideInterval: Integer;
    function GetDrawTextFlags: Cardinal;
    procedure SetAutoHide(Value : Boolean);
    procedure AutoHideHint(Sender : TObject);
    procedure SetHideInterval(Value : Integer);
  protected
    class procedure WSRegisterClass; override;
    procedure WMNCHitTest(var Message: TLMessage); message LM_NCHITTEST;
    procedure DoShowWindow; override;
    procedure UpdateRegion;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ActivateHint(ARect: TRect; const AHint: String); virtual;
    procedure ActivateHintData(ARect: TRect; const AHint: String;
                               AData: pointer); virtual;
    function CalcHintRect(MaxWidth: Integer; const AHint: String;
                          AData: Pointer): TRect; virtual;
    procedure InitializeWnd; override;
    procedure ReleaseHandle;
    procedure Paint; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: integer); override;
    class function GetControlClassDefaultSize: TSize; override;
  public
    property Alignment: TAlignment read FAlignment write FAlignment;
    property AutoHide: Boolean read FAutoHide write SetAutoHide;
    property BiDiMode;
    property HideInterval: Integer read FHideInterval write SetHideInterval;
  end;

  THintWindowClass = class of THintWindow;

  { TMonitor }

  TMonitor = class(TObject)
  private
    FHandle: HMONITOR;
    FMonitorNum: Integer;
    function GetInfo(out Info: TMonitorInfo): Boolean; {inline; fpc bug - compilation error with inline}
    function GetLeft: Integer;
    function GetHeight: Integer;
    function GetTop: Integer;
    function GetWidth: Integer;
    function GetBoundsRect: TRect;
    function GetWorkareaRect: TRect;
    function GetPrimary: Boolean;
  public
    property Handle: HMONITOR read FHandle;
    property MonitorNum: Integer read FMonitorNum;
    property Left: Integer read GetLeft;
    property Height: Integer read GetHeight;
    property Top: Integer read GetTop;
    property Width: Integer read GetWidth;
    property BoundsRect: TRect read GetBoundsRect;
    property WorkareaRect: TRect read GetWorkareaRect;
    property Primary: Boolean read GetPrimary;
  end;

  { TMonitorList }

  TMonitorList = class(TList)
  private
    function GetItem(AIndex: Integer): TMonitor;
    procedure SetItem(AIndex: Integer; const AValue: TMonitor);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    property Items[AIndex: Integer]: TMonitor read GetItem write SetItem; default;
  end;

  { TScreen }

  PCursorRec = ^TCursorRec;
  TCursorRec = record
    Next: PCursorRec;
    Index: Integer;
    Handle: HCURSOR;
  end;

  TScreenFormEvent = procedure(Sender: TObject; Form: TCustomForm) of object;
  TScreenControlEvent = procedure(Sender: TObject;
                                  LastControl: TControl) of object;

  TScreenNotification = (
    snFormAdded,
    snRemoveForm,
    snActiveControlChanged,
    snActiveFormChanged,
    snFormVisibleChanged
    );

  TMonitorDefaultTo = (mdNearest, mdNull, mdPrimary);

  { TScreen }

  TScreen = class(TLCLComponent)
  private
    FActiveControl: TWinControl;
    FActiveCustomForm: TCustomForm;
    FActiveForm: TForm;
    FCursor: TCursor;
    FCursorMap: TMap;
    FCustomForms: TFPList;
    FCustomFormsZOrdered: TFPList;
    FDefaultCursor: HCURSOR;
    FHintFont: TFont;
    FFocusedForm: TCustomForm;
    FFonts : TStrings;
    FFormList: TFPList;
    FDataModuleList: TFPList;
    FIconFont: TFont;
    FMenuFont: TFont;
    FScreenHandlers: array[TScreenNotification] of TMethodList;
    FLastActiveControl: TWinControl;
    FLastActiveCustomForm: TCustomForm;
    FMonitors: TMonitorList;
    FOnActiveControlChange: TNotifyEvent;
    FOnActiveFormChange: TNotifyEvent;
    FPixelsPerInch : integer;
    FSaveFocusedList: TFPList;
    FSystemFont: TFont;
    procedure DeleteCursor(AIndex: Integer);
    procedure DestroyCursors;
    procedure DestroyMonitors;
    function GetCursors(AIndex: Integer): HCURSOR;
    function GetCustomFormCount: Integer;
    function GetCustomFormZOrderCount: Integer;
    function GetCustomForms(Index: Integer): TCustomForm;
    function GetCustomFormsZOrdered(Index: Integer): TCustomForm;
    function GetDataModuleCount: Integer;
    function GetDataModules(AIndex: Integer): TDataModule;
    function GetDesktopLeft: Integer;
    function GetDesktopTop: Integer;
    function GetDesktopHeight: Integer;
    function GetDesktopWidth: Integer;
    function GetDesktopRect: TRect;
    function GetFonts : TStrings;
    function GetFormCount: Integer;
    function GetForms(IIndex: Integer): TForm;
    function GetHeight : Integer;
    function GetMonitor(Index: Integer): TMonitor;
    function GetMonitorCount: Integer;
    function GetPrimaryMonitor: TMonitor;
    function GetWidth : Integer;
    procedure AddForm(AForm: TCustomForm);
    procedure RemoveForm(AForm: TCustomForm);
    function SetFocusedForm(AForm: TCustomForm): Boolean;
    procedure SetCursor(const AValue: TCursor);
    procedure SetCursors(AIndex: Integer; const AValue: HCURSOR);
    procedure SetHintFont(const AValue: TFont);
    procedure SetIconFont(const AValue: TFont);
    procedure SetMenuFont(const AValue: TFont);
    procedure SetSystemFont(const AValue: TFont);
    procedure UpdateLastActive;
    procedure UpdateMonitors;
    procedure RestoreLastActive;
    procedure AddHandler(HandlerType: TScreenNotification;
                         const Handler: TMethod; AsFirst: Boolean);
    procedure RemoveHandler(HandlerType: TScreenNotification;
                            const Handler: TMethod);
    procedure DoAddDataModule(DataModule: TDataModule);
    procedure DoRemoveDataModule(DataModule: TDataModule);
    procedure NotifyScreenFormHandler(HandlerType: TScreenNotification;
                                      Form: TCustomForm);
    function GetWorkAreaHeight: Integer;
    function GetWorkAreaLeft: Integer;
    function GetWorkAreaRect: TRect;
    function GetWorkAreaTop: Integer;
    function GetWorkAreaWidth: Integer;
  protected
    function GetHintFont: TFont; virtual;
    function GetIconFont: TFont; virtual;
    function GetMenuFont: TFont; virtual;
    function GetSystemFont: TFont; virtual;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    function CustomFormIndex(AForm: TCustomForm): integer;
    function FormIndex(AForm: TForm): integer;
    function CustomFormZIndex(AForm: TCustomForm): integer;
    procedure MoveFormToFocusFront(ACustomForm: TCustomForm);
    procedure MoveFormToZFront(ACustomForm: TCustomForm);
    function GetCurrentModalForm: TCustomForm;
    function GetCurrentModalFormZIndex: Integer;
    function CustomFormBelongsToActiveGroup(AForm: TCustomForm): Boolean;
    function FindNonDesignerForm(const FormName: string): TCustomForm;
    function FindForm(const FormName: string): TCustomForm;
    function FindNonDesignerDataModule(const DataModuleName: string): TDataModule;
    function FindDataModule(const DataModuleName: string): TDataModule;
    procedure UpdateScreen;
    // handler
    procedure RemoveAllHandlersOfObject(AnObject: TObject); override;
    procedure AddHandlerFormAdded(OnFormAdded: TScreenFormEvent;
                                  AsFirst: Boolean=true);
    procedure RemoveHandlerFormAdded(OnFormAdded: TScreenFormEvent);
    procedure AddHandlerRemoveForm(OnRemoveForm: TScreenFormEvent;
                                   AsFirst: Boolean=true);
    procedure RemoveHandlerRemoveForm(OnRemoveForm: TScreenFormEvent);
    procedure AddHandlerActiveControlChanged(
                                    OnActiveControlChanged: TScreenControlEvent;
                                    AsFirst: Boolean=true);
    procedure RemoveHandlerActiveControlChanged(
                                   OnActiveControlChanged: TScreenControlEvent);
    procedure AddHandlerActiveFormChanged(OnActiveFormChanged: TScreenFormEvent;
                                          AsFirst: Boolean=true);
    procedure RemoveHandlerActiveFormChanged(OnActiveFormChanged: TScreenFormEvent);
    procedure AddHandlerFormVisibleChanged(OnFormVisibleChanged: TScreenFormEvent;
                                           AsFirst: Boolean=true);
    procedure RemoveHandlerFormVisibleChanged(OnFormVisibleChanged: TScreenFormEvent);

    function DisableForms(SkipForm: TCustomForm; DisabledList: TList = nil): TList;
    procedure EnableForms(var AFormList: TList);
    function MonitorFromPoint(const Point: TPoint;
      MonitorDefault: TMonitorDefaultTo = mdNearest): TMonitor;
    function MonitorFromRect(const Rect: TRect;
      MonitorDefault: TMonitorDefaultTo = mdNearest): TMonitor;
    function MonitorFromWindow(const Handle: THandle;
      MonitorDefault: TMonitorDefaultTo = mdNearest): TMonitor;
  public
    property ActiveControl: TWinControl read FActiveControl;
    property ActiveCustomForm: TCustomForm read FActiveCustomForm;
    property ActiveForm: TForm read FActiveForm;
    property Cursor: TCursor read FCursor write SetCursor;
    property Cursors[Index: Integer]: HCURSOR read GetCursors write SetCursors;
    property CustomFormCount: Integer read GetCustomFormCount;
    property CustomForms[Index: Integer]: TCustomForm read GetCustomForms;
    property CustomFormZOrderCount: Integer read GetCustomFormZOrderCount;
    property CustomFormsZOrdered[Index: Integer]: TCustomForm
                               read GetCustomFormsZOrdered; // lower index means on top
    property DesktopLeft: Integer read GetDesktopLeft;
    property DesktopTop: Integer read GetDesktopTop;

    property DesktopHeight: Integer read GetDesktopHeight;
    property DesktopWidth: Integer read GetDesktopWidth;
    property DesktopRect: TRect read GetDesktopRect;
    property FocusedForm: TCustomForm read FFocusedForm;
    property FormCount: Integer read GetFormCount;
    property Forms[Index: Integer]: TForm read GetForms;
    property DataModuleCount: Integer read GetDataModuleCount;
    property DataModules[Index: Integer]: TDataModule read GetDataModules;
    
    property HintFont: TFont read GetHintFont write SetHintFont;
    property IconFont: TFont read GetIconFont write SetIconFont;
    property MenuFont: TFont read GetMenuFont write SetMenuFont;
    property SystemFont: TFont read GetSystemFont write SetSystemFont;
    property Fonts: TStrings read GetFonts;

    property Height: Integer read Getheight;
    property MonitorCount: Integer read GetMonitorCount;
    property Monitors[Index: Integer]: TMonitor read GetMonitor;
    property PixelsPerInch: integer read FPixelsPerInch;
    property PrimaryMonitor: TMonitor read GetPrimaryMonitor;
    property Width: Integer read GetWidth;
    property WorkAreaRect: TRect read GetWorkAreaRect;
    property WorkAreaHeight: Integer read GetWorkAreaHeight;
    property WorkAreaLeft: Integer read GetWorkAreaLeft;
    property WorkAreaTop: Integer read GetWorkAreaTop;
    property WorkAreaWidth: Integer read GetWorkAreaWidth;
    property OnActiveControlChange: TNotifyEvent read FOnActiveControlChange
                                                 write FOnActiveControlChange;
    property OnActiveFormChange: TNotifyEvent read FOnActiveFormChange
                                              write FOnActiveFormChange;
  end;


  { TApplication }

  TQueryEndSessionEvent = procedure (var Cancel: Boolean) of object;
  TExceptionEvent = procedure (Sender: TObject; E: Exception) of object;
  TGetHandleEvent = procedure(var Handle: HWND) of object;
  TIdleEvent = procedure (Sender: TObject; var Done: Boolean) of object;
  TOnUserInputEvent = procedure(Sender: TObject; Msg: Cardinal) of object;
  TDataEvent = procedure (Data: PtrInt) of object;

  // application hint stuff
  TCMHintShow = record
    Msg: Cardinal;
    Reserved: WPARAM;
    HintInfo: PHintInfo;
    Result: LRESULT;
  end;

  TCMHintShowPause = record
    Msg: Cardinal;
    WasActive: Integer;
    Pause: PInteger;
    Result: LRESULT;
  end;

  TAppHintTimerType = (ahttNone, ahttShowHint, ahttHideHint, ahttReshowHint);

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
    AppHandlingException,
    AppNoExceptionMessages,
    AppActive, // application has focus
    AppDestroying,
    AppDoNotCallAsyncQueue,
    AppInitialized // initialization of application was done
    );
  TApplicationFlags = set of TApplicationFlag;

  TApplicationNavigationOption = (
    anoTabToSelectNext,
    anoReturnForDefaultControl,
    anoEscapeForCancelControl,
    anoF1ForHelp,
    anoArrowToSelectNextInParent
    );
  TApplicationNavigationOptions = set of TApplicationNavigationOption;

  TApplicationHandlerType = (
    ahtIdle,
    ahtIdleEnd,
    ahtKeyDownBefore, // before interface and LCL
    ahtKeyDownAfter,  // after interface and LCL
    ahtActivate,
    ahtDeactivate,
    ahtUserInput,
    ahtException,
    ahtEndSession,
    ahtQueryEndSession,
    ahtMinimize,
    ahtModalBegin,
    ahtModalEnd,
    ahtRestore,
    ahtDropFiles,
    ahtHelp,
    ahtHint,
    ahtShowHint,
    ahtGetMainFormHandle
    );

  PAsyncCallQueueItem = ^TAsyncCallQueueItem;
  TAsyncCallQueueItem = record
    Method: TDataEvent;
    Data: PtrInt;
    NextItem, PrevItem: PAsyncCallQueueItem;
  end;
  TAsyncCallQueue = record
    Top, Last: PAsyncCallQueueItem;
  end;
  TAsyncCallQueues = record
    CritSec: TRTLCriticalSection;
    Cur: TAsyncCallQueue; // currently processing
    Next: TAsyncCallQueue; // new calls added to this queue
  end;
  
  TApplicationType = (
    atDefault,
    atDesktop,
    atPDA,
    atKeyPadDevice
  );

  TApplicationShowGlyphs = (
    sbgAlways,  // show them always (default)
    sbgNever,   // show them never
    sbgSystem   // show them depending on OS
  );

  TTaskBarBehavior = (
    tbDefault,      // widgetset dependent
    tbMultiButton,  // show buttons for Forms with ShowTaskBar = stDefault
    tbSingleButton  // hide buttons for Forms with ShowTaskBar = stDefault
  );

  { TApplication }

  TApplication = class(TCustomApplication)
  private
    FApplicationHandlers: array[TApplicationHandlerType] of TMethodList;
    FApplicationType: TApplicationType;
    FCaptureExceptions: boolean;
    FComponentsToRelease: TFPList;
    FComponentsReleasing: TFPList;
    FCreatingForm: TForm;// currently created form (CreateForm), candidate for MainForm
    FFindGlobalComponentEnabled: boolean;
    FFlags: TApplicationFlags;
    FHint: string;
    FHintColor: TColor;
    FHintControl: TControl;
    FHintHidePause: Integer;
    FHintHidePausePerChar: Integer;
    FHintPause: Integer;
    FHintRect: TRect;
    FHintShortCuts: Boolean;
    FHintShortPause: Integer;
    FHintTimer: TCustomTimer;
    FHintTimerType: TAppHintTimerType;
    FHintWindow: THintWindow;
    FIcon: TIcon;
    FBigIconHandle: HICON;
    FMainFormOnTaskBar: Boolean;
    FModalLevel: Integer;
    FOnGetMainFormHandle: TGetHandleEvent;
    FOnModalBegin: TNotifyEvent;
    FOnModalEnd: TNotifyEvent;
    FShowButtonGlyphs: TApplicationShowGlyphs;
    FShowMenuGlyphs: TApplicationShowGlyphs;
    FSmallIconHandle: HICON;
    FIdleLockCount: Integer;
    FLastKeyDownSender: TWinControl;
    FLastKeyDownKey: Word;
    FLastKeyDownShift: TShiftState;
    FMainForm : TForm;
    FMouseControl: TControl;
    FNavigation: TApplicationNavigationOptions;
    FOldExceptProc: TExceptProc;
    FOldExitProc: Pointer;
    FOnActionExecute: TActionEvent;
    FOnActionUpdate: TActionEvent;
    FOnActivate: TNotifyEvent;
    FOnDeactivate: TNotifyEvent;
    FOnDestroy: TNotifyEvent;
    FOnDropFiles: TDropFilesEvent;
    FOnHelp: THelpEvent;
    FOnHint: TNotifyEvent;
    FOnIdle: TIdleEvent;
    FOnIdleEnd: TNotifyEvent;
    FOnEndSession: TNotifyEvent;
    FOnQueryEndSession: TQueryEndSessionEvent;
    FOnMinimize: TNotifyEvent;
    FOnRestore: TNotifyEvent;
    FOnShortcut: TShortcutEvent;
    FOnShowHint: TShowHintEvent;
    FOnUserInput: TOnUserInputEvent;
    FAsyncCall: TAsyncCallQueues;
    FShowHint: Boolean;
    FShowMainForm: Boolean;
    FLastMousePos: TPoint;
    FLastMouseControl: TControl;
    FLastMouseControlValid: Boolean;
    FBidiMode: TBiDiMode;
    FRestoreStayOnTop: TList;
    FTaskBarBehavior: TTaskBarBehavior;
    procedure DoOnIdleEnd;
    function GetActive: boolean;
    function GetCurrentHelpFile: string;
    function GetExename: String;
    function GetMainFormHandle: HWND;
    function GetTitle: string;
    procedure FreeIconHandles;
    procedure IconChanged(Sender: TObject);
    procedure SetBidiMode(const AValue: TBiDiMode);
    procedure SetFlags(const AValue: TApplicationFlags);
    procedure SetMainFormOnTaskBar(const AValue: Boolean);
    procedure SetNavigation(const AValue: TApplicationNavigationOptions);
    procedure SetShowButtonGlyphs(const AValue: TApplicationShowGlyphs);
    procedure SetShowMenuGlyphs(const AValue: TApplicationShowGlyphs);
    procedure SetTaskBarBehavior(const AValue: TTaskBarBehavior);
    procedure UpdateMouseControl(NewMouseControl: TControl);
    procedure UpdateMouseHint(CurrentControl: TControl);
    procedure SetCaptureExceptions(const AValue: boolean);
    procedure SetHint(const AValue: string);
    procedure SetHintColor(const AValue: TColor);
    procedure SetIcon(AValue: TIcon);
    procedure SetShowHint(const AValue: Boolean);
    procedure StopHintTimer;
    function  ValidateHelpSystem: Boolean;
    procedure WndProc(var AMessage : TLMessage);
    function DispatchAction(Msg: Longint; Action: TBasicAction): Boolean;
    procedure AddHandler(HandlerType: TApplicationHandlerType;
                         const Handler: TMethod; AsFirst: Boolean);
    procedure RemoveHandler(HandlerType: TApplicationHandlerType;
                            const Handler: TMethod);
    procedure RunLoop;
    procedure Activate;
    procedure Deactivate(Force: Boolean = False);
  protected
    function GetConsoleApplication: boolean; override;
    procedure NotifyIdleHandler(var Done: Boolean);
    procedure NotifyIdleEndHandler;
    procedure NotifyActivateHandler;
    procedure NotifyDeactivateHandler;
    procedure NotifyCustomForms(Msg: Word);
    function IsHintMsg(var Msg: TMsg): Boolean;
    function DoOnHelp(Command: Word; Data: PtrInt; var CallHelp: Boolean): Boolean; virtual;
    procedure DoOnMouseMove; virtual;
    procedure ShowHintWindow(const Info: THintInfoAtMouse);
    procedure OnHintTimer(Sender: TObject);
    procedure SetTitle(const AValue: String); override;
    procedure StartHintTimer(Interval: integer; TimerType: TAppHintTimerType);
    procedure UpdateVisible;
    procedure DoIdleActions;
    procedure MenuPopupHandler(Sender: TObject);
    procedure ProcessAsyncCallQueue;
    procedure FreeComponent(Data: PtrInt);
    procedure ReleaseComponents;
    procedure DoBeforeFinalization;
    function GetParams(Index: Integer): string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ActivateHint(CursorPos: TPoint; CheckHintControlChange: Boolean = False);
    function GetControlAtMouse: TControl;
    procedure ControlDestroyed(AControl: TControl);
    function BigIconHandle: HIcon;
    function SmallIconHandle: HIcon;
    procedure BringToFront;
    procedure CreateForm(InstanceClass: TComponentClass; out Reference);
    procedure UpdateMainForm(AForm: TForm);
    procedure QueueAsyncCall(const AMethod: TDataEvent; Data: PtrInt);
    procedure RemoveAsyncCalls(const AnObject: TObject);
    procedure ReleaseComponent(AComponent: TComponent);
    function ExecuteAction(ExeAction: TBasicAction): Boolean; override;
    function UpdateAction(TheAction: TBasicAction): Boolean; override;
    procedure HandleException(Sender: TObject); override;
    procedure HandleMessage;
    function HelpCommand(Command: Word; Data: PtrInt): Boolean;
    function HelpContext(Context: THelpContext): Boolean;
    function HelpKeyword(const Keyword: String): Boolean;
    procedure ShowHelpForObject(Sender: TObject);
    procedure RemoveStayOnTop(const ASystemTopAlso: Boolean = False);
    procedure RestoreStayOnTop(const ASystemTopAlso: Boolean = False);
    function IsWaiting: boolean;
    procedure CancelHint;
    procedure HideHint;
    procedure HintMouseMessage(Control : TControl; var AMessage: TLMessage);
    procedure Initialize; override;
    function MessageBox(Text, Caption: PChar; Flags: Longint): Integer;
    procedure Minimize;
    procedure ModalStarted;
    procedure ModalFinished;
    procedure Restore;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ProcessMessages;
    procedure Idle(Wait: Boolean);
    procedure Run;
    procedure ShowException(E: Exception); override;
    procedure Terminate; override;
    procedure DisableIdleHandler;
    procedure EnableIdleHandler;
    procedure NotifyUserInputHandler(Msg: Cardinal);
    procedure NotifyKeyDownBeforeHandler(Sender: TObject;
                                         var Key: Word; Shift: TShiftState);
    procedure NotifyKeyDownHandler(Sender: TObject;
                                   var Key: Word; Shift: TShiftState);
    procedure ControlKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ControlKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure AddOnIdleHandler(Handler: TIdleEvent; AsFirst: Boolean=true);
    procedure RemoveOnIdleHandler(Handler: TIdleEvent);
    procedure AddOnIdleEndHandler(Handler: TNotifyEvent; AsFirst: Boolean=true);
    procedure RemoveOnIdleEndHandler(Handler: TNotifyEvent);
    procedure AddOnUserInputHandler(Handler: TOnUserInputEvent;
                                    AsFirst: Boolean=true);
    procedure RemoveOnUserInputHandler(Handler: TOnUserInputEvent);
    procedure AddOnKeyDownBeforeHandler(Handler: TKeyEvent;
                                        AsFirst: Boolean=true);
    procedure RemoveOnKeyDownBeforeHandler(Handler: TKeyEvent);
    procedure AddOnKeyDownHandler(Handler: TKeyEvent; AsFirst: Boolean=true);
    procedure RemoveOnKeyDownHandler(Handler: TKeyEvent);
    procedure AddOnActivateHandler(Handler: TNotifyEvent; AsFirst: Boolean=true);
    procedure RemoveOnActivateHandler(Handler: TNotifyEvent);
    procedure AddOnDeactivateHandler(Handler: TNotifyEvent; AsFirst: Boolean=true);
    procedure RemoveOnDeactivateHandler(Handler: TNotifyEvent);
    procedure AddOnExceptionHandler(Handler: TExceptionEvent; AsFirst: Boolean=true);
    procedure RemoveOnExceptionHandler(Handler: TExceptionEvent);
    procedure AddOnEndSessionHandler(Handler: TNotifyEvent; AsFirst: Boolean=true);
    procedure RemoveOnEndSessionHandler(Handler: TNotifyEvent);
    procedure AddOnQueryEndSessionHandler(Handler: TQueryEndSessionEvent; AsFirst: Boolean=true);
    procedure RemoveOnQueryEndSessionHandler(Handler: TQueryEndSessionEvent);
    procedure AddOnMinimizeHandler(Handler: TNotifyEvent; AsFirst: Boolean=true);
    procedure RemoveOnMinimizeHandler(Handler: TNotifyEvent);
    procedure AddOnModalBeginHandler(Handler: TNotifyEvent; AsFirst: Boolean=true);
    procedure RemoveOnModalBeginHandler(Handler: TNotifyEvent);
    procedure AddOnModalEndHandler(Handler: TNotifyEvent; AsFirst: Boolean=true);
    procedure RemoveOnModalEndHandler(Handler: TNotifyEvent);
    procedure AddOnRestoreHandler(Handler: TNotifyEvent; AsFirst: Boolean=true);
    procedure RemoveOnRestoreHandler(Handler: TNotifyEvent);
    procedure AddOnDropFilesHandler(Handler: TDropFilesEvent; AsFirst: Boolean=true);
    procedure RemoveOnDropFilesHandler(Handler: TDropFilesEvent);
    procedure AddOnHelpHandler(Handler: THelpEvent; AsFirst: Boolean=true);
    procedure RemoveOnHelpHandler(Handler: THelpEvent);
    procedure AddOnHintHandler(Handler: TNotifyEvent; AsFirst: Boolean=true);
    procedure RemoveOnHintHandler(Handler: TNotifyEvent);
    procedure AddOnShowHintHandler(Handler: TShowHintEvent; AsFirst: Boolean=true);
    procedure RemoveOnShowHintHandler(Handler: TShowHintEvent);
    procedure AddOnGetMainFormHandleHandler(Handler: TGetHandleEvent; AsFirst: Boolean = True);
    procedure RemoveOnGetMainFormHandleHandler(Handler: TGetHandleEvent);
    procedure RemoveAllHandlersOfObject(AnObject: TObject); virtual;
    procedure DoBeforeMouseMessage(CurMouseControl: TControl);
    function  IsShortcut(var Message: TLMKey): boolean;
    procedure IntfQueryEndSession(var Cancel: Boolean);
    procedure IntfEndSession;
    procedure IntfAppActivate;
    procedure IntfAppDeactivate;
    procedure IntfAppMinimize;
    procedure IntfAppRestore;
    procedure IntfDropFiles(const FileNames: Array of String);
    procedure IntfThemeOptionChange(AThemeServices: TThemeServices; AOption: TThemeOption);

    function IsRTLLang(ALang: String): Boolean;
    function Direction(ALang: String): TBiDiMode;
  public
    procedure DoArrowKey(AControl: TWinControl; var Key: Word; Shift: TShiftState);
    procedure DoEscapeKey(AControl: TWinControl; var Key: Word; Shift: TShiftState);
    procedure DoReturnKey(AControl: TWinControl; var Key: Word; Shift: TShiftState);
    procedure DoTabKey(AControl: TWinControl; var Key: Word; Shift: TShiftState);

    property Active: boolean read GetActive;
    property ApplicationType : TApplicationType read FApplicationType write FApplicationType;
    property BidiMode: TBiDiMode read FBidiMode write SetBidiMode;
    property CaptureExceptions: boolean read FCaptureExceptions
                                        write SetCaptureExceptions;
    property FindGlobalComponentEnabled: boolean read FFindGlobalComponentEnabled
                                               write FFindGlobalComponentEnabled;
    property Flags: TApplicationFlags read FFlags write SetFlags;
    //property HelpSystem : IHelpSystem read FHelpSystem;
    property Hint: string read FHint write SetHint;
    property HintColor: TColor read FHintColor write SetHintColor;
    property HintHidePause: Integer read FHintHidePause write FHintHidePause;
    property HintHidePausePerChar: Integer read FHintHidePausePerChar write FHintHidePausePerChar;
    property HintPause: Integer read FHintPause write FHintPause;
    property HintShortCuts: Boolean read FHintShortCuts write FHintShortCuts;
    property HintShortPause: Integer read FHintShortPause write FHintShortPause;
    property Icon: TIcon read FIcon write SetIcon;
    property Navigation: TApplicationNavigationOptions read FNavigation write SetNavigation;
    property MainForm: TForm read FMainForm;
    property MainFormHandle: HWND read GetMainFormHandle;
    property MainFormOnTaskBar: Boolean read FMainFormOnTaskBar write SetMainFormOnTaskBar; platform;
    property ModalLevel: Integer read FModalLevel;
    property MouseControl: TControl read FMouseControl;
    property TaskBarBehavior: TTaskBarBehavior read FTaskBarBehavior write SetTaskBarBehavior;
    property OnActionExecute: TActionEvent read FOnActionExecute write FOnActionExecute;
    property OnActionUpdate: TActionEvent read FOnActionUpdate write FOnActionUpdate;
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
    property OnGetMainFormHandle: TGetHandleEvent read FOnGetMainFormHandle write FOnGetMainFormHandle;
    property OnIdle: TIdleEvent read FOnIdle write FOnIdle;
    property OnIdleEnd: TNotifyEvent read FOnIdleEnd write FOnIdleEnd;
    property OnEndSession: TNotifyEvent read FOnEndSession write FOnEndSession;
    property OnQueryEndSession: TQueryEndSessionEvent read FOnQueryEndSession write FOnQueryEndSession;
    property OnMinimize: TNotifyEvent read FOnMinimize write FOnMinimize;
    property OnModalBegin: TNotifyEvent read FOnModalBegin write FOnModalBegin;
    property OnModalEnd: TNotifyEvent read FOnModalEnd write FOnModalEnd;
    property OnRestore: TNotifyEvent read FOnRestore write FOnRestore;
    property OnDropFiles: TDropFilesEvent read FOnDropFiles write FOnDropFiles;
    property OnHelp: THelpEvent read FOnHelp write FOnHelp;
    property OnHint: TNotifyEvent read FOnHint write FOnHint;
    property OnShortcut: TShortcutEvent read FOnShortcut write FOnShortcut;
    property OnShowHint: TShowHintEvent read FOnShowHint write FOnShowHint;
    property OnUserInput: TOnUserInputEvent read FOnUserInput write FOnUserInput;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property ShowButtonGlyphs: TApplicationShowGlyphs read FShowButtonGlyphs write SetShowButtonGlyphs default sbgAlways;
    property ShowMenuGlyphs: TApplicationShowGlyphs read FShowMenuGlyphs write SetShowMenuGlyphs default sbgAlways;
    property ShowHint: Boolean read FShowHint write SetShowHint;
    property ShowMainForm: Boolean read FShowMainForm write FShowMainForm default True;
    property Title: String read GetTitle write SetTitle;
  end;

const
  DefHintColor = clInfoBk;           // default hint window color
  DefHintPause = 500;                // default pause before hint window displays (ms)
  DefHintShortPause = 0;             // default reshow pause
  DefHintHidePause = 5*DefHintPause; // default pause before hint is hidden (ms)
  DefHintHidePausePerChar = 200;     // added to DefHintHidePause (ms)

type
  { TApplicationProperties }

  TApplicationProperties = class(TLCLComponent)
  private
    FCaptureExceptions: boolean;
    FHelpFile: string;
    FHint: string;
    FHintColor: TColor;
    FHintHidePause: Integer;
    FHintPause: Integer;
    FHintShortCuts: Boolean;
    FHintShortPause: Integer;
    FOnDropFiles: TDropFilesEvent;
    FOnGetMainFormHandle: TGetHandleEvent;
    FOnModalBegin: TNotifyEvent;
    FOnModalEnd: TNotifyEvent;
    FShowButtonGlyphs: TApplicationShowGlyphs;
    FShowHint: Boolean;
    FShowMainForm: Boolean;
    FShowMenuGlyphs: TApplicationShowGlyphs;
    FTitle: String;

    FOnException: TExceptionEvent;
    FOnIdle: TIdleEvent;
    FOnIdleEnd: TNotifyEvent;
    FOnHelp: THelpEvent;
    FOnHint: TNotifyEvent;
    FOnShowHint: TShowHintEvent;
    FOnUserInput: TOnUserInputEvent;
    FOnEndSession : TNotifyEvent;
    FOnQueryEndSession : TQueryEndSessionEvent;
    FOnMinimize : TNotifyEvent;
    FOnRestore : TNotifyEvent;
  protected
    procedure SetCaptureExceptions(const AValue : boolean);
    procedure SetHelpFile(const AValue : string);
    procedure SetHint(const AValue : string);
    procedure SetHintColor(const AValue : TColor);
    procedure SetHintHidePause(const AValue : Integer);
    procedure SetHintPause(const AValue : Integer);
    procedure SetHintShortCuts(const AValue : Boolean);
    procedure SetHintShortPause(const AValue : Integer);
    procedure SetShowButtonGlyphs(const AValue: TApplicationShowGlyphs);
    procedure SetShowMenuGlyphs(const AValue: TApplicationShowGlyphs);
    procedure SetShowHint(const AValue : Boolean);
    procedure SetShowMainForm(const AValue: Boolean);
    procedure SetTitle(const AValue : String);

    procedure SetOnException(const AValue : TExceptionEvent);
    procedure SetOnGetMainFormHandle(const AValue: TGetHandleEvent);
    procedure SetOnIdle(const AValue : TIdleEvent);
    procedure SetOnIdleEnd(const AValue : TNotifyEvent);
    procedure SetOnEndSession(const AValue : TNotifyEvent);
    procedure SetOnQueryEndSession(const AValue : TQueryEndSessionEvent);
    procedure SetOnMinimize(const AValue : TNotifyEvent);
    procedure SetOnModalBegin(const AValue: TNotifyEvent);
    procedure SetOnModalEnd(const AValue: TNotifyEvent);
    procedure SetOnRestore(const AValue : TNotifyEvent);
    procedure SetOnDropFiles(const AValue: TDropFilesEvent);
    procedure SetOnHelp(const AValue : THelpEvent);
    procedure SetOnHint(const AValue : TNotifyEvent);
    procedure SetOnShowHint(const AValue : TShowHintEvent);
    procedure SetOnUserInput(const AValue : TOnUserInputEvent);
  public
    constructor Create(AOwner: TComponent); Override;
    destructor Destroy; override;
  published
    property CaptureExceptions: boolean read FCaptureExceptions
                                        write SetCaptureExceptions default True;
    property HelpFile: string read FHelpFile write SetHelpFile;
    property Hint: string read FHint write SetHint;
    property HintColor: TColor read FHintColor write SetHintColor default DefHintColor;
    property HintHidePause: Integer read FHintHidePause write SetHintHidePause default DefHintHidePause;
    property HintPause: Integer read FHintPause write SetHintPause default DefHintPause;
    property HintShortCuts: Boolean read FHintShortCuts write SetHintShortCuts default True;
    property HintShortPause: Integer read FHintShortPause write SetHintShortPause default DefHintShortPause;
    property ShowButtonGlyphs: TApplicationShowGlyphs read FShowButtonGlyphs write SetShowButtonGlyphs default sbgAlways;
    property ShowMenuGlyphs: TApplicationShowGlyphs read FShowMenuGlyphs write SetShowMenuGlyphs default sbgAlways;
    property ShowHint: Boolean read FShowHint write SetShowHint default True;
    property ShowMainForm: Boolean read FShowMainForm write SetShowMainForm default True;
    property Title: String read FTitle write SetTitle;

    property OnException: TExceptionEvent read FOnException write SetOnException;
    property OnGetMainFormHandle: TGetHandleEvent read FOnGetMainFormHandle write SetOnGetMainFormHandle;
    property OnIdle: TIdleEvent read FOnIdle write SetOnIdle;
    property OnIdleEnd: TNotifyEvent read FOnIdleEnd write SetOnIdleEnd;
    property OnEndSession : TNotifyEvent read FOnEndSession write SetOnEndSession;
    property OnQueryEndSession : TQueryEndSessionEvent read FOnQueryEndSession write SetOnQueryEndSession;
    property OnMinimize : TNotifyEvent read FOnMinimize write SetOnMinimize;
    property OnModalBegin: TNotifyEvent read FOnModalBegin write SetOnModalBegin;
    property OnModalEnd: TNotifyEvent read FOnModalEnd write SetOnModalEnd;
    property OnRestore : TNotifyEvent read FOnRestore write SetOnRestore;
    property OnDropFiles: TDropFilesEvent read FOnDropFiles write SetOnDropFiles;
    property OnHelp: THelpEvent read FOnHelp write SetOnHelp;
    property OnHint: TNotifyEvent read FOnHint write SetOnHint;
    property OnShowHint: TShowHintEvent read FOnShowHint write SetOnShowHint;
    property OnUserInput: TOnUserInputEvent read FOnUserInput write SetOnUserInput;
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
    procedure SelectOnlyThisComponent(AComponent: TComponent); virtual; abstract;
    function UniqueName(const BaseName: string): string; virtual; abstract;
  end;


  { TFormPropertyStorage - abstract base class }

  TFormPropertyStorage = class(TControlPropertyStorage)
  private
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

function KeysToShiftState(Keys: PtrUInt): TShiftState;
function KeyDataToShiftState(KeyData: PtrInt): TShiftState;
function ShiftStateToKeys(ShiftState: TShiftState): PtrUInt;

function WindowStateToStr(const State: TWindowState): string;
function StrToWindowState(const Name: string): TWindowState;
function dbgs(const State: TWindowState): string; overload;
function dbgs(const Action: TCloseAction): string; overload;
function dbgs(const Kind: TScrollBarKind): string; overload;

type
  TFocusState = Pointer;

function SaveFocusState: TFocusState;
procedure RestoreFocusState(FocusState: TFocusState);

type
  TGetDesignerFormEvent =
    function(APersistent: TPersistent): TCustomForm of object;

var
  OnGetDesignerForm: TGetDesignerFormEvent = nil;

function GetParentForm(Control:TControl): TCustomForm;
function GetFirstParentForm(Control:TControl): TCustomForm;
function GetDesignerForm(APersistent: TPersistent): TCustomForm;
function FindRootDesigner(APersistent: TPersistent): TIDesigner;

function IsAccel(VK: word; const Str: string): Boolean;
procedure NotifyApplicationUserInput(Msg: Cardinal);


function GetShortHint(const Hint: string): string;
function GetLongHint(const Hint: string): string;


var
  Application: TApplication = nil;
  Screen: TScreen = nil;
  ExceptionObject: TExceptObject;
  HintWindowClass: THintWindowClass = THintWindow;
  RequireDerivedFormResource: Boolean = False;

type
  TMessageBoxFunction =
    function(Text, Caption : PChar; Flags : Longint) : Integer;
var
  MessageBoxFunction: TMessageBoxFunction = nil;

const
  DefaultBorderIcons : array[TFormBorderStyle] of TBorderIcons =
    ([],                                        // bsNone
     [biSystemMenu, biMinimize],                // bsSingle
     [biSystemMenu, biMinimize, biMaximize],    // bsSizeable
     [biSystemMenu],                            // bsDialog
     [biSystemMenu, biMinimize],                // bsToolWindow
     [biSystemMenu, biMinimize, biMaximize]);   // bsSizeToolWin

procedure CreateWidgetset(AWidgetsetClass: TWidgetsetClass);
procedure FreeWidgetSet;

procedure Register;


implementation

{$ifdef WinCE}
  {$define extdecl := cdecl}
{$else}
  {$define extdecl := stdcall}
{$endif}

uses
  WSForms; // Widgetset uses circle is allowed

var
  HandlingException: Boolean = False;
  HaltingProgram: Boolean = False;
  LastFocusedControl: TWinControl = nil;

procedure Register;
begin
  RegisterComponents('Standard',[TFrame]);
  RegisterComponents('Additional',[TScrollBox, TApplicationProperties]);
end;

{------------------------------------------------------------------------------
  procedure NotifyApplicationUserInput;

 ------------------------------------------------------------------------------}
procedure NotifyApplicationUserInput(Msg: Cardinal);
begin
  if Application <> nil then
    Application.NotifyUserInputHandler(Msg);
end;


//------------------------------------------------------------------------------
procedure ExceptionOccurred(Sender: TObject; Addr:Pointer; FrameCount: Longint;
  Frames: PPointer);
Begin
  DebugLn('[FORMS.PP] ExceptionOccurred ');
  if HaltingProgram or HandlingException then Halt;
  HandlingException:=true;
  if Sender<>nil then
  begin
    DebugLn('  Sender=',Sender.ClassName);
    if Sender is Exception then
    begin
      DebugLn('  Exception=',Exception(Sender).Message);
      DumpExceptionBackTrace();
    end;
  end else
    DebugLn('  Sender=nil');
  if Application<>nil then
    Application.HandleException(Sender);
  HandlingException:=false;
end;

procedure BeforeFinalization;
// This is our ExitProc handler.
begin
  Application.DoBeforeFinalization;
end;

function SaveFocusState: TFocusState;
begin
  Result := LastFocusedControl;
end;

procedure RestoreFocusState(FocusState: TFocusState);
begin
  LastFocusedControl := TWinControl(FocusState);
end;

//------------------------------------------------------------------------------
function KeysToShiftState(Keys: PtrUInt): TShiftState;
begin
  Result := [];
  if Keys and MK_Shift <> 0 then Include(Result, ssShift);
  if Keys and MK_Control <> 0 then Include(Result, ssCtrl);
  if Keys and MK_LButton <> 0 then Include(Result, ssLeft);
  if Keys and MK_RButton <> 0 then Include(Result, ssRight);
  if Keys and MK_MButton <> 0 then Include(Result, ssMiddle);
  {$ifndef ver2_2_0}
  if Keys and MK_XBUTTON1 <> 0 then Include(Result, ssExtra1);
  if Keys and MK_XBUTTON2 <> 0 then Include(Result, ssExtra2);
  {$endif}
  if GetKeyState(VK_MENU) < 0 then Include(Result, ssAlt);
  if (GetKeyState(VK_LWIN) < 0) or (GetKeyState(VK_RWIN) < 0) then Include(Result, ssMeta);
end;

function KeyDataToShiftState(KeyData: PtrInt): TShiftState;
begin
  Result := MsgKeyDataToShiftState(KeyData);
end;

function ShiftStateToKeys(ShiftState: TShiftState): PtrUInt;
begin
  Result := 0;
  if ssShift  in ShiftState then Result := Result or MK_SHIFT;
  if ssCtrl   in ShiftState then Result := Result or MK_CONTROL;
  if ssLeft   in ShiftState then Result := Result or MK_LBUTTON;
  if ssRight  in ShiftState then Result := Result or MK_RBUTTON;
  if ssMiddle in ShiftState then Result := Result or MK_MBUTTON;
  {$ifndef ver2_2_0}
  if ssExtra1 in ShiftState then Result := Result or MK_XBUTTON1;
  if ssExtra2 in ShiftState then Result := Result or MK_XBUTTON2;
  {$endif}
end;

function WindowStateToStr(const State: TWindowState): string;
begin
  Result:=GetEnumName(TypeInfo(TWindowState),ord(State));
end;

function StrToWindowState(const Name: string): TWindowState;
begin
  Result:=TWindowState(GetEnumValueDef(TypeInfo(TWindowState),Name,
                                       ord(wsNormal)));
end;

function dbgs(const State: TWindowState): string; overload;
begin
  Result:=GetEnumName(TypeInfo(TWindowState),ord(State));
end;

function dbgs(const Action: TCloseAction): string; overload;
begin
  Result:=GetEnumName(TypeInfo(TCloseAction),ord(Action));
end;

function dbgs(const Kind: TScrollBarKind): string;
begin
  if Kind=sbVertical then
    Result:='sbVertical'
  else
    Result:='sbHorizontal';
end;

//------------------------------------------------------------------------------
function GetParentForm(Control: TControl): TCustomForm;
begin
  while Control.Parent <> nil do
    Control := Control.Parent;
  if Control is TCustomForm
  then Result := TCustomForm(Control)
  else Result := nil;
end;

//------------------------------------------------------------------------------
function IsAccel(VK: word; const Str: string): Boolean;
const
  AmpersandChar = '&';
var
  position: integer;
  ACaption, FoundChar: string;
begin
  ACaption := Str;
  Result := false;
  position := UTF8Pos(AmpersandChar, ACaption);
  // if AmpersandChar is on the last position then there is nothing to underscore, ignore this character
  while (position > 0) and (position < UTF8Length(ACaption)) do
  begin
    FoundChar := UTF8Copy(ACaption, position+1, 1);
    // two AmpersandChar characters together are not valid hot key
    if FoundChar <> AmpersandChar then begin
      Result := UTF8UpperCase(UTF16ToUTF8(WideString(WideChar(VK)))) = UTF8UpperCase(FoundChar);
      exit;
    end
    else begin
      UTF8Delete(ACaption, 1, position+1);
      position := UTF8Pos(AmpersandChar, ACaption);
    end;
  end;
end;

//==============================================================================

function FindRootDesigner(APersistent: TPersistent): TIDesigner;
var
  Form: TCustomForm;
begin
  Result:=nil;
  Form:=GetDesignerForm(APersistent);
  if Form<>nil then
    Result:=Form.Designer;
end;

function GetFirstParentForm(Control: TControl): TCustomForm;
begin
  while (Control<>nil) and (not (Control is TCustomForm)) do
    Control:=Control.Parent;
  Result:=TCustomForm(Control);
end;

function GetDesignerForm(APersistent: TPersistent): TCustomForm;
begin
  if APersistent = nil then Exit(nil);
  if Assigned(OnGetDesignerForm) then
    Result := OnGetDesignerForm(APersistent)
  else
  begin
    Result := nil;
    repeat
      if (APersistent is TComponent) then begin
        if TComponent(APersistent).Owner<>nil then
          APersistent:=TComponent(APersistent).Owner
        else
          exit;
      end else if APersistent is TCollection then begin
        if TCollection(APersistent).Owner<>nil then
          APersistent:=TCollection(APersistent).Owner
        else
          exit;
      end else if APersistent is TCollectionItem then begin
        if TCollectionItem(APersistent).Collection<>nil then
          APersistent:=TCollectionItem(APersistent).Collection
        else
          exit;
      end else
        exit;
    until false;
    if APersistent is TCustomForm then
      Result := TCustomForm(APersistent);
  end;
end;

function SendApplicationMsg(Msg: Cardinal; WParam: WParam; LParam: LParam): Longint;
var
  AMessage: TLMessage;
begin
  if Application<>nil then begin
    AMessage.Msg := Msg;
    AMessage.WParam := WParam;
    AMessage.LParam := LParam;
    { Can't simply use SendMessage, as the Application does not necessarily have a handle }
    Application.WndProc(AMessage);
    Result := AMessage.Result;
  end else
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

procedure CreateWidgetset(AWidgetsetClass: TWidgetsetClass);
begin
  //debugln('CreateWidgetset');
  CallInterfaceInitializationHandlers;
  WidgetSet := AWidgetsetClass.Create;
end;

procedure FreeWidgetSet;
begin
  //debugln('FreeWidgetSet');
  if Screen <> nil then
  begin
    Screen.DestroyCursors;
    Screen.DestroyMonitors;
  end;
  Application.Free;
  Application:=nil;
  FreeAllClipBoards;
  CallInterfaceFinalizationHandlers;
  WidgetSet.Free;
  WidgetSet:=nil;
end;


//==============================================================================

{$I controlscrollbar.inc}
{$I scrollingwincontrol.inc}
{$I scrollbox.inc}
{$I customframe.inc}
{$I customform.inc}
{$I customdockform.inc}
{$I monitor.inc}
{$I screen.inc}
{$I application.inc}
{$I applicationproperties.inc}
{$I hintwindow.inc}


//==============================================================================

procedure ImageDrawEvent(AImageList: TPersistent; ACanvas: TPersistent;
                     AX, AY, AIndex: Integer; ADrawEffect: TGraphicsDrawEffect);
var
  ImageList: TCustomImageList absolute AImageList;
  Canvas: TCanvas absolute ACanvas;
begin
  ImageList.Draw(Canvas,AX,AY,AIndex,ADrawEffect);
end;

initialization
  {$INCLUDE cursors.lrs}
  RegisterPropertyToSkip(TForm, 'OldCreateOrder', 'VCL compatibility property', '');
  RegisterPropertyToSkip(TForm, 'TextHeight', 'VCL compatibility property', '');
  RegisterPropertyToSkip(TForm, 'Scaled', 'VCL compatibility property', '');
  RegisterPropertyToSkip(TForm, 'TransparentColorValue', 'VCL compatibility property', '');
  LCLProc.OwnerFormDesignerModifiedProc:=@IfOwnerIsFormThenDesignerModified;
  ThemesImageDrawEvent:=@ImageDrawEvent;
  Screen:=TScreen.Create(nil);
  Application:=TApplication.Create(nil);

finalization
  //DebugLn('forms.pp - finalization section');
  LCLProc.OwnerFormDesignerModifiedProc:=nil;
  HintWindowClass:=nil;
  FreeThenNil(Application);
  FreeThenNil(Screen);

end.
