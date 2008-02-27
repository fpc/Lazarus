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
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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

{$DEFINE HasDefaultValues}

uses
  Classes, SysUtils, TypInfo, Math,
  AvgLvlTree, Maps, LCLStrConsts, LCLType, LCLProc, LCLIntf,
  InterfaceBase, LResources, GraphType, Graphics, Menus, LMessages, CustomTimer,
  ActnList, ClipBrd, CustApp, HelpIntfs, LCLClasses, Controls;

type
  TProcedure = procedure;
  TProcedureOfObject = procedure of object;

  // form position policies:
  TPosition = (
    poDesigned,        // use bounds from the designer (read from stream)
    poDefault,         // LCL decision (normally window manager decides)
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
    FAutoRange : Longint;
    FIncrement: TScrollBarInc;
    FKind: TScrollBarKind;
    FPage: TScrollBarInc;
    FPosition: Integer;
    FRange: Integer;
    FSmooth : Boolean;
    FVisible: Boolean;
    FOldScrollInfo: TScrollInfo;
    FOldScrollInfoValid: Boolean;
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
    procedure ScrollHandler(var Message: TLMScroll);
    procedure SetIncrement(const AValue: TScrollBarInc); virtual;
    procedure SetPage(const AValue: TScrollBarInc); virtual;
    procedure SetPosition(const Value: Integer); virtual;
    procedure SetRange(const Value: Integer); virtual;
    procedure SetSize(const AValue: integer); virtual;
    procedure SetSmooth(const Value: Boolean); virtual;
    procedure SetVisible(const Value: Boolean); virtual;
    procedure UpdateScrollBar; virtual;
    procedure InvalidateScrollInfo;
  {$ifdef VerboseScrollingWinControl}
    function DebugCondition: Boolean;
  {$endif}
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

  TScrollingWinControl = class(TCustomControl)
  private
    FHorzScrollBar: TControlScrollBar;
    FVertScrollBar: TControlScrollBar;
    FAutoScroll: Boolean;
    FIsUpdating: Boolean;
    procedure SetAutoScroll(Value: Boolean);
    procedure SetHorzScrollBar(Value: TControlScrollBar);
    procedure SetVertScrollBar(Value: TControlScrollBar);
    function StoreScrollBars : Boolean;
  protected
    procedure AlignControls(AControl: TControl; var ARect: TRect); override;
    procedure CreateWnd; override;
    function  GetClientScrollOffset: TPoint; override;
    function GetLogicalClientRect: TRect; override;// logical size of client area
    procedure DoOnResize; override;
    class function GetControlClassDefaultSize: TPoint; override;
    procedure WMHScroll(var Message : TLMHScroll); message LM_HScroll;
    procedure WMVScroll(var Message : TLMVScroll); message LM_VScroll;
    procedure ScrollBy(DeltaX, DeltaY: Integer);
    function ComputeScrollbars: Boolean; virtual;
    procedure ScrollbarHandler(ScrollKind: TScrollBarKind;
                               OldPosition: Integer); virtual;
    procedure Loaded; override;
  public
    constructor Create(TheOwner : TComponent); override;
    destructor Destroy; override;
    procedure UpdateScrollbars;
    function HasVisibleScrollbars: boolean; virtual;
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
    property AutoSize;
    //property AutoScroll;
    property BorderSpacing;
    property BiDiMode;
    property BorderStyle;
    property ChildSizing;
    property Constraints;
    //property DockSite;
    property DragCursor;
    property DragKind;

    property DragMode;
    property Enabled;
    property Color nodefault;
    property Ctl3D;
    property Font;
    property ParentBiDiMode;
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


  { TCustomFrame - not implemented yet }

  TCustomFrame = class(TScrollingWinControl)
  private
    procedure AddActionList(ActionList: TCustomActionList);
    procedure RemoveActionList(ActionList: TCustomActionList);
  protected
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure SetParent(AParent: TWinControl); override;
    class function GetControlClassDefaultSize: TPoint; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TCustomFrameClass = class of TCustomFrame;


  { TFrame - not implemented yet }

  TFrame = class(TCustomFrame)
  published
    property Align;
    property Anchors;
    property AutoScroll;
    property AutoSize;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Color nodefault;
    property Ctl3D;
    property Font;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
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
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;


  { TCustomForm }

  TIDesigner = class;

  TBorderIcon = (biSystemMenu, biMinimize, biMaximize, biHelp);
  TBorderIcons = set of TBorderIcon;

  TCloseEvent = procedure(Sender: TObject; var CloseAction: TCloseAction) of object;
  TCloseQueryEvent = procedure(Sender : TObject;
                               var CanClose : boolean) of object;
  THelpEvent = function(Command: Word; Data: Longint;
    var CallHelp: Boolean): Boolean of object;
    
  TDropFilesEvent = procedure (Sender: TObject; const FileNames: Array of String) of object;

  TShortCutEvent = procedure (var Msg: TLMKey; var Handled: Boolean) of object;

  TFormStateType = (
    fsCreating,  // initializing (form streaming)
    fsVisible,   // form should be shown
    fsShowing,
    fsModal,     // form is modal
    fsCreatedMDIChild,
    fsBorderStyleChanged,
    fsFormStyleChanged,
    fsFirstShow,  // form is shown for the first time
    fsDisableAutoSize
    );
  TFormState = set of TFormStateType;

  TModalResult = low(Integer)..high(Integer);
  PModalResult = ^TModalResult;

  TFormHandlerType = (
    fhtFirstShow,
    fhtClose,
    fhtCreate
    );

  TShowInTaskbar = (stDefault, stAlways, stNever);

  { TCustomForm }

  TCustomForm = class(TScrollingWinControl)
  private
    FActive: Boolean;
    FActiveControl: TWinControl;
    FActiveDefaultControl: TControl;
    FAllowDropFiles: Boolean;
    FBorderIcons: TBorderIcons;
    FDefaultControl: TControl;
    FCancelControl: TControl;
    FDesigner: TIDesigner;
    FFormState: TFormState;
    FFormStyle: TFormStyle;
    FFormUpdateCount: integer;
    FFormHandlers: array[TFormHandlerType] of TMethodList;
    FHelpFile: string;
    FIcon: TIcon;
    FKeyPreview: Boolean;
    FMenu: TMainMenu;
    FModalResult: TModalResult;
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
    FRestoredLeft: integer;
    FRestoredTop: integer;
    FRestoredWidth: integer;
    FRestoredHeight: integer;
    FShowInTaskbar: TShowInTaskbar;
    FWindowState: TWindowState;
    function GetPixelsPerInch: Longint;
    function GetRestoredLeft: integer;
    function GetRestoredTop: integer;
    function IsForm : Boolean;
    function IsHelpFileStored: boolean;
    function IsIconStored: Boolean;
    procedure CloseModal;
    procedure IconChanged(Sender: TObject);
    function IsKeyPreviewStored: boolean;
    procedure SetActive(AValue: Boolean);
    procedure SetActiveControl(AWinControl: TWinControl);
    procedure SetActiveDefaultControl(AControl: TControl);
    procedure SetAllowDropFiles(const AValue: Boolean);
    procedure SetBorderIcons(NewIcons: TBorderIcons);
    procedure SetFormBorderStyle(NewStyle: TFormBorderStyle);
    procedure SetCancelControl(NewControl: TControl);
    procedure SetDefaultControl(NewControl: TControl);
    procedure SetDesigner(Value : TIDesigner);
    procedure SetFormStyle(Value : TFormStyle);
    procedure SetIcon(AValue: TIcon);
    procedure SetMenu(Value : TMainMenu);
    procedure SetModalResult(const AValue: TModalResult);
    procedure SetPosition(Value : TPosition);
    procedure SetShowInTaskbar(Value: TShowInTaskbar);
    procedure SetWindowFocus;
    procedure SetWindowState(Value : TWindowState);
    procedure WMActivate(var Message : TLMActivate); message LM_ACTIVATE;
    procedure WMCloseQuery(var message: TLMessage); message LM_CLOSEQUERY;
    procedure WMDeactivate(var Message : TLMActivate); message LM_DEACTIVATE;
    procedure WMPaint(var message: TLMPaint); message LM_PAINT;
    procedure WMShowWindow(var message: TLMShowWindow); message LM_SHOWWINDOW;
    procedure WMSize(var message: TLMSize); message LM_Size;
    procedure CMBiDiModeChanged(var Message: TLMessage); message CM_BIDIMODECHANGED;
    procedure AddHandler(HandlerType: TFormHandlerType;
                         const Handler: TMethod; AsLast: Boolean);
    procedure RemoveHandler(HandlerType: TFormHandlerType;
                            const Handler: TMethod);
    function FindDefaultForActiveControl: TWinControl;
  protected
    FFormBorderStyle: TFormBorderStyle;
    FActionLists: TList;
    procedure CMShowingChanged(var Message: TLMessage); message CM_SHOWINGCHANGED;
    procedure Activate; dynamic;
    procedure ActiveChanged; dynamic;
    procedure BeginFormUpdate;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Deactivate;dynamic;
    procedure DestroyWnd; override;
    procedure DoClose(var CloseAction: TCloseAction); dynamic;
    procedure DoCreate; virtual;
    procedure DoDestroy; virtual;
    procedure DoHide; dynamic;
    procedure DoShow; dynamic;
    procedure EndFormUpdate;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure Loaded; override;
    procedure ChildHandlesCreated; override;
    procedure Notification(AComponent: TComponent; Operation : TOperation);override;
    procedure PaintWindow(dc : Hdc); override;
    procedure RequestAlign; override;
    procedure SetZOrder(Topmost: Boolean); override;
    procedure SetParent(NewParent: TWinControl); override;
    procedure UpdateShowing; override;
    procedure SetVisible(Value: boolean); override;
    procedure DoFirstShow; virtual;
    procedure UpdateWindowState;
    procedure VisibleChanging; override;
    procedure WndProc(var TheMessage : TLMessage); override;
    function FormIsUpdating: boolean; override;
    function VisibleIsStored: boolean;
    function ColorIsStored: boolean; override;
    procedure DoSendBoundsToInterface; override;
    procedure DoAutoSize; override;
    procedure SetAutoSize(const Value: Boolean); override;
    class function GetControlClassDefaultSize: TPoint; override;
  protected
    // drag and dock
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
    constructor CreateNew(AOwner: TComponent; Num : Integer{=0}); virtual;
    procedure BeforeDestruction; override;
    function GetIconHandle: HICON;
    destructor Destroy; override;
    procedure Close;
    function CloseQuery: boolean; virtual;
    procedure Release;
    procedure Hide;
    procedure Show;
    procedure ShowOnTop;
    procedure EnsureVisible(AMoveToTop: boolean = true);
    function NeedParentForAutoSize: Boolean; override;
    function WantChildKey(Child : TControl;
                          var Message : TLMessage): Boolean; virtual;
    procedure DefocusControl(Control: TWinControl; Removing: Boolean);
    procedure SetFocus; override;
    function SetFocusedControl(Control: TWinControl): Boolean ; Virtual;
    procedure FocusControl(WinControl: TWinControl);
    function ShowModal: Integer; virtual;
    procedure SetRestoredBounds(ALeft, ATop, AWidth, AHeight: integer);
    function GetRolesForControl(AControl: TControl): TControlRolesForForm;
    procedure RemoveAllHandlersOfObject(AnObject: TObject); override;
    procedure AddHandlerFirstShow(OnFirstShowHandler: TNotifyEvent;
                                  AsLast: Boolean=true);
    procedure RemoveHandlerFirstShow(OnFirstShowHandler: TNotifyEvent);
    procedure AddHandlerClose(OnCloseHandler: TCloseEvent; AsLast: Boolean=true);
    procedure RemoveHandlerClose(OnCloseHandler: TCloseEvent);
    procedure AddHandlerCreate(OnCreateHandler: TNotifyEvent; AsLast: Boolean=true);
    procedure RemoveHandlerCreate(OnCreateHandler: TNotifyEvent);
    function  IsShortcut(var Message: TLMKey): boolean; virtual;
    procedure IntfDropFiles(const FileNames: Array of String);
    procedure IntfHelp(AComponent: TComponent);
  public
    // drag and dock
    procedure Dock(NewDockSite: TWinControl; ARect: TRect); override;
  public
    property Active: Boolean read FActive;
    property ActiveControl: TWinControl read FActiveControl write SetActiveControl;
    property ActiveDefaultControl: TControl read FActiveDefaultControl write SetActiveDefaultControl;
    property AllowDropFiles: Boolean read FAllowDropFiles write SetAllowDropFiles default False;
    property BorderIcons: TBorderIcons read FBorderIcons write SetBorderIcons
      default [biSystemMenu, biMinimize, biMaximize];
    property BorderStyle: TFormBorderStyle
                      read FFormBorderStyle write SetFormBorderStyle default bsSizeable;
    property CancelControl: TControl read FCancelControl write SetCancelControl;
    property Caption stored IsForm;
    property Color default clBtnFace;
    property DefaultControl: TControl read FDefaultControl write SetDefaultControl;
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
    property OnDropFiles: TDropFilesEvent read FOnDropFiles write FOnDropFiles;
    property OnHelp: THelpEvent read FOnHelp write FOnHelp;
    property OnHide: TNotifyEvent read FOnHide write FOnHide;
    property OnResize stored IsForm;
    property OnShortcut: TShortcutEvent read FOnShortcut write FOnShortcut;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property OnWindowStateChange: TNotifyEvent
                         read fOnWindowStateChange write fOnWindowStateChange;
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
  protected
    procedure CreateWnd; override;
  public
    property ClientHandle: HWND read FClientHandle;
    property DockManager;
  published
    property Action;
    property ActiveControl;
    property Align;
    property AllowDropFiles;
    property AutoSize;
    property BiDiMode;
    property BorderIcons;
    property BorderStyle;
    property Caption;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Color;
    property Constraints;
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
    property OnMouseMove;
    property OnMouseUp;
    property OnPaint;
    property OnResize;
    property OnShortCut;
    property OnShow;
    property OnStartDock;
    property OnUnDock;
    property OnUTF8KeyPress;
    property OnWindowStateChange;
    property ParentBiDiMode;
    property ParentFont;
    property PixelsPerInch;
    property PopupMenu;
    property Position;
    property SessionProperties;
    property ShowHint;
    property ShowInTaskBar;
    property UseDockManager;
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
    FAutoHide : Boolean;
    FAutoHideTimer : TComponent;
    FHideInterval : Integer;
    procedure SetAutoHide(Value : Boolean);
    procedure AutoHideHint(Sender : TObject);
    procedure SetHideInterval(Value : Integer);
  protected
    procedure Paint; override;
    class function GetControlClassDefaultSize: TPoint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ActivateHint(ARect: TRect; const AHint: String); virtual;
    procedure ActivateHintData(ARect: TRect; const AHint: String;
                               AData: pointer); virtual;
    function CalcHintRect(MaxWidth: Integer; const AHint: String;
                          AData: Pointer): TRect; virtual;
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

  { TScreen }

  TScreen = class(TLCLComponent)
  private
    FActiveControl: TWinControl;
    FActiveCustomForm: TCustomForm;
    FActiveForm: TForm;
    FCursor: TCursor;
    FCursorCount: integer;
    FCursorMap: TMap;
    FCustomForms: TList;
    FCustomFormsZOrdered: TList;
    FDefaultCursor: HCURSOR;
    FHintFont: TFont;
    FFocusedForm: TCustomForm;
    FFonts : TStrings;
    FFormList: TList;
    FScreenHandlers: array[TScreenNotification] of TMethodList;
    FLastActiveControl: TWinControl;
    FLastActiveCustomForm: TCustomForm;
    FOnActiveControlChange: TNotifyEvent;
    FOnActiveFormChange: TNotifyEvent;
    FPixelsPerInch : integer;
    FSaveFocusedList: TList;
    procedure DeleteCursor(AIndex: Integer);
    procedure DestroyCursors;
    function GetCursors(AIndex: Integer): HCURSOR;
    function GetCustomFormCount: Integer;
    function GetCustomFormZOrderCount: Integer;
    function GetCustomForms(Index: Integer): TCustomForm;
    function GetCustomFormsZOrdered(Index: Integer): TCustomForm;
    function GetDesktopHeight: Integer;
    function GetDesktopWidth: Integer;
    function GetFonts : TStrings;
    function GetFormCount: Integer;
    function GetForms(IIndex: Integer): TForm;
    function GetHeight : Integer;
    function GetWidth : Integer;
    procedure AddForm(AForm: TCustomForm);
    procedure RemoveForm(AForm: TCustomForm);
    procedure SetCursor(const AValue: TCursor);
    procedure SetCursors(AIndex: Integer; const AValue: HCURSOR);
    procedure UpdateLastActive;
    procedure AddHandler(HandlerType: TScreenNotification;
                         const Handler: TMethod; AsLast: Boolean);
    procedure RemoveHandler(HandlerType: TScreenNotification;
                            const Handler: TMethod);
  protected
    function GetHintFont: TFont; virtual;
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
    function FindForm(const FormName: string): TCustomForm;
    procedure UpdateScreen;
    // handler
    procedure AddHandlerFormAdded(OnFormAdded: TScreenFormEvent;
                                  AsLast: Boolean=true);
    procedure RemoveHandlerFormAdded(OnFormAdded: TScreenFormEvent);
    procedure AddHandlerRemoveForm(OnRemoveForm: TScreenFormEvent;
                                   AsLast: Boolean=true);
    procedure RemoveHandlerRemoveForm(OnRemoveForm: TScreenFormEvent);
    procedure AddHandlerActiveControlChanged(
                                    OnActiveControlChanged: TScreenControlEvent;
                                    AsLast: Boolean=true);
    procedure RemoveHandlerActiveControlChanged(
                                   OnActiveControlChanged: TScreenControlEvent);
    procedure AddHandlerActiveFormChanged(
                             OnActiveFormChanged: TScreenActiveFormChangedEvent;
                             AsLast: Boolean=true);
    procedure RemoveHandlerActiveFormChanged(
                            OnActiveFormChanged: TScreenActiveFormChangedEvent);
    procedure RemoveAllHandlersOfObject(AnObject: TObject); override;
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
                                                    read GetCustomFormsZOrdered;
    property DesktopHeight: Integer read GetDesktopHeight;
    property DesktopWidth: Integer read GetDesktopWidth;
    property FocusedForm: TCustomForm read FFocusedForm;
    property FormCount: Integer read GetFormCount;
    property Forms[Index: Integer]: TForm read GetForms;
    property Fonts : TStrings read GetFonts;
    property Height : Integer read Getheight;
    property HintFont : TFont read GetHintFont;
    property Width : Integer read GetWidth;
    property OnActiveControlChange: TNotifyEvent read FOnActiveControlChange
                                                 write FOnActiveControlChange;
    property OnActiveFormChange: TNotifyEvent read FOnActiveFormChange
                                              write FOnActiveFormChange;
    property PixelsPerInch: integer read FPixelsPerInch;
  end;


  { TApplication }

  TQueryEndSessionEvent = procedure (var Cancel : Boolean) of object;
  TExceptionEvent = procedure (Sender: TObject; E: Exception) of object;
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
    AppDoNotCallAsyncQueue
    );
  TApplicationFlags = set of TApplicationFlag;

  TApplicationNavigationOption = (
    anoTabToSelectNext,
    anoReturnForDefaultControl,
    anoEscapeForCancelControl,
    anoF1ForHelp
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
    ahtRestore,
    ahtDropFiles,
    ahtHelp,
    ahtHint,
    ahtShowHint
    );

  PAsyncCallQueueItem = ^TAsyncCallQueueItem;
  TAsyncCallQueueItem = record
    Method: TDataEvent;
    Data: PtrInt;
    NextItem: PAsyncCallQueueItem;
  end;
  
  TApplicationType = (atDefault,atDesktop,atHandheld,atPDA,atSmartphone);

  { TApplication }

  TApplication = class(TCustomApplication)
  private
    FApplicationHandlers: array[TApplicationHandlerType] of TMethodList;
    FApplicationType: TApplicationType;
    FCaptureExceptions: boolean;
    FComponentsToRelease: TAvgLvlTree;
    FCreatingForm: TForm;// currently created form (CreateForm), candidate for MainForm
    FFindGlobalComponentEnabled: boolean;
    FFlags: TApplicationFlags;
    FHint: string;
    FHintColor: TColor;
    FHintControl: TControl;
    FHintHidePause: Integer;
    FHintHidePausePerChar: Integer;
    FHintPause: Integer;
    FHintShortCuts: Boolean;
    FHintShortPause: Integer;
    FHintTimer: TCustomTimer;
    FHintTimerType: TAppHintTimerType;
    FHintWindow: THintWindow;
    FIcon: TIcon;
    FIdleLockCount: Integer;
    FFormList: TList;
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
    FOnEndSession : TNotifyEvent;
    FOnQueryEndSession : TQueryEndSessionEvent;
    FOnMinimize: TNotifyEvent;
    FOnRestore: TNotifyEvent;
    FOnShortcut: TShortcutEvent;
    FOnShowHint: TShowHintEvent;
    FOnUserInput: TOnUserInputEvent;
    FAsyncCallQueue: PAsyncCallQueueItem;
    FAsyncCallQueueLast: PAsyncCallQueueItem;
    FShowHint: Boolean;
    FShowMainForm: Boolean;
    FLastMousePos: TPoint;
    FLastMouseControl: TControl;
    FLastMouseControlValid: Boolean;
    procedure DoOnIdleEnd;
    function GetActive: boolean;
    function GetCurrentHelpFile: string;
    function GetExename: String;
    function GetIconHandle: HICON;
    function GetTitle: string;
    procedure IconChanged(Sender: TObject);
    function InvokeHelp(Command: Word; Data: Longint): Boolean;
    function GetControlAtMouse: TControl;
    procedure SetFlags(const AValue: TApplicationFlags);
    procedure SetNavigation(const AValue: TApplicationNavigationOptions);
    procedure UpdateMouseControl(NewMouseControl: TControl);
    procedure MouseIdle(const CurrentControl: TControl);
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
                         const Handler: TMethod; AsLast: Boolean);
    procedure RemoveHandler(HandlerType: TApplicationHandlerType;
                            const Handler: TMethod);
    procedure RunLoop;
    procedure Activate;
    procedure Deactivate;
  protected
    function GetConsoleApplication: boolean; override;
    procedure NotifyIdleHandler(var Done: Boolean);
    procedure NotifyIdleEndHandler;
    procedure NotifyActivateHandler;
    procedure NotifyDeactivateHandler;
    function IsHintMsg(var Msg: TMsg): Boolean;
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
    procedure DoBeforeFinalization;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ControlDestroyed(AControl: TControl);
    procedure BringToFront;
    procedure CreateForm(InstanceClass: TComponentClass; out Reference);
    procedure UpdateMainForm(AForm: TForm);
    procedure QueueAsyncCall(const AMethod: TDataEvent; Data: PtrInt);
    procedure ReleaseComponent(AComponent: TComponent);
    function ExecuteAction(ExeAction: TBasicAction): Boolean; override;
    function UpdateAction(TheAction: TBasicAction): Boolean; override;
    procedure HandleException(Sender: TObject); override;
    procedure HandleMessage;
    function HelpContext(Sender: TObject; const Position: TPoint;
                         Context: THelpContext): Boolean;
    function HelpContext(Context: THelpContext): Boolean;
    function HelpKeyword(Sender: TObject; const Position: TPoint;
                         const Keyword: String): Boolean;
    function HelpKeyword(const Keyword: String): Boolean;
    procedure ShowHelpForObject(Sender: TObject);
    procedure HideAllFormsWithStayOnTop;
    function IsWaiting: boolean;
    procedure CancelHint;
    procedure HideHint;
    procedure HintMouseMessage(Control : TControl; var AMessage: TLMessage);
    property Icon: TIcon read FIcon write SetIcon;
    procedure Initialize; override;
    function MessageBox(Text, Caption: PChar; Flags: Longint): Integer;
    procedure Minimize;
    procedure Restore;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    Procedure ProcessMessages;
    Procedure Idle(Wait: Boolean);
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
    procedure AddOnIdleHandler(Handler: TIdleEvent; AsLast: Boolean=true);
    procedure RemoveOnIdleHandler(Handler: TIdleEvent);
    procedure AddOnIdleEndHandler(Handler: TNotifyEvent; AsLast: Boolean=true);
    procedure RemoveOnIdleEndHandler(Handler: TNotifyEvent);
    procedure AddOnUserInputHandler(Handler: TOnUserInputEvent;
                                    AsLast: Boolean=true);
    procedure RemoveOnUserInputHandler(Handler: TOnUserInputEvent);
    procedure AddOnKeyDownBeforeHandler(Handler: TKeyEvent;
                                        AsLast: Boolean=true);
    procedure RemoveOnKeyDownBeforeHandler(Handler: TKeyEvent);
    procedure AddOnKeyDownHandler(Handler: TKeyEvent; AsLast: Boolean=true);
    procedure RemoveOnKeyDownHandler(Handler: TKeyEvent);
    procedure AddOnActivateHandler(Handler: TNotifyEvent; AsLast: Boolean=true);
    procedure RemoveOnActivateHandler(Handler: TNotifyEvent);
    procedure AddOnDeactivateHandler(Handler: TNotifyEvent; AsLast: Boolean=true);
    procedure RemoveOnDeactivateHandler(Handler: TNotifyEvent);
    procedure AddOnExceptionHandler(Handler: TExceptionEvent; AsLast: Boolean=true);
    procedure RemoveOnExceptionHandler(Handler: TExceptionEvent);
    procedure AddOnEndSessionHandler(Handler: TNotifyEvent; AsLast: Boolean=true);
    procedure RemoveOnEndSessionHandler(Handler: TNotifyEvent);
    procedure AddOnQueryEndSessionHandler(Handler: TQueryEndSessionEvent; AsLast: Boolean=true);
    procedure RemoveOnQueryEndSessionHandler(Handler: TQueryEndSessionEvent);
    procedure AddOnMinimizeHandler(Handler: TNotifyEvent; AsLast: Boolean=true);
    procedure RemoveOnMinimizeHandler(Handler: TNotifyEvent);
    procedure AddOnRestoreHandler(Handler: TNotifyEvent; AsLast: Boolean=true);
    procedure RemoveOnRestoreHandler(Handler: TNotifyEvent);
    procedure AddOnDropFilesHandler(Handler: TDropFilesEvent; AsLast: Boolean=true);
    procedure RemoveOnDropFilesHandler(Handler: TDropFilesEvent);
    procedure AddOnHelpHandler(Handler: THelpEvent; AsLast: Boolean=true);
    procedure RemoveOnHelpHandler(Handler: THelpEvent);
    procedure AddOnHintHandler(Handler: TNotifyEvent; AsLast: Boolean=true);
    procedure RemoveOnHintHandler(Handler: TNotifyEvent);
    procedure AddOnShowHintHandler(Handler: TShowHintEvent; AsLast: Boolean=true);
    procedure RemoveOnShowHintHandler(Handler: TShowHintEvent);
    procedure RemoveAllHandlersOfObject(AnObject: TObject); virtual;
    procedure DoBeforeMouseMessage(CurMouseControl: TControl);
    function  IsShortcut(var Message: TLMKey): boolean;
    procedure IntfQueryEndSession(var Cancel : Boolean);
    procedure IntfEndSession;
    procedure IntfAppMinimize;
    procedure IntfAppRestore;
    procedure IntfDropFiles(const FileNames: Array of String);
  public
    procedure DoEscapeKey(AControl: TWinControl; var Key: Word;
                          Shift: TShiftState);
    procedure DoReturnKey(AControl: TWinControl; var Key: Word;
                          Shift: TShiftState);
    procedure DoTabKey(AControl: TWinControl; var Key: Word;Shift: TShiftState);
    property Active: boolean read GetActive;
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
    property Navigation: TApplicationNavigationOptions read FNavigation write SetNavigation;
    property MainForm: TForm read FMainForm;
    property OnActionExecute: TActionEvent read FOnActionExecute write FOnActionExecute;
    property OnActionUpdate: TActionEvent read FOnActionUpdate write FOnActionUpdate;
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
    property OnIdle: TIdleEvent read FOnIdle write FOnIdle;
    property OnIdleEnd: TNotifyEvent read FOnIdleEnd write FOnIdleEnd;
    property OnEndSession: TNotifyEvent read FOnEndSession write FOnEndSession;
    property OnQueryEndSession: TQueryEndSessionEvent read FOnQueryEndSession write FOnQueryEndSession;
    property OnMinimize: TNotifyEvent read FOnMinimize write FOnMinimize;
    property OnRestore: TNotifyEvent read FOnRestore write FOnRestore;
    property OnDropFiles: TDropFilesEvent read FOnDropFiles write FOnDropFiles;
    property OnHelp: THelpEvent read FOnHelp write FOnHelp;
    property OnHint: TNotifyEvent read FOnHint write FOnHint;
    property OnShortcut: TShortcutEvent read FOnShortcut write FOnShortcut;
    property OnShowHint: TShowHintEvent read FOnShowHint write FOnShowHint;
    property OnUserInput: TOnUserInputEvent read FOnUserInput write FOnUserInput;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property ShowHint: Boolean read FShowHint write SetShowHint;
    property ShowMainForm: Boolean read FShowMainForm write FShowMainForm default true;
    property Title: String read GetTitle write SetTitle;
    property ApplicationType : TApplicationType read FApplicationType write FApplicationType;
  end;


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
    FShowHint: Boolean;
    FShowMainForm: Boolean;
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
    procedure SetShowMainForm(const AValue: Boolean);
  protected
    Procedure SetCaptureExceptions(Const AValue : boolean);
    Procedure SetHelpFile(Const AValue : string);
    Procedure SetHint(Const AValue : string);
    Procedure SetHintColor(Const AValue : TColor);
    Procedure SetHintHidePause(Const AValue : Integer);
    Procedure SetHintPause(Const AValue : Integer);
    Procedure SetHintShortCuts(Const AValue : Boolean);
    Procedure SetHintShortPause(Const AValue : Integer);
    Procedure SetShowHint(Const AValue : Boolean);
    Procedure SetTitle(Const AValue : String);

    Procedure SetOnException(Const AValue : TExceptionEvent);
    Procedure SetOnIdle(Const AValue : TIdleEvent);
    Procedure SetOnIdleEnd(Const AValue : TNotifyEvent);
    Procedure SetOnEndSession(Const AValue : TNotifyEvent);
    Procedure SetOnQueryEndSession(Const AValue : TQueryEndSessionEvent);
    Procedure SetOnMinimize(Const AValue : TNotifyEvent);
    Procedure SetOnRestore(Const AValue : TNotifyEvent);
    Procedure SetOnDropFiles(const AValue: TDropFilesEvent);
    Procedure SetOnHelp(Const AValue : THelpEvent);
    Procedure SetOnHint(Const AValue : TNotifyEvent);
    Procedure SetOnShowHint(Const AValue : TShowHintEvent);
    Procedure SetOnUserInput(Const AValue : TOnUserInputEvent);
  public
    Constructor Create(AOwner: TComponent); Override;
    destructor Destroy; override;
  published
    property CaptureExceptions: boolean read FCaptureExceptions
                                        write SetCaptureExceptions;
    property HelpFile: string read FHelpFile write SetHelpFile;
    property Hint: string read FHint write SetHint;
    property HintColor: TColor read FHintColor write SetHintColor;
    property HintHidePause: Integer read FHintHidePause write SetHintHidePause;
    property HintPause: Integer read FHintPause write SetHintPause;
    property HintShortCuts: Boolean read FHintShortCuts write SetHintShortCuts;
    property HintShortPause: Integer read FHintShortPause write SetHintShortPause;
    property ShowHint: Boolean read FShowHint write SetShowHint;
    property ShowMainForm: Boolean read FShowMainForm write SetShowMainForm default true;
    property Title: String read FTitle write SetTitle;

    property OnException: TExceptionEvent read FOnException write SetOnException;
    property OnIdle: TIdleEvent read FOnIdle write SetOnIdle;
    property OnIdleEnd: TNotifyEvent read FOnIdleEnd write SetOnIdleEnd;
    property OnEndSession : TNotifyEvent read FOnEndSession write SetOnEndSession;
    property OnQueryEndSession : TQueryEndSessionEvent read FOnQueryEndSession write SetOnQueryEndSession;
    property OnMinimize : TNotifyEvent read FOnMinimize write SetOnMinimize;
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
    Procedure SelectOnlyThisComponent(AComponent: TComponent); virtual; abstract;
    function UniqueName(const BaseName: string): string; virtual; abstract;
  end;


  { TFormPropertyStorage - abstract base class }

  TFormPropertyStorage = class(TControlPropertyStorage)
  private
    procedure FormFirstShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;


{$IFNDEF UseFCLDataModule}
type
{ TDataModule }

  TDataModule = class(TComponent)
  private
    FDesignSize: TPoint;
    FDesignOffset: TPoint;
    FOnCreate: TNotifyEvent;
    FOnDestroy: TNotifyEvent;
    FOldCreateOrder: Boolean;
    function OldCreateOrderIsStored: boolean;
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
    property OldCreateOrder: Boolean read FOldCreateOrder write FOldCreateOrder
                                                  stored OldCreateOrderIsStored;
  end;

const
  AddDataModule: procedure (DataModule: TDataModule) of object = nil;
  RemoveDataModule: procedure (DataModule: TDataModule) of object = nil;
  ApplicationHandleException: procedure (Sender: TObject) of object = nil;
  ApplicationShowException: procedure (E: Exception) of object = nil;
{$ENDIF}


function KeysToShiftState(Keys: Word): TShiftState;
function KeyDataToShiftState(KeyData: Longint): TShiftState;

function WindowStateToStr(const State: TWindowState): string;
function StrToWindowState(const Name: string): TWindowState;

type
  TFocusState = Pointer;

function SaveFocusState: TFocusState;
procedure RestoreFocusState(FocusState: TFocusState);

type
  TGetDesignerFormEvent =
    function(AComponent: TComponent): TCustomForm of object;

var
  OnGetDesignerForm: TGetDesignerFormEvent = nil;

function GetParentForm(Control:TControl): TCustomForm;
function GetFirstParentForm(Control:TControl): TCustomForm;
function GetDesignerForm(AComponent: TComponent): TCustomForm;
function FindRootDesigner(AComponent: TComponent): TIDesigner;

function IsAccel(VK: word; const Str: string): Boolean;
procedure NotifyApplicationUserInput(Msg: Cardinal);


function GetShortHint(const Hint: string): string;
function GetLongHint(const Hint: string): string;


var
  Application: TApplication = nil;
  Screen: TScreen = nil;
  ExceptionObject: TExceptObject;
  HintWindowClass: THintWindowClass = THintWindow;

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


uses
  WSForms; // Widgetset uses circle is allowed

var
  FocusCount: Integer=0;
  HandlingException: boolean=False;
  HaltingProgram: boolean=False;

procedure Register;
begin
  RegisterComponents('Additional',[TScrollBox, TApplicationProperties]);
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
procedure ExceptionOccurred(Sender: TObject; Addr:Pointer; FrameCount: Longint;
  Frames: PPointer);
var
  FrameNumber: integer;
Begin
  DebugLn('[FORMS.PP] ExceptionOccurred ');
  if HaltingProgram or HandlingException then Halt;
  HandlingException:=true;
  if Sender<>nil then begin
    DebugLn('  Sender=',Sender.ClassName);
    if Sender is Exception then begin
      DebugLn('  Exception=',Exception(Sender).Message);
      DebugLn('  Stack trace:');
      DebugLn(BackTraceStrFunc(ExceptAddr));
      for FrameNumber := 0 to FrameCount-1 do
        DebugLn(BackTraceStrFunc(Frames[FrameNumber]));
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

//------------------------------------------------------------------------------
// The focus state is just the focus count for now. To save having to allocate
// anything, I just map the Integer to the TFocusState.
function SaveFocusState: TFocusState;
begin
  Result := TFocusState(PtrInt(FocusCount));
end;

procedure RestoreFocusState(FocusState: TFocusState);
begin
  FocusCount := integer(PtrUInt(FocusState));
end;

{function SendFocusMessage(Window: HWnd; Msg: Word): Boolean;
var
  Count: Integer;
begin
  Count := FocusCount;
  SendMessage(Window, Msg, 0, 0);
  Result := (FocusCount = Count);
end;}

//------------------------------------------------------------------------------
function KeysToShiftState(Keys:Word): TShiftState;
begin
  Result := [];
  if Keys and MK_Shift <> 0 then Include(Result,ssShift);
  if Keys and MK_Control <> 0 then Include(Result,ssCtrl);
  if Keys and MK_LButton <> 0 then Include(Result,ssLeft);
  if Keys and MK_RButton <> 0 then Include(Result,ssRight);
  if Keys and MK_MButton <> 0 then Include(Result,ssMiddle);
  if GetKeyState(VK_MENU) < 0 then Include(Result,ssAlt);
end;

function KeyDataToShiftState(KeyData: Longint): TShiftState;
begin
  Result := MsgKeyDataToShiftState(KeyData);
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
function IsAccel(VK: word; const Str: string): Boolean;
var
  lPos: integer;
begin
  lPos:=1;
  while (lPos<length(Str)) do begin
    if Str[lPos]<>'&' then begin
      inc(lPos);
    end else begin
      inc(lPos);
      if (Str[lPos]<>'&') then begin
        Result := UpCase(Str[lPos]) = UpCase(char(VK));
        exit;
      end else begin
        // skip double &&
        inc(lPos);
      end;
    end;
  end;
  Result := false;
end;

//==============================================================================

function FindRootDesigner(AComponent: TComponent): TIDesigner;
var
  Form: TCustomForm;
begin
  Result:=nil;
  Form:=GetDesignerForm(AComponent);
  if Form<>nil then
    Result:=Form.Designer;
end;

function GetFirstParentForm(Control: TControl): TCustomForm;
begin
  while (Control<>nil) and (not (Control is TCustomForm)) do
    Control:=Control.Parent;
  Result:=TCustomForm(Control);
end;

function GetDesignerForm(AComponent: TComponent): TCustomForm;
var
  OwnerComponent: TComponent;
begin
  Result:=nil;
  if AComponent=nil then exit;
  if Assigned(OnGetDesignerForm) then
    Result:=OnGetDesignerForm(AComponent)
  else begin
    OwnerComponent:=AComponent;
    while OwnerComponent.Owner<>nil do OwnerComponent:=OwnerComponent.Owner;
    if OwnerComponent is TCustomForm then Result:=TCustomForm(OwnerComponent);
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
    Screen.DestroyCursors;
  if Application=nil then exit;
  Application.Free;
  Application:=nil;
  FreeAllClipBoards;
  CallInterfaceFinalizationHandlers;
  WidgetSet.Free;
  WidgetSet:=nil;
end;


{$IFNDEF UseFCLDataModule}
{ TDataModule }

constructor TDataModule.Create(TheOwner: TComponent);
begin
  GlobalNameSpace.BeginWrite;
  try
    CreateNew(TheOwner,0);
    if (ClassType <> TDataModule) and not (csDesigning in ComponentState) then
    begin
      if not InitResourceComponent(Self, TDataModule) then begin
        raise EResNotFound.CreateFmt(lisLCLResourceSNotFound, [ClassName]);
      end;
      if OldCreateOrder then DoCreate;
    end;
  finally
    GlobalNameSpace.EndWrite;
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
  //try
    FOnCreate(Self);
  {except
    begin
      if not HandleCreateException then
        raise;
    end;
  end;}
end;

procedure TDataModule.DoDestroy;
begin
  if Assigned(FOnDestroy) then begin
    //try
      FOnDestroy(Self);
    {except
      begin
        if Assigned(ApplicationHandleException) then
          ApplicationHandleException(Self);
      end;
    end;}
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

function TDataModule.OldCreateOrderIsStored: boolean;
begin
  Result:=FOldCreateOrder;
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
{$I customframe.inc}
{$I customform.inc}
{$I customdockform.inc}
{$I screen.inc}
{$I application.inc}
{$I applicationproperties.inc}
{$I hintwindow.inc}


//==============================================================================

initialization
  {$INCLUDE cursors.lrs}
  LCLProc.OwnerFormDesignerModifiedProc:=@IfOwnerIsFormThenDesignerModified;
  Screen:=TScreen.Create(nil);
  Application:=TApplication.Create(nil);

finalization
  //DebugLn('forms.pp - finalization section');
  LCLProc.OwnerFormDesignerModifiedProc:=nil;
  HintWindowClass:=nil;
  FreeThenNil(Application);
  FreeThenNil(Screen);

end.



