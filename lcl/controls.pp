{  $Id$  }
{
 /***************************************************************************
                               Controls.pp
                             -------------------
                             Component Library Controls
                   Initial Revision  : Sat Apr 10 22:49:32 CST 1999


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
unit Controls;
     
{$mode objfpc}{$H+}

interface

{$ifdef Trace}
{$ASSERTIONS ON}
{$endif}

{$IFOPT C-}
// Uncomment for local trace
//  {$C+}
//  {$DEFINE ASSERT_IS_ON}
{$ENDIF}

uses
  Classes, SysUtils, vclglobals, GraphType, Graphics, LMessages, LCLLinux,
  ImgList, UTrace, Menus, LCLType;


const
  CM_BASE                 = $B000;
  CM_ACTIVATE             = CM_BASE + 0;
  CM_DEACTIVATE           = CM_BASE + 1;
  CM_GOTFOCUS             = CM_BASE + 2;
  CM_LOSTFOCUS            = CM_BASE + 3;
  CM_CANCELMODE           = CM_BASE + 4;
  CM_DIALOGKEY            = CM_BASE + 5;
  CM_DIALOGCHAR           = CM_BASE + 6;
  CM_FOCUSCHANGED         = CM_BASE + 7;
  CM_PARENTFONTCHANGED    = CM_BASE + 8;
  CM_PARENTCOLORCHANGED   = CM_BASE + 9;
  CM_HITTEST              = CM_BASE + 10;
  CM_VISIBLECHANGED       = CM_BASE + 11;
  CM_ENABLEDCHANGED       = CM_BASE + 12;
  CM_COLORCHANGED         = CM_BASE + 13;
  CM_FONTCHANGED          = CM_BASE + 14;
  CM_CURSORCHANGED        = CM_BASE + 15;
  CM_CTL3DCHANGED         = CM_BASE + 16;
  CM_PARENTCTL3DCHANGED   = CM_BASE + 17;
  CM_TEXTCHANGED          = CM_BASE + 18;
  CM_MOUSEENTER           = CM_BASE + 19;
  CM_MOUSELEAVE           = CM_BASE + 20;
  CM_MENUCHANGED          = CM_BASE + 21;
  CM_APPKEYDOWN           = CM_BASE + 22;
  CM_APPSYSCOMMAND        = CM_BASE + 23;
  CM_BUTTONPRESSED        = CM_BASE + 24;
  CM_SHOWINGCHANGED       = CM_BASE + 25;
  CM_ENTER                = CM_BASE + 26;
  CM_EXIT                 = CM_BASE + 27;
  CM_DESIGNHITTEST        = CM_BASE + 28;
  CM_ICONCHANGED          = CM_BASE + 29;
  CM_WANTSPECIALKEY       = CM_BASE + 30;
  CM_INVOKEHELP           = CM_BASE + 31;
  CM_WINDOWHOOK           = CM_BASE + 32;
  CM_RELEASE              = CM_BASE + 33;
  CM_SHOWHINTCHANGED      = CM_BASE + 34;
  CM_PARENTSHOWHINTCHANGED= CM_BASE + 35;
  CM_SYSCOLORCHANGE       = CM_BASE + 36;
  CM_WININICHANGE         = CM_BASE + 37;
  CM_FONTCHANGE           = CM_BASE + 38;
  CM_TIMECHANGE           = CM_BASE + 39;
  CM_TABSTOPCHANGED       = CM_BASE + 40;
  CM_UIACTIVATE           = CM_BASE + 41;
  CM_UIDEACTIVATE         = CM_BASE + 42;
  CM_DOCWINDOWACTIVATE    = CM_BASE + 43;
  CM_CONTROLLISTCHANGE    = CM_BASE + 44;
  CM_GETDATALINK          = CM_BASE + 45;
  CM_CHILDKEY             = CM_BASE + 46;
  CM_DRAG                 = CM_BASE + 47;
  CM_HINTSHOW             = CM_BASE + 48;
  CM_DIALOGHANDLE         = CM_BASE + 49;
  CM_ISTOOLCONTROL        = CM_BASE + 50;
  CM_RECREATEWND          = CM_BASE + 51;
  CM_INVALIDATE           = CM_BASE + 52;
  CM_SYSFONTCHANGED       = CM_BASE + 53;
  CM_CONTROLCHANGE        = CM_BASE + 54;
  CM_CHANGED              = CM_BASE + 55;
  CM_DOCKCLIENT           = CM_BASE + 56;
  CM_UNDOCKCLIENT         = CM_BASE + 57;
  CM_FLOAT                = CM_BASE + 58;
  CM_BORDERCHANGED        = CM_BASE + 59;
  CM_BIDIMODECHANGED      = CM_BASE + 60;
  CM_PARENTBIDIMODECHANGED= CM_BASE + 61;
  CM_ALLCHILDRENFLIPPED   = CM_BASE + 62;
  CM_ACTIONUPDATE         = CM_BASE + 63;
  CM_ACTIONEXECUTE        = CM_BASE + 64;
  CM_HINTSHOWPAUSE        = CM_BASE + 65;
  CM_DOCKNOTIFICATION     = CM_BASE + 66;
  CM_MOUSEWHEEL           = CM_BASE + 67;

  CN_BASE              = $BC00;
  CN_CHARTOITEM        = CN_BASE + LM_CHARTOITEM;
  CN_COMMAND           = CN_BASE + LM_COMMAND;
  CN_COMPAREITEM       = CN_BASE + LM_COMPAREITEM;
  CN_CTLCOLORBTN       = CN_BASE + LM_CTLCOLORBTN;
  CN_CTLCOLORDLG       = CN_BASE + LM_CTLCOLORDLG;
  CN_CTLCOLOREDIT      = CN_BASE + LM_CTLCOLOREDIT;
  CN_CTLCOLORLISTBOX   = CN_BASE + LM_CTLCOLORLISTBOX;
  CN_CTLCOLORMSGBOX    = CN_BASE + LM_CTLCOLORMSGBOX;
  CN_CTLCOLORSCROLLBAR = CN_BASE + LM_CTLCOLORSCROLLBAR;
  CN_CTLCOLORSTATIC    = CN_BASE + LM_CTLCOLORSTATIC;
  CN_DELETEITEM        = CN_BASE + LM_DELETEITEM;
  CN_DRAWITEM          = CN_BASE + LM_DRAWITEM;
  CN_HSCROLL           = CN_BASE + LM_HSCROLL;
  CN_MEASUREITEM       = CN_BASE + LM_MEASUREITEM;
  CN_PARENTNOTIFY      = CN_BASE + LM_PARENTNOTIFY;
  CN_VKEYTOITEM        = CN_BASE + LM_VKEYTOITEM;
  CN_VSCROLL           = CN_BASE + LM_VSCROLL;
  CN_KEYDOWN           = CN_BASE + LM_KEYDOWN;
  CN_KEYUP             = CN_BASE + LM_KEYUP;
  CN_CHAR              = CN_BASE + LM_CHAR;
  CN_SYSKEYDOWN        = CN_BASE + LM_SYSKEYDOWN;
  CN_SYSCHAR           = CN_BASE + LM_SYSCHAR;
  CN_NOTIFY            = CN_BASE + LM_NOTIFY;


const
   mrNone = 0;
   mrOK = mrNone + 1;
   mrCancel = mrNone + 2;
   mrAbort = mrNone + 3;
   mrRetry = mrNone + 4;
   mrIgnore = mrNone + 5;
   mrYes = mrNone + 6;
   mrNo = mrNone + 7;
   mrAll = mrNone + 8;
   mrNoToAll = mrNone + 9;
   mrYesToAll = mrNone + 10;
   mrLast = mrYesToAll;


type
  TWinControl = class;
  TControl = class;
  
  TDate = type TDateTime;
  TTime = type TDateTime;

  TCMMouseWheel = record
    MSg: Cardinal;
    ShiftState : TShiftState;
    Unused : Byte;
    WheelData : SmallInt;
    case Integer of
    0 : (
      XPos : SmallInt;
      YPos : SmallInt);
    1 : (
      Pos : TSmallPoint;
      Result : LongInt);
  end;

  TCMHitTest = TLMNCHitTest;

  TCMControlChange = record
    Msg : Cardinal;
    Control : TControl;
    Inserting : Boolean;
    Result : Longint;
  End;

  TCMDialogChar = TLMKEY;
  TCMDialogKey = TLMKEY;

  TAlign = (alNone, alTop, alBottom, alLeft, alRight, alClient);
  TAlignSet = set of TAlign;
  TAnchorKind = (akTop, akLeft, akRight, akBottom);
  TAnchors = set of TAnchorKind;
  TCaption = String;
  TCursor = -32768..32767;

  TFormStyle = (fsNormal, fsMDIChild, fsMDIFORM, fsStayOnTop);
  TFormBorderStyle = (bsNone, bsSingle, bsSizeable, bsDialog, bsToolWindow,
                      bsSizeToolWin);
  TBorderStyle = bsNone..bsSingle;
  TControlBorderStyle = TBorderStyle;

  TMouseButton = (mbLeft, mbRight, mbMiddle);

const
  // Cursor constants
  crDefault     = TCursor(0);
  crNone        = TCursor(-1);
  crArrow       = TCursor(-2);
  crCross       = TCursor(-3);
  crIBeam       = TCursor(-4);
  crSize        = TCursor(-22);
  crSizeNESW    = TCursor(-6);
  crSizeNS      = TCursor(-7);
  crSizeNWSE    = TCursor(-8);
  crSizeWE      = TCursor(-9);
  crUpArrow     = TCursor(-10);
  crHourGlass   = TCursor(-11);
  crDrag        = TCursor(-12);
  crNoDrop      = TCursor(-13);
  crHSplit      = TCursor(-14);
  crVSplit      = TCursor(-15);
  crMultiDrag   = TCursor(-16);
  crSQLWait     = TCursor(-17);
  crNo          = TCursor(-18);
  crAppStart    = TCursor(-19);
  crHelp        = TCursor(-20);
  crHandPoint   = TCursor(-21);
  crSizeAll     = TCursor(-22);



type
  TWndMethod = procedure(var Message : TLMessage) of Object;

  TControlStyleType = (
    csAcceptsControls,
    csCaptureMouse,
    csDesignInteractive,
    csClickEvents,
    csFramed,
    csSetCaption,
    csOpaque,
    csDoubleClicks,// control understands mouse double clicks
    csTripleClicks,// control understands mouse triple clicks
    csQuadClicks,  // control understands mouse quad clicks
    csFixedWidth,
    csFixedHeight,
    csNoDesignVisible,
    csReplicatable,
    csNoStdEvents,
    csDisplayDragImage,
    csReflector,
    csActionClient,
    csMenuEvents,
    csNoFocus);
  TControlStyle = set of TControlStyleType;
  
const
  csMultiClicks = [csDoubleClicks,csTripleClicks,csQuadClicks];


type
  TControlStateType = (
    csLButtonDown,
    csClicked,
    csPalette,
    csReadingState,
    csAlignmentNeeded,
    csFocusing,
    csCreating,
    csPaintCopy,
    csCustomPaint,
    csDestroyingHandle,
    csDocking,
    csVisibleSetInLoading
    );
  TControlState = set of TControlStateType;



  TControlCanvas = class(TCanvas)
  private
    FControl: Tcontrol;
    FDeviceContext: HDC;
    FWindowHandle: HWND;
    procedure SetControl(AControl: TControl);
  protected
    procedure CreateHandle; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure FreeHandle;
    property Control: TControl read FControl write SetControl;
  end;

  {TDragImageList}
   TDragImageList = class(TCustomImageList)
    end;



  TKeyEvent = procedure(Sender: TObject; var Key: Word; Shift:TShiftState) of Object;
  TKeyPressEvent = procedure(Sender: TObject; var Key: Char) of Object;
  
  TMouseEvent = Procedure(Sender : TOBject; Button: TMouseButton;
                          Shift : TShiftState; X, Y: Integer) of object;
  TMouseMoveEvent = Procedure(Sender: TObject; Shift: TShiftState;
                              X, Y: Integer) of object;
  TMouseWheelEvent = Procedure(Sender: TObject; Shift: TShiftState;
         WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean) of object;
  TMouseWheelUpDownEvent = Procedure(Sender: TObject;
          Shift: TShiftState; MousePos: TPoint; var Handled: Boolean) of object;
          
  TDragState = (dsDragEnter, dsDragLEave, dsDragMove);
  TDragMode = (dmManual , dmAutomatic);
  TDragKind = (dkDrag, dkDock);
  TDragMessage = (dmDragEnter, dmDragLeave, dmDragMove, dmDragDrop,
                  dmDragCancel,dmFindTarget);
  TDragOverEvent = Procedure(Sender, Source: TObject;
               X,Y : Integer; State: TDragState; Var Accept: Boolean) of Object;
  TDragDropEvent = Procedure(Sender, Source: TObject; X,Y : Integer) of Object;

  TDragObject = class;

  PDragRec = ^TDragRec;
  TDragRec = record
    Pos: TPoint;
    Source: TDragObject;
    Target: Pointer;
    Docking: Boolean;
  end;

  TCMDrag = packed record
    Msg: Cardinal;
    DragMessage: TDragMessage;
    Reserved1: Byte;
    Reserved2: Word;
    DragRec: PDragRec;
    Result: Longint;
  end;

  TDragObject = class(TObject)
  private
    FDragTarget: Pointer;
    FDragHandle: HWND;
    FDragPos: TPoint;
    FDragTargetPos: TPoint;
    FMouseDeltaX: Double;
    FMouseDeltaY: Double;
    FCancelling: Boolean;
    function Capture: HWND;
    procedure MouseMsg(var Msg: TLMessage);
    procedure ReleaseCapture(Handle: HWND);
  protected
    procedure Finished(Target: TObject; X, Y: Integer; Accepted: Boolean); virtual;
    function GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor; virtual;
    function GetDragImages: TDragImageList; virtual;
  public
    procedure Assign(Source: TDragObject); virtual;
    function GetName: string; virtual;
    procedure HideDragImage; virtual;
    function Instance: THandle; virtual;
    procedure ShowDragImage; virtual;
    property Cancelling: Boolean read FCancelling write FCancelling;
    property DragHandle: HWND read FDragHandle write FDragHandle;
    property DragPos: TPoint read FDragPos write FDragPos;
    property DragTargetPos: TPoint read FDragTargetPos write FDragTargetPos;
    property DragTarget: Pointer read FDragTarget write FDragTarget;
    property MouseDeltaX: Double read FMouseDeltaX;
    property MouseDeltaY: Double read FMouseDeltaX;
  end;

  TStartDragEvent = Procedure(Sender : TObject; DragObject: TDragObject) of Object;
  TEndDragEvent = Procedure(Sender , Target: TObject; X,Y : Integer) of Object;

  TBaseDragControlObject = class(TDragObject)
  private
    FControl : TControl;
  protected
    Procedure EndDrag(Target: TObject; X,Y : Integer); Virtual;
    procedure Finished(Target: TObject; X, Y: Integer; Accepted: Boolean); override;
  Public
    constructor Create(AControl : TControl); virtual;
    property Control : TControl read FControl write FControl;
  end;

  TDragControlObject = class(TBaseDragControlObject)
   end;

  TConstraintSize = 0..MaxInt;
  
  TSizeConstraints = class(TPersistent)
  private
    FControl: TControl;
    FMaxHeight: TConstraintSize;
    FMaxWidth: TConstraintSize;
    FMinHeight: TConstraintSize;
    FMinWidth: TConstraintSize;
    FOnChange: TNotifyEvent;
  protected
    procedure Change; dynamic;
    procedure AssignTo(Dest: TPersistent); override;
    property Control: TControl read FControl;
    procedure SetMaxHeight(Value : TConstraintSize); virtual;
    procedure SetMaxWidth(Value : TConstraintSize); virtual;
    procedure SetMinHeight(Value : TConstraintSize); virtual;
    procedure SetMinWidth(Value : TConstraintSize); virtual;
  public
    constructor Create(AControl: TControl); virtual;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property MaxHeight: TConstraintSize read FMaxHeight write SetMaxHeight default 0;
    property MaxWidth: TConstraintSize read FMaxWidth write SetMaxWidth default 0;
    property MinHeight: TConstraintSize read FMinHeight write SetMinHeight default 0;
    property MinWidth: TConstraintSize read FMinWidth write SetMinWidth default 0;
  end;
									      
  TConstrainedResizeEvent = procedure(Sender : TObject;
      var MinWidth, MinHeight, MaxWidth, MaxHeight : TConstraintSize) of object;


  { TControl }

  TControl = class(TComponent)
  private
    FAnchors : TAnchors;
    FAlign : TAlign;
    FAutoSize : Boolean;
    FCaption : TCaption;
    FColor : TColor;
    FConstraints : TSizeConstraints;
    FControlStyle: TControlStyle;
    FCtl3D : Boolean;
    FCursor : TCursor;
    //FDesktopFont: Boolean;
    FDragCursor : TCursor;
    FDragKind : TDragKind;
    FDragMode : TDragMode;
    FEnabled : Boolean;
    FFont : TFont;
    FHeight: Integer;
    FHostDockSite : TWinControl;
    FHint : String;
    FIsControl : Boolean;
    FLastHeight : Integer;
    FLastResize : TPoint;
    FLastResizeHeight: integer;
    FLastResizeWidth: integer;
    FLastResizeClientHeight: integer;
    FLastResizeClientWidth: integer;
    FLastWidth : Integer;
    FLeft: Integer;
    FOnActivate : TNotifyEvent;
    FOnResize: TNotifyEvent;
    FOnClick: TNotifyEvent;
    FOnConstrainedResize : TConstrainedResizeEvent;
    FOnDblClick : TNotifyEvent;
    FOnTripleClick : TNotifyEvent;
    FOnQuadClick : TNotifyEvent;
    FOnDragDrop : TDragDropEvent;
    FOnDragOver : TDragOverEvent;
    FOnEndDrag : TEndDragEvent;
    FOnMouseDown : TMouseEvent;
    FOnMouseMove : TMouseMoveEvent;
    FOnMouseUp: TMouseEvent;
    FOnStartDrag: TStartDragEvent;
    FParent: TWinControl;
    FParentColor : Boolean;
    FParentFont : Boolean;
    FParentShowHint : Boolean;
    FPopupMenu : TPopupMenu;
    FShowHint : Boolean;
    FText : TCaption;
    FTop: Integer;
    FWidth: Integer;
    FWindowProc: TWndMethod;
    FVisible: Boolean;
    procedure DoConstrainedResize(var NewWidth, NewHeight : integer);
    procedure CheckMenuPopup(const P : TSmallPoint);
    function GetClientHeight: Integer;
    function GetClientWidth: Integer;
    procedure SetAlign(Value : TAlign);
    procedure SetBoundsRect(const Rect : TRect);
    procedure SetClientHeight(Value: Integer);
    procedure SetClientSize(Value: TPoint);
    procedure SetClientWidth(Value: Integer);
    procedure SetConstraints(const Value : TSizeConstraints);
    procedure SetColor(Value : TColor);
    procedure SetCursor(Value : TCursor);
    procedure SetFont(Value: TFont);
    procedure SetHeight(Value: Integer);
    procedure SetLeft(Value: Integer);
    procedure SetMouseCapture(Value : Boolean);
    procedure SetParentShowHint(Value : Boolean);
    procedure SetPopupMenu(Value : TPopupMenu);
    procedure SetShowHint(Value : Boolean);
    procedure SetTop(Value: Integer);
    procedure SetVisible(Value: Boolean);
    procedure SetWidth(Value: Integer);
    function GetBoundsRect : TRect;
    function GetMouseCapture : Boolean;
    function IsCaptionStored : Boolean;
    procedure DoDragMsg(var Dragmsg : TCMDrag);
    procedure DoMouseDown(var Message: TLMMouse; Button: TMouseButton; Shift:TShiftState);
    procedure DoMouseUp(var Message: TLMMouse; Button: TMouseButton);
  protected
    FControlState: TControlState;
    procedure AdjustSize; dynamic;
    procedure SetAutoSize(const Value : Boolean); virtual;
    procedure BoundsChanged; dynamic;
    procedure DoConstraintsChange(Sender : TObject); virtual;
    procedure Changed;
    procedure WMLButtonDown(Var Message: TLMLButtonDown); message LM_LBUTTONDOWN;
    procedure WMRButtonDown(Var Message: TLMRButtonDown); message LM_RBUTTONDOWN;
    procedure WMMButtonDown(Var Message: TLMMButtonDown); message LM_MBUTTONDOWN;
    procedure WMLButtonDBLCLK(Var Message: TLMLButtonDblClk); message LM_LBUTTONDBLCLK;
    procedure WMRButtonDBLCLK(Var Message: TLMRButtonDblClk); message LM_RBUTTONDBLCLK;
    procedure WMMButtonDBLCLK(Var Message: TLMMButtonDblClk); message LM_MBUTTONDBLCLK;
    procedure WMLButtonTripleCLK(Var Message: TLMLButtonTripleClk); message LM_LBUTTONTRIPLECLK;
    procedure WMRButtonTripleCLK(Var Message: TLMRButtonTripleClk); message LM_RBUTTONTRIPLECLK;
    procedure WMMButtonTripleCLK(Var Message: TLMMButtonTripleClk); message LM_MBUTTONTRIPLECLK;
    procedure WMLButtonQuadCLK(Var Message: TLMLButtonQuadClk); message LM_LBUTTONQUADCLK;
    procedure WMRButtonQuadCLK(Var Message: TLMRButtonQuadClk); message LM_RBUTTONQUADCLK;
    procedure WMMButtonQuadCLK(Var Message: TLMMButtonQuadClk); message LM_MBUTTONQUADCLK;
    procedure WMMouseMove(Var Message: TLMMouseMove); message LM_MOUSEMOVE;
    procedure WMLButtonUp(var Message: TLMLButtonUp); message LM_LBUTTONUP;
    procedure WMRButtonUp(var Message: TLMRButtonUp); message LM_RBUTTONUP;
    procedure WMMButtonUp(var Message: TLMMButtonUp); message LM_MBUTTONUP;
    procedure WMDragStart(Var Message: TLMessage); message LM_DRAGSTART;  //not in delphi
    procedure WMMove(var Message: TLMMove); message LM_MOVE;
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    procedure WMWindowPosChanged(var Message: TLMWindowPosChanged); message LM_WINDOWPOSCHANGED;
    procedure LMCaptureChanged(Var Message: TLMessage); message LM_CaptureChanged;
    procedure CMEnabledChanged(var Message: TLMEssage); message CM_ENABLEDCHANGED;
    procedure CMHitTest(Var Message: TCMHittest) ; Message CM_HITTEST;
    procedure CMMouseEnter(var Message :TLMessage); message CM_MouseEnter;
    procedure CMMouseLeave(var Message :TLMessage); message CM_MouseLeave;
    procedure CMVisibleChanged(var Message : TLMessage); message CM_VISIBLECHANGED;
    procedure ConstrainedResize(var MinWidth, MinHeight, MaxWidth, MaxHeight : TConstraintSize); virtual;
    procedure Resize;
    procedure RequestAlign; dynamic;
    procedure BeginAutoDrag; dynamic;
    procedure ChangeBounds(ALeft, ATop, AWidth, AHeight : integer); virtual;
    procedure DoSetBounds(ALeft, ATop, AWidth, AHeight : integer); virtual;
    procedure ChangeScale(M,D : Integer); dynamic;
    procedure Click; dynamic;
    procedure DblClick; dynamic;
    procedure TripleClick; dynamic;
    procedure QuadClick; dynamic;
    procedure DoStartDrag(var DragObject: TDragObject); dynamic;
    procedure DragOver(Source: TObject; X,Y : Integer; State : TDragState; var Accept:Boolean); dynamic;
    procedure DragCanceled; dynamic;
    procedure CreateComponent(AOwner : TComponent);
    procedure DestroyComponent;
    procedure DoEndDrag(Target: TObject; X,Y : Integer); dynamic;
    procedure InvalidateControl(IsVisible, IsOpaque : Boolean);
    procedure SendDockNotification(Msg: Cardinal; WParam, LParam : Integer);
    procedure SetDragMode (Value: TDragMode); virtual;
    procedure SetEnabled(Value: Boolean); virtual;
    procedure SetHint(const Value: String); virtual;
    procedure SetName(const Value: TComponentName); override;
    procedure SetParent(AParent: TWinControl); virtual;
    Procedure SetParentComponent(Value: TComponent); override;
    function GetText: TCaption; virtual;
    procedure SetText(const Value: TCaption); virtual;
    procedure WndProc(var TheMessage : TLMessage); virtual;
    Procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); dynamic;
    Procedure MouseMove(Shift: TShiftState; X,Y: Integer);Dynamic;
    Procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); dynamic;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    Function CanAutoSize(var NewWidth, NewHeight : Integer): Boolean; virtual;
    Function GetClientOrigin : TPoint; virtual;
    Function GetClientRect: TRect; virtual;
    function GetDeviceContext(var WindowHandle: HWnd): HDC; virtual;
    Function GetEnabled: Boolean; virtual;
    Function GetPopupMenu: TPopupMenu; dynamic;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default FALSE;
    property DragCursor: TCursor read FDragCursor write FDragCursor default crDrag;
    property DragKind: TDragKind read FDragKind write FDragKind default dkDrag;
    property DragMode: TDragMode read fDragMode write SetDragMode default dmManual;
    property IsControl: Boolean read FIsControl write FIsControl;
    property MouseCapture: Boolean read GetMouseCapture write SetMouseCapture;
    property ParentFont: Boolean  read FParentFont write FParentFont;
    property ParentColor: Boolean  read FParentColor write FParentColor;
    property ParentShowHint : Boolean read FParentShowHint write SetParentShowHint default True;
    property PopupMenu : TPopupmenu read GetPopupmenu write SetPopupMenu;
    property Text: TCaption read GetText write SetText;
    property OnConstrainedResize : TConstrainedResizeEvent read FOnConstrainedResize write FOnConstrainedResize;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnTripleClick: TNotifyEvent read FOnTripleClick write FOnTripleClick;
    property OnQuadClick: TNotifyEvent read FOnQuadClick write FOnQuadClick;
    property OnDragDrop: TDragDropEvent read FOnDragDrop write FOnDragDrop;
    property OnDragOver: TDragOverEvent read FOnDragOver write FOnDragOver;
    property OnEndDrag: TEndDragEvent read FOnEndDrag write FOnEndDrag;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnStartDrag: TStartDragEvent read FOnStartDrag write FOnStartDrag;

  public
    FCompStyle : LongInt;
    // use overload to simulate default
    procedure BeginDrag(Immediate: Boolean; Threshold: Integer); //overload;
    procedure BeginDrag(Immediate: Boolean); //overload;
    procedure BringToFront;
    constructor Create(AOwner: TComponent);override;
    destructor Destroy; override;
    function HasParent : Boolean; override;
    procedure Refresh;
    procedure Repaint; virtual;
    Procedure Invalidate; virtual;
    procedure AddControl; virtual;
    Procedure DragDrop(Source: TObject; X,Y : Integer); Dynamic;
    procedure SendToBack;
    procedure SetBounds(aLeft, aTop, aWidth, aHeight : integer); virtual;
    function  GetTextBuf(Buffer: PChar; BufSize: Integer): Integer;
    Procedure SetTextBuf(Buffer : PChar);
    Function  Perform(Msg:Cardinal; WParam , LParam : LongInt): LongInt;
    Function  ScreenToClient(const Point : TPoint) : TPoint;
    Function  ClientToScreen(const Point : TPoint) : TPoint;
    Function  Dragging : Boolean;
    procedure Show;
    procedure Update; //override;   //pbd
    procedure SetZOrderPosition(Position : Integer); virtual;
    Procedure SetZOrder(Topmost: Boolean); virtual;
  public
    property Anchors : TAnchors read FAnchors write FAnchors default [akLeft,akTop];
    property Align : TAlign read FAlign write SetAlign;
    property BoundsRect : TRect read GetBoundsRect write SetBoundsRect;
    property Caption: TCaption read GetText write SetText stored IsCaptionStored;
    property ClientOrigin: TPoint read GetClientOrigin;
    property ClientRect: TRect read GetClientRect;
    property ClientHeight: Integer read GetClientHeight write SetClientHeight stored False;
    property ClientWidth: Integer read GetClientWidth write SetClientWidth stored False;
    property Constraints : TSizeConstraints read FConstraints write SetConstraints;
    property ControlState: TControlState read FControlState write FControlState;
    property ControlStyle : TControlStyle read FControlStyle write FControlStyle;
    property Color : TColor read FColor write SetColor;
    property Ctl3D : Boolean read FCtl3D write FCtl3D;  //Is this needed for anything other than compatability?
    property Enabled: Boolean read GetEnabled write SetEnabled default True;
    property Font : TFont read FFont write SetFont;
    property HostDockSite : TWincontrol read FHostDockSite write FHostDockSite;
    property Parent : TWinControl read FParent write SetParent;
    property ShowHint : Boolean read FShowHint write SetShowHint default False;
    property Visible: Boolean read FVisible write SetVisible default True;
    property WindowProc: TWndMethod read FWindowProc write FWindowProc;
  public
    property OnResize: TNotifyEvent read FOnResize write FOnResize;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  published
    property Cursor: TCursor read FCursor write SetCursor default crDefault;
    property Left: Integer read FLeft write SetLeft;
    property Height: Integer read FHeight write SetHeight;
    property Hint: String read FHint write SetHint;
    property Top: Integer read FTop write SetTop;
    property Width: Integer read FWidth write SetWidth;
  end;


  TLMEnter = TLMNoPara;
  TLMExit  = TLMNoPara;

  TCreateParams = record
    Caption: PChar;
    Style: Cardinal;
    ExStyle: Cardinal;
    X, Y: Integer;
    Width, Height: Integer;
    WndParent: HWnd;
    Param: Pointer;
    WindowClass: TWndClass;
    WinClassName: array[0..63] of Char;
  end;

  TBorderWidth = 0..MaxInt;

  TGetChildProc = procedure(Child: TComponent) of Object;
  TTabOrder = -1..32767;


  { TWinControl }

  TWinControlFlag = (wcfClientRectNeedsUpdate);
  TWinControlFlags = set of TWinControlFlag;

  TWinControl = class(TControl)
  private
    FAlignLevel : Word;
    FBorderWidth : TBorderWidth;
    FBoundsLockCount: integer;
    FBoundsRealized: TRect;
    FBrush: TBrush;
    FAdjustClientRectRealized: TRect;
    FControls : TList;
    FDefWndProc : Pointer;
    //FDockSite : Boolean;
    FClientWidth : Integer;
    FClientHeight : Integer;
    FFlags: TWinControlFlags;
    //FUseDockManager : Boolean;
    FOnKeyDown : TKeyEvent;
    FOnKeyPress: TKeyPressEvent;
    FOnKeyUp : TKeyEvent;
    FOnMouseWheel: TMouseWheelEvent;
    FOnMouseWheelDown: TMouseWheelUpDownEvent;
    FOnMouseWheelUp: TMouseWheelUpDownEvent;
    FOnEnter : TNotifyEvent;
    FOnExit  : TNotifyEvent;
    FParentWindow : hwnd;
    FParentCtl3D : Boolean;
    FHandle: Hwnd;
    FShowing : Boolean;
    FTabList : TList;
    FTabOrder : Integer;
    FTabStop : Boolean;
    FWinControls : TList;
    procedure AlignControl(AControl : TControl);
    function  GetControl(const Index: Integer): TControl;
    function  GetControlCount: Integer;
    function  GetHandle : HWND;
    function  GetIsResizing: boolean;
    procedure SetHandle(NewHandle: HWND);
    Function  GetTabOrder: TTabOrder;
    Procedure SetBorderWidth(Value : TBorderWidth);
    Procedure SetParentCtl3D(value : Boolean);
    Procedure SetTabOrder(Value : TTabOrder);
    Procedure UpdateTaborder(value : TTabOrder);
  protected
    AutoSizing : Boolean;

    procedure AdjustSize; override;
    procedure AdjustClientRect(var Rect: TRect); virtual;
    procedure AlignControls(AControl : TControl; var Rect: TRect); virtual;
    Procedure SetAutoSize(const Value : Boolean); Override;
    procedure BoundsChanged; override;
    Procedure CMDrag(var Message : TCMDrag); message CM_DRAG;
    procedure CMShowHintChanged(var Message: TLMessage); message CM_SHOWHINTCHANGED;
    procedure CMShowingChanged(var Message: TLMessage); message CM_SHOWINGCHANGED;
    procedure CMVisibleChanged(var TheMessage: TLMessage); message CM_VISIBLECHANGED;
    procedure CreateSubClass(var Params: TCreateParams;ControlClassName: PChar);
    procedure DoConstraintsChange(Sender : TObject); override;
    procedure DoSetBounds(ALeft, ATop, AWidth, AHeight : integer); override;
    procedure DoAutoSize; Virtual;
    procedure GetChildren(Proc : TGetChildProc; Root : TComponent); override;
    procedure PaintControls(DC: HDC; First: TControl);
    procedure PaintHandler(var TheMessage: TLMPaint);
    procedure PaintWindow(DC: HDC); virtual;
    procedure CMEnabledChanged(var Message: TLMEssage); message CM_ENABLEDCHANGED;
    procedure WMEraseBkgnd(var Message : TLMEraseBkgnd); message LM_ERASEBKGND;
    procedure WMNotify(var Message: TLMNotify); message LM_NOTIFY;
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
    procedure WMShowWindow(var Message: TLMShowWindow); message LM_SHOWWINDOW;
    //move to CM
    procedure WMEnter(var Message: TLMEnter); message LM_ENTER;
    procedure WMExit(var Message: TLMExit); message LM_EXIT;
    procedure WMMouseWheel(var Message: TLMMouseEvent); message LM_MOUSEWHEEL;
    procedure WMKeyDown(var Message: TLMKeyDown); message LM_KEYDOWN;
    procedure WMKeyUp(var Message: TLMKeyUp); message LM_KEYUP;
    procedure WMChar(var Message: TLMChar); message LM_CHAR;
    procedure WMPaint(var Msg: TLMPaint); message LM_PAINT;
    procedure WMDestroy(var Message: TLMDestroy); message LM_DESTROY;
    procedure WMMove(var Message: TLMMove); message LM_MOVE;
    procedure WMSize(var Message: TLMSize); message LM_SIZE;

    procedure CreateParams(var Params: TCreateParams); virtual;
    procedure DestroyHandle; virtual;
    procedure DoEnter; dynamic;
    procedure DoExit; dynamic;
    procedure KeyDown(var Key : Word; Shift : TShiftState); dynamic;
    procedure KeyPress(var Key : Char); dynamic;
    procedure KeyUp(var Key : Word; Shift : TShiftState); dynamic;
    procedure MainWndProc(var Message : TLMessage);
    procedure ReAlign;
    procedure ReCreateWnd;
    procedure RemoveFocus(Removing: Boolean);
    procedure SetHint(const Value: String); override;
    procedure SetText(const Value: TCaption); override;
    procedure UpdateControlState;
    procedure CreateHandle; virtual;
    procedure CreateWnd; virtual; //creates the window
    procedure InitializeWnd; virtual; //gets called after the window is created
    procedure DestroyWnd; virtual;
    procedure UpdateShowing; virtual;
    procedure ShowControl(AControl: TControl); virtual;
    procedure WndProc(var Message : TLMessage); override;
    function  DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; dynamic;
    function  DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; dynamic;
    function  DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; dynamic;
    function  DoKeyDown(var Message: TLMKey): Boolean;
    function  DoKeyPress(var Message: TLMKey): Boolean;
    function  DoKeyUp(var Message: TLMKey): Boolean;
    function  GetClientOrigin: TPoint; override;
    function  GetClientRect: TRect; override;
    function  GetDeviceContext(var WindowHandle: HWnd): HDC; override;
    function  IsControlMouseMsg(var TheMessage : TLMMouse): Boolean;
    procedure SetZOrderPosition(Position: Integer); override;
    procedure SetZOrder(Topmost: Boolean); override;

    property  BorderWidth : TBorderWidth read FBorderWidth write SetBorderWidth default 0;
    property  DefWndProc: Pointer read FDefWndProc write FDefWndPRoc;
    property  IsResizing : Boolean read GetIsResizing;

    property  ParentCtl3D : Boolean read FParentCtl3D write SetParentCtl3d default True;
    { events }
    property  OnEnter : TNotifyEvent read FOnEnter write FOnEnter;
    property  OnExit  : TNotifyEvent read FOnExit write FOnExit;
    property  OnKeyDown: TKeyEvent read FOnKeyDown write FOnKeyDown;
    property  OnKeyPress: TKeyPressEvent read FOnKeyPress write FOnKeyPress;
    property  OnKeyUp: TKeyEvent read FOnKeyUp write FOnKeyUp;
    property  OnMouseWheel: TMouseWheelEvent read FOnMouseWheel write FOnMouseWheel;
    property  OnMouseWheelDown: TMouseWheelUpDownEvent read FOnMouseWheelDown write FOnMouseWheelDown;
    property  OnMouseWheelUp: TMouseWheelUpDownEvent read FOnMouseWheelUp write FOnMouseWheelUp;
  public
    constructor Create(AOwner: TComponent);override;
    constructor CreateParented(ParentWindow: HWnd);
    class function CreateParentedControl(ParentWindow: HWnd): TWinControl;
    destructor Destroy; override;
    procedure BeginUpdateBounds;
    procedure EndUpdateBounds;
    Function CanFocus : Boolean;
    Function ControlAtPos(const Pos : TPoint; AllowDisabled : Boolean): TControl;
    Function ControlAtPos(const Pos : TPoint;
      AllowDisabled, AllowWinControls: Boolean): TControl;
    Function ControlAtPos(const Pos : TPoint;
      AllowDisabled, AllowWinControls, OnlyClientAreas: Boolean): TControl;
    function GetControlIndex(AControl: TControl): integer;
    procedure SetControlIndex(AControl: TControl; NewIndex: integer);
    procedure DoAdjustClientRectChange;
    procedure InvalidateClientRectCache;
    Function Focused : Boolean; dynamic;
    Procedure BroadCast(var Message);
    Procedure DisableAlign;
    Procedure EnableAlign;
    Procedure Invalidate; override;
    Procedure RemoveControl(AControl : TControl);
    Procedure InsertControl(AControl : TControl);
    Procedure Insert(AControl : TControl);
    Procedure Remove(AControl : TControl);
    procedure SetBounds(aLeft, aTop, aWidth, aHeight : integer); override;
    procedure Hide;
    procedure Repaint; override;
    Procedure SetFocus; virtual;
    Function FindChildControl(ControlName : String) : TControl;
    function HandleAllocated : Boolean;
    procedure HandleNeeded;
    property BoundsLockCount: integer read FBoundsLockCount;
    property Brush: TBrush read FBrush;
    property Controls[Index: Integer]: TControl read GetControl;
    property ControlCount: Integer read GetControlCount;
    property Handle : HWND read GetHandle write SetHandle;
    property Showing : Boolean read FShowing;
    property TabStop : Boolean read FTabStop write FTabStop default false;
    property TabOrder : TTabOrder read GetTabOrder write SetTaborder default -1;
  end;


  { TScrolledWindow }

  TScrolledWindow = Class(TWinControl)
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy; override;
  end;
  
  
  { TGraphicControl }

  TGraphicControl = class(TControl)
  private
    FCanvas: TCanvas;
    //FOnPaint: TNotifyEvent;
    procedure WMPaint(var Message: TLMPaint); message LM_PAINT;
  protected
    procedure Paint; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas: TCanvas read FCanvas;
  end;


  { TCustomControl }

  TCustomControl = class(TWinControl)
  private
//   FOnPaint : TNotifyEvent;
    FCanvas: TCanvas;
  protected
    procedure WMPaint(var Message: TLMPaint); message LM_PAINT;
    Procedure Paint; virtual;
    property Canvas: TCanvas read FCanvas write FCanvas;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;
 

  { TImageList }
 
  TImageList = class(TDragImageList)
//  published
//  Property Height;
//  Property Width;
//    property Count : Integer read FCount;
  end;


  { TMouse }

  TMouse = class
    FCapture : HWND;
    FDragImmediate : Boolean;
    FDragThreshold : Integer;
    Procedure SetCapture(const Value : HWND);
    Function GetCapture : HWND;
    Procedure SetCursorPos(value : TPoint);
    Function GetCursorPos : TPoint;
  public
    constructor Create;
    destructor Destroy; override;
    property Capture : HWND read GetCapture write SetCapture;
    property CursorPos : TPoint read GetCursorPos write SetCursorPos;
    property DragImmediate : Boolean read FDragImmediate write FDragImmediate default True;
    property DragThreshold : Integer read FDragThreshold write FDragThreshold default 5;
  end;


const
  AnchorAlign: array[TAlign] of TAnchors = (
    { alNone }
    [akLeft, akTop],
    { alTop }
    [akLeft, akTop, akRight],
    { alBottom }
    [akLeft, akRight, akBottom],
    { alLeft }
    [akLeft, akTop, akBottom],
    { alRight }
    [akRight, akTop, akBottom],
    { alClient }
    [akLeft, akTop, akRight, akBottom]);

function CNSendMessage(LM_Message : integer; Sender : TObject; data : pointer) : integer;
Function FindDragTarget(const Pos : TPoint; AllowDisabled: Boolean): TControl;
Function FindLCLWindow(const Pos : TPoint) : TWinControl;
Function FindControl(Handle : hwnd) : TWinControl;
Procedure MoveWindowOrg(dc : hdc; X,Y : Integer);

procedure SetCaptureControl(Control : TControl);
function GetCaptureControl : TControl;
procedure CancelDrag;

var
  NewStyleControls : Boolean;
  Mouse : TMouse;

function CursorToString(Cursor: TCursor): string;
function StringToCursor(const S: string): TCursor;
procedure GetCursorValues(Proc: TGetStrProc);
function CursorToIdent(Cursor: Longint; var Ident: string): Boolean;
function IdentToCursor(const Ident: string; var Cursor: Longint): Boolean;

function GetKeyShiftState: TShiftState;


implementation


//uses clause
//Needs dialogs for the SetVisible procedure.
uses Forms, Dialogs, Interfaces, Math;

var
  CaptureControl: TControl;
  
  DragCapture : HWND;
  DragControl : TControl;
  DragFreeObject : Boolean;
  DragObject : TDragObject;
  //DragSaveCursor : HCURSOR;
  DragStartPos : TPoint;
  //DragThreshold : Integer;
  
{------------------------------------------------------------------------------}
{  CNSendMessage                                                               }
{------------------------------------------------------------------------------}
function CNSendMessage(LM_Message : integer; Sender : TObject;
  Data : pointer) : integer;
begin
  result := InterfaceObject.IntSendMessage3(LM_Message, Sender, Data);
end;

{------------------------------------------------------------------------------}
{  FindControl                                                                 }
{------------------------------------------------------------------------------}
function FindControl(Handle : hwnd) : TWinControl;
begin
  if Handle <> 0 
  then Result := TWinControl(GetProp(Handle,'WinControl'))
  else Result := nil;
end;

Procedure MoveWindowOrg(dc : hdc; X,Y : Integer);
begin
  MoveWindowOrgEx(dc,X,Y);
end;

function DoControlMsg(handle:hwnd; var Message) : Boolean;
var
  Control : TWinControl;
begin
  Result := False;
  Control := FindControl(handle);
  if Control <> nil then
    with TLMessage(Message) do
    Begin
      Control.Perform(Msg + CN_BASE,WParam, LParam);
      DoControlMsg := True;
    end;
end;


{DragIntit}
Procedure DragInit(aDragObject : TDragObject; Immediate : Boolean;
  Threshold : Integer);
Begin
  DragObject := ADragObject;
  DragObject.Dragtarget := nil;
  GetCursorPos(DragStartPos);
  DragObject.DragPos := DragStartPos;
  DragCapture := DragObject.Capture;
  //DragThreshold := Threshold;
  //save the cursor yet
end;

{Draginitcontrol}
Procedure DragInitControl(Control : TControl; Immediate : Boolean;
  Threshold : Integer);
var
  DragObject : TDragObject;
  ok: boolean;
begin
  DragControl := Control;
  ok:=false;
  try
    DragObject := nil;
    DragFreeObject := False;
    if Control.fDragKind = dkDrag then
    begin
      Control.DoStartDrag(DragObject);
      if DragControl = nil then Exit;
      if DragObject = nil then
      Begin
        DragObject := TDragControlObject.Create(Control);
        DragFreeObject := True;
      End;
    end;
    DragInit(DragObject,Immediate,Threshold);
    ok:=true;
  finally
    if not ok then
      DragControl := nil;
  end;
end;


Procedure DragTo(P : TPoint);
Begin
  Assert(False, 'Trace:********************************************');
  Assert(False, 'Trace:*******************D R A G T O***************');
  Assert(False, 'Trace:********************************************');
end;

Function DragMessage(Handle : HWND; Msg : TDragMessage; Source : TDragObject; Target : Pointer; const Pos : TPoint): longint;
var
  DragRec : TDragRec;
Begin
  Assert(False, 'Trace:******');
  Assert(False, 'Trace:DragMessage');

  Result := 0;
  if Handle <> 0 then Begin
    DragRec.Pos := Pos;
    DragRec.Target := Target;
    DragRec.Source := Source;
    DragRec.Docking := False;//TODO: not supported at this point
    Result := SendMessage(Handle, CM_DRAG,longint(msg),Longint(@DragRec));
  end;
end;


Procedure DragDone(Drop : Boolean);
var
  Accepted : Boolean;
  DragSave : TDragObject;
  DragMsg : TDragMEssage;
  TargetPos : TPoint;
Begin
  Assert(False, 'Trace:*************************');
  Assert(False, 'Trace:*********DRAGDONE********');

  Accepted:=false;
  if (DragObject = nil) or (DragObject.Cancelling) then Exit;
  DragSave := DragObject;
  try
    DragObject.Cancelling := True;
    DragObject.ReleaseCapture(DragCapture);

    if DragObject.DragTarget <> nil then
    Begin
      dragMsg := dmDragDrop;
      if not Accepted then
      begin
        DragMsg := dmDragCancel;
        DragSave.FDragPos.X := 0;
        DragSave.FDragPos.Y := 0;
        TargetPos.X := 0;
        TargetPos.Y := 0;
      end;
      DragMessage(DragSave.DragHandle,DragMsg,DragSave,
                 DragSave.DragTarget,DragSave.DragPos);
    end;
    DragSave.Cancelling := False;
    DragSave.Finished(TObject(DragSave.DragTarget),TargetPos.X,TargetPos.Y,Accepted);
    DragSave := nil;
  finally
    DragControl := nil;
  end;
  DragObject := nil;
  if DragFreeObject then DragSave.Free;
  DragFreeObject := False;
end;

{------------------------------------------------------------------------------
  Function: FindLCLWindow
  Params: 
  Returns:  

 ------------------------------------------------------------------------------}
function FindLCLWindow(const Pos : TPoint) : TWinControl;
var
  Handle : HWND;
begin
  Handle := WindowFromPoint(Pos);
  Result := nil;
  while Handle <> 0 do
  begin
    Assert(False, Format('Trace:[FindLCLWindow] Find handle: 0x%x', [Handle]));
    Result := FindControl(Handle);
    
    if Result <> nil 
    then begin
      Assert(False, Format('Trace:[FindLCLWindow] Control found: %s Handle: 0x%x', [Result.ClassName, Handle]));
      Exit;
    end;
    
    Handle := GetParent(Handle);
  end;
  Assert(False, 'Trace:[FindLCLWindow] Nothing found');
end;

{------------------------------------------------------------------------------
  Function: FindDragTarget
  Params: 
  Returns:  

 ------------------------------------------------------------------------------}
function FindDragTarget(const Pos : TPoint; AllowDisabled: Boolean): TControl;
var
  Window : TWinControl;
  Control : TControl;
begin
  Result := nil;
  Window := FindLCLWindow(Pos);
  if Window <> nil
  then begin
    Result := Window;
    Assert(False, Format('Trace:[FindDragTarget] Found VCL window: %s Handle: 0x%x', [Window.ClassName, Window.Handle]));

    Control := Window.ControlAtPos(Window.ScreenToClient(pos), AllowDisabled);

    if Control <> nil 
    then Assert(False, Format('Trace:[FindDragTarget] Control at pos(%d, %d): %s', [Pos.X,Pos.Y, Control.ClassName]));
    
    if Control <> nil then Result := Control;
  end;
end;

{------------------------------------------------------------------------------
  Function: GetCaptureControl
  Params: 
  Returns:  

 ------------------------------------------------------------------------------}
function GetCaptureControl : TControl;
begin
  Result := FindControl(GetCapture);
  if (Result <> nil) 
  and (CaptureControl <> nil) 
  and (CaptureControl.Parent = Result) 
  then Result := CaptureControl;
end;

procedure SetCaptureControl(Control : TControl);
begin
  {$IFDEF VerboseMouseCapture}
  write('SetCaptureControl');
  if CaptureControl<>nil then
    write(' Old=',CaptureControl.Name,':',CaptureControl.ClassName)
  else
    write(' Old=nil');
  if Control<>nil then
    write(' New=',Control.Name,':',Control.ClassName)
  else
    write(' New=nil');
  writeln('');
  {$ENDIF}
  ReleaseCapture;
  CaptureControl := nil;
  if Control <> nil 
  then begin
    if not (Control is TWinControl)
    then begin
      if Control.Parent = nil then Exit;
      
      CaptureControl := Control;
      Control := Control.Parent;
    end;
    SetCapture(TWinControl(Control).Handle);
  end;
end;

procedure CancelDrag;
begin
  if DragObject <> nil then DragDone(False);
  DragControl := nil;
end;

function GetKeyShiftState: TShiftState;
begin
  Result:=[];
  if GetKeyState(VK_CONTROL)<>0 then
    Include(Result,ssCtrl);
  if GetKeyState(VK_SHIFT)<>0 then
    Include(Result,ssShift);
  if GetKeyState(VK_MENU)<>0 then
    Include(Result,ssAlt);
end;

{ Cursor translation function }

const
  DeadCursors = 1;

const
  Cursors: array[0..21] of TIdentMapEntry = (
    (Value: crDefault;      Name: 'crDefault'),
    (Value: crArrow;        Name: 'crArrow'),
    (Value: crCross;        Name: 'crCross'),
    (Value: crIBeam;        Name: 'crIBeam'),
    (Value: crSizeNESW;     Name: 'crSizeNESW'),
    (Value: crSizeNS;       Name: 'crSizeNS'),
    (Value: crSizeNWSE;     Name: 'crSizeNWSE'),
    (Value: crSizeWE;       Name: 'crSizeWE'),
    (Value: crUpArrow;      Name: 'crUpArrow'),
    (Value: crHourGlass;    Name: 'crHourGlass'),
    (Value: crDrag;         Name: 'crDrag'),
    (Value: crNoDrop;       Name: 'crNoDrop'),
    (Value: crHSplit;       Name: 'crHSplit'),
    (Value: crVSplit;       Name: 'crVSplit'),
    (Value: crMultiDrag;    Name: 'crMultiDrag'),
    (Value: crSQLWait;      Name: 'crSQLWait'),
    (Value: crNo;           Name: 'crNo'),
    (Value: crAppStart;     Name: 'crAppStart'),
    (Value: crHelp;         Name: 'crHelp'),
    (Value: crHandPoint;    Name: 'crHandPoint'),
    (Value: crSizeAll;      Name: 'crSizeAll'),

    { Dead cursors }
    (Value: crSize;         Name: 'crSize'));

function CursorToString(Cursor: TCursor): string;
begin
  if not CursorToIdent(Cursor, Result) then FmtStr(Result, '%d', [Cursor]);
end;

function StringToCursor(const S: string): TCursor;
var
  L: Longint;
begin
  if not IdentToCursor(S, L) then L := StrToInt(S);
  Result := L;
end;

procedure GetCursorValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := Low(Cursors) to High(Cursors) - DeadCursors do Proc(Cursors[I].Name);
end;

function CursorToIdent(Cursor: Longint; var Ident: string): Boolean;
begin
  Result := IntToIdent(Cursor, Ident, Cursors);
end;

function IdentToCursor(const Ident: string; var Cursor: Longint): Boolean;
begin
  Result := IdentToInt(Ident, Cursor, Cursors);
end;



// turn off before includes !!
{$IFDEF ASSERT_IS_ON}
  {$UNDEF ASSERT_IS_ON}
  {$C-}
{$ENDIF}

{$I sizeconstraints.inc}
{$I BaseDragControlObject.inc}
{$I controlsproc.inc}
{$I controlcanvas.inc}
{$I scrolledwindow.inc}
{$I wincontrol.inc}
{$I control.inc}
{$I graphiccontrol.inc}
{$I customcontrol.inc}
{$I mouse.inc}
{$I dragobject.inc}

initialization

  //writeln('controls.pp - initialization');
  Mouse := TMouse.create;
  DragControl := nil;
  CaptureControl := nil;
  
  RegisterIntegerConsts(TypeInfo(TCursor), @IdentToCursor, @CursorToIdent);

finalization
  Mouse.Free;
  
end.

{ =============================================================================

  $Log$
  Revision 1.71  2002/09/09 19:04:01  lazarus
  MG: started TTreeView dragging

  Revision 1.70  2002/09/09 14:01:05  lazarus
  MG: improved TScreen and ShowModal

  Revision 1.69  2002/09/08 10:01:59  lazarus
  MG: fixed streaming visible=false

  Revision 1.68  2002/09/06 22:32:20  lazarus
  Enabled cursor property + property editor.

  Revision 1.67  2002/09/05 12:11:42  lazarus
  MG: TNotebook is now streamable

  Revision 1.66  2002/09/03 08:07:17  lazarus
  MG: image support, TScrollBox, and many other things from Andrew

  Revision 1.65  2002/09/02 19:10:28  lazarus
  MG: TNoteBook now starts with no Page and TPage has no auto names

  Revision 1.64  2002/09/01 16:11:21  lazarus
  MG: double, triple and quad clicks now works

  Revision 1.63  2002/08/31 18:45:54  lazarus
  MG: added some property editors and started component editors

  Revision 1.62  2002/08/30 12:32:20  lazarus
  MG: MoveWindowOrgEx, Splitted FWinControls/FControls, TControl drawing, Better DesignerDrawing, ...

  Revision 1.61  2002/08/30 06:46:03  lazarus

  Use comboboxes. Use history. Prettify the dialog. Preselect text on show.
  Make the findreplace a dialog. Thus removing resiying code (handled by Anchors now anyway).
  Make Anchors work again and publish them for various controls.
  SelStart and Co. for TEdit, SelectAll procedure for TComboBox and TEdit.
  Clean up and fix some bugs for TComboBox, plus selection stuff.

  Revision 1.60  2002/08/24 12:54:59  lazarus
  MG: fixed mouse capturing, OI edit focus

  Revision 1.59  2002/08/23 19:00:15  lazarus
  MG: implemented Ctrl+Mouse links in source editor

  Revision 1.58  2002/08/22 16:22:39  lazarus
  MG: started debugging of mouse capturing

  Revision 1.57  2002/08/17 15:45:32  lazarus
  MG: removed ClientRectBugfix defines

  Revision 1.56  2002/08/07 09:55:29  lazarus
  MG: codecompletion now checks for filebreaks, savefile now checks for filedate

  Revision 1.55  2002/08/06 09:32:48  lazarus
  MG: moved TColor definition to graphtype.pp and registered TColor names

  Revision 1.54  2002/07/09 17:18:22  lazarus
  MG: fixed parser for external vars

  Revision 1.53  2002/06/21 15:41:56  lazarus
  MG: moved RectVisible, ExcludeClipRect and IntersectClipRect to interface dependent functions

  Revision 1.52  2002/06/19 19:46:08  lazarus
  MG: Form Editing: snapping, guidelines, modified on move/resize, creating components in csDesigning, ...

  Revision 1.51  2002/06/04 15:17:21  lazarus
  MG: improved TFont for XLFD font names

  Revision 1.50  2002/05/30 21:19:26  lazarus

  + implemented HasParent for TControl & changed TCustomForm.GetChildren
    accordingly (sorry, control.inc & customform.inc got wrong comment:-( )
    stoppok

  Revision 1.49  2002/05/24 07:16:31  lazarus
  MG: started mouse bugfix and completed Makefile.fpc

  Revision 1.48  2002/05/20 14:19:03  lazarus
  MG: activated the clientrect bugfixes

  Revision 1.47  2002/05/10 06:05:49  lazarus
  MG: changed license to LGPL

  Revision 1.46  2002/05/09 12:41:28  lazarus
  MG: further clientrect bugfixes

  Revision 1.45  2002/05/06 08:50:36  lazarus
  MG: replaced logo, increased version to 0.8.3a and some clientrectbugfix

  Revision 1.44  2002/04/24 16:11:17  lazarus
  MG: started new client rectangle

  Revision 1.43  2002/04/24 09:29:06  lazarus
  MG: fixed typos

  Revision 1.42  2002/04/22 13:07:44  lazarus
  MG: fixed AdjustClientRect of TGroupBox

  Revision 1.41  2002/04/21 06:53:54  lazarus
  MG: fixed save lrs to test dir

  Revision 1.40  2002/04/18 08:13:36  lazarus
  MG: added include comments

  Revision 1.39  2002/04/18 08:09:03  lazarus
  MG: added include comments

  Revision 1.38  2002/04/04 12:25:01  lazarus
  MG: changed except statements to more verbosity

  Revision 1.37  2002/03/31 23:20:37  lazarus
  MG: fixed initial size of TPage

  Revision 1.36  2002/03/29 17:12:52  lazarus
  MG: added Triple and Quad mouse clicks to lcl and synedit

  Revision 1.35  2002/03/25 17:59:19  lazarus
  GTK Cleanup
  Shane

  Revision 1.34  2002/03/16 21:40:54  lazarus
  MG: reduced size+move messages between lcl and interface

  Revision 1.33  2002/03/14 23:25:51  lazarus
  MG: fixed TBevel.Create and TListView.Destroy

  Revision 1.32  2002/03/13 22:48:16  lazarus
  Constraints implementation (first cut) and sizig - moving system rework to
  better match Delphi/Kylix way of doing things (the existing implementation
  worked by acident IMHO :-)

  Revision 1.31  2002/02/03 00:24:00  lazarus
  TPanel implemented.
  Basic graphic primitives split into GraphType package, so that we can
  reference it from interface (GTK, Win32) units.
  New Frame3d canvas method that uses native (themed) drawing (GTK only).
  New overloaded Canvas.TextRect method.
  LCLLinux and Graphics was split, so a bunch of files had to be modified.

  Revision 1.30  2002/01/04 21:07:49  lazarus
  MG: added TTreeView

  Revision 1.29  2002/01/01 18:38:36  lazarus
  MG: more wmsize messages :(

  Revision 1.28  2002/01/01 15:50:13  lazarus
  MG: fixed initial component aligning

  Revision 1.27  2001/12/08 08:54:45  lazarus
  MG: added TControl.Refresh

  Revision 1.26  2001/12/05 17:23:44  lazarus
  Added Calendar component
  Shane

  Revision 1.25  2001/11/10 10:48:00  lazarus
  MG: fixed set formicon on invisible forms

  Revision 1.24  2001/11/09 19:14:23  lazarus
  HintWindow changes
  Shane

  Revision 1.23  2001/10/31 16:29:21  lazarus
  Fixed the gtk mousemove bug where the control gets the coord's based on it's parent instead of itself.
  Shane

  Revision 1.22  2001/10/07 07:28:32  lazarus
  MG: fixed setpixel and TCustomForm.OnResize event

  Revision 1.21  2001/09/30 08:34:49  lazarus
  MG: fixed mem leaks and fixed range check errors

  Revision 1.20  2001/06/14 14:57:58  lazarus
  MG: small bugfixes and less notes

  Revision 1.19  2001/05/13 22:07:08  lazarus
  Implemented BringToFront / SendToBack.

  Revision 1.18  2001/03/27 21:12:53  lazarus
  MWE:
    + Turned on longstrings
    + modified memotest to add lines

  Revision 1.17  2001/03/26 14:58:31  lazarus
  MG: setwindowpos + bugfixes

  Revision 1.16  2001/03/19 14:00:50  lazarus
  MG: fixed many unreleased DC and GDIObj bugs

  Revision 1.14  2001/03/12 12:17:01  lazarus
  MG: fixed random function results

  Revision 1.13  2001/02/20 16:53:27  lazarus
  Changes for wordcompletion and many other things from Mattias.
  Shane

  Revision 1.12  2001/02/04 04:18:12  lazarus
  Code cleanup and JITFOrms bug fix.
  Shane

  Revision 1.11  2001/02/01 16:45:19  lazarus
  Started the code completion.
  Shane

  Revision 1.10  2001/01/23 23:33:54  lazarus
  MWE:
    - Removed old LM_InvalidateRect
    - did some cleanup in old  code
    + added some comments  on gtkobject data (gtkproc)

  Revision 1.9  2000/12/29 13:14:05  lazarus
  Using the lresources.pp and registering components.
  This is a major change but will create much more flexibility for the IDE.
  Shane

  Revision 1.8  2000/12/22 19:55:37  lazarus
  Added the Popupmenu code to the LCL.
  Now you can right click on the editor and a PopupMenu appears.
  Shane

  Revision 1.7  2000/12/20 17:35:58  lazarus
  Added GetChildren
  Shane

  Revision 1.6  2000/12/01 15:50:39  lazarus
  changed the TCOmponentInterface SetPropByName.  It works for a few properties, but not all.
  Shane

  Revision 1.5  2000/11/30 21:43:38  lazarus
  Changed TDesigner.  It's now notified when a control is added to it's CustomForm.
  It's created in main.pp when New Form is selected.

  Shane

  Revision 1.3  2000/11/27 18:52:37  lazarus
  Added the Object Inspector code.
  Added more form editor code.
  Shane

  Revision 1.2  2000/07/30 21:48:32  lazarus
  MWE:
    = Moved ObjectToGTKObject to GTKProc unit
    * Fixed array checking in LoadPixmap
    = Moved LM_SETENABLED to API func EnableWindow and EnableMenuItem
    ~ Some cleanup

  Revision 1.1  2000/07/13 10:28:23  michael
  + Initial import

  Revision 1.92  2000/07/09 20:18:55  lazarus
  MWE:
    + added new controlselection
    + some fixes
    ~ some cleanup

  Revision 1.91  2000/06/28 13:11:37  lazarus
  Fixed TNotebook so it gets page change events.  Shane

  Revision 1.90  2000/06/16 13:33:21  lazarus
  Created a new method for adding controls to the toolbar to be dropped onto the form!
  Shane

  Revision 1.89  2000/05/27 22:20:55  lazarus
  MWE & VRS:
    + Added new hint code

  Revision 1.88  2000/05/23 21:41:10  lazarus
  MWE:
    * Fixed (one ?) crash on close: Mouse is created/freed twice.
      Thanks to Vincent Snijders pointing at this.

  Revision 1.87  2000/05/14 21:56:11  lazarus
  MWE:
    + added local messageloop
    + added PostMessage
    * fixed Peekmessage
    * fixed ClientToScreen
    * fixed Flat style of Speedutton (TODO: Draw)
    + Added TApplicatio.OnIdle

  Revision 1.86  2000/05/10 22:52:57  lazarus
  MWE:
    = Moved some global api stuf to gtkobject

  Revision 1.85  2000/05/09 18:37:02  lazarus
  *** empty log message ***

  Revision 1.84  2000/05/09 12:52:02  lazarus
  *** empty log message ***

  Revision 1.83  2000/05/09 00:38:10  lazarus
  Changed writelns to Asserts.                          CAW

  Revision 1.82  2000/05/08 16:07:32  lazarus
  fixed screentoclient and clienttoscreen
  Shane

  Revision 1.80  2000/04/18 21:03:13  lazarus
  Added
  TControl.bringtofront
  Shane

  Revision 1.79  2000/04/18 14:02:32  lazarus
  Added Double Clicks.  Changed the callback in gtkcallback for the buttonpress event to check the event type.
  Shane

  Revision 1.78  2000/04/17 19:50:05  lazarus
  Added some compiler stuff built into Lazarus.
  This depends on the path to your compiler being correct in the compileroptions
  dialog.
  Shane

  Revision 1.77  2000/04/13 21:25:16  lazarus
  MWE:
    ~ Added some docu and did some cleanup.
  Hans-Joachim Ott <hjott@compuserve.com>:
    * TMemo.Lines works now.
    + TMemo has now a property Scrollbar.
    = TControl.GetTextBuf revised :-)
    + Implementation for CListBox columns added
    * Bug in TGtkCListStringList.Assign corrected.

  Revision 1.76  2000/04/10 15:05:30  lazarus
  Modified the way the MOuseCapture works.
  Shane

  Revision 1.74  2000/04/07 16:59:54  lazarus
  Implemented GETCAPTURE and SETCAPTURE along with RELEASECAPTURE.
  Shane

  Revision 1.73  2000/03/30 18:07:53  lazarus
  Added some drag and drop code
  Added code to change the unit name when it's saved as a different name.  Not perfect yet because if you are in a comment it fails.

  Shane

  Revision 1.72  2000/03/22 20:40:43  lazarus
  Added dragobject shell

  Revision 1.71  2000/03/20 20:08:33  lazarus
  Added a generic MOUSE class.
  Shane

  Revision 1.70  2000/03/15 20:15:31  lazarus
  MOdified TBitmap but couldn't get it to work
  Shane

  Revision 1.69  2000/03/15 00:51:57  lazarus
  MWE:
    + Added LM_Paint on expose
    + Added forced creation of gdkwindow if needed
    ~ Modified DrawFrameControl
    + Added BF_ADJUST support on DrawEdge
    - Commented out LM_IMAGECHANGED in TgtkObject.IntSendMessage3
       (It did not compile)

  Revision 1.68  2000/03/14 19:49:04  lazarus
  Modified the painting process for TWincontrol.  Now it runs throug it's FCONTROLS list and paints all them
  Shane

  Revision 1.67  2000/03/10 18:31:09  lazarus
  Added TSpeedbutton code
  Shane

  Revision 1.66  2000/03/06 00:05:05  lazarus
  MWE: Added changes from Peter Dyson <peter@skel.demon.co.uk> for a new
    release of mwEdit (0.92)

  Revision 1.65  2000/02/28 00:15:54  lazarus
  MWE:
    Fixed creation of visible componets at runtime. (when a new editor
      was created it didn't show up)
    Made the hiding/showing of controls more delphi compatible

  Revision 1.64  2000/02/24 21:15:30  lazarus
  Added TCustomForm.GetClientRect and RequestAlign to try and get the controls to align correctly when a MENU is present.  Not Complete yet.

  Fixed the bug in TEdit that caused it not to update it's text property.  I will have to
  look at TMemo to see if anything there was affected.

  Added SetRect to WinAPI calls
  Added AdjustWindowRectEx to WINAPI calls.
  Shane

  Revision 1.63  2000/02/22 22:19:49  lazarus
  TCustomDialog is a descendant of TComponent.
  Initial cuts a form's proper Close behaviour.

  Revision 1.62  2000/02/22 21:51:40  lazarus
  MWE: Removed some double (or triple) event declarations.
       The latest compiler doesn't like it

  Revision 1.61  2000/02/22 17:32:49  lazarus
  Modified the ShowModal call.
  For TCustomForm is simply sets the visible to true now and adds fsModal to FFormState.  In gtkObject.inc FFormState is checked.  If it contains fsModal then either gtk_grab_add or gtk_grab_remove is called depending on the value of VISIBLE.

  The same goes for TCustomDialog (open, save, font, color).
  I moved the Execute out of the individual dialogs and moved it into TCustomDialog and made it virtual because FONT needs to set some stuff before calling the inherited execute.
  Shane

  Revision 1.60  2000/02/19 18:11:59  lazarus
  More work on moving, resizing, forms' border style etc.

  Revision 1.59  2000/02/18 19:38:52  lazarus
  Implemented TCustomForm.Position
  Better implemented border styles. Still needs some tweaks.
  Changed TComboBox and TListBox to work again, at least partially.
  Minor cleanups.

  Revision 1.58  2000/01/18 21:47:00  lazarus
  Added OffSetRec

  Revision 1.57  2000/01/10 00:07:12  lazarus
  MWE:
    Added more scrollbar support for TWinControl
    Most signals for TWinContorl are jet connected to the wrong widget
      (now scrolling window, should be fixed)
    Added some cvs entries


}

