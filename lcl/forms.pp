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
  Classes, Controls, VCLGlobals, SysUtils, GraphType, Graphics, Menus,
  LCLLinux, LCLType, LMessages, CustomTimer, StdCtrls;

type
  TPosition = (poDesigned, poDefault, poDefaultPosOnly, poDefaultSizeOnly,
          poScreenCenter, poDesktopCenter, poMainFormCenter, poOwnerFormCenter);

  TWindowState = (wsNormal, wsMinimized, wsMaximized);
  TCloseAction = (caNone, caHide, caFree, caMinimize);

  TScrollingWinControl = class;

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

    procedure SetPosition(Value: Integer);
    procedure SetRange(Value: Integer);
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
    property Visible: Boolean read FVisible write SetVisible;// default True;
  end;

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
    property HorzScrollBar: TControlScrollBar read FHorzScrollBar write SetHorzScrollBar stored StoreScrollBars;
    property VertScrollBar: TControlScrollBar read FVertScrollBar write SetVertScrollBar stored StoreScrollBars;
  end;

  TScrollBox = class(TScrollingWinControl)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property Anchors;
    property AutoSize default True;
    //property AutoScroll;
    //property BiDiMode;
    //property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
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

  TCloseEvent = procedure(Sender: TObject; var Action: TCloseAction) of object;
  TCloseQueryEvent = procedure(Sender : TObject; var CanClose : boolean) of object;
  TFormState = set of (fsCreating, fsVisible, fsShowing, fsModal, fsCreatedMDIChild);
  TModalResult = low(Integer)..high(Integer);

  TCustomForm = class(TScrollingWinControl)
  private
    FActive : Boolean;
    FActiveControl : TWinControl;
    FBorderStyle : TFormBorderStyle;
    FDesigner : TIDesigner;
    FFormStyle : TFormStyle;
    FFormState: TFormState;
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
    procedure SetActiveControl(Value : TWinControl);
    procedure SetBorderStyle(Value : TFORMBorderStyle);
    procedure SetDesigner(Value : TIDesigner);
    procedure SetMenu(Value : TMainMenu);
    procedure SetFormStyle(Value : TFormStyle);
    procedure SetIcon(AValue: TIcon);
    procedure SetPosition(Value : TPosition);
    procedure SetVisible(Value: boolean);
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
    procedure WMDestroy(var message: TLMDestroy); message LM_DESTROY;
  protected
    procedure Activate; dynamic;
    function CloseQuery : boolean; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Loaded; override;
    procedure Deactivate;dynamic;
    procedure DoClose(var Action: TCloseAction); dynamic;
    procedure DoHide; dynamic;
    procedure DoShow; dynamic;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    // Delphi needed GetClientRect for window specific things, LCL not
    // Function GetClientRect : TRect ; Override;
    procedure Notification(AComponent: TComponent; Operation : TOperation);override;
    procedure PaintWindow(dc : Hdc); override;
    procedure RequestAlign; override;
    procedure UpdateShowing; override;
    procedure UpdateWindowState;
    procedure ValidateRename(AComponent: TComponent;
                             const CurName, NewName: string);override;
    procedure WndProc(var TheMessage : TLMessage); override;
    property TextHeight : Longint read FDummyTextHeight write FDummyTextHeight stored False;
    {events}
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
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateNew(AOwner: TComponent; Num : Integer); virtual;
    procedure BeforeDestruction; override;
    function GetIconHandle: HICON;
    destructor Destroy; override;
    procedure Close;
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

  TForm = class(TCustomForm)
  private
    FClientHandle: HWND;
    FDummyPPI : longint;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ClientHandle: HWND read FClientHandle;
  published
    property PixelsPerInch : Longint read FDummyPPI write FDummyPPI stored False;
    property ActiveCOntrol;
    property Align;
    property AutoSize;
    property BorderStyle;
    property Caption;
    property Color default clBtnFace;
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
  

  {THintWindow}
  
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
    property Color;
    property AutoHide : Boolean read FAutoHide write SetAutoHide;
    property HideInterval : Integer read FHideInterval write SetHideInterval;
  end;


  TScreen = class(TComponent)
  private
    FFocusedForm: TCustomForm;
    FFormList: TList;
    FHintFont : TFont;
    FPixelsPerInch : integer;
    FSaveFocusedList: TList;
    FFonts : TStrings;

    function GetFonts : TStrings;
    function GetFormCount: Integer;
    function GetForms(IIndex: Integer): TForm;
    function GetHeight : Integer;
    function GetWidth : Integer;
    procedure AddForm(FForm: TCustomForm);
    procedure RemoveForm(FForm: TCustomForm);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; Override;
    property FormCount: Integer read GetFormCount;
    property Forms[Index: Integer]: TForm read GetForms;
    property Fonts : TStrings read GetFonts;
    property PixelsPerInch : integer read FPixelsPerInch;
    property HintFont : TFont read FHintFont;
    property Height : Integer read Getheight;
    property Width : Integer read GetWidth;
  end;

  TExceptionEvent = procedure (Sender: TObject; E: Exception) of object;
  TIdleEvent = procedure (Sender: TObject; var Done: Boolean) of object;

  TApplication = class(TComponent)
  private
    FHandle : THandle;
    FIcon: TIcon;
    FList: TList;
    FMainForm : TForm;
    FMouseControl: TControl;
    FOnException: TExceptionEvent;
    FOnIdle: TIdleEvent;
    FTerminate : Boolean;
    FTitle : String;
  // MWE:Do we need this ??
    // function ProcessMessage(Var Msg : TMsg) : Boolean;
    procedure wndproc(var Message : TLMessage);
    function GetExename: String;
    function GetIconHandle: HICON;
    function GetTitle: string;
    procedure IconChanged(Sender: TObject);
    procedure Idle;
    procedure MouseIdle(const CurrentControl: TControl);
    procedure SetIcon(AValue: TIcon);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ControlDestroyed(AControl: TControl);
    Procedure BringToFront;
    procedure CreateForm(NewForm : TFormClass; var ref);
    procedure HandleException(Sender: TObject);
    procedure HandleMessage;
    procedure HintMouseMEssage(Control : TControl; var Message: TLMessage);
    property Icon: TIcon read FIcon write SetIcon;
    procedure Initialize;
    function MessageBox(Text, Caption : PChar; Flags : Longint) : Integer;
    procedure Notification(AComponent : TComponent; Operation : TOperation); override;
    Procedure ProcessMessages;
    procedure Run;
    procedure ShowException(E: Exception);
    procedure Terminate;
    property Exename: String read GetExeName;
    property Handle: THandle read FHandle;
    property Terminated: Boolean read FTerminate;
    property MainForm: TForm read FMainForm;
    property OnException: TExceptionEvent read FOnException write FOnException;
    property OnIdle: TIdleEvent read FOnIdle write FOnIdle;
    property Title: String read GetTitle write FTitle;
  end;

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


  TProcedure = procedure;


function KeysToShiftState(Keys:Word): TShiftState;
function KeyDataToShiftState(KeyData: Longint): TShiftState;

type
  TFocusState = type Pointer;

function SaveFocusState: TFocusState;
procedure RestoreFocusState(FocusState: TFocusState);

function GetParentForm(Control:TControl): TCustomForm;
function FindRootDesigner(AComponent: TComponent): TIDesigner;

function IsAccel(VK : Word; const Str : ShortString): Boolean;

function InitResourceComponent(Instance: TComponent; RootAncestor: TClass):Boolean;


var
  Application : TApplication;
  Screen : TScreen;
  ExceptionObject : TExceptObject;

type
  TMessageBoxFunction =
    function(Text, Caption : PChar; Flags : Longint) : Integer;
var
  MessageBoxFunction: TMessageBoxFunction;

implementation


uses 
  Interfaces, LResources, Math;

const
  FocusMessages : Boolean = true;
  FocusCount: Integer = 0;

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

//==============================================================================


{$I scrollingwincontrol.inc}
{$I scrollbox.inc}
{$I form.inc}
{$I customform.inc}
{$I screen.inc}
{$I application.inc}
{$I hintwindow.inc}


initialization
  Screen:= TScreen.Create(nil);
  Application:= TApplication.Create(nil);
  Focusmessages := True;

finalization
  writeln('forms.pp - finalization section');
  Application.Free;
  Application:= nil;
  Screen.Free;
  Screen:= nil;  

end.


