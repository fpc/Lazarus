{
 /***************************************************************************
                                  forms.pp
                             -------------------
                             Component Library Code

                             Implements:
                             TForm
                             TApplication

                   Initial Revision  : Sun Mar 28 23:15:32 CST 1999
                   Revised : Sat Jul 15 1999

 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
}



unit Forms;

{$mode objfpc}{$H+}

interface

{$ifdef Trace}
{$ASSERTIONS ON}
{$endif}

uses classes, controls, vclglobals, sysutils,graphics,Menus, LCLLinux,LMessages;

type
  TFormStyle = (fsNormal, fsMDIChild, fsMDIFORM, fsStayOnTop);
  TFormBorderStyle = (bsNone, bsSingle, bsSizeable, bsDialog, bsToolWindow, bsSizeToolWin);
  TBorderStyle = bsNone..bsSingle;

  TPosition = (poDesigned, poDefault, poDefaultPosOnly, poDefaultSizeOnly, poScreenCenter, poDesktopCenter, poMainFormCenter, poOwnerFormCenter);

  TWindowState = (wsNormal, wsMinimized, wsMaximized);
  TCloseAction = (caNone, caHide, caFree, caMinimize);

  TScrollBarKind = (sbHorizontal, sbVertical);
  TScrollBarInc = 1..32768;
  TScrollBarStyle = (ssRegular, ssFlat, ssHotTrack);

  TControlScrollBar = class(TPersistent)
  end;

  TScrollingWinControl = class(TWinControl)
  private
    //FHorzScrollBar : TControlScrollBar;
    //FVertScrollBar : TControlScrollBar;
    //FAutoScroll    : Boolean;
  end;

  TIDesigner = class;

    
  TCloseEvent = procedure(Sender: TObject; var Action: TCloseAction) of object;
  TCloseQueryEvent = procedure(Sender : TObject; var CanClose : boolean) of object;
  TFormState = set of (fsCreating, fsVisible, fsShowing, fsModal, fsCreatedMDIChild);
  TModalResult = low(Integer)..high(Integer);

  TCustomForm = class(TWinControl)
  private
    FActive : Boolean;
    FActiveControl : TWinControl;
    FBorderStyle : TFormBorderStyle;
    FCanvas : TControlCanvas;
    FDesigner : TIDesigner;
    FFormStyle : TFormStyle;
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
    FOnPaint: TNotifyEvent;
    FOnClose: TCloseEvent;
    FOnCloseQuery : TCloseQueryEvent;
    FPosition : TPosition;
    FWindowState : TWindowState;
    Procedure ClientWndProc(var Message: TLMessage);
    procedure DoCreate;
    procedure DoDestroy;
    Procedure SetActiveControl(Value : TWinControl);
    Procedure SetBorderStyle(Value : TFORMBorderStyle);
    Procedure SetDesigner(Value : TIDesigner);
    Procedure SetMenu(Value : TMainMenu);
    Procedure SetFormStyle(Value : TFormStyle);
    procedure SetIcon(AValue: TIcon);
    Procedure SetPosition(Value : TPosition);
    Procedure SetVisible(Value: boolean);
    Procedure SetWindowState(Value : TWIndowState);
    Function GetCanvas: TControlCanvas;
    Function IsForm : Boolean;
    procedure IconChanged(Sender: TObject);
    function IsIconStored: Boolean;
    function GetIconHandle: HICON;
    { events }
    Procedure WMActivate(var Message : TLMActivate); message LM_Activate;
    procedure WMPaint(var message: TLMPaint); message LM_PAINT;
    procedure WMSize(var message: TLMSize); message LM_Size;
    procedure WMShowWindow(var message: TLMShowWindow); message LM_SHOWWINDOW;
    procedure WMCloseQuery(var message: TLMessage); message LM_CLOSEQUERY;
    procedure WMDestroy(var message: TLMDestroy); message LM_DESTROY;
  protected
    FFormState: TFormState;
    procedure AttachSignals; override;
    function CloseQuery : boolean; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    Procedure DeActivate; dynamic;
    procedure DoClose(var Action: TCloseAction); dynamic;
    procedure DoHide; dynamic;
    procedure DoShow; dynamic;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    Function GetClientRect : TRect ; Override;
    Procedure Notification(AComponent: TComponent; Operation : TOperation);override;
    procedure Paint; dynamic;
    Procedure PaintWindow(dc : Hdc); override;
    Procedure RequestAlign; Override;
    procedure UpdateShowing; override;
    procedure UpdateWindowState;
    procedure ValidateRename(AComponent: TComponent; const CurName, NewName: shortstring);
    procedure WndProc(var Message : TLMessage); override;
    {events}
    property ActiveControl : TWinControl read FActiveControl write SetActiveControl;
    property Icon: TIcon read FIcon write SetIcon stored IsIconStored;
    property FormStyle : TFormStyle read FFormStyle write SetFormStyle default fsNormal;
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    property OnClose: TCloseEvent read FOnClose write FOnClose stored IsForm;
    property OnCloseQuery : TCloseQueryEvent read FOnCloseQuery write FOnCloseQuery stored IsForm;
    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
    property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property OnHide: TNotifyEvent read FOnHide write FOnHide;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnResize stored IsForm;
    property Position : TPosition read FPosition write SetPosition default poDesigned;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateNew(AOwner: TComponent; Num : Integer); virtual;
    procedure BeforeDestruction; override;
    destructor Destroy; override;
    procedure Close;
    procedure Hide;
    function WantChildKey(Child : TControl; var MEssage : TLMessage): Boolean; virtual;
    Procedure SetFocus; override;
    function SetFocusedControl(Control : TWinControl): Boolean ; Virtual;
    Procedure FocusControl(Control : TWinControl);
    Function  ShowModal : Integer;
    property Active : Boolean read FActive;
    property BorderStyle : TFormBorderStyle read FBorderStyle write SetBorderStyle default bsSizeable;
    property Canvas: TControlCanvas read GetCanvas;
    property Caption stored IsForm;
    property Designer : TIDesigner read FDesigner write SetDesigner;
    property FormState : TFormState read FFormState;
    property KeyPreview: Boolean read FKeyPreview write FKeyPreview;
    property Menu : TMainMenu read FMenu write SetMenu;
    property ModalResult : TModalResult read FModalResult write FModalResult;
    property Visible write SetVisible default False;
    property WindowState: TWindowState read FWindowState write SetWIndowState default wsNormal;
  end;

  TForm = class(TCustomForm)
   private
      FClientHandle: HWND;
   public
      constructor Create(AOwner: TComponent); override;
      destructor destroy; override;
      property ClientHandle: HWND read FClientHandle;
   published
      property ActiveCOntrol;
      property Align;
      property AutoSize;
      property Caption;
      property Color;
      property ClientHeight;
      property ClientWidth;
      property Enabled;
      property FormStyle;
      property Icon;
      property PopupMenu;
      property Position;
      property ShowHint;
      property Visible;
//      property WindowState;
      property OnActivate;
      property OnCreate;
      property OnClose;
      property OnCloseQuery;
      property OnDeactivate;
      property OnDestroy;
      property OnShow;
      property OnHide;
      property OnPaint;
      property OnResize;
   end;

  TFormClass = class of TForm;

  TScreen = class(TComponent)
  private
    FFormList: TList;
    FPixelsPerInch : integer;
    Function GetFormCount: Integer;
    Function GetForms(IIndex: Integer): TForm;
    function GetHeight : Integer;
    function GetWidth : Integer;
    procedure AddForm(FForm: TCustomForm);
    procedure RemoveForm(FForm: TCustomForm);
  public
    constructor Create(AOwner : TComponent); override;
    Destructor Destroy; Override;
    property FormCount: Integer read GetFormCount;
    property Forms[Index: Integer]: TForm read GetForms;
    property PixelsPerInch : integer read FPixelsPerInch;
    property Height : Integer read Getheight;
    property Width : Integer read GetWidth;
  end;

  TIdleEvent = procedure (Sender: TObject; var Done: Boolean) of object;

  TApplication = class(TComponent)
   private
      FHandle : THandle;
      FIcon: TIcon;
      FList: TList;
      FMainForm : TForm;
      FMouseControl: TControl;
      FOnIdle: TIdleEvent;
      FTerminate : Boolean;
    // MWE:Do we need this ??
      // function ProcessMessage(Var Msg : TMsg) : Boolean;
      procedure wndproc(var Message : TLMessage);
 //the following is used for Messagebox button clicks.  Temporary until I figure out a better way.
      procedure DefaultOnClick(Sender : TObject);
 //----      
      function GetExename: String;
      function GetIconHandle: HICON;
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
      procedure HandleMessage;
      procedure HintMouseMEssage(Control : TControl; var Message: TLMessage);
      property Icon: TIcon read FIcon write SetIcon;
      procedure Initialize;
      function MessageBox(Text, Caption : PChar; Flags : Longint) : Integer;
      procedure Notification(AComponent : TComponent; Operation : TOperation); override;
      Procedure ProcessMessages;
      procedure Run;
      procedure Terminate;
      property Exename: String read GetExeName;
      property Handle: THandle read FHandle;
      property Terminated: Boolean read FTerminate;
      property MainForm: TForm read FMainForm;
      property OnIdle: TIdleEvent read FOnIdle write FOnIdle;
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
      const CurName, NewName: shortstring); virtual; abstract;
    end;




  TProcedure = procedure;



function KeysToShiftState(Keys:Word): TShiftState;
function KeyDataToShiftState(KeyData: Longint): TShiftState;
function GetParentForm(Control:TControl): TCustomForm;
function IsAccel(VK : Word; const Str : ShortString): Boolean;
function InitResourceComponent(Instance: TComponent; RootAncestor: TClass):Boolean;


var
  Application : TApplication;
  Screen : TScreen;



implementation


uses 
  buttons,stdctrls,interfaces,lresources {,designer};

var
  FocusMessages : Boolean; //Should set it to TRUE by defualt but fpc does not handle that yet.

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

function GetParentForm(Control:TControl): TCustomForm;
begin
  while Control.parent <> nil do
    Control := Control.Parent;
  if Control is TCustomForm 
  then Result := TCustomForm(Control) 
  else Result := nil;
end;

function IsAccel(VK : Word; const Str : ShortString): Boolean;
begin
  Result := true;
end;



//==============================================================================

function InitResourceComponent(Instance: TComponent;
  RootAncestor: TClass):Boolean;

  function InitComponent(ClassType: TClass): Boolean;
  var CompResource:TLResource;
    a:integer;
  begin
    Result:=false;
    if (ClassType=TComponent) or (ClassType=RootAncestor) then exit;
    if Assigned(ClassType.ClassParent) then
      Result:=InitComponent(ClassType.ClassParent);
    CompResource:=LazarusResources.Find(Instance.ClassName);
    if (CompResource = nil) or (CompResource.Value='') then exit;
//    Writeln('Compresource.value is '+CompResource.Value);
    if (ClassType.InheritsFrom(TForm))
    and (CompResource.ValueType<>'FORMDATA') then exit;
    with TMemoryStream.Create do
      try
        Write(CompResource.Value[1],length(CompResource.Value));
        Position:=0;
        writeln('Signature=',copy(CompResource.Value,1,4));
        Instance:=ReadComponent(Instance);
        // MG: workaround til Visible=true is default
        if Instance is TControl then
          for a:=0 to Instance.ComponentCount-1 do
            if Instance.Components[a] is TControl then
              TControl(Instance.Components[a]).Visible:=true;
        // MG end of workaround
      finally
        Free;
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


//==============================================================================



{$I form.inc}
{$I Customform.inc}
{$I screen.inc}
{$I application.inc}

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


