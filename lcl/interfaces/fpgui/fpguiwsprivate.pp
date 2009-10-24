{ $Id: FPGUIwsprivate.pp 10697 2007-02-27 23:17:33Z marc $ }
{
                 ------------------------------------------
                 FPGUIwsprivate.pp  -  FPGUI internal classes
                 ------------------------------------------

 @created(Thu Feb 1st WET 2007)
 @lastmod($Date: 2007-02-27 18:17:33 -0500 (Tue, 27 Feb 2007) $)
 @author(Marc Weustink <marc@@lazarus.dommelstein.net>)

 This unit contains the private class hierarchy for the fpgui implemetation
 This hierarchy reflects (more or less) the  widget hierarchy

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

unit fpguiwsprivate;

{$mode delphi}

interface

uses
  // LCL
  LCLType, LMessages, LCLProc, Controls, Classes, SysUtils, Forms,
  LCLIntf, Menus, Dialogs, ExtCtrls,
  // widgetset
  WSControls, WSLCLClasses, WSProc,
  // interface
  fpg_widget, fpg_form, fpg_button, fpg_combobox, fpg_dialogs,
  fpg_edit, fpg_checkbox, fpg_radiobutton, fpg_tab, fpg_memo,
  fpg_menu, fpg_base;


type

  IContainer = interface(IInterface)
    procedure AddChild(AWidget: TfpgWidget);
    procedure RemoveChild(AWidget: TfpgWidget);
  end;

  { TFPGUIPrivate }

  TFPGUIPrivate = class(TInterfacedObject)
    function _AddRef : longint;stdcall;
    function _Release : longint;stdcall;
  end;

  { To access protected properties of TfpgWidget }
  TFPGWidgetHack = class(TfpgWidget)
  end;

  { TFPGUIPrivateWidget }
  { Private class for widgets }

  TFPGUIPrivateWidget = class(TFPGUIPrivate)
  private
    FWidget: TfpgWidget;
    FLCLObject: TWinControl;
    function GetVisible: Boolean;
    function GetWidgetProtected: TFPGWidgetHack;
    procedure SetVisible(const AValue: Boolean);
    { Handlers for default properties common to all TfpgWidget descendants}
    procedure PaintHandler(Sender: TObject{; const ARect: TfpgRect});
    procedure ClickHandler(Sender: TObject);
    procedure ResizeHandler(Sender: TObject);
    procedure MoveHandler(Sender: TObject);
    procedure EnterHandler(Sender: TObject);
    procedure ExitHandler(Sender: TObject);
    procedure   MsgPaint(var fpgmsg: TfpgMessageRec); message FPGM_PAINT;
    procedure   MsgResize(var fpgmsg: TfpgMessageRec); message FPGM_RESIZE;
    procedure   MsgMove(var fpgmsg: TfpgMessageRec); message FPGM_MOVE;
    procedure   MsgKeyChar(var fpgmsg: TfpgMessageRec); message FPGM_KEYCHAR;
    procedure   MsgKeyPress(var fpgmsg: TfpgMessageRec); message FPGM_KEYPRESS;
    procedure   MsgKeyRelease(var fpgmsg: TfpgMessageRec); message FPGM_KEYRELEASE;
    procedure   MsgMouseDown(var fpgmsg: TfpgMessageRec); message FPGM_MOUSEDOWN;
    procedure   MsgMouseUp(var fpgmsg: TfpgMessageRec); message FPGM_MOUSEUP;
    procedure   MsgMouseMove(var fpgmsg: TfpgMessageRec); message FPGM_MOUSEMOVE;
    procedure   MsgDoubleClick(var fpgmsg: TfpgMessageRec); message FPGM_DOUBLECLICK;
    procedure   MsgMouseEnter(var fpgmsg: TfpgMessageRec); message FPGM_MOUSEENTER;
    procedure   MsgMouseExit(var fpgmsg: TfpgMessageRec); message FPGM_MOUSEEXIT;
    procedure   MsgMouseScroll(var fpgmsg: TfpgMessageRec); message FPGM_SCROLL;
    //procedure MouseDown
  protected
    { Helper methods for descendents }
    function GetParentContainerWidget: TfpgWidget;
  public
    { Constructors / Destructors }
    constructor Create(ALCLObject: TWinControl; const AParams: TCreateParams); virtual;
    destructor Destroy; override;
    { Virtual methods }
    procedure CreateWidget(const AParams: TCreateParams); virtual;
    procedure SetEvents; virtual;
    procedure SetSize(AWidth, AHeight: LongInt); virtual;
    procedure SetPosition(AX, AY: Integer); virtual;
    function  HasStaticText: Boolean; virtual;
    procedure SetText(const AText: String); virtual;
    function  GetText: String; virtual;
  public
    { Properties }
    property LCLObject: TWinControl read FLCLObject;
    property Visible: Boolean read GetVisible write SetVisible;
    property Widget: TfpgWidget read FWidget write FWidget;
    property WidgetProtected: TFPGWidgetHack read GetWidgetProtected;
  end;


  { TFPGUIPrivateContainer }
  { Private class for containers }

  TFPGUIPrivateContainer = class(TFPGUIPrivateWidget, IContainer)
  private
  protected
  public
    constructor Create(ALCLObject: TWinControl; const AParams: TCreateParams); override;
    destructor Destroy; override;
  // IContainer
    procedure AddChild(AWidget: TfpgWidget);
    procedure RemoveChild(AWidget: TfpgWidget);
  end;


  { TFPGUIPrivateBin }
  { Private class for bins }

  TFPGUIPrivateBin = class(TFPGUIPrivateContainer)
  private
  protected
  public
  end;


  { TFPGUIPrivateWindow }
  { Private class for windows }

  TFPGUIPrivateWindow = class(TFPGUIPrivateBin)
  private
    { Event Handlers }
    procedure CloseHandler(Sender: TObject; var CloseAction: TCloseAction);
  protected
  public
    { Constructors / Destructors }
    constructor Create(ALCLObject: TWinControl; const AParams: TCreateParams); override;
    destructor Destroy; override;
    { Virtual methods }
    procedure CreateWidget(const AParams: TCreateParams); override;
    procedure SetEvents; override;
    procedure SetSize(AWidth, AHeight: LongInt); override;
    procedure SetPosition(AX, AY: Integer); override;
    function  HasStaticText: Boolean; override;
    procedure SetText(const AText: String); override;
    function  GetText: String; override;
  public
    MenuBar: TfpgMenuBar;
    { Other methods }
    function Form: TfpgForm;
  end;


  { TFPGUIPrivateDialog }
  { Private class for dialogs }

  { TFPGUIPrivateCommonDialog }

  TFPGUIPrivateCommonDialog = class(TFPGUIPrivate)
  private
    FDialog: TfpgBaseDialog;
    FLCLDialog: TCommonDialog;
  protected
    procedure   CreateDialog; virtual;
    function    InternalShowDialog: Boolean; virtual;
    procedure   UpdatePropsBefore; virtual;
    procedure   UpdatePropsAfter; virtual;
  public
    constructor Create(ALCLDialog: TCommonDialog); virtual;
    destructor  Destroy; override;

    function    ShowDialog: Boolean;

    property Dialog: TfpgBaseDialog read FDialog write FDialog;
    property LCLDialog: TCommonDialog read FLCLDialog;
  end;

  { TFPGUIPrivateFileDialog }

  { TFPGUIPrivateFontDialog }

  TFPGUIPrivateFontDialog = class(TFPGUIPrivateCommonDialog)
  protected
    procedure CreateDialog; override;
    function  InternalShowDialog: Boolean; override;
    procedure UpdatePropsBefore; override;
    procedure UpdatePropsAfter; override;
  public
    function  FontDialog: TfpgFontSelectDialog;
  end;

  { TFPGUIPrivateFileDialog }

  TFPGUIPrivateFileDialog = class(TFPGUIPrivateCommonDialog)
  private
  protected
    procedure UpdatePropsBefore; override;
    procedure UpdatePropsAfter; override;
    procedure CreateDialog; override;
  public
    function  FileDialog: TfpgFileDialog;
    function  LCLFileDialog: TFileDialog;
  end;

  { TFPGUIOpenDialog }
  { Private class for dialogs }

  { TFPGUIPrivateOpenDialog }

  TFPGUIPrivateOpenDialog = class(TFPGUIPrivateFileDialog)
  private
  protected
    function InternalShowDialog: Boolean; override;
  public
  end;

  { TFPGUIPrivateDialog }
  { Private class for dialogs }

  { TFPGUIPrivateSaveDialog }

  TFPGUIPrivateSaveDialog = class(TFPGUIPrivateFileDialog)
  private
  protected
    function InternalShowDialog: Boolean; override;
  public
  end;


  { TFPGUIPrivateButton }
  { Private class for buttons }

  TFPGUIPrivateButton = class(TFPGUIPrivateWidget)
  private
    procedure Clicked(Sender: TObject);
  protected
  public
    { Constructors / Destructors }
    constructor Create(ALCLObject: TWinControl; const AParams: TCreateParams); override;
    { Virtual methods }
    procedure CreateWidget(const AParams: TCreateParams); override;
    function  HasStaticText: Boolean; override;
    procedure SetText(const AText: String); override;
    function  GetText: String; override;
  public
    { Other methods }
    function Button: TfpgButton;
  end;

  { TFPGUIPrivateComboBox }

  TFPGUIPrivateComboBox = class(TFPGUIPrivateWidget)
  private
  protected
    procedure HandleChange(Sender: TObject);
    procedure HandleDropDown(Sender: TObject);
    procedure HandleCloseUp(Sender: TObject);
  public
    { Virtual methods }
    procedure CreateWidget(const AParams: TCreateParams); override;
    procedure SetEvents; override;
  public
    { Other methods }
    function ComboBox: TfpgComboBox;
  end;


  { TFPGUIPrivateEdit }

  TFPGUIPrivateEdit = class(TFPGUIPrivateWidget)
  private
  protected
  public
    { Virtual methods }
    procedure CreateWidget(const AParams: TCreateParams); override;
    function  HasStaticText: Boolean; override;
    procedure SetText(const AText: String); override;
    function  GetText: String; override;
  public
    { Other methods }
    function Edit: TfpgEdit;
  end;

  { TFPGUIPrivateCheckBox }

  TFPGUIPrivateCheckBox = class(TFPGUIPrivateWidget)
  private
  protected
    procedure HandleChange(Sender: TObject);
  public
    { Virtual methods }
    procedure CreateWidget(const AParams: TCreateParams); override;
    function  HasStaticText: Boolean; override;
    procedure SetText(const AText: String); override;
    function  GetText: String; override;
    procedure SetEvents; override;
  public
    { Other methods }
    function CheckBox: TfpgCheckBox;
  end;

  { TFPGUIPrivateRadioButton }

  TFPGUIPrivateRadioButton = class(TFPGUIPrivateWidget)
  private
  protected
  public
    { Virtual methods }
    procedure CreateWidget(const AParams: TCreateParams); override;
    function  HasStaticText: Boolean; override;
    procedure SetText(const AText: String); override;
    function  GetText: String; override;
  public
    { Other methods }
    function RadioButton: TfpgRadioButton;
  end;

  { TFPGUIPrivatePageControl }

  TFPGUIPrivatePageControl = class(TFPGUIPrivateWidget)
  private
  protected
  public
    { Virtual methods }
    procedure CreateWidget(const AParams: TCreateParams); override;
  end;

  { TFPGUIPrivateMemo }

  TFPGUIPrivateMemo = class(TFPGUIPrivateWidget)
  private
  protected
  public
    { Virtual methods }
    procedure CreateWidget(const AParams: TCreateParams); override;
    function  HasStaticText: Boolean; override;
    procedure SetText(const AText: String); override;
    function  GetText: String; override;
  public
    { Other methods }
    function Memo: TfpgMemo;
  end;
  
  { TFPGUIPrivatePopUpMenu }

  TFPGUIPrivatePopUpMenu = class(TFPGUIPrivateWidget)
  private
    FLCLMenu: TPopUpMenu;
    FItems: TMenuItem;
  protected
  public
    { Constructors / Destructors }
    constructor Create(ALCLObject: TPopUpMenu; AItems: TMenuItem); virtual;
    { Virtual methods }
  public
    { Other methods }
    function PopUpMenu: TfpgPopUpMenu;
    procedure PopUp(X, Y: Integer);
  end;

implementation

uses
  LCLMessageGlue, fpg_main;

{ TFPGUIPrivate }

function TFPGUIPrivate._AddRef: longint; stdcall;
begin
  Result := -1;
end;

function TFPGUIPrivate._Release: longint; stdcall;
begin
  Result := -1;
end;

{ TFPGUIPrivateWidget }

procedure TFPGUIPrivateWidget.SetVisible(const AValue: Boolean);
begin
  Widget.Visible := AValue;
end;

procedure TFPGUIPrivateWidget.PaintHandler(Sender: TObject{; const ARect: TfpgRect});
var
  Msg: TLMPaint;
  AStruct: PPaintStruct;
begin
  {$ifdef VerboseFPGUIPrivate}
    WriteLn('TFPGUIPrivateWindow.PaintHandler');
  {$endif}

  if (LCLObject is TWinControl) then
  begin
    FillChar(Msg, SizeOf(Msg), #0);

    Msg.Msg := LM_PAINT;
    New(AStruct);
    FillChar(AStruct^, SizeOf(TPaintStruct), 0);
    Msg.PaintStruct := AStruct;
    Msg.DC := BeginPaint(THandle(Self), AStruct^);

//    Msg.PaintStruct^.rcPaint := PaintData.ClipRect^;
    Msg.PaintStruct^.hdc := Msg.DC;


    // send paint message
    try
      // Saving clip rect and clip region
      try
        LCLObject.WindowProc(TLMessage(Msg));
      finally
        EndPaint(THandle(Self), AStruct^);
        Dispose(AStruct);
      end;
    except
      Application.HandleException(nil);
    end;
  end;


end;

procedure TFPGUIPrivateWidget.ClickHandler(Sender: TObject);
begin
  LCLSendClickedMsg(LCLObject);
end;

procedure TFPGUIPrivateWidget.ResizeHandler(Sender: TObject);
begin
  LCLSendSizeMsg(LCLObject, Widget.Width, Widget.Height, SIZENORMAL);
end;

procedure TFPGUIPrivateWidget.MoveHandler(Sender: TObject);
begin
  LCLSendMoveMsg(LCLObject, Widget.Left, Widget.Top);
end;

procedure TFPGUIPrivateWidget.EnterHandler(Sender: TObject);
begin

end;

procedure TFPGUIPrivateWidget.ExitHandler(Sender: TObject);
begin

end;

procedure TFPGUIPrivateWidget.MsgPaint(var fpgmsg: TfpgMessageRec);
var
  Msg: TLMPaint;
  AStruct: PPaintStruct;
begin
  {$ifdef VerboseFPGUIPrivate}
    WriteLn('TFPGUIPrivateWindow.PaintHandler');
  {$endif}

  if (LCLObject is TWinControl) then
  begin
    FillChar(Msg, SizeOf(Msg), #0);

    Msg.Msg := LM_PAINT;
    New(AStruct);
    FillChar(AStruct^, SizeOf(TPaintStruct), 0);
    Msg.PaintStruct := AStruct;
    Msg.DC := BeginPaint(THandle(Self), AStruct^);

//    Msg.PaintStruct^.rcPaint := PaintData.ClipRect^;
    Msg.PaintStruct^.hdc := Msg.DC;


    // send paint message
    try
      // Saving clip rect and clip region
      try
        LCLObject.WindowProc(TLMessage(Msg));
      finally
        EndPaint(THandle(Self), AStruct^);
        Dispose(AStruct);
      end;
    except
      Application.HandleException(nil);
    end;
  end;end;

procedure TFPGUIPrivateWidget.MsgResize(var fpgmsg: TfpgMessageRec);
begin
  LCLSendSizeMsg(LCLObject, fpgmsg.Params.rect.Width, fpgmsg.Params.rect.Height, SIZENORMAL);
end;

procedure TFPGUIPrivateWidget.MsgMove(var fpgmsg: TfpgMessageRec);
begin
  LCLSendMoveMsg(LCLObject, fpgmsg.Params.rect.Left, fpgmsg.Params.rect.Top);
end;

procedure TFPGUIPrivateWidget.MsgKeyChar(var fpgmsg: TfpgMessageRec);
begin

end;

procedure TFPGUIPrivateWidget.MsgKeyPress(var fpgmsg: TfpgMessageRec);
begin
  LCLSendKeyDownEvent(LCLObject, fpgmsg.Params.keyboard.keycode, fpgmsg.Params.keyboard.keycode, True, True);
end;

procedure TFPGUIPrivateWidget.MsgKeyRelease(var fpgmsg: TfpgMessageRec);
begin
  LCLSendKeyUpEvent(LCLObject, fpgmsg.Params.keyboard.keycode, fpgmsg.Params.keyboard.keycode, True, True);
end;

function fpgMouseButtonToTButton(AButton: Word): Controls.TMouseButton;
begin
  case AButton of
    MOUSE_LEFT: Result := Controls.mbLeft;
    MOUSE_MIDDLE: Result := Controls.mbMiddle;
    MOUSE_RIGHT: Result := Controls.mbRight;
  else
    Result := Controls.mbExtra1;

  end;
end;

procedure TFPGUIPrivateWidget.MsgMouseDown(var fpgmsg: TfpgMessageRec);
begin
  LCLSendMouseDownMsg(LCLObject, fpgmsg.Params.mouse.x, fpgmsg.Params.mouse.y,fpgMouseButtonToTButton(fpgmsg.Params.mouse.Buttons), fpgmsg.Params.mouse.shiftstate);
end;

procedure TFPGUIPrivateWidget.MsgMouseUp(var fpgmsg: TfpgMessageRec);
begin
  LCLSendMouseUpMsg(LCLObject, fpgmsg.Params.mouse.x, fpgmsg.Params.mouse.y,fpgMouseButtonToTButton(fpgmsg.Params.mouse.Buttons), fpgmsg.Params.mouse.shiftstate);
end;

procedure TFPGUIPrivateWidget.MsgMouseMove(var fpgmsg: TfpgMessageRec);
begin
  LCLSendMouseMoveMsg(LCLObject, fpgmsg.Params.mouse.x, fpgmsg.Params.mouse.y, fpgmsg.Params.mouse.shiftstate);
end;

procedure TFPGUIPrivateWidget.MsgDoubleClick(var fpgmsg: TfpgMessageRec);
begin
  LCLSendMouseMultiClickMsg(LCLObject, fpgmsg.Params.mouse.x, fpgmsg.Params.mouse.y, fpgMouseButtonToTButton(fpgmsg.Params.mouse.Buttons), 2, fpgmsg.Params.mouse.shiftstate);
end;

procedure TFPGUIPrivateWidget.MsgMouseEnter(var fpgmsg: TfpgMessageRec);
begin
  LCLSendMouseEnterMsg(LCLObject);
end;

procedure TFPGUIPrivateWidget.MsgMouseExit(var fpgmsg: TfpgMessageRec);
begin
  LCLSendMouseLeaveMsg(LCLObject);
end;

procedure TFPGUIPrivateWidget.MsgMouseScroll(var fpgmsg: TfpgMessageRec);
begin
  LCLSendMouseWheelMsg(LCLObject, fpgmsg.Params.mouse.x, fpgmsg.Params.mouse.y,fpgmsg.Params.mouse.delta, fpgmsg.Params.mouse.shiftstate);
end;

function TFPGUIPrivateWidget.GetParentContainerWidget: TfpgWidget;
begin
  // Note, if the Handle of the parent doesn't exist, it's automatically
  // created
  if Assigned(LCLObject.Parent) then
    Result := TFPGUIPrivateContainer(LCLObject.Parent.Handle).Widget
  else
    Result := nil;
end;

function TFPGUIPrivateWidget.GetVisible: Boolean;
begin
  Result := Widget.Visible;
end;

function TFPGUIPrivateWidget.GetWidgetProtected: TFPGWidgetHack;
begin
  REsult := TFPGWidgetHack(FWidget);
end;

constructor TFPGUIPrivateWidget.Create(ALCLObject: TWinControl; const AParams: TCreateParams);
begin
  FLCLObject := ALCLObject;

  CreateWidget(AParams);

  Widget.SetPosition(AParams.X, AParams.Y, AParams.Width, AParams.Height);
  
  SetEvents;
end;

destructor TFPGUIPrivateWidget.Destroy;
begin
  if (FWidget <> nil) then FreeAndNil(FWidget);

  inherited Destroy;
end;

procedure TFPGUIPrivateWidget.CreateWidget(const AParams: TCreateParams);
begin
  Widget := TfpgWidget.Create(nil);
end;

procedure TFPGUIPrivateWidget.SetEvents;
begin
  //WidgetProtected.OnPaint := PaintHandler;
  WidgetProtected.OnClick := ClickHandler;
  //WidgetProtected.OnResize:= ResizeHandler;
  //WidgetProtected.OnMove  := MoveHandler;
  fpgApplication.SetMessageHook(Widget, FPGM_PAINT, Self);
  fpgApplication.SetMessageHook(Widget, FPGM_ACTIVATE, Self);
  fpgApplication.SetMessageHook(Widget, FPGM_DEACTIVATE, Self);
  fpgApplication.SetMessageHook(Widget, FPGM_KEYPRESS, Self);
  fpgApplication.SetMessageHook(Widget, FPGM_KEYRELEASE, Self);
  fpgApplication.SetMessageHook(Widget, FPGM_KEYCHAR, Self);
  fpgApplication.SetMessageHook(Widget, FPGM_MOUSEDOWN, Self);
  fpgApplication.SetMessageHook(Widget, FPGM_MOUSEUP, Self);
  fpgApplication.SetMessageHook(Widget, FPGM_MOUSEMOVE, Self);
  fpgApplication.SetMessageHook(Widget, FPGM_DOUBLECLICK, Self);
  fpgApplication.SetMessageHook(Widget, FPGM_MOUSEENTER, Self);
  fpgApplication.SetMessageHook(Widget, FPGM_MOUSEEXIT, Self);
  fpgApplication.SetMessageHook(Widget, FPGM_CLOSE, Self);
  fpgApplication.SetMessageHook(Widget, FPGM_SCROLL, Self);
  fpgApplication.SetMessageHook(Widget, FPGM_RESIZE, Self);
  fpgApplication.SetMessageHook(Widget, FPGM_MOVE, Self);
  fpgApplication.SetMessageHook(Widget, FPGM_POPUPCLOSE, Self);
  fpgApplication.SetMessageHook(Widget, FPGM_HINTTIMER, Self);
end;

procedure TFPGUIPrivateWidget.SetSize(AWidth, AHeight: LongInt);
begin
  Widget.SetPosition(Widget.Left, Widget.Top, AWidth, AHeight);
end;

procedure TFPGUIPrivateWidget.SetPosition(AX, AY: Integer);
begin
  Widget.SetPosition(AX, AY, Widget.Width, Widget.Height);
end;

function TFPGUIPrivateWidget.HasStaticText: Boolean;
begin
  Result := False;
end;

procedure TFPGUIPrivateWidget.SetText(const AText: String);
begin

end;

function TFPGUIPrivateWidget.GetText: String;
begin
  Result := '';
end;

{ TFPGUIPrivateContainer }

constructor TFPGUIPrivateContainer.Create(ALCLObject: TWinControl;
  const AParams: TCreateParams);
begin
  inherited Create(ALCLObject, AParams);
end;

destructor TFPGUIPrivateContainer.Destroy;
begin
  inherited Destroy;
end;

procedure TFPGUIPrivateContainer.AddChild(AWidget: TfpgWidget);
begin
//  fFixed.AddWidget(AWidget, 0, 0);
end;

procedure TFPGUIPrivateContainer.RemoveChild(AWidget: TfpgWidget);
begin
//  fFixed.RemoveChild(AWidget);
end;

{ TFPGUIPrivateWindow }

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateWindow.Form
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TFPGUIPrivateWindow.Form: TfpgForm;
begin
  Result := TfpgForm(Widget);
end;

function TFPGUIPrivateWindow.HasStaticText: Boolean;
begin
  Result := True;
end;

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateWindow.SetSize
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TFPGUIPrivateWindow.SetSize(AWidth, AHeight: LongInt);
begin
  Form.SetPosition(Form.Top, Form.Left, AWidth, AHeight);
end;

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateWindow.SetPosition
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TFPGUIPrivateWindow.SetPosition(AX, AY: Integer);
begin
  Form.SetPosition(AX, AY, Form.Width, Form.Height);
end;

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateWindow.PaintHandler

  Sends a LM_PAINT message to the LCL. This is for windowed controls only
 ------------------------------------------------------------------------------}

procedure TFPGUIPrivateWindow.CloseHandler(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  if LCLSendCloseQueryMsg(LCLObject) = 0 then
    CloseAction := caNone;
end;

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateWindow.Create
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
constructor TFPGUIPrivateWindow.Create(ALCLObject: TWinControl; const AParams: TCreateParams);
begin
  inherited Create(ALCLObject, AParams);

//  Form.InsertChild(fFixed);
end;

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateWindow.CreateWidget
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TFPGUIPrivateWindow.CreateWidget(const AParams: TCreateParams);
begin
  {$IFDEF VerboseFPGUIPrivate}
    WriteLn('[TFPGUIPrivateWindow.CreateWidget]');
  {$ENDIF}

  Widget := TfpgForm.Create(nil);
  Form.SetPosition(AParams.X, AParams.Y, AParams.Width, AParams.Height);

  MenuBar := TfpgMenuBar.Create(Widget);
  MenuBar.Visible := False;
end;

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateWindow.SetEvents
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TFPGUIPrivateWindow.SetEvents;
begin
  inherited SetEvents;
  Form.OnClose := CloseHandler;
end;

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateWindow.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TFPGUIPrivateWindow.Destroy;
begin
{$IFDEF VerboseFPGUIPrivate}
  WriteLn('[TFPGUIPrivateWindow.Destroy]');
{$ENDIF}

  // Instead of destroying the form immediately, we call Close
  // and set CloseAction to caFree in OnClose,
  // which will do a delayed close
  Form.Close;

  // By setting the Widget to nil we prevent it's future
  // destruction
  Widget := nil;

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateWindow.SetText
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TFPGUIPrivateWindow.SetText(const AText: String);
begin
  Form.WindowTitle := AText;
end;

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateWindow.GetText
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TFPGUIPrivateWindow.GetText: String;
begin
  Result := Form.WindowTitle;
end;

{ TFPGUIPrivateButton }

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateButton.Clicked
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TFPGUIPrivateButton.Clicked(Sender: TObject);
begin
  LCLSendClickedMsg(LCLObject);
end;

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateButton.Button
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TFPGUIPrivateButton.Button: TfpgButton;
begin
  Result := TfpgButton(Widget);
end;

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateButton.CreateWidget
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TFPGUIPrivateButton.CreateWidget(const AParams: TCreateParams);
begin
{$IFDEF VerboseFPGUIPrivate}
  WriteLn('[TFPGUIPrivateButton.CreateWidget]');
{$ENDIF}
  Widget := TfpgButton.Create(GetParentContainerWidget());
  Widget.SetPosition(LCLObject.Left, LCLObject.Top, LCLObject.Width, LCLObject.Height);
end;

function TFPGUIPrivateButton.HasStaticText: Boolean;
begin
  Result := True;
end;

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateButton.Create
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
constructor TFPGUIPrivateButton.Create(ALCLObject: TWinControl; const AParams: TCreateParams);
begin
  inherited Create(ALCLObject, AParams);
end;

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateButton.SetText
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TFPGUIPrivateButton.SetText(const AText: String);
begin
  Button.Text := AText;
end;

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateButton.GetText
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TFPGUIPrivateButton.GetText: String;
begin
  Result := Button.Text;
end;

{ TFPGUIPrivateComboBox }

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateComboBox.ComboBox
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TFPGUIPrivateComboBox.ComboBox: TfpgComboBox;
begin
  Result := TfpgComboBox(Widget);
end;

procedure TFPGUIPrivateComboBox.HandleChange(Sender: TObject);
begin
  LCLSendSelectionChangedMsg(LCLObject);
end;

procedure TFPGUIPrivateComboBox.HandleDropDown(Sender: TObject);
begin
  LCLSendDropDownMsg(LCLObject);
end;

procedure TFPGUIPrivateComboBox.HandleCloseUp(Sender: TObject);
begin
  LCLSendCloseUpMsg(LCLObject);
end;

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateComboBox.CreateWidget
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TFPGUIPrivateComboBox.CreateWidget(const AParams: TCreateParams);
begin
  Widget := TfpgComboBox.Create(GetParentContainerWidget());
  Widget.SetPosition(LCLObject.Left, LCLObject.Top, LCLObject.Width, LCLObject.Height);
end;

procedure TFPGUIPrivateComboBox.SetEvents;
begin
  inherited SetEvents;
  ComboBox.OnChange     := HandleChange;
  ComboBox.OnDropDown   := HandleDropDown;
  ComboBox.OnCloseUp    := HandleCloseUp;
end;

{ TFPGUIPrivateEdit }

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateEdit.CreateWidget
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TFPGUIPrivateEdit.CreateWidget(const AParams: TCreateParams);
begin
  Widget := TfpgEdit.Create(GetParentContainerWidget());
  Widget.SetPosition(LCLObject.Left, LCLObject.Top, LCLObject.Width, LCLObject.Height);
end;

function TFPGUIPrivateEdit.HasStaticText: Boolean;
begin
  Result := True;
end;

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateEdit.Edit
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TFPGUIPrivateEdit.Edit: TfpgEdit;
begin
  Result := TfpgEdit(Widget);
end;

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateEdit.SetText
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TFPGUIPrivateEdit.SetText(const AText: String);
begin
  Edit.Text := AText;
end;

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateEdit.GetText
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TFPGUIPrivateEdit.GetText: String;
begin
  Result := Edit.Text;
end;

{ TFPGUIPrivateCheckBox }

function TFPGUIPrivateCheckBox.CheckBox: TfpgCheckBox;
begin
  Result := TfpgCheckBox(Widget);
end;

procedure TFPGUIPrivateCheckBox.HandleChange(Sender: TObject);
begin
  LCLSendChangedMsg(LCLObject);
end;

procedure TFPGUIPrivateCheckBox.CreateWidget(const AParams: TCreateParams);
begin
  Widget := TfpgCheckBox.Create(GetParentContainerWidget());
  Widget.SetPosition(LCLObject.Left, LCLObject.Top, LCLObject.Width, LCLObject.Height);
end;

function TFPGUIPrivateCheckBox.HasStaticText: Boolean;
begin
  Result := True;
end;

procedure TFPGUIPrivateCheckBox.SetText(const AText: String);
begin
  CheckBox.Text := AText;
end;

function TFPGUIPrivateCheckBox.GetText: String;
begin
  Result := CheckBox.Text;
end;

procedure TFPGUIPrivateCheckBox.SetEvents;
begin
  inherited SetEvents;
  CheckBox.OnChange := HandleChange;
end;

{ TFPGUIPrivateRadioButton }

function TFPGUIPrivateRadioButton.RadioButton: TfpgRadioButton;
begin
  Result := TfpgRadioButton(Widget);
end;

procedure TFPGUIPrivateRadioButton.CreateWidget(const AParams: TCreateParams);
begin
  Widget := TfpgRadioButton.Create(GetParentContainerWidget());
  Widget.SetPosition(LCLObject.Left, LCLObject.Top, LCLObject.Width, LCLObject.Height);
end;

function TFPGUIPrivateRadioButton.HasStaticText: Boolean;
begin
  Result := True;
end;

procedure TFPGUIPrivateRadioButton.SetText(const AText: String);
begin
  RadioButton.Text := AText;
end;

function TFPGUIPrivateRadioButton.GetText: String;
begin
  Result := RadioButton.Text;
end;

{ TFPGUIPrivateNotebook }

procedure TFPGUIPrivatePageControl.CreateWidget(const AParams: TCreateParams);
begin
  Widget := TfpgPageControl.Create(GetParentContainerWidget());
  Widget.SetPosition(LCLObject.Left, LCLObject.Top, LCLObject.Width, LCLObject.Height);
end;

{ TFPGUIPrivateMemo }

procedure TFPGUIPrivateMemo.CreateWidget(const AParams: TCreateParams);
begin
  Widget := TfpgMemo.Create(GetParentContainerWidget());
  Widget.SetPosition(LCLObject.Left, LCLObject.Top, LCLObject.Width, LCLObject.Height);
end;

function TFPGUIPrivateMemo.HasStaticText: Boolean;
begin
  Result := True;
end;

function TFPGUIPrivateMemo.Memo: TfpgMemo;
begin
  Result := TfpgMemo(Widget);
end;

procedure TFPGUIPrivateMemo.SetText(const AText: String);
begin
  Memo.Text := AText;
end;

function TFPGUIPrivateMemo.GetText: String;
begin
  Result := Memo.Text;
end;

{ TFPGUIPrivatePopUpMenu }

constructor TFPGUIPrivatePopUpMenu.Create(ALCLObject: TPopUpMenu; AItems: TMenuItem);
begin
  FLCLMenu := ALCLObject;
  FItems := AItems;

  // CreateWidget

  Widget := TfpgPopUpMenu.Create(nil);

  SetEvents;
end;

function TFPGUIPrivatePopUpMenu.PopUpMenu: TfpgPopUpMenu;
begin
  Result := TfpgPopUpMenu(Widget);
end;

procedure TFPGUIPrivatePopUpMenu.PopUp(X, Y: Integer);
begin
  PopUpMenu.ShowAt(PopUpMenu, X, Y);
end;

{ TFPGUIPrivateCommonDialog }

constructor TFPGUIPrivateCommonDialog.Create(ALCLDialog: TCommonDialog);
begin
  FLCLDialog := ALCLDialog;
  CreateDialog;
  WriteLn('Created ', ClassNAme, ':', Dialog.ClassName);
end;

destructor TFPGUIPrivateCommonDialog.Destroy;
begin
  Dialog.Free;
  inherited Destroy;
end;

procedure TFPGUIPrivateCommonDialog.CreateDialog;
begin
  Dialog := TfpgBaseDialog.Create(nil);
end;

function TFPGUIPrivateCommonDialog.InternalShowDialog: Boolean;
begin
  Result := Dialog.ShowModal = 1;
end;

procedure TFPGUIPrivateCommonDialog.UpdatePropsBefore;
begin
  Dialog.WindowTitle := LCLDialog.Title;
end;

procedure TFPGUIPrivateCommonDialog.UpdatePropsAfter;
begin

end;

function TFPGUIPrivateCommonDialog.ShowDialog: Boolean;
begin
  UpdatePropsBefore;
  Result := InternalShowDialog;
  LCLDialog.UserChoice := Dialog.ModalResult;
  UpdatePropsAfter;
end;

{ TFPGUIPrivateFileDialog }

procedure TFPGUIPrivateFileDialog.UpdatePropsBefore;


begin
  inherited UpdatePropsBefore;
  FileDialog.Filter := LCLFileDialog.Filter;
  if Length(LCLFileDialog.FileName) <>  0 then
    FileDialog.FileName := LCLFileDialog.FileName
  else
    FileDialog.FileName := LCLFileDialog.InitialDir;
end;

procedure TFPGUIPrivateFileDialog.UpdatePropsAfter;
begin
  inherited UpdatePropsAfter;
  LCLFileDialog.FileName := FileDialog.FileName;
  //LCLFileDialog.Files.Assign(FileDialog.);
end;

procedure TFPGUIPrivateFileDialog.CreateDialog;
begin
  Dialog := TfpgFileDialog.Create(nil);
end;

function TFPGUIPrivateFileDialog.FileDialog: TfpgFileDialog;
begin
  Result :=TfpgFileDialog(Dialog);
end;

function TFPGUIPrivateFileDialog.LCLFileDialog: TFileDialog;
begin
  Result := TFileDialog(LCLDialog) ;
end;

{ TFPGUIPrivateOpenDialog }

function TFPGUIPrivateOpenDialog.InternalShowDialog: Boolean;
begin
  Result:=FileDialog.RunOpenFile;
end;

{ TFPGUIPrivateSaveDialog }

function TFPGUIPrivateSaveDialog.InternalShowDialog: Boolean;
begin
  Result:=FileDialog.RunSaveFile;
end;

{ TFPGUIPrivateFontDialog }

procedure TFPGUIPrivateFontDialog.CreateDialog;
begin
  Dialog := TfpgFontSelectDialog.Create(nil);
end;

function TFPGUIPrivateFontDialog.InternalShowDialog: Boolean;
begin
  FontDialog.ShowModal;
end;

procedure TFPGUIPrivateFontDialog.UpdatePropsBefore;
begin
  inherited;
end;

procedure TFPGUIPrivateFontDialog.UpdatePropsAfter;
begin
  inherited;
end;

function TFPGUIPrivateFontDialog.FontDialog: TfpgFontSelectDialog;
begin
  Result := TfpgFontSelectDialog(Dialog);
end;

end.
  
