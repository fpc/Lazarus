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
  LCLIntf, Menus, Dialogs, ExtCtrls, Graphics,
  // widgetset
  WSControls, WSLCLClasses, WSProc,
  // interface
  fpg_main,
  fpg_widget, fpg_form, fpg_button, fpg_combobox, fpg_dialogs,
  fpg_edit, fpg_checkbox, fpg_radiobutton, fpg_tab, fpg_memo,
  fpg_menu, fpg_label, fpg_listbox, fpg_panel,
  fpg_popupwindow, fpg_base, fpguiproc;


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

  { TFPGWidgetHack }

  TFPGWidgetHack = class(TfpgWidget)
  protected
  end;

  { TFPGUIPrivateWidget }
  { Private class for widgets }

  TFPGUIPrivateWidget = class(TFPGUIPrivate)
  private
    FWidget: TfpgWidget;
    FLCLObject: TWinControl;
    FDC: HDC;
    FEntered: Boolean;
    function GetEnabled: Boolean;
    function GetFont: TFont; virtual;
    function GetVisible: Boolean;
    function GetWidgetProtected: TFPGWidgetHack;
    procedure SetDC(const AValue: HDC);
    procedure SetEnabled(const AValue: Boolean);
    procedure SetFont(const AValue: TFont); virtual;
    procedure SetVisible(const AValue: Boolean);
    { Handlers for default properties common to all TfpgWidget descendants}
    procedure PaintHandler(Sender: TObject);
    procedure ClickHandler(Sender: TObject);
    procedure EnterHandler(Sender: TObject);
    procedure ExitHandler(Sender: TObject);
    procedure   MsgDeactivate(var fpgmsg: TfpgMessageRec); message FPGM_DEACTIVATE;
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
    procedure SetWidgetPosition(AWidget: TfpgWidget; AX, AY: Integer); virtual;
  public
    { Constructors / Destructors }
    constructor Create(ALCLObject: TWinControl; const AParams: TCreateParams); virtual;
    destructor Destroy; override;
    { Virtual methods }
    procedure CreateWidget(const AParams: TCreateParams); virtual;
    procedure SetEvents; virtual;
    function  HasStaticText: Boolean; virtual;
    procedure SetText(const AText: String); virtual;
    function  GetText: String; virtual;
    procedure GetPreferredSize(var PreferredWidth, PreferredHeight: integer;
                               WithThemeSpace: Boolean); virtual;
    procedure GetClientRect(var ARect: TRect); virtual;
    procedure GetDefaultClientRect(var ARect: TRect); virtual;
    procedure AdjustRectXY(var AfpgRect: TfpgRect); virtual;
    procedure AdjustRectXYToInterface(var ARect: TRect); virtual;
    { Non-Virtual }
    procedure SetSize(AWidth, AHeight: LongInt);
    procedure SetPosition(AX, AY: Integer);
    procedure SetFocus;
  public
    { Properties }
    property LCLObject: TWinControl read FLCLObject;
    property Visible: Boolean read GetVisible write SetVisible;
    property Widget: TfpgWidget read FWidget write FWidget;
    property WidgetProtected: TFPGWidgetHack read GetWidgetProtected;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Font: TFont read GetFont write SetFont;
    property DC: HDC read FDC write SetDC;
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
    procedure MsgResize(var fpgmsg: TfpgMessageRec); message FPGM_RESIZE;
  protected
  public
    { Constructors / Destructors }
    constructor Create(ALCLObject: TWinControl; const AParams: TCreateParams); override;
    destructor Destroy; override;
    { Virtual methods }
    procedure CreateWidget(const AParams: TCreateParams); override;
    procedure SetEvents; override;
    function  HasStaticText: Boolean; override;
    procedure SetWidgetPosition(AWidget: TfpgWidget; AX, AY: Integer); override;
    procedure SetText(const AText: String); override;
    function  GetText: String; override;
    procedure GetClientRect(var ARect: TRect); override;
    procedure GetDefaultClientRect(var ARect: TRect); override;
    procedure AdjustRectXY(var AfpgRect: TfpgRect); override;
    procedure AdjustRectXYToInterface(var ARect: TRect); override;
  public
    MenuBar: TfpgMenuBar;
    { Other methods }
    function Form: TfpgForm;
    procedure SetFormBorderStyle(const AFormBorderStyle: TFormBorderStyle);
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
    procedure SetFont(const AValue: TFont); override;
  protected
  public
    { Virtual methods }
    procedure CreateWidget(const AParams: TCreateParams); override;
    function  HasStaticText: Boolean; override;
    procedure SetText(const AText: String); override;
    function  GetText: String; override;
  public
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
    procedure GetPreferredSize(var PreferredWidth,
                       PreferredHeight: integer; WithThemeSpace: Boolean);
                       override;
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
    procedure GetPreferredSize(var PreferredWidth,
                       PreferredHeight: integer; WithThemeSpace: Boolean);
                       override;
  public
    { Other methods }
    function RadioButton: TfpgRadioButton;
  end;

  { TFPGUIPrivateListBox }

  TFPGUIPrivateListBox = class(TFPGUIPrivateWidget)
  private
    procedure SetItemIndex(const AValue: integer);
    procedure SetFont(const AValue: TFont); override;
  protected
    procedure SelectionChangeHandler(Sender: TObject);
    procedure DoubleClickHandler(Sender: TObject; AButton: fpg_main.TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
  public
    { Virtual methods }
    procedure CreateWidget(const AParams: TCreateParams); override;
    function  HasStaticText: Boolean; override;
    procedure SetEvents; override;
  public
    { Other methods }
    function ListBox: TfpgListBox;
    property ItemIndex: integer write SetItemIndex;
  end;

  { TFPGUIPrivateGroupBox }

  TFPGUIPrivateGroupBox = class(TFPGUIPrivateWidget)
  private
    procedure SetFont(const AValue: TFont); override;
  protected
  public
    { Virtual methods }
    procedure CreateWidget(const AParams: TCreateParams); override;
    function  HasStaticText: Boolean; override;
    procedure SetWidgetPosition(AWidget: TfpgWidget; AX, AY: Integer); override;
    procedure SetText(const AText: String); override;
    function  GetText: String; override;
    procedure AdjustRectXY(var AfpgRect: TfpgRect); override;
    procedure AdjustRectXYToInterface(var ARect: TRect); override;
  public
    { Other methods }
    function GroupBox: TfpgGroupBox;
  end;

  { TFPGUIPrivateGroupBox }

  { TFPGUIPrivateCustomPanel }

  TFPGUIPrivateCustomPanel = class(TFPGUIPrivateWidget)
  private
  protected
  public
    { Virtual methods }
    procedure CreateWidget(const AParams: TCreateParams); override;
    function  HasStaticText: Boolean; override;
  public
    { Other methods }
    function Panel: TfpgPanel;
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
    procedure SetFont(const AValue: TFont); override;
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
    function GetStrings: TStrings;
  end;
  
  { TFPGUIPrivatePopUpMenu }

  TFPGUIPrivatePopUpMenu = class(TFPGUIPrivateWidget)
  private
    FLCLMenu: TPopUpMenu;
    FItems: TMenuItem;
  protected
  public
    { Constructors / Destructors }
    constructor Create(ALCLObject: TPopUpMenu; AItems: TMenuItem); reintroduce;
    destructor Destroy; override;
    { Virtual methods }
  public
    { Other methods }
    function PopUpMenu: TfpgPopUpMenu;
    procedure PopUp(X, Y: Integer);
  end;

var
  GlobalMouseCursorPos: TPoint;
  GlobalMouseCursorPosWidget: TFPGUIPrivateWidget;

implementation

uses
  LCLMessageGlue;

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
  AStruct: PPaintStruct;
  DC: HDC;
begin
  {$ifdef VerboseFPGUIPrivate}
    WriteLn('TFPGUIPrivateWindow.PaintHandler for ',LCLObject.Name);
  {$endif}
  DC:=GetDC(THandle(Self));
  LCLSendPaintMsg(Self.LCLObject,DC,@AStruct);
  ReleaseDC(THandle(Self),DC);
end;

procedure TFPGUIPrivateWidget.ClickHandler(Sender: TObject);
begin
  LCLSendClickedMsg(LCLObject);
end;

procedure TFPGUIPrivateWidget.EnterHandler(Sender: TObject);
begin
  if not FEntered then begin
      LCLSendSetFocusMsg(FLCLObject);
      FEntered:=true;
  end;
end;

procedure TFPGUIPrivateWidget.ExitHandler(Sender: TObject);
begin
  if FEntered then begin
    LCLSendKillFocusMsg(FLCLObject);
    FEntered:=false;
  end;
end;

procedure TFPGUIPrivateWidget.MsgDeactivate(var fpgmsg: TfpgMessageRec);
begin
  //Empty stub. To be implemented.
end;

procedure TFPGUIPrivateWidget.MsgPaint(var fpgmsg: TfpgMessageRec);
var
  Msg: TLMPaint;
  AStruct: PPaintStruct;
begin
  {$ifdef VerboseFPGUIPrivate}
    WriteLn('TFPGUIPrivateWindow.PaintHandler for ',LCLObject.Name);
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

procedure TFPGUIPrivateWidget.MsgResize(var fpgmsg: TfpgMessageRec);
begin
  LCLSendSizeMsg(LCLObject, fpgmsg.Params.rect.Width,fpgmsg.Params.rect.Height, SIZENORMAL, false);
end;

procedure TFPGUIPrivateWidget.MsgMove(var fpgmsg: TfpgMessageRec);
begin
  LCLSendMoveMsg(LCLObject, fpgmsg.Params.rect.Left, fpgmsg.Params.rect.Top,Move_Default,false);
end;

procedure TFPGUIPrivateWidget.MsgKeyChar(var fpgmsg: TfpgMessageRec);
begin
  //Empty stub, to be implemented.
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
var
  TmpRect: TRect;
begin
  TmpRect.TopLeft:=Point(fpgmsg.Params.mouse.x,fpgmsg.Params.mouse.y);
  AdjustRectXYToInterface(TmpRect);
  GlobalMouseCursorPos:=TmpRect.TopLeft;
  GlobalMouseCursorPosWidget:=Self;
  LCLSendMouseMoveMsg(LCLObject, TmpRect.Left, TmpRect.Top, fpgmsg.Params.mouse.shiftstate);
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

function TFPGUIPrivateWidget.GetEnabled: Boolean;
begin
  Result:=FWidget.Enabled;
end;

function TFPGUIPrivateWidget.GetFont: TFont;
begin
  Result:=nil;
end;

function TFPGUIPrivateWidget.GetWidgetProtected: TFPGWidgetHack;
begin
  Result := TFPGWidgetHack(FWidget);
end;

procedure TFPGUIPrivateWidget.SetDC(const AValue: HDC);
begin
  FDC:=AValue;
end;

procedure TFPGUIPrivateWidget.SetEnabled(const AValue: Boolean);
begin
  FWidget.Enabled:=AValue;
end;

procedure TFPGUIPrivateWidget.SetFont(const AValue: TFont);
begin
  //Widget does not have a default font for all widgets, one
  //by one must be implemented.
end;

constructor TFPGUIPrivateWidget.Create(ALCLObject: TWinControl; const AParams: TCreateParams);
begin
  FLCLObject := ALCLObject;

  CreateWidget(AParams);

  SetEvents;
end;

destructor TFPGUIPrivateWidget.Destroy;
begin
  if (FWidget <> nil) then begin
    //Be sure that if the object is the active object in parent
    //unset the activewidget. Anothe possibility is to forward
    //active widget to next in parent
    if FWidget.Parent<>nil then begin
      if FWidget.Parent.ActiveWidget=FWidget then begin
        FWidget.Parent.ActiveWidget:=nil;
      end;
    end;
    FreeAndNil(FWidget);
  end;

  inherited Destroy;
end;

procedure TFPGUIPrivateWidget.CreateWidget(const AParams: TCreateParams);
begin
  Widget := TfpgWidget.Create(nil);
end;

procedure TFPGUIPrivateWidget.SetEvents;
begin
  WidgetProtected.OnClick       := ClickHandler;
//  WidgetProtected.OnResize      := ResizeHandler;
  WidgetProtected.OnEnter       := EnterHandler;
  WidgetProtected.OnExit        := ExitHandler;
//  WidgetProtected.OnKeyPress    := KeyHandler;
//  WidgetProtected.OnMouseEnter  := MouseEnterHandler;
//  WidgetProtected.OnMouseExit   := MouseExitHandler;
  WidgetProtected.OnPaint       := PaintHandler;
//  WidgetProtected.OnMouseMove   := MouseMoveHandler;

  //WidgetProtected.OnMove  := MoveHandler;
  fpgApplication.SetMessageHook(Widget, FPGM_MOVE, Self);
  fpgApplication.SetMessageHook(Widget, FPGM_RESIZE, Self);
  fpgApplication.SetMessageHook(Widget, FPGM_KEYPRESS, Self);
  fpgApplication.SetMessageHook(Widget, FPGM_KEYRELEASE, Self);
  fpgApplication.SetMessageHook(Widget, FPGM_MOUSEMOVE, Self);
//  fpgApplication.SetMessageHook(Widget, FPGM_PAINT, Self);
(*  fpgApplication.SetMessageHook(Widget, FPGM_ACTIVATE, Self);
  fpgApplication.SetMessageHook(Widget, FPGM_DEACTIVATE, Self);
  fpgApplication.SetMessageHook(Widget, FPGM_KEYCHAR, Self);
  fpgApplication.SetMessageHook(Widget, FPGM_MOUSEDOWN, Self);
  fpgApplication.SetMessageHook(Widget, FPGM_MOUSEUP, Self);
  fpgApplication.SetMessageHook(Widget, FPGM_DOUBLECLICK, Self);
  fpgApplication.SetMessageHook(Widget, FPGM_MOUSEENTER, Self);
  fpgApplication.SetMessageHook(Widget, FPGM_MOUSEEXIT, Self);
  fpgApplication.SetMessageHook(Widget, FPGM_CLOSE, Self);
  fpgApplication.SetMessageHook(Widget, FPGM_SCROLL, Self);
  fpgApplication.SetMessageHook(Widget, FPGM_POPUPCLOSE, Self);
  fpgApplication.SetMessageHook(Widget, FPGM_HINTTIMER, Self);*)
end;

procedure TFPGUIPrivateWidget.SetSize(AWidth, AHeight: LongInt);
begin
  FWidget.SetPosition(FWidget.Left, FWidget.Top, AWidth, AHeight);
  LCLSendSizeMsg(FLCLObject,AWidth,AHeight,SIZENORMAL,false);
end;

procedure TFPGUIPrivateWidget.SetPosition(AX, AY: Integer);
begin
  if (FWidget.Parent<>nil) then begin
    //Request parent object to put the widget coordinates. This is needed for
    //widgets like TForm which reduces its clientrect when a menubar is visible.
    TFPGUIPrivateWidget(FLCLObject.Parent.Handle).SetWidgetPosition(FWidget,AX,AY);
  end else begin
    FWidget.SetPosition(AX, AY, FWidget.Width, FWidget.Height);
  end;
  LCLSendMoveMsg(FLCLObject,AX,AY,Move_Default,false);
end;

procedure TFPGUIPrivateWidget.SetWidgetPosition(AWidget: TfpgWidget; AX,
  AY: Integer);
begin
  if AWidget=nil then exit;
  AWidget.SetPosition(AX,AY,AWidget.Width,AWidget.Height);
end;

function TFPGUIPrivateWidget.HasStaticText: Boolean;
begin
  Result := False;
end;

procedure TFPGUIPrivateWidget.SetText(const AText: String);
begin
  //Do nothing, empty stub
end;

function TFPGUIPrivateWidget.GetText: String;
begin
  Result := '';
end;

procedure TFPGUIPrivateWidget.GetPreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  PreferredWidth:=FWidget.Width;
  PreferredHeight:=FWidget.Height;
end;

procedure TFPGUIPrivateWidget.GetClientRect(var ARect: TRect);
var
  CLRect: TfpgRect;
begin
  //ClientRect must have Left and Top = (0,0)
  CLRect:=FWidget.GetClientRect;
  ARect.Left:=0;
  ARect.Top:=0;
  ARect.Right:=CLRect.Width-CLRect.Left;
  ARect.Bottom:=CLRect.Height-CLRect.Top;
end;

procedure TFPGUIPrivateWidget.GetDefaultClientRect(var ARect: TRect);
begin
  TfpgRectToRect(FWidget.GetClientRect,ARect);
end;

procedure TFPGUIPrivateWidget.AdjustRectXY(var AfpgRect: TfpgRect);
begin
  //By default widgets do not need adjust, except form with menu, groupbox,...
end;

procedure TFPGUIPrivateWidget.AdjustRectXYToInterface(var ARect: TRect);
begin
  //By default widgets do not need adjust, except form with menu, groupbox,...
end;

procedure TFPGUIPrivateWidget.SetFocus;
begin
  if (not Widget.Focused) and Widget.Focusable then
    Widget.SetFocus;
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

procedure TFPGUIPrivateWindow.SetWidgetPosition(AWidget: TfpgWidget; AX,
  AY: Integer);
begin
  if MenuBar.Visible then
    AWidget.SetPosition(AX,AY+MenuBar.Height,AWidget.Width,AWidget.Height)
  else
    inherited;
end;

procedure TFPGUIPrivateWindow.CloseHandler(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  ClosePopups;
  CloseAction := caFree;
  if LCLSendCloseQueryMsg(LCLObject) = 0 then
    CloseAction := caNone;
end;

procedure TFPGUIPrivateWindow.MsgResize(var fpgmsg: TfpgMessageRec);
begin
  if (fpgmsg.Sender is TfpgMenuBar) then begin
    //Invalidate clientRectCache to force LCL to recalibrate
    //the available space in the form after a menubar visibility change.
    //Children are needed to invalidate for TLabel and others custom draw.
    FLCLObject.InvalidateClientRectCache(true);
  end;
  Inherited;
end;

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateWindow.Create
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
constructor TFPGUIPrivateWindow.Create(ALCLObject: TWinControl; const AParams: TCreateParams);
var
  FormWidget: TfpgForm;
begin
  inherited Create(ALCLObject, AParams);
  FormWidget:=Form;
  FormWidget.WindowPosition:=wpUser; //User controled by LCL
  FormWidget.SetPosition(AParams.X,AParams.Y,AParams.Width,AParams.Height);
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

  MenuBar := TfpgMenuBar.Create(Widget);
  MenuBar.Visible := false;
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
  // which will do a delayed close
  Form.OnClose:=nil; //Disconnect the handler
  Form.Close;
  Form.Free;

  // By setting the Widget to nil we prevent it's future
  // destruction
  Widget:=nil;
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

procedure TFPGUIPrivateWindow.GetClientRect(var ARect: TRect);
begin
  if MenuBar.Visible then begin
    ARect:=Rect(0,0,FWidget.Width,FWidget.Height-MenuBar.Height);
  end else begin
    ARect:=Rect(0,0,FWidget.Width,FWidget.Height);
  end;
end;

procedure TFPGUIPrivateWindow.GetDefaultClientRect(var ARect: TRect);
begin
  inherited GetDefaultClientRect(ARect);
  if MenuBar.Visible then begin
    ARect.Bottom:=ARect.Bottom-MenuBar.Height;
  end;
end;

procedure TFPGUIPrivateWindow.AdjustRectXY(var AfpgRect: TfpgRect);
begin
  if MenuBar.Visible then begin
    AfpgRect.Top:=AfpgRect.Top+MenuBar.Height;
  end;
end;

procedure TFPGUIPrivateWindow.AdjustRectXYToInterface(var ARect: TRect);
begin
  if MenuBar.Visible then begin
    ARect.Top:=ARect.Top-MenuBar.Height;
  end;
end;

procedure TFPGUIPrivateWindow.SetFormBorderStyle(
  const AFormBorderStyle: TFormBorderStyle);
begin
  case AFormBorderStyle of
    controls.bsNone: Widget.WindowAttributes:=Widget.WindowAttributes+[waBorderless];
    controls.bsSingle: Widget.WindowAttributes:=Widget.WindowAttributes-[waBorderless,waSizeable];
    controls.bsSizeable: Widget.WindowAttributes:=Widget.WindowAttributes+[waSizeable];
    controls.bsDialog: Widget.WindowAttributes:=Widget.WindowAttributes-[waSizeable,waBorderless];
    controls.bsToolWindow: Widget.WindowAttributes:=Widget.WindowAttributes-[waSizeable,waBorderless];
    controls.bsSizeToolWin: Widget.WindowAttributes:=Widget.WindowAttributes-[waBorderless];
  end;
  if not (waSizeable in Widget.WindowAttributes) then begin
    Form.Sizeable:=false;
  end else begin
    Form.Sizeable:=true;
  end;
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

procedure TFPGUIPrivateButton.SetFont(const AValue: TFont);
begin
  Button.FontDesc:=TFontToTfpgFontDesc(AValue);
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
end;

function TFPGUIPrivateButton.HasStaticText: Boolean;
begin
  Result := True;
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

procedure TFPGUIPrivateCheckBox.GetPreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
var
  Check: TfpgCheckBox;
begin
  inherited GetPreferredSize(PreferredWidth, PreferredHeight, WithThemeSpace);
  Check:=TfpgCheckBox(FWidget);
  PreferredWidth:=Check.Font.TextWidth(Check.Text)+24; //This is hardcoded in fpgui for image
  PreferredHeight:=Check.Font.Height+2;
end;

{ TFPGUIPrivateRadioButton }

function TFPGUIPrivateRadioButton.RadioButton: TfpgRadioButton;
begin
  Result := TfpgRadioButton(Widget);
end;

procedure TFPGUIPrivateRadioButton.CreateWidget(const AParams: TCreateParams);
begin
  Widget := TfpgRadioButton.Create(GetParentContainerWidget());
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

procedure TFPGUIPrivateRadioButton.GetPreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  //Autosize=true forces the control to calculate its preferred size.
  TfpgRadioButton(FWidget).AutoSize:=true;
  inherited GetPreferredSize(PreferredWidth, PreferredHeight, WithThemeSpace);
  if PreferredHeight<TfpgRadioButton(FWidget).Font.Height then begin
    PreferredHeight:=TfpgRadioButton(FWidget).Font.Height+2;
  end;
  TfpgRadioButton(FWidget).AutoSize:=false;
end;

{ TFPGUIPrivateNotebook }

procedure TFPGUIPrivatePageControl.CreateWidget(const AParams: TCreateParams);
begin
  Widget := TfpgPageControl.Create(GetParentContainerWidget());
end;

{ TFPGUIPrivateMemo }

procedure TFPGUIPrivateMemo.SetFont(const AValue: TFont);
begin
  Memo.FontDesc:=TFontToTfpgFontDesc(AValue);
end;

procedure TFPGUIPrivateMemo.CreateWidget(const AParams: TCreateParams);
begin
  Widget := TfpgMemo.Create(GetParentContainerWidget());
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

function TFPGUIPrivateMemo.GetStrings: TStrings;
begin
  Result:=Memo.Lines;
end;

{ TFPGUIPrivatePopUpMenu }

constructor TFPGUIPrivatePopUpMenu.Create(ALCLObject: TPopUpMenu; AItems: TMenuItem);
begin
  FLCLMenu := ALCLObject;
  FItems := AItems;

  // CreateWidget

  Widget := TfpgPopUpMenu.Create(nil);
end;

destructor TFPGUIPrivatePopUpMenu.Destroy;
begin
  PopUpMenu.Close;
  Inherited;
end;

function TFPGUIPrivatePopUpMenu.PopUpMenu: TfpgPopUpMenu;
begin
  Result := TfpgPopUpMenu(Widget);
end;

procedure TFPGUIPrivatePopUpMenu.PopUp(X, Y: Integer);
begin
  PopUpMenu.ShowAt(X, Y);
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
var
  ResultCode: TfpgModalResult;
begin
  ResultCode:=Dialog.ShowModal;
  case ResultCode of
    mrNone: Result:=false;
(*    mrOK: ;
    mrCancel: ;
    mrYes: ;
    mrNo: ;
    mrAbort: ;
    mrRetry: ;
    mrIgnore: ;
    mrAll: ;
    mrNoToAll: ;
    mrYesToAll: ;*)
    else
      Result:=true;
  end;
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
  { TODO : Map fpguimodalresult to LCL expected modal result }
  LCLDialog.UserChoice := integer(Dialog.ModalResult);
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
  Result:=Boolean(FontDialog.ShowModal);
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

{ TFPGUIPrivateListBox }

procedure TFPGUIPrivateListBox.SetItemIndex(const AValue: integer);
begin
  ListBox.FocusItem:=AValue;
end;

procedure TFPGUIPrivateListBox.SetFont(const AValue: TFont);
begin
  ListBox.FontDesc:=TFontToTfpgFontDesc(AValue);
end;

procedure TFPGUIPrivateListBox.SelectionChangeHandler(Sender: TObject);
begin
  LCLSendSelectionChangedMsg(FLCLObject);
end;

procedure TFPGUIPrivateListBox.DoubleClickHandler(Sender: TObject;
  AButton: fpg_main.TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
begin
  LCLSendMouseMultiClickMsg(FLCLObject,AMousePos.x,AMousePos.y,controls.TMouseButton(AButton),2,AShift);
end;

procedure TFPGUIPrivateListBox.CreateWidget(const AParams: TCreateParams);
begin
  Widget:=TfpgListBox.Create(GetParentContainerWidget());
end;

function TFPGUIPrivateListBox.HasStaticText: Boolean;
begin
  Result:=false;
end;

procedure TFPGUIPrivateListBox.SetEvents;
begin
  inherited SetEvents;
  ListBox.OnChange:=SelectionChangeHandler;
  ListBox.OnDoubleClick:=DoubleClickHandler;
end;

function TFPGUIPrivateListBox.ListBox: TfpgListBox;
begin
  Result:=TfpgListBox(FWidget);
end;

{ TFPGUIPrivateGroupBox }

procedure TFPGUIPrivateGroupBox.SetFont(const AValue: TFont);
begin
  GroupBox.FontDesc:=TFontToTfpgFontDesc(AValue);
end;

procedure TFPGUIPrivateGroupBox.CreateWidget(const AParams: TCreateParams);
begin
  Widget:=TfpgGroupBox.Create(GetParentContainerWidget());
end;

function TFPGUIPrivateGroupBox.HasStaticText: Boolean;
begin
  Result:=true;
end;

procedure TFPGUIPrivateGroupBox.SetWidgetPosition(AWidget: TfpgWidget; AX,
  AY: Integer);
var
  CLRect: TfpgRect;
begin
  CLRect:=FWidget.GetClientRect;
  AWidget.SetPosition(AX+CLRect.Left,AY+CLRect.Top,AWidget.Width,AWidget.Height);
end;

procedure TFPGUIPrivateGroupBox.SetText(const AText: String);
begin
  GroupBox.Text:=AText;
end;

function TFPGUIPrivateGroupBox.GetText: String;
begin
  Result:=GroupBox.Text;
end;

procedure TFPGUIPrivateGroupBox.AdjustRectXY(var AfpgRect: TfpgRect);
var
  CLRect: TfpgRect;
begin
  CLRect:=FWidget.GetClientRect;
  AfpgRect.Top:=AfpgRect.Top+CLRect.Top;
  AfpgRect.Left:=AfpgRect.Left+CLRect.Left;
end;

procedure TFPGUIPrivateGroupBox.AdjustRectXYToInterface(var ARect: TRect);
var
  CLRect: TfpgRect;
begin
  CLRect:=FWidget.GetClientRect;
  ARect.Top:=ARect.Top-CLRect.Top;
  ARect.Left:=ARect.Left-CLRect.Left;
end;

function TFPGUIPrivateGroupBox.GroupBox: TfpgGroupBox;
begin
  Result:=TfpgGroupBox(FWidget);
end;

{ TFPGUIPrivateCustomPanel }

procedure TFPGUIPrivateCustomPanel.CreateWidget(const AParams: TCreateParams);
begin
  FWidget:=TfpgPanel.Create(GetParentContainerWidget());
  Panel.Text:='';
end;

function TFPGUIPrivateCustomPanel.HasStaticText: Boolean;
begin
  Result:=false;
end;

function TFPGUIPrivateCustomPanel.Panel: TfpgPanel;
begin
  Result:=TfpgPanel(FWidget);
end;

end.
  
