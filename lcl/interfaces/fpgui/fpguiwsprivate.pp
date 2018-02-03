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
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

unit fpguiwsprivate;
{.$DEFINE VerboseFPGUIPrivate}
{.$DEFINE FPGUIDEBUGFOCUS}

// Define base widget, possible values: BASEWIDGET_WIDGET, BASEWIDGET_SCROLLFRAME, BASEWIDGET_PANEL
{$DEFINE BASEWIDGET_WIDGET}
{$mode delphi}

interface

uses
  // LCL
  LCLType, LMessages, LCLProc, Controls, Classes, SysUtils, Forms,
  LCLIntf, Menus, Dialogs, ExtCtrls, Graphics, StdCtrls,
  // widgetset
  WSControls, WSLCLClasses, WSProc,
  // interface
  fpg_main,
  fpg_widget, fpg_form, fpg_button, fpg_combobox, fpg_editcombo, fpg_dialogs,
  fpg_edit, fpg_checkbox, fpg_radiobutton, fpg_tab, fpg_memo,
  fpg_menu, fpg_label, fpg_listbox, fpg_panel, fpg_scrollbar, fpg_splitter,
  fpg_popupwindow, fpg_base, fpg_progressbar, fpguiproc, fpg_stylemanager,
  fpg_scrollframe
  ;


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
    FDefaultCursor: TMouseCursor;
    function GetEnabled: Boolean;
    function GetFont: TFont; virtual;
    function GetVisible: Boolean;
    function GetWidgetProtected: TFPGWidgetHack;
    procedure SetDC(const AValue: HDC);
    procedure SetEnabled(const AValue: Boolean);
    procedure SetFont(const AValue: TFont); virtual;
    procedure SetVisible(const AValue: Boolean);
    { Handlers for default properties common to all TfpgWidget descendants}
    procedure PaintHandler(Sender: TObject); virtual;
    procedure ClickHandler(Sender: TObject);
    procedure EnterHandler(Sender: TObject);
    procedure ExitHandler(Sender: TObject);
    procedure ResizeHandler(Sender: TObject);
    procedure KeyCharHandler(Sender: TObject; AChar: TfpgChar; var Consumed: boolean);
    procedure KeyPressHandler(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean);
    procedure MouseMoveHandler(Sender: TObject; AShift: TShiftState; const AMousePos: TPoint);
    procedure MouseEnterHandler(Sender: TObject);
    procedure MouseExitHandler(Sender: TObject);
    procedure MouseDownHandler(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure MouseUpHandler(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);

    procedure   MsgDeactivate(var fpgmsg: TfpgMessageRec); message FPGM_DEACTIVATE;
    procedure   MsgActivate(var fpgmsg: TfpgMessageRec); message FPGM_ACTIVATE;
    procedure   MsgMove(var fpgmsg: TfpgMessageRec); message FPGM_MOVE;
    procedure   MsgKeyRelease(var fpgmsg: TfpgMessageRec); message FPGM_KEYRELEASE;
    procedure   MsgDoubleClick(var fpgmsg: TfpgMessageRec); message FPGM_DOUBLECLICK;
    procedure   MsgMouseScroll(var fpgmsg: TfpgMessageRec); message FPGM_SCROLL;
    procedure   MsgUser(var fpgmsg: TfpgMessageRec); message FPGM_USER;
    //    procedure   MsgMouseMove(var fpgmsg: TfpgMessageRec); message FPGM_MOUSEMOVE;
    //    procedure   MsgMouseDown(var fpgmsg: TfpgMessageRec); message FPGM_MOUSEDOWN;
    //    procedure   MsgMouseUp(var fpgmsg: TfpgMessageRec); message FPGM_MOUSEUP;
    //    procedure   MsgMouseEnter(var fpgmsg: TfpgMessageRec); message FPGM_MOUSEENTER;
    //    procedure   MsgMouseExit(var fpgmsg: TfpgMessageRec); message FPGM_MOUSEEXIT;
    //    procedure   MsgPaint(var fpgmsg: TfpgMessageRec); message FPGM_PAINT;
    //    procedure   MsgResize(var fpgmsg: TfpgMessageRec); message FPGM_RESIZE;
    //    procedure   MsgKeyChar(var fpgmsg: TfpgMessageRec); message FPGM_KEYCHAR;
    //    procedure   MsgKeyPress(var fpgmsg: TfpgMessageRec); message FPGM_KEYPRESS;
  protected
    { Helper methods for descendents }
    function GetParentContainerWidget: TfpgWidget;
    function GetParentContainerPrivateWidgetFocusable: TFPGUIPrivateWidget;
    procedure SetWidgetPosition(AWidget: TfpgWidget; AX, AY: Integer); virtual;
  public
    { Constructors / Destructors }
    constructor Create(ALCLObject: TWinControl; const AParams: TCreateParams); virtual;
    destructor Destroy; override;
    { Virtual methods }
    procedure CreateWidget(const AParams: TCreateParams); virtual;
    procedure SetEvents; virtual;
    procedure UnSetEvents; virtual;
    function  HasStaticText: Boolean; virtual;
    procedure SetText(const AText: String); virtual;
    function  GetText: String; virtual;
    procedure GetPreferredSize(var PreferredWidth, PreferredHeight: integer;
                               WithThemeSpace: Boolean); virtual;
    procedure GetClientRect(var ARect: TRect); virtual;
    procedure GetDefaultClientRect(var ARect: TRect); virtual;
    procedure GetWindowRect(var ARect: TRect); virtual;
    procedure AdjustRectXY(var AfpgRect: TfpgRect); virtual;
    procedure AdjustRectXYToInterface(var ARect: TRect); virtual;
    procedure AdjustPointXY(var APoint: TPoint); virtual;
    procedure AdjustPointXYToInterface(var APoint: TPoint); virtual;
    procedure SetCursor(const aCursor: integer); virtual;
    { Non-Virtual }
    procedure SetSize(AWidth, AHeight: LongInt);
    procedure SetPosition(AX, AY: Integer);
    procedure SetFocus;
    procedure Paint; virtual;
    procedure Clear; virtual;
    procedure Invalidate; virtual;
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
    procedure UnSetEvents; override;
    function  HasStaticText: Boolean; override;
    procedure SetWidgetPosition(AWidget: TfpgWidget; AX, AY: Integer); override;
    procedure SetText(const AText: String); override;
    function  GetText: String; override;
    procedure GetClientRect(var ARect: TRect); override;
    procedure GetDefaultClientRect(var ARect: TRect); override;
    procedure AdjustRectXY(var AfpgRect: TfpgRect); override;
    procedure AdjustRectXYToInterface(var ARect: TRect); override;
    procedure AdjustPointXY(var APoint: TPoint); override;
    procedure AdjustPointXYToInterface(var APoint: TPoint); override;
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

  { TFPGUIPrivateColorDialog }

  TFPGUIPrivateColorDialog = class(TFPGUIPrivateCommonDialog)
  protected
    procedure CreateDialog; override;
//    function  InternalShowDialog: Boolean; override;
    procedure UpdatePropsBefore; override;
    procedure UpdatePropsAfter; override;
  public
    function  ColorDialog: TfpgColorSelectDialog;
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
    procedure GetPreferredSize(var PreferredWidth, PreferredHeight: integer;
                               WithThemeSpace: Boolean); override;
  public
    destructor Destroy; override;
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
    procedure SetFont(const AValue: TFont); override;
  protected
    procedure HandleChange(Sender: TObject);
    procedure HandleDropDown(Sender: TObject);
    procedure HandleCloseUp(Sender: TObject);
  public
    { Virtual methods }
    procedure CreateWidget(const AParams: TCreateParams); override;
    procedure SetEvents; override;
    function  HasStaticText: Boolean; override;
    procedure SetText(const AText: String); override;
    function  GetText: String; override;
  public
    { Other methods }
    function ComboBox: TfpgEditCombo;
  end;


  { TFPGUIPrivateEdit }

  TFPGUIPrivateEdit = class(TFPGUIPrivateWidget)
  private
    procedure SetFont(const AValue: TFont); override;
  protected
  public
    { Virtual methods }
    procedure CreateWidget(const AParams: TCreateParams); override;
    function  HasStaticText: Boolean; override;
    procedure SetText(const AText: String); override;
    function  GetText: String; override;
    procedure GetPreferredSize(var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); override;
  public
    { Other methods }
    function Edit: TfpgEdit;
  end;

  { TFPGUIPrivateCheckBox }

  TFPGUIPrivateCheckBox = class(TFPGUIPrivateWidget)
  private
    procedure SetFont(const AValue: TFont); override;
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
    procedure SetFont(const AValue: TFont); override;
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
    function GetItemIndex: integer;
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
    property ItemIndex: integer read GetItemIndex write SetItemIndex;
  end;

  { TFPGUIPrivateGroupBox }

  TFPGUIPrivateGroupBox = class(TFPGUIPrivateContainer)
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
    procedure AdjustPointXYToInterface(var APoint: TPoint); override;
  public
    { Other methods }
    function GroupBox: TfpgGroupBox;
  end;

  { TFPGUIPrivateCustomPanel }

  TFPGUIPrivateCustomPanel = class(TFPGUIPrivateContainer)
  private
    procedure SetFont(const AValue: TFont); override;
  protected
    procedure PaintHandler(Sender: TObject); override;
  public
    { Virtual methods }
    procedure CreateWidget(const AParams: TCreateParams); override;
    function  HasStaticText: Boolean; override;
    procedure SetWidgetPosition(AWidget: TfpgWidget; AX, AY: Integer); override;
    procedure AdjustRectXY(var AfpgRect: TfpgRect); override;
    procedure AdjustPointXY(var APoint: TPoint); override;
    procedure AdjustRectXYToInterface(var ARect: TRect); override;
    procedure AdjustPointXYToInterface(var APoint: TPoint); override;
  public
    { Other methods }
    procedure SetText(const AText: String); override;
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
    procedure GetPreferredSize(var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); override;
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

  { TFPGUIPrivateScrollBar }

  TFPGUIPrivateScrollBar = class(TFPGUIPrivateWidget)
  private
    function GetHorizontal: Boolean;
    function GetMax: integer;
    function GetMin: integer;
    function GetPosition: integer;
    procedure ScrollHandler(Sender: TObject; position: integer);
    procedure SetHorizontal(AValue: Boolean);
    procedure SetMax(AValue: integer);
    procedure SetMin(AValue: integer);
    procedure SetPosition(AValue: integer);
  protected
    function  DeliverMessage(var Msg): LRESULT;
  public
    { Virtual methods }
    procedure CreateWidget(const AParams: TCreateParams); override;
  public
    function ScrollBar: TfpgScrollBar;
    property Min: integer read GetMin write SetMin;
    property Max: integer read GetMax write SetMax;
    property Position: integer read GetPosition write SetPosition;
    property Horizontal: Boolean read GetHorizontal write SetHorizontal;
  end;

  TFPGUIPrivateProgressBar = class(TFPGUIPrivateWidget)
  public
    procedure CreateWidget(const AParams: TCreateParams); override;
  public
    function ProgressBar: TfpgProgressBar;
  end;

  { TFPGUIPrivateSplitter }

  TFPGUIPrivateSplitter = class(TFPGUIPrivateWidget)
  public
    procedure CreateWidget(const AParams: TCreateParams); override;
  public
    function Splitter: TfpgSplitter;
  end;

  { TFPGUIPrivateStaticText }

  TFPGUIPrivateStaticText = class(TFPGUIPrivateWidget)
  private
    procedure SetFont(const AValue: TFont); override;
  public
    procedure CreateWidget(const AParams: TCreateParams); override;
    function  HasStaticText: Boolean; override;
    procedure SetText(const AText: String); override;
    function  GetText: String; override;
  public
    function StaticText: TfpgLabel;
  end;

  { TFPGUIPrivateScrollingWinControl }

  TFPGUIPrivateScrollingWinControl = class(TFPGUIPrivateContainer)
  public
    procedure CreateWidget(const AParams: TCreateParams); override;
  public
    function ScrollFrame: TfpgScrollFrame;
    procedure Invalidate; override;
  end;

var
  GlobalMouseCursorPos: TPoint;
  GlobalMouseCursorPosPrivateWidget: TFPGUIPrivateWidget;
  GlobalMouseCapturedPrivateWidget: TFPGUIPrivateWidget;
  GlobalFocusedPrivateWidget: TFPGUIPrivateWidget;

implementation

uses
  LCLMessageGlue;

{ TFPGUIPrivateScrollingWinControl }

procedure TFPGUIPrivateScrollingWinControl.CreateWidget(
  const AParams: TCreateParams);
begin
  Widget := TfpgScrollFrame.Create(GetParentContainerWidget());
end;

function TFPGUIPrivateScrollingWinControl.ScrollFrame: TfpgScrollFrame;
begin
  Result:=TfpgScrollFrame(Widget);
end;

procedure TFPGUIPrivateScrollingWinControl.Invalidate;
begin
  inherited Invalidate;
  ScrollFrame.ContentFrame.Invalidate;
end;

{ TFPGUIPrivateStaticText }

procedure TFPGUIPrivateStaticText.SetFont(const AValue: TFont);
begin
  StaticText.FontDesc:=TFontToTfpgFontDesc(AValue);
end;

procedure TFPGUIPrivateStaticText.CreateWidget(const AParams: TCreateParams);
begin
  Widget := TfpgLabel.Create(GetParentContainerWidget());
end;

function TFPGUIPrivateStaticText.HasStaticText: Boolean;
begin
  Result:=true;
end;

procedure TFPGUIPrivateStaticText.SetText(const AText: String);
begin
  StaticText.Text:=AText;
end;

function TFPGUIPrivateStaticText.GetText: String;
begin
  Result:=StaticText.Text;
end;

function TFPGUIPrivateStaticText.StaticText: TfpgLabel;
begin
  Result:=TfpgLabel(Widget);
end;

{ TFPGUIPrivateSplitter }

procedure TFPGUIPrivateSplitter.CreateWidget(const AParams: TCreateParams);
begin
  Widget := TfpgSplitter.Create(GetParentContainerWidget());
  Splitter.AutoSnap:=false;
  (*
  case FLCLObject.Align of
    controls.TAlign.alNone:   Splitter.Align:=alNone;
    controls.TAlign.alTop:    Splitter.Align:=alTop;
    controls.TAlign.alBottom: Splitter.Align:=alBottom;
    controls.TAlign.alLeft:   Splitter.Align:=alLeft;
    controls.TAlign.alRight:  Splitter.Align:=alRight;
    controls.TAlign.alClient: Splitter.Align:=alClient;
    controls.TAlign.alCustom: Splitter.Align:=alNone;
  end;
  *)
end;

function TFPGUIPrivateSplitter.Splitter: TfpgSplitter;
begin
  Result:=TfpgSplitter(Widget);
end;

{ TFPGUIPrivateScrollBar }

function TFPGUIPrivateScrollBar.GetHorizontal: Boolean;
begin
  if ScrollBar.Orientation=orHorizontal then begin
    Result:=true;
  end else begin
    Result:=false;
  end;
end;

function TFPGUIPrivateScrollBar.GetMax: integer;
begin
  Result:=ScrollBar.Max;
end;

function TFPGUIPrivateScrollBar.GetMin: integer;
begin
  Result:=ScrollBar.Min;
end;

function TFPGUIPrivateScrollBar.GetPosition: integer;
begin
  Result:=ScrollBar.Position;
end;

procedure TFPGUIPrivateScrollBar.ScrollHandler(Sender: TObject;
  position: integer);
var
  Msg: TLMScroll;
begin
  FillChar(Msg{%H-}, SizeOf(Msg), #0);
  if ScrollBar.Orientation=orHorizontal then begin
    Msg.Msg := LM_HSCROLL;
  end else begin
    Msg.Msg := LM_VSCROLL;
  end;
  Msg.Pos:=ScrollBar.Position;
  Msg.ScrollBar:=HWND(LCLObject);
  Msg.ScrollCode:=4; //scPosition
  DeliverMessage(Msg);
end;

procedure TFPGUIPrivateScrollBar.SetHorizontal(AValue: Boolean);
begin
  if AValue then begin
    ScrollBar.Orientation:=orHorizontal;
  end else begin
    ScrollBar.Orientation:=orVertical;
  end;
end;

procedure TFPGUIPrivateScrollBar.SetMax(AValue: integer);
begin
  ScrollBar.Max:=AValue;
end;

procedure TFPGUIPrivateScrollBar.SetMin(AValue: integer);
begin
  ScrollBar.Min:=AValue;
end;

procedure TFPGUIPrivateScrollBar.SetPosition(AValue: integer);
begin
  ScrollBar.Position:=AValue;
end;

function TFPGUIPrivateScrollBar.DeliverMessage(var Msg): LRESULT;
begin
  LCLObject.WindowProc(TLMessage(Msg));
  Result:=TLMessage(Msg).Result;
end;

procedure TFPGUIPrivateScrollBar.CreateWidget(const AParams: TCreateParams);
begin
  Widget := TfpgScrollBar.Create(GetParentContainerWidget());
  ScrollBar.OnScroll:=ScrollHandler;
end;

function TFPGUIPrivateScrollBar.ScrollBar: TfpgScrollBar;
begin
  Result:=TfpgScrollBar(Widget);
end;

{ TFPGUIPrivateColorDialog }

procedure TFPGUIPrivateColorDialog.CreateDialog;
begin
  Dialog := TfpgColorSelectDialog.Create(nil);
end;

procedure TFPGUIPrivateColorDialog.UpdatePropsBefore;
begin
  inherited UpdatePropsBefore;
  TfpgColorSelectDialog(Dialog).SelectedColor:=TColorToTfpgColor(TColorDialog(LCLDialog).Color);
end;

procedure TFPGUIPrivateColorDialog.UpdatePropsAfter;
begin
  inherited UpdatePropsAfter;
  TColorDialog(LCLDialog).Color:=TTfpgColorToTColor(TfpgColorSelectDialog(Dialog).SelectedColor);
end;

function TFPGUIPrivateColorDialog.ColorDialog: TfpgColorSelectDialog;
begin
  Result:=TfpgColorSelectDialog(Dialog);
end;

{ TFPGUIPrivateProgressBar }

procedure TFPGUIPrivateProgressBar.CreateWidget(const AParams: TCreateParams);
begin
  Widget := TfpgProgressBar.Create(GetParentContainerWidget());
end;

function TFPGUIPrivateProgressBar.ProgressBar: TfpgProgressBar;
begin
  Result := TfpgProgressBar(Widget);
end;

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
  if (AValue=false) and (GlobalFocusedPrivateWidget=Self) then begin
    ExitHandler(Self);
  end;
end;

procedure TFPGUIPrivateWidget.PaintHandler(Sender: TObject{; const ARect: TfpgRect});
var
  AStruct: PaintStruct;
  lDC: HDC;
begin
  if not Assigned(LCLObject) or not Assigned(FWidget) then exit;
  {$ifdef VerboseFPGUIPrivate}
    WriteLn('TFPGUIPrivateWindow.PaintHandler for ',LCLObject.Name);
  {$endif}
  lDC:=Self.DC;
  FillChar(AStruct,sizeof(AStruct),0);
  AStruct.hdc:=lDC;
  AStruct.fRestore:=true;
  Self.GetClientRect(AStruct.rcPaint);
  LCLSendPaintMsg(LCLObject,lDC,@AStruct);
end;

procedure TFPGUIPrivateWidget.ClickHandler(Sender: TObject);
begin
  LCLSendClickedMsg(LCLObject);
end;

procedure TFPGUIPrivateWidget.EnterHandler(Sender: TObject);
begin
  if not FEntered then begin
    if not Assigned(FWidget) then begin
      GlobalFocusedPrivateWidget:=nil;
      exit;
    end;
  (*
  if not FWidget.Focusable then begin
    writeln(TimeToStr(now),' ',FLCLObject.Name,' Not focusable ',self.Widget.Width,',',self.Widget.Height);
    exit;
  end;
  *)
    GlobalFocusedPrivateWidget:=Self;
    FEntered:=true;
    LCLSendSetFocusMsg(FLCLObject);
    {$IFDEF FPGUIDEBUGFOCUS}
    writeln(TimeToStr(now),' ','Enter ',FLCLObject.Name,' ',self.Widget.Width,',',self.Widget.Height);
    {$ENDIF}
  end;
end;

procedure TFPGUIPrivateWidget.ExitHandler(Sender: TObject);
var
  PrivateWidget: TFPGUIPrivateWidget;
begin
  (*
  if not Widget.Focusable then begin
    {$IFDEF FPGUIDEBUGFOCUS}
    writeln(TimeToStr(now),' ','Non UNFOCUSABLE ',FLCLObject.Name);
    {$ENDIF}
    FEntered:=false;
    exit;
  end;
  *)
  if FEntered then begin
    GlobalFocusedPrivateWidget:=nil;
    FEntered:=false;
    LCLSendKillFocusMsg(FLCLObject);
    {$IFDEF FPGUIDEBUGFOCUS}
    writeln(TimeToStr(now),' ','Exit Handler ',FLCLObject.Name,' ',FWidget.Focusable);
    {$ENDIF}
  end;
end;

procedure TFPGUIPrivateWidget.ResizeHandler(Sender: TObject);
begin
  if Assigned(FLCLObject) then begin
    LCLSendSizeMsg(FLCLObject,Widget.Width,Widget.Height,SIZE_RESTORED,true);
  end;
end;

procedure TFPGUIPrivateWidget.MouseMoveHandler(Sender: TObject;
  AShift: TShiftState; const AMousePos: TPoint);
var
  lAdjustedMousePos: TPoint;
begin
  lAdjustedMousePos:=AMousePos;
  AdjustPointXYToInterface(lAdjustedMousePos);
  GlobalMouseCursorPos:=lAdjustedMousePos;
  GlobalMouseCursorPosPrivateWidget:=Self;

  LCLSendMouseMoveMsg(FLCLObject,lAdjustedMousePos.x,lAdjustedMousePos.y,AShift);
  NotifyApplicationUserInput(FLCLObject,LM_MOUSEMOVE);
end;

procedure TFPGUIPrivateWidget.MsgDeactivate(var fpgmsg: TfpgMessageRec);
begin
  //Empty stub. To be implemented.
  // No LCLMessage for deactivate
end;

procedure TFPGUIPrivateWidget.MsgActivate(var fpgmsg: TfpgMessageRec);
begin
  LCLSendActivateMsg(LCLObject,1,false,HWND(Self));
end;

(*
procedure TFPGUIPrivateWidget.MsgPaint(var fpgmsg: TfpgMessageRec);
begin
  {$ifdef VerboseFPGUIPrivate}
    WriteLn('TFPGUIPrivateWindow.PaintHandler for ',LCLObject.Name);
  {$endif}
  exit;
  {
  if (LCLObject is TWinControl) then
  begin
    FillChar(Msg, SizeOf(Msg), #0);

    Msg.Msg := LM_PAINT;
    New(AStruct);
    FillChar(AStruct^, SizeOf(TPaintStruct), 0);
    Msg.PaintStruct := AStruct;
    Msg.DC := BeginPaint(THandle(Self), AStruct^);

    Self.GetClientRect(Msg.PaintStruct^.rcPaint);
    //Msg.PaintStruct^.rcPaint := fpgRectToRect(fpgmsg.Params.rect);
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
  }
end;
*)

(*
procedure TFPGUIPrivateWidget.MsgResize(var fpgmsg: TfpgMessageRec);
begin
  LCLSendSizeMsg(LCLObject, fpgmsg.Params.rect.Width,fpgmsg.Params.rect.Height, SIZE_RESTORED, false);
end;
*)

procedure TFPGUIPrivateWidget.MsgMove(var fpgmsg: TfpgMessageRec);
var
  P: TPoint;
begin
  if Assigned(LCLObject) and Assigned(FWidget) then begin
    P:=Point(fpgmsg.Params.rect.Left,fpgmsg.Params.rect.Top);
    if Assigned(FLCLObject.Parent) then begin
      TFPGUIPrivateWidget(FLCLObject.Parent.Handle).AdjustPointXYToInterface(P);
    end;
    LCLSendMoveMsg(LCLObject, P.x, P.y,Move_Default,true);
  end;
end;

(*
procedure TFPGUIPrivateWidget.MsgKeyChar(var fpgmsg: TfpgMessageRec);
var
  lKey: string;
  lW: Word;
begin
  exit;
  lKey:=UTF8ToAnsi(fpgmsg.Params.keyboard.keychar);
  if Length(lKey)>0 then begin
    lW:=Byte(lKey[1]);
    LCLSendCharEvent(LCLObject,lW,0,true,false,true);
    // Changing keychar and keycode does nothing in target widget.
  end;
end;
*)

(*
procedure TFPGUIPrivateWidget.MsgKeyPress(var fpgmsg: TfpgMessageRec);
var
  lKey: Word;
begin
  lKey:=KeycodeToWindowsVirtKey(fpgmsg.Params.keyboard.keycode);
  LCLSendKeyDownEvent(LCLObject, lKey, fpgmsg.Params.keyboard.keycode, true, true);
end;
*)

procedure TFPGUIPrivateWidget.MsgKeyRelease(var fpgmsg: TfpgMessageRec);
var
  lKey: Word;
begin
  lKey:=KeycodeToWindowsVirtKey(fpgmsg.Params.keyboard.keycode);
  LCLSendKeyUpEvent(LCLObject, lKey, fpgmsg.Params.keyboard.keycode, True, True);
end;

function fpgMouseButtonToTButton(AButton: Word): Controls.TMouseButton; overload;
begin
  case AButton of
    MOUSE_LEFT: Result := Controls.mbLeft;
    MOUSE_MIDDLE: Result := Controls.mbMiddle;
    MOUSE_RIGHT: Result := Controls.mbRight;
  else
    Result := Controls.mbExtra1;

  end;
end;

function fpgMouseButtonToTButton(AButton: TMouseButton): Controls.TMouseButton; overload;
begin
  case AButton of
    mbLeft: Result := Controls.mbLeft;
    mbRight: Result := Controls.mbRight;
    mbMiddle: Result := Controls.mbMiddle;
  otherwise
    Result := Controls.mbExtra1;
  end;
end;


(*
procedure TFPGUIPrivateWidget.MsgMouseDown(var fpgmsg: TfpgMessageRec);
var
  TmpRect: TRect;
begin
  TmpRect.TopLeft:=Point(fpgmsg.Params.mouse.x,fpgmsg.Params.mouse.y);
  AdjustRectXYToInterface(TmpRect);
  LCLSendMouseDownMsg(LCLObject, TmpRect.Left, TmpRect.Top,fpgMouseButtonToTButton(fpgmsg.Params.mouse.Buttons), fpgmsg.Params.mouse.shiftstate);
end;
*)

(*
procedure TFPGUIPrivateWidget.MsgMouseUp(var fpgmsg: TfpgMessageRec);
var
  p: TPoint;
  ss: TShiftState;
  TmpRect: TRect;
begin
  TmpRect.TopLeft:=Point(fpgmsg.Params.mouse.x,fpgmsg.Params.mouse.y);
  AdjustRectXYToInterface(TmpRect);
  LCLSendMouseUpMsg(LCLObject, TmpRect.Left, TmpRect.Top,fpgMouseButtonToTButton(fpgmsg.Params.mouse.Buttons), fpgmsg.Params.mouse.shiftstate);
  ss:=fpgmsg.Params.mouse.shiftstate;
  if not (ssShift in ss) then
    if not (ssAlt in ss) then
      if not (ssCtrl in ss) then
        if (fpgmsg.Params.mouse.Buttons=MOUSE_RIGHT) then begin
          if Assigned(LCLObject.PopupMenu) then begin
            p.x:=fpgmsg.Params.mouse.x;
            p.y:=fpgmsg.Params.mouse.y;
            ClientToScreen(HWND(Self),p);
            LCLObject.PopupMenu.PopUp(p.x,p.y);
          end;
  end;
end;
*)

(*
procedure TFPGUIPrivateWidget.MsgMouseMove(var fpgmsg: TfpgMessageRec);
var
  lTmpPoint: TPoint;
begin
  lTmpPoint:=Point(fpgmsg.Params.mouse.x,fpgmsg.Params.mouse.y);
  AdjustPointXY(lTmpPoint);
  lTmpPoint:=Self.FWidget.WindowToScreen(Self.FWidget,lTmpPoint);
  GlobalMouseCursorPos:=lTmpPoint;
  GlobalMouseCursorPosPrivateWidget:=Self;
end;
*)

procedure TFPGUIPrivateWidget.MsgDoubleClick(var fpgmsg: TfpgMessageRec);
begin
  LCLSendMouseMultiClickMsg(LCLObject, fpgmsg.Params.mouse.x, fpgmsg.Params.mouse.y, fpgMouseButtonToTButton(fpgmsg.Params.mouse.Buttons), 2, fpgmsg.Params.mouse.shiftstate);
end;

(*
procedure TFPGUIPrivateWidget.MsgMouseEnter(var fpgmsg: TfpgMessageRec);
begin
  if Assigned(LCLObject) then begin
    LCLSendMouseEnterMsg(LCLObject);
  end;
end;
*)

(*
procedure TFPGUIPrivateWidget.MsgMouseExit(var fpgmsg: TfpgMessageRec);
begin
  if Assigned(LCLObject) then begin
    LCLSendMouseLeaveMsg(LCLObject);
  end;
end;
*)

procedure TFPGUIPrivateWidget.MsgMouseScroll(var fpgmsg: TfpgMessageRec);
begin
  if Assigned(LCLObject) then begin
    LCLSendMouseWheelMsg(LCLObject, fpgmsg.Params.mouse.x, fpgmsg.Params.mouse.y,fpgmsg.Params.mouse.delta, fpgmsg.Params.mouse.shiftstate);
  end;
end;

procedure TFPGUIPrivateWidget.MsgUser(var fpgmsg: TfpgMessageRec);
var
  lNewMessage: LMessages.TLMessage;
begin
  // User message. User message is in Param1, and wParam and lParam in Param 2/3
  lNewMessage.msg:=LM_USER+fpgmsg.Params.user.Param1;
  lNewMessage.wParam:=fpgmsg.Params.user.Param2;
  lNewMessage.lParam:=fpgmsg.Params.user.Param3;
  LCLObject.WindowProc(lNewMessage);
end;

function TFPGUIPrivateWidget.GetParentContainerWidget: TfpgWidget;
begin
  // Note, if the Handle of the parent doesn't exist, it's automatically
  // created
  if Assigned(LCLObject) and Assigned(LCLObject.Parent) then begin
    Result := TFPGUIPrivateContainer(LCLObject.Parent.Handle).Widget;
    if Result is TfpgScrollFrame then begin
      Result:=TfpgScrollFrame(Result).ContentFrame;
    end
  end else begin
    Result := nil;
  end;
end;

function TFPGUIPrivateWidget.GetParentContainerPrivateWidgetFocusable: TFPGUIPrivateWidget;
begin
  if Assigned(LCLObject) and Assigned(LCLObject.Parent) then begin
    Result := TFPGUIPrivateContainer(LCLObject.Parent.Handle);
    if not Result.Widget.Focusable then begin
      Result:=nil;
    end;
  end else begin
    Result := nil;
  end;
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

procedure TFPGUIPrivateWidget.MouseDownHandler(Sender: TObject;
  AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
var
  TmpPoint: TPoint;
begin
  TmpPoint:=AMousePos;
  AdjustPointXYToInterface(TmpPoint);
  LCLSendMouseDownMsg(LCLObject, TmpPoint.x, TmpPoint.y,fpgMouseButtonToTButton(AButton), AShift);
  // A mouse move message must be sent or click visual effect is not working in LCL
  LCLSendMouseMoveMsg(LCLObject, TmpPoint.x, TmpPoint.y,AShift);
end;

procedure TFPGUIPrivateWidget.MouseUpHandler(Sender: TObject;
  AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
var
  lPopupPoint: TPoint;
  lTmpPoint: TPoint;
begin
  lTmpPoint:=AMousePos;
  AdjustPointXYToInterface(lTmpPoint);
  LCLSendMouseUpMsg(LCLObject, lTmpPoint.x, lTmpPoint.y,fpgMouseButtonToTButton(AButton), AShift);
  if not (ssShift in AShift) then
    if not (ssAlt in AShift) then
      if not (ssCtrl in AShift) then
        if (AButton=mbRight) then begin
          if Assigned(LCLObject.PopupMenu) then begin
            lPopupPoint:=lTmpPoint;
            ClientToScreen(HWND(Self),lPopupPoint);
            LCLObject.PopupMenu.PopUp(lPopupPoint.x,lPopupPoint.y);
          end;
  end;
end;

procedure TFPGUIPrivateWidget.MouseExitHandler(Sender: TObject);
begin
  if Assigned(LCLObject) then begin
    LCLSendMouseLeaveMsg(LCLObject);
  end;
end;

procedure TFPGUIPrivateWidget.MouseEnterHandler(Sender: TObject);
begin
  if Assigned(LCLObject) then begin
    LCLSendMouseEnterMsg(LCLObject);
  end;
end;

procedure TFPGUIPrivateWidget.KeyPressHandler(Sender: TObject;
  var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean);
var
  lKey: Word;
begin
  lKey:=KeycodeToWindowsVirtKey(KeyCode);
  LCLSendKeyDownEvent(LCLObject, lKey, lKey, true, true);
end;

procedure TFPGUIPrivateWidget.KeyCharHandler(Sender: TObject; AChar: TfpgChar;
  var Consumed: boolean);
var
  lKey: string;
  lW: Word;
begin
  lKey:=UTF8ToAnsi(AChar);
  if Length(lKey)>0 then begin
    lW:=Byte(lKey[1]);
    LCLSendCharEvent(LCLObject,lW,0,true,false,true);
    // Changing keychar and keycode does nothing in target widget.
    if lW=0 then begin
      Consumed:=true;
    end;
  end;
end;

procedure TFPGUIPrivateWidget.SetDC(const AValue: HDC);
begin
  FDC:=AValue;
end;

procedure TFPGUIPrivateWidget.SetEnabled(const AValue: Boolean);
begin
  Widget.Enabled:=AValue;
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

  if (AParams.Style and WS_VISIBLE)=0 then begin
    Self.Widget.Visible:=false;
  end;
  if (AParams.Style and WS_DISABLED)=WS_DISABLED then begin
    Self.Widget.Enabled:=false;
  end;

  Self.Widget.Width:=AParams.Width;
  Self.Widget.Height:=AParams.Height;

  SetEvents;
end;

destructor TFPGUIPrivateWidget.Destroy;
begin
  if (FWidget <> nil) then begin
    UnSetEvents;
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
  if GlobalMouseCursorPosPrivateWidget=Self then begin
    GlobalMouseCursorPosPrivateWidget:=nil;
  end;
  if GlobalMouseCapturedPrivateWidget=Self then begin
    GlobalMouseCapturedPrivateWidget:=nil;
  end;
  if Assigned(FLCLObject) and Assigned(FWidget) then begin
    if Assigned(FLCLObject.Parent) then begin
      TFPGUIPrivateWidget(FLCLObject.Parent.Handle).Invalidate;
    end;
  end;
  inherited Destroy;
end;

procedure TFPGUIPrivateWidget.CreateWidget(const AParams: TCreateParams);
begin
{$IFDEF BASEWIDGET_WIDGET}
  Widget := TfpgWidget.Create(GetParentContainerWidget());
{$ELSE}
  {$IFDEF BASEWIDGET_SCROLLFRAME}
    Widget := TfpgScrollFrame.Create(GetParentContainerWidget());
  {$ELSE}
    {$IFDEF BASEWIDGET_PANEL}
      Widget := TfpgPanel.Create(GetParentContainerWidget());
      TfpgPanel(Widget).Style:=bsFlat;
      TfpgPanel(Widget).Text:='';
      TfpgPanel(Widget).ParentBackgroundColor:=false;
    {$ELSE}
      Syntax error, choose WIDGET,SCROLLFRAME or PANEL for BASEWIDGET
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
end;

procedure TFPGUIPrivateWidget.SetEvents;
begin
  WidgetProtected.OnPaint       := PaintHandler;
  WidgetProtected.OnClick       := ClickHandler;
  WidgetProtected.OnResize      := ResizeHandler;
  WidgetProtected.OnEnter       := EnterHandler;
  WidgetProtected.OnExit        := ExitHandler;
  WidgetProtected.OnKeyPress    := KeyPressHandler;
  WidgetProtected.OnKeyChar     := KeyCharHandler;
  WidgetProtected.OnMouseEnter  := MouseEnterHandler;
  WidgetProtected.OnMouseExit   := MouseExitHandler;
  WidgetProtected.OnMouseMove   := MouseMoveHandler;
  WidgetProtected.OnMouseDown   := MouseDownHandler;
  WidgetProtected.OnMouseUp     := MouseUpHandler;

  // Messages which can not be handled as Widget events.
  fpgApplication.SetMessageHook(Widget, FPGM_MOVE, Self);
  fpgApplication.SetMessageHook(Widget, FPGM_KEYRELEASE, Self);
  fpgApplication.SetMessageHook(Widget, FPGM_ACTIVATE, Self);
  fpgApplication.SetMessageHook(Widget, FPGM_DEACTIVATE, Self);
  fpgApplication.SetMessageHook(Widget, FPGM_DOUBLECLICK, Self);
  fpgApplication.SetMessageHook(Widget, FPGM_CLOSE, Self);
  fpgApplication.SetMessageHook(Widget, FPGM_SCROLL, Self);
  fpgApplication.SetMessageHook(Widget, FPGM_POPUPCLOSE, Self);
  fpgApplication.SetMessageHook(Widget, FPGM_HINTTIMER, Self);
  // fpgApplication.SetMessageHook(Widget, FPGM_KEYCHAR, Self);
  // fpgApplication.SetMessageHook(Widget, FPGM_MOUSEENTER, Self);
  // fpgApplication.SetMessageHook(Widget, FPGM_MOUSEEXIT, Self);
  // fpgApplication.SetMessageHook(Widget, FPGM_RESIZE, Self);
  // fpgApplication.SetMessageHook(Widget, FPGM_KEYPRESS, Self);
  // fpgApplication.SetMessageHook(Widget, FPGM_PAINT, Self);
  // fpgApplication.SetMessageHook(Widget, FPGM_MOUSEMOVE, Self);
  // fpgApplication.SetMessageHook(Widget, FPGM_MOUSEDOWN, Self);
  // fpgApplication.SetMessageHook(Widget, FPGM_MOUSEUP, Self);
end;

procedure TFPGUIPrivateWidget.UnSetEvents;
begin
  fpgApplication.UnSetMessageHook(Widget, FPGM_MOVE, Self);
  fpgApplication.UnSetMessageHook(Widget, FPGM_KEYRELEASE, Self);
  fpgApplication.UnSetMessageHook(Widget, FPGM_ACTIVATE, Self);
  fpgApplication.UnSetMessageHook(Widget, FPGM_DEACTIVATE, Self);
  fpgApplication.UnSetMessageHook(Widget, FPGM_DOUBLECLICK, Self);
  fpgApplication.UnSetMessageHook(Widget, FPGM_CLOSE, Self);
  fpgApplication.UnSetMessageHook(Widget, FPGM_SCROLL, Self);
  fpgApplication.UnSetMessageHook(Widget, FPGM_POPUPCLOSE, Self);
  fpgApplication.UnSetMessageHook(Widget, FPGM_HINTTIMER, Self);
end;

procedure TFPGUIPrivateWidget.SetSize(AWidth, AHeight: LongInt);
begin
  FWidget.SetPosition(FWidget.Left, FWidget.Top, AWidth, AHeight);
  FLCLObject.InvalidateClientRectCache(true);
  LCLSendSizeMsg(FLCLObject,AWidth,AHeight,SIZE_RESTORED,false);
end;

procedure TFPGUIPrivateWidget.SetPosition(AX, AY: Integer);
begin
  if (FWidget.Parent<>nil) then begin
    //Request parent object to put the widget coordinates. This is needed for
    //widgets like TForm which reduces its clientrect when a menubar is visible.
    TFPGUIPrivateWidget(FLCLObject.Parent.Handle).SetWidgetPosition(FWidget,AX,AY);
  end else begin
    SetWidgetPosition(FWidget,AX,AY);
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
  PreferredWidth:=0;
  PreferredHeight:=0;
end;

procedure TFPGUIPrivateWidget.GetClientRect(var ARect: TRect);
var
  CLRect: TfpgRect;
begin
  //ClientRect must have Left and Top = (0,0)
  CLRect:=FWidget.GetClientRect;
  ARect.Left:=0;
  ARect.Top:=0;
  ARect.Right:=CLRect.Width;
  ARect.Bottom:=CLRect.Height;
end;

procedure TFPGUIPrivateWidget.GetDefaultClientRect(var ARect: TRect);
var
  CLRect: TfpgRect;
begin
  //ClientRect must have Left and Top = (0,0)
  CLRect:=FWidget.GetClientRect;
  ARect.Left:=0;
  ARect.Top:=0;
  ARect.Right:=CLRect.Width;
  ARect.Bottom:=CLRect.Height;
end;

procedure TFPGUIPrivateWidget.GetWindowRect(var ARect: TRect);
begin
  ARect:=fpgRectToRect(FWidget.GetBoundsRect);
end;

procedure TFPGUIPrivateWidget.AdjustRectXY(var AfpgRect: TfpgRect);
begin
  //By default widgets do not need adjust, except form with menu, groupbox,...
end;

procedure TFPGUIPrivateWidget.AdjustRectXYToInterface(var ARect: TRect);
begin
  //By default widgets do not need adjust, except form with menu, groupbox,...
end;

procedure TFPGUIPrivateWidget.AdjustPointXY(var APoint: TPoint);
begin
  //By default widgets do not need adjust, except form with menu, groupbox,...
end;

procedure TFPGUIPrivateWidget.AdjustPointXYToInterface(var APoint: TPoint);
begin
  //By default widgets do not need adjust, except form with menu, groupbox,...
end;

procedure TFPGUIPrivateWidget.SetCursor(const aCursor: integer);
var
  c: integer;
begin
  c:=integer(High(TMouseCursor));
  if (aCursor<=c) and (aCursor>=0) then begin
    if TMouseCursor(aCursor)=mcDefault then begin
      FWidget.MouseCursor:=FDefaultCursor;
    end else begin
      FWidget.MouseCursor:=TMouseCursor(aCursor);
    end;
  end else begin
    FWidget.MouseCursor:=mcDefault;
  end;
end;

procedure TFPGUIPrivateWidget.SetFocus;
begin
  if Widget.Focusable then begin
    {$IFDEF FPGUIDEBUGFOCUS}
    writeln(FLCLObject.Name,' FOCUS / Focused: ',Widget.Focused,' Focusable: ',Widget.Focusable);
    {$ENDIF}
    Widget.SetFocus;
  end else begin
    {$IFDEF FPGUIDEBUGFOCUS}
    writeln(FLCLObject.Name,' FOCUS FAIL / Focused: ',Widget.Focused,' Focusable: ',Widget.Focusable);
    {$ENDIF}
  end;
end;

procedure TFPGUIPrivateWidget.Paint;
  procedure RecursviveInvalidate(const aComponent: TComponent);
  var
    j: integer;
    c: TComponent;
  begin
    for j := 0 to Pred(aComponent.ComponentCount) do
    begin
      c:=aComponent.Components[j];
      if c is TfpgWidget then begin
        RecursviveInvalidate(c);
        TfpgWidget(c).Invalidate;
      end;
    end;
  end;
begin
  if Assigned(FWidget) then begin
    RecursviveInvalidate(FWidget);
    FWidget.Invalidate;
  end;
end;

procedure TFPGUIPrivateWidget.Clear;
begin
  //Do nothing by now, introduces flickering
  (*
  Widget.Canvas.BeginDraw(false);
  Widget.Canvas.FillRectangle(Widget.GetClientRect);
  Widget.Canvas.EndDraw;
  *)
end;

procedure TFPGUIPrivateWidget.Invalidate;
begin
  Widget.Invalidate;
end;

{ TFPGUIPrivateContainer }

constructor TFPGUIPrivateContainer.Create(ALCLObject: TWinControl;
  const AParams: TCreateParams);
begin
  inherited Create(ALCLObject, AParams);
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
    AWidget.SetPosition(AX,AY,AWidget.Width,AWidget.Height);
end;

procedure TFPGUIPrivateWindow.CloseHandler(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  ClosePopups;
  CloseAction := caFree;
  if LCLSendCloseQueryMsg(LCLObject) = 0 then begin
    CloseAction := caNone;
  end;
end;

procedure TFPGUIPrivateWindow.MsgResize(var fpgmsg: TfpgMessageRec);
begin
  if (fpgmsg.Sender is TfpgMenuBar) then begin
    //Invalidate clientRectCache to force LCL to recalibrate
    //the available space in the form after a menubar visibility change.
    //Children are needed to invalidate for TLabel and others custom draw.
    FLCLObject.InvalidateClientRectCache(true);
  end;
  {$IFDEF VerboseFPGUIPrivate}
    WriteLn('[TFPGUIPrivateWindow.MsgResize]',DebugMessage(fpgmsg));
  {$ENDIF}
  LCLSendSizeMsg(FLCLObject,fpgmsg.Params.rect.Width,fpgmsg.Params.rect.Height,SIZE_RESTORED,true);
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
  MenuBar.Focusable:=false;

end;

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateWindow.SetEvents
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TFPGUIPrivateWindow.SetEvents;
begin
  inherited SetEvents;
  fpgApplication.SetMessageHook(Widget, FPGM_RESIZE, Self);
  Form.OnClose := CloseHandler;
end;

procedure TFPGUIPrivateWindow.UnSetEvents;
begin
  fpgApplication.UnsetMessageHook(Widget, FPGM_RESIZE, Self);
  inherited UnSetEvents;
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

  // Call unsetevents because setting Widget:=nil prevent the call
  // to UnSetEvents in base class Destroy
//  UnSetEvents;
  // Instead of destroying the form immediately, we call Close
  // which will do a delayed close
  Form.OnClose:=nil; //Disconnect the handler
  Form.Close;
//  Form.Free;

  // By setting the Widget to nil we prevent it's future
  // destruction
//  Widget:=nil;
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

procedure TFPGUIPrivateWindow.AdjustPointXY(var APoint: TPoint);
begin
  if MenuBar.Visible then begin
    APoint.y:=APoint.y+MenuBar.Height;
  end;
end;

procedure TFPGUIPrivateWindow.AdjustPointXYToInterface(var APoint: TPoint);
begin
  if MenuBar.Visible then begin
    APoint.y:=APoint.y-MenuBar.Height;
  end;
end;

(*
procedure TFPGUIPrivateWindow.PaintHandler(Sender: TObject);
var
  AStruct: TPaintStruct;
  DC: HDC;
begin
  DC:=GetDC(THandle(Self));
  FillByte(AStruct,sizeof(AStruct),0);
  AStruct.fErase:=true;
  AStruct.hdc:=DC;
  GetClientRect(AStruct.rcPaint);
  LCLSendPaintMsg(Self.LCLObject,DC,@AStruct);
  ReleaseDC(THandle(Self),DC);
end;
*)

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
  if AValue.Color=clDefault then begin
    Button.TextColor:=clText1;
  end else begin
    Button.TextColor:=TColorToTfpgColor(AValue.Color);
  end;
end;

procedure TFPGUIPrivateButton.GetPreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  PreferredWidth:=Button.Font.TextWidth(Button.Text)+15+15;
  PreferredHeight:=Button.Font.Height+5+5;
end;

destructor TFPGUIPrivateButton.Destroy;
begin
  if Button.ImageName<>'' then begin
    fpgImages.DeleteImage(Button.ImageName,true);
  end;
  inherited Destroy;
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
function TFPGUIPrivateComboBox.ComboBox: TfpgEditCombo;
begin
  Result := TfpgEditCombo(Widget);
end;

procedure TFPGUIPrivateComboBox.SetFont(const AValue: TFont);
begin
  ComboBox.FontDesc:=TFontToTfpgFontDesc(AValue);
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
  Widget := TfpgEditCombo.Create(GetParentContainerWidget());
  ComboBox.AutoCompletion:=true;
  ComboBox.AllowNew:=TAllowNew.anNo;
end;

procedure TFPGUIPrivateComboBox.SetEvents;
begin
  inherited SetEvents;
  ComboBox.OnChange     := HandleChange;
  ComboBox.OnDropDown   := HandleDropDown;
  ComboBox.OnCloseUp    := HandleCloseUp;
end;

function TFPGUIPrivateComboBox.HasStaticText: Boolean;
begin
  Result:=true;
end;

procedure TFPGUIPrivateComboBox.SetText(const AText: String);
begin
  ComboBox.Text:=AText;
end;

function TFPGUIPrivateComboBox.GetText: String;
begin
  Result:=ComboBox.Text;
end;

{ TFPGUIPrivateEdit }

procedure TFPGUIPrivateEdit.SetFont(const AValue: TFont);
begin
  Edit.FontDesc:=TFontToTfpgFontDesc(AValue);
end;

{------------------------------------------------------------------------------
  Method: TFPGUIPrivateEdit.CreateWidget
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TFPGUIPrivateEdit.CreateWidget(const AParams: TCreateParams);
begin
  Widget := TfpgEdit.Create(GetParentContainerWidget());
  TfpgEdit(Widget).IgnoreMouseCursor:=true;
  FDefaultCursor:=mcIBeam;
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

procedure TFPGUIPrivateEdit.GetPreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  PreferredWidth:=0;
  PreferredHeight:=0;
end;

{ TFPGUIPrivateCheckBox }

function TFPGUIPrivateCheckBox.CheckBox: TfpgCheckBox;
begin
  Result := TfpgCheckBox(Widget);
end;

procedure TFPGUIPrivateCheckBox.SetFont(const AValue: TFont);
begin
  CheckBox.FontDesc:=TFontToTfpgFontDesc(AValue);
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
begin
  PreferredWidth:=CheckBox.Font.TextWidth(CheckBox.Text)+24; //This is hardcoded in fpgui for image
  PreferredHeight:=CheckBox.Font.Height+2;
end;

{ TFPGUIPrivateRadioButton }

function TFPGUIPrivateRadioButton.RadioButton: TfpgRadioButton;
begin
  Result := TfpgRadioButton(Widget);
end;

procedure TFPGUIPrivateRadioButton.SetFont(const AValue: TFont);
begin
  RadioButton.FontDesc:=TFontToTfpgFontDesc(AValue);
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
var
  lDimensions: TPoint;
begin
  lDimensions.x:=RadioButton.Width;
  lDimensions.y:=RadioButton.Height;
  //Autosize=true forces the control to calculate its preferred size.
  RadioButton.AutoSize:=true;
  if PreferredHeight<RadioButton.Font.Height then begin
    PreferredHeight:=RadioButton.Font.Height+2;
  end;
  RadioButton.AutoSize:=false;
  RadioButton.Width:=lDimensions.x;
  RadioButton.Height:=lDimensions.y;
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
var
  lMemo: TfpgMemo;
begin
  lMemo := TfpgMemo.Create(GetParentContainerWidget());
  Widget:=lMemo;
//  Not supported by memo ??
//  lMemo.IgnoreMouseCursor:=true;
  FDefaultCursor:=mcIBeam;
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

procedure TFPGUIPrivateMemo.GetPreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  PreferredHeight:=0;
  PreferredWidth:=0;
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
  case Dialog.ModalResult of
    mrNone: LCLDialog.UserChoice:=controls.mrClose;
    mrOK: LCLDialog.UserChoice:=controls.mrOK;
    mrCancel: LCLDialog.UserChoice:=controls.mrCancel;
    mrYes: LCLDialog.UserChoice:=controls.mrYes;
    mrNo: LCLDialog.UserChoice:=controls.mrNo;
    mrAbort: LCLDialog.UserChoice:=controls.mrAbort;
    mrRetry: LCLDialog.UserChoice:=controls.mrRetry;
    mrIgnore: LCLDialog.UserChoice:=controls.mrIgnore;
    mrAll: LCLDialog.UserChoice:=controls.mrAll;
    mrNoToAll: LCLDialog.UserChoice:=controls.mrNoToAll;
    mrYesToAll: LCLDialog.UserChoice:=controls.mrYesToAll;
    mrHelp: LCLDialog.UserChoice:=controls.mrClose;
    otherwise
    LCLDialog.UserChoice:=controls.mrClose;
  end;
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

function TFPGUIPrivateListBox.GetItemIndex: integer;
begin
  Result:=ListBox.FocusItem;
end;

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
  if (AParams.ExStyle and WS_EX_CLIENTEDGE)=WS_EX_CLIENTEDGE then begin
    ListBox.PopupFrame:=false;
  end else begin
    ListBox.PopupFrame:=true;
  end;
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

procedure TFPGUIPrivateGroupBox.AdjustPointXYToInterface(var APoint: TPoint);
var
  CLRect: TfpgRect;
begin
  CLRect:=FWidget.GetClientRect;
  APoint.x:=APoint.x-CLRect.Left;
  APoint.y:=APoint.y-CLRect.Top;
end;

function TFPGUIPrivateGroupBox.GroupBox: TfpgGroupBox;
begin
  Result:=TfpgGroupBox(FWidget);
end;

{ TFPGUIPrivateCustomPanel }

procedure TFPGUIPrivateCustomPanel.SetFont(const AValue: TFont);
begin
  Panel.FontDesc:=TFontToTfpgFontDesc(AValue);
end;

procedure TFPGUIPrivateCustomPanel.PaintHandler(Sender: TObject);
var
  lRect: TfpgRect;
  lCustomPanel: TPanel;
  j: integer;
begin
  lRect:=FWidget.GetClientRect;
  lCustomPanel:=TPanel(FLCLObject);
  if (lCustomPanel.BorderStyle<>TBorderStyle.bsNone) and (lCustomPanel.BorderWidth>0) then begin
    for j := 0 to Pred(lCustomPanel.BorderWidth) do begin
      FWidget.Canvas.DrawRectangle(lRect);
      InflateRect(lRect,-1,-1);
    end;
  end;
  if lCustomPanel.BevelOuter<>TPanelBevel.bvNone then begin
    for j := 0 to Pred(lCustomPanel.BevelWidth) do begin
      if lCustomPanel.BevelOuter=TPanelBevel.bvLowered then begin
        FWidget.Canvas.DrawBevel(lRect,false);
      end else if lCustomPanel.BevelOuter=TPanelBevel.bvRaised then begin
        FWidget.Canvas.DrawBevel(lRect,true);
      end;
      InflateRect(lRect,-1,-1);
    end;
  end;
  if lCustomPanel.BevelInner<>TPanelBevel.bvNone then begin
    for j := 0 to Pred(lCustomPanel.BevelWidth) do begin
      if lCustomPanel.BevelInner=TPanelBevel.bvLowered then begin
        FWidget.Canvas.DrawBevel(lRect,false);
      end else if lCustomPanel.BevelInner=TPanelBevel.bvRaised then begin
        FWidget.Canvas.DrawBevel(lRect,true);
      end;
      InflateRect(lRect,-1,-1);
    end;
  end;
  inherited;
end;

procedure TFPGUIPrivateCustomPanel.CreateWidget(const AParams: TCreateParams);
begin
  FWidget:=TfpgPanel.Create(GetParentContainerWidget());
  Panel.Text:='';
  Panel.Style:=bsFlat;
  Panel.ParentBackgroundColor:=false;
end;

function TFPGUIPrivateCustomPanel.HasStaticText: Boolean;
begin
  Result:=true;
end;

procedure TFPGUIPrivateCustomPanel.SetWidgetPosition(AWidget: TfpgWidget; AX,
  AY: Integer);
var
  CLRect: TfpgRect;
begin
  CLRect:=FWidget.GetClientRect;
  AWidget.SetPosition(AX+CLRect.Left,AY+CLRect.Top,AWidget.Width,AWidget.Height);
end;

procedure TFPGUIPrivateCustomPanel.AdjustRectXY(var AfpgRect: TfpgRect);
var
  CLRect: TfpgRect;
begin
  CLRect:=FWidget.GetClientRect;
  AfpgRect.Top:=AfpgRect.Top+CLRect.Top;
  AfpgRect.Left:=AfpgRect.Left+CLRect.Left;
end;

procedure TFPGUIPrivateCustomPanel.AdjustPointXY(var APoint: TPoint);
var
  CLRect: TfpgRect;
begin
  CLRect:=FWidget.GetClientRect;
  APoint.x:=APoint.x+CLRect.Left;
  APoint.y:=APoint.y+CLRect.Top;
end;

procedure TFPGUIPrivateCustomPanel.AdjustRectXYToInterface(var ARect: TRect);
var
  CLRect: TfpgRect;
begin
  CLRect:=FWidget.GetClientRect;
  ARect.Top:=ARect.Top-CLRect.Top;
  ARect.Left:=ARect.Left-CLRect.Left;
end;

procedure TFPGUIPrivateCustomPanel.AdjustPointXYToInterface(var APoint: TPoint);
var
  CLRect: TfpgRect;
begin
  CLRect:=FWidget.GetClientRect;
  APoint.x:=APoint.x-CLRect.Left;
  APoint.y:=APoint.y-CLRect.Top;
end;

procedure TFPGUIPrivateCustomPanel.SetText(const AText: String);
begin
  Panel.Text := AText;
end;

function TFPGUIPrivateCustomPanel.Panel: TfpgPanel;
begin
  Result:=TfpgPanel(FWidget);
end;

end.
  
