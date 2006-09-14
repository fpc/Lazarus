{
 *****************************************************************************
 *                              QtPrivate.pp                                 *
 *                              --------------                               *
 *                                                                           *
 *                                                                           *
 *****************************************************************************

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
unit qtprivate;

{$mode delphi}{$H+}

interface

uses
  // Bindings
  qt4,
  // Free Pascal
  Classes, SysUtils, Types,
  // LCL
  LMessages, Forms, Controls, LCLType, LCLProc, ExtCtrls, StdCtrls;

type
  { TQtWidget }

  TQtWidget = class(TObject)
  private
    function QtKeyToLCLKey(key: Integer): Word;
  public
    Widget: QWidgetH;
    LCLObject: TWinControl;
  public
    constructor Create(const AWinControl: TWinControl; const AParams: TCreateParams); virtual;
    destructor Destroy; override;
  public
    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl; virtual;
    procedure SlotShow(vShow: Boolean); cdecl;
    procedure SlotDestroy; cdecl;
    procedure SlotFocus(FocusIn: Boolean); cdecl;
    procedure SlotKey(Event: QEventH); cdecl;
    procedure SlotMouse(Event: QEventH); cdecl;
    procedure SlotPaint(Event: QEventH); cdecl;
    procedure SlotResize; cdecl;
  public
    procedure SetColor(const Value: PQColor);
    procedure Update;
    procedure Repaint;
    procedure setWindowTitle(Str: PWideString);
    procedure WindowTitle(Str: PWideString);
    procedure Show;
  end;
  
  { TQtAbstractButton }

  TQtAbstractButton = class(TQtWidget)
  private
  public
    procedure SetText(text: PWideString);
    procedure Text(retval: PWideString);
    function isChecked: Boolean;
    procedure setChecked(p1: Boolean);
  end;

  { TQtPushButton }

  TQtPushButton = class(TQtAbstractButton)
  private
  public
    constructor Create(const AWinControl: TWinControl; const AParams: TCreateParams); override;
    destructor Destroy; override;
    procedure SlotClicked; cdecl;
  end;

  { TQtBrush }

  TQtBrush = class(TObject)
  private
  public
    Widget: QBrushH;
  public
    constructor Create(CreateHandle: Boolean); virtual;
    destructor Destroy; override;
    procedure setStyle(style: QtBrushStyle);
  end;

  { TQtFont }

  TQtFont = class(QBrushH)
  private
  public
    Widget: QFontH;
  public
    constructor Create(CreateHandle: Boolean); virtual;
    destructor Destroy; override;
  public
    function pointSize: Integer;
    procedure setPointSize(p1: Integer);
    function pixelSize: Integer;
    procedure setPixelSize(p1: Integer);
    function weight: Integer;
    procedure setWeight(p1: Integer);
    procedure setBold(p1: Boolean);
    procedure setItalic(b: Boolean);
    procedure setUnderline(p1: Boolean);
    procedure setStrikeOut(p1: Boolean);
    procedure setRawName(p1: string);
  end;

  { TQtDeviceContext }

  TQtDeviceContext = class(TObject)
  private
  public
    Widget: QPainterH;
    Origin: TPoint;
    vBrush: TQtBrush;
    vFont: TQtFont;
  public
    constructor Create(WidgetHandle: HWND); virtual;
    destructor Destroy; override;
  public
    procedure drawRect(x1: Integer; y1: Integer; w: Integer; h: Integer);
    procedure drawText(x: Integer; y: Integer; s: PWideString);
    procedure drawLine(x1: Integer; y1: Integer; x2: Integer; y2: Integer);
    procedure drawEllipse(x: Integer; y: Integer; w: Integer; h: Integer);
    procedure setBrushOrigin(x, y: Integer);
    procedure brushOrigin(retval: PPoint);
    function font: TQtFont;
    procedure setFont(f: TQtFont);
    function brush: TQtBrush;
    procedure setBrush(brush: TQtBrush);
  end;

  { TQtMainWindow }

  TQtMainWindow = class(TQtWidget)
  private
  public
    Splitter : QSplitterH;
  public
    Canvas: TQtDeviceContext;
  public
    constructor Create(const AWinControl: TWinControl; const AParams: TCreateParams); override;
    destructor Destroy; override;
    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl; override;
    procedure SlotWindowStateChange; cdecl;
  end;

  { TQtStaticText }

  TQtStaticText = class(TQtWidget)
  private
  public
    constructor Create(const AWinControl: TWinControl; const AParams: TCreateParams); override;
    destructor Destroy; override;
    procedure SetText(text: PWideString);
    procedure Text(retval: PWideString);
  end;

  { TQtTimer }

  TQtTimer = class(TQtWidget)
  private
    CallbackFunc: TFNTimerProc;
    Id: Integer;
    AppObject: QObjectH;
  public
    constructor CreateTimer(Interval: integer; const TimerFunc: TFNTimerProc; App: QObjectH); virtual;
    destructor Destroy; override;
    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl; override;
  end;

  { TQtCheckBox }

  TQtCheckBox = class(TQtAbstractButton)
  public
    constructor Create(const AWinControl: TWinControl; const AParams: TCreateParams); override;
    destructor Destroy; override;
    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl; override;
    function CheckState: QtCheckState;
    procedure setCheckState(state: QtCheckState);
  end;

  { TQtRadioButton }

  TQtRadioButton = class(TQtAbstractButton)
  public
    constructor Create(const AWinControl: TWinControl; const AParams: TCreateParams); override;
    destructor Destroy; override;
    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl; override;
  end;
  
  { TQtGroupBox }

  TQtGroupBox = class(TQtWidget)
  private
    VBoxLayout: QVBoxLayoutH;
  public
    constructor Create(const AWinControl: TWinControl; const AParams: TCreateParams); override;
    destructor Destroy; override;
  end;
  
  { TQtFrame }
  
  TQtFrame = class(TQtWidget)
  private
  public
    constructor Create(const AWinControl: TWinControl; const AParams: TCreateParams); override;
    destructor Destroy; override;
    procedure setFrameStyle(p1: Integer);
    procedure setFrameShape(p1: QFrameShape);
    procedure setFrameShadow(p1: QFrameShadow);
  end;

  { TQtLineEdit }
  
  TQtLineEdit = class(TQtWidget)
  private
  public
    constructor Create(const AWinControl: TWinControl; const AParams: TCreateParams); override;
    destructor Destroy; override;
  end;
  
  { TQtTextEdit }
  
  TQtTextEdit = class(TQtWidget)
  private
  public
    constructor Create(const AWinControl: TWinControl; const AParams: TCreateParams); override;
    destructor Destroy; override;
  end;
  
  { TQtTabWidget }
  
  TQtTabWidget = class(TQtWidget)
  private
  public
    constructor Create(const AWinControl: TWinControl; const AParams: TCreateParams); override;
    destructor Destroy; override;
    function insertTab(index: Integer; page: QWidgetH; p2: PWideString): Integer;
  end;
  
  { TQtComboBox }
  
  TQtComboBox = class(TQtWidget)
  private
  public
    constructor Create(const AWinControl: TWinControl; const AParams: TCreateParams); override;
    destructor Destroy; override;
    function currentIndex: Integer;
    procedure setCurrentIndex(index: Integer);
  end;
  
  { TQtSpinBox }
  
  TQtSpinBox = class(TQtWidget)
  private
  public
    constructor Create(const AWinControl: TWinControl; const AParams: TCreateParams); override;
    destructor Destroy; override;
  end;

  { TQtAbstractItemView }

  TQtAbstractItemView = class(TQtWidget)
  public
  end;

  { TQtListView }

  TQtListView = class(TQtAbstractItemView)
  public
  end;

  
  TQtListWidget = class(TQtListView)
  private
  public
    constructor Create(const AWinControl: TWinControl; const AParams: TCreateParams); override;
    destructor Destroy; override;
  public
    function currentRow: Integer;
    procedure setCurrentRow(row: Integer);
  end;  
  
implementation

{ TQtWidget }

{------------------------------------------------------------------------------
  Function: TQtWidget.Create
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
constructor TQtWidget.Create(const AWinControl: TWinControl; const AParams: TCreateParams);
var
  Parent: QWidgetH;
begin
  // Initializes the properties
  LCLObject := AWinControl;

  // Creates the widget
  {$ifdef VerboseQt}
    WriteLn('Calling QWidget_create');
  {$endif}
  Parent := TQtWidget(AWinControl.Parent.Handle).Widget;
  Widget := QWidget_create(Parent);

  // Sets it´ s initial properties
  QWidget_setGeometry(Widget, AWinControl.Left, AWinControl.Top,
   AWinControl.Width, AWinControl.Height);
end;

{------------------------------------------------------------------------------
  Function: TQtWidget.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtWidget.Destroy;
begin
  {$ifdef VerboseQt}
//    WriteLn('Calling QWidget_destroy');
  {$endif}

//  QWidget_destroy(QWidgetH(Widget));

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Function: TQtWidget.EventFilter
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TQtWidget.EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl;
begin
  Result := False;

  {$ifdef VerboseQt}
//  WriteLn(Integer(QEvent_type(Event)));
  {$endif}

  QEvent_ignore(Event);

  case QEvent_type(Event) of
   QEventShow: SlotShow(True);
   QEventHide: SlotShow(False);
   QEventDestroy: SlotDestroy;
   QEventFocusIn: SlotFocus(True);
   QEventFocusOut: SlotFocus(False);
   QEventKeyPress: SlotKey(Event);
   QEventKeyRelease: SlotKey(Event);
   QEventMouseButtonPress: SlotMouse(Event);
   QEventMouseButtonRelease: SlotMouse(Event);
   QEventMouseButtonDblClick: SlotMouse(Event);
   QEventMouseMove: SlotMouse(Event);
   QEventResize: SlotResize;

   QEventPaint: SlotPaint(Event);
  end;

{  GtkWidgetSet.SetCallback(LM_WINDOWPOSCHANGED, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_EXPOSEEVENT, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_KEYDOWN, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_KEYUP, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_CHAR, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_MOUSEMOVE, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_LBUTTONDOWN, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_LBUTTONUP, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_RBUTTONDOWN, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_RBUTTONUP, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_MBUTTONDOWN, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_MBUTTONUP, AGTKObject, AComponent);
  GtkWidgetSet.SetCallback(LM_MOUSEWHEEL, AGTKObject, AComponent);}
end;

{------------------------------------------------------------------------------
  Function: TQtWidget.SlotShow
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtWidget.SlotShow(vShow: Boolean); cdecl;
var
  Msg: TLMShowWindow;
begin
  FillChar(Msg, SizeOf(Msg), #0);

  Msg.Msg := LM_SHOWWINDOW;
  Msg.Show := vShow;

  try
    LCLObject.WindowProc(TLMessage(Msg));
  except
    Application.HandleException(nil);
  end;
end;

{------------------------------------------------------------------------------
  Function: TQtWidget.SlotDestroy
  Params:  None
  Returns: Nothing
  
  Currently commented because it was raising exception on software exit
 ------------------------------------------------------------------------------}
procedure TQtWidget.SlotDestroy; cdecl;
begin
{  FillChar(Msg, SizeOf(Msg), #0);

  Msg.Msg := LM_DESTROY;

  try
    LCLObject.WindowProc(TLMessage(Msg));
  except
    Application.HandleException(nil);
  end;}
end;

{------------------------------------------------------------------------------
  Function: TQtWidget.SlotFocus
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtWidget.SlotFocus(FocusIn: Boolean); cdecl;
var
  Msg: TLMessage;
begin
  FillChar(Msg, SizeOf(Msg), #0);

  if FocusIn then Msg.Msg := LM_SETFOCUS
  else Msg.Msg := LM_KILLFOCUS;

  try
    LCLObject.WindowProc(TLMessage(Msg));
  except
    Application.HandleException(nil);
  end;
end;

{------------------------------------------------------------------------------
  Function: TQtWidget.SlotKey
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtWidget.SlotKey(Event: QEventH); cdecl;
var
  Msg: TLMKey;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtWidget.SlotKey');
  {$endif}

  FillChar(Msg, SizeOf(Msg), #0);

  if QEvent_type(Event) = QEventKeyRelease then Msg.Msg := LM_KEYUP
  else Msg.Msg := LM_KEYDOWN;
  
  {------------------------------------------------------------------------------
   Translates a Qt4 Key to a LCL VK_ key
   ------------------------------------------------------------------------------}
  Msg.CharCode := QtKeyToLCLKey(QKeyEvent_key(QKeyEventH(Event)));
  
  try
    LCLObject.WindowProc(TLMessage(Msg));
  except
    Application.HandleException(nil);
  end;

//LM_KEYDOWN
//LM_KEYUP
// LM_CHAR
end;

{------------------------------------------------------------------------------
  Function: TQtWidget.SlotMouse
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtWidget.SlotMouse(Event: QEventH); cdecl;
begin

end;

{------------------------------------------------------------------------------
  Function: TQtWidget.SlotPaint
  Params:  None
  Returns: Nothing

  Sends a LM_PAINT message to the LCL. This is for custom painted controls only
 ------------------------------------------------------------------------------}
procedure TQtWidget.SlotPaint(Event: QEventH); cdecl;
var
  Msg: TLMPaint;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtWidget.SlotPaint');
  {$endif}

  if (LCLObject is TCustomControl) or (LCLObject is TCustomForm) then
  begin
    FillChar(Msg, SizeOf(Msg), #0);

    Msg.Msg := LM_PAINT;
    Msg.DC := 0;
    
    try
      LCLObject.WindowProc(TLMessage(Msg));
    except
      Application.HandleException(nil);
    end;
  end;
end;

{------------------------------------------------------------------------------
  Function: TQtWidget.SlotResize
  Params:  None
  Returns: Nothing

  Sends a LM_SIZE message to the LCL.
 ------------------------------------------------------------------------------}
procedure TQtWidget.SlotResize; cdecl;
var
  Msg: TLMSize;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtWidget.SlotResize');
  {$endif}

  FillChar(Msg, SizeOf(Msg), #0);

  Msg.Msg := LM_SIZE;

  case QWidget_windowState(Widget) of
   QtWindowMinimized: Msg.SizeType := SIZEICONIC;
   QtWindowMaximized: Msg.SizeType := SIZEFULLSCREEN;
   QtWindowFullScreen: Msg.SizeType := SIZEFULLSCREEN;
  else
   Msg.SizeType := SIZENORMAL;
  end;

  Msg.SizeType := Msg.SizeType or Size_SourceIsInterface;

  Msg.Width := QWidget_width(Widget);
  Msg.Height := QWidget_height(Widget);

  try
    LCLObject.WindowProc(TLMessage(Msg));
  except
    Application.HandleException(nil);
  end;
end;

{------------------------------------------------------------------------------
  Function: TQtWidget.SetColor
  Params:  QColorH
  Returns: Nothing

  Schedules a paint event for processing when Qt returns to the main event loop
 ------------------------------------------------------------------------------}
procedure TQtWidget.SetColor(const Value: PQColor);
var
  Palette: QPaletteH;
begin
  Palette:=QPalette_create(QWidget_palette(Widget));
  // Set the palette for all color groups (active, inactive, disabled)
  QPalette_setColor(Palette,QPaletteWindow,Value);
  QPalette_setColor(Palette,QPaletteButton,Value);
  QPalette_setColor(Palette,QPaletteBase,Value);
  // Set the Palette
  QWidget_setPalette(Widget,Palette);
  QPalette_destroy(Palette);
end;

{------------------------------------------------------------------------------
  Function: TQtWidget.Update
  Params:  None
  Returns: Nothing

  Schedules a paint event for processing when Qt returns to the main event loop
 ------------------------------------------------------------------------------}
procedure TQtWidget.Update;
begin
  QWidget_update(Widget);
end;

{------------------------------------------------------------------------------
  Function: TQtWidget.Repaint
  Params:  None
  Returns: Nothing

  Repaints the control imediately
 ------------------------------------------------------------------------------}
procedure TQtWidget.Repaint;
begin
  QWidget_repaint(Widget);
end;

procedure TQtWidget.setWindowTitle(Str: PWideString);
begin
  QWidget_setWindowTitle(Widget, Str);
end;

procedure TQtWidget.WindowTitle(Str: PWideString);
begin
  QWidget_WindowTitle(Widget, Str);
end;

procedure TQtWidget.Show;
begin
  QWidget_show(Widget);
end;

{------------------------------------------------------------------------------
  Function: TQtWidget.QtKeyToLCLKey
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TQtWidget.QtKeyToLCLKey(key: Integer): Word;
begin
  case key of
    QtKey_Escape: Result := VK_ESCAPE;
    QtKey_Tab: Result := VK_TAB;
//    QtKey_Backtab = 16777218 { $1000002 }; ????
//    QtKey_Backspace = 16777219 { $1000003 };
    QtKey_Return: Result := VK_RETURN;
    QtKey_Enter: Result := VK_RETURN;
    QtKey_Insert: Result := VK_RETURN;
    QtKey_Delete: Result := VK_RETURN;
    QtKey_Pause: Result := VK_PAUSE;
    QtKey_Print: Result := VK_PRINT;
//    QtKey_SysReq = 16777226 { $100000a };
//    QtKey_Clear = 16777227 { $100000b };
    QtKey_Home: Result := VK_HOME;
    QtKey_End: Result := VK_END;
    QtKey_Left: Result := VK_LEFT;
    QtKey_Up: Result := VK_UP;
    QtKey_Right: Result := VK_RIGHT;
    QtKey_Down: Result := VK_DOWN;
    QtKey_PageUp: Result := VK_PRIOR;
    QtKey_PageDown: Result := VK_NEXT;
    QtKey_Shift: Result := VK_LSHIFT;     // There is also RSHIFT
    QtKey_Control: Result := VK_LCONTROL; // There is also RCONTROL
{    QtKey_Meta: Result := VK_META;
    QtKey_Alt: Result := VK_ALT;
    QtKey_CapsLock: Result := VK_CAPSLOCK;
    QtKey_NumLock: Result := VK_NUMLOCK;
    QtKey_ScrollLock = 16777254  $1000026 ;}
    QtKey_F1: Result := VK_F1;
    QtKey_F2: Result := VK_F2;
    QtKey_F3: Result := VK_F3;
    QtKey_F4: Result := VK_F4;
    QtKey_F5: Result := VK_F5;
    QtKey_F6: Result := VK_F6;
    QtKey_F7: Result := VK_F7;
    QtKey_F8: Result := VK_F8;
    QtKey_F9: Result := VK_F9;
    QtKey_F10: Result := VK_F10;
    QtKey_F11: Result := VK_F11;
    QtKey_F12: Result := VK_F12;
    QtKey_F13: Result := VK_F13;
    QtKey_F14: Result := VK_F14;
    QtKey_F15: Result := VK_F15;
    QtKey_F16: Result := VK_F16;
    QtKey_F17: Result := VK_F17;
    QtKey_F18: Result := VK_F18;
    QtKey_F19: Result := VK_F19;
    QtKey_F20: Result := VK_F20;
    QtKey_F21: Result := VK_F21;
    QtKey_F22: Result := VK_F22;
    QtKey_F23: Result := VK_F23;
    QtKey_F24: Result := VK_F24;
{    QtKey_F25 = 16777288  $1000048 ;
    QtKey_F26 = 16777289  $1000049 ;
    QtKey_F27 = 16777290  $100004a ;
    QtKey_F28 = 16777291  $100004b ;
    QtKey_F29 = 16777292  $100004c ;
    QtKey_F30 = 16777293  $100004d ;
    QtKey_F31 = 16777294  $100004e ;
    QtKey_F32 = 16777295  $100004f ;
    QtKey_F33 = 16777296  $1000050 ;
    QtKey_F34 = 16777297  $1000051 ;
    QtKey_F35 = 16777298  $1000052 ;}
(*    QtKey_Super_L = 16777299 { $1000053 };
    QtKey_Super_R = 16777300 { $1000054 };
    QtKey_Menu = 16777301 { $1000055 };
    QtKey_Hyper_L = 16777302 { $1000056 };
    QtKey_Hyper_R = 16777303 { $1000057 };
    QtKey_Help = 16777304 { $1000058 };
    QtKey_Direction_L = 16777305 { $1000059 };
    QtKey_Direction_R = 16777312 { $1000060 };
    QtKey_Space = 32 { $20 };
    QtKey_Any = 32 { $20 };
    QtKey_Exclam = 33 { $21 };
    QtKey_QuoteDbl = 34 { $22 };
    QtKey_NumberSign = 35 { $23 };
    QtKey_Dollar = 36 { $24 };
    QtKey_Percent = 37 { $25 };
    QtKey_Ampersand = 38 { $26 };
    QtKey_Apostrophe = 39 { $27 };
    QtKey_ParenLeft = 40 { $28 };
    QtKey_ParenRight = 41 { $29 };
    QtKey_Asterisk = 42 { $2a };
    QtKey_Plus = 43 { $2b };
    QtKey_Comma = 44 { $2c };
    QtKey_Minus = 45 { $2d };
    QtKey_Period = 46 { $2e };
    QtKey_Slash = 47 { $2f };*)
    QtKey_0: Result := VK_0;
    QtKey_1: Result := VK_1;
    QtKey_2: Result := VK_2;
    QtKey_3: Result := VK_3;
    QtKey_4: Result := VK_4;
    QtKey_5: Result := VK_5;
    QtKey_6: Result := VK_6;
    QtKey_7: Result := VK_7;
    QtKey_8: Result := VK_8;
    QtKey_9: Result := VK_9;
//    QtKey_Colon = 58 { $3a };
//    QtKey_Semicolon = 59 { $3b };
//    QtKey_Less = 60 { $3c };
//    QtKey_Equal = 61 { $3d };
//    QtKey_Greater = 62 { $3e };
//    QtKey_Question = 63 { $3f };
//    QtKey_At = 64 { $40 };
    QtKey_A: Result := VK_A;
    QtKey_B: Result := VK_B;
    QtKey_C: Result := VK_C;
    QtKey_D: Result := VK_D;
    QtKey_E: Result := VK_E;
    QtKey_F: Result := VK_F;
    QtKey_G: Result := VK_G;
    QtKey_H: Result := VK_H;
    QtKey_I: Result := VK_I;
    QtKey_J: Result := VK_J;
    QtKey_K: Result := VK_K;
    QtKey_L: Result := VK_L;
    QtKey_M: Result := VK_M;
    QtKey_N: Result := VK_N;
    QtKey_O: Result := VK_O;
    QtKey_P: Result := VK_P;
    QtKey_Q: Result := VK_Q;
    QtKey_R: Result := VK_R;
    QtKey_S: Result := VK_S;
    QtKey_T: Result := VK_T;
    QtKey_U: Result := VK_U;
    QtKey_V: Result := VK_V;
    QtKey_W: Result := VK_W;
    QtKey_X: Result := VK_X;
    QtKey_Y: Result := VK_Y;
    QtKey_Z: Result := VK_Z;
(*    QtKey_BracketLeft = 91 { $5b };
    QtKey_Backslash = 92 { $5c };
    QtKey_BracketRight = 93 { $5d };
    QtKey_AsciiCircum = 94 { $5e };
    QtKey_Underscore = 95 { $5f };
    QtKey_QuoteLeft = 96 { $60 };
    QtKey_BraceLeft = 123 { $7b };
    QtKey_Bar = 124 { $7c };
    QtKey_BraceRight = 125 { $7d };
    QtKey_AsciiTilde = 126 { $7e };
    QtKey_nobreakspace = 160 { $a0 };
    QtKey_exclamdown = 161 { $a1 };
    QtKey_cent = 162 { $a2 };
    QtKey_sterling = 163 { $a3 };
    QtKey_currency = 164 { $a4 };
    QtKey_yen = 165 { $a5 };
    QtKey_brokenbar = 166 { $a6 };
    QtKey_section = 167 { $a7 };
    QtKey_diaeresis = 168 { $a8 };
    QtKey_copyright = 169 { $a9 };
    QtKey_ordfeminine = 170 { $aa };
    QtKey_guillemotleft = 171 { $ab };
    QtKey_notsign = 172 { $ac };
    QtKey_hyphen = 173 { $ad };
    QtKey_registered = 174 { $ae };
    QtKey_macron = 175 { $af };
    QtKey_degree = 176 { $b0 };
    QtKey_plusminus = 177 { $b1 };
    QtKey_twosuperior = 178 { $b2 };
    QtKey_threesuperior = 179 { $b3 };
    QtKey_acute = 180 { $b4 };
    QtKey_mu = 181 { $b5 };
    QtKey_paragraph = 182 { $b6 };
    QtKey_periodcentered = 183 { $b7 };
    QtKey_cedilla = 184 { $b8 };
    QtKey_onesuperior = 185 { $b9 };
    QtKey_masculine = 186 { $ba };
    QtKey_guillemotright = 187 { $bb };
    QtKey_onequarter = 188 { $bc };
    QtKey_onehalf = 189 { $bd };
    QtKey_threequarters = 190 { $be };
    QtKey_questiondown = 191 { $bf };
    QtKey_Agrave = 192 { $c0 };
    QtKey_Aacute = 193 { $c1 };
    QtKey_Acircumflex = 194 { $c2 };
    QtKey_Atilde = 195 { $c3 };
    QtKey_Adiaeresis = 196 { $c4 };
    QtKey_Aring = 197 { $c5 };
    QtKey_AE = 198 { $c6 };
    QtKey_Ccedilla = 199 { $c7 };
    QtKey_Egrave = 200 { $c8 };
    QtKey_Eacute = 201 { $c9 };
    QtKey_Ecircumflex = 202 { $ca };
    QtKey_Ediaeresis = 203 { $cb };
    QtKey_Igrave = 204 { $cc };
    QtKey_Iacute = 205 { $cd };
    QtKey_Icircumflex = 206 { $ce };
    QtKey_Idiaeresis = 207 { $cf };
    QtKey_ETH = 208 { $d0 };
    QtKey_Ntilde = 209 { $d1 };
    QtKey_Ograve = 210 { $d2 };
    QtKey_Oacute = 211 { $d3 };
    QtKey_Ocircumflex = 212 { $d4 };
    QtKey_Otilde = 213 { $d5 };
    QtKey_Odiaeresis = 214 { $d6 };
    QtKey_multiply = 215 { $d7 };
    QtKey_Ooblique = 216 { $d8 };
    QtKey_Ugrave = 217 { $d9 };
    QtKey_Uacute = 218 { $da };
    QtKey_Ucircumflex = 219 { $db };
    QtKey_Udiaeresis = 220 { $dc };
    QtKey_Yacute = 221 { $dd };
    QtKey_THORN = 222 { $de };
    QtKey_ssharp = 223 { $df };
    QtKey_division = 247 { $f7 };
    QtKey_ydiaeresis = 255 { $ff };
    QtKey_Multi_key = 16781600 { $1001120 };
    QtKey_Codeinput = 16781623 { $1001137 };
    QtKey_SingleCandidate = 16781628 { $100113c };
    QtKey_MultipleCandidate = 16781629 { $100113d };
    QtKey_PreviousCandidate = 16781630 { $100113e };
    QtKey_Mode_switch = 16781694 { $100117e };
    QtKey_Kanji = 16781601 { $1001121 };
    QtKey_Muhenkan = 16781602 { $1001122 };
    QtKey_Henkan = 16781603 { $1001123 };
    QtKey_Romaji = 16781604 { $1001124 };
    QtKey_Hiragana = 16781605 { $1001125 };
    QtKey_Katakana = 16781606 { $1001126 };
    QtKey_Hiragana_Katakana = 16781607 { $1001127 };
    QtKey_Zenkaku = 16781608 { $1001128 };
    QtKey_Hankaku = 16781609 { $1001129 };
    QtKey_Zenkaku_Hankaku = 16781610 { $100112a };
    QtKey_Touroku = 16781611 { $100112b };
    QtKey_Massyo = 16781612 { $100112c };
    QtKey_Kana_Lock = 16781613 { $100112d };
    QtKey_Kana_Shift = 16781614 { $100112e };
    QtKey_Eisu_Shift = 16781615 { $100112f };
    QtKey_Eisu_toggle = 16781616 { $1001130 };
    QtKey_Hangul = 16781617 { $1001131 };
    QtKey_Hangul_Start = 16781618 { $1001132 };
    QtKey_Hangul_End = 16781619 { $1001133 };
    QtKey_Hangul_Hanja = 16781620 { $1001134 };
    QtKey_Hangul_Jamo = 16781621 { $1001135 };
    QtKey_Hangul_Romaja = 16781622 { $1001136 };
    QtKey_Hangul_Jeonja = 16781624 { $1001138 };
    QtKey_Hangul_Banja = 16781625 { $1001139 };
    QtKey_Hangul_PreHanja = 16781626 { $100113a };
    QtKey_Hangul_PostHanja = 16781627 { $100113b };
    QtKey_Hangul_Special = 16781631 { $100113f };
    QtKey_Dead_Grave = 16781904 { $1001250 };
    QtKey_Dead_Acute = 16781905 { $1001251 };
    QtKey_Dead_Circumflex = 16781906 { $1001252 };
    QtKey_Dead_Tilde = 16781907 { $1001253 };
    QtKey_Dead_Macron = 16781908 { $1001254 };
    QtKey_Dead_Breve = 16781909 { $1001255 };
    QtKey_Dead_Abovedot = 16781910 { $1001256 };
    QtKey_Dead_Diaeresis = 16781911 { $1001257 };
    QtKey_Dead_Abovering = 16781912 { $1001258 };
    QtKey_Dead_Doubleacute = 16781913 { $1001259 };
    QtKey_Dead_Caron = 16781914 { $100125a };
    QtKey_Dead_Cedilla = 16781915 { $100125b };
    QtKey_Dead_Ogonek = 16781916 { $100125c };
    QtKey_Dead_Iota = 16781917 { $100125d };
    QtKey_Dead_Voiced_Sound = 16781918 { $100125e };
    QtKey_Dead_Semivoiced_Sound = 16781919 { $100125f };
    QtKey_Dead_Belowdot = 16781920 { $1001260 };
    QtKey_Dead_Hook = 16781921 { $1001261 };
    QtKey_Dead_Horn = 16781922 { $1001262 };
    QtKey_Back = 16777313 { $1000061 };
    QtKey_Forward = 16777314 { $1000062 };
    QtKey_Stop = 16777315 { $1000063 };
    QtKey_Refresh = 16777316 { $1000064 };
    QtKey_VolumeDown = 16777328 { $1000070 };
    QtKey_VolumeMute = 16777329 { $1000071 };
    QtKey_VolumeUp = 16777330 { $1000072 };
    QtKey_BassBoost = 16777331 { $1000073 };
    QtKey_BassUp = 16777332 { $1000074 };
    QtKey_BassDown = 16777333 { $1000075 };
    QtKey_TrebleUp = 16777334 { $1000076 };
    QtKey_TrebleDown = 16777335 { $1000077 };
    QtKey_MediaPlay = 16777344 { $1000080 };
    QtKey_MediaStop = 16777345 { $1000081 };
    QtKey_MediaPrevious = 16777346 { $1000082 };
    QtKey_MediaNext = 16777347 { $1000083 };
    QtKey_MediaRecord = 16777348 { $1000084 };
    QtKey_HomePage = 16777360 { $1000090 };
    QtKey_Favorites = 16777361 { $1000091 };
    QtKey_Search = 16777362 { $1000092 };
    QtKey_Standby = 16777363 { $1000093 };
    QtKey_OpenUrl = 16777364 { $1000094 };
    QtKey_LaunchMail = 16777376 { $10000a0 };
    QtKey_LaunchMedia = 16777377 { $10000a1 };
    QtKey_Launch0 = 16777378 { $10000a2 };
    QtKey_Launch1 = 16777379 { $10000a3 };
    QtKey_Launch2 = 16777380 { $10000a4 };
    QtKey_Launch3 = 16777381 { $10000a5 };
    QtKey_Launch4 = 16777382 { $10000a6 };
    QtKey_Launch5 = 16777383 { $10000a7 };
    QtKey_Launch6 = 16777384 { $10000a8 };
    QtKey_Launch7 = 16777385 { $10000a9 };
    QtKey_Launch8 = 16777386 { $10000aa };
    QtKey_Launch9 = 16777387 { $10000ab };
    QtKey_LaunchA = 16777388 { $10000ac };
    QtKey_LaunchB = 16777389 { $10000ad };
    QtKey_LaunchC = 16777390 { $10000ae };
    QtKey_LaunchD = 16777391 { $10000af };
    QtKey_LaunchE = 16777392 { $10000b0 };
    QtKey_LaunchF = 16777393 { $10000b1 };
    QtKey_MediaLast = 16842751 { $100ffff };
    QtKey_unknown = 33554431 { $1ffffff };*)
  else
    Result := VK_UNKNOWN;
  end;
end;

{ TQtAbstractButton }

{------------------------------------------------------------------------------
  Function: TQtAbstractButton.SetText
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtAbstractButton.SetText(text: PWideString);
begin
  QAbstractButton_setText(QAbstractButtonH(Widget), text);
end;

{------------------------------------------------------------------------------
  Function: TQtAbstractButton.Text
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtAbstractButton.Text(retval: PWideString);
begin
  QAbstractButton_text(QAbstractButtonH(Widget), retval);
end;

{------------------------------------------------------------------------------
  Function: TQtAbstractButton.isChecked
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TQtAbstractButton.isChecked: Boolean;
begin
  Result := QAbstractButton_isChecked(QAbstractButtonH(Widget));
end;

{------------------------------------------------------------------------------
  Function: TQtAbstractButton.setChecked
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtAbstractButton.setChecked(p1: Boolean);
begin
  QAbstractButton_setChecked(QAbstractButtonH(Widget), p1);
end;

{ TQtPushButton }

{------------------------------------------------------------------------------
  Function: TQtPushButton.Create
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
constructor TQtPushButton.Create(const AWinControl: TWinControl; const AParams: TCreateParams);
var
  Str: WideString;
  Parent: QWidgetH;
begin
  // Initializes the properties
  LCLObject := AWinControl;

  // Creates the widget
  {$ifdef VerboseQt}
    WriteLn('TQtPushButton.Create');
  {$endif}
  
  Str := WideString(AWinControl.Caption);
  Parent := TQtWidget(AWinControl.Parent.Handle).Widget;
  Widget := QPushButton_create(@Str, Parent);

  // Sets it´ s initial properties
  QWidget_setGeometry(Widget, AWinControl.Left, AWinControl.Top,
   AWinControl.Width, AWinControl.Height);
end;

{------------------------------------------------------------------------------
  Function: TQtPushButton.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtPushButton.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtPushButton.Destroy');
  {$endif}

  QPushButton_destroy(QPushButtonH(Widget));

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Function: TQtPushButton.SlotClicked
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtPushButton.SlotClicked; cdecl;
var
  Msg: TLMessage;
begin
  Msg.Msg := LM_CLICKED;

  try
    LCLObject.WindowProc(TLMessage(Msg));
  except
    Application.HandleException(nil);
  end;

{  if (TLMessage(AMessage).Msg=LM_PAINT)
  or (TLMessage(AMessage).Msg=LM_INTERNALPAINT)
  or (TLMessage(AMessage).Msg=LM_GtkPaint) then
    CurrentSentPaintMessageTarget:=TObject(Target);

  try
    if TObject(Target) is TControl
    then TControl(Target).WindowProc(TLMessage(Msg))
    else TObject(Target).Dispatch(TLMessage(Msg));
  except
    Application.HandleException(nil);
  end;

  CurrentSentPaintMessageTarget:=nil;}
end;

{ TQtFont }

{------------------------------------------------------------------------------
  Function: TQtFont.Create
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
constructor TQtFont.Create(CreateHandle: Boolean);
begin
  {$ifdef VerboseQt}
    WriteLn('TQtFont.Create CreateHandle: ', dbgs(CreateHandle));
  {$endif}

  if CreateHandle then Widget := QFont_create;
end;

{------------------------------------------------------------------------------
  Function: TQtFont.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtFont.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtFont.Destroy');
  {$endif}

  if Widget <> nil then QFont_destroy(Widget);

  inherited Destroy;
end;

function TQtFont.pointSize: Integer;
begin
  Result := QFont_pointSize(Widget);
end;

procedure TQtFont.setPointSize(p1: Integer);
begin
  QFont_setPointSize(Widget, p1);
end;

function TQtFont.pixelSize: Integer;
begin
  Result := QFont_pixelSize(Widget);
end;

procedure TQtFont.setPixelSize(p1: Integer);
begin
  QFont_setPixelSize(Widget, p1);
end;

function TQtFont.weight: Integer;
begin
  Result := QFont_weight(Widget);
end;

procedure TQtFont.setWeight(p1: Integer);
begin
  QFont_setWeight(Widget, p1);
end;

procedure TQtFont.setBold(p1: Boolean);
begin
  QFont_setBold(Widget, p1);
end;

procedure TQtFont.setItalic(b: Boolean);
begin
  QFont_setItalic(Widget, b);
end;

procedure TQtFont.setUnderline(p1: Boolean);
begin
  QFont_setUnderline(Widget, p1);
end;

procedure TQtFont.setStrikeOut(p1: Boolean);
begin
  QFont_setStrikeOut(Widget, p1);
end;

procedure TQtFont.setRawName(p1: string);
var
  Str: WideString;
begin
  Str := WideString(p1);

  QFont_setRawName(Widget, @Str);
end;

{ TQtDeviceContext }

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.Create
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
constructor TQtDeviceContext.Create(WidgetHandle: HWND);
var
  parent: QWidgetH;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtDeviceContext.Create ( WidgetHandle: ' + IntToStr(WidgetHandle) + ' )');
  {$endif}

  if WidgetHandle = 0 then Widget := QPainter_create
  else
  begin
    Parent := TQtMainWindow(WidgetHandle).Widget;
    Widget := QPainter_create(QWidget_to_QPaintDevice(Parent));
  end;

  vBrush := TQtBrush.Create(False);
  vFont := TQtFont.Create(False);
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtDeviceContext.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtDeviceContext.Destroy');
  {$endif}

  vBrush.Widget := nil;
  vBrush.Free;
  vFont.Widget := nil;
  vFont.Free;

  QPainter_destroy(Widget);

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.drawRect
  Params:  None
  Returns: Nothing
  
  Draws a rectangle. Helper function of winapi.Rectangle
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.drawRect(x1: Integer; y1: Integer; w: Integer; h: Integer);
begin
  QPainter_drawRect(Widget, x1, y1, w, h);
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.drawText
  Params:  None
  Returns: Nothing

  Draws a Text. Helper function of winapi.TextOut
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.drawText(x: Integer; y: Integer; s: PWideString);
begin
  QPainter_drawText(Widget, Origin.X + x, Origin.Y + y, s);
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.drawLine
  Params:  None
  Returns: Nothing

  Draws a Text. Helper function for winapi.LineTo
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.drawLine(x1: Integer; y1: Integer; x2: Integer; y2: Integer);
begin
  QPainter_drawLine(Widget, x1, y1, x2, y2);
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.drawEllipse
  Params:  None
  Returns: Nothing

  Draws a ellipse. Helper function for winapi.Ellipse
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.drawEllipse(x: Integer; y: Integer; w: Integer; h: Integer);
begin
  QPainter_drawEllipse(Widget, x, y, w, h);
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.setBrushOrigin
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.setBrushOrigin(x, y: Integer);
begin
  QPainter_setBrushOrigin(Widget, x, y);
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.brushOrigin
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.brushOrigin(retval: PPoint);
begin
  QPainter_brushOrigin(Widget, retval);
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.font
  Params:  None
  Returns: The current font object of the DC
 ------------------------------------------------------------------------------}
function TQtDeviceContext.font: TQtFont;
begin
  vFont.Widget := QPainter_font(Widget);

  Result := vFont;
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.setFont
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.setFont(f: TQtFont);
begin
//  if (f.Widget <> nil) and (Widget <> nil) then QPainter_setFont(Widget, f.Widget);
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.brush
  Params:  None
  Returns: The current brush object of the DC
 ------------------------------------------------------------------------------}
function TQtDeviceContext.brush: TQtBrush;
begin
  vBrush.Widget := QPainter_brush(Widget);

  Result := vBrush;
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.setBrush
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtDeviceContext.setBrush(brush: TQtBrush);
begin
  if (brush.Widget <> nil) and (Widget <> nil) then QPainter_setBrush(Widget, brush.Widget);
end;

{ TQtBrush }

{------------------------------------------------------------------------------
  Function: TQtBrush.Create
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
constructor TQtBrush.Create(CreateHandle: Boolean);
begin
  // Creates the widget
  {$ifdef VerboseQt}
    WriteLn('TQtBrush.Create CreateHandle: ', dbgs(CreateHandle));
  {$endif}

  if CreateHandle then Widget := QBrush_create;
end;

{------------------------------------------------------------------------------
  Function: TQtBrush.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtBrush.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtBrush.Destroy');
  {$endif}

  QBrush_destroy(Widget);

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Function: TQtBrush.setStyle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtBrush.setStyle(style: QtBrushStyle);
begin
  QBrush_setStyle(Widget, style);
end;

{ TQtMainWindow }

{------------------------------------------------------------------------------
  Function: TQtMainWindow.Create
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
constructor TQtMainWindow.Create(const AWinControl: TWinControl; const AParams: TCreateParams);
begin
  // Initializes the properties
  LCLObject := AWinControl;

  // Creates the widget
  {$ifdef VerboseQt}
    WriteLn('TQtMainWindow.Create');
  {$endif}
  Widget := QWidget_Create(nil, QtWindow);

  // Form initial position
  QWidget_setGeometry(Widget, AWinControl.Left, AWinControl.Top,
   AWinControl.Width, AWinControl.Height);

{  // Painting helper device
  Widget := QWidget_create;

  QWidget_setParent(Widget, Window);

  QWidget_setGeometry(Widget, AWinControl.Left, AWinControl.Top,
   AWinControl.Width, AWinControl.Height);

  // Required to implement OnPaint event
  QMainWindow_setCentralWidget(Window, Widget);

  // Accepts keyboard and mouse events
  QWidget_setFocusPolicy(Window, QtStrongFocus);
  QWidget_setFocusPolicy(Widget, QtStrongFocus);}
end;

{------------------------------------------------------------------------------
  Function: TQtMainWindow.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtMainWindow.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtMainWindow.Destroy');
  {$endif}

  QWidget_destroy(Widget);

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Function: TQtMainWindow.EventFilter
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TQtMainWindow.EventFilter(Sender: QObjectH; Event: QEventH): Boolean;
  cdecl;
begin
  Result := False;

  case QEvent_type(Event) of
   QEventWindowStateChange: SlotWindowStateChange;
  else
   // Inherited Callbacks
   inherited EventFilter(Sender, Event);
  end;
end;

{------------------------------------------------------------------------------
  Function: TQtMainWindow.SlotWindowStateChange
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtMainWindow.SlotWindowStateChange; cdecl;
var
  Msg: TLMSize;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtMainWindow.SlotWindowStateChange');
  {$endif}

  FillChar(Msg, SizeOf(Msg), #0);

  Msg.Msg := LM_SIZE;

  case QWidget_windowState(Widget) of
   QtWindowMinimized: Msg.SizeType := SIZEICONIC;
   QtWindowMaximized: Msg.SizeType := SIZEFULLSCREEN;
   QtWindowFullScreen: Msg.SizeType := SIZEFULLSCREEN;
  else
   Msg.SizeType := SIZENORMAL;
  end;

  Msg.SizeType := Msg.SizeType or Size_SourceIsInterface;

  Msg.Width := QWidget_width(Widget);
  Msg.Height := QWidget_height(Widget);

  try
    LCLObject.WindowProc(TLMessage(Msg));
  except
    Application.HandleException(nil);
  end;
end;

{ TQtStaticText }

{------------------------------------------------------------------------------
  Function: TQtStaticText.Create
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
constructor TQtStaticText.Create(const AWinControl: TWinControl; const AParams: TCreateParams);
var
  Str: WideString;
  Parent: QWidgetH;
begin
  // Initializes the properties
  LCLObject := AWinControl;

  // Creates the widget
  {$ifdef VerboseQt}
    WriteLn('TQtStaticText.Create');
  {$endif}
  Parent := TQtWidget(AWinControl.Parent.Handle).Widget;
  Widget := QLabel_create(Parent);

  // Sets it´ s initial properties
  QWidget_setGeometry(Widget, AWinControl.Left, AWinControl.Top,
   AWinControl.Width, AWinControl.Height);

  Str := WideString(AWinControl.Caption);
  SetText(@Str);
end;

{------------------------------------------------------------------------------
  Function: TQtStaticText.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtStaticText.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtStaticText.Destroy');
  {$endif}

  QLabel_destroy(QLabelH(Widget));

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Function: TQtStaticText.SetText
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtStaticText.SetText(text: PWideString);
begin
  QLabel_setText(QLabelH(Widget), text);
end;

{------------------------------------------------------------------------------
  Function: TQtStaticText.Text
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtStaticText.Text(retval: PWideString);
begin
  QLabel_text(QLabelH(Widget), retval);
end;

{ TQtTimer }

{------------------------------------------------------------------------------
  Function: TQtTimer.CreateTimer
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
constructor TQtTimer.CreateTimer(Interval: integer;
  const TimerFunc: TFNTimerProc; App: QObjectH);
var
  Method: TMethod;
  Hook : QObject_hookH;
begin
  AppObject := App;

  Id := QObject_startTimer(AppObject, Interval);
  
  CallbackFunc := TimerFunc;

  // Callback Event

  Hook := QObject_hook_create(AppObject);

  TEventFilterMethod(Method) := EventFilter;

  QObject_hook_hook_events(Hook, Method);
end;

{------------------------------------------------------------------------------
  Function: TQtTimer.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtTimer.Destroy;
begin
  QObject_killTimer(AppObject, id);

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Function: TQtTimer.EventFilter
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TQtTimer.EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl;
begin
  Result:=False;

  if QEvent_type(Event) = QEventTimer then
  begin
    QEvent_accept(Event);
    CallbackFunc;
  end;
end;

{ TQtCheckBox }

{------------------------------------------------------------------------------
  Function: TQtCheckBox.Create
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
constructor TQtCheckBox.Create(const AWinControl: TWinControl; const AParams: TCreateParams);
var
  Str: WideString;
  Parent: QWidgetH;
begin
  // Initializes the properties
  LCLObject := AWinControl;

  // Creates the widget
  {$ifdef VerboseQt}
    WriteLn('TQtCheckBox.Create');
  {$endif}

  if (AWinControl.Parent is TCustomCheckGroup) then
  begin
    Widget := QCheckBox_create;
    QLayout_addWidget(TQtGroupBox(AWinControl.Parent.Handle).VBoxLayout, Widget);
  end
  else
  begin
    Parent := TQtWidget(AWinControl.Parent.Handle).Widget;
    Widget := QCheckBox_create(Parent);

    // Sets it´ s initial properties
    QWidget_setGeometry(Widget, AWinControl.Left, AWinControl.Top,
     AWinControl.Width, AWinControl.Height);
  end;

  Str := WideString(AWinControl.Caption);
  SetText(@Str);
end;

{------------------------------------------------------------------------------
  Function: TQtCheckBox.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtCheckBox.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtCheckBox.Destroy');
  {$endif}

  QCheckBox_destroy(QCheckBoxH(Widget));

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Function: TQtCheckBox.EventFilter
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TQtCheckBox.EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl;
begin
  Result := False;

  // Inherited Callbacks
  inherited EventFilter(Sender, Event);
end;

{------------------------------------------------------------------------------
  Function: TQtCheckBox.CheckState
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TQtCheckBox.CheckState: QtCheckState;
begin
  Result := QCheckBox_checkState(QCheckBoxH(Widget));
end;

{------------------------------------------------------------------------------
  Function: TQtCheckBox.setCheckState
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtCheckBox.setCheckState(state: QtCheckState);
begin
  QCheckBox_setCheckState(QCheckBoxH(Widget), state);
end;

{ TQtRadioButton }

{------------------------------------------------------------------------------
  Function: TQtRadioButton.Create
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
constructor TQtRadioButton.Create(const AWinControl: TWinControl; const AParams: TCreateParams);
var
  Str: WideString;
  Parent: QWidgetH;
begin
  // Initializes the properties
  LCLObject := AWinControl;

  // Creates the widget
  {$ifdef VerboseQt}
    WriteLn('TQtRadioButton.Create');
  {$endif}
  
  if (AWinControl.Parent is TCustomRadioGroup) then
  begin
    Widget := QRadioButton_create;
    QLayout_addWidget(TQtGroupBox(AWinControl.Parent.Handle).VBoxLayout, Widget);
  end
  else
  begin
    Parent := TQtWidget(AWinControl.Parent.Handle).Widget;
    Widget := QRadioButton_create(Parent);

    // Sets it´ s initial properties
    QWidget_setGeometry(Widget, AWinControl.Left, AWinControl.Top,
     AWinControl.Width, AWinControl.Height);
  end;

  Str := WideString(AWinControl.Caption);
  SetText(@Str);
end;

{------------------------------------------------------------------------------
  Function: TQtRadioButton.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtRadioButton.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtRadioButton.Destroy');
  {$endif}

  QRadioButton_destroy(QRadioButtonH(Widget));

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Function: TQtRadioButton.EventFilter
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TQtRadioButton.EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl;
begin
  Result := False;
  
  // Inherited Callbacks
  inherited EventFilter(Sender, Event);
end;

{ TQtGroupBox }

{------------------------------------------------------------------------------
  Function: TQtGroupBox.Create
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
constructor TQtGroupBox.Create(const AWinControl: TWinControl;
  const AParams: TCreateParams);
var
  Parent: QWidgetH;
begin
  // Initializes the properties
  LCLObject := AWinControl;

  // Creates the widget
  {$ifdef VerboseQt}
    WriteLn('TQtGroupBox.Create');
  {$endif}
  Parent := TQtWidget(AWinControl.Parent.Handle).Widget;
  Widget := QGroupBox_create(Parent);

  // Sets it´ s initial properties
  QWidget_setGeometry(Widget, AWinControl.Left, AWinControl.Top,
   AWinControl.Width, AWinControl.Height);
   
  {------------------------------------------------------------------------------
    Adds a vertical layout if the control is a group
   ------------------------------------------------------------------------------}
  if (AWinControl is TCustomRadioGroup) or (AWinControl is TCustomCheckGroup) then
  begin
    VBoxLayout := QVBoxLayout_create;
    
    QWidget_setLayout(Widget, VBoxLayout);
  end;
end;

{------------------------------------------------------------------------------
  Function: TQtGroupBox.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtGroupBox.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtGroupBox.Destroy');
  {$endif}

  QGroupBox_destroy(QGroupBoxH(Widget));

  inherited Destroy;
end;

{ TQtFrame }

{------------------------------------------------------------------------------
  Function: TQtFrame.Create
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
constructor TQtFrame.Create(const AWinControl: TWinControl;
  const AParams: TCreateParams);
var
  Parent: QWidgetH;
begin
  // Initializes the properties
  LCLObject := AWinControl;

  // Creates the widget
  {$ifdef VerboseQt}
    WriteLn('TQtFrame.Create');
  {$endif}
  Parent := TQtWidget(AWinControl.Parent.Handle).Widget;
  Widget := QFrame_create(Parent);

  // Sets it´ s initial properties
  QWidget_setGeometry(Widget, AWinControl.Left, AWinControl.Top,
   AWinControl.Width, AWinControl.Height);
end;

{------------------------------------------------------------------------------
  Function: TQtFrame.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtFrame.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtFrame.Destroy');
  {$endif}

  QFrame_destroy(QFrameH(Widget));

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Function: TQtFrame.setFrameStyle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtFrame.setFrameStyle(p1: Integer);
begin
  QFrame_setFrameStyle(QFrameH(Widget), p1);
end;

{------------------------------------------------------------------------------
  Function: TQtFrame.setFrameShape
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtFrame.setFrameShape(p1: QFrameShape);
begin
  QFrame_setFrameShape(QFrameH(Widget), p1);
end;

{------------------------------------------------------------------------------
  Function: TQtFrame.setFrameShadow
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtFrame.setFrameShadow(p1: QFrameShadow);
begin
  QFrame_setFrameShadow(QFrameH(Widget), p1);
end;

{ TQtLineEdit }

{------------------------------------------------------------------------------
  Function: TQtLineEdit.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
constructor TQtLineEdit.Create(const AWinControl: TWinControl;
  const AParams: TCreateParams);
var
  Parent: QWidgetH;
  Str: WideString;
begin
  // Initializes the properties
  LCLObject := AWinControl;

  // Creates the widget
  {$ifdef VerboseQt}
    WriteLn('TQtLineEdit.Create');
  {$endif}
  Parent := TQtWidget(AWinControl.Parent.Handle).Widget;
  Str := WideString((AWinControl as TCustomEdit).Text);
  Widget := QLineEdit_create(@Str, Parent);

  // Sets it´ s initial properties
  QWidget_setGeometry(Widget, AWinControl.Left, AWinControl.Top,
   AWinControl.Width, AWinControl.Height);
end;

{------------------------------------------------------------------------------
  Function: TQtLineEdit.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtLineEdit.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtLineEdit.Destroy');
  {$endif}

  QLineEdit_destroy(QLineEditH(Widget));

  inherited Destroy;
end;

{ TQtTextEdit }

{------------------------------------------------------------------------------
  Function: TQtTextEdit.Create
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
constructor TQtTextEdit.Create(const AWinControl: TWinControl;
  const AParams: TCreateParams);
var
  Parent: QWidgetH;
  Str: WideString;

begin
  // Initializes the properties
  LCLObject := AWinControl;

  // Creates the widget
  {$ifdef VerboseQt}
    WriteLn('TQtTextEdit.Create');
  {$endif}
  
  Parent := TQtWidget(AWinControl.Parent.Handle).Widget;
  Str := (AWinControl as TCustomMemo).Text;
  Widget := QTextEdit_create(@Str, Parent);

  // Sets it´ s initial properties
  QWidget_setGeometry(Widget, AWinControl.Left, AWinControl.Top,
   AWinControl.Width, AWinControl.Height);
  QTextEdit_setReadOnly(QTextEditH(Widget),(AWinControl as TCustomMemo).ReadOnly);
  if (AWinControl as TCustomMemo).WordWrap then
     QTextEdit_setLineWrapMode(QTextEditH(Widget),QTextEditWidgetWidth)
  else
     QTextEdit_setLineWrapMode(QTextEditH(Widget),QTextEditNoWrap);
end;

{------------------------------------------------------------------------------
  Function: TQtTextEdit.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtTextEdit.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtTextEdit.Destroy');
  {$endif}

  QTextEdit_destroy(QTextEditH(Widget));

  inherited Destroy;
end;

{ TQtTabWidget }

{------------------------------------------------------------------------------
  Function: TQtTabWidget.Create
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
constructor TQtTabWidget.Create(const AWinControl: TWinControl;
  const AParams: TCreateParams);
var
  Parent: QWidgetH;
begin
  // Initializes the properties
  LCLObject := AWinControl;

  // Creates the widget
  {$ifdef VerboseQt}
    WriteLn('TQtTabWidget.Create');
  {$endif}
  Parent := TQtWidget(AWinControl.Parent.Handle).Widget;
  Widget := QTabWidget_create(Parent);

  // Sets it´ s initial properties
  QWidget_setGeometry(Widget, AWinControl.Left, AWinControl.Top,
   AWinControl.Width, AWinControl.Height);
end;

{------------------------------------------------------------------------------
  Function: TQtTabWidget.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtTabWidget.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtTabWidget.Destroy');
  {$endif}

  QTabWidget_destroy(QTabWidgetH(Widget));

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Function: TQtTabWidget.insertTab
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TQtTabWidget.insertTab(index: Integer; page: QWidgetH; p2: PWideString): Integer;
begin
  Result := QTabWidget_insertTab(QTabWidgetH(Widget), index, page, p2);
end;

{ TQtComboBox }

{------------------------------------------------------------------------------
  Function: TQtComboBox.Create
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
constructor TQtComboBox.Create(const AWinControl: TWinControl;
  const AParams: TCreateParams);
var
  Parent: QWidgetH;
  Str: WideString;
  i: Integer;
  data: QVariantH;
begin
  {------------------------------------------------------------------------------
    Creates dummy data
    
    This data is required, passing nil to QComboBox_addItem will cause a crash
   ------------------------------------------------------------------------------}
  data := QVariant_create(10);

  // Initializes the properties
  LCLObject := AWinControl;

  // Creates the widget
  {$ifdef VerboseQt}
    WriteLn('TQtComboBox.Create');
  {$endif}
  Parent := TQtWidget(AWinControl.Parent.Handle).Widget;
  Widget := QComboBox_create(Parent);

  // Add the items to the combo box
  for i := 0 to (AWinControl as TCustomComboBox).Items.Count - 1 do
  begin
    Str := WideString((AWinControl as TCustomComboBox).Items.Strings[i]);
    QComboBox_addItem(QComboBoxH(Widget), @Str, data);
  end;

  // Sets it´ s initial properties
  QWidget_setGeometry(Widget, AWinControl.Left, AWinControl.Top,
   AWinControl.Width, AWinControl.Height);

  // Clean up
  QVariant_destroy(data);
end;

{------------------------------------------------------------------------------
  Function: TQtComboBox.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtComboBox.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtComboBox.Destroy');
  {$endif}

  QComboBox_destroy(QComboBoxH(Widget));

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Function: TQtComboBox.currentIndex
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TQtComboBox.currentIndex: Integer;
begin
  Result := QComboBox_currentIndex(QComboBoxH(Widget));
end;

{------------------------------------------------------------------------------
  Function: TQtGroupBox.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtComboBox.setCurrentIndex(index: Integer);
begin
  QComboBox_setCurrentIndex(QComboBoxH(Widget), index);
end;

{ TQtSpinBox }

{------------------------------------------------------------------------------
  Function: TQtSpinBox.Create
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
constructor TQtSpinBox.Create(const AWinControl: TWinControl;
  const AParams: TCreateParams);
var
  Parent: QWidgetH;
begin
  // Initializes the properties
  LCLObject := AWinControl;

  // Creates the widget
  {$ifdef VerboseQt}
    WriteLn('TQtSpinBox.Create');
  {$endif}
  Parent := TQtWidget(AWinControl.Parent.Handle).Widget;
  Widget := QSpinBox_create(Parent);

  // Sets it´ s initial properties
  QWidget_setGeometry(Widget, AWinControl.Left, AWinControl.Top,
   AWinControl.Width, AWinControl.Height);
end;

{------------------------------------------------------------------------------
  Function: TQtSpinBox.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtSpinBox.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtSpinBox.Destroy');
  {$endif}

  QSpinBox_destroy(QSpinBoxH(Widget));

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Function: TQtListWidget.Create
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}//Luis Digital
constructor TQtListWidget.Create(const AWinControl: TWinControl;
  const AParams: TCreateParams);
var
  Parent: QWidgetH;
  Text: WideString;
  i: Integer;
begin
  // Initializes the properties
  LCLObject := AWinControl;

  // Creates the widget
  {$ifdef VerboseQt}
    WriteLn('TQListWidget.Create');
  {$endif}
  Parent := TQtWidget(AWinControl.Parent.Handle).Widget;
  Widget := QListWidget_create(Parent);
  
  // Sets the initial items
  for I := 0 to TCustomListBox(AWinControl).Items.Count - 1 do
  begin
    Text := WideString(TCustomListBox(AWinControl).Items.Strings[i]);
    QListWidget_addItem(QListWidgetH(Widget), @Text);
  end;

  // Sets it´ s initial properties
  QWidget_setGeometry(Widget, AWinControl.Left, AWinControl.Top,
   AWinControl.Width, AWinControl.Height);
end;

{------------------------------------------------------------------------------
  Function: TQtListWidget.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtListWidget.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('TQtListWidget.Destroy');
  {$endif}

  QListWidget_destroy(QListWidgetH(Widget));

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Function: TQtListWidget.currentRow
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TQtListWidget.currentRow: Integer;
begin
  Result := QListWidget_currentRow(QListWidgetH(Widget));
end;

{------------------------------------------------------------------------------
  Function: TQtListWidget.setCurrentRow
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtListWidget.setCurrentRow(row: Integer);
begin
  QListWidget_setCurrentRow(QListWidgetH(Widget), row);
end;

end.
