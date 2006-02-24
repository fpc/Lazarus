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
 *  See the file COPYING.LCL, included in this distribution,                 *
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
  qt4, Classes, SysUtils, LMessages, Forms, Controls, LCLType;

type

  { TQtWidget }

  TQtWidget = class(TObject)
  private
  public
    Widget: QWidgetH;
    LCLObject: TWinControl;
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

  { TQtButton }

  TQtButton = class(TQtAbstractButton)
  private
  public
    constructor Create(const AWinControl: TWinControl; const AParams: TCreateParams); virtual;
    destructor Destroy; override;
    procedure SlotClicked; cdecl;
  end;

  { TQtBrush }

  TQtBrush = class(QBrushH)
  private
  public
    Widget: QBrushH;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  { TQtDeviceContext }

  TQtDeviceContext = class(QPainterH)
  private
  public
    Widget: QPainterH;
    Brush: TQtBrush;
  public
    constructor Create(WidgetHandle: HWND); virtual;
    destructor Destroy; override;
    procedure drawRect(x1: Integer; y1: Integer; w: Integer; h: Integer);
  end;

  { TQtCustomForm }

  TQtCustomForm = class(TQtWidget)
  private
  public
    PaintBox : QWidgetH;
    Splitter : QSplitterH;
  public
    constructor Create(const AWinControl: TWinControl; const AParams: TCreateParams); virtual;
    destructor Destroy; override;
    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl;
    procedure SlotPaint; cdecl;
  end;

  { TQtStaticText }

  TQtStaticText = class(TQtWidget)
  private
  public
    constructor Create(const AWinControl: TWinControl; const AParams: TCreateParams); virtual;
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
    constructor Create(Interval: integer; TimerFunc: TFNTimerProc; App: QObjectH); virtual;
    destructor Destroy; override;
    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl;
  end;

  { TQtCheckBox }

  TQtCheckBox = class(TQtAbstractButton)
  public
    constructor Create(const AWinControl: TWinControl; const AParams: TCreateParams); virtual;
    destructor Destroy; override;
    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl;
    function CheckState: QtCheckState;
    procedure setCheckState(state: QtCheckState);
  end;

  { TQtRadioButton }

  TQtRadioButton = class(TQtAbstractButton)
  public
    constructor Create(const AWinControl: TWinControl; const AParams: TCreateParams); virtual;
    destructor Destroy; override;
    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl;
  end;

implementation

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

{ TQtButton }

{------------------------------------------------------------------------------
  Function: TQtButton.Create
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
constructor TQtButton.Create(const AWinControl: TWinControl; const AParams: TCreateParams);
var
  Str: WideString;
  Parent: QWidgetH;
begin
  // Initializes the properties
  LCLObject := AWinControl;

  // Creates the widget
  {$ifdef VerboseQt}
    WriteLn('Calling QPushButton_create');
  {$endif}
  Str := WideString(AWinControl.Caption);
  Parent := TQtWidget(AWinControl.Parent.Handle).Widget;
  Widget := QPushButton_create(@Str, Parent);

  // Sets it´ s initial properties
  QWidget_setGeometry(Widget, AWinControl.Left, AWinControl.Top,
   AWinControl.Width, AWinControl.Height);
end;

{------------------------------------------------------------------------------
  Function: TQtButton.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtButton.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('Calling QPushButton_destroy');
  {$endif}

  QPushButton_destroy(QPushButtonH(Widget));

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Function: TQtButton.SlotClicked
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtButton.SlotClicked; cdecl;
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
  // Creates the widget
  {$ifdef VerboseQt}
    WriteLn('Calling QPainter_create');
  {$endif}

  if WidgetHandle = 0 then Widget := QPainter_create
  else
  begin
    Parent := TQtCustomForm(WidgetHandle).PaintBox;
    Widget := QPainter_create(QWidget_to_QPaintDevice(Parent));
  end;

  // Creates the Brush
  Brush := TQtBrush.Create;
end;

{------------------------------------------------------------------------------
  Function: TQtDeviceContext.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtDeviceContext.Destroy;
begin
  Brush.Free;

  {$ifdef VerboseQt}
    WriteLn('Calling QPainter_destroy');
  {$endif}

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

{ TQtBrush }

{------------------------------------------------------------------------------
  Function: TQtBrush.Create
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
constructor TQtBrush.Create;
begin
  // Creates the widget
  {$ifdef VerboseQt}
    WriteLn('Calling QBrush_create');
  {$endif}

  Widget := QBrush_create;
end;

{------------------------------------------------------------------------------
  Function: TQtBrush.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtBrush.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('Calling QBrush_destroy');
  {$endif}

  QBrush_destroy(Widget);

  inherited Destroy;
end;

{ TQtCustomForm }

{------------------------------------------------------------------------------
  Function: TQtCustomForm.Create
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
constructor TQtCustomForm.Create(const AWinControl: TWinControl; const AParams: TCreateParams);
begin
  // Initializes the properties
  LCLObject := AWinControl;


  // Creates the widget
  {$ifdef VerboseQt}
    WriteLn('Calling QMainWindow_Create');
  {$endif}
  Widget := QMainWindow_Create;

  // Form initial position
  QWidget_setGeometry(Widget, AWinControl.Left, AWinControl.Top,
   AWinControl.Width, AWinControl.Height);
   
  // Painting helper device
  PaintBox := QWidget_create;

  QWidget_setParent(PaintBox, Widget);

  QWidget_setGeometry(PaintBox, AWinControl.Left, AWinControl.Top,
   AWinControl.Width, AWinControl.Height);
end;

{------------------------------------------------------------------------------
  Function: TQtCustomForm.Destroy
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
destructor TQtCustomForm.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('Calling QMainWindow_destroy');
  {$endif}

  QMainWindow_destroy(QMainWindowH(Widget));

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Function: TQtCustomForm.EventFilter
  Params:  None
  Returns: Nothing

  Receives various events from Qt and send the appropriate messages to the LCL
 ------------------------------------------------------------------------------}
function TQtCustomForm.EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl;
begin
  Result:=False;
  
  if QEvent_type(Event) = QEventPaint then SlotPaint;
end;

{------------------------------------------------------------------------------
  Function: TQtCustomForm.SlotPaint
  Params:  None
  Returns: Nothing

  Sends a LM_PAINT message to the LCL
 ------------------------------------------------------------------------------}
procedure TQtCustomForm.SlotPaint; cdecl;
var
  Msg: TLMessage;
begin
  FillChar(Msg, SizeOf(Msg), #0);

  Msg.Msg := LM_PAINT;

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
    WriteLn('Calling QLabel_create');
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
    WriteLn('Calling QLabel_destroy');
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

constructor TQtTimer.Create(Interval: integer; TimerFunc: TFNTimerProc; App: QObjectH);
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

destructor TQtTimer.Destroy;
begin
  QObject_killTimer(AppObject, id);

  inherited Destroy;
end;

function TQtTimer.EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl;
begin
  Result:=False;

  if QEvent_type(Event) = QEventTimer then CallbackFunc;
end;

{ TQtCheckBox }

constructor TQtCheckBox.Create(const AWinControl: TWinControl; const AParams: TCreateParams);
var
  Str: WideString;
  Parent: QWidgetH;
begin
  // Initializes the properties
  LCLObject := AWinControl;

  // Creates the widget
  {$ifdef VerboseQt}
    WriteLn('Calling QCheckBox_create');
  {$endif}
  Parent := TQtWidget(AWinControl.Parent.Handle).Widget;
  Widget := QCheckBox_create(Parent);

  // Sets it´ s initial properties
  QWidget_setGeometry(Widget, AWinControl.Left, AWinControl.Top,
   AWinControl.Width, AWinControl.Height);

  Str := WideString(AWinControl.Caption);
  SetText(@Str);
end;

destructor TQtCheckBox.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('Calling QCheckBox_destroy');
  {$endif}

  QCheckBox_destroy(QCheckBoxH(Widget));

  inherited Destroy;
end;

function TQtCheckBox.EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl;
begin

end;

function TQtCheckBox.CheckState: QtCheckState;
begin
  Result := QCheckBox_checkState(QCheckBoxH(Widget));
end;

procedure TQtCheckBox.setCheckState(state: QtCheckState);
begin
  QCheckBox_setCheckState(QCheckBoxH(Widget), state);
end;

{ TQtRadioButton }

constructor TQtRadioButton.Create(const AWinControl: TWinControl; const AParams: TCreateParams);
var
  Str: WideString;
  Parent: QWidgetH;
begin
  // Initializes the properties
  LCLObject := AWinControl;

  // Creates the widget
  {$ifdef VerboseQt}
    WriteLn('Calling QRadioButton_create');
  {$endif}
  Parent := TQtWidget(AWinControl.Parent.Handle).Widget;
  Widget := QRadioButton_create(Parent);

  // Sets it´ s initial properties
  QWidget_setGeometry(Widget, AWinControl.Left, AWinControl.Top,
   AWinControl.Width, AWinControl.Height);

  Str := WideString(AWinControl.Caption);
  SetText(@Str);
end;

destructor TQtRadioButton.Destroy;
begin
  {$ifdef VerboseQt}
    WriteLn('Calling QRadioButton_destroy');
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

end;

end.

