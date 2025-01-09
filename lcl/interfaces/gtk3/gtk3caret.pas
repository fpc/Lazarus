{
 *****************************************************************************
 *                               gtk3caret.pas                               *
 *                               -------------                               *
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit gtk3caret;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, LazGtk3, LazGdk3, LazGObject2, LazGLib2, LazGdkPixbuf2,
  LazPango1, LazPangoCairo1, LazCairo1;

type
  { TGtk3Caret }

  TGtk3Caret = class
  private
    FOwner: PGtkWidget;
    FVisible: Boolean;
    FBlinkTimerID: guint;
    FBlinkState: Boolean;
    FBlinkInterval: Integer;
    FPosX: Integer;
    FPosY: Integer;
    FWidth: Integer;
    FHeight: Integer;
    FRespondToFocus: Boolean;
    procedure RedrawCaret;
    procedure BlinkTimerCallback;
    procedure StartBlinking;
    procedure StopBlinking;
    procedure SetRespondToFocus(const ARespond: Boolean);
  public
    constructor Create(AOwner: PGtkWidget; AWidth, AHeight: Integer);
    destructor Destroy; override;
    procedure Show;
    procedure Hide;
    procedure SetPosition(const X, Y: Integer); //Set x and y at once and call one redraw for both.
    procedure SetBlinkInterval(const AInterval: Integer);
    property PosX: integer read FPosX;
    property PosY: integer read FPosY;
    property RespondToFocus: boolean read FRespondToFocus write SetRespondToFocus;
    property Visible: boolean read FVisible;
  end;

implementation
uses Gtk3Procs, gtk3int, Gtk3Objects, Gtk3Widgets;

{ TGtk3Caret }

procedure TGtk3Caret.RedrawCaret;
var
  cr: PCairo_t;
  W: TGtk3Widget;
  AHaveContext: Boolean;
begin
  {TODO: Implement bitmap caret.}
  if not Assigned(FOwner) or not FVisible then Exit;
  AHaveContext := False;
  W := Gtk3WidgetFromGtkWidget(FOwner);
  if W.Context <> 0 then
  begin
    AHaveContext := True;
    cr := TGtk3DeviceContext(W.Context).pcr;
  end;
  if Not AHaveContext then
    cr := gdk_cairo_create(gtk_widget_get_window(FOwner));
  try
    //writeln('Caret: BlinkState=',FBlinkState,' HaveContext=',AHaveContext,' X=',FPosX,' Y=',FPosY,' Self=',PtrUInt(Self));
    if FBlinkState then
    begin
      cairo_rectangle(cr, FPosX, FPosY, FWidth, FHeight);
      cairo_set_source_rgb(cr, 0, 0, 0);
      cairo_fill(cr);
      //cairo_stroke(cr);
    end else
    begin
      cairo_rectangle(cr, FPosX, FPosY, FWidth, FHeight);
      cairo_set_source_rgb(Cr, 1, 1, 1);
      cairo_fill(cr);
      //cairo_stroke(cr);
    end;
  finally
    if not AHaveContext then
      cairo_destroy(cr);
  end;
end;

constructor TGtk3Caret.Create(AOwner: PGtkWidget; AWidth, AHeight: Integer);
var
  ASettings: PGtkSettings;
  AValue: TGValue;
begin
  inherited Create;
  FOwner := AOwner;
  FWidth := AWidth;
  FHeight := AHeight;
  FVisible := False;
  FPosX := 0;
  FPosY := 0;
  FBlinkState := True;
  FBlinkTimerID := 0;
  ASettings := gtk_settings_get_default;
  FillByte(AValue{%H-}, SizeOf(AValue), 0);
  AValue.init(G_TYPE_INT);
  ASettings^.get_property('gtk-cursor-blink-time', @AValue);
  FBlinkInterval := AValue.get_int div CURSOR_ON_MULTIPLIER; // (AValue.get_int * CURSOR_OFF_MULTIPLIER) div CURSOR_DIVIDER;
  AValue.unset;
end;

destructor TGtk3Caret.Destroy;
begin
  StopBlinking;
  Hide;
  inherited Destroy;
end;

procedure TGtk3Caret.SetPosition(const X, Y: Integer);
begin
  if FVisible then
    gtk_widget_queue_draw_area(FOwner, FPosX, FPosY, FWidth, FHeight);
  FPosX := X;
  FPosY := Y;
  if FVisible then
    RedrawCaret;
end;

procedure TGtk3Caret.Show;
begin
  if FVisible then Exit;
  FVisible := True;
  FBlinkState := True;
  StartBlinking;
  RedrawCaret;
end;

procedure TGtk3Caret.Hide;
begin
  if not FVisible then Exit;
  StopBlinking;
  RedrawCaret;
  FVisible := False;
end;

procedure TGtk3Caret.BlinkTimerCallback;
begin
  FBlinkState := not FBlinkState;
  RedrawCaret;
end;

function BlinkTimerCallbackFunc(Data: gpointer): gboolean; cdecl;
begin
  TGtk3Caret(Data).BlinkTimerCallback;
  Result := True;
end;

procedure TGtk3Caret.StartBlinking;
begin
  if FBlinkTimerID <> 0 then Exit;
  FBlinkTimerID := g_timeout_add(FBlinkInterval, @BlinkTimerCallbackFunc, Self);
end;

procedure TGtk3Caret.StopBlinking;
begin
  if FBlinkTimerID = 0 then Exit;
  g_source_remove(FBlinkTimerID);
  FBlinkTimerID := 0;
  FBlinkState := False;
end;

procedure TGtk3Caret.SetBlinkInterval(const AInterval: Integer);
begin
  if FBlinkInterval = AInterval then
    exit;
  FBlinkInterval := AInterval;
  if FBlinkTimerID <> 0 then
  begin
    StopBlinking;
    StartBlinking;
  end;
end;

function FocusEventHandler({%H-}AWidget: PGtkWidget; AEvent: PGdkEvent; AUserData: gpointer): gboolean; cdecl;
var
  Caret: TGtk3Caret;
  Event: PGdkEventFocus;
begin
  Result := gtk_false;
  Caret := TGtk3Caret(AUserData);
  if not Assigned(Caret) then
    exit;
  if AEvent^.type_ = GDK_FOCUS_CHANGE then
  begin
    Event := PGdkEventFocus(AEvent);
    if Event^.in_ <> 0 then
      Caret.Show
    else
      Caret.Hide;
    Result := gtk_true;
  end;
end;

procedure TGtk3Caret.SetRespondToFocus(const ARespond: Boolean);
begin
  if FRespondToFocus = ARespond then Exit;

  FRespondToFocus := ARespond;
  if FRespondToFocus then
  begin
    g_signal_connect_data(FOwner, 'focus-in-event', TGCallback(@FocusEventHandler), Self, nil, []);
    g_signal_connect_data(FOwner, 'focus-out-event', TGCallback(@FocusEventHandler), Self, nil, []);
  end else
  begin
    //writeln('Disconnecting focus_in/focus_out events !');
    //focus-in-event
    g_signal_handlers_disconnect_matched(FOwner, [G_SIGNAL_MATCH_DATA], 0, 0, nil,
      nil, Self);
    //focus-out-event
    g_signal_handlers_disconnect_matched(FOwner, [G_SIGNAL_MATCH_DATA], 0, 0, nil,
      nil, Self);
  end;
end;

end.
