unit gtk3caret;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazGtk3, LazGdk3, LazGObject2, LazGLib2, LazCairo1, LCLType,
  gtk3procs;

type
  { TGtk3Caret }
  TGtk3Caret = class
  private
    FOwner: PGtkWidget;
    FCaretWidget: PGtkWidget;
    FBlinkTimerID: guint;
    FBlinkState: Boolean;
    FBlinkInterval: Integer;
    FWidth: Integer;
    FHeight: Integer;
    FRespondToFocus: Boolean;
    FPos: TPoint;
    FVisible: boolean;
    procedure BlinkTimerCallback; cdecl;
    procedure StartBlinking;
    procedure StopBlinking;
    class procedure OnDraw(AWidget: PGtkWidget; cr: PCairo_t; Data: gPointer); cdecl; static;
  public
    constructor Create(AOwner: PGtkWidget; AWidth, AHeight: Integer);
    destructor Destroy; override;
    procedure Show;
    procedure Hide;
    procedure SetPosition(X, Y: Integer);
    procedure SetBlinkInterval(AInterval: Integer);
    procedure SetRespondToFocus(ARespond: Boolean);
    property RespondToFocus: Boolean read FRespondToFocus write SetRespondToFocus;
    property PosX: integer read FPos.X;
    property PosY: integer read FPos.Y;
  end;

implementation
uses gtk3widgets, gtk3int;

function BlinkTimerCallbackFunc(Data: gpointer): gboolean; cdecl;
begin
  TGtk3Caret(Data).BlinkTimerCallback;
  Result := True;
end;

constructor TGtk3Caret.Create(AOwner: PGtkWidget; AWidth, AHeight: Integer);
var
  ASettings: PGtkSettings;
  AValue: TGValue;
begin
  inherited Create;
  FVisible := False;
  FPos := Point(0, 0);
  FOwner := AOwner;
  FWidth := AWidth;
  FHeight := AHeight;
  FBlinkState := True;
  FBlinkTimerID := 0;
  FRespondToFocus := False;

  FCaretWidget := gtk_drawing_area_new();
  gtk_widget_set_size_request(FCaretWidget, FWidth, FHeight);

  // Set drawing event for the caret
  g_signal_connect_data(FCaretWidget, 'draw', TGCallback(@OnDraw), Self, nil, G_CONNECT_DEFAULT);
  PGtkFixed(FOwner)^.put(FCaretWidget, 0, 0);

  g_object_set_data(FCaretWidget,'lclwidget', TGtk3Widget(HwndFromGtkWidget(FOwner)));
  FCaretWidget^.set_can_focus(False);
  FCaretWidget^.show;
  FCaretWidget^.set_opacity(0);

  ASettings := gtk_settings_get_default;

  FillByte(AValue, SizeOf(AValue), 0);
  AValue.init(G_TYPE_INT);
  ASettings^.get_property('gtk-cursor-blink-time', @AValue);
  FBlinkInterval := (AValue.get_int div CURSOR_ON_MULTIPLIER);
  AValue.unset;
  StartBlinking;
end;

destructor TGtk3Caret.Destroy;
begin
  StopBlinking;
  gtk_widget_destroy(FCaretWidget);
  inherited Destroy;
end;

procedure TGtk3Caret.BlinkTimerCallback; cdecl;
begin
  FBlinkState := not FBlinkState;
  gtk_widget_queue_draw(FCaretWidget);
end;

procedure TGtk3Caret.StartBlinking;
begin
  if FBlinkTimerID <> 0 then Exit;
  FBlinkTimerID := g_timeout_add_full(G_PRIORITY_DEFAULT_IDLE, FBlinkInterval, @BlinkTimerCallbackFunc, Self, nil);
end;

procedure TGtk3Caret.StopBlinking;
begin
  if FBlinkTimerID = 0 then Exit;
  g_source_remove(FBlinkTimerID);
  FBlinkTimerID := 0;
  FBlinkState := False;
end;

class procedure TGtk3Caret.OnDraw(AWidget: PGtkWidget; cr: PCairo_t; Data: GPointer); cdecl;
begin
  if Data = nil then
    exit;
  if not TGtk3Caret(Data).FVisible then
  begin
    cairo_set_operator(cr, CAIRO_OPERATOR_CLEAR);
    cairo_paint(cr);
  end else
  begin
    if TGtk3Caret(Data).FBlinkState then
      cairo_set_source_rgb(cr, 0, 0, 0)
    else
      cairo_set_source_rgba(cr, 1, 1, 1, 0);
    cairo_rectangle(cr, 0, 0, TGtk3Caret(Data).FWidth, TGtk3Caret(Data).FHeight);
    cairo_fill(cr);
  end;
end;

procedure TGtk3Caret.Show;
begin
  if not FCaretWidget^.get_visible then
    gtk_widget_show(FCaretWidget);
  FVisible := True;
  if FCaretWidget^.get_opacity = 0 then
    FCaretWidget^.set_opacity(1); // triggers redraw
end;

procedure TGtk3Caret.Hide;
begin
  FVisible := False;
end;

procedure TGtk3Caret.SetPosition(X, Y: Integer);
begin
  FPos.X := X;
  FPos.Y := Y;
  gtk_fixed_move(PGtkFixed(FOwner), FCaretWidget, X, Y);
end;

procedure TGtk3Caret.SetBlinkInterval(AInterval: Integer);
begin
  if FBlinkInterval = AInterval then Exit;
  FBlinkInterval := AInterval;
  if FBlinkTimerID <> 0 then
  begin
    StopBlinking;
    StartBlinking;
  end;
end;

procedure TGtk3Caret.SetRespondToFocus(ARespond: Boolean);
begin
  if FRespondToFocus = ARespond then Exit;
  FRespondToFocus := ARespond;
end;

end.

