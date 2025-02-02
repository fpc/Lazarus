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

uses Classes, SysUtils, LazGtk3, LazGdk3, LazGObject2, LazGLib2, LazCairo1,
  LCLType;

type
  { TGtk3Caret }

  TGtk3Caret = class
  private
    FOwner: PGtkWidget;
    FVisible: Boolean;
    FBlinkTimerID: guint;
    FBlinkState: Boolean;
    FBlinkInterval: Integer;
    FPos: TPoint;
    FLastPos: TPoint;
    FWidth: Integer;
    FHeight: Integer;
    FRespondToFocus: Boolean;
    FPosChanging: boolean;
    procedure BlinkTimerCallback;
    procedure CairoDrawCaret(cr: Pcairo_t; const X, Y: integer);
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
    procedure RedrawCaret;
    property PosX: integer read FPos.X;
    property PosY: integer read FPos.Y;
    property RespondToFocus: boolean read FRespondToFocus write SetRespondToFocus;
    property Visible: boolean read FVisible;
  end;

implementation
uses Gtk3Procs, gtk3int, Gtk3Widgets;

{ TGtk3Caret }

procedure TGtk3Caret.CairoDrawCaret(cr: Pcairo_t; const X, Y: integer);
begin
  if cr = nil then
    exit;
  cairo_set_operator(cr, CAIRO_OPERATOR_DIFFERENCE);
  if FBlinkState then
    cairo_set_source_rgb(cr, 0, 0, 0)
  else
    cairo_set_source_rgb(cr, 1, 1, 1);
  cairo_rectangle(cr, X, Y , FWidth, FHeight);
  cairo_fill(cr);
end;

procedure TGtk3Caret.RedrawCaret;
var
  cr: PCairo_t;
  W: TGtk3Widget;
  AHaveContext: Boolean;
  R: TRect;
begin
  {TODO: Implement bitmap caret.}
  if not Assigned(FOwner) or not FVisible then Exit;

  W := TGtk3Widget(HwndFromGtkWidget(FOwner));
  if W = nil then
    exit;
  if Gtk3IsGdkWindow(gtk_widget_get_window(FOwner)) then
   cr := gdk_cairo_create(gtk_widget_get_window(FOwner))
  else
    exit;
  try
    cairoDrawCaret(cr, FPos.X, FPos.Y);
  finally
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
  FPos.X := -1;
  FPos.Y := -1;
  FLastPos.X := -1;
  FLastPos.Y := -1;
  FBlinkState := True;
  FBlinkTimerID := 0;
  ASettings := gtk_settings_get_default;
  FillByte(AValue{%H-}, SizeOf(AValue), 0);
  AValue.init(G_TYPE_INT);
  ASettings^.get_property('gtk-cursor-blink-time', @AValue);
  FBlinkInterval := (AValue.get_int div CURSOR_ON_MULTIPLIER) div 2; // (AValue.get_int * CURSOR_OFF_MULTIPLIER) div CURSOR_DIVIDER;
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
  if (FPos.X = X) and (FPos.Y = Y) then
    exit;
  FPosChanging := True; // stop timer changing FBlinkState and call RedrawCaret until we go out from this proc.
  FBlinkState := False;
  if FVisible then
  begin
    if (FLastPos.X >= 0) and (FLastPos.Y >=0) and (FLastPos.X <> FPos.X) or (FLastPos.Y <> FPos.Y) then
      FOwner^.queue_draw_area(FLastPos.X, FLastPos.Y, FWidth, FHeight);
    if (FPos.X >= 0) and (FPos.Y >= 0) then
      FOwner^.queue_draw_area(FPos.X, FPos.Y, FWidth, FHeight);
  end;

  FLastPos.X := FPos.X;
  FLastPos.Y := FPos.Y;

  FPos.X := X;
  FPos.Y := Y;

  if FVisible then
    RedrawCaret; // redraw caret with CAIRO_OPERATOR_DIFFERENCE while FBlinkState = false.
  FBlinkState := True;
  if FVisible then
    RedrawCaret;
  FPosChanging := False;
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
var
  W: TGtk3Widget;
begin
  if FPosChanging or not FVisible then
    exit;
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
    g_signal_handlers_disconnect_matched(FOwner, [G_SIGNAL_MATCH_DATA], 0, 0, nil,
      nil, Self);
    g_signal_handlers_disconnect_matched(FOwner, [G_SIGNAL_MATCH_DATA], 0, 0, nil,
      nil, Self);
  end;
end;

end.
