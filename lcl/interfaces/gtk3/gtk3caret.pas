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
    procedure RepaintLayoutCaret(const ARect: TRect);
    procedure StartBlinking;
    procedure StopBlinking;
    procedure SetRespondToFocus(const ARespond: Boolean);
  public
    constructor Create(AOwner: PGtkWidget; AWidth, AHeight: Integer);
    destructor Destroy; override;
    procedure Show;
    procedure Hide;
    procedure CairoDrawCaret(cr: Pcairo_t);
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

procedure TGtk3Caret.RepaintLayoutCaret(const ARect: TRect);
var
  aWindow: PGdkWindow;
  aRegion: Pcairo_region_t;
  tmpSurf: Pcairo_surface_t;
  tmpCtx, cr: Pcairo_t;
  ACairoRect: Tcairo_rectangle_int_t;
  aLayout: PGtkLayout;
begin
  if not Gtk3IsLayout(FOwner) then
    exit;
  aLayout := PGtkLayout(FOwner);
  if Gtk3IsGdkWindow(aLayout^.get_bin_window) then
  begin
    aWindow := aLayout^.get_bin_window;
    cr := gdk_cairo_create(aWindow);
    aRegion := gdk_window_get_visible_region(aWindow);
    cairo_region_get_extents(aRegion, @ACairoRect);
    tmpSurf := cairo_surface_create_similar(cairo_get_target(cr), CAIRO_CONTENT_COLOR_ALPHA, ARect.Width, ARect.Height);
    tmpCtx := cairo_create(tmpSurf);
    cairo_translate(tmpCtx, ACairoRect.X, ACairoRect.Y);
    self.CairoDrawCaret(tmpCtx);
    cairo_set_source_surface(cr, tmpSurf, ARect.Left, ARect.Top);
    cairo_paint(cr);
    cairo_destroy(tmpCtx);
    cairo_surface_destroy(tmpSurf);
    cairo_destroy(cr);
    cairo_region_destroy(aRegion);
  end;
end;

{ TGtk3Caret }

procedure TGtk3Caret.RedrawCaret;
var
  W: TGtk3Widget;
begin
  {TODO: Implement bitmap caret.}
  if not Assigned(FOwner) or not FVisible then Exit;

  W := TGtk3Widget(HwndFromGtkWidget(FOwner));
  if W = nil then
    exit;
  if W.Context > 0 then
    exit
  else
  begin
    RepaintLayoutCaret(Bounds(FPos.X, FPos.Y, FWidth, FHeight));
    FOwner^.queue_draw_area(FPos.X, FPos.Y, FWidth, FHeight);
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
  FBlinkInterval := (AValue.get_int div CURSOR_ON_MULTIPLIER);
  AValue.unset;
end;

destructor TGtk3Caret.Destroy;
begin
  StopBlinking;
  Hide;
  inherited Destroy;
end;

procedure TGtk3Caret.SetPosition(const X, Y: Integer);
var
  W: TGtk3Widget;
begin
  if (FPos.X = X) and (FPos.Y = Y) then
    exit;
  FPosChanging := True; // stop timer changing FBlinkState and call RedrawCaret until we go out from this proc.
  W := TGtk3Widget(HwndFromGtkWidget(FOwner));
  if FVisible then
  begin
    if Assigned(W) and (W.Context = 0) then
    begin
      if (FLastPos.X >= 0) and (FLastPos.Y >=0) and (FLastPos.X <> FPos.X) or (FLastPos.Y <> FPos.Y) then
      begin
        FBlinkState := False;
        RepaintLayoutCaret(Bounds(FLastPos.X, FLastPos.Y, FWidth, FHeight));
        FOwner^.queue_draw_area(FLastPos.X, FLastPos.Y, FWidth, FHeight);
      end;
      if (FPos.X >= 0) and (FPos.Y >= 0) then
      begin
        FBlinkState := False;
        RepaintLayoutCaret(Bounds(FPos.X, FPos.Y, FWidth, FHeight));
        FOwner^.queue_draw_area(FPos.X, FPos.Y, FWidth, FHeight);
      end;
    end;
  end;

  FLastPos.X := FPos.X;
  FLastPos.Y := FPos.Y;

  FPos.X := X;
  FPos.Y := Y;
  if FVisible then
  begin
    if Assigned(W) and (W.Context = 0) then
    begin
      FBlinkState := True;
      RepaintLayoutCaret(Bounds(FPos.X, FPos.Y, FWidth, FHeight));
      FOwner^.queue_draw_area(FPos.X, FPos.Y, FWidth, FHeight);
    end;
  end;
  FBlinkState := not FBlinkState;
  FPosChanging := False;
end;

procedure TGtk3Caret.Show;
begin
  if FVisible then Exit;
  FVisible := True;
  FBlinkState := not FBlinkState;
  StartBlinking;
  RedrawCaret;
end;

procedure TGtk3Caret.Hide;
begin
  if not FVisible then Exit;
  FBlinkState := not FBlinkState;
  RedrawCaret;
  FVisible := False;
end;

procedure TGtk3Caret.CairoDrawCaret(cr: Pcairo_t);
begin
  cairo_save(cr);
  cairo_move_to(cr, FPos.X, FPos.Y);
  cairo_set_operator(cr, CAIRO_OPERATOR_DIFFERENCE);
  if FBlinkState then
    cairo_set_source_rgb(cr, 0, 0, 0)
  else
    cairo_set_source_rgb(cr, 1, 1, 1);
  cairo_rectangle(cr, FPos.X, FPos.Y , FWidth, FHeight);
  cairo_fill(cr);
  cairo_restore(cr);
end;

procedure TGtk3Caret.BlinkTimerCallback;
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
  BlinkTimerCallback;
end;

procedure TGtk3Caret.StopBlinking;
begin
  if FBlinkTimerID = 0 then Exit;
  g_source_remove(FBlinkTimerID);
  FBlinkTimerID := 0;
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

procedure TGtk3Caret.SetRespondToFocus(const ARespond: Boolean);
begin
  if FRespondToFocus = ARespond then Exit;

  FRespondToFocus := ARespond;
end;

end.

