{
 *****************************************************************************
 *                               gtk3mdiemulator.pas                         *
 *                               -------------------                         *
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  GTK3 MDI emulation - ported from my old, never-commited gtk2 mdi emulator.
  Hope this one will serve for years :)

  Provides TGtk3MDIWorkspace, a scrollable desktop and TGtk3MDIChildFrame, which
  hosts TGtk3MdiChildWindow (TGtk3Window).

  Author: Željan Rikalo

}

unit gtk3mdiemulator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Controls, Graphics,
  LazCairo1, LazGtk3, LazGdk3, LazGObject2, LazGLib2, LazPango1, LazPangoCairo1,
  LazGdkPixbuf2;

type
  TGtk3MDIWindowState = (mwsNormal, mwsMinimized, mwsMaximized);

  //Titlebar button identity.
  TGtk3MDIButtonKind = (mbkMin, mbkMax, mbkClose, mbkRestore);

  TGtk3MDIChildFrame = class;

  TGtk3MDICloseQueryEvent = function(AFrame: TGtk3MDIChildFrame): Boolean of object;
  TGtk3MDINotifyEvent = procedure(AFrame: TGtk3MDIChildFrame) of object;

  TGtk3MDIWorkspace = class;

  { TGtk3MDIChildFrame }

  TGtk3MDIChildFrame = class
  private
    FWorkspace: TGtk3MDIWorkspace;
    FFrame: PGtkWidget;
    FBox: PGtkWidget;
    FTitleBar: PGtkWidget;
    FClientHost: PGtkWidget;
    FCaption: String;
    FState: TGtk3MDIWindowState;
    FActive: Boolean;
    FUserData: Pointer;
    FBoundsBeforeMax: TRect;
    FDragging: Boolean;
    FDragStartX, FDragStartY: gint;
    FFrameStartX, FFrameStartY: gint;
    FScale: Double;
    FTitleBarH: gint;
    FBtnW: gint;
    FBorderW: gint;
    FResizeBorder: gint;
    FResizing: Boolean;
    FResizeEdges: Integer;
    FResizeStartX, FResizeStartY: gint;
    FResizeStartBounds: TRect;
    FMinimizedSlot: Integer;
    FBoundsBeforeMin: TRect;
    FStateBeforeMin: TGtk3MDIWindowState;
    FCurX, FCurY: Integer;
    FCurW, FCurH: Integer;
    FCaptureGesture: PGtkGesture;
    FBorderStyle: TFormBorderStyle;
    FSavedCursor: PGdkCursor;
    FSavedCursorValid: Boolean;
    FCursorOnEdge: Boolean;
    procedure SetCaption(const AValue: String);
    procedure SetActive(AValue: Boolean);
    procedure SetState(AValue: TGtk3MDIWindowState);
    procedure ComputeMetrics;
    function ButtonAtX(AX: gint): Integer;
    function EdgeAtPoint(AX, AY: gint): Integer;
    class function CursorNameForEdge(AEdge: Integer): PgChar; static;
    procedure SetFrameCursor(const AName: PgChar);
    procedure ApplyFrameResize(AMouseRootX, AMouseRootY: gint);
    class function FrameDraw(widget: PGtkWidget; cr: Pcairo_t; data: gpointer): gboolean; cdecl; static;
    class function TitleBarDraw(widget: PGtkWidget; cr: Pcairo_t; data: gpointer): gboolean; cdecl; static;
    class function TitleBarButtonPress(widget: PGtkWidget; event: PGdkEventButton; data: gpointer): gboolean; cdecl; static;
    class function TitleBarButtonRelease(widget: PGtkWidget; event: PGdkEventButton; data: gpointer): gboolean; cdecl; static;
    class function TitleBarMotionNotify(widget: PGtkWidget; event: PGdkEventMotion; data: gpointer): gboolean; cdecl; static;
    class function FrameButtonPress(widget: PGtkWidget; event: PGdkEventButton; data: gpointer): gboolean; cdecl; static;
    class function FrameButtonRelease(widget: PGtkWidget; event: PGdkEventButton; data: gpointer): gboolean; cdecl; static;
    class function FrameMotionNotify(widget: PGtkWidget; event: PGdkEventMotion; data: gpointer): gboolean; cdecl; static;
    class procedure FrameCaptureGesturePressed(gesture: PGtkGestureMultiPress; n_press: gint; x, y: gdouble; data: gpointer); cdecl; static;
    class procedure FrameSetFocusChild(container: PGtkContainer; widget: PGtkWidget; data: gpointer); cdecl; static;
    class procedure FrameGtkDestroy(widget: PGtkWidget; data: gpointer); cdecl; static;
    class function DeferCloseChild(data: gpointer): gboolean; cdecl; static;
    class function DeferFirstPaint(data: gpointer): gboolean; cdecl; static;
  public
    property TitleBarH: gint read FTitleBarH;
    property BtnW: gint read FBtnW;
  public
    constructor Create(AWorkspace: TGtk3MDIWorkspace);
    destructor Destroy; override;
    procedure AddClientHost(AContent: PGtkWidget);
    procedure SetBorderStyle(AValue: TFormBorderStyle);
    procedure SetBounds(L, T, W, H: Integer);
    function GetBounds: TRect;
    procedure RefitMaximized(AViewportW, AViewportH: Integer);
    procedure BringToFront;
    procedure Minimize;
    procedure Maximize;
    procedure Restore;
    property Frame: PGtkWidget read FFrame;
    property ClientHost: PGtkWidget read FClientHost;
    property Caption: String read FCaption write SetCaption;
    property State: TGtk3MDIWindowState read FState write SetState;
    property Active: Boolean read FActive write SetActive;
    property UserData: Pointer read FUserData write FUserData;
    property Workspace: TGtk3MDIWorkspace read FWorkspace;
  end;

  { TGtk3MDIWorkspace }

  TGtk3MDIWorkspace = class
  private
    FWidget: PGtkWidget;
    FContainer: PGtkWidget;
    FChildren: TFPList;
    FMinimizedSlots: TFPList;
    FActiveChild: TGtk3MDIChildFrame;
    FNextChildX, FNextChildY: Integer;
    FOwnerForm: TWinControl;
    FOnChildActivated: TGtk3MDINotifyEvent;
    FOnChildDeactivated: TGtk3MDINotifyEvent;
    FOnChildCloseQuery: TGtk3MDICloseQueryEvent;
    FOnChildStateChanged: TGtk3MDINotifyEvent;
    FOnNoActiveChild: TNotifyEvent;
    procedure SetActiveChildInternal(AValue: TGtk3MDIChildFrame);
    function AcquireMinimizedSlot(AFrame: TGtk3MDIChildFrame): Integer;
    procedure ReleaseMinimizedSlot(ASlot: Integer);
    procedure LayoutMinimizedFrame(AFrame: TGtk3MDIChildFrame);
    class procedure WorkspaceGtkDestroy(widget: PGtkWidget; data: gpointer); cdecl; static;
    class function WorkspaceDraw(widget: PGtkWidget; cr: Pcairo_t; data: gpointer): gboolean; cdecl; static;
  public
    constructor Create;
    destructor Destroy; override;
    function AddChild(const ACaption: String): TGtk3MDIChildFrame;
    procedure RemoveChild(AFrame: TGtk3MDIChildFrame);
    procedure ActivateChild(AFrame: TGtk3MDIChildFrame);
    procedure RelayoutMinimized;
    function ChildAtPoint(X, Y: gint): TGtk3MDIChildFrame;
    function FindNextActivatableChild(AExclude: TGtk3MDIChildFrame): TGtk3MDIChildFrame;
    property Widget: PGtkWidget read FWidget;
    property Container: PGtkWidget read FContainer;
    property Children: TFPList read FChildren;
    property OwnerForm: TWinControl read FOwnerForm write FOwnerForm;
    property ActiveChild: TGtk3MDIChildFrame read FActiveChild
      write SetActiveChildInternal;

    property OnChildActivated: TGtk3MDINotifyEvent
      read FOnChildActivated write FOnChildActivated;
    property OnChildDeactivated: TGtk3MDINotifyEvent
      read FOnChildDeactivated write FOnChildDeactivated;
    property OnChildCloseQuery: TGtk3MDICloseQueryEvent
      read FOnChildCloseQuery write FOnChildCloseQuery;
    property OnChildStateChanged: TGtk3MDINotifyEvent
      read FOnChildStateChanged write FOnChildStateChanged;
    property OnNoActiveChild: TNotifyEvent
      read FOnNoActiveChild write FOnNoActiveChild;
  end;

implementation

const
  TITLEBAR_HEIGHT = 24;
  DEFAULT_CHILD_W = 320;
  DEFAULT_CHILD_H = 200;
  CASCADE_OFFSET = 26;
  BTN_W = 22; //default titlebar buttons width
  BTN_COUNT = 3; //count of title bar right-side buttons
  BUTTONS_AREA_W = BTN_W * BTN_COUNT;
  MIN_ICON_W = 160; // width of a minimized child's titlebar strip
  RESIZE_BORDER = 6; //px at 96 DPI for resize hot zone
  EDGE_N = 1;
  EDGE_S = 2;
  EDGE_W = 4;
  EDGE_E = 8;
  MIN_CHILD_W = 80; //floor for resize so frame can't collapse
  MIN_CHILD_H = 0; //computed at runtime. titlebar + borders



{ TGtk3MDIChildFrame }

constructor TGtk3MDIChildFrame.Create(AWorkspace: TGtk3MDIWorkspace);
begin
  inherited Create;
  FWorkspace := AWorkspace;
  FCaption := '';
  FState := mwsNormal;
  FActive := False;
  FBoundsBeforeMax := Rect(0, 0, 0, 0);
  FBoundsBeforeMin := Rect(0, 0, 0, 0);
  FStateBeforeMin := mwsNormal;
  FMinimizedSlot := -1;
  FBorderStyle := bsSizeable;

  FFrame := gtk_event_box_new;
  g_object_set_data(PGObject(FFrame), 'lcl-mdi-frame', Self);

  FBox := gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
  PGtkContainer(FFrame)^.add(FBox);

  ComputeMetrics;
  PGtkWidget(FFrame)^.set_size_request(Round(DEFAULT_CHILD_W * FScale), Round(DEFAULT_CHILD_H * FScale));

  gtk_widget_set_margin_start(FBox, FBorderW);
  gtk_widget_set_margin_end(FBox, FBorderW);
  gtk_widget_set_margin_top(FBox, FBorderW);
  gtk_widget_set_margin_bottom(FBox, FBorderW);

  PGtkWidget(FFrame)^.add_events(1 shl Ord(GDK_BUTTON_PRESS_MASK) or 1 shl Ord(GDK_BUTTON_RELEASE_MASK) or 1 shl Ord(GDK_POINTER_MOTION_MASK));

  FTitleBar := gtk_drawing_area_new;
  FTitleBar^.set_size_request(-1, FTitleBarH);
  FTitleBar^.add_events(1 shl Ord(GDK_BUTTON_PRESS_MASK) or 1 shl Ord(GDK_BUTTON_RELEASE_MASK) or 1 shl Ord(GDK_POINTER_MOTION_MASK));
  PGtkBox(FBox)^.pack_start(FTitleBar, False, False, 0);

  FClientHost := nil;

  g_signal_connect_data(PGObject(FFrame), 'draw',
    TGCallback(@TGtk3MDIChildFrame.FrameDraw), Self, nil, G_CONNECT_DEFAULT);
  g_signal_connect_data(PGObject(FTitleBar), 'draw',
    TGCallback(@TGtk3MDIChildFrame.TitleBarDraw), Self, nil, G_CONNECT_DEFAULT);
  g_signal_connect_data(PGObject(FTitleBar), 'button-press-event',
    TGCallback(@TGtk3MDIChildFrame.TitleBarButtonPress), Self, nil, G_CONNECT_DEFAULT);
  g_signal_connect_data(PGObject(FTitleBar), 'button-release-event',
    TGCallback(@TGtk3MDIChildFrame.TitleBarButtonRelease), Self, nil, G_CONNECT_DEFAULT);
  g_signal_connect_data(PGObject(FTitleBar), 'motion-notify-event',
    TGCallback(@TGtk3MDIChildFrame.TitleBarMotionNotify), Self, nil, G_CONNECT_DEFAULT);
  g_signal_connect_data(PGObject(FFrame), 'button-press-event',
    TGCallback(@TGtk3MDIChildFrame.FrameButtonPress), Self, nil, G_CONNECT_DEFAULT);
  g_signal_connect_data(PGObject(FFrame), 'button-release-event',
    TGCallback(@TGtk3MDIChildFrame.FrameButtonRelease), Self, nil, G_CONNECT_DEFAULT);
  g_signal_connect_data(PGObject(FFrame), 'motion-notify-event',
    TGCallback(@TGtk3MDIChildFrame.FrameMotionNotify), Self, nil, G_CONNECT_DEFAULT);

  g_signal_connect_data(PGObject(FFrame), 'set-focus-child',
    TGCallback(@TGtk3MDIChildFrame.FrameSetFocusChild), Self, nil, G_CONNECT_DEFAULT);

  FCaptureGesture := PGtkGesture(gtk_gesture_multi_press_new(FFrame));
  gtk_event_controller_set_propagation_phase(
    PGtkEventController(FCaptureGesture), GTK_PHASE_CAPTURE);
  g_signal_connect_data(PGObject(FCaptureGesture), 'pressed',
    TGCallback(@TGtk3MDIChildFrame.FrameCaptureGesturePressed), Self, nil, G_CONNECT_DEFAULT);
  g_signal_connect_data(PGObject(FFrame), 'destroy',
    TGCallback(@TGtk3MDIChildFrame.FrameGtkDestroy), Self, nil, G_CONNECT_DEFAULT);

  PGtkWidget(FFrame)^.show_all;
end;

procedure TGtk3MDIChildFrame.ComputeMetrics;
var
  Screen: PGdkScreen;
  Res: gdouble;
  PangoCtx: PPangoContext;
  FontDesc: PPangoFontDescription;
  Metrics: PPangoFontMetrics;
  Asc, Desc, FontH: gint;
begin
  Screen := gdk_screen_get_default;
  Res := 96.0;
  if Screen <> nil then
    Res := gdk_screen_get_resolution(Screen);
  if Res <= 0 then Res := 96.0;
  FScale := Res / 96.0;

  //Font height from gtk's default pango context
  FontH := 12;
  PangoCtx := PGtkWidget(FFrame)^.get_pango_context;
  if PangoCtx <> nil then
  begin
    FontDesc := PPangoFontDescription(pango_context_get_font_description(PangoCtx));
    Metrics := pango_context_get_metrics(PangoCtx, FontDesc, nil);
    Asc := pango_font_metrics_get_ascent(Metrics) div PANGO_SCALE;
    Desc := pango_font_metrics_get_descent(Metrics) div PANGO_SCALE;
    FontH := Asc + Desc;
    pango_font_metrics_unref(Metrics);
  end;

  FTitleBarH := FontH + Round(10 * FScale);
  if FTitleBarH < Round(24 * FScale) then
    FTitleBarH := Round(24 * FScale);

  FBtnW := FTitleBarH;
  FBorderW := Round(1.5 * FScale);
  if FBorderW < 1 then FBorderW := 1;

  FResizeBorder := Round(RESIZE_BORDER * FScale);

  if FResizeBorder < FBorderW then
    FResizeBorder := FBorderW;
end;

function TGtk3MDIChildFrame.ButtonAtX(AX: gint): Integer;
(*Returns logical action index for the click handler:
  0 = minimize / restore-from-min toggle
  1 = maximize / restore-from-max toggle
  2 = close and -1 = no button hit

  Slots are arranged left-to-right. Their content depends on FBorderStyle
  and FState. Layout (slot index -> action):
  bsDialog -> [close] = [2]
  bsSingle/bsToolWindow normal -> [min, close] = [0,2]
  bsSingle/bsToolWindow min'd -> [restore, close] = [0,2]
  bsSizeable/bsSizeToolWin nor -> [min, max/restore, close] = [0,1,2]
  bsSizeable/bsSizeToolWin min -> [restore, close] = [0,2]
  bsNone -> [] *)
var
  Alloc: TGtkAllocation;
  ButtonsBaseX: gint;
  Actions: array[0..2] of Integer;
  N, SlotIdx: Integer;
begin
  Result := -1;
  if FTitleBar = nil then
    exit;

  FTitleBar^.get_allocation(@Alloc);

  case FBorderStyle of
    bsNone:
      N := 0;
    bsDialog:
    begin
      N := 1;
      Actions[0] := 2;
    end;
    bsSingle, bsToolWindow:
    begin
      N := 2;
      Actions[0] := 0;
      Actions[1] := 2;
    end;
  else
    if FState = mwsMinimized then
    begin
      N := 2;
      Actions[0] := 0;
      Actions[1] := 2;
    end else
    begin
      N := 3;
      Actions[0] := 0;
      Actions[1] := 1;
      Actions[2] := 2;
    end;
  end;
  if N = 0 then
    exit;

  ButtonsBaseX := Alloc.width - N * FBtnW;

  if AX < ButtonsBaseX then
    exit;

  SlotIdx := (AX - ButtonsBaseX) div FBtnW;

  if SlotIdx >= N then
    exit;

  Result := Actions[SlotIdx];
end;

destructor TGtk3MDIChildFrame.Destroy;
begin
  if FCaptureGesture <> nil then
  begin
    g_object_unref(FCaptureGesture);
    FCaptureGesture := nil;
  end;

  if FSavedCursor <> nil then
  begin
    g_object_unref(FSavedCursor);
    FSavedCursor := nil;
  end;

  if FFrame <> nil then
    gtk_widget_destroy(FFrame);

  FFrame := nil;
  FBox := nil;
  FTitleBar := nil;
  FClientHost := nil;

  inherited Destroy;
end;

procedure TGtk3MDIChildFrame.SetCaption(const AValue: String);
begin
  if FCaption = AValue then
    exit;

  FCaption := AValue;
  if FTitleBar <> nil then
    gtk_widget_queue_draw(FTitleBar);
end;

procedure TGtk3MDIChildFrame.SetActive(AValue: Boolean);
begin
  if FActive = AValue then
    exit;
  FActive := AValue;
  if FTitleBar <> nil then
    gtk_widget_queue_draw(FTitleBar);
  if FFrame <> nil then
    gtk_widget_queue_draw(FFrame);
  if (FWorkspace <> nil) then
  begin
    if FActive and Assigned(FWorkspace.FOnChildActivated) then
      FWorkspace.FOnChildActivated(Self)
    else
    if (not FActive) and Assigned(FWorkspace.FOnChildDeactivated) then
      FWorkspace.FOnChildDeactivated(Self);
  end;
end;

procedure TGtk3MDIChildFrame.SetState(AValue: TGtk3MDIWindowState);
begin
  case AValue of
    mwsNormal: Restore;
    mwsMinimized: Minimize;
    mwsMaximized: Maximize;
  end;
end;

procedure TGtk3MDIChildFrame.SetBounds(L, T, W, H: Integer);
var
  PosChanged, SizeChanged: Boolean;
begin
  if FFrame = nil then
    exit;
  if FState = mwsMaximized then
    exit;
  PosChanged := (FCurX <> L) or (FCurY <> T);
  SizeChanged := (FCurW <> W) or (FCurH <> H);
  if not (PosChanged or SizeChanged) then
    exit;
  FCurX := L;
  FCurY := T;
  FCurW := W;
  FCurH := H;
  if PosChanged and (FWorkspace.Container <> nil) then
    gtk_layout_move(PGtkLayout(FWorkspace.Container), FFrame, L, T);

  if SizeChanged then
    PGtkWidget(FFrame)^.set_size_request(W, H);
end;

function TGtk3MDIChildFrame.GetBounds: TRect;
begin
  Result := Rect(FCurX, FCurY, FCurX + FCurW, FCurY + FCurH);
end;

procedure TGtk3MDIChildFrame.SetBorderStyle(AValue: TFormBorderStyle);
begin
  if FBorderStyle = AValue then
    exit;
  FBorderStyle := AValue;
  if FTitleBar <> nil then
    gtk_widget_queue_draw(FTitleBar);
end;

procedure TGtk3MDIChildFrame.AddClientHost(AContent: PGtkWidget);
begin
  FClientHost := AContent;
  if AContent = nil then
    exit;
  PGtkBox(FBox)^.pack_start(AContent, True, True, 0);
  PGtkWidget(AContent)^.show_all;
  //defer a redraw so TGraphicControls paint after GdkWindows are fully mapped
  g_idle_add(@TGtk3MDIChildFrame.DeferFirstPaint, Self);
end;

class function TGtk3MDIChildFrame.DeferFirstPaint(data: gpointer): gboolean; cdecl;
var
  AFrame: TGtk3MDIChildFrame absolute data;
  i: Integer;
  Sibling: TGtk3MDIChildFrame;
  WSWindow: PGdkWindow;
begin
  if (AFrame <> nil) and (AFrame.FFrame <> nil) then
  begin
    gtk_widget_queue_draw(AFrame.FFrame);
    if AFrame.FWorkspace <> nil then
    begin
      for i := 0 to AFrame.FWorkspace.FChildren.Count - 1 do
      begin
        Sibling := TGtk3MDIChildFrame(AFrame.FWorkspace.FChildren[i]);
        if (Sibling <> AFrame) and (Sibling.FFrame <> nil) then
          gtk_widget_queue_draw(Sibling.FFrame);
      end;
      if AFrame.FWorkspace.FContainer <> nil then
      begin
        WSWindow := gtk_widget_get_window(AFrame.FWorkspace.FContainer);
        if WSWindow <> nil then
          WSWindow^.process_updates(True);
      end;
    end;
  end;
  Result := false;
end;

procedure TGtk3MDIChildFrame.RefitMaximized(AViewportW, AViewportH: Integer);
begin
  if (FFrame = nil) or (FState <> mwsMaximized) then
    exit;
  if (AViewportW = FCurW) and (AViewportH = FCurH) and (FCurX = 0) and (FCurY = 0) then
      exit;
  FCurX := 0;
  FCurY := 0;
  FCurW := AViewportW;
  FCurH := AViewportH;
  if (FWorkspace <> nil) and (FWorkspace.Container <> nil) then
    gtk_layout_move(PGtkLayout(FWorkspace.Container), FFrame, 0, 0);
  PGtkWidget(FFrame)^.set_size_request(AViewportW, AViewportH);
end;

procedure TGtk3MDIChildFrame.BringToFront;
var
  Idx: Integer;
  Win: PGdkWindow;
begin
  if FFrame = nil then
    exit;

  if (FWorkspace <> nil) and (FWorkspace.Container <> nil) then
  begin
    Idx := FWorkspace.FChildren.IndexOf(Self);
    if (Idx >= 0) and (Idx = FWorkspace.FChildren.Count - 1) then
    begin
      FWorkspace.ActivateChild(Self);
      exit;
    end;
    Win := PGtkWidget(FFrame)^.get_window;
    if Win <> nil then
      gdk_window_raise(Win);
    if (Idx >= 0) and (Idx < FWorkspace.FChildren.Count - 1) then
    begin
      FWorkspace.FChildren.Delete(Idx);
      FWorkspace.FChildren.Add(Self);
    end;
  end;
  if FWorkspace <> nil then
    FWorkspace.ActivateChild(Self);
end;

procedure TGtk3MDIChildFrame.Minimize;
var
  Next: TGtk3MDIChildFrame;
begin
  if FState = mwsMinimized then
    exit;
  FStateBeforeMin := FState;
  if FState = mwsNormal then
    FBoundsBeforeMin := GetBounds;
  FState := mwsMinimized;
  if FWorkspace <> nil then
  begin
    FMinimizedSlot := FWorkspace.AcquireMinimizedSlot(Self);
    FWorkspace.LayoutMinimizedFrame(Self);
    if Assigned(FWorkspace.FOnChildStateChanged) then
      FWorkspace.FOnChildStateChanged(Self);
    if FWorkspace.FActiveChild = Self then
    begin
      Next := FWorkspace.FindNextActivatableChild(Self);
      FWorkspace.FActiveChild := nil;
      Self.Active := False;
      if Next <> nil then
        FWorkspace.ActivateChild(Next)
      else
      if Assigned(FWorkspace.FOnNoActiveChild) then
        FWorkspace.FOnNoActiveChild(FWorkspace);
    end;
  end;
  if FTitleBar <> nil then
    gtk_widget_queue_draw(FTitleBar);
end;

procedure TGtk3MDIChildFrame.Maximize;
var
  WS: TGtkAllocation;
begin
  if FState = mwsMaximized then
    exit;
  //From minimized, first leave that slot before resizing.
  if FState = mwsMinimized then
  begin
    if FWorkspace <> nil then
      FWorkspace.ReleaseMinimizedSlot(FMinimizedSlot);
    FMinimizedSlot := -1;
  end else
  if FState = mwsNormal then
    FBoundsBeforeMax := GetBounds;
  if (FWorkspace <> nil) and (FWorkspace.Widget <> nil) then
    PGtkWidget(FWorkspace.Widget)^.get_allocation(@WS)
  else
    FillChar(WS, SizeOf(WS), 0);
  SetBounds(0, 0, WS.width, WS.height);
  FState := mwsMaximized;
  if (FWorkspace <> nil) and Assigned(FWorkspace.FOnChildStateChanged) then
    FWorkspace.FOnChildStateChanged(Self);
  if FTitleBar <> nil then
    gtk_widget_queue_draw(FTitleBar);
end;

procedure TGtk3MDIChildFrame.Restore;
var
  WS: TGtkAllocation;
begin
  case FState of
    mwsNormal: Exit;
    mwsMinimized:
    begin
      if FWorkspace <> nil then
        FWorkspace.ReleaseMinimizedSlot(FMinimizedSlot);
      FMinimizedSlot := -1;
      //Return to wichever state we minimized FROM.
      if FStateBeforeMin = mwsMaximized then
      begin
        if FWorkspace <> nil then
          FWorkspace.Container^.get_allocation(@WS)
        else
          FillChar(WS, SizeOf(WS), 0);
        SetBounds(0, 0, WS.width, WS.height);
        FState := mwsMaximized;
      end else
      begin
        SetBounds(FBoundsBeforeMin.Left, FBoundsBeforeMin.Top,
          FBoundsBeforeMin.Right - FBoundsBeforeMin.Left,
          FBoundsBeforeMin.Bottom - FBoundsBeforeMin.Top);
        FState := mwsNormal;
      end;
    end;
    mwsMaximized:
    begin
      FState := mwsNormal;
      SetBounds(FBoundsBeforeMax.Left, FBoundsBeforeMax.Top,
        FBoundsBeforeMax.Right - FBoundsBeforeMax.Left,
        FBoundsBeforeMax.Bottom - FBoundsBeforeMax.Top);
    end;
  end;

  if (FWorkspace <> nil) and Assigned(FWorkspace.FOnChildStateChanged) then
    FWorkspace.FOnChildStateChanged(Self);

  if FTitleBar <> nil then
    gtk_widget_queue_draw(FTitleBar);

  BringToFront;

end;

{ TGtk3MDIWorkspace }

constructor TGtk3MDIWorkspace.Create;
begin
  inherited Create;

  FChildren := TFPList.Create;
  FMinimizedSlots := TFPList.Create;
  FActiveChild := nil;
  FNextChildX := 0;
  FNextChildY := 0;
  FOwnerForm := nil;

  FWidget := gtk_scrolled_window_new(nil, nil);
  PGtkScrolledWindow(FWidget)^.set_policy(GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  PGtkWidget(FWidget)^.set_can_focus(False);
  PGtkScrolledWindow(FWidget)^.get_vscrollbar^.set_can_focus(False);
  PGtkScrolledWindow(FWidget)^.get_hscrollbar^.set_can_focus(False);

  FContainer := gtk_layout_new(nil, nil);
  PGtkContainer(FWidget)^.add(FContainer);

  g_signal_connect_data(PGObject(FWidget), 'destroy',
    TGCallback(@TGtk3MDIWorkspace.WorkspaceGtkDestroy), Self, nil, G_CONNECT_DEFAULT);

  gtk_widget_set_app_paintable(FContainer, True);
  g_signal_connect_data(PGObject(FContainer), 'draw',
    TGCallback(@TGtk3MDIWorkspace.WorkspaceDraw), Self, nil, G_CONNECT_DEFAULT);

  PGtkWidget(FWidget)^.show_all;
end;

destructor TGtk3MDIWorkspace.Destroy;
var
  i: Integer;
begin
  for i := FChildren.Count - 1 downto 0 do
    TGtk3MDIChildFrame(FChildren[i]).Free;
  FChildren.Free;
  FMinimizedSlots.Free;

  if FWidget <> nil then
    gtk_widget_destroy(FWidget);

  FWidget := nil;
  FContainer := nil;
  inherited Destroy;
end;

function TGtk3MDIWorkspace.AcquireMinimizedSlot(
  AFrame: TGtk3MDIChildFrame): Integer;
var
  i: Integer;
begin
  //Reuse first empty slot, otherwise append.
  for i := 0 to FMinimizedSlots.Count - 1 do
    if FMinimizedSlots[i] = nil then
    begin
      FMinimizedSlots[i] := AFrame;
      exit(i);
    end;
  Result := FMinimizedSlots.Count;
  FMinimizedSlots.Add(AFrame);
end;

procedure TGtk3MDIWorkspace.ReleaseMinimizedSlot(ASlot: Integer);
begin
  if (ASlot >= 0) and (ASlot < FMinimizedSlots.Count) then
    FMinimizedSlots[ASlot] := nil;

  while (FMinimizedSlots.Count > 0)
    and (FMinimizedSlots[FMinimizedSlots.Count - 1] = nil) do
    FMinimizedSlots.Delete(FMinimizedSlots.Count - 1);
end;

procedure TGtk3MDIWorkspace.LayoutMinimizedFrame(AFrame: TGtk3MDIChildFrame);
var
  Alloc: TGtkAllocation;
  IconW, IconH: gint;
  Cols, Col, Row: gint;
  XPos, YPos: gint;
begin

  if (AFrame = nil) or (AFrame.FMinimizedSlot < 0) then
    exit;

  IconW := Round(MIN_ICON_W * AFrame.FScale);
  IconH := AFrame.FTitleBarH + 2 * AFrame.FBorderW;

  PGtkWidget(FWidget)^.get_allocation(@Alloc);

  if Alloc.width <= 0 then
    Alloc.width := IconW * 4;

  if Alloc.height <= 0 then
    Alloc.height := IconH * 4;

  Cols := Alloc.width div IconW;

  if Cols < 1 then
    Cols := 1;

  Col := AFrame.FMinimizedSlot mod Cols;
  Row := AFrame.FMinimizedSlot div Cols;

  XPos := Col * IconW;
  YPos := Alloc.height - (Row + 1) * IconH;
  if YPos < 0 then
    YPos := 0;
  AFrame.SetBounds(XPos, YPos, IconW, IconH);
end;

procedure TGtk3MDIWorkspace.RelayoutMinimized;
var
  i: Integer;
  C: TGtk3MDIChildFrame;
begin
  for i := 0 to FMinimizedSlots.Count - 1 do
  begin
    C := TGtk3MDIChildFrame(FMinimizedSlots[i]);
    if C <> nil then
      LayoutMinimizedFrame(C);
  end;
end;

function TGtk3MDIWorkspace.AddChild(const ACaption: String): TGtk3MDIChildFrame;
begin
  Result := TGtk3MDIChildFrame.Create(Self);
  Result.Caption := ACaption;
  gtk_layout_put(PGtkLayout(FContainer), Result.Frame, FNextChildX, FNextChildY);
  Result.SetBounds(FNextChildX, FNextChildY, DEFAULT_CHILD_W, DEFAULT_CHILD_H);
  Inc(FNextChildX, CASCADE_OFFSET);
  Inc(FNextChildY, CASCADE_OFFSET);
  //TODO: make count from area clientwidth/clientheight
  if (FNextChildX > 200) or (FNextChildY > 200) then
  begin
    FNextChildX := 0;
    FNextChildY := 0;
  end;
  FChildren.Add(Result);
  ActivateChild(Result);
end;

procedure TGtk3MDIWorkspace.RemoveChild(AFrame: TGtk3MDIChildFrame);
var
  Idx: Integer;
begin
  if AFrame = nil then
    exit;
  if AFrame.FMinimizedSlot >= 0 then
  begin
    ReleaseMinimizedSlot(AFrame.FMinimizedSlot);
    AFrame.FMinimizedSlot := -1;
  end;
  Idx := FChildren.IndexOf(AFrame);
  if Idx < 0 then
    exit;
  FChildren.Delete(Idx);
  if FActiveChild = AFrame then
  begin
    if FChildren.Count > 0 then
      ActivateChild(TGtk3MDIChildFrame(FChildren[FChildren.Count - 1]))
    else
      FActiveChild := nil;
  end;
  AFrame.Free;
end;

procedure TGtk3MDIWorkspace.ActivateChild(AFrame: TGtk3MDIChildFrame);
begin
  if FActiveChild = AFrame then
    exit;
  if FActiveChild <> nil then
    FActiveChild.Active := False;
  FActiveChild := AFrame;
  if FActiveChild <> nil then
    FActiveChild.Active := True;
end;

procedure TGtk3MDIWorkspace.SetActiveChildInternal(AValue: TGtk3MDIChildFrame);
begin
  ActivateChild(AValue);
end;

function TGtk3MDIWorkspace.FindNextActivatableChild(AExclude: TGtk3MDIChildFrame): TGtk3MDIChildFrame;
var
  i: Integer;
  F: TGtk3MDIChildFrame;
begin
  Result := nil;
  //Walk top-of-z-order first, last in list = topmost.
  for i := FChildren.Count - 1 downto 0 do
  begin
    F := TGtk3MDIChildFrame(FChildren[i]);
    if (F = AExclude) or (F.State = mwsMinimized) then
      continue;
    exit(F);
  end;
end;

function TGtk3MDIWorkspace.ChildAtPoint(X, Y: gint): TGtk3MDIChildFrame;
var
  i: Integer;
  C: TGtk3MDIChildFrame;
  R: TRect;
begin
  Result := nil;
  for i := FChildren.Count - 1 downto 0 do
  begin
    C := TGtk3MDIChildFrame(FChildren[i]);
    R := C.GetBounds;
    if (X >= R.Left) and (X < R.Right) and (Y >= R.Top) and (Y < R.Bottom) then
      exit(C);
  end;
end;

function LookupThemeColor(AWidget: PGtkWidget; const AName: PgChar; var R, G, B: Double): Boolean;
var
  Ctx: PGtkStyleContext;
  C: TGdkRGBA;
begin
  Result := False;
  if AWidget = nil then
    exit;
  Ctx := AWidget^.get_style_context;
  if Ctx = nil then
  begin
    //writeln('Ctx=nil, prop=',AName);
    exit;
  end;
  if gtk_style_context_lookup_color(Ctx, AName, @C) then
  begin
    R := C.red;
    G := C.green;
    B := C.blue;
    Result := True;
  end;
end;

{Paint a themed symbolic icon recolored with (R,G,B) inside the rect. Returns False if the icon could not be loaded.}
function PaintThemedIcon(cr: Pcairo_t; AX, AY, AW, AH: gint; const AIconName: PgChar; AR, AG, AB: Double): Boolean;
var
  Theme: PGtkIconTheme;
  Info: PGtkIconInfo;
  Pix: PGdkPixbuf;
  Fg: TGdkRGBA;
  WasSym: gboolean;
  Err: PGError;
  IconSize, PW, PH: gint;
begin
  Result := False;
  Theme := gtk_icon_theme_get_default;

  if Theme = nil then
    exit;

  IconSize := AH * 60 div 100;
  if IconSize < 8 then
    IconSize := 8;

  Info := gtk_icon_theme_lookup_icon(Theme, AIconName, IconSize, []);
  if Info = nil then
    exit;

  Fg.red := AR;
  Fg.green := AG;
  Fg.blue := AB;
  Fg.alpha := 1;

  Err := nil;
  Pix := gtk_icon_info_load_symbolic(Info, @Fg, nil, nil, nil, @WasSym, @Err);
  g_object_unref(Info);

  if Err <> nil then
    g_error_free(Err);

  if Pix = nil then
    exit;

  PW := gdk_pixbuf_get_width(Pix);
  PH := gdk_pixbuf_get_height(Pix);
  gdk_cairo_set_source_pixbuf(cr, Pix,AX + (AW - PW) / 2, AY + (AH - PH) / 2);
  cairo_paint(cr);
  g_object_unref(Pix);
  Result := True;
end;

{Paint a button glyph: cross/square/underscore/ restore inside ARect.}
procedure DrawButtonGlyph(cr: Pcairo_t; AX, AY, AW, AH: gint; AKind: TGtk3MDIButtonKind; AScale: Double; AR, AG, AB: Double);
var
  CX, CY: Double;
  GW, Pad, LW, Off: Double;
begin
  CX := AX + AW / 2;
  CY := AY + AH / 2;
  GW := AW * 0.4;

  if GW < 6 then
    GW := 6;
  Pad := GW / 2;

  LW := 1.4 * AScale;
  if LW < 1 then
    LW := 1;

  cairo_set_line_width(cr, LW);
  cairo_set_line_cap(cr, CAIRO_LINE_CAP_ROUND);
  cairo_set_source_rgb(cr, AR, AG, AB);

  case AKind of
    mbkMin:// short underscore at the bottom
    begin
      cairo_move_to(cr, CX - Pad, CY + Pad * 0.4);
      cairo_line_to(cr, CX + Pad, CY + Pad * 0.4);
      cairo_stroke(cr);
    end;
    mbkMax://square outline
    begin
      cairo_rectangle(cr, CX - Pad, CY - Pad, GW, GW);
      cairo_stroke(cr);
    end;
    mbkClose: // X
    begin
      cairo_move_to(cr, CX - Pad, CY - Pad);
      cairo_line_to(cr, CX + Pad, CY + Pad);
      cairo_move_to(cr, CX + Pad, CY - Pad);
      cairo_line_to(cr, CX - Pad, CY + Pad);
      cairo_stroke(cr);
    end;
    mbkRestore: //two overlapping squares
    begin
      Off := GW * 0.30;
      cairo_rectangle(cr, CX - Pad + Off, CY - Pad - Off / 2, GW - Off, GW - Off);
      cairo_stroke(cr);
      cairo_rectangle(cr, CX - Pad, CY - Pad + Off / 2, GW - Off, GW - Off);
      cairo_stroke(cr);
    end;
  end;
end;

{Paint a button icon. Tries the symbolic theme icon first, falls back to the cairo glyph when the icon is unavailable.}
procedure PaintButtonIcon(cr: Pcairo_t; AX, AY, AW, AH: gint;
  AKind: TGtk3MDIButtonKind; AScale: Double; AR, AG, AB: Double);
const
  IconNames: array[TGtk3MDIButtonKind] of PgChar = (
    'window-minimize-symbolic',
    'window-maximize-symbolic',
    'window-close-symbolic',
    'window-restore-symbolic'
  );
begin
  if not PaintThemedIcon(cr, AX, AY, AW, AH, IconNames[AKind], AR, AG, AB) then
    DrawButtonGlyph(cr, AX, AY, AW, AH, AKind, AScale, AR, AG, AB);
end;

class function TGtk3MDIChildFrame.FrameDraw(widget: PGtkWidget; cr: Pcairo_t; data: gpointer): gboolean; cdecl;
var
  AFrame: TGtk3MDIChildFrame absolute data;
  Alloc: TGtkAllocation;
  R, G, B: Double;
begin
  widget^.get_allocation(@Alloc);

  R := 0.94;
  G := 0.94;
  B := 0.94;

  LookupThemeColor(widget, 'theme_bg_color', R, G, B);
  cairo_set_source_rgb(cr, R, G, B);
  cairo_rectangle(cr, 0, 0, Alloc.width, Alloc.height);
  cairo_fill(cr);

  if AFrame.Active then
  begin
    R := 0.20;
    G := 0.40;
    B := 0.70;
    LookupThemeColor(widget, 'theme_selected_bg_color', R, G, B);
  end else
  begin
    R := 0.55;
    G := 0.55;
    B := 0.55;
    if not LookupThemeColor(widget, 'unfocused_borders', R, G, B) then
      LookupThemeColor(widget, 'borders', R, G, B);
  end;
  cairo_set_source_rgb(cr, R, G, B);
  cairo_set_line_width(cr, AFrame.FBorderW);
  cairo_rectangle(cr, AFrame.FBorderW / 2, AFrame.FBorderW / 2,
    Alloc.width - AFrame.FBorderW, Alloc.height - AFrame.FBorderW);
  cairo_stroke(cr);

  Result := False;
end;

class function TGtk3MDIChildFrame.TitleBarDraw(widget: PGtkWidget; cr: Pcairo_t; data: gpointer): gboolean; cdecl;
var
  AFrame: TGtk3MDIChildFrame absolute data;
  Alloc: TGtkAllocation;
  Layout: PPangoLayout;
  TextW, TextH, TextAvail: gint;
  ButtonsX, BtnX, i, BtnCount: gint;
  BgR, BgG, BgB, FgR, FgG, FgB: Double;
  Kinds: array[0..2] of TGtk3MDIButtonKind;
begin
  Result := True;
  widget^.get_allocation(@Alloc);
  //Match TGtk3MDIChildFrame.ButtonAtX layout. Count buttons by border
  //style and current state so the paint matches the hit-test exactly.
  case AFrame.FBorderStyle of
    bsNone: BtnCount := 0;
    bsDialog: BtnCount := 1;
    bsSingle, bsToolWindow: BtnCount := 2;
  else
    if AFrame.State = mwsMinimized then
      BtnCount := 2 //when minimized we show restore and close btns.
    else
      BtnCount := 3;
  end;
  ButtonsX := Alloc.width - BtnCount * AFrame.FBtnW;
  TextAvail := ButtonsX - Round(8 * AFrame.FScale);

  if AFrame.Active then
  begin

    BgR := 0.20;
    BgG := 0.40;
    BgB := 0.70;
    LookupThemeColor(widget, 'theme_selected_bg_color', BgR, BgG, BgB);

    FgR := 1;
    FgG := 1;
    FgB := 1;
    LookupThemeColor(widget, 'theme_selected_fg_color', FgR, FgG, FgB);

  end else
  begin

    BgR := 0.55;
    BgG := 0.55;
    BgB := 0.55;
    if not LookupThemeColor(widget, 'theme_unfocused_bg_color', BgR, BgG, BgB) then
      LookupThemeColor(widget, 'theme_bg_color', BgR, BgG, BgB);

    FgR := 0.30;
    FgG := 0.30;
    FgB := 0.30;
    if not LookupThemeColor(widget, 'theme_unfocused_fg_color', FgR, FgG, FgB) then
      LookupThemeColor(widget, 'theme_fg_color', FgR, FgG, FgB);

  end;

  cairo_set_source_rgb(cr, BgR, BgG, BgB);
  cairo_rectangle(cr, 0, 0, Alloc.width, Alloc.height);
  cairo_fill(cr);

  if (AFrame.Caption <> '') and (TextAvail > 0) then
  begin
    Layout := pango_cairo_create_layout(cr);
    Layout^.set_text(PgChar(AFrame.Caption), Length(AFrame.Caption));
    Layout^.set_width(TextAvail * PANGO_SCALE);
    Layout^.set_ellipsize(PANGO_ELLIPSIZE_END);
    Layout^.get_pixel_size(@TextW, @TextH);
    cairo_set_source_rgb(cr, FgR, FgG, FgB);
    cairo_move_to(cr, Round(8 * AFrame.FScale), (Alloc.height - TextH) / 2);
    pango_cairo_show_layout(cr, Layout);
    Layout^.unref;
  end;

  case AFrame.FBorderStyle of
    bsNone: ; // no buttons
    bsDialog:
      Kinds[0] := mbkClose;
    bsSingle, bsToolWindow:
    begin
      if AFrame.State = mwsMinimized then
        Kinds[0] := mbkRestore
      else
        Kinds[0] := mbkMin;
      Kinds[1] := mbkClose;
    end;
  else
    if AFrame.State = mwsMinimized then
    begin
      Kinds[0] := mbkRestore;
      Kinds[1] := mbkClose;
    end else
    begin
      Kinds[0] := mbkMin;
      if AFrame.State = mwsMaximized then
        Kinds[1] := mbkRestore
      else
        Kinds[1] := mbkMax;
      Kinds[2] := mbkClose;
    end;
  end;
  for i := 0 to BtnCount - 1 do
  begin
    BtnX := ButtonsX + i * AFrame.FBtnW;
    PaintButtonIcon(cr, BtnX, 0, AFrame.FBtnW, Alloc.height,
      Kinds[i], AFrame.FScale, FgR, FgG, FgB);
  end;
end;

class function TGtk3MDIChildFrame.TitleBarButtonPress(widget: PGtkWidget; event: PGdkEventButton; data: gpointer): gboolean; cdecl;
var
  AFrame: TGtk3MDIChildFrame absolute data;
  Alloc: TGtkAllocation;
  Btn: Integer;
begin
  Result := False;
  if event^.button <> 1 then
    exit;

  Btn := AFrame.ButtonAtX(Trunc(event^.x));
  if Btn >= 0 then
  begin
    AFrame.BringToFront;
    case Btn of
      0:
      begin
        if AFrame.State = mwsMinimized then
          AFrame.Restore
        else
          AFrame.Minimize;
      end;
      1:
      begin
        if AFrame.State = mwsMaximized then
          AFrame.Restore
        else
          AFrame.Maximize;
      end;
      2: g_idle_add(@TGtk3MDIChildFrame.DeferCloseChild, AFrame);
    end;

    exit(true);

  end;

  if event^.type_ = GDK_2BUTTON_PRESS then
  begin
    case AFrame.State of
      mwsMinimized: AFrame.Restore;
      mwsMaximized: AFrame.Restore;
    else
      AFrame.Maximize;
    end;
    exit(true);
  end;

  if AFrame.State = mwsMinimized then
  begin

    AFrame.BringToFront;

    exit(true);
  end;

  if AFrame.State = mwsMaximized then
  begin

    AFrame.BringToFront;

    exit(True);
  end;

  AFrame.FFrame^.get_allocation(@Alloc);
  AFrame.FFrameStartX := Alloc.x;
  AFrame.FFrameStartY := Alloc.y;
  AFrame.BringToFront;
  AFrame.FDragStartX := Trunc(event^.x_root);
  AFrame.FDragStartY := Trunc(event^.y_root);
  AFrame.FDragging := True;

  gtk_grab_add(widget);

  Result := True;
end;

class function TGtk3MDIChildFrame.DeferCloseChild(data: gpointer): gboolean; cdecl;
var
  AFrame: TGtk3MDIChildFrame absolute data;
  CanClose: Boolean;
begin
  Result := False;
  if (AFrame = nil) or (AFrame.FWorkspace = nil) then
    exit;

  CanClose := True;

  if Assigned(AFrame.FWorkspace.FOnChildCloseQuery) then
    CanClose := AFrame.FWorkspace.FOnChildCloseQuery(AFrame);

  if CanClose then
    AFrame.FWorkspace.RemoveChild(AFrame);
end;

class function TGtk3MDIChildFrame.TitleBarButtonRelease(widget: PGtkWidget; event: PGdkEventButton; data: gpointer): gboolean; cdecl;
var
  AFrame: TGtk3MDIChildFrame absolute data;
begin
  Result := False;
  if (event^.button = 1) and AFrame.FDragging then
  begin
    AFrame.FDragging := False;
    gtk_grab_remove(widget);
    Result := True;
  end;
end;

class function TGtk3MDIChildFrame.TitleBarMotionNotify(widget: PGtkWidget; event: PGdkEventMotion; data: gpointer): gboolean; cdecl;
var
  AFrame: TGtk3MDIChildFrame absolute data;
  NewX, NewY: gint;
begin
  Result := False;

  if not AFrame.FDragging then
    exit;

  NewX := AFrame.FFrameStartX + (Trunc(event^.x_root) - AFrame.FDragStartX);
  NewY := AFrame.FFrameStartY + (Trunc(event^.y_root) - AFrame.FDragStartY);

  if NewX < 0 then
    NewX := 0;
  if NewY < 0 then
    NewY := 0;

  if (AFrame.FCurX <> NewX) or (AFrame.FCurY <> NewY) then
  begin
    gtk_layout_move(PGtkLayout(AFrame.FWorkspace.Container), AFrame.FFrame, NewX, NewY);
    AFrame.FCurX := NewX;
    AFrame.FCurY := NewY;
  end;
  gdk_event_request_motions(event);
  Result := True;
end;

class function TGtk3MDIChildFrame.FrameButtonPress(widget: PGtkWidget; event: PGdkEventButton; data: gpointer): gboolean; cdecl;
var
  AFrame: TGtk3MDIChildFrame absolute data;
  Edge: Integer;
begin
  Result := False;
  if event^.button <> 1 then
    exit;

  if (AFrame.FFrame = nil)
    or (event^.window <> PGtkWidget(AFrame.FFrame)^.get_window) then
    exit;

  Edge := AFrame.EdgeAtPoint(Trunc(event^.x), Trunc(event^.y));
  if Edge <> 0 then
  begin
    AFrame.BringToFront;
    AFrame.FResizeEdges := Edge;
    AFrame.FResizeStartX := Trunc(event^.x_root);
    AFrame.FResizeStartY := Trunc(event^.y_root);
    AFrame.FResizeStartBounds := AFrame.GetBounds;
    AFrame.FResizing := True;
    gtk_grab_add(widget);
    Result := True;
  end;
end;

class procedure TGtk3MDIChildFrame.FrameSetFocusChild(container: PGtkContainer; widget: PGtkWidget; data: gpointer); cdecl;
var
  AFrame: TGtk3MDIChildFrame absolute data;
begin
  if (widget <> nil) and (AFrame <> nil) then
    AFrame.BringToFront;
end;

class procedure TGtk3MDIChildFrame.FrameCaptureGesturePressed(gesture: PGtkGestureMultiPress; n_press: gint; x, y: gdouble; data: gpointer); cdecl;
var
  AFrame: TGtk3MDIChildFrame absolute data;
begin
  if AFrame <> nil then
    AFrame.BringToFront;
end;

class procedure TGtk3MDIChildFrame.FrameGtkDestroy(widget: PGtkWidget; data: gpointer); cdecl;
var
  AFrame: TGtk3MDIChildFrame absolute data;
begin
  if AFrame <> nil then
  begin
    AFrame.FFrame := nil;
    AFrame.FBox := nil;
    AFrame.FTitleBar := nil;
    AFrame.FClientHost := nil;
  end;
end;

class function TGtk3MDIWorkspace.WorkspaceDraw(widget: PGtkWidget; cr: Pcairo_t; data: gpointer): gboolean; cdecl;
var
  WS: TGtk3MDIWorkspace absolute data;
  Alloc: TGtkAllocation;
  FormColor: TColor;
  R, G, B: Double;
  RGB: LongInt;
begin
  Result := False;
  if WS = nil then
    Exit;
  widget^.get_allocation(@Alloc);
  if (WS.FOwnerForm <> nil) and not (csDestroying in WS.FOwnerForm.ComponentState) then
    FormColor := WS.FOwnerForm.Color
  else
    FormColor := clDefault;
  if FormColor = clDefault then
  begin
    R := 0.55;
    G := 0.55;
    B := 0.55;
    if not LookupThemeColor(widget, 'theme_unfocused_bg_color', R, G, B) then
      LookupThemeColor(widget, 'theme_bg_color', R, G, B);
    R := R * 0.85;
    G := G * 0.85;
    B := B * 0.85;
  end else
  begin
    RGB := ColorToRGB(FormColor);
    R := (RGB and $FF) / 255.0;
    G := ((RGB shr 8) and $FF) / 255.0;
    B := ((RGB shr 16) and $FF) / 255.0;
  end;
  cairo_set_source_rgb(cr, R, G, B);
  cairo_rectangle(cr, 0, 0, Alloc.width, Alloc.height);
  cairo_fill(cr);
end;

class procedure TGtk3MDIWorkspace.WorkspaceGtkDestroy(widget: PGtkWidget; data: gpointer); cdecl;
var
  WS: TGtk3MDIWorkspace absolute data;
begin
  if WS <> nil then
  begin
    WS.FWidget := nil;
    WS.FContainer := nil;
    WS.FOwnerForm := nil;
  end;
end;

function TGtk3MDIChildFrame.EdgeAtPoint(AX, AY: gint): Integer;
var
  Alloc: TGtkAllocation;
  HZ: gint;
begin
  Result := 0;
  if FFrame = nil then
    exit;

  if FState <> mwsNormal then
    exit;

  PGtkWidget(FFrame)^.get_allocation(@Alloc);
  HZ := FResizeBorder;
  if AY < HZ then
    Result := Result or EDGE_N;

  if AY >= Alloc.height - HZ then
    Result := Result or EDGE_S;

  if AX < HZ then
    Result := Result or EDGE_W;

  if AX >= Alloc.width - HZ then
    Result := Result or EDGE_E;
end;

class function TGtk3MDIChildFrame.CursorNameForEdge(AEdge: Integer): PgChar;
begin
  case AEdge of
    EDGE_N: Result := 'n-resize';
    EDGE_S: Result := 's-resize';
    EDGE_W: Result := 'w-resize';
    EDGE_E: Result := 'e-resize';
    EDGE_N or EDGE_W: Result := 'nw-resize';
    EDGE_N or EDGE_E: Result := 'ne-resize';
    EDGE_S or EDGE_W: Result := 'sw-resize';
    EDGE_S or EDGE_E: Result := 'se-resize';
  else
    Result := 'default';
  end;
end;

procedure TGtk3MDIChildFrame.SetFrameCursor(const AName: PgChar);
var
  Win: PGdkWindow;
  Display: PGdkDisplay;
  Cursor: PGdkCursor;
begin
  if FFrame = nil then
    exit;
  Win := PGtkWidget(FFrame)^.get_window;
  if Win = nil then
    exit;
  Display := gdk_display_get_default;
  if AName = 'default' then
    Cursor := nil
  else
    Cursor := gdk_cursor_new_from_name(Display, AName);
  gdk_window_set_cursor(Win, Cursor);
  if Cursor <> nil then
    g_object_unref(Cursor);
end;

procedure TGtk3MDIChildFrame.ApplyFrameResize(AMouseRootX, AMouseRootY: gint);
var
  DX, DY: gint;
  NewL, NewT, NewR, NewB: gint;
  MinH: gint;
begin
  if (FFrame = nil) or (FWorkspace = nil) or (FWorkspace.Container = nil) then
    exit;

  DX := AMouseRootX - FResizeStartX;
  DY := AMouseRootY - FResizeStartY;

  NewL := FResizeStartBounds.Left;
  NewT := FResizeStartBounds.Top;
  NewR := FResizeStartBounds.Right;
  NewB := FResizeStartBounds.Bottom;

  if (FResizeEdges and EDGE_W) <> 0 then
    NewL := NewL + DX;

  if (FResizeEdges and EDGE_E) <> 0 then
    NewR := NewR + DX;

  if (FResizeEdges and EDGE_N) <> 0 then
    NewT := NewT + DY;

  if (FResizeEdges and EDGE_S) <> 0 then
    NewB := NewB + DY;

  MinH := FTitleBarH + 2 * FResizeBorder;

  if NewR - NewL < MIN_CHILD_W then
  begin
    if (FResizeEdges and EDGE_W) <> 0 then
      NewL := NewR - MIN_CHILD_W
    else
      NewR := NewL + MIN_CHILD_W;
  end;
  if NewB - NewT < MinH then
  begin
    if (FResizeEdges and EDGE_N) <> 0 then
      NewT := NewB - MinH
    else
      NewB := NewT + MinH;
  end;
  if NewL < 0 then
    NewL := 0;
  if NewT < 0 then
    NewT := 0;

  SetBounds(NewL, NewT, NewR - NewL, NewB - NewT);
end;

class function TGtk3MDIChildFrame.FrameButtonRelease(widget: PGtkWidget; event: PGdkEventButton; data: gpointer): gboolean; cdecl;
var
  AFrame: TGtk3MDIChildFrame absolute data;
begin
  Result := False;
  if (event^.button = 1) and AFrame.FResizing then
  begin
    AFrame.FResizing := False;
    AFrame.FResizeEdges := 0;
    gtk_grab_remove(widget);
    Result := True;
  end;
end;

class function TGtk3MDIChildFrame.FrameMotionNotify(widget: PGtkWidget; event: PGdkEventMotion; data: gpointer): gboolean; cdecl;
var
  AFrame: TGtk3MDIChildFrame absolute data;
  Edge: Integer;
  FX, FY: gint;
begin
  Result := False;
  if AFrame.FResizing then
  begin
    AFrame.ApplyFrameResize(Trunc(event^.x_root), Trunc(event^.y_root));
    gdk_event_request_motions(event);
    exit(True);
  end;

  if (AFrame.FFrame = nil) or (event^.window <> PGtkWidget(AFrame.FFrame)^.get_window) then
    exit;

  FX := Trunc(event^.x);
  FY := Trunc(event^.y);
  Edge := AFrame.EdgeAtPoint(FX, FY);
  if Edge <> 0 then
  begin
    if not AFrame.FCursorOnEdge then
    begin
      AFrame.FSavedCursor := gdk_window_get_cursor(event^.window);
      if AFrame.FSavedCursor <> nil then
        g_object_ref(AFrame.FSavedCursor);
      AFrame.FSavedCursorValid := True;
      AFrame.FCursorOnEdge := True;
    end;
    AFrame.SetFrameCursor(TGtk3MDIChildFrame.CursorNameForEdge(Edge));
  end else
  begin
    if AFrame.FCursorOnEdge then
    begin
      gdk_window_set_cursor(event^.window, AFrame.FSavedCursor);
      if AFrame.FSavedCursor <> nil then
      begin
        g_object_unref(AFrame.FSavedCursor);
        AFrame.FSavedCursor := nil;
      end;
      AFrame.FSavedCursorValid := False;
      AFrame.FCursorOnEdge := False;
    end;
  end;
end;

end.

