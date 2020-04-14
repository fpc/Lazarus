unit qtx11dummywidget;

{$mode objfpc}{$H+}

{we use this unit only under x11 to get accurate frame size in our aps}

interface

uses
  Classes, SysUtils, Types, qtobjects, qt5;

type

  { TDummyWidget }

  TDummyWidget = class(TQtObject)
  private
    FFrameRect: TRect;
    FFirstPaintEvent: boolean;
    function GetWidget: QWidgetH;
    procedure SetWidget(AValue: QWidgetH);
  public
    constructor Create; override; overload;
    function GetWidgetFrame: TRect;
    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl; override;
    function ShowDummyWidget(const ALeft, ATop, AWidth,
      AHeight: integer): boolean;
    procedure SendToBack;
    procedure HideWidget;
    property Widget: QWidgetH read GetWidget write SetWidget;
  end;

implementation
{.$DEFINE DEBUGQTFRAMESIZE}
uses {$IFDEF DEBUGQTFRAMESIZE}LCLProc,{$ENDIF} qtint;

{ TDummyWidget }

function TDummyWidget.GetWidget: QWidgetH;
begin
  Result := QWidgetH(TheObject);
end;

procedure TDummyWidget.SetWidget(AValue: QWidgetH);
begin
  TheObject := AValue;
end;

function TDummyWidget.ShowDummyWidget(const ALeft, ATop, AWidth,
  AHeight: integer): boolean;
var
  R: TRect;
  {$IFDEF DEBUGQTFRAMESIZE}
  ATicks: QWord;
  {$ENDIF}
  ALoop: integer;
  AMaxLoops: integer;
begin
  Result := Assigned(Widget);
  if Result then
  begin
    if not IsWayland and QX11Info_isCompositingManagerRunning then
      {it is possible that we need 50-100 msec to get frame when running under compositing manager.
       Measured composition managers: kwin 89 msec}
      AMaxLoops := 200000
    else
      AMaxLoops := 20000;
    {$IFDEF DEBUGQTFRAMESIZE}
    writeln('ShowDummyWidget(start) WindowManager="',GetWindowManager,'" Compositing enabled="',QX11Info_isCompositingManagerRunning,'" IsWayland="',IsWayland,'" MaxLoops=',AMaxLoops);
    ATicks := GetTickCount64;
    {$ENDIF}
    if (ALeft <= 0) or (ATop <= 0) or (AWidth <= 0) or (AHeight <= 0) then
    begin
      //move off visible screen, some wm's does not allow such construct.
      QScreen_geometry(QGuiApplication_primaryScreen(), @R);
      QWidget_move(Widget, R.CenterPoint.x, R.CenterPoint.y);
      //set some reasonable size
      QWidget_resize(Widget, 75, 32);
    end else
    begin
      QWidget_move(Widget, ALeft + 1, ATop + 1);
      QWidget_resize(Widget, AWidth - 1, AHeight - 1);
    end;
    QWidget_setAttribute(Widget, QtWA_X11DoNotAcceptFocus, True);
    QWidget_show(Widget);

    {We are waiting until dummy window is laid out on screen by window manager
     ALoop variable is needed to avoid infinite loop.
     Usually we get result in about 20-30msec on modern X11 without compositing,
     but 30-100 msec on wm with compositing enabled.
     Older X11 or slower machine might need more loops to get result,
     but it won't be over 200 msec in any case.}

    ALoop := 0; // avoid infinite loop
    while not FFirstPaintEvent do
    begin
      inc(ALoop);
      QCoreApplication_processEvents();
      if ALoop > AMaxLoops then
        break;
    end;
    {$IFDEF DEBUGQTFRAMESIZE}
    writeln('ShowDummyWidget: 1st LOOP=',ALoop);
    {$ENDIF}
    R := Rect(0 ,0, 0, 0);
    ALoop := 0; // avoid infinite loop
    //Qt4 sets QtWA_Mapped before first paint event and that's wrong since x11 did not update decoration yet.
    //so we MUST wait infinite for the first paint event.
    while (R.Top <= 0) do // qt4 sets QtWA_Mapped before first paint event :(, maybe we should add eventFilter to this widget and wait for paint event
    begin
      inc(ALoop);
      R := GetWidgetFrame;
      QCoreApplication_processEvents();
      if ALoop > AMaxLoops then
        break;
    end;

    {$IFDEF DEBUGQTFRAMESIZE}
    writeln('ShowDummyWidget: 2nd LOOP=',ALoop,' LAST R=',dbgs(R));
    writeln('ShowDummyWidget: *finished* FRAME=',dbgs(GetWidgetFrame),' in ',GetTickCount64 - ATicks,' msec ');
    {$ENDIF}
  end;
end;

constructor TDummyWidget.Create;
begin
  inherited Create;
  FFrameRect := Rect(0, 0, 0, 0);
  Widget := QWidget_create(nil, QtWindow);
  QWidget_setAttribute(Widget, QtWA_ShowWithoutActivating);
  QWidget_setFocusPolicy(Widget, QtNoFocus);
  AttachEvents;
end;

function TDummyWidget.GetWidgetFrame: TRect;
var
  AFrame, AGeometry: TRect;
begin
  Result := FFrameRect;
  if not Assigned(Widget) then
    exit;
  QWidget_frameGeometry(Widget, @AFrame);
  QWidget_geometry(Widget, @AGeometry);
  FFrameRect := Rect(AGeometry.Left - AFrame.Left, AGeometry.Top - AFrame.Top,
    AFrame.Right - AGeometry.Right, AFrame.Bottom - AGeometry.Bottom);
  Result := FFrameRect;
end;

function TDummyWidget.EventFilter(Sender: QObjectH; Event: QEventH): Boolean;
  cdecl;
begin
  Result := False;
  if not FFirstPaintEvent and (QEvent_type(Event) = QEventPaint) then
    FFirstPaintEvent := True;
end;

procedure TDummyWidget.SendToBack;
begin
  if Assigned(Widget) then
    QWidget_lower(Widget);
end;

procedure TDummyWidget.HideWidget;
begin
  if Assigned(Widget) then
    QWidget_hide(Widget);
end;

end.
