{
 *****************************************************************************
 *                            qtsystemtrayicon.pas                           *
 *                            --------------------                           *
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit qtsystemtrayicon;
{$mode objfpc}{$H+}

interface
{$i qtdefines.inc}

uses
  Classes, types, Controls, ExtCtrls, Graphics, Forms, LCLType, LCLProc, LazUTF8,
  qtobjects, qt5, qtproc, qtint;

type
  TSysTrayIconPaintData = record
    PaintWidget: QWidgetH;
    ClipRect: types.PRect;
    ClipRegion: QRegionH;
    Context: TQtDeviceContext;
  end;

  { TQtSystemTrayIcon }

  TQtSystemTrayIcon = class(TQtObject)
  private
    FSysTrayHook: QObject_hookH;
    FHook: QSystemTrayIcon_hookH;
    FSysTrayWidget: QWidgetH;
    FCanvas: TCanvas;
    function BeginPaintInternal(var APaintData: TSysTrayIconPaintData): hdc;
    procedure EndPaintInternal(var APaintData: TSysTrayIconPaintData);
  public
    FTrayIcon: TCustomTrayIcon;
  public
    constructor Create(vIcon: QIconH); virtual;
    destructor Destroy; override;
  public
    procedure AttachEvents; override;
    procedure DetachEvents; override;
    procedure AttachSysTrayWidget(AWidget: QWidgetH);
    procedure DetachSysTrayWidget;
    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl; override;
    procedure setContextMenu(menu: QMenuH);
    procedure setIcon(icon: QIconH);
    procedure setToolTip(tip: WideString);
    procedure signalActivated(AReason: QSystemTrayIconActivationReason); cdecl;
    procedure showBaloonHint(const ATitle, AHint: String;
      const AFlag: QSystemTrayIconMessageIcon; const ATimeOut: Integer);
    function GetGeometry: TRect;
    function GetPosition: TPoint;
    procedure Show;
    procedure Hide;
    procedure UpdateSystemTrayWidget;
    property Canvas: TCanvas read FCanvas write FCanvas;
    property SysTrayWidget: QWidgetH read FSysTrayWidget;
  end;

implementation

{ TQtSystemTrayIcon }

constructor TQtSystemTrayIcon.Create(vIcon: QIconH);
var
  AName: WideString; {just to debug}
begin
  inherited Create;
  FSysTrayWidget := nil;
  FSysTrayHook := nil;
  if vIcon <> nil then
    TheObject := QSystemTrayIcon_create(vicon, nil)
  else
    TheObject := QSystemTrayIcon_create();
  AName := 'LCL_QSystemTrayIcon';
  QObject_setObjectName(TheObject, @AName);
  FCanvas := nil;
  QtWidgetSet.RegisterSysTrayIcon(Self);
  AttachEvents;
end;

destructor TQtSystemTrayIcon.Destroy;
begin
  QtWidgetSet.UnRegisterSysTrayIcon(Self);
  inherited Destroy;
end;

procedure TQtSystemTrayIcon.AttachEvents;
begin
  inherited AttachEvents;
  FHook := QSystemTrayIcon_hook_create(QSystemTrayIconH(TheObject));
  QSystemTrayIcon_hook_hook_activated(FHook, @signalActivated);
end;

procedure TQtSystemTrayIcon.DetachEvents;
begin
  DetachSysTrayWidget;
  if Assigned(FHook) then
  begin
    QSystemTrayIcon_hook_destroy(FHook);
    FHook := nil;
  end;
  inherited DetachEvents;
end;

procedure TQtSystemTrayIcon.AttachSysTrayWidget(AWidget: QWidgetH);
begin
  if (AWidget = nil) and (FSysTrayWidget <> nil) then
    DetachSysTrayWidget;
  FSysTrayWidget := AWidget;
  if FSysTrayWidget <> nil then
  begin
    FSysTrayHook := QObject_hook_create(FSysTrayWidget);
    QObject_hook_hook_events(FSysTrayHook, @EventFilter);
  end;
end;

procedure TQtSystemTrayIcon.DetachSysTrayWidget;
begin
  if FSysTrayWidget <> nil then
  begin
    if FSysTrayHook <> nil then
      QObject_hook_destroy(FSysTrayHook);
    FSysTrayHook := nil;
    FSysTrayWidget := niL;
  end;
end;

function TQtSystemTrayIcon.BeginPaintInternal(var APaintData: TSysTrayIconPaintData): hdc;
var
  DC: TQtDeviceContext;
begin
  DC := TQtDeviceContext.Create(FSysTrayWidget, True);
  Result := HDC(DC);
  if Result <> 0 then
  begin
    QPainter_setLayoutDirection(DC.Widget, QtLayoutDirectionAuto);
    if APaintData.ClipRegion <> nil then
    begin
      DC.setClipRegion(APaintData.ClipRegion);
      DC.setClipping(True);
    end;
    if APaintData.ClipRect <> nil then
    begin
      New(DC.vClipRect);
      DC.vClipRect^ := APaintData.ClipRect^;
    end;
    APaintData.Context := DC;
  end;
end;

procedure TQtSystemTrayIcon.EndPaintInternal(var APaintData: TSysTrayIconPaintData);
begin
  if APaintData.ClipRect <> nil then
    Dispose(APaintData.ClipRect);
  if APaintData.Context <> nil then
    APaintData.Context.Free;
  APaintData.Context := nil;
end;

function TQtSystemTrayIcon.EventFilter(Sender: QObjectH; Event: QEventH
  ): Boolean; cdecl;
var
  X, Y: Integer;
  R: TRect;
  P, APos: TQtPoint;
  AHint: WideString;
  {$IFDEF HASX11}
  PaintData: TSysTrayIconPaintData;
  {$ENDIF}
begin
  Result := False;
  if Sender <> FSysTrayWidget then
    exit;

  case QEvent_type(Event) of
    QEventPaint:
    begin
      if Assigned(FTrayIcon.OnPaint) then
      begin
        // qt kernel sets QtWA_PaintOnScreen and QtWA_NoSystemBackground
        // also OnPaint won't fire until we enter widget with mouse.
        // Thats so now until I find out howto find systrayicon private QWidget
        // without searching in QtWidgetSet.EventFilter.
        {$IFDEF HASX11}
        QObject_event(QWidgetH(Sender), Event);
        FillChar(PaintData{%H-}, SizeOf(PaintData), 0);
        with PaintData do
        begin
          PaintWidget := FSysTrayWidget;
          ClipRegion := QPaintEvent_Region(QPaintEventH(Event));
          if ClipRect = nil then
            New(ClipRect);
          QPaintEvent_Rect(QPaintEventH(Event), ClipRect);
        end;
        FCanvas := TCanvas.Create;
        try
          FCanvas.Handle := BeginPaintInternal(PaintData);
          if Assigned(FTrayIcon.OnPaint) then
            FTrayIcon.OnPaint(FTrayIcon);
          EndPaintInternal(PaintData);
        finally
          FreeThenNil(FCanvas);
        end;
        Result := True;
        {$ELSE}
        DebugLn('TQtSystemTrayIcon: Paint event is not supported.');
        {$ENDIF}
      end;
    end;
    QEventToolTip:
    begin
      if Assigned(FTrayIcon) and (FTrayIcon.Hint <> '') then
      begin
        R := GetGeometry;
        QtPoint(R.Left, R.Top);
        AHint := UTF8ToUTF16(FTrayIcon.Hint);
        QToolTip_showText(@P, @AHint);
      end;
    end;
    QEventMouseMove:
    begin
      if Assigned(FTrayIcon) and Assigned(FTrayIcon.OnMouseMove) then
      begin
        QMouseEvent_pos(QMouseEventH(Event), @APos);
        X := APos.X;
        // Y := QMouseEvent_pos(QMouseEventH(Event),);
        Y := APos.Y;
        FTrayIcon.OnMouseMove(FTrayIcon, [], X, Y);
        if Assigned(FTrayIcon.OnPaint) and (FSysTrayWidget <> nil) then
          QWidget_update(FSysTrayWidget);
      end;
    end;
  end;
end;

procedure TQtSystemTrayIcon.setContextMenu(menu: QMenuH);
begin
  QSystemTrayIcon_setContextMenu(QSystemTrayIconH(TheObject), menu);
end;

procedure TQtSystemTrayIcon.setIcon(icon: QIconH);
begin
  QSystemTrayIcon_setIcon(QSystemTrayIconH(TheObject), icon);
end;

procedure TQtSystemTrayIcon.setToolTip(tip: WideString);
begin
  QSystemTrayIcon_setToolTip(QSystemTrayIconH(TheObject), @tip)
end;

procedure TQtSystemTrayIcon.signalActivated(
  AReason: QSystemTrayIconActivationReason); cdecl;
var
  MousePos: TQtPoint;
begin
  if not Assigned(FTrayIcon) then
    exit;
  QCursor_pos(@MousePos);

  if Assigned(FTrayIcon.OnPaint) and (FSysTrayWidget <> nil) then
    QWidget_update(FSysTrayWidget); // trigger paint event.

  case AReason of
    QSystemTrayIconTrigger:
      begin
        if Assigned(FTrayIcon.OnMouseDown) then
          FTrayIcon.OnMouseDown(FTrayIcon, mbLeft, [], MousePos.x, MousePos.y);
        if Assigned(FTrayIcon.OnClick) then
          FTrayIcon.OnClick(FTrayIcon);
        if Assigned(FTrayIcon.OnMouseUp) then
          FTrayIcon.OnMouseUp(FTrayIcon, mbLeft, [], MousePos.x, MousePos.y);
      end;
    QSystemTrayIconDoubleClick:
      begin
        if Assigned(FTrayIcon.OnMouseDown) then
          FTrayIcon.OnMouseDown(FTrayIcon, mbLeft, [], MousePos.x, MousePos.y);

        if Assigned(FTrayIcon.OnDblClick) then
          FTrayIcon.OnDblClick(FTrayIcon);

        if Assigned(FTrayIcon.OnMouseUp) then
          FTrayIcon.OnMouseUp(FTrayIcon, mbLeft, [], MousePos.x, MousePos.y);
      end;
    QSystemTrayIconMiddleClick:
      begin
        if Assigned(FTrayIcon.OnMouseDown) then
          FTrayIcon.OnMouseDown(FTrayIcon, mbMiddle, [], MousePos.x, MousePos.y);
        if Assigned(FTrayIcon.OnMouseUp) then
          FTrayIcon.OnMouseUp(FTrayIcon, mbMiddle, [], MousePos.x, MousePos.y);
      end;
    QSystemTrayIconContext:
      begin
        if Assigned(FTrayIcon.OnMouseDown) then
          FTrayIcon.OnMouseDown(FTrayIcon, mbRight, [], MousePos.x, MousePos.y);

        if Assigned(FTrayIcon.OnMouseUp) then
          FTrayIcon.OnMouseUp(FTrayIcon, mbRight, [], MousePos.x, MousePos.y);
      end;
  end;
end;

procedure TQtSystemTrayIcon.showBaloonHint(const ATitle, AHint: String;
  const AFlag: QSystemTrayIconMessageIcon; const ATimeOut: Integer);
var
  WHint: WideString;
  WTitle: WideString;
begin
  WHint := GetUTF8String(AHint);
  WTitle := GetUTF8String(ATitle);
  QSystemTrayIcon_showMessage(QSystemTrayIconH(TheObject), @WTitle, @WHint, AFlag, ATimeOut);
end;

function TQtSystemTrayIcon.GetGeometry: TRect;
begin
  Result := Classes.Rect(0, 0, 0, 0);
  if Assigned(TheObject) then
    QSystemTrayIcon_geometry(QSystemTrayIconH(TheObject), @Result);
end;

function TQtSystemTrayIcon.GetPosition: TPoint;
var
  R: TRect;
begin
  R := GetGeometry;
  Result := Point(R.Left, R.Top);
end;

procedure TQtSystemTrayIcon.Show;
begin
  QSystemTrayIcon_show(QSystemTrayIconH(TheObject));
end;

procedure TQtSystemTrayIcon.Hide;
begin
  QSystemTrayIcon_hide(QSystemTrayIconH(TheObject));
end;

procedure TQtSystemTrayIcon.UpdateSystemTrayWidget;
begin
  if Assigned(FSysTrayWidget) then
    QWidget_update(FSysTrayWidget);
end;

end.
