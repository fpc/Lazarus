{%MainUnit qtint.pp}
{
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
//---------------------------------------------------------------

{------------------------------------------------------------------------------
  Method: TQtWidgetSet.Create
  Params:  None
  Returns: Nothing

  Contructor for the class.
 ------------------------------------------------------------------------------}
constructor TQtWidgetSet.Create;
begin
  inherited Create;

  InitStockItems;
  QtWidgetSet := Self;
  FDockImage := nil;
end;

{------------------------------------------------------------------------------
  Method: TQtWidgetSet.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TQtWidgetSet.Destroy;
begin
  if FDockImage <> nil then
    QRubberBand_destroy(FDockImage);
  DestroyGlobalCaret;
  Clipboard.Free;
  FreeStockItems;
  QtDefaultPrinter.Free;
  QtWidgetSet := nil;
  
  if SavedDCList<>nil then
    SavedDCList.Free;
    
  QtDefaultContext.Free;
  QtScreenContext.Free;

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method: TQtWidgetSet.Destroy
  Params:  None
  Returns: Nothing

  Creates a new timer and sets the callback event.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.CreateTimer(Interval: integer; TimerFunc: TFNTimerProc): THandle;
var
  QtTimer: TQtTimer;
begin
  QtTimer := TQtTimer.CreateTimer(Interval, TimerFunc, App);
  
  Result := PtrInt(QtTimer);
end;

{------------------------------------------------------------------------------
  Method: TQtWidgetSet.Destroy
  Params:  None
  Returns: Nothing

  Destroys a timer.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.DestroyTimer(TimerHandle: THandle): boolean;
begin
  TQtTimer(TimerHandle).Free;
  
  Result := True;
end;

{------------------------------------------------------------------------------
  Method: TQtWidgetSet.AppInit
  Params:  None
  Returns: Nothing

  Initializes the application
 ------------------------------------------------------------------------------}
procedure TQtWidgetSet.AppInit(var ScreenInfo: TScreenInfo);
var
  Method: TMethod;
  FHook: QObject_hookH;
  ScreenDC: HDC;
begin
  App := QApplication_Create(@argc, argv);
  WakeMainThread := @OnWakeMainThread;

  FOldFocusWidget := nil;
  
  {
    check whether this hook crashes on linux & darwin and why it is so
    we need this hook to catch release messages
  }
  // install global event filter
  FHook := QObject_hook_create(App);
  TEventFilterMethod(Method) := @EventFilter;
  QObject_hook_hook_events(FHook, Method);
  
  // install focus change slot

  ScreenDC := GetDC(0);
  try
    ScreenInfo.PixelsPerInchX := GetDeviceCaps(ScreenDC, LOGPIXELSX);
    ScreenInfo.PixelsPerInchY := GetDeviceCaps(ScreenDC, LOGPIXELSY);
    ScreenInfo.ColorDepth := GetDeviceCaps(ScreenDC, BITSPIXEL);
  finally
    ReleaseDC(0, ScreenDC);
  end;
  
  QtDefaultPrinter;
  
end;

{------------------------------------------------------------------------------
  Method: TQtWidgetSet.AppRun
  Params:  None
  Returns: Nothing

  Enter the main message loop
 ------------------------------------------------------------------------------}
procedure TQtWidgetSet.AppRun(const ALoop: TApplicationMainLoop);
begin
  // use LCL loop
  if Assigned(ALoop) then
    ALoop;
end;

{------------------------------------------------------------------------------
  Method: TQtWidgetSet.AppWaitMessage
  Params:  None
  Returns: Nothing

  Waits until a message arrives, processes that and returns control out of the function
  
  Utilized on Modal dialogs
 ------------------------------------------------------------------------------}
procedure TQtWidgetSet.AppWaitMessage;
begin
  {we cannot call directly processEvents() with this flag
   since it produces AV's sometimes, so better check is there
   any pending event.}
  if not QCoreApplication_hasPendingEvents then
    QCoreApplication_processEvents(QEventLoopWaitForMoreEvents or QEventLoopDeferredDeletion);
end;

{------------------------------------------------------------------------------
  Method: TQtWidgetSet.AppProcessMessages
  Params:  None
  Returns: Nothing

  Processes all messages on the quoue
 ------------------------------------------------------------------------------}
procedure TQtWidgetSet.AppProcessMessages;
begin
  if QCoreApplication_hasPendingEvents then
    QCoreApplication_processEvents(QEventLoopAllEvents or QEventLoopDeferredDeletion);
end;

{------------------------------------------------------------------------------
  Method: TQtWidgetSet.AppTerminate
  Params:  None
  Returns: Nothing

  Implements Application.Terminate and MainForm.Close.
 ------------------------------------------------------------------------------}
procedure TQtWidgetSet.AppTerminate;
begin
  AppProcessMessages; // process pending messages since there can be release messages
  QCoreApplication_quit;
end;

procedure TQtWidgetSet.AppMinimize;
begin
  if (Application.MainForm <> nil) and (Application.MainForm.HandleAllocated) then
    TQtMainWindow(Application.MainForm.Handle).ShowMinimized;
end;

procedure TQtWidgetSet.AppRestore;
begin
  if (Application.MainForm <> nil) and (Application.MainForm.HandleAllocated) then
    TQtMainWindow(Application.MainForm.Handle).ShowNormal;
end;

procedure TQtWidgetSet.AppBringToFront;
begin
  if (Application.MainForm <> nil) and (Application.MainForm.HandleAllocated) then
    TQtMainWindow(Application.MainForm.Handle).BringToFront;
end;

{procedure TQtWidgetSet.AppSetIcon(const AIcon: HICON);
var
  Icon: TQtIcon;
begin
  Icon := TQtIcon(AIcon);
  if Icon <> nil then
    QApplication_setWindowIcon(Icon.Handle)
  else
    QApplication_setWindowIcon(nil);
end;
}

procedure TQtWidgetSet.AppSetTitle(const ATitle: string);
var
  W: WideString;
begin
  W := GetUtf8String(ATitle);
  QCoreApplication_setApplicationName(@W);
end;

procedure TQtWidgetSet.AttachMenuToWindow(AMenuObject: TComponent);
var
  AWidget, AMenuWidget: TQtWidget;
  QtMainWindow: TQtMainWindow absolute AWidget;
  QtMenuBar: TQtMenuBar absolute AMenuWidget;
  R, R1: TRect;
begin
  AMenuWidget := TQtWidget((AMenuObject as TMenu).Handle);
  if AMenuWidget is TQtMenuBar then
  begin
    AWidget := TQtWidget(TWinControl(AMenuObject.Owner).Handle);
    if AWidget is TQtMainWindow then
    begin
      R := AWidget.LCLObject.ClientRect;
      R1 := QtMainWindow.MenuBar.getGeometry;
      R1.Right := R.Right;
      QtMenuBar.setGeometry(R1);
      QtMainWindow.setMenuBar(QMenuBarH(QtMenuBar.Widget));
      QtMainWindow.setVisible(True);
    end;
  end;
end;

function TQtWidgetSet.CreateThemeServices: TThemeServices;
begin
  Result := TQtThemeServices.Create;
end;

function TQtWidgetSet.EventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl;
var
  AObject: TQtObject;
begin
  Result := False;
  case QEvent_type(Event) of
    QEventFocusIn:
      begin
        if QObject_isWidgetType(Sender) then
        begin
          if QWidget_focusPolicy(QWidgetH(Sender)) = QtTabFocus then
          begin
            // remove tab from focus policy
            {$ifdef VerboseTabbedControls}
              WriteLn('found Taabed widget ', PtrUInt(Sender));
            {$endif}
            QWidget_setFocusPolicy(QWidgetH(Sender), QtClickFocus);
            if FOldFocusWidget <> nil then
              QWidget_setFocus(FOldFocusWidget);
          end
          else
            FOldFocusWidget := QWidgetH(Sender);
        end;
      end;
    LCLQt_Destroy:
      begin
        AObject := TQtObject(Pointer(QLCLMessageEvent_getWParam(QLCLMessageEventH(Event))));
        //WriteLn('Catched free for: ', PtrUInt(AObject), ' : ', AObject.ClassName);
        AObject.Free;
        Result := True;
        QEvent_Accept(Event);
      end;
    LCLQt_CheckSynchronize:
      begin
        // a thread is waiting -> synchronize
        CheckSynchronize;
      end;
  end;
end;

procedure TQtWidgetSet.OnWakeMainThread(Sender: TObject);
var
  Event: QEventH;
begin
  Event := QEvent_create(LCLQt_CheckSynchronize);
  QCoreApplication_postEvent(QCoreApplication_instance(), Event);
end;

function TQtWidgetSet.LCLPlatform: TLCLPlatform;
begin
  Result:= lpQT;
end;

function TQtWidgetSet.DCGetPixel(CanvasHandle: HDC; X, Y: integer): TGraphicsColor;
{var
  Color: QColorH;}
begin
  Result := clNone;

  if not IsValidDC(CanvasHandle) then Exit;
  
  if (TQtDeviceContext(CanvasHandle).vImage <> nil) then
  begin
    Result := TColor(QImage_pixel(TQtDeviceContext(CanvasHandle).vImage, X, Y));


{    Color := QColor_create(QImage_pixel(TQtDeviceContext(CanvasHandle).vImage, X, Y));

    Result := RGBToColor(QColor_red(Color), QColor_green(Color), QColor_blue(Color));

    QColor_destroy(Color);}
  end;
end;

procedure dbgcolor(msg: string; C:TQColor);
begin
  debugLn(msg+' spec=%x alpha=%x r=%x g=%x b=%x pad=%x',[c.ColorSpec,c.Alpha,c.r,c.g,c.b,c.pad]);
end;

procedure TQtWidgetSet.DCSetPixel(CanvasHandle: HDC; X, Y: integer; AColor: TGraphicsColor);
var
  ASavedColor: TQColor;
  Color: TQColor;
  Pen: QPenH;
  Painter: QPainterH;
begin
  if IsValidDC(CanvasHandle) then
  begin
    //WriteLn('TQtWidgetSet.DCSetPixel X=',X,' Y=',Y, ' AColor=',dbghex(AColor));
    Painter := TQtDeviceContext(CanvasHandle).Widget;
    Pen := QPainter_pen(Painter);
    QPen_color(Pen, @ASavedColor);
    QColor_setRgb(QColorH(@Color),Red(AColor),Green(AColor),Blue(AColor));
    QPainter_setPen(Painter, @Color);
    QPainter_drawPoint(Painter, X,Y);
    QPainter_setPen(Painter, @ASavedColor);
  end;
end;

procedure TQtWidgetSet.DCRedraw(CanvasHandle: HDC);
begin
  // TODO: implement me
end;

procedure TQtWidgetSet.SetDesigning(AComponent: TComponent);
begin

end;

function TQtWidgetSet.InitHintFont(HintFont: TObject): Boolean;
begin
  Result := False;
end;

{------------------------------------------------------------------------------
  Function: TQtWidgetSet.IsValidDC
  Params:   DC     -  handle to a device context (TQtDeviceContext)
  Returns:  True   -  if the DC is valid
 ------------------------------------------------------------------------------}
function TQtWidgetSet.IsValidDC(const DC: HDC): Boolean;
begin
  Result := (DC <> 0);
end;

{------------------------------------------------------------------------------
  Function: TQtWidgetSet.IsValidGDIObject
  Params:   GDIObject  -  handle to a GDI Object (TQtFont, TQtBrush, etc)
  Returns:  True       -  if the DC is valid
  
  Remark: All handles for GDI objects must be pascal objects so we can
 distinguish between them
 ------------------------------------------------------------------------------}
function TQtWidgetSet.IsValidGDIObject(const GDIObject: HGDIOBJ): Boolean;
var
  aObject: TObject;
begin
  Result := False;
  
  if GDIObject = 0 then Exit;
  
  aObject := TObject(GDIObject);

  if aObject is TObject then
  begin
    Result := (aObject is TQtFont) or (aObject is TQtBrush) or (aObject is TQtImage)
      or (aObject is TQtPen) or (aObject is TQTRegion);
  end;
end;

function TQtWidgetSet.DragImageList_BeginDrag(AImage: QImageH;
  AHotSpot: TPoint): Boolean;
var
  ASize: TSize;
  APixmap: QPixmapH;
  AMask: QBitmapH;
  ABrush: QBrushH;
  APalette: QPaletteH;
begin
  if FDragImageList = nil then
  begin
    FDragImageList := QWidget_create(nil, QtSubWindow or QtFramelessWindowHint or QtWindowStaysOnTopHint);
    QImage_size(AImage, @ASize);
    QWidget_setFixedSize(FDragImageList, @ASize);
    APixmap := QPixmap_create();
    QPixmap_fromImage(APixmap, AImage);
    AMask := QBitmap_create();
    QPixmap_mask(APixmap, AMask);
    QWidget_setMask(FDragImageList, AMask);
    ABrush := QBrush_create(AImage);
    APalette := QWidget_palette(FDragImageList);
    QPalette_setBrush(APalette, QPaletteWindow, ABrush);
    QBrush_destroy(ABrush);
    QBitmap_destroy(AMask);
    QPixmap_destroy(APixmap);
    
    QWidget_setAutoFillBackground(FDragImageList, True);
    
    FDragHotSpot := AHotSpot;
  end;
  Result := FDragImageList <> nil;
end;

procedure TQtWidgetSet.DragImageList_EndDrag;
begin
  if FDragImageList <> nil then
  begin
    QObject_deleteLater(FDragImageList);
    FDragImageList := nil;
  end;
end;

function TQtWidgetSet.DragImageList_DragMove(X, Y: Integer): Boolean;
begin
  Result := FDragImageList <> nil;
  if Result then
  begin
    QWidget_raise(FDragImageList);
    QWidget_move(FDragImageList, X - FDragHotSpot.X, Y - FDragHotSpot.Y);
  end;
end;

function TQtWidgetSet.DragImageList_SetVisible(NewVisible: Boolean): Boolean;
begin
  Result := FDragImageList <> nil;
  if Result then
    QWidget_setVisible(FDragImageList, NewVisible);
end;

{------------------------------------------------------------------------------
  Function: CreateDefaultFont
  Params:  none
  Returns: a TQtFont object

  Creates an default font, used for initial values
 ------------------------------------------------------------------------------}
function TQtWidgetSet.CreateDefaultFont: HFONT;
var
  QtFont: TQtFont;
begin
  QtFont := TQtFont.Create(True, True);
  QApplication_font(QtFont.Widget);
  Result := HFONT(QtFont);
end;

procedure TQtWidgetSet.DeleteDefaultDC;
begin
  if FStockDefaultDC <> 0 then
  TQtDeviceContext(FStockDefaultDC).Free;
  FStockDefaultDC := 0;
end;

procedure TQtWidgetSet.FreeStockItems;

  procedure DeleteAndNilObject(var h: HGDIOBJ);
  begin
    DeleteObject(h);
    h:=0;
  end;

begin
  DeleteAndNilObject(FStockNullBrush);
  DeleteAndNilObject(FStockBlackBrush);
  DeleteAndNilObject(FStockLtGrayBrush);
  DeleteAndNilObject(FStockGrayBrush);
  DeleteAndNilObject(FStockDkGrayBrush);
  DeleteAndNilObject(FStockWhiteBrush);

  DeleteAndNilObject(FStockNullPen);
  DeleteAndNilObject(FStockBlackPen);
  DeleteAndNilObject(FStockWhitePen);

  DeleteAndNilObject(FStockSystemFont);
end;

function TQtWidgetSet.GetQtDefaultDC: HDC;
begin
  Result := FStockDefaultDC;
end;

procedure TQtWidgetSet.SetQtDefaultDC(Handle: HDC);
begin
  FStockDefaultDC := Handle;
end;

procedure TQtWidgetSet.InitStockItems;
var
  LogBrush: TLogBrush;
  logPen : TLogPen;
begin
  FillChar(LogBrush,SizeOf(TLogBrush),0);
  LogBrush.lbStyle := BS_NULL;
  FStockNullBrush := CreateBrushIndirect(LogBrush);
  TQtBrush(FStockNullBrush).FShared := True;
  
  LogBrush.lbStyle := BS_SOLID;
  LogBrush.lbColor := $000000;
  FStockBlackBrush := CreateBrushIndirect(LogBrush);
  TQtBrush(FStockBlackBrush).FShared := True;
  
  LogBrush.lbColor := $C0C0C0;
  FStockLtGrayBrush := CreateBrushIndirect(LogBrush);
  TQtBrush(FStockLtGrayBrush).FShared := True;
  
  LogBrush.lbColor := $808080;
  FStockGrayBrush := CreateBrushIndirect(LogBrush);
  TQtBrush(FStockGrayBrush).FShared := True;
  
  LogBrush.lbColor := $404040;
  FStockDkGrayBrush := CreateBrushIndirect(LogBrush);
  TQtBrush(FStockDkGrayBrush).FShared := True;
  
  LogBrush.lbColor := $FFFFFF;
  FStockWhiteBrush := CreateBrushIndirect(LogBrush);
  TQtBrush(FStockWhiteBrush).FShared := True;

  LogPen.lopnStyle := PS_NULL;
  LogPen.lopnWidth.X := 1;
  LogPen.lopnColor := $FFFFFF;
  FStockNullPen := CreatePenIndirect(LogPen);
  TQtPen(FStockNullPen).FShared := True;
 
  LogPen.lopnStyle := PS_SOLID;
  FStockWhitePen := CreatePenIndirect(LogPen);
  TQtPen(FStockWhitePen).FShared := True;
 
  LogPen.lopnColor := $000000;
  FStockBlackPen := CreatePenIndirect(LogPen);
  TQtPen(FStockBlackPen).FShared := True;

  FStockSystemFont := 0;//Styles aren't initialized yet
  
  FStockDefaultDC := 0; // app must be initialized
end;

//------------------------------------------------------------------------
