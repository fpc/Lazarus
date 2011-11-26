{%MainUnit customdrawnint.pas}

{******************************************************************************
                                   customdrawnobject_win.inc
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

type
  Psaved_state = ^Tsaved_state;
  Tsaved_state = packed record
    angle : cfloat;
    x : cint32;
    y : cint32;
  end;

  Pengine = ^Tengine;
  Tengine = packed record
    app : Pandroid_app;
    animating : cint;
    display : EGLDisplay;
    surface : EGLSurface;
    context : EGLContext;
    width : cint32;
    height : cint32;
    state : Tsaved_state;
    //
    Image: TLazIntfImage;
    Canvas: TLazCanvas;
    RawImage: TRawImage;
  end;

const
 attribs: array[0..8] of EGLint = (
           EGL_SURFACE_TYPE, EGL_WINDOW_BIT,
           EGL_BLUE_SIZE, 8,
           EGL_GREEN_SIZE, 8,
           EGL_RED_SIZE, 8,
           EGL_NONE);

function engine_init_display(engine: Pengine): cint;
var w, h, dummy, format,numConfigs: EGLint;
    config: EGLConfig;
    surface: EGLSurface;
    context: EGLContext;
    display: Pointer;
begin
   // initialize OpenGL ES and EGL

   (*
    * Here specify the attributes of the desired configuration.
    * Below, we select an EGLConfig with at least 8 bits per color
    * component compatible with on-screen windows
     *)

   display := eglGetDisplay(EGL_DEFAULT_DISPLAY);

   eglInitialize(display, nil,nil);

(* Here, the application chooses the configuration it desires. In this
    * sample, we have a very simplified selection process, where we pick
    * the first EGLConfig that matches our criteria  *)

   eglChooseConfig(display, attribs, @config, 1, @numConfigs);

(* EGL_NATIVE_VISUAL_ID is an attribute of the EGLConfig that is
    * guaranteed to be accepted by ANativeWindow_setBuffersGeometry().
    * As soon as we picked a EGLConfig, we can safely reconfigure the
    * ANativeWindow buffers to match, using EGL_NATIVE_VISUAL_ID.  *)

   eglGetConfigAttrib(display, config, EGL_NATIVE_VISUAL_ID, @format);

   ANativeWindow_setBuffersGeometry(engine^.app^.window, 0, 0, format);

   surface := eglCreateWindowSurface(display, config, engine^.app^.window, nil);
   context := eglCreateContext(display, config, nil, nil);

   if eglMakeCurrent(display, surface, surface, context) = EGL_FALSE then
   begin
       LOGW('Unable to eglMakeCurrent');
       exit(-1);
   end;

   eglQuerySurface(display, surface, EGL_WIDTH, @w);
   eglQuerySurface(display, surface, EGL_HEIGHT, @h);

   engine^.display := display;
   engine^.context := context;
   engine^.surface := surface;
   engine^.width := w;
   engine^.height := h;
   engine^.state.angle := 0;

   UpdateControlLazImageAndCanvas(engine^.Image, engine^.Canvas,
     128, 128, clfRGB16_R5G6B5);
   engine^.Image.GetRawImage(engine^.RawImage);
   engine^.Canvas.Brush.FPColor := colRed;
   engine^.Canvas.Rectangle(0, 0, 100, 100);

   // Initialize GL state.
   glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_FASTEST);
   glEnable(GL_CULL_FACE);
   glShadeModel(GL_SMOOTH);
   glDisable(GL_DEPTH_TEST);

   result := 0;
end;

procedure engine_draw_frame(engine: Pengine);
const
  vertices: array[0..7] of GLfloat =
//      (-1.0, 1.0,  1.0, 1.0,  -1.0, -1.0,   1.0, -1.0);
    (0, 300,
     300, 300,
     0, 0,
     300, 0);
  normals: array[0..11] of GLfloat =
    (0.0, 0.0, 1.0,
     0.0, 0.0, 1.0,
     0.0, 0.0, 1.0,
     0.0, 0.0, 1.0);
  textureCoords: array[0..7] of GLfloat =
    (0.0, 0.0,
     1.0, 0.0,
     0.0, 1.0,
     1.0, 1.0);
var
  texture: GLuint;
  lError: GLenum;
begin
   if engine^.display = nil then exit;

   LOGW('[engine_draw_frame]');

   // Set a projection which matches the screen size

   glMatrixMode(GL_PROJECTION);
   lError := glGetError();
   if lError <> GL_NO_ERROR then LOGW(PChar('[engine_draw_frame] 1 Error='+IntToStr(Integer(lError))));
   glLoadIdentity();
   lError := glGetError();
   if lError <> GL_NO_ERROR then LOGW(PChar('[engine_draw_frame] 2 Error='+IntToStr(Integer(lError))));
   glOrthof(0, engine^.width, engine^.height, 0, 0, 1);
   lError := glGetError();
   if lError <> GL_NO_ERROR then LOGW(PChar('[engine_draw_frame] 3 Error='+IntToStr(Integer(lError))));
   glMatrixMode(GL_MODELVIEW);
   lError := glGetError();
   if lError <> GL_NO_ERROR then LOGW(PChar('[engine_draw_frame] 4 Error='+IntToStr(Integer(lError))));
   glLoadIdentity();
   lError := glGetError();
   if lError <> GL_NO_ERROR then LOGW(PChar('[engine_draw_frame] 5 Error='+IntToStr(Integer(lError))));

   // Displacement trick for exact pixelization
   glTranslatef(0.357, 0.357, 0);
   lError := glGetError();
   if lError <> GL_NO_ERROR then LOGW(PChar('[engine_draw_frame] 6 Error='+IntToStr(Integer(lError))));

   // Turn on 2D mode

   glDisable(GL_DEPTH_TEST);
   lError := glGetError();
   if lError <> GL_NO_ERROR then LOGW(PChar('[engine_draw_frame] 7 Error='+IntToStr(Integer(lError))));
   glClear(GL_COLOR_BUFFER_BIT);
   lError := glGetError();
   if lError <> GL_NO_ERROR then LOGW(PChar('[engine_draw_frame] 8 Error='+IntToStr(Integer(lError))));

   // Draw a square
   glVertexPointer(2, GL_FLOAT, 0, @vertices[0]);
   lError := glGetError();
   if lError <> GL_NO_ERROR then LOGW(PChar('[engine_draw_frame] 9 Error='+IntToStr(Integer(lError))));
   glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
   lError := glGetError();
   if lError <> GL_NO_ERROR then LOGW(PChar('[engine_draw_frame] 10 Error='+IntToStr(Integer(lError))));

{   glEnableClientState(GL_VERTEX_ARRAY);
   glEnableClientState(GL_NORMAL_ARRAY);
   glEnableClientState(GL_TEXTURE_COORD_ARRAY);

   glGenTextures(1, @texture);
   glBindTexture(GL_TEXTURE_2D, texture);
   glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA,
     128, 128, 0,//engine^.width, engine^.height, 0,
     GL_RGBA, GL_UNSIGNED_SHORT_5_6_5, Pointer(engine^.RawImage.Data));
   glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
   glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

   glBindTexture(GL_TEXTURE_2D, texture);
   glVertexPointer(2, GL_FLOAT, 0, @vertices[0]);
   glNormalPointer(GL_FLOAT, 0, @normals[0]);
   glTexCoordPointer(2, GL_FLOAT, 0, @textureCoords[0]);

   glLoadIdentity();

   glEnable(GL_TEXTURE_2D);
   glEnable(GL_BLEND);
   glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

   glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);

   // Just fill the screen with a color.
   //glClearColor(engine^.state.x/engine^.width, engine^.state.angle, engine^.state.y/engine^.height, 1);
   //glClear(GL_COLOR_BUFFER_BIT);}

   eglSwapBuffers(engine^.display, engine^.surface);
   lError := glGetError();
   if lError <> GL_NO_ERROR then LOGW(PChar('[engine_draw_frame] 11 Error='+IntToStr(Integer(lError))));
end;


procedure engine_term_display(engine: Pengine);
begin
   if (engine^.display <> EGL_NO_DISPLAY) then
   begin
       eglMakeCurrent(engine^.display, EGL_NO_SURFACE, EGL_NO_SURFACE, EGL_NO_CONTEXT);
       if (engine^.context <> EGL_NO_CONTEXT) then
           eglDestroyContext(engine^.display, engine^.context);
       if (engine^.surface <> EGL_NO_SURFACE) then
           eglDestroySurface(engine^.display, engine^.surface);
       eglTerminate(engine^.display);
   end;

   engine^.animating := 0;
   engine^.display := EGL_NO_DISPLAY;
   engine^.context := EGL_NO_CONTEXT;
   engine^.surface := EGL_NO_SURFACE;
end;

procedure engine_handle_cmd(app: Pandroid_app; cmd: cint32); cdecl;
var engine: Pengine;
begin
   engine := Pengine(app^.userData);
   case cmd of
      APP_CMD_SAVE_STATE:
         begin
            // The system has asked us to save our current state.  Do so.
            engine^.app^.savedState := malloc(sizeof(Tsaved_state));
            Psaved_state(engine^.app^.savedState)^ := engine^.state;
            engine^.app^.savedStateSize := sizeof(Tsaved_state);
         end;
      APP_CMD_INIT_WINDOW:
         begin
            // The window is being shown, get it ready.
            if (engine^.app^.window <> Nil) then
            begin
               LOGW('Initializing display');
               engine_init_display(engine);
               engine_draw_frame(engine);
            end;
         end;
      APP_CMD_TERM_WINDOW:
         begin
            // The window is being hidden or closed, clean it up.
            engine_term_display(engine);
         end;
      APP_CMD_GAINED_FOCUS:
         begin
            // When our app gains focus, we start monitoring the accelerometer.
            {if (engine^.accelerometerSensor <> Nil) then
            begin
               ASensorEventQueue_enableSensor(engine^.sensorEventQueue, engine^.accelerometerSensor);
               // We'd like to get 60 events per second (in us).
               ASensorEventQueue_setEventRate(engine^.sensorEventQueue, engine^.accelerometerSensor, (1000L/60)*1000);
            end;}
         end;
      APP_CMD_LOST_FOCUS:
         begin
            // When our app loses focus, we stop monitoring the accelerometer.
            // This is to avoid consuming battery while not being used.
            {if engine^.accelerometerSensor <> NULL then
               ASensorEventQueue_disableSensor(engine^.sensorEventQueue, engine^.accelerometerSensor);}
            // Also stop animating.
            engine^.animating := 0;
            engine_draw_frame(engine);
         end;
   end;
end;

function engine_handle_input(app: Pandroid_app; event: PAInputEvent): cint32; cdecl;
var engine: Pengine;
begin
   engine := Pengine(app^.userData);
   if AInputEvent_getType(event) = AINPUT_EVENT_TYPE_MOTION then
   begin
      engine^.animating := 1;
      {engine^.state.x := AMotionEvent_getX(event, 0);
      engine^.state.y := AMotionEvent_getY(event, 0);}
      result := 1;
   end
   else
      result := 0;
end;

procedure android_main(state: Pandroid_app); cdecl; export;
var engine: Tengine;
    ident,events: cint;
    source: Pandroid_poll_source;
    val: cint;
begin
   // Make sure glue isn't stripped.
   app_dummy();
   LOGW('Android main!');

   FillChar(engine, sizeof(Tengine), 0);
   LOGW('Android main 2!');

   state^.userData := @engine;
   state^.onAppCmd := @engine_handle_cmd;
   state^.onInputEvent := @engine_handle_input;
   engine.app := state;
   LOGW('Android main 3!');

   if state^.savedState <> nil then
      // We are starting with a previous saved state; restore from it.
      engine.state := Psaved_state(state^.savedState)^;

   LOGW('Entering loop');
   // loop waiting for stuff to do.

   while true do
   begin// Read all pending events.
      // If not animating, we will block forever waiting for events.
      // If animating, we loop until all events are read, then continue
      // to draw the next frame of animation.

      if engine.animating<>0 then
         val := 0
      else
         val := -1;
      ident := ALooper_pollAll(val, nil, @events,@source);
      while (ident >= 0) do
      begin
         // Process this event.
         if (source <> nil) then
            source^.process(state, source);

         // If a sensor has data, process it now.
         if (ident = LOOPER_ID_USER) then
         begin
            {if (engine.accelerometerSensor != nil) then
            begin
               ASensorEvent event;
               while (ASensorEventQueue_getEvents(engine.sensorEventQueue, &event, 1) > 0) do
               begin
                  LOGI("accelerometer: x=%f y=%f z=%f",
                          [event.acceleration.x, event.acceleration.y,
                          event.acceleration.z]);
               end;
            end;}
         end;

         // Check if we are exiting.
         if (state^.destroyRequested <> 0) then
         begin
            LOGW('Destroy requested');
            engine_term_display(@engine);
            exit;
         end;

         if engine.animating<>0 then
            val := 0
         else
            val := -1;
         ident := ALooper_pollAll(val, nil, @events,@source);
      end;

      if engine.animating <> 0 then
      begin
         // Done with events; draw next animation frame.
         engine.state.angle := engine.state.angle + 0.01;
         if (engine.state.angle > 1) then
            engine.state.angle := 0;
      end;

      // Drawing is throttled to the screen update rate, so there
      // is no need to do timing here.
      engine_draw_frame(@engine);
   end;
end;

{------------------------------------------------------------------------------
  Method: TCDWidgetSet.Create
  Params:  None
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
procedure TCDWidgetSet.BackendCreate;
begin

end;

{------------------------------------------------------------------------------
  Method: TWinCEWidgetSet.Destroy
  Params:  None
  Returns: Nothing

  destructor for the class.
 ------------------------------------------------------------------------------}
procedure TCDWidgetSet.BackendDestroy;
begin

end;

{------------------------------------------------------------------------------
  Method: TWinCEWidgetSet.AppInit
  Params:  None
  Returns: Nothing

  initialize Windows
 ------------------------------------------------------------------------------}
procedure TCDWidgetSet.AppInit(var ScreenInfo: TScreenInfo);
begin
   {$ifdef VerboseCDApplication}
   //DebugLn('TCDWidgetSet.AppInit');
   {$endif}
end;

procedure TCDWidgetSet.AppRun(const ALoop: TApplicationMainLoop);
begin
  {$ifdef VerboseCDApplication}
  DebugLn('TCDWidgetSet.AppRun');
  {$endif}
end;

(*
function TWinCEWidgetSet.GetAppHandle: THandle;
begin
  Result:= FAppHandle;
end;

procedure TWinCEWidgetSet.SetAppHandle(const AValue: THandle);
begin
  // Do it only if handle is not yet created (for example for DLL initialization)
  // if handle is already created we can't reassign it
  if AppHandle = 0 then
    FAppHandle := AValue;
end;*)

{------------------------------------------------------------------------------
  Method: TWinCEWidgetSet.AppMinimize
  Params:  None
  Returns: Nothing

  Minimizes the whole application to the taskbar
 ------------------------------------------------------------------------------}
procedure TCDWidgetSet.AppMinimize;
begin
//  Windows.SendMessage(FAppHandle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
end;

{------------------------------------------------------------------------------
  Method: TWinCEWidgetSet.AppRestore
  Params:  None
  Returns: Nothing

  Restore minimized whole application from taskbar
 ------------------------------------------------------------------------------}

procedure TCDWidgetSet.AppRestore;
begin
//  Windows.SendMessage(FAppHandle, WM_SYSCOMMAND, SC_RESTORE, 0);
end;


{------------------------------------------------------------------------------
  Method: TWinCEWidgetSet.AppBringToFront
  Params:  None
  Returns: Nothing

  Brings the entire application on top of all other non-topmost programs
 ------------------------------------------------------------------------------}
procedure TCDWidgetSet.AppBringToFront;
begin
end;

(*
procedure TWinCEWidgetSet.SetDesigning(AComponent: TComponent);
begin
  //if Data<>nil then EnableWindow((AComponent As TWinControl).Handle, boolean(Data^));
end;

{------------------------------------------------------------------------------
  Method: TWinCEWidgetSet.SetCallback
  Params: Msg    - message for which to set a callback
          Sender - object to which callback will be sent
  Returns:  nothing

  Applies a Message to the sender
 ------------------------------------------------------------------------------}
procedure TWinCEWidgetSet.SetCallback(Msg: LongInt; Sender: TObject);
var
  Window: HWnd;
begin
  //DebugLn('Trace:TWinCEWidgetSet.SetCallback - Start');
  //DebugLn(Format('Trace:TWinCEWidgetSet.SetCallback - Class Name --> %S', [Sender.ClassName]));
  //DebugLn(Format('Trace:TWinCEWidgetSet.SetCallback - Message Name --> %S', [GetMessageName(Msg)]));
  if Sender Is TControlCanvas then
    Window := TControlCanvas(Sender).Handle
  else if Sender Is TCustomForm then
    Window := TCustomForm(Sender).Handle
  else
    Window := TWinControl(Sender).Handle;
  if Window=0 then exit;

  //DebugLn('Trace:TWinCEWidgetSet.SetCallback - Exit');
end;

{------------------------------------------------------------------------------
  Method: TWinCEWidgetSet.RemoveCallbacks
  Params:   Sender - object from which to remove callbacks
  Returns:  nothing

  Removes Call Back Signals from the sender
 ------------------------------------------------------------------------------}
procedure TWinCEWidgetSet.RemoveCallbacks(Sender: TObject);
var
  Window: HWnd;
begin
  if Sender Is TControlCanvas then
    Window := TControlCanvas(Sender).Handle
  else if Sender Is TCustomForm then
    Window := TCustomForm(Sender).Handle
  else
    Window := (Sender as TWinControl).Handle;
  if Window=0 then exit;
end;*)

{------------------------------------------------------------------------------
  Method: TWinCEWidgetSet.AppProcessMessages
  Params:  None
  Returns: Nothing

  Handle all pending messages
 ------------------------------------------------------------------------------}
procedure TCDWidgetSet.AppProcessMessages;
begin
end;
(*
procedure TWinCEWidgetSet.CheckPipeEvents;
var
  lHandler: PPipeEventInfo;
//  lBytesAvail: dword;
//  SomethingChanged: Boolean;
  ChangedCount:integer;
begin
  lHandler := FWaitPipeHandlers;
  ChangedCount := 0;
  while (lHandler <> nil) and (ChangedCount < 10) do
  begin
    {
    roozbeh : ooops not supported
    SomethingChanged:=true;
    if Windows.PeekNamedPipe(lHandler^.Handle, nil, 0, nil, @lBytesAvail, nil) then
    begin
      if lBytesAvail <> 0 then
        lHandler^.OnEvent(lHandler^.UserData, [prDataAvailable])
      else
        SomethingChanged := false;
    end else
      lHandler^.OnEvent(lHandler^.UserData, [prBroken]);
    if SomethingChanged then
      lHandler := FWaitPipeHandlers
    else begin
      lHandler := lHandler^.Next;
      ChangedCount := 0;
    end;
    inc(ChangedCount);}
  end;
end;*)

{------------------------------------------------------------------------------
  Method: TWinCEWidgetSet.AppWaitMessage
  Params:  None
  Returns: Nothing

  Passes execution control to Windows
 ------------------------------------------------------------------------------}
//roozbeh:new update...whole procedure body is added.what is it?
procedure TCDWidgetSet.AppWaitMessage;
begin
end;

{------------------------------------------------------------------------------
  Method: TWinCEWidgetSet.AppTerminate
  Params:  None
  Returns: Nothing

  Tells Windows to halt and destroy
 ------------------------------------------------------------------------------}

procedure TCDWidgetSet.AppTerminate;
begin
  //DebugLn('Trace:TWinCEWidgetSet.AppTerminate - Start');
end;


procedure TCDWidgetSet.AppSetIcon(const Small, Big: HICON);
begin
end;

procedure TCDWidgetSet.AppSetTitle(const ATitle: string);
begin
end;

procedure TCDWidgetSet.AppSetVisible(const AVisible: Boolean);
begin
end;

function TCDWidgetSet.AppRemoveStayOnTopFlags(const ASystemTopAlso: Boolean = False): Boolean;
begin
end;

function TCDWidgetSet.AppRestoreStayOnTopFlags(const ASystemTopAlso: Boolean = False): Boolean;
begin
end;

procedure TCDWidgetSet.AppSetMainFormOnTaskBar(const DoSet: Boolean);
begin
end;

{------------------------------------------------------------------------------
  function: CreateTimer
  Params: Interval:
          TimerFunc: Callback
  Returns: a Timer id (use this ID to destroy timer)

  Design: A timer which calls TimerCallBackProc, is created.
    The TimerCallBackProc calls the TimerFunc.
 ------------------------------------------------------------------------------}
function TCDWidgetSet.CreateTimer(Interval: integer; TimerFunc: TWSTimerProc) : THandle;
begin
end;

{------------------------------------------------------------------------------
  function: DestroyTimer
  Params: TimerHandle
  Returns:
 ------------------------------------------------------------------------------}
function TCDWidgetSet.DestroyTimer(TimerHandle: THandle) : boolean;
begin
end;
(*
procedure TWinCEWidgetSet.HandleWakeMainThread(Sender: TObject);
begin
  // wake up GUI thread by sending a message to it
  Windows.PostMessage(AppHandle, WM_NULL, 0, 0);
end;
*)

// This code is unnecessary in FPC 2.6+,
// it was required when the 2.5.1 snapshot was created
{$ifdef ver2_5}
procedure PASCALMAIN; external name 'PASCALMAIN';

procedure FPC_SHARED_LIB_START; [public, alias: 'FPC_SHARED_LIB_START'];
begin
   PASCALMAIN;
end;
{$endif}

