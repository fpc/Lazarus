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

{function Java_com_pascal_lclproject_LCLActivity_stringFromJNI(env:PJNIEnv;this:jobject):jstring; cdecl;
var x:single;
begin
  __android_log_write(ANDROID_LOG_INFO,'nativetest','Java_com_bero_nativetest_Main_stringFromJNI entered');
  curEnv^.SetLongField(curEnv,curClass,nativeCodeLoaded,1);
  x:=8;
  result:=env^.NewStringUTF(env,pchar('Hello from native free pascal code by BeRo to the java world on the android platform ! '+floattostr(x*0.5)));
  __android_log_write(ANDROID_LOG_INFO,'nativetest','Java_com_bero_nativetest_Main_stringFromJNI exited');
end;}

const
  ACTION_DOWN = 0;
  ACTION_UP = 1;

function Java_com_pascal_lclproject_LCLActivity_LCLOnTouch(env:PJNIEnv;this:jobject; x, y: single; action: jint): jint; cdecl;
var
  lCurForm: TCDNonNativeForm;
  lTarget: TWinControl;
begin
  Result := 0;

  __android_log_write(ANDROID_LOG_INFO,'nativetest','LCLOnTouch called');
{  curJavaClass := env^^.GetObjectClass(env, this);
  __android_log_write(ANDROID_LOG_INFO,'nativetest','LCLOnTouch after GetObjectClass');}
  curJavaObject := this;
  curEnv := env;

  lCurForm := GetCurrentForm();
  lTarget := FindControlWhichReceivedEvent(lCurForm.LCLForm, lCurForm.Children, Round(X), Round(Y));

  case action of
    ACTION_DOWN: LCLSendMouseDownMsg(lTarget, Round(X), Round(Y), mbLeft, []);
    ACTION_UP:
    begin
      LCLSendMouseUpMsg(lTarget, Round(X), Round(Y), mbLeft, []);
      LCLSendClickedMsg(lTarget);
    end;
  end;
end;

function Java_com_pascal_lclproject_LCLActivity_LCLDrawToBitmap(
    env:PJNIEnv;this:jobject; width, height: jint; abitmap: jobject): jint; cdecl;
var
  pixels: PCardinal;
  lCurForm: TCDNonNativeForm;

  struct : TPaintStruct;
  lBitmap, lMask: HBITMAP;
  lRawImage: TRawImage;
  {$IFDEF VerboseCDPaintProfiler}
  lTimeStart: TDateTime;
  {$ENDIF}
begin
  Result := 0;
  AndroidBitmap_lockPixels(env, abitmap, @pixels);

  lCurForm := GetCurrentForm();
  if lCurForm <> nil then
  begin
    {$IFDEF VerboseCDPaintProfiler}
    //lTimeStart := NowUTC();
    {$ENDIF}
    {$IFDEF VerboseCDWindow}
     //DebugLn(Format('[TCDWSCustomForm.EvPaint] AWindowInfo: %x', [PtrInt(AWindowInfo)]));
    {$ENDIF}

    FillChar(struct, SizeOf(TPaintStruct), 0);

    // Prepare the non-native image and canvas
    UpdateControlLazImageAndCanvas(lCurForm.Image, lCurForm.Canvas, Width, Height, clfRGBA32, pixels);

    struct.hdc := HDC(lCurForm.Canvas);

    // Send the paint message to the LCL
    {$IFDEF VerboseCDWindow}
     //DebugLn(Format('[TCDWSCustomForm.EvPaint] OnPaint event started context: %x', [struct.hdc]));
    {$ENDIF}
    LCLSendPaintMsg(lCurForm.LCLForm, struct.hdc, @struct);
    {$IFDEF VerboseCDWindow}
     //DebugLn('[TCDWSCustomForm.EvPaint] OnPaint event ended');
    {$ENDIF}

    // Now paint all child win controls
    RenderChildWinControls(lCurForm.Image, lCurForm.Canvas, lCurForm.Children);
  end;

  // Now returns the bitmap buffer to LCLActivity so that it can render it
  AndroidBitmap_unlockPixels(env, abitmap);
end;

const NativeMethods: array[0..1] of JNINativeMethod=
       ((name:'LCLDrawToBitmap';
         signature:'(IILandroid/graphics/Bitmap;)I';
         fnPtr:@Java_com_pascal_lclproject_LCLActivity_LCLDrawToBitmap;),
        (name:'LCLOnTouch';
         signature:'(FFI)I';
         fnPtr:@Java_com_pascal_lclproject_LCLActivity_LCLOnTouch;)
       );

function JNI_OnLoad(vm:PJavaVM;reserved:pointer):jint; cdecl;
begin
  curVM:=vm;
{  __android_log_write(ANDROID_LOG_INFO,'nativetest','JNI_OnLoad called');
  __android_log_write(ANDROID_LOG_INFO,'nativetest',PChar(Format('CurVM=%x', [PtrInt(CurVM)])));
  __android_log_write(ANDROID_LOG_INFO,'nativetest',PChar(Format('CurVM^=%x', [PtrInt(CurVM^)])));
  __android_log_write(ANDROID_LOG_INFO,'nativetest',PChar(Format('CurVM^^.reserved0=%x', [PtrInt(CurVM^^.reserved0)])));
  __android_log_write(ANDROID_LOG_INFO,'nativetest',PChar(Format('CurVM^^.GetEnv=%x', [PtrInt(Pointer(@CurVM^^.GetEnv))])));
  if vm^^.GetEnv(curVM,@curEnv,JNI_VERSION_1_4)<>JNI_OK then begin //<<<--- THIS CRASHES
    __android_log_write(ANDROID_LOG_INFO{FATAL},'nativetest','curVM^.GetEnv failed');
    result:=JNI_ERR;
    exit;
  end;

  __android_log_write(ANDROID_LOG_INFO,'nativetest','Reading curClass');
  curClass:=curEnv^^.FindClass(curEnv,'com/pascal/lcltest/LCLActivity');
  if not assigned(curClass) then begin
  __android_log_write(ANDROID_LOG_FATAL,'nativetest','curEnv^.FindClass failed');
  result:=JNI_ERR;
  exit;
  end;
  if curEnv^^.RegisterNatives(curEnv,curClass,@NativeMethods[0],length(NativeMethods))<0 then begin
  __android_log_write(ANDROID_LOG_FATAL,'nativetest','curEnv^.RegisterNatives failed');
  result:=JNI_ERR;
  exit;
  end;

  nativeCodeLoaded:=curEnv^^.GetFieldID(curEnv,curClass,'nativeCodeLoaded','J');
  if not assigned(nativeCodeLoaded) then begin
  __android_log_write(ANDROID_LOG_FATAL,'nativetest','curEnv^.GetFieldID failed');
  result:=JNI_ERR;
  exit;
  end;         }

  result:=JNI_VERSION_1_4;// 1_6?
end;

procedure JNI_OnUnload(vm:PJavaVM;reserved:pointer); cdecl;
begin
end;

procedure TCDWidgetSet.AndroidDebugLn(AStr: string);
begin
  __android_log_write(ANDROID_LOG_INFO, 'lclproject', PChar(AccumulatedStr+AStr));
  AccumulatedStr := '';
end;

procedure TCDWidgetSet.AccumulatingDebugOut(AStr: string);
begin
  AccumulatedStr := AccumulatedStr + AStr;
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
  DebugLn('TCDWidgetSet.AppInit');
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

