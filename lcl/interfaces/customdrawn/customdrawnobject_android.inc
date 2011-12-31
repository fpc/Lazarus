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

const
  // Shared Mouse / Key constants
  ACTION_DOWN = 0;
  ACTION_UP = 1;

  // Touch constants
  ACTION_MOVE = 2;

  // Constants from android/view/KeyEvent up to level 8
  ACTION_MULTIPLE = 2;
  // Keys are in KeyCodes.pas

  // from android.view.KeyCharacterMap
  COMBINING_ACCENT = $80000000;

function Java_com_pascal_lclproject_LCLActivity_LCLOnTouch(env:PJNIEnv;this:jobject; x, y: single; action: jint): jint; cdecl;
var
  lCurForm: TCDNonNativeForm;
  lTarget: TWinControl;
  lEventPos: TPoint;
begin
  {$ifdef VerboseCDEvents}
  __android_log_write(ANDROID_LOG_INFO,'lclapp',PChar(Format('LCLOnTouch called x=%f y=%f action=%d', [x, y, action])));
  {$endif}
  eventResult := 0;

  lCurForm := GetCurrentForm();

  case action of
    ACTION_DOWN:
    begin
      CallbackMouseDown(lCurForm, Round(X), Round(Y), mbLeft, []);
    end;
    ACTION_UP:
    begin
      CallbackMouseUp(lCurForm, Round(X), Round(Y), mbLeft, []);
    end;
    ACTION_MOVE: CallbackMouseMove(lCurForm, Round(X), Round(Y), []);
  end;

  // This sends messages like Invalidate requests
  Result := eventResult;
end;

function Java_com_pascal_lclproject_LCLActivity_LCLDrawToBitmap(
    env:PJNIEnv;this:jobject; width, height: jint; abitmap: jobject): jint; cdecl;
var
  pixels: PCardinal;
  lCurForm: TCDNonNativeForm;

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
    {$IFDEF VerboseCDPaintEvent}
    DebugLn(Format('[Java_com_pascal_lclproject_LCLActivity_LCLDrawToBitmap] lCurForm:TCDNonNativeForm=%x', [PtrInt(lCurForm)]));
    {$ENDIF}

    // Prepare the non-native image and canvas
    UpdateControlLazImageAndCanvas(lCurForm.Image, lCurForm.Canvas, Width, Height, clfRGBA32, pixels, True, False);

    RenderForm(lCurForm.Image, lCurForm.Canvas, lCurForm.LCLForm);
  end;

  // Now returns the bitmap buffer to LCLActivity so that it can render it
  AndroidBitmap_unlockPixels(env, abitmap);
end;

function Java_com_pascal_lclproject_LCLActivity_LCLOnCreate(
    env:PJNIEnv; this:jobject; alclactivity: jobject): jint; cdecl;
begin
  __android_log_write(ANDROID_LOG_INFO, 'lclapp', 'LCLOnCreate called by LCLActivity.onCreate');
  Result := 0;
  javaActivityObject := alclactivity;
  Screen.UpdateScreen(); // Any values read before LCLOnCreate are wrong
  // Update the font size
  CDWidgetset.DefaultFontAndroidSize := Round(16 * (Screen.PixelsPerInch / 125));
  // Now inform the application
  if Assigned(CDWidgetset.ActivityOnCreate) then CDWidgetset.ActivityOnCreate()
  else Application.Run; // <- Support for older code up to 21 dezember
end;

// This one is for all simple dialogs: MessageBox, PromptUser (MessageDlg) and AskUser
function Java_com_pascal_lclproject_LCLActivity_LCLOnMessageBoxFinished(
    env:PJNIEnv; this:jobject; AResult: jint): jint; cdecl;
begin
  //__android_log_write(ANDROID_LOG_INFO, 'lclapp', 'LCLOnCreate called by LCLActivity.onCreate');
  Result := 0;
  if Assigned(Application.OnMessageDialogFinished) then
    Application.OnMessageDialogFinished(Application, AResult);
end;

function Java_com_pascal_lclproject_LCLActivity_LCLOnKey(
    env:PJNIEnv; this:jobject; AKind: jint; AKeyCode: jint;
    AEvent: jobject; AChar: jint): jint; cdecl;
var
  lCurForm: TCDNonNativeForm;
  lTarget, lFocusedControl: TWinControl;
  lKey: Word;
  lCombinedAChar: jint;
  lChar: Cardinal;
  AUTF8Text: string;
  AUTF8Char: TUTF8Char;
  lForm: TCDNonNativeForm;
begin
  lChar := Cardinal(AChar);
  {$ifdef VerboseCDEvents}
  __android_log_write(ANDROID_LOG_INFO,'lclapp',PChar(
    Format('[LCLOnKey] called AKind=%d AKeyCode=%x AChar=%s', [AKind, AKeyCode, UnicodeToUTF8(lChar)])));
  {$endif}
  eventResult := 0;

  lCurForm := GetCurrentForm();
  lKey := CDWidgetset.AndroidKeyCodeToLCLKeyCode(AKeyCode);

  case AKind of
    ACTION_DOWN: CallbackKeyDown(lCurForm, lKey);
    ACTION_UP:
    begin
      CallbackKeyUp(lCurForm, lKey);
      if (lChar <> 0) and ((COMBINING_ACCENT and lChar) = 0) then
      begin
        if CDWidgetset.CombiningAccent <> 0 then
        begin
(*          // Prepare the input
          lJavaString :=javaEnvRef^^.NewStringUTF(javaEnvRef, Str);
          javaEnvRef^^.SetObjectField(javaEnvRef, javaActivityObject, JavaField_lcltext, lJavaString);
          javaEnvRef^^.SetIntField(javaEnvRef, javaActivityObject, javaField_lcltextsize, lFontSize);

          // Call the method
          javaEnvRef^^.CallVoidMethod(javaEnvRef, javaActivityObject, javaMethod_LCLDoGetTextBounds);

          // Read the output
          Size.cx := javaEnvRef^^.GetIntField(javaEnvRef, javaActivityObject, javaField_lclwidth);
          Size.cy := javaEnvRef^^.GetIntField(javaEnvRef, javaActivityObject, javaField_lclheight);
          //
          lCombinedAChar := getDeadChar();*)
        end;
        AUTF8Text := UnicodeToUTF8(lChar);
        AUTF8Char := AUTF8Text;
        CallbackKeyChar(lCurForm, lKey, AUTF8Char);
        CDWidgetset.CombiningAccent := 0;
      end
      else if (lChar <> 0) and ((COMBINING_ACCENT and lChar) <> 0) then
      begin
        CDWidgetset.CombiningAccent := lChar;
      end;

      // Handle the Back hardware key
      if AKeyCode = AKEYCODE_BACK then
      begin
        //DebugLn(Format('CallbackKeyUp D lForm=%x', [PtrInt(lForm)]));
        // The back hardware key hides the current form and shows the one bellow it
        // except if the currently focused control is a text editor, in which case
        // it will take focus out of the text editor
        lForm := lCurForm;
        lFocusedControl := lForm.GetFocusedControl();
        if (lFocusedControl <> nil) and (csRequiresKeyboardInput in lFocusedControl.ControlStyle) then
        begin
          //DebugLn('[LCLOnKey] Sending focus to the form');
          CDWidgetset.CDSetFocusToControl(lForm.LCLForm, nil);
        end
        // If this is the main form, then go back to desktop
        else if (Application.MainForm <> nil) and (lForm = TCDForm(Application.MainForm.Handle)) then
        begin
          //DebugLn('[LCLOnKey] Back key is going to hide the application');
          eventResult := eventResult or 2;
        end
        // for other forms, hide them
        else
        begin
          //DebugLn('[LCLOnKey] Hiding the form');
          HideForm(lForm);
        end;
      end;
    end;
    //ACTION_MULTIPLE:
  end;

  // This sends messages like Invalidate requests
  Result := eventResult;
end;

function Java_com_pascal_lclproject_LCLActivity_LCLOnTimer(
    env:PJNIEnv; this:jobject; ATimer: jobject): jint; cdecl;
var
  lTimer: TCDTimer;
begin
  {$ifdef VerboseCDEvents}
  __android_log_write(ANDROID_LOG_INFO,'lclapp',PChar(
    Format('LCLOnTimer called ATimer=%x', [PtrInt(ATimer)])));
  {$endif}
  eventResult := 0;

  lTimer := FindTimerWithNativeHandle(PtrInt(ATimer));

  if lTimer <> nil then lTimer.TimerFunc()
  else DebugLn('[LCLOnTimer] OnTimer message sent to unknown timer!');

  // This sends messages like Invalidate requests
  Result := eventResult;
end;

function Java_com_pascal_lclproject_LCLActivity_LCLOnConfigurationChanged(
  env:PJNIEnv; this:jobject; ANewDPI, ANewWidth: jint): jint; cdecl;
var
  lForm: TCDNonNativeForm;
  lOldDPI, lNewDPI, lOldFormWidth, lNewFormWidth: Integer;
  i: Integer;
begin
  for i := 0 to GetFormCount()-1 do
  begin
    lForm := GetForm(i);

    if lForm.LayoutAutoAdjusted then lOldDPI := Screen.PixelsPerInch
    else lOldDPI := lForm.LCLForm.DesignTimeDPI;
    lNewDPI := ANewDPI;
    lOldFormWidth := lForm.LCLForm.Width;
    lNewFormWidth := ANewWidth;
    DebugLn(Format('[LCLOnConfigurationChanged] i=%d lOldDPI=%d lNewDPI=%d lOldFormWidth=%d lNewFormWidth=%d',
      [i, lOldDPI, lNewDPI, lOldFormWidth, lNewFormWidth]));
    lForm.LCLForm.AutoAdjustLayout(lapAutoAdjustWithoutHorizontalScrolling,
      lOldDPI, lNewDPI, lOldFormWidth, lNewFormWidth);
  end;

  // Update the font size
  CDWidgetset.DefaultFontAndroidSize := Round(16 * (ANewDPI / 125));
end;

function Java_com_pascal_lclproject_LCLActivity_LCLOnSensorChanged(
  env:PJNIEnv; this:jobject; ASensorKind: jint; AValues: JDoubleArray): jint; cdecl;
var
  arraydata: PDouble;
  arraylen: jsize;
  lIsCopy: jboolean;
  lSensorDataInt: Integer;
  lMessagingStatus: TLazMessagingStatus;
  lUnixTimeStamp: Int64;
begin
  Result := 0;

  if (javaEnvRef = nil) then Exit;

  // Get the elements and length
  lIsCopy := 0;
  arraylen := javaEnvRef^^.GetArrayLength(javaEnvRef, AValues);
  arraydata := javaEnvRef^^.GetDoubleArrayElements(javaEnvRef, AValues, lIsCopy);

  // Send the data to the LCL
  case ASensorKind of
    -11: // Defined by ourselves for Messaging Status
    begin
      lSensorDataInt := Round(arraydata[0]);
      case lSensorDataInt of
        1: lMessagingStatus := mssSentSuccessfully;
        2: lMessagingStatus := mssSendingGeneralError;
        3: lMessagingStatus := mssNoService;
        5: lMessagingStatus := mssRadioOff;
        10:lMessagingStatus := mssReceivedSuccessfully;
        11:lMessagingStatus := mssReceivingGeneralError;
      else
        lMessagingStatus := mssSendingGeneralError;
      end;

      if Assigned(Messaging.OnMessagingStatus) then
        Messaging.OnMessagingStatus(nil, lMessagingStatus);
    end;
    -10: // Defined by ourselves for PositionInfo
    begin
      PositionInfo.latitude := arraydata[0];
      PositionInfo.longitude := arraydata[1];
      PositionInfo.altitude := arraydata[2];
      PositionInfo.accuracy := arraydata[3];
      PositionInfo.altitudeAccuracy := PositionInfo.accuracy;
      PositionInfo.speed := arraydata[4];
      lUnixTimeStamp := Round(arraydata[5]);
      PositionInfo.timeStamp :=  UnixToDateTime(lUnixTimeStamp);

      if Assigned(PositionInfo.OnPositionRetrieved) then
        PositionInfo.OnPositionRetrieved(PositionInfo);
    end;
    1: // ACCELEROMETER
    begin
      Accelerometer.xaxis := arraydata[0];
      Accelerometer.yaxis := arraydata[1];
      Accelerometer.zaxis := arraydata[2];
      if Assigned(Accelerometer.OnSensorChanged) then
        Accelerometer.OnSensorChanged(Accelerometer);
    end;
  end;

  // Don't forget to release it
  javaEnvRef^^.ReleaseDoubleArrayElements(javaEnvRef, AValues, arraydata, 0);

  // This sends messages like Invalidate requests
  Result := eventResult;
end;

const NativeMethods: array[0..7] of JNINativeMethod=
  ((name:'LCLDrawToBitmap';
    signature:'(IILandroid/graphics/Bitmap;)I';
    fnPtr:@Java_com_pascal_lclproject_LCLActivity_LCLDrawToBitmap;),
   (name:'LCLOnTouch';
    signature:'(FFI)I';
    fnPtr:@Java_com_pascal_lclproject_LCLActivity_LCLOnTouch;),
   (name:'LCLOnCreate';
    signature:'(Lcom/pascal/lcltest/LCLActivity;)I'; //android/app/Activity;
    fnPtr:@Java_com_pascal_lclproject_LCLActivity_LCLOnCreate;),
   (name:'LCLOnMessageBoxFinished';
    signature:'(I)I';
    fnPtr:@Java_com_pascal_lclproject_LCLActivity_LCLOnMessageBoxFinished;),
   (name:'LCLOnKey';
    signature:'(IILandroid/view/KeyEvent;I)I';
    fnPtr:@Java_com_pascal_lclproject_LCLActivity_LCLOnKey;),
   (name:'LCLOnTimer';
    signature:'(Ljava/lang/Runnable;)I';
    fnPtr:@Java_com_pascal_lclproject_LCLActivity_LCLOnTimer;),
   (name:'LCLOnConfigurationChanged';
    signature:'(II)I';
    fnPtr:@Java_com_pascal_lclproject_LCLActivity_LCLOnConfigurationChanged;),
   (name:'LCLOnSensorChanged';
    signature:'(I[D)I';
    fnPtr:@Java_com_pascal_lclproject_LCLActivity_LCLOnSensorChanged;)
  );

function JNI_OnLoad(vm:PJavaVM;reserved:pointer):jint; cdecl;
begin
  javaVMRef := vm;

  __android_log_write(ANDROID_LOG_INFO, 'lclapp', 'JNI_OnLoad called');
{  __android_log_write(ANDROID_LOG_INFO, 'lclapp', PChar(Format('vm=%x', [PtrInt(vm)])));
  __android_log_write(ANDROID_LOG_INFO, 'lclapp', PChar(Format('vm^=%x', [PtrInt(vm^)])));
  __android_log_write(ANDROID_LOG_INFO, 'lclapp', PChar(Format('vm^^.reserved0=%x', [PtrInt(vm^^.reserved0)])));
  __android_log_write(ANDROID_LOG_INFO, 'lclapp', PChar(Format('vm^^.GetEnv=%x', [PtrInt(Pointer(@vm^^.GetEnv))])));  }
  {
    vm^^.GetEnv crashes HTC Wildfire, Alcatel and the Emulator if you don't build your project with -CpARMV6
    see: http://groups.google.com/group/android-ndk/browse_thread/thread/ba542483f062a828/ef9077617794e0f5
  }
  if vm^^.GetEnv(vm,@javaEnvRef,JNI_VERSION_1_4)<>JNI_OK then
  begin
    __android_log_write(ANDROID_LOG_FATAL, 'lclapp', 'curVM^.GetEnv failed');
    Exit(JNI_ERR);
  end;

  // Find our activity class
  __android_log_write(ANDROID_LOG_INFO,'lclapp','Reading our Activity Class');
  javaActivityClass := javaEnvRef^^.FindClass(javaEnvRef,'com/pascal/lcltest/LCLActivity');
  if not assigned(javaActivityClass) then
  begin
    __android_log_write(ANDROID_LOG_FATAL, 'lclapp', 'javaEnvRef^.FindClass failed');
    Exit(JNI_ERR);
  end;

  // Register Pascal exported calls
  if javaEnvRef^^.RegisterNatives(javaEnvRef, javaActivityClass, @NativeMethods[0],length(NativeMethods))<0 then
  begin
    __android_log_write(ANDROID_LOG_FATAL, 'lclapp', 'javaEnvRef^.RegisterNatives failed');
    // Exit(JNI_ERR); Don't exit if exporting the native method fails because it works without this too
  end;

  // Read all field IDs
  JavaField_lcltext := javaEnvRef^^.GetFieldID(javaEnvRef, javaActivityClass, 'lcltext', 'Ljava/lang/String;');
  JavaField_lcltitle := javaEnvRef^^.GetFieldID(javaEnvRef, javaActivityClass, 'lcltitle', 'Ljava/lang/String;');
  JavaField_lclbutton1str := javaEnvRef^^.GetFieldID(javaEnvRef, javaActivityClass, 'lclbutton1str', 'Ljava/lang/String;');
  JavaField_lclbutton2str := javaEnvRef^^.GetFieldID(javaEnvRef, javaActivityClass, 'lclbutton2str', 'Ljava/lang/String;');
  JavaField_lclbutton3str := javaEnvRef^^.GetFieldID(javaEnvRef, javaActivityClass, 'lclbutton3str', 'Ljava/lang/String;');
  //
  JavaField_lclwidth := javaEnvRef^^.GetFieldID(javaEnvRef, javaActivityClass, 'lclwidth', 'I');
  JavaField_lclheight := javaEnvRef^^.GetFieldID(javaEnvRef, javaActivityClass, 'lclheight', 'I');
  JavaField_lclbutton1 := javaEnvRef^^.GetFieldID(javaEnvRef, javaActivityClass, 'lclbutton1', 'I');
  JavaField_lclbutton2 := javaEnvRef^^.GetFieldID(javaEnvRef, javaActivityClass, 'lclbutton2', 'I');
  JavaField_lclbutton3 := javaEnvRef^^.GetFieldID(javaEnvRef, javaActivityClass, 'lclbutton3', 'I');
  JavaField_lclbitmap := javaEnvRef^^.GetFieldID(javaEnvRef, javaActivityClass, 'lclbitmap', 'Landroid/graphics/Bitmap;');
  JavaField_lcltextsize := javaEnvRef^^.GetFieldID(javaEnvRef, javaActivityClass, 'lcltextsize', 'I');
  // Text metrics
  javaField_lcltextascent := javaEnvRef^^.GetFieldID(javaEnvRef, javaActivityClass, 'lcltextascent', 'I');
  javaField_lcltextbottom := javaEnvRef^^.GetFieldID(javaEnvRef, javaActivityClass, 'lcltextbottom', 'I');
  javaField_lcltextdescent := javaEnvRef^^.GetFieldID(javaEnvRef, javaActivityClass, 'lcltextdescent', 'I');
  javaField_lcltextleading := javaEnvRef^^.GetFieldID(javaEnvRef, javaActivityClass, 'lcltextleading', 'I');
  javaField_lcltexttop := javaEnvRef^^.GetFieldID(javaEnvRef, javaActivityClass, 'lcltexttop', 'I');
  javaField_lclmaxwidth := javaEnvRef^^.GetFieldID(javaEnvRef, javaActivityClass, 'lclmaxwidth', 'I');
  javaField_lclmaxcount := javaEnvRef^^.GetFieldID(javaEnvRef, javaActivityClass, 'lclmaxcount', 'I');
  javaField_lclpartialwidths := javaEnvRef^^.GetFieldID(javaEnvRef, javaActivityClass, 'lclpartialwidths', '[F');
  // Timer
  javaField_lcltimerinterval := javaEnvRef^^.GetFieldID(javaEnvRef, javaActivityClass, 'lcltimerinterval', 'I');
  javaField_lcltimerid := javaEnvRef^^.GetFieldID(javaEnvRef, javaActivityClass, 'lcltimerid', 'Ljava/lang/Runnable;');
  // Screen Metrics
  javaField_lclxdpi := javaEnvRef^^.GetFieldID(javaEnvRef, javaActivityClass, 'lclxdpi', 'I');
  javaField_lclydpi := javaEnvRef^^.GetFieldID(javaEnvRef, javaActivityClass, 'lclydpi', 'I');
  javaField_lclformwidth := javaEnvRef^^.GetFieldID(javaEnvRef, javaActivityClass, 'lclformwidth', 'I');
  javaField_lclformheight := javaEnvRef^^.GetFieldID(javaEnvRef, javaActivityClass, 'lclformheight', 'I');
  javaField_lclscreenwidth := javaEnvRef^^.GetFieldID(javaEnvRef, javaActivityClass, 'lclscreenwidth', 'I');
  javaField_lclscreenheight := javaEnvRef^^.GetFieldID(javaEnvRef, javaActivityClass, 'lclscreenheight', 'I');
  // For LazDeviceAPIs
  javaField_lcldestination := javaEnvRef^^.GetFieldID(javaEnvRef, javaActivityClass, 'lcldestination', 'Ljava/lang/String;');
  javaField_lclkind := javaEnvRef^^.GetFieldID(javaEnvRef, javaActivityClass, 'lclkind', 'I');
  //
  if not assigned(JavaField_lcltext) then
  begin
    __android_log_write(ANDROID_LOG_FATAL, 'lclapp', 'javaEnvRef^.GetFieldID failed for lcltext');
    Exit(JNI_ERR);
  end;

  // Read all method IDs
  javaMethod_LCLDoGetTextBounds := javaEnvRef^^.GetMethodID(javaEnvRef, javaActivityClass, 'LCLDoGetTextBounds', '()V');
  javaMethod_LCLDoGetTextPartialWidths := javaEnvRef^^.GetMethodID(javaEnvRef, javaActivityClass, 'LCLDoGetTextPartialWidths', '()V');
  javaMethod_LCLDoDrawText := javaEnvRef^^.GetMethodID(javaEnvRef, javaActivityClass, 'LCLDoDrawText', '()V');
  javaMethod_LCLDoShowMessageBox := javaEnvRef^^.GetMethodID(javaEnvRef, javaActivityClass, 'LCLDoShowMessageBox', '()V');
  javaMethod_LCLDoCreateTimer := javaEnvRef^^.GetMethodID(javaEnvRef, javaActivityClass, 'LCLDoCreateTimer', '()V');
  javaMethod_LCLDoDestroyTimer := javaEnvRef^^.GetMethodID(javaEnvRef, javaActivityClass, 'LCLDoDestroyTimer', '()V');
  javaMethod_LCLDoHideVirtualKeyboard := javaEnvRef^^.GetMethodID(javaEnvRef, javaActivityClass, 'LCLDoHideVirtualKeyboard', '()V');
  javaMethod_LCLDoShowVirtualKeyboard := javaEnvRef^^.GetMethodID(javaEnvRef, javaActivityClass, 'LCLDoShowVirtualKeyboard', '()V');
  javaMethod_LCLDoStartReadingAccelerometer := javaEnvRef^^.GetMethodID(javaEnvRef, javaActivityClass, 'LCLDoStartReadingAccelerometer', '()V');
  javaMethod_LCLDoStopReadingAccelerometer := javaEnvRef^^.GetMethodID(javaEnvRef, javaActivityClass, 'LCLDoStopReadingAccelerometer', '()V');
  javaMethod_LCLDoSendMessage := javaEnvRef^^.GetMethodID(javaEnvRef, javaActivityClass, 'LCLDoSendMessage', '()V');
  javaMethod_LCLDoRequestPositionInfo := javaEnvRef^^.GetMethodID(javaEnvRef, javaActivityClass, 'LCLDoRequestPositionInfo', '()V');

  __android_log_write(ANDROID_LOG_INFO, 'lclapp', 'JNI_OnLoad finished');
  result:=JNI_VERSION_1_4;// 1_6 is another option
end;

procedure JNI_OnUnload(vm:PJavaVM;reserved:pointer); cdecl;
begin
end;

procedure TCDWidgetSet.AndroidDebugLn(AStr: string);
begin
  __android_log_write(ANDROID_LOG_INFO, 'lclapp', PChar(AccumulatedStr+AStr));
  AccumulatedStr := '';
end;

function TCDWidgetSet.AndroidKeyCodeToLCLKeyCode(AAndroidKeyCode: Integer): Word;
begin
  case AAndroidKeyCode of
{    AKEYCODE_SOFT_LEFT = 1;
    AKEYCODE_SOFT_RIGHT = 2;
    AKEYCODE_HOME = 3;}
    AKEYCODE_BACK: Result := VK_ESCAPE;
{    AKEYCODE_CALL = 5;
    AKEYCODE_ENDCALL = 6;}
    AKEYCODE_0: Result := VK_0;
    AKEYCODE_1: Result := VK_1;
    AKEYCODE_2: Result := VK_2;
    AKEYCODE_3: Result := VK_3;
    AKEYCODE_4: Result := VK_4;
    AKEYCODE_5: Result := VK_5;
    AKEYCODE_6: Result := VK_6;
    AKEYCODE_7: Result := VK_7;
    AKEYCODE_8: Result := VK_8;
    AKEYCODE_9: Result := VK_9;
{    AKEYCODE_STAR = 17;
    AKEYCODE_POUND = 18;}
    AKEYCODE_DPAD_UP:     Result := VK_UP;
    AKEYCODE_DPAD_DOWN:   Result := VK_DOWN;
    AKEYCODE_DPAD_LEFT:   Result := VK_LEFT;
    AKEYCODE_DPAD_RIGHT:  Result := VK_RIGHT;
    AKEYCODE_DPAD_CENTER: Result := VK_RETURN;
{    AKEYCODE_VOLUME_UP = 24;
    AKEYCODE_VOLUME_DOWN = 25;
    AKEYCODE_POWER = 26;
    AKEYCODE_CAMERA = 27;}
    AKEYCODE_CLEAR: Result := VK_CLEAR;
    AKEYCODE_A: Result := VK_A;
    AKEYCODE_B: Result := VK_B;
    AKEYCODE_C: Result := VK_C;
    AKEYCODE_D: Result := VK_D;
    AKEYCODE_E: Result := VK_E;
    AKEYCODE_F: Result := VK_F;
    AKEYCODE_G: Result := VK_G;
    AKEYCODE_H: Result := VK_H;
    AKEYCODE_I: Result := VK_I;
    AKEYCODE_J: Result := VK_J;
    AKEYCODE_K: Result := VK_K;
    AKEYCODE_L: Result := VK_L;
    AKEYCODE_M: Result := VK_M;
    AKEYCODE_N: Result := VK_N;
    AKEYCODE_O: Result := VK_O;
    AKEYCODE_P: Result := VK_P;
    AKEYCODE_Q: Result := VK_Q;
    AKEYCODE_R: Result := VK_R;
    AKEYCODE_S: Result := VK_S;
    AKEYCODE_T: Result := VK_T;
    AKEYCODE_U: Result := VK_U;
    AKEYCODE_V: Result := VK_V;
    AKEYCODE_W: Result := VK_W;
    AKEYCODE_X: Result := VK_X;
    AKEYCODE_Y: Result := VK_Y;
    AKEYCODE_Z: Result := VK_Z;
    //AKEYCODE_COMMA: Result := VK_?
    //AKEYCODE_PERIOD = 56;
    AKEYCODE_ALT_LEFT: Result := VK_LMENU;
    AKEYCODE_ALT_RIGHT: Result := VK_RMENU;
    AKEYCODE_SHIFT_LEFT: Result := VK_LSHIFT;
    AKEYCODE_SHIFT_RIGHT: Result := VK_RSHIFT;
    AKEYCODE_TAB: Result := VK_TAB;
    AKEYCODE_SPACE: Result := VK_SPACE;
{    AKEYCODE_SYM = 63;
    AKEYCODE_EXPLORER = 64;
    AKEYCODE_ENVELOPE = 65;}
    AKEYCODE_ENTER: Result := VK_RETURN;
    AKEYCODE_DEL: Result := VK_BACK; // The "Backspace" key
{    AKEYCODE_GRAVE = 68;
    AKEYCODE_MINUS = 69;
    AKEYCODE_EQUALS = 70;
    AKEYCODE_LEFT_BRACKET = 71;
    AKEYCODE_RIGHT_BRACKET = 72;
    AKEYCODE_BACKSLASH = 73;
    AKEYCODE_SEMICOLON = 74;
    AKEYCODE_APOSTROPHE = 75;
    AKEYCODE_SLASH = 76;
    AKEYCODE_AT = 77;
    AKEYCODE_NUM = 78;
    AKEYCODE_HEADSETHOOK = 79;
    AKEYCODE_FOCUS = 80;  // *Camera* focus
    AKEYCODE_PLUS = 81;}
    AKEYCODE_MENU: Result := VK_MENU;
{    AKEYCODE_NOTIFICATION = 83;
    AKEYCODE_SEARCH = 84;
    AKEYCODE_MEDIA_PLAY_PAUSE = 85;
    AKEYCODE_MEDIA_STOP = 86;
    AKEYCODE_MEDIA_NEXT = 87;
    AKEYCODE_MEDIA_PREVIOUS = 88;
    AKEYCODE_MEDIA_REWIND = 89;
    AKEYCODE_MEDIA_FAST_FORWARD = 90;
    AKEYCODE_MUTE = 91;}
    AKEYCODE_PAGE_UP: Result := VK_NEXT;
    AKEYCODE_PAGE_DOWN: Result := VK_PRIOR;
{    AKEYCODE_PICTSYMBOLS = 94;
    AKEYCODE_SWITCH_CHARSET = 95;
    AKEYCODE_BUTTON_A = 96;
    AKEYCODE_BUTTON_B = 97;
    AKEYCODE_BUTTON_C = 98;
    AKEYCODE_BUTTON_X = 99;
    AKEYCODE_BUTTON_Y = 100;
    AKEYCODE_BUTTON_Z = 101;
    AKEYCODE_BUTTON_L1 = 102;
    AKEYCODE_BUTTON_R1 = 103;
    AKEYCODE_BUTTON_L2 = 104;
    AKEYCODE_BUTTON_R2 = 105;
    AKEYCODE_BUTTON_THUMBL = 106;
    AKEYCODE_BUTTON_THUMBR = 107;
    AKEYCODE_BUTTON_START = 108;
    AKEYCODE_BUTTON_SELECT = 109;
    AKEYCODE_BUTTON_MODE = 110;}
    // New since API level 11 from android.view.KeyEvent
    AKEYCODE_NUMPAD_0: Result := VK_NUMPAD0;
    AKEYCODE_NUMPAD_1: Result := VK_NUMPAD1;
    AKEYCODE_NUMPAD_2: Result := VK_NUMPAD2;
    AKEYCODE_NUMPAD_3: Result := VK_NUMPAD3;
    AKEYCODE_NUMPAD_4: Result := VK_NUMPAD4;
    AKEYCODE_NUMPAD_5: Result := VK_NUMPAD5;
    AKEYCODE_NUMPAD_6: Result := VK_NUMPAD6;
    AKEYCODE_NUMPAD_7: Result := VK_NUMPAD7;
    AKEYCODE_NUMPAD_8: Result := VK_NUMPAD8;
    AKEYCODE_NUMPAD_9: Result := VK_NUMPAD9;
    AKEYCODE_NUMPAD_ADD: Result := VK_ADD;
    {AKEYCODE_NUMPAD_COMMA = $0000009f;
    AKEYCODE_NUMPAD_DIVIDE = $0000009a;
    AKEYCODE_NUMPAD_DOT = $0000009e;
    AKEYCODE_NUMPAD_ENTER = $000000a0;
    AKEYCODE_NUMPAD_EQUALS = $000000a1;
    AKEYCODE_NUMPAD_LEFT_PAREN = $000000a2;
    AKEYCODE_NUMPAD_MULTIPLY = $0000009b;
    AKEYCODE_NUMPAD_RIGHT_PAREN = $000000a3;
    AKEYCODE_NUMPAD_SUBTRACT = $0000009c;}
    AKEYCODE_NUM_LOCK: Result := VK_NUMLOCK;
  else
    Result := 0;
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
  // Setup DebugLn
  DebugLnProc := @AndroidDebugLn;
  DebugOutProc := @AccumulatingDebugOut;

  {$ifdef CD_UseNativeText}
  // Create the dummy screen DC
  ScreenBitmapRawImage.Init;
  ScreenBitmapHeight := 100;
  ScreenBitmapWidth := 100;
  ScreenBitmapRawImage.Description.Init_BPP32_A8R8G8B8_BIO_TTB(ScreenBitmapWidth, ScreenBitmapHeight);
  ScreenBitmapRawImage.CreateData(True);
  ScreenImage := TLazIntfImage.Create(0, 0);
  ScreenImage.SetRawImage(ScreenBitmapRawImage);
  ScreenDC := TLazCanvas.Create(ScreenImage);

  DefaultFontAndroidSize := 16;
  {$endif}
end;

{------------------------------------------------------------------------------
  Method: TWinCEWidgetSet.Destroy
  Params:  None
  Returns: Nothing

  destructor for the class.
 ------------------------------------------------------------------------------}
procedure TCDWidgetSet.BackendDestroy;
begin
  {$ifdef CD_UseNativeText}
  // Free the dummy screen DC
  ScreenImage.Free;
  ScreenDC.Free;
  {$endif}
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

  Forms.MessageBoxFunction := @CDMessageBoxFunction;

  if Application.LayoutAdjustmentPolicy = lapDefault then
    Application.LayoutAdjustmentPolicy := lapAutoAdjustWithoutHorizontalScrolling;
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
var
  lTimer: TCDTimer;
begin
  lTimer := TCDTimer.Create;

  Result := THandle(lTimer);

  lTimer.Interval := Interval;
  lTimer.TimerFunc := TimerFunc;

  // Prepare the input
  javaEnvRef^^.SetIntField(javaEnvRef, javaActivityObject, javaField_lcltimerinterval, Interval);

  // Call the method
  javaEnvRef^^.CallVoidMethod(javaEnvRef, javaActivityObject, javaMethod_LCLDoCreateTimer);

  // Read the output
  lTimer.NativeHandle := PtrInt(javaEnvRef^^.GetObjectField(javaEnvRef, javaActivityObject, javaField_lcltimerid));

  // Add it to our list
  AddTimer(lTimer);
  DebugLn(Format('[TCDWidgetSet.CreateTimer] Result=%x', [PtrInt(Result)]));
end;

{------------------------------------------------------------------------------
  function: DestroyTimer
  Params: TimerHandle
  Returns:
 ------------------------------------------------------------------------------}
function TCDWidgetSet.DestroyTimer(TimerHandle: THandle) : boolean;
var
  lTimer: TCDTimer;
begin
  DebugLn(Format('[TCDWidgetSet.DestroyTimer] TimerHandle=%x', [PtrInt(TimerHandle)]));
  Result := False;

  lTimer := TCDTimer(TimerHandle);

  if lTimer = nil then Exit;

  // Prepare the input
  javaEnvRef^^.SetObjectField(javaEnvRef, javaActivityObject, javaField_lcltimerid, jobject(lTimer.NativeHandle));

  // Call the method
  javaEnvRef^^.CallVoidMethod(javaEnvRef, javaActivityObject, javaMethod_LCLDoDestroyTimer);

  // Remove from the list
  RemoveTimer(lTimer);
  lTimer.Free;

  Result := True;
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

