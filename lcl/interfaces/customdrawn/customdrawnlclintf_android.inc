{%MainUnit customdrawnint.pas}
{******************************************************************************
                  All CustomDrawn interface support routines
                   Initial Revision  : Sat Jan 17 19:00:00 2004

  !! Keep alphabetical !!

 ******************************************************************************
 Implementation
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

//##apiwiz##sps##   // Do not remove

function TCDWidgetSet.AskUser(const DialogCaption, DialogMessage: string; DialogType:
   LongInt; Buttons: TDialogButtons; HelpCtx: Longint): LongInt;
begin
  Result := 0;
end;

(*{------------------------------------------------------------------------------
  Function: CreateEmptyRegion
  Params:
  Returns: valid empty region
 ------------------------------------------------------------------------------}
function TQtWidgetSet.CreateEmptyRegion: hRGN;
begin
  Result:= HRGN(TQtRegion.Create(True));
end;

{------------------------------------------------------------------------------
  Function: CreateStandardCursor
  Params:
  Returns:
 ------------------------------------------------------------------------------}
function TQtWidgetSet.CreateStandardCursor(ACursor: SmallInt): HCURSOR;
var
  CursorShape: QtCursorShape;
begin
  Result := 0;
  if ACursor < crLow then Exit;
  if ACursor > crHigh then Exit;

  // TODO: map is better
  case ACursor of
    crNone      : CursorShape := QtBlankCursor;
    crArrow     : CursorShape := QtArrowCursor;
    crCross     : CursorShape := QtCrossCursor;
    crIBeam     : CursorShape := QtIBeamCursor;
    crSizeAll   : CursorShape := QtSizeAllCursor;
    crSizeNESW  : CursorShape := QtSizeBDiagCursor;
    crSizeNS    : CursorShape := QtSizeVerCursor;
    crSizeNWSE  : CursorShape := QtSizeFDiagCursor;
    crSizeWE    : CursorShape := QtSizeHorCursor;
    crSizeNW    : CursorShape := QtSizeFDiagCursor;
    crSizeN     : CursorShape := QtSizeVerCursor;
    crSizeNE    : CursorShape := QtSizeBDiagCursor;
    crSizeW     : CursorShape := QtSizeHorCursor;
    crSizeE     : CursorShape := QtSizeHorCursor;
    crSizeSW    : CursorShape := QtSizeBDiagCursor;
    crSizeS     : CursorShape := QtSizeVerCursor;
    crSizeSE    : CursorShape := QtSizeFDiagCursor;
    crUpArrow   : CursorShape := QtUpArrowCursor;
    crHourGlass : CursorShape := QtWaitCursor;
    crHSplit    : CursorShape := QtSplitHCursor;
    crVSplit    : CursorShape := QtSplitVCursor;
    crNo        : CursorShape := QtForbiddenCursor;
    crAppStart  : CursorShape := QtBusyCursor;
    crHelp      : CursorShape := QtWhatsThisCursor;
    crHandPoint : CursorShape := QtPointingHandCursor;
  else
    CursorShape := QtCursorShape(-1);
  end;
  if CursorShape <> QtCursorShape(-1) then
    Result := HCURSOR(TQtCursor.Create(CursorShape));
end;

function TQtWidgetSet.CreateRubberBand(const ARect: TRect; const ABrush: HBrush): HWND;
begin
  // todo: think of ABrush
  Result := HWND(QRubberBand_create(QRubberBandRectangle));
  QRubberBand_setGeometry(QRubberBandH(Result), @ARect);
  QWidget_show(QRubberBandH(Result));
end;

procedure TQtWidgetSet.DrawDefaultDockImage(AOldRect, ANewRect: TRect; AOperation: TDockImageOperation);
begin
  if FDockImage = nil then
    FDockImage := QRubberBand_create(QRubberBandRectangle);

  QRubberBand_setGeometry(FDockImage, @ANewRect);
  case AOperation of
    disShow: QWidget_show(FDockImage);
    disHide: QWidget_hide(FDockImage);
  end;
end;

procedure TQtWidgetSet.DrawGrid(DC: HDC; const R: TRect; DX, DY: Integer);
var
  QtDC: TQtDeviceContext absolute DC;
  X, Y: Integer;
  W, H: Integer;
begin
  if not IsValidDC(DC) then
    exit;
  QtDC.save;
  try
    W := (R.Right - R.Left - 1) div DX;
    H := (R.Bottom - R.Top - 1) div DY;

    for X := 0 to W do
      for Y := 0 to H do
        QtDC.drawPoint(R.Left + X * DX, R.Top + Y * DY + 1);
  finally
    QtDC.restore;
  end;
end;

procedure TQtWidgetSet.DestroyRubberBand(ARubberBand: HWND);
begin
  QWidget_destroy(QRubberBandH(ARubberBand));
end;*)

(*
{------------------------------------------------------------------------------
  Function: FontIsMonoSpace
  Params:
  Returns:
 ------------------------------------------------------------------------------}
function TQtWidgetSet.FontIsMonoSpace(Font: HFont): Boolean;
var
  QtFontInfo: QFontInfoH;
begin
  Result := IsValidGDIObject(Font);
  if Result then
  begin
    QtFontInfo := QFontInfo_create(TQtFont(Font).FHandle);
    try
  	  Result := QFontInfo_fixedPitch(QtFontInfo);
    finally
	    QFontInfo_destroy(QtFontInfo);
    end;
  end;
end;*)

procedure TCDWidgetSet.HideVirtualKeyboard();
begin
  DebugLn('[TCDWidgetSet.HideVirtualKeyboard]');
  // Call the method
  javaEnvRef^^.CallVoidMethod(javaEnvRef, javaActivityObject, javaMethod_LCLDoHideVirtualKeyboard);
end;

function TCDWidgetSet.IsMobilePlatform: Boolean;
begin
  Result := True;
end;

function TCDWidgetSet.PromptUser(const DialogCaption : string;
                    const DialogMessage : string;
                          DialogType    : LongInt;
                          Buttons       : PLongInt;
                          ButtonCount   : LongInt;
                          DefaultIndex  : LongInt;
                          EscapeResult  : LongInt) : LongInt;
var
  lJavaString: jstring;
  BtnIndex, BtnKind: Integer;
  BtnText: string;
begin
  {$ifdef VerboseCDWinAPI}
    DebugLn(Format('[TCDWidgetSet.PromptUser] javaEnvRef=%x DialogCaption=%s '
      + 'DialogMessage=%s DialogType=%d ButtonCount=%d',
      [PtrInt(javaEnvRef), DialogCaption, DialogMessage, DialogType, ButtonCount]));
  {$endif}

  Result := 0; // The real result goes to Application.OnMessageDialogExecute

  if (javaEnvRef = nil) then Exit;

  // Prepare the input
  // String fields
  lJavaString :=javaEnvRef^^.NewStringUTF(javaEnvRef, PChar(DialogMessage));
  javaEnvRef^^.SetObjectField(javaEnvRef, javaActivityObject, JavaField_lcltext, lJavaString);
  lJavaString :=javaEnvRef^^.NewStringUTF(javaEnvRef, PChar(DialogCaption));
  javaEnvRef^^.SetObjectField(javaEnvRef, javaActivityObject, JavaField_lcltitle, lJavaString);

  // Read the buttons
  javaEnvRef^^.SetIntField(javaEnvRef, javaActivityObject, JavaField_lclbutton1, -1);
  javaEnvRef^^.SetIntField(javaEnvRef, javaActivityObject, JavaField_lclbutton2, -1);
  javaEnvRef^^.SetIntField(javaEnvRef, javaActivityObject, JavaField_lclbutton3, -1);
  for BtnIndex := 0 to ButtonCount - 1 do
  begin
    BtnKind := Buttons[BtnIndex];
    {$ifdef VerboseCDWinAPI}
      DebugLn(Format(':[TCDWidgetSet.PromptUser] BtnKind=%d', [BtnKind]));
    {$endif}
    case BtnKind of
      idButtonOK:      BtnText := RemoveAccelChars(rsMbOK);
      idButtonCancel:  BtnText := RemoveAccelChars(rsMbCancel);
      idButtonAbort:   BtnText := RemoveAccelChars(rsMbAbort);
      idButtonRetry:   BtnText := RemoveAccelChars(rsMbRetry);
      idButtonIgnore:  BtnText := RemoveAccelChars(rsMbIgnore);
      idButtonYes:     BtnText := RemoveAccelChars(rsMbYes);
      idButtonNo:      BtnText := RemoveAccelChars(rsMbNo);
      idButtonAll:     BtnText := RemoveAccelChars(rsMbAll);
      idButtonNoToAll: BtnText := RemoveAccelChars(rsMbNoToAll);
      idButtonYesToAll:BtnText := RemoveAccelChars(rsMbYesToAll);
    end;

    lJavaString :=javaEnvRef^^.NewStringUTF(javaEnvRef, PChar(BtnText));
    case BtnIndex of
      0:
      begin
        javaEnvRef^^.SetObjectField(javaEnvRef, javaActivityObject, JavaField_lclbutton1str, lJavaString);
        javaEnvRef^^.SetIntField(javaEnvRef, javaActivityObject, JavaField_lclbutton1, BtnKind);
      end;
      1:
      begin
        javaEnvRef^^.SetObjectField(javaEnvRef, javaActivityObject, JavaField_lclbutton2str, lJavaString);
        javaEnvRef^^.SetIntField(javaEnvRef, javaActivityObject, JavaField_lclbutton2, BtnKind);
      end;
      2:
      begin
        javaEnvRef^^.SetObjectField(javaEnvRef, javaActivityObject, JavaField_lclbutton3str, lJavaString);
        javaEnvRef^^.SetIntField(javaEnvRef, javaActivityObject, JavaField_lclbutton3, BtnKind);
      end;
    end;
  end;

  // Call the method
  javaEnvRef^^.CallVoidMethod(javaEnvRef, javaActivityObject, javaMethod_LCLDoShowMessageBox);
end;

{------------------------------------------------------------------------------
  Function: RawImage_FromDevice
  Params: ADC:
          ARect:
          ARawImage:
  Returns:

  This function is utilized when the function TBitmap.LoadFromDevice is called

  The main use for this function is to get a screenshot. It may have other uses,
   but this is the only one implemented here.

  MWE: exept for the desktop, there is always a bitmep selected in the DC.
       So get this internal bitmap and pass it to RawImage_FromBitmap
 ------------------------------------------------------------------------------}
function TCDWidgetSet.RawImage_FromDevice(out ARawImage: TRawImage; ADC: HDC; const ARect: TRect): Boolean;
begin
  {$ifdef VerboseCDWinAPI}
    DebugLn('Trace:> [WinAPI GetRawImageFromDevice] SrcDC: ', dbghex(ADC),
     ' SrcWidth: ', dbgs(ARect.Right - ARect.Left),
     ' SrcHeight: ', dbgs(ARect.Bottom - ARect.Top));
  {$endif}

  // todo: copy only passed rectangle

  Result := True;

  ARawImage.Init;
end;

function TCDWidgetset.ShowSelectItemDialog(const AItems: TStrings): Boolean;
var
  javaAndroidAppAlertDialogBuilderClass: JClass = nil;
  javaCharSequence: JClass = nil;
  javaMethod_AlertDialogBuilder_new, javaMethod_AlertDialogBuilder_setSingleChoiceItems,
    javaMethod_AlertDialogBuilder_show: JMethodID;
  JavaField_lclselectitemcallback: JFieldID;
  javaAlertDialog: JObject;
  javaObjectArray: JObjectArray;
  // array for the parameters
  lParams: array[0..2] of JValue;
  lJavaString: JString;
  i: Integer;
begin
  //__android_log_write(ANDROID_LOG_INFO, 'lclapp', '[TCDWidgetset.ShowSelectItemDialog]');

  // First get the classes
  javaAndroidAppAlertDialogBuilderClass := javaEnvRef^^.FindClass(javaEnvRef, 'android/app/AlertDialog$Builder');
  javaCharSequence := javaEnvRef^^.FindClass(javaEnvRef, 'java/lang/CharSequence');
  //__android_log_write(ANDROID_LOG_INFO, 'lclapp', PChar('[TCDWidgetset.ShowSelectItemDialog] 2 javaAndroidAppAlertDialogBuilderClass='+IntToStr(PtrInt(javaAndroidAppAlertDialogBuilderClass))));

  // final AlertDialog.Builder ad = new AlertDialog.Builder(this);
  javaMethod_AlertDialogBuilder_new := javaEnvRef^^.GetMethodID(javaEnvRef, javaAndroidAppAlertDialogBuilderClass, '<init>', '(Landroid/content/Context;)V');
  //__android_log_write(ANDROID_LOG_INFO, 'lclapp', '[TCDWidgetset.ShowSelectItemDialog] 2,5');
  lParams[0].l := javaActivityObject;
  javaAlertDialog := javaEnvRef^^.NewObjectA(javaEnvRef, javaAndroidAppAlertDialogBuilderClass, javaMethod_AlertDialogBuilder_new, @lParams[0]);
  //__android_log_write(ANDROID_LOG_INFO, 'lclapp', '[TCDWidgetset.ShowSelectItemDialog] 3');

  // ad.setSingleChoiceItems( adapter, -1, MyOnClickListener);
  // DialogInterface.OnClickListener lclselectitemcallback;
  // public AlertDialog.Builder setSingleChoiceItems (CharSequence[] items, int checkedItem, DialogInterface.OnClickListener listener)
  JavaField_lclselectitemcallback := javaEnvRef^^.GetFieldID(javaEnvRef, javaActivityClass, 'lclselectitemcallback', 'Landroid/content/DialogInterface$OnClickListener;');
  javaMethod_AlertDialogBuilder_setSingleChoiceItems := javaEnvRef^^.GetMethodID(javaEnvRef, javaAndroidAppAlertDialogBuilderClass, 'setSingleChoiceItems',
    '([Ljava/lang/CharSequence;ILandroid/content/DialogInterface$OnClickListener;)Landroid/app/AlertDialog$Builder;');
  javaObjectArray := javaEnvRef^^.NewObjectArray(javaEnvRef, AItems.Count, javaCharSequence, nil);
  lParams[0].l := javaObjectArray;
  //__android_log_write(ANDROID_LOG_INFO, 'lclapp', PChar('[TCDWidgetset.ShowSelectItemDialog] javaObjectArray='+IntToStr(PtrInt(javaObjectArray))));
  for i := 0 to AItems.Count-1 do
  begin
    lJavaString :=javaEnvRef^^.NewStringUTF(javaEnvRef, PChar(AItems.Strings[i]));
    javaEnvRef^^.SetObjectArrayElement(javaEnvRef, javaObjectArray, i, lJavaString);
    __android_log_write(ANDROID_LOG_INFO, 'lclapp', PChar('[TCDWidgetset.ShowSelectItemDialog] Adding string='+AItems.Strings[i]));
  end;
  lParams[1].i := -1;
  lParams[2].l := javaEnvRef^^.GetObjectField(javaEnvRef, javaActivityClass, JavaField_lclselectitemcallback);
  javaEnvRef^^.CallObjectMethodA(javaEnvRef, javaAlertDialog, javaMethod_AlertDialogBuilder_setSingleChoiceItems, @lParams[0]);

  // ad.show();
  javaMethod_AlertDialogBuilder_show := javaEnvRef^^.GetMethodID(javaEnvRef, javaAndroidAppAlertDialogBuilderClass, 'show', '()Landroid/app/AlertDialog;');
  javaEnvRef^^.CallObjectMethod(javaEnvRef, javaAlertDialog, javaMethod_AlertDialogBuilder_show);

  __android_log_write(ANDROID_LOG_INFO, 'lclapp', '[TCDWidgetset.ShowSelectItemDialog] END');
end;

procedure TCDWidgetSet.ShowVirtualKeyboard;
begin
  DebugLn('[TCDWidgetSet.ShowVirtualKeyboard]');
  // Call the method
  javaEnvRef^^.CallVoidMethod(javaEnvRef, javaActivityObject, javaMethod_LCLDoShowVirtualKeyboard);
end;

//##apiwiz##eps##   // Do not remove, no wizard declaration after this line
