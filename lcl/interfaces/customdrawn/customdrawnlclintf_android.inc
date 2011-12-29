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

procedure TCDWidgetSet.LazDeviceAPIs_RequestPositionInfo(AMethod: TLazPositionMethod);
begin
  // Call the method
  javaEnvRef^^.CallVoidMethod(javaEnvRef, javaActivityObject, javaMethod_LCLDoRequestPositionInfo);
end;

procedure TCDWidgetSet.LazDeviceAPIs_SendMessage(AMsg: TLazDeviceMessage);
var
  lJavaString: jstring;
  lStr: String;
begin
  // Prepare the input
  // String fields
  lStr := AMsg.Body;
  lJavaString :=javaEnvRef^^.NewStringUTF(javaEnvRef, PChar(lStr));
  javaEnvRef^^.SetObjectField(javaEnvRef, javaActivityObject, JavaField_lcltext, lJavaString);
  lStr := AMsg.destinationAddress.Text;
  lJavaString :=javaEnvRef^^.NewStringUTF(javaEnvRef, PChar(lStr));
  javaEnvRef^^.SetObjectField(javaEnvRef, javaActivityObject, JavaField_lcldestination, lJavaString);
  // Message type
  javaEnvRef^^.SetIntField(javaEnvRef, javaActivityObject, JavaField_lclkind, 1);
  // Call the method
  javaEnvRef^^.CallVoidMethod(javaEnvRef, javaActivityObject, javaMethod_LCLDoSendMessage);
end;

procedure TCDWidgetSet.LazDeviceAPIs_StartReadingAccelerometerData();
begin
  // Call the method
  javaEnvRef^^.CallVoidMethod(javaEnvRef, javaActivityObject, javaMethod_LCLDoStartReadingAccelerometer);
end;

procedure TCDWidgetSet.LazDeviceAPIs_StopReadingAccelerometerData();
begin
  // Call the method
  javaEnvRef^^.CallVoidMethod(javaEnvRef, javaActivityObject, javaMethod_LCLDoStopReadingAccelerometer);
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

procedure TCDWidgetSet.ShowVirtualKeyboard;
begin
  DebugLn('[TCDWidgetSet.ShowVirtualKeyboard]');
  // Call the method
  javaEnvRef^^.CallVoidMethod(javaEnvRef, javaActivityObject, javaMethod_LCLDoShowVirtualKeyboard);
end;

//##apiwiz##eps##   // Do not remove, no wizard declaration after this line
