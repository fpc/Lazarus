{%MainUnit customdrawnint.pas}
{******************************************************************************
                  All CustomDrawn interface support routines
                   Initial Revision  : Sat Jan 17 19:00:00 2004


  !! Keep alphabetical !!

 ******************************************************************************
 Implementation
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

//##apiwiz##sps##   // Do not remove

function TCDWidgetSet.AskUser(const DialogCaption, DialogMessage: string;
  DialogType: LongInt; Buttons: TDialogButtons; HelpCtx: Longint): LongInt;
begin
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
end;*)

(*{------------------------------------------------------------------------------
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

end;

function TCDWidgetSet.IsMobilePlatform: Boolean;
begin
  Result := Application.ApplicationType in [atPDA, atKeyPadDevice, atTV, atMobileEmulator];
end;

{------------------------------------------------------------------------------
  Function: PromptUser
  Params:
  Returns:
 ------------------------------------------------------------------------------}
function TCDWidgetSet.PromptUser(const DialogCaption : string;
                                 const DialogMessage : string;
                                       DialogType    : LongInt;
                                       Buttons       : PLongInt;
                                       ButtonCount   : LongInt;
                                       DefaultIndex  : LongInt;
                                       EscapeResult  : LongInt) : LongInt;
var
  i: Integer;
  Caption: String;
  TaskConfig: TTASKDIALOGCONFIG;
  DialogButtons: PTASKDIALOG_BUTTON;
  State: TApplicationState;
begin
  //TaskDialogIndirect is available in Vista and up, but only if app was built with manifest.
  //The check for the latter is done by checking for ComCtlVersionIE6 (which is set in the manifest)
  //The availability of TaskDialogIndirect does not depend on the status of ThemeServices
  //Issue #0027664
  if (WindowsVersion >= wvVista) and (GetFileVersion(comctl32) >= ComCtlVersionIE6) then
  begin
    FillChar(TaskConfig, SizeOf(TaskConfig), 0);
    TaskConfig.cbSize := SizeOf(TaskConfig);
    // if we skip hwndParent our form will be a root window - with the taskbar item and icon
    // this is unwanted
    if Assigned(Screen.ActiveCustomForm) then
      TaskConfig.hwndParent := Screen.ActiveCustomForm.Handle
    else
    if Assigned(Application.MainForm) then
      TaskConfig.hwndParent := Application.MainFormHandle
    else
      TaskConfig.hwndParent := AppHandle;
    TaskConfig.hInstance := HInstance;
    TaskConfig.dwFlags := TDF_ALLOW_DIALOG_CANCELLATION;
    if DialogCaption <> '' then
      Caption := DialogCaption
    else
    case DialogType of
      idDialogConfirm,
      idDialogInfo,
      idDialogWarning,
      idDialogError: Caption := GetDialogCaption(DialogType);
    else
      Caption := Application.Title;
    end;
    TaskConfig.pszWindowTitle := PWideChar(UTF8ToUTF16(Caption));

    case DialogType of
      idDialogConfirm:
        begin
          TaskConfig.hMainIcon := Windows.LoadIcon(0, IDI_QUESTION);
          TaskConfig.dwFlags := TaskConfig.dwFlags or TDF_USE_HICON_MAIN;
        end;
      idDialogInfo: TaskConfig.pszMainIcon := TD_INFORMATION_ICON;
      idDialogWarning: TaskConfig.pszMainIcon := TD_WARNING_ICON;
      idDialogError: TaskConfig.pszMainIcon := TD_ERROR_ICON;
      idDialogShield: TaskConfig.pszMainIcon := TD_SHIELD_ICON;
    else
      TaskConfig.dwFlags := TaskConfig.dwFlags or TDF_USE_HICON_MAIN;
    end;

    TaskConfig.pszContent := PWideChar(UTF8ToUTF16(DialogMessage));

    TaskConfig.cButtons := ButtonCount;
    GetMem(DialogButtons, SizeOf(TTASKDIALOG_BUTTON) * ButtonCount);
    for i := 0 to ButtonCount - 1 do
    begin
      DialogButtons[i].nButtonID := Buttons[i];
      DialogButtons[i].pszButtonText := UTF8StringToPWideChar(GetButtonCaption(Buttons[i]));
    end;
    TaskConfig.pButtons := DialogButtons;
    //we need idButtonXX value
    if DefaultIndex < ButtonCount then
      TaskConfig.nDefaultButton := Buttons[DefaultIndex]
    else
      TaskConfig.nDefaultButton := 0;

    State := SaveApplicationState;
    try
      Result := IDCANCEL;
      TaskDialogIndirect(@TaskConfig, @Result, nil, nil);
      if Result = IDCANCEL then
        Result := EscapeResult;
    finally
      RestoreApplicationState(State);
      for i := 0 to ButtonCount - 1 do
        FreeMem(DialogButtons[i].pszButtonText);
      FreeMem(DialogButtons);
    end;
  end
  else
    Result := inherited PromptUser(DialogCaption, DialogMessage, DialogType,
      Buttons, ButtonCount, DefaultIndex, EscapeResult);
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
  Result := True;

  ARawImage.Init;
end;

procedure TCDWidgetset.ShowVirtualKeyboard();
begin
end;

//##apiwiz##eps##   // Do not remove, no wizard declaration after this line
