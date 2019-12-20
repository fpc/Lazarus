{%MainUnit carbonint.pas}

{******************************************************************************
  All Carbon interface communication implementations.
  This are the implementation of the overrides of the Carbon Interface for the
  methods defined in the
  lcl/include/lclintf.inc


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

{------------------------------------------------------------------------------
  Method:  CreateStandardCursor
  Params:  ACursor - Cursor type
  Returns: Cursor object in Carbon for the specified cursor type
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.CreateStandardCursor(ACursor: SmallInt): hCursor;
var
  AThemeCursor: ThemeCursor;
begin
  {$IFDEF VerboseLCLIntf}
    DebugLn('TCarbonWidgetSet.CreateStandardCursor ACursor: ' + DbgS(ACursor));
  {$ENDIF}
  
  Result := 0;
  if (ACursor >= crLow) and (ACursor <= crHigh) then
  begin
    AThemeCursor := CursorToThemeCursor[TCursor(ACursor)];
    if AThemeCursor <> kThemeUndefCursor then
      Result := hCursor(TCarbonCursor.CreateThemed(AThemeCursor,
        TCursor(ACursor) = crDefault));
  end;
  
  {$IFDEF VerboseLCLIntf}
    DebugLn('TCarbonWidgetSet.CreateStandardCursor Result: ' + DbgS(Result));
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  Method:  DrawGrid
  Params:  DC     - Handle to device context
           R      - Grid rectangle
           DX, DY - Grid cell width and height

  Draws the point grid
 ------------------------------------------------------------------------------}
procedure TCarbonWidgetSet.DrawGrid(DC: HDC; const R: TRect; DX, DY: Integer);
begin
  {$IFDEF VerboseLCLIntf}
    DebugLn('TCarbonWidgetSet.DrawGrid Rect: ' + DbgS(R));
  {$ENDIF}
  
  if not CheckDC(DC, 'DrawGrid') then Exit;

  TCarbonDeviceContext(DC).DrawGrid(R, DX, DY);
end;

function TCarbonWidgetSet.GetAcceleratorString(const AVKey: Byte;
  const AShiftState: TShiftState): String;
begin
  Result:=inherited GetAcceleratorString(AVKey, AShiftState);
end;

function TCarbonWidgetSet.GetControlConstraints(Constraints: TObject): boolean;
begin
  Result:=inherited GetControlConstraints(Constraints);
end;

{------------------------------------------------------------------------------
  Method:  GetDesignerDC
  Params:  WindowHandle - Handle of window
  Returns: Device context for window designer
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.GetDesignerDC(WindowHandle: HWND): HDC;
begin
  Result := 0;
  {$IFDEF VerboseLCLIntf}
    DebugLn('TCarbonWidgetSet.GetDesignerDC Handle: ' + DbgS(WindowHandle));
  {$ENDIF}

  if not CheckWidget(WindowHandle, 'GetDesignerDC', TCarbonDesignWindow) then Exit;
  Result := HDC(TCarbonDesignWindow(WindowHandle).GetDesignContext);
end;

{------------------------------------------------------------------------------
  Method:  GetLCLOwnerObject
  Params:  Handle - Handle of window
  Returns: LCL control which has the specified widget
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.GetLCLOwnerObject(Handle: HWnd): TObject;
begin
  {$IFDEF VerboseLCLIntf}
    DebugLn('TCarbonWidgetSet.GetLCLOwnerObject Handle: ' + DbgS(Handle));
  {$ENDIF}
  
  Result := nil;
  if not CheckWidget(Handle, 'GetLCLOwnerObject') then Exit;
  
  Result := TCarbonWidget(Handle).LCLObject;
  
  {$IFDEF VerboseLCLIntf}
    DebugLn('TCarbonWidgetSet.GetLCLOwnerObject Result: ' + DbgS(Result));
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  Method:  IsDesignerDC
  Params:  WindowHandle - Handle of window
           DC           - Handle of device context
  Returns: If the device context is designer
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.IsDesignerDC(WindowHandle: HWND; DC: HDC): Boolean;
begin
  Result := False;
  {$IFDEF VerboseLCLIntf}
    DebugLn('TCarbonWidgetSet.IsDesignerDC Handle: ' + DbgS(WindowHandle), ' DC: ' + DbgS(DC));
  {$ENDIF}

  if not CheckWidget(WindowHandle, 'IsDesignerDC', TCarbonDesignWindow) then Exit;
  Result := DC = HDC(TCarbonDesignWindow(WindowHandle).GetDesignContext);
end;

{------------------------------------------------------------------------------
  Method:  PromptUser
  Params:  DialogCaption - Dialog caption
           DialogMessage - Dialog message text
           DialogType    - Type of dialog
           Buttons       - Pointer to button types
           ButtonCount   - Count of passed buttons
           DefaultIndex  - Index of default button
           EscapeResult  - Result value of escape
  Returns: The result value of pushed button
  
  Shows modal dialog with the specified caption, message and buttons and prompts
  user to push one.
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.PromptUser(const DialogCaption : string;
                                     const DialogMessage : string;
                                           DialogType    : LongInt;
                                           Buttons       : PLongInt;
                                           ButtonCount   : LongInt;
                                           DefaultIndex  : LongInt;
                                           EscapeResult  : LongInt) : LongInt;
 {Implements MessageDlg.
  Carbon's standard alert box only supports 3 buttons (plus optional
  help button).
  Note that alert's help button is not supported at this time since no help context
  is passed to this method.}
  
  // returns first found button ID from passed buttons in dialog buttons
  function FindButton(AButtons: Array of LongInt): LongInt;
  var
    I, J: Integer;
  begin
    for I := Low(AButtons) to High(AButtons) do
      for J := 0 to ButtonCount -1 do
      begin
        if AButtons[I] = Buttons[J] then
        begin
          Result := AButtons[I];
          Exit;
        end;
      end;
  end;
  
  function tr(TranslatedStr: string): string;
  begin
    Result:=TranslatedStr;
    DeleteAmpersands(Result);
  end;

{ Note: Not using Pointer(kAlertDefaultOKText) or Pointer(kAlertDefaultCancelText)
  since this just passes in -1, which tells button to use its normal text and
  we need to override with Yes and No. If Localizable.strings file is in app 
  bundle's .lproj folder, will use localized strings for above keys if they
  are defined in .strings file.}
var
  ParamRec    : AlertStdCFStringAlertParamRec;
  CFString    : CFStringRef;
  ButtonID    : Integer;
  RightBtnID  : LongInt;
  MiddleBtnID : LongInt;
  LeftBtnID   : LongInt;
  CaptionStr  : CFStringRef;
  MessageStr  : CFStringRef;
  AlertCode   : AlertType;
  AlertRef    : DialogRef;
  AlertBtnIdx : DialogItemIndex;
  I: Integer;
  BtnCaption: String;

const SName = 'PromptUser';
begin
  {$IFDEF VerboseLCLIntf}
    DebugLn('TCarbonWidgetSet.PromptUser DialogCaption: ' + DialogCaption +
      ' DialogMessage: ' + DialogMessage + ' DialogType: ' + DbgS(DialogType) +
      ' ButtonCount: ' + DbgS(ButtonCount) + ' DefaultIndex: ' +
      DbgS(DefaultIndex) + ' EscapeResult: ' + DbgS(EscapeResult));
  {$ENDIF}
  
  Result := -1;
  
  if (ButtonCount > 4) or ((ButtonCount = 4) and
    not (FindButton([idButtonHelp]) > 0)) then
  begin
    // if the button count is bigger than 3 + help button we can not use
    // native alert
    
    {$IFDEF VerboseLCLIntf}
      DebugLn('TCarbonWidgetSet.PromptUser Use LCL standard one.');
    {$ENDIF}
    
    Result := inherited;

    {$IFDEF VerboseLCLIntf}
      DebugLn('TCarbonWidgetSet.PromptUser LCL Result: ' + DbgS(Result));
    {$ENDIF}
    
    Exit;
  end;

  {Initialize record}
  ParamRec.version := kStdCFStringAlertVersionOne;
  ParamRec.movable := True;
  ParamRec.helpButton := False;
  ParamRec.defaultText := nil;
  ParamRec.cancelText := nil;
  ParamRec.otherText := nil;
  ParamRec.defaultButton := kAlertStdAlertOKButton;  {Right button}
  ParamRec.cancelButton := kAlertStdAlertCancelButton;
  ParamRec.position := kWindowDefaultPosition;
  ParamRec.flags := 0; 

  {English defaults to use if no Localizable.strings translations to use}
  {Convert LCL "id" button values to Carbon values}

  ButtonID := 0;
  for I := 0 to ButtonCount - 1 do
  begin
    if Buttons[I] = idButtonHelp then ParamRec.helpButton := True
    else
    begin
      BtnCaption:=tr(GetButtonCaption(Buttons[i]));
      if BtnCaption='' then
        continue;

      CreateCFString(BtnCaption, CFString);
      try
        case ButtonID of
          0: // set right button caption and result
            begin
              ParamRec.defaultText := CFCopyLocalizedString(CFString, nil);
              RightBtnID := Buttons[I];
            end;
          1: // set middle button caption and result
            begin
              ParamRec.cancelText := CFCopyLocalizedString(CFString, nil);
              MiddleBtnID := Buttons[I];
            end;
          2: // set left button caption and result
            begin
              ParamRec.otherText := CFCopyLocalizedString(CFString, nil);
              LeftBtnID := Buttons[I];
              
              // set cancel to left button if exists
              ParamRec.cancelButton := kAlertStdAlertOtherButton;
            end;
        end;
      finally
        FreeCFString(CFString);
      end;
      
      Inc(ButtonID);
    end;
  end;

  CreateCFString(DialogCaption, CaptionStr);
  CreateCFString(DialogMessage, MessageStr);

  {Note: kAlertCautionAlert displays alert icon and app's icon.
         kAlertStopAlert and kAlertNoteAlert only display app's icon.
         kAlertPlainAlert doesn't display any icon.}
  case DialogType of
    idDialogWarning : AlertCode := kAlertCautionAlert;
    idDialogError   : AlertCode := kAlertCautionAlert;
    idDialogInfo    : AlertCode := kAlertNoteAlert;
    idDialogConfirm : AlertCode := kAlertNoteAlert;
  else
    AlertCode := kAlertNoteAlert;
  end;

  try
    if OSError(CreateStandardAlert(AlertCode, CaptionStr, MessageStr, @ParamRec, AlertRef{%H-}),
      Self, SName, 'CreateStandardAlert') then Exit;
      
    if OSError(RunStandardAlert(AlertRef, nil, AlertBtnIdx{%H-}), Self, SName,
      'RunStandardAlert') then Exit;

    {Convert Carbon result to LCL "id" dialog result}
    case AlertBtnIdx of
      kAlertStdAlertOKButton     : Result := RightBtnID;
      kAlertStdAlertCancelButton : Result := MiddleBtnID;
      kAlertStdAlertOtherButton  : Result := LeftBtnID;
    end;

  finally
    FreeCFString(ParamRec.defaultText);
    FreeCFString(ParamRec.cancelText);
    FreeCFString(ParamRec.otherText);
    FreeCFString(CaptionStr);
    FreeCFString(MessageStr);
  end;
  
  {$IFDEF VerboseLCLIntf}
    DebugLn('TCarbonWidgetSet.PromptUser Result: ' + DbgS(Result));
  {$ENDIF}
end;  {TCarbonWidgetSet.PromptUser}

{------------------------------------------------------------------------------
  Function: RawImage_CreateBitmaps
  Params: ARawImage: Source raw image
          ABitmap:   Destination bitmap object
          AMask:     Destination mask object
          ASkipMask: When set, no mask is created
  Returns:

 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.RawImage_CreateBitmaps(const ARawImage: TRawImage; out ABitmap, AMask: HBitmap; ASkipMask: Boolean): Boolean;
const
  ALIGNMAP: array[TRawImageLineEnd] of TCarbonBitmapAlignment = (cbaByte, cbaByte, cbaWord, cbaDWord, cbaQWord, cbaDQWord);
var
  ADesc: TRawImageDescription absolute ARawImage.Description;
  bmpType: TCarbonBitmapType;
begin
  Result := RawImage_DescriptionToBitmapType(ADesc, bmpType);
  if not Result then begin
    debugln(['TCarbonWidgetSet.RawImage_CreateBitmaps TODO Depth=',ADesc.Depth,' alphaprec=',ADesc.AlphaPrec,' byteorder=',ord(ADesc.ByteOrder),' alpha=',ADesc.AlphaShift,' red=',ADesc.RedShift,' green=',adesc.GreenShift,' blue=',adesc.BlueShift]);
    exit;
  end;
  ABitmap := HBITMAP(TCarbonBitmap.Create(ADesc.Width, ADesc.Height, ADesc.Depth, ADesc.BitsPerPixel, ALIGNMAP[ADesc.LineEnd], bmpType, ARawImage.Data));

  if ASkipMask or (ADesc.MaskBitsPerPixel = 0)
  then AMask := 0
  else AMask := HBITMAP(TCarbonBitmap.Create(ADesc.Width, ADesc.Height, 1, ADesc.MaskBitsPerPixel, ALIGNMAP[ADesc.MaskLineEnd], cbtMask, ARawImage.Mask));

  Result := True;
end;

{------------------------------------------------------------------------------
  Function: RawImage_DescriptionFromBitmap
  Params: ABitmap:
          ADesc:
  Returns:

  Describes the inner format utilized by Carbon and specific information
  for the specified bitmap
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.RawImage_DescriptionFromBitmap(ABitmap: HBITMAP; out ADesc: TRawImageDescription): Boolean;
begin
  if CheckBitmap(ABitmap, 'RawImage_DescriptionFromBitmap')
  then Result := RawImage_DescriptionFromCarbonBitmap(ADesc, TCarbonBitmap(ABitmap))
  else Result := False;
end;


{------------------------------------------------------------------------------
  Function: RawImage_DescriptionFromDevice
  Params: ADC:   - Handle to device context
          ADesc: - Pointer to raw image description
  Returns: True if success

  Retrieves the standard image format utilized by Carbon
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.RawImage_DescriptionFromDevice(ADC: HDC; out ADesc: TRawImageDescription): Boolean;
var
  P: TPoint;
begin
  Result := False;

  FillStandardDescription(ADesc);
  if (ADC <> 0) and CheckDC(ADC, 'RawImage_DescriptionFromDevice')
  then begin
    P := TCarbonDeviceContext(ADC).Size;
    ADesc.Width := P.X;
    ADesc.Height := P.Y
  end;

  Result := True;
end;

{------------------------------------------------------------------------------
  Function: RawImage_FromBitmap
  Params: ARawImage: Image to create
          ABitmap:   Source bitmap
          AMask:     Source mask
          ARect:     Source rect (TODO)
  Returns: True if the function succeeds

  Creates a raw image from the specified bitmap
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.RawImage_FromBitmap(out ARawImage: TRawImage; ABitmap, AMask: HBITMAP; ARect: PRect = nil): Boolean;
begin
  if CheckBitmap(ABitmap, 'RawImage_FromBitmap')
  and ((AMask = 0) or CheckBitmap(AMask, 'RawImage_FromBitmap (mask)'))
  then Result := RawImage_FromCarbonBitmap(ARawImage, TCarbonBitmap(ABitmap), TCarbonBitmap(AMask), ARect)
  else Result := False;
end;

{------------------------------------------------------------------------------
  Function: RawImage_FromDevice
  Params: ARawImage: Image to create
          ADC:       Source dc
          ARect:     Source rect (TODO)

  This function is utilized when the function TBitmap.LoadFromDevice is called

  The main use for this function is to get a screenshot.

  MWE: exept for the desktop, there is always a bitmep selected in the DC.
       So get this internal bitmap and pass it to RawImage_FromBitmap
       
 The ScreenShot getting code uses OpenGL to get a CGImageRef.
 
 The only way to access the bytes of a CGImageRef is by drawing it to a canvas
 and then reading the data from the canvas. In doing it we can choose the pixel
 format for the canvas, so we choose a convenient one: ARGB, 32-bits depth,
 just like the standard image description.

 See also: Technical Q&A QA1509 - Getting the pixel data from a CGImage object

 http://developer.apple.com/qa/qa2007/qa1509.html
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.RawImage_FromDevice(out ARawImage: TRawImage; ADC: HDC; const ARect: TRect): Boolean;
var
  CBC: TCarbonBitmapContext absolute ADC;
  displayID: CGDirectDisplayID;
  ScreenImage: CGImageRef;
begin
  Result := False;

  // Verifies if we are getting the rawimage from a normal DC as opposed to a
  // desktop DC
  if CheckDC(ADC, 'RawImage_FromDevice') and (CBC is TCarbonBitmapContext) then
  begin
    Result := RawImage_FromCarbonBitmap(ARawImage, CBC.Bitmap, nil, @ARect);
    Exit;
  end;

  { Screenshot taking code starts here }

  { Get's a screenshot }
  displayID := CGMainDisplayID();
  ScreenImage := grabViaOpenGL(displayID, CGDisplayBounds(displayID));
  //dataProvider := CGImageGetDataProvider(ScreenImage);

  { Fills the image description }
  ARawImage.Init;
  FillStandardDescription(ARawImage.Description);
  ARawImage.Description.Height := CGImageGetHeight(ScreenImage);
  ARawImage.Description.Width := CGImageGetWidth(ScreenImage);
  ARawImage.Mask := nil;

  { Copies the image data to a local buffer }
  ARawImage.Data := GetImagePixelData(ScreenImage, ARawImage.DataSize);

  { clean-up }
  CGImageRelease(ScreenImage);

  Result := True;
end;

{------------------------------------------------------------------------------
  Function: RawImage_QueryDescription
  Params: AFlags:
          ADesc:
  Returns:

 ------------------------------------------------------------------------------}
//function TCarbonWidgetSet.RawImage_QueryDescription(AFlags: TRawImageQueryFlags; var ADesc: TRawImageDescription): Boolean;
//begin
//  // override only when queried formats are different from screen description
//end;

{------------------------------------------------------------------------------
  Method:  ReleaseDesignerDC
  Params:  Window - handle of window
           DC     - handle of designer device context
  Returns: 1 if the device context was released or 0 if it wasn't

  Releases a designer device context (DC)
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.ReleaseDesignerDC(Window: HWND; DC: HDC): Integer;
begin
  Result := 0;
  {$IFDEF VerboseLCLIntf}
    DebugLn('TCarbonWidgetSet.ReleaseDesignerDC Handle: ' + DbgS(Window));
  {$ENDIF}

  if not CheckWidget(Window, 'ReleaseDesignerDC', TCarbonDesignWindow) then Exit;
  TCarbonDesignWindow(Window).ReleaseDesignContext;
  Result := 1;
end;

{------------------------------------------------------------------------------
  Method:  SetMainMenuEnabled
  Params:  AEnabled

  Enables/disables main menu
 ------------------------------------------------------------------------------}
procedure TCarbonWidgetSet.SetMainMenuEnabled(AEnabled: Boolean);
begin
  {$IFDEF VerboseLCLIntf}
    DebugLn('TCarbonWidgetSet.SetMainMenuEnabled AEnabled: ' + DbgS(AEnabled));
  {$ENDIF}

  fMenuEnabled:=AEnabled;
  if FMainMenu <> 0 then
  begin
    if csDesigning in TCarbonMenu(FMainMenu).LCLMenuItem.ComponentState then Exit;
    TCarbonMenu(FMainMenu).SetEnable(AEnabled);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidgetSet.SetRootMenu
  Params:  AMenu - Main menu

  Sets the menu to menu bar
 ------------------------------------------------------------------------------}
procedure TCarbonWidgetSet.SetRootMenu(const AMenu: HMENU);
begin
  {$IFDEF VerboseLCLIntf}
    DebugLn('TCarbonWidgetSet.SetRootMenu AMenu: ' + DbgS(AMenu));
  {$ENDIF}
  if (AMenu <> 0) and CheckMenu(AMenu, 'SetRootMenu') and 
     not (csDesigning in TCarbonMenu(AMenu).LCLMenuItem.ComponentState) then    
  begin
    TCarbonMenu(AMenu).AttachToMenuBar;    
    FMainMenu := AMenu;
    SetMainMenuEnabled(MenuEnabled);
  end;
end;

{------------------------------------------------------------------------------
  Method:  SetCaptureWidget
  Params:  AWidget - Carbon widget to capture

  Sets captured Carbon widget
 ------------------------------------------------------------------------------}
procedure TCarbonWidgetSet.SetCaptureWidget(const AWidget: HWND);
begin
  {$IFDEF VerboseLCLIntf}
    DebugLn('TCarbonWidgetSet.SetCaptureWidget AWidget: ' + DbgS(AWidget));
  {$ENDIF}
  
  if AWidget <> FCaptureWidget then
  begin
    FCaptureWidget := AWidget;
    
    if FCaptureWidget <> 0 then
      LCLSendCaptureChangedMsg(TCarbonWidget(FCaptureWidget).LCLObject);
  end;
end;

{------------------------------------------------------------------------------
  Method:  SetTextFractional
  Params:  ACanvas - LCL Canvas

  Sets canvas text fractional enabled
 ------------------------------------------------------------------------------}
procedure TCarbonWidgetSet.SetTextFractional(ACanvas: TCanvas; AEnabled: Boolean);
begin
   {$IFDEF VerboseLCLIntf}
    DebugLn('TCarbonWidgetSet.SetTextFractional ACanvas: ' + DbgS(ACanvas) + ' AEnabled: ' + DbgS(AEnabled));
  {$ENDIF}
  
  if not CheckDC(ACanvas.Handle, 'SetTextFractional') then Exit;
  
  TCarbonDeviceContext(ACanvas.Handle).TextFractional := AEnabled;
end;

//##apiwiz##eps##   // Do not remove, no wizard declaration after this line

// included by carbonint.pas
