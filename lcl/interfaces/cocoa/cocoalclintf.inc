{%MainUnit cocoaint.pas}

{******************************************************************************
  All Cocoa interface communication implementations.
  This is the implementation of the overrides of the Cocoa Interface for the
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

{------------------------------------------------------------------------------
  Method:  CreateStandardCursor
  Params:  ACursor - Cursor type
  Returns: Cursor object in Cocoa for the specified cursor type
 ------------------------------------------------------------------------------}
function TCocoaWidgetSet.CreateStandardCursor(ACursor: SmallInt): hCursor;
begin
  case ACursor of
    crArrow,
    crAppStart,   // neither LCL nor Cocoa provides "crAppStart" cursor
    crDefault     : Result := HCursor(TCocoaCursor.CreateStandard(NSCursor.arrowCursor));
    crCross       : Result := HCursor(TCocoaCursor.CreateStandard(NSCursor.crosshairCursor));
    crIBeam       : Result := HCursor(TCocoaCursor.CreateStandard(NSCursor.IBeamCursor));
    crSizeNS,
    crVSplit      : Result := HCursor(TCocoaCursor.CreateStandard(NSCursor.resizeUpDownCursor));
    crSizeNESW    : Result := HCursor(TCocoaCursor.CreateFromCustomCursor( AllocCursorFromCursorByDegrees(NSCursor.resizeLeftRightCursor,  45) ));
    crSizeNWSE    : Result := HCursor(TCocoaCursor.CreateFromCustomCursor( AllocCursorFromCursorByDegrees(NSCursor.resizeLeftRightCursor, -45) ));
    crSizeWE,
    crHSplit      : Result := HCursor(TCocoaCursor.CreateStandard(NSCursor.resizeLeftRightCursor));
    crSizeN       : Result := HCursor(TCocoaCursor.CreateStandard(NSCursor.resizeUpCursor));
    crSizeNW      : Result := HCursor(TCocoaCursor.CreateFromCustomCursor( AllocCursorFromCursorByDegrees(NSCursor.resizeLeftCursor, -45) ));
    crSizeSW      : Result := HCursor(TCocoaCursor.CreateFromCustomCursor( AllocCursorFromCursorByDegrees(NSCursor.resizeLeftCursor, 45) ));
    crSizeW       : Result := HCursor(TCocoaCursor.CreateStandard(NSCursor.resizeLeftCursor));
    crSizeNE      : Result := HCursor(TCocoaCursor.CreateFromCustomCursor( AllocCursorFromCursorByDegrees(NSCursor.resizeRightCursor,  45) ));
    crSizeSE      : Result := HCursor(TCocoaCursor.CreateFromCustomCursor( AllocCursorFromCursorByDegrees(NSCursor.resizeRightCursor, -45) ));
    crSizeE       : Result := HCursor(TCocoaCursor.CreateStandard(NSCursor.resizeRightCursor));
    crSizeS       : Result := HCursor(TCocoaCursor.CreateStandard(NSCursor.resizeDownCursor));
    crNo,
    crNoDrop      : Result := HCursor(TCocoaCursor.CreateStandard(NSCursor.operationNotAllowedCursor));
    crHandPoint   : Result := HCursor(TCocoaCursor.CreateStandard(NSCursor.pointingHandCursor));
    //crHourGlass,
    crDrag        : Result := HCursor(TCocoaCursor.CreateStandard(NSCursor.dragCopyCursor));
  else
    // We answer with Result=0 for crHourGlass because Cocoa does not provide any API
    // to set the wait cursor. As a compromise to make cross-platform LCL apps written
    // in Windows/Linux behave as expected without change, we answer 0 here and
    // a non-native wait cursor will be utilized
    Result := 0;
  end;
end;

(*
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

function TCarbonWidgetSet.ExtUTF8Out(DC: HDC; X, Y: Integer; Options: Longint;
  Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;
begin
  Result:=inherited ExtUTF8Out(DC, X, Y, Options, Rect, Str, Count, Dx);
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
*)
{------------------------------------------------------------------------------
  Method:  IsDesignerDC
  Params:  WindowHandle - Handle of window
           DC           - Handle of device context
  Returns: If the device context is designer
 ------------------------------------------------------------------------------}
function TCocoaWidgetSet.IsDesignerDC(WindowHandle: HWND; DC: HDC): Boolean;
begin
  Result := (WindowHandle <> 0) and (DC <> 0) and TCocoaContext(DC).isDesignDC;
end;

procedure TCocoaWidgetSet.SyncClipboard();
begin
  fClipboard.Sync;
end;

type

  { TCocoaAlertCancelAccessoryView }

  TCocoaAlertCancelAccessoryView = objcclass(NSView)
    sheetOfWindow: NSWindow;
    function performKeyEquivalent (theEvent: NSEvent): LCLObjCBoolean; override;
  end;

  TCocoaSheetDelegate = objcclass(NSObject)
  public
    ended: Boolean;
    retCode: NSInteger;
    procedure alertDidEnd(alert:NSAlert; returncode: NSInteger; contextInfo: Pointer); message 'alertDidEnd:::';
  end;


procedure TCocoaSheetDelegate.alertDidEnd(alert:NSAlert;
  returncode: NSInteger; contextInfo: Pointer);
begin
  ended := true;
  retCode := returnCode;
end;

// it's placed as a separate function for an easier code management
function RunSheetAsModal(anAlert: NSAlert; ownerWindow: NSWindow): Integer;
var
  alertDel: TCocoaSheetDelegate;
begin
  alertDel:= TCocoaSheetDelegate.alloc.init;
  try
    anAlert.beginSheetModalForWindow_modalDelegate_didEndSelector_contextInfo(
      ownerWindow, alertDel, ObjCSelector('alertDidEnd:::'), nil);

    while not alertDel.ended do
      CocoaWidgetSet.AppProcessMessages;

    Result := alertDel.retCode;

  finally
    alertDel.release;
  end;

end;

{------------------------------------------------------------------------------
  Func:    CocoaPromptUser
  Params:  DialogCaption - Dialog caption
           DialogMessage - Dialog message text
           DialogType    - Type of dialog
           Buttons       - Pointer to button types
           ButtonCount   - Count of passed buttons
           DefaultIndex  - Index of default button
           EscapeResult  - Result value of escape
           sheetOfWindow - Shows the prompt as a sheet to the specified NSWindow,
                           if nil, the prompt is shown as an application modal dialog
           modalSheet    - (only used if sheetOfWindow is not null).
                           if true, the function doesn't return until the sheet
                           is closed
  Returns: The result value of pushed button

  Shows modal dialog or window sheet with the specified caption, message
  and buttons and prompts user to push one.
 ------------------------------------------------------------------------------}
function CocoaPromptUser(const DialogCaption : string;
                                     const DialogMessage : string;
                                           DialogType    : LongInt;
                                           Buttons       : PLongInt;
                                           ButtonCount   : LongInt;
                                           DefaultIndex  : LongInt;
                                           EscapeResult  : LongInt;
  sheetOfWindow : NSWindow; modalSheet: Boolean) : LongInt;
{Implements MessageDlg.}

const
  ButtonCaption: Array [idButtonOk..idButtonNoToAll] of PChar =
    ('OK', 'Cancel', ''{Help}, 'Yes', 'No', 'Close', 'Abort', 'Retry', 'Ignore',
     'All', 'Yes To All', 'No To All');
var
  anAlert: NSAlert;
  informativeText: NSString;
  messageText: NSString;
  I: Integer;
  aButton: NSButton;
  Str: string;
  cancelAccessory: TCocoaAlertCancelAccessoryView;
  needsCancel: Boolean;
begin
  {Str := 'TCocoaWidgetSet.PromptUser DialogCaption: ' + DialogCaption +
    ' DialogMessage: ' + DialogMessage + ' DialogType: ' + DbgS(DialogType) +
    ' ButtonCount: ' + DbgS(ButtonCount) + ' DefaultIndex: ' +
    DbgS(DefaultIndex) + ' EscapeResult: ' + DbgS(EscapeResult);
  Result := -1;}
  {$IFDEF VerboseLCLIntf}
    DebugLn('TCocoaWidgetSet.PromptUser DialogCaption: ' + DialogCaption +
      ' DialogMessage: ' + DialogMessage + ' DialogType: ' + DbgS(DialogType) +
      ' ButtonCount: ' + DbgS(ButtonCount) + ' DefaultIndex: ' +
      DbgS(DefaultIndex) + ' EscapeResult: ' + DbgS(EscapeResult));
  {$ENDIF}
  Result := -1;
  AnAlert := NSAlert.alloc.init;
  try
    cancelAccessory := nil;
    informativeText := NSStringUtf8(DialogMessage);
    messageText := NSStringUtf8(DialogCaption);
    case DialogType of
      idDialogWarning,
      idDialogError    : anAlert.setAlertStyle(NSCriticalAlertStyle);
      idDialogInfo     : anAlert.setAlertStyle(NSInformationalAlertStyle);
    end;

    try
      anAlert.setInformativeText(informativeText);
      anAlert.setMessageText(messageText);

      needsCancel := True;
      for I := 0 to ButtonCount - 1 do
      begin
        if Buttons[I] = idButtonHelp then
        begin
          anAlert.setShowsHelp(true)
          {$IFDEF VerboseLCLIntf}
          DebugLn('TCocoaWidgetSet.PromptUser Warning: Help button is shown but ignored');
          {$ENDIF}
        end
        else
        begin
          if (Buttons[I] < Low(ButtonCaption)) or (Buttons[I] > High(ButtonCaption)) then
          begin
            DebugLn('TCocoaWidgetSet.PromptUser Invalid button ID: ' + DbgS(Buttons[I]));
            Continue;
          end;

          aButton := anAlert.addButtonWithTitle(NSLocalizedString(NSSTR(ButtonCaption[Buttons[I]])));
          aButton.setKeyEquivalentModifierMask(0);

          if I = DefaultIndex then
            aButton.setKeyEquivalent(NSSTR(#13))
          else if I = 0 then
            // By default, the first button is the default button. If in our
            // case this should not be the case, remove the default status
            // from the first button.
            aButton.setKeyEquivalent(NSSTR(''));

          if Buttons[I]=mrCancel then begin
            needsCancel := False;
            aButton.setKeyEquivalent(NSSTR(#27));
          end;

          aButton.setTag(Buttons[I]);
        end;
      end;

      if needsCancel then begin
        cancelAccessory := TCocoaAlertCancelAccessoryView.alloc.init;
        cancelAccessory.sheetOfWindow := sheetOfWindow;
        cancelAccessory.setBounds(NSZeroRect);
        anAlert.setAccessoryView(cancelAccessory);
      end;

      ApplicationWillShowModal;

      if Assigned(sheetOfWindow) then
      begin
        if not (modalSheet) then
        begin
          anAlert.beginSheetModalForWindow_modalDelegate_didEndSelector_contextInfo(
            sheetOfWindow, nil, nil, nil
          );
          Result := 0;
        end
        else
          Result := RunSheetAsModal(anAlert, sheetOfWindow);
      end
      else
      begin
        ToggleAppMenu(false);
        Result := AnAlert.runModal;
        if Result = NSCancelButton then
          Result := EscapeResult;
        ToggleAppMenu(true); // modal menu doesn't have a window, disabling it
      end;
    finally
      if Assigned(cancelAccessory) then cancelAccessory.release;
      informativeText.release;
      messageText.release;
    end;

  finally
    AnAlert.release;
  end;

  {$IFDEF VerboseLCLIntf}
    DebugLn('TCocoaWidgetSet.PromptUser Result: ' + DbgS(Result));
  {$ENDIF}
end;

{TCocoaWidgetSet.PromptUser}

{ TCocoaAlertCancelAccessoryView }

function TCocoaAlertCancelAccessoryView.performKeyEquivalent(theEvent: NSEvent): LCLObjCBoolean;
begin
  if theEvent.keyCode = kVK_Escape then
  begin
    if Assigned(sheetOfWindow) then
      NSApplication(NSApp).endSheet(window) // use "sheetOfWindow.endSheet(window)" on 10.9+
    else
      NSApplication(NSApp).stopModalWithCode(NSCancelButton);
    Result := True;
  end
  else
    Result := inherited performKeyEquivalent(theEvent);
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
function TCocoaWidgetSet.PromptUser(const DialogCaption, DialogMessage: String;
  DialogType: longint; Buttons: PLongint; ButtonCount, DefaultIndex,
  EscapeResult: Longint): Longint;
begin
  Result := CocoaPromptUser(DialogCaption, DialogMessage, DialogType, Buttons, ButtonCount,
    DefaultIndex, EscapeResult);
end;  {TCocoaWidgetSet.PromptUser}

{------------------------------------------------------------------------------
  Method:  MessageBox
------------------------------------------------------------------------------}
function TCocoaWidgetSet.MessageBox(HWnd: HWND; lpText, lpCaption: PChar;
  uType: Cardinal): Integer;
var
  DialogType : LongInt;
  ButtonsArray : array[0..3] of LongInt;
  ButtonsCount : LongInt;
  DefButtonIndex: LongInt;
  SheetWnd : NSWindow;
begin
  FillChar(ButtonsArray, SizeOf(ButtonsArray), 0);

  if (uType and MB_RETRYCANCEL) = MB_RETRYCANCEL then
  begin
    ButtonsCount := 2;
    ButtonsArray[0] := idButtonRetry;
    ButtonsArray[1] := idButtonCancel;
  end
  else
  if (uType and MB_YESNO) = MB_YESNO then
  begin
    ButtonsCount := 2;
    ButtonsArray[0] := idButtonYes;
    ButtonsArray[1] := idButtonNo;
  end
  else
  if (uType and MB_YESNOCANCEL) = MB_YESNOCANCEL then
  begin
    ButtonsCount := 3;
    ButtonsArray[0] := idButtonYes;
    ButtonsArray[1] := idButtonNo;
    ButtonsArray[2] := idButtonCancel;
  end
  else
  if (uType and MB_ABORTRETRYIGNORE) = MB_ABORTRETRYIGNORE then
  begin
    ButtonsCount := 3;
    ButtonsArray[0] := idButtonAbort;
    ButtonsArray[1] := idButtonRetry;
    ButtonsArray[2] := idButtonIgnore;
  end
  else
  if (uType and MB_OKCANCEL) = MB_OKCANCEL then
  begin
    ButtonsCount := 2;
    ButtonsArray[0] := idButtonOk;
    ButtonsArray[1] := idButtonCancel;
  end
  else
  begin
    ButtonsCount := 1;
    ButtonsArray[0] := idButtonOk;
  end;

  if (uType and MB_ICONINFORMATION) = MB_ICONINFORMATION then
    DialogType := idDialogInfo
  else
  if (uType and MB_ICONWARNING) = MB_ICONWARNING then
    DialogType := idDialogWarning
  else
  if (uType and MB_ICONQUESTION) = MB_ICONQUESTION then
    DialogType := idDialogConfirm
  else
  if (uType and MB_ICONERROR) = MB_ICONERROR then
    DialogType := idDialogError
  else
    DialogType := idDialogInfo;

  if (uType and MB_DEFBUTTON2) = MB_DEFBUTTON2 then
    DefButtonIndex := Pred(2)
  else
  if (uType and MB_DEFBUTTON3) = MB_DEFBUTTON3 then
    DefButtonIndex := Pred(3)
  else
  if (uType and MB_DEFBUTTON4) = MB_DEFBUTTON4 then
    DefButtonIndex := Pred(4)
  else
    DefButtonIndex := Pred(1);

  if HWnd = 0 then
    SheetWnd := Nil
  else
    SheetWnd := NSView(HWnd).window();

  Result := CocoaPromptUser(
    string(lpCaption),
    string(lpText),
    DialogType,
    @ButtonsArray,
    ButtonsCount,
    DefButtonIndex,
    0 {EscapeResult},
    SheetWnd, true);

  case Result of
    idButtonOk : Result := IDOK;
    idButtonNo : Result := IDNO;
    idButtonYes : Result := IDYES;
    idButtonCancel : Result := IDCANCEL;
    idButtonRetry : Result := IDRETRY;
    idButtonAbort : Result := IDABORT;
    idButtonIgnore : Result := IDIGNORE;
  else
    Result := IDCANCEL;
  end;
end;

{------------------------------------------------------------------------------
  Function: RawImage_CreateBitmaps
  Params: ARawImage: Source raw image
          ABitmap:   Destination bitmap object
          AMask:     Destination mask object
          ASkipMask: When set, no mask is created
  Returns:

 ------------------------------------------------------------------------------}
function TCocoaWidgetSet.RawImage_CreateBitmaps(const ARawImage: TRawImage; out ABitmap, AMask: HBitmap; ASkipMask: Boolean): Boolean;
const
  ALIGNMAP: array[TRawImageLineEnd] of TCocoaBitmapAlignment = (cbaByte, cbaByte, cbaWord, cbaDWord, cbaQWord, cbaDQWord);
var
  ADesc: TRawImageDescription absolute ARawImage.Description;
  bmpType: TCocoaBitmapType;
begin
  Result := RawImage_DescriptionToBitmapType(ADesc, bmpType);
  if not Result then begin
    debugln(['TCocoaWidgetSet.RawImage_CreateBitmaps TODO Depth=',ADesc.Depth,' alphaprec=',ADesc.AlphaPrec,' byteorder=',ord(ADesc.ByteOrder),' alpha=',ADesc.AlphaShift,' red=',ADesc.RedShift,' green=',adesc.GreenShift,' blue=',adesc.BlueShift]);
    exit;
  end;
  ABitmap := HBITMAP(TCocoaBitmap.Create(ADesc.Width, ADesc.Height, ADesc.Depth, ADesc.BitsPerPixel, ALIGNMAP[ADesc.LineEnd], bmpType, ARawImage.Data));

  if ASkipMask or (ADesc.MaskBitsPerPixel = 0)
  then AMask := 0
  else AMask := HBITMAP(TCocoaBitmap.Create(ADesc.Width, ADesc.Height, 1, ADesc.MaskBitsPerPixel, ALIGNMAP[ADesc.MaskLineEnd], cbtMask, ARawImage.Mask));

  Result := True;
end;

{------------------------------------------------------------------------------
  Function: RawImage_DescriptionFromBitmap
  Params: ABitmap:
          ADesc:
  Returns:

  Describes the inner format utilized by Cocoa and specific information
  for the specified bitmap
 ------------------------------------------------------------------------------}
function TCocoaWidgetSet.RawImage_DescriptionFromBitmap(ABitmap: HBITMAP; out ADesc: TRawImageDescription): Boolean;
begin
  if CheckBitmap(ABitmap, 'RawImage_DescriptionFromBitmap')
  then Result := RawImage_DescriptionFromCocoaBitmap(ADesc, TCocoaBitmap(ABitmap))
  else Result := False;
end;

{------------------------------------------------------------------------------
  Function: RawImage_DescriptionFromDevice
  Params: ADC:   - Handle to device context
          ADesc: - Pointer to raw image description
  Returns: True if success

  Retrieves the standard image format utilized by Cocoa
 ------------------------------------------------------------------------------}
function TCocoaWidgetSet.RawImage_DescriptionFromDevice(ADC: HDC; out ADesc: TRawImageDescription): Boolean;
begin
  Result := False;

  FillStandardDescription(ADesc);
  if (ADC <> 0) and CheckDC(ADC, 'RawImage_DescriptionFromDevice') then
  begin
    with TCocoaContext(ADC).Size do
    begin
      ADesc.Width := cx;
      ADesc.Height := cy;
    end;
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
function TCocoaWidgetSet.RawImage_FromBitmap(out ARawImage: TRawImage; ABitmap, AMask: HBITMAP; ARect: PRect = nil): Boolean;
begin
  if CheckBitmap(ABitmap, 'RawImage_FromBitmap')
  and ((AMask = 0) or CheckBitmap(AMask, 'RawImage_FromBitmap (mask)'))
  then Result := RawImage_FromCocoaBitmap(ARawImage, TCocoaBitmap(ABitmap), TCocoaBitmap(AMask), ARect)
  else Result := False;
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWidgetSet.GetImagePixelData

  Used by RawImage_FromDevice. Copies the data from a CGImageRef into a local
  buffer.

  The buffer is created using GetMem, and the caller is responsible for using
  FreeMem to free the returned pointer.

  This function throws exceptions in case of errors and may return a nil pointer.
 ------------------------------------------------------------------------------}
function TCocoaWidgetSet.GetImagePixelData(AImage: CGImageRef; out bitmapByteCount: PtrUInt): Pointer;
var
  bitmapData: Pointer;
  context: CGContextRef = nil;
  colorSpace: CGColorSpaceRef;
  bitmapBytesPerRow, pixelsWide, pixelsHigh: PtrUInt;
  imageRect: CGRect;
begin
  Result := nil;

   // Get image width, height. The entire image is used.
  pixelsWide := CGImageGetWidth(AImage);
  pixelsHigh := CGImageGetHeight(AImage);
  imageRect.origin.x := 0.0;
  imageRect.origin.y := 0.0;
  imageRect.size.width := pixelsWide;
  imageRect.size.height := pixelsHigh;

  // The target format is fixed in ARGB, DQWord alignment, with 32-bits depth and
  // 8-bits per channel, the default image format on the LCL
  bitmapBytesPerRow   := ((pixelsWide * 4) + $F) and not PtrUInt($F);
  bitmapByteCount     := (bitmapBytesPerRow * pixelsHigh);

  // Use the generic RGB color space.
  colorSpace := CGColorSpaceCreateWithName(kCGColorSpaceGenericRGB);
  if (colorSpace = nil) then
    raise Exception.Create('Unable to create CGColorSpaceRef');

  // Allocate memory for image data. This is the destination in memory
  // where any drawing to the bitmap context will be rendered.
  bitmapData := System.GetMem( bitmapByteCount );
  if (bitmapData = nil) then
    raise Exception.Create('Unable to allocate memory');

  { Creates the bitmap context.

    Regardless of what the source image format is, it will be converted
    over to the format specified here by CGBitmapContextCreate. }
  context := CGBitmapContextCreate(bitmapData,
                                   pixelsWide,
                                   pixelsHigh,
                                   8,      // bits per component
                                   bitmapBytesPerRow,
                                   colorSpace,
                                   kCGImageAlphaNoneSkipFirst); // The function fails with kCGImageAlphaFirst
  if (context = nil) then
  begin
    System.FreeMem(bitmapData);
    raise Exception.Create('Unable to create CGContextRef');
  end;

  // Draw the image to the bitmap context. Once we draw, the memory
  // allocated for the context for rendering will then contain the
  // raw image data in the specified color space.
  CGContextDrawImage(context, imageRect, AImage);

  // Now we can get a pointer to the image data associated with the context.
  // ToDo: Verify if we should copy this data to a new buffer
  Result := CGBitmapContextGetData(context);

  { Clean-up }
  CGColorSpaceRelease(colorSpace);
  CGContextRelease(context);
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
var
  _CGDisplayCreateImage : function ( displayID: CGDirectDisplayID ): CGImageRef; cdecl = nil;

function CGDisplayCreateImageNone( displayID: CGDirectDisplayID ): CGImageRef; cdecl;
begin
  Result := nil;
end;

function TCocoaWidgetSet.RawImage_FromDevice(out ARawImage: TRawImage; ADC: HDC; const ARect: TRect): Boolean;
var
  CBC: TCocoaBitmapContext absolute ADC;
  Desc: TRawImageDescription absolute ARawImage.Description;
  displayID: CGDirectDisplayID;
  ScreenImage: CGImageRef;
begin
  Result := False;

  // Verifies if we are getting the rawimage from a normal DC as opposed to a
  // desktop DC
  if CheckDC(ADC, 'RawImage_FromDevice') and (CBC is TCocoaBitmapContext) then
  begin
    Result := RawImage_FromCocoaBitmap(ARawImage, CBC.Bitmap, nil, @ARect);
    Exit;
  end;

  { Screenshot taking code starts here }

  { Get's a screenshot }
  displayID := CGMainDisplayID();
  if not Assigned(Pointer(_CGDisplayCreateImage)) then begin
    Pointer(_CGDisplayCreateImage) := GetProcAddress(TLibHandle(RTLD_DEFAULT), 'CGDisplayCreateImage');
    if not Assigned(@_CGDisplayCreateImage) then
      Pointer(_CGDisplayCreateImage) := @CGDisplayCreateImageNone;
  end;
  ScreenImage := _CGDisplayCreateImage(displayID);

  { Fills the image description }
  ARawImage.Init;
  FillStandardDescription(ARawImage.Description);
  if Assigned(ScreenImage) then begin
    ARawImage.Description.Height := CGImageGetHeight(ScreenImage);
    ARawImage.Description.Width := CGImageGetWidth(ScreenImage);
    ARawImage.Data := GetImagePixelData(ScreenImage, ARawImage.DataSize);
  end;
  ARawImage.Mask := nil;

  { Copies the image data to a local buffer }

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
(*
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
*)

