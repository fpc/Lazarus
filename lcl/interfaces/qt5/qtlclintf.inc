{%MainUnit qtint.pp}
{******************************************************************************
                  All Qt5 interface support routines
                   Initial Revision  : Fri Sep 16 10:05:00 2016


  !! Keep alphabetical !!

  Support routines go to qtproc.pp

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

{
  This is the common cdecl callback function used for Qt5
  QSocketNotifier activated.
}

const
  IdButtonToQtStandardButton: array[idButtonOk..idButtonShield] of QMessageBoxStandardButton = (
 { idButtonOk       } QMessageBoxOk,
 { idButtonCancel   } QMessageBoxCancel,
 { idButtonHelp     } QMessageBoxHelp,
 { idButtonYes      } QMessageBoxYes,
 { idButtonNo       } QMessageBoxNo,
 { idButtonClose    } QMessageBoxClose,
 { idButtonAbort    } QMessageBoxAbort,
 { idButtonRetry    } QMessageBoxRetry,
 { idButtonIgnore   } QMessageBoxIgnore,
 { idButtonAll      } QMessageBoxNoButton,
 { idButtonYesToAll } QMessageBoxYesToAll,
 { idButtonNoToAll  } QMessageBoxNoToAll,
 { idButtonOpen     } QMessageBoxOpen,
 { idButtonSave     } QMessageBoxSave,
 { idButtonShield   } QMessageBoxNoButton
  );

procedure TQtWidgetSet.SocketNotifierRead_cb(aSocket: Integer); cdecl;
var
  wheh: PWaitHandleEventHandler;
begin
  if FSocketEventMap.GetData(aSocket, wheh) then
    wheh^.user_callback(wheh^.udata, EVE_IO_READ);
end;

procedure TQtWidgetSet.SocketNotifierWrite_cb(aSocket: Integer); cdecl;
var
  wheh: PWaitHandleEventHandler;
begin
  if FSocketEventMap.GetData(aSocket, wheh) then
    wheh^.user_callback(wheh^.udata, EVE_IO_WRITE);
end;

procedure TQtWidgetSet.SocketNotifierError_cb(aSocket: Integer); cdecl;
var
  wheh: PWaitHandleEventHandler;
begin
  if FSocketEventMap.GetData(aSocket, wheh) then
    wheh^.user_callback(wheh^.udata, EVE_IO_ERROR);
end;

function TQtWidgetSet.AddEventHandler(AHandle: THandle; AFlags: dword;
  AEventHandler: TWaitHandleEvent; AData: PtrInt): PEventHandler;
{
  QSocketNotifier requires 1 notifier per event type
  and doesn't provide userdata in the callback. We need to
  make a map of socket -> userdata to store userdata
  and also create 3 notifiers for each event. We also need to
  use our own constants for the event types in the userland callback.
  For simplicity same as GTK G_IO values are used here and
  their ORs will be emulated. The callback will always only get
  1 event tho.
}

  function CreateQt4NotifierRec(aNR: PWaitHandleEventHandler;
           const aType: QSocketNotifierType; aCallback: QSocketNotifier_activated_Event): PWaitHandleEventHandler;
  var
    qsn: QSocketNotifierH;
    qsn_hook: QSocketNotifier_hookH;
    i: QSocketNotifierType;
  begin
    if aNR = nil then begin
      Result := new(PWaitHandleEventHandler);
      for i := QSocketNotifierRead to QSocketNotifierException do begin
        Result^.qsn[i] := nil; // nil them so removeeventhandler can find out what to free
        Result^.qsn_hook[i] := nil;
      end;
    end else
      Result := aNR;

    qsn := QSocketNotifier_create(aHandle, aType);
    qsn_hook := QSocketNotifier_hook_create(qsn);
    QSocketNotifier_hook_hook_activated(qsn_hook, aCallback); // todo: !!

    Result^.qsn[aType] := qsn;
    Result^.qsn_hook[aType] := qsn_hook;
  end;

begin
  Result := nil;

  if AFlags and (EVE_IO_READ or EVE_IO_WRITE or EVE_IO_ERROR) = 0 then
    Exit; // no flag set, no dice

  if AFlags and EVE_IO_READ = EVE_IO_READ then
    Result := CreateQt4NotifierRec(Result, QSocketNotifierRead, @SocketNotifierRead_cb);

  if AFlags and EVE_IO_WRITE = EVE_IO_WRITE then
    Result := CreateQt4NotifierRec(Result, QSocketNotifierWrite, @SocketNotifierWrite_cb);

  if AFlags and EVE_IO_ERROR = EVE_IO_ERROR then
    Result := CreateQt4NotifierRec(Result, QSocketNotifierException, @SocketNotifierError_cb);

  PWaitHandleEventHandler(Result)^.user_callback := AEventHandler;
  PWaitHandleEventHandler(Result)^.udata := aData;
  PWaitHandleEventHandler(Result)^.socket := AHandle;

  if FSocketEventMap.HasId(aHandle) then begin // if we encounter this (shouldn't happen)
    Debugln('TQtWidgetSet.AddEventHandler Duplicate handle: ' + IntToStr(aHandle));
    FSocketEventMap.Delete(aHandle); // delete the previous one, potentially losing it..
  end;
  FSocketEventMap.Add(AHandle, Result);
end;

function TQtWidgetSet.AddPipeEventHandler(AHandle: THandle;
  AEventHandler: TPipeEvent; AData: PtrInt): PPipeEventHandler;
begin
  // todo
  Result := nil;
end;

function TQtWidgetSet.AddProcessEventHandler(AHandle: THandle;
  AEventHandler: TChildExitEvent; AData: PtrInt): PProcessEventHandler;
begin
  // todo
  Result := nil;
end;

function TQtWidgetSet.AskUser(const DialogCaption, DialogMessage: string;
  DialogType: LongInt; Buttons: TDialogButtons; HelpCtx: Longint): LongInt;
const
  ButtonResults : array[mrNone..mrYesToAll] of Longint = (
    -1, idButtonOK, idButtonCancel, idButtonAbort, idButtonRetry,
    idButtonIgnore, idButtonYes,idButtonNo, idButtonAll, idButtonNoToAll,
    idButtonYesToAll);
var
  BtnIdx, BtnID: LongInt;
  QtMessageBox: TQtMessageBox;
begin
  ReleaseCapture;
  QtMessageBox := TQtMessageBox.Create(nil);
  QtMessageBox.AttachEvents;
  try
    {Convert LCL "id" button values to Qt values}
    for BtnIdx := 0 to Buttons.Count - 1 do
    begin
      with Buttons[BtnIdx] do
      begin
        if (ModalResult >= Low(ButtonResults)) and (ModalResult <= High(ButtonResults)) then
          BtnID := ButtonResults[ModalResult]
        else
          BtnID := -1;
        if (BtnID >= Low(IdButtonToQtStandardButton)) and
           (BtnID <= High(IdButtonToQtStandardButton)) and
           (IdButtonToQtStandardButton[BtnID] <> QMessageBoxNoButton) then
          QtMessageBox.AddButton(UTF8ToUTF16(Caption), IdButtonToQtStandardButton[BtnID], ModalResult, Default, Cancel)
        else
          QtMessageBox.AddButton(UTF8ToUTF16(Caption), ModalResult, Default, Cancel);
      end;
    end;

    if DialogCaption <> '' then
      QtMessageBox.Title := UTF8ToUTF16(DialogCaption)
    else
      case DialogType of
        idDialogWarning: QtMessageBox.Title := UTF8ToUTF16(rsMtWarning);
        idDialogError: QtMessageBox.Title := UTF8ToUTF16(rsMtError);
        idDialogInfo : QtMessageBox.Title := UTF8ToUTF16(rsMtInformation);
        idDialogConfirm : QtMessageBox.Title := UTF8ToUTF16(rsMtConfirmation);
      end;

    QtMessageBox.MessageStr := UTF8ToUTF16(DialogMessage);

    case DialogType of
      idDialogWarning: QtMessageBox.MsgBoxType := QMessageBoxWarning;
      idDialogError: QtMessageBox.MsgBoxType := QMessageBoxCritical;
      idDialogInfo : QtMessageBox.MsgBoxType := QMessageBoxInformation;
      idDialogConfirm : QtMessageBox.MsgBoxType := QMessageBoxQuestion;
      else
        QtMessageBox.MsgBoxType := QMessageBoxNoIcon;
    end;

    Result := QtMessageBox.exec;
  finally
    QtMessageBox.Free;
  end;
end;


{------------------------------------------------------------------------------
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
end;

{------------------------------------------------------------------------------
  Function: FontIsMonoSpace
  Params:
  Returns:
 ------------------------------------------------------------------------------}
function TQtWidgetSet.FontIsMonoSpace(Font: HFont): boolean;
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
end;


function TQtWidgetSet.GetDesignerDC(WindowHandle: HWND): HDC;
var
  Widget: TQtWidget;
begin
  Widget := TQtWidget(WindowHandle);

  if (Widget <> nil) and (Widget is TQtDesignWidget) then
    Result := TQtDesignWidget(Widget).DesignContext
  else
    Result := 0;

  if Result = 0 then
    Result := GetDC(WindowHandle);
end;

function TQtWidgetSet.IsDesignerDC(WindowHandle: HWND; DC: HDC): Boolean;
begin
  Result := (WindowHandle <> 0) and (TQtWidget(WindowHandle) is TQtDesignWidget);
  if Result then
    Result := TQtDesignWidget(WindowHandle).DesignContext = DC;
end;

{------------------------------------------------------------------------------
  Function: PromptUser
  Params:
  Returns:

  Note: Qt appears to map Esc key to Cancel button, so no need for EscapeResult.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.PromptUser(const DialogCaption : string;
                                 const DialogMessage : string;
                                       DialogType    : LongInt;
                                       Buttons       : PLongInt;
                                       ButtonCount   : LongInt;
                                       DefaultIndex  : LongInt;
                                       EscapeResult  : LongInt) : LongInt;
var
  BtnIdx, BtnID: LongInt;
  QtMessageBox: TQtMessageBox;
begin
  ReleaseCapture;
  QtMessageBox := TQtMessageBox.Create(nil);
  QtMessageBox.AttachEvents;
  try
    {Convert LCL "id" button values to Qt values}
    for BtnIdx := 0 to ButtonCount-1 do
    begin
      BtnID := Buttons[BtnIdx];
      if (BtnID >= Low(IdButtonToQtStandardButton)) and
         (BtnID <= High(IdButtonToQtStandardButton)) and
         (IdButtonToQtStandardButton[BtnID] <> QMessageBoxNoButton) then
        QtMessageBox.AddButton(UTF8ToUTF16(GetButtonCaption(BtnID)), IdButtonToQtStandardButton[BtnID], BtnID,
          BtnIdx = DefaultIndex, (EscapeResult = mrCancel) and (BtnId = EscapeResult))
      else
        QtMessageBox.AddButton(UTF8ToUTF16(GetButtonCaption(BtnID)), BtnID, BtnIdx = DefaultIndex,
          (EscapeResult = mrCancel) and (BtnId = EscapeResult));
    end;

    if DialogCaption <> '' then
      QtMessageBox.Title := UTF8ToUTF16(DialogCaption)
    else
      case DialogType of
        idDialogWarning: QtMessageBox.Title := UTF8ToUTF16(rsMtWarning);
        idDialogError: QtMessageBox.Title := UTF8ToUTF16(rsMtError);
        idDialogInfo : QtMessageBox.Title := UTF8ToUTF16(rsMtInformation);
        idDialogConfirm : QtMessageBox.Title := UTF8ToUTF16(rsMtConfirmation);
      end;

    QtMessageBox.MessageStr := UTF8ToUTF16(DialogMessage);

    case DialogType of
      idDialogWarning: QtMessageBox.MsgBoxType := QMessageBoxWarning;
      idDialogError: QtMessageBox.MsgBoxType := QMessageBoxCritical;
      idDialogInfo : QtMessageBox.MsgBoxType := QMessageBoxInformation;
      idDialogConfirm : QtMessageBox.MsgBoxType := QMessageBoxQuestion;
      else
        QtMessageBox.MsgBoxType := QMessageBoxNoIcon;
    end;

    Result := QtMessageBox.exec;
  finally
    QtMessageBox.Free;
  end;
end;  {TQtWidgetSet.PromptUser}

{------------------------------------------------------------------------------
  Function: RadialPie
  Params: DC, PaintRect coordinates, StartAngle16, StopAngle16
  Returns: True if done, False if invalid

 ------------------------------------------------------------------------------}

function TQtWidgetSet.RadialPie(DC: HDC; x1, y1, x2, y2, Angle1, Angle2: Integer): Boolean;
var
  theWidth,theHeight: Integer;
begin
  Result := IsValidDC(DC) and (x2 > x1) and (y2 > y1);
  if Result then begin
    theWidth  := x2 - x1;
    theHeight := y2 - y1;
    QPainter_drawPie(TQtDeviceContext(DC).Widget, x1, y1, theWidth, theHeight, Angle1, Angle2);
  end;
end;

{------------------------------------------------------------------------------
  Function: RawImage_CreateBitmaps
  Params: ARawImage:
          ABitmap:
          AMask:
          ASkipMask: When set, no mask is created
  Returns:

  This functions is for TBitmap support

  The memory allocation code was added because it is necessary for
  TBitmap.LoadFromDevice support. For other operations it isnt needed
 ------------------------------------------------------------------------------}
function TQtWidgetSet.RawImage_CreateBitmaps(const ARawImage: TRawImage; out ABitmap, AMask: HBitmap; ASkipMask: Boolean): Boolean;
var
  Desc: TRawImageDescription absolute ARawImage.Description;
  NewData: PByte;
  ImageFormat: QImageFormat;
begin
  Result := False;
  ABitmap := 0;
  AMask := 0;

  //MWE: shouldn't the memory get copied at the place where it is needed,
  //     its not really oo if the bitmap (or thisr party) needs this to do it here.
  if ARawImage.DataSize > 0 then
  begin
    NewData := GetMem(ARawImage.DataSize);
    Move(ARawImage.Data^, NewData^, ARawImage.DataSize);
  end
  else
    NewData := nil;
    
  // this is only a rough implementation, there is no check against bitsperpixel
  case Desc.Depth of
    1: ImageFormat := QImageFormat_Mono;
    //2..14: ;
    15, 16: ImageFormat := QImageFormat_RGB16;
    24: ImageFormat := QImageFormat_RGB32;
    32: ImageFormat := QImageFormat_ARGB32;
  else
    ImageFormat := QImageFormat_ARGB32;
  end;
  ABitmap := HBitmap(TQtImage.Create(NewData, Desc.Width, Desc.Height, ImageFormat, True));
  Result := ABitmap <> 0;
  
  if ASkipMask then Exit;

  if (ARawImage.Mask <> nil) and (ARawImage.MaskSize > 0) then
  begin
    NewData := GetMem(ARawImage.MaskSize);
    Move(ARawImage.Mask^, NewData^, ARawImage.MaskSize);
  end
  else
    NewData := nil;
    
  AMask := HBitmap(TQtImage.Create(NewData, Desc.Width, Desc.Height, QImageFormat_Mono, True));
end;

{------------------------------------------------------------------------------
  Function: RawImage_DescriptionFromBitmap
  Params: ABitmap:
          ADesc:
  Returns:

  Describes the inner format utilized by Qt + the specific information for this image
 ------------------------------------------------------------------------------}
function TQtWidgetSet.RawImage_DescriptionFromBitmap(ABitmap: HBITMAP; out ADesc: TRawImageDescription): Boolean;
const
  QImageFormatToDepth: array[QImageFormat] of integer =
  (
 { QImageFormat_Invalid              } 0,
 { QImageFormat_Mono                 } 1,
 { QImageFormat_MonoLSB              } 1,
 { QImageFormat_Indexed8             } 8,
 { QImageFormat_RGB32                } 24,
 { QImageFormat_ARGB32               } 32,
 { QImageFormat_ARGB32_Premultiplied } 32,
 { QImageFormat_RGB16                } 16,
  { QImageFormat_ARGB8565_Premultiplied         } 24,
  { QImageFormat_RGB666                         } 24,
  { QImageFormat_ARGB6666_Premultiplied         } 24,
  { QImageFormat_RGB555                         } 16,
  { QImageFormat_ARGB8555_Premultiplied         } 24,
  { QImageFormat_RGB888                         } 24,
  { QImageFormat_RGB444                         } 16,
  { QImageFormat_ARGB4444_Premultiplied         } 16,
 { QImageNImageFormats               } 0
  );
var
  Image: TQtImage absolute ABitmap;
begin
  Result := CheckBitmap(ABitmap, 'RawImage_DescriptionFromBitmap');
  if not Result then Exit;

  //FillStandardDescription(ADesc);
  ADesc.Init;
  ADesc.Width := Image.Width;
  ADesc.Height := Image.Height;

  ADesc.BitOrder := riboReversedBits;
  ADesc.ByteOrder := riboLSBFirst;
  ADesc.LineOrder := riloTopToBottom;
  ADesc.LineEnd := rileDWordBoundary;

  ADesc.Depth := QImageFormatToDepth[Image.getFormat];
  ADesc.BitsPerPixel := ADesc.Depth;
  if ADesc.BitsPerPixel = 24 then
    ADesc.BitsPerPixel := 32;

  ADesc.Format := ricfRGBA;
  case ADesc.Depth of
    1, 8:
      begin
        ADesc.Format := ricfGray;

        ADesc.RedPrec := ADesc.BitsPerPixel;
      end;
    16:
      begin
        ADesc.Depth := 15;

        ADesc.RedPrec := 5;
        ADesc.GreenPrec := 5;
        ADesc.BluePrec := 5;

        ADesc.RedShift := 10;
        ADesc.GreenShift := 5;
        ADesc.BlueShift := 0;
      end;
    24:
      begin
        ADesc.RedPrec := 8;
        ADesc.GreenPrec := 8;
        ADesc.BluePrec := 8;

        ADesc.RedShift := 16;
        ADesc.GreenShift := 8;
        ADesc.BlueShift := 0;
      end;
    32:
      begin
        ADesc.AlphaPrec := 8;
        ADesc.RedPrec := 8;
        ADesc.GreenPrec := 8;
        ADesc.BluePrec := 8;

        ADesc.AlphaShift := 24;
        ADesc.RedShift := 16;
        ADesc.GreenShift := 8;
        ADesc.BlueShift := 0;
      end;
  end;
end;

{------------------------------------------------------------------------------
  Function: RawImage_DescriptionFromDevice
  Params: ADC:
          ADesc:
  Returns:

  Describes the standard format utilized by Qt
 ------------------------------------------------------------------------------}
function TQtWidgetSet.RawImage_DescriptionFromDevice(ADC: HDC; out ADesc: TRawImageDescription): Boolean;
begin
  Result := true;

  FillStandardDescription(ADesc{%H-});
end;

{------------------------------------------------------------------------------
  Function: RawImage_FromBitmap
  Params: ABitmap:
          AMask:
          ARect:
          ARawImage:
  Returns:

  Creates a raw image from a bitmap
 ------------------------------------------------------------------------------}
function TQtWidgetSet.RawImage_FromBitmap(out ARawImage: TRawImage; ABitmap, AMask: HBITMAP; ARect: PRect = nil): Boolean;
var
  Desc: TRawImageDescription absolute ARawImage.Description;
  Image: TQtImage absolute ABitmap;
  Mask: TQtImage absolute AMask;
  
  WorkImage, WorkMask: TQtImage;
  R: TRect;
  Width, Height: Integer;
  InvertPixels: Boolean;
  Px: QRgb;
begin
  Result := False;
  
  if not CheckBitmap(ABitmap, 'RawImage_FromBitmap') then Exit;
  if (AMask <> 0) and not CheckBitmap(AMask, 'RawImage_FromBitmap (mask)') then Exit;

  ARawImage.Init;
  RawImage_DescriptionFromBitmap(ABitmap, Desc);

  if ARect = nil
  then begin
    Width := Image.Width;
    Height := Image.Height;
    R := Rect(0, 0, Width, Height)
  end
  else begin
    R := ARect^;
    Width := R.Right - R.Left;
    Height := R.Bottom - R.Top;
  end;
  
  if (Width = Image.Width) and (Height = Image.Height)
  then begin
    WorkImage := Image;
    WorkMask := Mask;
  end
  else begin
    WorkImage := TQtImage.Create;
    WorkImage.CopyFrom(Image.Handle, R.Left, R.Top, Width, Height);
    if Mask <> nil then
    begin
      WorkMask := TQtImage.Create;
      WorkMask.CopyFrom(Mask.Handle, R.Left, R.Top, Width, Height);
    end
    else
      WorkMask := nil;
  end;

  Desc.Width := WorkImage.width;
  Desc.Height := WorkImage.height;

  // copy data
  ARawImage.DataSize := WorkImage.numBytes;
  ReAllocMem(ARawImage.Data, ARawImage.DataSize);
  if ARawImage.DataSize > 0 then
    Move(WorkImage.bits^, ARawImage.Data^, ARawImage.DataSize);

  if WorkMask <> nil then
  begin
    Desc.MaskLineEnd := rileDWordBoundary;
    Desc.MaskBitOrder := riboReversedBits;
    Desc.MaskBitsPerPixel := 1;
    ARawImage.MaskSize := WorkMask.numBytes;
    ReAllocMem(ARawImage.Mask, ARawImage.MaskSize);
    if ARawImage.MaskSize > 0 then
    begin
      InvertPixels := False;
      if WorkImage <> nil then
      begin
        Px := QImage_pixel(WorkImage.Handle, 0, 0);
        InvertPixels :=
          not QImage_hasAlphaChannel(WorkMask.Handle) and
          not QImage_hasAlphaChannel(WorkImage.Handle) and
          // invert only if WorkImage is RGB32 fmt and allGray
          (WorkImage.getFormat = QImageFormat_RGB32) and
          QImage_allGray(WorkImage.Handle) and
          ((Px = 0) or (Px = $FF))
      end;
      if InvertPixels then
        WorkMask.invertPixels(QImageInvertRGB);
      Move(WorkMask.bits^, ARawImage.Mask^, ARawImage.MaskSize);
      if InvertPixels then
        WorkMask.invertPixels(QImageInvertRGB);
    end;
  end;
  
  if WorkImage <> Image then
    WorkImage.Free;
  if WorkMask <> Mask then
    WorkMask.Free;

  Result := True;
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
function TQtWidgetSet.RawImage_FromDevice(out ARawImage: TRawImage; ADC: HDC; const ARect: TRect): Boolean;
var
  Desc: TRawImageDescription absolute ARawImage.Description;
  DCSize: TSize;
  APixmap: QPixmapH;
  Image: QImageH;
  Context: TQtDeviceContext;
  AWnd: HWND;
  
  procedure RawImage_FromImage(AImage: QImageH);
  begin
    ARawImage.DataSize := QImage_numBytes(AImage);
    ARawImage.Data := GetMem(ARawImage.DataSize);
    Move(QImage_bits(AImage)^, ARawImage.Data^, ARawImage.DataSize);
    ARawImage.Mask := nil;
  end;
  
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:> [WinAPI GetRawImageFromDevice] SrcDC: ', dbghex(ADC),
     ' SrcWidth: ', dbgs(ARect.Right - ARect.Left),
     ' SrcHeight: ', dbgs(ARect.Bottom - ARect.Top));
  {$endif}

  // todo: copy only passed rectangle

  Result := True;

  ARawImage.Init;
  FillStandardDescription(ARawImage.Description);
  Context := TQtDeviceContext(ADC);

  with DCSize, Context.getDeviceSize do
  begin
    cx := x;
    cy := y;
  end;

  if Context.Parent <> nil then
  begin
    AWnd := HwndFromWidgetH(Context.Parent);
    // AWnd should never be 0 at this point
    APixmap := TQtWidget(AWnd).grabWindow(0, 0, DCSize.cx, DCSize.cy, Context.Parent);
    Image := QImage_Create;
    QPixmap_toImage(APixmap, Image);
    RawImage_FromImage(Image);
    QImage_destroy(Image);
    QPixmap_destroy(APixmap);
  end else
  begin
    if Context.vImage <> nil then
      RawImage_FromImage(Context.vImage.Handle)
    else
    if Context.ParentPixmap <> nil then
    begin
      Image := QImage_create();
      QPixmap_toImage(Context.ParentPixmap, Image);
      RawImage_FromImage(Image);
      QImage_destroy(Image);
    end else
      Result := False;
  end;

  // In this case we use the size of the context
  Desc.Width := DCSize.cx;
  Desc.Height := DCSize.cy;

  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:< [WinAPI GetRawImageFromDevice]');
  {$endif}
end;

{------------------------------------------------------------------------------
  Function: RawImage_QueryDescription
  Params: AFlags:
          ADesc:
  Returns:

 ------------------------------------------------------------------------------}
function TQtWidgetSet.RawImage_QueryDescription(AFlags: TRawImageQueryFlags; var ADesc: TRawImageDescription): Boolean;
begin
  Result := inherited RawImage_QueryDescription(AFlags, ADesc);
  if ADesc.BitsPerPixel > 8 then
    ADesc.BitsPerPixel := 32
  else
  if ADesc.BitsPerPixel > 1 then
    ADesc.BitsPerPixel := 8;
end;

function TQtWidgetSet.ReleaseDesignerDC(Window: HWND; DC: HDC): Integer;
begin
  Result := 1;
end;

procedure TQtWidgetSet.RemoveEventHandler(var AHandler: PEventHandler);
var
  wheh: PWaitHandleEventHandler;
  i: QSocketNotifierType;
begin
  wheh := PWaitHandleEventHandler(aHandler);
  FSocketEventMap.Delete(wheh^.socket); // delete from the map

  for i := QSocketNotifierRead to QSocketNotifierException do
    if Assigned(wheh^.qsn[i]) then begin
      QSocketNotifier_destroy(wheh^.qsn[i]);
      QSocketNotifier_hook_destroy(wheh^.qsn_hook[i]);
    end;
  dispose(wheh);
  aHandler := nil;
end;

procedure TQtWidgetSet.RemovePipeEventHandler(var AHandler: PPipeEventHandler);
begin
  // todo
end;

procedure TQtWidgetSet.RemoveProcessEventHandler(var AHandler: PProcessEventHandler);
begin
  // todo
end;

procedure TQtWidgetSet.SetEventHandlerFlags(AHandler: PEventHandler;
  NewFlags: dword);
var
  wheh: PWaitHandleEventHandler;
  do_read: boolean;
  do_write: boolean;
  do_error: boolean;
begin
  wheh := PWaitHandleEventHandler(aHandler);

  do_read := NewFlags and EVE_IO_READ = EVE_IO_READ;
  do_write := NewFlags and EVE_IO_WRITE = EVE_IO_WRITE;
  do_error := NewFlags and EVE_IO_ERROR = EVE_IO_ERROR;

  QSocketNotifier_setEnabled(wheh^.qsn[QSocketNotifierRead], do_read);
  QSocketNotifier_setEnabled(wheh^.qsn[QSocketNotifierWrite], do_write);
  QSocketNotifier_setEnabled(wheh^.qsn[QSocketNotifierException], do_error);
end;

procedure TQtWidgetSet.SetRubberBandRect(const ARubberBand: HWND; const ARect: TRect);
begin
  if ARubberBand = 0 then
    exit;
  QRubberBand_setGeometry(QRubberBandH(ARubberBand), @ARect);
end;

function TQtWidgetSet.ExtUTF8Out(DC: HDC; X, Y: Integer; Options: Longint; Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;
begin
  Result := ExtTextOut(DC,X,Y,Options,Rect,Str,Count,Dx);
end;

function TQtWidgetSet.TextUTF8Out(DC: HDC; X, Y: Integer; Str: PChar; Count: Longint): Boolean;
begin
  Result := False;
  if IsValidDC(DC) then
    Result := TextOut(DC, X, Y, Str, Count);
end;

//##apiwiz##eps##   // Do not remove, no wizard declaration after this line
