{%MainUnit qtint.pp}
{ $Id$ }

{******************************************************************************
                  All QT interface support routines
                   Initial Revision  : Sat Jan 17 19:00:00 2004


  !! Keep alphabetical !!

  Support routines go to qtproc.pp

 ******************************************************************************
 Implementation
 ******************************************************************************

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

//##apiwiz##sps##   // Do not remove

function TQtWidgetSet.AddEventHandler(AHandle: THandle; AFlags: dword;
  AEventHandler: TWaitHandleEvent; AData: PtrInt): PEventHandler;
begin
  // todo
  Result := nil;
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

{------------------------------------------------------------------------------
  Function: CreateStandardCursor
  Params:
  Returns:
 ------------------------------------------------------------------------------}
function TQtWidgetSet.CreateStandardCursor(ACursor: SmallInt): hCursor;
var
  CursorShape: QtCursorShape;
begin
  Result := 0;
  if ACursor < crLow then Exit;
  if ACursor > crHigh then Exit;

  // TODO: map is better
  case TCursor(ACursor) of
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
    Result := hCursor(QCursor_create(CursorShape));
end;

{------------------------------------------------------------------------------
  Function: DrawArrow
  Params:
  Returns:
 ------------------------------------------------------------------------------}
procedure TQtWidgetSet.DrawArrow(Arrow: TComponent; Canvas: TPersistent);
const
  QtArrowTypeMap: array[TArrowType] of QStylePrimitiveElement =
  (
{atUp   } QStylePE_IndicatorArrowUp,
{atDown } QStylePE_IndicatorArrowDown,
{atLeft } QStylePE_IndicatorArrowLeft,
{atRight} QStylePE_IndicatorArrowRight
  );
var
  DC: TQtDeviceContext;
  ARect: TRect;
  StyleOption: QStyleOptionH;
begin
  DC := TQtDeviceContext(TCanvas(Canvas).Handle);
  ARect := TControl(Arrow).ClientRect;
  
  StyleOption := QStyleOption_create(1, integer(QStyleOptionSO_Default));
  try
    // I dont know the reason, but under windows down arrow size is very small
    // and is not dependent on passed ARect.
    // There is nothing in qt source that can cause such bad painting.
    // Other styles draw down arrow very well.
    QStyleOption_initFrom(StyleOption, DC.Parent);
    QStyleOption_setRect(StyleOption, @ARect);
    QStyle_drawPrimitive(QApplication_style, QtArrowTypeMap[TArrow(Arrow).ArrowType],
      StyleOption, DC.Widget, DC.Parent);
  finally
    QStyleOption_destroy(StyleOption);
  end;
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


{------------------------------------------------------------------------------
  Function: FontCanUTF8
  Params:
  Returns:
 ------------------------------------------------------------------------------}
function TQtWidgetSet.FontCanUTF8(Font: HFont): Boolean;
begin
  {$ifdef windows}
  {$ifdef WindowsUnicodeSupport}
    Result := True;
  {$else}
    Result := False;
  {$endif}
  {$else}
    Result := IsValidGDIObject(Font);
  {$endif}
end;

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
    QtFontInfo := QFontInfo_create(TQtFont(Font).Widget);
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

{------------------------------------------------------------------------------
  Function: IntfSendsUTF8KeyPress
  Params:
  Returns:
 ------------------------------------------------------------------------------}

function TQtWidgetSet.IntfSendsUTF8KeyPress: boolean;
begin
  Result := True;
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
  QBtns      : QMessageBoxStandardButtons;
  DefQBtn    : QMessageBoxStandardButton;
  CurQBtn    : QMessageBoxStandardButton;
  BtnIdx     : LongInt;
  CaptionStr : WideString;
  MessageStr : WideString;
  QResult    : QMessageBoxStandardButton;
begin
  QBtns := 0;
  DefQBtn := QMessageBoxNoButton;
   {Convert LCL "id" button values to Qt values}
  for BtnIdx := 0 to ButtonCount-1 do
    begin
    case Buttons[BtnIdx] of
      idButtonYes      : CurQBtn := QMessageBoxYes;
      idButtonNo       : CurQBtn := QMessageBoxNo;
      idButtonOK       : CurQBtn := QMessageBoxOk;
      idButtonCancel   : CurQBtn := QMessageBoxCancel;
      idButtonAbort    : CurQBtn := QMessageBoxAbort;
      idButtonRetry    : CurQBtn := QMessageBoxRetry;
      idButtonIgnore   : CurQBtn := QMessageBoxIgnore;
//      idButtonAll      : no equivalent
      idButtonNoToAll  : CurQBtn := QMessageBoxNoToAll;
      idButtonYesToAll : CurQBtn := QMessageBoxYesToAll;
      idButtonHelp     : CurQBtn := QMessageBoxHelp;
      idButtonClose    : CurQBtn := QMessageBoxClose;
      else
        CurQBtn := QMessageBoxNoButton;
      end;
    QBtns := QBtns or CurQBtn;
    if BtnIdx = DefaultIndex then  {Default button?}
      DefQBtn := CurQBtn;
    end;

  CaptionStr := GetUtf8String(DialogCaption);
  MessageStr := GetUtf8String(DialogMessage);

  case DialogType of
    idDialogWarning : 
     QResult := QMessageBox_warning(nil, @CaptionStr, @MessageStr, QBtns, DefQBtn);
    idDialogError : 
     QResult := QMessageBox_critical(nil, @CaptionStr, @MessageStr, QBtns, DefQBtn);
    idDialogInfo : 
     QResult := QMessageBox_information(nil, @CaptionStr, @MessageStr, QBtns, DefQBtn);
    idDialogConfirm : 
     QResult := QMessageBox_question(nil, @CaptionStr, @MessageStr, QBtns, DefQBtn);
    else
      // simple show message
      QResult := QMessageBox_information(nil, @CaptionStr, @MessageStr, QBtns, DefQBtn);
    end;

   {Convert Qt result to LCL "id" dialog result}
  case QResult of
    QMessageBoxOk       : Result := idButtonOK;
    QMessageBoxCancel   : Result := idButtonCancel;
    QMessageBoxAbort    : Result := idButtonAbort;
    QMessageBoxRetry    : Result := idButtonRetry;
    QMessageBoxIgnore   : Result := idButtonIgnore;
    QMessageBoxYes      : Result := idButtonYes;
    QMessageBoxNo       : Result := idButtonNo;
//    QMessageBoxAll      : no equivalent
    QMessageBoxNoToAll  : Result := idButtonNoToAll;
    QMessageBoxYesToAll : Result := idButtonYesToAll;
    else
      Result := -1;
    end;
end;  {TQtWidgetSet.PromptUser}

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
  NewData := GetMem(ARawImage.DataSize);
  Move(ARawImage.Data^, NewData^, ARawImage.DataSize);

  {$note TODO: use format given in rawimage}
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
  if ARawImage.Mask = nil then Exit;
  if ARawImage.MaskSize = 0 then Exit;
  
  NewData := GetMem(ARawImage.MaskSize);
  Move(ARawImage.Mask^, NewData^, ARawImage.MaskSize);

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
var
  Image: TQtImage absolute ABitmap;
begin
  Result := CheckBitmap(ABitmap, 'RawImage_DescriptionFromBitmap');
  if not Result then Exit;

  FillStandardDescription(ADesc);

  ADesc.Width := Image.Width;
  ADesc.Height := Image.Height;
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

  FillStandardDescription(ADesc);
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
function TQtWidgetSet.RawImage_FromBitmap(out ARawImage: TRawImage; ABitmap, AMask: HBITMAP; const ARect: TRect): Boolean;
var
  Desc: TRawImageDescription absolute ARawImage.Description;
  Image: TQtImage absolute ABitmap;
  Mask: TQtImage absolute AMask;
  
  WorkImage, WorkMask: TQtImage;
  Width, Height: Integer;
begin
  Result := False;
  
  if not CheckBitmap(ABitmap, 'RawImage_FromBitmap') then Exit;
  if (AMask <> 0) and not CheckBitmap(AMask, 'RawImage_FromBitmap (mask)') then Exit;


  ARawImage.Init;
  FillStandardDescription(ARawImage.Description);
  
  Width := ARect.Right - ARect.Left;
  Height := ARect.Bottom - ARect.Top;
  
  if (Width = Image.Width) and (Height = Image.Height)
  then begin
    WorkImage := Image;
    WorkMask := Mask;
  end
  else begin
    WorkImage := TQtImage.Create;
    WorkImage.CopyFrom(Image.Handle, ARect.Left, ARect.Top, Width, Height);
    if Mask <> nil then
    begin
      WorkMask := TQtImage.Create;
      WorkMask.CopyFrom(Mask.Handle, ARect.Left, ARect.Top, Width, Height);
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

  if WorkMask <> nil
  then begin
    ARawImage.MaskSize := WorkMask.numBytes;
    ReAllocMem(ARawImage.Mask, ARawImage.MaskSize);
    if ARawImage.MaskSize > 0 then
       Move(WorkMask.bits^, ARawImage.Mask^, ARawImage.MaskSize);
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

  //SrcWidth, SrcHeight: Integer;
  WinID: Cardinal;
  DCSize: TSize;
  Pixmap: TQtPixmap;
  Image: QImageH;
  Context: TQtDeviceContext;
  
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
  
  Pixmap := TQtPixmap.Create(@DCSize);
  
  if Context.Parent <> nil then
  begin
    WinID := QWidget_winId(Context.Parent);
    try
      // if you have dual monitors then getDeviceSize return
      // more width than screen width, but grabWindow will only grab one
      // screen, so its width will be less
      // Solution: we can either pass prefered size to grabWindow or
      // correct Description size after. I see the first solution as more correct.
      Pixmap.grabWindow(WinID, 0, 0, DCSize.cx, DCSize.cy);
      Image := QImage_Create;
      Pixmap.toImage(Image);
      RawImage_FromImage(Image);
      QImage_destroy(Image);
    finally
      Pixmap.Free;
    end;
  end else
  begin
    if Context.vImage <> nil then
      RawImage_FromImage(Context.vImage)
    else
      Result := False;
  end;

  // In this case we use the size of the context
  Desc.Width := DCSize.cx;
  Desc.Height := DCSize.cy;

  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:< [WinAPI GetRawImageFromDevice]');
  {$endif}
end;

function TQtWidgetSet.ReleaseDesignerDC(Window: HWND; DC: HDC): Integer;
begin
  Result := 1;
end;

procedure TQtWidgetSet.RemoveEventHandler(var AHandler: PEventHandler);
begin
  // todo
end;

procedure TQtWidgetSet.RemovePipeEventHandler(var AHandler: PPipeEventHandler);
begin
  // todo
end;

procedure TQtWidgetSet.RemoveProcessEventHandler(var AHandler: PProcessEventHandler);
begin
  // todo
end;

function TQtWidgetSet.TextUTF8Out(DC: HDC; X, Y: Integer; Str: PChar; Count: Longint): Boolean;
begin
  Result := False;
  if IsValidDC(DC) then
    Result := TextOut(DC, X, Y, Str, Count);
end;

//##apiwiz##eps##   // Do not remove, no wizard declaration after this line
