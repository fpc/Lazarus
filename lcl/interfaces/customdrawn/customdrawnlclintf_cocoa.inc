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
begin
end;

(*{------------------------------------------------------------------------------
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

  if Context.Parent <> nil then
  begin
    Pixmap := TQtPixmap.Create(@DCSize);
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
      RawImage_FromImage(Context.vImage.FHandle)
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
end;*)

procedure TCDWidgetset.ShowVirtualKeyboard();
begin
end;

//##apiwiz##eps##   // Do not remove, no wizard declaration after this line
