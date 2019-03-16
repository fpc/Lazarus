{ $Id$}
{
 *****************************************************************************
 *                             Win32WSImgList.pp                             * 
 *                             -----------------                             * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit Win32WSImgList;

{$mode objfpc}{$H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
// rtl
  CommCtrl, Windows, SysUtils, Classes,
// lcl
  ImgList, GraphType, Graphics, LCLType,
// ws
  Win32Extra, Win32Int, Win32Proc, InterfaceBase,
  WSImgList, WSLCLClasses, WSProc, WSReferences;

type

  { TWin32WSCustomImageList }

  TWin32WSCustomImageListResolution = class(TWSCustomImageListResolution)
  protected
    class procedure AddData(AListHandle: TLCLIntfHandle;
      ACount, AReplaceIndex, AWidth, AHeight: Integer; AData: PRGBAQuad);
  published
    class procedure Clear(AList: TCustomImageListResolution); override;
    class function CreateReference(AList: TCustomImageListResolution; ACount, AGrow, AWidth,
      AHeight: Integer; AData: PRGBAQuad): TWSCustomImageListReference; override;
    class procedure Delete(AList: TCustomImageListResolution; AIndex: Integer); override;
    class procedure DestroyReference(AComponent: TComponent); override;
    class procedure Draw(AList: TCustomImageListResolution; AIndex: Integer; ACanvas: TCanvas;
      ABounds: TRect; ABkColor, ABlendColor: TColor; ADrawEffect: TGraphicsDrawEffect;
      AStyle: TDrawingStyle; AImageType: TImageType); override;
    class procedure DrawToDC(AList: TCustomImageListResolution; AIndex: Integer; ADC: HDC;
      ABounds: TRect; ABkColor, ABlendColor: TColor; ADrawEffect: TGraphicsDrawEffect;
      AStyle: TDrawingStyle; AImageType: TImageType);
    class procedure Insert(AList: TCustomImageListResolution; AIndex: Integer; AData: PRGBAQuad); override;
    class procedure Move(AList: TCustomImageListResolution; ACurIndex, ANewIndex: Integer); override;
    class procedure Replace(AList: TCustomImageListResolution; AIndex: Integer; AData: PRGBAQuad); override;
  end;


implementation

uses
  intfgraphics;

const
  DRAWINGSTYLEMAP: array[TDrawingStyle] of DWord = (
{ dsFocus       } ILD_FOCUS,
{ dsSelected    } ILD_SELECTED,
{ dsNormal      } ILD_NORMAL,
{ dsTransparent } ILD_TRANSPARENT
  );
  
  IMAGETPYEMAP: array[TImageType] of DWord = (
{ itImage } ILD_NORMAL,
{ itMask }  ILD_MASK
  );

function ColorToImagelistColor(AColor: TColor): TColorRef;
begin
  case AColor of
    clNone: Result := CLR_NONE;
    clDefault: Result := CLR_DEFAULT;
  else
    Result := ColorToRGB(AColor);
  end;
end;

function GetColorDepth(ADC: HDC): Integer; inline; overload;
begin
  Result := GetDeviceCaps(ADC, BITSPIXEL) * GetDeviceCaps(ADC, PLANES);
end;

function GetColorDepth: Integer; inline; overload;
var
  DC: HDC;
begin
  DC := GetDC(0);
  Result := GetColorDepth(DC);
  ReleaseDC(0, DC);
end;

class procedure TWin32WSCustomImageListResolution.AddData(AListHandle: TLCLIntfHandle;
  ACount, AReplaceIndex, AWidth, AHeight: Integer; AData: PRGBAQuad);

  procedure DoAddAlpha;
  var
    Info: Windows.TBitmapInfo;
    BitsPtr: Pointer;
    bmp: HBITMAP;
    DC: HDC;
    DataCount, DataSize: Integer;
  begin
    FillChar(Info, SizeOf(Info), 0);
    Info.bmiHeader.biSize := SizeOf(Info.bmiHeader);
    Info.bmiHeader.biWidth := AWidth;
    Info.bmiHeader.biHeight := -AHeight; // request top down
    Info.bmiHeader.biPlanes := 1;
    Info.bmiHeader.biBitCount := 32;
    Info.bmiHeader.biCompression := BI_RGB;

    BitsPtr := nil;
    DC := GetDC(0);
    bmp := Windows.CreateDIBSection(DC, Info, DIB_RGB_COLORS, BitsPtr, 0, 0);
    ReleaseDC(0, DC);

    if BitsPtr = nil
    then begin
      DeleteObject(bmp);
      Exit;
    end;

    DataCount := AWidth * AHeight;
    DataSize := DataCount * SizeOf(AData^);
    while ACount > 0 do
    begin
      System.Move(AData^, BitsPtr^, DataSize);
      if AReplaceIndex = -1
      then ImageList_Add(AListHandle, bmp, 0)
      else ImageList_Replace(AListHandle, AReplaceIndex, bmp, 0);
      Inc(AData, DataCount);
      Dec(ACount);
    end;

    DeleteObject(bmp);
  end;

  procedure DoAdd;
  var
    Info: Windows.TBitmapInfo;
    BitsPtr, MaskPtr: Pointer;
    P, LinePtr: PByte;
    bmp, msk: HBITMAP;
    DC: HDC;
    DataCount, DataSize, x, y, MaskStride: Integer;
  begin
    FillChar(Info, SizeOf(Info), 0);
    Info.bmiHeader.biSize := SizeOf(Info.bmiHeader);
    Info.bmiHeader.biWidth := AWidth;
    Info.bmiHeader.biHeight := -AHeight; // request top down
    Info.bmiHeader.biPlanes := 1;
    Info.bmiHeader.biBitCount := 32;
    Info.bmiHeader.biCompression := BI_RGB;

    BitsPtr := nil;
    MaskPtr := nil;
    msk := 0;
    bmp := 0;
    DC := GetDC(0);
    bmp := Windows.CreateDIBSection(DC, Info, DIB_RGB_COLORS, BitsPtr, 0, 0);
    ReleaseDC(0, DC);

    if (bmp = 0) or (BitsPtr = nil) then
    begin
      DeleteObject(bmp);
      Exit;
    end;

    DataCount := AWidth * AHeight;
    DataSize := DataCount * SizeOf(AData^);
    MaskStride := ((AWidth + 15) shr 4) shl 1; // align to Word
    MaskPtr := AllocMem(AHeight * MaskStride);
    while ACount > 0 do
    begin
      System.Move(AData^, BitsPtr^, DataSize);

      // create mask
      LinePtr := MaskPtr;
      for y := 1 to AHeight do
      begin
        p := LinePtr;
        for x := 1 to AWidth do
        begin
          P^ := (P^ and $FE) or ((not AData^.Alpha) shr 7);
          if x and $7 = 0
          then Inc(p)
          else P^ := Byte(P^ shl 1);
          Inc(AData);
        end;
        // finish mask shifting
        if (AWidth and $7) <> 0 then
          P^ := P^ shl (7 - (AWidth and 7));
        Inc(LinePtr, MaskStride);
      end;

      msk := CreateBitmap(AWidth, AHeight, 1, 1, MaskPtr);
      if AReplaceIndex = -1
      then ImageList_Add(AListHandle, bmp, msk)
      else ImageList_Replace(AListHandle, AReplaceIndex, bmp, msk);
      Dec(ACount);
      DeleteObject(msk);
    end;
    FreeMem(MaskPtr);

    DeleteObject(bmp);
  end;

begin
  if Win32WidgetSet.CommonControlsVersion >= ComCtlVersionIE6
  then DoAddAlpha
  else DoAdd;
end;

class procedure TWin32WSCustomImageListResolution.Clear(
  AList: TCustomImageListResolution);
begin
  if not WSCheckReferenceAllocated(AList, 'Clear')
  then Exit;
  ImageList_SetImageCount(AList.Reference._Handle, 0);
end;

class function TWin32WSCustomImageListResolution.CreateReference(
  AList: TCustomImageListResolution; ACount, AGrow, AWidth, AHeight: Integer;
  AData: PRGBAQuad): TWSCustomImageListReference;
var
  Flags: DWord;
begin
  if Win32WidgetSet.CommonControlsVersion >= ComCtlVersionIE6
  then begin
    Flags := ILC_COLOR32;
  end
  else begin
    case GetColorDepth of
      04: FLAGS := ILC_COLOR4 or ILC_MASK;
      08: FLAGS := ILC_COLOR8 or ILC_MASK;
      16: FLAGS := ILC_COLOR16 or ILC_MASK;
      24: FLAGS := ILC_COLOR24 or ILC_MASK;
      32: FLAGS := ILC_COLOR32 or ILC_MASK;
    else
      FLAGS := ILC_COLOR or ILC_MASK;
    end;
  end;
  {$PUSH}
  {$WARNINGS OFF}
  Result._Init(ImageList_Create(AWidth, AHeight, Flags, ACount, AGrow));
  if Result.Allocated and (ACount > 0) then
    AddData(Result._Handle, ACount, -1, AWidth, AHeight, AData);
  {$POP}
end;

class procedure TWin32WSCustomImageListResolution.Delete(
  AList: TCustomImageListResolution; AIndex: Integer);
begin
  if not WSCheckReferenceAllocated(AList, 'Delete')
  then Exit;
  ImageList_Remove(AList.Reference._Handle, AIndex);
end;

class procedure TWin32WSCustomImageListResolution.DestroyReference(AComponent: TComponent);
begin
  if not WSCheckReferenceAllocated(TCustomImageListResolution(AComponent), 'DestroyReference')
  then Exit;
  ImageList_Destroy(TCustomImageListResolution(AComponent).Reference._Handle);
end;

class procedure TWin32WSCustomImageListResolution.Draw(AList: TCustomImageListResolution;
  AIndex: Integer; ACanvas: TCanvas; ABounds: TRect; ABkColor,
  ABlendColor: TColor; ADrawEffect: TGraphicsDrawEffect; AStyle: TDrawingStyle;
  AImageType: TImageType);
begin
  if not WSCheckReferenceAllocated(AList, 'Draw')
  then Exit;
  DrawToDC(AList, AIndex, ACanvas.Handle, ABounds, ABkColor, ABlendColor, ADrawEffect, AStyle, AImageType);
end;

class procedure TWin32WSCustomImageListResolution.DrawToDC(
  AList: TCustomImageListResolution; AIndex: Integer; ADC: HDC; ABounds: TRect;
  ABkColor, ABlendColor: TColor; ADrawEffect: TGraphicsDrawEffect;
  AStyle: TDrawingStyle; AImageType: TImageType);
var
  DrawParams: TImageListDrawParams;
  RawImg: TRawImage;
  ListImg, DeviceImg: TLazIntfImage;
  OldBmp, ImgHandle, MskHandle: HBitmap;
  ImgDC: HDC;
  HasComCtl6: Boolean;
begin
  HasComCtl6 := Win32WidgetSet.CommonControlsVersion >= ComCtlVersionIE6;
  // If we are using comctl > 6 then COLOR_32 is supported and alpha bitmaps will
  // be drawn correctly. If version is lower than our alpha bitmaps will be drawn
  // with mask and with no alpha. But if we draw with effect different fron normal
  // we will draw using another method with alpha even using comctl < 6. To prevent
  // such inconsistency in drawing lets check whether we need alpha drawing first
  // and whether imagelist has native alpha drawing. If it has then we will use
  // ImageList_DrawEx in other case we will draw alpha bitmap ourself.
  if (ADrawEffect = gdeNormal) and (HasComCtl6 or (GetColorDepth(ADC) < 32)) then
  begin
    ImageList_DrawEx(AList.Reference._Handle, AIndex, ADC, ABounds.Left,
      ABounds.Top, ABounds.Right, ABounds.Bottom, ColorToImagelistColor(ABkColor),
      ColorToImagelistColor(ABlendColor), DRAWINGSTYLEMAP[AStyle] or IMAGETPYEMAP[AImageType]);
  end
  else
  if (ADrawEffect = gdeDisabled) and HasComCtl6 then
  begin
    // if it is manifested exe then use winXP algoriphm of gray painting
    FillChar(DrawParams, SizeOf(DrawParams), 0);
    DrawParams.cbSize := SizeOf(DrawParams);
    DrawParams.himl := AList.Reference._Handle;
    DrawParams.i := AIndex;
    DrawParams.hdcDst := ADC;
    DrawParams.x := ABounds.Left;
    DrawParams.y := ABounds.Top;
    DrawParams.cx := ABounds.Right;
    DrawParams.cy := ABounds.Bottom;
    DrawParams.rgbBk := ColorToImagelistColor(ABkColor);
    DrawParams.rgbFg := ColorToImagelistColor(ABlendColor);
    DrawParams.fStyle := DRAWINGSTYLEMAP[AStyle] or IMAGETPYEMAP[AImageType];
    DrawParams.fState := ILS_SATURATE; // draw greyed
    ImageList_DrawIndirect(@DrawParams);
  end
  else
  begin
    if ABounds.Right = 0 then
      ABounds.Right := AList.Width;
    if ABounds.Bottom = 0 then
      ABounds.Bottom := AList.Height;
    // use RawImage_PerformEffect to perform drawing effect
    AList.GetRawImage(AIndex, RawImg);
    RawImg.PerformEffect(ADrawEffect, True);

    if not Widgetset.RawImage_CreateBitmaps(RawImg, ImgHandle, MskHandle, True)
    then begin
      // bummer, the widgetset doesn't support our 32bit format, try device
      ListImg := TLazIntfImage.Create(RawImg, False);
      DeviceImg := TLazIntfImage.Create(0,0,[]);
      DeviceImg.DataDescription := GetDescriptionFromDevice(0, AList.Width, AList.Height);
      DeviceImg.CopyPixels(ListImg);
      DeviceImg.GetRawImage(RawImg);
      Widgetset.RawImage_CreateBitmaps(RawImg, ImgHandle, MskHandle);
      DeviceImg.Free;
      ListImg.Free;
    end;
    
    ImgDC := CreateCompatibleDC(ADC);
    OldBmp := SelectObject(ImgDC, ImgHandle);
    WidgetSet.StretchMaskBlt(ADC, ABounds.Left, ABounds.Top, ABounds.Right, ABounds.Bottom,
      ImgDC, 0, 0, ABounds.Right, ABounds.Bottom, MskHandle, 0, 0, SRCCOPY);
    RawImg.FreeData;
    SelectObject(ImgDC, OldBmp);
    if ImgHandle<>0 then DeleteObject(ImgHandle);
    if MskHandle<>0 then DeleteObject(MskHandle);
    DeleteDC(ImgDC);
  end;
end;

class procedure TWin32WSCustomImageListResolution.Insert(
  AList: TCustomImageListResolution; AIndex: Integer; AData: PRGBAQuad);
var
  ImageList: HImageList;
  Count: Integer;
begin
  if not WSCheckReferenceAllocated(AList, 'Insert')
  then Exit;

  ImageList := AList.Reference._Handle;
  Count := ImageList_GetImageCount(ImageList);
  
  if (AIndex <= Count) and (AIndex >= 0) then
  begin
    AddData(ImageList, 1, -1, AList.Width, AList.Height, AData);
    if AIndex <> Count
    then Move(AList, Count, AIndex);
  end;
end;

class procedure TWin32WSCustomImageListResolution.Move(AList: TCustomImageListResolution;
  ACurIndex, ANewIndex: Integer);
var
  n: integer;
  Handle: THandle;
begin
  if not WSCheckReferenceAllocated(AList, 'Move')
  then Exit;
  
  if ACurIndex = ANewIndex
  then Exit;
        
  Handle := AList.Reference._Handle;
  if ACurIndex < ANewIndex
  then begin       
    for n := ACurIndex to ANewIndex - 1 do
      ImageList_Copy(Handle, n + 1, Handle, n, ILCF_SWAP);
  end
  else begin
    for n := ACurIndex downto ANewIndex + 1 do
      ImageList_Copy(Handle, n - 1, Handle, n, ILCF_SWAP);
  end;
end;

class procedure TWin32WSCustomImageListResolution.Replace(
  AList: TCustomImageListResolution; AIndex: Integer; AData: PRGBAQuad);
var
  ImageList: HImageList;
  Count: Integer;
begin
  if not WSCheckReferenceAllocated(AList, 'Replace')
  then Exit;

  ImageList := AList.Reference._Handle;
  Count := ImageList_GetImageCount(ImageList);

  if (AIndex < Count) and (AIndex >= 0)
  then AddData(ImageList, 1, AIndex, AList.Width, AList.Height, AData);
end;

end.
