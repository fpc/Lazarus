{ $Id: wincewsimglist.pp 13178 2007-12-06 01:17:52Z marc $}
{
 *****************************************************************************
 *                             wincewsimglist.pp                             * 
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
unit wincewsimglist;

{$mode objfpc}{$H+}

interface

uses
  // Libs
  Windows,
  commctrl,
  // RTL, LCL
  SysUtils, Classes, ImgList, GraphType, Graphics, LCLType,
  // Widgetset
  WinCEExtra, WinCEInt, WinCEProc, InterfaceBase,
  WSImgList, WSLCLClasses, WSProc, WSReferences;

type

  { TWinCEWSCustomImageListResolution }

  TWinCEWSCustomImageListResolution = class(TWSCustomImageListResolution)
  private
  protected
    class procedure AddData(AListHandle: TLCLIntfHandle; ACount, AReplaceIndex, AWidth, AHeight: Integer; AData: PRGBAQuad);
  published
    class procedure Clear(AList: TCustomImageListResolution); override;
    class function CreateReference(AList: TCustomImageListResolution; ACount, AGrow, AWidth,
      AHeight: Integer; AData: PRGBAQuad): TWSCustomImageListReference; override;
    class procedure Delete(AList: TCustomImageListResolution; AIndex: Integer); override;
    class procedure DestroyReference(AComponent: TComponent); override;
    class procedure Draw(AList: TCustomImageListResolution; AIndex: Integer; ACanvas: TCanvas;
      ABounds: TRect; ABkColor, ABlendColor: TColor; ADrawEffect: TGraphicsDrawEffect; AStyle: TDrawingStyle; AImageType: TImageType); override;
    class procedure DrawToDC(AList: TCustomImageListResolution; AIndex: Integer; ADC: HDC;
      ABounds: TRect; ABkColor, ABlendColor: TColor; ADrawEffect: TGraphicsDrawEffect; AStyle: TDrawingStyle; AImageType: TImageType);
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

function ColorToImagelistColor(AColor: TColor): DWord;
begin
  case AColor of
    clNone: Result := CLR_NONE;
    clDefault: Result := CLR_DEFAULT;
  else
    Result := ColorToRGB(AColor);
  end;
end;

class function GetColorDepth: Integer;
var
  DC: HDC;
begin
  DC := GetDC(0);
  Result := GetDeviceCaps(DC, BITSPIXEL) * GetDeviceCaps(DC, PLANES);
  ReleaseDC(0, DC);
end;

class procedure TWinCEWSCustomImageListResolution.AddData(AListHandle: TLCLIntfHandle; ACount, AReplaceIndex, AWidth, AHeight: Integer; AData: PRGBAQuad);

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
          else P^ := P^ shl 1;
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
  DoAdd;
end;

class procedure TWinCEWSCustomImageListResolution.Clear(AList: TCustomImageListResolution);
begin
  if not WSCheckReferenceAllocated(AList, 'Clear')
  then Exit;
  ImageList_SetImageCount(AList.Reference._Handle, 0);
end;

class function TWinCEWSCustomImageListResolution.CreateReference(AList: TCustomImageListResolution;
  ACount, AGrow, AWidth, AHeight: Integer; AData: PRGBAQuad): TWSCustomImageListReference;
var
  Flags: DWord;
begin
  FLAGS := ILC_COLOR or ILC_MASK; // the only supported wince flags
  Result._Init(ImageList_Create(AWidth, AHeight, Flags, ACount, AGrow));
  if Result.Allocated
  then AddData(Result._Handle, ACount, -1, AWidth, AHeight, AData);
end;

class procedure TWinCEWSCustomImageListResolution.Delete(AList: TCustomImageListResolution;
  AIndex: Integer);
begin
  if not WSCheckReferenceAllocated(AList, 'Delete')
  then Exit;
  ImageList_Remove(AList.Reference._Handle, AIndex);
end;

class procedure TWinCEWSCustomImageListResolution.DestroyReference(AComponent: TComponent);
begin
  if not WSCheckReferenceAllocated(TCustomImageListResolution(AComponent), 'DestroyReference')
  then Exit;
  ImageList_Destroy(TCustomImageListResolution(AComponent).Reference._Handle);
end;

class procedure TWinCEWSCustomImageListResolution.Draw(AList: TCustomImageListResolution; AIndex: Integer;
  ACanvas: TCanvas; ABounds: TRect; ABkColor, ABlendColor: TColor; ADrawEffect: TGraphicsDrawEffect; AStyle: TDrawingStyle; AImageType: TImageType);
begin
  if not WSCheckReferenceAllocated(AList, 'Draw')
  then Exit;
  DrawToDC(AList, AIndex, ACanvas.Handle, ABounds, ABkColor, ABlendColor, ADrawEffect, AStyle, AImageType);
end;

class procedure TWinCEWSCustomImageListResolution.DrawToDC(AList: TCustomImageListResolution;
  AIndex: Integer; ADC: HDC; ABounds: TRect; ABkColor, ABlendColor: TColor;
  ADrawEffect: TGraphicsDrawEffect; AStyle: TDrawingStyle;
  AImageType: TImageType);
var
  RawImg: TRawImage;
  ListImg, DeviceImg: TLazIntfImage;
  OldBmp, ImgHandle, MskHandle: HBitmap;
  ImgDC: HDC;
begin
  if ADrawEffect = gdeNormal then
  begin
    ImageList_DrawEx(AList.Reference._Handle, AIndex, ADC, ABounds.Left,
      ABounds.Top, ABounds.Right, ABounds.Bottom, ColorToImagelistColor(ABkColor),
      ColorToImagelistColor(ABlendColor), DRAWINGSTYLEMAP[AStyle] or IMAGETPYEMAP[AImageType]);
  end
  else
  begin
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

class procedure TWinCEWSCustomImageListResolution.Insert(AList: TCustomImageListResolution;
  AIndex: Integer; AData: PRGBAQuad);
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

class procedure TWinCEWSCustomImageListResolution.Move(AList: TCustomImageListResolution;
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

class procedure TWinCEWSCustomImageListResolution.Replace(AList: TCustomImageListResolution;
  AIndex: Integer; AData: PRGBAQuad);
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
