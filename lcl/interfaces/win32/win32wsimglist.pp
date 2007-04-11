{ $Id$}
{
 *****************************************************************************
 *                             Win32WSImgList.pp                             * 
 *                             -----------------                             * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

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
  Windows, SysUtils, Classes, ImgList, GraphType, Graphics, LCLType,
  WinExt,
////////////////////////////////////////////////////
  WSImgList, WSLCLClasses, WSProc;

type

  { TWin32WSCustomImageList }

  TWin32WSCustomImageList = class(TWSCustomImageList)
  private
  protected
    class procedure InternalCreateBitmap(AList: TCustomImageList; AWidth, AHeight: Integer; AData: PRGBAQuad;
      var hbmImage, hbmMask: HBitmap);
    class procedure InternalDestroyBitmap(hbmImage, hbmMask: HBitmap);
  public
    class procedure Clear(AList: TCustomImageList); override;
    class function CreateHandle(AList: TCustomImageList; ACount, AGrow, AWidth,
      AHeight: Integer; AData: PRGBAQuad): TLCLIntfHandle; override;
    class procedure Delete(AList: TCustomImageList; AIndex: Integer); override;
    class procedure DestroyHandle(AComponent: TComponent); override;
    class procedure Draw(AList: TCustomImageList; AIndex: Integer; ACanvas: TCanvas;
      ABounds: TRect; AEnabled: Boolean; AStyle: TDrawingStyle); override;
    class procedure Insert(AList: TCustomImageList; AIndex: Integer; AData: PRGBAQuad); override;
    class procedure Move(AList: TCustomImageList; ACurIndex, ANewIndex: Integer); override;
    class procedure Replace(AList: TCustomImageList; AIndex: Integer; AData: PRGBAQuad); override;
  end;


implementation

const
  DrawingStyleMap: array[TDrawingStyle] of DWord =
  (
{ dsFocus       } ILD_FOCUS,
{ dsSelected    } ILD_SELECTED,
{ dsNormal      } ILD_NORMAL,
{ dsTransparent } ILD_TRANSPARENT
  );

{ TWin32WSCustomImageList }

class procedure TWin32WSCustomImageList.InternalCreateBitmap(AList: TCustomImageList;
  AWidth, AHeight: Integer; AData: PRGBAQuad; var hbmImage, hbmMask: HBitmap);
begin
  // this will work only with Comctl32.dll version 6 (XP manifest)
  // in other case we need separate image and mask
  hbmImage := CreateBitmap(AWidth, AHeight, 1, 32, AData);
  hbmMask := 0;
end;

class procedure TWin32WSCustomImageList.InternalDestroyBitmap(hbmImage,
  hbmMask: HBitmap);
begin
  if hbmImage <> 0 then
    DeleteObject(hbmImage);
  if hbmMask <> 0 then
    DeleteObject(hbmMask);
end;

class procedure TWin32WSCustomImageList.Clear(AList: TCustomImageList);
begin
  if not WSCheckHandleAllocated(AList, 'Clear')
  then Exit;
  ImageList_SetImageCount(HImageList(AList.Handle), 0);
end;

class function TWin32WSCustomImageList.CreateHandle(AList: TCustomImageList;
  ACount, AGrow, AWidth, AHeight: Integer; AData: PRGBAQuad): TLCLIntfHandle;
var
  FLags: DWord;
  hbmImage, hbmMask: HBITMAP;
  i: integer;
begin
  if (Win32Platform and VER_PLATFORM_WIN32_NT) <> 0 then
    Flags := ILC_COLOR32 or ILC_MASK
  else
    Flags := ILC_COLOR16 or ILC_MASK;
  Result := ImageList_Create(AWidth, AHeight, Flags, ACount, AGrow);
  
  if AData <> nil then
  begin
    // this is very slow method :(
    for i := 0 to ACount - 1 do
    begin
      InternalCreateBitmap(AList, AWidth, AHeight, @AData[AWidth * AHeight * i],
        hbmImage, hbmMask);
      ImageList_Add(Result, hbmImage, hbmMask);
      InternalDestroyBitmap(hbmMask, hbmImage);
    end;
  end;
end;

class procedure TWin32WSCustomImageList.Delete(AList: TCustomImageList;
  AIndex: Integer);
begin
  if not WSCheckHandleAllocated(AList, 'Delete')
  then Exit;
  ImageList_Remove(HImageList(AList.Handle), AIndex);
end;

class procedure TWin32WSCustomImageList.DestroyHandle(AComponent: TComponent);
begin
  if not WSCheckHandleAllocated(TCustomImageList(AComponent), 'DestroyHandle')
  then Exit;
  ImageList_Destroy(TCustomImageList(AComponent).Handle);
end;

class procedure TWin32WSCustomImageList.Draw(AList: TCustomImageList; AIndex: Integer;
  ACanvas: TCanvas; ABounds: TRect; AEnabled: Boolean; AStyle: TDrawingStyle);
begin
  if not WSCheckHandleAllocated(AList, 'Draw')
  then Exit;

  ImageList_DrawEx(HImageList(AList.Handle), AIndex, ACanvas.Handle, ABounds.Left,
    ABounds.Top, ABounds.Right, ABounds.Bottom, CLR_NONE, CLR_NONE,
    DrawingStyleMap[AStyle]);
end;

class procedure TWin32WSCustomImageList.Insert(AList: TCustomImageList;
  AIndex: Integer; AData: PRGBAQuad);
var
  AImageList: HImageList;
  ACount: Integer;
  hbmImage, hbmMask: HBITMAP;
begin
  if not WSCheckHandleAllocated(AList, 'Insert')
  then Exit;

  AImageList := HImageList(AList.Handle);
  ACount := ImageList_GetImageCount(AImageList);
  
  if (AIndex <= ACount) and (AIndex >= 0) then
  begin
    InternalCreateBitmap(AList, AList.Width, AList.Height, AData, hbmImage, hbmMask);
    ImageList_Add(AImageList, hbmImage, hbmMask);
    InternalDestroyBitmap(hbmImage, hbmMask);
    if AIndex <> ACount 
    then Move(AList, ACount, AIndex);
  end;
end;

class procedure TWin32WSCustomImageList.Move(AList: TCustomImageList;
  ACurIndex, ANewIndex: Integer);
var
  n: integer;
  Handle: THandle;
begin
  if not WSCheckHandleAllocated(AList, 'Move')
  then Exit;
  
  if ACurIndex = ANewIndex
  then Exit;
        
  Handle := AList.Handle;
  if ACurIndex < ANewIndex
  then begin       
    for n := ACurIndex to ANewIndex - 1 do
      ImageList_Copy(Handle, n + 1, Handle, n, ILCF_SWAP);
  end
  else begin
    for n := ACurIndex downto ANewIndex - 1 do
      ImageList_Copy(Handle, n - 1, Handle, n, ILCF_SWAP);
  end;
end;

class procedure TWin32WSCustomImageList.Replace(AList: TCustomImageList;
  AIndex: Integer; AData: PRGBAQuad);
var
  hbmImage, hbmMask: HBITMAP;
begin
  if not WSCheckHandleAllocated(AList, 'Replace')
  then Exit;

  InternalCreateBitmap(AList, AList.Width, AList.Height, AData, hbmImage, hbmMask);
  ImageList_Replace(HImageList(AList.Handle), AIndex, hbmImage, hbmMask);
  InternalDestroyBitmap(hbmImage, hbmMask);
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TCustomImageList, TWin32WSCustomImageList);
////////////////////////////////////////////////////
end.
