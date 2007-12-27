{ $Id$}
{
 *****************************************************************************
 *                               WSImgList.pp                                * 
 *                               ------------                                * 
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
unit WSImgList;

{$mode objfpc}{$H+}

interface
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// 1) Only class methods allowed
// 2) Class methods have to be published and virtual
// 3) To get as little as posible circles, the uses
//    clause should contain only those LCL units 
//    needed for registration. WSxxx units are OK
// 4) To improve speed, register only classes in the 
//    initialization section which actually 
//    implement something
// 5) To enable your XXX widgetset units, look at
//    the uses clause of the XXXintf.pp
////////////////////////////////////////////////////
uses
  Classes, Contnrs, GraphType, Graphics, ImgList, LCLType, LCLIntf,
  WSLCLClasses, WSProc;

type
  { TWSCustomImageList }

  TWSCustomImageList = class(TWSLCLHandleComponent)
    class procedure Clear(AList: TCustomImageList); virtual;
    class function  CreateHandle(AList: TCustomImageList; ACount, AGrow, AWidth,
      AHeight: Integer; AData: PRGBAQuad): TLCLIntfHandle; virtual;

    class procedure Delete(AList: TCustomImageList; AIndex: Integer); virtual;
    class procedure DestroyHandle(AComponent: TComponent); override;
    class procedure Draw(AList: TCustomImageList; AIndex: Integer; ACanvas: TCanvas;
      ABounds: TRect; ABkColor, ABlendColor: TColor; AEnabled: Boolean; AStyle: TDrawingStyle; AImageType: TImageType); virtual;

    class procedure Insert(AList: TCustomImageList; AIndex: Integer; AData: PRGBAQuad); virtual;

    class procedure Move(AList: TCustomImageList; ACurIndex, ANewIndex: Integer); virtual;

    class procedure Replace(AList: TCustomImageList; AIndex: Integer; AData: PRGBAQuad); virtual;
  end;
  TWSCustomImageListClass = class of TWSCustomImageList;

implementation

type

  { TDefaultImageListImplementor }

  TDefaultImageListImplementor = class(TObjectList)
  private
  public
    constructor Create(AWidth, AHeight: Integer); reintroduce;
    procedure Draw(AIndex: Integer; ACanvas: TCanvas; ABounds: TRect; AEnabled: Boolean; AStyle: TDrawingStyle);
  end;

{ TDefaultImageListImplementor }

constructor TDefaultImageListImplementor.Create(AWidth, AHeight: Integer);
begin
  inherited Create(True);
  FWidth := AWidth;
  FHeight := AHeight;
end;

procedure TDefaultImageListImplementor.Draw(AIndex: Integer; ACanvas: TCanvas;
  ABounds: TRect; AEnabled: Boolean; AStyle: TDrawingStyle);
var
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap(Items[AIndex]);
  ACanvas.Draw(ABounds.Left, ABounds.Top, Bitmap);
end;

function InternalCreateBitmap(AList: TCustomImageList; AWidth, AHeight: Integer; AData: PRGBAQuad): TBitmap;
var
  hbmImage, hbmMask: HBitmap;
  RawImg: TRawImage;
begin
  FillChar(RawImg, SizeOf(RawImg), 0);
  AList.FillDescription(RawImg.Description);
  RawImg.DataSize := AWidth * AHeight * SizeOF(AData[0]);
  RawImg.Data := PByte(AData);

  RawImage_CreateBitmaps(RawImg, hbmImage, hbmMask);
  Result := TBitmap.Create;
  Result.Handle := hbmImage;
  Result.MaskHandle := hbmMask;
end;


{ TWSCustomImageList }

class procedure TWSCustomImageList.Clear(AList: TCustomImageList);
begin
  if not WSCheckHandleAllocated(AList, 'Clear')
  then Exit;
  TDefaultImageListImplementor(AList.Handle).Clear;
end;

class function TWSCustomImageList.CreateHandle(AList: TCustomImageList; ACount,
  AGrow, AWidth, AHeight: Integer; AData: PRGBAQuad): TLCLIntfHandle;
var
  ABitmap: TBitmap;
  i: integer;
begin
  Result := TLCLIntfHandle(TDefaultImageListImplementor.Create(AWidth, AHeight));

  if AData <> nil then
  begin
    // this is very slow method :(
    for i := 0 to ACount - 1 do
    begin
      ABitmap := InternalCreateBitmap(AList, AWidth, AHeight, @AData[AWidth * AHeight * i]);
      TDefaultImageListImplementor(Result).Add(ABitmap);
    end;
  end;
end;

class procedure TWSCustomImageList.Delete(AList: TCustomImageList;
  AIndex: Integer);
begin
  if not WSCheckHandleAllocated(AList, 'Delete')
  then Exit;
  TDefaultImageListImplementor(AList.Handle).Delete(AIndex);
end;

class procedure TWSCustomImageList.DestroyHandle(AComponent: TComponent);
begin
  if not WSCheckHandleAllocated(TCustomImageList(AComponent), 'DestroyHandle')
  then Exit;
  TDefaultImageListImplementor(TCustomImageList(AComponent).Handle).Free;
end;

class procedure TWSCustomImageList.Draw(AList: TCustomImageList; AIndex: Integer;
  ACanvas: TCanvas; ABounds: TRect; ABkColor, ABlendColor: TColor; AEnabled: Boolean; AStyle: TDrawingStyle; AImageType: TImageType);
begin
  if not WSCheckHandleAllocated(AList, 'Draw')
  then Exit;

  TDefaultImageListImplementor(AList.Handle).Draw(AIndex, ACanvas, ABounds, AEnabled, AStyle);
end;

class procedure TWSCustomImageList.Insert(AList: TCustomImageList;
  AIndex: Integer; AData: PRGBAQuad);
var
  AImageList: TDefaultImageListImplementor;
  ACount: Integer;
  ABitmap: TBitmap;
begin
  if not WSCheckHandleAllocated(AList, 'Insert')
  then Exit;

  AImageList := TDefaultImageListImplementor(AList.Handle);
  ACount := AImageList.Count;

  if (AIndex <= ACount) and (AIndex >= 0) then
  begin
    ABitmap := InternalCreateBitmap(AList, AList.Width, AList.Height, AData);
    AImageList.Add(ABitmap);
    if AIndex <> ACount then
      Move(AList, ACount, AIndex);
  end;
end;

class procedure TWSCustomImageList.Move(AList: TCustomImageList; ACurIndex,
  ANewIndex: Integer);
begin
  if not WSCheckHandleAllocated(AList, 'Move')
  then Exit;

  if ACurIndex = ANewIndex
  then Exit;

  TDefaultImageListImplementor(AList.Handle).Move(ACurIndex, ANewIndex);
end;

class procedure TWSCustomImageList.Replace(AList: TCustomImageList;
  AIndex: Integer; AData: PRGBAQuad);
var
  ABitmap: TBitmap;
begin
  if not WSCheckHandleAllocated(AList, 'Replace')
  then Exit;

  ABitmap := InternalCreateBitmap(AList, AList.Width, AList.Height, AData);
  TDefaultImageListImplementor(AList.Handle)[AIndex] := ABitmap;
end;

initialization

////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TCustomImageList, TWSCustomImageList);
////////////////////////////////////////////////////
end.
