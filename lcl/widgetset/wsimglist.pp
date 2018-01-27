{ $Id$}
{
 *****************************************************************************
 *                               WSImgList.pp                                * 
 *                               ------------                                * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit WSImgList;

{$mode objfpc}{$H+}
{$I lcl_defines.inc}

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
  Classes, GraphType, Graphics, IntfGraphics, ImgList, LCLType, LCLIntf,
  WSLCLClasses, WSProc, WSReferences, WSFactory;

type
  { TWSCustomImageListResolution }

  TWSCustomImageListResolution = class(TWSLCLReferenceComponent)
  published
    class procedure Clear(AList: TCustomImageListResolution); virtual;
    class function  CreateReference(AList: TCustomImageListResolution; ACount, AGrow, AWidth,
      AHeight: Integer; AData: PRGBAQuad): TWSCustomImageListReference; virtual;

    class procedure Delete(AList: TCustomImageListResolution; AIndex: Integer); virtual;
    class procedure DestroyReference(AComponent: TComponent); override;
    class procedure Draw(AList: TCustomImageListResolution; AIndex: Integer; ACanvas: TCanvas;
      ABounds: TRect; ABkColor, ABlendColor: TColor; ADrawEffect: TGraphicsDrawEffect; AStyle: TDrawingStyle; AImageType: TImageType); virtual;

    class procedure Insert(AList: TCustomImageListResolution; AIndex: Integer; AData: PRGBAQuad); virtual;

    class procedure Move(AList: TCustomImageListResolution; ACurIndex, ANewIndex: Integer); virtual;

    class procedure Replace(AList: TCustomImageListResolution; AIndex: Integer; AData: PRGBAQuad); virtual;
  end;
  TWSCustomImageListResolutionClass = class of TWSCustomImageListResolution;

  procedure RegisterCustomImageListResolution;

implementation

type

  { TDefaultImageListImplementor }

  // Dont use TObjectList due to a bug in it (fixed in fpc > 2.2.2)
  TDefaultImageListImplementor = class(TList)
  private
    FList: TCustomImageListResolution;
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    constructor Create(AList: TCustomImageListResolution); reintroduce;
    procedure Draw(AIndex: Integer; ACanvas: TCanvas; ABounds: TRect; ADrawEffect: TGraphicsDrawEffect; AStyle: TDrawingStyle);
  end;

{ TDefaultImageListImplementor }

procedure TDefaultImageListImplementor.Notify(Ptr: Pointer;
  Action: TListNotification);
begin
  if Action = lnDeleted then
    TBitmap(Ptr).Free;
  inherited Notify(Ptr, Action);
end;

constructor TDefaultImageListImplementor.Create(AList: TCustomImageListResolution);
begin
  inherited Create;
  FList := AList;
end;

procedure TDefaultImageListImplementor.Draw(AIndex: Integer; ACanvas: TCanvas;
  ABounds: TRect; ADrawEffect: TGraphicsDrawEffect; AStyle: TDrawingStyle);
var
  ABitmap: TBitmap;
  RawImg: TRawImage;
  ListImg, DeviceImg: TLazIntfImage;
  ImgHandle, MskHandle: HBitmap;
begin
  if (AIndex < 0) or (AIndex >= Count) then Exit;
  if ADrawEffect = gdeNormal then
  begin
    ABitmap := TBitmap(Items[AIndex]);
    ACanvas.Draw(ABounds.Left, ABounds.Top, ABitmap);
  end
  else
  begin
    FList.GetRawImage(AIndex, RawImg);
    RawImg.PerformEffect(ADrawEffect, True);

    ABitmap := TBitmap.Create;
    if not CreateCompatibleBitmaps(RawImg, ImgHandle, MskHandle, True)
    then begin
      // bummer, the widgetset doesn't support our 32bit format, try device
      ListImg := TLazIntfImage.Create(RawImg, False);
      DeviceImg := TLazIntfImage.Create(0, 0, []);
      DeviceImg.DataDescription := GetDescriptionFromDevice(0, FList.Width, FList.Height);
      DeviceImg.CopyPixels(ListImg);
      DeviceImg.GetRawImage(RawImg);
      RawImage_CreateBitmaps(RawImg, ImgHandle, MskHandle);
      DeviceImg.Free;
      ListImg.Free;
    end;
    ABitmap.SetHandles(ImgHandle, MskHandle);
    ACanvas.Draw(ABounds.Left, ABounds.Top, ABitmap);
    ABitmap.Free;
    RawImg.FreeData;
  end;
end;

function InternalCreateBitmap(AList: TCustomImageListResolution; AWidth, AHeight: Integer; AData: PRGBAQuad): TBitmap;
var
  hbmImage, hbmMask: HBitmap;
  RawImg: TRawImage;
begin
  FillChar(RawImg, SizeOf(RawImg), 0);
  AList.FillDescription(RawImg.Description);
  RawImg.DataSize := AWidth * AHeight * SizeOF(AData[0]);
  RawImg.Data := PByte(AData);

  CreateCompatibleBitmaps(RawImg, hbmImage, hbmMask);
  //RawImage_CreateBitmaps(RawImg, hbmImage, hbmMask);
  Result := TBitmap.Create;
  Result.SetHandles(hbmImage, hbmMask);
end;


{ TWSCustomImageListResolution }

class procedure TWSCustomImageListResolution.Clear(AList: TCustomImageListResolution);
begin
  if not WSCheckReferenceAllocated(AList, 'Clear')
  then Exit;
  TDefaultImageListImplementor(AList.Reference.Ptr).Clear;
end;

class function TWSCustomImageListResolution.CreateReference(
  AList: TCustomImageListResolution; ACount, AGrow, AWidth, AHeight: Integer;
  AData: PRGBAQuad): TWSCustomImageListReference;
var
  impl: TDefaultImageListImplementor;

  ABitmap: TBitmap;
  i: integer;
begin
  impl := TDefaultImageListImplementor.Create(AList);
  Result{%H-}._Init(impl);

  if AData <> nil then
  begin
    // this is very slow method :(
    for i := 0 to ACount - 1 do
    begin
      ABitmap := InternalCreateBitmap(AList, AWidth, AHeight, @AData[AWidth * AHeight * i]);
      impl.Add(ABitmap);
    end;
  end;
end;

class procedure TWSCustomImageListResolution.Delete(AList: TCustomImageListResolution;
  AIndex: Integer);
begin
  if not WSCheckReferenceAllocated(AList, 'Delete')
  then Exit;
  TDefaultImageListImplementor(AList.Reference.Ptr).Delete(AIndex);
end;

class procedure TWSCustomImageListResolution.DestroyReference(AComponent: TComponent);
begin
  if not WSCheckReferenceAllocated(TCustomImageListResolution(AComponent), 'DestroyReference')
  then Exit;
  TObject(TCustomImageListResolution(AComponent).Reference.Ptr).Free;
end;

class procedure TWSCustomImageListResolution.Draw(AList: TCustomImageListResolution;
  AIndex: Integer; ACanvas: TCanvas; ABounds: TRect; ABkColor,
  ABlendColor: TColor; ADrawEffect: TGraphicsDrawEffect; AStyle: TDrawingStyle;
  AImageType: TImageType);
begin
  if not WSCheckReferenceAllocated(AList, 'Draw')
  then Exit;

  TDefaultImageListImplementor(AList.Reference.Ptr).Draw(AIndex, ACanvas, ABounds, ADrawEffect, AStyle);
end;

class procedure TWSCustomImageListResolution.Insert(AList: TCustomImageListResolution;
  AIndex: Integer; AData: PRGBAQuad);
var
  AImageList: TDefaultImageListImplementor;
  ACount: Integer;
  ABitmap: TBitmap;
begin
  if not WSCheckReferenceAllocated(AList, 'Insert')
  then Exit;

  AImageList := TDefaultImageListImplementor(AList.Reference.Ptr);
  ACount := AImageList.Count;

  if (AIndex <= ACount) and (AIndex >= 0) then
  begin
    ABitmap := InternalCreateBitmap(AList, AList.Width, AList.Height, AData);
    AImageList.Add(ABitmap);
    if AIndex <> ACount then
      Move(AList, ACount, AIndex);
  end;
end;

class procedure TWSCustomImageListResolution.Move(AList: TCustomImageListResolution;
  ACurIndex, ANewIndex: Integer);
begin
  if not WSCheckReferenceAllocated(AList, 'Move')
  then Exit;

  if ACurIndex = ANewIndex
  then Exit;

  TDefaultImageListImplementor(AList.Reference.Ptr).Move(ACurIndex, ANewIndex);
end;

class procedure TWSCustomImageListResolution.Replace(AList: TCustomImageListResolution;
  AIndex: Integer; AData: PRGBAQuad);
var
  ABitmap: TBitmap;
begin
  if not WSCheckReferenceAllocated(AList, 'Replace')
  then Exit;

  ABitmap := InternalCreateBitmap(AList, AList.Width, AList.Height, AData);
  TDefaultImageListImplementor(AList.Reference.Ptr)[AIndex] := ABitmap;
end;

{ WidgetSetRegistration }

procedure RegisterCustomImageListResolution;
const
  Done: Boolean = False;
begin
  if Done then exit;
  if not WSRegisterCustomImageListResolution then
    RegisterWSComponent(TCustomImageListResolution, TWSCustomImageListResolution);
  Done := True;
end;

end.
