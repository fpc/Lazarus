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
  Classes, ImgList, Graphics, LCLType,
  WSLCLClasses;

type
  { TWSCustomImageList }

  TWSCustomImageList = class(TWSLCLHandleComponent)
    class procedure Clear(AList: TCustomImageList); virtual;
    class function  CreateHandle(AList: TCustomImageList; ACount, AGrow, AWidth,
      AHeight: Integer; AData: PRGBAQuad): TLCLIntfHandle; virtual;

    class procedure Delete(AList: TCustomImageList; AIndex: Integer); virtual;
    class procedure DestroyHandle(AList: TCustomImageList); virtual;
    class procedure Draw(AList: TCustomImageList; AIndex: Integer; ACanvas: TCanvas;
      ABounds: TRect; AEnabled: Boolean; AStyle: TDrawingStyle); virtual;

    class procedure Insert(AList: TCustomImageList; AIndex: Integer; AData: PRGBAQuad); virtual;

    class procedure Move(AList: TCustomImageList; ACurIndex, ANewIndex: Integer); virtual;

    class procedure Replace(AList: TCustomImageList; AIndex: Integer; AData: PRGBAQuad); virtual;
  end;

implementation

{ TWSCustomImageList }


class procedure TWSCustomImageList.Clear(AList: TCustomImageList);
begin
end;

class function TWSCustomImageList.CreateHandle(AList: TCustomImageList; ACount,
  AGrow, AWidth, AHeight: Integer; AData: PRGBAQuad): TLCLIntfHandle;
begin
  Result := 0;
end;

class procedure TWSCustomImageList.Delete(AList: TCustomImageList;
  AIndex: Integer);
begin
end;

class procedure TWSCustomImageList.DestroyHandle(AList: TCustomImageList);
begin
end;

class procedure TWSCustomImageList.Draw(AList: TCustomImageList; AIndex: Integer;
  ACanvas: TCanvas; ABounds: TRect; AEnabled: Boolean; AStyle: TDrawingStyle);
begin
end;

class procedure TWSCustomImageList.Insert(AList: TCustomImageList;
  AIndex: Integer; AData: PRGBAQuad);
begin
end;

class procedure TWSCustomImageList.Move(AList: TCustomImageList; ACurIndex,
  ANewIndex: Integer);
begin
end;

class procedure TWSCustomImageList.Replace(AList: TCustomImageList;
  AIndex: Integer; AData: PRGBAQuad);
begin
end;

initialization

////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TCustomImageList, TWSCustomImageList);
////////////////////////////////////////////////////
end.
