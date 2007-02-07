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
//    class function  CreateBitmap(AList: TCustomImageList; AIndex: Integer; const ABounds: TRect; AEnabled: Boolean; AStyle: TDrawingStyle);
    class function  CreateHandle(AList: TCustomImageList; ACount, AGrow: Integer; const AParams: TCreateParams): TLCLIntfHandle; virtual;

    class procedure Delete(AList: TCustomImageList; AIndex: Integer); virtual;
    class procedure Draw(AList: TCustomImageList; ACanvas: TCanvas; ABounds: TRect; AEnabled: Boolean; AStyle: TDrawingStyle); virtual;

    class procedure Insert(AList: TCustomImageList; AIndex: Integer; AImage, AMask: TBitmap); virtual;
    class procedure InsertIcon(AList: TCustomImageList; AIndex: Integer; AImage: TIcon); virtual;
    class procedure InsertMasked(AList: TCustomImageList; Index: Integer; Image: TBitmap; MaskColor: TColor); virtual;

    class procedure Move(AList: TCustomImageList; ACurIndex, ANewIndex: Integer); virtual;

    class procedure Replace(AList: TCustomImageList; AIndex: Integer; AImage, AMask: TBitmap); virtual;
    class procedure ReplaceIcon(AList: TCustomImageList; AIndex: Integer; AImage: TIcon); virtual;
    class procedure ReplaceMasked(AList: TCustomImageList; AIndex: Integer; ANewImage: TBitmap; AMaskColor: TColor); virtual;
  end;


implementation

{ TWSCustomImageList }

class procedure TWSCustomImageList.Clear(AList: TCustomImageList);
begin
end;

class function TWSCustomImageList.CreateHandle(AList: TCustomImageList; ACount, AGrow: Integer; const AParams: TCreateParams): TLCLIntfHandle;
begin
end;

class procedure TWSCustomImageList.Delete(AList: TCustomImageList; AIndex: Integer);
begin
end;

class procedure TWSCustomImageList.Draw(AList: TCustomImageList; ACanvas: TCanvas; ABounds: TRect; AEnabled: Boolean; AStyle: TDrawingStyle);
begin
end;

class procedure TWSCustomImageList.Insert(AList: TCustomImageList; AIndex: Integer; AImage, AMask: TBitmap);
begin
end;

class procedure TWSCustomImageList.InsertIcon(AList: TCustomImageList; AIndex: Integer; AImage: TIcon);
begin
end;

class procedure TWSCustomImageList.InsertMasked(AList: TCustomImageList; Index: Integer; Image: TBitmap; MaskColor: TColor);
begin
end;

class procedure TWSCustomImageList.Move(AList: TCustomImageList; ACurIndex, ANewIndex: Integer);
begin
end;

class procedure TWSCustomImageList.Replace(AList: TCustomImageList; AIndex: Integer; AImage, AMask: TBitmap);
begin
end;

class procedure TWSCustomImageList.ReplaceIcon(AList: TCustomImageList; AIndex: Integer; AImage: TIcon);
begin
end;

class procedure TWSCustomImageList.ReplaceMasked(AList: TCustomImageList; AIndex: Integer; ANewImage: TBitmap; AMaskColor: TColor);
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