{
 *****************************************************************************
 *                               Gtk3WSImgList.pp                                * 
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
unit Gtk3WSImgList;

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
  Classes, GraphType, Graphics, IntfGraphics, ImgList, LCLType, LCLIntf,
  WSLCLClasses, WSProc, WSReferences, WSFactory, WSImgList;

type
  { TGtk3WSCustomImageList }

  TGtk3WSCustomImageList = class(TWSCustomImageList)
  published
    class procedure Clear(AList: TCustomImageList); override;
    class function  CreateReference(AList: TCustomImageList; ACount, AGrow, AWidth,
      AHeight: Integer; AData: PRGBAQuad): TWSCustomImageListReference; override;

    class procedure Delete(AList: TCustomImageList; AIndex: Integer); override;
    class procedure DestroyReference(AComponent: TComponent); override;
    class procedure Draw(AList: TCustomImageList; AIndex: Integer; ACanvas: TCanvas;
      ABounds: TRect; ABkColor, ABlendColor: TColor; ADrawEffect: TGraphicsDrawEffect; AStyle: TDrawingStyle; AImageType: TImageType); override;

    class procedure Insert(AList: TCustomImageList; AIndex: Integer; AData: PRGBAQuad); override;

    class procedure Move(AList: TCustomImageList; ACurIndex, ANewIndex: Integer); override;

    class procedure Replace(AList: TCustomImageList; AIndex: Integer; AData: PRGBAQuad); override;
  end;
  TGtk3WSCustomImageListClass = class of TGtk3WSCustomImageList;


implementation

{ TGtk3WSCustomImageList }

class procedure TGtk3WSCustomImageList.Clear(AList: TCustomImageList);
begin
  inherited Clear(AList);
end;

class function TGtk3WSCustomImageList.CreateReference(AList: TCustomImageList;
  ACount, AGrow, AWidth, AHeight: Integer; AData: PRGBAQuad
  ): TWSCustomImageListReference;
begin
  Result:=inherited CreateReference(AList, ACount, AGrow, AWidth, AHeight, AData
    );
end;

class procedure TGtk3WSCustomImageList.Delete(AList: TCustomImageList;
  AIndex: Integer);
begin
  inherited Delete(AList, AIndex);
end;

class procedure TGtk3WSCustomImageList.DestroyReference(AComponent: TComponent);
begin
  inherited DestroyReference(AComponent);
end;

class procedure TGtk3WSCustomImageList.Draw(AList: TCustomImageList;
  AIndex: Integer; ACanvas: TCanvas; ABounds: TRect; ABkColor,
  ABlendColor: TColor; ADrawEffect: TGraphicsDrawEffect; AStyle: TDrawingStyle;
  AImageType: TImageType);
begin
  inherited Draw(AList, AIndex, ACanvas, ABounds, ABkColor, ABlendColor,
    ADrawEffect, AStyle, AImageType);
end;

class procedure TGtk3WSCustomImageList.Insert(AList: TCustomImageList;
  AIndex: Integer; AData: PRGBAQuad);
begin
  inherited Insert(AList, AIndex, AData);
end;

class procedure TGtk3WSCustomImageList.Move(AList: TCustomImageList; ACurIndex,
  ANewIndex: Integer);
begin
  inherited Move(AList, ACurIndex, ANewIndex);
end;

class procedure TGtk3WSCustomImageList.Replace(AList: TCustomImageList;
  AIndex: Integer; AData: PRGBAQuad);
begin
  inherited Replace(AList, AIndex, AData);
end;

end.
