{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}
unit IDEImagesIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, ImgList, Controls, Graphics, LResources;

type

  { TIDEImages }

  TIDEImages = class
  private
    FImages_16: TCustomImageList;
    FImages_24: TCustomImageList;
    FImageNames_16: TStringList;
    FImageNames_24: TStringList;
  protected
    function GetImages_16: TCustomImageList;
    function GetImages_24: TCustomImageList;
  public
    constructor Create;
    destructor Destroy; override;

    function GetImageIndex(ImageSize: Integer; ImageName: String): Integer;
    function LoadImage(ImageSize: Integer; ImageName: String): Integer;
    
    property Images_16: TCustomImageList read GetImages_16;
    property Images_24: TCustomImageList read GetImages_24;
  end;

function IDEImages: TIDEImages;

implementation

var
  FIDEImages: TIDEImages;

{ TIDEImages }

function TIDEImages.GetImages_16: TCustomImageList;
begin
  if FImages_16 = nil then
  begin
    FImages_16 := TImageList.Create(nil);
    FImages_16.Width := 16;
    FImages_16.Height := 16;
  end;
  Result := FImages_16;
end;

function TIDEImages.GetImages_24: TCustomImageList;
begin
  if FImages_24 = nil then
  begin
    FImages_24 := TImageList.Create(nil);
    FImages_24.Width := 24;
    FImages_24.Height := 24;
  end;
  Result := FImages_24;
end;

constructor TIDEImages.Create;
begin
  FImages_16 := nil;
  FImages_24 := nil;
  FImageNames_16 := TStringList.Create;
  FImageNames_16.Sorted := True;
  FImageNames_16.Duplicates := dupIgnore;
  FImageNames_24 := TStringList.Create;
  FImageNames_24.Sorted := True;
  FImageNames_24.Duplicates := dupIgnore;
end;

destructor TIDEImages.Destroy;
begin
  FImages_16.Free;
  FImages_24.Free;
  FImageNames_16.Free;
  FImageNames_24.Free;
  inherited Destroy;
end;

function TIDEImages.GetImageIndex(ImageSize: Integer; ImageName: String): Integer;
var
  List: TStringList;
begin
  case ImageSize of
    16: List := FImageNames_16;
    24: List := FImageNames_24;
  else
    List := nil;
  end;
  if List <> nil then
  begin
    Result := List.IndexOf(ImageName);
    if Result <> -1 then
      Result := PtrInt(List.Objects[Result]);
  end
  else
    Result := -1;
end;

function TIDEImages.LoadImage(ImageSize: Integer; ImageName: String): Integer;
var
  List: TCustomImageList;
  Names: TStringList;
  Graphic: TGraphic;
  GraphicClass: TGraphicClass;
  Stream: TLazarusResourceStream;
begin
  Result := GetImageIndex(ImageSize, ImageName);
  if Result = -1 then
  begin
    List := nil;
    Names := nil;
    case ImageSize of
      16:
        begin
          List := FImages_16;
          Names := FImageNames_16;
        end;
      24:
        begin
          List := FImages_24;
          Names := FImageNames_24;
        end;
    end;
    
    if List <> nil then
    begin
      try
        Stream := TLazarusResourceStream.Create(ImageName, nil);
        if (Stream.Res <> nil) then
        begin
          GraphicClass := GetGraphicClassForFileExtension(Stream.Res.ValueType);
          if GraphicClass <> nil then
          begin
            Graphic := GraphicClass.Create;
            if Graphic is TBitmap then
            try
              Graphic.LoadFromStream(Stream);
              Result := List.Add(TBitmap(Graphic), nil);
              Names.AddObject(ImageName, TObject(PtrInt(Result)));
              //DebugLn(['TIDEImages.LoadImage ',ImageName,' ',Graphic.Transparent]);
              //if Graphic is TBitmap then
              //  DebugLn(['TIDEImages.LoadImage ',dbgs(TBitmap(Graphic).TransparentColor),' ',dbgs(ord(TBitmap(Graphic).TransparentMode))]);
            finally
              Graphic.Free;
            end;
          end;
        end;
        Stream.Free;
      except
      end;
    end;
  end;
end;

function IDEImages: TIDEImages;
begin
  if FIDEImages = nil then
    FIDEImages := TIDEImages.Create;
  Result := FIDEImages;
end;

initialization
  FIDEImages := nil;

finalization
  FIDEImages.Free;
  FIDEImages:=nil;

end.
