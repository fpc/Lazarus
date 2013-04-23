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
    FImages_12: TCustomImageList;
    FImages_16: TCustomImageList;
    FImages_24: TCustomImageList;
    FImageNames_12: TStringList;
    FImageNames_16: TStringList;
    FImageNames_24: TStringList;
  protected
    function GetImages_12: TCustomImageList;
    function GetImages_16: TCustomImageList;
    function GetImages_24: TCustomImageList;
  public
    constructor Create;
    destructor Destroy; override;

    function GetImageIndex(ImageSize: Integer; ImageName: String): Integer;
    function LoadImage(ImageSize: Integer; ImageName: String): Integer;
    
    property Images_12: TCustomImageList read GetImages_12;
    property Images_16: TCustomImageList read GetImages_16;
    property Images_24: TCustomImageList read GetImages_24;
  end;

function IDEImages: TIDEImages;

implementation

var
  FIDEImages: TIDEImages;

{ TIDEImages }

function TIDEImages.GetImages_12: TCustomImageList;
begin
  if FImages_12 = nil then
  begin
    FImages_12 := TImageList.Create(nil);
    FImages_12.Width := 12;
    FImages_12.Height := 12;
  end;
  Result := FImages_12;
end;

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
  FImageNames_12 := TStringList.Create;
  FImageNames_12.Sorted := True;
  FImageNames_12.Duplicates := dupIgnore;
  FImageNames_16 := TStringList.Create;
  FImageNames_16.Sorted := True;
  FImageNames_16.Duplicates := dupIgnore;
  FImageNames_24 := TStringList.Create;
  FImageNames_24.Sorted := True;
  FImageNames_24.Duplicates := dupIgnore;
end;

destructor TIDEImages.Destroy;
begin
  FreeAndNil(FImages_12);
  FreeAndNil(FImages_16);
  FreeAndNil(FImages_24);
  FreeAndNil(FImageNames_12);
  FreeAndNil(FImageNames_16);
  FreeAndNil(FImageNames_24);
  inherited Destroy;
end;

function TIDEImages.GetImageIndex(ImageSize: Integer; ImageName: String): Integer;
var
  List: TStringList;
begin
  case ImageSize of
    12: List := FImageNames_12;
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
begin
  Result := GetImageIndex(ImageSize, ImageName);
  if Result <> -1 then Exit;

  case ImageSize of
    12:
      begin
        List := Images_12; // make sure we have a list
        Names := FImageNames_12;
      end;
    16:
      begin
        List := Images_16; // make sure we have a list
        Names := FImageNames_16;
      end;
    24:
      begin
        List := Images_24; // make sure we have a list
        Names := FImageNames_24;
      end;
  else
    Exit;
  end;
  try
    Result := List.AddLazarusResource(ImageName);
  except
    on E: Exception do
      DebugLn('While loading IDEImages: ' + e.Message);
  end;
  Names.AddObject(ImageName, TObject(PtrInt(Result)));
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
