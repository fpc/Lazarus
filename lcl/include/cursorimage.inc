{%MainUnit ../graphics.pp}

{******************************************************************************
                             TCursorImage
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}


{ TSharedCursorImage }

procedure TSharedCursorImage.FreeHandle;
begin
  if FHandle = 0 then Exit;

  DestroyCursor(FHandle);
  FHandle := 0;
end;

class function TSharedCursorImage.GetImagesClass: TIconImageClass;
begin
  Result := TCursorImageImage;
end;

////////////////////////////////////////////////////////////////////////////////

{ TCursorImage }

class function TCursorImage.GetFileExtensions: string;
begin
  Result := 'cur';
end;

function TCursorImage.GetResourceType: TResourceType;
begin
  Result := RT_GROUP_CURSOR;
end;

class function TCursorImage.GetSharedImageClass: TSharedRasterImageClass;
begin
  Result := TSharedCursorImage;
end;

class function TCursorImage.GetTypeID: Word;
begin
  Result := 2;
end;

function TCursorImage.LazarusResourceTypeValid(const ResourceType: string): boolean;
var
  ResType: String;
begin
  if Length(ResourceType) < 3 then Exit(False);

  ResType := UpperCase(ResourceType);
  case ResType[1] of
    'C': begin
      Result := (ResType = 'CUR') or (ResType = 'CURSOR');
    end;
  else
    Result := inherited LazarusResourceTypeValid(ResType);
  end;
end;

function TCursorImage.ReleaseHandle: HCURSOR;
begin
  HandleNeeded;
  Result := FSharedImage.ReleaseHandle;
end;

function TCursorImage.GetCursorHandle: HCURSOR;
begin
  Result := GetHandle;
end;

procedure TCursorImage.SetCursorHandle(AValue: HCURSOR);
begin
  SetHandle(AValue);
end;

function TCursorImage.GetHotSpot: TPoint;
begin
  if FCurrent = -1
  then Result := Point(0, 0)
  else Result := TCursorImageImage(TSharedCursorImage(FSharedImage).FImages[FCurrent]).HotSpot;
end;

procedure TCursorImage.HandleNeeded;
var
  IconInfo: TIconInfo;
  h: TPoint;
begin
  if FSharedImage.FHandle <> 0 then Exit;

  IconInfo.fIcon := False;
  H := HotSpot;
  IconInfo.xHotspot := H.X;
  IconInfo.yHotSpot := H.Y;
  IconInfo.hbmMask := MaskHandle;
  IconInfo.hbmColor := BitmapHandle;
  FSharedImage.FHandle := WidgetSet.CreateIconIndirect(@IconInfo);
end;

class function TCursorImage.GetDefaultSize: TSize;
begin
  Result := Size(GetSystemMetrics(SM_CXCURSOR), GetSystemMetrics(SM_CYCURSOR));
end;


