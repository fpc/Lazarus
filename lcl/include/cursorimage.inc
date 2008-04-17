{%MainUnit ../graphics.pp}

{******************************************************************************
                             TCursorImage
 ******************************************************************************

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

{ TCursorImage }

class function TCursorImage.GetFileExtensions: string;
begin
  Result := 'cur';
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

function TCursorImage.ReleaseCursorHandle: HCURSOR;
begin
  Result := CursorHandle;
  FCursorHandle := 0;
end;

function TCursorImage.GetCursorHandle: HCURSOR;
begin
  CursorHandleNeeded;
  Result := FCursorHandle;
end;

procedure TCursorImage.CursorHandleNeeded;
var
  IconInfo: TIconInfo;
begin
  if FCursorHandle = 0 then
  begin
    IconInfo.fIcon := False;
    IconInfo.xHotspot := HotSpot.X;
    IconInfo.yHotSpot := HotSpot.Y;
    IconInfo.hbmMask := MaskHandle;
    IconInfo.hbmColor := Handle;
    FCursorHandle := WidgetSet.CreateCursor(@IconInfo);
  end;
end;

constructor TCursorImage.Create;
begin
  inherited Create;
  FHotSpot := Point(0, 0);
  FCursorHandle := 0;
  FOwnHandle := True;
end;

destructor TCursorImage.Destroy;
begin
  if (FCursorHandle <> 0) then
    WidgetSet.DestroyCursor(FCursorHandle);
  inherited Destroy;
end;

function TCursorImage.GetBitmapNativeType: TBitmapNativeType;
begin
  Result := bnCursor;
end;


