// included by graphics.pp

{ TBitmapImage

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

destructor TBitmapImage.Destroy;
begin
  FreeAndNil(FSaveStream);
  FSaveStreamType:=bnNone;
  if FDIBHandle <> 0 then
  begin
    //TODO:write this function
    //DeselectBitmap(FDIBHandle);
    DeleteObject(FDIBHandle);
    FDIBHandle := 0;
  end;
  FreeHandle;
  //TODO Write CloseHandle
  //if FDIB.dshSection <> 0 then CloseHandle(FDIB.dshSection);
  inherited Destroy;
end;

function TBitmapImage.HandleAllocated: boolean;
begin
  Result:=FHandle<>0;
end;

function TBitmapImage.GetHandleType: TBitmapHandleType;
begin
  if (FHandle = 0) or (FHandle = FDIBHandle) then
    if FDIBHandle = 0 then
      if FDIB.dsbmih.biSize = 0 then
        Result := bmDDB
      else
        Result := bmDIB
    else
      Result := bmDIB
  else
    Result := bmDDB;
end;

procedure TBitmapImage.FreeHandle;
begin
  if (FHandle <> 0) and (FHandle <> FDIBHandle) then
  begin
    //TODO: Once written, remove comment below
    //DeselectBitmap(FHandle);
    DeleteObject(FHandle);
  end;
  if FMaskHandle <> 0 then
  begin
    //TODO: Once written, remove comment below
    //DeselectBitmap(FMaskHandle);
    DeleteObject(FMaskHandle);
    FMaskHandle := 0;
  end;
  //TODO: Once written, remove comment below
  //InternalDeletePalette(FPalette);
  FHandle := 0;
  FPalette := 0;
end;

function TBitmapImage.ReleaseHandle: HBITMAP;
begin
  Result:=FHandle;
  if FHandle = FDIBHandle then
  begin
    FDIBHandle := 0;
    FDIB.dsbm.bmBits := nil;
  end;
end;

function TBitmapImage.IsEmpty: boolean;
begin
  Result := (FHandle = 0) and (FDIBHandle = 0);
end;

// included by graphics.pp

