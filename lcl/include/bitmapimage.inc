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
  //TODO Write CLoseHandle
  //if FDIB.dshSection <> 0 then CloseHandle(FDIB.dshSection);
  inherited Destroy;
end;

function TBitmapImage.HandleAllocated: boolean;
begin
  Result:=FHandle<>0;
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
