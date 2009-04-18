{%MainUnit ../graphics.pp}

{ TSharedImage

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

procedure TSharedImage.Reference;
begin
  Inc(FRefCount);
end;

procedure TSharedImage.Release;
begin
  if Pointer(Self) <> nil then begin
    Dec(FRefCount);
    if FRefCount<0 then
      RaiseGDBException('');
    if FRefCount = 0 then begin
      FreeHandle;
      Free;
    end;
  end;
end;

// included by graphics.pp
