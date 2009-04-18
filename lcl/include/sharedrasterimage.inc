{%MainUnit ../graphics.pp}

{ TSharedRasterImage

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

constructor TSharedRasterImage.Create;
begin
  inherited Create;
end;

destructor TSharedRasterImage.Destroy;
begin
  FreeAndNil(FSaveStream);
  FreeHandle;
  inherited Destroy;
end;

function TSharedRasterImage.HandleAllocated: boolean;
begin
  Result := FHandle <> 0;
end;

procedure TSharedRasterImage.FreeHandle;
begin
  if FHandle = 0 then Exit;
  
  DeleteObject(FHandle);
  FHandle := 0;
end;

function TSharedRasterImage.ReleaseHandle: THandle;
// simply return the current handle and set to 0 without freeing handles
begin
  Result := FHandle;
  FHandle := 0;
end;


function TSharedRasterImage.IsEmpty: boolean;
begin
  Result := (FHandle = 0) and (SaveStream = nil);
end;

// included by graphics.pp
