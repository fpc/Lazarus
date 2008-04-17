{%MainUnit ../graphics.pp}

{******************************************************************************
                                  TBitmapCanvas
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
{------------------------------------------------------------------------------
  Method: TBitmapCanvas.Create
  Params:  ABitMap: the owner of the class
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TBitmapCanvas.Create(ABitmap : TCustomBitmap);
begin
  inherited Create;
  FBitmap := ABitmap;
end;

{------------------------------------------------------------------------------
  Method:  TBitmapCanvas.CreateHandle
  Params:  None
  Returns: Nothing

  Creates the handle.
 ------------------------------------------------------------------------------}
procedure TBitmapCanvas.CreateHandle;
var
  DC: HDC;
begin
  if HandleAllocated then exit;
  if FBitmap = nil then exit;
  FBitmap.HandleNeeded;
  FBitmap.PaletteNeeded;
  if FBitmap.FImage.BitmapCanvas <> nil then
    (FBitmap.FImage.BitmapCanvas as TBitmapCanvas).FreeDC;
  FBitmap.FImage.BitmapCanvas := Self;
  DC := CreateCompatibleDC(0);

  FOldBitmap := 0;
  FOldBitmapValid:=false;
  if FBitmap.HandleAllocated then begin
    FOldBitmap := SelectObject(DC, FBitmap.Handle);
    FOldBitmapValid:=true;
  end;

  FOldPalette:=0;
  FOldPaletteValid:=false;
  if FBitmap.PaletteAllocated then begin
    FOldPalette := SelectPalette(DC, FBitmap.FImage.FPalette, True);
    FOldPaletteValid:=true;
    RealizePalette(DC);
  end;

  Handle := DC;
  //DebugLn('TBitmapCanvas.CreateHandle END Self=',DbgS(Self),' DC=',DbgS(DC),
  //  ' Handle=',DbgS(GetUpdatedHandle([csHandleValid])));
end;

{------------------------------------------------------------------------------
  Method: TBitmapCanvas.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TBitmapCanvas.Destroy;
begin
  FreeDC;
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method:  TControlCanvas.FreeDC
  Params:  None
  Returns: Nothing

  Frees the device context
 ------------------------------------------------------------------------------}
procedure TBitmapCanvas.FreeDC;
var
  OldHandle: HBITMAP;
begin
  if not HandleAllocated then exit;
  //DebugLn('TBitmapCanvas.FreeDC START Self=',DbgS(Self),' FBitmap=',DbgS(FBitmap));
  if FBitmap<>nil then begin
    OldHandle := FHandle;
    Handle := 0;
    FBitmap.FImage.BitmapCanvas := nil;
    if FOldBitmapValid then begin
      SelectObject(OldHandle, FOldBitmap);
      FOldBitmap:=0;
      FOldBitmapValid:=false;
    end;
    if FOldPaletteValid then begin
      SelectPalette(OldHandle, FOldPalette, True);
      FOldPalette:=0;
      FOldPaletteValid:=false;
    end;
    DeleteDC(OldHandle);
  end else begin
    Handle:=0;
  end;
end;

// included by graphics.pp

