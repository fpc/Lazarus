{******************************************************************************
                                  TCustomImageList
 ******************************************************************************

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

const
  SInvalidIndex = 'Invalid ImageList Index';
  SInvalidImageSize = 'Invalid image size';

{------------------------------------------------------------------------------
  Method:  CopyImage
  Params:  Destination, Source: the destination/source canvas
  	       DestinationRect: the rectangle where the image is copied to 
  	       SourceRect: the rectangle containing the part to be copied 
  Returns: Nothing

  Internal routine to copy a rectangle from a source canvas to a rectangle on 
  the destination canvas
 ------------------------------------------------------------------------------}
procedure CopyImage(Destination, Source: TCanvas; DestinationRect, SourceRect: TRect);
begin
  Destination.CopyRect(
    DestinationRect,
    Source,
    SourceRect
  );
end;


{ TCustomImageList }

{------------------------------------------------------------------------------
  Function: TCustomImageList.Add
  Params:   Image: a bitmap image
            Mask: a bitmap which defines the transparent parts of Image
  Returns:  The index of the added image, -1 if unsuccesfull.

  Adds one or more (bitmap width / imagelist width) bitmaps to the list.
  If Mask is nil, the image has no transparent parts.
 ------------------------------------------------------------------------------}
function TCustomImageList.Add(Image, Mask: TBitmap): Integer;
begin
  try
    Result := Count;
    Insert(Result, Image, Mask);
    Change;
  except
    on E: Exception do begin
      writeln('TCustomImageList.Add ',E.Message);
      Result := -1; // Ignore exceptions, just return -1
    end;
  end;
end;

{------------------------------------------------------------------------------
  Function: TCustomImageList.AddIcon
  Params:   Image: the Icon to be added;
  Returns:  The index of the added icon, -1 if unsuccesfull.

  Adds an icon to the list.
 ------------------------------------------------------------------------------}
function TCustomImageList.AddIcon(Image: TIcon): Integer;
begin 
  //!!! check one or more

  //No Icon Support yet

  Result := -1;
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.AddImages
  Params:  Value: An imagelist containing images to be added
  Returns: Nothing

  Adds images from another imagelist to the list.
 ------------------------------------------------------------------------------}
procedure TCustomImageList.AddImages(Value: TCustomImageList);
var
  n: Integer;
  Image, Mask: TBitmap;
begin
  //!!! check one or more
  if Value <> nil 
  then begin
    Image := TBitmap.Create;
    try
      with Image do
      begin
        Height := FHeight;
        Width := FWidth;
      end;
      Mask := TBitmap.Create;
      try
        with Mask do
        begin
          Height := FHeight;
          Width := FWidth;
        end;
        with Value do 
          for n := 0 to Count - 1 do 
        begin
          GetImages(n, Image, Mask);
          Add(Image, Mask);
        end;
        Change;
      finally
        Mask.Free;
      end;
    finally
      Image.Free;
    end;
  end;
end;

{------------------------------------------------------------------------------
  Function: TCustomImageList.AddMasked
  Params:   Image: A bitmap to be added
            MaskColor: The color acting as transparant color
  Returns:  The index of the added icon, -1 if unsuccesfull.

  Adds one or more (bitmap width / imagelist width) bitmaps to the list. 
  Every occurance of MaskColor will be converted to transparent.
 ------------------------------------------------------------------------------}
function TCustomImageList.AddMasked(Image: TBitmap; MaskColor: TColor): Integer;
begin
  try
    Result := Count;
    InsertMasked(Result, Image, MaskColor);
    Change;
  except
    on E: Exception do begin
      writeln('TCustomImageList.AddMasked ',E.Message);
      Result := -1; // Ignore exceptions, just return -1
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.AllocBitmap
  Params:  Amount: the amount of free image position which should be availabe 
  Returns: Nothing

  Checks if there is enough space for Amount images,  increases the internal 
  list if necessary .
 ------------------------------------------------------------------------------}
procedure TCustomImageList.AllocBitmap(Amount: Integer);
var
  Num: Integer;
begin
  Assert(FAllocCount >= FCount, 'Less space allocated than images');
  if FAllocCount < FCount + Amount
  then begin
    // calculate number of blocks
    Num := Amount div FAllocBy;
    // add an extra block for the remainder.
    if Amount mod FAllocBy <> 0 then Inc(Num);

    FBitMap.Height := FBitMap.Height + Num * FAllocBy * FHeight;
    FMaskBitmap.Height := FBitMap.Height;
    Inc(FAllocCount, Num * FAllocBy);
  end;

  //raise Exception.Create('Unable to allocate bitmap space');
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.Change
  Params:  None
  Returns: Nothing

  Fires the change event.
 ------------------------------------------------------------------------------}
procedure TCustomImageList.Change;
begin
  NotifyChangeLink;
  if Assigned(FOnChange) then FOnChange(Self);
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.Clear
  Params:  None
  Returns: Nothing

  Clears the list.
 ------------------------------------------------------------------------------}
procedure TCustomImageList.Clear;
begin
  FCount := 0;
  FImageList.Clear;
  Change;
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.Create
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TCustomImageList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHeight := 16;
  FWidth := 16;
  FImageList := TList.Create;  //shane
  Initialize;
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.CreateSize
  Params:  AHeight: The height of an image 
           AWidth: The width of an image
  Returns: Nothing

  Runtime constructor for the class with a given width and height.
 ------------------------------------------------------------------------------}
constructor TCustomImageList.CreateSize(AWidth, AHeight: Integer);
begin
  inherited Create(nil);
  FHeight := AHeight;
  FWidth := AWidth;
  FImageList := TList.Create;  //shane
  Initialize;
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.Delete
  Params:  Index: the index of the image to be deleted.
  Returns: Nothing

  Deletes the image identified by Index. An index of -1 deletes all
 ------------------------------------------------------------------------------}
procedure TCustomImageList.Delete(Index: Integer);
begin
  if {(Index < 0) or} (Index >= FCount) // !! Delphi4 has no check for < -1
  then raise EInvalidOperation.Create(SInvalidIndex);

  if Index = -1
  then Clear
  else begin

//    ShiftImages(FBitmap.Canvas, Index, 1);
//    ShiftImages(FMaskBitmap.Canvas, Index, 1);
  FImageList.Delete(Index);  //shane
  Change;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomImageList.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TCustomImageList.Destroy;
var i: integer;
begin
  FBitmap.Free;
  FMaskBitmap.Free;
  FChangeLinkList.Free;
  for i:=0 to FImageList.Count-1 do TObject(FImageList[i]).Free;
  FImageList.Free;  //shane
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.Draw
  Params:  Canvas: the canvas to draw on
           X, Y: co-ordinates of the top, left corner of thetarget location 
           Index: index of the image to be drawn
           Enabled: True, draws the image
                    False, draws the image disabled (embossed)
  Returns: Nothing

  Draws the requested image on the given canvas.
 ------------------------------------------------------------------------------}
procedure TCustomImageList.Draw(Canvas: TCanvas; X, Y, Index: Integer;
  Enabled: Boolean);
var
  aBitmap : TBitmap;
begin
  if (FCount = 0) or (Index >= FCount) then Exit;
  aBitmap := TBitmap(FImageList[Index]);

  Canvas.CopyRect(Rect(X, Y, X + FWidth, Y + FHeight), aBitmap.Canvas,
                  Rect(0, 0, FWidth, FHeight));
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.GetBitmap
  Params:  Index: the index of the requested image
           Image: a bitmap as a container for the bitmap
  Returns: Nothing

  Fetches the index'th image into a bitmap.
 ------------------------------------------------------------------------------}
procedure TCustomImageList.GetBitmap(Index: Integer; Image: TBitmap);
begin
  if (FCount = 0) or (Image = nil) then Exit; 

  with Image do
  begin
    Width := FWidth;
    Height := FHeight;
    Self.Draw(Canvas, 0, 0, Index, True);
  end;

end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.GetCount
  Params:
  Returns: Count

  Fetches the index'th image into an icon.
 ------------------------------------------------------------------------------}
Function TCustomImageList.GetCount : Integer;
begin
  Result := FImageList.Count;
  FCount := Result;
end;


{------------------------------------------------------------------------------
  Function: TCustomImageList.GetHotspot
  Params:   None
  Returns:  The co-ordinates for the hotspot of the drag image

  Returns the co-ordinates for the hotspot of the drag image.
 ------------------------------------------------------------------------------}
function TCustomImageList.GetHotSpot: TPoint;
begin
  Result := Point(0, 0);
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.GetIcon
  Params:  Index: the index of the requested image
           Image: an icon as a container for the bitmap
  Returns: Nothing

  Fetches the index'th image into an icon.
 ------------------------------------------------------------------------------}
procedure TCustomImageList.GetIcon(Index: Integer; Image: TIcon);
begin
  if (Index < 0) or (Index >= FCount)
  then raise EInvalidOperation.Create(SInvalidIndex);

  //No Icon Support yet

end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.GetImages
  Params:  Index: the index of the requested image
           Image: a bitmap as a container for the bitmap
           Mask:  a bitmap as a container for the mask
  Returns: Nothing

  Fetches the index'th image and mask into a bitmap.
 ------------------------------------------------------------------------------}
procedure TCustomImageList.GetImages(Index: Integer; const Image, Mask: TBitmap);
begin
  with Image do FBitmap.Canvas.CopyRect(
    Rect(0, 0, Width, Height),
    Canvas,
    Rect(0, Index * FHeight, FWidth, (Index + 1) * FHeight)
  );
  with Mask do FMaskBitmap.Canvas.CopyRect(
    Rect(0, 0, Width, Height),
    Canvas,
    Rect(0, Index * FHeight, FWidth, (Index + 1) * FHeight)
  );
end;

{------------------------------------------------------------------------------
  Function: TCustomImageList.HandleAllocated
  Params:   None
  Returns:  True if a handle is allocated

  This function checks if the internal image is allocated
 ------------------------------------------------------------------------------}
function TCustomImageList.HandleAllocated: Boolean;
begin
  Result := FBitmap <> nil;
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.Initialize
  Params:  None
  Returns: Nothing

  Initializes the internal bitmap structures and the changelink list. 
  It is used by the Create and CreateSize constructors
 ------------------------------------------------------------------------------}
procedure TCustomImageList.Initialize;
begin
  FChangeLinkList := TList.Create;
  FAllocBy := 4;
  FAllocCount := 0;
  FBlendColor := clNone;
  FBkColor := clNone;

  if (Height < 1) or (Height > 32768) or (Width < 1) 
  then raise EInvalidOperation.Create(SInvalidImageSize);

  FBitmap := TBitmap.Create;
  FBitmap.Height := Height;
  FBitmap.Width := Width;
  FMaskBitmap := TBitmap.Create;
  with FMaskBitmap do
  begin
    Height := Height;
    Width := Width;
    Canvas.Brush.Color := clWhite;
    Monochrome := True;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.Insert
  Params:  Index: the index of the inserted image
           Image: a bitmap image
           Mask: a bitmap which defines the transparent parts of Image
  Returns: Nothing

  Inserts one or more (bitmap width / imagelist width) bitmaps into the list 
  at the index'th position.  If Mask is nil, the image has no transparent parts.
 ------------------------------------------------------------------------------}
procedure TCustomImageList.Insert(Index: Integer; Image, Mask: TBitmap);
//var
//  n, nCount: Integer;
//  I, M: TBitmap;
//  DR, SR: TRect;
begin
  if (Index > Count)
  then raise EInvalidOperation.Create(SInvalidIndex);

  if (Index < 0) then Index := 0;

  if (Image <> nil)
  then begin
    FImageList.Insert(Index,Image);
    Change;
 {   nCount := Image.Width div FWidth;
    if nCount > 0
    then begin
      AllocBitmap(nCount);

      if Index <> FCount
      then begin
        ShiftImages(FBitmap.Canvas, Index, nCount);
        ShiftImages(FMaskBitmap.Canvas, Index, nCount);
      end;
      Inc(FCount, nCount);

      I := TBitmap.Create;
      try
        with I do
        begin
          Height := FHeight;
          Width := FWidth;
        end;
        M := TBitmap.Create;
        try
          with M do
          begin
            Height := FHeight;
            Width := FWidth;
          end;

          // insert the new images one by one
          DR := Rect(0, 0, FWidth, FHeight);
          SR := DR;
          for n := Index to Index + nCount - 1 do
          begin
            CopyImage(I.Canvas, Image.Canvas, DR, SR);
            if Mask = nil
            then begin
              Replace(n, I, nil);
            end
            else begin
              CopyImage(M.Canvas, Mask.Canvas, DR, SR);
              Replace(n, I, M);
            end;
            Inc(SR.Left, FWidth);
            Inc(SR.Right, FWidth);
          end;

        finally
          M.Free;
        end;
      finally
        I.Free;
      end;
    end;
}

  end;
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.InsertIcon
  Params:  Index: the index of the inserted image
           Image: the Icon to be inserted
  Returns: Nothing

  Inserts an icon into the list at the index'th position.
 ------------------------------------------------------------------------------}
procedure TCustomImageList.InsertIcon(Index: Integer; Image: TIcon);
begin
  if (Index > FCount)
  then raise EInvalidOperation.Create(SInvalidIndex);

  if (Index < 0) then Index := 0;
  
  //No Icon Support yet

end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.InsertMasked
  Params:  Index: the index of the inserted image
           Image: A bitmap to be inserted
           MaskColor: The color acting as transparant color
  Returns: Nothing

  Adds one or more (bitmap width / imagelist width) bitmaps to the list. 
  Every occurance of MaskColor will be converted to transparent.
 ------------------------------------------------------------------------------}
procedure TCustomImageList.InsertMasked(Index: Integer; Image: TBitmap; MaskColor: TColor);
var
  Mask: TBitmap;
begin
  Mask := TBitmap.Create;
  try
    with Mask do
    begin
      Height := Image.Height;
      Width := Image.Width;
      Assign(Image);
      Mask(MaskColor);
    end;
    Insert(Index, Image, Mask);
    Change;
  finally
    Mask.Free;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.Move
  Params:  CurIndex: the index of the image to be moved
           NewIndex: the new index of the image
  Returns: Nothing

  Moves an image from the CurIndex'th location to NewIndex'th location
 ------------------------------------------------------------------------------}
procedure TCustomImageList.Move(CurIndex, NewIndex: Integer);
var
  Image, Mask: TBitmap;
begin
  if CurIndex <> NewIndex then
  begin
    Image := TBitmap.Create;
    try
      with Image do
      begin
        Height := FHeight;
        Width := FWidth;
      end;
      Mask := TBitmap.Create;
      try
        with Mask do
        begin
          Height := FHeight;
          Width := FWidth;
        end;
        GetImages(CurIndex, Image, Mask);
        Delete(CurIndex);
        Insert(NewIndex, Image, Mask);
        Change;
      finally
        Mask.Free;
      end;
    finally
      Image.Free;
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.NotifyChangeLink
  Params:  None
  Returns: Nothing

  Internal function to notify the subscribed objects of a change 
  of the imagelist.
 ------------------------------------------------------------------------------}
procedure TCustomImageList.NotifyChangeLink;
var
  nIndex: Integer;
begin
  if FChangeLinkList <> nil then
    with FChangeLinkList do
      for nIndex := 0 to Count - 1 do TChangeLink(Items[nIndex]).Change
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.RegisterChanges
  Params:  Value: a reference to changelink object
  Returns: Nothing

  Registers an object to get notified of a change of the imagelist.
 ------------------------------------------------------------------------------}
procedure TCustomImageList.RegisterChanges(Value: TChangeLink);
begin
  if (Value <> nil) and (FChangeLinkList.IndexOf(Value) = -1)
  then begin
    Value.Sender := Self;
    FChangeLinkList.Add(Value);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.Replace
  Params:  Index: the index of the replaceded image
           Image: a bitmap image
           Mask: a bitmap which defines the transparent parts of Image
  Returns: Nothing.

  Replaces the index'th image with the image given. If Mask is nil, 
  the image has no transparent parts.
 ------------------------------------------------------------------------------}
procedure TCustomImageList.Replace(Index: Integer; Image, Mask: TBitmap);
var
  DestinationRect, SourceRect: TRect;
begin
  if (Index >= FCount)
  then raise EInvalidOperation.Create(SInvalidIndex);

  if (Index < 0) then Index := 0;

  DestinationRect := Rect(0, Index * FHeight, FWidth, (Index + 1) * FHeight);
  SourceRect := Rect(0, 0, FWidth, FHeight);

  CopyImage(FBitmap.Canvas, Image.Canvas, DestinationRect, SourceRect);
  if Mask <> nil
  then CopyImage(FMaskBitmap.Canvas, Mask.Canvas, DestinationRect, SourceRect)
  else FMaskBitmap.Canvas.FillRect(DestinationRect);

  Change;
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.Replace
  Params:  Index: the index of the replaceded image
           Image: an icon image
  Returns: Nothing.

  Replaces the index'th image with the image given.
 ------------------------------------------------------------------------------}
procedure TCustomImageList.ReplaceIcon(Index: Integer; Image: TIcon);
begin
  if (Index > FCount)
  then raise EInvalidOperation.Create(SInvalidIndex);

  if (Index < 0) then Index := 0;

  // No Icon suppport yet
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.ReplaceMasked
  Params:  Index: the index of the replaceded image
           Image: A bitmap image
           MaskColor: The color acting as transparant color
  Returns: Nothing

  Replaces the index'th image with the image given.
  Every occurance of MaskColor will be converted to transparent.
 ------------------------------------------------------------------------------}
procedure TCustomImageList.ReplaceMasked(Index: Integer; NewImage: TBitmap; MaskColor: TColor);
begin
  if (Index >= FCount)
  then raise EInvalidOperation.Create(SInvalidIndex);

  if (Index < 0) then Index := 0;

end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.SetBkColor
  Params:  Value: The background color
  Returns: Nothing

  Sets the backgroundcolor for the transparen parts.
 ------------------------------------------------------------------------------}
procedure TCustomImageList.SetBkColor(const Value: TColor);
begin
  if FBkColor <> Value
  then begin
    FBkColor := Value;
    Change;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.SetHeight
  Params:  Value: the height of an image
  Returns: Nothing

  Sets the height of an image. If the height differs from the original height,
  the list contents wil be deleted.
 ------------------------------------------------------------------------------}
procedure TCustomImageList.SetHeight(const Value: Integer);
begin
  if FHeight <> Value
  then begin
    FHeight := Value;
    FBitMap.Height := 0;
    AllocBitmap(0);
    Clear;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.SetWidth
  Params:  Value: the width of an image
  Returns: Nothing

  Sets the width of an image. If the width differs from the original width,
  the list contents wil be deleted.
 ------------------------------------------------------------------------------}
procedure TCustomImageList.SetWidth(const Value: Integer);
begin
  if FWidth <> Value
  then begin
    FWidth := Value;
    FBitmap.Width := FWidth;
    FMaskBitmap.Width := FWidth;
    Clear;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.ShiftImages
  Params:  Source: source canvas on which the images are moved
           Start: start of the image to shift 
           Shift: number of images to shift
  Returns: Nothing

  Internal routine to move images on the internal image list.
 ------------------------------------------------------------------------------}
procedure TCustomImageList.ShiftImages(const Source: TCanvas;
  Start, Shift: Integer);
var
  FMoveBitmap: TBitmap;
begin
  try
    FMoveBitmap := TBitmap.Create;
    with FMoveBitmap do
    begin
      Width := FWidth;
      Height := (1 + FCount - Start) * FHeight;
    end;
    FMoveBitmap.Canvas.CopyRect(
      Rect(0, 0, FWidth, FMoveBitmap.Height),
      Source,
      Rect(0, Start * FHeight, FWidth, (FCount + 1) * FHeight)
    );
    Source.CopyRect(
      Rect(0, (Start + Shift) * FHeight, FWidth, (FCount + Shift + 1) * FHeight),
      FMoveBitmap.Canvas,
      Rect(0, 0, FWidth, FMoveBitmap.Height)
    );
  finally
    FMoveBitmap.Free;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCustomImageList.UnRegisterChanges
  Params:  Value: a reference to changelink object
  Returns: Nothing

  Unregisters an object for notifications.
 ------------------------------------------------------------------------------}
procedure TCustomImageList.UnRegisterChanges(Value: TChangeLink);
begin
  FChangeLinkList.Remove(Value);
end;

{******************************************************************************
                                  TChangeLink
 ******************************************************************************}

{------------------------------------------------------------------------------
  Method:  TChangeLink.Change
  Params:  None
  Returns: Nothing

  Fires the OnChange event.
 ------------------------------------------------------------------------------}
procedure TChangeLink.Change;
begin
  if Assigned(FOnChange) then FOnChange(Sender)
end;

{------------------------------------------------------------------------------
  Method: TChangeLink.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TChangeLink.Destroy;
begin
  if Sender <> nil 
  then Sender.UnRegisterChanges(Self);
  inherited Destroy;
end;


{

  $Log$
  Revision 1.12  2002/08/22 13:45:58  lazarus
  MG: fixed non AutoCheck menuitems and editor bookmark popupmenu

  Revision 1.11  2002/06/08 17:16:02  lazarus
  MG: added close buttons and images to TNoteBook and close buttons to source editor

  Revision 1.10  2002/05/10 06:05:52  lazarus
  MG: changed license to LGPL

  Revision 1.9  2002/04/04 12:25:01  lazarus
  MG: changed except statements to more verbosity

  Revision 1.8  2002/01/07 13:58:15  lazarus
  Changes to TListView to display images and fixes to TCustomImageList to notify it's owners of changes.

  Shane

  Revision 1.7  2002/01/03 15:07:08  lazarus
  MG: fixed TCustomImageList.CreateSize

  Revision 1.6  2001/06/14 14:57:58  lazarus
  MG: small bugfixes and less notes

  Revision 1.5  2001/03/19 14:40:49  lazarus
  MG: fixed many unreleased DC and GDIObj bugs

  Revision 1.3  2001/02/06 13:55:23  lazarus
  Changed the files from mode delphi to mode objfpc
  Shane

  Revision 1.2  2001/01/11 20:16:47  lazarus
  Added some TImageList code.
  Added a bookmark resource with 10 resource images.
  Removed some of the IFDEF's in mwCustomEdit around the inherited code.
  Shane

  Revision 1.1  2000/07/13 10:28:26  michael
  + Initial import

  Revision 1.1  2000/04/02 20:49:56  lazarus
  MWE:
    Moved lazarus/lcl/*.inc files to lazarus/lcl/include

  Revision 1.5  2000/03/21 23:47:33  lazarus
  MWE:
    + Added TBitmap.MaskHandle & TGraphic.Draw & TBitmap.Draw

  Revision 1.4  1999/09/26 17:06:44  lazarus
  MWE: Exept for resource loading, streaming and icons, finished
       implementation of TCustomImageList.

  Revision 1.3  1999/09/26 15:37:20  lazarus
  MWE: implemented some more methods and documented most

  Revision 1.2  1999/08/20 15:44:39  lazarus
  TImageList changes added from Marc Weustink

  Revision 1.1  1999/08/12 16:22:19  lazarus
  Templates initially created      CAW

}

