(******************************************************************************
                                     TBitMap
 ******************************************************************************)

procedure TBitMap.Assign(Source: TPersistent);
begin
//TODO: Finish TBITMAP ASSIGN
end;

procedure TBitmap.Draw(ACanvas: TCanvas; const Rect: TRect);
begin
  Assert(False, 'Trace:TODO: [TBitmap.Draw]');
end;

constructor TBitmap.Create;
begin
  inherited Create;
  FPixelFormat := pfDevice;
  FCanvas := TBitmapCanvas.Create(Self);
  FImage := TBitmapImage.Create;
  FImage.Reference;
  FTransparentColor := clNone;
end;

destructor TBitMap.Destroy;
begin
  FImage.Release;
  FCanvas.Free;
  inherited Destroy;
end;

procedure TBitMap.FreeContext;
begin
  if (FCanvas <> nil) then TBitmapCanvas(FCanvas).FreeDC;
end;

procedure TBitMap.FreeImage;
begin
end;

procedure TBitMap.Mask(ATransparentColor: TColor);
begin
end;


function TBitmap.GetHandle: HBITMAP;
begin
  HandleNeeded;
  Result := FImage.FHandle;
end;

function TBitmap.GetMaskHandle: HBITMAP;
begin
  MaskHandleNeeded;
  Result := FImage.FMaskHandle;
end;

procedure TBitMap.HandleNeeded;
var n : integer;
begin
//  if FHandle = 0 then CNSendMessage(LM_CREATE, Self, nil);
  if FImage.FHandle = 0 then begin
    case PixelFormat of
      pfDevice : n:= ScreenInfo.ColorDepth;
      pf1bit : n:= 1;
      pf4bit : n:= 4;
      pf8bit : n:= 8;
      pf15bit : n:= 15;
      pf16bit : n:= 16;
      pf24bit : n:= 24;
      pf32bit : n:= 32;
      else raise EInvalidOperation.Create('Unsupported bitmap format.');
    end;
    FImage.FHandle:= CreateBitmap(Width, Height, 1, n, nil);
  end;
end;

procedure TBitMap.MaskHandleNeeded;
begin
  //TODO
end;

procedure TBitMap.LoadFromStream(Stream: TStream);
begin
  ReadStream(Stream, Stream.Size - Stream.Position);
end;

procedure TBitMap.LoadFromResourceName(Instance: THandle; const ResName: String);
begin
end;

procedure TBitMap.LoadFromResourceID(Instance: THandle; ResID: Integer);
begin
end;

Procedure TBitmap.LoadFromXPMFile(Filename : String);
var
  pstr : PChar;
Begin
  HandleNeeded;
  pStr := StrAlloc(length(Filename) + 1);
  StrPCopy(pStr, Filename);

  CNSendMessage(LM_LOADXPM,Self,pstr);
  StrDispose(pStr);
end;


Procedure TBitmap.NewImage(NHandle: HBITMAP; NPallette: HPALETTE;
   const NDIB : TDIBSection; OS2Format : Boolean);
Begin

end;

procedure TBitMap.PaletteNeeded;
begin
end;

procedure TBitmap.ReadStream(Stream: TStream; Size: Longint);
type
  BITMAP = record
          bmType : Longint;
          bmWidth : Longint;
          bmHeight : Longint;
          bmWidthBytes : Longint;
          bmPlanes : Word;
          bmBitsPixel : Word;
          bmBits : Pointer;
       end;
  PBITMAP = ^BITMAP;

  BITMAPINFOHEADER = record
          biSize : DWORD;
          biWidth : Longint;
          biHeight : Longint;
          biPlanes : WORD;
          biBitCount : WORD;
          biCompression : DWORD;
          biSizeImage : DWORD;
          biXPelsPerMeter : Longint;
          biYPelsPerMeter : Longint;
          biClrUsed : DWORD;
          biClrImportant : DWORD;
       end;

  RGBQUAD = record
          rgbBlue : BYTE;
          rgbGreen : BYTE;
          rgbRed : BYTE;
          rgbReserved : BYTE;
       end;

  BITMAPINFO = record
          bmiHeader : BITMAPINFOHEADER;
          bmiColors : array[0..0] of RGBQUAD;
       end;

  PBITMAPINFO = ^BITMAPINFO;

  TBitsObj = array[1..1] of byte;
  PBitsObj = ^TBitsObj;

const
  BI_RGB = 0;

var
  BmpHead: TBitmapFileHeader;
  DIB: TDIBSection;
  ReadSize: integer;
  BmpInfo:PBitmapInfo;
  ImgSize:longint;
  Bits:PBitsObj;
  InfoSize,BmpWidth,BmpHeight:integer;
  BitsPerPixel,ColorsUsed:integer;
begin
  FreeContext;
  if Size = 0 then
  begin
    FillChar(DIB, sizeof(DIB), 0);
    NewImage(0, 0, DIB, False);
  end
  else
  begin
    ReadSize:=Stream.Read(BmpHead, sizeof(BmpHead));
    if (BmpHead.bfType <> $4D42) or (BmpHead.bfOffBits<ReadSize) then begin
      raise EInOutError.Create('Invalid windows bitmap (header)');
    end;
    InfoSize:=BmpHead.bfOffBits-SizeOf(BmpHead);
    GetMem(BmpInfo,InfoSize);
    try
      ReadSize:=Stream.Read(BmpInfo^,InfoSize);
      if ReadSize<>InfoSize then
        raise EInOutError.Create('Invalid windows bitmap (info)');
      if BmpInfo^.bmiHeader.biSize<>sizeof(BitmapInfoHeader) then
        raise EInOutError.Create('OS2 bitmaps are not supported yet');
      if BmpInfo^.bmiHeader.biCompression<>bi_RGB then
        raise EInOutError.Create('RLE compression is not supported yet');

      // Let's now support only 24bit bmps! Then we can use the palette. Not yet.
      BitsPerPixel:=BmpInfo^.bmiHeader.biBitCount;
      if BitsPerPixel<>24 then begin
        ColorsUsed:=BmpInfo^.bmiHeader.biClrUsed;
        if ColorsUsed=0 then ColorsUsed:=1 shl ColorsUsed;
        // s:=SizeOf(TLogPalette)+(CoorsUsed-1)*SizeOf(TPaletteEntry);
      end;
      // Palette is fake now. Then it'll be better!
      // EInOutError.Create('Only truecolor is supported yet.');
      BmpHeight:=BmpInfo^.bmiHeader.biHeight;
      BmpWidth:=BmpInfo^.bmiHeader.biWidth;
      ImgSize:=BmpInfo^.bmiHeader.biSizeImage;
      GetMem(Bits,ImgSize);
      try
        ReadSize:=Stream.Read(Bits^,ImgSize);
        if ReadSize<>InfoSize then
          raise EInOutError.Create('Invalid windows bitmap (bits)');

        // ToDo: create a bitmap handle

      finally
        FreeMem(Bits);
      end;
    finally
      FreeMem(BmpInfo);
    end;
  end;
end;

procedure TBitmap.WriteStream(Stream: TStream; WriteSize: Boolean);
var Size: longint;
begin
  // ToDo
  // this is only a workaround till we can save bitmaps
  if WriteSize then begin
    Size:=0;
    Stream.Write(Size, SizeOf(Size));
  end;
end;

procedure TBitMap.SaveToStream(Stream: TStream);
begin
  WriteStream(Stream, False);
end;

procedure TBitmap.SetHandle(Value: HBITMAP);
begin
  // TODO: the properties from new bitmap
  FImage.FHandle := Value;
end;

procedure TBitmap.SetMaskHandle(Value: HBITMAP);
begin
  with FImage do
  begin
    if FMaskHandle <> Value then
    begin
      FMaskHandle := Value;
      //FMaskBitsValid := True;
      //FMaskValid := True;
    end;
  end;
end;

Function TBitmap.ReleaseHandle : HBITMAP;
Begin
  Result := GetHandle;
  FImage.FHandle := 0;
end;

function TBitmap.GetEmpty: boolean;
begin
  with FImage do
    Result := (FHandle = 0) and (FDIBHandle = 0);
end;

function TBitmap.GetHeight: Integer;
begin
  Result := FHeight;
end;

function TBitmap.GetWidth: Integer;
begin
  Result := FWidth;
end;

procedure TBitmap.ReadData(Stream: TStream);
var
  Size: Longint;
begin
  Stream.Read(Size, SizeOf(Size));
  ReadStream(Stream, Size);
end;

procedure TBitmap.WriteData(Stream: TStream);
begin
  WriteStream(Stream, True);
end;

procedure TBitmap.SetWidth(Value: Integer);
begin
  FWidth:=Value;
  // ToDo
end;

procedure TBitmap.SetHeight(Value: Integer);
begin
  FHeight:=Value;
  // ToDo
end;


{ =============================================================================

  $Log$
  Revision 1.3  2001/03/05 14:20:04  lazarus
  added streaming to tgraphic, added tpicture

  Revision 1.2  2000/12/29 20:32:33  lazarus
  Speedbuttons can now draw disabled images.
  Shane

  Revision 1.1  2000/07/13 10:28:24  michael
  + Initial import

  Revision 1.2  2000/05/09 00:46:41  lazarus
  Changed writelns to Asserts.                          CAW

  Revision 1.1  2000/04/02 20:49:55  lazarus
  MWE:
    Moved lazarus/lcl/*.inc files to lazarus/lcl/include

  Revision 1.18  2000/03/30 18:07:53  lazarus
  Added some drag and drop code
  Added code to change the unit name when it's saved as a different name.  Not perfect yet because if you are in a comment it fails.

  Shane

  Revision 1.17  2000/03/21 23:47:33  lazarus
  MWE:
    + Added TBitmap.MaskHandle & TGraphic.Draw & TBitmap.Draw

  Revision 1.16  2000/03/19 03:52:08  lazarus
  Added onclick events for the speedbuttons.
  Shane

  Revision 1.15  2000/03/16 23:58:46  lazarus
  MWE:
    Added TPixmap for XPM support

  Revision 1.14  2000/03/15 20:15:31  lazarus
  MOdified TBitmap but couldn't get it to work
  Shane

  Revision 1.13  2000/03/10 12:51:14  lazarus
  *** empty log message ***

  Revision 1.12  2000/03/07 19:00:15  lazarus
  Minor changes.  Added the skeleton for TSpeedbutton
  Shane

  Revision 1.11  2000/03/06 00:05:05  lazarus
  MWE: Added changes from Peter Dyson <peter@skel.demon.co.uk> for a new
    release of mwEdit (0.92)

  Revision 1.10  2000/01/18 22:18:34  lazarus

  Moved bitmap creation into appropriate place. Cleaned up a bit.
  Finished DeleteObject procedure.

  Revision 1.9  1999/12/31 14:58:00  lazarus
  MWE:
    Set unkown VK_ codesto 0
    Added pfDevice support for bitmaps

  Revision 1.8  1999/12/18 18:27:31  lazarus
  MWE:
    Rearranged some events to get a LM_SIZE, LM_MOVE and LM_WINDOWPOSCHANGED
    Initialized the TextMetricstruct to zeros to clear unset values
    Get mwEdit to show more than one line
    Fixed some errors in earlier commits

  Revision 1.7  1999/12/14 22:05:37  lazarus
  More changes for TToolbar
  Shane

  Revision 1.6  1999/11/25 23:45:08  lazarus
  MWE:
    Added font as GDIobject
    Added some API testcode to testform
    Commented out some more IFDEFs in mwCustomEdit

  Revision 1.5  1999/11/17 01:16:39  lazarus
  MWE:
    Added some more API stuff
    Added an initial TBitmapCanvas
    Added some DC stuff
    Changed and commented out, original gtk linedraw/rectangle code. This
      is now called through the winapi wrapper.

}
