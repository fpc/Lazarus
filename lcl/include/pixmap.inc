(******************************************************************************
                                     TPixmap
 ******************************************************************************)

procedure TPixmap.LoadFromLazarusResource(const ResName: String);
var
  ms:TMemoryStream;
  res:TLResource;
begin
  res:=LazarusResources.Find(ResName);
  if (res<>nil) and (res.Value<>'') and (res.ValueType='XPM') then begin
    ms:=TMemoryStream.Create;
    try
      ms.Write(res.Value[1],length(res.Value));
      ms.Position:=0;
      LoadFromStream(ms);
    finally
      ms.Free;
    end;
  end;
end;

procedure TPixmap.LoadFromResourceName(Instance: THandle; const ResName: String);
begin
end;

procedure TPixmap.LoadFromResourceID(Instance: THandle; ResID: Integer);
begin
end;

procedure TPixmap.ReadStream(Stream: TStream; Size: Longint);
type
  TCharArray = array[0..0] of PChar;
  PCharArray = ^TCharArray;
var
  Buf: PCharArray;
  BufPtr: ^PChar;
  i, j, LineCount, PixLen, Line3Start, LineStart, LineEnd: Integer;
  s : String;
  
  procedure ParseDataLine;
  var a: integer;
  begin
    if s[LineStart]<>'"' then exit;
    a:=LineStart+1;
    while (a<LineEnd) and (s[a]<>'"') do inc(a);
    if a>=LineEnd then exit;
    s[LineEnd]:=#0;
    BufPtr^ := @s[LineStart+1];
    Inc(BufPtr);
  end;
  
begin
  FreeContext;

  // Convert a XPM filedata format to a XPM memory format
  // by filling an array of PChar with the contents between 
  // the ""'s in the file

  // read stream into string
  PixLen:=Stream.Size;
  if PixLen=0 then exit;
  SetLength(s,PixLen);
  Stream.Read(s[1],PixLen);
  // count line ends
  Line3Start:=-1;
  LineCount:=1;
  i:=1;
  while (i<=PixLen) do begin
    if not (s[i] in [#10,#13]) then
      inc(i)
    else begin
      inc(LineCount);
      inc(i);
      if (i<=PixLen) and (s[i] in [#10,#13]) and (s[i]<>s[i-1]) then
        inc(i);
      if (LineCount=3) and (i<PixLen) then Line3Start:=i;
    end;
  end;
  if (s[PixLen] in [#10,#13]) then dec(LineCount);
  // build PChar Array of Data-Line Starts ( " characters)
  if (LineCount>2) and (Line3Start>=1) and (s[Line3Start]='"') then begin
    Buf := GetMem((LineCount+1) * SizeOf(PCharArray));
    try
      BufPtr := Pointer(Buf);
      LineStart:=1;
      i:=1;
      while (i<=PixLen) do begin
        if not (s[i] in [#10,#13]) then
          inc(i)
        else begin
          // found line end
          LineEnd:=i;
          ParseDataLine;
          inc(i);
          if (i<=PixLen) and (s[i] in [#10,#13]) and (s[i]<>s[i-1]) then
            inc(i);
          LineStart:=i;
        end;
      end;
      if not (s[PixLen] in [#10,#13]) and (LineStart<=PixLen) then begin
        LineEnd:=PixLen+1;
        ParseDataLine;
      end;
      BufPtr^ := nil;
      // Create the pixmap
      if FTransparentColor = clNone then
        // create a transparent pixmap (with mask)
        Handle := CreatePixmapIndirect(Buf, -1)
      else
        // create an opaque pixmap.
        // Transparent pixels are filled with FTransparentColor
        Handle := CreatePixmapIndirect(Buf, ColorToRGB(FTransparentColor));

      // set width and height
      // the third line is of the form '"<width as decimal> <height as decimal> '
      LineEnd:=Line3Start;
      while (LineEnd<PixLen) and not (s[LineEnd] in [#13,#10]) do inc(LineEnd);
      i:=Line3Start+1; // ignore initial "
      j:=i;
      while (j<LineEnd) and (s[j]<>' ') do inc(j);
      FWidth := StrToIntDef(copy(s,i,j-i),1);
      inc(j);
      i:=j;      
      while (j<LineEnd) and (s[j]<>' ') do inc(j);
      FHeight := StrToIntDef(copy(s,i,j-i),1);
    finally
      FreeMem(Buf);
    end;
  end;
end;

{ =============================================================================

  $Log$
  Revision 1.11  2001/09/30 08:34:50  lazarus
  MG: fixed mem leaks and fixed range check errors

  Revision 1.10  2001/06/26 00:08:36  lazarus
  MG: added code for form icons from Rene E. Beszon

  Revision 1.9  2001/06/06 12:30:41  lazarus
  MG: bugfixes

  Revision 1.8  2001/03/19 14:40:49  lazarus
  MG: fixed many unreleased DC and GDIObj bugs

  Revision 1.6  2001/02/04 18:24:41  lazarus
  Code cleanup
  Shane

  Revision 1.5  2001/01/04 16:12:54  lazarus
  Removed some writelns and changed the property editor for TStrings a bit.
  Shane

  Revision 1.4  2001/01/03 18:44:54  lazarus
  The Speedbutton now has a numglyphs setting.
  I started the TStringPropertyEditor

  Revision 1.3  2000/12/29 20:32:33  lazarus
  Speedbuttons can now draw disabled images.
  Shane

  Revision 1.2  2000/07/30 21:48:32  lazarus
  MWE:
    = Moved ObjectToGTKObject to GTKProc unit
    * Fixed array checking in LoadPixmap
    = Moved LM_SETENABLED to API func EnableWindow and EnableMenuItem
    ~ Some cleanup

  Revision 1.1  2000/07/13 10:28:27  michael
  + Initial import

  Revision 1.2  2000/05/09 02:07:40  lazarus
  Replaced writelns with Asserts.                          CAW

  Revision 1.1  2000/04/02 20:49:56  lazarus
  MWE:
    Moved lazarus/lcl/*.inc files to lazarus/lcl/include

  Revision 1.2  2000/03/17 17:07:00  lazarus
  Added images to speedbuttons
  Shane

  Revision 1.1  2000/03/17 13:21:29  lazarus
  MWE:
    Forgot to add inc.


}
