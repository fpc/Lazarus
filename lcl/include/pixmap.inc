(******************************************************************************
                                     TPixmap
 ******************************************************************************)

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
  P: PChar;
  S: TStringList;
  n, BufIndex: Integer;
begin
  FreeContext;

  // Convert a XPM filedata format to a XPM memory format
  // by filling an array of PChar whith the contents between 
  // the ""'s in the file 

  S := TStringList.Create;
  try
    S.LoadFromStream(Stream);
    Buf := GetMem(S.Count * SizeOf(PCharArray));
    try
      BufIndex := 0;
      for n := 0 to S.Count - 1 do
      if S.Strings[n][1] = '"'
      then begin
        Buf^[BufIndex] := @S.Strings[n][2];
        P := StrScan(Buf^[BufIndex], '"');
        if p <>  nil then p^ := #0;
        Inc(BufIndex);
      end;
      if FTransparentColor = clNone 
      then Handle := CreatePixmapIndirect(Buf, -1)
      else Handle := CreatePixmapIndirect(Buf, ColorToRGB(FTransparentColor));
    finally
    { Assert(False, 'Trace:*****************************************');
     Assert(False, 'Trace:the buf is -');
     for n := 0 to BufIndex-1 do
         Assert(False, 'Trace:' + strpas(Buf^[n])); }
      FreeMem(Buf);
    end;
  finally
    S.Free;
  end;
end;

{ =============================================================================

  $Log$
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
