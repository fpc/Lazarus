{%MainUnit ../graphics.pp}
{******************************************************************************
                                     TPixmap
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

function TestStreamIsXPM(const AStream: TStream): boolean;
type
  TXPMRange = (xrCode,xrStaticKeyWord, xrCharKeyWord);
  
  function Check(const s: string): boolean;
  var
    Buf: string;
  begin
    Result:=false;
    SetLength(Buf,length(s));
    if AStream.Read(Buf[1],length(Buf))<>length(Buf) then exit;
    if Buf<>s then exit;
    Result:=true;
  end;
  
var
  OldPosition: TStreamSeekType;
  c: char;
  Range: TXPMRange;
  LastChar: Char;
begin
  Result:=false;
  OldPosition:=AStream.Position;
  // skip comments and search 'static char*'
  Range:=xrCode;
  try
    repeat
      if AStream.Read(c,1)<>1 then exit;
      case c of
      ' ',#9,#10,#13: ;
      '/':
        begin
          // read comment
          if AStream.Read(c,1)<>1 then exit;
          if c<>'*' then exit;
          repeat
            LastChar:=c;
            if AStream.Read(c,1)<>1 then exit;
            if c in [#0..#8,#11,#12,#14..#31] then exit;
          until (c='/') and (LastChar='*');
        end;
      's':
        begin
          if Range<>xrCode then exit;
          // read 'tatic', the rest of 'static'
          if not Check('tatic') then exit;
          Range:=xrStaticKeyWord;
          // read space
          if (AStream.Read(c,1)<>1) or (not (c in [' ',#9,#10,#13])) then exit;
        end;
      'c':
        begin
          if Range<>xrStaticKeyWord then exit;
          // read 'har', the rest of 'char'
          if not Check('har') then exit;
          Range:=xrCharKeyWord;
        end;
      '*':
        begin
          if Range<>xrCharKeyWord then exit;
          Result:=true;
          exit;
        end;
      else
        exit;
      end;
    until false;

  finally
    AStream.Position:=OldPosition;
  end;
end;

{-------------------------------------------------------------------------------
  function XPMToPPChar
  Params: const XPM: string
  Result: PPChar
  
  Converts the source of an XPM image file into an array of PChar.
  The memory is allocated as one block, so freeing can simply be done via
  FreeMem(TheResult). The data is just behind the array.
  
  An XPM file can be included in a C source file, because it has a C syntax.
  In the compiled program it is simply an array of PChar.
  This function converts an XPM source file to the same array, by removing the
  comments and the string constant symbols.
-------------------------------------------------------------------------------}
function XPMToPPChar(const XPM: string): PPChar;
var
  TrimmedXPM: string;
  SrcLen, SrcPos, DestPos, LineCount, CurLine, LineLen: integer;
  LineStart: PChar;
begin
  Result := nil;
  // trim all comments and symbols and extract only the xpm data between ""
  SrcLen:=length(XPM);
  SetLength(TrimmedXPM,SrcLen+1); // +1 because TrimmedXPM should be at least
                                  // one byte
  SrcPos:=1;
  DestPos:=1;
  LineCount:=1;
  while (SrcPos<=SrcLen) do begin
    case XPM[SrcPos] of

    '/':
      begin
        if (SrcPos<SrcLen) and (XPM[SrcPos+1]='*') then begin
          // this is a C comment
          // -> skip comment
          inc(SrcPos,2);
          while (SrcPos<SrcLen) do begin
            if (XPM[SrcPos]='*') and (XPM[SRcPos+1]='/') then begin
              // comment end found
              inc(SrcPos,2);
              break;
            end;
            inc(SrcPos);
          end;
        end;
      end;

    '"':
      begin
        // start of a string constant
        inc(SrcPos);
        while (SrcPos<SrcLen) do begin
          if XPM[SrcPos]='\' then
            inc(SrcPos);
          if (XPM[SrcPos]='"') and (XPM[SrcPos-1]<>'\') then begin
            // string end found
            inc(SrcPos);
            break;
          end;
          // copy char
          TrimmedXPM[DestPos]:=XPM[SrcPos];
          inc(DestPos);
          inc(SrcPos);
        end;
      end;

    ',':
      begin
        // new string constant
        // -> add #0 char
        inc(SrcPos);
        TrimmedXPM[DestPos]:=#0;
        inc(DestPos);
        inc(LineCount);
      end;

    else
      inc(SrcPos);
    end;
  end;
  TrimmedXPM[DestPos]:=#0; // DestPos now contains the length of trimmed data

  // create array of PChar + image data
  GetMem(Result,SizeOf(PChar)*(LineCount+1)+DestPos);

  // copy data
  Move(TrimmedXPM[1],Result[LineCount+1],DestPos);

  // calculate the array of PChar
  LineStart:=@Result[LineCount+1];
  for CurLine:=0 to LineCount-1 do begin
    LineLen:=StrLen(LineStart)+1;
    Result[CurLine]:=LineStart;
    inc(LineStart,LineLen);
  end;
  Result[LineCount]:=nil; // mark the end of the array with a nil
end;

{-------------------------------------------------------------------------------
  function LazResourceXPMToPPChar
  Params: const ResourceName: string
  Result: PPChar

  Loads the source of an XPM image file from the LazarusResources into an array
  of PChar.
  See XPMToPPChar for more info.
-------------------------------------------------------------------------------}
function LazResourceXPMToPPChar(const ResourceName: string): PPChar;
var XPMSource: TLResource;
begin
  XPMSource:=LazarusResources.Find(ResourceName);
  if XPMSource.ValueType<>'XPM' then
    raise Exception.Create('LazResourceXPMToPPChar: The resource "'
                           +ResourceName+'" is not of type XPM');
  Result:=XPMToPPChar(XPMSource.Value);
end;

{-------------------------------------------------------------------------------
  function ReadXPMFromStream(Stream: TStream; Size: integer): PPChar;

  Converts the source of an XPM image file into an array of PChar.
  See XPMToPPChar for more info.
-------------------------------------------------------------------------------}
function ReadXPMFromStream(Stream: TStream; Size: integer): PPChar;
var
  s: string;
begin
  Result:=nil;
  if Size<=0 then exit;
  SetLength(s,Size);
  Stream.Read(s[1],Size);
  Result:=XPMToPPChar(s);
end;

{-------------------------------------------------------------------------------
  function ReadXPMSize(XPM: PPChar; var Width, Height, ColorCount: integer
    ): boolean;
    
  Reads the first line of an XPM PChar array, whcih contains the width, height
  and number of colors of the XPM.
-------------------------------------------------------------------------------}
function ReadXPMSize(XPM: PPChar; var Width, Height, ColorCount: integer
  ): boolean;
var
  LinePos: PChar;
  
  function ReadNumber(var i: integer): boolean;
  begin
    Result:=false;
    // skip space
    while (LinePos^ in [' ',#9]) do inc(LinePos);
    // read number
    i:=0;
    while (LinePos^ in ['0'..'9']) do begin
      i:=i*10+ord(LinePos^)-ord('0');
      inc(LinePos);
      Result:=true;
    end;
  end;
  
begin
  Result:=false;
  if (XPM=nil) or (XPM[0]=nil) then exit;
  LinePos:=XPM[0];
  if not ReadNumber(Width) then exit;
  if not ReadNumber(Height) then exit;
  if not ReadNumber(ColorCount) then exit;
  Result:=true;
end;

{ TPixmap }

function TPixmap.LazarusResourceTypeValid(const ResourceType: string): boolean;
begin
  Result:=(ResourceType='XPM');
end;

procedure TPixmap.WriteStream(Stream: TStream; WriteSize: Boolean);
begin
  WriteStreamWithFPImage(Stream,WriteSize,TLazWriterXPM);
end;

function TPixmap.GetDefaultFPReader: TFPCustomImageReaderClass;
begin
  Result:=TLazReaderXPM;
end;

function TPixmap.GetDefaultFPWriter: TFPCustomImageWriterClass;
begin
  Result:=TLazWriterXPM;
end;


// included by graphics.pp

{
  $Log$
  Revision 1.32  2005/06/25 15:34:03  mattias
  fixed a few fpc over warnings  from Andrew Haines

  Revision 1.31  2005/06/22 09:45:59  mattias
  implemented saving alpha bmp and using transparency for IDE glyph editor

  Revision 1.30  2004/09/14 10:23:44  mattias
  implemented finding DefineProperties in registered TPersistent, implemented auto commenting of missing units for Delphi unit conversion

  Revision 1.29  2004/04/10 17:58:57  mattias
  implemented mainunit hints for include files

  Revision 1.28  2004/04/03 16:47:46  mattias
  implemented converting gdkbitmap to RawImage mask

  Revision 1.27  2004/02/29 22:51:54  mattias
  added jpeg example

  Revision 1.26  2004/02/27 00:42:41  marc
  * Interface CreateComponent splitup
  * Implemented CreateButtonHandle on GTK interface
    on win32 interface it still needs to be done
  * Changed ApiWizz to support multilines and more interfaces

  Revision 1.25  2004/02/24 19:40:17  mattias
  TBitmap can now read form streams without knowing the size

  Revision 1.24  2004/02/02 22:01:51  mattias
  fpImage is now used as default, deactivate it with -dDisableFPImage

  Revision 1.23  2003/12/25 14:17:07  mattias
  fixed many range check warnings

  Revision 1.22  2003/09/08 13:07:17  mattias
  TBitmap now uses fpImage for writing bitmaps

  Revision 1.21  2003/09/02 21:32:56  mattias
  implemented TOpenPictureDialog

  Revision 1.20  2003/08/20 17:03:48  mattias
  implemented TPixmap and TPortableNetworkGraphic with fpImage

  Revision 1.19  2003/06/25 10:38:28  mattias
  implemented saving original stream of TBitmap

  Revision 1.18  2003/04/03 17:42:13  mattias
  added exception handling for createpixmapindirect

  Revision 1.17  2002/12/18 17:59:12  mattias
  minor cleanup

  Revision 1.16  2002/12/18 17:52:18  mattias
  fixed lazarus xml files for fpc 1.1

  Revision 1.15  2002/09/16 16:18:50  lazarus
  MG: fixed mem leak in TPixmap

  Revision 1.14  2002/09/02 08:13:17  lazarus
  MG: fixed GraphicClass.Create

  Revision 1.13  2002/06/08 17:16:02  lazarus
  MG: added close buttons and images to TNoteBook and close buttons to source editor

  Revision 1.12  2002/05/10 06:05:55  lazarus
  MG: changed license to LGPL

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
