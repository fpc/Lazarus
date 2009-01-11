unit JcfUnicodeFiles;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is JcfUnicodeFiles, released March 2007.
The Initial Developer of the Original Code is Anthony Steele. 
Portions created by Anthony Steele are Copyright (C) 2007 Anthony Steele.
All Rights Reserved.
Contributor(s): Anthony Steele.

The contents of this file are subject to the Mozilla Public License Version 1.1
(the "License"). you may not use this file except in compliance with the License.
You may obtain a copy of the License at http://www.mozilla.org/NPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied.
See the License for the specific language governing rights and limitations
under the License.

Alternatively, the contents of this file may be used under the terms of
the GNU General Public License Version 2 or later (the "GPL") 
See http://www.gnu.org/licenses/gpl.html
------------------------------------------------------------------------------*)
{*)}

{$I JcfGlobal.inc}

interface

type
  TFileContentType = (eUnknown, e8Bit, eUtf8,
    eUtf16LittleEndian, eUtf16BigEndian,
    eUtf32LittleEndian, eUtf32BigEndian);

function TypeOfTextFile(const psFileName: string): TFileContentType;

procedure ReadTextFile(const psFileName: string; out psContents: WideString;
  out peContentType: TFileContentType);

procedure WriteTextFile(const psFileName: string; const psContents: WideString;
  const peContentType: TFileContentType);


implementation

uses
  { delphi }
  Classes, SysUtils, Dialogs;

const
  // byte order markers (BOM)
  // these are found at the start of the file

  /// 3 bytes for UTF-8
  Utf8Marker12 = $BBEF;
  Utf8Marker1 = $EF;
  Utf8Marker2 = $BB;
  Utf8Marker3  = $BF;

  // 4 bytes for UTF-16. Big or little-endian
  Utf16LittleEndianMarker = $FEFF;
  Utf16BigEndianMarker = $FFFE;

  // 4 bytes for utf-32. Big or little-endian
  Utf32LittleEndianMarker1 = $FEFF;
  Utf32LittleEndianMarker2 = $0000;

  Utf32BigEndianMarker1 = $0000;
  Utf32BigEndianMarker2 = $FFFE;

  MaxAnsiChar = 127;

function ReadFileHeader(const pcFileStream: TFileStream): TFileContentType;
var
  word1: word;
  word2: word;
  byte3: byte;
begin
  // small files are ansi
  if pcFileStream.Size < 4 then
  begin
    Result := e8Bit;
    exit;
  end;

  // read the first 4 bytes
  pcFileStream.Seek(0, soFromBeginning);

  pcFileStream.Read(word1, SizeOf(word));
  pcFileStream.Read(word2, SizeOf(word));

  byte3 := byte(word2);

  if (word1 = Utf32LittleEndianMarker1) and (word2 = Utf32LittleEndianMarker2) then
  begin
    Result := eUtf32LittleEndian;
  end
  else if (word1 = Utf32BigEndianMarker1) and (word2 = Utf32BigEndianMarker2) then
  begin
    Result := eUtf32BigEndian;
  end
  else if (word1 = Utf8Marker12) and (byte3 = Utf8Marker3) then
  begin
    Result := eUtf8;
  end
  else if (word1 = Utf16LittleEndianMarker) then
  begin
    Result := eUtf16LittleEndian;
  end
  else if (word1 = Utf16BigEndianMarker) then
  begin
    Result := eUtf16BigEndian;
  end
  else
  begin
    Result := e8Bit;
  end;

end;

function TypeOfTextFile(const psFileName: string): TFileContentType;
var
  fs: TFileStream;
begin
  {open file}
  fs := TFileStream.Create(psFileName, fmOpenRead);
  try
    Result := ReadFileHeader(fs);
  finally
    // close the file
    fs.Free;
  end;
end;

procedure ReadPastFileHeader(const pcFileStream: TFileStream; const peContentType: TFileContentType);
var
  liOffsetBytes: integer;
begin
  case peContentType of
    e8Bit:
      liOffsetBytes := 0;
    eUtf8:
      liOffsetBytes := 3;
    eUtf16LittleEndian, eUtf16BigEndian:
      liOffsetBytes := 2;
    eUtf32LittleEndian, eUtf32BigEndian:
      liOffsetBytes := 4;
    else
      raise Exception.Create('Unknown file content type: ' + IntToStr(Ord(peContentType)));
  end;

  pcFileStream.Seek(liOffsetBytes, soFromBeginning);
end;

{ this is one of the few cases where 'AnsiString' must be used }
function Read8BitFile(const pcFileStream: TFileStream): WideString;
var
  liBytesRemaining: integer;
  lsContents8bit: AnsiString;
begin
  liBytesRemaining := pcFileStream.Size - pcFileStream.Position;
  // read the bytes into a string
  SetLength(lsContents8bit, liBytesRemaining);
  if pcFileStream.Size > 0 then
  begin
    pcFileStream.ReadBuffer(lsContents8bit[1], liBytesRemaining);
  end;

  // convert to wide string
  Result := WideString(lsContents8bit);
end;


function ReadUtf8File(const pcFileStream: TFileStream): WideString;
var
  liBytesRemaining: integer;
  lsContents: AnsiString;
begin
  liBytesRemaining := pcFileStream.Size - pcFileStream.Position;
  // read the bytes into a string
  SetLength(lsContents, liBytesRemaining);
  if pcFileStream.Size > 0 then
  begin
    pcFileStream.ReadBuffer(lsContents[1], liBytesRemaining);
  end;

  // convert to wide string
  {$IFDEF DELPHI12}
  Result := UTF8ToWideString(lsContents);
  {$ELSE}
  Result := UTF8Decode(lsContents);
  {$ENDIF}
end;


function Read16BitFile(const pcFileStream: TFileStream; const pbBigEndian: boolean): WideString;
var
  liBytesRemaining: integer;
  liLoop: integer;
  lsWideContents: WideString;
begin
  // read it
  liBytesRemaining := pcFileStream.Size - pcFileStream.Position;
  SetLength(lsWideContents, liBytesRemaining div 2);
  pcFileStream.Read(lsWideContents[1], liBytesRemaining);

  if pbBigEndian then
  begin
    // swap the bytes
    for liLoop := 1 to Length(lsWideContents) do
      lsWideContents[liLoop] := WideChar(Swap(word(lsWideContents[liLoop])));
  end;

  Result := lsWideContents;
end;

function SwapWords(const value: UCS4Char): UCS4Char;
var
  hi: word;
  lo: word;
begin
  // split into 16-bit words
  hi := value shr 16;
  lo := (value and $0000FFFF); // Prevent Range check error by converting to a word

  hi := Swap(hi);
  lo := Swap(lo);

  // recombine
  Result := (lo shl 16) + hi;
end;

function Read32BitFile(const pcFileStream: TFileStream; pbBigEndian: boolean): WideString;
var
  liBytesRemaining: integer;
  charsRemaining: integer;
  ucs4Chars: UCS4String;
  liLoop: integer;
begin
  liBytesRemaining := pcFileStream.Size - pcFileStream.Position;
  charsRemaining := liBytesRemaining div 4;

  SetLength(ucs4Chars, charsRemaining);
  pcFileStream.Read(ucs4Chars[0], liBytesRemaining);

  if pbBigEndian then
  begin
    // swap the bytes
    for liLoop := 0 to charsRemaining - 1 do
    begin
      ucs4Chars[liLoop] := SwapWords(ucs4Chars[liLoop]);
    end;
  end;

  Result := UCS4StringToWideString(ucs4Chars);
end;

{ read in a text file,
  the file can contain 8-bit or 16-bit chars
  code is much adapted from a sample by Mike Shkolnik
  in nntp://borland.public.delphi.rtl.general
  Re: Read UNICODE/ANSI/ASCII Text File to WideString
  at: Jan 23 2006, 12:17
  found at http://delphi.newswhat.com/geoxml/forumhistorythread?groupname=borland.public.delphi.rtl.general&messageid=43d485bf$1@newsgroups.borland.com
}
procedure ReadTextFile(const psFileName: string; out psContents: WideString;
  out peContentType: TFileContentType);
var
  fs: TFileStream;
begin
  psContents    := '';
  peContentType := eUnknown;

  {open file}
  fs := TFileStream.Create(psFileName, fmOpenRead);
  try
    peContentType := ReadFileHeader(fs);

    ReadPastFileHeader(fs, peContentType);

    case peContentType of
      e8Bit:
        psContents := Read8BitFile(fs);

      eUtf8:
        psContents := ReadUtf8File(fs);

      eUtf16LittleEndian, eUtf16BigEndian:
        psContents := Read16BitFile(fs, peContentType = eUtf16BigEndian);

      eUtf32LittleEndian, eUtf32BigEndian:
        psContents := Read32BitFile(fs, peContentType = eUtf32BigEndian);
      else
        raise Exception.Create('Unknown file content type: ' + IntToStr(Ord(peContentType)));

    end;
  finally
    // close the file
    fs.Free;
  end;
end;

{ this is one of the few cases when "AnsiString" must be used }
procedure Write8BitFile(const pcFileStream: TFileStream; const psContents: WideString);
var
  Len:    integer;
  lsContents: AnsiString;
begin
  lsContents := AnsiString(psContents);

  Len := Length(lsContents);

  if Len > 0 then
  begin
    pcFileStream.WriteBuffer(lsContents[1], Len);
  end;
end;

{ this is one of the few cases when "AnsiString" must be used }
procedure WriteUtf8File(const pcFileStream: TFileStream; const psContents: WideString);
var
  Len:    integer;
  lsContents: AnsiString;
  utf8Header: array [0..2] of byte;
begin
  lsContents := UTF8Encode(psContents);

  Len := Length(lsContents);

  // write the BOM
  utf8Header[0] := Utf8Marker1;
  utf8Header[1] := Utf8Marker2;
  utf8Header[2] := Utf8Marker3;
  pcFileStream.WriteBuffer(utf8Header[0], 3);

  if Len > 0 then
  begin
    pcFileStream.WriteBuffer(lsContents[1], Len);
  end;
end;

procedure Write16BitFile(const pcFileStream: TFileStream;
  const psContents: WideString; const pbBigEndian: boolean);
var
  Len:    integer;
  liLoop: integer;
  wChar:  word;
begin
  Len := Length(psContents);

  if Len > 0 then
  begin
    if pbBigEndian then
    begin
      // write the BOM
      wChar := Utf16BigEndianMarker;
      pcFileStream.WriteBuffer(wChar, 2);

      for liLoop := 1 to Len do
      begin
        wChar := Swap(word(psContents[liLoop]));
        pcFileStream.WriteBuffer(wChar, 2);
      end;
    end
    else
    begin
      // write the BOM
      wChar := Utf16LittleEndianMarker;
      pcFileStream.WriteBuffer(wChar, 2);

      pcFileStream.WriteBuffer(psContents[1], Len * 2);
    end;
  end;
end;

procedure Write32BitFile(const pcFileStream: TFileStream;
  const psContents: WideString; const pbBigEndian: boolean);
var
  Len:    integer;
  liLoop: integer;
  lsUcs4String: UCS4String;
  lcUcs4Char: UCS4Char;
  wChar: word;
begin
  Len := Length(psContents);

  if Len > 0 then
  begin
    lsUcs4String := WideStringToUCS4String(psContents);

    if pbBigEndian then
    begin
      // write the BOM
      wChar := Utf32BigEndianMarker1;
      pcFileStream.WriteBuffer(wChar, 2);
      wChar := Utf32BigEndianMarker2;
      pcFileStream.WriteBuffer(wChar, 2);

      for liLoop := 0 to Len do
      begin
        lcUcs4Char := SwapWords(lsUcs4String[liLoop]);
        pcFileStream.WriteBuffer(lcUcs4Char, 4);
      end;
    end
    else
    begin
      // write the BOM
      wChar := Utf32LittleEndianMarker1;
      pcFileStream.WriteBuffer(wChar, 2);
      wChar := Utf32LittleEndianMarker2;
      pcFileStream.WriteBuffer(wChar, 2);

      // an array not a real string, indexed from zero
      pcFileStream.WriteBuffer(lsUcs4String[0], (Len + 1) * 4);
    end;
  end;
end;

procedure WriteTextFile(const psFileName: string; const psContents: WideString;
  const peContentType: TFileContentType);
var
  fs:     TFileStream;
 begin
  fs := TFileStream.Create(psFileName, fmCreate);
  try

   case peContentType of
     e8Bit:
     begin
       Write8BitFile(fs, psContents);
     end;

     eUtf8:
     begin
       WriteUtf8File(fs, psContents);
     end;

     eUtf16LittleEndian, eUtf16BigEndian:
     begin
       Write16BitFile(fs, psContents, peContentType = eUtf16BigEndian);
     end;

     eUtf32LittleEndian, eUtf32BigEndian:
     begin
       Write32BitFile(fs, psContents, peContentType = eUtf32BigEndian);
     end;

     else
       raise Exception.Create('Unknown file content type: ' + IntToStr(Ord(peContentType)));

   end;

  finally
    fs.Free;
  end;
end;

end.
