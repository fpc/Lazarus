{ Copyright (C) <2005> <Andrew Haines> chmbase.pas

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
{
  See the file COPYING.modifiedLGPL, included in this distribution,
  for details about the copyright.
}
unit chmbase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;
  
type
  {$PACKRECORDS C}
  TITSFHeader= record
    ITSFsig: array [0..3] of char;
    Version: LongWord;
    HeaderLength: LongWord;
    Unknown_1: LongWord;
    TimeStamp: LongWord; //bigendian
    LanguageID: LongWord;
    Guid1: TGuid;
    Guid2: TGuid;
  end;
  TITSFHeaderEntry = record
    PosFromZero: QWord;
    Length: QWord;
  end;
  
  //Version 3 has this qword. 2 does not
  TITSFHeaderSuffix = record
    Offset: QWord; // offset within file of content section 0
  end;
  
  TITSPHeaderPrefix = record
    Unknown1: LongWord;// = $01FE
    Unknown2: LongWord;// = 0
    FileSize: QWord;
    Unknown3: LongWord;// =0
    Unknown4: LongWord;// =0
  end;
  
  TITSPHeader = record
    ITSPsig: array [0..3] of char; // = 'ITSP'
    Version: LongWord;             // =1
    DirHeaderLength: Longword;     // Length of the directory header
    Unknown1: LongWord;            // =$0a
    ChunkSize: LongWord;           // $1000
    Density: LongWord; // usually = 2
    IndexTreeDepth: LongWord;// 1 if there is no index 2 if there is one level of PMGI chunks
    IndexOfRootChunk: LongInt;// -1 if no root chunk
    FirstPMGLChunkIndex,
    LastPMGLChunkIndex: LongWord;
    Unknown2: LongInt; // = -1
    DirectoryChunkCount: LongWord;
    LanguageID: LongWord;
    GUID: TGuid;
    LengthAgain: LongWord; //??? $54
    Unknown3: LongInt; // = -1
    Unknown4: LongInt; // = -1
    Unknown5: LongInt; // = -1
  end;
  
  TPMGchunktype = (ctPMGL, ctPMGI, ctUnknown);
  
  TPMGListChunk = record
    PMGLsig: array [0..3] of char;
    UnusedSpace: Longword; ///!!! this value can also represent the size of quickref area in the end of the chunk
    Unknown1: Longword; //always 0
    PreviousChunkIndex: LongInt; // chunk number of the prev listing chunk when reading dir in sequence
                                 // (-1 if this is the first listing chunk)
    NextChunkIndex: LongInt; // chunk number of the next listing chunk (-1 if this is the last chunk)
  end;

  PPMGListChunkEntry = ^TPMGListChunkEntry;
  TPMGListChunkEntry = record
    //NameLength: LongInt; we don't need this permanantly so I've moved it to a temp var
    Name: String;
    ContentSection: LongWord;//QWord;
    ContentOffset: QWord;
    DecompressedLength: QWord;
  end;
  
  TPMGIIndexChunk = record
    PMGIsig: array [0..3] of char;
    UnusedSpace: LongWord; // has a quickref area
  end;
  
  TPMGIIndexChunkEntry = record
    Name: String;
    ListingChunk: DWord;
  end;

  
const
  ITSFHeaderGUID : TGuid = '{7C01FD10-7BAA-11D0-9E0C-00A0C922E6EC}';
  ITSFFileSig: array [0..3] of char = 'ITSF';
  
  ITSPHeaderGUID : TGuid = '{5D02926A-212E-11D0-9DF9-00A0C922E6EC}';
  ITSPHeaderSig: array [0..3] of char = 'ITSP';

  // this function will advance the stream to the end of the compressed integer
  // and return the value
  function GetCompressedInteger(const Stream: TStream): DWord;
  // returns the number of bytes written to the stream
  function WriteCompressedInteger(const Stream: TStream; ANumber: DWord): DWord;
  function WriteCompressedInteger(Buffer: Pointer; ANumber: DWord): DWord;


implementation

function GetCompressedInteger(const Stream: TStream): DWord;
var
  total: QWord = 0;
  temp: Byte;
  Sanity: Integer = 0;
begin
  temp := Stream.ReadByte;
  while temp >= $80 do begin
    total := total shl 7;
    total := total + temp and $7f;
    temp := Stream.ReadByte;
    Inc(Sanity);
    if Sanity > 8 then begin
      Result := 0;
      Exit;
    end;
  end;
  Result := (total shl 7) + temp;
end;

function WriteCompressedInteger(const Stream: TStream; ANumber: DWord): DWord;
var
  Buffer: QWord; // Easily large enough
begin
  Result := WriteCompressedInteger(@Buffer, ANumber);
  Result := Stream.Write(Buffer, Result);
end;

function WriteCompressedInteger(Buffer: Pointer; ANumber: DWord): DWord;
var
  bit: dword;
  mask: QWord;
  buf: PByte;
  Value: DWord = 0;
  TheEnd: DWord = 0;
  I: Integer;
begin
  bit := (sizeof(DWord)*8)div 7*7;
  buf := @Value;
  while True do begin
    mask := $7f shl bit;
    if (bit = 0) or ((ANumber and mask)<>0) then break;
    Dec(bit, 7);
  end;

  while True do begin
    buf^ := Byte(((ANumber shr bit)and $7f));
    if(bit = 0) then break;
    buf^ := buf^ or $80;
    Inc(buf);
    Dec(bit, 7);
    Inc(TheEnd);
  end;

  buf := @Value;
  //Result := Stream.Write(Value, TheEnd+1);
  Result := TheEnd+1;
  Move(Value, Buffer^, Result);

end;

end.

