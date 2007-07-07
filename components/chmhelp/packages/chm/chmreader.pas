{ Copyright (C) <2005> <Andrew Haines> chmreader.pas

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
unit chmreader;

{$mode objfpc}{$H+}

//{$DEFINE CHM_DEBUG}
{$DEFINE CHM_DEBUG_CHUNKS}

interface

uses
  Classes, SysUtils, chmbase, paslzx;
  
type

  TLZXResetTableArr = array of QWord;
  
  PContextItem = ^TContextItem;
  TContextItem = record
    Context: THelpContext;
    Url: String;
  end;
  
  TContextList = class(TList)
  public
    procedure AddContext(Context: THelpContext; Url: String);
    function GetURL(Context: THelpContext): String;
    procedure Clear; override;
  end;
  { TITSFReader }

  TFileEntryForEach = procedure(Name: String; Offset, UncompressedSize, Section: Integer) of object;

  TITSFReader = class(TObject)
  protected
    fStream: TStream;
    fFreeStreamOnDestroy: Boolean;
    fChmHeader: TITSFHeader;
    fHeaderSuffix: TITSFHeaderSuffix;
    fDirectoryHeader: TITSPHeader;
    fDirectoryHeaderPos: Int64;
    fDirectoryHeaderLength: QWord;
    fDirectoryEntriesStartPos: Int64;
    fDirectoryEntries: array of TPMGListChunkEntry;
    fCachedEntry: TPMGListChunkEntry; //contains the last entry found by ObjectExists
    fDirectoryEntriesCount: LongWord;
  private
    procedure ReadHeader;
    function  GetChunkType(Stream: TMemoryStream; ChunkIndex: LongInt): TPMGchunktype;
    function  GetDirectoryChunk(Index: Integer; OutStream: TStream): Integer;
    function  ReadPMGLchunkEntryFromStream(Stream: TMemoryStream; var PMGLEntry: TPMGListChunkEntry): Boolean;
    function  ReadPMGIchunkEntryFromStream(Stream: TMemoryStream; var PMGIEntry: TPMGIIndexChunkEntry): Boolean;
    procedure LookupPMGLchunk(Stream: TMemoryStream; out PMGLChunk: TPMGListChunk);
    procedure LookupPMGIchunk(Stream: TMemoryStream; out PMGIChunk: TPMGIIndexChunk);

    procedure GetSections(out Sections: TStringList);
    function  GetBlockFromSection(SectionPrefix: String; StartPos: QWord; BlockLength: QWord): TMemoryStream;
    function  FindBlocksFromUnCompressedAddr(var ResetTableEntry: TPMGListChunkEntry;
       out CompressedSize: Int64; out UnCompressedSize: Int64; out LZXResetTable: TLZXResetTableArr): QWord;  // Returns the blocksize
  public
    constructor Create(AStream: TStream; FreeStreamOnDestroy: Boolean); virtual;
    destructor Destroy; override;
  public
    ChmLastError: LongInt;
    function IsValidFile: Boolean;
    procedure GetCompleteFileList(ForEach: TFileEntryForEach);
    function ObjectExists(Name: String): QWord; // zero if no. otherwise it is the size of the object
                                                // NOTE directories will return zero size even if they exist
    function GetObject(Name: String): TMemoryStream; // YOU must Free the stream
  end;
  
  { TChmReader }

  TChmReader = class(TITSFReader)
  protected
    fDefaultPage: String;
    fIndexFile: String;
    fTOCFile: String;
    fTitle: String;
    fPreferedFont: String;
    fContextList: TContextList;
    fLocaleID: DWord;
  private
    procedure ReadCommonData;
  public
    constructor Create(AStream: TStream; FreeStreamOnDestroy: Boolean); override;
    destructor Destroy; override;
  public
    function GetContextUrl(Context: THelpContext): String;
    function HasContextList: Boolean;
    property DefaultPage: String read fDefaultPage;
    property IndexFile: String read fIndexFile;
    property TOCFile: String read fTOCFile;
    property Title: String read fTitle write fTitle;
    property PreferedFont: String read fPreferedFont;
    property LocaleID: dword read fLocaleID;
  end;

  { TChmFileList }
  TChmFileList = class;
  TChmFileOpenEvent = procedure(ChmFileList: TChmFileList; Index: Integer) of object;
  TChmFileList = class(TStringList)
  protected
    fLastChm: TChmReader;
    fUnNotifiedFiles: TList;
    fOnOpenNewFile: TChmFileOpenEvent;
    procedure Delete(Index: Integer); override;
    function GetChm(AIndex: Integer): TChmReader;
    function GetFileName(AIndex: Integer): String;
    procedure OpenNewFile(AFileName: String);
    function CheckOpenFile(AFileName: String): Boolean;
    function MetaObjectExists(var Name: String): QWord;
    function MetaGetObject(Name: String): TMemoryStream;
    procedure SetOnOpenNewFile(AValue: TChmFileOpenEvent);
  public
    constructor Create(PrimaryFileName: String);
    destructor Destroy; override;
    function GetObject(Name: String): TMemoryStream;
    function IsAnOpenFile(AFileName: String): Boolean;
    function ObjectExists(Name: String; fChm: TChmReader = nil): QWord;
    //properties
    property Chm[Index: Integer]: TChmReader read GetChm;
    property FileName[Index: Integer]: String read GetFileName;
    property OnOpenNewFile: TChmFileOpenEvent read fOnOpenNewFile write SetOnOpenNewFile;
  end;
  
//ErrorCodes
const
  ERR_NO_ERR = 0;
  ERR_STREAM_NOT_ASSIGNED = 1;
  ERR_NOT_SUPPORTED_VERSION = 2;
  ERR_NOT_VALID_FILE = 3;
  ERR_UNKNOWN_ERROR = 10;
  
  function ChmErrorToStr(Error: Integer): String;

implementation

function ChmErrorToStr(Error: Integer): String;
begin
  Result := '';
  case Error of
    ERR_STREAM_NOT_ASSIGNED    : Result := 'ERR_STREAM_NOT_ASSIGNED';
    ERR_NOT_SUPPORTED_VERSION  : Result := 'ERR_NOT_SUPPORTED_VERSION';
    ERR_NOT_VALID_FILE         : Result := 'ERR_NOT_VALID_FILE';
    ERR_UNKNOWN_ERROR          : Result := 'ERR_UNKNOWN_ERROR';
  end;
end;

function ChunkType(Stream: TMemoryStream): TPMGchunktype;
var
  ChunkID: array[0..3] of char;
begin
  Result := ctUnknown;
  if Stream.Size< 4 then exit;
  Move(Stream.Memory^, ChunkId[0], 4);
  if ChunkID = 'PMGL' then Result := ctPMGL
  else if ChunkID = 'PMGI' then Result := ctPMGI;
end;

{ TITSFReader }

procedure TITSFReader.ReadHeader;
var
fHeaderEntries: array [0..1] of TITSFHeaderEntry;
begin
  fStream.Position := 0;
  fStream.Read(fChmHeader,SizeOf(fChmHeader));

  // Fix endian issues
  {$IFDEF ENDIAN_BIG}
  fChmHeader.Version := LEtoN(fChmHeader.Version);
  fChmHeader.HeaderLength := LEtoN(fChmHeader.HeaderLength);
  //Unknown_1
  fChmHeader.TimeStamp := BEtoN(fChmHeader.TimeStamp);//bigendian
  fChmHeader.LanguageID := LEtoN(fChmHeader.LanguageID);
  //Guid1
  //Guid2
  {$ENDIF}
  
  if not IsValidFile then Exit;
  
  // Copy EntryData into memory
  fStream.Read(fHeaderEntries[0], SizeOf(fHeaderEntries));

  if fChmHeader.Version > 2 then
    fStream.Read(fHeaderSuffix.Offset, SizeOf(QWord));
  fHeaderSuffix.Offset := LEtoN(fHeaderSuffix.Offset);
  // otherwise this is set in fill directory entries
  
  fStream.Position := LEtoN(fHeaderEntries[1].PosFromZero);
  fDirectoryHeaderPos := LEtoN(fHeaderEntries[1].PosFromZero);
  fStream.Read(fDirectoryHeader, SizeOf(fDirectoryHeader));
  {$IFDEF ENDIAN_BIG}
  with fDirectoryHeader do begin
    Version := LEtoN(Version);
    DirHeaderLength := LEtoN(DirHeaderLength);
    //Unknown1
    ChunkSize := LEtoN(ChunkSize);
    Density := LEtoN(Density);
    IndexTreeDepth := LEtoN(IndexTreeDepth);
    IndexOfRootChunk := LEtoN(IndexOfRootChunk);
    FirstPMGLChunkIndex := LEtoN(FirstPMGLChunkIndex);
    LastPMGLChunkIndex := LEtoN(LastPMGLChunkIndex);
    //Unknown2
    DirectoryChunkCount := LEtoN(DirectoryChunkCount);
    LanguageID := LEtoN(LanguageID);
    //GUID: TGuid;
    LengthAgain := LEtoN(LengthAgain);
  end;
  {$ENDIF}
  {$IFDEF CHM_DEBUG}
  WriteLn('PMGI depth = ', fDirectoryHeader.IndexTreeDepth);
  WriteLn('PMGI Root =  ', fDirectoryHeader.IndexOfRootChunk);
  {$ENDIF}
  fDirectoryEntriesStartPos := fStream.Position;
  fDirectoryHeaderLength := LEtoN(fHeaderEntries[1].Length);
end;

procedure TChmReader.ReadCommonData;
   // A little helper proc to make reading a null terminated string easier
   function ReadString(const Stream: TStream): String;
   var
     buf: array[0..49] of char;
   begin
     Result := '';
     repeat
       Stream.Read(buf, 50);
       Result := Result + buf;
     until Pos(#0, buf) > -1;
   end;
   procedure ReadFromSystem;
   var
     //Version: DWord;
     EntryType: Word;
     EntryLength: Word;
     Data: array[0..511] of char;
     fSystem: TMemoryStream;
     Tmp: String;
   begin
     fSystem := TMemoryStream(GetObject('/#SYSTEM'));
     if fSystem = nil then begin
       exit;
     end;
     fSystem.Position := 0;
     if fSystem.Size < SizeOf(DWord) then begin
       fSystem.Free;
       Exit;
     end;
     {Version := }LEtoN(fSystem.ReadDWord);
     while fSystem.Position < fSystem.Size do begin
       EntryType := LEtoN(fSystem.ReadWord);
       EntryLength := LEtoN(fSystem.ReadWord);
       case EntryType of
         0: // Table of contents
         begin
           if EntryLength > 511 then EntryLength := 511;
           fSystem.Read(Data[0], EntryLength);
           Data[EntryLength] := #0;
           fTOCFile := '/'+Data;
         end;
         1: // Index File
         begin
           if EntryLength > 511 then EntryLength := 511;
           fSystem.Read(Data[0], EntryLength);
           Data[EntryLength] := #0;
           fIndexFile := '/'+Data;
         end;
         2: // DefaultPage
         begin
           if EntryLength > 511 then EntryLength := 511;
           fSystem.Read(Data[0], EntryLength);
           Data[EntryLength] := #0;
           fDefaultPage := '/'+Data;
         end;
         3: // Title of chm
         begin
           if EntryLength > 511 then EntryLength := 511;
           fSystem.Read(Data[0], EntryLength);
           Data[EntryLength] := #0;
           fTitle := Data;
         end;
         4: // Locale ID
         begin
           fLocaleID := LEtoN(fSystem.ReadDWord);
           fSystem.Position := (fSystem.Position + EntryLength) - SizeOf(DWord);
         end;
         6: // chm file name. use this to get the index and toc name
         begin
           if EntryLength > 511 then EntryLength := 511;
           fSystem.Read(Data[0], EntryLength);
           Data[EntryLength] := #0;
           if (fIndexFile = '') then begin
             Tmp := '/'+Data+'.hhk';
             if (ObjectExists(Tmp) > 0) then begin
               fIndexFile := Tmp;
             end
           end;
           if (fTOCFile = '') then begin
             Tmp := '/'+Data+'.hhc';
             if (ObjectExists(Tmp) > 0) then begin
               fTOCFile := Tmp;
             end;
           end;
         end;
         16: // Prefered font
         begin
           if EntryLength > 511 then EntryLength := 511;
           fSystem.Read(Data[0], EntryLength);
           Data[EntryLength] := #0;
           fPreferedFont := Data;
         end;
       else
         // Skip entries we are not interested in
         fSystem.Position := fSystem.Position + EntryLength;
       end;
     end;
     fSystem.Free;
   end;
   procedure ReadFromWindows;
   var
     fWindows,
     fStrings: TMemoryStream;
     EntryCount,
     EntrySize: DWord;
     EntryStart: Int64;
     StrPosition: DWord;
     X: Integer;
     OffSet: Int64;
   begin
     fWindows := TMemoryStream(GetObject('/#WINDOWS'));
     if fWindows = nil then begin
       exit;
     end;
     fStrings := TMemoryStream(GetObject('/#STRINGS'));
     if fStrings = nil then begin
       if fWindows <> nil then fWindows.Free;
       Exit;
     end;
     fWindows.Position := 0;
     if (fWindows.Size = 0) or (fStrings.Size = 0) then begin
       fWindows.Free;
       fStrings.Free;
       Exit;
     end;
     EntryCount := LEtoN(fWindows.ReadDWord);
     EntrySize := LEtoN(fWindows.ReadDWord);
     OffSet := fWindows.Position;
     for X := 0 to EntryCount -1 do begin
       EntryStart := OffSet + (X*EntrySize);
       if fTitle = '' then begin
         fWindows.Position := EntryStart + $14;
         StrPosition := LEtoN(fWindows.ReadDWord);
         fStrings.Position := StrPosition;
         fTitle := '/'+ReadString(fStrings);
       end;
       if fTOCFile = '' then begin
         fWindows.Position := EntryStart + $60;
         StrPosition := LEtoN(fWindows.ReadDWord);
         fStrings.Position := StrPosition;
         fTOCFile := '/'+ReadString(fStrings);
       end;
       if fIndexFile = '' then begin
         fWindows.Position := EntryStart + $64;
         StrPosition := LEtoN(fWindows.ReadDWord);
         fStrings.Position := StrPosition;
         fIndexFile := '/'+ReadString(fStrings);
       end;
       if fDefaultPage = '' then begin
         fWindows.Position := EntryStart + $68;
         StrPosition := LEtoN(fWindows.ReadDWord);
         fStrings.Position := StrPosition;
         fDefaultPage := '/'+ReadString(fStrings);
       end;
     end;
   end;
   procedure ReadContextIds;
   var
     fIVB,
     fStrings: TStream;
     Str: String;
     Value: DWord;
     OffSet: DWord;
     //TotalSize: DWord;
   begin
     fIVB := GetObject('/#IBV');
     if fIVB = nil then Exit;
     fStrings := GetObject('/#STRINGS');
     if fStrings = nil then begin
       fIVB.Free;
       Exit;
     end;
     fIVB.Position := 0;
     {TotalSize := }LEtoN(fIVB.ReadDWord);
     while fIVB.Position < fIVB.Size do begin
       Value := LEtoN(fIVB.ReadDWord);
       OffSet := LEtoN(fIVB.ReadDWord);
       fStrings.Position := Offset;
       Str := '/'+ReadString(fStrings);
       fContextList.AddContext(Value, Str);
     end;
   end;
begin
   ReadFromSystem;
   ReadFromWindows;
   ReadContextIds;
   {$IFDEF CHM_DEBUG}   
   WriteLn('TOC=',fTocfile);
   WriteLn('DefaultPage=',fDefaultPage);
   {$ENDIF}
end;

constructor TChmReader.Create(AStream: TStream; FreeStreamOnDestroy: Boolean);
begin
  inherited Create(AStream, FreeStreamOnDestroy);
  if not IsValidFile then exit;

  fContextList := TContextList.Create;
  ReadCommonData;
end;

destructor TChmReader.Destroy;
begin
  fContextList.Free;
  inherited Destroy;
end;

function TITSFReader.GetChunkType(Stream: TMemoryStream; ChunkIndex: LongInt): TPMGchunktype;
var
  Sig: array[0..3] of char;
begin
  Result := ctUnknown;
  Stream.Position := fDirectoryEntriesStartPos + (fDirectoryHeader.ChunkSize * ChunkIndex);

  Stream.Read(Sig, 4);
  if Sig = 'PMGL' then Result := ctPMGL
  else if Sig = 'PMGI' then Result := ctPMGI;
end;

function TITSFReader.GetDirectoryChunk(Index: Integer; OutStream: TStream): Integer;
begin
  Result := Index;
  fStream.Position := fDirectoryEntriesStartPos + (fDirectoryHeader.ChunkSize * Index);
  OutStream.Position := 0;
  OutStream.Size := fDirectoryHeader.ChunkSize;
  OutStream.CopyFrom(fStream, fDirectoryHeader.ChunkSize);
  OutStream.Position := 0;
end;

procedure TITSFReader.LookupPMGLchunk(Stream: TMemoryStream; out PMGLChunk: TPMGListChunk);
begin
  //Stream.Position := fDirectoryEntriesStartPos + (fDirectoryHeader.ChunkSize * ChunkIndex);
  Stream.Read(PMGLChunk, SizeOf(PMGLChunk));
  {$IFDEF ENDIAN_BIG}
  with PMGLChunk do begin
    UnusedSpace := LEtoN(UnusedSpace);
    //Unknown1
    PreviousChunkIndex := LEtoN(PreviousChunkIndex);
    NextChunkIndex := LEtoN(NextChunkIndex);
  end;
  {$ENDIF}
end;

function TITSFReader.ReadPMGLchunkEntryFromStream(Stream: TMemoryStream; var PMGLEntry: TPMGListChunkEntry): Boolean;
var
Buf: array [0..1023] of char;
NameLength: LongInt;
begin
  Result := False;
  //Stream.Position := fDirectoryEntriesStartPos + (fDirectoryHeader.ChunkSize * ChunkIndex);
  NameLength := LongInt(GetCompressedInteger(Stream));

  if NameLength > 1022 then NameLength := 1022;
  Stream.Read(buf[0], NameLength);
  buf[NameLength] := #0;
  PMGLEntry.Name := buf;
  PMGLEntry.ContentSection := LongWord(GetCompressedInteger(Stream));
  PMGLEntry.ContentOffset := GetCompressedInteger(Stream);
  PMGLEntry.DecompressedLength := GetCompressedInteger(Stream);
  if NameLength = 0 then Exit; // failed GetCompressedInteger sanity check
  Result := True;
end;

procedure TITSFReader.LookupPMGIchunk(Stream: TMemoryStream; out PMGIChunk: TPMGIIndexChunk);
begin
  //Stream.Position := fDirectoryEntriesStartPos + (fDirectoryHeader.ChunkSize * ChunkIndex);
  Stream.Read(PMGIChunk, SizeOf(PMGIChunk));
  {$IFDEF ENDIAN_BIG}
  with PMGIChunk do begin
    UnusedSpace := LEtoN(UnusedSpace);
  end;
  {$ENDIF}
end;

function TITSFReader.ReadPMGIchunkEntryFromStream(Stream: TMemoryStream;
  var PMGIEntry: TPMGIIndexChunkEntry): Boolean;
var
Buf: array [0..1023] of char;
NameLength: LongInt;
begin
  Result := False;
  //Stream.Position := fDirectoryEntriesStartPos + (fDirectoryHeader.ChunkSize * ChunkIndex);
  NameLength := LongInt(GetCompressedInteger(Stream));
  if NameLength > 1023 then NameLength := 1023;
  Stream.Read(buf, NameLength);

  buf[NameLength] := #0;
  PMGIEntry.Name := buf;
  
  PMGIEntry.ListingChunk := GetCompressedInteger(Stream);
  if NameLength = 0 then Exit; // failed GetCompressedInteger sanity check
  Result := True;
end;

constructor TITSFReader.Create(AStream: TStream; FreeStreamOnDestroy: Boolean);
begin
  fStream := AStream;
  fFreeStreamOnDestroy := FreeStreamOnDestroy;
  ReadHeader;
  if not IsValidFile then Exit;
end;

destructor TITSFReader.Destroy;
begin
  SetLength(fDirectoryEntries, 0);
  if fFreeStreamOnDestroy then FreeAndNil(fStream);

  inherited Destroy;
end;

function TITSFReader.IsValidFile: Boolean;
begin
  if (fStream = nil) then ChmLastError := ERR_STREAM_NOT_ASSIGNED
  else if (fChmHeader.ITSFsig <> 'ITSF') then ChmLastError := ERR_NOT_VALID_FILE
  else if (fChmHeader.Version <> 2) and (fChmHeader.Version <> 3) then
    ChmLastError := ERR_NOT_SUPPORTED_VERSION;
  Result := ChmLastError = ERR_NO_ERR;
end;

procedure TITSFReader.GetCompleteFileList(ForEach: TFileEntryForEach);
var
  ChunkStream: TMemoryStream;
  I : Integer;
  Entry: TPMGListChunkEntry;
  PMGLChunk: TPMGListChunk;
  CutOffPoint: Integer;
  NameLength: Integer;
  {$IFDEF CHM_DEBUG_CHUNKS}
  PMGIChunk: TPMGIIndexChunk;
  PMGIndex: Integer;
  {$ENDIF}
begin
  if ForEach = nil then Exit;
  ChunkStream := TMemoryStream.Create;
  for I := 0 to fDirectoryHeader.DirectoryChunkCount-1 do begin
    GetDirectoryChunk(I, ChunkStream);
    case ChunkType(ChunkStream) of
    ctPMGL:
     begin
       LookupPMGLchunk(ChunkStream, PMGLChunk);
       {$IFDEF CHM_DEBUG_CHUNKS}
        WriteLn('PMGL: ', I, ' Prev PMGL: ', PMGLChunk.PreviousChunkIndex, ' Next PMGL: ', PMGLChunk.NextChunkIndex);
       {$ENDIF}
       CutOffPoint := ChunkStream.Size - PMGLChunk.UnusedSpace - 10;
       while ChunkStream.Position <  CutOffPoint do begin
         NameLength := GetCompressedInteger(ChunkStream);
         SetLength(Entry.Name, NameLength);
         ChunkStream.ReadBuffer(Entry.Name[1], NameLength);
         if (Entry.Name = '') or (ChunkStream.Position > CutOffPoint) then
           Continue; // we have entered the quickref section
         Entry.ContentSection := GetCompressedInteger(ChunkStream);
         Entry.ContentOffset := GetCompressedInteger(ChunkStream);
         Entry.DecompressedLength := GetCompressedInteger(ChunkStream);
         fCachedEntry := Entry; // if the caller trys to get this data we already know where it is :)
         ForEach(Entry.Name, Entry.ContentOffset, Entry.DecompressedLength, Entry.ContentSection);
       end;
     end;
    {$IFDEF CHM_DEBUG_CHUNKS}
    ctPMGI:
     begin
       WriteLn('PMGI: ', I);
       LookupPMGIchunk(ChunkStream, PMGIChunk);
       CutOffPoint := ChunkStream.Size - PMGIChunk.UnusedSpace - 10;
       while ChunkStream.Position <  CutOffPoint do begin
         NameLength := GetCompressedInteger(ChunkStream);
         SetLength(Entry.Name, NameLength);
         ChunkStream.ReadBuffer(Entry.Name[1], NameLength);
         PMGIndex := GetCompressedInteger(ChunkStream);
         WriteLn(Entry.Name, '  ', PMGIndex);
       end;
     end;
    ctUnknown: WriteLn('UNKNOWN CHUNKTYPE!' , I);
    {$ENDIF}
    end;
  end;
end;

function TITSFReader.ObjectExists(Name: String): QWord;
var
  ChunkStream: TMemoryStream;
  QuickRefCount: Word;
  QuickRefIndex: array of Word;
  ItemCount: Integer;
  procedure ReadQuickRefSection;
  var
    OldPosn: Int64;
    Posn: Integer;
    I: Integer;
  begin
    OldPosn := ChunkStream.Position;
    Posn := ChunkStream.Size-1-SizeOf(Word);
    ChunkStream.Position := Posn;
    
    ItemCount := LEToN(ChunkStream.ReadWord);
    //WriteLn('Max ITems for next block = ', ItemCount-1);
    QuickRefCount := ItemCount  div (1 + (1 shl fDirectoryHeader.Density));
    //WriteLn('QuickRefCount = ' , QuickRefCount);
    SetLength(QuickRefIndex, QuickRefCount+1);
    for I := 1 to QuickRefCount do begin
      Dec(Posn, SizeOf(Word));
      ChunkStream.Position := Posn;
      QuickRefIndex[I] := LEToN(ChunkStream.ReadWord);
    end;
    Inc(QuickRefCount);
    ChunkStream.Position := OldPosn;
  end;
  function ReadString(StreamPosition: Integer = -1): String;
  var
    NameLength: Integer;
  begin
    if StreamPosition > -1 then ChunkStream.Position := StreamPosition;

    NameLength := GetCompressedInteger(ChunkStream);
    SetLength(Result, NameLength);
    ChunkStream.Read(Result[1], NameLength);
  end;
var
  PMGLChunk: TPMGListChunk;
  PMGIChunk: TPMGIIndexChunk;
  //ChunkStream: TMemoryStream; declared above  
  Entry: TPMGListChunkEntry;
  NextIndex: Integer;
  EntryName: String;  
  CRes: Integer;
  I: Integer;
begin
  Result := 0;

  if Name = '' then Exit;
  if fDirectoryHeader.DirectoryChunkCount = 0 then exit;

  //WriteLn('Looking for ', Name);
  if Name = fCachedEntry.Name then
    Exit(fCachedEntry.DecompressedLength); // we've already looked it up

  ChunkStream := TMemoryStream.Create;

  try
  
  NextIndex := fDirectoryHeader.IndexOfRootChunk;
  if NextIndex < 0 then NextIndex := 0; // no PMGI chunks
  
  while NextIndex > -1  do begin
    GetDirectoryChunk(NextIndex, ChunkStream);
    NextIndex := -1;
    ReadQuickRefSection;
    //WriteLn('In Block ', ChunkIndex);
    case ChunkType(ChunkStream) of
      ctUnknown: // something is wrong
        begin
          {$IFDEF CHM_DEBUG}WriteLn(ChunkIndex, ' << Unknown BlockType!');{$ENDIF}
          Break;
        end;
      ctPMGI: // we must follow the PMGI tree until we reach a PMGL block
        begin
          LookupPMGIchunk(ChunkStream, PMGIChunk);          

          //QuickRefIndex[0] := ChunkStream.Position;

          I := 0;
          while ChunkStream.Position <= ChunkStream.Size - PMGIChunk.UnusedSpace do begin;
            EntryName := ReadString;
            if EntryName = '' then break;
            if ChunkStream.Position >= ChunkStream.Size - PMGIChunk.UnusedSpace then break;
            CRes := ChmCompareText(Name, EntryName);
            if CRes = 0 then begin
              // no more need of this block. onto the next!              
              NextIndex := GetCompressedInteger(ChunkStream);
              Continue;
            end;
            if  CRes < 0 then begin
              if I = 0 then Break; // File doesn't exist
              // file is in previous entry              
              Break;
            end;
            NextIndex := GetCompressedInteger(ChunkStream);
            Inc(I);
          end;
        end;
      ctPMGL:
        begin
          LookupPMGLchunk(ChunkStream, PMGLChunk);
          QuickRefIndex[0] := ChunkStream.Position;
          I := 0;
          while ChunkStream.Position <= ChunkStream.Size - PMGLChunk.UnusedSpace do begin
            // we consume the entry by reading it            
            Entry.Name := ReadString;
            if Entry.Name = '' then break;
            if ChunkStream.Position >= ChunkStream.Size - PMGLChunk.UnusedSpace then break;

            Entry.ContentSection := GetCompressedInteger(ChunkStream);
            Entry.ContentOffset := GetCompressedInteger(ChunkStream);
            Entry.DecompressedLength := GetCompressedInteger(ChunkStream);

            CRes := ChmCompareText(Name, Entry.Name);
            if CRes = 0 then begin
              fCachedEntry := Entry;
              Result := Entry.DecompressedLength;              
              Break;
            end;
            Inc(I);
          end;
        end; // case
    end;
  end;
  finally
  ChunkStream.Free;
  end;
end;

function TITSFReader.GetObject(Name: String): TMemoryStream;
var
  SectionNames: TStringList;
  Entry: TPMGListChunkEntry;
  SectionName: String;
begin
  Result := nil;
  if ObjectExists(Name) = 0 then begin
    //WriteLn('Object ', name,' Doesn''t exist or is zero sized.');
    Exit;
  end;

  Entry := fCachedEntry;
  if Entry.ContentSection = 0 then begin
    Result := TMemoryStream.Create;
    fStream.Position := fHeaderSuffix.Offset+ Entry.ContentOffset;
    Result.CopyFrom(fStream, fCachedEntry.DecompressedLength);
  end
  else begin // we have to get it from ::DataSpace/Storage/[MSCompressed,Uncompressed]/ControlData
    GetSections(SectionNames);
    FmtStr(SectionName, '::DataSpace/Storage/%s/',[SectionNames[Entry.ContentSection-1]]);
    Result := GetBlockFromSection(SectionName, Entry.ContentOffset, Entry.DecompressedLength);
    SectionNames.Free;
  end;
  if Result <> nil then Result.Position := 0;
end;

function TChmReader.GetContextUrl(Context: THelpContext): String;
begin
  // will get '' if context not found
 Result := fContextList.GetURL(Context);
end;

function TChmReader.HasContextList: Boolean;
begin
  Result := fContextList.Count > 0;
end;

procedure TITSFReader.GetSections(out Sections: TStringList);
var
  Stream: TStream;
  EntryCount: Word;
  X: Integer;
  {$IFDEF ENDIAN_BIG}
  I: Integer;
  {$ENDIF}
  WString: array [0..31] of WideChar;
  StrLength: Word;
begin
  Sections := TStringList.Create;
  //WriteLn('::DataSpace/NameList Size = ', ObjectExists('::DataSpace/NameList'));
  Stream := GetObject('::DataSpace/NameList');

  if Stream = nil then begin
    //WriteLn('Failed to get ::DataSpace/NameList!');
    exit;
  end;
  
  Stream.Position := 2;
  EntryCount := LEtoN(Stream.ReadWord);
  for X := 0 to EntryCount -1 do begin
    StrLength := LEtoN(Stream.ReadWord);
    if StrLength > 31 then StrLength := 31;
    Stream.Read(WString, SizeOf(WideChar)*(StrLength+1)); // the strings are stored null terminated
    {$IFDEF ENDIAN_BIG}
    for I := 0 to StrLength-1 do
      WString[I] := WideChar(LEtoN(Ord(WString[I])));
    {$ENDIF}
    Sections.Add(WString);
  end;
  // the sections are sorted alphabetically, this way section indexes will jive
  Sections.Sort;
  Stream.Free;
end;

function TITSFReader.GetBlockFromSection(SectionPrefix: String; StartPos: QWord;
  BlockLength: QWord): TMemoryStream;
var
  Compressed: Boolean;
  Sig: Array [0..3] of char;
  CompressionVersion: LongWord;
  CompressedSize: Int64;
  UnCompressedSize: Int64;
  //LZXResetInterval: LongWord;
  //LZXWindowSize: LongWord;
  //LZXCacheSize: LongWord;
  ResetTableEntry: TPMGListChunkEntry;
  ResetTable: TLZXResetTableArr;
  WriteCount: QWord;
  BlockWriteLength: QWord;
  WriteStart: LongWord;
  ReadCount:LongInt;
  LZXState: PLZXState;
  InBuf: array of Byte;
  OutBuf: PByte;
  BlockSize: QWord;
  X: Integer;
  FirstBlock, LastBlock: LongInt;
  ResultCode: LongInt;
  procedure ReadBlock;
  begin
    if ReadCount > Length(InBuf) then
      SetLength(InBuf, ReadCount);
    fStream.Read(InBuf[0], ReadCount);
  end;
begin
  // okay now the fun stuff ;)
  Result := nil;
  Compressed := ObjectExists(SectionPrefix+'Transform/{7FC28940-9D31-11D0-9B27-00A0C91E9C7C}/InstanceData/ResetTable')>0;
  // the easy method
  if Not(Compressed) then begin
    if ObjectExists(SectionPrefix+'Content') > 0 then begin
      Result := TMemoryStream.Create;
      fStream.Position := fHeaderSuffix.Offset + fCachedEntry.ContentOffset + StartPos;
      Result.CopyFrom(fStream, BlockLength);
    end;
    Exit;
  end
  else
    ResetTableEntry := fCachedEntry;

  // First make sure that it is a compression we can read
  if ObjectExists(SectionPrefix+'ControlData') > 0 then begin
    fStream.Position := fHeaderSuffix.Offset + fCachedEntry.ContentOffset + 4;
    fStream.Read(Sig, 4);
    if Sig <> 'LZXC' then Exit;
    CompressionVersion := LEtoN(fStream.ReadDWord);
    if CompressionVersion > 2 then exit;
    {LZXResetInterval := }LEtoN(fStream.ReadDWord);
    {LZXWindowSize := }LEtoN(fStream.ReadDWord);
    {LZXCacheSize := }LEtoN(fStream.ReadDWord);


    BlockSize := FindBlocksFromUnCompressedAddr(ResetTableEntry, CompressedSize, UnCompressedSize, ResetTable);
    if UncompressedSize > 0 then ; // to avoid a compiler note
    if StartPos > 0 then
      FirstBlock := StartPos div BlockSize
    else
      FirstBlock := 0;
    LastBlock := (StartPos+BlockLength) div BlockSize;

    if ObjectExists(SectionPrefix+'Content') = 0 then exit;
    //WriteLn('Compressed Data start''s at: ', fHeaderSuffix.Offset + fCachedEntry.ContentOffset,' Size is: ', fCachedEntry.DecompressedLength);
    Result := TMemoryStream.Create;
    Result.Size := BlockLength;
    SetLength(InBuf,BlockSize);
    OutBuf := GetMem(BlockSize);
    // First Init a PLZXState
    LZXState := LZXinit(16);
    if LZXState = nil then begin
      Exit;
    end;
    // if FirstBlock is odd (1,3,5,7 etc) we have to read the even block before it first.
    if (FirstBlock <> 0) and (FirstBlock mod 2 > 0) then begin
      fStream.Position := fHeaderSuffix.Offset + fCachedEntry.ContentOffset + (ResetTable[FirstBLock-1]);
      ReadCount := ResetTable[FirstBlock] - ResetTable[FirstBlock-1];
      BlockWriteLength:=BlockSize;
      ReadBlock;
      ResultCode := LZXdecompress(LZXState, @InBuf[0], OutBuf, ReadCount, LongInt(BlockWriteLength));
    end;
    // now start the actual decompression loop
    for X := FirstBlock to LastBlock do begin
      fStream.Position := fHeaderSuffix.Offset + fCachedEntry.ContentOffset + (ResetTable[X]);

      if X = FirstBLock then
        WriteStart := StartPos - (X*BlockSize)
      else
        WriteStart := 0;

      if X = High(ResetTable) then
        ReadCount := CompressedSize - ResetTable[X]
      else
        ReadCount := ResetTable[X+1] - ResetTable[X];

      BlockWriteLength := BlockSize;
      
      if FirstBlock = LastBlock then begin
        WriteCount := BlockLength;
      end
      else if X = LastBlock then
        WriteCount := (StartPos+BlockLength) - (X*BlockSize)
      else WriteCount := BlockSize - WriteStart;

      ReadBlock;
      ResultCode := LZXdecompress(LZXState, @InBuf[0], OutBuf, ReadCount, LongInt(BlockWriteLength));

      //now write the decompressed data to the stream
      if ResultCode = DECR_OK then begin
        Result.Write(OutBuf[WriteStart], Int64(WriteCount));
      end
      else begin
        {$IFDEF CHM_DEBUG} // windows gui program will cause an exception with writeln's
        WriteLn('Decompress FAILED with error code: ', ResultCode);
        {$ENDIF}
        Result.Free;
        Result := Nil;
        FreeMem(OutBuf);
        SetLength(ResetTable,0);
        LZXteardown(LZXState);
        Exit;
      end;
      
      // if the next block is an even numbered block we have to reset the decompressor state
      if (X < LastBlock) and (X mod 2 > 0) then LZXreset(LZXState);

    end;
    FreeMem(OutBuf);
    SetLength(ResetTable,0);
    LZXteardown(LZXState);
  end;
end;

function TITSFReader.FindBlocksFromUnCompressedAddr(var ResetTableEntry: TPMGListChunkEntry;
  out CompressedSize: Int64; out UnCompressedSize: Int64; out LZXResetTable: TLZXResetTableArr): QWord;
var
  BlockCount: LongWord;
  {$IFDEF ENDIAN_BIG}
  I: Integer;
  {$ENDIF}
begin
  Result := 0;
  fStream.Position := fHeaderSuffix.Offset + ResetTableEntry.ContentOffset;
  fStream.ReadDWord;
  BlockCount := LEtoN(fStream.ReadDWord);
  fStream.ReadDWord;
  fStream.ReadDWord; // TableHeaderSize;
  fStream.Read(UnCompressedSize, SizeOf(QWord));
  UnCompressedSize := LEtoN(UnCompressedSize);
  fStream.Read(CompressedSize, SizeOf(QWord));
  CompressedSize := LEtoN(CompressedSize);
  fStream.Read(Result, SizeOf(QWord)); // block size
  Result := LEtoN(Result);

  // now we are located at the first block index

  SetLength(LZXResetTable, BlockCount);
  fStream.Read(LZXResetTable[0], SizeOf(QWord)*BlockCount);
  {$IFDEF ENDIAN_BIG}
  for I := 0 to High(LZXResetTable) do
    LZXResetTable[I] := LEtoN(LZXResetTable[I]);
  {$ENDIF}
end;

{ TContextList }

procedure TContextList.AddContext(Context: THelpContext; Url: String);
var
  ContextItem: PContextItem;
begin
  New(ContextItem);
  Add(ContextItem);
  ContextItem^.Context := Context;
  ContextItem^.Url := Url;
end;

function TContextList.GetURL(Context: THelpContext): String;
var
  X: Integer;
begin
  Result := '';
  for X := 0 to Count-1 do begin
    if PContextItem(Get(X))^.Context = Context then begin
      Result := PContextItem(Get(X))^.Url;
      Exit;
    end;
  end;
end;

procedure TContextList.Clear;
var
  X: Integer;
begin
  for X := Count-1 downto 0 do begin
    Dispose(PContextItem(Get(X)));
    Delete(X);
  end;
end;


{ TChmFileList }

procedure TChmFileList.Delete(Index: Integer);
begin
  Chm[Index].Free;
  inherited Delete(Index);
end;

function TChmFileList.GetChm(AIndex: Integer): TChmReader;
begin
  Result := TChmReader(Objects[AIndex]);
end;

function TChmFileList.GetFileName(AIndex: Integer): String;
begin
  Result := Strings[AIndex];
end;

procedure TChmFileList.OpenNewFile(AFileName: String);
var
AStream: TFileStream;
AChm: TChmReader;
AIndex: Integer;
begin
  if not FileExists(AFileName) then exit;
  AStream := TFileStream.Create(AFileName, fmOpenRead);
  AChm := TChmReader.Create(AStream, True);
  AIndex := AddObject(AFileName, AChm);
  fLastChm := AChm;
  if Assigned(fOnOpenNewFile) then fOnOpenNewFile(Self, AIndex)
  else fUnNotifiedFiles.Add(AChm);
end;

function TChmFileList.CheckOpenFile(AFileName: String): Boolean;
var
  X: Integer;
  
begin
  Result := False;
  for X := 0 to Count-1 do begin
    if ExtractFileName(FileName[X]) = AFileName then begin
      fLastChm := Chm[X];
      Result := True;
      Exit;
    end;
  end;
  if not Result then begin
    AFileName := ExtractFilePath(FileName[0])+AFileName;
    if FileExists(AFileName) and (ExtractFileExt(AFileName) = '.chm') then OpenNewFile(AFileName);
    Result := True;
  end;
end;

function TChmFileList.MetaObjectExists(var Name: String): QWord;
var
  AFileName: String;
  URL: String;
  fStart, fEnd: Integer;
  Found: Boolean;
begin
  Found := False;
  Result := 0;
  //Known META file link types
  //       ms-its:name.chm::/topic.htm
  //mk:@MSITStore:name.chm::/topic.htm
  if Pos('ms-its:', Name) > 0 then begin
    fStart := Pos('ms-its:', Name)+Length('ms-its:');
    fEnd := Pos('::', Name)-fStart;
    AFileName := Copy(Name, fStart, fEnd);
    fStart := fEnd+fStart+2;
    fEnd := Length(Name) - (fStart-1);
    URL := Copy(Name, fStart, fEnd);
    Found := True;
  end
  else if Pos('mk:@MSITStore:', Name) > 0 then begin
    fStart := Pos('mk:@MSITStore:', Name)+Length('mk:@MSITStore:');
    fEnd := Pos('::', Name)-fStart;
    AFileName := Copy(Name, fStart, fEnd);
    fStart := fEnd+fStart+2;
    fEnd := Length(Name) - (fStart-1);
    URL := Copy(Name, fStart, fEnd);
    Found := True;
  end;
  if not Found then exit;
  if CheckOpenFile(AFileName) then
    Result := fLastChm.ObjectExists(URL);
  if Result > 0 then NAme := Url;
end;

function TChmFileList.MetaGetObject(Name: String): TMemoryStream;
begin
  Result := nil;
  if MetaObjectExists(Name) > 0 then Result := fLastChm.GetObject(Name);
end;

constructor TChmFileList.Create(PrimaryFileName: String);
begin
  inherited Create;
  fUnNotifiedFiles := TList.Create;
  OpenNewFile(PrimaryFileName);
end;

destructor TChmFileList.Destroy;
begin
  fUnNotifiedFiles.Free;
end;

procedure TChmFileList.SetOnOpenNewFile(AValue: TChmFileOpenEvent);
var
  X: Integer;
begin
  fOnOpenNewFile := AValue;
  if AValue = nil then exit;
  for X := 0 to fUnNotifiedFiles.Count-1 do
    AValue(Self, X);
  fUnNotifiedFiles.Clear;
end;

function TChmFileList.ObjectExists(Name: String; fChm: TChmReader = nil): QWord;
begin
  Result := 0;
  if Count = 0 then exit;
  if fChm <> nil then fLastChm := fChm;
  Result := fLastChm.ObjectExists(Name);
  if Result = 0 then begin
    Result := Chm[0].ObjectExists(Name);
    if Result > 0 then fLastChm := Chm[0];
  end;
  if Result = 0 then begin
    Result := MetaObjectExists(Name);
  end;
end;

function TChmFileList.GetObject(Name: String): TMemoryStream;
begin
  Result := nil;
  if Count = 0 then exit;
  Result := fLastChm.GetObject(Name);
  if Result = nil then Result := MetaGetObject(Name);
end;

function TChmFileList.IsAnOpenFile(AFileName: String): Boolean;
var
  X: Integer;
begin
  Result := False;
  for X := 0 to Count-1 do begin
    if AFileName = FileName[X] then Exit(True);
  end;
end;

end.

