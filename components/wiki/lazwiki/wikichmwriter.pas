{ Copyright (C) <2005> <Andrew Haines> chmwriter.pas

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
  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.
}
unit wikichmwriter;
{$MODE OBJFPC}{$H+}
{ $DEFINE LZX_USETHREADS}

interface
uses Classes,
  wikiChmBase, wikichmtypes, wikichmspecialfiles, wikiHtmlIndexer, wikichmsitemap,
  contnrs, Avl_Tree{$IFDEF LZX_USETHREADS}, lzxcompressthread{$ENDIF};

Const
   DefaultHHC = 'Default.hhc';
   DefaultHHK = 'Default.hhk';

Type
  TGetDataFunc = function (const DataName: String; out PathInChm: String; out FileName: String; var Stream: TStream): Boolean of object;
  //  DataName :  A FileName or whatever so that the getter can find and open the file to add
  //  PathInChm:  This is the absolute path in the archive. i.e. /home/user/helpstuff/
  //              becomes '/' and /home/user/helpstuff/subfolder/ > /subfolder/
  //  FileName :  /home/user/helpstuff/index.html > index.html
  //  Stream   :  the file opened with DataName should be written to this stream

Type
   TStringIndex = Class    // AVLTree needs wrapping in non automated reference type also used in filewriter.
                      TheString : String;
                      StrId     : Integer;
                    end;
   TUrlStrIndex = Class
                      UrlStr    : String;
                      UrlStrId  : Integer;
                    end;

  { TITSFWriter }

  TITSFWriter = class(TObject)
    FOnLastFile: TNotifyEvent;
  private
    ForceExit: Boolean;
    FInternalFiles: TFileEntryList; // Contains a complete list of files in the chm including
    FFrameSize: LongWord;           // uncompressed files and special internal files of the chm
    FCurrentStream: TStream; // used to buffer the files that are to be compressed
    FCurrentIndex: Integer;
    FOnGetFileData: TGetDataFunc;
    FSection0: TMemoryStream;
    FSection1: TStream; // Compressed Stream
    FSection1Size: QWord;
    FSection1ResetTable: TMemoryStream; // has a list of frame positions NOT window positions
    FDirectoryListings: TStream;
    FOutStream: TStream;
    FFileNames: TStrings;
    FDestroyStream: Boolean;
    FTempStream: TStream;
    FPostStream: TStream;
    FWindowSize: LongWord;
    FReadCompressedSize: QWord; // Current Size of Uncompressed data that went in Section1 (compressed)
    FPostStreamActive: Boolean;
    // Linear order of file
    ITSFHeader: TITSFHeader;
    HeaderSection0Table: TITSFHeaderEntry;  // points to HeaderSection0
    HeaderSection1Table: TITSFHeaderEntry; // points to HeaderSection1
    HeaderSuffix: TITSFHeaderSuffix; //contains the offset of CONTENTSection0 from zero
    HeaderSection0: TITSPHeaderPrefix;
    HeaderSection1: TITSPHeader; // DirectoryListings header
    FReadmeMessage : String;
    // DirectoryListings
    // CONTENT Section 0 (section 1 is contained in section 0)
    // EOF
    // end linear header parts
    procedure InitITSFHeader;
    procedure InitHeaderSectionTable;
    procedure SetTempRawStream(const AValue: TStream);
    procedure WriteHeader(Stream: TStream);
    procedure CreateDirectoryListings;
    procedure WriteDirectoryListings(Stream: TStream);
    procedure WriteInternalFilesBefore; virtual;
    procedure WriteInternalFilesAfter; virtual;
    procedure StartCompressingStream;
    procedure WriteREADMEFile;
    procedure WriteFinalCompressedFiles; virtual;
    procedure WriteSection0;
    procedure WriteSection1;
    procedure WriteDataSpaceFiles(const AStream: TStream);

    procedure FileAdded(AStream: TStream; const AEntry: TFileEntryRec); virtual;
    // callbacks for lzxcomp
    function  AtEndOfData: Longbool;
    function  GetData(Count: LongInt; Buffer: PByte): LongInt;
    function  WriteCompressedData(Count: Longint; Buffer: Pointer): LongInt;
    procedure MarkFrame(UnCompressedTotal, CompressedTotal: LongWord);
    // end callbacks
    {$IFDEF LZX_USETHREADS}
    // callbacks for lzx compress threads
    function  LTGetData(Sender: TLZXCompressor; WantedByteCount: Integer; Buffer: Pointer): Integer;
    function  LTIsEndOfFile(Sender: TLZXCompressor): Boolean;
    procedure LTChunkDone(Sender: TLZXCompressor; CompressedSize: Integer; UncompressedSize: Integer; Buffer: Pointer);
    procedure LTMarkFrame(Sender: TLZXCompressor; CompressedTotal: Integer; UncompressedTotal: Integer);
    {$ENDIF}
    // end callbacks
  public
    constructor Create(AOutStream: TStream; FreeStreamOnDestroy: Boolean); virtual;
    destructor Destroy; override;
    procedure Execute;
    procedure AddStreamToArchive(AFileName, APath: String; AStream: TStream; Compress: Boolean = True);
    procedure PostAddStreamToArchive(AFileName, APath: String; AStream: TStream; Compress: Boolean = True);
    property WindowSize: LongWord read FWindowSize write FWindowSize default 2; // in $8000 blocks
    property FrameSize: LongWord read FFrameSize write FFrameSize default 1; // in $8000 blocks
    property FilesToCompress: TStrings read FFileNames;
    property OnGetFileData: TGetDataFunc read FOnGetFileData write FOnGetFileData;
    property OnLastFile: TNotifyEvent read FOnLastFile write FOnLastFile;
    property OutStream: TStream read FOutStream;
    property TempRawStream: TStream read FTempStream write SetTempRawStream;
    property ReadmeMessage : String read fReadmeMessage write fReadmeMessage;
    //property LocaleID: dword read ITSFHeader.LanguageID write ITSFHeader.LanguageID;
  end;

  { TChmWriter }

  TChmWriter = class(TITSFWriter)
  private
    FHasBinaryTOC: Boolean;
    FHasBinaryIndex: Boolean;
    FDefaultFont: String;
    FDefaultPage: String;
    FFullTextSearch: Boolean;
    FFullTextSearchAvailable: Boolean;
    FSearchTitlesOnly: Boolean;
    FStringsStream: TMemoryStream; // the #STRINGS file
    FTopicsStream: TMemoryStream;  // the #TOPICS file
    FURLTBLStream: TMemoryStream;  // the #URLTBL file. has offsets of strings in URLSTR
    FURLSTRStream: TMemoryStream;  // the #URLSTR file
    FFiftiMainStream: TMemoryStream;
    FContextStream: TMemoryStream; // the #IVB file
    FTitle: String;
    FHasTOC: Boolean;
    FHasIndex: Boolean;
    FIndexedFiles: TIndexedWordList;
    FAvlStrings   : TAVLTree;    // dedupe strings
    FAvlURLStr    : TAVLTree;    // dedupe urltbl + binindex must resolve URL to topicid
    SpareString   : TStringIndex;
    SpareUrlStr   : TUrlStrIndex;
    FWindows      : TObjectList;
    FDefaultWindow: String;
    FTocName      : String;
    FIndexName    : String;
  protected
    procedure FileAdded(AStream: TStream; const AEntry: TFileEntryRec); override;
  private
    procedure WriteInternalFilesBefore; override;
    procedure WriteInternalFilesAfter; override;
    procedure WriteFinalCompressedFiles; override;
    procedure WriteSYSTEM;
    procedure WriteITBITS;
    procedure WriteSTRINGS;
    procedure WriteTOPICS;
    procedure WriteIVB; // context ids
    procedure WriteURL_STR_TBL;
    procedure WriteOBJINST;
    procedure WriteFiftiMain;
    procedure WriteWindows;

    function AddString(AString: String): LongWord;
    function AddURL(AURL: String; TopicsIndex: DWord): LongWord;
    procedure CheckFileMakeSearchable(AStream: TStream; AFileEntry: TFileEntryRec);
    function AddTopic(ATitle,AnUrl:AnsiString):integer;
    function NextTopicIndex: Integer;
    procedure Setwindows (AWindowList:TObjectList);

  public
    constructor Create(AOutStream: TStream; FreeStreamOnDestroy: Boolean); override;
    destructor Destroy; override;
    procedure AppendTOC(AStream: TStream);
    procedure AppendBinaryTOCFromSiteMap(ASiteMap: TChmSiteMap);
    procedure AppendBinaryIndexFromSiteMap(ASiteMap: TChmSiteMap;chw:boolean);
    procedure AppendBinaryTOCStream(AStream: TStream);
    procedure AppendBinaryIndexStream(IndexStream,DataStream,MapStream,Propertystream: TStream;chw:boolean);
    procedure AppendIndex(AStream: TStream);
    procedure AppendSearchDB(AName: String; AStream: TStream);
    procedure AddContext(AContext: DWord; ATopic: String);

    property Title: String read FTitle write FTitle;
    property FullTextSearch: Boolean read FFullTextSearch write FFullTextSearch;
    property SearchTitlesOnly: Boolean read FSearchTitlesOnly write FSearchTitlesOnly;
    property HasBinaryTOC: Boolean read FHasBinaryTOC write FHasBinaryTOC;
    property HasBinaryIndex: Boolean read FHasBinaryIndex write FHasBinaryIndex;
    property DefaultFont: String read FDefaultFont write FDefaultFont;
    property DefaultPage: String read FDefaultPage write FDefaultPage;
    property Windows : TObjectlist read fwindows write setwindows;
    property TOCName : String read FTocName write FTocName;
    property IndexName : String read FIndexName write FIndexName;
    property DefaultWindow : string read fdefaultwindow write fdefaultwindow;
  end;

Function CompareStrings(Node1, Node2: Pointer): integer; // also used in filewriter

implementation
uses dateutils, sysutils, paslzxcomp, wikichmFiftiMain;

const
  LZX_WINDOW_SIZE = 16; // 16 = 2 frames = 1 shl 16
  LZX_FRAME_SIZE = $8000;

  {$ifdef binindex}
    procedure logentry(s:string);
    begin
      Writeln(s);
      flush(stdout);
    end;
  {$endif}
{$I wikichmobjinstconst.inc}


Function CompareStrings(Node1, Node2: Pointer): integer;
var n1,n2 : TStringIndex;
begin
  n1:=TStringIndex(Node1); n2:=TStringIndex(Node2);
  Result := CompareText(n1.TheString, n2.TheString);
  if Result < 0 then Result := -1
  else if Result > 0 then Result := 1;
end;


Function CompareUrlStrs(Node1, Node2: Pointer): integer;
var n1,n2 : TUrlStrIndex;
begin
  n1:=TUrlStrIndex(Node1); n2:=TUrlStrIndex(Node2);
  Result := CompareText(n1.UrlStr, n2.UrlStr);
  if Result < 0 then Result := -1
  else if Result > 0 then Result := 1;
end;

{ TChmWriter }

procedure TITSFWriter.InitITSFHeader;
begin
  with ITSFHeader do begin
    ITSFsig := ITSFFileSig;
    Version := NToLE(DWord(3));
    // we fix endian order when this is written to the stream
    HeaderLength := NToLE(DWord(SizeOf(TITSFHeader) + (SizeOf(TGuid)*2)+ (SizeOf(TITSFHeaderEntry)*2) + SizeOf(TITSFHeaderSuffix)));
    Unknown_1 := NToLE(DWord(1));
    TimeStamp:= NToBE(MilliSecondOfTheDay(Now)); //bigendian
    LanguageID := NToLE(DWord($0409)); // English / English_US
  end;
end;

procedure TITSFWriter.InitHeaderSectionTable;
begin
  // header section 0
  HeaderSection0Table.PosFromZero := LEToN(ITSFHeader.HeaderLength);
  HeaderSection0Table.Length := SizeOf(TITSPHeaderPrefix);
  // header section 1
  HeaderSection1Table.PosFromZero := HeaderSection0Table.PosFromZero + HeaderSection0Table.Length;
  HeaderSection1Table.Length := SizeOf(TITSPHeader)+FDirectoryListings.Size;

  //contains the offset of CONTENT Section0 from zero
  HeaderSuffix.Offset := HeaderSection1Table.PosFromZero + HeaderSection1Table.Length;

  // now fix endian stuff
  HeaderSection0Table.PosFromZero := NToLE(HeaderSection0Table.PosFromZero);
  HeaderSection0Table.Length := NToLE(HeaderSection0Table.Length);
  HeaderSection1Table.PosFromZero := NToLE(HeaderSection1Table.PosFromZero);
  HeaderSection1Table.Length := NToLE(HeaderSection1Table.Length);

  with HeaderSection0 do begin // TITSPHeaderPrefix;
    Unknown1 := NToLE(DWord($01FE));
    Unknown2 := 0;
    // at this point we are putting together the headers. content sections 0 and 1 are complete
    FileSize := NToLE(HeaderSuffix.Offset + FSection0.Size + FSection1Size);
    Unknown3 := 0;
    Unknown4 := 0;
  end;
  with HeaderSection1 do begin // TITSPHeader; // DirectoryListings header
    ITSPsig := ITSPHeaderSig;
    Version := NToLE(DWord(1));
    DirHeaderLength := NToLE(DWord(SizeOf(TITSPHeader)));  // Length of the directory header
    Unknown1 := NToLE(DWord($0A));
    ChunkSize := NToLE(DWord($1000));
    Density := NToLE(DWord(2));
    // updated when directory listings were created
    //IndexTreeDepth := 1 ; // 1 if there is no index 2 if there is one level of PMGI chunks. will update as
    //IndexOfRootChunk := -1;// if no root chunk
    //FirstPMGLChunkIndex,
    //LastPMGLChunkIndex: LongWord;

    Unknown2 := NToLE(Longint(-1));
    //DirectoryChunkCount: LongWord;
    LanguageID := NToLE(DWord($0409));
    GUID := ITSPHeaderGUID;
    LengthAgain := NToLE(DWord($54));
    Unknown3 := NToLE(Longint(-1));
    Unknown4 := NToLE(Longint(-1));
    Unknown5 := NToLE(Longint(-1));
  end;

  // more endian stuff
  HeaderSuffix.Offset := NToLE(HeaderSuffix.Offset);
end;

procedure TITSFWriter.SetTempRawStream(const AValue: TStream);
begin
  if (FCurrentStream.Size > 0) or (FSection1.Size > 0) then
    raise Exception.Create('Cannot set the TempRawStream once data has been written to it!');
  if AValue = nil then
    raise Exception.Create('TempRawStream cannot be nil!');
  if FCurrentStream = AValue then
    exit;
  FCurrentStream.Free;
  FCurrentStream := AValue;
end;

procedure TITSFWriter.WriteHeader(Stream: TStream);
begin
  Stream.Write(ITSFHeader, SizeOf(TITSFHeader));

  if ITSFHeader.Version < 4 then
  begin
    Stream.Write(ITSFHeaderGUID, SizeOf(TGuid));
    Stream.Write(ITSFHeaderGUID, SizeOf(TGuid));
  end;
  Stream.Write(HeaderSection0Table, SizeOf(TITSFHeaderEntry));
  Stream.Write(HeaderSection1Table, SizeOf(TITSFHeaderEntry));
  Stream.Write(HeaderSuffix, SizeOf(TITSFHeaderSuffix));
  Stream.Write(HeaderSection0, SizeOf(TITSPHeaderPrefix));

end;

procedure TITSFWriter.CreateDirectoryListings;
type
  TFirstListEntry = record
    Entry: array[0..511] of byte;
    Size: Integer;
  end;
var
  Buffer: array [0..511] of Byte;
  IndexBlock: TPMGIDirectoryChunk;
  ListingBlock: TDirectoryChunk;
  I: Integer;
  Size: Integer;
  FESize: Integer;
  FileName: String;
  FileNameSize: Integer;
  LastListIndex: Integer;
  FirstListEntry: TFirstListEntry;
  ChunkIndex: Integer;
  ListHeader: TPMGListChunk;
const
  PMGL = 'PMGL';
  PMGI = 'PMGI';
  procedure UpdateLastListChunk;
  var
    Tmp: QWord;
  begin
    if ChunkIndex < 1 then begin
      Exit;
    end;
    Tmp := FDirectoryListings.Position;
    FDirectoryListings.Position := (LastListIndex) * $1000;
    FDirectoryListings.Read(ListHeader, SizeOf(TPMGListChunk));
    FDirectoryListings.Position := (LastListIndex) * $1000;
    ListHeader.NextChunkIndex := NToLE(ChunkIndex);
    FDirectoryListings.Write(ListHeader, SizeOf(TPMGListChunk));
    FDirectoryListings.Position := Tmp;
  end;
  procedure WriteIndexChunk(ShouldFinish: Boolean = False);
  var
    IndexHeader: TPMGIIndexChunk;
    ParentIndex,
    TmpIndex: TPMGIDirectoryChunk;
  begin
    with IndexHeader do
    begin
      PMGIsig := PMGI;
      UnusedSpace := NToLE(IndexBlock.FreeSpace);
    end;
    IndexBlock.WriteHeader(@IndexHeader);
    IndexBlock.WriteChunkToStream(FDirectoryListings, ChunkIndex, ShouldFinish);
    IndexBlock.Clear;
    if HeaderSection1.IndexOfRootChunk < 0 then HeaderSection1.IndexOfRootChunk := ChunkIndex;
    if ShouldFinish then
    begin
      HeaderSection1.IndexTreeDepth := 2;
      ParentIndex := IndexBlock.ParentChunk;
      if ParentIndex <> nil then
      repeat // the parent index is notified by our child index when to write
        HeaderSection1.IndexOfRootChunk := ChunkIndex;
        TmpIndex := ParentIndex;
        ParentIndex := ParentIndex.ParentChunk;
        TmpIndex.Free;
        Inc(HeaderSection1.IndexTreeDepth);
        Inc(ChunkIndex);
      until ParentIndex = nil;
    end;
    Inc(ChunkIndex);

  end;
  procedure WriteListChunk;
  begin
    with ListHeader do begin
      PMGLsig := PMGL;
      UnusedSpace := NToLE(ListingBlock.FreeSpace);
      Unknown1 :=  0;
      PreviousChunkIndex := NToLE(LastListIndex);
      NextChunkIndex := NToLE(Longint(-1)); // we update this when we write the next chunk
    end;
    if HeaderSection1.FirstPMGLChunkIndex <= 0 then
      HeaderSection1.FirstPMGLChunkIndex := NToLE(ChunkIndex);
    HeaderSection1.LastPMGLChunkIndex := NToLE(ChunkIndex);
    ListingBlock.WriteHeader(@ListHeader);
    ListingBlock.WriteChunkToStream(FDirectoryListings);
    ListingBlock.Clear;
    UpdateLastListChunk;

    LastListIndex := ChunkIndex;
    Inc(ChunkIndex);
    // now add to index
    if not IndexBlock.CanHold(FirstListEntry.Size) then
      WriteIndexChunk;
    IndexBlock.WriteEntry(FirstListEntry.Size, @FirstListEntry.Entry[0])
  end;
begin
  // first sort the listings
  FInternalFiles.Sort;
  HeaderSection1.IndexTreeDepth := 1;
  HeaderSection1.IndexOfRootChunk := -1;

  ChunkIndex := 0;

  IndexBlock := TPMGIDirectoryChunk.Create(SizeOf(TPMGIIndexChunk));
  ListingBlock := TDirectoryChunk.Create(SizeOf(TPMGListChunk));

  LastListIndex  := -1;

  // add files to a pmgl block until it is full.
  // after the block is full make a pmgi block and add the first entry of the pmgl block
  // repeat until the index block is full and start another.
  // the pmgi chunks take care of needed parent chunks in the tree
  for I := 0 to FInternalFiles.Count-1 do begin
    Size := 0;
    FileName := FInternalFiles.FileEntry[I].Path + FInternalFiles.FileEntry[I].Name;
    FileNameSize := Length(FileName);
    // filename length
    Inc(Size, WriteCompressedInteger(@Buffer[Size], FileNameSize));
    // filename
    Move(FileName[1], Buffer[Size], FileNameSize);
    Inc(Size, FileNameSize);
    FESize := Size;
    // File is compressed...
    Inc(Size, WriteCompressedInteger(@Buffer[Size], Ord(FInternalFiles.FileEntry[I].Compressed)));
    // Offset from section start
    Inc(Size, WriteCompressedInteger(@Buffer[Size], FInternalFiles.FileEntry[I].DecompressedOffset));
    // Size when uncompressed
    Inc(Size, WriteCompressedInteger(@Buffer[Size], FInternalFiles.FileEntry[I].DecompressedSize));

    if not ListingBlock.CanHold(Size) then
      WriteListChunk;

    ListingBlock.WriteEntry(Size, @Buffer[0]);

    if ListingBlock.ItemCount = 1 then begin // add the first list item to the index
      Move(Buffer[0], FirstListEntry.Entry[0], FESize);
      FirstListEntry.Size := FESize + WriteCompressedInteger(@FirstListEntry.Entry[FESize], ChunkIndex);
    end;
  end;
  if ListingBlock.ItemCount > 0 then WriteListChunk;

  if ChunkIndex > 1 then begin
    if (IndexBlock.ItemCount > 1)
    or ( (IndexBlock.ItemCount > 0) and (HeaderSection1.IndexOfRootChunk > -1) )
    then WriteIndexChunk(True);
  end;

  HeaderSection1.DirectoryChunkCount := NToLE(DWord(FDirectoryListings.Size div $1000));

  IndexBlock.Free;
  ListingBlock.Free;

  //now fix some endian stuff
  HeaderSection1.IndexOfRootChunk := NToLE(HeaderSection1.IndexOfRootChunk);
  HeaderSection1.IndexTreeDepth := NtoLE(HeaderSection1.IndexTreeDepth);
end;

procedure TITSFWriter.WriteDirectoryListings(Stream: TStream);
begin
  Stream.Write(HeaderSection1, SizeOf(HeaderSection1));
  FDirectoryListings.Position := 0;
  Stream.CopyFrom(FDirectoryListings, FDirectoryListings.Size);
  FDirectoryListings.Position := 0;
  //TMemoryStream(FDirectoryListings).SaveToFile('dirlistings.pmg');
end;

procedure TITSFWriter.WriteInternalFilesBefore;
begin
  // written to Section0 (uncompressed)
  WriteREADMEFile;
end;

procedure TITSFWriter.WriteInternalFilesAfter;
begin
end;

procedure IterateWord(aword:TIndexedWord;State:pointer);
var i,cnt : integer;
begin
  cnt:=pinteger(state)^;
  for i := 0 to AWord.DocumentCount-1 do
    Inc(cnt, AWord.GetLogicalDocument(i).NumberOfIndexEntries);
          // was commented in original procedure, seems to list index entries per doc.
            //WriteLn(AWord.TheWord,'             documents = ', AWord.DocumentCount, ' h
  pinteger(state)^:=cnt;
end;

procedure TITSFWriter.WriteREADMEFile;
const DISCLAIMER_STR = 'This archive was not made by the MS HTML Help Workshop(r)(tm) program, but by Free Pascal''s chm package '+chmpackageversion+'.'#13#10;
var
  Entry: TFileEntryRec;
begin
  // This procedure puts a file in the archive that says it wasn't compiled with the MS compiler
  Entry.Compressed := False;
  Entry.DecompressedOffset := FSection0.Position;
  FSection0.Write(DISCLAIMER_STR, SizeOf(DISCLAIMER_STR));
  if length(FReadmeMessage)>0 then
    FSection0.Write(FReadmeMessage[1], length(FReadmeMessage));
  Entry.DecompressedSize := FSection0.Position - Entry.DecompressedOffset;
  Entry.Path := '/';
  Entry.Name := '_#_README_#_'; //try to use a name that won't conflict with normal names
  FInternalFiles.AddEntry(Entry);
end;

procedure TITSFWriter.WriteFinalCompressedFiles;
begin

end;


procedure TITSFWriter.WriteSection0;
begin
  FSection0.Position := 0;
  FOutStream.CopyFrom(FSection0, FSection0.Size);
end;

procedure TITSFWriter.WriteSection1;
begin
  WriteContentToStream(FOutStream, FSection1);
end;

procedure TITSFWriter.WriteDataSpaceFiles(const AStream: TStream);
var
  Entry: TFileEntryRec;
begin
  // This procedure will write all files starting with ::
  Entry.Compressed := False; // None of these files are compressed

  //  ::DataSpace/NameList
  Entry.DecompressedOffset := FSection0.Position;
  Entry.DecompressedSize := WriteNameListToStream(FSection0, [snUnCompressed,snMSCompressed]);
  Entry.Path := '::DataSpace/';
  Entry.Name := 'NameList';
  FInternalFiles.AddEntry(Entry, False);

  //  ::DataSpace/Storage/MSCompressed/ControlData
  Entry.DecompressedOffset := FSection0.Position;
  Entry.DecompressedSize := WriteControlDataToStream(FSection0, 2, 2, 1);
  Entry.Path := '::DataSpace/Storage/MSCompressed/';
  Entry.Name := 'ControlData';
  FInternalFiles.AddEntry(Entry, False);

  //  ::DataSpace/Storage/MSCompressed/SpanInfo
  Entry.DecompressedOffset := FSection0.Position;
  Entry.DecompressedSize := WriteSpanInfoToStream(FSection0, FReadCompressedSize);
  Entry.Path := '::DataSpace/Storage/MSCompressed/';
  Entry.Name := 'SpanInfo';
  FInternalFiles.AddEntry(Entry, False);

  //  ::DataSpace/Storage/MSCompressed/Transform/List
  Entry.DecompressedOffset := FSection0.Position;
  Entry.DecompressedSize := WriteTransformListToStream(FSection0);
  Entry.Path := '::DataSpace/Storage/MSCompressed/Transform/';
  Entry.Name := 'List';
  FInternalFiles.AddEntry(Entry, False);

  //  ::DataSpace/Storage/MSCompressed/Transform/{7FC28940-9D31-11D0-9B27-00A0C91E9C7C}/
  //  ::DataSpace/Storage/MSCompressed/Transform/{7FC28940-9D31-11D0-9B27-00A0C91E9C7C}/InstanceData/ResetTable
  Entry.DecompressedOffset := FSection0.Position;
  Entry.DecompressedSize := WriteResetTableToStream(FSection0, FSection1ResetTable);
  Entry.Path := '::DataSpace/Storage/MSCompressed/Transform/{7FC28940-9D31-11D0-9B27-00A0C91E9C7C}/InstanceData/';
  Entry.Name := 'ResetTable';
  FInternalFiles.AddEntry(Entry, True);


  //  ::DataSpace/Storage/MSCompressed/Content do this last
  Entry.DecompressedOffset := FSection0.Position;
  Entry.DecompressedSize := FSection1Size; // we will write it directly to FOutStream later
  Entry.Path := '::DataSpace/Storage/MSCompressed/';
  Entry.Name := 'Content';
  FInternalFiles.AddEntry(Entry, False);


end;

procedure TITSFWriter.FileAdded(AStream: TStream; const AEntry: TFileEntryRec);
begin
  // do nothing here
end;

function _AtEndOfData(arg: pointer): LongBool; cdecl;
begin
  Result := TITSFWriter(arg).AtEndOfData;
end;

function TITSFWriter.AtEndOfData: LongBool;
begin
  Result := ForceExit or (FCurrentIndex >= FFileNames.Count-1);
  if Result then
    Result := Integer(FCurrentStream.Position) >= Integer(FCurrentStream.Size)-1;
end;

function _GetData(arg: pointer; Count: LongInt; Buffer: Pointer): LongInt; cdecl;
begin
  Result := TITSFWriter(arg).GetData(Count, PByte(Buffer));
end;

function TITSFWriter.GetData(Count: LongInt; Buffer: PByte): LongInt;
var
  FileEntry: TFileEntryRec;
begin
  Result := 0;
  while (Result < Count) and (not AtEndOfData) do begin
    Inc(Result, FCurrentStream.Read(Buffer[Result], Count-Result));
    if (Result < Count) and (not AtEndOfData)
    then begin
      // the current file has been read. move to the next file in the list
      FCurrentStream.Position := 0;
      Inc(FCurrentIndex);
      ForceExit := OnGetFileData(FFileNames[FCurrentIndex], FileEntry.Path, FileEntry.Name, FCurrentStream);
      FileEntry.DecompressedSize := FCurrentStream.Size;
      FileEntry.DecompressedOffset := FReadCompressedSize; //269047723;//to test writing really large numbers
      FileEntry.Compressed := True;

      FileAdded(FCurrentStream, FileEntry);

      FInternalFiles.AddEntry(FileEntry);
      // So the next file knows it's offset
      Inc(FReadCompressedSize,  FileEntry.DecompressedSize);
      FCurrentStream.Position := 0;
    end;

    // this is intended for programs to add perhaps a file
    // after all the other files have been added.
    if (AtEndOfData)
    and (FCurrentStream <> FPostStream) then
    begin
      FPostStreamActive := True;
      if Assigned(FOnLastFile) then
        FOnLastFile(Self);
      FCurrentStream.Free;
      WriteFinalCompressedFiles;
      FCurrentStream := FPostStream;
      FCurrentStream.Position := 0;
      Inc(FReadCompressedSize, FCurrentStream.Size);
    end;
  end;
end;

function _WriteCompressedData(arg: pointer; Count: LongInt; Buffer: Pointer): LongInt; cdecl;
begin
  Result := TITSFWriter(arg).WriteCompressedData(Count, Buffer);
end;

function TITSFWriter.WriteCompressedData(Count: Longint; Buffer: Pointer): LongInt;
begin
  // we allocate a MB at a time to limit memory reallocation since this
  // writes usually 2 bytes at a time
  if (FSection1 is TMemoryStream) and (FSection1.Position >= FSection1.Size-1) then begin
    FSection1.Size := FSection1.Size+$100000;
  end;
  Result := FSection1.Write(Buffer^, Count);
  Inc(FSection1Size, Result);
end;

procedure _MarkFrame(arg: pointer; UncompressedTotal, CompressedTotal: LongWord); cdecl;
begin
  TITSFWriter(arg).MarkFrame(UncompressedTotal, CompressedTotal);
end;

procedure TITSFWriter.MarkFrame(UnCompressedTotal, CompressedTotal: LongWord);
  procedure WriteQWord(Value: QWord);
  begin
    FSection1ResetTable.Write(NToLE(Value), 8);
  end;
  procedure IncEntryCount;
  var
    OldPos: QWord;
    Value: DWord;
  begin
    OldPos := FSection1ResetTable.Position;
    FSection1ResetTable.Position := $4;
    Value := LeToN(FSection1ResetTable.ReadDWord)+1;
    FSection1ResetTable.Position := $4;
    FSection1ResetTable.WriteDWord(NToLE(Value));
    FSection1ResetTable.Position := OldPos;
  end;
  procedure UpdateTotalSizes;
  var
    OldPos: QWord;
  begin
    OldPos := FSection1ResetTable.Position;
    FSection1ResetTable.Position := $10;
    WriteQWord(FReadCompressedSize); // size of read data that has been compressed
    WriteQWord(CompressedTotal);
    FSection1ResetTable.Position := OldPos;
  end;
begin
  if FSection1ResetTable.Size = 0 then begin
    // Write the header
    FSection1ResetTable.WriteDWord(NtoLE(DWord(2)));
    FSection1ResetTable.WriteDWord(0); // number of entries. we will correct this with IncEntryCount
    FSection1ResetTable.WriteDWord(NtoLE(DWord(8))); // Size of Entries (qword)
    FSection1ResetTable.WriteDWord(NtoLE(DWord($28))); // Size of this header
    WriteQWord(0); // Total Uncompressed Size
    WriteQWord(0); // Total Compressed Size
    WriteQWord(NtoLE($8000)); // Block Size
    WriteQWord(0); // First Block start
  end;
  IncEntryCount;
  UpdateTotalSizes;
  WriteQWord(CompressedTotal); // Next Block Start
  // We have to trim the last entry off when we are done because there is no next block in that case
end;

{$IFDEF LZX_USETHREADS}
function TITSFWriter.LTGetData(Sender: TLZXCompressor; WantedByteCount: Integer;
  Buffer: Pointer): Integer;
begin
  Result := GetData(WantedByteCount, Buffer);
  //WriteLn('Wanted ', WantedByteCount, ' got ', Result);
end;

function TITSFWriter.LTIsEndOfFile(Sender: TLZXCompressor): Boolean;
begin
  Result := AtEndOfData;
end;

procedure TITSFWriter.LTChunkDone(Sender: TLZXCompressor;
  CompressedSize: Integer; UncompressedSize: Integer; Buffer: Pointer);
begin
  WriteCompressedData(CompressedSize, Buffer);
end;

procedure TITSFWriter.LTMarkFrame(Sender: TLZXCompressor;
  CompressedTotal: Integer; UncompressedTotal: Integer);
begin
  MarkFrame(UncompressedTotal, CompressedTotal);
  //WriteLn('Mark Frame C = ', CompressedTotal, ' U = ', UncompressedTotal);
end;
{$ENDIF}


constructor TITSFWriter.Create(AOutStream: TStream; FreeStreamOnDestroy: Boolean);
begin
  if AOutStream = nil then Raise Exception.Create('TITSFWriter.OutStream Cannot be nil!');
  FOutStream := AOutStream;
  FCurrentIndex := -1;
  FCurrentStream := TMemoryStream.Create;
  FInternalFiles := TFileEntryList.Create;
  FSection0 := TMemoryStream.Create;
  FSection1 := TMemoryStream.Create;
  FSection1ResetTable := TMemoryStream.Create;
  FDirectoryListings := TMemoryStream.Create;
  FPostStream := TMemoryStream.Create;;
  FDestroyStream := FreeStreamOnDestroy;
  FFileNames := TStringList.Create;
end;

destructor TITSFWriter.Destroy;
begin
  if FDestroyStream then FOutStream.Free;
  FInternalFiles.Free;
  FCurrentStream.Free;
  FSection0.Free;
  FSection1.Free;
  FSection1ResetTable.Free;
  FDirectoryListings.Free;
  FFileNames.Free;
  inherited Destroy;
end;

procedure TITSFWriter.Execute;
begin
  writeln('TITSFWriter.Execute START');
  InitITSFHeader;
  FOutStream.Position := 0;
  FSection1Size := 0;

  // write any internal files to FCurrentStream that we want in the compressed section
  WriteInternalFilesBefore;

  writeln('TITSFWriter.Execute AAA1');
  // move back to zero so that we can start reading from zero :)
  FReadCompressedSize := FCurrentStream.Size;
  FCurrentStream.Position := 0;  // when compressing happens, first the FCurrentStream is read
                                 // before loading user files. So we can fill FCurrentStream with
                                 // internal files first.

  // this gathers ALL files that should be in section1 (the compressed section)
  writeln('TITSFWriter.Execute AAA2');
  StartCompressingStream;
  FSection1.Size := FSection1Size;

  writeln('TITSFWriter.Execute AAA3');
  WriteInternalFilesAfter;

  //this creates all special files in the archive that start with ::DataSpace
  writeln('TITSFWriter.Execute AAA4');
  WriteDataSpaceFiles(FSection0);

  // creates all directory listings including header
  writeln('TITSFWriter.Execute AAA5');
  CreateDirectoryListings;

  // do this after we have compressed everything so that we know the values that must be written
  writeln('TITSFWriter.Execute AAA6');
  InitHeaderSectionTable;

  // Now we can write everything to FOutStream
  writeln('TITSFWriter.Execute AAA7');
  WriteHeader(FOutStream);
  writeln('TITSFWriter.Execute AAA8');
  WriteDirectoryListings(FOutStream);
  writeln('TITSFWriter.Execute AAA9');
  WriteSection0; //does NOT include section 1 even though section0.content IS section1
  writeln('TITSFWriter.Execute AAA10');
  WriteSection1; // writes section 1 to FOutStream
  writeln('TITSFWriter.Execute END');
end;

// this procedure is used to manually add files to compress to an internal stream that is
// processed before FileToCompress is called. Files added this way should not be
// duplicated in the FilesToCompress property.
procedure TITSFWriter.AddStreamToArchive(AFileName, APath: String; AStream: TStream; Compress: Boolean = True);
var
  TargetStream: TStream;
  Entry: TFileEntryRec;
begin
  // in case AddStreamToArchive is used after we should be writing to the post stream
  if FPostStreamActive then
  begin
    PostAddStreamToArchive(AFileName, APath, AStream, Compress);
    Exit;
  end;
  if AStream = nil then Exit;
  if Compress then
    TargetStream := FCurrentStream
  else
    TargetStream := FSection0;

  Entry.Name := AFileName;
  Entry.Path := APath;
  Entry.Compressed :=  Compress;
  Entry.DecompressedOffset := TargetStream.Position;
  Entry.DecompressedSize := AStream.Size;
  FileAdded(AStream,Entry);
  FInternalFiles.AddEntry(Entry);
  AStream.Position := 0;
  TargetStream.CopyFrom(AStream, AStream.Size);
end;

procedure TITSFWriter.PostAddStreamToArchive(AFileName, APath: String;
  AStream: TStream; Compress: Boolean);
var
  TargetStream: TStream;
  Entry: TFileEntryRec;
begin
  if AStream = nil then Exit;
  if Compress then
    TargetStream := FPostStream
  else
    TargetStream := FSection0;

  Entry.Name := AFileName;
  Entry.Path := APath;
  Entry.Compressed :=  Compress;
  if not Compress then
    Entry.DecompressedOffset := TargetStream.Position
  else
    Entry.DecompressedOffset := FReadCompressedSize + TargetStream.Position;
  Entry.DecompressedSize := AStream.Size;
  FInternalFiles.AddEntry(Entry);
  AStream.Position := 0;
  TargetStream.CopyFrom(AStream, AStream.Size);
  FileAdded(AStream, Entry);
end;

procedure TITSFWriter.StartCompressingStream;
var
  {$IFNDEF LZX_USETHREADS}
  LZXdata: Plzx_data;
  WSize: LongInt;
  {$ELSE}
  Compressor: TLZXCompressor;
  {$ENDIF}
begin
 {$IFNDEF LZX_USETHREADS}
  lzx_init(@LZXdata, LZX_WINDOW_SIZE, @_GetData, Self, @_AtEndOfData,
              @_WriteCompressedData, Self, @_MarkFrame, Self);

  WSize := 1 shl LZX_WINDOW_SIZE;
  while not AtEndOfData do begin
    lzx_reset(LZXdata);
    lzx_compress_block(LZXdata, WSize, True);
  end;

  //we have to mark the last frame manually
  MarkFrame(LZXdata^.len_uncompressed_input, LZXdata^.len_compressed_output);

  lzx_finish(LZXdata, nil);
  {$ELSE}
  Compressor := TLZXCompressor.Create(4);
  Compressor.OnChunkDone  :=@LTChunkDone;
  Compressor.OnGetData    :=@LTGetData;
  Compressor.OnIsEndOfFile:=@LTIsEndOfFile;
  Compressor.OnMarkFrame  :=@LTMarkFrame;
  Compressor.Execute(True);
  //Sleep(20000);
  Compressor.Free;
  {$ENDIF}
end;


procedure TChmWriter.WriteSystem;
var
  Entry: TFileEntryRec;
  TmpStr: String;
  TmpTitle: String;
const
  VersionStr = 'HHA Version 4.74.8702'; // does this matter?
begin


  // this creates the /#SYSTEM file
  Entry.Name := '#SYSTEM';
  Entry.Path := '/';
  Entry.Compressed := False;
  Entry.DecompressedOffset := FSection0.Position;

 { if FileExists('#SYSTEM') then
  begin
    TmpStream := TMemoryStream.Create;
    TmpStream.LoadFromFile('#SYSTEM');
    TmpStream.Position := 0;
    FSection0.CopyFrom(TmpStream, TmpStream.Size);
  end;                                    }
  // EntryCodeOrder: 10 9 4 2 3 16 6 0 1 5
  FSection0.WriteDWord(NToLE(Word(3))); // Version
  if Title <> '' then
    TmpTitle := Title
  else
    TmpTitle := 'default';

  // Code -> Length -> Data
  // 10
  FSection0.WriteWord(NToLE(Word(10)));
  FSection0.WriteWord(NToLE(Word(SizeOf(DWord))));
  FSection0.WriteDWord(NToLE(MilliSecondOfTheDay(Now)));
  // 9
  FSection0.WriteWord(NToLE(Word(9)));
  FSection0.WriteWord(NToLE(Word(SizeOf(VersionStr)+1)));
  FSection0.Write(VersionStr, SizeOf(VersionStr));
  FSection0.WriteByte(0);
  // 4 A struct that is only needed to set if full text search is on.
  FSection0.WriteWord(NToLE(Word(4)));
  FSection0.WriteWord(NToLE(Word(36))); // size

  FSection0.WriteDWord(NToLE(DWord($0409)));
  FSection0.WriteDWord(1);
  writeln('TChmWriter.WriteSystem FFullTextSearch=',FFullTextSearch,' FFullTextSearchAvailable=',FFullTextSearchAvailable);
  FSection0.WriteDWord(NToLE(DWord(Ord(FFullTextSearch and FFullTextSearchAvailable))));
  FSection0.WriteDWord(0);
  FSection0.WriteDWord(0);

  // two for a QWord
  FSection0.WriteDWord(0);
  FSection0.WriteDWord(0);

  FSection0.WriteDWord(0);
  FSection0.WriteDWord(0);




  ////////////////////////<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  // 2  default page to load
  if FDefaultPage <> '' then begin
    FSection0.WriteWord(NToLE(Word(2)));
    FSection0.WriteWord(NToLE(Word(Length(FDefaultPage)+1)));
    FSection0.Write(FDefaultPage[1], Length(FDefaultPage));
    FSection0.WriteByte(0);
  end;
  // 3  Title
  if FTitle <> '' then begin
    FSection0.WriteWord(NToLE(Word(3)));
    FSection0.WriteWord(NToLE(Word(Length(FTitle)+1)));
    FSection0.Write(FTitle[1], Length(FTitle));
    FSection0.WriteByte(0);
  end;

  // 16 Default Font
  if FDefaultFont <> '' then begin
    FSection0.WriteWord(NToLE(Word(16)));
    FSection0.WriteWord(NToLE(Word(Length(FDefaultFont)+1)));
    FSection0.Write(FDefaultFont[1], Length(FDefaultFont));
    FSection0.WriteByte(0);
  end;

  // 6
  // unneeded. if output file is :  /somepath/OutFile.chm the value here is outfile(lowercase)
  {FSection0.WriteWord(6);
  FSection0.WriteWord(Length('test1')+1);
  Fsection0.Write('test1', 5);
  FSection0.WriteByte(0);}

  // 0 Table of contents filename
  if FHasTOC then begin
    if fTocName ='' then
      TmpStr := DefaultHHC
    else
      TmpStr := fTocName;
    FSection0.WriteWord(0);
    FSection0.WriteWord(NToLE(Word(Length(TmpStr)+1)));
    FSection0.Write(TmpStr[1], Length(TmpStr));
    FSection0.WriteByte(0);
  end;
  // 1
  // hhk Index
  if FHasIndex then begin
    if fIndexName='' then
      TmpStr := DefaultHHK
    else
      TmpStr := fIndexName;
    FSection0.WriteWord(NToLE(Word(1)));
    FSection0.WriteWord(NToLE(Word(Length(TmpStr)+1)));
    FSection0.Write(TmpStr[1], Length(TmpStr));
    FSection0.WriteByte(0);
  end;
  // 5 Default Window

  if FDefaultWindow<>'' then
    begin
      FSection0.WriteWord(NTOLE(Word(5)));
      tmpstr:=FDefaultWindow;
      FSection0.WriteWord(NToLE(Word(Length(TmpStr)+1)));
      FSection0.Write(TmpStr[1], Length(TmpStr));
      FSection0.WriteByte(0);
    end;

  // 7 Binary Index
  if FHasBinaryIndex then
  begin
    {$ifdef binindex}
      logentry('binary index!');
    {$endif}
    FSection0.WriteWord(NToLE(Word(7)));
    FSection0.WriteWord(NToLE(Word(4)));
    FSection0.WriteDWord(DWord(0)); // what is this number to be?
  end;

  // 11 Binary TOC
  if FHasBinaryTOC then
  begin
    FSection0.WriteWord(NToLE(Word(11)));
    FSection0.WriteWord(NToLE(Word(4)));
    FSection0.WriteDWord(DWord(0)); // what is this number to be?
  end;


  Entry.DecompressedSize := FSection0.Position - Entry.DecompressedOffset;
  FInternalFiles.AddEntry(Entry);
end;

procedure TChmWriter.WriteITBITS;
var
  Entry: TFileEntryRec;
begin
  // This is an empty and useless file
  Entry.Name := '#ITBITS';
  Entry.Path := '/';
  Entry.Compressed := False;
  Entry.DecompressedOffset :=0;// FSection0.Position;
  Entry.DecompressedSize := 0;

  FInternalFiles.AddEntry(Entry);
end;

procedure TChmWriter.WriteSTRINGS;
begin
  if FStringsStream.Size = 0 then;
    FStringsStream.WriteByte(0);
  FStringsStream.Position := 0;
  PostAddStreamToArchive('#STRINGS', '/', FStringsStream);
end;

procedure TChmWriter.WriteTOPICS;
//var
  //FHits: Integer;
begin
  if FTopicsStream.Size = 0 then
    Exit;
  FTopicsStream.Position := 0;
  PostAddStreamToArchive('#TOPICS', '/', FTopicsStream);
 // I commented the code below since the result seemed unused
 // FHits:=0;
 //   FIndexedFiles.ForEach(@IterateWord,FHits);
end;

procedure TChmWriter.WriteIVB;
begin
  if FContextStream = nil then exit;

  FContextStream.Position := 0;
  // the size of all the entries
  FContextStream.WriteDWord(NToLE(DWord(FContextStream.Size-SizeOf(dword))));

  FContextStream.Position := 0;
  AddStreamToArchive('#IVB', '/', FContextStream);
end;

procedure TChmWriter.WriteURL_STR_TBL;
begin
  if FURLSTRStream.Size <> 0 then begin
    FURLSTRStream.Position := 0;
    PostAddStreamToArchive('#URLSTR', '/', FURLSTRStream);
  end;
  if FURLTBLStream.Size <> 0 then begin
    FURLTBLStream.Position := 0;
    PostAddStreamToArchive('#URLTBL', '/', FURLTBLStream);
  end;
end;

procedure TChmWriter.WriteOBJINST;
var
  i: Integer;
  ObjStream: TMemoryStream;
  //Flags: Word;
begin
  ObjStream := TMemorystream.Create;
  // this file is needed to enable searches for the ms reader
  ObjStream.WriteDWord(NtoLE($04000000));
  ObjStream.WriteDWord(NtoLE(Dword(2))); // two entries

  ObjStream.WriteDWord(NtoLE(DWord(24))); // offset into file of entry
  ObjStream.WriteDWord(NtoLE(DWord(2691))); // size

  ObjStream.WriteDWord(NtoLE(DWord(2715))); // offset into file of entry
  ObjStream.WriteDWord(NtoLE(DWord(36))); // size

  // first entry
  // write guid 4662DAAF-D393-11D0-9A56-00C04FB68BF7
  ObjStream.WriteDWord(NtoLE($4662DAAF));
  ObjStream.WriteWord(NtoLE($D393));
  ObjStream.WriteWord(NtoLE($11D0));
  ObjStream.WriteWord(NtoLE($569A));
  ObjStream.WriteByte($00);
  ObjStream.WriteByte($C0);
  ObjStream.WriteByte($4F);
  ObjStream.WriteByte($B6);
  ObjStream.WriteByte($8B);
  ObjStream.WriteByte($F7);

  ObjStream.WriteDWord(NtoLE($04000000));
  ObjStream.WriteDWord(NtoLE(11));  // bit flags
  ObjStream.WriteDWord(NtoLE(DWord(1252)));
  ObjStream.WriteDWord(NtoLE(DWord(1033)));
  ObjStream.WriteDWord(NtoLE($00000000));
  ObjStream.WriteDWord(NtoLE($00000000));
  ObjStream.WriteDWord(NtoLE($00145555));
  ObjStream.WriteDWord(NtoLE($00000A0F));
  ObjStream.WriteWord(NtoLE($0100));
  ObjStream.WriteDWord(NtoLE($00030005));
  for i := 0 to 5 do
    ObjStream.WriteDWord($00000000);
  ObjStream.WriteWord($0000);
  // okay now the fun stuff
  for i := 0 to $FF do
  ObjStream.Write(ObjInstEntries[i], SizeOF(TObjInstEntry));
  {begin
    if i = 1 then
      Flags := 7
    else
      Flags := 0;
    if (i >= $41) and (i <= $5A) then
      Flags := Flags or 2;
    if (i >= $61) and (i <= $7A) then
      Flags := Flags or 1;
    if i = $27 then
      Flags := Flags or 6;
    ObjStream.WriteWord(NtoLE(Flags));
    ObjStream.WriteWord(NtoLE(Word(i)));
    if (i >= $41) and (i <= $5A) then
      ObjStream.WriteByte(NtoLE(i+$20))
    else
      ObjStream.WriteByte(NtoLE(i));
    ObjStream.WriteByte(NtoLE(i));
    ObjStream.WriteByte(NtoLE(i));
    ObjStream.WriteByte(NtoLE(i));
    ObjStream.WriteWord(NtoLE($0000));
  end;}
  ObjStream.WriteDWord(NtoLE($E66561C6));
  ObjStream.WriteDWord(NtoLE($73DF6561));
  ObjStream.WriteDWord(NtoLE($656F8C73));
  ObjStream.WriteWord(NtoLE($6F9C));
  ObjStream.WriteByte(NtoLE($65));
  // third bit of second entry
  // write guid 8FA0D5A8-DEDF-11D0-9A61-00C04FB68BF7
  ObjStream.WriteDWord(NtoLE($8FA0D5A8));
  ObjStream.WriteWord(NtoLE($DEDF));
  ObjStream.WriteWord(NtoLE($11D0));
  ObjStream.WriteWord(NtoLE($619A));
  ObjStream.WriteByte($00);
  ObjStream.WriteByte($C0);
  ObjStream.WriteByte($4F);
  ObjStream.WriteByte($B6);
  ObjStream.WriteByte($8B);
  ObjStream.WriteByte($F7);

  ObjStream.WriteDWord(NtoLE($04000000));
  ObjStream.WriteDWord(NtoLE(DWord(1)));
  ObjStream.WriteDWord(NtoLE(DWord(1252)));
  ObjStream.WriteDWord(NtoLE(DWord(1033)));
  ObjStream.WriteDWord(NtoLE(DWord(0)));

  // second entry
  // write guid 4662DAB0-D393-11D0-9A56-00C04FB68B66
  ObjStream.WriteDWord(NtoLE($4662DAB0));
  ObjStream.WriteWord(NtoLE($D393));
  ObjStream.WriteWord(NtoLE($11D0));
  ObjStream.WriteWord(NtoLE($569A));
  ObjStream.WriteByte($00);
  ObjStream.WriteByte($C0);
  ObjStream.WriteByte($4F);
  ObjStream.WriteByte($B6);
  ObjStream.WriteByte($8B);
  ObjStream.WriteByte($66);

  ObjStream.WriteDWord(NtoLE(DWord(666))); // not kidding
  ObjStream.WriteDWord(NtoLE(DWord(1252)));
  ObjStream.WriteDWord(NtoLE(DWord(1033)));
  ObjStream.WriteDWord(NtoLE(DWord(10031)));
  ObjStream.WriteDWord(NtoLE(DWord(0)));

  ObjStream.Position := 0;
  AddStreamToArchive('$OBJINST', '/', ObjStream, True);
  ObjStream.Free;

end;

procedure TChmWriter.WriteFiftiMain;
var
  SearchWriter: TChmSearchWriter;
begin
  if FTopicsStream.Size = 0 then
    Exit;
  SearchWriter := TChmSearchWriter.Create(FFiftiMainStream, FIndexedFiles);
  // do not add an empty $FIftiMain
  writeln('TChmWriter.WriteFiftiMain SearchWriter.HasData=',SearchWriter.HasData);
  if not SearchWriter.HasData then
  begin
    FFullTextSearchAvailable := False;
    SearchWriter.Free;
    Exit;
  end;
  FFullTextSearchAvailable := True;
  SearchWriter.WriteToStream;
  SearchWriter.Free;

  if FFiftiMainStream.Size = 0 then
    Exit;

  FFiftiMainStream.Position := 0;
  PostAddStreamToArchive('$FIftiMain', '/', FFiftiMainStream);
end;

procedure TChmWriter.WriteWindows;
Var WindowStream : TMemoryStream;
    i,j          : Integer;
    win          : TChmWindow;
begin
  if FWindows.Count>0 then
    begin
      WindowStream:=TMemoryStream.Create;
      WindowStream.WriteDword(NToLE(dword(FWindows.Count)));
      WindowStream.WriteDword(NToLE(dword(196))); // 1.1 or later. 188 is old style.
      for i:=0 to FWindows.Count-1 Do
        begin
          Win:=TChmWindow(FWindows[i]);
          WindowStream.WriteDword(NToLE(dword(196 )));                   //  0 size of entry.
          WindowStream.WriteDword(NToLE(dword(0 )));                     //  4 unknown (bool Unicodestrings?)
          WindowStream.WriteDword(NToLE(addstring(win.window_type )));   //  8 Arg 0, name of window
          WindowStream.WriteDword(NToLE(dword(win.flags )));             //  C valid fields
          WindowStream.WriteDword(NToLE(dword(win.nav_style)));          // 10 arg 10 navigation pane style
          WindowStream.WriteDword(NToLE(addstring(win.title_bar_text))); // 14 Arg 1,  title bar text
          WindowStream.WriteDword(NToLE(dword(win.styleflags)));         // 18 Arg 14, style flags
          WindowStream.WriteDword(NToLE(dword(win.xtdstyleflags)));      // 1C Arg 15, xtd style flags
          WindowStream.WriteDword(NToLE(dword(win.left)));               // 20 Arg 13, rect.left
          WindowStream.WriteDword(NToLE(dword(win.top)));                // 24 Arg 13, rect.top
          WindowStream.WriteDword(NToLE(dword(win.right)));              // 28 Arg 13, rect.right
          WindowStream.WriteDword(NToLE(dword(win.bottom)));             // 2C Arg 13, rect.bottom
          WindowStream.WriteDword(NToLE(dword(win.window_show_state)));  // 30 Arg 16, window show state
          WindowStream.WriteDword(NToLE(dword(0)));                      // 34  -    , HWND hwndhelp                OUT: window handle"
          WindowStream.WriteDword(NToLE(dword(0)));                      // 38  -    , HWND hwndcaller              OUT: who called this window"
          WindowStream.WriteDword(NToLE(dword(0)));                      // 3C  -    , HH_INFO_TYPE paINFO_TYPES    IN: Pointer to an array of Information Types"
          WindowStream.WriteDword(NToLE(dword(0)));                      // 40  -    , HWND hwndtoolbar             OUT: toolbar window in tri-pane window"
          WindowStream.WriteDword(NToLE(dword(0)));                      // 44  -    , HWND hwndnavigation          OUT: navigation window in tri-pane window"
          WindowStream.WriteDword(NToLE(dword(0)));                      // 48  -    , HWND hwndhtml                OUT: window displaying HTML in tri-pane window"
          WindowStream.WriteDword(NToLE(dword(win.navpanewidth)));       // 4C Arg 11, width of nav pane
          WindowStream.WriteDword(NToLE(dword(0)));                      // 50  -    , rect.left,   OUT:Specifies the coordinates of the Topic pane
          WindowStream.WriteDword(NToLE(dword(0)));                      // 54  -    , rect.top ,   OUT:Specifies the coordinates of the Topic pane
          WindowStream.WriteDword(NToLE(dword(0)));                      // 58  -    , rect.right,  OUT:Specifies the coordinates of the Topic pane
          WindowStream.WriteDword(NToLE(dword(0)));                      // 5C  -    , rect.bottom, OUT:Specifies the coordinates of the Topic pane
          WindowStream.WriteDword(NToLE(addstring(win.toc_file)));       // 60 Arg 2,  toc file
          WindowStream.WriteDword(NToLE(addstring(win.index_file)));     // 64 Arg 3,  index file
          WindowStream.WriteDword(NToLE(addstring(win.default_file)));   // 68 Arg 4,  default file
          WindowStream.WriteDword(NToLE(addstring(win.home_button_file))); // 6c Arg 5,  home button file.
          WindowStream.WriteDword(NToLE(dword(win.buttons)));            // 70 arg 12,
          WindowStream.WriteDword(NToLE(dword(win.navpane_initially_closed))); // 74 arg 17
          WindowStream.WriteDword(NToLE(dword(win.navpane_default)));    // 78 arg 18,
          WindowStream.WriteDword(NToLE(dword(win.navpane_location)));   // 7C arg 19,
          WindowStream.WriteDword(NToLE(dword(win.wm_notify_id)));       // 80 arg 20,
          for j:=0 to 4 do
            WindowStream.WriteDword(NToLE(dword(0)));                    // 84  -      byte[20] unknown -  "BYTE tabOrder[HH_MAX_TABS + 1]; // IN/OUT: tab order: Contents, Index, Search, History, Favorites, Reserved 1-5, Custom tabs"
          WindowStream.WriteDword(NToLE(dword(0)));                      // 94  -      int cHistory; // IN/OUT: number of history items to keep (default is 30)
          WindowStream.WriteDword(NToLE(addstring(win.Jumpbutton_1_Text)));  // 9C Arg 7,  The text of the Jump 1 button.
          WindowStream.WriteDword(NToLE(addstring(win.Jumpbutton_2_Text)));  // A0 Arg 9,  The text of the Jump 2 button.
          WindowStream.WriteDword(NToLE(addstring(win.Jumpbutton_1_File)));  // A4 Arg 6,  The file shown for Jump 1 button.
          WindowStream.WriteDword(NToLE(addstring(win.Jumpbutton_2_File)));  // A8 Arg 8,  The file shown for Jump 1 button.
          for j:=0 to 3 do
            WindowStream.WriteDword(NToLE(dword(0)));                    // AA  -      byte[16] (TRECT) "RECT rcMinSize; // Minimum size for window (ignored in version 1)"
          //   1.1+ fields
          WindowStream.WriteDword(NToLE(dword(0)));                      // BC -       int cbInfoTypes; // size of paInfoTypes;
          WindowStream.WriteDword(NToLE(dword(0)));                      // C0  -      LPCTSTR pszCustomTabs; // multiple zero-terminated strings
        end;
      WindowStream.Position := 0;
      AddStreamToArchive('#WINDOWS', '/', WindowStream, True);
      WindowStream.Free;
    end;
end;

procedure TChmWriter.WriteInternalFilesAfter;
begin
  // This creates and writes the #ITBITS (empty) file to section0
  WriteITBITS;
  // This creates and writes the #SYSTEM file to section0
  WriteSystem;
end;

procedure TChmWriter.WriteFinalCompressedFiles;
begin
  inherited WriteFinalCompressedFiles;
  WriteTOPICS;
  WriteURL_STR_TBL;
  WriteSTRINGS;
  WriteWINDOWS;
  WriteFiftiMain;
end;

procedure TChmWriter.FileAdded(AStream: TStream; const AEntry: TFileEntryRec);
begin
  inherited FileAdded(AStream, AEntry);
  //writeln('TChmWriter.FileAdded FullTextSearch=',FullTextSearch);
  if FullTextSearch then
    CheckFileMakeSearchable(AStream, AEntry);
end;

procedure TChmWriter.WriteInternalFilesBefore;
begin
  inherited WriteInternalFilesBefore;
  WriteIVB;
  WriteOBJINST;
end;

constructor TChmWriter.Create(AOutStream: TStream; FreeStreamOnDestroy: Boolean);
begin
  inherited Create(AOutStream, FreeStreamOnDestroy);
  FStringsStream := TmemoryStream.Create;
  FTopicsStream := TMemoryStream.Create;
  FURLSTRStream := TMemoryStream.Create;
  FURLTBLStream := TMemoryStream.Create;
  FFiftiMainStream := TMemoryStream.Create;
  FIndexedFiles := TIndexedWordList.Create;
  FAvlStrings   := TAVLTree.Create(@CompareStrings);    // dedupe strings
  FAvlURLStr    := TAVLTree.Create(@CompareUrlStrs);    // dedupe urltbl + binindex must resolve URL to topicid
  SpareString   := TStringIndex.Create;                 // We need an object to search in avltree
  SpareUrlStr   := TUrlStrIndex.Create;                 //    to avoid create/free circles we keep one in spare
                                                        //    for searching purposes
  FWindows      := TObjectlist.Create(True);
  FDefaultWindow:= '';
end;

destructor TChmWriter.Destroy;
begin
  if Assigned(FContextStream) then FContextStream.Free;
  FIndexedFiles.Free;
  FStringsStream.Free;
  FTopicsStream.Free;
  FURLSTRStream.Free;
  FURLTBLStream.Free;
  FFiftiMainStream.Free;
  SpareString.free;
  SpareUrlStr.free;
  FAvlUrlStr.FreeAndClear;
  FAvlUrlStr.Free;
  FAvlStrings.FreeAndClear;
  FAvlStrings.Free;
  FWindows.Free;

  inherited Destroy;
end;


function TChmWriter.AddString(AString: String): LongWord;
var
  NextBlock: DWord;
  Pos: DWord;
  n  : TAVLTreeNode;
  StrRec : TStringIndex;
begin
  // #STRINGS starts with a null char
  if FStringsStream.Size = 0 then FStringsStream.WriteByte(0);

  SpareString.TheString:=AString;
  n:=fAvlStrings.FindKey(SpareString,@CompareStrings);
  if assigned(n) then
   exit(TStringIndex(n.data).strid);

  // each entry is a null terminated string
  Pos := DWord(FStringsStream.Position);

  // Strings are contained in $1000 byte blocks and cannot cross blocks
  NextBlock := ($0000F000 and Pos) + $00001000;
  if Length(AString) + 1 > NextBlock then
  begin
    FStringsStream.Size:= NextBlock;
    FStringsStream.Position := NextBlock;
  end;

  Result := FStringsStream.Position;
  FStringsStream.WriteBuffer(AString[1], Length(AString));
  FStringsStream.WriteByte(0);

  StrRec:=TStringIndex.Create;
  StrRec.TheString:=AString;
  StrRec.Strid    :=Result;
  fAvlStrings.Add(StrRec);
end;

function TChmWriter.AddURL ( AURL: String; TopicsIndex: DWord ) : LongWord;

  procedure CheckURLStrBlockCanHold(Const AString: String);
  var
    Rem: LongWord;
    Len: LongWord;
  begin
    Rem := $4000 - (FURLSTRStream.Size mod $4000);
    Len := 9 + Length(AString);  // 2 dwords the string and NT
    if Rem < Len then
      while Rem > 0 do
      begin
        FURLSTRStream.WriteByte(0);
        Dec(Rem);
      end;
  end;

  function AddURLString(Const AString: String): DWord;
  var urlstrrec : TUrlStrIndex;
  begin
    CheckURLStrBlockCanHold(AString);
    if FURLSTRStream.Size mod $4000 = 0 then
      FURLSTRStream.WriteByte(0);
      Result := FURLSTRStream.Position;
      UrlStrRec:=TUrlStrIndex.Create;
      UrlStrRec.UrlStr:=AString;
      UrlStrRec.UrlStrid:=result;
      FAvlUrlStr.Add(UrlStrRec);
      FURLSTRStream.WriteDWord(NToLE(DWord(0))); // URL Offset for topic after the the "Local" value
      FURLSTRStream.WriteDWord(NToLE(DWord(0))); // Offset of FrameName??
      if Length(AString) > 0 then
        FURLSTRStream.Write(AString[1], Length(AString));
      FURLSTRStream.WriteByte(0); //NT
  end;

  function LookupUrlString(const AUrl : String):DWord;
  var n :TAvlTreeNode;
  begin
    SpareUrlStr.UrlStr:=AUrl;
    n:=FAvlUrlStr.FindKey(SpareUrlStr,@CompareUrlStrs);
    if assigned(n) Then
      result:=TUrlStrIndex(n.data).UrlStrId
    else
      result:=AddUrlString(AUrl);
  end;


var UrlIndex : Integer;

begin
  if (Length(AURL) > 0) and (AURL[1] = '/') then Delete(AURL,1,1);
  UrlIndex:=LookupUrlString(AUrl);

  //if $1000 - (FURLTBLStream.Size mod $1000) = 4 then // we are at 4092
  if FURLTBLStream.Size and $FFC = $FFC then // faster :)
    FURLTBLStream.WriteDWord(0);
  Result := FURLTBLStream.Position;
  FURLTBLStream.WriteDWord(0);//($231e9f5c); //unknown
  FURLTBLStream.WriteDWord(NtoLE(TopicsIndex)); // Index of topic in #TOPICS
  FURLTBLStream.WriteDWord(NtoLE(UrlIndex));
end;



procedure TChmWriter.CheckFileMakeSearchable(AStream: TStream; AFileEntry: TFileEntryRec);

  var
    TopicEntry: TTopicEntry;
    ATitle: String;
begin
  //writeln('TChmWriter.CheckFileMakeSearchable ',AFileEntry.Name);
  if Pos('.ht', AFileEntry.Name) > 0 then
  begin
    //writeln('TChmWriter.CheckFileMakeSearchable indexing...');
    ATitle := FIndexedFiles.IndexFile(AStream, NextTopicIndex, FSearchTitlesOnly);
    if ATitle <> '' then
      TopicEntry.StringsOffset := AddString(ATitle)
    else
      TopicEntry.StringsOffset := $FFFFFFFF;
    TopicEntry.URLTableOffset := AddURL(AFileEntry.Path+AFileEntry.Name, NextTopicIndex);
    TopicEntry.InContents := 2;
    TopicEntry.Unknown := 0;
    TopicEntry.TocOffset := 0;
    FTopicsStream.WriteDWord(LEtoN(TopicEntry.TocOffset));
    FTopicsStream.WriteDWord(LEtoN(TopicEntry.StringsOffset));
    FTopicsStream.WriteDWord(LEtoN(TopicEntry.URLTableOffset));
    FTopicsStream.WriteWord(LEtoN(TopicEntry.InContents));
    FTopicsStream.WriteWord(LEtoN(TopicEntry.Unknown));
  end;
end;

function TChmWriter.AddTopic(ATitle,AnUrl:AnsiString):integer;

var
    TopicEntry: TTopicEntry;

begin
    if ATitle <> '' then
      TopicEntry.StringsOffset := AddString(ATitle)
    else
      TopicEntry.StringsOffset := $FFFFFFFF;
    result:=NextTopicIndex;
    TopicEntry.URLTableOffset := AddURL(AnUrl, Result);
    TopicEntry.InContents := 2;
    TopicEntry.Unknown := 0;
    TopicEntry.TocOffset := 0;
    FTopicsStream.WriteDWord(LEtoN(TopicEntry.TocOffset));
    FTopicsStream.WriteDWord(LEtoN(TopicEntry.StringsOffset));
    FTopicsStream.WriteDWord(LEtoN(TopicEntry.URLTableOffset));
    FTopicsStream.WriteWord(LEtoN(TopicEntry.InContents));
    FTopicsStream.WriteWord(LEtoN(TopicEntry.Unknown));
end;

function TChmWriter.NextTopicIndex: Integer;
begin
  Result := FTopicsStream.Size div 16;
end;

procedure TChmWriter.AppendTOC(AStream: TStream);

var tmpstr : string;
begin
  fHasTOC := True;
  if fTocName = '' then
    tmpstr := defaulthhc
  else
    tmpstr := fTocName;
  PostAddStreamToArchive(tmpstr, '/', AStream, True);
end;

procedure TChmWriter.AppendBinaryTOCFromSiteMap(ASiteMap: TChmSiteMap);
var
  Header: TTOCIdxHeader;
  Entry: TTocEntry;
  EntryInfo: TTOCEntryPageBookInfo;


  EntryInfoStream,
  EntryTopicOffsetStream,
  EntryStream: TMemoryStream;

  TOCIDXStream: TMemoryStream;

  NextLevelItems,
  CurrentLevelItems: TFPList;
  i,j: Integer;
  MenuItem: TChmSiteMapItem;
  MenuItems: TChmSiteMapItems;
  TopicEntry: TTopicEntry;
  EntryCount: DWord = $29A;
  procedure FixParentBookFirstChildOffset(AChildOffset: DWord);
  var
    ParentEntry: TTOCEntryPageBookInfo;
  begin
    // read parent entry
    EntryInfoStream.Position := MenuItems.InternalData;
    EntryInfoStream.Read(ParentEntry, SizeOf(ParentEntry));
    // update child offset
    ParentEntry.FirstChildOffset:= NtoLE(DWord(4096 + AChildOffset));
    // write back to stream
    EntryInfoStream.Position := MenuItems.InternalData;
    EntryInfoStream.Write(ParentEntry, SizeOf(ParentEntry));
    // move to end of stream
    EntryInfoStream.Position := AChildOffset;
  end;

begin
  FillChar(Header, 4096, 0);
  // create streams
  TOCIDXStream := TMemoryStream.Create;
  EntryInfoStream := TMemoryStream.Create;
  EntryTopicOffsetStream := TMemoryStream.Create;
  EntryStream := TMemoryStream.Create;

  NextLevelItems := TFPList.Create;

  NextLevelItems.Add(ASiteMap.Items);

  if NextLevelItems.Count = 0 then
      FreeAndNil(NextLevelItems);

  while NextLevelItems <> nil do
  begin
    CurrentLevelItems := NextLevelItems;
    NextLevelItems := TFPList.Create;

    for i := 0 to CurrentLevelItems.Count-1 do
    begin
      MenuItems := TChmSiteMapItems(CurrentLevelItems.Items[i]);

      for j := 0 to MenuItems.Count-1 do
      begin
        MenuItem := MenuItems.Item[j];
        // first figure out the props
        EntryInfo.Props := 0;
        if MenuItem.Children.Count > 0 then
          EntryInfo.Props := EntryInfo.Props or TOC_ENTRY_HAS_CHILDREN;
        if Length(MenuItem.Local) > 0 then
          EntryInfo.Props := EntryInfo.Props or TOC_ENTRY_HAS_LOCAL;


      if EntryInfo.Props and TOC_ENTRY_HAS_LOCAL > 0 then
      begin
        // Write #TOPICS entry
        TopicEntry.TocOffset      := NtoLE(DWord(4096 + EntryInfoStream.Position));
        TopicEntry.StringsOffset  := NtoLE(AddString(MenuItem.Text));
        TopicEntry.URLTableOffset := NtoLE(AddURL(MenuItem.Local, NextTopicIndex));
        TopicEntry.InContents     := NtoLE(Word( 2 ));
        TopicEntry.Unknown        := 0;
        EntryInfo.TopicsIndexOrStringsOffset := NtoLE(Dword(NextTopicIndex));;
        FTopicsStream.Write(TopicEntry, SizeOf(TopicEntry));
        EntryTopicOffsetStream.WriteDWord(EntryInfo.TopicsIndexOrStringsOffset);

        // write TOCEntry
        Entry.PageBookInfoOffset:= NtoLE(4096 + EntryInfoStream.Position);
        Entry.IncrementedInt  := NtoLE(EntryCount);
        EntryStream.Write(Entry, SizeOf(Entry));
        Inc(EntryCount);

      end
      else
      begin
        EntryInfo.TopicsIndexOrStringsOffset := NtoLE(AddString(MenuItem.Text));
      end;


        // write TOCEntryInfo

        EntryInfo.Unknown1 := 0;
        EntryInfo.EntryIndex := NtoLE(Word(EntryCount - $29A)); //who knows how useful any of this is

        if MenuItems.InternalData <> maxLongint then
          EntryInfo.ParentPageBookInfoOffset := MenuItems.InternalData
        else
          EntryInfo.ParentPageBookInfoOffset := 0;

        if j = MenuItems.Count-1 then
          EntryInfo.NextPageBookOffset := 0
        else if (EntryInfo.Props and TOC_ENTRY_HAS_CHILDREN) > 0 then
          EntryInfo.NextPageBookOffset := 4096 + EntryInfoStream.Position + 28
        else
          EntryInfo.NextPageBookOffset := 4096 + EntryInfoStream.Position + 20;

        // Only if TOC_ENTRY_HAS_CHILDREN is set are these written
        EntryInfo.FirstChildOffset := 0; // we will update this when the child is written
        // in fact lets update the *parent* of this item now if needed
        if (j = 0) and (MenuItems.InternalData <> maxLongint) then
          FixParentBookFirstChildOffset(EntryInfoStream.Position);

        EntryInfo.Unknown3 := 0;

        // fix endian order
        EntryInfo.Props := NtoLE(EntryInfo.Props);
        EntryInfo.ParentPageBookInfoOffset := NtoLE(EntryInfo.ParentPageBookInfoOffset);
        EntryInfo.NextPageBookOffset := NtoLE(EntryInfo.NextPageBookOffset);

        if MenuItem.Children.Count > 0 then
        begin
          NextLevelItems.Add(MenuItem.Children);
          MenuItem.Children.InternalData := EntryInfoStream.Position;
        end;

        // write to stream
        EntryInfoStream.Write(EntryInfo, PageBookInfoRecordSize(@EntryInfo));
      end;
    end;

    FreeAndNil(CurrentLevelItems);
    if NextLevelItems.Count = 0 then
      FreeAndNil(NextLevelItems);
  end;

  // write all streams to TOCIdxStream and free everything
  EntryInfoStream.Position:=0;
  EntryTopicOffsetStream.Position:=0;
  EntryStream.Position:=0;

  Header.BlockSize := NtoLE(DWord(4096));
  Header.EntriesCount := NtoLE(DWord(EntryCount - $29A));
  Header.EntriesOffset := NtoLE(DWord(4096 + EntryInfoStream.Size + EntryTopicOffsetStream.Size));
  Header.TopicsOffset := NtoLE(DWord(4096 + EntryInfoStream.Size));

  TOCIDXStream.Write(Header, SizeOf(Header));

  TOCIDXStream.CopyFrom(EntryInfoStream, EntryInfoStream.Size);
  EntryInfoStream.Free;

  TOCIDXStream.CopyFrom(EntryTopicOffsetStream, EntryTopicOffsetStream.Size);
  EntryTopicOffsetStream.Free;

  TOCIDXStream.CopyFrom(EntryStream, EntryStream.Size);
  EntryStream.Free;

  TOCIDXStream.Position := 0;
  AppendBinaryTOCStream(TOCIDXStream);
  TOCIDXStream.Free;
end;

Const
      BinIndexIdent : array[0..1] of char = (CHR($3B),CHR($29));
      AlwaysX44     : Array[0..15] of char = ('X','4','4',#0,#0,#0,#0,#0,
                                              #0,#0,#0,#0,#0,#0,#0,#0);
      DataEntry     : Array[0..12] of Byte = ($00,$00,$00,$00,$05,$00,$00,$00,$80,$00,$00,$00,$00);
{
  IndexStream:=TMemoryStream.Create;
  IndexStream.Write(BinIndexIdent,2);
  IndexStream.Write(NToLE(word(2)),2);
  IndexStream.Write(NToLE(word(2048)),2);
  IndexStream.Write(AlwaysX44,sizeof(AlwaysX44));
  IndexStrem.Write (dword(0),2);
}

Const DefBlockSize  = 2048;

Type TIndexBlock = Array[0..DefBlockSize-1] of Byte;

procedure writeword(var p:pbyte;w:word); inline;

begin
  pword(p)^:=NToLE(w);
  inc(pword(p));
end;

procedure writedword(var p:pbyte;d:dword); inline;

begin
  pdword(p)^:=NToLE(d);
  inc(pdword(p));
end;

procedure TChmWriter.AppendBinaryIndexFromSiteMap(ASiteMap: TChmSiteMap;chw:boolean);

Var
  IndexStream : TMemoryStream;
  //n           : Integer;
  curblock    : TIndexBlock;    // current listing block being built
  TestBlock   : TIndexBlock;    // each entry is first built here. then moved to curblock
  curind      : integer;        // next byte to write in testblock.
  blocknr     : Integer;        // blocknr of block in testblock;
  lastblock   : Integer;        // blocknr of last block.
  Entries     : Integer;        // Number of entries in this block so far
  TotalEntries: Integer;        // Total number of entries
  MapEntries  : Integer;
  MapIndex    : Integer;
  indexblocknr: Integer;
  blockind    : Integer;        // next byte to write in blockn[blocknr]
  blockentries: Integer;        // entries so far ins blockn[blocknr]
  blockn      : Array Of TIndexBlock;
  BlockNPlus1 : Array of TIndexBlock;
  Mod13value  : integer;        // A value that is increased by 13 for each entry. (?!?!)
  EntryToIndex: boolean;        // helper var to make sure the first block is always indexed.
  blocknplusindex   : Integer;  // blocks in level n+1 (second part)
  blocknplusentries : Integer;  // The other blocks indexed on creation.
  datastream,mapstream,propertystream : TMemoryStream;

procedure preparecurrentblock;

var p: PBTreeBlockHeader;

begin
  p:=@curblock[0];
  p^.Length:=NToLE(Defblocksize-curind);
  p^.NumberOfEntries:=Entries;
  p^.IndexOfPrevBlock:=dword(lastblock);
  p^.IndexOfNextBlock:=Blocknr;
  IndexStream.Write(curblock[0],Defblocksize);
  MapStream.Write(NToLE(MapEntries),sizeof(dword));
  MapStream.Write(NToLE(BlockNr),Sizeof(DWord));
  MapEntries:=TotalEntries;
  curind:=sizeof(TBtreeBlockHeader);   // index into current block;
  lastblock:=blocknr;
  inc(blocknr);
end;

procedure prepareindexblockn(listingblocknr:integer);
var p:PBTreeIndexBlockHeader;
begin
  p:=@Blockn[IndexBlockNr];
  p^.Length:=defblocksize-BlockInd;
  p^.NumberOfEntries:=BlockEntries;

// p^.IndexOfChildBlock  // already entered on block creation, since of first entry, not last.
  inc(Indexblocknr);
  BlockEntries:=0;
  BlockInd:=0;
  if Indexblocknr>=length(blockn) then
    setlength(blockn,length(blockn)+1);  // larger increments also possible. #blocks is kept independantly.
  p:=@Blockn[IndexBlockNr];
  p^.IndexOfChildBlock:=ListingBlockNr;
  blockind:=sizeof(TBTreeIndexBlockHeader);
end;

procedure finalizeindexblockn(p:pbyte;var ind:integer;Entries:integer);
var ph:PBTreeIndexBlockHeader;
begin
  ph:=PBTreeIndexBlockHeader(p);
  ph^.Length:=defblocksize-Ind;
  ph^.NumberOfEntries:=Entries;
// p^.IndexOfChildBlock  // already entered on block creation, since of first entry, not last.
//  inc(Ind);
end;

procedure CurEntryToIndex(entrysize:integer);
var p,pentry : pbyte;
    indexentrysize : integer;
begin
  indexentrysize:=entrysize-sizeof(dword);         // index entry is 4 bytes shorter, and only the last dword differs
  if (blockind+indexentrysize)>=Defblocksize then
    prepareindexblockn(blocknr);
  p:=@blockn[Indexblocknr][blockind];
  move(testblock[0],p^,indexentrysize);
  pentry:=@p[indexentrysize-sizeof(dword)];         // ptr to last dword
  writedword(pentry,blocknr);                      // patch up the "index of child field"
  inc(blockind,indexentrysize);
end;

procedure CreateEntry(Item:TChmSiteMapItem;Str:WideString;commaatposition:integer);

var p      : pbyte;
    topicid: integer;
    seealso: Integer;
    entrysize:Integer;
    i      : Integer;
begin
  inc(TotalEntries);
  p:=@TestBlock[0];
  for i:=1 to Length(str) do
    WriteWord(p,Word(str[i]));   // write the wstr in little endian
  WriteWord(p,0);                // NT
//  if item.seealso='' then    // no seealso for now
    seealso:=0;
 // else
//    seealso:=2;
  WriteWord(p,seealso);          // =0 not a see also 2 =seealso
  WriteWord(p,2);                // Entrydepth.  We can't know it, so write 2.
  WriteDword(p,commaatposition); // position of the comma
  WriteDword(p,0);               // unused 0
  WriteDword(p,1);               // for now only local pair.
  TopicId:=AddTopic(Item.Text,item.Local);
  WriteDword(p,TopicId);
  // if seealso then _here_ a wchar NT string with seealso?
  WriteDword(p,1);               // always 1 (unknown);
  WriteDword(p,mod13value);      //a value that increments with 13.
  mod13value:=mod13value+13;
  entrysize:=p-pbyte(@testblock[0]);
  if (curind+entrysize)>=Defblocksize then
    begin
      preparecurrentblock;
      EntrytoIndex:=true;
    end;
  if EntryToIndex Then
    begin
      CurEntryToIndex(entrysize);
      EntryToIndex:=False;
    end;
  move(testblock[0],curblock[curind],entrysize);
  inc(curind,entrysize);
  datastream.write(DataEntry,Sizeof(DataEntry));
end;

procedure MoveIndexEntry(nr:integer;bytes:integer;childblock:integer);
var
  pscr,pdest : pbyte;
begin
  {$ifdef binindex}
    writeln(' moveindexentry ',nr,' bytes:',bytes,' childblock:',childblock);
    flush(stdout);
  {$endif}

  if ((blockind+bytes)>=defblocksize) then
    begin
      {$ifdef binindex}
      writeln(' in scalecheck  ',blockind);
      flush(stdout);
      {$endif}

      FinalizeIndexBlockn(@blocknplus1[blocknplusindex][0],blockind,blocknplusentries);
      inc(blocknplusindex);
      if blocknplusindex>=length(blocknplus1) then
        setlength(blocknplus1,length(blocknplus1)+1);
      blockInd:=Sizeof(TBTreeIndexBlockHeader);
      pdword(@blocknplus1[blocknplusindex][0])[4]:=NToLE(ChildBlock);  /// init 2nd level index to first 1st level index block
      end;
  {$ifdef binindex}
    writeln(' len:',length(blocknplus1),' blockind:',blockind,' index:',blocknplusindex);
    flush(stdout);
  {$endif}

  // copy entry from one indexblock to another
  pscr:=@blockn[nr][sizeof(TBtreeIndexBlockHeader)];
  pdest:=@blocknplus1[blocknplusindex][blockind];
  move(pscr^,pdest^,bytes);
  pdword(@pdest[bytes-sizeof(dword)])^:=NToLE(childblock);    // correcting the childindex
  inc (blockind,bytes);
  inc(blocknplusentries); // not needed for writing, but used to check if something has been written. End condition
end;

function ScanIndexBlock(blk:Pbyte):Integer;

var start : pbyte;
    n     : Integer;
    i     : Integer;
begin
  start:=@blk[sizeof(TBtreeIndexBlockHeader)];
  blk:=start;
  while pword(blk)^<>0 do   // skip wchar
    inc(pword(blk));
  inc(pword(blk));          // skip NT
  inc(pword(blk));          // skip see also
  inc(pword(blk));          // skip depth
  inc(pdword(blk));         // skip Character Index.
  inc(pdword(blk));          // skip always  0
  n:=LEToN(pdword(blk)^);
  inc(pdword(blk));          // skip nr of pairs.
  for i:= 1 to n do
      inc(pdword(blk));          // skip <n> topicids
  inc(pdword(blk));          // skip childindex
  Result:=blk-start;
end;

procedure CombineWithChildren(ParentItem:TChmSiteMapItem;Str:WideString;commaatposition:integer;first:boolean);
var i    : Integer;
    Item : TChmSiteMapItem;
begin
  if ParentItem.Children.Count = 0 Then
    Begin
     // comment/fix next
     //   if commatposition=length(str) then commaatposition:=0;
       if first then
        CreateEntry(ParentItem,Str,0)
       else
        CreateEntry(ParentItem,Str,commaatposition);
    End
  Else
    for i:=0 to ParentItem.Children.Count-1 do
      begin
        item := TChmSiteMapItem(ParentItem.Children.Item[i]);
        if first Then
          CombineWithChildren(Item,Str+', '+item.text,commaatposition+2,false)
        else
          CombineWithChildren(Item,Str+', '+item.text,commaatposition,false);
      end;
end;

Var i             : Integer;
    Key           : WideString;
    Item          : TChmSiteMapItem;
    ListingBlocks : Integer;
    EntryBytes    : Integer;
    Hdr           : TBTreeHeader;
    TreeDepth     : Integer;

{$ifdef binindex}
procedure printloopvars(i:integer);

begin
  Writeln('location :' ,i, ' blocknr :', blocknr,' level:',TreeDepth);
  Writeln('blockn      length: ',length(blockn),' indexblocknr: ',indexblocknr,' blockind ',blockind);
  Writeln('blocknplus1 length: ',length(blocknplus1),' blocknplusindex:',blocknplusindex,' entries:',blocknplusentries);
  flush(stdout);
end;
{$endif}
begin
  IndexStream:=TMemoryStream.Create;
  indexstream.size:=sizeof(TBTreeHeader);
  IndexStream.position:=Sizeof(TBTreeHeader);
  datastream:=TMemoryStream.Create;
  mapstream :=TMemoryStream.Create;
  mapstream.size:=2;
  mapstream.position:=2;
  propertystream :=TMemoryStream.Create;
  propertystream.write(NToLE(0),sizeof(4));
  // we iterate over all entries and write listingblocks directly to the stream.
  // and the first (and maybe last) level is written to blockn.
  // we can't do higher levels yet because we don't know how many listblocks we get
  BlockNr     :=0;   // current block number
  Lastblock   :=-1;  // previous block nr or -1 if none.
  Entries     :=0;   // entries in this block
  TotalEntries:=0;   // entries so far.
  Mod13value  :=0;   // value that increments by 13 entirely.
  indexblocknr:=0;   // nr of first index block.
  BlockEntries:=0;   // entries into current block;
  MapEntries  :=0;   // entries before the current listing block, for MAP file
  TreeDepth   :=0;

  curind      :=sizeof(TBTreeBlockHeader);      // index into current listing block;
  blockind    :=sizeof(TBtreeIndexBlockHeader); // index into current index block

  Setlength(blockn,1);
  pdword(@blockn[0][4])^:=NToLE(0);  /// init first listingblock nr to 0 in the first index block
  EntryToIndex   := True;
  for i:=0 to ASiteMap.Items.Count-1 do
    begin
      item := TChmSiteMapItem(ASiteMap.Items.Item[i]);
      key  :=Item.Text;
      {$ifdef chm_windowsbinindex}
      // append 2 to all index level 0 entries. This
      // so we can see if Windows loads the binary or textual index.
      CombineWithChildren(Item,Key+'2',length(key)+1,true);
      {$else}
      CombineWithChildren(Item,Key,length(key),true);
      {$endif}
    end;
  PrepareCurrentBlock;     // flush last listing block.
  Listingblocks:=blocknr;   // blocknr is from now on the number of the first block in blockn.
                            // we still need the # of listingblocks for the header though

  {$ifdef binindex}
    writeln('binindex: listingblocks : '+inttostr(listingblocks),' indexblocks: ',indexblocknr,' entries:',blockentries);
  {$endif}

  // we have now created and written the listing blocks, and created the first level of index in <blockn>
  // the following loop uses <blockn> to calculate the next level (in blocknplus1), then write out blockn,
  // and repeat until we have no entries left.

  // First we finalize the current set of blocks

  if  Blockind<>sizeof(TBtreeIndexBlockHeader) Then
    begin
      {$ifdef binindex}
        writeln('finalizing level 1 index');
      {$endif}
      FinalizeIndexBlockN(@blockn[indexblocknr][0],blockind,blockentries); // also increasing indexblocknr
      inc(IndexBlockNr);
    end;
  {$ifdef binindex}
    writeln('binindex: listingblocks : '+inttostr(listingblocks),' indexblocks: ',indexblocknr,' entries:',blockentries);
  {$endif}


  while (Indexblocknr>1) do
    begin
      {$ifdef binindex}
        printloopvars(1);
      {$endif}

      blockind      :=sizeof(TBtreeIndexBlockHeader);
      pdword(@blockn[0][4])^:=NToLE(Listingblocks);  /// init 2nd level index to first 1st level index block
      blocknplusindex     :=0;
      blocknplusentries   :=0;
      if length(blocknplus1)<1 then
        Setlength(blocknplus1,1);

      EntryToIndex        :=True;
      {$ifdef binindex}
        printloopvars(2);
      {$endif}
      for i:=0 to Indexblocknr-1 do
        begin
          Entrybytes:=ScanIndexBlock(@blockn[i][0]);
//          writeln('after scan ,',i, ' bytes: ',entrybytes,' blocknr:',blocknr,' indexblocknr:',indexblocknr,' to:',blocknr+i);
          MoveIndexEntry(i,Entrybytes,blocknr+i);
          indexStream.Write(blockn[i][0],defblocksize);
        end;

      {$ifdef binindex}
        printloopvars(3);
      {$endif}

      If Blockind<>sizeof(TBtreeIndexBlockHeader) Then
        begin
          {$ifdef binindex}
            logentry('finalizing');
          {$endif}
          FinalizeIndexBlockn(@blocknplus1[blocknplusindex][0],blockind,blocknplusentries);
          inc(blocknplusindex);
        end;

      inc(blocknr,indexblocknr);

      indexblocknr:=blocknplusindex;
      blockn:=copy(blocknplus1); setlength(blocknplus1,1);
      {$ifdef binindex}
        printloopvars(5);
      {$endif}

      inc(TreeDepth);
    end;
  indexStream.Write(blockn[0][0],defblocksize);
  inc(blocknr);
  // Fixup header.
  hdr.ident[0]:=chr($3B); hdr.ident[1]:=chr($29);
  hdr.flags          :=NToLE(word($2));           // bit $2 is always 1, bit $0400 1 if dir? (always on)
  hdr.blocksize      :=NToLE(word(defblocksize)); // size of blocks (2048)
  hdr.dataformat     :=AlwaysX44;           // "X44" always the same, see specs.
  hdr.unknown0       :=NToLE(0);            // always 0
  hdr.lastlstblock   :=NToLE(dword(ListingBlocks-1)); // index of last listing block in the file;
  hdr.indexrootblock :=NToLE(dword(blocknr-1));    // Index of the root block in the file.
  hdr.unknown1       :=NToLE(dword(-1));           // always -1
  hdr.nrblock        :=NToLE(blocknr);      // Number of blocks
  hdr.treedepth      :=NToLE(word(TreeDepth));    // The depth of the tree of blocks (1 if no index blocks, 2 one level of index blocks, ...)
  hdr.nrkeywords     :=NToLE(Totalentries); // number of keywords in the file.
  hdr.codepage       :=NToLE(dword(1252));         // Windows code page identifier (usually 1252 - Windows 3.1 US (ANSI))
  hdr.lcid           :=NToLE(0);            //  ???? LCID from the HHP file.
  if not chw then
    hdr.ischm        :=NToLE(dword(1))             // 0 if this a BTREE and is part of a CHW file, 1 if it is a BTree and is part of a CHI or CHM file
  else
    hdr.ischm        :=NToLE(0);
  hdr.unknown2       :=NToLE(dword(10031));        // Unknown. Almost always 10031. Also 66631 (accessib.chm, ieeula.chm, iesupp.chm, iexplore.chm, msoe.chm, mstask.chm, ratings.chm, wab.chm).
  hdr.unknown3       :=NToLE(0);            // unknown 0
  hdr.unknown4       :=NToLE(0);            // unknown 0
  hdr.unknown5       :=NToLE(0);            // unknown 0

  IndexStream.Position:=0;
  IndexStream.write(hdr,sizeof(hdr));
  {$ifdef binindex}
    logentry('before append');
  {$endif}

  AppendBinaryIndexStream(IndexStream,datastream,MapStream,PropertyStream,chw);
  IndexStream.Free;
  PropertyStream.Free;
  MapStream.Free;
  DataStream.Free;
end;

procedure TChmWriter.AppendBinaryTOCStream(AStream: TStream);
begin
  AddStreamToArchive('#TOCIDX', '/', AStream, True);
end;

procedure TChmWriter.AppendBinaryIndexStream(IndexStream,DataStream,MapStream,Propertystream: TStream;chw:boolean);

procedure stadd(fn:string;stream:TStream);

begin
  Stream.Position:=0;
  if CHW then
    fn:=uppercase(fn);
  {$ifdef binindex}
    logentry('before append '+fn);
  {$endif}
  AddStreamToArchive(fn,'/$WWKeywordLinks/',stream,True);
end;

begin
  stadd('BTree',IndexStream);
  stadd('Data', DataStream);
  stadd('Map' , MapStream);
  stadd('Property', PropertyStream);
end;

procedure TChmWriter.AppendIndex(AStream: TStream);
var tmpstr : string;
begin
  FHasIndex := True;
  if fIndexName = '' then
    tmpstr:=defaulthhk
  else
    tmpstr:=fIndexName;
  PostAddStreamToArchive(tmpstr, '/', AStream, True);
end;

procedure TChmWriter.AppendSearchDB(AName: String; AStream: TStream);
begin
  PostAddStreamToArchive(AName, '/', AStream);
end;


procedure TChmWriter.AddContext(AContext: DWord; ATopic: String);
var
  Offset: DWord;
begin
  if FContextStream = nil then begin
    FContextStream:=TMemoryStream.Create;
    // #IVB starts with a dword which is the size of the stream - sizeof(dword)
    FContextStream.WriteDWord(0);
    // we will update this when we write the file to the final stream
  end;
  // an entry is a context id and then the offset of the name of the topic in the strings file
  FContextStream.WriteDWord(NToLE(AContext));
  Offset := NToLE(AddString(ATopic));
  FContextStream.WriteDWord(Offset);
end;

procedure TChmWriter.SetWindows(AWindowList:TObjectList);

var i : integer;
    x : TCHMWindow;
begin
  FWindows.Clear;
  for i:=0 to AWindowList.count -1 do
    begin
      x:=TChmWindow.Create;
      x.assign(TChmWindow(AWindowList[i]));
      Fwindows.Add(x);
    end;
end;


end.

