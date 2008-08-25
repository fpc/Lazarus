{******************************************************************}
{*     IPSTRMS.PAS - Various stream classes                       *}
{******************************************************************}

{ $Id$ }

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower Internet Professional
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * Markus Kaemmerer <mk@happyarts.de> SourceForge: mkaemmerer
 *
 * ***** END LICENSE BLOCK ***** *)

{ Global defines potentially affecting this unit }

{$I IPDEFINE.INC}

unit IpStrms;
  {- Ansi text stream class}

interface

uses
  {$IFDEF IP_LAZARUS}
  FPCAdds,
  LCLType,
  GraphType,
  LCLIntf,
  FileUtil,
  {$ELSE}
  Windows,
  {$ENDIF}
  SysUtils,
  Classes,
  IpUtils,
  IpConst;

{ TIpMemMapStream  }
type
  TIpMemMapStream = class(TStream)
  protected
    FCanGrow : Boolean;
      { If True then the map file can grow if the user writes past the
        current end of the stream. Note that growing may be expensive
        time-wise. }
    FDataSize : Longint;
      { The amount of data actually written to the stream. }
    FGrowthFactor : Double;
      { The factor by which the map file is to be grow each time a size
        increase is needed. }
    FReadOnly : Boolean;
      { If set to True then file is to be opened for read-only access. }
    FSize : Longint;
      { The current size of the mapped file. When creating files, the size
        must be pre-set. The size is fixed unless the CanGrow property is
        set to True. }
    mmFileExists : Boolean;
      { Set to True if the file existed when the open method was called. }
    mmFileHandle : THandle;
    mmFileIsTemp : Boolean;
      { If set to True then file was created by this stream. }
    mmFileName : string;
    mmMapHandle : THandle;
    mmPointer : Pointer;
      { Pointer to the beginning of the file. }
    mmPos : Longint;
      { Current position in the file. }

    { Verification methods }
    procedure CheckClosed(const aMethodName : string);
    procedure CheckFileName;

    procedure CloseFile;
    procedure CloseMap;

    procedure OpenFile;
    procedure OpenMap;

    procedure Resize(const NewSize : Longint);
    procedure SetSize(NewSize : Longint); override;
  public
    constructor Create(const FileName : string;
                       const ReadOnly, Temporary : Boolean);
    destructor Destroy; override;
    procedure Open;
      { After the stream has been created, call this method to open the file. }
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;

    property ReadOnly : Boolean read FReadOnly;
      { Returns True if the file is being opened in read-only mode. }

    property CanGrow : Boolean read FCanGrow write FCanGrow;
      { If True then the mapped stream can grow in size when data is written
        past the current end of the stream. Note this involves closing &
        reopening the map which may be expensive time-wise.
        Defaults to True. }

    property DataSize : Longint read FDataSize;
      { The amount of data actually written to the stream. It is calculated
        based upon the highest position to which data was written.
        For example, if an app seeks to position 100 and writes 100 bytes
        of data then the data size is 201. }

    property GrowthFactor : Double read FGrowthFactor write FGrowthFactor;
      { The factor by which the stream will be grown in size if CanGrow is
        True and data is written past the current end of stream.
        Defaults to 0.25. }

    property Memory: Pointer read mmPointer;
      { Points to the memory associated with the file. }

    property Size : Longint read FSize write SetSize;
      { For temporary files, specify the maximum size of the file via this
        property. }
  end;

{ TIpBufferedStream }
type
  TIpBufferedStream = class(TStream)
    private {- property variables }
      FBufCount: Longint;
      FBuffer  : PAnsiChar;
      FBufOfs  : Longint;
      FBufPos  : Longint;
      FBufSize : Longint;
      FDirty   : Boolean;
      FSize    : {$IFDEF IP_LAZARUS}TStreamSeekType{$ELSE}longint{$ENDIF};
      FStream  : TStream;

    protected {- methods }
      procedure bsInitForNewStream; virtual;
      procedure bsReadFromStream;
      procedure bsSetStream(aValue : TStream);
      procedure bsWriteToStream;

    public {- methods }
      constructor Create(aStream : TStream);
      constructor CreateEmpty;
      destructor  Destroy; override;

      procedure Flush;                                                 {!!.12}
        { Flush any unwritten changes to the stream. }
      procedure FreeStream;
      function ReadChar(var aCh : AnsiChar) : Boolean;
      function Read(var Buffer; Count : Longint) : Longint; override;
      function Seek(Offset : Longint; Origin : word) : Longint; override;
      function Write(const Buffer; Count : Longint) : Longint; override;

    public {-properties }
      property FastSize: {$IFDEF IP_LAZARUS}TStreamSeekType{$ELSE}longint{$ENDIF}
        read FSize;
      property Stream : TStream
        read FStream write bsSetStream;
   end;


{ TIpAnsiTextStream }
type
  TIpAnsiTextStream = class(TIpBufferedStream)
    private {- property variables }
      FLineEndCh   : AnsiChar;
      FLineLen     : Integer;
      FLineTerm    : TIpLineTerminator;
      FFixedLine   : PAnsiChar;
      FLineCount   : Longint;
      FLineCurrent : Longint;
      FLineCurOfs  : Longint;
      FLineIndex   : TList;
      FLineInxStep : Longint;
      FLineInxTop  : Integer;

    protected {- methods }
      procedure atsGetLine(var aStartPos, aEndPos, aLen : Longint);
      function  atsGetLineCount : Longint;
      procedure atsResetLineIndex;
      procedure atsSetLineTerm(aValue : TIpLineTerminator);
      procedure atsSetLineEndCh(aValue : AnsiChar);
      procedure atsSetLineLen(aValue : Integer);

    public {- properties }
      property FixedLineLength : Integer
        read FLineLen write atsSetLineLen;
      property LineCount : Longint
        read atsGetLineCount;
      property LineTermChar : AnsiChar
        read FLineEndCh write atsSetLineEndCh;
      property LineTerminator : TIpLineTerminator
        read FLineTerm write atsSetLineTerm;

    public {- methods }
      constructor Create(aStream : TStream);
      destructor Destroy; override;

      function  AtEndOfStream : Boolean;
      procedure bsInitForNewStream; override;                        {!!.01}
      function  ReadLine : string;
      function  ReadLineArray(aCharArray : PAnsiChar; aLen : Longint) : Longint;
      function  ReadLineZ(aSt : PAnsiChar; aMaxLen : Longint) : PAnsiChar;
      function  SeekNearestLine(aOffset : Longint) : Longint;
      function  SeekLine(aLineNum : Longint) : Longint;
      procedure WriteLine(const aSt : string);
      procedure WriteLineArray(aCharArray : PAnsiChar; aLen : Longint);
      procedure WriteLineZ(aSt : PAnsiChar);
  end;

{ TIpDownloadFileStream }
type
  TIpDownloadFileStream = class(TStream)
    private
      FHandle   : THandle;
      FPath     : string;
      FFileName : string;
      FRenamed  : boolean;
    protected
      procedure dfsMakeTempFile(const aPath : string);         
    public
      constructor Create(const aPath : string);
      destructor Destroy; override;

      function Read(var Buffer; Count : Longint) : Longint; override;
      procedure Rename(aNewName : string);
      procedure Move(aNewName: string);
      function Seek(Offset : Longint; Origin : Word) : Longint; override;
      function Write(const Buffer; Count : Longint) : Longint; override;

      property Handle : THandle read FHandle;
      property FileName : string read FFileName;
  end;


{ TIpByteStream }
type
  TIpByteStream = class
    private {variables}
      FStream  : TStream;
      BufEnd   : Integer;
      BufPos   : Integer;
      Buffer   : array[0..1023] of Byte;
    protected {methods}
      function GetPosition : Integer;
      function GetSize : Integer;
    public {methods}
      constructor Create(aStream : TStream);
      destructor Destroy; override;
      function Read(var b :Byte) : Boolean;
    public {properties}
      property Position : Integer
        read GetPosition;
      property Size : longint
        read GetSize;
  end;




implementation

const
  LineTerm : array [TIpLineTerminator] of
               array [0..1] of AnsiChar =
                 ('', #13, #10, #13#10, '');

const
  LineIndexCount = 1024;
  LineIndexMax   = pred(LineIndexCount);


{--- Helper routines ---------------------------------------------------------}

function MinLong(A, B : Longint) : Longint;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

{-----------------------------------------------------------------------------}
{                          TIpMemMapStream                                    }
{-----------------------------------------------------------------------------}

constructor TIpMemMapStream.Create(const FileName : string;
                                   const ReadOnly, Temporary : Boolean);
begin
  inherited Create;

  FCanGrow := True;
  FDataSize := 0;
  FGrowthFactor := 0.25;
  FReadOnly := ReadOnly;
  FSize := 64 * 1024;
  mmFileName := FileName;
  mmFileIsTemp := Temporary;
end;

{-----------------------------------------------------------------------------}

destructor TIpMemMapStream.Destroy;
begin
  CloseMap;
  CloseFile;
  
  { If map file was temporary then get rid of it. }
  if mmFileIsTemp and FileExistsUTF8(mmFileName) then
    DeleteFileUTF8(mmFileName);

  inherited;
end;

{-----------------------------------------------------------------------------}

procedure TIpMemMapStream.CheckClosed(const aMethodName : string);
begin
  if mmFileHandle <> 0 then
    raise EIpBaseException.CreateFmt(SMemMapMustBeClosed, [aMethodName]);
end;

{-----------------------------------------------------------------------------}

procedure TIpMemMapStream.CheckFileName;
begin
  if mmFileName = '' then
    raise EIpBaseException.Create(SMemMapFilenameRequired);
end;

{-----------------------------------------------------------------------------}

procedure TIpMemMapStream.CloseFile;
begin
  {$IFDEF IP_LAZARUS}
  writeln('TIpMemMapStream.CloseFile ToDo');
  {$ELSE}
  if mmFileHandle <> 0 then
    CloseHandle(mmFileHandle);
  {$ENDIF}
end;

{-----------------------------------------------------------------------------}

procedure TIpMemMapStream.CloseMap;
begin
  {$IFDEF IP_LAZARUS}
  writeln('TIpMemMapStream.CloseMap ToDo');
  {$ELSE}
  FlushViewOfFile(mmPointer, 0);
  UnMapViewOfFile(mmPointer);
  if mmMapHandle <> 0 then
    CloseHandle(mmMapHandle);
  {$ENDIF}
end;

{-----------------------------------------------------------------------------}

procedure TIpMemMapStream.Open;
begin
  OpenFile;
  OpenMap;
end;

{-----------------------------------------------------------------------------}

procedure TIpMemMapStream.OpenFile;
{$IFDEF IP_LAZARUS}
begin
  writeln('TIpMemMapStream.OpenFile ToDo');
end;
{$ELSE}
var
  CreateMode,
  Flags,
  OpenMode : DWORD;
begin

  { Check requirements. }
  CheckFileName;
  CheckClosed('Open');

  { Are we opening an existing file or creating a new file? }
  if not FileExistsUTF8(mmFileName) then
    CreateMode:= CREATE_ALWAYS
  else
    CreateMode := OPEN_EXISTING;

  OpenMode := GENERIC_READ;
  if FReadOnly then
    Flags := FILE_ATTRIBUTE_NORMAL or FILE_FLAG_SEQUENTIAL_SCAN
  else begin
    OpenMode := OpenMode or GENERIC_WRITE;
    Flags := FILE_ATTRIBUTE_NORMAL or FILE_FLAG_RANDOM_ACCESS;
  end;

  mmFileExists := (CreateMode = OPEN_EXISTING);

  mmFileHandle := CreateFile(PChar(mmFileName),
                             OpenMode,
                             0,  { exclusive }
                             nil,
                             CreateMode,
                             Flags,
                             0);

  if mmFileHandle = INVALID_HANDLE_VALUE then
    { Raise exception. }
    raise EIpBaseException.Create(SysErrorMessage(GetLastError) + SFilename +
                                  mmFileName);
end;
{$ENDIF}

{-----------------------------------------------------------------------------}

procedure TIpMemMapStream.OpenMap;
{$IFDEF IP_LAZARUS}
begin
  writeln('TIpMemMapStream.OpenMap ToDo');
end;
{$ELSE}
var
  AccessMode,
  ProtectMode,
  SizeHigh : DWORD;
  Size : DWORD;
begin
  { If this was an existing file then get the size of the file. }
  if mmFileExists then begin
    SizeHigh := 0;
    Size := GetFileSize(mmFileHandle, @SizeHigh);
    FSize := Size;
    FDataSize := Size;
    if Size = $FFFFFFFF then
      { Raise exception. }
      raise EIpBaseException.Create(SysErrorMessage(GetLastError) + SFilename +
                                    mmFileName);
  end
  else
    Size := FSize;

  { Read-only? }
  if FReadOnly then begin
    AccessMode := FILE_MAP_READ;
    ProtectMode := PAGE_READONLY;
  end
  else begin
    AccessMode := FILE_MAP_ALL_ACCESS;
    ProtectMode := PAGE_READWRITE;
  end;

  mmMapHandle := CreateFileMapping(mmFileHandle, nil, ProtectMode,
                                   0, Size, nil);
  if mmMapHandle = 0 then
    { Raise exception. }
    raise EIpBaseException.Create(SysErrorMessage(GetLastError) + SFilename +
                                  mmFileName);

  mmPointer := MapViewOfFile(mmMapHandle, AccessMode, 0, 0, Size);
  if mmPointer = nil then
    raise EIpBaseException.Create(SysErrorMessage(GetLastError) + SFilename +
                                  mmFileName);
  mmPos := 0;
end;
{$ENDIF}

{-----------------------------------------------------------------------------}

procedure TIpMemMapStream.Resize(const NewSize : Longint);
var
  SavPos : Longint;
begin
  { Close the map. }
  if NewSize < FSize then
    SavPos := 0
  else
    SavPos := mmPos;
  CloseMap;

  {$IFDEF IP_LAZARUS}
  writeln('TIpMemMapStream.Resize ToDo');
  {$ELSE}
  { Update the size of the file. }
  if SetFilePointer(mmFileHandle, NewSize, nil, FILE_BEGIN) <> $FFFFFFFF then begin
    if SetEndOfFile(mmFileHandle) = false then
      raise EIpBaseException.Create(SysErrorMessage(GetLastError) + SFilename +
                                    mmFileName);
  end
  else
    raise EIpBaseException.Create(SysErrorMessage(GetLastError) + SFilename +
                                  mmFileName);
  {$ENDIF}

  { Update internal size information. }
  FSize := NewSize;
  if FSize < FDataSize then
    FDataSize := FSize;

  { Re-open the map. }
  mmFileExists := True;
  OpenMap;
  mmPos := SavPos;
end;

{-----------------------------------------------------------------------------}

procedure TIpMemMapStream.SetSize(NewSize : Longint);
begin
  if mmFileHandle <> 0 then
    Resize(NewSize);
  FSize := NewSize;
end;

{-----------------------------------------------------------------------------}

function TIpMemMapStream.Read(var Buffer; Count: Longint): Longint;
begin
  if mmFileHandle = 0 then
    raise EIpBaseException.CreateFmt(SMemMapMustBeOpen, ['Read']);
  if (mmPos + Count) > FDataSize then
    Result := FDataSize - mmPos
  else
    Result := Count;
  Move(PByteArray(mmPointer)[mmPos], Buffer, Result);
  inc(mmPos, Result);
end;

{-----------------------------------------------------------------------------}

function TIpMemMapStream.Write(const Buffer; Count: Longint): Longint;
var
  NewSize : Longint;
begin
  if mmFileHandle = 0 then
    raise EIpBaseException.CreateFmt(SMemMapMustBeOpen, ['Write']);
  if not FReadOnly then begin
    if (mmPos + Count) > FSize then begin
      if FCanGrow then begin
        { Grow the stream. }
        NewSize := FSize + Trunc(FSize * FGrowthFactor);
        if NewSize < FSize + Count then
          NewSize := FSize + Count;
        Resize(NewSize);
        Result := Count;
      end
      else
        Result := FSize - mmPos;
    end
    else
      Result := Count;

    Move(Buffer, PByteArray(mmPointer)[mmPos], Result);
    inc(mmPos, Result);
    if mmPos > FDataSize then
      FDataSize := mmPos + 1;
  end
  else
    Result := 0;
end;

{-----------------------------------------------------------------------------}

function TIpMemMapStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  if mmFileHandle = 0 then
    raise EIpBaseException.CreateFmt(SMemMapMustBeOpen, ['Seek']);
  case Origin of
    soFromBeginning :
      if Offset < 0 then
        raise EIpBaseException.Create(SOriginFromBegin)
      else
        mmPos := Offset;

    soFromCurrent   :
      mmPos := mmPos + Offset;

    soFromEnd       :
      if Offset > 0 then
        raise EIpBaseException.Create(SOriginFromEnd)
      else
        mmPos := FSize + Offset;
  end;  { case }
  Result := mmPos;
end;

{-----------------------------------------------------------------------------}
{                          TIpBufferedStream                                  }
{-----------------------------------------------------------------------------}

const
  BufferSize = 16384; // higher values for more speed but more memory

constructor TIpBufferedStream.Create(aStream : TStream);
begin
  inherited Create;

  {allocate the buffer}
  FBufSize := BufferSize;

  GetMem(FBuffer, FBufSize);

  {save the stream}
  if (aStream = nil) then
    raise EIpBaseException.Create(SNoStreamErr);
  FStream := aStream;

  bsInitForNewStream;
end;

{-----------------------------------------------------------------------------}

constructor TIpBufferedStream.CreateEmpty;
begin
  inherited Create;

  {allocate the buffer}
  FBufSize := BufferSize;
  GetMem(FBuffer, FBufSize);
  bsInitForNewStream
end;

{-----------------------------------------------------------------------------}

destructor TIpBufferedStream.Destroy;
begin
  if (FBuffer <> nil) and (FStream <> nil) then
    if FDirty then
      bsWriteToStream;
  FreeMem(FBuffer, FBufSize);

  inherited Destroy;
end;

{-----------------------------------------------------------------------------}

procedure TIpBufferedStream.bsInitForNewStream;
begin
  if (FStream <> nil) then
    FSize := FStream.Size
  else
    FSize := 0;
  FBufCount := 0;
  FBufOfs := 0;
  FBufPos := 0;
  FDirty := false;
end;

{-----------------------------------------------------------------------------}

function TIpBufferedStream.ReadChar(var aCh : AnsiChar) : Boolean;
begin
  {is there anything to read?}
  if (FSize = (FBufOfs + FBufPos)) then begin
    Result := false;
    Exit;
  end;
  {if we get here, we'll definitely read a character}
  Result := true;
  {make sure that the buffer has some data in it}
  if (FBufCount = 0) then
    bsReadFromStream
  else if (FBufPos = FBufCount) then begin
    if FDirty then
      bsWriteToStream;
    FBufPos := 0;
    inc(FBufOfs, FBufSize);
    bsReadFromStream;
  end;
  {get the next character}
  aCh := AnsiChar(FBuffer[FBufPos]);
  inc(FBufPos);
end;

{-----------------------------------------------------------------------------}

procedure TIpBufferedStream.bsReadFromStream;
var
  NewPos : Longint;
begin
  {assumptions: FBufOfs is where to read the buffer
                FBufSize is the number of bytes to read
                FBufCount will be the number of bytes read}
  NewPos := FStream.Seek(FBufOfs, soFromBeginning);
  if (NewPos <> FBufOfs) then
    raise EIpBaseException.Create(SNoSeekForRead);
  FBufCount := FStream.Read(FBuffer^, FBufSize);
end;

{-----------------------------------------------------------------------------}

procedure TIpBufferedStream.bsSetStream(aValue : TStream);
begin
  if (aValue <> FStream) then begin
    {if the buffer is dirty, flush it to the current stream}
    if FDirty and (FStream <> nil) then
      bsWriteToStream;
    {remember the stream and initialize all fields}
    FStream := aValue;
    bsInitForNewStream;
  end;
end;

{-----------------------------------------------------------------------------}

procedure TIpBufferedStream.bsWriteToStream;
var
  NewPos       : Longint;
  BytesWritten : Longint;
begin
  {assumptions: FDirty is true
                FBufOfs is where to write the buffer
                FBufCount is the number of bytes to write
                FDirty will be set false afterwards}
  NewPos := FStream.Seek(FBufOfs, soFromBeginning);
  if (NewPos <> FBufOfs) then
    raise EIpBaseException.Create(SNoSeekForWrite);
  BytesWritten := FStream.Write(FBuffer^, FBufCount);
  if (BytesWritten <> FBufCount) then
    raise EIpBaseException.Create(SCannotWriteToStream);
  FDirty := false;
end;
{Begin !!.12}

{-----------------------------------------------------------------------------}

procedure TIpBufferedStream.Flush;
begin
  if FDirty then
    bsWriteToStream;
end;
{End !!.12}

{-----------------------------------------------------------------------------}

procedure TIpBufferedStream.FreeStream ;
begin
  if (FBuffer <> nil) and (FStream <> nil) then begin
    if FDirty then
      bsWriteToStream;
    FStream.Free;
    FStream := nil;
  end;
end;

{-----------------------------------------------------------------------------}

function TIpBufferedStream.Read(var Buffer; Count : Longint) : Longint;
var
  BytesToGo   : Longint;
  BytesToRead : Longint;
  BufAsBytes  : TByteArray absolute Buffer;
  DestPos     : Longint;
begin
  Result := 0;
  if not Assigned(FStream) then
    Exit;
  {calculate the number of bytes we could read if possible}
  BytesToGo := MinLong(Count, FSize - (FBufOfs + FBufPos));
  {we will return this number of bytes or raise an exception}
  Result := BytesToGo;
  {are we going to read some data after all?}
  if (BytesToGo > 0) then begin
    {make sure that the buffer has some data in it}
    if (FBufCount = 0) then
      bsReadFromStream;
    {read as much as we can from the current buffer}
    BytesToRead := MinLong(BytesToGo, FBufCount - FBufPos);
    {transfer that number of bytes}
    Move(FBuffer[FBufPos], BufAsBytes[0], BytesToRead);
    {update our counters}
    inc(FBufPos, BytesToRead);
    dec(BytesToGo, BytesToRead);
    {if we have more bytes to read then we've reached the end of the
     buffer and so we need to read another, and another, etc}
    DestPos := 0;
    while BytesToGo > 0 do begin
      {if the current buffer is dirty, write it out}
      if FDirty then
        bsWriteToStream;
      {position and read the next buffer}
      FBufPos := 0;
      inc(FBufOfs, FBufSize);
      bsReadFromStream;
      {calculate the new destination position, and the number of bytes
       to read from this buffer}
      inc(DestPos, BytesToRead);
      BytesToRead := MinLong(BytesToGo, FBufCount - FBufPos);
      {transfer that number of bytes}
      Move(FBuffer[FBufPos], BufAsBytes[DestPos], BytesToRead);
      {update our counters}
      inc(FBufPos, BytesToRead);
      dec(BytesToGo, BytesToRead);
    end;
  end;
end;

{-----------------------------------------------------------------------------}

function TIpBufferedStream.Seek(Offset : Longint; Origin : word) : Longint;
var
  NewPos : Longint;
  NewOfs : Longint;
begin
  Result := 0;
  if not Assigned(FStream) then
    Exit;
  {optimization: to help code that just wants the current stream
   position (ie, reading the Position property), check for this as a
   special case}
  if (Offset = 0) and (Origin = soFromCurrent) then begin
    Result := FBufOfs + FBufPos;
    Exit;
  end;
  {calculate the desired position}
  case Origin of
    soFromBeginning : NewPos := Offset;
    soFromCurrent   : NewPos := (FBufOfs + FBufPos) + Offset;
    soFromEnd       : NewPos := FSize + Offset;
  else
    raise EIpBaseException.Create(SBadSeekOrigin);
    NewPos := 0; {to fool the compiler's warning--we never get here}
  end;
  {force the new position to be valid}
  if (NewPos < 0) then
    NewPos := 0
  else if (NewPos > FSize) then
    NewPos := FSize;
  {calculate the offset for the buffer}
  NewOfs := (NewPos div FBufSize) * FBufSize;
  {if the offset differs, we have to move the buffer window}
  if (NewOfs <> FBufOfs) then begin
    {check to see whether we have to write the current buffer to the
     original stream first}
    if FDirty then
      bsWriteToStream;
    {mark the buffer as empty}
    FBufOfs := NewOfs;
    FBufCount := 0;
  end;
  {set the position within the buffer}
  FBufPos := NewPos - FBufOfs;
  Result := NewPos;
end;

{-----------------------------------------------------------------------------}

function TIpBufferedStream.Write(const Buffer; Count : Longint) : Longint;
type
  TIpByteArray = array[0..MaxInt-1] of Byte;
var
  BytesToGo   : Longint;
  BytesToWrite: Longint;
  BufAsBytes  : TIpByteArray absolute Buffer;
  DestPos     : Longint;
begin
  Result := 0;
  if not Assigned(FStream) then
    Exit;
  {calculate the number of bytes we should be able to write}
  BytesToGo := Count;
  {we will return this number of bytes or raise an exception}
  Result := BytesToGo;
  {are we going to write some data?}
  if (BytesToGo > 0) then begin
    {try and make sure that the buffer has some data in it}
    if (FBufCount = 0) and ((FBufOfs + FBufPos) < FSize) then
      bsReadFromStream;
    {write as much as we can to the current buffer}
    BytesToWrite := MinLong(BytesToGo, FBufSize - FBufPos);
    {transfer that number of bytes}
    Move(BufAsBytes[0], FBuffer[FBufPos], BytesToWrite);
    FDirty := true;
    {update our counters}
    inc(FBufPos, BytesToWrite);
    if (FBufCount < FBufPos) then begin
      FBufCount := FBufPos;
      FSize := FBufOfs + FBufPos;
    end;
    dec(BytesToGo, BytesToWrite);
    {if we have more bytes to write then we've reached the end of the
     buffer and so we need to write another, and another, etc}
    DestPos := 0;
    while BytesToGo > 0 do begin
      {as the current buffer is dirty, write it out}
      bsWriteToStream;
      {position and read the next buffer, if required}
      FBufPos := 0;
      inc(FBufOfs, FBufSize);
      if (FBufOfs < FSize) then
        bsReadFromStream
      else
        FBufCount := 0;
      {calculate the new destination position, and the number of bytes
       to write to this buffer}
      inc(DestPos, BytesToWrite);
      BytesToWrite := MinLong(BytesToGo, FBufSize - FBufPos);
      {transfer that number of bytes}
      if BytesToWrite > 0 then
        Move(BufAsBytes[DestPos], FBuffer[0], BytesToWrite);
      FDirty := true;
      {update our counters}
      inc(FBufPos, BytesToWrite);
      if (FBufCount < FBufPos) then begin
        FBufCount := FBufPos;
        FSize := FBufOfs + FBufPos;
      end;
      dec(BytesToGo, BytesToWrite);
    end;
  end;
end;

{-----------------------------------------------------------------------------}
{                           TIpAnsiTextStream                                 }
{-----------------------------------------------------------------------------}

constructor TIpAnsiTextStream.Create(aStream : TStream);
begin
  inherited Create(aStream);

  {set up the line index variables}
  atsResetLineIndex;
end;

{-----------------------------------------------------------------------------}

destructor TIpAnsiTextStream.Destroy;
begin
  {if needed, free the fixed line buffer}
  if (FFixedLine <> nil) then
    FreeMem(FFixedLine, FixedLineLength);
  {free the line index}
  FLineIndex.Free;
  inherited Destroy;
end;

{-----------------------------------------------------------------------------}

function TIpAnsiTextStream.AtEndOfStream : Boolean;
begin
  Result := FSize = (FBufOfs + FBufPos);
end;

{-----------------------------------------------------------------------------}

procedure TIpAnsiTextStream.atsGetLine(var aStartPos, aEndPos, aLen : Longint);
var
  Done   : Boolean;
  Ch     : AnsiChar;
  PrevCh : AnsiChar;
  TempLineTerm: Integer;
begin
  if (LineTerminator = ltNone) then begin
    aStartPos := FBufOfs + FBufPos;
    aEndPos := Seek(aStartPos + FixedLineLength, soFromBeginning);
    aLen := aEndPos - aStartPos;
  end
  else begin
    aStartPos := FBufOfs + FBufPos;
    Ch := #0;
    Done := false;

    // use temp as local variable for speed
    case LineTerminator of
      ltCRLF : TempLineTerm := 0;
      ltLF   : TempLineTerm := 1;
      ltCR   : TempLineTerm := 2;
      ltOther: TempLineTerm := 3;
    else
      raise EIpBaseException.Create(SBadLineTerminator);
    end;

    if FDirty then
      bsWriteToStream;

    while not Done do
    begin
      PrevCh := Ch;

      {is there anything to read?}
      if (FSize = (FBufOfs + FBufPos)) then begin
        aEndPos := FBufOfs + FBufPos;
        aLen := aEndPos - aStartPos;
        Done := True;
      end;

      {make sure that the buffer has some data in it}
      if (FBufCount = 0) then
        bsReadFromStream
      else if (FBufPos = FBufCount) then begin
        FBufPos := 0;
        inc(FBufOfs, FBufSize);
        bsReadFromStream;
      end;

      {get the next character}
      Ch := AnsiChar(FBuffer[FBufPos]);
      inc(FBufPos);

      case TempLineTerm of
        0   : if (Ch = #10) then begin
                     Done := true;
                     aEndPos := FBufOfs + FBufPos;
                   if PrevCh = #13 then
                     aLen := aEndPos - aStartPos - 2
                   else
                     aLen := aEndPos - aStartPos - 1;
                   end;
        1   : if (Ch = #10) then begin
                     Done := true;
                     aEndPos := FBufOfs + FBufPos;
                     aLen := aEndPos - aStartPos - 1;
                   end;
        2   : if (Ch = #13) then begin
                     Done := true;
                     aEndPos := FBufOfs + FBufPos;
                       aLen := aEndPos - aStartPos - 1;
                   end;
        3   : if (Ch = LineTermChar) then begin
                     Done := true;
                     aEndPos := FBufOfs + FBufPos;
                     aLen := aEndPos - aStartPos - 1;
                   end;
      end;
    end;
  end;
end;

{-----------------------------------------------------------------------------}

function TIpAnsiTextStream.atsGetLineCount : Longint;
begin
  if FLineCount < 0 then
    Result := MaxLongInt
  else
    Result := FLineCount;
end;

{-----------------------------------------------------------------------------}

procedure TIpAnsiTextStream.atsResetLineIndex;
begin
  {make sure we have a line index}
  if (FLineIndex = nil) then begin
    FLineIndex := TList.Create;  {create the index: even elements are}
    FLineIndex.Count := LineIndexCount * 2; {linenums, odd are offsets}

    {if we didn't have a line index, set up some reasonable defaults}
    FLineTerm := ltCRLF;  {normal Windows text file terminator}
    FLineEndCh := #10;    {not used straight away}
    FLineLen := 80;       {not used straight away}
  end;
  FLineIndex[0] := pointer(0); {the first line is line 0 and...}
  FLineIndex[1] := pointer(0); {...it starts at position 0}
  FLineInxTop := 0;            {the top valid index}
  FLineInxStep := 1;           {step count before add a line to index}
  FLineCount := -1;            {number of lines (-1 = don't know)}
  FLineCurrent := 0;           {current line}
  FLineCurOfs := 0;            {current line offset}
end;

{-----------------------------------------------------------------------------}

procedure TIpAnsiTextStream.atsSetLineTerm(aValue : TIpLineTerminator);
begin
  if (aValue <> LineTerminator) and ((FBufOfs + FBufPos) = 0) then begin
    {if there was no terminator, free the line buffer}
    if (LineTerminator = ltNone) then begin
      FreeMem(FFixedLine, FixedLineLength);
      FFixedLine := nil;
    end;
    {set the new value}
    FLineTerm := aValue;
    {if there is no terminator now, allocate the line buffer}
    if (LineTerminator = ltNone) then begin
      GetMem(FFixedLine, FixedLineLength);
    end;
    atsResetLineIndex;
  end;
end;

{-----------------------------------------------------------------------------}

procedure TIpAnsiTextStream.atsSetLineEndCh(aValue : AnsiChar);
begin
  if ((FBufOfs + FBufPos) = 0) then begin
    FLineEndCh := aValue;
    atsResetLineIndex;
  end;
end;

{-----------------------------------------------------------------------------}

procedure TIpAnsiTextStream.atsSetLineLen(aValue : Integer);
begin
  if (aValue <> FixedLineLength) and ((FBufOfs + FBufPos) = 0) then begin
    {validate the new length first}
    if (aValue < 1) or (aValue > 1024) then
      raise EIpBaseException.Create(SBadLineLength);

    {set the new value; note that if there is no terminator we need to
     free the old line buffer, and then allocate a new one}
    if (LineTerminator = ltNone) then
      FreeMem(FFixedLine, FixedLineLength);
    FLineLen := aValue;
    if (LineTerminator = ltNone) then
      GetMem(FFixedLine, FixedLineLength);
    atsResetLineIndex;
  end;
end;

{-----------------------------------------------------------------------------}

procedure TIpAnsiTextStream.bsInitForNewStream;
begin
  inherited bsInitForNewStream;
  atsResetLineIndex;
end;

{-----------------------------------------------------------------------------}

function TIpAnsiTextStream.ReadLine : string;
var
  CurPos : Longint;
  EndPos : Longint;
  Len    : Longint;
  StLen  : Longint;
begin
  if not Assigned(FStream) then
    Exit;
  atsGetLine(CurPos, EndPos, Len);
  if (LineTerminator = ltNone) then begin
    {at this point, Len will either equal FixedLineLength, or it will
     be less than it because we read the last line of all and it was
     short}
    StLen := FixedLineLength;
    {$IFDEF MSWindows}
    SetLength(Result, StLen);
    {$ELSE}
    {$IFDEF IP_LAZARUS}
    SetLength(Result, StLen);
    {$ELSE}
    if (StLen > 255) then
      StLen := 255;
    Result[0] := char(StLen);
    {$ENDIF}
    {$ENDIF}
    if (Len < StLen) then
      FillChar(Result[Len+1], StLen-Len, ' ');
  end
  else {LineTerminator is not ltNone} begin
    {$IFDEF MSWindows}
    SetLength(Result, Len);
    {$ELSE}
    {$IFDEF IP_LAZARUS}
    SetLength(Result, Len);
    {$ELSE}
    if (Len > 255) then
      Len := 255;
    Result[0] := char(Len);
    {$ENDIF}
    {$ENDIF}
  end;
  {read the line}
  Seek(CurPos, soFromBeginning);
  if Len > 0 then
    Read(Result[1], Len);
  Seek(EndPos, soFromBeginning);
end;

{-----------------------------------------------------------------------------}

function TIpAnsiTextStream.ReadLineArray(aCharArray : PAnsiChar;
                                         aLen       : Longint)
                                                    : Longint;
var
  CurPos : Longint;
  EndPos : Longint;
  Len    : Longint;
  StLen  : Longint;
begin
  Result := 0;
  if not Assigned(FStream) then
    Exit;
  atsGetLine(CurPos, EndPos, Len);
  if (LineTerminator = ltNone) then begin
    {at this point, Len will either equal FixedLineLength, or it will
     be less than it because we read the last line of all and it was
     short}
    StLen := FixedLineLength;
    if (StLen > aLen) then
      StLen := aLen;
    if (Len < StLen) then
      FillChar(aCharArray[Len], StLen-Len, ' ');
    Result := StLen;
  end
  else {LineTerminator is not ltNone} begin
    if (Len > aLen) then
      Len := aLen;
    Result := Len;
  end;
  Seek(CurPos, soFromBeginning);
  Read(aCharArray[0], Len);
  Seek(EndPos, soFromBeginning);
end;

{-----------------------------------------------------------------------------}

function TIpAnsiTextStream.ReadLineZ(aSt : PAnsiChar; aMaxLen : Longint) : PAnsiChar;
var
  CurPos : Longint;
  EndPos : Longint;
  Len    : Longint;
  StLen  : Longint;
begin
  Result := nil;
  if not Assigned(FStream) then
    Exit;
  Result := aSt;
  atsGetLine(CurPos, EndPos, Len);
  if (LineTerminator = ltNone) then begin
    {at this point, Len will either equal FixedLineLength, or it will
     be less than it because we read the last line of all and it was
     short}
    StLen := FixedLineLength;
    if (StLen > aMaxLen) then
      StLen := aMaxLen;
    if (Len < StLen) then
      FillChar(Result[Len], StLen-Len, ' ');
    Result[StLen] := #0;
  end
  else {LineTerminator is not ltNone} begin
    if (Len > aMaxLen) then
      Len := aMaxLen;
    Result[Len] := #0;
  end;
  Seek(CurPos, soFromBeginning);
  Read(Result[0], Len);
  Seek(EndPos, soFromBeginning);
end;

{-----------------------------------------------------------------------------}

function TIpAnsiTextStream.SeekNearestLine(aOffset : Longint) : Longint;
var
  CurLine : Longint;
  CurOfs  : Longint;
  CurPos  : Longint;
  EndPos  : Longint;
  Len     : Longint;
  i       : Longint;
  Done    : Boolean;
  L, R, M : Integer;
begin
  Result := 0;
  if not Assigned(FStream) then
    Exit;
  {if the offset we want is for the current line, reposition at the
   current line offset, return the current line number and exit}
  if (aOffset = FLineCurOfs) then begin
    Seek(FLineCurOfs, soFromBeginning);
    Result := FLineCurrent;
    Exit;
  end;
  {if the offset requested is less than or equal to zero, just
   position at line zero (ie, the start of the stream)}
  if (aOffset <= 0) then begin
    Seek(0, soFromBeginning);
    FLineCurrent := 0;
    FLineCurOfs := 0;
    Result := 0;
    Exit;
  end;
  {if the offset requested is greater than or equal to the size of the
   stream, position at the end of the stream (note that if we don't
   know the number of lines in the stream yet, FLineCount is set to
   -1 and we can't take this shortcut because we need to return the
   true value)}
  if (FLineCount >= 0) and (aOffset >= FSize) then begin
    Seek(0, soFromEnd);
    FLineCurrent := FLineCount;
    FLineCurOfs := FSize;
    Result := FLineCount;
    Exit;
  end;
  {if the offset requested is greater than the top item in the
   line index, we shall have to build up the index until we get to the
   line we require, or just beyond}
  if (aOffset > Longint(FLineIndex[FLineInxTop+1])) then begin
    {position at the last known line offset}
    CurLine := Longint(FLineIndex[FLineInxTop]);
    CurOfs := Longint(FLineIndex[FLineInxTop+1]);
    Seek(CurOfs, soFromBeginning);
    Done := false;
    {continue reading lines in chunks of FLineInxStep and add an index
     entry for each chunk}
    while not Done do begin
      for i := 0 to pred(FLineInxStep) do begin
        atsGetLine(CurPos, EndPos, Len);
        inc(CurLine);
        CurOfs := EndPos;
        if (EndPos = FSize) then begin
          Done := true;
          Break;
        end;
      end;
      if Done then
        FLineCount := CurLine
      else begin
        inc(FLineInxTop, 2);
        if (FLineInxTop = (LineIndexCount * 2)) then begin
          {we've exhausted the space in the index: rescale}
          FLineInxTop := FLineInxTop div 2;
          for i := 0 to pred(FLineInxTop) do begin
            if Odd(i) then
              FLineIndex.Exchange((i*2)-1, i)
            else
              FLineIndex.Exchange(i*2, i);
          end;
          FLineInxStep := FLineInxStep * 2;
        end;
        FLineIndex[FLineInxTop] := pointer(CurLine);
        FLineIndex[FLineInxTop+1] := pointer(CurOfs);
        if (aOffset <= CurOfs) then
          Done := true;
      end;
    end;
  end;
  {we can now work out where the nearest item in the index is to the
   line we require}
  L := 1;
  R := FLineInxTop+1;
  while (L <= R) do begin
    M := (L + R) div 2;
    if not Odd(M) then
      inc(M);
    if (aOffset < Longint(FLineIndex[M])) then
      R := M - 2
    else if (aOffset > Longint(FLineIndex[M])) then
      L := M + 2
    else begin
      FLineCurrent := Longint(FLineIndex[M-1]);
      FLineCurOfs := Longint(FLineIndex[M]);
      Seek(FLineCurOfs, soFromBeginning);
      Result := FLineCurrent;
      Exit;
    end;
  end;
  {the item at L-2 will have the nearest smaller offset than the
   one we want, hence the nearest smaller line is at L-3; start here
   and read through the stream forwards}
  CurLine := Longint(FLineIndex[L-3]);
  Seek(Longint(FLineIndex[L-2]), soFromBeginning);
  while true do begin
    atsGetLine(CurPos, EndPos, Len);
    inc(CurLine);
    if (EndPos > aOffset) then begin
      FLineCurrent := CurLine - 1;
       FLineCurOfs := CurPos;
      Seek(CurPos, soFromBeginning);
      Result := CurLine - 1;
      Exit;
    end
    else if (CurLine = FLineCount) or (EndPos = aOffset) then begin
      FLineCurrent := CurLine;
      FLineCurOfs := EndPos;
      Seek(EndPos, soFromBeginning);
      Result := CurLine;
      Exit;
    end;
  end;
end;

{-----------------------------------------------------------------------------}

function TIpAnsiTextStream.SeekLine(aLineNum : Longint) : Longint;
var
  CurLine : Longint;
  CurOfs  : Longint;
  CurPos  : Longint;
  EndPos  : Longint;
  Len     : Longint;
  i       : Longint;
  Done    : Boolean;
  L, R, M : Integer;
begin
  Result := 0;
  if not Assigned(FStream) then
    Exit;
  {if the line number we want is the current line, reposition at the
   current line offset, return the current line number and exit}
  if (aLineNum = FLineCurrent) then begin
    Seek(FLineCurOfs, soFromBeginning);
    Result := FLineCurrent;
    Exit;
  end;
  {if the line number requested is less than or equal to zero, just
   position at line zero (ie, the start of the stream)}
  if (aLineNum <= 0) then begin
    Seek(0, soFromBeginning);
    FLineCurrent := 0;
    FLineCurOfs := 0;
    Result := 0;
    Exit;
  end;
  {if the line number requested is greater than or equal to the line
   count, position at the end of the stream (note that if we don't
   know the number of lines in the stream yet, FLineCount is set to
   -1)}
  if (FLineCount >= 0) and (aLineNum > FLineCount) then begin
    Seek(0, soFromEnd);
    FLineCurrent := FLineCount;
    FLineCurOfs := FSize;
    Result := FLineCount;
    Exit;
  end;
  {if the line number requested is greater than the top item in the
   line index, we shall have to build up the index until we get to the
   line we require, or just beyond}
  if (aLineNum > Longint(FLineIndex[FLineInxTop])) then begin
    {position at the last known line offset}
    CurLine := Longint(FLineIndex[FLineInxTop]);
    CurOfs := Longint(FLineIndex[FLineInxTop+1]);
    Seek(CurOfs, soFromBeginning);
    Done := false;
    {continue reading lines in chunks of FLineInxStep and add an index
     entry for each chunk}
    while not Done do begin
      for i := 0 to pred(FLineInxStep) do begin
        atsGetLine(CurPos, EndPos, Len);
        inc(CurLine);
        CurOfs := EndPos;
        if (EndPos = FSize) then begin
          Done := true;
          Break;
        end;
      end;
      if Done then
        FLineCount := CurLine
      else begin
        inc(FLineInxTop, 2);
        if (FLineInxTop = (LineIndexCount * 2)) then begin
          {we've exhausted the space in the index: rescale}
          FLineInxTop := FLineInxTop div 2;
          for i := 0 to pred(FLineInxTop) do begin
            if Odd(i) then
              FLineIndex.Exchange((i*2)-1, i)
            else
              FLineIndex.Exchange(i*2, i);
          end;
          FLineInxStep := FLineInxStep * 2;
        end;
        FLineIndex[FLineInxTop] := pointer(CurLine);
        FLineIndex[FLineInxTop+1] := pointer(CurOfs);
        if (aLineNum <= CurLine) then
          Done := true;
      end;
    end;
  end;
  {we can now work out where the nearest item in the index is to the
   line we require}
  L := 0;
  R := FLineInxTop;
  while (L <= R) do begin
    M := (L + R) div 2;
    if Odd(M) then
      dec(M);
    if (aLineNum < Longint(FLineIndex[M])) then
      R := M - 2
    else if (aLineNum > Longint(FLineIndex[M])) then
      L := M + 2
    else begin
      FLineCurrent := Longint(FLineIndex[M]);
      FLineCurOfs := Longint(FLineIndex[M+1]);
      Seek(FLineCurOfs, soFromBeginning);
      Result := FLineCurrent;
      Exit;
    end;
  end;
  {the item at L-2 will have the nearest smaller line number than the
   one we want; start here and read through the stream forwards}
  CurLine := Longint(PtrInt(FLineIndex[L-2]));
  Seek(Longint(PtrInt(FLineIndex[L-1])), soFromBeginning);
  while true do begin
    atsGetLine(CurPos, EndPos, Len);
    inc(CurLine);
    if (CurLine = FLineCount) or (CurLine = aLineNum) then begin
      FLineCurrent := CurLine;
      FLineCurOfs := EndPos;
      Seek(EndPos, soFromBeginning);
      Result := CurLine;
      Exit;
    end;
  end;
end;

{-----------------------------------------------------------------------------}

procedure TIpAnsiTextStream.WriteLine(const aSt : string);
{Rewritten !!.15}
begin
  if Length(aSt) > 0 then
    WriteLineArray(@aSt[1], length(aSt))
  else
    WriteLineArray(nil, 0);
end;

{-----------------------------------------------------------------------------}

procedure TIpAnsiTextStream.WriteLineArray(aCharArray : PAnsiChar;
                                           aLen       : Longint);
var
  C : AnsiChar;
begin
  if not Assigned(FStream) then
    Exit;
  if (aCharArray = nil) then
    aLen := 0;
  if (LineTerminator = ltNone) then begin
    if (aLen >= FixedLineLength) then
      Write(aCharArray[0], FixedLineLength)
    else begin
      FillChar(FFixedLine[aLen], FixedLineLength-aLen, ' ');
      if (aLen > 0) then
        Move(aCharArray[0], FFixedLine[0], aLen);
      Write(FFixedLine[0], FixedLineLength);
    end;
  end
  else begin
    if (aLen > 0) then
      Write(aCharArray[0], aLen);
    case LineTerminator of
      ltNone : {this'll never get hit};
      ltCR   : Write(LineTerm[ltCR], 1);
      ltLF   : Write(LineTerm[ltLF], 1);
      ltCRLF : Write(LineTerm[ltCRLF], 2);
      ltOther: begin
                 C := LineTermChar;
                 Write(C, 1);
               end;
    else
      raise EIpBaseException.Create(SBadLineTerminator);
    end;
  end;
end;

{-----------------------------------------------------------------------------}

procedure TIpAnsiTextStream.WriteLineZ(aSt : PAnsiChar);
var
  LenSt : Longint;
begin
  if not Assigned(FStream) then
    Exit;
  if (aSt = nil) then
    LenSt := 0
  else
    LenSt := StrLen(aSt);
  WriteLineArray(aSt, LenSt);
end;


{ TIpDownloadFileStream }

constructor TIpDownloadFileStream.Create(const aPath : string);
begin
  FHandle := INVALID_HANDLE_VALUE;
  inherited Create;
  dfsMakeTempFile(aPath);

  FHandle := THandle(FileOpen(FFileName, fmShareDenyNone + fmOpenReadWrite));
  if (Handle = INVALID_HANDLE_VALUE) then
{$IFDEF Version6OrHigher}
    RaiseLastOSError; 
{$ELSE}
    RaiseLastWin32Error;
{$ENDIF}
end;

destructor TIpDownloadFileStream.Destroy;
begin
  {$IFDEF IP_LAZARUS}
  writeln('ToDo: TIpDownloadFileStream.Destroy ');
  {$ELSE}
  FlushFileBuffers(FHandle);
  if (Handle <> INVALID_HANDLE_VALUE) then
    CloseHandle(Handle);
  {$ENDIF}
  inherited Destroy;
end;

procedure TIpDownloadFileStream.dfsMakeTempFile(const aPath : string);
begin
  { Make sure the path has no backslash. }
  if aPath[length(aPath)] = '\' then
    FPath := Copy(aPath, 1, pred(length(aPath)))
  else
    FPath := aPath;

  { Check that it really exists. }
  if not DirExists(aPath) then
    raise EIpBaseException.Create(SBadPath);

  { Create a new uniquely named file in that folder. }
  FFileName := GetTemporaryFile(FPath);                                {!!.12}
end;

function TIpDownloadFileStream.Read(var Buffer; Count : Longint) : Longint;
{$IFDEF IP_LAZARUS}
begin
  writeln('ToDo: TIpDownloadFileStream.Read ');
  Result:=0;
end;
{$ELSE}
var
  ReadOK : Bool;
begin
  ReadOK := ReadFile(Handle, Buffer, Count, DWord(Result), nil);

  if not ReadOK then begin
    raise EIpBaseException.Create(SysErrorMessage(GetLastError) + SFilename + FFileName);
    Result := 0;
  end;
end;
{$ENDIF}

procedure TIpDownloadFileStream.Rename(aNewName : string);
var
  NewFullName : string;
begin
  {$IFDEF IP_LAZARUS}
  writeln('ToDo: TIpDownloadFileStream.Rename ');
  {$ENDIF}
  {close the current handle}
  {$IFNDEF IP_LAZARUS}
  CloseHandle(Handle);
  {$ENDIF}
  FHandle := INVALID_HANDLE_VALUE;
  {calculate the full new name}
  NewFullName := FPath + '\' + aNewName;
  {rename the file}
{$IFDEF Version6OrHigher}
  {$IFNDEF IP_LAZARUS}
  if not MoveFile(PAnsiChar(FFileName), PAnsiChar(NewFullName)) then
    RaiseLastOSError;
  {$ENDIF}
{$ELSE}
  Win32Check(MoveFile(PAnsiChar(FFileName), PAnsiChar(NewFullName)));
{$ENDIF}
  {open up the same file, but with its new name}
  FFileName := NewFullName;
  try
    FHandle := THandle(FileOpen(FFileName, fmShareDenyNone + fmOpenRead));
  except
    { do nothing }
  end;

  if (Handle = INVALID_HANDLE_VALUE) then
{$IFDEF Version6OrHigher}
    RaiseLastOSError;
{$ELSE}
    RaiseLastWin32Error;
{$ENDIF}

  FRenamed  := true;
end;

procedure TIpDownloadFileStream.Move(aNewName : string);
begin
  {$IFDEF IP_LAZARUS}
  writeln('ToDo: TIpDownloadFileStream.Move ');
  {$ENDIF}
  {close the current handle}
  {$IFNDEF IP_LAZARUS}
  CloseHandle(Handle);
  {$ENDIF}
  FHandle := INVALID_HANDLE_VALUE;
  {copy the file}                                                      {!!.01}
{$IFDEF Version6OrHigher}
  {$IFNDEF IP_LAZARUS}
  if not CopyFile(PAnsiChar(FFileName), PAnsiChar(aNewName), False) then
    RaiseLastOSError;
  {$ENDIF}
{$ELSE}
  Win32Check(CopyFile(PAnsiChar(FFileName),                            {!!.01}
    PAnsiChar(aNewName), False));                                      {!!.01}
{$ENDIF}

  {open up the same file, but with its new name}
  FFileName := aNewName;
  try
    FHandle := THandle(FileOpen(FFileName, fmShareDenyNone + fmOpenRead));
  except
    { do nothing }
  end;

  if (Handle = INVALID_HANDLE_VALUE) then
{$IFDEF Version6OrHigher}
    RaiseLastOSError;
{$ELSE}
    RaiseLastWin32Error;
{$ENDIF}

  FRenamed  := true;
end;

function TIpDownloadFileStream.Seek(Offset : Longint; Origin : Word) : Longint;
begin
  {$IFDEF IP_LAZARUS}
  writeln('ToDo: TIpDownloadFileStream.Seek');
  Result := 0;
  {$ELSE}
  Result := SetFilePointer(Handle, Offset, nil, Origin);
  {$ENDIF}
end;

function TIpDownloadFileStream.Write(const Buffer; Count : Longint) : Longint;
{$IFDEF IP_LAZARUS}
begin
  writeln('ToDo: TIpDownloadFileStream.Write');
  Result:=Count;
end;
{$ELSE}
var
  WriteOK : Bool;
begin
  WriteOK := WriteFile(Handle, Buffer, Count, DWord(Result), nil);

  if not WriteOK then begin
    raise EIpBaseException.Create(SysErrorMessage(GetLastError) + SFilename + FFileName);
    Result := 0
  end;
end;
{$ENDIF}


{ TIpByteStream }
constructor TIpByteStream.Create(aStream : TStream);
begin
  inherited Create;
  FStream := aStream;
end;

destructor TIpByteStream.Destroy;
begin
  inherited Destroy;
end;

function TIpByteStream.Read(var b : Byte) : Boolean;
begin
  Result := True;
  if (BufPos = BufEnd) then begin
    BufPos := 0;
    BufEnd := FStream.Read(Buffer, SizeOf(Buffer));
    if (BufEnd = 0) then begin
      Result := False;
      Exit;
    end;
  end;
  b := Buffer[BufPos];
  Inc(BufPos);
end;

function TIpByteStream.GetPosition : Integer;
begin
  Result := FStream.Position - BufEnd + BufPos;
end;

function TIpByteStream.GetSize : Integer;
begin
  Result := FStream.Size;
end;

end.

