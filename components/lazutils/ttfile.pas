(*******************************************************************
 *
 *  TTFile.Pas                                                1.3
 *
 *    File I/O Component (specification)
 *
 *  Copyright 1996 David Turner, Robert Wilhelm and Werner Lemberg
 *
 *  This file is part of the FreeType project, and may only be used
 *  modified and distributed under the terms of the FreeType project
 *  license, LICENSE.TXT. By continuing to use, modify or distribute
 *  this file you indicate that you have read the license and
 *  understand and accept it fully.
 *
 *  NOTES :
 *
 *  Changes from 1.2 to 1.3 :
 *
 *  - Moved stream into TFreeTypeStream object
 *
 *  Changes from 1.1 to 1.2 :
 *
 *  - Changes the stream operations semantics. See changes.txt
 *
 *  - stream records are now allocated on demand in the heap
 *
 *  - introduced the 'frame cache' to avoid Allocating/Freeing
 *    each frame, even tiny ones..
 *
 *  - support for thread-safety and re-entrancy
 *
 *    ( re-entrancy is there for information only.. )
 *
 *  Changes from 1.0 to 1.1 :
 *
 *  - defined the type TT_Stream for file handles
 *  - renamed ( and cleaned ) the API.
 *
 *  - caching and memory-mapped files use the same API :
 *
 *      TT_Access_Frame to notify
 *
 *  - only the interface was really rewritten. This component still
 *    only supports one opened file at a time.
 *
 ******************************************************************)

Unit TTFile;

interface

{$I TTCONFIG.INC}
{$R-}

uses TTTypes,
     TTError,
     Classes;

type
  { TFreeTypeStream }

  TFreeTypeStream = class
  private
    function GetSize: longint;
  private
    FCurrentFrame : PByte;
    FFrameCursor  : Longint;
    FFrameSize    : LongInt;
    FFrameCache : PByte;

    FName: string;
    FStream: TStream;
    FBase,FStoredSize,FPosit: Longint;
    FOwnedStream: boolean;
    FOpen: boolean;
    FUsed: boolean;
    function GetFilePos: longint;
    function GetFileSize: longint;
    function GetPosition: longint;
    property Size: longint read GetSize;
    procedure Init;
  public
    constructor Create(APathName: string);
    constructor Create(AStream: TStream; AStreamOwner: boolean);
    destructor Destroy; override;
    function Activate: TError;
    function Deactivate: TError;
    function SeekFile(APos: Longint): TError;
    function SkipFile(ADist: Longint): TError;
    function ReadFile( var ABuff; ACount : Int ) : TError;
    function ReadAtFile( APos : Long; var ABuff; ACount : Int ) : TError;

   (*********************************************************************)
   (*                                                                   *)
   (*  Frame Functions                                                  *)
   (*                                                                   *)
   (*********************************************************************)

   (* Access the next aSize bytes *)
   function AccessFrame( aSize : Int ) : TError;

   (* Access the next min(aSize,file_size-file_pos) bytes *)
   function CheckAndAccessFrame( aSize : Int ) : TError;

   (* Forget the previously cached frame *)
   function ForgetFrame :  TError;

   (* The following functions should only be used after a      *)
   (* AccessFrame and before a ForgetFrame                     *)

   (* They do not provide error handling, intentionnaly, and are much faster *)
   (* moreover, they could be converted to MACROS in the C version           *)

   function GET_Byte   : Byte;
   function GET_Char   : ShortInt;
   function GET_Short  : Short;
   function GET_UShort : UShort;
   function GET_Long   : Long;
   function GET_ULong  : ULong;
   function GET_Tag4   : ULong;

    property Open: boolean read FOpen;
    property Name: string read FName;
    property Base: longint read FBase;
    property Position: longint read GetPosition;
    property Used: boolean read FUsed;
  end;

  function  TTFile_Init : TError;
  procedure TTFile_Done;

 (*********************************************************************)
 (*                                                                   *)
 (*  Stream Functions                                                 *)
 (*                                                                   *)
 (*********************************************************************)

 function  TT_Open_Stream( name       : String;
                           var stream : TT_Stream ) : TError;
 (* Open a file and return a stream handle for it               *)
 (* should only be used for a new typeface object's main stream *)
 function  TT_Open_Stream( AStream: TStream; AStreamOwner: boolean;
                           var stream : TT_Stream ) : TError;

 procedure TT_Close_Stream( var stream : TT_Stream );
 (* closes, then discards a stream, when it becomes unuseful *)
 (* should only be used for a typeface object's main stream  *)

 function  TT_Use_Stream( org_stream : TT_Stream;
                          out ftstream: TFreeTypeStream ) : TError;
 (* notices the component that we're going to use the file   *)
 (* opened in 'org_stream', and report errors to the 'error' *)
 (* variable. the 'stream' variable is untouched, except in  *)
 (* re-entrant buids.                                        *)

 (* in re-entrant builds, the original file handle is duplicated *)
 (* to a new stream which reference is passed to the 'stream'    *)
 (* variable.. thus, each thread can have its own file cursor to *)
 (* access the same file concurrently..                          *)

 procedure TT_Flush_Stream( stream : TT_Stream );
 (* closes a stream's font handle. This is useful to save      *)
 (* system resources.                                          *)

 procedure TT_Done_Stream( stream : TT_Stream );
 (* notice the file component that we don't need to perform    *)
 (* file ops on the stream 'stream' anymore..                  *)
 (*                                                            *)
 (* in re-entrant builds, should also discard the stream       *)

 function TT_Stream_Size( stream : TT_Stream ) : longint;

implementation

uses
  SysUtils;

  (* THREADS: TTMutex, *)

const
  frame_cache_size = 2048;
  (* we allocate a single block where we'll place all of our frames *)
  (* instead of allocating an new block on each access. Note that   *)
  (* frames that are bigger than this constant are effectively      *)
  (* allocated in the heap..                                        *)

  function TT_Stream_Size( stream : TT_Stream ) : longint;
  var
    rec : TFreeTypeStream;
  begin
    rec := TFreeTypeStream(stream.z);
    if rec = nil then
      TT_Stream_Size := 0
    else
      TT_Stream_Size := rec.Size;
  end;

(*******************************************************************
 *
 *  Function    :  TTFile_Init
 *
 *  Description :  Init the file component
 *
 ******************************************************************)
 function TTFile_Init : TError;
 begin
   TTFile_Init := Success;
 end;

(*******************************************************************
 *
 *  Function    :  TTFile_Done
 *
 *  Description :  Finalize the file component
 *
 ******************************************************************)
 procedure TTFile_Done;
 begin
   //nothing
 end;

(*******************************************************************
 *
 *  Function    :  TT_Open_Stream
 *
 *  Description :  opens the font file in a new stream
 *
 *  Input  :  stream  : target stream variable
 *            name    : file pathname
 *            error   : the variable that will be used to
 *                      report stream errors
 *
 *  Output :  True on sucess.
 *
 ******************************************************************)
 function TT_Open_Stream( name       : String;
                          var stream : TT_Stream ) : TError;
 var
   ftstream : TFreeTypeStream;

 begin
   TT_Open_Stream := Failure;
   stream.z := nil;
   ftstream := nil;

   try
     ftstream := TFreeTypeStream.Create(name);
     if ftstream.Activate then
       raise exception.Create('Cannot activate stream, file may not exist');
   except
     on ex: Exception do
     begin
       ftstream.free;
       exit;
     end;
   end;

   stream.z:= ftstream;
   TT_Open_Stream := Success;
 end;

 function TT_Open_Stream(AStream: TStream; AStreamOwner: boolean;
   var stream: TT_Stream): TError;
 var
   ftstream : TFreeTypeStream;

 begin
   TT_Open_Stream := Failure;
   stream.z := nil;
   ftstream := nil;

   try
     ftstream := TFreeTypeStream.Create(AStream,AStreamOwner);
     if ftstream.Activate then
       raise exception.Create('Cannot activate');
   except
     on ex: Exception do
     begin
       ftstream.free;
       exit;
     end;
   end;

   stream.z:= ftstream;
   TT_Open_Stream := Success;
 end;

(*******************************************************************
 *
 *  Function    : TT_Close_Stream
 *
 *  Description : Closes the font file and releases memory buffer
 *
 *  Input  :  None
 *
 *  Output :  True ( always )
 *
 ******************************************************************)
 procedure TT_Close_Stream( var stream : TT_Stream );
 begin
   if stream.z = nil then exit;
   TFreeTypeStream(stream.z).Free;
   stream.z   := nil;
 end;

(*******************************************************************
 *
 *  Function    : TT_Use_Stream
 *
 *  Description : Acquire the file mutex (blocking call)
 *
 *  Input  :  org_stream : original stream to use
 *            stream     : duplicate stream (in re-entrant builds)
 *                         set to 'org_stream' otherwise
 *            error      : error report variable
 *
 *  Output :  True on success. False on failure
 *
 ******************************************************************)
 function  TT_Use_Stream( org_stream : TT_Stream;
                          out ftstream: TFreeTypeStream) : TError;
 begin
   TT_Use_Stream := Failure;

   ftstream:= TFreeTypeStream(org_stream.z);
   if ftstream= nil then exit;
   if ftstream.FUsed then
   begin
     error := TT_Err_File_Error;
     ftstream := nil;
     exit;
   end;
   ftstream.FUsed := true;

   result := ftstream.Activate;
 end;

(*******************************************************************
 *
 *  Function    : TT_Flush_Stream
 *
 *  Description : closes a stream
 *
 *  Input  :  stream : the stream
 *
 *  Output :  True on success. False on failure
 *
 ******************************************************************)
 procedure TT_Flush_Stream( stream : TT_Stream );
 begin
   if stream.z = nil then exit;
   TFreeTypeStream(stream.z).Deactivate;
 end;

(*******************************************************************
 *
 *  Function    : TT_Done_Stream
 *
 *  Description : Release the file mutex on a stream
 *
 *  Input  :  stream : the stream
 *
 *  Output :  Nothing.
 *
 ******************************************************************)
 procedure TT_Done_Stream( stream : TT_Stream );
 {$IF FPC_FULLVERSION<20701}
 var
   p: Pointer;
 {$ENDIF}
 begin
   if stream.z = nil then exit;
   {$IF FPC_FULLVERSION<20701}
   {$HINT workaround for fpc bug 23868 when compiling with -O2}
   p:=stream.z;
   TFreeTypeStream(p).FUsed := false;
   {$ELSE}
   TFreeTypeStream(stream.z).FUsed := false;
   {$ENDIF}
 end;

(*******************************************************************
 *
 *  Function    :  AccessFrame
 *
 *  Description :  Notifies the component that we're going to read
 *                 aSize bytes from the current file position.
 *                 This function should load/cache/map these bytes
 *                 so that they will be addressed by the GET_xxx
 *                 functions easily.
 *
 *  Input  :  aSize   number of bytes to access.
 *
 *  Output :  True on success. False on failure
 *
 *            The function fails is the byte range is not within the
 *            the file, or if there is not enough memory to cache
 *            the bytes properly ( which usually means that aSize is
 *            too big in both cases ).
 *
 *            It will also fail if you make two consecutive calls
 *            to AccessFrame, without a ForgetFrame between
 *            them.
 *
 ******************************************************************)
 function TFreeTypeStream.AccessFrame( aSize : Int ) : TError;
 begin
   result := Failure;

   if FCurrentFrame <> nil then
   begin
     error := TT_Err_Nested_Frame_Access;
     exit;
   end;
   (* We already are accessing one frame *)

   if aSize > frame_cache_size then
     GetMem( FCurrentFrame, aSize )
   else
     FCurrentFrame := FFrameCache;

   if ReadFile( FCurrentFrame^, aSize ) then
   begin
     if aSize > frame_cache_size then
       FreeMem( FCurrentFrame, aSize );

     FCurrentFrame := nil;
     exit;
   end;

   FFrameSize   := aSize;
   FFrameCursor := 0;

   result := Success;
 end;

(*******************************************************************
 *
 *  Function    :  CheckAndAccess_Frame
 *
 *  Description :  Notifies the component that we're going to read
 *                 aSize bytes from the current file position.
 *                 This function should load/cache/map these bytes
 *                 so that they will be addressed by the GET_xxx
 *                 functions easily.
 *
 *  Input  :  aSize   number of bytes to access.
 *
 *  Output :  True on success. False on failure
 *
 *            The function fails is the byte range is not within the
 *            the file, or if there is not enough memory to cache
 *            the bytes properly ( which usually means that aSize is
 *            too big in both cases ).
 *
 *            It will also fail if you make two consecutive calls
 *            to AccessFrame, without a ForgetFrame between
 *            them.
 *
 *
 * NOTE :  The only difference with AccessFrame is that we check
 *         that the frame is within the current file.  We otherwise
 *         truncate it..
 *
 ******************************************************************)
 function TFreeTypeStream.CheckAndAccessFrame( aSize : Int ) : TError;
 var
   readBytes : Longint;
 begin
   readBytes := Size - Position;
   if aSize > readBytes then aSize := readBytes;
   result := AccessFrame( aSize);
 end;

(*******************************************************************
 *
 *  Function    :  ForgetFrame
 *
 *  Description :  Releases a cached frame after reading
 *
 *  Input  :  None
 *
 *  Output :  True on success. False on failure
 *
 ******************************************************************)
 function TFreeTypeStream.ForgetFrame : TError;
 begin
   result := Failure;

   if FCurrentFrame = nil then exit;

   if FFrameSize > frame_cache_size then
     FreeMem( FCurrentFrame, FFrameSize );

   FFrameSize    := 0;
   FCurrentFrame := nil;
   FFrameCursor  := 0;
 end;

(*******************************************************************
 *
 *  Function    :  GET_Byte
 *
 *  Description :  Extracts a byte from the current file frame
 *
 *  Input  :  None
 *
 *  Output :  Extracted Byte.
 *
 *  NOTES : We consider that the programmer is intelligent enough
 *          not to try to get a byte that is out of the frame. Hence,
 *          we provide no bounds check here. (A misbehaving client
 *          could easily page fault using this call).
 *
 ******************************************************************)
 function TFreeTypeStream.GET_Byte : Byte;
 begin
   GET_Byte := FCurrentFrame^[FFrameCursor];
   inc( FFrameCursor );
 end;

(*******************************************************************
 *
 *  Function    :  GET_Char
 *
 *  Description :  Extracts a signed byte from the current file frame
 *
 *  Input  :  None
 *
 *  Output :  Extracted char.
 *
 *  NOTES : We consider that the programmer is intelligent enough
 *          not to try to get a byte that is out of the frame. Hence,
 *          we provide no bounds check here. (A misbehaving client
 *          could easily page fault using this call).
 *
 ******************************************************************)
 function TFreeTypeStream.GET_Char : ShortInt;
 begin
   GET_Char := ShortInt( FCurrentFrame^[FFrameCursor] );
   inc( FFrameCursor );
 end;

(*******************************************************************
 *
 *  Function    :  GET_Short
 *
 *  Description :  Extracts a short from the current file frame
 *
 *  Input  :  None
 *
 *  Output :  Extracted short.
 *
 *  NOTES : We consider that the programmer is intelligent enough
 *          not to try to get a byte that is out of the frame. Hence,
 *          we provide no bounds check here. (A misbehaving client
 *          could easily page fault using this call).
 *
 ******************************************************************)
 function TFreeTypeStream.GET_Short : Short;
 begin
   GET_Short := (Short(FCurrentFrame^[ FFrameCursor ]) shl 8) or
                 Short(FCurrentFrame^[FFrameCursor+1]);
   inc( FFrameCursor, 2 );
 end;

(*******************************************************************
 *
 *  Function    :  GET_UShort
 *
 *  Description :  Extracts an unsigned  short from the frame
 *
 *  Input  :  None
 *
 *  Output :  Extracted ushort.
 *
 *  NOTES : We consider that the programmer is intelligent enough
 *          not to try to get a byte that is out of the frame. Hence,
 *          we provide no bounds check here. (A misbehaving client
 *          could easily page fault using this call).
 *
 ******************************************************************)
 function TFreeTypeStream.GET_UShort : UShort;
 begin
   GET_UShort := (UShort(FCurrentFrame^[ FFrameCursor ]) shl 8) or
                  UShort(FCurrentFrame^[FFrameCursor+1]);
   inc( FFrameCursor, 2 );
 end;

(*******************************************************************
 *
 *  Function    :  GET_Long
 *
 *  Description :  Extracts a long from the frame
 *
 *  Input  :  None
 *
 *  Output :  Extracted long.
 *
 *  NOTES : We consider that the programmer is intelligent enough
 *          not to try to get a byte that is out of the frame. Hence,
 *          we provide no bounds check here. (A misbehaving client
 *          could easily page fault using this call).
 *
 ******************************************************************)
 function TFreeTypeStream.GET_Long : Long;
 begin
   GET_Long := (Long(FCurrentFrame^[ FFrameCursor ]) shl 24) or
               (Long(FCurrentFrame^[FFrameCursor+1]) shl 16) or
               (Long(FCurrentFrame^[FFrameCursor+2]) shl 8 ) or
               (Long(FCurrentFrame^[FFrameCursor+3])       );
   inc( FFrameCursor, 4 );
 end;

(*******************************************************************
 *
 *  Function    :  GET_ULong
 *
 *  Description :  Extracts an unsigned long from the frame
 *
 *  Input  :  None
 *
 *  Output :  Extracted ulong.
 *
 *  NOTES : We consider that the programmer is intelligent enough
 *          not to try to get a byte that is out of the frame. Hence,
 *          we provide no bounds check here. (A misbehaving client
 *          could easily page fault using this call).
 *
 ******************************************************************)
 function TFreeTypeStream.GET_ULong : ULong;
 begin
   GET_ULong := (ULong(FCurrentFrame^[ FFrameCursor ]) shl 24) or
                (ULong(FCurrentFrame^[FFrameCursor+1]) shl 16) or
                (ULong(FCurrentFrame^[FFrameCursor+2]) shl 8 ) or
                (ULong(FCurrentFrame^[FFrameCursor+3])       );
   inc( FFrameCursor, 4 );
 end;

(*******************************************************************
 *
 *  Function    :  GET_Tag4
 *
 *  Description :  Extracts a Tag from the frame
 *
 *  Input  :  None
 *
 *  Output :  Extracted 4 byte Tag.
 *
 *  NOTES : We consider that the programmer is intelligent enough
 *          not to try to get a byte that is out of the frame. Hence,
 *          we provide no bounds check here. (A misbehaving client
 *          could easily page fault using this call).
 *
 ******************************************************************)
 function TFreeTypeStream.GET_Tag4 : ULong;
 var
   C : array[0..3] of Byte;
 begin
   move ( FCurrentFrame^[FFrameCursor], c{%H-}, 4 );
   inc( FFrameCursor, 4 );

   GET_Tag4 := ULong(C);
 end;

{ TFreeTypeStream }

function TFreeTypeStream.GetFileSize: longint;
begin
 if FStream = nil then
    result := 0
  else
    result := FStream.Size;
end;

function TFreeTypeStream.GetPosition: longint;
begin
  if Open then result := GetFilePos else result := FPosit;
end;

procedure TFreeTypeStream.Init;
begin
  FOpen:= false;
  FStream := nil;
  FBase:= 0;
  FStoredSize:= -1;
  FPosit:= 0;

  (* empty frame *)
  FCurrentFrame := nil;
  FFrameCursor  := 0;
  FFrameSize    := 0;

  (* create frame cache *)
  GetMem( FFrameCache, frame_cache_size );
end;

constructor TFreeTypeStream.Create(APathName: string);
begin
  if APathName = '' then
    raise exception.Create('Empty path name');
  Init;
  FName:= APathName;
end;

constructor TFreeTypeStream.Create(AStream: TStream; AStreamOwner: boolean);
begin
  Init;
  FStream:= AStream;
  FOwnedStream := AStreamOwner;
end;

destructor TFreeTypeStream.Destroy;
begin
  Deactivate;
  if FOwnedStream then FreeAndNil(FStream);

  if FCurrentFrame <> nil then ForgetFrame;
  if FFrameCache <> nil then
    FreeMem( FFrameCache, frame_cache_size );
  FFrameCache := nil;

  inherited Destroy;
end;

 function TFreeTypeStream.Activate: TError;
 begin
   result := Success;
   if Open then exit;

   //in case stream provided by user
   if (FName = '') and (FStream <> nil) then
   begin
     FOpen := True;
     exit;
   end;

   try
     FStream := TFileStream.Create(FName, fmOpenRead or fmShareDenyWrite);
     FOpen := True;
     FBase := 0;
     try
       if FStoredSize = -1 then FStoredSize := FStream.Size;
       if FPosit <> 0 then FStream.Position:= FPosit;
     except
       on ex:exception do
       begin
         FreeAndNil(FStream);
         FOpen := False;
         error := TT_Err_File_Error;
         result := Failure;
         exit;
       end;
     end;
   except
     on ex:exception do
     begin
       error := TT_Err_Could_Not_Open_File;
       result := Failure;
       exit;
     end;
   end;
 end;

 function TFreeTypeStream.Deactivate: TError;
 begin
   result := Success;
   if not Open then exit;

   if FName = '' then //in case stream provided by user
   begin
     FOpen := false;
     exit;
   end;

   FPosit := FStream.Position;
   FreeAndNil(FStream);
   FOpen := false;
 end;

 function TFreeTypeStream.SeekFile(APos: Longint): TError;
 begin
   if FStream = nil then
   begin
     error := TT_Err_File_Error;
     result := Failure;
     exit;
   end;
   try
     FStream.Position := APos;
   except
     on ex: exception do
     begin
       error        := TT_Err_Invalid_File_Offset;
       result := Failure;
       exit;
     end;
   end;
   result := Success;
 end;

 function TFreeTypeStream.SkipFile(ADist: Longint): TError;
 begin
   result := SeekFile(Position+ADist);
 end;

 function TFreeTypeStream.ReadFile(var ABuff; ACount: Int): TError;
 begin
   result := Failure;
   if FStream = nil then
   begin
     error := TT_Err_Invalid_File_Read;
     exit;
   end;
   try
     if FStream.Read(ABuff,ACount) <> ACount then
     begin
       error := TT_Err_Invalid_File_Read;
       exit;
     end;
     result := success;
   except
     on ex: Exception do
     begin
       error := TT_Err_Invalid_File_Read;
       exit;
     end;
   end;
 end;

function TFreeTypeStream.ReadAtFile(APos: Long; var ABuff; ACount: Int): TError;
begin
  result := Failure;

  if SeekFile( APos ) or
     ReadFile( ABuff, ACount ) then exit;

  result := Success;
end;

function TFreeTypeStream.GetSize: longint;
begin
  if Open then
    result := GetFileSize
  else
    result := FStoredSize;
end;

function TFreeTypeStream.GetFilePos: longint;
begin
  if FStream= nil then
    result := 0
  else
    result := FStream.Position;
end;

end.
