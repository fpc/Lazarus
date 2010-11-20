{%MainUnit ../filectrl.pp}
{------------------------------------------------------------------------------
  procedure CheckIfFileIsExecutable(const AFilename: string);
 ------------------------------------------------------------------------------}
procedure CheckIfFileIsExecutable(const AFilename: string);
begin
  // TProcess does not report, if a program can not be executed
  // to get good error messages consider the OS
  if not FileExistsUTF8(AFilename) then begin
    raise Exception.Create('file "'+AFilename+'" does not exist');
  end;
  if DirPathExists(AFilename) then begin
    raise Exception.Create('file "'+AFilename+'" is a directory and not an executable');
  end;
end;

{------------------------------------------------------------------------------
  procedure CheckIfFileIsSymlink(const AFilename: string);
 ------------------------------------------------------------------------------}
procedure CheckIfFileIsSymlink(const AFilename: string);
begin
  // to get good error messages consider the OS
  if not FileExistsUTF8(AFilename) then begin
    raise Exception.Create('file "'+AFilename+'" does not exist');
  end;
  raise Exception.Create('"'+AFilename+'" is not symlink');
end;

{------------------------------------------------------------------------------
  function FileIsReadable(const AFilename: string): boolean;
 ------------------------------------------------------------------------------}
function FileIsReadable(const AFilename: string): boolean;
begin
  Result:=true;
end;

{------------------------------------------------------------------------------
  FileIsWritable
 ------------------------------------------------------------------------------}
function FileIsWritable(const AFilename: string): boolean;
begin
  Result:=((FileGetAttrUTF8(AFilename) and faReadOnly)=0);
end;


{------------------------------------------------------------------------------
  function FileIsExecutable(const AFilename: string): boolean;
 ------------------------------------------------------------------------------}
function FileIsExecutable(const AFilename: string): boolean;
begin
  Result:=FileExistsUTF8(AFilename);
end;

{------------------------------------------------------------------------------
  function FileIsSymlink(const AFilename: string): boolean;
 ------------------------------------------------------------------------------}
function FileIsSymlink(const AFilename: string): boolean;
{$ifndef wince}
const
  IO_REPARSE_TAG_MOUNT_POINT = $A0000003;
  IO_REPARSE_TAG_SYMLINK     = $A000000C;
var
  Attr: Longint;
  Rec: TSearchRec;
{$endif}
begin
{$ifndef wince}
  Attr := FileGetAttrUTF8(AFilename);
  if (Attr <> -1) and (Attr and FILE_ATTRIBUTE_REPARSE_POINT <> 0) then
  begin
    FindFirstUTF8(AFilename, Attr, Rec);
    if Rec.FindHandle <> feInvalidHandle then
    begin
      Windows.FindClose(Rec.FindHandle);
      Result := (Rec.FindData.dwReserved0 = IO_REPARSE_TAG_SYMLINK) or (Rec.FindData.dwReserved0 = IO_REPARSE_TAG_MOUNT_POINT);
    end
    else
      Result := False;
  end
  else
{$endif}
    Result := False;
end;

{------------------------------------------------------------------------------
  GetFileDescription
 ------------------------------------------------------------------------------}
function GetFileDescription(const AFilename: string): string;
begin
  // date + time
  Result:=rsModified;
  try
    Result:=Result+FormatDateTime('DD/MM/YYYY hh:mm',
                           FileDateToDateTime(FileAgeUTF8(AFilename)));
  except
    Result:=Result+'?';
  end;
end;

{------------------------------------------------------------------------------
  function ReadAllLinks(const Filename: string;
    ExceptionOnError: boolean): string;
 ------------------------------------------------------------------------------}
function ReadAllLinks(const Filename: string;
  ExceptionOnError: boolean): string;
begin
  Result:=Filename;
end;

{------------------------------------------------------------------------------
  function FilenameIsAbsolute(const TheFilename: string):boolean;
 ------------------------------------------------------------------------------}
function FilenameIsAbsolute(const TheFilename: string):boolean;
begin
  Result:=FilenameIsWinAbsolute(TheFilename);
end;


function NeedRTLAnsi: boolean;
{$IFDEF WinCE}
// CP_UTF8 is missing in the windows unit of the Windows CE RTL
const
  CP_UTF8 = 65001;
{$ENDIF}
begin
  if FNeedRTLAnsiValid then
    exit(FNeedRTLAnsi);
  FNeedRTLAnsi:=GetACP<>CP_UTF8;
  FNeedRTLAnsiValid:=true;
  Result:=FNeedRTLAnsi;
end;

function ConsoleToUTF8(const s: string): string;// converts UTF8 string to console encoding (used by Write, WriteLn)
var
  Dst: PChar;
begin
  {$ifdef WinCE}
  Result := SysToUTF8(s);
  {$else}
  Dst := AllocMem((Length(s) + 1) * SizeOf(Char));
  if OemToChar(PChar(s), Dst) then
    Result := StrPas(Dst)
  else
    Result := s;
  FreeMem(Dst);
  Result := SysToUTF8(Result);
  {$endif}
end;

function UTF8ToConsole(const s: string): string;
var
  Dst: PChar;
begin
  {$ifdef WinCE}
  Result := UTF8ToSys(s);
  {$else}
  Result := UTF8ToSys(s);
  Dst := AllocMem((Length(Result) + 1) * SizeOf(Char));
  if CharToOEM(PChar(Result), Dst) then
    Result := StrPas(Dst);
  FreeMem(Dst);
  {$endif}
end;

{------------------------------------------------------------------------------
  FileSize
 ------------------------------------------------------------------------------}
{$ifndef WinCE}
function FileSizeAnsi(const Filename: string): int64;
var
  FindData: TWIN32FindDataA;
  FindHandle: THandle;
  Str: AnsiString;
begin
  // Fix for the bug 14360:
  // Don't assign the widestring to TSearchRec.name because it is of type
  // string, which will generate a conversion to the system encoding
  Str := UTF8Decode(Filename);
  FindHandle:=Windows.FindFirstFileA(PAnsiChar(Str), FindData);
  if FindHandle=Windows.Invalid_Handle_value then
  begin
    Result:=-1;
    exit;
  end;
  Result:=(int64(FindData.nFileSizeHigh) shl 32)+FindData.nFileSizeLow;
  Windows.FindClose(FindHandle);
end;
{$endif}

function FileSizeWide(const Filename: string): int64;
var
  FindData: TWIN32FindDataW;
  FindHandle: THandle;
  Str: WideString;
begin
  // Fix for the bug 14360:
  // Don't assign the widestring to TSearchRec.name because it is of type
  // string, which will generate a conversion to the system encoding
  Str := UTF8Decode(Filename);
  FindHandle:=Windows.FindFirstFileW(PWideChar(Str), FindData);
  if FindHandle=Windows.Invalid_Handle_value then
  begin
    Result:=-1;
    exit;
  end;
  Result:=(int64(FindData.nFileSizeHigh) shl 32)+FindData.nFileSizeLow;
  Windows.FindClose(FindHandle);
end;

{------------------------------------------------------------------------------
  FindFirstUTF8
 ------------------------------------------------------------------------------}
{$ifndef WinCE}
function FindFirstAnsi(const Path: string; Attr: Longint; out Rslt: TSearchRec): Longint;
begin
  Result:=SysUtils.FindFirst(UTF8ToSys(Path),Attr,Rslt);
  Rslt.Name:=SysToUTF8(Rslt.Name);
end;
{$endif}

function WinToDosTime (Var Wtime : TFileTime;var DTime:longint):longbool;
var
  lft : TFileTime;
begin
  WinToDosTime:=FileTimeToLocalFileTime(WTime,lft)
    {$ifndef WinCE}
    and FileTimeToDosDateTime(lft,Longrec(Dtime).Hi,LongRec(DTIME).lo)
    {$endif}
    ;
end;

function FindMatch(var f: TSearchRec) : Longint;
begin
  { Find file with correct attribute }
  While (F.FindData.dwFileAttributes and cardinal(F.ExcludeAttr))<>0 do
   begin
     if FindNextUTF8(F)<>0 then
      begin
        Result:=GetLastError;
        exit;
      end;
   end;
  { Convert some attributes back }
  WinToDosTime(F.FindData.ftLastWriteTime,F.Time);
  f.size:=F.FindData.NFileSizeLow+(qword(maxdword)+1)*F.FindData.NFileSizeHigh;
  f.attr:=F.FindData.dwFileAttributes;
  f.Name:=F.FindData.cFileName;
  Result:=0;
end;

procedure FindWideToAnsi(const wide: TWIN32FINDDATAW; var ansi: TWIN32FINDDATA);
var
  ws : WideString;
  an : AnsiString;
begin
  {$ifdef WinCE}
  ansi := wide;
  {$else}
  SetLength(ws, length(wide.cAlternateFileName));
  Move(wide.cAlternateFileName[0], ws[1], length(ws)*2);
  an:=ws; // no need to utf8 for cAlternateFileName (it's always ansi encoded)
  Move(an[1], ansi.cAlternateFileName, sizeof(ansi.cAlternateFileName));

  ws:=PWideChar(@wide.cFileName[0]);
  an:=UTF8Encode(ws);
  ansi.cFileName:=an;
  if length(an)<length(ansi.cFileName) then ansi.cFileName[ length(an)]:=#0;

  with ansi do
  begin
    dwFileAttributes := wide.dwFileAttributes;
    ftCreationTime := wide.ftCreationTime;
    ftLastAccessTime := wide.ftLastAccessTime;
    ftLastWriteTime := wide.ftLastWriteTime;
    nFileSizeHigh := wide.nFileSizeHigh;
    nFileSizeLow := wide.nFileSizeLow;
    dwReserved0 := wide.dwReserved0;
    dwReserved1 := wide.dwReserved1;
  end;
  {$endif}
end;

function FindFirstWide(const Path: string; Attr: Longint; out Rslt: TSearchRec): Longint;
var
  find  : TWIN32FINDDATAW;
begin
  Rslt.Name:=Path;
  Rslt.Attr:=attr;
  Rslt.ExcludeAttr:=(not Attr) and ($1e);
                 { $1e = faHidden or faSysFile or faVolumeID or faDirectory }
  { FindFirstFile is a Win32 Call }
  Rslt.FindHandle:=Windows.FindFirstFileW( PWideChar(UTF8Decode(Path)),find);
  If Rslt.FindHandle=Invalid_Handle_value then
  begin
    Result:=GetLastError;
    Exit;
  end;
  { Find file with correct attribute }
  FindWideToAnsi(find, Rslt.FindData);
  Result:=FindMatch(Rslt);
end;

{------------------------------------------------------------------------------
  FindNextUTF8
 ------------------------------------------------------------------------------}
{$ifndef WinCE}
function FindNextAnsi(var Rslt: TSearchRec): Longint;
begin
  Rslt.Name:=UTF8ToSys(Rslt.Name);
  Result:=SysUtils.FindNext(Rslt);
  Rslt.Name:=SysToUTF8(Rslt.Name);
end;
{$endif}

function FindNextWide(var Rslt: TSearchRec): Longint;
var
  wide  : TWIN32FINDDATAW;
begin
  if FindNextFileW(Rslt.FindHandle, wide) then
  begin
    FindWideToAnsi(wide, Rslt.FindData);
    Result := FindMatch(Rslt);
  end
  else
    Result := Integer(GetLastError);
end;

{------------------------------------------------------------------------------
  FindNextUTF8
 ------------------------------------------------------------------------------}
{$ifndef WinCE}
procedure FindCloseAnsi(var F: TSearchrec);
begin
  SysUtils.FindClose(F);
end;
{$endif}

procedure FindCloseWide(var F: TSearchrec);
begin
  Windows.FindClose(f.FindHandle);
end;


{------------------------------------------------------------------------------
  FileGetAttrUTF8
 ------------------------------------------------------------------------------}
{$ifndef WinCE}
function FileGetAttrAnsi(const FileName: String): Longint;
begin
  Result:=SysUtils.FileGetAttr(UTF8ToSys(Filename));
end;
{$endif}

function FileGetAttrWide(const FileName: String): Longint;
begin
  Result:=Integer(Windows.GetFileAttributesW(PWideChar(UTF8Decode(FileName))));
end;

{------------------------------------------------------------------------------
  FileSetAttrUTF8
 ------------------------------------------------------------------------------}
{$ifndef WinCE}
function FileSetAttrAnsi(const Filename: String; Attr: longint): Longint;
begin
  Result:=SysUtils.FileSetAttr(UTF8ToSys(Filename),Attr);
end;
{$endif}

function FileSetAttrWide(const Filename: String; Attr: longint): Longint;
begin
  if Windows.SetFileAttributesW(PWideChar(UTF8Decode(FileName)), Attr) then
    Result:=0
  else
    Result := Integer(Windows.GetLastError);
end;

{------------------------------------------------------------------------------
  DeleteFileUTF8
 ------------------------------------------------------------------------------}
{$ifndef WinCE}
function DeleteFileAnsi(const FileName: String): Boolean;
begin
  Result:=SysUtils.DeleteFile(UTF8ToSys(Filename));
end;
{$endif}

function DeleteFileWide(const FileName: String): Boolean;
begin
  Result:=Windows.DeleteFileW(PWideChar(UTF8Decode(FileName)));
end;

{------------------------------------------------------------------------------
  RenameFileUTF8
 ------------------------------------------------------------------------------}
{$ifndef WinCE}
function RenameFileAnsi(const OldName, NewName: String): Boolean;
begin
  Result:=SysUtils.RenameFile(UTF8ToSys(OldName),UTF8ToSys(NewName));
end;
{$endif}

function RenameFileWide(const OldName, NewName: String): Boolean;
begin
  Result:=MoveFileW(PWideChar(UTF8Decode(OldName)), PWideChar(UTF8Decode(NewName)));
end;

{------------------------------------------------------------------------------
  GetCurrentDirUTF8
 ------------------------------------------------------------------------------}
{$ifndef WinCE}
function GetCurrentDirAnsi: String;
begin
  Result:=SysToUTF8(SysUtils.GetCurrentDir);
end;
{$endif}

function GetCurrentDirWide: String;
var
  w   : WideString;
  res : Integer;
begin
  {$ifdef WinCE}
  raise Exception.Create('[GetCurrentDirWide] The concept of the current directory doesn''t exist in Windows CE');
  {$else}
  res:=GetCurrentDirectoryW(0, nil);
  SetLength(w, res);
  res:=Windows.GetCurrentDirectoryW(res, @w[1]);
  SetLength(w, res);
  Result:=UTF8Encode(w);
  {$endif}
end;

{------------------------------------------------------------------------------
  SetCurrentDirUTF8
 ------------------------------------------------------------------------------}
{$ifndef WinCE}
function SetCurrentDirAnsi(const NewDir: String): Boolean;
begin
  Result:=SysUtils.SetCurrentDir(UTF8ToSys(NewDir));
end;
{$endif}

function SetCurrentDirWide(const NewDir: String): Boolean;
begin
  {$ifdef WinCE}
  raise Exception.Create('[SetCurrentDirWide] The concept of the current directory doesn''t exist in Windows CE');
  {$else}
  Result:=Windows.SetCurrentDirectoryW(PWidechar(UTF8Decode(NewDir)));
  {$endif}
end;

{------------------------------------------------------------------------------
  CreateDirUTF8
 ------------------------------------------------------------------------------}
{$ifndef WinCE}
function CreateDirAnsi(const NewDir: String): Boolean;
begin
  Result:=SysUtils.CreateDir(UTF8ToSys(NewDir));
end;
{$endif}

function CreateDirWide(const NewDir: String): Boolean;
begin
  Result:=Windows.CreateDirectoryW(PWideChar(UTF8Decode(NewDir)), nil);
end;

{------------------------------------------------------------------------------
  RemoveDirUTF8
 ------------------------------------------------------------------------------}
{$ifndef WinCE}
function RemoveDirAnsi(const Dir: String): Boolean;
begin
  Result:=SysUtils.RemoveDir(UTF8ToSys(Dir));
end;
{$endif}

function RemoveDirWide(const Dir: String): Boolean;
begin
  Result:=Windows.RemoveDirectoryW(PWideChar(UTF8Decode(Dir)));
end;

var
  FileSize_        : function (const Filename: string): int64 = @FileSizeWide;
  FindFirst_       : function (const Path: string; Attr: Longint;
                       out Rslt: TSearchRec): Longint = @FindFirstWide;
  FindNext_        : function (var Rslt: TSearchRec): Longint = @FindNextWide;
  FindClose_       : procedure (var F: TSearchrec) = @FindCloseWide;
  FileGetAttr_     : function (const FileName: String): Longint = @FileGetAttrWide;
  FileSetAttr_     : function (const Filename: String; Attr: longint): Longint = @FileSetAttrWide;
  DeleteFile_      : function (const FileName: String): Boolean = @DeleteFileWide;
  RenameFile_      : function (const OldName, NewName: String): Boolean = @RenameFileWide;
  GetCurrentDir_   : function : String = @GetCurrentDirWide;
  SetCurrentDir_   : function (const NewDir: String): Boolean = @SetCurrentDirWide;
  CreateDir_       : function (const NewDir: String): Boolean = @CreateDirWide;
  RemoveDir_       : function (const Dir: String): Boolean = @RemoveDirWide;

function FileSize(const Filename: string): int64;
begin
  Result:=FileSize_(FileName);
end;

function FindFirstUTF8(const Path: string; Attr: Longint; out Rslt: TSearchRec): Longint;
begin
  Result:=FindFirst_(Path, Attr, Rslt);
end;

function FindNextUTF8(var Rslt: TSearchRec): Longint;
begin
  Result:=FindNext_(Rslt);
end;

procedure FindCloseUTF8(var F: TSearchrec);
begin
  FindClose_(F);
end;

function DeleteFileUTF8(const FileName: String): Boolean;
begin
  Result:=DeleteFile_(FileName);
end;

function RenameFileUTF8(const OldName, NewName: String): Boolean;
begin
  Result:=RenameFile_(OldName, NewName);
end;

function GetCurrentDirUTF8: String;
begin
  Result:=GetCurrentDir_();
end;

function SetCurrentDirUTF8(const NewDir: String): Boolean;
begin
  Result:=SetCurrentDir_(NewDir);
end;

function CreateDirUTF8(const NewDir: String): Boolean;
begin
  Result:=CreateDir_(NewDir);
end;

function RemoveDirUTF8(const Dir: String): Boolean;
begin
  Result:=RemoveDir_(Dir);
end;

function FileGetAttrUTF8(const FileName: String): Longint;
begin
  Result:=FileGetAttr_(FileName);
end;

function FileSetAttrUTF8(const Filename: String; Attr: longint): Longint;
begin
  Result:=FileSetAttr_(Filename, Attr);
end;

function FileExistsUTF8(const Filename: string): boolean;
var
  Attr: Longint;
begin
  Attr:=FileGetAttrUTF8(FileName);
  if Attr <> -1 then
    Result:= (Attr and FILE_ATTRIBUTE_DIRECTORY) = 0
  else
    Result:=False;
end;

function DirectoryExistsUTF8(const Directory: string): boolean;
var
  Attr: Longint;
begin
  Attr:=FileGetAttrUTF8(Directory);
  if Attr <> -1 then
    Result:= (Attr and FILE_ATTRIBUTE_DIRECTORY) > 0
  else
    Result:=False;
end;

procedure InitFileUtils;
begin
  {$ifndef WinCE}
  if Win32MajorVersion <= 4 then
  begin
    FileSize_:=@FileSizeAnsi;
    FileGetAttr_:=@FileGetAttrAnsi;
    FileSetAttr_:=@FileSetAttrAnsi;
    DeleteFile_:=@DeleteFileAnsi;
    RenameFile_:=@RenameFileAnsi;
    SetCurrentDir_:=@SetCurrentDirAnsi;
    GetCurrentDir_:=@GetCurrentDirAnsi;
    CreateDir_:=@CreateDirAnsi;
    RemoveDir_:=@RemoveDirAnsi;
    FindFirst_:=@FindFirstAnsi;
    FindNext_:=@FindNextAnsi;
    FindClose_:=@FindCloseAnsi;
  end;
  {$endif}
end;
