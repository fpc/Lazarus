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
var
  FileHandle: THandle;
  FileInfo: TBYHANDLEFILEINFORMATION;
begin
  FileHandle := FileOpen(UTF8ToSys(AFileName), fmOpenRead or fmShareDenyNone);
  if FileHandle <> feInvalidHandle then
  begin
    GetFileInformationByHandle(HFile(FileHandle), FileInfo);
    Result := FileInfo.nNumberOfLinks > 1;
    FileClose(FileHandle);
  end
  else
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
  Dst := AllocMem((Length(s) + 1) * SizeOf(Char));
  if OemToChar(PChar(s), Dst) then
    Result := StrPas(Dst)
  else
    Result := s;
  FreeMem(Dst);
  Result := SysToUTF8(Result);
end;

function UTF8ToConsole(const s: string): string;
var
  Dst: PChar;
begin
  Result := UTF8ToSys(s);
  Dst := AllocMem((Length(Result) + 1) * SizeOf(Char));
  if CharToOEM(PChar(Result), Dst) then
    Result := StrPas(Dst);
  FreeMem(Dst);
end;

{------------------------------------------------------------------------------
  FileSize
 ------------------------------------------------------------------------------}
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
function FindFirstAnsi(const Path: string; Attr: Longint; out Rslt: TSearchRec): Longint;
begin
  Result:=SysUtils.FindFirst(UTF8ToSys(Path),Attr,Rslt);
  Rslt.Name:=SysToUTF8(Rslt.Name);
end;

function WinToDosTime (Var Wtime : TFileTime;var DTime:longint):longbool;
var
  lft : TFileTime;
begin
  WinToDosTime:=FileTimeToLocalFileTime(WTime,lft) and
                FileTimeToDosDateTime(lft,Longrec(Dtime).Hi,LongRec(DTIME).lo);
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
  f.Name:=StrPas(@F.FindData.cFileName[0]);
  Result:=0;
end;

procedure FindWideToAnsi(const wide: TWIN32FINDDATAW; var ansi: TWIN32FINDDATA);
var
  ws : WideString;
  an : AnsiString;
begin
  SetLength(ws, length(wide.cAlternateFileName));
  Move(wide.cAlternateFileName[0], ws[1], length(ws)*2);
  an:=ws; // no need to utf8 for cAlternateFileName (it's always ansi encoded)
  Move(an[1], ansi.cAlternateFileName, sizeof(ansi.cAlternateFileName));

  ws:=PWideChar(@wide.cFileName[0]);
  an:=UTF8Encode(ws);
  ansi.cFileName:=an;
  if length(an)<length(ansi.cFileName) then ansi.cFileName[ length(an)]:=#0;

  with ansi do begin
    dwFileAttributes := wide.dwFileAttributes;
    ftCreationTime := wide.ftCreationTime;
    ftLastAccessTime := wide.ftLastAccessTime;
    ftLastWriteTime := wide.ftLastWriteTime;
    nFileSizeHigh := wide.nFileSizeHigh;
    nFileSizeLow := wide.nFileSizeLow;
    dwReserved0 := wide.dwReserved0;
    dwReserved1 := wide.dwReserved1;
  end;
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
function FindNextAnsi(var Rslt: TSearchRec): Longint;
begin
  Rslt.Name:=UTF8ToSys(Rslt.Name);
  Result:=SysUtils.FindNext(Rslt);
  Rslt.Name:=SysToUTF8(Rslt.Name);
end;

function FindNextWide(var Rslt: TSearchRec): Longint;
var
  wide  : TWIN32FINDDATAW;
begin
  if FindNextFileW(Rslt.FindHandle, wide) then begin
    FindWideToAnsi(wide, Rslt.FindData);
    Result := FindMatch(Rslt);
  end else
    Result := GetLastError;
end;

{------------------------------------------------------------------------------
  FindNextUTF8
 ------------------------------------------------------------------------------}
procedure FindCloseAnsi(var F: TSearchrec);
begin
  SysUtils.FindClose(F);
end;

procedure FindCloseWide(var F: TSearchrec);
begin
  Windows.FindClose(f.FindHandle);
end;


{------------------------------------------------------------------------------
  FileGetAttrUTF8
 ------------------------------------------------------------------------------}
function FileGetAttrAnsi(const FileName: String): Longint;
begin
  Result:=SysUtils.FileGetAttr(UTF8ToSys(Filename));
end;

function FileGetAttrWide(const FileName: String): Longint;
begin
  Result:=Windows.GetFileAttributesW(PWideChar(UTF8Decode(FileName)));
end;

{------------------------------------------------------------------------------
  FileSetAttrUTF8
 ------------------------------------------------------------------------------}
function FileSetAttrAnsi(const Filename: String; Attr: longint): Longint;
begin
  Result:=SysUtils.FileSetAttr(UTF8ToSys(Filename),Attr);
end;

function FileSetAttrWide(const Filename: String; Attr: longint): Longint;
begin
  if Windows.SetFileAttributesW(PWideChar(UTF8Decode(FileName)), Attr) then
    Result:=0
  else
    Result := Windows.GetLastError;
end;

{------------------------------------------------------------------------------
  DeleteFileUTF8
 ------------------------------------------------------------------------------}
function DeleteFileAnsi(const FileName: String): Boolean;
begin
  Result:=SysUtils.DeleteFile(UTF8ToSys(Filename));
end;

function DeleteFileWide(const FileName: String): Boolean;
begin
  Result:=Windows.DeleteFileW(PWideChar(UTF8Decode(FileName)));
end;

{------------------------------------------------------------------------------
  RenameFileUTF8
 ------------------------------------------------------------------------------}
function RenameFileAnsi(const OldName, NewName: String): Boolean;
begin
  Result:=SysUtils.RenameFile(UTF8ToSys(OldName),UTF8ToSys(NewName));
end;

function RenameFileWide(const OldName, NewName: String): Boolean;
begin
  Result:=MoveFileW(PWideChar(UTF8Decode(OldName)), PWideChar(UTF8Decode(NewName)));
end;

{------------------------------------------------------------------------------
  GetCurrentDirUTF8
 ------------------------------------------------------------------------------}
function GetCurrentDirAnsi: String;
begin
  Result:=SysToUTF8(SysUtils.GetCurrentDir);
end;

function GetCurrentDirWide: String;
var
  w   : WideString;
  res : Integer;
begin
  res:=GetCurrentDirectoryW(0, nil);
  SetLength(w, res);
  res:=Windows.GetCurrentDirectoryW(res, @w[1]);
  SetLength(w, res);
  Result:=UTF8Encode(w);
end;

{------------------------------------------------------------------------------
  SetCurrentDirUTF8
 ------------------------------------------------------------------------------}
function SetCurrentDirAnsi(const NewDir: String): Boolean;
begin
  Result:=SysUtils.SetCurrentDir(UTF8ToSys(NewDir));
end;

function SetCurrentDirWide(const NewDir: String): Boolean;
begin
  Result:=Windows.SetCurrentDirectoryW(PWidechar(UTF8Decode(NewDir)));
end;

{------------------------------------------------------------------------------
  CreateDirUTF8
 ------------------------------------------------------------------------------}
function CreateDirAnsi(const NewDir: String): Boolean;
begin
  Result:=SysUtils.CreateDir(UTF8ToSys(NewDir));
end;

function CreateDirWide(const NewDir: String): Boolean;
begin
  Result:=Windows.CreateDirectoryW(PWideChar(UTF8Decode(NewDir)), nil);
end;

{------------------------------------------------------------------------------
  RemoveDirUTF8
 ------------------------------------------------------------------------------}
function RemoveDirAnsi(const Dir: String): Boolean;
begin
  Result:=SysUtils.RemoveDir(UTF8ToSys(Dir));
end;

function RemoveDirWide(const Dir: String): Boolean;
begin
  Result:=Windows.RemoveDirectoryW(PWideChar(UTF8Decode(Dir)));
end;

var
  FileSize_        : function (const Filename: string): int64 = @FileSizeAnsi;
  FindFirst_       : function (const Path: string; Attr: Longint;
                       out Rslt: TSearchRec): Longint = @FindFirstAnsi;
  FindNext_        : function (var Rslt: TSearchRec): Longint = @FindNextAnsi;
  FindClose_       : procedure (var F: TSearchrec) = @FindCloseAnsi;
  FileGetAttr_     : function (const FileName: String): Longint = @FileGetAttrAnsi;
  FileSetAttr_     : function (const Filename: String; Attr: longint): Longint = @FileSetAttrAnsi;
  DeleteFile_      : function (const FileName: String): Boolean = @DeleteFileAnsi;
  RenameFile_      : function (const OldName, NewName: String): Boolean = @RenameFileAnsi;
  GetCurrentDir_   : function : String = @GetCurrentDirAnsi;
  SetCurrentDir_   : function (const NewDir: String): Boolean = @SetCurrentDirAnsi;
  CreateDir_       : function (const NewDir: String): Boolean = @CreateDirAnsi;
  RemoveDir_       : function (const Dir: String): Boolean = @RemoveDirAnsi;

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
  Attr:Dword;
begin
  Attr:=FileGetAttrUTF8(FileName);
  if Attr <> $ffffffff then
    Result:= (Attr and FILE_ATTRIBUTE_DIRECTORY) = 0
  else
    Result:=False;
end;

function DirectoryExistsUTF8(const Directory: string): boolean;
var
  Attr:Dword;
begin
  Attr:=FileGetAttrUTF8(Directory);
  if Attr <> $ffffffff then
    Result:= (Attr and FILE_ATTRIBUTE_DIRECTORY) > 0
  else
    Result:=False;
end;

procedure InitFileUtils;
begin
  if Win32MajorVersion > 4 then begin
    FileSize_:=@FileSizeWide;
    FileGetAttr_:=@FileGetAttrWide;
    FileSetAttr_:=@FileSetAttrWide;
    DeleteFile_:=@DeleteFileWide;
    RenameFile_:=@RenameFileWide;
    SetCurrentDir_:=@SetCurrentDirWide;
    GetCurrentDir_:=@GetCurrentDirWide;
    CreateDir_:=@CreateDirWide;
    RemoveDir_:=@RemoveDirWide;
    FindFirst_:=@FindFirstWide;
    FindNext_:=@FindNextWide;
    FindClose_:=@FindCloseWide;
  end;
end;
