{%MainUnit lazfileutils.pas}

function FilenameIsAbsolute(const TheFilename: string):boolean;
begin
  Result := Pos(':', TheFilename) > 1;
end;

function FileOpenUTF8(const FileName: string; Mode: Integer): THandle;
begin
  Result := SysUtils.FileOpen(UTF8ToSys(FileName), Mode);
end;

function FileCreateUTF8(const FileName: string): THandle;
begin
  Result := SysUtils.FileCreate(UTF8ToSys(FileName));
end;

function FileCreateUTF8(const FileName: string; Rights: Cardinal): THandle;
begin
  Result := SysUtils.FileCreate(UTF8ToSys(FileName), Rights);
end;

function FileCreateUtf8(const FileName: String; ShareMode: Integer;
  Rights: Cardinal): THandle;
begin
  Result := SysUtils.FileCreate(UTF8ToSys(FileName), ShareMode, Rights);
end;

function FileGetAttrUTF8(const FileName: String): Longint;
begin
  Result:=SysUtils.FileGetAttr(UTF8ToSys(Filename));
end;

function FileSetAttrUTF8(const Filename: String; Attr: longint): Longint;
begin
  Result:=SysUtils.FileSetAttr(UTF8ToSys(Filename),Attr);
  InvalidateFileStateCache(Filename);
end;

function FileExistsUTF8(const Filename: string): boolean;
begin
  Result:=SysUtils.FileExists(UTF8ToSys(Filename));
end;

function DirectoryExistsUTF8(const Directory: string): Boolean;
begin
  Result:=SysUtils.DirectoryExists(UTF8ToSys(Directory));
end;

function FileAgeUTF8(const FileName: string): Longint;
begin
  Result:=SysUtils.FileAge(UTF8ToSys(Filename));
end;

function FileSetDateUTF8(const FileName: String; Age: Longint): Longint;
begin
  Result := SysUtils.FileSetDate(UTF8ToSys(Filename), Age);
  InvalidateFileStateCache(Filename);
end;

function FileSizeUtf8(const Filename: string): int64;
var
  Info: TSearchRec;
  Str: AnsiString;
begin
  Result := 0;
  Str := Utf8ToAnsi(Filename);
  if SysUtils.FindFirst (str, faAnyFile and faDirectory, Info) = 0 then
  begin
    Result := Info.Size;
  end;
  SysUtils.FindClose(Info);
end;

{------------------------------------------------------------------------------
  function ReadAllLinks(const Filename: string;
    ExceptionOnError: boolean): string;
 ------------------------------------------------------------------------------}
function ReadAllLinks(const Filename: string;
  ExceptionOnError: boolean): string;
begin
    Result:='';
end;

function GetPhysicalFilename(const Filename: string;
  OnError: TPhysicalFilenameOnError): string;
begin
  Result:=Filename;
end;

function CreateDirUTF8(const NewDir: String): Boolean;
begin
  Result:=SysUtils.CreateDir(UTF8ToSys(NewDir));
end;

function RemoveDirUTF8(const Dir: String): Boolean;
begin
  Result:=SysUtils.RemoveDir(UTF8ToSys(Dir));
end;

function DeleteFileUTF8(const FileName: String): Boolean;
begin
  Result:=SysUtils.DeleteFile(UTF8ToSys(Filename));
  if Result then
    InvalidateFileStateCache;
end;

function RenameFileUTF8(const OldName, NewName: String): Boolean;
begin
  Result:=SysUtils.RenameFile(UTF8ToSys(OldName),UTF8ToSys(NewName));
  if Result then
    InvalidateFileStateCache;
end;

function SetCurrentDirUTF8(const NewDir: String): Boolean;
begin
  Result:=SysUtils.SetCurrentDir(UTF8ToSys(NewDir));
end;

function FindFirstUTF8(const Path: string; Attr: Longint; out Rslt: TSearchRec
  ): Longint;
begin
  Result:=SysUtils.FindFirst(UTF8ToSys(Path),Attr,Rslt);
  Rslt.Name:=SysToUTF8(Rslt.Name);
end;

function FindNextUTF8(var Rslt: TSearchRec): Longint;
begin
  Rslt.Name:=UTF8ToSys(Rslt.Name);
  Result:=SysUtils.FindNext(Rslt);
  Rslt.Name:=SysToUTF8(Rslt.Name);
end;


function ExpandFileNameUTF8(const FileName: string; BaseDir: string): string;
var
  IsAbs: Boolean;
  CurDir, Fn: String;
begin
  Fn := FileName;
  ForcePathDelims(Fn);
  IsAbs := FileNameIsAbsolute(Fn);
  if (not IsAbs) then
  begin
    CurDir := GetCurrentDirUtf8;
  end;
  if IsAbs then
  begin
    Result := ResolveDots(Fn);
  end
  else
  begin
    if (BaseDir = '') then
      Fn := IncludeTrailingPathDelimiter(CurDir) + Fn
    else
      Fn := IncludeTrailingPathDelimiter(BaseDir) + Fn;
    Fn := ResolveDots(Fn);
    //if BaseDir is not absolute then this needs to be expanded as well
    if not FileNameIsAbsolute(Fn) then
      Fn := ExpandFileNameUtf8(Fn, '');
    Result := Fn;
  end;
end;

function GetCurrentDirUTF8: String;
begin
  Result:=SysToUTF8(SysUtils.GetCurrentDir);
end;

function FileIsExecutable(const AFilename: string): boolean;
var
  Fn: string;
  MyLock: BPTR;
  Info: TFileInfoBlock;
begin
  Result := False;
  Fn := Utf8ToSys(AFilename);
  MyLock := AmigaDos.Lock(PChar(Fn), SHARED_LOCK);
  if PtrUInt(MyLock) <> 0 then
  begin
    Examine(MyLock, @Info);
    Result := (Info.fib_Protection and FIBF_EXECUTE) <> 0;
    AmigaDos.UnLock(MyLock);
  end;
end;

procedure CheckIfFileIsExecutable(const AFilename: string);
begin
  // TProcess does not report, if a program can not be executed
  // to get good error messages consider the OS
  if not FileExistsUTF8(AFilename) then begin
    raise Exception.Create(SysUtils.Format(lrsFileDoesNotExist, [AFilename]));
  end;
  if DirPathExists(AFilename) then begin
    raise Exception.Create(SysUtils.Format(lrsFileIsADirectoryAndNotAnExecutable, [
      AFilename]));
  end;
end;

function FileIsSymlink(const AFilename: string): boolean;
begin
  Result := False;
end;

procedure CheckIfFileIsSymlink(const AFilename: string);
begin
  // to get good error messages consider the OS
  if not FileExistsUTF8(AFilename) then begin
    raise Exception.Create(SysUtils.Format(lrsFileDoesNotExist, [AFilename]));
  end;
  if not FileIsSymLink(AFilename) then
    raise Exception.Create(SysUtils.Format(lrsIsNotASymbolicLink, [AFilename]));
end;

function FileIsHardLink(const AFilename: string): boolean;
begin
  Result := false;
end;

function FileIsReadable(const AFilename: string): boolean;
var
  Fn: string;
  MyLock: BPTR;
  Info: TFileInfoBlock;
begin
  Result := False;
  Fn := Utf8ToSys(AFilename);
  MyLock := AmigaDos.Lock(PChar(Fn), SHARED_LOCK);
  if PtrUInt(MyLock) <> 0 then
  begin
    Examine(MyLock, @Info);
    Result := (Info.fib_Protection and FIBF_READ) <> 0;
    AmigaDos.UnLock(MyLock);
  end;
end;

function FileIsWritable(const AFilename: string): boolean;
var
  Fn: string;
  MyLock: BPTR;
  Info: TFileInfoBlock;
begin
  Result := False;
  Fn := Utf8ToSys(AFilename);
  MyLock := AmigaDos.Lock(PChar(Fn), SHARED_LOCK);
  if PtrUInt(MyLock) <> 0 then
  begin
    Examine(MyLock, @Info);
    Result := (Info.fib_Protection and FIBF_WRITE) <> 0;
    AmigaDos.UnLock(MyLock);
  end;
end;


function IsUNCPath(const Path: String): Boolean;
begin
  Result := false;
end;

function ExtractUNCVolume(const Path: String): String;
begin
  Result := '';
end;

function GetFileDescription(const AFilename: string): string;
var
  Fn: string;
  MyLock: BPTR;
  Info: TFileInfoBlock;
begin
  Result := '';
  Fn := Utf8ToSys(AFilename);
  MyLock := AmigaDos.Lock(PChar(Fn), SHARED_LOCK);
  if PtrUInt(MyLock) <> 0 then
  begin
    Examine(MyLock, @Info);
    if (Info.fib_Protection and FIBF_ARCHIVE) <> 0 then
      Result := Result + 'a';
    if (Info.fib_Protection and FIBF_SCRIPT) <> 0 then
      Result := Result + 's';
    if (Info.fib_Protection and FIBF_PURE) <> 0 then
      Result := Result + 'p';
    if (Info.fib_Protection and FIBF_EXECUTE) <> 0 then
      Result := Result + 'e';
    if (Info.fib_Protection and FIBF_READ) <> 0 then
      Result := Result + 'r';
    if (Info.fib_Protection and FIBF_WRITE) <> 0 then
      Result := Result + 'w';
    if (Info.fib_Protection and FIBF_DELETE) <> 0 then
      Result := Result + 'd';
    AmigaDos.UnLock(MyLock);
  end;
end;


function GetAppConfigDirUTF8(Global: Boolean; Create: boolean = false): string;
begin
  Result := SysToUTF8(SysUtils.GetAppConfigDir(Global));
  if Result = '' then exit;
  if Create and not ForceDirectoriesUTF8(Result) then
    raise EInOutError.Create(SysUtils.Format(lrsUnableToCreateConfigDirectoryS,[Result]));
end;

function GetAppConfigFileUTF8(Global: Boolean; SubDir: boolean;
  CreateDir: boolean): string;
var
  Dir: string;
begin
  Result := SysToUTF8(SysUtils.GetAppConfigFile(Global,SubDir));
  if not CreateDir then exit;
  Dir := ExtractFilePath(Result);
  if Dir = '' then exit;
  if not ForceDirectoriesUTF8(Dir) then
    raise EInOutError.Create(SysUtils.Format(lrsUnableToCreateConfigDirectoryS,[Dir]));
end;

function GetShellLinkTarget(const FileName: string): string;
begin
  Result := Filename;
end;

procedure InitLazFileUtils;
begin
  //dummy
end;

procedure FinalizeLazFileUtils;
begin
  //dummy
end;
