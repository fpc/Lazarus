{%MainUnit lazfileutils.pas}

function FilenameIsAbsolute(const TheFilename: string):boolean;
begin
  Result:=FilenameIsUnixAbsolute(TheFilename);
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

function FileAgeUTF8(const FileName: string): int64;
begin
  Result:=SysUtils.FileAge(UTF8ToSys(Filename));
end;

function TUnicodeSearchRecHelper.UniversalTime: int64;
begin
  Result:=Time;
end;

function TRawbyteSearchRecHelper.UniversalTime: int64;
begin
  Result:=Time;
end;

function UniversalFileAgeUtf8(const FileName: string): int64; // -1 if not exists
begin
  Result := SysUtils.FileAge(UTF8ToSys(Filename));
end;

function FileSetDateUTF8(const FileName: String; Age: Longint): Longint;
begin
  Result := SysUtils.FileSetDate(UTF8ToSys(Filename), Age);
  InvalidateFileStateCache(Filename);
end;

function FileSizeUtf8(const Filename: string): int64;
var
  Info : TSearchRec;
begin
  if FindFirst(FileName,0,Info)=0 then
    begin
    Result:=Info.Size;
    FindClose(Info);
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

function GetUnixPhysicalFilename(const Filename: string;
  ExceptionOnError: boolean): string;
var
  OldPath: String;
  NewPath: String;
  p: PChar;
begin
  Result:=Filename;
  p:=PChar(Result);
  repeat
    while p^='/' do
      inc(p);
    if p^=#0 then exit;
    if p^<>'/' then
    begin
      repeat
        inc(p);
      until p^ in [#0,'/'];
      OldPath:=LeftStr(Result,p-PChar(Result));
      NewPath:=ReadAllLinks(OldPath,ExceptionOnError);
      if NewPath='' then exit('');
      if OldPath<>NewPath then
      begin
        Result:=NewPath+copy(Result,length(OldPath)+1,length(Result));
        p:=PChar(Result)+length(NewPath);
      end;
    end;
  until false;
  Result:=ResolveDots(Result);
end;

function GetPhysicalFilename(const Filename: string;
  OnError: TPhysicalFilenameOnError): string;
begin
  Result:=GetUnixPhysicalFilename(Filename,OnError=pfeException);
  if (Result='') and (OnError=pfeOriginal) then
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
  CurDir, HomeDir, Fn: String;
begin
  Fn := FileName;
  ForcePathDelims(Fn);
  IsAbs := FileNameIsUnixAbsolute(Fn);
  if (not IsAbs) then
  begin
    CurDir := GetCurrentDirUtf8;
    if ((Length(Fn) > 1) and (Fn[1] = '~') and (Fn[2] = '/')) or (Fn = '~') then
    begin
      HomeDir := GetEnvironmentVariableUTF8('HOME');
      if not FileNameIsUnixAbsolute(HomeDir) then
        HomeDir := ExpandFileNameUtf8(HomeDir,'');
      Fn := HomeDir + Copy(Fn,2,length(Fn));
      IsAbs := True;
    end;
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
    if not FileNameIsUnixAbsolute(Fn) then
      Fn := ExpandFileNameUtf8(Fn, '');
    Result := Fn;
  end;
end;

function GetCurrentDirUTF8: String;
begin
  Result:=SysToUTF8(SysUtils.GetCurrentDir);
end;

function FileIsExecutable(const AFilename: string): boolean;
begin
  Result:=False;
end;

procedure CheckIfFileIsExecutable(const AFilename: string);

begin
  raise Exception.Create('Not supported');
end;

function FileIsSymlink(const AFilename: string): boolean;
begin
  Result := False;
end;

procedure CheckIfFileIsSymlink(const AFilename: string);
var
  AText: string;
begin
  // to get good error messages consider the OS
  if not FileExistsUTF8(AFilename) then begin
    raise Exception.Create(Format(lrsFileDoesNotExist, [AFilename]));
  end;
  raise Exception.Create('Not supported');
end;

function FileIsHardLink(const AFilename: string): boolean;

begin
  Result := false;
end;

function FileIsReadable(const AFilename: string): boolean;
begin
  Result := True;
end;

function FileIsWritable(const AFilename: string): boolean;
begin
  Result := True;
end;


function IsUNCPath(const Path: String): Boolean;
begin
  Result := false;
end;

function ExtractUNCVolume(const Path: String): String;
begin
  Result := '';
end;


function GetPermissions(const AFilename: string): string;
var
  Info : TSearchRec;
  Attr : Longint;
begin
  Result := '-rw-rw-rw-';

  if FindFIrst(AFileName,faSymlink,Info)=0 then
    begin
    FindClose(Info);
    end
  else
    Exit('');  

  // file or directory permissions
  // file type
  
  Attr := info.Attr;
  if (Attr and faSymlink)<>0 then
    Result[1] := 'l'
  else
  if (Attr and faDirectory) <> 0 then
    Result[1] := 'd'
  else

  // user permissions
    Result[2] := 'r';
    Result[3] := 'w';
  //
    Result[5] := 'r';
    Result[6] := 'w';

  // other permissions
    Result[8] := 'r';
    Result[9] := 'w';
end;

function GetFileDescription(const AFilename: string): string;
var
  info: TsearchRec;
  // permissions
  // user
  // group
  // size
  // date
  // time
  // mode: mode_t;
begin
  Result := '';
  if not (FindFirst(AFilename,0,info{%H-})=0) then exit;

  // file type and permissions
  Result := GetPermissions(AFilename);

  // mode := info.st_mode;

  // user name
  //Result := Result + ' Owner: ' + IntToStr(info.uid) + '.' + IntToStr(info.gid);

  // size
  Result := Result + lrsSize + IntToStr(info.size);

  // date + time
  Result := Result + lrsModified;
  try
    Result := Result + FormatDateTime('DD/MM/YYYY hh:mm',
      FileDateToDateTime(FileAgeUTF8(AFilename)));
  except
    Result := Result + '?';
  end;
end;

function GetAppConfigDirUTF8(Global: Boolean; Create: boolean = false): string;
begin
  Result := SysToUTF8(SysUtils.GetAppConfigDir(Global));
  if Result = '' then exit;
  if Create and not ForceDirectoriesUTF8(Result) then
    raise EInOutError.Create(Format(lrsUnableToCreateConfigDirectoryS,[Result]));
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
    raise EInOutError.Create(Format(lrsUnableToCreateConfigDirectoryS,[Dir]));
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
