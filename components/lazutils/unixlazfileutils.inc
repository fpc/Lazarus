{%MainUnit lazfileutils.pas}

function FilenameIsAbsolute(const TheFilename: string):boolean;
begin
  Result:=FilenameIsUnixAbsolute(TheFilename);
end;

function FileOpenUTF8(Const FileName : string; Mode : Integer) : THandle;
begin
  Result := SysUtils.FileOpen(UTF8ToSys(FileName), Mode);
end;

function FileCreateUTF8(Const FileName : string) : THandle;
begin
  Result := SysUtils.FileCreate(UTF8ToSys(FileName));
end;

function FileCreateUTF8(Const FileName : string; Rights: Cardinal) : THandle;
begin
  Result := SysUtils.FileCreate(UTF8ToSys(FileName), Rights);
end;

Function FileCreateUtf8(Const FileName : String; ShareMode : Integer; Rights : Cardinal) : THandle;
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

function FileAgeUTF8(const FileName: String): Longint;
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
  st: baseunix.stat;
  SysFileName: String;
begin
  SysFileName := Utf8ToSys(FileName);
  if not fpstat(pointer(SysFileName),st{%H-})>=0 then
    exit(-1);
  Result := st.st_size;
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

procedure FindCloseUTF8(var F: TSearchrec);
begin
  SysUtils.FindClose(F);
end;

function ExpandFileNameUtf8(const FileName: string; {const} BaseDir: String = ''): String;
var
  IsAbs: Boolean;
  CurDir, HomeDir, Fn: String;
begin
  Fn := FileName;
  DoDirSeparators(Fn);
  IsAbs := FileNameIsUnixAbsolute(Fn);
  if (not IsAbs) then
  begin
    CurDir := GetCurrentDirUtf8;
    if ((Length(Fn) > 1) and (Fn[1] = '~') and (Fn[2] = '/')) or (Fn = '~') then
    begin
      {$Hint use GetEnvironmentVariableUTF8}
      HomeDir := SysToUtf8(GetEnvironmentVariable('HOME'));
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


procedure InitLazFileUtils;
begin
  //dummy
end;
