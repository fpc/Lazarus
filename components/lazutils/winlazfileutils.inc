{%MainUnit lazfileutils.pas}


var
//procedural variables for procedures that are implemented different on Win9x and NT or WinCE platform
//They are intialized in InitLazFileUtils
  _FileAgeUtf8         : function (const Filename:string):Longint;
  _FileSizeUtf8        : function (const Filename: string): int64;
  _FileSetDateUtf8   : function (const FileName: String; Age: Longint): Longint;
  //FindFirst_       : function (const Path: string; Attr: Longint;
  //                     out Rslt: TSearchRec): Longint;
  //FindNext_        : function (var Rslt: TSearchRec): Longint;
  //FindClose_       : procedure (var F: TSearchrec);
  _FileGetAttrUtf8   : function (const FileName: String): Longint;
  _FileSetAttrUtf8   : function (const Filename: String; Attr: longint): Longint;
  //DeleteFile_      : function (const FileName: String): Boolean;
  //RenameFile_      : function (const OldName, NewName: String): Boolean;
  _GetCurrentDirUtf8  : function: String ;
  _GetDirUtf8         : procedure(DriveNr: Byte; var Dir: String);
  _FileOpenUtf8       : function(Const FileName : string; Mode : Integer) : THandle;
  _FileCreateUtf8     : function(Const FileName : String; ShareMode : Integer; Rights: Integer) : THandle;
  //SetCurrentDir_   : function (const NewDir: String): Boolean;
  //CreateDir_       : function (const NewDir: String): Boolean;
  //RemoveDir_       : function (const Dir: String): Boolean ;

function FilenameIsAbsolute(const TheFilename: string):boolean;
begin
  Result:=FilenameIsWinAbsolute(TheFilename);
end;




function ExpandFileNameUtf8(const FileName: string; {const} BaseDir: String = ''): String;
var
  IsAbs, StartsWithRoot, CanUseBaseDir : Boolean;
  {$ifndef WinCE}
  HasDrive: Boolean;
  FnDrive, CurDrive, BaseDirDrive: Char;
  {$endif}
  CurDir, Fn: String;
begin
  //writeln('LazFileUtils.ExpandFileNameUtf8');
  //writeln('FileName = "',FileName,'"');
  //writeln('BaseDir  = "',BaseDir,'"');

  //{$ifndef WinCE}
  //if (Win32Platform = VER_PLATFORM_WIN32_WINDOWS) then
  //  Result := SysToUtf8(SysUtils.ExpandFileName(Utf8ToSys(FileName)))
  //else
  //{$endif}
  Fn := FileName;
  DoDirSeparators(Fn);
  IsAbs := FileNameIsWinAbsolute(Fn);
  if not IsAbs then
  begin
    StartsWithRoot := (Fn = '\') or
                      ((Length(Fn) > 1) and
                      (Fn[1] = DirectorySeparator) and
                      (Fn[2] <> DirectorySeparator));
    {$ifndef WinCE}
    HasDrive := (Length(Fn) > 1) and
                (Fn[2] = ':') and
                (UpCase(Fn[1]) in ['A'..'Z']);

    if HasDrive then
    begin
      FnDrive := UpCase(Fn[1]);
      _GetDirUtf8(Byte(FnDrive)-64, CurDir);
      CurDrive := UpCase(GetCurrentDirUtf8[1]);
    end
    else
    begin
      CurDir := GetCurrentDirUtf8;
      FnDrive := UpCase(CurDir[1]);
      CurDrive := FnDrive;
    end;

    //writeln('HasDrive = ',HasDrive,' Fn = ',Fn);
    //writeln('CurDir = ',CurDir);
    //writeln('CurDrive = ',CurDrive);
    //writeln('FnDrive  = ',FnDrive);

    if (Length(BaseDir) > 1) and (UpCase(BaseDir[1]) in ['A'..'Z']) and (BaseDir[2] = ':') then
    begin
      BaseDirDrive := BaseDir[1]
    end
    else
    begin
      if HasDrive then
        BaseDirDrive := CurDrive
      else
        BaseDirDrive := #0;
    end;

    //You cannot use BaseDir if both FileName and BaseDir includes a drive and they are not the same
    CanUseBaseDir := ((BaseDirDrive = #0) or
                     (not HasDrive) or
                     (HasDrive and (FnDrive = BaseDirDrive)))
                     and (BaseDir <> '');

    //writeln('CanUseBaseDir = ',CanUseBaseDir);

    if not HasDrive and StartsWithRoot and not CanUseBaseDir then
    begin
      //writeln('HasDrive and StartsWithRoot');
      Fn := Copy(CurDir,1,2) + Fn;
      HasDrive := True;
      IsAbs := True;
    end;
    //FileNames like C:foo, strip Driveletter + colon
    if HasDrive and not IsAbs then Delete(Fn,1,2);

    //writeln('HasDrive = ',Hasdrive,' Fn = ',Fn);
    {$else}
    CanUseBaseDir := True;
    {$endif WinCE}
  end;
  if IsAbs then
  begin
    //writeln('IsAbs = True -> Exit');
    Result := ResolveDots(Fn);
  end
  else
  begin
    if not CanUseBaseDir or (BaseDir = '') then
      Fn := IncludeTrailingPathDelimiter(CurDir) + Fn
    else
    begin
      if (Fn[1] = DirectorySeparator) then Delete(Fn,1,1);
      Fn := IncludeTrailingPathDelimiter(BaseDir) + Fn;
    end;

    Fn := ResolveDots(Fn);
    //if BaseDir is something like 'z:foo\' or '\' then this needs to be expanded as well
    if not FileNameIsAbsolute(Fn) then
      Fn := ExpandFileNameUtf8(Fn, '');
    Result := Fn;
  end;
end;


function GetCurrentDirUTF8: String;
begin
  Result:=_GetCurrentDirUtf8();
end;

procedure GetDirUtf8(DriveNr: Byte; var Dir: String);
begin
  _GetDirUtf8(DriveNr, Dir);
end;

function FileOpenUTF8(Const FileName : string; Mode : Integer) : THandle;
begin
  Result := _FileOpenUtf8(FileName, Mode);
end;

function FileCreateUTF8(Const FileName : string) : THandle;
begin
  Result := _FileCreateUtf8(FileName, fmShareExclusive, 0);
end;

function FileCreateUTF8(Const FileName : string; Rights: Cardinal) : THandle;
begin
  Result := _FileCreateUtf8(FileName, fmShareExclusive, Rights);
end;

Function FileCreateUtf8(Const FileName : String; ShareMode : Integer; Rights : Cardinal) : THandle;
begin
  Result := _FileCreateUtf8(FileName, ShareMode, Rights);
end;

function FileGetAttrUTF8(const FileName: String): Longint;
begin
  Result := _FileGetAttrUtf8(FileName);
end;

function FileSetAttrUTF8(const Filename: String; Attr: longint): Longint;
begin
  Result := _FileSetAttrUtf8(Filename, Attr);
  InvalidateFileStateCache(Filename);
end;

function FileExistsUTF8(const Filename: string): boolean;
var
  Attr: Longint;
begin
  Attr := _FileGetAttrUTF8(FileName);
  if Attr <> -1 then
    Result:= (Attr and FILE_ATTRIBUTE_DIRECTORY) = 0
  else
    Result:=False;
end;

function DirectoryExistsUTF8(const Directory: string): boolean;
var
  Attr: Longint;
begin
  Attr := _FileGetAttrUTF8(Directory);
  if Attr <> -1 then
    Result := (Attr and FILE_ATTRIBUTE_DIRECTORY) > 0
  else
    Result := False;
end;

function FileAgeUTF8(const FileName: String): Longint;
begin
  Result := _FileAgeUtf8(FileName);
end;

function FileSetDateUTF8(const FileName: String; Age: Longint): Longint;
begin
  Result := _FileSetDateUtf8(Filename, Age);
  InvalidateFileStateCache(Filename);
end;

function FileSizeUtf8(const Filename: string): int64;
begin
  Result := _FileSizeUtf8(FileName);
end;


{******* ANSI functions *******}

{$ifndef WinCE}
//No ANSII functions on WinCE
function GetCurrentDirAnsi: String;
begin
  Result:=SysToUTF8(SysUtils.GetCurrentDir);
end;

procedure GetDirAnsi(DriveNr: Byte; var Dir: String);
begin
  GetDir(DriveNr, Dir);
  Dir := SysToUtf8(Dir);
end;


function FileOpenAnsi(Const FileName : string; Mode : Integer) : THandle;
begin
  Result := FileOpen(UTF8ToSys(FileName), Mode);
  //if fail api return feInvalidHandle (INVALIDE_HANDLE=feInvalidHandle=-1)
end;


function FileCreateAnsi(Const FileName : string; ShareMode: Integer; Rights: Integer) : THandle;
begin
  Result := FileCreate(Utf8ToSys(FileName), Sharemode, Rights);
end;

function FileGetAttrAnsi(const FileName: String): Longint;
begin
  Result:=SysUtils.FileGetAttr(UTF8ToSys(Filename));
end;

function FileSetAttrAnsi(const Filename: String; Attr: longint): Longint;
begin
  Result:=SysUtils.FileSetAttr(UTF8ToSys(Filename),Attr);
end;


function FileAgeAnsi(const FileName: String): Longint;
begin
  Result := SysUtils.FileAge(UTF8ToSys(Filename));
end;

function FileSetDateAnsi(const FileName: String; Age: Longint): Longint;
begin
 Result := SysUtils.FileSetDate(UTF8ToSys(Filename), Age);
end;

function FileSizeAnsi(const Filename: string): int64;
var
  FindData: TWIN32FindDataA;
  FindHandle: THandle;
  Str: AnsiString;
begin
  Str := Utf8ToAnsi(Filename);
  FindHandle := Windows.FindFirstFileA(PAnsiChar(Str), FindData);
  if FindHandle = Windows.Invalid_Handle_value then
  begin
    Result := -1;
    exit;
  end;
  Result := (int64(FindData.nFileSizeHigh) shl 32) + FindData.nFileSizeLow;
  Windows.FindClose(FindHandle);
end;



{$endif WinCE}




{******* Wide functions *******}
const
  ShareModes: array[0..4] of Integer = (
               0,
               0,
               FILE_SHARE_READ,
               FILE_SHARE_WRITE,
               FILE_SHARE_READ or FILE_SHARE_WRITE);

  AccessModes: array[0..2] of Cardinal  = (
    GENERIC_READ,
    GENERIC_WRITE,
    GENERIC_READ or GENERIC_WRITE);

function WinToDosTime(Var Wtime : TFileTime; var DTime:longint):longbool;
var
  lft : TFileTime;
begin
  WinToDosTime:=FileTimeToLocalFileTime(WTime,lft)
    {$ifndef WinCE}
    and FileTimeToDosDateTime(lft,Longrec(Dtime).Hi,LongRec(DTIME).lo)
    {$endif}
    ;
end;

Function DosToWinTime(DosTime:longint; Var Wintime : TFileTime):longbool;
var
 lft : TFileTime;
begin
 DosToWinTime:=
   {$ifndef wince}
   DosDateTimeToFileTime(longrec(DosTime).hi,longrec(DosTime).lo,@lft) and
   {$endif}
   LocalFileTimeToFileTime(lft,Wintime);                                        ;
end;

function GetCurrentDirWide: String;
var
  w   : WideString;
  res : Integer;
begin
  {$ifdef WinCE}
  Result := '\';
  // Previously we sent an exception here, which is correct, but this causes
  // trouble with code which isnt tested for WinCE, so lets just send a dummy result instead
  // Exception.Create('[GetCurrentDirWide] The concept of the current directory doesn''t exist in Windows CE');
  {$else}
  res:=GetCurrentDirectoryW(0, nil);
  SetLength(w, res);
  res:=Windows.GetCurrentDirectoryW(res, @w[1]);
  SetLength(w, res);
  Result:=UTF8Encode(w);
  {$endif}
end;

procedure GetDirWide(DriveNr: Byte; var Dir: String);
{This procedure may not be threadsafe, because SetCurrentDirectory isn't}
var
  w, D: WideString;
  SavedDir: WideString;
  res : Integer;
begin
  {$ifdef WinCE}
  Dir := '\';
  // Previously we sent an exception here, which is correct, but this causes
  // trouble with code which isnt tested for WinCE, so lets just send a dummy result instead
  // Exception.Create('[GetCurrentDirWide] The concept of the current directory doesn''t exist in Windows CE');
  {$else}
  //writeln('GetDirWide START');
  if not (DriveNr = 0) then
  begin
    res := GetCurrentDirectoryW(0, nil);
    SetLength(SavedDir, res);
    res:=Windows.GetCurrentDirectoryW(res, @SavedDir[1]);
    SetLength(SavedDir,res);

    D := WideChar(64 + DriveNr) + ':';
    if not SetCurrentDirectoryW(@D[1]) then
    begin
      Dir := Char(64 + DriveNr) + ':\';
      SetCurrentDirectoryW(@SavedDir[1]);
      Exit;
    end;
  end;
  res := GetCurrentDirectoryW(0, nil);
  SetLength(w, res);
  res := GetCurrentDirectoryW(res, @w[1]);
  SetLength(w, res);
  Dir:=UTF8Encode(w);
  if not (DriveNr = 0) then SetCurrentDirectoryW(@SavedDir[1]);
  //writeln('GetDirWide END');
  {$endif}
end;


function FileOpenWide(Const FileName : string; Mode : Integer) : THandle;

begin
  Result := CreateFileW(PWideChar(UTF8Decode(FileName)), dword(AccessModes[Mode and 3]),
                         dword(ShareModes[(Mode and $F0) shr 4]), nil, OPEN_EXISTING,
                         FILE_ATTRIBUTE_NORMAL, 0);
  //if fail api return feInvalidHandle (INVALIDE_HANDLE=feInvalidHandle=-1)
end;


function FileCreateWide(Const FileName : string;  ShareMode: Integer; Rights: Integer) : THandle;
begin
  Result := CreateFileW(PWideChar(UTF8Decode(FileName)), GENERIC_READ or GENERIC_WRITE,
                       dword(ShareModes[(ShareMode and $F0) shr 4]), nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
end;


function FileGetAttrWide(const FileName: String): Longint;
begin
  Result:=Integer(Windows.GetFileAttributesW(PWideChar(UTF8Decode(FileName))));
end;

function FileSetAttrWide(const Filename: String; Attr: longint): Longint;
begin
  if Windows.SetFileAttributesW(PWideChar(UTF8Decode(FileName)), Attr) then
    Result:=0
  else
    Result := Integer(Windows.GetLastError);
end;

function FileAgeWide(const FileName: String): Longint;
var
  Hnd: THandle;
  FindData: TWin32FindDataW;
begin
  Hnd := FindFirstFileW(PWideChar(UTF8ToUTF16(FileName)), FindData);
   if Hnd <> INVALID_HANDLE_VALUE then
    begin
      Windows.FindClose(Hnd);
      if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
        If WinToDosTime(FindData.ftLastWriteTime,Result) then
          exit;
    end;
   Result := -1;
end;

function FileSetDateWide(const FileName: String; Age: Longint): Longint;
var
 FT:TFileTime;
 fh: HANDLE;
begin
   try
     fh := CreateFileW(PWideChar(UTF8ToUTF16(FileName)),
                       FILE_WRITE_ATTRIBUTES,
                       0, nil, OPEN_EXISTING,
                       FILE_ATTRIBUTE_NORMAL, 0);
     if (fh <> feInvalidHandle) and (DosToWinTime(Age,FT) and SetFileTime(fh, nil, nil, @FT)) then
       Result := 0
     else
       Result := GetLastError;
   finally
     if (fh <> feInvalidHandle) then FileClose(fh);
   end;
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
  FindHandle := Windows.FindFirstFileW(PWideChar(Str), FindData);
  if FindHandle = Windows.Invalid_Handle_value then
  begin
    Result := -1;
    exit;
  end;
  Result := (int64(FindData.nFileSizeHigh) shl 32) + FindData.nFileSizeLow;
  Windows.FindClose(FindHandle);
end;




procedure InitLazFileUtils;
begin
  {$ifndef WinCE}
  if Win32MajorVersion <= 4 then
  begin
    _FileAgeUtf8 := @FileAgeAnsi;
    _FileSizeUtf8 := @FileSizeAnsi;
    _FileSetDateUtf8 := @FileSetDateAnsi;
    _FileGetAttrUtf8 := @FileGetAttrAnsi;
    _FileSetAttrUtf8 := @FileSetAttrAnsi;
    //DeleteFile_ := @DeleteFileAnsi;
    //RenameFile_ := @RenameFileAnsi;
    //SetCurrentDir_ := @SetCurrentDirAnsi;
    _GetCurrentDirUtf8 := @GetCurrentDirAnsi;
    _GetDirUtf8 := @GetDirAnsi;
    _FileOpenUtf8 := @FileOpenAnsi;
    _FileCreateUtf8 := @FileCreateAnsi;
    //CreateDir_ := @CreateDirAnsi;
    //RemoveDir_ := @RemoveDirAnsi;
    //FindFirst_ := @FindFirstAnsi;
    //FindNext_ := @FindNextAnsi;
    //FindClose_ := @FindCloseAnsi;
  end
  else
  {$endif}
  begin
    _FileAgeUtf8 := @FileAgeWide;
    _FileSizeUtf8 := @FileSizeWide;
    _FileSetDateUtf8 := @FileSetDateWide;
    _FileGetAttrUtf8 := @FileGetAttrWide;
    _FileSetAttrUtf8 := @FileSetAttrWide;
    //DeleteFile_ := @DeleteFileWide;
    //RenameFile_ := @RenameFileWide;
    //SetCurrentDir_ := @SetCurrentDirWide;
    _GetCurrentDirUtf8 :=@ GetCurrentDirWide;
    _GetDirUtf8 := @GetDirWide;
    _FileOpenUtf8 := @FileOpenWide;
    _FileCreateUtf8 := @FileCreateWide;
    //CreateDir_ := @CreateDirWide;
    //RemoveDir_ := @RemoveDirWide;
    //FindFirst_ := @FindFirstWide;
    //FindNext_ := @FindNextWide;
    //FindClose_ := @FindCloseWide;
  end;
end;
