{%MainUnit lazfileutils.pas}


var
//procedural variables for procedures that are implemented different on Win9x and NT or WinCE platform
//They are intialized in InitLazFileUtils
  _FileAgeUtf8         : function(const Filename:string):Longint;
  _FileSizeUtf8        : function(const Filename: string): int64;
  _FileSetDateUtf8     : function(const FileName: String; Age: Longint): Longint;
  _FindFirstUtf8       : function(const Path: string; Attr: Longint;
                                  out Rslt: TSearchRec): Longint;
  _FindNextUtf8        : function(var Rslt: TSearchRec): Longint;
  _FileGetAttrUtf8     : function(const FileName: String): Longint;
  _FileSetAttrUtf8     : function(const Filename: String; Attr: longint): Longint;
  _DeleteFileUtf8      : function(const FileName: String): Boolean;
  _RenameFileUtf8      : function(const OldName, NewName: String): Boolean;
  _GetCurrentDirUtf8   : function: String ;
  _GetDirUtf8          : procedure(DriveNr: Byte; var Dir: String);
  _FileOpenUtf8        : function(Const FileName : string; Mode : Integer) : THandle;
  _FileCreateUtf8      : function(Const FileName : String; ShareMode : Integer; Rights: Integer) : THandle;
  _SetCurrentDirUtf8   : function(const NewDir: String): Boolean;
  _CreateDirUtf8       : function(const NewDir: String): Boolean;
  _RemoveDirUtf8       : function(const Dir: String): Boolean ;
  _GetAppConfigDirUTF8 : function(Global: Boolean; Create: boolean = false): string;
  _GetAppConfigFileUTF8: function(Global: Boolean; SubDir: boolean = false;CreateDir: boolean = false): string;




// ************* "Stubs" that just call Ansi or WideString routines ***********************


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

function CreateDirUTF8(const NewDir: String): Boolean;
begin
  Result := _CreateDirUTF8(NewDir);
end;

function RemoveDirUTF8(const Dir: String): Boolean;
begin
  Result := _RemoveDirUtf8(Dir);
end;

function DeleteFileUTF8(const FileName: String): Boolean;
begin
  Result := _DeleteFileUtf8(Filename);
  if Result then
    InvalidateFileStateCache;
end;

function RenameFileUTF8(const OldName, NewName: String): Boolean;
begin
  Result := _RenameFileUtf8(OldName,NewName);
  if Result then
    InvalidateFileStateCache;
end;

function SetCurrentDirUTF8(const NewDir: String): Boolean;
begin
  Result := _SetCurrentDirUtf8(NewDir);
end;

function FindFirstUTF8(const Path: string; Attr: Longint; out Rslt: TSearchRec
  ): Longint;
begin
  Result := _FindFirstUtf8(Path, Attr, Rslt);
end;

function FindNextUTF8(var Rslt: TSearchRec): Longint;
begin
  Result := _FindNextUtf8(Rslt);
end;

function GetAppConfigDirUTF8(Global: Boolean; Create: boolean = false): string;
begin
  Result := _GetAppConfigDirUtf8(Global, Create);
end;

function GetAppConfigFileUTF8(Global: Boolean; SubDir: boolean;
  CreateDir: boolean): string;
begin
  Result := _GetAppConfigFileUTF8(Global, SubDir, CreateDir);
end;

// ******** Start of AnsiString specific implementations ************

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

function CreateDirAnsi(const NewDir: String): Boolean;
begin
  Result:=SysUtils.CreateDir(UTF8ToSys(NewDir));
end;

function RemoveDirAnsi(const Dir: String): Boolean;
begin
  Result:=SysUtils.RemoveDir(UTF8ToSys(Dir));
end;

function DeleteFileAnsi(const FileName: String): Boolean;
begin
  Result:=SysUtils.DeleteFile(UTF8ToSys(Filename));
end;

function RenameFileAnsi(const OldName, NewName: String): Boolean;
begin
  Result:=SysUtils.RenameFile(UTF8ToSys(OldName),UTF8ToSys(NewName));
end;

function SetCurrentDirAnsi(const NewDir: String): Boolean;
begin
  Result:=SysUtils.SetCurrentDir(UTF8ToSys(NewDir));
end;

function FindFirstAnsi(const Path: string; Attr: Longint; out Rslt: TSearchRec): Longint;
begin
  Result:=SysUtils.FindFirst(UTF8ToSys(Path),Attr,Rslt);
  Rslt.Name:=SysToUTF8(Rslt.Name);
end;

function FindNextAnsi(var Rslt: TSearchRec): Longint;
begin
  Rslt.Name:=UTF8ToSys(Rslt.Name);
  Result:=SysUtils.FindNext(Rslt);
  Rslt.Name:=SysToUTF8(Rslt.Name);
end;

function GetAppConfigDirAnsi(Global: Boolean; Create: boolean = false): string;
begin
  Result := SysToUTF8(SysUtils.GetAppConfigDir(Global));
  if Result = '' then exit;
  if Create and not ForceDirectoriesUTF8(Result) then
    raise EInOutError.Create(Format(lrsUnableToCreateConfigDirectoryS,[Result]));
end;

function GetAppConfigFileAnsi(Global: Boolean; SubDir: boolean;
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


{$endif WinCE}

// ******** End of AnsiString specific implementations ************


// ******** Start of WideString specific implementations ************

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
   if Hnd <> Windows.INVALID_HANDLE_VALUE then
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

function CreateDirWide(const NewDir: String): Boolean;
begin
  Result:=Windows.CreateDirectoryW(PWideChar(UTF8Decode(NewDir)), nil);
end;

function RemoveDirWide(const Dir: String): Boolean;
begin
  Result:=Windows.RemoveDirectoryW(PWideChar(UTF8Decode(Dir)));
end;

function DeleteFileWide(const FileName: String): Boolean;
begin
  Result:=Windows.DeleteFileW(PWideChar(UTF8Decode(FileName)));
end;

function RenameFileWide(const OldName, NewName: String): Boolean;
begin
  Result:=MoveFileW(PWideChar(UTF8Decode(OldName)), PWideChar(UTF8Decode(NewName)));
end;

function SetCurrentDirWide(const NewDir: String): Boolean;
begin
  {$ifdef WinCE}
  raise Exception.Create('[SetCurrentDirWide] The concept of the current directory doesn''t exist in Windows CE');
  {$else}
  Result:=Windows.SetCurrentDirectoryW(PWidechar(UTF8Decode(NewDir)));
  {$endif}
end;

{$IF DEFINED(WinCE) OR (FPC_FULLVERSION>=20701)}
  {$define FindData_W}
{$endif}


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
  { The structures are different at this point
    in win32 it is the ansi structure with a utf-8 string
    in wince it is a wide structure }
  {$ifdef FindData_W}
  f.Name:=UTF8Encode(UnicodeString(F.FindData.cFileName));
  {$else}
  f.Name:=F.FindData.cFileName;
  {$endif}
  Result:=0;
end;

{$IFNDEF FindData_W}

{ This function does not really convert from wide to ansi, but from wide to
  a utf-8 encoded ansi version of the data structures in win32 and does
  nothing in wince

  See FindMatch also }
procedure FindWideToAnsi(const wide: TWIN32FINDDATAW; var ansi: TWIN32FINDDATA);
var
  ws: WideString;
  an: AnsiString;
begin
  SetLength(ws, length(wide.cAlternateFileName));
  Move(wide.cAlternateFileName[0], ws[1], length(ws)*2);
  an := AnsiString(ws); // no need to utf8 for cAlternateFileName (it's always ansi encoded)
  Move(an[1], ansi.cAlternateFileName, sizeof(ansi.cAlternateFileName));

  ws := PWideChar(@wide.cFileName[0]);
  an := UTF8Encode(ws);
  ansi.cFileName := an;
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
end;
{$ENDIF}

function FindFirstWide(const Path: string; Attr: Longint; out Rslt: TSearchRec): Longint;
var
  find: TWIN32FINDDATAW;
begin
  Rslt.Name:=Path;
  Rslt.Attr:=attr;
  Rslt.ExcludeAttr:=(not Attr) and ($1e);
                 { $1e = faHidden or faSysFile or faVolumeID or faDirectory }
  { FindFirstFile is a Win32 Call }
  Rslt.FindHandle:=Windows.FindFirstFileW( PWideChar(UTF8Decode(Path)),find);
  If Rslt.FindHandle=Windows.Invalid_Handle_value then
  begin
    Result:=GetLastError;
    Exit;
  end;
  { Find file with correct attribute }
  {$IFNDEF FindData_W}
  FindWideToAnsi(find, Rslt.FindData);
  {$ELSE}
  Rslt.FindData := find;
  {$IFEND}
  Result := FindMatch(Rslt);
end;


function FindNextWide(var Rslt: TSearchRec): Longint;
var
  wide: TWIN32FINDDATAW;
begin
  if FindNextFileW(Rslt.FindHandle, wide) then
  begin
    {$IFNDEF FindData_W}
    FindWideToAnsi(wide, Rslt.FindData);
    {$ELSE}
    Rslt.FindData := wide;
    {$ENDIF}
    Result := FindMatch(Rslt);
  end
  else
    Result := Integer(GetLastError);
end;

//fpc 2.6.0 does not have StrLen for WideChar.
//Remove this when we have 2.6.4 or higher
{$if FPC_FULLVERSION < 20602}
function StrLen(PW: PWideChar): SizeInt; overload;
var
  i: SizeInt;
begin
  i:=0;
  if assigned(PW) then
    while (PW[i] <> #0) do inc(i);
  Result := i;
end;
{$endif}

Type
  PFNSHGetFolderPathW = Function(Ahwnd: HWND; Csidl: Integer; Token: THandle; Flags: DWord; Path: PWChar): HRESULT; stdcall;

var
  SHGetFolderPathW : PFNSHGetFolderPathW = Nil;
  CFGDLLHandle : THandle = 0;

Procedure InitDLL;
Var
  pathBuf: array[0..MAX_PATH-1] of char;
  pathLength: Integer;
begin
  { Load shfolder.dll using a full path, in order to prevent spoofing (Mantis #18185)
    Don't bother loading shell32.dll because shfolder.dll itself redirects SHGetFolderPath
    to shell32.dll whenever possible. }
  pathLength:=GetSystemDirectory(pathBuf, MAX_PATH);
  if (pathLength>0) and (pathLength<MAX_PATH-14) then { 14=length('\shfolder.dll'#0) }
  begin
    StrLCopy(@pathBuf[pathLength],'\shfolder.dll',MAX_PATH-pathLength-1);
    CFGDLLHandle:=LoadLibrary(pathBuf);

    if (CFGDLLHandle<>0) then
    begin
      Pointer(ShGetFolderPathW):=GetProcAddress(CFGDLLHandle,'SHGetFolderPathW');
      If @ShGetFolderPathW=nil then
      begin
        FreeLibrary(CFGDLLHandle);
        CFGDllHandle:=0;
      end;
    end;
  end;
  If (@ShGetFolderPathW=Nil) then
    Raise Exception.Create('Could not determine SHGetFolderPathW Function');
end;

function GetWindowsSpecialDirW(ID :  Integer) : String;
Var
  APath : Array[0..MAX_PATH] of WideChar;
  WS: WideString;
  Len: SizeInt;
begin
  Result := '';
  if (CFGDLLHandle = 0) then
    InitDLL;
  If (SHGetFolderPathW <> Nil) then
  begin
    FillChar(APath, SizeOf(APath), #0);
    if SHGetFolderPathW(0,ID or CSIDL_FLAG_CREATE,0,0,@APATH[0]) = S_OK then
    begin
      Len := StrLen(APath);
      SetLength(WS, Len);
      System.Move(APath[0], WS[1], Len * SizeOf(WideChar));
      //WS := (StrPas(@APath[0]));
      Result := AppendPathDelim(Utf16ToUtf8(WS));
    end;
  end
  else
    Result := SysToUtf8(GetWindowsSpecialDir(ID));
end;


function DGetAppConfigDir(Global : Boolean) : String;
begin
  Result := ChompPathDelim(ExtractFilePath(ParamStrUtf8(0)));
end;


function GetAppConfigDirWide(Global: Boolean; Create: boolean = false): string;
begin
  If Global then
    Result := GetWindowsSpecialDirW(CSIDL_COMMON_APPDATA)
  else
    Result := GetWindowsSpecialDirW(CSIDL_LOCAL_APPDATA);
  If (Result <> '') then
    begin
      if VendorName <> '' then
        Result := AppendPathDelim(Result + VendorName);
      Result := AppendPathDelim(Result + ApplicationName);
    end
  else
    Result := AppendPathDelim(DGetAppConfigDir(Global));
  if Result = '' then exit;
  if Create and not ForceDirectoriesUtf8(Result) then
    raise EInOutError.Create(Format(lrsUnableToCreateConfigDirectoryS,[Result]));
end;

function GetAppConfigFileWide(Global: Boolean; SubDir: boolean;
  CreateDir: boolean): string;
var
  Dir: string;
begin
  Result := GetAppConfigDirWide(Global);
  if SubDir then
    Result := AppendPathDelim(Result + 'Config');
  Result := Result + ApplicationName + ConfigExtension;
  if not CreateDir then exit;
  Dir := ExtractFilePath(Result);
  if Dir = '' then exit;
  if not ForceDirectoriesUTF8(Dir) then
    raise EInOutError.Create(Format(lrsUnableToCreateConfigDirectoryS,[Dir]));
end;

// ******** End of WideString specific implementations ************




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
  //if Filename uses ExtendedLengthPath scheme then it cannot be expanded
  //AND it should not be altered by ForcePathDelims or ResolveDots
  //See: http://msdn.microsoft.com/en-us/library/windows/desktop/aa365247%28v=vs.85%29.aspx
  if (Length(Fn) > 3) and (Fn[1] = PathDelim) and (Fn[2] = PathDelim) and
     (Fn[3] = '?') and (Fn[4] = PathDelim) //Do NOT use AllowDirectorySeparators here!
     then Exit;
  ForcePathDelims(Fn);
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

function FileIsExecutable(const AFilename: string): boolean;
begin
  Result:=FileExistsUTF8(AFilename);
end;

procedure CheckIfFileIsExecutable(const AFilename: string);
begin
  // TProcess does not report, if a program can not be executed
  // to get good error messages consider the OS
  if not FileExistsUTF8(AFilename) then begin
    raise Exception.Create(Format(lrsFileDoesNotExist, [AFilename]));
  end;
  if DirPathExists(AFilename) then begin
    raise Exception.Create(Format(lrsFileIsADirectoryAndNotAnExecutable, [
      AFilename]));
  end;
end;

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

procedure CheckIfFileIsSymlink(const AFilename: string);
begin
  // to get good error messages consider the OS
  if not FileExistsUTF8(AFilename) then begin
    raise Exception.Create(Format(lrsFileDoesNotExist, [AFilename]));
  end;
  if not FileIsSymLink(AFilename) then
    raise Exception.Create(Format(lrsIsNotASymbolicLink, [AFilename]));
end;


function FileIsHardLink(const AFilename: string): boolean;
var
  H: THandle;
  FileInfo: BY_HANDLE_FILE_INFORMATION;
begin
  Result := false;
  {$ifndef wince}
  //HardLinks are not supported in Win9x platform
  if (Win32Platform = VER_PLATFORM_WIN32_WINDOWS) then Exit;
  H := FileOpenUtf8(aFilename, fmOpenRead);
  if (H <> feInvalidHandle) then
  begin
    FillChar(FileInfo, SizeOf(BY_HANDLE_FILE_INFORMATION),0);
    if GetFileInformationByHandle(H, FileInfo) then
      Result := (FileInfo.nNumberOfLinks > 1);
    FileClose(H);
  end;
  {$endif}
end;

function FileIsReadable(const AFilename: string): boolean;
begin
  Result:=true;
end;

function FileIsWritable(const AFilename: string): boolean;
begin
  Result := ((FileGetAttrUTF8(AFilename) and faReadOnly) = 0);
end;


function IsUNCPath(const Path: String): Boolean;
begin
  Result := (Length(Path) > 2) and (Path[1] in AllowDirectorySeparators) and (Path[2] in AllowDirectorySeparators);
end;

function ExtractUNCVolume(const Path: String): String;
var
  I, Len: Integer;

  // the next function reuses Len variable
  function NextPathDelim(const Start: Integer): Integer;// inline;
  begin
    Result := Start;
    while (Result <= Len) and not (Path[Result] in AllowDirectorySeparators) do
      inc(Result);
  end;

begin
  if not IsUNCPath(Path) then
    Exit('');
  I := 3;
  Len := Length(Path);
  if Path[I] = '?' then
  begin
    // Long UNC path form like:
    // \\?\UNC\ComputerName\SharedFolder\Resource or
    // \\?\C:\Directory
    inc(I);
    if not (Path[I] in AllowDirectorySeparators) then
      Exit('');
    if UpperCase(Copy(Path, I + 1, 3)) = 'UNC' then
    begin
      inc(I, 4);
      if I < Len then
        I := NextPathDelim(I + 1);
      if I < Len then
        I := NextPathDelim(I + 1);
    end;
  end
  else
  begin
    I := NextPathDelim(I);
    if I < Len then
      I := NextPathDelim(I + 1);
  end;
  Result := Copy(Path, 1, I);
end;

function GetFileDescription(const AFilename: string): string;
begin
  // date + time
  Result:=lrsModified;
  try
    Result:=Result+FormatDateTime('DD/MM/YYYY hh:mm',
                           FileDateToDateTime(FileAgeUTF8(AFilename)));
  except
    Result:=Result+'?';
  end;
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
    _DeleteFileUtf8 := @DeleteFileAnsi;
    _RenameFileUtf8 := @RenameFileAnsi;
    _SetCurrentDirUtf8 := @SetCurrentDirAnsi;
    _GetCurrentDirUtf8 := @GetCurrentDirAnsi;
    _GetDirUtf8 := @GetDirAnsi;
    _FileOpenUtf8 := @FileOpenAnsi;
    _FileCreateUtf8 := @FileCreateAnsi;
    _CreateDirUtf8 := @CreateDirAnsi;
    _RemoveDirUtf8 := @RemoveDirAnsi;
    _FindFirstUtf8 := @FindFirstAnsi;
    _FindNextUtf8 := @FindNextAnsi;
    _GetAppConfigDirUtf8 := @GetAppConfigDirAnsi;
    _GetAppConfigFileUtf8 := @GetAppConfigFileAnsi;
  end
  else
  {$endif}
  begin
    _FileAgeUtf8 := @FileAgeWide;
    _FileSizeUtf8 := @FileSizeWide;
    _FileSetDateUtf8 := @FileSetDateWide;
    _FileGetAttrUtf8 := @FileGetAttrWide;
    _FileSetAttrUtf8 := @FileSetAttrWide;
    _DeleteFileUtf8 := @DeleteFileWide;
    _RenameFileUtf8 := @RenameFileWide;
    _SetCurrentDirUtf8 := @SetCurrentDirWide;
    _GetCurrentDirUtf8 :=@ GetCurrentDirWide;
    _GetDirUtf8 := @GetDirWide;
    _FileOpenUtf8 := @FileOpenWide;
    _FileCreateUtf8 := @FileCreateWide;
    _CreateDirUtf8 := @CreateDirWide;
    _RemoveDirUtf8 := @RemoveDirWide;
    _FindFirstUtf8 := @FindFirstWide;
    _FindNextUtf8 := @FindNextWide;
    _GetAppConfigDirUtf8 := @GetAppConfigDirWide;
    _GetAppConfigFileUtf8 := @GetAppConfigFileWide;
  end;
end;

procedure FinalizeLazFileUtils;
begin
  if CFGDLLHandle <> 0 then
    FreeLibrary(CFGDllHandle);
end;
