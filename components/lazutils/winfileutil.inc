{%MainUnit fileutil.pas}
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
  function FileIsHardLink(const AFilename: string): boolean;
 ------------------------------------------------------------------------------}
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

{------------------------------------------------------------------------------
  GetFileDescription
 ------------------------------------------------------------------------------}
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

{------------------------------------------------------------------------------
  function ReadAllLinks(const Filename: string;
    ExceptionOnError: boolean): string;
 ------------------------------------------------------------------------------}
function ReadAllLinks(const Filename: string;
  ExceptionOnError: boolean): string;
begin
  Result:=Filename;
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


function ExtractShortPathNameUTF8(const FileName: String): String;
var
  lPathSize: DWORD;
  WideFileName, WideResult: UnicodeString;
begin
  // WinCE doesnt have this concept
  {$ifdef WinCE}
  Result := FileName;
  {$else}
  if Win32MajorVersion >= 5 then
  begin
    WideFileName := UTF8ToUTF16(FileName);
    SetLength(WideResult,Max_Path);
    lPathSize := GetShortPathNameW(PWideChar(WideFileName), PWideChar(WideResult), Length(WideResult));
    SetLength(WideResult,lPathSize);
    Result := UTF16ToUTF8(WideResult);
  end
  else
    Result:=SysToUTF8(SysUtils.ExtractShortPathName(UTF8ToSys(FileName)));
  {$endif}
end;


