{%MainUnit fileutil.pas}
{******************************************************************************
                                  Fileutil
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
 

function NeedRTLAnsi: boolean;
begin
  Result := LazUtf8.NeedRTLAnsi;
end;

procedure SetNeedRTLAnsi(NewValue: boolean);
begin
  LazUtf8.SetNeedRTLAnsi(NewValue);
end;


function UTF8ToSys(const s: string): string;
begin
  Result := LazUtf8.UTF8ToSys(s);
end;

function SysToUTF8(const s: string): string;
begin
  Result := LazUtf8.SysToUTF8(s);
end;

function FileGetAttrUTF8(const FileName: String): Longint;
begin
  Result := LazFileUtils.FileGetAttrUtf8(FileName);
end;

function FileSetAttrUTF8(const Filename: String; Attr: longint): Longint;
begin
  Result := LazFileUtils.FileSetAttrUtf8(Filename, Attr);
end;

function FileExistsUTF8(const Filename: string): boolean;
begin
  Result := LazFileUtils.FileExistsUTF8(FileName);
end;

function DirectoryExistsUTF8(const Directory: string): Boolean; inline;
begin
  Result := LazFileUtils.DirectoryExistsUTF8(Directory);
end;

function FileAgeUTF8(const FileName: string): Longint;
begin
  Result := LazFileUtils.FileAgeUTF8(FileName);
end;

function FileSetDateUTF8(const FileName: String; Age: Longint): Longint;
begin
  Result := LazFileUtils.FileSetDateUTF8(FileName, Age);
end;

function FileSize(const Filename: string): int64;
begin
  Result := LazFileUtils.FileSizeUtf8(FileName);
end;

{$IFDEF darwin}
function GetDarwinSystemFilename(Filename: string): string;
var
  s: CFStringRef;
  l: CFIndex;
begin
  if Filename='' then exit('');
  s:=CFStringCreateWithCString(nil,Pointer(Filename),kCFStringEncodingUTF8);
  l:=CFStringGetMaximumSizeOfFileSystemRepresentation(s);
  SetLength(Result,l);
  if Result<>'' then begin
    CFStringGetFileSystemRepresentation(s,@Result[1],length(Result));
    SetLength(Result,StrLen(PChar(Result)));
  end;
  CFRelease(s);
end;
{$ENDIF}


function ExpandFileNameUTF8(const FileName: string): string;
begin
  Result := LazFileUtils.ExpandFileNameUtf8(Filename);
end;

// ToDo: For ExpandUNCFileNameUTF8
//
// Don't convert to and from Sys, because this RTL routines
// simply work in simple string operations, without calling native
// APIs which would really require Ansi
//
// The Ansi conversion just ruins Unicode strings
//
// See bug http://bugs.freepascal.org/view.php?id=20229
// It needs fixing like we did for LazFileUtils.ExpandFileNameUtf8(Filename) on Windows

function ExpandUNCFileNameUTF8(const FileName: string): string;
begin
  Result:=SysUtils.ExpandUNCFileName(Filename);
end;


function FileOpenUTF8(Const FileName : string; Mode : Integer) : THandle;
begin
  Result := LazFileUtils.FileOpenUtf8(FileName, Mode);
end;

function FileCreateUTF8(Const FileName : string) : THandle;
begin
  Result := LazFileUtils.FileCreateUtf8(FileName);
end;

function FileCreateUTF8(Const FileName : string; Rights: Cardinal) : THandle;
begin
  Result := LazFileUtils.FileCreateUtf8(FileName, Rights);
end;

function FindFirstUTF8(const Path: string; Attr: Longint; out Rslt: TSearchRec): Longint;
begin
  Result := LazFileUtils.FindFirstUtf8(Path, Attr, Rslt);
end;

function FindNextUTF8(var Rslt: TSearchRec): Longint;
begin
  Result := LazFileUtils.FindNextUTF8(Rslt);
end;

procedure FindCloseUTF8(var F: TSearchrec);
begin
  LazFileUtils.FindCloseUtf8(F);
end;

function DeleteFileUTF8(const FileName: String): Boolean;
begin
  Result := LazFileUtils.DeleteFileUtf8(FileName);
end;

function RenameFileUTF8(const OldName, NewName: String): Boolean;
begin
  Result := LazFileUtils.RenameFileUtf8(OldName, NewName);
end;

function GetCurrentDirUTF8: String;
begin
  Result := LazFileUtils.GetCurrentDirUtf8();
end;

function SetCurrentDirUTF8(const NewDir: String): Boolean;
begin
  Result := LazFileUtils.SetCurrentDirUtf8(NewDir);
end;

function CreateDirUTF8(const NewDir: String): Boolean;
begin
  Result := LazFileUtils.CreateDirUtf8(NewDir);
end;

function RemoveDirUTF8(const Dir: String): Boolean;
begin
  Result := LazFileUtils.RemoveDirUtf8(Dir);
end;

function ParamStrUTF8(Param: Integer): string;
begin
  Result:=SysToUTF8(ObjPas.ParamStr(Param));
end;



function GetEnvironmentStringUTF8(Index: Integer): String;
begin
  // on Windows SysUtils.GetEnvironmentString returns OEM encoded string
  // so ConsoleToUTF8 function should be used!
  // RTL issue: http://bugs.freepascal.org/view.php?id=15233
  Result:=ConsoleToUTF8(SysUtils.GetEnvironmentString(Index));
end;

function GetEnvironmentVariableUTF8(const EnvVar: String): String;
begin
  // on Windows SysUtils.GetEnvironmentString returns OEM encoded string
  // so ConsoleToUTF8 function should be used!
  // RTL issue: http://bugs.freepascal.org/view.php?id=15233
  Result:=ConsoleToUTF8(SysUtils.GetEnvironmentVariable(UTF8ToSys(EnvVar)));
end;

function GetAppConfigDirUTF8(Global: Boolean; Create: boolean = false): string;
begin
  Result:=SysToUTF8(SysUtils.GetAppConfigDir(Global));
  if Result='' then exit;
  if Create and not ForceDirectoriesUTF8(Result) then
    raise EInOutError.Create(Format(lrsUnableToCreateConfigDirectoryS,[Result]));
end;

function GetAppConfigFileUTF8(Global: Boolean; SubDir: boolean;
  CreateDir: boolean): string;
var
  Dir: string;
begin
  Result:=SysToUTF8(SysUtils.GetAppConfigFile(Global,SubDir));
  if not CreateDir then exit;
  Dir:=ExtractFilePath(Result);
  if Dir='' then exit;
  if not ForceDirectoriesUTF8(Dir) then
    raise EInOutError.Create(Format(lrsUnableToCreateConfigDirectoryS,[Dir]));
end;

function SysErrorMessageUTF8(ErrorCode: Integer): String;
begin
  Result:=SysToUTF8(SysUtils.SysErrorMessage(ErrorCode));
end;

function DirPathExists(const FileName: String): Boolean;
begin
  Result := LazFileUtils.DirPathExists(FileName);
end;

function CompareFilenames(const Filename1, Filename2: string): integer;
begin
  Result := LazFileUtils.CompareFilenames(Filename1, Filename2);
end;

function CompareFilenamesIgnoreCase(const Filename1, Filename2: string): integer;
begin
  Result := LazFileUtils.CompareFilenamesIgnoreCase(Filename1, Filename2);
end;

function CompareFilenames(const Filename1, Filename2: string;
  ResolveLinks: boolean): integer;
var
  File1: String;
  File2: String;
begin
  File1:=Filename1;
  File2:=Filename2;
  if ResolveLinks then begin
    File1:=ReadAllLinks(File1,false);
    if (File1='') then File1:=Filename1;
    File2:=ReadAllLinks(File2,false);
    if (File2='') then File2:=Filename2;
  end;
  Result:=CompareFilenames(File1,File2);
end;

function CompareFilenames(Filename1: PChar; Len1: integer;
  Filename2: PChar; Len2: integer; ResolveLinks: boolean): integer;
var
  File1: string;
  File2: string;
  {$IFNDEF NotLiteralFilenames}
  i: Integer;
  {$ENDIF}
begin
  if (Len1=0) or (Len2=0) then begin
    Result:=Len1-Len2;
    exit;
  end;
  if ResolveLinks then begin
    SetLength(File1,Len1);
    System.Move(Filename1^,File1[1],Len1);
    SetLength(File2,Len2);
    System.Move(Filename2^,File2[1],Len2);
    Result:=CompareFilenames(File1,File2,true);
  end else begin
    {$IFDEF NotLiteralFilenames}
    SetLength(File1,Len1);
    System.Move(Filename1^,File1[1],Len1);
    SetLength(File2,Len2);
    System.Move(Filename2^,File2[1],Len2);
    Result:=CompareFilenames(File1,File2);
    {$ELSE}
    Result:=0;
    i:=0;
    while (Result=0) and ((i<Len1) and (i<Len2)) do begin
      Result:=Ord(Filename1[i])
             -Ord(Filename2[i]);
      Inc(i);
    end;
    if Result=0 Then
      Result:=Len1-Len2;
    {$ENDIF}
  end;
end;

function FilenameIsAbsolute(const TheFilename: string):boolean;
begin
  Result := LazFileUtils.FilenameIsAbsolute(TheFileName);
end;

function FilenameIsWinAbsolute(const TheFilename: string): boolean;
begin
  Result := LazFileUtils.FilenameIsWinAbsolute(TheFileName);
end;

function FilenameIsUnixAbsolute(const TheFilename: string): boolean;
begin
  Result := LazFileUtils.FilenameIsUnixAbsolute(TheFileName);
end;

function FilenameIsPascalUnit(const Filename: string): boolean;
var
  i: Integer;
begin
  for i:=Low(PascalFileExt) to High(PascalFileExt) do
    if CompareFileExt(Filename,PascalFileExt[i],false)=0 then
      exit(true);
  Result:=false;
end;

function AppendPathDelim(const Path: string): string;
begin
  Result := LazFileUtils.AppendPathDelim(Path);
end;

function TrimFilename(const AFilename: string): string;
begin
  Result := LazFileUtils.TrimFilename(AFileName);
end;

function ExtractFileNameWithoutExt(const AFilename: string): string;
var
  p: Integer;
begin
  Result:=AFilename;
  p:=length(Result);
  while (p>0) do begin
    case Result[p] of
      PathDelim: exit;
      '.': exit(copy(Result,1, p-1));
    end;
    dec(p);
  end;
end;

function CompareFileExt(const Filename, Ext: string; CaseSensitive: boolean): integer;
// Ext can contain a point or not
var
  n, e : AnsiString;
  FileLen, FilePos, ExtLen, ExtPos: integer;
begin
  FileLen:=length(Filename);
  ExtLen:=length(Ext);
  FilePos:=FileLen;
  while (FilePos>=1) and (Filename[FilePos]<>'.') do dec(FilePos);
  if FilePos<1 then begin
    // no extension in filename
    Result:=1;
    exit;
  end;
  // skip point
  inc(FilePos);
  ExtPos:=1;
  if (ExtPos<=ExtLen) and (Ext[1]='.') then inc(ExtPos);

  // compare extensions
  n:=Copy(Filename, FilePos, length(FileName));
  e:=Copy(Ext, ExtPos, length(Ext));
  if CaseSensitive then
    Result:=CompareStr(n, e)
  else
    Result:=UTF8CompareText(n, e);
  if Result<0 then Result:=1
  else if Result>0 then Result:=1;
end;

function CompareFileExt(const Filename, Ext: string): integer;
begin
  Result:=CompareFileExt(Filename,Ext,false);
end;

function ChompPathDelim(const Path: string): string;
begin
  Result := LazFileUtils.ChompPathDelim(Path);
end;

function FileIsText(const AFilename: string): boolean;
begin
  Result := LazFileUtils.FileIsText(AFilename);
end;

function FileIsText(const AFilename: string; out FileReadable: boolean): boolean;
begin
  Result := LazFileUtils.FileIsText(AFileName, FileReadable);
end;

function TryReadAllLinks(const Filename: string): string;
begin
  Result:=ReadAllLinks(Filename,false);
  if Result='' then
    Result:=Filename;
end;

{------------------------------------------------------------------------------
  function ExtractFileNameOnly(const AFilename: string): string;
 ------------------------------------------------------------------------------}
function ExtractFileNameOnly(const AFilename: string): string;
var
  StartPos: Integer;
  ExtPos: Integer;
begin
  StartPos:=length(AFilename)+1;
  while (StartPos>1)
  and (AFilename[StartPos-1]<>PathDelim)
  {$IFDEF Windows}and (AFilename[StartPos-1]<>':'){$ENDIF}
  do
    dec(StartPos);
  ExtPos:=length(AFilename);
  while (ExtPos>=StartPos) and (AFilename[ExtPos]<>'.') do
    dec(ExtPos);
  if (ExtPos<StartPos) then ExtPos:=length(AFilename)+1;
  Result:=copy(AFilename,StartPos,ExtPos-StartPos);
end;


{------------------------------------------------------------------------------
  function ForceDirectory(DirectoryName: string): boolean;
 ------------------------------------------------------------------------------}
function ForceDirectory(DirectoryName: string): boolean;
var i: integer;
  Dir: string;
begin
  DoDirSeparators(DirectoryName);
  DirectoryName := AppendPathDelim(DirectoryName);
  i:=1;
  while i<=length(DirectoryName) do begin
    if DirectoryName[i]=PathDelim then begin
      Dir:=copy(DirectoryName,1,i-1);
      if not DirPathExists(Dir) then begin
        Result:=CreateDirUTF8(Dir);
        if not Result then exit;
      end;
    end;
    inc(i);
  end;
  Result:=true;
end;

{------------------------------------------------------------------------------
  function DeleteDirectory(const DirectoryName: string;
    OnlyChilds: boolean): boolean;
 ------------------------------------------------------------------------------}
function DeleteDirectory(const DirectoryName: string;
  OnlyChildren: boolean): boolean;
const
  //Don't follow symlinks on *nix, just delete them
  DeleteMask = faAnyFile {$ifdef unix} or faSymLink {$endif unix};
var
  FileInfo: TSearchRec;
  CurSrcDir: String;
  CurFilename: String;
begin
  Result:=false;
  CurSrcDir:=CleanAndExpandDirectory(DirectoryName);
  if FindFirstUTF8(CurSrcDir+GetAllFilesMask,DeleteMask,FileInfo)=0 then begin
    repeat
      // check if special file
      if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='') then
        continue;
      CurFilename:=CurSrcDir+FileInfo.Name;
      if ((FileInfo.Attr and faDirectory)>0)
         {$ifdef unix} and ((FileInfo.Attr and faSymLink)=0) {$endif unix} then begin
        if not DeleteDirectory(CurFilename,false) then exit;
      end else begin
        if not DeleteFileUTF8(CurFilename) then exit;
      end;
    until FindNextUTF8(FileInfo)<>0;
  end;
  FindCloseUTF8(FileInfo);
  if (not OnlyChildren) and (not RemoveDirUTF8(CurSrcDir)) then exit;
  Result:=true;
end;

{------------------------------------------------------------------------------
  function ProgramDirectory: string;
 ------------------------------------------------------------------------------}
function ProgramDirectory: string;
var
  Flags: TSearchFileInPathFlags;
begin
  Result:=ParamStrUTF8(0);
  if ExtractFilePath(Result)='' then begin
    // program was started via PATH
    {$IFDEF WINDOWS}
    Flags:=[];
    {$ELSE}
    Flags:=[sffDontSearchInBasePath];
    {$ENDIF}
    Result:=SearchFileInPath(Result,'',GetEnvironmentVariableUTF8('PATH'),':',Flags);
  end;
  // resolve links
  Result:=ReadAllLinks(Result,false);
  // extract file path and expand to full name
  Result:=ExpandFileNameUTF8(ExtractFilePath(Result));
end;

function DirectoryIsWritable(const DirectoryName: string): boolean;
var
  TempFilename: String;
  fs: TFileStream;
  s: String;
begin
  TempFilename:=GetTempFilename(DirectoryName,'tstperm');
  Result:=false;
  try
    fs:=TFileStream.Create(UTF8ToSys(TempFilename),fmCreate);
    s:='WriteTest';
    fs.Write(s[1],length(s));
    fs.Free;
    DeleteFileUTF8(TempFilename);
    Result:=true;
  except
  end;
end;

{------------------------------------------------------------------------------
  function CleanAndExpandFilename(const Filename: string): string;
 ------------------------------------------------------------------------------}
function CleanAndExpandFilename(const Filename: string): string;
begin
  Result := LazFileutils.CleanAndExpandFilename(FileName);
end;

{------------------------------------------------------------------------------
  function CleanAndExpandDirectory(const Filename: string): string;
 ------------------------------------------------------------------------------}
function CleanAndExpandDirectory(const Filename: string): string;
begin
  Result := LazFileUtils.CleanAndExpandDirectory(FileName);
end;

function CreateAbsoluteSearchPath(const SearchPath, BaseDirectory: string
  ): string;
var
  PathLen: Integer;
  EndPos: Integer;
  StartPos: Integer;
  CurDir: String;
  NewCurDir: String;
  DiffLen: Integer;
  BaseDir: String;
begin
  Result:=SearchPath;
  if (SearchPath='') or (BaseDirectory='') then exit;
  BaseDir:=AppendPathDelim(BaseDirectory);

  PathLen:=length(Result);
  EndPos:=1;
  while EndPos<=PathLen do begin
    StartPos:=EndPos;
    while (Result[StartPos]=';') do begin
      inc(StartPos);
      if StartPos>PathLen then exit;
    end;
    EndPos:=StartPos;
    while (EndPos<=PathLen) and (Result[EndPos]<>';') do inc(EndPos);
    CurDir:=copy(Result,StartPos,EndPos-StartPos);
    if not FilenameIsAbsolute(CurDir) then begin
      NewCurDir:=BaseDir+CurDir;
      if NewCurDir<>CurDir then begin
        DiffLen:=length(NewCurDir)-length(CurDir);
        Result:=copy(Result,1,StartPos-1)+NewCurDir
                +copy(Result,EndPos,PathLen-EndPos+1);
        inc(EndPos,DiffLen);
        inc(PathLen,DiffLen);
      end;
    end;
    StartPos:=EndPos;
  end;
end;

function CreateRelativePath(const Filename, BaseDirectory: string;
  UsePointDirectory: boolean): string;
begin
  if (BaseDirectory='') or (Filename='') then Exit(FileName);
  Result := SysUtils.ExtractRelativePath(ExpandFileNameUtf8(IncludetrailingPathDelimiter(BaseDirectory)),FileName);
  if UsePointDirectory and (Result='') and (Filename<>'') then
    Result:='.'; // Filename is the BaseDirectory
end;

function CreateAbsolutePath(const Filename, BaseDirectory: string): string;
begin
  if (Filename='') or FilenameIsAbsolute(Filename) then
    Result:=Filename
  {$IFDEF Windows}
  else if (Filename[1]='\') then
    // only use drive of BaseDirectory
    Result:=ExtractFileDrive(BaseDirectory)+Filename
  {$ENDIF}
  else
    Result:=AppendPathDelim(BaseDirectory)+Filename;
  Result:=TrimFilename(Result);
end;

function FileIsInPath(const Filename, Path: string): boolean;
var
  ExpFile: String;
  ExpPath: String;
  l: integer;
begin
  ExpFile:=CleanAndExpandFilename(Filename);
  ExpPath:=CleanAndExpandDirectory(Path);
  l:=length(ExpPath);
  Result:=(l>0) and (length(ExpFile)>l) and (ExpFile[l]=PathDelim)
          and (CompareFilenames(ExpPath,LeftStr(ExpFile,l))=0);
end;

function FileIsInDirectory(const Filename, Directory: string): boolean;
var
  ExpFile: String;
  ExpDir: String;
  LenFile: Integer;
  LenDir: Integer;
  p: LongInt;
begin
  ExpFile:=CleanAndExpandFilename(Filename);
  ExpDir:=CleanAndExpandDirectory(Directory);
  LenFile:=length(ExpFile);
  LenDir:=length(ExpDir);
  p:=LenFile;
  while (p>0) and (ExpFile[p]<>PathDelim) do dec(p);
  Result:=(p=LenDir) and (p<LenFile)
          and (CompareFilenames(ExpDir,LeftStr(ExpFile,p))=0);
end;

function CopyFile(const SrcFilename, DestFilename: String;
                  Flags: TCopyFileFlags=[cffOverwriteFile]): Boolean;
var
  SrcHandle: THandle;
  DestHandle: THandle;
  Buffer: array[1..4096] of byte;
  ReadCount, WriteCount, TryCount: LongInt;
begin
  Result := False;
  // check overwrite
  if (not (cffOverwriteFile in Flags)) and FileExistsUTF8(DestFileName) then
    exit;
  // check directory
  if (cffCreateDestDirectory in Flags)
  and (not DirectoryExistsUTF8(ExtractFilePath(DestFileName)))
  and (not ForceDirectoriesUTF8(ExtractFilePath(DestFileName))) then
    exit;
  TryCount := 0;
  While TryCount <> 3 Do Begin
    SrcHandle := FileOpenUTF8(SrcFilename, fmOpenRead or fmShareDenyWrite);
    if (THandle(SrcHandle)=feInvalidHandle) then Begin
      Inc(TryCount);
      Sleep(10);
    End
    Else Begin
      TryCount := 0;
      Break;
    End;
  End;
  If TryCount > 0 Then
    raise EFOpenError.Createfmt({SFOpenError}'Unable to open file "%s"', [SrcFilename]);
  try
    DestHandle := FileCreateUTF8(DestFileName);
    if (THandle(DestHandle)=feInvalidHandle) then
      raise EFCreateError.createfmt({SFCreateError}'Unable to create file "%s"',[DestFileName]);
    try
      repeat
        ReadCount:=FileRead(SrcHandle,Buffer[1],High(Buffer));
        if ReadCount<=0 then break;
        WriteCount:=FileWrite(DestHandle,Buffer[1],ReadCount);
        if WriteCount<ReadCount then
          raise EWriteError.createfmt({SFCreateError}'Unable to write to file "%s"',[DestFileName])
      until false;
    finally
      FileClose(DestHandle);
    end;
    if (cffPreserveTime in Flags) then
      FileSetDateUTF8(DestFilename, FileGetDate(SrcHandle));
    Result := True;
  finally
    FileClose(SrcHandle);
  end;
end;

function CopyFile(const SrcFilename, DestFilename: string; PreserveTime: Boolean): boolean;
// Flags parameter can be used for the same thing.
var
  Flags: TCopyFileFlags;
begin
  if PreserveTime then
    Flags:=[cffPreserveTime, cffOverwriteFile]
  else
    Flags:=[cffOverwriteFile];
  Result := CopyFile(SrcFilename, DestFilename, Flags);
end;

{ TCopyDirTree for CopyDirTree function }
type
  TCopyDirTree = class(TFileSearcher)
  private
    FSourceDir: string;
    FTargetDir: string;
    FFlags: TCopyFileFlags;
    FCopyFailedCount:Integer;
  protected
    procedure DoFileFound; override;
    procedure DoDirectoryFound; override;
  end;

procedure TCopyDirTree.DoFileFound;
var
  NewLoc: string;
begin
  // ToDo: make sure StringReplace works in all situations !
  NewLoc:=StringReplace(FileName, FSourceDir, FTargetDir, []);
  if not CopyFile(FileName, NewLoc, FFlags) then
    Inc(FCopyFailedCount);
end;

procedure TCopyDirTree.DoDirectoryFound;
var
  NewPath:String;
begin
  NewPath:=StringReplace(FileName, FSourceDir, FTargetDir, []);
  // ToDo: make directories also respect cffPreserveTime flag.
  if not DirectoryExistsUTF8(NewPath) then
    if not ForceDirectoriesUTF8(NewPath) then
      Inc(FCopyFailedCount);
end;

function CopyDirTree(const SourceDir, TargetDir: string; Flags: TCopyFileFlags=[]): Boolean;
var
  Searcher: TCopyDirTree;
begin
  Result:=False;
  Searcher:=TCopyDirTree.Create;
  try
    // Destination directories are always created. User setting has no effect!
    Flags:=Flags-[cffCreateDestDirectory];
    Searcher.FFlags:=Flags;
    Searcher.FCopyFailedCount:=0;
    Searcher.FSourceDir:=SourceDir;
    Searcher.FTargetDir:=TargetDir;
    Searcher.Search(SourceDir);
    Result:=True;
  finally
    Result:=Searcher.FCopyFailedCount=0;
    Searcher.Free;
  end;
end;

function GetTempFilename(const Directory, Prefix: string): string;
var
  i: Integer;
  CurPath: String;
begin
  CurPath:=AppendPathDelim(ExpandFileNameUTF8(Directory))+Prefix;
  i:=1;
  repeat
    Result:=CurPath+IntToStr(i)+'.tmp';
    if not (FileExistsUTF8(Result) or DirectoryExistsUTF8(Result)) then exit;
    inc(i);
  until false;
end;

function SearchFileInPath(const Filename, BasePath, SearchPath,
  Delimiter: string; Flags: TSearchFileInPathFlags): string;
var
  p, StartPos, l: integer;
  CurPath, Base: string;
begin
//debugln('[SearchFileInPath] Filename="',Filename,'" BasePath="',BasePath,'" SearchPath="',SearchPath,'" Delimiter="',Delimiter,'"');
  if (Filename='') then begin
    Result:='';
    exit;
  end;
  // check if filename absolute
  if FilenameIsAbsolute(Filename) then begin
    if FileExistsUTF8(Filename) then begin
      Result:=CleanAndExpandFilename(Filename);
      exit;
    end else begin
      Result:='';
      exit;
    end;
  end;
  Base:=CleanAndExpandDirectory(BasePath);
  // search in current directory
  if (not (sffDontSearchInBasePath in Flags))
  and FileExistsUTF8(Base+Filename) then begin
    Result:=CleanAndExpandFilename(Base+Filename);
    exit;
  end;
  // search in search path
  StartPos:=1;
  l:=length(SearchPath);
  while StartPos<=l do begin
    p:=StartPos;
    while (p<=l) and (pos(SearchPath[p],Delimiter)<1) do inc(p);
    CurPath:=TrimFilename(copy(SearchPath,StartPos,p-StartPos));
    if CurPath<>'' then begin
      if not FilenameIsAbsolute(CurPath) then
        CurPath:=Base+CurPath;
      Result:=CleanAndExpandFilename(AppendPathDelim(CurPath)+Filename);
      if FileExistsUTF8(Result) then exit;
    end;
    StartPos:=p+1;
  end;
  Result:='';
end;

function SearchAllFilesInPath(const Filename, BasePath, SearchPath,
  Delimiter: string; Flags: TSearchFileInPathFlags): TStrings;
  
  procedure Add(NewFilename: string);
  var
    i: Integer;
  begin
    NewFilename:=TrimFilename(NewFilename);
    if not FileExistsUTF8(NewFilename) then exit;
    if Result=nil then begin
      Result:=TStringList.Create;
    end else begin
      for i:=0 to Result.Count-1 do
        if CompareFilenames(Result[i],NewFilename)=0 then exit;
    end;
    Result.Add(NewFilename);
  end;
  
var
  p, StartPos, l: integer;
  CurPath, Base: string;
begin
  Result:=nil;
  if (Filename='') then exit;
  // check if filename absolute
  if FilenameIsAbsolute(Filename) then begin
    Add(CleanAndExpandFilename(Filename));
    exit;
  end;
  Base:=CleanAndExpandDirectory(BasePath);
  // search in current directory
  if (not (sffDontSearchInBasePath in Flags)) then begin
    Add(CleanAndExpandFilename(Base+Filename));
  end;
  // search in search path
  StartPos:=1;
  l:=length(SearchPath);
  while StartPos<=l do begin
    p:=StartPos;
    while (p<=l) and (pos(SearchPath[p],Delimiter)<1) do inc(p);
    CurPath:=TrimFilename(copy(SearchPath,StartPos,p-StartPos));
    if CurPath<>'' then begin
      if not FilenameIsAbsolute(CurPath) then
        CurPath:=Base+CurPath;
      Add(CleanAndExpandFilename(AppendPathDelim(CurPath)+Filename));
    end;
    StartPos:=p+1;
  end;
end;

function FindDiskFilename(const Filename: string): string;
// Searches for the filename case on disk.
// The file must exist.
// For example:
//   If Filename='file' and there is only a 'File' then 'File' will be returned.
var
  StartPos: Integer;
  EndPos: LongInt;
  FileInfo: TSearchRec;
  CurDir: String;
  CurFile: String;
  AliasFile: String;
  Ambiguous: Boolean;
begin
  Result:=Filename;
  if not FileExistsUTF8(Filename) then exit;
  // check every directory and filename
  StartPos:=1;
  {$IFDEF WINDOWS}
  // uppercase Drive letter and skip it
  if ((length(Result)>=2) and (Result[1] in ['A'..'Z','a'..'z'])
  and (Result[2]=':')) then begin
    StartPos:=3;
    if Result[1] in ['a'..'z'] then
      Result[1]:=upcase(Result[1]);
  end;
  {$ENDIF}
  repeat
    // skip PathDelim
    while (StartPos<=length(Result)) and (Result[StartPos]=PathDelim) do
      inc(StartPos);
    // find end of filename part
    EndPos:=StartPos;
    while (EndPos<=length(Result)) and (Result[EndPos]<>PathDelim) do
      inc(EndPos);
    if EndPos>StartPos then begin
      // search file
      CurDir:=copy(Result,1,StartPos-1);
      CurFile:=copy(Result,StartPos,EndPos-StartPos);
      AliasFile:='';
      Ambiguous:=false;
      if FindFirstUTF8(CurDir+GetAllFilesMask,faAnyFile,FileInfo)=0 then
      begin
        repeat
          // check if special file
          if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='')
          then
            continue;
          if CompareFilenamesIgnoreCase(FileInfo.Name,CurFile)=0 then begin
            //debugln('FindDiskFilename ',FileInfo.Name,' ',CurFile);
            if FileInfo.Name=CurFile then begin
              // file found, has already the correct name
              AliasFile:='';
              break;
            end else begin
              // alias found, but has not the correct name
              if AliasFile='' then begin
                AliasFile:=FileInfo.Name;
              end else begin
                // there are more than one candidate
                Ambiguous:=true;
              end;
            end;
          end;
        until FindNextUTF8(FileInfo)<>0;
      end;
      FindCloseUTF8(FileInfo);
      if (AliasFile<>'') and (not Ambiguous) then begin
        // better filename found -> replace
        Result:=CurDir+AliasFile+copy(Result,EndPos,length(Result));
      end;
    end;
    StartPos:=EndPos+1;
  until StartPos>length(Result);
end;

function FindDiskFileCaseInsensitive(const Filename: string): string;
var
  FileInfo: TSearchRec;
  ShortFilename: String;
  CurDir: String;
begin
  Result:='';
  CurDir:=ExtractFilePath(Filename);
  if FindFirstUTF8(CurDir+GetAllFilesMask,faAnyFile, FileInfo)=0 then begin
    ShortFilename:=ExtractFilename(Filename);
    repeat
      // check if special file
      if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='')
      then
        continue;
      if CompareFilenamesIgnoreCase(FileInfo.Name,ShortFilename)=0 then begin
        if FileInfo.Name=ShortFilename then begin
          // fits exactly
          Result:=Filename;
          break;
        end;
        // fits case insensitive
        Result:=CurDir+FileInfo.Name;
        // search further
      end;
    until FindNextUTF8(FileInfo)<>0;
  end;
  FindCloseUTF8(FileInfo);
end;

function FindDefaultExecutablePath(const Executable: string): string;
begin
  if FilenameIsAbsolute(Executable) then begin
    Result:=Executable;
    if FileExistsUTF8(Result) then exit;
    {$IFDEF Windows}
    if ExtractFileExt(Result)='' then begin
      Result:=Result+'.exe';
      if FileExistsUTF8(Result) then exit;
    end;
    {$ENDIF}
  end else begin
    Result:=SearchFileInPath(Executable,'',
                             GetEnvironmentVariableUTF8('PATH'), PathSeparator,
                             [sffDontSearchInBasePath]);
    if Result<>'' then exit;
    {$IFDEF Windows}
    if ExtractFileExt(Executable)='' then begin
      Result:=SearchFileInPath(Executable+'.exe','',
                               GetEnvironmentVariableUTF8('PATH'), PathSeparator,
                               [sffDontSearchInBasePath]);
      if Result<>'' then exit;
    end;
    {$ENDIF}
  end;
  Result:='';
end;

type

  { TListFileSearcher }

  TListFileSearcher = class(TFileSearcher)
  private
    FList: TStrings;
  protected
    procedure DoFileFound; override;
  public
    constructor Create(AList: TStrings);
  end;

{ TListFileSearcher }

procedure TListFileSearcher.DoFileFound;
begin
  FList.Add(FileName);
end;

constructor TListFileSearcher.Create(AList: TStrings);
begin
  inherited Create;
  FList := AList;
end;

function FindAllFiles(const SearchPath: String; SearchMask: String;
  SearchSubDirs: Boolean): TStringList;
var
  Searcher: TListFileSearcher;
begin
  Result := TStringList.Create;
  Searcher := TListFileSearcher.Create(Result);
  try
    Searcher.Search(SearchPath, SearchMask, SearchSubDirs);
  finally
    Searcher.Free;
  end;
end;

type

  { TListDirectoriesSearcher }

  TListDirectoriesSearcher = class(TFileSearcher)
  private
    FDirectoriesList :TStrings;
  protected
    procedure DoDirectoryFound; override;
  public
    constructor Create(AList: TStrings);
  end;

constructor TListDirectoriesSearcher.Create(AList: TStrings);
begin
  inherited Create;
  FDirectoriesList := AList;
end;

procedure TListDirectoriesSearcher.DoDirectoryFound;
begin
  FDirectoriesList.Add(FileName);
end;

function FindAllDirectories(const SearchPath : string;
  SearchSubDirs: Boolean = True): TStringList;
var
  Searcher :TFileSearcher;
begin
  Result := TStringList.Create;
  Searcher := TListDirectoriesSearcher.Create(Result);
  try
    Searcher.Search(SearchPath, AllFilesMask, SearchSubDirs);
  finally
    Searcher.Free;
  end;
end;

{ TFileIterator }

function TFileIterator.GetFileName: String;
begin
  Result := FPath + FFileInfo.Name;
end;

procedure TFileIterator.Stop;
begin
  FSearching := False;
end;

function TFileIterator.IsDirectory: Boolean;
begin
  Result := (FFileInfo.Attr and faDirectory) <> 0;
end;

{ TFileSearcher }

procedure TFileSearcher.RaiseSearchingError;
begin
  raise Exception.Create('The file searcher is already searching!');
end;

procedure TFileSearcher.DoDirectoryEnter;
begin
  if Assigned(FonDirectoryEnter) then FOnDirectoryEnter(Self);
end;

procedure TFileSearcher.DoDirectoryFound;
begin
  if Assigned(FOnDirectoryFound) then OnDirectoryFound(Self);
end;

procedure TFileSearcher.DoFileFound;
begin
  if Assigned(FOnFileFound) then OnFileFound(Self);
end;

constructor TFileSearcher.Create;
begin
  inherited Create;
  FMaskSeparator := ';';
  FFollowSymLink := True;
  FFileAttribute := faAnyFile;
  FDirectoryAttribute := faDirectory;
  FSearching := False;
end;

procedure TFileSearcher.Search(const ASearchPath: String; ASearchMask: String;
  ASearchSubDirs: Boolean; CaseSensitive: Boolean = False);
var
  MaskList: TMaskList;

  procedure DoSearch(const APath: String; const ALevel: Integer);
  var
    P: String;
    PathInfo: TSearchRec;
  begin
    P := APath + AllDirectoryEntriesMask;

    if FindFirstUTF8(P, FileAttribute, PathInfo) = 0 then
    try
      repeat
        // skip special files
        if (PathInfo.Name = '.') or (PathInfo.Name = '..') or
          (PathInfo.Name = '') then Continue;
        // Deal with both files and directories
        if (PathInfo.Attr and faDirectory) = 0 then
        begin             // File
          {$IFDEF Windows}
          if (MaskList = nil) or MaskList.MatchesWindowsMask(PathInfo.Name)
          {$ELSE}
          if (MaskList = nil) or MaskList.Matches(PathInfo.Name)
          {$ENDIF}
          then begin
            FPath := APath;
            FLevel := ALevel;
            FFileInfo := PathInfo;
            DoFileFound;
          end;
        end
        else begin        // Directory
          FPath := APath;
          FLevel := ALevel;
          FFileInfo := PathInfo;
          DoDirectoryFound;
        end;

      until (FindNextUTF8(PathInfo) <> 0) or not FSearching;
    finally
      FindCloseUTF8(PathInfo);
    end;

    if ASearchSubDirs or (ALevel > 0) then
      // search recursively in directories
      if FindFirstUTF8(P, DirectoryAttribute, PathInfo) = 0 then
      try
        repeat
          if (PathInfo.Name = '.') or (PathInfo.Name = '..') or
             (PathInfo.Name = '') or ((PathInfo.Attr and faDirectory) = 0) or
             (not FFollowSymLink and FileIsSymlink(APath + PathInfo.Name))
          then Continue;

          FPath := APath;
          FLevel := ALevel;
          FFileInfo := PathInfo;
          DoDirectoryEnter;
          if not FSearching then Break;

          DoSearch(AppendPathDelim(APath + PathInfo.Name), Succ(ALevel));

        until (FindNextUTF8(PathInfo) <> 0);
      finally
        FindCloseUTF8(PathInfo);
      end;
  end;

begin
  if FSearching then RaiseSearchingError;

  MaskList := TMaskList.Create(ASearchMask, FMaskSeparator, CaseSensitive);
  // empty mask = all files mask
  if MaskList.Count = 0 then
    FreeAndNil(MaskList);

  FSearching := True;
  try
    DoSearch(AppendPathDelim(ASearchPath), 0);
  finally
    FSearching := False;
    if MaskList <> nil then MaskList.Free;
  end;
end;

function GetAllFilesMask: string;
begin
  {$IFDEF WINDOWS}
  Result:='*.*';
  {$ELSE}
  Result:='*';
  {$ENDIF}
end;

function GetExeExt: string;
begin
  {$IFDEF WINDOWS}
  Result:='.exe';
  {$ELSE}
  Result:='';
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  function ReadFileToString(const Filename: string): string;
 ------------------------------------------------------------------------------}
function ReadFileToString(const Filename: String): String;
var
  SrcHandle: THandle;
  ReadCount: LongInt;
  s: String;
begin
  Result := '';
  s:='';
  try
    Setlength(s, FileSize(Filename));
    if s='' then exit;
    SrcHandle := FileOpenUTF8(Filename, fmOpenRead or fmShareDenyWrite);
    if (THandle(SrcHandle)=feInvalidHandle) then
      exit;
    try
      ReadCount:=FileRead(SrcHandle,s[1],length(s));
      if ReadCount<length(s) then
        exit;
    finally
      FileClose(SrcHandle);
    end;
    Result:=s;
  except
    // ignore errors, Result string will be empty
  end;
end;

{------------------------------------------------------------------------------
  function FileSearchUTF8(const Name, DirList: String): String;
 ------------------------------------------------------------------------------}
function FileSearchUTF8(const Name, DirList: String; ImplicitCurrentDir : Boolean = True): String;
Var
  I : longint;
  Temp : String;

begin
  Result:=Name;
  temp:=SetDirSeparators(DirList);
  // Start with checking the file in the current directory
  If ImplicitCurrentDir and (Result <> '') and FileExistsUTF8(Result) Then
    exit;
  while True do begin
    If Temp = '' then
      Break; // No more directories to search - fail
    I:=pos(PathSeparator,Temp);
    If I<>0 then
      begin
        Result:=Copy (Temp,1,i-1);
        system.Delete(Temp,1,I);
      end
    else
      begin
        Result:=Temp;
        Temp:='';
      end;
    If Result<>'' then
      Result:=IncludeTrailingPathDelimiter(Result)+name;
    If (Result <> '') and FileExistsUTF8(Result) Then
      exit;
  end;
  Result:='';
end;

{------------------------------------------------------------------------------
  function ForceDirectoriesUTF8(const Dir: string): Boolean;
 ------------------------------------------------------------------------------}
function ForceDirectoriesUTF8(const Dir: string): Boolean;

  var
    E: EInOutError;
    ADrv : String;

  function DoForceDirectories(Const Dir: string): Boolean;
  var
    ADir : String;
    APath: String;
  begin
    Result:=True;
    ADir:=ExcludeTrailingPathDelimiter(Dir);
    if (ADir='') then Exit;
    if Not DirectoryExistsUTF8(ADir) then
      begin
        APath := ExtractFilePath(ADir);
        //this can happen on Windows if user specifies Dir like \user\name/test/
        //and would, if not checked for, cause an infinite recusrsion and a stack overflow
        if (APath = ADir) then
          Result := False
        else
          Result:=DoForceDirectories(APath);
        if Result then
          Result := CreateDirUTF8(ADir);
      end;
  end;

  function IsUncDrive(const Drv: String): Boolean;
  begin
    Result := (Length(Drv) > 2) and (Drv[1] = PathDelim) and (Drv[2] = PathDelim);
  end;

begin
  Result := False;
  ADrv := ExtractFileDrive(Dir);
  if (ADrv<>'') and (not DirectoryExistsUTF8(ADrv))
  {$IFNDEF FORCEDIR_NO_UNC_SUPPORT} and (not IsUncDrive(ADrv)){$ENDIF} then Exit;
  if Dir='' then
    begin
      E:=EInOutError.Create(SCannotCreateEmptyDir);
      E.ErrorCode:=3;
      Raise E;
    end;
  Result := DoForceDirectories(SetDirSeparators(Dir));
end;

{------------------------------------------------------------------------------
  function ForceDirectoriesUTF8(const Dir: string): Boolean;
 ------------------------------------------------------------------------------}
function FileIsReadOnlyUTF8(const FileName: String): Boolean;
begin
  Result:=FileGetAttrUTF8(FileName) and faReadOnly > 0;
end;

