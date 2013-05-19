unit LazFileUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazUTF8, LazUtf8Classes, LUResStrings;

{$IFDEF Windows}
  {$define CaseInsensitiveFilenames}
  {$define HasUNCPaths}
{$ENDIF}
{$IFDEF darwin}
  {$define CaseInsensitiveFilenames}
{$ENDIF}
{$IF defined(CaseInsensitiveFilenames) or defined(darwin)}
  {$DEFINE NotLiteralFilenames} // e.g. HFS+ normalizes file names
{$ENDIF}

function CompareFilenames(const Filename1, Filename2: string): integer;
function CompareFilenamesIgnoreCase(const Filename1, Filename2: string): integer;
function CompareFileExt(const Filename, Ext: string;
                        CaseSensitive: boolean): integer;
function CompareFilenameStarts(const Filename1, Filename2: string): integer;
function CompareFilenames(Filename1: PChar; Len1: integer;
  Filename2: PChar; Len2: integer): integer;
function CompareFilenamesP(Filename1, Filename2: PChar;
  IgnoreCase: boolean = false // false = use default
  ): integer;

function DirPathExists(DirectoryName: string): boolean;
function DirectoryIsWritable(const DirectoryName: string): boolean;
function ExtractFileNameOnly(const AFilename: string): string;
function FilenameIsAbsolute(const TheFilename: string):boolean;
function FilenameIsWinAbsolute(const TheFilename: string):boolean;
function FilenameIsUnixAbsolute(const TheFilename: string):boolean;
function ForceDirectory(DirectoryName: string): boolean;
procedure CheckIfFileIsExecutable(const AFilename: string);
function FileIsExecutable(const AFilename: string): boolean;
function FileIsReadable(const AFilename: string): boolean;
function FileIsWritable(const AFilename: string): boolean;
function FileIsText(const AFilename: string): boolean;
function FileIsText(const AFilename: string; out FileReadable: boolean): boolean;
function FilenameIsTrimmed(const TheFilename: string): boolean;
function FilenameIsTrimmed(StartPos: PChar; NameLen: integer): boolean;
function TrimFilename(const AFilename: string): string;
function ResolveDots(const AFilename: string): string;
function CleanAndExpandFilename(const Filename: string): string; // empty string returns current directory
function CleanAndExpandDirectory(const Filename: string): string; // empty string returns current directory
function TrimAndExpandFilename(const Filename: string; const BaseDir: string = ''): string; // empty string returns empty string
function TrimAndExpandDirectory(const Filename: string; const BaseDir: string = ''): string; // empty string returns empty string
function CreateRelativePath(const Filename, BaseDirectory: string;
                            UsePointDirectory: boolean = false): string;
function FileIsInPath(const Filename, Path: string): boolean;
function AppendPathDelim(const Path: string): string;
function ChompPathDelim(const Path: string): string;

// search paths
function CreateAbsoluteSearchPath(const SearchPath, BaseDirectory: string): string;
function CreateRelativeSearchPath(const SearchPath, BaseDirectory: string): string;
function MinimizeSearchPath(const SearchPath: string): string;
function FindPathInSearchPath(APath: PChar; APathLen: integer;
                              SearchPath: PChar; SearchPathLen: integer): PChar;

// file operations
function FileExistsUTF8(const Filename: string): boolean;
function FileAgeUTF8(const FileName: string): Longint;
function DirectoryExistsUTF8(const Directory: string): Boolean;
function ExpandFileNameUTF8(const FileName: string; {const} BaseDir: string = ''): string;
function FindFirstUTF8(const Path: string; Attr: Longint; out Rslt: TSearchRec): Longint;
function FindNextUTF8(var Rslt: TSearchRec): Longint;
procedure FindCloseUTF8(var F: TSearchrec);
function FileSetDateUTF8(const FileName: String; Age: Longint): Longint;
function FileGetAttrUTF8(const FileName: String): Longint;
function FileSetAttrUTF8(const Filename: String; Attr: longint): Longint;
function DeleteFileUTF8(const FileName: String): Boolean;
function RenameFileUTF8(const OldName, NewName: String): Boolean;
function FileSearchUTF8(const Name, DirList : String): String;
function FileIsReadOnlyUTF8(const FileName: String): Boolean;
function GetCurrentDirUTF8: String;
function SetCurrentDirUTF8(const NewDir: String): Boolean;
function CreateDirUTF8(const NewDir: String): Boolean;
function RemoveDirUTF8(const Dir: String): Boolean;
function ForceDirectoriesUTF8(const Dir: string): Boolean;

function FileOpenUTF8(Const FileName : string; Mode : Integer) : THandle;
function FileCreateUTF8(Const FileName : string) : THandle; overload;
function FileCreateUTF8(Const FileName : string; Rights: Cardinal) : THandle; overload;
Function FileCreateUtf8(Const FileName : String; ShareMode : Integer; Rights : Cardinal) : THandle; overload;


// UNC paths
function IsUNCPath(const {%H-}Path: String): Boolean;
function ExtractUNCVolume(const {%H-}Path: String): String;

procedure SplitCmdLineParams(const Params: string; ParamList: TStrings;
                             ReadBackslash: boolean = false);

type
  TInvalidateFileStateCacheEvent = procedure(const Filename: string);
var
  OnInvalidateFileStateCache: TInvalidateFileStateCacheEvent = nil;
procedure InvalidateFileStateCache(const Filename: string = ''); inline;

implementation

// to get more detailed error messages consider the os
uses
{$IFDEF Windows}
  Windows;
{$ELSE}
  {$IFDEF darwin}
  MacOSAll,
  {$ENDIF}
  Unix, BaseUnix;
{$ENDIF}

{$I lazfileutils.inc}
{$IFDEF windows}
  {$I winlazfileutils.inc}
{$ELSE}
  {$I unixlazfileutils.inc}
{$ENDIF}

function CompareFilenames(const Filename1, Filename2: string): integer;
{$IFDEF darwin}
var
  F1: CFStringRef;
  F2: CFStringRef;
{$ENDIF}
begin
  {$IFDEF darwin}
  if Filename1=Filename2 then exit(0);
  if (Filename1='') or (Filename2='') then
    exit(length(Filename2)-length(Filename1));
  F1:=CFStringCreateWithCString(nil,Pointer(Filename1),kCFStringEncodingUTF8);
  F2:=CFStringCreateWithCString(nil,Pointer(Filename2),kCFStringEncodingUTF8);
  Result:=CFStringCompare(F1,F2,kCFCompareNonliteral
          {$IFDEF CaseInsensitiveFilenames}+kCFCompareCaseInsensitive{$ENDIF});
  CFRelease(F1);
  CFRelease(F2);
  {$ELSE}
    {$IFDEF CaseInsensitiveFilenames}
    Result:=UTF8CompareText(Filename1, Filename2);
    {$ELSE}
    Result:=CompareStr(Filename1, Filename2);
    {$ENDIF}
  {$ENDIF}
end;

function CompareFilenamesIgnoreCase(const Filename1, Filename2: string
  ): integer;
{$IFDEF darwin}
var
  F1: CFStringRef;
  F2: CFStringRef;
{$ENDIF}
begin
  {$IFDEF darwin}
  if Filename1=Filename2 then exit(0);
  F1:=CFStringCreateWithCString(nil,Pointer(Filename1),kCFStringEncodingUTF8);
  F2:=CFStringCreateWithCString(nil,Pointer(Filename2),kCFStringEncodingUTF8);
  Result:=CFStringCompare(F1,F2,kCFCompareNonliteral+kCFCompareCaseInsensitive);
  CFRelease(F1);
  CFRelease(F2);
  {$ELSE}
  Result:=UTF8CompareText(Filename1, Filename2);
  {$ENDIF}
end;

function CompareFileExt(const Filename, Ext: string;
  CaseSensitive: boolean): integer;
var
  FileLen, FilePos, ExtLen, ExtPos: integer;
  FileChar, ExtChar: char;
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
  while true do begin
    if FilePos<=FileLen then begin
      if ExtPos<=ExtLen then begin
        FileChar:=Filename[FilePos];
        ExtChar:=Ext[ExtPos];
        if not CaseSensitive then begin
          FileChar:=FPUpChars[FileChar];
          ExtChar:=FPUpChars[ExtChar];
        end;
        if FileChar=ExtChar then begin
          inc(FilePos);
          inc(ExtPos);
        end else if FileChar>ExtChar then begin
          Result:=1;
          exit;
        end else begin
          Result:=-1;
          exit;
        end;
      end else begin
        // fileext longer than ext
        Result:=1;
        exit;
      end;
    end else begin
      if ExtPos<=ExtLen then begin
        // fileext shorter than ext
        Result:=-1;
        exit;
      end else begin
        // equal
        Result:=0;
        exit;
      end;
    end;
  end;
end;

function FileIsExecutable(const AFilename: string): boolean;
{$IFNDEF WINDOWS}
var
  Info : Stat;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  Result:=FileExistsUTF8(AFilename);
  {$ELSE}
  // first check AFilename is not a directory and then check if executable
  Result:= (FpStat(AFilename,info{%H-})<>-1) and FPS_ISREG(info.st_mode) and
           (BaseUnix.FpAccess(AFilename,BaseUnix.X_OK)=0);
  {$ENDIF}
end;

procedure CheckIfFileIsExecutable(const AFilename: string);
{$IFNDEF Windows}
var AText: string;
{$ENDIF}
begin
  // TProcess does not report, if a program can not be executed
  // to get good error messages consider the OS
  if not FileExistsUTF8(AFilename) then begin
    raise Exception.CreateFmt(ctsFileDoesNotExist,[AFilename]);
  end;
  {$IFNDEF Windows}
  if not(BaseUnix.FpAccess(AFilename,BaseUnix.X_OK)=0) then
  begin
    AText:='"'+AFilename+'"';
    case fpGetErrno of
    ESysEAcces:
      AText:='read access denied for '+AText;
    ESysENoEnt:
      AText:='a directory component in '+AText
                          +' does not exist or is a dangling symlink';
    ESysENotDir:
      AText:='a directory component in '+Atext+' is not a directory';
    ESysENoMem:
      AText:='insufficient memory';
    ESysELoop:
      AText:=AText+' has a circular symbolic link';
    else
      AText:=Format(ctsFileIsNotExecutable,[AText]);
    end;
    raise Exception.Create(AText);
  end;
  {$ENDIF}

  // ToDo: windows and xxxbsd
end;

function ExtractFileNameOnly(const AFilename: string): string;
var ExtLen: integer;
begin
  // beware: filename.ext1.ext2
  Result:=ExtractFilename(AFilename);
  ExtLen:=length(ExtractFileExt(Result));
  Result:=copy(Result,1,length(Result)-ExtLen);
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

function CompareFilenameStarts(const Filename1, Filename2: string): integer;
var
  len1: Integer;
  len2: Integer;
begin
  len1:=length(Filename1);
  len2:=length(Filename2);
  if len1=len2 then begin
    Result:=CompareFilenames(Filename1,Filename2);
    exit;
  end else if len1>len2 then
    Result:=CompareFilenames(copy(Filename1,1,len2),Filename2)
  else
    Result:=CompareFilenames(Filename1,copy(Filename2,1,len1));
  if Result<>0 then exit;
  if len1<len2 then
    Result:=-1
  else
    Result:=1;
end;

function CompareFilenames(Filename1: PChar; Len1: integer; Filename2: PChar;
  Len2: integer): integer;
var
  {$IFDEF NotLiteralFilenames}
  File1: string;
  File2: string;
  {$ELSE}
  i: Integer;
  {$ENDIF}
begin
  if (Len1=0) or (Len2=0) then begin
    Result:=Len1-Len2;
    exit;
  end;
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

function CompareFilenamesP(Filename1, Filename2: PChar;
  IgnoreCase: boolean = false): integer;
var
  {$IFDEF darwin}
  F1: CFStringRef;
  F2: CFStringRef;
  Flags: CFStringCompareFlags;
  {$ELSE}
  File1, File2: string;
  Len1: SizeInt;
  Len2: SizeInt;
  {$ENDIF}
begin
  if (Filename1=nil) or (Filename1^=#0) then begin
    if (Filename2=nil) or (Filename2^=#0) then begin
      // both empty
      exit(0);
    end else begin
      // filename1 empty, filename2 not empty
      exit(-1);
    end;
  end else if (Filename2=nil) or (Filename2^=#0) then begin
    // filename1 not empty, filename2 empty
    exit(1);
  end;

  {$IFDEF CaseInsensitiveFilenames}
  // this platform is by default case insensitive
  IgnoreCase:=true;
  {$ENDIF}
  {$IFDEF darwin}
  F1:=CFStringCreateWithCString(nil,Pointer(Filename1),kCFStringEncodingUTF8);
  F2:=CFStringCreateWithCString(nil,Pointer(Filename2),kCFStringEncodingUTF8);
  Flags:=kCFCompareNonliteral;
  if IgnoreCase then Flags+=kCFCompareCaseInsensitive;
  Result:=CFStringCompare(F1,F2,Flags);
  CFRelease(F1);
  CFRelease(F2);
  {$ELSE}
  if IgnoreCase then begin
    // compare case insensitive
    Len1:=StrLen(Filename1);
    SetLength(File1,Len1);
    System.Move(Filename1^,File1[1],Len1);
    Len2:=StrLen(Filename2);
    SetLength(File2,Len2);
    System.Move(Filename2^,File2[1],Len2);
    Result:=UTF8CompareText(File1,File2);
  end else begin
    // compare literally
    while (Filename1^=Filename2^) and (Filename1^<>#0) do begin
      inc(Filename1);
      Inc(Filename2);
    end;
    Result:=ord(Filename1^)-ord(Filename2^);
  end;
  {$ENDIF}
end;

function DirPathExists(DirectoryName: string): boolean;
begin
  Result:=DirectoryExistsUTF8(ChompPathDelim(DirectoryName));
end;

function DirectoryIsWritable(const DirectoryName: string): boolean;
var
  TempFilename: String;
  fs: TFileStream;
  s: String;
begin
  TempFilename:=SysUtils.GetTempFilename(AppendPathDelim(DirectoryName),'tstperm');
  Result:=false;
  try
    fs:=TFileStream.Create(UTF8ToSys(TempFilename),fmCreate);
    s:='WriteTest';
    fs.Write(s[1],length(s));
    fs.Free;
    if not DeleteFileUTF8(TempFilename) then
      InvalidateFileStateCache(TempFilename);
    Result:=true;
  except
  end;
end;

function ForceDirectory(DirectoryName: string): boolean;
var i: integer;
  Dir: string;
begin
  DoDirSeparators(DirectoryName);
  DirectoryName:=AppendPathDelim(DirectoryName);
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

function FileIsReadable(const AFilename: string): boolean;
begin
  {$IFDEF Windows}
  Result:=true;
  {$ELSE}
  Result:= BaseUnix.FpAccess(AFilename,BaseUnix.R_OK)=0;
  {$ENDIF}
end;

function FileIsWritable(const AFilename: string): boolean;
begin
  {$IFDEF Windows}
  Result:=((FileGetAttrUTF8(AFilename) and faReadOnly)=0);
  {$ELSE}
  Result:= BaseUnix.FpAccess(AFilename,BaseUnix.W_OK)=0;
  {$ENDIF}
end;

function FileIsText(const AFilename: string): boolean;
var
  FileReadable: Boolean;
begin
  Result:=FileIsText(AFilename,FileReadable);
  if FileReadable then ;
end;

function FileIsText(const AFilename: string; out FileReadable: boolean): boolean;
var
  fs: TFileStreamUtf8;
  Buf: string;
  Len: integer;
  NewLine: boolean;
  p: PChar;
  ZeroAllowed: Boolean;
begin
  Result:=false;
  FileReadable:=true;
  try
    fs := TFileStreamUtf8.Create(AFilename, fmOpenRead or fmShareDenyNone);
    try
      // read the first 1024 bytes
      Len:=1024;
      SetLength(Buf,Len+1);
      Len:=fs.Read(Buf[1],Len);
      if Len>0 then begin
        Buf[Len+1]:=#0;
        p:=PChar(Buf);
        ZeroAllowed:=false;
        if (p[0]=#$EF) and (p[1]=#$BB) and (p[2]=#$BF) then begin
          // UTF-8 BOM (Byte Order Mark)
          inc(p,3);
        end else if (p[0]=#$FF) and (p[1]=#$FE) then begin
          // ucs-2le BOM FF FE
          inc(p,2);
          ZeroAllowed:=true;
        end else if (p[0]=#$FE) and (p[1]=#$FF) then begin
          // ucs-2be BOM FE FF
          inc(p,2);
          ZeroAllowed:=true;
        end;
        NewLine:=false;
        while true do begin
          case p^ of
          #0:
            if p-PChar(Buf)>=Len then
              break
            else if not ZeroAllowed then
              exit;
          // #10,#13: new line
          // #12: form feed
          // #26: end of file
          #1..#8,#11,#14..#25,#27..#31: exit;
          #10,#13: NewLine:=true;
          end;
          inc(p);
        end;
        if NewLine or (Len<1024) then
          Result:=true;
      end else
        Result:=true;
    finally
      fs.Free;
    end;
  except
    on E: Exception do begin
      FileReadable:=false;
    end;
  end;
end;

function FilenameIsTrimmed(const TheFilename: string): boolean;
begin
  Result:=FilenameIsTrimmed(PChar(Pointer(TheFilename)),// pointer type cast avoids #0 check
                            length(TheFilename));
end;

function FilenameIsTrimmed(StartPos: PChar; NameLen: integer): boolean;
var
  i: Integer;
begin
  Result:=false;
  if NameLen<=0 then begin
    Result:=true;
    exit;
  end;
  // check heading spaces
  if StartPos[0]=' ' then exit;
  // check trailing spaces
  if StartPos[NameLen-1]=' ' then exit;
  // check ./ at start
  if (StartPos[0]='.') and (StartPos[1]=PathDelim) then exit;
  i:=0;
  while i<NameLen do begin
    if StartPos[i]<>PathDelim then
      inc(i)
    else begin
      inc(i);
      if i=NameLen then break;

      // check for double path delimiter
      if (StartPos[i]=PathDelim) then exit;

      if (StartPos[i]='.') and (i>0) then begin
        inc(i);
        // check /./ or /. at end
        if (StartPos[i]=PathDelim) or (i=NameLen) then exit;
        if StartPos[i]='.' then begin
          inc(i);
          // check /../ or /.. at end
          if (StartPos[i]=PathDelim) or (i=NameLen) then exit;
        end;
      end;
    end;
  end;
  Result:=true;
end;

function TrimFilename(const AFilename: string): string;
//Trim leading and trailing spaces
//then call ResolveDots to trim double path delims and expand special dirs like .. and .

var
  Len, Start: Integer;
begin
  Result := AFileName;
  Len := Length(AFileName);
  if (Len > 0) and not FilenameIsTrimmed(Result) then
  begin
    Start := 1;
    while (Len > 0) and (AFileName[Len] = #32) do Dec(Len);
    while (Start <= Len) and (AFilename[Start] = #32) do Inc(Start);
    if Start > 1 then System.Delete(Result,1,Start-1);
    SetLength(Result, Len - (Start - 1));
    Result := ResolveDots(Result);
  end;
end;

{------------------------------------------------------------------------------
  function CleanAndExpandFilename(const Filename: string): string;
 ------------------------------------------------------------------------------}
function CleanAndExpandFilename(const Filename: string): string;
begin
  Result:=ExpandFileNameUTF8(TrimFileName(SetDirSeparators(Filename)));
end;

{------------------------------------------------------------------------------
  function CleanAndExpandDirectory(const Filename: string): string;
 ------------------------------------------------------------------------------}
function CleanAndExpandDirectory(const Filename: string): string;
begin
  Result:=AppendPathDelim(CleanAndExpandFilename(Filename));
end;

function TrimAndExpandFilename(const Filename: string; const BaseDir: string): string;
begin
  Result:=ChompPathDelim(TrimFilename(SetDirSeparators(Filename)));
  if Result='' then exit;
  Result:=TrimFilename(ExpandFileNameUTF8(Result,BaseDir));
end;

function TrimAndExpandDirectory(const Filename: string; const BaseDir: string): string;
begin
  Result:=TrimFilename(SetDirSeparators(Filename));
  if Result='' then exit;
  Result:=TrimFilename(AppendPathDelim(ExpandFileNameUTF8(Result,BaseDir)));
end;

function CreateRelativePath(const Filename, BaseDirectory: string;
  UsePointDirectory: boolean): string;
{
Creates a relative path from BaseDirectory to Filename.
A trailing path delimiter of BaseDirectory is ignored.
If there is no relative path it returns Filename.
If BaseDirectory and Filename are the same and UsePointDirectory is false it
returns the empty string. If UsePointDirectory is true it returns '.'.
Duplicate path delimiters are treated as one.

In other words if it returns a relative file name then the following is true:
TrimFilename(Filename) = TrimFilename(BaseDirectory+PathDelim+Result).

This function is thread safe and therefore does not support current directories
as needed by Windows file names like D:test.

Filename='/a' BaseDir='/a' Result=''
Filename='/a' BaseDir='/a' UsePointDirectory=true Result='.'
Filename='/a' BaseDir='/a/' Result=''
Filename='/a/b' BaseDir='/a/b' Result=''
Filename='/a/b' BaseDir='/a/b/' Result=''
Filename='/a' BaseDir='/a/' Result=''
Filename='/a' BaseDir='' Result='/a'
Filename='/a/b' BaseDir='/a' Result='b'
Filename='/a/b' BaseDir='/a/' Result='b'
Filename='/a/b' BaseDir='/a//' Result='b'
Filename='/a' BaseDir='/a/b' Result='../'
Filename='/a' BaseDir='/a/b/' Result='../'
Filename='/a' BaseDir='/a/b//' Result='../'
Filename='/a/' BaseDir='/a/b' Result='../'
Filename='/a' BaseDir='/a/b/c' Result='../../'
Filename='/a' BaseDir='/a/b//c' Result='../../'
Filename='/a' BaseDir='/a//b/c' Result='../../'
Filename='/a' BaseDir='/a//b/c/' Result='../../'
Filename='/a' BaseDir='/b' Result='/a'

}
var
  FileNameLength: Integer;
  BaseDirLen: Integer;
  SamePos: Integer;
  UpDirCount: Integer;
  BaseDirPos: Integer;
  ResultPos: Integer;
  i: Integer;
  FileNameRestLen: Integer;
  CmpBaseDirectory: String;
  CmpFilename: String;
  p: Integer;
  DirCount: Integer;
begin
  Result:=Filename;
  if (BaseDirectory='') or (Filename='') then exit;

  {$IFDEF Windows}
  // check for different windows file drives
  if (CompareText(ExtractFileDrive(Filename),
                  ExtractFileDrive(BaseDirectory))<>0)
  then
    exit;
  {$ENDIF}
  CmpBaseDirectory:=BaseDirectory;
  CmpFilename:=Filename;
  {$IFDEF darwin}
  CmpBaseDirectory:=GetDarwinSystemFilename(CmpBaseDirectory);
  CmpFilename:=GetDarwinSystemFilename(CmpFilename);
  {$ENDIF}
  {$IFDEF CaseInsensitiveFilenames}
  CmpBaseDirectory:=AnsiUpperCaseFileName(CmpBaseDirectory);
  CmpFilename:=AnsiUpperCaseFileName(CmpFilename);
  {$ENDIF}

  FileNameLength:=length(CmpFilename);
  while (FileNameLength>0) and (CmpFilename[FileNameLength]=PathDelim) do
    dec(FileNameLength);
  BaseDirLen:=length(CmpBaseDirectory);
  while (BaseDirLen>0) and (CmpBaseDirectory[BaseDirLen]=PathDelim) do
    dec(BaseDirLen);
  if BaseDirLen=0 then exit;

  //DebugLn(['CreateRelativePath START ',copy(CmpBaseDirectory,1,BaseDirLen),' ',copy(CmpFilename,1,FileNameLength)]);

  // count shared directories
  p:=1;
  DirCount:=0;
  BaseDirPos:=p;
  while (p<=FileNameLength) and (BaseDirPos<=BaseDirLen)
  and (CmpFileName[p]=CmpBaseDirectory[BaseDirPos]) do
  begin
    if CmpFilename[p]=PathDelim then
    begin
      inc(DirCount);
      repeat
        inc(p);
      until (p>FileNameLength) or (CmpFilename[p]<>PathDelim);
      repeat
        inc(BaseDirPos);
      until (BaseDirPos>BaseDirLen) or (CmpBaseDirectory[BaseDirPos]<>PathDelim);
    end else begin
      inc(p);
      inc(BaseDirPos);
    end;
  end;
  UpDirCount:=0;
  if ((BaseDirPos>BaseDirLen) or (CmpBaseDirectory[BaseDirPos]=PathDelim))
  and ((p>FileNameLength) or (CmpFilename[p]=PathDelim)) then
  begin
    // for example File=/a BaseDir=/a/b
    inc(DirCount);
  end else begin
    // for example File=/aa BaseDir=/ab
    inc(UpDirCount);
  end;
  if DirCount=0 then exit;
  if FilenameIsAbsolute(BaseDirectory) and (DirCount=1) then exit;

  // calculate needed up directories
  while (BaseDirPos<=BaseDirLen) do begin
    if (CmpBaseDirectory[BaseDirPos]=PathDelim) then
    begin
      inc(UpDirCount);
      repeat
        inc(BaseDirPos);
      until (BaseDirPos>BaseDirLen) or (CmpBaseDirectory[BaseDirPos]<>PathDelim);
    end else
      inc(BaseDirPos);
  end;

  // create relative filename
  SamePos:=1;
  p:=0;
  FileNameLength:=length(Filename);
  while (SamePos<=FileNameLength) do begin
    if (Filename[SamePos]=PathDelim) then begin
      repeat
        inc(SamePos);
      until (SamePos>FileNameLength) or (Filename[SamePos]<>PathDelim);
      inc(p);
      if p>=DirCount then
        break;
    end else
      inc(SamePos);
  end;
  FileNameRestLen:=FileNameLength-SamePos+1;
  //writeln('DirCount=',DirCount,' UpDirCount=',UpDirCount,' FileNameRestLen=',FileNameRestLen,' SamePos=',SamePos);
  SetLength(Result,3*UpDirCount+FileNameRestLen);
  ResultPos:=1;
  for i:=1 to UpDirCount do begin
    Result[ResultPos]:='.';
    Result[ResultPos+1]:='.';
    Result[ResultPos+2]:=PathDelim;
    inc(ResultPos,3);
  end;
  if FileNameRestLen>0 then
    System.Move(Filename[SamePos],Result[ResultPos],FileNameRestLen);

  if UsePointDirectory and (Result='') and (Filename<>'') then
    Result:='.'; // Filename is the BaseDirectory
end;

{------------------------------------------------------------------------------
  function FileIsInPath(const Filename, Path: string): boolean;
 ------------------------------------------------------------------------------}
function FileIsInPath(const Filename, Path: string): boolean;
var
  ExpFile: String;
  ExpPath: String;
  l: integer;
begin
  if Path='' then begin
    Result:=false;
    exit;
  end;
  ExpFile:=TrimFilename(Filename);
  ExpPath:=AppendPathDelim(TrimFilename(Path));
  l:=length(ExpPath);
  Result:=(l>0) and (length(ExpFile)>l) and (ExpFile[l]=PathDelim)
          and (CompareFilenames(ExpPath,LeftStr(ExpFile,l))=0);
end;

function AppendPathDelim(const Path: string): string;
begin
  if (Path<>'') and (Path[length(Path)]<>PathDelim) then
    Result:=Path+PathDelim
  else
    Result:=Path;
end;

function ChompPathDelim(const Path: string): string;
var
  Len, MinLen: Integer;
begin
  if Path = '' then
    exit;

  Result:=Path;
  Len:=length(Result);
  if (Result[1]=PathDelim) then begin
    MinLen := 1;
    {$IFDEF HasUNCPaths}
    if (Len >= 2) and (Result[2]=PathDelim) then
      MinLen := 2; // keep UNC '\\', chomp 'a\' to 'a'
    {$ENDIF}
  end
  else begin
    MinLen := 0;
    {$IFdef MSWindows}
    if (Len >= 3) and (Result[1] in ['a'..'z', 'A'..'Z'])  and
       (Result[2] = ':') and (Result[3]=PathDelim)
    then
      MinLen := 3;
    {$ENDIF}
  end;

  while (Len > MinLen) and (Result[Len]=PathDelim) do dec(Len);
  if Len<length(Result) then
    SetLength(Result,Len);
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

function CreateRelativeSearchPath(const SearchPath, BaseDirectory: string
  ): string;
var
  PathLen: Integer;
  EndPos: Integer;
  StartPos: Integer;
  CurDir: String;
  NewCurDir: String;
  DiffLen: Integer;
begin
  Result:=SearchPath;
  if (SearchPath='') or (BaseDirectory='') then exit;

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
    if FilenameIsAbsolute(CurDir) then begin
      NewCurDir:=CreateRelativePath(CurDir,BaseDirectory);
      if (NewCurDir<>CurDir) and (NewCurDir='') then
        NewCurDir:='.';
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

function MinimizeSearchPath(const SearchPath: string): string;
// trim the paths, remove doubles and empty paths
var
  StartPos: Integer;
  EndPos: LongInt;
  NewPath: String;
begin
  Result:=SearchPath;
  StartPos:=1;
  while StartPos<=length(Result) do begin
    EndPos:=StartPos;
    while (EndPos<=length(Result)) and (Result[EndPos]<>';') do
      inc(EndPos);
    if StartPos<EndPos then begin
      // trim path and chomp PathDelim
      if (Result[EndPos-1]=PathDelim)
      or (not FilenameIsTrimmed(@Result[StartPos],EndPos-StartPos)) then begin
        NewPath:=ChompPathDelim(
                           TrimFilename(copy(Result,StartPos,EndPos-StartPos)));
        Result:=copy(Result,1,StartPos-1)+NewPath+copy(Result,EndPos,length(Result));
        EndPos:=StartPos+length(NewPath);
      end;
      // check if path already exists
      if (Length(Result) > 0) and
         (FindPathInSearchPath(@Result[StartPos],EndPos-StartPos, @Result[1],StartPos-1) <> nil)
      then begin
        // remove path
        System.Delete(Result,StartPos,EndPos-StartPos+1);
      end else begin
        StartPos:=EndPos+1;
      end;
    end else begin
      // remove empty path
      System.Delete(Result,StartPos,1);
    end;
  end;
  if (Result<>'') and (Result[length(Result)]=';') then
    SetLength(Result,length(Result)-1);
end;

function FindPathInSearchPath(APath: PChar; APathLen: integer;
  SearchPath: PChar; SearchPathLen: integer): PChar;
var
  StartPos: Integer;
  EndPos: LongInt;
  NextStartPos: LongInt;
  CmpPos: LongInt;
  UseQuickCompare: Boolean;
  PathStr: String;
  CurFilename: String;
begin
  Result:=nil;
  if SearchPath=nil then exit;
  if (APath=nil) or (APathLen=0) then exit;
  // ignore trailing PathDelim at end
  while (APathLen>1) and (APath[APathLen-1]=PathDelim) do dec(APathLen);

  {$IFDEF CaseInsensitiveFilenames}
  UseQuickCompare:=false;
  {$ELSE}
    {$IFDEF NotLiteralFilenames}
    CmpPos:=0;
    while (CmpPos<APathLen) and (ord(APath[CmpPos]<128)) do inc(CmpPos);
    UseQuickCompare:=CmpPos=APathLen;
    {$ELSE}
    UseQuickCompare:=true;
    {$ENDIF}
  {$ENDIF}
  if not UseQuickCompare then begin
    SetLength(PathStr,APathLen);
    System.Move(APath^,PathStr[1],APathLen);
  end;

  StartPos:=0;
  while StartPos<SearchPathLen do begin
    // find current path bounds
    NextStartPos:=StartPos;
    while (SearchPath[NextStartPos]<>';') and (NextStartPos<SearchPathLen) do
      inc(NextStartPos);
    EndPos:=NextStartPos;
    // ignore trailing PathDelim at end
    while (EndPos>StartPos+1) and (SearchPath[EndPos-1]=PathDelim) do
      dec(EndPos);
    // compare current path
    if UseQuickCompare then begin
      if EndPos-StartPos=APathLen then begin
        CmpPos:=0;
        while CmpPos<APathLen do begin
          if APath[CmpPos]<>SearchPath[StartPos+CmpPos] then
            break;
          inc(CmpPos);
        end;
        if CmpPos=APathLen then begin
          Result:=@SearchPath[StartPos];
          exit;
        end;
      end;
    end else if EndPos>StartPos then begin
      // use CompareFilenames
      CurFilename:='';
      SetLength(CurFilename,EndPos-StartPos);
      System.Move(SearchPath[StartPos],CurFilename[1],EndPos-StartPos);
      if CompareFilenames(PathStr,CurFilename)=0 then begin
        Result:=@SearchPath[StartPos];
        exit;
      end;
    end;
    StartPos:=NextStartPos+1;
  end;
end;



function FileAgeUTF8(const FileName: String): Longint;
begin
  Result:=SysUtils.FileAge(UTF8ToSys(Filename));
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

function FileSetDateUTF8(const FileName: String; Age: Longint): Longint;
begin
  Result:=SysUtils.FileSetDate(UTF8ToSys(Filename),Age);
  InvalidateFileStateCache(Filename);
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

function FileSearchUTF8(const Name, DirList: String): String;
begin
  Result:=SysToUTF8(SysUtils.FileSearch(UTF8ToSys(Name),UTF8ToSys(DirList)));
end;

function FileIsReadOnlyUTF8(const FileName: String): Boolean;
begin
  Result:=SysUtils.FileIsReadOnly(UTF8ToSys(Filename));
end;

function SetCurrentDirUTF8(const NewDir: String): Boolean;
begin
  Result:=SysUtils.SetCurrentDir(UTF8ToSys(NewDir));
end;

function CreateDirUTF8(const NewDir: String): Boolean;
begin
  Result:=SysUtils.CreateDir(UTF8ToSys(NewDir));
end;

function RemoveDirUTF8(const Dir: String): Boolean;
begin
  Result:=SysUtils.RemoveDir(UTF8ToSys(Dir));
end;

function ForceDirectoriesUTF8(const Dir: string): Boolean;
begin
  Result:=SysUtils.ForceDirectories(UTF8ToSys(Dir));
end;

procedure InvalidateFileStateCache(const Filename: string);
begin
  if Assigned(OnInvalidateFileStateCache) then
    OnInvalidateFileStateCache(Filename);
end;

procedure SplitCmdLineParams(const Params: string; ParamList: TStrings;
                             ReadBackslash: boolean = false);
// split spaces, quotes are parsed as single parameter
// if ReadBackslash=true then \" is replaced to " and not treated as quote
// #0 is always end
type
  TMode = (mNormal,mApostrophe,mQuote);
var
  p: Integer;
  Mode: TMode;
  Param: String;
begin
  p:=1;
  while p<=length(Params) do
  begin
    // skip whitespace
    while (p<=length(Params)) and (Params[p] in [' ',#9,#10,#13]) do inc(p);
    if (p>length(Params)) or (Params[p]=#0) then
      break;
    //writeln('SplitCmdLineParams After Space p=',p,'=[',Params[p],']');
    // read param
    Param:='';
    Mode:=mNormal;
    while p<=length(Params) do
    begin
      case Params[p] of
      #0:
        break;
      '\':
        begin
          inc(p);
          if ReadBackslash then
            begin
            // treat next character as normal character
            if (p>length(Params)) or (Params[p]=#0) then
              break;
            if ord(Params[p])<128 then
            begin
              Param+=Params[p];
              inc(p);
            end else begin
              // next character is already a normal character
            end;
          end else begin
            // treat backslash as normal character
            Param+='\';
          end;
        end;
      '''':
        begin
          inc(p);
          case Mode of
          mNormal:
            Mode:=mApostrophe;
          mApostrophe:
            Mode:=mNormal;
          mQuote:
            Param+='''';
          end;
        end;
      '"':
        begin
          inc(p);
          case Mode of
          mNormal:
            Mode:=mQuote;
          mApostrophe:
            Param+='"';
          mQuote:
            Mode:=mNormal;
          end;
        end;
      ' ',#9,#10,#13:
        begin
          if Mode=mNormal then break;
          Param+=Params[p];
          inc(p);
        end;
      else
        Param+=Params[p];
        inc(p);
      end;
    end;
    //writeln('SplitCmdLineParams Param=#'+Param+'#');
    ParamList.Add(Param);
  end;
end;

function IsUNCPath(const Path: String): Boolean;
begin
  {$IFDEF Windows}
  Result := (Length(Path) > 2) and (Path[1] = PathDelim) and (Path[2] = PathDelim);
  {$ELSE}
  Result := false;
  {$ENDIF}
end;

function ExtractUNCVolume(const Path: String): String;
{$IFDEF Windows}
var
  I, Len: Integer;

  // the next function reuses Len variable
  function NextPathDelim(const Start: Integer): Integer;// inline;
  begin
    Result := Start;
    while (Result <= Len) and (Path[Result] <> PathDelim) do
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
    if Path[I] <> PathDelim then
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
{$ELSE}
begin
  Result := '';
end;
{$ENDIF}

initialization
  InitLazFileUtils;

end.

