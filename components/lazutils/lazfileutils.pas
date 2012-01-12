unit LazFileUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazUTF8, LUResStrings;

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
function CleanAndExpandFilename(const Filename: string): string; // empty string returns current directory
function CleanAndExpandDirectory(const Filename: string): string; // empty string returns current directory
function TrimAndExpandFilename(const Filename: string): string; // empty string returns empty string
function TrimAndExpandDirectory(const Filename: string): string; // empty string returns empty string
function CreateRelativePath(const Filename, BaseDirectory: string;
                            UsePointDirectory: boolean = false): string;
function FileIsInPath(const Filename, Path: string): boolean;
function AppendPathDelim(const Path: string): string;
function ChompPathDelim(const Path: string): string;

// file operations
function FileExistsUTF8(const Filename: string): boolean;
function FileAgeUTF8(const FileName: string): Longint;
function DirectoryExistsUTF8(const Directory: string): Boolean;
function ExpandFileNameUTF8(const FileName: string): string;
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
    Result:=AnsiCompareText(Filename1, Filename2);
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
  Result:=AnsiCompareText(Filename1, Filename2);
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
    raise Exception.CreateFmt(ctsFileDoesNotExists,[AFilename]);
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

function FilenameIsAbsolute(const TheFilename: string):boolean;
begin
  {$IFDEF Windows}
  // windows
  Result:=FilenameIsWinAbsolute(TheFilename);
  {$ELSE}
  // unix
  Result:=FilenameIsUnixAbsolute(TheFilename);
  {$ENDIF}
end;

function FilenameIsWinAbsolute(const TheFilename: string): boolean;
begin
  Result:=((length(TheFilename)>=2) and (TheFilename[1] in ['A'..'Z','a'..'z'])
           and (TheFilename[2]=':'))
     or ((length(TheFilename)>=2)
         and (TheFilename[1]='\') and (TheFilename[2]='\'));
end;

function FilenameIsUnixAbsolute(const TheFilename: string): boolean;
begin
  Result:=(TheFilename<>'') and (TheFilename[1]='/');
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
  fs: TFileStream;
  Buf: string;
  Len: integer;
  NewLine: boolean;
  p: PChar;
  ZeroAllowed: Boolean;
begin
  Result:=false;
  FileReadable:=true;
  try
    fs := TFileStream.Create(UTF8ToSys(AFilename), fmOpenRead or fmShareDenyNone);
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
// trim double path delims, heading and trailing spaces
// and special dirs . and ..
var SrcPos, DestPos, l, DirStart: integer;
  c: char;
  MacroPos: LongInt;
begin
  Result:=AFilename;
  if FilenameIsTrimmed(Result) then exit;

  l:=length(AFilename);
  SrcPos:=1;
  DestPos:=1;

  // skip trailing spaces
  while (l>=1) and (AFilename[l]=' ') do dec(l);

  // skip heading spaces
  while (SrcPos<=l) and (AFilename[SrcPos]=' ') do inc(SrcPos);

  // trim double path delimiters and special dirs . and ..
  while (SrcPos<=l) do begin
    c:=AFilename[SrcPos];
    // check for double path delims
    if (c=PathDelim) then begin
      inc(SrcPos);
      {$IFDEF Windows}
      if (DestPos>2)
      {$ELSE}
      if (DestPos>1)
      {$ENDIF}
      and (Result[DestPos-1]=PathDelim) then begin
        // skip second PathDelim
        continue;
      end;
      Result[DestPos]:=c;
      inc(DestPos);
      continue;
    end;
    // check for special dirs . and ..
    if (c='.') then begin
      if (SrcPos<l) then begin
        if (AFilename[SrcPos+1]=PathDelim)
        and ((DestPos=1) or (AFilename[SrcPos-1]=PathDelim)) then begin
          // special dir ./
          // -> skip
          inc(SrcPos,2);
          continue;
        end else if (AFilename[SrcPos+1]='.')
        and (SrcPos+1=l) or (AFilename[SrcPos+2]=PathDelim) then
        begin
          // special dir ..
          //  1. ..      -> copy
          //  2. /..     -> skip .., keep /
          //  3. C:..    -> copy
          //  4. C:\..   -> skip .., keep C:\
          //  5. \\..    -> skip .., keep \\
          //  6. xxx../..   -> copy
          //  7. xxxdir/..  -> trim dir and skip ..
          //  8. xxxdir/..  -> trim dir and skip ..
          if DestPos=1 then begin
            //  1. ..      -> copy
          end else if (DestPos=2) and (Result[1]=PathDelim) then begin
            //  2. /..     -> skip .., keep /
            inc(SrcPos,2);
            continue;
          {$IFDEF Windows}
          end else if (DestPos=3) and (Result[2]=':')
          and (Result[1] in ['a'..'z','A'..'Z']) then begin
            //  3. C:..    -> copy
          end else if (DestPos=4) and (Result[2]=':') and (Result[3]=PathDelim)
          and (Result[1] in ['a'..'z','A'..'Z']) then begin
            //  4. C:\..   -> skip .., keep C:\
            inc(SrcPos,2);
            continue;
          end else if (DestPos=3) and (Result[1]=PathDelim)
          and (Result[2]=PathDelim) then begin
            //  5. \\..    -> skip .., keep \\
            inc(SrcPos,2);
            continue;
          {$ENDIF}
          end else if (DestPos>1) and (Result[DestPos-1]=PathDelim) then begin
            if (DestPos>3)
            and (Result[DestPos-2]='.') and (Result[DestPos-3]='.')
            and ((DestPos=4) or (Result[DestPos-4]=PathDelim)) then begin
              //  6. ../..   -> copy
            end else begin
              //  7. xxxdir/..  -> trim dir and skip ..
              DirStart:=DestPos-2;
              while (DirStart>1) and (Result[DirStart-1]<>PathDelim) do
                dec(DirStart);
              MacroPos:=DirStart;
              while MacroPos<DestPos do begin
                if (Result[MacroPos]='$')
                and (Result[MacroPos+1] in ['(','a'..'z','A'..'Z']) then begin
                  // 8. directory contains a macro -> keep
                  break;
                end;
                inc(MacroPos);
              end;
              if MacroPos=DestPos then begin
                DestPos:=DirStart;
                inc(SrcPos,2);
                continue;
              end;
            end;
          end;
        end;
      end else begin
        // special dir . at end of filename
        if DestPos=1 then begin
          Result:='.';
          exit;
        end else begin
          // skip
          break;
        end;
      end;
    end;
    // copy directory
    repeat
      Result[DestPos]:=c;
      inc(DestPos);
      inc(SrcPos);
      if (SrcPos>l) then break;
      c:=AFilename[SrcPos];
      if c=PathDelim then break;
    until false;
  end;
  // trim result
  if DestPos<=length(AFilename) then
    SetLength(Result,DestPos-1);
end;

{------------------------------------------------------------------------------
  function CleanAndExpandFilename(const Filename: string): string;
 ------------------------------------------------------------------------------}
function CleanAndExpandFilename(const Filename: string): string;
begin
  Result:=ExpandFileNameUTF8(TrimFileName(Filename));
end;

{------------------------------------------------------------------------------
  function CleanAndExpandDirectory(const Filename: string): string;
 ------------------------------------------------------------------------------}
function CleanAndExpandDirectory(const Filename: string): string;
begin
  Result:=AppendPathDelim(CleanAndExpandFilename(Filename));
end;

function TrimAndExpandFilename(const Filename: string): string;
begin
  Result:=ChompPathDelim(TrimFilename(Filename));
  if Result='' then exit;
  Result:=TrimFilename(ExpandFileNameUTF8(Result));
end;

function TrimAndExpandDirectory(const Filename: string): string;
begin
  Result:=TrimFilename(Filename);
  if Result='' then exit;
  Result:=TrimFilename(AppendPathDelim(ExpandFileNameUTF8(Result)));
end;

function CreateRelativePath(const Filename, BaseDirectory: string;
  UsePointDirectory: boolean): string;
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

  //WriteLn('CreateRelativePath START ',copy(CmpBaseDirectory,1,BaseDirLen),' ',copy(CmpFilename,1,FileNameLength));

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

function FileExistsUTF8(const Filename: string): boolean;
begin
  Result:=SysUtils.FileExists(UTF8ToSys(Filename));
end;

function FileAgeUTF8(const FileName: String): Longint;
begin
  Result:=SysUtils.FileAge(UTF8ToSys(Filename));
end;

function DirectoryExistsUTF8(const Directory: string): Boolean;
begin
  Result:=SysUtils.DirectoryExists(UTF8ToSys(Directory));
end;

function ExpandFileNameUTF8(const FileName: string): string;
begin
  Result:=SysToUTF8(SysUtils.ExpandFileName(UTF8ToSys(Filename)));
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

function FileGetAttrUTF8(const FileName: String): Longint;
begin
  Result:=SysUtils.FileGetAttr(UTF8ToSys(Filename));
end;

function FileSetAttrUTF8(const Filename: String; Attr: longint): Longint;
begin
  Result:=SysUtils.FileSetAttr(UTF8ToSys(Filename),Attr);
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

function GetCurrentDirUTF8: String;
begin
  Result:=SysToUTF8(SysUtils.GetCurrentDir);
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

end.

