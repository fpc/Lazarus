{%MainUnit ../filectrl.pp}
{******************************************************************************
                                  Filectrl
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
 
var
  FNeedRTLAnsi: boolean = false;
  FNeedRTLAnsiValid: boolean = false;


procedure SetNeedRTLAnsi(NewValue: boolean);
begin
  FNeedRTLAnsi:=NewValue;
  FNeedRTLAnsiValid:=true;
end;

function IsASCII(const s: string): boolean; inline;
var
  i: Integer;
begin
  for i:=1 to length(s) do if ord(s[i])>127 then exit(false);
  Result:=true;
end;

function UTF8ToSys(const s: string): string;
begin
  if NeedRTLAnsi and (not IsASCII(s)) then
    Result := UTF8ToAnsi(s)
  else
    Result := s;
end;

function SysToUTF8(const s: string): string;
begin
  if NeedRTLAnsi and (not IsASCII(s)) then
    Result:=AnsiToUTF8(s)
  else
    Result:=s;
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

function FileAgeUTF8(const FileName: String): Longint;
begin
  Result:=SysUtils.FileAge(UTF8ToSys(Filename));
end;

function ExpandFileNameUTF8(const FileName: string): string;
begin
  Result:=SysToUTF8(SysUtils.ExpandFileName(UTF8ToSys(Filename)));
end;

function ExpandUNCFileNameUTF8(const FileName: string): string;
begin
  Result:=SysToUTF8(SysUtils.ExpandUNCFileName(UTF8ToSys(Filename)));
end;

function ExtractShortPathNameUTF8(const FileName: String): String;
begin
  {$ifdef ver2_2_0}
  Result := '';
  {$else}
  Result:=SysToUTF8(SysUtils.ExtractShortPathName(UTF8ToSys(FileName)));
  {$endif}
end;

function FileSetDateUTF8(const FileName: String; Age: Longint): Longint;
begin
  Result:=SysUtils.FileSetDate(UTF8ToSys(Filename),Age);
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

function GetAppConfigDirUTF8(Global: Boolean): string;
begin
  Result:=SysToUTF8(SysUtils.GetAppConfigDir(Global));
end;

function GetAppConfigFileUTF8(Global: Boolean; SubDir: boolean): string;
begin
  Result:=SysToUTF8(SysUtils.GetAppConfigFile(Global,SubDir));
end;

function SysErrorMessageUTF8(ErrorCode: Integer): String;
begin
  Result:=SysToUTF8(SysUtils.SysErrorMessage(ErrorCode));
end;

{------------------------------------------------------------------------------
  DirPathExists
 ------------------------------------------------------------------------------}
function DirPathExists(const FileName: String): Boolean;
var
  F: Longint;
  dirExist: Boolean;
begin
  dirExist := false;

  F := FileGetAttrUTF8(ChompPathDelim(FileName));
  if F <> -1 then
    if (F and faDirectory) <> 0 then
      dirExist := true;
  Result := dirExist;
end;

{------------------------------------------------------------------------------
  function CompareFilenames(const Filename1, Filename2: string): integer;
 ------------------------------------------------------------------------------}
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

{------------------------------------------------------------------------------
  function CompareFilenames(const Filename1, Filename2: string;
    ResolveLinks: boolean): integer;
 ------------------------------------------------------------------------------}
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

{------------------------------------------------------------------------------
  function FilenameIsPascalUnit(const Filename: string): boolean;
 ------------------------------------------------------------------------------}
function FilenameIsPascalUnit(const Filename: string): boolean;
var
  i: Integer;
begin
  for i:=Low(PascalFileExt) to High(PascalFileExt) do
    if CompareFileExt(Filename,PascalFileExt[i],false)=0 then
      exit(true);
  Result:=false;
end;

{------------------------------------------------------------------------------
  function AppendPathDelim(const Path: string): string;
 ------------------------------------------------------------------------------}
function AppendPathDelim(const Path: string): string;
begin
  if (Path<>'') and (Path[length(Path)]<>PathDelim) then
    Result:=Path+PathDelim
  else
    Result:=Path;
end;

{------------------------------------------------------------------------------
  function TrimFilename(const AFilename: string): string;
 ------------------------------------------------------------------------------}
function TrimFilename(const AFilename: string): string;
// trim double path delims, heading and trailing spaces
// and special dirs . and ..

  function FilenameIsTrimmed(const TheFilename: string): boolean;
  var
    l: Integer;
    i: Integer;
  begin
    Result:=false;
    if TheFilename='' then begin
      Result:=true;
      exit;
    end;
    // check heading spaces
    if TheFilename[1]=' ' then exit;
    // check trailing spaces
    l:=length(TheFilename);
    if TheFilename[l]=' ' then exit;
    i:=1;
    while i<=l do begin
      case TheFilename[i] of
      
      PathDelim:
        // check for double path delimiter
        if (i<l) and (TheFilename[i+1]=PathDelim) then exit;
        
      '.':
        if (i=1) or (TheFilename[i-1]=PathDelim) then begin
          // check for . directories
          if ((i<l) and (TheFilename[i+1]=PathDelim)) or ((i=l) and (i>1)) then exit;
          // check for .. directories
          if (i<l) and (TheFilename[i+1]='.')
          and ((i+1=l) or ((i+2<=l) and (TheFilename[i+2]=PathDelim))) then exit;
        end;

      end;
      inc(i);
    end;
    Result:=true;
  end;

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

  // trim double path delims and special dirs . and ..
  while (SrcPos<=l) do begin
    c:=AFilename[SrcPos];
    // check for double path delims
    if (c=PathDelim) then begin
      inc(SrcPos);
      {$IFDEF WINDOWS}
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
          //  1. ..      -> keep
          //  2. /..     -> skip .., keep /
          //  3. C:..    -> keep
          //  4. C:\..   -> skip .., keep C:\
          //  5. \\..    -> skip .., keep \\
          //  6. xxx../..   -> keep
          //  7. xxxdir$Macro/..  -> keep
          //  8. xxxdir/..  -> trim dir and skip ..
          if DestPos=1 then begin
            //  1. ..      -> keep
          end else if (DestPos=2) and (Result[1]=PathDelim) then begin
            //  2. /..     -> skip .., keep /
            inc(SrcPos,2);
            continue;
          {$IFDEF WINDOWS}
          end else if (DestPos=3) and (Result[2]=':')
          and (Result[1] in ['a'..'z','A'..'Z']) then begin
            //  3. C:..    -> keep
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
              //  6. ../..   -> keep
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

{------------------------------------------------------------------------------
  function CompareFileExt(const Filename, Ext: string;
    CaseSensitive: boolean): integer;
    
  Ext can contain a point or not
 ------------------------------------------------------------------------------}
function CompareFileExt(const Filename, Ext: string;
  CaseSensitive: boolean): integer;
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
    Result:=AnsiCompareText(n, e);
  if Result<0 then Result:=1
  else if Result>0 then Result:=1;
end;

function CompareFileExt(const Filename, Ext: string): integer;
begin
  Result:=CompareFileExt(Filename,Ext,false);
end;

{------------------------------------------------------------------------------
  function ChompPathDelim(const Path: string): string;
 ------------------------------------------------------------------------------}
function ChompPathDelim(const Path: string): string;
begin
  if (Path<>'') and (Path[length(Path)]=PathDelim) then
    Result:=LeftStr(Path,length(Path)-1)
  else
    Result:=Path;
end;

{------------------------------------------------------------------------------
  function FileIsText(const AFilename: string): boolean;
 ------------------------------------------------------------------------------}
function FileIsText(const AFilename: string): boolean;
var
  FileReadable: Boolean;
begin
  Result:=FileIsText(AFilename,FileReadable);
  if FileReadable then ;
end;

function FileIsText(const AFilename: string; out FileReadable: boolean): boolean;
const
  BufLen = 1024;
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
      Len:=BufLen;
      SetLength(Buf,BufLen+1);
      Len:=fs.Read(Buf[1],BufLen);
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
  OnlyChilds: boolean): boolean;
var
  FileInfo: TSearchRec;
  CurSrcDir: String;
  CurFilename: String;
begin
  Result:=false;
  CurSrcDir:=CleanAndExpandDirectory(DirectoryName);
  if FindFirstUTF8(CurSrcDir+GetAllFilesMask,faAnyFile,FileInfo)=0 then begin
    repeat
      // check if special file
      if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='') then
        continue;
      CurFilename:=CurSrcDir+FileInfo.Name;
      if (FileInfo.Attr and faDirectory)>0 then begin
        if not DeleteDirectory(CurFilename,false) then exit;
      end else begin
        if not DeleteFileUTF8(CurFilename) then exit;
      end;
    until FindNextUTF8(FileInfo)<>0;
  end;
  FindCloseUTF8(FileInfo);
  if (not OnlyChilds) and (not RemoveDirUTF8(DirectoryName)) then exit;
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
  Result:=ExpandFileNameUTF8(TrimFileName(Filename));
end;

{------------------------------------------------------------------------------
  function CleanAndExpandDirectory(const Filename: string): string;
 ------------------------------------------------------------------------------}
function CleanAndExpandDirectory(const Filename: string): string;
begin
  Result:=AppendPathDelim(CleanAndExpandFilename(Filename));
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

  {$IFDEF MSWindows}
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
    inc(DirCount);
    inc(BaseDirPos);
  end else
    inc(UpDirCount);
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

{------------------------------------------------------------------------------
  function FileIsInPath(const Filename, Path: string): boolean;
 ------------------------------------------------------------------------------}
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

{------------------------------------------------------------------------------
  function FileIsInPath(const Filename, Path: string): boolean;
 ------------------------------------------------------------------------------}
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

{------------------------------------------------------------------------------
  function CopyFile(const SrcFilename, DestFilename: string): boolean;
 ------------------------------------------------------------------------------}
function CopyFile(const SrcFilename, DestFilename: string): boolean;
begin
  Result := CopyFile(SrcFilename, DestFilename, false);
end;

{------------------------------------------------------------------------------
  function CopyFile(const SrcFilename, DestFilename: string PreserveTime:
    boolean): boolean;
 ------------------------------------------------------------------------------}
function CopyFile(const SrcFilename, DestFilename: String; PreserveTime: Boolean): Boolean;
var
  SrcFS: TFileStream;
  DestFS: TFileStream;
begin
  try
    SrcFS := TFileStream.Create(UTF8ToSys(SrcFilename), fmOpenRead or fmShareDenyWrite);
    try
      DestFS := TFileStream.Create(UTF8ToSys(DestFilename), fmCreate);
      try
        DestFS.CopyFrom(SrcFS, SrcFS.Size);
      finally
        DestFS.Free;
      end;
      if PreserveTime then
        FileSetDateUTF8(DestFilename, FileGetDate(SrcFS.Handle));
    finally
      SrcFS.Free;
    end;
    Result := True;
  except
    Result := False;
  end;
end;

{------------------------------------------------------------------------------
  function GetTempFilename(const Directory, Prefix: string): string;
 ------------------------------------------------------------------------------}
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

{------------------------------------------------------------------------------
  function SearchFileInPath(const Filename, BasePath, SearchPath,
    Delimiter: string; Flags: TSearchFileInPathFlags): string;
 ------------------------------------------------------------------------------}
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
  //
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
  FSearching := False;
end;

procedure TFileSearcher.Search(const ASearchPath: String; ASearchMask: String;
  ASearchSubDirs: Boolean; AMaskSeparator: char);
var
  MaskList: TMaskList;

  procedure DoSearch(const APath: String; const ALevel: Integer);
  var
    P: String;
    PathInfo: TSearchRec;
  begin
    P := APath + AllDirectoryEntriesMask;

    if FindFirstUTF8(P, faAnyFile, PathInfo) = 0 then
    try
      begin
        repeat
          // skip special files
          if (PathInfo.Name = '.') or (PathInfo.Name = '..') or
            (PathInfo.Name = '') then Continue;

          if (PathInfo.Attr and faDirectory) = 0 then
          begin
            if (MaskList = nil) or MaskList.Matches(PathInfo.Name) then
            begin
              FPath := APath;
              FLevel := ALevel;
              FFileInfo := PathInfo;
              DoFileFound;
            end;
          end
          else
          begin
            FPath := APath;
            FLevel := ALevel;
            FFileInfo := PathInfo;
            DoDirectoryFound;
          end;

        until (FindNextUTF8(PathInfo) <> 0) or not FSearching;
      end;
    finally
      FindCloseUTF8(PathInfo);
    end;
    
    if ASearchSubDirs or (ALevel > 0) then // search recursively in directories
      if FindFirstUTF8(P, faAnyFile, PathInfo) = 0 then
      try
        begin
          repeat
            if (PathInfo.Name = '.') or (PathInfo.Name = '..') or
              (PathInfo.Name = '') or ((PathInfo.Attr and faDirectory) = 0) then Continue;
              
            FPath := APath;
            FLevel := ALevel;
            FFileInfo := PathInfo;
            DoDirectoryEnter;
            if not FSearching then Break;
            
            DoSearch(AppendPathDelim(APath + PathInfo.Name), Succ(ALevel));
            
          until (FindNextUTF8(PathInfo) <> 0);
        end;
      finally
        FindCloseUTF8(PathInfo);
      end;
  end;

begin
  if FSearching then RaiseSearchingError;

  MaskList := TMaskList.Create(ASearchMask,AMaskSeparator);
  // empty mask = all files mask
  if MaskList.Count = 0 then FreeAndNil(MaskList);

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
  fs: TFileStream;
begin
  Result := '';
  try
    fs := TFileStream.Create(UTF8ToSys(Filename), fmOpenRead or fmShareDenyWrite);
    try
      Setlength(Result, fs.Size);
      if Result <> '' then
        fs.Read(Result[1], Length(Result));
    finally
      fs.Free;
    end;
  except
    Result := '';
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
        if (APath = ADir) then Result := False
          else Result:=DoForceDirectories(APath);
      If Result then
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

