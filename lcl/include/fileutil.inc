{%MainUnit ../filectrl.pp}
{******************************************************************************
                                  Filectrl
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
 
{------------------------------------------------------------------------------
  DirPathExists
 ------------------------------------------------------------------------------}
function DirPathExists(const FileName: String): Boolean;
var
  F: Longint;
  dirExist: Boolean;
begin
  dirExist := false;

  F := FileGetAttr(ChompPathDelim(FileName));
  if F <> -1 then
    if (F and faDirectory) <> 0 then
      dirExist := true;
  Result := dirExist;
end;

{------------------------------------------------------------------------------
  function CompareFilenames(const Filename1, Filename2: string): integer;
 ------------------------------------------------------------------------------}
function CompareFilenames(const Filename1, Filename2: string): integer;
begin
  {$IFDEF CaseInsensitiveFilenames}
  Result:=AnsiCompareText(Filename1, Filename2);
  {$ELSE}
  Result:=CompareStr(Filename1, Filename2);
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
  {$IFNDEF CaseInsensitiveFilenames}
  i: Integer;
  {$ENDIF}
begin
  if (Len1=0) or (Len2=0) then begin
    Result:=Len1-Len2;
    exit;
  end;
  if ResolveLinks then begin
    SetLength(File1,Len1);
    Move(Filename1^,File1[1],Len1);
    SetLength(File2,Len2);
    Move(Filename2^,File2[1],Len2);
    Result:=CompareFilenames(File1,File2,true);
  end else begin
    {$IFDEF CaseInsensitiveFilenames}
    SetLength(File1,Len1);
    Move(Filename1^,File1[1],Len1);
    SetLength(File2,Len2);
    Move(Filename2^,File2[1],Len2);
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

{------------------------------------------------------------------------------
  function FilenameIsAbsolute(const TheFilename: string):boolean;
 ------------------------------------------------------------------------------}
function FilenameIsAbsolute(const TheFilename: string):boolean;
begin
  {$IFDEF WINDOWS}
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
      '.': Result:=copy(Result,1, p-1);
    end;
    dec(p);
  end;
end;

{------------------------------------------------------------------------------
  function CompareFileExt(const Filename, Ext: string;
    CaseSensitive: boolean): integer;
 ------------------------------------------------------------------------------}
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
          FileChar:=UpChars[FileChar];
          ExtChar:=UpChars[ExtChar];
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

function FileIsText(const AFilename: string; out FileReadable: boolean
  ): boolean;
var fs: TFileStream;
  Buf: string;
  Len, i: integer;
  NewLine: boolean;
begin
  Result:=false;
  FileReadable:=true;
  try
    fs:=TFileStream.Create(AFilename,fmOpenRead);
    try
      // read the first 1024 bytes
      Len:=1024;
      if Len>fs.Size then Len:=integer(fs.Size);
      if Len>0 then begin
        SetLength(Buf,Len);
        fs.Read(Buf[1],length(Buf));
        NewLine:=false;
        for i:=1 to length(Buf) do begin
          case Buf[i] of
          #0..#8,#11..#12,#14..#31: exit;
          #10,#13: NewLine:=true;
          end;
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

{------------------------------------------------------------------------------
  procedure CheckIfFileIsSymlink(const AFilename: string);
 ------------------------------------------------------------------------------}
procedure CheckIfFileIsSymlink(const AFilename: string);
{$IFNDEF WINDOWS}
var
  AText: string;
{$ENDIF}
begin
  // to get good error messages consider the OS
  if not FileExists(AFilename) then begin
    raise Exception.Create('file "'+AFilename+'" does not exist');
  end;
  {$IFDEF WINDOWS}
  raise Exception.Create('"'+AFilename+'" is not symlink');
  {$ELSE}
  if FpReadLink(AFilename)='' then begin
    AText:='"'+AFilename+'"';
    case fpGetErrno() of
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
      AText:=AText+' is not a symbolic link';
    end;
    raise Exception.Create(AText);
  end;
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  function FileIsSymlink(const AFilename: string): boolean;
 ------------------------------------------------------------------------------}
function FileIsSymlink(const AFilename: string): boolean;
begin
  {$IFDEF WINDOWS}
  Result:=false;
  {$ELSE}
  Result:=(FpReadLink(AFilename)<>'');
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  function FileIsReadable(const AFilename: string): boolean;
 ------------------------------------------------------------------------------}
function FileIsReadable(const AFilename: string): boolean;
begin
  {$IFDEF WINDOWS}
  Result:=true;
  {$ELSE}
  Result:= BaseUnix.FpAccess(AFilename,BaseUnix.R_OK)=0;
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  FileIsWritable
 ------------------------------------------------------------------------------}
function FileIsWritable(const AFilename: string): boolean;
begin
  {$IFDEF WINDOWS}
  Result:=((FileGetAttr(AFilename) and faReadOnly)=0);
  {$ELSE}
  Result:= BaseUnix.FpAccess(AFilename,BaseUnix.W_OK)=0;
  {$ENDIF}
end;

function FileSize(const Filename: string): int64;
var
  FileInfo: TSearchRec;
begin
  if SysUtils.FindFirst(Filename,faAnyFile,FileInfo)=0 then begin
    Result:=FileInfo.Size;
  end else
    Result:=-1;
  SysUtils.FindClose(FileInfo);
end;

{------------------------------------------------------------------------------
  GetFileDescription
 ------------------------------------------------------------------------------}
function GetFileDescription(const AFilename: string): string;
{$IFDEF WINDOWS}
{$ELSE}
var
  info: Stat;
  // permissions
  // user
  // group
  // size
  // date
  // time
  mode: mode_t;
{$ENDIF}
begin
  Result:='';
  {$IFDEF WINDOWS}

  {$ELSE}
  if not (FpStat(AFilename,info)=0) then exit;
  
  // permissions
  // file type
  mode:= info.st_mode;
  if STAT_IFLNK and mode=STAT_IFLNK then
    Result:=Result+'l'
  else
  if STAT_IFDIR and mode=STAT_IFDIR then
    Result:=Result+'d'
  else
  if STAT_IFBLK and mode=STAT_IFBLK then
    Result:=Result+'b'
  else
  if STAT_IFCHR and mode=STAT_IFCHR then
    Result:=Result+'c'
  else
    Result:=Result+'-';
  // user permissions
  if STAT_IRUSR and mode=STAT_IRUsr then
    Result:=Result+'r'
  else
    Result:=Result+'-';
  if STAT_IWUsr and mode=STAT_IWUsr then
    Result:=Result+'w'
  else
    Result:=Result+'-';
  if STAT_IXUsr and mode=STAT_IXUsr then
    Result:=Result+'x'
  else
    Result:=Result+'-';
  // group permissions
  if STAT_IRGRP and mode=STAT_IRGRP then
    Result:=Result+'r'
  else
    Result:=Result+'-';
  if STAT_IWGRP and mode=STAT_IWGRP then
    Result:=Result+'w'
  else
    Result:=Result+'-';
  if STAT_IXGRP and mode=STAT_IXGRP then
    Result:=Result+'x'
  else
    Result:=Result+'-';
  // other permissions
  if STAT_IROTH and mode=STAT_IROTH then
    Result:=Result+'r'
  else
    Result:=Result+'-';
  if STAT_IWOTH and mode=STAT_IWOTH then
    Result:=Result+'w'
  else
    Result:=Result+'-';
  if STAT_IXOTH and mode=STAT_IXOTH then
    Result:=Result+'x'
  else
    Result:=Result+'-';


  // user name
  //Result:=Result+' Owner: '+IntToStr(info.uid)+'.'+IntToStr(info.gid);

  // size
  Result:=Result+rsSize+IntToStr(info.st_size);
  
  {$ENDIF}
  // date + time
  Result:=Result+rsModified;
  try
    Result:=Result+FormatDateTime('DD/MM/YYYY hh:mm',
                           FileDateToDateTime(FileAge(AFilename)));
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
{$IFNDEF WINDOWS}
var
  LinkFilename: string;
  AText: string;
{$ENDIF}
begin
  Result:=Filename;
  {$IFDEF WINDOWS}

  {$ELSE}
  repeat
    LinkFilename:=FpReadLink(Result);
    if LinkFilename='' then begin
      AText:='"'+Filename+'"';
      case fpGetErrno() of
      ESysEAcces:
        AText:='read access denied for '+AText;
      ESysENoEnt:
        AText:='a directory component in '+AText
                            +' does not exist or is a dangling symlink';
      ESysENotDir:
        AText:='a directory component in '+AText+' is not a directory';
      ESysENoMem:
        AText:='insufficient memory';
      ESysELoop:
        AText:=AText+' has a circular symbolic link';
      else
        // not a symbolic link, just a regular file
        exit;
      end;
      if (not ExceptionOnError) then begin
        Result:='';
        exit;
      end;
      raise EFOpenError.Create(AText);
    end else begin
      if not FilenameIsAbsolute(LinkFilename) then
        Result:=ExpandFilename(ExtractFilePath(Result)+LinkFilename)
      else
        Result:=LinkFilename;
    end;
  until false;
  {$ENDIF}
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
  function FileIsExecutable(const AFilename: string): boolean;
 ------------------------------------------------------------------------------}
function FileIsExecutable(const AFilename: string): boolean;
{$IFNDEF WINDOWS}
var
  Info : Stat;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  Result:=FileExists(AFilename);
  {$ELSE}
  // first check AFilename is not a directory and then check if executable
  Result:= (FpStat(AFilename,info)<>-1) and FPS_ISREG(info.st_mode) and
           (BaseUnix.FpAccess(AFilename,BaseUnix.X_OK)=0);
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  procedure CheckIfFileIsExecutable(const AFilename: string);
 ------------------------------------------------------------------------------}
procedure CheckIfFileIsExecutable(const AFilename: string);
{$IFNDEF WINDOWS}
var AText: string;
{$ENDIF}
begin
  // TProcess does not report, if a program can not be executed
  // to get good error messages consider the OS
  if not FileExists(AFilename) then begin
    raise Exception.Create('file "'+AFilename+'" does not exist');
  end;
  if DirPathExists(AFilename) then begin
    raise Exception.Create('file "'+AFilename+'" is a directory and not an executable');
  end;
  {$IFNDEF WINDOWS}
  if not FileIsExecutable(AFilename) then
  begin
    AText:='"'+AFilename+'"';
    case fpGetErrno() of
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
      AText:=AText+' is not executable';
    end;
    raise Exception.Create(AText);
  end;
  {$ENDIF}
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
        Result:=CreateDir(Dir);
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
  if SysUtils.FindFirst(CurSrcDir+GetAllFilesMask,faAnyFile,FileInfo)=0 then begin
    repeat
      // check if special file
      if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='') then
        continue;
      CurFilename:=CurSrcDir+FileInfo.Name;
      if (FileInfo.Attr and faDirectory)>0 then begin
        if not DeleteDirectory(CurFilename,false) then exit;
      end else begin
        if not DeleteFile(CurFilename) then exit;
      end;
    until SysUtils.FindNext(FileInfo)<>0;
  end;
  SysUtils.FindClose(FileInfo);
  if (not OnlyChilds) and (not RemoveDir(DirectoryName)) then exit;
  Result:=true;
end;

{------------------------------------------------------------------------------
  function ProgramDirectory: string;
 ------------------------------------------------------------------------------}
function ProgramDirectory: string;
var
  Flags: TSearchFileInPathFlags;
begin
  Result:=ParamStr(0);
  if ExtractFilePath(Result)='' then begin
    // program was started via PATH
    {$IFDEF WINDOWS}
    Flags:=[];
    {$ELSE}
    Flags:=[sffDontSearchInBasePath];
    {$ENDIF}
    Result:=SearchFileInPath(Result,'',GetEnvironmentVariable('PATH'),':',Flags);
  end;
  // resolve links
  Result:=ReadAllLinks(Result,false);
  // extract file path and expand to full name
  Result:=ExpandFilename(ExtractFilePath(Result));
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
    fs:=TFileStream.Create(TempFilename,fmCreate);
    s:='WriteTest';
    fs.Write(s[1],length(s));
    fs.Free;
    DeleteFile(TempFilename);
    Result:=true;
  except
  end;
end;

{------------------------------------------------------------------------------
  function CleanAndExpandFilename(const Filename: string): string;
 ------------------------------------------------------------------------------}
function CleanAndExpandFilename(const Filename: string): string;
begin
  Result:=ExpandFilename(TrimFileName(Filename));
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

function CreateRelativePath(const Filename, BaseDirectory: string): string;
var
  FileNameLength: Integer;
  BaseDirLen: Integer;
  MinLen: Integer;
  SamePos: Integer;
  UpDirCount: Integer;
  BaseDirPos: Integer;
  ResultPos: Integer;
  i: Integer;
  FileNameRestLen: Integer;
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

  FileNameLength:=length(Filename);
  BaseDirLen:=length(BaseDirectory);

  // skip matching directories
  MinLen:=FileNameLength;
  if MinLen>BaseDirLen then MinLen:=BaseDirLen;
  SamePos:=1;
  while (SamePos<=MinLen) do begin
    {$IFDEF CaseInsensitiveFilenames}
    if AnsiStrLIComp(@FileName[SamePos],@BaseDirectory[SamePos],1)=0
    {$ELSE}
    if FileName[SamePos]=BaseDirectory[SamePos]
    {$ENDIF}
    then
      inc(SamePos)
    else
      break;
  end;
  if (SamePos>MinLen)
  and (((SamePos<=BaseDirLen) and (BaseDirectory[SamePos]=PathDelim))
    or ((SamePos<=FileNameLength) and (Filename[SamePos]=PathDelim))
    or (BaseDirLen=FileNameLength))
  then begin
    // Filename lies in BaseDirectory
    // or Filename is parent directory of BaseDirectory
    // or Filename is BaseDirectory
  end else begin
    // difference found -> step back to path delimiter
    repeat
      dec(SamePos);
      if (SamePos<1) then exit;
    until (FileName[SamePos]=PathDelim);
  end;
  if (SamePos=1) and (Filename[1]=PathDelim) then exit;

  // calculate needed up directories
  UpDirCount:=0;
  BaseDirPos:=SamePos+1;
  while (BaseDirPos<=BaseDirLen) do begin
    if (BaseDirectory[BaseDirPos]=PathDelim) then
      inc(UpDirCount);
    inc(BaseDirPos);
  end;
  if (SamePos<BaseDirLen) and (BaseDirectory[BaseDirLen]<>PathDelim) then
    inc(UpDirCount);

  // create relative filename
  FileNameRestLen:=FileNameLength-SamePos;
  SetLength(Result,3*UpDirCount+FileNameRestLen);
  ResultPos:=1;
  for i:=1 to UpDirCount do begin
    Result[ResultPos]:='.';
    Result[ResultPos+1]:='.';
    Result[ResultPos+2]:=PathDelim;
    inc(ResultPos,3);
  end;
  if FileNameRestLen>0 then
    Move(Filename[SamePos+1],Result[ResultPos],FileNameRestLen);
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
function CopyFile(const SrcFilename, DestFilename: string; PreserveTime: boolean
  ): boolean;
var
  SrcFS: TFileStream;
  DestFS: TFileStream;
begin
  try
    SrcFS:=TFileStream.Create(SrcFilename,fmOpenRead);
    try
      DestFS:=TFileStream.Create(DestFilename,fmCreate);
      try
        DestFS.CopyFrom(SrcFS,SrcFS.Size);
        if PreserveTime then
          FileSetDate(DestFS.Handle, FileGetDate(SrcFS.Handle));
      finally
        DestFS.Free;
      end;
    finally
      SrcFS.Free;
    end;
    Result:=true;
  except
    Result:=false;
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
  CurPath:=AppendPathDelim(ExpandFilename(Directory))+Prefix;
  i:=1;
  repeat
    Result:=CurPath+IntToStr(i)+'.tmp';
    if not FileExists(Result) then exit;
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
    if FileExists(Filename) then begin
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
  and FileExists(Base+Filename) then begin
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
      if FileExists(Result) then exit;
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
    if not FileExists(NewFilename) then exit;
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
  if not FileExists(Filename) then exit;
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
      if SysUtils.FindFirst(CurDir+GetAllFilesMask,faAnyFile,FileInfo)=0 then
      begin
        repeat
          // check if special file
          if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='')
          then
            continue;
          if CompareText(FileInfo.Name,CurFile)=0 then begin
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
                break;
              end;
            end;
          end;
        until SysUtils.FindNext(FileInfo)<>0;
      end;
      SysUtils.FindClose(FileInfo);
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
  if SysUtils.FindFirst(CurDir+GetAllFilesMask,faAnyFile, FileInfo)=0 then begin
    ShortFilename:=ExtractFilename(Filename);
    repeat
      // check if special file
      if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='')
      then
        continue;
      if CompareText(FileInfo.Name,ShortFilename)=0 then begin
        if FileInfo.Name=ShortFilename then begin
          // fits exactly
          Result:=Filename;
          break;
        end;
        // fits case insensitive
        Result:=CurDir+FileInfo.Name;
        // search further
      end;
    until SysUtils.FindNext(FileInfo)<>0;
  end;
  SysUtils.FindClose(FileInfo);
end;

function FindDefaultExecutablePath(const Executable: string): string;
begin
  if FilenameIsAbsolute(Executable) then begin
    Result:=Executable;
    if FileExists(Result) then exit;
    {$IFDEF Windows}
    if ExtractFileExt(Result)='' then begin
      Result:=Result+'.exe';
      if FileExists(Result) then exit;
    end;
    {$ENDIF}
  end else begin
    Result:=SearchFileInPath(Executable,'',
                             SysUtils.GetEnvironmentVariable('PATH'), PathSeparator,
                             [sffDontSearchInBasePath]);
    if Result<>'' then exit;
    {$IFDEF Windows}
    if ExtractFileExt(Executable)='' then begin
      Result:=SearchFileInPath(Executable+'.exe','',
                               SysUtils.GetEnvironmentVariable('PATH'), PathSeparator,
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

    if SysUtils.FindFirst(P, faAnyFile, PathInfo) = 0 then
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

        until (SysUtils.FindNext(PathInfo) <> 0) or not FSearching;
      end;
    finally
      SysUtils.FindClose(PathInfo);
    end;
    
    if ASearchSubDirs or (ALevel > 0) then // search recursively in directories
      if SysUtils.FindFirst(P, faAnyFile, PathInfo) = 0 then
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
            
          until (SysUtils.FindNext(PathInfo) <> 0);
        end;
      finally
        SysUtils.FindClose(PathInfo);
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
function ReadFileToString(const Filename: string): string;
var
  fs: TFileStream;
begin
  Result:='';
  try
    fs:=TFileStream.Create(Filename,fmOpenRead);
    try
      Setlength(Result,fs.Size);
      if Result<>'' then
        fs.Read(Result[1],length(Result));
    finally
      fs.Free;
    end;
  except
    Result:='';
  end;
end;


