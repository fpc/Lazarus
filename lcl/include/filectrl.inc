{******************************************************************************
                                  Filectrl
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
 
{------------------------------------------------------------------------------
  DirectoryExists
 ------------------------------------------------------------------------------}
function DirectoryExists(const FileName: String): Boolean;
var
  F: Longint;
  dirExist: Boolean;
begin
  dirExist := false;

  F := FileGetAttr(FileName);
  if F <> -1 then
    if (F and faDirectory) <> 0 then
      dirExist := true;
  Result := dirExist;
end;

{------------------------------------------------------------------------------
  function FilenameIsAbsolute(TheFilename: string):boolean;
 ------------------------------------------------------------------------------}
function FilenameIsAbsolute(TheFilename: string):boolean;
begin
  DoDirSeparators(TheFilename);
  {$IFDEF win32}
  // windows
  Result:=(copy(TheFilename,1,2)='\\') or ((length(TheFilename)>3) and
     (upcase(TheFilename[1]) in ['A'..'Z']) and (copy(TheFilename,2,2)=':\'));
  {$ELSE}
  Result:=(TheFilename<>'') and (TheFilename[1]='/');
  {$ENDIF}
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
var SrcPos, DestPos, l, DirStart: integer;
  c: char;
begin
  Result:=AFilename;
  l:=length(AFilename);
  SrcPos:=1;
  DestPos:=1;

  // skip trailing spaces
  while (l>=1) and (AFilename[SrcPos]=' ') do dec(l);

  // skip heading spaces
  while (SrcPos<=l) and (AFilename[SrcPos]=' ') do inc(SrcPos);

  // trim double path delims and special dirs . and ..
  while (SrcPos<=l) do begin
    c:=AFilename[SrcPos];
    // check for double path delims
    if (c=PathDelim) then begin
      inc(SrcPos);
      {$IFDEF win32}
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
        if (AFilename[SrcPos+1]=PathDelim) then begin
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
          if DestPos=1 then begin
            //  1. ..      -> copy
          end else if (DestPos=2) and (Result[1]=PathDelim) then begin
            //  2. /..     -> skip .., keep /
            inc(SrcPos,2);
            continue;
          {$IFDEF win32}
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
              DestPos:=DirStart;
              inc(SrcPos,2);
              continue;
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
var fs: TFileStream;
  Buf: string;
  Len, i: integer;
  NewLine: boolean;
begin
  Result:=false;
  try
    fs:=TFileStream.Create(AFilename,fmOpenRead);
    try
      // read the first 1024 bytes
      Len:=1024;
      if Len>fs.Size then Len:=fs.Size;
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
  end;
end;

{------------------------------------------------------------------------------
  procedure CheckIfFileIsSymlink(const AFilename: string);
 ------------------------------------------------------------------------------}

procedure CheckIfFileIsSymlink(const AFilename: string);
{$IFNDEF win32}
var
  AText: string;
{$ENDIF}
begin
  // to get good error messages consider the OS
  if not FileExists(AFilename) then begin
    raise Exception.Create('file "'+AFilename+'" does not exist');
  end;
  {$IFNDEF win32}
  if {$IFDEF Ver1_0}Linux{$ELSE}Unix{$ENDIF}.ReadLink(AFilename)='' then begin
    AText:='"'+AFilename+'"';
    case LinuxError of
    sys_eacces: AText:='read access denied for '+AText;
    sys_enoent: AText:='a directory component in '+AText
                          +' does not exist or is a dangling symlink';
    sys_enotdir: AText:='a directory component in '+Atext+' is not a directory';
    sys_enomem: AText:='insufficient memory';
    sys_eloop: AText:=AText+' has a circular symbolic link';
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
  try
    CheckIfFileIsSymlink(AFilename);
    Result:=true;
  except
    Result:=false;
  end;
end;

{------------------------------------------------------------------------------
  function FileIsReadable(const AFilename: string): boolean;
 ------------------------------------------------------------------------------}
function FileIsReadable(const AFilename: string): boolean;
begin
  {$IFDEF win32}
  Result:=true;
  {$ELSE}
  Result:={$IFDEF Ver1_0}Linux{$ELSE}Unix{$ENDIF}.Access(
    AFilename,{$IFDEF Ver1_0}Linux{$ELSE}Unix{$ENDIF}.R_OK);
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  FileIsWritable
 ------------------------------------------------------------------------------}
function FileIsWritable(const AFilename: string): boolean;
begin
  {$IFDEF win32}
  Result:=((FileGetAttr(AFilename) and faReadOnly)>0);
  {$ELSE}
  Result:={$IFDEF Ver1_0}Linux{$ELSE}Unix{$ENDIF}.Access(
    AFilename,{$IFDEF Ver1_0}Linux{$ELSE}Unix{$ENDIF}.W_OK);
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  GetFileDescription
 ------------------------------------------------------------------------------}
function GetFileDescription(const AFilename: string): string;
{$IFDEF win32}

{$ELSE}
var
  info: stat;
  // permissions
  // user
  // group
  // size
  // date
  // time
{$ENDIF}
begin
  Result:='';
  {$IFDEF win32}

  {$ELSE}
  if not FStat(AFilename,info) then exit;
  
  // permissions
  // file type
  if STAT_IFLNK and info.mode=STAT_IFLNK then
    Result:=Result+'l'
  else
  if STAT_IFDIR and info.mode=STAT_IFDIR then
    Result:=Result+'d'
  else
  if STAT_IFBLK and info.mode=STAT_IFBLK then
    Result:=Result+'b'
  else
  if STAT_IFCHR and info.mode=STAT_IFCHR then
    Result:=Result+'c'
  else
    Result:=Result+'-';
  // other permissions
  if STAT_IROTH and info.mode=STAT_IROTH then
    Result:=Result+'r'
  else
    Result:=Result+'-';
  if STAT_IWOTH and info.mode=STAT_IWOTH then
    Result:=Result+'w'
  else
    Result:=Result+'-';
  if STAT_IXOTH and info.mode=STAT_IXOTH then
    Result:=Result+'x'
  else
    Result:=Result+'-';
  // group permissions
  if STAT_IRGRP and info.mode=STAT_IRGRP then
    Result:=Result+'r'
  else
    Result:=Result+'-';
  if STAT_IWGRP and info.mode=STAT_IWGRP then
    Result:=Result+'w'
  else
    Result:=Result+'-';
  if STAT_IXGRP and info.mode=STAT_IXGRP then
    Result:=Result+'x'
  else
    Result:=Result+'-';
  // user permissions
  if STAT_IRUSR and info.mode=STAT_IRUsr then
    Result:=Result+'r'
  else
    Result:=Result+'-';
  if STAT_IWUsr and info.mode=STAT_IWUsr then
    Result:=Result+'w'
  else
    Result:=Result+'-';
  if STAT_IXUsr and info.mode=STAT_IXUsr then
    Result:=Result+'x'
  else
    Result:=Result+'-';
    

  // user name
  //Result:=Result+' Owner: '+IntToStr(info.uid)+'.'+IntToStr(info.gid);

  // size
  Result:=Result+'  size '+IntToStr(info.size);
  
  {$ENDIF}
  // date + time
  Result:=Result+'  modified '+FormatDateTime('DD/MM/YYYY hh:mm',
                         FileDateToDateTime(FileAge(AFilename)));
end;

{------------------------------------------------------------------------------
  function ExtractFileNameOnly(const AFilename: string): string;
 ------------------------------------------------------------------------------}
function ExtractFileNameOnly(const AFilename: string): string;
var ExtLen: integer;
begin
  Result:=ExtractFilename(AFilename);
  ExtLen:=length(ExtractFileExt(Result));
  Result:=copy(Result,1,length(Result)-ExtLen);
end;

{------------------------------------------------------------------------------
  function FileIsExecutable(const AFilename: string): boolean;
 ------------------------------------------------------------------------------}
function FileIsExecutable(const AFilename: string): boolean;
begin
  try
    CheckIfFileIsExecutable(AFilename);
    Result:=true;
  except
    Result:=false;
  end;
end;

{------------------------------------------------------------------------------
  procedure CheckIfFileIsExecutable(const AFilename: string);
 ------------------------------------------------------------------------------}
procedure CheckIfFileIsExecutable(const AFilename: string);
{$IFNDEF win32}
var AText: string;
{$ENDIF}
begin
  // TProcess does not report, if a program can not be executed
  // to get good error messages consider the OS
  if not FileExists(AFilename) then begin
    raise Exception.Create('file "'+AFilename+'" does not exist');
  end;
  {$IFNDEF win32}
  if not{$IFDEF Ver1_0}Linux{$ELSE}Unix{$ENDIF}.Access(
    AFilename,{$IFDEF Ver1_0}Linux{$ELSE}Unix{$ENDIF}.X_OK) then
  begin
    AText:='"'+AFilename+'"';
    case LinuxError of
    sys_eacces: AText:='execute access denied for '+AText;
    sys_enoent: AText:='a directory component in '+AText
                          +' does not exist or is a dangling symlink';
    sys_enotdir: AText:='a directory component in '+Atext+' is not a directory';
    sys_enomem: AText:='insufficient memory';
    sys_eloop: AText:=AText+' has a circular symbolic link';
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
  i:=1;
  while i<=length(DirectoryName) do begin
    if DirectoryName[i]=PathDelim then begin
      Dir:=copy(DirectoryName,1,i-1);
      if not DirectoryExists(Dir) then begin
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
const
  {$IFDEF Win32}
  FindMask = '*.*';
  {$ELSE}
  FindMask = '*';
  {$ENDIF}
var
  FileInfo: TSearchRec;
  CurSrcDir: String;
  CurFilename: String;
begin
  Result:=false;
  CurSrcDir:=CleanAndExpandDirectory(DirectoryName);
  if SysUtils.FindFirst(CurSrcDir+FindMask,faAnyFile,FileInfo)=0 then begin
    repeat
      // check if special file
      if (FileInfo.Name='.') or (FileInfo.Name='..') then continue;
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

{------------------------------------------------------------------------------
  function CopyFile(const SrcFilename, DestFilename: string): boolean;
 ------------------------------------------------------------------------------}
function CopyFile(const SrcFilename, DestFilename: string): boolean;
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
  function SearchFileInPath(const Filename, BasePath, SearchPath,
    Delimiter: string; SearchLoUpCase: boolean): string;
 ------------------------------------------------------------------------------}
function SearchFileInPath(const Filename, BasePath, SearchPath,
  Delimiter: string; SearchLoUpCase: boolean): string;

  function FileDoesExists(const AFilename: string): boolean;
  var s: string;
  begin
    s:=ExpandFilename(TrimFilename(AFilename));
    Result:=FileExists(s);
    if Result then begin
      SearchFileInPath:=s;
      exit;
    end;
    {$IFNDEF Win32}
    if SearchLoUpCase then begin

    end;
    {$ENDIF}
  end;

var
  p, StartPos, l: integer;
  CurPath, Base: string;
begin
//writeln('[SearchFileInPath] Filename="',Filename,'" BasePath="',BasePath,'" SearchPath="',SearchPath,'" Delimiter="',Delimiter,'"');
  if (Filename='') then begin
    Result:=Filename;
    exit;
  end;
  // check if filename absolute
  if FilenameIsAbsolute(Filename) then begin
    if FileExists(Filename) then begin
      Result:=ExpandFilename(Filename);
      exit;
    end else begin
      Result:='';
      exit;
    end;
  end;
  Base:=ExpandFilename(AppendPathDelim(BasePath));
  // search in current directory
  if FileExists(Base+Filename) then begin
    Result:=Base+Filename;
    exit;
  end;
  // search in search path
  StartPos:=1;
  l:=length(SearchPath);
  while StartPos<=l do begin
    p:=StartPos;
    while (p<=l) and (pos(SearchPath[p],Delimiter)<1) do inc(p);
    CurPath:=Trim(copy(SearchPath,StartPos,p-StartPos));
    if CurPath<>'' then begin
      if not FilenameIsAbsolute(CurPath) then
        CurPath:=Base+CurPath;
      Result:=ExpandFilename(AppendPathDelim(CurPath)+Filename);
      if FileExists(Result) then exit;
    end;
    StartPos:=p+1;
  end;
  Result:='';
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
        fs.Read(Result,length(Result));
    finally
      fs.Free;
    end;
  except
    Result:='';
  end;
end;


{
  $Log$
  Revision 1.8  2002/12/23 13:20:46  mattias
  fixed backuping symlinks

  Revision 1.7  2002/12/23 10:12:40  mattias
  added symlink test and unit types

  Revision 1.6  2002/12/17 19:49:34  mattias
  finished publish project

  Revision 1.5  2002/12/12 17:47:46  mattias
  new constants for compatibility

  Revision 1.4  2002/12/09 16:48:36  mattias
  added basic file handling functions to filectrl

  Revision 1.3  2002/05/29 21:44:38  lazarus
  MG: improved TCommon/File/OpenDialog, fixed TListView scrolling and broder

  Revision 1.2  2002/05/10 06:05:52  lazarus
  MG: changed license to LGPL

  Revision 1.1  2000/07/13 10:28:25  michael
  + Initial import

  Revision 1.1  2000/04/24 05:02:43  lazarus
  Added filectrl unit for DirectoryExists function.     CAW


}

