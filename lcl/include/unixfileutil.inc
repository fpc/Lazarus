{%MainUnit ../filectrl.pp}
{------------------------------------------------------------------------------
  procedure CheckIfFileIsExecutable(const AFilename: string);
 ------------------------------------------------------------------------------}
procedure CheckIfFileIsExecutable(const AFilename: string);
var AText: string;
begin
  // TProcess does not report, if a program can not be executed
  // to get good error messages consider the OS
  if not FileExistsUTF8(AFilename) then begin
    raise Exception.Create('file "'+AFilename+'" does not exist');
  end;
  if DirPathExists(AFilename) then begin
    raise Exception.Create('file "'+AFilename+'" is a directory and not an executable');
  end;
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
end;

{------------------------------------------------------------------------------
  procedure CheckIfFileIsSymlink(const AFilename: string);
 ------------------------------------------------------------------------------}
procedure CheckIfFileIsSymlink(const AFilename: string);
var
  AText: string;
begin
  // to get good error messages consider the OS
  if not FileExistsUTF8(AFilename) then begin
    raise Exception.Create('file "'+AFilename+'" does not exist');
  end;
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
end;

{------------------------------------------------------------------------------
  function FileIsReadable(const AFilename: string): boolean;
 ------------------------------------------------------------------------------}
function FileIsReadable(const AFilename: string): boolean;
begin
  Result:= BaseUnix.FpAccess(AFilename,BaseUnix.R_OK)=0;
end;

{------------------------------------------------------------------------------
  FileIsWritable
 ------------------------------------------------------------------------------}
function FileIsWritable(const AFilename: string): boolean;
begin
  Result:= BaseUnix.FpAccess(AFilename,BaseUnix.W_OK)=0;
end;

{------------------------------------------------------------------------------
  function FileIsExecutable(const AFilename: string): boolean;
 ------------------------------------------------------------------------------}
function FileIsExecutable(const AFilename: string): boolean;
var
  Info : Stat;
begin
  // first check AFilename is not a directory and then check if executable
  Result:= (FpStat(AFilename,info)<>-1) and FPS_ISREG(info.st_mode) and
           (BaseUnix.FpAccess(AFilename,BaseUnix.X_OK)=0);
end;

{------------------------------------------------------------------------------
  function FileIsSymlink(const AFilename: string): boolean;
 ------------------------------------------------------------------------------}
function FileIsSymlink(const AFilename: string): boolean;
begin
  Result := FpReadLink(AFilename) <> '';
end;


{------------------------------------------------------------------------------
  FileSize
 ------------------------------------------------------------------------------}
function FileSize(const Filename: string): int64;
var
  st: baseunix.stat;
begin
  if not fpstat(pointer(Filename),st)>=0 then
    exit(-1);
  Result:=st.st_size;
end;

{------------------------------------------------------------------------------
  GetFileDescription
 ------------------------------------------------------------------------------}
function GetFileDescription(const AFilename: string): string;
var
  info: Stat;
  // permissions
  // user
  // group
  // size
  // date
  // time
  mode: mode_t;
begin
  Result:='';
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

  // date + time
  Result:=Result+rsModified;
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
var
  LinkFilename: string;
  AText: string;
begin
  Result:=Filename;
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
        Result:=ExpandFileNameUTF8(ExtractFilePath(Result)+LinkFilename)
      else
        Result:=LinkFilename;
    end;
  until false;
end;

{------------------------------------------------------------------------------
  function FilenameIsAbsolute(const TheFilename: string):boolean;
 ------------------------------------------------------------------------------}
function FilenameIsAbsolute(const TheFilename: string):boolean;
begin
  Result:=FilenameIsUnixAbsolute(TheFilename);
end;

function NeedRTLAnsi: boolean;
var
  Lang: String;
  i: LongInt;
  Encoding: String;
begin
  if FNeedRTLAnsiValid then
    exit(FNeedRTLAnsi);
  FNeedRTLAnsi:=false;
  Lang := SysUtils.GetEnvironmentVariable('LC_ALL');
  if Length(lang) = 0 then
  begin
    Lang := SysUtils.GetEnvironmentVariable('LC_MESSAGES');
    if Length(Lang) = 0 then
    begin
      Lang := SysUtils.GetEnvironmentVariable('LANG');
    end;
  end;
  i:=System.Pos('.',Lang);
  if (i>0) then begin
    Encoding:=copy(Lang,i+1,length(Lang)-i);
    FNeedRTLAnsi:=(SysUtils.CompareText(Encoding,'UTF-8')<>0)
              and (SysUtils.CompareText(Encoding,'UTF8')<>0);
  end;
  FNeedRTLAnsiValid:=true;
  Result:=FNeedRTLAnsi;
end;

function ConsoleToUTF8(const s: string): string;// converts UTF8 string to console encoding (used by Write, WriteLn)
begin
  Result := SysToUTF8(S);
end;

function UTF8ToConsole(const s: string): string;
begin
  Result := UTF8ToSys(s);
end;


function FileExistsUTF8(const Filename: string): boolean;
begin
  Result:=SysUtils.FileExists(UTF8ToSys(Filename));
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

function FileGetAttrUTF8(const FileName: String): Longint;
begin
  Result:=SysUtils.FileGetAttr(UTF8ToSys(Filename));
end;

function FileSetAttrUTF8(const Filename: String; Attr: longint): Longint;
begin
  Result:=SysUtils.FileSetAttr(UTF8ToSys(Filename),Attr);
end;

function DeleteFileUTF8(const FileName: String): Boolean;
begin
  Result:=SysUtils.DeleteFile(UTF8ToSys(Filename));
end;

function RenameFileUTF8(const OldName, NewName: String): Boolean;
begin
  Result:=SysUtils.RenameFile(UTF8ToSys(OldName),UTF8ToSys(NewName));
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

function DirectoryExistsUTF8(const Directory: string): Boolean;
begin
  Result:=SysUtils.DirectoryExists(UTF8ToSys(Directory));
end;

procedure InitFileUtils;
begin
end;

