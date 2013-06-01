{%MainUnit fileutil.pas}


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
  if not (FpStat(AFilename,info{%H-})=0) then exit;

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
  Result:=Result+lrsSize+IntToStr(info.st_size);

  // date + time
  Result:=Result+lrsModified;
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
  Depth: Integer;
begin
  Result:=Filename;
  Depth:=0;
  while Depth<12 do begin
    inc(Depth);
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
  end;
  // probably an endless loop
  if ExceptionOnError then
    raise EFOpenError.Create('too many links, maybe an endless loop.')
  else
    Result:='';
end;


function ConsoleToUTF8(const s: string): string;// converts UTF8 string to console encoding (used by Write, WriteLn)
begin
  Result := SysToUTF8(S);
end;

function UTF8ToConsole(const s: string): string;
begin
  Result := UTF8ToSys(s);
end;


function ExtractShortPathNameUTF8(const FileName: String): String;
begin
  Result:=SysToUTF8(SysUtils.ExtractShortPathName(UTF8ToSys(FileName)));
end;

function ParamStrUTF8(Param: Integer): string;
begin
  Result:=SysToUTF8(ObjPas.ParamStr(Param));
end;



procedure InitFileUtil;
begin
  //dummy procedure
end;
