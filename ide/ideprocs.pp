{
  Simple functions
   - for file access, not yet in fpc.
   - recent list
   - xmlconfig formats
}
unit IDEProcs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, XMLCfg;

//
const
  // ToDo: find the constant in the fpc units.
  EndOfLine:shortstring={$IFDEF win32}#13+{$ENDIF}#10;

// files
function FilenameIsAbsolute(Filename: string):boolean;
function DirectoryExists(DirectoryName: string): boolean;
function ForceDirectory(DirectoryName: string): boolean;
function ExtractFileNameOnly(const AFilename: string): string;
procedure CheckIfFileIsExecutable(const AFilename: string);
function FileIsExecutable(const AFilename: string): boolean;
function FileIsReadable(const AFilename: string): boolean;
function FileIsWritable(const AFilename: string): boolean;
function FileIsText(const AFilename: string): boolean;
function CompareFilenames(const Filename1, Filename2: string): integer;
function AppendPathDelim(const Path: string): string;
function TrimFilename(const AFilename: string): string;
function SearchFileInPath(const Filename, BasePath, SearchPath,
                          Delimiter: string): string;
procedure SplitCmdLine(const CmdLine: string;
                       var ProgramFilename, Params: string);

// XMLConfig
procedure LoadRecentList(XMLConfig: TXMLConfig; List: TStringList; 
  const Path: string);
procedure SaveRecentList(XMLConfig: TXMLConfig; List: TStringList; 
  const Path: string);
procedure AddToRecentList(const s: string; RecentList: TStringList;
  Max: integer);
procedure LoadRect(XMLConfig: TXMLConfig; const Path:string; var ARect:TRect);
procedure SaveRect(XMLConfig: TXMLConfig; const Path:string; var ARect:TRect);

// miscellaneous
procedure FreeThenNil(var Obj: TObject);


implementation


// to get more detailed error messages consider the os
{$IFDEF Win32}
{$ELSE}
uses
 {$IFDEF Ver1_0}
  Linux
 {$ELSE}
  Unix
 {$ENDIF}
  ;
{$ENDIF}

procedure AddToRecentList(const s: string; RecentList: TStringList;
  Max: integer);
var i: integer;
begin
  i:=RecentList.Count-1;
  while i>=0 do begin
    if RecentList[i]=s then RecentList.Delete(i);
    dec(i);
  end;
  RecentList.Insert(0,s);
  if Max>0 then
    while RecentList.Count>Max do
      RecentList.Delete(RecentList.Count-1);
end;

procedure SaveRecentList(XMLConfig: TXMLConfig; List: TStringList; 
  const Path: string);
var i: integer;
begin
  XMLConfig.SetValue(Path+'Count',List.Count);
  for i:=0 to List.Count-1 do
    XMLConfig.SetValue(Path+'Item'+IntToStr(i+1)+'/Value',List[i]);
end;

procedure LoadRecentList(XMLConfig: TXMLConfig; List: TStringList; 
  const Path: string);
var i,Count: integer;
  s: string;
begin
  Count:=XMLConfig.GetValue(Path+'Count',0);
  List.Clear;
  for i:=1 to Count do begin
    s:=XMLConfig.GetValue(Path+'Item'+IntToStr(i)+'/Value','');
    if s<>'' then List.Add(s);
  end;
end;

procedure LoadRect(XMLConfig: TXMLConfig; const Path:string; var ARect:TRect);
begin
  ARect.Left:=XMLConfig.GetValue(Path+'Left',ARect.Left);
  ARect.Top:=XMLConfig.GetValue(Path+'Top',ARect.Top);
  ARect.Right:=XMLConfig.GetValue(Path+'Right',ARect.Right);
  ARect.Bottom:=XMLConfig.GetValue(Path+'Bottom',ARect.Bottom);
end;

procedure SaveRect(XMLConfig: TXMLConfig; const Path:string; var ARect:TRect);
begin
  XMLConfig.SetValue(Path+'Left',ARect.Left);
  XMLConfig.SetValue(Path+'Top',ARect.Top);
  XMLConfig.SetValue(Path+'Right',ARect.Right);
  XMLConfig.SetValue(Path+'Bottom',ARect.Bottom);
end;

function CompareFilenames(const Filename1, Filename2: string): integer;
begin
  {$IFDEF WIN32}
  Result:=AnsiCompareText(Filename1, Filename2);
  {$ELSE}
  Result:=AnsiCompareStr(Filename1, Filename2);
  {$ENDIF}
end;

function FileIsExecutable(const AFilename: string): boolean;
begin
  try
    CheckIfFileIsExecutable(AFilename);
    Result:=true;
  except
    Result:=false;
  end;
end;

procedure CheckIfFileIsExecutable(const AFilename: string);
var AText: string;
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
  
  // ToDo: windows and xxxbsd
end;

function ExtractFileNameOnly(const AFilename: string): string;
var ExtLen: integer;
begin
  Result:=ExtractFilename(AFilename);
  ExtLen:=length(ExtractFileExt(Result));
  Result:=copy(Result,1,length(Result)-ExtLen);
end;

function FilenameIsAbsolute(Filename: string):boolean;
begin
  DoDirSeparators(Filename);
  {$IFDEF win32}
  // windows
  Result:=(copy(Filename,1,2)='\\') or ((length(Filename)>3) and
     (upcase(Filename[1]) in ['A'..'Z']) and (copy(Filename,2,2)=':\'));
  {$ELSE}
  Result:=(Filename<>'') and (Filename[1]='/');
  {$ENDIF}
end;

function DirectoryExists(DirectoryName: string): boolean;
var sr: TSearchRec;
begin
  if (DirectoryName<>'')
  and (DirectoryName[length(DirectoryName)]=PathDelim) then
    DirectoryName:=copy(DirectoryName,1,length(DirectoryName)-1);
  if FindFirst(DirectoryName,faAnyFile,sr)=0 then
    Result:=((sr.Attr and faDirectory)>0)
  else
    Result:=false;
  FindClose(sr);
end;

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
  end;
  Result:=true;
end;

function FileIsReadable(const AFilename: string): boolean;
begin
  {$IFDEF win32}
  Result:=true;
  {$ELSE}
  Result:={$IFDEF Ver1_0}Linux{$ELSE}Unix{$ENDIF}.Access(
    AFilename,{$IFDEF Ver1_0}Linux{$ELSE}Unix{$ENDIF}.R_OK);
  {$ENDIF}
end;

function FileIsWritable(const AFilename: string): boolean;
begin
  {$IFDEF win32}
  Result:=((FileGetAttr(AFilename) and faReadOnly)>0);
  {$ELSE}
  Result:={$IFDEF Ver1_0}Linux{$ELSE}Unix{$ENDIF}.Access(
    AFilename,{$IFDEF Ver1_0}Linux{$ELSE}Unix{$ENDIF}.W_OK);
  {$ENDIF}
end;

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

function AppendPathDelim(const Path: string): string;
begin
  if (Path<>'') and (Path[length(Path)]<>PathDelim) then
    Result:=Path+PathDelim
  else
    Result:=Path;
end;

function SearchFileInPath(const Filename, BasePath, SearchPath,
  Delimiter: string): string;

  function FileDoesExists(const AFilename: string): boolean;
  var s: string;
  begin
    s:=ExpandFilename(TrimFilename(AFilename));
    Result:=FileExists(s);
    if Result then begin
      SearchFileInPath:=s;
      exit;
    end;
  end;

var
  p, StartPos, l: integer;
  CurPath, Base: string;
begin
//writeln('[SearchFileInPath] Filename="',Filename,'" BasePath="',BasePath,'" SearchPath="',SearchPath,'" Delimiter="',Delimiter,'"');
  if (Filename='') then begin
    Result:='';
    exit;
  end;
  // check if filename absolute
  if FilenameIsAbsolute(Filename) then begin
    if FileDoesExists(Filename) then exit;
    Result:='';
    exit;
  end;
  Base:=AppendPathDelim(BasePath);
  // search in current directory
  if FileDoesExists(Base+Filename) then exit;
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
      if FileDoesExists(AppendPathDelim(CurPath)+Filename) then exit;
    end;
    StartPos:=p+1;
  end;
  Result:='';
end;

procedure SplitCmdLine(const CmdLine: string;
                       var ProgramFilename, Params: string);
var p: integer;
begin
  p:=1;
  while (p<=length(CmdLine)) and (CmdLine[p]>' ') do begin
    if (CmdLine[p] in ['/','\']) and (CmdLine[p]<>PathDelim) then begin
      // skip special char
      inc(p);
    end;
    inc(p);
  end;
  ProgramFilename:=LeftStr(CmdLine,p-1);
  while (p<=length(CmdLine)) and (CmdLine[p]<=' ') do inc(p);
  Params:=RightStr(CmdLine,length(CmdLine)-p+1);
end;

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
          and (Result[2]=PathDelim) then
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

procedure FreeThenNil(var Obj: TObject);
begin
  Obj.Free;
  Obj:=nil;
end;

end.
