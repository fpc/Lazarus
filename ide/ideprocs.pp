{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Simple functions
   - for file access, not yet in fpc.
   - recent list
   - xmlconfig formats
}
unit IDEProcs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Laz_XMLCfg, GetText;

type
  TCommentType = (
    comtDefault,    // automatically decide
    comtNone,       // no comment
    comtPascal,     // {}
    comtDelphi,     // //
    comtTurboPascal,// (* *)
    comtCPP,        // /* */
    comtPerl,       // #
    comtHtml        // <!-- -->
    );
  TCommentTypes = set of TCommentType;

//
const
  // ToDo: find the constant in the fpc units.
  EndOfLine:shortstring={$IFDEF win32}#13+{$ENDIF}#10;

// files
function FilenameIsAbsolute(Filename: string):boolean;
function DirectoryExists(DirectoryName: string): boolean;
function ForceDirectory(DirectoryName: string): boolean;
function BackupFile(const Filename, BackupFilename: string): boolean;
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
function ConvertSpecialFileChars(const Filename: string): string;
function PrepareCmdLineOption(const Option: string): string;

// XMLConfig
procedure LoadRecentList(XMLConfig: TXMLConfig; List: TStringList; 
  const Path: string);
procedure SaveRecentList(XMLConfig: TXMLConfig; List: TStringList; 
  const Path: string);
procedure AddToRecentList(const s: string; RecentList: TStringList;
  Max: integer);
procedure RemoveFromRecentList(const s: string; RecentList: TStringList);
procedure LoadRect(XMLConfig: TXMLConfig; const Path:string; var ARect:TRect);
procedure SaveRect(XMLConfig: TXMLConfig; const Path:string; var ARect:TRect);

// miscellaneous
procedure FreeThenNil(var Obj: TObject);
function TabsToSpaces(const s: string; TabWidth: integer): string;
function CommentLines(const s: string): string;
function CommentText(const s: string; CommentType: TCommentType): string;
function UncommentLines(const s: string): string;
procedure TranslateResourceStrings(const BaseDirectory, CustomLang: string);
function NameToValidIdentifier(const s: string): string;
function GetCurrentUserName: string;
function GetCurrentMailAddress: string;

implementation


// to get more detailed error messages consider the os
{$IFDEF Win32}
uses
  Dos;
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
begin
  RemoveFromRecentList(s,RecentList);
  RecentList.Insert(0,s);
  if Max>0 then
    while RecentList.Count>Max do
      RecentList.Delete(RecentList.Count-1);
end;

procedure RemoveFromRecentList(const s: string; RecentList: TStringList);
var i: integer;
begin
  i:=RecentList.Count-1;
  while i>=0 do begin
    if RecentList[i]=s then RecentList.Delete(i);
    dec(i);
  end;
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
  if SysUtils.FindFirst(DirectoryName,faAnyFile,sr)=0 then
    Result:=((sr.Attr and faDirectory)>0)
  else
    Result:=false;
  SysUtils.FindClose(sr);
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
  Result:=FileExists(AFilename);
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

procedure FreeThenNil(var Obj: TObject);
begin
  Obj.Free;
  Obj:=nil;
end;

{-------------------------------------------------------------------------------
  BackupFile

  Params: const Filename, BackupFilename: string
  Result: boolean
  
  Rename Filename to Backupfilename and create empty Filename with same
  file attributes
-------------------------------------------------------------------------------}
function BackupFile(const Filename, BackupFilename: string): boolean;
var
  FHandle: Integer;
  {$IFDEF Win32}
  OldAttr: Longint;
  {$ELSE}
  OldInfo: Stat;
  {$ENDIF}
begin
  Result:=false;
  // store file attributes
  {$IFDEF Win32}
  OldAttr:=FileGetAttr(Filename);
  {$ELSE}
  FStat(Filename,OldInfo);
  {$ENDIF}
  // rename file
  if not RenameFile(Filename,BackupFilename) then exit;
  // create empty file
  FHandle:=FileCreate(FileName);
  FileClose(FHandle);
  // restore file attributes
  {$IFDEF Win32}
  FileSetAttr(FileName,OldAttr);
  {$ELSE}
  Chmod(Filename,
         OldInfo.Mode and (STAT_IRWXO+STAT_IRWXG+STAT_IRWXU
                           +STAT_ISUID+STAT_ISGID+STAT_ISVTX));
  {$ENDIF}

  Result:=true;
end;

{-------------------------------------------------------------------------------
  TabsToSpaces

  Params: const s: string; TabWidth: integer
  Result: string

  Convert all tabs to TabWidth number of spaces.
-------------------------------------------------------------------------------}
function TabsToSpaces(const s: string; TabWidth: integer): string;
var i, SrcLen, TabCount, SrcPos, DestPos: integer;
begin
  SrcLen:=length(s);
  TabCount:=0;
  for SrcPos:=1 to SrcLen do
    if s[SrcPos]=#9 then inc(TabCount);
  if TabCount=0 then begin
    Result:=s;
    exit;
  end;
  SetLength(Result,SrcLen+TabCount*(TabWidth-1));
  DestPos:=1;
  for SrcPos:=1 to SrcLen do begin
    if s[SrcPos]<>#9 then begin
      Result[DestPos]:=s[SrcPos];
      inc(DestPos);
    end else begin
      for i:=1 to TabWidth do begin
        Result[DestPos]:=' ';
        inc(DestPos);
      end;
    end;
  end;
end;

procedure TranslateUnitResourceStrings(const ResUnitName, AFilename: string);
var
  mo: TMOFile;
  TableID, StringID, TableCount: Integer;
  s: String;
begin
  if (ResUnitName='') or (AFilename='') or (not FileExists(AFilename)) then
    exit;
  try
    mo := TMOFile.Create(AFilename);
    try
      for TableID:=0 to ResourceStringTableCount - 1 do
      begin
        TableCount := ResourceStringCount(TableID);

        // check if this table belongs to the ResUnitName
        if TableCount=0 then continue;
        s:=GetResourceStringName(TableID,0);
        if AnsiCompareText(ResUnitName+'.',LeftStr(s,length(ResUnitName)+1))<>0
        then continue;

        // translate all resource strings of the unit
        for StringID := 0 to TableCount - 1 do begin
          s := mo.Translate(GetResourceStringDefaultValue(TableID,StringID),
            GetResourceStringHash(TableID,StringID));
          if Length(s) > 0 then begin
            SetResourceStringValue(TableID,StringID,s);
          end;
        end;
      end;
    finally
      mo.Free;
    end;
  except
    on e: Exception do;
  end;
end;

procedure TranslateUnitResourceStrings(const ResUnitName, BaseFilename,
  Lang, FallbackLang: string);
begin
  if (ResUnitName='') or (BaseFilename='')
  or ((Lang='') and (FallbackLang='')) then exit;
  
  if FallbackLang<>'' then
    TranslateUnitResourceStrings(ResUnitName,Format(BaseFilename,[FallbackLang]));
  if Lang<>'' then
    TranslateUnitResourceStrings(ResUnitName,Format(BaseFilename,[Lang]));
end;

procedure GetLanguageIDs(var Lang, FallbackLang: string);
begin
  Lang := GetEnv('LC_ALL');
  FallbackLang:='';
  
  if Length(Lang) = 0 then
  begin
    Lang := GetEnv('LC_MESSAGES');
    if Length(Lang) = 0 then
    begin
      Lang := GetEnv('LANG');
      if Length(Lang) = 0 then
        exit;   // no language defined via environment variables
    end;
  end;

  FallbackLang := Copy(Lang, 1, 2);
  Lang := Copy(Lang, 1, 5);
end;

{-------------------------------------------------------------------------------
  TranslateResourceStrings

  Params: none
  Result: none

  Translates all resourcestrings of the resource string files:
    - lclstrconsts.pas
    - codetoolsstrconsts.pas
    - lazarusidestrconsts.pas
-------------------------------------------------------------------------------}
procedure TranslateResourceStrings(const BaseDirectory, CustomLang: string);
var
  Lang, FallbackLang: String;
begin
  if CustomLang='' then begin
    GetLanguageIDs(Lang,FallbackLang);
  end else begin
    Lang:=CustomLang;
    FallbackLang:='';
  end;
  TranslateUnitResourceStrings('LclStrConsts',
     AppendPathDelim(BaseDirectory)+'lcl/languages/lcl.%s.mo',
                     Lang,FallbackLang);
  TranslateUnitResourceStrings('LazarusIDEStrConsts',
     AppendPathDelim(BaseDirectory)+'languages/lazaruside.%s.mo',
                     Lang,FallbackLang);
  TranslateUnitResourceStrings('CodeToolsStrConsts',
     AppendPathDelim(BaseDirectory)+'components/codetools/languages/codetools.%s.mo',
                     Lang,FallbackLang);
end;

{-------------------------------------------------------------------------------
  NameToValidIdentifier

  Params: const s: string
  Result: string

  Replaces all non identifier characters into underscores '_'
-------------------------------------------------------------------------------}
function NameToValidIdentifier(const s: string): string;
var i: integer;
begin
  if s='' then begin
    Result:='_';
  end else begin
    Result:=s;
    if not (Result[1] in ['A'..'Z', 'a'..'z', '_']) then begin
      Result[1]:='_';
    end;
    for i:=2 to length(Result) do begin
      if not (Result[i] in ['A'..'Z', 'a'..'z', '0'..'9', '_']) then begin
        Result[i]:='_';
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  ConvertSpecialFileChars

  Params: const Filename: string
  Result: string

  Replaces all spaces in a filename.
-------------------------------------------------------------------------------}
function ConvertSpecialFileChars(const Filename: string): string;
const
  SpecialChar = '\';
var i: integer;
begin
  Result:=Filename;
  i:=1;
  while (i<=length(Result)) do begin
    if Result[i]<>' ' then begin
      inc(i);
    end else begin
      Result:=LeftStr(Result,i-1)+SpecialChar+RightStr(Result,length(Result)-i+1);
      inc(i,2);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  PrepareCmdLineOption

  Params: const Option: string
  Result: string

  If there is a space in the option add " "
-------------------------------------------------------------------------------}
function PrepareCmdLineOption(const Option: string): string;
var i: integer;
begin
  Result:=Option;
  for i:=1 to length(Result) do begin
    if Result[i]=' ' then begin
      Result:='"'+Result+'"';
      exit;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  function CommentLines(const s: string): string;

  Comment every line with a Delphicomment //
-------------------------------------------------------------------------------}
function CommentLines(const s: string): string;
var
  CurPos: integer;
  Dest: string;
  
  procedure FindLineEnd;
  begin
    while (CurPos<=length(Dest))
    and (not (Dest[CurPos] in [#10,#13])) do
      inc(CurPos);
  end;

  procedure CommentLine;
  begin
    Dest:=LeftStr(Dest,CurPos-1)+'//'+RightStr(Dest,length(Dest)-CurPos+1);
    FindLineEnd;
  end;

begin
  Dest:=s;
  CurPos:=1;
  // find code start in line
  while (CurPos<=length(Dest)) do begin
    case Dest[CurPos] of
    
    ' ',#9:
      // skip space
      inc(CurPos);

    #10,#13:
      // line end found -> skip
      inc(CurPos);

    else
      // code start found
      CommentLine;
    end;
  end;
  Result:=Dest;
end;

{-------------------------------------------------------------------------------
  function CommentLines(const s: string; CommentType: TCommentType): string;

  Comment s.
-------------------------------------------------------------------------------}
function CommentText(const s: string; CommentType: TCommentType): string;

  procedure GetTextInfo(var Len, LineCount: integer;
    var LastLineEmpty: boolean);
  var
    p: integer;
  begin
    Len:=length(s);
    LineCount:=1;
    p:=1;
    while p<=Len do
      if not (s[p] in [#10,#13]) then begin
        inc(p);
      end else begin
        inc(p);
        inc(LineCount);
        if (p<=Len) and (s[p] in [#10,#13]) and (s[p]<>s[p-1]) then
          inc(p);
      end;
    LastLineEmpty:=(Len=0) or (s[Len] in [#10,#13]);
  end;

  procedure DoCommentBlock(const FirstLineStart, LineStart, LastLine: string);
  var
    OldLen, NewLen, LineCount, OldPos, NewPos: integer;
    LastLineEmpty: boolean;
  begin
    GetTextInfo(OldLen,LineCount,LastLineEmpty);
    
    NewLen:=OldLen+length(FirstLineStart)
                  +(LineCount-1)*length(LineStart);
    if LastLineEmpty then
      dec(NewLen,length(LineStart))
    else
      inc(NewLen,length(EndOfLine));
    if (LastLine<>'') then begin
      inc(NewLen,length(LastLine)+length(EndOfLine));
    end;

    SetLength(Result,NewLen);
    NewPos:=1;
    OldPos:=1;

    // add first line start
    if FirstLineStart<>'' then begin
      System.Move(FirstLineStart[1],Result[NewPos],length(FirstLineStart));
      inc(NewPos,length(FirstLineStart));
    end;
    // copy all lines and add new linestart
    while (OldPos<=OldLen) do begin
      if (not (s[OldPos] in [#10,#13])) then begin
        Result[NewPos]:=s[OldPos];
        inc(OldPos);
        inc(NewPos);
      end else begin
        Result[NewPos]:=s[OldPos];
        inc(OldPos);
        inc(NewPos);
        if (OldPos<=OldLen) and (s[OldPos] in [#10,#13])
        and (s[OldPos]<>s[OldPos-1]) then begin
          Result[NewPos]:=s[OldPos];
          inc(OldPos);
          inc(NewPos);
        end;
        // start new line
        if (LineStart<>'') and (OldPos<OldLen) then begin
          System.Move(LineStart[1],Result[NewPos],length(LineStart));
          inc(NewPos,length(LineStart));
        end;
      end;
    end;
    if not LastLineEmpty then begin
      System.Move(EndOfLine[1],Result[NewPos],length(EndOfLine));
      inc(NewPos,length(EndOfLine));
    end;
    // add last line
    if LastLine<>'' then begin
      System.Move(LastLine[1],Result[NewPos],length(LastLine));
      inc(NewPos,length(LastLine));
      System.Move(EndOfLine[1],Result[NewPos],length(EndOfLine));
      inc(NewPos,length(EndOfLine));
    end;
    if NewPos<>NewLen+1 then
      raise Exception.Create('IDEProcs.CommentText ERROR: '
        +IntToStr(NewPos-1)+'<>'+IntToStr(NewLen));
  end;

begin
  Result:=s;
  if CommentType=comtNone then exit;
  if CommentType=comtDefault then CommentType:=comtPascal;
    
  case CommentType of
    comtPascal: DoCommentBlock('{ ','  ','}');
    comtDelphi: DoCommentBlock('// ','// ','');
    comtTurboPascal: DoCommentBlock('(* ',' * ',' *)');
    comtCPP: DoCommentBlock('/* ',' * ',' */');
    comtPerl: DoCommentBlock('# ','# ','');
    comtHtml: DoCommentBlock('<!-- ','  ','-->');
  end;
end;

{-------------------------------------------------------------------------------
  function CommentLines(const s: string): string;

  Uncomment every line with a Delphicomment //
-------------------------------------------------------------------------------}
function UncommentLines(const s: string): string;
var
  CurPos: integer;
  Dest: string;

  procedure FindLineEnd;
  begin
    while (CurPos<=length(Dest))
    and (not (Dest[CurPos] in [#10,#13])) do
      inc(CurPos);
  end;

  procedure UncommentLine;
  begin
    Dest:=LeftStr(Dest,CurPos-1)+RightStr(Dest,length(Dest)-CurPos-1);
    FindLineEnd;
  end;

begin
  Dest:=s;
  CurPos:=1;
  // find Delphi comment line
  while (CurPos<=length(Dest)) do begin
    case Dest[CurPos] of

    ' ',#9:
      // skip space
      inc(CurPos);

    #10,#13:
      // line end found -> skip
      inc(CurPos);

    else
      // code start found
      if (Dest[CurPos]='/') and (CurPos<length(Dest)) and (Dest[CurPos+1]='/')
      then
        UncommentLine;
      FindLineEnd;
    end;
  end;
  Result:=Dest;
end;

function GetCurrentUserName: string;
begin
  Result:=GetEnv('USER');
end;

function GetCurrentMailAddress: string;
begin
  Result:='<'+GetCurrentUserName+'@'+GetEnv('HOSTNAME')+'>';
end;

end.
