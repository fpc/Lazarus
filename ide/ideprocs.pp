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
  Classes, SysUtils, Laz_XMLCfg, GetText,
  FileCtrl, FileProcs;

type
  // comments
  TCommentType = (
    comtDefault,    // decide automatically
    comtNone,       // no comment
    comtPascal,     // {}
    comtDelphi,     // //
    comtTurboPascal,// (* *)
    comtCPP,        // /* */
    comtPerl,       // #
    comtHtml        // <!-- -->
    );
  TCommentTypes = set of TCommentType;

  // copy
  TOnCopyFileMethod =
    procedure(const Filename: string; var Copy: boolean;
      Data: TObject) of object;

  TCopyErrorType = (
    ceSrcDirDoesNotExists,
    ceCreatingDirectory,
    ceCopyFileError
    );
    
  TCopyErrorData = record
    Error: TCopyErrorType;
    Param1: string;
    Param2: string;
  end;
      
  TOnCopyErrorMethod =
    procedure(const ErrorData: TCopyErrorData; var Handled: boolean;
      Data: TObject) of object;

//
const
  // ToDo: find the constant in the fpc units.
  EndOfLine: shortstring={$IFDEF win32}#13+{$ENDIF}#10;

// files
function BackupFile(const Filename, BackupFilename: string): boolean;
function CompareFilenames(const Filename1, Filename2: string): integer;
function SearchFileInPath(const Filename, BasePath, SearchPath,
                          Delimiter: string): string;
function FilenameIsMatching(const Mask, Filename: string;
  MatchExactly: boolean): boolean;
procedure SplitCmdLine(const CmdLine: string;
                       var ProgramFilename, Params: string);
function ConvertSpecialFileChars(const Filename: string): string;
function PrepareCmdLineOption(const Option: string): string;
function CopyFileWithMethods(const SrcFilename, DestFilename: string;
             OnCopyError: TOnCopyErrorMethod; Data: TObject): boolean;
function CopyDirectoryWithMethods(const SrcDirectory, DestDirectory: string;
             OnCopyFile: TOnCopyFileMethod; OnCopyError: TOnCopyErrorMethod;
             Data: TObject): boolean;

// XMLConfig
procedure LoadRecentList(XMLConfig: TXMLConfig; List: TStringList;
  const Path: string);
procedure SaveRecentList(XMLConfig: TXMLConfig; List: TStringList;
  const Path: string);
function AddToRecentList(const s: string; RecentList: TStringList;
  Max: integer): boolean;
procedure RemoveFromRecentList(const s: string; RecentList: TStringList);
procedure LoadRect(XMLConfig: TXMLConfig; const Path:string; var ARect:TRect);
procedure SaveRect(XMLConfig: TXMLConfig; const Path:string; var ARect:TRect);
procedure LoadStringList(XMLConfig: TXMLConfig; List: TStringList;
  const Path: string);
procedure SaveStringList(XMLConfig: TXMLConfig; List: TStringList;
  const Path: string);

// miscellaneous
procedure FreeThenNil(var Obj: TObject);

function TabsToSpaces(const s: string; TabWidth: integer): string;
function CommentLines(const s: string): string;
function CommentText(const s: string; CommentType: TCommentType): string;
function UncommentLines(const s: string): string;
function CrossReplaceChars(const Src: string; PrefixChar: char;
  const SpecialChars: string): string;
function SimpleSyntaxToRegExpr(const Src: string): string;

procedure TranslateResourceStrings(const BaseDirectory, CustomLang: string);

function NameToValidIdentifier(const s: string): string;
function BinaryStrToText(const s: string): string;

function EnvironmentAsStringList: TStringList;
procedure AssignEnvironmentTo(DestStrings, Overrides: TStrings);
function GetCurrentUserName: string;
function GetCurrentMailAddress: string;

procedure RaiseException(const Msg: string);


implementation


// to get more detailed error messages consider the os
uses
  Dos
  {$IfNDef Win32}
     {$IFDEF Ver1_0}
       ,Linux
     {$ELSE}
       ,Unix
     {$ENDIF}
  {$EndIf};

function AddToRecentList(const s: string; RecentList: TStringList;
  Max: integer): boolean;
begin
  if (RecentList.Count>0) and (RecentList[0]=s) then begin
    Result:=false;
    exit;
  end else begin
    Result:=true;
  end;
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
begin
  SaveStringList(XMLConfig,List,Path);
end;

procedure LoadRecentList(XMLConfig: TXMLConfig; List: TStringList;
  const Path: string);
begin
  LoadStringList(XMLConfig,List,Path);
end;

procedure LoadStringList(XMLConfig: TXMLConfig; List: TStringList;
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

procedure SaveStringList(XMLConfig: TXMLConfig; List: TStringList;
  const Path: string);
var i: integer;
begin
  XMLConfig.SetValue(Path+'Count',List.Count);
  for i:=0 to List.Count-1 do
    XMLConfig.SetValue(Path+'Item'+IntToStr(i+1)+'/Value',List[i]);
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
  Result:=FileProcs.CompareFilenames(FileName1,FileName2);
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
    {$IFDEF Ver1_0}sys_eacces{$ELSE}ESysEAcces{$ENDIF}:
      AText:='read access denied for '+AText;
    {$IFDEF Ver1_0}sys_enoent{$ELSE}ESysENoEnt{$ENDIF}:
      AText:='a directory component in '+AText
                          +' does not exist or is a dangling symlink';
    {$IFDEF Ver1_0}sys_enotdir{$ELSE}ESysENotDir{$ENDIF}:
      AText:='a directory component in '+Atext+' is not a directory';
    {$IFDEF Ver1_0}sys_enomem{$ELSE}ESysENoMem{$ENDIF}:
      AText:='insufficient memory';
    {$IFDEF Ver1_0}sys_eloop{$ELSE}ESysELoop{$ENDIF}:
      AText:=AText+' has a circular symbolic link';
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
  Result:=FileProcs.FilenameIsAbsolute(Filename);
end;

function DirectoryExists(DirectoryName: string): boolean;
begin
  Result:=FileProcs.DirectoryExists(DirectoryName);
end;

function ForceDirectory(DirectoryName: string): boolean;
begin
  Result:=FileProcs.ForceDirectory(DirectoryName);
end;

function FileIsReadable(const AFilename: string): boolean;
begin
  Result:=FileProcs.FileIsReadable(AFilename);
end;

function FileIsWritable(const AFilename: string): boolean;
begin
  Result:=FileProcs.FileIsWritable(AFilename);
end;

function FileIsText(const AFilename: string): boolean;
begin
  Result:=FileProcs.FileIsText(AFilename);
end;

function AppendPathDelim(const Path: string): string;
begin
  Result:=FileProcs.AppendPathDelim(Path);
end;

function ChompPathDelim(const Path: string): string;
begin
  Result:=FileProcs.ChompPathDelim(Path);
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

function FilenameIsMatching(const Mask, Filename: string;
  MatchExactly: boolean): boolean;
begin
  Result:=FileProcs.FilenameIsMatching(Mask,Filename,MatchExactly);
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
  
  if not FileIsSymlink(Filename) then begin
    // not a symlink
    // rename old file, create empty new file
  
    // rename file
    if not RenameFile(Filename,BackupFilename) then exit;
    // create empty file
    FHandle:=FileCreate(FileName);
    FileClose(FHandle);
  end else begin
    // file is a symlink
    // -> copy file
    if not CopyFile(Filename,BackupFilename) then exit;
  end;
  
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
  // LCL
  TranslateUnitResourceStrings('LclStrConsts',
    AppendPathDelim(BaseDirectory)+'lcl/languages/lcl.%s.mo',
                    Lang,FallbackLang);
  // IDE without objectinspector
  TranslateUnitResourceStrings('LazarusIDEStrConsts',
    AppendPathDelim(BaseDirectory)+'languages/lazaruside.%s.mo',
                    Lang,FallbackLang);
  // objectinspector
  TranslateUnitResourceStrings('ObjInspStrConsts',
    AppendPathDelim(BaseDirectory)+'languages/objinspstrconsts.%s.mo',
                    Lang,FallbackLang);
  // CodeTools
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
  function BinaryStrToText(const s: string): string;

  Replaces special chars (<#32) into pascal char constants #xxx.
-------------------------------------------------------------------------------}
function BinaryStrToText(const s: string): string;
var
  i, OldLen, NewLen, OldPos, NewPos: integer;
begin
  OldLen:=length(s);
  NewLen:=OldLen;
  for i:=1 to OldLen do begin
    if s[i]<' ' then begin
      inc(NewLen); // one additional char for #
      if ord(s[i])>9 then inc(NewLen);
      if ord(s[i])>99 then inc(NewLen);
    end;
  end;
  if OldLen=NewLen then begin
    Result:=s;
    exit;
  end;
  SetLength(Result,NewLen);
  OldPos:=1;
  NewPos:=1;
  while OldPos<=OldLen do begin
    if s[OldPos]>=' ' then begin
      Result[NewPos]:=s[OldPos];
    end else begin
      Result[NewPos]:='#';
      inc(NewPos);
      i:=ord(s[OldPos]);
      if i>99 then begin
        Result[NewPos]:=chr((i div 100)+ord('0'));
        inc(NewPos);
        i:=i mod 100;
      end;
      if i>9 then begin
        Result[NewPos]:=chr((i div 10)+ord('0'));
        inc(NewPos);
        i:=i mod 10;
      end;
      Result[NewPos]:=chr(i+ord('0'));
    end;
    inc(NewPos);
    inc(OldPos);
  end;
  if NewPos-1<>NewLen then
    RaiseException('ERROR: BinaryStrToText: '+IntToStr(NewLen)+'<>'+IntToStr(NewPos-1));
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

{------------------------------------------------------------------------------
  procedure RaiseException(const Msg: string);

  Raises an exception.
  gdb does not catch fpc Exception objects, therefore this procedure raises
  a standard AV which is catched by gdb.
 ------------------------------------------------------------------------------}
procedure RaiseException(const Msg: string);
begin
  writeln('ERROR in IDE: ',Msg);
  // creates an exception, that gdb catches:
  writeln('Creating gdb catchable error:');
  if (length(Msg) div (length(Msg) div 10000))=0 then ;
end;


function EnvironmentAsStringList: TStringList;
var
  i, SysVarCount, e: integer;
  Variable, Value: string;
Begin
  Result:=TStringList.Create;
  SysVarCount:=EnvCount;
  for i:=0 to SysVarCount-1 do begin
    Variable:=EnvStr(i+1);
    e:=1;
    while (e<=length(Variable)) and (Variable[e]<>'=') do inc(e);
    Value:=copy(Variable,e+1,length(Variable)-e);
    Variable:=LeftStr(Variable,e-1);
    Result.Values[Variable]:=Value;
  end;
end;

procedure AssignEnvironmentTo(DestStrings, Overrides: TStrings);
var
  EnvList: TStringList;
  i: integer;
  Variable, Value: string;
begin
  // get system environment
  EnvList:=EnvironmentAsStringList;
  try
    if Overrides<>nil then begin
      // merge overrides
      for i:=0 to Overrides.Count-1 do begin
        Variable:=Overrides.Names[i];
        Value:=Overrides.Values[Variable];
        EnvList.Values[Variable]:=Value;
      end;
    end;
    DestStrings.Assign(EnvList);
  finally
    EnvList.Free;
  end;
end;

function CopyDirectoryWithMethods(const SrcDirectory, DestDirectory: string;
  OnCopyFile: TOnCopyFileMethod; OnCopyError: TOnCopyErrorMethod;
  Data: TObject): boolean;
  
  function HandleError(ErrorNumber: TCopyErrorType;
    const Param1, Param2: string): boolean;
  var
    ErrorData: TCopyErrorData;
  begin
    Result:=false;
    if Assigned(OnCopyError) then begin
      ErrorData.Error:=ErrorNumber;
      ErrorData.Param1:=Param1;
      ErrorData.Param2:=Param2;
      OnCopyError(ErrorData,Result,Data);
    end;
  end;
  
  function CopyDir(const CurSrcDir, CurDestDir: string): boolean;
  // both dirs must end with PathDelim
  const
    {$IFDEF Win32}
    FindMask = '*.*';
    {$ELSE}
    FindMask = '*';
    {$ENDIF}
  var
    FileInfo: TSearchRec;
    CurFilename,
    SubSrcDir, SubDestDir,
    DestFilename: string;
    DoCopy: boolean;
  begin
    Result:=false;
    if not ForceDirectory(CurDestDir)
    and not HandleError(ceCreatingDirectory,CurDestDir,'') then exit;
    
    if SysUtils.FindFirst(CurSrcDir+FindMask,faAnyFile,FileInfo)=0 then begin
      repeat
        // check if special file
        if (FileInfo.Name='.') or (FileInfo.Name='..') then continue;
        CurFilename:=CurSrcDir+FileInfo.Name;
        // check if src file
        if FilenameIsMatching(DestDirectory,CurFilename,false) then continue;
        
        // check user filter
        if Assigned(OnCopyFile) then begin
          DoCopy:=true;
          OnCopyFile(CurFilename,DoCopy,Data);
          if not DoCopy then continue;
        end;

        // copy
        if (FileInfo.Attr and faDirectory)>0 then begin
          // copy sub directory
          SubSrcDir:=AppendPathDelim(CurFilename);
          SubDestDir:=AppendPathDelim(CurDestDir+FileInfo.Name);
          if not CopyDir(SubSrcDir,SubDestDir) then exit;
        end else begin
          // copy file
          DestFilename:=CurDestDir+FileInfo.Name;
          if not CopyFileWithMethods(CurFilename,DestFilename,OnCopyError,Data)
          then
            exit;
        end;
      until SysUtils.FindNext(FileInfo)<>0;
    end;
    SysUtils.FindClose(FileInfo);
    
    Result:=true;
  end;
  
var
  SrcDir, DestDir: string;
begin
  Result:=true;
  SrcDir:=AppendPathDelim(TrimFilename(SrcDirectory));
  DestDir:=AppendPathDelim(TrimFilename(DestDirectory));
  if CompareFilenames(SrcDir,DestDir)=0 then exit;

  if (not DirectoryExists(SrcDir))
  and not HandleError(ceSrcDirDoesNotExists,SrcDir,'') then exit;
  
  CopyDir(SrcDir,DestDirectory);
end;

function CopyFileWithMethods(const SrcFilename, DestFilename: string;
  OnCopyError: TOnCopyErrorMethod; Data: TObject): boolean;
var
  SrcFileStream, DestFileStream: TFileStream;
  {$IFDEF Win32}
  OldAttr: Longint;
  {$ELSE}
  OldInfo: Stat;
  {$ENDIF}
begin
  Result:=false;
  if CompareFilenames(SrcFilename,DestFilename)=0 then exit;
  
  // read file attributes
  {$IFDEF Win32}
  OldAttr:=FileGetAttr(SrcFilename);
  {$ELSE}
  FStat(SrcFilename,OldInfo);
  {$ENDIF}
  
  //writeln('CopyFileWithMethods ',SrcFilename,' ',DestFilename);
  // copy file
  try
    SrcFileStream:=TFileStream.Create(SrcFilename,fmOpenRead);
    try
      DestFileStream:=TFileSTream.Create(DestFilename,fmCreate);
      try
        DestFileStream.CopyFrom(SrcFileStream,SrcFileStream.Size);
      finally
        DestFileStream.Free;
      end;
    finally
      SrcFileStream.Free;
    end;
  except
    exit;
  end;
  
  // copy file attributes
  {$IFDEF Win32}
  FileSetAttr(DestFileName,OldAttr);
  {$ELSE}
  Chmod(DestFilename,
         OldInfo.Mode and (STAT_IRWXO+STAT_IRWXG+STAT_IRWXU
                           +STAT_ISUID+STAT_ISGID+STAT_ISVTX));
  {$ENDIF}

  Result:=true;
end;

{------------------------------------------------------------------------------
  function CrossReplaceChars(const Src: string; PrefixChar: char;
    const SpecialChars: string): string;

------------------------------------------------------------------------------}
function CrossReplaceChars(const Src: string; PrefixChar: char;
  const SpecialChars: string): string;
var
  SrcLen, SrcPos: Integer;
  DestLen: Integer;
  c: Char;
  NeedsChange: boolean;
  DestPos: Integer;
begin
  Result:=Src;
  SrcLen:=length(Src);
  SrcPos:=1;
  DestLen:=SrcLen;
  NeedsChange:=false;
  while (SrcPos<=SrcLen) do begin
    c:=Src[SrcPos];
    if (c<>PrefixChar) then begin
      if System.Pos(c,SpecialChars)>=1 then begin
        // in front of each SpecialChars will be PrefixChar inserted
        inc(DestLen);
        NeedsChange:=true;
      end;
      inc(SrcPos);
    end else begin
      inc(SrcPos);
      if (SrcPos<=SrcLen) and (System.Pos(Src[SrcPos],SpecialChars)>=1) then
      begin
        // each prefixed SpecialChars will be reduced
        dec(DestLen);
        NeedsChange:=true;
      end;
      inc(SrcPos);
    end;
  end;
  if not NeedsChange then exit;
  SetLength(Result,DestLen);
  SrcPos:=1;
  DestPos:=1;
  while (SrcPos<=SrcLen) do begin
    c:=Src[SrcPos];
    if (c<>PrefixChar) then begin
      if System.Pos(c,SpecialChars)>=1 then begin
        // in front of each SpecialChars will be PrefixChar inserted
        Result[DestPos]:=PrefixChar;
        inc(DestPos);
      end;
      Result[DestPos]:=c;
      inc(SrcPos);
      inc(DestPos);
    end else begin
      inc(SrcPos);
      if SrcPos<=SrcLen then begin
        if (System.Pos(Src[SrcPos],SpecialChars)<1) then begin
          Result[DestPos]:=c;
          inc(DestPos);
        end;
        Result[DestPos]:=Src[SrcPos];
        inc(DestPos);
        inc(SrcPos);
      end else begin
        Result[DestPos]:=c;
        inc(DestPos);
      end;
    end;
  end;
end;

{------------------------------------------------------------------------------
  function SimpleSyntaxToRegExpr(const Src: string): string;

  . -> \.
  * -> .*
  ? -> .
  , -> |
  ; -> |
  
  Finally enclose by ^( )$
------------------------------------------------------------------------------}
function SimpleSyntaxToRegExpr(const Src: string): string;
var
  SrcLen, SrcPos: Integer;
  DestLen: Integer;
  c: Char;
  DestPos: Integer;
begin
  Result:=Src;
  SrcLen:=length(Src);
  SrcPos:=1;
  DestLen:=SrcLen+4;
  while (SrcPos<=SrcLen) do begin
    c:=Src[SrcPos];
    case c of
    '\': inc(SrcPos);
    '*','.':
      inc(DestLen);
    end;
    inc(SrcPos);
  end;
  SetLength(Result,DestLen);
  SrcPos:=1;
  Result[1]:='^';
  Result[2]:='(';
  DestPos:=3;
  while (SrcPos<=SrcLen) do begin
    c:=Src[SrcPos];
    case c of
    '\':
      begin
        Result[DestPos]:=c;
        inc(DestPos);
        inc(SrcPos);
        Result[DestPos]:=Src[SrcPos];
        inc(DestPos);
      end;
    '.':
      begin
        Result[DestPos]:='\';
        inc(DestPos);
        Result[DestPos]:='.';
        inc(DestPos);
      end;
    '*':
      begin
        Result[DestPos]:='.';
        inc(DestPos);
        Result[DestPos]:='*';
        inc(DestPos);
      end;
    '?':
      begin
        Result[DestPos]:='.';
        inc(DestPos);
      end;
    ',',';':
      begin
        Result[DestPos]:='|';
        inc(DestPos);
      end;
    else
      Result[DestPos]:=Src[SrcPos];
      inc(DestPos);
    end;
    inc(SrcPos);
  end;
  Result[DestPos]:=')';
  inc(DestPos);
  Result[DestPos]:='$';
end;

end.

