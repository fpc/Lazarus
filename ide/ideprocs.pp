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
  Classes, SysUtils, Laz_XMLCfg, GetText, FileCtrl, FileProcs;

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
  EmptyLine: shortstring={$IFDEF win32}#13#10#13#10{$ELSE}#10#10{$ENDIF};

// files
function TrimSearchPath(const SearchPath, BaseDirectory: string): string;
function BackupFile(const Filename, BackupFilename: string): boolean;
function ClearFile(const Filename: string; RaiseOnError: boolean): boolean;
function CompareFilenames(const Filename1, Filename2: string): integer;
function CompareFilenames(const Filename1, Filename2: string;
  ResolveLinks: boolean): integer;
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
function ProgramDirectory: string;
function FindFilesCaseInsensitive(const Directory,
  CaseInsensitiveFilename: string; IgnoreExact: boolean): TStringList;
function FindFirstFileWithExt(const Directory, Ext: string): string;
function FindShortFileNameOnDisk(const Filename: string): string;
function FilenameIsPascalUnit(const Filename: string): boolean;
function FilenameIsPascalSource(const Filename: string): boolean;
function FilenameIsFormText(const Filename: string): boolean;
function MergeSearchPaths(const OldSearchPath, AddSearchPath: string): string;
function RemoveSearchPaths(const SearchPath, RemoveSearchPath: string): string;
function CreateRelativeSearchPath(const SearchPath, BaseDirectory: string): string;
function ShortenSearchPath(const SearchPath, BaseDirectory, ChompDirectory: string): string;
function GetNextDirectoryInSearchPath(const SearchPath: string;
                                      var NextStartPos: integer): string;
function SearchDirectoryInSearchPath(const SearchPath, Directory: string;
                                     DirStartPos: integer): integer;
function CreateRelativePath(const Filename, BaseDirectory: string): string;
function CreateAbsolutePath(const SearchPath, BaseDirectory: string): string;
function SwitchPathDelims(const Filename: string; Switch: boolean): string;

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
procedure LoadStringList(XMLConfig: TXMLConfig; List: TStrings;
  const Path: string);
procedure SaveStringList(XMLConfig: TXMLConfig; List: TStrings;
  const Path: string);
function FindProgram(const Programname, BaseDirectory: string;
  WithBaseDirectory: boolean): string;

// text conversion
function TabsToSpaces(const s: string; TabWidth: integer): string;
function CommentLines(const s: string): string;
function CommentText(const s: string; CommentType: TCommentType): string;
function UncommentLines(const s: string): string;
function CrossReplaceChars(const Src: string; PrefixChar: char;
  const SpecialChars: string): string;
function SimpleSyntaxToRegExpr(const Src: string): string;
function NameToValidIdentifier(const s: string): string;
function BinaryStrToText(const s: string): string;
function SplitString(const s: string; Delimiter: char): TStringList;
function SpecialCharsToSpaces(const s: string): string;

// translation/internationalization/localization
procedure TranslateResourceStrings(const BaseDirectory, CustomLang: string);

// environment
function EnvironmentAsStringList: TStringList;
procedure AssignEnvironmentTo(DestStrings, Overrides: TStrings);
function GetCurrentUserName: string;
function GetCurrentMailAddress: string;
procedure GetProgramSearchPath(var SearchPath: string; var Delim: char);

// debugging
procedure RaiseException(const Msg: string);

// miscellaneous
procedure FreeThenNil(var Obj: TObject);
function CompareCaret(const FirstCaret, SecondCaret: TPoint): integer;
function CompareBoolean(b1, b2: boolean): integer;
function CompareStringPointerI(Data1, Data2: Pointer): integer;
procedure CheckList(List: TList; TestListNil, TestDoubles, TestNils: boolean);
procedure CheckEmptyListCut(List1, List2: TList);
function AnsiSearchInStringList(List: TStrings; const s: string): integer;
procedure FindMatchingTextFiles(FileList: TStringList; TheDirectory: string;
                                mask: string; recursive: boolean);
function FindInFiles(TheFileList: TStringList; Searchfor: String;
                     WholeWord: Boolean; CaseSensitive: Boolean;
                     RegExp: Boolean): TStringList;

const
  {$IFDEF Win32}
  FindMask = '*.*';
  {$ELSE}
  FindMask = '*';
  {$ENDIF}


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

{-------------------------------------------------------------------------------
  function FindFilesCaseInsensitive(const Directory,
    CaseInsensitiveFilename: string; IgnoreExact: boolean): TStringLists;

  Search case insensitive in Directory for all files
  named CaseInsensitiveFilename
-------------------------------------------------------------------------------}
function FindFilesCaseInsensitive(const Directory,
  CaseInsensitiveFilename: string; IgnoreExact: boolean): TStringList;
var
  FileInfo: TSearchRec;
begin
  Result:=nil;
  if SysUtils.FindFirst(AppendPathDelim(Directory)+FindMask,
                        faAnyFile,FileInfo)=0
  then begin
    repeat
      // check if special file
      if (FileInfo.Name='.') or (FileInfo.Name='..') then continue;
      if (AnsiCompareText(CaseInsensitiveFilename,FileInfo.Name)=0)
      and ((not IgnoreExact)
           or (CompareFilenames(CaseInsensitiveFilename,FileInfo.Name)<>0))
      then begin
        if Result=nil then Result:=TStringList.Create;
        Result.Add(FileInfo.Name);
      end;
    until SysUtils.FindNext(FileInfo)<>0;
  end;
  SysUtils.FindClose(FileInfo);
end;

function FilenameIsPascalSource(const Filename: string): boolean;
var Ext: string;
begin
  Ext:=lowercase(ExtractFileExt(Filename));
  Result:=((Ext='.pp') or (Ext='.pas') or (Ext='.lpr')
          or (Ext='.dpr') or (Ext='.dpk'))
          and (ExtractFileNameOnly(Filename)<>'');
end;

function FindShortFileNameOnDisk(const Filename: string): string;
var
  FileInfo: TSearchRec;
  ADirectory: String;
  ShortFilename: String;
begin
  Result:='';
  ADirectory:=ExtractFilePath(Filename);
  if SysUtils.FindFirst(AppendPathDelim(ADirectory)+FindMask,
                        faAnyFile,FileInfo)=0
  then begin
    ShortFilename:=ExtractFilename(Filename);
    repeat
      // check if special file
      if (FileInfo.Name='.') or (FileInfo.Name='..') then continue;
      if CompareFilenames(ShortFilename,FileInfo.Name)=0 then begin
        Result:=FileInfo.Name;
        break;
      end;
    until SysUtils.FindNext(FileInfo)<>0;
  end;
  SysUtils.FindClose(FileInfo);
end;

function FilenameIsPascalUnit(const Filename: string): boolean;
var Ext: string;
begin
  Ext:=lowercase(ExtractFileExt(Filename));
  Result:=((Ext='.pp') or (Ext='.pas'))
          and (ExtractFileNameOnly(Filename)<>'');
end;

function FilenameIsFormText(const Filename: string): boolean;
var Ext: string;
begin
  Ext:=lowercase(ExtractFileExt(Filename));
  Result:=((Ext='.lfm') or (Ext='.dfm') or (Ext='.xfm'))
          and (ExtractFileNameOnly(Filename)<>'');
end;

function MergeSearchPaths(const OldSearchPath, AddSearchPath: string): string;
var
  l: Integer;
  EndPos: Integer;
  StartPos: Integer;
  NewPath: String;
begin
  Result:=OldSearchPath;
  if Result='' then begin
    Result:=AddSearchPath;
    exit;
  end;
  l:=length(AddSearchPath);
  EndPos:=1;
  while EndPos<=l do begin
    StartPos:=EndPos;
    while (AddSearchPath[StartPos]=';') do begin
      inc(StartPos);
      if StartPos>l then exit;
    end;
    EndPos:=StartPos;
    while (EndPos<=l) and (AddSearchPath[EndPos]<>';') do inc(EndPos);
    if SearchDirectoryInSearchPath(Result,AddSearchPath,StartPos)<1 then
    begin
      // new path found -> add
      NewPath:=copy(AddSearchPath,StartPos,EndPos-StartPos);
      if Result<>'' then
        NewPath:=';'+NewPath;
      Result:=Result+NewPath;
    end;
  end;
end;

function RemoveSearchPaths(const SearchPath, RemoveSearchPath: string): string;
var
  OldPathLen: Integer;
  EndPos: Integer;
  StartPos: Integer;
  ResultStartPos: Integer;
begin
  Result:=SearchPath;
  OldPathLen:=length(SearchPath);
  EndPos:=1;
  ResultStartPos:=1;
  repeat
    StartPos:=EndPos;
    while (StartPos<=OldPathLen) and (SearchPath[StartPos]=';') do
      inc(StartPos);
    if StartPos>OldPathLen then break;
    EndPos:=StartPos;
    while (EndPos<=OldPathLen) and (SearchPath[EndPos]<>';') do
      inc(EndPos);
    if SearchDirectoryInSearchPath(RemoveSearchPath,SearchPath,StartPos)>0 then
    begin
      // remove path -> skip
    end else begin
      // keep path -> copy
      if ResultStartPos>1 then begin
        Result[ResultStartPos]:=';';
        inc(ResultStartPos);
      end;
      while StartPos<EndPos do begin
        Result[ResultStartPos]:=SearchPath[StartPos];
        inc(ResultStartPos);
        inc(StartPos);
      end;
    end;
  until false;
  SetLength(Result,ResultStartPos-1);
end;

function ShortenSearchPath(const SearchPath, BaseDirectory,
  ChompDirectory: string): string;
// every search path that is a subdirectory of ChompDirectory will be shortened
// before the test relative paths are expanded by BaseDirectory
var
  BaseEqualsChompDir: boolean;

  function Normalize(var ADirectory: string): boolean;
  begin
    if FilenameIsAbsolute(ADirectory) then begin
      Result:=true;
      ADirectory:=ADirectory;
    end else begin
      if BaseEqualsChompDir then
        Result:=false
      else begin
        Result:=true;
        ADirectory:=AppendPathDelim(BaseDirectory)+ADirectory;
      end;
    end;
    if Result then
      ADirectory:=AppendPathDelim(TrimFilename(ADirectory));
  end;

var
  PathLen: Integer;
  EndPos: Integer;
  StartPos: Integer;
  CurDir: String;
  NewCurDir: String;
  DiffLen: Integer;
begin
  Result:=SearchPath;
  if (SearchPath='') or (ChompDirectory='') then exit;

  PathLen:=length(Result);
  EndPos:=1;
  BaseEqualsChompDir:=CompareFilenames(BaseDirectory,ChompDirectory)=0;
  while EndPos<=PathLen do begin
    StartPos:=EndPos;
    while (Result[StartPos]=';') do begin
      inc(StartPos);
      if StartPos>PathLen then exit;
    end;
    EndPos:=StartPos;
    while (EndPos<=PathLen) and (Result[EndPos]<>';') do inc(EndPos);
    CurDir:=copy(Result,StartPos,EndPos-StartPos);
    NewCurDir:=CurDir;
    if Normalize(NewCurDir) then begin
      if CompareFilenames(NewCurDir,ChompDirectory)=0 then
        NewCurDir:='.'
      else if FileIsInPath(NewCurDir,ChompDirectory) then
        NewCurDir:=AppendPathDelim(CreateRelativePath(NewCurDir,BaseDirectory));
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

function GetNextDirectoryInSearchPath(const SearchPath: string;
                                      var NextStartPos: integer): string;
var
  PathLen: Integer;
  CurStartPos: Integer;
begin
  PathLen:=length(SearchPath);
  repeat
    while (NextStartPos<=PathLen) and (SearchPath[NextStartPos]=';') do
      inc(NextStartPos);
    CurStartPos:=NextStartPos;
    while (NextStartPos<=PathLen) and (SearchPath[NextStartPos]<>';') do
      inc(NextStartPos);
    Result:=TrimFilename(copy(SearchPath,CurStartPos,NextStartPos-CurStartPos));
    if Result<>'' then exit;
  until (NextStartPos>PathLen);
  Result:='';
end;

function SearchDirectoryInSearchPath(const SearchPath,
  Directory: string; DirStartPos: integer): integer;
var
  PathLen: Integer;
  DirLen: Integer;
  EndPos: Integer;
  StartPos: Integer;
  DirEndPos: Integer;
  CurDirLen: Integer;
  i: Integer;
begin
  Result:=-1;
  DirLen:=length(Directory);
  if (SearchPath='')
  or (Directory='') or (DirStartPos>DirLen) or (Directory[DirStartPos]=';') then
    exit;
  DirEndPos:=DirStartPos;
  while (DirEndPos<=DirLen) and (Directory[DirEndPos]<>';') do inc(DirEndPos);
  CurDirLen:=DirEndPos-DirStartPos;
  PathLen:=length(SearchPath);
  EndPos:=1;
  while EndPos<=PathLen do begin
    StartPos:=EndPos;
    while (SearchPath[StartPos]=';') do begin
      inc(StartPos);
      if StartPos>PathLen then exit;
    end;
    EndPos:=StartPos;
    while (EndPos<=PathLen) and (SearchPath[EndPos]<>';') do inc(EndPos);
    if EndPos-StartPos=CurDirLen then begin
      i:=CurDirLen-1;
      while i>=0 do begin
        if SearchPath[StartPos+i]<>Directory[DirStartPos+i] then break;
        dec(i);
      end;
      if i<0 then begin
        Result:=StartPos;
        exit;
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
  // check for different windows file drives
  if (AnsiCompareText(ExtractFileDrive(Filename),
                     ExtractFileDrive(BaseDirectory))<>0)
  then
    exit;
    
  FileNameLength:=length(Filename);
  BaseDirLen:=length(BaseDirectory);

  // skip matching directories
  MinLen:=FileNameLength;
  if MinLen>BaseDirLen then MinLen:=BaseDirLen;
  SamePos:=1;
  while (SamePos<=MinLen) do begin
    {$IFDEF win32}
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
    if BaseDirectory[BaseDirPos]=PathDelim then inc(UpDirCount);
    inc(BaseDirPos);
  end;
  if BaseDirectory[BaseDirLen]<>PathDelim then inc(UpDirCount);
  
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

function CreateAbsolutePath(const SearchPath, BaseDirectory: string): string;
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

function SwitchPathDelims(const Filename: string; Switch: boolean): string;
begin
  Result:=Filename;
  if Switch then
    DoDirSeparators(Result);
end;

function FindFirstFileWithExt(const Directory, Ext: string): string;
var
  FileInfo: TSearchRec;
begin
  Result:='';
  if SysUtils.FindFirst(AppendPathDelim(Directory)+FindMask,
                        faAnyFile,FileInfo)=0
  then begin
    repeat
      // check if special file
      if (FileInfo.Name='.') or (FileInfo.Name='..') then continue;
      // check extension
      if CompareFileExt(FileInfo.Name,Ext,false)=0 then begin
        Result:=AppendPathDelim(Directory)+FileInfo.Name;
        break;
      end;
    until SysUtils.FindNext(FileInfo)<>0;
  end;
  SysUtils.FindClose(FileInfo);
end;

procedure LoadRecentList(XMLConfig: TXMLConfig; List: TStringList;
  const Path: string);
begin
  LoadStringList(XMLConfig,List,Path);
end;

procedure LoadStringList(XMLConfig: TXMLConfig; List: TStrings;
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

procedure SaveStringList(XMLConfig: TXMLConfig; List: TStrings;
  const Path: string);
var i: integer;
begin
  XMLConfig.SetDeleteValue(Path+'Count',List.Count,0);
  for i:=0 to List.Count-1 do
    XMLConfig.SetDeleteValue(Path+'Item'+IntToStr(i+1)+'/Value',List[i],'');
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
  XMLConfig.SetDeleteValue(Path+'Left',ARect.Left,0);
  XMLConfig.SetDeleteValue(Path+'Top',ARect.Top,0);
  XMLConfig.SetDeleteValue(Path+'Right',ARect.Right,0);
  XMLConfig.SetDeleteValue(Path+'Bottom',ARect.Bottom,0);
end;

function CompareFilenames(const Filename1, Filename2: string): integer;
begin
  Result:=FileCtrl.CompareFilenames(FileName1,FileName2);
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

function AppendPathDelim(const Path: string): string;
begin
  Result:=FileProcs.AppendPathDelim(Path);
end;

function CompareFilenames(const Filename1, Filename2: string;
  ResolveLinks: boolean): integer;
begin
  Result:=FileCtrl.CompareFilenames(FileName1,FileName2,ResolveLinks);
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
  function CompareCaret(const FirstCaret, SecondCaret: TPoint): integer;
-------------------------------------------------------------------------------}
function CompareCaret(const FirstCaret, SecondCaret: TPoint): integer;
begin
  if (FirstCaret.Y<SecondCaret.Y) then
    Result:=1
  else if (FirstCaret.Y>SecondCaret.Y) then
    Result:=-1
  else if (FirstCaret.X<SecondCaret.X) then
    Result:=1
  else if (FirstCaret.X>SecondCaret.X) then
    Result:=-1
  else
    Result:=0;
end;

{-------------------------------------------------------------------------------
  procedure CheckList(List: TList; TestListNil, TestDoubles, TestNils: boolean);
-------------------------------------------------------------------------------}
procedure CheckList(List: TList; TestListNil, TestDoubles, TestNils: boolean);
var
  Cnt: Integer;
  i: Integer;
  CurItem: Pointer;
  j: Integer;
begin
  if List=nil then begin
    if TestListNil then
      RaiseException('CheckList List is Nil');
    exit;
  end;
  Cnt:=List.Count;
  if TestNils then begin
    for i:=0 to Cnt-1 do
      if List[i]=nil then
        RaiseException('CheckList item is Nil');
  end;
  if TestDoubles then begin
    for i:=0 to Cnt-2 do begin
      CurItem:=List[i];
      for j:=i+1 to Cnt-1 do begin
        if List[j]=CurItem then
          RaiseException('CheckList Double');
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  procedure CheckEmptyListCut(List1, List2: TList);
-------------------------------------------------------------------------------}
procedure CheckEmptyListCut(List1, List2: TList);
var
  Cnt1: Integer;
  i: Integer;
begin
  if (List1=nil) or (List2=nil) then exit;
  Cnt1:=List1.Count;
  for i:=0 to Cnt1 do begin
    if List2.IndexOf(List1[i])>=0 then
      RaiseException('CheckEmptyListCut');
  end;
end;

{-------------------------------------------------------------------------------
  function CompareBoolean(b1, b2: boolean): integer;
-------------------------------------------------------------------------------}
function CompareBoolean(b1, b2: boolean): integer;
begin
  if b1=b2 then
    Result:=0
  else if b1 then
    Result:=1
  else
    Result:=-1;
end;

function CompareStringPointerI(Data1, Data2: Pointer): integer;
begin
  Result:=AnsiStrIComp(Data1,Data2);
end;

{-------------------------------------------------------------------------------
  function AnsiSearchInStringList(List: TStrings; const s: string): integer;
-------------------------------------------------------------------------------}
function AnsiSearchInStringList(List: TStrings; const s: string): integer;
begin
  Result:=List.Count-1;
  while (Result>=0) and (AnsiCompareText(List[Result],s)<>0) do dec(Result);
end;

{-------------------------------------------------------------------------------
  function TrimSearchPath(const SearchPath, BaseDirectory: string): boolean;
  
  - Removes empty paths.
  - Uses TrimFilename on every path.
  - If BaseDirectory<>'' then every relative Filename will be expanded.
-------------------------------------------------------------------------------}
function TrimSearchPath(const SearchPath, BaseDirectory: string): string;
var
  CurPath: String;
  EndPos: Integer;
  StartPos: Integer;
  l: Integer;
  BaseDir: String;
begin
  Result:='';
  EndPos:=1;
  l:=length(SearchPath);
  BaseDir:=AppendPathDelim(TrimFilename(BaseDirectory));
  while EndPos<=l do begin
    StartPos:=EndPos;
    while (StartPos<=l) and (SearchPath[StartPos]=';') do inc(StartPos);
    if StartPos>l then break;
    EndPos:=StartPos;
    while (EndPos<=l) and (SearchPath[EndPos]<>';') do inc(EndPos);
    CurPath:=copy(SearchPath,StartPos,EndPos-StartPos);
    if CurPath<>'' then begin
      if (BaseDir<>'') and (not FilenameIsAbsolute(CurPath)) then
        CurPath:=BaseDir+CurPath;
      CurPath:=AppendPathDelim(TrimFilename(CurPath));
      if Result<>'' then
        CurPath:=';'+CurPath;
      Result:=Result+CurPath;
    end;
  end;
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
  function ClearFile(const Filename: string; RaiseOnError: boolean): boolean;
-------------------------------------------------------------------------------}
function ClearFile(const Filename: string; RaiseOnError: boolean): boolean;
var
  fs: TFileStream;
begin
  if FileExists(Filename) then begin
    try
      fs:=TFileStream.Create(Filename,fmOpenWrite);
      fs.Size:=0;
      fs.Free;
    except
      on E: Exception do begin
        Result:=false;
        if RaiseOnError then raise;
        exit;
      end;
    end;
  end;
  Result:=true;
end;

function FindProgram(const Programname, BaseDirectory: string;
  WithBaseDirectory: boolean): string;
var
  Flags: TSearchFileInPathFlags;
  SearchPath: string;
  Delim: char;
begin
  if FilenameIsAbsolute(Programname) then begin
    if FileExists(Programname) then
      Result:=Programname
    else
      Result:='';
    exit;
  end;
  Flags:=[];
  if not WithBaseDirectory then
    Include(Flags,sffDontSearchInBasePath);
  GetProgramSearchPath(SearchPath,Delim);
  Result:=FileCtrl.SearchFileInPath(Programname,BaseDirectory,SearchPath,
                                    Delim,Flags);
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
  function SpecialCharsToSpaces(const s: string): string;
-------------------------------------------------------------------------------}
function SpecialCharsToSpaces(const s: string): string;
var
  i: Integer;
begin
  Result:=s;
  for i:=1 to length(Result) do
    if Result[i]<' ' then Result[i]:=' ';
  if Result='' then exit;
  if (Result[1]=' ') or (Result[length(Result)]=' ') then
    Result:=Trim(Result);
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
  Dir: String;
begin
  if CustomLang='' then begin
    GetLanguageIDs(Lang,FallbackLang);
  end else begin
    Lang:=CustomLang;
    FallbackLang:='';
  end;
  Dir:=AppendPathDelim(BaseDirectory);
  // LCL
  TranslateUnitResourceStrings('LclStrConsts',
    Dir+'lcl/languages/lcl.%s.mo',Lang,FallbackLang);
  // IDE without objectinspector
  TranslateUnitResourceStrings('LazarusIDEStrConsts',
    Dir+'languages/lazaruside.%s.mo',Lang,FallbackLang);
  // objectinspector
  TranslateUnitResourceStrings('ObjInspStrConsts',
    Dir+'languages/objinspstrconsts.%s.mo',Lang,FallbackLang);
  // CodeTools
  TranslateUnitResourceStrings('CodeToolsStrConsts',
    Dir+'components/codetools/languages/codetools.%s.mo',Lang,FallbackLang);
  // SynEdit
  TranslateUnitResourceStrings('SynEditStrConst',
    Dir+'components/synedit/languages/synedit.%s.mo',Lang,FallbackLang);
  // SynMacroRecorder
  TranslateUnitResourceStrings('SynMacroRecorder',
    Dir+'components/synedit/languages/synmacrorecorder.%s.mo',Lang,FallbackLang);
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
  function SplitString(const s: string; Delimiter: char): TStringList;
-------------------------------------------------------------------------------}
function SplitString(const s: string; Delimiter: char): TStringList;
var
  SLen: Integer;
  StartPos: Integer;
  EndPos: Integer;
begin
  Result:=TStringList.Create;
  SLen:=length(s);
  StartPos:=1;
  EndPos:=1;
  repeat
    if (EndPos<=sLen) and (s[EndPos]<>Delimiter) then
      inc(EndPos)
    else begin
      if EndPos>StartPos then
        Result.Add(copy(s,StartPos,EndPos-StartPos));
      StartPos:=EndPos+1;
      if StartPos>sLen then exit;
      inc(EndPos);
    end;
  until false;
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

  If there is a space in the option add " " around the whole option
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

procedure GetProgramSearchPath(var SearchPath: string; var Delim: char);
begin
  SearchPath:=GetEnv('PATH');
  Delim:=':';
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

function ProgramDirectory: string;
begin
  Result:=FileCtrl.ProgramDirectory;
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
        // in front of each SpecialChar will be a PrefixChar inserted
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

{FindMatchingTextFiles Adds filenames that match a mask to a user supplied
string list. Procedure will optionally search subdirectories.

FileList: TStringList  //List to put the matching file names.
TheDirectory: String   //Directory to start search
Mask: String           //Search mask,
                       //multiple mask seperater by ; ie '*.pas;*.pp;...'
Recursive: boolean     //search subdirectories?
}
procedure FindMatchingTextFiles(FileList: TStringList; TheDirectory: string;
                                mask: string; recursive: boolean);
var
  //List of File masks to use in the search.
  MaskList: TStringList;
  //Loop counter
  i:        integer;
  //Result of FindFirst, FindNext
  FileInfo: TSearchRec;
  //Temp Storage for The search Directoru
  TempDir: string;

  {Function GetMasks: TStringList
  returns a list of mask from a string seperater by ;}
  function GetMasks: TStringList;
  var
    //Position Tracking wihtin the string.
    curpos,startpos: integer;
    //Used as mask seperator
  const
    MaskSeperator = ';';

  begin
    Result:= TStringList.Create;
    if mask<>'' then
    begin
      //do we have multiple masks
      if (pos(MaskSeperator,Mask)>0) then
      begin
        startpos:=1;
        curpos:=1;
        repeat //loop through the string and get the masks.
          while (curpos<=length(mask)) and (mask[curpos] <> MaskSeperator) do
            inc(curpos);
          //add the mask to the list
          Result.Add(copy(mask,startpos,curpos-startpos));
          inc(curpos);//skip the seperator
          startpos:= curpos;//start on next mask
        until curpos > length(mask);
      end//if
      else
      begin
        result.Add(mask);
      end;//else
    end//if
    else
    begin
      Result.Add(FindMask) //OS Independent Mask
    end;//else
  end;//GetMasks

begin
  //if we have a list and a valid directory
  if ((FileList<>(nil)) and (DirectoryExists(TheDirectory))) then
  begin //make sure path ends with delimiter
    TempDir:= AppendPathDelim(TheDirectory);
    try
      MaskList:= GetMasks;//Returns a list of file masks.
      for i:= 0 to MaskList.Count -1 do
      begin
        try
          if SysUtils.FindFirst(TempDir + MaskList[i],
                                faAnyFile,FileInfo)=0 then
          begin
            repeat
              // check if special file, skip directories this time
              if (FileInfo.Name='.') or (FileInfo.Name='..')
              or ((faDirectory and FileInfo.Attr)>0) then continue;
              //Make sure this is a text file as we will be search
              if (FileIsText(TempDir + FileInfo.Name))and
             (FileIsReadable(TempDir + FileInfo.Name)) then
              begin
                FileList.Add(TempDir + FileInfo.Name);
              end;//if
            until SysUtils.FindNext(FileInfo)<>0;
          end;//if
        finally
          SysUtils.FindClose(FileInfo);
        end;//try-finally
      end;//for
    finally
      MaskList.Free;
    end;//try-finally
    //If selected then Look for and search subdirectories
    if (recursive) and (SysUtils.FindFirst(TempDir
                        +FindMask,faAnyFile,FileInfo)=0) then
    begin
      if ((faDirectory and FileInfo.Attr)>0) then
      begin
        repeat
          // check if special file
          if (FileInfo.Name='.') or (FileInfo.Name='..') then continue;
          FindMatchingTextFiles
            (FileList,TempDir + FileInfo.Name,mask,recursive);
        until SysUtils.FindNext(FileInfo)<>0;
      end;//if
    end;//if
  end;//if
end;//FindMatchingFiles

{FindInFiles Search for the first occurence of a string in a text file, returns
a list of files with a match.
TheFileList: TStringList   List of files to be searched.
SearchFor: String          The string to search for.
WholeWord: Boolean         Search for whole word matches.
CaseSensitive: Boolean     Case sensitive search.
RegExp: Boolean            SearchFor is to be treated as a regular expression
}
function FindInFiles(TheFileList: TStringList; Searchfor: String;
                     WholeWord: Boolean; CaseSensitive: Boolean;
                     RegExp: Boolean): TStringList;
var
  ThisFile: TStringList; //The File being searched
  i:        integer;     //Loop Counter
  Lines:    integer;     //Loop Counter
  Match:    integer;     //Position of match in line.
  StartWord:boolean;     //Does the word start with a sperator charater?
  EndWord:  boolean;     //Does the word end with a seperator charater?
  TheLine:  string;      //Temp Storage for the current line in the file.
  TempSearch: string;    //Temp Storage for the search string.

  const
  WordBreakChars = ['.', ',', ';', ':', '"', '''', '!', '?', '[', ']', '(',
                ')', '{', '}', '^', '-', '=', '+', '*', '/', '\', '|', ' '];
begin
  Result:= TStringList.Create;
  try
    ThisFile:= TStringList.Create;
    if (Not CaseSensitive) and (not RegExp) then
      TempSearch:= UpperCase(SearchFor)
    else
      TempSearch:= SearchFor;
    for i:= 0 to TheFileList.Count -1 do
    begin
      ThisFile.LoadFromFile(TheFileList.Strings[i]);
      for Lines:= 0 to ThisFile.Count -1 do
      begin
        TheLine:= ThisFile.Strings[Lines];
        if not CaseSensitive then
          TheLine:= UpperCase(TheLine);
        Match:= pos(TempSearch,TheLine);
        //look at the char before and after the match to see if they are in
        //our list of word seperator charaters.
        if WholeWord and (Match > 0) then
        begin //is this the first word on the line or does the word start with
              //one of the word seperator charaters.
          if (Match = 1) or (TheLine[Match-1] in WordBreakChars) then
            StartWord := True
          else
            StartWord := False;
          if StartWord then // evaluate end only if start is true.
          begin
            if (Match + length(TempSearch) >= length(TheLine)) or
                (TheLine[Match + Length(TempSearch)] in WordBreakChars) then
              EndWord:= True
            else
              EndWord:= False;
          end;//if
          if StartWord And EndWord then
          begin
            Result.Add(TheFileList.Strings[i]+'('+IntToStr(lines+1)+
                       ','+ IntToStr(match) + ')'+' '+'None:'+' '+SearchFor);
            break;//junp out we found our match
          end;//if
        end;//if
        if not WholeWord and (Match > 0) then
        begin
          Result.Add(TheFileList.Strings[i]+'('+IntToStr(lines+1)+
                     ','+IntToStr(match)+')'+' '+'None:'+' '+SearchFor);
          break;//junp out we found our match
        end;//if
      end;//for
    end;//for
  finally
    ThisFile.Free;
  end;//Try-finally
end;//FindInFiles

end.

