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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
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
  // RTL
  Classes, SysUtils, Laz_AVL_Tree,
  // LazUtils
  FileUtil, LazFileUtils, LazUtilities, LazFileCache, LazUTF8, LazUTF8Classes,
  Laz2_XMLCfg, AvgLvlTree,
  // LCL
  StdCtrls, ExtCtrls,
  // CodeTools
  BasicCodeTools, SourceLog, FileProcs, CodeToolManager, CodeToolsConfig, CodeCache,
  // IDE
  IDECmdLine, LazConf;

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
      
// file operations
function BackupFileForWrite(const Filename, BackupFilename: string): boolean;
function ClearFile(const Filename: string; RaiseOnError: boolean): boolean;
function CreateEmptyFile(const Filename: string): boolean;
function CopyFileWithMethods(const SrcFilename, DestFilename: string;
             {%H-}OnCopyError: TOnCopyErrorMethod; {%H-}Data: TObject): boolean;
function CopyDirectoryWithMethods(const SrcDirectory, DestDirectory: string;
             OnCopyFile: TOnCopyFileMethod; OnCopyError: TOnCopyErrorMethod;
             Data: TObject): boolean;

// file names
function ConvertSpecialFileChars(const Filename: string): string;
function FilenameIsPascalSource(const Filename: string): boolean;
function FilenameIsFormText(const Filename: string): boolean;
function ChompEndNumber(const s: string): string;
function ShortDisplayFilename(const aFileName: string): string;

// cmd line
procedure SplitCmdLine(const CmdLine: string;
                       out ProgramFilename, Params: string);
function PrepareCmdLineOption(const Option: string): string;
function AddCmdLineParameter(const CmdLine, AddParameter: string): string;

// find file
function FindFilesCaseInsensitive(const Directory,
  CaseInsensitiveFilename: string; IgnoreExact: boolean): TStringList;
function FindFirstFileWithExt(const Directory, Ext: string): string;
function FindShortFileNameOnDisk(const Filename: string): string;
function CreateNonExistingFilename(const BaseFilename: string): string;
function FindFPCTool(const Executable, CompilerFilename: string): string;
procedure ResolveLinksInFileList(List: TStrings; RemoveDanglingLinks: Boolean);
function FindProgram(ProgramName, BaseDirectory: string;
                     WithBaseDirectory: boolean): string;

// search paths
function TrimSearchPath(const SearchPath, BaseDirectory: string;
                  DeleteDoubles: boolean = false; ExpandPaths: boolean = false): string;
function MergeSearchPaths(const OldSearchPath, AddSearchPath: string): string;
procedure MergeSearchPaths(SearchPath: TStrings; const AddSearchPath: string);
function RemoveSearchPaths(const SearchPath, RemoveSearchPath: string): string;
function RemoveNonExistingPaths(const SearchPath, BaseDirectory: string): string;
function RebaseSearchPath(const SearchPath,
                          OldBaseDirectory, NewBaseDirectory: string;
                          SkipPathsStartingWithMacro: boolean): string;
function ShortenSearchPath(const SearchPath, BaseDirectory,
                           ChompDirectory: string): string;
function GetNextDirectoryInSearchPath(const SearchPath: string;
                                      var NextStartPos: integer): string;
function GetNextUsedDirectoryInSearchPath(const SearchPath,
                          FilterDir: string; var NextStartPos: integer): string;
function SearchPathToList(const SearchPath: string): TStringList;
function SearchDirectoryInSearchPath(const SearchPath, Directory: string;
                                     DirStartPos: integer = 1): integer;
function SearchDirectoryInSearchPath(SearchPath: TStrings;
                    const Directory: string; DirStartPos: integer = 0): integer;

// Recent item lists
type
  TRecentListType = (
    rltCaseSensitive,
    rltCaseInsensitive,
    rltFile
    );
const
  RecentListTypeNames: array[TRecentListType] of string = (
    'CaseSensitive',
    'CaseInsensitive',
    'File'
    );
function IndexInRecentList(List: TStrings; ListType: TRecentListType;
  const Path: string): integer;
function StrToRecentListType(s: string): TRecentListType;
function CompareRecentListItem(s1, s2: string; ListType: TRecentListType): boolean;
procedure LoadRecentList(XMLConfig: TXMLConfig; List: TStrings; const Path: string;
                         ListType: TRecentListType);
procedure SaveRecentList(XMLConfig: TXMLConfig; List: TStrings;
                         const Path: string); overload;
procedure SaveRecentList(XMLConfig: TXMLConfig; List: TStrings;
                         const Path: string; aMax: Integer); overload;
function AddToRecentList(const s: string; List: TStrings; aMax: integer;
                         ListType: TRecentListType): boolean;
function AddComboTextToRecentList(cb: TCombobox; aMax: integer;
                                  ListType: TRecentListType): boolean;
procedure RemoveFromRecentList(const s: string; List: TStrings;
                               ListType: TRecentListType);
procedure CleanUpRecentList(List: TStrings; ListType: TRecentListType);

// XMLconfig
procedure LoadRect(XMLConfig: TXMLConfig; const Path:string;
                   var ARect:TRect);
procedure LoadRect(XMLConfig: TXMLConfig; const Path:string;
                   var ARect:TRect; const DefaultRect: TRect);
procedure SaveRect(XMLConfig: TXMLConfig; const Path:string;
                   const ARect: TRect);
procedure SaveRect(XMLConfig: TXMLConfig; const Path:string;
                   const ARect, DefaultRect: TRect);
procedure LoadPoint(XMLConfig: TXMLConfig; const Path:string;
                    var APoint:TPoint; const DefaultPoint: TPoint);
procedure SavePoint(XMLConfig: TXMLConfig; const Path:string;
                    const APoint, DefaultPoint:TPoint);
procedure LoadStringList(XMLConfig: TXMLConfig; List: TStrings; const Path: string);
procedure SaveStringList(XMLConfig: TXMLConfig; List: TStrings; const Path: string);
procedure LoadStringToStringTree(XMLConfig: TXMLConfig;
                                 Tree: TStringToStringTree; const Path: string);
procedure SaveStringToStringTree(XMLConfig: TXMLConfig;
                                 Tree: TStringToStringTree; const Path: string);
procedure MakeXMLName(var Name: string);
function LoadXMLConfigViaCodeBuffer(Filename: string): TXMLConfig;

// Point conversion
function PointToCfgStr(const Point: TPoint): string;
procedure CfgStrToPoint(const s: string; var Point: TPoint;
                        const DefaultPoint: TPoint);

// text conversion
function TabsToSpaces(const s: string; TabWidth: integer; UseUTF8: boolean
                      ): string;
function CommentLines(const s: string): string;
function CommentText(const s: string; CommentType: TCommentType): string;
function UncommentLines(const s: string): string;
function CrossReplaceChars(const Src: string; PrefixChar: char;
                           const SpecialChars: string): string;
function SimpleSyntaxToRegExpr(const Src: string): string;
function BinaryStrToText(const s: string): string;
function SplitString(const s: string; Delimiter: char): TStrings;
procedure SplitString(const s: string; Delimiter: char; AddTo: TStrings;
                      ClearList: boolean = true);
function SpecialCharsToSpaces(const s: string; FixUTF8: boolean): string;
function SpecialCharsToHex(const s: string): string;
function LineBreaksToSystemLineBreaks(const s: string): string;
function LineBreaksToDelimiter(const s: string; Delimiter: char): string;
function StringListToText(List: TStrings; const Delimiter: string;
                          IgnoreEmptyLines: boolean = false): string;
function StringListPartToText(List: TStrings; FromIndex, ToIndex: integer;
                              const Delimiter: string;
                              IgnoreEmptyLines: boolean = false): string;
function StringListToString(List: TStrings; FromIndex, ToIndex: integer;
                            IgnoreEmptyLines: boolean = false): string;
procedure StringToStringList(const s: string; List: TStrings);

// environment
type
  TParseString = record
    UnparsedValue: string;
    ParsedValue: string;
    ParseStamp: integer;
    Parsing: boolean;
  end;

function GetCurrentUserName: string;
function GetCurrentMailAddress: string;
function GetProgramSearchPath: string;

// debugging
procedure RaiseException(const Msg: string);

// miscellaneous
procedure CheckList(List: TList; TestListNil, TestDoubles, TestNils: boolean);
procedure CheckList(List: TFPList; TestListNil, TestDoubles, TestNils: boolean);
procedure CheckEmptyListCut(List1, List2: TList);
procedure RemoveDoubles(List: TStrings);
function SearchInStringListI(List: TStrings; const s: string): integer; // search ASCII case insensitive, not UTF-8
procedure ReverseList(List: TList);
procedure ReverseList(List: TFPList);
procedure FreeListObjects(List: TList; FreeList: boolean);
procedure FreeListObjects(List: TFPList; FreeList: boolean);
function CompareMemStreamText(s1, s2: TMemoryStream): Boolean;

function CheckGroupItemChecked(CheckGroup: TCheckGroup; const Caption: string): Boolean;


implementation

{$IfNdef MSWindows}
{$ifNdef HASAMIGA}
// to get more detailed error messages consider the os
uses
  Unix, BaseUnix;
{$EndIf}
{$EndIf}

{-------------------------------------------------------------------------------
  function FindFilesCaseInsensitive(const Directory,
    CaseInsensitiveFilename: string; IgnoreExact: boolean): TStringLists;

  Search Pascal case insensitive in Directory for all files
  named CaseInsensitiveFilename
-------------------------------------------------------------------------------}
function FindFilesCaseInsensitive(const Directory,
  CaseInsensitiveFilename: string; IgnoreExact: boolean): TStringList;
var
  FileInfo: TSearchRec;
begin
  Result:=nil;
  if FindFirstUTF8(AppendPathDelim(Directory)+GetAllFilesMask,
                        faAnyFile,FileInfo)=0
  then begin
    repeat
      // check if special file
      if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='') then
        continue;
      if (CompareText(CaseInsensitiveFilename,FileInfo.Name)=0) // Pascal insensitibity, not UTF-8, thing about Turkish I
      and ((not IgnoreExact)
           or (CompareFilenames(CaseInsensitiveFilename,FileInfo.Name)<>0))
      then begin
        if Result=nil then Result:=TStringList.Create;
        Result.Add(FileInfo.Name);
      end;
    until FindNextUTF8(FileInfo)<>0;
  end;
  FindCloseUTF8(FileInfo);
end;

function FilenameIsPascalSource(const Filename: string): boolean;
var
  s: string;
  i: Integer;
begin
  Result:=False;
  // Check unit name
  s:=ExtractFileNameOnly(Filename);
  if (s='') or not IsDottedIdentifier(s) then
    exit;
  // Check extension
  s:=lowercase(ExtractFileExt(Filename));
  for i:=Low(PascalSourceExt) to High(PascalSourceExt) do
    if s=PascalSourceExt[i] then
      exit(True);
end;

function FilenameIsFormText(const Filename: string): boolean;
var
  Ext: string;
begin
  Ext:=lowercase(ExtractFileExt(Filename));
  Result:=((Ext='.lfm') or (Ext='.dfm') or (Ext='.xfm'))
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
  if FindFirstUTF8(AppendPathDelim(ADirectory)+GetAllFilesMask,
                        faAnyFile,FileInfo)=0
  then begin
    ShortFilename:=ExtractFilename(Filename);
    repeat
      // check if special file
      if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='') then
        continue;
      if CompareFilenames(ShortFilename,FileInfo.Name)=0 then begin
        Result:=FileInfo.Name;
        break;
      end;
    until FindNextUTF8(FileInfo)<>0;
  end;
  FindCloseUTF8(FileInfo);
end;

function CreateNonExistingFilename(const BaseFilename: string): string;
var
  PostFix: String;
  PreFix: String;
  i: Integer;
begin
  if not FileExistsUTF8(BaseFilename) then begin
    Result:=BaseFilename;
    exit;
  end;
  PostFix:=ExtractFileExt(BaseFilename);
  PreFix:=copy(BaseFilename,1,length(BaseFilename)-length(PostFix));
  i:=0;
  repeat
    inc(i);
    Result:=PreFix+IntToStr(i)+PostFix;
  until not FileExistsUTF8(Result);
end;

function FindFPCTool(const Executable, CompilerFilename: string): string;
begin
  if ConsoleVerbosity>=0 then
    DebugLn('Hint: (lazarus) FindFPCTool Executable="',Executable,'" CompilerFilename="',CompilerFilename,'"');
  Result:=FindDefaultExecutablePath(Executable);
  if Result<>'' then exit;
  Result:=AppendPathDelim(ExtractFilePath(CompilerFilename))+Executable;
  if ConsoleVerbosity>=0 then
    DebugLn('Hint: (lazarus) FindFPCTool Try="',Result);
  if FileExistsUTF8(Result) then exit;
  Result:='';
end;

procedure ResolveLinksInFileList(List: TStrings; RemoveDanglingLinks: Boolean);
var
  i: Integer;
  OldFilename: string;
  NewFilename: String;
begin
  if List=nil then exit;
  for i:=List.Count-1 downto 0 do begin
    OldFilename:=List[i];
    NewFilename:=GetPhysicalFilenameCached(OldFilename,true);
    //DebugLn(['ResolveLinksInFileList OldFilename=',OldFilename,' NewFilename=',NewFilename]);
    if NewFilename='' then begin
      if RemoveDanglingLinks then
        List.Delete(i);
    end
    else if NewFilename<>OldFilename then
      List[i]:=NewFilename;
  end;
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

procedure MergeSearchPaths(SearchPath: TStrings; const AddSearchPath: string);
var
  l: Integer;
  EndPos: Integer;
  StartPos: Integer;
begin
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
    if SearchDirectoryInSearchPath(SearchPath,AddSearchPath,StartPos)<1 then
    begin
      // new path found -> add
      SearchPath.Add(copy(AddSearchPath,StartPos,EndPos-StartPos));
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
    //DebugLn('RemoveSearchPaths Dir="',copy(SearchPath,StartPos,EndPos-StartPos),'" RemoveSearchPath="',RemoveSearchPath,'"');
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

function RebaseSearchPath(const SearchPath, OldBaseDirectory,
  NewBaseDirectory: string; SkipPathsStartingWithMacro: boolean): string;
// change every relative search path
var
  EndPos: Integer;
  StartPos: Integer;
  CurPath: String;
begin
  Result:=SearchPath;
  if CompareFilenames(OldBaseDirectory,NewBaseDirectory)=0 then exit;
  EndPos:=1;
  repeat
    StartPos:=EndPos;
    while (StartPos<=length(Result)) and (Result[StartPos]=';') do
      inc(StartPos);
    if StartPos>length(Result) then break;
    EndPos:=StartPos;
    while (EndPos<=length(Result)) and (Result[EndPos]<>';') do
      inc(EndPos);
    if EndPos>StartPos then begin
      CurPath:=copy(Result,StartPos,EndPos-StartPos);
      if (not FilenameIsAbsolute(CurPath))
      and ((not SkipPathsStartingWithMacro) or (CurPath[1]<>'$'))
      then begin
        CurPath:=TrimFilename(AppendPathDelim(OldBaseDirectory)+CurPath);
        CurPath:=CreateRelativePath(CurPath,NewBaseDirectory);
        Result:=copy(Result,1,StartPos-1)+CurPath
                   +copy(Result,EndPos,length(Result));
        EndPos:=StartPos+length(CurPath);
      end;
    end;
  until false;
end;

function ShortenSearchPath(const SearchPath, BaseDirectory,
  ChompDirectory: string): string;
// Every search path that is a subdirectory of ChompDirectory will be shortened.
// Before the test relative paths are expanded by BaseDirectory.
var
  BaseEqualsChompDir: boolean;

  function Normalize(var ADirectory: string): boolean;
  begin
    if FilenameIsAbsolute(ADirectory) then begin
      Result:=true;
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
    while (Result[StartPos] in [';',#0..#32]) do begin
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
  if PathLen>0 then begin
    repeat
      while (NextStartPos<=PathLen)
      and (SearchPath[NextStartPos] in [';',#0..#32]) do
        inc(NextStartPos);
      CurStartPos:=NextStartPos;
      while (NextStartPos<=PathLen) and (SearchPath[NextStartPos]<>';') do
        inc(NextStartPos);
      Result:=TrimFilename(copy(SearchPath,CurStartPos,NextStartPos-CurStartPos));
      if Result<>'' then exit;
    until (NextStartPos>PathLen);
  end else begin
    NextStartPos:=1;
  end;
  Result:='';
end;

function GetNextUsedDirectoryInSearchPath(const SearchPath,
                    FilterDir: string; var NextStartPos: integer): string;
// searches next directory in search path,
// which is equal to FilterDir or is in FilterDir
begin
  while (NextStartPos<=length(SearchPath)) do begin
    Result:=GetNextDirectoryInSearchPath(SearchPath,NextStartPos);
    if (Result<>'')
    and ((CompareFilenames(Result,FilterDir)=0)
      or FileIsInPath(Result,FilterDir))
    then
      exit;
  end;
  Result:=''
end;

function SearchPathToList(const SearchPath: string): TStringList;
var
  p: Integer;
  CurDir: String;
begin
  Result:=TStringList.Create;
  p:=1;
  repeat
    CurDir:=GetNextDirectoryInSearchPath(SearchPath,p);
    if CurDir='' then break;
    Result.Add(CurDir);
  until false;
end;

function SearchDirectoryInSearchPath(const SearchPath, Directory: string;
  DirStartPos: integer): integer;
// -1 on not found
var
  PathLen: Integer;
  DirLen: Integer;
  EndPos: Integer;
  StartPos: Integer;
  DirEndPos: Integer;
  CurDirLen: Integer;
  CurDirEndPos: Integer;
begin
  Result:=-1;
  DirLen:=length(Directory);
  if (SearchPath='')
  or (Directory='') or (DirStartPos>DirLen) or (Directory[DirStartPos]=';') then
    exit;
  DirEndPos:=DirStartPos;
  while (DirEndPos<=DirLen) and (Directory[DirEndPos]<>';') do inc(DirEndPos);
  // ignore PathDelim at end
  if (DirEndPos>DirStartPos) and (Directory[DirEndPos-1]=PathDelim) then begin
    while (DirEndPos>DirStartPos) and (Directory[DirEndPos-1]=PathDelim) do
      dec(DirEndPos);
    // check if it is the root path '/'
    if DirEndPos=DirStartPos then DirEndPos:=DirStartPos+1;
  end;
  CurDirLen:=DirEndPos-DirStartPos;
  //DebugLn('SearchDirectoryInSearchPath Dir="',copy(Directory,DirStartPos,CurDirLen),'"');
  PathLen:=length(SearchPath);
  EndPos:=1;
  while EndPos<=PathLen do begin
    StartPos:=EndPos;
    while (SearchPath[StartPos] in [';',#0..#32]) do begin
      inc(StartPos);
      if StartPos>PathLen then exit;
    end;
    EndPos:=StartPos;
    while (EndPos<=PathLen) and (SearchPath[EndPos]<>';') do inc(EndPos);
    CurDirEndPos:=EndPos;
    // ignore PathDelim at end
    if (CurDirEndPos>StartPos) and (SearchPath[CurDirEndPos-1]=PathDelim) then
    begin
      while (CurDirEndPos>StartPos) and (SearchPath[CurDirEndPos-1]=PathDelim)
      do
        dec(CurDirEndPos);
      // check if it is the root path '/'
      if CurDirEndPos=StartPos then CurDirEndPos:=StartPos+1;
    end;
    //DebugLn('SearchDirectoryInSearchPath CurDir="',copy(SearchPath,StartPos,CurDirEndPos-StartPos),'"');
    if CurDirEndPos-StartPos=CurDirLen then begin
      // directories have same length -> compare chars
      if FileUtil.CompareFilenames(@SearchPath[StartPos],CurDirLen,
                          @Directory[DirStartPos],CurDirLen,
                          false)=0
      then begin
        // directory found
        Result:=StartPos;
        exit;
      end;
    end;
    StartPos:=EndPos;
  end;
end;

function SearchDirectoryInSearchPath(SearchPath: TStrings;
  const Directory: string; DirStartPos: integer): integer;
var
  DirLen: Integer;
  DirEndPos: Integer;
  CurDirLen: Integer;
  CurPath: string;
  CurPathLen: Integer;
begin
  Result:=-1;
  DirLen:=length(Directory);
  if (SearchPath.Count=0)
  or (Directory='') or (DirStartPos>DirLen) or (Directory[DirStartPos]=';') then
    exit;
  DirEndPos:=DirStartPos;
  while (DirEndPos<=DirLen) and (Directory[DirEndPos]<>';') do inc(DirEndPos);
  // ignore PathDelim at end
  if (DirEndPos>DirStartPos) and (Directory[DirEndPos-1]=PathDelim) then begin
    while (DirEndPos>DirStartPos) and (Directory[DirEndPos-1]=PathDelim) do
      dec(DirEndPos);
    // check if it is the root path '/'
    if DirEndPos=DirStartPos then DirEndPos:=DirStartPos+1;
  end;
  CurDirLen:=DirEndPos-DirStartPos;
  
  // search in all search paths
  Result:=SearchPath.Count-1;
  while Result>=0 do begin
    CurPath:=SearchPath[Result];
    CurPathLen:=length(CurPath);
    if CurPathLen>0 then
    begin
      while (CurPathLen>1) and (CurPath[CurPathLen]=PathDelim) do dec(CurPathLen);
    end;
    if (CurPathLen>0)
    and (FileUtil.CompareFilenames(@CurPath[1],CurPathLen,
                                   @Directory[DirStartPos],CurDirLen,
                                   false)=0)
    then begin
      // directory found
      exit;
    end;
    dec(Result);
  end;
end;

function RemoveNonExistingPaths(const SearchPath, BaseDirectory: string): string;
var
  StartPos: Integer;
  EndPos: LongInt;
  CurPath: String;
  MacroStartPos: LongInt;
begin
  Result:=SearchPath;
  StartPos:=1;
  while StartPos<=length(Result) do begin
    EndPos:=StartPos;
    while (EndPos<=length(Result)) and (Result[EndPos]=';') do inc(EndPos);
    if EndPos>StartPos then begin
      // empty paths, e.g. ;;;;
      // remove
      Result:=copy(Result,1,StartPos-1)+copy(Result,EndPos,length(Result));
      EndPos:=StartPos;
    end;
    while (EndPos<=length(Result)) and (Result[EndPos]<>';') do inc(EndPos);
    
    CurPath:=copy(Result,StartPos,EndPos-StartPos);

    // cut macros
    MacroStartPos:=System.Pos('$(',CurPath);
    if MacroStartPos>0 then begin
      CurPath:=copy(CurPath,1,MacroStartPos-1);
      if (CurPath<>'') and (CurPath[length(CurPath)]<>PathDelim) then
        CurPath:=ExtractFilePath(CurPath);
    end;

    // make path absolute
    if (CurPath<>'') and (not FilenameIsAbsolute(CurPath)) then
      CurPath:=AppendPathDelim(BaseDirectory)+CurPath;

    if ((CurPath='') and (MacroStartPos<1))
    or (not DirPathExistsCached(CurPath)) then begin
      // path does not exist -> remove
      Result:=copy(Result,1,StartPos-1)+copy(Result,EndPos+1,length(Result));
      EndPos:=StartPos;
    end else begin
      StartPos:=EndPos+1;
    end;
  end;
end;

function ChompEndNumber(const s: string): string;
var
  NewLen: Integer;
begin
  Result:=s;
  NewLen:=length(Result);
  while (NewLen>0) and (Result[NewLen] in ['0'..'9']) do
    dec(NewLen);
  Result:=copy(Result,1,NewLen);
end;

function ShortDisplayFilename(const aFileName: string): string;
// Shorten a long filename for display.
// Add '...' after the 2. path delimiter, then the end part of filename.
const
  Limit = 80;
var
  StartLen, EndLen, SepCnt: Integer;
begin
  if Length(aFileName) > Limit then
  begin
    StartLen := 1;
    SepCnt := 0;
    while StartLen < Length(aFileName) - (Limit div 2) do
    begin
      if aFileName[StartLen] in AllowDirectorySeparators then
      begin
        Inc(SepCnt);
        if SepCnt = 2 then Break;
      end;
      Inc(StartLen);
    end;
    EndLen := Limit - StartLen - 3;
    Result := Copy(aFileName, 1, StartLen) + '...'
            + Copy(aFileName, Length(aFileName)-EndLen+1, EndLen);
  end
  else
    Result := aFileName;
end;

function FindFirstFileWithExt(const Directory, Ext: string): string;
var
  FileInfo: TSearchRec;
begin
  Result:='';
  if FindFirstUTF8(AppendPathDelim(Directory)+GetAllFilesMask,
                        faAnyFile,FileInfo)=0
  then begin
    repeat
      // check if special file
      if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='') then
        continue;
      // check extension
      if CompareFileExt(FileInfo.Name,Ext,false)=0 then begin
        Result:=AppendPathDelim(Directory)+FileInfo.Name;
        break;
      end;
    until FindNextUTF8(FileInfo)<>0;
  end;
  FindCloseUTF8(FileInfo);
end;

// Recent item lists :

function IndexInRecentList(List: TStrings; ListType: TRecentListType;
  const Path: string): integer;
begin
  Result:=List.Count-1;
  while (Result>=0) and (not CompareRecentListItem(List[Result],Path,ListType)) do
    dec(Result);
end;

function StrToRecentListType(s: string): TRecentListType;
begin
  for Result:=Low(TRecentListType) to high(TRecentListType)  do
    if SysUtils.CompareText(s,RecentListTypeNames[Result])=0 then exit;
  Result:=rltCaseSensitive;
end;

function CompareRecentListItem(s1, s2: string; ListType: TRecentListType): boolean;
begin
  case ListType of
  rltCaseInsensitive: Result:=UTF8LowerCase(s1)=UTF8LowerCase(s2);
  rltFile: Result:=CompareFilenames(ChompPathDelim(s1),ChompPathDelim(s2))=0;
  else Result:=s1=s2;
  end;
end;

procedure LoadRecentList(XMLConfig: TXMLConfig; List: TStrings;
  const Path: string; ListType: TRecentListType);
begin
  LoadStringList(XMLConfig,List,Path);
  CleanUpRecentList(List,ListType);
end;

procedure SaveRecentList(XMLConfig: TXMLConfig; List: TStrings; const Path: string);
begin
  SaveStringList(XMLConfig,List,Path);
end;

procedure SaveRecentList(XMLConfig: TXMLConfig; List: TStrings;
  const Path: string; aMax: Integer);
var
  i: Integer;
  s: String;
begin
  if aMax>0 then
    while List.Count>aMax do    // Truncate list to aMax items.
      List.Delete(List.Count-1);
  SaveStringList(XMLConfig,List,Path);
  i:=List.Count+1;
  while True do
  begin
    s:=Path+'Item'+IntToStr(i);
    if not XMLConfig.HasPath(s+'/Value',True) then Break;
    XMLConfig.DeletePath(s);    // Remove excess items from XML.
    Inc(i);
  end;
end;

function AddToRecentList(const s: string; List: TStrings; aMax: integer;
  ListType: TRecentListType): boolean;
begin
  if (List.Count>0) and CompareRecentListItem(List[0],s,ListType) then
    exit(false);
  Result:=true;
  RemoveFromRecentList(s,List,ListType);
  List.Insert(0,s);
  if aMax>0 then
    while List.Count>aMax do
      List.Delete(List.Count-1);
end;

function AddComboTextToRecentList(cb: TCombobox; aMax: integer;
  ListType: TRecentListType): boolean;
var
  List: TStringList;
begin
  List:=TStringList.Create;
  try
    List.Assign(cb.Items);
    Result:=AddToRecentList(cb.Text,List,aMax,ListType);
    if Result then
    begin
      cb.Items.Assign(List);
      cb.ItemIndex:=0;
    end;
  finally
    List.Free;
  end;
end;

procedure RemoveFromRecentList(const s: string; List: TStrings;
  ListType: TRecentListType);
var
  i: integer;
begin
  for i:=List.Count-1 downto 0 do
    if CompareRecentListItem(List[i],s,ListType) then
      List.Delete(i);
end;

procedure CleanUpRecentList(List: TStrings; ListType: TRecentListType);
var
  i: Integer;
begin
  for i:=List.Count-1 downto 1 do
    if (List[i]='') or CompareRecentListItem(List[i],List[i-1],ListType) then
      List.Delete(i);
end;

// XMLConfig

procedure LoadStringList(XMLConfig: TXMLConfig; List: TStrings; const Path: string);
var
  i,Count: integer;
  s: string;
begin
  Count:=XMLConfig.GetValue(Path+'Count',0);
  List.Clear;
  for i:=1 to Count do begin
    s:=XMLConfig.GetValue(Path+'Item'+IntToStr(i)+'/Value','');
    if s<>'' then List.Add(s);
  end;
end;

procedure SaveStringList(XMLConfig: TXMLConfig; List: TStrings; const Path: string);
var
  i: integer;
begin
  XMLConfig.SetDeleteValue(Path+'Count',List.Count,0);
  for i:=0 to List.Count-1 do
    XMLConfig.SetDeleteValue(Path+'Item'+IntToStr(i+1)+'/Value',List[i],'');
end;

procedure LoadStringToStringTree(XMLConfig: TXMLConfig;
  Tree: TStringToStringTree; const Path: string);
var
  Cnt: LongInt;
  SubPath: String;
  CurName: String;
  CurValue: String;
  i: Integer;
begin
  Tree.Clear;
  Cnt:=XMLConfig.GetValue(Path+'Count',0);
  for i:=0 to Cnt-1 do begin
    SubPath:=Path+'Item'+IntToStr(i)+'/';
    CurName:=XMLConfig.GetValue(SubPath+'Name','');
    CurValue:=XMLConfig.GetValue(SubPath+'Value','');
    Tree.Values[CurName]:=CurValue;
  end;
end;

procedure SaveStringToStringTree(XMLConfig: TXMLConfig;
  Tree: TStringToStringTree; const Path: string);
var
  Node: TAvlTreeNode;
  Item: PStringToStringItem;
  i: Integer;
  SubPath: String;
begin
  XMLConfig.SetDeleteValue(Path+'Count',Tree.Tree.Count,0);
  Node:=Tree.Tree.FindLowest;
  i:=0;
  while Node<>nil do begin
    Item:=PStringToStringItem(Node.Data);
    SubPath:=Path+'Item'+IntToStr(i)+'/';
    XMLConfig.SetDeleteValue(SubPath+'Name',Item^.Name,'');
    XMLConfig.SetDeleteValue(SubPath+'Value',Item^.Value,'');
    Node:=Tree.Tree.FindSuccessor(Node);
    inc(i);
  end;
end;

procedure MakeXMLName(var Name: string);
var
  i: Integer;
begin
  i:=1;
  while i<=length(Name) do begin
    if (Name[i] in ['a'..'z','A'..'Z','_'])
    or (i>1) and (Name[i] in ['0'..'9']) then begin
      inc(i);
    end else begin
      System.Delete(Name,i,1);
    end;
  end;
end;

function LoadXMLConfigViaCodeBuffer(Filename: string): TXMLConfig;
var
  Code: TCodeBuffer;
begin
  Result:=nil;
  Code:=CodeToolBoss.LoadFile(Filename,true,false);
  if Code=nil then exit;
  try
    Result:=TCodeBufXMLConfig.CreateWithCache(Filename);
  except
    on E: Exception do begin
      debugln(['LoadXMLConfigViaCodeBuffer Filename="',Filename,'": ',E.Message]);
    end;
  end;
end;

procedure LoadRect(XMLConfig: TXMLConfig; const Path: string;
  var ARect: TRect);
begin
  LoadRect(XMLConfig,Path,ARect,Rect(0,0,0,0));
end;

procedure LoadRect(XMLConfig: TXMLConfig; const Path:string; var ARect:TRect;
  const DefaultRect: TRect);
begin
  ARect.Left:=XMLConfig.GetValue(Path+'Left',DefaultRect.Left);
  ARect.Top:=XMLConfig.GetValue(Path+'Top',DefaultRect.Top);
  ARect.Right:=XMLConfig.GetValue(Path+'Right',DefaultRect.Right);
  ARect.Bottom:=XMLConfig.GetValue(Path+'Bottom',DefaultRect.Bottom);
end;

procedure SaveRect(XMLConfig: TXMLConfig; const Path: string; const ARect: TRect);
begin
  SaveRect(XMLConfig,Path,ARect,Rect(0,0,0,0));
end;

procedure SaveRect(XMLConfig: TXMLConfig; const Path:string;
  const ARect, DefaultRect: TRect);
begin
  XMLConfig.SetDeleteValue(Path+'Left',ARect.Left,DefaultRect.Left);
  XMLConfig.SetDeleteValue(Path+'Top',ARect.Top,DefaultRect.Top);
  XMLConfig.SetDeleteValue(Path+'Right',ARect.Right,DefaultRect.Right);
  XMLConfig.SetDeleteValue(Path+'Bottom',ARect.Bottom,DefaultRect.Bottom);
end;

procedure LoadPoint(XMLConfig: TXMLConfig; const Path: string;
                    var APoint: TPoint; const DefaultPoint: TPoint);
begin
  APoint.X:=XMLConfig.GetValue(Path+'X',DefaultPoint.X);
  APoint.Y:=XMLConfig.GetValue(Path+'Y',DefaultPoint.Y);
end;

procedure SavePoint(XMLConfig: TXMLConfig; const Path: string;
                    const APoint, DefaultPoint: TPoint);
begin
  XMLConfig.SetDeleteValue(Path+'X',APoint.X,DefaultPoint.X);
  XMLConfig.SetDeleteValue(Path+'Y',APoint.Y,DefaultPoint.Y);
end;

procedure SplitCmdLine(const CmdLine: string;
                       out ProgramFilename, Params: string);
var p, s, l: integer;
  quote: char;
begin
  ProgramFilename:='';
  Params:='';
  if CmdLine='' then exit;
  p:=1;
  s:=1;
  if (CmdLine[p] in ['"','''']) then
  begin
    // skip quoted string
    quote:=CmdLine[p];
    inc(s);
    repeat
      inc(p);
      if p>Length(CmdLine) then Break;
      // check if we have an escape char
      if (CmdLine[p] = '\') and (CmdLine[p]<>PathDelim) then inc(p);
    until (p>Length(CmdLine)) or (CmdLine[p]=quote);
    // go past last character or quoted string
    l:=p-s;
    inc(p);
  end else begin
    while (p<=length(CmdLine)) and (CmdLine[p]>' ') do begin
      if (CmdLine[p] in ['/','\']) and (CmdLine[p]<>PathDelim) then begin
        // skip special char
        inc(p);
      end;
      inc(p);
    end;
    l:=p-s;
  end;
  ProgramFilename:=Copy(CmdLine,s,l);
  while (p<=length(CmdLine)) and (CmdLine[p]<=' ') do inc(p);
  Params:=RightStr(CmdLine,length(CmdLine)-p+1);
end;

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

procedure CheckList(List: TFPList; TestListNil, TestDoubles, TestNils: boolean);
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

procedure RemoveDoubles(List: TStrings);
var
  i: Integer;
  List2: TStringList;
begin
  if List=nil then exit;
  List2:=TStringList.Create;
  List2.AddStrings(List);
  List2.Sort;
  List.Assign(List2);
  List2.Free;
  for i:=List.Count-2 downto 0 do begin
    if List[i]=List[i+1] then List.Delete(i+1);
  end;
end;

function SearchInStringListI(List: TStrings; const s: string): integer;
begin
  if List=nil then exit(-1);
  Result:=List.Count-1;
  while (Result>=0) and (CompareText(List[Result],s)<>0) do dec(Result);
end;

{-------------------------------------------------------------------------------
  procedure ReverseList(List: TList);
  
  Reverse the order of a TList
-------------------------------------------------------------------------------}
procedure ReverseList(List: TList);
var
  i: Integer;
  j: Integer;
begin
  if List=nil then exit;
  i:=0;
  j:=List.Count-1;
  while i<j do begin
    List.Exchange(i,j);
    inc(i);
    dec(j);
  end;
end;

procedure ReverseList(List: TFPList);
var
  i: Integer;
  j: Integer;
begin
  if List=nil then exit;
  i:=0;
  j:=List.Count-1;
  while i<j do begin
    List.Exchange(i,j);
    inc(i);
    dec(j);
  end;
end;

procedure FreeListObjects(List: TList; FreeList: boolean);
var
  i: Integer;
begin
  for i:=0 to List.Count-1 do
    TObject(List[i]).Free;
  List.Clear;
  if FreeList then
    List.Free;
end;

procedure FreeListObjects(List: TFPList; FreeList: boolean);
var
  i: Integer;
begin
  if List=nil then exit;
  for i:=0 to List.Count-1 do
    TObject(List[i]).Free;
  List.Clear;
  if FreeList then
    List.Free;
end;

{-------------------------------------------------------------------------------
  function TrimSearchPath(const SearchPath, BaseDirectory: string): boolean;
  
  - Removes empty paths.
  - Uses TrimFilename on every path.
  - If BaseDirectory<>'' then every relative Filename will be expanded.
  - removes doubles
-------------------------------------------------------------------------------}
function TrimSearchPath(const SearchPath, BaseDirectory: string;
  DeleteDoubles: boolean; ExpandPaths: boolean): string;
var
  CurPath: String;
  EndPos: Integer;
  StartPos: Integer;
  len: Integer;
  BaseDir: String;
begin
  Result:='';
  EndPos:=1;
  len:=length(SearchPath);
  BaseDir:=AppendPathDelim(TrimFilename(BaseDirectory));
  while EndPos<=len do begin
    StartPos:=EndPos;
    // skip empty paths and space chars at start
    while (StartPos<=len) and (SearchPath[StartPos] in [';',#0..#32]) do
      inc(StartPos);
    if StartPos>len then break;
    EndPos:=StartPos;
    while (EndPos<=len) and (SearchPath[EndPos]<>';') do inc(EndPos);
    CurPath:=copy(SearchPath,StartPos,EndPos-StartPos);
    if CurPath<>'' then begin
      // non empty path => expand, trim and normalize
      if ExpandPaths then
        CurPath:=TrimAndExpandDirectory(CurPath,BaseDir)
      else if (BaseDir<>'') and (not FilenameIsAbsolute(CurPath)) then
        CurPath:=BaseDir+CurPath;
      CurPath:=ChompPathDelim(TrimFilename(CurPath));
      if CurPath='' then CurPath:='.';
      // check if path already exists
      if (not DeleteDoubles) or (SearchDirectoryInSearchPath(Result,CurPath)<1)
      then begin
        if Result<>'' then
          CurPath:=';'+CurPath;
        if CurPath<>'' then
          Result:=Result+CurPath
        else
          Result:=Result+'.';
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  BackupFileForWrite

  Params: const Filename, BackupFilename: string
  Result: boolean
  
  Rename Filename to Backupfilename and create empty Filename with same
  file attributes
-------------------------------------------------------------------------------}
function BackupFileForWrite(const Filename, BackupFilename: string): boolean;

  function FileIsLocked(const {%H-}FileName: String): Boolean;
  {$ifdef Windows}
  var
    FHandle: THandle;
  {$endif}
  begin
    {$ifdef Windows}
    // try to open with all denies
    FHandle := FileOpen(UTF8ToSys(FileName), fmOpenRead or fmShareDenyRead or fmShareDenyWrite);
    Result := FHandle = feInvalidHandle;
    if not Result then
      FileClose(FHandle);
    {$else}
    Result := False;
    {$endif}
  end;

var
  FHandle: THandle;
  Code: TCodeBuffer;
  {$IF defined(MSWindows) or defined(HASAMIGA)}
  OldAttr: Longint;
  {$ELSE}
  OldInfo: Stat;
  {$ENDIF}
begin
  Result := False;

  // store file attributes
  {$IF defined(MSWindows) or defined(HASAMIGA)}
  OldAttr := FileGetAttrUTF8(Filename);
  {$ELSE}
  if FpStat(Filename, OldInfo{%H-})<>0 then
    exit; // can't backup this file
  {$ENDIF}
  
  // if not a symlink/hardlink or locked => rename old file (quick), create empty new file
  if not FileIsSymlink(Filename) and
     not FileIsHardLink(FileName) and
     not FileIsLocked(Filename) and
     RenameFileUTF8(Filename, BackupFilename) then
  begin
    // create empty file
    FHandle := FileCreate(UTF8ToSys(FileName));
    FileClose(FHandle);
    Code:=CodeToolBoss.FindFile(Filename);
    if Code<>nil then
      Code.InvalidateLoadDate;
  end
  else // file is a symlink/hardlink or locked or rename failed => copy file (slow)
  if not CopyFile(Filename, BackupFilename) then exit;

  // restore file attributes
  {$IFdef MSWindows}
  FileSetAttrUTF8(FileName, OldAttr);
  {$ELSE}
  FpChmod(Filename, OldInfo.st_Mode and (STAT_IRWXO+STAT_IRWXG+STAT_IRWXU
                                        +STAT_ISUID+STAT_ISGID+STAT_ISVTX));
  {$ENDIF}

  Result := True;
end;

{-------------------------------------------------------------------------------
  function ClearFile(const Filename: string; RaiseOnError: boolean): boolean;
  
  Empty file if exists.
-------------------------------------------------------------------------------}
function ClearFile(const Filename: string; RaiseOnError: boolean): boolean;
var
  fs: TFileStreamUTF8;
begin
  if FileExistsUTF8(Filename) then begin
    try
      InvalidateFileStateCache;
      fs:=TFileStreamUTF8.Create(Filename,fmOpenWrite);
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

function FindProgram(ProgramName, BaseDirectory: string;
  WithBaseDirectory: boolean): string;
var
  Flags: TSearchFileInPathFlags;
begin
  Result:='';
  if ProgramName='' then exit;
  {$IFDEF Unix}
  if ProgramName[1]='~' then begin
    Delete(ProgramName,1,1);
    ProgramName:=GetEnvironmentVariableUTF8('HOME')+ProgramName;
  end;
  {$ENDIF}
  ProgramName:=ResolveDots(ProgramName);
  if FilenameIsAbsolute(ProgramName) then begin
    if FileExistsCached(ProgramName) then
      Result:=ProgramName
    else
      Result:='';
    exit;
  end;
  Flags:=[];
  if not WithBaseDirectory then
    Include(Flags,sffDontSearchInBasePath);
  Result:=FileUtil.SearchFileInPath(ProgramName,BaseDirectory,
                                    GetProgramSearchPath,PathSep,Flags);
end;

function PointToCfgStr(const Point: TPoint): string;
begin
  Result:=IntToStr(Point.X)+','+IntToStr(Point.Y);
end;

procedure CfgStrToPoint(const s: string; var Point: TPoint;
  const DefaultPoint: TPoint);
var
  p: Integer;
begin
  p:=1;
  while (p<=length(s)) and (s[p]<>',') do inc(p);
  Point.X:=StrToIntDef(copy(s,1,p-1),DefaultPoint.X);
  Point.Y:=StrToIntDef(copy(s,p+1,length(s)-p),DefaultPoint.Y);
end;

{-------------------------------------------------------------------------------
  TabsToSpaces

  Params: const s: string; TabWidth: integer
  Result: string

  Convert all tabs to TabWidth number of spaces.
-------------------------------------------------------------------------------}
function TabsToSpaces(const s: string; TabWidth: integer; UseUTF8: boolean): string;

  function ConvertTabsToSpaces(const Src: string; var Dest: string): integer;
  var
    SrcLen: Integer;
    SrcPos: Integer;
    PhysicalX: Integer;
    CurTabWidth: Integer;
    i: Integer;
    CharLen: Integer;
    DestPos: Integer;
  begin
    //DebugLn('ConvertTabsToSpaces ',dbgs(length(Dest)));
    SrcLen:=length(Src);
    SrcPos:=1;
    DestPos:=1;
    PhysicalX:=1;
    while (SrcPos<=SrcLen) do begin
      if (SrcPos and $fffff)=0 then
        DebugLn('ConvertTabsToSpaces ',dbgs(SrcPos));
      case Src[SrcPos] of
      #9:
        begin
          CurTabWidth:=TabWidth - ((PhysicalX-1) mod TabWidth);
          for i:=1 to CurTabWidth do begin
            if Dest<>'' then
              Dest[DestPos]:=' ';
            inc(DestPos);
          end;
          inc(PhysicalX,CurTabWidth);
          inc(SrcPos);
        end;
      #10,#13:
        begin
          if Dest<>'' then
            Dest[DestPos]:=Src[SrcPos];
          inc(SrcPos);
          inc(DestPos);
          if (SrcPos<=SrcLen) and (s[SrcPos] in [#10,#13])
          and (s[SrcPos-1]<>s[SrcPos]) then
            inc(SrcPos);
          PhysicalX:=1;
        end;
      else
        begin
          if Dest<>'' then
            Dest[DestPos]:=Src[SrcPos];
          inc(PhysicalX);
          if UseUTF8 then
            CharLen:=UTF8CodepointSize(@s[SrcPos])
          else
            CharLen:=1;
          for i:=1 to CharLen do begin
            if Dest<>'' then
              Dest[DestPos]:=Src[SrcPos];
            inc(DestPos);
            inc(SrcPos);
          end;
        end;
      end;
    end;
    Result:=DestPos-1;
  end;
  
var
  NewLen: LongInt;
begin
  Result:='';
  NewLen:=ConvertTabsToSpaces(s,Result);
  if NewLen=length(s) then
    Result:=s
  else begin
    SetLength(Result,NewLen);
    ConvertTabsToSpaces(s,Result);
  end;
  //DebugLn('TabsToSpaces ',dbgs(length(Result)));
end;

function SplitString(const s: string; Delimiter: char): TStrings;
begin
  Result:=TStringList.Create;
  SplitString(s,Delimiter,Result,false);
end;

procedure SplitString(const s: string; Delimiter: char; AddTo: TStrings;
  ClearList: boolean);
var
  SLen: Integer;
  StartPos: Integer;
  EndPos: Integer;
begin
  if ClearList then
    AddTo.Clear;
  SLen:=length(s);
  StartPos:=1;
  EndPos:=1;
  repeat
    if (EndPos<=sLen) and (s[EndPos]<>Delimiter) then
      inc(EndPos)
    else begin
      if EndPos>StartPos then
        AddTo.Add(copy(s,StartPos,EndPos-StartPos));
      StartPos:=EndPos+1;
      if StartPos>sLen then exit;
      inc(EndPos);
    end;
  until false;
end;

{-------------------------------------------------------------------------------
  function SpecialCharsToSpaces(const s: string): string;

  Converts illegal characters to spaces.
  Trim leading and trailing spaces.
-------------------------------------------------------------------------------}
function SpecialCharsToSpaces(const s: string; FixUTF8: boolean): string;
var
  i: Integer;
  p: LongInt;
begin
  Result:=s;
  if Result='' then exit;
  // convert line breaks to single spaces
  i:=length(Result);
  while (i>=1) do begin
    if Result[i] in [#10,#13] then begin
      Result[i]:=' ';
      p:=i;
      while (i>1) and (Result[i-1] in [#10,#13]) do dec(i);
      if p>i then
        System.Delete(Result,i,p-i);
    end;
    dec(i);
  end;

  // convert special characters to spaces
  for i:=1 to length(Result) do
    if Result[i] in [#0..#31,#127] then Result[i]:=' ';
  if Result='' then exit;
  if FixUTF8 then
    UTF8FixBroken(Result);
  Result:=UTF8Trim(Result);
end;

function SpecialCharsToHex(const s: string): string;
var
  i: Integer;
begin
  Result:=s;
  if Result='' then exit;
  for i:=length(Result) downto 1 do
    if Result[i]<' ' then
      Result:=copy(Result,1,i-1)
              +'#'+Format('%d',[ord(Result[i])])
              +copy(Result,i+1,length(Result));
end;

function LineBreaksToSystemLineBreaks(const s: string): string;
begin
  Result:=ChangeLineEndings(s,LineEnding);
end;

function LineBreaksToDelimiter(const s: string; Delimiter: char): string;
var
  p: Integer;
  StartPos: LongInt;
begin
  Result:=s;
  p:=1;
  while (p<=length(Result)) do begin
    if Result[p] in [#10,#13] then begin
      StartPos:=p;
      repeat
        inc(p);
      until (p>length(Result)) or (not (Result[p] in [#10,#13]));
      if p<=length(Result) then
        Result:=copy(Result,1,StartPos-1)+Delimiter+copy(Result,p,length(Result))
      else
        Result:=copy(Result,1,StartPos-1);
    end else begin
      inc(p);
    end;
  end;
end;

function StringListToText(List: TStrings; const Delimiter: string;
  IgnoreEmptyLines: boolean): string;
begin
  if List=nil then
    Result:=''
  else
    Result:=StringListPartToText(List,0,List.Count-1,Delimiter,IgnoreEmptyLines);
end;

function StringListPartToText(List: TStrings; FromIndex, ToIndex: integer;
  const Delimiter: string; IgnoreEmptyLines: boolean): string;
var
  i: Integer;
  s: string;
  Size: Integer;
  p: Integer;
begin
  if (List=nil) or (FromIndex>ToIndex) or (FromIndex>=List.Count) then begin
    Result:='';
    exit;
  end;
  if FromIndex<0 then FromIndex:=0;
  if ToIndex>=List.Count then ToIndex:=List.Count-1;
  // calculate size
  Size:=0;
  for i:=FromIndex to ToIndex do begin
    s:=List[i];
    if IgnoreEmptyLines and (s='') then continue;
    if Size>0 then
      inc(Size,length(Delimiter));
    inc(Size,length(s));
  end;
  // build string
  SetLength(Result,Size);
  p:=1;
  for i:=FromIndex to ToIndex do begin
    s:=List[i];
    if IgnoreEmptyLines and (s='') then continue;
    if (p>1) and (Delimiter<>'') then begin
      System.Move(Delimiter[1],Result[p],length(Delimiter));
      inc(p,length(Delimiter));
    end;
    if s<>'' then begin
      System.Move(s[1],Result[p],length(s));
      inc(p,length(s));
    end;
  end;
end;

function StringListToString(List: TStrings; FromIndex, ToIndex: integer;
  IgnoreEmptyLines: boolean): string;
// concatenates strings with #10 characters
// and quotes strings containing #10 with '
var
  Size: PtrInt;
  i: PtrInt;
  s: string;
  j: PtrInt;
  p: PtrInt;
begin
  if (List=nil) or (FromIndex>ToIndex) or (FromIndex>=List.Count) then begin
    Result:='';
    exit;
  end;
  if FromIndex<0 then FromIndex:=0;
  if ToIndex>=List.Count then ToIndex:=List.Count-1;
  // calculate size
  Size:=0;
  for i:=FromIndex to ToIndex do begin
    s:=List[i];
    if IgnoreEmptyLines and (s='') then continue;
    if Size>0 then
      inc(Size);// adding #10 as delimiter
    inc(Size,length(s));
    if System.Pos(#10,s)>0 then begin
      inc(Size,2);
      for j:=1 to length(s) do begin
        if s[j]='''' then
          inc(Size);
      end;
    end;
  end;
  // build string
  SetLength(Result,Size);
  p:=1;
  for i:=FromIndex to ToIndex do begin
    s:=List[i];
    if IgnoreEmptyLines and (s='') then continue;
    if p>1 then begin
      Result[p]:=#10;
      inc(p);
    end;
    if System.Pos(#10,s)<1 then begin
      // just copy the string
      if s<>'' then begin
        System.Move(s[1],Result[p],length(s));
        inc(p,length(s));
      end;
    end else begin
      // quote
      Result[p]:='''';
      inc(p);
      for j:=1 to length(s) do begin
        if s[p]='''' then begin
          Result[p]:='''';
          inc(p);
        end;
        Result[p]:=s[j];
        inc(p);
      end;
      Result[p]:='''';
      inc(p);
    end;
  end;
  //DebugLn(['StringListToString ',dbgstr(Result),' ',Size,' ',p]);
  if Size<>p-1 then
    RaiseException('StringListToString');
end;

procedure StringToStringList(const s: string; List: TStrings);
var
  p: PtrInt;
  LineStartPos: PtrInt;
  Size: PtrInt;
  DstPos: PtrInt;
  Line: string;
begin
  if s='' then exit;
  p:=1;
  while true do begin
    if s[p]='''' then begin
      // quoted
      Size:=0;
      inc(p);
      LineStartPos:=p;
      while p<=length(s) do begin
        if (s[p]='''') then begin
          inc(p);
          if (p>length(s)) or (s[p]<>'''') then break;
        end;
        inc(Size);
        inc(p);
      end;
      SetLength(Line,Size);
      p:=LineStartPos;
      DstPos:=1;
      while p<=length(s) do begin
        if (s[p]='''') then begin
          inc(p);
          if (p>length(s)) or (s[p]<>'''') then break;
        end;
        Line[DstPos]:=s[p];
        inc(DstPos);
        inc(p);
      end;
      List.Add(Line);
      // skip line end
      if p>length(s) then exit;
      if s[p]=#10 then
        inc(p);
    end else begin
      // just copy the string
      LineStartPos:=p;
      while (p<=length(s)) and (s[p]<>#10) do inc(p);
      List.Add(copy(s,LineStartPos,p-LineStartPos));
      // skip line end
      if p>length(s) then exit;
      inc(p);
    end;
    if p>length(s) then begin
      List.Add('');
      exit;
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

  If there is a space in the option add " " around the whole option
-------------------------------------------------------------------------------}
function PrepareCmdLineOption(const Option: string): string;
var i: integer;
begin
  Result:=Option;
  if (Result='') or (Result[1]='"') then exit;
  for i:=1 to length(Result) do begin
    if Result[i]=' ' then begin
      Result:='"'+Result+'"';
      exit;
    end;
  end;
end;

function AddCmdLineParameter(const CmdLine, AddParameter: string): string;
begin
  Result:=CmdLine;
  if (Result<>'') and (Result[length(Result)]<>' ') then
    Result:=Result+' ';
  Result:=Result+AddParameter;
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

  procedure GetTextInfo(out Len, LineCount: integer; out LastLineEmpty: boolean);
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
  Result:=GetEnvironmentVariableUTF8('USER');
end;

function GetCurrentMailAddress: string;
begin
  Result:='<'+GetCurrentUserName+'@'+GetEnvironmentVariableUTF8('HOSTNAME')+'>';
end;

function GetProgramSearchPath: string;
begin
  GetProgramSearchPath := GetEnvironmentVariableUTF8('PATH');
end;

{------------------------------------------------------------------------------
  procedure RaiseException(const Msg: string);

  Raises an exception.
  gdb does not catch fpc Exception objects, therefore this procedure raises
  a standard AV which is catched by gdb.
 ------------------------------------------------------------------------------}
procedure RaiseException(const Msg: string);
begin
  DebugLn('ERROR in IDE: ',Msg);
  // creates an exception, that gdb catches:
  DebugLn('Creating gdb catchable error:');
  if (length(Msg) div (length(Msg) div 10000))=0 then ;
end;

function CopyDirectoryWithMethods(const SrcDirectory, DestDirectory: string;
  OnCopyFile: TOnCopyFileMethod; OnCopyError: TOnCopyErrorMethod;
  Data: TObject): boolean;
var
  SrcDir, DestDir: string;

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
    if (CompareFilenames(CurSrcDir,DestDir)=0)
    or (CompareFilenames(CurDestDir,SrcDir)=0) then begin
      // copying into subdirectory. For example: /home/ to /home/user/
      // or copying from subdirectory. For example: /home/user/ to /home/
      // -> skip
      Result:=true;
      exit;
    end;
    
    if not ForceDirectory(CurDestDir)
    and not HandleError(ceCreatingDirectory,CurDestDir,'') then exit;
    
    if FindFirstUTF8(CurSrcDir+GetAllFilesMask,faAnyFile,FileInfo)=0 then begin
      repeat
        // check if special file
        if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='')
        then continue;
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
      until FindNextUTF8(FileInfo)<>0;
    end;
    FindCloseUTF8(FileInfo);
    
    Result:=true;
  end;
  
begin
  Result:=true;
  SrcDir:=AppendPathDelim(TrimAndExpandDirectory(SrcDirectory));
  DestDir:=AppendPathDelim(TrimAndExpandDirectory(DestDirectory));
  if CompareFilenames(SrcDir,DestDir)=0 then exit;

  if (not DirPathExists(SrcDir))
  and not HandleError(ceSrcDirDoesNotExists,SrcDir,'') then exit;
  
  CopyDir(SrcDir,DestDirectory);
end;

function CreateEmptyFile(const Filename: string): boolean;
var
  fs: TFileStreamUTF8;
begin
  Result:=false;
  try
    InvalidateFileStateCache;
    fs:=TFileStreamUTF8.Create(Filename,fmCreate);
    fs.Free;
    Result:=true;
  except
  end;
end;

function CopyFileWithMethods(const SrcFilename, DestFilename: string;
  OnCopyError: TOnCopyErrorMethod; Data: TObject): boolean;
var
  SrcFileStream, DestFileStream: TFileStreamUTF8;
  {$IFdef MSWindows}
  OldAttr: Longint;
  {$ELSE}
  OldInfo: Stat;
  {$ENDIF}
begin
  Result:=false;
  if CompareFilenames(SrcFilename,DestFilename)=0 then exit;
  
  // read file attributes
  {$IFdef MSWindows}
  OldAttr:=FileGetAttrUTF8(SrcFilename);
  {$ELSE}
  FpStat(SrcFilename,OldInfo{%H-});
  {$ENDIF}
  
  // copy file
  try
    SrcFileStream:=TFileStreamUTF8.Create(SrcFilename,fmOpenRead);
    try
      InvalidateFileStateCache;
      DestFileStream:=TFileStreamUTF8.Create(DestFilename,fmCreate);
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
  {$IFdef MSWindows}
  FileSetAttrUTF8(DestFileName,OldAttr);
  {$ELSE}
  FpChmod(DestFilename, OldInfo.st_Mode and (STAT_IRWXO+STAT_IRWXG+STAT_IRWXU
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

  * -> .*
  ? -> .
  , -> |
  ; -> |
  Backslash characters .+

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
    '*','.','+':
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
    '.','+':
      begin
        Result[DestPos]:='\';
        inc(DestPos);
        Result[DestPos]:=c;
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

function CompareMemStreamText(s1, s2: TMemoryStream): Boolean;
// compare text in s2, s2 ignoring line ends
var
  p1: PChar;
  p2: PChar;
  Count1: Int64;
  Count2: Int64;
begin
  Result:=false;
  if s1.Memory=nil then begin
    Result:=s2.Memory=nil;
  end else begin
    if s2.Memory<>nil then begin
      p1:=PChar(s1.Memory);
      p2:=PChar(s2.Memory);
      Count1:=s1.Size;
      Count2:=s2.Size;
      repeat
        if not (p1^ in [#10,#13]) then begin
          // p1 has normal char
          if p1^=p2^ then begin
            inc(p1);
            dec(Count1);
            inc(p2);
            dec(Count2);
          end else begin
            exit(false);
          end;
        end else begin
          // p1 has a newline
          if (p2^ in [#10,#13]) then begin
            // p2 has a newline
            if (Count1>1) and (p1[1] in [#10,#13]) and (p1[0]<>p1[1]) then
            begin
              inc(p1,2);
              dec(Count1,2);
            end else begin
              inc(p1);
              dec(Count1);
            end;
            if (Count2>1) and (p2[1] in [#10,#13]) and (p2[0]<>p2[1]) then
            begin
              inc(p2,2);
              dec(Count2,2);
            end else begin
              inc(p2);
              dec(Count2);
            end;
          end else begin
            // p1 has newline, p2 not
            exit(false);
          end;
        end;
        if Count1=0 then begin
          Result:=Count2=0;
          exit;
        end else if Count2=0 then begin
          exit(false);
        end;
      until false;
    end;
  end;
end;

function CheckGroupItemChecked(CheckGroup: TCheckGroup; const Caption: string): Boolean;
begin
  Result := CheckGroup.Checked[CheckGroup.Items.IndexOf(Caption)];
end;

end.

