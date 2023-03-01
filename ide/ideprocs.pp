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
  FileUtil, LazFileUtils, LazUtilities, LazFileCache, LazUTF8,
  Laz2_XMLCfg, LazLoggerBase, LazTracer,
  // LCL
  StdCtrls, ExtCtrls,
  // CodeTools
  BasicCodeTools, CodeToolManager, CodeToolsConfig, CodeCache, KeywordFuncLists,
  // BuildIntf
  PackageIntf,
  // IDE
  TransferMacros, LazConf, LazarusIDEStrConsts;

const
  SBuildMethod: array[TBuildMethod] of string = (
    'Lazarus',
    'FPMake',
    'Both'
    );
function StringToBuildMethod(const BuildMethod: string): TBuildMethod;
function GetFPCVer: String;

// file operations
function BackupFileForWrite(const Filename, BackupFilename: string): boolean;
function CreateEmptyFile(const Filename: string): boolean;

// file names
function FilenameIsPascalSource(const Filename: string): boolean;
function ChompEndNumber(const s: string): string;

// find file
function FindFilesCaseInsensitive(const Directory,
  CaseInsensitiveFilename: string; IgnoreExact: boolean): TStringList;
function FindFirstFileWithExt(const Directory, Ext: string): string;
function CreateNonExistingFilename(const BaseFilename: string): string;
function FindFPCTool(const Executable, CompilerFilename: string): string;
procedure ResolveLinksInFileList(List: TStrings; RemoveDanglingLinks: Boolean);
function FindProgram(ProgramName, BaseDirectory: string;
                     WithBaseDirectory: boolean): string;

// XMLconfig
function LoadXMLConfigViaCodeBuffer(Filename: string): TXMLConfig;

// Point conversion
function PointToCfgStr(const Point: TPoint): string;
procedure CfgStrToPoint(const s: string; var Point: TPoint; const DefaultPoint: TPoint);

// environment
function GetCurrentUserName: string;
function GetCurrentChangeLog: string;
function GetProgramSearchPath: string;

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
procedure CheckCompNameValidity(const AName: string);


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
  Result:=AppendPathDelim(ExtractFilePath(CompilerFilename))+Executable;
  if ConsoleVerbosity>=0 then
    DebugLn('Hint: (lazarus) FindFPCTool Try="',Result);
  if FileExistsUTF8(Result) then exit;
  Result:=FindDefaultExecutablePath(Executable);
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

function StringToBuildMethod(const BuildMethod: string): TBuildMethod;
begin
  if BuildMethod=SBuildMethod[bmFPMake] then
    result := bmFPMake
  else if BuildMethod=SBuildMethod[bmBoth] then
    result := bmBoth
  else
    result := bmLazarus;
end;

function GetFPCVer: String;
begin
  Result:='$(FPCVer)';
  GlobalMacroList.SubstituteStr(Result);
end;

function ChompEndNumber(const s: string): string;
var
  NewLen: Integer;
begin
  Result:=s;
  NewLen:=length(Result);
  while (NewLen>0) and (Result[NewLen] in ['0'..'9']) do
    dec(NewLen);
  SetLength(Result,NewLen);
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
      if FilenameExtIs(FileInfo.Name,Ext,false) then begin
        Result:=AppendPathDelim(Directory)+FileInfo.Name;
        break;
      end;
    until FindNextUTF8(FileInfo)<>0;
  end;
  FindCloseUTF8(FileInfo);
end;

// XMLConfig

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

procedure CheckList(List: TList; TestListNil, TestDoubles, TestNils: boolean);
var
  Cnt: Integer;
  i: Integer;
  CurItem: Pointer;
  j: Integer;
begin
  if List=nil then begin
    if TestListNil then
      RaiseGDBException('CheckList List is Nil');
    exit;
  end;
  Cnt:=List.Count;
  if TestNils then begin
    for i:=0 to Cnt-1 do
      if List[i]=nil then
        RaiseGDBException('CheckList item is Nil');
  end;
  if TestDoubles then begin
    for i:=0 to Cnt-2 do begin
      CurItem:=List[i];
      for j:=i+1 to Cnt-1 do begin
        if List[j]=CurItem then
          RaiseGDBException('CheckList Double');
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
      RaiseGDBException('CheckList List is Nil');
    exit;
  end;
  Cnt:=List.Count;
  if TestNils then begin
    for i:=0 to Cnt-1 do
      if List[i]=nil then
        RaiseGDBException('CheckList item is Nil');
  end;
  if TestDoubles then begin
    for i:=0 to Cnt-2 do begin
      CurItem:=List[i];
      for j:=i+1 to Cnt-1 do begin
        if List[j]=CurItem then
          RaiseGDBException('CheckList Double');
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
      RaiseGDBException('CheckEmptyListCut');
  end;
end;

procedure RemoveDoubles(List: TStrings);
var
  i: Integer;
  List2: TStringListUTF8Fast;
begin
  if List=nil then exit;
  List2:=TStringListUTF8Fast.Create;
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
  Flags:=[sffFile,sffExecutable];
  if not WithBaseDirectory then
    Include(Flags,sffDontSearchInBasePath);
  Result:=FileUtil.SearchFileInPath(ProgramName,BaseDirectory,
                                    GetProgramSearchPath,PathSep,Flags);
end;

function PointToCfgStr(const Point: TPoint): string;
begin
  Result:=IntToStr(Point.X)+','+IntToStr(Point.Y);
end;

procedure CfgStrToPoint(const s: string; var Point: TPoint; const DefaultPoint: TPoint);
var
  p: Integer;
begin
  p:=1;
  while (p<=length(s)) and (s[p]<>',') do inc(p);
  Point.X:=StrToIntDef(copy(s,1,p-1),DefaultPoint.X);
  Point.Y:=StrToIntDef(copy(s,p+1,length(s)-p),DefaultPoint.Y);
end;

function GetCurrentUserName: string;
begin
  Result:=GetEnvironmentVariableUTF8({$IFDEF MSWindows}'USERNAME'{$ELSE}'USER'{$ENDIF});
end;

function GetCurrentChangeLog: string;
begin
  Result:='<'+GetCurrentUserName+'@'+
  {$IF defined(MSWindows) or defined(HASAMIGA)}
    GetEnvironmentVariableUTF8('COMPUTERNAME')
  {$ELSE}
    GetHostname
  {$ENDIF}
    + '>';
end;

function GetProgramSearchPath: string;
begin
  GetProgramSearchPath := GetEnvironmentVariableUTF8('PATH');
end;

function CreateEmptyFile(const Filename: string): boolean;
var
  fs: TFileStream;
begin
  Result:=false;
  try
    InvalidateFileStateCache;
    fs:=TFileStream.Create(Filename,fmCreate);
    fs.Free;
    Result:=true;
  except
  end;
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

procedure CheckCompNameValidity(const AName: string);
// Raises an exception if not valid.
begin
  if not IsValidIdent(AName) then
    raise Exception.Create(Format(lisComponentNameIsNotAValidIdentifier, [Aname]));
  if WordIsKeyWord.DoItCaseInsensitive(PChar(AName))
  or WordIsDelphiKeyWord.DoItCaseInsensitive(PChar(AName))
  or WordIsPredefinedFPCIdentifier.DoItCaseInsensitive(PChar(AName))
  or WordIsPredefinedDelphiIdentifier.DoItCaseInsensitive(PChar(AName)) then
    raise Exception.Create(Format(lisComponentNameIsAPascalKeyword, [AName]));
end;

end.

