{
 /***************************************************************************
                               lazconf.pp
                             -------------------
                           Lazarus Config Functions
                   Initial Revision  : Tue Apr 18 22:10:00 CET 2000

 ***************************************************************************/

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
}

{
@author(Config Path Functions - Curtis White <cwhite@aracnet.com>)
@created(18-Apr-2000)
@lastmod(18-Apr-2000)

This unit contains functions to manage OS specific configuration path
information from within Lazarus.
}
unit LazConf;

{$mode objfpc}{$H+}

interface

{$ifdef Trace}
  {$ASSERTIONS ON}
{$endif}

uses
  SysUtils, Classes,
  // LazUtils
  FileUtil, LazFileUtils, LazUTF8, LazUTF8Classes, LazLoggerBase,
  // Codetools
  DefineTemplates;

const
  LazarusVersionStr = {$I version.inc};

function CompareLazarusVersion(V1, V2: string): integer;

{ Config Path Functions }

{ The primary config path is the local or user specific path.
  If the primary config path does not exist, it will automatically be
  created by the IDE.
  The secondary config path is for templates. The IDE will never write to it.
  If a config file is not found in the primary config file, Lazarus will
  copy the template file from the secondary config file. If there is no
  template file, the IDE will use defaults.
}
function GetPrimaryConfigPath: String;
function GetSecondaryConfigPath: String;
function CreatePrimaryConfigPath: boolean;
procedure SetPrimaryConfigPath(const NewValue: String);
procedure SetSecondaryConfigPath(const NewValue: String);
procedure CopySecondaryConfigFile(const ShortFilename: String);
function GetProjectSessionsConfigPath: String;

function GetDefaultTestBuildDirectory: string;
procedure GetDefaultTestBuildDirs(List: TStrings);
// create a pascal file, which can be used to test the compiler
function CreateCompilerTestPascalFilename: string;

function FindDefaultExecutablePath(const Executable: string): string;
procedure GetDefaultCompilerFilenames(List: TStrings); // list of standard paths of compiler on various distributions
function FindDefaultCompilerPath: string; // full path of GetDefaultCompilerFilename
function FindDefaultMakePath: string; // full path of "make"
procedure GetDefaultMakeFilenames(List: TStrings); // list of standard paths of "make" on various distributions
function GetDefaultFPCSrcDirectories: TStringList;
function GetDefaultLazarusSrcDirectories: TStringList;

// returns the standard executable extension (e.g '.exe')
function GetExecutableExt(TargetOS: string = ''): string;
function MakeStandardExeFilename(TargetOS, Filename: string): string;
// returns the standard library extension (e.g '.dll' or '.dylib')
function GetLibraryExt(TargetOS: string = ''): string;
// returns the standard library prefix (e.g 'lib')
function GetLibraryPrefix(TargetOS: string = ''): string;
function MakeStandardLibFilename(TargetOS, Filename: string): string;

// returns the standard file extension for compiled units (e.g '.ppu')
function GetDefaultCompiledUnitExt({%H-}FPCVersion, {%H-}FPCRelease: integer): string;

function OSLocksExecutables: boolean;

// returns the default browser
procedure GetDefaultBrowser(var Browser, Params: string);

// Replace OnGetApplicationName, so that Application.Title
// doesn't interfere with GetAppConfigDir and related.
function GetLazarusApplicationName: string;

type
  TLazConfMacroFunc = procedure(var s: string);
var
  LazConfMacroFunc: TLazConfMacroFunc = nil;
procedure LazConfSubstituteMacros(var s: string);
procedure AddFilenameToList(List: TStrings; const Filename: string;
  SkipEmpty: boolean = true);

const
  EmptyLine = LineEnding + LineEnding;
  EndOfLine: shortstring = LineEnding;
  
const
  ExitCodeRestartLazarus = 99;

implementation

{$I lazconf.inc}

procedure AddFilenameToList(List: TStrings; const Filename: string;
  SkipEmpty: boolean);
var
  i: Integer;
begin
  if SkipEmpty and (Filename='') then exit;
  for i:=0 to List.Count-1 do
    if CompareFilenames(List[i],Filename)=0 then exit;
  List.Add(Filename);
end;

function GetLazarusApplicationName: string;
begin
  Result := 'lazarus';
end;

procedure LazConfSubstituteMacros(var s: string);
begin
  if Assigned(LazConfMacroFunc) then
    LazConfMacroFunc(s);
end;

{---------------------------------------------------------------------------
  function CreateCompilerTestPascalFilename: string;
 ---------------------------------------------------------------------------}
function CreateCompilerTestPascalFilename: string;

  function CreateFile(const Filename: string): boolean;
  var
    fs: TFileStreamUTF8;
  begin
    if FileExistsUTF8(Filename) then exit(true);
    Result:=false;
    try
      fs:=TFileStreamUTF8.Create(Filename,fmCreate);
      fs.Free;
      Result:=true;
    except
    end;
  end;

begin
  Result:=AppendPathDelim(GetPrimaryConfigPath)+'compilertest.pas';
  if CreateFile(Result) then exit;
  Result:=AppendPathDelim(ExpandFileNameUTF8(GetTempDir))+'compilertest.pas';
  if CreateFile(Result) then exit;
  Debugln('unable to create temporary file ',Result);
  Result:='';
end;

function FindDefaultExecutablePath(const Executable: string): string;
begin
  if FilenameIsAbsolute(Executable) then
    Result:=Executable
  else
    Result:=SearchFileInPath(Executable,'',
                             GetEnvironmentVariableUTF8('PATH'),PathSeparator,
                             [sffDontSearchInBasePath]);
  Result:=TrimFilename(Result);
end;

function CompareLazarusVersion(V1, V2: string): integer;
// compare decimal numbers in strings
// For example
//   '0.9.30' < '1.0'
//   '1.0 RC1' < '1.0 RC2'
//   '1.0 RC2' < '1.0'
//   '1.0' < '1.1'
//   '1.0' < '1.0-0'
//   '1.0 RC2' < '1.0.1'
//   '1.0-2' < '1.0.0'

// Rules: 'RC' < EndOfString < '-' < '.'

  function ReadNumber(var p: PChar): integer;
  begin
    if p^=#0 then exit(-1);

    if p^ in ['0'..'9'] then begin
      Result:=0;
      while (p^ in ['0'..'9']) do begin
        if Result<100000 then
          Result:=Result*10+ord(p^)-ord('0');
        inc(p);
      end;
    end else begin
      while (p^ in [' ',#9]) do inc(p);
      case p^ of
      '-': Result:=1;
      '.': Result:=2;
      else Result:=-2; // for example 'RC'
      end;
      while not (p^ in [#0,'0'..'9']) do inc(p);
    end;
  end;

var
  p1: PChar;
  p2: PChar;
  Number1: Integer;
  Number2: Integer;
begin
  if V1='' then begin
    if V2='' then
      exit(0)
    else
      exit(-1);
  end else begin
    if V2='' then
      exit(1);
  end;
  p1:=PChar(V1);
  p2:=PChar(V2);
  while (p1^<>#0) or (p2^<>#0) do begin
    Number1:=ReadNumber(p1);
    Number2:=ReadNumber(p2);
    if Number1>Number2 then exit(1);
    if Number1<Number2 then exit(-1);
  end;
  Result:=0;
end;

{---------------------------------------------------------------------------
  getPrimaryConfigPath function
 ---------------------------------------------------------------------------}
function GetPrimaryConfigPath: String;
begin
  Result := PrimaryConfigPath;
end;

{---------------------------------------------------------------------------
  getSecondaryConfigPath function
 ---------------------------------------------------------------------------}
function GetSecondaryConfigPath: String;
begin
  Result := SecondaryConfigPath;
end;

{---------------------------------------------------------------------------
  createPrimaryConfigPath procedure
 ---------------------------------------------------------------------------}
function CreatePrimaryConfigPath: boolean;
begin
  Result:=ForceDirectoriesUTF8(GetPrimaryConfigPath);
end;

{---------------------------------------------------------------------------
  SetPrimaryConfigPath procedure
 ---------------------------------------------------------------------------}
procedure SetPrimaryConfigPath(const NewValue: String);
begin
  debugln('SetPrimaryConfigPath NewValue="',UTF8ToConsole(NewValue),'" -> "',UTF8ToConsole(ExpandFileNameUTF8(NewValue)),'"');
  PrimaryConfigPath := ChompPathDelim(ExpandFileNameUTF8(NewValue));
end;

{---------------------------------------------------------------------------
  SetSecondaryConfigPath procedure
 ---------------------------------------------------------------------------}
procedure SetSecondaryConfigPath(const NewValue: String);
begin
  debugln('SetSecondaryConfigPath NewValue="',UTF8ToConsole(NewValue),'" -> "',UTF8ToConsole(ExpandFileNameUTF8(NewValue)),'"');
  SecondaryConfigPath := ChompPathDelim(ExpandFileNameUTF8(NewValue));
end;

{---------------------------------------------------------------------------
  CopySecondaryConfigFile procedure
 ---------------------------------------------------------------------------}
procedure CopySecondaryConfigFile(const ShortFilename: String);
var
  PrimaryFilename, SecondaryFilename: string;
begin
  if ShortFilename='' then exit;
  PrimaryFilename:=AppendPathDelim(GetPrimaryConfigPath)+ShortFilename;
  SecondaryFilename:=AppendPathDelim(GetSecondaryConfigPath)+ShortFilename;
  if (not FileExistsUTF8(PrimaryFilename))
  and (FileExistsUTF8(SecondaryFilename)) then begin
    debugln(['CopySecondaryConfigFile ',SecondaryFilename,' -> ',PrimaryFilename]);
    if not CreatePrimaryConfigPath then begin
      debugln(['WARNING: unable to create primary config directory "',GetPrimaryConfigPath,'"']);
      exit;
    end;
    if not CopyFile(SecondaryFilename,PrimaryFilename) then begin
      debugln(['WARNING: unable to copy config "',SecondaryFilename,'" to "',PrimaryFilename,'"']);
      exit;
    end;
    InvalidateFileStateCache; // we have to invalidate cache in order FileExistsCached finds the new primary config file
  end;
end;

function GetProjectSessionsConfigPath: String;
begin
  Result:=AppendPathDelim(GetPrimaryConfigPath)+'projectsessions';
end;

function GetExecutableExt(TargetOS: string): string;
begin
  if TargetOS='' then
    TargetOS:=GetCompiledTargetOS;
  if (CompareText(copy(TargetOS,1,3), 'win') = 0)
  or (CompareText(copy(TargetOS,1,3), 'dos') = 0) then
    Result:='.exe'
  else
    Result:='';
end;

function MakeStandardExeFilename(TargetOS, Filename: string): string;
var
  StdExt: String;
begin
  Result:=Filename;
  if TargetOS='' then
    TargetOS:=GetCompiledTargetOS;
  StdExt:=GetExecutableExt(TargetOS);
  if StdExt='' then exit;
  Result:=ChangeFileExt(Result,StdExt);
end;

function GetLibraryExt(TargetOS: string): string;
begin
  if TargetOS='' then
    TargetOS:=GetCompiledTargetOS;
  if CompareText(copy(TargetOS,1,3), 'win') = 0 then
    Result:='.dll'
  else if CompareText(TargetOS, 'darwin') = 0 then
    Result:='.dylib'
  else if (CompareText(TargetOS, 'linux') = 0)
  or (CompareText(TargetOS, 'android') = 0)
  or (CompareText(TargetOS, 'freebsd') = 0)
  or (CompareText(TargetOS, 'openbsd') = 0)
  or (CompareText(TargetOS, 'netbsd') = 0)
  or (CompareText(TargetOS, 'haiku') = 0) then
    Result:='.so'
  else
    Result:='';
end;

function GetLibraryPrefix(TargetOS: string): string;
var
  SrcOS: String;
begin
  if TargetOS='' then
    TargetOS:=GetCompiledTargetOS;
  Result:='';
  SrcOS:=GetDefaultSrcOSForTargetOS(TargetOS);
  if CompareText(SrcOS, 'unix') = 0 then
    Result:='lib';
end;

function MakeStandardLibFilename(TargetOS, Filename: string): string;
var
  StdExt: String;
  StdPrefix: String;
begin
  Result:=Filename;
  if TargetOS='' then
    TargetOS:=GetCompiledTargetOS;
  // change extension
  StdExt:=GetLibraryExt(TargetOS);
  if StdExt<>'' then
    Result:=ChangeFileExt(Result,StdExt);
  // change prefix
  StdPrefix:=GetLibraryPrefix(TargetOS);
  if StdPrefix<>'' then
    Result:=ExtractFilePath(Result)+StdPrefix+ExtractFileName(Result);
  // lowercase
  if (CompareText(TargetOS,'linux')=0)
  or (CompareText(TargetOS,'freebsd')=0)
  or (CompareText(TargetOS,'netbsd')=0)
  or (CompareText(TargetOS,'openbsd')=0)
  then
    Result:=ExtractFilePath(Result)+lowercase(ExtractFileName(Result));
end;

function GetDefaultLazarusSrcDirectories: TStringList;
var
  i: Integer;
begin
  Result:=TStringList.Create;
  for i:=low(DefaultLazarusSrcDirs) to high(DefaultLazarusSrcDirs) do
    Result.Add(DefaultLazarusSrcDirs[i]);
end;

function GetDefaultFPCSrcDirectories: TStringList;
var
  i: Integer;
begin
  Result:=TStringList.Create;
  for i:=low(DefaultFPCSrcDirs) to high(DefaultFPCSrcDirs) do
    Result.Add(DefaultFPCSrcDirs[i]);
end;

initialization
  Randomize;
  InternalInit;

end.


