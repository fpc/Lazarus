{%MainUnit ../linux/lazconf.inc}
// included by linux/lazconf.inc, freebsd/lazconf.inc, netbsd/lazconf.inc
// todo: use $target here ?

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
}

const
  DefaultFPCSrcDirs: array[1..14] of string = (
    '/usr/share/fpcsrc',
    '/usr/local/share/fpcsrc',
    '/usr/fpcsrc',
    '/usr/share/fpc/src',
    '/usr/fpc/src',
    '/usr/local/fpc/src',
    '/usr/local/share/fpc/src',
    '/usr/local/src/fpc',
    '/usr/lib/fpc/src',
    '/usr/local/lib/fpc/src',
    '/vol/fpc/src',
    '/vol/lib/fpc/src',
    // These paths are created by the fpc rpm creation script and do not
    // contain all sources. So, they are searched last.
    '/usr/src/fpc',
    '/vol/src/fpc'
    );

var
  PrimaryConfigPath,
  SecondaryConfigPath: string;

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
  createPrimaryConfigPath function
 ---------------------------------------------------------------------------}
procedure CreatePrimaryConfigPath;
begin
  CreateDir(GetPrimaryConfigPath);
end;

{---------------------------------------------------------------------------
  SetPrimaryConfigPath procedure
 ---------------------------------------------------------------------------}
procedure SetPrimaryConfigPath(const NewValue: String);
begin
writeln('SetPrimaryConfigPath NewValue="',NewValue,'" -> "',ExpandFileName(NewValue),'"');
  PrimaryConfigPath:=ExpandFileName{FExpand}(NewValue);
end;

{---------------------------------------------------------------------------
  SetSecondaryConfigPath procedure
 ---------------------------------------------------------------------------}
procedure SetSecondaryConfigPath(const NewValue: String);
begin
  SecondaryConfigPath:=ExpandFileName{FExpand}(NewValue);
end;

{---------------------------------------------------------------------------
  CopySecondaryConfigFile procedure
 ---------------------------------------------------------------------------}
procedure CopySecondaryConfigFile(const AFilename: String);
var
  PrimaryFilename, SecondaryFilename: string;
  SrcFS, DestFS: TFileStream;
begin
  PrimaryFilename:=GetPrimaryConfigPath+PathDelim+AFilename;
  SecondaryFilename:=GetSecondaryConfigPath+PathDelim+AFilename;
  if (not FileExists(PrimaryFilename))
  and (FileExists(SecondaryFilename)) then begin
    try
      SrcFS:=TFileStream.Create(SecondaryFilename,fmOpenRead);
      try
        DestFS:=TFileStream.Create(PrimaryFilename,fmCreate);
        try
          DestFS.CopyFrom(SrcFS,SrcFS.Size);
        finally
          DestFS.Free;
        end;
      finally
        SrcFS.Free;
      end;
    except
    end;
  end;
end;

{---------------------------------------------------------------------------
  function CreateCompilerTestPascalFilename: string;
 ---------------------------------------------------------------------------}
function CreateCompilerTestPascalFilename: string;
var
  fs: TFileStream;
begin
  Result:=AppendPathDelim(GetPrimaryConfigPath)+'compilertest.pas';
  if not FileExists(Result) then begin
    fs:=TFileStream.Create(Result,fmCreate);
    fs.Free;
  end;
end;

{---------------------------------------------------------------------------
  function FindDefaultCompilerPath: string;
 ---------------------------------------------------------------------------}
function FindDefaultCompilerPath: string;
begin
  Result:=FindDefaultExecutablePath(GetDefaultCompilerFilename);
end;

{---------------------------------------------------------------------------
  function FindDefaultMakePath: string;
 ---------------------------------------------------------------------------}
function FindDefaultMakePath: string;
begin
  Result:=FindDefaultExecutablePath('make');
end;

{---------------------------------------------------------------------------
  function GetDefaultExecutableExt: string;
 ---------------------------------------------------------------------------}
function GetDefaultExecutableExt: string;
begin
  Result:='';
end;

function GetDefaultCompiledUnitExt(FPCVersion, FPCRelease: integer): string;
begin
  Result:='.ppu';
end;

function OSLocksExecutables: boolean;
begin
  Result:=false;
end;

function GetDefaultTestBuildDirectory: string;
begin
  Result:='/tmp/';
  if not DirPathExists(Result) then begin
    if DirPathExists('/var/tmp/') then
      Result:='/var/tmp/';
  end;
end;

procedure GetDefaultCompilerFilenames(List: TStrings);
begin
  List.Add('/usr/bin/'+GetDefaultCompilerFilename);
  List.Add('/opt/fpc/'+GetDefaultCompilerFilename);
end;

procedure GetDefaultMakeFilenames(List: TStrings);
begin
  List.Add('/usr/bin/make');
end;

procedure GetDefaultTestBuildDirs(List: TStrings);
begin
  List.Add('/tmp/');
  List.Add('/var/tmp/');
end;

function GetDefaultLCLWidgetType: string;
begin
  Result:='gtk';
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

{---------------------------------------------------------------------------
  procedure InternalInit;
 ---------------------------------------------------------------------------}
procedure InternalInit;
begin
  PrimaryConfigPath:=ExpandFileName{FExpand}('~/.lazarus');
  SecondaryConfigPath:='/etc/lazarus';
end;


