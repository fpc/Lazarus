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
    
  DefaultLazarusSrcDirs: array[1..7] of string = (
    '/usr/share/lazarus',
    '/usr/local/share/lazarus',
    '/usr/local/lib/lazarus',
    '/usr/local/lazarus',
    '/usr/lib/lazarus',
    '~/pascal/lazarus',
    '~/lazarus'
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
  //writeln('SetPrimaryConfigPath NewValue="',NewValue,'" -> "',ExpandFileName(NewValue),'"');
  PrimaryConfigPath:=ExpandFileName(NewValue);
end;

{---------------------------------------------------------------------------
  SetSecondaryConfigPath procedure
 ---------------------------------------------------------------------------}
procedure SetSecondaryConfigPath(const NewValue: String);
begin
  SecondaryConfigPath:=ExpandFileName(NewValue);
end;

{---------------------------------------------------------------------------
  function CreateCompilerTestPascalFilename: string;
 ---------------------------------------------------------------------------}
function CreateCompilerTestPascalFilename: string;

  function CreateFile(const Filename: string): boolean;
  var
    fs: TFileStream;
  begin
    if FileExists(Filename) then exit(true);
    Result:=false;
    try
      fs:=TFileStream.Create(Filename,fmCreate);
      fs.Free;
      Result:=true;
    except
    end;
  end;

begin
  Result:=AppendPathDelim(GetPrimaryConfigPath)+'compilertest.pas';
  if CreateFile(Result) then exit;
  Result:=AppendPathDelim(GetTempDir)+'compilertest1.pas';
  if CreateFile(Result) then exit;
  writeln('unable to create temporay file ',Result);
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
  List.Add('/usr/local/bin/'+GetDefaultCompilerFilename);
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

procedure GetDefaultBrowser(var Browser, Params: string);

  function Find(const ShortFilename: string; var Filename: string): boolean;
  begin
    Filename:=SearchFileInPath(ShortFilename,'',
                   SysUtils.GetEnvironmentVariable('PATH'),PathSeparator,[]);
    Result:=Filename<>'';
  end;

begin
  Params:='%s';
  Browser:='';
  // prefer open source ;)
  if Find('mozilla',Browser) then exit;
  if Find('galeon',Browser) then exit;
  if Find('konqueror',Browser) then exit;
  if Find('safari',Browser) then exit;
  if Find('netscape',Browser) then exit;
  if Find('opera',Browser) then exit;
  if Find('iexplore.exe',Browser) then exit;
end;

{---------------------------------------------------------------------------
  procedure InternalInit;
 ---------------------------------------------------------------------------}
procedure InternalInit;
begin
  PrimaryConfigPath:=ExpandFileName('~/.lazarus');
  SecondaryConfigPath:='/etc/lazarus';
end;


