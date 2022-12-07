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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}

const
  DefaultFPCSrcDirs: array[1..15] of string = (
    // search first for sources with right version
    '/usr/share/fpcsrc/$(FPCVer)',
    // then search for global paths
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

  DefaultLazarusSrcDirs: array[1..8] of string = (
    '/usr/share/lazarus',
    '/usr/local/share/lazarus',
    '/usr/local/lib/lazarus',
    '/usr/local/lazarus',
    '/usr/lib/lazarus',
    '/usr/lib/lazarus/'+LazarusVersionStr,
    '~/pascal/lazarus',
    '~/lazarus'
    );

var
  PrimaryConfigPath,
  SecondaryConfigPath: string;

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
  {$IFDEF FreeBSD}
  Result:=FindDefaultExecutablePath('gmake');
  {$ELSE}
  Result:=FindDefaultExecutablePath('make');
  {$ENDIF}
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
  Result:='~/tmp/';
end;

procedure GetDefaultCompilerFilenames(List: TStrings);
begin
  AddFilenameToList(List,'/usr/local/bin/'+GetDefaultCompilerFilename);
  AddFilenameToList(List,'/usr/bin/'+GetDefaultCompilerFilename);
  AddFilenameToList(List,'/opt/fpc/'+GetDefaultCompilerFilename);
end;

procedure GetDefaultMakeFilenames(List: TStrings);
begin
  AddFilenameToList(List,'/usr/bin/make');
end;

procedure GetDefaultTestBuildDirs(List: TStrings);
begin
  AddFilenameToList(List,'~/tmp/');
  AddFilenameToList(List,'/tmp/');
  AddFilenameToList(List,'/var/tmp/');
end;

procedure GetDefaultBrowser(var Browser, Params: string);

  function Find(const ShortFilename: string; var Filename: string): boolean;
  begin
    Filename:=SearchFileInPath(ShortFilename,'',
                           GetEnvironmentVariableUTF8('PATH'),PathSeparator,[]);
    Result:=Filename<>'';
  end;

begin
  Params:='%s';
  Browser:='';
  // prefer open source ;)
  if Find('xdg-open',Browser) then exit;
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
  // For the Unix file functions only the slash is a directory separator.
  // The RTL defines AllowDirectorySeparators ['/','\'] for historical reasons.
  AllowDirectorySeparators:=['/'];

  PrimaryConfigPath:=ExpandFileNameUTF8('~/.lazarus');
  SecondaryConfigPath:='/etc/lazarus';
end;


