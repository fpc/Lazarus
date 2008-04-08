{%MainUnit ../win32/lazconf.inc}
// included by win32/lazconf.inc, win64/lazconf.inc
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
  DefaultFPCSrcDirs: array[1..2] of string = (
    '$(LazarusDir)\fpcsrc',  // this value is set in internalinit
    'c:\pp\source'
    );

  DefaultLazarusSrcDirs: array[1..1] of string = (
    'c:\lazarus'
    );

var
  DefaultDrive: String;
  PrimaryConfigPath,
  SecondaryConfigPath: string;
  DefaultFPCTarget,
  DefaultFPCVersion : string;

procedure GetDefaultLCLLibPaths(List: TStrings);
begin
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
procedure CreatePrimaryConfigPath;
begin
  CreateDir(GetPrimaryConfigPath);
end;

{---------------------------------------------------------------------------
  SetPrimaryConfigPath procedure
 ---------------------------------------------------------------------------}
procedure SetPrimaryConfigPath(const NewValue: String);
begin
  PrimaryConfigPath := NewValue;
end;

{---------------------------------------------------------------------------
  SetSecondaryConfigPath procedure
 ---------------------------------------------------------------------------}
procedure SetSecondaryConfigPath(const NewValue: String);
begin
  SecondaryConfigPath := NewValue;
end;

function FindDefaultCompilerPath: string;
begin
  Result := SearchFileInPath(GetDefaultCompilerFilename,
             format('%sfpc\%s\bin\%s',
               [AppendPathDelim(ProgramDirectory), DefaultFPCVersion, DefaultFPCTarget]),
             SysUtils.GetEnvironmentVariable('PATH'),';',
             []);
  if Result <> '' then exit;
  Result := DefaultDrive + AppendPathDelim(ProgramDirectory) +
    format('fpc\%s\bin\%s\%s',
    [DefaultFPCVersion, DefaultFPCTarget, GetDefaultCompilerFilename]);
  if not FileExists(Result) then Result:='';
end;

function FindDefaultMakePath: string;
begin
  Result := SearchFileInPath('make.exe',
             format('%sfpc\%s\bin\%s',
               [AppendPathDelim(ProgramDirectory), DefaultFPCVersion, DefaultFPCTarget]),
             SysUtils.GetEnvironmentVariable('PATH'),';',
                            []);
end;

function CreateCompilerTestPascalFilename: string;
var
  fs: TFileStream;
begin
  Result := AppendPathDelim(GetPrimaryConfigPath) + 'compilertest.pas';
  if not FileExists(Result) then
  begin
    fs := TFileStream.Create(Result, fmCreate);
    fs.Free;
  end;
end;

function GetDefaultCompiledUnitExt(FPCVersion, FPCRelease: integer): string;
begin
  if (FPCVersion=1) and (FPCRelease=0) then
    Result:='.ppw'
  else
    Result:='.ppu';
end;

function OSLocksExecutables: boolean;
begin
  Result:=true;
end;

function GetDefaultTestBuildDirectory: string;
begin
  Result := SysUtils.GetTempDir;
  if Result <> '' then exit;
  Result := DefaultDrive + '\temp\';
  if DirPathExists(Result) then exit;
  Result := DefaultDrive + '\windows\temp\';
end;

procedure GetDefaultCompilerFilenames(List: TStrings);
begin
  List.Add(DefaultDrive + format('fpc\%s\bin\%s\%s',
    [DefaultFPCVersion, DefaultFPCTarget, GetDefaultCompilerFilename]));
  List.Add(AppendPathDelim(ProgramDirectory) + format('fpc\%s\bin\%s\%s',
    [DefaultFPCVersion, DefaultFPCTarget, GetDefaultCompilerFilename]));
end;

procedure GetDefaultMakeFilenames(List: TStrings);
begin
  List.Add(DefaultDrive + format('fpc\%s\bin\%s\make.exe',
    [DefaultFPCVersion, DefaultFPCTarget]));
  List.Add(AppendPathDelim(ProgramDirectory) +
    format('fpc\%s\bin\%s\make.exe',[DefaultFPCVersion, DefaultFPCTarget]));
end;

procedure GetDefaultTestBuildDirs(List: TStrings);
begin
  List.Add(SysUtils.GetTempDir);
  List.Add(DefaultDrive + '\temp\');
  List.Add(DefaultDrive + '\windows\temp\');
end;

procedure GetDefaultBrowser(var Browser, Params: string);
begin
  Browser:= SearchFileInPath('rundll32.exe','',
                           SysUtils.GetEnvironmentVariable('PATH'),';',
                           [sffDontSearchInBasePath]);
  Params:='url.dll,FileProtocolHandler %s';
end;

{---------------------------------------------------------------------------
  procedure InternalInit;
 ---------------------------------------------------------------------------}
procedure InternalInit;
begin
  DefaultDrive := ExtractFileDrive(ProgramDirectory);
  DefaultFPCTarget:= {$I %FPCTARGET%};
  DefaultFPCVersion:= {$I %FPCVERSION%};
  PrimaryConfigPath := ChompPathDelim(ExtractFilePath(Paramstr(0)));
  SecondaryConfigPath := SysUtils.GetEnvironmentVariable('WINDIR');
  If SecondaryConfigPath = '' Then
    SecondaryConfigPath := DefaultDrive + '\windows';
  DefaultFPCSrcDirs[1] := AppendPathDelim(ProgramDirectory) + 'fpcsrc';
  DefaultLazarusSrcDirs[1] := DefaultDrive + '\lazarus';
end;

