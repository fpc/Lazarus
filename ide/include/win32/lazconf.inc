{***************************************************************************
                               lazconf.inc
 ***************************************************************************

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
  DefaultFPCSrcDirs: array[1..1] of string = (
    'C:\pp'
    );

var
  PrimaryConfigPath,
  SecondaryConfigPath: string;

function GetDefaultTargetOS: string;
begin
  Result:='win32';
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
procedure createPrimaryConfigPath;
begin
  CreateDir(GetPrimaryConfigPath);
end;

{---------------------------------------------------------------------------
  SetPrimaryConfigPath procedure
 ---------------------------------------------------------------------------}
procedure SetPrimaryConfigPath(const NewValue: String);
begin
  PrimaryConfigPath:=NewValue;
end;

{---------------------------------------------------------------------------
  SetSecondaryConfigPath procedure
 ---------------------------------------------------------------------------}
procedure SetSecondaryConfigPath(const NewValue: String);
begin
  SecondaryConfigPath:=NewValue;
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

function FindDefaultCompilerPath: string;
begin
  Result:=SearchFileInPath(GetDefaultCompilerFilename,'',
                           GetEnvironmentVariable('PATH'),';',
                           [sffDontSearchInBasePath]);
  if Result<>'' then exit;
  Result:='c:\pp\bin\win32\ppc386.exe';
  if not FileExists(Result) then Result:='';
end;

function FindDefaultMakePath: string;
begin
  Result:=SearchFileInPath('make.exe','',
                           GetEnvironmentVariable('PATH'),';',
                           [sffDontSearchInBasePath]);
end;

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
  function GetDefaultExecutableExt: string;
 ---------------------------------------------------------------------------}
function GetDefaultExecutableExt: string;
begin
  Result:='.exe';
end;

function GetDefaultCompiledUnitExt(FPCVersion, FPCRelease: integer): string;
begin
  if (FPCVersion=1) and (FPCRelease=0) then
    Result:='.ppw'
  else
    Result:='.ppu';
end;

function GetDefaultTestBuildDirectory: string;
begin
  Result:=GetEnvironmentVariable('TEMP');
  if Result<>'' then exit;
  Result:='c:\temp\';
  if DirPathExists(Result) then exit;
  Result:='c:\windows\temp\';
end;

procedure GetDefaultCompilerFilenames(List: TStrings);
begin
  List.Add('c:\pp\bin\win32\ppc386.exe');
end;

procedure GetDefaultMakeFilenames(List: TStrings);
begin
  List.Add('c:\pp\bin\win32\make.exe');
end;

procedure GetDefaultTestBuildDirs(List: TStrings);
begin
  List.Add('c:\tmp\');
  List.Add('c:\windows\temp\');
end;

function GetDefaultLCLWidgetType: string;
begin
  Result:='win32';
end;

{---------------------------------------------------------------------------
  procedure InternalInit;
 ---------------------------------------------------------------------------}
procedure InternalInit;
begin
  PrimaryConfigPath:=ChompPathDelim(ExtractFilePath(Paramstr(0)));
  SecondaryConfigPath:=GetEnvironmentVariable('WINDIR');
  If SecondaryConfigPath = '' Then
    SecondaryConfigPath:='c:\windows';
end;

{
  $Log$
  Revision 1.23  2004/08/16 18:05:29  vincents
  replaced GetEnv from dos unit by GetEnvironmentVariable from SysUtils unit

  Revision 1.22  2004/08/13 12:28:01  mattias
  replaced ppc386 with platform independent name

  Revision 1.21  2004/07/30 15:34:09  vincents
  make executable location is a environment option now.

  Revision 1.20  2003/12/21 13:58:06  mattias
  renamed DirectoryExists to DirPathExists to reduce ambigiousity

  Revision 1.19  2003/12/20 01:20:52  mattias
  splitted output directories for cross compilation

  Revision 1.18  2003/11/16 19:28:33  mattias
  build lazarus now uses custom compiler path

  Revision 1.17  2003/11/15 13:07:09  mattias
  added ambigious unit check for IDE

  Revision 1.16  2003/09/17 22:06:56  mattias
  implemented default lcl widget type

  Revision 1.15  2003/09/09 11:22:17  mattias
  added fileexists for secondary config files

  Revision 1.14  2003/09/09 11:11:02  mattias
  fixed win32 intf showmoadl and lazconf

  Revision 1.13  2003/08/25 12:16:14  marco
   * reverted last patch. CVS dirty.

  Revision 1.12  2003/08/25 11:58:59  marco
   * fixed duplicate dos reference

  Revision 1.11  2003/08/18 10:02:37  mattias
  fixed win32 lazconf and improved enclose selection

  Revision 1.10  2003/08/15 14:28:48  mattias
  clean up win32 ifdefs

  Revision 1.9  2003/08/15 14:01:20  mattias
  combined lazconf things for unix

  Revision 1.8  2003/02/06 20:46:51  mattias
  default fpc src dirs and clean ups

  Revision 1.7  2002/12/20 11:08:47  mattias
  method resolution clause, class ancestor find declaration, 1.1. makros

  Revision 1.6  2002/07/01 05:53:31  lazarus
  MG: improved default make path for build lazarus

  Revision 1.5  2002/05/10 06:57:51  lazarus
  MG: updated licenses

  Revision 1.4  2001/12/16 22:24:55  lazarus
  MG: changes for new compiler 20011216

  Revision 1.3  2001/12/10 08:44:23  lazarus
  MG: added search for compiler, if not set

  Revision 1.2  2001/05/27 11:52:01  lazarus
  MG: added --primary-config-path=<filename> cmd line option

  Revision 1.1  2000/07/13 10:28:22  michael
  + Initial import

  Revision 1.1  2000/04/21 05:12:42  lazarus
  Added lazconf.pp file.
  Added include/linux and include/win32 directories.
  Added lazconf.inc files for both linux and win32.


}

