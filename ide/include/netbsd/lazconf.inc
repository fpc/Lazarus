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

{Should become unit Unix in the future. For both Linux and FreeBSD}
uses
  linux;
  
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
  //MkDir(GetPrimaryConfigPath);
end;

{---------------------------------------------------------------------------
  SetPrimaryConfigPath procedure
 ---------------------------------------------------------------------------}
procedure SetPrimaryConfigPath(const NewValue: String);
begin
  PrimaryConfigPath:=FExpand(NewValue);
end;

{---------------------------------------------------------------------------
  SetSecondaryConfigPath procedure
 ---------------------------------------------------------------------------}
procedure SetSecondaryConfigPath(const NewValue: String);
begin
  SecondaryConfigPath:=FExpand(NewValue);
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
  if not FileExists(PrimaryFilename) then begin
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
  Result:=SearchFileInPath('ppc386','',GetEnv('PATH'),':');
end;

function FindDefaultMakePath: string;
begin
  Result:=SearchFileInPath('make','',GetEnv('PATH'),':');
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

initialization
  PrimaryConfigPath:=FExpand('~/.lazarus');
  SecondaryConfigPath:='/etc/lazarus';


{
  $Log$
  Revision 1.4  2002/12/20 11:08:47  mattias
  method resolution clause, class ancestor find declaration, 1.1. makros

  Revision 1.3  2002/07/01 05:53:31  lazarus
  MG: improved default make path for build lazarus

  Revision 1.2  2002/05/10 06:57:51  lazarus
  MG: updated licenses

  Revision 1.1  2002/01/22 19:02:05  marco
   * copied from FreeBSD. Lazarus starts up under NetBSD, and doesn't need changes

  Revision 1.4  2001/12/16 22:24:55  lazarus
  MG: changes for new compiler 20011216

  Revision 1.3  2001/12/10 08:44:23  lazarus
  MG: added search for compiler, if not set

  Revision 1.2  2001/05/27 11:52:01  lazarus
  MG: added --primary-config-path=<filename> cmd line option

  Revision 1.1  2001/01/14 20:50:23  marco
   * Initial version. Same as Linux, and probably will be that way.

}

