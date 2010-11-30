{
 /***************************************************************************
                            initialsetupdlgs.pas
                            --------------------
       Contains the dialogs to help users to setup basic settings.


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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner
  
  Abstract:
    Procedures and dialogs to check environment. The IDE uses these procedures
    at startup to check for example the lazarus directory and warns if, there
    it looks suspicious.
}
unit InitialSetupDlgs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Forms, Controls, Buttons, Dialogs, FileUtil,
  ComCtrls, Laz_XMLCfg,
  LazarusIDEStrConsts, LazConf, EnvironmentOpts, IDEProcs;
  
procedure SetupCompilerFilename(var InteractiveSetup: boolean);
procedure SetupFPCSourceDirectory(var InteractiveSetup: boolean);
procedure SetupLazarusDirectory(var InteractiveSetup: boolean);

function GetValueFromSecondaryConfig(OptionFilename, Path: string): string;

implementation

procedure SetupCompilerFilename(var InteractiveSetup: boolean);
var
  DefaultCompPath: String;
  CurCompilerFilename: String;
  r: integer;
begin
  CurCompilerFilename:=EnvironmentOptions.GetCompilerFilename;
  if CurCompilerFilename='' then
    CurCompilerFilename:=FindDefaultCompilerPath;
  if not FileIsExecutable(CurCompilerFilename) then
    CurCompilerFilename:=GetValueFromSecondaryConfig(EnvOptsConfFileName,
      'EnvironmentOptions/CompilerFilename/Value');
  if not FileIsExecutable(CurCompilerFilename) then begin
    if not InteractiveSetup then exit;
    if CurCompilerFilename='' then begin
      MessageDlg(lisFreePascalCompilerNotFound,
        Format(lisTheFreePascalCompilerFilenameWasNotFoundItIsRecomm, [
          GetDefaultCompilerFilename, #13]),
        mtWarning,[mbIgnore],0);
    end else begin
      DefaultCompPath:=FindDefaultCompilerPath;
      if CompareFilenames(DefaultCompPath,CurCompilerFilename)<>0 then begin
        r:=MessageDlg(lisInvalidCompilerFilename,
           Format(lisTheCurrentCompilerFilenameIsNotAValidExecutableCho, ['"',
             CurCompilerFilename, '"', #13, #13, '"', DefaultCompPath, '"', #13]
             ),
           mtWarning,[mbOk,mbIgnore],0);
        if r=mrOk then
          CurCompilerFilename:=DefaultCompPath;
      end else begin
        MessageDlg(lisInvalidCompilerFilename,
           Format(lisTheCurrentCompilerFilenameIsNotAValidExecutablePlease, ['"',
             CurCompilerFilename, '"', #13, #13]),
           mtWarning,[mbIgnore],0);
      end;
    end;
  end;
  EnvironmentOptions.CompilerFilename:=CurCompilerFilename;
end;

procedure SetupFPCSourceDirectory(var InteractiveSetup: boolean);
var
  CurFPCSrcDir: String;
  DefaultFPCSrcDir: String;
  r: integer;
  Changed: Boolean;
begin
  CurFPCSrcDir:=EnvironmentOptions.GetFPCSourceDirectory;
  Changed:=false;
  if CurFPCSrcDir='' then begin
    CurFPCSrcDir:=FindDefaultFPCSrcDirectory;
    Changed:=true;
  end;
  if not DirectoryExistsUTF8(CurFPCSrcDir) then
  begin
    CurFPCSrcDir:=GetValueFromSecondaryConfig(EnvOptsConfFileName,
      'EnvironmentOptions/FPCSourceDirectory/Value');
    Changed:=true;
  end;
  if not CheckFPCSourceDir(CurFPCSrcDir) then begin
    if (not InteractiveSetup)
    or (not FileIsExecutable(EnvironmentOptions.GetCompilerFilename)) then
      exit;
    if CurFPCSrcDir='' then begin
      MessageDlg(lisFreePascalSourcesNotFound,
        Format(lisTheFreePascalSourceDirectoryWasNotFoundSomeCodeFun, [#13,
          #13, #13]),
        mtWarning,[mbIgnore],0);
    end else begin
      DefaultFPCSrcDir:=FindDefaultFPCSrcDirectory;
      if CompareFilenames(DefaultFPCSrcDir,CurFPCSrcDir)<>0 then begin
        r:=MessageDlg(lisInvalidFreePascalSourceDirectory,
           Format(lisTheCurrentFreePascalSourceDirectoryDoesNotLookCorr, ['"',
             CurFPCSrcDir, '"', #13, #13, '"', DefaultFPCSrcDir, '"', #13]),
           mtWarning,[mbOk,mbIgnore],0);
        if r=mrOk then begin
          CurFPCSrcDir:=DefaultFPCSrcDir;
          Changed:=true;
        end;
      end else begin
        MessageDlg(lisInvalidFreePascalSourceDirectory,
           Format(lisTheCurrentFreePascalSourceDirectoryDoesNotLookCorr2, ['"',
             CurFPCSrcDir, '"', #13, #13]),
           mtWarning,[mbIgnore],0);
      end;
    end;
  end;
  if Changed then
    EnvironmentOptions.FPCSourceDirectory:=CurFPCSrcDir;
end;

procedure SetupLazarusDirectory(var InteractiveSetup: boolean);
var
  CurLazDir: String;
  DefaultLazDir: String;
  r: integer;
begin
  CurLazDir:=EnvironmentOptions.LazarusDirectory;
  if CurLazDir='' then begin
    CurLazDir:=ProgramDirectory(true);
    if not CheckLazarusDirectory(CurLazDir) then
      CurLazDir:=FindDefaultLazarusSrcDirectory;
  end;
  if not CheckLazarusDirectory(CurLazDir) then
    CurLazDir:=GetValueFromSecondaryConfig(EnvOptsConfFileName,
      'EnvironmentOptions/LazarusDirectory/Value');
  if not CheckLazarusDirectory(CurLazDir) then begin
    if not InteractiveSetup then exit;
    if CurLazDir='' then begin
      MessageDlg(lisLazarusDirectoryNotFound,
        Format(lisTheLazarusDirectoryWasNotFoundYouWillNotBeAbleToCr, [#13, #13]
          ),
        mtWarning,[mbIgnore],0);
    end else begin
      DefaultLazDir:=ProgramDirectory(true);
      if CompareFilenames(DefaultLazDir,CurLazDir)<>0 then begin
        r:=MessageDlg(lisLazarusDirectoryNotFound,
           Format(lisTheCurrentLazarusDirectoryDoesNotLookCorrectWithou, ['"',
             CurLazDir, '"', #13, #13, #13, '"', DefaultLazDir, '"', #13]),
           mtWarning,[mbOk,mbIgnore],0);
        if r=mrOk then
          CurLazDir:=DefaultLazDir;
      end else begin
        MessageDlg(lisLazarusDirectoryNotFound,
           Format(lisTheCurrentLazarusDirectoryDoesNotLookCorrectWithou2, ['"',
             CurLazDir, '"', #13, #13, #13]),
           mtWarning,[mbIgnore],0);
      end;
    end;
  end;
  EnvironmentOptions.LazarusDirectory:=CurLazDir;
end;

function GetValueFromSecondaryConfig(OptionFilename, Path: string): string;
var
  XMLConfig: TXMLConfig;
begin
  if not FilenameIsAbsolute(OptionFilename) then
    OptionFilename:=AppendPathDelim(GetSecondaryConfigPath)+OptionFilename;
  if FileExistsCached(OptionFilename) then
  begin
    try
      XMLConfig:=TXMLConfig.Create(OptionFilename);
      try
        Result:=XMLConfig.GetValue(Path,'');
      finally
        XMLConfig.Free;
      end;
    except
      on E: Exception do begin
        debugln(['GetValueFromSecondaryConfig File='+OptionFilename+': '+E.Message]);
      end;
    end;
  end;
end;

end.

