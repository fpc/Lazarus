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
  
}
unit InitialSetupDlgs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Buttons, Dialogs, EnvironmentOpts,
  FileCtrl, LazarusIDEStrConsts, ExtCtrls, StdCtrls, ComCtrls, LazConf,
  IDEProcs;
  
procedure SetupCompilerFilename(var InteractiveSetup: boolean);
procedure SetupFPCSourceDirectory(var InteractiveSetup: boolean);
procedure SetupLazarusDirectory(var InteractiveSetup: boolean);


implementation

procedure SetupCompilerFilename(var InteractiveSetup: boolean);
var
  DefaultCompPath: String;
  CurCompilerFilename: String;
  r: integer;
begin
  CurCompilerFilename:=EnvironmentOptions.CompilerFilename;
  if CurCompilerFilename='' then
    CurCompilerFilename:=FindDefaultCompilerPath;
  if not FileIsExecutable(CurCompilerFilename) then begin
    if not InteractiveSetup then exit;
    if CurCompilerFilename='' then begin
      MessageDlg('Free Pascal Compiler not found',
        'The Free Pascal compiler (filename: ppc386) was not found.'#13
        +'It is recommended that you install fpc.',
        mtWarning,[mbIgnore],0);
    end else begin
      DefaultCompPath:=FindDefaultCompilerPath;
      if CompareFilenames(DefaultCompPath,CurCompilerFilename)<>0 then begin
        r:=MessageDlg('Invalid Compiler Filename',
           'The current compiler filename "'+CurCompilerFilename+'"'#13
           +'is not a valid executable.'#13
           +'Choose Ok to choose the default "'+DefaultCompPath+'".'#13
           +'Otherwise check Environment -> Environment Options -> Files',
           mtWarning,[mbOk,mbIgnore],0);
        if r=mrOk then
          CurCompilerFilename:=DefaultCompPath;
      end else begin
        MessageDlg('Invalid Compiler Filename',
           'The current compiler filename "'+CurCompilerFilename+'"'#13
           +'is not a valid executable.'#13
           +'Plz check Environment -> Environment Options -> Files',
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
begin
  CurFPCSrcDir:=EnvironmentOptions.FPCSourceDirectory;
  if CurFPCSrcDir='' then
    CurFPCSrcDir:=FindDefaultFPCSrcDirectory;
  if not CheckFPCSourceDir(CurFPCSrcDir) then begin
    if (not InteractiveSetup)
    or (not FileIsExecutable(EnvironmentOptions.CompilerFilename)) then
      exit;
    if CurFPCSrcDir='' then begin
      MessageDlg('Free Pascal Sources not found',
        'The Free Pascal source directory was not found.'#13
        +'Some code functions will not work.'#13
        +'It is recommended that you install it and set the path'#13
        +'Environment -> Environment Options -> Files',
        mtWarning,[mbIgnore],0);
    end else begin
      DefaultFPCSrcDir:=FindDefaultFPCSrcDirectory;
      if CompareFilenames(DefaultFPCSrcDir,CurFPCSrcDir)<>0 then begin
        r:=MessageDlg('Invalid Free Pascal source directory',
           'The current Free Pascal source directory "'+CurFPCSrcDir+'"'#13
           +'does not look correct.'#13
           +'Choose Ok to choose the default "'+DefaultFPCSrcDir+'".'#13
           +'Otherwise check Environment -> Environment Options -> Files',
           mtWarning,[mbOk,mbIgnore],0);
        if r=mrOk then
          CurFPCSrcDir:=DefaultFPCSrcDir;
      end else begin
        MessageDlg('Invalid Free Pascal source directory',
           'The current Free Pascal source directory "'+CurFPCSrcDir+'"'#13
           +'does not look correct.'#13
           +'Check Environment -> Environment Options -> Files',
           mtWarning,[mbIgnore],0);
      end;
    end;
  end;
  EnvironmentOptions.FPCSourceDirectory:=CurFPCSrcDir;
end;

procedure SetupLazarusDirectory(var InteractiveSetup: boolean);
var
  CurLazDir: String;
  DefaultLazDir: String;
  r: integer;
begin
  CurLazDir:=EnvironmentOptions.LazarusDirectory;
  if CurLazDir='' then
    CurLazDir:=ProgramDirectory;
  if not CheckLazarusDirectory(CurLazDir) then begin
    if not InteractiveSetup then exit;
    if CurLazDir='' then begin
      MessageDlg('Lazarus directory not found',
        'The Lazarus directory was not found.'#13
        +'You will not be able to create LCL applications.'#13
        +'Plz check Environment -> Environment Options -> Files',
        mtWarning,[mbIgnore],0);
    end else begin
      DefaultLazDir:=ProgramDirectory;
      if CompareFilenames(DefaultLazDir,CurLazDir)<>0 then begin
        r:=MessageDlg('Lazarus directory not found',
           'The current Lazarus directory "'+CurLazDir+'"'#13
           +'does not look correct.'#13
           +'Without it You will not be able to create LCL applications.'#13
           +'Choose Ok to choose the default "'+DefaultLazDir+'".'#13
           +'Otherwise check Environment -> Environment Options -> Files',
           mtWarning,[mbOk,mbIgnore],0);
        if r=mrOk then
          CurLazDir:=DefaultLazDir;
      end else begin
        MessageDlg('Lazarus directory not found',
           'The current Lazarus directory "'+CurLazDir+'"'#13
           +'does not look correct.'#13
           +'Without it You will not be able to create LCL applications.'#13
           +'Check Environment -> Environment Options -> Files',
           mtWarning,[mbIgnore],0);
      end;
    end;
  end;
  EnvironmentOptions.LazarusDirectory:=CurLazDir;
end;

end.

