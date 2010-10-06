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

 Author: Mattias Gaertner

 Abstract:
   IDE dialog showing stats about the used FPC.
}
unit IDEFPCInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  FileProcs, DefineTemplates,
  BaseBuildManager, Project, EnvironmentOpts, LazarusIDEStrConsts, AboutFrm;

type

  { TIDEFPCInfoDialog }

  TIDEFPCInfoDialog = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    procedure UpdateMemo;
    procedure GatherIDEVersion(sl: TStrings);
    procedure GatherEnvironmentVars(sl: TStrings);
    procedure GatherGlobalOptions(sl: TStrings);
    procedure GatherProjectOptions(sl: TStrings);
    procedure GatherActiveOptions(sl: TStrings);
    procedure GatherFPCCfg(sl: TStrings);
  public
  end;

function ShowFPCInfo: TModalResult;

implementation

function ShowFPCInfo: TModalResult;
var
  Dlg: TIDEFPCInfoDialog;
begin
  Dlg:=TIDEFPCInfoDialog.Create(nil);
  try
    Result:=Dlg.ShowModal;
  finally
    Dlg.Free;
  end;
end;

{$R *.lfm}

{ TIDEFPCInfoDialog }

procedure TIDEFPCInfoDialog.FormCreate(Sender: TObject);
begin
  Caption:='Information about used FPC';

  UpdateMemo;
end;

procedure TIDEFPCInfoDialog.UpdateMemo;
var
  sl: TStringList;
begin
  sl:=TStringList.Create;
  try
    GatherIDEVersion(sl);
    GatherEnvironmentVars(sl);
    GatherGlobalOptions(sl);
    GatherProjectOptions(sl);
    GatherActiveOptions(sl);
    GatherFPCCfg(sl);
    Memo1.Lines.Assign(sl);
  finally
    sl.Free;
  end;
end;

procedure TIDEFPCInfoDialog.GatherIDEVersion(sl: TStrings);
const
  LazarusVersionStr= {$I version.inc};
begin
  sl.Add('Lazarus version: '+GetLazarusVersionString);
  sl.Add('Lazarus svn revision: '+LazarusRevisionStr);
  sl.Add('Lazarus build date: '+{$I %date%});
  sl.Add('Lazarus was compiled for '+GetCompiledTargetCPU+'-'+GetCompiledTargetOS);
  sl.Add('Lazarus was compiled with fpc '+{$I %FPCVERSION%});
  sl.Add('');
end;

procedure TIDEFPCInfoDialog.GatherEnvironmentVars(sl: TStrings);

  procedure Add(EnvName: string);
  begin
    sl.Add(EnvName+'='+GetEnvironmentVariableUTF8(EnvName));
  end;

begin
  sl.Add('Environment variables:');
  Add('PATH');
  Add('PP');
  Add('FPCDIR');
  Add('USESVN2REVISIONINC');
  Add('USER');
  Add('HOME');
  Add('PWD');
  Add('LANG');
  Add('LANGUAGE');
  sl.Add('');
end;

procedure TIDEFPCInfoDialog.GatherGlobalOptions(sl: TStrings);
begin
  sl.add('Global IDE options:');
  sl.Add('LazarusDirectory='+EnvironmentOptions.LazarusDirectory);
  sl.Add('CompilerFilename='+EnvironmentOptions.CompilerFilename);
  sl.Add('CompilerMessagesFilename='+EnvironmentOptions.CompilerMessagesFilename);
  sl.Add('');
end;

procedure TIDEFPCInfoDialog.GatherProjectOptions(sl: TStrings);
begin
  sl.Add('Project:');
  if Project1<>nil then begin
    sl.Add('lpi='+Project1.ProjectInfoFile);
    sl.Add('Directory='+Project1.ProjectDirectory);
    sl.Add('TargetOS='+Project1.CompilerOptions.TargetOS);
    sl.Add('TargetCPU='+Project1.CompilerOptions.TargetCPU);
    sl.Add('CompilerFilename='+Project1.CompilerOptions.CompilerPath);
  end else begin
    sl.Add('no project');
  end;
  sl.Add('');
end;

procedure TIDEFPCInfoDialog.GatherActiveOptions(sl: TStrings);
begin
  sl.Add('Active target:');
  sl.Add('TargetOS='+BuildBoss.GetTargetOS(true));
  sl.Add('TargetCPU='+BuildBoss.GetTargetCPU(true));
  sl.Add('');
end;

procedure TIDEFPCInfoDialog.GatherFPCCfg(sl: TStrings);
begin
  sl.Add('');
end;

end.

