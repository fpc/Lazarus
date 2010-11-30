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
   IDE dialog showing stats about the IDE.
}
unit IDEInfoDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  DefineTemplates, EnvironmentOpts, AboutFrm, LazConf, LazarusIDEStrConsts;

type

  { TIDEInfoDialog }

  TIDEInfoDialog = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    procedure GatherIDEVersion(sl: TStrings);
    procedure GatherEnvironmentVars(sl: TStrings);
    procedure GatherGlobalOptions(sl: TStrings);
  public
    procedure UpdateMemo;
  end;

var
  IDEInfoDialog: TIDEInfoDialog;

function ShowIDEInfo: TModalResult;


implementation

function ShowIDEInfo: TModalResult;
var
  Dlg: TIDEInfoDialog;
begin
  Dlg:=TIDEInfoDialog.Create(nil);
  try
    Result:=Dlg.ShowModal;
  finally
    Dlg.Free;
  end;
end;

{$R *.lfm}

{ TIDEInfoDialog }

procedure TIDEInfoDialog.FormCreate(Sender: TObject);
begin
  Caption:=lisIDEInfoInformationAboutTheIDE;

  UpdateMemo;
end;

procedure TIDEInfoDialog.GatherIDEVersion(sl: TStrings);
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

procedure TIDEInfoDialog.GatherEnvironmentVars(sl: TStrings);
var
  i: Integer;
begin
  sl.Add('Environment variables:');
  for i:=0 to GetEnvironmentVariableCount-1 do
    sl.Add(GetEnvironmentStringUTF8(i)+'='+GetEnvironmentVariableUTF8(GetEnvironmentStringUTF8(i)));
  sl.Add('');
end;

procedure TIDEInfoDialog.GatherGlobalOptions(sl: TStrings);
begin
  sl.add('Global IDE options:');
  sl.Add('Primary config directory='+GetPrimaryConfigPath);
  sl.Add('Secondary config directory='+GetSecondaryConfigPath);
  sl.Add('LazarusDirectory='+EnvironmentOptions.LazarusDirectory);
  sl.Add('CompilerFilename='+EnvironmentOptions.CompilerFilename);
  sl.Add('Real CompilerFilename='+EnvironmentOptions.GetCompilerFilename);
  sl.Add('CompilerMessagesFilename='+EnvironmentOptions.CompilerMessagesFilename);
  sl.Add('FPC source directory='+EnvironmentOptions.FPCSourceDirectory);
  sl.Add('Real FPC source directory='+EnvironmentOptions.GetFPCSourceDirectory);
  sl.Add('Test directory='+EnvironmentOptions.GetTestBuildDirectory);
  sl.Add('');
end;

procedure TIDEInfoDialog.UpdateMemo;
var
  sl: TStringList;
begin
  sl:=TStringList.Create;
  try
    GatherIDEVersion(sl);
    GatherGlobalOptions(sl);
    GatherEnvironmentVars(sl);
    Memo1.Lines.Assign(sl);
  finally
    sl.Free;
  end;
end;

end.

