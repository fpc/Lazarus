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
 
  Abstract:
    Dialog to open/start a new project.
}
unit ProjectWizardDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  LazarusIDEStrConsts;

type
  TProjectWizardSelectionType=(tpws_new,
                               tpws_open,
                               tpws_convert,
                               tpws_closeIDE);

  { TProjectWizardDialog }

  TProjectWizardDialog = class(TForm)
    btnNewProject: TBitBtn;
    btnConvertProject: TBitBtn;
    btnCloseIDE: TBitBtn;
    btnOpenProject: TBitBtn;
    procedure btnCloseIDEClick(Sender: TObject);
    procedure btnConvertProjectClick(Sender: TObject);
    procedure btnNewProjectClick(Sender: TObject);
    procedure btnOpenProjectClick(Sender: TObject);
  private
    FResult: TProjectWizardSelectionType;
  public
    property Result: TProjectWizardSelectionType read FResult;
  end; 

function ShowProjectWizardDlg:TProjectWizardSelectionType;

implementation

{ TProjectWizardDialog }

function ShowProjectWizardDlg:TProjectWizardSelectionType;
var
  ProjectWizardDialog: TProjectWizardDialog;
begin
  result:=tpws_closeIDE;
  ProjectWizardDialog:=TProjectWizardDialog.create(nil);
  ProjectWizardDialog.btnNewProject.caption:=lisPWNewProject;
  ProjectWizardDialog.btnOpenProject.caption:=lisPWOpenProject;
  ProjectWizardDialog.btnConvertProject.caption:=lisPWConvertProject;
  ProjectWizardDialog.btnCloseIDE.caption:=lisQuitLazarus;
  try
    if ProjectWizardDialog.showmodal<>mrOk then exit;
    result:=ProjectWizardDialog.result;
  finally
    ProjectWizardDialog.free;
  end;
end;

procedure TProjectWizardDialog.btnNewProjectClick(Sender: TObject);
begin
  FResult:=tpws_new;
end;

procedure TProjectWizardDialog.btnConvertProjectClick(Sender: TObject);
begin
  FResult:=tpws_convert;
end;

procedure TProjectWizardDialog.btnCloseIDEClick(Sender: TObject);
begin
  FResult:=tpws_closeIDE;
end;

procedure TProjectWizardDialog.btnOpenProjectClick(Sender: TObject);
begin
  FResult:=tpws_open;
end;

initialization
  {$I projectwizarddlg.lrs}

end.

