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

  Author: Michael Van Canneyt
}
unit frmOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, Spin, EditBtn, ExtCtrls, ComCtrls, ButtonPanel;

type

  { TOptionsForm }

  TOptionsForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    CBConfirmDelete: TCheckBox;
    CBCreateBackup: TCheckBox;
    CBSkipEmptyNodes: TCheckBox;
    CBShowHints: TCheckBox;
    CBStartMaximized: TCheckBox;
    CBReopenLast: TCheckBox;
    EBackupExtension: TEdit;
    EDefaultExtension: TEdit;
    FEMakeSkel: TFileNameEdit;
    FEfpdoc: TFileNameEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    LFEMakeskel: TLabel;
    LFEfpdoc: TLabel;
    LEDefaultExtension: TLabel;
    LEBackupExtension: TLabel;
    LEMaxMRU: TLabel;
    PageControl1: TPageControl;
    SEMaxRecentUsed: TSpinEdit;
    tabGeneral: TTabSheet;
    tabDesktop: TTabSheet;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure OptionsFormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure OptionsToForm;
    procedure FormToOptions;
  end;

implementation

uses LazDEOpts, LazDEMsg;

{$R *.lfm}

{ TOptionsForm }

procedure TOptionsForm.OptionsFormShow(Sender: TObject);
begin
  OptionsToForm;
end;

procedure TOptionsForm.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  if ModalResult = mrOk then
    FormToOptions;
end;

procedure TOptionsForm.FormCreate(Sender: TObject);
begin
  Caption:=sOptDlgOptions;
  tabGeneral.Caption:=sOptDlgGeneral;
  tabDesktop.Caption:=sOptDlgDesktop;
  CBShowHints.Caption:=sOptDlgShowHints;
  CBConfirmDelete.Caption:=sOptDlgConfirmDeletes;
  CBCreateBackup.Caption:=sOptDlgCreateBackups;
  CBSkipEmptyNodes.Caption:=sOptDlgSkipEmptyNodes;
  CBStartMaximized.Caption:=sOptDlgStartMaximized;
  CBReopenLast.Caption:=sOptDlgReopenLastFile;
  LEDefaultExtension.Caption:=sOptDlgDefaultExtension;
  LEBackupExtension.Caption:=sOptDlgBackupExtension;
  LEMaxMRU.Caption:=sOptDlgMaxRecentUsed;
  LFEMakeskel.Caption:=sOptDlgMakeskelProgram;
  LFEfpdoc.Caption:=sOptDlgFpdocProgram;
end;

procedure TOptionsForm.optionstoform;
begin
  CBCreateBackup.Checked:=CreateBackup;
  CBSkipEmptyNodes.Checked:=SkipEmptyNodes;
  CBConfirmDelete.Checked:=ConfirmDelete;
  EBackupExtension.Text:=BackupExtension;
  EDefaultExtension.Text:=DefaultExtension;
  SEMaxRecentUsed.Text:=IntToStr(MaxRecentUsed);
  FEMakeskel.Text:=cmdMakeSkel;
  FEFPDoc.Text:=cmdfpdoc;
  CBShowHints.Checked:=ShowHelpHints;
  CBStartMaximized.Checked := StartMaximized;
  CBReopenLast.Checked := ReopenLast;
end;

procedure TOptionsForm.FormToOptions;
begin
  CreateBackup:=CBCreateBackup.Checked;
  SkipEmptyNodes:=CBSkipEmptyNodes.Checked;
  ConfirmDelete:=CBConfirmDelete.Checked;
  BackupExtension:=EBackupExtension.Text;
  DefaultExtension:=EDefaultExtension.Text;
  MaxRecentUsed:=StrToIntDef(SEMaxRecentUsed.Text,0);
  cmdMakeSkel:=FEMakeskel.Text;
  cmdfpdoc:=FEFPDoc.Text;
  ShowHelpHints:=CBShowHints.Checked;
  StartMaximized := CBStartMaximized.Checked;
  ReopenLast := CBReopenLast.Checked;
  SaveOptions;
end;

end.


