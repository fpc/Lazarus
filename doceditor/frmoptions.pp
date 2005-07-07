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
  Buttons, Spin, EditBtn, ExtCtrls, ComCtrls;

type

  { TOptionsForm }

  TOptionsForm = class(TForm)
    BOK: TButton;
    BCancel: TButton;
    CBConfirmDelete: TCheckBox;
    CBCreateBackup: TCheckBox;
    CBSkipEmptyNodes: TCheckBox;
    CBShowHints: TCheckBox;
    EBackupExtension: TEdit;
    EDefaultExtension: TEdit;
    FEMakeSkel: TFileNameEdit;
    FEfpdoc: TFileNameEdit;
    LFEMakeskel: TLabel;
    LFEfpdoc: TLabel;
    LEDefaultExtension: TLabel;
    LEBackupExtension: TLabel;
    LEMaxMRU: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    SEMaxRecentUsed: TSpinEdit;
    tabGeneral: TTabSheet;
    tabDesktop: TTabSheet;
    procedure BOKClick(Sender: TObject);
    procedure OptionsFormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure OptionsToForm;
    procedure FormToOptions;
  end;

var
  OptionsForm: TOptionsForm;

implementation

uses LazDEOpts;

{ TOptionsForm }

procedure TOptionsForm.OptionsFormShow(Sender: TObject);
begin
  if Sender=nil then ;
  OptionsToForm;
end;

procedure TOptionsForm.BOKClick(Sender: TObject);
begin
  if Sender=nil then ;
  FormToOptions;
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
  SaveOptions;
end;

initialization
  {$I frmoptions.lrs}

end.

