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
unit Backup_Options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, StdCtrls, ExtCtrls,
  EnvironmentOpts, LazarusIDEStrConsts, IDEProcs, IDEOptionsIntf;

type

  { TBackupOptionsFrame }

  TBackupOptionsFrame = class(TAbstractIDEOptionsEditor)
    BackupHelpLabel: TLabel;
    BakOtherAddExtComboBox:TComboBox;
    BakOtherAddExtLabel:TLabel;
    BakOtherMaxCounterComboBox:TComboBox;
    BakOtherMaxCounterLabel:TLabel;
    BakOtherSubDirComboBox:TComboBox;
    BakOtherSubDirLabel:TLabel;
    BakOtherTypeRadioGroup:TRadioGroup;
    BakProjAddExtComboBox:TComboBox;
    BakProjAddExtLabel:TLabel;
    BakProjMaxCounterComboBox:TComboBox;
    BakProjMaxCounterLabel:TLabel;
    BakProjSubDirComboBox:TComboBox;
    BakProjSubDirLabel:TLabel;
    BakProjTypeRadioGroup:TRadioGroup;
    Bevel2b:TBevel;
    Bevel2a:TBevel;
    Bevel1b:TBevel;
    Bevel1a:TBevel;
    BackupProjectGroupLabel:TLabel;
    BackupOtherGroupLabel:TLabel;
    procedure BakTypeRadioGroupClick(Sender: TObject);
  private
  public
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TBackupOptionsFrame }

procedure TBackupOptionsFrame.BakTypeRadioGroupClick(Sender: TObject);
var
  i: integer;
begin
  i := TRadioGroup(Sender).ItemIndex;
  if Sender=BakProjTypeRadioGroup then
  begin
    BakProjAddExtComboBox.Enabled:=(i=4);
    BakProjAddExtLabel.Enabled:=BakProjAddExtComboBox.Enabled;
    BakProjMaxCounterComboBox.Enabled:=(i=3);
    BakProjMaxCounterLabel.EnableD:=BakProjMaxCounterComboBox.Enabled;
  end else
  begin
    BakOtherAddExtComboBox.Enabled:=(i=4);
    BakOtherAddExtLabel.Enabled:=BakOtherAddExtComboBox.Enabled;
    BakOtherMaxCounterComboBox.Enabled:=(i=3);
    BakOtherMaxCounterLabel.EnableD:=BakOtherMaxCounterComboBox.Enabled;
  end;
end;

function TBackupOptionsFrame.GetTitle: String;
begin
  Result := dlgEnvBckup;
end;

procedure TBackupOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  BackupHelpLabel.Caption := dlgEnvBackupHelpNote;
  BackupProjectGroupLabel.Caption := dlgProjFiles;

  with BakProjTypeRadioGroup do
  begin
    Caption := dlgEnvType;
    with Items do
    begin
      BeginUpdate;
      Add(lisNoBackupFiles);
      Add(dlgSmbFront);
      Add(dlgSmbBehind);
      Add(dlgSmbCounter);
      Add(dlgCustomExt);
      Add(dlgBckUpSubDir);
      EndUpdate;
    end;
  end;

  BakProjAddExtLabel.Caption := dlgEdCustomExt;
  with BakProjAddExtComboBox.Items do
  begin
    BeginUpdate;
    Clear;
    Add('bak');
    Add('old');
    EndUpdate;
  end;

  BakProjMaxCounterLabel.Caption := dlgMaxCntr;
  with BakProjMaxCounterComboBox.Items do
  begin
    BeginUpdate;
    Clear;
    Add('1');
    Add('2');
    Add('3');
    Add('5');
    Add('9');
    Add(BakMaxCounterInfiniteTxt);
    EndUpdate;
  end;

  BakProjSubDirLabel.Caption := dlgEdBSubDir;
  BakProjSubDirComboBox.Text:='';
  with BakProjSubDirComboBox.Items do
  begin
    BeginUpdate;
    Clear;
    Add(dlgBakNoSubDirectory);
    Add('backup');
    EndUpdate;
  end;

  BackupOtherGroupLabel.Caption := dlgEnvOtherFiles;
  with BakOtherTypeRadioGroup do
  begin
    Caption:=dlgEnvType;
    with Items do
    begin
      BeginUpdate;
      Add(lisNoBackupFiles);
      Add(dlgSmbFront);
      Add(dlgSmbBehind);
      Add(dlgSmbCounter);
      Add(dlgCustomExt);
      Add(dlgBckUpSubDir);
      EndUpdate;
    end;
  end;

  BakOtherAddExtLabel.Caption := dlgEdCustomExt;
  with BakOtherAddExtComboBox.Items do
  begin
    BeginUpdate;
    Add('bak');
    Add('old');
    EndUpdate;
  end;

  BakOtherMaxCounterLabel.Caption := dlgMaxCntr;
  with BakOtherMaxCounterComboBox.Items do
  begin
    BeginUpdate;
    Clear;
    Add('1');
    Add('2');
    Add('3');
    Add('5');
    Add('9');
    Add(BakMaxCounterInfiniteTxt);
    EndUpdate;
  end;

  BakOtherSubDirLabel.Caption := dlgEdBSubDir;
  with BakOtherSubDirComboBox.Items do
  begin
    BeginUpdate;
    Clear;
    Add(dlgBakNoSubDirectory);
    Add('backup');
    EndUpdate;
  end;
end;

procedure TBackupOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TEnvironmentOptions do
  begin
    with BackupInfoProjectFiles do
    begin
      case BackupType of
       bakNone:          BakProjTypeRadioGroup.ItemIndex:=0;
       bakSymbolInFront: BakProjTypeRadioGroup.ItemIndex:=1;
       bakSymbolBehind:  BakProjTypeRadioGroup.ItemIndex:=2;
       bakCounter:       BakProjTypeRadioGroup.ItemIndex:=3;
       bakUserDefinedAddExt: BakProjTypeRadioGroup.ItemIndex:=4;
       bakSameName:      BakProjTypeRadioGroup.ItemIndex:=5;
      end;
      SetComboBoxText(BakProjAddExtComboBox,AdditionalExtension);
      if MaxCounter<=0 then
        SetComboBoxText(BakProjMaxCounterComboBox,BakMaxCounterInfiniteTxt)
      else
        SetComboBoxText(BakProjMaxCounterComboBox,IntToStr(MaxCounter));
      if SubDirectory<>'' then
        SetComboBoxText(BakProjSubDirComboBox,SubDirectory)
      else
        SetComboBoxText(BakProjSubDirComboBox,dlgBakNoSubDirectory);
    end;
    BakTypeRadioGroupClick(BakProjTypeRadioGroup);
    with BackupInfoOtherFiles do
    begin
      case BackupType of
       bakNone:          BakOtherTypeRadioGroup.ItemIndex:=0;
       bakSymbolInFront: BakOtherTypeRadioGroup.ItemIndex:=1;
       bakSymbolBehind:  BakOtherTypeRadioGroup.ItemIndex:=2;
       bakCounter:       BakOtherTypeRadioGroup.ItemIndex:=3;
       bakUserDefinedAddExt: BakOtherTypeRadioGroup.ItemIndex:=4;
       bakSameName:      BakOtherTypeRadioGroup.ItemIndex:=5;
      end;
      SetComboBoxText(BakOtherAddExtComboBox,AdditionalExtension);
      if MaxCounter<=0 then
        SetComboBoxText(BakOtherMaxCounterComboBox,BakMaxCounterInfiniteTxt)
      else
        SetComboBoxText(BakOtherMaxCounterComboBox,IntToStr(MaxCounter));
      if SubDirectory<>'' then
        SetComboBoxText(BakOtherSubDirComboBox,SubDirectory)
      else
        SetComboBoxText(BakOtherSubDirComboBox,dlgBakNoSubDirectory);
    end;
    BakTypeRadioGroupClick(BakOtherTypeRadioGroup);
  end;
end;

procedure TBackupOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TEnvironmentOptions do 
  begin
    with BackupInfoProjectFiles do
    begin
      case BakProjTypeRadioGroup.ItemIndex of
       0: BackupType:=bakNone;
       1: BackupType:=bakSymbolInFront;
       2: BackupType:=bakSymbolBehind;
       3: BackupType:=bakCounter;
       4: BackupType:=bakUserDefinedAddExt;
       5: BackupType:=bakSameName;
      end;
      AdditionalExtension:=BakProjAddExtComboBox.Text;
      if BakProjMaxCounterComboBox.Text=BakMaxCounterInfiniteTxt then
        MaxCounter:=0
      else
        MaxCounter:=StrToIntDef(BakProjMaxCounterComboBox.Text,1);
      SubDirectory:=BakProjSubDirComboBox.Text;
      if SubDirectory=dlgBakNoSubDirectory then
        SubDirectory:='';
    end;
    with BackupInfoOtherFiles do
    begin
      case BakOtherTypeRadioGroup.ItemIndex of
       0: BackupType:=bakNone;
       1: BackupType:=bakSymbolInFront;
       2: BackupType:=bakSymbolBehind;
       3: BackupType:=bakCounter;
       4: BackupType:=bakUserDefinedAddExt;
       5: BackupType:=bakSameName;
      end;
      AdditionalExtension:=BakOtherAddExtComboBox.Text;
      if BakOtherMaxCounterComboBox.Text=BakMaxCounterInfiniteTxt then
        MaxCounter:=0
      else
        MaxCounter:=StrToIntDef(BakOtherMaxCounterComboBox.Text,1);
      if BakOtherSubDirComboBox.Text=dlgBakNoSubDirectory then
        SubDirectory:=''
      else
        SubDirectory:=BakOtherSubDirComboBox.Text;
    end;
  end;
end;

class function TBackupOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEnvironmentOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupEnvironment, TBackupOptionsFrame, EnvOptionsBackup);
end.

