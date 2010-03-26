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

  Author: Juha Manninen

  Abstract:
    Settings for ConvertDelphi unit. Used for unit, project and package conversion.
}
unit ConvertSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, EditBtn, Buttons, ExtCtrls, DialogProcs, CodeToolsStructs;

type

  TConvertTarget = (ctLazarus, ctLazarusWin, ctLazarusAndDelphi);

  { TConvertSettings }

  TConvertSettings = class
  private
    fTitle: String;       // Used for form caption.
    // Unit, Project or Package top file and path.
    fMainFilename: String;
    fMainPath: String;
    // Actual user settings.
    fBackupFiles: boolean;
    fTarget: TConvertTarget;
//    fFormFileRename: boolean;
    fAutoMissingProperties: boolean;
    fAutoMissingComponents: boolean;
    // Replacement properties for Delphi properties.
    fReplaceProps: TStringToStringTree;
    function GetBackupPath: String;
    procedure SetMainFilename(const AValue: String);
  public
    constructor Create(const ATitle: string);
    destructor Destroy; override;
    function RunForm: TModalResult;

    // Lazarus file name based on Delphi file name, keep suffix.
    function DelphiToLazFilename(const DelphiFilename: string;
      LowercaseFilename: boolean): string; overload;
    // Lazarus file name based on Delphi file name with new suffix.
    function DelphiToLazFilename(const DelphiFilename, LazExt: string;
      LowercaseFilename: boolean): string; overload;

    // Create Lazarus file name and copy/rename from Delphi file, keep suffix.
    function RenameDelphiToLazFile(const DelphiFilename: string;
      out LazFilename: string; LowercaseFilename: boolean): TModalResult; overload;
    // Create Lazarus file name and copy/rename from Delphi file with new suffix.
    function RenameDelphiToLazFile(const DelphiFilename, LazExt: string;
      out LazFilename: string; LowercaseFilename: boolean): TModalResult; overload;

    function RenameFile(const SrcFilename, DestFilename: string): TModalResult;
    function BackupFile(const AFilename: string): TModalResult;
  public
    property MainFilename: String read fMainFilename write SetMainFilename;
    property MainPath: String read fMainPath;
    property BackupPath: String read GetBackupPath;

    property BackupFiles: boolean read fBackupFiles;
    property Target: TConvertTarget read fTarget;
//    property FormFileRename: boolean read fFormFileRename;
    property AutoMissingProperties: boolean read fAutoMissingProperties;
    property AutoMissingComponents: boolean read fAutoMissingComponents;
    property ReplaceProps: TStringToStringTree read fReplaceProps;
  end;


  { TConvertSettingsForm }

  TConvertSettingsForm = class(TForm)
    BackupCheckBox: TCheckBox;
    FormFileRenameCheckBox: TCheckBox;
    MainPathEdit: TLabeledEdit;
    TargetRadioGroup: TRadioGroup;
    ReplacementCompsButton: TBitBtn;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    BtnPanel: TPanel;
    HelpButton: TBitBtn;
    SettingsGroupBox: TGroupBox;
    MissingStuffGroupBox: TGroupBox;
    MissingStuffLabel: TLabel;
    MissingComponentCheckBox: TCheckBox;
    MissingPropertyCheckBox: TCheckBox;
    procedure btnOKClick(Sender: TObject);
    procedure ReplacementCompsButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TargetRadioGroupClick(Sender: TObject);
  private
    fSettings: TConvertSettings;
  public
    constructor Create(AOwner: TComponent; ASettings: TConvertSettings); reintroduce;
    destructor Destroy; override;
  end; 

var
  ConvertSettingsForm: TConvertSettingsForm;

implementation


{$R *.lfm}

{ TConvertSettings }

constructor TConvertSettings.Create(const ATitle: string);
begin
  fTitle:=ATitle;
  fMainFilename:='';
  fMainPath:='';
  fReplaceProps:=TStringToStringTree.Create(false);
  // Now hard-code some values. Later move them to a config file.
  fReplaceProps['TFlowPanel']:='TPanel';
  fReplaceProps['TGridPanel']:='TPanel';
  fReplaceProps['TComboBoxEx']:='TComboBox';
  fReplaceProps['TCoolBar']:='TPanel';
  fReplaceProps['TRichEdit']:='TMemo';
  fReplaceProps['TDBRichEdit']:='TDBMemo';
  fReplaceProps['TPNGObject']:='TPortableNetworkGraphic';
  fReplaceProps['TTntForm']:='TForm';
end;

destructor TConvertSettings.Destroy;
begin
  fReplaceProps.Free;
  inherited Destroy;
end;

function TConvertSettings.RunForm: TModalResult;
var
  SettingsForm: TConvertSettingsForm;
begin
  SettingsForm:=TConvertSettingsForm.Create(nil, Self);
  with SettingsForm do
  try
    Caption:=fTitle;
    MainPathEdit.Text:=fMainPath;
{
    // ToDo: Load from XML.
    // Settings --> UI.
    BackupCheckBox.Checked          :=fBackupFiles;
    TargetRadioGroup.ItemIndex      :=integer(fTarget);
    FormFileRenameCheckBox.Checked  :=fFormFileRename;
    MissingPropertyCheckBox.Checked :=fAutoMissingProperties;
    MissingComponentCheckBox.Checked:=fAutoMissingComponents;
}
    Result:=ShowModal;
    if Result=mrOK then begin
      // UI --> Settings.
      fBackupFiles          :=BackupCheckBox.Checked;
      fTarget               :=TConvertTarget(TargetRadioGroup.ItemIndex);
//      fFormFileRename       :=FormFileRenameCheckBox.Checked;
      fAutoMissingProperties:=MissingPropertyCheckBox.Checked;
      fAutoMissingComponents:=MissingComponentCheckBox.Checked;
      // ToDo: Save to XML.
    end;
  finally
    Free;
  end;
end;

function TConvertSettings.DelphiToLazFilename(const DelphiFilename: string;
                                              LowercaseFilename: boolean): string;
begin
  Result:=DelphiToLazFilename(DelphiFilename,'',LowercaseFilename);
end;

function TConvertSettings.DelphiToLazFilename(const DelphiFilename, LazExt: string;
                                              LowercaseFilename: boolean): string;
var
  RelPath, SubPath, fn: string;
begin
  RelPath:=FileUtil.CreateRelativePath(DelphiFilename, fMainPath);
  SubPath:=ExtractFilePath(RelPath);
  if LazExt='' then                 // Include ext in filename if not defined.
    fn:=ExtractFileName(RelPath)
  else
    fn:=ExtractFileNameOnly(RelPath);
  if LowercaseFilename then
    fn:=LowerCase(fn);
  Result:=fMainPath+SubPath+fn+LazExt;
end;

function TConvertSettings.RenameDelphiToLazFile(const DelphiFilename: string;
  out LazFilename: string; LowercaseFilename: boolean): TModalResult;
begin
  Result:=RenameDelphiToLazFile(DelphiFilename,'',LazFilename,LowercaseFilename);
end;

function TConvertSettings.RenameDelphiToLazFile(const DelphiFilename, LazExt: string;
  out LazFilename: string; LowercaseFilename: boolean): TModalResult;
var
  RelPath, SubPath, fn: string;
begin
  RelPath:=FileUtil.CreateRelativePath(DelphiFilename, fMainPath);
  SubPath:=ExtractFilePath(RelPath);
  if LazExt='' then                 // Include ext in filename if not defined.
    fn:=ExtractFileName(RelPath)
  else
    fn:=ExtractFileNameOnly(RelPath);
  if LowercaseFilename then
    fn:=LowerCase(fn);
  // Rename in the same directory.
  if fBackupFiles then begin
    Result:=BackupFile(DelphiFilename); // Save before rename.
    if Result<>mrOK then exit;
  end;
  LazFilename:=fMainPath+SubPath+fn+LazExt;
  Result:=RenameFileWithErrorDialogs(DelphiFilename,LazFilename,[mbAbort]);
end;

function TConvertSettings.RenameFile(const SrcFilename, DestFilename: string): TModalResult;
begin
//  Result:=mrOK;
  if fBackupFiles then
    BackupFile(SrcFilename); // Save before rename.
  Result:=RenameFileWithErrorDialogs(SrcFilename,DestFilename,[mbAbort]);
end;

function TConvertSettings.BackupFile(const AFilename: string): TModalResult;
var
  bp, fn: String;
begin
  bp:=BackupPath;
  fn:=ExtractFileName(AFilename);
  Result:=CopyFileWithErrorDialogs(AFilename,bp+fn,[mbAbort]);
end;

procedure TConvertSettings.SetMainFilename(const AValue: String);
begin
  fMainFilename:=AValue;
  fMainPath:=ExtractFilePath(AValue);
end;

function TConvertSettings.GetBackupPath: String;
const
  BackupPathName='ConverterBackup';
begin
  Result:='';
  if fBackupFiles then begin
    Result:=fMainPath+BackupPathName+PathDelim;
    // Create backup path if needed.
    if not DirectoryExistsUTF8(Result) then
      CreateDirUTF8(Result);
  end;
end;


{ TConvertSettingsForm }

constructor TConvertSettingsForm.Create(AOwner: TComponent; ASettings: TConvertSettings);
begin
  inherited Create(AOwner);
  fSettings:=ASettings;
end;

destructor TConvertSettingsForm.Destroy;
begin
  inherited Destroy;
end;

procedure TConvertSettingsForm.FormCreate(Sender: TObject);
begin
  MainPathEdit.Text:='';
end;

procedure TConvertSettingsForm.FormDestroy(Sender: TObject);
begin
  ;
end;

procedure TConvertSettingsForm.TargetRadioGroupClick(Sender: TObject);
// Delphi compatibility doesn't allow renaming the form file.
var
  Trg: TConvertTarget;
begin
  Trg:=TConvertTarget((Sender as TRadioGroup).ItemIndex);
{  if Trg=ctLazarusAndDelphi then
    FormFileRenameCheckBox.Checked:=false;
  FormFileRenameCheckBox.Enabled:=Trg<>ctLazarusAndDelphi; }
end;

procedure TConvertSettingsForm.ReplacementCompsButtonClick(Sender: TObject);
begin
  //ShowMessage('Sorry, not implemented yet!');
end;

procedure TConvertSettingsForm.btnOKClick(Sender: TObject);
begin
  ModalResult:=mrOk;
end;


end.

