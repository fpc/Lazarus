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
    Defines the TExternalToolOptions which stores the settings of a single
    external tool. (= Programfilename and parameters)
    All TExternalToolOptions are stored in a TExternalToolList
    (see exttooldialog.pas).
    And this unit provides TExternalToolOptionDlg which is a dialog for editing
    a TExternalToolOptions;
    
}
unit ExtToolEditDlg;

{$mode objfpc}
{$H+}

{$I ide.inc}

interface

uses
  {$IFDEF IDE_MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, FileUtil,
  LCLType, Controls, Forms, Buttons, StdCtrls, ComCtrls,
  Dialogs, ExtCtrls, LCLProc, ButtonPanel,
  IDEMsgIntf, IDEExternToolIntf,
  PropEdits, TransferMacros, LazarusIDEStrConsts,
  EditMsgScannersDlg;

type
  { TExternalToolOptions }

  TExternalToolOptions = class(TIDEExternalToolOptions)
  private
    fKey: word;
    fShift: TShiftState;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    // key and shift are loaded with the keymapping in the editoroptions
    property Key: word read fKey write fKey;
    property Shift: TShiftState read fShift write fShift;
  end;

  { TExternalToolOptionDlg -
    the editor dialog for a single external tool}

  TExternalToolOptionDlg = class(TForm)
    ButtonPanel: TButtonPanel;
    ScannersButton: TButton;
    TitleLabel: TLabel;
    TitleEdit: TEdit;
    FilenameLabel: TLabel;
    FilenameEdit: TEdit;
    OpenDialog: TOpenDialog;
    OpenButton:TButton;
    ParametersLabel: TLabel;
    ParametersEdit: TEdit;
    WorkingDirLabel: TLabel;
    WorkingDirEdit: TEdit;
    OptionsGroupBox: TGroupBox;
    OptionScanOutputForFPCMessagesCheckBox: TCheckBox;
    OptionScanOutputForMakeMessagesCheckBox: TCheckBox;
    KeyGroupBox: TGroupBox;
    MacrosGroupbox: TGroupbox;
    MacrosListbox: TListbox;
    MacrosInsertButton: TButton;
    chkHideMainForm: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure MacrosInsertButtonClick(Sender: TObject);
    procedure MacrosListboxClick(Sender: TObject);
    procedure MacrosListboxDblClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure OpenButtonClick(sender : TOBject);
    procedure ScannersButtonClick(Sender: TObject);
  private
    fOptions: TExternalToolOptions;
    fTransferMacros: TTransferMacroList;
    fScanners: TStrings;
    fKeyBox: TShortCutGrabBox;
    procedure FillMacroList;
    procedure LoadFromOptions;
    procedure SaveToOptions;
    procedure UpdateButtons;
    function ScannersToString(List: TStrings): string;
    procedure SetComboBox(AComboBox: TComboBox; const AValue: string);
    procedure SetOptions(TheOptions: TExternalToolOptions);
    procedure SetTransferMacros(TransferMacroList: TTransferMacroList);
  public
    property Options: TExternalToolOptions read fOptions write SetOptions;
    property MacroList: TTransferMacroList
           read fTransferMacros write SetTransferMacros;
  end;


function ShowExtToolOptionDlg(TransferMacroList: TTransferMacroList;
  ExternalToolOptions: TExternalToolOptions):TModalResult;
  
implementation

{$R *.lfm}

uses
  IDEContextHelpEdit;

function ShowExtToolOptionDlg(TransferMacroList: TTransferMacroList;
  ExternalToolOptions: TExternalToolOptions):TModalResult;
var ExternalToolOptionDlg: TExternalToolOptionDlg;
begin
  Result:=mrCancel;
  ExternalToolOptionDlg:=TExternalToolOptionDlg.Create(nil);
  try
    ExternalToolOptionDlg.Options:=ExternalToolOptions;
    ExternalToolOptionDlg.MacroList:=TransferMacroList;
    Result:=ExternalToolOptionDlg.ShowModal;
    if Result=mrOk then
      ExternalToolOptions.Assign(ExternalToolOptionDlg.Options);
  finally
    ExternalToolOptionDlg.Free;
  end;
end;

{ TExternalToolOptionDlg }

procedure TExternalToolOptionDlg.OpenButtonClick(sender : TOBject);
begin
  OpenDialog.FileName := FilenameEdit.Text;
  if OpenDialog.Execute then FilenameEdit.Text := OpenDialog.FileName;
End;

procedure TExternalToolOptionDlg.ScannersButtonClick(Sender: TObject);
begin
  if ShowEditMsgScannersDialog('Edit tool '+copy(TitleEdit.Text,1,20),
    fScanners)=mrOk
  then
    UpdateButtons;
end;

procedure TExternalToolOptionDlg.SaveToOptions;
begin
  fOptions.Title:=TitleEdit.Text;
  fOptions.Filename:=FilenameEdit.Text;
  fOptions.CmdLineParams:=ParametersEdit.Text;
  fOptions.WorkingDirectory:=WorkingDirEdit.Text;
  fOptions.Key:=fKeyBox.Key;
  fOptions.Shift:=fKeyBox.ShiftState;
  fOptions.ScanOutputForFPCMessages:=
    OptionScanOutputForFPCMessagesCheckBox.Checked;
  fOptions.ScanOutputForMakeMessages:=
    OptionScanOutputForMakeMessagesCheckBox.Checked;
  FOptions.HideMainForm := chkHideMainForm.Checked;
  fOptions.Scanners:=fScanners;
end;

procedure TExternalToolOptionDlg.UpdateButtons;
begin
  if IDEMsgScanners.Count>0 then begin
    ScannersButton.Visible:=true;
    ScannersButton.Caption:=Format(lisetEditCustomScanners, [ScannersToString(
      fScanners)]);
  end else begin
    ScannersButton.Visible:=false;
  end;
end;

function TExternalToolOptionDlg.ScannersToString(List: TStrings): string;
var
  i: Integer;
begin
  if (List=nil) or (List.Count=0) then begin
    Result:='none';
  end else begin
    Result:='';
    for i:=0 to List.Count-1 do begin
      if Result<>'' then
        Result:=Result+',';
      Result:=Result+List[i];
      if length(Result)>20 then begin
        Result:=copy(Result,1,20);
        break;
      end;
    end;
  end;
end;

procedure TExternalToolOptionDlg.LoadFromOptions;
begin
  TitleEdit.Text:=fOptions.Title;
  FilenameEdit.Text:=fOptions.Filename;
  ParametersEdit.Text:=fOptions.CmdLineParams;
  WorkingDirEdit.Text:=fOptions.WorkingDirectory;
  fKeyBox.Key:=fOptions.Key;
  fKeyBox.ShiftState:=fOptions.Shift;
  OptionScanOutputForFPCMessagesCheckBox.Checked:=
    fOptions.ScanOutputForFPCMessages;
  OptionScanOutputForMakeMessagesCheckBox.Checked:=
    fOptions.ScanOutputForMakeMessages;
  chkHideMainForm.Checked := FOptions.HideMainForm;
  fScanners.Assign(fOptions.Scanners);
  UpdateButtons;
end;

procedure TExternalToolOptionDlg.FormCreate(Sender: TObject);
begin
  fScanners:=TStringList.Create;
  Caption:=lisEdtExtToolEditTool;

  TitleLabel.Caption:=dlgPOTitle;

  FilenameLabel.Caption:=lisEdtExtToolProgramfilename;

  OpenButton.Hint:=lisClickHereToBrowseTheFileHint;

  with OpenDialog do begin
    Title:=lisSelectFile;
    Filter:=dlgAllFiles+' ('+GetAllFilesMask+')|'+GetAllFilesMask
      +'|'+lisExePrograms+' (*.exe)|*.exe';
  end;

  ParametersLabel.Caption:=lisEdtExtToolParameters;

  WorkingDirLabel.Caption:=lisEdtExtToolWorkingDirectory;

  OptionsGroupBox.Caption:=lisLazBuildOptions;

  with OptionScanOutputForFPCMessagesCheckBox do
    Caption:=lisEdtExtToolScanOutputForFreePascalCompilerMessages;

  with OptionScanOutputForMakeMessagesCheckBox do
    Caption:=lisEdtExtToolScanOutputForMakeMessages;

  chkHideMainForm.Caption := lisEdtExtToolHideMainForm;

  with KeyGroupBox do
    Caption:=lisEdtExtToolKey;

  fKeyBox:=TShortCutGrabBox.Create(Self);
  with fKeyBox do begin
    Name:='fKeyBox';
    Align:=alClient;
    BorderSpacing.Around:=6;
    Parent:=KeyGroupBox;
  end;

  with MacrosGroupbox do
    Caption:=lisEdtExtToolMacros;

  with MacrosInsertButton do
    Caption:=lisCodeTemplAdd;
    
  ButtonPanel.OKButton.Caption:=lisOk;
  ButtonPanel.HelpButton.Caption:=lisMenuHelp;
  ButtonPanel.CancelButton.Caption:=dlgCancel;

  fOptions:=TExternalToolOptions.Create;
end;

procedure TExternalToolOptionDlg.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fOptions);
  FreeAndNil(fScanners);
end;

procedure TExternalToolOptionDlg.HelpButtonClick(Sender: TObject);
begin
  ShowContextHelpForIDE(Self);
end;

procedure TExternalToolOptionDlg.SetOptions(TheOptions: TExternalToolOptions);
begin
  if fOptions=TheOptions then exit;
  fOptions.Assign(TheOptions);
  LoadFromOptions;
end;

procedure TExternalToolOptionDlg.SetTransferMacros(
  TransferMacroList: TTransferMacroList);
begin
  if fTransferMacros=TransferMacroList then exit;
  fTransferMacros:=TransferMacroList;
  if MacrosListbox=nil then exit;
  FillMacroList;
end;

procedure TExternalToolOptionDlg.FillMacroList;
var i: integer;
begin
  MacrosListbox.Items.BeginUpdate;
  MacrosListbox.Items.Clear;
  if fTransferMacros<>nil then begin
    for i:=0 to fTransferMacros.Count-1 do begin
      if fTransferMacros[i].MacroFunction=nil then begin
        MacrosListbox.Items.Add('$('+fTransferMacros[i].Name+') - '
                    +fTransferMacros[i].Description);
      end else begin
        MacrosListbox.Items.Add('$'+fTransferMacros[i].Name+'() - '
                    +fTransferMacros[i].Description);
      end;
    end;
  end;
  MacrosListbox.Items.EndUpdate;
end;

procedure TExternalToolOptionDlg.SetComboBox(
  AComboBox: TComboBox; const AValue: string);
var i: integer;
begin
  i:=AComboBox.Items.IndexOf(AValue);
  if i>=0 then
    AComboBox.ItemIndex:=i
  else begin
    AComboBox.Items.Add(AValue);
    AComboBox.ItemIndex:=AComboBox.Items.IndexOf(AValue);
  end;
end;

procedure TExternalToolOptionDlg.MacrosInsertButtonClick(Sender: TObject);
var i: integer;
  s: string;
begin
  i:=MacrosListbox.ItemIndex;
  if i<0 then exit;
  if fTransferMacros[i].MacroFunction=nil then
    s:='$('+fTransferMacros[i].Name+')'
  else
    s:='$'+fTransferMacros[i].Name+'()';
  ParametersEdit.Text:=ParametersEdit.Text+s;
end;

procedure TExternalToolOptionDlg.MacrosListboxClick(Sender: TObject);
begin
  MacrosInsertButton.Enabled:=(MacrosListbox.ItemIndex>=0);
end;

procedure TExternalToolOptionDlg.MacrosListboxDblClick(Sender: TObject);
begin
  MacrosInsertButtonClick(nil);
end;

procedure TExternalToolOptionDlg.OKButtonClick(Sender: TObject);
begin
  if (TitleEdit.Text<>'') and (FilenameEdit.Text<>'') then
    SaveToOptions
  else begin
    MessageDlg(lisEdtExtToolTitleAndFilenameRequired,
                  lisEdtExtToolAValidToolNeedsAtLeastATitleAndAFilename,
                  mtError, [mbCancel], 0);
    ModalResult:=mrCancel;
  end;
end;

{ TExternalToolOptions }

procedure TExternalToolOptions.Assign(Source: TPersistent);
var
  Src: TExternalToolOptions;
begin
  if Source is TExternalToolOptions then begin
    Src:=TExternalToolOptions(Source);
    fKey:=Src.fKey;
    fShift:=Src.fShift;
  end;
  inherited Assign(Source);
end;

procedure TExternalToolOptions.Clear;
begin
  fKey:=VK_UNKNOWN;
  fShift:=[];
  inherited Clear;
end;

end.
