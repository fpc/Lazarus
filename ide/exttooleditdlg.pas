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
  Classes, SysUtils, LCLType, Controls, Forms, Buttons, StdCtrls, ComCtrls,
  Dialogs, LResources, LazConfigStorage, Laz_XMLCfg,
  IDEExternToolIntf,
  KeyMapping, TransferMacros, IDEProcs, LazarusIDEStrConsts;

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

  {
    the editor dialog for a single external tool
  }

  { TExternalToolOptionDlg }

  TExternalToolOptionDlg = class(TForm)
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
    KeyCtrlCheckBox: TCheckBox;
    KeyAltCheckBox: TCheckBox;
    KeyShiftCheckBox: TCheckBox;
    KeyComboBox: TComboBox;
    KeyGrabButton: TButton;
    MacrosGroupbox: TGroupbox;
    MacrosListbox: TListbox;
    MacrosInsertButton: TButton;
    OKButton: TBitBtn;
    CancelButton: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift:TShiftState);
    procedure KeyGrabButtonClick(Sender: TObject);
    procedure MacrosInsertButtonClick(Sender: TObject);
    procedure MacrosListboxClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure OpenButtonClick(sender : TOBject);
  private
    fOptions: TExternalToolOptions;
    fTransferMacros: TTransferMacroList;
    GrabbingKey: integer; // 0=none, 1=Default key
    procedure ActivateGrabbing(AGrabbingKey: integer);
    procedure DeactivateGrabbing;
    procedure FillMacroList;
    procedure LoadFromOptions;
    procedure SaveToOptions;
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
  If OpenDialog.Execute Then FilenameEdit.Text := OpenDialog.FileName;
End;

procedure TExternalToolOptionDlg.SaveToOptions;
begin
  fOptions.Title:=TitleEdit.Text;
  fOptions.Filename:=FilenameEdit.Text;
  fOptions.CmdLineParams:=ParametersEdit.Text;
  fOptions.WorkingDirectory:=WorkingDirEdit.Text;
  fOptions.Key:=EditorKeyStringToVKCode(KeyComboBox.Text);
  fOptions.Shift:=[];
  if fOptions.Key<>VK_UNKNOWN then begin
    if KeyCtrlCheckBox.Checked then
      fOptions.Shift := fOptions.Shift + [ssCtrl];
    if KeyAltCheckBox.Checked then
      fOptions.Shift := fOptions.Shift + [ssAlt];
    if KeyShiftCheckBox.Checked then
      fOptions.Shift := fOptions.Shift + [ssShift];
  end;
  fOptions.ScanOutputForFPCMessages:=
    OptionScanOutputForFPCMessagesCheckBox.Checked;
  fOptions.ScanOutputForMakeMessages:=
    OptionScanOutputForMakeMessagesCheckBox.Checked;
end;

procedure TExternalToolOptionDlg.LoadFromOptions;
begin
  TitleEdit.Text:=fOptions.Title;
  FilenameEdit.Text:=fOptions.Filename;
  ParametersEdit.Text:=fOptions.CmdLineParams;
  WorkingDirEdit.Text:=fOptions.WorkingDirectory;
  SetComboBox(KeyComboBox,KeyAndShiftStateToEditorKeyString(fOptions.Key,[]));
  KeyCtrlCheckBox.Checked:=(ssCtrl in fOptions.Shift);
  KeyShiftCheckBox.Checked:=(ssShift in fOptions.Shift);
  KeyAltCheckBox.Checked:=(ssAlt in fOptions.Shift);
  OptionScanOutputForFPCMessagesCheckBox.Checked:=
    fOptions.ScanOutputForFPCMessages;
  OptionScanOutputForMakeMessagesCheckBox.Checked:=
    fOptions.ScanOutputForMakeMessages;
end;

procedure TExternalToolOptionDlg.FormCreate(Sender: TObject);
var
  i: word;
  s: string;
begin
  GrabbingKey:=0;
  Caption:=lisEdtExtToolEditTool;

  with TitleLabel do
    Caption:=dlgPOTitle;

  with FilenameLabel do
    Caption:=lisEdtExtToolProgramfilename;

  with OpenButton do
    Hint:=lisClickHereToBrowseTheFileHint;

  with OpenDialog do begin
    Title:=lisSelectFile;
    Filter:=lisExePrograms+' (*.exe)|*.exe|'+lisAllFiles+' (*.*)|*.*';
  end;

  with ParametersLabel do
    Caption:=lisEdtExtToolParameters;

  with WorkingDirLabel do
    Caption:=lisEdtExtToolWorkingDirectory;

  with OptionsGroupBox do
    Caption:=lisLazBuildOptions;

  with OptionScanOutputForFPCMessagesCheckBox do
    Caption:=lisEdtExtToolScanOutputForFreePascalCompilerMessages;

  with OptionScanOutputForMakeMessagesCheckBox do
    Caption:=lisEdtExtToolScanOutputForMakeMessages;

  with KeyGroupBox do
    Caption:=lisEdtExtToolKey;

  with KeyCtrlCheckBox do
    Caption:=lisEdtExtToolCtrl;

  with KeyAltCheckBox do
    Caption:=lisEdtExtToolAlt;

  with KeyShiftCheckBox do
    Caption:=lisEdtExtToolShift;

  with KeyComboBox do begin
    Items.BeginUpdate;
    Items.Add(srVK_NONE);
    for i:=1 to 145 do begin
      s:=KeyAndShiftStateToEditorKeyString(i,[]);
      if not EditorKeyStringIsIrregular(s) then
        Items.Add(s);
    end;
    Items.EndUpdate;
    ItemIndex:=0;
  end;

  with KeyGrabButton do
    Caption:=srkmGrabKey;

  with MacrosGroupbox do
    Caption:=lisEdtExtToolMacros;

  with MacrosInsertButton do
    Caption:=lisCodeTemplAdd;

  fOptions:=TExternalToolOptions.Create;
end;

procedure TExternalToolOptionDlg.FormDestroy(Sender: TObject);
begin
  fOptions.Free;
end;

procedure TExternalToolOptionDlg.KeyGrabButtonClick(Sender: TObject);
begin
  ActivateGrabbing(1);
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

procedure TExternalToolOptionDlg.DeactivateGrabbing;
var i: integer;
begin
  if GrabbingKey=0 then exit;
  // enable all components
  for i:=0 to ComponentCount-1 do begin
    if (Components[i] is TWinControl) then
      TWinControl(Components[i]).Enabled:=true;
  end;
  if GrabbingKey=1 then
    KeyGrabButton.Caption:=srkmGrabKey;
  GrabbingKey:=0;
end;

procedure TExternalToolOptionDlg.ActivateGrabbing(AGrabbingKey: integer);
var i: integer;
begin
  if GrabbingKey>0 then exit;
  GrabbingKey:=AGrabbingKey;
  if GrabbingKey=0 then exit;
  // disable all components
  for i:=0 to ComponentCount-1 do begin
    if (Components[i] is TWinControl) then begin
      if ((GrabbingKey=1) and (Components[i]<>KeyGrabButton)
      and (Components[i]<>KeyGroupBox)) then
        TWinControl(Components[i]).Enabled:=false;
    end;
  end;
  if GrabbingKey=1 then
    KeyGrabButton.Caption:=srkmPressKey;
end;

procedure TExternalToolOptionDlg.FormKeyUp(Sender: TObject; var Key: Word;
  Shift:TShiftState);
begin
  //writeln('TExternalToolOptionDlg.FormKeyUp Sender=',Classname
  //   ,' Key=',Key,' Ctrl=',ssCtrl in Shift,' Shift=',ssShift in Shift
  //   ,' Alt=',ssAlt in Shift,' AsString=',KeyAndShiftStateToStr(Key,Shift)
  //   );
  if Key in [VK_CONTROL, VK_SHIFT, VK_LCONTROL, VK_RCONTROl,
             VK_LSHIFT, VK_RSHIFT] then exit;
  if (GrabbingKey in [1]) then begin
    if GrabbingKey=1 then begin
      KeyCtrlCheckBox.Checked:=(ssCtrl in Shift);
      KeyShiftCheckBox.Checked:=(ssShift in Shift);
      KeyAltCheckBox.Checked:=(ssAlt in Shift);
      SetComboBox(KeyComboBox,KeyAndShiftStateToEditorKeyString(Key,[]));
    end;
    DeactivateGrabbing;
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

procedure TExternalToolOptionDlg.OKButtonClick(Sender: TObject);
begin
  if (TitleEdit.Text='') or (FilenameEdit.Text='') then begin
    MessageDlg(lisEdtExtToolTitleAndFilenameRequired,
                  lisEdtExtToolAValidToolNeedsAtLeastATitleAndAFilename,
                  mtError, [mbCancel], 0);
    exit;
  end;
  SaveToOptions;
  ModalResult:=mrOk;
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

initialization
  {$I exttooleditdlg.lrs}

end.
