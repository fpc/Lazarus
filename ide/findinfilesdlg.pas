{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Find in files dialog form.

}
unit FindInFilesDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LCLIntf, Controls, StdCtrls, Forms, Buttons,
  ExtCtrls, LResources, FileUtil, LazarusIDEStrConsts, Dialogs, SynEditTypes,
  IDEDialogs, IDEWindowIntf, InputHistory;

type
  { TLazFindInFilesDialog }

  TLazFindInFilesDialog = class(TForm)
    ReplaceCheckBox: TCheckBox;
    ReplaceTextComboBox: TComboBox;
    IncludeSubDirsCheckBox: TCheckBox;
    FileMaskComboBox: TComboBox;
    DirectoryBrowse: TBitBtn;
    DirectoryComboBox: TComboBox;
    DirectoryLabel: TLabel;
    FileMaskLabel: TLabel;
    OKButton: TButton;
    CancelButton: TButton;
    DirectoryOptionsGroupBox: TGroupBox;
    OptionsCheckGroupBox: TCheckGroup;
    SelectDirectoryDialog: TSelectDirectoryDialog;
    TextToFindComboBox: TComboBox;
    TextToFindLabel: TLabel;
    WhereRadioGroup: TRadioGroup;
    procedure DirectoryBrowseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure ReplaceCheckBoxChange(Sender: TObject);
    procedure WhereRadioGroupClick(Sender: TObject);
  private
    function GetFindText: string;
    function GetOptions: TLazFindInFileSearchOptions;
    function GetReplaceText: string;
    function GetSynOptions: TSynSearchOptions;
    procedure SetFindText(const NewFindText: string);
    procedure SetOptions(NewOptions: TLazFindInFileSearchOptions);
    procedure SetReplaceText(const AValue: string);
    procedure SetSynOptions(NewOptions: TSynSearchOptions);
    procedure UpdateReplaceCheck;
  public
    property Options: TLazFindInFileSearchOptions read GetOptions
                                                  write SetOptions;
    property FindText: string read GetFindText write SetFindText;
    property ReplaceText: string read GetReplaceText write SetReplaceText;
    property SynSearchOptions: TSynSearchOptions read GetSynOptions
                                                 write SetSynOptions;
  end;


var FindInFilesDialog: TLazFindInFilesDialog;


implementation

{ TLazFindInFilesDialog }

procedure TLazFindInFilesDialog.SetFindText(const NewFindText: string);
begin
  TextToFindComboBox.Text := NewFindText;
  TextToFindComboBox.SelectAll;
  ActiveControl := TextToFindComboBox;
end;

function TLazFindInFilesDialog.GetFindText: string;
begin
  Result := TextToFindComboBox.Text;
end;

function TLazFindInFilesDialog.GetReplaceText: string;
begin
  Result:=ReplaceTextComboBox.Text;
end;

procedure TLazFindInFilesDialog.WhereRadioGroupClick(Sender: TObject);
begin
  DirectoryOptionsGroupBox.Enabled := (WhereRadioGroup.ItemIndex = 2)
end;

procedure TLazFindInFilesDialog.DirectoryBrowseClick(Sender: TObject);
begin
  InitIDEFileDialog(SelectDirectoryDialog);
  if DirectoryExists(DirectoryComboBox.Text) then
   SelectDirectoryDialog.InitialDir := DirectoryComboBox.Text else
   SelectDirectoryDialog.InitialDir := GetCurrentDir;
  if SelectDirectoryDialog.Execute then
    DirectoryComboBox.Text := SelectDirectoryDialog.FileName;
  StoreIDEFileDialog(SelectDirectoryDialog);
end;

procedure TLazFindInFilesDialog.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TLazFindInFilesDialog.FormCreate(Sender: TObject);
begin
  Caption := srkmecFindInFiles;

  TextToFindLabel.Caption := lisFindFileTextToFind;
  ReplaceCheckBox.Caption := lisMenuReplace;

  OptionsCheckGroupBox.Caption := dlgFROpts;
  OptionsCheckGroupBox.Items[0] := lisFindFileCaseSensitive;
  OptionsCheckGroupBox.Items[1] := lisFindFileWholeWordsOnly;
  OptionsCheckGroupBox.Items[2] := lisFindFileRegularExpressions;
  OptionsCheckGroupBox.Items[3] := lisFindFileMultiLine;

  WhereRadioGroup.Caption:=lisFindFileWhere;
  WhereRadioGroup.Items[0] := lisFindFilesearchAllFilesInProject;
  WhereRadioGroup.Items[1] := lisFindFilesearchAllOpenFiles;
  WhereRadioGroup.Items[2] := lisFindFilesearchInDirectories;

  DirectoryOptionsGroupBox.Caption := lisFindFileDirectoryOptions;
  DirectoryLabel.Caption := lisCodeToolsDefsInsertBehindDirectory;
  FileMaskLabel.Caption := lisFindFileFileMaskBak;

  IncludeSubDirsCheckBox.Caption := lisFindFileIncludeSubDirectories;

  OkButton.Caption := lisLazBuildOk;
  CancelButton.Caption := dlgCancel;
  
  ReplaceCheckBox.Enabled:=true;

  UpdateReplaceCheck;
  DirectoryOptionsGroupBox.Enabled:=WhereRadioGroup.ItemIndex=2;

  IDEDialogLayoutList.ApplyLayout(Self,420,460);
end;

procedure TLazFindInFilesDialog.ReplaceCheckBoxChange(Sender: TObject);
begin
  UpdateReplaceCheck;
end;

procedure TLazFindInFilesDialog.SetOptions(NewOptions: TLazFindInFileSearchOptions);
begin
  OptionsCheckGroupBox.Checked[0] := fifMatchCase in NewOptions;
  OptionsCheckGroupBox.Checked[1] := fifWholeWord in NewOptions;
  OptionsCheckGroupBox.Checked[2] := fifRegExpr in NewOptions;
  OptionsCheckGroupBox.Checked[3] := fifMultiLine in NewOptions;
  DirectoryOptionsGroupBox.Enabled := fifSearchDirectories in NewOptions;
  IncludeSubDirsCheckBox.Checked := fifIncludeSubDirs in NewOptions;
  ReplaceCheckBox.Checked := [fifReplace,fifReplaceAll]*NewOptions<>[];
  
  if fifSearchProject in NewOptions then WhereRadioGroup.ItemIndex := 0;
  if fifSearchOpen in NewOptions then WhereRadioGroup.ItemIndex := 1;
  if fifSearchDirectories in NewOptions then WhereRadioGroup.ItemIndex := 2;

  UpdateReplaceCheck;
end;

function TLazFindInFilesDialog.GetOptions: TLazFindInFileSearchOptions;
begin
  Result := [];
  if OptionsCheckGroupBox.Checked[0] then Include(Result, fifMatchCase);
  if OptionsCheckGroupBox.Checked[1] then Include(Result, fifWholeWord);
  if OptionsCheckGroupBox.Checked[2] then Include(Result, fifRegExpr);
  if OptionsCheckGroupBox.Checked[3] then Include(Result, fifMultiLine);
  if IncludeSubDirsCheckBox.Checked then Include(Result, fifIncludeSubDirs);
  if ReplaceCheckBox.Checked then Include(Result, fifReplace);

  case WhereRadioGroup.ItemIndex of
    0: Include(Result, fifSearchProject);
    1: Include(Result, fifSearchOpen);
    2: Include(Result, fifSearchDirectories);
  end;//case
end;

function TLazFindInFilesDialog.GetSynOptions: TSynSearchOptions;
begin
  Result := [];
  if OptionsCheckGroupBox.Checked[0] then Include(Result, ssoMatchCase);
  if OptionsCheckGroupBox.Checked[1] then Include(Result, ssoWholeWord);
  if OptionsCheckGroupBox.Checked[2] then Include(Result, ssoRegExpr);
  if OptionsCheckGroupBox.Checked[3] then Include(Result, ssoRegExprMultiLine);
  if ReplaceCheckBox.Checked then Include(Result, ssoReplace);
end;//GetSynOptions

procedure TLazFindInFilesDialog.SetReplaceText(const AValue: string);
begin
  ReplaceTextComboBox.Text := AValue;
end;

procedure TLazFindInFilesDialog.SetSynOptions(NewOptions: TSynSearchOptions);
begin
  OptionsCheckGroupBox.Checked[0] := ssoMatchCase in NewOptions;
  OptionsCheckGroupBox.Checked[1] := ssoWholeWord in NewOptions;
  OptionsCheckGroupBox.Checked[2] := ssoRegExpr in NewOptions;
  OptionsCheckGroupBox.Checked[3] := ssoRegExprMultiLine in NewOptions;
  ReplaceCheckBox.Checked := ([ssoReplace,ssoReplaceAll]*NewOptions <> []);

  UpdateReplaceCheck;
end;//SetSynOptions

procedure TLazFindInFilesDialog.UpdateReplaceCheck;
begin
  ReplaceTextComboBox.Enabled:=ReplaceCheckBox.Checked;
  if ReplaceCheckBox.Checked then
    OKButton.Caption:=lisMenuReplace
  else
    OKButton.Caption:=lisMenuFind;
end;

initialization
  {$I findinfilesdlg.lrs}
  FindInFilesDialog := nil;

end.

