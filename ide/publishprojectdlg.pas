{ /***************************************************************************
                 publishprojectdlg.pp  -  Lazarus IDE unit
                 -----------------------------------------

 ***************************************************************************/

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
    - TPublishProjectDialog
    The dialog for TPublishModuleOptions to publish projects and packages.

}
unit PublishProjectDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Buttons,
  StdCtrls, Dialogs, LCLType,
  IDEWindowIntf, IDEHelpIntf,
  ProjectDefs, PackageDefs, PublishModule, IDEOptionDefs, InputHistory,
  LazarusIDEStrConsts, ExtCtrls, ButtonPanel;

type
  { TPublishProjectDialog }

  TPublishProjectDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    DestDirGroupBox: TGroupBox;
    DestDirComboBox: TComboBox;
    BrowseDestDirBitBtn: TBitBtn;
    CommandAfterLabel: TLabel;
    CommandAfterCombobox: TComboBox;

    FilesGroupbox: TGroupBox;
    IgnoreBinariesCheckbox: TCheckBox;

    IncludeFilterCombobox: TComboBox;
    IncFilterSimpleSyntaxCheckbox: TCheckBox;
    UseIncludeFilterCheckbox: TCheckBox;
    IncludeFilterGroupbox: TGroupBox;

    ExcludeFilterCombobox: TComboBox;
    ExcFilterSimpleSyntaxCheckbox: TCheckBox;
    UseExcludeFilterCheckbox: TCheckBox;
    ExcludeFilterGroupbox: TGroupBox;

    ProjectInfoGroupbox: TGroupBox;
    SaveEditorInfoOfNonProjectFilesCheckbox: TCheckBox;
    SaveClosedEditorFilesInfoCheckbox: TCheckBox;

    procedure BrowseDestDirBitBtnCLICK(Sender: TObject);
    procedure DestDirGroupBoxRESIZE(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure OkButtonCLICK(Sender: TObject);
    procedure SaveSettingsButtonClick(Sender: TObject);
  private
    FOptions: TPublishModuleOptions;
    procedure SetComboBox(AComboBox: TComboBox; const NewText: string;
                          MaxItemCount: integer);
    procedure LoadHistoryLists;
    procedure SaveHistoryLists;
    procedure SetOptions(const AValue: TPublishModuleOptions);
    function CheckFilter: boolean;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromOptions(SrcOpts: TPublishModuleOptions);
    procedure SaveToOptions(DestOpts: TPublishModuleOptions);
    property Options: TPublishModuleOptions read FOptions write SetOptions;
  end;

function ShowPublishProjectDialog(
  PublishOptions: TPublishModuleOptions): TModalResult;


implementation

{$R *.lfm}

function ShowPublishProjectDialog(
  PublishOptions: TPublishModuleOptions): TModalResult;
var
  PublishProjectDialog: TPublishProjectDialog;
begin
  PublishProjectDialog:=TPublishProjectDialog.Create(nil);
  with PublishProjectDialog do begin
    Options:=PublishOptions;
    Result:=ShowModal;
    Free;
  end;
end;

{ TPublishProjectDialog }

procedure TPublishProjectDialog.DestDirGroupBoxRESIZE(Sender: TObject);
begin
  with DestDirComboBox do
    SetBounds(Left,Top,
              Parent.ClientWidth-2*Left-BrowseDestDirBitBtn.Width-5,Height);
  with BrowseDestDirBitBtn do
    Left:=DestDirComboBox.Left+DestDirComboBox.Width+5;
  with CommandAfterCombobox do
    SetBounds(Left,Top,Parent.ClientWidth-2*Left,Height);
end;

procedure TPublishProjectDialog.BrowseDestDirBitBtnCLICK(Sender: TObject);
var
  SelectDirDialog: TSelectDirectoryDialog;
  NewDir: String;
begin
  SelectDirDialog:=TSelectDirectoryDialog.Create(Self);
  InputHistories.ApplyFileDialogSettings(SelectDirDialog);
  SelectDirDialog.Title:=lisChooseDirectory;
  if SelectDirDialog.Execute then begin
    NewDir:=ExpandFileNameUTF8(SelectDirDialog.Filename);
    SeTComboBox(DestDirComboBox,NewDir,20);
  end;
  SelectDirDialog.Free;
end;

procedure TPublishProjectDialog.FormCreate(Sender: TObject);
begin
  DestDirGroupBox.Caption:=lisDestinationDirectory;
  CommandAfterLabel.Caption:=lisCommandAfter;

  FilesGroupbox.Caption:=dlgEnvFiles;
  IgnoreBinariesCheckbox.Caption:=lisIgnoreBinaries;

  IncFilterSimpleSyntaxCheckbox.Caption:=lisSimpleSyntax;
  IncFilterSimpleSyntaxCheckbox.Hint:=
    lisNormallyTheFilterIsARegularExpressionInSimpleSynta;
  UseIncludeFilterCheckbox.Caption:=lisUseIncludeFilter;
  IncludeFilterGroupbox.Caption:=lisIncludeFilter;

  ExcFilterSimpleSyntaxCheckbox.Caption:=lisSimpleSyntax;
  ExcFilterSimpleSyntaxCheckbox.Hint:=
    lisNormallyTheFilterIsARegularExpressionInSimpleSynta;
  UseExcludeFilterCheckbox.Caption:=lisUseExcludeFilter;
  ExcludeFilterGroupbox.Caption:=lisExcludeFilter;

  ProjectInfoGroupbox.Caption:=lisProjectInformation;
  SaveEditorInfoOfNonProjectFilesCheckbox.Caption:=
                                        lisSaveEditorInfoOfNonProjectFiles;
  SaveClosedEditorFilesInfoCheckbox.Caption:=lisSaveInfoOfClosedEditorFiles;

  ButtonPanel1.OkButton.Caption := lisLazBuildOk;
  ButtonPanel1.OKButton.OnClick := @OkButtonCLICK;

  ButtonPanel1.CloseButton.Caption := lisSaveSettings;
  ButtonPanel1.CloseButton.ModalResult := mrNone;
  ButtonPanel1.CloseButton.Kind := bkCustom;
  ButtonPanel1.CloseButton.LoadGlyphFromStock(idButtonSave);
  if ButtonPanel1.CloseButton.Glyph.Empty then
    ButtonPanel1.CloseButton.LoadGlyphFromLazarusResource('laz_save');
  ButtonPanel1.CloseButton.OnClick := @SaveSettingsButtonCLICK;

  ButtonPanel1.HelpButton.OnClick := @HelpButtonClick;
end;

procedure TPublishProjectDialog.HelpButtonClick(Sender: TObject);
begin
  LazarusHelp.ShowHelpForIDEControl(Self);
end;

procedure TPublishProjectDialog.OkButtonCLICK(Sender: TObject);
begin
  if not CheckFilter then exit;
  if Options<>nil then SaveToOptions(Options);
end;

procedure TPublishProjectDialog.SaveSettingsButtonClick(Sender: TObject);
begin
  if not CheckFilter then exit;
  if Options<>nil then SaveToOptions(Options);
end;

procedure TPublishProjectDialog.SetComboBox(AComboBox: TComboBox;
  const NewText: string; MaxItemCount: integer);
begin
  AComboBox.AddHistoryItem(NewText,MaxItemCount,true,false);
end;

procedure TPublishProjectDialog.LoadHistoryLists;
var
  List: THistoryList;
begin
  // destination directories
  List:=InputHistories.HistoryLists.GetList(hlPublishProjectDestDirs,true);
  List.AppendEntry(SetDirSeparators('$(TestDir)/publishedproject/'));
  List.AppendEntry(SetDirSeparators('$(TestDir)/publishedpackage/'));
  List.AppendEntry(SetDirSeparators('$(ProjPath)/published/'));
  DestDirComboBox.Items.Assign(List);
  
  // command after
  List:=InputHistories.HistoryLists.GetList(hlPublishProjectCommandsAfter,true);
  List.AppendEntry(SetDirSeparators(
                 'tar czf $MakeFile($(ProjPublishDir)).tgz $(ProjPublishDir)'));
  List.AppendEntry(SetDirSeparators(
              'tar czf $(TestDir)/project.tgz -C $(TestDir) publishedproject'));
  List.AppendEntry(SetDirSeparators(
              'tar czf $(TestDir)/package.tgz -C $(TestDir) publishedpackage'));
  CommandAfterCombobox.Items.Assign(List);

  // file filter
  List:=InputHistories.HistoryLists.GetList(hlPublishProjectIncludeFileFilter,
                                            true);
  if List.Count=0 then begin
    List.Add(DefPublProjIncFilter);
  end;
  IncludeFilterCombobox.Items.Assign(List);

  List:=InputHistories.HistoryLists.GetList(hlPublishProjectExcludeFileFilter,
                                            true);
  if List.Count=0 then begin
    List.Add(DefPublProjExcFilter);
  end;
  ExcludeFilterCombobox.Items.Assign(List);
end;

procedure TPublishProjectDialog.SaveHistoryLists;
begin
  // destination directories
  SetComboBox(DestDirComboBox,DestDirComboBox.Text,20);
  InputHistories.HistoryLists.GetList(hlPublishProjectDestDirs,true).Assign(
    DestDirComboBox.Items);
    
  // command after
  SetComboBox(CommandAfterCombobox,CommandAfterCombobox.Text,20);
  InputHistories.HistoryLists.GetList(hlPublishProjectCommandsAfter,true).Assign(
    CommandAfterCombobox.Items);

  // file filter
  SetComboBox(IncludeFilterCombobox,IncludeFilterCombobox.Text,20);
  InputHistories.HistoryLists.GetList(hlPublishProjectIncludeFileFilter,true).Assign(
    IncludeFilterCombobox.Items);
  SetComboBox(ExcludeFilterCombobox,ExcludeFilterCombobox.Text,20);
  InputHistories.HistoryLists.GetList(hlPublishProjectExcludeFileFilter,true).Assign(
    ExcludeFilterCombobox.Items);
end;

procedure TPublishProjectDialog.SetOptions(const AValue: TPublishModuleOptions);
begin
  if FOptions=AValue then exit;
  FOptions:=AValue;
  if FOptions is TPublishPackageOptions then
    Caption:=lisPkgEditPublishPackage
  else
    Caption:=lisPublishProject;
  LoadFromOptions(FOptions);
end;

function TPublishProjectDialog.CheckFilter: boolean;
begin
  Result:=false;
  if Options<>nil then begin
    if not Options.IncludeFilterValid then begin
      if MessageDlg(lisPublProjInvalidIncludeFilter, mtError, [mbIgnore,
        mbCancel], 0)
        =mrCancel
      then exit;
    end;
    if not Options.ExcludeFilterValid then begin
      if MessageDlg(lisPublProjInvalidExcludeFilter, mtError, [mbIgnore,
        mbCancel], 0)
        =mrCancel
      then exit;
    end;
  end;
  Result:=true;
end;

constructor TPublishProjectDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Position:=poScreenCenter;
  IDEDialogLayoutList.ApplyLayout(Self,500,500);
  if Height<500 then Height:=500;
  LoadHistoryLists;
end;

destructor TPublishProjectDialog.Destroy;
begin
  SaveHistoryLists;
  inherited Destroy;
end;

procedure TPublishProjectDialog.LoadFromOptions(SrcOpts: TPublishModuleOptions);
var
  ProjSrcOpts: TPublishProjectOptions;
begin
  // destination
  SeTComboBox(DestDirComboBox,SrcOpts.DestinationDirectory,20);
  SeTComboBox(CommandAfterCombobox,SrcOpts.CommandAfter,20);

  // file filter
  IgnoreBinariesCheckbox.Checked:=SrcOpts.IgnoreBinaries;
  UseIncludeFilterCheckbox.Checked:=SrcOpts.UseIncludeFileFilter;
  IncFilterSimpleSyntaxCheckbox.Checked:=SrcOpts.IncludeFilterSimpleSyntax;
  SeTComboBox(IncludeFilterCombobox,SrcOpts.IncludeFileFilter,20);
  UseExcludeFilterCheckbox.Checked:=SrcOpts.UseExcludeFileFilter;
  ExcFilterSimpleSyntaxCheckbox.Checked:=SrcOpts.ExcludeFilterSimpleSyntax;
  SeTComboBox(ExcludeFilterCombobox,SrcOpts.ExcludeFileFilter,20);

  // project info
  if SrcOpts is TPublishProjectOptions then begin
    ProjSrcOpts:=TPublishProjectOptions(SrcOpts);
    SaveEditorInfoOfNonProjectFilesCheckbox.Checked:=
      ProjSrcOpts.SaveEditorInfoOfNonProjectFiles;
    SaveClosedEditorFilesInfoCheckbox.Checked:=
      ProjSrcOpts.SaveClosedEditorFilesInfo;
    ProjectInfoGroupbox.Enabled:=true;
  end else begin
    ProjectInfoGroupbox.Enabled:=false;
  end;
end;

procedure TPublishProjectDialog.SaveToOptions(DestOpts: TPublishModuleOptions);
var
  ProjDestOpts: TPublishProjectOptions;
begin
  // destination
  DestOpts.DestinationDirectory:=DestDirComboBox.Text;
  DestOpts.CommandAfter:=CommandAfterCombobox.Text;
  
  // file filter
  DestOpts.IgnoreBinaries:=IgnoreBinariesCheckbox.Checked;
  DestOpts.UseIncludeFileFilter:=UseIncludeFilterCheckbox.Checked;
  DestOpts.IncludeFilterSimpleSyntax:=IncFilterSimpleSyntaxCheckbox.Checked;
  DestOpts.IncludeFileFilter:=IncludeFilterCombobox.Text;
  DestOpts.UseExcludeFileFilter:=UseExcludeFilterCheckbox.Checked;
  DestOpts.ExcludeFilterSimpleSyntax:=ExcFilterSimpleSyntaxCheckbox.Checked;
  DestOpts.ExcludeFileFilter:=ExcludeFilterCombobox.Text;
  
  // project info
  if DestOpts is TPublishProjectOptions then begin
    ProjDestOpts:=TPublishProjectOptions(DestOpts);
    ProjDestOpts.SaveEditorInfoOfNonProjectFiles:=
      SaveEditorInfoOfNonProjectFilesCheckbox.Checked;
    ProjDestOpts.SaveClosedEditorFilesInfo:=
      SaveClosedEditorFilesInfoCheckbox.Checked;
  end;
end;

end.

