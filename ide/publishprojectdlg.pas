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
  Classes, SysUtils, Forms, Controls, Graphics, LResources, Buttons, StdCtrls,
  ProjectDefs, PackageDefs, PublishModule, IDEOptionDefs, InputHistory, Dialogs,
  LazarusIDEStrConsts;

type
  { TPublishProjectDialog }

  TPublishProjectDialog = class(TForm)
    DestDirGroupBox: TGROUPBOX;
    DestDirComboBox: TCOMBOBOX;
    BrowseDestDirBitBtn: TBITBTN;
    CommandAfterLabel: TLABEL;
    CommandAfterCombobox: TCOMBOBOX;

    FilesGroupbox: TGROUPBOX;
    IgnoreBinariesCheckbox: TCHECKBOX;

    ExcludeFilterCombobox: TCOMBOBOX;
    ExcFilterSimpleSyntaxCheckbox: TCHECKBOX;
    UseExcludeFilterCheckbox: TCHECKBOX;
    ExcludeFilterGroupbox: TGROUPBOX;

    IncludeFilterCombobox: TCOMBOBOX;
    IncFilterSimpleSyntaxCheckbox: TCHECKBOX;
    UseIncludeFilterCheckbox: TCHECKBOX;
    IncludeFilterGroupbox: TGROUPBOX;

    ProjectInfoGroupbox: TGROUPBOX;
    SaveEditorInfoOfNonProjectFilesCheckbox: TCHECKBOX;
    SaveClosedEditorFilesInfoCheckbox: TCHECKBOX;

    OkButton: TBUTTON;
    SaveSettingsButton: TBUTTON;
    CancelButton: TBUTTON;
    procedure BrowseDestDirBitBtnCLICK(Sender: TObject);
    procedure DestDirGroupBoxRESIZE(Sender: TObject);
    procedure ExcludeFilterGroupboxRESIZE(Sender: TObject);
    procedure FilesGroupboxRESIZE(Sender: TObject);
    procedure IncludeFilterGroupboxRESIZE(Sender: TObject);
    procedure OkButtonCLICK(Sender: TObject);
    procedure ProjectInfoGroupboxResize(Sender: TObject);
    procedure PublishProjectDialogResize(Sender: TObject);
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
  OpenDialog: TOpenDialog;
  NewDir: String;
begin
  OpenDialog:=TOpenDialog.Create(Self);
  InputHistories.ApplyFileDialogSettings(OpenDialog);
  OpenDialog.Title:=lisChooseDirectory;
  if OpenDialog.Execute then begin
    NewDir:=ExpandFilename(OpenDialog.Filename);
    SetComboBox(DestDirComboBox,NewDir,20);
  end;
  OpenDialog.Free;
end;

procedure TPublishProjectDialog.ExcludeFilterGroupboxRESIZE(Sender: TObject);
begin
end;

procedure TPublishProjectDialog.FilesGroupboxRESIZE(Sender: TObject);
begin
end;

procedure TPublishProjectDialog.IncludeFilterGroupboxRESIZE(Sender: TObject);
begin
end;

procedure TPublishProjectDialog.OkButtonCLICK(Sender: TObject);
begin
  if not CheckFilter then exit;
  if Options<>nil then SaveToOptions(Options);
end;

procedure TPublishProjectDialog.ProjectInfoGroupboxResize(Sender: TObject);
begin
end;

procedure TPublishProjectDialog.PublishProjectDialogResize(Sender: TObject);
begin
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

procedure TPublishProjectDialog.SetOptions(const AValue: TPublishModuleOptions
  );
begin
  if FOptions=AValue then exit;
  FOptions:=AValue;
  if FOptions is TPublishPackageOptions then
    Caption:=lisPkgEditPublishPackage
  else
    Caption:=lisMenuPublishProject;
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
  IDEDialogLayoutList.ApplyLayout(Self,500,470);
  LoadHistoryLists;
end;

destructor TPublishProjectDialog.Destroy;
begin
  SaveHistoryLists;
  inherited Destroy;
end;

procedure TPublishProjectDialog.LoadFromOptions(SrcOpts: TPublishModuleOptions
  );
var
  ProjSrcOpts: TPublishProjectOptions;
begin
  // destination
  SetComboBox(DestDirComboBox,SrcOpts.DestinationDirectory,20);
  SetComboBox(CommandAfterCombobox,SrcOpts.CommandAfter,20);

  // file filter
  IgnoreBinariesCheckbox.Checked:=SrcOpts.IgnoreBinaries;
  UseIncludeFilterCheckbox.Checked:=SrcOpts.UseIncludeFileFilter;
  IncFilterSimpleSyntaxCheckbox.Checked:=SrcOpts.IncludeFilterSimpleSyntax;
  SetComboBox(IncludeFilterCombobox,SrcOpts.IncludeFileFilter,20);
  UseExcludeFilterCheckbox.Checked:=SrcOpts.UseExcludeFileFilter;
  ExcFilterSimpleSyntaxCheckbox.Checked:=SrcOpts.ExcludeFilterSimpleSyntax;
  SetComboBox(ExcludeFilterCombobox,SrcOpts.ExcludeFileFilter,20);

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

procedure TPublishProjectDialog.SaveToOptions(DestOpts: TPublishModuleOptions
  );
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

initialization
  {$I publishprojectdlg.lrs}

end.

