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

}
unit PublishProjectDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, LResources, Buttons, StdCtrls,
  ProjectDefs, IDEOptionDefs, IDEProcs, InputHistory;

type
  { TPublishProjectDialog }

  TPublishProjectDialog = class(TForm)
    DestDirGroupBox: TGROUPBOX;
    DestDirComboBox: TCOMBOBOX;
    BrowseDestDirBitBtn: TBITBTN;
    CommandAfterLabel: TLABEL;
    CommandAfterCombobox: TCOMBOBOX;

    FilesGroupbox: TGROUPBOX;
    UseIncludeFilterCheckbox: TCHECKBOX;
    IncludeFileFilterCombobox: TCOMBOBOX;
    UseExcludeFilterCheckbox: TCHECKBOX;
    ExcludeFileFilterCombobox: TCOMBOBOX;

    ProjectInfoGroupbox: TGROUPBOX;
    SaveEditorInfoOfNonProjectFilesCheckbox: TCHECKBOX;
    SaveClosedEditorFilesInfoCheckbox: TCHECKBOX;

    OkButton: TBUTTON;
    SaveSettingsButton: TBUTTON;
    CancelButton: TBUTTON;
    procedure DestDirGroupBoxRESIZE(Sender: TObject);
    procedure FilesGroupboxRESIZE(Sender: TObject);
    procedure OkButtonCLICK(Sender: TObject);
    procedure ProjectInfoGroupboxResize(Sender: TObject);
    procedure PublishProjectDialogResize(Sender: TObject);
    procedure SaveSettingsButtonClick(Sender: TObject);
  private
    FOptions: TPublishProjectOptions;
    procedure SetComboBox(AComboBox: TComboBox; const NewText: string;
      MaxItemCount: integer);
    procedure LoadHistoryLists;
    procedure SaveHistoryLists;
    procedure SetOptions(const AValue: TPublishProjectOptions);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromOptions(SrcOpts: TPublishProjectOptions);
    procedure SaveToOptions(DestOpts: TPublishProjectOptions);
    property Options: TPublishProjectOptions read FOptions write SetOptions;
  end;

function ShowPublishProjectDialog(
  PublishOptions: TPublishProjectOptions): TModalResult;


implementation


function ShowPublishProjectDialog(
  PublishOptions: TPublishProjectOptions): TModalResult;
var
  PublishProjectDialog: TPublishProjectDialog;
begin
  PublishProjectDialog:=TPublishProjectDialog.Create(Application);
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

procedure TPublishProjectDialog.FilesGroupboxRESIZE(Sender: TObject);
begin
  with FilesGroupbox do
    SetBounds(Left,Top,Parent.ClientWidth-2*Left,Height);
  with CommandAfterLabel do
    SetBounds(Left,Top,Parent.ClientWidth-2*Left,Height);
  with UseIncludeFilterCheckbox do
    SetBounds(Left,Top,Parent.ClientWidth-2*Left,Height);
  with IncludeFileFilterCombobox do
    SetBounds(Left,Top,Parent.ClientWidth-2*Left,Height);
  with UseExcludeFilterCheckbox do
    SetBounds(Left,Top,Parent.ClientWidth-2*Left,Height);
  with ExcludeFileFilterCombobox do
    SetBounds(Left,Top,Parent.ClientWidth-2*Left,Height);
end;

procedure TPublishProjectDialog.OkButtonCLICK(Sender: TObject);
begin
  if Options<>nil then SaveToOptions(Options);
end;

procedure TPublishProjectDialog.ProjectInfoGroupboxResize(Sender: TObject);
begin
  with SaveEditorInfoOfNonProjectFilesCheckbox do
    SetBounds(Left,Top,Parent.ClientWidth-Left,Height);
  with SaveClosedEditorFilesInfoCheckbox do
    SetBounds(Left,Top,Parent.ClientWidth-Left,Height);
end;

procedure TPublishProjectDialog.PublishProjectDialogResize(Sender: TObject);
begin
  with DestDirGroupBox do
    SetBounds(Left,Top,Parent.ClientWidth-2*Left,Height);
  with FilesGroupbox do
    SetBounds(Left,Top,Parent.ClientWidth-2*Left,Height);
  with ProjectInfoGroupbox do
    SetBounds(Left,Top,Parent.ClientWidth-2*Left,Height);
end;

procedure TPublishProjectDialog.SaveSettingsButtonClick(Sender: TObject);
begin
  if Options<>nil then
    SaveToOptions(Options);
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
  List:=InputHistories.HistoryLists.GetList(PublishProjectDestDirs,true);
  if List.Count=0 then begin
    List.Add('$(TestDir)/publishedproject/');
    List.Add('$(ProjectDir)/published/');
  end;
  DestDirComboBox.Items.Assign(List);
  
  // command after
  List:=InputHistories.HistoryLists.GetList(PublishProjectCommandsAfter,true);
  if List.Count=0 then begin
    List.Add('tar czf $(ProjPublishDir).tgz $(ProjPublishDir)');
  end;
  CommandAfterCombobox.Items.Assign(List);

  // file filter
  List:=InputHistories.HistoryLists.GetList(PublishProjectIncludeFileFilter,true);
  if List.Count=0 then begin
    List.Add('*.{pas,pp,inc,lfm,lpr,lrs,lpi,lpk,fpc,sh,xml}');
  end;
  IncludeFileFilterCombobox.Items.Assign(List);

  List:=InputHistories.HistoryLists.GetList(PublishProjectExcludeFileFilter,true);
  if List.Count=0 then begin
    List.Add('*.{bak,ppu,ppw,o,so};*~;backup');
  end;
  ExcludeFileFilterCombobox.Items.Assign(List);
end;

procedure TPublishProjectDialog.SaveHistoryLists;
begin
  // destination directories
  SetComboBox(DestDirComboBox,DestDirComboBox.Text,20);
  InputHistories.HistoryLists.GetList(PublishProjectDestDirs,true).Assign(
    DestDirComboBox.Items);
    
  // command after
  SetComboBox(CommandAfterCombobox,CommandAfterCombobox.Text,20);
  InputHistories.HistoryLists.GetList(PublishProjectCommandsAfter,true).Assign(
    CommandAfterCombobox.Items);

  // file filter
  SetComboBox(IncludeFileFilterCombobox,IncludeFileFilterCombobox.Text,20);
  InputHistories.HistoryLists.GetList(PublishProjectIncludeFileFilter,true).Assign(
    IncludeFileFilterCombobox.Items);
  SetComboBox(ExcludeFileFilterCombobox,ExcludeFileFilterCombobox.Text,20);
  InputHistories.HistoryLists.GetList(PublishProjectExcludeFileFilter,true).Assign(
    ExcludeFileFilterCombobox.Items);
end;

procedure TPublishProjectDialog.SetOptions(const AValue: TPublishProjectOptions
  );
begin
  if FOptions=AValue then exit;
  FOptions:=AValue;
  LoadFromOptions(FOptions);
end;

constructor TPublishProjectDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Position:=poScreenCenter;
  IDEDialogLayoutList.ApplyLayout(Self,500,405);
  LoadHistoryLists;
end;

destructor TPublishProjectDialog.Destroy;
begin
  SaveHistoryLists;
  inherited Destroy;
end;

procedure TPublishProjectDialog.LoadFromOptions(SrcOpts: TPublishProjectOptions
  );
begin
  // destination
  SetComboBox(DestDirComboBox,SrcOpts.DestinationDirectory,20);
  SetComboBox(CommandAfterCombobox,SrcOpts.CommandAfter,20);

  // file filter
  UseIncludeFilterCheckbox.Checked:=SrcOpts.UseIncludeFileFilter;
  SetComboBox(IncludeFileFilterCombobox,SrcOpts.IncludeFileFilter,20);
  UseExcludeFilterCheckbox.Checked:=SrcOpts.UseExcludeFileFilter;
  SetComboBox(ExcludeFileFilterCombobox,SrcOpts.ExcludeFileFilter,20);

  // project info
  SaveEditorInfoOfNonProjectFilesCheckbox.Checked:=
    SrcOpts.SaveEditorInfoOfNonProjectFiles;
  SaveClosedEditorFilesInfoCheckbox.Checked:=
    SrcOpts.SaveClosedEditorFilesInfo;
end;

procedure TPublishProjectDialog.SaveToOptions(DestOpts: TPublishProjectOptions
  );
begin
  // destination
  DestOpts.DestinationDirectory:=DestDirComboBox.Text;
  DestOpts.CommandAfter:=CommandAfterCombobox.Text;
  
  // file filter
  DestOpts.UseIncludeFileFilter:=UseIncludeFilterCheckbox.Checked;
  DestOpts.IncludeFileFilter:=IncludeFileFilterCombobox.Text;
  DestOpts.UseExcludeFileFilter:=UseExcludeFilterCheckbox.Checked;
  DestOpts.ExcludeFileFilter:=ExcludeFileFilterCombobox.Text;
  
  // project info
  DestOpts.SaveEditorInfoOfNonProjectFiles:=
    SaveEditorInfoOfNonProjectFilesCheckbox.Checked;
  DestOpts.SaveClosedEditorFilesInfo:=
    SaveClosedEditorFilesInfoCheckbox.Checked;
end;

initialization
  {$I publishprojectdlg.lrs}

end.

