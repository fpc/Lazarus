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
    
    procedure PublishProjectDialogResize(Sender: TObject);
  private
    procedure SetComboBox(AComboBox: TComboBox; const NewText: string;
      MaxItemCount: integer);
    procedure LoadHistoryLists;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromOptions(SrcOpts: TPublishProjectOptions);
    procedure SaveToOptions(DestOpts: TPublishProjectOptions);
  end;

var
  PublishProjectDialog: TPublishProjectDialog;

implementation

{ TPublishProjectDialog }

procedure TPublishProjectDialog.PublishProjectDialogResize(Sender: TObject);
begin

end;

procedure TPublishProjectDialog.SetComboBox(AComboBox: TComboBox;
  const NewText: string; MaxItemCount: integer);
begin
  AComboBox.AddHistoryItem(NewText,MaxItemCount,true,false);
end;

procedure TPublishProjectDialog.LoadHistoryLists;
begin

end;

constructor TPublishProjectDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Position:=poScreenCenter;
  IDEDialogLayoutList.ApplyLayout(Self,500,460);
end;

destructor TPublishProjectDialog.Destroy;
begin
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

