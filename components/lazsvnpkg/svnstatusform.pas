{ Copyright (C) 2008 Darius Blaszijk

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

unit SVNStatusForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, ButtonPanel, ExtCtrls, LCLProc, Process,
  SVNClasses, Menus;

type
  { TSVNStatusFrm }

  TSVNStatusFrm = class(TForm)
    ButtonPanel: TButtonPanel;
    ImageList: TImageList;
    mnuRevert: TMenuItem;
    mnuShowDiff: TMenuItem;
    PopupMenu1: TPopupMenu;
    Splitter: TSplitter;
    SVNCommitMsgMemo: TMemo;
    SVNFileListView: TListView;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mnuRevertClick(Sender: TObject);
    procedure mnuShowDiffClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure SVNFileListViewColumnClick(Sender: TObject; Column: TListColumn);
    procedure SVNFileListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    FRepositoryPath: string;
    { private declarations }
    SVNStatus: TSVNStatus;
    procedure SetRepositoryPath(const AValue: string);
    procedure UpdateFilesListView(Data: PtrInt);
    procedure ChangeCursor(ACursor: TCursor);
  public
    { public declarations }
    property RepositoryPath: string read FRepositoryPath write SetRepositoryPath;
  end;

procedure ShowSVNStatusFrm(ARepoPath: string);

implementation

uses
  SVNDiffForm, SVNCommitForm;

procedure ShowSVNStatusFrm(ARepoPath: string);
var
  SVNStatusFrm: TSVNStatusFrm;
begin
  SVNStatusFrm := TSVNStatusFrm.Create(nil);

  SVNStatusFrm.RepositoryPath:=ARepoPath;
  SVNStatusFrm.ShowModal;

  SVNStatusFrm.Free;
end;

{ TSVNStatusFrm }

procedure TSVNStatusFrm.FormShow(Sender: TObject);
begin
  SVNStatus := TSVNStatus.Create(RepositoryPath);
  SVNStatus.Sort(siChecked, sdAscending);

  Caption := Format('%s - %s...', [RepositoryPath, rsLazarusSVNCommit]);
  Application.QueueAsyncCall(@UpdateFilesListView, 0);
end;

procedure TSVNStatusFrm.mnuRevertClick(Sender: TObject);
var
  AProcess: TProcess;
begin
  if Assigned(SVNFileListView.Selected) then
  begin

    AProcess := TProcess.Create(nil);

    if pos(RepositoryPath,SVNFileListView.Selected.SubItems[0]) <> 0 then
      AProcess.CommandLine := SVNExecutable + ' revert "' + SVNFileListView.Selected.SubItems[0] + '"'
    else
      AProcess.CommandLine := SVNExecutable + ' revert "' + AppendPathDelim(RepositoryPath) + SVNFileListView.Selected.SubItems[0] + '"';

    debugln('TSVNStatusFrm.mnuRevertClick commandline=', AProcess.CommandLine);
    AProcess.Options := AProcess.Options + [poWaitOnExit];
    AProcess.ShowWindow := swoHIDE;
    AProcess.Execute;
    AProcess.Free;

    //now delete the entry from the list
    SVNStatus.List.Delete(SVNFileListView.Selected.Index);

    //update the listview again
    UpdateFilesListView(0);
  end;
end;

procedure TSVNStatusFrm.mnuShowDiffClick(Sender: TObject);
begin
  {$note implement opening file in source editor}
  if Assigned(SVNFileListView.Selected) then
  begin
    debugln('TSVNStatusFrm.mnuShowDiffClick Path=' ,SVNFileListView.Selected.SubItems[0]);

    if pos(RepositoryPath,SVNFileListView.Selected.SubItems[0]) <> 0 then
      ShowSVNDiffFrm('-r HEAD', '"' + SVNFileListView.Selected.SubItems[0] + '"')
    else
      ShowSVNDiffFrm('-r HEAD', '"' + AppendPathDelim(RepositoryPath) + SVNFileListView.Selected.SubItems[0] + '"');
  end;
end;

procedure TSVNStatusFrm.OKButtonClick(Sender: TObject);
var
  i: integer;
  CmdLine: string;
  StatusItem : PSVNStatusItem;
  FileName: string;
begin
  if SVNCommitMsgMemo.Text = '' then
    if MessageDlg ('No message set.', 'Do you wish to continue?', mtConfirmation,
                  [mbYes, mbNo],0) <> mrYes then
      exit;

  //commit the checked files
  CmdLine := SVNExecutable + ' commit --force-log ';

  for i := 0 to SVNStatus.List.Count - 1 do
  begin
    StatusItem := PSVNStatusItem(SVNStatus.List.Items[i]);

    if StatusItem^.Checked then
      if pos(RepositoryPath,StatusItem^.Path) = 0 then
        CmdLine := CmdLine + ' "' + AppendPathDelim(RepositoryPath) + StatusItem^.Path + '"'
      else
        CmdLine := CmdLine + ' "' + StatusItem^.Path + '"';
  end;

  FileName := GetTempFileName('','');
  SVNCommitMsgMemo.Lines.SaveToFile(FileName);
  CmdLine := CmdLine + ' --file ' + FileName;

  ShowSVNCommitFrm(CmdLine);
  DeleteFile(FileName);
end;

procedure TSVNStatusFrm.SVNFileListViewColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  case Column.Index of
    0: SVNStatus.ReverseSort(siChecked);
    1: SVNStatus.ReverseSort(siPath);
    2: SVNStatus.ReverseSort(siExtension);
    3: SVNStatus.ReverseSort(siItemStatus);
    4: SVNStatus.ReverseSort(siPropStatus);
    5: SVNStatus.ReverseSort(siAuthor);
    6: SVNStatus.ReverseSort(siRevision);
    7: SVNStatus.ReverseSort(siCommitRevision);
    8: SVNStatus.ReverseSort(siDate);
  end;

  UpdateFilesListView(0);
end;

procedure TSVNStatusFrm.SVNFileListViewSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  PSVNStatusItem(SVNStatus.List.Items[Item.Index])^.Checked:=Item.Checked;
end;

procedure TSVNStatusFrm.UpdateFilesListView(Data: PtrInt);
var
  i: integer;
  StatusItem : PSVNStatusItem;
  Path: string;
begin
  SVNFileListView.BeginUpdate;
  SVNFileListView.Clear;

  for i := 0 to SVNStatus.List.Count - 1 do
  begin

    with SVNFileListView.Items.Add do
    begin
      StatusItem := PSVNStatusItem(SVNStatus.List.Items[i]);

      //checkboxes
      Caption := '';
      Checked := StatusItem^.Checked;

      //path
      Path := StatusItem^.Path;
      if pos(RepositoryPath, Path) = 1 then
        System.Delete(Path, 1, Length(RepositoryPath) - 1);
      SubItems.Add(Path);

      //extension
      SubItems.Add(StatusItem^.Extension);

      //file status
      SubItems.Add(StatusItem^.ItemStatus);

      //property status
      SubItems.Add(StatusItem^.PropStatus);

      //check if file is versioned
      if (LowerCase(StatusItem^.ItemStatus) <> 'unversioned') and
         (LowerCase(StatusItem^.ItemStatus) <> 'added') then
      begin
        //revision
        SubItems.Add(IntToStr(StatusItem^.Revision));

        //commit revision
        SubItems.Add(IntToStr(StatusItem^.CommitRevision));

        //author
        SubItems.Add(StatusItem^.Author);

        //date
        SubItems.Add(DateTimeToStr(StatusItem^.Date));
      end;
    end;
  end;
  SVNFileListView.EndUpdate;

  ChangeCursor(crDefault);
end;

procedure TSVNStatusFrm.SetRepositoryPath(const AValue: string);
begin
  FRepositoryPath := AppendPathDelim(AValue);
end;

procedure TSVNStatusFrm.ChangeCursor(ACursor: TCursor);
begin
  SVNCommitMsgMemo.Cursor := ACursor;
  SVNFileListView.Cursor := ACursor;
end;

procedure TSVNStatusFrm.FormCreate(Sender: TObject);
begin
  ChangeCursor(crHourGlass);

  mnuShowDiff.Caption:=rsShowDiff;
  mnuRevert.Caption := rsRevert;

  ButtonPanel.OKButton.OnClick:=@OKButtonClick;

  SetColumn(SVNFileListView, 0, 25, '', False);
  SetColumn(SVNFileListView, 1, 300, rsPath, False);
  SetColumn(SVNFileListView, 2, 75, rsExtension, True);
  SetColumn(SVNFileListView, 3, 100, rsFileStatus, True);
  SetColumn(SVNFileListView, 4, 125, rsPropertyStatus, True);
  SetColumn(SVNFileListView, 5, 75, rsRevision, True);
  SetColumn(SVNFileListView, 6, 75, rsCommitRevision, True);
  SetColumn(SVNFileListView, 7, 75, rsAuthor, True);
  SetColumn(SVNFileListView, 8, 75, rsDate, True);

  ImageList.AddLazarusResource('menu_svn_diff');
  ImageList.AddLazarusResource('menu_svn_revert');
end;

procedure TSVNStatusFrm.FormDestroy(Sender: TObject);
begin
  SVNStatus.Free;
end;

initialization
  {$I svnstatusform.lrs}

end.

