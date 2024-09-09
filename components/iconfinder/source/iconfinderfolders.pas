{
 *****************************************************************************
  This file is part of a Lazarus Package, IconFinder.

  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.
 *****************************************************************************

 A form for management of the folders from which icons are included in the
 icon finder library.
}

unit IconFinderFolders;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  LazLoggerBase, LazFileUtils,
  Forms, Controls, Graphics, Dialogs, ButtonPanel, StdCtrls, CheckLst, ExtCtrls,
  IconFinderStrConsts, IconThumbnails;

type

  { TIconFolderForm }

  TIconFolderForm = class(TForm)
    CenterBevel: TBevel;
    btnDeleteFolder: TButton;
    btnMoveFolderUp: TButton;
    btnMoveFolderDown: TButton;
    btnAddFolder: TButton;
    ButtonPanel1: TButtonPanel;
    clbFolders: TCheckListBox;
    FolderPanel: TGroupBox;
    SelectDirectoryDialog: TSelectDirectoryDialog;
    procedure btnDeleteFolderClick(Sender: TObject);
    procedure btnMoveFolderDownClick(Sender: TObject);
    procedure btnMoveFolderUpClick(Sender: TObject);
    procedure btnAddFolderClick(Sender: TObject);
    procedure clbFoldersSelectionChange(Sender: TObject; {%H-}User: boolean);
    procedure FormCreate(Sender: TObject);
  private

  public
    procedure GetIconFolders(AList: TStrings);
    procedure SetIconFolders(AList: TStrings);
    procedure UpdateLanguage;

  end;

implementation

{$R *.lfm}

procedure TIconFolderForm.btnDeleteFolderClick(Sender: TObject);
var
  res: TModalResult;
  idx: Integer;
begin
  idx := clbFolders.ItemIndex;
  if idx > -1 then
  begin
    res := MessageDlg(RSFolders_ConfirmDeleteFolderMsg, mtConfirmation, [mbYes, mbNo], 0);
    if res = mrYes then
      clbFolders.Items.Delete(idx);
  end;
end;

procedure TIconFolderForm.btnMoveFolderDownClick(Sender: TObject);
var
  idx: Integer;
begin
  idx := clbFolders.ItemIndex;
  if (idx > -1) and (idx < clbFolders.Items.Count-1) then
  begin
    clbFolders.Items.Move(idx, idx+1);
    clbFolders.ItemIndex := idx+1;
  end;
end;

procedure TIconFolderForm.btnMoveFolderUpClick(Sender: TObject);
var
  idx: Integer;
begin
  idx := clbFolders.ItemIndex;
  if idx > 0 then
  begin
    clbFolders.Items.Move(idx, idx-1);
    clbFolders.ItemIndex := idx-1;
  end;
end;

procedure TIconFolderForm.btnAddFolderClick(Sender: TObject);

  function DirContainsFileMask(ADirectory, AMask: String): Boolean;
  var
    info: TSearchRec;
    masks: TStringArray;
    i: Integer;
  begin
    masks := AMask.Split(';');
    for i := 0 to High(masks) do
    begin
      if FindFirst(ADirectory + masks[i], faAnyFile, info) = 0 then
      begin
        Result := true;
        exit;
      end;
    end;
    Result := false;
  end;

var
  idx: Integer;
  folder: String;
begin
  if SelectDirectoryDialog.Execute then
  begin
    folder := AppendPathDelim(SelectDirectoryDialog.FileName);
    if not DirContainsFileMask(folder, IMAGES_MASK) then
    begin
      MessageDlg(Format(RSFolders_NoImages, [folder]), mtError, [mbOK], 0);
      exit;
    end;
    idx := clbFolders.Items.Add(SelectDirectoryDialog.FileName);
    clbFolders.Checked[idx] := true;
  end;
end;

procedure TIconFolderForm.clbFoldersSelectionChange(Sender: TObject;
  User: boolean);
var
  idx: Integer;
begin
  idx := clbFolders.ItemIndex;
  btnDeleteFolder.Enabled := idx > -1;
  btnMoveFolderUp.Enabled := idx > 0;
  btnMoveFolderDown.Enabled := (idx > -1) and (idx < clbFolders.Items.Count-1);
end;

procedure TIconFolderForm.FormCreate(Sender: TObject);
begin
  UpdateLanguage;
end;

{ Copies the listbox entries to the given list which will be passed on to the
  IconViewer's IconList by the caller.
  Hidden folders have non-nil Objects property in the outpist list. }
procedure TIconFolderForm.GetIconFolders(AList: TStrings);
const
  HIDDEN: Array[boolean] of PtrUInt = (0, 1);
var
  i: Integer;
  folder: String;
begin
  AList.BeginUpdate;
  try
    AList.Clear;
    for i := 0 to clbFolders.Items.Count-1 do
    begin
      folder := clbFolders.Items[i];
      AList.AddObject(folder, TObject(HIDDEN[not clbFolders.Checked[i]]));
    end;
  finally
    AList.EndUpdate;
  end;
end;

{ Copies the provided list entries to the checklistbox items. Entries which
  have a non-zero Objects property are unchecked in the checklistbox. }
procedure TIconFolderForm.SetIconFolders(AList: TStrings);
var
  i: Integer;
  folder: String;
  isHidden: Boolean;
begin
  clbFolders.Items.BeginUpdate;
  try
    clbFolders.Items.Clear;
    for i := 0 to AList.Count-1 do
    begin
      folder := AList.Strings[i];
      isHidden := AList.Objects[i] <> nil;
      clbFolders.Items.Add(folder);
      clbFolders.Checked[i] := not isHidden;
    end;
  finally
    clbFolders.Items.EndUpdate;
  end;
end;

procedure TIconFolderForm.UpdateLanguage;
begin
  Caption := RSFolders_IconFinderFolders;
  FolderPanel.Caption := RSFolders_IconFinderFolders;
  btnAddFolder.Caption := RSFolders_Add;
  btnDeleteFolder.Caption := RSFolders_Delete;
  btnMoveFolderUp.Caption := RSFolders_MoveUp;
  btnMoveFolderDown.Caption := RSFolders_MoveDown;
end;

end.

