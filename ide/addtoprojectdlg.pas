{
 /***************************************************************************
                          addtoprojectdlg.pas
                          -------------------


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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner
}
unit AddToProjectDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  Forms, Controls, ComCtrls, StdCtrls, Dialogs, ButtonPanel,
  // LazUtils
  FileUtil, LazFileUtils,
  // IDEIntf
  IDEWindowIntf,
  // IDE
  LazarusIDEStrConsts, IDEDialogs, Project, ProjPackChecks;
  
type
  { TAddToProjectDialog }

  TAddToProjectDialog = class(TForm)
    AddFileListView: TListView;
    ButtonPanel: TButtonPanel;
    procedure AddFileButtonClick(Sender: TObject);
    procedure AddFileListViewSelectItem(Sender: TObject; {%H-}Item: TListItem;
      {%H-}Selected: Boolean);
    procedure AddToProjectDialogClose(Sender: TObject;
                                      var {%H-}CloseAction: TCloseAction);
  private
    fProject: TProject;
    fFileNames: TStrings;
    procedure UpdateAvailableFiles;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;
  
function ShowAddToProjectDlg(AProject: TProject; AFileNames: TStrings): TModalResult;


implementation

{$R *.lfm}

function ShowAddToProjectDlg(AProject: TProject; AFileNames: TStrings): TModalResult;
var
  AddToProjectDialog: TAddToProjectDialog;
begin
  AddToProjectDialog:=TAddToProjectDialog.Create(nil);
  try
    AddToProjectDialog.fProject:=AProject;
    AddToProjectDialog.fFileNames:=AFileNames;
    AddToProjectDialog.UpdateAvailableFiles;
    Result:=AddToProjectDialog.ShowModal;
  finally
    AddToProjectDialog.Free;
  end;
end;

{ TAddToProjectDialog }

constructor TAddToProjectDialog.Create(TheOwner: TComponent);
var
  CurColumn: TListColumn;
begin
  inherited Create(TheOwner);
  Caption:=lisProjAddEditorFile;
  IDEDialogLayoutList.ApplyLayout(Self,500,300);
  ButtonPanel.OKButton.ModalResult := mrNone;
  ButtonPanel.OKButton.OnClick := @AddFileButtonClick;
  ButtonPanel.OKButton.Caption:=lisA2PAddFiles;
  ButtonPanel.OkButton.Enabled:=AddFileListView.SelCount>0;
  CurColumn:=AddFileListView.Columns.Add;
  CurColumn.Caption:=lisA2PFilename2;
end;

destructor TAddToProjectDialog.Destroy;
begin
  inherited Destroy;
end;

procedure TAddToProjectDialog.AddToProjectDialogClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TAddToProjectDialog.AddFileButtonClick(Sender: TObject);
var
  NewFilename, NewUnitName, OtherFile: string;
  i, j: Integer;
begin
  for i:=0 to AddFileListView.Items.Count-1 do
    if AddFileListView.Items[i].Selected then
    begin
      NewFilename:=AddFileListView.Items[i].Caption;
      if not FilenameIsAbsolute(NewFilename) then  // expand filename
        NewFilename:=TrimFilename(fProject.Directory+PathDelim+NewFilename);
      case TPrjFileCheck.AddingFile(fProject, NewFilename) of
        mrOk: begin
            // check if unitname already exists in selection
            if FilenameHasPascalExt(NewFilename) then
            begin
              NewUnitName:=ExtractFileNameOnly(NewFilename);
              for j:=0 to fFileNames.Count-1 do
              begin
                OtherFile:=fFileNames[j];
                if FilenameHasPascalExt(OtherFile)
                and (CompareText(ExtractFileNameOnly(OtherFile), NewUnitName)=0)
                then begin
                  IDEMessageDialog(lisProjAddUnitNameAlreadyExists,
                    Format(lisProjAddTheUnitNameAlreadyExistsInTheSelection,
                           [NewUnitName, LineEnding, OtherFile]),
                    mtWarning, [mbCancel]);
                  exit;
                end;
              end;
            end;
            fFileNames.Add(NewFilename);  // Add to valid files
          end;  // mrOk
        mrIgnore: continue;
        mrCancel: exit;
      end;
    end;
  ModalResult:=mrOk;  // everything ok
end;

procedure TAddToProjectDialog.AddFileListViewSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  ButtonPanel.OkButton.Enabled:=AddFileListView.SelCount>0;
end;

procedure TAddToProjectDialog.UpdateAvailableFiles;
var
  CurFile: TUnitInfo;
  NewListItem: TListItem;
  NewFilename: String;
begin
  AddFileListView.Items.BeginUpdate;
  if fProject<>nil then begin
    CurFile:=fProject.FirstUnitWithEditorIndex;
    while CurFile<>nil do begin
      if (not CurFile.IsPartOfProject) and (not CurFile.IsVirtual) then begin
        NewFilename:=CreateRelativePath(CurFile.Filename,fProject.Directory);
        NewListItem:=AddFileListView.Items.Add;
        NewListItem.Caption:=NewFilename;
        NewListItem.Selected:=True;
      end;
      CurFile:=CurFile.NextUnitWithEditorIndex;
    end;
  end;
  AddFileListView.Items.EndUpdate;
  ButtonPanel.OkButton.Enabled:=AddFileListView.SelCount>0;
end;

end.

