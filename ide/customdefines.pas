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
}
unit CustomDefines;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, Forms, LCLProc, Dialogs,
  StdCtrls, Buttons, FileUtil, ButtonPanel, ExtCtrls, CheckLst, strutils,
  IDEHelpIntf, LazarusIDEStrConsts, IDEProcs, Compiler;

type

  { TCustomDefinesForm }

  TCustomDefinesForm = class(TForm)
    AddBtn: TBitBtn;
    ButtonPanel: TButtonPanel;
    DefinesCheckList: TCheckListBox;
    gbNewDefine: TGroupBox;
    RemoveBtn: TBitBtn;
    edDefine: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure AddBtnClick(Sender: TObject);
    procedure EditChange(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure DefinesCheckListClick(Sender: TObject);
    procedure DefinesCheckListDblClick(Sender: TObject);
    procedure RemoveBtnClick(Sender: TObject);
    procedure DefinesCheckListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FOptionsReader: TCompilerOptReader;
    procedure DeleteSelected;
    procedure UpdateButtons;
  public
    function FromCustomOptions(aStrings: TStrings): TModalResult;
    function ToCustomOptions(aStrings: TStrings): TModalResult;
  end;


implementation

{$R *.lfm}

uses
  LCLType, LazConf;

{ TCustomDefinesForm }

procedure TCustomDefinesForm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  Caption := lisLazBuildDefines;
  gbNewDefine.Caption := lisCodeToolsDefsDefine;
  AddBtn.Caption := lisBtnAdd;
  AddBtn.LoadGlyphFromLazarusResource('laz_add');
  RemoveBtn.Caption := lisBtnRemove;
  RemoveBtn.LoadGlyphFromLazarusResource('laz_delete');
  FOptionsReader := TCompilerOptReader.Create;
end;

procedure TCustomDefinesForm.FormShow(Sender: TObject);
begin
  DefinesCheckListClick(Nil);
  ActiveControl := DefinesCheckList;
end;

procedure TCustomDefinesForm.FormDestroy(Sender: TObject);
begin
  FOptionsReader.Free;
end;

procedure TCustomDefinesForm.AddBtnClick(Sender: TObject);
begin
  DefinesCheckList.Items.Add(edDefine.Text);
  DefinesCheckList.ItemIndex := DefinesCheckList.Items.Count-1;
  UpdateButtons;
end;

procedure TCustomDefinesForm.EditChange(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TCustomDefinesForm.HelpButtonClick(Sender: TObject);
begin
  LazarusHelp.ShowHelpForIDEControl(Self);
end;

procedure TCustomDefinesForm.DefinesCheckListClick(Sender: TObject);
begin
  with DefinesCheckList do begin
    if ItemIndex > -1 then
      edDefine.Text := Items[ItemIndex];
  end;
  UpdateButtons;
end;

procedure TCustomDefinesForm.DefinesCheckListDblClick(Sender: TObject);
begin
  //ModalResult := mrOK;
end;

procedure TCustomDefinesForm.RemoveBtnClick(Sender: TObject);
begin
  DeleteSelected;
end;

procedure TCustomDefinesForm.DefinesCheckListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_DELETE then begin
    DeleteSelected;
    Key := 0;
  end;
end;

procedure TCustomDefinesForm.DeleteSelected;
var
  i: Integer;
begin
  with DefinesCheckList.Items do
    for i := Count-1 downto 0 do
      if DefinesCheckList.Selected[i] then begin
        Delete(i);
        UpdateButtons;
      end;
end;

procedure TCustomDefinesForm.UpdateButtons;
begin
  AddBtn.Enabled := (edDefine.Text <> '')
                and (DefinesCheckList.Items.IndexOf(edDefine.Text) = -1);
  RemoveBtn.Enabled := DefinesCheckList.SelCount > 0;
end;

function TCustomDefinesForm.FromCustomOptions(aStrings: TStrings): TModalResult;
var
  s: String;
  i, ListInd: Integer;
begin
  // Parse and separate defines from other options.
  FOptionsReader.FromCustomOptions(aStrings);
  // Check the found defines in the GUI.
  for i := 0 to FOptionsReader.Defines.Count-1 do
  begin
    s := Copy(FOptionsReader.Defines[i], 3, MaxInt); // Skip '-d'.
    ListInd := DefinesCheckList.Items.IndexOf(s);
    if ListInd = -1 then
    begin
      DefinesCheckList.Items.Add(s);
      ListInd := DefinesCheckList.Items.Count-1;
    end;
    DefinesCheckList.Checked[ListInd] := True;
  end;
end;

function TCustomDefinesForm.ToCustomOptions(aStrings: TStrings): TModalResult;
var
  i: Integer;
begin
  // First add all options except defines.
  aStrings.Clear;
  aStrings.AddStrings(FOptionsReader.OtherOptions);
  // Then add checked defines from the GUI.
  with DefinesCheckList do
    for i := 0 to Count-1 do
      if Checked[i] then begin
        aStrings.Add('-d' + Items[i]);
      end;
end;

end.

