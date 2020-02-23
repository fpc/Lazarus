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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}
unit DefinesGui;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, StdCtrls, Buttons, ButtonPanel, CheckLst, LCLType, Controls,
  IDEHelpIntf, IDEImagesIntf, LazarusIDEStrConsts, Compiler;

type

  { TDefinesGuiForm }

  TDefinesGuiForm = class(TForm)
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
    procedure DefinesCheckListKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
  private
    FIdleConnected: Boolean;
    FOptionsReader: TCompilerOptReader;
    FOptionsThread: TCompilerOptThread;
    FCustomOptions: TStrings;
    FUseComments: Boolean;
    procedure SetIdleConnected(AValue: Boolean);
    procedure OnIdle(Sender: TObject; var {%H-}Done: Boolean);
    procedure DeleteSelected;
    procedure UpdateButtons;
  private
    property IdleConnected: Boolean read FIdleConnected write SetIdleConnected;
  public
    function ToCustomOptions(aStrings: TStrings): TModalResult;
  public
    property OptionsReader: TCompilerOptReader read FOptionsReader write FOptionsReader;
    property OptionsThread: TCompilerOptThread read FOptionsThread write FOptionsThread;
    property CustomOptions: TStrings read FCustomOptions write FCustomOptions;
    property UseComments: Boolean read FUseComments write FUseComments;
  end;


implementation

{$R *.lfm}

{ TDefinesGuiForm }

procedure TDefinesGuiForm.FormCreate(Sender: TObject);
begin
  Caption := lisLazBuildDefines;
  gbNewDefine.Caption := lisCodeToolsDefsDefine;
  AddBtn.Caption := lisBtnAdd;
  IDEImages.AssignImage(AddBtn, 'laz_add');
  RemoveBtn.Caption := lisBtnRemove;
  IDEImages.AssignImage(RemoveBtn, 'laz_delete');
end;

procedure TDefinesGuiForm.FormShow(Sender: TObject);
begin
  DefinesCheckListClick(Nil);
  ActiveControl := DefinesCheckList;
  IdleConnected := True;
end;

procedure TDefinesGuiForm.FormDestroy(Sender: TObject);
begin

end;

procedure TDefinesGuiForm.AddBtnClick(Sender: TObject);
begin
  DefinesCheckList.Items.Add(edDefine.Text);
  DefinesCheckList.ItemIndex := DefinesCheckList.Items.Count-1;
  UpdateButtons;
end;

procedure TDefinesGuiForm.EditChange(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TDefinesGuiForm.HelpButtonClick(Sender: TObject);
begin
  LazarusHelp.ShowHelpForIDEControl(Self);
end;

procedure TDefinesGuiForm.DefinesCheckListClick(Sender: TObject);
begin
  with DefinesCheckList do
    if ItemIndex > -1 then
      edDefine.Text := Items[ItemIndex];
  UpdateButtons;
end;

procedure TDefinesGuiForm.DefinesCheckListDblClick(Sender: TObject);
begin
  //ModalResult := mrOK;
end;

procedure TDefinesGuiForm.RemoveBtnClick(Sender: TObject);
begin
  DeleteSelected;
end;

procedure TDefinesGuiForm.DefinesCheckListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_DELETE then
  begin
    DeleteSelected;
    Key := 0;
  end;
end;

procedure TDefinesGuiForm.SetIdleConnected(AValue: Boolean);
begin
  if FIdleConnected = AValue then exit;
  FIdleConnected := AValue;
  if FIdleConnected then
    Application.AddOnIdleHandler(@OnIdle)
  else
    Application.RemoveOnIdleHandler(@OnIdle);
end;

procedure TDefinesGuiForm.OnIdle(Sender: TObject; var Done: Boolean);
var
  s: String;
  i, ListInd: Integer;
begin
  IdleConnected := False;
  Screen.BeginWaitCursor;
  try
    FOptionsThread.EndParsing;            // Make sure the options are read.
    // Parse and separate defines from other options.
    FOptionsReader.FromCustomOptions(FCustomOptions);
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
  finally
    Screen.EndWaitCursor;
  end;
end;

procedure TDefinesGuiForm.DeleteSelected;
var
  i: Integer;
begin
  with DefinesCheckList.Items do
    for i := Count-1 downto 0 do
      if DefinesCheckList.Selected[i] then
      begin
        Delete(i);
        UpdateButtons;
      end;
end;

procedure TDefinesGuiForm.UpdateButtons;
begin
  AddBtn.Enabled := (edDefine.Text <> '')
                and (DefinesCheckList.Items.IndexOf(edDefine.Text) = -1);
  RemoveBtn.Enabled := DefinesCheckList.SelCount > 0;
end;

function TDefinesGuiForm.ToCustomOptions(aStrings: TStrings): TModalResult;
var
  i: Integer;
begin
  // First update defines to OptionsReader.
  FOptionsReader.Defines.Clear;
  for i := 0 to DefinesCheckList.Count-1 do
    if DefinesCheckList.Checked[i] then
      FOptionsReader.Defines.Add('-d' + DefinesCheckList.Items[i]);
  // Then add all options and defines.
  FOptionsReader.ToCustomOptions(aStrings, FUseComments);
  Result:=mrOk;
end;

end.

