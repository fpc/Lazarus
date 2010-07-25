{  $Id$  }
{
 /***************************************************************************
                          ViewUnit_dlg.pp
                          ---------------
   TViewUnit is the application dialog for displaying all units in a project.
   It gets used for the "View Units", "View Forms" and "Remove from Project"
   menu items.


   Initial Revision  : Sat Feb 19 17:42 CST 1999


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
}
unit ViewUnit_Dlg;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Math, Controls, Forms, Dialogs, Buttons, StdCtrls,
  LazarusIdeStrConsts, LCLType, LCLIntf, LMessages, IDEWindowIntf, IDEContextHelpEdit,
  ExtCtrls, ButtonPanel, Menus, StrUtils;

type
  TViewUnitsEntry = class
  public
    Name: string;
    ID: integer;
    Selected: boolean;
    constructor Create(const AName: string; AnID: integer; ASelected: boolean);
  end;

  { TViewUnitDialog }

  TViewUnitDialog = class(TForm)
    ButtonPanel: TButtonPanel;
    Edit: TEdit;
    ListBox: TListBox;
    mniMultiSelect: TMenuItem;
    mniSort: TMenuItem;
    popListBox: TPopupMenu;
    procedure EditChange(Sender: TObject);
    procedure EditEnter(Sender: TObject);
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure HelpButtonClick(Sender: TObject);
    procedure mniSortClick(Sender: TObject);
    Procedure OKButtonClick(Sender :TObject);
    Procedure CancelButtonClick(Sender :TObject);
    procedure ListboxClick(Sender: TObject);
    procedure ListboxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MultiselectCheckBoxClick(Sender :TObject);
  private
    FBlockListBoxChange: boolean;
    procedure FocusEdit;
    procedure SearchList(StartIndex: Integer = -1);
  public
    constructor Create(TheOwner: TComponent); override;
  end;

function ShowViewUnitsDlg(Entries: TStringList; AllowMultiSelect: boolean;
  var CheckMultiSelect: Boolean; const Caption: string): TModalResult;
  // Entries is a list of TViewUnitsEntry(s)

implementation

{$R *.lfm}

function ShowViewUnitsDlg(Entries: TStringList; AllowMultiSelect: boolean;
  var CheckMultiSelect: Boolean; const Caption: string): TModalResult;
var
  ViewUnitDialog: TViewUnitDialog;
  i: integer;
begin
  ViewUnitDialog:=TViewUnitDialog.Create(nil);
  try
    ViewUnitDialog.Caption:=Caption;
    ViewUnitDialog.mniMultiselect.Enabled := AllowMultiSelect;
    ViewUnitDialog.mniMultiselect.Checked := CheckMultiSelect;
    ViewUnitDialog.ListBox.MultiSelect := ViewUnitDialog.mniMultiselect.Enabled;
    with ViewUnitDialog.ListBox.Items do begin
      BeginUpdate;
      Clear;
      for i:=0 to Entries.Count-1 do
        Add(TViewUnitsEntry(Entries.Objects[i]).Name);
      EndUpdate;
    end;
    for i:=0 to Entries.Count-1 do
      ViewUnitDialog.ListBox.Selected[i]:=TViewUnitsEntry(Entries.Objects[i]).Selected;
    Result:=ViewUnitDialog.ShowModal;
    if Result=mrOk then begin
      for i:=0 to Entries.Count-1 do begin
        TViewUnitsEntry(Entries.Objects[i]).Selected:=ViewUnitDialog.ListBox.Selected[i];
      end;
      CheckMultiSelect := ViewUnitDialog.mniMultiselect.Checked;
    end;
  finally
    ViewUnitDialog.Free;
  end;
end;

function SearchItem(Items: TStrings; Text: String; StartIndex: Integer = -1): Integer;
var
  i: integer;
begin
  // Items can be unsorted => use simple traverse
  Result := -1;
  Text := AnsiLowerCase(Text);
  for i := StartIndex +1 to Items.Count - 1 do
    if AnsiContainsText(Items[i], Text) then
    begin
      Result := i;
      break;
    end;
end;

{ TViewUnitsEntry }

constructor TViewUnitsEntry.Create(const AName: string; AnID: integer;
  ASelected: boolean);
begin
  inherited Create;
  Name := AName;
  ID := AnID;
  Selected := ASelected;
end;

{ TViewUnitDialog }

constructor TViewUnitDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  IDEDialogLayoutList.ApplyLayout(Self,450,300);
  mniMultiSelect.Caption := dlgMultiSelect;

  ButtonPanel.OKButton.Caption:=lisOk;
  ButtonPanel.HelpButton.Caption:=lisMenuHelp;
  ButtonPanel.CancelButton.Caption:=dlgCancel;
end;

Procedure TViewUnitDialog.OKButtonClick(Sender : TOBject);
Begin
  IDEDialogLayoutList.SaveLayout(Self);
  ModalResult := mrOK;
End;

procedure TViewUnitDialog.HelpButtonClick(Sender: TObject);
begin
  ShowContextHelpForIDE(Self);
end;

procedure TViewUnitDialog.mniSortClick(Sender: TObject);
var
  TmpList: TStringList;
  i: Integer;
  SelName: String;
begin
  TmpList := TStringList.Create;
  try
    TmpList.Assign(ListBox.Items);
    if ListBox.MultiSelect then
    begin
      for i := 0 to ListBox.Count -1 do
        if ListBox.Selected[i] then
          TmpList.Objects[i] := TObject(-1);
    end;
    TmpList.Sort;
    if ListBox.ItemIndex >= 0 then
      SelName := ListBox.Items[ListBox.ItemIndex]
    else
      SelName := '';
    ListBox.Items := TmpList;
    if SelName <> '' then
    begin
      ListBox.ItemIndex := TmpList.IndexOf(SelName);
      ListBox.MakeCurrentVisible;
    end;
    if ListBox.MultiSelect then
    begin
      ListBox.ClearSelection;
      for i := 0 to TmpList.Count -1 do
        if TmpList.Objects[i] <> nil then
          ListBox.Selected[i] := True;
    end;
  finally
    TmpList.Free;
  end;
end;

procedure TViewUnitDialog.EditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
  
  procedure MoveItemIndex(d: integer); inline;
  var
    NewIndex: Integer;
  begin
    NewIndex := Min(ListBox.Items.Count - 1, Max(0, ListBox.ItemIndex + D));
    ListBox.ItemIndex := NewIndex;
    ListBoxClick(nil);
  end;

  function PageCount: Integer;
  begin
    if ListBox.ItemHeight > 0 then
      Result := ListBox.Height div ListBox.ItemHeight
    else
      Result := 0;
  end;
  
begin
  case Key of
    VK_UP: MoveItemIndex(-1);
    VK_DOWN:
      begin
        MoveItemIndex(1);
        // Avoid switching to next control in TabOrder in gtk2
        Key := 0;
      end;
    VK_NEXT: MoveItemIndex(PageCount);
    VK_PRIOR: MoveItemIndex(-PageCount);
    VK_RETURN: OKButtonClick(nil);
    VK_RIGHT: SearchList(ListBox.ItemIndex);
  end;
end;

procedure TViewUnitDialog.EditChange(Sender: TObject);
begin
  // the change was initiated by the listbox,
  // so don't make any changes to the listbox
  if FBlockListBoxChange then exit;
  
  SearchList();
end;

procedure TViewUnitDialog.EditEnter(Sender: TObject);
begin
  FocusEdit;
end;

Procedure TViewUnitDialog.CancelButtonClick(Sender : TOBject);
Begin
  IDEDialogLayoutList.SaveLayout(Self);
  ModalResult := mrCancel;
end;

procedure TViewUnitDialog.ListboxClick(Sender: TObject);
begin
  FBlockListBoxChange := true;
  
  if ListBox.ItemIndex <> -1 then
    Edit.Text := ListBox.Items[ListBox.ItemIndex];
  
  FBlockListBoxChange := false;
end;

procedure TViewUnitDialog.ListboxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    OKButtonClick(nil);
end;

procedure TViewUnitDialog.MultiselectCheckBoxClick(Sender :TObject);
begin
  ListBox.Multiselect := mniMultiSelect.Checked;
end;

procedure TViewUnitDialog.FocusEdit;
begin
  Edit.SelectAll;
  Edit.SetFocus;
end;

procedure TViewUnitDialog.SearchList(StartIndex: Integer);
var
  Index: Integer;
begin
  Index := SearchItem(ListBox.Items, Edit.Text, StartIndex);
  if Index >= 0 then
  begin
    ListBox.ItemIndex := Index;
    ListBox.MakeCurrentVisible;
    if ListBox.MultiSelect then
    begin
      ListBox.ClearSelection;
      ListBox.Selected[Index] := True;
    end;
  end;
end;

end.

