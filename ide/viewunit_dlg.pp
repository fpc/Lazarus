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
  LazarusIdeStrConsts, LCLType, LCLIntf, LMessages,
  ExtCtrls, ButtonPanel, Menus, StrUtils, ImgList,
  IDEWindowIntf, IDEHelpIntf, IDEImagesIntf, ListFilterEdit;

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
    BtnPanel: TPanel;
    ButtonPanel: TButtonPanel;
    DummySpeedButton: TSpeedButton;
    FilterEdit: TListFilterEdit;
    ListBox: TListBox;
    mniMultiSelect: TMenuItem;
    OptionsBitBtn: TSpeedButton;
    popListBox: TPopupMenu;
    RemoveBitBtn: TSpeedButton;
    SortAlphabeticallySpeedButton: TSpeedButton;
    procedure ListboxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure SortAlphabeticallySpeedButtonClick(Sender: TObject);
    procedure OKButtonClick(Sender :TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender :TObject);
    procedure ListboxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MultiselectCheckBoxClick(Sender :TObject);
  private
    FSortAlphabetically: boolean;
    FImageIndex: Integer;
    procedure SetSortAlphabetically(const AValue: boolean);
  public
    constructor Create(TheOwner: TComponent); override;
    property SortAlphabetically: boolean read FSortAlphabetically write SetSortAlphabetically;
  end;

// Entries is a list of TViewUnitsEntry(s)
function ShowViewUnitsDlg(Entries: TStringList; AllowMultiSelect: boolean;
  var CheckMultiSelect: Boolean; const aCaption: string; aImageIndex: Integer): TModalResult;

implementation

{$R *.lfm}

function ShowViewUnitsDlg(Entries: TStringList; AllowMultiSelect: boolean;
  var CheckMultiSelect: Boolean; const aCaption: string; aImageIndex: Integer): TModalResult;
var
  ViewUnitDialog: TViewUnitDialog;
  UEntry: TViewUnitsEntry;
  i: integer;
begin
  ViewUnitDialog:=TViewUnitDialog.Create(nil);
  with ViewUnitDialog do
  try
    Caption:=aCaption;
    mniMultiselect.Enabled := AllowMultiSelect;
    mniMultiselect.Checked := CheckMultiSelect;
    ListBox.MultiSelect := mniMultiselect.Enabled;
    if aImageIndex > -1 then FImageIndex:=aImageIndex; // otherwise FImageIndex will stay "0"
    // Data items
    for i:=0 to Entries.Count-1 do begin
      UEntry:=TViewUnitsEntry(Entries.Objects[i]);
      FilterEdit.Data.Add(UEntry.Name);
    end;
    FilterEdit.InvalidateFilter;
    // Initial selection
    for i:=0 to Entries.Count-1 do
      if TViewUnitsEntry(Entries.Objects[i]).Selected then begin
        UEntry:=TViewUnitsEntry(Entries.Objects[i]);
        FilterEdit.SelectionList.Add(UEntry.Name);
      end;
    // Show the dialog
    Result:=ShowModal;
    if Result=mrOk then begin
      // Return new selections from the dialog
      FilterEdit.StoreSelection;
      for i:=0 to Entries.Count-1 do begin
        UEntry:=TViewUnitsEntry(Entries.Objects[i]);
        UEntry.Selected:=FilterEdit.SelectionList.IndexOf(UEntry.Name)>-1;
      end;
      CheckMultiSelect := mniMultiselect.Checked;
    end;
  finally
    Free;
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
  //ActiveControl:=FilterEdit;
  mniMultiSelect.Caption := dlgMultiSelect;
  ButtonPanel.OKButton.Caption:=lisOk;
  ButtonPanel.HelpButton.Caption:=lisMenuHelp;
  ButtonPanel.CancelButton.Caption:=dlgCancel;
  SortAlphabeticallySpeedButton.Hint:=lisPESortFilesAlphabetically;
  SortAlphabeticallySpeedButton.LoadGlyphFromLazarusResource('pkg_sortalphabetically');
end;

procedure TViewUnitDialog.SortAlphabeticallySpeedButtonClick(Sender: TObject);
begin
  SortAlphabetically:=SortAlphabeticallySpeedButton.Down;
end;

procedure TViewUnitDialog.ListboxDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
begin
  if Index < 0 then Exit;
  with ListBox do
  begin
    Canvas.FillRect(ARect);
    IDEImages.Images_16.Draw(Canvas, 1, ARect.Top, FImageIndex);
    Canvas.TextRect(ARect, ARect.Left + 20, ARect.Top, Items[Index]);
  end;
end;

procedure TViewUnitDialog.OKButtonClick(Sender: TObject);
Begin
  IDEDialogLayoutList.SaveLayout(Self);
  ModalResult := mrOK;
End;

procedure TViewUnitDialog.HelpButtonClick(Sender: TObject);
begin
  LazarusHelp.ShowHelpForIDEControl(Self);
end;

procedure TViewUnitDialog.CancelButtonClick(Sender: TObject);
Begin
  IDEDialogLayoutList.SaveLayout(Self);
  ModalResult := mrCancel;
end;

procedure TViewUnitDialog.ListboxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    OKButtonClick(nil)
  // A hack to prevent 'O' working as shortcut for OK-button.
  // Should be removed when issue #20599 is resolved.
  else if (Key = VK_O) and (Shift = []) then
    Key:=VK_UNKNOWN;
end;

procedure TViewUnitDialog.MultiselectCheckBoxClick(Sender :TObject);
begin
  ListBox.Multiselect := mniMultiSelect.Checked;
end;

procedure TViewUnitDialog.SetSortAlphabetically(const AValue: boolean);
begin
  if FSortAlphabetically=AValue then exit;
  FSortAlphabetically:=AValue;
  SortAlphabeticallySpeedButton.Down:=SortAlphabetically;
  FilterEdit.SortData:=SortAlphabetically;
  FilterEdit.InvalidateFilter;
end;

end.

