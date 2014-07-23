{
  Copyright (C) 2007 Graeme Geldenhuys (graemeg@gmail.com)

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit EdtTbConfigFrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, ComCtrls, MenuIntf, editortoolbar_str, TreeFilterEdit;

type

  { TEdtTbConfigForm }

  TEdtTbConfigForm = class(TForm)
    Bevel1: TBevel;
    btnAdd: TSpeedButton;
    btnMoveDown: TSpeedButton;
    btnMoveUp: TSpeedButton;
    btnOK: TButton;
    btnCancel: TButton;
    btnAddDivider: TButton;
    btnRemove: TSpeedButton;
    cbPos: TComboBox;
    cbVisible: TCheckBox;
    lblpos: TLabel;
    lblMenuTree: TLabel;
    lblToolbar: TLabel;
    lbToolbar: TListBox;
    pnlButtons: TPanel;
    FilterEdit: TTreeFilterEdit;
    Splitter1: TSplitter;
    TV: TTreeView;
    procedure btnCancelClick(Sender: TObject);
    procedure cbPosChange(Sender: TObject);
    procedure cbVisibleChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lbToolbarSelectionChange(Sender: TObject; User: boolean);
    procedure btnAddClick(Sender: TObject);
    procedure btnAddDividerClick(Sender: TObject);
    procedure btnMoveDownClick(Sender: TObject);
    procedure btnMoveUpClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure TVSelectionChanged(Sender: TObject);
  private
    FToolBarPos: string;
    FToolBarShow: boolean;
    procedure SetupCaptions;
    procedure LoadCategories;
    procedure LoadSettings;
    procedure SaveSettings;
    procedure AddMenuItem(ParentNode: TTreeNode; Item: TIDEMenuItem);
  public
    class function Execute: boolean;
    class procedure Setup;
  end; 

Var
  sPosValues: array[0..3] of string = ('Top','Bottom','Right','Left');
  sLocalizedPosValues: array[0..3] of string;

implementation

{$R *.lfm}

uses
  editortoolbar_impl, LazConfigStorage, BaseIDEIntf, LazIDEIntf, IDEImagesIntf;

{
Function IndexFromLocalized (var AValue: string): Integer;
var
  i:Integer;
begin
 for i:= 0 to 3 do begin
   if AValue = sLocalizedPosValues[i] then begin
    Result := I;
    exit;
   end;
 end;
 Result := 0; // default is Top
end;
}

Function IndexFromEnglish (var AValue: string): Integer;
var
  i:Integer;
begin
 for i:= 0 to 3 do begin
   if AValue = sPosValues[i] then begin
    Result := I;
    exit;
   end;
 end;
 Result := 0; // default is Top
end;

{ TEdtTbConfigForm }

procedure TEdtTbConfigForm.FormCreate(Sender: TObject);
begin
  inherited;
  pnlButtons.Color := clBtnFace;

  // load button images
  btnAdd.LoadGlyphFromResourceName(HInstance, 'arrow_right');
  btnRemove.LoadGlyphFromResourceName(HInstance, 'arrow_left');
  btnMoveUp.LoadGlyphFromResourceName(HInstance, 'arrow_up');
  btnMoveDown.LoadGlyphFromResourceName(HInstance, 'arrow_down');

  btnAdd.Hint      := rsAddSelected;
  btnRemove.Hint   := rsRemoveSelected;
  btnMoveUp.Hint   := rsMoveSelectedUp;
  btnMoveDown.Hint := rsMoveSelectedDown;
  cbVisible.Hint   := rsShowHide;

  TV.Images := IDEImages.Images_16;
  SetupCaptions;
  LoadCategories;
  LoadSettings;
end;

procedure TEdtTbConfigForm.cbPosChange(Sender: TObject);
var
  i: Integer;
begin
  i:= cbPos.ItemIndex;
  if i >= 0 then begin
    FToolbarPos:= sPosValues[i];
  end;
end;

procedure TEdtTbConfigForm.btnCancelClick(Sender: TObject);
begin
end;

procedure TEdtTbConfigForm.cbVisibleChange(Sender: TObject);
begin
  FToolBarShow:= cbVisible.Checked;
end;

procedure TEdtTbConfigForm.lbToolbarSelectionChange(Sender: TObject; User: boolean);
var
  i: Integer;
begin
  i := lbToolbar.ItemIndex;
  btnRemove.Enabled := i > -1;
  btnMoveUp.Enabled := i > 0;
  btnMoveDown.Enabled := (i > -1) and (i < lbToolbar.Items.Count-1);
end;

procedure TEdtTbConfigForm.TVSelectionChanged(Sender: TObject);
var
  n: TTreeNode;
begin
  n := TV.Selected;
  btnAdd.Enabled := (Assigned(n) and Assigned(n.Data));
end;

procedure TEdtTbConfigForm.btnAddClick(Sender: TObject);
var
  n: TTreeNode;
begin
  n := TV.Selected;
  if (Assigned(n) and Assigned(n.Data)) then
  begin
    btnAdd.Enabled := False;
    lbToolbar.Items.AddObject(TIDEMenuItem(n.Data).Caption, TObject(n.Data));
    lbToolbar.ItemIndex := lbToolbar.Items.Count-1;
    lbToolbarSelectionChange(lblToolbar, False);
  end;
end;

procedure TEdtTbConfigForm.btnRemoveClick(Sender: TObject);
begin
  if lbToolbar.ItemIndex > -1 then begin
    lbToolbar.Items.Delete(lbToolbar.ItemIndex);
    lbToolbarSelectionChange(lbToolbar, False);
    TVSelectionChanged(TV);
  end;
end;

procedure TEdtTbConfigForm.btnAddDividerClick(Sender: TObject);
begin
  lbToolbar.Items.Add(cDivider);
end;

procedure TEdtTbConfigForm.btnMoveDownClick(Sender: TObject);
begin
  if lbToolbar.ItemIndex = -1 then
    exit;
  if lbToolbar.ItemIndex < lbToolbar.Items.Count - 1 then
  begin
    lbToolbar.Items.Exchange(lbToolbar.ItemIndex, lbToolbar.ItemIndex+1);
    lbToolbar.ItemIndex := lbToolbar.ItemIndex+1;
  end;
end;

procedure TEdtTbConfigForm.btnMoveUpClick(Sender: TObject);
begin
  if lbToolbar.ItemIndex = -1 then
    exit;
  if lbToolbar.ItemIndex > 0 then
  begin
    lbToolbar.Items.Exchange(lbToolbar.ItemIndex, lbToolbar.ItemIndex-1);
    lbToolbar.ItemIndex := lbToolbar.ItemIndex-1;
  end;
end;

procedure TEdtTbConfigForm.btnOKClick(Sender: TObject);
begin
  SaveSettings;
  if not FToolBarShow then ShowMessageFmt(rsWarning,[rsMenuView,rsEditorToolbar]);
end;

procedure TEdtTbConfigForm.SetupCaptions;
var
  i: integer;
begin
  Caption               := rsEditorToolbarConfigForm;
  btnOK.Caption         := rsOK;
  btnCancel.Caption     := rsCancel;
  btnAddDivider.Caption := rsAddDivider;
  lblMenuTree.Caption   := rsMenuTree;
  lblToolbar.Caption    := rsToolbar;
  lblpos.Caption        := rsPosition;
  cbVisible.Caption     := rsVisible;
  sLocalizedPosValues[0] := rsTop;
  sLocalizedPosValues[1] := rsBottom;
  sLocalizedPosValues[2] := rsRight;
  sLocalizedPosValues[3] := rsLeft;
  for i := 0 to 3 do
  begin
   cbPos.Items[i] := sLocalizedPosValues[i]; // localized
  end;
end;

procedure TEdtTbConfigForm.LoadCategories;
var
  i: integer;
begin
  TV.Items.BeginUpdate;
  try
    TV.Items.Clear;
    for i := 0 to IDEMenuRoots.Count-1 do
      AddMenuItem(nil, IDEMenuRoots[i]);
  finally
    TV.Items.EndUpdate;
  end;
end;

procedure TEdtTbConfigForm.LoadSettings;
var
  i: integer;
  c: integer;
  cfg: TConfigStorage;
  value: string;
  mi: TIDEMenuItem;
  ms: TIDEMenuSection;
begin
  cfg := GetIDEConfigStorage(cSettingsFile, True);
  try
    c := cfg.GetValue('Count', 0);

    if c = 0 then begin
      // Let's provide a Jump Back/Jump Forward as a starting default
      ms := itmJumpings;
      mi := ms.FindByName('itmJumpBack');
      if Assigned(mi) then
        lbToolbar.Items.AddObject(mi.Caption, TObject(mi));
      mi := ms.FindByName('itmJumpForward');
      if Assigned(mi) then
        lbToolbar.Items.AddObject(mi.Caption, TObject(mi));
    end

    else begin
     for i := 0 to c - 1 do
     begin
       value := cfg.GetValue('Button' + Format('%2.2d', [i+1]) + '/Value', '');
       if value <> '' then
       begin
        if value = cDivider then
        begin
          lbToolbar.Items.Add(value);
          Continue;
        end;

        mi := IDEMenuRoots.FindByPath(value, false);
        if Assigned(mi) then
          lbToolbar.Items.AddObject(mi.Caption, TObject(mi));
       end;
     end;
    end;
    value := cfg.GetValue('Position','Top');
    FToolbarPos:= value;
    FToolBarShow:= cfg.GetValue('Visible',true);
  finally
    cfg.Free;
  end;
  i := IndexFromEnglish(FToolBarPos);
  cbPos.Text:= sLocalizedPosValues[i];
  cbVisible.Checked:= FToolBarShow;
end;

procedure TEdtTbConfigForm.SaveSettings;
var
  i: integer;
  cfg: TConfigStorage;
begin
  cfg := GetIDEConfigStorage(cSettingsFile, False);
  try
    cfg.SetValue('Count', lbToolbar.Items.Count);
    for i := 0 to lbToolbar.Items.Count - 1 do
    begin
      if lbToolbar.Items[i] = cDivider then
        cfg.SetDeleteValue('Button' + Format('%2.2d', [i+1]) + '/Value', cDivider, '')
      else
        cfg.SetDeleteValue('Button' + Format('%2.2d', [i+1]) + '/Value', TIDEMenuItem(lbToolbar.Items.Objects[i]).GetPath, '');
    end;
    cfg.SetValue('Position', FToolbarPos);
    cfg.SetValue('Visible',FToolBarShow);
    cfg.WriteToDisk;
  finally
    cfg.Free;
  end;
end;

procedure TEdtTbConfigForm.AddMenuItem(ParentNode: TTreeNode; Item: TIDEMenuItem);
var
  n: TTreeNode;
  i: integer;
  sec: TIDEMenuSection;
begin
  n := TV.Items.AddChild(ParentNode, Format('%s', [Item.Caption]));
  n.ImageIndex := Item.ImageIndex;
  n.SelectedIndex := Item.ImageIndex;
  if Item is TIDEMenuSection then
  begin
    sec := (Item as TIDEMenuSection);
    for i := 0 to sec.Count-1 do
      AddMenuItem(n, sec.Items[i]);
  end
  else
    n.Data := Item;
end;

class function TEdtTbConfigForm.Execute: boolean;
var
  frm: TEdtTbConfigForm;
begin
  frm := TEdtTbConfigForm.Create(nil);
  try
    result := frm.ShowModal = mrOK;
  finally
    frm.Free;
  end;
end;

{TEdtTbConfigForm.Setup - called if no user items in config. }
class procedure TEdtTbConfigForm.Setup;
var
  frm: TEdtTbConfigForm;
begin
  // Create inserts the default Items
  frm := TEdtTbConfigForm.Create(nil);
  try
    // Must save them, less they're inserted twice
    frm.SaveSettings;
  finally
    frm.Free;
  end;

end;

end.

