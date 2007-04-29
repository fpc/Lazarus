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
  Buttons, StdCtrls, ComCtrls, MenuIntf;

type
  TEdtTbConfigForm = class(TForm)
    Bevel1: TBevel;
    btnRemove: TBitBtn;
    btnAdd: TBitBtn;
    btnMoveUp: TBitBtn;
    btnMoveDown: TBitBtn;
    btnOK: TButton;
    btnCancel: TButton;
    btnAddDivider: TButton;
    lblMenuTree: TLabel;
    lblToolbar: TLabel;
    lbToolbar: TListBox;
    pnlButtons: TPanel;
    TV: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure TVChange(Sender: TObject; Node: TTreeNode);
    procedure btnAddClick(Sender: TObject);
    procedure btnAddDividerClick(Sender: TObject);
    procedure btnMoveDownClick(Sender: TObject);
    procedure btnMoveUpClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
  private
    procedure SetupCaptions;
    procedure LoadCategories;
    procedure LoadSettings;
    procedure SaveSettings;
    procedure AddMenuItem(ParentNode: TTreeNode; Item: TIDEMenuItem);
  public
    class function Execute: boolean;
  end; 


implementation

uses
  editortoolbar_impl
  ,LazConfigStorage
  ,BaseIDEIntf
  ,LazIDEIntf
  ;


resourcestring
  rsEditorToolbarConfigForm = 'Editor Toolbar Configuration';
  rsOK                      = 'OK';
  rsCancel                  = 'Cancel';
  rsToolbar                 = 'Toolbar';
  rsMenuTree                = 'Menu Tree';
  rsAddDivider              = 'Add Divider';


{ TEdtTbConfigForm }

procedure TEdtTbConfigForm.FormCreate(Sender: TObject);
begin
  inherited;
  pnlButtons.Color := clBtnFace;
  // load button images
  btnRemove.Glyph.LoadFromLazarusResource('arrowleft_blue16');
  btnAdd.Glyph.LoadFromLazarusResource('arrowright_blue16');
  btnMoveUp.Glyph.LoadFromLazarusResource('arrowup_blue16');
  btnMoveDown.Glyph.LoadFromLazarusResource('arrowdown_blue16');

  SetupCaptions;
  LoadCategories;
  LoadSettings;
end;

procedure TEdtTbConfigForm.TVChange(Sender: TObject; Node: TTreeNode);
var
  n: TTreeNode;
begin
  if Sender = nil then ;
  if Node = nil then ;
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
    lbToolbar.Items.AddObject(TIDEMenuItem(n.Data).Caption, TObject(n.Data));
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
var
  i: integer;
begin
  SaveSettings;
  
  if lbToolbar.Items.Count = 0 then
  begin
    { resets the toolbar to only contain static (default) items }
    gEditorToolbar.ClearToolbar;
    gEditorToolbar.AddStaticItems;
    Exit; //==>
  end;

  gEditorToolbar.ClearToolbar;
  gEditorToolbar.AddCustomItems;
  gEditorToolbar.AddStaticItems;
end;

procedure TEdtTbConfigForm.btnRemoveClick(Sender: TObject);
begin
  if lbToolbar.ItemIndex > -1 then
    lbToolbar.Items.Delete(lbToolbar.ItemIndex);
end;

procedure TEdtTbConfigForm.SetupCaptions;
begin
  Caption               := rsEditorToolbarConfigForm;
  btnOK.Caption         := rsOK;
  btnCancel.Caption     := rsCancel;
  btnAddDivider.Caption := rsAddDivider;
  lblMenuTree.Caption   := rsMenuTree;
  lblToolbar.Caption    := rsToolbar;
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
begin
  cfg := GetIDEConfigStorage(cSettingsFile, True);
  try
    c := cfg.GetValue('Count', 0);
    for i := 0 to c - 1 do
    begin
      value := cfg.GetValue('Button' + Format('%2.2d', [i+1]) + '/Value', '');
      if value = cDivider then
      begin
        lbToolbar.Items.Add(value);
        Continue;
      end;
      
      mi := IDEMenuRoots.FindByPath(value, true);
      if Assigned(mi) then
        lbToolbar.Items.AddObject(mi.Caption, TObject(mi));
    end;
  finally
    cfg.Free;
  end;
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
    cfg.WriteToDisk;
  finally
    cfg.Free;
  end;
end;

procedure TEdtTbConfigForm.AddMenuItem(ParentNode: TTreeNode;
    Item: TIDEMenuItem);
var
  n: TTreeNode;
  i: integer;
  sec: TIDEMenuSection;
begin
  n := TV.Items.AddChild(ParentNode, Format('%s', [Item.Caption]));
  n.Data := Item;
  if Item is TIDEMenuSection then
  begin
    sec := (Item as TIDEMenuSection);
    for i := 0 to sec.Count-1 do
      AddMenuItem(n, sec.Items[i]);
  end;
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

initialization
  {$I edttbconfigfrm.lrs}

end.

