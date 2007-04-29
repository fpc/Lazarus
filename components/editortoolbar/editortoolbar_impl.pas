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

unit editortoolbar_impl;

{$mode objfpc}{$H+}

interface

uses
  Classes
  ,CodeToolManager
  ,CodeTree
  ,jumpto_impl
  ,Forms
  ,ComCtrls
  ,Controls
  ,Menus
  ,MenuIntf
  ;


const
  cSettingsFile = 'editortoolbar.xml';
  cDivider      = '---------------';


type

  TEditorToolbar = class(TObject)
  private
    FJumpHandler: TJumpHandler;
    W: TForm;
    TB: TToolbar;
    BI: TImageList;
    procedure   CreateEditorToolbar(AW: TForm; var ATB: TToolbar; var ABI: TImageList);
    function    CreateJumpItem(AJumpType: TJumpType; O: TComponent): TMenuItem;
    procedure   DoConfigureToolbar(Sender: TObject);
  protected
    procedure   AddButton(AMenuItem: TIDEMenuItem);
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   InitEditorToolBar;
    procedure   AddCustomItems;
    procedure   AddDivider;
    procedure   AddStaticItems;
    procedure   ClearToolbar;
  end;
  
  
procedure Register;
function  gEditorToolbar: TEditorToolbar;

implementation

uses
  CodeAtom
  ,SrcEditorIntf
  ,LazIDEIntf
  ,CustomCodeTool
  ,Dialogs
  ,SysUtils
  ,LResources
  ,EdtTbConfigFrm
  ,LazConfigStorage
  ,BaseIDEIntf
  ;


var
  uEditorToolbar: TEditorToolbar;
  

// Singleton function
function gEditorToolbar: TEditorToolbar;
begin
  if not Assigned(uEditorToolbar) then
    uEditorToolbar := TEditorToolbar.Create;
  result := uEditorToolbar;
end;

procedure TEditorToolbar.CreateEditorToolbar(AW: TForm; var ATB: TToolbar; var ABI: TImageList);
begin
  ABI := TImageList.Create(AW);
  ATB := TToolbar.Create(AW);
  ATB.Parent   := AW;
  ATB.Height   := 26;
  ATB.Align    := alTop;
  ATB.Flat     := True;
  ATB.Images   := ABI;
  ATB.ShowHint := True;
end;

function TEditorToolbar.CreateJumpItem(AJumpType: TJumpType; O: TComponent): TMenuItem;
begin
  Result := TMenuItem.Create(O);
  Result.Tag      := Ord(AJumpType);
  Result.OnClick  := @FJumpHandler.DoJump;
  Result.Caption  := cJumpNames[AJumpType];
end;

procedure TEditorToolbar.DoConfigureToolbar(Sender: TObject);
begin
  TEdtTbConfigForm.Execute;
end;

constructor TEditorToolbar.Create;
begin
  FJumpHandler := TJumpHandler.Create(nil);
end;

destructor TEditorToolbar.Destroy;
begin
  FJumpHandler.Free;
  inherited Destroy;
end;

procedure TEditorToolbar.InitEditorToolBar;
begin
  if not Assigned(W) then
  begin
    W := SourceEditorWindow;
    BI := nil;
    TB := nil;
    CreateEditorToolBar(W, TB, BI);
  end;

  AddCustomItems;
  AddStaticItems;
end;

procedure TEditorToolbar.AddButton(AMenuItem: TIDEMenuItem);
var
  B: TToolButton;
  i: integer;
begin
  B := TToolButton.Create(TB);
  B.Parent      := TB;
  B.Caption     := AMenuItem.Caption;
  B.Hint        := AMenuItem.Caption; // or should we use AMenuItem.Hint?
  // If we have a image, us it. Otherwise supply a default.
  if AMenuItem.HasBitmap then
  begin
    i := BI.Add(AMenuItem.Bitmap, AMenuItem.Bitmap);
    B.ImageIndex := i;
  end
  else
    B.ImageIndex  := BI.AddLazarusResource('execute16');

  B.Style       := tbsButton;
  B.OnClick     := AMenuItem.OnClick;
end;

procedure TEditorToolbar.AddCustomItems;
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
    for i := c - 1 downto 0 do
    begin
      value := cfg.GetValue('Button' + Format('%2.2d', [i+1]) + '/Value', '');
      if value = cDivider then
        AddDivider
      else
      begin
        mi := IDEMenuRoots.FindByPath(value, True);
        if Assigned(mi) then
          AddButton(mi);
      end;
    end;
  finally
    cfg.Free;
  end;
end;

procedure TEditorToolbar.AddDivider;
var
  B: TToolButton;
begin
  B := TToolbutton.Create(TB);
  B.Parent      := TB;
  B.Style       := tbsDivider;
end;

procedure TEditorToolbar.AddStaticItems;
var
  B: TToolButton;
  PM: TPopupMenu;
  T: TJumpType;
begin
  TB.BeginUpdate;
  try
    if TB.ButtonCount <> 0 then
      AddDivider;

    // JumpTo Button
    B := TToolbutton.Create(TB);
    B.Parent      := TB;
    B.Caption     := 'Jump To';
    B.Hint        := B.Caption;
    B.ImageIndex  := BI.AddLazarusResource('jumpto16');
    B.Style       := tbsDropDown;
    B.OnClick     := @FJumpHandler.DoJumpToImplementation;

    PM := TPopupMenu.Create(W);
    B.DropdownMenu := PM;

    for T := Low(TJumpType) to High(TJumpType) do
      PM.Items.Add(CreateJumpItem(T,W));

    AddDivider;

    // Config Button
    B := TToolbutton.Create(TB);
    B.Parent      := TB;
    B.Caption     := 'Configure Toolbar';
    B.Hint        := B.Caption;
    B.ImageIndex  := BI.AddLazarusResource('preferences16');
    B.Style       := tbsButton;
    B.OnClick     := @DoConfigureToolbar;
  finally
    TB.EndUpdate;
  end;
end;

procedure TEditorToolbar.ClearToolbar;
var
  i: integer;
begin
  TB.BeginUpdate;
  try
    for i := TB.ButtonCount-1 downto 0 do
      TB.Buttons[i].Visible := False;
//      TB.Controls[i].Free;    // This causes a crash!
  finally
    TB.EndUpdate;
  end;
end;

procedure Register;
begin
  if (SourceEditorWindow <> nil) then
    gEditorToolbar.InitEditorToolBar;
end;


initialization
  uEditorToolbar := nil;
  {$I toolbar.lrs}    // all required images

finalization
  uEditorToolbar.Free;

end.

