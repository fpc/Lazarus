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
  ,IDEImagesIntf
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
    PM: TPopupMenu;
    CfgButton: TToolButton;
    procedure   CreateEditorToolbar(AW: TForm; var ATB: TToolbar);
    function    CreateJumpItem(AJumpType: TJumpType; O: TComponent): TMenuItem;
    procedure   DoConfigureToolbar(Sender: TObject);
  protected
    procedure   AddButton(AMenuItem: TIDEMenuItem);
    procedure   PositionAtEnd(AToolbar: TToolbar; AButton: TToolButton);
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

procedure TEditorToolbar.CreateEditorToolbar(AW: TForm; var ATB: TToolbar);
begin
  ATB := TToolbar.Create(AW);
  ATB.Parent   := AW;
  ATB.Height   := 26;
  ATB.Align    := alTop;
  ATB.Flat     := True;
  ATB.Images   := IDEImages.Images_16;
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
  if TEdtTbConfigForm.Execute then
  begin
    ClearToolbar;
    AddStaticItems;
    AddCustomItems;
  end;
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
var
  T: TJumpType;
begin
  if not Assigned(W) then
  begin
    W := SourceEditorWindow;
    TB := nil;
    CfgButton := nil;
    CreateEditorToolBar(W, TB);

    PM := TPopupMenu.Create(W);
    for T := Low(TJumpType) to High(TJumpType) do
      PM.Items.Add(CreateJumpItem(T,W));
  end;

  AddStaticItems;
  AddCustomItems;
end;

procedure TEditorToolbar.AddButton(AMenuItem: TIDEMenuItem);
var
  B: TToolButton;
begin
  B := TToolButton.Create(TB);
  B.Caption     := AMenuItem.Caption;
  B.Hint        := AMenuItem.Caption; // or should we use AMenuItem.Hint?
  // If we have a image, us it. Otherwise supply a default.
  if AMenuItem.ImageIndex <> -1 then
    B.ImageIndex := AMenuItem.ImageIndex
  else
    B.ImageIndex  := IDEImages.LoadImage(16, 'execute16');

  B.Style       := tbsButton;
  B.OnClick     := AMenuItem.OnClick;
  PositionAtEnd(TB, B);
end;

// position the button next to the last button
procedure TEditorToolbar.PositionAtEnd(AToolbar: TToolbar; AButton: TToolButton);
var
  SiblingButton: TToolButton;
begin
  if AToolbar.ButtonCount > 0 then
  begin
    SiblingButton := AToolbar.Buttons[AToolbar.ButtonCount-1];
    AButton.SetBounds(SiblingButton.Left + SiblingButton.Width,
      SiblingButton.Top, AButton.Width, AButton.Height);
  end;
  AButton.Parent := AToolbar;
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
  TB.BeginUpdate;
  try
    c := cfg.GetValue('Count', 0);
    for i := 1 to c do
    begin
      value := cfg.GetValue('Button' + Format('%2.2d', [i]) + '/Value', '');
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
    TB.EndUpdate;
  end;
end;

procedure TEditorToolbar.AddDivider;
var
  B: TToolButton;
begin
  B := TToolbutton.Create(TB);
  B.Style := tbsDivider;
  PositionAtEnd(TB, B);
end;

procedure TEditorToolbar.AddStaticItems;
var
  B: TToolButton;  
begin
  TB.BeginUpdate;
  try
    // Config Button
    if CfgButton = nil then
      CfgButton := TToolbutton.Create(TB);
    CfgButton.Caption     := 'Configure Toolbar';
    CfgButton.Hint        := CfgButton.Caption;
    CfgButton.ImageIndex  := IDEImages.LoadImage(16, 'preferences16');
    CfgButton.Style       := tbsButton;
    CfgButton.OnClick     := @DoConfigureToolbar;
    PositionAtEnd(TB, CfgButton);
    
    AddDivider;
    
    // JumpTo Button
    B := TToolbutton.Create(TB);
    B.Caption       := 'Jump To';
    B.Hint          := B.Caption;
    B.ImageIndex    := IDEImages.LoadImage(16, 'jumpto16');
    B.Style         := tbsDropDown;
    B.OnClick       := @FJumpHandler.DoJumpToImplementation;
    B.DropdownMenu  := PM;
    PositionAtEnd(TB, B);

    if TB.ButtonCount <> 0 then
      AddDivider;
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
    for i := TB.ButtonCount - 1 downto 0 do
      if TB.Buttons[i] <> CfgButton then
        TB.Buttons[i].Free
      else
        TB.Buttons[i].Parent := nil;
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

