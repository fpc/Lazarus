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
  ,jumpto_impl
  ,Forms
  ,ComCtrls
  ,Controls
  ,Menus
  ,MenuIntf
  ,IDEImagesIntf
  ,SrcEditorIntf
  ,editortoolbar_str
  ,IDECommands;


const
  cSettingsFile = 'editortoolbar.xml';
  cDivider      = '---------------';
  iAll          = 15;
  iDesign       = 1;
  iDebug        = 2;
  iHTML         = 4;
  iCustom       = 8;

type

  { TEditorToolbar }

  TEditorToolbar = class(TComponent)
  private
    FJumpHandler: TJumpHandler;
    FWindow: TSourceEditorWindowInterface;
    TB: TToolbar;
    PM: TPopupMenu;
    PPUP: TPopupMenu;
    CfgButton: TToolButton;
    procedure   CreateEditorToolbar(AW: TForm; var ATB: TToolbar);
    function    CreateJumpItem(AJumpType: TJumpType; O: TComponent): TMenuItem;
    function    CreateProfileItem(ProfIndx: Integer; O: TComponent): TMenuItem;
    procedure   DoConfigureToolbar(Sender: TObject);
  protected
    procedure   MenuItemClick (Sender: TObject);
    procedure   AddButton(AMenuItem: TIDEMenuItem);
    procedure   PositionAtEnd(AToolbar: TToolbar; AButton: TToolButton);
    procedure   Reload;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   InitEditorToolBar;
    procedure   AddCustomItems;
    procedure   AddDivider;
    procedure   AddStaticItems;
    procedure   ClearToolbar;
    property    OwnerWindow: TSourceEditorWindowInterface read FWindow;
  end;
  

  { TEditorToolbarList }

  TEditorToolbarList = class
  private
    FToolBarList: TFPList;
  protected
    procedure SourceWindowCreated(Sender: TObject);
    procedure SourceWindowDestroyed(Sender: TObject);
    procedure AddBar(ABar: TEditorToolbar);
    procedure DelBar(ABar: TEditorToolbar);
    procedure ReloadAll;
  public
    constructor Create;
    destructor Destroy; override;
  end;

function GetShortcut(AMenuItem: TIDEMenuItem): string;
function GetProfileIndex (aMask: Integer): Integer;

procedure Register;

var
  sToolbarPos: string;
  bToolBarShow: boolean;
  EditorMenuCommand:TIDEMenuCommand;
  ProfileMask: array [0..4] of Integer = (iAll,iDesign,iDebug,iHTML,iCustom);
  ProfileNames: array [0..4] of String = (rsAll,rsDesign,rsDebug,rsHTML,rsCustom);

implementation

{$R toolbar.res}    // all required images

uses
  LazIDEIntf
  ,CustomCodeTool
  ,Dialogs
  ,SysUtils
  ,LResources
  ,EdtTbConfigFrm
  ,LazConfigStorage
  ,BaseIDEIntf
  ,LCLProc;

type

  { TEditToolBarToolButton }

  TEditToolBarToolButton = class(TToolButton)
  private
    FMenuItem: TIDEMenuItem;
  public
    procedure Click; override;
    property MenuItem: TIDEMenuItem read FMenuItem write FMenuItem;
  end;

var
  uEditorToolbarList: TEditorToolbarList;

procedure ConfigureToolbar (Sender:TObject);
begin
  if TEdtTbConfigForm.Execute then
    uEditorToolbarList.ReloadAll;
end;

procedure ToggleToolbar (Sender:TObject);
var
  ToolBarVisible: Boolean;
begin
  ToolBarVisible:= not bToolBarShow;
  EditorMenuCommand.Checked:= ToolBarVisible;
  bToolBarShow:= ToolBarVisible;
  TEdtTbConfigForm.UpdateVisible(ToolBarVisible);
  uEditorToolbarList.ReloadAll;
end;

procedure TEditToolBarToolButton.Click;
begin
  inherited Click;
  if assigned(FMenuItem) then
    FMenuItem.TriggerClick;
end;

{ TEditorToolbarList }

procedure TEditorToolbarList.SourceWindowCreated(Sender: TObject);
begin
  TEditorToolbar.Create(Sender as TSourceEditorWindowInterface);
end;

procedure TEditorToolbarList.SourceWindowDestroyed(Sender: TObject);
var
  i: integer;
  aBar: TEditorToolbar;
begin
  // Let's remove from our list the destroyed window and then destroy the ToolBar
  for i:= 0 to FToolBarList.Count -1 do begin
    aBar := TEditorToolbar(FToolBarList[i]);
    if aBar.OwnerWindow = TSourceEditorWindowInterface(Sender) then begin
      DelBar(aBar);
      aBar.Free;
      exit;
    end;
  end;
end;

procedure TEditorToolbarList.AddBar(ABar: TEditorToolbar);
begin
  FToolBarList.Add(ABar);
end;

procedure TEditorToolbarList.DelBar(ABar: TEditorToolbar);
begin
  FToolBarList.Remove(ABar);
end;

procedure TEditorToolbarList.ReloadAll;
var
  i: Integer;
begin
  for i := 0 to FToolBarList.Count - 1 do
    TEditorToolbar(FToolBarList[i]).Reload
end;

constructor TEditorToolbarList.Create;
begin
  inherited;
  uEditorToolbarList := self;
  FToolBarList := TFPList.Create;

  if SourceEditorManagerIntf <> nil then begin
    SourceEditorManagerIntf.RegisterChangeEvent(semWindowCreate, @SourceWindowCreated);
    SourceEditorManagerIntf.RegisterChangeEvent(semWindowDestroy,@SourceWindowDestroyed);
  end;

end;

destructor TEditorToolbarList.Destroy;
begin
  while FToolBarList.Count > 0 do
    TEditorToolbar(FToolBarList[0]).Free;
  FreeAndNil(FToolBarList);
  inherited Destroy;
end;

{ TEditorToolbar }

procedure TEditorToolbar.CreateEditorToolbar(AW: TForm; var ATB: TToolbar);
begin
  { It must be created with Align = alTop, so that the first positioning
  of buttons is correct. }
  ATB := TToolbar.Create(AW);
  ATB.Parent   := AW;
  ATB.Height   := 26;
  ATB.Align    := alTop;
  ATB.Flat     := True;
  ATB.Images   := IDEImages.Images_16;
  ATB.ShowHint := True;
  ATB.Hint     := rsHint;
  ATB.PopupMenu := PPUP;
end;

function TEditorToolbar.CreateJumpItem(AJumpType: TJumpType; O: TComponent): TMenuItem;
begin
  Result := TMenuItem.Create(O);
  Result.Tag      := Ord(AJumpType);
  Result.OnClick  := @FJumpHandler.DoJump;
  Result.Caption  := cJumpNames[AJumpType];
end;

function TEditorToolbar.CreateProfileItem(ProfIndx: Integer; O: TComponent
  ): TMenuItem;
begin
  Result            := TMenuItem.Create(O);
  Result.Tag        := ProfIndx;
  Result.Caption    := ProfileNames[ProfIndx];
  Result.GroupIndex := 1;
  Result.RadioItem  := True;
  //Result.AutoCheck  := True;
  Result.OnClick    := @MenuItemClick;
end;

procedure TEditorToolbar.DoConfigureToolbar(Sender: TObject);
begin
  if TEdtTbConfigForm.Execute then
    uEditorToolbarList.ReloadAll;
end;

procedure TEditorToolbar.MenuItemClick(Sender: TObject);
var
  cfg: TConfigStorage;
  Value: Integer;
begin
  if (Sender <> nil ) and (Sender is TMenuItem) then begin
    TMenuItem(Sender).Checked:= True;
    Value:= TMenuItem(Sender).Tag;
    CurrProfile := ProfileMask[Value];
    cfg := GetIDEConfigStorage(cSettingsFile,True);
    try
      cfg.SetDeleteValue('Profile',CurrProfile,iAll);
      cfg.WriteToDisk;
    finally
      cfg.Free;
    end;
    Reload;
  end;
end;

constructor TEditorToolbar.Create(AOwner: TComponent);
var
  T: TJumpType;
  c: integer;
  cfg: TConfigStorage;
begin
  uEditorToolbarList.AddBar(Self);
  if assigned(TB) then exit;

  FJumpHandler := TJumpHandler.Create(nil);
  FWindow := TSourceEditorWindowInterface(AOwner);

  PPUP := TPopupMenu.Create(FWindow);
  for c := 0 to High(ProfileNames) do begin
    PPUP.Items.Add(CreateProfileItem(c,FWindow));
  end;

  CreateEditorToolBar(FWindow, TB);

  PM := TPopupMenu.Create(FWindow);
  for T := Low(TJumpType) to High(TJumpType) do
    PM.Items.Add(CreateJumpItem(T, FWindow));

  AddStaticItems;
  // Let's verify if it's a first start
  c:= 0; // Just in case...
  cfg := GetIDEConfigStorage(cSettingsFile, True);
  try
    c := cfg.GetValue('Count', 0);
  finally
    cfg.Free;
  end;
  if c = 0 then
    TEdtTbConfigForm.Setup;

  AddCustomItems;
end;

destructor TEditorToolbar.Destroy;
begin
  uEditorToolbarList.DelBar(Self);
  FJumpHandler.Free;
  inherited Destroy;
end;

procedure TEditorToolbar.InitEditorToolBar;
begin
  TB := nil;
  CfgButton := nil;
end;

procedure TEditorToolbar.AddButton(AMenuItem: TIDEMenuItem);
var
  B: TEditToolBarToolButton;
  ACaption: string;
begin
  B := TEditToolBarToolButton.Create(TB);
  ACaption      := AMenuItem.Caption;
  DeleteAmpersands(ACaption);
  B.Caption     := ACaption;
  // Get Shortcut, if any, and append to Hint
  ACaption:= ACaption + GetShortcut(AMenuItem);
  B.Hint        := ACaption;
  // If we have a image, us it. Otherwise supply a default.
  if AMenuItem.ImageIndex <> -1 then
    B.ImageIndex := AMenuItem.ImageIndex
  else
    B.ImageIndex  := IDEImages.LoadImage(16, 'execute16');

  B.Style       := tbsButton;
  B.MenuItem := AMenuItem;
  //B.OnClick     := AMenuItem.OnClick;
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

procedure TEditorToolbar.Reload;
begin
  ClearToolbar;
  AddStaticItems;
  AddCustomItems;
end;

procedure TEditorToolbar.AddCustomItems;
var
  i: integer;
  c: integer;
  cfg: TConfigStorage;
  value: string;
  profile: Integer;
  mi: TIDEMenuItem;
  ShowItem: Boolean;

procedure SetTbPos;
begin
case sToolbarPos of
    'Top': begin
      TB.Align:= alTop;
      TB.Height:= 26;
      end;
    'Bottom': begin
      TB.Align:= alBottom;
      TB.Height:= 26;
      end;
    'Left': begin
      TB.Align:= alLeft;
      TB.Width:= 26;
      end;
    'Right': begin
      TB.Align:= alRight;
      TB.Width:= 26;
      end;
  end;
end;

begin
  cfg := GetIDEConfigStorage(cSettingsFile, True);
  TB.BeginUpdate;
  try
    c:= cfg.GetValue('Profile',iAll);
    CurrProfile := c;
    c := GetProfileIndex(CurrProfile);
    PPUP.Items[c].Checked:= True;
    c := cfg.GetValue('Count', 0);
    for i := 1 to c do
    begin
      value := cfg.GetValue('Button' + Format('%2.2d', [i]) + '/Value', '');
      profile := cfg.GetValue('Button' + Format('%2.2d', [i]) + '/Profile', iall);
      ShowItem := (CurrProfile = iAll) or ((profile and CurrProfile) <> 0);
      if (value <> '') and ShowItem then
       begin
        if value = cDivider then
          AddDivider
        else
        begin
          mi := IDEMenuRoots.FindByPath(value,false);
          if Assigned(mi) then
            AddButton(mi);
        end;
       end;
    end;
    sToolbarPos := cfg.GetValue('Position','Top');
    bToolBarShow:= cfg.GetValue('Visible',true);
    EditorMenuCommand.Checked:= bToolBarShow;
    SetTbPos;
  finally
    cfg.Free;
    TB.EndUpdate;
  end;
  TB.Visible:= bToolBarShow;
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
    CfgButton.Caption     := rsConfigureToo;
    CfgButton.Hint        := CfgButton.Caption;
    CfgButton.ImageIndex  := IDEImages.LoadImage(16, 'preferences16');
    CfgButton.Style       := tbsButton;
    CfgButton.OnClick     := @DoConfigureToolbar;
    PositionAtEnd(TB, CfgButton);
    
    AddDivider;
    
    // JumpTo Button
    B := TToolbutton.Create(TB);
    B.Caption       := rsJumpTo;
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

function GetShortcut(AMenuItem: TIDEMenuItem): string;
var
  ACommand: TIDECommand;
  AShortcut: string;
begin
  Result := '';
  AShortcut:= '';
  if AMenuItem is TIDEMenuCommand then begin
   ACommand := TIDEMenuCommand(AMenuItem).Command;
   if Assigned(ACommand) then  AShortcut:= ShortCutToText(ACommand.AsShortCut);
   if AShortcut <> '' then Result:= ' (' + AShortcut +')';
 end;
end;

function GetProfileIndex(aMask: Integer): Integer;
var
  I: Integer;
begin
  for I:= 0 to High(ProfileMask) do begin
    if aMask = ProfileMask[I] then begin
      Result := I;
      Exit;
    end;
  end;
  Result := 0;
end;

procedure Register;
{$IFDEF LCLQt}
var
  MenuIcon: string;
{$ENDIF}
begin
  if uEditorToolbarList = nil then begin
    TEditorToolbarList.Create;
    EditorMenuCommand:= RegisterIDEMenuCommand(itmViewSecondaryWindows,'EditorToolBar',
      rsEditorToolbar,nil,@ToggleToolbar);
    EditorMenuCommand.Checked:= True;
    EditorMenuCommand.Enabled:= True;
    // GTK2 and Windows do not show both Icon and checkbox. Only Qt Does
{$IFDEF LCLQt}
    MenuIcon:= 'menu_editor_options';
    //MenuIcon:= 'menu_editor_toolbar'; TODO!
    EditorMenuCommand.ImageIndex := IDEImages.LoadImage(16, MenuIcon);
{$ENDIF}
  end;

end;


initialization
  uEditorToolbarList := nil;
  sToolbarPos:= 'Top';
  bToolBarShow:= True;
  CurrProfile:= iAll;

finalization
  uEditorToolbarList.Free;

end.

