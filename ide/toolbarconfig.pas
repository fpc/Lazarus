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

unit ToolbarConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Graphics, Dialogs, ExtCtrls, Buttons, StdCtrls,
  Controls, ComCtrls, Menus, TreeFilterEdit, LazarusIDEStrConsts,
  IDECommands, MenuIntf, IDEImagesIntf, LCLProc, ButtonPanel;

type
  { TLvItem }
  TLvItem = class (TObject)
    Item: TIDEMenuItem;
    LvIndex: Integer;
  end;

  { TToolBarConfig }

  TToolBarConfig = class(TForm)
    btnHelp: TBitBtn;
    btnAdd: TSpeedButton;
    btnMoveDown: TSpeedButton;
    btnMoveUp: TSpeedButton;
    btnOK: TButton;
    btnCancel: TButton;
    btnRemove: TSpeedButton;
    lbSelect: TLabel;
    lblMenuTree: TLabel;
    lblToolbar: TLabel;
    lvToolbar: TListView;
    miAll: TMenuItem;
    miDesign: TMenuItem;
    miDebug: TMenuItem;
    miHTML: TMenuItem;
    miCustom: TMenuItem;
    pnlButtons: TButtonPanel;
    FilterEdit: TTreeFilterEdit;
    sbAddDivider: TSpeedButton;
    btnClear: TSpeedButton;
    Splitter1: TSplitter;
    TV: TTreeView;
    procedure btnClearClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure btnShowClick(Sender: TObject);
    procedure btnHideClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbToolbarSelectionChange(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnAddDividerClick(Sender: TObject);
    procedure btnMoveDownClick(Sender: TObject);
    procedure btnMoveUpClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure lvToolbarDrawItem(Sender: TCustomListView; AItem: TListItem;
      ARect: TRect; {%H-}AState: TOwnerDrawState);
    procedure lvToolbarSelectItem(Sender: TObject; {%H-}Item: TListItem;
      {%H-}Selected: Boolean);
    procedure TVSelectionChanged(Sender: TObject);
  private
    Image: TBitMap;
    defImageIndex: integer;
    divImageIndex: Integer;
    // Main list related entries
    MainList: TStringList;
    function GetMainListIndex(Item: TListItem): Integer;
    procedure InsertMainListItem (Item,NextItem: TListItem);
    procedure RemoveMainListItem (Item:TListItem);
    procedure ExchangeMainListItem (Item1,Item2: TListItem);
    procedure SetupCaptions;
    procedure LoadCategories;
    procedure AddMenuItem(ParentNode: TTreeNode; Item: TIDEMenuItem; Level: Integer);
    function RootNodeCaption(Item: TIDEMenuItem): string;
    procedure AddListItem(Item: TIDEMenuItem);
    procedure AddToolBarItem(Item: TIDEMenuItem);
    procedure AddDivider;
    procedure FillToolBar;
  public
    procedure LoadSettings(SL: TStringList);
    procedure SaveSettings(SL: TStringList);
  end;

function ShowToolBarConfig(aNames: TStringList): TModalResult;
function GetShortcut(AMenuItem: TIDEMenuItem): string;


implementation

{$R *.lfm}

const
  cDivider = '---------------';

function ShowToolBarConfig(aNames: TStringList): TModalResult;
var
  Conf: TToolBarConfig;
begin
  Conf := TToolBarConfig.Create(Nil);
  try
    if Assigned(aNames) then
      Conf.LoadSettings(aNames);
    Result := Conf.ShowModal;
    if (Result = mrOK) and Assigned(aNames) then
      Conf.SaveSettings(aNames);
  finally
    Conf.Free;
  end;
end;

function GetShortcut(AMenuItem: TIDEMenuItem): string;
var
  ACommand: TIDECommand;
  AShortcut: string;
begin
  Result := '';
  AShortcut := '';
  if AMenuItem is TIDEMenuCommand then
  begin
    ACommand := TIDEMenuCommand(AMenuItem).Command;
    if Assigned(ACommand) then
      AShortcut := ShortCutToText(ACommand.AsShortCut);
    if AShortcut <> '' then
      Result := ' (' + AShortcut + ')';
  end;
end;

{ TToolBarConfig }

procedure TToolBarConfig.FormCreate(Sender: TObject);
begin
  inherited;
  //we have to ownerdraw the listview on qt
  {$IFDEF LCLQT}
  lvToolbar.OwnerDraw := True;
  {$ENDIF}
  pnlButtons.Color := clBtnFace;
  // load button images
  btnAdd.LoadGlyphFromResourceName(HInstance, 'arrow_right');
  btnRemove.LoadGlyphFromResourceName(HInstance, 'arrow_left');
  btnMoveUp.LoadGlyphFromResourceName(HInstance, 'arrow_up');
  btnMoveDown.LoadGlyphFromResourceName(HInstance, 'arrow_down');
  btnClear.LoadGlyphFromResourceName(HINSTANCE,'menu_close');
  sbAddDivider.LoadGlyphFromResourceName(HINSTANCE, 'menu_divider16');

  btnAdd.Hint      := lisCoolBarAddSelected;
  btnRemove.Hint   := lisCoolBarRemoveSelected;
  btnMoveUp.Hint   := lisCoolBarMoveSelectedUp;
  btnMoveDown.Hint := lisCoolBarMoveSelectedDown;
  sbAddDivider.Hint:= lisCoolBarAddDivider;
  btnClear.Hint    := lisCoolBarClearSelection;

  TV.Images := IDEImages.Images_16;
  lvToolbar.SmallImages := IDEImages.Images_16;
  // default image to be used when none is available
  defImageIndex := IDEImages.LoadImage(16, 'execute');
  // Image for divider
  divImageIndex := IDEImages.Images_16.Add(sbAddDivider.Glyph,nil);

  MainList := TStringList.Create;
  MainList.OwnsObjects:= True; // it should be the default, but just to make sure...
  Image := TBitmap.Create;
  pnlButtons.Helpbutton.OnClick := @btnHelpClick;
  SetupCaptions;
  LoadCategories;
end;

procedure TToolBarConfig.FormDestroy(Sender: TObject);
begin
  MainList.Free;
  Image.Free;
end;

procedure TToolBarConfig.btnClearClick(Sender: TObject);
begin
  lvToolbar.Selected := nil;
end;

procedure TToolBarConfig.btnHelpClick(Sender: TObject);
begin
  ShowMessageFmt('%s%s%s%s%s%s%s', [lisCoolBarHelp1, LineEnding, lisCoolBarHelp2, LineEnding,
                                    lisCoolBarHelp3, LineEnding, lisCoolBarHelp4]);
end;

procedure TToolBarConfig.btnShowClick(Sender: TObject);
begin
  lvToolbar.Columns[1].Visible:= true;
end;

procedure TToolBarConfig.btnHideClick(Sender: TObject);
begin
  lvToolbar.Columns[1].Visible:= false;
end;


procedure TToolBarConfig.lbToolbarSelectionChange(Sender: TObject);
var
  i: Integer;
begin
  i := lvToolbar.ItemIndex;
  btnRemove.Enabled := i > -1;
  btnMoveUp.Enabled := i > 0;
  btnMoveDown.Enabled := (i > -1) and (i < lvToolbar.Items.Count-1);
end;

procedure TToolBarConfig.TVSelectionChanged(Sender: TObject);
var
  n: TTreeNode;
begin
  n := TV.Selected;
  btnAdd.Enabled := (Assigned(n) and Assigned(n.Data));
end;

function TToolBarConfig.GetMainListIndex(Item: TListItem): Integer;
var
  I: Integer;
begin
  for I:= 0 to MainList.Count -1 do
    if TLvItem(MainList.Objects[I]).LvIndex = Item.Index then
      Exit(I);
  Result := -1;
end;

procedure TToolBarConfig.InsertMainListItem(Item,NextItem: TListItem);
var
  I,J: Integer;
  aMainListItem: TLvItem;
begin
  aMainListItem := TLvItem.Create;
  aMainListItem.Item := TIDEMenuItem(Item.Data);
  aMainListItem.LvIndex := Item.Index;
  if NextItem = Nil then
    MainList.AddObject(Item.Caption,aMainListItem)
  else begin
    I := GetMainListIndex(NextItem);
    MainList.InsertObject(I,Item.Caption,aMainListItem);
    for J := I+1 to MainList.Count -1 do begin
      aMainListItem := TLvItem(MainList.Objects[J]);
      aMainListItem.LvIndex:= aMainListItem.LvIndex +1;
    end;
  end;
end;

procedure TToolBarConfig.RemoveMainListItem(Item: TListItem);
var
  I,J: Integer;
  aMainListItem: TLvItem;
begin
  I := GetMainListIndex(Item);
  if I > -1 then begin
    MainList.Delete(I);
    for J := I to MainList.Count -1 do begin
      aMainListItem := TLvItem(MainList.Objects[J]);
      aMainListItem.LvIndex:= aMainListItem.LvIndex -1;
    end;
  end;
end;

procedure TToolBarConfig.ExchangeMainListItem(Item1, Item2: TListItem);
var
  MainIndex1,MainIndex2: Integer;
  aMainListItem: TLvItem;
begin
  MainIndex1:= GetMainListIndex(Item1);
  MainIndex2:= GetMainListIndex(Item2);
  MainList.Exchange(MainIndex1,MainIndex2);
  aMainListItem := TLvItem(MainList.Objects[MainIndex1]);
  aMainListItem.LvIndex:= Item1.Index;
  aMainListItem := TLvItem(MainList.Objects[MainIndex2]);
  aMainListItem.LvIndex:= Item2.Index;
end;

procedure TToolBarConfig.btnAddClick(Sender: TObject);
var
  n, nNext: TTreeNode;
  ACaption: string;
  lvItem: TListItem;
  anIndex: Integer;
begin
  n := TV.Selected;
  if (Assigned(n) and Assigned(n.Data)) then
  begin
    btnAdd.Enabled := False;
    anIndex:= lvToolbar.ItemIndex;
    ACaption:= TIDEMenuItem(n.Data).Caption;
    DeleteAmpersands(ACaption);
    if anIndex > -1 then
      lvItem            := lvToolbar.Items.Insert(lvToolbar.ItemIndex)
    else
      lvItem            := lvToolbar.Items.Add;
    lvItem.Caption      := ACaption;
    lvItem.Data         := n.Data;
    {$IF not DEFINED(LCLQt)}
    if n.ImageIndex > -1 then
      lvItem.ImageIndex   := n.ImageIndex
    else
      lvItem.ImageIndex   := defImageIndex;
    {$ENDIF}
    //lvItem.SubItems.Add(IntToStr(CurrProfile));
    if anIndex > -1 then begin
      // clear previous selection to avoid double sel in Qt
      lvToolbar.Selected := nil;
      lvToolbar.ItemIndex := lvItem.Index;
      InsertMainListItem(lvItem,lvToolbar.Items[anIndex]);
    end
    else begin
     // lvToolbar.ItemIndex := lvToolbar.Items.Count-1;
      InsertMainListItem(lvItem,Nil);
    end;
    lbToolbarSelectionChange(lblToolbar);
    nNext := TV.Selected.GetNext;
    TV.Selected.Visible:= False;
    if nNext <> nil then
      TV.Selected := nNext;
  end;
end;

procedure TToolBarConfig.btnRemoveClick(Sender: TObject);
Var
  mi: TIDEMenuItem;
  n: TTreeNode;
  I: Integer;
  lvItem: TListItem;
begin
  I := lvToolbar.ItemIndex;
  if I > -1 then begin
    lvItem := lvToolbar.Items[I];
    mi := TIDEMenuItem(lvItem.Data);
    RemoveMainListItem(lvItem);
    lvToolbar.Items.Delete(lvToolbar.ItemIndex);
    if I < lvToolbar.Items.Count then
      lvToolbar.Selected := lvToolbar.Items[I]; // Qt Workaround
    lbToolbarSelectionChange(lvToolbar);
    if assigned(mi) then begin
      n:= TV.Items.FindNodeWithData(mi);
      n.Visible:= True;
    end;
    TVSelectionChanged(TV);
  end;
end;

procedure TToolBarConfig.lvToolbarDrawItem(Sender: TCustomListView;
  AItem: TListItem; ARect: TRect; AState: TOwnerDrawState);
var
  ImageIndex: integer;
begin
  with Sender.Canvas do
  begin
    if AItem.Selected then
    begin
      Brush.Color := clHighlight;
      Font.Color := clHighlightText;
    end
    else begin
      Brush.Color := clDefault;
      Font.Color := clDefault;
    end;
    FillRect(ARect);

    if AItem.Caption = cDivider then
      ImageIndex := divImageIndex
    else if Assigned(AItem.Data) and (TIDEMenuItem(AItem.Data).ImageIndex > -1) then
      ImageIndex := TIDEMenuItem(AItem.Data).ImageIndex
    else
      ImageIndex := defImageIndex;
    Image.Clear;
    lvToolBar.SmallImages.GetBitmap(ImageIndex, Image);
    Draw(ARect.Left + 2, ARect.Top + 2, Image);

    TextOut(ARect.Left + 21, ARect.Top + 2, AItem.Caption);
  end;
end;

procedure TToolBarConfig.lvToolbarSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  lbToolbarSelectionChange(Sender);
  lbSelect.Caption:= IntToStr(lvToolbar.ItemIndex)+ ' / ' + IntToStr(lvToolbar.Items.Count);
  btnClear.Enabled:= lvToolbar.Selected <> nil;
  btnRemove.Enabled:= btnClear.Enabled;
end;


procedure TToolBarConfig.btnAddDividerClick(Sender: TObject);
var
  lvItem: TListItem;
  anIndex: Integer;
begin
  anIndex := lvToolbar.ItemIndex;
  if anIndex > -1 then
    lvItem := lvToolbar.Items.Insert(anIndex)
  else
    lvItem := lvToolbar.Items.Add;
  lvItem.Selected := False;
  lvItem.Caption:= cDivider;
  {$IF not DEFINED(LCLQt)}
  lvItem.ImageIndex:= divImageIndex;
  {$ENDIF}
  //lvItem.SubItems.Add(IntToStr(CurrProfile));
  if lvToolbar.ItemIndex > -1 then
    InsertMainListItem(lvItem,lvToolbar.Items[anIndex])
  else
    InsertMainListItem(lvItem,Nil);
end;

procedure TToolBarConfig.btnMoveDownClick(Sender: TObject);
var
  Index1,Index2: Integer;
begin
  if lvToolbar.ItemIndex = -1 then
    Exit;
  if lvToolbar.ItemIndex < lvToolbar.Items.Count - 1 then begin
    Index1 := lvToolbar.ItemIndex;
    Index2 := Index1+1;
    lvToolbar.Items.Exchange(Index1,Index2);
    ExchangeMainListItem(lvToolbar.Items[Index1],lvToolbar.Items[Index2]);
    lvToolbar.Items[Index1].Selected := False;
    lvToolbar.Items[Index2].Selected := False;
    lvToolbar.Selected := nil;
    lvToolbar.ItemIndex:= Index2;
  end;
end;

procedure TToolBarConfig.btnMoveUpClick(Sender: TObject);
var
  Index1,Index2: Integer;
begin
  if lvToolbar.ItemIndex = -1 then
    exit;
  if lvToolbar.ItemIndex > 0 then begin
    Index1:= lvToolbar.ItemIndex;
    Index2:= Index1-1;
    lvToolbar.Items.Exchange(Index1, Index2);
    ExchangeMainListItem(lvToolbar.Items[Index1],lvToolbar.Items[Index2]);
    lvToolbar.Items[Index1].Selected := False;
    lvToolbar.Items[Index2].Selected := False;
    lvToolbar.Selected := nil;
    lvToolbar.ItemIndex:= Index2;
  end;
end;

procedure TToolBarConfig.SetupCaptions;
begin
  Caption               := lisToolbarConfiguration;
  lblMenuTree.Caption   := lisCoolbarAvailableCommands;
  lblToolbar.Caption    := lisCoolbarToolbarCommands;
end;

procedure TToolBarConfig.LoadCategories;
var
  i: integer;
begin
  TV.Items.BeginUpdate;
  try
    TV.Items.Clear;
    for i := 0 to IDEMenuRoots.Count-1 do
      AddMenuItem(nil, IDEMenuRoots[i],0);
  finally
    TV.Items.EndUpdate;
  end;
end;

procedure TToolBarConfig.AddMenuItem(ParentNode: TTreeNode; Item: TIDEMenuItem; Level: Integer);
var
  n: TTreeNode;
  i: integer;
  sec: TIDEMenuSection;
  ACaption: string;
  hasCaption: boolean;
begin
  if Item is TIDEMenuSection then
  begin
    if Item.Name <> Item.Caption then hasCaption:= true
    else hasCaption:= false;
    sec := (Item as TIDEMenuSection);
    if sec.Count > 0 then begin // skip empty sections
      if Level= 0 then ACaption:= RootNodeCaption(Item)
      else begin
        if hasCaption then ACaption:= Item.Caption
        else ACaption:= '---';
      end;
      DeleteAmpersands(ACaption);
      if (Level > 0) and ( not hasCaption) then n:= ParentNode
      else begin
        n := TV.Items.AddChild(ParentNode, Format('%s', [ACaption]));
        n.ImageIndex := Item.ImageIndex;
        n.SelectedIndex := Item.ImageIndex;
      end;
      for i := 0 to sec.Count-1 do
        AddMenuItem(n, sec.Items[i],Level+1);
    end;
  end
  else begin
    if Item.Caption <> '-' then begin // workaround for HTML Editor dividers
      ACaption:= Item.Caption;
      DeleteAmpersands(ACaption);
      ACaption:= ACaption + GetShortcut(Item);
      n := TV.Items.AddChild(ParentNode, Format('%s', [ACaption]));
      n.ImageIndex := Item.ImageIndex;
      n.SelectedIndex := Item.ImageIndex;
      n.Data := Item;
    end;
  end;
end;

function TToolBarConfig.RootNodeCaption(Item: TIDEMenuItem): string;
var
  AName: string;
begin
  AName:= Item.Caption;
  case AName of
    'IDEMainMenu':            Result := lisCoolbarIDEMainMenu;    // mnuMain
    'SourceTab':              Result := lisCoolbarSourceTab;      // SourceTabMenuRootName
    'SourceEditor':           Result := lisCoolbarSourceEditor;   // SourceEditorMenuRootName
    'Messages':               Result := lisCoolbarMessages;       // MessagesMenuRootName
    'Code Explorer':          Result := lisCoolbarCodeExplorer;   // CodeExplorerMenuRootName
    'CodeTemplates':          Result := lisCoolbarCodeTemplates;  // CodeTemplatesMenuRootName
    'Designer':               Result := lisCoolbarDesigner;       // DesignerMenuRootName
    'PackageEditor':          Result := lisCoolbarPackageEditor;  // PackageEditorMenuRootName
    'PackageEditorFiles':     Result := lisCoolbarPackageEditorFiles // PackageEditorMenuFilesRootName
    else                      Result := Item.Caption;
  end;
end;

procedure TToolBarConfig.AddListItem(Item: TIDEMenuItem);
var
  aListItem: TLvItem;
begin
  aListItem := TLvItem.Create;
  if assigned(Item) then begin
    aListItem.Item := Item;
    MainList.AddObject(Item.Caption,aListItem);
  end
  else begin
    aListItem.Item := nil;
    MainList.AddObject(cDivider,aListItem);
  end;
end;

procedure TToolBarConfig.AddToolBarItem(Item: TIDEMenuItem);
Var
  n: TTreeNode;
  ACaption: string;
  lvItem: TListItem;
begin
  if Assigned(Item) then begin
    ACaption:= Item.Caption;
    DeleteAmpersands(ACaption);
    ACaption:= ACaption+GetShortcut(Item);
    lvItem := lvToolbar.Items.Add;
    lvItem.Caption:= ACaption;
    lvItem.Data:= Item;
    {$IF not DEFINED(LCLQt)}
    if Item.ImageIndex > -1 then
      lvItem.ImageIndex:= Item.ImageIndex
    else
      lvItem.ImageIndex:= defImageIndex;
    {$ENDIF}
   // lvItem.SubItems.Add(IntToStr(PMask));
    n:= TV.Items.FindNodeWithData(Item);
    n.Visible:= False;
  end;
end;

procedure TToolBarConfig.AddDivider;
var
  lvItem: TListItem;
begin
  lvItem := lvToolbar.Items.Add;
  lvItem.Caption:= cDivider;
  {$IF not DEFINED(LCLQt)}
  lvItem.ImageIndex:= divImageIndex;
  {$ENDIF}
  //  lvItem.SubItems.Add(IntToStr(PMask));
end;

procedure TToolBarConfig.FillToolBar;
var
  I: Integer;
  aListItem: TLvItem;
  aCaption: string;
  mi: TIDEMenuItem;
begin
  for I:= 0 to MainList.Count -1 do
  begin
    aListItem := TLvItem(MainList.Objects[I]);
    mi := aListItem.Item;
    aCaption := MainList.Strings[I];
    if aCaption = cDivider then
      AddDivider
    else
      AddToolBarItem(mi);
    aListItem.LvIndex:= lvToolbar.Items.Count - 1;
  end;
end;

procedure TToolBarConfig.LoadSettings(SL: TStringList);
var
  I: Integer;
  Value: string;
  MI: TIDEMenuItem;
begin
  for I := 0 to SL.Count - 1 do
  begin
    Value := SL.Strings[I];
    if Value = '' then Continue;
    if Value = cDivider then
      MI := nil
    else
      MI := IDEMenuRoots.FindByPath(Value, false);
    AddListItem(MI);
  end;
  FillToolBar;
end;

procedure TToolBarConfig.SaveSettings(SL: TStringList);
var
  lvItem: TLvItem;
  I: Integer;
begin
  SL.Clear;
  for i := 0 to MainList.Count - 1 do
  begin
    lvItem := TLvItem(MainList.Objects[I]);
    if MainList[I] = cDivider then
      SL.Add(cDivider)
    else
      SL.Add(lvItem.Item.GetPath);
  end;
end;


end.

