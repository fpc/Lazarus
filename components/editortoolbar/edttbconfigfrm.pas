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
  Buttons, StdCtrls, ComCtrls, Menus, ButtonPanel, MenuIntf, editortoolbar_str,
  TreeFilterEdit;

type
  { TLvItem }
  TLvItem = class (TObject)
    Item: TIDEMenuItem;
    LvIndex: Integer;
    Profile: Integer;
  end;

  { TEdtTbConfigForm }

  TEdtTbConfigForm = class(TForm)
    btnAdd: TSpeedButton;
    btnMoveDown: TSpeedButton;
    btnMoveUp: TSpeedButton;
    btnRemove: TSpeedButton;
    btnShow: TButton;
    btnHide: TButton;
    cbPos: TComboBox;
    cbProfile: TComboBox;
    lblProfile: TLabel;
    lbSelect: TLabel;
    lblpos: TLabel;
    lblMenuTree: TLabel;
    lblToolbar: TLabel;
    lvToolbar: TListView;
    miAll: TMenuItem;
    miDesign: TMenuItem;
    miDebug: TMenuItem;
    miHTML: TMenuItem;
    miCustom: TMenuItem;
    FilterEdit: TTreeFilterEdit;
    pnlButtons: TButtonPanel;
    puMenuItems: TPopupMenu;
    sbAddDivider: TSpeedButton;
    btnClear: TSpeedButton;
    Splitter1: TSplitter;
    TV: TTreeView;
    procedure btnClearClick(Sender: TObject);
    procedure btnShowClick(Sender: TObject);
    procedure btnHideClick(Sender: TObject);
    procedure cbPosChange(Sender: TObject);
    procedure cbProfileChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure lbToolbarSelectionChange(Sender: TObject; User: boolean);
    procedure btnAddClick(Sender: TObject);
    procedure btnAddDividerClick(Sender: TObject);
    procedure btnMoveDownClick(Sender: TObject);
    procedure btnMoveUpClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure lvToolbarDrawItem(Sender: TCustomListView; AItem: TListItem;
      ARect: TRect; AState: TOwnerDrawState);
    procedure lvToolbarSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure miAllClick(Sender: TObject);
    procedure miCustomClick(Sender: TObject);
    procedure miDebugClick(Sender: TObject);
    procedure miDesignClick(Sender: TObject);
    procedure miHTMLClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure pnlButtonsResize(Sender: TObject);
    procedure puMenuItemsPopup(Sender: TObject);
    procedure TVSelectionChanged(Sender: TObject);
  private
    FToolBarPos: string;
    FToolBarShow: boolean;
    Image: TBitMap;
    defImageIndex: integer;
    divImageIndex: Integer;
    // Main list related entries
    MainList: TStringList;
    function GetMainListIndex(Item: TListItem): Integer;
    procedure UpdateMainListProfile(Item: TListItem);
    procedure InsertMainListItem (Item,NextItem: TListItem);
    procedure RemoveMainListItem (Item:TListItem);
    procedure ExchangeMainListItem (Item1,Item2: TListItem);

    procedure SetupCaptions;
    procedure LoadCategories;
    procedure LoadSettings;
    procedure LoadStyleSettings;
    procedure SaveSettings;
    procedure AddMenuItem(ParentNode: TTreeNode; Item: TIDEMenuItem; Level: Integer);
    function RootNodeCaption(Item: TIDEMenuItem): string;
    procedure AddListItem(Item: TIDEMenuItem; PMask: Integer);
    procedure AddToolBarItem(Item: TIDEMenuItem; PMask: Integer);
    procedure AddDivider(PMask: Integer);
    procedure FillToolBar;
    procedure ReloadToolBar;
  public
    class function Execute: boolean;
    class procedure Setup;
    class procedure UpdateVisible(NewStatus: Boolean);
  end; 

Var
  sPosValues: array[0..3] of string = ('Top','Bottom','Right','Left');
  sLocalizedPosValues: array[0..3] of string;
  CurrProfile: Integer;

implementation

{$R *.lfm}

uses
  editortoolbar_impl, LazConfigStorage, BaseIDEIntf, LazIDEIntf, IDEImagesIntf,LCLProc;

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
  //we have to ownerdraw the listview on qt
  {$IFDEF LCLQT}
  lvToolbar.OwnerDraw := True;
  {$ENDIF}

  // load button images
  btnAdd.LoadGlyphFromResourceName(HInstance, 'arrow_right');
  btnRemove.LoadGlyphFromResourceName(HInstance, 'arrow_left');
  btnMoveUp.LoadGlyphFromResourceName(HInstance, 'arrow_up');
  btnMoveDown.LoadGlyphFromResourceName(HInstance, 'arrow_down');
  btnClear.LoadGlyphFromResourceName(HINSTANCE,'menu_close');
  sbAddDivider.LoadGlyphFromResourceName(HINSTANCE, 'menu_divider16');

  btnAdd.Hint      := rsAddSelected;
  btnRemove.Hint   := rsRemoveSelected;
  btnMoveUp.Hint   := rsMoveSelectedUp;
  btnMoveDown.Hint := rsMoveSelectedDown;
  sbAddDivider.Hint:= rsAddDivider;
  btnClear.Hint    := rsClearSelection;

  TV.Images := IDEImages.Images_16;
  lvToolbar.SmallImages := IDEImages.Images_16;
  // default image to be used when none is available
  defImageIndex := IDEImages.LoadImage(16, 'execute16');
  // Image for divider
  divImageIndex := IDEImages.Images_16.Add(sbAddDivider.Glyph,nil);

  MainList := TStringList.Create;
  MainList.OwnsObjects:= True; // it should be the default, but just to make sure...
  Image := TBitmap.Create;
  SetupCaptions;
  LoadStyleSettings;
  LoadCategories;
  LoadSettings;
end;

procedure TEdtTbConfigForm.FormDestroy(Sender: TObject);
begin
  Image.Free;
  MainList.Free;
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

procedure TEdtTbConfigForm.cbProfileChange(Sender: TObject);
begin
  CurrProfile:=  ProfileMask[cbProfile.ItemIndex];
  ReloadToolBar;
end;

procedure TEdtTbConfigForm.btnClearClick(Sender: TObject);
begin
  lvToolbar.Selected := nil;
end;

procedure TEdtTbConfigForm.HelpButtonClick(Sender: TObject);
begin
  ShowMessageFmt('%s%s%s%s%s%s%s', [rsHelp1,LineEnding,rsHelp2,LineEnding,rsHelp3,LineEnding,rsHelp4]);
end;

procedure TEdtTbConfigForm.btnShowClick(Sender: TObject);
begin
  lvToolbar.Columns[1].Visible:= true;
end;

procedure TEdtTbConfigForm.btnHideClick(Sender: TObject);
begin
  lvToolbar.Columns[1].Visible:= false;
end;

procedure TEdtTbConfigForm.lbToolbarSelectionChange(Sender: TObject; User: boolean);
var
  i: Integer;
begin
  i := lvToolbar.ItemIndex;
  btnRemove.Enabled := i > -1;
  btnMoveUp.Enabled := i > 0;
  btnMoveDown.Enabled := (i > -1) and (i < lvToolbar.Items.Count-1);
end;

procedure TEdtTbConfigForm.TVSelectionChanged(Sender: TObject);
var
  n: TTreeNode;
begin
  n := TV.Selected;
  btnAdd.Enabled := (Assigned(n) and Assigned(n.Data));
end;

function TEdtTbConfigForm.GetMainListIndex(Item: TListItem): Integer;
var
  I: Integer;
begin
  for I:= 0 to MainList.Count -1 do begin
    if TLvItem(MainList.Objects[I]).LvIndex = Item.Index then begin
      Result := I;
      Exit;
    end;
  end;
  Result := -1;
end;

procedure TEdtTbConfigForm.UpdateMainListProfile(Item: TListItem);
var
  I: Integer;
begin
  I := GetMainListIndex(Item);
  if I > -1 then TLvItem(MainList.Objects[I]).Profile:= StrToInt(Item.SubItems[0]);
end;

procedure TEdtTbConfigForm.InsertMainListItem(Item,NextItem: TListItem);
var
  I,J: Integer;
  aMainListItem: TLvItem;
begin
  aMainListItem := TLvItem.Create;
  aMainListItem.Item := TIDEMenuItem(Item.Data);
  aMainListItem.Profile := StrToInt(Item.SubItems[0]);
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

procedure TEdtTbConfigForm.RemoveMainListItem(Item: TListItem);
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

procedure TEdtTbConfigForm.ExchangeMainListItem(Item1, Item2: TListItem);
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

procedure TEdtTbConfigForm.btnAddClick(Sender: TObject);
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
    if anIndex > -1 then begin
      lvItem            := lvToolbar.Items.Insert(lvToolbar.ItemIndex);
    end
    else begin
      lvItem            := lvToolbar.Items.Add;
    end;
    lvItem.Caption      := ACaption;
    lvItem.Data         := n.Data;
    lvItem.SubItems.Add(IntToStr(CurrProfile));
    {$IF not DEFINED(LCLQt)}
    if n.ImageIndex > -1 then
      lvItem.ImageIndex   := n.ImageIndex
    else
      lvItem.ImageIndex   := defImageIndex;
    {$ENDIF}
    if anIndex > -1 then begin
      // clear previous selection to avoid double sel in Qt
      lvToolbar.Selected := nil;
      lvToolbar.ItemIndex := lvItem.Index;
      InsertMainListItem(lvItem,lvToolbar.Items[anIndex]);
    end
    else begin
      InsertMainListItem(lvItem,Nil);
    end;
    lbToolbarSelectionChange(lblToolbar, False);
    nNext := TV.Selected.GetNext;
    TV.Selected.Visible:= False;
    if nNext <> nil then
      TV.Selected := nNext;
  end;
end;

procedure TEdtTbConfigForm.btnRemoveClick(Sender: TObject);
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
    lbToolbarSelectionChange(lvToolbar, False);
    if assigned(mi) then begin
      n:= TV.Items.FindNodeWithData(mi);
      n.Visible:= True;
    end;
    TVSelectionChanged(TV);
  end;
end;

procedure TEdtTbConfigForm.lvToolbarDrawItem(Sender: TCustomListView;
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
    else
    begin
      if Assigned(AItem.Data) then
      begin
        if TIDEMenuItem(AItem.Data).ImageIndex > -1 then
          ImageIndex := TIDEMenuItem(AItem.Data).ImageIndex
        else
          ImageIndex := defImageIndex;
      end
      else
        ImageIndex := defImageIndex;
    end;
    Image.Clear;
    lvToolBar.SmallImages.GetBitmap(ImageIndex, Image);
    Draw(ARect.Left + 2, ARect.Top + 2, Image);

    TextOut(ARect.Left + 21, ARect.Top + 2, AItem.Caption);
  end;
end;

procedure TEdtTbConfigForm.lvToolbarSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  lbToolbarSelectionChange(Sender,False);
  lbSelect.Caption:= IntToStr(lvToolbar.ItemIndex)+ ' / ' + IntToStr(lvToolbar.Items.Count);
  btnClear.Enabled:= lvToolbar.Selected <> nil;
  btnRemove.Enabled:= btnClear.Enabled;
end;

procedure TEdtTbConfigForm.miAllClick(Sender: TObject);
begin
  if lvToolbar.ItemIndex = -1 then begin
    puMenuItems.Close;
    Exit
  end;
  if miAll.Checked then begin
    lvToolbar.Selected.SubItems[0] := IntToStr(iAll);
    miDesign.Checked := False;
    miDebug.Checked := False;
    miHTML.Checked := False;
    miCustom.Checked := False;
  end
  else
    lvToolbar.Selected.SubItems[0] := '0';
  UpdateMainListProfile(lvToolbar.Selected);
end;

procedure TEdtTbConfigForm.miCustomClick(Sender: TObject);
var
  aMask: Integer;
begin
  if lvToolbar.ItemIndex = -1 then begin
    puMenuItems.Close;
    Exit
  end;
  aMask:= StrToInt(lvToolbar.Selected.SubItems[0]);
  if (aMask and iCustom) = 0 then
    aMask := aMask or iCustom
  else
    aMask := aMask and (not iCustom);
  miCustom.Checked:= (aMask and iCustom) <> 0;
  lvToolbar.Selected.SubItems[0] := IntToStr(aMask);
  UpdateMainListProfile(lvToolbar.Selected);
end;

procedure TEdtTbConfigForm.miDebugClick(Sender: TObject);
var
  aMask: Integer;
begin
  if lvToolbar.ItemIndex = -1 then begin
    puMenuItems.Close;
    Exit
  end;
  aMask:= StrToInt(lvToolbar.Selected.SubItems[0]);
  if (aMask and iDebug) = 0 then
    aMask := aMask or iDebug
  else
    aMask := aMask and (not iDebug);
  miDebug.Checked:= (aMask and iDebug) <> 0;
  lvToolbar.Selected.SubItems[0] := IntToStr(aMask);
  UpdateMainListProfile(lvToolbar.Selected);
end;

procedure TEdtTbConfigForm.miDesignClick(Sender: TObject);
var
  aMask: Integer;
begin
  if lvToolbar.ItemIndex = -1 then begin
    puMenuItems.Close;
    Exit
  end;
  aMask:= StrToInt(lvToolbar.Selected.SubItems[0]);
  if (aMask and iDesign) = 0 then
    aMask := aMask or iDesign
  else
    aMask := aMask and (not iDesign);
  miDesign.Checked:= (aMask and iDesign) <> 0;
  lvToolbar.Selected.SubItems[0] := IntToStr(aMask);
  UpdateMainListProfile(lvToolbar.Selected);
end;

procedure TEdtTbConfigForm.miHTMLClick(Sender: TObject);
var
  aMask: Integer;
begin
  if lvToolbar.ItemIndex = -1 then begin
    puMenuItems.Close;
    Exit
  end;
  aMask:= StrToInt(lvToolbar.Selected.SubItems[0]);
  if (aMask and iHTML) = 0 then
    aMask := aMask or iHTML
  else
    aMask := aMask and (not iHTML);
  miHTML.Checked:= (aMask and iHTML) <> 0;
  lvToolbar.Selected.SubItems[0] := IntToStr(aMask);
  UpdateMainListProfile(lvToolbar.Selected);
end;

procedure TEdtTbConfigForm.puMenuItemsPopup(Sender: TObject);
var
  aMask: Integer;
begin
 if lvToolbar.ItemIndex = -1 then begin
   puMenuItems.Close;
   Exit
 end;
  aMask:= StrToInt(lvToolbar.Selected.SubItems[0]);
  if aMask = iAll then begin
    miAll.Checked:= True;
    miDesign.Checked := False;
    miDebug.Checked := False;
    miHTML.Checked := False;
    miCustom.Checked := False;
  end
  else begin
    miAll.Checked:= False;
    if (aMask and iDesign) <> 0 then miDesign.Checked:= True
    else miDesign.Checked:= False;
    if (aMask and iDebug) <> 0 then miDebug.Checked:= True
    else miDebug.Checked:= False;
    if (aMask and iHTML) <> 0 then miHTML.Checked:= True
    else miHTML.Checked:= False;
    if (aMask and iCustom) <> 0 then miCustom.Checked:= True
    else miCustom.Checked:= False;
  end;
end;

procedure TEdtTbConfigForm.btnAddDividerClick(Sender: TObject);
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
  lvItem.SubItems.Add(IntToStr(CurrProfile));
  if lvToolbar.ItemIndex > -1 then
    InsertMainListItem(lvItem,lvToolbar.Items[anIndex])
  else
    InsertMainListItem(lvItem,Nil);
end;

procedure TEdtTbConfigForm.btnMoveDownClick(Sender: TObject);
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

procedure TEdtTbConfigForm.btnMoveUpClick(Sender: TObject);
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

procedure TEdtTbConfigForm.OKButtonClick(Sender: TObject);
begin
  SaveSettings;
end;

procedure TEdtTbConfigForm.pnlButtonsResize(Sender: TObject);
begin
  cbProfile.Left := (pnlButtons.Width - cbProfile.Width) div 2;
  lblProfile.Left := cbProfile.Left - lblProfile.Width - 10;
end;

procedure TEdtTbConfigForm.SetupCaptions;
var
  i: integer;
begin
  Caption                         := rsEditorToolbarConfigForm;
  pnlButtons.HelpButton.Caption   := rsHelp;
  pnlButtons.OKButton.Caption     := rsOK;
  pnlButtons.CancelButton.Caption := rsCancel;
  lblMenuTree.Caption             := rsMenuTree;
  lblToolbar.Caption              := rsToolbar;
  lblpos.Caption                  := rsPosition;
  lblProfile.Caption              := rsProfile;

  sLocalizedPosValues[0] := rsTop;
  sLocalizedPosValues[1] := rsBottom;
  sLocalizedPosValues[2] := rsRight;
  sLocalizedPosValues[3] := rsLeft;

  for i := 0 to high(sLocalizedPosValues) do
  begin
   cbPos.Items[i] := sLocalizedPosValues[i]; // localized
  end;
  for i := 0 to high(sLocalizedProfileNames) do
  begin
    cbProfile.Items[i] := sLocalizedProfileNames[i];
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
      AddMenuItem(nil, IDEMenuRoots[i],0);
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
  aProfileMask: Integer;
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
      AddListItem(mi,iAll);
      mi := ms.FindByName('itmJumpForward');
      AddListItem(mi,iAll);
    end
    else begin
     for i := 0 to c - 1 do
     begin
       value := cfg.GetValue('Button' + Format('%2.2d', [i+1]) + '/Value', '');
       aProfileMask := cfg.GetValue('Button' + Format('%2.2d', [i+1]) + '/Profile', iAll);
       if value <> '' then
       begin
        if value = cDivider then
        begin
          AddListItem(nil,aProfileMask);
          Continue;
        end;
        mi := IDEMenuRoots.FindByPath(value, false);
        AddListItem(mi,aProfileMask);
       end;
     end;
    end;
    value := cfg.GetValue('Position','Top');
    FToolbarPos	       := value;
    FToolBarShow       := cfg.GetValue('Visible',true);
  finally
    cfg.Free;
  end;
  FillToolBar;
  i := IndexFromEnglish(FToolBarPos);
  cbPos.Text:= sLocalizedPosValues[i];
end;

procedure TEdtTbConfigForm.LoadStyleSettings;
var
  cfg: TConfigStorage;
  value: Integer;
begin
  cfg := GetIDEConfigStorage(cSettingsFile, True);
  try
    value         := cfg.GetValue('Profile',iAll);
    CurrProfile   := value;
    value         := GetProfileIndex(value);
    cbProfile.Text:= sLocalizedProfileNames[value];
  finally
    cfg.Free;
  end;
end;

procedure TEdtTbConfigForm.SaveSettings;
var
  i: integer;
  cfg: TConfigStorage;
  lvItem: TLvItem;
  aProfileMask: Integer;
begin
  cfg := GetIDEConfigStorage(cSettingsFile, False);
  try
    cfg.SetValue('Count', MainList.Count);
    for i := 0 to MainList.Count - 1 do begin
      lvItem := TLvItem(MainList.Objects[I]);
      aProfileMask:= lvItem.Profile;
      if MainList[I] = cDivider then
        cfg.SetDeleteValue('Button' + Format('%2.2d', [i+1]) + '/Value', cDivider, '')
      else
        cfg.SetDeleteValue('Button' + Format('%2.2d', [i+1]) + '/Value', lvItem.Item.GetPath, '');
      cfg.SetDeleteValue('Button' + Format('%2.2d', [i+1]) + '/Profile', aProfileMask,iAll);
    end;
    cfg.SetValue('Position', FToolbarPos);
    cfg.SetValue('Visible',FToolBarShow);
    cfg.SetValue('Profile',CurrProfile);
    cfg.WriteToDisk;
  finally
    cfg.Free;
  end;
end;

procedure TEdtTbConfigForm.AddMenuItem(ParentNode: TTreeNode; Item: TIDEMenuItem; Level: Integer);
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
      ACaption:= ACaption+GetShortcut(Item);
      n := TV.Items.AddChild(ParentNode, Format('%s', [ACaption]));
      n.ImageIndex := Item.ImageIndex;
      n.SelectedIndex := Item.ImageIndex;
      n.Data := Item;

    end;
  end;
end;

function TEdtTbConfigForm.RootNodeCaption(Item: TIDEMenuItem): string;
var
  AName: string;
begin
AName:= Item.Caption;
case AName of
  'IDEMainMenu':            Result := rsIDEMainMenu;    // mnuMain
  'SourceTab':              Result := rsSourceTab;      // SourceTabMenuRootName
  'SourceEditor':           Result := rsSourceEditor;   // SourceEditorMenuRootName
  'Messages':               Result := rsMessages;       // MessagesMenuRootName
  'Code Explorer':          Result := rsCodeExplorer;   // CodeExplorerMenuRootName
  'CodeTemplates':          Result := rsCodeTemplates;  // CodeTemplatesMenuRootName
  'Designer':               Result := rsDesigner;       // DesignerMenuRootName
  'PackageEditor':          Result := rsPackageEditor;  // PackageEditorMenuRootName
  'PackageEditorFiles':     Result := rsPackageEditorFiles // PackageEditorMenuFilesRootName
  else                      Result := Item.Caption;
end;
end;

procedure TEdtTbConfigForm.AddListItem(Item: TIDEMenuItem; PMask: Integer);
var
  aListItem: TLvItem;
begin
  aListItem := TLvItem.Create;
  if assigned(Item) then begin
    aListItem.Item := Item;
    aListItem.Profile:= Pmask;
    MainList.AddObject(Item.Caption,aListItem);
  end
  else begin
    aListItem.Item := nil;
    aListItem.Profile:= PMask;
    MainList.AddObject(cDivider,aListItem);
  end;

end;

procedure TEdtTbConfigForm.AddToolBarItem(Item: TIDEMenuItem; PMask: Integer);
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
    lvItem.SubItems.Add(IntToStr(PMask));
    n:= TV.Items.FindNodeWithData(Item);
    n.Visible:= False;
  end;
end;

procedure TEdtTbConfigForm.AddDivider(PMask: Integer);
var
  lvItem: TListItem;
begin
  lvItem := lvToolbar.Items.Add;
  lvItem.Caption:= cDivider;
  {$IF not DEFINED(LCLQt)}
  lvItem.ImageIndex:= divImageIndex;
  {$ENDIF}
  lvItem.SubItems.Add(IntToStr(PMask));
end;

procedure TEdtTbConfigForm.FillToolBar;
var
  I: Integer;
  aListItem: TLvItem;
  aCaption: string;
  aPMask: Integer;
  mi: TIDEMenuItem;
  canShow: Boolean;
begin
  for I:= 0 to MainList.Count -1 do begin
    aListItem := TLvItem(MainList.Objects[I]);
    aPMask    := aListItem.Profile;
    canShow := (CurrProfile = iAll) or ((aPMask and CurrProfile) <> 0);
    if canShow then begin
      mi := aListItem.Item;
      aCaption  := MainList.Strings[I];
      if aCaption = cDivider then AddDivider(aPMask)
      else AddToolBarItem(mi,aPMask);
      aListItem.LvIndex:= lvToolbar.Items.Count-1;
    end;
  end;
end;

procedure TEdtTbConfigForm.ReloadToolBar;
begin
  lvToolbar.Clear;
  FillToolBar;
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

class procedure TEdtTbConfigForm.Setup;
var
  frm: TEdtTbConfigForm;
begin
  frm := TEdtTbConfigForm.Create(nil);
  try
    frm.SaveSettings;
  finally
    frm.Free;
  end;
end;

class procedure TEdtTbConfigForm.UpdateVisible(NewStatus: Boolean);
var
  frm: TEdtTbConfigForm;
begin
  frm := TEdtTbConfigForm.Create(nil);
  try
    frm.FToolBarShow:= NewStatus;
    frm.SaveSettings;
  finally
    frm.Free;
  end;
end;

end.

