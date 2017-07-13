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
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}

unit ToolbarConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  LCLProc, LclIntf, Controls, Forms, Graphics, ExtCtrls, Buttons, StdCtrls,
  ComCtrls, Menus, ButtonPanel,
  // LazControls
  TreeFilterEdit,
  // LazUtils
  Laz2_XMLCfg,
  // IdeIntf
  ToolBarIntf, IDEImagesIntf,
  // IDE
  LazarusIDEStrConsts;

const
  IDEToolBarConfigVersion = 1;
  // 1 added file version in config

type
  { TLvItem }
  TLvItem = class (TObject)
    Item: TIDEButtonCommand;
    LvIndex: Integer;
  end;

  { TToolBarConfig }

  TToolBarConfig = class(TForm)
    btnAdd: TSpeedButton;
    btnAddDivider: TSpeedButton;
    btnCancel: TButton;
    btnHelp: TBitBtn;
    btnMoveDown: TSpeedButton;
    btnMoveUp: TSpeedButton;
    btnOK: TButton;
    btnRemove: TSpeedButton;
    FilterEdit: TTreeFilterEdit;
    lblMenuTree: TLabel;
    lblToolbar: TLabel;
    lblSelect: TLabel;
    lvToolbar: TListView;
    miAll: TMenuItem;
    miCustom: TMenuItem;
    miDebug: TMenuItem;
    miDesign: TMenuItem;
    miHTML: TMenuItem;
    pnlButtons: TButtonPanel;
    Splitter1: TSplitter;
    TV: TTreeView;
    procedure btnHelpClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lvToolbarDblClick(Sender: TObject);
    procedure lvToolbarEnterExit(Sender: TObject);
    procedure TVDblClick(Sender: TObject);
    procedure UpdateButtonsState;
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
    procedure AddCommand;
    procedure AddTailItem;
    function GetMainListIndex(Item: TListItem): Integer;
    procedure InsertMainListItem(Item, NextItem: TListItem);
    procedure RemoveCommand;
    procedure RemoveMainListItem(Item: TListItem);
    procedure ExchangeMainListItem(Item1, Item2: TListItem);
    procedure SetupCaptions;
    procedure LoadCategories;
    procedure AddMenuItem(ParentNode: TTreeNode; Item: TIDEButtonCommand);
    function RootNodeCaption(Item: TIDEButtonCommand): string;
    procedure AddListItem(Item: TIDEButtonCommand);
    procedure AddToolBarItem(Item: TIDEButtonCommand);
    procedure AddDivider;
    procedure FillToolBar;
  public
    procedure LoadSettings(SL: TStringList);
    procedure SaveSettings(SL: TStringList);
  end;

  { TIDEToolBarOptionsBase }

  TIDEToolBarOptionsBase = class
  private
    FButtonNames: TStringList;
  protected
    procedure LoadButtonNames(XMLConfig: TXMLConfig; SubPath: String);
    procedure SaveButtonNames(XMLConfig: TXMLConfig; SubPath: String);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Equals(Opts: TIDEToolBarOptionsBase): boolean; overload;
    procedure Assign(Source: TIDEToolBarOptionsBase);
    //procedure Load(XMLConfig: TXMLConfig; Path: String);
    //procedure Save(XMLConfig: TXMLConfig; Path: String);
  published
    property ButtonNames: TStringList read FButtonNames; // write FButtonNames;
  end;

  { TIDEToolbarBase }

  TIDEToolbarBase = class(TComponent)
  private
  protected
    FToolBar: TToolBar;
    procedure AddButton(ACommand: TIDEButtonCommand);
    procedure AddDivider;
    procedure CopyFromOptions(Options: TIDEToolBarOptionsBase);
    procedure PositionAtEnd(AToolBar: TToolBar; AButton: TToolButton);
    procedure PostCopyOptions; virtual;
  public
    //constructor Create(AOwner: TComponent); override;
    //destructor Destroy; override;
    property ToolBar: TToolBar read FToolBar;
  end;

const
  cIDEToolbarDivider = '---------------';
  cTailItemCaption = '                                             ';

function ShowToolBarConfig(aNames: TStringList): TModalResult;


implementation

{$R *.lfm}

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

{ TToolBarConfig }

procedure TToolBarConfig.FormCreate(Sender: TObject);
begin
  inherited;
  //we have to ownerdraw the listview on qt
  {$IF DEFINED(LCLQT) OR DEFINED(LCLQT5)}
  lvToolbar.OwnerDraw := True;
  {$ENDIF}
  pnlButtons.Color := clBtnFace;
  lblSelect.Caption := '';
  // load button images
  TIDEImages.AssignImage(btnAdd.Glyph, 'arrow__darkgreen_right');
  TIDEImages.AssignImage(btnRemove.Glyph, 'arrow__darkred_left');
  TIDEImages.AssignImage(btnMoveUp.Glyph, 'arrow__darkgreen_up');
  TIDEImages.AssignImage(btnMoveDown.Glyph, 'arrow__darkgreen_down');
  TIDEImages.AssignImage(btnAddDivider.Glyph, 'menu_divider16');
  TIDEImages.AssignImage(FilterEdit.Glyph, 'btnfiltercancel');

  btnAddDivider.Caption := '---';

  btnAdd.Hint      := lisCoolBarAddSelected;
  btnRemove.Hint   := lisCoolBarRemoveSelected;
  btnMoveUp.Hint   := lisCoolBarMoveSelectedUp;
  btnMoveDown.Hint := lisCoolBarMoveSelectedDown;
  btnAddDivider.Hint:= lisCoolBarAddDivider;

  TV.Images := IDEImages.Images_16;
  lvToolbar.SmallImages := IDEImages.Images_16;
  // default image to be used when none is available
  defImageIndex := IDEImages.LoadImage('execute');
  // Image for divider
  divImageIndex := IDEImages.Images_16.Add(btnAddDivider.Glyph,nil);

  MainList := TStringList.Create;
  MainList.OwnsObjects:= True; // it should be the default, but just to make sure...
  Image := TBitmap.Create;
  SetupCaptions;
  LoadCategories;
end;

procedure TToolBarConfig.FormDestroy(Sender: TObject);
begin
  MainList.Free;
  Image.Free;
end;

procedure TToolBarConfig.lvToolbarDblClick(Sender: TObject);
begin
  RemoveCommand;
end;

procedure TToolBarConfig.lvToolbarEnterExit(Sender: TObject);
begin
  UpdateButtonsState;
end;

procedure TToolBarConfig.TVDblClick(Sender: TObject);
begin
  AddCommand;
end;

procedure TToolBarConfig.btnHelpClick(Sender: TObject);
begin
  OpenUrl('http://wiki.freepascal.org/IDE_Window:_Toolbar_Config');
end;

procedure TToolBarConfig.UpdateButtonsState;
var
  i: Integer;
begin
  i:=lvToolbar.ItemIndex;
  btnAdd.Enabled:=(Assigned(TV.Selected) and Assigned((TV.Selected).Data));
  btnRemove.Enabled:=(ActiveControl=lvToolbar) and (i>-1) and (i<lvToolbar.Items.Count-1);
  btnMoveUp.Enabled:=(ActiveControl=lvToolbar) and (i>0) and (i<lvToolbar.Items.Count-1);
  btnMoveDown.Enabled:=(ActiveControl=lvToolbar) and (i>-1) and (i<lvToolbar.Items.Count-2);
  btnAddDivider.Enabled:=(lvToolbar.Selected = nil) or (lvToolbar.Selected.Caption <> cIDEToolbarDivider);
end;

procedure TToolBarConfig.TVSelectionChanged(Sender: TObject);
begin
  UpdateButtonsState;
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

procedure TToolBarConfig.InsertMainListItem(Item, NextItem: TListItem);
var
  NextInd, I: Integer;
  aMainListItem: TLvItem;
begin
  aMainListItem := TLvItem.Create;
  aMainListItem.Item := TIDEButtonCommand(Item.Data);
  aMainListItem.LvIndex := Item.Index;
  Assert(Assigned(NextItem), 'TToolBarConfig.InsertMainListItem: NextItem = Nil.');
  NextInd := GetMainListIndex(NextItem);
  MainList.InsertObject(NextInd, Item.Caption, aMainListItem);
  for I := NextInd+1 to MainList.Count -1 do
  begin
    aMainListItem := TLvItem(MainList.Objects[I]);
    aMainListItem.LvIndex := aMainListItem.LvIndex +1;
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
      aMainListItem.LvIndex := aMainListItem.LvIndex -1;
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
begin
  AddCommand;
end;

procedure TToolBarConfig.AddCommand;
var
  n, nNext: TTreeNode;
  ACaption: string;
  lvItem: TListItem;
  anIndex: Integer;
begin
  n := TV.Selected;
  if (n = Nil) or (n.Data = Nil) then
    Exit;
  ACaption := TIDEButtonCommand(n.Data).Caption;
  DeleteAmpersands(ACaption);
  anIndex := lvToolbar.ItemIndex;
  if anIndex = -1 then
  begin
    anIndex := lvToolbar.Items.Count-1;    // Add before the last empty item.
    Assert(anIndex >= 0, 'TToolBarConfig.AddCommand: Index < 0.');
  end;
  lvItem := lvToolbar.Items.Insert(anIndex);
  lvItem.Caption := ACaption;
  lvItem.Data := n.Data;
  {$IF not DEFINED(LCLQt) and not DEFINED(LCLQt5)}
  if n.ImageIndex > -1 then
    lvItem.ImageIndex := n.ImageIndex
  else
    lvItem.ImageIndex := defImageIndex;
  {$ENDIF}
  //lvItem.SubItems.Add(IntToStr(CurrProfile));
  // clear previous selection to avoid double sel in Qt
  lvToolbar.Selected := nil;
  lvToolbar.ItemIndex := lvItem.Index;
  InsertMainListItem(lvItem, lvToolbar.Items[anIndex]);
  // Update selection in TreeView.
  nNext := TV.Selected.GetNext;
  TV.Selected.Visible:= False;
  if nNext <> nil then
    TV.Selected := nNext;
  UpdateButtonsState;
end;

procedure TToolBarConfig.btnAddDividerClick(Sender: TObject);
var
  lvItem: TListItem;
  anIndex: Integer;
begin
  anIndex := lvToolbar.ItemIndex;
  if anIndex = -1 then
  begin
    anIndex := lvToolbar.Items.Count-1;    // Add before the last empty item.
    Assert(anIndex >= 0, 'TToolBarConfig.btnAddDividerClick: Index < 0.');
  end;
  lvItem := lvToolbar.Items.Insert(anIndex);
  //lvItem.Selected := False;
  lvItem.Caption:= cIDEToolbarDivider;
  {$IF not DEFINED(LCLQt) and not DEFINED(LCLQt5)}
  lvItem.ImageIndex := divImageIndex;
  {$ENDIF}
  lvToolbar.Selected := nil;
  lvToolbar.ItemIndex := lvItem.Index;
  InsertMainListItem(lvItem, lvToolbar.Items[anIndex]);
  UpdateButtonsState;
end;

procedure TToolBarConfig.btnRemoveClick(Sender: TObject);
begin
  RemoveCommand;
end;

procedure TToolBarConfig.RemoveCommand;
Var
  mi: TIDEButtonCommand;
  n: TTreeNode;
  I: Integer;
  lvItem: TListItem;
begin
  I := lvToolbar.ItemIndex;
  if (ActiveControl=lvToolbar) and (i>-1) and (i<lvToolbar.Items.Count-1) then
  begin
    lvItem := lvToolbar.Items[I];
    mi := TIDEButtonCommand(lvItem.Data);
    RemoveMainListItem(lvItem);
    lvToolbar.Items.Delete(lvToolbar.ItemIndex);
    if I < lvToolbar.Items.Count then
      lvToolbar.Selected := lvToolbar.Items[I]; // Qt Workaround
    if assigned(mi) then begin
      n:= TV.Items.FindNodeWithData(mi);
      if n<>nil then
        n.Visible:= True;
    end;
    UpdateButtonsState;
  end;
end;

procedure TToolBarConfig.lvToolbarDrawItem(Sender: TCustomListView;
  AItem: TListItem; ARect: TRect; AState: TOwnerDrawState);
var
  ImgInd: integer;
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

    ImgInd := -1;
    if AItem.Caption = cIDEToolbarDivider then
      ImgInd := divImageIndex
    else if Assigned(AItem.Data) and (TIDEButtonCommand(AItem.Data).ImageIndex > -1) then
      ImgInd := TIDEButtonCommand(AItem.Data).ImageIndex
    else if AItem.Caption <> cTailItemCaption then
      ImgInd := defImageIndex;
    if ImgInd > -1 then
    begin
      Image.Clear;
      lvToolBar.SmallImages.GetBitmap(ImgInd, Image);
      Draw(ARect.Left + 2, ARect.Top + 2, Image);
    end;
    TextOut(ARect.Left + 21, ARect.Top + 2, AItem.Caption);
  end;
end;

procedure TToolBarConfig.lvToolbarSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  UpdateButtonsState;

  if lvToolbar.ItemIndex<lvToolbar.Items.Count-1 then
    lblSelect.Caption:=Format('%d / %d', [lvToolbar.ItemIndex+1, lvToolbar.Items.Count-1])
  else
    lblSelect.Caption:=Format('%d+ / %d', [lvToolbar.ItemIndex, lvToolbar.Items.Count-1])
end;

procedure TToolBarConfig.btnMoveDownClick(Sender: TObject);
var
  Index1,Index2: Integer;
begin
  if (ActiveControl=lvToolbar) and (lvToolbar.ItemIndex>-1)
    and (lvToolbar.ItemIndex<lvToolbar.Items.Count-2) then
  begin
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
  if (ActiveControl=lvToolbar) and (lvToolbar.ItemIndex>0)
    and (lvToolbar.ItemIndex<lvToolbar.Items.Count-1) then
  begin
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
  i, l: integer;
  xCategory: TIDEToolButtonCategory;
  xCaption: string;
  n: TTreeNode;
begin
  TV.Items.BeginUpdate;
  try
    TV.Items.Clear;
    for i := 0 to IDEToolButtonCategories.Count-1 do
    begin
      xCategory := IDEToolButtonCategories[i];
      xCaption := xCategory.Description;
      DeleteAmpersands(xCaption);
      n := TV.Items.AddChild(nil, Format('%s', [xCaption]));
      for l := 0 to xCategory.ButtonCount-1 do
        AddMenuItem(n, xCategory.Buttons[l]);
    end;
  finally
    TV.Items.EndUpdate;
  end;
end;

procedure TToolBarConfig.AddMenuItem(ParentNode: TTreeNode;
  Item: TIDEButtonCommand);
var
  n: TTreeNode;
begin
  if Item.Caption <> '-' then begin // workaround for HTML Editor dividers
    n := TV.Items.AddChild(ParentNode, Format('%s', [Item.GetCaptionWithShortCut]));
    n.ImageIndex := Item.ImageIndex;
    n.SelectedIndex := Item.ImageIndex;
    n.Data := Item;
  end;
end;

function TToolBarConfig.RootNodeCaption(Item: TIDEButtonCommand): string;
var
  aCaption: string;
begin
  aCaption:= Item.Caption;
  case aCaption of
    'IDEMainMenu':            Result := lisCoolbarIDEMainMenu;    // mnuMain
    'SourceTab':              Result := lisCoolbarSourceTab;      // SourceTabMenuRootName
    'SourceEditor':           Result := lisCoolbarSourceEditor;   // SourceEditorMenuRootName
    'Messages':               Result := lisCoolbarMessages;       // MessagesMenuRootName
    'Code Explorer':          Result := lisCoolbarCodeExplorer;   // CodeExplorerMenuRootName
    'CodeTemplates':          Result := lisCoolbarCodeTemplates;  // CodeTemplatesMenuRootName
    'Designer':               Result := lisCoolbarDesigner;       // DesignerMenuRootName
    'PackageEditor':          Result := lisCoolbarPackageEditor;  // PackageEditorMenuRootName
    'PackageEditorFiles':     Result := lisCoolbarPackageEditorFiles // PackageEditorMenuFilesRootName
    else                      Result := aCaption;
  end;
end;

procedure TToolBarConfig.AddListItem(Item: TIDEButtonCommand);
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
    MainList.AddObject(cIDEToolbarDivider,aListItem);
  end;
end;

procedure TToolBarConfig.AddToolBarItem(Item: TIDEButtonCommand);
Var
  n: TTreeNode;
  lvItem: TListItem;
begin
  if Assigned(Item) then begin
    lvItem := lvToolbar.Items.Add;
    lvItem.Caption:= Item.GetCaptionWithShortCut;
    lvItem.Data:= Item;
    {$IF not DEFINED(LCLQt) and not DEFINED(LCLQt5)}
    if Item.ImageIndex > -1 then
      lvItem.ImageIndex:= Item.ImageIndex
    else
      lvItem.ImageIndex:= defImageIndex;
    {$ENDIF}
   // lvItem.SubItems.Add(IntToStr(PMask));
    n:= TV.Items.FindNodeWithData(Item);
    if n<>nil then
      n.Visible:= False;
  end;
end;

procedure TToolBarConfig.AddDivider;
var
  lvItem: TListItem;
begin
  lvItem := lvToolbar.Items.Add;
  lvItem.Caption:= cIDEToolbarDivider;
  {$IF not DEFINED(LCLQt) and not DEFINED(LCLQt5)}
  lvItem.ImageIndex:= divImageIndex;
  {$ENDIF}
  //  lvItem.SubItems.Add(IntToStr(PMask));
end;

procedure TToolBarConfig.AddTailItem;
var
  lvItem: TListItem;
begin
  { Tail item is an extra item, added at the end of list for
    make possible to select place for inserting new command
    "after last command".
    In MainList it presented as extra divider at the end of list.
    In lvToolbar it presented as extra divider at the end of list
    with empty caption (only spaces).
    TToolBarConfig.SaveSettings excludes this item from saving.
    In lvToolbar this item may only be selected, any actions
    with it are prohibited. }

  lvItem := lvToolbar.Items.Add;
  lvItem.Caption := cTailItemCaption;
end;

procedure TToolBarConfig.FillToolBar;
var
  I: Integer;
  aListItem: TLvItem;
  aCaption: string;
  mi: TIDEButtonCommand;
begin
  for I:= 0 to MainList.Count -1 do
  begin
    aListItem := TLvItem(MainList.Objects[I]);
    mi := aListItem.Item;
    aCaption := MainList.Strings[I];
    if aCaption = cIDEToolbarDivider then
      if i<(MainList.Count-1) then
        AddDivider
      else
        AddTailItem  // add tail item instead extra divider at the end of list
    else
      AddToolBarItem(mi);
    aListItem.LvIndex:= lvToolbar.Items.Count - 1;
  end;
end;

procedure TToolBarConfig.LoadSettings(SL: TStringList);
var
  I: Integer;
  Value: string;
  MI: TIDEButtonCommand;
begin
  for I := 0 to SL.Count - 1 do
  begin
    Value := SL[I];
    if Value = '' then Continue;
    if Value = cIDEToolbarDivider then
      MI := nil
    else
    begin
      MI := IDEToolButtonCategories.FindItemByMenuPathOrName(Value);
      SL[I] := Value;
    end;
    AddListItem(MI);
  end;

  AddListItem(nil);  // add extra divider at the end of list. This extra divider
                     // will presented in lvToolbar as "tail item" (see comment in AddTailItem)
  FillToolBar;
  lvToolbar.ItemIndex:=lvToolbar.Items.Count-1;
end;

procedure TToolBarConfig.SaveSettings(SL: TStringList);
var
  lvItem: TLvItem;
  I: Integer;
begin
  SL.Clear;
  for i := 0 to MainList.Count - 2 do  // excluding tail item
  begin
    lvItem := TLvItem(MainList.Objects[I]);
    if MainList[I] = cIDEToolbarDivider then
      SL.Add(cIDEToolbarDivider)
    else
      SL.Add(lvItem.Item.Name);
  end;
end;

{ TIDEToolBarOptionsBase }

constructor TIDEToolBarOptionsBase.Create;
begin
  FButtonNames := TStringList.Create;
end;

destructor TIDEToolBarOptionsBase.Destroy;
begin
  FButtonNames.Free;
  inherited Destroy;
end;

procedure TIDEToolBarOptionsBase.Clear;
begin
  FButtonNames.Clear;
end;

function TIDEToolBarOptionsBase.Equals(Opts: TIDEToolBarOptionsBase): boolean;
begin
  Result := FButtonNames.Equals(Opts.FButtonNames);
end;

procedure TIDEToolBarOptionsBase.Assign(Source: TIDEToolBarOptionsBase);
begin
  FButtonNames.Assign(Source.FButtonNames);
end;

procedure TIDEToolBarOptionsBase.LoadButtonNames(XMLConfig: TXMLConfig; SubPath: String);
var
  ButtonCount: Integer;
  ButtonName: string;
  I, FileVersion: Integer;
begin
  FileVersion := XMLConfig.GetValue(SubPath + 'Version', 0);
  ButtonCount := XMLConfig.GetValue(SubPath + 'Count', 0);
  if (FileVersion < 1) and (ButtonCount = 0) then  // Old format
    ButtonCount := XMLConfig.GetValue(SubPath + 'ButtonCount/Value', 0);
  for I := 1 to ButtonCount do
  begin
    ButtonName := XMLConfig.GetValue(SubPath + 'Button' + IntToStr(I) + '/Name', '');
    if (FileVersion < 1) and (ButtonName = '') then  // Old format
      ButtonName := XMLConfig.GetValue(SubPath + 'Buttons/Name' + IntToStr(I) + '/Value', '');
    if ButtonName <> '' then
      ButtonNames.Add(ButtonName);
  end;
end;

procedure TIDEToolBarOptionsBase.SaveButtonNames(XMLConfig: TXMLConfig; SubPath: String);
var
  I: Integer;
begin
  XMLConfig.SetValue(SubPath + 'Version', IDEToolBarConfigVersion);
  XMLConfig.SetDeleteValue(SubPath + 'Count', ButtonNames.Count, 0);
  for I := 0 to ButtonNames.Count-1 do
    XMLConfig.SetDeleteValue(SubPath + 'Button' + IntToStr(I+1) + '/Name', ButtonNames[I], '');
end;

{ TIDEToolbarBase }
{                           For future needs ...
constructor TIDEToolbarBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TIDEToolbarBase.Destroy;
begin
  inherited Destroy;
end;
}
procedure TIDEToolbarBase.AddButton(ACommand: TIDEButtonCommand);
var
  B: TIDEToolButton;
begin
  B := ACommand.ToolButtonClass.Create(FToolBar);
  B.Hint := ACommand.GetHintOrCaptionWithShortCut;
  B.Enabled := ACommand.Enabled;
  // If we have a image, use it. Otherwise supply a default.
  if ACommand.ImageIndex <> -1 then
    B.ImageIndex := ACommand.ImageIndex
  else
    B.ImageIndex := IDEImages.LoadImage('execute');
  B.Style := tbsButton;
  B.Item := ACommand;
  PositionAtEnd(FToolBar, B);
  ACommand.ToolButtonAdded(B);
end;

procedure TIDEToolbarBase.AddDivider;
var
  B: TToolButton;
begin
  B := TToolButton.Create(FToolBar);
  B.Style := tbsDivider;
  PositionAtEnd(FToolBar, B);
end;

procedure TIDEToolbarBase.CopyFromOptions(Options: TIDEToolBarOptionsBase);
var
  mi: TIDEButtonCommand;
  ButtonName: string;
  i: Integer;
begin
  FToolBar.BeginUpdate;
  try
    for i := 0 to Options.ButtonNames.Count-1 do
    begin
      ButtonName := Options.ButtonNames[i];
      if ButtonName = cIDEToolbarDivider then
        AddDivider
      else
      begin
        mi := IDEToolButtonCategories.FindItemByMenuPathOrName(ButtonName);
        Options.ButtonNames[i] := ButtonName;
        if Assigned(mi) then
          AddButton(mi);
      end;
    end;
    PostCopyOptions;
  finally
    FToolBar.EndUpdate;
  end;
end;

procedure TIDEToolbarBase.PositionAtEnd(AToolBar: TToolBar; AButton: TToolButton);
// position the button next to the last button
var
  SiblingButton: TToolButton;
begin
  if AToolBar.ButtonCount > 0 then
  begin
    SiblingButton := AToolBar.Buttons[AToolBar.ButtonCount-1];
    AButton.SetBounds(SiblingButton.Left + SiblingButton.Width,
      SiblingButton.Top, AButton.Width, AButton.Height);
  end;
  AButton.Parent := AToolBar;
end;

procedure TIDEToolbarBase.PostCopyOptions;
begin
  // Can be overridden.
end;

end.

