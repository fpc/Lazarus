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
  // LCL and LazControls
  LCLProc, Forms, Graphics, ExtCtrls, Buttons, StdCtrls,
  Controls, ComCtrls, Menus, ButtonPanel, TreeFilterEdit, LclIntf,
  // LazUtils
  Laz2_XMLCfg,
  // IdeIntf
  IDECommands, ToolBarIntf, IDEImagesIntf,
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
  OpenUrl('http://wiki.freepascal.org/IDE_Window:_Toolbar_Config');
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
  aMainListItem.Item := TIDEButtonCommand(Item.Data);
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
    ACaption:= TIDEButtonCommand(n.Data).Caption;
    DeleteAmpersands(ACaption);
    if anIndex > -1 then
      lvItem            := lvToolbar.Items.Insert(lvToolbar.ItemIndex)
    else
      lvItem            := lvToolbar.Items.Add;
    lvItem.Caption      := ACaption;
    lvItem.Data         := n.Data;
    {$IF not DEFINED(LCLQt) and not DEFINED(LCLQt5)}
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
  mi: TIDEButtonCommand;
  n: TTreeNode;
  I: Integer;
  lvItem: TListItem;
begin
  I := lvToolbar.ItemIndex;
  if I > -1 then begin
    lvItem := lvToolbar.Items[I];
    mi := TIDEButtonCommand(lvItem.Data);
    RemoveMainListItem(lvItem);
    lvToolbar.Items.Delete(lvToolbar.ItemIndex);
    if I < lvToolbar.Items.Count then
      lvToolbar.Selected := lvToolbar.Items[I]; // Qt Workaround
    lbToolbarSelectionChange(lvToolbar);
    if assigned(mi) then begin
      n:= TV.Items.FindNodeWithData(mi);
      if n<>nil then
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

    if AItem.Caption = cIDEToolbarDivider then
      ImageIndex := divImageIndex
    else if Assigned(AItem.Data) and (TIDEButtonCommand(AItem.Data).ImageIndex > -1) then
      ImageIndex := TIDEButtonCommand(AItem.Data).ImageIndex
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
  lvItem.Caption:= cIDEToolbarDivider;
  {$IF not DEFINED(LCLQt) and not DEFINED(LCLQt5)}
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
    B.ImageIndex := IDEImages.LoadImage(16, 'execute');
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

