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
  ToolBarIntf, IDEImagesIntf, IDEWindowIntf,
  // IDE
  LazarusIDEStrConsts;

const
  IDEToolBarConfigVersion = 1;  // File version in configuration.

type
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
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
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
    procedure lvToolbarSelectItem(Sender: TObject; {%H-}Item: TListItem;
      {%H-}Selected: Boolean);
    procedure TVSelectionChanged(Sender: TObject);
  private
    Image: TBitMap;
    defImageIndex: integer;
    procedure AddCommand;
    procedure AddDivider;
    procedure AddTailItem;
    procedure AddToolBarItem(CmdItem: TIDEButtonCommand);
    procedure InsertItem(Item: TListItem);
    procedure MoveUpDown(aOffset: integer);
    function NewLvItem(aCaption: string): TListItem;
    procedure RemoveCommand;
    procedure SetupCaptions;
    procedure LoadCategories;
    procedure SortCategories(ACtgList: TStrings);
    procedure AddMenuItem(ParentNode: TTreeNode; CmdItem: TIDEButtonCommand);
    function RootNodeCaption(CmdItem: TIDEButtonCommand): string;
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
  pnlButtons.Color := clBtnFace;
  // load button images
  IDEImages.AssignImage(btnAdd, 'arrow__darkgreen_right');
  IDEImages.AssignImage(btnRemove, 'arrow__darkred_left');
  IDEImages.AssignImage(btnMoveUp, 'arrow__darkgreen_up');
  IDEImages.AssignImage(btnMoveDown, 'arrow__darkgreen_down');
  //IDEImages.AssignImage(btnAddDivider, 'menu_divider16');  // uncomment if 'menu_divider16' exists (currently not)

  btnAddDivider.Caption := '---';
  btnAdd.Hint       := lisCoolBarAddSelected;
  btnRemove.Hint    := lisCoolBarRemoveSelected;
  btnMoveUp.Hint    := lisCoolBarMoveSelectedUp;
  btnMoveDown.Hint  := lisCoolBarMoveSelectedDown;
  btnAddDivider.Hint:= lisCoolBarAddDivider;

  TV.Images := IDEImages.Images_16;
  lvToolbar.SmallImages := IDEImages.Images_16;
  // default image to be used when none is available
  defImageIndex := IDEImages.LoadImage('execute');

  Image := TBitmap.Create;
  SetupCaptions;
  LoadCategories;
  IDEDialogLayoutList.ApplyLayout(Self);
end;

procedure TToolBarConfig.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TToolBarConfig.FormDestroy(Sender: TObject);
begin
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
  I: Integer;
begin
  I := lvToolbar.ItemIndex;
  btnAdd.Enabled := Assigned(TV.Selected) and Assigned(TV.Selected.Data);
  btnRemove.Enabled := (I>-1) and (I<lvToolbar.Items.Count-1);
  btnMoveUp.Enabled := (I>0) and (I<lvToolbar.Items.Count-1);
  btnMoveDown.Enabled := (I>-1) and (I<lvToolbar.Items.Count-2);
  btnAddDivider.Enabled := True;
end;

procedure TToolBarConfig.TVSelectionChanged(Sender: TObject);
begin
  UpdateButtonsState;
end;

procedure TToolBarConfig.InsertItem(Item: TListItem);
begin
  lvToolbar.ItemIndex := -1;
  lvToolbar.Selected := nil;
  if Item.Index < lvToolbar.Items.Count then
    lvToolbar.ItemIndex := Item.Index+1
  else
    lvToolbar.ItemIndex := Item.Index;
end;

procedure TToolBarConfig.btnAddClick(Sender: TObject);
begin
  AddCommand;
end;

function TToolBarConfig.NewLvItem(aCaption: string): TListItem;
var
  I: Integer;
begin
  I := lvToolbar.ItemIndex;
  if I = -1 then
    I := lvToolbar.Items.Count-1;    // Add before the last empty item.
  Result := lvToolbar.Items.Insert(I);
  Result.Caption := aCaption;
end;

procedure TToolBarConfig.AddCommand;
var
  Node: TTreeNode;
  CmdCaption: string;
  lvItem: TListItem;
begin
  Node := TV.Selected;
  if (Node = Nil) or (Node.Data = Nil) then
    Exit;
  CmdCaption := TIDEButtonCommand(Node.Data).Caption;
  DeleteAmpersands(CmdCaption);
  lvItem := NewLvItem(CmdCaption);
  lvItem.Data := Node.Data;
  if Node.ImageIndex > -1 then
    lvItem.ImageIndex := Node.ImageIndex
  else
    lvItem.ImageIndex := defImageIndex;
  InsertItem(lvItem);                  // Add the newly created item to ListView.
  // Update selection in TreeView.
  Node := TV.Selected.GetNext;
  TV.Selected.Visible := False;
  if Node <> nil then
    TV.Selected := Node;
  UpdateButtonsState;
end;

procedure TToolBarConfig.RemoveCommand;
Var
  Cmd: TIDEButtonCommand;
  Node: TTreeNode;
  I: Integer;
begin
  I := lvToolbar.ItemIndex;
  if (I<0) or (I>=lvToolbar.Items.Count-1) then Exit;
  Cmd := TIDEButtonCommand(lvToolbar.Items[I].Data);
  lvToolbar.Items.Delete(I);
  {$IF DEFINED(LCLQt) or DEFINED(LCLQt5)}
  lvToolbar.ItemIndex := -1;     // Try to make LCLQt behave.
  lvToolbar.ItemIndex := I;
  {$ENDIF}
  lvToolbar.Selected := lvToolbar.Items[I];
  // Show the command as available again in TreeView.
  if Assigned(Cmd) then
  begin
    Node:= TV.Items.FindNodeWithData(Cmd);
    if Node<>nil then
      Node.Visible:= True;
  end;
  UpdateButtonsState;
end;

procedure TToolBarConfig.btnAddDividerClick(Sender: TObject);
var
  lvItem: TListItem;
begin
  lvItem := NewLvItem(cIDEToolbarDivider);
  lvItem.ImageIndex := -1;
  InsertItem(lvItem);
  UpdateButtonsState;
end;

procedure TToolBarConfig.btnRemoveClick(Sender: TObject);
begin
  RemoveCommand;
end;

procedure TToolBarConfig.lvToolbarSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  UpdateButtonsState;
end;

procedure TToolBarConfig.MoveUpDown(aOffset: integer);
var
  Index1,Index2: Integer;
begin
  Index1 := lvToolbar.ItemIndex;
  Index2 := Index1 + aOffset;
  lvToolbar.Items.Exchange(Index1,Index2);
  lvToolbar.Items[Index1].Selected := False;
  lvToolbar.Items[Index2].Selected := False;
  lvToolbar.ItemIndex:= -1;
  lvToolbar.Selected := Nil;
  lvToolbar.ItemIndex:= Index2;
  lvToolbar.Selected := lvToolbar.Items[Index2];
end;

procedure TToolBarConfig.btnMoveDownClick(Sender: TObject);
begin
  if (lvToolbar.ItemIndex<0) or (lvToolbar.ItemIndex>=lvToolbar.Items.Count-2) then
    Exit;
  MoveUpDown(1);
end;

procedure TToolBarConfig.btnMoveUpClick(Sender: TObject);
begin
  if (lvToolbar.ItemIndex<1) or (lvToolbar.ItemIndex>=lvToolbar.Items.Count-1) then
    Exit;
  MoveUpDown(-1);
end;

procedure TToolBarConfig.SetupCaptions;
begin
  Caption             := lisToolbarConfiguration;
  lblMenuTree.Caption := lisCoolbarAvailableCommands;
  lblToolbar.Caption  := lisCoolbarToolbarCommands;
end;

procedure TToolBarConfig.LoadCategories;
var
  i, l: integer;
  xCategory: TIDEToolButtonCategory;
  xCaption: string;
  Node: TTreeNode;
  SortedCtgList: TStringList;
begin
  TV.Items.BeginUpdate;
  SortedCtgList := TStringList.Create;
  try
    SortedCtgList.OwnsObjects := False;
    for i := 0 to IDEToolButtonCategories.Count-1 do
    begin
      xCategory := IDEToolButtonCategories[i];
      SortedCtgList.AddObject(xCategory.Description, xCategory);
    end;
    SortCategories(SortedCtgList);

    TV.Items.Clear;
    for i := 0 to SortedCtgList.Count-1 do
    begin
      xCaption := SortedCtgList[i];
      xCategory := SortedCtgList.Objects[i] as TIDEToolButtonCategory;
      DeleteAmpersands(xCaption);
      Node := TV.Items.AddChild(nil, Format('%s', [xCaption]));
      for l := 0 to xCategory.ButtonCount-1 do
        AddMenuItem(Node, xCategory.Buttons[l]);
    end;
  finally
    SortedCtgList.Free;
    TV.Items.EndUpdate;
  end;
end;

procedure TToolBarConfig.SortCategories(ACtgList: TStrings);
var
  NewIndex: Integer;

  procedure MoveItem(s: String);
  var
    OldIndex: Integer;
  begin
    OldIndex := ACtgList.IndexOf(s);
    if (OldIndex<0) or (NewIndex>=ACtgList.Count) then Exit;
    ACtgList.Move(OldIndex, NewIndex);
    Inc(NewIndex);
  end;

begin
  NewIndex := 0;
  MoveItem(srkmCatFileMenu);
  MoveItem(srkmCatCmdCmd);
  MoveItem(srkmCatSelection);
  MoveItem(srkmCatMacroRecording);
  MoveItem(srkmCatSearchReplace);
  MoveItem(srkmCatMarker);
  MoveItem(srkmCatViewMenu);
  MoveItem(srkmCatCodeTools);
  MoveItem(srkmCatEditing);
  MoveItem(srkmCatProjectMenu);
  MoveItem(srkmCatRunMenu);
  MoveItem(srkmCatPackageMenu);
  MoveItem(srkmCatToolMenu);
  MoveItem(srkmCatSrcNoteBook);
  MoveItem(srkmCarHelpMenu);
end;

procedure TToolBarConfig.AddMenuItem(ParentNode: TTreeNode; CmdItem: TIDEButtonCommand);
var
  Node: TTreeNode;
begin
  if CmdItem.Caption = '-' then Exit;   // divider
  Node := TV.Items.AddChild(ParentNode, Format('%s', [CmdItem.GetCaptionWithShortCut]));
  Node.ImageIndex := CmdItem.ImageIndex;
  Node.SelectedIndex := CmdItem.ImageIndex;
  Node.Data := CmdItem;
end;

function TToolBarConfig.RootNodeCaption(CmdItem: TIDEButtonCommand): string;
begin
  case CmdItem.Caption of
    'IDEMainMenu':        Result := lisCoolbarIDEMainMenu;    // mnuMain
    'SourceTab':          Result := lisCoolbarSourceTab;      // SourceTabMenuRootName
    'SourceEditor':       Result := lisCoolbarSourceEditor;   // SourceEditorMenuRootName
    'Messages':           Result := lisCoolbarMessages;       // MessagesMenuRootName
    'Code Explorer':      Result := lisCoolbarCodeExplorer;   // CodeExplorerMenuRootName
    'CodeTemplates':      Result := lisCoolbarCodeTemplates;  // CodeTemplatesMenuRootName
    'Designer':           Result := lisCoolbarDesigner;       // DesignerMenuRootName
    'PackageEditor':      Result := lisCoolbarPackageEditor;  // PackageEditorMenuRootName
    'PackageEditorFiles': Result := lisCoolbarPackageEditorFiles // PackageEditorMenuFilesRootName
    else                  Result := CmdItem.Caption;
  end;
end;

procedure TToolBarConfig.AddToolBarItem(CmdItem: TIDEButtonCommand);
Var
  Node: TTreeNode;
  lvItem: TListItem;
begin
  if CmdItem=Nil then Exit;
  lvItem := lvToolbar.Items.Add;
  lvItem.Caption := CmdItem.GetCaptionWithShortCut;
  lvItem.Data := CmdItem;
  if CmdItem.ImageIndex > -1 then
    lvItem.ImageIndex := CmdItem.ImageIndex
  else
    lvItem.ImageIndex := defImageIndex;
  Node := TV.Items.FindNodeWithData(CmdItem);
  if Node<>nil then
    Node.Visible := False;
end;

procedure TToolBarConfig.AddDivider;
var
  lvItem: TListItem;
begin
  lvItem := lvToolbar.Items.Add;
  lvItem.Caption := cIDEToolbarDivider;
  lvItem.ImageIndex := -1;
end;

procedure TToolBarConfig.AddTailItem;
// An extra item at the end of list so that new command can be inserted there.
// TToolBarConfig.SaveSettings excludes this item from saving.
// In lvToolbar this item may only be selected, any actions with it are prohibited.
var
  lvItem: TListItem;
begin
  lvItem := lvToolbar.Items.Add;
  lvItem.Caption := cTailItemCaption;
end;

procedure TToolBarConfig.LoadSettings(SL: TStringList);
var
  I: Integer;
  Value: string;
  Cmd: TIDEButtonCommand;
begin
  for I := 0 to SL.Count - 1 do
  begin
    Value := SL[I];
    if Value = '' then Continue;
    if Value = cIDEToolbarDivider then
      AddDivider                // Add divider.
    else
    begin
      Cmd := IDEToolButtonCategories.FindItemByMenuPathOrName(Value);
      AddToolBarItem(Cmd);      // Add command.
      if Value <> SL[I] then
        DebugLn(['TToolBarConfig.LoadSettings: SL[I]=', SL[I], ', Value=', Value]);
      SL[I] := Value;
    end;
  end;
  AddTailItem;                  // Add tail item at the end.
  lvToolbar.ItemIndex:=lvToolbar.Items.Count-1;
end;

procedure TToolBarConfig.SaveSettings(SL: TStringList);
var
  lvItem: TListItem;
  Cmd: TIDEButtonCommand;
  I: Integer;
begin
  SL.Clear;
  for I := 0 to lvToolbar.Items.Count - 2 do  // excluding tail item
  begin
    lvItem := lvToolbar.Items[I];
    Cmd := TIDEButtonCommand(lvItem.Data);
    if lvItem.Caption = cIDEToolbarDivider then
      SL.Add(cIDEToolbarDivider)
    else begin
      Assert(Assigned(Cmd), 'TToolBarConfig.SaveSettings: Cmd = Nil.');
      SL.Add(Cmd.Name);
    end;
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
  Cmd: TIDEButtonCommand;
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
        Cmd := IDEToolButtonCategories.FindItemByMenuPathOrName(ButtonName);
        Options.ButtonNames[i] := ButtonName;
        if Assigned(Cmd) then
          AddButton(Cmd);
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

