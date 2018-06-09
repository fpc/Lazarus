{
 *****************************************************************************
  This file is part of the EducationLaz package

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Frame to setup the IDE menus.
}
unit EduMenu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Laz_AVL_Tree,
  // LCL
  LCLProc, LResources, Forms, StdCtrls, ComCtrls, Controls, ExtCtrls,
  // LazUtils
  AvgLvlTree, LazConfigStorage,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf, MenuIntf, IDEImagesIntf,
  // Education
  EduOptions;

type

  { TEduMenuOptions }

  TEduMenuOptions = class(TEduOptionsNode)
  private
    fHidden: TStringToStringTree;
    function GetMenuHidden(MenuPath: string): boolean;
    procedure SetMenuHidden(MenuPath: string; const AValue: boolean);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ClearHMenuidden;
    function Load(Config: TConfigStorage): TModalResult; override;
    function Save(Config: TConfigStorage): TModalResult; override;
    function MenuItemToPath(Item: TIDEMenuItem): string;
    function FindItemWithPath(Path: string): TIDEMenuItem;
    function KeepItemVisible(Item: TIDEMenuItem): boolean;
    procedure Apply(Enable: boolean); override;
    property MenuHidden[MenuPath: string]: boolean read GetMenuHidden write SetMenuHidden;
  end;

  { TEduMenuFrame }

  TEduMenuFrame = class(TAbstractIDEOptionsEditor)
    EduMenuBtnPanel: TPanel;
    MenusGroupBox: TGroupBox;
    MenusTreeView: TTreeView;
    ShowAllButton: TButton;
    ShowAllChildsButton: TButton;
    procedure FrameClick(Sender: TObject);
    procedure MenusTreeViewMouseDown(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure ShowAllButtonClick(Sender: TObject);
    procedure ShowAllChildsButtonClick(Sender: TObject);
  private
    ShowImgID: LongInt;
    HalfHideImgID: integer;
    HideImgID: LongInt;
    procedure FillMenuTree;
    procedure SaveMenuTree;
    function TVNodeToIDEMenuPath(TVNode: TTreeNode): string;
    procedure UpdateTVNodeImage(TVNode: TTreeNode);
    function GetCleanCaption(Item: TIDEMenuItem): string;
    function TVNodeToName(TVNode: TTreeNode): string;
  public
    function GetTitle: String; override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
  end;

var
  EduMenuOptions: TEduMenuOptions = nil;

procedure Register;

implementation

procedure Register;
begin
  EduMenuOptions:=TEduMenuOptions.Create;
  EducationOptions.Root.Add(EduMenuOptions);
  EduOptionMenuID:=RegisterIDEOptionsEditor(EduOptionID,TEduMenuFrame,
                                            EduOptionMenuID)^.Index;
end;

{ TEduMenuFrame }

procedure TEduMenuFrame.FrameClick(Sender: TObject);
begin

end;

procedure TEduMenuFrame.MenusTreeViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: TTreeNode;
  Hit: THitTests;
begin
  if Button<>mbLeft then exit;
  Node:=MenusTreeView.GetNodeAt(X,Y);
  if (Node=nil) then exit;
  Hit:=MenusTreeView.GetHitTestInfoAt(X,Y);
  if [htOnIcon,htOnStateIcon]*Hit<>[] then begin
    if Node.StateIndex=HideImgID then
      Node.StateIndex:=ShowImgID
    else
      Node.StateIndex:=HideImgID;
    UpdateTVNodeImage(Node);
  end;
end;

procedure TEduMenuFrame.ShowAllButtonClick(Sender: TObject);
var
  TVNode: TTreeNode;
begin
  MenusTreeView.BeginUpdate;
  TVNode:=MenusTreeView.Items.GetFirstNode;
  while TVNode<>nil do begin
    TVNode.StateIndex:=ShowImgID;
    TVNode:=TVNode.GetNext;
  end;
  MenusTreeView.EndUpdate;
end;

procedure TEduMenuFrame.ShowAllChildsButtonClick(Sender: TObject);
var
  TVNode: TTreeNode;
  SelNode: TTreeNode;
begin
  MenusTreeView.BeginUpdate;
  SelNode:=MenusTreeView.Selected;
  if SelNode<>nil then begin
    TVNode:=SelNode.GetFirstChild;
    while (TVNode<>nil) and (TVNode.HasAsParent(SelNode)) do begin
      TVNode.StateIndex:=ShowImgID;
      TVNode:=TVNode.GetNext;
    end;
    UpdateTVNodeImage(SelNode);
  end;
  MenusTreeView.EndUpdate;
end;

procedure TEduMenuFrame.FillMenuTree;

  procedure Add(ParentTVNode: TTreeNode; Item: TIDEMenuItem; var ContainsHidden: boolean);
  var
    TVNode: TTreeNode;
    Section: TIDEMenuSection;
    i: Integer;
    HasHiddenChilds: Boolean;
  begin
    if Item.Name='' then exit;
    TVNode:=MenusTreeView.Items.AddChild(ParentTVNode,Item.Name+': "'+GetCleanCaption(Item)+'"');
    HasHiddenChilds:=false;
    if Item is TIDEMenuSection then begin
      Section:=TIDEMenuSection(Item);
      for i:=0 to Section.Count-1 do
        Add(TVNode,Section[i],HasHiddenChilds);
    end;
    if EduMenuOptions.MenuHidden[EduMenuOptions.MenuItemToPath(Item)] then
      TVNode.StateIndex:=HideImgID
    else if HasHiddenChilds then
      TVNode.StateIndex:=HalfHideImgID
    else
      TVNode.StateIndex:=ShowImgID;
    if TVNode.StateIndex<>ShowImgID then
      ContainsHidden:=true;
  end;

var
  i: Integer;
  Hidden: boolean;
begin
  MenusTreeView.BeginUpdate;
  MenusTreeView.Items.Clear;
  if MenusTreeView.StateImages=nil then
    MenusTreeView.StateImages:=IDEImages.Images_16;
  ShowImgID:=IDEImages.LoadImage('menu_run');
  HalfHideImgID:=IDEImages.LoadImage('menu_run_file');
  HideImgID:=IDEImages.LoadImage('menu_stop');
  Hidden:=false;
  for i:=0 to IDEMenuRoots.Count-1 do
    Add(nil,IDEMenuRoots[i],Hidden);
  MenusTreeView.EndUpdate;
end;

procedure TEduMenuFrame.SaveMenuTree;
var
  TVNode: TTreeNode;
  NewHide: Boolean;
  OldHide: boolean;
  Item: TIDEMenuItem;
  Path: String;
  OldHidden: TStringToStringTree;
begin
  OldHidden:=TStringToStringTree.Create(false);
  try
    OldHidden.Assign(EduMenuOptions.fHidden);
    EduMenuOptions.ClearHMenuidden;
    TVNode:=MenusTreeView.Items.GetFirstNode;
    while TVNode<>nil do begin
      NewHide:=TVNode.StateIndex=HideImgID;
      Path:=TVNodeToIDEMenuPath(TVNode);
      OldHide:=OldHidden.Contains(Path);
      EduMenuOptions.MenuHidden[Path]:=NewHide;
      if NewHide<>OldHide then begin
        Item:=EduMenuOptions.FindItemWithPath(Path);
        if (Item<>nil) and (not EduMenuOptions.KeepItemVisible(Item)) then begin
          Item.Visible:=not NewHide;
          //debugln(['TEduMenuFrame.SaveMenuTree changed visibility: ',Item.GetPath,' visible=',Item.Visible,' Path=',Path,' OldHide=',OldHide,' NewHide=',NewHide]);
        end;
      end;
      TVNode:=TVNode.GetNext;
    end;
  finally
    OldHidden.Free;
  end;
end;

function TEduMenuFrame.TVNodeToIDEMenuPath(TVNode: TTreeNode): string;
begin
  Result:='';
  while TVNode<>nil do begin
    if Result<>'' then
      Result:='/'+Result;
    Result:=TVNodeToName(TVNode)+Result;
    TVNode:=TVNode.Parent;
  end;
end;

procedure TEduMenuFrame.UpdateTVNodeImage(TVNode: TTreeNode);

  function ContainsHiddenNode(Node: TTreeNode): boolean;
  begin
    if (Node.StateIndex=HideImgID) and (Node<>TVNode) then
      exit(true);
    Node:=Node.GetFirstChild;
    while Node<>nil do begin
      if ContainsHiddenNode(Node) then exit(true);
      Node:=Node.GetNextSibling;
    end;
    Result:=false;
  end;

begin
  if TVNode=nil then exit;
  if TVNode.StateIndex=HideImgID then
    TVNode.StateIndex:=HideImgID
  else if ContainsHiddenNode(TVNode) then
    TVNode.StateIndex:=HalfHideImgID
  else
    TVNode.StateIndex:=ShowImgID;
  UpdateTVNodeImage(TVNode.Parent);
end;

function TEduMenuFrame.GetCleanCaption(Item: TIDEMenuItem): string;
var
  i: Integer;
begin
  Result:=Item.Caption;
  for i:=length(Result) downto 1 do
    if Result[i] in ['"','&'] then
      System.Delete(Result,i,1);
end;

function TEduMenuFrame.TVNodeToName(TVNode: TTreeNode): string;
var
  p: Integer;
begin
  Result:=TVNode.Text;
  p:=length(Result);
  if Result[p]='"' then begin
    dec(p);
    while (Result[p]<>'"') do dec(p);
    while (Result[p]<>':') do dec(p);
    Result:=copy(Result,1,p-1);
  end;
end;

function TEduMenuFrame.GetTitle: String;
begin
  Result:=ersEduMenuTitle
end;

procedure TEduMenuFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  if AOptions=EducationOptions then begin
    FillMenuTree;
  end;
end;

procedure TEduMenuFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  ShowAllButton.Caption:=ersShowAll;
  ShowAllChildsButton.Caption:=ersShowAllChilds;
  MenusGroupBox.Caption:=ersIDEMenuItems;
end;

class function TEduMenuFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result:=EducationIDEOptionsClass;
end;

procedure TEduMenuFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  if AOptions=EducationOptions then begin
    SaveMenuTree;
  end;
end;

{ TEduMenuOptions }

function TEduMenuOptions.GetMenuHidden(MenuPath: string): boolean;
begin
  Result:=fHidden[MenuPath]='1';
end;

procedure TEduMenuOptions.SetMenuHidden(MenuPath: string; const AValue: boolean);
begin
  if AValue then
    fHidden[MenuPath]:='1'
  else
    fHidden.Remove(MenuPath);
end;

constructor TEduMenuOptions.Create;
begin
  inherited Create;
  Name:='Menus';
  fHidden:=TStringToStringTree.Create(false);
end;

destructor TEduMenuOptions.Destroy;
begin
  FreeAndNil(fHidden);
  inherited Destroy;
end;

procedure TEduMenuOptions.ClearHMenuidden;
begin
  fHidden.Clear;
end;

function TEduMenuOptions.Load(Config: TConfigStorage): TModalResult;
var
  Cnt: LongInt;
  i: Integer;
  MenuPath: String;
begin
  fHidden.Clear;
  Cnt:=Config.GetValue('Hidden/Count',0);
  for i:=1 to Cnt do begin
    MenuPath:=Config.GetValue('Hidden/Item'+IntToStr(i),'');
    if MenuPath='' then continue;
    fHidden[MenuPath]:='1';
  end;
  Result:=inherited Load(Config);
end;

function TEduMenuOptions.Save(Config: TConfigStorage): TModalResult;
var
  Node: TAvlTreeNode;
  Item: PStringToStringItem;
  Cnt: Integer;
begin
  Cnt:=0;
  Node:=fHidden.Tree.FindLowest;
  while Node<>nil do begin
    inc(Cnt);
    Item:=PStringToStringItem(Node.Data);
    Config.SetDeleteValue('Hidden/Item'+IntToStr(Cnt),Item^.Name,'');
    Node:=fHidden.Tree.FindSuccessor(Node);
  end;
  Config.SetDeleteValue('Hidden/Count',Cnt,0);
  Result:=inherited Save(Config);
end;

function TEduMenuOptions.MenuItemToPath(Item: TIDEMenuItem): string;
begin
  Result:='';
  while Item<>nil do begin
    if Result<>'' then
      Result:='/'+Result;
    Result:=Item.Name+Result;
    Item:=Item.Section;
  end;
end;

function TEduMenuOptions.FindItemWithPath(Path: string): TIDEMenuItem;
begin
  Result:=IDEMenuRoots.FindByPath(Path,false);
end;

function TEduMenuOptions.KeepItemVisible(Item: TIDEMenuItem): boolean;
begin
  if (Item=itmSecondaryTools) or (Item.HasAsParent(itmSecondaryTools)) then exit(true);
  Result:=false;
end;

procedure TEduMenuOptions.Apply(Enable: boolean);

  procedure ApplyRecursive(Item: TIDEMenuItem);
  var
    Section: TIDEMenuSection;
    i: Integer;
  begin
    if (not KeepItemVisible(Item)) then begin
      if Enable then begin
        if MenuHidden[MenuItemToPath(Item)] then
          Item.Visible:=false;
        // Note: do not show items. Some items should be hidden independent of education.
      end else begin
        if MenuHidden[MenuItemToPath(Item)] then
          Item.Visible:=true;
      end;
    end;
    if Item is TIDEMenuSection then begin
      Section:=TIDEMenuSection(Item);
      for i:=0 to Section.Count-1 do
        ApplyRecursive(Section[i]);
    end;
  end;

var
  i: Integer;
begin
  inherited Apply(Enable);
  for i:=0 to IDEMenuRoots.Count-1 do
    ApplyRecursive(IDEMenuRoots[i]);
end;

{$R *.lfm}

end.
