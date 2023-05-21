{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Author: Marius
  Modified by Juha Manninen, Balazs Szekely

  Abstract:
    A dialog to quickly find components and to add the found component
    to the designed form.
}
unit ComponentList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  LCLType, Forms, Controls, Graphics, StdCtrls, ExtCtrls, ComCtrls, Menus, Buttons,
  Dialogs, ImgList,
  // LazUtils
  LazLoggerBase, LazUTF8, LazFileCache, AvgLvlTree,
  // LazControls
  TreeFilterEdit,
  // IdeIntf
  FormEditingIntf, IDEImagesIntf, PropEdits, MenuIntf, ComponentReg, LazIDEIntf,
  TextTools,
  // IDE
  LazarusIDEStrConsts, PackageDefs, IDEOptionDefs, EnvironmentOpts, Designer;

const
  ComponentListMenuRootName = 'ComponentList';
var
  // menu section CompListMenuSectionOpen
  CompListMenuOpenUnit: TIDEMenuCommand;
  CompListMenuOpenPackage: TIDEMenuCommand;

  // menu section CompListMenuSectionExpand
  CompListMenuExpand: TIDEMenuCommand;
  CompListMenuExpandAll: TIDEMenuCommand;

  // menu section CompListMenuSectionCollapse
  CompListMenuCollapse: TIDEMenuCommand;
  CompListMenuCollapseAll: TIDEMenuCommand;

type

  { TComponentListForm }

  TComponentListForm = class(TForm)
    chbKeepOpen: TCheckBox;
    ButtonPanel: TPanel;
    OKButton: TButton;
    LabelSearch: TLabel;
    PageControl: TPageControl;
    FilterPanel: TPanel;
    ListTree: TTreeView;
    PalletteTree: TTreeView;
    InheritanceTree: TTreeView;
    pnPaletteTree: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    CompListPopupMenu: TPopupMenu;
    TabSheetPaletteTree: TTabSheet;
    TabSheetInheritance: TTabSheet;
    TabSheetList: TTabSheet;
    tmDeselect: TTimer;
    TreeFilterEd: TTreeFilterEdit;
    SelectionToolButton: TSpeedButton;
    procedure chbKeepOpenChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListTreeSelectionChanged(Sender: TObject);
    procedure miCollapseAllClick(Sender: TObject);
    procedure miCollapseClick(Sender: TObject);
    procedure miExpandAllClick(Sender: TObject);
    procedure miExpandClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure ComponentsDblClick(Sender: TObject);    
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);    
    procedure CompListPopupMenuPopup(Sender: TObject);
    procedure tmDeselectTimer(Sender: TObject);
    procedure TreeFilterEdAfterFilter(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    function TreeFilterEdFilterItemEx(const ACaption: string;
      ItemData: Pointer; out Done: Boolean): Boolean;
    procedure TreeFilterEdKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TreeKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure SelectionToolButtonClick(Sender: TObject);
  private
    FOnOpenPackage: TNotifyEvent;
    FOnOpenUnit: TNotifyEvent;
    PrevChangeStamp: Integer;
    FInitialized: Boolean;
    FIgnoreSelection: Boolean;
    FPageControlChange: Boolean;
    FActiveTree: TTreeView;
    FAddCompNewLeft, FAddCompNewTop: Integer;
    FAddCompNewParent: TComponent;
    procedure ClearSelection;
    procedure ComponentWasAdded({%H-}ALookupRoot, {%H-}AComponent: TComponent;
                                {%H-}ARegisteredComponent: TRegisteredComponent);
    procedure miOpenPackage(Sender: TObject);
    procedure miOpenUnit(Sender: TObject);
    procedure SelectionWasChanged;
    procedure AddComponentInheritanceNodes(ClassToNodeTree: TPointerToPointerTree; Comp: TRegisteredComponent);
    procedure UpdateComponents;
    procedure UpdateButtonState;
    function IsDocked: Boolean;
    procedure AddSelectedComponent(ASaveSelection: boolean = false; AAddNeighboring: boolean = false);
  protected
    procedure UpdateShowing; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetSelectedComponent: TRegisteredComponent;
    function GetSelectedPkgComp: TPkgComponent;
    property OnOpenPackage: TNotifyEvent read FOnOpenPackage write FOnOpenPackage;
    property OnOpenUnit: TNotifyEvent read FOnOpenUnit write FOnOpenUnit;
  end;
  
var
  ComponentListForm: TComponentListForm;

procedure RegisterStandardComponentListMenuItems;

implementation

{$R *.lfm}

procedure RegisterStandardComponentListMenuItems;
var
  AParent: TIDEMenuSection;
begin
  ComponentListMenuRoot := RegisterIDEMenuRoot(ComponentListMenuRootName);

  // register the section for open
  CompListMenuSectionOpen:=RegisterIDEMenuSection(ComponentListMenuRoot,'Open');
  AParent:=CompListMenuSectionOpen;
  CompListMenuOpenUnit:=RegisterIDEMenuCommand(AParent,'Open Unit','Open Unit');// resourcestring is set later
  CompListMenuOpenPackage:=RegisterIDEMenuCommand(AParent,'Open Package','Open Package'); // resourcestring is set later

  // register the section for expand
  CompListMenuSectionExpand:=RegisterIDEMenuSection(ComponentListMenuRoot,'Expand');
  AParent:=CompListMenuSectionExpand;
  CompListMenuExpand:=RegisterIDEMenuCommand(AParent, 'Expand', lisExpand);
  CompListMenuExpandAll:=RegisterIDEMenuCommand(AParent, 'Expand All', lisExpandAll2);

  // register the section for collapse
  CompListMenuSectionCollapse:=RegisterIDEMenuSection(ComponentListMenuRoot,'Collapse');
  AParent:=CompListMenuSectionCollapse;
  CompListMenuCollapse:=RegisterIDEMenuCommand(AParent, 'Collapse', lisCollapse);
  CompListMenuCollapseAll:=RegisterIDEMenuCommand(AParent, 'Collapse All',lisCollapseAll2);
end;

function GetSelectedDesignComponent: TComponent;
var
  ASelections: TPersistentSelectionList;
begin
  Result := nil;
  ASelections := TPersistentSelectionList.Create;
  try
    GlobalDesignHook.GetSelection(ASelections);
    if (ASelections.Count > 0) and (ASelections[0] is TComponent) then
      Result := TComponent(ASelections[0])
    else if GlobalDesignHook.LookupRoot is TComponent then
      Result := TComponent(GlobalDesignHook.LookupRoot);
  finally
    ASelections.Free;
  end;
end;

function GetSelectedTreeComp(aTree: TTreeView): TRegisteredComponent;
begin
  if Assigned(aTree.Selected) then
    Result := TRegisteredComponent(aTree.Selected.Data)
  else
    Result := nil;
end;

function GetRegCompClassname(RegComp: TRegisteredComponent): string;
begin
  Result:=RegComp.ComponentClass.ClassName;
  if RegComp.HasAmbiguousClassName then
    Result:=Result+'('+RegComp.ComponentClass.UnitName+')';
end;

procedure SelectTreeComp(aTree: TTreeView);
var
  Node: TTreeNode;
begin
  with IDEComponentPalette do
    if Assigned(Selected) then
      Node := aTree.Items.FindNodeWithText(GetRegCompClassname(Selected))
    else
      Node := Nil;
  aTree.Selected := Node;
  if aTree.Selected <> nil then
    aTree.Selected.MakeVisible;
end;

{ TComponentListForm }

constructor TComponentListForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Name:=NonModalIDEWindowNames[nmiwComponentList];
  FActiveTree := ListTree;

  IDEImages.AssignImage(SelectionToolButton, 'tmouse');
  with SelectionToolButton do begin
    ShowHint := EnvironmentOptions.ShowHintsForComponentPalette;
    Width := ComponentPaletteBtnWidth;
    BorderSpacing.Around := (FilterPanel.Height - ComponentPaletteImageHeight) div 2;
  end;

  //Translations
  LabelSearch.Caption := lisMenuFind;
  Caption := lisCmpLstComponents;
  TabSheetList.Caption := lisCmpLstList;
  TabSheetPaletteTree.Caption := lisCmpLstPalette;
  TabSheetInheritance.Caption := lisCmpLstInheritance;
  OKButton.Caption := lisUse;
  chbKeepOpen.Caption := lisKeepOpen;
  SelectionToolButton.Hint := lisSelectionTool;

  ListTree.Images := TPkgComponent.Images;
  PalletteTree.Images := TPkgComponent.Images;
  InheritanceTree.Images := TPkgComponent.Images;
  if Assigned(IDEComponentPalette) then
  begin
    UpdateComponents;
    TreeFilterEd.InvalidateFilter;
    IDEComponentPalette.AddHandlerComponentAdded(@ComponentWasAdded);
    IDEComponentPalette.AddHandlerSelectionChanged(@SelectionWasChanged);
    IDEComponentPalette.AddHandlerUpdated(@UpdateComponents);
  end;
  chbKeepOpen.Checked := EnvironmentOptions.ComponentListKeepOpen;
  PageControl.PageIndex := EnvironmentOptions.ComponentListPageIndex;
  PageControlChange(Nil);
end;

destructor TComponentListForm.Destroy;
begin
  if Assigned(IDEComponentPalette) then begin
    IDEComponentPalette.RemoveHandlerUpdated(@UpdateComponents);
    IDEComponentPalette.RemoveHandlerSelectionChanged(@SelectionWasChanged);
    IDEComponentPalette.RemoveHandlerComponentAdded(@ComponentWasAdded);
  end;
  ComponentListForm := nil;
  inherited Destroy;
end;

procedure TComponentListForm.AddSelectedComponent(ASaveSelection: boolean; AAddNeighboring: boolean);
var
  AComponent: TRegisteredComponent;
  NewParent: TComponent;
  CurDesigner: TDesigner;
  lOldEvent: TOnComponentAdded;
begin
  AComponent := GetSelectedComponent;
  NewParent := GetSelectedDesignComponent;

  // get parent component of neighboring component
  if AAddNeighboring and assigned(NewParent) then
  begin
    if (not (NewParent is TCustomForm)) and
       (not (NewParent is TCustomFrame)) then
    begin
      if NewParent.HasParent then
        NewParent := NewParent.GetParentComponent;
    end;
  end;
  if NewParent = nil then
    Exit;

  // get designer
  CurDesigner := TDesigner(FindRootDesigner(NewParent));
  if CurDesigner = nil then
    Exit;

  // check parent
  CurDesigner.AddComponentCheckParent(NewParent, NewParent, nil, AComponent.ComponentClass);
  if NewParent = nil then
    Exit;

  // calculate offset
  if FAddCompNewParent <> NewParent then
  begin
    FAddCompNewLeft := 0;
    FAddCompNewTop := 0;
    FAddCompNewParent := NewParent;
  end;
  Inc(FAddCompNewLeft, EnvironmentOptions.GridSizeX);
  Inc(FAddCompNewTop, EnvironmentOptions.GridSizeY);

  // add component
  if ASaveSelection then
  begin
    lOldEvent := CurDesigner.OnComponentAdded; // save event
    CurDesigner.OnComponentAdded := nil;       // clear event
    CurDesigner.AddComponent(AComponent, AComponent.ComponentClass, NewParent, FAddCompNewLeft, FAddCompNewTop, 0, 0);
    CurDesigner.OnComponentAdded := lOldEvent; // restore event
  end else begin
    CurDesigner.AddComponent(AComponent, AComponent.ComponentClass, NewParent, FAddCompNewLeft, FAddCompNewTop, 0, 0);
  end;

end;

procedure TComponentListForm.chbKeepOpenChange(Sender: TObject);
begin
  EnvironmentOptions.ComponentListKeepOpen := chbKeepOpen.Checked;
end;

procedure TComponentListForm.FormShow(Sender: TObject);
begin
  //DebugLn(['*** TComponentListForm.FormShow, Parent=', Parent, ', Parent.Parent=', ParentParent]);
  ButtonPanel.Visible := not IsDocked;
  if ButtonPanel.Visible then
  begin                              // ComponentList is undocked
    PageControl.AnchorSideBottom.Side := asrTop;
    UpdateButtonState;
    if TreeFilterEd.CanSetFocus then // Focus filter if window is undocked and top parent can focus
      TreeFilterEd.SetFocus;
    TreeFilterEd.SelectAll;
  end
  else                               // ComponentList is docked
    PageControl.AnchorSideBottom.Side := asrBottom;
end;

procedure TComponentListForm.ClearSelection;
begin
  ListTree.Selected := Nil;
  PalletteTree.Selected := Nil;
  InheritanceTree.Selected := Nil;
end;

function TComponentListForm.GetSelectedComponent: TRegisteredComponent;
begin
  Result := nil;
  if ListTree.IsVisible then
    Result := GetSelectedTreeComp(ListTree)
  else if PalletteTree.IsVisible then
    Result := GetSelectedTreeComp(PalletteTree)
  else if InheritanceTree.IsVisible then
    Result := GetSelectedTreeComp(InheritanceTree)
end;

function TComponentListForm.GetSelectedPkgComp: TPkgComponent;
begin
  Result:=GetSelectedComponent as TPkgComponent;
end;

function TComponentListForm.IsDocked: Boolean;
begin
  Result := (HostDockSite<>Nil) and (HostDockSite.Parent<>Nil);
end;

procedure TComponentListForm.ComponentWasAdded(ALookupRoot, AComponent: TComponent;
  ARegisteredComponent: TRegisteredComponent);
begin
  ClearSelection;
  UpdateButtonState;
end;

procedure TComponentListForm.miOpenPackage(Sender: TObject);
var
  PkgComponent: TPkgComponent;
begin
  PkgComponent:=GetSelectedPkgComp;
  if (PkgComponent=nil) or (PkgComponent.PkgFile=nil)
      or (PkgComponent.PkgFile.LazPackage=nil) then exit;
  if Assigned(OnOpenPackage) then
    OnOpenPackage(PkgComponent.PkgFile.LazPackage);
end;

procedure TComponentListForm.miOpenUnit(Sender: TObject);
var
  PkgComponent: TPkgComponent;
begin
  PkgComponent:=GetSelectedPkgComp;
  if (PkgComponent=nil) or (PkgComponent.PkgFile=nil)
      or (PkgComponent.PkgFile.LazPackage=nil) then exit;
  if Assigned(OnOpenUnit) then
    OnOpenUnit(PkgComponent);
end;

procedure TComponentListForm.SelectionWasChanged;
begin
  SelectionToolButton.Down := (IDEComponentPalette.Selected = nil);
  if FIgnoreSelection then
    Exit;
  if ListTree.IsVisible then
    SelectTreeComp(ListTree)
  else if PalletteTree.IsVisible then
    SelectTreeComp(PalletteTree)
  else if InheritanceTree.IsVisible then
    SelectTreeComp(InheritanceTree)
end;

procedure TComponentListForm.UpdateButtonState;
begin
  OKButton.Enabled := Assigned(GetSelectedComponent);
end;

procedure TComponentListForm.UpdateShowing;
begin
  if (ButtonPanel<>nil) and ButtonPanel.Visible then
    UpdateButtonState;
  inherited UpdateShowing;
end;

procedure TComponentListForm.AddComponentInheritanceNodes(
  ClassToNodeTree: TPointerToPointerTree; Comp: TRegisteredComponent);
// Walk down to parent, stop on TComponent,
//  since components are at least TComponent descendants.
var
  InheritanceList: TFPList;
  aClass: TClass;
  Node, ParentNode: TTreeNode;
  aClassName: string;
  i: Integer;
  II: TImageIndex;
  CurRegComp: TRegisteredComponent;
begin
  InheritanceList := TFPList.Create;
  try
    aClass := Comp.ComponentClass;
    while (aClass.ClassInfo <> nil) and (aClass.ClassType <> TComponent.ClassType) do
    begin
      InheritanceList.Add(TObject(aClass));
      aClass := aClass.ClassParent;
    end;
    // Build the tree
    ParentNode:=nil;
    for i := InheritanceList.Count - 1 downto 0 do
    begin
      aClass := TClass(InheritanceList[i]);
      Node:=TTreeNode(ClassToNodeTree[aClass]);
      if Node=nil then
      begin
        if aClass=Comp.ComponentClass then
          CurRegComp:=Comp
        else
          CurRegComp:=IDEComponentPalette.FindRegComponent(aClass);
        if CurRegComp<>nil then
          aClassName:=GetRegCompClassname(CurRegComp)
        else
          aClassName:=aClass.ClassName;

        // Add the node
        Node := InheritanceTree.Items.AddChildObject(ParentNode, aClassName, CurRegComp);
        ClassToNodeTree[aClass]:=Node;
        if CurRegComp is TPkgComponent then
          II := TPkgComponent(CurRegComp).ImageIndex
        else
          II := -1;
        if II>=0 then
        begin
          Node.ImageIndex := II;
          Node.SelectedIndex := II;
        end;
      end;
      ParentNode:=Node;
    end;
  finally
    InheritanceList.Free;
  end;
end;

procedure TComponentListForm.UpdateComponents;
// Fill all three tabsheets: Flat list, Palette layout and Component inheritence.
var
  Pg: TBaseComponentPage;
  Comps: TRegisteredCompList;
  Comp: TRegisteredComponent;
  ParentNode: TTreeNode;
  AListNode: TTreeNode;
  APaletteNode: TTreeNode;
  i, j: Integer;
  CurIcon: TImageIndex;
  aClassName: String;
  aClassToTreeNode: TPointerToPointerTree;
begin
  if [csDestroying,csLoading]*ComponentState<>[] then exit;
  Screen.BeginWaitCursor;
  ListTree.BeginUpdate;
  PalletteTree.BeginUpdate;
  InheritanceTree.Items.BeginUpdate;
  aClassToTreeNode := TPointerToPointerTree.Create;
  try
    ListTree.Items.Clear;
    PalletteTree.Items.Clear;
    InheritanceTree.Items.Clear;
 //   ParentInheritence := InheritanceTree.Items.Add(nil, 'TComponent');
//    FClassList.AddObject('TComponent', ParentInheritence);
    // Iterate all pages
    for i := 0 to IDEComponentPalette.Pages.Count-1 do
    begin
      Pg := IDEComponentPalette.Pages[i];
      if not Pg.Visible then Continue;
      Comps := IDEComponentPalette.RefUserCompsForPage(Pg.PageName);
      // Palette layout Page header
      ParentNode := PalletteTree.Items.AddChild(nil, Pg.PageName);
      // Iterate components of one page
      for j := 0 to Comps.Count-1 do begin
        Comp := Comps[j];
        if not Comp.Visible then Continue;
        aClassName := GetRegCompClassname(Comp);
        // Flat list item
        AListNode := ListTree.Items.AddChildObject(Nil, aClassName, Comp);
        // Palette layout item
        APaletteNode := PalletteTree.Items.AddChildObject(ParentNode, aClassName, Comp);
        if Comp is TPkgComponent then
          CurIcon := TPkgComponent(Comp).ImageIndex
        else
          CurIcon := -1;
        if CurIcon>=0 then
        begin
          AListNode.ImageIndex := CurIcon;
          AListNode.SelectedIndex := AListNode.ImageIndex;
          APaletteNode.ImageIndex := AListNode.ImageIndex;
          APaletteNode.SelectedIndex := AListNode.ImageIndex;
        end;
        // Component inheritence item
        AddComponentInheritanceNodes(aClassToTreeNode,Comp);
      end;
    end;
    InheritanceTree.AlphaSort;
    {$IFnDEF NoComponentListTreeExpand}
    InheritanceTree.FullExpand;    // Some users may not want the trees expanded.
    PalletteTree.FullExpand;
    {$ENDIF}
    PrevChangeStamp := IDEComponentPalette.ChangeStamp;
  finally
    aClassToTreeNode.Free;
    InheritanceTree.Items.EndUpdate;
    PalletteTree.EndUpdate;
    ListTree.EndUpdate;
    Screen.EndWaitCursor;
  end;
end;

procedure TComponentListForm.TreeFilterEdAfterFilter(Sender: TObject);
begin
  if TreeFilterEd.Filter = '' then
    IDEComponentPalette.SetSelectedComp(nil, False);
  UpdateButtonState;
end;

procedure TComponentListForm.ComponentsDblClick(Sender: TObject);
// This is used for all 3 treeviews
begin
  OKButtonClick(nil);       // Select and close this form
end;

procedure TComponentListForm.ListTreeSelectionChanged(Sender: TObject);
var
  AComponent: TRegisteredComponent;
begin
  UpdateButtonState;
  if FInitialized then
  begin
    if FPageControlChange then
      Exit;
    AComponent:=GetSelectedComponent;
    if AComponent<>nil then
      IDEComponentPalette.SetSelectedComp(AComponent, ssShift in GetKeyShiftState)
    else
    begin
      FIgnoreSelection := True;
      IDEComponentPalette.SetSelectedComp(nil, False);
      FIgnoreSelection := False;
    end;
  end
  else begin
    // Only run once when the IDE starts.
    FInitialized := True;
    IDEComponentPalette.SetSelectedComp(nil, False);
    ListTree.Selected := Nil;
    PalletteTree.Selected := Nil;
    InheritanceTree.Selected := Nil;
  end
end;

procedure TComponentListForm.TreeKeyPress(Sender: TObject; var Key: char);
// This is used for all 3 treeviews
begin
  if Key = Char(VK_RETURN) then
    ComponentsDblClick(Sender);
end;

procedure TComponentListForm.PageControlChange(Sender: TObject);
begin
  //DebugLn(['TComponentListForm.PageControlChange: Start']);
  FPageControlChange := True;
  case PageControl.PageIndex of
    0: begin
         TreeFilterEd.FilteredTreeview := ListTree;
         FActiveTree := ListTree;
        end;
    1: begin
         TreeFilterEd.FilteredTreeview := PalletteTree;
         FActiveTree := PalletteTree;
       end;
    2: begin
         TreeFilterEd.FilteredTreeview := InheritanceTree;
         FActiveTree := InheritanceTree;
        end;
  end;
  EnvironmentOptions.ComponentListPageIndex := PageControl.PageIndex;
  FActiveTree.BeginUpdate;
  tmDeselect.Enabled := True;
end;

function TComponentListForm.TreeFilterEdFilterItemEx(const ACaption: string;
  ItemData: Pointer; out Done: Boolean): Boolean;
begin
  Done := true;
  result := MultiWordSearch(TreeFilterEd.Text, ACaption);
end;

procedure TComponentListForm.TreeFilterEdKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  c: char;
begin
  if KeyToQWERTY(Key, Shift, c) then
    TreeFilterEd.SelText := c;
end;

procedure TComponentListForm.tmDeselectTimer(Sender: TObject);
begin
  tmDeselect.Enabled := False;
  FActiveTree.Selected := nil;
  SelectionWasChanged;
  FActiveTree.EndUpdate;
  FPageControlChange := False;
end;

procedure TComponentListForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  ClearSelection;
  IDEComponentPalette.Selected := Nil;
end;

procedure TComponentListForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  AComponent: TRegisteredComponent;
begin
  // add a neighboring component and leave focus on the selected component
  if (Key = VK_RETURN) and (Shift = [ssCtrl]) then
  begin
    AComponent := GetSelectedComponent;
    if AComponent <> nil then
      AddSelectedComponent(true, true);
    if TreeFilterEd.CanSetFocus then // not necessarily here?
      TreeFilterEd.SetFocus;
    Key := 0;
    exit;
  end;

  // add a child component and leave focus on the selected component
  if (Key = VK_RETURN) and (Shift = [ssShift]) then
  begin
    AComponent := GetSelectedComponent;
    if AComponent <> nil then
      AddSelectedComponent(true);
    if TreeFilterEd.CanSetFocus then // not necessarily here?
      TreeFilterEd.SetFocus;
    Key := 0;
    exit;
  end;

  // close
  if (Key = VK_ESCAPE) and (Shift = []) then
  begin
    if (IDEComponentPalette.Selected = nil) and not IsDocked then
      Close                            // close only if no component is selected
    else
      ClearSelection; // unselect if component is selected
    Key := 0;
    exit;
  end;

  // set focus on filter
  if (Key = VK_F) and (Shift = [ssCtrl]) then
  begin
    if TreeFilterEd.CanSetFocus then // not necessarily here?
      TreeFilterEd.SetFocus;
    Key := 0;
    exit;
  end;

  // select tab - [Ctrl+Tab] and [Ctrl+Shift+Tab]
  if (Key = VK_TAB) and ((Shift = [ssCtrl]) or (Shift = [ssCtrl, ssShift])) then
  begin
    with PageControl do
      if Shift = [ssCtrl]
        then PageIndex := (            PageIndex + 1) mod PageCount
        else PageIndex := (PageCount + PageIndex - 1) mod PageCount; // add "PageCount" - so that there is no negative number
    PageControlChange(Sender);
    Key := 0;
    exit;
  end;
end;

procedure TComponentListForm.OKButtonClick(Sender: TObject);
// Select component from palette and close this form. User can insert the component.
var
  AComponent: TRegisteredComponent;
  OldFocusedControl: TWinControl;
begin
  AComponent := GetSelectedComponent;
  if AComponent=nil then
    Exit;

  OldFocusedControl := Screen.ActiveControl;
  AddSelectedComponent;
  if (OldFocusedControl<>nil) and OldFocusedControl.CanSetFocus then
    OldFocusedControl.SetFocus; // AddComponent in docked mode steals focus to designer, get it back

  if not IsDocked and not chbKeepOpen.Checked then
    Close;
end;

procedure TComponentListForm.miCollapseAllClick(Sender: TObject);
begin
  TreeFilterEd.FilteredTreeview.FullCollapse;
end;

procedure TComponentListForm.miCollapseClick(Sender: TObject);
var
  Node: TTreeNode;
begin
  Node := TreeFilterEd.FilteredTreeview.Selected;
  if Node = nil then
    Exit;
  if (Node.Level > 0) and (Node.HasChildren = False) then
    Node := Node.Parent;
  Node.Collapse(True);
end;

procedure TComponentListForm.miExpandAllClick(Sender: TObject);
begin
  TreeFilterEd.FilteredTreeview.FullExpand;
end;

procedure TComponentListForm.miExpandClick(Sender: TObject);
var
  Node: TTreeNode;
begin
  Node := TreeFilterEd.FilteredTreeview.Selected;
  if Node = nil then
    Exit;
  if (Node.Level > 0) and (Node.HasChildren = False) then
    Node := Node.Parent;
  Node.Expand(True);
end;

procedure TComponentListForm.CompListPopupMenuPopup(Sender: TObject);
var
  Node: TTreeNode;
  PkgComponent: TPkgComponent;
  APackage: TLazPackage;
  PkgFile: TPkgFile;
  ShownFilename, UnitFilename: String;
begin
  //debugln(['TComponentListForm.CompListPopupMenuPopup ']);
  ComponentListMenuRoot.MenuItem:=CompListPopupMenu.Items;

  // expand/collapse
  if (PageControl.ActivePage=TabSheetPaletteTree)
      or (PageControl.ActivePage=TabSheetInheritance) then
  begin
    Node := TreeFilterEd.FilteredTreeview.Selected;
    if Node = nil then
    begin
      CompListMenuExpand.Enabled := False;
      CompListMenuCollapse.Enabled := False;
    end
    else
    begin
      CompListMenuExpand.Enabled := (Node.HasChildren) and (not Node.Expanded);
      CompListMenuCollapse.Enabled := (Node.HasChildren) and (Node.Expanded);
    end;
    CompListMenuExpand.Visible := true;
    CompListMenuExpandAll.Visible := true;
    CompListMenuCollapse.Visible := true;
    CompListMenuCollapseAll.Visible := true;
    CompListMenuExpand.OnClick:=@miExpandClick;
    CompListMenuExpandAll.OnClick:=@miExpandAllClick;
    CompListMenuCollapse.OnClick:=@miCollapseClick;
    CompListMenuCollapseAll.OnClick:=@miCollapseAllClick;
  end else begin
    CompListMenuExpand.Visible := false;
    CompListMenuExpandAll.Visible := false;
    CompListMenuCollapse.Visible := false;
    CompListMenuCollapseAll.Visible := false;
  end;

  // open unit/package
  PkgComponent:=GetSelectedPkgComp;
  if (PkgComponent=nil) or (PkgComponent.PkgFile=nil) then
  begin
    CompListMenuOpenUnit.Caption:=lisOpenUnit;
    CompListMenuOpenUnit.Visible:=false;
    CompListMenuOpenPackage.Caption:=lisOpenPackage3;
    CompListMenuOpenPackage.Visible:=false;
  end else begin
    PkgFile:=PkgComponent.PkgFile;
    APackage:=PkgFile.LazPackage;
    CompListMenuOpenPackage.Caption:=Format(lisCPOpenPackage, [APackage.IDAsString]);
    CompListMenuOpenPackage.Visible:=true;
    CompListMenuOpenPackage.OnClick:=@miOpenPackage;

    ShownFilename:=PkgFile.Filename;
    UnitFilename:=PkgFile.GetFullFilename;
    if not FileExistsCached(UnitFilename) then begin
      UnitFilename:=LazarusIDE.FindSourceFile(ExtractFilename(UnitFilename),
                                              APackage.Directory,[]);
      if FileExists(UnitFilename) then
        UnitFilename:=ShownFilename;
    end;
    CompListMenuOpenUnit.Caption:=Format(lisCPOpenUnit, [ShownFilename]);
    CompListMenuOpenUnit.Enabled:=FileExistsCached(UnitFilename);
    CompListMenuOpenUnit.Visible:=true;
    CompListMenuOpenUnit.OnClick:=@miOpenUnit;
  end;
end;

procedure TComponentListForm.SelectionToolButtonClick(Sender: TObject);
begin
  SelectionToolButton.Down := True;
  IDEComponentPalette.SetSelectedComp(nil, False);
end;

end.

