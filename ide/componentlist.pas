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
  // LazControls
  TreeFilterEdit,
  // IdeIntf
  FormEditingIntf, PropEdits, ComponentReg,
  // IDE
  LazarusIDEStrConsts, PackageDefs, IDEOptionDefs, EnvironmentOpts, Designer;

type

  { TComponentListForm }

  TComponentListForm = class(TForm)
    chbKeepOpen: TCheckBox;
    ListTree: TTreeView;
    ButtonPanel: TPanel;
    miCollapse: TMenuItem;
    miCollapseAll: TMenuItem;
    miExpand: TMenuItem;
    miExpandAll: TMenuItem;
    OKButton: TButton;
    LabelSearch: TLabel;
    PageControl: TPageControl;
    FilterPanel: TPanel;
    PalletteTree: TTreeView;
    InheritanceTree: TTreeView;
    pnPaletteTree: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    pmCollapseExpand: TPopupMenu;
    TabSheetPaletteTree: TTabSheet;
    TabSheetInheritance: TTabSheet;
    TabSheetList: TTabSheet;
    tmDeselect: TTimer;
    TreeFilterEd: TTreeFilterEdit;
    SelectionToolButton: TSpeedButton;
    procedure chbKeepOpenChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListTreeSelectionChanged(Sender: TObject);
    procedure miCollapseAllClick(Sender: TObject);
    procedure miCollapseClick(Sender: TObject);
    procedure miExpandAllClick(Sender: TObject);
    procedure miExpandClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure ComponentsDblClick(Sender: TObject);    
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);    
    procedure pmCollapseExpandPopup(Sender: TObject);
    procedure tmDeselectTimer(Sender: TObject);
    procedure TreeFilterEdAfterFilter(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure TreeKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure SelectionToolButtonClick(Sender: TObject);
  private
    PrevPageIndex: Integer;
    PrevChangeStamp: Integer;
    // List for Component inheritence view
    FClassList: TStringList;
    FKeepSelected: Boolean;
    FInitialized: Boolean;
    FIgnoreSelection: Boolean;
    FPageControlChange: Boolean;
    FActiveTree: TTreeView;
    FAddCompNewLeft, FAddCompNewTop: Integer;
    FAddCompNewParent: TComponent;
    procedure ClearSelection;
    procedure SelectionWasChanged;
    procedure ComponentWasAdded({%H-}ALookupRoot, {%H-}AComponent: TComponent;
                                {%H-}ARegisteredComponent: TRegisteredComponent);
    procedure DoComponentInheritence(Comp: TRegisteredComponent);
    procedure UpdateComponents;
    procedure UpdateButtonState;
    function IsDocked: Boolean;
    procedure AddSelectedComponent;
  protected
    procedure UpdateShowing; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetSelectedComponent: TRegisteredComponent;
  end;
  
var
  ComponentListForm: TComponentListForm;

implementation

{$R *.lfm}

{ TComponentListForm }

constructor TComponentListForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Name:=NonModalIDEWindowNames[nmiwComponentList];
  FActiveTree := ListTree;

  with SelectionToolButton do begin
    LoadGlyphFromResourceName(hInstance, 'tmouse');
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
  PrevPageIndex := -1;
  PageControl.ActivePage := TabSheetList;
  if Assigned(IDEComponentPalette) then
  begin
    UpdateComponents;
    TreeFilterEd.InvalidateFilter;
    IDEComponentPalette.AddHandlerSelectionChanged(@SelectionWasChanged);
    IDEComponentPalette.AddHandlerComponentAdded(@ComponentWasAdded);
  end;

  chbKeepOpen.Checked := EnvironmentOptions.ComponentListKeepOpen;
end;

procedure TComponentListForm.AddSelectedComponent;
var
  AComponent: TRegisteredComponent;
  ASelections: TPersistentSelectionList;
  NewParent: TComponent;
  CurDesigner: TDesigner;
begin
  AComponent := GetSelectedComponent;
  ASelections := TPersistentSelectionList.Create;
  try
    GlobalDesignHook.GetSelection(ASelections);
    if (ASelections.Count>0) and (ASelections[0] is TComponent) then
      NewParent := TComponent(ASelections[0])
    else if GlobalDesignHook.LookupRoot is TComponent then
      NewParent := TComponent(GlobalDesignHook.LookupRoot)
    else
      NewParent := nil;
  finally
    ASelections.Free;
  end;

  if NewParent=nil then
    Exit;

  CurDesigner:=TDesigner(FindRootDesigner(NewParent));
  if CurDesigner=nil then
    Exit;

  CurDesigner.AddComponentCheckParent(NewParent, NewParent, nil, AComponent.ComponentClass);
  if NewParent=nil then
    Exit;

  if FAddCompNewParent<>NewParent then
  begin
    FAddCompNewLeft := 0;
    FAddCompNewTop := 0;
    FAddCompNewParent := NewParent;
  end;
  Inc(FAddCompNewLeft, 8);
  Inc(FAddCompNewTop, 8);
  CurDesigner.AddComponent(AComponent, AComponent.ComponentClass, NewParent, FAddCompNewLeft, FAddCompNewTop, 0, 0);
end;

procedure TComponentListForm.chbKeepOpenChange(Sender: TObject);
begin
  EnvironmentOptions.ComponentListKeepOpen := chbKeepOpen.Checked;
end;

destructor TComponentListForm.Destroy;
begin
  if Assigned(IDEComponentPalette) then
    IDEComponentPalette.RemoveHandlerComponentAdded(@ComponentWasAdded);
  ComponentListForm := nil;
  inherited Destroy;
end;

procedure TComponentListForm.FormShow(Sender: TObject);
begin
  //DebugLn(['*** TComponentListForm.FormShow, Parent=', Parent, ', Parent.Parent=', ParentParent]);
  ButtonPanel.Visible := not IsDocked;
  if ButtonPanel.Visible then
  begin                              // ComponentList is undocked
    PageControl.AnchorSideBottom.Side := asrTop;
    UpdateButtonState;
    if TreeFilterEd.CanFocus then    // Focus filter if window is undocked
      TreeFilterEd.SetFocus;
    TreeFilterEd.SelectAll;
  end
  else                               // ComponentList is docked
    PageControl.AnchorSideBottom.Side := asrBottom;
end;

procedure TComponentListForm.FormActivate(Sender: TObject);
begin
  if Assigned(IDEComponentPalette) and (IDEComponentPalette.ChangeStamp<>PrevChangeStamp) then
    UpdateComponents;
end;

procedure TComponentListForm.ClearSelection;
begin
  ListTree.Selected := Nil;
  PalletteTree.Selected := Nil;
  InheritanceTree.Selected := Nil;
end;

procedure SelectTreeComp(aTree: TTreeView);
var
  Node: TTreeNode;
begin
  with IDEComponentPalette do
    if Assigned(Selected) then
      Node := aTree.Items.FindNodeWithText(Selected.ComponentClass.ClassName)
    else
      Node := Nil;
  aTree.Selected := Node;
  if aTree.Selected <> nil then
    aTree.Selected.MakeVisible;
end;

procedure TComponentListForm.SelectionWasChanged;
begin
  SelectionToolButton.Down := (IDEComponentPalette.Selected = nil);

  // ToDo: Select the component in active treeview.
  if FIgnoreSelection then
    Exit;

  if ListTree.IsVisible then
    SelectTreeComp(ListTree)
  else if PalletteTree.IsVisible then
    SelectTreeComp(PalletteTree)
  else if InheritanceTree.IsVisible then
    SelectTreeComp(InheritanceTree)
end;

function GetSelectedTreeComp(aTree: TTreeView): TRegisteredComponent;
begin
  if Assigned(aTree.Selected) then
    Result := TRegisteredComponent(aTree.Selected.Data)
  else
    Result := nil;
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

procedure TComponentListForm.DoComponentInheritence(Comp: TRegisteredComponent);
// Walk down to parent, stop on TComponent,
//  since components are at least TComponent descendants.
var
  PalList: TStringList;
  AClass: TClass;
  Node: TTreeNode;
  ClssName: string;
  i, Ind: Integer;
  II: TImageIndex;
begin
  PalList := TStringList.Create;
  try
    AClass := Comp.ComponentClass;
    while (AClass.ClassInfo <> nil) and (AClass.ClassType <> TComponent.ClassType) do
    begin
      PalList.AddObject(AClass.ClassName, TObject(AClass));
      AClass := AClass.ClassParent;
    end;
    // Build the tree
    for i := PalList.Count - 1 downto 0 do
    begin
      AClass := TClass(PalList.Objects[i]);
      ClssName := PalList[i];
      if not FClassList.Find(ClssName, Ind) then
      begin
        // Find out parent position
        if Assigned(AClass.ClassParent)
        and FClassList.Find(AClass.ClassParent.ClassName, Ind) then
          Node := TTreeNode(FClassList.Objects[Ind])
        else
          Node := nil;
        // Add the item
        if ClssName <> Comp.ComponentClass.ClassName then
          Node := InheritanceTree.Items.AddChild(Node, ClssName)
        else
        begin
          Node := InheritanceTree.Items.AddChildObject(Node, ClssName, Comp);
          if Comp is TPkgComponent then
            II := TPkgComponent(Comp).ImageIndex
          else
            II := -1;
          if II>=0 then
          begin
            Node.ImageIndex := II;
            Node.SelectedIndex := Node.ImageIndex;
          end;
        end;
        FClassList.AddObject(ClssName, Node);
      end;
    end;
  finally
    PalList.Free;
  end;
end;

procedure TComponentListForm.UpdateComponents;
// Fill all three tabsheets: Flat list, Palette layout and Component inheritence.
var
  Pg: TBaseComponentPage;
  Comps: TStringList;
  Comp: TRegisteredComponent;
  ParentNode: TTreeNode;
  AListNode: TTreeNode;
  APaletteNode: TTreeNode;
  i, j: Integer;
  CurIcon: TImageIndex;
begin
  if [csDestroying,csLoading]*ComponentState<>[] then exit;
  Screen.BeginWaitCursor;
  ListTree.BeginUpdate;
  PalletteTree.BeginUpdate;
  InheritanceTree.Items.BeginUpdate;
  FClassList := TStringList.Create;
  try
    ListTree.Items.Clear;
    PalletteTree.Items.Clear;
    InheritanceTree.Items.Clear;
    FClassList.Sorted := true;
    FClassList.CaseSensitive := false;
    FClassList.Duplicates := dupIgnore;
 //   ParentInheritence := InheritanceTree.Items.Add(nil, 'TComponent');
//    FClassList.AddObject('TComponent', ParentInheritence);
    // Iterate all pages
    for i := 0 to IDEComponentPalette.Pages.Count-1 do
    begin
      Pg := IDEComponentPalette.Pages[i];
      Comps := IDEComponentPalette.RefUserCompsForPage(Pg.PageName);
      // Palette layout Page header
      ParentNode := PalletteTree.Items.AddChild(nil, Pg.PageName);
      // Iterate components of one page
      for j := 0 to Comps.Count-1 do begin
        Comp := Comps.Objects[j] as TRegisteredComponent;
        // Flat list item
        AListNode := ListTree.Items.AddChildObject(Nil, Comps[j], Comp);
        // Palette layout item
        APaletteNode := PalletteTree.Items.AddChildObject(ParentNode, Comps[j], Comp);
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
        DoComponentInheritence(Comp);
      end;
    end;
    PalletteTree.FullExpand;
    InheritanceTree.AlphaSort;
    InheritanceTree.FullExpand;
    PrevChangeStamp := IDEComponentPalette.ChangeStamp;
  finally
    FClassList.Free;
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
  FPageControlChange := True;
  Assert(PageControl.PageIndex <> PrevPageIndex, Format(
    'TComponentListForm.PageControlChange: PageControl.PageIndex = PrevPageIndex = %d',
    [PrevPageIndex]));
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
  TreeFilterEd.InvalidateFilter;
  PrevPageIndex := PageControl.PageIndex;
  FActiveTree.BeginUpdate;
  tmDeselect.Enabled := True;
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
  if not FKeepSelected then
  begin
    ClearSelection;
    IDEComponentPalette.Selected := Nil;
  end;
  FKeepSelected := False;
end;

procedure TComponentListForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key=VK_ESCAPE then
  begin
    if (IDEComponentPalette.Selected = nil) and not IsDocked  then //close only if no component is selected
      Close
    else
      ClearSelection; //unselect if component is selected
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

  if IsDocked or chbKeepOpen.Checked then
  begin
    OldFocusedControl := Screen.ActiveControl;
    AddSelectedComponent;
    if (OldFocusedControl<>nil) and OldFocusedControl.CanSetFocus then // AddComponent in docked mode steals focus to designer, get it back
      OldFocusedControl.SetFocus;
  end else
  begin
    FKeepSelected := True;
    Close;
  end;
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

procedure TComponentListForm.pmCollapseExpandPopup(Sender: TObject);
var
  Node: TTreeNode;
begin
  Node := TreeFilterEd.FilteredTreeview.Selected;
  if Node = nil then
  begin
    miExpand.Enabled := False;
    miCollapse.Enabled := False;
  end
  else
  begin
    miExpand.Enabled := (Node.HasChildren) and (not Node.Expanded);
    miCollapse.Enabled := (Node.HasChildren) and (Node.Expanded);
  end;
end;

procedure TComponentListForm.SelectionToolButtonClick(Sender: TObject);
begin
  SelectionToolButton.Down := True;
  IDEComponentPalette.SetSelectedComp(nil, False);
end;

end.

