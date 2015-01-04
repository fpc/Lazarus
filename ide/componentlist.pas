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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Marius
  Modified by Juha Manninen

  Abstract:
    A dialog to quickly find components and to add the found component
    to the designed form.
}
unit ComponentList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, Forms, Controls, Graphics, StdCtrls, ExtCtrls,
  ComCtrls, ButtonPanel, LazarusIDEStrConsts, ComponentReg,
  PackageDefs, IDEImagesIntf, TreeFilterEdit, fgl;

type

  TRegisteredCompList = specialize TFPGList<TRegisteredComponent>;

  { TComponentListForm }

  TComponentListForm = class(TForm)
    ListTree: TTreeView;
    ButtonPanel: TButtonPanel;
    OKButton: TPanelBitBtn;
    LabelSearch: TLabel;
    PageControl: TPageControl;
    FilterPanel: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    TabSheetInheritance: TTabSheet;
    TabSheetList: TTabSheet;
    TabSheetPaletteTree: TTabSheet;
    InheritanceTree: TTreeView;
    PalletteTree: TTreeView;
    TreeFilterEd: TTreeFilterEdit;
    procedure FormActivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure ComponentsDblClick(Sender: TObject);
    procedure ComponentsClick(Sender: TObject);
    //procedure ComponentsListboxDrawItem(Control: TWinControl; Index: Integer;
    //  ARect: TRect; State: TOwnerDrawState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure TreeCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure TreeFilterEdAfterFilter(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure TreeKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    PrevPageIndex: Integer;
    PrevChangeStamp: Integer;
    // List for Component inheritence view
    FClassList: TStringList;
    FKeepSelected: Boolean;
    procedure ClearSelection;
    procedure ComponentWasAdded;
    procedure DoComponentInheritence(Comp: TRegisteredComponent);
    procedure UpdateComponentSelection;
    procedure UpdateButtonState;
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
  //Translations
  LabelSearch.Caption := lisMenuFind;
  Caption := lisCmpLstComponents;
  TabSheetList.Caption := lisCmpLstList;
  TabSheetPaletteTree.Caption := lisCmpLstPalette;
  TabSheetInheritance.Caption := lisCmpLstInheritance;
  ButtonPanel.OKButton.Caption := lisUseAndClose;

  ListTree.Images:=IDEImages.Images_24;
  InheritanceTree.Images:=ListTree.Images;
  PalletteTree.Images:=ListTree.Images;
  PrevPageIndex := -1;
  PageControl.ActivePage := TabSheetList;
  if Assigned(IDEComponentPalette) then
  begin
    UpdateComponentSelection;
    with ListTree do
      Selected := Items.GetFirstNode;
    TreeFilterEd.InvalidateFilter;
    IDEComponentPalette.AddHandlerComponentAdded(@ComponentWasAdded);
  end;
end;

destructor TComponentListForm.Destroy;
begin
  if Assigned(IDEComponentPalette) then
    IDEComponentPalette.RemoveHandlerComponentAdded(@ComponentWasAdded);
  ComponentListForm := nil;
  inherited Destroy;
end;

procedure TComponentListForm.FormShow(Sender: TObject);
var
  ParentParent: TWinControl;  // Used for checking if the form is anchored.
begin
  ParentParent := Nil;
  if Assigned(Parent) then
    ParentParent := Parent.Parent;
  //DebugLn(['*** TComponentListForm.FormShow, Parent=', Parent, ', Parent.Parent=', ParentParent]);
  ButtonPanel.Visible := ParentParent=Nil;
  if ButtonPanel.Visible then begin
    PageControl.AnchorSideBottom.Side := asrTop;
    UpdateButtonState;
  end
  else
    PageControl.AnchorSideBottom.Side := asrBottom;
end;

procedure TComponentListForm.FormActivate(Sender: TObject);
begin
  if Assigned(IDEComponentPalette) and (IDEComponentPalette.ChangeStamp<>PrevChangeStamp) then
    UpdateComponentSelection;
end;

procedure TComponentListForm.ClearSelection;
begin
  ListTree.Selected := Nil;
  PalletteTree.Selected := Nil;
  InheritanceTree.Selected := Nil;
end;

function TComponentListForm.GetSelectedComponent: TRegisteredComponent;
begin
  Result:=nil;
  if ListTree.IsVisible then
  begin
    if Assigned(ListTree.Selected) then
      Result := TRegisteredComponent(ListTree.Selected.Data);
  end
  else if PalletteTree.IsVisible then
  begin
    if Assigned(PalletteTree.Selected) then
      Result := TRegisteredComponent(PalletteTree.Selected.Data);
  end
  else if InheritanceTree.IsVisible then
  begin
    if Assigned(InheritanceTree.Selected) then
      Result := TRegisteredComponent(InheritanceTree.Selected.Data);
  end;
end;

procedure TComponentListForm.ComponentWasAdded;
begin
  ClearSelection;
  UpdateButtonState;
end;

procedure TComponentListForm.UpdateButtonState;
begin
  ButtonPanel.OKButton.Enabled := Assigned(GetSelectedComponent);
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
          Node := InheritanceTree.Items.AddChildObject(Node, ClssName, Comp);
        FClassList.AddObject(ClssName, Node);
      end;
    end;
  finally
    PalList.Free;
  end;
end;

procedure TComponentListForm.UpdateComponentSelection;
// Fill all three tabsheets: Flat list, Palette layout and Component inheritence.
var
  Pg: TBaseComponentPage;
  Comps: TStringList;
  Comp: TRegisteredComponent;
  ParentNode: TTreeNode;
  i, j: Integer;
begin
  if [csDestroying,csLoading]*ComponentState<>[] then exit;
  Screen.Cursor := crHourGlass;
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
        ListTree.Items.AddChildObject(Nil, Comps[j], Comp);
        // Palette layout item
        PalletteTree.Items.AddChildObject(ParentNode, Comps[j], Comp);
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
    Screen.Cursor := crDefault;
  end;
end;

procedure TComponentListForm.TreeCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  Comp: TRegisteredComponent;
  ARect: TRect;
  CurIcon: TCustomBitmap;
  Indent, IconWidth, IconHeight, NodeTextHeight: Integer;
begin
  DefaultDraw := False;
  Indent := (Sender as TTreeView).Indent;
  Comp := TRegisteredComponent(Node.Data);
  with Sender.Canvas do
  begin
    if cdsSelected in State then
    begin
      Brush.Color := clHighlight;   //Brush.Style := ...
      Font.Color := clHighlightText;
    end
    else begin
      Brush.Color := clDefault;
      Font.Color := clDefault;
    end;
    ARect := Node.DisplayRect(False);
    FillRect(ARect);
    //Brush.Style := bsClear;     //don't paint over the background bitmap.
    ARect.Left := ARect.Left + (Node.Level * Indent);
    // ARect.Left now points to the left of the image, or text if no image
    CurIcon := nil;
    if Comp is TPkgComponent then
      CurIcon := TPkgComponent(Comp).Icon;
    if CurIcon<>nil then
    begin
      IconWidth := CurIcon.Width;
      IconHeight := CurIcon.Height;
      ARect.Left := ARect.Left + Indent;
      //ARect.Left is now the leftmost portion of the image.
      Draw(ARect.Left+(25-IconWidth) div 2,
           ARect.Top+(ARect.Bottom-ARect.Top-IconHeight) div 2, CurIcon);
      ARect.Left := ARect.Left + IconWidth + 2;
    end;
    NodeTextHeight := TextHeight(Node.Text);
    Inc(ARect.Top, (ARect.Bottom - ARect.Top - NodeTextHeight) div 2);
    //Now we are finally in a position to draw the text.
    TextOut(ARect.Left, ARect.Top, Node.Text);
  end;
end;

procedure TComponentListForm.TreeFilterEdAfterFilter(Sender: TObject);
begin
  UpdateButtonState;
end;

procedure TComponentListForm.ComponentsDblClick(Sender: TObject);
// This is used for all 3 treeviews
begin
  OKButtonClick(nil);       // Select and close this form
end;

procedure TComponentListForm.ComponentsClick(Sender: TObject);
// This is used for all 3 treeviews
var
  AComponent: TRegisteredComponent;
begin
  AComponent:=GetSelectedComponent;
  if AComponent<>nil then
    IDEComponentPalette.Selected:=AComponent;
  UpdateButtonState;
end;

procedure TComponentListForm.TreeKeyPress(Sender: TObject; var Key: char);
// This is used for all 3 treeviews
begin
  if Key = Char(VK_RETURN) then
    ComponentsDblClick(Sender);
end;

procedure TComponentListForm.PageControlChange(Sender: TObject);
begin
  Assert(PageControl.PageIndex <> PrevPageIndex, Format(
    'TComponentListForm.PageControlChange: PageControl.PageIndex = PrevPageIndex = %d',
    [PrevPageIndex]));
  case PageControl.PageIndex of
    0: TreeFilterEd.FilteredTreeview := ListTree;
    1: TreeFilterEd.FilteredTreeview := PalletteTree;
    2: TreeFilterEd.FilteredTreeview := InheritanceTree;
  end;
  TreeFilterEd.InvalidateFilter;
  PrevPageIndex := PageControl.PageIndex;
end;

procedure TComponentListForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Parent=nil then begin
    ClearSelection;
    if not fKeepSelected then
      IDEComponentPalette.Selected := Nil;
  end
  else begin
    // Using a dock manager...
    CloseAction := caNone;
    //todo: helper function in DockManager or IDEDockMaster for closing forms.
    // Only close the window if it's floating.
    // AnchorDocking doesn't seem to initialize 'FloatingDockSiteClass' so we can't just check 'Floating'.
    // Also, AnchorDocking use nested forms, so the check for HostDockSite.Parent.
    if Assigned(HostDockSite) and (HostDockSite.DockClientCount <= 1)
      and (HostDockSite is TCustomForm) and (HostDockSite.Parent = nil) then
    begin
      TCustomForm(HostDockSite).Close;
    end;
  end;
  FKeepSelected := False;
end;

procedure TComponentListForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
//Close the form on escape key like every other IDE dialog does
begin
  if Key=VK_ESCAPE then
    Close;
end;

procedure TComponentListForm.OKButtonClick(Sender: TObject);
// Select component from palette and close this form. User can insert the component.
var
  AComponent: TRegisteredComponent;
begin
  AComponent := GetSelectedComponent;
  if AComponent<>nil then begin
    IDEComponentPalette.Selected := AComponent;
    FKeepSelected := True;
    Close;
  end;
end;

end.

