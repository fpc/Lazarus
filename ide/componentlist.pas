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
    A dialog to quickly find components and create the found components
    directly on the designed form.
}
unit ComponentList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, Forms, Controls, Graphics, StdCtrls, ExtCtrls,
  ComCtrls, ButtonPanel, Dialogs, LazarusIDEStrConsts, ComponentReg,
  PackageDefs, FormEditingIntf, PropEdits, ListFilterEdit, TreeFilterEdit, fgl;

type

  TRegisteredCompList = specialize TFPGList<TRegisteredComponent>;

  { TComponentListForm }

  TComponentListForm = class(TForm)
    ButtonPanel: TButtonPanel;
    LabelSearch: TLabel;
    ComponentsListbox: TListBox;
    ListFilterEd: TListFilterEdit;
    PageControl: TPageControl;
    Panel3: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    TabSheetInheritance: TTabSheet;
    TabSheetListBox: TTabSheet;
    TabSheetPaletteTree: TTabSheet;
    InheritanceTree: TTreeView;
    PalletteTree: TTreeView;
    TreeFilterEd: TTreeFilterEdit;
    procedure ComponentsListboxDblClick(Sender: TObject);
    procedure ComponentsListboxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure PalletteTreeCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure ComponentsListboxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure InheritanceTreeDblClick ( Sender: TObject ) ;
    procedure InheritanceTreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure PageControlChange(Sender: TObject);
    procedure PalletteTreeDblClick(Sender: TObject);
    procedure PalletteTreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure UpdateComponentSelection(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    PrevPageIndex: Integer;
    Processing: boolean;
    FComponentList: TRegisteredCompList;
    procedure FindAllLazarusComponents;
    procedure AddSelectedComponent(AComponent: TRegisteredComponent);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;
  
var
  ComponentListForm: TComponentListForm;

implementation

{$R *.lfm}

{ TComponentListForm }

constructor TComponentListForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FComponentList := TRegisteredCompList.Create;
  ButtonPanel.CloseButton.Cancel := True;
  ComponentsListBox.ItemHeight  :=ComponentPaletteImageHeight + 2;
  PalletteTree.DefaultItemHeight:=ComponentPaletteImageHeight + 2;

  //Translations..
  LabelSearch.Caption := lisMenuFind;
  Caption := lisCmpLstComponents;
  TabSheetListBox.Caption := lisCmpLstList;
  TabSheetPaletteTree.Caption := lisCmpLstPalette;
  TabSheetInheritance.Caption := lisCmpLstInheritance;

  //PLEASE add a defaultpage property in TPagecontrol
  PageControl.ActivePage := TabSheetListBox;
  PrevPageIndex := -1;
  TreeFilterEd.Visible := False;

  FindAllLazarusComponents;
  UpdateComponentSelection(nil);
  ListFilterEd.InvalidateFilter;
end;

destructor TComponentListForm.Destroy;
begin
  ComponentListForm := nil;
  FComponentList.Free;
  inherited Destroy;
end;

procedure TComponentListForm.FindAllLazarusComponents;
//Collect all available components (excluding hidden)
var
  AComponent: TRegisteredComponent;
  APage: TBaseComponentPage;
  i, j: Integer;
begin
  if Assigned(IDEComponentPalette) then
  begin
    for i := 0 to IDEComponentPalette.Count-1 do
    begin
      APage := IDEComponentPalette.Pages[i];
      if APage.Visible then
        for j :=  0 to APage.Count-1 do
        begin
          AComponent := APage.Items[j];
          if AComponent.Visible and (AComponent.PageName<>'') then
            FComponentList.Add(AComponent);
        end;
    end;
  end;
end;

procedure TComponentListForm.UpdateComponentSelection(Sender: TObject);
//Apply the filter and fill the three tabsheets
var
  AComponent: TRegisteredComponent;
  AClassName: string;
  AClassList, List: TStringlist;
  i, j, AIndex: Integer;
  ANode: TTreeNode;
  AClass: TClass;
begin
  if Processing then exit;
  Processing := true;
  Screen.Cursor := crHourGlass;
  try

    //First tabsheet (ComponentsListbox)
    ComponentsListbox.Items.BeginUpdate;
    try
      ComponentsListbox.Items.Clear;
      for i := 0 to FComponentList.Count-1 do
      begin
        AComponent := FComponentList[i];
        AClassName := AComponent.ComponentClass.ClassName;
        ListFilterEd.Data.AddObject(AClassName, AComponent);
      end;
    finally
      ComponentsListbox.Items.EndUpdate;
    end;

    //Second tabsheet (palette layout)
    PalletteTree.BeginUpdate;
    try
      PalletteTree.Items.Clear;
      for i := 0 to FComponentList.Count-1 do
      begin
        AComponent := FComponentList[i];
        AClassName := AComponent.ComponentClass.ClassName;
        //find out parent node
        ANode := PalletteTree.Items.FindTopLvlNode(AComponent.PageName);
        if ANode = nil then
          ANode := PalletteTree.Items.AddChild(nil, AComponent.PageName);
        //add the item
        ANode := PalletteTree.Items.AddChildObject(ANode, AClassName, AComponent);
      end;
      PalletteTree.FullExpand;
    finally
      PalletteTree.EndUpdate;
    end;

    //Third tabsheet (component inheritence)
    List := TStringlist.Create;
    AClassList:= TStringlist.Create;
    InheritanceTree.Items.BeginUpdate;
    try
      InheritanceTree.Items.Clear;
      AClassList.Sorted := true;
      AClassList.CaseSensitive := false;
      AClassList.Duplicates := dupIgnore;

      for i := 0 to FComponentList.Count-1 do
      begin
        AComponent := FComponentList[i];
        AClassName := AComponent.ComponentClass.ClassName;
        // walk down to parent, stop on tcomponent, since components are at least
        // a tcomponent descendant
        List.Clear;
        AClass := AComponent.ComponentClass;
        while (AClass.ClassInfo <> nil) and (AClass.ClassType <> TComponent.ClassType) do
        begin
          List.AddObject(AClass.ClassName, TObject(AClass));
          AClass := AClass.ClassParent;
        end;

        //build the tree
        for j := List.Count - 1 downto 0 do
        begin
          AClass := TClass(List.Objects[j]);
          AClassName := List[j];

          if not AClassList.Find(AClassName, AIndex)
          then begin
            //find out parent position
            if Assigned(AClass.ClassParent) and AClassList.Find(AClass.ClassParent.ClassName, AIndex)
            then ANode := TTreeNode(AClassList.Objects[AIndex])
            else ANode := nil;

            //add the item
            if AClassName <> AComponent.ComponentClass.ClassName
            then ANode := InheritanceTree.Items.AddChild(ANode, AClassName)
            else ANode := InheritanceTree.Items.AddChildObject(ANode, AClassName, AComponent);
            AClassList.AddObject(AClassName, ANode);
          end;
        end;
      end;

      InheritanceTree.AlphaSort;
      InheritanceTree.FullExpand;
    finally
      List.Free;
      AClassList.Free;
      InheritanceTree.Items.EndUpdate;
    end;
    
  finally
    Screen.Cursor := crDefault;
    Processing := false;
  end;
end;

procedure TComponentListForm.AddSelectedComponent(AComponent: TRegisteredComponent);
// Add the DblClicked component to the current designed form
var
  TypeClass: TComponentClass;
  X, Y: integer;
  DisableAutoSize: Boolean;
  ParentComponent: TComponent;
  NewComponent: TComponent;
begin
  if not Assigned(AComponent) then Exit;
  if not Assigned(FormEditingHook) then Exit;
  //TComponentPalette(IDEComponentPalette).Selected := AComponent;

  TypeClass:=AComponent.ComponentClass;
  ParentComponent:=FormEditingHook.GetDefaultComponentParent(TypeClass);
  if ParentComponent=nil then exit;

  //Would be lovely if it would only select the component as if it was
  //clicked in the palette bar so you can drop it on your own position.
  if not FormEditingHook.GetDefaultComponentPosition(TypeClass,ParentComponent,X,Y)
  then exit;

  DisableAutoSize:=true;
  NewComponent:=FormEditingHook.CreateComponent(ParentComponent,TypeClass,'',
                                                X,Y,0,0,DisableAutoSize);
  if Assigned(NewComponent) then begin
    if DisableAutoSize and (NewComponent is TControl) then
      TControl(NewComponent).EnableAutoSizing;
    GlobalDesignHook.PersistentAdded(NewComponent,true);
  end;
end;

procedure TComponentListForm.ComponentsListboxDblClick(Sender: TObject);
begin
  AddSelectedComponent(TRegisteredComponent(ComponentsListbox.Items.Objects[ComponentsListbox.ItemIndex]));
end;

procedure TComponentListForm.ComponentsListboxDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  Comp: TRegisteredComponent;
  CurStr: string;
  CurIcon: TCustomBitmap;
  TxtH, IconWidth, IconHeight: Integer;
begin
  if (Index<0) or (Index>=ComponentsListBox.Items.Count) then exit;
  // draw registered component
  Comp:=TRegisteredComponent(ComponentsListBox.Items.Objects[Index]);
  with ComponentsListBox.Canvas do begin
    CurStr:=Comp.ComponentClass.ClassName;
//  CurStr:=Format(lisPckEditPage,[Comp.ComponentClass.ClassName,Comp.Page.PageName]);
    TxtH:=TextHeight(CurStr);
    FillRect(ARect);
    CurIcon:=nil;
    if Comp is TPkgComponent then
      CurIcon:=TPkgComponent(Comp).Icon;
    if CurIcon<>nil then
    begin
      IconWidth:=CurIcon.Width;
      IconHeight:=CurIcon.Height;
      Draw(ARect.Left+(25-IconWidth) div 2,
           ARect.Top+(ARect.Bottom-ARect.Top-IconHeight) div 2, CurIcon);
    end;
    TextOut(ARect.Left+25,
            ARect.Top+(ARect.Bottom-ARect.Top-TxtH) div 2, CurStr);
  end;
end;

procedure TComponentListForm.PalletteTreeCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  Comp: TRegisteredComponent;
  ARect: TRect;
  CurIcon: TCustomBitmap;
  Indent, IconWidth, IconHeight: Integer;
begin
  DefaultDraw := False;
  Indent := (Sender as TTreeView).Indent;
  Comp:=TRegisteredComponent(Node.Data);
  with Sender.Canvas do
  begin
    if cdsSelected in State then
    begin
      Brush.Color := clHighlight;   //Brush.Style := ...
      Font.Color := clHighlightedText;
    end
    else begin
      Brush.Color := clDefault;  //Brush.Style := ...
      Font.Color := clDefault;
    end;
    ARect := Node.DisplayRect(False);
    FillRect(ARect);
    //Brush.Style := bsClear;     //don't paint over the background bitmap.
    ARect.Left := ARect.Left + (Node.Level * Indent);
    // ARect.Left now points to the left of the image, or text if no image
    CurIcon:=nil;
    if Comp is TPkgComponent then
      CurIcon:=TPkgComponent(Comp).Icon;
    if CurIcon<>nil then
    begin
      IconWidth:=CurIcon.Width;
      IconHeight:=CurIcon.Height;
      ARect.Left := ARect.Left + Indent;
      //ARect.Left is now the leftmost portion of the image.
      Draw(ARect.Left+(25-IconWidth) div 2,
           ARect.Top+(ARect.Bottom-ARect.Top-IconHeight) div 2, CurIcon);
      ARect.Left := ARect.Left + IconWidth + 2;
    end;
    //Now we are finally in a position to draw the text.
    TextOut(ARect.Left, ARect.Top, Node.Text);
  end;
end;

procedure TComponentListForm.ComponentsListboxKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    if (ComponentsListbox.ItemIndex >= 0) then
      ComponentsListboxDblClick(Sender);
end;

procedure TComponentListForm.PalletteTreeDblClick(Sender: TObject);
var
  AComponent: TRegisteredComponent;
begin
  if not Assigned(PalletteTree.Selected) then exit;
  AComponent := TRegisteredComponent(PalletteTree.Selected.Data);
  if not Assigned(AComponent) then exit;
  AddSelectedComponent(AComponent);
end;

procedure TComponentListForm.PalletteTreeKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    PalletteTreeDblClick(Sender);
end;

procedure TComponentListForm.InheritanceTreeDblClick(Sender:TObject);
var
  AComponent: TRegisteredComponent;
begin
  if not Assigned(InheritanceTree.Selected) then exit;
  AComponent := TRegisteredComponent(InheritanceTree.Selected.Data);
  if not Assigned(AComponent) then exit;
  AddSelectedComponent(AComponent);
end;

procedure TComponentListForm.InheritanceTreeKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    InheritanceTreeDblClick(Sender);
end;

procedure TComponentListForm.PageControlChange(Sender: TObject);
begin
  Assert(PageControl.PageIndex <> PrevPageIndex,
         Format('PageControl.PageIndex = PrevPageIndex = %d', [PrevPageIndex]));
  case PageControl.PageIndex of
    0: begin
      ListFilterEd.Visible := True;
      ListFilterEd.SetFocus;
      ListFilterEd.Text := TreeFilterEd.Text;
      ListFilterEd.InvalidateFilter;
      TreeFilterEd.Visible := False;
    end;
    1: begin
      TreeFilterEd.Visible := True;
      TreeFilterEd.FilteredTreeview := PalletteTree;
      TreeFilterEd.SetFocus;
      if PrevPageIndex = 0 then
        TreeFilterEd.Text := ListFilterEd.Text;
      TreeFilterEd.InvalidateFilter;
      ListFilterEd.Visible := False;
    end;
    2: begin
      TreeFilterEd.Visible := True;
      TreeFilterEd.FilteredTreeview := InheritanceTree;
      TreeFilterEd.SetFocus;
      if PrevPageIndex = 0 then
        TreeFilterEd.Text := ListFilterEd.Text;
      TreeFilterEd.InvalidateFilter;
      ListFilterEd.Visible := False;
    end;
  end;
  PrevPageIndex := PageControl.PageIndex;
end;

procedure TComponentListForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
//Close the form on escape key like every other IDE dialog does
begin
  if (Key=VK_ESCAPE) and (Parent=nil) then
    Close;
end;

end.

