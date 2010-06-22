{  $Id$  }
{
 /***************************************************************************
                          findcomponent.pas
                          --------------------


 ***************************************************************************/

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

  Abstract:
    A dialog to quickly find components and create the found components
    directly on the designed form. This avoids a lot of scrolling in my
    already overfull palette (i love having the lazarus sources!).
}
unit ComponentList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, FormEditingIntf, LazarusIDEStrConsts, ExtCtrls, ComCtrls,
  ComponentPalette, ComponentReg, PackageDefs, ExtDlgs, FormEditor, PropEdits,
  LCLType, Menus, ButtonPanel, IDEWindowIntf;


const
  ComponentListFormName = 'ComponentList';
type
  { TComponentListForm }

  TComponentListForm = class(TForm)
    ButtonPanel: TButtonPanel;
    LabelSearch: TLabel;
    ListboxComponents: TListBox;
    PageControl: TPageControl;
    Panel3: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    PatternEdit: TEdit;
    TabSheetInheritance: TTabSheet;
    TabSheetListBox: TTabSheet;
    TabSheetPaletteTree: TTabSheet;
    TreeInheritance: TTreeView;
    TreePallette: TTreeView;
    procedure FormShow(Sender: TObject);
    procedure ListboxComponentsDblClick(Sender: TObject);
    procedure ListboxComponentsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TreeInheritanceDblClick ( Sender: TObject ) ;
    procedure TreeInheritanceKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TreePalletteDblClick(Sender: TObject);
    procedure TreePalletteKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure UpdateComponentSelection(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure PatternEditChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FTimer: TTimer;
    Processing: boolean;
    FComponentList: TFPList;
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
  FTimer := TTimer.Create(nil);
  FTimer.Interval := 500;
  FTimer.Enabled := false;
  FTimer.OnTimer := @Timer1Timer;
  FComponentList := TFPList.Create;

  ButtonPanel.CloseButton.Cancel := True;

  //Translations..
  LabelSearch.Caption := lisMenuFind;
  Caption := lisCmpLstComponents;
  TabSheetListBox.Caption := lisCmpLstList;
  TabSheetPaletteTree.Caption := lisCmpLstPalette;
  TabSheetInheritance.Caption := lisCmpLstInheritance;
  
  //PLEASE add a defaultpage property in TPagecontrol
  PageControl.ActivePage := TabSheetListBox;

  FindAllLazarusComponents;
  UpdateComponentSelection(nil);
  IDEDialogLayoutList.ApplyLayout(Self);
end;

destructor TComponentListForm.Destroy;
begin
  IDEDialogLayoutList.SaveLayout(Self);
  ComponentListForm := nil;
  FComponentList.Free;
  FTimer.Free;
  inherited Destroy;
end;

procedure TComponentListForm.FindAllLazarusComponents;
//Collect all available components (excluding hidden)
var
  AComponent: TRegisteredComponent;
  APage: TBaseComponentPage;
  i, j: Integer;
begin
  if Assigned(IDEComponentPalette)
  then begin
    for i := 0 to IDEComponentPalette.Count-1 do
    begin
      APage := IDEComponentPalette.Pages[i];
      if not APage.Visible then continue;
      for j :=  0 to APage.Count-1 do
      begin
        AComponent := APage.Items[j];
        if not AComponent.Visible then continue;
        if AComponent.PageName='' then continue;
        FComponentList.Add(AComponent);
      end;
    end;
  end;
end;

procedure TComponentListForm.UpdateComponentSelection(Sender: TObject);
//Apply the filter and fill the three tabsheets
var
  AComponent: TRegisteredComponent;
  AFilter, AClassName: string;
  AClassList, List: TStringlist;
  i, j, AIndex: Integer;
  ANode: TTreeNode;
  AClass: TClass;
begin
  if Processing
  then exit;
  Processing := true;
  FTimer.Enabled := false;
  Screen.Cursor := crHourGlass;
  try
    AFilter := UpperCase(PatternEdit.Text);

    //First tabsheet (ListboxComponents)
    ListboxComponents.Items.BeginUpdate;
    try
      ListboxComponents.Items.Clear;
      for i := 0 to FComponentList.Count-1 do
      begin
        AComponent := TRegisteredComponent(FComponentList[i]);
        AClassName := AComponent.ComponentClass.ClassName;
        if (AFilter='') or (Pos(AFilter, UpperCase(AClassName))>0)
        then ListboxComponents.Items.AddObject(AClassName, AComponent);
      end;
    finally
      ListboxComponents.Items.EndUpdate;
    end;

    //Second tabsheet (palette layout)
    TreePallette.BeginUpdate;
    try
      TreePallette.Items.Clear;
      for i := 0 to FComponentList.Count-1 do
      begin
        AComponent := TRegisteredComponent(FComponentList[i]);
        AClassName := AComponent.ComponentClass.ClassName;
        if (AFilter='') or (Pos(AFilter, UpperCase(AClassName))>0)
        then begin
          //find out parent node
          ANode := TreePallette.Items.FindTopLvlNode(AComponent.PageName);
          if ANode = nil
          then ANode := TreePallette.Items.AddChild(nil, AComponent.PageName);
          //add the item
          TreePallette.Items.AddChildObject(ANode, AClassName, AComponent);
        end;
      end;
      TreePallette.FullExpand;
    finally
      TreePallette.EndUpdate;
    end;


    //Third tabsheet (component inheritence)
    List := TStringlist.Create;
    AClassList:= TStringlist.Create;
    TreeInheritance.Items.BeginUpdate;
    try
      TreeInheritance.Items.Clear;
      AClassList.Sorted := true;
      AClassList.CaseSensitive := false;
      AClassList.Duplicates := dupIgnore;

      for i := 0 to FComponentList.Count-1 do
      begin
        AComponent := TRegisteredComponent(FComponentList[i]);
        AClassName := AComponent.ComponentClass.ClassName;
        if (AFilter='') or (Pos(AFilter, UpperCase(AClassName))>0)then
        begin

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
              then ANode := TreeInheritance.Items.AddChild(ANode, AClassName)
              else ANode := TreeInheritance.Items.AddChildObject(ANode, AClassName, AComponent);
              AClassList.AddObject(AClassName, ANode);
            end;
          end;
        end;
      end;


      TreeInheritance.AlphaSort;
      TreeInheritance.FullExpand;
    finally
      List.Free;
      AClassList.Free;
      TreeInheritance.Items.EndUpdate;
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
  if not Assigned(AComponent)
  then Exit;
  if not Assigned(FormEditingHook)
  then Exit;
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

procedure TComponentListForm.ListboxComponentsDblClick(Sender: TObject);
begin
  AddSelectedComponent(TRegisteredComponent(ListboxComponents.Items.Objects[ListboxComponents.ItemIndex]));
end;

procedure TComponentListForm.ListboxComponentsKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    ListboxComponentsDblClick(Sender);
end;

procedure TComponentListForm.TreePalletteDblClick(Sender: TObject);
var
  AComponent: TRegisteredComponent;
begin
  if not Assigned(TreePallette.Selected)
  then exit;
  AComponent := TRegisteredComponent(TreePallette.Selected.Data);
  if not Assigned(AComponent)
  then exit;

  AddSelectedComponent(AComponent);
end;

procedure TComponentListForm.TreePalletteKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    TreePalletteDblClick(Sender);
end;

procedure TComponentListForm.TreeInheritanceDblClick(Sender:TObject);
var
  AComponent: TRegisteredComponent;
begin
  if not Assigned(TreeInheritance.Selected)
  then exit;
  AComponent := TRegisteredComponent(TreeInheritance.Selected.Data);
  if not Assigned(AComponent)
  then exit;
  AddSelectedComponent(AComponent);
end;

procedure TComponentListForm.TreeInheritanceKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    TreeInheritanceDblClick(Sender);
end;

procedure TComponentListForm.FormShow(Sender: TObject);
begin
  if PatternEdit.Canfocus
  then PatternEdit.SetFocus;
end;

procedure TComponentListForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
//Close the form on escape key like every other IDE dialog does
begin
  if (Key=VK_ESCAPE) and (Parent=nil) then
    Close;
end;

procedure TComponentListForm.PatternEditChange(Sender: TObject);
begin
  //Reset for proper delay
  FTimer.Enabled := false;
  FTimer.Enabled := true;
end;

procedure TComponentListForm.Timer1Timer(Sender: TObject);
begin
  UpdateComponentSelection(nil);
end;

end.

