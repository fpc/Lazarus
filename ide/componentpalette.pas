{
 /***************************************************************************
                          componentpalette.pas
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

  Author: Mattias Gaertner, Juha Manninen

  Abstract:
   The implementation of the component palette.
   Supports reordering of pages and components by user settings in environment options.
}
unit ComponentPalette;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, Graphics, ComCtrls, Buttons, Menus, ExtCtrls,
  FileUtil, LazFileCache, AVL_Tree, PropEdits, FormEditingIntf, LazIDEIntf,
  {$IFDEF CustomIDEComps}
  CustomIDEComps,
  {$ENDIF}
  LazarusIDEStrConsts, ComponentReg, DesignerProcs, PackageDefs, EnvironmentOpts;

type
  TComponentSelectionMode = (
    csmSingle, // reset selection on component add
    csmMulty   // don't reset selection on component add
  );

  TComponentPalette = class;

  { TCompPaletteUserOrder }

  // Like TCompPaletteOptions but collects all pages and components,
  //  including the original ones. The palette is later synchronized with this.
  TCompPaletteUserOrder = class(TBaseCompPaletteOptions)
  private
    fPalette: TComponentPalette;
    // Reference to either EnvironmentOptions.ComponentPaletteOptions or a copy of it.
    fOptions: TCompPaletteOptions;
  public
    constructor Create(aPalette: TBaseComponentPalette);
    destructor Destroy; override;
    procedure Clear;
    function SortPagesAndCompsUserOrder: Boolean;
  public
    property Options: TCompPaletteOptions read fOptions write fOptions;
  end;

  { TComponentPalette }

  TComponentPalette = class(TBaseComponentPalette)
    PalettePopupMenu: TPopupMenu;
    PopupMenu: TPopupMenu;
    OpenPackageMenuItem: TMenuItem;
    OpenUnitMenuItem: TMenuItem;
    procedure ActivePageChanged(Sender: TObject);
    procedure OnScrollBoxResize(Sender: TObject);
    procedure OpenPackageClicked(Sender: TObject);
    procedure OpenUnitClicked(Sender: TObject);
    procedure ComponentListClicked(Sender: TObject);
    procedure PalettePopupMenuPopup(Sender: TObject);
    procedure PopupMenuPopup(Sender: TObject);
  private
    // Tree of TRegisteredComponent sorted for componentclass
    fComponentCache: TAVLTree;
    // List of original page names. Object holds another StringList for component names.
    fComponentPageCache: TStringList;
    FPageControl: TPageControl;
    fNoteBookNeedsUpdate: boolean;
    FOnOpenPackage: TNotifyEvent;
    FOnOpenUnit: TNotifyEvent;
    FOnClassSelected: TNotifyEvent;
    FSelected: TRegisteredComponent;
    FSelectionMode: TComponentSelectionMode;
    fUnregisteredIcon: TCustomBitmap;
    fSelectButtonIcon: TCustomBitmap;
    fUpdatingPageControl: boolean;
    // User ordered + original pages and components.
    fUserOrder: TCompPaletteUserOrder;
    procedure CacheOrigComponentPages;
    procedure SetPageControl(const AValue: TPageControl);
    procedure SelectionToolClick(Sender: TObject);
    procedure ComponentBtnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ComponentBtnMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ComponentBtnDblClick(Sender: TObject);
    procedure OnPageMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure CreatePopupMenu;
    procedure UnselectAllButtons;
  protected
    procedure AssignOrigCompsForPage(DestComps: TStringList; PageName: string); override;
    function RefOrigCompsForPage(PageName: string): TStringList; override;
    procedure DoBeginUpdate; override;
    procedure DoEndUpdate(Changed: boolean); override;
    procedure OnPageAddedComponent(Component: TRegisteredComponent); override;
    procedure OnPageRemovedComponent(Page: TBaseComponentPage;
                                     Component: TRegisteredComponent); override;
    procedure Update; override;
    procedure CheckComponentDesignerVisible(AComponent: TComponent;
                                            var Invisible: boolean);
    procedure SetSelected(const AValue: TRegisteredComponent); override;
    function GetSelected: TRegisteredComponent; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure ClearButtons;
    function CreateNewPage(const NewPageName: string;
                           const Priority: TComponentPriority): TBaseComponentPage;
    function CreatePagesFromUserOrder: Boolean;
    procedure DoAfterComponentAdded; override;
    function GetUnregisteredIcon: TCustomBitmap;
    function GetSelectButtonIcon: TCustomBitmap;
    function SelectButton(Button: TComponent): boolean;
    procedure ReAlignButtons(Page: TCustomPage);
    procedure UpdateNoteBookButtons;
    procedure OnGetNonVisualCompIcon(Sender: TObject;
                                     AComponent: TComponent; var Icon: TCustomBitmap);
    function FindComponent(const CompClassName: string): TRegisteredComponent; override;
    procedure RegisterCustomIDEComponents(
                       const RegisterProc: RegisterUnitComponentProc); override;
    procedure UpdateVisible; override;
  public
    property PageControl: TPageControl read FPageControl write SetPageControl;
    property SelectionMode: TComponentSelectionMode read FSelectionMode write FSelectionMode;
    property OnOpenPackage: TNotifyEvent read FOnOpenPackage write FOnOpenPackage;
    property OnOpenUnit: TNotifyEvent read FOnOpenUnit write FOnOpenUnit;
    property OnClassSelected: TNotifyEvent read FOnClassSelected write FOnClassSelected;
    // User ordered + original pages and components.
    property UserOrder: TCompPaletteUserOrder read fUserOrder;
  end;

function CompareControlsWithTag(Control1, Control2: Pointer): integer;

implementation

{$R ../images/components_images.res}

uses 
  MainBase;

const
  OVERVIEW_PANEL_WIDTH = 20;

function CompareRegisteredComponents(Data1, Data2: Pointer): integer;
var
  RegComp1: TRegisteredComponent;
  RegComp2: TRegisteredComponent;
begin
  RegComp1:=TRegisteredComponent(Data1);
  RegComp2:=TRegisteredComponent(Data2);
  Result:=CompareText(RegComp1.ComponentClass.ClassName,
                      RegComp2.ComponentClass.ClassName);
end;

function CompareClassNameWithRegisteredComponent(Key, Data: Pointer): integer;
var
  AClassName: String;
  RegComp: TRegisteredComponent;
begin
  AClassName:=String(Key);
  RegComp:=TRegisteredComponent(Data);
  Result:=CompareText(AClassName,RegComp.ComponentClass.ClassName);
end;

function CompareControlsWithTag(Control1, Control2: Pointer): integer;
var
  Ctrl1: TControl absolute Control1;
  Ctrl2: TControl absolute Control2;
begin
  if Ctrl1.Tag>Ctrl2.Tag then
    Result:=1
  else if Ctrl1.Tag<Ctrl2.Tag then
    Result:=-1
  else
    Result:=0;
end;

{ TCompPaletteUserOrder }

constructor TCompPaletteUserOrder.Create(aPalette: TBaseComponentPalette);
begin
  inherited Create;
  fPalette:=TComponentPalette(aPalette);
end;

destructor TCompPaletteUserOrder.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TCompPaletteUserOrder.Clear;
begin
  inherited Clear;
end;

function TCompPaletteUserOrder.SortPagesAndCompsUserOrder: Boolean;
// Calculate page order using user config and default order. User config takes priority.
// This order will finally be shown in the palette.
var
  DstComps: TStringList;
  PageI, i: Integer;
  PgName: String;
begin
  Result:=True;
  Clear;
  fPalette.CacheOrigComponentPages;
  // First add user defined page order from EnvironmentOptions,
  FComponentPages.Assign(fOptions.PageNames);
  // then add other pages which don't have user configuration
  for PageI := 0 to fPalette.OrigPagePriorities.Count-1 do
  begin
    PgName:=fPalette.OrigPagePriorities.Keys[PageI];
    if (FComponentPages.IndexOf(PgName) = -1)
    and (fOptions.HiddenPageNames.IndexOf(PgName) = -1) then
      FComponentPages.Add(PgName);
  end;
  // Map components with their pages
  for PageI := 0 to FComponentPages.Count-1 do
  begin
    PgName := FComponentPages[PageI];
    DstComps := TStringList.Create;
    FComponentPages.Objects[PageI] := DstComps;
    i := fOptions.ComponentPages.IndexOf(PgName);
    if i >= 0 then                      // Add components reordered by user.
      DstComps.Assign(fOptions.ComponentPages.Objects[i] as TStringList)
    else                                // Add components that were not reordered.
      fPalette.AssignOrigCompsForPage(DstComps, PgName);
  end;
end;

{ TComponentPalette }

procedure TComponentPalette.ActivePageChanged(Sender: TObject);
begin
  if FPageControl=nil then exit;
  ReAlignButtons(FPageControl.ActivePage);
  if (FSelected<>nil)
  and (FSelected.RealPage.PageComponent=FPageControl.ActivePage)
  then exit;
  Selected:=nil;
end;

procedure TComponentPalette.OnScrollBoxResize(Sender: TObject);
begin
  if TControl(Sender).Parent is TCustomPage then
    ReAlignButtons(TCustomPage(TControl(Sender).Parent));
end;

procedure TComponentPalette.OpenPackageClicked(Sender: TObject);
var
  PkgComponent: TPkgComponent;
begin
  PkgComponent:=TPkgComponent(FindButton(PopupMenu.PopupComponent));
  if (PkgComponent=nil) or (PkgComponent.PkgFile=nil)
  or (PkgComponent.PkgFile.LazPackage=nil) then exit;
  if Assigned(OnOpenPackage) then
    OnOpenPackage(PkgComponent.PkgFile.LazPackage);
end;

procedure TComponentPalette.OpenUnitClicked(Sender: TObject);
var
  PkgComponent: TPkgComponent;
begin
  PkgComponent:=TPkgComponent(FindButton(PopupMenu.PopupComponent));
  if (PkgComponent=nil) or (PkgComponent.PkgFile=nil)
  or (PkgComponent.PkgFile.LazPackage=nil) then exit;
  if Assigned(OnOpenUnit) then
    OnOpenUnit(PkgComponent);
end;

procedure TComponentPalette.ComponentListClicked(Sender: TObject);
begin
  MainIDE.DoShowComponentList;
end;

procedure TComponentPalette.PalettePopupMenuPopup(Sender: TObject);
begin
  ;
end;

procedure TComponentPalette.PopupMenuPopup(Sender: TObject);
var
  PkgComponent: TPkgComponent;
  APackage: TLazPackage;
  UnitFilename: String;
  ShownFilename: String;
begin
  PkgComponent:=TPkgComponent(FindButton(PopupMenu.PopupComponent));
  APackage:=nil;
  if (PkgComponent<>nil) and (PkgComponent.PkgFile<>nil) then
    APackage:=PkgComponent.PkgFile.LazPackage;
  if APackage=nil then begin
    OpenPackageMenuItem.Visible:=false;
    OpenUnitMenuItem.Visible:=false;
  end else begin
    OpenPackageMenuItem.Caption:=Format(lisCPOpenPackage, [APackage.IDAsString]);
    OpenPackageMenuItem.Visible:=true;
    ShownFilename:=PkgComponent.PkgFile.Filename;
    UnitFilename:=PkgComponent.PkgFile.GetFullFilename;
    if not FileExistsCached(UnitFilename) then begin
      UnitFilename:=LazarusIDE.FindSourceFile(ExtractFilename(UnitFilename),
                                              APackage.Directory,[]);
      if FileExistsUTF8(UnitFilename) then
        UnitFilename:=ShownFilename;
    end;
    OpenUnitMenuItem.Caption:=Format(lisCPOpenUnit, [ShownFilename]);
    OpenUnitMenuItem.Visible:=true;
    OpenUnitMenuItem.Enabled:=FileExistsCached(UnitFilename);
  end;
end;

procedure TComponentPalette.SetPageControl(const AValue: TPageControl);
var
  MenuItem: TMenuItem;
begin
  if FPageControl=AValue then exit;
  ClearButtons;
  FPageControl:=AValue;
  if FPageControl<>nil then begin
    FPageControl.OnChange:=@ActivePageChanged;
    if PalettePopupMenu=nil then begin
      PalettePopupMenu:=TPopupMenu.Create(nil);
      PalettePopupMenu.OnPopup:=@PalettePopupMenuPopup;
      PalettePopupMenu.Name:='PalettePopupMenu';
      // Component List
      MenuItem:=TMenuItem.Create(PalettePopupMenu);
      with MenuItem do begin
        Name:='ComponentListMenuItem';
        Caption:=lisCompPalComponentList;
        OnClick:=@ComponentListClicked;
      end;
      PalettePopupMenu.Items.Add(MenuItem);
    end;
    FPageControl.PopupMenu:=PalettePopupMenu;
  end;
  UpdateNoteBookButtons;
end;

procedure TComponentPalette.SelectionToolClick(Sender: TObject);
begin
  SelectButton(TComponent(Sender));
end;

procedure TComponentPalette.ComponentBtnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button=mbLeft then
  begin
    if ssShift in Shift then
      SelectionMode := csmMulty
    else
      SelectionMode := csmSingle;
    SelectButton(TComponent(Sender));
    if Assigned(OnClassSelected) then
      OnClassSelected(Self);
  end;
end;

procedure TComponentPalette.ComponentBtnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
   { If the visual state is down, but internal "no selection" then
    just do visual unselection of all buttons
    This trick is for double-click handling (to unselect the button visually ). }
  if ((Sender as TCustomSpeedButton).Down) and (Selected = Nil) then
    UnselectAllButtons;
end;

procedure TComponentPalette.ComponentBtnDblClick(Sender: TObject);
var
  TypeClass: TComponentClass;
  ParentComp: TComponent;
  X, Y: integer;
  AComponent: TComponent;
  DisableAutoSize: Boolean;
begin
  //debugln('TComponentPalette.ComponentBtnDblClick ',TComponent(Sender).Name);
  if SelectButton(TComponent(Sender)) and (FSelected<>nil) then begin
    if FormEditingHook<>nil then begin
      if assigned(FSelected.OnGetCreationClass) then
      begin
        FSelected.OnGetCreationClass(Self,TypeClass);
        if TypeClass=nil then exit;
      end else
        TypeClass:=FSelected.ComponentClass;
      ParentComp:=FormEditingHook.GetDefaultComponentParent(TypeClass);
      if ParentComp=nil then exit;
      if not FormEditingHook.GetDefaultComponentPosition(TypeClass,ParentComp,X,Y)
      then exit;
      //debugln('TComponentPalette.ComponentBtnDblClick ',dbgsName(Sender),' ',dbgs(X),',',dbgs(Y));
      DisableAutoSize:=true;
      AComponent:=FormEditingHook.CreateComponent(ParentComp,TypeClass,'',X,Y,0,0,
        DisableAutoSize);
      if AComponent<>nil then begin
        if DisableAutoSize and (AComponent is TControl) then
          TControl(AComponent).EnableAutoSizing;
        GlobalDesignHook.PersistentAdded(AComponent,true);
      end;
    end;
  end;
  Selected:=nil;
  if Assigned(OnClassSelected) then
    OnClassSelected(Self);
end;

// unselect all other buttons on all other PageControl pages
procedure TComponentPalette.UnselectAllButtons;
var
  i: Integer;
  CurPage: TBaseComponentPage;
  SelectButtonOnPage: TSpeedButton;
begin
  for i:=0 to Pages.Count-1 do begin
    CurPage:=Pages[i];
    if (FSelected=nil) or (FSelected.RealPage<>CurPage) then begin
      SelectButtonOnPage:=TSpeedButton(CurPage.SelectButton);
      if SelectButtonOnPage<>nil then SelectButtonOnPage.Down:=true;
    end;
  end;
end;

procedure TComponentPalette.SetSelected(const AValue: TRegisteredComponent);
begin
  if FSelected=AValue then exit;
  FSelected:=AValue;
  if FSelected<>nil then begin
    if (FSelected.RealPage=nil) or (FSelected.RealPage.Palette<>Self)
    or (not FSelected.Visible)
    or (not FSelected.CanBeCreatedInDesigner) then
      FSelected:=nil;
  end;
  if FPageControl=nil then exit;
  UnselectAllButtons;
  // select button
  if (FSelected<>nil) and (FPageControl<>nil) then begin
    TSpeedButton(FSelected.Button).Down:=true;
    FPageControl.ActivePage:=TTabSheet(FSelected.RealPage.PageComponent);
  end;
end;

function TComponentPalette.GetSelected: TRegisteredComponent;
begin
  Result:=FSelected;
end;

procedure TComponentPalette.CreatePopupMenu;
var
  MenuItem: TMenuItem;
begin
  if PopupMenu<>nil then exit;
  PopupMenu:=TPopupMenu.Create(nil);
  PopupMenu.OnPopup:=@PopupMenuPopup;
  PopupMenu.Name:='ComponentPopupMenu';
  
  OpenPackageMenuItem:=TMenuItem.Create(PopupMenu);
  with OpenPackageMenuItem do begin
    Name:='OpenPackageMenuItem';
    Caption:=lisCompPalOpenPackage;
    OnClick:=@OpenPackageClicked;
  end;
  PopupMenu.Items.Add(OpenPackageMenuItem);

  OpenUnitMenuItem:=TMenuItem.Create(PopupMenu);
  with OpenUnitMenuItem do begin
    Name:='OpenUnitMenuItem';
    Caption:=lisCompPalOpenUnit;
    OnClick:=@OpenUnitClicked;
  end;
  PopupMenu.Items.Add(OpenUnitMenuItem);

  PopupMenu.Items.AddSeparator;

  MenuItem:=TMenuItem.Create(PopupMenu);
  with MenuItem do begin
    Name:='ComponentListMenuItem';
    Caption:=lisCompPalComponentList;
    OnClick:=@ComponentListClicked;
  end;
  PopupMenu.Items.Add(MenuItem);
end;

procedure TComponentPalette.DoBeginUpdate;
begin
  inherited DoBeginUpdate;
end;

procedure TComponentPalette.DoEndUpdate(Changed: boolean);
begin
  if Changed or fNoteBookNeedsUpdate then
    UpdateNoteBookButtons;
  inherited DoEndUpdate(Changed);
end;

procedure TComponentPalette.OnPageMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if (WheelDelta > 0) then
  begin
    if (PageControl.ActivePageIndex > 0) then
      PageControl.ActivePageIndex := PageControl.ActivePageIndex - 1;
  end else begin
    if (PageControl.ActivePageIndex < PageControl.PageCount-1) then
      PageControl.ActivePageIndex := PageControl.ActivePageIndex + 1;
  end;
  Handled := True;
end;

procedure TComponentPalette.OnPageAddedComponent(Component: TRegisteredComponent);
begin
  fComponentCache.Add(Component);
  inherited OnPageAddedComponent(Component);
end;

procedure TComponentPalette.OnPageRemovedComponent(Page: TBaseComponentPage;
  Component: TRegisteredComponent);
begin
  fComponentCache.Remove(Component);
  inherited OnPageRemovedComponent(Page, Component);
end;

procedure TComponentPalette.CacheOrigComponentPages;
var
  sl: TStringList;
  PageI, CompI: Integer;
  PgName: string;
  Comp: TRegisteredComponent;
begin
  if fComponentPageCache.Count > 0 then Exit;  // Cache only once.
  for PageI := 0 to fOrigPagePriorities.Count-1 do
  begin
    PgName:=fOrigPagePriorities.Keys[PageI];
    Assert((PgName <> '') and not fComponentPageCache.Find(PgName, CompI),
                  Format('CacheComponentPages: %s already cached.', [PgName]));
    // Add a cache StringList for this page name.
    sl := TStringList.Create;
    fComponentPageCache.AddObject(PgName, sl);
    // Find all components for this page and add them to cache.
    for CompI := 0 to fComps.Count-1 do begin
      Comp := fComps[CompI];
      if Comp.OrigPageName = PgName then //if SameText(Comp.OrigPageName, PgName) then
        sl.Add(Comp.ComponentClass.ClassName);
    end;
  end;
end;

procedure TComponentPalette.AssignOrigCompsForPage(DestComps: TStringList; PageName: string);
var
  sl: TStringList;
  i: Integer;
begin
  if fComponentPageCache.Find(PageName, i) then begin
    sl := fComponentPageCache.Objects[i] as TStringList;
    DestComps.Assign(sl);
  end
  else
    raise Exception.Create(Format('AssignCompsFromCache: %s not found in cache.', [PageName]));
end;

function TComponentPalette.RefOrigCompsForPage(PageName: string): TStringList;
var
  i: Integer;
begin
  if fComponentPageCache.Find(PageName, i) then
    Result := fComponentPageCache.Objects[i] as TStringList
  else
    Result := Nil;
end;

procedure TComponentPalette.Update;
begin
  inherited Update;
  UpdateNoteBookButtons;
end;

procedure TComponentPalette.CheckComponentDesignerVisible(
  AComponent: TComponent; var Invisible: boolean);
var
  RegComp: TRegisteredComponent;
  AControl: TControl;
begin
  if (AComponent is TControl) then begin
    AControl:=TControl(AComponent);
    Invisible:=(csNoDesignVisible in AControl.ControlStyle)
  end else begin
    RegComp:=FindComponent(AComponent.ClassName);
    Invisible:=(RegComp=nil) or (RegComp.OrigPageName='');
  end;
end;

constructor TComponentPalette.Create;
begin
  inherited Create;
  FSelectionMode:=csmSingle;
  fUserOrder:=TCompPaletteUserOrder.Create(Self);
  fUserOrder.Options:=EnvironmentOptions.ComponentPaletteOptions;
  fComponentCache:=TAVLTree.Create(@CompareRegisteredComponents);
  fComponentPageCache:=TStringList.Create;
  fComponentPageCache.Sorted:=True;
  OnComponentIsInvisible:=@CheckComponentDesignerVisible;
end;

destructor TComponentPalette.Destroy;
var
  i: Integer;
begin
  if OnComponentIsInvisible=@CheckComponentDesignerVisible then
    OnComponentIsInvisible:=nil;
  PageControl:=nil;
  for i := 0 to fComponentPageCache.Count-1 do
    fComponentPageCache.Objects[i].Free;  // Free also the contained StringList.
  FreeAndNil(fComponentPageCache);
  FreeAndNil(fComponentCache);
  FreeAndNil(fUserOrder);
  FreeAndNil(fUnregisteredIcon);
  FreeAndNil(fSelectButtonIcon);
  FreeAndNil(PopupMenu);
  FreeAndNil(PalettePopupMenu);
  inherited Destroy;
end;

procedure TComponentPalette.Clear;
begin
  ClearButtons;
  fUserOrder.Clear;
  inherited Clear;
end;

procedure TComponentPalette.ClearButtons;
begin
  if FPageControl<>nil then
    FPageControl.DisableAlign;
  Selected:=nil;
  if PopupMenu<>nil then begin
    PopupMenu.Free;
    PopupMenu:=nil;
    OpenPackageMenuItem:=nil;
  end;
  if FPageControl<>nil then
    FPageControl.EnableAlign;
end;

function TComponentPalette.CreateNewPage(const NewPageName: string;
  const Priority: TComponentPriority): TBaseComponentPage;
var
  InsertIndex: Integer;
begin
  Result:=TBaseComponentPage.Create(NewPageName);
  Result.Priority:=Priority;
  InsertIndex:=0;
  while (InsertIndex<Pages.Count)
  and (ComparePriority(Priority,Pages[InsertIndex].Priority)<=0) do
    inc(InsertIndex);
  fPages.Insert(InsertIndex,Result);
  Result.Palette:=Self;
  if CompareText(NewPageName,'Hidden')=0 then
    Result.Visible:=false;
end;

function TComponentPalette.CreatePagesFromUserOrder: Boolean;
var
  i, j: Integer;
  PgName: String;
  Pg: TBaseComponentPage;
  CompNames: TStringList;
  Comp: TRegisteredComponent;
begin
  Result := True;
  for i:=0 to fPages.Count-1 do
    fPages[i].Free;
  fPages.Clear;
  for i := 0 to fUserOrder.ComponentPages.Count-1 do
  begin
    PgName := fUserOrder.ComponentPages[i];
    Pg:=CreateNewPage(PgName, ComponentPriorityNormal);
    CompNames := TStringList(fUserOrder.ComponentPages.Objects[i]);
    for j := 0 to CompNames.Count-1 do
    begin
      Comp := FindComponent(CompNames[j]);
      if Assigned(Comp) then
        Comp.RealPage := Pg;
    end;
  end;
end;

procedure TComponentPalette.DoAfterComponentAdded;
begin
  inherited DoAfterComponentAdded;
  if not (ssShift in GetKeyShiftState) and (SelectionMode = csmSingle) then
    Selected := nil;
end;

function TComponentPalette.GetUnregisteredIcon: TCustomBitmap;
begin
  if fUnregisteredIcon = nil then 
  begin
    fUnregisteredIcon := CreateBitmapFromResourceName(hInstance, 'unregisteredcomponent');
    if fUnregisteredIcon = nil then
      fUnregisteredIcon := CreateBitmapFromResourceName(hInstance, 'default');
  end;
  Result := fUnregisteredIcon;
end;

function TComponentPalette.GetSelectButtonIcon: TCustomBitmap;
begin
  if fSelectButtonIcon=nil then 
    fSelectButtonIcon := CreateBitmapFromResourceName(hInstance, 'tmouse');
  Result:=fSelectButtonIcon;
end;

function TComponentPalette.SelectButton(Button: TComponent): boolean;
var
  NewComponent: TRegisteredComponent;
begin
  NewComponent := FindButton(Button);
  Selected := NewComponent;
  Result := (Selected = NewComponent);
end;

procedure TComponentPalette.ReAlignButtons(Page: TCustomPage);
var
  j: integer;
  buttonx: integer;
  CurButton: TSpeedButton;
  MaxBtnPerRow: Integer;
  ButtonTree: TAVLTree;
  Node: TAVLTreeNode;
  ScrollBox: TScrollBox;
  //WSClientRect: TRect;
begin
  //DebugLn(['TComponentPalette.ReAlignButtons ',Page.Caption,' ',Page.ClientWidth]);
  if not Page.Visible then exit;
  if FPageControl<>nil then
    PageControl.DisableAutoSizing{$IFDEF DebugDisableAutoSizing}('TComponentPalette.ReAlignButtons'){$ENDIF};
  ButtonTree:=nil;
  try
    if (Page.ComponentCount=0) or not (Page.Components[0] is TScrollBox) then exit;
    ButtonTree:=TAVLTree.Create(@CompareControlsWithTag);
    ScrollBox:=TScrollBox(Page.Components[0]);
    // skip the first control (this is the selection tool (TSpeedButton))
    for j:= 1 to ScrollBox.ControlCount-1 do begin
      CurButton:=TSpeedbutton(ScrollBox.Controls[j]);
      if (CurButton is TSpeedButton) and CurButton.Visible then
        ButtonTree.Add(CurButton);
    end;
    if ButtonTree.Count=0 then exit;

    ButtonX:= ((ComponentPaletteBtnWidth*3) div 2) + 2;

    {GetClientRect(ScrollBox.Handle,WSClientRect);
    debugln(['TComponentPalette.ReAlignButtons ScrollBox.Bounds=',dbgs(ScrollBox.BoundsRect),
      ' ClientRect=',dbgs(ScrollBox.ClientRect),' WSClientRect=',dbgs(WSClientRect),
      ' VertScrollBar.Size=',ScrollBox.VertScrollBar.Size,
      ' ClientSizeWithoutBar=',ScrollBox.VertScrollBar.ClientSizeWithoutBar,
      ' IsScrollBarVisible=',ScrollBox.VertScrollBar.IsScrollBarVisible,
      ' HorzScrollBar.Size=',ScrollBox.HorzScrollBar.Size,
      ' Page=',ScrollBox.HorzScrollBar.Page,
      ' Range=',ScrollBox.HorzScrollBar.Range,
      ' IsScrollBarVisible=',ScrollBox.HorzScrollBar.IsScrollBarVisible
      ]);}
    {$IFDEF LCLCarbon}
    MaxBtnPerRow:=ButtonTree.Count;
    {$ELSE}
    MaxBtnPerRow:=((ScrollBox.VertScrollBar.ClientSizeWithoutBar - ButtonX) div ComponentPaletteBtnWidth);
    {$ENDIF}
    // If we need to wrap, make sure we have space for the scrollbar
    if MaxBtnPerRow < ButtonTree.Count then
      MaxBtnPerRow:=((ScrollBox.VertScrollBar.ClientSizeWithBar - ButtonX) div ComponentPaletteBtnWidth);
    //debugln(['TComponentPalette.ReAlignButtons MaxBtnPerRow=',MaxBtnPerRow,' ButtonTree.Count=',ButtonTree.Count,' ',ButtonX + MaxBtnPerRow * ComponentPaletteBtnWidth]);
    if MaxBtnPerRow<1 then MaxBtnPerRow:=1;

    j:=0;
    Node:=ButtonTree.FindLowest;
    while Node<>nil do begin
      CurButton:=TSpeedbutton(Node.Data);
      CurButton.SetBounds(ButtonX + (j mod MaxBtnPerRow) * ComponentPaletteBtnWidth,
                          (j div MaxBtnPerRow) * ComponentPaletteBtnHeight,
                          CurButton.Width, CurButton.Height);
      //DebugLn(['TComponentPalette.ReAlignButtons ',CurButton.Name,' ',dbgs(CurButton.BoundsRect)]);
      inc(j);
      Node:=ButtonTree.FindSuccessor(Node);
    end;
  finally
    if PageControl<>nil then
      PageControl.EnableAutoSizing{$IFDEF DebugDisableAutoSizing}('TComponentPalette.ReAlignButtons'){$ENDIF};
    FreeAndNil(ButtonTree);
  end;
end;

procedure TComponentPalette.UpdateNoteBookButtons;
var
  OldActivePage: TTabSheet;

  procedure RemoveUnneededPage(aSheet: TCustomPage);
  var
    PageInd: Integer;
    Pg: TBaseComponentPage;
    Btn: TSpeedButton;
  begin
    PageInd:=IndexOfPageComponent(aSheet);
    if (PageInd<0) or (not Pages[PageInd].Visible) then begin
      // page is not needed anymore => delete
      if PageInd>=0 then begin
        Pg:=Pages[PageInd];
        Btn:=TSpeedButton(Pg.SelectButton);
        if Btn<>nil then begin
          Pg.SelectButton:=nil;
          Application.ReleaseComponent(Btn);
        end;
        Pages[PageInd].PageComponent:=nil;
      end;
      if aSheet=OldActivePage then
        OldActivePage:=nil;
      aSheet.Visible:=false;
      Application.ReleaseComponent(aSheet);
    end;
  end;

  procedure InsertVisiblePage(aCompPage: TBaseComponentPage; var aVisPageIndex: integer);
  var
    PageInd: Integer;
    PanelRight: TPanel;
    BtnRight: TSpeedButton;
    TabControl: TCustomTabControl;
  begin
    if not aCompPage.Visible then Exit;
    TabControl := TCustomTabControl(FPageControl);
    if aCompPage.PageComponent=nil then begin
      // insert a new PageControl page
      TabControl.Pages.Insert(aVisPageIndex, aCompPage.PageName);
      aCompPage.PageComponent := FPageControl.Page[aVisPageIndex];
      with TScrollBox.Create(aCompPage.PageComponent) do begin
        Align := alClient;
        BorderStyle := bsNone;
        BorderWidth := 0;
        HorzScrollBar.Visible := false;
        {$IFDEF LCLCarbon}
        // carbon has not implemented turning scrollbars on and off
        VertScrollBar.Visible := false;
        AutoScroll:=false;
        {$ENDIF}
        VertScrollBar.Increment := ComponentPaletteBtnHeight;
        Parent := aCompPage.PageComponent;
      end;
      PanelRight := TPanel.Create(aCompPage.PageComponent);
      with PanelRight do
      begin
        Align := alRight;
        Caption := '';
        BevelOuter := bvNone;
        Width := OVERVIEW_PANEL_WIDTH;
        Visible := True; // EnvironmentOptions.IDESpeedButtonsVisible;
        Parent := aCompPage.PageComponent;
        OnMouseWheel := @OnPageMouseWheel;
      end;
      BtnRight:=TSpeedButton.Create(aCompPage.PageComponent);
      with BtnRight do
      begin
        LoadGlyphFromResourceName(HInstance, 'SelCompPage');
        Flat := True;
        SetBounds(2,1,16,16);
        Hint := 'Click to Select Palette Page';
        OnClick := @MainIDE.SelComponentPageButtonClick;
        OnMouseWheel := @OnPageMouseWheel;
        Parent := PanelRight;
      end;
      inc(aVisPageIndex);
    end else begin
      // move to the right position
      PageInd := aCompPage.PageComponent.PageIndex;
      if (PageInd<>aVisPageIndex) and (aVisPageIndex < TabControl.Pages.Count) then
        TabControl.Pages.Move(PageInd, aVisPageIndex);
    end;
  end;

  procedure CreateSelectionButton(aCompPage: TBaseComponentPage; aButtonUniqueName: string;
    aScrollBox: TScrollBox);
  var
    Btn: TSpeedButton;
  begin
    if Assigned(aCompPage.SelectButton) then Exit;
    Btn := TSpeedButton.Create(nil);
    aCompPage.SelectButton:=Btn;
    with Btn do begin
      Name := 'PaletteSelectBtn' + aButtonUniqueName;
      OnClick := @SelectionToolClick;
      OnMouseWheel := @OnPageMouseWheel;
      LoadGlyphFromResourceName(hInstance, 'tmouse');
      Flat := True;
      GroupIndex:= 1;
      Down := True;
      Hint := lisSelectionTool;
      ShowHint := EnvironmentOptions.ShowHintsForComponentPalette;
      SetBounds(0,0,ComponentPaletteBtnWidth,ComponentPaletteBtnHeight);
      Parent := aScrollBox;
    end;
  end;

  procedure CreateOrDelButton(aComp: TPkgComponent; aButtonUniqueName: string;
    aScrollBox: TScrollBox; var aBtnIndex: integer);
  var
    Btn: TSpeedButton;
  begin
    if aComp.Visible then begin
      inc(aBtnIndex);
      //DebugLn(['TComponentPalette.UpdateNoteBookButtons Component ',DbgSName(aComp.ComponentClass),' ',aComp.Visible,' Prio=',dbgs(aComp.GetPriority)]);
      if aComp.Button=nil then begin
        Btn := TSpeedButton.Create(nil);
        aComp.Button:=Btn;
        CreatePopupMenu;
        with Btn do begin
          Name := 'PaletteBtnPage' + aButtonUniqueName + aComp.ComponentClass.ClassName;
          // Left and Top will be set in ReAlignButtons.
          SetBounds(Left,Top,ComponentPaletteBtnWidth,ComponentPaletteBtnHeight);
          Glyph.Assign(aComp.Icon);
          GroupIndex := 1;
          Flat := true;
          OnMouseDown := @ComponentBtnMouseDown;
          OnMouseUp := @ComponentBtnMouseUp;
          OnDblClick := @ComponentBtnDblClick;
          OnMouseWheel := @OnPageMouseWheel;
          ShowHint := EnvironmentOptions.ShowHintsForComponentPalette;
          Hint := aComp.ComponentClass.ClassName + sLineBreak
            + '(' + aComp.ComponentClass.UnitName + ')';
          Btn.PopupMenu:=Self.PopupMenu;
        end;
        //debugln(['TComponentPalette.UpdateNoteBookButtons Created Button: ',aComp.ComponentClass.ClassName,' ',aComp.Button.Name]);
      end else begin
        Btn:=TSpeedButton(aComp.Button);
        //DebugLn(['TComponentPalette.UpdateNoteBookButtons Keep Button: ',aComp.ComponentClass.ClassName,' ',aComp.Button.Name,' ',DbgSName(TControl(aComp.Button).Parent)]);
      end;
      Btn.Parent := aScrollBox;
      Btn.Tag:=aBtnIndex;
    end
    else if aComp.Button<>nil then begin
      //debugln(['TComponentPalette.UpdateNoteBookButtons Destroy Button: ',aComp.ComponentClass.ClassName,' ',aComp.Button.Name]);
      Application.ReleaseComponent(aComp.Button);
      aComp.Button:=nil;
    end;
  end;

  procedure CreateButtons(aPageIndex: integer; aCompNames: TStringList);
  // Create speedbuttons for every visible component
  var
    i, BtnIndex: Integer;
    ScrollBox: TScrollBox;
    Pg: TBaseComponentPage;
    Comp: TPkgComponent;
  begin
    Pg := Pages[aPageIndex];
    if not Pg.Visible then Exit;
    ScrollBox := Pg.GetScrollBox;
    Assert(Assigned(ScrollBox), 'CreateButtons: ScrollBox not assigned.');
    ScrollBox.OnResize := @OnScrollBoxResize;
    ScrollBox.OnMouseWheel := @OnPageMouseWheel;
    //DebugLn(['TComponentPalette.UpdateNoteBookButtons PAGE=',Pg.PageName,' PageIndex=',Pg.PageComponent.PageIndex]);
    // create selection button
    CreateSelectionButton(Pg, IntToStr(aPageIndex), ScrollBox);
    // create component buttons and delete unneeded ones
    BtnIndex := 0;
    for i := 0 to aCompNames.Count-1 do begin
      Comp := FindComponent(aCompNames[i]) as TPkgComponent;
      CreateOrDelButton(Comp, Format('%d_%d_',[aPageIndex,i]), ScrollBox, BtnIndex);
    end;
    ReAlignButtons(Pg.PageComponent);
  end;

var
  i, PgInd, VisPageIndex: Integer;
  PgName: String;
begin
  if fUpdatingPageControl then exit;
  if IsUpdateLocked then begin
    fNoteBookNeedsUpdate:=true;
    exit;
  end;
  if FPageControl=nil then begin
    fNoteBookNeedsUpdate:=false;
    exit;
  end;
  // lock
  fUpdatingPageControl:=true;
  FPageControl.DisableAlign;
  try
    OldActivePage:=FPageControl.ActivePage;
    fUserOrder.SortPagesAndCompsUserOrder;
    CreatePagesFromUserOrder;
    // remove every page in the PageControl without a visible page
    for i:=FPageControl.PageCount-1 downto 0 do
      RemoveUnneededPage(FPageControl.Pages[i]);
    // insert a PageControl page for every visible palette page
    VisPageIndex := 0;
    for i := 0 to fUserOrder.ComponentPages.Count-1 do
    begin
      PgName := fUserOrder.ComponentPages[i];
      PgInd := IndexOfPageName(PgName);
      if PgInd >= 0 then
      begin
        InsertVisiblePage(Pages[PgInd], VisPageIndex);
        CreateButtons(PgInd, fUserOrder.ComponentPages.Objects[i] as TStringList);
      end;
    end;

    // restore active page
    if (OldActivePage<>nil) and (FPageControl.IndexOf(OldActivePage) >= 0) then
      FPageControl.ActivePage:=OldActivePage
    else if FPageControl.PageCount>0 then
      FPageControl.PageIndex:=0;
  finally
    // unlock
    fUpdatingPageControl:=false;
    fNoteBookNeedsUpdate:=false;
    FPageControl.EnableAlign;
  end;
end;

procedure TComponentPalette.OnGetNonVisualCompIcon(Sender: TObject;
  AComponent: TComponent; var Icon: TCustomBitmap);
var
  ARegComp: TRegisteredComponent;
begin
  if AComponent<>nil then
    ARegComp:=FindComponent(AComponent.ClassName)
  else
    ARegComp:=nil;
  if ARegComp<>nil then
    Icon:=TPkgComponent(ARegComp).Icon
  else
    Icon:=GetUnregisteredIcon;
end;

function TComponentPalette.FindComponent(const CompClassName: string): TRegisteredComponent;
var
  ANode: TAVLTreeNode;
begin
  ANode:=fComponentCache.FindKey(Pointer(CompClassName),
                             @CompareClassNameWithRegisteredComponent);
  if ANode<>nil then
    Result:=TRegisteredComponent(ANode.Data)
  else
    Result:=nil;
end;

procedure TComponentPalette.RegisterCustomIDEComponents(
  const RegisterProc: RegisterUnitComponentProc);
begin
  //inherited RegisterCustomIDEComponents(RegisterProc);
  {$IFDEF CustomIDEComps}
  CustomIDEComps.RegisterCustomComponents(RegisterProc);
  {$ENDIF}
end;

procedure TComponentPalette.UpdateVisible;
begin
  PageControl.DisableAutoSizing{$IFDEF DebugDisableAutoSizing}('TComponentPalette.ShowHideControls'){$ENDIF};
  try
    inherited UpdateVisible;
  finally
    PageControl.EnableAutoSizing{$IFDEF DebugDisableAutoSizing}('TComponentPalette.ShowHideControls'){$ENDIF};
  end;
end;

end.

