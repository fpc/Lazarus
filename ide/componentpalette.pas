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
  FileUtil, LazFileCache, AVL_Tree, PropEdits, LCLProc, FormEditingIntf, LazIDEIntf,
  {$IFDEF CustomIDEComps}
  CustomIDEComps,
  {$ENDIF}
  LazarusIDEStrConsts, ComponentReg, DesignerProcs, PackageDefs, EnvironmentOpts;

const
  CompPalSelectionToolBtnPrefix = 'PaletteSelectBtn';
  CompPaletteCompBtnPrefix = 'PaletteBtn';
  {$IFDEF VerboseComponentPalette}
  CompPalVerbPgName = 'Standard';
  {$ENDIF}
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
    // Component cache, a tree of TRegisteredComponent sorted for componentclass
    fComponentCache: TAVLTree;
    // Two page caches, one for original pages, one for user ordered pages.
    // Lists have page names. Object holds another StringList for component names.
    fOrigComponentPageCache: TStringList;  // Original
    fUserComponentPageCache: TStringList;  // User ordered
    //
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
    // Used by UpdateNoteBookButtons
    fOldActivePage: TTabSheet;
    fVisiblePageIndex: integer;
    fBtnIndex: integer;
    // User ordered + original pages and components
    fUserOrder: TCompPaletteUserOrder;
    //procedure AssociatePageComps(aPageInd: Integer; aCompNames: TStringList);
    function CreatePagesFromUserOrder: Boolean;
    procedure CacheOrigComponentPages;
    procedure CreateButtons(aPageIndex: integer; aCompNames: TStringList);
    procedure CreateOrDelButton(aComp: TPkgComponent; aButtonUniqueName: string;
      aScrollBox: TScrollBox);
    procedure CreateSelectionButton(aCompPage: TBaseComponentPage;
      aButtonUniqueName: string; aScrollBox: TScrollBox);
    procedure InsertVisiblePage(aCompPage: TBaseComponentPage);
    procedure RemoveUnneededPage(aSheet: TCustomPage);
    procedure ReAlignButtons(aSheet: TCustomPage);
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
    function GetUnregisteredIcon: TCustomBitmap;
    function GetSelectButtonIcon: TCustomBitmap;
    function SelectAButton(Button: TComponent): boolean;
    function IsSelectionToolBtn(aControl: TControl): boolean;
  protected
    procedure DoBeginUpdate; override;
    procedure DoEndUpdate(Changed: boolean); override;
    procedure OnPageAddedComponent(Component: TRegisteredComponent); override;
    procedure OnPageRemovedComponent(Page: TBaseComponentPage;
                                     Component: TRegisteredComponent); override;
    procedure CheckComponentDesignerVisible(AComponent: TComponent;
                                            var Invisible: boolean);
    procedure SetSelected(const AValue: TRegisteredComponent); override;
    function GetSelected: TRegisteredComponent; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure ClearButtons;
    procedure DoAfterComponentAdded; override;
    procedure UpdateNoteBookButtons;
    procedure OnGetNonVisualCompIcon(Sender: TObject;
                                     AComponent: TComponent; var Icon: TCustomBitmap);
    function FindComponent(const CompClassName: string): TRegisteredComponent; override;
    procedure RegisterCustomIDEComponents(
                       const RegisterProc: RegisterUnitComponentProc); override;
    procedure Update; override;
    procedure AssignOrigCompsForPage(DestComps: TStringList; PageName: string); override;
    procedure AssignOrigVisibleCompsForPage(DestComps: TStringList; PageName: string); override;
    function RefUserCompsForPage(PageName: string): TStringList; override;
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
  if (FSelected<>nil)
  and (FSelected.RealPage.PageComponent=FPageControl.ActivePage) then
    exit;
  DebugLn('TComponentPalette.ActivePageChanged: Calling ReAlignButtons, setting Selected:=nil.');
  ReAlignButtons(FPageControl.ActivePage);
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
  SelectAButton(TComponent(Sender));
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
    SelectAButton(TComponent(Sender));
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
  if SelectAButton(TComponent(Sender)) and (FSelected<>nil) then begin
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
      if SelectButtonOnPage<>nil then
        SelectButtonOnPage.Down:=true;
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
    Assert(Assigned(FSelected.RealPage), 'TComponentPalette.SetSelected: FSelected.RealPage = Nil.');
    {$IFDEF VerboseComponentPalette}
    DebugLn(['TComponentPalette.SetSelected: Setting FPageControl.ActivePage=',
      FSelected.RealPage.PageComponent, ', Index ', FSelected.RealPage.PageComponent.PageIndex]);
    {$ENDIF}
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
  if fOrigComponentPageCache.Count > 0 then Exit;  // Fill cache only once.
  for PageI := 0 to fOrigPagePriorities.Count-1 do
  begin
    PgName:=fOrigPagePriorities.Keys[PageI];
    Assert((PgName <> '') and not fOrigComponentPageCache.Find(PgName, CompI),
                  Format('CacheComponentPages: %s already cached.', [PgName]));
    // Add a cache StringList for this page name.
    sl := TStringList.Create;
    fOrigComponentPageCache.AddObject(PgName, sl);
    // Find all components for this page and add them to cache.
    for CompI := 0 to fComps.Count-1 do begin
      Comp := fComps[CompI];
      if Comp.OrigPageName = PgName then //if SameText(Comp.OrigPageName, PgName) then
        sl.AddObject(Comp.ComponentClass.ClassName, Comp);
    end;
  end;
end;

procedure TComponentPalette.AssignOrigCompsForPage(DestComps: TStringList; PageName: string);
var
  sl: TStringList;
  i: Integer;
begin
  if fOrigComponentPageCache.Find(PageName, i) then begin
    sl := fOrigComponentPageCache.Objects[i] as TStringList;
    DestComps.Assign(sl);
  end
  else
    raise Exception.Create(Format('AssignOrigCompsForPage: %s not found in cache.', [PageName]));
end;

procedure TComponentPalette.AssignOrigVisibleCompsForPage(DestComps: TStringList;
  PageName: string);
var
  sl: TStringList;
  i: Integer;
begin
  if fOrigComponentPageCache.Find(PageName, i) then
  begin
    sl := fOrigComponentPageCache.Objects[i] as TStringList;
    for i := 0 to sl.Count-1 do
      if FindComponent(sl[i]).Visible then
        DestComps.Add(sl[i]);
  end
  else
    raise Exception.Create(Format('AssignOrigVisibleCompsForPage: %s not found in cache.', [PageName]));
end;

function TComponentPalette.RefUserCompsForPage(PageName: string): TStringList;
var
  i: Integer;
begin
  if fUserComponentPageCache.Find(PageName, i) then
    Result := fUserComponentPageCache.Objects[i] as TStringList
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
  fOrigComponentPageCache:=TStringList.Create;
  fOrigComponentPageCache.OwnsObjects:=True;
  fOrigComponentPageCache.Sorted:=True;
  fUserComponentPageCache:=TStringList.Create;
  fUserComponentPageCache.OwnsObjects:=True;
  fUserComponentPageCache.Sorted:=True;
  OnComponentIsInvisible:=@CheckComponentDesignerVisible;
end;

destructor TComponentPalette.Destroy;
begin
  if OnComponentIsInvisible=@CheckComponentDesignerVisible then
    OnComponentIsInvisible:=nil;
  PageControl:=nil;
  FreeAndNil(fUserComponentPageCache);
  FreeAndNil(fOrigComponentPageCache);
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

function TComponentPalette.CreatePagesFromUserOrder: Boolean;
var
  UserPageI, CurPgInd, CompI: Integer;
  aVisibleCompCnt: integer;
  PgName: String;
  Pg: TBaseComponentPage;
  CompNames, UserComps: TStringList;
  Comp: TRegisteredComponent;
begin
  Result := True;
  fUserComponentPageCache.Clear;
  {$IFDEF VerboseComponentPalette}
  debugln(['TComponentPalette.CreatePagesFromUserOrder HideControls=',HideControls]);
  {$ENDIF}
  for UserPageI := 0 to fUserOrder.ComponentPages.Count-1 do
  begin
    PgName := fUserOrder.ComponentPages[UserPageI];
    CurPgInd := IndexOfPageName(PgName);
    if CurPgInd = -1 then begin
      // Create a new page
      Pg := TBaseComponentPage.Create(PgName);
      fPages.Insert(UserPageI, Pg);
      Pg.Palette := Self;
    end
    else if CurPgInd <> UserPageI then
      Pages.Move(CurPgInd, UserPageI); // Move page to right place.
    Pg := Pages[UserPageI];
    Assert(PgName = Pg.PageName,
      Format('TComponentPalette.CreatePagesFromUserOrder: Page names differ, "%s" and "%s".',
             [PgName, Pg.PageName]));
    // New cache page
    UserComps := TStringList.Create;
    fUserComponentPageCache.AddObject(PgName, UserComps);
    // Associate components belonging to this page
    aVisibleCompCnt := 0;
    CompNames := TStringList(fUserOrder.ComponentPages.Objects[UserPageI]);
    for CompI := 0 to CompNames.Count-1 do
    begin
      Comp := FindComponent(CompNames[CompI]);
      Assert(Assigned(Comp),
        Format('TComponentPalette.CreatePagesFromUserOrder: Comp %s not found.',[CompNames[CompI]]));
      Comp.RealPage := Pg;
      UserComps.AddObject(CompNames[CompI], Comp);
      if VoteCompVisibility(Comp) then
        inc(aVisibleCompCnt);
    end;
    {$IFDEF VerboseComponentPalette}
    if PgName=CompPalVerbPgName then
      debugln(['TComponentPalette.CreatePagesFromUserOrder HideControl=',HideControls,' aVisibleCompCnt=',aVisibleCompCnt]);
    {$ENDIF}
    Pg.Visible := (CompareText(PgName,'Hidden')<>0) and (aVisibleCompCnt>0);
  end;
  // Remove left-over pages.
  while fPages.Count > fUserOrder.ComponentPages.Count do begin
    Pg := fPages[fPages.Count-1];
    {$IFDEF VerboseComponentPalette}
    DebugLn(['TComponentPalette.CreatePagesFromUserOrder: Deleting left-over page=',
             Pg.PageName, ', Index=', fPages.Count-1]);
    {$ENDIF}
    fPages.Delete(fPages.Count-1);
    Pg.Free;
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

function TComponentPalette.SelectAButton(Button: TComponent): boolean;
var
  NewComponent: TRegisteredComponent;
begin
  NewComponent := FindButton(Button);
  Selected := NewComponent;
  Result := (Selected = NewComponent);
end;

procedure TComponentPalette.ReAlignButtons(aSheet: TCustomPage);
var
  j: integer;
  buttonx: integer;
  CurButton: TSpeedButton;
  MaxBtnPerRow: Integer;
  ButtonTree: TAVLTree;
  Node: TAVLTreeNode;
  ScrollBox: TScrollBox;
begin
  {$IFDEF VerboseComponentPalette}
  if aSheet.Caption = CompPalVerbPgName then
    DebugLn(['TComponentPalette.ReAlignButtons START ',aSheet.Caption,' Visible=',aSheet.Visible,' ClientWidth=',aSheet.ClientWidth]);
  {$ENDIF}
  if not aSheet.Visible then exit;
  if FPageControl<>nil then
    PageControl.DisableAutoSizing{$IFDEF DebugDisableAutoSizing}('TComponentPalette.ReAlignButtons'){$ENDIF};
  ButtonTree:=nil;
  try
    if (aSheet.ComponentCount=0) or not (aSheet.Components[0] is TScrollBox) then
      exit;
    ScrollBox:=TScrollBox(aSheet.Components[0]);
    ButtonTree:=TAVLTree.Create(@CompareControlsWithTag);
    for j:=0 to ScrollBox.ControlCount-1 do begin
      CurButton:=TSpeedbutton(ScrollBox.Controls[j]);
      if IsSelectionToolBtn(CurButton) then continue;
      if (CurButton is TSpeedButton) and CurButton.Visible then
        ButtonTree.Add(CurButton);
    end;
    if ButtonTree.Count=0 then exit;

    ButtonX:= ((ComponentPaletteBtnWidth*3) div 2) + 2;

    {$IFDEF VerboseComponentPalette}
    if aSheet.Caption = CompPalVerbPgName then
      DebugLn(['TComponentPalette.ReAlignButtons',
        ' ButtonTree.Count=',ButtonTree.Count,
        ' ScrollBox.ControlCount=',ScrollBox.ControlCount,
        ' ScrollBox.Bounds=',dbgs(ScrollBox.BoundsRect),
        ' VertScrollBar.Size=',ScrollBox.VertScrollBar.Size,
        ' ClientSizeWithoutBar=',ScrollBox.VertScrollBar.ClientSizeWithoutBar,
        ' IsScrollBarVisible=',ScrollBox.VertScrollBar.IsScrollBarVisible,
        ' HorzScrollBar.Size=',ScrollBox.HorzScrollBar.Size,
        ' Page=',ScrollBox.HorzScrollBar.Page,
        ' Range=',ScrollBox.HorzScrollBar.Range,
        ' IsScrollBarVisible=',ScrollBox.HorzScrollBar.IsScrollBarVisible
        ]);
    {$ENDIF}
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
      {$IFDEF VerboseComponentPalette}
      if aSheet.Caption = CompPalVerbPgName then
        DebugLn(['TComponentPalette.ReAlignButtons ',CurButton.Name,' ',dbgs(CurButton.BoundsRect)]);
      {$ENDIF}
      inc(j);
      Node:=ButtonTree.FindSuccessor(Node);
    end;
    aSheet.Invalidate;
  finally
    if PageControl<>nil then
      PageControl.EnableAutoSizing{$IFDEF DebugDisableAutoSizing}('TComponentPalette.ReAlignButtons'){$ENDIF};
    FreeAndNil(ButtonTree);
  end;
end;

function TComponentPalette.IsSelectionToolBtn(aControl: TControl): boolean;
begin
  Result:=(aControl is TSpeedButton)
    and (LeftStr(aControl.Name,length(CompPalSelectionToolBtnPrefix))=CompPalSelectionToolBtnPrefix);
end;

// Methods used by UpdateNoteBookButtons:

procedure TComponentPalette.RemoveUnneededPage(aSheet: TCustomPage);
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
        Btn.Visible:=false;
      end;
      Pages[PageInd].PageComponent:=nil;
    end;
    if aSheet=fOldActivePage then
      fOldActivePage:=nil;
    aSheet.Visible:=false;
    {$IFDEF VerboseComponentPalette}
    if aSheet.Caption = CompPalVerbPgName then
      DebugLn(['TComponentPalette.RemoveUnneededPage: Removing Page=', aSheet.Caption, ', index=', PageInd]);
    {$ENDIF}
    Application.ReleaseComponent(aSheet);
  end;
end;

procedure TComponentPalette.InsertVisiblePage(aCompPage: TBaseComponentPage);
var
  TabIndex: Integer;
  PanelRight: TPanel;
  BtnRight: TSpeedButton;
  TabControl: TCustomTabControl;
begin
  if not aCompPage.Visible then Exit;
  TabControl := TCustomTabControl(FPageControl);
  if aCompPage.PageComponent=nil then
  begin
    // insert a new PageControl page
    {$IFDEF VerboseComponentPalette}
    if aCompPage.PageName = CompPalVerbPgName then
      DebugLn(['TComponentPalette.InsertVisiblePage: Inserting Page=', aCompPage.PageName, ', at index=', fVisiblePageIndex]);
    {$ENDIF}
    TabControl.Pages.Insert(fVisiblePageIndex, aCompPage.PageName);
    aCompPage.PageComponent := FPageControl.Page[fVisiblePageIndex];
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
  end
  else begin
    // move to the right position
    TabIndex := aCompPage.PageComponent.PageIndex;
    if (TabIndex<>fVisiblePageIndex) and (fVisiblePageIndex < TabControl.Pages.Count) then
    begin
      {$IFDEF VerboseComponentPalette}
      if aCompPage.PageName = CompPalVerbPgName then
        DebugLn(['TComponentPalette.InsertVisiblePage: Moving Page=', aCompPage.PageName, ' from ', TabIndex, ' to ', fVisiblePageIndex]);
      {$ENDIF}
      TabControl.Pages.Move(TabIndex, fVisiblePageIndex);
    end;
  end;
  inc(fVisiblePageIndex);
end;

procedure TComponentPalette.CreateSelectionButton(aCompPage: TBaseComponentPage;
  aButtonUniqueName: string; aScrollBox: TScrollBox);
var
  Btn: TSpeedButton;
begin
  if Assigned(aCompPage.SelectButton) then Exit;
  Btn := TSpeedButton.Create(nil);
  aCompPage.SelectButton:=Btn;
  with Btn do begin
    Name := CompPalSelectionToolBtnPrefix + aButtonUniqueName;
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

procedure TComponentPalette.CreateOrDelButton(aComp: TPkgComponent; aButtonUniqueName: string;
  aScrollBox: TScrollBox);
var
  Btn: TSpeedButton;
begin
  if aComp.Visible then begin
    inc(fBtnIndex);
    {$IFDEF VerboseComponentPalette}
    //DebugLn(['TComponentPalette.CreateOrDelButton Component ',DbgSName(aComp.ComponentClass), ' ',aComp.Visible,' Prio=',dbgs(aComp.GetPriority)]);
    {$ENDIF}
    if aComp.Button=nil then begin
      Btn := TSpeedButton.Create(nil);
      aComp.Button:=Btn;
      CreatePopupMenu;
      with Btn do begin
        Name := CompPaletteCompBtnPrefix + aButtonUniqueName + aComp.ComponentClass.ClassName;
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
      {$IFDEF VerboseComponentPalette}
      if aComp.RealPage.PageName = CompPalVerbPgName then
        DebugLn(['TComponentPalette.CreateOrDelButton Created Button: ',aComp.ComponentClass.ClassName,' ',aComp.Button.Name]);
      {$ENDIF}
    end else begin
      Btn:=TSpeedButton(aComp.Button);
      {$IFDEF VerboseComponentPalette}
      if aComp.RealPage.PageName = CompPalVerbPgName then
        DebugLn(['TComponentPalette.CreateOrDelButton Keep Button: ',aComp.ComponentClass.ClassName,' ',aComp.Button.Name,' ',DbgSName(TControl(aComp.Button).Parent)]);
      {$ENDIF}
    end;
    Btn.Parent := aScrollBox;
    Btn.Tag:=fBtnIndex;
  end
  else if aComp.Button<>nil then begin
    {$IFDEF VerboseComponentPalette}
    if aComp.RealPage.PageName = CompPalVerbPgName then
      DebugLn(['TComponentPalette.CreateOrDelButton Destroy Button: ',aComp.ComponentClass.ClassName,' ',aComp.Button.Name]);
    {$ENDIF}
    Btn:=TSpeedButton(aComp.Button);
    Application.ReleaseComponent(Btn);
    aComp.Button:=nil;
    Btn.Visible:=false;
  end;
end;

procedure TComponentPalette.CreateButtons(aPageIndex: integer; aCompNames: TStringList);
// Create speedbuttons for every visible component
var
  i: Integer;
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
  {$IFDEF VerboseComponentPalette}
  if Pg.PageName = CompPalVerbPgName then
    DebugLn(['TComponentPalette.CreateButtons PAGE=',Pg.PageName,' PageIndex=',Pg.PageComponent.PageIndex]);
  {$ENDIF}
  // create selection button
  CreateSelectionButton(Pg, IntToStr(aPageIndex), ScrollBox);
  // create component buttons and delete unneeded ones
  fBtnIndex := 0;
  for i := 0 to aCompNames.Count-1 do begin
    Comp := FindComponent(aCompNames[i]) as TPkgComponent;
    CreateOrDelButton(Comp, Format('%d_%d_',[aPageIndex,i]), ScrollBox);
  end;
  ReAlignButtons(Pg.PageComponent);
end;

procedure TComponentPalette.UpdateNoteBookButtons;
var
  i: Integer;
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
  //FPageControl.DisableAutoSizing;
  try
    fOldActivePage:=FPageControl.ActivePage;
    fUserOrder.SortPagesAndCompsUserOrder;
    CreatePagesFromUserOrder;

    // remove every page in the PageControl without a visible page
    for i:=FPageControl.PageCount-1 downto 0 do
      RemoveUnneededPage(FPageControl.Pages[i]);
    Application.ProcessMessages; // PageIndex of tabs are not updated without this.
    // insert a PageControl page for every visible palette page
    fVisiblePageIndex := 0;
    for i := 0 to fPages.Count-1 do
    begin
      // fPages and fUserOrder.ComponentPages are now synchronized, same index applies.
      Assert(fPages[i].PageName = fUserOrder.ComponentPages[i],
             'UpdateNoteBookButtons: Page names do not match.');
      InsertVisiblePage(Pages[i]);
      CreateButtons(i, fUserOrder.ComponentPages.Objects[i] as TStringList);
    end;

    // restore active page
    if (fOldActivePage<>nil) and (FPageControl.IndexOf(fOldActivePage) >= 0) then begin
      {$IFDEF VerboseComponentPalette}
      DebugLn(['TComponentPalette.UpdateNoteBookButtons: Restoring FPageControl.ActivePage=',
               fOldActivePage.Caption]);
      {$ENDIF}
      FPageControl.ActivePage:=fOldActivePage
    end
    else if FPageControl.PageCount>0 then
      FPageControl.PageIndex:=0;
  finally
    // unlock
    fUpdatingPageControl:=false;
    fNoteBookNeedsUpdate:=false;
    //FPageControl.EnableAutoSizing;
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

end.

