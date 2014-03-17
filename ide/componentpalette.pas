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

  Author: Mattias Gaertner

  Abstract:
   The implementation of the component palette.
}
unit ComponentPalette;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, Graphics, ComCtrls, Buttons, FileUtil,
  LazFileCache, Menus, LResources, AVL_Tree, PropEdits, FormEditingIntf,
  LazIDEIntf, IDEProcs, LCLProc, ExtCtrls,
  {$IFDEF CustomIDEComps}
  CustomIDEComps,
  {$ENDIF}
  LazarusIDEStrConsts, ComponentReg, ExtendedTabControls, DesignerProcs, PackageDefs,
  EnvironmentOpts;

type
  TComponentSelectionMode = (
    csmSingle, // reset selection on component add
    csmMulty   // don't reset selection on component add
  );

  { TComponentPalette }

  TComponentPalette = class(TBaseComponentPalette)
    PalettePopupMenu: TPopupMenu;
    PopupMenu: TPopupMenu;
    OpenPackageMenuItem: TMenuItem;
    OpenUnitMenuItem: TMenuItem;
    procedure ActivePageChanged(Sender: TObject);
    procedure OnPageResize(Sender: TObject);
    procedure OpenPackageClicked(Sender: TObject);
    procedure OpenUnitClicked(Sender: TObject);
    procedure ComponentListClicked(Sender: TObject);
    procedure PalettePopupMenuPopup(Sender: TObject);
    procedure PopupMenuPopup(Sender: TObject);
  private
    fComponents: TAVLTree; // tree of TRegisteredComponent sorted for componentclass
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
    procedure SetPageControl(const AValue: TPageControl);
    procedure SelectionToolClick(Sender: TObject);
    procedure ComponentBtnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ComponentBtnMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ComponentBtnDblClick(Sender: TObject);
    procedure CreatePopupMenu;
    procedure UnselectAllButtons;
    function SortPagesAndCompsUserOrder: Boolean;
  protected
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
    procedure DoAfterComponentAdded; override;
    function GetUnregisteredIcon: TCustomBitmap;
    function GetSelectButtonIcon: TCustomBitmap;
    procedure ClearButtons; override;
    function SelectButton(Button: TComponent): boolean;
    procedure ReAlignButtons(Page: TCustomPage);
    procedure UpdateNoteBookButtons;
    procedure OnGetNonVisualCompIcon(Sender: TObject;
                                     AComponent: TComponent; var Icon: TCustomBitmap);
    function FindComponent(const CompClassName: string
                           ): TRegisteredComponent; override;
    procedure RegisterCustomIDEComponents(
                       const RegisterProc: RegisterUnitComponentProc); override;
    procedure UpdateVisible; override;
  public
    property PageControl: TPageControl read FPageControl write SetPageControl;
    property SelectionMode: TComponentSelectionMode read FSelectionMode write FSelectionMode;
    property OnOpenPackage: TNotifyEvent read FOnOpenPackage write FOnOpenPackage;
    property OnOpenUnit: TNotifyEvent read FOnOpenUnit write FOnOpenUnit;
    property OnClassSelected: TNotifyEvent read FOnClassSelected write FOnClassSelected;
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

{ TComponentPalette }

procedure TComponentPalette.ActivePageChanged(Sender: TObject);
begin
  if FPageControl=nil then exit;
  if (FSelected<>nil)
  and (FSelected.Page.PageComponent=FPageControl.ActivePage)
  then exit;
  Selected:=nil;
end;

procedure TComponentPalette.OnPageResize(Sender: TObject);
begin
  if Sender is TCustomPage then
    ReAlignButtons(TCustomPage(Sender));
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
  MainIDE.DoShowComponentList(true);
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
  for i:=0 to Count-1 do begin
    CurPage:=Pages[i];
    if (FSelected=nil) or (FSelected.Page<>CurPage) then begin
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
    if (FSelected.Page=nil) or (FSelected.Page.Palette<>Self)
    or (not FSelected.Visible)
    or (not FSelected.CanBeCreatedInDesigner) then
      FSelected:=nil;
  end;
  if FPageControl=nil then exit;
  UnselectAllButtons;
  // select button
  if (FSelected<>nil) and (FPageControl<>nil) then begin
    TSpeedButton(FSelected.Button).Down:=true;
    FPageControl.ActivePage:=TTabSheet(FSelected.Page.PageComponent);
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

procedure TComponentPalette.OnPageAddedComponent(Component: TRegisteredComponent);
begin
  fComponents.Add(Component);
  inherited OnPageAddedComponent(Component);
end;

procedure TComponentPalette.OnPageRemovedComponent(Page: TBaseComponentPage;
  Component: TRegisteredComponent);
begin
  fComponents.Remove(Component);
  inherited OnPageRemovedComponent(Page, Component);
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
    Invisible:=(RegComp=nil) or (RegComp.PageName='');
  end;
end;

constructor TComponentPalette.Create;
begin
  inherited Create;
  FSelectionMode := csmSingle;
  fComponents:=TAVLTree.Create(@CompareRegisteredComponents);
  OnComponentIsInvisible:=@CheckComponentDesignerVisible;
end;

destructor TComponentPalette.Destroy;
begin
  if OnComponentIsInvisible=@CheckComponentDesignerVisible then
    OnComponentIsInvisible:=nil;
  PageControl:=nil;
  FreeAndNil(fComponents);
  FreeAndNil(fUnregisteredIcon);
  FreeAndNil(fSelectButtonIcon);
  FreeAndNil(PopupMenu);
  FreeAndNil(PalettePopupMenu);
  inherited Destroy;
end;

procedure TComponentPalette.DoAfterComponentAdded;
begin
  inherited DoAfterComponentAdded;
  if not (ssShift in GetKeyShiftState) and (SelectionMode = csmSingle) then
    Selected := nil;
end;

function TComponentPalette.GetUnregisteredIcon: TCustomBitMap;
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
  inherited ClearButtons;
  if FPageControl<>nil then
    FPageControl.EnableAlign;
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
  Rows: Integer;
  MaxBtnPerRow: Integer;
  ButtonTree: TAVLTree;
  Node: TAVLTreeNode;
  ScrollBox: TScrollBox;
begin
  //DebugLn(['TComponentPalette.ReAlignButtons ',Page.Caption,' ',Page.ClientWidth]);
  if FPageControl<>nil then
    PageControl.DisableAutoSizing{$IFDEF DebugDisableAutoSizing}('TComponentPalette.ReAlignButtons'){$ENDIF};
  ButtonTree:=nil;
  try
    if (Page.ComponentCount=0) or not (Page.Components[0] is TScrollBox) then exit;
    ButtonTree:=TAVLTree.Create(@CompareControlsWithTag);

    ScrollBox := TScrollBox(Page.Components[0]);
    // skip the first control (this is the selection tool (TSpeedButton))
    for j:= 1 to ScrollBox.ControlCount-1 do begin
      CurButton:=TSpeedbutton(ScrollBox.Controls[j]);
      if (CurButton is TSpeedButton) and CurButton.Visible then
        ButtonTree.Add(CurButton);
    end;
    if ButtonTree.Count=0 then exit;

    ButtonX:= ((ComponentPaletteBtnWidth*3) div 2) + 2;

    MaxBtnPerRow:=((Page.ClientWidth - ButtonX - OVERVIEW_PANEL_WIDTH) div ComponentPaletteBtnWidth);
    if MaxBtnPerRow<1 then MaxBtnPerRow:=1;
    Rows:=((ButtonTree.Count-1) div MaxBtnPerRow)+1;
    //DebugLn(['TComponentPalette.ReAlignButtons ',DbgSName(Page),' PageIndex=',Page.PageIndex,' ClientRect=',dbgs(Page.ClientRect)]);
    // automatically set optimal row count and re-position controls to use height optimally

    if Rows <= 0 then Rows:= 1; // avoid division by zero

    j:=0;
    Node:=ButtonTree.FindLowest;
    while Node<>nil do begin
      CurButton:=TSpeedbutton(Node.Data);
      CurButton.SetBounds(ButtonX + (j div Rows) * ComponentPaletteBtnWidth,
                          (j mod Rows) * ComponentPaletteBtnHeight,
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

function TComponentPalette.SortPagesAndCompsUserOrder: Boolean;
// Calculate page order by user config and default order. User config takes priority.
// This order will be shown in the palette.
var
  Pg: TBaseComponentPage;
  Comp: TRegisteredComponent;
  SrcComps, DstComps: TStringList;
  i, PgInd, CompInd: Integer;
  PgName: String;
begin
  Result := True;
  for i:=0 to fPagesUserOrder.Count-1 do
    fPagesUserOrder.Objects[i].Free;   // Free also the contained StringList.
  fPagesUserOrder.Clear;
  with EnvironmentOptions do begin
    // First add user defined page order from EnvironmentOptions,
    fPagesUserOrder.Assign(ComponentPaletteOptions.PageNames);
    // then add other pages which don't have user configuration
    for PgInd := 0 to fPagesDefaultOrder.Count-1 do
    begin
      Pg:=TBaseComponentPage(fPagesDefaultOrder[PgInd]);
      if (fPagesUserOrder.IndexOf(Pg.PageName) = -1)
      and (ComponentPaletteOptions.HiddenPageNames.IndexOf(Pg.PageName) = -1) then
        fPagesUserOrder.Add(Pg.PageName);
    end;
    // Add components for every page
    for i := 0 to fPagesUserOrder.Count-1 do
    begin
      PgName := fPagesUserOrder[i];
      DstComps := TStringList.Create;
      fPagesUserOrder.Objects[i] := DstComps;
      PgInd := ComponentPaletteOptions.ComponentPages.IndexOf(PgName);
      if PgInd >= 0 then
      begin
        // Add components that were reordered by user
        SrcComps := ComponentPaletteOptions.ComponentPages.Objects[PgInd] as TStringList;
        DstComps.Assign(SrcComps);
      end
      else begin
        // Add components that were not reordered.
        PgInd := IndexOfPageName(PgName);
        if PgInd >= 0 then
        begin
          Pg:=Pages[PgInd];
          for CompInd := 0 to Pg.Count-1 do
          begin
            Comp := Pg[CompInd];
            DstComps.Add(Comp.ComponentClass.ClassName);
          end;
        end;
      end;
    end;
  end;
end;

procedure TComponentPalette.UpdateNoteBookButtons;
var
  OldActivePage: TTabSheet;

  procedure RemoveUnneededPage(aSheet: TCustomPage);
  var
    PageIndex: Integer;
    CurPage: TBaseComponentPage;
    CurBtn: TSpeedButton;
  begin
    PageIndex:=IndexOfPageComponent(aSheet);
    if (PageIndex<0) or (not Pages[PageIndex].Visible) then begin
      // page is not needed anymore => delete
      if PageIndex>=0 then begin
        CurPage:=Pages[PageIndex];
        CurBtn:=TSpeedButton(CurPage.SelectButton);
        if CurBtn<>nil then begin
          CurPage.SelectButton:=nil;
          Application.ReleaseComponent(CurBtn);
        end;
        Pages[PageIndex].PageComponent:=nil;
      end;
      if aSheet=OldActivePage then
        OldActivePage:=nil;
      aSheet.Visible:=false;
      Application.ReleaseComponent(aSheet);
    end;
  end;

  procedure InsertVisiblePage(aCompPage: TBaseComponentPage; var aVisPageIndex: integer);
  var
    CurPageIndex: Integer;
    CurScrollBox: TScrollBox;
    PanelRight: TPanel;
    BtnRight: TSpeedButton;
  begin
    if not aCompPage.Visible then Exit;
    if aCompPage.PageComponent=nil then begin
      // insert a new PageControl page
      TCustomTabControl(FPageControl).Pages.Insert(aVisPageIndex, aCompPage.PageName);
      aCompPage.PageComponent := FPageControl.Page[aVisPageIndex];
      CurScrollBox := TScrollBox.Create(aCompPage.PageComponent);
      with CurScrollBox do begin
        Align := alClient;
        BorderStyle := bsNone;
        BorderWidth := 0;
        HorzScrollBar.Visible := false;
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
      end;
      BtnRight:=TSpeedButton.Create(aCompPage.PageComponent);
      with BtnRight do
      begin
        LoadGlyphFromResourceName(HInstance, 'SelCompPage');
        Flat := True;
        SetBounds(2,1,16,16);
        Hint := 'Click to Select Palette Page';
        OnClick := @MainIDE.SelComponentPageButtonClick;
        Parent := PanelRight;
      end;
    end else begin
      // move to the right position
      CurPageIndex := aCompPage.PageComponent.PageIndex;
      if CurPageIndex<>aVisPageIndex then
        TCustomTabControl(FPageControl).Pages.Move(CurPageIndex, aVisPageIndex);
    end;
    inc(aVisPageIndex);
  end;

  procedure CreateSelectionButton(aCompPage: TBaseComponentPage; aButtonUniqueName: string;
    aScrollBox: TScrollBox);
  var
    CurBtn: TSpeedButton;
  begin
    if Assigned(aCompPage.SelectButton) then Exit;
    CurBtn := TSpeedButton.Create(nil);
    aCompPage.SelectButton:=CurBtn;
    with CurBtn do begin
      Name := 'PaletteSelectBtn' + aButtonUniqueName;
      OnClick := @SelectionToolClick;
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

  procedure CreateOrDeleteButton(aComp: TPkgComponent; aButtonUniqueName: string;
    aScrollBox: TScrollBox; var aBtnIndex: integer);
  var
    CurBtn: TSpeedButton;
  begin
    if aComp.Visible then begin
      inc(aBtnIndex);
      //DebugLn(['TComponentPalette.UpdateNoteBookButtons Component ',DbgSName(aComp.ComponentClass),' ',aComp.Visible,' Prio=',dbgs(aComp.GetPriority)]);
      if aComp.Button=nil then begin
        CurBtn := TSpeedButton.Create(nil);
        aComp.Button:=CurBtn;
        CreatePopupMenu;
        with CurBtn do begin
          Name := 'PaletteBtnPage' + aButtonUniqueName + aComp.ComponentClass.ClassName;
          // Left and Top will be set in ReAlignButtons.
          SetBounds(Left,Top,ComponentPaletteBtnWidth,ComponentPaletteBtnHeight);
          Glyph.Assign(aComp.Icon);
          GroupIndex := 1;
          Flat := true;
          OnMouseDown := @ComponentBtnMouseDown;
          OnMouseUp := @ComponentBtnMouseUp;
          OnDblClick := @ComponentBtnDblClick;
          ShowHint := EnvironmentOptions.ShowHintsForComponentPalette;
          Hint := aComp.ComponentClass.ClassName + sLineBreak
            + '(' + aComp.ComponentClass.UnitName + ')';
          CurBtn.PopupMenu:=Self.PopupMenu;
        end;
        //debugln(['TComponentPalette.UpdateNoteBookButtons Created Button: ',aComp.ComponentClass.ClassName,' ',aComp.Button.Name]);
      end else begin
        CurBtn:=TSpeedButton(aComp.Button);
        //DebugLn(['TComponentPalette.UpdateNoteBookButtons Keep Button: ',aComp.ComponentClass.ClassName,' ',aComp.Button.Name,' ',DbgSName(TControl(aComp.Button).Parent)]);
      end;
      CurBtn.Parent := aScrollBox;
      CurBtn.Tag:=aBtnIndex;
    end
    else if aComp.Button<>nil then begin
      //debugln(['TComponentPalette.UpdateNoteBookButtons Destroy Button: ',aComp.ComponentClass.ClassName,' ',aComp.Button.Name]);
      TControl(aComp.Button).Visible:=false;
      Application.ReleaseComponent(aComp.Button);
      aComp.Button:=nil;
    end;
  end;

  procedure CreateButtons(aPageIndex: integer);
  var
    i, BtnIndex: Integer;
    CurNoteBookPage: TCustomPage;
    CurScrollBox: TScrollBox;
    CompPage: TBaseComponentPage;
  begin
    CompPage := Pages[aPageIndex];
    if not CompPage.Visible then Exit;
    CurNoteBookPage := CompPage.PageComponent;
    CurNoteBookPage.OnResize := @OnPageResize;
    CurScrollBox := CurNoteBookPage.Components[0] as TScrollBox;
    //DebugLn(['TComponentPalette.UpdateNoteBookButtons PAGE=',CompPage.PageName,' PageIndex=',CurNoteBookPage.PageIndex]);
    // create selection button
    CreateSelectionButton(CompPage, IntToStr(aPageIndex), CurScrollBox);
    // create component buttons and delete unneeded ones
    BtnIndex:=0;
    for i:=0 to CompPage.Count-1 do
      CreateOrDeleteButton(TPkgComponent(CompPage[i]),
        IntToStr(aPageIndex)+'_'+IntToStr(i)+'_', CurScrollBox, BtnIndex);
    ReAlignButtons(CurNoteBookPage);
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
    SortPagesDefaultOrder;           // Updates fPagesDefaultOrder
    SortPagesAndCompsUserOrder;      // Updates fPagesUserOrder
    // remove every page in the PageControl without a visible page
    for i:=FPageControl.PageCount-1 downto 0 do
      RemoveUnneededPage(FPageControl.Pages[i]);
    // insert a PageControl page for every visible palette page
    VisPageIndex := 0;
    for i := 0 to fPagesUserOrder.Count-1 do
    begin
      PgName := fPagesUserOrder[i];
      PgInd := IndexOfPageName(PgName);
      if PgInd >= 0 then
      begin
        InsertVisiblePage(Pages[PgInd], VisPageIndex);
        CreateButtons(PgInd);  // create speedbuttons for every visible component
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
  ANode:=fComponents.FindKey(Pointer(CompClassName),
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

