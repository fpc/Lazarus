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
  Classes, SysUtils, LCLProc, Controls, Forms, Dialogs, Graphics, ExtCtrls,
  ComCtrls, Buttons, FileUtil, Menus, LResources, AVL_Tree,
  PropEdits, FormEditingIntf, LazIDEIntf, MacroIntf,
  {$IFDEF CustomIDEComps}
  CustomIDEComps,
  {$ENDIF}
  LazarusIDEStrConsts, ComponentReg, DesignerProcs, IDEProcs, PackageDefs,
  FindPaletteComp;

type
  TComponentSelectionMode = (
    csmSingle, // reset selection on component add
    csmMulty   // don't reset selection on component add
  );

  { TComponentPalette }

  TComponentPalette = class(TBaseComponentPalette)
    PopupMenu: TPopupMenu;
    OpenPackageMenuItem: TMenuItem;
    OpenUnitMenuItem: TMenuItem;
    FindComponentMenuItem: TMenuItem;
    procedure ActivePageChanged(Sender: TObject);
    procedure OnPageResize(Sender: TObject);
    procedure OpenPackageClicked(Sender: TObject);
    procedure OpenUnitClicked(Sender: TObject);
    procedure FindComponentClicked(Sender: TObject);
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
    procedure ReAlignButtons(Page: TTabSheet);
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
  if Sender is TTabSheet then
    ReAlignButtons(TTabSheet(Sender));
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

procedure TComponentPalette.FindComponentClicked(Sender: TObject);
var
  AComponent: TRegisteredComponent;
begin
  if ShowFindPaletteComponentDlg(AComponent)<>mrOk then exit;
  Selected:=AComponent;
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
begin
  if FPageControl=AValue then exit;
  ClearButtons;
  FPageControl:=AValue;
  if FPageControl<>nil then begin
    FPageControl.OnChange:=@ActivePageChanged;
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
  if ssShift in Shift then
    SelectionMode := csmMulty
  else
    SelectionMode := csmSingle;
  SelectButton(TComponent(Sender));
  if Assigned(OnClassSelected) then
    OnClassSelected(Self);
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
begin
  if PopupMenu<>nil then exit;
  PopupMenu:=TPopupMenu.Create(nil);
  PopupMenu.OnPopup:=@PopupMenuPopup;
  PopupMenu.Name:='ComponentPalettePopupMenu';
  
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

  FindComponentMenuItem:=TMenuItem.Create(PopupMenu);
  with FindComponentMenuItem do begin
    Name:='FindComponentMenuItem';
    Caption:=lisCompPalFindComponent;
    OnClick:=@FindComponentClicked;
  end;
  PopupMenu.Items.Add(FindComponentMenuItem);
end;

procedure TComponentPalette.DoBeginUpdate;
begin
  inherited DoBeginUpdate;
end;

procedure TComponentPalette.DoEndUpdate(Changed: boolean);
begin
  if Changed or fNoteBookNeedsUpdate then UpdateNoteBookButtons;
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
    fUnregisteredIcon := CreateBitmapFromLazarusResource('unregisteredcomponent');
    if fUnregisteredIcon = nil then
      fUnregisteredIcon := CreateBitmapFromLazarusResource('default');
  end;
  Result := fUnregisteredIcon;
end;

function TComponentPalette.GetSelectButtonIcon: TCustomBitmap;
begin
  if fSelectButtonIcon=nil then 
    fSelectButtonIcon := CreateBitmapFromLazarusResource('tmouse');
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

procedure TComponentPalette.ReAlignButtons(Page: TTabSheet);
var
  j: integer;
  buttonx: integer;
  CurButton: TSpeedButton;
  Rows: Integer;
  MaxBtnPerRow: Integer;
  ButtonTree: TAVLTree;
  Node: TAVLTreeNode;
begin
  //DebugLn(['TComponentPalette.ReAlignButtons ',Page.Caption,' ',Page.ClientWidth]);
  if FPageControl<>nil then
    PageControl.DisableAutoSizing{$IFDEF DebugDisableAutoSizing}('TComponentPalette.ReAlignButtons'){$ENDIF};
  ButtonTree:=nil;
  try
    ButtonTree:=TAVLTree.Create(@CompareControlsWithTag);

    // skip the first control (this is the selection tool (TSpeedButton))
    for j:= 1 to Page.ControlCount-1 do begin
      CurButton:=TSpeedbutton(Page.Controls[j]);
      if not (CurButton is TSpeedButton) then continue;
      if not CurButton.Visible then continue;
      ButtonTree.Add(CurButton);
    end;
    if ButtonTree.Count=0 then exit;

    ButtonX:= ((ComponentPaletteBtnWidth*3) div 2) + 2;

    MaxBtnPerRow:=((Page.ClientWidth - ButtonX) div ComponentPaletteBtnWidth);
    if MaxBtnPerRow<1 then MaxBtnPerRow:=1;
    Rows:=((ButtonTree.Count-1) div MaxBtnPerRow)+1;
    //DebugLn(['TComponentPalette.ReAlignButtons ',DbgSName(Page),' PageIndex=',Page.PageIndex,' ClientRect=',dbgs(Page.ClientRect)]);
    // automatically set optimal row count and re-position controls to use height optimally

    if Rows <= 0 then Rows:= 1; // avoid division by zero

    j:=0;
    Node:=ButtonTree.FindLowest;
    while Node<>nil do begin
      CurButton:=TSpeedbutton(Node.Data);
      CurButton.SetBounds(
        ButtonX + (j div Rows) * ComponentPaletteBtnWidth,
        (j mod Rows) * ComponentPaletteBtnHeight,
        CurButton.Width,CurButton.Height);
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
  i: Integer;
  PageIndex: Integer;
  CurPage: TBaseComponentPage;
  CurNoteBookPage: TTabSheet;
  CurComponent: TPkgComponent;
  CurBtn: TSpeedButton;
  CurPageIndex: Integer;
  j: Integer;
  OldActivePage: TTabSheet;
  BtnIndex: Integer;
  SortedCompList: TFPList;
  k: Integer;
  SortedPageList: TFPList;
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
  //writeln('TComponentPalette.UpdateNoteBookButtons A');
  // lock
  fUpdatingPageControl:=true;
  FPageControl.DisableAlign;
  SortedPageList:=TFPList.Create;
  SortedCompList:=TFPList.Create;
  try
    OldActivePage:=FPageControl.ActivePage;

    // remove every page in the PageControl without a visible page
    for i:=FPageControl.PageCount-1 downto 0 do begin
      PageIndex:=IndexOfPageComponent(FPageControl.Pages[i]);
      if (PageIndex<0) or (not Pages[PageIndex].Visible) then begin
        // page is not needed anymore => delete
        if PageIndex>=0 then begin
          CurPage:=Pages[PageIndex];
          CurBtn:=TSpeedButton(CurPage.SelectButton);
          CurPage.SelectButton:=nil;
          Application.ReleaseComponent(CurBtn);
          Pages[PageIndex].PageComponent:=nil;
        end;
        if FPageControl.Pages[i]=OldActivePage then
          OldActivePage:=nil;
        FPageControl.Pages[i].Visible:=false;
        Application.ReleaseComponent(FPageControl.Pages[i]);
      end;
    end;

    // sort pages
    for i:=0 to Count-1 do begin
      CurPage:=Pages[i];
      j:=SortedPageList.Count-1;
      //debugln(['TComponentPalette.UpdateNoteBookButtons Page ',CurPage.PageName,' Prio=',dbgs(CurPage.GetMaxComponentPriority)]);
      while (j>=0)
      and (ComparePriority(CurPage.GetMaxComponentPriority,
               TBaseComponentPage(SortedPageList[j]).GetMaxComponentPriority)>0)
      do
        dec(j);
      SortedPageList.Insert(j+1,CurPage);
    end;

    // insert a PageControl page for every visible palette page
    PageIndex:=0;
    for i:=0 to SortedPageList.Count-1 do begin
      CurPage:=TBaseComponentPage(SortedPageList[i]);
      if not CurPage.Visible then continue;
      if CurPage.PageComponent=nil then begin
        // insert a new PageControl page
        TCustomTabControl(FPageControl).Pages.Insert(PageIndex,CurPage.PageName);
        CurPage.PageComponent:=FPageControl.Page[PageIndex];
      end else begin
        // move to the right position
        CurPageIndex:=TTabSheet(CurPage.PageComponent).PageIndex;
        if CurPageIndex<>PageIndex then
          TCustomTabControl(FPageControl).Pages.Move(CurPageIndex,PageIndex);
      end;
      inc(PageIndex);
    end;

    // create a speedbutton for every visible component
    for i:=0 to Count-1 do begin
      CurPage:=Pages[i];
      if not CurPage.Visible then continue;
      CurNoteBookPage:=TTabSheet(CurPage.PageComponent);
      if not (CurNoteBookPage is TTabSheet) then RaiseException('CurNoteBookPage');
      CurNoteBookPage.OnResize:=@OnPageResize;
      //DebugLn(['TComponentPalette.UpdateNoteBookButtons PAGE=',CurPage.PageName,' PageIndex=',CurNoteBookPage.PageIndex]);

      // create selection button
      if CurPage.SelectButton=nil
      then begin
        CurBtn:=TSpeedButton.Create(nil);
        CurPage.SelectButton:=CurBtn;
        with CurBtn do
        begin
          Name:='PaletteSelectBtn'+IntToStr(i);
          OnClick := @SelectionToolClick;
          LoadGlyphFromLazarusResource('tmouse');
          Flat := True;
          GroupIndex:= 1;
          Down := True;
          Hint := lisSelectionTool;
          SetBounds(0,0,ComponentPaletteBtnWidth,ComponentPaletteBtnHeight);
          Parent:=CurNoteBookPage;
        end;
      end;

      // sort components for priority
      SortedCompList.Clear;
      for j:=0 to CurPage.Count-1 do begin
        CurComponent:=TPkgComponent(CurPage[j]);
        k:=SortedCompList.Count-1;
        while (k>=0)
        and (ComparePriority(CurComponent.GetPriority,TPkgComponent(SortedCompList[k]).GetPriority)>0)
        do
          dec(k);
        SortedCompList.Insert(k+1,CurComponent);
      end;

      // create component buttons and delete unneeded
      BtnIndex:=0;
      for j:=0 to SortedCompList.Count-1 do begin
        CurComponent:=TPkgComponent(SortedCompList[j]);
        if CurComponent.Visible then begin
          inc(BtnIndex);
          //DebugLn(['TComponentPalette.UpdateNoteBookButtons Component ',DbgSName(CurComponent.ComponentClass),' ',CurComponent.Visible,' Prio=',dbgs(CurComponent.GetPriority)]);
          if CurComponent.Button=nil then begin
            CurBtn:=TSpeedButton.Create(nil);
            CurComponent.Button:=CurBtn;
            CreatePopupMenu;
            with CurBtn do
            begin
              Name:='PaletteBtnPage'+IntToStr(i)+'_'+IntToStr(j)
                    +'_'+CurComponent.ComponentClass.ClassName;
              // Left and Top will be set in ReAlignButtons.
              SetBounds(Left,Top,ComponentPaletteBtnWidth,ComponentPaletteBtnHeight);
              Glyph.Assign(CurComponent.Icon);
              GroupIndex := 1;
              Flat := true;
              OnMouseDown:= @ComponentBtnMouseDown;
              OnMouseUp:=@ComponentBtnMouseUp;
              OnDblClick := @ComponentBtnDblClick;
              ShowHint := true;
              Hint := CurComponent.ComponentClass.ClassName;
              CurBtn.PopupMenu:=Self.PopupMenu;
              Parent := CurNoteBookPage;
              Tag:=BtnIndex;
            end;
            //debugln(['TComponentPalette.UpdateNoteBookButtons Created Button: ',CurComponent.ComponentClass.ClassName,' ',CurComponent.Button.Name]);
          end else begin
            CurBtn:=TSpeedButton(CurComponent.Button);
            CurBtn.Parent := CurNoteBookPage;
            CurBtn.Tag:=BtnIndex;
            //DebugLn(['TComponentPalette.UpdateNoteBookButtons Keep Button: ',CurComponent.ComponentClass.ClassName,' ',CurComponent.Button.Name,' ',DbgSName(TControl(CurComponent.Button).Parent)]);
          end;
        end else if CurComponent.Button<>nil then begin
          //debugln(['TComponentPalette.UpdateNoteBookButtons Destroy Button: ',CurComponent.ComponentClass.ClassName,' ',CurComponent.Button.Name]);
          TControl(CurComponent.Button).Visible:=false;
          Application.ReleaseComponent(CurComponent.Button);
          CurComponent.Button:=nil;
        end;
      end;

      ReAlignButtons(CurNoteBookPage);
    end;
    // restore active page
    if (OldActivePage<>nil) and (FPageControl.IndexOf(OldActivePage)>=0) then
    begin
      FPageControl.ActivePage:=OldActivePage;
    end else if FPageControl.PageCount>0 then begin
      FPageControl.PageIndex:=0;
    end;
  finally
    // unlock
    fUpdatingPageControl:=false;
    fNoteBookNeedsUpdate:=false;
    FPageControl.EnableAlign;
    SortedCompList.Free;
    SortedPageList.Free;
  end;
  //debugln('TComponentPalette.UpdateNoteBookButtons END');
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
  if ARegComp<>nil then begin
    Icon:=TPkgComponent(ARegComp).Icon;
  end else begin
    Icon:=GetUnregisteredIcon;
  end;
end;

function TComponentPalette.FindComponent(const CompClassName: string
  ): TRegisteredComponent;
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
  inherited RegisterCustomIDEComponents(RegisterProc);
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

initialization
{$I ../images/components_images.lrs}

end.

