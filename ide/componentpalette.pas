{  $Id$  }
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
  Classes, SysUtils, LCLProc, Controls, Dialogs, Graphics, ExtCtrls, Buttons,
  Menus, LResources, AVL_Tree,
  PropEdits, FormEditingIntf, LazIDEIntf, MacroIntf,
  {$IFDEF CustomIDEComps}
  CustomIDEComps,
  {$ENDIF}
  LazarusIDEStrConsts, ComponentReg, DesignerProcs, IDEProcs, PackageDefs,
  FindPaletteComp;

type
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
    FNoteBook: TNotebook;
    fNoteBookNeedsUpdate: boolean;
    FOnOpenPackage: TNotifyEvent;
    FOnOpenUnit: TNotifyEvent;
    FSelected: TRegisteredComponent;
    fUnregisteredIcon: TBitmap;
    fSelectButtonIcon: TBitmap;
    fUpdatingNotebook: boolean;
    procedure SetNoteBook(const AValue: TNotebook);
    procedure SelectionToolClick(Sender: TObject);
    procedure ComponentBtnClick(Sender: TObject);
    procedure ComponentBtnDblClick(Sender: TObject);
    procedure SetSelected(const AValue: TRegisteredComponent);
    procedure CreatePopupMenu;
  protected
    procedure DoEndUpdate(Changed: boolean); override;
    procedure OnPageAddedComponent(Component: TRegisteredComponent); override;
    procedure OnPageRemovedComponent(Page: TBaseComponentPage;
                                     Component: TRegisteredComponent); override;
    procedure Update; override;
    procedure CheckComponentDesignerVisible(AComponent: TComponent;
                                            var Invisible: boolean);
  public
    constructor Create;
    destructor Destroy; override;
    function GetUnregisteredIcon: TBitmap;
    function GetSelectButtonIcon: TBitmap;
    procedure ClearButtons; override;
    function SelectButton(Button: TComponent): boolean;
    procedure ReAlignButtons(Page: TPage);
    procedure UpdateNoteBookButtons;
    procedure OnGetNonVisualCompIcon(Sender: TObject;
                                     AComponent: TComponent; var Icon: TBitmap);
    function FindComponent(const CompClassName: string
                           ): TRegisteredComponent; override;
    procedure RegisterCustomIDEComponents(
                       const RegisterProc: RegisterUnitComponentProc); override;
  public
    property NoteBook: TNotebook read FNoteBook write SetNoteBook;
    property Selected: TRegisteredComponent read FSelected write SetSelected;
    property OnOpenPackage: TNotifyEvent read FOnOpenPackage write FOnOpenPackage;
    property OnOpenUnit: TNotifyEvent read FOnOpenUnit write FOnOpenUnit;
  end;

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

{ TComponentPalette }

procedure TComponentPalette.ActivePageChanged(Sender: TObject);
begin
  if FNoteBook=nil then exit;
  if (FSelected<>nil)
  and (FSelected.Page.PageComponent=FNoteBook.ActivePageComponent)
  then exit;
  Selected:=nil;
end;

procedure TComponentPalette.OnPageResize(Sender: TObject);
begin
  if Sender is TPage then
    ReAlignButtons(TPage(Sender));
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
    OpenPackageMenuItem.Caption:=Format(lisCPOpenPackage, [APackage.IDAsString]
      );
    OpenPackageMenuItem.Visible:=true;
    ShownFilename:=PkgComponent.PkgFile.Filename;
    UnitFilename:=APackage.SubstitutePkgMacro(ShownFilename,true);
    IDEMacros.SubstituteMacros(UnitFilename);
    if not FileExists(UnitFilename) then begin
      UnitFilename:=LazarusIDE.FindSourceFile(ExtractFilename(UnitFilename),
                                              APackage.Directory,[]);
      if FileExists(UnitFilename) then
        UnitFilename:=ShownFilename;
    end;
    OpenUnitMenuItem.Caption:=Format(lisCPOpenUnit, [ShownFilename]);
    OpenUnitMenuItem.Visible:=true;
    OpenUnitMenuItem.Enabled:=FileExists(UnitFilename);
  end;
end;

procedure TComponentPalette.SetNoteBook(const AValue: TNotebook);
begin
  if FNoteBook=AValue then exit;
  ClearButtons;
  FNoteBook:=AValue;
  if FNoteBook<>nil then begin
    FNoteBook.OnPageChanged:=@ActivePageChanged;
  end;
  UpdateNoteBookButtons;
end;

procedure TComponentPalette.SelectionToolClick(Sender: TObject);
begin
  SelectButton(TComponent(Sender));
end;

procedure TComponentPalette.ComponentBtnClick(Sender: TObject);
begin
  SelectButton(TComponent(Sender));
end;

procedure TComponentPalette.ComponentBtnDblClick(Sender: TObject);
var
  TypeClass: TComponentClass;
  ParentCI: TIComponentInterface;
  X, Y: integer;
  CompIntf: TIComponentInterface;
begin
  //debugln('TComponentPalette.ComponentBtnDblClick ',TComponent(Sender).Name);
  if SelectButton(TComponent(Sender)) and (FSelected<>nil) then begin
    if FormEditingHook<>nil then begin
      TypeClass:=FSelected.ComponentClass;
      ParentCI:=FormEditingHook.GetDefaultComponentParent(TypeClass);
      if ParentCI=nil then exit;
      if not FormEditingHook.GetDefaultComponentPosition(TypeClass,ParentCI,X,Y)
      then exit;
      //debugln('TComponentPalette.ComponentBtnDblClick ',dbgsName(Sender),' ',dbgs(X),',',dbgs(Y));
      CompIntf:=FormEditingHook.CreateComponent(ParentCI,TypeClass,'',X,Y,0,0);
      if CompIntf<>nil then begin
        GlobalDesignHook.PersistentAdded(CompIntf.Component,true);
      end;
    end;
  end;
  Selected:=nil;
end;

procedure TComponentPalette.SetSelected(const AValue: TRegisteredComponent);
var
  SelectButtonOnPage: TSpeedButton;
  CurPage: TBaseComponentPage;
  i: Integer;
begin
  if FSelected=AValue then exit;
  FSelected:=AValue;
  if FSelected<>nil then begin
    if (FSelected.Page=nil) or (FSelected.Page.Palette<>Self)
    or (not FSelected.Visible)
    or (not FSelected.CanBeCreatedInDesigner) then
      FSelected:=nil;
  end;
  if FNoteBook=nil then exit;
  // unselect all other buttons on all other notebook pages
  for i:=0 to Count-1 do begin
    CurPage:=Pages[i];
    if (FSelected=nil) or (FSelected.Page<>CurPage) then begin
      SelectButtonOnPage:=TSpeedButton(CurPage.SelectButton);
      if SelectButtonOnPage<>nil then SelectButtonOnPage.Down:=true;
    end;
  end;
  // select button
  if (FSelected<>nil) and (FNoteBook<>nil) then begin
    TSpeedButton(FSelected.Button).Down:=true;
    FNoteBook.ActivePageComponent:=TPage(FSelected.Page.PageComponent);
  end;
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

procedure TComponentPalette.DoEndUpdate(Changed: boolean);
begin
  if Changed or fNoteBookNeedsUpdate then UpdateNoteBookButtons;
  inherited DoEndUpdate(Changed);
end;

procedure TComponentPalette.OnPageAddedComponent(Component: TRegisteredComponent
  );
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
  fComponents:=TAVLTree.Create(@CompareRegisteredComponents);
  OnComponentIsInvisible:=@CheckComponentDesignerVisible;
end;

destructor TComponentPalette.Destroy;
begin
  if OnComponentIsInvisible=@CheckComponentDesignerVisible then
    OnComponentIsInvisible:=nil;
  NoteBook:=nil;
  FreeAndNil(fComponents);
  FreeAndNil(fUnregisteredIcon);
  FreeAndNil(fSelectButtonIcon);
  FreeAndNil(PopupMenu);
  inherited Destroy;
end;

function TComponentPalette.GetUnregisteredIcon: TBitMap;
begin
  if fUnregisteredIcon = nil then 
  begin
    fUnregisteredIcon := LoadBitmapFromLazarusResource('unregisteredcomponent');
    if fUnregisteredIcon = nil then
      fUnregisteredIcon := LoadBitmapFromLazarusResource('default');
  end;
  Result := fUnregisteredIcon;
end;

function TComponentPalette.GetSelectButtonIcon: TBitmap;
begin
  if fSelectButtonIcon=nil then 
    fSelectButtonIcon := LoadBitmapFromLazarusResource('tmouse');
  Result:=fSelectButtonIcon;
end;

procedure TComponentPalette.ClearButtons;
begin
  if FNoteBook<>nil then
    FNoteBook.DisableAlign;
  Selected:=nil;
  if PopupMenu<>nil then begin
    PopupMenu.Free;
    PopupMenu:=nil;
    OpenPackageMenuItem:=nil;
  end;
  inherited ClearButtons;
  if FNoteBook<>nil then
    FNoteBook.EnableAlign;
end;

function TComponentPalette.SelectButton(Button: TComponent): boolean;
var
  NewComponent: TRegisteredComponent;
begin
  NewComponent:=FindButton(Button);
  Selected:=NewComponent;
  Result:=(Selected=NewComponent);
end;

procedure TComponentPalette.ReAlignButtons(Page: TPage);
var
  j: integer;
  buttonx: integer;
  CurButton: TSpeedButton;
  Rows: Integer;
  ButtonCount: Integer;
  MaxBtnPerRow: Integer;
begin
  ButtonCount:=0;
  // skip the first control (this is the selection tool (TSpeedButton))
  for j:= 1 to Page.ControlCount-1 do begin
    CurButton:=TSpeedbutton(Page.Controls[j]);
    if not (CurButton is TSpeedButton) then continue;
    inc(ButtonCount);
  end;

  ButtonX:= ((ComponentPaletteBtnWidth*3) div 2) + 2;

  MaxBtnPerRow:=((Page.ClientWidth - ButtonX) div ComponentPaletteBtnWidth);
  Rows:=((ButtonCount-1) div MaxBtnPerRow)+1;
  //DebugLn(['TComponentPalette.ReAlignButtons ',DbgSName(Page),' PageIndex=',Page.PageIndex,' ClientRect=',dbgs(Page.ClientRect)]);
  // automatically set optimal row count and re-position controls to use height optimally

  if Rows <= 0 then Rows:= 1; // avoid division by zero

  for j:= 1 to Page.ControlCount-1 do begin
    CurButton:=TSpeedbutton(Page.Controls[j]);
    if not (CurButton is TSpeedButton) then continue;
    CurButton.SetBounds(
      ButtonX + ((j-1) div Rows) * ComponentPaletteBtnWidth,
      ((j-1) mod Rows) * ComponentPaletteBtnHeight,
      CurButton.Width,CurButton.Height)
  end;
end;

procedure TComponentPalette.UpdateNoteBookButtons;
var
  i: Integer;
  PageIndex: Integer;
  CurPage: TBaseComponentPage;
  CurNoteBookPage: TPage;
  CurComponent: TPkgComponent;
  CurBtn: TSpeedButton;
  CurPageIndex: Integer;
  j: Integer;
  OldActivePage: String;
  Bitmap: TBitmap;
begin
  if fUpdatingNotebook then exit;
  if IsUpdateLocked then begin
    fNoteBookNeedsUpdate:=true;
    exit;
  end;
  if FNoteBook=nil then begin
    fNoteBookNeedsUpdate:=false;
    exit;
  end;
  //writeln('TComponentPalette.UpdateNoteBookButtons A');
  // lock
  fUpdatingNotebook:=true;
  FNoteBook.DisableAlign;
  try
    OldActivePage:=FNoteBook.ActivePage;
    // remove every page in the notebook without a visible page
    for i:=FNoteBook.PageCount-1 downto 0 do begin
      PageIndex:=IndexOfPageComponent(FNoteBook.Page[i]);
      if (PageIndex<0) or (not Pages[PageIndex].Visible) then begin
        if PageIndex>=0 then
          Pages[i].PageComponent:=nil;
        FNoteBook.Pages.Delete(i);
      end;
    end;
    // insert a notebook page for every visible palette page
    PageIndex:=0;
    for i:=0 to Count-1 do begin
      if not Pages[i].Visible then continue;
      if Pages[i].PageComponent=nil then begin
        // insert a new notebook page
        FNoteBook.Pages.Insert(PageIndex,Pages[i].PageName);
        Pages[i].PageComponent:=FNoteBook.Page[PageIndex];
      end else begin
        // move to the right position
        CurPageIndex:=TPage(Pages[i].PageComponent).PageIndex;
        if CurPageIndex<>PageIndex then
          FNoteBook.Pages.Move(CurPageIndex,PageIndex);
      end;
      inc(PageIndex);
    end;
    // create a speedbutton for every visible component
    for i:=0 to Count-1 do begin
      CurPage:=Pages[i];
      if not CurPage.Visible then continue;
      CurNoteBookPage:=TPage(CurPage.PageComponent);
      if not (CurNoteBookPage is TPage) then RaiseException('CurNoteBookPage');
      CurNoteBookPage.OnResize:=@OnPageResize;

      // create selection button
      if CurPage.SelectButton=nil then begin
        CurBtn:=TSpeedButton.Create(nil);
        CurPage.SelectButton:=CurBtn;
        with CurBtn do begin
          Name:='PaletteSelectBtn'+IntToStr(i);
          OnClick := @SelectionToolClick;
          Bitmap := LoadBitmapFromLazarusResource('tmouse');
          Glyph := Bitmap;
          Bitmap.Free;
          Flat := True;
          GroupIndex:= 1;
          Down := True;
          Hint := lisSelectionTool;
          SetBounds(0,0,ComponentPaletteBtnWidth,ComponentPaletteBtnHeight);
          Parent:=CurNoteBookPage;
        end;
      end;

      // create component buttons
      for j:=0 to CurPage.Count-1 do begin
        CurComponent:=TPkgComponent(CurPage[j]);
        if CurComponent.Visible then begin
          if CurComponent.Button=nil then begin
            CurBtn:=TSpeedButton.Create(nil);
            CurComponent.Button:=CurBtn;
            CreatePopupMenu;
            with CurBtn do begin
              Name:='PaletteBtnPage'+IntToStr(i)+'_'+IntToStr(j)
                    +'_'+CurComponent.ComponentClass.ClassName;
              // Left and Top will be set in ReAlignButtons.
              SetBounds(Left,Top,ComponentPaletteBtnWidth,ComponentPaletteBtnHeight);
              Glyph := CurComponent.Icon;
              GroupIndex := 1;
              Flat := true;
              OnClick := @ComponentBtnClick;
              OnDblClick := @ComponentBtnDblClick;
              Hint := CurComponent.ComponentClass.ClassName;
              CurBtn.PopupMenu:=Self.PopupMenu;
              Parent := CurNoteBookPage;
            end;
            //writeln('TComponentPalette.UpdateNoteBookButtons Created Button: ',CurComponent.ComponentClass.ClassName,' ',CurComponent.Button.Name);
          end;
        end else if CurComponent.Button<>nil then begin
          //writeln('TComponentPalette.UpdateNoteBookButtons Destroy Button: ',CurComponent.ComponentClass.ClassName,' ',CurComponent.Button.Name);
          CurComponent.Button.Free;
          CurComponent.Button:=nil;
        end;
      end;
      
      ReAlignButtons(CurNoteBookPage);
    end;
    // restore active page
    if (OldActivePage<>'') and (FNoteBook.Pages.IndexOf(OldActivePage)>=0) then
    begin
      FNoteBook.ActivePage:=OldActivePage;
    end else if FNoteBook.PageCount>0 then begin
      FNoteBook.PageIndex:=0;
    end;
  finally
    // unlock
    fUpdatingNotebook:=false;
    fNoteBookNeedsUpdate:=false;
    FNoteBook.EnableAlign;
  end;
  //writeln('TComponentPalette.UpdateNoteBookButtons END');
end;

procedure TComponentPalette.OnGetNonVisualCompIcon(Sender: TObject;
  AComponent: TComponent; var Icon: TBitmap);
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

initialization

{$I ../images/components_images.lrs}

end.

