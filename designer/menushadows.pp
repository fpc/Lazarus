unit MenuShadows;

{$mode objfpc}{$H+}

interface

uses
  // FCL + LCL
  Classes, SysUtils, Types, typinfo,
  ActnList, ButtonPanel, Buttons, Controls, Dialogs, StdCtrls, ExtCtrls, Menus,
  Forms, Graphics, ImgList, Themes, LCLType, LCLIntf, LCLProc,
  // LazUtils
  LazUTF8,
  // IdeIntf
  FormEditingIntf, IDEWindowIntf, ComponentEditors, IDEDialogs, PropEdits,
  // IDE
  LazarusIDEStrConsts, MenuDesignerBase, MenuEditorForm, MenuShortcuts, MenuTemplates;

type

  TShadowMenu = class;
  TShadowBox = class;

  { TFake }

  TFake = class(TCustomControl)
  private
    FShadowMenu: TShadowMenu;
    FMinWidth: integer;
  protected
    function GetShouldBeVisible: boolean; virtual; abstract;
    procedure SetVisibilitySizeAndPosition; virtual; abstract;
    procedure TextChanged; override;
    procedure Paint; override;
    class function GetControlClassDefaultSize: TSize; override;
  public
    constructor Create(anOwner: TShadowMenu); reintroduce;
    procedure Refresh;
    property ShouldBeVisible: boolean read GetShouldBeVisible;
  end;

  TAddSiblingFake = class(TFake)
  protected
    function GetShouldBeVisible: boolean; override;
    procedure SetVisibilitySizeAndPosition; override;
  end;

  TAddSubmenuFake = class(TFake)
  protected
    function GetShouldBeVisible: boolean; override;
    procedure SetVisibilitySizeAndPosition; override;
  end;

  TAddFirstFake = class(TFake)
  protected
    function GetShouldBeVisible: boolean; override;
    procedure SetVisibilitySizeAndPosition; override;
  end;

  TShadowItemDisplayState = (dsNormal, dsSelected, dsDisabled);

  TMenuDesigner = class;

  { TShadowItem }

  TShadowItem = class(TShadowItemBase)
  strict private
    FBottomFake: TFake;
    FParentBox: TShadowBox;
    FRightFake: TFake;
    FShadowMenu: TShadowMenu;
    FShowingBottomFake: boolean;
    FShowingRightFake: boolean;
    FState: TShadowItemDisplayState;
    function GetBitmapLeftTop: TPoint;
    function GetBottomFake: TFake;
    function GetIconTopLeft: TPoint;
    function GetIsInMenuBar: boolean;
    function GetIsMainMenu: boolean;
    function GetLevel: integer;
    function GetRightFake: TFake;
    function GetShortcutWidth: integer;
    function GetShowingBottomFake: boolean;
    function GetShowingRightFake: boolean;
    function GetSubImagesIconTopLeft: TPoint;
    procedure RecursiveHideChildren(aMI: TMenuItem);
    procedure SetState(AValue: TShadowItemDisplayState);
  private
    function GetHeight: integer;
    function GetWidth: integer;
    function HasChildBox(out aChildBox: TShadowBox): boolean;
    procedure HideChainFromRoot;
    procedure HideChildren;
    procedure ShowChainToRoot;
    procedure ShowChildBox;
    procedure ShowDisabled;
    procedure ShowNormal;
    procedure ShowSelected;
  protected
    procedure DblClick; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
  public
    constructor CreateWithBoxAndItem(aSMenu: TShadowMenu; aParentBox: TShadowBox;
      aRealItem: TMenuItem);
    property BottomFake: TFake read GetBottomFake write FBottomFake;
    property IsInMenuBar: boolean read GetIsInMenuBar;
    property IsMainMenu: boolean read GetIsMainMenu;
    property Level: integer read GetLevel;
    property ParentBox: TShadowBox read FParentBox;
    property RightFake: TFake read GetRightFake write FRightFake;
    property ShowingBottomFake: boolean read GetShowingBottomFake write FShowingBottomFake;
    property ShowingRightFake: boolean read GetShowingRightFake write FShowingRightFake;
    //property State: TShadowItemDisplayState read FState;
  end;

  { TShadowBox }

  TShadowBox = class(TShadowBoxBase)
  strict private
    FParentBox: TShadowBox;
    FShadowMenu: TShadowMenu;
    FUpdating: boolean;
    function GetIsMainMenu: boolean;
    function GetIsMenuBar: boolean;
    function GetShadowCount: integer;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure ShowAllUnSelected;
  private
    function GetInnerDims: TPoint;
    procedure AddItemAndShadow(existingSI: TShadowItem; addBefore: boolean;
      isSeparator: boolean=False);
    procedure LocateShadows;
    procedure RemoveAllSeparators;
    procedure SelectPrevious(aSI: TShadowItem);
    procedure SelectSuccessor(aSI: TShadowItem);
    property IsMainMenu: boolean read GetIsMainMenu;
    property IsMenuBar: boolean read GetIsMenuBar;
    property ParentBox: TShadowBox read FParentBox;
    property ShadowCount: integer read GetShadowCount;
    property Updating: boolean read FUpdating;
  protected
    procedure Paint; override;
  public
    constructor CreateWithParentBox(aSMenu: TShadowMenu; aParentBox: TShadowBox;
      aParentItem: TMenuItem);
    destructor Destroy; override;
    procedure SetUnCheckedAllExcept(aMI: TMenuItem);
  end;

  TPopEnum = {%region}
    (popItemMoveBefore, popItemMoveAfter,
     popSeparators_,
       popAddSeparatorBefore, popAddSeparatorAfter, popRemoveAllSeparators,
     popItemDelete, popItemAddBefore, popItemAddAfter, popItemAddSubMenu,
     popItemSep,
     popAddImgListIcon, popItemAddOnClick, popItemEditCaption,
     popItemOISep,
     popShortcuts_,
       popListShortcuts, popListShortcutsAccelerators, popResolveShortcutConflicts,
     popTemplates_,
       popSaveAsTemplate, popAddFromTemplate, popDeleteTemplate);{%endregion}

  { TShadowMenu }

  TShadowMenu = class(TShadowMenuBase)
  strict private
    FActionList: TActionList;
    FAddImgListIconAction: TAction;
    FAddItemFake: TFake;
    FAddFirstItemFake: TFake;
    FAddSubmenuFake: TFake;
    FBoxList: TFPList;
    FInitialising: boolean;
    FInitialSelectedMenuItem: TMenuItem;
    FIsMainMenu: boolean;
    FItemsPopupMenu: TPopupMenu;
    FLookupRoot: TComponent;
    FMainCanvas: TCanvas;
    FRootBox: TShadowBox;
    procedure DeleteBox(aMI: TMenuItem);
    procedure DeleteItm(anItem: TMenuItem);
    function GetActionForEnum(anEnum: TPopEnum): TAction;
    function GetBoxContainingMenuItem(aMI: TMenuItem): TShadowBox;
    function GetHighestLevelVisibleBox: TShadowBox;
    function GetMaxVisibleBoxDims(aSB: TShadowBox): TPoint;
    function GetMaxVisibleFakeDims: TPoint;
    function GetMenuBarCumWidthForItemIndex(anIndex: integer): integer;
    function GetParentItemHeightInBox(aParentItem: TMenuItem): integer;
    function GetSelectedShadowBox: TShadowBox;
    function GetSelectedShadowItem: TShadowItem;
    procedure AddManyItems(aPrimaries, aDepth: integer);
    procedure AddSubMenuTo(anExistingSI: TShadowItem);
    procedure ConnectSpeedButtonOnClickMethods;
    procedure CreateShadowBoxesAndItems;
    procedure DeleteChildlessShadowAndItem(anExistingSI: TShadowItem);
    procedure DeleteShadowAndItemAndChildren(anExistingSI: TShadowItem);
    procedure OnDesignerModified(Sender: TObject);
    procedure OnObjectPropertyChanged(Sender: TObject; NewObject: TPersistent);
    procedure OnDesignerRefreshPropertyValues;
    procedure RecursiveCreateShadows(aParentBox: TShadowBox; aMI: TMenuItem);
    procedure SetupPopupMenu;
    procedure UpdateButtonGlyphs(isInBar: boolean);
    // user actions
    procedure AddFromTemplate(Sender: TObject);
    procedure AddImageListIcon(Sender: TObject);
    procedure AddItemAfter(Sender: TObject);
    procedure AddItemBefore(Sender: TObject);
    procedure AddSeparatorAbove(Sender: TObject);
    procedure AddSeparatorBelow(Sender: TObject);
    procedure AddSubMenu(Sender: TObject);
    procedure AddFirstMenu(Sender: TObject);
    procedure DeleteTemplate(Sender: TObject);
    procedure EditCaption(Sender: TObject);
    procedure ListShortcuts(Sender: TObject);
    procedure ListShortcutsAndAccelerators(Sender: TObject);
    procedure MoveItemAfter(Sender: TObject);
    procedure MoveItemBefore(Sender: TObject);
    procedure RemoveAllSeparators(Sender: TObject);
    procedure ResolveShortcutConflicts(Sender: TObject);
    procedure SaveAsTemplate(Sender: TObject);
  private
    FMenu: TMenu;
    FDesigner: TMenuDesigner;
    FEditorDesigner: TComponentEditorDesigner;
    function GetStringWidth(const aText: string; isBold: boolean): integer;
    function GetMenuBarIconWidth(aMI: TMenuItem): integer;
    function OnClickIsAssigned(aMI: TMenuItem): boolean;
    procedure AddOnClick(Sender: TObject);
    procedure DeleteItem(Sender: TObject);
    function GetBoxWithParentItem(aParentMI: TMenuItem): TShadowBox;
    procedure HideFakes;
    procedure RemoveEmptyBox(aSB: TShadowBox);
    procedure SetSelectedShadow(const prevSelectedItem, curSelectedItem: TMenuItem; viaDesigner: boolean);
    procedure UpdateActionsEnabledness;
  private
    property AddItemFake: TFake read FAddItemFake;
    property AddSubmenuFake: TFake read FAddSubmenuFake;
    property BoxList: TFPList read FBoxList;
    property ItemsPopupMenu: TPopupMenu read FItemsPopupMenu;
    property RootBox: TShadowBox read FRootBox;
  protected
    procedure Paint; override;
    procedure SetParent(NewParent: TWinControl); override;
  public
    constructor Create(aDesigner: TMenuDesigner; aForm: TForm; aMenu: TMenu;
      aSelect: TMenuItem; aWidth, aHeight: integer); reintroduce;
    destructor Destroy; override;
    function GetParentBoxForMenuItem(aMI: TMenuItem): TShadowBox;
    function GetShadowForMenuItem(aMI: TMenuItem): TShadowItem;
    procedure HideBoxesAboveLevel(aLevel: integer);
    procedure RefreshFakes;
    procedure SetSelectedMenuItem(aMI: TMenuItem;
      viaDesigner, prevWasDeleted: boolean); override;
    procedure UpdateBoxLocationsAndSizes;
    procedure UpdateSelectedItemInfo;
  public
    property EditorDesigner: TComponentEditorDesigner read FEditorDesigner;
    property IsMainMenu: boolean read FIsMainMenu;
    property LookupRoot: TComponent read FLookupRoot;
    property SelectedShadowBox: TShadowBox read GetSelectedShadowBox;
    property SelectedShadowItem: TShadowItem read GetSelectedShadowItem;
  end;

  { TMenuDesigner }

  TMenuDesigner = class(TMenuDesignerBase)
  private
    FGui: TMenuDesignerForm;
  public
    constructor Create;
    destructor Destroy; override;
    procedure CreateShadowMenu(aMenu: TMenu; aSelect: TMenuItem;
      aWidth, aHeight: integer); override;
  end;

  { TMenuComponentEditor - the default component editor for TMenu }

  TMainMenuComponentEditor = class(TComponentEditor)
  public
    procedure Edit; override;
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  { TMenuItemsPropertyEditor - property editor for TMenuItem properties.
    Invokes the parent menu's component editor }

  TMenuItemsPropertyEditor = class(TClassPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

procedure ShowMenuEditor(aMenu: TMenu);
function MenuDesigner: TMenuDesigner;


implementation

const
  Shortcut_Offset = 23;
  Popup_Origin: TPoint = (x:15; y:15);

var
  ShadowItemID: integer = 0;
  ShadowBoxID: integer = 0;

  MenuDesignerSingleton: TMenuDesigner = nil;

procedure ShowMenuEditor(aMenu: TMenu);
begin
  if (aMenu = nil) then
    RaiseGDBException(lisMenuEditorShowMenuEditorTMenuParameterIsNil);
  MenuDesigner.FGui.SetMenu(aMenu, nil);
  SetPopupModeParentForPropertyEditor(MenuDesigner.FGui);
  MenuDesigner.FGui.ShowOnTop;
end;

function MenuDesigner: TMenuDesigner; // refer always to a single instance
begin
  if (MenuDesignerSingleton = nil) then
    MenuDesignerSingleton:=TMenuDesigner.Create;
  Result:=MenuDesignerSingleton;
end;


type

  { TLineEditor }

  TLineEditor = class(TForm)
  strict private
    FEdit: TEdit;
    function GetEditedLine: string;
  protected
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
    constructor CreateWithShadowItem(aSI: TShadowItem);
    property EditedLine: string read GetEditedLine;
  end;

  { TResolveConflictsDlg }

  TResolveConflictsDlg = class(TForm)
  strict private
    FButtonPanel: TButtonPanel;
    FConflictsGroupBox: TGroupBox;
    FConflictsListBox: TListBox;
    FCurrentEdit: TEdit;
    FInitialConflictsCount: integer;
    FRemainingConflictsCountLabel: TLabel;
    FResolvedConflictsCount: integer;
    FResolvedConflictsCountLabel: TLabel;
    FSelectedDuplicate: TSCInfo;
    FSelectedInfo: TSCInfo;
    FSelectedUnique: TSCInfo;
    FShortcuts: TMenuShortcuts;
    FShadowMenu: TShadowMenu;
    procedure ConflictsBoxSelectionChange(Sender: TObject; {%H-}User: boolean);
    procedure CreateListboxItems;
    procedure InitialPopulateListBox;
    procedure OKButtonClick(Sender: TObject);
    procedure RePopulateListBox;
    procedure UpdateStatistics;
  public
    constructor Create(aShortcuts: TMenuShortcuts; aShadowMenu: TShadowMenu); reintroduce;
    destructor Destroy; override;
  end;

  { TShortcutDisplayDlg }

  TShortcutDisplayDlg = class(TForm)
  strict private
    FLastSortIndex: integer;
    FMenu: TMenu;
    FscList: TStringList;
    FSingleMenuOnly: boolean;
    FShortcutsOnly: boolean;
    FShortcuts: TMenuShortcuts;
    FShadowMenu: TShadowMenu;
    // GUI
    FBPanel: TButtonPanel;
    FDualDisplay: TDualDisplay;
    FGBDisplay: TGroupBox;
    FLabel: TLabel;
    procedure AddSCitemToListRecursive(anItem: TMenuItem);
    procedure DisplayAllDlgClick(isHeader: boolean; index: integer);
    procedure DisplaySingleMenuClick(isHeader: boolean; index: integer);
    procedure UpdateContents(singleMenuOnly: boolean=False);
    procedure UpdateFromMenu(anIndex: integer= -1);
  public
    constructor CreateWithShortcutsOnly(aShortcuts: TMenuShortcuts; shortcutsOnly: boolean;
                                        aShadowMenu: TShadowMenu; aMenu: TMenu=nil);
    destructor Destroy; override;
  end;

// utility functions

function ItemStateToStr(aState: TShadowItemDisplayState): string;
begin
  Result:=GetEnumName(TypeInfo(TShadowItemDisplayState), Ord(aState));
end;

function GetPreviousNonSepItem(aMI: TMenuItem): TMenuItem;
var
  idx: integer;
begin
  Result:=nil;
  idx:=aMI.MenuIndex;
  if (idx = 0) then
    Exit
  else repeat
    idx:=Pred(idx);
    Result:=aMI.Parent.Items[idx];
  until not Result.IsLine or (idx = 0);
  if Result.IsLine then
    Result:=nil;
end;

function GetPreviousItem(aMI: TMenuItem): TMenuItem;
var
  idx: integer;
begin
  idx:=aMI.MenuIndex;
  if (idx = 0) then
    Exit(nil)
  else
    Result:=aMI.Parent.Items[Pred(idx)];
end;

function GetNextItem(aMI: TMenuItem): TMenuItem;
var
  idx: integer;
begin
  idx:=aMI.MenuIndex;
  if (idx = Pred(aMI.Parent.Count)) then
    Exit(nil)
  else
    Result:=aMI.Parent.Items[Succ(idx)];
end;

function GetNextNonSepItem(aMI: TMenuItem): TMenuItem;
var
  idx, maxIdx: integer;
begin
  Result:=nil;
  idx:=aMI.MenuIndex;
  maxIdx:=Pred(aMI.Parent.Count);
  if (idx = maxIdx) then
    Exit
  else repeat
    idx:=Succ(idx);
    Result:=aMI.Parent.Items[idx];
  until not Result.IsLine or (idx = maxIdx);
  if Result.IsLine then
    Result:=nil;
end;

function GetNewCaptionFor(aSI: TShadowItem): string;
var
  dlg: TLineEditor;
  mr: TModalResult;
begin
  dlg:=TLineEditor.CreateWithShadowItem(aSI);
  try
    mr:=dlg.ShowModal;
    if (mr = mrOK) then
      Result:=dlg.EditedLine
    else
      Result:='';
  finally
    dlg.Free;
  end;
end;

function PreviousItemIsSeparator(aMI: TMenuItem): boolean;
var
  idx: integer;
begin
  if (aMI = nil) then
    Exit(False);
  idx:=aMI.MenuIndex;
  Result:=(idx > 0) and aMI.Parent.Items[Pred(idx)].IsLine;
end;

function NextItemIsSeparator(aMI: TMenuItem): boolean;
var
  idx: integer;
begin
  if (aMI = nil) then
    Exit(False);
  idx:=aMI.MenuIndex;
  Result:=(idx < Pred(aMI.Parent.Count)) and aMI.Parent.Items[Succ(idx)].IsLine;
end;

function GetChildSeparatorCount(aMI: TMenuItem): integer;
var
  i: integer;
begin
  Result:=0;
  for i:=0 to aMI.Count-1 do
    if aMI.Items[i].IsLine then
      Inc(Result);
end;

{function CommaTextIntoTwo(const aText: string; out a2ndHalf: string): string;
var
  p, len: integer;
begin
  a2ndHalf:='';
  len:=Length(aText);
  if (len = 0) then
    Exit('');
  p:=Pos(',', aText);
  if (p = 0) then
    Exit(aText);
  Result:=Copy(aText, 1, Pred(p));
  a2ndHalf:=Copy(aText, Succ(p), len - p);
end;}

function AIsDescendantOfB(miA, miB: TMenuItem): boolean;
var
  tmp: TMenuItem;
begin
  if (miA = nil) or (miB = nil) then
    Exit(False);
  tmp:=miA.Parent;
  repeat
    if (tmp = miB) then
      Exit(True);
    tmp:=tmp.Parent;
  until (tmp = nil);
  Result:=False;
end;

function LevelZeroAndNoGrandchildren(aMI: TMenuItem): boolean;
var
  i: integer;
begin
  Result:=(aMI.Parent <> nil) and (aMI.Parent.Parent = nil);
  if Result then
    for i:=0 to aMI.Count-1 do
      if (aMI.Items[i].Count > 0) then
        Exit(False);
end;

function SortByItemMenuIndex(Item1, Item2: Pointer): Integer;
var
  si1: TShadowItem absolute Item1;
  si2: TShadowItem absolute Item2;
  i1, i2: integer;
begin
  i1:=si1.RealItem.MenuIndex;
  i2:=si2.RealItem.MenuIndex;
  if (i1 > i2) then
    Result:=1
  else if (i2 > i1) then
    Result:= -1
  else
    Result:=0;
end;

function SortByBoxLevel(Item1, Item2: Pointer): Integer;
var
  sb1: TShadowBox absolute Item1;
  sb2: TShadowBox absolute Item2;
  lvl1, lvl2: integer;
begin
  lvl1:=sb1.Level;
  lvl2:=sb2.Level;
  if (lvl1 > lvl2) then
    Result:=1
  else if (lvl1 < lvl2) then
    Result:= -1
  else
    Result:=0;
end;
{
function ResolvedConflictsDlg: TModalResult;
var
  dlg: TResolveConflictsDlg;
begin
  dlg:=TResolveConflictsDlg.Create(nil);
  try
    Result:=dlg.ShowModal;
  finally
    dlg.Free;
  end;
end;
}
function ListShortCutDlg(aShortcuts: TMenuShortcuts; shortcutsOnly: boolean;
  aShadowMenu: TShadowMenu; aMenu: TMenu): TModalResult;
var
  dlg: TShortcutDisplayDlg;
begin
  dlg:=TShortcutDisplayDlg.CreateWithShortcutsOnly(aShortcuts, shortcutsOnly,
                                                   aShadowMenu, aMenu);
  try
    Result:=dlg.ShowModal;
  finally
    FreeAndNil(dlg);
  end;
end;

function SortByComponent(Item1, Item2: Pointer): Integer;
var
  SCInfo1: TSCInfo absolute Item1;
  SCInfo2: TSCInfo absolute Item2;
begin
  if (SCInfo1.ComponentName > SCInfo2.ComponentName) then
    Result:= +1
  else if (SCInfo1.ComponentName < SCInfo2.ComponentName) then
    Result:= -1
  else
    Result:=0;
end;

{ TAddFirstFake }

function TAddFirstFake.GetShouldBeVisible: boolean;
begin
  Result:=(FShadowMenu.FMenu<>nil) and (FShadowMenu.FMenu.Items.Count=0);
end;

procedure TAddFirstFake.SetVisibilitySizeAndPosition;
begin
  if ShouldBeVisible then begin
    SetBounds(Left, Top, FMinWidth, DropDown_Height);
    Show;
  end
  else begin
    Hide;
  end;
end;

{ TAddSubmenuFake }

function TAddSubmenuFake.GetShouldBeVisible: boolean;
var
  item: TMenuItem;
begin
  item:=FShadowMenu.SelectedMenuItem;
  if (item = nil) then
    Exit(False)
  else
    Result:=not item.IsLine and (item.Count = 0);
end;

procedure TAddSubmenuFake.SetVisibilitySizeAndPosition;
var
  selShadow: TShadowItem;
  selMI: TMenuItem;
  w: integer;
begin
  selMI:=FShadowMenu.SelectedMenuItem;
  if (selMI=nil) then
    Exit;
  selShadow:=FShadowMenu.GetShadowForMenuItem(selMI);
  Assert(selShadow<>nil,'TFake.SetVisibilitySizeAndPosition: selectedItem is nil');
  if not ShouldBeVisible then begin
    if selMI.IsInMenuBar then
      selShadow.BottomFake:=nil
    else
      selShadow.RightFake:=nil;
    Hide;
  end
  else begin
    w:=FMinWidth;
    if selMI.IsInMenuBar then begin
      if (selShadow.Width > w) then
        w:=selShadow.Width;
      SetBounds(selShadow.Left, MenuBar_Height + 1, w, MenuBar_Height);
      selShadow.ShowingBottomFake:=True;
      selShadow.BottomFake:=Self;
      selShadow.ShowingRightFake:=False;
    end
    else begin
      SetBounds(selShadow.ParentBox.Left + selShadow.BoundsRect.Right + 1,
                selShadow.ParentBox.Top + selShadow.Top, w, DropDown_Height);
      selShadow.ShowingRightFake:=True;
      selShadow.RightFake:=Self;
      selShadow.ShowingBottomFake:=False;
    end;
    Show;
  end;
end;

{ TAddSiblingFake }

function TAddSiblingFake.GetShouldBeVisible: boolean;
var
  item: TMenuItem;
begin
  item:=FShadowMenu.SelectedMenuItem;
  if (item = nil) then
    Exit(False)
  else
    Result:=(item.MenuIndex = Pred(item.Parent.Count));
end;

procedure TAddSiblingFake.SetVisibilitySizeAndPosition;
var
  selShadow: TShadowItem;
  selMI: TMenuItem;
  w: integer;
begin
  selMI:=FShadowMenu.SelectedMenuItem;
  if (selMI=nil) then
    Exit;
  selShadow:=FShadowMenu.GetShadowForMenuItem(selMI);
  Assert(selShadow<>nil,'TFake.SetVisibilitySizeAndPosition: selectedItem is nil');
  if not ShouldBeVisible then begin
    if selMI.IsInMenuBar then
      selShadow.RightFake:=nil
    else
      selShadow.BottomFake:=nil;
    Hide;
  end
  else begin
    if selMI.IsInMenuBar then begin
      SetBounds(selShadow.Left + selShadow.Width + 1, 0, FMinWidth, MenuBar_Height);
      selShadow.ShowingRightFake:=True;
      selShadow.RightFake:=Self;
      selShadow.ShowingBottomFake:=False;
    end
    else begin
      w:=selShadow.ParentBox.Width - Gutter_X;
      if (FMinWidth > w) then
        w:=FMinWidth;
      SetBounds(selShadow.ParentBox.Left + selShadow.Left + Gutter_X,
                selShadow.ParentBox.Top + selShadow.ParentBox.Height + 1,
                w, DropDown_Height);
      selShadow.ShowingBottomFake:=True;
      selShadow.BottomFake:=Self;
      selShadow.ShowingRightFake:=False;
    end;
    Show;
  end;
end;

{ TResolveConflictsDlg }

constructor TResolveConflictsDlg.Create(aShortcuts: TMenuShortcuts;
  aShadowMenu: TShadowMenu);
begin
  inherited CreateNew(Nil);
  FShortcuts:=aShortcuts;
  FShortcuts.ShortcutList.ScanContainerForShortcutsAndAccelerators;
  FInitialConflictsCount:=FShortcuts.ShortcutList.InitialDuplicatesCount;
  FShadowMenu:=aShadowMenu;
  FResolvedConflictsCount:=0;
  Position:=poScreenCenter;
  Constraints.MinWidth:=400;
  Constraints.MinHeight:=256;
  Caption:=Format(lisMenuEditorMenuItemShortcutConflictsInS,
                  [(GlobalDesignHook.LookupRoot as TComponent).Name]);
  FConflictsGroupBox:=TGroupBox.Create(Self);
  with FConflictsGroupBox do begin
    Caption:=Format(lisMenuEditorConflictsFoundInitiallyD,
      [FShortcuts.ShortcutList.InitialDuplicatesCount]);
    Align:=alTop;
    Top:=0;
    BorderSpacing.Around:=Margin;
    BorderSpacing.Top:=Margin;
    Parent:=Self;
  end;

  FResolvedConflictsCountLabel:=TLabel.Create(Self);
  with FResolvedConflictsCountLabel do begin
    BorderSpacing.Around:=Margin;
    Align:=alTop;
    Top:=1;
    Name:='ResolvedConflictsCountLabel';
    Caption:=Name;
    Parent:=FConflictsGroupBox;
  end;

  FRemainingConflictsCountLabel:=TLabel.Create(Self);
  with FRemainingConflictsCountLabel do begin
    BorderSpacing.Around:=Margin;
    Align:=alTop;
    Top:=2;
    Name:='RemainingConflictsCountLabel';
    Caption:=Name;
    Parent:=FConflictsGroupBox;
  end;

  FConflictsListBox:=TListBox.Create(Self);
  with FConflictsListBox do begin
    Color:=clBtnFace;
    Align:=alTop;
    Top:=3;
    BorderSpacing.Around:=Margin;
    Height:=100;
    Name:='ConflictsListBox';
    OnSelectionChange:=@ConflictsBoxSelectionChange;
    Parent:=FConflictsGroupBox;
  end;

  FCurrentEdit:=TEdit.Create(Self);
  with FCurrentEdit do begin
    Align:=alTop;
    Top:=4;
    BorderSpacing.Around:=Margin;
    ReadOnly:=True;
    Name:='CurrentEdit';
    Text:=Name;
    Parent:=FConflictsGroupBox;
  end;

  FConflictsGroupBox.AutoSize:=True;

  FButtonPanel:=TButtonPanel.Create(Self);
  with FButtonPanel do begin
    Align:=alTop;
    Top:=1;
    BorderSpacing.Right:=Margin;
    ShowBevel:=False;
    ShowButtons:=[pbOK, pbCancel];
    ShowGlyphs:=[pbClose];
    OKButton.Enabled:=False;
    OKButton.ModalResult:=mrNone;
    OKButton.Caption:=lisMenuEditorResolveSelectedConflict;
    OKButton.OnClick:=@OKButtonClick;
    Parent:=Self;
  end;

  InitialPopulateListBox;
  AutoSize:=True;
end;

destructor TResolveConflictsDlg.Destroy;
begin
  inherited Destroy;
end;

procedure TResolveConflictsDlg.ConflictsBoxSelectionChange(Sender: TObject; User: boolean);
begin
  if (FConflictsListBox.ItemIndex < 0) then
    Exit;
  FSelectedDuplicate:=TSCInfo(FConflictsListBox.Items.Objects[FConflictsListBox.ItemIndex]);
  Assert(FSelectedDuplicate<>nil,'TResolveConflictsDlg.ConflictsBoxSelectionChange: FSelectedDuplicate is nil');
  FSelectedUnique:=FShortcuts.ShortcutList.FindUniqueInfoForShortcut(FSelectedDuplicate.Shortcut);
  Assert(FSelectedDuplicate<>nil,'TResolveConflictsDlg.ConflictsBoxSelectionChange: FSelectedDuplicate is nil');
  Assert(FSelectedUnique<>nil,'TResolveConflictsDlg.ConflictsBoxSelectionChange: FSelectedUnique is nil');
  if (FSelectedDuplicate.Kind in MenuItem_Kinds) then
    FSelectedInfo:=FSelectedDuplicate
  else if (FSelectedUnique.Kind in MenuItem_Kinds) then
    FSelectedInfo:=FSelectedUnique
  else FSelectedInfo:=FSelectedDuplicate;
  FCurrentEdit.Text:=Format(lisMenuEditorEditingSdotS,
         [FSelectedInfo.ComponentName, KindToPropertyName(FSelectedInfo.Kind)]);
  FButtonPanel.OKButton.Enabled:=True;
end;

procedure TResolveConflictsDlg.CreateListboxItems;
var
  sUnique: string;
  sDup: string;
  infUnique: TSCInfo;
  p: pointer;
  infDup: TSCInfo absolute p;
begin
  FConflictsListBox.OnSelectionChange:=nil;
  FConflictsListBox.Items.Clear;
  for p in FShortcuts.ShortcutList.InitialDuplicates do begin
    sDup:=Format(lisMenuEditorSInS, [ShortCutToText(infDup.Shortcut),
      infDup.ComponentName]);
    infUnique:=FShortcuts.ShortcutList.FindUniqueInfoForShortcut(infDup.Shortcut);
    Assert(infUnique<>nil,'TResolveConflictsDlg.PopulateListBox: missing unique shortcut');
    sUnique:=Format(lisMenuEditorSInS, [ShortCutToText(infUnique.Shortcut),
      infUnique.ComponentName]);
    FConflictsListBox.Items.AddObject(Format(lisMenuEditorSConflictsWithS, [
      sDup, sUnique]), infDup);
  end;
  FConflictsListBox.OnSelectionChange:=@ConflictsBoxSelectionChange;
end;

procedure TResolveConflictsDlg.OKButtonClick(Sender: TObject);
var
  newShortcut: TShortCut;
  newCaption: string;
  si: TShadowItem = nil;

  procedure AddSecondaryShortcut;
  var
    scList: TShortCutList;
  begin
    scList:=TShortCutList.Create;
    try
      scList.Add(ShortCutToText(newShortcut));
      TAction(FSelectedInfo.Component).SecondaryShortCuts.Assign(scList);
    finally
      scList.Free;
    end;
  end;

begin
  Assert(FSelectedInfo<>nil,'TShortcutScanDlg.ResolveConflictClick: FSelectedInfo is nil');
  if NewShortcutOrCaptionIsValidDlg(FSelectedInfo, newShortcut, newCaption) then
    begin
      case FSelectedInfo.Kind of
        scMenuItemAccel:   FSelectedInfo.MenuItem.Caption:=newCaption;
        scMenuItemSC:      FSelectedInfo.MenuItem.ShortCut:=newShortcut;
        scMenuItemKey2:    FSelectedInfo.MenuItem.ShortCutKey2:=newShortcut;
        scActionAccel:     TAction(FSelectedInfo.Component).Caption:=newCaption;
        scActionSC:        TAction(FSelectedInfo.Component).ShortCut:=newShortcut;
        scActionSecondary: AddSecondaryShortcut;
        scOtherCompAccel:  TControl(FSelectedInfo.Component).Caption:=newCaption;
      end;
      if (FSelectedInfo.Kind in MenuItem_Kinds) then begin
        FShadowMenu.EditorDesigner.PropertyEditorHook.RefreshPropertyValues;
        FShadowMenu.EditorDesigner.Modified;
        si:=FShadowMenu.GetShadowForMenuItem(FSelectedInfo.MenuItem);
        if (si <> nil) then begin
          FShadowMenu.UpdateBoxLocationsAndSizes;
          si.Repaint;
        end;
      end
      else case FSelectedInfo.Kind of
        scActionAccel, scActionSC, scActionSecondary: begin
            GlobalDesignHook.RefreshPropertyValues;
            GlobalDesignHook.Modified(TAction(FSelectedInfo.Component));
          end;
        scOtherCompAccel: begin
            GlobalDesignHook.RefreshPropertyValues;
            GlobalDesignHook.Modified(TControl(FSelectedInfo.Component));
          end;
      end;

      RePopulateListBox;
    end;
end;

procedure TResolveConflictsDlg.InitialPopulateListBox;
begin
  if (FShortcuts.ShortcutList.InitialDuplicatesCount > 0) then begin
    FResolvedConflictsCount:=0;
    FResolvedConflictsCountLabel.Caption:=Format(
      lisMenuEditorResolvedConflictsS, [IntToStr(FResolvedConflictsCount)]);
    CreateListboxItems;
    FRemainingConflictsCountLabel.Caption:=Format(
      lisMenuEditorRemainingConflictsS, [IntToStr(FConflictsListBox.Count)]);
    FConflictsListBox.ItemIndex:=0;
  end
  else begin
    FButtonPanel.OKButton.Enabled:=False;
    FSelectedInfo:=nil;
    FConflictsListBox.OnSelectionChange:=nil;
    FConflictsListBox.Items.Add(lisMenuEditorNoShortcutConflicts);
    FCurrentEdit.Text:=lisMenuEditorNoShortcutConflicts;
    FResolvedConflictsCountLabel.Caption:=Format(lisMenuEditorResolvedConflictsS,['0']);
    FRemainingConflictsCountLabel.Caption:=Format(lisMenuEditorRemainingConflictsS,['0']);
  end;
end;

procedure TResolveConflictsDlg.RePopulateListBox;
begin
  FConflictsListBox.OnSelectionChange:=nil;
  FConflictsListBox.Items.Clear;
  FConflictsListBox.ItemIndex:= -1;
  FButtonPanel.OKButton.Enabled:=False;
  FShortcuts.ShortcutList.ScanContainerForShortcutsAndAccelerators;
  if (FShortcuts.ShortcutList.InitialDuplicatesCount > 0) then begin
    CreateListboxItems;
    UpdateStatistics;
    FConflictsListBox.ItemIndex:=0;
    FConflictsListBox.OnSelectionChange:=@ConflictsBoxSelectionChange;
  end
  else begin
    FButtonPanel.OKButton.Enabled:=False;
    FSelectedInfo:=nil;
    FConflictsListBox.OnSelectionChange:=nil;
    FRemainingConflictsCountLabel.Caption:=Format(lisMenuEditorRemainingConflictsS,['0']);
    FResolvedConflictsCountLabel.Caption:=Format(
      lisMenuEditorResolvedConflictsS, [FInitialConflictsCount]);
    FConflictsListBox.Items.Add(lisMenuEditorNoShortcutConflicts);
    FCurrentEdit.Text:=lisMenuEditorConflictResolutionComplete;
    FButtonPanel.CancelButton.Caption:=lisBtnClose;
  end;
end;

procedure TResolveConflictsDlg.UpdateStatistics;
begin
  FResolvedConflictsCount:=FInitialConflictsCount - FConflictsListBox.Count;
  FResolvedConflictsCountLabel.Caption:=Format(lisMenuEditorResolvedConflictsS,
    [IntToStr(FResolvedConflictsCount)]);
  FRemainingConflictsCountLabel.Caption:=Format(
    lisMenuEditorRemainingConflictsS, [IntToStr(FInitialConflictsCount-FResolvedConflictsCount)]);
end;

{ TShortcutDisplayDlg }

constructor TShortcutDisplayDlg.CreateWithShortcutsOnly(aShortcuts: TMenuShortcuts;
  shortcutsOnly: boolean; aShadowMenu: TShadowMenu; aMenu: TMenu);
var
  s: string;
  lurStr: string;
begin
  inherited CreateNew(nil);
  FShortcutsOnly:=shortcutsOnly;
  FShortcuts:=aShortcuts;
  FMenu:=aMenu;
  FShadowMenu:=aShadowMenu;
  FSingleMenuOnly:=(FMenu <> nil);
  FLastSortIndex:= -1;
  if FSingleMenuOnly then
    Caption:=Format(lisMenuEditorShortcutsUsedInS, [FMenu.Name])
  else begin
    if shortcutsOnly then
      s:=lisMenuEditorSShortcuts
    else
      s:=lisMenuEditorSShortcutsAndAcceleratorKeys;
    lurStr:=TComponent(GlobalDesignHook.LookupRoot).Name;
    Caption:=Format(s, [lurStr]);
  end;

  BorderStyle:=bsDialog;
  Position:=poScreenCenter;
  Constraints.MinWidth:=460;
  Constraints.MaxHeight:=460;

  FBPanel:=TButtonPanel.Create(Self);
  with FBPanel do begin
    ShowBevel:=False;
    BorderSpacing.Around:=Spacing;
    BorderSpacing.Right:=Spacing;
    Constraints.MinHeight:=42;
    ShowButtons:=[pbClose];
    CloseButton.Constraints.MaxHeight:=30;
    Parent:=Self;
  end;

  FLabel:=TLabel.Create(Self);
  FLabel.WordWrap:=True;
  FLabel.Constraints.MaxWidth:=340;
  FLabel.BorderSpacing.Left:=Margin;
  FLabel.AutoSize:=True;
  FLabel.Parent:=FBPanel;

  FGBDisplay:=TGroupBox.Create(Self);
  with FGBDisplay do begin
    Align:=alClient;
    BorderSpacing.Around:=Margin*2;
    AutoSize:=True;
    Parent:=Self;
  end;

  FDualDisplay:=TDualDisplay.Create(Self);
  with FDualDisplay do begin
    Align:=alClient;
    BorderSpacing.Around:=Margin;
    Parent:=FGBDisplay;
    if FSingleMenuOnly then begin
      OnDisplayClick:=@DisplaySingleMenuClick;
      AddHeader(lisMenuEditorShortcutSourceProperty);
      Caption:=lisMenuEditorShortcuts;
    end
    else begin
      OnDisplayClick:=@DisplayAllDlgClick;
      if FShortcutsOnly then begin
        AddHeader(lisMenuEditorShortcutSourceProperty);
        Caption:=Format(lisMenuEditorSShortcuts, [lurStr]);
      end
      else begin
        AddHeader(lisMenuEditorShortcutSourceProperty);
        Caption:=Format(lisMenuEditorSShortcutsAndAcceleratorKeys, [lurStr]);
      end;
    end;
  end;
  UpdateContents(FSingleMenuOnly);
  FLabel.Caption:=lisMenuEditorClickANonGreyedItemToEditItsShortcut;
  AutoSize:=True;
end;

destructor TShortcutDisplayDlg.Destroy;
begin
  FreeAndNil(FscList);
  inherited Destroy;
end;

procedure TShortcutDisplayDlg.DisplaySingleMenuClick(isHeader: boolean; index: integer);
var
  mi: TMenuItem;
  sc: TShortCut;
  result: boolean;
  si: TShadowItem;
  info: TSCInfo;
  isMainSC: boolean;
begin
  case isHeader of

    True: begin // header click
      if (FLastSortIndex = index) or (FscList.Count = 0) then
        Exit;
      FLastSortIndex:=index;
      UpdateFromMenu(index);
    end;

    False: begin // contents click
      info:=TSCInfo(FscList.Objects[index]);
      mi:=info.MenuItem;
      if (mi <> nil) then
      begin
        sc:=info.Shortcut;
        isMainSC:=(info.Kind = scMenuItemSC);
        result:=AddNewOrEditShortcutDlg(mi, isMainSC, sc);
        if result then
        begin
          if isMainSC then
            mi.ShortCut:=sc
          else
            mi.ShortCutKey2:=sc;
          si:=FShadowMenu.GetShadowForMenuItem(mi);
          if (si <> nil) then begin
            FShadowMenu.UpdateBoxLocationsAndSizes;
            si.Repaint;
          end;
          FShortcuts.UpdateShortcutList(False);
          UpdateFromMenu;
          GlobalDesignHook.RefreshPropertyValues;
          GlobalDesignHook.Modified(mi);
          FShadowMenu.RefreshFakes;
        end;
      end;
    end;

  end; // case
end;

procedure TShortcutDisplayDlg.DisplayAllDlgClick(isHeader: boolean; index: integer);
var
  i: integer;
  dt: TDisplayType;
  inf: TSCInfo;
  mi: TMenuItem;
  sc: TShortCut;
  isMainSC, isCaptionSC, result: boolean;
  si: TShadowItem;
begin
  case isHeader of
    True: begin
      if (FLastSortIndex = index) or (FDualDisplay.ContentsCount = 0) then
        Exit;
      FLastSortIndex:=index;
      FDualDisplay.ClearContents;
      case index of
        0: FShortcuts.ShortcutList.ScanList.Sort;
        1: FShortcuts.ShortcutList.SortByComponentPropertyName;
      end;
      for i:=0 to FShortcuts.ShortcutList.ScanList.Count-1 do
        begin
          inf:=TSCInfo(FShortcuts.ShortcutList.ScanList.Objects[i]);
          if (inf.Kind in MenuItem_Kinds) then
            dt:=dtBlack
          else
            dt:=dtGreyed;
          FDualDisplay.AddLine(FShortcuts.ShortcutList.ScanList[i] + ',' +
                     inf.Component.Name+ '.' + KindToPropertyName(inf.Kind), dt);
        end;
    end;
    False: begin
      inf:=TSCInfo(FShortcuts.ShortcutList.ScanList.Objects[index]);
      mi:=inf.MenuItem;
      if (mi <> nil) then
        begin
          isMainSC:=(inf.Kind in ShortcutOnly_Kinds);
          isCaptionSC:=(inf.Kind in Accelerator_Kinds);
          sc:=inf.Shortcut;
          if isCaptionSC then
            result:=EditCaptionDlg(mi, sc)
          else result:=AddNewOrEditShortcutDlg(mi, isMainSC, sc);
          if result then
            begin
              if not isCaptionSC and isMainSC then
                mi.ShortCut:=sc
              else if not isCaptionSC then
                mi.ShortCutKey2:=sc;
              si:=FShadowMenu.GetShadowForMenuItem(mi);
              if (si <> nil) then
              begin
                FShadowMenu.UpdateBoxLocationsAndSizes;
                si.Repaint;
              end;
              FShortcuts.UpdateShortcutList(True);
              UpdateContents;
              GlobalDesignHook.RefreshPropertyValues;
              GlobalDesignHook.Modified(mi);
              FShadowMenu.RefreshFakes;
            end;
        end;
    end;
  end; // case
end;

procedure TShortcutDisplayDlg.UpdateContents(singleMenuOnly: boolean);
var
  i: integer;
  dt: TDisplayType;
  kind: TSCKind;
  inf: TSCInfo;
begin
  FDualDisplay.ClearContents;
  if singleMenuOnly then
    UpdateFromMenu
  else begin
    FShortcuts.UpdateShortcutList(not FShortcutsOnly);
    if (FShortcuts.ShortcutList.ScanList.Count = 0) then
      begin
        FDualDisplay.AddLine(lisMenuEditorNoneNone, dtGreyed);
        if FShortcutsOnly then
          FGBDisplay.Caption:=lisMenuEditorShortcuts
        else
          FGBDisplay.Caption:=lisMenuEditorShortcutsAndAcceleratorKeys;
      end
    else
      begin
        if FShortcutsOnly then
          FGBDisplay.Caption:=Format(lisMenuEditorShortcutsD,
            [FShortcuts.ShortcutList.ShortcutsInContainerCount])
        else
          FGBDisplay.Caption:=Format(lisMenuEditorShortcutsDAndAcceleratorKeysD,
            [FShortcuts.ShortcutList.ShortcutsInContainerCount,
             FShortcuts.ShortcutList.AcceleratorsInContainerCount]);

        FDualDisplay.BeginUpdate;
        for i:=0 to FShortcuts.ShortcutList.ScanList.Count-1 do
          begin
            inf:=TSCInfo(FShortcuts.ShortcutList.ScanList.Objects[i]);
            kind:=inf.Kind;
            if (kind in MenuItem_Kinds) then
              dt:=dtBlack
            else
              dt:=dtGreyed;
            FDualDisplay.AddLine(FShortcuts.ShortcutList.ScanList[i] + ',' +
                         inf.Component.Name + '.' + KindToPropertyName(inf.Kind), dt);
          end;
        FDualDisplay.EndUpdate;
      end;
  end;
end;

procedure TShortcutDisplayDlg.AddSCitemToListRecursive(anItem: TMenuItem);
var
  inf: TSCInfo;
  i: integer;
begin
  if (anItem.ShortCut <> 0) then begin
    inf:=TSCInfo.CreateWithParams(anItem, scMenuItemSC, anItem.ShortCut);
    FscList.AddObject(ShortCutToText(anItem.ShortCut), TObject(inf));
  end;
  if (anItem.ShortCutKey2 <> 0) and (anItem.ShortCutKey2 <> anItem.ShortCut) then begin
    inf:=TSCInfo.CreateWithParams(anItem, scMenuItemKey2, anItem.ShortCutKey2);
    FscList.AddObject(ShortCutToText(anItem.ShortCutKey2), TObject(inf));
  end;
  for i:=0 to anItem.Count-1 do
    AddSCitemToListRecursive(anItem[i]);
end;

procedure TShortcutDisplayDlg.UpdateFromMenu(anIndex: integer);
var
  i: integer;
  inf: TSCInfo;
begin
  Assert(FMenu<>nil,'TShortcutDisplayDlg.UpdateFromMenu: FMenu is nil');
  FreeAndNil(FscList);
  FscList:=TStringList.Create;
  FscList.CaseSensitive:=False;
  FDualDisplay.ClearContents;
  for i:=0 to FMenu.Items.Count-1 do
    AddSCitemToListRecursive(FMenu.Items[i]);
  if (FscList.Count = 0) then
    begin
      FDualDisplay.AddLine(lisMenuEditorNoneNone, dtGreyed);
      FGBDisplay.Caption:=lisMenuEditorShortcuts;
    end
  else
    begin
      FGBDisplay.Caption := Format(lisMenuEditorShortcutsUsedInSD,
          [FMenu.Name, FscList.Count]);
      case anIndex of
        -1: ; // unsorted
        0: FscList.Sort;
        1: FscList.CustomSort(@SortByComponentPropertyName);
      end;
      FDualDisplay.BeginUpdate;
      for i:=0 to FscList.Count-1 do
        begin
          inf:=TSCInfo(FscList.Objects[i]);
          FDualDisplay.AddLine(FscList[i] + ',' +
            inf.Component.Name + '.' + KindToPropertyName(inf.Kind), dtBlack);
        end;
      FDualDisplay.EndUpdate;
      FDualDisplay.InvalidateContents;
    end;
end;

{ TLineEditor }

constructor TLineEditor.CreateWithShadowItem(aSI: TShadowItem);
var
  topLeft: TPoint;
begin
  inherited CreateNew(Nil);
  BorderStyle:=bsNone;
  Name:='LineEditor';
  // ToDo: Get rid of the global MenuDesigner reference.
  topLeft:=MenuDesigner.ShadowMenu.ClientToScreen(aSI.BoundsRect.TopLeft);
  SetInitialBounds(topLeft.x + aSI.ParentBox.Left,
                   topLeft.y + aSI.ParentBox.Top,
                   aSI.Width+1, aSI.Height+1);
  FEdit:=TEdit.Create(Self);
  FEdit.Align:=alClient;
  FEdit.Text:=aSI.RealItem.Caption;
  FEdit.OnKeyDown:=@EditKeyDown;
  FEdit.Parent:=Self;
end;

function TLineEditor.GetEditedLine: string;
begin
  Result:=FEdit.Text;
end;

procedure TLineEditor.EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE: begin ModalResult:=mrCancel; Key:=0; end;
    VK_RETURN: begin ModalResult:=mrOK; Key:=0; end;
    else inherited KeyDown(Key, Shift);
  end;
end;

{ TFake }

constructor TFake.Create(anOwner: TShadowMenu);
begin
  inherited Create(anOwner);
  FShadowMenu:=anOwner;
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, cx, cy);
  BorderStyle:=bsNone;
  Visible:=False;
  Canvas.Pen.Color:=clBtnShadow;
  Canvas.Pen.Style:=psDot;
  Canvas.Font.Color:=clBtnShadow;
  Canvas.Brush.Color:=clBtnFace;
  Parent:=anOwner;
end;

class function TFake.GetControlClassDefaultSize: TSize;
begin
  Result.cx:=100;
  Result.cy:=DropDown_Height;
end;

procedure TFake.Paint;
var
  r: TRect;
  sz: TSize;
  y: integer;
begin
  r:=ClientRect;
  Canvas.FillRect(r);
  Canvas.RoundRect(r, 3, 3);
  sz:=Canvas.TextExtent(Caption);
  y:=(r.Bottom - r.Top - sz.cy) div 2;
  if (y < 2) then
    y:=2;
  Canvas.TextOut((r.Right - r.Left - sz.cx) div 2, y, Caption);
end;

procedure TFake.Refresh;
begin
  SetVisibilitySizeAndPosition;
end;

procedure TFake.TextChanged;
begin
  inherited TextChanged;
  FMinWidth:=FShadowMenu.GetStringWidth(Caption, False) + Double_MenuBar_Text_Offset;
end;

{ TShadowMenu }

procedure TShadowMenu.AddItemAfter(Sender: TObject);
var
  si: TShadowItem;
begin
  si:=SelectedShadowItem;
  if (si <> nil) then
    si.ParentBox.AddItemAndShadow(si, False);
end;

procedure TShadowMenu.AddItemBefore(Sender: TObject);
var
  si: TShadowItem;
begin
  si:=SelectedShadowItem;
  if (si <> nil) then
    si.ParentBox.AddItemAndShadow(si, True);
end;

procedure TShadowMenu.AddOnClick(Sender: TObject);
var
  compEditor: TDefaultComponentEditor;
begin
  if (FSelectedMenuItem <> nil) then begin
    FDesigner.FGui.BeginUpdate;
    try
      compEditor:=TDefaultComponentEditor.Create(FSelectedMenuItem, FEditorDesigner);
      compEditor.Edit;
      UpdateSelectedItemInfo;
    finally
      compEditor.Free;
      FDesigner.FGui.EndUpdate;
    end;
  end;
end;

procedure TShadowMenu.AddSubMenu(Sender: TObject);
var
  si: TShadowItem;
begin
  si:=SelectedShadowItem;
  if (si <> nil) then begin
    HideFakes;
    AddSubMenuTo(si);
  end;
end;

procedure TShadowMenu.DeleteItem(Sender: TObject);
var
  si: TShadowItem;
begin
  if (FDesigner.TotalMenuItemsCount > 0) then
  begin
    if (Sender is TShadowItem) then
      DeleteChildlessShadowAndItem(TShadowItem(Sender))
    else begin
      si:=SelectedShadowItem;
      if (si <> nil) then
        DeleteChildlessShadowAndItem(si);
    end;
  end;
end;

procedure TShadowMenu.EditCaption(Sender: TObject);
var
  s: string;
  selected: TShadowItem;
begin
  selected:=SelectedShadowItem;
  if (selected <> nil) then begin
    HideFakes;
    s:=GetNewCaptionFor(selected);
    if (s = cLineCaption) and  // disallow renaming to '-' in these cases
       (FSelectedMenuItem.IsInMenuBar or (FSelectedMenuItem.Count > 0)) then begin
      RefreshFakes;
      Exit;
    end;
    if (s <> '') then begin
      FSelectedMenuItem.Caption:=s;
      GlobalDesignHook.RefreshPropertyValues;
      GlobalDesignHook.Modified(FSelectedMenuItem);
      UpdateBoxLocationsAndSizes;
      selected.Repaint;
      FDesigner.FGui.UpdateStatistics;
    end;
    RefreshFakes;
  end;
end;

procedure TShadowMenu.AddSeparatorAbove(Sender: TObject);
var
  selected: TShadowItem;
begin
  if (FSelectedMenuItem <> nil) then begin
    selected:=SelectedShadowItem;
    selected.ParentBox.AddItemAndShadow(selected, True, True);
  end;
end;

procedure TShadowMenu.AddSeparatorBelow(Sender: TObject);
var
  selected: TShadowItem;
begin
  if (FSelectedMenuItem <> nil) then begin
    selected:=SelectedShadowItem;
    selected.ParentBox.AddItemAndShadow(selected, False, True);
  end;
end;

procedure TShadowMenu.MoveItemAfter(Sender: TObject);
var
  nextI, parentI: TMenuItem;
  currIdx: integer;
  selected: TShadowItem;
begin
  if (FSelectedMenuItem <> nil) then begin
    nextI:=GetNextItem(FSelectedMenuItem);
    parentI:=FSelectedMenuItem.Parent;
    selected:=SelectedShadowItem;
    if (nextI <> nil) and (parentI <> nil) then
      begin
        HideFakes;
        HideBoxesAboveLevel(selected.Level);
        currIdx:=FSelectedMenuItem.MenuIndex;
        parentI.Remove(nextI);
        parentI.Remove(FSelectedMenuItem);
        parentI.Insert(currIdx, nextI);
        parentI.Insert(Succ(currIdx), FSelectedMenuItem);
        FEditorDesigner.PropertyEditorHook.RefreshPropertyValues;
        FEditorDesigner.PropertyEditorHook.Modified(FMenu);
        selected.ParentBox.LocateShadows;
        UpdateBoxLocationsAndSizes;
        selected.ShowChildBox;
        RefreshFakes;
        UpdateActionsEnabledness;
      end;
  end;
end;

procedure TShadowMenu.MoveItemBefore(Sender: TObject);
var
  previousI, parentI: TMenuItem;
  currIdx: integer;
  selected: TShadowItem;
begin
  if (FSelectedMenuItem <> nil) then begin
    previousI:=GetPreviousItem(FSelectedMenuItem);
    parentI:=FSelectedMenuItem.Parent;
    selected:=SelectedShadowItem;
    if (previousI <> nil) and (parentI <> nil) then
      begin
        HideFakes;
        HideBoxesAboveLevel(selected.Level);
        currIdx:=FSelectedMenuItem.MenuIndex;
        parentI.Remove(previousI);
        parentI.Remove(FSelectedMenuItem);
        parentI.Insert(Pred(currIdx), FSelectedMenuItem);
        parentI.Insert(currIdx, previousI);
        FEditorDesigner.PropertyEditorHook.RefreshPropertyValues;
        FEditorDesigner.PropertyEditorHook.Modified(FMenu);
        selected.ParentBox.LocateShadows;
        UpdateBoxLocationsAndSizes;
        selected.ShowChildBox;
        RefreshFakes;
        UpdateActionsEnabledness;
      end;
  end;
end;

procedure TShadowMenu.RemoveAllSeparators(Sender: TObject);
begin
  if (FSelectedMenuItem <> nil) then
    SelectedShadowItem.ParentBox.RemoveAllSeparators;
end;

procedure TShadowMenu.ConnectSpeedButtonOnClickMethods;
begin
  with FDesigner.FGui do begin
    AddSeparatorAboveButton.OnClick:=@AddSeparatorAbove;
    AddSeparatorBelowButton.OnClick:=@AddSeparatorBelow;
    MoveItemUpButton.OnClick:=@MoveItemBefore;
    MoveItemDownButton.OnClick:=@MoveItemAfter;
    DeleteItemButton.OnClick:=@DeleteItem;
    AddItemAboveButton.OnClick:=@AddItemBefore;
    AddItemBelowButton.OnClick:=@AddItemAfter;
    AddSubMenuButton.OnClick:=@AddSubMenu;
  end;
end;

procedure TShadowMenu.RecursiveCreateShadows(aParentBox: TShadowBox; aMI: TMenuItem);
var
  j: integer;
  sb: TShadowBox;
begin
  TShadowItem.CreateWithBoxAndItem(Self, aParentBox, aMI);
  if (aMI.Count > 0) then
  begin
    sb:=TShadowBox.CreateWithParentBox(Self, aParentBox, aMI);
    for j:=0 to aMI.Count-1 do
      RecursiveCreateShadows(sb, aMI.Items[j]);
  end;
end;

procedure TShadowMenu.CreateShadowBoxesAndItems;
var
  i: integer;
begin
  if (FMenu.Items.Count > 0) then
  begin
    FRootBox:=TShadowBox.CreateWithParentBox(Self, nil, FMenu.Items);
    for i:=0 to FMenu.Items.Count-1 do begin
      if FIsMainMenu and FMenu.Items[i].IsLine then
        RaiseGDBException(lisMenuEditorSomeWidgetsetsDoNotAllowSeparatorsInTheMainMenubar);
      RecursiveCreateShadows(FRootBox, FMenu.Items[i]);
    end;
  end;
end;

procedure TShadowMenu.DeleteChildlessShadowAndItem(anExistingSI: TShadowItem);
var
  nearestMI, mi: TMenuItem;
  box: TShadowBox;
  ownsIt: TComponent;
begin
  FDesigner.FGui.BeginUpdate;
  try
    mi:=anExistingSI.RealItem;
    if (mi.Count > 0) then
      DeleteShadowAndItemAndChildren(anExistingSI)
    else begin
      HideFakes;
      if (mi = FSelectedMenuItem) then
        FSelectedMenuItem:=nil;
      nearestMI:=GetNextNonSepItem(mi);
      if (nearestMI = nil) then
        nearestMI:=GetPreviousNonSepItem(mi);
      if (nearestMI = nil) then
      begin
        if mi.Parent<>FMenu.Items then
          nearestMI:=mi.Parent;
      end;
      box:=anExistingSI.ParentBox;
      box.ParentMenuItem.Remove(mi);
      ownsIt:=mi.Owner;
      if (ownsIt <> nil) then
        ownsIt.RemoveComponent(mi);
      anExistingSI.RealItem:=nil;
      box.ShadowList.Remove(anExistingSI);
      anExistingSI.Parent:=nil;
      box.RemoveComponent(anExistingSI);
      FreeAndNil(anExistingSI);
      FEditorDesigner.PropertyEditorHook.PersistentDeleting(TPersistent(mi));
      FreeAndNil(mi);
      FEditorDesigner.PropertyEditorHook.PersistentDeleted;
      FEditorDesigner.Modified;

      if (box.ShadowCount = 0) then begin
        FBoxList.Remove(box);
        box.Parent:=nil;
        RemoveComponent(box);
        if box=FRootBox then
          FRootBox:=nil;
        FreeAndNil(box);
      end;
      UpdateBoxLocationsAndSizes;
      SetSelectedMenuItem(nearestMI, False, True);
      FDesigner.FGui.UpdateStatistics;
    end;
  finally
    FDesigner.FGui.EndUpdate;
  end;
end;

procedure TShadowMenu.DeleteBox(aMI: TMenuItem);
var
  i: integer;
  sb: TShadowBox;
  si: TShadowItem;
begin
  for i:=aMI.Count-1 downto 0 do
    DeleteBox(aMI.Items[i]);
  sb:=GetBoxContainingMenuItem(aMI);
  Assert(sb<>nil,'TShadowMenu.DeleteBox: internal error');
  sb.Hide;
  sb.ShadowList.Remove(GetShadowForMenuItem(aMI));
  if (sb.ShadowCount = 0) then
  begin
    FBoxList.Remove(sb);
    sb.Parent:=nil;
    RemoveComponent(sb);
    si:=GetShadowForMenuItem(sb.ParentMenuItem);
    if Assigned(si) then
      si.Repaint;
    FreeAndNil(sb);
  end;
end;

procedure TShadowMenu.DeleteItm(anItem: TMenuItem);
var
  i: integer;
begin
  for i:=anItem.Count-1 downto 0 do
    DeleteItm(anItem.Items[i]);
  anItem.Parent.Remove(anItem);
  GlobalDesignHook.DeletePersistent(TPersistent(anItem));
  GlobalDesignHook.Modified(anItem);
end;

procedure TShadowMenu.DeleteShadowAndItemAndChildren(anExistingSI: TShadowItem);
var
  firstBoxToDelete: TShadowBox;
  mi: TMenuItem;
  i: integer;
begin
  if IDEQuestionDialogAb(
       lisDelete,
       lisMenuEditorDeleteThisItemAndItsSubitems,
       mtWarning, [mrYes, mrNo], False) = mrYes then
  begin
    firstBoxToDelete:=GetBoxWithParentItem(anExistingSI.RealItem);
    Assert(firstBoxToDelete<>nil,'TShadowMenu.DeleteShadowAndItemAndChildren: no children');
    // Delete boxes recursively
    mi:=firstBoxToDelete.ParentMenuItem;
    Assert(mi<>nil,'TShadowMenu,DeleteShadowAndItemAndChildren: RecursiveBoxDelete internal error');
    for i:=mi.Count-1 downto 0 do
      DeleteBox(mi.Items[i]);
    // Delete children recursively
    mi:=anExistingSI.RealItem;
    for i:=mi.Count-1 downto 0 do
      DeleteItm(mi.Items[i]);
    DeleteChildlessShadowAndItem(anExistingSI);
  end;
end;

function TShadowMenu.GetSelectedShadowItem: TShadowItem;
begin
  Result:=GetShadowForMenuItem(FSelectedMenuItem);
end;

function TShadowMenu.GetStringWidth(const aText: string; isBold: boolean): integer;
begin
  if isBold then
    FMainCanvas.Font.Style:=[fsBold]
  else
    FMainCanvas.Font.Style:=[];
  Result:=FMainCanvas.TextWidth(aText);
end;

function TShadowMenu.GetMenuBarIconWidth(aMI: TMenuItem): integer;
begin
  Result:=0;
  if aMI.IsInMenuBar then begin
    if aMI.HasIcon and (aMI.ImageIndex > -1) and
       (FMenu.Images <> nil) then
         Inc(Result, FMenu.Images.Width)
    else if (aMI.Bitmap <> nil) and not aMI.Bitmap.Empty then
      Inc(Result, aMI.Bitmap.Width);
    if (Result > 24) then
      Result:=24;
  end;
end;

procedure TShadowMenu.AddManyItems(aPrimaries, aDepth: integer);
var
  p, d: integer;
  mi, mi2: TMenuItem;
  sb: TShadowBox;

  function NewMenuItem(aParentMI: TMenuItem): TMenuItem;
  begin
    Result:=TMenuItem.Create(FLookupRoot);
    Result.Name:=FEditorDesigner.CreateUniqueComponentName('TMenuItem');
    Result.Caption:=Result.Name;
    if (aParentMI = nil) then
      FMenu.Items.Add(Result)
    else aParentMI.Add(Result);
    FEditorDesigner.PropertyEditorHook.PersistentAdded(Result, False);
    FEditorDesigner.PropertyEditorHook.Modified(Result);
  end;

begin
  if not FIsMainMenu then
    begin
      for p:=1 to aPrimaries do
        TShadowItem.CreateWithBoxAndItem(Self, FRootBox, NewMenuItem(nil));
      UpdateBoxLocationsAndSizes;
    end
  else
    begin
      for p:=0 to aPrimaries-1 do
        begin
          if (p = 0) then
            mi:=FMenu.Items[0]
          else
            begin
              mi:=NewMenuItem(nil);
              TShadowItem.CreateWithBoxAndItem(Self, FRootBox, mi);
            end;
          sb:=TShadowBox.CreateWithParentBox(Self, FRootBox, mi);
          for d:=1 to aDepth do
            begin
              mi2:=NewMenuItem(mi);
              TShadowItem.CreateWithBoxAndItem(Self, sb, mi2);
            end;
        end;
      UpdateBoxLocationsAndSizes;
      HideBoxesAboveLevel(0);
    end;
  SetSelectedMenuItem(FMenu.Items[0], False, False);
  SelectedShadowItem.ShowChildBox;
  FDesigner.FGui.UpdateStatistics;
end;

function TShadowMenu.GetBoxWithParentItem(aParentMI: TMenuItem): TShadowBox;
var
  p: pointer;
  sb: TShadowBox absolute p;
begin
  Assert(aParentMI<>nil,'TShadowMenu.GetBoxWithParentItem: parent item is nil');
  for p in FBoxList do
    if (sb.ParentMenuItem = aParentMI) then
      Exit(sb);
  Result:=nil;
end;

function TShadowMenu.GetHighestLevelVisibleBox: TShadowBox;
var
  i: integer;
begin
  FBoxList.Sort(@SortByBoxLevel);
  for i:=FBoxList.Count-1 downto 0 do
    if TShadowBox(FBoxList[i]).Visible then
      Exit(TShadowBox(FBoxList[i]));
  Result:=nil;
end;

function TShadowMenu.GetMaxVisibleBoxDims(aSB: TShadowBox): TPoint;
begin
  Result:=Point(0,0);
  if (aSB = nil) or not aSB.Visible then
    Exit
  else Result:=Point(aSB.BoundsRect.Right, aSB.BoundsRect.Bottom);
end;

function TShadowMenu.GetMaxVisibleFakeDims: TPoint;
begin
  Result:=Point(0, 0);
  if FAddItemFake.Visible then
    Result:=Point(FAddItemFake.BoundsRect.Right, FAddItemFake.BoundsRect.Bottom);
  if FAddSubMenuFake.Visible then begin
    if (FAddSubmenuFake.BoundsRect.Right > Result.x) then
      Result.x:=FAddSubmenuFake.BoundsRect.Right;
    if (FAddSubmenuFake.BoundsRect.Bottom > Result.y) then
      Result.y:=FAddSubmenuFake.BoundsRect.Bottom;
  end;
end;

function TShadowMenu.GetSelectedShadowBox: TShadowBox;
var
  sel: TShadowItem;
begin
  sel:=SelectedShadowItem;
  if (sel = nil) then
    Result:=nil
  else
    Result:=sel.ParentBox;
end;

procedure TShadowMenu.AddSubMenuTo(anExistingSI: TShadowItem);
var
  newMI: TMenuItem;
  box: TShadowBox;
begin
  if (anExistingSI.RealItem.Count <> 0) then
    Exit;
  newMI:=TMenuItem.Create(FLookupRoot);
  newMI.Name:=FEditorDesigner.CreateUniqueComponentName(newMI.ClassName);
  newMI.Caption:=newMI.Name;
  anExistingSI.RealItem.Add(newMI);
  GlobalDesignHook.PersistentAdded(newMI, False);
  GlobalDesignHook.Modified(newMI);
  box:=TShadowBox.CreateWithParentBox(Self, anExistingSI.ParentBox, anExistingSI.RealItem);
  TShadowItem.CreateWithBoxAndItem(Self, box, newMI);
  UpdateBoxLocationsAndSizes;
  SetSelectedMenuItem(newMI, False, False);
  FDesigner.FGui.UpdateStatistics;
end;

procedure TShadowMenu.SetupPopupMenu;
var
  pe: TPopEnum;
  ac: TAction;
  primaryItem, mi: TMenuItem;

  procedure NewPopItem(const aCaption: string; anOnClick: TNotifyEvent;
                       aShortcut: TShortCut=0; aShortCut2: TShortCut=0);
  begin
    ac:=TAction.Create(Self);
    with ac do begin
      ac.ActionList:=FActionList;
      ac.DisableIfNoHandler:=False;
      Tag:=PtrInt(pe);
      Caption:=aCaption;
      OnExecute:=anOnClick;
      ShortCut:=aShortcut;
    end;
    mi:=TMenuItem.Create(Self);
    FItemsPopupMenu.Items.Add(mi);
    mi.Action:=ac;
    mi.ShortCutKey2:=aShortCut2;
  end;

  procedure NewPopPrimary(const aCaption: string);
  begin
    ac:=TAction.Create(Self);
    with ac do begin
      ActionList:=FActionList;
      DisableIfNoHandler:=False;
      Tag:=PtrInt(pe);
      Caption:=aCaption;
    end;
    mi:=TMenuItem.Create(Self);
    FItemsPopupMenu.Items.Add(mi);
    mi.Action:=ac;
    primaryItem:=mi;
  end;

  procedure NewPopSub(const aPrimary: TMenuItem; const aCaption: string;
                      anOnClick: TNotifyEvent; aShortcut: TShortCut=0);
  begin
    ac:=TAction.Create(Self);
    with ac do begin
      ActionList:=FActionList;
      DisableIfNoHandler:=False;
      Tag:=PtrInt(pe);
      Caption:=aCaption;
      OnExecute:=anOnClick;
      ShortCut:=aShortcut;
    end;
    mi:=TMenuItem.Create(Self);
    aPrimary.Add(mi);
    mi.Action:=ac;
  end;

  procedure NewSeparatorAction;
  begin
    FItemsPopupMenu.Items.AddSeparator;
    ac:=TAction.Create(Self);
    ac.ActionList:=FActionList;
    ac.Tag:=PtrInt(pe);
    ac.Name:=GetEnumName(TypeInfo(TPopEnum), PtrInt(pe));
  end;

begin
  for pe in TPopEnum do
    with FDesigner.FGui do
    case pe of
      popItemAddOnClick:
        NewPopItem(lisMenuEditorAddOnClickHandler, @AddOnClick);
      popItemAddBefore: begin
        NewPopItem('', @AddItemBefore, KeyToShortCut(VK_INSERT,[]));
        AddItemAboveButton.Action:=ac;
      end;
      popItemAddAfter: begin
        NewPopItem('', @AddItemAfter);
        AddItemBelowButton.Action:=ac;
      end;
      popItemAddSubMenu: begin
        NewPopItem('', @AddSubMenu,KeyToShortCut(VK_INSERT,[ssCtrl]));
        AddSubMenuButton.Action:=ac;
      end;
      popItemDelete: begin
        NewPopItem(lisMenuEditorDeleteItem, @DeleteItem, KeyToShortCut(VK_DELETE, []));
        DeleteItemButton.Action:=ac;
      end;
      popItemOISep:
        NewSeparatorAction;
      popItemEditCaption:
        NewPopItem(lisMenuEditorEditCaption, @EditCaption, KeyToShortCut(VK_RETURN, []));
      popItemMoveBefore: begin
        NewPopItem('', @MoveItemBefore, KeyToShortCut(VK_UP,[ssCtrl]));
        MoveItemUpButton.Action:=ac;
      end;
      popItemMoveAfter: begin
        NewPopItem('', @MoveItemAfter, KeyToShortCut(VK_DOWN,[ssCtrl]));
        MoveItemDownButton.Action:=ac;
      end;
      popAddImgListIcon: begin
        NewPopItem('', @AddImageListIcon);
        FAddImgListIconAction:=ac;
      end;
      popItemSep:
        NewSeparatorAction;
      popSeparators_:
        NewPopPrimary(lisMenuEditorSeParators);
      popAddSeparatorBefore: begin
        NewPopSub(primaryItem, lisMenuEditorAddSeparatorBefore, @AddSeparatorAbove);
        AddSeparatorAboveButton.Action:=ac;
      end;
      popAddSeparatorAfter: begin
        NewPopSub(primaryItem, lisMenuEditorAddSeparatorAfter, @AddSeparatorBelow);
        AddSeparatorBelowButton.Action:=ac;
      end;
      popRemoveAllSeparators:
        NewPopSub(primaryItem, lisMenuEditorRemoveAllSeparators, @RemoveAllSeparators);
      popShortcuts_:
        NewPopPrimary(lisMenuEditorShortcUts2);
      popListShortcuts:
        NewPopSub(primaryItem, '', @ListShortcuts);
      popListShortcutsAccelerators:
        NewPopSub(primaryItem, '', @ListShortcutsAndAccelerators);
      popResolveShortcutConflicts:
        NewPopSub(primaryItem, lisMenuEditorResolveShortcutConflicts, @ResolveshortcutConflicts);
      popTemplates_:
        NewPopPrimary(lisMenuEditorTemplates);
      popSaveAsTemplate:
        NewPopSub(primaryItem, lisMenuEditorSaveMenuAsATemplate, @SaveAsTemplate);
      popAddFromTemplate:
        NewPopSub(primaryItem, lisMenuEditorAddFromTemplate, @AddFromTemplate);
      popDeleteTemplate:
        NewPopSub(primaryItem, lisMenuEditorDeleteMenuTemplate, @DeleteTemplate);
    end; // case
end;

function TShadowMenu.GetMenuBarCumWidthForItemIndex(anIndex: integer): integer;
var
  w: integer;
  mi: TMenuItem;
begin
  Result:=0;
  if anIndex <> 0 then
    repeat
      mi:=FMenu.Items[Pred(anIndex)];
      w:=GetStringWidth(mi.Caption, mi.Default) +
         Double_MenuBar_Text_Offset + GetMenuBarIconWidth(mi);
      Inc(Result, w);
      Dec(anIndex)
    until (anIndex <= 0);
end;

function TShadowMenu.GetParentItemHeightInBox(aParentItem: TMenuItem): integer;

  function HeightOfItem(anIndex: integer): integer;
  begin
    if aParentItem.Parent.Items[anIndex].IsLine then
      Result:=Separator_Height
    else
      Result:=DropDown_Height;
  end;

var
  idx: integer = 0;
begin
  Result:=1;
  repeat
    if (idx < aParentItem.MenuIndex) then
      Inc(Result, HeightOfItem(idx));
    Inc(idx);
  until (idx >= aParentItem.MenuIndex);
end;

procedure TShadowMenu.UpdateBoxLocationsAndSizes;
var
  p: pointer;
  sb: TShadowBox absolute p;
  s: pointer;
  si: TShadowItem absolute s;
  lft, w, idx: integer;
  pt: TPoint;
begin
  FBoxList.Sort(@SortByBoxLevel);
  for p in FBoxList do begin
    if sb.IsMenuBar then
      begin
        sb.Align:=alTop;
        sb.Height:=MenuBar_Height;
        lft:=0;
        for s in sb.ShadowList do begin
          w:=si.GetWidth;
          si.SetBounds(lft, 0, w, MenuBar_Height);
          Inc(lft, w);
        end;
      end
    else if FIsMainMenu and (sb.Level = 1) then
      begin
        pt:=sb.GetInnerDims;
        idx:=sb.ParentMenuItem.MenuIndex;
        lft:=GetMenuBarCumWidthForItemIndex(idx);
        sb.SetBounds(lft, MenuBar_Height+1, pt.x+2, pt.y+2);
      end
    else begin
      pt:=sb.GetInnerDims;
      if (sb.Level = 0) then
        sb.SetBounds(Popup_Origin.x, Popup_Origin.y, pt.x+2, pt.y+2)
      else sb.SetBounds(sb.ParentBox.Left+sb.ParentBox.Width,
                        sb.ParentBox.Top+GetParentItemHeightInBox(sb.ParentMenuItem),
                        pt.x+2, pt.y+2);
    end;
  end;
  RefreshFakes;
end;

procedure TShadowMenu.RemoveEmptyBox(aSB: TShadowBox);
var
  miToSelect: TMenuItem;
begin
  if (aSB.ShadowCount = 0) then begin
    miToSelect:=aSB.ParentMenuItem;
    FBoxList.Remove(aSB);
    aSB.Parent:=nil;
    RemoveComponent(aSB);
    FreeAndNil(aSB);
    UpdateBoxLocationsAndSizes;
    SetSelectedMenuItem(miToSelect, False, True);
  end;
end;

procedure TShadowMenu.HideFakes;
begin
  FAddSubmenuFake.Hide;
  FAddItemFake.Hide;
  FAddFirstItemFake.Hide;
end;

procedure TShadowMenu.RefreshFakes;
begin
  Application.ProcessMessages;
  FAddItemFake.Refresh;
  FAddSubmenuFake.Refresh;
  FAddFirstItemFake.Refresh;
end;

procedure TShadowMenu.UpdateButtonGlyphs(isInBar: boolean);
begin
  if (FSelectedMenuItem <> nil) and (isInBar <> FDesigner.VariableGlyphsInMenuBar) then
    FDesigner.FGui.LoadVariableButtonGlyphs(isInBar);
end;

procedure TShadowMenu.AddFromTemplate(Sender: TObject);
var
  newItem: TMenuItem;
  sb: TShadowBox;
  i: integer;
begin
  if (FSelectedMenuItem <> nil) and (FSelectedMenuItem.Parent.Parent = nil) then
  begin
    HideFakes;
    newItem:=InsertMenuTemplateDlg;
    if (newItem <> nil) then
    begin
      FMenu.Items.Add(newItem);
      FLookupRoot.InsertComponent(newItem);
      newItem.Name:=FEditorDesigner.CreateUniqueComponentName(newItem.ClassName);
      FEditorDesigner.PropertyEditorHook.PersistentAdded(TPersistent(newItem), False);
      FEditorDesigner.Modified;
      TShadowItem.CreateWithBoxAndItem(Self, FRootBox, newItem);
      if (newItem.Count > 0) then begin
        sb:=TShadowBox.CreateWithParentBox(Self, FRootBox, newItem);
        for i:=0 to newItem.Count-1 do
        begin
          FLookupRoot.InsertComponent(newItem.Items[i]);
          newItem.Items[i].Name:=FEditorDesigner.CreateUniqueComponentName(newItem.Items[i].ClassName);
          FEditorDesigner.PropertyEditorHook.PersistentAdded(TPersistent(newItem.Items[i]), False);
          FEditorDesigner.Modified;
          TShadowItem.CreateWithBoxAndItem(Self, sb, newItem.Items[i]);
        end;
      end;
      UpdateBoxLocationsAndSizes;
      SetSelectedMenuItem(newItem, False, False);
    end;
  end;
end;

procedure TShadowMenu.AddImageListIcon(Sender: TObject);
var
  idx: integer;
  selected: TShadowItem;
begin
  if (FSelectedMenuItem <> nil) then begin
    selected:=SelectedShadowItem;
    if (FMenu.Images <> nil) then
    begin
      idx:=ChooseIconFromImageListDlg(FMenu.Images);
      if (idx > -1) then begin
        FSelectedMenuItem.ImageIndex:=idx;
        selected.Repaint;
        UpdateActionsEnabledness;
        FEditorDesigner.PropertyEditorHook.RefreshPropertyValues;
        FEditorDesigner.Modified;
      end;
    end
    else if (selected.Level > 0) and
            (FSelectedMenuItem.Parent.SubMenuImages <> nil) then
      begin
        idx:=ChooseIconFromImageListDlg(FSelectedMenuItem.Parent.SubMenuImages);
        if (idx > -1) then begin
          FSelectedMenuItem.ImageIndex:=idx;
          selected.Repaint;
          UpdateActionsEnabledness;
          FEditorDesigner.PropertyEditorHook.RefreshPropertyValues;
          FEditorDesigner.Modified;
        end;
      end;
  end;
end;

procedure TShadowMenu.DeleteTemplate(Sender: TObject);
begin
  if SavedTemplatesExist and DeleteMenuTemplateDlg then begin
    FDesigner.UpdateTemplatesCount;
    UpdateActionsEnabledness;
  end;
end;

procedure TShadowMenu.ListShortcuts(Sender: TObject);
begin
  ListShortCutDlg(FDesigner.Shortcuts, True, Self, FMenu);
end;

procedure TShadowMenu.ListShortcutsAndAccelerators(Sender: TObject);
begin
  ListShortCutDlg(FDesigner.Shortcuts, False, Self, Nil);
end;

procedure TShadowMenu.ResolveShortcutConflicts(Sender: TObject);
var
  dlg: TResolveConflictsDlg;
begin
  dlg:=TResolveConflictsDlg.Create(FDesigner.Shortcuts, Self);
  try
    if dlg.ShowModal <> mrCancel then
      UpdateActionsEnabledness;
  finally
    dlg.Free;
  end;
end;

procedure TShadowMenu.SaveAsTemplate(Sender: TObject);
var
  dlg: TMenuTemplateDialog;
begin
  if (FSelectedMenuItem <> nil) and LevelZeroAndNoGrandchildren(FSelectedMenuItem) then
  begin
    //SaveMenuTemplateDlg(FSelectedMenuItem);
    dlg:=TMenuTemplateDialog.CreateWithMode(FMenu, dmSave);
    try
      dlg.MenuToSave:=FSelectedMenuItem;
      dlg.ShowModal;
    finally
      dlg.Free;
    end;
    FDesigner.UpdateTemplatesCount;
    UpdateActionsEnabledness;
  end;
end;

procedure TShadowMenu.OnObjectPropertyChanged(Sender: TObject;
  NewObject: TPersistent);
var
  propertyEditor: TPropertyEditor absolute Sender;
  i: Integer;
  persistent: TPersistent;
  mi: TMenuItem absolute persistent;
begin
  if not (Sender is TPropertyEditor) or (NewObject = nil) then
    Exit;
  if (NewObject is TAction) then
    for i:=0 to propertyEditor.PropCount-1 do begin
      persistent:=propertyEditor.GetComponent(i);
      if (persistent is TMenuItem) then begin
        if GetShadowForMenuItem(mi) <> nil then
        begin
          UpdateBoxLocationsAndSizes;
          RefreshFakes;
          if (FSelectedMenuItem <> nil) then
            SelectedShadowItem.Repaint;
        end;
      end;
    end;
  if (NewObject is TImageList) and (NewObject = FMenu.Images) then
    UpdateActionsEnabledness;
end;

procedure TShadowMenu.OnDesignerModified(Sender: TObject);
var
  i: integer;
  persistent: TPersistent;
  mi: TMenuItem absolute persistent;
  refreshNeeded: boolean = False;
begin
  if FDesigner.FGui.IsUpdate then
    Exit;

  if (Sender is TPropertyEditor) then begin
    for i:=0 to TPropertyEditor(Sender).PropCount-1 do begin
      persistent:=TPropertyEditor(Sender).GetComponent(i);
      if (persistent is TMenuItem) then begin
        if GetShadowForMenuItem(mi) <> nil then
          refreshNeeded:=True;
      end;
    end;
    if refreshNeeded then begin
      UpdateBoxLocationsAndSizes;
      if ((mi.Action <> nil) and (TAction(mi.Action).ShortCut <> 0)) or
         (mi.ShortCut <> 0) or (mi.ShortCutKey2 <> 0) then
           FDesigner.Shortcuts.UpdateShortcutList(True);
      if (FSelectedMenuItem <> nil) then begin
        RefreshFakes;
        SelectedShadowItem.Repaint;
      end;
      FDesigner.FGui.UpdateStatistics;
    end;
  end;
end;

procedure TShadowMenu.OnDesignerRefreshPropertyValues;
var
  comp: TComponent;
  mi: TMenuItem absolute comp;
  selBox: TShadowBox;
begin
  if FSelectedMenuItem = nil then
    Exit;
  comp:=GlobalDesignHook.GetComponent(FSelectedMenuItem.Name);
  if (comp<>nil) and (comp is TMenuItem) then
  begin
    selBox:=SelectedShadowBox;
    if (selBox.LastRIValue <> mi.RadioItem) then
      FDesigner.FGui.UpdateSubmenuGroupBox(FSelectedMenuItem, selBox, selBox=FRootBox);
  end;
end;

function TShadowMenu.GetBoxContainingMenuItem(aMI: TMenuItem): TShadowBox;
var
  p: pointer;
  sb: TShadowBox absolute p;
  s: pointer;
  si: TShadowItem absolute s;
begin
  for p in FBoxList do
    for s in sb.ShadowList do
      if (si.RealItem = aMI) then
        Exit(sb);
  Result:=nil;
end;

function TShadowMenu.GetParentBoxForMenuItem(aMI: TMenuItem): TShadowBox;
var
  p: pointer;
  sb: TShadowBox absolute p;
  s: pointer;
  si: TShadowItem absolute s;
begin
  for p in FBoxList do
    for s in sb.ShadowList do
      if (si.RealItem = aMI) then
        Exit(sb);
  Result:=nil;
end;

function TShadowMenu.GetShadowForMenuItem(aMI: TMenuItem): TShadowItem;
var
  p: pointer;
  sb: TShadowBox absolute p;
  ps: pointer;
  si: TShadowItem absolute ps;
begin
  for p in FBoxList do
    for ps in sb.ShadowList do
      if (si.RealItem = aMI) then
        Exit(si);
  Result:=nil;
end;

function TShadowMenu.OnClickIsAssigned(aMI: TMenuItem): boolean;
begin
  if (aMI = nil) then
    Exit(False);
  Result:=(FEditorDesigner.PropertyEditorHook.GetMethodName
      (GetMethodProp(aMI, 'OnClick'), aMI) <> '');
end;

procedure TShadowMenu.Paint;
begin
  if FInitialising then
    Exit;
end;

procedure TShadowMenu.SetParent(NewParent: TWinControl);
begin
  inherited SetParent(NewParent);
  if (NewParent <> nil) and not (csDestroying in ComponentState) then
    begin
      Align:=alNone;
      CreateShadowBoxesAndItems;
      UpdateBoxLocationsAndSizes;
      HideBoxesAboveLevel(0);
      Application.ProcessMessages;
      FInitialising:=True;
      if (FInitialSelectedMenuItem <> nil) then begin
        SetSelectedMenuItem(FInitialSelectedMenuItem, True, False);
        UpdateActionsEnabledness;
      end;
    end;
end;

procedure TShadowMenu.SetSelectedMenuItem(aMI: TMenuItem;
  viaDesigner, prevWasDeleted: boolean);
var
  prevSelectedMenuItem: TMenuItem;
  prevSelectedShadow: TShadowItem;
begin
  if (aMI = nil) then
  begin
    if prevWasDeleted then
      SetSelectedShadow(nil, nil, False)
    else
      SetSelectedShadow(FSelectedMenuItem, nil, False);
    FSelectedMenuItem:=nil;
    RefreshFakes;
    Exit;
  end;
  if (FSelectedMenuItem <> aMI) then
  begin
    if (FSelectedMenuItem = nil) or prevWasDeleted then begin
      prevSelectedMenuItem:=nil;
      prevSelectedShadow:=nil;
    end
    else begin
      prevSelectedMenuItem:=FSelectedMenuItem;
      prevSelectedShadow:=GetShadowForMenuItem(prevSelectedMenuItem);
    end;
    if (prevSelectedShadow <> nil) then begin
      if prevSelectedMenuItem.Enabled then
        prevSelectedShadow.ShowNormal
      else
        prevSelectedShadow.ShowDisabled;
      if not AIsDescendantOfB(aMI, prevSelectedMenuItem) then
        prevSelectedShadow.HideChildren;
    end;
    FSelectedMenuItem:=aMI;
    SetSelectedShadow(prevSelectedMenuItem, FSelectedMenuItem, viaDesigner);
  end;
  FDesigner.FGui.ButtonsGroupBox.Enabled:=(aMI <> nil);
end;

procedure TShadowMenu.SetSelectedShadow(const prevSelectedItem,
  curSelectedItem: TMenuItem; viaDesigner: boolean);
var
  selectedShadow, prevShadow: TShadowItem;
begin
  selectedShadow:=GetShadowForMenuItem(curSelectedItem);
  if selectedShadow=nil then
  begin
    HideFakes;
    if (FSelectedMenuItem <> nil) then begin
      SelectedShadowItem.ShowNormal;
      FSelectedMenuItem:=nil;
    end;
    UpdateSelectedItemInfo;
    if not viaDesigner and (FMenu<>nil) then
      FEditorDesigner.SelectOnlyThisComponent(FMenu);
  end else
  begin
    if (prevSelectedItem <> nil) then begin
      prevShadow:=GetShadowForMenuItem(prevSelectedItem);
      if (prevShadow <> nil) and
         (selectedShadow.ParentBox.ParentMenuItem <> prevSelectedItem) and
         (prevShadow.ParentBox <> selectedShadow.ParentBox) then
           prevShadow.HideChainFromRoot;
      end;
    UpdateButtonGlyphs(FSelectedMenuItem.IsInMenuBar);
    selectedShadow.ShowChainToRoot;
    selectedShadow.ShowSelected;
    HideBoxesAboveLevel(selectedShadow.Level);
    selectedShadow.ShowChildBox;

    UpdateSelectedItemInfo;
    if not viaDesigner then
      FEditorDesigner.SelectOnlyThisComponent(curSelectedItem);

    if not FDesigner.FGui.Visible then
      FDesigner.FGui.ShowOnTop;
    selectedShadow.SetFocus;
    UpdateActionsEnabledness;
    RefreshFakes;
  end;
end;

function TShadowMenu.GetActionForEnum(anEnum: TPopEnum): TAction;
var
  i: integer;
begin
  for i:=0 to FActionList.ActionCount do
    if TAction(FActionList.Actions[i]).Tag = PtrInt(anEnum) then
      Exit(TAction(FActionList.Actions[i]));
  Result:=nil;
end;

procedure TShadowMenu.UpdateActionsEnabledness;
var
  ac, ac1, ac2, ac3: TAction;
  pe: TPopEnum;
  isInBar, isFirst, isLast, prevIsSeparator, nextIsSeparator,
    levelZero, levelZeroOr1, primarySCEnabled: boolean;
begin
  if (FSelectedMenuItem = nil) then
    Exit;
  isInBar:=FSelectedMenuItem.IsInMenuBar;
  isFirst:=(FSelectedMenuItem.MenuIndex = 0);
  isLast:=(FSelectedMenuItem.MenuIndex = Pred(FSelectedMenuItem.Parent.Count));
  prevIsSeparator:=PreviousItemIsSeparator(FSelectedMenuItem);
  nextIsSeparator:=NextItemIsSeparator(FSelectedMenuItem);
  levelZero:=(FSelectedMenuItem.Parent <> nil) and (FSelectedMenuItem.Parent.Parent = nil);
  levelZeroOr1:=LevelZeroAndNoGrandchildren(FSelectedMenuItem);
  primarySCEnabled:=not isInBar and (FSelectedMenuItem.Parent.Count > 1);

  for pe in TPopEnum do
  begin
    ac:=GetActionForEnum(pe);
    case pe of
      popItemAddOnClick: ac.Enabled:=not OnClickIsAssigned(FSelectedMenuItem);
      popItemAddBefore:
        if isInBar then begin
          ac.Caption:=lisMenuEditorAddNewItemBefore;
          ac.Hint:=lisMenuEditorAddANewItemBeforeSelectedItem;
        end
        else begin
          ac.Caption:=lisMenuEditorAddNewItemAbove;
          ac.Hint:=lisMenuEditorAddANewItemAboveSelectedItem;
        end;
      popItemAddAfter:
        if isInBar then begin
          ac.Caption:=lisMenuEditorAddNeWItemAfter;
          ac.Hint:=lisMenuEditorAddANewItemAfterSelectedItem;
        end
        else begin
          ac.Caption:=lisMenuEditorAddNeWItemBelow;
          ac.Hint:=lisMenuEditorAddANewItemBelowSelectedItem;
        end;
      popItemAddSubMenu: begin
        ac.Enabled:=(FSelectedMenuItem.Count = 0) and not FSelectedMenuItem.IsLine;
        if isInBar then begin
          ac.Caption:=lisMenuEditorAddSubmenuBelow;
          ac.Hint:=lisMenuEditorAddASubmenuBelowSelectedItem;
        end
        else begin
          ac.Caption:=lisMenuEditorAddSubmenuRight;
          ac.Hint:=
            lisMenuEditorAddASubmenuAtTheRightOfSelectedItem;
        end;
      end;
      popItemDelete: ac.Enabled:=(FMenu.Items.Count > 0);
      //popItemOISep
      //popItemEditCaption
      popItemMoveBefore: begin
        ac.Enabled:=not isFirst;
        if isInBar then begin
          ac.Caption:=lisMenuEditorMoveItemLeft;
          ac.Hint:=lisMenuEditorMoveSelectedItemToTheLeft;
        end
        else begin
          ac.Caption:=lisMenuEditorMoveItemUp;
          ac.Hint:=lisMenuEditorMoveSelectedItemUp; end;
      end;
      popItemMoveAfter: begin
        ac.Enabled:=not isLast;
        if isInBar then begin
          ac.Caption:=lisMenuEditorMoVeItemRight;
          ac.Hint:=lisMenuEditorMoveSelectedItemToTheRight;
        end
        else begin
          ac.Caption:=lisMenuEditorMoVeItemDown;
          ac.Hint:=lisMenuEditorMoveSelectedItemDown;
        end;
      end;
      popAddImgListIcon: begin
        ac.Enabled:=(FMenu.Images <> nil) and (FMenu.Images.Count > 0);
        if ac.Enabled then begin
          if (FSelectedMenuItem.ImageIndex < 0) then
            ac.Caption:=Format(lisMenuEditorAddIconFromS + ' ...',
                               [FMenu.Images.Name])
          else ac.Caption:=lisMenuEditorChangeImagelistIcon;
          if (FMenu.Images.Count = 1) and (FSelectedMenuItem.ImageIndex = 0) then
            ac.Enabled:=False;
        end
        else
          ac.Caption:=lisMenuEditorAddImagelistIcon;
      end;
      //popItemSep
      popSeparators_: ac.Enabled:=primarySCEnabled;
      popAddSeparatorBefore:
        ac.Enabled:=primarySCEnabled and not isFirst and not prevIsSeparator;
      popAddSeparatorAfter:
        ac.Enabled:=primarySCEnabled and not isLast and not nextIsSeparator;
      popRemoveAllSeparators:
        ac.Enabled:=primarySCEnabled and (GetChildSeparatorCount(FSelectedMenuItem.Parent) > 0);
      //popShortcuts_
      popListShortcuts: begin
        ac.Enabled:=(FDesigner.Shortcuts.ShortcutMenuItemsCount > 0);
        ac.Caption:=Format(lisMenuEditorListShortcutsForS, [FMenu.Name]);
      end;
      popListShortcutsAccelerators: begin
        ac.Enabled:=(FDesigner.Shortcuts.ShortcutList.AcceleratorsInContainerCount > 0);
        ac.Caption:=Format(lisMenuEditorListShortcutsAndAccelerators,[FLookupRoot.Name]);
      end;
      popResolveShortcutConflicts: ac.Enabled:=
        (FDesigner.Shortcuts.ShortcutList.InitialDuplicatesCount > 0);
      popTemplates_:          ac.Enabled:=levelZero or FDesigner.TemplatesSaved;
      popSaveAsTemplate:      ac.Enabled:=levelZeroOr1;
      popAddFromTemplate:     ac.Enabled:=levelZero;
      popDeleteTemplate:      ac.Enabled:=FDesigner.TemplatesSaved;
    end; // case
  end; // for
  ac:=GetActionForEnum(popShortcuts_);
  ac1:=GetActionForEnum(popListShortcuts);
  ac2:=GetActionForEnum(popListShortcutsAccelerators);
  ac3:=GetActionForEnum(popResolveShortcutConflicts);
  ac.Enabled:=ac1.Enabled or ac2.Enabled or ac3.Enabled;
end;

constructor TShadowMenu.Create(aDesigner: TMenuDesigner; aForm: TForm;
  aMenu: TMenu; aSelect: TMenuItem; aWidth, aHeight: integer);
begin
  Assert(aMenu<>nil,'TShadowMenu.Create: TMenu parameter is nil');
  inherited Create(nil);
  FDesigner := aDesigner;
  FMainCanvas := aForm.Canvas;
  FInitialSelectedMenuItem := aSelect;
  SetInitialBounds(0, 0, aWidth, aHeight);
  FMenu := aMenu;
  FIsMainMenu := (FMenu is TMainMenu);
  Name := 'ShadowMenu';
  FEditorDesigner := FindRootDesigner(FMenu) as TComponentEditorDesigner;
  FLookupRoot := FEditorDesigner.LookupRoot;
  FBoxList := TFPList.Create;
  FItemsPopupMenu := TPopupMenu.Create(Self);
  FItemsPopupMenu.Name := 'ItemsPopupMenu';
  FActionList := TActionList.Create(Self);
  SetupPopupMenu;
  FAddItemFake := TAddSiblingFake.Create(Self);
  FAddItemFake.OnClick := @AddItemAfter;
  FAddItemFake.Caption := lisMenuEditorAddMenuItem;
  FAddItemFake.Name := 'AddItemFake';
  FAddSubmenuFake := TAddSubmenuFake.Create(Self);
  FAddSubmenuFake.OnClick := @AddSubMenu;
  FAddSubmenuFake.Caption := lisMenuEditorAddSubmenu;
  FAddSubmenuFake.Name := 'AddSubmenuFake';
  FAddFirstItemFake := TAddFirstFake.Create(Self);
  FAddFirstItemFake.OnClick := @AddFirstMenu;
  FAddFirstItemFake.Caption := lisMenuEditorAddMenuItem;
  FAddFirstItemFake.Name := 'AddFirstItemFake';
  FAddFirstItemFake.Left := Popup_Origin.x;
  FAddFirstItemFake.Top := Popup_Origin.y;
  ConnectSpeedButtonOnClickMethods;
  GlobalDesignHook.AddHandlerObjectPropertyChanged(@OnObjectPropertyChanged);
  GlobalDesignHook.AddHandlerModified(@OnDesignerModified);
  GlobalDesignHook.AddHandlerRefreshPropertyValues(@OnDesignerRefreshPropertyValues);
  Parent := aForm;
  AutoSize := False;
  Color := clBtnFace;
  BorderStyle := bsNone;
  Align := alClient;
end;

procedure TShadowMenu.AddFirstMenu(Sender: TObject);
var
  newMI: TMenuItem;
  box: TShadowBox;
begin
  newMI:=TMenuItem.Create(FLookupRoot);
  newMI.Name:=FEditorDesigner.CreateUniqueComponentName(newMI.ClassName);
  newMI.Caption:=newMI.Name;
  FMenu.Items.Add(newMI);
  GlobalDesignHook.PersistentAdded(newMI, False);
  GlobalDesignHook.Modified(newMI);
  box:=TShadowBox.CreateWithParentBox(Self, nil, FMenu.Items);
  FRootBox:=box;
  TShadowItem.CreateWithBoxAndItem(Self, box, newMI);
  UpdateBoxLocationsAndSizes;
  SetSelectedMenuItem(newMI, False, False);
  FDesigner.FGui.UpdateStatistics;
end;

destructor TShadowMenu.Destroy;
begin
  FEditorDesigner:=nil;
  FreeAndNil(FBoxList);
  inherited Destroy;
end;

procedure TShadowMenu.HideBoxesAboveLevel(aLevel: integer);
var
  p: pointer;
  sb: TShadowBox absolute p;
begin
  for p in FBoxList do
    if (sb.Level > aLevel) then
      sb.Hide;
end;

procedure TShadowMenu.UpdateSelectedItemInfo;
begin
  FDesigner.FGui.UpdateItemInfo(FMenu, FSelectedMenuItem, SelectedShadowBox,
                                FEditorDesigner.PropertyEditorHook);
end;

{ TShadowBox }

procedure TShadowBox.BeginUpdate;
begin
  FUpdating:=True;
end;

procedure TShadowBox.EndUpdate;
begin
  FUpdating:=False;
end;

procedure TShadowBox.ShowAllUnSelected;
var
  p: pointer;
  si: TShadowItem absolute p;
begin
  for p in FShadowList do
    si.ShowNormal;
end;

function TShadowBox.GetIsMainMenu: boolean;
begin
  Result:=FShadowMenu.IsMainMenu;
end;

function TShadowBox.GetIsMenuBar: boolean;
begin
  Result:=(FLevel = 0) and IsMainMenu;
end;

function TShadowBox.GetShadowCount: integer;
begin
  Result:=FShadowList.Count;
end;

procedure TShadowBox.Paint;
var
  r: TRect;
  dets: TThemedElementDetails;
begin
  r:=ClientRect;
  BeginUpdate;
    if IsMenuBar then begin
      dets:=ThemeServices.GetElementDetails(tmBarBackgroundActive);
      ThemeServices.DrawElement(Canvas.Handle, dets, r);
    end
    else begin
      Canvas.FillRect(r);
      Canvas.Frame(r);
    end;
    LocateShadows;
  EndUpdate;
end;

procedure TShadowBox.SelectPrevious(aSI: TShadowItem);
var
  prevMI: TMenuItem;
begin
  prevMI:=GetPreviousNonSepItem(aSI.RealItem);
  if (prevMI <> nil) then
    FShadowMenu.SetSelectedMenuItem(prevMI, False, False);
end;

procedure TShadowBox.SelectSuccessor(aSI: TShadowItem);
var
  nextMI: TMenuItem;
begin
  nextMI:=GetNextNonSepItem(aSI.RealItem);
  if (nextMI <> nil) then
    FShadowMenu.SetSelectedMenuItem(nextMI, False, False);
end;

procedure TShadowBox.AddItemAndShadow(existingSI: TShadowItem;
  addBefore: boolean; isSeparator: boolean);
var
  idx: integer;
  newMI: TMenuItem;
begin
  FShadowMenu.HideFakes;
  idx:=existingSI.RealItem.MenuIndex;
  if not addBefore then
    Inc(idx);
  newMI:=TMenuItem.Create(FShadowMenu.LookupRoot);
  newMI.Name:=FShadowMenu.FEditorDesigner.CreateUniqueComponentName(newMI.ClassName);
  if isSeparator then
    newMI.Caption:=cLineCaption
  else newMI.Caption:=newMI.Name;
  existingSI.RealItem.Parent.Insert(idx, newMI);
  TShadowItem.CreateWithBoxAndItem(FShadowMenu, existingSI.ParentBox, newMI);
  FShadowMenu.UpdateBoxLocationsAndSizes;
  GlobalDesignHook.PersistentAdded(newMI, not isSeparator);
  GlobalDesignHook.Modified(newMI);
  if not isSeparator then
    FShadowMenu.FDesigner.FGui.UpdateStatistics;
  FShadowMenu.UpdateActionsEnabledness;
end;

procedure TShadowBox.RemoveAllSeparators;
var
  mi, nearestMI: TMenuItem;
  i, sepCount: integer;
  si: TShadowItem;
  ownsIt: TComponent;
begin
  if (IsMainMenu and (Self = FShadowMenu.RootBox)) then
    Exit;
  sepCount:=GetChildSeparatorCount(FParentMenuItem);
  if (sepCount > 0) then begin
    FShadowMenu.HideFakes;
    ShowAllUnSelected;
    nearestMI:=GetNextNonSepItem(FShadowMenu.SelectedMenuItem);
    if (nearestMI = nil) then
      nearestMI:=GetPreviousNonSepItem(FShadowMenu.SelectedMenuItem);
    if (nearestMI = nil) then
      nearestMI:=FParentMenuItem;
    for i:=ParentMenuItem.Count-1 downto 0 do
    begin
      mi:=ParentMenuItem.Items[i];
      if mi.IsLine then
      begin
        si:=FShadowMenu.GetShadowForMenuItem(mi);
        ownsIt:=mi.Owner;
        Assert(si<>nil,'TShadowBox.RemoveAllSeparators: shadow for separator is nil');
        FShadowList.Remove(si);
        RemoveComponent(si);
        FreeAndNil(si);
        ParentMenuItem.Remove(mi);
        FShadowMenu.FEditorDesigner.PropertyEditorHook.PersistentDeleting(TPersistent(mi));
        if (ownsIt <> nil) then begin
          ownsIt.RemoveComponent(mi);
          FreeAndNil(mi);
        end;
      end;
    end;
    if (GetShadowCount = 0) then
      FShadowMenu.RemoveEmptyBox(Self)
    else begin
      FShadowMenu.UpdateBoxLocationsAndSizes;
      FShadowMenu.SetSelectedMenuItem(nearestMI, False, True);
    end;
  end;
end;

procedure TShadowBox.LocateShadows;
var
  p: pointer;
  si: TShadowItem absolute p;
  len, t, w, h: integer;
begin
  if (ShadowCount = 0) then
    Exit;
  FShadowList.Sort(@SortByItemMenuIndex);
  if IsMenuBar then begin
    len:=0;
    for p in FShadowList do begin
      w:=si.GetWidth;
      si.SetBounds(len, 0, w, MenuBar_Height);
      Inc(len, w);
    end;
  end
  else begin
    w:=GetInnerDims.x;
    t:=1;
    for p in FShadowList do begin
      h:=si.GetHeight;
      si.SetBounds(1, t, w, h);
      Inc(t, h);
    end;
  end;
end;

function TShadowBox.GetInnerDims: TPoint;
var
  p: pointer;
  si: TShadowItem absolute p;
  w: integer;
begin
  FillChar(Result{%H-}, SizeOf(Result), 0);
  for p in FShadowList do begin
    Inc(Result.y, si.GetHeight);
    w:=si.GetWidth;
    if (Result.x < w) then
      Result.x:=w;
  end;
end;

constructor TShadowBox.CreateWithParentBox(aSMenu: TShadowMenu;
  aParentBox: TShadowBox; aParentItem: TMenuItem);
begin
  inherited Create(aSMenu, aParentItem);
  Name := 'ShadowBox' + IntToStr(ShadowBoxID);
  Inc(ShadowBoxID);
  FShadowMenu := aSMenu;
  FParentBox := aParentBox;
  if (FParentBox = nil) then
    FLevel:=0
  else
    FLevel := aParentBox.Level + 1;
  Canvas.Pen.Color := clLtGray;
  Canvas.Brush.Color := clBtnFace;
  FShadowMenu.BoxList.Add(Self);
  Parent := FShadowMenu;
end;

destructor TShadowBox.Destroy;
begin
  inherited Destroy;
end;

procedure TShadowBox.SetUnCheckedAllExcept(aMI: TMenuItem);
var
  i: integer;
begin
  if (aMI = nil) or (FShadowMenu.GetParentBoxForMenuItem(aMI) <> Self) or
     (FParentMenuItem = nil) then
    Exit;
  for i:=0 to Pred(FParentMenuItem.Count) do
    begin
      if (FParentMenuItem.Items[i] = aMI) then
        Continue;
      if FParentMenuItem.Items[i].RadioItem and
         (FParentMenuItem.Items[i].GroupIndex = aMI.GroupIndex) then
        begin
          FParentMenuItem.Items[i].Checked:=False;
          FShadowMenu.FEditorDesigner.PropertyEditorHook.RefreshPropertyValues;
          FShadowMenu.GetShadowForMenuItem(FParentMenuItem.Items[i]).Invalidate;
        end;
    end;
end;

{ TShadowItem }

function TShadowItem.GetWidth: integer;
var
  w: integer;
begin
  w:=FShadowMenu.GetStringWidth(FRealItem.Caption, FRealItem.Default);
  if FRealItem.IsInMenuBar then
    Result:=w + Double_MenuBar_Text_Offset + FShadowMenu.GetMenuBarIconWidth(FRealItem)
  else
    Result:=w + Double_DropDown_Text_Offset + GetShortcutWidth;
end;

function TShadowItem.HasChildBox(out aChildBox: TShadowBox): boolean;
begin
  aChildBox:=nil;
  Result:=(FRealItem.Count > 0);
  if Result then begin
    aChildBox:=FShadowMenu.GetBoxWithParentItem(FRealItem);
    Assert(aChildBox<>nil,'TShadowItem.HasChildBox: children exist but not the container for them');
  end;
end;

procedure TShadowItem.RecursiveHideChildren(aMI: TMenuItem);
var
  container: TShadowBox;
  firstChild: TMenuItem;
begin
  container:=FShadowMenu.GetParentBoxForMenuItem(aMI);
  Assert(container<>nil,'TShadowItem.HideChildren: missing parent box for '+aMI.Caption);
  container.Hide;
  if (aMI.Count > 0) then begin
    firstChild:=aMI.Items[0];
    Assert(firstChild<>nil,'TShadowItem.HideChildren: missing child');
    RecursiveHideChildren(firstChild);
  end;
end;

procedure TShadowItem.HideChildren;
var
  child: TMenuItem;
begin
  if (FRealItem.Count > 0) then begin
    child:=FRealItem.Items[0];
    Assert(child<>nil,'TShadowItem.HideChildren: missing child');
    RecursiveHideChildren(child);
  end;
end;

procedure TShadowItem.DblClick;
begin
  inherited DblClick;
  if not FShadowMenu.OnClickIsAssigned(FRealItem) then
    FShadowMenu.AddOnClick(nil);
end;

function TShadowItem.GetHeight: integer;
begin
  if FRealItem.IsInMenuBar then
    Result:=MenuBar_Height
  else if FRealItem.IsLine then
    Result:=Separator_Height
  else
    Result:=DropDown_Height;
end;

function TShadowItem.GetIsInMenuBar: boolean;
begin
  Result:=FRealItem.IsInMenuBar;
end;

function TShadowItem.GetIsMainMenu: boolean;
begin
  Result:=FShadowMenu.IsMainMenu;
end;

function TShadowItem.GetLevel: integer;
begin
  Result:=FParentBox.Level;
end;

function TShadowItem.GetBottomFake: TFake;
begin
  Result:=nil;
  if (FShadowMenu.SelectedShadowItem = Self) then
    case FRealItem.IsInMenuBar of
      False: if (FShadowMenu.AddItemFake.Visible) then
               Result:=FBottomFake;
      True: if (FShadowMenu.AddSubMenuFake.Visible) then
              Result:=FBottomFake;
    end;
end;

function TShadowItem.GetRightFake: TFake;
begin
  Result:=nil;
  if (FShadowMenu.SelectedShadowItem = Self) then
    case FRealItem.IsInMenuBar of
      False: if (FShadowMenu.AddSubMenuFake.Visible) then
               Result:=FRightFake;
      True: if FShadowMenu.AddItemFake.Visible then
              Result:=FRightFake;
    end;
end;

function TShadowItem.GetShortcutWidth: integer;
var
  hasSC, hasSC2: boolean;
begin
  Result:=0;
  if FRealItem.IsInMenuBar then
    Exit;
  hasSC:=(FRealItem.ShortCut <> 0);
  if hasSC then
    Inc(Result, FShadowMenu.GetStringWidth(ShortCutToText(FRealItem.ShortCut),
                                           FRealItem.Default));
  hasSC2:=(FRealItem.ShortCutKey2 <> 0);
  if hasSC2 then
    Inc(Result, FShadowMenu.GetStringWidth(ShortCutToText(FRealItem.ShortCutKey2),
                                           FRealItem.Default));
  if (hasSC or hasSC2) then
    Inc(Result, Shortcut_Offset);
  if (hasSC and hasSC2) then
    Inc(Result, FShadowMenu.GetStringWidth(', ', False));
end;

function TShadowItem.GetShowingBottomFake: boolean;
begin
  Result:=(BottomFake <> nil) and BottomFake.Visible;
end;

function TShadowItem.GetShowingRightFake: boolean;
begin
  Result:=(RightFake <> nil) and RightFake.Visible;
end;

procedure TShadowItem.SetState(AValue: TShadowItemDisplayState);
begin
  if (FState <> AValue) then begin
    FState:=AValue;
    Repaint;
  end;
end;

function TShadowItem.GetIconTopLeft: TPoint;
begin
  Result:=Point(1, 1);
  if (FShadowMenu.FMenu.Images.Height < ClientHeight) then
    Result.y:=(ClientHeight - FShadowMenu.FMenu.Images.Height) div 2;
  if (FShadowMenu.FMenu.Images.Width < Gutter_X) then
    Result.x:=(Gutter_X - FShadowMenu.FMenu.Images.Width) div 2;
end;

function TShadowItem.GetBitmapLeftTop: TPoint;
begin
  Result:=Point(1, 1);
  if (FRealItem.Bitmap.Height < ClientHeight) then
    Result.y:=(ClientHeight - FRealItem.Bitmap.Height) div 2;
  if (FRealItem.Bitmap.Width < Gutter_X) then
    Result.x:=(Gutter_X - FRealItem.Bitmap.Width) div 2;
end;

function TShadowItem.GetSubImagesIconTopLeft: TPoint;
begin
  Result:=Point(1, 1);
  if (FRealItem.Parent.SubMenuImages.Height < ClientHeight) then
    Result.y:=(ClientHeight - FRealItem.Parent.SubMenuImages.Height) div 2;
  if (FRealItem.Parent.SubMenuImages.Width < Gutter_X) then
    Result.x:=(Gutter_X - FRealItem.Parent.SubMenuImages.Width) div 2;
end;

procedure TShadowItem.Paint;
var
  r, gutterR: TRect;
  textFlags: integer = DT_VCENTER or DT_SINGLELINE or DT_EXPANDTABS or DT_CENTER;
  tStyle: TTextStyle;
  s: string;

  procedure DrawMenuBarItem;
  var
    oldFontStyle: TFontStyles;
    oldFontColor: TColor;
    x, y: integer;
    sz: TSize;
    pt: TPoint;
    dets: TThemedElementDetails;
  begin
    if (FState = dsSelected) then begin
      Canvas.Brush.Color:=clHighlight;
      Canvas.FillRect(r);
      sz:=Canvas.TextExtent(s);
      y:=(r.Bottom - r.Top - sz.cy) div 2;
      x:=(r.Right - r.Left - sz.cx) div 2;
      if FRealItem.HasIcon and (FRealItem.ImageIndex > -1) and (FShadowMenu.FMenu.Images <> nil) then begin
        pt:=GetIconTopLeft;
        FShadowMenu.FMenu.Images.Draw(Canvas, 0, pt.y, FRealItem.ImageIndex);
        Inc(x, MenuBar_Text_Offset);
      end
      else if (FRealItem.Bitmap <> nil) and not FRealItem.Bitmap.Empty then begin
        pt:=GetBitmapLeftTop;
        Canvas.Draw(0, pt.y, RealItem.Bitmap);
        Inc(x, MenuBar_Text_Offset);
      end;
      oldFontStyle:=Canvas.Font.Style;
      if FRealItem.Default then
        Canvas.Font.Style:=[fsBold]
      else Canvas.Font.Style:=[];
      oldFontColor:=Canvas.Font.Color;
      Canvas.Font.Color:=clHighlightText;
      Canvas.TextRect(r, x, y, s, tStyle);
      Canvas.Font.Color:=oldFontColor;
      Canvas.Font.Style:=oldFontStyle;
    end
    else begin
      InflateRect(r, 1, 0); // hack needed only on Windows?
      case FState of
        dsNormal:   dets:=ThemeServices.GetElementDetails(tmBarBackgroundActive);
        dsDisabled: dets:=ThemeServices.GetElementDetails(tmBarItemDisabled);
      end;
      ThemeServices.DrawElement(Canvas.Handle, dets, r);
      if FRealItem.HasIcon and (FRealItem.ImageIndex > -1) and (FShadowMenu.FMenu.Images <> nil) then
        ThemeServices.DrawIcon(Canvas, dets, Point(0,0), FShadowMenu.FMenu.Images, FRealItem.ImageIndex)
      else if (FRealItem.Bitmap <> nil) and not FRealItem.Bitmap.Empty then begin
        pt:=GetBitmapLeftTop;
        Canvas.Draw(pt.x, pt.y, RealItem.Bitmap);
      end;
      r.Left:=FShadowMenu.GetMenuBarIconWidth(FRealItem);
      if FRealItem.Default then begin
        oldFontStyle:=Canvas.Font.Style;
        Canvas.Font.Style:=[fsBold];
      end;
      ThemeServices.DrawText(Canvas, dets, FRealItem.Caption, r, textFlags, 0);
      if (FState = dsDisabled) then begin // perhaps this display hack is only needed on Windows?
        Canvas.Pen.Color:=clBtnShadow;
        Canvas.Line(0, MenuBar_Height-1, ClientWidth, MenuBar_Height-1);
      end;
      if FRealItem.Default then
        Canvas.Font.Style:=oldFontStyle;
    end;
  end;

  procedure DrawBackgroundAndGutter;
  begin
    case FState of
      dsNormal, dsDisabled: Canvas.Brush.Color:=clBtnFace;
      dsSelected: Canvas.Brush.Color:=clHighlight;
    end;
    if FRealItem.IsLine and (FState = dsSelected) then
      Canvas.FillRect(r.Left, r.Top+2, r.Right, r.Bottom+2)
    else
      Canvas.FillRect(r);
    gutterR:=Rect(Gutter_X, 0, Gutter_X+1, ClientHeight);
    LCLIntf.DrawEdge(Canvas.Handle, gutterR, EDGE_ETCHED, BF_LEFT);
  end;

  procedure DrawCheckMarkIcon;
  var
    pt: TPoint;
    dets: TThemedElementDetails;
  begin
    if FRealItem.Checked then begin
      gutterR:=r;
      gutterR.Right:=Gutter_X;
      if FRealItem.RadioItem then       // radioItem
        case FState of
          dsNormal: begin
  	      dets:=ThemeServices.GetElementDetails(tmPopupCheckBackgroundNormal);
  	      ThemeServices.DrawElement(Canvas.Handle, dets, gutterR);
  	      dets:=ThemeServices.GetElementDetails(tmPopupBulletNormal);
  	      ThemeServices.DrawElement(Canvas.Handle, dets, gutterR);
  	    end;
          dsSelected: begin
              dets:=ThemeServices.GetElementDetails(tmPopupItemHot);
              ThemeServices.DrawElement(Canvas.Handle, dets, gutterR);
              dets:=ThemeServices.GetElementDetails(tmPopupBulletNormal);
              ThemeServices.DrawElement(Canvas.Handle, dets, gutterR);
            end;
          dsDisabled: begin
              dets:=ThemeServices.GetElementDetails(tmPopupCheckBackgroundDisabled);
  	      ThemeServices.DrawElement(Canvas.Handle, dets, gutterR);
  	      dets:=ThemeServices.GetElementDetails(tmPopupBulletDisabled);
  	      ThemeServices.DrawElement(Canvas.Handle, dets, gutterR);
            end;
        end
      else begin                              // checkmark
  	dets:=ThemeServices.GetElementDetails(tmPopupCheckBackgroundNormal);
  	ThemeServices.DrawElement(Canvas.Handle, dets, gutterR);
  	dets:=ThemeServices.GetElementDetails(tmPopupCheckMarkNormal);
  	ThemeServices.DrawElement(Canvas.Handle, dets, gutterR);
      end;
    end
    else                                     // not checked
      if FRealItem.HasIcon and (FRealItem.GlyphShowMode<>gsmNever) and
        (FRealItem.ImageIndex > -1) and (FShadowMenu.FMenu.Images <> nil) and
        (FRealItem.ImageIndex < FShadowMenu.FMenu.Images.Count) then
          ThemeServices.DrawIcon(Canvas, dets, GetIconTopLeft,
                                 FShadowMenu.FMenu.Images, FRealItem.ImageIndex)
      else
        if (FRealItem.ImageIndex > -1) and (FParentBox.Level > 0) and
          (FRealItem.Parent.SubMenuImages <> nil) and
          (FRealItem.ImageIndex < FRealItem.Parent.SubMenuImages.Count) then
            ThemeServices.DrawIcon(Canvas, dets, GetSubImagesIconTopLeft,
                                   RealItem.Parent.SubMenuImages, RealItem.ImageIndex)
        else if FRealItem.HasBitmap and not FRealItem.Bitmap.Empty then begin
  	  pt:=GetBitmapLeftTop;
  	  Canvas.Draw(pt.x, pt.y, RealItem.Bitmap);
        end;
  end;

  procedure DrawText;
  var
    oldFontColor: TColor;
    oldFontStyle: TFontStyles;
    s1, s2: string;
    sc1, sc2: boolean;
    x, y: integer;
  begin
    Canvas.Brush.Style:=bsClear;
    if FRealItem.RightJustify then
      textFlags:=textFlags or DT_RIGHT
    else
      textFlags:=textFlags or DT_LEFT;
    r.Left:=DropDown_Text_Offset;
    oldFontStyle:=Canvas.Font.Style;
    if FRealItem.Default then
      Canvas.Font.Style:=[fsBold]
    else Canvas.Font.Style:=[];
    x:=DropDown_Text_Offset;
    y:=(Height-Canvas.TextHeight(s)) div 2;
    case FState of
      dsNormal: Canvas.TextRect(r, x, y, s, tStyle);
      dsSelected: begin
          OldFontColor:=Canvas.Font.Color;
          Canvas.Font.Color:=clHighlightText;
          Canvas.TextRect(r, x, y, s, tStyle);
          Canvas.Font.Color:=oldFontColor;
        end;
      dsDisabled: begin
          OldFontColor:=Canvas.Font.Color;
          Canvas.Font.Color:=clBtnShadow;
          Canvas.TextRect(r, x, y, s, tStyle);
          Canvas.Font.Color:=OldFontColor;
        end;
    end;

    sc1:=(FRealItem.ShortCut <> 0);
    if sc1 then
      s1:=ShortCutToText(FRealItem.Shortcut);
    sc2:=(FRealItem.ShortCutKey2 <> 0);
    if sc2 then
      s2:=ShortCutToText(FRealItem.ShortCutKey2);
    if sc1 or sc2 then    //#todo allow for rightjustify?
    begin
      if sc1 and not sc2 then
        s:=s1
      else if sc2 and not sc1 then
        s:=s2
      else
        s:=s1 + ', ' + s2;
      x:=r.Right - Canvas.TextWidth(s) - DropDown_Height;
      case FState of
        dsNormal: Canvas.TextRect(r, x, y, s, tStyle);
        dsSelected: begin
            OldFontColor:=Canvas.Font.Color;
            Canvas.Font.Color:=clHighlightText;
            Canvas.TextRect(r, x, y, s, tStyle);
            Canvas.Font.Color:=oldFontColor;
          end;
        dsDisabled: begin
            OldFontColor:=Canvas.Font.Color;
            Canvas.Font.Color:=clBtnShadow;
            Canvas.TextRect(r, x, y, s, tStyle);
            Canvas.Font.Color:=OldFontColor;
          end;
      end;
    end;
    Canvas.Font.Style:=oldFontStyle;
  end;

  procedure DrawChevron;
  var
    pts: array of TPoint;
    oldBrushColor, oldPenColor: TColor;
  begin
    { ToDo: This should be done by theme services
            but it must be implemented for different widgetsets first.
    dets:=ThemeServices.GetElementDetails(tmPopupSubmenuNormal);
    ThemeServices.DrawElement(Canvas.Handle, dets, r);
    }
    r.Right:=ClientWidth;
    r.Left:=r.Right - MenuBar_Height;
    SetLength(pts, 4);
    pts[0]:=Point(r.Left, 9);
    pts[1]:=Point(r.Left + 4, 12);
    pts[2]:=Point(r.Left, 15);
    pts[3]:=pts[0];
    oldBrushColor:=Canvas.Brush.Color;
    oldPenColor:=Canvas.Pen.Color;
    if (FState = dsSelected) then begin
      Canvas.Pen.Color:=clHighlightText;
      Canvas.Brush.Color:=clHighlightText;
    end
    else begin
      Canvas.Brush.Color:=clBlack;
      Canvas.Pen.Color:=clBlack;
    end;
    Canvas.Polygon(pts);
    Canvas.Brush.Color:=oldBrushColor;
    Canvas.Pen.Color:=oldPenColor;
  end;

var
  alygn: TAlignment;
begin
  if FParentBox.Updating then Exit;
  r:=ClientRect;
  if FRealItem.RightJustify then
    alygn:=taRightJustify
  else
    alygn:=taLeftJustify;
  if (FRealItem.Caption = '') then
    s:=FRealItem.Name
  else
    s:=FRealItem.Caption;
  FillChar(tStyle{%H-}, SizeOf(tStyle), 0);
  with tStyle do begin
    Alignment:=BidiFlipAlignment(alygn, UseRightToLeftAlignment);
    Layout:=tlCenter;
    SingleLine:=True;
    Clipping:=True;
    ShowPrefix:=True;
    RightToLeft:=UseRightToLeftReading;
    ExpandTabs:=True;
  end;
  if FRealItem.IsInMenuBar then
    DrawMenuBarItem
  else begin
    DrawBackgroundAndGutter;
    if FRealItem.IsLine then begin
      gutterR:=Rect(Gutter_X, Separator_Centre, ClientWidth, Separator_Centre);
      LCLIntf.DrawEdge(Canvas.Handle, gutterR, EDGE_ETCHED, BF_TOP);
      Exit;
    end;
    if (FRealItem.Checked or FRealItem.HasIcon) then
      DrawCheckMarkIcon;
    DrawText;
    if (FRealItem.Count > 0) then
      DrawChevron;
  end;
end;

procedure TShadowItem.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Shift = []) then
    case Key of
      VK_LEFT: begin
        if IsInMenuBar then
          FParentBox.SelectPrevious(Self)
        else if (IsMainMenu and (Level > 1)) or (not IsMainMenu and (level > 0)) then
          FShadowMenu.SetSelectedMenuItem(FParentBox.ParentMenuItem, False, False);
        Key:=0;
      end;
      VK_RIGHT: begin
        if IsInMenuBar then
          FParentBox.SelectSuccessor(Self)
        else if (FRealItem.Count > 0) then begin
          ShowChildBox;
          FShadowMenu.SetSelectedMenuItem(FRealItem.Items[0], False, False);
        end;
        Key:=0;
      end;
      VK_DOWN: begin
        if IsInMenuBar and (FRealItem.Count > 0) then begin
          ShowChildBox;
          FShadowMenu.SetSelectedMenuItem(FRealItem.Items[0], False, False);
        end
        else FParentBox.SelectSuccessor(Self);
        Key:=0;
      end;
      VK_UP: begin
        if (FRealItem.MenuIndex = 0) and FParentBox.ParentMenuItem.IsInMenuBar then
          FShadowMenu.SetSelectedMenuItem(FParentBox.ParentMenuItem, False, False)
        else if not IsInMenuBar then
          FParentBox.SelectPrevious(Self);
        Key:=0;
      end;
      VK_DELETE: begin
        Key:=0;
        FShadowMenu.DeleteItem(Self);
      end;
      else inherited KeyDown(Key, Shift);
    end // case
  else inherited KeyDown(Key, Shift);
end;

procedure TShadowItem.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and FRealItem.AutoCheck then begin
    FRealItem.Checked:=not FRealItem.Checked;
    FParentBox.Repaint;
    FShadowMenu.FEditorDesigner.PropertyEditorHook.RefreshPropertyValues;
  end;
  if (FState = dsSelected) then
    SetFocus
  else
    FShadowMenu.SetSelectedMenuItem(FRealItem, False, False);
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TShadowItem.ShowNormal;
begin
  if (FState <> dsNormal) then begin
    FState:=dsNormal;
    Repaint;
  end;
end;

procedure TShadowItem.ShowSelected;
begin
  if (FState <> dsSelected) then begin
    FState:=dsSelected;
    Repaint;
  end;
end;

procedure TShadowItem.ShowDisabled;
begin
  if (FState <> dsDisabled) then begin
    FState:=dsDisabled;
    Repaint;
  end;
end;

procedure TShadowItem.ShowChainToRoot;
var
  sb: TShadowBox;
begin
  sb:=FParentBox;
  while (sb <> FShadowMenu.RootBox) do begin
    sb.Show;
    sb:=sb.ParentBox;
  end;
end;

procedure TShadowItem.HideChainFromRoot;
var
  sb: TShadowBox;
begin
  sb:=FParentBox;
  while (sb <> FShadowMenu.RootBox) do begin
    sb.Hide;
    sb:=sb.ParentBox;
  end;
end;

procedure TShadowItem.ShowChildBox;
var
  sb: TShadowBox;
begin
  if HasChildBox(sb) then
    sb.Show;
end;

constructor TShadowItem.CreateWithBoxAndItem(aSMenu: TShadowMenu;
  aParentBox: TShadowBox; aRealItem: TMenuItem);
begin
  inherited Create(aParentBox, aRealItem);
  Name:='ShadowItem' + IntToStr(ShadowItemID);
  Inc(ShadowItemID);
  FShadowMenu:=aSMenu;
  FParentBox:=aParentBox;
  FParentBox.ShadowList.Add(Self);
  Canvas.Brush.Color:=clBtnFace;
  SetInitialBounds(0, 0, GetWidth, GetHeight);
  if FRealItem.Enabled then
    FState:=dsNormal
  else
    FState:=dsDisabled;
  TabStop:=False;
  TabOrder:= -1;
  PopupMenu:=FShadowMenu.ItemsPopupMenu;
  Parent:=FParentBox;
end;

{ TMenuDesigner }

constructor TMenuDesigner.Create;
begin
  inherited Create;
  FGui:=TMenuDesignerForm.Create(Self);
end;

destructor TMenuDesigner.Destroy;
begin
  FGui.Free;
  if (GlobalDesignHook <> nil) then
    GlobalDesignHook.RemoveAllHandlersForObject(Self);
  inherited Destroy;
end;

procedure TMenuDesigner.CreateShadowMenu(aMenu: TMenu; aSelect: TMenuItem;
  aWidth, aHeight: integer);
begin
  FShadowMenu := TShadowMenu.Create(Self, FGui, aMenu, aSelect, aWidth, aHeight);
end;

{ TMainMenuComponentEditor}

procedure TMainMenuComponentEditor.Edit;
begin
  ShowMenuEditor(Component as TMenu);
end;

function TMainMenuComponentEditor.GetVerbCount: Integer;
begin
  Result:=1;
end;

function TMainMenuComponentEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result:=lisMenuEditorMenuEditor + ' ...';
    else Result:='';
  end;
end;

procedure TMainMenuComponentEditor.ExecuteVerb(Index: Integer);
begin
  if (Index = 0) then
    Edit;
end;

{ TMenuItemsPropertyEditor }

procedure TMenuItemsPropertyEditor.Edit;
var
  mnu: TMenu;
  mnuItem: TMenuItem;
  designer: TComponentEditorDesigner;
begin
  mnuItem:=TMenuItem(GetObjectValue(TMenuItem));
  if (mnuItem <> nil) then
  begin
    mnu:=mnuItem.GetParentMenu;
    designer:=FindRootDesigner(mnu) as TComponentEditorDesigner;
    if (mnu <> nil) and (designer <> nil) then
      ShowMenuEditor(mnu);
  end;
end;

function TMenuItemsPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable, paReadOnly];
end;

initialization
  RegisterComponentEditor(TMenu, TMainMenuComponentEditor);
  RegisterPropertyEditor(TypeInfo(TMenu), TMenu, 'Items', TMenuItemsPropertyEditor);

finalization
  FreeAndNil(MenuDesignerSingleton);

end.

