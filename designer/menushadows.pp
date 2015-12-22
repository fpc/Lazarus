unit MenuShadows;

{$mode objfpc}{$H+}

interface

uses
  ActnList, ButtonPanel, Buttons, Classes, ComponentEditors, Controls, Dialogs, ExtCtrls,
  Forms, Graphics, IDEDialogs, ImgList, LazarusIDEStrConsts, LazUTF8, LCLIntf, LCLProc,
  LCLType, Menus, PropEdits, StdCtrls, SysUtils, Themes, types, typinfo;

const
  Margin = 6;
  Double_Margin = Margin shl 1;

type

TShadowMenu = class;

TShadowBox = class;

{ TFake }

TFake = class(TCustomControl)
strict private
  FAddSubMenu: boolean;
  FShadowMenu: TShadowMenu;
  function GetShouldBeVisible: boolean;
  procedure SetVisibilitySizeAndPosition;
protected
  class function GetControlClassDefaultSize: TSize; override;
  procedure Paint; override;
public
  constructor CreateWithPurpose(anOwner: TShadowMenu; addsASubmenu: boolean);
  procedure Refresh;
  property AddSubMenu: boolean read FAddSubMenu;
  property ShouldBeVisible: boolean read GetShouldBeVisible;
end;

TShadowItemDisplayState = (dsNormal, dsSelected, dsDisabled);

{ TShadowItem }

TShadowItem = class(TCustomControl)
strict private
  FBottomFake: TFake;
  FParentBox: TShadowBox;
  FRealItem: TMenuItem;
  FRightFake: TFake;
  FShadowMenu: TShadowMenu;
  FShowingBottomFake: boolean;
  FShowingRightFake: boolean;
  FState: TShadowItemDisplayState;
  function GetBottomFake: TFake;
  function GetIsInMenuBar: boolean;
  function GetIsMainMenu: boolean;
  function GetLevel: integer;
  function GetMenu: TMenu;
  function GetRightFake: TFake;
  function GetShortcutWidth: integer;
  function GetShowingBottomFake: boolean;
  function GetShowingRightFake: boolean;
  procedure SetState(AValue: TShadowItemDisplayState);
protected
  function GetHeight: integer;
  function GetWidth: integer;
  function HasChildBox(out aChildBox: TShadowBox): boolean;
  procedure DblClick; override;
  procedure HideChainFromRoot;
  procedure HideChildren;
  procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  procedure Paint; override;
  procedure ShowChainToRoot;
  procedure ShowChildBox;
  procedure ShowDisabled;
  procedure ShowNormal;
  procedure ShowSelected;
public
  constructor CreateWithBoxAndItem(aSMenu: TShadowMenu; aParentBox: TShadowBox; aMI: TMenuItem);
  property BottomFake: TFake read GetBottomFake write FBottomFake;
  property IsInMenuBar: boolean read GetIsInMenuBar;
  property IsMainMenu: boolean read GetIsMainMenu;
  property Level: integer read GetLevel;
  property Menu: TMenu read GetMenu;
  property ParentBox: TShadowBox read FParentBox;
  property RealItem: TMenuItem read FRealItem write FRealItem;
  property RightFake: TFake read GetRightFake write FRightFake;
  property ShowingBottomFake: boolean read GetShowingBottomFake write FShowingBottomFake;
  property ShowingRightFake: boolean read GetShowingRightFake write FShowingRightFake;
  property State: TShadowItemDisplayState read FState;
end;

{ TShadowBox }

TShadowBox = class(TCustomControl)
strict private
  FLevel: integer;
  FParentBox: TShadowBox;
  FParentMenuItem: TMenuItem;
  FShadowList: TFPList;
  FShadowMenu: TShadowMenu;
  FUpdating: boolean;
  function GetIsMainMenu: boolean;
  function GetIsMenuBar: boolean;
  function GetRadioGroupList: TStringList;
  function GetShadowCount: integer;
  procedure BeginUpdate;
  procedure EndUpdate;
  procedure ShowAllUnSelected;
protected
  function GetHasRadioItemInfo(out aByteArr: TByteDynArray): boolean;
  function GetInnerDims: TPoint;
  procedure AddItemAndShadow(existingSI: TShadowItem; addBefore:boolean; isSeparator: boolean=False);
  procedure LocateShadows;
  procedure Paint; override;
  procedure RemoveAllSeparators;
  procedure SelectPrevious(aSI: TShadowItem);
  procedure SelectSuccessor(aSI: TShadowItem);
  property IsMainMenu: boolean read GetIsMainMenu;
  property IsMenuBar: boolean read GetIsMenuBar;
  property Level: integer read FLevel;
  property ParentBox: TShadowBox read FParentBox;
  property ParentMenuItem: TMenuItem read FParentMenuItem;
  property ShadowCount: integer read GetShadowCount;
  property ShadowList: TFPList read FShadowList;
  property Updating: boolean read FUpdating;
public
  constructor CreateWithParentBox(aSMenu: TShadowMenu; aParentBox: TShadowBox; aParentItem: TMenuItem);
  destructor Destroy; override;
  procedure SetUnCheckedAllExcept(aMI: TMenuItem);
  property RadioGroupList: TStringList read GetRadioGroupList;
end;

TPopEnum = {%region}
  (popItemAddOnClick, popItemAddBefore, popItemAddAfter, popItemAddSubMenu, popItemDelete,
   popItemAddSep,
   popItemEditCaption, popItemMoveBefore, popItemMoveAfter, popAddImgListIcon,
   popItemSep,
   popSeparators_,
     popAddSeparatorBefore, popAddSeparatorAfter, popRemoveAllSeparators,
   popCheckRadio,
   popShortcuts_,
     popListShortcuts, popListShortcutsAccelerators, popResolveShortcutConflicts,
   popTemplates_,
     popSaveAsTemplate, popAddFromTemplate, popDeleteTemplate);{%endregion}

{ TShadowMenu }

TShadowMenu = class(TCustomPanel)
strict private
  FActionList: TActionList;
  FAddedSingleInitialItem: boolean;
  FAddImgListIconAction: TAction;
  FAddItemFake: TFake;
  FAddSubmenuFake: TFake;
  FBoxList: TFPList;
  FInitialising: boolean;
  FInitialSelectedMenuItem: TMenuItem;
  FIsMainMenu: boolean;
  FItemsPopupMenu: TPopupMenu;
  FLookupRoot: TComponent;
  FRootBox: TShadowBox;
  FSelectedMenuItem: TMenuItem;
  function GetBoxContainingMenuItem(aMI: TMenuItem): TShadowBox;
  function GetBoxCount: integer;
  function GetHighestLevelVisibleBox: TShadowBox;
  function GetMaxVisibleBoxDims(aSB: TShadowBox): TPoint;
  function GetMaxVisibleFakeDims: TPoint;
  function GetSelectedShadowItem: TShadowItem;
  procedure AddManyItems(aPrimaries, aDepth: integer);
  procedure AddSubMenuTo(anExistingSI: TShadowItem);
  procedure ConnectSpeedButtonOnClickMethods;
  procedure CreateShadowBoxesAndItems;
  procedure DeleteChildlessShadowAndItem(anExistingSI: TShadowItem);
  procedure DeleteShadowAndItemAndChildren(anExistingSI: TShadowItem);
  procedure GetUserInitialMenuBuildPolicy;
  procedure OnDesignerModified(Sender: TObject);
  procedure OnObjectPropertyChanged(Sender: TObject; NewObject: TPersistent);
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
  procedure CheckmarkRadioManagement(Sender: TObject);
  procedure DeleteTemplate(Sender: TObject);
  procedure EditCaption(Sender: TObject);
  procedure ListShortcuts(Sender: TObject);
  procedure ListShortcutsAndAccelerators(Sender: TObject);
  procedure MoveItemAfter(Sender: TObject);
  procedure MoveItemBefore(Sender: TObject);
  procedure RemoveAllSeparators(Sender: TObject);
  procedure ResolveShortcutConflicts(Sender: TObject);
  procedure SaveAsTemplate(Sender: TObject);
protected
  FEditorDesigner: TComponentEditorDesigner;
  FMenu: TMenu;
  function GetParentBoxForMenuItem(aMI: TMenuItem): TShadowBox;
  function GetShadowForMenuItem(aMI: TMenuItem): TShadowItem;
  function OnClickIsAssigned(aMI: TMenuItem): boolean;
  procedure AddOnClick(Sender: TObject);
  procedure AdjustSizeAndPosition(Sender: TObject);
  procedure DeleteItem(Sender: TObject);
  function GetBoxWithParentItem(aParentMI: TMenuItem): TShadowBox;
  procedure HideFakes;
  procedure Paint; override;
  procedure RemoveEmptyBox(aSB: TShadowBox);
  procedure SetParent(NewParent: TWinControl); override;
  procedure SetSelectedShadow(const prevSelectedItem, curSelectedItem: TMenuItem; viaDesigner: boolean);
  procedure UpdateActionsEnabledness;
  procedure UpdateBoxLocationsAndSizes;
  property AddItemFake: TFake read FAddItemFake;
  property AddSubmenuFake: TFake read FAddSubmenuFake;
  property BoxCount: integer read GetBoxCount;
  property BoxList: TFPList read FBoxList;
  property ItemsPopupMenu: TPopupMenu read FItemsPopupMenu;
  property RootBox: TShadowBox read FRootBox;
public
  constructor CreateWithMenuAndDims(aMenu: TMenu; aSelect: TMenuItem; aWidth, aHeight: integer);
  destructor Destroy; override;
  procedure HideBoxesAboveLevel(aLevel: integer);
  procedure RefreshFakes;
  procedure SetSelectedMenuItem(aMI: TMenuItem; viaDesigner, prevWasDeleted: boolean);
  procedure UpdateSelectedItemInfo;
  property AddedSingleInitialItem: boolean read FAddedSingleInitialItem write FAddedSingleInitialItem;
  property IsMainMenu: boolean read FIsMainMenu;
  property LookupRoot: TComponent read FLookupRoot;
  property SelectedMenuItem: TMenuItem read FSelectedMenuItem;
  property SelectedShadowItem: TShadowItem read GetSelectedShadowItem;
end;

{ TScrollPanel }

  TScrollPanel = class(TCustomPanel)
  strict private
    FChildControl: TWinControl;
    FHeightDim: integer;
    FHMax: integer;
    FHSBar: TScrollBar;
    FVMax: integer;
    FVSBar: TScrollBar;
    FWidthDim: integer;
    procedure HScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure OnChildControlResize(Sender: TObject);
    procedure SetLargeChange(AValue: integer);
    procedure VScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
  protected
    procedure SetParent(NewParent: TWinControl); override;
  public
    constructor CreateWithChild(TheOwner: TComponent; aChild: TWinControl);
    property ChildControl: TWinControl read FChildControl write FChildControl;
    property HMax: integer read FHMax;
    property HSBar: TScrollBar read FHSBar;
    property VMax: integer read FVMax;
    property VSBar: TScrollBar read FVSBar;
  end;

TSCKind = (scUnknown,
           scMenuItemSC, scMenuItemKey2, scMenuItemAccel,
           scActionSC, scActionSecondary, scActionAccel,
           scOtherCompAccel);

const
  Accelerator_Kinds = [scMenuItemAccel, scActionAccel, scOtherCompAccel];
  MenuItem_Kinds = [scMenuItemSC, scMenuItemKey2, scMenuItemAccel];
  ShortcutOnly_Kinds = [scMenuItemSC, scMenuItemKey2, scActionSC, scActionSecondary];

type

 { TSCInfo }

TSCInfo = class(TObject)
strict private
  FComponent: TComponent;
  FComponentName: string;
  FKind: TSCKind;
  FShortcut: TShortCut;
  function GetAction: TAction;
  function GetCaption: string;
  function GetMenuItem: TMenuItem;
  function GetToCompositeString: string;
public
  constructor CreateWithParams(aComponent: TComponent; aKind: TSCKind; aSC: TShortCut);
  property Action: TAction read GetAction;
  property Caption: string read GetCaption;
  property Component: TComponent read FComponent;
  property ComponentName: string read FComponentName;
  property Kind: TSCKind read FKind;
  property MenuItem: TMenuItem read GetMenuItem;
  property Shortcut: TShortCut read FShortcut;
  property ToCompositeString: string read GetToCompositeString;
end;

  { TSCList }

  TSCList = class(TObject)
  strict private
    FAcceleratorsInContainerCount: integer;
    FInitialDuplicates: TFPList;
    FScanList: TStringList;
    FShortcutsInContainerCount: integer;
    FUniqueList: TFPList;
    function GetInitialDuplicatesCount: integer;
    function GetScanListCompName(index: integer): string;
    function GetUniqueCount: integer;
  public
    constructor Create;
    destructor Destroy; override;
    function FindUniqueInfoForShortcut(aSC: TShortCut): TSCInfo;
    function UniqueListContainsShortcut(aSC: TShortCut): boolean;
    procedure ClearAllLists;
    procedure ScanContainerForShortcutsAndAccelerators;
    procedure ScanContainerForShortcutsOnly;
    procedure ScanSCListForDuplicates;
    procedure SortByComponentPropertyName;
    property AcceleratorsInContainerCount: integer read FAcceleratorsInContainerCount write FAcceleratorsInContainerCount;
    property InitialDuplicates: TFPList read FInitialDuplicates;
    property InitialDuplicatesCount: integer read GetInitialDuplicatesCount;
    property ScanList: TStringList read FScanList;
    property ScanListCompName[index: integer]: string read GetScanListCompName;
    property ShortcutsInContainerCount: integer read FShortcutsInContainerCount write FShortcutsInContainerCount;
    property UniqueCount: integer read GetUniqueCount;
  end;

// utility functions

function AIsDescendantOfB(miA, miB: TMenuItem): boolean;

function AmpersandStripped(const aText: string): string;

//function CommaTextIntoTwo(const aText: string; out a2ndHalf: string): string;

function GetAcceleratedItemsCount(aMenu: TMenu): integer;

function GetChildSeparatorCount(aMI: TMenuItem): integer;

function GetMenuBarIconWidth(aMI: TMenuItem): integer;

function GetNestingLevelDepth(aMenu: TMenu): integer;

function GetNewCaptionFor(aSI: TShadowItem): string;

function GetNextItem(aMI: TMenuItem): TMenuItem;

function GetNextNonSepItem(aMI: TMenuItem): TMenuItem;

function GetPreviousItem(aMI: TMenuItem): TMenuItem;

function GetPreviousNonSepItem(aMI: TMenuItem): TMenuItem;

function GetSavedTemplatesCount: integer;

function GetStringWidth(const aText: string; isBold: boolean): integer;

function HasAccelerator(const aText: string; out aShortcut: TShortCut): boolean;

function ItemStateToStr(aState: TShadowItemDisplayState): string;

function KindToPropertyName(aKind: TSCKind): string;

function LevelZeroAndNoGrandchildren(aMI: TMenuItem): boolean;

function NextItemIsSeparator(aMI: TMenuItem): boolean;

function PreviousItemIsSeparator(aMI: TMenuItem): boolean;

function SavedTemplatesExist: boolean;

function SplitCommaText(const aCommaText: string; out firstBit: string): string;

procedure DoShortcutAccelScanCount(const aSCList: TSCList; shortcutsOnly: boolean);

// utility dialogs

function AddNewOrEditShortcutDlg(aMI: TMenuItem; isMainSCut: boolean; var aShortcut: TShortCut): boolean;

function DeleteMenuTemplateDlg: boolean;

function DlgChooseIconFromImageList(anImageList: TCustomImageList): integer;

function EditCaptionDlg(aMI: TMenuItem; var aShortcut: TShortCut): boolean;

function GetCheckMarkPropertiesDlg(aMI: TMenuItem; const aGroupIndexArr: TByteDynArray;
                               aShadowMenu: TShadowMenu; out aChecked: boolean;
                               out anAutoCheck: boolean; out anAlwaysShowCheckable: boolean;
                               out aRadioItem: boolean; out aGroupIndex: byte): boolean;

function InsertMenuTemplateDlg: TMenuItem;

function ListShortCutDlg(shortcutsOnly: boolean; aMenu: TMenu=nil): TModalResult;

function ShowMultiItemDlg(const aSMenu: TShadowMenu; out primaryItemCount, subMenuDepth: integer): TModalResult;

procedure SaveMenuTemplateDlg(aMenuItem: TMenuItem);

function ResolvedConflictsDlg: TModalResult;

function NewShortcutOrCaptionIsValidDlg(aConflictingInfo: TSCInfo;
                                        out aNewShortcut: TShortCut;
                                        out aNewCaption: string): boolean;


implementation

uses MenuEditorForm, Laz2_XMLCfg, LazFileUtils, ComCtrls;

const
  MenuBar_Height = 20;
  Separator_Height = 7;
  Separator_Centre = 3;
  DropDown_Height = 24;
  Shortcut_Offset = 23;
  MenuBar_Text_Offset = 7;
  Double_MenuBar_Text_Offset = MenuBar_Text_Offset shl 1;
  DropDown_Text_Offset = 35;
  Double_DropDown_Text_Offset = DropDown_Text_Offset shl 1;
  Treble_DropDown_Text_Offset = 3*DropDown_Text_Offset;
  Gutter_Offset = 6;
  Gutter_X = DropDown_Text_Offset - Gutter_Offset;
  Popup_Origin: TPoint = (x:15; y:15);
  Ampersand_Char = '&';
  Leading = 4;
  Double_Leading = Leading shl 1;
  Treble_Leading = Leading + Double_Leading;
  VDim = 20;
  VTextOffset = 2;
  Header_Color = TColor($00EDEFD6);
  ShortCutKeys: array[0..48] of word = (VK_UNKNOWN,   //#todo extend this list, or use one from elsewhere in LCL?
    VK_0, VK_1, VK_2, VK_3, VK_4, VK_5, VK_6, VK_7, VK_8, VK_9,
    VK_A, VK_B, VK_C, VK_D, VK_E, VK_F, VK_G, VK_H, VK_I, VK_J, VK_K, VK_L,
    VK_M, VK_N, VK_O, VK_P, VK_Q, VK_R, VK_S, VK_T, VK_U, VK_V, VK_W, VK_X,
    VK_Y, VK_Z, VK_F1, VK_F2, VK_F3, VK_F4, VK_F5, VK_F6, VK_F7, VK_F8,
    VK_F9, VK_F10, VK_F11, VK_F12);

  MenuTemplatesFilename='menutemplates.xml';

var
  ShadowItemID: integer = 0;
  ShadowBoxID: integer = 0;

type

{ TLineEditor }

TLineEditor = class(TForm)
strict private
  FEdit: TEdit;
  function GetEditedLine: string;
protected
  procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
public
  constructor CreateWithShadowItem(anOwner: TComponent; aSI: TShadowItem);
  property EditedLine: string read GetEditedLine;
end;

{ TMultiItemDlg }

TMultiItemDlg = class(TForm)
strict private
  FButtonPanel: TButtonPanel;
  FPrimaryItemCountRadioBox: TRadioGroup;
  FShadowMenu: TShadowMenu;
  FSubMenuDepthRadioGroup: TRadioGroup;
  function GetPrimaryItemCount: integer;
  function GetSubMenuDepth: integer;
  procedure RadioGroupSelectionChanged(Sender: TObject);
public
  constructor CreateWithShadowMenu(aSMenu: TShadowMenu);
  property PrimaryItemCount: integer read GetPrimaryItemCount;
  property SubMenuDepth: integer read GetSubMenuDepth;
end;

{ TAddShortcutDialog }

TAddShortcutDialog = class(TForm)
strict private
  FButtonPanel: TButtonPanel;
  FMenuItem: TMenuItem;
  FNewShortcut: TShortCut;
  FOldShortcut: TShortCut;
  FShortCutGrabBox: TShortCutGrabBox;
  procedure OKButtonClick(Sender: TObject);
  procedure OnGrabBoxCloseUp(Sender: TObject);
public
  constructor CreateWithMenuItem(AOwner: TComponent; aMI: TMenuItem; isMainSC: boolean; aSC: TShortCut);
  property NewShortcut: TShortCut read FNewShortcut;
  property OldShortcut: TShortCut write FOldShortcut;
end;

TDisplayType = (dtNone, dtBlack, dtBlackBold, dtGreyed, dtGreyedBold);

TDisplayClickEvent = procedure(isHeader: boolean; index: integer) of object;

TDualDisplay = class;

TContents = class(TCustomControl)
  private
    FCol1MaxTextWidth: integer;
    FCol2MaxTextWidth: integer;
    FDualDisplay: TDualDisplay;
    FOnContentsClick: TModalDialogFinished;
    FSList: TStringList;
  protected
    procedure DoContentsClick(anIndex: integer);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    property Col1MaxTextWidth: integer read FCol1MaxTextWidth;
    property Col2MaxTextWidth: integer read FCol2MaxTextWidth;
    property SList: TStringList read FSList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddToList(const aLine: string; aDisplayType: TDisplayType=dtBlack);
    procedure Clear;
    property OnContentsClick: TModalDialogFinished read FOnContentsClick write FOnContentsClick;
  end;

  { THeader }

  THeader = class(TCustomControl)
  private
    FCol1Header: string;
    FCol2Header: string;
    FColumn1TextWidth: integer;
    FDisplayType: TDisplayType;
    FDualDisplay: TDualDisplay;
    FOnHeaderClick: TModalDialogFinished;
  protected
    procedure DoHeaderClick(anIndex: integer);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AddHeader(const aHeader: string; aDisplayType: TDisplayType);
    procedure Clear;
    property Column1TextWidth: integer read FColumn1TextWidth;
    property OnHeaderClick: TModalDialogFinished read FOnHeaderClick write FOnHeaderClick;
  end;

  { TDualDisplay }

  TDualDisplay = class(TCustomControl)
  private
    FCol1Right: integer;
    FContents: TContents;
    FHeader: THeader;
    FOnDisplayClick: TDisplayClickEvent;
    FSBox: TScrollBox;
    FUpdating: boolean;
    function GetContentsCount: integer;
    procedure HeaderContentsClick(Sender: TObject; index: integer);
    procedure SetCol1Right(AValue: integer);
  protected
    class function GetControlClassDefaultSize: TSize; override;
    function TextWidth(const aText: string): integer;
    property Updating: boolean read FUpdating;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AddHeader(const aHeader: string; aDT: TDisplayType=dtBlackBold);
    procedure AddLine(const aLine: string; aDT: TDisplayType=dtBlack);
    procedure BeginUpdate;
    procedure Clear;
    procedure ClearContents;
    procedure ClearHeader;
    procedure EndUpdate;
    procedure InvalidateContents;
    property Col1Right: integer read FCol1Right write SetCol1Right;
    property ContentsCount: integer read GetContentsCount;
    property OnDisplayClick: TDisplayClickEvent read FOnDisplayClick write FOnDisplayClick;
  end;

{ TShortcutDisplayDlg }

TShortcutDisplayDlg = class(TForm)
strict private
  FLastSortIndex: integer;
  FMenu: TMenu;
  FscList: TStringList;
  FSingleMenuOnly: boolean;
  FShortcutsOnly: boolean;
  // GUI
  FBPanel: TButtonPanel;
  FDualDisplay: TDualDisplay;
  FGBDisplay: TGroupBox;
  FLabel: TLabel;
  procedure DisplayAllDlgClick(isHeader: boolean; index: integer);
  procedure DisplaySingleMenuClick(isHeader: boolean; index: integer);
  procedure UpdateContents(singleMenuOnly: boolean=False);
  procedure UpdateFromMenu(anIndex: integer= -1);
public
  constructor CreateWithShortcutsOnly(shortcutsOnly: boolean; aMenu: TMenu=nil);
  destructor Destroy; override;
end;

  { TEditCaptionDialog }

TEditCaptionDialog = class(TForm)
  strict private
    FButtonPanel: TButtonPanel;
    FEdit: TEdit;
    FGBEdit: TGroupBox;
    FMenuItem: TMenuItem;
    FNewShortcut: TShortCut;
    FOldShortcut: TShortCut;
    procedure EditOnChange(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  public
    constructor CreateWithMenuItem(AOwner: TComponent; aMI: TMenuItem; aSC: TShortCut);
    property NewShortcut: TShortCut read FNewShortcut;
    property OldShortcut: TShortCut write FOldShortcut;
  end;

{ TCheckMarkDialog }

TCheckMarkDialog = class(TForm)
  procedure OnIdle(Sender: TObject; var {%H-}Done: Boolean);
strict private
  FButtonPanel: TButtonPanel;
  FCheckMarkGroup: TCheckGroup;
  FDisplayGroupIndexes: TDualDisplay;
  FGBProperties: TGroupBox;
  FGroupIndexArray: TByteDynArray;
  FLGroupIndex: TLabel;
  FMenuItem: TMenuItem;
  FRadioGroupCombo: TComboBox;
  FShadowMenu: TShadowMenu;
  FStatusBar: TStatusBar;
  function GetAutoCheck: boolean;
  function GetChecked: boolean;
  function GetGroupIndex: byte;
  function GetRadioItem: boolean;
  function GetShowAlwaysCheckable: boolean;
  procedure CheckMarkGroupItemClick(Sender: TObject; Index: integer);
  procedure SetIdleEvent(enableIt: boolean);
public
  constructor CreateWithMenuItem(AOwner: TComponent; aMI: TMenuItem;
                                 aByteArr: TByteDynArray; aShadowMenu: TShadowMenu);
  property AutoCheck: boolean read GetAutoCheck;
  property Checked: boolean read GetChecked;
  property GroupIndex: byte read GetGroupIndex;
  property RadioItem: boolean read GetRadioItem;
  property ShowAlwaysCheckable: boolean read GetShowAlwaysCheckable;
end;

TRadioIconGroup = class;

TRadioIconState = (risUp, risDown, risPressed, risUncheckedHot, risCheckedHot);

    { TRadioIcon }

TRadioIcon = class(TGraphicControl)
strict private
  FBGlyph: TButtonGlyph;
  FOnChange: TNotifyEvent;
  FRIGroup: TRadioIconGroup;
  FRIState: TRadioIconState;
  function GetChecked: Boolean;
  procedure SetChecked(aValue: Boolean);
protected
  procedure DoChange;
  procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  procedure MouseEnter; override;
  procedure MouseLeave; override;
  procedure Paint; override;
public
  constructor CreateWithGlyph(aRIGroup: TRadioIconGroup; anImgIndex: integer);
  destructor Destroy; override;
  property Checked: Boolean read GetChecked write SetChecked;
  property OnChange: TNotifyEvent read FOnChange write FOnChange;
end;

{ TRadioIconGroup }

TRadioIconGroup = class(TScrollBox)
strict private
  FItemIndex: integer;
  FOnSelectItem: TNotifyEvent;
  FRIArray: array of TRadioIcon;
  procedure CreateRadioItems;
  procedure ApplyLayout;
  procedure RIOnChange(Sender: TObject);
  procedure DoSelectItem;
protected
  FImageList: TCustomImageList;
  FedSize: TSize;
  FedUnchecked, FedChecked, FedPressed, FedUncheckedHot, FedCheckedHot: TThemedElementDetails;
  FGlyphPt: TPoint;
  FSpacing: integer;
  FRadioHeight, FRadioWidth: integer;
  FRadioRect: TRect;
  procedure SetParent(NewParent: TWinControl); override;
public
  constructor CreateWithImageList(AOwner: TComponent; anImgList: TCustomImageList);
  property ItemIndex: integer read FItemIndex;
  property OnSelectItem: TNotifyEvent read FOnSelectItem write FOnSelectItem;
end;

{ TdlgChooseIcon }

TdlgChooseIcon = class(TForm)
  private
    FButtonPanel: TButtonPanel;
    FRadioIconGroup: TRadioIconGroup;
    function GetImageIndex: integer;
    procedure RIGClick(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure SetRadioIconGroup(anImageList: TCustomImageList);
    property ImageIndex: integer read GetImageIndex;
  end;

{ TMenuTemplate }

 TMenuTemplate = class(TObject)
 strict private
   FDescription: string;
   FIsStandardTemplate: boolean;
   FPrimaryItem: string;
   FSubList: TStringList;
   function GetShortcut(index: integer): TShortCut;
   function GetSubItem(index: integer): string;
   function GetSubItemCount: integer;
 public
   class function MenuItemToString(aMenuItem: TMenuItem; aDescription: string): string;
   constructor CreateFromString(const aMenuString: string);
   destructor Destroy; override;
   function ReadFromString(const aString: string): boolean;
   property Description: string read FDescription write FDescription;
   property IsStandardTemplate: boolean read FIsStandardTemplate write FIsStandardTemplate;
   property PrimaryItem: string read FPrimaryItem;
   property Shortcut[index: integer]: TShortCut read GetShortcut;
   property SubItem[index: integer]: string read GetSubItem;
   property SubItemCount: integer read GetSubItemCount;
 end;

 TDialogMode = (dmInsert, dmSave, dmDelete);

 { TMenuTemplates }

 TMenuTemplates = class(TObject)
 strict private
   FTemplateList: TFPList;
   function GetDescription(index: integer): string;
   function GetMenu(index: integer): TMenuItem;
   function GetMenuCount: integer;
   function GetMenuTemplate(index: integer): TMenuTemplate;
   function GetPrimaryItem(index: integer): string;
   procedure CheckIndex(anIndex: integer);
   procedure LoadDefaultTemplates;
   procedure LoadSavedTemplates;
 public
   constructor CreateForMode(aDialogMode: TDialogMode=dmInsert);
   destructor Destroy; override;
   function GetIndexOfTemplate(aMT: TMenuTemplate): integer;
   procedure AddTemplate(const aTemplateText: string; isStandard: boolean=True);
   procedure SaveTemplateToConfig(aMenuTemplate: TMenuTemplate);
   property Description[index: integer]: string read GetDescription;
   property Menu[index: integer]: TMenuItem read GetMenu;
   property MenuCount: integer read GetMenuCount;
   property MenuTemplate[index: integer]: TMenuTemplate read GetMenuTemplate;
   property PrimaryItem[index: integer]: string read GetPrimaryItem;
 end;

{ TPreview }

TPreview = class(TGraphicControl)
 strict private
   FDisplayAsPopup: boolean;
   FTemplate: TMenuTemplate;
   function GetSize: TSize;
   procedure SetDisplayAsPopup(AValue: boolean);
 protected
   procedure Paint; override;
   procedure SetParent(NewParent: TWinControl); override;
 public
   property DisplayAsPopup: boolean read FDisplayAsPopup write SetDisplayAsPopup;
   procedure Clear;
   procedure LoadTemplate(aMenuTemplate: tmenuTemplate);
end;

{ TMenuTemplateDialog }

TMenuTemplateDialog = class(TForm)
  strict private
    // GUI
    FBCancel: TBitBtn;
    FBExecute: TBitBtn;
    FCBDisplay: TCheckBox;
    FEDescription: TEdit;
    FGChoose: TGroupBox;
    FLDescription: TLabel;
    FMenuToSave: TMenuItem;
    FPButtons: TCustomPanel;
    FPDescription: TPanel;
    FPreview: TPreview;
    FPRight: TCustomPanel;
    FScrollBoxPreview: TScrollBox;
    FSplitter: TSplitter;
    FTVTemplates: TTreeView;
    // data
    FDialogMode: TDialogMode;
    FMenuToInsert: TMenuItem;
    FNewMenuTemplate: TMenuTemplate;
    FNoneSavedNode: TTreeNode;
    FSavedNode: TTreeNode;
    FStandardNode: TTreeNode;
    FTemplates: TMenuTemplates;
    procedure BExecuteDeleteClick(Sender: TObject);
    procedure BExecuteInsertClick(Sender: TObject);
    procedure BExecuteSaveClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure CBDisplayChange(Sender: TObject);
    procedure DeleteSelectedFromConfigFile;
    procedure DeleteSelectedTemplate;
    procedure EDescriptionChange(Sender: TObject);
    procedure PopulateTreeView;
    procedure SaveMenuAsTemplate;
    procedure SetupGUI;
    procedure ShowPreview(aMenuTemplate: TMenuTemplate);
    procedure TVAdvancedCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
                State: TCustomDrawState; Stage: TCustomDrawStage;
                var {%H-}PaintImages, DefaultDraw: Boolean);
    procedure TVEditing(Sender: TObject; {%H-}Node: TTreeNode; var AllowEdit: Boolean);
    procedure TVSelectionChanged(Sender: TObject);
  protected
    procedure DoShowWindow; override;
  public
    constructor CreateWithMode(AOwner: TComponent; aDialogMode: TDialogMode=dmInsert);
    destructor Destroy; override;
    property MenuToInsert: TMenuItem read FMenuToInsert;
    property MenuToSave: TMenuItem read FMenuToSave write FMenuToSave;
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
  procedure ConflictsBoxSelectionChange(Sender: TObject; {%H-}User: boolean);
  procedure CreateListboxItems;
  procedure InitialPopulateListBox;
  procedure OKButtonClick(Sender: TObject);
  procedure RePopulateListBox;
  procedure UpdateStatistics;
public
  constructor {%H-}Create(TheOwner: TComponent);
  destructor Destroy; override;
end;

{ TEditShortcutCaptionDialog }

TEditShortcutCaptionDialog = class(TForm)
strict private
    FEditingCaption: boolean;
    FInfo: TSCInfo;
    FNewCaption: string;
    FNewShortcut: TShortCut;
    FOldCaption: string;
    // GUI controls
    FButtonPanel: TButtonPanel;
    FEdit: TEdit;
    FGrabBox: TCustomShortCutGrabBox;
    FGroupBox: TGroupBox;
    procedure CaptionEditChange(Sender: TObject);
    procedure GrabBoxEnter(Sender: TObject);
    procedure GrabBoxExit(Sender: TObject);
    procedure OKButtonOnClick(Sender: TObject);
  protected
    procedure Activate; override;
  public
    constructor {%H-}CreateNew(anOwner: TComponent; aSCInfo: TSCInfo);
    property NewCaption: string read FNewCaption;
    property NewShortcut: TShortCut read FNewShortcut;
  end;

// utility functions

function GetSavedTemplatesCount: integer;
var
  mt: TMenuTemplates;
begin
  mt:=TMenuTemplates.CreateForMode(dmDelete);
  try
    Result:=mt.MenuCount;
  finally
    mt.Free;
  end;
end;

function GetStringWidth(const aText: string; isBold: boolean): integer;
var
  cnv: TCanvas;
begin
  cnv:=MenuDesigner.Canvas;
  if isBold then
    cnv.Font.Style:=[fsBold]
  else cnv.Font.Style:=[];
  Result:=cnv.TextWidth(aText);
end;

function ItemStateToStr(aState: TShadowItemDisplayState): string;
begin
  Result:=GetEnumName(TypeInfo(TShadowItemDisplayState), Ord(aState));
end;

function GetMenuBarIconWidth(aMI: TMenuItem): integer;
begin
  Result:=0;
  if aMI.IsInMenuBar then begin
    if aMI.HasIcon and (aMI.ImageIndex > -1) and
       (MenuDesigner.EditedMenu.Images <> nil) then
         Inc(Result, MenuDesigner.EditedMenu.Images.Width)
    else if (aMI.Bitmap <> nil) and not aMI.Bitmap.Empty then
      Inc(Result, aMI.Bitmap.Width);
    if (Result > 24) then
      Result:=24;
  end;
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
  else Result:=aMI.Parent.Items[Pred(idx)];
end;

function GetNextItem(aMI: TMenuItem): TMenuItem;
var
  idx: integer;
begin
  idx:=aMI.MenuIndex;
  if (idx = Pred(aMI.Parent.Count)) then
    Exit(nil)
  else Result:=aMI.Parent.Items[Succ(idx)];
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
  dlg:=TLineEditor.CreateWithShadowItem(nil, aSI);
  try
    mr:=dlg.ShowModal;
    if (mr = mrOK) then
      Result:=dlg.EditedLine
    else Result:='';
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
  for i:=0 to aMI.Count-1 do begin
    if aMI.Items[i].IsLine then
      Inc(Result);
  end;
end;

function HasAccelerator(const aText: string; out aShortcut: TShortCut): boolean;
var
  p: integer;
  aCopy, foundChar, accelStr: string;
begin
  if (aText = '') then begin
    aShortcut:=0;
    Exit(False);
  end;
  aCopy:=aText;
  Result:=False;
  p:=LazUTF8.UTF8Pos(Ampersand_Char, aCopy);
  while (p > 0) and (p < LazUTF8.UTF8Length(aCopy)) do
  begin
    foundChar:=LazUTF8.UTF8Copy(aCopy, p+1, 1);
    if (foundChar <> Ampersand_Char) then begin
      accelStr:=LazUTF8.UTF8UpperCase(foundChar); // force uppercase
      Result:=True;
      Break;
    end
    else begin
      LazUTF8.UTF8Delete(aCopy, 1, p+1);
      p:=LazUTF8.UTF8Pos(Ampersand_Char, aCopy);
    end;
  end;
  if Result then
    aShortcut:=KeyToShortCut(Ord(accelStr[1]),
    {$if defined(darwin) or defined(macos) or defined(iphonesim)} [ssMeta]
    {$else} [ssAlt] {$endif})
  else aShortcut:=0;
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

function SplitCommaText(const aCommaText: string; out firstBit: string): string;
var
  p: integer;
begin
  if (aCommaText = '') then begin
    firstBit:='';
    Exit('');
  end;
  p:=Pos(',', aCommaText);
  if (p = 0) then begin
    firstBit:=aCommaText;
    Exit('');
  end;
  firstBit:=Copy(aCommaText, 1, Pred(p));
  Result:=Copy(aCommaText, Succ(p), Length(aCommaText)-p);
end;

function KindToPropertyName(aKind: TSCKind): string;
begin
   case aKind of
    scUnknown:   Result:='<unknown property>';
    scActionAccel, scMenuItemAccel, scOtherCompAccel:
                  Result:='Caption';
    scActionSC, scMenuItemSC: Result:='ShortCut';
    scActionSecondary: Result:='SecondaryShortcuts';
    scMenuItemKey2:    Result:='ShortCutKey2';
  end;
end;

function GetAcceleratedItemsCount(aMenu: TMenu): integer;
var
  i: integer;

  procedure RecursiveCountAcceleratedCaptions(aMI: TMenuItem);
  var
    j: integer;
    sc: TShortCut;
  begin
    if HasAccelerator(aMI.Caption, sc) then
      Inc(Result);
    for j:=0 to aMI.Count-1 do
      RecursiveCountAcceleratedCaptions(aMI.Items[j]);
  end;

begin
  Result:=0;
  for i:=0 to aMenu.Items.Count-1 do
    RecursiveCountAcceleratedCaptions(aMenu.Items[i]);
end;

function SavedTemplatesExist: boolean;
var
  XMLConfig: TXMLConfig;
  cfgPath, s, templateCfgName: string;

  function InvalidXML: boolean; // perform a quick check, far from a full validation
  var
    sl: TStringList;
    tr0, s: string;
  begin
    sl:=TStringList.Create;
    try
      sl.LoadFromFile(templateCfgName);
      if (sl.Count < 3) then
        Exit(True);
      tr0:=Trim(sl[0]);
      s:=Copy(tr0, 1, 15);
      if not SameText(s, '<?xml version="') then
        Exit(True);
      s:=Copy(tr0, Length(tr0) - 17, 18);
      if not SameText(s, 'encoding="UTF-8"?>') then
        Exit(True);
      if not SameText(Trim(sl[1]), '<CONFIG>') then
        Exit(True);
      if not SameText(Trim(sl[Pred(sl.Count)]), '</CONFIG>') then
        Exit(True);
      Result:=False;
    finally
       sl.Free;
    end;
  end;

begin
  cfgPath:=SetDirSeparators(ExtractFilePath(ChompPathDelim(SysToUTF8(GetAppConfigDir(False)))) + 'lazarus');
  templateCfgName:=cfgPath + DirectorySeparator + MenuTemplatesFilename;
  if not FileExistsUTF8(templateCfgName) then
      Exit(False);
  if InvalidXML then // file is corrupted or not XML, so discard to prevent exception
    DeleteFile(templateCfgName);
  XMLConfig:=TXMLConfig.Create(templateCfgName);
  try
    s:=XMLConfig.GetValue('menu_1/Name/Value', 'missing_Menu_1_Name');
    Result:=(CompareText(s, 'missing_Menu_1_Name') = 0);
  finally
    XMLConfig.Free;
  end;
end;

procedure DoShortcutAccelScanCount(const aSCList: TSCList; shortcutsOnly: boolean);
var
  dm: TDataModule;
  frm: TCustomForm;
  i, a: integer;
  aLst: TActionList;
  ac: TAction;
  sc: TShortCut;
  container: TComponent;

  procedure AddInfoToScanList(aComp: TComponent; aSC: TShortCut; aKind: TSCKind);
  var
    isAccel: boolean;
  begin
    isAccel:=(aKind in Accelerator_Kinds);
    if isAccel and not shortcutsOnly then
      aSCList.AcceleratorsInContainerCount:=aSCList.AcceleratorsInContainerCount+1
    else aSCList.ShortcutsInContainerCount:=aSCList.ShortcutsInContainerCount+1;
    aSCList.ScanList.AddObject(ShortCutToText(aSC), TSCInfo.CreateWithParams(aComp, aKind, aSC));
  end;

  procedure ScanMenu(aMenu: TMenu);
  var
    i: integer;

    procedure RecursiveScanItem(anItem:TMenuItem);
    var
      j: integer;
      sc: TShortCut;
    begin
      if (anItem.ShortCut <> 0) then
        AddInfoToScanList(anItem, anItem.ShortCut, scMenuItemSC);
      if (anItem.ShortCutKey2 <> 0) then
        AddInfoToScanList(anItem, anItem.ShortCutKey2, scMenuItemKey2);
      if not shortcutsOnly and HasAccelerator(anItem.Caption, sc) then
        AddInfoToScanList(anItem, sc, scMenuItemAccel);
      for j:=0 to anItem.Count-1 do
        RecursiveScanItem(anItem.Items[j]);
    end;

  begin
    for i:=0 to aMenu.Items.Count-1 do
      RecursiveScanItem(aMenu.Items[i]);
  end;

begin
  container:=GlobalDesignHook.LookupRoot as TComponent;
  aSCList.ClearAllLists;
  aSCList.AcceleratorsInContainerCount:=0;
  aSCList.ShortcutsInContainerCount:=0;
  if (container is TDataModule) then begin
    dm:=TDataModule(container);
    for i:=0 to dm.ComponentCount-1 do
      if (dm.Components[i] is TMenu) then
        ScanMenu(TMenu(dm.Components[i]));
  end
  else if (container is TCustomForm) then begin
    frm:=TCustomForm(container);
    for i:=0 to frm.ComponentCount-1 do
      if (frm.Components[i] is TMenu) then
        ScanMenu(TMenu(frm.Components[i]))
      else if (frm.Components[i] is TActionList) then begin
        aLst:=TActionList(frm.Components[i]);
        for a:=0 to aLst.ActionCount-1 do begin
          ac:=TAction(aLst.Actions[a]);
          if (ac.ShortCut > 0) then
            AddInfoToScanList(ac, ac.ShortCut, scActionSC);
          if (ac.SecondaryShortCuts.Count > 0) then
            AddInfoToScanList(ac, ac.SecondaryShortCuts.ShortCuts[0], scActionSecondary);
          if not shortcutsOnly and HasAccelerator(ac.Caption, sc) then
            AddInfoToScanList(ac, sc, scActionAccel);
        end;
      end
      else if not shortcutsOnly and (frm.Components[i] is TControl) and
             HasAccelerator(TControl(frm.Components[i]).Caption, sc) then
               AddInfoToScanList(frm.Components[i], sc, scOtherCompAccel);
  end;
  Assert(aSCList.AcceleratorsInContainerCount+aSCList.ShortcutsInContainerCount=
         aSCList.ScanList.Count,'DoShortcutAccelScanCount: internal counting error');
end;

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

function AmpersandStripped(const aText: string): string;
var
  p: integer;
begin
  Result:=aText;
  p:=Pos(Ampersand_Char, Result);
  while (p > 0) do begin
    Delete(Result, p, 1);
    p:=Pos(Ampersand_Char, Result);
  end;
end;

function GetNestingLevelDepth(aMenu: TMenu): integer;
var
  i: integer;

  procedure CheckLevel(aMI: TMenuItem; aLevel: integer);
  var
    j: integer;
  begin
    if (aMI.Count > 0) then begin
      if (Succ(aLevel) > Result) then
        Result:=Succ(aLevel);
      for j:=0 to aMI.Count-1 do
        CheckLevel(aMI.Items[j], Succ(aLevel));
    end;
  end;

begin
  Result:=0;
  for i:=0 to aMenu.Items.Count-1 do
    CheckLevel(aMenu.Items[i], 0);
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
  else Result:=0;
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
  else Result:=0;
end;

function ShowMultiItemDlg(const aSMenu: TShadowMenu; out primaryItemCount,
    subMenuDepth: integer): TModalResult;
var
  dlg: TMultiItemDlg;
begin
  primaryItemCount:=0;
  subMenuDepth:=0;
  dlg:=TMultiItemDlg.CreateWithShadowMenu(aSMenu);
  try
    Result:=dlg.ShowModal;
    if (Result = mrOK) then begin
      primaryItemCount:=dlg.PrimaryItemCount;
      if not aSMenu.IsMainMenu then
        Dec(primaryItemCount);
      if aSMenu.IsMainMenu then
        subMenuDepth:=dlg.SubMenuDepth;
    end;
  finally
    dlg.Free;
  end;
end;

function ListShortCutDlg(shortcutsOnly: boolean; aMenu: TMenu): TModalResult;
var
  dlg: TShortcutDisplayDlg;
begin
  dlg:=TShortcutDisplayDlg.CreateWithShortcutsOnly(shortcutsOnly, aMenu);
  try
    Result:=dlg.ShowModal;
  finally
    FreeAndNil(dlg);
  end;
end;

function AddNewOrEditShortcutDlg(aMI: TMenuItem; isMainSCut: boolean;
  var aShortcut: TShortCut): boolean;
var
  dlg: TAddShortcutDialog;
begin
  dlg:=TAddShortcutDialog.CreateWithMenuItem(nil, aMI, isMainSCut, aShortcut);
  try
    if (dlg.ShowModal = mrOK) then
      begin
        aShortcut:=dlg.NewShortcut;
        Result:=True;
      end
    else Result:=False;
  finally
    dlg.Free;
  end;
end;

function EditCaptionDlg(aMI: TMenuItem; var aShortcut: TShortCut): boolean;
var
  dlg: TEditCaptionDialog;
begin
  dlg:=TEditCaptionDialog.CreateWithMenuItem(nil, aMI, aShortcut);
  try
    if (dlg.ShowModal = mrOK) then
      begin
        aShortcut:=dlg.NewShortcut;
        Result:=True;
      end
    else Result:=False;
  finally
    dlg.Free;
  end;
end;

function GetCheckMarkPropertiesDlg(aMI: TMenuItem;
  const aGroupIndexArr: TByteDynArray; aShadowMenu: TShadowMenu; out
  aChecked: boolean; out anAutoCheck: boolean; out
  anAlwaysShowCheckable: boolean; out aRadioItem: boolean; out aGroupIndex: byte
  ): boolean;
var
  dlg: TCheckMarkDialog;
  mr: TModalResult;
begin
  dlg:=TCheckMarkDialog.CreateWithMenuItem(nil, aMI, aGroupIndexArr, aShadowMenu);
  try
    dlg.PopupMode := pmAuto;
    mr:=dlg.ShowModal;
    if mr = mrOK then
      begin
        Result:=True;
        aChecked:=dlg.Checked;
        anAutoCheck:=dlg.AutoCheck;
        anAlwaysShowCheckable:=dlg.ShowAlwaysCheckable;
        aRadioItem:=dlg.RadioItem;
        aGroupIndex:=dlg.GroupIndex;
      end
    else begin
      aChecked:=False;
      anAutoCheck:=False;
      anAlwaysShowCheckable:=False;
      Result:=False;
    end;
  finally
    dlg.Free;
  end;
end;

function DlgChooseIconFromImageList(anImageList: TCustomImageList): integer;
var
  dlg: TdlgChooseIcon;
  mr: TModalResult;
begin
  if (anImageList = nil) then
    Exit(-1);
  if (anImageList.Count = 0) then
    Exit(-1)
  else if (anImageList.Count = 1) then
         Exit(0);
  dlg:=TdlgChooseIcon.Create(nil);
  try
    dlg.SetRadioIconGroup(anImageList);
    mr:=dlg.ShowModal;
    if (mr = mrOK) then
      Result:=dlg.ImageIndex
    else Result:= -1;
  finally
    dlg.Free;
  end;
end;

procedure SaveMenuTemplateDlg(aMenuItem: TMenuItem);
var
  dlg: TMenuTemplateDialog;
begin
  dlg:=TMenuTemplateDialog.CreateWithMode(nil, dmSave);
  try
    dlg.MenuToSave:=aMenuItem;
    dlg.ShowModal;
  finally
    dlg.Free;
  end;
end;

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

function NewShortcutOrCaptionIsValidDlg(aConflictingInfo: TSCInfo; out
  aNewShortcut: TShortCut; out aNewCaption: string): boolean;
var
  dlg: TEditShortcutCaptionDialog;
  ok: boolean;
  sc: TShortCut;
begin
  dlg:=TEditShortcutCaptionDialog.CreateNew(nil, aConflictingInfo);
  try
    Result:=(dlg.ShowModal = mrOK);
    case (aConflictingInfo.Kind in Accelerator_Kinds) of
      True: begin
              if HasAccelerator(dlg.NewCaption, sc) then
                ok:=(sc <> aConflictingInfo.Shortcut)
              else ok:=True;
            end;
      False: ok:=(aConflictingInfo.Shortcut <> dlg.NewShortcut);
    end;
    Result:=Result and ok;
    if Result then
      begin
        aNewShortcut:=dlg.NewShortcut;
        aNewCaption:=dlg.NewCaption;
      end
    else
      begin
        aNewShortcut:=0;
        aNewCaption:='';
      end;
  finally
    FreeAndNil(dlg);
  end;
end;

function InsertMenuTemplateDlg: TMenuItem;
var
  dlg: TMenuTemplateDialog;
begin
  dlg:=TMenuTemplateDialog.CreateWithMode(nil, dmInsert);
  try
    if (dlg.ShowModal = mrOK) then
      Result:=dlg.MenuToInsert
    else Result:=nil;
  finally
    dlg.Free;
  end;
end;

function DeleteMenuTemplateDlg: boolean;
var
  dlg: TMenuTemplateDialog;
  mr: TModalResult;
begin
  dlg:=TMenuTemplateDialog.CreateWithMode(nil, dmDelete);
  try
    mr:=dlg.ShowModal;
    Result:=(mr = mrOK);
  finally
    dlg.Free;
  end;
end;

{ TShortcutDisplayDlg }

function SortOnComponentPropertyName(List: TStringList; Index1, Index2: Integer
  ): Integer;
var
  s1, s2: string;
begin
  s1:=TSCInfo(List.Objects[Index1]).ToCompositeString;
  s2:=TSCInfo(List.Objects[Index2]).ToCompositeString;
  Result:=AnsiCompareText(s1, s2);
end;

{ TMenuTemplates }

function SortByComponent(Item1, Item2: Pointer): Integer;
var
  SCInfo1: TSCInfo absolute Item1;
  SCInfo2: TSCInfo absolute Item2;
begin
  if (SCInfo1.ComponentName > SCInfo2.ComponentName) then
    Result:= +1
  else if (SCInfo1.ComponentName < SCInfo2.ComponentName) then
    Result:= -1
  else Result:=0;
end;

{ TEditShortcutCaptionDialog }

function SortByShortcut(Item1, Item2: Pointer): Integer;
var
  inf1: TSCInfo absolute Item1;
  inf2: TSCInfo absolute Item2;
begin
  if (inf1.Shortcut > inf2.Shortcut) then
    Result:= +1
  else if (inf1.Shortcut < inf2.Shortcut) then
    Result:= -1
  else Result:=0;
end;

function SortFPListByComponentPropertyName(Item1, Item2: Pointer): Integer;
var
  inf1: TSCInfo absolute Item1;
  inf2: TSCInfo absolute Item2;
begin
  if (inf1.ComponentName > inf2.ComponentName) then
    Result:= +1
  else if (inf1.ComponentName < inf2.ComponentName) then
    Result:= -1
  else Result:=0;
end;

function SortByComponentPropertyName(List: TStringList; Index1, Index2: Integer): Integer;
var
  name1: string;
  name2: string;
begin
  name1:=TSCInfo(List.Objects[Index1]).ComponentName;
  name2:=TSCInfo(List.Objects[Index2]).ComponentName;
  if (name1 > name2) then
    Result:= +1
  else if (name2 > name1) then
    Result:= -1
  else Result:=0;
end;

{ TScrollPanel }

procedure TScrollPanel.HScrollBarScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
var
  sb: TScrollBar absolute Sender;
  delta, tmp: integer;
  widthDiff: integer;
begin
  if (FChildControl = nil) or (FChildControl.Width <= FWidthDim) then
    Exit;
  widthDiff:=FChildControl.Width - FWidthDim;
  case ScrollCode of
    scLineUp:    if (ScrollPos > 0) and (FChildControl.Left < 0) then begin
                   if (ScrollPos > widthDiff) then
                     ScrollPos:=widthDiff-1;
                   delta:=trunc(FChildControl.Left/ScrollPos);
                   FChildControl.Left:=FChildControl.Left - sb.SmallChange*delta;
                 end;
    scLineDown:  if (ScrollPos <= FHMax) then begin
                   delta:=trunc(ScrollPos*(FChildControl.Width-FWidthDim)/FHMax);
                   FChildControl.Left:= -sb.SmallChange*delta;
                 end
                 else if (FChildControl.Left <> -widthDiff) then
                   FChildControl.Left:= -widthDiff;
    scPageUp:    if (ScrollPos > 0) and (FChildControl.Left < 0) then begin
                   if (ScrollPos > widthDiff) then
                     ScrollPos:=widthDiff-1;
                   delta:= -trunc(sb.LargeChange*FChildControl.Left/ScrollPos);
                   tmp:=FChildControl.Left + delta;
                   if (tmp > 0) then
                     tmp:=0;
                   FChildControl.Left:=tmp;
                 end;
    scPageDown:  if (ScrollPos > 0) and (FChildControl.Left <= 0) then begin
                   delta:=trunc((FChildControl.Left+widthDiff)/ScrollPos);
                   if (delta = 0) then
                     delta:=1;
                   tmp:=FChildControl.Left - delta;
                   if (tmp < -widthDiff) then
                     tmp:=-widthDiff;
                   FChildControl.Left:=tmp;
                 end;
    scPosition:  if (ScrollPos = 0) and (FChildControl.Left <> 0) then
                   FChildControl.Left:=0
                 else if (ScrollPos >= FHMax) and (FChildControl.Left <> -widthDiff) then
                   FChildControl.Left:= -widthDiff;
    scTrack:     FChildControl.Left:=-trunc(ScrollPos*(FChildControl.Width-FWidthDim)/FHMax);
  end;
end;

procedure TScrollPanel.VScrollBarScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
var
  sb: TScrollBar absolute Sender;
  delta, tmp: integer;
  heightDiff: integer;
begin
  if (FChildControl = nil) or (FChildControl.Height <= FHeightDim) then
    Exit;
  heightDiff:=FChildControl.Height - FHeightDim;
  case ScrollCode of
    scLineUp:    if (ScrollPos > 0) and (FChildControl.Top < 0) then begin
                   if (ScrollPos > heightDiff) then
                     ScrollPos:=heightDiff-1;
                   delta:=trunc(FChildControl.Top/ScrollPos);
                   FChildControl.Top:=FChildControl.Top - sb.SmallChange*delta;
                 end;
    scLineDown:  if (ScrollPos <= FVMax) then begin
                   delta:=trunc(ScrollPos*(FChildControl.Height-FHeightDim)/FVMax);
                   FChildControl.Top:= -sb.SmallChange*delta;
                 end
                 else if (FChildControl.Top <> -heightDiff) then
                   FChildControl.Top:= -heightDiff;
    scPageUp:    if (ScrollPos > 0) and (FChildControl.Top < 0) then begin
                   if (ScrollPos > heightDiff) then
                     ScrollPos:=heightDiff-1;
                   delta:= -trunc(sb.LargeChange*FChildControl.Top/ScrollPos);
                   tmp:=FChildControl.Top + delta;
                   if (tmp > 0) then
                     tmp:=0;
                   FChildControl.Top:=tmp;
                 end;
    scPageDown:  if (ScrollPos > 0) and (FChildControl.Top <= 0) then begin
                   delta:=trunc((FChildControl.Top + heightDiff)/ScrollPos);
                   if (delta = 0) then
                     delta:=1;
                   tmp:=FChildControl.Top - delta;
                   if (tmp < -heightDiff) then
                     tmp:=-heightDiff;
                   FChildControl.Top:=tmp;
                 end;
    scPosition:  if (ScrollPos = 0) and (FChildControl.Top <> 0) then
                   FChildControl.Top:=0
                 else if (ScrollPos >= FVMax) and (FChildControl.Top <> -heightDiff) then
                   FChildControl.Top:= -heightDiff;
    scTrack:     FChildControl.Top:=-trunc(ScrollPos*(FChildControl.Height-FHeightDim)/FVMax);
  end;
end;

procedure TScrollPanel.OnChildControlResize(Sender: TObject);
begin
  if (Sender = FChildControl) then begin
    FHeightDim:=Height;
    if FHSBar.Visible then
      Dec(FHeightDim, FHSBar.Height);
    FWidthDim:=Width;
    if FVSBar.Visible then
      Dec(FWidthDim, FVSBar.Width);

    if (FChildControl.Width > FWidthDim) then begin
      FHSBar.Visible:=True;
      FHeightDim:=Height - FHSBar.Height;
      if (FChildControl.Height > FHeightDim) then begin
        FVSBar.Visible:=True;
        FWidthDim:=Width - FVSBar.Width;
      end;
    end
    else if FHSBar.Visible then
      FHSBar.Visible:=False;

    if not FVSBar.Visible and (FChildControl.Height > FHeightDim) then begin
      FVSBar.Visible:=True;
      FWidthDim:=Width - FVSBar.Width;
    end;

    if FVSBar.Visible and (FChildControl.Height < FHeightDim) then
      FVSBar.Visible:=False;
  end;
end;

procedure TScrollPanel.SetLargeChange(AValue: integer);
begin
  if (FHSBar.LargeChange <> AValue) then begin
    FHSBar.LargeChange:=AValue;
    FVSBar.LargeChange:=AValue;
  end;
end;

procedure TScrollPanel.SetParent(NewParent: TWinControl);
begin
  inherited SetParent(NewParent);
  if (NewParent <> nil) then begin
    FChildControl.Parent:=Self;
  end;
end;

constructor TScrollPanel.CreateWithChild(TheOwner: TComponent;
  aChild: TWinControl);
begin
  inherited Create(TheOwner);
  Caption:='';
  BevelOuter:=bvNone;
  BevelInner:=bvNone;
  FHSBar:=TScrollBar.Create(Self);
  with FHSBar do begin
    PageSize:=30;
    Max:=100;
    Align:=alBottom;
    OnScroll:=@HScrollBarScroll;
    Visible:=False;
    Parent:=Self;
  end;
  FVSBar:=TScrollBar.Create(Self);
  with FVSBar do begin
    Kind:=sbVertical;
    PageSize:=30;
    Max:=100;
    Align:=alRight;
    OnScroll:=@VScrollBarScroll;
    Visible:=False;
    Parent:=Self;
  end;
  SetLargeChange(10);
  FHMax:=FHSBar.Max - FHSBar.PageSize;
  FVMax:=FVSBar.Max - FVSBar.PageSize;
  FChildControl:=aChild;
  FChildControl.OnResize:=@OnChildControlResize;
end;

{ TEditShortcutCaptionDialog }

procedure TEditShortcutCaptionDialog.CaptionEditChange(Sender: TObject);
var
  newSC: TShortCut;
  hasAccel: boolean;
  ed: TEdit absolute Sender;
  inf: TSCInfo;
begin
  if not (Sender is TEdit) then
    Exit;
  if HasAccelerator(ed.Text, newSC) then
    begin
      if MenuDesigner.ShortcutList.UniqueListContainsShortcut(newSC) then
        begin
          inf:=MenuDesigner.ShortcutList.FindUniqueInfoForShortcut(newSC);
          IDEMessageDialogAb(lisMenuEditorFurtherShortcutConflict,
                     Format(lisMenuEditorSIsAlreadyInUse,
                     [ShortCutToText(newSC), inf.Component.Name, LineEnding]),
                     mtWarning, [mbOK], False);
          FEdit.Text:=AmpersandStripped(FOldCaption);
          FEdit.SetFocus;
        end
      else
        begin
          FNewShortcut:=newSC;
          FNewCaption:=ed.Text;
        end;
    end
  else
    begin
      FNewShortcut:=0;
      FNewCaption:=ed.Text;
    end;
  hasAccel:=HasAccelerator(FEdit.Text, newSC);
  FButtonPanel.OKButton.Enabled:=not hasAccel or (hasAccel and (newSC <> FInfo.Shortcut));
end;

procedure TEditShortcutCaptionDialog.GrabBoxEnter(Sender: TObject);
begin
  if not FButtonPanel.OKButton.Enabled then
    FButtonPanel.OKButton.Enabled:=True;
end;

procedure TEditShortcutCaptionDialog.GrabBoxExit(Sender: TObject);
var
  newSC: TShortCut;
  inf: TSCInfo;
begin
  newSC:=KeyToShortCut(FGrabBox.Key, FGrabBox.ShiftState);
  if (FInfo.Shortcut = newSC) then
    begin
      IDEMessageDialogAb(lisMenuEditorShortcutNotYetChanged,
           Format(lisMenuEditorYouHaveToChangeTheShortcutFromSStoAvoidAConflict,
                  [ShortCutToText(FInfo.Shortcut), LineEnding]),
                  mtWarning, [mbOK], False);
      FGrabBox.KeyComboBox.SetFocus;
      Exit;
    end;
  if MenuDesigner.ShortcutList.UniqueListContainsShortcut(newSC) then
    begin
      inf:=MenuDesigner.ShortcutList.FindUniqueInfoForShortcut(newSC);
      IDEMessageDialogAb(lisMenuEditorFurtherShortcutConflict,
           Format(lisMenuEditorSIsAlreadyInUse,
                  [ShortCutToText(newSC), inf.Component.Name, LineEnding]),
                  mtWarning, [mbOK], False);
      FGrabBox.KeyComboBox.SetFocus;
    end
  else
    begin
      FNewShortcut:=newSC;
      FButtonPanel.OKButton.Enabled:=True;
    end;
end;

procedure TEditShortcutCaptionDialog.OKButtonOnClick(Sender: TObject);
begin
  if FEditingCaption then
    begin
      if (FEdit.Text = '') then
        begin
          IDEMessageDialogAb(lisMenuEditorCaptionCannotBeBlank,
                     lisMenuEditorYouMustEnterTextForTheCaption,
                     mtWarning, [mbOK], False);
          FEdit.Text:=AmpersandStripped(FOldCaption);
          FEdit.SetFocus;
        end
      else ModalResult:=mrOK;
    end
  else
    ModalResult:=mrOK;
end;

procedure TEditShortcutCaptionDialog.Activate;
begin
  inherited Activate;
  FButtonPanel.OKButton.Enabled:=False;
end;

constructor TEditShortcutCaptionDialog.CreateNew(anOwner: TComponent;
  aSCInfo: TSCInfo);
var
  s: string;
  sse: TShiftStateEnum;
  i: integer;
begin
  Assert(aSCInfo<>nil,'TEditShortcutCaptionDialog.CreateNew: aSCInfo is nil');
  Assert(aSCInfo.Kind<>scUnknown,'TEditShortcutCaptionDialog.CreateNew: aSCInfo is unknown type');
  Assert(MenuDesigner.ShortcutList.UniqueCount>0,'TEditShortcutCaptionDialog.CreateNew: unique list is empty');
  inherited CreateNew(anOwner);
  FInfo:=aSCInfo;
  FEditingCaption:=(FInfo.Kind in Accelerator_Kinds);
  Position:=poScreenCenter;
  BorderStyle:=bsDialog;
  Constraints.MinWidth:=300;

  FGroupBox:=TGroupBox.Create(Self);
  if FEditingCaption then
    begin
      Caption:=Format(lisMenuEditorChangeConflictingAcceleratorS,
                      [ShortCutToText(FInfo.Shortcut)]);
      if (FInfo.Kind = scMenuItemAccel) then
        FOldCaption:=FInfo.MenuItem.Caption;
      FEdit:=TEdit.Create(Self);
      with FEdit do
        begin
          Align:=alClient;
          BorderSpacing.Around:=Margin;
          AutoSize:=True;
          Text:=FOldCaption;
          OnChange:=@CaptionEditChange;
          Parent:=FGroupBox;
        end;
      s:=lisMenuEditorCaption;
    end
  else
    begin
      Caption:=Format(lisMenuEditorChangeShortcutConflictS,
                      [ShortCutToText(FInfo.Shortcut)]);
      s:=KindToPropertyName(FInfo.Kind);
      // don't set values to old shortcut since they need to be changed anyhow
      FGrabBox:=TCustomShortCutGrabBox.Create(Self);
      with FGrabBox do
        begin
          Align:=alClient;
          BorderSpacing.Around:=Margin;
          AutoSize:=True;
          GrabButton.Caption:=lisMenuEditorGrabKey;
         // this rather restricted list covers most of the common values needed
          with KeyComboBox.Items do
            begin
              Clear;
              BeginUpdate;
              for i:=Low(ShortCutKeys) to High(ShortCutKeys) do
                Add(ShortCutToText(ShortCutKeys[i]));
              EndUpdate;
            end;
          GrabButton.OnEnter:=@GrabBoxEnter; // we can't alter any grabBox OnClick event
          KeyComboBox.OnEnter:=@GrabBoxEnter;
          for sse in ShiftButtons do
            ShiftCheckBox[sse].OnEnter:=@GrabBoxEnter;
          OnExit:=@GrabBoxExit;
          FGrabBox.Caption:=Format(lisMenuEditorChangeShortcutCaptionForComponent,
                           [s, FInfo.Component.Name]);
          Parent:=FGroupBox;
        end;
    end;
  FGroupBox.Caption:=Format(lisMenuEditorEditingSForS,[s, FInfo.Component.Name]);
  FGroupBox.Align:=alTop;
  FGroupBox.BorderSpacing.Around:=Margin;
  FGroupBox.AutoSize:=True;
  FGroupBox.Parent:=Self;

  FButtonPanel:=TButtonPanel.Create(Self);
  with FButtonPanel do
    begin
      ShowButtons:=[pbOK, pbCancel];
      Top:=1;
      Align:=alTop;
      OKButton.OnClick:=@OKButtonOnClick;
      OKButton.ModalResult:=mrNone;
      OKButton.Enabled:=False;
      ShowBevel:=False;
      Parent:=Self;
    end;
  AutoSize:=True;
end;

{ TResolveConflictsDlg }

procedure TResolveConflictsDlg.ConflictsBoxSelectionChange(Sender: TObject;
  User: boolean);
begin
  if (FConflictsListBox.ItemIndex < 0) then
    Exit;
  FSelectedDuplicate:=TSCInfo(FConflictsListBox.Items.Objects[FConflictsListBox.ItemIndex]);
  Assert(FSelectedDuplicate<>nil,'TResolveConflictsDlg.ConflictsBoxSelectionChange: FSelectedDuplicate is nil');
  FSelectedUnique:=MenuDesigner.ShortcutList.FindUniqueInfoForShortcut(FSelectedDuplicate.Shortcut);
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
  for p in MenuDesigner.ShortcutList.InitialDuplicates do begin
    sDup:=Format(lisMenuEditorSInS, [ShortCutToText(infDup.Shortcut),
      infDup.ComponentName]);
    infUnique:=MenuDesigner.ShortcutList.FindUniqueInfoForShortcut(infDup.Shortcut);
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
        MenuDesigner.ShadowMenu.FEditorDesigner.PropertyEditorHook.RefreshPropertyValues;
        MenuDesigner.ShadowMenu.FEditorDesigner.Modified;
        si:=MenuDesigner.ShadowMenu.GetShadowForMenuItem(FSelectedInfo.MenuItem);
        if (si <> nil) then begin
          MenuDesigner.ShadowMenu.UpdateBoxLocationsAndSizes;
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
  if (MenuDesigner.ShortcutList.InitialDuplicatesCount > 0) then begin
    FResolvedConflictsCount:=0;
    FResolvedConflictsCountLabel.Caption:=Format(
      lisMenuEditorResolvedConflictsD, [FResolvedConflictsCount]);
    CreateListboxItems;
    FRemainingConflictsCountLabel.Caption:=Format(
      lisMenuEditorRemainingConflictsD, [FConflictsListBox.Count]);
    FConflictsListBox.ItemIndex:=0;
  end
  else begin
    FButtonPanel.OKButton.Enabled:=False;
    FSelectedInfo:=nil;
    FConflictsListBox.OnSelectionChange:=nil;
    FConflictsListBox.Items.Add(lisMenuEditorNoShortcutConflictsFound);
    FCurrentEdit.Text:=lisMenuEditorNoShortcutConflictsToResolve;
    FResolvedConflictsCountLabel.Caption:=lisMenuEditorResolvedConflicts0;
    FRemainingConflictsCountLabel.Caption:=lisMenuEditorRemainingConflicts0;
  end;
end;

procedure TResolveConflictsDlg.RePopulateListBox;
begin
  FConflictsListBox.OnSelectionChange:=nil;
  FConflictsListBox.Items.Clear;
  FConflictsListBox.ItemIndex:= -1;
  FButtonPanel.OKButton.Enabled:=False;
  MenuDesigner.ShortcutList.ScanContainerForShortcutsAndAccelerators;
  if (MenuDesigner.ShortcutList.InitialDuplicatesCount > 0) then begin
    CreateListboxItems;
    UpdateStatistics;
    FConflictsListBox.ItemIndex:=0;
    FConflictsListBox.OnSelectionChange:=@ConflictsBoxSelectionChange;
  end
  else begin
    FButtonPanel.OKButton.Enabled:=False;
    FSelectedInfo:=nil;
    FConflictsListBox.OnSelectionChange:=nil;
    FRemainingConflictsCountLabel.Caption:=lisMenuEditorRemainingConflicts0;
    FResolvedConflictsCountLabel.Caption:=Format(
      lisMenuEditorResolvedConflictsD, [FInitialConflictsCount]);
    FConflictsListBox.Items.Add(lisMenuEditorNoShortcutConflictsRemain);
    FCurrentEdit.Text:=lisMenuEditorConflictResolutionComplete;
    FButtonPanel.CancelButton.Caption:=lisMenuEditorClose;
  end;
end;

procedure TResolveConflictsDlg.UpdateStatistics;
begin
  FResolvedConflictsCount:=FInitialConflictsCount - FConflictsListBox.Count;
  FResolvedConflictsCountLabel.Caption:=Format(lisMenuEditorResolvedConflictsD,
    [FResolvedConflictsCount]);
  FRemainingConflictsCountLabel.Caption:=Format(
    lisMenuEditorRemainingConflictsD, [FConflictsListBox.Count]);
end;

constructor TResolveConflictsDlg.Create(TheOwner: TComponent);
begin
  inherited CreateNew(TheOwner);
  MenuDesigner.ShortcutList.ScanContainerForShortcutsAndAccelerators;
  FInitialConflictsCount:=MenuDesigner.ShortcutList.InitialDuplicatesCount;
  FResolvedConflictsCount:=0;
  Position:=poScreenCenter;
  Constraints.MinWidth:=400;
  Constraints.MinHeight:=256;
  Caption:=lisMenuEditorMenuItemShortcutConflictsIn +
           (GlobalDesignHook.LookupRoot as TComponent).Name;

  FConflictsGroupBox:=TGroupBox.Create(Self);
  with FConflictsGroupBox do begin
    Caption:=Format(lisMenuEditorShortcutConflictsFoundInitiallyD, [
      MenuDesigner.ShortcutList.InitialDuplicatesCount]);
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

{ TMenuTemplates }

function TMenuTemplates.GetDescription(index: integer): string;
begin
  CheckIndex(index);
  Result:=TMenuTemplate(FTemplateList[index]).Description;
end;

function TMenuTemplates.GetMenu(index: integer): TMenuItem;
var
  mt: TMenuTemplate;
  submi, mi: TMenuItem;
  i: integer;

  procedure AddSubItem(anIndex: integer);
  begin
    submi:=TMenuItem.Create(nil);
    submi.Caption:=mt.SubItem[anIndex];
    submi.ShortCut:=mt.Shortcut[anIndex];
    mi.Insert(anIndex, submi);
  end;

begin
  CheckIndex(index);
  mt:=TMenuTemplate(FTemplateList[index]);
  mi:=TMenuItem.Create(nil);
  mi.Caption:=mt.PrimaryItem;
  for i:=0 to mt.SubItemCount-1 do
    AddSubItem(i);
  Result:=mi;
end;

function TMenuTemplates.GetMenuCount: integer;
begin
  Result:=FTemplateList.Count;
end;

function TMenuTemplates.GetMenuTemplate(index: integer): TMenuTemplate;
begin
  CheckIndex(index);
  Result:=TMenuTemplate(FTemplateList[index]);
end;

function TMenuTemplates.GetPrimaryItem(index: integer): string;
begin
  CheckIndex(index);
  Result:=TMenuTemplate(FTemplateList[index]).PrimaryItem;
end;

procedure TMenuTemplates.CheckIndex(anIndex: integer);
begin
  Assert((anIndex > -1) and (anIndex < FTemplateList.Count),
       Format('TMenuTemplates.CheckIndex: index (%d) out of bounds[0-%d]',
              [anIndex, Pred(FTemplateList.Count)]));
end;

procedure TMenuTemplates.LoadDefaultTemplates;
begin
  AddTemplate(lisMenuEditorBasicEditMenuTemplate);
  AddTemplate(lisMenuEditorBasicFileMenuTemplate);
  AddTemplate(lisMenuEditorBasicWindowMenuTemplate);
  AddTemplate(lisMenuEditorBasicHelpMenuTemplate);
end;

procedure TMenuTemplates.LoadSavedTemplates;
var
  XMLConfig: TXMLConfig;
  i, j: integer;
  cfgPath, s, sc, sText, tmp: string;
begin
  cfgPath:=SetDirSeparators(ExtractFilePath(ChompPathDelim(SysToUTF8(GetAppConfigDir(False)))) + 'lazarus');
  XMLConfig:=TXMLConfig.Create(cfgPath + DirectorySeparator + MenuTemplatesFilename);
  try
    i:=1;
    s:=XMLConfig.GetValue(Format('menu_%d/Name/Value',[i]), 'missing Name');
    while (s <> 'missing Name') do
    begin
      tmp:=XMLConfig.GetValue(Format('menu_%d/Description/Value',[i]),
                                'missing Description');
      sText:=Format('%s,%s,',[s, tmp]);
      s:=XMLConfig.GetValue(Format('menu_%d/SubItems/Value',[i]), '');
      if (s = 'true') then
        begin
          j:=0;
          s:=XMLConfig.GetValue(Format('menu_%d/subitem_%d/Name/Value',[i,j]), 'nonexistent subitem');
          sc:=XMLConfig.GetValue(Format('menu_%d/subitem_%d/Shortcut/Value',[i,j]), 'nonexistent shortcut');
          while (s <> 'nonexistent subitem') do
            begin
              if (CompareText(sc, 'nonexistent shortcut') = 0) then
                sc := '';
              AppendStr(sText, s + ',' + sc + ',');
              Inc(j);
              s:=XMLConfig.GetValue(Format('menu_%d/subitem_%d/Name/Value',[i,j]), 'nonexistent subitem');
              sc:=XMLConfig.GetValue(Format('menu_%d/subitem_%d/Shortcut/Value',[i,j]), 'nonexistent shortcut');
            end;
        end;
      Inc(i);
      s:=XMLConfig.GetValue(Format('menu_%d/Name/Value',[i]), 'missing Name');
      AddTemplate(sText, False);
    end;
  finally
    XMLConfig.Free;
  end;
end;

constructor TMenuTemplates.CreateForMode(aDialogMode: TDialogMode);
begin
  inherited Create;
  FTemplateList:=TFPList.Create;
  if (aDialogMode = dmInsert) then
    LoadDefaultTemplates;
  LoadSavedTemplates;
end;

destructor TMenuTemplates.Destroy;
var
  p: pointer;
  mt: TMenuTemplate absolute p;
begin
  for p in FTemplateList do
    mt.Free;
  FreeAndNil(FTemplateList);
  inherited Destroy;
end;

function TMenuTemplates.GetIndexOfTemplate(aMT: TMenuTemplate): integer;
begin
  if (aMT = nil) then
    Result:= -1
  else Result:=FTemplateList.IndexOf(aMT);
end;

procedure TMenuTemplates.AddTemplate(const aTemplateText: string;
  isStandard: boolean);
var
  mt: TMenuTemplate;
begin
  if (aTemplateText = '') then
    Exit;
  mt:=TMenuTemplate.CreateFromString(aTemplateText);
  mt.IsStandardTemplate:=isStandard;
  FTemplateList.Add(mt);
end;

procedure TMenuTemplates.SaveTemplateToConfig(aMenuTemplate: TMenuTemplate);
var
  XMLConfig: TXMLConfig;
  i, j: integer;
  cfgPath, s: string;
begin
  cfgPath:=SetDirSeparators(ExtractFilePath(ChompPathDelim(SysToUTF8(GetAppConfigDir(False)))) + 'lazarus');
  XMLConfig:=TXMLConfig.Create(cfgPath + DirectorySeparator + MenuTemplatesFilename);
  try
    i:=1;
    s:=XMLConfig.GetValue(Format('menu_%d/Name/Value',[i]), 'missing');
    while not SameText(s, 'missing') do
      begin
        Inc(i);
        s:=XMLConfig.GetValue(Format('menu_%d/Name/Value',[i]), 'missing');
      end;
    XMLConfig.SetValue(Format('menu_%d/Name/Value',[i]), aMenuTemplate.PrimaryItem);
    XMLConfig.SetValue(format('menu_%d/Description/Value',[i]), aMenuTemplate.Description);
    if (aMenuTemplate.SubItemCount > 0) then
      begin
        XMLConfig.SetValue(Format('menu_%d/SubItems/Value', [i]), 'true');
      end;
    for j:=0 to aMenuTemplate.SubItemCount-1 do
      begin
        XMLConfig.SetValue(Format('menu_%d/subitem_%d/Name/Value',[i,j]), aMenuTemplate.SubItem[j]);
        XMLConfig.SetValue(Format('menu_%d/subitem_%d/Shortcut/Value',[i,j]), ShortCutToText(aMenuTemplate.Shortcut[j]));
      end;
    InvalidateFileStateCache;
    XMLConfig.Flush;
  finally
    XMLConfig.Free;
  end;
end;

{ TMenuTemplate }

function TMenuTemplate.GetShortcut(index: integer): TShortCut;
begin
  Result:=TShortCut(PtrUInt(FSubList.Objects[index]));
end;

function TMenuTemplate.GetSubItem(index: integer): string;
begin
  Result:=FSubList[index];
end;

function TMenuTemplate.GetSubItemCount: integer;
begin
  Result:=FSubList.Count;
end;

class function TMenuTemplate.MenuItemToString(aMenuItem: TMenuItem;
  aDescription: string): string;
var
    sc: TShortCut;
    scStr: string;
    i: integer;
    mi: TMenuItem;
  begin
    if (aMenuItem = nil) then
      Result:=''
    else begin
      Result:=Format('%s,%s,',[aMenuItem.Caption, aDescription]);
      for i:=0 to aMenuItem.Count-1 do begin
        mi:=aMenuItem.Items[i];
        sc:=mi.ShortCut;
        if (sc = 0) then
          begin
            if (mi.ShortCutKey2 = 0) then
              scStr:=''
            else scStr:=ShortCutToText(mi.ShortCutKey2);
          end
        else scStr:=ShortCutToText(sc);
        AppendStr(Result, Format('%s,%s,',[mi.Caption, scStr]));
      end;
    end;
  end;

constructor TMenuTemplate.CreateFromString(const aMenuString: string);
begin
  FSubList:=TStringList.Create;
  if not ReadFromString(aMenuString) then
    Assert(False,'TMenuTemplate.CreateFromString: Attempt to read invalid menu template');
end;

destructor TMenuTemplate.Destroy;
begin
  FSubList.Free;
  inherited Destroy;
end;

function TMenuTemplate.ReadFromString(const aString: string): boolean;
var
  sl: TStringList;
  s: string;
  i: integer;
begin
  Result:=True;
  sl:=TStringList.Create;
  try
    sl.StrictDelimiter:=True;
    sl.CommaText:=aString;
    case sl.Count of
      0: Result:=False;
      1: FPrimaryItem:=sl[0];
      2: begin
           FPrimaryItem:=sl[0];
           FDescription:=sl[1];
         end;
      else begin
             FPrimaryItem:=sl[0];
             FDescription:=sl[1];
             for i:=2 to sl.Count-1 do
               begin
                 if not Odd(i) then
                   s:=sl[i]
                 else FSubList.AddObject(s, TObject(PtrUInt(TextToShortCut(sl[i]))));
               end;
           end;
    end; // case
  finally
    sl.Free;
  end;
end;

{ TMenuTemplateDialog }

procedure TMenuTemplateDialog.BExecuteDeleteClick(Sender: TObject);
begin
  DeleteSelectedFromConfigFile;
end;

procedure TMenuTemplateDialog.BExecuteInsertClick(Sender: TObject);
var
  mt: TMenuTemplate;
begin
  Assert(FTVTemplates.Selected<>nil,'TMenuTemplateDialog.BExecuteInsertClick: FTVTemplates.Selected is nil');
  mt:=TMenuTemplate(FTVTemplates.Selected.Data);
  FMenuToInsert:=FTemplates.Menu[FTemplates.GetIndexOfTemplate(mt)];
end;

procedure TMenuTemplateDialog.BExecuteSaveClick(Sender: TObject);
var
  trimmed: string;
begin
  trimmed:=Trim(FEDescription.Text);
  if (Length(trimmed) < 4) then
    begin
      IDEMessageDialogAb(lisMenuEditorInadequateDescription,
              Format(lisMenuEditorSIsNotASufficientDescriptionPleaseExpand,[trimmed]),
              mtWarning, [mbOK], False);
      FEDescription.SelectAll;
      FEDescription.SetFocus;
    end
  else
    begin
      FNewMenuTemplate.Description:=trimmed;
      FTemplates.AddTemplate(TMenuTemplate.MenuItemToString(FMenuToSave, trimmed), False);
      FEDescription.ReadOnly:=True;
      SaveMenuAsTemplate;
    end;
end;

procedure TMenuTemplateDialog.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TMenuTemplateDialog.CBDisplayChange(Sender: TObject);
begin
  FPreview.DisplayAsPopup:=FCBDisplay.Checked;
end;

procedure TMenuTemplateDialog.DeleteSelectedFromConfigFile;
var
  highestOldIdx, i, idxToDelete: integer;
  XMLConfig: TXMLConfig;
  cfgPath, s, desc, currDesc: string;

  procedure ExchangeConfigTemplates(aLower, aHigher: integer);
  var
    j: integer;
    valueN, valueS: string;
  begin
    j:=1;
    while not SameText(XMLConfig.GetValue(Format('menu_%d/subitem_%d/Name/Value',
                       [aLower, j]), 'nonexistent'), 'nonexistent') do
      begin
        XMLConfig.DeletePath(Format('menu_%d/subitem_%d',[aLower, j]));
        Inc(j);
      end;

    j:=1;
    valueN:=XMLConfig.GetValue(Format('menu_%d/subitem_%d/Name/Value',
                              [aHigher, j]), 'nonexistent');
    while not SameText(valueN, 'nonexistent') do
      begin
        XMLConfig.SetValue(Format('menu_%d/subitem_%d/Name/Value',[aLower, j]), valueN);
        valueS:=XMLConfig.GetValue(Format('menu_%d/subitem_%d/Shortcut/Value',
                                  [aHigher, j]), 'nonexistent');
        if not SameText(valueS, 'nonexistent') then
          XMLConfig.SetValue(Format('menu_%d/subitem_%d/Shortcut/Value',[aLower, j]), valueS);
        Inc(j);
        valueN:=XMLConfig.GetValue(Format('menu_%d/subitem_%d/Name/Value',
                              [aHigher, j]), 'nonexistent');
      end;

    valueN:=XMLConfig.GetValue(Format('menu_%d/Name/Value',[aHigher]), 'nonexistent');
    if not SameText(valueN, 'nonexistent') then
      XMLConfig.SetValue(Format('menu_%d/Name/Value',[aLower]), valueN);

    valueN:=XMLConfig.GetValue(Format('menu_%d/Description/Value',[aHigher]), 'nonexistent');
    if not SameText(valueN, 'nonexistent') then
      XMLConfig.SetValue(Format('menu_%d/Description/Value',[aLower]), valueN);
  end;

begin
  desc:=TMenuTemplate(FTVTemplates.Selected.Data).Description;
  cfgPath:=SetDirSeparators(ExtractFilePath(ChompPathDelim(SysToUTF8(GetAppConfigDir(False)))) + 'lazarus');
  XMLConfig:=TXMLConfig.Create(cfgPath + DirectorySeparator + MenuTemplatesFilename);
  try
    i:=0;
    idxToDelete:= -1;
    repeat
      Inc(i);
      s:=XMLConfig.GetValue(Format('menu_%d/Name/Value',[i]), 'nonexistent');
      currDesc:=XMLConfig.GetValue(Format('menu_%d/Description/Value',[i]), 'noDescription');
      if (CompareText(desc, currDesc) = 0) then
        idxToDelete:=i;
    until CompareText(s, 'nonexistent') = 0;
    highestOldIdx:=Pred(i);
    Assert(idxToDelete>-1,'TMenuTemplateDialog.DeleteSelectedFromConfigFile: nonexistent template index');
    if (idxToDelete < highestOldIdx) then
      ExchangeConfigTemplates(idxToDelete, highestOldIdx);
    XMLConfig.DeletePath(Format('menu_%d',[highestOldIdx]));
    InvalidateFileStateCache;
    XMLConfig.Flush;
  finally
    XMLConfig.Free;
  end;
end;

procedure TMenuTemplateDialog.DeleteSelectedTemplate;
begin
  Assert(FTVTemplates.Selected<>nil,'TMenuTemplateDialog.DeleteSelectedTemplate: FTVTemplates.Selected is nil');
  DeleteSelectedFromConfigFile;
end;

procedure TMenuTemplateDialog.EDescriptionChange(Sender: TObject);
begin
  if (Length(FEDescription.Text) > 3) and not FBExecute.Enabled then
    FBExecute.Enabled:=True;
end;

procedure TMenuTemplateDialog.PopulateTreeView;
var
  mt: TMenuTemplate;
  i: integer;
  processed: string;

begin
  for i:=0 to FTemplates.MenuCount-1 do
    begin
      mt:=FTemplates.MenuTemplate[i];
      processed:=AmpersandStripped(mt.PrimaryItem);
      if mt.IsStandardTemplate then
        FTVTemplates.Items.AddChildObject(FStandardNode, processed, mt)
      else begin
        AppendStr(processed, Format(' [%s]',[mt.Description]));
        FTVTemplates.Items.AddChildObject(FSavedNode, processed, mt);
      end;
    end;
end;

procedure TMenuTemplateDialog.SaveMenuAsTemplate;
var
  s: string;
begin
  Assert(FMenuToSave<>nil,'TInsertTemplateDialog.SaveMenuAsTemplate: FMenuToSave is nil');
  Assert(FNewMenuTemplate<>nil,'TInsertTemplateDialog.SaveMenuAsTemplate: FNewMenuTemplate is nil');
  FTemplates.SaveTemplateToConfig(FNewMenuTemplate);
  s:=AmpersandStripped(FNewMenuTemplate.PrimaryItem);
  IDEMessageDialogAb(lisMenuEditorTemplateSaved, Format(
    lisMenuEditorANewMenuTemplateHasBeenSaved,
                     [FNewMenuTemplate.Description, s, FNewMenuTemplate.SubItemCount]),
                     mtInformation, [mbOK], False);
end;

procedure TMenuTemplateDialog.SetupGUI;
begin
  FPButtons:=TCustomPanel.Create(Self);
  with FPButtons do begin
    ControlStyle:=ControlStyle - [csSetCaption];
    BevelInner:=bvNone;
    BevelOuter:=bvNone;
    BorderSpacing.Around:=Margin;
    Align:=alBottom;
    AutoSize:=True;
    Parent:=Self;
  end;

  FBCancel:=TBitBtn.Create(Self);
  with FBCancel do begin
    Kind:=bkCancel;
    AutoSize:=True;
    BorderSpacing.Left:=2*Spacing;
    Align:=alRight;
    Left:=1;
    OnClick:=@CancelButtonClick;
    Parent:=FPButtons;
  end;

  FBExecute:=TBitBtn.Create(Self);
  with FBExecute do begin
    Kind:=bkOK;
    case FDialogMode of
      dmSave: begin
                Caption:=lisMenuEditorSaveMenuShownAsANewTemplate;
                OnClick:=@BExecuteSaveClick;
              end;
      dmInsert: begin
                  Caption:=lisMenuEditorInsertSelectedMenuTemplate;
                  OnClick:=@BExecuteInsertClick;
                end;
      dmDelete: begin
                  Caption:=lisMenuEditorDeleteSelectedMenuTemplate;
                  OnClick:=@BExecuteDeleteClick;
                end;
    end;
    AutoSize:=True;
    Align:=alRight;
    Left:=0;
    Enabled:=False;
    Parent:=FPButtons;
  end;

  FGChoose:=TGroupBox.Create(Self);
  with FGChoose do begin
    Align:=alClient;
    BorderSpacing.Around:=Margin;
    Parent:=Self;
  end;

  FPDescription:=TPanel.Create(Self);
  with FPDescription do begin
    Align:=alTop;
    BevelInner:=bvNone;
    BevelOuter:=bvNone;
    BorderSpacing.Bottom:=Margin;
    BorderSpacing.Top:=Margin;
    AutoSize:=True;
    Parent:=FGChoose;
  end;

  FEDescription:=TEdit.Create(Self);
  with FEDescription do begin
    Align:=alClient;
    BorderSpacing.Right:=Margin;
    Parent:=FPDescription;
  end;

  FLDescription:=TLabel.Create(Self);
  with FLDescription do begin
    AutoSize:=True;
    BorderSpacing.Around:=Margin;
    Align:=alLeft;
    Parent:=FPDescription;
  end;

  FTVTemplates:=TTreeView.Create(Self);
  with FTVTemplates do begin
    Align:=alLeft;
    Width:=200;
    Align:=alLeft;
    Indent:=Margin;
    Options:=[tvoAutoExpand, tvoAutoItemHeight, tvoKeepCollapsedNodes,
              tvoShowRoot, tvoNoDoubleClickExpand];
    BorderSpacing.Bottom:=Margin;
    BorderSpacing.Left:=Margin;
    OnAdvancedCustomDrawItem:=@TVAdvancedCustomDrawItem;
    OnEditing:=@TVEditing;
    Parent:=FGChoose;
  end;

  FSplitter:=TSplitter.Create(Self);
  FSplitter.Left:=FTVTemplates.BoundsRect.Right;
  FSplitter.Parent:=FGChoose;

  FPRight:=TCustomPanel.Create(Self);
  with FPRight do begin
    Width:=200;
    BevelInner:=bvNone;
    BevelOuter:=bvNone;
    BorderSpacing.Bottom:=Margin;
    BorderSpacing.Right:=Margin;
    Align:=alClient;
    Parent:=FGChoose;
  end;

  FCBDisplay:=TCheckBox.Create(Self);
  with FCBDisplay do begin
    Align:=alTop;
    Alignment:=taLeftJustify;
    BorderSpacing.Bottom:=Margin;
    BorderSpacing.Left:=Margin;
    AutoSize:=True;
    Caption:=lisMenuEditorDisplayPreviewAsPopupMenu;
    OnChange:=@CBDisplayChange;
    Parent:=FPRight;
  end;

  FScrollBoxPreview:=TScrollBox.Create(Self);
  with FScrollBoxPreview do begin
    Align:=alClient;
    TabStop:=False;
    Parent:=FPRight;
  end;
  FPreview:=TPreview.Create(Self);
  FPreview.Parent:=FScrollBoxPreview;

  case FDialogMode of
    dmSave: begin
              FSavedNode:=FTVTemplates.Items.AddFirst(nil,
                                                   lisMenuEditorExistingSavedTemplates);
              FGChoose.Caption:=lisMenuEditorSaveMenuAsTemplateForFutureUse;
              FLDescription.Caption:=lisMenuEditorEnterAMenuDescription;
              FLDescription.FocusControl:=FEDescription;
            end;
    dmInsert: begin
                FStandardNode:=FTVTemplates.Items.AddFirst(nil,
                                                         lisMenuEditorStandardTemplates);
                FSavedNode:=FTVTemplates.Items.Add(FStandardNode,
                                                   lisMenuEditorSavedTemplates);
                FGChoose.Caption:=lisMenuEditorChooseTemplateToInsert;
                FLDescription.Caption:=lisMenuEditorTemplateDescription;
                FTVTemplates.OnSelectionChanged:=@TVSelectionChanged;
                FEDescription.ReadOnly:=True;
              end;
    dmDelete: begin
                FStandardNode:=nil;
                FSavedNode:=FTVTemplates.Items.AddFirst(nil,
                                                    lisMenuEditorExistingSavedTemplates);
                FGChoose.Caption:=lisMenuEditorChooseTemplateToDelete;
                FLDescription.Caption:=lisMenuEditorTemplateDescription;
                FTVTemplates.OnSelectionChanged:=@TVSelectionChanged;
                FEDescription.ReadOnly:=True;
              end;
  end;
end;

procedure TMenuTemplateDialog.ShowPreview(aMenuTemplate: TMenuTemplate);
begin
  FPreview.Clear;
  if (aMenuTemplate <> nil) then
    FPreview.LoadTemplate(aMenuTemplate);
end;

procedure TMenuTemplateDialog.TVAdvancedCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
  var PaintImages, DefaultDraw: Boolean);
var
  nodeR, textR: TRect;
begin
  DefaultDraw:=(Stage <> cdPrePaint);
  if DefaultDraw then Exit;

  nodeR:=Node.DisplayRect(False);
  textR:=Node.DisplayRect(True);
  if (Node.Level = 0) then begin
    Sender.Canvas.Font.Color:=clGray;
    Sender.Canvas.Font.Style:=[fsBold];
  end
  else begin
    Sender.Canvas.Font.Color:=clBlack;
    Sender.Canvas.Font.Style:=[];
  end;
  if (cdsSelected in State) and (Node.Level > 0) then
    Sender.Canvas.Brush.Color:=Sender.SelectionColor
  else Sender.Canvas.Brush.Color:=Sender.Color;
  Sender.Canvas.FillRect(nodeR);
  Sender.Canvas.TextOut(textR.Left, textR.Top, Node.Text);
end;

procedure TMenuTemplateDialog.TVEditing(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
begin
  AllowEdit:=False;
end;

procedure TMenuTemplateDialog.TVSelectionChanged(Sender: TObject);
var
  tv: TTreeView absolute Sender;
  node: TTreeNode;
  mt: TMenuTemplate;
begin
  if not (Sender is TTreeView) then
    Exit;

  node:=tv.Selected;
  if (node = nil) or (node = FStandardNode) or (node = FSavedNode) or
     (node = FNoneSavedNode) or (node.Data = nil) then
  begin
    FBExecute.Enabled:=False;
    FEDescription.Text:='';
    ShowPreview(nil);
  end
  else begin
    Assert(node.Data<>nil,'TMenuTemplateDialog.TVSelectionChanged: node.Data is nil');
    mt:=TMenuTemplate(node.Data);
    FEDescription.Text:=mt.Description;
    ShowPreview(mt);
    if FDialogMode in [dmInsert, dmDelete] then
      FBExecute.Enabled:=True;
  end;
end;

procedure TMenuTemplateDialog.DoShowWindow;
var
  menuString: string;
  noneSaved: boolean;
begin
  inherited DoShowWindow;
  noneSaved:=not SavedTemplatesExist;
  case FDialogMode of
    dmSave: begin
              menuString:=TMenuTemplate.MenuItemToString(FMenuToSave, '');
              FNewMenuTemplate:=TMenuTemplate.CreateFromString(menuString);
              FPreview.LoadTemplate(FNewMenuTemplate);
              if noneSaved then
                FNoneSavedNode:=FTVTemplates.Items.AddChild(FSavedNode,lisMenuEditorNone);
              FEDescription.SetFocus;
              FEDescription.OnChange:=@EDescriptionChange;
            end;
    dmInsert: begin
                if noneSaved then
                  FNoneSavedNode:=FTVTemplates.Items.AddChild(FSavedNode,lisMenuEditorNone);
                TVSelectionChanged(FTVTemplates);
                FTVTemplates.Selected:=FTVTemplates.Items[1];
                FTVTemplates.SetFocus;
              end;
    dmDelete: begin
                if noneSaved then begin
                  IDEMessageDialogAb(lisMenuEditorNoUserSavedTemplates,
                         Format('%s%s%s%s',[lisMenuEditorThereAreNoUserSavedMenuTemplates,
                               LineEnding, LineEnding,
                               lisMenuEditorOnlyStandardDefaultTemplatesAreAvailable]),
                               mtInformation, [mbOK], False);
                  ModalResult:=mrCancel;
                end
                else begin
                  TVSelectionChanged(FTVTemplates);
                  FTVTemplates.Selected:=FTVTemplates.Items[1];
                  FTVTemplates.SetFocus;
                end;
              end;
  end;
end;

constructor TMenuTemplateDialog.CreateWithMode(AOwner: TComponent;
  aDialogMode: TDialogMode);
begin
  inherited CreateNew(AOwner);
  FDialogMode:=aDialogMode;
  BorderStyle:=bsDialog;
  SetInitialBounds(0, 0, 530, 380);
  Position:=poScreenCenter;
  case aDialogMode of
    dmSave: Caption:=lisMenuEditorSaveMenuAsTemplate;
    dmInsert: Caption:=lisMenuEditorInsertMenuTemplateInto;
    dmDelete: Caption:=lisMenuEditorDeleteSavedMenuTemplate;
  end;
  FTemplates:=TMenuTemplates.CreateForMode(FDialogMode);
  SetupGUI;
  PopulateTreeView;
end;

destructor TMenuTemplateDialog.Destroy;
begin
  FreeAndNil(FNewMenuTemplate);
  FreeAndNil(FTemplates);
  FreeAndNil(FPreview);
  inherited Destroy;
end;

{ TPreview }

function TPreview.GetSize: TSize;
var
    w, h: integer;
    i, tmp: integer;
    s: string;
begin
  if (FTemplate = nil) then
    begin
      FillChar(Result{%H-}, SizeOf(Result), 0);
      SetBounds(0,0,0,0);
    end
  else case FDisplayAsPopup of
    True: begin
            w:=5;
            h:=10;
            for i:=0 to FTemplate.SubItemCount-1 do
              begin
                s:=FTemplate.SubItem[i];
                if (s = '-') then
                  Inc(h, Separator_Height)
                 else
                  begin
                    Inc(h, DropDown_Height);
                    tmp:=Canvas.TextWidth(s);
                    if (FTemplate.Shortcut[i] <> 0) then
                      Inc(tmp, Canvas.TextWidth(ShortCutToText(FTemplate.Shortcut[i])) + Double_MenuBar_Text_Offset);
                    if (tmp > w) then
                      w:=tmp;
                  end;
              end;
            Result.cx:=w + 2*Double_DropDown_Text_Offset + Canvas.TextWidth(FTemplate.PrimaryItem);
            Result.cy:=h + 2;
          end;
    False: begin
             w:=0;
             h:=MenuBar_Height;
             for i:=0 to FTemplate.SubItemCount-1 do
               begin
                 s:=FTemplate.SubItem[i];
                 if (s = '-') then
                   Inc(h, Separator_Height)
                  else
                   begin
                     Inc(h, DropDown_Height);
                     tmp:=Canvas.TextWidth(s);
                     if (FTemplate.Shortcut[i] <> 0) then
                       Inc(tmp, Canvas.TextWidth(ShortCutToText(FTemplate.Shortcut[i])) + Double_MenuBar_Text_Offset);
                     if (tmp > w) then
                       w:=tmp;
                   end;
               end;
             Result.cx:=w + Double_DropDown_Text_Offset;
             Result.cy:=h + 2;
           end;
  end;
end;

procedure TPreview.SetDisplayAsPopup(AValue: boolean);
var
  sz: TSize;
begin
  if FDisplayAsPopup=AValue then Exit;
  FDisplayAsPopup:=AValue;
  SetBounds(0,0,0,0);
  sz:=GetSize;
  SetBounds(0, 0, sz.cx, sz.cy);
end;

procedure TPreview.Paint;
var
  r, rBar, rDrop: TRect;
  dets: TThemedElementDetails;
  textFlags: integer = DT_VCENTER or DT_SINGLELINE or DT_EXPANDTABS;
  i, t, h, w, l: integer;
  txt: string;
  separator: boolean;
  szB: TSize;
begin
  r:=ClientRect;
  Canvas.FillRect(r);
  Canvas.Frame(r);
  InflateRect(r, -1, -1);
  rBar:=r;
  if FDisplayAsPopup then
    begin
      rBar.Top:=rBar.Top+5;
      rBar.Left:=rBar.Left+5;
      rBar.Bottom:=rBar.Top + DropDown_Height;
      rBar.Right:=rBar.Left + DropDown_Text_Offset - Gutter_Offset;
      dets:=ThemeServices.GetElementDetails(tmPopupGutter);
      ThemeServices.DrawElement(Canvas.Handle, dets, rBar);
      w:=Canvas.TextWidth(FTemplate.PrimaryItem);
      rBar.Right:=rBar.Left + w + Double_DropDown_Text_Offset;
      rBar.Left:=rBar.Left + DropDown_Text_Offset;
      dets:=ThemeServices.GetElementDetails(tmPopupItemNormal);
      ThemeServices.DrawText(Canvas, dets, FTemplate.PrimaryItem, rBar, textFlags, 0);
      rBar.Left:=r.Left+5;
      Canvas.Pen.Color:=clLtGray;
      Canvas.Frame(rBar);
      if (FTemplate.SubItemCount > 0) then
        begin
          rDrop:=rBar;
          dets:=ThemeServices.GetElementDetails(tmPopupSubmenuNormal);
          rDrop.Left:=rDrop.Right - DropDown_Text_Offset;
          ThemeServices.DrawElement(Canvas.Handle, dets, rDrop);
        end;
      rDrop:=rBar;
      szB:=Size(rBar);
      OffsetRect(rDrop, szB.cx + 1, 2);
      rDrop.Right:=r.Right-1;
      rDrop.Bottom:=r.Bottom-1;
      Canvas.Frame(rDrop);
      l:=rDrop.Left+1;
      w:=r.Right-2;
      t:=rDrop.Top+1;
      for i:=0 to Pred(FTemplate.SubItemCount) do
        begin
          txt:=FTemplate.SubItem[i];
          separator:=(txt = '-');
          if separator then
            h:=Separator_Height
          else h:=DropDown_Height;
          rDrop:=Rect(l, t, w, t+h);
          Inc(t, h);
          rBar:=rDrop;
          rBar.Right:=rBar.Left + DropDown_Text_Offset - Gutter_Offset;
          dets:=ThemeServices.GetElementDetails(tmPopupGutter);
          ThemeServices.DrawElement(Canvas.Handle, dets, rBar);
          if separator then
            begin
              dets:=ThemeServices.GetElementDetails(tmPopupSeparator);
              ThemeServices.DrawElement(Canvas.Handle, dets, rDrop);
            end
          else
            begin
              rDrop.Left:=rDrop.Left + DropDown_Text_Offset;
              dets:=ThemeServices.GetElementDetails(tmPopupItemNormal);
              ThemeServices.DrawText(Canvas, dets, txt, rDrop, textFlags, 0);
              if (FTemplate.Shortcut[i] <> 0) then
                begin
                  txt:=ShortCutToText(FTemplate.Shortcut[i]);
                  rDrop.Left:=r.Right - Canvas.TextWidth(txt) - Double_MenuBar_Text_Offset;
                  ThemeServices.DrawText(Canvas, dets, txt, rDrop, textFlags, 0);
                end;
            end;
        end;
     end
  else
    begin
      rBar.Bottom:=rBar.Top + MenuBar_Height;
      dets:=ThemeServices.GetElementDetails(tmBarBackgroundActive);
      ThemeServices.DrawElement(Canvas.Handle, dets, rBar);
      rBar.Left:=rBar.Left + MenuBar_Text_Offset;
      ThemeServices.DrawText(Canvas, dets, FTemplate.PrimaryItem, rBar, textFlags, 0);
      t:=MenuBar_Height + 1;
      for i:=0 to Pred(FTemplate.SubItemCount) do
        begin
          rBar:=r;
          rBar.Top:=t;
          txt:=FTemplate.SubItem[i];
          separator:=(txt = '-');
          if separator then
            h:=Separator_Height
          else h:=DropDown_Height;
          rBar.Bottom:=rBar.Top + h;
          rBar.Right:=DropDown_Text_Offset - Gutter_Offset;
          dets:=ThemeServices.GetElementDetails(tmPopupGutter);
          ThemeServices.DrawElement(Canvas.Handle, dets, rBar);
          rBar.Left:=rBar.Left + DropDown_Text_Offset;
          rBar.Right:=r.Right;
          if separator then
            begin
              dets:=ThemeServices.GetElementDetails(tmPopupSeparator);
              ThemeServices.DrawElement(Canvas.Handle, dets, rBar);
            end
          else
            begin
              dets:=ThemeServices.GetElementDetails(tmPopupItemNormal);
              ThemeServices.DrawText(Canvas, dets, txt, rBar, textFlags, 0);
              if (FTemplate.Shortcut[i] <> 0) then
                begin
                  txt:=ShortCutToText(FTemplate.Shortcut[i]);
                  rBar.Left:=r.Right - Canvas.TextWidth(txt) - Double_MenuBar_Text_Offset;
                  ThemeServices.DrawText(Canvas, dets, txt, rBar, textFlags, 0);
                end;
            end;
          inc(t, h);
        end;
    end;
end;

procedure TPreview.SetParent(NewParent: TWinControl);
var
  sz: TSize;
begin
  inherited SetParent(NewParent);
  if (NewParent <> nil) then
    begin
      sz:=GetSize;
      SetBounds(0, 0, sz.cx, sz.cy);
      Canvas.Pen.Color:=clLtGray;
      Canvas.Brush.Color:=clBtnFace;
    end;
end;

procedure TPreview.Clear;
var
  sz: TSize;
begin
  FTemplate:=nil;
  sz:=GetSize;
  SetBounds(0, 0, sz.cx, sz.cy);
end;

procedure TPreview.LoadTemplate(aMenuTemplate: tmenuTemplate);
var
  sz: TSize;
begin
  FTemplate:=aMenuTemplate;
  sz:=GetSize;
  SetBounds(0, 0, sz.cx, sz.cy);
end;

{ TRadioIcon }

function TRadioIcon.GetChecked: Boolean;
begin
  Result:=FRIState in [risDown, risPressed, risCheckedHot];
end;

procedure TRadioIcon.SetChecked(aValue: Boolean);
begin
  case aValue of
    True: if (FRIState <> risDown) then begin // set to True
            FRIState:=risDown;
            Repaint;
          end;
    False: if (FRIState <> risUp) then begin // set to False
             FRIState:=risUp;
             Repaint;
           end;
  end;
end;

procedure TRadioIcon.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TRadioIcon.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and (FRIState in [risUncheckedHot, risUp]) then begin
    FRIState:=risPressed;
    Repaint;
    DoChange;
  end;
end;

procedure TRadioIcon.MouseEnter;
begin
  inherited MouseEnter;
  case FRIState of
    risUp: FRIState:=risUncheckedHot;
    risDown: FRIState:=risCheckedHot;
  end;
  Repaint;
end;

procedure TRadioIcon.MouseLeave;
begin
  case FRIState of
    risPressed, risCheckedHot: FRIState:=risDown;
    risUncheckedHot:           FRIState:=risUp;
  end;
  Repaint;
  inherited MouseLeave;
end;

procedure TRadioIcon.Paint;
var
  ted: TThemedElementDetails;
begin
  if (Canvas.Brush.Color <> Color) then
    Canvas.Brush.Color:=Color;
  Canvas.FillRect(ClientRect);
  case FRIState of
    risUp:           ted:=FRIGroup.FedUnchecked;
    risDown:         ted:=FRIGroup.FedChecked;
    risPressed:      ted:=FRIGroup.FedPressed;
    risUncheckedHot: ted:=FRIGroup.FedUncheckedHot;
    risCheckedHot:   ted:=FRIGroup.FedCheckedHot;
  end;
  ThemeServices.DrawElement(Canvas.Handle, ted, FRIGroup.FRadioRect);
  FBGlyph.Draw(Canvas, ClientRect, FRIGroup.FGlyphPt, bsUp, False, 0);

  inherited Paint;
end;

constructor TRadioIcon.CreateWithGlyph(aRIGroup: TRadioIconGroup;
  anImgIndex: integer);
begin
  Assert(anImgIndex > -1,'TRadioIcon.CreateWithGlyph: param not > -1');
  inherited Create(aRIGroup);
  FRIGroup:=aRIGroup;

  FBGlyph:=TButtonGlyph.Create;
  FBGlyph.IsDesigning:=False;
  FBGlyph.ShowMode:=gsmAlways;
  FBGlyph.OnChange:=nil;
  FBGlyph.CacheSetImageList(FRIGroup.FImageList);
  FBGlyph.CacheSetImageIndex(0, anImgIndex);
  Tag:=anImgIndex;

  SetInitialBounds(0, 0, FRIGroup.FRadioWidth, FRIGroup.FRadioHeight);
  ControlStyle:=ControlStyle + [csCaptureMouse]-[csSetCaption, csClickEvents, csOpaque];
  FRIState:=risUp;
  Color:=clBtnFace;
end;

destructor TRadioIcon.Destroy;
begin
  FreeAndNil(FBGlyph);
  inherited Destroy;
end;

{ TRadioIconGroup }

procedure TRadioIconGroup.CreateRadioItems;
var
  i: integer;
begin
  SetLength(FRIArray, FImageList.Count);
  for i:=Low(FRIArray) to High(FRIArray) do
    begin
      FRIArray[i]:=TRadioIcon.CreateWithGlyph(Self, i);
      FRIArray[i].OnChange:=@RIOnChange;
    end;
end;

procedure TRadioIconGroup.ApplyLayout;
const
  BPanelVertDim = 46;
var
  areaToFill, unitArea, hBorderAndMargins, hSpace, vSpace, sepn, vSepn, oldCols,
    count, cols, rows, lastRowCount, space, i, h, v, num, denom, gap,
    hInc, vInc, maxIdx, vBorderAndMargins: integer;
  lft: integer = Margin;
  tp: integer = Margin;
  r: integer = 1;
  c: integer = 1;

  procedure CalcSepn;
  begin
    rows:=count div cols;
    if (cols*rows < count) or (rows < 2) then
      Inc(rows);
    lastRowCount:=count mod cols;
    if (lastRowCount = 0) then
      lastRowCount:=cols;
    num:=space + hSpace*FRIArray[0].Height - lastRowCount*unitArea;
    denom:=Pred(rows)*hSpace + FRIArray[0].Height*Pred(cols)*Pred(rows);
    Assert(denom > 0,'TRadioIconGroup.ApplyLayout: divisor is zero');
    sepn:=trunc(num/denom);
    repeat
      Dec(sepn);
      h:=cols*FRIArray[0].Width + Pred(cols)*sepn;
    until (h < hSpace) or (sepn <= Margin);
  end;

begin
  hBorderAndMargins:=integer(BorderSpacing.Left)+integer(BorderSpacing.Right)+integer(BorderSpacing.Around*2) + Double_Margin;
  hSpace:=Parent.ClientWidth - hBorderAndMargins;
  vBorderAndMargins:=integer(BorderSpacing.Top)+integer(BorderSpacing.Bottom)+integer(BorderSpacing.Around*2) + Double_Margin;
  vSpace:=Parent.ClientHeight - vBorderAndMargins - BPanelVertDim;
  areaToFill:=hSpace*vSpace;
  unitArea:=FRIArray[0].Width*FRIArray[0].Height;
  count:=Length(FRIArray);
  space:=areaToFill - count*unitArea;

  cols:=trunc(sqrt(count)); // assume area is roughly square
  if (cols = 0) then
    Inc(cols);
  oldCols:=cols;
  CalcSepn;

  gap:=hSpace - h;
  if (gap > 0) and (gap > FRIArray[0].Width) then
    begin
      Inc(cols);
      CalcSepn;
    end;
  if (sepn <= Margin) then
    begin
      cols:=oldcols;
      CalcSepn;
    end;

  vSepn:=sepn;
  v:=rows*FRIArray[0].Height + Pred(rows)*vSepn;
  if (v > vSpace) then
  repeat
    Dec(vSepn);
    v:=rows*FRIArray[0].Height + Pred(rows)*vSepn;
  until (v < vSpace) or (vSepn <= Margin);

  hInc:=FRIArray[0].Width + sepn;
  vInc:=FRIArray[0].Height + vSepn;
  maxIdx:=High(FRIArray);
  for i:=Low(FRIArray) to maxIdx do
    begin
      FRIArray[i].Left:=lft;
      FRIArray[i].Top:=tp;
      Inc(c);
      Inc(lft, hInc);
      if (c > cols) and (i < maxIdx) then
        begin
          c:=1;
          lft:=Margin;
          Inc(r);
          Inc(tp, vInc);
        end;
    end;
  Assert(r <= rows,'TRadioIconGroup.ApplyLayout: error in calculation of space needed');
end;

procedure TRadioIconGroup.RIOnChange(Sender: TObject);
var
  aRi: TRadioIcon;
  i: integer;
begin
  if not (Sender is TRadioIcon) then
    Exit;
  aRi:=TRadioIcon(Sender);
  FItemIndex:=aRi.Tag;
  DoSelectItem;
  if aRi.Checked then
    begin
     for i:=Low(FRIArray) to High(FRIArray) do
       if (i <> FItemIndex) then
         FRIArray[i].Checked:=False;
    end;
end;

procedure TRadioIconGroup.DoSelectItem;
begin
  if Assigned(FOnSelectItem) then
    FOnSelectItem(Self);
end;

procedure TRadioIconGroup.SetParent(NewParent: TWinControl);
var
  i: Integer;
begin
  inherited SetParent(NewParent);
  if (NewParent <> nil) then
    begin
      ApplyLayout;
      for i:=Low(FRIArray) to High(FRIArray) do
        FRIArray[i].SetParent(Self);
    end;
end;

constructor TRadioIconGroup.CreateWithImageList(AOwner: TComponent;
  anImgList: TCustomImageList);
var
  topOffset: integer;
begin
  Assert(AOwner<>nil,'TRadioIconGroup.CreateWithImageList: AOwner is nil');
  Assert(anImgList<>nil,'TRadioIconGroup.CreateWithImageList:anImgList is nil');

  inherited Create(AOwner);
  FImageList:=anImgList;
  FedUnChecked:=ThemeServices.GetElementDetails(tbRadioButtonUncheckedNormal);
  FedChecked:=ThemeServices.GetElementDetails(tbRadioButtonCheckedNormal);
  FedPressed:=ThemeServices.GetElementDetails(tbRadioButtonCheckedPressed);
  FedUncheckedHot:=ThemeServices.GetElementDetails(tbRadioButtonUncheckedHot);
  FedCheckedHot:=ThemeServices.GetElementDetails(tbRadioButtonCheckedHot);
  FedSize:=ThemeServices.GetDetailSize(FedUnChecked);
  FRadioHeight:=FedSize.cy;
  if (anImgList.Height > FRadioHeight) then
    FRadioHeight:=anImgList.Height;
  topOffset:=(FRadioHeight - FedSize.cy) div 2;
  FRadioRect:=Rect(0, topOffset, FedSize.cx, topOffset+FedSize.cy);
  FSpacing:=5;
  FRadioWidth:=FedSize.cx + FSpacing + anImgList.Width;
  FGlyphPt:=Point(FedSize.cx+FSpacing, 0);
  FItemIndex:= -1;
  CreateRadioItems;
end;

{ TdlgChooseIcon }

function TdlgChooseIcon.GetImageIndex: integer;
begin
  Result:=FRadioIconGroup.ItemIndex;
end;

procedure TdlgChooseIcon.RIGClick(Sender: TObject);
begin
  if not FButtonPanel.OKButton.Enabled then
    FButtonPanel.OKButton.Enabled:=True;
  FButtonPanel.OKButton.SetFocus;
end;

constructor TdlgChooseIcon.Create(TheOwner: TComponent);
begin
  inherited CreateNew(TheOwner);
  Position:=poScreenCenter;
  BorderStyle:=bsDialog;
  Width:=250;
  Height:=250;
  FButtonPanel:=TButtonPanel.Create(Self);
  FButtonPanel.ShowButtons:=[pbOK, pbCancel];
  FButtonPanel.OKButton.Name:='OKButton';
  FButtonPanel.OKButton.DefaultCaption:=True;
  FButtonPanel.OKButton.Enabled:=False;
  FButtonPanel.CancelButton.Name:='CancelButton';
  FButtonPanel.CancelButton.DefaultCaption:=True;
  FButtonPanel.Parent:=Self;
end;

procedure TdlgChooseIcon.SetRadioIconGroup(anImageList: TCustomImageList);
begin
  FRadioIconGroup:=TRadioIconGroup.CreateWithImageList(Self, anImageList);
  with FRadioIconGroup do begin
    Align:=alClient;
    BorderSpacing.Top:=FButtonPanel.BorderSpacing.Around;
    BorderSpacing.Left:=FButtonPanel.BorderSpacing.Around;
    BorderSpacing.Right:=FButtonPanel.BorderSpacing.Around;
    TabOrder:=0;
    OnSelectItem:=@RIGClick;
    Parent:=Self;
  end;
  Caption:=Format(lisMenuEditorPickAnIconFromS, [anImageList.Name]);
end;

{ TCheckMarkDialog }

procedure TCheckMarkDialog.OnIdle(Sender: TObject; var Done: Boolean);
const
  counter: integer = 0;
begin
  Inc(counter);
  if (counter > 250) then begin
    FStatusBar.SimpleText:='';
    SetIdleEvent(False);
  end;
end;

function TCheckMarkDialog.GetAutoCheck: boolean;
begin
  Result:=FCheckMarkGroup.Checked[1];
end;

function TCheckMarkDialog.GetChecked: boolean;
begin
  Result:=FCheckMarkGroup.Checked[0];
end;

function TCheckMarkDialog.GetGroupIndex: byte;
var
  i, len: integer;
  s: string;
  numeric: boolean;
begin
  len:=Length(FRadioGroupCombo.Text);
  if (len = 0) then
    Exit(0);
  i:=1;
  repeat
    if FRadioGroupCombo.Text[i] in ['0'..'9'] then
      begin
        AppendStr(s{%H-}, FRadioGroupCombo.Text[i]);
        numeric:=True;
      end
    else numeric:=False;
    Inc(i);
  until not numeric or (i > len);
  i:=StrToIntDef(s, 0);
  if (i < 0) or (i > High(Byte)-1)then
    Result:=0
  else Result:=i;
end;

function TCheckMarkDialog.GetRadioItem: boolean;
begin
  Result:=FCheckMarkGroup.Checked[3];
end;

function TCheckMarkDialog.GetShowAlwaysCheckable: boolean;
begin
  Result:=FCheckMarkGroup.Checked[2];
end;

procedure TCheckMarkDialog.CheckMarkGroupItemClick(Sender: TObject;
  Index: integer);
begin
  if (Index = 3) then
  begin
    FRadioGroupCombo.Enabled:=FCheckMarkGroup.Checked[3];
    if FCheckMarkGroup.Checked[0] and FCheckMarkGroup.Checked[3] then
      FShadowMenu.GetParentBoxForMenuItem(FMenuItem).SetUnCheckedAllExcept(FMenuItem);
  end;
end;

procedure TCheckMarkDialog.SetIdleEvent(enableIt: boolean);
begin
  case enableIt of
    True:  Application.AddOnIdleHandler(@OnIdle);
    False: Application.RemoveOnIdleHandler(@OnIdle);
  end;
end;

constructor TCheckMarkDialog.CreateWithMenuItem(AOwner: TComponent;
  aMI: TMenuItem; aByteArr: TByteDynArray; aShadowMenu: TShadowMenu);
var
  b: byte;
  sl: TStringList;
  s: string;
begin
  inherited CreateNew(AOwner);
  FMenuItem:=aMI;
  FGroupIndexArray:= aByteArr;
  FShadowMenu:=aShadowMenu;
  Position:=poScreenCenter;
  BorderStyle:=bsDialog;
  Caption:=lisMenuEditorEditCheckmarkProps;

  FButtonPanel:=TButtonPanel.Create(Self);
  with FButtonPanel do begin
    ShowButtons:=[pbOK, pbCancel];
    OKButton.Name:='OKButton';
    OKButton.DefaultCaption:=True;
    CancelButton.Name:='CancelButton';
    CancelButton.DefaultCaption:=True;
    ShowBevel:=False;
    BorderSpacing.Right:=Spacing;
    BorderSpacing.Bottom:=Spacing;
    Parent:=Self;
  end;

  FStatusBar:=TStatusBar.Create(Self);
  with FStatusBar do begin
    SimplePanel:=True;
    SizeGrip:=False;
    Parent:=Self;
  end;

  FGBProperties:=TGroupBox.Create(Self);
  with FGBProperties do begin
    Align:=alClient;
    AutoSize:=True;
    Constraints.MinWidth:=350;
    BorderSpacing.Around:=Margin;
    BorderSpacing.Top:=Margin;
    Caption:=Format(lisMenuEditorCheckMarkAndRadioItemProps,[FMenuItem.Name, FMenuItem.Caption]);
    Parent:=Self;
  end;

  FCheckMarkGroup:=TCheckGroup.Create(Self);
  with FCheckMarkGroup do begin
    Columns:=3;
    Items.CommaText:='&Checked,&AutoCheck,&ShowAlwaysCheckable,&RadioItem';
    Align:=alTop;
    AutoSize:=True;
    OnItemClick:=@CheckMarkGroupItemClick;
    Checked[0]:=FMenuItem.Checked;
    Checked[1]:=FMenuItem.AutoCheck;
    Checked[2]:=FMenuItem.ShowAlwaysCheckable;
    Checked[3]:=FMenuItem.RadioItem;
    Parent:=FGBProperties;
  end;

  FRadioGroupCombo:=TComboBox.Create(Self);
  if FMenuItem.RadioItem then
    FRadioGroupCombo.Text:=IntToStr(FMenuItem.GroupIndex)
  else
    begin
      FRadioGroupCombo.Text:='0';
      FRadioGroupCombo.Enabled:=False;
    end;
  if (Length(FGroupIndexArray) > 0) then
    for b:=0 to Pred(Length(FGroupIndexArray)) do
      if (FGroupIndexArray[b] > 0) then
        begin
          FRadioGroupCombo.Items.Add(Format(lisMenuEditorDDItems, [b,
             FGroupIndexArray[b]]));
          if (FGroupIndexArray[b] = 1) then begin
            FStatusBar.SimpleText:=
  Format(lisMenuEditorNoteGroupIndexDHasOnlyOneRadioItemItIsNotYetAGroup, [b]);
            SetIdleEvent(True);
          end;
        end;
  with FRadioGroupCombo do begin
    BorderSpacing.Around:=Margin;
    AnchorSideLeft.Control:=FGBProperties;
    AnchorSideTop.Control:=FCheckMarkGroup;
    AnchorSideTop.Side:=asrBottom;
    AnchorSideBottom.Control:=FGBProperties;
    AnchorSideBottom.Side:=asrBottom;
    Anchors:=[akTop, akLeft, akBottom];
    Width:=100;
    Parent:=FGBProperties;
  end;

  FLGroupIndex:=TLabel.Create(Self);
  with FLGroupIndex do begin
    Caption:=lisMenuEditorGroupIndexDropdownShowsAnyExistingGroups;
    FocusControl:=FRadioGroupCombo;
    BorderSpacing.Around:=Margin;
    BorderSpacing.Top:=2;
    AnchorSideLeft.Control:=FRadioGroupCombo;
    AnchorSideLeft.Side:=asrBottom;
    AnchorSideTop.Control:=FCheckMarkGroup;
    AnchorSideTop.Side:=asrBottom;
    AnchorSideBottom.Control:=FGBProperties;
    AnchorSideBottom.Side:=asrBottom;
    Anchors:=[akTop, akLeft, akBottom];
    Parent:=FGBProperties;
  end;


  FDisplayGroupIndexes:=TDualDisplay.Create(Self);
  with FDisplayGroupIndexes do
    begin
      Constraints.MinHeight:=163;
      Constraints.MaxHeight:=Screen.Height - 300;
      BorderSpacing.Around:=Margin;
      AnchorSideLeft.Control:=FGBProperties;
      AnchorSideTop.Control:=FRadioGroupCombo;
      AnchorSideTop.Side:=asrBottom;
      AnchorSideBottom.Control:=FGBProperties;
      AnchorSideBottom.Side:=asrBottom;
      AnchorSideRight.Control:=FGBProperties;
      AnchorSideRight.Side:=asrBottom;
      Anchors:=[akTop, akLeft, akBottom, akRight];
      Parent:=FGBProperties;
      AddLine(lisMenuEditorGroupIndexMenuItems, dtBlackBold);
      try
        sl:=FShadowMenu.GetParentBoxForMenuItem(FMenuItem).RadioGroupList;
        if (sl = nil) then
          AddLine(lisMenuEditorNoneNone, dtGreyed)
        else
          for s in sl do
            AddLine(s);
      finally
        sl.Free;
      end;
    end;
  AutoSize:=True;
end;

{ TEditCaptionDialog }

procedure TEditCaptionDialog.EditOnChange(Sender: TObject);
var
  hasAccel: boolean;
  sc: TShortCut;
begin
  if (FEdit.Text = '') then
    begin
      FEdit.Text:=lisMenuEditorCaptionShouldNotBeBlank;
      FEdit.SetFocus;
    end
  else
  begin
    hasAccel:=HasAccelerator(FEdit.Text, sc);
    if (not hasAccel) or (hasAccel and (sc <> FOldShortcut)) then
    begin
      FNewShortcut:=sc;
      FButtonPanel.OKButton.Enabled:=True;
    end;
  end;
end;

procedure TEditCaptionDialog.OKButtonClick(Sender: TObject);
begin
  FMenuItem.Caption:=FEdit.Text;
end;

constructor TEditCaptionDialog.CreateWithMenuItem(AOwner: TComponent;
  aMI: TMenuItem; aSC: TShortCut);
var
  key: word;
  sstate: TShiftState;
  p: integer;
  ch: Char;
begin
  inherited CreateNew(AOwner);
  FMenuItem:=aMI;
  FOldShortcut:=aSC;
  ShortCutToKey(aSC, key, sstate);
  Position:=poScreenCenter;
  BorderStyle:=bsDialog;
  Caption:=Format(lisMenuEditorEditingCaptionOfS, [FMenuItem.Name]);

  FButtonPanel:=TButtonPanel.Create(Self);
  with FButtonPanel do
    begin
      ShowButtons:=[pbOK, pbCancel];
      OKButton.Name:='OKButton';
      OKButton.DefaultCaption:=True;
      OKButton.Enabled:=False;
      OKButton.OnClick:=@OKButtonClick;
      CancelButton.Name:='CancelButton';
      CancelButton.DefaultCaption:=True;
      ShowBevel:=False;
      Parent:=Self;
    end;

  FGBEdit:=TGroupBox.Create(Self);
  with FGBEdit do
    begin
      BorderSpacing.Around:=Margin;
      p:=LazUTF8.UTF8Pos('&', aMI.Caption);
      if (p > 0) and (p < LazUTF8.UTF8Length(aMI.Caption)) then
        ch:=aMI.Caption[Succ(p)] // gets correct case of key
      else ch:=Chr(Ord(key));    // fallback
    Caption:=Format(lisMenuEditorAcceleratorKeySNeedsChanging, [ch]);
      Align:=alClient;
      Parent:=Self;
    end;

  FEdit:=TEdit.Create(Self);
  with FEdit do
    begin;
      BorderSpacing.Around:=Margin;
      Text:=FMenuItem.Caption;
      Align:=alClient;
      OnChange:=@EditOnChange;
      Parent:=FGBEdit;
    end;

  AutoSize:=True;
end;

{ TAddShortcutDialog }

procedure TAddShortcutDialog.OKButtonClick(Sender: TObject);
begin
  if (FShortCutGrabBox.Key <> VK_UNKNOWN) then
    FNewShortcut:=KeyToShortCut(FShortCutGrabBox.Key, FShortCutGrabBox.ShiftState)
  else FNewShortcut:=0;
end;

procedure TAddShortcutDialog.OnGrabBoxCloseUp(Sender: TObject);
begin
  if (FShortCutGrabBox.KeyComboBox.ItemIndex = 0) then
    FShortCutGrabBox.ShiftState:=[];
end;

constructor TAddShortcutDialog.CreateWithMenuItem(AOwner: TComponent;
  aMI: TMenuItem; isMainSC: boolean; aSC: TShortCut);
var
  editing: boolean;
  key: word;
  shift: TShiftState;
  i: integer;
begin
  inherited CreateNew(AOwner);
  FMenuItem:=aMI;
  FOldShortcut:=aSC;
  editing:=(aSC <> 0);
  Position:=poScreenCenter;
  BorderStyle:=bsDialog;
  case editing of
    False: if isMainSC then
             Caption:=Format(lisMenuEditorEnterANewShortCutForS, [FMenuItem.Name])
           else Caption:=Format(lisMenuEditorEnterANewShortCutKey2ForS, [FMenuItem.Name]);
    True : if isMainSC then
             Caption:=Format(lisMenuEditorChangeTheShortCutForS, [FMenuItem.Name])
           else Caption:=Format(lisMenuEditorChangeTheShortCutKey2ForS, [FMenuItem.Name]);
  end;
  FButtonPanel:=TButtonPanel.Create(Self);
  FButtonPanel.ShowButtons:=[pbOK, pbCancel];
  FButtonPanel.OKButton.Name:='OKButton';
  FButtonPanel.OKButton.DefaultCaption:=True;
  FButtonPanel.OKButton.OnClick:=@OKButtonClick;
  FButtonPanel.CancelButton.Name:='CancelButton';
  FButtonPanel.CancelButton.DefaultCaption:=True;
  FButtonPanel.Parent:=Self;
  FShortCutGrabBox:=TShortCutGrabBox.Create(Self);
  FShortCutGrabBox.BorderSpacing.Around:=Margin;
  FShortCutGrabBox.GrabButton.Caption:='&Grab key';
  // this rather restricted list covers most of the common values needed
  // #todo - extend list?
  with FShortCutGrabBox.KeyComboBox.Items do
    begin
      Clear;
      BeginUpdate;
      Add(lisMenuEditorNone);
      for i:=1 to High(ShortCutKeys) do
        Add(ShortCutToText(ShortCutKeys[i]));
      EndUpdate;
    end;
  {$if defined(darwin) or defined(macos) or defined(iphonesim)}
    FShortCutGrabBox.AllowedShifts:=[ssShift, ssCtrl, ssMeta]
  {$else} FShortCutGrabBox.AllowedShifts:=[ssShift, ssCtrl, ssAlt] {$endif};
  FShortCutGrabBox.KeyComboBox.OnCloseUp:=@OnGrabBoxCloseUp;
  FShortCutGrabBox.Align:=alClient;
  FShortCutGrabBox.MainOkButton:=FButtonPanel.OKButton;
  if editing then begin
    ShortCutToKey(FOldShortcut, key, shift);
    FShortCutGrabBox.ShiftState:=shift;
    FShortCutGrabBox.Key:=key;
  end;
  FShortCutGrabBox.Parent:=Self;
  AutoSize:=True;
end;

{ TShortcutDisplayDlg }

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
              else mi.ShortCutKey2:=sc;
              si:=MenuDesigner.ShadowMenu.GetShadowForMenuItem(mi);
              if (si <> nil) then begin
                MenuDesigner.ShadowMenu.UpdateBoxLocationsAndSizes;
                si.Repaint;
              end;
              MenuDesigner.UpdateShortcutList(False);
              UpdateFromMenu;
              GlobalDesignHook.RefreshPropertyValues;
              GlobalDesignHook.Modified(mi);
              MenuDesigner.ShadowMenu.RefreshFakes;
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
        0: MenuDesigner.ShortcutList.ScanList.Sort;
        1: MenuDesigner.ShortcutList.SortByComponentPropertyName;
      end;
      for i:=0 to MenuDesigner.ShortcutList.ScanList.Count-1 do
        begin
          inf:=TSCInfo(MenuDesigner.ShortcutList.ScanList.Objects[i]);
          if (inf.Kind in MenuItem_Kinds) then
            dt:=dtBlack
          else dt:=dtGreyed;
          FDualDisplay.AddLine(MenuDesigner.ShortcutList.ScanList[i] + ',' +
                     inf.Component.Name+ '.' + KindToPropertyName(inf.Kind), dt);
        end;
    end;
    False: begin
      inf:=TSCInfo(MenuDesigner.ShortcutList.ScanList.Objects[index]);
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
              si:=MenuDesigner.ShadowMenu.GetShadowForMenuItem(mi);
              if (si <> nil) then
                begin
                  MenuDesigner.ShadowMenu.UpdateBoxLocationsAndSizes;
                  si.Repaint;
                end;
              MenuDesigner.UpdateShortcutList(True);
              UpdateContents;
              GlobalDesignHook.RefreshPropertyValues;
              GlobalDesignHook.Modified(mi);
              MenuDesigner.ShadowMenu.RefreshFakes;
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
    MenuDesigner.UpdateShortcutList(not FShortcutsOnly);
    if (MenuDesigner.ShortcutList.ScanList.Count = 0) then
      begin
        FDualDisplay.AddLine(lisMenuEditorNoneNone, dtGreyed);
        if FShortcutsOnly then
          FGBDisplay.Caption:=lisMenuEditorShortcuts
        else FGBDisplay.Caption:=lisMenuEditorShortcutsAndAcceleratorKeys;
      end
    else
      begin
        if FShortcutsOnly then
          FGBDisplay.Caption:=Format(
            lisMenuEditorShortcutsAssociatedWithMenusAndActionsD,
            [MenuDesigner.ShortcutList.ShortcutsInContainerCount])
        else FGBDisplay.Caption:=Format(
          lisMenuEditorShortcutsDAndAcceleratorKeysD,
            [MenuDesigner.ShortcutList.ShortcutsInContainerCount,
            MenuDesigner.ShortcutList.AcceleratorsInContainerCount]);

        FDualDisplay.BeginUpdate;
        for i:=0 to MenuDesigner.ShortcutList.ScanList.Count-1 do
          begin
            inf:=TSCInfo(MenuDesigner.ShortcutList.ScanList.Objects[i]);
            kind:=inf.Kind;
            if (kind in MenuItem_Kinds) then
              dt:=dtBlack
            else dt:=dtGreyed;
            FDualDisplay.AddLine(MenuDesigner.ShortcutList.ScanList[i] + ',' +
                         inf.Component.Name + '.' + KindToPropertyName(inf.Kind), dt);
          end;
        FDualDisplay.EndUpdate;
      end;
  end;
end;

procedure TShortcutDisplayDlg.UpdateFromMenu(anIndex: integer);
var
  i: integer;
  inf: TSCInfo;

  procedure AddSCitemToListRecursive(anItem: TMenuItem);
  var
    inf: TSCInfo;
    j: integer;
  begin
    if (anItem.ShortCut <> 0) then begin
      inf:=TSCInfo.CreateWithParams(anItem, scMenuItemSC, anItem.ShortCut);
      FscList.AddObject(ShortCutToText(anItem.ShortCut), TObject(inf));
    end;
    if (anItem.ShortCutKey2 <> 0) and (anItem.ShortCutKey2 <> anItem.ShortCut) then begin
      inf:=TSCInfo.CreateWithParams(anItem, scMenuItemKey2, anItem.ShortCutKey2);
      FscList.AddObject(ShortCutToText(anItem.ShortCutKey2), TObject(inf));
    end;
    for j:=0 to anItem.Count-1 do
      AddSCitemToListRecursive(anItem[j]);
  end;

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
      FGBDisplay.Caption:=Format('Shortcuts used in %s (%d)',
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

constructor TShortcutDisplayDlg.CreateWithShortcutsOnly(shortcutsOnly: boolean;
  aMenu: TMenu);
var
  s: string;
  lurStr: string;
begin
  inherited CreateNew(nil);
  FShortcutsOnly:=shortcutsOnly;
  FMenu:=aMenu;
  FSingleMenuOnly:=(FMenu <> nil);
  FLastSortIndex:= -1;
  if FSingleMenuOnly then
    Caption:=Format(lisMenuEditorShortcutsUsedInS, [FMenu.Name])
  else begin
    if shortcutsOnly then
      s:=lisMenuEditorSShortcuts
    else s:=lisMenuEditorSShortcutsAcceleratorKeys;
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
    //Constraints.MinHeight:=200;
    //Constraints.MaxHeight:=200;
    Parent:=FGBDisplay;
    if FSingleMenuOnly then begin
      OnDisplayClick:=@DisplaySingleMenuClick;
      AddHeader(lisMenuEditorShortcutPropertyWithShortcut);
      Caption:=lisMenuEditorShortcuts;
    end
    else begin
      OnDisplayClick:=@DisplayAllDlgClick;
      if FShortcutsOnly then begin
        AddHeader(lisMenuEditorShortcutPropertyWithShortcut);
        Caption:=Format(lisMenuEditorSShortcutSummary, [lurStr]);
      end
      else begin
        AddHeader(lisMenuEditorShortcutPropertyWithShortcutAccelerator);
        Caption:=Format(lisMenuEditorSShortcutsAndAcceleratorKeys, [lurStr]);
      end;
    end;
  end;
  UpdateContents(FSingleMenuOnly);
  FLabel.Caption:=lisMenuEditorClickANonGreyedItemToEditItsShortcut+ LineEnding
                  + lisMenuEditorOrClickHeaderToSortByThatColumn;
  AutoSize:=True;
end;

destructor TShortcutDisplayDlg.Destroy;
begin
  FreeAndNil(FscList);
  inherited Destroy;
end;

{ TSCList }

function TSCList.GetScanListCompName(index: integer): string;
var
  inf: TSCInfo;
begin
  if (index > -1) and (index < FScanList.Count) then begin
    inf:=TSCInfo(FScanList.Objects[index]);
    if (inf.ComponentName <> '') then
      Result:=inf.ComponentName
    else Result:=lisMenuEditorComponentIsUnnamed;
  end
  else Result:=Format(
    lisMenuEditorTSCListGetScanListCompNameInvalidIndexDForFScanLis, [index]);
end;

function TSCList.GetInitialDuplicatesCount: integer;
begin
  Result:=FInitialDuplicates.Count;
end;

function TSCList.GetUniqueCount: integer;
begin
  Result:=FUniqueList.Count;
end;

procedure TSCList.ClearAllLists;
var
  i: integer;
begin
  for i:=0 to FScanList.Count-1 do
    TSCInfo(FScanList.Objects[i]).Free;
  FScanList.Clear;
  FUniqueList.Clear;
  FInitialDuplicates.Clear;
end;

function TSCList.UniqueListContainsShortcut(aSC: TShortCut): boolean;
var
  p: pointer;
  inf: TSCInfo absolute p;
begin
  for p in FUniqueList do
    if (inf.Shortcut = aSC) then
      Exit(True);
  Result:=False;
end;

function TSCList.FindUniqueInfoForShortcut(aSC: TShortCut): TSCInfo;
var
  p: pointer;
  inf: TSCInfo absolute p;
begin
  for p in FUniqueList do
    if (inf.Shortcut = aSC) then
      Exit(inf);
  Result:=nil;
end;

constructor TSCList.Create;
begin
  FScanList:=TStringList.Create;
  FUniqueList:=TFPList.Create;
  FInitialDuplicates:=TFPList.Create;
  ScanContainerForShortcutsAndAccelerators;
end;

destructor TSCList.Destroy;
begin
  ClearAllLists;
  FreeAndNil(FUniqueList);
  FreeAndNil(FInitialDuplicates);
  FreeAndNil(FScanList);
  inherited Destroy;
end;

procedure TSCList.ScanContainerForShortcutsAndAccelerators;
begin
  DoShortcutAccelScanCount(Self, False);
  ScanSCListForDuplicates;
  if (FInitialDuplicates.Count > 0) then
    FInitialDuplicates.Sort(@SortByShortcut);
  if (FUniqueList.Count > 0) then
    FUniqueList.Sort(@SortByShortcut);
end;

procedure TSCList.ScanContainerForShortcutsOnly;
begin
  DoShortcutAccelScanCount(Self, True);
end;

procedure TSCList.ScanSCListForDuplicates;
var
  i: integer;
  inf2, inf1: TSCInfo;

begin
  FreeAndNil(FUniqueList);
  FreeAndNil(FInitialDuplicates);
  FUniqueList:=TFPList.Create;
  FInitialDuplicates:=TFPList.Create;
  for i:=0 to FScanList.Count-1 do
    if UniqueListContainsShortcut(TSCInfo(FScanList.Objects[i]).Shortcut) then
      FInitialDuplicates.Add(FScanList.Objects[i])
    else FUniqueList.Add(FScanList.Objects[i]);
  if (FInitialDuplicates.Count > 0) then begin
    FInitialDuplicates.Sort(@SortFPListByComponentPropertyName);
    for i:=FInitialDuplicates.Count-1 downto 1 do begin
      inf2:=TSCInfo(FInitialDuplicates[i]);
      inf1:=TSCInfo(FInitialDuplicates[i-1]);
      if (CompareText(inf2.ComponentName, inf1.ComponentName) = 0) and
      (inf2.Shortcut = inf1.Shortcut) then
        FInitialDuplicates.Delete(i);
    end;
  end;
end;

procedure TSCList.SortByComponentPropertyName;
begin
  FScanList.CustomSort(@SortOnComponentPropertyName);
end;

{ TSCInfo }

function TSCInfo.GetAction: TAction;
begin
  if (FComponent is TAction) then
    Result:=TAction(FComponent)
  else Result:=nil;
end;

function TSCInfo.GetCaption: string;
begin
  if (FComponent is TControl) then
    Result:=TControl(FComponent).Caption
  else Result:=lisMenuEditorComponentIsUnexpectedKind;
end;

function TSCInfo.GetMenuItem: TMenuItem;
begin
  if (FComponent is TMenuItem) then
    Result:=TMenuItem(FComponent)
  else Result:=nil;
end;

function TSCInfo.GetToCompositeString: string;
begin
  Result:=FComponent.Name + ShortCutToText(FShortcut);
end;

constructor TSCInfo.CreateWithParams(aComponent: TComponent; aKind: TSCKind;
  aSC: TShortCut);
begin
  FComponent:=aComponent;
  FComponentName:=aComponent.Name;
  FKind:=aKind;
  FShortcut:=aSC;
end;

{ TLineEditor }

function TLineEditor.GetEditedLine: string;
begin
  Result:=FEdit.Text;
end;

procedure TLineEditor.EditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE: begin ModalResult:=mrCancel; Key:=0; end;
    VK_RETURN: begin ModalResult:=mrOK; Key:=0; end;
    else inherited KeyDown(Key, Shift);
  end;
end;

constructor TLineEditor.CreateWithShadowItem(anOwner: TComponent;
  aSI: TShadowItem);
var
  topLeft: TPoint;
begin
  inherited CreateNew(anOwner);
  BorderStyle:=bsNone;
  Name:='LineEditor';
  topLeft:=MenuDesigner.ShadowMenu.ClientToScreen(aSI.BoundsRect.TopLeft);
  SetInitialBounds(topLeft.x + asi.ParentBox.Left,
                   topLeft.y + aSI.ParentBox.Top,
                   aSI.Width+1, aSI.Height+1);
  FEdit:=TEdit.Create(Self);
  FEdit.Align:=alClient;
  FEdit.Text:=aSI.RealItem.Caption;
  FEdit.OnKeyDown:=@EditKeyDown;
  FEdit.Parent:=Self;
end;

{ TMultiItemDlg }

procedure TMultiItemDlg.RadioGroupSelectionChanged(Sender: TObject);
begin
  FButtonPanel.OKButton.Enabled:=(FPrimaryItemCountRadioBox.ItemIndex > -1);
  if FShadowMenu.IsMainMenu then
    FButtonPanel.OKButton.Enabled:=FButtonPanel.OKButton.Enabled and
      (FPrimaryItemCountRadioBox.ItemIndex > -1);
end;

function TMultiItemDlg.GetPrimaryItemCount: integer;
begin
  Result:=FPrimaryItemCountRadioBox.ItemIndex + 2;
end;

function TMultiItemDlg.GetSubMenuDepth: integer;
begin
  Result:=FSubMenuDepthRadioGroup.ItemIndex + 2;
end;

constructor TMultiItemDlg.CreateWithShadowMenu(aSMenu: TShadowMenu);
begin
  inherited CreateNew(nil);
  FShadowMenu:=aSMenu;
  Position:=poScreenCenter;
  Name:='MultiItemDialog';
  Caption:=lisMenuEditorInitialMenuPopulation;
  FPrimaryItemCountRadioBox:=TRadioGroup.Create(Self);
  with FPrimaryItemCountRadioBox do begin
    Align:=alTop;
    Top:=1;
    BorderSpacing.Around:=Margin;
    Items.CommaText:='2,3,4,5,6,7,8';
    ItemIndex:= -1;
    OnSelectionChanged:=@RadioGroupSelectionChanged;
    case aSMenu.IsMainMenu of
      False: begin
         Caption:=lisMenuEditorNumberOfInitialPopupMenuItems;
         Constraints.MinWidth:=280;
         Columns:=1;
         Height:=180;
        end;
      True: begin
          Caption:=lisMenuEditorNumberOfInitialMenubarItems;
          Constraints.MinWidth:=350;
          Columns:=7;
          Height:=50;
        end;
    end;
    Parent:=Self;
  end;
  if aSMenu.IsMainMenu then begin
    FSubMenuDepthRadioGroup:=TRadioGroup.Create(Self);
    with FSubMenuDepthRadioGroup do begin
      Align:=alTop;
      Top:=2;
      BorderSpacing.Around:=Margin;
      Columns:=1;
      Caption:=lisMenuEditorNumberOfDropdownItemsForEachMenubarItem;
      Items.CommaText:='2,3,4,5,6,7,8';
      ItemIndex:= -1;
      OnSelectionChanged:=@RadioGroupSelectionChanged;
      Height:=180;
      Parent:=Self;
    end;
  end;
  FButtonPanel:=TButtonPanel.Create(Self);
  with FButtonPanel do begin
    ShowButtons:=[pbOK, pbCancel];
    Align:=alTop;
    Top:=3;
    OKButton.Enabled:=False;
    ShowBevel:=False;
    Parent:=Self;
  end;
  AutoSize:=True;
end;

{ TFake }

function TFake.GetShouldBeVisible: boolean;
var
  item: TMenuItem;
begin
  item:=FShadowMenu.SelectedMenuItem;
  if (item = nil) then
    Exit(False)
  else case FAddSubMenu of
    True: Result:=not item.IsLine and (item.Count = 0);
    False: Result:=(item.MenuIndex = Pred(item.Parent.Count));
  end;
end;

procedure TFake.SetVisibilitySizeAndPosition;
var
  selShadow: TShadowItem;
  selMI: TMenuItem;
  w: integer;
begin
  selMI:=FShadowMenu.SelectedMenuItem;
  selShadow:=FShadowMenu.GetShadowForMenuItem(selMI);
  Assert(selShadow<>nil,'TFake.SetVisibilitySizeAndPosition: selectedItem is nil');
  if not ShouldBeVisible then begin
    case FAddSubMenu of
      True: if selMI.IsInMenuBar then
              selShadow.BottomFake:=nil
            else selShadow.RightFake:=nil;
      False: if selMI.IsInMenuBar then
               selShadow.RightFake:=nil
             else selShadow.BottomFake:=nil;
    end;
    Hide;
  end
  else begin
    w:=Treble_DropDown_Text_Offset;
    if (selShadow.Width < w) then
      w:=selShadow.Width;
    case FAddSubMenu of
      True: begin
        if selMI.IsInMenuBar then begin
          SetBounds(selShadow.Left, MenuBar_Height + 1,
                    selShadow.Width, MenuBar_Height);
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
      end;
      False: begin // AddItem
        if selMI.IsInMenuBar then begin
          SetBounds(selShadow.Left + selShadow.Width + 1, 0, w, MenuBar_Height);
          selShadow.ShowingRightFake:=True;
          selShadow.RightFake:=Self;
          selShadow.ShowingBottomFake:=False;
        end
        else begin
          SetBounds(selShadow.ParentBox.Left + selShadow.Left + Gutter_X,
                    selShadow.ParentBox.Top + selShadow.ParentBox.Height + 1,
                    selShadow.Width - Gutter_X, DropDown_Height);
          selShadow.ShowingBottomFake:=True;
          selShadow.BottomFake:=Self;
          selShadow.ShowingRightFake:=False;
        end;
      end;
    end; // case
    Show;
  end;
end;

class function TFake.GetControlClassDefaultSize: TSize;
begin
  Result.cx:=100;
  Result.cy:=DropDown_Height;
end;

procedure TFake.Paint;
type TTexture = (txSquare, txArrowRight, txArrowDown);
var
  r: TRect;

  procedure DoPattern(aTexture: TTexture; aDim: integer);
  var
    row, col, maxRow, maxCol, x1, x2, y1, y2: integer;
    drawBlob: boolean;
  begin
    maxRow:=r.Right - r.Left;
    maxCol:=2*(r.Bottom - r.Top);
    Canvas.Brush.Color:=clBtnFace;
    Canvas.FillRect(r);
    case aTexture of
      txSquare: begin
        Canvas.Brush.Color:=clGradientActiveCaption;
        for row:=0 to maxRow do
          for col:=0 to maxCol do begin
            if not Odd(row) then
              drawBlob:=Odd(col)
            else drawBlob:=not Odd(col);
            if drawBlob then
              Canvas.FillRect(col*aDim, row*aDim, Succ(col)*aDim, Succ(row)*aDim);
          end;
      end;
      txArrowRight: begin
        for row:=0 to maxRow do
          for col:=0 to maxCol do begin
            if not Odd(row) then
              drawBlob:=Odd(col)
            else drawBlob:=not Odd(col);
            if drawBlob then begin
              x1:=col*aDim; y1:=row*aDim-1; y2:=y1+8;
              Canvas.Line(x1, y1, x1, y2);
              Inc(x1);
              Canvas.Line(x1, y1, x1, y2);
              Inc(x1); Inc(y1); Dec(y2);
              Canvas.Line(x1, y1, x1, y2);
              Inc(x1);
              Canvas.Line(x1, y1, x1, y2);
              Inc(x1); Inc(y1); Dec(y2);
              Canvas.Line(x1, y1, x1, y2);
              Inc(x1);
              Canvas.Line(x1, y1, x1, y2);
              Inc(x1); Inc(y1); Dec(y2);
              Canvas.Line(x1, y1, x1, y2);
              Inc(x1);
              Canvas.Line(x1, y1, x1, y2);
              Inc(x1);
              Canvas.Line(x1, y1, x1, y2);
            end;
          end;
      end;
      txArrowDown: begin
        for row:=0 to maxRow do
          for col:=0 to maxCol do begin
            if not Odd(row) then
              drawBlob:=Odd(col)
            else drawBlob:=not Odd(col);
            if drawBlob then begin
              x1:=col*aDim; y1:=row*aDim-1; x2:=x1+8;
              Canvas.Line(x1, y1, x2, y1);
              Inc(y1);
              Canvas.Line(x1, y1, x2, y1);
              Inc(x1); Inc(y1); Dec(x2);
              Canvas.Line(x1, y1, x2, y1);
              Inc(y1);
              Canvas.Line(x1, y1, x2, y1);
              Inc(x1); Inc(y1); Dec(x2);
              Canvas.Line(x1, y1, x2, y1);
              Inc(y1);
              Canvas.Line(x1, y1, x2, y1);
              Inc(x1); Inc(y1); Dec(x2);
              Canvas.Line(x1, y1, x2, y1);
              Inc(y1);
              Canvas.Line(x1, y1, x2, y1);
              Inc(y1);
              Canvas.Line(x1, y1, x2, y1);
            end;
          end;
      end;
    end;
  end;

  procedure Draw(aTexture: TTexture);
  begin
    case aTexture of
      txSquare: DoPattern(aTexture, 6);
      txArrowDown, txArrowRight: DoPattern(aTexture, 7);
    end;
  end;

begin
  r:=ClientRect;
  case FAddSubMenu of
    True: if FShadowMenu.SelectedMenuItem.IsInMenuBar then
            Draw(txArrowDown)
          else Draw(txArrowRight);
    False: Draw(txSquare);
  end;
end;

constructor TFake.CreateWithPurpose(anOwner: TShadowMenu; addsASubmenu: boolean);
begin
  inherited Create(anOwner);
  FAddSubMenu:=addsASubmenu;
  FShadowMenu:=anOwner;
  if FAddSubMenu then
    Name:='AddsSubMenuFake'
  else Name:='AddItemFake';
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, cx, cy);
  BorderStyle:=bsNone;
  Visible:=False;
  Canvas.Pen.Color:=clGradientActiveCaption;
  Parent:=anOwner;
end;

procedure TFake.Refresh;
begin
  if (FShadowMenu.SelectedMenuItem <> nil) then
    SetVisibilitySizeAndPosition;
end;

{ TShadowMenu }

procedure TShadowMenu.AddItemAfter(Sender: TObject);
var
  si: TShadowItem;
begin
  si:=SelectedShadowItem;
  if (si <> nil) then begin
    si.ParentBox.AddItemAndShadow(si, False);
  end;
end;

procedure TShadowMenu.AddItemBefore(Sender: TObject);
var
  si: TShadowItem;
begin
  si:=SelectedShadowItem;
  if (si <> nil) then begin
    si.ParentBox.AddItemAndShadow(si, True);
  end;
end;

procedure TShadowMenu.AddOnClick(Sender: TObject);
var
  compEditor: TDefaultComponentEditor;
begin
  if (FSelectedMenuItem <> nil) then begin
    compEditor:=TDefaultComponentEditor.Create(FSelectedMenuItem, FEditorDesigner);
    try
      compEditor.Edit;
      UpdateSelectedItemInfo;
    finally
      compEditor.Free;
    end;
  end;
end;

procedure TShadowMenu.AdjustSizeAndPosition(Sender: TObject);
var
  h, w, waif, haif, wasf, hasf, whv, hhv: integer;
  selRightmost, selBottommost, selHCentre, selVCentre: integer;
  sb: TShadowBox;
  si: TShadowItem absolute Sender;
  inMenuBar: boolean;

  function Highest(int1, int2, int3: integer): integer;
  begin
    Result:=int1;
    if (int2 > Result) then
      Result:=int2;
    if (int3 > Result) then
      Result:=int3;
  end;

  procedure CalcHighestFakeDims;
  begin
    if (FAddItemFake.Visible) then begin
        waif:=FAddItemFake.BoundsRect.Right;
        haif:=FAddItemFake.BoundsRect.Bottom;
      end
      else begin
        waif:=0;
        haif:=0;
      end;
      if (FAddSubmenuFake.Visible) then begin
        hasf:=FAddSubmenuFake.BoundsRect.Bottom;
        wasf:=FAddSubmenuFake.BoundsRect.Right;
      end
      else begin
        hasf:=0;
        wasf:=0;
      end;
  end;

  procedure CentreNear;
  var
    newLeft, newTop, range: integer;
    viewWidth, viewHCentre, viewHeight, viewVCentre: integer;
  begin
    viewWidth:=Parent.Width;
    if TScrollPanel(Parent).VSBar.Visible then
      Dec(viewWidth, TScrollPanel(Parent).VSBar.Width);
    viewHCentre:=viewWidth div 2;
    if (selHCentre > viewHCentre)
      then newLeft:=viewHCentre - selHCentre;
    if newLeft < (viewWidth - w) then
      newLeft:=viewWidth - w;
    if (newLeft > 0) then
      newLeft:=0;
    viewHeight:=Parent.Height;
    if TScrollPanel(Parent).HSBar.Visible then
      Dec(viewHeight, TScrollPanel(Parent).HSBar.Height);
    viewVCentre:=viewHeight div 2;
    if (selVCentre > viewVCentre) then
      newTop:=viewVCentre - selVCentre;
    if (newTop < viewHeight - h) then
      newTop:=viewHeight - h;
    if (newTop > 0) then
      newTop:=0;
    DisableAlign;
      SetBounds(newLeft, newTop, w, h);
    EnableAlign;
    range:=viewWidth - MenuDesigner.Scroller.HSBar.PageSize;
    MenuDesigner.Scroller.HSBar.Position:=abs(newLeft)*range div w;
    range:=viewHeight - MenuDesigner.Scroller.VSBar.PageSize;
    MenuDesigner.Scroller.VSBar.Position:=abs(newTop)*range div h;
  end;

begin
  Assert(Sender<>nil,'TShadowMenu.AdjustSizeAndPosition: Sender is nil');
  Assert(sender is TShadowItem,'TShadowMenu.AdjustSizeAndPosition: Sender is not TShadowItem');
  Assert(si.RealItem=FSelectedMenuItem,'TShadowMenu.AdjustSizeAndPosition: Sender is not selected');

  if si.HasChildBox(sb) then begin
    whv:=sb.BoundsRect.Right;
    hhv:=sb.BoundsRect.Bottom;
    if (si.ParentBox.BoundsRect.Bottom > hhv) then
      hhv:=si.ParentBox.BoundsRect.Bottom;
  end
  else begin
    whv:=si.ParentBox.BoundsRect.Right;
    hhv:=si.ParentBox.BoundsRect.Bottom;
  end;

  CalcHighestFakeDims;
  w:=Highest(waif, wasf, whv);
  if (w < Parent.Width) then
    w:=Parent.Width;
  h:=Highest(haif, hasf, hhv);
  if (h < Parent.Height) then
    h:=Parent.Height;
  selRightmost:=GetHighestLevelVisibleBox.BoundsRect.Right; // childbox elsewhere may be visible
  if si.ShowingRightFake then begin
    Assert(si.RightFake <> nil,'TShadowMenu.AdjustSizeAndPosition: RightFake is visible yet nil');
    if (si.RightFake.BoundsRect.Right > selRightmost) then
      selRightmost:=si.RightFake.BoundsRect.Right;
  end;
  selHCentre:=si.ParentBox.Left + (selRightmost - si.ParentBox.Left) div 2;
  inMenuBar:=si.RealItem.IsInMenuBar;
  if inMenuBar and (FMenu.Items.Count > 4) and (si.RealItem.MenuIndex < 4) then
    selHCentre:=Parent.Width div 2 - FAddItemFake.Width;
  if inMenuBar and (si.RealItem.MenuIndex = Pred(FMenu.Items.Count)) then begin
    selHCentre:=w div 2 + FAddItemFake.Width;
  end;

  if si.ShowingBottomFake then begin
    Assert(si.BottomFake <> nil,'TShadowMenu.AdjustSizeAndPosition: BottomFake is visible yet nil');
    selBottommost:=si.BottomFake.BoundsRect.Bottom;
  end
  else selBottommost:=si.ParentBox.BoundsRect.Bottom;
  selVCentre:=si.ParentBox.Top + si.Top + (selBottommost - si.Top) div 2;
  CentreNear;
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
  if (MenuDesigner.TotalMenuItemsCount > 1) then
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
    if (FSelectedMenuItem.IsInMenuBar) and (s = cLineCaption) then begin
      RefreshFakes;
      Exit;
    end;
    if (s <> '') then begin
      FSelectedMenuItem.Caption:=s;
      GlobalDesignHook.RefreshPropertyValues;
      GlobalDesignHook.Modified(FSelectedMenuItem);
      UpdateBoxLocationsAndSizes;
      selected.Repaint;
      MenuDesigner.UpdateStatistics;
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
  with MenuDesigner do begin
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

function TShadowMenu.GetBoxCount: integer;
begin
  Result:=FBoxList.Count;
end;

procedure TShadowMenu.CreateShadowBoxesAndItems;
var
  i: integer;

  procedure RecursiveCreateShadows(aParentBox: TShadowBox; aMI: TMenuItem);
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

begin
  if (FMenu.Items.Count > 0) then
    begin
      FRootBox:=TShadowBox.CreateWithParentBox(Self, nil, FMenu.Items);
      for i:=0 to FMenu.Items.Count-1 do begin
        if FIsMainMenu and FMenu.Items[i].IsLine then
          RaiseGDBException(
            lisMenuEditorSomeWidgetsetsDoNotAllowSeparatorsInTheMainMenubar);
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
      nearestMI:=mi.Parent;
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
    FEditorDesigner.Modified;

    if (box.ShadowCount = 0) then begin
      FBoxList.Remove(box);
      box.Parent:=nil;
      RemoveComponent(box);
      FreeAndNil(box);
    end;
    UpdateBoxLocationsAndSizes;
    SetSelectedMenuItem(nearestMI, False, True);
    MenuDesigner.UpdateStatistics;
  end;
end;

procedure TShadowMenu.DeleteShadowAndItemAndChildren(anExistingSI: TShadowItem);
var
  firstBoxToDelete: TShadowBox;

  procedure RecursiveDeleteBox(aBoxToDelete: TShadowBox);
  var
  mi: TMenuItem;
  i: integer;

  procedure DeleteBox(aMI: TMenuItem);
  var
    j: integer;
    sb: TShadowBox;
    si: TShadowItem;
  begin
    for j:=aMI.Count-1 downto 0 do
      DeleteBox(aMI.Items[j]);
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

  begin
    mi:=aBoxToDelete.ParentMenuItem;
    Assert(mi<>nil,'TShadowMenu,DeleteShadowAndItemAndChildren: RecursiveBoxDelete internal error');
    for i:=mi.Count-1 downto 0 do
      DeleteBox(mi.Items[i]);
  end;

  procedure RecursiveDeleteChildrenItemsOf(aMI: TMenuItem);
  var
    i: integer;

    procedure DeleteItm(anItem: TMenuItem);
    var
      j: integer;
    begin
      for j:=anItem.Count-1 downto 0 do
        DeleteItm(anItem.Items[j]);
      anItem.Parent.Remove(anItem);
      GlobalDesignHook.DeletePersistent(TPersistent(anItem));
      GlobalDesignHook.Modified(anItem);
    end;

  begin
    for i:=aMI.Count-1 downto 0 do
      DeleteItm(aMI.Items[i]);
  end;

begin
  if IDEQuestionDialogAb(
     lisMenuEditorDeletingItemWithASubmenu,
     Format('%s%s%s', [lisMenuEditorDeletingThisItemWillDeleteAllSubitemsToo,
                      LineEnding, lisMenuEditorDeleteThisItemAndItsSubitems]),
     mtWarning, [mrYes, lisMenuEditorCancelDeletion, mrNo,
       lisMenuEditorDeleteAllSubitems], False) = mrNo then
  begin
    firstBoxToDelete:=GetBoxWithParentItem(anExistingSI.RealItem);
    Assert(firstBoxToDelete<>nil,'TShadowMenu.DeleteShadowAndItemAndChildren: no children');
    RecursiveDeleteBox(firstBoxToDelete);
    RecursiveDeleteChildrenItemsOf(anExistingSI.RealItem);
    DeleteChildlessShadowAndItem(anExistingSI);
  end;
end;

procedure TShadowMenu.GetUserInitialMenuBuildPolicy;
var
  primaries, depth: integer;
  mr: TModalResult;
begin
  mr:=IDEQuestionDialogAb(lisMenuEditorStartingToCreateAMenu,
        lisMenuEditorDoYouWantToAddMenuItemsOneByOne + LineEnding +
          lisMenuEditorOrStartWithASkeletonMenuOfSeveralItems,
        mtInformation,
        [mrYes, lisMenuEditorAddItemsOneByOne, mrNo, lisMenuEditorStartWithSeveralItems],
        False);
  case mr of
    mrNo: if (ShowMultiItemDlg(Self, primaries, depth) = mrOK) then
            AddManyItems(primaries, depth);
    mrYes: ;
  end;
end;

function TShadowMenu.GetSelectedShadowItem: TShadowItem;
begin
  Result:=GetShadowForMenuItem(FSelectedMenuItem);
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
  MenuDesigner.UpdateStatistics;
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
  MenuDesigner.UpdateStatistics;
end;

procedure TShadowMenu.SetupPopupMenu;
var
  pe: TPopEnum;
  ac: TAction;
  primaryItem, mi: TMenuItem;

  procedure NewPopItem(const aCaption, aHint: string; anOnClick: TNotifyEvent;
            aShortcut: TShortCut=0; aShortCut2: TShortCut=0);
  begin
    ac:=TAction.Create(Self);
    with ac do begin
      ac.ActionList:=FActionList;
      ac.DisableIfNoHandler:=False;
      Tag:=PtrInt(pe);
      Caption:=aCaption;
      Hint:=aHint;
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

  procedure NewPopSub(const aPrimary: TMenuItem; const aCaption, aHint: string;
                      anOnClick: TNotifyEvent; aShortcut: TShortCut=0);
  begin
    ac:=TAction.Create(Self);
    with ac do begin
      ActionList:=FActionList;
      DisableIfNoHandler:=False;
      Tag:=PtrInt(pe);
      Caption:=aCaption;
      Hint:=aHint;
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
  for pe in TPopEnum do begin
    case pe of
      popItemAddOnClick:      NewPopItem(lisMenuEditorAddOnClickHandler,
                                lisMenuEditorAddAnOnClickEventToSelectedItem,
                                @AddOnClick);
      popItemAddBefore:       begin NewPopItem('','', @AddItemBefore, KeyToShortCut(VK_INSERT,[]));
                                MenuDesigner.AddItemAboveButton.Action:=ac;
                              end;
      popItemAddAfter:        begin NewPopItem('','', @AddItemAfter);
                                MenuDesigner.AddItemBelowButton.Action:=ac;
                              end;
      popItemAddSubMenu:      begin NewPopItem('','', @AddSubMenu,KeyToShortCut(VK_INSERT,[ssCtrl]));
                                MenuDesigner.AddSubMenuButton.Action:=ac;
                              end;
      popItemDelete:          begin NewPopItem(lisMenuEditorDeleteItem,
                                lisMenuEditorDeleteTheSelecteditem,
                                @DeleteItem, KeyToShortCut(VK_DELETE, []));
                                MenuDesigner.DeleteItemButton.Action:=ac;
                              end;
      popItemAddSep:          NewSeparatorAction;
      popItemEditCaption:     NewPopItem(lisMenuEditorEditCaption,
                                lisMenuEditorEditTheSelectedItemSCaption,
                                @EditCaption, KeyToShortCut(VK_RETURN, []));
      popItemMoveBefore:      begin NewPopItem('','', @MoveItemBefore, KeyToShortCut(VK_UP,[ssCtrl]));
                                MenuDesigner.MoveItemUpButton.Action:=ac;
                              end;
      popItemMoveAfter:       begin NewPopItem('','', @MoveItemAfter, KeyToShortCut(VK_DOWN,[ssCtrl]));
                                MenuDesigner.MoveItemDownButton.Action:=ac;
                              end;
      popAddImgListIcon:      begin NewPopItem('', lisMenuEditorAddAnIconFromTheMenuSImageList,
                                @AddImageListIcon);
                                FAddImgListIconAction:=ac;
                              end;
      popItemSep:             NewSeparatorAction;
      popSeparators_:         NewPopPrimary(lisMenuEditorSeParators);
      popAddSeparatorBefore:  begin NewPopSub(primaryItem, lisMenuEditorAddSeparatorBefore,
                                lisMenuEditorAddASeparatorBeforeSelectedItem,
                                @AddSeparatorAbove);
                                MenuDesigner.AddSeparatorAboveButton.Action:=ac;
                              end;
      popAddSeparatorAfter:   begin NewPopSub(primaryItem, lisMenuEditorAddSeparatorAfter,
                                lisMenuEditorAddASeparatorAfterSelectedItem,
                                @AddSeparatorBelow);
                                MenuDesigner.AddSeparatorBelowButton.Action:=ac;
                              end;
      popRemoveAllSeparators: NewPopSub(primaryItem, lisMenuEditorRemoveAllSeparators,
                                lisMenuEditorRemoveEverySeparatorinThisSubmenu,
                                @RemoveAllSeparators);
      popCheckRadio:          NewPopItem(lisMenuEditorCheckmarkRadioitem,
                                lisMenuEditorManageCheckMarksAndRadiogroups,
                                @CheckmarkRadioManagement);
      popShortcuts_:          NewPopPrimary(lisMenuEditorShortcUts2);
      popListShortcuts:       NewPopSub(primaryItem, '',
                                lisMenuEditorDisplayAListOfMenuitemShortcuts,
                                @ListShortcuts);
      popListShortcutsAccelerators: NewPopSub(primaryItem, '',
                                      lisMenuEditorDisplayAListOfBothShortcutsAndAcceleratorKeys,
                                      @ListShortcutsAndAccelerators);
      popResolveShortcutConflicts: NewPopSub(primaryItem,
                                     lisMenuEditorResolveShortcutConflicts,
                                     lisMenuEditorDiscoverAndResolveAnyConflictingShortcuts,
                                     @ResolveshortcutConflicts);
      popTemplates_:          NewPopPrimary(lisMenuEditorTemplates);
      popSaveAsTemplate:      NewPopSub(primaryItem, lisMenuEditorSaveMenuAsATemplate,
                                lisMenuEditorSaveThisMenuLayoutForFutureReuse,
                                @SaveAsTemplate);
      popAddFromTemplate:     NewPopSub(primaryItem, lisMenuEditorAddFromTemplate,
                                lisMenuEditorUseAMenuTemplateToConstructMenuItemsHere,
                                @AddFromTemplate);
      popDeleteTemplate:      NewPopSub(primaryItem, lisMenuEditorDeleteMenuTemplate,
                                lisMenuEditorDeletePreviouslySavedMenuTemplate,
                                @DeleteTemplate);
    end; // case
  end; // for pe
end;

procedure TShadowMenu.UpdateBoxLocationsAndSizes;
var
  p: pointer;
  sb: TShadowBox absolute p;
  s: pointer;
  si: TShadowItem absolute s;
  lft, w, idx: integer;
  pt: TPoint;

  function GetMenuBarCumWidthForItemIndex(anIndex: integer): integer;
  var
    w: integer;
    mi: TMenuItem;
  begin
    Result:=0;
    if (anIndex = 0) then
      Exit
    else repeat
      mi:=FMenu.Items[Pred(anIndex)];
      w:=GetStringWidth(mi.Caption, mi.Default) +
         Double_MenuBar_Text_Offset + GetMenuBarIconWidth(mi);
      Inc(Result, w);
      Dec(anIndex)
    until (anIndex <= 0);
  end;

  function GetParentItemHeightInBox(aParentItem: TMenuItem): integer;
  var
    idx: integer = 0;

    function HeightOfItem(anIndex: integer): integer;
    begin
      if aParentItem.Parent.Items[anIndex].IsLine then
        Result:=Separator_Height
      else Result:=DropDown_Height;
    end;

  begin
    Result:=1;
    repeat
      if (idx < aParentItem.MenuIndex) then
        Inc(Result, HeightOfItem(idx));
      Inc(idx);
    until (idx >= aParentItem.MenuIndex);
  end;

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
end;

procedure TShadowMenu.RefreshFakes;
begin
  Application.ProcessMessages;
  FAddItemFake.Refresh;
  FAddSubmenuFake.Refresh;
end;

procedure TShadowMenu.UpdateButtonGlyphs(isInBar: boolean);
begin
  if (FSelectedMenuItem <> nil) and (isInBar <> MenuDesigner.VariableGlyphsInMenuBar) then
    MenuDesigner.LoadVariableButtonGlyphs(isInBar);
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
      idx:=DlgChooseIconFromImageList(FMenu.Images);
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
        idx:=DlgChooseIconFromImageList(FSelectedMenuItem.Parent.SubMenuImages);
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

procedure TShadowMenu.CheckmarkRadioManagement(Sender: TObject);
var
  check, autochk, showAlways, rItem: boolean;
  rGroup: byte;
  bArr: TByteDynArray;
  si: TShadowItem;
begin
  if (FSelectedMenuItem = nil) then
    Exit;
  si:=SelectedShadowItem;
  if (si =nil) then
    Exit;
  if not si.ParentBox.GetHasRadioItemInfo(bArr) then
    SetLength(bArr, 0);
  if GetCheckMarkPropertiesDlg(FSelectedMenuItem, bArr, Self,
                               check, autochk, showAlways, rItem, rGroup) then
    begin
      FSelectedMenuItem.Checked:=check;
      FSelectedMenuItem.AutoCheck:=autochk;
      FSelectedMenuItem.ShowAlwaysCheckable:=showAlways;
      FSelectedMenuItem.RadioItem:=rItem;
      FSelectedMenuItem.GroupIndex:=rGroup;
      SelectedShadowItem.Repaint;
      FEditorDesigner.PropertyEditorHook.RefreshPropertyValues;
    end;
end;

procedure TShadowMenu.DeleteTemplate(Sender: TObject);
begin
  if SavedTemplatesExist then
    if DeleteMenuTemplateDlg then
      MenuDesigner.UpdateTemplatesCount;
end;

procedure TShadowMenu.ListShortcuts(Sender: TObject);
begin
  ListShortCutDlg(True, FMenu);
end;

procedure TShadowMenu.ListShortcutsAndAccelerators(Sender: TObject);
begin
  ListShortCutDlg(False);
end;

procedure TShadowMenu.ResolveShortcutConflicts(Sender: TObject);
begin
  if (ResolvedConflictsDlg <> mrCancel) then
    UpdateActionsEnabledness;
end;

procedure TShadowMenu.SaveAsTemplate(Sender: TObject);
begin
  if (FSelectedMenuItem <> nil) and LevelZeroAndNoGrandchildren(FSelectedMenuItem) then
    begin
      SaveMenuTemplateDlg(FSelectedMenuItem);
      MenuDesigner.UpdateTemplatesCount;
    end;
end;

procedure TShadowMenu.OnObjectPropertyChanged(Sender: TObject;
  NewObject: TPersistent);
var
  propertyEditor: TPropertyEditor absolute Sender;
  i: Integer;
  persistent: TPersistent;
  mi: TMenuItem absolute persistent;
  invalidateNeeded: boolean = False;
  si: TShadowItem;

begin
  if not (Sender is TPropertyEditor) then
    Exit;
  if (NewObject is TAction) then
    for i:=0 to propertyEditor.PropCount-1 do begin
      persistent:=propertyEditor.GetComponent(i);
      if (persistent is TMenuItem) then begin
        si:=GetShadowForMenuItem(mi);
        if (si = nil) then
          Continue
        else InvalidateNeeded:=True;
      end;
    end;
  if InvalidateNeeded then begin
    UpdateBoxLocationsAndSizes;
    RefreshFakes;
    if (FSelectedMenuItem <> nil) then
      SelectedShadowItem.Repaint;
  end;
end;

procedure TShadowMenu.OnDesignerModified(Sender: TObject);
var
  i: integer;
  persistent: TPersistent;
  mi: TMenuItem absolute persistent;
  si: TShadowItem;
  refreshNeeded: boolean = False;
begin
  if (Sender is TPropertyEditor) then begin
    for i:=0 to TPropertyEditor(Sender).PropCount-1 do begin
      persistent:=TPropertyEditor(Sender).GetComponent(i);
      if (persistent is TMenuItem) then begin
        si:=GetShadowForMenuItem(mi);
        if (si = nil) then
          Continue
        else refreshNeeded:=True;
      end;
    end;
    if refreshNeeded then begin
      UpdateBoxLocationsAndSizes;
      if ((mi.Action <> nil) and (TAction(mi.Action).ShortCut <> 0)) or
         (mi.ShortCut <> 0) or (mi.ShortCutKey2 <> 0) then
           MenuDesigner.UpdateShortcutList(True);
      if (FSelectedMenuItem <> nil) then begin
        RefreshFakes;
        SelectedShadowItem.Repaint;
      end;
      MenuDesigner.UpdateStatistics;
    end;
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
      end
      else if FAddedSingleInitialItem then
        GetUserInitialMenuBuildPolicy;
    end;
end;

procedure TShadowMenu.SetSelectedMenuItem(aMI: TMenuItem; viaDesigner,
  prevWasDeleted: boolean);
var
  prevSelectedMenuItem: TMenuItem;
  prevSelectedShadow: TShadowItem;
begin
  if (aMI = nil) then begin
    if prevWasDeleted then
      SetSelectedShadow(nil, nil, False)
    else SetSelectedShadow(FSelectedMenuItem, nil, False);
    FSelectedMenuItem:=nil;
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
        else prevSelectedShadow.ShowDisabled;
        if not AIsDescendantOfB(aMI, prevSelectedMenuItem) then
          prevSelectedShadow.HideChildren;
      end;
      FSelectedMenuItem:=aMI;
      SetSelectedShadow(prevSelectedMenuItem, FSelectedMenuItem, viaDesigner);
    end;
  MenuDesigner.ButtonsGroupBox.Enabled:=(aMI <> nil);
end;

procedure TShadowMenu.SetSelectedShadow(const prevSelectedItem,
  curSelectedItem: TMenuItem; viaDesigner: boolean);
var
  selectedShadow, prevShadow: TShadowItem;
begin
  selectedShadow:=GetShadowForMenuItem(curSelectedItem);
  case (selectedShadow = nil) of
    True:begin
      HideFakes;
      if (curSelectedItem = nil) and viaDesigner then
        UpdateSelectedItemInfo;
    end;
    False: begin
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
      AdjustSizeAndPosition(selectedShadow);

      if not MenuDesigner.Visible then
        MenuDesigner.ShowOnTop;
      selectedShadow.SetFocus;
      UpdateActionsEnabledness;
      RefreshFakes;
    end;
  end;
end;

procedure TShadowMenu.UpdateActionsEnabledness;
var
  ac, ac1, ac2, ac3: TAction;
  pe: TPopEnum;
  isInBar, isFirst, isLast, prevIsSeparator, nextIsSeparator,
    levelZero, levelZeroOr1, primarySCEnabled: boolean;

  function GetActionForEnum(anEnum: TPopEnum): TAction;
  var
    i: integer;
  begin
    for i:=0 to FActionList.ActionCount do
      if TAction(FActionList.Actions[i]).Tag = PtrInt(anEnum) then
        Exit(TAction(FActionList.Actions[i]));
    Result:=nil;
  end;

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
      popItemAddOnClick:     ac.Enabled:=not OnClickIsAssigned(FSelectedMenuItem);
      popItemAddBefore:       if isInBar then begin
                                ac.Caption:=lisMenuEditorAddNewItemBefore;
                                ac.Hint:=
                                  lisMenuEditorAddANewItemBeforeSelectedItem;
                                  end
                              else begin
                                ac.Caption:=lisMenuEditorAddNewItemAbove;
                                ac.Hint:=
                                  lisMenuEditorAddANewItemAboveSelectedItem;
                                  end;
      popItemAddAfter:        if isInBar then begin
                                ac.Caption:=lisMenuEditorAddNeWItemAfter;
                                ac.Hint:=
                                  lisMenuEditorAddANewItemAfterSelectedItem; end
                              else begin
                                ac.Caption:=lisMenuEditorAddNeWItemBelow;
                                ac.Hint:=
                                  lisMenuEditorAddANewItemBelowSelectedItem;
                                  end;
      popItemAddSubMenu:      begin ac.Enabled:=(FSelectedMenuItem.Count = 0) and not FSelectedMenuItem.IsLine;
                                if isInBar then begin
                                  ac.Caption:=lisMenuEditorAddSubmenuBelow;
                                  ac.Hint:=
                                    lisMenuEditorAddASubmenuBelowSelectedItem;
                                    end
                                else begin
                                  ac.Caption:=lisMenuEditorAddSubmenuRight;
                                  ac.Hint:=
                                    lisMenuEditorAddASubmenuAtTheRightOfSelectedItem;
                                end;
                              end;
      popItemDelete:          ac.Enabled:=(FMenu.Items.Count > 1);
      //popItemAddSep
      //popItemEditCaption
      popItemMoveBefore:      begin ac.Enabled:=not isFirst;
                                if isInBar then begin
                                  ac.Caption:=lisMenuEditorMoveItemLeft;
                                  ac.Hint:=
                                    lisMenuEditorMoveSelectedItemToTheLeft; end
                                else begin
                                  ac.Caption:=lisMenuEditorMoveItemUp;
                                  ac.Hint:=lisMenuEditorMoveSelectedItemUp; end;
                              end;
      popItemMoveAfter:       begin ac.Enabled:=not isLast;
                                if isInBar then begin
                                  ac.Caption:=lisMenuEditorMoVeItemRight;
                                  ac.Hint:=
                                    lisMenuEditorMoveSelectedItemToTheRight; end
                                else begin
                                  ac.Caption:=lisMenuEditorMoVeItemDown;
                                  ac.Hint:=lisMenuEditorMoveSelectedItemDown;
                                    end;
                              end;
      popAddImgListIcon:      begin ac.Enabled:=(FMenu.Images <> nil) and (FMenu.Images.Count > 0);
                                if ac.Enabled then begin
                                  if (FSelectedMenuItem.ImageIndex < 0) then
                                    ac.Caption:=Format(lisMenuEditorAddIconFromS2, [FMenu.Images.Name])
                                  else ac.Caption:=lisMenuEditorChangeImagelistIcon;
                                  if (FMenu.Images.Count = 1) and (FSelectedMenuItem.ImageIndex = 0) then
                                    ac.Enabled:=False;
                                end
                                else ac.Caption:=lisMenuEditorAddImagelistIcon2;
                              end;
      //popItemSep
      popSeparators_:         ac.Enabled:=primarySCEnabled;
      popAddSeparatorBefore:  ac.Enabled:=primarySCEnabled and not isFirst and not prevIsSeparator;
      popAddSeparatorAfter:   ac.Enabled:=primarySCEnabled and not isLast and not nextIsSeparator;
      popRemoveAllSeparators: ac.Enabled:=primarySCEnabled and (GetChildSeparatorCount(FSelectedMenuItem.Parent) > 0);
      popCheckRadio:          ac.Enabled:=not isInBar;
      //popShortcuts_
      popListShortcuts:       begin ac.Enabled:=(MenuDesigner.ShortcutMenuItemsCount > 0);
                                ac.Caption:=Format(lisMenuEditorListShortcutsForS, [FMenu.Name]);
                              end;
      popListShortcutsAccelerators: begin ac.Enabled:=(MenuDesigner.ShortcutList.AcceleratorsInContainerCount > 0);
                                ac.Caption:=Format(lisMenuEditorListShortcutsAndAccelerators,[FLookupRoot.Name]);
                              end;
      popResolveShortcutConflicts: ac.Enabled:=(MenuDesigner.ShortcutList.InitialDuplicatesCount > 0);
      popTemplates_:          ac.Enabled:=levelZero or MenuDesigner.TemplatesSaved;
      popSaveAsTemplate:      ac.Enabled:=levelZeroOr1;
      popAddFromTemplate:     ac.Enabled:=levelZero;
      popDeleteTemplate:      ac.Enabled:=MenuDesigner.TemplatesSaved;
    end; // case
  end; // for
  ac:=GetActionForEnum(popShortcuts_);
  ac1:=GetActionForEnum(popListShortcuts);
  ac2:=GetActionForEnum(popListShortcutsAccelerators);
  ac3:=GetActionForEnum(popResolveShortcutConflicts);
  ac.Enabled:=ac1.Enabled or ac2.Enabled or ac3.Enabled;
end;

constructor TShadowMenu.CreateWithMenuAndDims(aMenu: TMenu; aSelect: TMenuItem;
  aWidth, aHeight: integer);
var
  mi: TMenuItem;
begin
  Assert(aMenu<>nil,'TShadowMenu.CreateWithMenuAndDims: TMenu parameter is nil');
  inherited Create(nil);
  FMenu:=aMenu;
  FInitialSelectedMenuItem:=aSelect;
  SetInitialBounds(0, 0, aWidth, aHeight);
  FIsMainMenu:=(FMenu is TMainMenu);
  Name:='ShadowMenu';
  FEditorDesigner:=FindRootDesigner(FMenu) as TComponentEditorDesigner;
  FLookupRoot:=FEditorDesigner.LookupRoot;

  if (FMenu.Items.Count = 0) then begin
    mi:=TMenuItem.Create(FLookupRoot);
    FMenu.Items.Insert(0, mi);
    mi.Name:=FEditorDesigner.CreateUniqueComponentName('TMenuItem');
    mi.Caption:=mi.Name;
    FAddedSingleInitialItem:=True;
  end;

  FBoxList:=TFPList.Create;
  FItemsPopupMenu:=TPopupMenu.Create(Self);
  FItemsPopupMenu.Name:='ItemsPopupMenu';
  FActionList:=TActionList.Create(Self);
  SetupPopupMenu;
  FAddItemFake:=TFake.CreateWithPurpose(Self, False);
  FAddItemFake.OnClick:=@AddItemAfter;
  FAddSubmenuFake:=TFake.CreateWithPurpose(Self, True);
  FAddSubmenuFake.OnClick:=@AddSubMenu;
  ConnectSpeedButtonOnClickMethods;
  GlobalDesignHook.AddHandlerObjectPropertyChanged(@OnObjectPropertyChanged);
  GlobalDesignHook.AddHandlerModified(@OnDesignerModified);
  AutoSize:=False;
  Color:=clBtnFace;
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
var
  s: string;
  method: TMethod;
begin
  if (FSelectedMenuItem = nil) then with MenuDesigner do begin
    Caption:=Format(lisMenuEditorEditingSSNoMenuitemSelected2,
                                 [FMenu.Owner.Name, FMenu.Name]);
    ButtonsGroupBox.Enabled:=False;
  end
  else begin
    method:=GetMethodProp(FSelectedMenuItem, 'OnClick');
    s:=FEditorDesigner.PropertyEditorHook.GetMethodName(method, FSelectedMenuItem);
    if (s = '') then
      s:=lisMenuEditorIsNotAssigned;
    MenuDesigner.Caption:=Format(lisMenuEditorSSSOnClickS,
               [FMenu.Owner.Name, FMenu.Name, FSelectedMenuItem.Name, s]);
    if not MenuDesigner.ButtonsGroupBox.Enabled then
      MenuDesigner.ButtonsGroupBox.Enabled:=True;
  end;
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

function TShadowBox.GetRadioGroupList: TStringList;
var
  ba: TByteDynArray;
  i: integer;
begin
  if not GetHasRadioItemInfo(ba) then
    Exit(nil);
  Result:=TStringList.Create;
  if (FLevel = 0) then
    begin
      for i:=0 to FShadowMenu.FMenu.Items.Count-1 do
        if FShadowMenu.FMenu.Items[i].RadioItem then
          Result.Add(Format('%d,%s "%s"',[FShadowMenu.FMenu.Items[i].GroupIndex,
                            FShadowMenu.FMenu.Items[i].Name, FShadowMenu.FMenu.Items[i].Caption]));
    end
  else
    for i:=0 to FParentMenuItem.Count-1 do
      if FParentMenuItem.Items[i].RadioItem then
        Result.Add(Format('%d,%s "%s"',[FParentMenuItem.Items[i].GroupIndex,
         FParentMenuItem.Items[i].Name, FParentMenuItem.Items[i].Caption]));
  Result.Sort;
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
    MenuDesigner.UpdateStatistics;
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

function TShadowBox.GetHasRadioItemInfo(out aByteArr: TByteDynArray): boolean;
var
  i: integer;

  procedure AddGroup(aGroupIndex: byte);
  begin
    if (aGroupIndex >= Length(aByteArr)) then
      begin
        SetLength(aByteArr, Succ(aGroupIndex));
        aByteArr[aGroupIndex]:=1;
      end
    else Inc(aByteArr[aGroupIndex]);;
  end;

begin
  SetLength(aByteArr, 0);
  if (FLevel = 0) then
    begin
      for i:=0 to FShadowMenu.FMenu.Items.Count-1 do
        if FShadowMenu.FMenu.Items[i].RadioItem then
          AddGroup(FShadowMenu.FMenu.Items[i].GroupIndex);
    end
  else
    for i:=0 to FParentMenuItem.Count-1 do
      if FParentMenuItem.Items[i].RadioItem then
        AddGroup(FParentMenuItem.Items[i].GroupIndex);
  Result:=(Length(aByteArr) > 0);
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
  Assert(aParentItem<>nil,'TShadowBox.CreateWithParentBox: aParentItem parameter is nil');
  inherited Create(aSMenu);
  Name:='ShadowBox' + IntToStr(ShadowBoxID);
  Inc(ShadowBoxID);
  FShadowMenu:=aSMenu;
  FParentBox:=aParentBox;
  if (FParentBox = nil) then
    FLevel:=0
  else FLevel:=aParentBox.Level + 1;
  FParentMenuItem:=aParentItem;
  Canvas.Pen.Color:=clLtGray;
  Canvas.Brush.Color:=clBtnFace;
  FShadowList:=TFPList.Create;
  aSMenu.BoxList.Add(Self);
  Parent:=aSMenu;
end;

destructor TShadowBox.Destroy;
begin
  FreeAndNil(FShadowList);
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
  w:=GetStringWidth(FRealItem.Caption, FRealItem.Default);
  if FRealItem.IsInMenuBar then
    Result:=w + Double_MenuBar_Text_Offset + GetMenuBarIconWidth(FRealItem)
  else Result:=w + Double_DropDown_Text_Offset + GetShortcutWidth;
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

procedure TShadowItem.HideChildren;
var
  child: TMenuItem;
  s: string;

  procedure RecursiveHideChildren(aMI: TMenuItem);
  var
    container: TShadowBox;
    firstChild: TMenuItem;
  begin
    container:=FShadowMenu.GetParentBoxForMenuItem(aMI);
    s:=aMI.Caption;
    Assert(container<>nil,'TShadowItem.HideChildren: missing parent box for '+s);
    container.Hide;
    if (aMI.Count > 0) then begin
      firstChild:=aMI.Items[0];
      Assert(firstChild<>nil,'TShadowItem.HideChildren: missing child');
      RecursiveHideChildren(firstChild);
    end;
  end;

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
  else Result:=DropDown_Height;
end;

function TShadowItem.GetIsInMenuBar: boolean;
begin
  Result:=FRealItem.IsInMenuBar;
end;

function TShadowItem.GetBottomFake: TFake;
begin
  if (FShadowMenu.SelectedShadowItem <> Self) then
    Result:=nil
  else case FRealItem.IsInMenuBar of
    False: if (FShadowMenu.AddItemFake.Visible) then
             Result:=FBottomFake
           else Result:=nil;
    True: if (FShadowMenu.AddSubMenuFake.Visible) then
            Result:=FBottomFake
          else Result:=nil;
  end;
end;

function TShadowItem.GetIsMainMenu: boolean;
begin
  Result:=FShadowMenu.IsMainMenu;
end;

function TShadowItem.GetLevel: integer;
begin
  Result:=FParentBox.Level;
end;

function TShadowItem.GetMenu: TMenu;
begin
  Result:=FShadowMenu.FMenu;
end;

function TShadowItem.GetRightFake: TFake;
begin
  if (FShadowMenu.SelectedShadowItem <> Self) then
    Result:=nil
  else case FRealItem.IsInMenuBar of
    False: if (FShadowMenu.AddSubMenuFake.Visible) then
             Result:=FRightFake
           else Result:=nil;
    True: if FShadowMenu.AddItemFake.Visible then
            Result:=FRightFake
          else Result:=nil;
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
    Inc(Result, GetStringWidth(ShortCutToText(FRealItem.ShortCut), FRealItem.Default));
  hasSC2:=(FRealItem.ShortCutKey2 <> 0);
  if hasSC2 then
    Inc(Result, GetStringWidth(ShortCutToText(FRealItem.ShortCutKey2), FRealItem.Default));
  if (hasSC or hasSC2) then
    Inc(Result, Shortcut_Offset);
  if (hasSC and hasSC2) then
    Inc(Result, GetStringWidth(', ', False));
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

procedure TShadowItem.Paint;
var
  r, gutterR: TRect;
  pt: TPoint;
  dets: TThemedElementDetails;
  textFlags: integer = DT_VCENTER or DT_SINGLELINE or DT_EXPANDTABS or DT_CENTER;

   function GetIconTopLeft: TPoint;
    begin
      Result:=Point(1, 1);
      if (FShadowMenu.FMenu.Images.Height < ClientHeight) then
        Result.y:=(ClientHeight - FShadowMenu.FMenu.Images.Height) div 2;
      if (FShadowMenu.FMenu.Images.Width < Gutter_X) then
        Result.x:=(Gutter_X - FShadowMenu.FMenu.Images.Width) div 2;
    end;

   function GetBitmapLeftTop: TPoint;
    begin
      Result:=Point(1, 1);
      if (FRealItem.Bitmap.Height < ClientHeight) then
        Result.y:=(ClientHeight - FRealItem.Bitmap.Height) div 2;
      if (FRealItem.Bitmap.Width < Gutter_X) then
        Result.x:=(Gutter_X - FRealItem.Bitmap.Width) div 2;
    end;

  procedure DrawMenuBarItem;
  var
    oldFontStyle: TFontStyles;
  begin
    InflateRect(r, 1, 0); // hack needed only on Windows?
    case FState of
      dsNormal:   dets:=ThemeServices.GetElementDetails(tmBarBackgroundActive);
      dsDisabled: dets:=ThemeServices.GetElementDetails(tmBarItemDisabled);
      dsSelected: dets:=ThemeServices.GetElementDetails(tmBarItemPushed);
    end;
    ThemeServices.DrawElement(Canvas.Handle, dets, r);
    if FRealItem.HasIcon and (FRealItem.ImageIndex > -1) and (FShadowMenu.FMenu.Images <> nil) then
      ThemeServices.DrawIcon(Canvas, dets, Point(0,0), FShadowMenu.FMenu.Images, FRealItem.ImageIndex)
    else if (FRealItem.Bitmap <> nil) and not FRealItem.Bitmap.Empty then begin
      pt:=GetBitmapLeftTop;
      Canvas.Draw(pt.x, pt.y, RealItem.Bitmap);
    end;
    r.Left:=GetMenuBarIconWidth(FRealItem);
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

  procedure DrawBackgroundAndGutter;
  begin
    case FState of
      dsNormal, dsDisabled: Canvas.Brush.Color:=clBtnFace;
      dsSelected: Canvas.Brush.Color:=clHighlight;
    end;
    if FRealItem.IsLine and (FState = dsSelected) then
      Canvas.FillRect(r.Left, r.Top+2, r.Right, r.Bottom+2)
    else Canvas.FillRect(r);
    gutterR:=Rect(Gutter_X, 0, Gutter_X+1, ClientHeight);
    LCLIntf.DrawEdge(Canvas.Handle, gutterR, EDGE_ETCHED, BF_LEFT);
  end;

  procedure DrawCheckMarkIcon;

    function GetSubImagesIconTopLeft: TPoint;
    begin
      Result:=Point(1, 1);
      if (FRealItem.Parent.SubMenuImages.Height < ClientHeight) then
        Result.y:=(ClientHeight - FRealItem.Parent.SubMenuImages.Height) div 2;
      if (FRealItem.Parent.SubMenuImages.Width < Gutter_X) then
        Result.x:=(Gutter_X - FRealItem.Parent.SubMenuImages.Width) div 2;
    end;

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
    s, s1, s2: string;
    sc1, sc2: boolean;
    x, y: integer;
    tStyle : TTextStyle;
    align: TAlignment;
  begin
    FillChar(tStyle{%H-}, SizeOf(tStyle), 0);
    Canvas.Brush.Style:=bsClear;
    if FRealItem.RightJustify then
      align:=taRightJustify
    else align:=taLeftJustify;
    with tStyle do begin
      Alignment:=BidiFlipAlignment(align, UseRightToLeftAlignment);
      Layout:=tlCenter;
      SingleLine:=True;
      Clipping:=True;
      ShowPrefix:=True;
      RightToLeft:=UseRightToLeftReading;
      ExpandTabs:=True;
    end;
    if (FRealItem.Caption = '') then
      s:=FRealItem.Name
    else s:=FRealItem.Caption;
    if FRealItem.RightJustify then
      textFlags:=textFlags or DT_RIGHT
    else textFlags:=textFlags or DT_LEFT;
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
          Canvas.TextRect(r, x, y, s);
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
             else s:=s1 + ', ' + s2;
        x:=r.Right - Canvas.TextWidth(s) - DropDown_Height;
        case FState of
          dsNormal: Canvas.TextRect(r, x, y, s, tStyle);
          dsSelected: begin
              OldFontColor:=Canvas.Font.Color;
              Canvas.Font.Color:=clHighlightText;
              Canvas.TextRect(r, x, y, s);
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
  begin
    r.Right:=ClientWidth;
    r.Left:=r.Right - DropDown_Text_Offset;
    dets:=ThemeServices.GetElementDetails(tmPopupSubmenuNormal);
    ThemeServices.DrawElement(Canvas.Handle, dets, r);
  end;

begin
  if not FParentBox.Updating then begin
    r:=ClientRect;
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
  else FShadowMenu.SetSelectedMenuItem(FRealItem, False, False);
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
  aParentBox: TShadowBox; aMI: TMenuItem);
begin
  inherited Create(aParentBox);
  Name:='ShadowItem' + IntToStr(ShadowItemID);
  Inc(ShadowItemID);
  FShadowMenu:=aSMenu;
  FParentBox:=aParentBox;
  FRealItem:=aMI;
  FParentBox.ShadowList.Add(Self);

  Canvas.Brush.Color:=clBtnFace;
  SetInitialBounds(0, 0, GetWidth, GetHeight);
  if FRealItem.Enabled then
    FState:=dsNormal
  else FState:=dsDisabled;
  TabStop:=False;
  TabOrder:= -1;
  PopupMenu:=FShadowMenu.ItemsPopupMenu;
  Parent:=FParentBox;
end;

{ THeader }

procedure THeader.DoHeaderClick(anIndex: integer);
begin
  if Assigned(FOnHeaderClick) then
    FOnHeaderClick(Self, anIndex);
end;

procedure THeader.Paint;
begin
  Canvas.Brush.Color:=Header_Color;
  Canvas.FillRect(ClientRect);
  case FDisplayType of
    dtNone: begin FCol1Header:=''; FCol2Header:=''; end;
    dtBlack: begin
      if (Canvas.Font.Color <> clBlack) then Canvas.Font.Color:=clBlack;
      if (Canvas.Font.Style <> []) then Canvas.Font.Style:=[];
    end;
    dtBlackBold: begin
      if (Canvas.Font.Color <> clBlack) then Canvas.Font.Color:=clBlack;
      if (Canvas.Font.Style <> [fsBold]) then Canvas.Font.Style:=[fsBold];
    end;
    dtGreyed: begin
      if (Canvas.Font.Color <> clGrayText) then Canvas.Font.Color:=clGrayText;
      if (Canvas.Font.Style <> []) then Canvas.Font.Style:=[];
    end;
    dtGreyedBold: begin
      if (Canvas.Font.Color <> clGrayText) then Canvas.Font.Color:=clGrayText;
      if (Canvas.Font.Style <> [fsBold]) then Canvas.Font.Style:=[fsBold];
    end;
  end;
  Canvas.TextOut(FDualDisplay.Col1Right - Leading - FColumn1TextWidth, VTextOffset, FCol1Header);
  Canvas.TextOut(FDualDisplay.Col1Right + Leading, VTextOffset, FCol2Header);
end;

procedure THeader.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i: integer=0;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (X > FDualDisplay.Col1Right) then
    i:=1;
  DoHeaderClick(i);
end;

constructor THeader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDualDisplay:=AOwner as TDualDisplay;
  Align:=alTop;
  Height:=VDim;
  Canvas.Font.Style:=[fsBold];
end;

procedure THeader.AddHeader(const aHeader: string; aDisplayType: TDisplayType);
begin
  FCol2Header:=SplitCommaText(aHeader, FCol1Header);
  FDisplayType:=aDisplayType;
  FColumn1TextWidth:=FDualDisplay.TextWidth(FCol1Header);
  Repaint;
end;

procedure THeader.Clear;
begin
  FColumn1TextWidth:=0;
  FDisplayType:=dtNone;
  Invalidate;
end;

{ TDualDisplay }

function TDualDisplay.GetContentsCount: integer;
begin
  Result:=FContents.SList.Count;
end;

procedure TDualDisplay.HeaderContentsClick(Sender: TObject; index: integer);
begin
  if Assigned(FOnDisplayClick) then begin
    Assert(Sender<>nil,'TDualDisplay.HeaderContentsClick: Sender is nil');
    Assert(index>-1,'TDualDisplay.HeaderContentsClick: index is negative');
    if (Sender is TContents) then begin
      Assert(index<GetContentsCount,'TDualDisplay.HeaderContentsClick: index exceeds contents count');
      FOnDisplayClick(False, index);
    end
    else if (Sender is THeader) then begin
      Assert(index<2,'TDualDisplay.HeaderContentsClick: index value too high');
      FOnDisplayClick(True, index);
    end
    else Assert(True,'TDualDisplay.HeaderContentsClick: Sender is invalid type');
  end;
end;

procedure TDualDisplay.SetCol1Right(AValue: integer);
begin
  if (FCol1Right <> AValue) then begin
    FCol1Right:=AValue;
    FHeader.Invalidate;
    FContents.Invalidate;
  end;
end;

class function TDualDisplay.GetControlClassDefaultSize: TSize;
begin
  Result.cx:=200;
  Result.cy:=120;
end;

function TDualDisplay.TextWidth(const aText: string): integer;
begin
  Result:=Canvas.TextWidth(aText);
end;

constructor TDualDisplay.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name:='DualDisplay';
  Color:=clBtnFace;
  Canvas.Font.Style:=[fsBold];
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, cx, cy);

  FHeader:=THeader.Create(Self);
  with FHeader do begin
    Name:='Header';
    OnHeaderClick:=@HeaderContentsClick;
    Parent:=Self;
  end;

  FSBox:=TScrollBox.Create(Self);
  with FSBox do begin
    Align:=alClient;
    BorderStyle:=bsNone;
    AutoScroll:=True;
    Parent:=Self;
  end;

  FContents:=TContents.Create(Self);
  with FContents do begin
    Name:='Contents';
    SetInitialBounds(0, 0, FSBox.Width, FSBox.Height);
    OnContentsClick:=@HeaderContentsClick;
    Color:=clBtnFace;
    Parent:=FSBox;
  end;
end;

procedure TDualDisplay.AddLine(const aLine: string; aDT: TDisplayType);
var
  tmp: integer;
begin
  FContents.AddToList(aLine, aDT);
  tmp:=FCol1Right - Double_Leading;
  if (FContents.Col1MaxTextWidth > tmp) then
    SetCol1Right(FContents.Col1MaxTextWidth + Double_Leading);
  tmp:=FContents.Width;
  if (tmp > ClientWidth) then begin
    Width:=tmp;
    FHeader.Width:=tmp;
  end;
end;

procedure TDualDisplay.BeginUpdate;
begin
  FUpdating:=True;
end;

procedure TDualDisplay.AddHeader(const aHeader: string; aDT: TDisplayType);
var
  tmp: integer;
begin
  FHeader.AddHeader(aHeader, aDT);
  tmp:=FCol1Right - Double_Leading;
  if (FHeader.Column1TextWidth > tmp) then
    SetCol1Right(FHeader.Column1TextWidth + Double_Leading);
  tmp:=TextWidth(aHeader) + Treble_Leading;
  if (tmp > Width) then begin
    Width:=tmp;
    FHeader.Width:=tmp;
    FContents.Width:=tmp;
  end;
  FHeader.Repaint;
end;

procedure TDualDisplay.ClearHeader;
begin
  FHeader.Clear;
end;

procedure TDualDisplay.EndUpdate;
begin
  FUpdating:=False;
end;

procedure TDualDisplay.ClearContents;
begin
  FContents.Clear;
end;

procedure TDualDisplay.InvalidateContents;
begin
  FContents.Invalidate;
end;

procedure TDualDisplay.Clear;
begin
  FHeader.Clear;
  FContents.Clear;
end;

{ TContents }

procedure TContents.DoContentsClick(anIndex: integer);
begin
  if Assigned(FOnContentsClick) and (anIndex < FSList.Count) then
    FOnContentsClick(Self, anIndex);
end;

procedure TContents.Paint;
var
  s, s1, s2: string;
  i: integer = 0;
  col1, col2: integer;
  dt: TDisplayType;
begin
  if FDualDisplay.Updating then
    Exit;
  Canvas.FillRect(ClientRect);
  col2:=FDualDisplay.Col1Right + Leading;
  for s in FSList do begin
    s2:=SplitCommaText(s, s1);
    col1:=FDualDisplay.Col1Right - Leading - Canvas.TextWidth(s1);
    dt:=TDisplayType(PtrUInt(FSList.Objects[i]));
    case dt of
      dtNone: begin s1:=''; s2:=''; end;
      dtBlack: begin
        if (Canvas.Font.Color <> clBlack) then Canvas.Font.Color:=clBlack;
        if (Canvas.Font.Style <> []) then Canvas.Font.Style:=[];
      end;
      dtBlackBold: begin
        if (Canvas.Font.Color <> clBlack) then Canvas.Font.Color:=clBlack;
        if (Canvas.Font.Style <> [fsBold]) then Canvas.Font.Style:=[fsBold];
      end;
      dtGreyed: begin
        if (Canvas.Font.Color <> clGrayText) then Canvas.Font.Color:=clGrayText;
        if (Canvas.Font.Style <> []) then Canvas.Font.Style:=[];
      end;
      dtGreyedBold: begin
        if (Canvas.Font.Color <> clGrayText) then Canvas.Font.Color:=clGrayText;
        if (Canvas.Font.Style <> [fsBold]) then Canvas.Font.Style:=[fsBold];
      end;
    end;
    Canvas.TextOut(col1, i*VDim + VTextOffset, s1);
    Canvas.TextOut(col2, i*VDim + VTextOffset, s2);
    Inc(i);
  end;
end;

procedure TContents.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  DoContentsClick(Y div VDim);
end;

constructor TContents.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDualDisplay:=AOwner as TDualDisplay;
  FSList:=TStringList.Create;
  Color:=clBtnFace;
end;

destructor TContents.Destroy;
begin
  FreeAndNil(FSList);
  inherited Destroy;
end;

procedure TContents.AddToList(const aLine: string; aDisplayType: TDisplayType);
var
  h, w, cw, ch: integer;
  second, first: string;
begin
  Assert(Parent<>nil,'TContents.AddToList: Parent is nil');
  Assert(aDisplayType<>dtNone,'TContents.AddToList: TDisplayType=dtNone');
  FSList.AddObject(aLine, TObject(PtrUInt(aDisplayType)));
  second:=SplitCommaText(aLine, first);
  w:=FDualDisplay.TextWidth(second);
  if (w > FCol2MaxTextWidth) then
    FCol2MaxTextWidth:=w;
  w:=FDualDisplay.TextWidth(first);
  if (w > FCol1MaxTextWidth) then
    FCol1MaxTextWidth:=w;
  w:=FCol1MaxTextWidth + FCol2MaxTextWidth + Treble_Leading;
  if (w < Parent.Width) then
    w:=Parent.Width;
  h:=FSList.Count*VDim;
  ch:=ClientHeight;
  cw:=ClientWidth;
  if (h > ch) or (w > cw) then
    SetBounds(0, 0, w, h);
end;

procedure TContents.Clear;
begin
  FSList.Clear;
  Height:=0;
end;

end.

