{***************************************************************************
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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Author: Howard Page-Clark }

unit MenuEditorForm;

{$mode objfpc}{$H+}

interface

uses
  // FCL
  Classes, SysUtils, Types, typinfo, math,
  // LazUtils
  LazTracer,
  // LCL
  Controls, StdCtrls, ExtCtrls, Forms, Graphics, Buttons, Menus, ButtonPanel,
  ImgList, Themes, LCLintf,
  // IdeIntf
  FormEditingIntf, PropEdits, ObjectInspector, IDEImagesIntf,
  // IDE
  LazarusIDEStrConsts, MenuDesignerBase, MenuShortcuts;

type

  { TMenuDesignerForm }

  TMenuDesignerForm = class(TForm)
    AddItemAboveButton: TSpeedButton;
    AddItemBelowButton: TSpeedButton;
    AddSeparatorAboveButton: TSpeedButton;
    AddSeparatorBelowButton: TSpeedButton;
    AddSubMenuButton: TSpeedButton;
    ButtonsGroupBox: TGroupBox;
    CaptionedItemsCountLabel: TLabel;
    DeepestNestingLevelLabel: TLabel;
    DeleteItemButton: TSpeedButton;
    GroupIndexLabel: TLabel;
    HelpButton: TBitBtn;
    IconCountLabel: TLabel;
    LeftPanel: TPanel;
    MoveItemDownButton: TSpeedButton;
    MoveItemUpButton: TSpeedButton;
    PopupAssignmentsCountLabel: TLabel;
    RadioGroupsLabel: TLabel;
    ShortcutItemsCountLabel: TLabel;
    StatisticsGroupBox: TGroupBox;
    RadioItemGroupBox: TGroupBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  strict private
    FDesigner: TMenuDesignerBase;
    FEditedMenu: TMenu;
    FAcceleratorMenuItemsCount: integer;
    FCaptionedItemsCount: integer;
    FDeepestNestingLevel: integer;
    FAddingItem: Boolean;
    FGUIEnabled: boolean;
    FIconsCount: integer;
    FUpdateCount: integer;
    FPopupAssignments: TStringList;
    FPopupAssignmentsListBox: TListBox;
    function GetItemCounts(out aCaptionedItemCount, aShortcutItemCount,
                           anIconCount, anAccelCount: integer): integer;
    function GetPopupAssignmentCount: integer;
    function GetSelectedMenuComponent(const aSelection: TPersistentSelectionList;
      out isTMenu: boolean; out isTMenuItem: boolean): TPersistent;
    procedure DisableGUI;
    procedure EnableGUI(selectedIsNil: boolean);
    procedure HidePopupAssignmentsInfo;
    procedure InitializeStatisticVars;
    procedure LoadFixedButtonGlyphs;
    procedure OnDesignerSetSelection(const ASelection: TPersistentSelectionList);
    procedure ProcessForPopup(aControl: TControl);
    procedure SetupPopupAssignmentsDisplay;
  public
    constructor Create(aDesigner: TMenuDesignerBase); reintroduce;
    destructor Destroy; override;
    procedure LoadVariableButtonGlyphs(isInMenubar: boolean);
    procedure SetMenu(aMenu: TMenu; aMenuItem: TMenuItem);
    procedure ShowPopupAssignmentsInfo;
    procedure BeginUpdate;
    procedure EndUpdate;
    function IsUpdate: Boolean;
    procedure UpdateStatistics;
    procedure UpdateSubmenuGroupBox(selMI: TMenuItem; selBox: TShadowBoxBase);
    procedure UpdateItemInfo(aMenu: TMenu; aMenuItem: TMenuItem;
      aShadowBox: TShadowBoxBase; aPropEditHook: TPropertyEditorHook);
    //property EditedMenu: TMenu read FEditedMenu;
    //property AcceleratorMenuItemsCount: integer read FAcceleratorMenuItemsCount;
    property AddingItem: Boolean read FAddingItem write FAddingItem;
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
    procedure RefreshLayout;
    property ItemIndex: integer read FItemIndex;
    property OnSelectItem: TNotifyEvent read FOnSelectItem write FOnSelectItem;
  end;

  { TdlgChooseIcon }

  TdlgChooseIcon = class(TForm)
  private
    FButtonPanel: TButtonPanel;
    FRadioIconGroup: TRadioIconGroup;
    procedure dlgChooseIconResize(Sender: TObject);
    function GetImageIndex: integer;
    procedure RIGClick(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure SetRadioIconGroup(anImageList: TCustomImageList);
    property ImageIndex: integer read GetImageIndex;
  end;

function GetNestingLevelDepth(aMenu: TMenu): integer;
function ChooseIconFromImageListDlg(anImageList: TCustomImageList): integer;


implementation

{$R *.lfm}

function GetNestingLevelDepth(aMenu: TMenu): integer;

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

var
  i: integer;
begin
  Result:=0;
  for i:=0 to aMenu.Items.Count-1 do
    CheckLevel(aMenu.Items[i], 0);
end;

function ChooseIconFromImageListDlg(anImageList: TCustomImageList): integer;
var
  dlg: TdlgChooseIcon;
begin
  if (anImageList = nil) or (anImageList.Count = 0) then
    Exit(-1);
  if (anImageList.Count = 1) then
    Exit(0);
  dlg := TdlgChooseIcon.Create(nil);
  try
    dlg.SetRadioIconGroup(anImageList);
    if (dlg.ShowModal = mrOK) then
      Result := dlg.ImageIndex
    else
      Result := -1;
  finally
    dlg.Free;
  end;
end;

{ TMenuDesignerForm }

constructor TMenuDesignerForm.Create(aDesigner: TMenuDesignerBase);
begin
  Inherited Create(Nil);  // LazarusIDE.OwningComponent
  FDesigner := aDesigner;
end;

destructor TMenuDesignerForm.Destroy;
begin
  inherited Destroy;
end;

procedure TMenuDesignerForm.FormCreate(Sender: TObject);
begin
  Name:='MenuDesignerWindow';
  Caption:=lisMenuEditorMenuEditor;
  ButtonsGroupBox.Caption:=lisMenuEditorMenuItemActions;
  RadioItemGroupBox.Caption:=lisMenuEditorRadioItem;
  FGUIEnabled:=False;
  LoadFixedButtonGlyphs;
  LoadVariableButtonGlyphs(True);
  KeyPreview:=True;
  InitializeStatisticVars;
  SetupPopupAssignmentsDisplay;
end;

procedure TMenuDesignerForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FPopupAssignments);
end;

procedure TMenuDesignerForm.FormShow(Sender: TObject);
begin
  GlobalDesignHook.AddHandlerSetSelection(@OnDesignerSetSelection);
end;

procedure TMenuDesignerForm.FormHide(Sender: TObject);
begin
  FDesigner.FreeShadowMenu;
  GlobalDesignHook.RemoveHandlerSetSelection(@OnDesignerSetSelection);
end;

procedure TMenuDesignerForm.HelpButtonClick(Sender: TObject);
const
  helpPath = 'http://wiki.lazarus.freepascal.org/IDE_Window:_Menu_Editor';
begin
  //LazarusHelp.ShowHelpForIDEControl(Self);
  OpenURL(helpPath);
end;

procedure TMenuDesignerForm.OnDesignerSetSelection(const ASelection: TPersistentSelectionList);
var
  mnu: TMenu;
  mi, tmp: TMenuItem;
  isTMenu, isTMenuItem: boolean;
  persist: TPersistent;
begin
  if FUpdateCount > 0 then
    Exit; // This event will be executed after all updates, look at EndUpdate

  //debugln(['TMenuDesignerForm.OnDesignerSetSelection: ']);
  //ASelection.WriteDebugReport;
  persist:=GetSelectedMenuComponent(ASelection, isTMenu, isTMenuItem);
  //debugln(['TMenuDesignerForm.OnDesignerSetSelection isTMenu=',isTMenu,' isTMenuItem=',isTMenuItem,' persist=',DbgSName(persist)]);
  if (persist <> nil) then
  begin
    if isTMenu then
      SetMenu(TMenu(persist), nil)
    else if isTMenuItem then begin
      mi:=TMenuItem(persist);
      tmp:=mi;
      while (tmp.Parent <> nil) do
        tmp:=tmp.Parent;
      mnu:=tmp.Menu;
      if (mnu = nil) then
        mnu:=mi.GetParentMenu;
      if (mnu = FEditedMenu) and (FDesigner.ShadowMenu <> nil) then
        FDesigner.ShadowMenu.SetSelectedMenuItem(mi, True, False)
      else if (mnu <> nil) then
        SetMenu(mnu, mi);
    end;
  end
  else if not AddingItem then
    SetMenu(nil, nil);
end;

procedure TMenuDesignerForm.ShowPopupAssignmentsInfo;
var
  count: integer;
begin
  if (FEditedMenu <> nil) and (FEditedMenu is TPopupMenu) then begin
    count:=GetPopupAssignmentCount;
    PopupAssignmentsCountLabel.Enabled:=True;
    if (count > 0) then
      PopupAssignmentsCountLabel.BorderSpacing.Bottom:=0
    else
      PopupAssignmentsCountLabel.BorderSpacing.Bottom:=Double_Margin;
    if (count= -1) then
      PopupAssignmentsCountLabel.Caption:=Format(lisMenuEditorPopupAssignmentsS,[lisMenuEditorNA])
    else
      PopupAssignmentsCountLabel.Caption:=Format(lisMenuEditorPopupAssignmentsS, [IntToStr(count)]);
    if (count > 0) then begin
      FPopupAssignmentsListBox.Items.Assign(FPopupAssignments);
      FPopupAssignmentsListBox.Visible:=True;
    end
    else
      FPopupAssignmentsListBox.Visible:=False;
  end;
end;

procedure TMenuDesignerForm.HidePopupAssignmentsInfo;
begin
  if (FEditedMenu <> nil) and (FEditedMenu is TMainMenu) then begin
    PopupAssignmentsCountLabel.Caption:=Format(lisMenuEditorPopupAssignmentsS,[lisMenuEditorNA]);
    PopupAssignmentsCountLabel.Enabled:=False;
    FPopupAssignmentsListBox.Visible:=False;
  end;
end;

procedure TMenuDesignerForm.SetupPopupAssignmentsDisplay;
begin
  FPopupAssignmentsListBox:=TListBox.Create(Self);
  with FPopupAssignmentsListBox do begin
    Name:='FPopupAssignmentsListBox';
    Color:=clBtnFace;
    BorderSpacing.Top:=2;
    BorderSpacing.Left:=3*Margin;
    BorderSpacing.Right:=Margin;
    BorderSpacing.Bottom:=Margin;
    Anchors:=[akTop, akLeft, akRight];
    AnchorSideLeft.Control:=StatisticsGroupBox;
    AnchorSideTop.Control:=PopupAssignmentsCountLabel;
    AnchorSideTop.Side:=asrBottom;
    AnchorSideRight.Control:=StatisticsGroupBox;
    AnchorSideRight.Side:=asrBottom;
    ParentFont:=False;
    TabStop:=False;
    BorderStyle:=bsNone;
    ExtendedSelect:=False;
    Height:=45;
    Parent:=StatisticsGroupBox;
    Visible:=False;
  end;
end;

function TMenuDesignerForm.GetItemCounts(out aCaptionedItemCount,
  aShortcutItemCount, anIconCount, anAccelCount: integer): integer;
var
  imgCount: integer;

  procedure ProcessItems(aMI: TMenuItem);
  var
    i: integer;
    sc: TShortCut;
  begin
    Inc(Result);
    if not aMI.IsLine and (aMI.Caption <> '') then begin
      Inc(aCaptionedItemCount);
      if HasAccelerator(aMI.Caption, sc) then
        Inc(anAccelCount);
    end;
    if (aMI.ShortCut <> 0) or (aMI.ShortCutKey2 <> 0) then
      Inc(aShortcutItemCount);
    if (imgCount > 0) and (aMI.ImageIndex > -1) and (aMI.ImageIndex < imgCount) then
      Inc(anIconCount)
    else if aMI.HasBitmap and not aMI.Bitmap.Empty then
      Inc(anIconCount);
    for i:=0 to aMI.Count-1 do
      ProcessItems(aMI.Items[i]);   // Recursive call for sub-menus.
  end;

var
  i: integer;
begin
  Result:=0;
  if (FEditedMenu = nil) then
    Exit;
  aCaptionedItemCount:=0;
  aShortcutItemCount:=0;
  anIconCount:=0;
  anAccelCount:=0;
  if (FEditedMenu.Images <> nil) and (FEditedMenu.Images.Count > 0) then
    imgCount:=FEditedMenu.Images.Count
  else
    imgCount:=0;
  for i:=0 to FEditedMenu.Items.Count-1 do
    ProcessItems(FEditedMenu.Items[i]);
end;

function TMenuDesignerForm.GetSelectedMenuComponent(const aSelection: TPersistentSelectionList;
                                out isTMenu: boolean; out isTMenuItem: boolean): TPersistent;
begin
  if (aSelection.Count = 1) then begin
    if (aSelection.Items[0] is TMenu) then
      begin
        isTMenu:=True;
        isTMenuItem:=False;
        Result:=aSelection.Items[0];
      end
    else
    if (aSelection.Items[0] is TMenuItem) then
      begin
        isTMenu:=False;
        isTMenuItem:=True;
        Result:=aSelection.Items[0];
      end
    else begin
      isTMenu:=False;
      isTMenuItem:=False;
      Result:=nil;
    end;
  end
  else
    Result:=nil;
end;

procedure TMenuDesignerForm.ProcessForPopup(aControl: TControl);
var
  wc: TWinControl;
  j:integer;
begin
  if (aControl.PopupMenu = FEditedMenu) and (aControl.Name <> '') then
    FPopupAssignments.Add(aControl.Name);
  if (aControl is TWinControl) then begin
    wc:=TWinControl(aControl);
    for j:=0 to wc.ControlCount-1 do
      ProcessForPopup(wc.Controls[j]);   // Recursive call
  end;
end;

function TMenuDesignerForm.GetPopupAssignmentCount: integer;
var
  lookupRoot: TPersistent;
begin
  lookupRoot:=GlobalDesignHook.LookupRoot;
  if (FEditedMenu is TMainMenu) or (lookupRoot is TDataModule) then
    Exit(-1)
  else begin
    FreeAndNil(FPopupAssignments);
    FPopupAssignments:=TStringList.Create;
    ProcessForPopup(lookupRoot as TControl);
    Result:=FPopupAssignments.Count;
  end
end;

procedure TMenuDesignerForm.LoadVariableButtonGlyphs(isInMenubar: boolean);
begin
  if isInMenubar then
  begin
    IDEImages.AssignImage(MoveItemUpButton, 'arrow_left');
    IDEImages.AssignImage(MoveItemDownButton, 'arrow_right');
    IDEImages.AssignImage(AddItemAboveButton, 'add_item_left', 24);
    IDEImages.AssignImage(AddItemBelowButton, 'add_item_right', 24);
    IDEImages.AssignImage(AddSubMenuButton, 'add_submenu_below', 24);
  end else
  begin
    IDEImages.AssignImage(MoveItemUpButton, 'arrow_up');
    IDEImages.AssignImage(MoveItemDownButton, 'arrow_down');
    IDEImages.AssignImage(AddItemAboveButton, 'add_item_above', 24);
    IDEImages.AssignImage(AddItemBelowButton, 'add_item_below', 24);
    IDEImages.AssignImage(AddSubMenuButton, 'add_submenu_right', 24);
  end;
  UpdateSubmenuGroupBox(nil, nil);
  FDesigner.VariableGlyphsInMenuBar:=isInMenubar;
end;

procedure TMenuDesignerForm.LoadFixedButtonGlyphs;
begin
  IDEImages.AssignImage(DeleteItemButton, 'laz_delete');
  IDEImages.AssignImage(AddSeparatorAboveButton, 'add_sep_above', 24);
  IDEImages.AssignImage(AddSeparatorBelowButton, 'add_sep_below', 24);
  HelpButton.Hint:=lisMenuEditorGetHelpToUseThisEditor;
end;

procedure TMenuDesignerForm.EnableGUI(selectedIsNil: boolean);
var
  isPopupMenu: boolean;
begin
  if not FGUIEnabled then
  begin
    StatisticsGroupBox.Font.Style:=[fsBold];
    StatisticsGroupBox.Caption:=FEditedMenu.Name;
    StatisticsGroupBox.Enabled:=True;
    ButtonsGroupBox.Enabled:=not selectedIsNil;
    if selectedIsNil then
      Caption:=Format(lisMenuEditorEditingSSNoMenuItemSelected,
        [TComponent(GlobalDesignHook.LookupRoot).Name, FEditedMenu.Name]);
    isPopupMenu:=(FEditedMenu is TPopupMenu);
    LoadVariableButtonGlyphs(not isPopupMenu);
    if isPopupMenu then
      ShowPopupAssignmentsInfo
    else HidePopupAssignmentsInfo;
    FGUIEnabled:=True;
  end;
end;

procedure TMenuDesignerForm.InitializeStatisticVars;
begin
  FDesigner.Shortcuts.ResetMenuItemsCount;
  FIconsCount := -1;
  FDeepestNestingLevel := -1;
  FCaptionedItemsCount := -1;
end;

procedure TMenuDesignerForm.DisableGUI;
begin
  if FGUIEnabled then
  begin
    StatisticsGroupBox.Font.Style:=[];
    StatisticsGroupBox.Caption:=lisMenuEditorNoMenuSelected;
    CaptionedItemsCountLabel.Caption:=Format(lisMenuEditorCaptionedItemsS,[lisMenuEditorNA]);
    ShortcutItemsCountLabel.Caption:=Format(lisMenuEditorShortcutItemsS,[lisMenuEditorNA]);
    IconCountLabel.Caption:=Format(lisMenuEditorItemsWithIconS, [lisMenuEditorNA]);
    DeepestNestingLevelLabel.Caption:=Format(lisMenuEditorDeepestNestedMenuLevelS, [lisMenuEditorNA]);
    PopupAssignmentsCountLabel.Caption:=Format(lisMenuEditorPopupAssignmentsS,[lisMenuEditorNA]);
    StatisticsGroupBox.Enabled:=False;
    UpdateSubmenuGroupBox(nil, nil);
    ButtonsGroupBox.Enabled:=False;
    FPopupAssignmentsListBox.Visible:=False;
    FGUIEnabled:=False;
    InitializeStatisticVars;
    Caption:=Format('%s - %s',[lisMenuEditorMenuEditor, lisMenuEditorNoMenuSelected]);
  end;
end;

procedure TMenuDesignerForm.SetMenu(aMenu: TMenu; aMenuItem: TMenuItem);
var
  selection: TMenuItem;
begin
  if (aMenu = nil) then
  begin
    DisableGUI;
    FDesigner.FreeShadowMenu;
    FEditedMenu:=nil;
  end
  else begin
    if (aMenu = FEditedMenu) and (FDesigner.ShadowMenu <> nil) then
      FDesigner.ShadowMenu.SetSelectedMenuItem(aMenuItem, True, False)
    else begin
      selection := nil;
      if aMenu = FEditedMenu then
      begin
        if (FDesigner.ShadowMenu = nil) and (FEditedMenu.Items.Count > 0) then
          selection := FEditedMenu.Items[0];
      end
      else begin
        FDesigner.FreeShadowMenu;
        FEditedMenu := aMenu;
        selection := aMenuItem;
      end;
      FGUIEnabled := False;
      EnableGUI(selection = nil);
      UpdateStatistics;
      FDesigner.CreateShadowMenu(FEditedMenu, selection, Width-LeftPanel.Width, Height);
    end;
  end;
end;

procedure TMenuDesignerForm.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TMenuDesignerForm.EndUpdate;
var
  OI: TObjectInspectorDlg;
begin
  if FUpdateCount<=0 then
    RaiseGDBException('');
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
  begin
    OI := FormEditingHook.GetCurrentObjectInspector;
    if Assigned(OI) then
      OnDesignerSetSelection(OI.Selection);
  end;
end;

function TMenuDesignerForm.IsUpdate: Boolean;
begin
  Result := FUpdateCount > 0;
end;

procedure TMenuDesignerForm.UpdateStatistics;
var
  captions, shrtcuts, icons, accels, tmp: integer;
  s: String;
begin
  if not SameText(StatisticsGroupBox.Caption, FEditedMenu.Name) then
    StatisticsGroupBox.Caption:=FEditedMenu.Name;

  FDesigner.TotalMenuItemsCount:=GetItemCounts(captions, shrtcuts, icons, accels);
  if (FCaptionedItemsCount <> captions) then begin
    FCaptionedItemsCount:=captions;
    CaptionedItemsCountLabel.Caption:=
      Format(lisMenuEditorCaptionedItemsS, [IntToStr(captions)]);
  end;
  s:=FDesigner.Shortcuts.Statistics(shrtcuts);
  if s <> '' then
    ShortcutItemsCountLabel.Caption := s;
  if (FIconsCount <> icons) then begin
    FIconsCount:=icons;
    IconCountLabel.Caption:=
      Format(lisMenuEditorItemsWithIconS, [IntToStr(FIconsCount)]);
  end;
  if (FAcceleratorMenuItemsCount <> accels) then
    FAcceleratorMenuItemsCount:=accels;
  tmp:=GetNestingLevelDepth(FEditedMenu);
  if (FDeepestNestingLevel <> tmp) then begin
    DeepestNestingLevelLabel.Caption:=
      Format(lisMenuEditorDeepestNestedMenuLevelS, [IntToStr(tmp)]);
    FDeepestNestingLevel:=tmp;
  end;
  StatisticsGroupBox.Invalidate;
end;

function JoinToString(aGroups: TByteArray): String;
var
  i: Integer;
begin
  Result:='';
  for i:=0 to Length(aGroups)-1 do
  begin
    if i>0 then
      Result:=Result+', ';
    Result:=Result+IntToStr(aGroups[i]);
  end;
end;

procedure TMenuDesignerForm.UpdateSubmenuGroupBox(selMI: TMenuItem; selBox: TShadowBoxBase);
var
  Groups: TByteArray;
begin
  if Assigned(selMI) then
    selBox.LastRIValue:=selMI.RadioItem;
  RadioItemGroupBox.Visible:=Assigned(selMI) and selMI.RadioItem;
  if RadioItemGroupBox.Visible then
  begin
    GroupIndexLabel.Caption:=Format(lisMenuEditorGroupIndexD, [selMI.GroupIndex]);
    Groups:=selBox.RadioGroupValues;
    RadioGroupsLabel.Visible:=Length(Groups)>1;
    if RadioGroupsLabel.Visible then
      RadioGroupsLabel.Caption:=Format(lisMenuEditorGroupIndexValuesS, [JoinToString(Groups)]);
    RadioItemGroupBox.Visible:=True;
  end;
end;

procedure TMenuDesignerForm.UpdateItemInfo(aMenu: TMenu; aMenuItem: TMenuItem;
  aShadowBox: TShadowBoxBase; aPropEditHook: TPropertyEditorHook);
var
  s: string;
  method: TMethod;
begin
  if aMenuItem = nil then
  begin
    Caption:=Format(lisMenuEditorEditingSSNoMenuitemSelected,
                    [aMenu.Owner.Name, aMenu.Name]);
    ButtonsGroupBox.Enabled:=False;
    UpdateSubmenuGroupBox(nil, nil);
  end
  else begin
    method:=GetMethodProp(aMenuItem, 'OnClick');
    s:=aPropEditHook.GetMethodName(method, aMenuItem);
    if s = '' then
      s:=lisMenuEditorIsNotAssigned;
    Caption:=Format(lisMenuEditorSSSOnClickS,
                    [aMenu.Owner.Name, aMenu.Name, aMenuItem.Name, s]);
    ButtonsGroupBox.Enabled:=True;
    UpdateSubmenuGroupBox(aMenuItem, aShadowBox);
  end;
end;

{ TRadioIcon }

constructor TRadioIcon.CreateWithGlyph(aRIGroup: TRadioIconGroup;
  anImgIndex: integer);
begin
  Assert(anImgIndex > -1, 'TRadioIcon.CreateWithGlyph: param not > -1');
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

function TRadioIcon.GetChecked: Boolean;
begin
  Result:=FRIState in [risDown, risPressed, risCheckedHot];
end;

procedure TRadioIcon.SetChecked(aValue: Boolean);
begin
  case aValue of
    True: if (FRIState <> risDown) then begin // set to True
            FRIState:=risDown;
            Invalidate;
          end;
    False: if (FRIState <> risUp) then begin // set to False
             FRIState:=risUp;
             Invalidate;
           end;
  end;
end;

procedure TRadioIcon.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TRadioIcon.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and (FRIState in [risUncheckedHot, risUp]) then begin
    FRIState:=risPressed;
    Invalidate;
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
  Invalidate;
end;

procedure TRadioIcon.MouseLeave;
begin
  case FRIState of
    risPressed, risCheckedHot: FRIState:=risDown;
    risUncheckedHot:           FRIState:=risUp;
  end;
  Invalidate;
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
  FBGlyph.Draw(Canvas, ClientRect, FRIGroup.FGlyphPt, bsUp, False, 0, Font.PixelsPerInch, GetCanvasScaleFactor);

  inherited Paint;
end;

{ TRadioIconGroup }

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
  FRadioHeight:=Max(FedSize.cy, anImgList.Height);
  topOffset:=(FRadioHeight - FedSize.cy) div 2;
  FRadioRect:=Rect(0, topOffset, FedSize.cx, topOffset+FedSize.cy);
  FSpacing:=5;
  FRadioWidth:=FedSize.cx + FSpacing + anImgList.Width;
  FGlyphPt:=Point(FedSize.cx+FSpacing, 0);
  FItemIndex:= -1;
  HorzScrollBar.Visible := False;
  CreateRadioItems;
end;

procedure TRadioIconGroup.RefreshLayout;
var
  i: Integer;
  ColCount: Integer;
  RadioIconWidth: Integer;
  RadioIconHeight: Integer;
begin
  RadioIconWidth := FRIArray[0].Width + ScaleX(20, 96);
  RadioIconHeight := FRIArray[0].Height + ScaleX(20, 96);
  ColCount := Max(ClientWidth div RadioIconWidth, 1);
  for i := 0 to High(FRIArray) do
  begin
    FRIArray[i].Left := ScaleX(10, 96) + (i mod ColCount) * RadioIconWidth;
    FRIArray[i].Top := ScaleY(10, 96) + (i div ColCount) * RadioIconHeight;
  end;
end;

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
    RefreshLayout;
    for i:=Low(FRIArray) to High(FRIArray) do
      FRIArray[i].SetParent(Self);
  end;
end;

{ TdlgChooseIcon }

constructor TdlgChooseIcon.Create(TheOwner: TComponent);
begin
  inherited CreateNew(TheOwner);
  Position:=poScreenCenter;
  BorderStyle:=bsSizeable;
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
  OnResize := @dlgChooseIconResize;
end;

function TdlgChooseIcon.GetImageIndex: integer;
begin
  Result:=FRadioIconGroup.ItemIndex;
end;

procedure TdlgChooseIcon.dlgChooseIconResize(Sender: TObject);
begin
  if Assigned(FRadioIconGroup) then
    FRadioIconGroup.RefreshLayout;
end;

procedure TdlgChooseIcon.RIGClick(Sender: TObject);
begin
  FButtonPanel.OKButton.Enabled:=True;
  FButtonPanel.OKButton.SetFocus;
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

end.

