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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Howard Page-Clark }

unit MenuEditorForm;

{$mode objfpc}{$H+}

interface

uses
  // FCL + LCL
  Classes, SysUtils, typinfo,
  Controls, StdCtrls, ExtCtrls, Forms, Graphics, Buttons, Menus, LCLintf, LCLProc,
  // IdeIntf
  LazIDEIntf, FormEditingIntf, PropEdits,
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
    LeftPanel:TPanel;
    MoveItemDownButton: TSpeedButton;
    MoveItemUpButton: TSpeedButton;
    PopupAssignmentsCountLabel: TLabel;
    RadioGroupsLabel: TLabel;
    ShortcutItemsCountLabel: TLabel;
    StatisticsGroupBox: TGroupBox;
    SubmenuGroupBox: TGroupBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  strict private
    FDesigner: TMenuDesignerBase;
    FEditedMenu: TMenu;
    FAcceleratorMenuItemsCount: integer;
    FCaptionedItemsCount: integer;
    FDeepestNestingLevel: integer;
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
    procedure ScanLookupRoot(aForm: TCustomForm);
    procedure SetupPopupAssignmentsDisplay;
  public
    constructor Create(aDesigner: TMenuDesignerBase);
    destructor Destroy; override;
    procedure LoadVariableButtonGlyphs(isInMenubar: boolean);
    procedure SetMenu(aMenu: TMenu; aMenuItem: TMenuItem);
    procedure ShowPopupAssignmentsInfo;
    procedure BeginUpdate;
    procedure EndUpdate;
    function IsUpdate: Boolean;
    procedure UpdateStatistics;
    procedure UpdateSubmenuGroupBox(selMI: TMenuItem; selBox: TShadowBoxBase; boxIsRoot:boolean);
    procedure UpdateItemInfo(aMenu: TMenu; aMenuItem: TMenuItem;
      aShadowBox: TShadowBoxBase; aPropEditHook: TPropertyEditorHook);
    //property EditedMenu: TMenu read FEditedMenu;
    //property AcceleratorMenuItemsCount: integer read FAcceleratorMenuItemsCount;
  end;

function GetNestingLevelDepth(aMenu: TMenu): integer;


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
  FGUIEnabled:=False;
  LoadFixedButtonGlyphs;
  LoadVariableButtonGlyphs(True);
  KeyPreview:=True;
  GlobalDesignHook.AddHandlerSetSelection(@OnDesignerSetSelection);
  InitializeStatisticVars;
  SetupPopupAssignmentsDisplay;
end;

procedure TMenuDesignerForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FPopupAssignments);
end;

procedure TMenuDesignerForm.HelpButtonClick(Sender: TObject);
const
  helpPath = 'http://wiki.lazarus.freepascal.org/IDE_Window:_menu_editor';
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

  persist:=GetSelectedMenuComponent(ASelection, isTMenu, isTMenuItem);
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
  else
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
  Result:=0;
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

procedure TMenuDesignerForm.ScanLookupRoot(aForm: TCustomForm);
var
  i: integer;
begin
  if (aForm.PopupMenu = FEditedMenu) then
    FPopupAssignments.Add(aForm.Name);
  for i:=0 to aForm.ControlCount-1 do
    ProcessForPopup(aForm.Controls[i]);
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
    ScanLookupRoot(lookupRoot as TCustomForm);
    Result:=FPopupAssignments.Count;
  end
end;

procedure TMenuDesignerForm.LoadVariableButtonGlyphs(isInMenubar: boolean);
begin
  if isInMenubar then
  begin
    MoveItemUpButton.LoadGlyphFromResourceName(HINSTANCE,'arrow_left');
    MoveItemDownButton.LoadGlyphFromResourceName(HINSTANCE,'arrow_right');
    AddItemAboveButton.LoadGlyphFromResourceName(HINSTANCE,'add_item_left');
    AddItemBelowButton.LoadGlyphFromResourceName(HINSTANCE,'add_item_right');
    AddSubMenuButton.LoadGlyphFromResourceName(HINSTANCE,'add_submenu_below');
  end else
  begin
    MoveItemUpButton.LoadGlyphFromResourceName(HINSTANCE,'arrow_up');
    MoveItemDownButton.LoadGlyphFromResourceName(HINSTANCE,'arrow_down');
    AddItemAboveButton.LoadGlyphFromResourceName(HINSTANCE,'add_item_above');
    AddItemBelowButton.LoadGlyphFromResourceName(HINSTANCE,'add_item_below');
    AddSubMenuButton.LoadGlyphFromResourceName(HINSTANCE,'add_submenu_right');
  end;
  UpdateSubmenuGroupBox(nil, nil, False);
  FDesigner.VariableGlyphsInMenuBar:=isInMenubar;
end;

procedure TMenuDesignerForm.LoadFixedButtonGlyphs;
begin
  DeleteItemButton.LoadGlyphFromResourceName(HINSTANCE,'laz_delete');
  AddSeparatorAboveButton.LoadGlyphFromResourceName(HINSTANCE,'add_sep_above');
  AddSeparatorBelowButton.LoadGlyphFromResourceName(HINSTANCE,'add_sep_below');
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
  if FGUIEnabled then begin
    StatisticsGroupBox.Font.Style:=[];
    StatisticsGroupBox.Caption:=lisMenuEditorNoMenuSelected;
    CaptionedItemsCountLabel.Caption:=Format(lisMenuEditorCaptionedItemsS,[lisMenuEditorNA]);
    ShortcutItemsCountLabel.Caption:=Format(lisMenuEditorShortcutItemsS,[lisMenuEditorNA]);
    IconCountLabel.Caption:=Format(lisMenuEditorItemsWithIconS, [lisMenuEditorNA]);
    DeepestNestingLevelLabel.Caption:=Format(lisMenuEditorDeepestNestedMenuLevelS, [lisMenuEditorNA]);
    PopupAssignmentsCountLabel.Caption:=Format(lisMenuEditorPopupAssignmentsS,[lisMenuEditorNA]);
    StatisticsGroupBox.Enabled:=False;
    UpdateSubmenuGroupBox(nil, nil, False);
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
    Application.ProcessMessages;
  end
  else begin
    if (aMenu = FEditedMenu) and (FDesigner.ShadowMenu <> nil) then
      FDesigner.ShadowMenu.SetSelectedMenuItem(aMenuItem, True, False)
    else begin
      if (aMenu = FEditedMenu) and (FDesigner.ShadowMenu = nil) then
      begin
        if (FEditedMenu.Items.Count > 0) then
          selection := FEditedMenu.Items[0]
        else
          selection := nil;
      end
      else if (aMenu <> FEditedMenu) then
      begin
        FDesigner.ShadowMenu.Free;
        FDesigner.ShadowMenu := Nil;
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
begin
  if FUpdateCount<=0 then
    RaiseGDBException('');
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    OnDesignerSetSelection(FormEditingHook.GetCurrentObjectInspector.Selection);
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

procedure TMenuDesignerForm.UpdateSubmenuGroupBox(selMI: TMenuItem;
  selBox: TShadowBoxBase; boxIsRoot: boolean);
begin
  if SubmenuGroupBox = nil then
    Exit;

  if (selMI = nil) then begin
    SubmenuGroupBox.Caption:=lisMenuEditorNoMenuSelected;
    RadioGroupsLabel.Caption:='';
    GroupIndexLabel.Caption:='';
  end
  else begin
    selBox.LastRIValue:=selMI.RadioItem;
    if boxIsRoot then
      SubmenuGroupBox.Caption:=lisMenuEditorRootMenu
    else SubmenuGroupBox.Caption:=Format(lisMenuEditorSSubmenu,[selBox.ParentMenuItem.Name]);

    if selMI.RadioItem then begin
      GroupIndexLabel.Caption:=Format(lisMenuEditorSGroupIndexD,
                                      [selMI.Name, selMI.GroupIndex]);
      GroupIndexLabel.Enabled:=True;
    end
    else begin
      GroupIndexLabel.Caption:=Format(lisMenuEditorSIsNotARadioitem,
                                      [selMI.Name]);
      GroupIndexLabel.Enabled:=False;
    end;

    if selBox.HasRadioItems then begin
      RadioGroupsLabel.Caption:=Format(lisMenuEditorGroupIndexValueSS,
                                       [selBox.RadioGroupsString]);
      RadioGroupsLabel.Enabled:=True;
    end
    else begin
      RadioGroupsLabel.Caption:=lisMenuEditorNoRadioitemsInThisMenu;
      RadioGroupsLabel.Enabled:=False;
      RadioGroupsLabel.Invalidate; //for some reason this seems necessary
    end;
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
    UpdateSubmenuGroupBox(nil, nil, False);
  end
  else begin
    method:=GetMethodProp(aMenuItem, 'OnClick');
    s:=aPropEditHook.GetMethodName(method, aMenuItem);
    if s = '' then
      s:=lisMenuEditorIsNotAssigned;
    Caption:=Format(lisMenuEditorSSSOnClickS,
                    [aMenu.Owner.Name, aMenu.Name, aMenuItem.Name, s]);
    ButtonsGroupBox.Enabled:=True;
    UpdateSubmenuGroupBox(aMenuItem, aShadowBox, aShadowBox.Level=0);
  end;
end;

end.

