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
  Buttons, Classes, ComponentEditors, Controls, ExtCtrls, Forms,
  Graphics, LazarusIDEStrConsts, LazIDEIntf, LCLintf, LCLProc, Menus,
  MenuShadows, PropEdits, StdCtrls, SysUtils, FormEditingIntf, IDEWindowIntf;

type

  { TMenuDesigner }

  TMenuDesigner = class(TForm)
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
    FAcceleratorMenuItemsCount: integer;
    FCaptionedItemsCount:integer;
    FDeepestNestingLevel: integer;
    FEditedMenu: TMenu;
    FGUIEnabled: boolean;
    FIconsCount: integer;
    FPopupAssignments: TStringList;
    FPopupAssignmentsListBox: TListBox;
    FSavedTemplatesCount: integer;
    FScroller: TScrollPanel;
    FShadowMenu: TShadowMenu;
    FShortcutConflictsCount: integer;
    FShortcutList: TSCList;
    FShortcutMenuItemsCount: integer;
    FTemplatesSaved: boolean;
    FTotalMenuItemsCount: integer;
    FVariableGlyphsInMenuBar: boolean;
    FUpdateCount: integer;
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
    procedure SetupPopupAssignmentsDisplay;
  public
    procedure LoadVariableButtonGlyphs(isInMenubar: boolean);
    procedure SetMenu(aMenu: TMenu; aMenuItem: TMenuItem);
    procedure ShowPopupAssignmentsInfo;
    procedure UpdateShortcutList(includeAccelerators: boolean=False);
    procedure UpdateStatistics;
    procedure UpdateTemplatesCount;
    procedure UpdateSubmenuGroupBox(selMI: TMenuItem; selBox: TShadowBox; boxIsRoot:boolean);
    procedure BeginUpdate;
    procedure EndUpdate;
    function IsUpdate: Boolean;
    property AcceleratorMenuItemsCount: integer read FAcceleratorMenuItemsCount;
    property EditedMenu: TMenu read FEditedMenu;
    property SavedTemplatesCount: integer read FSavedTemplatesCount;
    property Scroller: TScrollPanel read FScroller;
    property ShadowMenu: TShadowMenu read FShadowMenu;
    property ShortcutConflictsCount: integer read FShortcutConflictsCount;
    property ShortcutList: TSCList read FShortcutList;
    property ShortcutMenuItemsCount: integer read FShortcutMenuItemsCount;
    property TemplatesSaved: boolean read FTemplatesSaved;
    property TotalMenuItemsCount: integer read FTotalMenuItemsCount;
    property VariableGlyphsInMenuBar: boolean read FVariableGlyphsInMenuBar;
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

{$R *.lfm}

var
  MenuDesignerSingleton: TMenuDesigner = nil;

procedure ShowMenuEditor(aMenu: TMenu);
begin
  if (aMenu = nil) then
    RaiseGDBException(lisMenuEditorShowMenuEditorTMenuParameterIsNil);
  MenuDesigner.SetMenu(aMenu, nil);
  SetPopupModeParentForPropertyEditor(MenuDesigner);
  MenuDesigner.ShowOnTop;
end;

function MenuDesigner: TMenuDesigner; // refer always to a single instance
begin
  if (MenuDesignerSingleton = nil) then
    MenuDesignerSingleton:=TMenuDesigner.Create(LazarusIDE.OwningComponent);
  Result:=MenuDesignerSingleton;
end;

{ TMenuDesigner }

procedure TMenuDesigner.FormCreate(Sender: TObject);
begin
  Name:='MenuDesignerWindow';
  Caption:=lisMenuEditorMenuEditor;
  ButtonsGroupBox.Caption:=lisMenuEditorMoveSeparateDeleteInsertItems;
  FEditedMenu:=nil;
  FGUIEnabled:=False;
  LoadFixedButtonGlyphs;
  LoadVariableButtonGlyphs(True);
  KeyPreview:=True;
  GlobalDesignHook.AddHandlerSetSelection(@OnDesignerSetSelection);
  FShortcutList:=TSCList.Create;
  InitializeStatisticVars;
  FTemplatesSaved:=SavedTemplatesExist;
  SetupPopupAssignmentsDisplay;
end;

procedure TMenuDesigner.FormDestroy(Sender: TObject);
begin
  FEditedMenu:=nil;
  if (GlobalDesignHook <> nil) then
    GlobalDesignHook.RemoveAllHandlersForObject(Self);
  if (FShadowMenu <> nil) then begin
    FShadowMenu.Parent:=nil;
    FShadowMenu.Free;
    FShadowMenu:=nil;
  end;
  FreeAndNil(FShortcutList);
  FreeAndNil(FPopupAssignments);
  if MenuDesignerSingleton=Self then
    MenuDesignerSingleton := nil;
end;

procedure TMenuDesigner.HelpButtonClick(Sender: TObject);
const
  helpPath = 'http://wiki.lazarus.freepascal.org/IDE_Window:_menu_editor';
begin
  //LazarusHelp.ShowHelpForIDEControl(Self);
  OpenURL(helpPath);
end;

procedure TMenuDesigner.OnDesignerSetSelection(const ASelection: TPersistentSelectionList);
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
        if (mnu = FEditedMenu) and (FShadowMenu <> nil) then
          FShadowMenu.SetSelectedMenuItem(mi, True, False)
        else if (mnu <> nil) then
               SetMenu(mnu, mi);
      end;
    end
  else SetMenu(nil, nil);
end;

procedure TMenuDesigner.ShowPopupAssignmentsInfo;
var
  count: integer;
begin
  if (FEditedMenu <> nil) and (FEditedMenu is TPopupMenu) then begin
    count:=GetPopupAssignmentCount;
    PopupAssignmentsCountLabel.Enabled:=True;
    if (count > 0) then
      PopupAssignmentsCountLabel.BorderSpacing.Bottom:=0
    else PopupAssignmentsCountLabel.BorderSpacing.Bottom:=Double_Margin;
    if (count= -1) then
      PopupAssignmentsCountLabel.Caption:=Format(lisMenuEditorPopupAssignmentsS,[lisMenuEditorNA])
    else PopupAssignmentsCountLabel.Caption:=Format(lisMenuEditorPopupAssignmentsS, [IntToStr(count)]);
    if (count > 0) then begin
      FPopupAssignmentsListBox.Items.Assign(FPopupAssignments);
      FPopupAssignmentsListBox.Visible:=True;
    end
    else FPopupAssignmentsListBox.Visible:=False;
  end;
end;

procedure TMenuDesigner.HidePopupAssignmentsInfo;
begin
  if (FEditedMenu <> nil) and (FEditedMenu is TMainMenu) then begin
    PopupAssignmentsCountLabel.Caption:=Format(lisMenuEditorPopupAssignmentsS,[lisMenuEditorNA]);
    PopupAssignmentsCountLabel.Enabled:=False;
    FPopupAssignmentsListBox.Visible:=False;
  end;
end;

procedure TMenuDesigner.SetupPopupAssignmentsDisplay;
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

function TMenuDesigner.GetItemCounts(out aCaptionedItemCount,
  aShortcutItemCount, anIconCount, anAccelCount: integer): integer;
var
  i, imgCount: integer;
  hasImages: boolean;
  sc: TShortCut;

  procedure ProcessItems(aMI: TMenuItem);
  var
    i: integer;
  begin
    Inc(Result);
    if not aMI.IsLine and (aMI.Caption <> '') then begin
      Inc(aCaptionedItemCount);
      if HasAccelerator(aMI.Caption, sc) then
        Inc(anAccelCount);
    end;
    if (aMI.ShortCut <> 0) or (aMI.ShortCutKey2 <> 0) then
      Inc(aShortcutItemCount);
    if hasImages and (aMI.ImageIndex > -1) and (aMI.ImageIndex < imgCount) then
      Inc(anIconCount)
    else if aMI.HasBitmap and not aMI.Bitmap.Empty then
      Inc(anIconCount);
    for i:=0 to aMI.Count-1 do
      ProcessItems(aMI.Items[i]);
  end;

begin
  if (FEditedMenu = nil) then
    Exit;
  aCaptionedItemCount:=0;
  aShortcutItemCount:=0;
  anIconCount:=0;
  imgCount:=0;
  anAccelCount:=0;
  Result:=0;
  hasImages:=(FEditedMenu.Images <> nil) and (FEditedMenu.Images.Count > 0);
  if hasImages then
    imgCount:=FEditedMenu.Images.Count;
  for i:=0 to FEditedMenu.Items.Count-1 do
    ProcessItems(FEditedMenu.Items[i]);
end;

function TMenuDesigner.GetSelectedMenuComponent(const aSelection: TPersistentSelectionList;
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
  else Result:=nil;
end;

function TMenuDesigner.GetPopupAssignmentCount: integer;
var
  lookupRoot: TPersistent;

  procedure ScanLookupRoot;
  var
    frm: TCustomForm;
    i: integer;

    procedure ProcessForPopup(aControl: TControl);
    var
      wc: TWinControl;
      j:integer;
    begin
      if (aControl.PopupMenu = FEditedMenu) and (aControl.Name <> '') then
        FPopupAssignments.Add(aControl.Name);
      if (aControl is TWinControl) then begin
        wc:=TWinControl(aControl);
        for j:=0 to wc.ControlCount-1 do
          ProcessForPopup(wc.Controls[j]);
      end;
    end;

  begin
    frm:=TCustomForm(lookupRoot);
    if (frm.PopupMenu = FEditedMenu) then
      FPopupAssignments.Add(frm.Name);
    for i:=0 to frm.ControlCount-1 do
      ProcessForPopup(frm.Controls[i]);
  end;

begin
  lookupRoot:=GlobalDesignHook.LookupRoot;
  if (FEditedMenu is TMainMenu) or (lookupRoot is TDataModule) then
    Exit(-1)
  else begin
    FreeAndNil(FPopupAssignments);
    FPopupAssignments:=TStringList.Create;
    ScanLookupRoot;
    Result:=FPopupAssignments.Count;
  end
end;

procedure TMenuDesigner.LoadVariableButtonGlyphs(isInMenubar: boolean);
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
  FVariableGlyphsInMenuBar:=isInMenubar;
end;

procedure TMenuDesigner.LoadFixedButtonGlyphs;
begin
  DeleteItemButton.LoadGlyphFromResourceName(HINSTANCE,'laz_delete');
  AddSeparatorAboveButton.LoadGlyphFromResourceName(HINSTANCE,'add_sep_above');
  AddSeparatorBelowButton.LoadGlyphFromResourceName(HINSTANCE,'add_sep_below');
  HelpButton.Hint:=lisMenuEditorGetHelpToUseThisEditor;
end;

procedure TMenuDesigner.EnableGUI(selectedIsNil: boolean);
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

procedure TMenuDesigner.InitializeStatisticVars;
begin
  FShortcutMenuItemsCount:= -1;
  FIconsCount:= -1;
  FDeepestNestingLevel:= -1;
  FCaptionedItemsCount:= -1;
end;

procedure TMenuDesigner.DisableGUI;
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

procedure TMenuDesigner.SetMenu(aMenu: TMenu; aMenuItem: TMenuItem);
var
  selection: TMenuItem;
  w: integer;
begin
  if (aMenu = nil) then begin
    DisableGUI;
    FShadowMenu.SelectedMenuItem:=nil;
    if Assigned(GlobalDesignHook) then
      GlobalDesignHook.RemoveAllHandlersForObject(FShadowMenu);
    FreeAndNil(FShadowMenu);
    FEditedMenu:=nil;
    Application.ProcessMessages;
  end
  else
    begin
      if (aMenu = FEditedMenu) and (FShadowMenu <> nil) then
        FShadowMenu.SetSelectedMenuItem(aMenuItem, True, False)
      else begin
        if (aMenu = FEditedMenu) and (FShadowMenu = nil) then begin
          if (FEditedMenu.Items.Count > 0) then
            selection:=FEditedMenu.Items[0]
          else selection:=nil;
        end
        else if (aMenu <> FEditedMenu) then begin
          if (FShadowMenu <> nil) then
            FreeAndNil(FShadowMenu);
          FEditedMenu:=aMenu;
          selection:=aMenuItem;
        end;

        FGUIEnabled:=False;
        EnableGUI(selection = nil);
        UpdateStatistics;
        FShortcutList.ClearAllLists;
        FShortcutList.ScanContainerForShortcutsAndAccelerators;
        FShortcutConflictsCount:=FShortcutList.InitialDuplicatesCount;
        w:=Width - LeftPanel.Width;
        FShadowMenu:=TShadowMenu.CreateWithMenuAndDims(FEditedMenu, selection, w, Height);
        FScroller:=TScrollPanel.CreateWithChild(Self, FShadowMenu);
        FScroller.Align:=alClient;
        FScroller.Parent:=Self;
      end;
    end;
end;

procedure TMenuDesigner.UpdateStatistics;
var
  captions, shortcuts, icons, accels, tmp: integer;
begin
  if not SameText(StatisticsGroupBox.Caption, FEditedMenu.Name) then
    StatisticsGroupBox.Caption:=FEditedMenu.Name;

  FTotalMenuItemsCount:=GetItemCounts(captions, shortcuts, icons, accels);
  if (FCaptionedItemsCount <> captions) then begin
    FCaptionedItemsCount:=captions;
    CaptionedItemsCountLabel.Caption:=
      Format(lisMenuEditorCaptionedItemsS, [IntToStr(captions)]);
  end;
  if (FShortcutMenuItemsCount <> shortcuts) then begin
    FShortcutMenuItemsCount:=shortcuts;
    ShortcutItemsCountLabel.Caption:=
      Format(lisMenuEditorShortcutItemsS, [IntToStr(FShortcutMenuItemsCount)]);
  end;
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

procedure TMenuDesigner.UpdateShortcutList(includeAccelerators: boolean);
begin
  if includeAccelerators then
    FShortcutList.ScanContainerForShortcutsAndAccelerators
  else FShortcutList.ScanContainerForShortcutsOnly;
end;

procedure TMenuDesigner.UpdateTemplatesCount;
begin
  FTemplatesSaved:=SavedTemplatesExist;
  if not FTemplatesSaved then begin
    FSavedTemplatesCount:=GetSavedTemplatesCount;
    Exit;
  end
  else FSavedTemplatesCount:=GetSavedTemplatesCount;
end;

procedure TMenuDesigner.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TMenuDesigner.EndUpdate;
begin
  if FUpdateCount<=0 then
    RaiseGDBException('');
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    OnDesignerSetSelection(FormEditingHook.GetCurrentObjectInspector.Selection);
end;

function TMenuDesigner.IsUpdate: Boolean;
begin
  Result := FUpdateCount > 0;
end;

procedure TMenuDesigner.UpdateSubmenuGroupBox(selMI: TMenuItem;
  selBox: TShadowBox; boxIsRoot: boolean);
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
      SubmenuGroupBox.Caption:=Format('%s',[lisMenuEditorRootMenu])
    else SubmenuGroupBox.Caption:=Format('%s %s',[selBox.ParentMenuItem.Name, lisMenuEditorSubmenu]);

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

end.
