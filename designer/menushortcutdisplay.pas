unit MenuShortcutDisplay;

{$mode objfpc}{$H+}

interface

uses
  // FCL + LCL
  Classes, SysUtils,
  ButtonPanel, Controls, StdCtrls, Menus, Forms, LCLIntf, LCLProc,
  // LazUtils
  LazUTF8,
  // IdeIntf
  FormEditingIntf, PropEdits,
  // IDE
  LazarusIDEStrConsts, MenuDesignerBase, MenuShortcuts;

type

  { TShortcutDisplayDlg }

  TShortcutDisplayDlg = class(TForm)
  strict private
    FLastSortIndex: integer;
    FMenu: TMenu;
    FscList: TStringList;
    FSingleMenuOnly: boolean;
    FShortcutsOnly: boolean;
    FShortcuts: TMenuShortcuts;
    FShadowMenu: TShadowMenuBase;
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
                                        aShadowMenu: TShadowMenuBase; aMenu: TMenu=nil);
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
  end;

function ListShortCutDlg(aShortcuts: TMenuShortcuts; shortcutsOnly: boolean;
  aShadowMenu: TShadowMenuBase; aMenu: TMenu): TModalResult;
function EditCaptionDlg(aMI: TMenuItem; var aShortcut: TShortCut): boolean;

implementation

function ListShortCutDlg(aShortcuts: TMenuShortcuts; shortcutsOnly: boolean;
  aShadowMenu: TShadowMenuBase; aMenu: TMenu): TModalResult;
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

function EditCaptionDlg(aMI: TMenuItem; var aShortcut: TShortCut): boolean;
var
  dlg: TEditCaptionDialog;
begin
  dlg := TEditCaptionDialog.CreateWithMenuItem(nil, aMI, aShortcut);
  try
    Result := dlg.ShowModal = mrOK;
    if Result then
      aShortcut := dlg.NewShortcut;
  finally
    dlg.Free;
  end;
end;

{ TShortcutDisplayDlg }

constructor TShortcutDisplayDlg.CreateWithShortcutsOnly(aShortcuts: TMenuShortcuts;
  shortcutsOnly: boolean; aShadowMenu: TShadowMenuBase; aMenu: TMenu);
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
  si: TShadowItemBase;
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
  si: TShadowItemBase;
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

{ TEditCaptionDialog }

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
      p:=UTF8Pos('&', aMI.Caption);
      if (p > 0) and (p < UTF8Length(aMI.Caption)) then
        ch:=aMI.Caption[Succ(p)] // gets correct case of key
      else
        ch:=Chr(Ord(key));    // fallback
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

end.

