unit MenuResolveConflicts;

{$mode objfpc}{$H+}

interface

uses
  // FCL + LCL
  Classes, SysUtils,
  ActnList, ButtonPanel, Controls, StdCtrls, Menus, Forms, Graphics, LCLProc,
  // IdeIntf
  FormEditingIntf, PropEdits,
  // IDE
  LazarusIDEStrConsts, MenuDesignerBase, MenuShortcuts;

type

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
    FShadowMenu: TShadowMenuBase;
    procedure ConflictsBoxSelectionChange(Sender: TObject; {%H-}User: boolean);
    procedure CreateListboxItems;
    procedure InitialPopulateListBox;
    procedure OKButtonClick(Sender: TObject);
    procedure RePopulateListBox;
    procedure UpdateStatistics;
  public
    constructor Create(aShortcuts: TMenuShortcuts; aShadowMenu: TShadowMenuBase); reintroduce;
    destructor Destroy; override;
  end;


implementation

{ TResolveConflictsDlg }

constructor TResolveConflictsDlg.Create(aShortcuts: TMenuShortcuts;
  aShadowMenu: TShadowMenuBase);
begin
  inherited CreateNew(Nil);
  FShortcuts:=aShortcuts;
  FShortcuts.ShortcutList.ScanContainerForShortcutsAndAccelerators;
  FInitialConflictsCount:=FShortcuts.ShortcutList.InitialDuplicates.Count;
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
      [FShortcuts.ShortcutList.InitialDuplicates.Count]);
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
  infDup: TSCInfo;
begin
  FConflictsListBox.OnSelectionChange:=nil;
  FConflictsListBox.Items.Clear;
  for infDup in FShortcuts.ShortcutList.InitialDuplicates do begin
    sDup:=Format(lisMenuEditorSInS, [ShortCutToText(infDup.Shortcut),
      infDup.ComponentName]);
    infUnique:=FShortcuts.ShortcutList.FindUniqueInfoForShortcut(infDup.Shortcut);
    Assert(infUnique<>nil,'TResolveConflictsDlg.PopulateListBox: missing unique shortcut');
    sUnique:=Format(lisMenuEditorSInS, [ShortCutToText(infUnique.Shortcut),
      infUnique.ComponentName]);
    FConflictsListBox.Items.AddObject(Format(lisMenuEditorSConflictsWithS,[sDup,sUnique]),
                                      infDup);
  end;
  FConflictsListBox.OnSelectionChange:=@ConflictsBoxSelectionChange;
end;

procedure TResolveConflictsDlg.OKButtonClick(Sender: TObject);
var
  newShortcut: TShortCut;
  newCaption: string;
  si: TShadowItemBase = nil;

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
  if (FShortcuts.ShortcutList.InitialDuplicates.Count > 0) then begin
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
  if (FShortcuts.ShortcutList.InitialDuplicates.Count > 0) then begin
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

end.

