unit DesktopManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types,
  LCLIntf, LCLType, LCLProc, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ButtonPanel, Menus, ComCtrls, ActnList,
  // LazUtils
  Laz2_XMLCfg,
  // IdeIntf
  IDEImagesIntf, ToolBarIntf, IDEWindowIntf, IDEDialogs,
  // IDE
  LazarusIDEStrConsts, EnvironmentOpts, IDEOptionDefs, InputHistory, MainIntf;

type

  { TDesktopForm }

  TDesktopForm = class(TForm)
    AssociatedDebugDesktopComboBox: TComboBox;
    ExportBitBtn: TBitBtn;
    ImportBitBtn: TBitBtn;
    ImportAction: TAction;
    ExportAction: TAction;
    ExportAllAction: TAction;
    AssociatedDebugDesktopLabel: TLabel;
    MoveUpAction: TAction;
    MoveDownAction: TAction;
    DeleteAction: TAction;
    RenameAction: TAction;
    SetDebugDesktopAction: TAction;
    SetActiveDesktopAction: TAction;
    SaveAction: TAction;
    ActionList1: TActionList;
    AutoSaveActiveDesktopCheckBox: TCheckBox;
    ButtonPanel1: TButtonPanel;
    LblGrayedInfo: TLabel;
    ExportMenu: TPopupMenu;
    ExportItem: TMenuItem;
    ExportAllItem: TMenuItem;
    DesktopListBox: TListBox;
    ToolBar1: TToolBar;
    SaveTB: TToolButton;
    ToolButton1: TToolButton;
    SetActiveDesktopTB: TToolButton;
    SetDebugDesktopTB: TToolButton;
    RenameTB: TToolButton;
    DeleteTB: TToolButton;
    MoveUpTB: TToolButton;
    MoveDownTB: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    procedure AssociatedDebugDesktopComboBoxChange(Sender: TObject);
    procedure DeleteActionClick(Sender: TObject);
    procedure DesktopListBoxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; {%H-}State: TOwnerDrawState);
    procedure DesktopListBoxKeyPress(Sender: TObject; var Key: char);
    procedure DesktopListBoxSelectionChange(Sender: TObject; {%H-}User: boolean);
    procedure ExportAllActionClick(Sender: TObject);
    procedure ExportActionClick(Sender: TObject);
    procedure ExportBitBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure ImportActionClick(Sender: TObject);
    procedure MoveUpDownActionClick(Sender: TObject);
    procedure RenameActionClick(Sender: TObject);
    procedure SaveActionClick(Sender: TObject);
    procedure SetActiveDesktopActionClick(Sender: TObject);
    procedure SetDebugDesktopActionClick(Sender: TObject);
  private
    FActiveDesktopChanged: Boolean;

    procedure RefreshList(SelectName: string = '');
    procedure RefreshAssociatedDebugList(SelectedDesktop: TCustomDesktopOpt);
    procedure ExportDesktops(const aDesktops: array of TCustomDesktopOpt);
  end;

  TShowDesktopItem = class(TMenuItem)
  public
    DesktopName: string;
  end;

  TShowDesktopsToolButton = class(TIDEToolButton)
  private class var
    DoChangeDesktopName: string;
  private
    procedure ChangeDesktop(Sender: TObject);
    class procedure DoChangeDesktop({%H-}Data: PtrInt);
    procedure SaveAsDesktop(Sender: TObject);
    procedure ToggleAsDebugDesktop(Sender: TObject);
    procedure MenuOnPopup(Sender: TObject);

    procedure RefreshMenu;
  public
    procedure DoOnAdded; override;
  end;

function ShowDesktopManagerDlg: TModalResult;
function SaveCurrentDesktop(const aDesktopName: string; const aShowOverwriteDialog: Boolean): Boolean;
function ToggleDebugDesktop(const aDesktopName: string; const aShowIncompatibleDialog: Boolean): Boolean;

implementation

{$R *.lfm}

function ShowDesktopManagerDlg: TModalResult;
var
  theForm: TDesktopForm;
  xActiveDesktopChanged: Boolean;
begin
  //IMPORTANT INFORMATION:
  //Desktop Manager must stay a modal dialog! Do not redesign it to a modeless IDE dialog!!!

  theForm := TDesktopForm.Create(Nil);
  try
    theForm.AutoSaveActiveDesktopCheckBox.Checked := EnvironmentOptions.AutoSaveActiveDesktop;

    Result := theForm.ShowModal;

    xActiveDesktopChanged := theForm.FActiveDesktopChanged;
    EnvironmentOptions.AutoSaveActiveDesktop := theForm.AutoSaveActiveDesktopCheckBox.Checked;
  finally
    theForm.Free;
  end;

  if xActiveDesktopChanged then
    EnvironmentOptions.UseDesktop(EnvironmentOptions.ActiveDesktop);
end;

function SaveCurrentDesktop(const aDesktopName: string;
  const aShowOverwriteDialog: Boolean): Boolean;
var
  dskIndex: Integer;
  dsk: TDesktopOpt;
begin
  Result := False;
  if aDesktopName = '' then
    Exit;

  with EnvironmentOptions do
  begin
    dskIndex := Desktops.IndexOf(aDesktopName);
    if (dskIndex >= 0) and
       aShowOverwriteDialog and
       (MessageDlg(Format(dlgOverwriteDesktop, [aDesktopName]), mtWarning, mbYesNo, 0) <> mrYes)
    then
      Exit;

    if (dskIndex >= 0) then//old desktop must be recreated (because of docked/undocked desktops!)
    begin
      debugln(['TDesktopForm.SaveBitBtnClick: Deleting ', aDesktopName]);
      Desktops.Delete(dskIndex);
    end;

    debugln(['TDesktopForm.SaveBitBtnClick: Creating ', aDesktopName]);
    dsk := TDesktopOpt.Create(aDesktopName);
    if dskIndex < 0 then
      Desktops.Add(dsk)
    else
      Desktops.Insert(dskIndex, dsk);
    debugln(['TDesktopForm.SaveBitBtnClick: Assign from active desktop to ', aDesktopName]);
    if ObjectInspector1<>nil then
      EnvironmentOptions.ObjectInspectorOptions.Assign(ObjectInspector1);
    Desktop.ImportSettingsFromIDE(EnvironmentOptions);
    dsk.Assign(Desktop);
    ActiveDesktopName := aDesktopName;
    Result := True;
  end;
end;

function ToggleDebugDesktop(const aDesktopName: string;
  const aShowIncompatibleDialog: Boolean): Boolean;
var
  xDsk: TCustomDesktopOpt;
begin
  Result := False;
  xDsk := EnvironmentOptions.Desktops.Find(aDesktopName);
  if not Assigned(xDsk) then
    Exit;

  if not xDsk.Compatible then
  begin
    if aShowIncompatibleDialog then
      MessageDlg(dlgCannotUseDockedUndockedDesktop, mtError, [mbOK], 0);
    Exit;
  end;

  if EnvironmentOptions.DebugDesktopName = aDesktopName then
    EnvironmentOptions.DebugDesktopName := ''
  else
    EnvironmentOptions.DebugDesktopName := aDesktopName;
  Result := True;
end;

procedure TShowDesktopsToolButton.ChangeDesktop(Sender: TObject);
begin
  DoChangeDesktopName := (Sender as TShowDesktopItem).DesktopName;
  Application.QueueAsyncCall(@DoChangeDesktop, 1);
end;

class procedure TShowDesktopsToolButton.DoChangeDesktop(Data: PtrInt);
var
  xDesktopName: string;
  xDesktop: TCustomDesktopOpt;
begin
  xDesktopName := DoChangeDesktopName;
  if xDesktopName = '' then
    Exit;

  xDesktop := EnvironmentOptions.Desktops.Find(xDesktopName);
  if xDesktop = nil then
    Exit;

  if not xDesktop.Compatible or not (xDesktop is TDesktopOpt) then
  begin
    MessageDlg(dlgCannotUseDockedUndockedDesktop, mtError, [mbOK], 0);
    Exit;
  end;

  EnvironmentOptions.UseDesktop(TDesktopOpt(xDesktop));
end;

procedure TShowDesktopsToolButton.DoOnAdded;
begin
  inherited DoOnAdded;

  DropdownMenu := TPopupMenu.Create(Self);
  Style := tbsDropDown;
  DropdownMenu.OnPopup := @MenuOnPopup;
  if Assigned(FToolBar) then
    DropdownMenu.Images := IDEImages.Images_16;
end;

procedure TShowDesktopsToolButton.MenuOnPopup(Sender: TObject);
begin
  RefreshMenu;
end;

procedure TShowDesktopsToolButton.RefreshMenu;
  procedure _AddItem(const _Desktop: TCustomDesktopOpt; const _Parent: TMenuItem;
    const _OnClick: TNotifyEvent; const _AllowIncompatible: Boolean);
  var
    xItem: TShowDesktopItem;
  begin
    if not _Desktop.Compatible and not _AllowIncompatible then
      Exit;

    xItem := TShowDesktopItem.Create(_Parent.Menu);
    _Parent.Add(xItem);
    xItem.Caption := _Desktop.Name;
    xItem.OnClick := _OnClick;
    xItem.DesktopName := _Desktop.Name;
    xItem.Checked := _Desktop.Name = EnvironmentOptions.ActiveDesktopName;
    if not _Desktop.Compatible then
      xItem.ImageIndex := IDEImages.LoadImage('state_warning')
    else
    if _Desktop.Name = EnvironmentOptions.DebugDesktopName then
      xItem.ImageIndex := IDEImages.LoadImage('debugger');
  end;

var
  xPM: TPopupMenu;
  i: Integer;
  xDesktop: TCustomDesktopOpt;
  xMISaveAs, xMISaveAsNew, xMIToggleDebug: TMenuItem;
begin
  xPM := DropdownMenu;
  xPM.Items.Clear;

  xMISaveAs := TMenuItem.Create(xPM);
  xMISaveAs.Caption := dlgSaveCurrentDesktopAs;
  xMISaveAs.ImageIndex := IDEImages.LoadImage('laz_save');
  xMIToggleDebug := TMenuItem.Create(xPM);
  xMIToggleDebug.Caption := dlgToggleDebugDesktop;
  xMIToggleDebug.ImageIndex := IDEImages.LoadImage('debugger');
  // Saved desktops
  for i:=0 to EnvironmentOptions.Desktops.Count-1 do
  begin
    xDesktop := EnvironmentOptions.Desktops[i];
    _AddItem(xDesktop, xPM.Items, @ChangeDesktop, False);
    _AddItem(xDesktop, xMISaveAs, @SaveAsDesktop, True);
    _AddItem(xDesktop, xMIToggleDebug, @ToggleAsDebugDesktop, False);
  end;

  if xPM.Items.Count > 0 then
    xPM.Items.AddSeparator;
  xPM.Items.Add(xMISaveAs);
  xPM.Items.Add(xMIToggleDebug);

  if xMISaveAs.Count > 0 then
    xMISaveAs.AddSeparator;
  xMISaveAsNew := TMenuItem.Create(xPM);
  xMISaveAs.Add(xMISaveAsNew);
  xMISaveAsNew.Caption := dlgNewDesktop;
  xMISaveAsNew.OnClick := @SaveAsDesktop;
  xMISaveAsNew.ImageIndex := IDEImages.LoadImage('menu_saveas');
end;

procedure TShowDesktopsToolButton.SaveAsDesktop(Sender: TObject);
var
  xDesktopName: string;
  xShowOverwriteDlg: Boolean;
begin
  if Sender is TShowDesktopItem then
  begin
    xDesktopName := (Sender as TShowDesktopItem).DesktopName;
    xShowOverwriteDlg := False;
  end else
  begin
    if not InputQuery(dlgDesktopName, dlgSaveCurrentDesktopAs, xDesktopName)
    or (xDesktopName = '') // xDesktopName MUST NOT BE EMPTY !!!
    then
      Exit;
    xShowOverwriteDlg := True;
  end;

  SaveCurrentDesktop(xDesktopName, xShowOverwriteDlg);
end;

procedure TShowDesktopsToolButton.ToggleAsDebugDesktop(Sender: TObject);
begin
  ToggleDebugDesktop((Sender as TShowDesktopItem).DesktopName, True);
end;

{ TDesktopForm }

procedure TDesktopForm.FormCreate(Sender: TObject);
begin
  // buttons captions & text
  ToolBar1.Images := IDEImages.Images_16;
  Caption := dlgManageDesktops;
  SaveAction.Hint := dlgSaveCurrentDesktopAs;
  SaveAction.ImageIndex := IDEImages.LoadImage('laz_save');
  DeleteAction.Hint := lisDelete;
  DeleteAction.ImageIndex := IDEImages.LoadImage('laz_cancel');
  RenameAction.Hint := lisRename;
  RenameAction.ImageIndex := IDEImages.LoadImage('laz_edit');
  MoveUpAction.Hint := lisMenuEditorMoveUp;
  MoveUpAction.ImageIndex := IDEImages.LoadImage('arrow_up');
  MoveDownAction.Hint := lisMenuEditorMoveDown;
  MoveDownAction.ImageIndex := IDEImages.LoadImage('arrow_down');
  SetActiveDesktopAction.Hint := dlgSetActiveDesktop;
  SetActiveDesktopAction.ImageIndex := IDEImages.LoadImage('laz_tick');
  SetDebugDesktopAction.Hint := dlgToggleDebugDesktop;
  SetDebugDesktopAction.ImageIndex := IDEImages.LoadImage('debugger');
  AutoSaveActiveDesktopCheckBox.Caption := dlgAutoSaveActiveDesktop;
  AutoSaveActiveDesktopCheckBox.Hint := dlgAutoSaveActiveDesktopHint;
  LblGrayedInfo.Caption := '';
  AssociatedDebugDesktopLabel.Caption := dlgAssociatedDebugDesktop;
  AssociatedDebugDesktopLabel.Hint := dlgAssociatedDebugDesktopHint;
  LblGrayedInfo.Font.Color := clGrayText;

  ExportAction.Hint := lisExport;
  ExportAction.Caption := lisExportSelected;
  ExportAllAction.Caption := lisExportAll;
  ImportAction.Hint := lisImport;
  ExportBitBtn.LoadGlyphFromStock(idButtonSave);
  ExportBitBtn.Caption := lisExportSub;
  ImportBitBtn.LoadGlyphFromStock(idButtonOpen);
  ImportBitBtn.Caption := lisImport;

  ButtonPanel1.HelpButton.TabOrder := 0;
  ExportBitBtn.TabOrder := 1;
  ImportBitBtn.TabOrder := 2;
  ButtonPanel1.OKButton.TabOrder := 3;
end;

procedure TDesktopForm.FormShow(Sender: TObject);
var
  xIndex: Integer;
begin
  RefreshList;
  xIndex := DesktopListBox.Items.IndexOf(EnvironmentOptions.ActiveDesktopName);
  if xIndex >= 0 then
    DesktopListBox.ItemIndex := xIndex;
end;

procedure TDesktopForm.HelpButtonClick(Sender: TObject);
begin
  OpenUrl('http://wiki.freepascal.org/IDE_Window:_Desktops');
end;

procedure TDesktopForm.RefreshList(SelectName: string);
var
  DskTop: TCustomDesktopOpt;
  i: Integer;
  HasNonCompatible: Boolean;
begin
  if (SelectName='') and (DesktopListBox.ItemIndex>=0) then
    SelectName:=DesktopListBox.Items[DesktopListBox.ItemIndex];

  HasNonCompatible := False;
  DesktopListBox.Clear;
  // Saved desktops
  DesktopListBox.Items.BeginUpdate;
  try
    for i:=0 to EnvironmentOptions.Desktops.Count-1 do
    begin
      DskTop := EnvironmentOptions.Desktops[i];
      DesktopListBox.Items.AddObject(DskTop.Name, DskTop);
      if not DskTop.Compatible then
        HasNonCompatible := True;
    end;
  finally
    DesktopListBox.Items.EndUpdate;
  end;
  if HasNonCompatible then
  begin
    if IDEDockMaster=nil then
      LblGrayedInfo.Caption := dlgGrayedDesktopsDocked
    else
      LblGrayedInfo.Caption := dlgGrayedDesktopsUndocked;
  end else
    LblGrayedInfo.Caption := '';

  i := DesktopListBox.Items.IndexOf(SelectName);
  if (i < 0) and (DesktopListBox.Count > 0) then
    i := 0;
  DesktopListBox.ItemIndex := i;

  DesktopListBoxSelectionChange(DesktopListBox, False);
end;

procedure TDesktopForm.RenameActionClick(Sender: TObject);
var
  xDesktopName, xOldDesktopName: String;
  dskIndex: Integer;
begin
  if DesktopListBox.ItemIndex = -1 then
    Exit;

  xDesktopName := DesktopListBox.Items[DesktopListBox.ItemIndex];
  xOldDesktopName := xDesktopName;

  if not InputQuery(dlgRenameDesktop, dlgDesktopName, xDesktopName)
  or (xDesktopName = '') // xDesktopName MUST NOT BE EMPTY !!!
  or (xDesktopName = xOldDesktopName)
  then
    Exit;

  with EnvironmentOptions do
  begin
    dskIndex := Desktops.IndexOf(xDesktopName);//delete old entry in list if new name is present
    if (dskIndex >= 0) then
    begin
      if (MessageDlg(Format(dlgOverwriteDesktop, [xDesktopName]), mtWarning, mbYesNo, 0) = mrYes) then
        Desktops.Delete(dskIndex)
      else
        Exit;
    end;

    dskIndex := Desktops.IndexOf(xOldDesktopName);//rename
    if Desktops[dskIndex].Name = EnvironmentOptions.ActiveDesktopName then
      EnvironmentOptions.ActiveDesktopName := xDesktopName;
    if Desktops[dskIndex].Name = EnvironmentOptions.DebugDesktopName then
      EnvironmentOptions.DebugDesktopName := xDesktopName;
    Desktops[dskIndex].Name := xDesktopName;
    RefreshList(xDesktopName);
  end;
end;

procedure TDesktopForm.AssociatedDebugDesktopComboBoxChange(Sender: TObject);
var
  SelDesktop: TDesktopOpt;
begin
  if DesktopListBox.ItemIndex = -1 then
    Exit;

  SelDesktop := DesktopListBox.Items.Objects[DesktopListBox.ItemIndex] as TDesktopOpt;
  SelDesktop.AssociatedDebugDesktopName := AssociatedDebugDesktopComboBox.Text;
  if SelDesktop.Name = EnvironmentOptions.ActiveDesktopName then
    EnvironmentOptions.Desktop.AssociatedDebugDesktopName := SelDesktop.AssociatedDebugDesktopName;
end;

procedure TDesktopForm.DeleteActionClick(Sender: TObject);
var
  dskName: String;
  dskIndex: Integer;
begin
  if DesktopListBox.ItemIndex = -1 then
    Exit;
  dskName := DesktopListBox.Items[DesktopListBox.ItemIndex];
  if MessageDlg(Format(dlgReallyDeleteDesktop, [dskName]), mtConfirmation, mbYesNo, 0) <> mrYes then
    Exit;
  dskIndex := EnvironmentOptions.Desktops.IndexOf(dskName);
  if dskIndex >= 0 then
  begin
    debugln(['TDesktopForm.SaveBitBtnClick: Deleting ', dskName]);
    EnvironmentOptions.Desktops.Delete(dskIndex);
    if DesktopListBox.ItemIndex+1 < DesktopListBox.Count then
      dskName := DesktopListBox.Items[DesktopListBox.ItemIndex+1]
    else if DesktopListBox.ItemIndex > 0 then
      dskName := DesktopListBox.Items[DesktopListBox.ItemIndex-1]
    else
      dskName := '';
    RefreshList(dskName);
  end;
end;

procedure TDesktopForm.ExportActionClick(Sender: TObject);
var
  xDesktopName: String;
  xDesktop: TCustomDesktopOpt;
begin
  if DesktopListBox.ItemIndex < 0 then
    Exit;

  xDesktopName := DesktopListBox.Items[DesktopListBox.ItemIndex];
  if xDesktopName = '' then
    Exit;

  xDesktop := EnvironmentOptions.Desktops.Find(xDesktopName);
  if xDesktop = nil then
    Exit;

  ExportDesktops([xDesktop]);
end;

procedure TDesktopForm.ExportBitBtnClick(Sender: TObject);
var
  p: TPoint;
begin
  p := ExportBitBtn.ClientToScreen(Point(0,ExportBitBtn.Height));
  ExportMenu.PopUp(p.x,p.y);
end;

procedure TDesktopForm.ExportDesktops(
  const aDesktops: array of TCustomDesktopOpt);
var
  xXMLCfg: TRttiXMLConfig;
  xConfigStore: TXMLOptionsStorage;
  xSaveDialog: TSaveDialog;
  xFileName: string;
  xCurPath: String;
  I: Integer;
begin
  if Length(aDesktops) = 0 then
    Exit;

  xSaveDialog := TSaveDialog.Create(nil);
  try
    try
      InputHistories.ApplyFileDialogSettings(xSaveDialog);
      xSaveDialog.Filter := dlgFilterXML +' (*.xml)|*.xml';
      xSaveDialog.Options := xSaveDialog.Options + [ofOverwritePrompt];
      if xSaveDialog.Execute then
      begin
        xFileName := xSaveDialog.FileName;
        if ExtractFileExt(xFileName) = '' then
          xFileName := xFileName + '.xml';

        xXMLCfg := nil;
        xConfigStore := nil;
        try
          xXMLCfg := TRttiXMLConfig.CreateClean(xFileName);
          xConfigStore := TXMLOptionsStorage.Create(xXMLCfg);
          xCurPath := 'Desktops/';
          xXMLCfg.SetDeleteValue(xCurPath + 'Count', Length(aDesktops), 0);
          for I := 0 to Length(aDesktops)-1 do
          begin
            aDesktops[I].SetConfig(xXMLCfg, xConfigStore);
            aDesktops[I].Save(xCurPath + 'Desktop'+IntToStr(I+1)+'/');
          end;
          xConfigStore.WriteToDisk;
          ShowMessageFmt(dlgDesktopsExported, [Length(aDesktops), xFileName]);
        finally
          xConfigStore.Free;
          xXMLCfg.Free;
        end;
      end;
      InputHistories.StoreFileDialogSettings(xSaveDialog);
    except
      on E: Exception do
      begin
        DebugLn('ERROR: [TDesktopMangerDialog.ExportBitBtnClick] ', E.Message);
        Raise;
      end;
    end;
  finally
    xSaveDialog.Free;
  end;
end;

procedure TDesktopForm.ImportActionClick(Sender: TObject);
var
  xXMLCfg: TRttiXMLConfig;
  xConfigStore: TXMLOptionsStorage;
  xOpenDialog: TIDEOpenDialog;
  xDesktopName, xOldDesktopName, xFileName, xDesktopDockMaster: string;
  xCurPath, xDesktopPath: string;
  I: Integer;
  xCount, xImportedCount: Integer;
  xDsk: TCustomDesktopOpt;
begin
  xOpenDialog := IDEOpenDialogClass.Create(nil);
  try
    try
      InputHistories.ApplyFileDialogSettings(xOpenDialog);
      xOpenDialog.Filter := dlgFilterXML +' (*.xml)|*.xml';
      if xOpenDialog.Execute then
      begin
        xFileName := xOpenDialog.FileName;
        xXMLCfg := nil;
        xConfigStore := nil;
        try
          xXMLCfg := TRttiXMLConfig.Create(xFileName);
          xConfigStore := TXMLOptionsStorage.Create(xXMLCfg);

          xCurPath := 'Desktops/';
          xCount := xXMLCfg.GetValue(xCurPath+'Count', 0);
          xImportedCount := 0;
          for I := 1 to xCount do
          begin
            xDesktopPath := xCurPath+'Desktop'+IntToStr(I)+'/';
            if not xXMLCfg.HasPath(xDesktopPath, True) then
              Continue;

            xDesktopName := xXMLCfg.GetValue(xDesktopPath+'Name', '');
            xOldDesktopName := xDesktopName;
            xDesktopDockMaster := xXMLCfg.GetValue(xDesktopPath+'DockMaster', '');
            if not EnvironmentOptions.DesktopCanBeLoaded(xDesktopDockMaster) then
              Continue; //desktop not compatible

            //show a dialog to modify desktop name
            if (EnvironmentOptions.Desktops.IndexOf(xDesktopName) >= 0) and
               not InputQuery(dlgDesktopName, dlgImportDesktopExists, xDesktopName)
            then
              Continue;

            if xDesktopName = '' then
              Continue;
            xDsk := EnvironmentOptions.Desktops.Find(xDesktopName);
            if Assigned(xDsk) and
               (xOldDesktopName <> xDesktopName) and
               (MessageDlg(Format(dlgOverwriteDesktop, [xDesktopName]), mtWarning, mbYesNo, 0) <> mrYes)
            then
              Continue;

            if Assigned(xDsk) then //if desktop is to be rewritten, it has to be recreated
              EnvironmentOptions.Desktops.Remove(xDsk);

            xDsk := TDesktopOpt.Create(xDesktopName, xDesktopDockMaster<>'');
            EnvironmentOptions.Desktops.Add(xDsk);

            if xDsk.Name = EnvironmentOptions.ActiveDesktopName then
              FActiveDesktopChanged := True;
            xDsk.SetConfig(xXMLCfg, xConfigStore);
            xDsk.Load(xDesktopPath);
            Inc(xImportedCount);
          end;//for

          if xImportedCount>0 then
          begin
            ShowMessageFmt(dlgDesktopsImported, [xImportedCount, xFileName]);
            RefreshList;
          end;
        finally
          xConfigStore.Free;
          xXMLCfg.Free;
        end;
      end;
      InputHistories.StoreFileDialogSettings(xOpenDialog);
    except
      on E: Exception do
      begin
        DebugLn('ERROR: [TDesktopMangerDialog.ImportBitBtnClick] ', E.Message);
        Raise;
      end;
    end;
  finally
    xOpenDialog.Free;
  end;
end;

procedure TDesktopForm.MoveUpDownActionClick(Sender: TObject);
var
  xIncPos: Integer;
  xOldName: String;
begin
  xIncPos := (Sender as TComponent).Tag;
  if (DesktopListBox.ItemIndex < 0) or
     (DesktopListBox.ItemIndex >= EnvironmentOptions.Desktops.Count) or
     (DesktopListBox.ItemIndex+xIncPos < 0) or
     (DesktopListBox.ItemIndex+xIncPos >= EnvironmentOptions.Desktops.Count)
  then
    Exit; //index out of range

  xOldName := EnvironmentOptions.Desktops[DesktopListBox.ItemIndex].Name;
  EnvironmentOptions.Desktops.Move(DesktopListBox.ItemIndex, DesktopListBox.ItemIndex+xIncPos);
  RefreshList(xOldName);
end;

procedure TDesktopForm.RefreshAssociatedDebugList(SelectedDesktop: TCustomDesktopOpt);
var
  DskTop: TCustomDesktopOpt;
  i: Integer;
begin
  AssociatedDebugDesktopComboBox.Items.BeginUpdate;
  try
    AssociatedDebugDesktopComboBox.Clear;
    AssociatedDebugDesktopComboBox.Enabled := SelectedDesktop<>nil;
    if not AssociatedDebugDesktopComboBox.Enabled then
      Exit;
    AssociatedDebugDesktopComboBox.Items.AddObject(dlgPOIconDescNone, nil);
    // Saved desktops
    for i:=0 to EnvironmentOptions.Desktops.Count-1 do
    begin
      DskTop := EnvironmentOptions.Desktops[i];
      if DskTop.Compatible = SelectedDesktop.Compatible then
        AssociatedDebugDesktopComboBox.Items.AddObject(DskTop.Name, DskTop);
    end;
  finally
    AssociatedDebugDesktopComboBox.Items.EndUpdate;
  end;
end;

procedure TDesktopForm.DesktopListBoxDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  xLB: TListBox;
  xDesktopName, xInfo, xText: string;

  OldBrushStyle: TBrushStyle;
  OldTextStyle: TTextStyle;
  NewTextStyle: TTextStyle;
  OldFontStyle: TFontStyles;
  xDesktop: TCustomDesktopOpt;
  xTextLeft, xIconLeft: Integer;
begin
  xLB := Control as TListBox;
  if (Index < 0) or (Index >= xLB.Count) then
    Exit;

  xLB.Canvas.FillRect(ARect);
  OldBrushStyle := xLB.Canvas.Brush.Style;
  xLB.Canvas.Brush.Style := bsClear;

  OldFontStyle := xLB.Canvas.Font.Style;
  OldTextStyle := xLB.Canvas.TextStyle;
  NewTextStyle := OldTextStyle;
  NewTextStyle.Layout := tlCenter;
  NewTextStyle.RightToLeft := Control.UseRightToLeftReading;
  if Control.UseRightToLeftAlignment then
  begin
    NewTextStyle.Alignment := taRightJustify;
    ARect.Right := ARect.Right - 2;
  end
  else
  begin
    NewTextStyle.Alignment := taLeftJustify;
    ARect.Left := ARect.Left + 2;
  end;

  xLB.Canvas.TextStyle := NewTextStyle;

  if Index < EnvironmentOptions.Desktops.Count then
  begin
    xDesktop := EnvironmentOptions.Desktops[Index];
    xDesktopName := xDesktop.Name;
  end else
  begin
    //something went wrong ...
    raise Exception.Create('Desktop manager internal error: the desktop list doesn''t match the listbox content.');
  end;
  xInfo := '';
  xTextLeft := ARect.Left+ToolBar1.Images.Width + 4;
  xIconLeft := ARect.Left+2;
  if (xDesktopName <> '') and (EnvironmentOptions.ActiveDesktopName = xDesktopName) then
  begin
    if xInfo <> '' then
      xInfo := xInfo + ', ';
    xInfo := xInfo + dlgActiveDesktop;
    xLB.Canvas.Font.Style := xLB.Canvas.Font.Style + [fsBold];
    ToolBar1.Images.Draw(xLB.Canvas, xIconLeft, (ARect.Top+ARect.Bottom-ToolBar1.Images.Height) div 2, SetActiveDesktopTB.ImageIndex, xDesktop.Compatible);//I don't see a problem painting the tick over the "run" icon...
  end;
  if (xDesktopName <> '') and (EnvironmentOptions.DebugDesktopName = xDesktopName) then
  begin
    if xInfo <> '' then
      xInfo := xInfo + ', ';
    xInfo := xInfo + dlgDebugDesktop;
    if (EnvironmentOptions.ActiveDesktopName = xDesktopName) then
    begin
      xTextLeft := xTextLeft + ToolBar1.Images.Width;
      xIconLeft := xIconLeft + ToolBar1.Images.Width;
    end;
    ToolBar1.Images.Draw(xLB.Canvas, xIconLeft, (ARect.Top+ARect.Bottom-ToolBar1.Images.Height) div 2, SetDebugDesktopTB.ImageIndex, xDesktop.Compatible);
  end;
  ARect.Left := xTextLeft;
  xText := xDesktopName;
  if xInfo <> '' then
    xText := xText + ' ('+xInfo+')';

  if not xDesktop.Compatible then
    xLB.Canvas.Font.Color := LblGrayedInfo.Font.Color;

  xLB.Canvas.TextRect(ARect, ARect.Left, (ARect.Top+ARect.Bottom-xLB.Canvas.TextHeight('Hg')) div 2, xText);
  xLB.Canvas.Brush.Style := OldBrushStyle;
  xLB.Canvas.TextStyle := OldTextStyle;
  xLB.Canvas.Font.Style := OldFontStyle;
end;

procedure TDesktopForm.DesktopListBoxKeyPress(Sender: TObject; var Key: char);
begin
  if Key = Char(VK_RETURN) then
    SetActiveDesktopActionClick(Sender);
end;

procedure TDesktopForm.DesktopListBoxSelectionChange(Sender: TObject; User: boolean);
var
  HasSel, IsActive, IsDebug: Boolean;
  CurName: String;
  SelDesktop: TCustomDesktopOpt;
begin
  HasSel := DesktopListBox.ItemIndex>=0;
  if HasSel then
  begin
    SelDesktop := DesktopListBox.Items.Objects[DesktopListBox.ItemIndex] as TCustomDesktopOpt;
    RefreshAssociatedDebugList(SelDesktop);
    if (SelDesktop.AssociatedDebugDesktopName<>'') then
    begin
      AssociatedDebugDesktopComboBox.ItemIndex :=
        AssociatedDebugDesktopComboBox.Items.IndexOfObject(EnvironmentOptions.Desktops.Find(SelDesktop.AssociatedDebugDesktopName));
      if AssociatedDebugDesktopComboBox.ItemIndex<0 then
        AssociatedDebugDesktopComboBox.ItemIndex := 0;
    end else
      AssociatedDebugDesktopComboBox.ItemIndex := 0;
    CurName := DesktopListBox.Items[DesktopListBox.ItemIndex];
    IsActive := CurName = EnvironmentOptions.ActiveDesktopName;
    IsDebug := CurName = EnvironmentOptions.DebugDesktopName;
  end
  else begin
    IsActive := False;
    IsDebug := False;
    RefreshAssociatedDebugList(nil);
  end;
  SetActiveDesktopAction.Enabled := HasSel and not IsActive;
  SetDebugDesktopAction.Enabled := HasSel and not IsDebug;
  RenameAction.Enabled := HasSel;
  DeleteAction.Enabled := HasSel and not (IsActive or IsDebug);
  MoveUpAction.Enabled := HasSel and (DesktopListBox.ItemIndex > 0);
  MoveDownAction.Enabled := HasSel and (DesktopListBox.ItemIndex < DesktopListBox.Items.Count-1);
  ExportAction.Enabled := HasSel;
  ExportAllAction.Enabled := DesktopListBox.Items.Count>0;
  ExportBitBtn.Enabled := ExportItem.Enabled or ExportAllItem.Enabled;
end;

procedure TDesktopForm.ExportAllActionClick(Sender: TObject);
var
  xDesktops: array of TCustomDesktopOpt;
  I: Integer;
begin
  SetLength(xDesktops, EnvironmentOptions.Desktops.Count);
  for I := 0 to Length(xDesktops)-1 do
    xDesktops[I] := EnvironmentOptions.Desktops[I];
  ExportDesktops(xDesktops);
end;

procedure TDesktopForm.SaveActionClick(Sender: TObject);
var
  xDesktopName, xOldDesktopName: string;
begin
  if DesktopListBox.ItemIndex >= 0 then
    xDesktopName := DesktopListBox.Items[DesktopListBox.ItemIndex]
  else
    xDesktopName := '';
  xOldDesktopName := xDesktopName;

  if not InputQuery(dlgDesktopName, dlgSaveCurrentDesktopAs, xDesktopName)
  or (xDesktopName = '') // xDesktopName MUST NOT BE EMPTY !!!
  then
    Exit;

  if SaveCurrentDesktop(xDesktopName, xOldDesktopName <> xDesktopName{ask only if manually inserted}) then
  begin
    if xDesktopName = EnvironmentOptions.ActiveDesktopName then
      FActiveDesktopChanged := True;
    RefreshList(xDesktopName);
  end;
end;

procedure TDesktopForm.SetActiveDesktopActionClick(Sender: TObject);
begin
  if (DesktopListBox.ItemIndex = -1) or
     (EnvironmentOptions.ActiveDesktopName = DesktopListBox.Items[DesktopListBox.ItemIndex])
  then
    Exit;

  if not EnvironmentOptions.Desktops[DesktopListBox.ItemIndex].Compatible then
  begin
    MessageDlg(dlgCannotUseDockedUndockedDesktop, mtError, [mbOK], 0);
    Exit;
  end;

  EnvironmentOptions.ActiveDesktopName := DesktopListBox.Items[DesktopListBox.ItemIndex];
  FActiveDesktopChanged := True;
  RefreshList;
end;

procedure TDesktopForm.SetDebugDesktopActionClick(Sender: TObject);
var
  xDesktopName: String;
begin
  if DesktopListBox.ItemIndex = -1 then
    Exit;

  xDesktopName := DesktopListBox.Items[DesktopListBox.ItemIndex];
  ToggleDebugDesktop(xDesktopName, True);
  RefreshList(xDesktopName);
end;

end.

