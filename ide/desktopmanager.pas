unit DesktopManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ButtonPanel,
  LCLType, LazarusIDEStrConsts, LCLProc, EnvironmentOpts,
  IDEWindowIntf, IDEOptionsIntf, IDEOptionDefs, Laz2_XMLCfg, InputHistory,
  MenuIntf, MainBar, Menus, ComCtrls;

type

  { TDesktopForm }

  TDesktopForm = class(TForm)
    ExportBitBtn: TBitBtn;
    ButtonPanel1: TButtonPanel;
    ImportBitBtn: TBitBtn;
    RenameBitBtn: TBitBtn;
    SelectedDesktopLabel: TLabel;
    SetDebugDesktopBitBtn: TBitBtn;
    DesktopListBox: TListBox;
    SaveBitBtn: TBitBtn;
    DeleteBitBtn: TBitBtn;
    procedure DeleteBitBtnClick(Sender: TObject);
    procedure DesktopListBoxDblClick(Sender: TObject);
    procedure DesktopListBoxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; {%H-}State: TOwnerDrawState);
    procedure DesktopListBoxKeyPress(Sender: TObject; var Key: char);
    procedure DesktopListBoxSelectionChange(Sender: TObject; {%H-}User: boolean);
    procedure ExportBitBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ImportBitBtnClick(Sender: TObject);
    procedure RenameBitBtnClick(Sender: TObject);
    procedure SaveBitBtnClick(Sender: TObject);
    procedure SetDebugDesktopBitBtnClick(Sender: TObject);
  private
    procedure RefreshList(const SelectName: string = '');
  end;

  TShowDesktopItem = class(TMenuItem)
  public
    DesktopName: string;
  end;

  TShowDesktopsToolButton = class(TIDEToolButton)
  private
    procedure ChangeDesktop(Sender: TObject);
  public
    procedure DoOnAdded; override;
    procedure RefreshMenu;
  end;

function ShowDesktopManagerDlg: TModalResult;

implementation

{$R *.lfm}

function ShowDesktopManagerDlg: TModalResult;
var
  theForm: TDesktopForm;
  xDesktopName: String;
  xDesktop: TDesktopOpt;
  I: Integer;
  xButtons: TIDEMenuCommandButtons;
begin
  xDesktopName := '';
  theForm := TDesktopForm.Create(Nil);
  try
    Result := theForm.ShowModal;
    if (Result = mrOK) and (theForm.DesktopListBox.ItemIndex >= 0) then
      xDesktopName := theForm.DesktopListBox.Items[theForm.DesktopListBox.ItemIndex];
  finally
    theForm.Free;
  end;

  xButtons := MainIDEBar.itmToolManageDesktops.ToolButtons;
  for I := 0 to xButtons.Count-1 do
  if xButtons[I] is TShowDesktopsToolButton then
    TShowDesktopsToolButton(xButtons[I]).RefreshMenu;

  if xDesktopName <> '' then
    with EnvironmentOptions do
    begin
      xDesktop := Desktops.Find(xDesktopName);
      if xDesktop <> nil then
        EnvironmentOptions.UseDesktop(xDesktop);
    end;
end;

procedure TShowDesktopsToolButton.ChangeDesktop(Sender: TObject);
var
  xDesktopName: string;
  xDesktop: TDesktopOpt;
begin
  xDesktopName := (Sender as TShowDesktopItem).DesktopName;
  if xDesktopName <> '' then
  begin
    xDesktop := EnvironmentOptions.Desktops.Find(xDesktopName);
    if xDesktop <> nil then
      EnvironmentOptions.UseDesktop(xDesktop);
  end;
end;

procedure TShowDesktopsToolButton.DoOnAdded;
begin
  inherited DoOnAdded;
  RefreshMenu;
end;

procedure TShowDesktopsToolButton.RefreshMenu;
var
  xItem: TShowDesktopItem;
  xPM: TPopupMenu;
  i: Integer;
  xDesktop: TDesktopOpt;
begin
  xPM := DropdownMenu;
  if xPM = nil then
  begin
    xPM := TPopupMenu.Create(Self);
    DropdownMenu := xPM;
    Style := tbsDropDown;
  end;

  xPM.Items.Clear;

  // Saved desktops
  for i:=0 to EnvironmentOptions.Desktops.Count-1 do
  begin
    xDesktop := EnvironmentOptions.Desktops[i];
    xItem := TShowDesktopItem.Create(xPM);
    xPM.Items.Add(xItem);
    xItem.Caption := xDesktop.Name;
    xItem.OnClick := @ChangeDesktop;
    xItem.DesktopName := xDesktop.Name;
  end;
end;

{ TDesktopForm }

procedure TDesktopForm.FormCreate(Sender: TObject);
begin
  RefreshList;

  // buttons captions & text
  Caption := dlgManageDesktops;
  SelectedDesktopLabel.Caption := dlgSelectedDesktop;
  SaveBitBtn.Caption := dlgSaveCurrentDesktop;
  SaveBitBtn.LoadGlyphFromResourceName(HInstance, 'laz_save');
  DeleteBitBtn.Caption := lisDelete;
  DeleteBitBtn.LoadGlyphFromResourceName(HInstance, 'laz_delete');
  RenameBitBtn.Caption := lisRename;
  RenameBitBtn.LoadGlyphFromResourceName(HInstance, 'laz_edit');
  ExportBitBtn.Caption := lisExport;
  ImportBitBtn.Caption := lisImport;
  SetDebugDesktopBitBtn.Caption := dlgToggleSelectedDebugDesktop;
  SetDebugDesktopBitBtn.LoadGlyphFromResourceName(HInstance, 'menu_run');
  ButtonPanel1.OKButton.Caption := dlgCloseAndUseSelectedDesktop;
  ButtonPanel1.CancelButton.Caption := lisClose;
end;

procedure TDesktopForm.RefreshList(const SelectName: string);
var
  i: Integer;
begin
  DesktopListBox.Clear;
  // Saved desktops
  with EnvironmentOptions do
    for i:=0 to Desktops.Count-1 do
      DesktopListBox.Items.Add(Desktops[i].Name);

  i := DesktopListBox.Items.IndexOf(SelectName);
  if (i < 0) and (DesktopListBox.Count > 0) then
    i := 0;
  DesktopListBox.ItemIndex := i;

  DesktopListBoxSelectionChange(DesktopListBox, False);
end;

procedure TDesktopForm.RenameBitBtnClick(Sender: TObject);
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
    Desktops[dskIndex].Name := xDesktopName;
    RefreshList(xDesktopName);
  end;
end;

procedure TDesktopForm.DeleteBitBtnClick(Sender: TObject);
var
  xDesktopName: String;
  dskIndex: Integer;
begin
  if DesktopListBox.ItemIndex = -1 then
    Exit;

  xDesktopName := DesktopListBox.Items[DesktopListBox.ItemIndex];
  if MessageDlg(Format(dlgReallyDeleteDesktop, [xDesktopName]), mtConfirmation, mbYesNo, 0) <> mrYes then
    Exit;

  with EnvironmentOptions do
  begin
    dskIndex := Desktops.IndexOf(xDesktopName);
    if dskIndex >= 0 then
    begin
      debugln(['TDesktopForm.SaveBitBtnClick: Deleting ', xDesktopName]);
      Desktops.Delete(dskIndex);
      if DesktopListBox.ItemIndex+1 < DesktopListBox.Count then
        xDesktopName := DesktopListBox.Items[DesktopListBox.ItemIndex+1]
      else if DesktopListBox.ItemIndex > 0 then
        xDesktopName := DesktopListBox.Items[DesktopListBox.ItemIndex-1]
      else
        xDesktopName := '';
      RefreshList(xDesktopName);
    end;
  end;
end;


procedure TDesktopForm.ExportBitBtnClick(Sender: TObject);
var
  FXMLCfg: TRttiXMLConfig;
  FConfigStore: TXMLOptionsStorage;
  SaveDialog: TSaveDialog;
  xDesktopName, Filename: string;
  xDesktopID: Integer;
begin
  if DesktopListBox.ItemIndex < 0 then
    Exit;

  xDesktopName := DesktopListBox.Items[DesktopListBox.ItemIndex];
  if xDesktopName = '' then
    Exit;

  xDesktopID := EnvironmentOptions.Desktops.IndexOf(xDesktopName);
  if xDesktopID = -1 then
    Exit;

  SaveDialog := TSaveDialog.Create(nil);
  try
    try
      InputHistories.ApplyFileDialogSettings(SaveDialog);
      SaveDialog.Filter := dlgFilterXML +' (*.xml)|*.xml';
      SaveDialog.Options := SaveDialog.Options + [ofOverwritePrompt];
      if SaveDialog.Execute then
      begin
        Filename := SaveDialog.Filename;
        if ExtractFileExt(Filename) = '' then
          Filename := Filename + '.xml';
        FXMLCfg := TRttiXMLConfig.CreateClean(Filename);
        try
          FConfigStore := TXMLOptionsStorage.Create(FXMLCfg);
          try
            EnvironmentOptions.Desktops.SaveToXML(FXMLCfg, FConfigStore, xDesktopID);
            FConfigStore.WriteToDisk;
            ShowMessageFmt(lisSuccessfullyExported, [SaveDialog.Filename]);
          finally
            FConfigStore.Free;
          end;
        finally
          FreeAndNil(FXMLCfg);
        end;
      end;
      InputHistories.StoreFileDialogSettings(SaveDialog);
    except
      on E: Exception do
      begin
        DebugLn('ERROR: [TDesktopMangerDialog.ExportBitBtnClick] ', E.Message);
      end;
    end;
  finally
    SaveDialog.Free;
  end;
end;

procedure TDesktopForm.ImportBitBtnClick(Sender: TObject);
var
  FXMLCfg: TRttiXMLConfig;
  FConfigStore: TXMLOptionsStorage;
  OpenDialog: TOpenDialog;
  xDesktopName, Filename: string;
  xDesktopID: Integer;
begin
  if DesktopListBox.ItemIndex < 0 then
    Exit;

  xDesktopName := DesktopListBox.Items[DesktopListBox.ItemIndex];
  if xDesktopName = '' then
    Exit;

  xDesktopID := EnvironmentOptions.Desktops.IndexOf(xDesktopName);
  if xDesktopID = -1 then
    Exit;

  OpenDialog := TOpenDialog.Create(nil);
  try
    try
      InputHistories.ApplyFileDialogSettings(OpenDialog);
      OpenDialog.Filter := dlgFilterXML +' (*.xml)|*.xml';
      if OpenDialog.Execute then
      begin
        Filename := OpenDialog.Filename;
        FXMLCfg := TRttiXMLConfig.Create(Filename);
        try
          FConfigStore := TXMLOptionsStorage.Create(FXMLCfg);
          try
            EnvironmentOptions.Desktops.LoadFromXML(FXMLCfg, FConfigStore, xDesktopID);
            ShowMessageFmt(lisSuccessfullyImported, [OpenDialog.Filename]);
          finally
            FConfigStore.Free;
          end;
        finally
          FreeAndNil(FXMLCfg);
        end;
      end;
      InputHistories.StoreFileDialogSettings(OpenDialog);
    except
      on E: Exception do
      begin
        DebugLn('ERROR: [TDesktopMangerDialog.ImportBitBtnClick] ', E.Message);
      end;
    end;
  finally
    OpenDialog.Free;
  end;
end;

procedure TDesktopForm.DesktopListBoxDblClick(Sender: TObject);
begin
  if ButtonPanel1.OKButton.Enabled then
    ButtonPanel1.OKButton.Click;
end;

procedure TDesktopForm.DesktopListBoxDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  xLB: TListBox;
  xName: string;

  OldBrushStyle: TBrushStyle;
  OldTextStyle: TTextStyle;
  NewTextStyle: TTextStyle;
  OldFontStyle: TFontStyles;
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

  xName := xLB.Items[Index];
  if (xName <> '') and (EnvironmentOptions.DebugDesktopName = xName) then
  begin
    xName := xName + ' ('+dlgDebugDesktop+')';
    xLB.Canvas.Font.Style := xLB.Canvas.Font.Style + [fsItalic];
  end;

  xLB.Canvas.TextRect(ARect, ARect.Left, ARect.Top, xName);
  xLB.Canvas.Brush.Style := OldBrushStyle;
  xLB.Canvas.TextStyle := OldTextStyle;
  xLB.Canvas.Font.Style := OldFontStyle;
end;

procedure TDesktopForm.DesktopListBoxKeyPress(Sender: TObject; var Key: char);
begin
  if Key = Char(VK_RETURN) then
    DesktopListBoxDblClick(Sender);
end;

procedure TDesktopForm.DesktopListBoxSelectionChange(Sender: TObject;
  User: boolean);
begin
  DeleteBitBtn.Enabled := DesktopListBox.ItemIndex>=0;
  RenameBitBtn.Enabled := DeleteBitBtn.Enabled;
  SetDebugDesktopBitBtn.Enabled := DeleteBitBtn.Enabled;
  ButtonPanel1.OKButton.Enabled := DeleteBitBtn.Enabled;
  ExportBitBtn.Enabled := DeleteBitBtn.Enabled;
  ImportBitBtn.Enabled := DeleteBitBtn.Enabled;
end;

procedure TDesktopForm.SaveBitBtnClick(Sender: TObject);
var
  dsk: TDesktopOpt;
  xDesktopName, xOldDesktopName: string;
begin
  if DesktopListBox.ItemIndex >= 0 then
    xDesktopName := DesktopListBox.Items[DesktopListBox.ItemIndex]
  else
    xDesktopName := '';
  xOldDesktopName := xDesktopName;

  if not InputQuery(dlgSaveCurrentDesktop, dlgDesktopName, xDesktopName)
  or (xDesktopName = '') // xDesktopName MUST NOT BE EMPTY !!!
  then
    Exit;

  with EnvironmentOptions do
  begin
    dsk := Desktops.Find(xDesktopName);
    if Assigned(dsk) and
       (xOldDesktopName <> xDesktopName) and//ask only if manually inserted
       (MessageDlg(Format(dlgOverwriteDesktop, [xDesktopName]), mtWarning, mbYesNo, 0) <> mrYes)
    then
      Exit;

    if not Assigned(dsk) then
    begin
      debugln(['TDesktopForm.SaveBitBtnClick: Adding ', xDesktopName]);
      dsk := TDesktopOpt.Create(xDesktopName, False);
      Desktops.Add(dsk);
    end;
    debugln(['TDesktopForm.SaveBitBtnClick: Assign from ', Desktop.Name, ' to ', dsk.Name]);
    Desktop.IDEWindowCreatorsLayoutList.StoreWindowPositions;
    dsk.Assign(Desktop);
    RefreshList(xDesktopName);
  end;
end;

procedure TDesktopForm.SetDebugDesktopBitBtnClick(Sender: TObject);
var
  xDesktopName: String;
begin
  if DesktopListBox.ItemIndex = -1 then
    Exit;

  xDesktopName := DesktopListBox.Items[DesktopListBox.ItemIndex];
  if EnvironmentOptions.DebugDesktopName = xDesktopName then
    EnvironmentOptions.DebugDesktopName := ''
  else
    EnvironmentOptions.DebugDesktopName := xDesktopName;

  RefreshList(xDesktopName);
end;

end.

