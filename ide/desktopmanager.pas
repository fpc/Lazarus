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
    ExportAllBitBtn: TBitBtn;
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
    procedure ExportAllBitBtnClick(Sender: TObject);
    procedure ExportBitBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ImportBitBtnClick(Sender: TObject);
    procedure RenameBitBtnClick(Sender: TObject);
    procedure SaveBitBtnClick(Sender: TObject);
    procedure SetDebugDesktopBitBtnClick(Sender: TObject);
  private
    procedure RefreshList(SelectName: string = '');
    procedure ExportDesktops(const aDesktops: array of TDesktopOpt);
  end;

  TShowDesktopItem = class(TMenuItem)
  public
    DesktopName: string;
  end;

  TShowDesktopsToolButton = class(TIDEToolButton)
  private
    procedure ChangeDesktop(Sender: TObject);
    procedure SaveAsDesktop(Sender: TObject);
  public
    procedure DoOnAdded; override;
    procedure RefreshMenu;
  end;

function ShowDesktopManagerDlg: TModalResult;
function SaveCurrentDesktop(const aDesktopName: string; const aShowOverwriteDialog: Boolean): Boolean;
procedure RefreshDesktopMenus;

implementation

{$R *.lfm}

function ShowDesktopManagerDlg: TModalResult;
var
  theForm: TDesktopForm;
  xDesktopName: String;
  xDesktop: TDesktopOpt;
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

  RefreshDesktopMenus;

  if xDesktopName <> '' then
    with EnvironmentOptions do
    begin
      xDesktop := Desktops.Find(xDesktopName);
      if xDesktop <> nil then
        EnvironmentOptions.UseDesktop(xDesktop);
    end;
end;

function SaveCurrentDesktop(const aDesktopName: string;
  const aShowOverwriteDialog: Boolean): Boolean;
var
  dsk: TDesktopOpt;
begin
  Result := False;
  if aDesktopName = '' then
    Exit;

  with EnvironmentOptions do
  begin
    dsk := Desktops.Find(aDesktopName);
    if Assigned(dsk) and
       aShowOverwriteDialog and
       (MessageDlg(Format(dlgOverwriteDesktop, [aDesktopName]), mtWarning, mbYesNo, 0) <> mrYes)
    then
      Exit;

    if not Assigned(dsk) then
    begin
      debugln(['TDesktopForm.SaveBitBtnClick: Adding ', aDesktopName]);
      dsk := TDesktopOpt.Create(aDesktopName, False);
      Desktops.Add(dsk);
    end;
    debugln(['TDesktopForm.SaveBitBtnClick: Assign from ', Desktop.Name, ' to ', dsk.Name]);
    Desktop.IDEWindowCreatorsLayoutList.StoreWindowPositions;
    dsk.Assign(Desktop);
    Result := True;
  end;
end;

procedure RefreshDesktopMenus;
var
  xButtons: TIDEMenuCommandButtons;
  I: Integer;
begin
  xButtons := MainIDEBar.itmToolManageDesktops.ToolButtons;
  for I := 0 to xButtons.Count-1 do
    if xButtons[I] is TShowDesktopsToolButton then
      TShowDesktopsToolButton(xButtons[I]).RefreshMenu;
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
  xMISaveAs: TMenuItem;
  xMI: TMenuItem;
begin
  xPM := DropdownMenu;
  if xPM = nil then
  begin
    xPM := TPopupMenu.Create(Self);
    DropdownMenu := xPM;
    Style := tbsDropDown;
  end;

  xPM.Items.Clear;

  xMISaveAs := TMenuItem.Create(xPM);
  xMISaveAs.Caption := dlgSaveCurrentDesktopAs;
  // Saved desktops
  for i:=0 to EnvironmentOptions.Desktops.Count-1 do
  begin
    xDesktop := EnvironmentOptions.Desktops[i];
    xItem := TShowDesktopItem.Create(xPM);
    xPM.Items.Add(xItem);
    xItem.Caption := xDesktop.Name;
    xItem.OnClick := @ChangeDesktop;
    xItem.DesktopName := xDesktop.Name;

    xItem := TShowDesktopItem.Create(xPM);
    xMISaveAs.Add(xItem);
    xItem.Caption := xDesktop.Name;
    xItem.OnClick := @SaveAsDesktop;
    xItem.DesktopName := xDesktop.Name;
  end;

  if xPM.Items.Count > 0 then
    xPM.Items.AddSeparator;
  xPM.Items.Add(xMISaveAs);

  if xMISaveAs.Count > 0 then
    xMISaveAs.AddSeparator;
  xMI := TMenuItem.Create(xPM);
  xMISaveAs.Add(xMI);
  xMI.Caption := dlgNewDesktop;
  xMI.OnClick := @SaveAsDesktop;
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

  if SaveCurrentDesktop(xDesktopName, xShowOverwriteDlg) then
    RefreshDesktopMenus;
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
  ExportAllBitBtn.Caption := lisExportAll;
  ImportBitBtn.Caption := lisImport;
  SetDebugDesktopBitBtn.Caption := dlgToggleSelectedDebugDesktop;
  SetDebugDesktopBitBtn.LoadGlyphFromResourceName(HInstance, 'menu_run');
  ButtonPanel1.OKButton.Caption := dlgCloseAndUseSelectedDesktop;
  ButtonPanel1.CancelButton.Caption := lisClose;
end;

procedure TDesktopForm.RefreshList(SelectName: string);
var
  i: Integer;
begin
  if (SelectName='') and (DesktopListBox.ItemIndex>=0) then
    SelectName:=DesktopListBox.Items[DesktopListBox.ItemIndex];

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
  xDesktopName: String;
  xDesktop: TDesktopOpt;
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

procedure TDesktopForm.ExportDesktops(const aDesktops: array of TDesktopOpt);
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

procedure TDesktopForm.ImportBitBtnClick(Sender: TObject);
var
  xXMLCfg: TRttiXMLConfig;
  xConfigStore: TXMLOptionsStorage;
  xOpenDialog: TOpenDialog;
  xDesktopName, xOldDesktopName, xFileName: string;
  xCurPath, xDesktopPath: string;
  I: Integer;
  xCount, xImportedCount: Integer;
  xDsk: TDesktopOpt;
begin
  xOpenDialog := TOpenDialog.Create(nil);
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
            //show a dialog to modify desktop name
            if (EnvironmentOptions.Desktops.IndexOf(xDesktopName) >= 0) and
               not InputQuery(dlgDesktopName, dlgImportDesktopExists, xDesktopName)
            then
              Continue;

            if xDesktopName = '' then
              Continue;
            xDsk := EnvironmentOptions.Desktops.Find(xDesktopName);
            if not Assigned(xDsk) then
            begin
              xDsk := TDesktopOpt.Create(xDesktopName, False);
              EnvironmentOptions.Desktops.Add(xDsk);
            end else
            if (xOldDesktopName <> xDesktopName) and
               (MessageDlg(Format(dlgOverwriteDesktop, [xDesktopName]), mtWarning, mbYesNo, 0) <> mrYes)
            then
              Continue;

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
  ExportAllBitBtn.Enabled := DesktopListBox.Items.Count>0;
end;

procedure TDesktopForm.ExportAllBitBtnClick(Sender: TObject);
var
  xDesktops: array of TDesktopOpt;
  I: Integer;
begin
  SetLength(xDesktops, EnvironmentOptions.Desktops.Count);
  for I := 0 to Length(xDesktops)-1 do
    xDesktops[I] := EnvironmentOptions.Desktops[I];
  ExportDesktops(xDesktops);
end;

procedure TDesktopForm.SaveBitBtnClick(Sender: TObject);
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
    RefreshList(xDesktopName);
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

