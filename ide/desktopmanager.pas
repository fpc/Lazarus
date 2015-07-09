unit DesktopManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ButtonPanel, LCLType,
  LazarusIDEStrConsts, LCLProc, EnvironmentOpts;

type

  { TDesktopForm }

  TDesktopForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    SetDebugDesktopBitBtn: TBitBtn;
    DesktopListBox: TListBox;
    SaveBitBtn: TBitBtn;
    DeleteBitBtn: TBitBtn;
    procedure DeleteBitBtnClick(Sender: TObject);
    procedure DesktopListBoxDblClick(Sender: TObject);
    procedure DesktopListBoxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure DesktopListBoxKeyPress(Sender: TObject; var Key: char);
    procedure DesktopListBoxSelectionChange(Sender: TObject; {%H-}User: boolean);
    procedure FormCreate(Sender: TObject);
    procedure SaveBitBtnClick(Sender: TObject);
    procedure SetDebugDesktopBitBtnClick(Sender: TObject);
  private
    procedure RefreshList(const SelectName: string = '');
  public

  end;

function ShowDesktopManagerDlg: TModalResult;

//var DesktopForm: TDesktopForm;

implementation

uses
  IDEWindowIntf, IDEOptionsIntf;

{$R *.lfm}

function ShowDesktopManagerDlg: TModalResult;
var
  theForm: TDesktopForm;
  xDesktopName: String;
  xDesktop: TDesktopOpt;
begin
  xDesktopName := '';
  theForm:=TDesktopForm.Create(Nil);
  try
    Result := theForm.ShowModal;
    if (Result = mrOK) and (theForm.DesktopListBox.ItemIndex >= 0) then
      xDesktopName := theForm.DesktopListBox.Items[theForm.DesktopListBox.ItemIndex];
  finally
    theForm.Free;
  end;

  if xDesktopName <> '' then
    with EnvironmentOptions do
    begin
      xDesktop := Desktops.Find(xDesktopName);
      if xDesktop<>nil then
        EnvironmentOptions.UseDesktop(xDesktop);
    end;
end;

{ TDesktopForm }

procedure TDesktopForm.FormCreate(Sender: TObject);
begin
  RefreshList;

  // buttons captions & text
  Caption := dlgManageDesktops;
  SaveBitBtn.Caption := dlgSaveCurrentDesktop;
  SaveBitBtn.LoadGlyphFromResourceName(HInstance, 'laz_save');
  DeleteBitBtn.Caption := dlgDeleteSelectedDesktop;
  DeleteBitBtn.LoadGlyphFromResourceName(HInstance, 'laz_delete');
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

  DesktopListBox.ItemIndex := DesktopListBox.Items.IndexOf(SelectName);
  DesktopListBoxSelectionChange(DesktopListBox, False);
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
  SetDebugDesktopBitBtn.Enabled := DeleteBitBtn.Enabled;
  ButtonPanel1.OKButton.Enabled := DeleteBitBtn.Enabled;
end;

procedure TDesktopForm.SaveBitBtnClick(Sender: TObject);
var
  dsk: TDesktopOpt;
  xDesktopName: string;
begin
  if DesktopListBox.ItemIndex >= 0 then
    xDesktopName := DesktopListBox.Items[DesktopListBox.ItemIndex]
  else
    xDesktopName := '';

  if not InputQuery(dlgSaveCurrentDesktop, dlgDesktopNameWillBeOverwritten, xDesktopName)
  or (xDesktopName = '') // xDesktopName MUST NOT BE EMPTY !!!
  then
    Exit;

  with EnvironmentOptions do
  begin
    dsk := Desktops.Find(xDesktopName);
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

