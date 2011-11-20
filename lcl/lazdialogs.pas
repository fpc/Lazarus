unit lazdialogs;

{$mode delphi}

interface

uses
  // RTL
  Classes, SysUtils,
  // LCL
  Forms, ShellCtrls, Buttons, StdCtrls, ExtCtrls, FileCtrl, ComCtrls,
  Dialogs, ButtonPanel, lclstrconsts, FileUtil, Controls;

type
  TLazFileDialogKind = (
    ldkOpenDesktop, ldkSaveDesktop, ldkOpenPDA, ldkSavePDA, ldkSelectDirectory);

  { TLazarusFileDialogForm }

  TLazarusFileDialogForm = class(TForm)
  private
    FKind: TLazFileDialogKind;
    procedure SetFilter(AFilter: string);
  public
    // User interface
    ButtonPanel: TButtonPanel;
    ShellTreeView: TShellTreeView;
    ShellListView: TShellListView;
    SaveEdit: TEdit;
    FilterComboBox: TFilterComboBox;
    // input/output
    FileName: string;
    Filter: string;
    InitialDir: string;
    Title: string;
    //
    constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;
    procedure Initialize(AKind: TLazFileDialogKind);
    procedure HandleOkClick(ASender: TObject);
    procedure HandleCancelClick(ASender: TObject);
    procedure HandleCloseQuery(Sender : TObject; var CanClose : boolean);
    procedure HandleEditChange(ASender: TObject);
    procedure HandleSelectItem(Sender: TObject;
     Item: TListItem; Selected: Boolean);
    procedure HandleTreeViewSelectionChanged(ASender: TObject);
  end;

  { TLazOpenDialog }

  TLazOpenDialog = class(TOpenDialog)
  protected
    FForm: TLazarusFileDialogForm;
    class procedure WSRegisterClass; override;
    function DoExecute: boolean; override;
    procedure DoInitialize; virtual;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

  { TLazSaveDialog }

  TLazSaveDialog = class(TLazOpenDialog)
  protected
    procedure DoInitialize; override;
  end;

  { TLazSelectDirectoryDialog }

  TLazSelectDirectoryDialog = class(TLazOpenDialog)
  protected
    procedure DoInitialize; override;
  end;

implementation

{ TLazarusFileDialogForm }

procedure TLazarusFileDialogForm.SetFilter(AFilter: string);
begin
  if AFilter = '' then
    FilterComboBox.Filter := Format(rsAllFiles,[GetAllFilesMask, GetAllFilesMask,''])
  else
    FilterComboBox.Filter := AFilter;
end;

{
  The size of the window is determined only when creating the
  handle, so any reference to TForm.Width and TForm.Height
  here doesnt correspond to the final value.
}
constructor TLazarusFileDialogForm.CreateNew(AOwner: TComponent; Num: Integer = 0);
begin
  inherited CreateNew(AOwner, Num);

  Self.Position := poScreenCenter;
end;

procedure TLazarusFileDialogForm.Initialize(AKind: TLazFileDialogKind);
begin
  FKind := AKind;

  ButtonPanel := TButtonPanel.Create(Self);
  ButtonPanel.Parent := Self;
  ButtonPanel.Left := 0;
  ButtonPanel.Height := 20;
  ButtonPanel.Top := Height - ButtonPanel.Height;
  ButtonPanel.Width := Width;
  ButtonPanel.Align := alBottom;
  ButtonPanel.ShowButtons := [pbOK, pbCancel];
  ButtonPanel.OKButton.OnClick := HandleOkClick;
  ButtonPanel.CancelButton.OnClick := HandleCancelClick;

  if AKind in [ldkOpenDesktop, ldkSaveDesktop, ldkOpenPDA, ldkSavePDA] then
  begin
    // Add the ShellTreeView to the dialog
    ShellTreeView := TShellTreeView.Create(Self);
    ShellTreeView.Parent := Self;
    ShellTreeView.Left := 0;
    ShellTreeView.Top := 0;
    ShellTreeView.Width := Width;
    ShellTreeView.Height := 100;
    ShellTreeView.Align := alTop;

    // Add the ShellListView to the dialog
    ShellListView := TShellListView.Create(Self);
    ShellListView.Parent := Self;
    ShellListView.Left := 0;
    ShellListView.Top := ShellTreeView.Height;
    ShellListView.Width := Width;
    ShellListView.Height := Height - ShellTreeView.Height - ButtonPanel.Height;
    ShellListView.Align := alClient;
    ShellListView.ShellTreeView := ShellTreeView;
    ShellListView.ScrollBars := ssVertical;
    ShellListView.OnSelectItem := HandleSelectItem;

    // TEdit for save dialog
    if AKind in [ldkSaveDesktop, ldkSavePDA] then
    begin
      SaveEdit := TEdit.Create(Self);
      SaveEdit.Parent := Self;
      SaveEdit.Left := 0;
      SaveEdit.Height := 20;
      SaveEdit.Top := Height - ButtonPanel.Height - SaveEdit.Height;
      SaveEdit.Width := Width;
      SaveEdit.Align := alBottom;
      SaveEdit.Text := SysUtils.ExtractFileName(FileName);
      SaveEdit.OnChange := HandleEditChange;
    end;

    // TFilterComboBox
    FilterComboBox := TFilterComboBox.Create(Self);
    FilterComboBox.Parent := Self;
    FilterComboBox.Left := 0;
    FilterComboBox.Height := 20;
    FilterComboBox.Top := Height - ButtonPanel.Height - FilterComboBox.Height;
    if SaveEdit <> nil then
      FilterComboBox.Top := FilterComboBox.Top - SaveEdit.Height;
    FilterComboBox.Width := Width;
    FilterComboBox.Align := alBottom;
    SetFilter(Filter);
    FilterComboBox.ShellListView := ShellListView;

    // In the save dialog it is enabled when there is a text in the TEdit
    if AKind in [ldkSaveDesktop, ldkSavePDA] then
      ButtonPanel.OKButton.Enabled := SaveEdit.Text <> ''
    // In a TOpenDialog the Ok button is only enabled when a file is selected
    else
      ButtonPanel.OkButton.Enabled := False;
  end
  else if FKind = ldkSelectDirectory then
  begin
    // Add the ShellTreeView to the dialog
    ShellTreeView := TShellTreeView.Create(Self);
    ShellTreeView.Parent := Self;
    ShellTreeView.Left := 0;
    ShellTreeView.Top := 0;
    ShellTreeView.Align := alClient;
    ShellTreeView.OnSelectionChanged := HandleTreeViewSelectionChanged;

    ButtonPanel.OKButton.Enabled := False;
  end;

  // Form events
  OnCloseQuery := HandleCloseQuery;
end;

// The Ok button code should be only a simple mrOk,
// because there is the dialog Ok button, which will
// always be active and will set the ModalResult to mrOk
// so the code needs to affect it too, and this can be
// done in CloseQuery
procedure TLazarusFileDialogForm.HandleOkClick(ASender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TLazarusFileDialogForm.HandleCancelClick(ASender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TLazarusFileDialogForm.HandleCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  if ModalResult = mrCancel then
  begin
    CanClose := True;
    Exit;
  end;

  CanClose := False;

  if FKind in [ldkSaveDesktop, ldkSavePDA] then
  begin
    if SaveEdit.Text = '' then Exit;

    FileName := ShellTreeView.GetPathFromNode(ShellTreeView.Selected);
    FileName := IncludeTrailingPathDelimiter(FileName);
    FileName := FileName + SaveEdit.Text;
    CanClose := True;
  end
  else if FKind in [ldkOpenDesktop, ldkOpenPDA] then
  begin
    if ShellListView.Selected = nil then Exit;

    FileName := ShellListView.GetPathFromItem(ShellListView.Selected);
    CanClose := True;
  end
  else
  begin
    if ShellTreeView.Selected = nil then Exit;

    FileName := ShellTreeView.GetPathFromNode(ShellTreeView.Selected);
    CanClose := True;
  end;
end;

procedure TLazarusFileDialogForm.HandleEditChange(ASender: TObject);
begin
  ButtonPanel.OkButton.Enabled := SaveEdit.Text <> '';
end;

procedure TLazarusFileDialogForm.HandleSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  // Selecting an item changes the filename in the TEdit
  // in save dialogs
  if (FKind in [ldkSaveDesktop, ldkSavePDA]) and Selected then
  begin
    SaveEdit.Text := Item.Caption;
  end
  // In the OpenDialog the state of the Ok button is dependent
  // on the selection of an item
  else
  begin
    ButtonPanel.OkButton.Enabled := Selected;
  end;
end;

// Used only in the TLazSelectDirectoryDialog
procedure TLazarusFileDialogForm.HandleTreeViewSelectionChanged(ASender: TObject);
begin
  ButtonPanel.OKButton.Enabled := True;
end;

{ TLazOpenDialog }

class procedure TLazOpenDialog.WSRegisterClass;
begin
  // Do nothing, because this dialog doesn't require a WS implementation
end;

function TLazOpenDialog.DoExecute: boolean;
begin
  Result := FForm.ShowModal <> mrCancel;
  FileName := FForm.FileName;
end;

procedure TLazOpenDialog.DoInitialize;
begin
  FForm.Initialize(ldkOpenDesktop);
end;

constructor TLazOpenDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FForm := TLazarusFileDialogForm.CreateNew(Self);
  FForm.FileName := FileName;
  FForm.Filter := Filter;
  FForm.Title := Title;
  DoInitialize;
  FForm.Hide;
end;

{ TLazSaveDialog }

procedure TLazSaveDialog.DoInitialize;
begin
  FForm.Initialize(ldkSaveDesktop);
end;

{ TLazSelectDirectoryDialog }

procedure TLazSelectDirectoryDialog.DoInitialize;
begin
  FForm.Initialize(ldkSelectDirectory);
end;

end.

