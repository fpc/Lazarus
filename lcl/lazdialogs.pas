unit LazDialogs;

{$mode objfpc}{$H+}

interface

uses
  // RTL
  Classes, SysUtils, math,
  // LCL
  Graphics, Forms, ShellCtrls, Buttons, StdCtrls, ExtCtrls, FileCtrl, ComCtrls,
  Dialogs, ButtonPanel, LCLStrConsts, FileUtil, Controls;

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

  { TLazMessageDialog }
  TLazMessageDialog = class(TForm)
  private
    Image1: TImage;
    Label1: TLabel; // we need a TLabel to be able to resize it properly
    btnList: array [0..11] of TBitBtn;
    NumButtons: Integer;
  public
    constructor CreateNew(TheOwner: TComponent; Num: Integer = 0); override;
  end;

function LazMessageDlg(const aCaption, aMsg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;
function LazMessageDlg(const aMsg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;

implementation

var
  { Declared here for the time being to make it possibly work with LCLCustodrawn}
  LazMessageDialog: TLazMessageDialog;

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
  ButtonPanel.OKButton.OnClick := @HandleOkClick;
  ButtonPanel.CancelButton.OnClick := @HandleCancelClick;

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
    ShellListView.OnSelectItem := @HandleSelectItem;

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
      SaveEdit.OnChange := @HandleEditChange;
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
    ShellTreeView.OnSelectionChanged := @HandleTreeViewSelectionChanged;

    ButtonPanel.OKButton.Enabled := False;
  end;

  // Form events
  OnCloseQuery := @HandleCloseQuery;
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
{ Dialog Functions }

function LazMessageDlg(const aCaption, aMsg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;
Var
  I: Integer;
  textWidth: Integer;
  ButtonPos: Integer;
  RequiredWidth: Integer;
  BlankRight: Integer; // blank space at the right of last button
begin
  {$ifdef LCLCustomdrawn} if not assigned(LazMessageDialog) then {$endif}
  LazMessageDialog:= TLazMessageDialog.CreateNew(Application);
  with LazMessageDialog do begin
    Label1.Caption:= aMsg;
    Label1.Parent:= LazMessageDialog;
    {Select Image (and Caption) from DlgType}
    case DlgType of
      mtWarning: begin
        Caption:= rsMtWarning;
        image1.Picture.LoadFromResourceName(hInstance, 'dialog_warning', TPortableNetworkGraphic);
      end;
      mtError: begin
        Caption:= rsMtError;
        image1.Picture.LoadFromResourceName(hInstance, 'dialog_error', TPortableNetworkGraphic);
      end;
      mtConfirmation: begin
        Caption:= rsMtConfirmation;
        image1.Picture.LoadFromResourceName(hInstance, 'dialog_confirmation', TPortableNetworkGraphic);
      end;
      mtInformation: begin
        Caption:= rsMtInformation;
        image1.Picture.LoadFromResourceName(hInstance, 'dialog_information', TPortableNetworkGraphic);
      end;
      mtCustom: begin
        Caption:= ApplicationName;
        Image1.Width:= 8;
        Image1.Hide;
      end;
    end;
    Image1.Parent := LazMessageDialog;

    if aCaption <> '' then  //A custom dialog caption has been required
      Caption:= aCaption;
    Label1.Left:= Image1.Left + Image1.Width + 8;

    {Select Buttons from Buttons}
    if (Buttons = []) or (Buttons = [mbHelp]) then
      Buttons:= Buttons + [mbOK]; // the dialog must provide a modal result
    NumButtons:= 0;
    { The order of Buttons is the same as in Qt - Totally different from GTK2}
    if mbHelp in Buttons then begin
      btnList[NumButtons] := TBitBtn.Create(LazMessageDialog);
      btnList[NumButtons].Parent := LazMessageDialog;
      btnList[NumButtons].Kind:= bkHelp;
      inc(NumButtons);
    end;
    if mbYes in Buttons then begin
      btnList[NumButtons] := TBitBtn.Create(LazMessageDialog);
      btnList[NumButtons].Parent := LazMessageDialog;
      btnList[NumButtons].Kind:= bkYes;
      inc(NumButtons);
    end;
    if mbYesToAll in Buttons then begin
      btnList[NumButtons] := TBitBtn.Create(LazMessageDialog);
      btnList[NumButtons].Parent := LazMessageDialog;
      btnList[NumButtons].Kind:= bkYesToAll;
      inc(NumButtons);
    end;
    if mbNo in Buttons then begin
      btnList[NumButtons] := TBitBtn.Create(LazMessageDialog);
      btnList[NumButtons].Parent := LazMessageDialog;
      btnList[NumButtons].Kind:= bkNo;
      inc(NumButtons);
    end;
    if mbNoToAll in Buttons then begin
      btnList[NumButtons] := TBitBtn.Create(LazMessageDialog);
      btnList[NumButtons].Parent := LazMessageDialog;
      btnList[NumButtons].Kind:= bkNoToAll;
      inc(NumButtons);
    end;
    if mbAll in Buttons then begin
      btnList[NumButtons] := TBitBtn.Create(LazMessageDialog);
      btnList[NumButtons].Parent := LazMessageDialog;
      btnList[NumButtons].Kind:= bkAll;
      inc(NumButtons);
    end;
    if mbOK in Buttons then begin
      btnList[NumButtons] := TBitBtn.Create(LazMessageDialog);
      btnList[NumButtons].Parent := LazMessageDialog;
      btnList[NumButtons].Kind:= bkOK;
      inc(NumButtons);
    end;
    if mbRetry in Buttons then begin
      btnList[NumButtons] := TBitBtn.Create(LazMessageDialog);
      btnList[NumButtons].Parent := LazMessageDialog;
      btnList[NumButtons].Kind:= bkRetry;
      inc(NumButtons);
    end;
    if mbIgnore in Buttons then begin
      btnList[NumButtons] := TBitBtn.Create(LazMessageDialog);
      btnList[NumButtons].Parent := LazMessageDialog;
      btnList[NumButtons].Kind:= bkIgnore;
      inc(NumButtons);
    end;
    if mbCancel in Buttons then begin
      btnList[NumButtons] := TBitBtn.Create(LazMessageDialog);
      btnList[NumButtons].Parent := LazMessageDialog;
      btnList[NumButtons].Kind:= bkCancel;
      inc(NumButtons);
    end;
    if mbAbort in Buttons then begin
      btnList[NumButtons] := TBitBtn.Create(LazMessageDialog);
      btnList[NumButtons].Parent := LazMessageDialog;
      btnList[NumButtons].Kind:= bkAbort;
      inc(NumButtons);
    end;
    if mbClose in Buttons then begin
      btnList[NumButtons] := TBitBtn.Create(LazMessageDialog);
      btnList[NumButtons].Parent := LazMessageDialog;
      btnList[NumButtons].Kind:= bkClose;
      inc(NumButtons);
    end;

    ButtonPos:= Image1.Left;
    for I:= 0 to NumButtons -1 do begin
      btnList[I].Constraints.MinHeight:= 25;
      btnList[I].Constraints.MinWidth:= 75;
      btnList[I].Left:= ButtonPos;
      btnList[I].Top:= Image1.Top + Image1.Height + 10;
      {next line is required because even if Auyosize is true,
       width property is changed only when the button is
       painted. Either we wait (but Application.ProcessMessages is not enough)
       or we force the actual width. We may safely use form canvas, because our
       components inherit the font from the form (ParentFont = true by default)}
      btnList[I].Width:= LazMessageDialog.Canvas.TextExtent(btnList[I].Caption).cx
      + btnList[I].Glyph.Width + 20;

      btnList[I].Visible:= True;
      ButtonPos:= ButtonPos + btnList[I].Width + 8;
    end;

  { See comment above for width property. Static Text apparently doesn't behave
  properly when Text is changed at run time. The width is set as appropriate, but
  the text is written only up to the previous width. Therefore we must use a TLabel}
  textWidth:= LazMessageDialog.Canvas.TextExtent(Label1.Caption).cx;

  Label1.Width:= textWidth;
  textWidth:= label1.Left + label1.Width;
  RequiredWidth:= Max(textWidth,ButtonPos);
  Width := RequiredWidth + 10;
  Height:= btnList[0].Top + btnList[0].Height + 10;
  // now let's move our buttons to the right side of dialog, if appropriate
  BlankRight:= Width - ButtonPos;
  if BlankRight > 10 then
    for I:= 0 to NumButtons-1 do btnList[I].Left:= btnList[I].Left+BlankRight ;
  end;
  result := LazMessageDialog.ShowModal;
  {$ifndef LCLCustomdrawn}LazMessageDialog.Release;{$endif}
end;

function LazMessageDlg(const aMsg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;
begin
  result := LazMessageDlg('',aMsg,DlgType,Buttons,HelpCtx);
end;

{ TLazMessageDialog }

constructor TLazMessageDialog.CreateNew(TheOwner: TComponent; Num: Integer = 0);
begin
  inherited CreateNew(TheOwner);
  FormStyle:= fsStayOnTop;
  Position:= poMainFormCenter;
  Image1 := TImage.Create(Self);
  Image1.Top:= 10;
  Image1.Left:= 10;
  Image1.Width:= 48;
  Image1.Height:= 48;
  Label1 := TLabel.Create(Self);
  Label1.Top:= Image1.Top;
  Label1.Left:= Image1.Left + Image1.Width + 10;
  Label1.Caption:= 'Label1';
  Width:= Image1.Width + Label1.Width + 20;
  Height:= Image1.Height + 20;
end;


end.

