unit SelectItemDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel;

type

  { TSelectItemDialog }

  TSelectItemDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    cbSelection: TComboBox;
    edName: TEdit;
    lbDropCaption: TLabel;
    lbNameCaption: TLabel;
    lbInfo: TLabel;
    procedure CancelButtonClick(Sender: TObject);
    procedure cbSelectionChange(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure edNameChange(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    FHasPrompt: Boolean;
    FDropDownPrompt: String;
    FItemIndex: integer;
    FShowName: boolean;
    function GetNameCaption: String;
    function GetNameText: String;
    function GetNameTextHint: String;
    procedure SetDropDownPrompt(AValue: String);
    procedure SetNameCaption(AValue: String);
    procedure SetNameText(AValue: String);
    procedure SetNameTextHint(AValue: String);
    procedure SetShowName(AValue: boolean);

  public
    constructor Create(AnOwner: TComponent; ACaption, AnInfo, ADropCaption: TCaption; AButtons: TPanelButtons = [pbOK, pbCancel]);
    constructor Create(AnOwner: TComponent; ACaption, AnInfo, ADropCaption, ANameCaption: TCaption; AButtons: TPanelButtons = [pbOK, pbCancel]);
    procedure SetItems(AnItems: array of string);
    procedure SetItems(AnItems: TStrings);
    function Execute: integer;

    property ItemIndex: integer read FItemIndex write FItemIndex;
    property DropDownPrompt: String read FDropDownPrompt write SetDropDownPrompt;

    property ShowName: boolean read FShowName write SetShowName;
    property NameCaption: String read GetNameCaption write SetNameCaption;
    property NameText: String read GetNameText write SetNameText;
    property NameTextHint: String read GetNameTextHint write SetNameTextHint;
  end;

function ShowChooseItemDialog(
  ACaption, AnInfo, ADropCaption: TCaption;
  AnItems: array of string; AnInitialIndex: integer = 0;
  AButtons: TPanelButtons = [pbOK, pbCancel]
): integer;

function ShowChooseItemDialog(
  ACaption, AnInfo, ADropCaption: TCaption;
  AnItems: TStrings; AnInitialIndex: integer = 0;
  AButtons: TPanelButtons = [pbOK, pbCancel]
): integer;

function ShowChooseItemDialog(
  ACaption, AnInfo, ADropCaption: TCaption;
  AnItems: array of string; APromptSelectText: string;
  AButtons: TPanelButtons = [pbOK, pbCancel]
): integer;

function ShowChooseItemDialog(
  ACaption, AnInfo, ADropCaption: TCaption;
  AnItems: TStrings; APromptSelectText: string;
  AButtons: TPanelButtons = [pbOK, pbCancel]
): integer;

function ShowChooseItemDialog(
  ACaption, AnInfo, ADropCaption, ANameCaption: TCaption;
  var AName: String;
  AnItems: array of string; AnInitialIndex: integer = 0;
  ANameHintText: TCaption = '';
  AButtons: TPanelButtons = [pbOK, pbCancel]
): integer;

function ShowChooseItemDialog(
  ACaption, AnInfo, ADropCaption, ANameCaption: TCaption;
  var AName: String;
  AnItems: TStrings; AnInitialIndex: integer = 0;
  ANameHintText: TCaption = '';
  AButtons: TPanelButtons = [pbOK, pbCancel]
): integer;

function ShowChooseItemDialog(
  ACaption, AnInfo, ADropCaption, ANameCaption: TCaption;
  var AName: String;
  AnItems: array of string; APromptSelectText: string;
  ANameHintText: TCaption = '';
  AButtons: TPanelButtons = [pbOK, pbCancel]
): integer;

function ShowChooseItemDialog(
  ACaption, AnInfo, ADropCaption, ANameCaption: TCaption;
  var AName: String;
  AnItems: TStrings; APromptSelectText: string;
  ANameHintText: TCaption = '';
  AButtons: TPanelButtons = [pbOK, pbCancel]
): integer;

implementation

function ShowChooseItemDialog(ACaption, AnInfo, ADropCaption: TCaption; AnItems: array of string;
  AnInitialIndex: integer; AButtons: TPanelButtons): integer;
var
  dlg: TSelectItemDialog;
begin
  dlg := TSelectItemDialog.Create(Screen.ActiveForm, ACaption, AnInfo, ADropCaption, AButtons);
  dlg.SetItems(AnItems);
  dlg.ItemIndex := AnInitialIndex;
  Result := dlg.Execute;
  dlg.Free;
end;

function ShowChooseItemDialog(ACaption, AnInfo, ADropCaption: TCaption; AnItems: TStrings;
  AnInitialIndex: integer; AButtons: TPanelButtons): integer;
var
  dlg: TSelectItemDialog;
begin
  dlg := TSelectItemDialog.Create(Screen.ActiveForm, ACaption, AnInfo, ADropCaption, AButtons);
  dlg.SetItems(AnItems);
  dlg.ItemIndex := AnInitialIndex;
  Result := dlg.Execute;
  dlg.Free;
end;

function ShowChooseItemDialog(ACaption, AnInfo, ADropCaption: TCaption; AnItems: array of string;
  APromptSelectText: string; AButtons: TPanelButtons): integer;
var
  dlg: TSelectItemDialog;
begin
  dlg := TSelectItemDialog.Create(Screen.ActiveForm, ACaption, AnInfo, ADropCaption, AButtons);
  dlg.SetItems(AnItems);
  dlg.DropDownPrompt := APromptSelectText;
  Result := dlg.Execute;
  dlg.Free;
end;

function ShowChooseItemDialog(ACaption, AnInfo, ADropCaption: TCaption; AnItems: TStrings;
  APromptSelectText: string; AButtons: TPanelButtons): integer;
var
  dlg: TSelectItemDialog;
begin
  dlg := TSelectItemDialog.Create(Screen.ActiveForm, ACaption, AnInfo, ADropCaption, AButtons);
  dlg.SetItems(AnItems);
  dlg.DropDownPrompt := APromptSelectText;
  Result := dlg.Execute;
  dlg.Free;
end;

function ShowChooseItemDialog(ACaption, AnInfo, ADropCaption, ANameCaption: TCaption;
  var AName: String; AnItems: array of string; AnInitialIndex: integer; ANameHintText: TCaption;
  AButtons: TPanelButtons): integer;
var
  dlg: TSelectItemDialog;
begin
  dlg := TSelectItemDialog.Create(Screen.ActiveForm, ACaption, AnInfo, ADropCaption, ANameCaption, AButtons);
  dlg.SetItems(AnItems);
  dlg.ItemIndex := AnInitialIndex;
  dlg.NameTextHint := ANameHintText;
  dlg.NameText := AName;
  Result := dlg.Execute;
  AName := dlg.NameText;
  dlg.Free;
end;

function ShowChooseItemDialog(ACaption, AnInfo, ADropCaption, ANameCaption: TCaption;
  var AName: String; AnItems: TStrings; AnInitialIndex: integer; ANameHintText: TCaption;
  AButtons: TPanelButtons): integer;
var
  dlg: TSelectItemDialog;
begin
  dlg := TSelectItemDialog.Create(Screen.ActiveForm, ACaption, AnInfo, ADropCaption, ANameCaption, AButtons);
  dlg.SetItems(AnItems);
  dlg.ItemIndex := AnInitialIndex;
  dlg.NameTextHint := ANameHintText;
  dlg.NameText := AName;
  Result := dlg.Execute;
  AName := dlg.NameText;
  dlg.Free;
end;

function ShowChooseItemDialog(ACaption, AnInfo, ADropCaption, ANameCaption: TCaption;
  var AName: String; AnItems: array of string; APromptSelectText: string; ANameHintText: TCaption;
  AButtons: TPanelButtons): integer;
var
  dlg: TSelectItemDialog;
begin
  dlg := TSelectItemDialog.Create(Screen.ActiveForm, ACaption, AnInfo, ADropCaption, ANameCaption, AButtons);
  dlg.SetItems(AnItems);
  dlg.DropDownPrompt := APromptSelectText;
  dlg.NameTextHint := ANameHintText;
  dlg.NameText := AName;
  Result := dlg.Execute;
  AName := dlg.NameText;
  dlg.Free;
end;

function ShowChooseItemDialog(ACaption, AnInfo, ADropCaption, ANameCaption: TCaption;
  var AName: String; AnItems: TStrings; APromptSelectText: string; ANameHintText: TCaption;
  AButtons: TPanelButtons): integer;
var
  dlg: TSelectItemDialog;
begin
  dlg := TSelectItemDialog.Create(Screen.ActiveForm, ACaption, AnInfo, ADropCaption, ANameCaption, AButtons);
  dlg.SetItems(AnItems);
  dlg.DropDownPrompt := APromptSelectText;
  dlg.NameTextHint := ANameHintText;
  dlg.NameText := AName;
  Result := dlg.Execute;
  AName := dlg.NameText;
  dlg.Free;
end;

{$R *.lfm}

{ TSelectItemDialog }

procedure TSelectItemDialog.cbSelectionChange(Sender: TObject);
begin
  ButtonPanel1.OKButton.Enabled :=
    ( (not FHasPrompt) or (cbSelection.ItemIndex > 0) ) and
    ( (not FShowName)  or (edName.Text <> '') );
end;

procedure TSelectItemDialog.CloseButtonClick(Sender: TObject);
begin
  ModalResult := mrClose;
end;

procedure TSelectItemDialog.edNameChange(Sender: TObject);
begin
  ButtonPanel1.OKButton.Enabled :=
    ( (not FHasPrompt) or (cbSelection.ItemIndex > 0) ) and
    ( (not FShowName)  or (edName.Text <> '') );
end;

procedure TSelectItemDialog.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TSelectItemDialog.OKButtonClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TSelectItemDialog.SetDropDownPrompt(AValue: String);
begin
  if FDropDownPrompt = AValue then Exit;
  FDropDownPrompt := AValue;
  FItemIndex := -1;
end;

procedure TSelectItemDialog.SetNameCaption(AValue: String);
begin
  lbNameCaption.Caption := AValue;
end;

function TSelectItemDialog.GetNameText: String;
begin
  Result := edName.Text;
end;

function TSelectItemDialog.GetNameCaption: String;
begin
  Result := lbNameCaption.Caption;
end;

function TSelectItemDialog.GetNameTextHint: String;
begin
  Result := edName.TextHint;
end;

procedure TSelectItemDialog.SetNameText(AValue: String);
begin
  edName.Text := AValue;
end;

procedure TSelectItemDialog.SetNameTextHint(AValue: String);
begin
  edName.TextHint := AValue;
end;

procedure TSelectItemDialog.SetShowName(AValue: boolean);
begin
  if FShowName = AValue then Exit;
  FShowName := AValue;
end;

constructor TSelectItemDialog.Create(AnOwner: TComponent; ACaption, AnInfo, ADropCaption: TCaption;
  AButtons: TPanelButtons);
begin
  inherited Create(AnOwner);
  Caption := ACaption;
  lbInfo.Caption := AnInfo;
  lbInfo.Visible :=  AnInfo <> '';
  lbDropCaption.Caption := ADropCaption;
  lbDropCaption.Visible := ADropCaption <> '';
  ButtonPanel1.ShowButtons := AButtons;
end;

constructor TSelectItemDialog.Create(AnOwner: TComponent; ACaption, AnInfo, ADropCaption,
  ANameCaption: TCaption; AButtons: TPanelButtons);
begin
  Create(AnOwner, ACaption, AnInfo, ADropCaption, AButtons);
  NameCaption := ANameCaption;
  ShowName := True;
end;

procedure TSelectItemDialog.SetItems(AnItems: array of string);
var
  i: Integer;
begin
  cbSelection.Items.Clear;
  for i := 0 to Length(AnItems) - 1 do
    cbSelection.Items.Add(AnItems[i]);
end;

procedure TSelectItemDialog.SetItems(AnItems: TStrings);
begin
  cbSelection.Items.Assign(AnItems);
end;

function TSelectItemDialog.Execute: integer;
begin
  FHasPrompt := FDropDownPrompt <> '';
  if FHasPrompt then begin
    cbSelection.Items.Insert(0, FDropDownPrompt);
    cbSelection.ItemIndex := FItemIndex+1;
  end
  else
    cbSelection.ItemIndex := FItemIndex;

  lbNameCaption.Visible := FShowName;
  edName.Visible := FShowName;

  cbSelectionChange(nil);
  ShowModal;
  Result := cbSelection.ItemIndex;
  if FHasPrompt then
    dec(Result);
  FItemIndex := Result;
end;

end.

