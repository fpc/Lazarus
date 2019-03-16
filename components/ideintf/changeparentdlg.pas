{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: FTurtle

  Abstract:
    Dialog for choosing new parent name.
}

unit ChangeParentDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strutils,
  // LCL
  Forms, Controls, StdCtrls, ButtonPanel,
  // LazControls
  ListFilterEdit,
  // IdeIntf
  ObjInspStrConsts, PropEditUtils, IDEImagesIntf, IDEWindowIntf;

type

  { TChangeParentDlg }

  TChangeParentDlg = class(TForm)
    ButtonPanel: TButtonPanel;
    chShowClasses: TCheckBox;
    lblSelectedControls: TLabel;
    lblCurentParents: TLabel;
    ListBox: TListBox;
    ListFilterEdit: TListFilterEdit;
    procedure chShowClassesClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure ListBoxDblClick(Sender: TObject);
    procedure ListBoxSelectionChange(Sender: TObject; User: boolean);
    procedure ListFilterEditAfterFilter(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    class var
      FSavedWidth: Integer;
      FSavedHeight: Integer;
      FSavedShowClasses: Boolean;
  private
    FCandidates: TFPList;
    FIgnoredCandidateName: string;
    function GetSelectedItem: string;
    procedure RefreshList;
    procedure SetSelection(ASelection: TPersistentSelectionList);
    procedure UpdateOKButtonState;
  public
    function ShowModal: Integer; override;
  public
    property SelectedItem: string read GetSelectedItem;
    property Selection: TPersistentSelectionList write SetSelection;
    property Candidates: TFPList write FCandidates;
  end;

function ShowChangeParentDlg(ASelection: TPersistentSelectionList;
  ACandidates: TFPList; out ANewParentName: string): Boolean;


implementation

{$R *.lfm}

{ TChangeParentDlg }

const
  colon = ': ';

function ShowChangeParentDlg(ASelection: TPersistentSelectionList;
  ACandidates: TFPList; out ANewParentName: string): Boolean;
begin
  if not Assigned(ASelection) or not Assigned(ACandidates) then
    Exit(False);

  with TChangeParentDlg.Create(nil) do
  try
    Selection := ASelection;
    Candidates := ACandidates;
    Result := (ShowModal = mrOK);
    if Result then
      ANewParentName := SelectedItem;
  finally
    Free;
  end;
end;


procedure TChangeParentDlg.FormCreate(Sender: TObject);
begin
  Constraints.MinHeight := 250;
  Constraints.MinWidth := 175;

  Height := FSavedHeight;  // see "initialization"
  Width := FSavedWidth;
  chShowClasses.Checked := FSavedShowClasses;

  Caption := oisChangeParent;
  chShowClasses.Caption := oisShowClasses;

  IDEDialogLayoutList.ApplyLayout(Self);
end;

{$PUSH}
{$HINTS OFF}
procedure TChangeParentDlg.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FSavedHeight := Height;
  FSavedWidth := Width;
  FSavedShowClasses := chShowClasses.Checked;
  IDEDialogLayoutList.SaveLayout(Self);
end;
{$POP}

procedure TChangeParentDlg.chShowClassesClick(Sender: TObject);
begin
  if Assigned(FCandidates) then
    RefreshList;
end;

procedure TChangeParentDlg.ListBoxDblClick(Sender: TObject);
begin
  ButtonPanel.OKButton.Click;
end;

{$PUSH}
{$HINTS OFF}
procedure TChangeParentDlg.ListBoxSelectionChange(Sender: TObject; User: boolean);
begin
  UpdateOKButtonState;
end;
{$POP}

procedure TChangeParentDlg.ListFilterEditAfterFilter(Sender: TObject);
begin
  UpdateOKButtonState;
end;

procedure TChangeParentDlg.OKButtonClick(Sender: TObject);
begin
  if ListBox.ItemIndex < 0 then
    ModalResult := mrNone;
end;

function TChangeParentDlg.GetSelectedItem: string;
var
  n: Integer;
begin
  if ListBox.ItemIndex < 0 then
    Exit('');
  Result := ListBox.Items[ListBox.ItemIndex];
  n := Pos(colon, Result);
  if n>0 then
    SetLength(Result, n-1);
end;

procedure TChangeParentDlg.RefreshList;
var
  i: Integer;
  OldIndex: Integer;

  function MakeItem(ACandidate: TWinControl): string;
  begin
    if chShowClasses.Checked then
      Result := ACandidate.Name + colon + ACandidate.ClassName
    else
      Result := ACandidate.Name;
  end;

  function IsIgnoredName: Boolean;
  begin
    Result := (TWinControl(FCandidates.Items[i]).Name = FIgnoredCandidateName);
  end;

begin
  OldIndex := ListBox.ItemIndex;
  ListFilterEdit.FilteredListbox := nil;
  ListBox.Items.Clear;
  for i:=0 to FCandidates.Count-1 do
    if not IsIgnoredName then
      ListBox.Items.Add(MakeItem(TWinControl(FCandidates.Items[i])));
  ListBox.ItemIndex := OldIndex;  // if list was filtered it may select other item
  ListFilterEdit.FilteredListbox := ListBox;
  ListFilterEdit.Text := '';
  UpdateOKButtonState;
end;

procedure TChangeParentDlg.SetSelection(ASelection: TPersistentSelectionList);
var
  i, ControlsCount: Integer;
  sControls, sParents: string;
  CurParentNameList: TStringList;

  procedure AddControlName(AControlName: string);
  begin
    Inc(ControlsCount);
    if ControlsCount = 1 then
      sControls := AControlName
    else
      sControls := sControls + ', ' + AControlName;
  end;

  procedure TryAddParentName(AParentName: string);
  begin
    if CurParentNameList.IndexOf(AParentName) < 0 then
      CurParentNameList.Append(AParentName);
  end;

  procedure SetIgnoredCandidateName;
  var
    j: Integer;
  begin
    FIgnoredCandidateName := CurParentNameList[0];
    for j:=1 to CurParentNameList.Count-1 do
      if FIgnoredCandidateName <> CurParentNameList[j] then
      begin
        FIgnoredCandidateName := '';
        Break;
      end;
  end;

begin
  ControlsCount := 0;
  CurParentNameList := TStringList.Create;

  for i:=0 to ASelection.Count-1 do
    if ASelection.Items[i] is TControl then
      begin
        AddControlName(TControl(ASelection.Items[i]).Name);
        TryAddParentName(TControl(ASelection.Items[i]).Parent.Name);
      end;

  sControls := IfThen(ControlsCount > 1, oisSelectedControls, oisSelectedControl) +
    ': ' + sControls;

  if CurParentNameList.Count > 0 then
  begin
    sParents := IfThen(CurParentNameList.Count > 1, oisCurrentParents, oisCurrentParent) +
      ': ' + CurParentNameList[0];
    for i:=1 to CurParentNameList.Count-1 do
      sParents := sParents + ', ' + CurParentNameList[i];
    SetIgnoredCandidateName;
  end;

  lblSelectedControls.Caption := sControls;
  lblCurentParents.Caption := sParents;

  CurParentNameList.Free;
end;

procedure TChangeParentDlg.UpdateOKButtonState;
begin
  ButtonPanel.OKButton.Enabled := (ListBox.ItemIndex > -1);
end;

function TChangeParentDlg.ShowModal: Integer;
begin
  RefreshList;
  Result := inherited ShowModal;
end;

initialization
  TChangeParentDlg.FSavedWidth := 250;
  TChangeParentDlg.FSavedHeight := 390;
  TChangeParentDlg.FSavedShowClasses := False;

end.

