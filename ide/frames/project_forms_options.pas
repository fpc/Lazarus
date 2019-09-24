unit project_forms_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf, ProjectIntf, IDEImagesIntf,
  // IDE
  PackageDefs, Project, LazarusIDEStrConsts;

type

  { TProjectFormsOptionsFrame }

  TProjectFormsOptionsFrame = class(TAbstractIDEOptionsEditor)
    FormsAddToAutoCreatedFormsBtn: TSpeedButton;
    FormsAutoCreatedLabel: TLabel;
    FormsAutoCreatedListBox: TListBox;
    FormsAutoCreateNewFormsCheckBox: TCheckBox;
    FormsAvailFormsLabel: TLabel;
    FormsAvailFormsListBox: TListBox;
    FormsMoveAutoCreatedFormsDownBtn: TSpeedButton;
    FormsMoveAutoCreatedFormUpBtn: TSpeedButton;
    FormsRemoveFromAutoCreatedFormsBtn: TSpeedButton;
    lblMiddle: TLabel;
    procedure FormsAddToAutoCreatedFormsBtnClick(Sender: TObject);
    procedure FormsMoveAutoCreatedFormsDownBtnClick(Sender: TObject);
    procedure FormsMoveAutoCreatedFormUpBtnClick(Sender: TObject);
    procedure FormsRemoveFromAutoCreatedFormsBtnClick(Sender: TObject);
  private
    function FirstAutoCreateFormSelected: Integer;
    function FirstAvailFormSelected: Integer;
    procedure SelectOnlyThisAutoCreateForm(Index: integer);
  public
    function GetTitle: string; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TProjectFormsOptionsFrame }

procedure TProjectFormsOptionsFrame.SelectOnlyThisAutoCreateForm(Index: integer);
var
  i: integer;
begin
  with FormsAutoCreatedListBox do
    for i := 0 to Items.Count - 1 do
      Selected[i] := (i = Index);
end;

procedure TProjectFormsOptionsFrame.FormsMoveAutoCreatedFormUpBtnClick(
  Sender: TObject);
var
  i: integer;
  h: string;
begin
  i := FirstAutoCreateFormSelected;
  if i < 1 then
    Exit;
  with FormsAutoCreatedListBox do
  begin
    Items.BeginUpdate;
    h := Items[i];
    Items[i] := Items[i - 1];
    Items[i - 1] := h;
    Items.EndUpdate;
  end;
  SelectOnlyThisAutoCreateForm(i - 1);
end;

procedure TProjectFormsOptionsFrame.FormsRemoveFromAutoCreatedFormsBtnClick(
  Sender: TObject);
var
  i, NewPos, cmp: integer;
  OldFormName: string;
begin
  FormsAutoCreatedListBox.Items.BeginUpdate;
  FormsAvailFormsListBox.Items.BeginUpdate;
  i := 0;
  while i < FormsAutoCreatedListBox.Items.Count do
    if FormsAutoCreatedListBox.Selected[i] then
    begin
      OldFormName := FormsAutoCreatedListBox.Items[i];
      FormsAutoCreatedListBox.Items.Delete(i);
      NewPos := 0;
      cmp := 1;
      while (NewPos < FormsAvailFormsListBox.Items.Count) do
      begin
        cmp := CompareText(FormsAvailFormsListBox.Items[NewPos], OldFormName);
        if cmp < 0 then
          Inc(NewPos)
        else
          break;
      end;
      if cmp = 0 then
        continue;
      FormsAvailFormsListBox.Items.Insert(NewPos, OldFormName);
    end
    else
      Inc(i);
  FormsAvailFormsListBox.Items.EndUpdate;
  FormsAutoCreatedListBox.Items.EndUpdate;
end;

function TProjectFormsOptionsFrame.FirstAutoCreateFormSelected: Integer;
begin
  Result := 0;
  while (Result < FormsAutoCreatedListBox.Items.Count) and
    (not FormsAutoCreatedListBox.Selected[Result]) do
    inc(Result);
  if Result = FormsAutoCreatedListBox.Items.Count then
    Result := -1;
end;

function TProjectFormsOptionsFrame.FirstAvailFormSelected: Integer;
begin
  Result := 0;
  while (Result < FormsAvailFormsListBox.Items.Count) and
    (not FormsAvailFormsListBox.Selected[Result]) do
    inc(Result);
  if Result = FormsAvailFormsListBox.Items.Count then
    Result := -1;
end;

procedure TProjectFormsOptionsFrame.FormsMoveAutoCreatedFormsDownBtnClick(
  Sender: TObject);
var
  i: integer;
  h: string;
begin
  i := FirstAutoCreateFormSelected;
  if (i < 0) or (i >= FormsAutoCreatedListBox.Items.Count - 1) then
    exit;
  with FormsAutoCreatedListBox do
  begin
    Items.BeginUpdate;
    h := Items[i];
    Items[i] := Items[i + 1];
    Items[i + 1] := h;
    Items.EndUpdate;
  end;
  SelectOnlyThisAutoCreateForm(i + 1);
end;

procedure TProjectFormsOptionsFrame.FormsAddToAutoCreatedFormsBtnClick(
  Sender: TObject);
var
  i: integer;
  NewFormName: string;
begin
  FormsAutoCreatedListBox.Items.BeginUpdate;
  with FormsAvailFormsListBox do
  begin
    Items.BeginUpdate;
    i := 0;
    while i < Items.Count do
      if Selected[i] then
      begin
        NewFormName := Items[i];
        Items.Delete(i);
        FormsAutoCreatedListBox.Items.Add(NewFormName);
      end
      else
        Inc(i);
    Items.EndUpdate;
  end;
  FormsAutoCreatedListBox.Items.EndUpdate;
end;

function TProjectFormsOptionsFrame.GetTitle: string;
begin
  Result := dlgPOFroms;
end;

procedure TProjectFormsOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  FormsAutoCreatedLabel.Caption := dlgAutoCreateForms;
  FormsAutoCreatedListBox.Hint := dlgAutoCreateFormsHint;
  FormsAvailFormsLabel.Caption := dlgAvailableForms;
  FormsAvailFormsListBox.Hint := dlgAvailableFormsHint;
  FormsAutoCreateNewFormsCheckBox.Caption := dlgAutoCreateNewForms;
  IDEImages.AssignImage(FormsMoveAutoCreatedFormUpBtn, 'arrow_up');
  IDEImages.AssignImage(FormsMoveAutoCreatedFormsDownBtn, 'arrow_down');
  IDEImages.AssignImage(FormsAddToAutoCreatedFormsBtn, 'arrow_left');
  IDEImages.AssignImage(FormsRemoveFromAutoCreatedFormsBtn, 'arrow_right');
end;

procedure TProjectFormsOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  Project: TProject;

  procedure FillAutoCreateFormsListbox;
  var
    sl: TStrings;
  begin
    sl := Project.GetAutoCreatedFormsList;
    FormsAutoCreatedListBox.Items.BeginUpdate;
    FormsAutoCreatedListBox.Items.Clear;
    if sl <> nil then
    begin
      FormsAutoCreatedListBox.Items.Assign(sl);
      sl.Free;
    end;
    FormsAutoCreatedListBox.Items.EndUpdate;
  end;

  function IndexOfAutoCreateForm(FormName: string): integer;
  var
    p: integer;
  begin
    p := Pos(':', FormName);
    if p > 0 then
      FormName := Copy(FormName, 1, p - 1);
    Result := FormsAutoCreatedListBox.Items.Count - 1;
    while (Result >= 0) do
    begin
      p := Pos(':', FormsAutoCreatedListBox.Items[Result]);
      if p < 1 then
        p := Length(FormsAutoCreatedListBox.Items[Result]) + 1;
      if CompareText(copy(FormsAutoCreatedListBox.Items[Result], 1, p - 1),
        FormName) = 0 then
        Exit;
      Dec(Result);
    end;
  end;

  procedure FillAvailFormsListBox;
  var
    sl: TStringList;
    i: integer;
  begin
    FormsAvailFormsListBox.Items.BeginUpdate;
    FormsAvailFormsListBox.Items.Clear;

    if (Project <> nil) then
    begin
      sl := TStringList.Create;
      try
        for i := 0 to Project.UnitCount - 1 do
          if (Project.Units[i].IsPartOfProject) and
            (Project.Units[i].ComponentName <> '') and
            (Project.Units[i].ResourceBaseClass in [pfcbcForm, pfcbcCustomForm,pfcbcDataModule]) and
            (IndexOfAutoCreateForm(Project.Units[i].ComponentName) < 0) then
            sl.Add(Project.Units[i].ComponentName);
        sl.Sort;
        FormsAvailFormsListBox.Items.Assign(sl);
      finally
        sl.Free;
      end;
    end;
    FormsAvailFormsListBox.Items.EndUpdate;
  end;

begin
  Project := (AOptions as TProjectIDEOptions).Project;
  FillAutoCreateFormsListbox;
  FillAvailFormsListBox;
  FormsAutoCreateNewFormsCheckBox.Checked := Project.AutoCreateForms;

  FormsMoveAutoCreatedFormUpBtn.ShowHint := true;
  FormsMoveAutoCreatedFormsDownBtn.ShowHint := true;
  FormsMoveAutoCreatedFormUpBtn.Hint := lisMMMoveSelectedItemUp;
  FormsMoveAutoCreatedFormsDownBtn.Hint := lisMMMoveSelectedItemDown;
end;

procedure TProjectFormsOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  Project: TProject;
begin
  Project := (AOptions as TProjectIDEOptions).Project;
  Project.AutoCreateForms := FormsAutoCreateNewFormsCheckBox.Checked;
  Project.TmpAutoCreatedForms := FormsAutoCreatedListBox.Items;
end;

class function TProjectFormsOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TProjectIDEOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupProject, TProjectFormsOptionsFrame, ProjectOptionsForms);

end.

