unit uFrameSearch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynEditSearch, SynEditTypes, Forms, Controls, StdCtrls,
  Buttons, Menus, ActnList, ExtCtrls, LCLType;

type

  TOnAfterSearch = procedure (Sender:TObject; cnt: Integer) of object;

  { TFrame1 }

  TFrame1 = class(TFrame)
    actFindNext        :TAction;
    actFindPrevious    :TAction;
    actCaseSensitive   :TAction;
    actSelectOnly: TAction;
    actWholeScope: TAction;
    actReplace         :TAction;
    actReplaceAll      :TAction;
    actPromptOnReplace :TAction;
    actWholeWords      :TAction;
    ActionList1        :TActionList;
    cbReplace          :TCheckBox;
    cbWholeWords       :TCheckBox;
    cbWholeScope: TCheckBox;
    cbSelectionOnly: TCheckBox;
    CheckBox2          :TCheckBox;
    CheckBox3          :TCheckBox;
    CheckBox4          :TCheckBox;
    EditInfoCallParams: TEdit;
    EditSearch         :TEdit;
    EditReplace        :TEdit;
    LabelInfoCallParams: TLabel;
    mniReplaceAll      :TMenuItem;
    mniWholeWords      :TMenuItem;
    mniCaseSensitive   :TMenuItem;
    mniPromptOnReplace :TMenuItem;
    SpeedButtonSearchFwd       :TSpeedButton;
    SpeedButtonSearchBack       :TSpeedButton;
    SpeedButtonClose   :TSpeedButton;
    procedure actFindNextExecute(Sender :TObject);
    procedure actFindPreviousExecute(Sender :TObject);
    procedure ActionList1Update(AAction: TBasicAction; var Handled: Boolean);
    procedure ActionUpdateOptions(Sender:TObject);
    procedure ActionUpdateReplaceOptions(Sender :TObject);
    procedure ActUpdateFindButtons(Sender :TObject);
    procedure cbReplaceChange(Sender :TObject);
    procedure cbWholeScopeChange(Sender: TObject);
    procedure cbSelectionOnlyChange(Sender: TObject);
    procedure EditKeyDown(Sender :TObject; var Key :Word; Shift :TShiftState);
    procedure SpeedButtonCloseClick(Sender :TObject);
  private
    FBackwards     :Boolean;
    FOnCloseFrame  :TNotifyEvent;
    FOnAfterSearch :TOnAfterSearch;
    FSynedit       :TSynEdit;
    FInCheckBoxChange  :Boolean;
    function GetOptions:TSynSearchOptions;
    procedure DoSearch;
  protected
    procedure DoCloseFrame;
  public
    constructor Create(aOwner :TComponent); override;
    property Editor        :TSynEdit     read FSynedit       write FSynedit;
    property OnAfterSearch :TOnAfterSearch read FOnAfterSearch write FOnAfterSearch;
    property OnCloseFrame  :TNotifyEvent read FOnCloseFrame  write FOnCloseFrame;
  end;

implementation

{$R *.lfm}

procedure TFrame1.SpeedButtonCloseClick(Sender :TObject);
begin
  DoCloseFrame;
end;

procedure TFrame1.ActUpdateFindButtons(Sender :TObject);
begin
  if Sender is TAction then TAction(Sender).Enabled := (EditSearch.Text <> '') and Assigned(FSynedit);
end;

procedure TFrame1.ActionUpdateOptions(Sender :TObject);
begin
  if Sender is TAction then TAction(Sender).Enabled := Assigned(FSynedit);
end;

procedure TFrame1.ActionUpdateReplaceOptions(Sender :TObject);
begin
  if Sender is TAction then TAction(Sender).Enabled := Assigned(FSynedit) and cbReplace.Checked;
end;

function TFrame1.GetOptions :TSynSearchOptions;
begin
  Result := [ssoFindContinue];
  if actWholeScope.Checked then Result := [ssoEntireScope];
  if actSelectOnly.Checked then Result := [ssoSelectedOnly];

  if actCaseSensitive.Checked    then Result := Result+[ssoMatchCase];
  if actWholeWords.Checked       then Result := Result+[ssoWholeWord];
  if FBackwards                  then Result := Result+[ssoBackwards];
  if cbReplace.Checked  then begin
    Result := Result+[ssoReplace];
    if actReplaceAll.Checked       then Result := Result+[ssoReplaceAll];
    if actPromptOnReplace.Checked  then Result := Result+[ssoPrompt];
  end;
end;

procedure TFrame1.DoSearch;
var
  cnt: Integer;
begin
  if cbReplace.Checked then
    cnt := FSynedit.SearchReplace(EditSearch.Text, EditReplace.Text, GetOptions)
  else
    cnt := FSynedit.SearchReplace(EditSearch.Text, '', GetOptions);

  actWholeScope.Checked := False;
  actSelectOnly.Checked := False;

  if Assigned(FOnAfterSearch) then FOnAfterSearch(FSynedit, cnt);
end;

procedure TFrame1.DoCloseFrame;
begin
  if Assigned(FOnCloseFrame) then FOnCloseFrame(Self);
end;

constructor TFrame1.Create(aOwner :TComponent);
begin
  inherited Create(aOwner);
  FBackwards := False;
  FSynedit   := Nil;
end;

procedure TFrame1.cbReplaceChange(Sender :TObject);
begin
  EditReplace.Enabled := cbReplace.Checked;
end;

procedure TFrame1.cbWholeScopeChange(Sender: TObject);
begin
  if FInCheckBoxChange then exit;
  FInCheckBoxChange := True;
  if not actWholeScope.Checked then
    actSelectOnly.Checked := False;
  FInCheckBoxChange := False;
end;

procedure TFrame1.cbSelectionOnlyChange(Sender: TObject);
begin
  if FInCheckBoxChange then exit;
  FInCheckBoxChange := True;
  if not actSelectOnly.Checked then
    actWholeScope.Checked := False;
  FInCheckBoxChange := False;
end;

procedure TFrame1.EditKeyDown(Sender :TObject; var Key :Word;  Shift :TShiftState);
begin
  if Key = VK_RETURN then begin
    Key := 0;
    if ssShift in Shift then
      actFindPreviousExecute(actFindPrevious)
    else
      actFindNextExecute(actFindNext);
  end;

  if Key = VK_ESCAPE then begin
    Key := 0;
    DoCloseFrame;
  end;
end;

procedure TFrame1.actFindNextExecute(Sender :TObject);
begin
  FBackwards := False;
  DoSearch;
end;

procedure TFrame1.actFindPreviousExecute(Sender :TObject);
begin
  FBackwards := True;
  DoSearch;
end;

procedure TFrame1.ActionList1Update(AAction: TBasicAction; var Handled: Boolean
  );
var
  opts: TSynSearchOptions;
  i: TSynSearchOption;
  s, tmp: String;
begin
  opts := GetOptions;
  for i := low(opts) to high(opts) do
    if i in opts then begin
      WriteStr(tmp, i);
      if s <> '' then s := s + ', ';
      s := s + tmp;
    end;
  if cbReplace.Checked then
    EditInfoCallParams.Text := 'Synedit.SearchReplace('''+EditSearch.Text+''', '''+EditReplace.Text+''', ['+s+'])'
  else
    EditInfoCallParams.Text := 'Synedit.SearchReplace('''+EditSearch.Text+''', '''', ['+s+'])';
end;

end.

