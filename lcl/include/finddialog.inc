{%MainUnit ../dialogs.pp}
{******************************************************************************
                                  TFindDialog
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}


type
  { TFindDialogForm }

  TFindDialogForm = class(TForm)
    EntireScopeCheckBox: TCheckBox;
    FindButton: TButton;
    CancelButton: TButton;
    HelpButton: TButton;
    WholeWordsOnlyCheckBox: TCheckBox;
    CaseSensitiveCheckBox: TCheckBox;
    EditFind: TEdit;
    FindLabel: TLabel;
    DirectionRadioGroup: TRadioGroup;
    procedure EditFindChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

procedure TFindDialogForm.EditFindChange(Sender: TObject);
begin
  FindButton.Enabled:=EditFind.Text<>'';
end;

procedure TFindDialogForm.FormCreate(Sender: TObject);
begin
  EditFindChange(nil);
end;

{ TFindDialog }

function TFindDialog.GetReplaceText: string;
begin
  Result:=FReplaceText;
end;

procedure TFindDialog.SetReplaceText(const AValue: string);
begin
  if FReplaceText = AValue then exit;
  FReplaceText:=AValue;
end;

procedure TFindDialog.FindClick(Sender: TObject);
begin
  GetFormValues;
  FOptions := FOptions - [frReplace, frReplaceAll] + [frFindNext];
  Find;
end;

procedure TFindDialog.HelpClick(Sender: TObject);
begin
  Help;
end;

procedure TFindDialog.CancelClick(Sender: TObject);
begin
  CloseDialog;
end;

procedure TFindDialog.UpdatePosition;
begin
  if Assigned(FFindForm) then
  begin
    FFindForm.Top:=FFormTop;
    FFindForm.Left:=FFormLeft;
  end;
end;

procedure TFindDialog.DoCloseForm(Sender: TObject; var CloseAction: TCloseAction);
begin
  if csDesigning in ComponentState then exit;
  if Assigned(OnClose) then OnClose(Self);
end;

function TFindDialog.GetFindText: string;
begin
  Result:=FFindText;
end;

function TFindDialog.GetLeft: Integer;
begin
  Result:=FFormLeft;
end;

function TFindDialog.GetPosition: TPoint;
begin
  Result:=Point(FFormLeft, FFormTop);
end;

function TFindDialog.GetTop: Integer;
begin
  Result:=FFormTop;
end;

procedure TFindDialog.SetFindText(const AValue: string);
begin
  if FFindText = AValue then exit;
  FFindText:=AValue;
end;

procedure TFindDialog.SetLeft(const AValue: Integer);
begin
  if FFormLeft <> AValue then
  begin
    FFormLeft:=AValue;
    UpdatePosition;
  end;
end;

procedure TFindDialog.SetPosition(const AValue: TPoint);
begin
  if (FFormLeft<>AValue.x) or (FFormTop<>AValue.y) then
  begin;
    FFormLeft:=AValue.x;
    FFormTop:=AValue.y;
    UpdatePosition;
  end;
end;

procedure TFindDialog.SetTop(const AValue: Integer);
begin
  if FFormTop <> AValue then
  begin
    FFormTop:=AValue;
    UpdatePosition;
  end;
end;

procedure TFindDialog.Find;
begin
  if Assigned(FOnFind) then
    FOnFind(Self);
end;

procedure TFindDialog.Help;
begin
  if Assigned(FOnHelpClicked) then
    FOnHelpClicked(Self);
end;

procedure TFindDialog.Replace;
begin
  if Assigned(FOnReplace) then
    FOnReplace(Self);
end;

function TFindDialog.CreateForm: TForm;
begin
  Result:=TFindDialogForm.Create(nil);// do not use Self as Owner, otherwise as desgntime this will not work
  //DebugLn(['TFindDialog.CreateForm ',DbgSName(Result),' ',Result.ControlCount]);
  with TFindDialogForm(Result) do begin
    FindButton.Caption:=rsFind;
    CancelButton.Caption:=rsMbCancel;
    HelpButton.Caption:=ifsVK_HELP;
    WholeWordsOnlyCheckBox.Caption:=rsWholeWordsOnly;
    CaseSensitiveCheckBox.Caption:=rsCaseSensitive;
    EntireScopeCheckBox.Caption:=rsEntireScope;
    FindLabel.Caption:=rsText;
    DirectionRadioGroup.Caption:=rsDirection;
    DirectionRadioGroup.Items[0]:=rsForward;
    DirectionRadioGroup.Items[1]:=rsBackward;
    //Setting up button eventhandlers
    FindButton.OnClick := @FindClick;
    HelpButton.OnClick := @HelpClick;
    CancelButton.OnClick := @CancelClick;
    //Once the issues concerning FormStyle = fsStayOnTop are resolved we can implement this
    //so that the form should stay on top of the application (not on top of everything)
    //See bug #8471 in Mantis
    //FormStyle := fsStayOnTop;
  end;
end;

procedure TFindDialog.SetFormValues;
var
  Dlg: TFindDialogForm;
begin
  Dlg:=TFindDialogForm(FFindForm);
  Dlg.EditFind.Text:=FFindText;
  Dlg.EditFind.SelectAll;
  Dlg.ActiveControl:=Dlg.EditFind;

  Dlg.WholeWordsOnlyCheckBox.Checked:=frWholeWord in Options;
  Dlg.CaseSensitiveCheckBox.Checked:=frMatchCase in Options;
  Dlg.EntireScopeCheckBox.Checked:=frEntireScope in Options;
  Dlg.DirectionRadioGroup.ItemIndex:=ord(not(frDown in Options));

  Dlg.WholeWordsOnlyCheckBox.Enabled:=not (frDisableWholeWord in Options);
  Dlg.CaseSensitiveCheckBox.Enabled:=not (frDisableMatchCase in Options);
  Dlg.DirectionRadioGroup.Enabled:=not (frDisableUpDown in Options);

  Dlg.WholeWordsOnlyCheckBox.Visible:=not (frHideWholeWord in Options);
  Dlg.CaseSensitiveCheckBox.Visible:=not (frHideMatchCase in Options);
  Dlg.DirectionRadioGroup.Visible:=not (frHideUpDown in Options);
  Dlg.HelpButton.Visible:=(frShowHelp in Options);
end;

procedure TFindDialog.GetFormValues;
var
  Dlg: TFindDialogForm;
begin
  Dlg:=TFindDialogForm(FFindForm);
  if Dlg.DirectionRadioGroup.ItemIndex = 0 then
    FOptions:=FOptions + [frDown]
  else
    FOptions:=FOptions - [frDown];
  if Dlg.WholeWordsOnlyCheckBox.Checked then
    FOptions:=FOptions + [frWholeWord]
  else
    FOptions:=FOptions - [frWholeWord];
  if Dlg.CaseSensitiveCheckBox.Checked then
    FOptions:=FOptions + [frMatchCase]
  else
    FOptions:=FOptions - [frMatchCase];
  if Dlg.EntireScopeCheckBox.Checked then
    FOptions:=FOptions + [frEntireScope]
  else
    FOptions:=FOptions - [frEntireScope];
  FFindText := Dlg.EditFind.Text;
end;

constructor TFindDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptions:=[frDown];
end;

destructor TFindDialog.Destroy;
begin
  FreeAndNil(FFindForm);
  inherited Destroy;
end;

procedure TFindDialog.CloseDialog;
begin
  if Assigned(FFindForm) then
    FFindForm.Close;
end;

function TFindDialog.Execute: Boolean;
begin
  Result := False;
  if not Assigned(FFindForm) then
    FFindForm:=CreateForm;
    
  if Assigned(FFindForm) then begin
    SetFormValues;
    FFindForm.OnClose:=@DoCloseForm;
    FFindForm.OnShow := Self.OnShow;
    FFindForm.HelpContext:=HelpContext;
    FFindForm.Caption:=Title;
    //Once the issues concerning FormStyle = fsStayOnTop are resolved we can
    //make this a non-Modal window using FFindForm.Show;
    FFindForm.ShowModal;
    Result := true;
  end;
end;


