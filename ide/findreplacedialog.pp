{
  Author: Mattias Gaertner

  Abstract:
    Find and replace dialog form.
    Usage:
      Add to program
        "Application.CreateForm(TLazFindReplaceDialog, FindReplaceDlg);"
      Set the FindReplaceDlg.Options poperty
      then do MResult:=FindReplaceDlg.ShowModal
      ShowModal can have three possible results:
        - mrOk for Find/Replace.
        - mrAll for ReplaceAll
        - mrCancel for Cancel

  ToDo:
  
}
unit FindReplaceDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLLinux, Controls, StdCtrls, Forms, Buttons, ExtCtrls,
  LResources, SynEdit;

type
  TLazFindReplaceDialog = class(TFORM)
  private
    fReplaceAllClickedLast:boolean;
    procedure SetOptions(NewOptions:TSynSearchOptions);
    function GetOptions:TSynSearchOptions;
    function GetFindText:AnsiString;
    procedure SetFindText(NewFindText:AnsiString);
    function GetReplaceText:AnsiString;
    procedure SetReplaceText(NewReplaceText:AnsiString);
  published
    TextToFindLabel:TLabel;
    ReplaceWithLabel:TLabel;
    TextToFindEdit:TEdit;
    ReplaceTextEdit:TEdit;
    OptionsGroupBox:TGroupBox;
    CaseSensitiveCheckBox:TCheckBox;
    WholeWordsOnlyCheckBox:TCheckBox;
    RegularExpressionsCheckBox:TCheckBox;
    PromptOnReplaceCheckBox:TCheckBox;
    DirectionRadioGroup:TRadioGroup;
    ScopeRadioGroup:TRadioGroup;
    OriginRadioGroup:TRadioGroup;
    OkButton:TButton;
    ReplaceAllButton:TButton;
    CancelButton:TButton;
    procedure TextToFindEditKeyDown(Sender: TObject; var Key:Word;
       Shift:TShiftState);
    procedure OkButtonClick(Sender:TObject);
    procedure ReplaceAllButtonClick(Sender:TObject);
    procedure CancelButtonClick(Sender:TObject);
    property Options:TSynSearchOptions read GetOptions write SetOptions;
    property FindText:AnsiString read GetFindText write SetFindText;
    property ReplaceText:AnsiString read GetReplaceText write SetReplaceText;
  public
    constructor Create(AOwner:TComponent); override;
  end;

var FindReplaceDlg:TLazFindReplaceDialog;


implementation


{ TLazFindReplaceDialog }

constructor TLazFindReplaceDialog.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  if LazarusResources.Find(ClassName)=nil then begin
    Caption:='';
    Width:=317;
    Height:=285;
    
    TextToFindLabel:=TLabel.Create(Self);
    with TextToFindLabel do begin
      Name:='TextToFindLabel';
      Parent:=Self;
      Left:=8;
      Top:=8;
      Width:=80;
      Height:=15;
      Caption:='Text to Find';
      Show;
    end;

    ReplaceWithLabel:=TLabel.Create(Self);
    with ReplaceWithLabel do begin
      Name:='ReplaceWithLabel';
      Parent:=Self;	
      Left:=8;
      Top:=32;
      Width:=80;
      Height:=15;
      Caption:='Replace With';
      Show;
    end;

    TextToFindEdit:=TEdit.Create(Self);
    with TextToFindEdit do begin
      Name:='TextToFindEdit';
      Parent:=Self;
      Left:=90;
      Top:=4;
      Width:=220;
      Height:=21;
      Text:='';
      OnKeyDown:=@TextToFindEditKeyDown;
      Show;
    end;

    ReplaceTextEdit:=Tedit.Create(Self);
    with ReplaceTextEdit do begin
      Name:='ReplaceTextEdit';
      Parent:=Self;
      Left:=90;
      Top:=28;
      Width:=220;
      Height:=21;
      Text:='';
      OnKeyDown:=@TextToFindeditKeyDown;
      Show;
    end;
    
    OptionsGroupBox:=TGroupBox.Create(Self);
    with OptionsGroupBox do begin
      Name:='OptionsGroupBox';
      Parent:=Self;
      Left:=4;
      Top:=58;
      Width:=150;
      Height:=105;
      Caption:='Options';
      Show;
    end;

    CaseSensitiveCheckBox:=TCheckBox.Create(Self);
    with CaseSensitiveCheckBox do begin
      Name:='CaseSensitiveCheckBox';
      Parent:=OptionsGroupBox;
      Left:=8;
      Top:=6;
      Width:=135;
      Height:=17;
      Caption:='Case Sensitive';
      Show;
    end;

    WholeWordsOnlyCheckBox:=TCheckBox.Create(Self);
    with WholeWordsOnlyCheckBox do begin
      Name:='WholeWordsOnlyCheckBox';
      Parent:=OptionsGroupBox;
      Left:=8;
      Top:=26;
      Width:=135;
      Height:=17;
      Caption:='Whole Words Only';
      Show;
    end;

    RegularExpressionsCheckBox:=TCheckBox.Create(Self);
    with RegularExpressionsCheckBox do begin
      Name:='RegularExpressionsCheckBox';
      Parent:=OptionsGroupBox;
      Left:=8;
      Top:=46;
      Width:=135;
      Height:=17;
      Caption:='Regular Expressions';
      Show;
    end;

    PromptOnReplaceCheckBox:=TCheckBox.Create(Self);
    with PromptOnReplaceCheckBox do begin
      Name:='PromptOnReplaceCheckBox';
      Parent:=OptionsGroupBox;
      Left:=8;
      Top:=66;
      Width:=135;
      Height:=17;
      Caption:='Prompt On Replace';
      Checked:=true;
      Show;
    end;

    OriginRadioGroup:=TRadioGroup.Create(Self);
    with OriginRadioGroup do begin
      Name:='OriginRadioGroup';
      Parent:=Self;
      Left:=161;
      Top:=58;
      Width:=150;
      Height:=105;
      Caption:='Origin';
      with Items do begin
        BeginUpdate;
        Clear;
        Add('From Cursor');
        Add('Entire Scope');
        EndUpdate;
      end;
      ItemIndex:=0;
      Show;
    end;

    ScopeRadioGroup:=TRadioGroup.Create(Self);
    with ScopeRadioGroup do begin
      Name:='ScopeRadioGroup';
      Parent:=Self;
      Left:=4;
      Top:=168;
      Width:=150;
      Height:=65;
      Caption:='Scope';
      with Items do begin
        BeginUpdate;
        Clear;
        Add('Global');
        Add('Selected Text');
        EndUpdate;
      end;
      ItemIndex:=0;
      Show;
    end;

    DirectionRadioGroup:=TRadioGroup.Create(Self);
    with DirectionRadioGroup do begin
      Name:='DirectionRadioGroup';
      Parent:=Self;
      Left:=161;
      Top:=168;
      Width:=150;
      Height:=65;
      Caption:='Direction';
      with Items do begin
        BeginUpdate;
        Clear;
        Add('Up');
        Add('Down');
        EndUpdate;
      end;
      ItemIndex:=1;
      Show;
    end;

    OkButton:=TButton.Create(Self);
    with OkButton do begin
      Name:='OkButton';
      Parent:=Self;
      Left:=33;
      Top:=245;
      Width:=75;
      Height:=25;
      Caption:='Ok';
      OnClick:=@OkButtonClick;
      Show;
    end;

    ReplaceAllButton:=TButton.Create(Self);
    with ReplaceAllButton do begin
      Name:='ReplaceAllButton';
      Parent:=Self;
      Left:=121;
      Top:=245;
      Width:=75;
      Height:=25;
      Caption:='Replace All';
      OnClick:=@ReplaceAllButtonClick;
      Show;
    end;

    CancelButton:=TButton.Create(Self);
    with CancelButton do begin
      Name:='CancelButton';
      Parent:=Self;
      Left:=209;
      Top:=245;
      Width:=75;
      Height:=25;
      Caption:='Cancel';
      OnClick:=@CancelButtonClick;
      Show;
    end;
  end;
  fReplaceAllClickedLast:=false;
  TextToFindedit.SetFocus;
end;

procedure TLazFindReplaceDialog.TextToFindeditKeyDown(
  Sender: TObject; var Key:Word; Shift:TShiftState);
begin
  if (Key=VK_RETURN) then OkButtonClick(Sender);
  if (Key=VK_ESCAPE) then CancelButtonClick(Sender);
  if Key=VK_TAB then begin
    if Sender=TextToFindEdit then
      ReplaceTextEdit.SetFocus;
    if Sender=ReplaceTextEdit then
      TextToFindEdit.SetFocus;
  end;
end;

procedure TLazFindReplaceDialog.OkButtonClick(Sender:TObject);
begin
  fReplaceAllClickedLast:=false;
  TextToFindedit.SetFocus;
  ModalResult:=mrOk;
end;

procedure TLazFindReplaceDialog.ReplaceAllButtonClick(Sender:TObject);
begin
  fReplaceAllClickedLast:=true;
  TextToFindedit.SetFocus;
  ModalResult:=mrAll;
end;

procedure TLazFindReplaceDialog.CancelButtonClick(Sender:TObject);
begin
  TextToFindedit.SetFocus;
  ModalResult:=mrCancel;
end;

procedure TLazFindReplaceDialog.SetOptions(NewOptions:TSynSearchOptions);
begin
  CaseSensitiveCheckBox.Checked:=ssoMatchCase in NewOptions;
  WholeWordsOnlyCheckBox.Checked:=ssoWholeWord in NewOptions;
  RegularExpressionsCheckBox.Checked:=ssoRegExpr in NewOptions;
  PromptOnReplaceCheckBox.Checked:=ssoPrompt in NewOptions;
  if ssoEntireScope in NewOptions
    then OriginRadioGroup.ItemIndex:=1
    else OriginRadioGroup.ItemIndex:=0;
  if ssoSelectedOnly in NewOptions
    then ScopeRadioGroup.ItemIndex:=1
    else ScopeRadioGroup.ItemIndex:=0;
  if ssoBackwards in NewOptions
    then DirectionRadioGroup.ItemIndex:=0
    else DirectionRadioGroup.ItemIndex:=1;
  ReplaceAllButton.Enabled:=ssoReplace in NewOptions;
  ReplaceTextEdit.Enabled:=ReplaceAllButton.Enabled;
  ReplaceWithLabel.Enabled:=ReplaceAllButton.Enabled;
  PromptOnReplaceCheckBox.Enabled:=ReplaceAllButton.Enabled;
  if ssoReplace in NewOptions then begin
    Caption:='Replace';
    OkButton.Caption:='Replace';
  end else begin
    Caption:='Find';
    OkButton.Caption:='Find';
  end;
end;

function TLazFindReplaceDialog.GetOptions:TSynSearchOptions;
begin
  Result:=[];
  if CaseSensitiveCheckBox.Checked then Include(Result,ssoMatchCase);
  if WholeWordsOnlyCheckBox.Checked then Include(Result,ssoWholeWord);
  if RegularExpressionsCheckBox.Checked then Include(Result,ssoRegExpr);
  if PromptOnReplaceCheckBox.Checked then Include(Result,ssoPrompt);
  if OriginRadioGroup.ItemIndex=1 then Include(Result,ssoEntireScope);
  if ScopeRadioGroup.ItemIndex=1 then include(Result,ssoSelectedOnly);
  if DirectionRadioGroup.ItemIndex=0 then include(Result,ssoBackwards);
  if ReplaceAllButton.Enabled then include(Result,ssoReplace);
  if fReplaceAllClickedLast then include(Result,ssoReplaceAll);
end;

function TLazFindReplaceDialog.GetFindText:AnsiString;
begin
  Result:=TextToFindedit.Text;
end;

procedure TLazFindReplaceDialog.SetFindText(NewFindText:AnsiString);
begin
  TextToFindedit.Text:=NewFindText;
end;

function TLazFindReplaceDialog.GetReplaceText:AnsiString;
begin
  Result:=ReplaceTextEdit.Text;
end;

procedure TLazFindReplaceDialog.SetReplaceText(NewReplaceText:AnsiString);
begin
  ReplaceTextEdit.Text:=NewReplaceText;
end;

end.
