{  $Id$  }
{
 /***************************************************************************
                          findreplacedialog.pp
                          --------------------

 ***************************************************************************/

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

{$Define DeleteMeWhenComboBoxFocusIsFixed}

interface

uses
  Classes, SysUtils, LCLType, Controls, StdCtrls, Forms, Buttons, ExtCtrls,
  LResources, SynEdit, IDEProcs;

type
  TFindDlgComponent = (fdcText, fdcReplace);
  TOnFindDlgKey = procedure(Sender: TObject; var Key: Word; Shift:TShiftState;
                           FindDlgComponent: TFindDlgComponent) of Object;

  TLazFindReplaceDialog = class(TForm)
  private
    FOnKey: TOnFindDlgKey;
    fReplaceAllClickedLast:boolean;
    function GetComponentText(c: TFindDlgComponent): string;
    procedure SetComponentText(c: TFindDlgComponent; const AValue: string);
    procedure SetOnKey(const AValue: TOnFindDlgKey);
    procedure SetOptions(NewOptions:TSynSearchOptions);
    function GetOptions:TSynSearchOptions;
    function GetFindText:AnsiString;
    procedure SetFindText(NewFindText:AnsiString);
    function GetReplaceText:AnsiString;
    procedure SetReplaceText(NewReplaceText:AnsiString);
    procedure SetComboBoxText(AComboBox:TComboBox;const AText:AnsiString);
  published
    TextToFindLabel:TLabel;
    ReplaceWithLabel:TLabel;
    {$IFDEF DeleteMeWhenComboBoxFocusIsFixed}
    TextToFindComboBox:TEdit;
    ReplaceTextComboBox:TEdit;
    {$ELSE}
    TextToFindComboBox:TComboBox;
    ReplaceTextComboBox:TComboBox;
    {$ENDIF}
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
    procedure TextToFindComboboxKeyDown(Sender: TObject; var Key:Word;
       Shift:TShiftState);
    procedure OkButtonClick(Sender:TObject);
    procedure ReplaceAllButtonClick(Sender:TObject);
    procedure CancelButtonClick(Sender:TObject);
    procedure FormResize(Sender: TObject);
    property Options:TSynSearchOptions read GetOptions write SetOptions;
    property FindText:AnsiString read GetFindText write SetFindText;
    property ReplaceText:AnsiString read GetReplaceText write SetReplaceText;
    property OnKey: TOnFindDlgKey read FOnKey write SetOnKey;
  public
    property ComponentText[c: TFindDlgComponent]: string
      read GetComponentText write SetComponentText;
    constructor Create(TheOwner:TComponent); override;
  end;

var FindReplaceDlg:TLazFindReplaceDialog;


implementation


{ TLazFindReplaceDialog }

constructor TLazFindReplaceDialog.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner);
  if LazarusResources.Find(ClassName)=nil then begin
    Caption:='';
    Width:=400;
    Height:=285;
    Position:=poDesigned;

    {$IFDEF DeleteMeWhenComboBoxFocusIsFixed}
    TextToFindComboBox:=TEdit.Create(Self);
    {$ELSE}
    TextToFindComboBox:=TComboBox.Create(Self);
    {$ENDIF}
    with TextToFindComboBox do begin
      Name:='TextToFindComboBox';
      Parent:=Self;
      Left:=90;
      Top:=4;
      Width:=220;
      Height:=21;
      Text:='';
      OnKeyDown:=@TextToFindComboBoxKeyDown;
      Visible:=true;
    end;

    TextToFindLabel:=TLabel.Create(Self);
    with TextToFindLabel do begin
      Name:='TextToFindLabel';
      Parent:=Self;
      Left:=8;
      Top:=8;
      Width:=80;
      Height:=15;
      Caption:='Text to Find';
      Visible:=true;
    end;

    {$IFDEF DeleteMeWhenComboBoxFocusIsFixed}
    ReplaceTextComboBox:=TEdit.Create(Self);
    {$ELSE}
    ReplaceTextComboBox:=TComboBox.Create(Self);
    {$ENDIF}
    with ReplaceTextComboBox do begin
      Name:='ReplaceTextComboBox';
      Parent:=Self;
      Left:=90;
      Top:=28;
      Width:=220;
      Height:=21;
      Text:='';
      OnKeyDown:=@TextToFindComboBoxKeyDown;
      Visible:=true;
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
      Visible:=true;
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
      Visible:=true;
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
      Visible:=true;
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
      Visible:=true;
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
      Visible:=true;
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
      Visible:=true;
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
      Visible:=true;
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
      Visible:=true;
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
      Visible:=true;
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
      Visible:=true;
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
      Visible:=true;
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
      Visible:=true;
    end;
    
    OnResize:=@FormResize;
  end;
  fReplaceAllClickedLast:=false;
  FormResize(Self);
  TextToFindComboBox.SetFocus;
end;

procedure TLazFindReplaceDialog.TextToFindComboBoxKeyDown(
  Sender: TObject; var Key:Word; Shift:TShiftState);
var Component: TFindDlgComponent;
begin
  if (Key=VK_RETURN) then
    OkButtonClick(Sender)
  else if (Key=VK_ESCAPE) then
    CancelButtonClick(Sender)
  else if Key=VK_TAB then begin
    if (Sender=TextToFindComboBox) and (ReplaceTextComboBox.Enabled) then
      ReplaceTextComboBox.SetFocus;
    if Sender=ReplaceTextComboBox then
      TextToFindComboBox.SetFocus;
  end else if Assigned(OnKey) then begin
    if Sender=TextToFindComboBox then
      Component:=fdcText
    else
      Component:=fdcReplace;
    OnKey(Sender, Key, Shift, Component);
  end;
end;

procedure TLazFindReplaceDialog.OkButtonClick(Sender:TObject);
begin
  fReplaceAllClickedLast:=false;
  TextToFindComboBox.SetFocus;
  ModalResult:=mrOk;
end;

procedure TLazFindReplaceDialog.ReplaceAllButtonClick(Sender:TObject);
begin
  fReplaceAllClickedLast:=true;
  TextToFindComboBox.SetFocus;
  ModalResult:=mrAll;
end;

procedure TLazFindReplaceDialog.CancelButtonClick(Sender:TObject);
begin
  TextToFindComboBox.SetFocus;
  ModalResult:=mrCancel;
end;

procedure TLazFindReplaceDialog.FormResize(Sender: TObject);
var MaxX, TxtLabelWidth, OptionsWidth, ButtonWidth: integer;
begin
  TxtLabelWidth:=100;
  MaxX:=ClientWidth;
  OptionsWidth:=(MaxX-3*4) div 2;
  ButtonWidth:=90;

  with TextToFindLabel do begin
    Left:=8;
    Top:=8;
    Width:=TxtLabelWidth;
  end;

  with TextToFindComboBox do begin
    Left:=TextToFindLabel.Left+TextToFindLabel.Width+2;
    Top:=TextToFindLabel.Top-4;
    Width:=MaxX-Left-4;
  end;

  with ReplaceWithLabel do begin
    Left:=TextToFindLabel.Left;
    Top:=TextToFindLabel.Top+TextToFindLabel.Height+8;
    Width:=TextToFindLabel.Width;
  end;

  with ReplaceTextComboBox do begin
    Left:=TextToFindComboBox.Left;
    Top:=ReplaceWithLabel.Top-4;
    Width:=MaxX-Left-4;
  end;

  with OptionsGroupBox do begin
    Left:=4;
    Top:=ReplaceWithLabel.Top+ReplaceWithLabel.Height+3;
    Width:=OptionsWidth;
    Height:=110;
  end;

  with CaseSensitiveCheckBox do begin
    Left:=8;
    Top:=3;
    Width:=Parent.ClientWidth-Left;
  end;

  with WholeWordsOnlyCheckBox do begin
    Left:=8;
    Top:=CaseSensitiveCheckBox.Top+CaseSensitiveCheckBox.Height+5;
    Width:=Parent.ClientWidth-Left;
  end;

  with RegularExpressionsCheckBox do begin
    Left:=8;
    Top:=WholeWordsOnlyCheckBox.Top+WholeWordsOnlyCheckBox.Height+5;
    Width:=Parent.ClientWidth-Left;
  end;

  with PromptOnReplaceCheckBox do begin
    Left:=8;
    Top:=RegularExpressionsCheckBox.Top+RegularExpressionsCheckBox.Height+5;
    Width:=Parent.ClientWidth-Left;
  end;

  with OriginRadioGroup do begin
    Left:=OptionsGroupBox.Left+OptionsGroupBox.Width+4;
    Top:=OptionsGroupBox.Top;
    Width:=OptionsWidth;
    Height:=OptionsGroupBox.Height;
  end;

  with ScopeRadioGroup do begin
    Left:=OptionsGroupBox.Left;
    Top:=OptionsGroupBox.Top+OptionsGroupBox.Height+5;
    Width:=OptionsWidth;
    Height:=65;
  end;

  with DirectionRadioGroup do begin
    Left:=OriginRadioGroup.Left;
    Top:=OriginRadioGroup.Top+OriginRadioGroup.Height+5;
    Width:=OptionsWidth;
    Height:=65;
  end;

  with OkButton do begin
    Left:=MaxX-10-3*(ButtonWidth+7);
    Top:=245;
    Width:=75;
  end;

  with ReplaceAllButton do begin
    Left:=MaxX-10-2*(ButtonWidth+7);
    Top:=245;
    Width:=75;
  end;

  with CancelButton do begin
    Left:=MaxX-10-1*(ButtonWidth+7);
    Top:=245;
    Width:=75;
  end;
end;

function TLazFindReplaceDialog.GetComponentText(c: TFindDlgComponent): string;
begin
  case c of
  fdcText: Result:=FindText;
  else
    Result:=Replacetext;
  end;
end;

procedure TLazFindReplaceDialog.SetComponentText(c: TFindDlgComponent;
  const AValue: string);
begin
  case c of
  fdcText: FindText:=AValue;
  else
    Replacetext:=AValue;
  end;
end;

procedure TLazFindReplaceDialog.SetOnKey(const AValue: TOnFindDlgKey);
begin
  FOnKey:=AValue;
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
  ReplaceTextComboBox.Enabled:=ReplaceAllButton.Enabled;
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
  Result:=TextToFindComboBox.Text;
end;

procedure TLazFindReplaceDialog.SetFindText(NewFindText:AnsiString);
begin
  {$IFDEF DeleteMeWhenComboBoxFocusIsFixed}
  TextToFindComboBox.Text:=NewFindText;
  {$ELSE}
  SetComboBoxText(TextToFindComboBox,NewFindText);
  {$ENDIF}
end;

function TLazFindReplaceDialog.GetReplaceText:AnsiString;
begin
  Result:=ReplaceTextComboBox.Text;
end;

procedure TLazFindReplaceDialog.SetReplaceText(NewReplaceText:AnsiString);
begin
  {$IFDEF DeleteMeWhenComboBoxFocusIsFixed}
  ReplaceTextComboBox.Text:=NewReplaceText;
  {$ELSE}
  SetComboBoxText(ReplaceTextComboBox,NewReplaceText);
  {$ENDIF}
end;

procedure TLazFindReplaceDialog.SetComboBoxText(AComboBox:TComboBox;
  const AText:AnsiString);
var a:integer;
begin
  a:=AComboBox.Items.IndexOf(AText);
  if a>=0 then
    AComboBox.ItemIndex:=a
  else begin
    AComboBox.Items.Add(AText);
    AComboBox.ItemIndex:=AComboBox.Items.IndexOf(AText);
  end;
end;

end.
