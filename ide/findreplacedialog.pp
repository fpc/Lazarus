{
 /***************************************************************************
                          findreplacedialog.pp
                          --------------------

 ***************************************************************************/

  Author: Mattias Gaertner

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

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
  Classes, SysUtils, LCLType, Controls, StdCtrls, Forms, Buttons, ExtCtrls,
  LResources, SynEditTypes, SynEdit, IDEProcs, LazarusIdeStrConsts;

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
  public
    TextToFindLabel:TLabel;
    ReplaceWithLabel:TLabel;
    TextToFindComboBox:TComboBox;
    ReplaceTextComboBox:TComboBox;
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
    
    constructor Create(TheOwner:TComponent); override;
    procedure TextToFindComboboxKeyDown(Sender: TObject; var Key:Word;
       Shift:TShiftState);  
    procedure OkButtonClick(Sender:TObject);
    procedure ReplaceAllButtonClick(Sender:TObject);
    procedure CancelButtonClick(Sender:TObject);
    property Options:TSynSearchOptions read GetOptions write SetOptions;
    property FindText:AnsiString read GetFindText write SetFindText;
    property ReplaceText:AnsiString read GetReplaceText write SetReplaceText;
    property OnKey: TOnFindDlgKey read FOnKey write SetOnKey;
    property ComponentText[c: TFindDlgComponent]: string
      read GetComponentText write SetComponentText;
  end;

var FindReplaceDlg:TLazFindReplaceDialog;


implementation


{ TLazFindReplaceDialog }

constructor TLazFindReplaceDialog.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner);
  if LazarusResources.Find(ClassName)=nil then begin
    Name:='LazFindReplaceDialog';
    Caption:='';
    Width:=400;
    Height:=266;
    BorderStyle:= bsDialog;
    Position:=poDesigned;

    TextToFindComboBox:=TComboBox.Create(Self);
    with TextToFindComboBox do begin
      Name:='TextToFindComboBox';
      Parent:=Self;
      Left:=90;
      Top:=4;
      Width:= 306;
      Anchors:= [akLeft, akTop, akRight];
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
      Width:= 100;
      Caption:=dlgTextToFing;
      Visible:=true;
      FocusControl:= TextToFindComboBox;
    end;

    ReplaceTextComboBox:=TComboBox.Create(Self);
    with ReplaceTextComboBox do begin
      Name:='ReplaceTextComboBox';
      Parent:=Self;
      Left:=90;
      Top:=28;
      Width:= 306;
      Anchors:= [akLeft, akTop, akRight];
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
      Width:= 100;
      Caption:=dlgReplaceWith;
      Visible:=true;
      FocusControl:= ReplaceTextComboBox;
    end;

    OptionsGroupBox:=TGroupBox.Create(Self);
    with OptionsGroupBox do begin
      Name:='OptionsGroupBox';
      Parent:=Self;
      Left:=4;
      Top:=58;
      Width:=194;
      Height:=105;
      Caption:=dlgFROpts ;
      Visible:=true;
    end;

    CaseSensitiveCheckBox:=TCheckBox.Create(Self);
    with CaseSensitiveCheckBox do begin
      Name:='CaseSensitiveCheckBox';
      Parent:=OptionsGroupBox;
      AutoSize := True;
      Left:=8;
      Top:=6;
      Width:=155;
      Height:=17;
      Caption:=dlgCaseSensitive ;
      Visible:=true;
    end;

    WholeWordsOnlyCheckBox:=TCheckBox.Create(Self);
    with WholeWordsOnlyCheckBox do begin
      Name:='WholeWordsOnlyCheckBox';
      Parent:=OptionsGroupBox;
      AutoSize := False;
      Left:=8;
      Top:=26;
      Width:=155;
      Height:=17;
      Caption:=dlgWholeWordsOnly;
      Visible:=true;
    end;

    RegularExpressionsCheckBox:=TCheckBox.Create(Self);
    with RegularExpressionsCheckBox do begin
      Name:='RegularExpressionsCheckBox';
      Parent:=OptionsGroupBox;
      AutoSize := False;
      Left:=8;
      Top:=46;
      Width:=155;
      Height:=17;
      Caption:=dlgRegularExpressions ;
      Visible:=true;
    end;

    PromptOnReplaceCheckBox:=TCheckBox.Create(Self);
    with PromptOnReplaceCheckBox do begin
      Name:='PromptOnReplaceCheckBox';
      Parent:=OptionsGroupBox;
      AutoSize := False;
      Left:=8;
      Top:=66;
      Width:=135;
      Height:=17;
      Caption:=dlgPromptOnReplace ;
      Checked:=true;
      Visible:=true;
    end;

    OriginRadioGroup:=TRadioGroup.Create(Self);
    with OriginRadioGroup do begin
      Name:='OriginRadioGroup';
      Parent:= Self;
      Left:= 202;
      Top:= 58;
      Width:= 194;
      Height:=105;
      Caption:=dlgSROrigin;
      with Items do begin
        BeginUpdate;
        Clear;
        Add(dlgFromCursor);
        Add(dlgEntireScope);
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
      Width:=194;
      Height:=65;
      Caption:=dlgScope;
      with Items do begin
        BeginUpdate;
        Clear;
        Add(dlgGlobal);
        Add(dlgSelectedText);
        EndUpdate;
      end;
      ItemIndex:=0;
      Visible:=true;
    end;

    DirectionRadioGroup:=TRadioGroup.Create(Self);
    with DirectionRadioGroup do begin
      Name:='DirectionRadioGroup';
      Parent:=Self;
      Left:=202;
      Top:=168;
      Width:=194;
      Height:=65;
      Caption:=dlgDirection;
      with Items do begin
        BeginUpdate;
        Clear;
        Add(dlgUpWord);
        Add(dlgDownWord );
        EndUpdate;
      end;
      ItemIndex:=1;
      Visible:=true;
    end;

    OkButton:=TButton.Create(Self);
    with OkButton do begin
      Name:='OkButton';
      Parent:= Self;
      Left:= 143;
      Top:= 237;
      Caption:='Ok';
      OnClick:=@OkButtonClick;
      Visible:=true;
    end;

    ReplaceAllButton:=TButton.Create(Self);
    with ReplaceAllButton do begin
      Name:='ReplaceAllButton';
      Parent:= Self;
      Left:= 222;
      Top:= 237;
      Width:=99;
      Caption:=dlgReplaceAll;
      OnClick:=@ReplaceAllButtonClick;
      Visible:=true;
    end;

    CancelButton:=TButton.Create(Self);
    with CancelButton do begin
      Name:='CancelButton';
      Parent:= Self;
      Left:= 321;
      Top:= 237;
      Caption:=dlgCancel;
      OnClick:=@CancelButtonClick;
      Visible:=true;
    end;
    
  end;
  fReplaceAllClickedLast:=false;
  TextToFindComboBox.SetFocus;
end;

procedure TLazFindReplaceDialog.TextToFindComboBoxKeyDown(
  Sender: TObject; var Key:Word; Shift:TShiftState);
var Component: TFindDlgComponent;
begin
  //writeln('TLazFindReplaceDialog.TextToFindComboBoxKeyDown Key=',Key,' RETURN=',VK_RETURN,' TAB=',VK_TAB,' DOWN=',VK_DOWN,' UP=',VK_UP);
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
    Caption:=lisMenuReplace;
    OkButton.Caption:=lisMenuReplace;
  end else begin
    Caption:=lisMenuFind ;
    OkButton.Caption:=lisMenuFind ;
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
//  SetComboBoxText(TextToFindComboBox,NewFindText);
  TextToFindComboBox.Text:= NewFindText;
  TextToFindComboBox.SelectAll;
end;

function TLazFindReplaceDialog.GetReplaceText:AnsiString;
begin
  Result:=ReplaceTextComboBox.Text;
end;

procedure TLazFindReplaceDialog.SetReplaceText(NewReplaceText:AnsiString);
begin
  SetComboBoxText(ReplaceTextComboBox,NewReplaceText);
end;

procedure TLazFindReplaceDialog.SetComboBoxText(AComboBox:TComboBox;
  const AText:AnsiString);
var a:integer;
begin
  a:=AComboBox.Items.IndexOf(AText);
  //writeln('TLazFindReplaceDialog.SetComboBoxText ',AText,' ',a);
  if a>=0 then
    AComboBox.ItemIndex:=a
  else begin
    AComboBox.Items.Add(AText);
    AComboBox.ItemIndex:=AComboBox.Items.IndexOf(AText);
  end;
end;

end.
