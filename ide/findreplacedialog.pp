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

}
unit FindReplaceDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, Controls, StdCtrls, Forms, Buttons, ExtCtrls,
  LResources, Dialogs, SynEditTypes, SynRegExpr, SynEdit, LazarusIdeStrConsts;

type
  TFindDlgComponent = (fdcText, fdcReplace);
  TOnFindDlgKey = procedure(Sender: TObject; var Key: Word; Shift:TShiftState;
                           FindDlgComponent: TFindDlgComponent) of Object;

  TLazFindReplaceDialog = class(TForm)
    TextToFindLabel: TLabel;
    ReplaceWithLabel: TLabel;
    TextToFindComboBox: TComboBox;
    ReplaceTextComboBox: TComboBox;
    OptionsGroupBox: TGroupBox;
    CaseSensitiveCheckBox: TCheckBox;
    WholeWordsOnlyCheckBox: TCheckBox;
    RegularExpressionsCheckBox: TCheckBox;
    MultiLineCheckBox: TCheckBox;
    PromptOnReplaceCheckBox: TCheckBox;
    DirectionRadioGroup: TRadioGroup;
    ScopeRadioGroup: TRadioGroup;
    OriginRadioGroup: TRadioGroup;
    OkButton: TButton;
    ReplaceAllButton: TButton;
    CancelButton: TButton;
    procedure LazFindReplaceDialogResize(Sender: TObject);
    procedure OptionsGroupBoxResize(Sender: TObject);
    procedure TextToFindComboboxKeyDown(Sender: TObject; var Key: Word;
       Shift: TShiftState);
    procedure OkButtonClick(Sender: TObject);
    procedure ReplaceAllButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
  private
    FOnKey: TOnFindDlgKey;
    fReplaceAllClickedLast: boolean;
    RegExpr: TRegExpr;
    function CheckInput: boolean;
    function GetComponentText(c: TFindDlgComponent): string;
    procedure SetComponentText(c: TFindDlgComponent; const AValue: string);
    procedure SetOnKey(const AValue: TOnFindDlgKey);
    procedure SetOptions(NewOptions: TSynSearchOptions);
    function GetOptions: TSynSearchOptions;
    function GetFindText: AnsiString;
    procedure SetFindText(const NewFindText: AnsiString);
    function GetReplaceText: AnsiString;
    procedure SetReplaceText(const NewReplaceText: AnsiString);
    procedure SetComboBoxText(AComboBox: TComboBox; const AText: AnsiString);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  public
    property Options:TSynSearchOptions read GetOptions write SetOptions;
    property FindText:AnsiString read GetFindText write SetFindText;
    property ReplaceText:AnsiString read GetReplaceText write SetReplaceText;
    property OnKey: TOnFindDlgKey read FOnKey write SetOnKey;
    property ComponentText[c: TFindDlgComponent]: string
      read GetComponentText write SetComponentText;
  end;

var FindReplaceDlg: TLazFindReplaceDialog;


implementation


{ TLazFindReplaceDialog }

constructor TLazFindReplaceDialog.Create(TheOwner:TComponent);
var
  x: Integer;
  y: Integer;
  ComboX: Integer;
  OptionW: Integer;
  OptionsGrpW: Integer;
  OptionY: Integer;
  OptionH: Integer;
begin
  inherited Create(TheOwner);
  Name:='LazFindReplaceDialog';
  Caption:='';
  Width:=400;
  Height:=300;
  BorderStyle:= bsDialog;
  Position:=poDesigned;
  OnResize:=@LazFindReplaceDialogResize;

  x:=4;
  y:=4;
  ComboX:=x+90;
  TextToFindComboBox:=TComboBox.Create(Self);
  with TextToFindComboBox do begin
    Name:='TextToFindComboBox';
    Parent:=Self;
    SetBounds(ComboX,y,Parent.ClientWidth-ComboX-x,Height);
    Anchors:= [akLeft, akTop, akRight];
    Text:='';
    OnKeyDown:=@TextToFindComboBoxKeyDown;
  end;

  TextToFindLabel:=TLabel.Create(Self);
  with TextToFindLabel do begin
    Name:='TextToFindLabel';
    Parent:=Self;
    SetBounds(x,y+3,ComboX-x,Height);
    Caption:=dlgTextToFing;
    FocusControl:= TextToFindComboBox;
  end;
  inc(y,TextToFindComboBox.Height+1);
  
  ReplaceTextComboBox:=TComboBox.Create(Self);
  with ReplaceTextComboBox do begin
    Name:='ReplaceTextComboBox';
    Parent:=Self;
    SetBounds(ComboX,y,Parent.ClientWidth-ComboX-x,Height);
    Anchors:= [akLeft, akTop, akRight];
    Text:='';
    OnKeyDown:=@TextToFindComboBoxKeyDown;
  end;
  
  ReplaceWithLabel:=TLabel.Create(Self);
  with ReplaceWithLabel do begin
    Name:='ReplaceWithLabel';
    Parent:=Self;	
    SetBounds(x,y+3,ComboX-x,Height);
    Caption:=dlgReplaceWith;
    FocusControl:= ReplaceTextComboBox;
  end;
  inc(y,ReplaceTextComboBox.Height+1);

  OptionsGrpW:=(ClientWidth-15) div 2;
  x:=5;
  OptionsGroupBox:=TGroupBox.Create(Self);
  with OptionsGroupBox do begin
    Name:='OptionsGroupBox';
    Parent:=Self;
    SetBounds(x,y,OptionsGrpW,160);
    Caption:=dlgFROpts;
    OnResize:=@OptionsGroupBoxResize;
  end;

  OptionH:=OptionsGroupBox.ClientHeight div 5;
  OptionW:=OptionsGroupBox.ClientWidth-12;
  OptionY:=2;
  
  CaseSensitiveCheckBox:=TCheckBox.Create(Self);
  with CaseSensitiveCheckBox do begin
    Name:='CaseSensitiveCheckBox';
    Parent:=OptionsGroupBox;
    AutoSize := True;
    SetBounds(8,OptionY,OptionW,Height);
    Caption:=dlgCaseSensitive;
    Hint:=lisDistinguishBigAndSmallLettersEGAAndA;
    ShowHint:=true;
    inc(OptionY,OptionH);
  end;

  WholeWordsOnlyCheckBox:=TCheckBox.Create(Self);
  with WholeWordsOnlyCheckBox do begin
    Name:='WholeWordsOnlyCheckBox';
    Parent:=OptionsGroupBox;
    AutoSize := False;
    SetBounds(8,OptionY,OptionW,Height);
    Caption:=dlgWholeWordsOnly;
    Hint:=lisOnlySearchForWholeWords;
    ShowHint:=true;
    inc(OptionY,OptionH);
  end;

  RegularExpressionsCheckBox:=TCheckBox.Create(Self);
  with RegularExpressionsCheckBox do begin
    Name:='RegularExpressionsCheckBox';
    Parent:=OptionsGroupBox;
    AutoSize := False;
    SetBounds(8,OptionY,OptionW,Height);
    Caption:=dlgRegularExpressions;
    Hint:=lisActivateRegularExpressionSyntaxForTextAndReplaceme;
    ShowHint:=true;
    inc(OptionY,OptionH);
  end;

  MultiLineCheckBox:=TCheckBox.Create(Self);
  with MultiLineCheckBox do begin
    Name:='MultiLineCheckBox';
    Parent:=OptionsGroupBox;
    AutoSize := False;
    SetBounds(8,OptionY,OptionW,Height);
    Caption:=dlgMultiLine;
    Enabled:=false;
    Hint:=lisAllowSearchingForMultipleLines;
    ShowHint:=true;
    inc(OptionY,OptionH);
  end;

  PromptOnReplaceCheckBox:=TCheckBox.Create(Self);
  with PromptOnReplaceCheckBox do begin
    Name:='PromptOnReplaceCheckBox';
    Parent:=OptionsGroupBox;
    AutoSize := False;
    SetBounds(8,OptionY,OptionW,Height);
    Caption:=dlgPromptOnReplace;
    Checked:=true;
    Hint:=lisAskBeforeReplacingEachFoundText;
    ShowHint:=true;
    inc(OptionY,OptionH);
  end;

  OriginRadioGroup:=TRadioGroup.Create(Self);
  with OriginRadioGroup do begin
    Name:='OriginRadioGroup';
    Parent:= Self;
    SetBounds(x+OptionsGrpW+x,OptionsGroupBox.Top,OptionsGrpW,65);
    Caption:=dlgSROrigin;
    with Items do begin
      BeginUpdate;
      Clear;
      Add(dlgFromCursor);
      Add(dlgEntireScope);
      EndUpdate;
    end;
    ItemIndex:=0;
  end;

  ScopeRadioGroup:=TRadioGroup.Create(Self);
  with ScopeRadioGroup do begin
    Name:='ScopeRadioGroup';
    Parent:=Self;
    SetBounds(OriginRadioGroup.Left,
              OriginRadioGroup.Top+OriginRadioGroup.Height+5,
              OriginRadioGroup.Width,65);
    Caption:=dlgScope;
    with Items do begin
      BeginUpdate;
      Clear;
      Add(dlgGlobal);
      Add(dlgSelectedText);
      EndUpdate;
    end;
    ItemIndex:=0;
  end;

  DirectionRadioGroup:=TRadioGroup.Create(Self);
  with DirectionRadioGroup do begin
    Name:='DirectionRadioGroup';
    Parent:=Self;
    SetBounds(OriginRadioGroup.Left,
              ScopeRadioGroup.Top+ScopeRadioGroup.Height+5,
              OriginRadioGroup.Width,65);
    Caption:=dlgDirection;
    with Items do begin
      BeginUpdate;
      Clear;
      Add(dlgUpWord);
      Add(dlgDownWord);
      EndUpdate;
    end;
    ItemIndex:=1;
  end;

  OkButton:=TButton.Create(Self);
  with OkButton do begin
    Name:='OkButton';
    Parent:= Self;
    Default:=true;
    SetBounds(Parent.ClientWidth-350,Parent.ClientHeight-32,120,Height);
    Caption:='Ok';
    OnClick:=@OkButtonClick;
  end;

  ReplaceAllButton:=TButton.Create(Self);
  with ReplaceAllButton do begin
    Name:='ReplaceAllButton';
    Parent:= Self;
    SetBounds(OkButton.Left+OkButton.Width+10,OkButton.Top,120,Height);
    Caption:=dlgReplaceAll;
    OnClick:=@ReplaceAllButtonClick;
  end;

  CancelButton:=TButton.Create(Self);
  with CancelButton do begin
    Name:='CancelButton';
    Parent:= Self;
    Cancel:=true;
    SetBounds(ReplaceAllButton.Left+ReplaceAllButton.Width+10,OkButton.Top,
              80,Height);
    Caption:=dlgCancel;
    OnClick:=@CancelButtonClick;
  end;
    
  fReplaceAllClickedLast:=false;
  ActiveControl:=TextToFindComboBox;
end;

destructor TLazFindReplaceDialog.Destroy;
begin
  RegExpr.Free;
  inherited Destroy;
end;

procedure TLazFindReplaceDialog.TextToFindComboBoxKeyDown(
  Sender: TObject; var Key:Word; Shift:TShiftState);
var Component: TFindDlgComponent;
begin
  //writeln('TLazFindReplaceDialog.TextToFindComboBoxKeyDown Key=',Key,' RETURN=',VK_RETURN,' TAB=',VK_TAB,' DOWN=',VK_DOWN,' UP=',VK_UP);
  if (Key=VK_RETURN) then begin
    OkButtonClick(Sender);
    Key:=VK_UNKNOWN;
  end else if (Key=VK_ESCAPE) then begin
    CancelButtonClick(Sender);
    Key:=VK_UNKNOWN;
  end else if Key=VK_TAB then begin
    if (Sender=TextToFindComboBox) and (ReplaceTextComboBox.Enabled) then
      ReplaceTextComboBox.SetFocus;
    if Sender=ReplaceTextComboBox then
      TextToFindComboBox.SetFocus;
    Key:=VK_UNKNOWN;
  end else if Assigned(OnKey) then begin
    if Sender=TextToFindComboBox then
      Component:=fdcText
    else
      Component:=fdcReplace;
    OnKey(Sender, Key, Shift, Component);
  end;
end;

procedure TLazFindReplaceDialog.OptionsGroupBoxResize(Sender: TObject);
var
  OptionH: Integer;
  OptionW: Integer;
  OptionY: Integer;
begin
  OptionH:=OptionsGroupBox.ClientHeight div 5;
  OptionW:=OptionsGroupBox.ClientWidth-12;
  OptionY:=2;

  with CaseSensitiveCheckBox do begin
    SetBounds(8,OptionY,OptionW,Height);
    inc(OptionY,OptionH);
  end;

  with WholeWordsOnlyCheckBox do begin
    SetBounds(8,OptionY,OptionW,Height);
    inc(OptionY,OptionH);
  end;

  with RegularExpressionsCheckBox do begin
    SetBounds(8,OptionY,OptionW,Height);
    inc(OptionY,OptionH);
  end;

  with MultiLineCheckBox do begin
    SetBounds(8,OptionY,OptionW,Height);
    inc(OptionY,OptionH);
  end;

  with PromptOnReplaceCheckBox do begin
    SetBounds(8,OptionY,OptionW,Height);
    inc(OptionY,OptionH);
  end;
end;

procedure TLazFindReplaceDialog.LazFindReplaceDialogResize(Sender: TObject);
var
  x: Integer;
  y: Integer;
  ComboX: Integer;
  OptionsGrpW: Integer;
begin
  x:=4;
  y:=4;
  ComboX:=x+90;
  with TextToFindComboBox do begin
    SetBounds(ComboX,y,Parent.ClientWidth-ComboX-x,Height);
  end;

  with TextToFindLabel do begin
    SetBounds(x,y+3,ComboX-x,Height);
  end;
  inc(y,TextToFindComboBox.Height+1);

  with ReplaceTextComboBox do begin
    SetBounds(ComboX,y,Parent.ClientWidth-ComboX-x,Height);
  end;

  with ReplaceWithLabel do begin
    SetBounds(x,y+3,ComboX-x,Height);
  end;
  inc(y,ReplaceTextComboBox.Height+1);

  OptionsGrpW:=(ClientWidth-15) div 2;
  x:=5;
  with OptionsGroupBox do begin
    SetBounds(x,y,OptionsGrpW,160);
  end;

  with OriginRadioGroup do begin
    SetBounds(x+OptionsGrpW+x,OptionsGroupBox.Top,OptionsGrpW,65);
  end;

  with ScopeRadioGroup do begin
    SetBounds(OriginRadioGroup.Left,
              OriginRadioGroup.Top+OriginRadioGroup.Height+5,
              OriginRadioGroup.Width,65);
  end;

  with DirectionRadioGroup do begin
    SetBounds(OriginRadioGroup.Left,
              ScopeRadioGroup.Top+ScopeRadioGroup.Height+5,
              OriginRadioGroup.Width,65);
  end;

  with OkButton do begin
    SetBounds(Parent.ClientWidth-350,Parent.ClientHeight-32,120,Height);
  end;

  with ReplaceAllButton do begin
    SetBounds(OkButton.Left+OkButton.Width+10,OkButton.Top,120,Height);
  end;

  with CancelButton do begin
    SetBounds(ReplaceAllButton.Left+ReplaceAllButton.Width+10,OkButton.Top,
              80,Height);
  end;
end;

procedure TLazFindReplaceDialog.OkButtonClick(Sender:TObject);
begin
  if not CheckInput then exit;
  fReplaceAllClickedLast:=false;
  ActiveControl:=TextToFindComboBox;
  ModalResult:=mrOk;
end;

procedure TLazFindReplaceDialog.ReplaceAllButtonClick(Sender:TObject);
begin
  if not CheckInput then exit;
  fReplaceAllClickedLast:=true;
  ActiveControl:=TextToFindComboBox;
  ModalResult:=mrAll;
end;

procedure TLazFindReplaceDialog.CancelButtonClick(Sender:TObject);
begin
  ActiveControl:=TextToFindComboBox;
  ModalResult:=mrCancel;
end;

function TLazFindReplaceDialog.CheckInput: boolean;
begin
  Result:=false;
  if RegularExpressionsCheckBox.Checked then begin
    if RegExpr=nil then RegExpr:=TRegExpr.Create;
    try
      RegExpr.Expression:=FindText;
      RegExpr.Exec('test');
    except
      on E: ERegExpr do begin
        MessageDlg(lisUEErrorInRegularExpression,
          E.Message,mtError,[mbCancel],0);
        exit;
      end;
    end;
    if ReplaceTextComboBox.Enabled then begin
      try
        RegExpr.Substitute(ReplaceText);
      except
        on E: ERegExpr do begin
          MessageDlg(lisUEErrorInRegularExpression,
            E.Message,mtError,[mbCancel],0);
          exit;
        end;
      end;
    end;
  end;
  Result:=true;
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
  MultiLineCheckBox.Checked:=ssoRegExprMultiLine in NewOptions;
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
  if MultiLineCheckBox.Checked then Include(Result,ssoRegExprMultiLine);
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

procedure TLazFindReplaceDialog.SetFindText(const NewFindText:AnsiString);
begin
//  SetComboBoxText(TextToFindComboBox,NewFindText);
  TextToFindComboBox.Text:= NewFindText;
  TextToFindComboBox.SelectAll;
end;

function TLazFindReplaceDialog.GetReplaceText:AnsiString;
begin
  Result:=ReplaceTextComboBox.Text;
end;

procedure TLazFindReplaceDialog.SetReplaceText(const NewReplaceText:AnsiString);
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
