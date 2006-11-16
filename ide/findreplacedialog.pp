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
  Classes, Math, SysUtils, LCLProc, LCLType, Controls, StdCtrls, Forms, Buttons,
  ExtCtrls, LResources, Dialogs, SynEditTypes, SynRegExpr, SynEdit,
  LazarusIdeStrConsts, IDEWindowIntf;

type
  TFindDlgComponent = (fdcText, fdcReplace);
  TOnFindDlgKey = procedure(Sender: TObject; var Key: Word; Shift:TShiftState;
                           FindDlgComponent: TFindDlgComponent) of Object;

  { TLazFindReplaceDialog }

  TLazFindReplaceDialog = class(TForm)
    BackwardRadioButton: TRadioButton;
    ReplaceAllButton: TBitBtn;
    CaseSensitiveCheckBox: TCheckBox;
    EntireScopeRadioButton: TRadioButton;
    ForwardRadioButton: TRadioButton;
    FromCursorRadioButton: TRadioButton;
    GlobalRadioButton: TRadioButton;
    DirectionGroupBox: TGroupBox;
    OriginGroupBox: TGroupBox;
    ScopeGroupBox: TGroupBox;
    OptionsGroupBox: TGroupBox;
    MultiLineCheckBox: TCheckBox;
    OKButton: TBitBtn;
    PromptOnReplaceCheckBox: TCheckBox;
    RegularExpressionsCheckBox: TCheckBox;
    SelectedRadioButton: TRadioButton;
    TextToFindLabel: TLabel;
    ReplaceWithLabel: TLabel;
    TextToFindComboBox: TComboBox;
    ReplaceTextComboBox: TComboBox;
    CancelButton: TBitBtn;
    WholeWordsOnlyCheckBox: TCheckBox;
    procedure FormChangeBounds(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
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
    property Options: TSynSearchOptions read GetOptions write SetOptions;
    property FindText:AnsiString read GetFindText write SetFindText;
    property ReplaceText:AnsiString read GetReplaceText write SetReplaceText;
    property OnKey: TOnFindDlgKey read FOnKey write SetOnKey;
    property ComponentText[c: TFindDlgComponent]: string
      read GetComponentText write SetComponentText;
  end;

var
  LazFindReplaceDialog: TLazFindReplaceDialog = nil;


implementation


{ TLazFindReplaceDialog }

constructor TLazFindReplaceDialog.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner);

  IDEDialogLayoutList.ApplyLayout(Self,420,350);

  Caption:='';

  TextToFindComboBox.Text:='';
  TextToFindLabel.Caption:=dlgTextToFing;
  ReplaceTextComboBox.Text:='';
  ReplaceWithLabel.Caption:=dlgReplaceWith;

  OptionsGroupBox.Caption:=dlgFROpts;

  with CaseSensitiveCheckBox do begin
    Caption:=dlgCaseSensitive;
    Hint:=lisDistinguishBigAndSmallLettersEGAAndA;
  end;

  with WholeWordsOnlyCheckBox do begin
    Caption:=dlgWholeWordsOnly;
    Hint:=lisOnlySearchForWholeWords;
  end;

  with RegularExpressionsCheckBox do begin
    Caption:=dlgRegularExpressions;
    Hint:=lisActivateRegularExpressionSyntaxForTextAndReplaceme;
  end;

  with MultiLineCheckBox do begin
    Caption:=dlgMultiLine;
    Hint:=lisAllowSearchingForMultipleLines;
  end;

  with PromptOnReplaceCheckBox do begin
    Caption:=dlgPromptOnReplace;
    Hint:=lisAskBeforeReplacingEachFoundText;
  end;

  OriginGroupBox.Caption:=dlgSROrigin;
  FromCursorRadioButton.Caption := dlgFromCursor;
  EntireScopeRadioButton.Caption := dlgEntireScope;

  ScopeGroupBox.Caption:=dlgScope;
  GlobalRadioButton.Caption := dlgGlobal;
  SelectedRadioButton.Caption := dlgSelectedText;

  DirectionGroupBox.Caption:=dlgDirection;
  ForwardRadioButton.Caption := lisFRForwardSearch;
  BackwardRadioButton.Caption := lisFRBackwardSearch;

  ReplaceAllButton.Caption:=dlgReplaceAll;
  CancelButton.Caption:=dlgCancel;

  fReplaceAllClickedLast:=false;
end;

destructor TLazFindReplaceDialog.Destroy;
begin
  RegExpr.Free;
  inherited Destroy;
  if LazFindReplaceDialog=Self then
    LazFindReplaceDialog:=nil;
end;

procedure TLazFindReplaceDialog.TextToFindComboBoxKeyDown(
  Sender: TObject; var Key:Word; Shift:TShiftState);
var Component: TFindDlgComponent;
begin
  //debugln('TLazFindReplaceDialog.TextToFindComboBoxKeyDown Key=',Key,' RETURN=',VK_RETURN,' TAB=',VK_TAB,' DOWN=',VK_DOWN,' UP=',VK_UP);
  if Assigned(OnKey) then begin
    if Sender=TextToFindComboBox then
      Component:=fdcText
    else
      Component:=fdcReplace;
    OnKey(Sender, Key, Shift, Component);
  end;
end;

procedure TLazFindReplaceDialog.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TLazFindReplaceDialog.OptionsGroupBoxResize(Sender: TObject);
var
  h: integer;
begin
  DisableAlign;
  h := (OptionsGroupBox.Height-12) div 3;
  OriginGroupBox.Height := h;
  ScopeGroupBox.Height := h;
  DirectionGroupBox.Height := OptionsGroupBox.Height-2*h-12;
  EnableAlign;
end;

procedure TLazFindReplaceDialog.FormChangeBounds(Sender: TObject);
var
  w: integer;
begin
  DisableAlign;
  w := (ClientWidth - 18) div 2;
  OptionsGroupBox.Width := w;
  OriginGroupBox.Width := w;
  ScopeGroupBox.Width := w;
  DirectionGroupBox.Width := w;
  EnableAlign;
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
    then EntireScopeRadioButton.Checked:=True
    else FromCursorRadioButton.Checked:=True;
  if ssoSelectedOnly in NewOptions
    then SelectedRadioButton.Checked:=True
    else GlobalRadioButton.Checked:=True;
  if ssoBackwards in NewOptions
    then BackwardRadioButton.Checked:=True
    else ForwardRadioButton.Checked:=True;

  ReplaceAllButton.Visible:=ssoReplace in NewOptions;
  ReplaceTextComboBox.Enabled:=ReplaceAllButton.Visible;
  ReplaceWithLabel.Enabled:=ReplaceAllButton.Visible;
  PromptOnReplaceCheckBox.Enabled:=ReplaceAllButton.Visible;

  if ssoReplace in NewOptions then begin
    Caption:=lisMenuReplace;
    OkButton.Caption:=lisMenuReplace;
  end else begin
    Caption:=lisMenuFind;
    OkButton.Caption:=lisMenuFind;
  end;
  //DebugLn(['TLazFindReplaceDialog.SetOptions END ssoSelectedOnly=',ssoSelectedOnly in NewOptions,' SelectedRadioButton.Checked=',SelectedRadioButton.Checked]);
end;

function TLazFindReplaceDialog.GetOptions:TSynSearchOptions;
begin
  Result:=[];
  if CaseSensitiveCheckBox.Checked then Include(Result,ssoMatchCase);
  if WholeWordsOnlyCheckBox.Checked then Include(Result,ssoWholeWord);
  if RegularExpressionsCheckBox.Checked then Include(Result,ssoRegExpr);
  if MultiLineCheckBox.Checked then Include(Result,ssoRegExprMultiLine);
  if PromptOnReplaceCheckBox.Checked then Include(Result,ssoPrompt);

  if EntireScopeRadioButton.Checked then Include(Result,ssoEntireScope);
  if SelectedRadioButton.Checked then include(Result,ssoSelectedOnly);
  if BackwardRadioButton.Checked then include(Result,ssoBackwards);
  if ReplaceAllButton.Visible then include(Result,ssoReplace);
  if fReplaceAllClickedLast then include(Result,ssoReplaceAll);
end;

function TLazFindReplaceDialog.GetFindText:AnsiString;
begin
  Result:=TextToFindComboBox.Text;
end;

procedure TLazFindReplaceDialog.SetFindText(const NewFindText: AnsiString);
begin
//  SetComboBoxText(TextToFindComboBox,NewFindText);
  TextToFindComboBox.Text:=NewFindText;
  TextToFindComboBox.SelectAll;
  //debugln('TLazFindReplaceDialog.SetFindText A TextToFindComboBox.SelStart=',dbgs(TextToFindComboBox.SelStart),' TextToFindComboBox.SelLength=',dbgs(TextToFindComboBox.SelLength),' TextToFindComboBox.Text="',TextToFindComboBox.Text,'" NewFindText="',DbgStr(NewFindText),'"');
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

initialization
  {$I findreplacedialog.lrs}

end.
