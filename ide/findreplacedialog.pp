{
 /***************************************************************************
                          findreplacedialog.pp
                          --------------------

 ***************************************************************************/

  Author: Mattias Gaertner

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
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
  ExtCtrls, Dialogs, Graphics,
  SynEditTypes, SynRegExpr, SynEdit,
  IDEImagesIntf, IDEWindowIntf, LazarusIdeStrConsts, IDEContextHelpEdit,
  InputHistory;

type
  TFindDlgComponent = (fdcText, fdcReplace);
  TOnFindDlgKey = procedure(Sender: TObject; var Key: Word; Shift:TShiftState;
                           FindDlgComponent: TFindDlgComponent) of Object;

  { TLazFindReplaceDialog }

  TLazFindReplaceDialog = class(TForm)
    BackwardRadioButton: TRadioButton;
    HelpButton: TBitBtn;
    BtnPanel: TPanel;
    CancelButton: TBitBtn;
    CaseSensitiveCheckBox: TCheckBox;
    DirectionGroupBox: TGroupBox;
    EntireScopeRadioButton: TRadioButton;
    ForwardRadioButton: TRadioButton;
    FromCursorRadioButton: TRadioButton;
    GlobalRadioButton: TRadioButton;
    MultiLineCheckBox: TCheckBox;
    OKButton: TBitBtn;
    OptionsGroupBox: TGroupBox;
    OriginGroupBox: TGroupBox;
    PromptOnReplaceCheckBox: TCheckBox;
    RegularExpressionsCheckBox: TCheckBox;
    ReplaceAllButton: TBitBtn;
    ReplaceTextComboBox: TComboBox;
    ReplaceWithCheckbox: TCheckBox;
    ScopeGroupBox: TGroupBox;
    SelectedRadioButton: TRadioButton;
    EnableAutoCompleteSpeedButton: TSpeedButton;
    TextToFindComboBox: TComboBox;
    TextToFindLabel: TLabel;
    WholeWordsOnlyCheckBox: TCheckBox;
    procedure EnableAutoCompleteSpeedButtonClick(Sender: TObject);
    procedure FormChangeBounds(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure HelpButtonClick(Sender: TObject);
    procedure OptionsGroupBoxResize(Sender: TObject);
    procedure ReplaceWithCheckboxChange(Sender: TObject);
    procedure TextToFindComboboxKeyDown(Sender: TObject; var Key: Word;
       Shift: TShiftState);
    procedure OkButtonClick(Sender: TObject);
    procedure ReplaceAllButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
  private
    FOnKey: TOnFindDlgKey;
    fReplaceAllClickedLast: boolean;
    RegExpr: TRegExpr;
    DlgHistoryIndex: array[TFindDlgComponent] of integer;
    DlgUserText: array[TFindDlgComponent] of string;
    function CheckInput: boolean;
    function GetComponentText(c: TFindDlgComponent): string;
    function GetEnableAutoComplete: boolean;
    procedure SetComponentText(c: TFindDlgComponent; const AValue: string);
    procedure SetEnableAutoComplete(const AValue: boolean);
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
    procedure UpdateHints;
    procedure ResetUserHistory;
  public
    property Options: TSynSearchOptions read GetOptions write SetOptions;
    property EnableAutoComplete: boolean read GetEnableAutoComplete
                                         write SetEnableAutoComplete;
    property FindText:AnsiString read GetFindText write SetFindText;
    property ReplaceText:AnsiString read GetReplaceText write SetReplaceText;
    property OnKey: TOnFindDlgKey read FOnKey write SetOnKey;
    property ComponentText[c: TFindDlgComponent]: string
      read GetComponentText write SetComponentText;
  end;

var
  LazFindReplaceDialog: TLazFindReplaceDialog = nil;


implementation

{$R *.lfm}

{ TLazFindReplaceDialog }

constructor TLazFindReplaceDialog.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner);

  IDEDialogLayoutList.ApplyLayout(Self,420,350);

  Caption:='';

  TextToFindComboBox.Text:='';
  TextToFindLabel.Caption:=dlgTextToFing;
  ReplaceTextComboBox.Text:='';
  ReplaceWithCheckbox.Caption:=dlgReplaceWith;
  EnableAutoCompleteSpeedButton.LoadGlyphFromLazarusResource('menu_stepinto');

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
    Caption:=lisFindFileMultiLinePattern;
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

  HelpButton.Caption:=lisMenuHelp;
  ReplaceAllButton.Caption:=dlgReplaceAll;
  OKButton.Caption:=lisOk;
  CancelButton.Caption:=dlgCancel;

  fReplaceAllClickedLast:=false;

  UpdateHints;
end;

destructor TLazFindReplaceDialog.Destroy;
begin
  RegExpr.Free;
  inherited Destroy;
  if LazFindReplaceDialog=Self then
    LazFindReplaceDialog:=nil;
end;

procedure TLazFindReplaceDialog.UpdateHints;
begin
  if EnableAutoComplete then
    EnableAutoCompleteSpeedButton.Hint:=lisAutoCompletionOn
  else
    EnableAutoCompleteSpeedButton.Hint:=lisAutoCompletionOff;
end;

procedure TLazFindReplaceDialog.ResetUserHistory;
var
  c: TFindDlgComponent;
begin
  for c := Low(TFindDlgComponent) to High(TFindDlgComponent) do
    DlgHistoryIndex[c] := -1;
end;

procedure TLazFindReplaceDialog.TextToFindComboBoxKeyDown(
  Sender: TObject; var Key:Word; Shift:TShiftState);
var
  Component: TFindDlgComponent;
  HistoryList: TStringList;
  CurText: string;
  CurIndex: integer;

  procedure SetHistoryText;
  var s: string;
  begin
    if DlgHistoryIndex[Component]>=0 then
      s := HistoryList[DlgHistoryIndex[Component]]
    else
      s := DlgUserText[Component];
    //writeln('  SetHistoryText "',s,'"');
    ComponentText[Component]:=s
  end;

  procedure FetchFocus;
  begin
    if Sender is TWinControl then
      TWinControl(Sender).SetFocus;
  end;

begin
  //debugln('TLazFindReplaceDialog.TextToFindComboBoxKeyDown Key=',Key,' RETURN=',VK_RETURN,' TAB=',VK_TAB,' DOWN=',VK_DOWN,' UP=',VK_UP);
  if Sender=TextToFindComboBox then
    Component:=fdcText
  else
    Component:=fdcReplace;

  if Assigned(OnKey) then begin
    OnKey(Sender, Key, Shift, Component);
  end
  else
  begin
    if Component=fdcText then
      HistoryList:= InputHistories.FindHistory
    else
      HistoryList:= InputHistories.ReplaceHistory;
    CurIndex := DlgHistoryIndex[Component];
    CurText := ComponentText[Component];
    if Key=VK_UP then begin
      // go forward in history
      if CurIndex >= 0 then begin
        if (HistoryList[CurIndex] <> CurText) then
          DlgUserText[Component] := CurText; // save user text
        dec(DlgHistoryIndex[Component]);
        SetHistoryText;
      end;
      FetchFocus;
      Key:=VK_UNKNOWN;
    end
    else if Key=VK_DOWN then begin
      // go back in history
      if (CurIndex<0)
      or (HistoryList[CurIndex] <> CurText) then
        DlgUserText[Component] := CurText; // save user text
      if CurIndex < HistoryList.Count-1 then
      begin
        inc(DlgHistoryIndex[Component]);
        SetHistoryText;
      end;
      FetchFocus;
      Key:=VK_UNKNOWN;
    end;
  end;
end;

procedure TLazFindReplaceDialog.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TLazFindReplaceDialog.HelpButtonClick(Sender: TObject);
begin
  ShowContextHelpForIDE(Self);
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

procedure TLazFindReplaceDialog.ReplaceWithCheckboxChange(Sender: TObject);
begin
  ReplaceAllButton.Visible:=ReplaceWithCheckbox.Checked;
  ReplaceTextComboBox.Enabled:=ReplaceAllButton.Visible;
  PromptOnReplaceCheckBox.Enabled:=ReplaceAllButton.Visible;
  if ReplaceAllButton.Visible then
  begin
    Caption := lisMenuReplace;
    OkButton.Caption := lisBtnReplace;
  end else
  begin
    Caption := lisMenuFind;
    OkButton.Caption := lisBtnFind;
  end;
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

procedure TLazFindReplaceDialog.EnableAutoCompleteSpeedButtonClick(
  Sender: TObject);
begin
  TextToFindComboBox.AutoComplete:=EnableAutoCompleteSpeedButton.Down;
  ReplaceTextComboBox.AutoComplete:=EnableAutoCompleteSpeedButton.Down;
  UpdateHints;
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

function TLazFindReplaceDialog.GetEnableAutoComplete: boolean;
begin
  Result:=EnableAutoCompleteSpeedButton.Down;
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

procedure TLazFindReplaceDialog.SetEnableAutoComplete(const AValue: boolean);
begin
  EnableAutoCompleteSpeedButton.Down:=AValue;
  TextToFindComboBox.AutoComplete:=AValue;
  ReplaceTextComboBox.AutoComplete:=AValue;
  UpdateHints;
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
  ReplaceWithCheckbox.Checked:=ssoReplace in NewOptions;
  ReplaceTextComboBox.Enabled:=ReplaceAllButton.Visible;
  PromptOnReplaceCheckBox.Enabled:=ReplaceAllButton.Visible;

  if ssoReplace in NewOptions then
  begin
    Caption := lisMenuReplace;
    OkButton.Caption := lisBtnReplace;
  end else
  begin
    Caption := lisMenuFind;
    OkButton.Caption := lisBtnFind;
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
  //debugln('TLazFindReplaceDialog.SetComboBoxText ',AText,' ',a);
  if a>=0 then
    AComboBox.ItemIndex:=a
  else begin
    AComboBox.Items.Add(AText);
    AComboBox.ItemIndex:=AComboBox.Items.IndexOf(AText);
  end;
end;

end.
