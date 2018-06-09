{ Copyright (C) 2007 Salvatore Coppola

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.

  Abstract:
  This unit is a message dialog composer for Lazarus/FPC.
  It takes into account all message dialogs platform indpendently.
}

unit MessageComposer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  LResources, LCLType, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Spin, Grids, ActnList, Buttons, EditBtn,
  // LazUtils
  ModalResultDef,
  // IdeIntf
  IDECommands, MenuIntf, LazIDEIntf, SrcEditorIntf;

type

  { TFormMessagesComposer }

  TFormMessagesComposer = class(TForm)
    KindMessageComboBox: TComboBox;
    KindMessageLabel: TLabel;
    MsgMemo: TMemo;
    ButtonsNotebook: TNotebook;
    PageString: TPage;
    ButtonPanel: TPanel;
    SourceNotebook: TNotebook;
    Page1: TPage;
    Page2: TPage;
    PageIf: TPage;
    PageCase: TPage;
    UpdateQuestioDlgResult: TAction;
    SetIfOrCase: TAction;
    PositionBevel: TBevel;
    Bevel2: TBevel;
    HelpCtxBevel: TBevel;
    CaseResultCheckGroup: TCheckGroup;
    BeginEndCheckBox: TCheckBox;
    IfResultComboBox: TComboBox;
    StringResultEdit: TEdit;
    GetParamsFmt: TAction;
    OkButton: TBitBtn;
    CancelButton: TBitBtn;
    GetMessageForSource: TAction;
    Label1: TLabel;
    StringResultLabel: TLabel;
    IfThenRadioButton: TRadioButton;
    IfThenElseRadioButton: TRadioButton;
    CaseOfEndRadioButton: TRadioButton;
    CaseOfEndElseRadioButton: TRadioButton;
    SourceWrapperGroupBox: TGroupBox;
    Test: TAction;
    TestButton: TButton;
    MaskInputCheckBox: TCheckBox;
    PromptEdit: TEdit;
    ValueEdit: TEdit;
    PromptLabel: TLabel;
    HelpFileNameEdit: TFileNameEdit;
    HelpFileNameLabel: TLabel;
    MessagesInit: TAction;
    DefaultEdit: TEdit;
    ValueLabel: TLabel;
    DefaultValue: TLabel;
    XLabel: TLabel;
    YLabel: TLabel;
    MessageSetup: TAction;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    DelConst: TAction;
    AddConst: TAction;
    ActionList1: TActionList;
    CaptionEdit: TEdit;
    ButtonsCheckGroup: TCheckGroup;
    DlgTypeComboBox: TComboBox;
    DlgTypeLabel: TLabel;
    HelpKeyWordEdit: TEdit;
    HelpContextLabel: TLabel;
    HelpKeyWordLabel: TLabel;
    CaptionLabel: TLabel;
    MsgLabel: TLabel;
    HelpContextSpinEdit: TSpinEdit;
    ButtonsStringGrid: TStringGrid;
    ButtonsPanel: TPanel;
    Panel2: TPanel;
    XSpinEdit: TSpinEdit;
    YSpinEdit: TSpinEdit;
    procedure AddConstExecute(Sender: TObject);
    procedure ButtonsCheckGroupItemClick(Sender: TObject; {%H-}Index: integer);
    procedure ButtonsStringGridSelectEditor(Sender: TObject; aCol,
      {%H-}aRow: Integer; var Editor: TWinControl);
    procedure DelConstExecute(Sender: TObject);
    procedure GetMessageForSourceExecute(Sender: TObject);
    procedure GetParamsFmtExecute(Sender: TObject);
    procedure MessageSetupExecute(Sender: TObject);
    procedure MessagesInitExecute(Sender: TObject);
    procedure SetIfOrCaseExecute(Sender: TObject);
    procedure TestExecute(Sender: TObject);
    procedure UpdateQuestioDlgResultExecute(Sender: TObject);
  private

  public

  end;

procedure Register;

var
  srcMessage: string;

implementation

{$R *.lfm}

const
  cMessageComposer = 'Message Composer';
  DoubleSpace = '  ';
  idxPageIf = 0;
  idxPageCase = 1;
  idxPageString = 2;

type
  TComposedMsgKind = (
    cmkMsgDlgSimple,             //MessageDlg[Msg, DlgType, Buttons(TMsgDlgButtons), HelpCtx]
    cmkMsgDlgCaption,            //MessageDlg[Caption, Msg, DlgType, Buttons(TMsgDlgButtons), HelpCtx]
    cmkMsgDlgCaptionHelpKeyWord, //MessageDlg[Caption, Msg, DlgType, Buttons(TMsgDlgButtons), HelpKeyword]
    cmkMsgDlgPosSimple,          //MessageDlgPos[Msg, DlgType, Buttons(TMsgDlgButtons), HelpCtx, X, Y]
    cmkMsgDlgPosHelpSimple,      //MessageDlgPosHelp[Msg, DlgType, Buttons(TMsgDlgButtons), HelpCtx, X, Y, HelpFileName]
    cmkQDlgHelpCtx,              //QuestionDlg[Caption, Msg, DlgType, Buttons(array of const), HelpCtx]
    cmkQDlgHelpKeyword,          //QuestionDlg[Caption, Msg, DlgType, Buttons(array of const), HelpKeyword]
    cmkShowMsg,                  //ShowMessage[Msg: string];
    cmkShowMsgFmt,               //ShowMessageFmt[Msg, Params(array of const)];
    cmkShowMsgPos,               //ShowMessagePos[Msg, X, Y];
    cmkInpQMask,                 //InputQuery[Caption, Prompt, MaskInput(Boolean), Value(String)]
    cmkInpQSimple,               //InputQuery[Caption, Prompt, Value(String)]
    cmkInpBox,                   //InputBox[Caption, Prompt, Default]
    cmkPasswBox                  //PasswordBox[Caption, Prompt]
  );

  const ComposedMsgStrings: array[TComposedMsgKind] of string = (
    'MessageDlg[Msg, DlgType, Buttons(TMsgDlgButtons), HelpCtx]',
    'MessageDlg[Caption, Msg, DlgType, Buttons(TMsgDlgButtons), HelpCtx]',
    'MessageDlg[Caption, Msg, DlgType, Buttons(TMsgDlgButtons), HelpKeyword]',
    'MessageDlgPos[Msg, DlgType, Buttons(TMsgDlgButtons), HelpCtx, X, Y]',
    'MessageDlgPosHelp[Msg, DlgType, Buttons(TMsgDlgButtons), HelpCtx, X, Y, HelpFileName]',
    'QuestionDlg[Caption, Msg, DlgType, Buttons(array of const), HelpCtx]',
    'QuestionDlg[Caption, Msg, DlgType, Buttons(array of const), HelpKeyword]',
    'ShowMessage[Msg: string];',
    'ShowMessageFmt[Msg, Params(array of const)];',
    'ShowMessagePos[Msg, X, Y];',
    'InputQuery[Caption, Prompt, MaskInput(Boolean), Value(String)]',
    'InputQuery[Caption, Prompt, Value(String)]',
    'InputBox[Caption, Prompt, Default]',
    'PasswordBox[Caption, Prompt]'
  );




resourcestring
  SMessageComposerMenuCaption = 'Message Composer ...';
  SMessageComposerCaption = 'Message Composer';
  SMsgCaption = 'The message to be shown';
  SDlgCaption = 'Dialog caption';
  SDlgType = 'Dialog type';
  SPromptCaption = 'Text asking the user for input';
  SMaskInput = 'Mask input';
  SSourceWrapper = 'Source wrapper';
  SKindofMessage = 'Kind of message';
  SNotImplementedYet = 'Not implemented yet';
  rsTest = 'Test';
  rsOk = 'OK';
  rsCancel = 'Cancel';
  SHelpContext = 'Help context';
  SHelpKeyword = 'Help keyword';
  SHelpFilename = 'Help filename';
  SValueVar = 'Value (var)';
  SDefault = 'Default';
  SButtonsTMsgDlgButtons = 'Buttons (TMsgDlgButtons)';
  SAdd = 'Add';
  SDelete = 'Delete';
  SButtonsArrayOfConst = 'Buttons (array of const)';
  SModalResult = 'Modal result';
  SIfResult = '"If" result';
  SCaseResult = '"Case" result';

var
  CmdMessageComposer: TIDECommand;



{ This is where it all starts. Gets called from Lazarus. }
procedure ExecuteMessagesComposer(Sender: TObject);
var FormMessagesComposer: TFormMessagesComposer;

  procedure FormatSrcMessage;
  var ListSrcMessages: TStringList;
      indx: integer;
      BaseStart: string;
  begin
    BaseStart := EmptyStr;
    for indx := 1 to SourceEditorManagerIntf.ActiveEditor.CursorTextXY.x-
                     Length(SourceEditorManagerIntf.ActiveEditor.Selection)-1 do
      BaseStart := BaseStart+#32;

    ListSrcMessages := TStringList.Create;
    ListSrcMessages.Text := srcMessage;
    if (Pos('if ',srcMessage) = 1)or(Pos('case ',srcMessage) = 1) then
      for indx := 1 to ListSrcMessages.Count-1 do
        ListSrcMessages.Strings[indx] := BaseStart+ListSrcMessages.Strings[indx];

    if Pos('case ',srcMessage) = 1 then
      for indx := 1 to ListSrcMessages.Count-2 do
        ListSrcMessages.Strings[indx] := DoubleSpace+ListSrcMessages.Strings[indx];

    srcMessage := ListSrcMessages.Text;
    ListSrcMessages.Free;
  end;

begin
  Assert(Sender <> nil);  // removes compiler warning
  if SourceEditorManagerIntf.ActiveEditor = nil then exit;
  FormMessagesComposer := TFormMessagesComposer.Create(nil);
  try
    FormMessagesComposer.ShowModal;
    if FormMessagesComposer.ModalResult = mrOK then  begin
      FormMessagesComposer.GetMessageForSource.Execute;
      FormatSrcMessage;
      SourceEditorManagerIntf.ActiveEditor.Selection := srcMessage;
    end;
  finally
    FormMessagesComposer.Free;
  end;
end;

{ TFormMessagesComposer }

procedure TFormMessagesComposer.AddConstExecute(Sender: TObject);
begin
  ButtonsStringGrid.RowCount := ButtonsStringGrid.RowCount+1;
  ButtonsStringGrid.AutoAdjustColumns;
  UpdateQuestioDlgResult.Execute;
end;

procedure TFormMessagesComposer.ButtonsCheckGroupItemClick(Sender: TObject;
  Index: integer);
begin
  MessageSetup.Execute;
end;

procedure TFormMessagesComposer.ButtonsStringGridSelectEditor(Sender: TObject; aCol,
  aRow: Integer; var Editor: TWinControl);
var
  ComposedMsgKind: TComposedMsgKind;
begin
  if aCol<>1 then exit;
  ComposedMsgKind := TComposedMsgKind(KindMessageComboBox.ItemIndex);
  case ComposedMsgKind of
    cmkQDlgHelpCtx, cmkQDlgHelpKeyword: begin
      Editor := TStringGrid(Sender).EditorByStyle(cbsPickList);
      TPickListCellEditor(Editor).Style := csDropDownList;
      TPickListCellEditor(Editor).Clear;
      TPickListCellEditor(Editor).Items.Add(ModalResultStr[mrNone]);
      TPickListCellEditor(Editor).Items.Add(ModalResultStr[mrOK]);
      TPickListCellEditor(Editor).Items.Add(ModalResultStr[mrCancel]);
      TPickListCellEditor(Editor).Items.Add(ModalResultStr[mrAbort]);
      TPickListCellEditor(Editor).Items.Add(ModalResultStr[mrRetry]);
      TPickListCellEditor(Editor).Items.Add(ModalResultStr[mrIgnore]);
      TPickListCellEditor(Editor).Items.Add(ModalResultStr[mrYes]);
      TPickListCellEditor(Editor).Items.Add(ModalResultStr[mrNo]);
      TPickListCellEditor(Editor).Items.Add(ModalResultStr[mrAll]);
      TPickListCellEditor(Editor).Items.Add(ModalResultStr[mrNoToAll]);
      TPickListCellEditor(Editor).Items.Add(ModalResultStr[mrYesToAll]);
    end;
    cmkShowMsgFmt: begin
      Editor := TStringGrid(Sender).EditorByStyle(cbsAuto);
//D Decimal format. Precision digits in it
//E Scientific format. Args is a Floating point value. Precision is used to specify the total number of decimals (exponent is3 digits)
//F Fixed point format. Args is a floating point value. Precision indicates the number of digits following the decimal point.
//G General number format. Args is a floating point value. The argument is converted to a decimal string using fixed point notation or scientific notation, depending on which gives the shortest result. Precision is used to determine the number of digits after the decimal point.
//M Currency format. Args must be a floating point value. The argument is converted to a decimal string using currency notation. This means that fixed-point notation is used, but that the currency symbol is appended. If precision is specified, then then it overrides the CurrencyDecimals global variable used in the FloatToStrF
//N Number format. This is the same as fixed point format, except that thousand separators are inserted in the resulting string.
//P Pointer format. The next argument in the Args array must be a pointer (typed or untyped). The pointer value is converted to a string of length 8, representing the hexadecimal value of the pointer.
//S String format. The next argument in the Args array must be a string. The argument is simply copied to the result string. If Precision is specified, then only Precision characters are copied to the result string.
//U Unsigned decimal format. The next argument in the Args array should be an unsigned integer. The argument is converted to a decimal string. If precision is specified, then the string will have at least Precision digits in it. If needed, the string is (left) padded with zeroes.
//X hexadecimal format. The next argument in the Args array must be an integer. The argument is converted to a hexadecimal string with just enough characters to contain the value of the integer. If Precision is specified then the resulting hexadecimal representation will have at least Precision characters in it (with a maximum value of 32).
    end;
  end;
  ButtonsStringGrid.AutoAdjustColumns;
end;

procedure TFormMessagesComposer.DelConstExecute(Sender: TObject);
begin
  if ButtonsStringGrid.RowCount = 1 then exit;
  ButtonsStringGrid.RowCount := ButtonsStringGrid.RowCount-1;
  ButtonsStringGrid.AutoAdjustColumns;
  UpdateQuestioDlgResult.Execute;
end;

procedure TFormMessagesComposer.GetMessageForSourceExecute(Sender: TObject);
var Msg, MsgCaption, MsgHelpKeyword, HelpFileName, Prompt, MsgDefault,
    Value, DlgType, MsgButtons, HelpCtx, X, Y, strParam: string;
    i: integer;
    intParam: integer;
    floatParam: double;
    ComposedMsgKind: TComposedMsgKind;
const
  BoolS: Array[Boolean] of String = ('False','True');
begin
  ComposedMsgKind := TComposedMsgKind(KindMessageComboBox.ItemIndex);
  Msg := QuotedStr(MsgMemo.Lines.Text);

  if Copy(Msg,Length(Msg)-Length(LineEnding),Length(LineEnding))=LineEnding then
    Delete(Msg,Length(Msg)-Length(LineEnding),Length(LineEnding));
  i := pos(LineEnding,Msg);
  while i>0 do begin
    Delete(Msg,i,Length(LineEnding));
    system.Insert(QuotedStr('+LineEnding+'),Msg,i);
    i := pos(LineEnding,Msg)
  end;

  DlgType := DlgTypeComboBox.Text;
  MsgButtons := EmptyStr;
  if ButtonsNotebook.PageIndex = 0 then begin  //MessageDlg();
    if ButtonsCheckGroup.Checked[0] then
      MsgButtons := MsgButtons+' mbYes,';
    if ButtonsCheckGroup.Checked[1] then
      MsgButtons := MsgButtons+' mbNo,';
    if ButtonsCheckGroup.Checked[2] then
      MsgButtons := MsgButtons+' mbOK,';
    if ButtonsCheckGroup.Checked[3] then
      MsgButtons := MsgButtons+' mbCancel,';
    if ButtonsCheckGroup.Checked[4] then
      MsgButtons := MsgButtons+' mbAbort,';
    if ButtonsCheckGroup.Checked[5] then
      MsgButtons := MsgButtons+' mbRetry,';
    if ButtonsCheckGroup.Checked[6] then
      MsgButtons := MsgButtons+' mbIgnore,';
    if ButtonsCheckGroup.Checked[7] then
      MsgButtons := MsgButtons+' mbAll,';
    if ButtonsCheckGroup.Checked[8] then
      MsgButtons := MsgButtons+' mbNoToAll,';
    if ButtonsCheckGroup.Checked[9] then
      MsgButtons := MsgButtons+' mbYesToAll,';
    if ButtonsCheckGroup.Checked[10] then
      MsgButtons := MsgButtons+' mbHelp,';
    if ButtonsCheckGroup.Checked[11] then
      MsgButtons := MsgButtons+' mbClose,';
  end else begin //maybe QuestionDlg() or ShowMessageFmt();
    if ComposedMsgKind=cmkShowMsgFmt then begin //ShowMessageFmt();
      for i := 1 to ButtonsStringGrid.RowCount-1 do begin
      //'D', 'E', 'F', 'G', 'M', 'N', 'P', 'S', 'U', 'X' format
        strParam := #32;
        if (ButtonsStringGrid.Cells[0, i] = '%D') then begin
          intParam := StrToIntDef(ButtonsStringGrid.Cells[1, i], 0);
          strParam := IntToStr(intParam);
        end;
        if (ButtonsStringGrid.Cells[0, i] = '%E')or
           (ButtonsStringGrid.Cells[0, i] = '%F')or
           (ButtonsStringGrid.Cells[0, i] = '%G')or
           (ButtonsStringGrid.Cells[0, i] = '%M')or
           (ButtonsStringGrid.Cells[0, i] = '%N') then begin
          floatParam := StrToFloatDef(ButtonsStringGrid.Cells[1, i], 0.0);
          strParam := FloatToStr(floatParam);
          if pos('.',strParam) = 0 then
            strParam := strParam+'.0';
        end;
        if (ButtonsStringGrid.Cells[0, i] = '%P') then
          strParam := ButtonsStringGrid.Cells[1, i];//user must knows pointer var in his source
        if (ButtonsStringGrid.Cells[0, i] = '%S') then
          strParam := QuotedStr(ButtonsStringGrid.Cells[1, i]);
        if (ButtonsStringGrid.Cells[0, i] = '%U') then begin
          intParam := abs(StrToIntDef(ButtonsStringGrid.Cells[1, i], 0));
          strParam := IntToStr(intParam);
        end;
        if (ButtonsStringGrid.Cells[0, i] = '%X') then begin
          intParam := StrToIntDef(ButtonsStringGrid.Cells[1, i], 0);
          strParam := IntToStr(intParam);
        end;
        if strParam<>#32 then
          MsgButtons := MsgButtons+#32+strParam+',';
      end;
      if strParam=#32 then
        MsgButtons := strParam+',';
    end else
      for i := 1 to ButtonsStringGrid.RowCount-1 do begin
        if (ButtonsStringGrid.Cells[1, i] = EmptyStr)or
           (ButtonsStringGrid.Cells[0, i] = EmptyStr) then continue;
        MsgButtons := MsgButtons+#32+ButtonsStringGrid.Cells[1, i]+', '+
                      QuotedStr(ButtonsStringGrid.Cells[0, i])+',';
      end;
  end;
  if MsgButtons<>EmptyStr then begin
    MsgButtons[1] := '[';
    MsgButtons[Length(MsgButtons)] := ']';
  end else
    MsgButtons:='[]';
  MsgCaption := QuotedStr(CaptionEdit.Text);
  HelpCtx := HelpContextSpinEdit.Text;
  MsgHelpKeyword := QuotedStr(HelpKeyWordEdit.Text);
  HelpFileName := QuotedStr(HelpFileNameEdit.Text);
  X := XSpinEdit.Text;
  Y := YSpinEdit.Text;
  Prompt := QuotedStr(PromptEdit.Text);
  MsgDefault := QuotedStr(DefaultEdit.Text);
  Value := ValueEdit.Text;//user must knows Value var in his source
  case ComposedMsgKind of
    cmkMsgDlgSimple: srcMessage := 'MessageDlg('+Msg+', '+DlgType+', '+MsgButtons+','+HelpCtx+')';
    cmkMsgDlgCaption: srcMessage := 'MessageDlg('+MsgCaption+', '+Msg+', '+DlgType+', '+
                                   MsgButtons+', '+HelpCtx+')';
    cmkMsgDlgCaptionHelpKeyWord: srcMessage := 'MessageDlg('+MsgCaption+', '+Msg+', '+DlgType+', '+
                                   MsgButtons+', '+MsgHelpKeyword+')';
    cmkMsgDlgPosSimple: srcMessage := 'MessageDlgPos('+Msg+', '+DlgType+', '+MsgButtons+', '+
                                      HelpCtx+', '+X+', '+Y+')';
    cmkMsgDlgPosHelpSimple: srcMessage := 'MessageDlgPosHelp('+Msg+', '+DlgType+', '+MsgButtons+', '+
                                          HelpCtx+', '+X+', '+Y+', '+HelpFileName+')';
    cmkQDlgHelpCtx: srcMessage := 'QuestionDlg('+MsgCaption+', '+Msg+', '+DlgType+', '+
                                    MsgButtons+', '+HelpCtx+')';
    cmkQDlgHelpKeyword: srcMessage := 'QuestionDlg('+MsgCaption+', '+Msg+', '+DlgType+', '+
                                    MsgButtons+', '+MsgHelpKeyword+')';
    cmkShowMsg: srcMessage := 'ShowMessage('+Msg+')';
    cmkShowMsgFmt: srcMessage := 'ShowMessageFmt('+Msg+', '+MsgButtons+')';
    cmkShowMsgPos: srcMessage := 'ShowMessagePos('+Msg+', '+X+', '+Y+')';
    cmkInpQMask: srcMessage := 'InputQuery('+MsgCaption+', '+Prompt+', '+
                                BoolS[MaskInputCheckBox.Checked]+', '+Value+')';
    cmkInpQSimple: srcMessage := 'InputQuery('+MsgCaption+', '+Prompt+', '+Value+')';
    cmkInpBox: srcMessage := 'InputBox('+MsgCaption+', '+Prompt+', '+MsgDefault+')';
    cmkPasswBox: srcMessage := 'PasswordBox('+MsgCaption+', '+Prompt+')'
  end;

  if SourceWrapperGroupBox.Enabled then begin //no showmessages
    if (IfThenRadioButton.Checked)or(IfThenElseRadioButton.Checked) then begin
      srcMessage := 'if '+srcMessage+' = ';
      if SourceNoteBook.PageIndex <> idxPageString then
        srcMessage := srcMessage+IfResultComboBox.Text
      else
        srcMessage := srcMessage+QuotedStr(StringResultEdit.Text);
      srcMessage := srcMessage+' then';
      if BeginEndCheckBox.Checked then
        srcMessage := srcMessage+' begin '+LineEnding+'end';
    end;
    if IfThenElseRadioButton.Checked then begin
      srcMessage := srcMessage+LineEnding+'else';
      if BeginEndCheckBox.Checked then
        srcMessage := srcMessage+' begin '+LineEnding+'end';
    end;
    if CaseOfEndRadioButton.Checked then begin
      srcMessage := 'case '+srcMessage+' of'+LineEnding;
      for i := 0 to CaseResultCheckGroup.Items.Count-1 do begin
        if not CaseResultCheckGroup.Checked[i] then continue;
        if BeginEndCheckBox.Checked then
          srcMessage := srcMessage+CaseResultCheckGroup.Items[i]+
                        ': begin'+LineEnding+'end;'+LineEnding
        else
          srcMessage := srcMessage+CaseResultCheckGroup.Items[i]+
                        ': ;'+LineEnding;
      end;
      srcMessage := srcMessage+'end;'
    end;
    if CaseOfEndElseRadioButton.Checked then begin
      srcMessage := 'case '+srcMessage+' of'+LineEnding;
      for i := 0 to CaseResultCheckGroup.Items.Count-1 do begin
        if not CaseResultCheckGroup.Checked[i] then continue;
        if BeginEndCheckBox.Checked then
          srcMessage := srcMessage+CaseResultCheckGroup.Items[i]+
                        ': begin'+LineEnding+'end;'+LineEnding
        else
          srcMessage := srcMessage+CaseResultCheckGroup.Items[i]+
                        ': ;'+LineEnding;
      end;
      if BeginEndCheckBox.Checked then
        srcMessage := srcMessage+'else begin'+LineEnding+'end;'+LineEnding
      else
        srcMessage := srcMessage+'else'+LineEnding;
      srcMessage := srcMessage+'end;'
    end;
  end;
end;

procedure TFormMessagesComposer.GetParamsFmtExecute(Sender: TObject);
const FormatParams = ['D', 'E', 'F', 'G', 'M', 'N', 'P', 'S', 'U', 'X'];
var strtmp: string;
    ListParams: TStringList;
    indx: integer;
    chrtmp: Char;
    ComposedMsgKind: TComposedMsgKind;
begin
  ComposedMsgKind := TComposedMsgKind(KindMessageComboBox.ItemIndex);
  if ComposedMsgKind<>cmkShowMsgFmt then exit;
  ButtonsStringGrid.Cells[0, 0] := 'Params (array of const)';
  ButtonsStringGrid.Cells[1, 0] := 'Values';
  ButtonsStringGrid.FixedCols := 1;
  Panel2.Visible := false;

  ListParams := TStringList.Create;
  strtmp := UpperCase(MsgMemo.Lines.Text);

  indx := pos('%', strtmp);
  while (indx>0)and(indx<Length(strtmp)) do begin
    chrtmp := strtmp[indx+1];
    if chrtmp in FormatParams then
      ListParams.Add('%'+chrtmp);
    Delete(strtmp, indx, 1);
    indx := pos('%',strtmp);
  end;

  if ListParams.Count>0 then begin
    ListParams.Insert(0,'Params (array of const)');
    ButtonsStringGrid.RowCount := ListParams.Count;
    if ButtonsStringGrid.Cols[0].Text<>ListParams.Text then
      ButtonsStringGrid.Cols[0] := ListParams;
  end;
  ListParams.Free;
  ButtonsStringGrid.AutoAdjustColumns;
end;

procedure TFormMessagesComposer.MessageSetupExecute(Sender: TObject);
var indx: integer;
    ListResult: TStringList;
    ComposedMsgKind: TComposedMsgKind;
begin
  ComposedMsgKind := TComposedMsgKind(KindMessageComboBox.ItemIndex);
  if ComposedMsgKind in [cmkMsgDlgSimple,cmkMsgDlgCaption,cmkMsgDlgCaptionHelpKeyWord,
                         cmkMsgDlgPosSimple,cmkMsgDlgPosHelpSimple] then
    ButtonsNotebook.PageIndex := 0 // ButtonsCheckGroup
  else
    ButtonsNotebook.PageIndex := 1; // ButtonsPanel
//Msg
  case ComposedMsgKind of
    cmkMsgDlgSimple,cmkMsgDlgCaption,cmkMsgDlgCaptionHelpKeyWord,
    cmkMsgDlgPosSimple,cmkMsgDlgPosHelpSimple,
    cmkQDlgHelpCtx,cmkQDlgHelpKeyword,cmkShowMsg,
    cmkShowMsgFmt,cmkShowMsgPos:
    begin
      MsgMemo.Color := clWindow;
      MsgMemo.Enabled := true;
    end;
    else begin
      MsgMemo.Color := clBtnFace;
      MsgMemo.Enabled := false;
    end;
  end;
//Caption
  case ComposedMsgKind of
    cmkMsgDlgCaption,
    cmkMsgDlgCaptionHelpKeyWord,
    cmkQDlgHelpCtx,
    cmkQDlgHelpKeyword,
    cmkInpQMask,
    cmkInpQSimple,
    cmkInpBox,
    cmkPasswBox: begin
      CaptionEdit.Color := clWindow;
      CaptionEdit.Enabled := true;
    end;
    else begin
      CaptionEdit.Color := clBtnFace;
      CaptionEdit.Enabled := false;
    end;
  end;
//DlgType;
  case ComposedMsgKind of
    cmkMsgDlgSimple,
    cmkMsgDlgCaption,
    cmkMsgDlgCaptionHelpKeyWord,
    cmkMsgDlgPosSimple,
    cmkMsgDlgPosHelpSimple,
    cmkQDlgHelpCtx,
    cmkQDlgHelpKeyword: begin
      DlgTypeComboBox.Color := clWindow;
      DlgTypeComboBox.Enabled := true;
    end;
    else begin
      DlgTypeComboBox.Color := clBtnFace;
      DlgTypeComboBox.Enabled := false;
    end;
  end;
//HelpContext
  case ComposedMsgKind of
    cmkMsgDlgSimple,
    cmkMsgDlgCaption,
    cmkMsgDlgPosSimple,
    cmkMsgDlgPosHelpSimple,
    cmkQDlgHelpCtx: begin
      HelpContextSpinEdit.Color := clWindow;
      HelpContextSpinEdit.Enabled := true;
    end;
    else begin
      HelpContextSpinEdit.Color := clBtnFace;
      HelpContextSpinEdit.Enabled := false;
    end;
  end;
//HelpKeyword
  case ComposedMsgKind of
    cmkMsgDlgCaptionHelpKeyWord,
    cmkQDlgHelpKeyword: begin
      HelpKeyWordEdit.Color := clWindow;
      HelpKeyWordEdit.Enabled := true;
    end;
    else begin
      HelpKeyWordEdit.Color := clBtnFace;
      HelpKeyWordEdit.Enabled := false;
    end;
  end;
//Position X Y
  case ComposedMsgKind of
    cmkMsgDlgPosSimple,
    cmkMsgDlgPosHelpSimple,
    cmkShowMsgPos: begin
      XSpinEdit.Color := clWindow;
      XSpinEdit.Enabled := true;
      YSpinEdit.Color := clWindow;
      YSpinEdit.Enabled := true;
    end;
    else begin
      XSpinEdit.Color := clBtnFace;
      XSpinEdit.Enabled := false;
      YSpinEdit.Color := clBtnFace;
      YSpinEdit.Enabled := false;
    end;
  end;
//HelpFileName
  case ComposedMsgKind of
    cmkMsgDlgPosHelpSimple: begin
      HelpFileNameEdit.Color := clWindow;
      HelpFileNameEdit.Enabled := true;
    end;
    else begin
      HelpFileNameEdit.Color := clBtnFace;
      HelpFileNameEdit.Enabled := false;
    end;
  end;
//Params (array of const)
  case ComposedMsgKind of
    cmkShowMsgFmt: begin
      GetParamsFmt.Execute;
    end;
    else begin
      ButtonsStringGrid.Cells[0, 0] := SButtonsArrayOfConst;
      ButtonsStringGrid.Cells[1, 0] := SModalResult;
      ButtonsStringGrid.FixedCols := 0;
      Panel2.Visible := true;
    end;
  end;
//Prompt
  case ComposedMsgKind of
    cmkInpQMask,
    cmkInpQSimple,
    cmkInpBox,
    cmkPasswBox: begin
      PromptEdit.Color := clWindow;
      PromptEdit.Enabled := true;
    end;
    else begin
      PromptEdit.Color := clBtnFace;
      PromptEdit.Enabled := false;
    end;
  end;
//MaskInput
  case ComposedMsgKind of
    cmkInpQMask: begin
      MaskInputCheckBox.Enabled := true;
    end;
    else begin
      MaskInputCheckBox.Enabled := false;
    end;
  end;
//Value
  case ComposedMsgKind of
    cmkInpQMask,
    cmkInpQSimple: begin
      ValueEdit.Color := clWindow;
      ValueEdit.Enabled := true;
    end;
    else begin
      ValueEdit.Color := clBtnFace;
      ValueEdit.Enabled := false;
    end;
  end;
//Default
  case ComposedMsgKind of
    cmkInpBox: begin
      DefaultEdit.Color := clWindow;
      DefaultEdit.Enabled := true;
    end;
    else begin
      DefaultEdit.Color := clBtnFace;
      DefaultEdit.Enabled := false;
    end;
  end;

/////  Results and Source Wrapper for message //////

//InputBox(); PasswordBox();
  case ComposedMsgKind of
    cmkInpBox,
    cmkPasswBox: begin
      SourceNotebook.PageIndex := idxPageString;
      if CaseOfEndRadioButton.Checked then IfThenRadioButton.Checked := True;
      if CaseOfEndElseRadioButton.Checked then IfThenElseRadioButton.Checked := True;
      CaseOfEndRadioButton.Enabled := False;
      CaseOfEndElseRadioButton.Enabled := False;
      //StringResultEdit.Enabled := true;
      //StringResultEdit.Color := clWindow;
      //IfResultComboBox.Enabled := false;
      //IfResultComboBox.Color := clBtnFace;
      //CaseResultCheckGroup.Enabled := false;
      //CaseOfEndElseRadioButton.Enabled := false;
      //CaseOfEndRadioButton.Enabled := false;
    end;
    else begin
      CaseOfEndRadioButton.Enabled := True;
      CaseOfEndElseRadioButton.Enabled := True;
      if IfThenRadioButton.Checked or IfThenElseRadioButton.Checked then
        SourceNotebook.PageIndex := idxPageIf
      else
        SourceNotebook.PageIndex := idxPageCase;
      //StringResultEdit.Enabled := false;
      //StringResultEdit.Color := clBtnFace;
      //IfResultComboBox.Enabled := true;
      //IfResultComboBox.Color := clWindow;
      //CaseResultCheckGroup.Enabled := true;
      //CaseOfEndElseRadioButton.Enabled := true;
      //CaseOfEndRadioButton.Enabled := true;
    end;
  end;

//ShowMessage(); ShowMessageFmt(); ShowMessagePos();
  case ComposedMsgKind of
    cmkShowMsg,
    cmkShowMsgFmt,
    cmkShowMsgPos: SourceWrapperGroupBox.Enabled := false;
    else
      SourceWrapperGroupBox.Enabled := true;
  end;

//MessageDlg() Result
  case ComposedMsgKind of
    cmkMsgDlgSimple,
    cmkMsgDlgCaption,
    cmkMsgDlgCaptionHelpKeyWord,
    cmkMsgDlgPosSimple,
    cmkMsgDlgPosHelpSimple: begin
      ListResult := TStringList.Create;
      for indx := 0 to ButtonsCheckGroup.Items.Count-1 do
        if ButtonsCheckGroup.Checked[indx] then begin
          if ButtonsCheckGroup.Items[indx] = 'mbOK' then
            ListResult.Add(ModalResultStr[mrOK]);
          if ButtonsCheckGroup.Items[indx] = 'mbCancel' then
            ListResult.Add(ModalResultStr[mrCancel]);
          if ButtonsCheckGroup.Items[indx] = 'mbYes' then
            ListResult.Add(ModalResultStr[mrYes]);
          if ButtonsCheckGroup.Items[indx] = 'mbNo' then
            ListResult.Add(ModalResultStr[mrNo]);
          if ButtonsCheckGroup.Items[indx] = 'mbAbort' then
            ListResult.Add(ModalResultStr[mrAbort]);
          if ButtonsCheckGroup.Items[indx] = 'mbRetry' then
            ListResult.Add(ModalResultStr[mrRetry]);
          if ButtonsCheckGroup.Items[indx] = 'mbIgnore' then
            ListResult.Add(ModalResultStr[mrIgnore]);
          if ButtonsCheckGroup.Items[indx] = 'mbAll' then
            ListResult.Add(ModalResultStr[mrAll]);
          if ButtonsCheckGroup.Items[indx] = 'mbNoToAll' then
            ListResult.Add(ModalResultStr[mrNoToAll]);
          if ButtonsCheckGroup.Items[indx] = 'mbYesToAll' then
            ListResult.Add(ModalResultStr[mrYesToAll]);
          if (ButtonsCheckGroup.Items[indx] = 'mbClose')and
             (ListResult.IndexOf(ModalResultStr[mrCancel]) = -1) then
            ListResult.Add(ModalResultStr[mrCancel]);
        end;
      if ListResult.Text<>IfResultComboBox.Items.Text then begin
        IfResultComboBox.Items := ListResult;
        CaseResultCheckGroup.Items := ListResult;
        if ListResult.Count>0 then begin
          IfResultComboBox.ItemIndex := 0;
          CaseResultCheckGroup.Checked[0] := true;
        end;
      end;
      ListResult.Free;
    end;
  end;

//QuestionDlg() Result
  case ComposedMsgKind of
    cmkQDlgHelpCtx,
    cmkQDlgHelpKeyword: begin
      ListResult := TStringList.Create;
      ListResult.Assign(ButtonsStringGrid.Cols[1]);
      ListResult.Delete(0);
      if ListResult.Text<>IfResultComboBox.Items.Text then begin
        IfResultComboBox.Items := ListResult;
        CaseResultCheckGroup.Items := ListResult;
        if ListResult.Count>0 then begin
          IfResultComboBox.ItemIndex := 0;
          CaseResultCheckGroup.Checked[0] := true;
        end;
      end;
      ListResult.Free;
    end;
  end;

//InputQuery() Result
  case ComposedMsgKind of
    cmkInpQMask,
    cmkInpQSimple: if (CaseResultCheckGroup.Items.Count=0) or (CaseResultCheckGroup.Items[0]<>'false') then begin
      CaseResultCheckGroup.Items.Clear;
      CaseResultCheckGroup.Items.Add('false');
      CaseResultCheckGroup.Items.Add('true');
      IfResultComboBox.Items := CaseResultCheckGroup.Items;
      IfResultComboBox.ItemIndex := 0;
      CaseResultCheckGroup.Checked[0] := true;
    end;
  end;
  ButtonsStringGrid.AutoAdjustColumns;
  ButtonsPanel.Visible := not (ComposedMsgKind in [cmkShowMsg,cmkShowMsgPos,
                                 cmkInpQMask,cmkInpQSimple,cmkInpBox,cmkPasswBox]);
end;

procedure TFormMessagesComposer.MessagesInitExecute(Sender: TObject);
begin
  XSpinEdit.MaxValue := Screen.Width;
  XLabel.Caption := 'X ('+IntToStr(Screen.Width)+')';
  YSpinEdit.MaxValue := Screen.Height;
  YLabel.Caption := 'Y ('+IntToStr(Screen.Height)+')';
  HelpContextSpinEdit.Width := 80;
  XSpinEdit.Width := 60;
  YSpinEdit.Width := 60;

  KindMessageComboBox.Clear;
  KindMessageCombobox.Items.AddStrings(ComposedMsgStrings);
  KindMessageComboBox.ItemIndex := 0;
  Caption := SMessageComposerCaption;
  CaptionLabel.Caption := SDlgCaption;
  DlgTypeLabel.Caption := SDlgType;
  MsgLabel.Caption := SMsgCaption;
  ButtonsCheckGroup.Caption := SButtonsTMsgDlgButtons;
  ButtonsStringGrid.Cells[0,0] := SButtonsArrayOfConst;
  ButtonsStringGrid.Cells[1,0] := SModalResult;
  BitBtn1.Caption := SAdd;
  BitBtn2.Caption := SDelete;
  PromptLabel.Caption := SPromptCaption;
  MaskInputCheckBox.Caption := SMaskInput;
  ValueLabel.Caption := SValueVar;
  DefaultValue.Caption := SDefault;
  SourceWrapperGroupBox.Caption := SSourceWrapper;
  KindMessageLabel.Caption := SKindofMessage;
  Label1.Caption := SIfResult;
  CaseResultCheckGroup.Caption := SCaseResult;
  TestButton.Caption:=rsTest;
  OkButton.Caption:=rsOk;
  CancelButton.Caption:=rsCancel;
  HelpContextLabel.Caption := SHelpContext;
  HelpKeyWordLabel.Caption := SHelpKeyword;
  HelpFileNameLabel.Caption := SHelpFilename;
  MessageSetup.Execute;
end;

procedure TFormMessagesComposer.SetIfOrCaseExecute(Sender: TObject);
var
  ComposedMsgKind: TComposedMsgKind;
begin
  ComposedMsgKind := TComposedMsgKind(KindMessageComboBox.ItemIndex);
  if (ComposedMsgKind in [cmkInpBox, cmkPasswBox]) then exit;
  if IfThenRadioButton.Checked or IfThenElseRadioButton.Checked then
    SourceNotebook.PageIndex := idxPageIf
  else
    SourceNotebook.PageIndex := idxPageCase;
end;

procedure TFormMessagesComposer.TestExecute(Sender: TObject);
var Msg, MsgCaption, MsgHelpKeyword, HelpFileName, Prompt,
    MsgDefault, Value: string;
    DlgType: TMsgDlgType;
    MsgButtons: TMsgDlgButtons;
    HelpCtx: Longint;
    X, Y: integer;
    ComposedMsgKind: TComposedMsgKind;
begin
  Msg := MsgMemo.Lines.Text;
  DlgType := TMsgDlgType(DlgTypeComboBox.ItemIndex);
  MsgButtons := [];
  if ButtonsCheckGroup.Checked[0] then
    MsgButtons := MsgButtons+[mbYes];
  if ButtonsCheckGroup.Checked[1] then
    MsgButtons := MsgButtons+[mbNo];
  if ButtonsCheckGroup.Checked[2] then
    MsgButtons := MsgButtons+[mbOK];
  if ButtonsCheckGroup.Checked[3] then
    MsgButtons := MsgButtons+[mbCancel];
  if ButtonsCheckGroup.Checked[4] then
    MsgButtons := MsgButtons+[mbAbort];
  if ButtonsCheckGroup.Checked[5] then
    MsgButtons := MsgButtons+[mbRetry];
  if ButtonsCheckGroup.Checked[6] then
    MsgButtons := MsgButtons+[mbIgnore];
  if ButtonsCheckGroup.Checked[7] then
    MsgButtons := MsgButtons+[mbAll];
  if ButtonsCheckGroup.Checked[8] then
    MsgButtons := MsgButtons+[mbNoToAll];
  if ButtonsCheckGroup.Checked[9] then
    MsgButtons := MsgButtons+[mbYesToAll];
  if ButtonsCheckGroup.Checked[10] then
    MsgButtons := MsgButtons+[mbHelp];
  if ButtonsCheckGroup.Checked[11] then
    MsgButtons := MsgButtons+[mbClose];
  MsgCaption := CaptionEdit.Text;
  HelpCtx := HelpContextSpinEdit.Value;
  HelpKeyword := HelpKeyWordEdit.Text;
  X := XSpinEdit.Value;
  Y := YSpinEdit.Value;
  Prompt := PromptEdit.Text;
  MsgDefault := DefaultEdit.Text;
  Value := ValueEdit.Text;
  HelpFileName:='';
  MsgHelpKeyword:='';
  ComposedMsgKind := TComposedMsgKind(KindMessageComboBox.ItemIndex);
  case ComposedMsgKind of
    cmkMsgDlgSimple: MessageDlg(Msg, DlgType, MsgButtons, HelpCtx);
    cmkMsgDlgCaption: MessageDlg(MsgCaption, Msg, DlgType, MsgButtons, HelpCtx);
    cmkMsgDlgCaptionHelpKeyWord: MessageDlg(MsgCaption, Msg, DlgType, MsgButtons, MsgHelpKeyword);
    cmkMsgDlgPosSimple: MessageDlgPos(Msg, DlgType, MsgButtons, HelpCtx, X, Y);
    cmkMsgDlgPosHelpSimple: MessageDlgPosHelp(Msg, DlgType, MsgButtons, HelpCtx, X, Y, HelpFileName);
    cmkQDlgHelpCtx: QuestionDlg(MsgCaption, Msg+' ('+SNotImplementedYet+')', DlgType, [mrYes, 'Yes', mrNo, 'No',
       mrCancel, 'Cancel'], HelpCtx);
    cmkQDlgHelpKeyword: QuestionDlg(MsgCaption, Msg+' ('+SNotImplementedYet+')', DlgType, [mrYes, 'Yes', mrNo, 'No',
       mrCancel, 'Cancel'], HelpKeyword);
    cmkShowMsg: ShowMessage(Msg);
    cmkShowMsgFmt: ShowMessageFmt(Msg+' ('+SNotImplementedYet+')', ['Yes','No','Cancel']);
    cmkShowMsgPos: ShowMessagePos(Msg, X, Y);
    cmkInpQMask: InputQuery(MsgCaption, Prompt, MaskInputCheckBox.Checked, Value);
    cmkInpQSimple: InputQuery(MsgCaption, Prompt, Value);
    cmkInpBox: InputBox(Caption, Prompt, MsgDefault);
    cmkPasswBox: PasswordBox(MsgCaption, Prompt)
  end;
end;

procedure TFormMessagesComposer.UpdateQuestioDlgResultExecute(Sender: TObject);
var ListResult: TStringList;
begin
  ListResult := TStringList.Create;
  ListResult.Assign(ButtonsStringGrid.Cols[1]);
  ListResult.Delete(0);
  if ListResult.Text<>IfResultComboBox.Items.Text then begin
    IfResultComboBox.Items := ListResult;
    CaseResultCheckGroup.Items := ListResult;
    if ListResult.Count>0 then begin
      IfResultComboBox.ItemIndex := 0;
      CaseResultCheckGroup.Checked[0] := true;
    end;
  end;
  ListResult.Free;
end;

{ Registers a keyboard shortcut and menu item in Lazarus}
procedure Register;
var
  Key1,Key2: TIDEShortCut;
  Cat: TIDECommandCategory;
begin
  Key1 := IDEShortCut(VK_M, [ssCtrl], VK_UNKNOWN, []);
  Key2 := IDEShortCut(VK_UNKNOWN, [], VK_UNKNOWN, []);
  Cat := IDECommandList.CreateCategory(Nil, cMessageComposer,
                                       SMessageComposerCaption,
                                       IDECmdScopeSrcEditOnly);

  CmdMessageComposer := RegisterIDECommand(Cat, cMessageComposer,
                                          SMessageComposerCaption,
                                          Key1, Key2,nil,
                                          @ExecuteMessagesComposer);

  RegisterIDEMenuCommand(itmSourceTools, cMessageComposer,
                         SMessageComposerMenuCaption,
                         nil, nil, CmdMessageComposer);
end;

end.

