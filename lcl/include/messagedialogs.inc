{%MainUnit ../dialogs.pp}

{******************************************************************************
                                  MessageDialogs
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
  current design flaws:

  - ??? There has to be at least one :-)

  Delphi compatibility:

   - the interface is almost like in delphi 5
     
  TODO:
    - Help Context
    - Help-button
    - User ability to customize Button order

}
function ModalEscapeValue(Buttons: TMsgDlgButtons): TModalResult;
begin
    Result := mrCancel;
end;

function ModalDefaultButton(Buttons : TMsgDlgButtons) : TMsgDlgbtn;
var
  b: TMsgDlgBtn;
begin
  If mbYes in Buttons then
    Result := mbYes
  else
    If mbOk in Buttons then
      Result := mbOk
  else
    If mbYesToAll in Buttons then
      Result := mbYesToAll
  else
    If mbAll in Buttons then
      Result := mbAll
  else
    If mbRetry in Buttons then
      Result := mbRetry
  else
    If mbCancel in Buttons then
      Result := mbCancel
  else
    If mbNo in Buttons then
      Result := mbNo
  else
    If mbNoToAll in Buttons then
      Result := mbNoToAll
  else
    If mbAbort in Buttons then
      Result := mbAbort
  else
    If mbIgnore in Buttons then
      Result := mbIgnore
  else
  begin
    for b := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
      if b in Buttons then
        Result := b;
  end;
end;

const
  DialogIds : Array[mtWarning..mtCustom] of Longint = (idDialogWarning,
    idDialogError, idDialogInfo, idDialogConfirm, idDialogBase);

  ButtonIds : Array[TMsgDlgbtn] of Longint = (idButtonYes, idButtonNo,
    idButtonOK, idButtonCancel, idButtonAbort, idButtonRetry, idButtonIgnore,
    idButtonAll, idButtonNoToAll, idButtonYesToAll, idButtonHelp,
    idButtonClose);

  DialogResults : Array[idButtonOK..idButtonNoToAll] of TModalResult = (
      mrOk, mrCancel,
    mrNone{Help - when a mbHelp button is pressed the help system is started,
                  the dialog does not close },
    mrYes, mrNo, mrClose, mrAbort, mrRetry,
    mrIgnore, mrAll, mrYesToAll, mrNoToAll);

  ButtonResults : Array[mrNone..mrClose] of Longint = (
    -1, idButtonOK, idButtonCancel, idButtonAbort, idButtonRetry,
    idButtonIgnore, idButtonYes, idButtonNo, idButtonAll, idButtonNoToAll,
    idButtonYesToAll, idButtonClose);

function GetPromptUserButtons(Buttons: TMsgDlgButtons; var CancelValue,
  DefaultIndex, ButtonCount : Longint; UseDefButton: Boolean; DefButton: TMsgDlgBtn) : PLongint;
var
  CurBtn      : TMsgDlgBtn; // variable to loop through TMsgDlgButtons
  DefaultButton : TMsgDlgBtn;
begin
 if (Buttons = []) or (Buttons = [mbHelp]) then
    Buttons := Buttons + [mbOk];
  //Native PromptUser() dialog should return mrCancel on Escape or [X]-bordericon
  //TPromptDialog.CreatMessageDialog responds to Escape in "old Delhpi" style,
  //it will return mrCancel, mrNo, or mrOK if one of these buttons is present, else it will not respons to Escape key,
  //TPromptDialog.CreatMessageDialog does not use the CancelValue variable
  CancelValue := idButtonCancel;
  if UseDefButton then
    DefaultButton := DefButton
  else
    DefaultButton := ModalDefaultButton(Buttons);
  DefaultIndex := 0;
  ButtonCount := 0;
  Result := nil;
  for CurBtn := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
  begin
    if CurBtn in Buttons then
    begin
      ReallocMem(Result, (ButtonCount + 1) * SizeOf(Longint));
      Result[ButtonCount] := ButtonIds[CurBtn];
      if DefaultButton = CurBtn then
        DefaultIndex := ButtonCount;
      Inc(ButtonCount)
    end;
  end;
end;

function MessageDlg(const aMsg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): TModalResult;
var
  DefaultIndex,
  CancelValue,
  ButtonCount : Longint;
  Btns : PLongint;
begin
  Btns := GetPromptUserButtons(Buttons, CancelValue, DefaultIndex, ButtonCount,
    False, mbYes);
  Result := DialogResults[PromptUser(aMsg, DialogIds[DlgType], Btns, ButtonCount,
    DefaultIndex, CancelValue)];
  ReallocMem(Btns, 0);
end;

function MessageDlg(const aCaption, aMsg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): TModalResult;
var
  DefaultIndex,
  CancelValue,
  ButtonCount : Longint;
  Btns : PLongint;
begin
  Btns := GetPromptUserButtons(Buttons, CancelValue, DefaultIndex, ButtonCount,
    False, mbYes);
  Result := DialogResults[PromptUser(aCaption, aMsg, DialogIds[DlgType], Btns,
    ButtonCount, DefaultIndex, CancelValue)];
  ReallocMem(Btns, 0);
end;

function MessageDlg(const aCaption, aMsg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn): TModalResult;
var
  DefaultIndex,
  CancelValue,
  ButtonCount : Longint;
  Btns : PLongint;
begin
  Btns := GetPromptUserButtons(Buttons, CancelValue, DefaultIndex, ButtonCount,
    True, DefaultButton);
  Result := DialogResults[PromptUser(aCaption, aMsg, DialogIds[DlgType], Btns,
    ButtonCount, DefaultIndex, CancelValue)];
  ReallocMem(Btns, 0);
end;

function MessageDlg(const aCaption, aMsg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; const HelpKeyword: string): TModalResult;
begin
  // TODO: handle HelpKeyword
  Result:=MessageDlg(aCaption, aMsg, DlgType, Buttons, 0);
end;

function MessageDlgPos(const aMsg: String; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; Helpctx : Longint; X,Y : Integer): TModalResult;
var
  DefaultIndex,
  CancelValue,
  ButtonCount : Longint;
  Btns : PLongint;
begin
  Btns := GetPromptUserButtons(Buttons, CancelValue, DefaultIndex, ButtonCount,
    False, mbYes);
  Result := DialogResults[PromptUserAtXY(aMsg, DialogIds[DlgType], Btns,
    ButtonCount, DefaultIndex, CancelValue, X, Y)];
  ReallocMem(Btns, 0);
end;

function MessageDlgPosHelp(const aMsg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer;
  const HelpFileName: string): TModalResult;
begin
  DebugLn ('MessageDlgPosHelp ****** NOT YET FULLY IMPLEMENTED ********');
//TODO: set helpcontext and helpfile
  result := MessageDlgPos(aMsg, DlgType, buttons, helpctx, X, Y);
end;

procedure ShowMessage(const aMsg: string);
begin
  NotifyUser(aMsg, idDialogBase);
end;

procedure ShowMessageFmt(const aMsg: string; Params: array of const);
begin
  NotifyUser(Format(aMsg, Params), idDialogBase);
end;

procedure ShowMessagePos(const aMsg: string; X, Y: Integer);
begin
  NotifyUserAtXY(aMsg, idDialogBase, X, Y);
end;

//----------------------------------------------------------------------------//
//-----------------------Prompt User For Information--------------------------//
function InputBox(const ACaption, APrompt, ADefault : String) : String;
begin
  Result := ADefault;
  InputQuery(ACaption, APrompt, Result);
end;

function PasswordBox(const ACaption, APrompt : String) : String;
begin
  Result := '';
  InputQuery(ACaption, APrompt, True, Result);
end;

function SelectDirectory(const Caption, InitialDirectory: string;
  out Directory: string): boolean;
begin
  Result:=SelectDirectory(Caption,InitialDirectory,Directory,false);
end;

function SelectDirectory(const Caption, InitialDirectory: string;
  out Directory: string; ShowHidden: boolean; HelpCtx: Longint): boolean;
var
  SelectDirectoryDialog: TSelectDirectoryDialog;
begin
  SelectDirectoryDialog:=TSelectDirectoryDialog.Create(nil);
  try
    if ShowHidden then
      SelectDirectoryDialog.Options:=SelectDirectoryDialog.Options
                                     +[ofForceShowHidden];
    SelectDirectoryDialog.InitialDir:=InitialDirectory;
    SelectDirectoryDialog.Title:=Caption;
    SelectDirectoryDialog.HelpContext:=HelpCtx;
    Result:=SelectDirectoryDialog.Execute;
    if Result then
      Directory:=SelectDirectoryDialog.Filename
    else
      Directory:='';
  finally
    SelectDirectoryDialog.Free;
  end;
end;

function SelectDirectory(out Directory: string;
  Options: TSelectDirOpts; HelpCtx: Longint): Boolean;
var
  SelectDirectoryDialog: TSelectDirectoryDialog;
begin
  SelectDirectoryDialog:=TSelectDirectoryDialog.Create(nil);
  // TODO: sdAllowCreate,
  // TODO: sdPrompt
  try
    SelectDirectoryDialog.HelpContext:=HelpCtx;
    Result:=SelectDirectoryDialog.Execute;
    if Result then begin
      Directory:=SelectDirectoryDialog.Filename;
      if (sdPerformCreate in Options) and (not DirPathExists(Directory)) then
        ForceDirectoriesUTF8(Directory);
    end else
      Directory:='';
  finally
    SelectDirectoryDialog.Free;
  end;
end;

function InputQuery(const ACaption, APrompt : String; MaskInput : Boolean;
  var Value : String) : Boolean;
begin
  Result := LCLIntf.RequestInput(ACaption, APrompt, MaskInput, Value);
end;

function InputQuery(const ACaption, APrompt : String; var Value : String) : Boolean;
begin
  Result := InputQuery(ACaption, APrompt, False, Value);
end;

// included by dialogs.pp
