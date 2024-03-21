{%MainUnit ../dialogs.pp}

{******************************************************************************
                                  TFileDialog
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

{------------------------------------------------------------------------------}
{  TFileDialog Create                                                          }
{------------------------------------------------------------------------------}
constructor TFileDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  fCompStyle := csFileDialog;
  FFiles := TStringList.Create;
  FHistoryList:=TStringList.Create;
  FFilterIndex := 1;
  FInternalFilterIndex := 1;
end;

{------------------------------------------------------------------------------}
{  TFileDialog Destroy                                                         }
{------------------------------------------------------------------------------}
destructor TFileDialog.Destroy;
begin
  FHistoryList.Free;
  FFiles.Free;
  inherited Destroy;
end;

procedure TFileDialog.DoCanClose(var CanClose: Boolean);
begin
  {$ifdef DebugCommonDialogEvents}
  debugln(['TFileDialog.DoCanClose: FUserChoice = ',ModalResultStr[FUserChoice],' HandleAllocated=',HandleAllocated]);
  {$endif}
  //Only call OnCanClose if user did not cancel the dialog, see:
  //http://docwiki.embarcadero.com/Libraries/Berlin/en/Vcl.Dialogs.TOpenDialog_Events
  if (FUserChoice = mrOK) then
    inherited DoCanClose(CanClose)
  else
  begin
    FDoCanCloseCalled := True;
    CanClose := True;
  end;
end;



{------------------------------------------------------------------------------}
{  TFileDialog DoOnTypeChange                                                         }
{------------------------------------------------------------------------------}
procedure TFileDialog.DoTypeChange;
begin
  if Assigned(FOnTypeChange) then
    OnTypeChange(Self);
end;

{------------------------------------------------------------------------------
  TFileDialog DoExecute
------------------------------------------------------------------------------}
procedure TFileDialog.SetHistoryList(const AValue: TStrings);
begin
  FHistoryList.Assign(AValue);
end;

procedure TFileDialog.IntfFileTypeChanged(NewFilterIndex: Integer);
begin
  if FilterIndex <> NewFilterIndex then
  begin
    FInternalFilterIndex := NewFilterIndex;
    DoTypeChange;
  end;
end;

class function TFileDialog.FindMaskInFilter(aFilter, aMask: string): integer;
{ The filter is a list of pairs of captions and masks separated by pipe |
  The masks are separated by semicolon ;
  For example:
    Pascal Files|*.pas;*.pp|All files|*.*
}
var
  p: PChar;
  MaskStart: PChar;
begin
  Result:=0;
  if (aFilter='') or (aMask='') then exit;
  p:=PChar(aFilter);
  while p^<>#0 do
  begin
    // skip caption
    while not (p^ in ['|',#0]) do inc(p);
    if p^=#0 then break;
    // parse masks
    repeat
      inc(p);
      MaskStart:=p;
      while not (p^ in [';','|',#0]) do inc(p);
      //debugln(['TFileDialog.ExtractAllFilterMasks Mask=',copy(aFilter,MaskStart-PChar(aFilter)+1,p-MaskStart)]);
      if (p>MaskStart)
      and (CompareFilenames(MaskStart,p-MaskStart,PChar(aMask),length(aMask),false)=0)
      then
        exit(MaskStart-PChar(aFilter)+1);
      if p^='|' then break;
      if p^=#0 then exit;
    until false;
    inc(p);
  end;
end;

class function TFileDialog.ExtractAllFilterMasks(aFilter: string;
  SkipAllFilesMask: boolean): string;
{ The filter is a list of pairs of captions and masks separated by pipe |
  The masks are separated by semicolon ;
  For example:
    Pascal Files|*.pas;*.pp|Include Files|*.inc|All files|*.*
    Result: *.pas;*.pp;*.inc
}
var
  AllFilesMask: string;

  procedure AddMask(MaskStart: PChar; MaskLen: integer; var Masks: string);
  var
    p: PChar;
    NewMask: string;
    Start: PChar;
  begin
    if (AllFilesMask<>'')
    and (CompareFilenames(@AllFilesMask[1],length(AllFilesMask),MaskStart,MaskLen,false)=0)
    then exit; // skip all files mask

    if Masks<>'' then
    begin
      p:=PChar(Masks);
      repeat
        Start:=p;
        while not (p^ in [#0,';']) do inc(p);
        if (CompareFilenames(Start,p-Start,MaskStart,MaskLen,false)=0) then
          exit;
        if p^=#0 then break;
        inc(p);
      until false;
    end;
    if Masks<>'' then
      Masks:=Masks+';';
    SetLength(NewMask,MaskLen);
    System.Move(MaskStart^,NewMask[1],length(NewMask));
    Masks:=Masks+NewMask;
  end;

var
  p: PChar;
  MaskStart: PChar;
begin
  Result:='';
  if aFilter='' then exit;
  if SkipAllFilesMask then
    AllFilesMask:=GetAllFilesMask
  else
    AllFilesMask:='';
  //debugln(['TFileDialog.ExtractAllFilterMasks Filter=',aFilter]);
  p:=PChar(aFilter);
  while p^<>#0 do
  begin
    // skip caption
    while not (p^ in ['|',#0]) do inc(p);
    if p^=#0 then break;
    // parse masks
    repeat
      inc(p);
      MaskStart:=p;
      while not (p^ in [';','|',#0]) do inc(p);
      //debugln(['TFileDialog.ExtractAllFilterMasks Mask=',copy(aFilter,MaskStart-PChar(aFilter)+1,p-MaskStart)]);
      if p>MaskStart then
        AddMask(MaskStart,p-MaskStart,Result);
      if p^='|' then break;
    until p^=#0;
    inc(p);
  end;
end;

{------------------------------------------------------------------------------
  procedure TFileDialog.SetDefaultExt(const AValue: string);
------------------------------------------------------------------------------}
procedure TFileDialog.SetDefaultExt(const AValue: string);
begin
  FDefaultExt:=AValue;
  if (FDefaultExt<>'') and (FDefaultExt[1]<>'.') then
    FDefaultExt:='.'+FDefaultExt;
end;

procedure TFileDialog.SetFilterIndex(const AValue: Integer);
begin
  FFilterIndex := AValue;
  if FHandle = 0 then
    FInternalFilterIndex := AValue;
end;

class procedure TFileDialog.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterFileDialog;
end;

{------------------------------------------------------------------------------
  TFileDialog GetFilterIndex
------------------------------------------------------------------------------}
function TFileDialog.GetFilterIndex: Integer;
begin
  Result := FInternalFilterIndex;
end;

{------------------------------------------------------------------------------}
{  TFileDialog SetFilter                                                       }
{------------------------------------------------------------------------------}
procedure TFileDialog.SetFilter(const value : string);
begin
  FFilter := Value;  // make sure this is defined first before the CNSendMessage
end;

{------------------------------------------------------------------------------}
{  TFileDialog SetFileName                                                     }
{------------------------------------------------------------------------------}
procedure TFileDialog.SetFileName(const value : string);
begin
  if FFilename=Value then exit;
  FFileName := Value;  // make sure this is defined first before the CNSendMessage
end;

{******************************************************************************
                                  TOpenDialog
 ******************************************************************************}

class procedure TOpenDialog.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterOpenDialog;
end;

procedure TOpenDialog.ResolveLinks;
var i: integer;
begin
  if Filename<>'' then
    Filename:=GetPhysicalFilename(FileName,pfeOriginal);
  if Files<>nil then
    for i:=0 to Files.Count-1 do
      if Files[i]<>'' then
        Files[i]:=GetPhysicalFilename(Files[i],pfeOriginal);
end;

function PromptForFileName(var AFileName: string;
                           const AFilter: string = '';
                           const ADefaultExt: string = '';
                           const ATitle: string = '';
                           const AInitialDir: string = '';
                           AIsSaveDialog: Boolean = False): Boolean;
var
  Dlg: TOpenDialog;
begin
  if AIsSaveDialog then
    Dlg := TSaveDialog.Create(nil)
  else
    Dlg := TOpenDialog.Create(nil);
  try
    Dlg.FileName := AFileName;
    Dlg.Filter := AFilter;
    Dlg.DefaultExt := ADefaultExt;
    Dlg.Title := ATitle;
    Dlg.InitialDir := AInitialDir;
    Result := Dlg.Execute;
    if Result then
      AFileName := Dlg.FileName;
  finally
    Dlg.Free;
  end;
end;

function TaskDlg(const aCaption, aMsg, aInfo: string; DlgIcon: TTaskDialogIcon;
  AButtons: TMsgDlgButtons;
  const aFooter: string; const aDetails: string;
  AFlags: TTaskDialogFlags): TModalResult;
var
  RadioIndex: integer;
  Checked: Boolean;
begin
  Result := TaskDlg(aCaption, aMsg, aInfo, DlgIcon, AButtons, [], -1,
    '', Checked, [], RadioIndex, -1,
    aFooter, aDetails, '', '', AFlags);
end;

function TaskDlg(const aCaption, aMsg, aInfo: string; DlgIcon: TTaskDialogIcon;
  AButtons: TMsgDlgButtons; ADefaultButton: TMsgDlgBtn;
  const aFooter: string; const aDetails: string;
  AFlags: TTaskDialogFlags): TModalResult;
var
  RadioIndex: integer;
  Checked: Boolean;
begin
  Result := TaskDlg(aCaption, aMsg, aInfo, DlgIcon, AButtons, [], ord(ADefaultButton),
    '', Checked, [], RadioIndex, -1,
    aFooter, aDetails, '', '', AFlags);
end;

function TaskDlg(const aCaption, aMsg, aInfo: string; DlgIcon: TTaskDialogIcon;
  AButtons: TMsgDlgButtons;
  const ARadioButtons: array of string; out RadioIndex: integer; ADefaultRadio: Integer;
  const aFooter: string; const aDetails: string;
  AFlags: TTaskDialogFlags): TModalResult;
var
  Checked: Boolean;
begin
  Result := TaskDlg(aCaption, aMsg, aInfo, DlgIcon, AButtons, [], -1,
    '', Checked, ARadioButtons, RadioIndex, ADefaultRadio,
    aFooter, aDetails, '', '', AFlags);
end;

function TaskDlg(const aCaption, aMsg, aInfo: string; DlgIcon: TTaskDialogIcon;
  AButtons: TMsgDlgButtons; ADefaultButton: TMsgDlgBtn;
  const ARadioButtons: array of string; out RadioIndex: integer; ADefaultRadio: Integer;
  const aFooter: string; const aDetails: string;
  AFlags: TTaskDialogFlags): TModalResult;
var
  Checked: Boolean;
begin
  Result := TaskDlg(aCaption, aMsg, aInfo, DlgIcon, AButtons, [], ord(ADefaultButton),
    '', Checked, ARadioButtons, RadioIndex, ADefaultRadio,
    aFooter, aDetails, '', '', AFlags);
end;

function TaskDlg(const aCaption, aMsg, aInfo: string; DlgIcon: TTaskDialogIcon;
  AButtons: TMsgDlgButtons;
  const ACheckBoxText: string; out Checked: boolean;
  const aFooter: string; const aDetails: string;
  AFlags: TTaskDialogFlags): TModalResult;
var
  RadioIndex: integer;
begin
  Result := TaskDlg(aCaption, aMsg, aInfo, DlgIcon, AButtons, [], -1,
    ACheckBoxText, Checked, [], RadioIndex, -1,
    aFooter, aDetails, '', '', AFlags);
end;

function TaskDlg(const aCaption, aMsg, aInfo: string; DlgIcon: TTaskDialogIcon;
  AButtons: TMsgDlgButtons; ADefaultButton: TMsgDlgBtn;
  const ACheckBoxText: string; out Checked: boolean;
  const aFooter: string; const aDetails: string;
  AFlags: TTaskDialogFlags): TModalResult;
var
  RadioIndex: integer;
begin
  Result := TaskDlg(aCaption, aMsg, aInfo, DlgIcon, AButtons, [], ord(ADefaultButton),
    ACheckBoxText, Checked, [], RadioIndex, -1,
    aFooter, aDetails, '', '', AFlags);
end;

function TaskDlg(const aCaption, aMsg, aInfo: string; DlgIcon: TTaskDialogIcon;
  AButtons: TMsgDlgButtons;
  const ACheckBoxText: string; out Checked: boolean;
  const ARadioButtons: array of string; out RadioIndex: integer; ADefaultRadio: Integer;
  const aFooter: string; const aDetails: string;
  AFlags: TTaskDialogFlags): TModalResult;
begin
  Result := TaskDlg(aCaption, aMsg, aInfo, DlgIcon, AButtons, [], -1,
    ACheckBoxText, Checked, ARadioButtons, RadioIndex, ADefaultRadio,
    aFooter, aDetails, '', '', AFlags);
end;

function TaskDlg(const aCaption, aMsg, aInfo: string; DlgIcon: TTaskDialogIcon;
  AButtons: TMsgDlgButtons; ADefaultButton: TMsgDlgBtn;
  const ACheckBoxText: string; out Checked: boolean;
  const ARadioButtons: array of string; out RadioIndex: integer; ADefaultRadio: Integer;
  const aFooter: string; const aDetails: string;
  AFlags: TTaskDialogFlags): TModalResult;
begin
  Result := TaskDlg(aCaption, aMsg, aInfo, DlgIcon, AButtons, [], ord(ADefaultButton),
    ACheckBoxText, Checked, ARadioButtons, RadioIndex, ADefaultRadio,
    aFooter, aDetails, '', '', AFlags);
end;

function TaskDlg(const aCaption, aMsg, aInfo: string; DlgIcon: TTaskDialogIcon;
  AButtons: TMsgDlgButtons; const ACustomButtons: array of string; ADefaultButton: Integer;
  const ACheckBoxText: string; out Checked: boolean;
  const ARadioButtons: array of string; out RadioIndex: integer; ADefaultRadio: Integer;
  const aFooter: string; const aDetails: string; const aShowDetails: string;
  const aHideDetails: string;
  AFlags: TTaskDialogFlags): TModalResult;
const
  //(mbYes, mbNo, mbOK, mbCancel, mbAbort, mbRetry, mbIgnore, mbAll, mbNoToAll, mbYesToAll, mbHelp, mbClose);
  MB2MR: array [TMsgDlgBtn] of TModalResult = (
    mrYes, mrNo, mrOK, mrCancel, mrAbort, mrRetry, mrIgnore,
    mrAll, mrNoToAll, mrYesToAll, -1 {help}, mrClose
  );
  MBCommonMask: TMsgDlgButtons = [mbOK, mbCancel, mbYes, mbNo, mbRetry, mbClose];
  MB2Common: array [TMsgDlgBtn] of TTaskDialogCommonButton= (
    tcbYes, tcbNo, tcbOk, tcbCancel, tcbNo{X}, tcbRetry, tcbNo{X},
    tcbNo{X}, tcbNo{X}, tcbNo{X}, tcbNo{X}, tcbClose
  );
var
  TheDlg: TTaskDialog;
  btn: TMsgDlgBtn;
  cbtn: TTaskDialogCommonButtons;
  i: Integer;
begin
  Result := mrCancel;
  RadioIndex := -1;
  Checked := False;
  TheDlg := TTaskDialog.Create(nil);
  try
    TheDlg.Flags := AFlags;
    TheDlg.Caption := aCaption;
    TheDlg.Title := aMsg;
    TheDlg.Text := aInfo;
    TheDlg.FooterText := aFooter;
    TheDlg.ExpandedText:= aDetails;
    TheDlg.ExpandButtonCaption := aShowDetails;
    TheDlg.CollapseButtonCaption := aHideDetails;

    TheDlg.MainIcon := DlgIcon;
    if ACheckBoxText <> '' then
      TheDlg.VerificationText := ACheckBoxText;

    for i := 0 to Length(ACustomButtons) - 1 do
      with TheDlg.Buttons.Add do begin
        ModalResult := 500+i;
        Default := 500+i = ADefaultButton;
        Caption := ACustomButtons[i];
      end;

    if AButtons = AButtons * MBCommonMask then begin
      cbtn := [];
      for btn := low(AButtons) to high(AButtons) do
        if btn in AButtons then
          cbtn := cbtn + [MB2Common[btn]];
      TheDlg.CommonButtons := cbtn;
    end
    else begin
      TheDlg.CommonButtons := [];
      for btn := low(AButtons) to high(AButtons) do
        if btn in AButtons then
          begin
            with TheDlg.Buttons.Add do begin
              ModalResult := MB2MR[btn];
              Default := ord(btn) = ADefaultButton;
              case btn of
                mbYes:      Caption := rsMbYes;
                mbNo:       Caption := rsMbNo;
                mbOK:       Caption := rsMbOk;
                mbCancel:   Caption := rsMbCancel;
                mbAbort:    Caption := rsMbAbort;
                mbRetry:    Caption := rsMbRetry;
                mbIgnore:   Caption := rsMbIgnore;
                mbAll:      Caption := rsMbAbort;
                mbNoToAll:  Caption := rsMbNoToAll;
                mbYesToAll: Caption := rsMbYesToAll;
                mbHelp:     Caption := rsMbHelp;
                mbClose:    Caption := rsMbClose;
              end;
            end;
          end;
    end;

    for i := 0 to Length(ARadioButtons) - 1 do
      with TheDlg.RadioButtons.Add do begin
        ModalResult := i+1;
        Default := i = ADefaultRadio;
        Caption := ARadioButtons[i];
      end;


    if not TheDlg.Execute() then
       exit;
    Result := TheDlg.ModalResult;
    if TheDlg.RadioButton <> nil then
      RadioIndex := TheDlg.RadioButton.ModalResult - 1;
    Checked := tfVerificationFlagChecked in TheDlg.Flags;
  finally
    TheDlg.free
  end;
end;


//Helper functions
{
Extracts the actual filters from a TOpenDialog.Filter:
- it removes the filter "names"
- the resulting array will be zero-indexed
Example:
  Input : 'All files|*.*|foo|*.foo|bar|*.barfoo;*.bar;|Pascal|*.p?;*.pas'
  Output: ['*.*','*.foo','*.barfoo;*.bar;','*.p?;*.pas']
}
function ExtractFilterValues(const Filter: String): TStringArray;
var
  Arr: TStringArray;
  i: Integer;
begin
  Result := nil;
  Arr := Filter.Split('|'{$if fpc_fullversion >= 30202}, TStringSplitOptions.ExcludeLastEmpty{$endif});
  SetLength(Result, Length(Arr) div 2);
  for i := Low(Arr) to High(Arr) do
  begin
    if Odd(i) then Result[i div 2] := Arr[i];
  end;
end;

function GetExtensionFromFilterAtIndex(const Filter: String; Index: Integer): String;
{
  Returns a file extension from a filter as used in TOpen/TSaveDialog
  - it will return the extension (including the leading period) that matches the index (index starts at 1)
  - it will return an empty string if the extension contains a wildcard, or on any failure
  - filters have the format of:
    'Text files (*.txt)|*.txt|'+
    'Pascal files (*.pp;*.pas)|*.pp;*.pas|'+
    'All files (*.*)|*.*'
  - if a given extension is a composite (like '*.pp;*.pas') it will return the first one from the list
}
var
  i: Integer;
  FilterArr, CurrFilterArr: TStringArray;
  CurrFilter, S: String;
begin
  Result := '';
  if Index < 1 then
    Exit;
  FilterArr := ExtractFilterValues(Filter);
  Dec(Index); //adjust for zero-base FilterArr;
  if Index > High(FilterArr) then
    Exit;
  CurrFilter := FilterArr[Index];
  CurrFilterArr := CurrFilter.Split(';'{$if fpc_fullversion >= 30202}, TStringSplitOptions.ExcludeLastEmpty{$endif});
  for i := Low(CurrFilterArr) to High(CurrFilterArr) do
  begin
    S := ExtractFileExt(CurrFilterArr[i]);
    //if S is something like '*.p?;*.pas;' return the first one without a wildcard in the extension: e.g. '.pas'
    if (Pos('?',S) = 0) and (Pos('*',S) = 0) then
      Exit(S);
  end;
end;

function TOpenDialog.CheckFile(var AFilename: string): boolean;
var
  Dir, Ext: string;
begin
  Result:=true;
  if (DefaultExt<>'') and (ExtractFileExt(AFilename)='')
  and (not FileExistsUTF8(AFilename)) then begin
    Ext := GetExtensionFromFilterAtIndex(Filter, FilterIndex);
    if (Length(Ext) > 0) then
      AFileName := AFileName + Ext
    else
      AFilename:=AFilename+DefaultExt;
  end;
  //ofOverwritePrompt -> is done in the interface
  if (ofPathMustExist in Options)
  and (not DirPathExists(ExtractFileDir(AFilename))) then begin
    Result:=false;
    MessageDlg(rsfdPathMustExist,
               Format(rsfdPathNoExist,[ExtractFileDir(AFilename)]),
               mtError,[mbCancel],0);
    exit;
  end;
  if (ofFileMustExist in Options)
  and (not CheckFileMustExist(AFileName)) then begin
    // CheckFileMustExists shows message dialog
    Result:=false;
    exit;
  end;
  if ofNoReadOnlyReturn in Options then begin
    if FileExistsUTF8(AFilename) then
      Result := FileIsWritable(AFilename)
    else begin { File does not exist - check directory }
      Dir := ExtractFileDir(AFilename);
      if Dir = '' then
        Dir := '.';
      Result := DirectoryIsWritable(Dir);
    end;
    if not Result then begin
        MessageDlg(rsfdFileReadOnlyTitle,
                   Format(rsfdFileReadOnly,[AFileName]),
                   mtError,[mbCancel],0);
        exit;
    end;
  end;
end;

function TOpenDialog.CheckFileMustExist(const AFileName: string): boolean;
begin
  if not FileExistsUTF8(AFilename) then begin
    Result:=false;
    MessageDlg(rsfdFileMustExist,
               Format(rsfdFileNotExist,[AFileName]),mtError,
               [mbCancel],0);
  end else
    Result:=true;
end;

function TOpenDialog.CheckAllFiles: boolean;
var
  AFilename: String;
  i: Integer;
begin
  Result:=true;

  AFilename:=Filename;
  if (AFilename<>'')
  or (not (ofAllowMultiSelect in Options)) then begin
    Result:=CheckFile(AFilename);
    Filename:=AFilename;
    if not Result then exit;
  end;

  if ofAllowMultiSelect in Options then begin
    for i:=0 to Files.Count-1 do begin
      AFilename:=Files[i];
      Result:=CheckFile(AFilename);
      Files[i]:=AFilename;
      if not Result then exit;
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method: TOpenDialog.DoExecute
  Params:  none
  Returns: true if valid was selected

  Starts dialogs and lets user choose a filename.
 ------------------------------------------------------------------------------}
function TOpenDialog.DoExecute: boolean;
begin
  Exclude(FOptions, ofExtensionDifferent);
  Result:=inherited DoExecute;
  if not (ofNoResolveLinks in Options) then
    ResolveLinks;
  if not (ofNoChangeDir in Options) then begin
    if (ExtractFilePath(Filename)<>'') then
      InitialDir:=ExtractFilePath(Filename)
    else if (Files.Count>0) and (ExtractFilePath(Files[0])<>'') then
      InitialDir:=ExtractFilePath(Files[0]);
  end;
  if not Result then exit;
  Result:=CheckAllFiles;
  //The behavour of ofExtensionDifferent is based upon the workings of Windows LPOPENFILENAME behaviour
  //This behaviour is completely erratic when multiple files are selected, it can be on or off given the same input
  //Delphi 7 seems to not include the ofExtensionDifferent flag if multiple files are selected, regardless of their extensions
  //So, we unset the ofExtensionDifferent if multiple files are selected
  if Result then begin
    if (Files.Count > 1) then
      Exclude(FOptions, ofExtensionDifferent)
    else begin
    if (DefaultExt <> '') and (CompareFileNames(DefaultExt,ExtractFileExt(Filename)) <> 0) then
      Include(FOptions, ofExtensionDifferent);
    end;
  end;
end;

function TOpenDialog.DefaultTitle: string;
begin
  Result:= rsfdOpenFile;
end;

{------------------------------------------------------------------------------
  Method: TOpenDialog.Create
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TOpenDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  fCompStyle:=csOpenFileDialog;
  FOptions := DefaultOpenDialogOptions;
  FOptionsEx := [];
end;

procedure TOpenDialog.DoCanClose(var CanClose: Boolean);
begin
  //Delphi never calls OnCanClose in this case
  if not (ofOldStyleDialog in Options) then
    inherited DoCanClose(CanClose)
  else
  begin
    FDoCanCloseCalled := True;
    CanClose := True;
  end;
end;

procedure TOpenDialog.DoFolderChange;
begin
  if Assigned(OnFolderChange) then OnFolderChange(Self);
end;

procedure TOpenDialog.DoSelectionChange;
var
  CurFilename: String;
begin
  CurFilename:=Filename;
  if FLastSelectionChangeFilename=CurFilename then exit;
  FLastSelectionChangeFilename:=CurFilename;
  if Assigned(OnSelectionChange) then OnSelectionChange(Self);
end;

procedure TOpenDialog.IntfSetOption(const AOption: TOpenOption;
  const AValue: Boolean);
begin
  if AValue then
    Include(FOptions, AOption)
  else
    Exclude(FOptions, AOption);
end;


{******************************************************************************
                                  TSaveDialog
 ******************************************************************************}
class procedure TSaveDialog.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterSaveDialog;
end;

function TSaveDialog.DefaultTitle: string;
begin
  Result:=rsfdFileSaveAs;
end;

{------------------------------------------------------------------------------
  Method: TSaveDialog.Create
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TSaveDialog.Create (AOwner : TComponent);
begin
  inherited Create(AOwner);
  fCompStyle:=csSaveFileDialog;
end;

{******************************************************************************
                             TSelectDirectoryDialog
 ******************************************************************************}

{ TSelectDirectoryDialog }

constructor TSelectDirectoryDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCompStyle:=csSelectDirectoryDialog;
end;

class procedure TSelectDirectoryDialog.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterSelectDirectoryDialog;
end;

function TSelectDirectoryDialog.CheckFileMustExist(const AFilename: string): boolean;
begin
  if not DirPathExists(AFilename) then begin
    Result:=false;
    MessageDlg(rsfdDirectoryMustExist,
               Format(rsfdDirectoryNotExist,[AFileName]),mtError,
               [mbCancel],0);
  end else
    Result:=true;
end;

function TSelectDirectoryDialog.DefaultTitle: string;
begin
  Result:=rsfdSelectDirectory;
end;


