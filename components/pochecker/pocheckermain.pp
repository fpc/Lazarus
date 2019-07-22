{
  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
  Boston, MA 02110-1335, USA.
}

// Original version made by Bart Broersma

unit pocheckermain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazFileUtils, Forms, Controls, Graphics, Dialogs,
  StdCtrls, CheckLst, Buttons, ExtCtrls, ComCtrls, Types,
  LCLType, LazUTF8, LCLTranslator,
  {$IFnDEF POCHECKERSTANDALONE}
  MenuIntf,
  {$ENDIF}
  PoFamilies, ResultDlg, pocheckerconsts, PoCheckerSettings,
  PoFamilyLists, PoCheckerMemoDlg;

type

  { TPoCheckerForm }

  TPoCheckerForm = class(TForm)
    TBImageList: TImageList;
    SelectAllMasterFilesBtn: TButton;
    SelectDirectoryDialog: TSelectDirectoryDialog;
    MainToolBar: TToolBar;
    ScanDirToolButton: TToolButton;
    Div1ToolButton: TToolButton;
    RunToolButton: TToolButton;
    UnselectAllMasterFilesBtn: TButton;
    ClearMasterFilesBtn: TButton;
    LangFilter: TComboBox;
    MasterPoListBox: TListBox;
    StatusBar: TStatusBar;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure ClearMasterFilesBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LangFilterChange(Sender: TObject);
    procedure MasterPoListBoxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure MasterPoListBoxSelectionChange(Sender: TObject; User: boolean);
    procedure RunToolButtonClick(Sender: TObject);
    procedure ScanDirToolButtonClick(Sender: TObject);
    procedure SelectAllMasterFilesBtnClick(Sender: TObject);
    procedure UnselectAllMasterFilesBtnClick(Sender: TObject);
  private
    //PoFamily: TPoFamily;
    PoFamilyList: TPoFamilyList;
    FPoCheckerSettings: TPoCheckerSettings;
    procedure OnTestStart(const ATestName, APoFileName: string);
    procedure OnTestEnd(const {%H-}ATestName: string; const {%H-}ErrorCount: integer);
    procedure FillTestListBox;
    function GetTestTypesFromListBox: TPoTestTypes;
    function GetTestOptions: TPoTestOptions;
    procedure SetTestTypeCheckBoxes(TestTypes: TPoTestTypes);
    procedure ShowError(const Msg: string);
    procedure ScanDirectory(ADir: String);
    function TryCreatepoFamilyList(var MasterList, SL: TStringList; const LangID: TLangID): Boolean;
    procedure RunSelectedTests(var SL, StatL, DupL: TStringList);
    procedure ClearStatusBar;
    procedure UpdateGUI(HasSelection: Boolean);
    function GetSelectedMasterFiles: TStringList;
    procedure AddToMasterPoList(S: TStrings);
    procedure SetSelectedMasterFiles(S: TStrings);
    procedure ApplyConfig;
    procedure SaveConfig;
    function LangFilterIndexToLangID(Index: Integer): TLangID;
    function LangIdToLangFilterIndex(LangID: TLangID): Integer;
    procedure PopulateLangFilter;
    procedure ApplyTranslations;
  published
    UnselectAllTestsBtn: TButton;
    SelectAllTestsBtn: TButton;
    SelectTestLabel: TLabel;
    TestListBox: TCheckListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SelectAllTestsBtnClick(Sender: TObject);
    procedure UnselectAllTestsBtnClick(Sender: TObject);
  end;

var
  PoCheckerForm: TPoCheckerForm;

procedure Register;

implementation

{$R *.lfm}


procedure ShowPoCheckerForm();
begin
  if not Assigned(PoCheckerForm) then
    PoCheckerForm := TPoCheckerForm.Create(Application);
  PoCheckerForm.Show;
end;


{ TPoCheckerForm }

procedure TPoCheckerForm.FormCreate(Sender: TObject);
begin
  FPoCheckerSettings := TPoCheckerSettings.Create;
  FPoCheckerSettings.LoadConfig;
  //debugln('TPoCheckerForm.FormCreate A:');
  {$IFDEF POCHECKERSTANDALONE}
  //Initializing translation
  if SetDefaultLang('', '..' + PathDelim + 'languages', 'pocheckerconsts') <> '' then
    TranslateLCLResourceStrings('', SetDirSeparators('../../../lcl/languages/'));
  {$ENDIF}
  ApplyTranslations;
  FillTestListBox;
  ClearStatusBar;
  PopulateLangFilter;
  ApplyConfig;
  LangFilter.Invalidate; //Items[0] may have been changed
end;


procedure TPoCheckerForm.FormDestroy(Sender: TObject);
begin
  if Assigned(PoFamilyList) then
    PoFamilyList.Free;
  if Assigned(FPoCheckerSettings) then
    FPoCheckerSettings.Free;
end;

procedure TPoCheckerForm.SelectAllTestsBtnClick(Sender: TObject);
begin
  TestListBox.CheckAll(cbChecked, False, False);
end;

procedure TPoCheckerForm.UnselectAllTestsBtnClick(Sender: TObject);
begin
  TestListBox.CheckAll(cbUnchecked, False, False);
end;

procedure TPoCheckerForm.LangFilterChange(Sender: TObject);
begin
  //This looks silly, but it makes that ItemIndex has the right value
  //in OnDestroy when you dropdown and change the filter, and then close
  //the form and no call to ItemIndex was made after changing the filter....
  //If someone figures out why, or has a better solution: please implement that
  LangFilter.ItemIndex;
end;

procedure TPoCheckerForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  SaveConfig;
end;

procedure TPoCheckerForm.ClearMasterFilesBtnClick(Sender: TObject);
begin
  MasterPoListBox.Clear;
  UpdateGUI(False);
end;

procedure TPoCheckerForm.FormShow(Sender: TObject);
begin
  WindowState := FPoCheckerSettings.MainFormWindowState;
  SetSelectedMasterFiles(FPoCheckerSettings.MasterPoSelList);
end;

procedure TPoCheckerForm.MasterPoListBoxDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  LB: TListBox;
  AText: String;
begin
  LB := TListBox(Control);
  with LB.Canvas do
  begin
    //if odSelected in State then Brush.Color := $00FFD2A6;
    FillRect(ARect);
    AText := ExtractFilename(LB.Items[Index]);
    TextOut(ARect.Left, ARect.Top, AText);
    if (odFocused in State) then
    begin
      Brush.Color := LB.Color;
      DrawFocusRect(ARect);
    end;
  end;
end;

procedure TPoCheckerForm.MasterPoListBoxSelectionChange(Sender: TObject;
  User: boolean);
begin
  //debugln('TPoCheckerForm.MasterPoListBoxSelectionChange: User = ',DbgS(User));
  if User then
  begin
    UpdateGUI(MasterPoListBox.SelCount > 0);
  end;
  UnselectAllMasterFilesBtn.Enabled := (MasterPoListBox.SelCount <> 0);
  SelectAllMasterFilesBtn.Enabled := (MasterPoListBox.Items.Count > 0);
end;

procedure TPoCheckerForm.RunToolButtonClick(Sender: TObject);
var
  AMasterList, SL, StatL, DupL: TStringList;
  LangIdx: Integer;
  ALangID: TLangID;
begin
  LangIdx := LangFilter.ItemIndex;
  ALangID := LangFilterIndexToLangID(LangIdx);
  SL := TStringList.Create;
  StatL := TStringList.Create;
  DupL := TStringList.Create;
  AMasterList := GetSelectedMasterFiles;
  try
    if TryCreatePoFamilyList(AMasterList, SL, ALangID) then
      RunSelectedTests(SL, StatL, DupL);
  finally
    SL.Free;
    StatL.Free;
    DupL.Free;
    AMasterList.Free;
  end;
end;

procedure TPoCheckerForm.ScanDirToolButtonClick(Sender: TObject);
begin
  if SelectDirectoryDialog.Execute then
  begin
    ScanDirectory(SelectDirectoryDialog.FileName);
  end;
end;

procedure TPoCheckerForm.SelectAllMasterFilesBtnClick(Sender: TObject);
begin
  MasterPoListBox.SelectAll;
  UpdateGUI(MasterPoListBox.SelCount > 0);
end;

procedure TPoCheckerForm.UnselectAllMasterFilesBtnClick(Sender: TObject);
begin
  MasterPoListBox.ClearSelection;
  UpdateGUI(False);
end;

procedure TPoCheckerForm.OnTestStart(const ATestName, APoFileName: string);
begin
  //debugln('OnTestStart: ATestName = "',AtestName,'" APoFileName = "',APoFileName);
  StatusBar.SimplePanel := True;
  StatusBar.SimpleText := Format(sCurrentTest,[ATestName,APoFileName]);
  Application.ProcessMessages;
end;


procedure TPoCheckerForm.OnTestEnd(const ATestName: string; const ErrorCount: integer);
begin
  //CurTestLabel.Caption := '';
  //CurPoLabel.Caption :=  '';
  //debugln('OnTestEnd [', ATestName, ']: ErrorCount = ', DbgS(ErrorCount));
  //Application.ProcessMessages;
end;


procedure TPoCheckerForm.FillTestListBox;
var
  Typ: TPoTestType;
begin
  TestListBox.Items.Clear;
  for Typ := Low(PoTestTypeNames) to High(PoTestTypeNames) do
    TestListBox.Items.Add(PoTestTypeNames[Typ]);
end;


function TPoCheckerForm.GetTestTypesFromListBox: TPoTestTypes;
var
  Typ: TPoTestType;
  Index: integer;
begin
  Result := [];
  for Typ := Low(TPoTestType) to High(TPoTestType) do
  begin
    Index := Ord(Typ);
    if (Index < TestListBox.Count) then
    begin
      if TestListBox.Checked[Index] then
        Result := Result + [Typ];
    end;
  end;
end;

function TPoCheckerForm.GetTestOptions: TPoTestOptions;
var
  ALangID: TLangID;
begin
  Result := [];
  ALangID := LangFilterIndexToLangID(LangFilter.ItemIndex);
  if ALangID = lang_all then
    Include(Result,ptoFindAllChildren);
end;

procedure TPoCheckerForm.SetTestTypeCheckBoxes(TestTypes: TPoTestTypes);
var
  Typ: TPoTestType;
  Index: integer;
begin
  for Typ := Low(TPoTestType) to High(TPoTestType) do
  begin
    Index := Ord(Typ);
    if (Index < TestListBox.Count) then
    begin
      TestListBox.Checked[Index] := (Typ in TestTypes)
    end;
  end;
end;

procedure TPoCheckerForm.ShowError(const Msg: string);
begin
  MessageDlg('Po-checker', Msg, mtError, [mbOK], 0);
end;

procedure TPoCheckerForm.ScanDirectory(ADir: String);
var
  SL, ML, OL, CurFiles, MissingFiles: TStringList;
  i: Integer;
  S, Mn: String;
  Cur: TCursor;
begin
  Cur := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  StatusBar.SimpleText := sScanningInProgress;
  try
    ML := FindAllFiles(ADir, '*.pot', True);
    OL := TStringList.Create;
    SL := FindAllFiles(ADir, '*.po', True);
    // first we check if all already present master .po files exist and remove them if not
    CurFiles := TStringList.Create;
    MissingFiles := TStringList.Create;
    CurFiles.Assign(MasterPoListBox.Items);
    i := 0;
    while i < CurFiles.Count do
    begin
      if not FileExistsUTF8(CurFiles[i]) then
      begin
        MissingFiles.Add(CurFiles[i]);
        MasterPoListBox.Items.Delete(MasterPoListBox.Items.IndexOf(CurFiles[i]));
      end;
      Inc(i);
    end;
    if ML.Count > 0 then
      AddToMasterPoList(ML);
    for i := 0 to SL.Count - 1 do
    begin
      S := SL[i];
      Mn := ExtractMasterNameFromChildName(S);
      if (Mn <> '') and (MasterPoListBox.Items.IndexOf(Mn) = -1) then
        OL.Add(S);
    end;
    if (OL.Count > 0) or (MissingFiles.Count > 0) then
    begin
      S := '';
      if OL.Count > 0 then
        S := Format(sTheFollowingOrphanedPoFileSFound, [IntToStr(OL.Count)]) + LineEnding + OL.Text;
      if MissingFiles.Count > 0 then
      begin
        if S <> '' then
          S := S + LineEnding;
        S := S + Format(sTheFollowingMissingMasterPoFileSWereRemoved, [
          IntToStr(MissingFiles.Count)]) + LineEnding + MissingFiles.Text;
      end;
      MemoDlg(sTroublesomeFiles, S);
    end;
    UpdateGUI(MasterPoListBox.SelCount > 0);
  finally
    ML.Free;
    OL.Free;
    CurFiles.Free;
    MissingFiles.Free;
    SL.Free;
    StatusBar.SimpleText := '';
    Screen.Cursor := Cur;
  end;
end;


function TPoCheckerForm.TryCreatepoFamilyList(var MasterList, SL: TStringList;
  const LangID: TLangID): Boolean;
var
  Fn, Msg, FamilyMsg: String;
  i, Cnt: Integer;
begin
  Result := False;
  Msg := '';
  Cnt := MasterList.Count;
  for i := Cnt - 1 downto 0 do
  begin
    Fn := MasterList.Strings[i];
    if not FileExistsUtf8(Fn) then
    begin
      MasterList.Delete(i);
      Msg := Format('"%s"',[Fn]) + LineEnding + Msg;
    end;
  end;
  if Msg <> '' then
    //MessageDlg('PoChecker',Format(sFilesNotFoundAndRemoved,[Msg]), mtInformation, [mbOk], 0);
    Msg := Format(sFilesNotFoundAndRemoved,[Msg]);
  Cnt := MasterList.Count;
  if Cnt = 0 then
    Msg := Msg + LineEnding + LineEnding + LineEnding + sNoFilesLeftToCheck;
  if Msg <> '' then
  begin
    SL.AddText(Msg);
    SL.Add('');
  end;
  if Cnt = 0 then
  begin
    //MessageDlg('PoChecker', sNoFilesLeftToCheck, mtInformation, [mbOk], 0);
    Exit;
  end;
  try
    if Assigned(PoFamilyList) then PoFamilyList.Free;
    PoFamilyList := TPoFamilyList.Create(MasterList, LangID, FamilyMsg);
    if FamilyMsg <> '' then
    begin
      //MessageDlg('PoChecker',Format(sFilesNotFoundAndRemoved,[FamilyMsg]), mtInformation, [mbOk], 0);
      if Msg = '' then
        FamilyMsg := Format(sFilesNotFoundAndRemoved,[FamilyMsg]);
      if PoFamilyList.Count = 0 then
        FamilyMsg := FamilyMsg + LineEnding + LineEnding + LineEnding + sNoFilesLeftToCheck;
      if FamilyMsg <> '' then
      begin
        SL.AddText(FamilyMsg);
        SL.Add('');
      end;
      if PoFamilyList.Count = 0 then
      begin
        //MessageDlg('PoChecker', sNoFilesLeftToCheck, mtInformation, [mbOk], 0);
        FreeAndNil(PoFamilyList);
        Exit;
      end;
    end;
    PoFamilyList.OnTestStart := @OnTestStart;
    PoFamilyList.OnTestEnd := @OnTestEnd;
    Result := True;
  except
    on E: Exception do
    begin
      Result := False;
      ShowError(Format(sErrorOnCreate, [E.Message]));
      if Assigned(PoFamilyList) then
      begin
        try
          FreeAndNil(PoFamilyList);
        except
          on E: Exception do
          begin
            ShowError(Format(sErrorOnCleanUp, [E.Message]));
          end;
        end;
      end;
    end;
  end;
end;


procedure TPoCheckerForm.RunSelectedTests(var SL, StatL, DupL: TStringList);
var
  TestTypes: TPoTestTypes;
  TestOptions: TPoTestOptions;
  ErrorCount, NonFuzzyErrorCount, WarningCount: integer;
  TotalTranslatedCount, TotalUntranslatedCount, TotalFuzzyCount: Integer;
  TotalPercTranslated: Double;
  ResultDlg: TResultDlgForm;
  mr: TModalResult;
begin
  TestTypes := GetTestTypesFromListBox;
  if (TestTypes = []) then
  begin
    ShowError(sNoTestSelected);
    Exit;
  end;
  TestOptions := GetTestOptions;
  Application.ProcessMessages;
  mr := mrNone;
  try
    PoFamilyList.TestTypes := TestTypes;
    PoFamilyList.TestOptions := TestOptions;

    PoFamilyList.RunTests(ErrorCount, NonFuzzyErrorCount, WarningCount, TotalTranslatedCount, TotalUntranslatedCount, TotalFuzzyCount, SL, StatL, DupL);
    //debugln('RunSelectedTests: ', Format(sTotalErrors, [ErrorCount]));
    //debugln('                  ', Format(sTotalWarnings, [WarningCount]));
    TotalPercTranslated := 100 * TotalTranslatedCount / (TotalTranslatedCount + TotalUntranslatedCount + TotalFuzzyCount);

    SL.Insert(0, sLastSearchPath);
    SL.Insert(1, SelectDirectoryDialog.FileName);
    SL.Insert(2, '');
    SL.Insert(3, sLanguage);
    SL.Insert(4, LangFilter.Text);
    SL.Insert(5, '');

    if NonFuzzyErrorCount > 0 then
      SL.Add(Format(sTotalErrorsNonFuzzy, [ErrorCount, NonFuzzyErrorCount]))
    else
      SL.Add(Format(sTotalErrors, [ErrorCount]));

    if not (ptoFindAllChildren in TestOptions) then
    begin
      SL.Add(Format(sTotalUntranslatedStrings, [IntToStr(TotalUntranslatedCount)]));
      SL.Add(Format(sTotalFuzzyStrings, [IntToStr(TotalFuzzyCount)]));
      SL.Add('');
      SL.Add(Format(sTotalTranslatedStrings, [IntToStr(TotalTranslatedCount), TotalPercTranslated]));

      StatL.Add(Format(sTotalUntranslatedStrings, [IntToStr(TotalUntranslatedCount)]));
      StatL.Add(Format(sTotalFuzzyStrings, [IntToStr(TotalFuzzyCount)]));
      StatL.Add('');
      StatL.Add(Format(sTotalTranslatedStrings, [IntToStr(TotalTranslatedCount), TotalPercTranslated]));
    end;

    DupL.Add(Format(sTotalWarnings, [WarningCount]));

    ResultDlg := TResultDlgForm.Create(nil);
    try
      ResultDlg.FTestOptions := TestOptions;
      ResultDlg.FTotalTranslated := TotalTranslatedCount;
      ResultDlg.FTotalUntranslated := TotalUntranslatedCount;
      ResultDlg.FTotalFuzzy := TotalFuzzyCount;
      ResultDlg.FTotalPercTranslated := TotalPercTranslated;
      ResultDlg.Log.Assign(SL);
      ResultDlg.StatLog.Assign(StatL);

      ResultDlg.DupLog.Assign(DupL);

      ResultDlg.PoFamilyList := PoFamilyList;
      ResultDlg.PoFamilyStats := PoFamilyList.PoFamilyStats;
      ResultDlg.Settings := FPoCheckerSettings;
      mr := ResultDlg.ShowModal;
    finally
      ResultDlg.Free;
    end;
  finally
    ClearStatusBar;
  end;
  if mr = mrOpenEditorFile then WindowState:= wsMinimized;
end;


procedure TPoCheckerForm.ClearStatusBar;
begin
  StatusBar.SimpleText := '';
end;

procedure TPoCheckerForm.UpdateGUI(HasSelection: Boolean);
begin
  RunToolButton.Enabled := HasSelection;
  TestListBox.Enabled := HasSelection;
  SelectAllTestsBtn.Enabled := HasSelection;
  UnselectAllTestsBtn.Enabled := HasSelection;
  UnselectAllMasterFilesBtn.Enabled := HasSelection;
  ClearMasterFilesBtn.Enabled := (MasterPoListBox.Items.Count > 0);
  SelectAllMasterFilesBtn.Enabled := (MasterPoListBox.Items.Count > 0);
end;

function TPoCheckerForm.GetSelectedMasterFiles: TStringList;
var
  i: Integer;
  Fn: String;
begin
  Result := TStringList.Create;
  for i := 0 to MasterPoListBox.Items.Count - 1 do
  begin
    Fn := MasterpoListBox.Items[i];
    if MasterPoListBox.Selected[i] then
      Result.Add(Fn);
  end;
end;

procedure TPoCheckerForm.AddToMasterPoList(S: TStrings);
var
  i, Idx: Integer;
  Str: String;
begin
  {
  Idx := MasterPoListBox.ItemIndex;
  if (Idx <> -1) then
    PrevItem := MasterPoListBox.Items[Idx]
  else
    PrevItem := '';
  }
  MasterPoListBox.Items.BeginUpdate;
  try
    for i := 0 to S.Count - 1 do
    begin
      Str := S[i];
      //skip files that do not exist (anymore)
      if (FileExistsUtf8(Str)) and (not FileExistsUTF8(ExtractFilePath(Str) + '.pocheckerignore')) then
      begin
        Idx := MasterPoListBox.Items.IndexOf(Str);
        if (Idx = -1) then
          MasterPoListBox.Items.Add(Str);
      end
    end;
    {
    if (PrevItem <> '') then
    begin
      Idx := MasterPoListBox.Items.IndexOf(PrevItem);
      MasterPoListBox.ItemIndex := Idx;
    end;
    }
  finally
    MasterPoListBox.Items.EndUpdate;
  end;
end;

procedure TPoCheckerForm.SetSelectedMasterFiles(S: TStrings);
var
  i, Idx: Integer;
  Fn: String;
  HasSelection: Boolean;
begin
  MasterPoListBox.ClearSelection;
  HasSelection := False;
  for i := 0 to S.Count - 1 do
  begin
    Fn := S.Strings[i];
    Idx := MasterPoListBox.Items.IndexOf(Fn);
    if (Idx <> -1) then
    begin
      MasterPoListBox.Selected[Idx] := True;
      HasSelection := True;
    end;
  end;
  //debugln('TPoCheckerForm.SetSelectedMasterFiles: S.Count = ',DbgS(S.Count),' HasSelection = ',DbgS(HasSelection));
  UpdateGUI(HasSelection);
end;


procedure TPoCheckerForm.ApplyConfig;
var
  ARect: TRect;
  Abbr: String;
  ID: TLangID;
begin
  ARect := FPoCheckerSettings.MainFormGeometry;
  if not IsDefaultRect(ARect) and IsValidRect(ARect) then
  begin
    ARect := FitToRect(ARect, Screen.WorkAreaRect);
    BoundsRect := ARect;
  end;
  SetTestTypeCheckBoxes(FPoCheckerSettings.TestTypes);
  SelectDirectoryDialog.Filename := FPoCheckerSettings.SelectDirectoryFilename;
  Abbr := FPoCheckerSettings.LangFilterLanguageAbbr;
  ID := LangAbbrToLangId(Abbr);
  LangFilter.ItemIndex := LangIdToLangFilterIndex(ID);
  AddToMasterPoList(FPoCheckerSettings.MasterPoList);
end;

procedure TPoCheckerForm.SaveConfig;
var
  SL: TStringList;
  ID: TLangID;
begin
  FPoCheckerSettings.SelectDirectoryFilename := SelectDirectoryDialog.Filename;
  //FPoCheckerSettings.LangFilterIndex := LangFilter.ItemIndex;
  ID := LangFilterIndexToLangID(LangFilter.ItemIndex);
  FPoCheckerSettings.LangFilterLanguageAbbr := LanguageAbbr[ID];
  FPoCheckerSettings.TestTypes := GetTestTypesFromListBox;
  FPoCheckerSettings.TestOptions := GetTestOptions;
  FPoCheckerSettings.MainFormWindowState := WindowState;
  if (WindowState = wsNormal) then
    FPoCheckerSettings.MainFormGeometry := BoundsRect
  else
    FPoCheckerSettings.MainFormGeometry := Rect(RestoredLeft, RestoredTop, RestoredLeft + RestoredWidth, RestoredTop + RestoredHeight);
  FPoCheckerSettings.MasterPoList := MasterPoListBox.Items;
  SL := GetSelectedMasterFiles;
  try
    FPoCheckerSettings.MasterPoSelList := SL;
  finally
    SL.Free;
  end;
  FPoCheckerSettings.SaveConfig;
end;

function ListSortFunc(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := Utf8CompareText(List.Strings[Index1], List.Strings[Index2]);
end;

function TPoCheckerForm.LangFilterIndexToLangID(Index: Integer): TLangID;
//Requires that items for a language end in [%lang_abbr%]
var
  S, Abbr: String;
  p: Integer;
begin
  Result := lang_all;
  S := LangFilter.Items[Index];
  p := Length(S); //no need to use Utf8 functions, we look for lower ASCII
  if (p = 0) or (not (S[p] = ']')) then Exit;
  repeat
    Dec(p);
  until (p = 0) or (S[p] = '[');
  if (p = 0) then Exit;
  Abbr := Copy(S, p+1, Length(S)-p-1);
  //DbgOut('Abbr = ',Abbr);
  Result := LangAbbrToLangID(Abbr);
  //debugln(' ID = ',Result);
end;

function TPoCheckerForm.LangIdToLangFilterIndex(LangID: TLangID): Integer;
//Requires that items for a language end in [%lang_abbr%]
var
  Abbr, S: String;
  p: SizeInt;
  i: Integer;
begin
  Result := 0; // All Languages
  if (LangID = lang_all) then Exit;
  Abbr := LanguageAbbr[LangID];
  for i := 1 to LangFilter.Items.Count - 1 do
  begin
    S := LangFilter.Items[i];
    //no need to use Utf8 functions, we look for lower ASCII
    p := Pos('['+Abbr+']',S);
    if (p > 0) then
      Exit(i);
  end;
end;

procedure TPoCheckerForm.PopulateLangFilter;
var
  ID: TLangID;
  Abbr, LangName, S: String;
  SL: TStringList;
begin
  LangFilter.Items.BeginUpdate;
  SL := TStringList.Create;
  try
    LangFilter.Items.Clear;
    for ID := Succ(Low(TLangID)) to High(TLangID) do
    begin
      Abbr := LanguageAbbr[ID];
      LangName := LanguageNames[ID];
      S := Format('%s [%s]',[LangName, Abbr]);
      SL.Add(S);
      SL.CustomSort(@ListSortFunc);
    end;
    SL.Sorted := False;
    SL.Insert(0, LanguageNames[lang_all]);
    LangFilter.Items.Assign(SL);
    LangFilter.Items.EndUpdate;
    LangFilter.ItemIndex := 0;
  finally
    SL.Free;
    LangFilter.Items.EndUpdate;
  end;
end;

procedure TPoCheckerForm.ApplyTranslations;
begin
  LocalizePoTestTypeNames;
  LocalizeLanguageNames;
  Caption := sGUIPoFileCheckingTool;
  SelectTestLabel.Caption := sSelectTestTypes;
  ScanDirToolButton.Caption := sScanDir;
  RunToolButton.Caption := sRunSelectedTests;
  ClearMasterFilesBtn.Caption := sClearListBox;
  UnselectAllMasterFilesBtn.Caption := sUnselectListBox;
  SelectAllMasterFilesBtn.Caption := sSelectAllListBox;
  LangFilter.Items[0] := sAllLanguages;
  SelectAllTestsBtn.Caption := sSelectAllTests;
  UnselectAllTestsBtn.Caption := sUnselectAllTests;
end;

procedure IDEMenuClicked(Sender: TObject);
begin
  ShowPoCheckerForm;
end;

procedure Register;
begin
  {$IFNDEF POCHECKERSTANDALONE}
  RegisterIDEMenuCommand(itmSecondaryTools, 'mnuPoChecker',
    rsPoChecker, nil, @IDEMenuClicked);
  {$ENDIF}
end;

end.
