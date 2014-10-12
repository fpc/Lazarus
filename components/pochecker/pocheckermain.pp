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
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

// Original version made by Bart Broersma

unit pocheckermain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, LCLProc, CheckLst, Buttons, ExtCtrls, ComCtrls, Types,
  LCLType,
  {$IFDEF POCHECKERSTANDALONE}
  Translations,
  {$ELSE}
  IDEIntf, MenuIntf,
  {$ENDIF}
  SimplePoFiles, PoFamilies, ResultDlg, pocheckerconsts, PoCheckerSettings,
  PoFamilyLists, PoCheckerMemoDlg;

type

  { TPoCheckerForm }

  TPoCheckerForm = class(TForm)
    SelectDirectoryDialog: TSelectDirectoryDialog;
    UnselectMasterBtn: TButton;
    ClearMasterBtn: TButton;
    LangFilter: TComboBox;
    MasterPoListBox: TListBox;
    ScanDirBtn: TBitBtn;
    StatusBar: TStatusBar;
    procedure MasterPoListBoxResize(Sender: TObject);
    procedure ClearMasterBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LangFilterChange(Sender: TObject);
    procedure MasterPoListBoxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure MasterPoListBoxSelectionChange(Sender: TObject; User: boolean);
    procedure ScanDirBtnClick(Sender: TObject);
    procedure UnselectChildBtnClick(Sender: TObject);
    procedure UnselectMasterBtnClick(Sender: TObject);
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
    procedure SetTestOptionCheckBoxes(TestOptions: TPoTestOptions);
    procedure ShowError(const Msg: string);
    function TrySelectFile(out Filename: String): Boolean;
    procedure ScanDirectory(ADir: String);
    function TryCreatepoFamilyList(MasterList: TStrings; const LangID: TLangID): Boolean;
    procedure RunSelectedTests;
    procedure ClearStatusBar;
    procedure UpdateGUI(HasSelection: Boolean);
    function GetSelectedMasterFiles: TStringList;
    procedure AddToMasterPoList(Fn: String);
    procedure AddToMasterPoList(S: TStrings);
    procedure SetSelectedMasterFiles(S: TStrings);
    procedure LoadConfig;
    procedure SaveConfig;
    function LangFilterIndexToLangID(Index: Integer): TLangID;
    function LangIdToLangFilterIndex(LangID: TLangID): Integer;
    procedure PopulateLangFilter;
  published
    IgnoreFuzzyCheckBox: TCheckBox;
    UnselectAllBtn: TButton;
    SelectAllBtn: TButton;
    SelectBasicBtn: TButton;
    NoErrLabel: TLabel;
    RunBtn: TBitBtn;
    OpenBtn: TBitBtn;
    Button3: TButton;
    SelectTestLabel: TLabel;
    OpenDialog: TOpenDialog;
    TestListBox: TCheckListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OpenBtnClick(Sender: TObject);
    procedure RunBtnClick(Sender: TObject);
    procedure SelectAllBtnClick(Sender: TObject);
    procedure SelectBasicBtnClick(Sender: TObject);
    procedure UnselectAllBtnClick(Sender: TObject);
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
{$IFDEF POCHECKERSTANDALONE}
var
  Lang, T, AppPath: string;
{$ENDIF}
begin
  //debugln('TPoCheckerForm.FormCreate A:');
  {$IFDEF POCHECKERSTANDALONE}
  //Initializing translation
  Lang := GetEnvironmentVariableUTF8('LANG');
  T := '';
  if Lang = '' then
    LCLGetLanguageIDs(Lang, T);
  if Lang <> '' then
  begin
    {$ifdef windows}
    AppPath := ExtractFilePath(ParamStr(0));
    {$else}
    AppPath := '';
    {$endif}
    Lang := copy(Lang, 1, 2);
    Translations.TranslateUnitResourceStrings('PoCheckerConsts',
      AppPath + '..' + DirectorySeparator + 'languages' + DirectorySeparator +
      'pocheckerconsts.' + Lang + '.po');
    //requires the user copies the LCLStrConsts translations there!
    Translations.TranslateUnitResourceStrings('LCLStrConsts',
      AppPath + '..' + DirectorySeparator + 'languages' + DirectorySeparator +
      'lclstrconsts.' + Lang + '.po');
  end;
  {$ENDIF}
  Caption := sGUIPoFileCheckingTool;
  SelectTestLabel.Caption := sSelectTestTypes;
  //FindAllPOsCheckBox.Caption := sFindAllTranslatedPoFiles;
  IgnoreFuzzyCheckBox.Caption := sIgnoreFuzzyTranslations;
  OpenBtn.Caption := sOpenAPoFile;
  ScanDirBtn.Caption := sScanDir;
  RunBtn.Caption := sRunSelectedTests;
  ClearMasterBtn.Caption := sClearListBox;
  UnselectMasterBtn.Caption := sUnselectListBox;
  LangFilter.Items[0] := sAllLanguages;
  NoErrLabel.Caption := sNoErrorsFound;
  FillTestListBox;
  ClearStatusBar;
  NoErrLabel.Visible := False;
  SelectAllBtn.Caption := sSelectAllTests;
  SelectBasicBtn.Caption := sSelectBasicTests;
  UnselectAllBtn.Caption := sUnselectAllTests;
  PopulateLangFilter;
  LoadConfig;
  LangFilter.Invalidate; //Items[0] may have been changed
end;


procedure TPoCheckerForm.FormDestroy(Sender: TObject);
begin
  if Assigned(PoFamilyList) then
    PoFamilyList.Free;
  SaveConfig;
  if Assigned(FPoCheckerSettings) then
    FPoCheckerSettings.Free;
end;


procedure TPoCheckerForm.OpenBtnClick(Sender: TObject);
var
  Fn: String;
begin
  if TrySelectFile(Fn) then
  begin
    if IsMasterPoName(Fn) then
      AddToMasterPoList(Fn)
    else
      //AddToChildPoList(Fn);
      AddToMasterPoList(ExtractMasterNameFromChildName(Fn));
    UpdateGUI(True);
  end
  else
  begin
    UpdateGUI(False);
  end;
end;


procedure TPoCheckerForm.RunBtnClick(Sender: TObject);
var
  AMasterList: TStringList;
  LangIdx: Integer;
  ALangID: TLangID;
begin
  LangIdx := LangFilter.ItemIndex;
  ALangID := LangFilterIndexToLangID(LangIdx);
  AMasterList := GetSelectedMasterFiles;
  try
    if TryCreatePoFamilyList(AMasterList, ALangID) then
      RunSelectedTests
    else
    begin
      if Assigned(PoFamilyList) then FreeAndNil(PoFamilyList);
    end;
  finally
    AMasterList.Free;
  end;
end;

procedure TPoCheckerForm.SelectAllBtnClick(Sender: TObject);
begin
  TestListBox.CheckAll(cbChecked, False, False);
end;


procedure TPoCheckerForm.SelectBasicBtnClick(Sender: TObject);
var
  i: integer;
begin
  // Set / reset "basic" CheckListBox items.
  for i := 0 to TestListBox.Count - 3 do
    TestListBox.Checked[i] := True;
end;

procedure TPoCheckerForm.UnselectAllBtnClick(Sender: TObject);
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


procedure TPoCheckerForm.MasterPoListBoxResize(Sender: TObject);
var
  ATop: Integer;
begin
  //Can't seem to get this to work with just Anchors
  ATop := MasterPoListBox.Top + MasterPoListBox.Height;
  LangFilter.Top := ATop + 5;
end;


procedure TPoCheckerForm.ClearMasterBtnClick(Sender: TObject);
begin
  MasterPoListBox.Clear;
  UpdateGUI(False);
end;

procedure TPoCheckerForm.FormShow(Sender: TObject);
begin
  WindowState := FPoCheckerSettings.MainFormWindowState;
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
  UnselectMasterBtn.Enabled := (MasterPoListBox.SelCount <> 0);
end;

procedure TPoCheckerForm.ScanDirBtnClick(Sender: TObject);
begin
  if SelectDirectoryDialog.Execute then
  begin
    ScanDirectory(SelectDirectoryDialog.FileName);
  end;
end;

procedure TPoCheckerForm.UnselectChildBtnClick(Sender: TObject);
begin
end;

procedure TPoCheckerForm.UnselectMasterBtnClick(Sender: TObject);
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
    case Typ of
      pttCheckNrOfItems:
        TestListBox.Items.Add(sCheckNumberOfItems);
      pttCheckFormatArgs:
        TestListBox.Items.Add(sCheckForIncompatibleFormatArguments);
      pttCheckMissingIdentifiers:
        TestListBox.Items.Add(sCheckMissingIdentifiers);
      pttCheckMismatchedOriginals:
        TestListBox.Items.Add(sCheckForMismatchesInUntranslatedStrings);
      pttCheckDuplicateOriginals:
        TestListBox.Items.Add(sCheckForDuplicateUntranslatedValues);
      pttCheckStatistics:
        TestListBox.Items.Add(sCheckStatistics);
      else
        TestListBox.Items.Add(PoTestTypeNames[Typ]);
    end;
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
begin
  Result := [];
  //if FindAllPOsCheckBox.Checked then
  //  Result := Result + [ptoFindAllChildren];
  if IgnoreFuzzyCheckBox.Checked then
    Result := Result + [ptoIgnoreFuzzyStrings];
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

procedure TPoCheckerForm.SetTestOptionCheckBoxes(TestOptions: TPoTestOptions);
begin
  //FindAllPOsCheckBox.Checked := (ptoFindAllChildren in TestOptions);
  IgnoreFuzzyCheckBox.Checked := (ptoIgnoreFuzzyStrings in TestOptions);
end;


procedure TPoCheckerForm.ShowError(const Msg: string);
begin
  MessageDlg('Po-checker', Msg, mtError, [mbOK], 0);
end;


function TPoCheckerForm.TrySelectFile(out Filename: String): boolean;
begin
  Result := False;
  Filename := '';
  if OpenDialog.Execute then
  begin
    Filename := OpenDialog.FileName;
    Result := (CompareText(ExtractFileExt(Filename), '.po') = 0);
    if not Result then Filename := '';
  end;
end;

procedure TPoCheckerForm.ScanDirectory(ADir: String);
var
  SL, ML: TStringList;
  i: Integer;
  S: String;
begin
  SL := FindAllFiles(ADir, '*.po',True);
  try
    ML := TStringList.Create;
    for i := 0 to SL.Count - 1 do
    begin
      S := SL[i];
      //debugln('TPoCheckerForm.ScanDirectory: S = "',ExtractFilename(S),'"');
      if IsMasterPoName(S) then
        ML.Add(S);
    end;
    if (ML.Count > 0) then AddToMasterPoList(ML);
  finally
    SL.Free;
    ML.Free;
  end;
end;


function TPoCheckerForm.TryCreatepoFamilyList(MasterList: TStrings; const LangID: TLangID): Boolean;
var
  Fn, Msg: String;
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
  if (Msg <> '') then
    //MessageDlg('PoChecker',Format(sFilesNotFoundAndRemoved,[Msg]), mtInformation, [mbOk], 0);
    Msg := Format(sFilesNotFoundAndRemoved,[Msg]);
  Cnt := MasterList.Count;
  if (Cnt = 0) then
    Msg := Msg + LineEnding + LineEnding + LineEnding + sNoFilesLeftToCheck;
  if (Msg <> '') then
    MemoDlg('PoChecker',Msg);
  if (Cnt = 0) then
  begin
    //MessageDlg('PoChecker', sNoFilesLeftToCheck, mtInformation, [mbOk], 0);
    Exit;
  end;
  try
    if Assigned(PoFamilyList) then PoFamilyList.Free;
    PoFamilyList := TPoFamilyList.Create(MasterList, LangID, Msg);
    if (Msg <> '') then
    begin
      //MessageDlg('PoChecker',Format(sFilesNotFoundAndRemoved,[Msg]), mtInformation, [mbOk], 0);
      Msg := Format(sFilesNotFoundAndRemoved,[Msg]);
      if (PoFamilyList.Count = 0) then
        Msg := Msg + LineEnding + LineEnding + LineEnding + sNoFilesLeftToCheck;
      if (Msg <> '') then
        MemoDlg('PoChecker',Msg);
      if (PoFamilyList.Count = 0) then
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


procedure TPoCheckerForm.RunSelectedTests;
var
  TestTypes: TPoTestTypes;
  TestOptions: TPoTestOptions;
  ErrorCount, WarningCount: integer;
  SL: TStrings;
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
  NoErrLabel.Visible := False;
  Application.ProcessMessages;
  SL := TStringList.Create;
  mr := mrNone;
  try

    PoFamilyList.RunTests(TestTypes, TestOptions, ErrorCount, WarningCount, SL);
    //debugln('RunSelectedTests: ', Format(sTotalErrors, [ErrorCount]));
    //debugln('                  ', Format(sTotalWarnings, [WarningCount]));
    if (ErrorCount > 0) or (WarningCount > 0) or
      (pttCheckStatistics in TestTypes) then
    begin
      SL.Add(Format(sTotalErrors, [ErrorCount]));
      SL.Add(Format(sTotalWarnings, [WarningCount]));
      ResultDlg := TResultDlgForm.Create(nil);
      try
        ResultDlg.Log.Assign(SL);
        FreeAndNil(SL);                 //No need to keep 2 copies of this data
        if (pttCheckStatistics in TestTypes) then
          ResultDlg.PoFamilyStats := PoFamilyList.PoFamilyStats
        else
          ResultDlg.PoFamilyStats := nil;
        ResultDlg.Settings := FPoCheckerSettings;
        mr := ResultDlg.ShowModal;
      finally
        ResultDlg.Free;
      end;
    end;
    NoErrLabel.Visible := (ErrorCount = 0);
  finally
    if Assigned(SL) then
      SL.Free;
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
  NoErrLabel.Visible := False;
  if HasSelection then
  begin
    RunBtn.Enabled := True;
    TestListBox.Enabled := True;
    SelectAllBtn.Enabled := True;
    SelectBasicBtn.Enabled := True;
    UnselectAllBtn.Enabled := True;
    UnselectMasterBtn.Enabled := True;
  end
  else
  begin
    RunBtn.Enabled := False;
    TestListBox.Enabled := False;
    SelectAllBtn.Enabled := False;
    SelectBasicBtn.Enabled := False;
    UnselectAllBtn.Enabled := False;
    UnselectMasterBtn.Enabled := False;
  end;
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

procedure TPoCheckerForm.AddToMasterPoList(Fn: String);
var
  Idx: Integer;
begin
  if not FileExistsUtf8(Fn) then Exit;
  Idx := MasterPoListBox.Items.IndexOf(Fn);
  if (Idx = -1) then
  begin
    MasterPoListBox.Items.Add(Fn);
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
      if FileExistsUtf8(Str) and IsMasterPoName(Str) then
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


procedure TPoCheckerForm.LoadConfig;
var
  ARect: TRect;
  Abbr: String;
  ID: TLangID;
begin
  FPoCheckerSettings := TPoCheckerSettings.Create;
  FPoCheckerSettings.LoadConfig;
  ARect := FPoCheckerSettings.MainFormGeometry;
  if not IsDefaultRect(ARect) and IsValidRect(ARect) then
  begin
    ARect := FitToRect(ARect, Screen.WorkAreaRect);
    BoundsRect := ARect;
  end;
  SetTestTypeCheckBoxes(FPoCheckerSettings.TestTypes);
  SetTestOptionCheckBoxes(FPoCheckerSettings.TestOptions);
  SelectDirectoryDialog.Filename := FPoCheckerSettings.SelectDirectoryFilename;
  OpenDialog.FileName := FPoCheckerSettings.OpenDialogFilename;
  Abbr := FPoCheckerSettings.LangFilterLanguageAbbr;
  ID := LangAbbrToLangId(Abbr);
  LangFilter.ItemIndex := LangIdToLangFilterIndex(ID);
  AddToMasterPoList(FPoCheckerSettings.MasterPoList);
  SetSelectedMasterFiles(FPoCheckerSettings.MasterPoSelList);
end;

procedure TPoCheckerForm.SaveConfig;
var
  SL: TStringList;
  ID: TLangID;
begin
  FPoCheckerSettings.SelectDirectoryFilename := SelectDirectoryDialog.Filename;
  FPoCheckerSettings.OpenDialogFilename := OpenDialog.FileName;
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
      S := Format('%s  [%s]',[LangName, Abbr]);
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


function SameItem(Item1, Item2: TPoFileItem): boolean;
begin
  Result := (Item1.Identifier = Item2.Identifier) and
    (Item1.Original = Item2.Original) and (Item1.Context = Item2.Context) and
    (Item1.Flags = Item2.Flags) and (Item1.PreviousID = Item2.PreviousID) and
    (Item1.Translation = Item2.Translation);
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
