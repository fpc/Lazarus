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
  StdCtrls, LCLProc, CheckLst, Buttons, ExtCtrls, ComCtrls, Masks, Types,
  LCLType,
  {$IFDEF POCHECKERSTANDALONE}
  Translations,
  {$ELSE}
  IDEIntf, MenuIntf,
  {$ENDIF}
  SimplePoFiles, PoFamilies, ResultDlg, pocheckerconsts, PoCheckerSettings;

type

  { TPoCheckerForm }

  TPoCheckerForm = class(TForm)
    SelectDirectoryDialog: TSelectDirectoryDialog;
    UnselectChildBtn: TButton;
    ClearChildBtn: TButton;
    UnselectMasterBtn: TButton;
    ClearMasterBtn: TButton;
    LangFilter: TComboBox;
    ChildPoListBox: TListBox;
    MasterPoListBox: TListBox;
    ScanDirBtn: TBitBtn;
    StatusBar: TStatusBar;
    procedure ChildPoListBoxResize(Sender: TObject);
    procedure ChildPoListBoxSelectionChange(Sender: TObject; User: boolean);
    procedure ClearChildBtnClick(Sender: TObject);
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
    PoFamily: TPoFamily;
    FSelectedPoName: String;
    FPoCheckerSettings: TPoCheckerSettings;
    FChildPoList: TStringList;
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
    function TryCreatePoFamily(Filename: String): Boolean;
    procedure RunSelectedTests;
    procedure ClearStatusBar;
    procedure SetSelectedPoName(AFilename: String);
    procedure AddToMasterPoList(Fn: String);
    procedure AddToMasterPoList(S: TStrings);
    procedure AddToChildPoList(Fn: String);
    procedure AddToChildPoList(S: TStrings);
    procedure LoadConfig;
    procedure SaveConfig;
  published
    IgnoreFuzzyCheckBox: TCheckBox;
    UnselectAllBtn: TButton;
    SelectAllBtn: TButton;
    SelectBasicBtn: TButton;
    FindAllPOsCheckBox: TCheckBox;
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
  Lang, T: string;
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
    Lang := copy(Lang, 1, 2);
    Translations.TranslateUnitResourceStrings('PoCheckerConsts',
      '..' + DirectorySeparator + 'languages' + DirectorySeparator +
      'pocheckerconsts.' + Lang + '.po');
    Translations.TranslateUnitResourceStrings('LCLStrConsts',
      '..' + DirectorySeparator + 'languages' + DirectorySeparator +
      'lclstrconsts.' + Lang + '.po');
  end;
  {$ENDIF}
  FChildPoList := TStringList.Create;
  FChildPoList.Sorted := True;
  FChildPoList.Duplicates := dupIgnore;
  Caption := sGUIPoFileCheckingTool;
  SelectTestLabel.Caption := sSelectTestTypes;
  FindAllPOsCheckBox.Caption := sFindAllTranslatedPoFiles;
  IgnoreFuzzyCheckBox.Caption := sIgnoreFuzzyTranslations;
  OpenBtn.Caption := sOpenAPoFile;
  ScanDirBtn.Caption := sScanDir;
  RunBtn.Caption := sRunSelectedTests;
  ClearMasterBtn.Caption := sClearListBox;
  ClearChildBtn.Caption := sClearListBox;
  UnselectMasterBtn.Caption := sUnselectListBox;
  UnselectChildBtn.Caption := sUnselectListBox;
  LangFilter.Items[0] := sAllLanguages;
  NoErrLabel.Caption := sNoErrorsFound;
  FillTestListBox;
  ClearStatusBar;
  NoErrLabel.Visible := False;
  SelectAllBtn.Caption := sSelectAllTests;
  SelectBasicBtn.Caption := sSelectBasicTests;
  UnselectAllBtn.Caption := sUnselectAllTests;
  LoadConfig;
  LangFilter.Invalidate; //Items[0] may have been changed
end;


procedure TPoCheckerForm.FormDestroy(Sender: TObject);
begin
  if Assigned(PoFamily) then
    PoFamily.Free;
  SaveConfig;
  if Assigned(FPoCheckerSettings) then
    FPoCheckerSettings.Free;
  FChildPoList.Free;
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
      AddToChildPoList(Fn);
    SetSelectedPoName(Fn);
  end
  else
  begin
    SetSelectedPoName('');
  end;
end;


procedure TPoCheckerForm.RunBtnClick(Sender: TObject);
begin
  if TryCreatePoFamily(FSelectedPoName) then
    RunSelectedTests
  else
  begin
    if Assigned(PoFamily) then FreeAndNil(PoFamily);
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
var
  Filter, Mask, PrevItem: String;
  Idx, i: Integer;
begin
  Idx := ChildPoListBox.ItemIndex;
  if (Idx > -1) then
    PrevItem := ChildPoListBox.Items[Idx]
  else
    PrevItem := '';
  //debugln('TPoCheckerForm.LangFilterChange: PrevItem = "',PrevItem,'"');
  Filter := LangFilter.Items.Strings[LangFilter.ItemIndex];
  if (LangFilter.ItemIndex > 0) then
  begin
    Mask := '*.'+Filter+'.po';
    ChildPoListBox.Clear;
    ChildPoListBox.Items.BeginUpdate;
    try
    for i := 0 to FChildPoList.Count - 1 do
      begin
        if MatchesMask(FChildPoList.Strings[i], Mask, False) then
          ChildPoListBox.Items.Add(FChildPoList.Strings[i]);
      end;
    finally
      ChildPoListBox.Items.EndUpdate;
    end;
  end
  else
    ChildPoListBox.Items.Assign(FChildPoList);
  if (Idx <> -1) then
  begin
    Idx := ChildPoListBox.Items.IndexOf(PrevItem);
    debugln('TPoCheckerForm.LangFilterChange: IndexOf(PrevItem) = ',DbgS(Idx));
    if (Idx <> -1) then
    begin
      ChildPoListBox.ItemIndex := Idx;
    end
    else
    begin
      if (MasterPoListBox.ItemIndex = -1) then
        SetSelectedPoName('');
    end;
  end;
  UnselectChildBtn.Enabled := (Idx <> -1);
end;

procedure TPoCheckerForm.ChildPoListBoxSelectionChange(Sender: TObject;
  User: boolean);
begin
  //debugln('TPoCheckerForm.ChildPoListBoxSelectionChange: User = ',DbgS(User));
  if User then
  begin
    SetSelectedPoName(ChildPoListBox.Items[ChildPoListBox.ItemIndex]);
  end;
  UnselectChildBtn.Enabled := (ChildPoListBox.ItemIndex <> -1);
end;

procedure TPoCheckerForm.ChildPoListBoxResize(Sender: TObject);
begin
  //Can't seem to get this to work with just Anchors
  LangFilter.Top := ChildPoListBox.Top + ChildPoListBox.Height + 10;
end;

procedure TPoCheckerForm.ClearChildBtnClick(Sender: TObject);
begin
  FChildPoList.Clear;
  LangFilterChange(LangFilter);
  if (MasterPoListBox.ItemIndex = -1) then
    SetSelectedPoName('');
end;

procedure TPoCheckerForm.ClearMasterBtnClick(Sender: TObject);
begin
  MasterPoListBox.Clear;
  if (ChildPoListBox.ItemIndex = -1) then
    SetSelectedPoName('');
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
    SetSelectedPoName(MasterPoListBox.Items[MasterPoListBox.ItemIndex]);
  end;
  UnselectMasterBtn.Enabled := (MasterPoListBox.ItemIndex <> -1);
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
  ChildpoListBox.ItemIndex := -1;
  if (MasterPoListBox.ItemIndex = -1) then
    SetSelectedPoName('');
end;

procedure TPoCheckerForm.UnselectMasterBtnClick(Sender: TObject);
begin
  MasterPoListBox.ItemIndex := -1;
  if (ChildPoListBox.ItemIndex = -1) then
    SetSelectedPoName('');
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
  if FindAllPOsCheckBox.Checked then
    Result := Result + [ptoFindAllChildren];
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
  FindAllPOsCheckBox.Checked := (ptoFindAllChildren in TestOptions);
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
  SL, ML, CL: TStringList;
  i: Integer;
  S: String;
begin
  SL := FindAllFiles(ADir, '*.po',True);
  try
    ML := TStringList.Create;
    CL := TStringList.Create;
    for i := 0 to SL.Count - 1 do
    begin
      S := SL[i];
      //debugln('TPoCheckerForm.ScanDirectory: S = "',ExtractFilename(S),'"');
      if IsMasterPoName(S) then
        ML.Add(S)
      else
        CL.Add(S);
    end;
    if (ML.Count > 0) then AddToMasterPoList(ML);
    if (CL.Count > 0) then AddToChildPoList(CL);
  finally
    SL.Free;
    ML.Free;
    CL.Free;
  end;
end;

function TPoCheckerForm.TryCreatePoFamily(Filename: String): Boolean;
var
  ChosenMasterName, ChosenChildName, ShortFn: String;
begin
  Result := False;
  ShortFn := ExtractFileName(Filename);
  if IsMasterPoName(Filename) then
  begin
    ChosenMasterName := Filename;
    ChosenChildName := '';
  end
  else
  begin //not a mastername, may be a child
    ChosenChildName := Filename;
    ChosenMasterName := ExtractMasterNameFromChildName(Filename);
    if (ChosenMasterName = '') then
    begin
      ChosenMasterName := '';
      ChosenChildName := '';
      ShowError(Format(sNotAProperFileName, [ShortFn]));
    end
    else
    if not FileExistsUtf8(ChosenMasterName) then
    begin
      ShowError(Format(sCannotFindMaster,
        [ExtractFileName(ChosenMasterName), ShortFn]));
      ChosenMasterName := '';
      ChosenChildName := '';
    end;
  end;
  Result := (ChosenMasterName <> '');
  if Result then
  begin
    if Assigned(PoFamily) then
      PoFamily.Free;
    try
      PoFamily := TPoFamily.Create(ChosenMasterName, ChosenChildName);
      PoFamily.OnTestStart := @OnTestStart;
      PoFamily.OnTestEnd := @OnTestEnd;
    except
      on E: Exception do
      begin
        Result := False;
        ShowError(Format(sErrorOnCreate, [E.Message]));
        if Assigned(PoFamily) then
        begin
          try
            PoFamily.Free;
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
    if (not (ptoFindAllChildren in TestOptions)) and Assigned(PoFamily.Child) and
      (PoFamily.ChildName <> FSelectedPoName) then
      PoFamily.ChildName := FSelectedPoName;
    PoFamily.RunTests(TestTypes, TestOptions, ErrorCount, WarningCount, SL);
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
          ResultDlg.PoFamilyStats := PoFamily.PoFamilyStats
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

procedure TPoCheckerForm.SetSelectedPoName(AFilename: String);
var
  Idx: Integer;
begin
  //debugln('TPoCheckerForm.SetSelectedPoName: ((FSelectedPoName = AFilename)) = ',DbgS((FSelectedPoName = AFilename)));
  NoErrLabel.Visible := False;
  //if (FSelectedPoName = AFilename) then Exit;
  FSelectedPoName := AFilename;
  if (AFilename <> '') then
  begin
    RunBtn.Enabled := True;
    TestListBox.Enabled := True;
    SelectAllBtn.Enabled := True;
    SelectBasicBtn.Enabled := True;
    UnselectAllBtn.Enabled := True;
    Caption := sGUIPoFileCheckingTool + ' [' + ExtractFileName(AFilename) + ']';
    OpenDialog.InitialDir:= ExtractFileDir(AFilename);
    if IsMasterPoName(AFilename) then
    begin
      Idx := MasterPoListBox.Items.IndexOf(AFilename);
      MasterPoListBox.ItemIndex := Idx;
      ChildPoListBox.ItemIndex := -1;
    end
    else
    begin
      Idx := ChildPoListBox.Items.IndexOf(AFilename);
      ChildPoListBox.ItemIndex := Idx;
      MasterPoListBox.ItemIndex := -1;
    end;
  end
  else
  begin
    RunBtn.Enabled := False;
    TestListBox.Enabled := False;
    SelectAllBtn.Enabled := False;
    SelectBasicBtn.Enabled := False;
    UnselectAllBtn.Enabled := False;
    Caption := sGUIPoFileCheckingTool;
    ChildPoListBox.ItemIndex := -1;
    MasterPoListBox.ItemIndex := -1;
  end;
end;

procedure TPoCheckerForm.AddToMasterPoList(Fn: String);
var
  Idx: Integer;
begin
  Idx := MasterPoListBox.Items.IndexOf(Fn);
  if (Idx = -1) then
  begin
    MasterPoListBox.Items.Add(Fn);
  end;
end;

procedure TPoCheckerForm.AddToMasterPoList(S: TStrings);
var
  i, Idx: Integer;
  Str, PrevItem: String;
begin
  Idx := MasterPoListBox.ItemIndex;
  if (Idx <> -1) then
    PrevItem := MasterPoListBox.Items[Idx]
  else
    PrevItem := '';
  MasterPoListBox.Items.BeginUpdate;
  try
    for i := 0 to S.Count - 1 do
    begin
      Str := S[i];
      Idx := MasterPoListBox.Items.IndexOf(Str);
      if (Idx = -1) then
        MasterPoListBox.Items.Add(Str);
    end;
    if (PrevItem <> '') then
    begin
      Idx := MasterPoListBox.Items.IndexOf(PrevItem);
      MasterPoListBox.ItemIndex := Idx;
    end;
  finally
    MasterPoListBox.Items.EndUpdate;
  end;
end;

procedure TPoCheckerForm.AddToChildPoList(Fn: String);
var
  Idx: Integer;
begin
  Idx := FChildPoList.Add(Fn);
  if (Idx > -1) then
    LangFilterChange(LangFilter);
end;

procedure TPoCheckerForm.AddToChildPoList(S: TStrings);
var
  i, Idx: Integer;
  Str: String;
begin
  for i := 0 to S.Count - 1 do
  begin
    Str := S[i];
    Idx := FChildPoList.IndexOf(Str);
    if (Idx = -1) then
      FChildPoList.Add(Str);
  end;
  LangFilterChange(LangFilter);
end;


procedure TPoCheckerForm.LoadConfig;
var
  ARect: TRect;
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
  AddToMasterPoList(FPoCheckerSettings.MasterPoList);
  AddToChildPoList(FPoCheckerSettings.ChildPoList);
  if (CompareText(ExtractFileExt(FPoCheckerSettings.LastSelectedFile), '.po') = 0) then
  begin
    if IsMasterPoName(FPoCheckerSettings.LastSelectedFile) then
      AddToMasterPoList(FPoCheckerSettings.LastSelectedFile)
    else
      AddToChildPoList(FPoCheckerSettings.LastSelectedFile);
    SetSelectedPoName(FPoCheckerSettings.LastSelectedFile)
  end
  else
    SetSelectedPoName('');
end;

procedure TPoCheckerForm.SaveConfig;
begin
  FPoCheckerSettings.LastSelectedFile := FSelectedPoName;
  FPoCheckerSettings.SelectDirectoryFilename := SelectDirectoryDialog.Filename;
  FPoCheckerSettings.OpenDialogFilename := OpenDialog.FileName;
  FPoCheckerSettings.TestTypes := GetTestTypesFromListBox;
  FPoCheckerSettings.TestOptions := GetTestOptions;
  FPoCheckerSettings.MainFormWindowState := WindowState;
  if (WindowState = wsNormal) then
    FPoCheckerSettings.MainFormGeometry := BoundsRect
  else
    FPoCheckerSettings.MainFormGeometry := Rect(RestoredLeft, RestoredTop, RestoredLeft + RestoredWidth, RestoredTop + RestoredHeight);
  FPoCheckerSettings.MasterPoList := MasterPoListBox.Items;
  FPoCheckerSettings.ChildPoList := FChildPoList;
  FPoCheckerSettings.SaveConfig;
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
