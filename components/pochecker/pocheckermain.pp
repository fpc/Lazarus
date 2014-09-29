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
  StdCtrls, LCLProc, CheckLst, Buttons, ExtCtrls,
  {$IFDEF POCHECKERSTANDALONE}
  Translations,
  {$ELSE}
  IDEIntf, MenuIntf,
  {$ENDIF}
  SimplePoFiles, PoFamilies, ResultDlg, pocheckerconsts, PoCheckerSettings;

type

  { TPoCheckerForm }

  TPoCheckerForm = class(TForm)
  private
    PoFamily: TPoFamily;
    FSelectedPoName: String;
    FNewRun: boolean;
    FPoCheckerSettings: TPoCheckerSettings;
    procedure OnTestStart(const ATestName, APoFileName: string);
    procedure OnTestEnd(const ATestName: string; const ErrorCount: integer);
    procedure FillTestListBox;
    function GetTestTypesFromListBox: TPoTestTypes;
    function GetTestOptions: TPoTestOptions;
    procedure SetTestTypeCheckBoxes(TestTypes: TPoTestTypes);
    procedure SetTestOptionCheckBoxes(TestOptions: TPoTestOptions);
    procedure ShowError(const Msg: string);
    function TrySelectFile(out Filename: String): Boolean;
    function TryCreatePoFamily(Filename: String): Boolean;
    procedure RunSelectedTests;
    procedure ClearAndDisableStatusPanel;
    procedure SetSelectedPoName(AFilename: String);
    procedure LoadConfig;
    procedure SaveConfig;
  published
    IgnoreFuzzyCheckBox: TCheckBox;
    UnselectAllBtn: TButton;
    SelectAllBtn: TButton;
    SelectBasicBtn: TButton;
    FindAllPOsCheckBox: TCheckBox;
    CurTestHeaderLabel: TLabel;
    CurPoHeaderLabel: TLabel;
    CurTestLabel: TLabel;
    CurPoLabel: TLabel;
    NoErrLabel: TLabel;
    StatusPanel: TPanel;
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

  Caption := sGUIPoFileCheckingTool;
  SelectTestLabel.Caption := sSelectTestTypes;
  FindAllPOsCheckBox.Caption := sFindAllTranslatedPoFiles;
  IgnoreFuzzyCheckBox.Caption := sIgnoreFuzzyTranslations;
  OpenBtn.Caption := sOpenAPoFile;
  RunBtn.Caption := sRunSelectedTests;
  NoErrLabel.Caption := sNoErrorsFound;
  FillTestListBox;
  ClearAndDisableStatusPanel;
  NoErrLabel.Visible := False;
  CurTestHeaderLabel.Caption := sCurrentTest;
  CurPoHeaderLabel.Caption := sCurrentPoFile;
  SelectAllBtn.Caption := sSelectAllTests;
  SelectBasicBtn.Caption := sSelectBasicTests;
  UnselectAllBtn.Caption := sUnselectAllTests;
  LoadConfig;
end;


procedure TPoCheckerForm.FormDestroy(Sender: TObject);
begin
  if Assigned(PoFamily) then
    PoFamily.Free;
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
    SetSelectedPoName(Fn);
  end
  else
  begin
    SetSelectedPoName('');
  end;
 FNewRun:= False;
end;


procedure TPoCheckerForm.RunBtnClick(Sender: TObject);
begin
  if FNewRun then TryCreatePoFamily(FSelectedPoName);
  RunSelectedTests;
  FNewRun:= True;
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


procedure TPoCheckerForm.OnTestStart(const ATestName, APoFileName: string);
begin
  //debugln('OnTestStart: ATestName = "',AtestName,'" APoFileName = "',APoFileName);
  CurTestLabel.Caption := ATestName;
  CurPoLabel.Caption := APoFileName;
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
  NoErrLabel.Visible := False;
  Result := False;
  Filename := '';
  if OpenDialog.Execute then
  begin
    Filename := OpenDialog.FileName;
    Result := TryCreatePoFamily(Filename);
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
  try
    StatusPanel.Enabled := True;
    if (not (ptoFindAllChildren in TestOptions)) and Assigned(PoFamily.Child) and
      (PoFamily.ChildName <> FSelectedPoName) then
      PoFamily.ChildName := FSelectedPoName;
    PoFamily.RunTests(TestTypes, TestOptions, ErrorCount, WarningCount, SL);
    debugln('RunSelectedTests: ', Format(sTotalErrors, [ErrorCount]));
    debugln('                  ', Format(sTotalWarnings, [WarningCount]));
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
        ResultDlg.ShowModal;
      finally
        ResultDlg.Free;
      end;
    end;
    NoErrLabel.Visible := (ErrorCount = 0);
  finally
    if Assigned(SL) then
      SL.Free;
    ClearAndDisableStatusPanel;
  end;
end;


procedure TPoCheckerForm.ClearAndDisableStatusPanel;
begin
  CurTestLabel.Caption := '';
  CurPoLabel.Caption := '';
  StatusPanel.Enabled := False;
end;

procedure TPoCheckerForm.SetSelectedPoName(AFilename: String);
begin
  if (FSelectedPoName = AFilename) then Exit;
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
  end
  else
  begin
    RunBtn.Enabled := False;
    TestListBox.Enabled := False;
    SelectAllBtn.Enabled := False;
    SelectBasicBtn.Enabled := False;
    UnselectAllBtn.Enabled := False;
    Caption := sGUIPoFileCheckingTool;
  end;
end;


procedure TPoCheckerForm.LoadConfig;
function IsSaneRect(ARect: TRect): Boolean;
const
  MinWH = 50; //arbitrary
begin
  Result := (ARect.Right > ARect.Left + MinWH) and
            (ARect.Bottom > ARect.Bottom + MinWH);
end;
var
  ARect: TRect;
begin
  FPoCheckerSettings := TPoCheckerSettings.Create;
  FPoCheckerSettings.LoadConfig;
  ARect := FPoCheckerSettings.MainFormGeometry;
  if IsSaneRect(ARect) then BoundsRect := ARect;

  //DebugLn('  TestOptions after loading = ');
  //DebugLn('  ',DbgS(FPoCheckerSettings.TestOptions));
  //debugln('  TPoCheckerForm.FormCreate: TestTypes   after loading = ');
  //DebugLn('  ',DbgS(FPoCheckerSettings.TestTypes));
  SetTestTypeCheckBoxes(FPoCheckerSettings.TestTypes);
  SetTestOptionCheckBoxes(FPoCheckerSettings.TestOptions);
  if (FPoCheckerSettings.LastSelectedFile <> '') then
  begin
    //debugln('Trying to load ',FPoCheckerSettings.LastSelectedFile);
    if TryCreatePoFamily(FPoCheckerSettings.LastSelectedFile) then
      SetSelectedPoName(FPoCheckerSettings.LastSelectedFile)
    else
      SetSelectedPoName('');
  end;
end;

procedure TPoCheckerForm.SaveConfig;
begin
  FPoCheckerSettings.SaveSettingsOnExit := True; //ToDo: create a checkbox for this
  FPoCheckerSettings.LastSelectedFile := FSelectedPoName;
  FPoCheckerSettings.TestTypes := GetTestTypesFromListBox;
  FPoCheckerSettings.TestOptions := GetTestOptions;
  FPoCheckerSettings.MainFormGeometry := BoundsRect;
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
