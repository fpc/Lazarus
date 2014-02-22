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
  SimplePoFiles, PoFamilies, ResultDlg, pocheckerconsts;

type

  { TPoCheckerForm }

  TPoCheckerForm = class(TForm)
    FindAllPOsCheckBox: TCheckBox;
    SelectAllCheckBox: TCheckBox;
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
    procedure SelectAllCheckBoxClick(Sender: TObject);
  private
    PoFamily: TPoFamily;
    FChosenMasterName: String;
    FChosenChildName: String;
    procedure OnTestStart(const ATestName, APoFileName: String);
    procedure OnTestEnd(const ATestName: String; const ErrorCount: Integer);
    procedure FillTestListBox;
    function GetOptionsFromListBox: TPoTestOptions;
    procedure ShowError(const Msg: String);
    function TrySelectFile: Boolean;
    procedure RunSelectedTests;
    procedure ClearAndDisableStatusPanel;
  public

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
  {$IFDEF POCHECKERSTANDALONE}
  //Initializing translation
  Lang := GetEnvironmentVariableUTF8('LANG');
  if Lang = '' then
    LCLGetLanguageIDs(Lang, T);
  if Lang <> '' then
  begin
    Lang := copy(Lang, 1, 2);
    Translations.TranslateUnitResourceStrings('PoCheckerConsts', '..'+DirectorySeparator+'languages'+DirectorySeparator+'pocheckerconsts.'+Lang+'.po');
    Translations.TranslateUnitResourceStrings('LCLStrConsts', '..'+DirectorySeparator+'languages'+DirectorySeparator+'lclstrconsts.'+Lang+'.po');
  end;
  {$ENDIF}

  Caption := sGUIPoFileCheckingTool;
  SelectTestLabel.Caption := sSelectTestTypes;
  FindAllPOsCheckBox.Caption := sFindAllTranslatedPoFiles;
  OpenBtn.Caption := sOpenAPoFile;
  RunBtn.Caption := sRunSelectedTests;
  NoErrLabel.Caption := sNoErrorsFound;
  FillTestListBox;
  ClearAndDisableStatusPanel;
  NoErrLabel.Visible := False;
  CurTestHeaderLabel.Caption := sCurrentTest;
  CurPoHeaderLabel.Caption := sCurrentPoFile;
end;

procedure TPoCheckerForm.FormDestroy(Sender: TObject);
begin
  if Assigned(PoFamily) then PoFamily.Free;
end;

procedure TPoCheckerForm.OpenBtnClick(Sender: TObject);
begin
  if TrySelectFile then
  begin
    RunBtn.Enabled := True;
    TestListBox.Enabled := True;
    SelectAllCheckBox.Enabled := True;
  end
  else begin
    RunBtn.Enabled := False;
    TestListBox.Enabled := False;
    SelectAllCheckBox.Enabled := False;
  end;
end;

procedure TPoCheckerForm.RunBtnClick(Sender: TObject);
begin
  RunSelectedTests;
end;

procedure TPoCheckerForm.SelectAllCheckBoxClick(Sender: TObject);
var
  cb: TCheckBox;
  i: Integer;
begin
  cb := Sender as TCheckBox;
  // Set / reset "basic" CheckListBox items.
  for i := 0 to TestListBox.Count - 3 do
    TestListBox.Checked[i] := cb.Checked;
end;

procedure TPoCheckerForm.OnTestStart(const ATestName, APoFileName: String);
begin
  //debugln('OnTestStart: ATestName = "',AtestName,'" APoFileName = "',APoFileName);
  CurTestLabel.Caption := ATestName;
  CurPoLabel.Caption :=  APoFileName;
  Application.ProcessMessages;
end;

procedure TPoCheckerForm.OnTestEnd(const ATestName: String; const ErrorCount: Integer);
begin
  //CurTestLabel.Caption := '';
  //CurPoLabel.Caption :=  '';
  debugln('OnTestEnd [',ATestName,']: ErrorCount = ',DbgS(ErrorCount));
  //Application.ProcessMessages;
end;

procedure TPoCheckerForm.FillTestListBox;
var
  Opt: TPoTestOption;
begin
  for Opt := Low(PoTestOptionNames) to Pred(High(PoTestOptionNames)) do
    case Opt of
      ptoCheckNrOfItems: TestListBox.Items.Add(sCheckNumberOfItems);
      ptoCheckFormatArgs: TestListBox.Items.Add(sCheckForIncompatibleFormatArguments);
      ptoCheckMissingIdentifiers: TestListBox.Items.Add(sCheckMissingIdentifiers);
      ptoCheckMismatchedOriginals: TestListBox.Items.Add(sCheckForMismatchesInUntranslatedStrings);
      ptoCheckDuplicateOriginals: TestListBox.Items.Add(sCheckForDuplicateUntranslatedValues);
      ptoCheckStatistics: TestListBox.Items.Add(sCheckStatistics);
      else
        TestListBox.Items.Add(PoTestOptionNames[Opt]);
    end;
  SelectAllCheckBox.Caption := sSelectBasicTests;
end;

function TPoCheckerForm.GetOptionsFromListBox: TPoTestOptions;
var
  Opt: TPoTestOption;
  Index: Integer;
begin
  Result := [];
  for Opt := Low(TpoTestOption) to Pred(High(TPoTestOption)) do
  begin
    Index := Ord(Opt);
    if (Index < TestListBox.Count) then
    begin
      if TestListBox.Checked[Index] then
        Result := Result + [Opt];
    end;
  end;
  if FindAllPOsCheckBox.Checked then
    Result := Result + [High(TPoTestOption)];
end;

procedure TPoCheckerForm.ShowError(const Msg: String);
begin
  MessageDlg('Po-checker', Msg, mtError, [mbOk], 0);
end;

function TPoCheckerForm.TrySelectFile: Boolean;
var
  Fn: String;
  ShortFn: String;
  OK: Boolean;
begin
  NoErrLabel.Visible := False;
  OK := False;
  if OpenDialog.Execute then
  begin
    Fn := OpenDialog.FileName;
    ShortFn := ExtractFileName(Fn);
    if IsMasterPoName(Fn) then
    begin
      FChosenMasterName := Fn;
      FChosenChildName := '';
    end
    else
    begin //not a mastername, may be a child
      FChosenChildName := Fn;
      FChosenMasterName := ExtractMasterNameFromChildName(Fn);
      if (FChosenMasterName = '') then
      begin
        FChosenMasterName := '';
        FChosenChildName := '';
        ShowError(Format(sNotAProperFileName,[ShortFn]));
      end
      else if not FileExistsUtf8(FChosenMasterName) then
      begin
        FChosenMasterName := '';
        FChosenChildName := '';
        ShowError(Format(sCannotFindMaster,[ShortFn]));
      end;
    end;
    OK := (FChosenMasterName <> '');
    if OK then
    begin
      if Assigned(PoFamily) then
        PoFamily.Free;
      try
        PoFamily := TPoFamily.Create(FChosenMasterName, FChosenChildName);
        PoFamily.OnTestStart := @OnTestStart;
        PoFamily.OnTestEnd := @OnTestEnd;
      except
        on E: Exception do
        begin
          OK := False;
          ShowError(Format(sErrorOnCreate,[E.Message]));
          if Assigned(PoFamily) then
          begin
            try
              PoFamily.Free;
            except
              on E: Exception do
              begin
                ShowError(Format(sErrorOnCleanUp,[E.Message]));
              end;
            end;
          end;
        end;
      end;
    end;
  end;
  Result := OK;
end;

procedure TPoCheckerForm.RunSelectedTests;
var
  Options: TPoTestOptions;
  ErrorCount, WarningCount: Integer;
  SL: TStrings;
  ResultDlg: TResultDlgForm;
begin
  Options := GetOptionsFromListBox;
  if (Options = []) then
  begin
    ShowError(sNoTestSelected);
    Exit;
  end;
  NoErrLabel.Visible := False;
  Application.ProcessMessages;
  SL := TStringList.Create;
  try
    StatusPanel.Enabled := True;
    if (not (ptoFindAllChildren in Options)) and Assigned(PoFamily.Child)
        and (PoFamily.ChildName <> FChosenChildName) then
      PoFamily.ChildName := FChosenChildName;
    PoFamily.RunTests(Options, ErrorCount, WarningCount, SL);
    debugln('RunSelectedTests: ',Format(sTotalErrors,[ErrorCount]));
    debugln('                  ',Format(sTotalWarnings,[WarningCount]));
    if (ErrorCount > 0) or (WarningCount > 0) then
    begin
      SL.Add(Format(sTotalErrors,[ErrorCount]));
      SL.Add(Format(sTotalWarnings,[WarningCount]));
      ResultDlg := TResultDlgForm.Create(Nil);
      try
        ResultDlg.Log.Assign(SL);
        FreeAndNil(SL);                 //No need to keep 2 copies of this data
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


function SameItem(Item1, Item2: TPoFileItem): Boolean;
begin
  Result := (Item1.Identifier = Item2.Identifier) and
            (Item1.Original = Item2.Original) and
            (Item1.Context = Item2.Context) and
            (Item1.Flags = Item2.Flags) and
            (Item1.PreviousID = Item2.PreviousID) and
            (Item1.Translation = Item2.Translation);
end;

procedure IDEMenuClicked(Sender: TObject);
begin
  ShowPoCheckerForm;
end;

procedure Register;
begin
  {$IFNDEF POCHECKERSTANDALONE}
  RegisterIDEMenuCommand(itmSecondaryTools, 'mnuPoChecker', rsPoChecker, nil, @IDEMenuClicked);
  {$ENDIF}
end;

end.

