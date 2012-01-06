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
  StdCtrls, LCLProc, CheckLst, Buttons, ExtCtrls, IDEIntf, MenuIntf,
  SimplePoFiles, PoFamilies, ResultDlg;

type

  { TPoCheckerForm }

  TPoCheckerForm = class(TForm)
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
    FChoosenMasterName: String;
    FChoosenChildName: String;
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

resourcestring
  rsPoChecker = 'PO File Checker';
  sSelectAllTests = 'Select all tests';
  sUnSelectAllTests = 'Unselect all tests';
  sCannotFindMaster = 'Cannot find master po file:' + LineEnding + '%s' + LineEnding + 'for selected file' + LineEnding + '%s';
  sNotAProperFileName = 'Selected filename' + LineEnding + '%s' + LineEnding + 'does not seem to be a proper name for a po-file';
  sErrorOnCreate = 'Error creating an instance of TPoFamily:' + LineEnding + '%s';
  sErrorOnCleanup = 'An unrecoverable error occurred' + LineEnding + '%s' + LineEnding + 'Please close the program';

  sTotalErrors = 'Total errors / warnings found: %d';
  //sNoErrorsFound = 'No errors found.';
  sNoTestSelected = 'There are no tests selected.';

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
  FillTestListBox;
  ClearAndDisableStatusPanel;
  NoErrLabel.Visible := False;
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
  // Caption text : select all / unselect all
  if cb.Checked then
    cb.Caption := sUnSelectAllTests
  else
    cb.Caption := sSelectAllTests;
  // Set / reset all CheckListBox items.
  for i := 0 to TestListBox.Count - 1 do
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
  for Opt := Low(PoTestOptionNames) to High(PoTestOptionNames) do
    TestListBox.Items.Add(PoTestOptionNames[Opt]);
  SelectAllCheckBox.Caption := sSelectAllTests;
end;

function TPoCheckerForm.GetOptionsFromListBox: TPoTestOptions;
var
  Opt: TPoTestOption;
  Index: Integer;
begin
  Result := [];
  for Opt := Low(TpoTestOption) to High(TPoTestOption) do
  begin
    Index := Ord(Opt);
    if (Index < TestListBox.Count) then
    begin
      if TestListBox.Checked[Index] then
        Result := Result + [Opt];
    end;
  end;
end;

procedure TPoCheckerForm.ShowError(const Msg: String);
begin
  MessageDlg('GPoCheck', Msg, mtError, [mbOk], 0);
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
      FChoosenMasterName := Fn;
      FChoosenChildName := '';
    end
    else
    begin //not a mastername, may be a child
      FChoosenChildName := Fn;
      FChoosenMasterName := ExtractMasterNameFromChildName(Fn);
      if (FChoosenMasterName = '') then
      begin
        FChoosenMasterName := '';
        FChoosenChildName := '';
        ShowError(Format(sNotAProperFileName,[ShortFn]));
      end
      else if not FileExistsUtf8(FChoosenMasterName) then
      begin
        FChoosenMasterName := '';
        FChoosenChildName := '';
        ShowError(Format(sCannotFindMaster,[ShortFn]));
      end;
    end;
    OK := (FChoosenMasterName <> '');
    if OK then
    begin
      if Assigned(PoFamily) then
        PoFamily.Free;
      try
        PoFamily := TPoFamily.Create(FChoosenMasterName, FChoosenChildName);
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
  ErrorCount: Integer;
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
    if (not (ptoFindAllChilds in Options)) and Assigned(PoFamily.Child)
        and (PoFamily.ChildName <> FChoosenChildName) then
      PoFamily.ChildName := FChoosenChildName;
    PoFamily.RunTests(Options, ErrorCount, SL);
    if (ErrorCount > 0) then
    begin
      debugln('RunSelectedTests: ',Format(sTotalErrors,[ErrorCount]));
      SL.Add(Format(sTotalErrors,[ErrorCount]));
      ResultDlg := TResultDlgForm.Create(Nil);
      try
        ResultDlg.Log.Assign(SL);
        FreeAndNil(SL);                 //No need to keep 2 copies of this data
        ResultDlg.ShowModal;
      finally
        ResultDlg.Free;
      end;
    end
    else  //no errors
      NoErrLabel.Visible := True;
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
  RegisterIDEMenuCommand(itmSecondaryTools, 'mnuPoChecker', rsPoChecker, nil, @IDEMenuClicked);
end;

end.

