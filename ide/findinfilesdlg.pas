{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Find in files modal dialog form.

}
unit FindInFilesDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  Controls, StdCtrls, Forms, Buttons, ExtCtrls, Dialogs, ButtonPanel,
  // LazUtils
  LazFileUtils, LazLoggerBase,
  // SynEdit
  SynEditTypes, SynEdit,
  // IdeIntf
  MacroIntf, IDEWindowIntf, SrcEditorIntf, IDEDialogs,
  IdeIntfStrConsts, ProjectGroupIntf, InputHistory,
  // IdeUtils
  IdeUtilsPkgStrConsts,
  // IdeConfig
  EnvironmentOpts, SearchPathProcs,
  // IDE
  LazarusIDEStrConsts, InputhistoryWithSearchOpt, EditorOptions, Project,
  SearchFrm, SearchResultView;

type
  { TLazFindInFilesDialog }

  TLazFindInFilesDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    ReplaceCheckBox: TCheckBox;
    ReplaceTextComboBox: TComboBox;
    IncludeSubDirsCheckBox: TCheckBox;
    FileMaskComboBox: TComboBox;
    DirectoriesBrowse: TBitBtn;
    DirectoriesComboBox: TComboBox;
    DirectoriesLabel: TLabel;
    FileMaskLabel: TLabel;
    DirectoriesOptionsGroupBox: TGroupBox;
    OptionsCheckGroupBox: TCheckGroup;
    SelectDirectoryDialog: TSelectDirectoryDialog;
    TextToFindComboBox: TComboBox;
    TextToFindLabel: TLabel;
    WhereRadioGroup: TRadioGroup;
    procedure DirectoriesBrowseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OKButtonClick(Sender : TObject);
    procedure ReplaceCheckBoxChange(Sender: TObject);
    procedure WhereRadioGroupClick(Sender: TObject);
  private
    FProject: TProject;
    function GetFindText: string;
    function GetOptions: TLazFindInFileSearchOptions;
    function GetReplaceText: string;
    function GetSynOptions: TSynSearchOptions;
    procedure SetFindText(const NewFindText: string);
    procedure SetOptions(NewOptions: TLazFindInFileSearchOptions);
    procedure SetReplaceText(const AValue: string);
    procedure SetSynOptions(NewOptions: TSynSearchOptions);
    procedure UpdateReplaceCheck;
    procedure UpdateDirectoryOptions;
    procedure Execute(aResultsPage: integer = -1);
  public
    property Options: TLazFindInFileSearchOptions read GetOptions
                                                  write SetOptions;
    property FindText: string read GetFindText write SetFindText;
    property ReplaceText: string read GetReplaceText write SetReplaceText;
    property SynSearchOptions: TSynSearchOptions read GetSynOptions
                                                 write SetSynOptions;
    function GetBaseDirectory: string;
    procedure LoadHistory;
    procedure SaveHistory;
    procedure FindInSearchPath(SearchPath: string);
    procedure FindInFiles(aProject: TProject);
    procedure InitFindText;
    procedure FindInFiles(aProject: TProject; const aFindText: string;
      aDialog: boolean = true; aResultsPage: integer = -1);
    procedure FindInFiles(aProject: TProject; const aFindText: string;
      aOptions: TLazFindInFileSearchOptions; aFileMask, aDir: string;
      aDialog: boolean = true; aResultsPage: integer = -1);
    function GetResolvedDirectories: string;
    property LazProject: TProject read FProject write FProject;
  end;

function FindInFilesDialog: TLazFindInFilesDialog;

implementation

var
  FindInFilesDialogSingleton: TLazFindInFilesDialog = nil;

function FindInFilesDialog: TLazFindInFilesDialog;
begin
  if FindInFilesDialogSingleton = nil then
    FindInFilesDialogSingleton := TLazFindInFilesDialog.Create(Application);
  Result := FindInFilesDialogSingleton;
end;

{$R *.lfm}

{ TLazFindInFilesDialog }

procedure TLazFindInFilesDialog.SetFindText(const NewFindText: string);
begin
  TextToFindComboBox.Text := NewFindText;
  TextToFindComboBox.SelectAll;
  ActiveControl := TextToFindComboBox;
end;

function TLazFindInFilesDialog.GetFindText: string;
begin
  Result := TextToFindComboBox.Text;
end;

function TLazFindInFilesDialog.GetReplaceText: string;
begin
  Result:=ReplaceTextComboBox.Text;
end;

procedure TLazFindInFilesDialog.WhereRadioGroupClick(Sender: TObject);
begin
  UpdateDirectoryOptions;
end;

procedure TLazFindInFilesDialog.DirectoriesBrowseClick(Sender: TObject);
var
  Dir: String;
  OldDirs: String;
  p: Integer;
begin
  InitIDEFileDialog(SelectDirectoryDialog);
  // use the first directory as initialdir for the dialog
  OldDirs:=GetResolvedDirectories;
  p:=1;
  repeat
    Dir:=GetNextDirectoryInSearchPath(OldDirs,p);
    if Dir='' then break;
    if DirectoryExistsUTF8(Dir) then break;
  until false;
  if Dir<>'' then
    SelectDirectoryDialog.InitialDir := Dir
  else
    SelectDirectoryDialog.InitialDir := GetBaseDirectory;

  SelectDirectoryDialog.FileName:='';
  if SelectDirectoryDialog.Execute then
    DirectoriesComboBox.Text := AppendPathDelim(TrimFilename(SelectDirectoryDialog.FileName));
  StoreIDEFileDialog(SelectDirectoryDialog);
end;

procedure TLazFindInFilesDialog.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TLazFindInFilesDialog.FormCreate(Sender: TObject);
begin
  Caption := srkmecFindInFiles;

  TextToFindLabel.Caption := dlgTextToFind;
  ReplaceCheckBox.Caption := dlgReplaceWith;

  with OptionsCheckGroupBox do
  begin
    Caption := lisOptions;
    Items.Clear; // remove all designed-time items
    Buttons[ Items.Add(dlgCaseSensitive           ) ].Tag := PtrInt(fifMatchCase);
    Buttons[ Items.Add(dlgWholeWordsOnly          ) ].Tag := PtrInt(fifWholeWord);
    Buttons[ Items.Add(dlgRegularExpressions      ) ].Tag := PtrInt(fifRegExpr  );
    Buttons[ Items.Add(lisFindFileMultiLinePattern) ].Tag := PtrInt(fifMultiLine);
  end;

  with WhereRadioGroup do
  begin
    Caption:=lisFindFileWhere;
    Items.Clear; // remove all designed-time items
    Buttons[ Items.Add(lisFindFilesearchAllFilesInProject) ].Tag := PtrInt(fifSearchProject     );
    if assigned(ProjectGroupManager) then
      Buttons[ Items.Add(lisFindFilesSearchInProjectGroup) ].Tag := PtrInt(fifSearchProjectGroup);
    Buttons[ Items.Add(lisFindFilesearchAllOpenFiles     ) ].Tag := PtrInt(fifSearchOpen        );
    ItemIndex := Items.Count - 1; // select previous item
    Buttons[ Items.Add(lisFindFilesearchInDirectories    ) ].Tag := PtrInt(fifSearchDirectories );
    Buttons[ Items.Add(lisFindFilesearchInActiveFile     ) ].Tag := PtrInt(fifSearchActive      );
  end;

  DirectoriesOptionsGroupBox.Caption := lisDirectories;
  DirectoriesComboBox.Hint:=lisMultipleDirectoriesAreSeparatedWithSemicolons;
  DirectoriesLabel.Caption := lisFindFileDirectories;
  FileMaskLabel.Caption := lisFindFileFileMask;

  IncludeSubDirsCheckBox.Caption := lisFindFileIncludeSubDirectories;

  ButtonPanel1.HelpButton.Caption := lisMenuHelp;
  ButtonPanel1.CancelButton.Caption := lisCancel;

  ReplaceCheckBox.Enabled:=true;

  UpdateReplaceCheck;
  UpdateDirectoryOptions;

  AutoSize:=IDEDialogLayoutList.Find(Self,false)=nil;
  IDEDialogLayoutList.ApplyLayout(Self);
end;

procedure TLazFindInFilesDialog.FormShow(Sender: TObject);
begin
  TextToFindComboBox.DropDownCount:=EnvironmentOptions.DropDownCount;
  ReplaceTextComboBox.DropDownCount:=EnvironmentOptions.DropDownCount;
  DirectoriesComboBox.DropDownCount:=EnvironmentOptions.DropDownCount;
  FileMaskComboBox.DropDownCount:=EnvironmentOptions.DropDownCount;
end;

procedure TLazFindInFilesDialog.OKButtonClick(Sender : TObject);
var
  Directories, Dir: String;
  p: Integer;
begin
  // checking the existence of folders
  if fifSearchDirectories = TLazFindInFileSearchOption(
    WhereRadioGroup.Buttons[WhereRadioGroup.ItemIndex].Tag) then
  begin
    Directories:=GetResolvedDirectories;
    p:=1;
    repeat
      Dir:=GetNextDirectoryInSearchPath(Directories,p);
      if (Dir<>'') and not DirectoryExistsUTF8(Dir) then
      begin
        IDEMessageDialog(lisEnvOptDlgDirectoryNotFound,
                   Format(dlgSeachDirectoryNotFound,[Dir]),
                   mtWarning, [mbOk]);
        ModalResult:=mrNone;
        Break;
      end;
    until Dir='';
  end;
end;

procedure TLazFindInFilesDialog.ReplaceCheckBoxChange(Sender: TObject);
begin
  UpdateReplaceCheck;
end;

procedure TLazFindInFilesDialog.SetOptions(NewOptions: TLazFindInFileSearchOptions);
var
  i: Integer;
begin
  IncludeSubDirsCheckBox.Checked := fifIncludeSubDirs in NewOptions;
  ReplaceCheckBox       .Checked := [fifReplace, fifReplaceAll] * NewOptions <> [];
  with OptionsCheckGroupBox do
    for i := 0 to Items.Count - 1 do
      Buttons[i].Checked := TLazFindInFileSearchOption(Buttons[i].Tag) in NewOptions;
  with WhereRadioGroup do
    for i := 0 to Items.Count - 1 do
      if TLazFindInFileSearchOption(Buttons[i].Tag) in NewOptions then
        ItemIndex := i;

  UpdateReplaceCheck;
  UpdateDirectoryOptions;
end;

function TLazFindInFilesDialog.GetOptions: TLazFindInFileSearchOptions;
var
  i: Integer;
begin
  Result := [];
  if IncludeSubDirsCheckBox.Checked then Include(Result, fifIncludeSubDirs);
  if ReplaceCheckBox       .Checked then Include(Result, fifReplace);
  with OptionsCheckGroupBox do
    for i := 0 to Items.Count - 1 do
      if Buttons[i].Checked then
        Include(Result, TLazFindInFileSearchOption(Buttons[i].Tag));
  with WhereRadioGroup do
    Include(Result, TLazFindInFileSearchOption(Buttons[ItemIndex].Tag));
end;

function TLazFindInFilesDialog.GetSynOptions: TSynSearchOptions;
var
  i: Integer;
begin
  Result := [];
  if ReplaceCheckBox.Checked then Include(Result, ssoReplace);
  with OptionsCheckGroupBox do
    for i := 0 to Items.Count - 1 do
      if Buttons[i].Checked then
        case TLazFindInFileSearchOption(Buttons[i].Tag) of
          fifMatchCase: Include(Result, ssoMatchCase       );
          fifWholeWord: Include(Result, ssoWholeWord       );
          fifRegExpr  : Include(Result, ssoRegExpr         );
          fifMultiLine: Include(Result, ssoRegExprMultiLine);
        end;
end;

procedure TLazFindInFilesDialog.SetReplaceText(const AValue: string);
begin
  ReplaceTextComboBox.Text := AValue;
end;

procedure TLazFindInFilesDialog.SetSynOptions(NewOptions: TSynSearchOptions);
var
  i: Integer;
begin
  ReplaceCheckBox.Checked := ([ssoReplace, ssoReplaceAll] * NewOptions <> []);
  with OptionsCheckGroupBox do
    for i := 0 to Items.Count - 1 do
      case TLazFindInFileSearchOption(Buttons[i].Tag) of
        fifMatchCase: Buttons[i].Checked := ssoMatchCase        in NewOptions;
        fifWholeWord: Buttons[i].Checked := ssoWholeWord        in NewOptions;
        fifRegExpr  : Buttons[i].Checked := ssoRegExpr          in NewOptions;
        fifMultiLine: Buttons[i].Checked := ssoRegExprMultiLine in NewOptions;
      end;

  UpdateReplaceCheck;
end;

procedure TLazFindInFilesDialog.UpdateReplaceCheck;
begin
  ReplaceTextComboBox.Enabled:=ReplaceCheckBox.Checked;
  if ReplaceCheckBox.Checked then
    ButtonPanel1.OKButton.Caption := lisBtnReplace
  else
    ButtonPanel1.OKButton.Caption := lisBtnFind;
end;

procedure TLazFindInFilesDialog.UpdateDirectoryOptions;
begin
  with WhereRadioGroup do
    case TLazFindInFileSearchOption(Buttons[ItemIndex].Tag) of
      fifSearchDirectories: begin
        DirectoriesOptionsGroupBox.Enabled := true;
        DirectoriesBrowse         .Enabled := true;
        DirectoriesComboBox       .Enabled := true;
      end;
      fifSearchProjectGroup: begin
        DirectoriesOptionsGroupBox.Enabled := true;
        DirectoriesBrowse         .Enabled := false;
        DirectoriesComboBox       .Enabled := false;
      end;
      else
        DirectoriesOptionsGroupBox.Enabled := false;
    end;
end;

function TLazFindInFilesDialog.GetBaseDirectory: string;
begin
  Result:='';
  if Project1<>nil then
    Result:=Project1.Directory;
  if Result='' then
    Result:=GetCurrentDirUTF8;
end;

const
  SharedOptions = [ssoMatchCase,ssoWholeWord,ssoRegExpr,ssoRegExprMultiLine];

procedure TLazFindInFilesDialog.LoadHistory;

  procedure AssignToComboBox(AComboBox: TComboBox; Strings: TStrings);
  begin
    AComboBox.Items.Assign(Strings);
    if AComboBox.Items.Count>0 then
      AComboBox.ItemIndex := 0;
  end;

  procedure AddFileToComboBox(AComboBox: TComboBox; Filename: string);
  var
    i: Integer;
  begin
    if Filename='' then exit;
    Filename:=AppendPathDelim(TrimFilename(Filename));
    for i:=0 to AComboBox.Items.Count-1 do begin
      if CompareFilenames(Filename,AComboBox.Items[i])=0 then begin
        // move to front (but not top, top should be the last used directory)
        if i>2 then
          AComboBox.Items.Move(i,1);
        exit;
      end;
    end;
    // insert in front (but not top, top should be the last used directory)
    if AComboBox.Items.Count>0 then
      i:=1
    else
      i:=0;
    AComboBox.Items.Insert(i,Filename);
  end;

var
  SrcEdit: TSourceEditorInterface;
begin
  SrcEdit := SourceEditorManagerIntf.ActiveEditor;
  //DebugLn('TSourceNotebook.LoadFindInFilesHistory ',dbgsName(TextToFindComboBox),' ',dbgsName(FindHistory));
  TextToFindComboBox.Items.Assign(InputHistories.FindHistory);
  ReplaceTextComboBox.Items.Assign(InputHistories.ReplaceHistory);
  if not EditorOpts.FindTextAtCursor then begin
    if TextToFindComboBox.Items.Count>0 then begin
      //debugln('TSourceNotebook.LoadFindInFilesHistory A TextToFindComboBox.Text=',TextToFindComboBox.Text);
      TextToFindComboBox.ItemIndex:=0;
      TextToFindComboBox.SelectAll;
      //debugln('TSourceNotebook.LoadFindInFilesHistory B TextToFindComboBox.Text=',TextToFindComboBox.Text);
    end;
  end;
  // show last used directories and directory of current file
  AssignToComboBox(DirectoriesComboBox, InputHistories.FindInFilesPathHistory);
  if (SrcEdit<>nil) and (FilenameIsAbsolute(SrcEdit.FileName)) then
    AddFileToComboBox(DirectoriesComboBox, ExtractFilePath(SrcEdit.FileName));
  if DirectoriesComboBox.Items.Count>0 then
    DirectoriesComboBox.Text:=DirectoriesComboBox.Items[0];
  // show last used file masks
  AssignToComboBox(FileMaskComboBox, InputHistories.FindInFilesMaskHistory);
  Options := InputHistories.FindInFilesSearchOptions;
  //share basic options with FindReplaceDlg
  SynSearchOptions := InputHistoriesSO.FindOptions[False] * SharedOptions;
end;

procedure TLazFindInFilesDialog.SaveHistory;
var
  Dir: String;
begin
  if ReplaceCheckBox.Checked then
    InputHistories.AddToReplaceHistory(ReplaceText);
  InputHistories.AddToFindHistory(FindText);
  Dir:=AppendPathDelim(TrimFilename(DirectoriesComboBox.Text));
  if Dir<>'' then
    InputHistories.AddToFindInFilesPathHistory(Dir);
  InputHistories.AddToFindInFilesMaskHistory(FileMaskComboBox.Text);
  InputHistories.FindInFilesSearchOptions:=Options;
  //share basic options with FindReplaceDlg
  InputHistoriesSO.FindOptions[False] := InputHistoriesSO.FindOptions[False] - SharedOptions
                                              + (SynSearchOptions*SharedOptions);
  InputHistories.Save;
end;

procedure TLazFindInFilesDialog.FindInSearchPath(SearchPath: string);
var
  i: Integer;
begin
  debugln(['TLazFindInFilesDialog.FindInSearchPath ',SearchPath]);
  InitFindText;
  LoadHistory;
  DirectoriesComboBox.Text:=SearchPath;

  // select fifSearchDirectories
  with WhereRadioGroup do
    for i := 0 to Items.Count - 1 do
      if fifSearchDirectories = TLazFindInFileSearchOption(Buttons[i].Tag) then
        ItemIndex := i;

  // disable replace. Find in files is often called,
  // but almost never to replace with the same parameters
  Options := Options-[fifReplace,fifReplaceAll];
  if ShowModal <> mrOK then
    Exit;
  Execute;
end;

procedure TLazFindInFilesDialog.FindInFiles(aProject: TProject);
begin
  InitFindText;
  FindInFiles(aProject, FindText);
end;

procedure TLazFindInFilesDialog.InitFindText;
var
  TempEditor: TSourceEditorInterface;
  NewFindText: String;
begin
  NewFindText:='';
  TempEditor := SourceEditorManagerIntf.ActiveEditor;
  if TempEditor <> nil
  then //with TempEditor.EditorComponent do
  begin
    if EditorOpts.FindTextAtCursor
    then begin
      if TempEditor.SelectionAvailable and (TempEditor.BlockBegin.Y = TempEditor.BlockEnd.Y)
      then NewFindText := TempEditor.Selection
      else NewFindText := TSynEdit(TempEditor.EditorControl).GetWordAtRowCol(TempEditor.CursorTextXY);
    end else begin
      if InputHistories.FindHistory.Count>0 then
        NewFindText:=InputHistories.FindHistory[0];
    end;
  end;
  FindText:=NewFindText;
end;

procedure TLazFindInFilesDialog.FindInFiles(
  aProject: TProject; const aFindText: string;
  aDialog: boolean = true; aResultsPage: integer = -1);
begin
  LazProject := aProject;
  LoadHistory;

  // if there is no FindText, use the most recently used FindText
  if (aFindText = '') and (InputHistories.FindHistory.Count > 0)
    then FindText := InputHistories.FindHistory[0]
    else FindText := aFindText;

  // disable replace. Find in files is often called,
  // but almost never to replace with the same parameters
  Options := Options - [fifReplace, fifReplaceAll];

  if aDialog then
    if ShowModal <> mrOk then
      exit;

  Execute(aResultsPage);
end;

procedure TLazFindInFilesDialog.FindInFiles(
  aProject: TProject; const aFindText: string;
  aOptions: TLazFindInFileSearchOptions; aFileMask, aDir: string;
  aDialog: boolean = true; aResultsPage: integer = -1);
begin
  LazProject := aProject;
  LoadHistory;

  // if there is no FindText, use the most recently used FindText
  if (aFindText = '') and (InputHistories.FindHistory.Count > 0)
    then FindText := InputHistories.FindHistory[0]
    else FindText := aFindText;

  Options := aOptions;
  DirectoriesComboBox.Text := TrimFilename(aDir);
  FileMaskComboBox.Text := aFileMask;

  // disable replace. Find in files is often called,
  // but almost never to replace with the same parameters
  Options := Options - [fifReplace, fifReplaceAll];

  if aDialog then
    if ShowModal <> mrOk then
      exit;

  Execute(aResultsPage);
end;

function TLazFindInFilesDialog.GetResolvedDirectories: string;
begin
  Result:=DirectoriesComboBox.Text;
  IDEMacros.SubstituteMacros(Result);
  Result:=TrimSearchPath(Result,GetBaseDirectory,true,true);
end;

procedure TLazFindInFilesDialog.Execute(aResultsPage: integer = -1);
var
  SearchForm: TSearchProgressForm;
begin
  { Only then in manual dialog data entry }
  if aResultsPage < 0 then
    SaveHistory;

  SearchForm := TSearchProgressForm.Create(SearchResultsView);
  with SearchForm do begin
    SearchOptions     := self.Options;
    SearchText        := self.FindText;
    ReplaceText       := self.ReplaceText;
    SearchMask        := self.FileMaskComboBox.Text;
    SearchDirectories := self.GetResolvedDirectories;
    ResultsPageIndex  := aResultsPage;
  end;

  try
    if FindText <> '' then
      with WhereRadioGroup do
        case TLazFindInFileSearchOption(Buttons[ItemIndex].Tag) of
          fifSearchActive     : SearchForm.DoSearchActiveFile;
          fifSearchOpen       : SearchForm.DoSearchOpenFiles;
          fifSearchDirectories: SearchForm.DoSearchDirs;
          fifSearchProject: begin
            if assigned(LazProject) then
              SearchForm.DoSearchProject(LazProject)
            else
              SearchForm.DoSearchProject(Project1);
          end;
          fifSearchProjectGroup: begin
            SearchForm.SearchOptions:=SearchForm.SearchOptions-[fifIncludeSubDirs];
            SearchForm.DoSearchProjectGroup;
          end;
        end;
  finally
    FreeAndNil(SearchForm);
  end;

  FProject := nil;
end;

end.

