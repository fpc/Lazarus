{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Find in files dialog form.

}
unit FindInFilesDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LCLIntf, Controls, StdCtrls, Forms, Buttons,
  ExtCtrls, FileProcs, LazarusIDEStrConsts, Dialogs, SynEditTypes, ButtonPanel,
  MacroIntf, IDEWindowIntf, SrcEditorIntf, IDEHelpIntf, IDEDialogs,
  InputHistory, EditorOptions, SearchFrm, Project, SynEdit, SearchResultView;

type
  { TLazFindInFilesDialog }

  TLazFindInFilesDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    ReplaceCheckBox: TCheckBox;
    ReplaceTextComboBox: TComboBox;
    IncludeSubDirsCheckBox: TCheckBox;
    FileMaskComboBox: TComboBox;
    DirectoryBrowse: TBitBtn;
    DirectoryComboBox: TComboBox;
    DirectoryLabel: TLabel;
    FileMaskLabel: TLabel;
    DirectoryOptionsGroupBox: TGroupBox;
    OptionsCheckGroupBox: TCheckGroup;
    SelectDirectoryDialog: TSelectDirectoryDialog;
    TextToFindComboBox: TComboBox;
    TextToFindLabel: TLabel;
    WhereRadioGroup: TRadioGroup;
    procedure DirectoryBrowseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure OKButtonClick(Sender : TObject);
    procedure ReplaceCheckBoxChange(Sender: TObject);
    procedure WhereRadioGroupClick(Sender: TObject);
  private
    function GetFindText: string;
    function GetOptions: TLazFindInFileSearchOptions;
    function GetReplaceText: string;
    function GetSynOptions: TSynSearchOptions;
    procedure SetFindText(const NewFindText: string);
    procedure SetOptions(NewOptions: TLazFindInFileSearchOptions);
    procedure SetReplaceText(const AValue: string);
    procedure SetSynOptions(NewOptions: TSynSearchOptions);
    procedure UpdateReplaceCheck;
  public
    property Options: TLazFindInFileSearchOptions read GetOptions
                                                  write SetOptions;
    property FindText: string read GetFindText write SetFindText;
    property ReplaceText: string read GetReplaceText write SetReplaceText;
    property SynSearchOptions: TSynSearchOptions read GetSynOptions
                                                 write SetSynOptions;
    procedure LoadHistory;
    procedure SaveHistory;
    procedure FindInFilesPerDialog(AProject: TProject);
    procedure InitFromLazSearch(Sender: TObject);
    procedure FindInFiles(AProject: TProject; const AFindText: string);
    function GetResolvedDirectory: string;
  end;

function FindInFilesDialog: TLazFindInFilesDialog;

implementation

const  // WhereRadioGroup's ItemIndex in a more informative form.
  ItemIndProject     = 0;
  ItemIndOpenFiles   = 1;
  ItemIndDirectories = 2;
  ItemIndActiveFile  = 3;

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
  DirectoryOptionsGroupBox.Enabled := (WhereRadioGroup.ItemIndex = ItemIndDirectories)
end;

procedure TLazFindInFilesDialog.DirectoryBrowseClick(Sender: TObject);
var
  Dir: String;
begin
  InitIDEFileDialog(SelectDirectoryDialog);
  Dir:=GetResolvedDirectory;
  if DirectoryExistsUTF8(Dir) then
    SelectDirectoryDialog.InitialDir := Dir
  else
    SelectDirectoryDialog.InitialDir := GetCurrentDirUTF8;
  if SelectDirectoryDialog.Execute then
    DirectoryComboBox.Text := AppendPathDelim(TrimFilename(SelectDirectoryDialog.FileName));
  StoreIDEFileDialog(SelectDirectoryDialog);
end;

procedure TLazFindInFilesDialog.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TLazFindInFilesDialog.FormCreate(Sender: TObject);
begin
  Caption := srkmecFindInFiles;

  TextToFindLabel.Caption := dlgTextToFind;
  ReplaceCheckBox.Caption := dlgReplaceWith;

  OptionsCheckGroupBox.Caption := dlgFROpts;
  OptionsCheckGroupBox.Items[0] := dlgCaseSensitive;
  OptionsCheckGroupBox.Items[1] := dlgWholeWordsOnly;
  OptionsCheckGroupBox.Items[2] := dlgRegularExpressions;
  OptionsCheckGroupBox.Items[3] := lisFindFileMultiLinePattern;

  WhereRadioGroup.Caption:=lisFindFileWhere;
  WhereRadioGroup.Items[ItemIndProject]     := lisFindFilesearchAllFilesInProject;
  WhereRadioGroup.Items[ItemIndOpenFiles]   := lisFindFilesearchAllOpenFiles;
  WhereRadioGroup.Items[ItemIndDirectories] := lisFindFilesearchInDirectories;
  WhereRadioGroup.Items[ItemIndActiveFile]  := lisFindFilesearchInActiveFile;

  DirectoryOptionsGroupBox.Caption := lisFindFileDirectoryOptions;
  DirectoryLabel.Caption := lisFindFileDirectory;
  FileMaskLabel.Caption := lisFindFileFileMask;

  IncludeSubDirsCheckBox.Caption := lisFindFileIncludeSubDirectories;

  ButtonPanel1.HelpButton.Caption := lisMenuHelp;
  ButtonPanel1.CancelButton.Caption := lisCancel;

  ReplaceCheckBox.Enabled:=true;

  UpdateReplaceCheck;
  DirectoryOptionsGroupBox.Enabled:=WhereRadioGroup.ItemIndex=ItemIndDirectories;

  AutoSize:=IDEDialogLayoutList.Find(Self,false)=nil;
  IDEDialogLayoutList.ApplyLayout(Self);
end;

procedure TLazFindInFilesDialog.HelpButtonClick(Sender: TObject);
begin
  LazarusHelp.ShowHelpForIDEControl(Self);
end;

procedure TLazFindInFilesDialog.OKButtonClick(Sender : TObject);
var
  Dir: String;
begin
  Dir:=GetResolvedDirectory;
  if (WhereRadioGroup.ItemIndex=ItemIndDirectories) and not DirectoryExistsUTF8(Dir) then
  begin
    IDEMessageDialog(lisEnvOptDlgDirectoryNotFound,
               Format(dlgSeachDirectoryNotFound,[Dir]),
               mtWarning, [mbOk]);
    ModalResult:=mrNone;
  end
end;

procedure TLazFindInFilesDialog.ReplaceCheckBoxChange(Sender: TObject);
begin
  UpdateReplaceCheck;
end;

procedure TLazFindInFilesDialog.SetOptions(NewOptions: TLazFindInFileSearchOptions);
begin
  OptionsCheckGroupBox.Checked[0] := fifMatchCase in NewOptions;
  OptionsCheckGroupBox.Checked[1] := fifWholeWord in NewOptions;
  OptionsCheckGroupBox.Checked[2] := fifRegExpr in NewOptions;
  OptionsCheckGroupBox.Checked[3] := fifMultiLine in NewOptions;
  DirectoryOptionsGroupBox.Enabled := fifSearchDirectories in NewOptions;
  IncludeSubDirsCheckBox.Checked := fifIncludeSubDirs in NewOptions;
  ReplaceCheckBox.Checked := [fifReplace,fifReplaceAll]*NewOptions<>[];
  
  if fifSearchProject     in NewOptions then WhereRadioGroup.ItemIndex := ItemIndProject;
  if fifSearchOpen        in NewOptions then WhereRadioGroup.ItemIndex := ItemIndOpenFiles;
  if fifSearchDirectories in NewOptions then WhereRadioGroup.ItemIndex := ItemIndDirectories;
  if fifSearchActive      in NewOptions then WhereRadioGroup.ItemIndex := ItemIndActiveFile;

  UpdateReplaceCheck;
end;

function TLazFindInFilesDialog.GetOptions: TLazFindInFileSearchOptions;
begin
  Result := [];
  if OptionsCheckGroupBox.Checked[0] then Include(Result, fifMatchCase);
  if OptionsCheckGroupBox.Checked[1] then Include(Result, fifWholeWord);
  if OptionsCheckGroupBox.Checked[2] then Include(Result, fifRegExpr);
  if OptionsCheckGroupBox.Checked[3] then Include(Result, fifMultiLine);
  if IncludeSubDirsCheckBox.Checked then Include(Result, fifIncludeSubDirs);
  if ReplaceCheckBox.Checked then Include(Result, fifReplace);

  case WhereRadioGroup.ItemIndex of
    ItemIndProject    : Include(Result, fifSearchProject);
    ItemIndOpenFiles  : Include(Result, fifSearchOpen);
    ItemIndDirectories: Include(Result, fifSearchDirectories);
    ItemIndActiveFile : Include(Result, fifSearchActive);
  end;//case
end;

function TLazFindInFilesDialog.GetSynOptions: TSynSearchOptions;
begin
  Result := [];
  if OptionsCheckGroupBox.Checked[0] then Include(Result, ssoMatchCase);
  if OptionsCheckGroupBox.Checked[1] then Include(Result, ssoWholeWord);
  if OptionsCheckGroupBox.Checked[2] then Include(Result, ssoRegExpr);
  if OptionsCheckGroupBox.Checked[3] then Include(Result, ssoRegExprMultiLine);
  if ReplaceCheckBox.Checked then Include(Result, ssoReplace);
end;//GetSynOptions

procedure TLazFindInFilesDialog.SetReplaceText(const AValue: string);
begin
  ReplaceTextComboBox.Text := AValue;
end;

procedure TLazFindInFilesDialog.SetSynOptions(NewOptions: TSynSearchOptions);
begin
  OptionsCheckGroupBox.Checked[0] := ssoMatchCase in NewOptions;
  OptionsCheckGroupBox.Checked[1] := ssoWholeWord in NewOptions;
  OptionsCheckGroupBox.Checked[2] := ssoRegExpr in NewOptions;
  OptionsCheckGroupBox.Checked[3] := ssoRegExprMultiLine in NewOptions;
  ReplaceCheckBox.Checked := ([ssoReplace,ssoReplaceAll]*NewOptions <> []);

  UpdateReplaceCheck;
end;//SetSynOptions

procedure TLazFindInFilesDialog.UpdateReplaceCheck;
begin
  ReplaceTextComboBox.Enabled:=ReplaceCheckBox.Checked;
  if ReplaceCheckBox.Checked then
    ButtonPanel1.OKButton.Caption := lisBtnReplace
  else
    ButtonPanel1.OKButton.Caption := lisBtnFind;
end;

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
  AssignToComboBox(DirectoryComboBox, InputHistories.FindInFilesPathHistory);
  if (SrcEdit<>nil) and (FilenameIsAbsolute(SrcEdit.FileName)) then
    AddFileToComboBox(DirectoryComboBox, ExtractFilePath(SrcEdit.FileName));
  if DirectoryComboBox.Items.Count>0 then
    DirectoryComboBox.Text:=DirectoryComboBox.Items[0];
  // show last used file masks
  AssignToComboBox(FileMaskComboBox, InputHistories.FindInFilesMaskHistory);
  Options := InputHistories.FindInFilesSearchOptions;
end;

procedure TLazFindInFilesDialog.SaveHistory;
var
  Dir: String;
begin
  InputHistories.AddToFindHistory(FindText);
  Dir:=AppendPathDelim(TrimFilename(DirectoryComboBox.Text));
  if Dir<>'' then
    InputHistories.AddToFindInFilesPathHistory(Dir);
  InputHistories.AddToFindInFilesMaskHistory(FileMaskComboBox.Text);
  InputHistories.FindInFilesSearchOptions:=Options;
  InputHistories.Save;
end;

procedure TLazFindInFilesDialog.FindInFilesPerDialog(AProject: TProject);
var
  TempEditor: TSourceEditorInterface;
Begin
  FindText:='';
  TempEditor := SourceEditorManagerIntf.ActiveEditor;
  if TempEditor <> nil
  then //with TempEditor.EditorComponent do
  begin
    if EditorOpts.FindTextAtCursor
    then begin
      if TempEditor.SelectionAvailable and (TempEditor.BlockBegin.Y = TempEditor.BlockEnd.Y)
      then FindText := TempEditor.Selection
      else FindText := TSynEdit(TempEditor.EditorControl).GetWordAtRowCol(TempEditor.CursorTextXY);
    end else begin
      if InputHistories.FindHistory.Count>0 then
        FindText:=InputHistories.FindHistory[0];
    end;
  end;

  FindInFiles(AProject, FindText);
end;

procedure TLazFindInFilesDialog.InitFromLazSearch(Sender: TObject);
var
  Dir: String;
begin
  Dir:=AppendPathDelim(TrimFilename(TLazSearch(Sender).SearchDirectory));
  if Dir<>'' then
    DirectoryComboBox.Text:= Dir;
  Options:= TLazSearch(Sender).SearchOptions;
  FileMaskComboBox.Text:= TLazSearch(Sender).SearchMask;
end;

procedure TLazFindInFilesDialog.FindInFiles(AProject: TProject; const AFindText: string);
var
  SearchForm:  TSearchProgressForm;
begin
  LoadHistory;

  // if there is no FindText, use the most recently used FindText
  FindText:= AFindText;
  if (FindText = '') and (InputHistories.FindHistory.Count > 0) then
    FindText := InputHistories.FindHistory[0];

  // disable replace. Find in files is often called,
  // but almost never to replace with the same parameters
  Options := Options-[fifReplace,fifReplaceAll];
  if ShowModal=mrOk then
  begin
    SaveHistory;

    SearchForm:= TSearchProgressForm.Create(SearchResultsView);
    with SearchForm do begin
      SearchOptions   := self.Options;
      SearchText      := self.FindText;
      ReplaceText     := self.ReplaceText;
      SearchMask      := self.FileMaskComboBox.Text;
      SearchDirectory := self.GetResolvedDirectory;
    end;

    try
      if FindText <> '' then
      begin
        case WhereRadioGroup.ItemIndex of
          ItemIndProject    : SearchForm.DoSearchProject(AProject);
          ItemIndOpenFiles  : SearchForm.DoSearchOpenFiles;
          ItemIndDirectories: SearchForm.DoSearchDir;
          ItemIndActiveFile : SearchForm.DoSearchActiveFile;
        end;
      end;
    finally
      FreeAndNil(SearchForm);
    end;
  end;
end;

function TLazFindInFilesDialog.GetResolvedDirectory: string;
begin
  Result:=DirectoryComboBox.Text;
  IDEMacros.SubstituteMacros(Result);
  Result:=TrimAndExpandDirectory(Result);
end;

end.

