unit ExampleManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, lazutf8classes, ListFilterEdit, Forms, Controls,
  Graphics, Dialogs, StdCtrls, ExtCtrls, ButtonPanel, Buttons, EditBtn, LCLProc,
  IDEWindowIntf, LazIDEIntf, MainIntf, EnvironmentOpts, LazarusIDEStrConsts;

type

  { TExampleManagerForm }

  TExampleManagerForm = class(TForm)
    BuildAllSelectedButton: TBitBtn;
    cgIncludedDirs: TCheckGroup;
    lbProjectCount: TLabel;
    lbRootDirectory: TLabel;
    SelectPanel: TPanel;
    RelativeCheckBox: TCheckBox;
    DescriptionMemo: TMemo;
    Splitter1: TSplitter;
    cbIncludeAllDirs: TCheckBox;
    ProjectFilter: TListFilterEdit;
    OpenSelectedButton: TBitBtn;
    lbConstruction: TLabel;
    ProjectsListBox: TListBox;
    SelectAllButton: TBitBtn;
    ButtonPanel1: TButtonPanel;
    edRootDirectory: TDirectoryEdit;
    ActionGroupBox: TGroupBox;
    SelectNoneButton: TBitBtn;
    ProjectsGroupBox: TGroupBox;
    procedure cbIncludeAllDirsClick(Sender: TObject);
    procedure cgIncludedDirsItemClick(Sender: TObject; Index: integer);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure BuildAllSelectedButtonClick(Sender: TObject);
    procedure OpenSelectedButtonClick(Sender: TObject);
    procedure ProjectFilterAfterFilter(Sender: TObject);
    procedure ProjectsListBoxSelectionChange(Sender: TObject; User: boolean);
    procedure RelativeCheckBoxClick(Sender: TObject);
    procedure edRootDirectoryChange(Sender: TObject);
    procedure SelectAllButtonClick(Sender: TObject);
    procedure SelectNoneButtonClick(Sender: TObject);
  private
    fSelectedFilename: string;
    fFirstSelectedIndex: Integer;
    fChangingSelections: Boolean;
    fIdleConnected: boolean;
    procedure FillProjectsPending;
    procedure SetIdleConnected(const AValue: boolean);
    procedure OnIdle(Sender: TObject; var Done: Boolean);
  public
    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
    property IdleConnected: boolean read fIdleConnected write SetIdleConnected;
  end;

function ShowExampleManagerDlg: TModalResult;


implementation

{$R *.lfm}

const
  DirectoryChoices: array[0..7] of string = (
    'example',   // 1st row
    'sample',
    'demo',
    'test',
    'examples',  // 2nd row
    'samples',
    'demos',
    'tests'
  );

function ShowExampleManagerDlg: TModalResult;
var
  theForm: TExampleManagerForm;
begin
  Result:=mrCancel;
  theForm:=TExampleManagerForm.Create(Nil);
  try
    Result:=theForm.ShowModal;
    if Result=mrYes then
      MainIDEInterface.DoOpenProjectFile(theForm.fSelectedFilename,
          [ofOnlyIfExists,ofAddToRecent,ofUseCache]);

  finally
    theForm.Free;
  end;
end;

type

  { TListFileSearcher }

  TListFileSearcher = class(TFileSearcher)
  private
    fForm: TExampleManagerForm;
  protected
    procedure DoFileFound; override;
  public
    constructor Create(aForm: TExampleManagerForm);
  end;

{ TListFileSearcher }

procedure TListFileSearcher.DoFileFound;
begin
  fForm.ProjectFilter.Items.Add(FileName);
end;

constructor TListFileSearcher.Create(aForm: TExampleManagerForm);
begin
  inherited Create;
  fForm:=aForm;
end;

{ TExampleManagerForm }

constructor TExampleManagerForm.Create(AnOwner: TComponent);
var
  i: Integer;
begin
  inherited Create(AnOwner);
  fFirstSelectedIndex:=-1;
  fChangingSelections:=False;

  Caption:=lisKMExampleProjects;
  cbIncludeAllDirs.Caption:=lisIncludeAllSubDirectories;
  cgIncludedDirs.Caption:=lisIncludeSubDirectories;

  // Add potential included directories to CheckGroup
  cgIncludedDirs.Items.Clear;
  for i := Low(DirectoryChoices) to High(DirectoryChoices) do
  begin
    cgIncludedDirs.Items.Add(DirectoryChoices[i]);
    cgIncludedDirs.Checked[i]:=True;
  end;
  cbIncludeAllDirsClick(cbIncludeAllDirs);
  lbProjectCount.Caption:='';

  // Projects and their Actions
  ProjectsGroupBox.Caption:=lisMEProjects;
  ActionGroupBox.Caption:=lisMEAction;

  RelativeCheckBox.Caption:=lisRelativePaths;
  OpenSelectedButton.Caption:=lisExamplesOpenFirstSelected;
  BuildAllSelectedButton.Caption:=lisExamplesBuildAllSelected;
  SelectAllButton.Caption:=lisMenuSelectAll;
  SelectNoneButton.Caption:=lisSAMSelectNone;

  OpenSelectedButton.LoadGlyphFromLazarusResource('laz_open');
  BuildAllSelectedButton.LoadGlyphFromLazarusResource('menu_build_all');
  SelectAllButton.LoadGlyphFromLazarusResource('menu_select_all');
  SelectNoneButton.LoadGlyphFromLazarusResource('ce_default');

  edRootDirectory.Text:=EnvironmentOptions.GetParsedLazarusDirectory;
  FillProjectsPending;
end;

destructor TExampleManagerForm.Destroy;
begin
  inherited Destroy;
end;

procedure TExampleManagerForm.FormCreate(Sender: TObject);
begin
  IDEDialogLayoutList.ApplyLayout(Self);
end;

procedure TExampleManagerForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TExampleManagerForm.FillProjectsPending;
begin
  IdleConnected:=True;
end;

procedure TExampleManagerForm.SetIdleConnected(const AValue: boolean);
begin
  if fIdleConnected=AValue then exit;
  fIdleConnected:=AValue;
  if fIdleConnected then
    Application.AddOnIdleHandler(@OnIdle)
  else
    Application.RemoveOnIdleHandler(@OnIdle);
end;

procedure TExampleManagerForm.OnIdle(Sender: TObject; var Done: Boolean);
var
  Searcher: TListFileSearcher;
  AllDirs, IncludedDirs: TStringList;
  i, j: Integer;
  LastDir: String;
begin
  Screen.Cursor:=crHourGlass;
  Searcher:=TListFileSearcher.Create(Self);
  IncludedDirs:=TStringList.Create;
  AllDirs:=Nil;
  try
    // Collect each matching directory name to a list.
    if (edRootDirectory.Text<>'') and not cbIncludeAllDirs.Checked then
    begin
      AllDirs:=FindAllDirectories(edRootDirectory.Text);
      for i:=0 to AllDirs.Count-1 do
      begin
        LastDir:=ExtractFileName(AllDirs[i]);
        for j:=Low(DirectoryChoices) to High(DirectoryChoices) do
        begin
          if cgIncludedDirs.Checked[j] and (LastDir=DirectoryChoices[j]) then
          begin
            IncludedDirs.Add(AllDirs[i]);
            Break;
          end;
        end;
      end;
    end
    // Add only the root directory name to list. Will find all projects in one go.
    else if cbIncludeAllDirs.Checked then
      IncludedDirs.Add(edRootDirectory.Text);
    ProjectFilter.Items.Clear;
    // Find projects in all included directories.
    for i:=0 to IncludedDirs.Count-1 do
      Searcher.Search(IncludedDirs[i], '*.lpi');
    ProjectFilter.InvalidateFilter;
    DescriptionMemo.Clear;
    IdleConnected:=false;
  finally
    AllDirs.Free;
    IncludedDirs.Free;
    Searcher.Free;
    Screen.Cursor:=crDefault;
  end;
end;

procedure TExampleManagerForm.edRootDirectoryChange(Sender: TObject);
begin
  FillProjectsPending;
end;

procedure TExampleManagerForm.cbIncludeAllDirsClick(Sender: TObject);
begin
  cgIncludedDirs.Enabled:=not (Sender as TCheckBox).Checked;
  FillProjectsPending;
end;

procedure TExampleManagerForm.cgIncludedDirsItemClick(Sender: TObject; Index: integer);
begin
  FillProjectsPending;
end;

procedure TExampleManagerForm.OpenSelectedButtonClick(Sender: TObject);
begin
  if fFirstSelectedIndex <> -1 then
  begin
    if FileExistsUTF8(ProjectsListBox.Items[fFirstSelectedIndex]) then
    begin
      fSelectedFilename:=ProjectsListBox.Items[fFirstSelectedIndex];
      ModalResult:=mrYes;      // mrYes means the selected file will be opened.
    end
    else begin
      ShowMessage(Format(lisFileNotFound3, [ProjectsListBox.Items[fFirstSelectedIndex]]));
    end;
  end;
end;

procedure TExampleManagerForm.ProjectFilterAfterFilter(Sender: TObject);
begin
  lbProjectCount.Caption:=IntToStr(ProjectsListBox.Count)+lisProjectCount;
end;

procedure TExampleManagerForm.BuildAllSelectedButtonClick(Sender: TObject);
var
  i: Integer;
begin
  for i:=0 to ProjectsListBox.Items.Count-1 do
  begin
    if ProjectsListBox.Selected[i] then begin
       ; // ToDo
    end;
  end;
end;

procedure TExampleManagerForm.SelectAllButtonClick(Sender: TObject);
begin
  fChangingSelections:=True;
  ProjectsListBox.SelectAll;
  fChangingSelections:=False;
  ProjectsListBoxSelectionChange(ProjectsListBox, False); // In the end update buttons
end;

procedure TExampleManagerForm.SelectNoneButtonClick(Sender: TObject);
var
  i: Integer;
begin
  fChangingSelections:=True;
  for i:=0 to ProjectsListBox.Items.Count-1 do
    ProjectsListBox.Selected[i]:=False;
  fChangingSelections:=False;
  ProjectsListBoxSelectionChange(ProjectsListBox, False);
end;

procedure TExampleManagerForm.RelativeCheckBoxClick(Sender: TObject);
begin
  ;
end;

// Project list selection changes. Adjust buttons.
procedure TExampleManagerForm.ProjectsListBoxSelectionChange(Sender: TObject; User: boolean);
var
  HasSelected: Boolean;
  ReadMe, RealReadMe: String;
  i: Integer;
begin
  if not fChangingSelections then
  begin
    HasSelected := ProjectsListBox.SelCount > 0;
    OpenSelectedButton.Enabled := HasSelected;
//    BuildAllSelectedButton.Enabled := HasSelected;
    SelectNoneButton.Enabled := HasSelected;
    // Find the first selected item and show README.txt contents.
    if HasSelected then
      for i:=0 to ProjectsListBox.Items.Count-1 do
        if ProjectsListBox.Selected[i] then
        begin
          fFirstSelectedIndex:=i;
          ReadMe:=ExtractFilePath(ProjectsListBox.Items[i])+'README.txt';
          RealReadMe:=FindDiskFileCaseInsensitive(ReadMe);
          if RealReadMe <> '' then
            LoadStringsFromFileUTF8(DescriptionMemo.Lines,RealReadMe)
          else
            DescriptionMemo.Clear;
          Break;
        end
    else
      fFirstSelectedIndex:=-1;
  end;
end;

end.

