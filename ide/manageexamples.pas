unit ManageExamples;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, ListFilterEdit, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ExtCtrls, ButtonPanel, //FileUtil,
  LCLProc, Buttons, EditBtn, LazIDEIntf, MainIntf, EnvironmentOpts, //AvgLvlTree,
  LazarusIDEStrConsts;

type

  { TManageExamplesForm }

  TManageExamplesForm = class(TForm)
    BuildAllSelectedButton: TBitBtn;
    RelativeCheckBox: TCheckBox;
    DescriptionMemo: TMemo;
    TestCaseCheckBox: TCheckBox;
    ExamplesCheckBox: TCheckBox;
    DirectoryComboBox: TComboBox;
    ProjectFilter: TListFilterEdit;
    OpenSelectedButton: TBitBtn;
    Label1: TLabel;
    ProjectsListBox: TListBox;
    RootRadioGroup: TRadioGroup;
    SelectAllButton: TBitBtn;
    ButtonPanel1: TButtonPanel;
    RootDirectoryEdit: TDirectoryEdit;
    ActionGroupBox: TGroupBox;
    SelectNoneButton: TBitBtn;
    ProjectsGroupBox: TGroupBox;
    procedure BuildAllSelectedButtonClick(Sender: TObject);
    procedure OpenSelectedButtonClick(Sender: TObject);
    procedure ProjectsListBoxSelectionChange(Sender: TObject; User: boolean);
    procedure RelativeCheckBoxClick(Sender: TObject);
    procedure RootRadioGroupClick(Sender: TObject);
    procedure DirectoryComboBoxChange(Sender: TObject);
    procedure RootDirectoryEditChange(Sender: TObject);
    procedure SelectAllButtonClick(Sender: TObject);
    procedure SelectNoneButtonClick(Sender: TObject);
    procedure ExamplesCheckBoxChange(Sender: TObject);
  private
    fSelectedFilename: string;
    fFirstSelectedIndex: Integer;
    fChangingSelections: Boolean;
    fNeedsFindDirectories: Boolean;
    fNeedsFindProjects: Boolean;
    fUpdating: Boolean;
    fIdleConnected: boolean;
    procedure FillDirectoriesBending;
    procedure FillProjectsBending;
    procedure SetIdleConnected(const AValue: boolean);
    procedure OnIdle(Sender: TObject; var Done: Boolean);
  public
    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
    property IdleConnected: boolean read fIdleConnected write SetIdleConnected;
  end;

function ShowManageExamplesDlg: TModalResult;


implementation

{$R *.lfm}

function ShowManageExamplesDlg: TModalResult;
var
  theForm: TManageExamplesForm;
begin
  Result:=mrCancel;
  theForm:=TManageExamplesForm.Create(Nil);
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
    fForm: TManageExamplesForm;
  protected
    procedure DoFileFound; override;
  public
    constructor Create(aForm: TManageExamplesForm);
  end;

{ TListFileSearcher }

procedure TListFileSearcher.DoFileFound;
begin
  fForm.ProjectFilter.Data.Add(FileName) // fForm.ProjectsListBox.Items.Add(FileName)
end;

constructor TListFileSearcher.Create(aForm: TManageExamplesForm);
begin
  fForm := aForm;
end;

{ TManageExamplesForm }

constructor TManageExamplesForm.Create(AnOwner: TComponent);
var
  path: String;
begin
  inherited Create(AnOwner);
  fFirstSelectedIndex:=-1;
  fChangingSelections:=False;
  fUpdating:=False;
  fNeedsFindDirectories:=False;
  fNeedsFindProjects:=False;

  Caption:=lisKMExampleProjects;
  ExamplesCheckBox.Caption:=lisIncludeExamples;
  TestCaseCheckBox.Caption:=lisIncludeTestcases;

  RootRadioGroup.Caption:=lisSearchProjectsFrom;
  RootRadioGroup.Items.Add(lisLazarusSource);
  RootRadioGroup.Items.Add(dlgCOOther);  // could use lisCEOtherGroup
  RootRadioGroup.ItemIndex:=0;
  RootRadioGroupClick(RootRadioGroup);

  RelativeCheckBox.Caption:=lisRelativePaths;
  OpenSelectedButton.Caption:=lisExamplesOpenFirstSelected;
  BuildAllSelectedButton.Caption:=lisExamplesBuildAllSelected;
  SelectAllButton.Caption:=lisMenuSelectAll;
  SelectNoneButton.Caption:=lisSAMSelectNone;

  OpenSelectedButton.LoadGlyphFromLazarusResource('laz_open');
  BuildAllSelectedButton.LoadGlyphFromLazarusResource('menu_build_all');
  SelectAllButton.LoadGlyphFromLazarusResource('menu_select_all');
  SelectNoneButton.LoadGlyphFromLazarusResource('ce_default');

  FillDirectoriesBending;
  FillProjectsBending;
end;

destructor TManageExamplesForm.Destroy;
begin
  inherited Destroy;
end;

procedure TManageExamplesForm.FillDirectoriesBending;
begin
  fNeedsFindDirectories:=True;
  IdleConnected:=True;
end;

procedure TManageExamplesForm.FillProjectsBending;
begin
  fNeedsFindProjects:=True;
  IdleConnected:=True;
end;

procedure TManageExamplesForm.SetIdleConnected(const AValue: boolean);
begin
  if fIdleConnected=AValue then exit;
  fIdleConnected:=AValue;
  if fIdleConnected then
    Application.AddOnIdleHandler(@OnIdle)
  else
    Application.RemoveOnIdleHandler(@OnIdle);
end;

procedure TManageExamplesForm.OnIdle(Sender: TObject; var Done: Boolean);
var
  Searcher: TListFileSearcher;
  AllDirs: TStringList;
  i: Integer;
  LastDir: String;
begin
  IdleConnected:=false;
  if fUpdating then Exit;
  fUpdating:=True;
  if fNeedsFindDirectories then begin
    Screen.Cursor:=crHourGlass;
    Application.ProcessMessages;
    DirectoryComboBox.Items.Clear;
    DirectoryComboBox.Text:='';
    RootDirectoryEdit.Text:='';
    AllDirs:=FindAllDirectories(EnvironmentOptions.LazarusDirectory);
    try
      for i:=0 to AllDirs.Count-1 do begin
        LastDir:=ExtractFileName(AllDirs[i]);
        if (ExamplesCheckBox.Checked and (LastDir='examples'))
        or (TestCaseCheckBox.Checked and (LastDir='tests')) then
          DirectoryComboBox.Items.Add(AllDirs[i]);
      end;
      // Add something to combobox to prevent crash with GTK2.
      if DirectoryComboBox.Items.Count = 0 then
        DirectoryComboBox.Items.Add('[empty]');
      DirectoryComboBox.ItemIndex:=0;
      DirectoryComboBoxChange(DirectoryComboBox);
    finally
      AllDirs.Free;
      fNeedsFindDirectories:=False;
      Screen.Cursor:=crDefault;
    end;
  end;
  if fNeedsFindProjects and (RootDirectoryEdit.Text<>'') then
  try
    Screen.Cursor:=crHourGlass;
    Application.ProcessMessages;
    ProjectFilter.Data.Clear; //  ProjectsListBox.Items.Clear;
    Searcher:=TListFileSearcher.Create(Self);
    Searcher.Search(RootDirectoryEdit.Text, '*.lpi');
    ProjectFilter.InvalidateFilter;
  finally
    Searcher.Free;
    fNeedsFindProjects:=False;
    Screen.Cursor:=crDefault;
  end;
  fUpdating:=False;
end;

procedure TManageExamplesForm.RootRadioGroupClick(Sender: TObject);
var
  LazSrc: Boolean;
begin
  LazSrc:=RootRadioGroup.ItemIndex=0;
  ExamplesCheckBox.Enabled:=LazSrc;
  TestCaseCheckBox.Enabled:=LazSrc;
  DirectoryComboBox.Enabled:=LazSrc;
  RootDirectoryEdit.Enabled:=not LazSrc;
end;

procedure TManageExamplesForm.DirectoryComboBoxChange(Sender: TObject);
begin
  if DirectoryExists(DirectoryComboBox.Text) then begin
    RootDirectoryEdit.Text:=DirectoryComboBox.Text;
    FillProjectsBending;
  end;
end;

procedure TManageExamplesForm.RootDirectoryEditChange(Sender: TObject);
begin
  FillProjectsBending;
end;

procedure TManageExamplesForm.ExamplesCheckBoxChange(Sender: TObject);
begin
  FillDirectoriesBending;
end;

procedure TManageExamplesForm.OpenSelectedButtonClick(Sender: TObject);
begin
  if fFirstSelectedIndex <> -1 then begin
    if FileExistsUTF8(ProjectsListBox.Items[fFirstSelectedIndex]) then begin
      fSelectedFilename:=ProjectsListBox.Items[fFirstSelectedIndex];
      ModalResult:=mrYes;      // mrYes means the selected file will be opened.
    end else begin
      ShowMessage(Format(lisFileNotFound3, [ProjectsListBox.Items[fFirstSelectedIndex]]));
    end;
  end;
end;

procedure TManageExamplesForm.BuildAllSelectedButtonClick(Sender: TObject);
var
  i: Integer;
begin
  for i:=0 to ProjectsListBox.Items.Count-1 do begin
    if ProjectsListBox.Selected[i] then begin
       ; // ToDo
    end;
  end;
end;

procedure TManageExamplesForm.SelectAllButtonClick(Sender: TObject);
begin
  fChangingSelections:=True;
  ProjectsListBox.SelectAll;
  fChangingSelections:=False;
  ProjectsListBoxSelectionChange(ProjectsListBox, False); // In the end update buttons
end;

procedure TManageExamplesForm.SelectNoneButtonClick(Sender: TObject);
var
  i: Integer;
begin
  fChangingSelections:=True;
  for i:=0 to ProjectsListBox.Items.Count-1 do
    ProjectsListBox.Selected[i]:=False;
  fChangingSelections:=False;
  ProjectsListBoxSelectionChange(ProjectsListBox, False);
end;

procedure TManageExamplesForm.RelativeCheckBoxClick(Sender: TObject);
begin
  ;
end;

// Project list selection changes. Adjust buttons.

procedure TManageExamplesForm.ProjectsListBoxSelectionChange(Sender: TObject; User: boolean);
var
  HasSelected: Boolean;
  ReadMe, RealReadMe: String;
  i: Integer;
begin
  if not fChangingSelections then begin
    HasSelected := ProjectsListBox.SelCount > 0;
    OpenSelectedButton.Enabled := HasSelected;
//    BuildAllSelectedButton.Enabled := HasSelected;
    SelectNoneButton.Enabled := HasSelected;
    // Find the first selected item and show README.txt contents.
    if HasSelected then
      for i:=0 to ProjectsListBox.Items.Count-1 do
        if ProjectsListBox.Selected[i] then begin
          fFirstSelectedIndex:=i;
          ReadMe:=ExtractFilePath(ProjectsListBox.Items[i])+'README.txt';
          RealReadMe:=FindDiskFileCaseInsensitive(ReadMe);
          if RealReadMe <> '' then
            DescriptionMemo.Lines.LoadFromFile(RealReadMe)
          else
            DescriptionMemo.Clear;
          Break;
        end
    else
      fFirstSelectedIndex:=-1;
  end;
end;

end.

