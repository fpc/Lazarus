unit ManageExamples; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, CheckLst, ButtonPanel, PairSplitter, EditBtn, LCLProc, AvgLvlTree,
  Buttons, LazIDEIntf, MainIntf, EnvironmentOpts, LazarusIDEStrConsts;

type

  { TManageExamplesForm }

  TManageExamplesForm = class(TForm)
    BuildAllSelectedButton: TBitBtn;
    OpenSelectedButton: TBitBtn;
    Label1: TLabel;
    ProjectsListBox: TListBox;
    SelectAllButton: TBitBtn;
    ButtonPanel1: TButtonPanel;
    RootDirectoryEdit: TDirectoryEdit;
    ActionGroupBox: TGroupBox;
    RootDirectoryLabel: TLabel;
    SelectNoneButton: TBitBtn;
    SettingsGroupBox: TGroupBox;
    ProjectsGroupBox: TGroupBox;
    procedure BuildAllSelectedButtonClick(Sender: TObject);
    procedure OpenSelectedButtonClick(Sender: TObject);
    procedure ProjectsListBoxSelectionChange(Sender: TObject; User: boolean);
    procedure RootDirectoryEditChange(Sender: TObject);
    procedure SelectAllButtonClick(Sender: TObject);
    procedure SelectNoneButtonClick(Sender: TObject);
    procedure ShowPathCheckBoxChange(Sender: TObject);
  private
    fChangingSelections: Boolean;
    fUpdating: Boolean;
    fIdleConnected: boolean;
    fSelectedFilename: string;
    procedure FillProjectList(Immediately: boolean);
    procedure SetIdleConnected(const AValue: boolean);
    procedure OnIdle(Sender: TObject; var Done: Boolean);
  public
    constructor Create(AnOwner: TComponent);
    destructor Destroy; override;
    property IdleConnected: boolean read fIdleConnected write SetIdleConnected;
  end;

function ShowManageExamplesDlg: TModalResult;


implementation

{$R *.lfm}

function ShowManageExamplesDlg: TModalResult;
var
  theForm: TManageExamplesForm;
  path: String;
begin
  Result:=mrCancel;
  theForm:=TManageExamplesForm.Create(Nil);
  try
    path:=EnvironmentOptions.LazarusDirectory+'examples';
    if DirectoryExistsUTF8(path) then begin
      theForm.RootDirectoryEdit.Text:=path;
      theForm.FillProjectList(False);
    end;
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
var
  ShortName, FilePath: String;
begin
  fForm.ProjectsListBox.Items.Add(FileName)
end;

constructor TListFileSearcher.Create(aForm: TManageExamplesForm);
begin
  fForm := aForm;
end;

{ TManageExamplesForm }

constructor TManageExamplesForm.Create(AnOwner: TComponent);
begin
  inherited Create(AnOwner);
  fChangingSelections:=False;
  fUpdating:=False;

  OpenSelectedButton.Caption:=lisExamplesOpenFirstSelected;
  BuildAllSelectedButton.Caption:=lisExamplesBuildAllSelected;
  SelectAllButton.Caption:=lisMenuSelectAll;
  SelectNoneButton.Caption:=lisSAMSelectNone;

  OpenSelectedButton.LoadGlyphFromLazarusResource('laz_open');
  BuildAllSelectedButton.LoadGlyphFromLazarusResource('menu_build_all');
  SelectAllButton.LoadGlyphFromLazarusResource('menu_select_all');
  SelectNoneButton.LoadGlyphFromLazarusResource('ce_default');
end;

destructor TManageExamplesForm.Destroy;
begin
  inherited Destroy;
end;

procedure TManageExamplesForm.FillProjectList(Immediately: boolean);
var
  Searcher: TListFileSearcher;
begin
  if not Immediately then begin
    IdleConnected:=true;
    exit;
  end;
  if fUpdating then Exit;
  if RootDirectoryEdit.Text<>'' then
  try
    fUpdating:=True;
    ProjectsListBox.Items.Clear;
    Searcher:=TListFileSearcher.Create(Self);
    Searcher.Search(RootDirectoryEdit.Text, '*.lpi');
  finally
    Searcher.Free;
    fUpdating:=False;
  end;
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
begin
  if Done then ;
  IdleConnected:=false;
  FillProjectList(true);
end;

procedure TManageExamplesForm.RootDirectoryEditChange(Sender: TObject);
begin
  FillProjectList(False);
end;

procedure TManageExamplesForm.ShowPathCheckBoxChange(Sender: TObject);
begin
  FillProjectList(False);
end;

procedure TManageExamplesForm.OpenSelectedButtonClick(Sender: TObject);
var
  i: Integer;
begin
  for i:=0 to ProjectsListBox.Items.Count-1 do begin
    if ProjectsListBox.Selected[i] then begin
      if FileExistsUTF8(ProjectsListBox.Items[i]) then begin
        fSelectedFilename:=ProjectsListBox.Items[i];
        ModalResult:=mrYes;      // mrYes means the selected file will be opened.
        Break;
      end else begin
        ShowMessage(Format(lisFileNotFound3, [ProjectsListBox.Items[i]]));
      end;
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

// Project list selection changes. Adjust buttons.

procedure TManageExamplesForm.ProjectsListBoxSelectionChange(Sender: TObject; User: boolean);
var
  HasSelected: Boolean;
begin
  if not fChangingSelections then begin
    HasSelected := ProjectsListBox.SelCount > 0;
    OpenSelectedButton.Enabled := HasSelected;
//    BuildAllSelectedButton.Enabled := HasSelected;
    SelectNoneButton.Enabled := HasSelected;
  end;
end;

end.

