unit BuildModesManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, Buttons, Menus, ButtonPanel, LCLProc,
  ProjectIntf, IDEImagesIntf, IDEOptionsIntf, CompOptsIntf,
  PackageDefs, TransferMacros, PathEditorDlg, Project, LazarusIDEStrConsts,
  CompilerOptions, IDEProcs, BuildModeDiffDlg;

type

  { TBuildModesForm }

  TBuildModesForm = class(TForm)
    BuildModeAddSpeedButton: TSpeedButton;
    BuildModeDeleteSpeedButton: TSpeedButton;
    BuildModeDiffSpeedButton: TSpeedButton;
    BuildModeMoveDownSpeedButton: TSpeedButton;
    BuildModeMoveUpSpeedButton: TSpeedButton;
    BuildModesGroupBox: TGroupBox;
    BuildModesPopupMenu: TPopupMenu;
    BuildModesStringGrid: TStringGrid;
    ButtonPanel1: TButtonPanel;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BuildModeDiffSpeedButtonClick(Sender: TObject);
    procedure BuildModeAddSpeedButtonClick(Sender: TObject);
    procedure BuildModeDeleteSpeedButtonClick(Sender: TObject);
    procedure BuildModeMoveDownSpeedButtonClick(Sender: TObject);
    procedure BuildModeMoveUpSpeedButtonClick(Sender: TObject);
    procedure BuildModesCheckboxToggled(Sender: TObject;
      aCol, aRow: Integer; aState: TCheckboxState);
    procedure BuildModesStringGridSelection(Sender: TObject;
      aCol, aRow: Integer);
    procedure BuildModesStringGridValidateEntry(Sender: TObject;
      aCol, aRow: Integer; const OldValue: string; var NewValue: String);
    procedure FormShow(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    fActiveBuildMode: TProjectBuildMode;
    fBuildModes: TProjectBuildModes;
    fShowSession: boolean;
    fModeActiveCol: integer;
    fModeInSessionCol: integer;
    fModeNameCol: integer;
    procedure FillBuildModesGrid(aOnlyActiveState: Boolean = False);
    function GetActiveBuildMode: TProjectBuildMode;
    procedure SetActiveBuildMode(AValue: TProjectBuildMode);
    procedure UpdateBuildModeButtons;
    procedure SetShowSession(const AValue: boolean);
    procedure DoShowSession;
    procedure UpdateDialogCaption;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetSelectedBuildMode: TProjectBuildMode;
    procedure SetActiveBuildModeByID(AValue: TProjectBuildMode);
  public
    property ActiveBuildMode: TProjectBuildMode read GetActiveBuildMode write SetActiveBuildMode;
    property BuildModes: TProjectBuildModes read fBuildModes;
    property ShowSession: boolean read fShowSession write SetShowSession;
  end;

var
  OnLoadIDEOptionsHook: TOnLoadIDEOptions;
  OnSaveIDEOptionsHook: TOnSaveIDEOptions;

function ShowBuildModesDlg(aShowSession: Boolean): TModalResult;
procedure SwitchBuildMode(aBuildModeID: string);
procedure UpdateBuildModeCombo(aCombo: TComboBox);


implementation

{$R *.lfm}

function ShowBuildModesDlg(aShowSession: Boolean): TModalResult;
var
  frm: TBuildModesForm;
begin
  frm := TBuildModesForm.Create(nil);
  try
    Assert(Assigned(Project1), 'ShowBuildModesDlg: Project is not assigned.');
    // Save changes
    OnSaveIDEOptionsHook(Nil, Project1.CompilerOptions);
    // Copy to dialog
    frm.fBuildModes.Assign(Project1.BuildModes, True);
    frm.SetActiveBuildModeByID(Project1.ActiveBuildMode);
    frm.fShowSession:=aShowSession;
    // Show the form. Let user add / edit / delete.
    Result := frm.ShowModal;
    if Result = mrOk then
    begin
      // Copy back from dialog
      Project1.BuildModes.Assign(frm.fBuildModes, True);
      // Switch
      Project1.ActiveBuildModeID:=frm.fActiveBuildMode.Identifier;
      IncreaseBuildMacroChangeStamp;
      // Load options
      OnLoadIDEOptionsHook(Nil, Project1.CompilerOptions);
    end;
  finally
    frm.Free;
  end;
end;

procedure SwitchBuildMode(aBuildModeID: string);
begin
  OnSaveIDEOptionsHook(Nil, Project1.CompilerOptions);    // Save changes
  Project1.ActiveBuildModeID := aBuildModeID;             // Switch
  IncreaseBuildMacroChangeStamp;
  OnLoadIDEOptionsHook(Nil, Project1.CompilerOptions);    // Load options
end;

procedure UpdateBuildModeCombo(aCombo: TComboBox);
var
  i, ActiveIndex: Integer;
  CurMode: TProjectBuildMode;
  sl: TStringList;
begin
  ActiveIndex := 0;
  sl:=TStringList.Create;
  try
    sl.Add(lisAllBuildModes);
    for i := 0 to Project1.BuildModes.Count-1 do
    begin
      CurMode := Project1.BuildModes[i];
      sl.Add(CurMode.Identifier);
      if CurMode = Project1.ActiveBuildMode then
        ActiveIndex := sl.Count-1;  // Will be set as ItemIndex in Combo.
    end;
    aCombo.Items.Assign(sl);
    aCombo.ItemIndex := ActiveIndex;
  finally
    sl.Free;
  end;
end;

{ TBuildModesForm }

constructor TBuildModesForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fBuildModes := TProjectBuildModes.Create(Nil);
end;

destructor TBuildModesForm.Destroy;
begin
  fBuildModes.Free;
  inherited Destroy;
end;

procedure TBuildModesForm.FormCreate(Sender: TObject);
begin
  ;
end;

procedure TBuildModesForm.FormDestroy(Sender: TObject);
begin
  ;
end;

procedure TBuildModesForm.FormShow(Sender: TObject);
begin
  // options dialog
  UpdateDialogCaption;
  BuildModesGroupBox.Caption:=lisBuildModes;
  DoShowSession;
  // modes
  FillBuildModesGrid;
  UpdateBuildModeButtons;

  BuildModeAddSpeedButton.LoadGlyphFromLazarusResource('laz_add');
  BuildModeDeleteSpeedButton.LoadGlyphFromLazarusResource('laz_delete');
  BuildModeMoveUpSpeedButton.LoadGlyphFromLazarusResource('arrow_up');
  BuildModeMoveDownSpeedButton.LoadGlyphFromLazarusResource('arrow_down');
  BuildModeDiffSpeedButton.LoadGlyphFromLazarusResource('menu_tool_diff');
end;

procedure TBuildModesForm.BuildModeDiffSpeedButtonClick(Sender: TObject);
begin
  // show diff dialog
  ShowBuildModeDiffDialog(GetSelectedBuildMode);
  IncreaseBuildMacroChangeStamp;
end;

procedure TBuildModesForm.BuildModeAddSpeedButtonClick(Sender: TObject);
var
  i: Integer;
  NewName, Identifier: String;
  CurMode, NewMode: TProjectBuildMode;
begin
  // use current mode as template
  i:=BuildModesStringGrid.Row-1;
  if (i>=0) then
  begin
    Identifier:=BuildModesStringGrid.Cells[fModeNameCol,i+1];
    CurMode:=fBuildModes[i];
  end
  else begin
    Identifier:='Mode';
    CurMode:=nil;
  end;
  // find unique name
  i:=0;
  repeat
    inc(i);
    NewName:=Identifier+IntToStr(i);
  until fBuildModes.Find(NewName)=nil;
  // create new mode
  NewMode:=fBuildModes.Add(NewName);
  // clone
  if CurMode<>nil then
    NewMode.Assign(CurMode);
  fActiveBuildMode:=NewMode; // activate
  FillBuildModesGrid;     // show
  // select identifier
  BuildModesStringGrid.Col:=fModeNameCol;
  BuildModesStringGrid.Row:=BuildModesStringGrid.RowCount-1;
  BuildModesStringGrid.EditorMode:=true;
end;

procedure TBuildModesForm.BuildModeDeleteSpeedButtonClick(Sender: TObject);
var
  i: Integer;
  CurMode: TProjectBuildMode;
  Grid: TStringGrid;
begin
  Grid:=BuildModesStringGrid;
  i:=Grid.Row-1;
  if i<0 then exit;
  if fBuildModes.Count=1 then
  begin
    MessageDlg(lisCCOErrorCaption, lisThereMustBeAtLeastOneBuildMode,
      mtError,[mbCancel],0);
    exit;
  end;
  CurMode:=fBuildModes[i];
  // when delete the activated: activate another
  if fActiveBuildMode=CurMode then
  begin
    if i<fBuildModes.Count-1 then
      fActiveBuildMode:=fBuildModes[i+1]
    else
      fActiveBuildMode:=fBuildModes[i-1];
  end;
  if fActiveBuildMode=CurMode then begin
    debugln(['TBuildModesForm.BuildModeDeleteSpeedButtonClick activate failed']);
    exit;
  end;
  // delete mode
  fBuildModes.Delete(i);
  FillBuildModesGrid;
  // select next mode
  if i>=Grid.RowCount then
    Grid.Row:=Grid.RowCount-1
  else
    Grid.Row:=i;
end;

procedure TBuildModesForm.BuildModeMoveDownSpeedButtonClick(Sender: TObject);
var
  i: Integer;
begin
  i:=BuildModesStringGrid.Row-1;
  if i+1>=fBuildModes.Count then exit;
  fBuildModes.Move(i,i+1);
  fBuildModes[0].InSession:=false;
  inc(i);
  FillBuildModesGrid;
  BuildModesStringGrid.Row:=i+1;
end;

procedure TBuildModesForm.BuildModeMoveUpSpeedButtonClick(Sender: TObject);
var
  i: Integer;
begin
  i:=BuildModesStringGrid.Row-1;
  if i<=0 then exit;
  fBuildModes.Move(i,i-1);
  dec(i);
  fBuildModes[0].InSession:=false;
  FillBuildModesGrid;
  BuildModesStringGrid.Row:=i+1;
end;

procedure TBuildModesForm.BuildModesCheckboxToggled(Sender: TObject;
  aCol, aRow: Integer; aState: TCheckboxState);
var
  CurMode: TProjectBuildMode;
  i: Integer;
  Grid: TStringGrid;
begin
  debugln(['TBuildModesForm.BuildModesCheckboxToggled Row=',aRow,' Col=',aCol,' ',ord(aState)]);
  i:=aRow-1;
  if (i<0) or (i>=fBuildModes.Count) then exit;
  CurMode:=fBuildModes[i];
  Grid:=BuildModesStringGrid;
  if aCol=fModeActiveCol then
  begin
    // activate
    if CurMode=fActiveBuildMode then begin
      debugln(['TBuildModesForm.BuildModesCheckboxToggled, is ActiveBuildMode',i]);
      // Switch back to Checked state. There must always be an active mode
      Grid.Cells[aCol,aRow]:=Grid.Columns[aCol].ValueChecked;
    end
    else begin
      debugln(['TBuildModesForm.BuildModesCheckboxToggled, another Mode',i]);
      fActiveBuildMode:=CurMode;
      FillBuildModesGrid(True);
    end;
  end else if aCol=fModeInSessionCol then
  begin
    // in session
    if (aState=cbChecked) and (i=0) then
    begin
      Grid.Cells[aCol,aRow]:=Grid.Columns[aCol].ValueUnchecked;
      MessageDlg(lisCCOErrorCaption,
        lisTheFirstBuildModeIsTheDefaultModeAndMustBeStoredIn,
        mtError,[mbCancel],0);
      exit;
    end;
    CurMode.InSession:=aState=cbChecked;
  end;
end;

procedure TBuildModesForm.BuildModesStringGridSelection(Sender: TObject;
  aCol, aRow: Integer);
begin
  UpdateBuildModeButtons;
end;

procedure TBuildModesForm.BuildModesStringGridValidateEntry(Sender: TObject;
  aCol, aRow: Integer; const OldValue: string; var NewValue: String);
var
  CurMode: TProjectBuildMode;
  s: string;
  i: Integer;
  b: Boolean;
begin
  debugln(['TBuildModesForm.BuildModesStringGridValidateEntry Row=',aRow,' Col=',aCol]);
  i:=aRow-1;
  if (i<0) or (i>=fBuildModes.Count) then exit;
  CurMode:=fBuildModes[i];
  if aCol=fModeInSessionCol then
  begin
    // in session
    b:=NewValue=BuildModesStringGrid.Columns[aCol].ValueChecked;
    if b and (i=0) then
    begin
      NewValue:=OldValue;
      MessageDlg(lisCCOErrorCaption,lisTheFirstBuildModeIsTheDefaultModeAndMustBeStoredIn,
                 mtError,[mbCancel],0);
      exit;
    end;
    CurMode.InSession:=b;
  end
  else if aCol=fModeNameCol then
  begin
    // identifier
    s:=NewValue;
    for i:=1 to length(s) do
      if s[i]<' ' then
        s[i]:=' ';
    CurMode.Identifier:=s;
    NewValue:=s;
  end;
  UpdateDialogCaption;
end;

procedure TBuildModesForm.FillBuildModesGrid(aOnlyActiveState: Boolean);
var
  i: Integer;
  CurMode: TProjectBuildMode;
  Grid: TStringGrid;
begin
  Grid:=BuildModesStringGrid;
  Grid.BeginUpdate;
  Grid.RowCount:=fBuildModes.Count+1;
  for i:=0 to fBuildModes.Count-1 do
  begin
    CurMode:=fBuildModes[i];
    // active
    if CurMode=fActiveBuildMode then
      Grid.Cells[fModeActiveCol,i+1]:=Grid.Columns[fModeActiveCol].ValueChecked
    else
      Grid.Cells[fModeActiveCol,i+1]:=Grid.Columns[fModeActiveCol].ValueUnchecked;
    if not aOnlyActiveState then
    begin
      // in session
      if fModeInSessionCol>=0 then
        if CurMode.InSession then
          Grid.Cells[fModeInSessionCol,i+1]:=Grid.Columns[fModeInSessionCol].ValueChecked
        else
          Grid.Cells[fModeInSessionCol,i+1]:=Grid.Columns[fModeInSessionCol].ValueUnchecked;
      // identifier
      Grid.Cells[fModeNameCol,i+1]:=CurMode.Identifier;
    end;
  end;
  Grid.EndUpdate(true);
end;

procedure TBuildModesForm.UpdateBuildModeButtons;
var
  i: Integer;
  CurMode: TProjectBuildMode;
  Identifier: string;
begin
  i:=BuildModesStringGrid.Row-1;
  if (fBuildModes<>nil) and (i>=0) and (i<fBuildModes.Count) then
  begin
    CurMode:=fBuildModes[i];
    Identifier:=BuildModesStringGrid.Cells[fModeNameCol,i+1];
  end
  else
    CurMode:=nil;

  BuildModeAddSpeedButton.Hint:=Format(lisAddNewBuildModeCopyingSettingsFrom, [Identifier]);
  BuildModeDeleteSpeedButton.Enabled:=(CurMode<>nil) and (fBuildModes.Count>1);
  BuildModeDeleteSpeedButton.Hint:=Format(lisDeleteMode, [Identifier]);
  BuildModeMoveUpSpeedButton.Enabled:=(CurMode<>nil) and (i>0);
  BuildModeMoveUpSpeedButton.Hint:=Format(lisMoveOnePositionUp, [Identifier]);
  BuildModeMoveDownSpeedButton.Enabled:=i<BuildModesStringGrid.RowCount-2;
  BuildModeMoveDownSpeedButton.Hint:=Format(lisMoveOnePositionDown, [Identifier]);
  BuildModeDiffSpeedButton.Hint:=lisShowDifferencesBetweenModes;
end;

procedure TBuildModesForm.SetShowSession(const AValue: boolean);
begin
  if AValue=fShowSession then exit;
  fShowSession:=AValue;
  DoShowSession;
  FillBuildModesGrid;
end;

procedure TBuildModesForm.DoShowSession;
var
  Grid: TStringGrid;
begin
  Grid:=BuildModesStringGrid;
  Grid.BeginUpdate;
  fModeActiveCol:=0;
  if fShowSession then
  begin
    fModeInSessionCol:=1;
    fModeNameCol:=2;
    if Grid.Columns.Count<3 then
      Grid.Columns.Insert(fModeInSessionCol);
  end else begin
    fModeInSessionCol:=-1;
    fModeNameCol:=1;
    if Grid.Columns.Count>2 then
      Grid.Columns.Delete(1);
  end;
  BuildModesStringGrid.Columns[fModeActiveCol].Title.Caption:=lisActive;
  BuildModesStringGrid.Columns[fModeActiveCol].SizePriority:=1;
  BuildModesStringGrid.Columns[fModeActiveCol].ButtonStyle:=cbsCheckboxColumn;
  if fModeInSessionCol>=0 then
  begin
    BuildModesStringGrid.Columns[fModeInSessionCol].Title.Caption:=lisInSession;
    BuildModesStringGrid.Columns[fModeInSessionCol].SizePriority:=1;
    BuildModesStringGrid.Columns[fModeInSessionCol].ButtonStyle:=cbsCheckboxColumn;
  end;
  BuildModesStringGrid.Columns[fModeNameCol].Title.Caption:=lisName;
  BuildModesStringGrid.Columns[fModeNameCol].SizePriority:=10;
  BuildModesStringGrid.Columns[fModeNameCol].ButtonStyle:=cbsAuto;
  Grid.EndUpdate(true);
end;

procedure TBuildModesForm.UpdateDialogCaption;
var
  s: String;
begin
  if Project1<>nil then
  begin
    s := Project1.GetTitleOrName;
    s:=Format(dlgProjectOptionsFor, [s]);
    if fBuildModes.Count>1 then
      s:=s+', '+copy(fActiveBuildMode.GetCaption,1,12);
  end else
    s:='TBuildModesForm.UpdateDialogCaption: no project';
  Caption:=s;
end;

function TBuildModesForm.GetSelectedBuildMode: TProjectBuildMode;
var
  i: LongInt;
begin
  Result:=nil;
  i:=BuildModesStringGrid.Row-1;
  if (i<0) or (i>=fBuildModes.Count) then exit;
  Result:=fBuildModes[i];
end;

procedure TBuildModesForm.OKButtonClick(Sender: TObject);
begin
  ;
end;

procedure TBuildModesForm.CancelButtonClick(Sender: TObject);
begin
  ;
end;

function TBuildModesForm.GetActiveBuildMode: TProjectBuildMode;
begin
  Result := fActiveBuildMode;
end;

procedure TBuildModesForm.SetActiveBuildMode(AValue: TProjectBuildMode);
begin
  fActiveBuildMode := AValue;
end;

procedure TBuildModesForm.SetActiveBuildModeByID(AValue: TProjectBuildMode);
var
  i: Integer;
begin
  for i:=0 to fBuildModes.Count-1 do
  begin
    if fBuildModes[i].Identifier=AValue.Identifier then
    begin
      ActiveBuildMode:=fBuildModes[i];
      Break;
    end;
  end;
end;

end.

