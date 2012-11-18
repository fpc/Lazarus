unit BuildModesManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, Buttons, Menus, ButtonPanel, LCLProc,
  ProjectIntf, IDEImagesIntf, IDEOptionsIntf, CompOptsIntf,
  PackageDefs, TransferMacros, //compiler_inherited_options,
  PathEditorDlg, Project, LazarusIDEStrConsts, CompilerOptions, // PackageSystem,
  IDEProcs, BuildModeDiffDlg;

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
//    FOnLoadOptionsHook: TOnLoadIDEOptions;
//    FOnSaveOptionsHook: TOnSaveIDEOptions;
    fBuildModes: TProjectBuildModes;
    FLoadShowSessionFromProject: boolean;
    FProject: TProject;
    FShowSession: boolean;
    FSwitchingMode: boolean;
    fModeActiveCol: integer;
    fModeInSessionCol: integer;
    fModeNameCol: integer;
    procedure FillBuildModesGrid;
    procedure UpdateBuildModeButtons;
    procedure ActivateMode(aMode: TProjectBuildMode);
    procedure UpdateShowSession;
    procedure SetShowSession(const AValue: boolean);
    procedure DoShowSession;
    procedure UpdateDialogCaption;
  public
    property SwitchingMode: boolean read FSwitchingMode; // the active mode is currently switched
    property ShowSession: boolean read FShowSession write SetShowSession;
    property LoadShowSessionFromProject: boolean read FLoadShowSessionFromProject
                                              write FLoadShowSessionFromProject;
    function GetSelectedBuildMode: TProjectBuildMode;
  public
//    property OnLoadIDEOptionsHook: TOnLoadIDEOptions read FOnLoadOptionsHook write FOnLoadOptionsHook;
//    property OnSaveIDEOptionsHook: TOnSaveIDEOptions read FOnSaveOptionsHook write FOnSaveOptionsHook;
  end;

function ShowBuildModesDlg(ABuildModes: TProjectBuildModes): TModalResult;


implementation

{$R *.lfm}

function ShowBuildModesDlg(ABuildModes: TProjectBuildModes): TModalResult;
var
  BuildModesForm: TBuildModesForm;
begin
  Result := mrCancel;
  BuildModesForm := TBuildModesForm.Create(nil);
  try
    BuildModesForm.fBuildModes.Assign(ABuildModes); // Copy to dialog.
    Result := BuildModesForm.ShowModal;
    if Result = mrOk then
      ABuildModes.Assign(BuildModesForm.fBuildModes); // Copy back from dialog.
  finally
    BuildModesForm.Free;
  end;
end;

{ TBuildModesForm }

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
  FProject:=Project1;       // Now hardcoded.
  // modes
  UpdateShowSession;
  FillBuildModesGrid;
  UpdateBuildModeButtons;
  // options dialog
  UpdateDialogCaption;

  BuildModesGroupBox.Caption:=lisBuildModes;
  DoShowSession;

  BuildModeAddSpeedButton.LoadGlyphFromLazarusResource('laz_add');
  BuildModeDeleteSpeedButton.LoadGlyphFromLazarusResource('laz_delete');
  BuildModeMoveUpSpeedButton.LoadGlyphFromLazarusResource('arrow_up');
  BuildModeMoveDownSpeedButton.LoadGlyphFromLazarusResource('arrow_down');
  BuildModeDiffSpeedButton.LoadGlyphFromLazarusResource('menu_tool_diff');
end;

procedure TBuildModesForm.BuildModeDiffSpeedButtonClick(Sender: TObject);
begin
  FSwitchingMode:=true;
  try
    // save changes
//    OnSaveIDEOptionsHook(Self,FProject.CompilerOptions);
    // show diff dialog
    ShowBuildModeDiffDialog(GetSelectedBuildMode);
    IncreaseBuildMacroChangeStamp;
    // load options
//    OnLoadIDEOptionsHook(Self,FProject.CompilerOptions);
  finally
    FSwitchingMode:=false;
  end;
end;

procedure TBuildModesForm.BuildModeAddSpeedButtonClick(Sender: TObject);
var
  i: Integer;
  NewName: String;
  Identifier: String;
  CurMode: TProjectBuildMode;
  NewMode: TProjectBuildMode;
begin
  // use current mode as template
  i:=BuildModesStringGrid.Row-1;
  if (i>=0) then
  begin
    Identifier:=BuildModesStringGrid.Cells[fModeNameCol,i+1];
    CurMode:=FProject.BuildModes[i];
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
  until FProject.BuildModes.Find(NewName)=nil;
  // create new mode
  NewMode:=FProject.BuildModes.Add(NewName);
  // clone
  if CurMode<>nil then
    NewMode.Assign(CurMode);
  // show
  FillBuildModesGrid;
  // activate
  ActivateMode(NewMode);
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
  if FProject.BuildModes.Count=1 then
  begin
    MessageDlg(lisCCOErrorCaption, lisThereMustBeAtLeastOneBuildMode,
      mtError,[mbCancel],0);
    exit;
  end;
  CurMode:=FProject.BuildModes[i];
  // when delete the activated: activate another
  if FProject.ActiveBuildMode=CurMode then
  begin
    if i<FProject.BuildModes.Count-1 then
      ActivateMode(FProject.BuildModes[i+1])
    else
      ActivateMode(FProject.BuildModes[i-1]);
  end;
  if FProject.ActiveBuildMode=CurMode then begin
    debugln(['TBuildModesEditorFrame.BuildModeDeleteSpeedButtonClick activate failed']);
    exit;
  end;
  // delete mode
  FProject.BuildModes.Delete(i);
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
  if i+1>=FProject.BuildModes.Count then exit;
  FProject.BuildModes.Move(i,i+1);
  FProject.BuildModes[0].InSession:=false;
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
  FProject.BuildModes.Move(i,i-1);
  dec(i);
  FProject.BuildModes[0].InSession:=false;
  FillBuildModesGrid;
  BuildModesStringGrid.Row:=i+1;
end;

procedure TBuildModesForm.BuildModesCheckboxToggled(Sender: TObject;
  aCol, aRow: Integer; aState: TCheckboxState);
var
  CurMode: TProjectBuildMode;
  b: Boolean;
  i: Integer;
  Grid: TStringGrid;
begin
  debugln(['TBuildModesForm.BuildModesCheckboxToggled Row=',aRow,' Col=',aCol,' ',ord(aState)]);
  i:=aRow-1;
  if (i<0) or (i>=FProject.BuildModes.Count) then exit;
  CurMode:=FProject.BuildModes[i];
  Grid:=BuildModesStringGrid;
  if aCol=fModeActiveCol then
  begin
    // activate
    if CurMode=FProject.ActiveBuildMode then begin
      debugln(['TBuildModesForm.BuildModesCheckboxToggled, is ActiveBuildMode',i]);
      // there must always be an active mode
      Grid.Cells[aCol,aRow]:=Grid.Columns[aCol].ValueChecked;
    end
    else begin
      debugln(['TBuildModesForm.BuildModesCheckboxToggled, another Mode',i]);
      ActivateMode(CurMode);
    end;
  end else if aCol=fModeInSessionCol then
  begin
    // in session
    b:=aState=cbChecked;
    if b and (i=0) then
    begin
      Grid.Cells[aCol,aRow]:=Grid.Columns[aCol].ValueUnchecked;
      MessageDlg(lisCCOErrorCaption,
        lisTheFirstBuildModeIsTheDefaultModeAndMustBeStoredIn,
        mtError,[mbCancel],0);
      exit;
    end;
    CurMode.InSession:=b;
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
  j: Integer;
  b: Boolean;
  i: Integer;
begin
  debugln(['TBuildModesForm.BuildModesStringGridValidateEntry Row=',aRow,' Col=',aCol]);
  i:=aRow-1;
  if (i<0) or (i>=FProject.BuildModes.Count) then exit;
  CurMode:=FProject.BuildModes[i];
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
    for j:=1 to length(s) do
      if s[j]<' ' then
        s[j]:=' ';
    CurMode.Identifier:=s;
    NewValue:=s;
  end;
  UpdateDialogCaption;
end;

procedure TBuildModesForm.FillBuildModesGrid;
var
  i: Integer;
  CurMode: TProjectBuildMode;
  Grid: TStringGrid;
begin
  if FProject=nil then exit;

  Grid:=BuildModesStringGrid;
  Grid.BeginUpdate;
  Grid.RowCount:=FProject.BuildModes.Count+1;

  for i:=0 to FProject.BuildModes.Count-1 do begin
    CurMode:=FProject.BuildModes[i];
    // active
    if CurMode=FProject.ActiveBuildMode then
      Grid.Cells[fModeActiveCol,i+1]:=Grid.Columns[fModeActiveCol].ValueChecked
    else
      Grid.Cells[fModeActiveCol,i+1]:=Grid.Columns[fModeActiveCol].ValueUnchecked;
    // in session
    if fModeInSessionCol>=0 then
      if CurMode.InSession then
        Grid.Cells[fModeInSessionCol,i+1]:=Grid.Columns[fModeInSessionCol].ValueChecked
      else
        Grid.Cells[fModeInSessionCol,i+1]:=Grid.Columns[fModeInSessionCol].ValueUnchecked;
    // identifier
    Grid.Cells[fModeNameCol,i+1]:=CurMode.Identifier;
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
  if (FProject<>nil) and (FProject.BuildModes<>nil)
  and (i>=0) and (i<FProject.BuildModes.Count) then
  begin
    CurMode:=FProject.BuildModes[i];
    Identifier:=BuildModesStringGrid.Cells[fModeNameCol,i+1];
  end
  else
    CurMode:=nil;

  BuildModeAddSpeedButton.Hint:=Format(lisAddNewBuildModeCopyingSettingsFrom, [Identifier]);
  BuildModeDeleteSpeedButton.Enabled:=(CurMode<>nil) and (FProject.BuildModes.Count>1);
  BuildModeDeleteSpeedButton.Hint:=Format(lisDeleteMode, [Identifier]);
  BuildModeMoveUpSpeedButton.Enabled:=(CurMode<>nil) and (i>0);
  BuildModeMoveUpSpeedButton.Hint:=Format(lisMoveOnePositionUp, [Identifier]);
  BuildModeMoveDownSpeedButton.Enabled:=i<BuildModesStringGrid.RowCount-2;
  BuildModeMoveDownSpeedButton.Hint:=Format(lisMoveOnePositionDown, [Identifier]);
  BuildModeDiffSpeedButton.Hint:=lisShowDifferencesBetweenModes;
end;

procedure TBuildModesForm.ActivateMode(aMode: TProjectBuildMode);
begin
  if aMode=FProject.ActiveBuildMode then exit;
  FSwitchingMode:=true;
  try
    // save changes
//    OnSaveIDEOptionsHook(Self,FProject.CompilerOptions);
    // switch
    FProject.ActiveBuildMode:=aMode;
    IncreaseBuildMacroChangeStamp;
    // load options
//    OnLoadIDEOptionsHook(Self,FProject.CompilerOptions);
  finally
    FSwitchingMode:=false;
  end;
end;

procedure TBuildModesForm.UpdateShowSession;
begin
  if LoadShowSessionFromProject then
    ShowSession:=(FProject<>nil)
             and (FProject.SessionStorage in [pssInProjectDir,pssInIDEConfig]);
end;

procedure TBuildModesForm.SetShowSession(const AValue: boolean);
begin
  if AValue=FShowSession then exit;
  FShowSession:=AValue;
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
  if FShowSession then
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
  if FProject<>nil then
  begin
    s := FProject.GetTitleOrName;
    s:=Format(dlgProjectOptionsFor, [s]);
    if FProject.BuildModes.Count>1 then
      s:=s+', '+copy(FProject.ActiveBuildMode.GetCaption,1,12);
  end else
    s:='TBuildModesEditorFrame.GetDialogCaption: no project';
  Caption:=s;
end;

function TBuildModesForm.GetSelectedBuildMode: TProjectBuildMode;
var
  i: LongInt;
begin
  Result:=nil;
  if FProject=nil then exit;
  i:=BuildModesStringGrid.Row-1;
  if (i<0) or (i>=FProject.BuildModes.Count) then exit;
  Result:=FProject.BuildModes[i];
end;

procedure TBuildModesForm.OKButtonClick(Sender: TObject);
begin
  ;
end;

procedure TBuildModesForm.CancelButtonClick(Sender: TObject);
begin
  ;
end;

end.

