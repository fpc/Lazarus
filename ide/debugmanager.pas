{  $Id$  }
{
 /***************************************************************************
                             debugmanager.pp
                             ---------------
      TDebugManager controls all debugging related stuff in the IDE.


 ***************************************************************************/

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}
unit DebugManager;

{$mode objfpc}{$H+}

interface

{$I ide.inc}

uses
{$IFDEF IDE_MEM_CHECK}
  MemCheck,
{$ENDIF}
  Classes, SysUtils, Forms, Controls, Dialogs, Menus, FileCtrl, Laz_XMLCfg,
  SynEdit, CodeCache, CodeToolManager,
  CompilerOptions, EditorOptions, EnvironmentOpts, KeyMapping, UnitEditor,
  Project, IDEProcs, InputHistory, Debugger, RunParamsOpts, ExtToolDialog,
  IDEOptionDefs, LazarusIDEStrConsts, ProjectDefs, BaseDebugManager, MainBar,
  SourceMarks,
  DebuggerDlg, Watchesdlg, BreakPointsdlg, LocalsDlg, DBGOutputForm,
  GDBMIDebugger, CallStackDlg;


type
  TDebugDialogType = (
    ddtOutput,
    ddtBreakpoints,
    ddtWatches,
    ddtLocals,
    ddtCallStack
    );
    
  TDebugManager = class(TBaseDebugManager)
    // Menu events
    procedure mnuViewDebugDialogClick(Sender: TObject);
    procedure mnuResetDebuggerClicked(Sender : TObject);

    // SrcNotebook events
    function OnSrcNotebookAddWatchesAtCursor(Sender: TObject): boolean;

    // Debugger events
    procedure OnDebuggerChangeState(ADebugger: TDebugger; OldState: TDBGState);
    procedure OnDebuggerCurrentLine(Sender: TObject;
                                    const ALocation: TDBGLocationRec);
    procedure OnDebuggerOutput(Sender: TObject; const AText: String);
    procedure OnDebuggerException(Sender: TObject; const AExceptionClass: String;
                                  const AExceptionText: String);
  private
    FDebugger: TDebugger;
    FDebuggerUpdateLock: integer;
    FBreakpointsNotification: TDBGBreakPointsNotification;// Notification for
      // our BreakPoints

    // When no debugger is created the IDE stores all debugger settings in its
    // own variables. When the debugger object is created these items point
    // to the corresponding items in the FDebugger object.
    FBreakPointGroups: TDBGBreakPointGroups;
    FWatches: TDBGWatches;
    FDialogs: array[TDebugDialogType] of TDebuggerDlg;

    // When a source file is not found, the user can choose one
    // Here are all choises stored
    FUserSourceFiles: TStringList;

    // Breakpoint routines
    procedure BreakpointAdded(const ASender: TDBGBreakPoints;
                              const ABreakpoint: TDBGBreakPoint);
    procedure BreakpointRemoved(const ASender: TDBGBreakPoints;
                                const ABreakpoint: TDBGBreakPoint);
    procedure CreateSourceMarkForBreakPoint(const ABreakpoint: TDBGBreakPoint;
                                            ASrcEdit: TSourceEditor);
    procedure GetSourceEditorForBreakPoint(const ABreakpoint: TDBGBreakPoint;
                                           var ASrcEdit: TSourceEditor);

    // Dialog routines
    procedure DebugDialogDestroy(Sender: TObject);
    procedure ViewDebugDialog(const ADialogType: TDebugDialogType);
    procedure DestroyDebugDialog(const ADialogType: TDebugDialogType);
    procedure InitDebugOutputDlg;
    procedure InitBreakPointDlg;
    procedure InitWatchesDlg;
    procedure InitLocalsDlg;
    procedure InitCallStackDlg;
  protected
    function  GetState: TDBGState; override;
    function  GetCommands: TDBGCommands; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure ConnectMainBarEvents; override;
    procedure ConnectSourceNotebookEvents; override;
    procedure SetupMainBarShortCuts; override;
    
    procedure LoadProjectSpecificInfo(XMLConfig: TXMLConfig); override;
    procedure SaveProjectSpecificInfo(XMLConfig: TXMLConfig); override;
    procedure DoRestoreDebuggerMarks(AnUnitInfo: TUnitInfo); override;
    procedure BeginUpdateDialogs;
    procedure EndUpdateDialogs;

    function DoInitDebugger: TModalResult; override;
    function DoPauseProject: TModalResult; override;
    function DoStepIntoProject: TModalResult; override;
    function DoStepOverProject: TModalResult; override;
    function DoRunToCursor: TModalResult; override;
    function DoStopProject: TModalResult; override;
    function DoBeginChangeDebugger: TModalResult;
    function DoEndChangeDebugger: TModalResult;

    procedure RunDebugger; override;
    procedure EndDebugging; override;
    function Evaluate(const AExpression: String;
                      var AResult: String): Boolean; override;

    function DoCreateBreakPoint(const AFilename: string;
                                ALine: integer): TModalResult; override;
    function DoDeleteBreakPoint(const AFilename: string;
                                ALine: integer): TModalResult; override;
    function DoDeleteBreakPointAtMark(
                        const ASourceMark: TSourceMark): TModalResult; override;
    function DoCreateWatch(const AExpression: string): TModalResult; override;
  end;
  
  

implementation

const
  DebugDlgIDEWindow: array[TDebugDialogType] of TNonModalIDEWindow = (
    nmiwDbgOutput,  nmiwBreakPoints, nmiwWatches, nmiwLocals, nmiwCallStack
  );
  
type
  TManagedBreakPoint = class(TDBGBreakPoint)
  private
    FMaster: TDBGBreakPoint;
    FSourceMark: TSourceMark;
    procedure SetSourceMark(const AValue: TSourceMark);
    procedure OnSourceMarkPositionChanged(Sender: TObject);
    procedure OnSourceMarkBeforeFree(Sender: TObject);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetHitCount: Integer; override;
    function GetValid: TValidState; override;
    procedure SetActions(const AValue: TDBGBreakPointActions); override;
    procedure SetEnabled(const AValue: Boolean); override;
    procedure SetExpression(const AValue: String); override;
    procedure UpdateSourceMarkImage;
    procedure UpdateSourceMarkLineColor;
    procedure UpdateSourceMark;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure ResetMaster;
    property SourceMark: TSourceMark read FSourceMark write SetSourceMark;
  end;
  
  TManagedBreakPoints = class(TDBGBreakPoints)
  private
    FMaster: TDBGBreakPoints;
    FMasterNotification: TDBGBreakPointsNotification;
    procedure SetMaster(const AValue: TDBGBreakPoints);
    procedure MasterUpdate(const ASender: TDBGBreakPoints;
                           const ABreakpoint: TDBGBreakPoint);
  public
    constructor Create;
    destructor Destroy; override;
    property Master: TDBGBreakPoints read FMaster write SetMaster;
  end;

{ TManagedBreakPoints }

constructor TManagedBreakPoints.Create;
begin
  FMaster := nil;
  FMasterNotification := TDBGBreakPointsNotification.Create;
  FMasterNotification.AddReference;
  FMasterNotification.OnUpdate := @MasterUpdate;

  inherited Create(nil, TManagedBreakPoint);
end;

destructor TManagedBreakPoints.Destroy;
begin
  FreeThenNil(FMasterNotification);
  inherited Destroy;
end;

procedure TManagedBreakPoints.MasterUpdate(const ASender: TDBGBreakPoints;
  const ABreakpoint: TDBGBreakPoint);

  function FindItem: TManagedBreakPoint;
  var
    n: Integer;
  begin
writeln('TManagedBreakPoints.MasterUpdate A ',ABreakpoint.ClassName,' ',ABreakpoint.Source,' ',ABreakpoint.Line);
    for n := 0 to Count - 1 do
    begin
      Result := TManagedBreakPoint(Items[n]);
      if Result.FMaster = ABreakPoint
      then Exit;
    end;
    Result := nil;
  end;
  
var
  bp: TManagedBreakPoint;
begin
  if ABreakPoint = nil
  then begin
    Update(nil);
  end
  else begin
    bp := FindItem;
    if bp <> nil then begin
      bp.UpdateSourceMark;
      Update(bp);
    end;
  end;
end;

procedure TManagedBreakPoints.SetMaster(const AValue: TDBGBreakPoints);
var
  n: Integer;
begin
  if FMaster = AValue then Exit;
  
  if FMaster <> nil
  then FMaster.RemoveNotification(FMasterNotification);
  
  FMaster := AValue;
  if FMaster = nil
  then begin
    for n := 0 to Count - 1 do
      TManagedBreakPoint(Items[n]).ResetMaster;
  end
  else begin
    FMaster.AddNotification(FMasterNotification);
    FMaster.Assign(Self);
  end;
end;


{ TManagedBreakPoint }

procedure TManagedBreakPoint.SetSourceMark(const AValue: TSourceMark);
begin
  if FSourceMark=AValue then exit;
  if FSourceMark<>nil then begin
    FSourceMark.RemoveAllHandlersForObject(Self);
    FSourceMark.Data:=nil;
  end;
  FSourceMark:=AValue;
  if FSourceMark<>nil then begin
    FSourceMark.AddPositionChangedHandler(@OnSourceMarkPositionChanged);
    FSourceMark.AddBeforeFreeHandler(@OnSourceMarkBeforeFree);
    FSourceMark.Data:=Self;
    FSourceMark.IsBreakPoint:=true;
    FSourceMark.Line:=Line;
    FSourceMark.Visible:=true;
    UpdateSourceMarkImage;
  end;
end;

procedure TManagedBreakPoint.OnSourceMarkPositionChanged(Sender: TObject);
begin

end;

procedure TManagedBreakPoint.OnSourceMarkBeforeFree(Sender: TObject);
begin
  SourceMark:=nil;
end;

procedure TManagedBreakPoint.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);
  if TManagedBreakPoints(GetOwner).FMaster <> nil
  then FMaster := TDBGBreakpoint(Dest);
end;

constructor TManagedBreakPoint.Create (ACollection: TCollection );
begin
  inherited Create(ACollection);
  FMaster := nil;
end;

destructor TManagedBreakPoint.Destroy;
begin
  FreeAndNil(FMaster);
  inherited Destroy;
end;

function TManagedBreakPoint.GetHitCount: Integer;
begin
  if FMaster = nil
  then Result := 0
  else Result := FMaster.HitCount;
end;

function TManagedBreakPoint.GetValid: TValidState;
begin
  if FMaster = nil
  then Result := vsUnknown
  else Result := FMaster.Valid;
end;

procedure TManagedBreakPoint.ResetMaster;
begin
  FMaster := nil;
  Changed(False);
end;

procedure TManagedBreakPoint.SetActions(const AValue: TDBGBreakPointActions);
begin
  if Actions=AValue then exit;
  inherited SetActions(AValue);
  if FMaster <> nil then FMaster.Actions := AValue;
end;

procedure TManagedBreakPoint.SetEnabled(const AValue: Boolean);
begin
  if Enabled=AValue then exit;
  inherited SetEnabled(AValue);
  if FMaster <> nil then FMaster.Enabled := AValue;
  UpdateSourceMarkImage;
end;

procedure TManagedBreakPoint.SetExpression(const AValue: String);
begin
  if AValue=Expression then exit;
  inherited SetExpression(AValue);
  if FMaster <> nil then FMaster.Expression := AValue;
end;

procedure TManagedBreakPoint.UpdateSourceMarkImage;
var
  Img: Integer;
begin
  if SourceMark=nil then exit;
  case Valid of
  vsValid:
    if Enabled then
      Img:=SourceEditorMarks.ActiveBreakPointImg
    else
      Img:=SourceEditorMarks.InactiveBreakPointImg;
  vsInvalid: Img:=SourceEditorMarks.InvalidBreakPointImg;
  else
    Img:=SourceEditorMarks.UnknownBreakPointImg;
  end;
  SourceMark.ImageIndex:=Img;
end;

procedure TManagedBreakPoint.UpdateSourceMarkLineColor;
var
  aha: TAdditionalHilightAttribute;
begin
  if SourceMark=nil then exit;
  aha:=ahaNone;
  case Valid of
  vsValid:
    if Enabled then
      aha:=ahaEnabledBreakpoint
    else
      aha:=ahaDisabledBreakpoint;
  vsInvalid: aha:=ahaInvalidBreakpoint;
  else
    aha:=ahaUnknownBreakpoint;
  end;
  SourceMark.LineColorAttrib:=aha;
end;

procedure TManagedBreakPoint.UpdateSourceMark;
begin
  UpdateSourceMarkImage;
  UpdateSourceMarkLineColor;
end;


//-----------------------------------------------------------------------------
// Menu events
//-----------------------------------------------------------------------------

procedure TDebugManager.mnuViewDebugDialogClick(Sender: TObject);
begin                       
  ViewDebugDialog(TDebugDialogType(TMenuItem(Sender).Tag));
end;

procedure TDebugManager.mnuResetDebuggerClicked(Sender: TObject);
begin
  if State = dsNone then Exit;

  EndDebugging;
  DoInitDebugger;
end;

//-----------------------------------------------------------------------------
// ScrNoteBook events
//-----------------------------------------------------------------------------

function TDebugManager.OnSrcNotebookAddWatchesAtCursor(Sender : TObject
  ): boolean;
var
  SE: TSourceEditor;
  WatchVar: String;
begin
  Result:=false;

  // get the sourceEditor.
  SE := TSourceNotebook(Sender).GetActiveSE;
  if not Assigned(SE) then Exit;
  WatchVar := SE.GetWordAtCurrentCaret;
  if WatchVar = ''  then Exit;

  if DoCreateWatch(WatchVar)<>mrOk then exit;
  Result:=true;
end;

//-----------------------------------------------------------------------------
// Debugger events
//-----------------------------------------------------------------------------

procedure TDebugManager.OnDebuggerException(Sender: TObject;
  const AExceptionClass: String; const AExceptionText: String);
var
  msg: String;
begin
  if Destroying then exit;
  
  if AExceptionText = ''
  then msg := Format('Project %s raised exception class ''%s''.', [Project1.Title, AExceptionClass])
  else msg := Format('Project %s raised exception class ''%s'' with message ''%s''.', [Project1.Title, AExceptionClass, AExceptionText]);

  MessageDlg('Error', msg, mtError,[mbOk],0);
end;

procedure TDebugManager.OnDebuggerOutput(Sender: TObject; const AText: String);
begin
  if Destroying then exit;
  if FDialogs[ddtOutput] <> nil
  then TDbgOutputForm(FDialogs[ddtOutput]).AddText(AText);
end;

procedure TDebugManager.OnDebuggerChangeState(ADebugger: TDebugger;
  OldState: TDBGState);
const
  // dsNone, dsIdle, dsStop, dsPause, dsRun, dsError
  TOOLSTATEMAP: array[TDBGState] of TIDEToolStatus = (
    // dsNone, dsIdle, dsStop, dsPause, dsRun,      dsError
    itNone, itNone, itNone, itDebugger, itDebugger, itDebugger
  );
  STATENAME: array[TDBGState] of string = (
    'dsNone', 'dsIdle', 'dsStop', 'dsPause', 'dsRun', 'dsError'
  );
var
  Editor: TSourceEditor;
begin
  if (ADebugger<>FDebugger) or (ADebugger=nil) then
    RaiseException('TDebugManager.OnDebuggerChangeState');

  if Destroying then exit;

  WriteLN('[TDebugManager.OnDebuggerChangeState] state: ', STATENAME[FDebugger.State]);

  // All conmmands
  // -------------------
  // dcRun, dcPause, dcStop, dcStepOver, dcStepInto, dcRunTo, dcJumpto, dcBreak, dcWatch
  // -------------------

  with MainIDE do begin
    // For 'run' and 'step' bypass 'idle', so we can set the filename later
    RunSpeedButton.Enabled := (dcRun in FDebugger.Commands) or (FDebugger.State = dsIdle);
    itmProjectRun.Enabled := RunSpeedButton.Enabled;
    PauseSpeedButton.Enabled := dcPause in FDebugger.Commands;
    itmProjectPause.Enabled := PauseSpeedButton.Enabled;
    StepIntoSpeedButton.Enabled := (dcStepInto in FDebugger.Commands) or (FDebugger.State = dsIdle);
    itmProjectStepInto.Enabled := StepIntoSpeedButton.Enabled;
    StepOverSpeedButton.Enabled := (dcStepOver in FDebugger.Commands)  or (FDebugger.State = dsIdle);
    itmProjectStepOver.Enabled := StepOverSpeedButton.Enabled;

    itmProjectRunToCursor.Enabled := dcRunTo in FDebugger.Commands;
    itmProjectStop.Enabled := dcStop in FDebugger.Commands;;

    // TODO: add other debugger menuitems
    // TODO: implement by actions

    ToolStatus := TOOLSTATEMAP[FDebugger.State];
  end;

  if (FDebugger.State in [dsRun]) then begin
    // hide IDE during run
    if EnvironmentOptions.HideIDEOnRun then
      MainIDE.HideIDE;
  end else if (OldState in [dsRun]) then begin
    // unhide IDE
    MainIDE.UnhideIDE;
  end;
  
  // unmark execution line
  if (FDebugger.State <> dsPause)
  and (SourceNotebook <> nil)
  then begin
    Editor := SourceNotebook.GetActiveSE;
    if Editor <> nil
    then Editor.ExecutionLine := -1;
  end;

  case FDebugger.State of 

  dsError:
    begin
      WriteLN('Ooops, the debugger entered the error state');
      MessageDlg(lisDebuggerError,
        Format(lisDebuggerErrorOoopsTheDebuggerEnteredTheErrorState, [#13#13,
          #13, #13#13]),
        mtError, [mbOK],0);
    end;

  dsStop:
    if (OldState<>dsIdle) then begin
      MessageDlg(lisExecutionStopped,
        Format(lisExecutionStoppedOn, [#13#13]),
        mtInformation, [mbOK],0);
    end;

  end;
end;

procedure TDebugManager.OnDebuggerCurrentLine(Sender: TObject;
  const ALocation: TDBGLocationRec);
// debugger paused program due to pause or error
// -> show the current execution line in editor
// if SrcLine = -1 then no source is available
var
  S, SrcFile: String;
  OpenDialog: TOpenDialog;
  NewSource: TCodeBuffer;
  Editor: TSourceEditor;
  n: Integer;
begin
  if (Sender<>FDebugger) or (Sender=nil) then exit;
  if Destroying then exit;

  //TODO: Show assembler window if no source can be found.
  if ALocation.SrcLine = -1 
  then begin
    MessageDlg(lisExecutionPaused,
      Format(lisExecutionPausedAdress, [#13#13, ALocation.Adress, #13,
        ALocation.FuncName, #13, ALocation.SrcFile, #13#13#13, #13]),
      mtInformation, [mbOK],0);
    
    Exit;
  end;

  SrcFile := MainIDE.FindSourceFile(ALocation.SrcFile,Project1.ProjectDirectory,
                      [fsfSearchForProject,fsfUseIncludePaths,fsfUseDebugPath]);
  if SrcFile = '' then SrcFile := ALocation.SrcFile;
  
  if not FilenameIsAbsolute(SrcFile)
  then begin
    // first attempt to get a longer name
    // short file, look in the user list
    for n := 0 to FUserSourceFiles.Count - 1 do
    begin
      S := FUserSourceFiles[n];
      if CompareFileNames(ExtractFilenameOnly(SrcFile), ExtractFilenameOnly(S)) = 0
      then begin
        if FileExists(S)
        then begin
          FUserSourceFiles.Move(n, 0); // move most recent first
          SrcFile := S;
          Break;
        end;
      end;
    end;
  end;
    
  if (not FilenameIsAbsolute(SrcFile)) or (not FileExists(SrcFile))
  then begin

    if MessageDlg(lisFileNotFound,
      Format(lisTheFileWasNotFoundDoYouWantToLocateItYourself, ['"',
        SrcFile, '"', #13, #13, #13])
      ,mtConfirmation, [mbYes, mbNo], 0) <> mrYes
    then Exit;

    repeat
      OpenDialog:=TOpenDialog.Create(Application);
      try
        InputHistories.ApplyFileDialogSettings(OpenDialog);
        OpenDialog.Title:=lisOpenFile+' '+SrcFile;
        OpenDialog.Options:=OpenDialog.Options+[ofFileMustExist];
        OpenDialog.FileName := SrcFile;
        if not OpenDialog.Execute then
          exit;
        SrcFile:=CleanAndExpandFilename(OpenDialog.FileName);
        InputHistories.StoreFileDialogSettings(OpenDialog);
      finally
        OpenDialog.Free;
      end;
    until FilenameIsAbsolute(SrcFile) and FileExists(SrcFile);

    FUserSourceFiles.Insert(0, SrcFile);
  end;

  NewSource:=CodeToolBoss.LoadFile(SrcFile,true,false);
  if NewSource=nil then begin
    exit;
  end;

  // clear old error and execution lines
  if SourceNotebook<>nil then begin
    SourceNotebook.ClearExecutionLines;
    SourceNotebook.ClearErrorLines;
  end;
  
  // jump editor to execution line
  if MainIDE.DoJumpToCodePos(nil,nil,NewSource,1,ALocation.SrcLine,-1,true)
    <>mrOk then exit;

  // mark execution line
  if SourceNotebook <> nil
  then Editor := SourceNotebook.GetActiveSE
  else Editor := nil;

  if Editor <> nil
  then Editor.ExecutionLine:=ALocation.SrcLine;
end;

//-----------------------------------------------------------------------------
// Debugger dialog routines
//-----------------------------------------------------------------------------

// Common handler
// The tag of the destroyed form contains the form variable pointing to it
procedure TDebugManager.DebugDialogDestroy(Sender: TObject);
begin
  if  (TForm(Sender).Tag >= Ord(Low(TDebugDialogType)))
  and (TForm(Sender).Tag <= Ord(High(TDebugDialogType)))
  then FDialogs[TDebugDialogType(TForm(Sender).Tag)] := nil;
end;

procedure TDebugManager.ViewDebugDialog(const ADialogType: TDebugDialogType);
const
  DEBUGDIALOGCLASS: array[TDebugDialogType] of TDebuggerDlgClass = (
    TDbgOutputForm, TBreakPointsDlg, TWatchesDlg, TLocalsDlg, TCallStackDlg
  );
var
  CurDialog: TDebuggerDlg;
begin
  if Destroying then exit;
  if FDialogs[ADialogType] = nil
  then begin
    FDialogs[ADialogType] := DEBUGDIALOGCLASS[ADialogType].Create(Self);
    CurDialog:=FDialogs[ADialogType];
    CurDialog.Name:=NonModalIDEWindowNames[DebugDlgIDEWindow[ADialogType]];
    CurDialog.Tag := Integer(ADialogType);
    CurDialog.OnDestroy := @DebugDialogDestroy;
    EnvironmentOptions.IDEWindowLayoutList.Apply(CurDialog,CurDialog.Name);
    case ADialogType of
    ddtOutput:      InitDebugOutputDlg;
    ddtBreakpoints: InitBreakPointDlg;
    ddtWatches:     InitWatchesDlg;
    ddtLocals:      InitLocalsDlg;
    ddtCallStack:   InitCallStackDlg;
    end;
    //DoInitDebugger;
    CurDialog.Debugger := FDebugger;
  end else begin
    CurDialog:=FDialogs[ADialogType];
    if (CurDialog is TBreakPointsDlg) then begin
      if (Project1<>nil) then
        TBreakPointsDlg(CurDialog).BaseDirectory:=Project1.ProjectDirectory;
    end;
  end;
  FDialogs[ADialogType].Show;
  FDialogs[ADialogType].BringToFront;
end;

procedure TDebugManager.DestroyDebugDialog(const ADialogType: TDebugDialogType);
begin
  if FDialogs[ADialogType] = nil then Exit;
  FDialogs[ADialogType].OnDestroy := nil;
  FDialogs[ADialogType].Debugger := nil;
  FDialogs[ADialogType].Free;
  FDialogs[ADialogType] := nil;
end;

procedure TDebugManager.InitDebugOutputDlg;
begin
end;

procedure TDebugManager.InitBreakPointDlg;
var
  TheDialog: TBreakPointsDlg;
begin
  TheDialog:=TBreakPointsDlg(FDialogs[ddtBreakpoints]);
  if (Project1<>nil) then
    TheDialog.BaseDirectory:=Project1.ProjectDirectory;
  TheDialog.BreakPoints:=FBreakPoints;
end;

procedure TDebugManager.InitWatchesDlg;
var
  TheDialog: TWatchesDlg;
begin
  TheDialog:=TWatchesDlg(FDialogs[ddtWatches]);
  TheDialog.WatchesUpdate(FWatches);
end;

procedure TDebugManager.InitLocalsDlg;
begin
end;

procedure TDebugManager.InitCallStackDlg;
begin
end;

constructor TDebugManager.Create(TheOwner: TComponent);
var
  DialogType: TDebugDialogType;
begin
  for DialogType := Low(TDebugDialogType) to High(TDebugDialogType) do
    FDialogs[DialogType] := nil; 

  FDebugger := nil;
  FBreakPoints := TManagedBreakPoints.Create;
  FBreakpointsNotification := TDBGBreakPointsNotification.Create;
  FBreakpointsNotification.AddReference;
  FBreakpointsNotification.OnAdd := @BreakpointAdded;
  FBreakpointsNotification.OnRemove := @BreakpointRemoved;
  FBreakPoints.AddNotification(FBreakpointsNotification);
  
  FBreakPointGroups := TDBGBreakPointGroups.Create;
  FWatches := TDBGWatches.Create(nil, TDBGWatch);
  
  FUserSourceFiles := TStringList.Create;
  
  inherited Create(TheOwner);
end;

destructor TDebugManager.Destroy;
var
  DialogType: TDebugDialogType;
begin                          
  FDestroying:=true;
  
  for DialogType := Low(TDebugDialogType) to High(TDebugDialogType) do 
    DestroyDebugDialog(DialogType);
  
  TManagedBreakpoints(FBreakpoints).Master := nil;

  if FDebugger <> nil
  then begin
//@@    if FDebugger.BreakPoints = FBreakPoints
//@@    then FBreakPoints := nil;
    if FDebugger.BreakPointGroups = FBreakPointGroups
    then FBreakPointGroups := nil;
    if FDebugger.Watches = FWatches
    then FWatches := nil;
  
    FreeThenNil(FDebugger);
  end
  else begin
//@@    FreeThenNil(FBreakPoints);
    FreeThenNil(FBreakPointGroups);
    FreeThenNil(FWatches);
  end;
  
  FreeThenNil(FBreakPoints);
  FreeThenNil(FBreakpointsNotification);
  FreeThenNil(FUserSourceFiles);

  inherited Destroy;
end;

procedure TDebugManager.ConnectMainBarEvents;
begin
  with MainIDE do begin
    itmViewWatches.OnClick := @mnuViewDebugDialogClick;
    itmViewWatches.Tag := Ord(ddtWatches);
    itmViewBreakPoints.OnClick := @mnuViewDebugDialogClick;
    itmViewBreakPoints.Tag := Ord(ddtBreakPoints);
    itmViewLocals.OnClick := @mnuViewDebugDialogClick;
    itmViewLocals.Tag := Ord(ddtLocals);
    itmViewCallStack.OnClick := @mnuViewDebugDialogClick;
    itmViewCallStack.Tag := Ord(ddtCallStack);
    itmViewDebugOutput.OnClick := @mnuViewDebugDialogClick;
    itmViewDebugOutput.Tag := Ord(ddtOutput);

    itmProjectResetDebugger.OnClick := @mnuResetDebuggerClicked;
  end;
end;

procedure TDebugManager.ConnectSourceNotebookEvents;
begin
  SourceNotebook.OnAddWatchAtCursor := @OnSrcNotebookAddWatchesAtCursor;
end;

procedure TDebugManager.SetupMainBarShortCuts;
begin
  with MainIDE, EditorOpts.KeyMap do
  begin
    itmViewWatches.ShortCut := CommandToShortCut(ecToggleWatches);
    itmViewBreakpoints.ShortCut := CommandToShortCut(ecToggleBreakPoints);
    itmViewDebugOutput.ShortCut := CommandToShortCut(ecToggleDebuggerOut);
    itmViewLocals.ShortCut := CommandToShortCut(ecToggleLocals);
    itmViewCallStack.ShortCut := CommandToShortCut(ecToggleCallStack);
  end;
end;

{------------------------------------------------------------------------------
  procedure TDebugManager.LoadProjectSpecificInfo(XMLConfig: TXMLConfig);

  Called when the main project is loaded from the XMLConfig.
------------------------------------------------------------------------------}
procedure TDebugManager.LoadProjectSpecificInfo(XMLConfig: TXMLConfig);
begin
  if FDebugger=nil then begin
    FBreakPointGroups.LoadFromXMLConfig(XMLConfig,
                                      'Debugging/'+XMLBreakPointGroupsNode+'/');
    FBreakPoints.LoadFromXMLConfig(XMLConfig,'Debugging/'+XMLBreakPointsNode+'/',
                                 @Project1.LongenFilename,
                                 @FBreakPointGroups.GetGroupByName);
    FWatches.LoadFromXMLConfig(XMLConfig,'Debugging/'+XMLWatchesNode+'/');
  end else begin
    FDebugger.LoadFromXMLConfig(XMLConfig,'Debugging/',@Project1.LongenFilename);
  end;
end;

{------------------------------------------------------------------------------
  procedure TDebugManager.SaveProjectSpecificInfo(XMLConfig: TXMLConfig);

  Called when the main project is saved to an XMLConfig.
------------------------------------------------------------------------------}
procedure TDebugManager.SaveProjectSpecificInfo(XMLConfig: TXMLConfig);
begin
  if FDebugger=nil then begin
    FBreakPointGroups.SaveToXMLConfig(XMLConfig,
                                      'Debugging/'+XMLBreakPointGroupsNode+'/');
    FBreakPoints.SaveToXMLConfig(XMLConfig,'Debugging/'+XMLBreakPointsNode+'/',
                                 @Project1.ShortenFilename);
    FWatches.SaveToXMLConfig(XMLConfig,'Debugging/'+XMLWatchesNode+'/');
  end else begin
    FDebugger.SaveToXMLConfig(XMLConfig,'Debugging/',@Project1.ShortenFilename);
  end;
end;

procedure TDebugManager.DoRestoreDebuggerMarks(AnUnitInfo: TUnitInfo);
var
  ASrcEdit: TSourceEditor;
  i: Integer;
  CurBreakPoint: TDBGBreakPoint;
  SrcFilename: String;
begin
  if (AnUnitInfo.EditorIndex<0) or Destroying then exit;
  ASrcEdit:=SourceNotebook.Editors[AnUnitInfo.EditorIndex];
  // set breakpoints for this unit
  SrcFilename:=AnUnitInfo.Filename;
  for i:=0 to FBreakpoints.Count-1 do begin
    CurBreakPoint:=FBreakpoints[i];
    if CompareFileNames(CurBreakPoint.Source,SrcFilename)=0 then
      CreateSourceMarkForBreakPoint(CurBreakPoint,ASrcEdit);
  end;
end;

procedure TDebugManager.BeginUpdateDialogs;
var
  DialogType: TDebugDialogType;
  CurDialog: TDebuggerDlg;
begin
  for DialogType:=Low(FDialogs) to High(FDialogs) do begin
    CurDialog:=FDialogs[DialogType];
    if CurDialog<>nil then CurDialog.BeginUpdate;
  end;
end;

procedure TDebugManager.BreakpointAdded(const ASender: TDBGBreakPoints;
  const ABreakpoint: TDBGBreakPoint);
var
  BP: TDBGBreakPoint;
begin
writeln('TDebugManager.BreakpointAdded A ',ABreakpoint.Source,' ',ABreakpoint.Line);
  ABreakpoint.InitialEnabled := True;
  ABreakpoint.Enabled := True;
  if FDebugger <> nil
  then begin
    BP := FDebugger.BreakPoints.Add(ABreakpoint.Source, ABreakpoint.Line);
    BP.Assign(ABreakPoint);
  end;
  CreateSourceMarkForBreakPoint(ABreakpoint,nil);
  Project1.Modified := True;
end;

procedure TDebugManager.BreakpointRemoved(const ASender: TDBGBreakPoints;
  const ABreakpoint: TDBGBreakPoint);
begin
writeln('TDebugManager.BreakpointRemoved A ',ABreakpoint.Source,' ',ABreakpoint.Line,' ',TManagedBreakPoint(ABreakpoint).SourceMark<>nil);
  if TManagedBreakPoint(ABreakpoint).SourceMark<>nil then
    TManagedBreakPoint(ABreakpoint).SourceMark.Free;
  if Project1<>nil then
    Project1.Modified := True;
end;

procedure TDebugManager.CreateSourceMarkForBreakPoint(
  const ABreakpoint: TDBGBreakPoint; ASrcEdit: TSourceEditor);
var
  ManagedBreakPoint: TManagedBreakPoint;
  NewSrcMark: TSourceMark;
begin
  if not (ABreakpoint is TManagedBreakPoint) then
    RaiseException('TDebugManager.CreateSourceMarkForBreakPoint');
  ManagedBreakPoint:=TManagedBreakPoint(ABreakpoint);

  if (ManagedBreakPoint.SourceMark<>nil) or Destroying then exit;
  if ASrcEdit=nil then
    GetSourceEditorForBreakPoint(ManagedBreakPoint,ASrcEdit);
  if ASrcEdit=nil then exit;
  NewSrcMark:=TSourceMark.Create(ASrcEdit.EditorComponent,nil);
  SourceEditorMarks.Add(NewSrcMark);
  ManagedBreakPoint.SourceMark:=NewSrcMark;
  ASrcEdit.EditorComponent.Marks.Add(NewSrcMark);
end;

procedure TDebugManager.GetSourceEditorForBreakPoint(
  const ABreakpoint: TDBGBreakPoint; var ASrcEdit: TSourceEditor);
var
  Filename: String;
begin
  Filename:=ABreakpoint.Source;
  if Filename<>'' then
    ASrcEdit:=SourceNotebook.FindSourceEditorWithFilename(ABreakpoint.Source)
  else
    ASrcEdit:=nil;
end;

procedure TDebugManager.EndUpdateDialogs;
var
  DialogType: TDebugDialogType;
  CurDialog: TDebuggerDlg;
begin
  for DialogType:=Low(FDialogs) to High(FDialogs) do begin
    CurDialog:=FDialogs[DialogType];
    if CurDialog<>nil then CurDialog.EndUpdate;
  end;
end;

//-----------------------------------------------------------------------------
// Debugger routines
//-----------------------------------------------------------------------------

function TDebugManager.DoInitDebugger: TModalResult;
var
//@@  OldBreakpoints: TDBGBreakpoints;
//@@  OldBreakPointGroups: TDBGBreakPointGroups;
  OldWatches: TDBGWatches;
  
  procedure ResetDialogs;
  var
    DialogType: TDebugDialogType;
  begin                          
    for DialogType := Low(TDebugDialogType) to High(TDebugDialogType) do 
    begin
      if FDialogs[DialogType] <> nil
      then FDialogs[DialogType].Debugger := FDebugger;
    end;
  end;
  
  procedure SaveDebuggerItems;
  begin
    // copy the break point list without the group references
//@@    OldBreakpoints := TDBGBreakpoints.Create(nil, TDBGBreakpoint);
//@@    OldBreakpoints.Assign(FBreakPoints);
    
    // copy the groups and all group references
//@@    OldBreakPointGroups := TDBGBreakPointGroups.Create;
//@@    OldBreakPointGroups.Regroup(FBreakPointGroups,FBreakPoints,OldBreakPoints);

    // copy the watches
    OldWatches := TDBGWatches.Create(nil, TDBGWatch);
    OldWatches.Assign(FWatches);

//@@    FBreakPointGroups := nil;
//@@    FBreakPoints := nil;
    FWatches := nil;
  end;
  
  procedure RestoreDebuggerItems;
  begin
    // restore the break point list without the group references
//@@    if (OldBreakpoints<>nil) then
//@@      FBreakPoints.Assign(OldBreakpoints);

    // restore the groups and all group references
//@@    if (OldBreakPointGroups<>nil) then begin
//@@      if (OldBreakpoints<>nil) then
//@@        FBreakPointGroups.Regroup(OldBreakPointGroups,OldBreakpoints,
//@@                                  FBreakPoints)
//@@      else
//@@        FBreakPointGroups.Assign(OldBreakPointGroups);
//@@    end;

    // restore the watches
    if OldWatches<>nil then
      FWatches.Assign(OldWatches);
  end;
  
  procedure FreeDebugger;
  begin
    SaveDebuggerItems;
    TManagedBreakPoints(FBreakPoints).Master := nil;
    FDebugger.Free;
    FDebugger := nil;
    ResetDialogs;
  end;

var
  LaunchingCmdLine, LaunchingApplication, LaunchingParams: String;
begin
  WriteLN('[TDebugManager.DoInitDebugger] A');

  Result:=mrCancel;
  if (Project1.MainUnitID < 0) or Destroying then Exit;

  LaunchingCmdLine:=MainIDE.GetRunCommandLine;
  SplitCmdLine(LaunchingCmdLine,LaunchingApplication,LaunchingParams);
  if (not FileExists(LaunchingApplication)) then exit;

//@@  OldBreakpoints := nil;
//@@  OldBreakPointGroups := nil;
  OldWatches := nil;

  BeginUpdateDialogs;
  try
    try
      case EnvironmentOptions.DebuggerType of
        dtGnuDebugger: begin
          // check if debugger is already created with the right type
          if (FDebugger <> nil)
          and ( not(FDebugger is TGDBMIDebugger)
                or (FDebugger.ExternalDebugger <> EnvironmentOptions.DebuggerFilename)
              )
          then begin
            // the current debugger is the wrong type -> free it
            FreeDebugger;
          end;
          // create debugger object
          if FDebugger = nil
          then begin
            SaveDebuggerItems;
            FDebugger := TGDBMIDebugger.Create(EnvironmentOptions.DebuggerFilename);
//@@            FBreakPointGroups := FDebugger.BreakPointGroups;
//@@            FBreakPoints := FDebugger.BreakPoints;

            TManagedBreakPoints(FBreakPoints).Master := FDebugger.BreakPoints; //!!
            
            FWatches := FDebugger.Watches;
            ResetDialogs;
          end;
          // restore debugger items
          RestoreDebuggerItems;
        end;
      else
        if FDebugger=nil then
          FreeDebugger;
        exit;
      end;
    finally
//@@      OldBreakpoints.Free;
//@@      OldBreakPointGroups.Free;
      OldWatches.Free;
    end;
    FDebugger.OnState     := @OnDebuggerChangeState;
    FDebugger.OnCurrent   := @OnDebuggerCurrentLine;
    FDebugger.OnDbgOutput := @OnDebuggerOutput;
    FDebugger.OnException := @OnDebuggerException;
    if FDebugger.State = dsNone
    then FDebugger.Init;

    FDebugger.FileName := LaunchingApplication;
    FDebugger.Arguments := LaunchingParams;
    Project1.RunParameterOptions.AssignEnvironmentTo(FDebugger.Environment);

    if FDialogs[ddtOutput] <> nil
    then TDbgOutputForm(FDialogs[ddtOutput]).Clear;

    //TODO: Show/hide debug menuitems based on FDebugger.SupportedCommands
  finally
    EndUpdateDialogs;
  end;

  Result := mrOk;
  WriteLN('[TDebugManager.DoInitDebugger] END');
end;

// still part of main, should go here when dummydebugger is finished
//
//function TDebugManager.DoRunProject: TModalResult;

function TDebugManager.DoPauseProject: TModalResult;
begin
  Result := mrCancel;
  if (MainIDE.ToolStatus <> itDebugger)
  or (FDebugger = nil) or Destroying
  then Exit;
  FDebugger.Pause;
  Result := mrOk;
end;

function TDebugManager.DoStepIntoProject: TModalResult;
begin
  if (MainIDE.DoInitProjectRun <> mrOK)
  or (MainIDE.ToolStatus <> itDebugger)
  or (FDebugger = nil) or Destroying
  then begin
    Result := mrAbort;
    Exit;
  end;

  FDebugger.StepInto;
  Result := mrOk;
end;

function TDebugManager.DoStepOverProject: TModalResult;
begin
  if (MainIDE.DoInitProjectRun <> mrOK)
  or (MainIDE.ToolStatus <> itDebugger)
  or (FDebugger = nil) or Destroying
  then begin
    Result := mrAbort;
    Exit;
  end;

  FDebugger.StepOver;
  Result := mrOk;
end;

function TDebugManager.DoStopProject: TModalResult;
begin
  Result := mrCancel;
  SourceNotebook.ClearExecutionLines;
  if (MainIDE.ToolStatus <> itDebugger)
  or (FDebugger=nil) or Destroying
  then Exit;

  FDebugger.Stop;
  Result := mrOk;
end;

function TDebugManager.DoBeginChangeDebugger: TModalResult;
begin
  inc(FDebuggerUpdateLock);
  if FDebuggerUpdateLock=1 then begin
    // the update has begun
    if FDebugger<>nil then begin
      // switch the debugger into a state, where the IDE can change its items

      // quick hack: simply forbid editing during run
      if FDebugger.State in dsRunStates then begin
        Result:=mrCancel;
        MessageDlg('Program is running',
          'You can not change any debugger item while the program is running.'#13
          +'Pause or Stop it first.',mtError,[mbCancel],0);
      end else
        Result:=mrOk;
      if Result<>mrOk then begin
        dec(FDebuggerUpdateLock);
        exit;
      end;
    end;
  end;
  Result:=mrOk;
end;

function TDebugManager.DoEndChangeDebugger: TModalResult;
begin
  if FDebuggerUpdateLock<=0 then
    RaiseException('TDebugManager.DoEndChangeDebugger');
  dec(FDebuggerUpdateLock);
  if FDebuggerUpdateLock=0 then begin
    // the update has ended -> restore the debugger state
    if FDebugger<>nil then begin
      Result:=mrOk;
      
      if Result<>mrOk then exit;
    end;
  end;
  Result:=mrOk;
end;

procedure TDebugManager.RunDebugger;
begin
  if Destroying then exit;
  if (FDebugger <> nil) then FDebugger.Run;
end;

procedure TDebugManager.EndDebugging;
begin
  if FDebugger <> nil then FDebugger.Done;
end;

function TDebugManager.Evaluate(const AExpression: String;
  var AResult: String): Boolean;
begin
  Result := (not Destroying)
        and (MainIDE.ToolStatus = itDebugger)
        and (dcEvaluate in DebugBoss.Commands)
        and (FDebugger <> nil)
        and FDebugger.Evaluate(AExpression, AResult)
end;

function TDebugManager.DoCreateBreakPoint(const AFilename: string;
  ALine: integer): TModalResult;
begin
  Result:=DoBeginChangeDebugger;
  if Result<>mrOk then exit;
  FBreakPoints.Add(AFilename, ALine);
  Result:=DoEndChangeDebugger;
end;

function TDebugManager.DoDeleteBreakPoint(const AFilename: string;
  ALine: integer): TModalResult;
var
  OldBreakPoint: TDBGBreakPoint;
begin
  Result:=DoBeginChangeDebugger;
  if Result<>mrOk then exit;
  OldBreakPoint:=FBreakPoints.Find(AFilename,ALine);
  if OldBreakPoint=nil then exit;
  OldBreakPoint.Free;
  Project1.Modified:=true;
  Result:=DoEndChangeDebugger;
end;

function TDebugManager.DoDeleteBreakPointAtMark(const ASourceMark: TSourceMark
  ): TModalResult;
var
  OldBreakPoint: TDBGBreakPoint;
begin
  // consistency check
  if (ASourceMark=nil) or (not ASourceMark.IsBreakPoint)
  or (ASourceMark.Data=nil) or (not (ASourceMark.Data is TDBGBreakPoint)) then
    RaiseException('TDebugManager.DoDeleteBreakPointAtMark');
  
writeln('TDebugManager.DoDeleteBreakPointAtMark A ',ASourceMark.GetFilename,' ',ASourceMark.Line);
  Result:=DoBeginChangeDebugger;
  if Result<>mrOk then exit;
  OldBreakPoint:=TDBGBreakPoint(ASourceMark.Data);
writeln('TDebugManager.DoDeleteBreakPointAtMark B ',OldBreakPoint.ClassName,' ',OldBreakPoint.Source,' ',OldBreakPoint.Line);
  OldBreakPoint.Free;
  Project1.Modified:=true;
  Result:=DoEndChangeDebugger;
end;

function TDebugManager.DoCreateWatch(const AExpression: string): TModalResult;
var
  NewWatch: TDBGWatch;
begin
  Result:=DoBeginChangeDebugger;
  if Result<>mrOk then exit;
  NewWatch := FWatches.Add(AExpression);
  NewWatch.Enabled := True;
  NewWatch.InitialEnabled := True;
  Project1.Modified:=true;
  Result:=DoEndChangeDebugger;
end;

function TDebugManager.DoRunToCursor: TModalResult;
var
  ActiveSrcEdit: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
  UnitFilename: string;
begin
  if (MainIDE.DoInitProjectRun <> mrOK)
  or (MainIDE.ToolStatus <> itDebugger)
  or (FDebugger = nil) or Destroying
  then begin
    Result := mrAbort;
    Exit;
  end;

  Result := mrCancel;

  MainIDE.GetCurrentUnit(ActiveSrcEdit, ActiveUnitInfo);
  if (ActiveSrcEdit=nil) or (ActiveUnitInfo=nil)
  then begin
    MessageDlg(lisRunToFailed, lisPleaseOpenAUnitBeforeRun, mtError,
      [mbCancel],0);
    Exit;
  end;

  if not ActiveUnitInfo.Source.IsVirtual
  then UnitFilename:=ActiveUnitInfo.Filename
  else UnitFilename:=MainIDE.GetTestUnitFilename(ActiveUnitInfo);

  FDebugger.RunTo(ExtractFilename(UnitFilename),
                  ActiveSrcEdit.EditorComponent.CaretY);

  Result := mrOK;
end;

function TDebugManager.GetState: TDBGState;       
begin
  if FDebugger = nil
  then Result := dsNone
  else Result := FDebugger.State;
end;

function TDebugManager.GetCommands: TDBGCommands; 
begin
  if FDebugger = nil
  then Result := []
  else Result := FDebugger.Commands;
end;

end.

{ =============================================================================
  $Log$
  Revision 1.37  2003/05/29 17:40:10  marc
  MWE: * Fixed string resolving
       * Updated exception handling

  Revision 1.36  2003/05/29 07:25:02  mattias
  added Destroying flag, debugger now always shuts down

  Revision 1.35  2003/05/28 22:43:21  marc
  MWE: * Fixed adding/removing breakpoints while paused

  Revision 1.34  2003/05/28 15:56:19  mattias
  implemented sourcemarks

  Revision 1.33  2003/05/28 09:00:35  mattias
  watches dialog now without DoInitDebugger

  Revision 1.32  2003/05/28 08:46:23  mattias
  break;points dialog now gets the items without debugger

  Revision 1.31  2003/05/28 00:58:50  marc
  MWE: * Reworked breakpoint handling

  Revision 1.30  2003/05/27 20:58:12  mattias
  implemented enable and deleting breakpoint in breakpoint dlg

  Revision 1.29  2003/05/27 15:04:00  mattias
  small fixes for debugger without file

  Revision 1.28  2003/05/27 08:01:31  marc
  MWE: + Added exception break
       * Reworked adding/removing breakpoints
       + Added Unknown breakpoint type

  Revision 1.27  2003/05/26 11:08:20  mattias
  fixed double breakpoints

  Revision 1.26  2003/05/26 10:34:46  mattias
  implemented search, fixed double loading breakpoints

  Revision 1.25  2003/05/25 15:31:11  mattias
  implemented searching for indirect include files

  Revision 1.24  2003/05/25 12:12:36  mattias
  added TScreen handlers, implemented TMainIDE.UnHideIDE

  Revision 1.23  2003/05/24 17:51:34  marc
  MWE: Added an usersource history

  Revision 1.22  2003/05/24 11:06:43  mattias
  started Hide IDE on run

  Revision 1.21  2003/05/23 18:50:07  mattias
  implemented searching debugging files in inherited unit paths

  Revision 1.20  2003/05/23 16:46:13  mattias
  added message, that debugger is readonly while running

  Revision 1.19  2003/05/23 14:12:50  mattias
  implemented restoring breakpoints

  Revision 1.18  2003/05/22 17:06:49  mattias
  implemented InitialEnabled for breakpoints and watches

  Revision 1.17  2003/05/22 06:50:04  mattias
  fixed double formats

  Revision 1.16  2003/05/21 16:19:12  mattias
  implemented saving breakpoints and watches

  Revision 1.15  2003/05/20 21:41:07  mattias
  started loading/saving breakpoints

  Revision 1.14  2003/05/18 10:42:57  mattias
  implemented deleting empty submenus

  Revision 1.13  2003/05/03 23:00:33  mattias
  localization

  Revision 1.12  2003/04/02 17:06:27  mattias
  improved deb creation

  Revision 1.11  2003/03/11 09:57:51  mattias
  implemented ProjectOpt: AutoCreateNewForms, added designer Show Options

  Revision 1.10  2002/12/09 16:48:34  mattias
  added basic file handling functions to filectrl

  Revision 1.9  2002/11/05 22:41:13  lazarus
  MWE:
    * Some minor debugger updates
    + Added evaluate to debugboss
    + Added hint debug evaluation

  Revision 1.8  2002/10/02 00:17:03  lazarus
  MWE:
    + Honoured the ofQuiet flag in DoOpenNotExistingFile, so custom messages
      can be shown
    + Added a dialog to make custom locate of a debug file possible

}
