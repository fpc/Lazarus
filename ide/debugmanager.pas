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
    function OnSrcNotebookCreateBreakPoint(Sender: TObject;
                                           Line: Integer): boolean;
    function OnSrcNotebookDeleteBreakPoint(Sender: TObject;
                                           Line: Integer): boolean;

    // Debugger events
    procedure OnDebuggerChangeState(ADebugger: TDebugger; OldState: TDBGState);
    procedure OnDebuggerCurrentLine(Sender: TObject;
                                    const ALocation: TDBGLocationRec);
    procedure OnDebuggerOutput(Sender: TObject; const AText: String);
    procedure OnDebuggerException(Sender: TObject; const AExceptionID: Integer;
                                  const AExceptionText: String);
  private
    FDebugger: TDebugger;
    FDebuggerUpdateLock: integer;
    // When no debugger is created the IDE stores all debugger settings in its
    // own variables. When the debugger object is created these items point
    // to the corresponding items in the FDebugger object.
    FBreakPoints: TDBGBreakPoints;
    FBreakPointGroups: TDBGBreakPointGroups;
    FWatches: TDBGWatches;
    FDialogs: array[TDebugDialogType] of TDebuggerDlg;

    // When a source file is not found, the user can choose one
    // Here are all choises stored
    FUserSourceFiles: TStringList;

    procedure DebugDialogDestroy(Sender: TObject);
    procedure ViewDebugDialog(const ADialogType: TDebugDialogType);
    procedure DestroyDebugDialog(const ADialogType: TDebugDialogType);
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
                                Line: integer): TModalResult;
    function DoDeleteBreakPoint(const AFilename: string;
                                Line: integer): TModalResult;
    function DoCreateWatch(const VariableName: string): TModalResult;
  end;
  
  

implementation

const
  DebugDlgIDEWindow: array[TDebugDialogType] of TNonModalIDEWindow = (
    nmiwDbgOutput,  nmiwBreakPoints, nmiwWatches, nmiwLocals, nmiwCallStack
  );

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

function TDebugManager.OnSrcNotebookCreateBreakPoint(Sender: TObject;
  Line: Integer): boolean;
var
  AFilename: String;
begin
  Result:=false;
  if SourceNotebook.Notebook = nil then Exit;

  AFilename:=TSourceNotebook(Sender).GetActiveSE.FileName;
  if DoCreateBreakPoint(AFilename,Line)<>mrOk then exit;
  Result:=true;
end;

function TDebugManager.OnSrcNotebookDeleteBreakPoint(Sender: TObject;
  Line: Integer): boolean;
var
  AFilename: String;
begin
  Result:=false;
  if SourceNotebook.Notebook = nil then Exit;

  AFilename:=TSourceNotebook(Sender).GetActiveSE.FileName;
  if DoDeleteBreakPoint(AFilename,Line)<>mrOk then exit;
  Result:=true;
end;

//-----------------------------------------------------------------------------
// Debugger events
//-----------------------------------------------------------------------------

procedure TDebugManager.OnDebuggerException(Sender: TObject;
  const AExceptionID: Integer; const AExceptionText: String);
begin
  MessageDlg('Error',
    Format('Project %s raised exception class %d with message ''%s''.',
           [Project1.Title, AExceptionID, AExceptionText]),
    mtError,[mbOk],0);
end;

procedure TDebugManager.OnDebuggerOutput(Sender: TObject; const AText: String);
begin
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
begin
  if (ADebugger<>FDebugger) or (ADebugger=nil) then
    RaiseException('TDebugManager.OnDebuggerChangeState');

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
  n: Integer;
begin
  if (Sender<>FDebugger) or (Sender=nil) then exit;

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
                                    [fsfSearchForProject,fsfUseIncludePaths]);
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
  SourceNotebook.GetActiveSE.ExecutionLine:=ALocation.SrcLine;
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
  if FDialogs[ADialogType] = nil
  then begin
    FDialogs[ADialogType] := DEBUGDIALOGCLASS[ADialogType].Create(Self);
    CurDialog:=FDialogs[ADialogType];
    CurDialog.Name:=NonModalIDEWindowNames[DebugDlgIDEWindow[ADialogType]];
    CurDialog.Tag := Integer(ADialogType);
    CurDialog.OnDestroy := @DebugDialogDestroy;
    DoInitDebugger;
    CurDialog.Debugger := FDebugger;
    EnvironmentOptions.IDEWindowLayoutList.Apply(CurDialog,CurDialog.Name);
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

constructor TDebugManager.Create(TheOwner: TComponent);
var
  DialogType: TDebugDialogType;
begin
  for DialogType := Low(TDebugDialogType) to High(TDebugDialogType) do
    FDialogs[DialogType] := nil; 

  FDebugger := nil;
  FBreakPoints := TDBGBreakPoints.Create(nil, TDBGBreakPoint);
  FBreakPointGroups := TDBGBreakPointGroups.Create;
  FWatches := TDBGWatches.Create(nil, TDBGWatch);
  
  FUserSourceFiles := TStringList.Create;
  
  inherited Create(TheOwner);
end;

destructor TDebugManager.Destroy;
var
  DialogType: TDebugDialogType;
begin                          
  for DialogType := Low(TDebugDialogType) to High(TDebugDialogType) do 
    DestroyDebugDialog(DialogType);
  
  if FDebugger <> nil
  then begin
    if FDebugger.BreakPoints = FBreakPoints
    then FBreakPoints := nil;
    if FDebugger.BreakPointGroups = FBreakPointGroups
    then FBreakPointGroups := nil;
    if FDebugger.Watches = FWatches
    then FWatches := nil;
  
    FreeThenNil(FDebugger);
  end
  else begin
    FreeThenNil(FBreakPoints);
    FreeThenNil(FBreakPointGroups);
    FreeThenNil(FWatches);
  end;
  
  FreeAndNil(FUserSourceFiles);

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
  SourceNotebook.OnCreateBreakPoint := @OnSrcNotebookCreateBreakPoint;
  SourceNotebook.OnDeleteBreakPoint := @OnSrcNotebookDeleteBreakPoint;
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
  EditorComponent: TSynEdit;
  Marks: TSynEditMarkList;
  i: Integer;
  CurBreakPoint: TDBGBreakPoint;
  SrcFilename: String;
  AMark: TSynEditMark;
  CurType: TSrcEditMarkerType;
begin
  if AnUnitInfo.EditorIndex<0 then exit;
  ASrcEdit:=SourceNotebook.Editors[AnUnitInfo.EditorIndex];
  EditorComponent:=ASrcEdit.EditorComponent;
  Marks:=EditorComponent.Marks;
  // delete old breakpoints
  for i:=Marks.Count-1 downto 0 do begin
    AMark:=Marks[i];
    if ASrcEdit.IsBreakPointMark(Marks[i]) then begin
      Marks.Delete(i);
      AMark.Free;
    end;
  end;
  // set breakpoints
  SrcFilename:=AnUnitInfo.Filename;
  for i:=0 to FBreakpoints.Count-1 do begin
    CurBreakPoint:=FBreakpoints[i];
    if CompareFileNames(CurBreakPoint.Source,SrcFilename)=0 then begin
      if CurBreakPoint.Enabled then begin
        if CurBreakPoint.Valid in [vsValid,vsUnknown] then
          CurType:=semActiveBreakPoint
        else
          CurType:=semInvalidBreakPoint;
      end else
        CurType:=semInactiveBreakPoint;
      ASrcEdit.SetBreakPoint(CurBreakPoint.Line,CurType);
    end;
  end;
end;

//-----------------------------------------------------------------------------
// Debugger routines
//-----------------------------------------------------------------------------

function TDebugManager.DoInitDebugger: TModalResult;
var
  OldBreakpoints: TDBGBreakpoints;
  OldBreakPointGroups: TDBGBreakPointGroups;
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
    OldBreakpoints := TDBGBreakpoints.Create(nil, TDBGBreakpoint);
    OldBreakpoints.Assign(FBreakPoints);
    
    // copy the groups and all group references
    OldBreakPointGroups := TDBGBreakPointGroups.Create;
    OldBreakPointGroups.Regroup(FBreakPointGroups,FBreakPoints,OldBreakPoints);

    // copy the watches
    OldWatches := TDBGWatches.Create(nil, TDBGWatch);
    OldWatches.Assign(FWatches);

    FBreakPointGroups := nil;
    FBreakPoints := nil;
    FWatches := nil;
  end;
  
  procedure RestoreDebuggerItems;
  begin
    // restore the break point list without the group references
    if (OldBreakpoints<>nil) then
      FBreakPoints.Assign(OldBreakpoints);

    // restore the groups and all group references
    if (OldBreakPointGroups<>nil) then begin
      if (OldBreakpoints<>nil) then
        FBreakPointGroups.Regroup(OldBreakPointGroups,OldBreakpoints,
                                  FBreakPoints)
      else
        FBreakPointGroups.Assign(OldBreakPointGroups);
    end;

    // restore the watches
    if OldWatches<>nil then
      FWatches.Assign(OldWatches);
  end;
  
  procedure FreeDebugger;
  begin
    SaveDebuggerItems;
    FDebugger.Free;
    FDebugger := nil;
    ResetDialogs;
  end;

var
  LaunchingCmdLine, LaunchingApplication, LaunchingParams: String;
begin
  WriteLN('[TDebugManager.DoInitDebugger] A');

  Result:=mrCancel;
  if Project1.MainUnitID < 0 then Exit;

  LaunchingCmdLine:=MainIDE.GetRunCommandLine;
  SplitCmdLine(LaunchingCmdLine,LaunchingApplication,LaunchingParams);

  OldBreakpoints := nil;
  OldBreakPointGroups := nil;
  OldWatches := nil;
  
  try

    case EnvironmentOptions.DebuggerType of
      dtGnuDebugger: begin
        // check if debugger already created with the right type
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
          FBreakPointGroups := FDebugger.BreakPointGroups;
          FBreakPoints := FDebugger.BreakPoints;
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
    OldBreakpoints.Free;
    OldBreakPointGroups.Free;
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
  or (FDebugger = nil)
  then Exit;
  FDebugger.Pause;
  Result := mrOk;
end;

function TDebugManager.DoStepIntoProject: TModalResult;
begin
  if (MainIDE.DoInitProjectRun <> mrOK)
  or (MainIDE.ToolStatus <> itDebugger)
  or (FDebugger = nil)
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
  or (FDebugger = nil)
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
  or (FDebugger=nil)
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
          +'Stop it first.',mtError,[mbCancel],0);
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
  if FDebugger <> nil then FDebugger.Run;
end;

procedure TDebugManager.EndDebugging;
begin
  if FDebugger <> nil then FDebugger.Done;
end;

function TDebugManager.Evaluate(const AExpression: String;
  var AResult: String): Boolean;
begin
  Result := (MainIDE.ToolStatus = itDebugger)
        and (dcEvaluate in DebugBoss.Commands)
        and (FDebugger <> nil)
        and FDebugger.Evaluate(AExpression, AResult);
end;

function TDebugManager.DoCreateBreakPoint(const AFilename: string; Line: integer
  ): TModalResult;
var
  NewBreak: TDBGBreakPoint;
begin
  Result:=DoBeginChangeDebugger;
  if Result<>mrOk then exit;
  NewBreak := FBreakPoints.Add(AFilename,Line);
  NewBreak.InitialEnabled := True;
  NewBreak.Enabled := True;
  Project1.Modified:=true;
  Result:=DoEndChangeDebugger;
end;

function TDebugManager.DoDeleteBreakPoint(const AFilename: string; Line: integer
  ): TModalResult;
var
  OldBreakPoint: TDBGBreakPoint;
begin
  Result:=DoBeginChangeDebugger;
  if Result<>mrOk then exit;
  OldBreakPoint:=FBreakPoints.Find(AFilename,Line);
  if OldBreakPoint=nil then exit;
  OldBreakPoint.Free;
  Project1.Modified:=true;
  Result:=DoEndChangeDebugger;
end;

function TDebugManager.DoCreateWatch(const VariableName: string): TModalResult;
var
  NewWatch: TDBGWatch;
begin
  Result:=DoBeginChangeDebugger;
  if Result<>mrOk then exit;
  NewWatch := FWatches.Add(VariableName);
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
  or (FDebugger = nil)
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

  FDebugger.RunTo(ExtractFilename(UnitFilename), ActiveSrcEdit.EditorComponent.CaretY);

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
