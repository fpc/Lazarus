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
  Classes, SysUtils, Forms, Controls, Dialogs, CompilerOptions, EditorOptions,
  EnvironmentOpts, KeyMapping, UnitEditor, Project, IDEProcs,
  Debugger, RunParamsOpts, ExtToolDialog, LazarusIDEStrConsts,
  ProjectDefs, BaseDebugManager, MainBar, DebuggerDlg;
  
type
  TDebugDialogType = (ddtOutput, ddtBreakpoints, ddtWatches, ddtLocals, ddtCallStack);

  TDebugManager = class(TBaseDebugManager)
    // Menu events
    procedure mnuViewDebugDialogClick(Sender: TObject);

    // SrcNotebook events
    procedure OnSrcNotebookAddWatchesAtCursor(Sender: TObject);
    procedure OnSrcNotebookCreateBreakPoint(Sender: TObject; Line: Integer);
    procedure OnSrcNotebookDeleteBreakPoint(Sender: TObject; Line: Integer);

    // Debugger events
    procedure OnDebuggerChangeState(Sender: TObject);
    procedure OnDebuggerCurrentLine(Sender: TObject; const ALocation: TDBGLocationRec);
    procedure OnDebuggerOutput(Sender: TObject; const AText: String);
    procedure OnDebuggerException(Sender: TObject; const AExceptionID: Integer; const AExceptionText: String);
  private
    FBreakPoints: TDBGBreakPoints; // Points to debugger breakpoints if available
                                   // Else to own objet
    FWatches: TDBGWatches;         // Points to debugger watchess if available
                                   // Else to own objet
    FDialogs: array[TDebugDialogType] of TDebuggerDlg;

    FDebugger: TDebugger;

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

    function DoInitDebugger: TModalResult; override;
    function DoPauseProject: TModalResult; override;
    function DoStepIntoProject: TModalResult; override;
    function DoStepOverProject: TModalResult; override;
    function DoRunToCursor: TModalResult; override;
    function DoStopProject: TModalResult; override;
    
    procedure RunDebugger; override;
    procedure EndDebugging; override;
    function Evaluate(const AExpression: String; var AResult: String): Boolean; override;

  end;
  
  

implementation

uses
  Menus,
  Watchesdlg, BreakPointsdlg, LocalsDlg, DBGOutputForm, GDBMIDebugger, 
  CallStackDlg;

 
//-----------------------------------------------------------------------------
// Menu events
//-----------------------------------------------------------------------------

procedure TDebugManager.mnuViewDebugDialogClick(Sender: TObject);
begin                       
  ViewDebugDialog(TDebugDialogType(TMenuItem(Sender).Tag));
end;

//-----------------------------------------------------------------------------
// ScrNoteBook events
//-----------------------------------------------------------------------------

procedure TDebugManager.OnSrcNotebookAddWatchesAtCursor(Sender : TObject);
var
  SE : TSourceEditor;
  WatchVar: String;
  NewWatch: TdbgWatch;
begin
  if FDebugger = nil then Exit;

  //get the sourceEditor.
  SE := TSourceNotebook(sender).GetActiveSE;
  if not Assigned(SE) then Exit;
  WatchVar := SE.GetWordAtCurrentCaret;
  if WatchVar = ''  then Exit;

  NewWatch := FWatches.Add(WatchVar);
  NewWatch.Enabled := True;
end;

procedure TDebugManager.OnSrcNotebookCreateBreakPoint(Sender: TObject; Line: Integer);
var
  NewBreak: TDBGBreakPoint;
begin
  if SourceNotebook.Notebook = nil then Exit;

  NewBreak := FBreakPoints.Add(ExtractFilename(TSourceNotebook(sender).GetActiveSe.FileName), Line);
  NewBreak.Enabled := True;
end;

procedure TDebugManager.OnSrcNotebookDeleteBreakPoint(Sender: TObject; Line: Integer);
begin
  if SourceNotebook.Notebook = nil then Exit;

  FBreakPoints.Find(ExtractFilename(TSourceNotebook(sender).GetActiveSe.FileName), Line).Free;
end;

//-----------------------------------------------------------------------------
// Debugger events
//-----------------------------------------------------------------------------

procedure TDebugManager.OnDebuggerException(Sender: TObject; const AExceptionID: Integer; const AExceptionText: String);
begin
  MessageDlg('Error',
    Format('Project %s raised exception class %d with message ''%s''.', [Project1.Title, AExceptionID, AExceptionText]),
    mtError,[mbOk],0);
end;

procedure TDebugManager.OnDebuggerOutput(Sender: TObject; const AText: String);
begin
  if FDialogs[ddtOutput] <> nil
  then TDbgOutputForm(FDialogs[ddtOutput]).AddText(AText);
end;

procedure TDebugManager.OnDebuggerChangeState(Sender: TObject);
const
  // dsNone, dsIdle, dsStop, dsPause, dsRun, dsError
  TOOLSTATEMAP: array[TDBGState] of TIDEToolStatus = (
    // dsNone, dsIdle, dsStop, dsPause, dsRun, dsError
    itNone, itNone, itNone, itDebugger, itDebugger, itDebugger
  );
  STATENAME: array[TDBGState] of string = (
    'dsNone', 'dsIdle', 'dsStop', 'dsPause', 'dsRun', 'dsError'
  );
begin
  // Is the next line needed ???
  if (Sender<>FDebugger) or (Sender=nil) then exit;

  WriteLN('[TDebugManager.OnDebuggerChangeState] state: ', STATENAME[FDebugger.State]);

  // All conmmands
  // -------------------
  // dcRun, dcPause, dcStop, dcStepOver, dcStepInto, dcRunTo, dcJumpto, dcBreak, dcWatch
  // -------------------

  with MainIDE do begin
    // For run end step bypass idle, so we can set the filename later
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

  case FDebugger.State of 
    dsError: begin
      WriteLN('Ooops, the debugger entered the error state');
      MessageDlg('Debugger error',
        'Debugger error'#13#13 + 
        'Ooops, the debugger entered the error state'#13 + 
        'Save your work now !'#13#13 + 
        'Hit Stop, and hope the best, we''re pulling the plug.', 
        mtError, [mbOK],0);
    end;
    dsStop: begin
      MessageDlg('Execution stopped',
        'Execution stopped'#13#13, 
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
  ActiveSrcEdit: TSourceEditor;
  SearchFile, UnitFile: String;
  OpenDialog: TOpenDialog;
  UnitInfo: TUnitInfo;
  n: Integer;
begin
  if (Sender<>FDebugger) or (Sender=nil) then exit;

  //TODO: Show assembler window if no source can be found.
  if ALocation.SrcLine = -1 
  then begin
    MessageDlg('Execution paused',
      Format('Execution paused'#13#13 + 
             '  Adress: $%p'#13 + 
             '  Procedure: %s'#13 + 
             '  File: %s'#13#13#13 + 
             '(Some day an assembler window might popup here :)'#13,  
        [ALocation.Adress, ALocation.FuncName, ALocation.SrcFile]),
      mtInformation, [mbOK],0);
    
    Exit;
  end;

  UnitFile := MainIDE.FindUnitFile(ALocation.SrcFile);
  if UnitFile = ''
  then UnitFile := ALocation.SrcFile;
  
  if MainIDE.DoOpenEditorFile(UnitFile,-1,[ofOnlyIfExists, ofQuiet]) <> mrOk 
  then begin
    // Try to find it ourself in the project files
    SearchFile := ExtractFilenameOnly(ALocation.SrcFile); 
    UnitFile := '';
    for n := Project1.UnitCount - 1 downto 0 do
    begin
      UnitInfo := Project1.Units[n];
      if CompareFileNames(SearchFile, ExtractFilenameOnly(UnitInfo.FileName)) = 0
      then begin
        UnitFile := UnitInfo.FileName;
        Break;
      end;
    end;
    
    if (UnitFile = '')
    or (MainIDE.DoOpenEditorFile(UnitFile,-1,[ofOnlyIfExists, ofQuiet]) <> mrOk)
    then begin
      UnitFile := ALocation.SrcFile;
      repeat
        if MessageDlg('File not found',
          'The file "'+UnitFile+'"'#13
          +'was not found.'#13
          +'Do you want to locate it yourself ?'#13
          ,mtConfirmation, [mbYes, mbNo], 0) <> mrYes   
        then Exit;
        
        OpenDialog := TOpenDialog.Create(Application);
        try
          OpenDialog.Title := lisOpenFile;
          OpenDialog.FileName := ALocation.SrcFile;
          if not OpenDialog.Execute 
          then Exit;
          UnitFile := OpenDialog.FileName;
        finally
          OpenDialog.Free;
        end;
      until MainIDE.DoOpenEditorFile(UnitFile,-1,[ofOnlyIfExists, ofQuiet]) = mrOk;
    end;
  end;  

  ActiveSrcEdit := SourceNoteBook.GetActiveSE;
  if ActiveSrcEdit=nil then exit;

  with ActiveSrcEdit.EditorComponent do
  begin
    CaretXY:=Point(1, ALocation.SrcLine);
    BlockBegin:=CaretXY;
    BlockEnd:=CaretXY;
    TopLine:=ALocation.SrcLine-(LinesInWindow div 2);
  end;
  SourceNotebook.ClearExecutionLines;
  SourceNotebook.ClearErrorLines;
  ActiveSrcEdit.ExecutionLine:=ALocation.SrcLine;
  // ActiveSrcEdit.ErrorLine:=ALocation.SrcLine;
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
begin
  if FDialogs[ADialogType] = nil
  then begin
    try
      FDialogs[ADialogType] := DEBUGDIALOGCLASS[ADialogType].Create(Self);
    except
      on E: Exception do begin
        WriteLN('[ERROR] IDE: Probably FPC bug #1888 caused an exception while creating class ''', DEBUGDIALOGCLASS[ADialogType].ClassName, '''');
        WriteLN('[ERROR] IDE: Exception message: ', E.Message);
        Exit;
      end;
    end;
    FDialogs[ADialogType].Tag := Integer(ADialogType);
    FDialogs[ADialogType].OnDestroy := @DebugDialogDestroy;
    DoInitDebugger;
    FDialogs[ADialogType].Debugger := FDebugger;
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
  FWatches := TDBGWatches.Create(nil, TDBGWatch);
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
    if FDebugger.Watches = FWatches
    then FWatches := nil;
  
    FreeThenNil(FDebugger);
  end
  else begin
    FreeThenNil(FBreakPoints);
    FreeThenNil(FWatches);
  end;

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

//-----------------------------------------------------------------------------
// Debugger routines
//-----------------------------------------------------------------------------

function TDebugManager.DoInitDebugger: TModalResult;
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
var
  OldBreakpoints: TDBGBreakpoints;
  OldWatches: TDBGWatches;
  LaunchingCmdLine, LaunchingApplication, LaunchingParams: String;
begin
  WriteLN('[TDebugManager.DoInitDebugger] A');

  Result:=mrCancel;
  if Project1.MainUnit < 0 then Exit;

  LaunchingCmdLine:=MainIDE.GetRunCommandLine;
  SplitCmdLine(LaunchingCmdLine,LaunchingApplication,LaunchingParams);

  OldBreakpoints := nil;
  OldWatches := nil;

  case EnvironmentOptions.DebuggerType of
    dtGnuDebugger: begin
      if (FDebugger <> nil)
      and ( not(FDebugger is TGDBMIDebugger)
            or (FDebugger.ExternalDebugger <> EnvironmentOptions.DebuggerFilename)
          )
      then begin
        OldBreakpoints := TDBGBreakpoints.Create(nil, TDBGBreakpoint);
        OldBreakpoints.Assign(FBreakPoints);
        FBreakPoints := nil;

        OldWatches := TDBGWatches.Create(nil, TDBGWatch);
        OldWatches.Assign(FWatches);
        FWatches := nil;

        FDebugger.Free;
        FDebugger := nil;
        ResetDialogs;
      end;
      if FDebugger = nil
      then begin
        if FBreakPoints <> nil
        then begin
          OldBreakpoints := TDBGBreakpoints.Create(nil, TDBGBreakpoint);
          OldBreakpoints.Assign(FBreakPoints);
        end;
        if FWatches <> nil
        then begin
          OldWatches := TDBGWatches.Create(nil, TDBGWatch);
          OldWatches.Assign(FWatches);
        end;
        FDebugger := TGDBMIDebugger.Create(EnvironmentOptions.DebuggerFilename);
        FBreakPoints := FDebugger.BreakPoints;
        FWatches := FDebugger.Watches;
        ResetDialogs;
      end;
      if OldBreakpoints <> nil
      then FBreakPoints.Assign(OldBreakpoints);
      if OldWatches <> nil
      then FWatches.Assign(OldWatches);
    end;
  else
    OldBreakpoints := FBreakPoints;
    FBreakPoints := TDBGBreakpoints.Create(nil, TDBGBreakpoint);
    FBreakPoints.Assign(OldBreakpoints);

    OldWatches := FWatches;
    FWatches := TDBGWatches.Create(nil, TDBGWatch);
    FWatches.Assign(OldWatches);

    FDebugger.Free;
    FDebugger := nil;
    ResetDialogs;
    Exit;
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

  // property BreakPointGroups: TDBGBreakPointGroups read FBreakPointGroups; // list of all breakpoints
  // property Watches: TDBGWatches read FWatches;   // list of all watches localvars etc

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

procedure TDebugManager.RunDebugger;
begin
  if FDebugger <> nil then FDebugger.Run;
end;

procedure TDebugManager.EndDebugging;
begin
  if FDebugger <> nil then FDebugger.Done;
end;

function TDebugManager.Evaluate(const AExpression: String; var AResult: String): Boolean;
begin
  Result := (MainIDE.ToolStatus = itDebugger)
        and (FDebugger <> nil)
        and FDebugger.Evaluate(AExpression, AResult);
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
    MessageDlg('Run to failed','Please open a unit before run.',mtError,
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
