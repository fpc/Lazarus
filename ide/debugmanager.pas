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
  SynEdit,
  CompilerOptions, EditorOptions, EnvironmentOpts, KeyMapping, UnitEditor,
  Project, IDEProcs, Debugger, RunParamsOpts, ExtToolDialog, IDEOptionDefs,
  LazarusIDEStrConsts, ProjectDefs, BaseDebugManager, MainBar, DebuggerDlg,
  Watchesdlg, BreakPointsdlg, LocalsDlg, DBGOutputForm, GDBMIDebugger,
  CallStackDlg;


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
    procedure OnSrcNotebookAddWatchesAtCursor(Sender: TObject);
    procedure OnSrcNotebookCreateBreakPoint(Sender: TObject; Line: Integer);
    procedure OnSrcNotebookDeleteBreakPoint(Sender: TObject; Line: Integer);

    // Debugger events
    procedure OnDebuggerChangeState(ADebugger: TDebugger; OldState: TDBGState);
    procedure OnDebuggerCurrentLine(Sender: TObject;
                                    const ALocation: TDBGLocationRec);
    procedure OnDebuggerOutput(Sender: TObject; const AText: String);
    procedure OnDebuggerException(Sender: TObject; const AExceptionID: Integer;
                                  const AExceptionText: String);
  private
    FDebugger: TDebugger;
    // When no debugger is created the IDE stores all debugger settings in its
    // own variables. When the debugger object is created these items point
    // to the corresponding items in the FDebugger object.
    FBreakPoints: TDBGBreakPoints;
    FBreakPointGroups: TDBGBreakPointGroups;
    FWatches: TDBGWatches;
    FDialogs: array[TDebugDialogType] of TDebuggerDlg;

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
    
    procedure RunDebugger; override;
    procedure EndDebugging; override;
    function Evaluate(const AExpression: String;
                      var AResult: String): Boolean; override;

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

procedure TDebugManager.OnSrcNotebookAddWatchesAtCursor(Sender : TObject);
var
  SE: TSourceEditor;
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
  NewWatch.InitialEnabled := True;
  Project1.Modified:=true;
end;

procedure TDebugManager.OnSrcNotebookCreateBreakPoint(Sender: TObject;
  Line: Integer);
var
  NewBreak: TDBGBreakPoint;
begin
  if SourceNotebook.Notebook = nil then Exit;

  NewBreak := FBreakPoints.Add(TSourceNotebook(Sender).GetActiveSE.FileName,
                  Line);
  NewBreak.InitialEnabled := True;
  NewBreak.Enabled := True;
  Project1.Modified:=true;
end;

procedure TDebugManager.OnSrcNotebookDeleteBreakPoint(Sender: TObject;
  Line: Integer);
var
  OldBreakPoint: TDBGBreakPoint;
begin
  if SourceNotebook.Notebook = nil then Exit;

  OldBreakPoint:=FBreakPoints.Find(TSourceNotebook(Sender).GetActiveSE.FileName,
                                   Line);
  if OldBreakPoint=nil then exit;
  OldBreakPoint.Free;
  Project1.Modified:=true;
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
    // dsNone, dsIdle, dsStop, dsPause, dsRun, dsError
    itNone, itNone, itNone, itDebugger, itDebugger, itDebugger
  );
  STATENAME: array[TDBGState] of string = (
    'dsNone', 'dsIdle', 'dsStop', 'dsPause', 'dsRun', 'dsError'
  );
begin
  // Is the next line needed ???
  if (ADebugger<>FDebugger) or (ADebugger=nil) then exit;

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
    MessageDlg(lisExecutionPaused,
      Format(lisExecutionPausedAdress, [#13#13, ALocation.Adress, #13,
        ALocation.FuncName, #13, ALocation.SrcFile, #13#13#13, #13]),
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
        if MessageDlg(lisFileNotFound,
          Format(lisTheFileWasNotFoundDoYouWantToLocateItYourself, ['"',
            UnitFile, '"', #13, #13, #13])
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
        and (dcEvaluate in DebugBoss.Commands)
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
