{ $Id$}

{------------------------------------------------------------------------------}
{   All debugger related IDE routines                                          }
{------------------------------------------------------------------------------}

{$IFDEF IDE_HEAD}
    itmViewwatches: TMenuItem;
    itmViewBreakpoints: TMenuItem;
    itmViewLocals: TMenuItem;
    itmViewDebugOutput: TMenuItem;
     
    // Menu events
    procedure mnuViewWatchesClick(Sender: TObject);
    procedure mnuViewBreakPointsClick(Sender: TObject);
    procedure mnuViewDebugOutputClick(Sender: TObject);
    procedure mnuViewLocalsClick(Sender: TObject);

    // SrcNotebook events
    procedure OnSrcNotebookAddWatchesAtCursor(Sender: TObject);
    procedure OnSrcNotebookCreateBreakPoint(Sender: TObject; Line: Integer);
    procedure OnSrcNotebookDeleteBreakPoint(Sender: TObject; Line: Integer);

    // Debugger events
    procedure OnDebuggerChangeState(Sender: TObject);
    procedure OnDebuggerCurrentLine(Sender: TObject; const ALocation: TDBGLocationRec);
    procedure OnDebuggerOutput(Sender: TObject; const AText: String);
    procedure OnDebuggerException(Sender: TObject; const AExceptionID: Integer; const AExceptionText: String);
    

{$ELSE} {$IFDEF IDE_PRIVATE}
    FBreakPoints: TDBGBreakPoints; // Points to debugger breakpoints if available
                                   // Else to own objet
    FDebugOutputDlg: TDBGOutputForm;
    FBreakPointsDlg: TBreakPointsDlg;
    FLocalsDlg: TLocalsDlg;

    FDebugger: TDebugger;

    procedure DebugConstructor;
    procedure DebugCreateShortCuts;
    procedure DebugLoadMenus;
    
    procedure DebugDialogDestroy(Sender: TObject);
    procedure ViewDebugDialog(const ADialogClass: TDebuggerDlgClass; var ADialog: TDebuggerDlg); 
    
{$ELSE} {$IFDEF IDE_PUBLIC}
    function DoInitDebugger: TModalResult;
    function DoPauseProject: TModalResult;
    function DoStepIntoProject: TModalResult;
    function DoStepOverProject: TModalResult;
    function DoRunToCursor: TModalResult;
    function DoStopProject: TModalResult;

{$ELSE} {$IFDEF IDE_IMPLEMENTATION}
//=============================================================================
// I M P L E M E N T A T I O N
//=============================================================================
    
//-----------------------------------------------------------------------------
// IDE initialization
//-----------------------------------------------------------------------------

procedure TMainIDE.DebugConstructor;
begin        
  // TWatchesDlg
  Watches_Dlg := TWatchesDlg.Create(Self);


  FBreakPointsDlg := nil;
  FLocalsDlg := nil;
  FDebugger := nil;
  FBreakPoints := TDBGBreakPoints.Create(nil, TDBGBreakPoint);
end;
    
procedure TMainIDE.DebugCreateShortCuts;
begin          
  with EditorOpts.KeyMap do 
  begin
    itmViewWatches.ShortCut := CommandToShortCut(ecToggleWatches);
    itmViewBreakpoints.ShortCut := CommandToShortCut(ecToggleBreakPoints);
    itmViewDebugOutput.ShortCut := CommandToShortCut(ecToggleDebuggerOut);  
    itmViewLocals.ShortCut := CommandToShortCut(ecToggleLocals);
  end;
end;
    
procedure TMainIDE.DebugLoadMenus;
begin               
  itmViewWatches := TMenuItem.Create(Self);
  itmViewWatches.Name:='itmViewWatches';
  itmViewWatches.Caption := 'Watches';
  itmViewWatches.OnClick := @mnuViewWatchesClick;
  itmViewDebugWindows.Add(itmViewWatches);

  itmViewBreakPoints := TMenuItem.Create(Self);
  itmViewBreakPoints.Name:='itmViewBreakPoints';
  itmViewBreakPoints.Caption := 'BreakPoints';
  itmViewBreakPoints.OnClick := @mnuViewBreakPointsClick;
  itmViewDebugWindows.Add(itmViewBreakPoints);

  itmViewLocals := TMenuItem.Create(Self);
  itmViewLocals.Name:='itmViewLocals';
  itmViewLocals.Caption := 'Local Variables';
  itmViewLocals.OnClick := @mnuViewLocalsClick;
  itmViewDebugWindows.Add(itmViewLocals);

  itmViewDebugOutput := TMenuItem.Create(Self);
  itmViewDebugOutput.Name:='itmViewDebugOutput';
  itmViewDebugOutput.Caption := 'Debug output';
  itmViewDebugOutput.OnClick := @mnuViewDebugOutputClick;
  itmViewDebugWindows.Add(itmViewDebugOutput);
end;
    
//-----------------------------------------------------------------------------
// Menu events
//-----------------------------------------------------------------------------

procedure TMainIDE.mnuViewWatchesClick(Sender : TObject);
begin
  Watches_dlg.Show;
end;

procedure TMainIDE.mnuViewBreakPointsClick(Sender : TObject);
begin
  ViewDebugDialog(TBreakPointsDlg, FBreakPointsDlg);
end;

procedure TMainIDE.mnuViewDebugOutputClick(Sender : TObject);
begin             
  ViewDebugDialog(TDBGOutputForm, FDebugOutputDlg);
end;

procedure TMainIDE.mnuViewLocalsClick(Sender : TObject);
begin             
  ViewDebugDialog(TLocalsDlg, FLocalsDlg);
end;

//-----------------------------------------------------------------------------
// ScrNoteBook events
//-----------------------------------------------------------------------------

procedure TMainIDE.OnSrcNotebookAddWatchesAtCursor(Sender : TObject);
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

  NewWatch := TdbgWatch(FDebugger.Watches.Add);
  NewWatch.Expression := WatchVar;
  NewWatch.Enabled := True;
end;

procedure TMainIDE.OnSrcNotebookCreateBreakPoint(Sender: TObject; Line: Integer);
var
  NewBreak: TDBGBreakPoint;
begin
  if SourceNotebook.Notebook = nil then Exit;

  NewBreak := FBreakPoints.Add(ExtractFilename(TSourceNotebook(sender).GetActiveSe.FileName), Line);
  NewBreak.Enabled := True;
end;

procedure TMainIDE.OnSrcNotebookDeleteBreakPoint(Sender: TObject; Line: Integer);
begin
  if SourceNotebook.Notebook = nil then Exit;

  FBreakPoints.Find(ExtractFilename(TSourceNotebook(sender).GetActiveSe.FileName), Line).Free;
end;

//-----------------------------------------------------------------------------
// Debugger events
//-----------------------------------------------------------------------------

procedure TMainIDE.OnDebuggerException(Sender: TObject; const AExceptionID: Integer; const AExceptionText: String);
begin
  MessageDlg('Error', 
    Format('Project %s raised exception class %d with message ''%s''.', [Project1.Title, AExceptionID, AExceptionText]),
    mtError,[mbOk],0);
end;

procedure TMainIDE.OnDebuggerOutput(Sender: TObject; const AText: String);
begin
  if FDebugOutputDlg <> nil
  then FDebugOutputDlg.AddText(AText);
end;

procedure TMainIDE.OnDebuggerChangeState(Sender: TObject);
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
  
  WriteLN('[TMainIDE.OnDebuggerChangeState] state: ', STATENAME[FDebugger.State]);

  // All conmmands
  // -------------------
  // dcRun, dcPause, dcStop, dcStepOver, dcStepInto, dcRunTo, dcJumpto, dcBreak, dcWatch
  // -------------------
  
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
  
  if FDebugger.State = dsError
  then begin
    WriteLN('Ooops, the debugger entered the error state');
  end;
end;

procedure TMainIDE.OnDebuggerCurrentLine(Sender: TObject; 
  const ALocation: TDBGLocationRec);
// debugger paused program due to pause or error
// -> show the current execution line in editor
// if SrcLine = -1 then no source is available
var 
  ActiveSrcEdit: TSourceEditor;
  UnitFile: String;
begin
  if (Sender<>FDebugger) or (Sender=nil) then exit;

  //TODO: Show assembler window if no source can be found.
  if ALocation.SrcLine = -1 then Exit; 
  
  UnitFile := FindUnitFile(ALocation.SrcFile);
  if UnitFile = '' 
  then UnitFile := ALocation.SrcFile;
  if DoOpenEditorFile(UnitFile, [ofOnlyIfExists]) <> mrOk then exit;
  
  ActiveSrcEdit := SourceNoteBook.GetActiveSE;
  if ActiveSrcEdit=nil then exit;
  
  with ActiveSrcEdit.EditorComponent do 
  begin
    CaretXY:=Point(1, ALocation.SrcLine);
    BlockBegin:=CaretXY;
    BlockEnd:=CaretXY;
    TopLine:=ALocation.SrcLine-(LinesInWindow div 2);
  end;
  ActiveSrcEdit.ErrorLine:=ALocation.SrcLine;
end;

//-----------------------------------------------------------------------------
// Debugger dialog routines
//-----------------------------------------------------------------------------

// Common handler
// The tag of the destroyed form contains the form variable pointing to it
procedure TMainIDE.DebugDialogDestroy(Sender: TObject);
begin
  if TForm(Sender).Tag <> 0 
  then PPointer(TForm(Sender).Tag)^ := nil;
end;

procedure TMainIDE.ViewDebugDialog(const ADialogClass: TDebuggerDlgClass; var ADialog: TDebuggerDlg);
begin
  if ADialog = nil
  then begin  
    try
      ADialog := ADialogClass.Create(Self);
    except
      on E: Exception do begin
        WriteLN('[ERROR] IDE: Probably FPC bug #1888 caused an exception while creating class ''', ADialogClass.ClassName, '''');
        WriteLN('[ERROR] IDE: Exception message: ', E.Message);
        Exit;
      end;
    end;
    ADialog.Tag := Integer(@ADialog);
    ADialog.OnDestroy := @DebugDialogDestroy;
    DoInitDebugger;
    ADialog.Debugger := FDebugger;
  end;
  ADialog.Show;
  ADialog.BringToFront;
end;

//-----------------------------------------------------------------------------
// Debugger routines
//-----------------------------------------------------------------------------

function TMainIDE.DoInitDebugger: TModalResult;
  procedure ResetDialogs;
  begin          
    if FDebugOutputDlg <> nil
    then FDebugOutputDlg.Debugger := FDebugger;
    if FBreakPointsDlg <> nil
    then FBreakPointsDlg.Debugger := FDebugger;
    if FLocalsDlg <> nil
    then FLocalsDlg.Debugger := FDebugger;
  end;
var
  OldBreakpoints: TDBGBreakpoints;
begin
  WriteLN('[TMainIDE.DoInitDebugger] A');  
  
  Result:=mrCancel;
  if Project1.MainUnit < 0 then Exit;
  
  OldBreakpoints := nil;  

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
        FDebugger := TGDBMIDebugger.Create(EnvironmentOptions.DebuggerFilename);
        FBreakPoints := FDebugger.BreakPoints;
        ResetDialogs;
      end;
      if OldBreakpoints <> nil
      then FBreakPoints.Assign(OldBreakpoints);
    end;
  else
    OldBreakpoints := FBreakPoints;
    FBreakPoints := TDBGBreakpoints.Create(nil, TDBGBreakpoint);
    FBreakPoints.Assign(OldBreakpoints);
    
    FDebugger.Free;
    FDebugger := nil;
    ResetDialogs;
    Exit;
  end;
  FDebugger.OnState:=@OnDebuggerChangeState;
  FDebugger.OnCurrent:=@OnDebuggerCurrentLine;
  FDebugger.OnDbgOutput := @OnDebuggerOutput;
  FDebugger.OnException := @OnDebuggerException;
  if FDebugger.State = dsNone 
  then FDebugger.Init;
  
  //TODO: Show/hide debug menuitems based on FDebugger.SupportedCommands 
    
  // property BreakPointGroups: TDBGBreakPointGroups read FBreakPointGroups; // list of all breakpoints
  // property Watches: TDBGWatches read FWatches;   // list of all watches localvars etc
  
  Result := mrOk;
  WriteLN('[TMainIDE.DoInitDebugger] END');  
end;

// still part of main, should go here when dummydebugger is finished
//
//function TMainIDE.DoRunProject: TModalResult;

function TMainIDE.DoPauseProject: TModalResult;
begin
  Result := mrCancel;
  if (ToolStatus <> itDebugger) 
  or (FDebugger = nil) 
  then Exit;
  FDebugger.Pause;
  Result := mrOk;
end;

function TMainIDE.DoStepIntoProject: TModalResult;
begin
  if (DoInitProjectRun <> mrOK)
  or (ToolStatus <> itDebugger)
  or (FDebugger = nil) 
  then begin
    Result := mrAbort;
    Exit;
  end;

  FDebugger.StepInto;
  Result := mrOk;
end;

function TMainIDE.DoStepOverProject: TModalResult;
begin
  if (DoInitProjectRun <> mrOK)
  or (ToolStatus <> itDebugger)
  or (FDebugger = nil) 
  then begin
    Result := mrAbort;
    Exit;
  end;

  FDebugger.StepOver;
  Result := mrOk;
end;

function TMainIDE.DoStopProject: TModalResult;
begin
  Result := mrCancel;
  if (ToolStatus <> itDebugger) 
  or (FDebugger=nil) 
  then Exit;

  FDebugger.Stop;
  Result := mrOk;
end;

function TMainIDE.DoRunToCursor: TModalResult;
var 
  ActiveSrcEdit: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
  UnitFilename: string;
begin
  if (DoInitProjectRun <> mrOK)
  or (ToolStatus <> itDebugger)
  or (FDebugger = nil) 
  then begin
    Result := mrAbort;
    Exit;
  end;

  Result := mrCancel;

  GetCurrentUnit(ActiveSrcEdit, ActiveUnitInfo);
  if (ActiveSrcEdit=nil) or (ActiveUnitInfo=nil) 
  then begin
    MessageDlg('Run to failed','Please open a unit before run.',mtError,
      [mbCancel],0);
    Exit;
  end;

  if not ActiveUnitInfo.Source.IsVirtual 
  then UnitFilename:=ActiveUnitInfo.Filename
  else UnitFilename:=GetTestUnitFilename(ActiveUnitInfo);

  FDebugger.RunTo(ExtractFilename(UnitFilename), ActiveSrcEdit.EditorComponent.CaretY);

  Result := mrOK;
end;




//=============================================================================
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
