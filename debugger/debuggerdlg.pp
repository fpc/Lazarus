{ $Id$ }
{                    ----------------------------------------
                       DebuggerDlg.pp  -  Base class for all
                         debugger related forms
                     ----------------------------------------

 @created(Wed Mar 16st WET 2001)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@dommelstein.net>)

 This unit contains the base class for all debugger related dialogs.
 All common info needed for the IDE is found in this class

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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}
unit DebuggerDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  // LCL
  Forms, Controls, LCLProc,
  // LazUtils
  LazFileUtils, LazLoggerBase,
  // IdeIntf
  IDEImagesIntf, IDECommands,
  // DebuggerIntf
  DbgIntfDebuggerBase, DbgIntfMiscClasses,
  // IDE
  MainIntf, EditorOptions, BaseDebugManager, Debugger;

type

  { TDebuggerDlg }

  TDebuggerDlg = class(TForm)
  private
    FUpdateCount: integer;
  protected
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoClose(var CloseAction: TCloseAction); override;
    procedure DoBeginUpdate; virtual;
    procedure DoEndUpdate; virtual;
  public
    procedure BeginUpdate;
    procedure EndUpdate;
    function UpdateCount: integer;
    function IsUpdating: Boolean;
  private (* provide some common properties *)
    FSnapshotManager: TSnapshotManager;
    FSnapshotNotification: TSnapshotNotification;
    FThreadsMonitor: TIdeThreadsMonitor;
    FThreadsNotification: TThreadsNotification;
    FCallStackMonitor: TIdeCallStackMonitor;
    FCallStackNotification: TCallStackNotification;
    FLocalsMonitor: TIdeLocalsMonitor;
    FLocalsNotification: TLocalsNotification;
    FWatchesMonitor: TIdeWatchesMonitor;
    FWatchesNotification: TWatchesNotification;
    FRegistersMonitor: TIdeRegistersMonitor;
    FRegistersNotification: TRegistersNotification;
    FBreakPoints: TIDEBreakPoints;
    FBreakpointsNotification: TIDEBreakPointsNotification;
    function  GetSnapshotNotification: TSnapshotNotification;
    function  GetThreadsNotification: TThreadsNotification;
    function  GetCallStackNotification: TCallStackNotification;
    function  GetLocalsNotification: TLocalsNotification;
    function  GetWatchesNotification: TWatchesNotification;
    function  GetRegistersNotification: TRegistersNotification;
    function  GetBreakpointsNotification: TIDEBreakPointsNotification;
    procedure SetSnapshotManager(const AValue: TSnapshotManager);
    procedure SetThreadsMonitor(const AValue: TIdeThreadsMonitor);
    procedure SetCallStackMonitor(const AValue: TIdeCallStackMonitor);
    procedure SetLocalsMonitor(const AValue: TIdeLocalsMonitor);
    procedure SetWatchesMonitor(const AValue: TIdeWatchesMonitor);
    procedure SetRegistersMonitor(AValue: TIdeRegistersMonitor);
    procedure SetBreakPoints(const AValue: TIDEBreakPoints);
  protected
    procedure JumpToUnitSource(AnUnitInfo: TDebuggerUnitInfo; ALine: Integer);
    procedure DoWatchesChanged; virtual; // called if the WatchesMonitor object was changed
    procedure DoRegistersChanged; virtual; // called if the WatchesMonitor object was changed
    procedure DoBreakPointsChanged; virtual; // called if the BreakPoint(Monitor) object was changed
    function GetBreakPointImageIndex(ABreakPoint: TIDEBreakPoint; AIsCurLine: Boolean = False): Integer;
    property SnapshotNotification:    TSnapshotNotification  read GetSnapshotNotification;
    property ThreadsNotification:     TThreadsNotification   read GetThreadsNotification;
    property CallStackNotification:   TCallStackNotification read GetCallStackNotification;
    property LocalsNotification:      TLocalsNotification    read GetLocalsNotification;
    property WatchesNotification:     TWatchesNotification   read GetWatchesNotification;
    property RegistersNotification:   TRegistersNotification read GetRegistersNotification;
    property BreakpointsNotification: TIDEBreakPointsNotification read GetBreakpointsNotification;
  protected
    // publish as needed
    property SnapshotManager:  TSnapshotManager  read FSnapshotManager  write SetSnapshotManager;
    property ThreadsMonitor:   TIdeThreadsMonitor   read FThreadsMonitor   write SetThreadsMonitor;
    property CallStackMonitor: TIdeCallStackMonitor read FCallStackMonitor write SetCallStackMonitor;
    property LocalsMonitor:    TIdeLocalsMonitor    read FLocalsMonitor    write SetLocalsMonitor;
    property WatchesMonitor:   TIdeWatchesMonitor   read FWatchesMonitor   write SetWatchesMonitor;
    property RegistersMonitor: TIdeRegistersMonitor read FRegistersMonitor write SetRegistersMonitor;
    property BreakPoints:      TIDEBreakPoints   read FBreakPoints      write SetBreakPoints;
  public
    destructor  Destroy; override;
  end;
  TDebuggerDlgClass = class of TDebuggerDlg;

var
  OnProcessCommand: procedure(Sender: TObject; Command: word; var Handled: boolean) of object;

procedure CreateDebugDialog(Sender: TObject; aFormName: string;
                            var AForm: TCustomForm; DoDisableAutoSizing: boolean);

implementation

var
  DBG_LOCATION_INFO: PLazLoggerLogGroup;
  BrkImgIdxInitialized: Boolean;
  ImgBreakPoints: Array [0..10] of Integer;

procedure CreateDebugDialog(Sender: TObject; aFormName: string; var AForm: TCustomForm;
  DoDisableAutoSizing: boolean);
begin
  DebugBoss.CreateDebugDialog(Sender, aFormName, AForm, DoDisableAutoSizing);
end;

{ TDebuggerDlg }

procedure TDebuggerDlg.BeginUpdate;
begin
  Inc(FUpdateCount);
  if FUpdateCount = 1 then DoBeginUpdate;
end;

procedure TDebuggerDlg.EndUpdate;
begin
  if FUpdateCount < 1 then RaiseGDBException('TDebuggerDlg.EndUpdate');
  Dec(FUpdateCount);
  if FUpdateCount = 0 then DoEndUpdate;
end;

function TDebuggerDlg.UpdateCount: integer;
begin
  Result := FUpdateCount;
end;

function TDebuggerDlg.IsUpdating: Boolean;
begin
  Result := FUpdateCount > 0;
end;

function TDebuggerDlg.GetSnapshotNotification: TSnapshotNotification;
begin
  If FSnapshotNotification = nil then begin
    FSnapshotNotification := TSnapshotNotification.Create;
    FSnapshotNotification.AddReference;
    if (FSnapshotManager <> nil)
    then FSnapshotManager.AddNotification(FSnapshotNotification);
  end;
  Result := FSnapshotNotification;
end;

function TDebuggerDlg.GetRegistersNotification: TRegistersNotification;
begin
  If FRegistersNotification = nil then begin
    FRegistersNotification := TRegistersNotification.Create;
    FRegistersNotification.AddReference;
    if (FRegistersMonitor <> nil)
    then FRegistersMonitor.AddNotification(FRegistersNotification);
  end;
  Result := FRegistersNotification;
end;

function TDebuggerDlg.GetThreadsNotification: TThreadsNotification;
begin
  if FThreadsNotification = nil then begin
    FThreadsNotification := TThreadsNotification.Create;
    FThreadsNotification.AddReference;
    if (FThreadsMonitor <> nil)
    then FThreadsMonitor.AddNotification(FThreadsNotification);
  end;
  Result := FThreadsNotification;
end;

function TDebuggerDlg.GetCallStackNotification: TCallStackNotification;
begin
  if FCallStackNotification = nil then begin
    FCallStackNotification := TCallStackNotification.Create;
    FCallStackNotification.AddReference;
    if (FCallStackMonitor <> nil)
    then FCallStackMonitor.AddNotification(FCallStackNotification);
  end;
  Result := FCallStackNotification;
end;

function TDebuggerDlg.GetLocalsNotification: TLocalsNotification;
begin
  If FLocalsNotification = nil then begin
    FLocalsNotification := TLocalsNotification.Create;
    FLocalsNotification.AddReference;
    if (FLocalsMonitor <> nil)
    then FLocalsMonitor.AddNotification(FLocalsNotification);
  end;
  Result := FLocalsNotification;
end;

function TDebuggerDlg.GetWatchesNotification: TWatchesNotification;
begin
  If FWatchesNotification = nil then begin
    FWatchesNotification := TWatchesNotification.Create;
    FWatchesNotification.AddReference;
    if (FWatchesMonitor <> nil)
    then FWatchesMonitor.AddNotification(FWatchesNotification);
  end;
  Result := FWatchesNotification;
end;

function TDebuggerDlg.GetBreakpointsNotification: TIDEBreakPointsNotification;
begin
  If FBreakpointsNotification = nil then begin
    FBreakpointsNotification := TIDEBreakPointsNotification.Create;
    FBreakpointsNotification.AddReference;
    if (FBreakPoints <> nil)
    then FBreakPoints.AddNotification(FBreakpointsNotification);
  end;
  Result := FBreakpointsNotification;
end;

procedure TDebuggerDlg.SetRegistersMonitor(AValue: TIdeRegistersMonitor);
begin
  if FRegistersMonitor = AValue then exit;
  BeginUpdate;
  try
    if (FRegistersMonitor <> nil) and (FRegistersNotification <> nil)
    then FRegistersMonitor.RemoveNotification(FRegistersNotification);
    FRegistersMonitor := AValue;
    if (FRegistersMonitor <> nil) and (FRegistersNotification <> nil)
    then FRegistersMonitor.AddNotification(FRegistersNotification);
    DoRegistersChanged;
  finally
    EndUpdate;
  end;
end;

procedure TDebuggerDlg.SetSnapshotManager(const AValue: TSnapshotManager);
begin
  if FSnapshotManager = AValue then exit;
  BeginUpdate;
  try
    if (FSnapshotManager <> nil) and (FSnapshotNotification <> nil)
    then FSnapshotManager.RemoveNotification(FSnapshotNotification);
    FSnapshotManager := AValue;
    if (FSnapshotManager <> nil) and (FSnapshotNotification <> nil)
    then FSnapshotManager.AddNotification(FSnapshotNotification);
    if assigned(FSnapshotNotification.OnChange) then FSnapshotNotification.OnChange(nil);
    if assigned(FSnapshotNotification.OnCurrent) then FSnapshotNotification.OnCurrent(nil);
  finally
    EndUpdate;
  end;
end;

procedure TDebuggerDlg.SetThreadsMonitor(const AValue: TIdeThreadsMonitor);
begin
  if FThreadsMonitor = AValue then exit;
  BeginUpdate;
  try
    if (FThreadsMonitor <> nil) and (FThreadsNotification <> nil)
    then FThreadsMonitor.RemoveNotification(FThreadsNotification);
    FThreadsMonitor := AValue;
    if (FThreadsMonitor <> nil) and (FThreadsNotification <> nil)
    then FThreadsMonitor.AddNotification(FThreadsNotification);
    if assigned(FThreadsNotification.OnChange) then FThreadsNotification.OnChange(nil);
    if assigned(FThreadsNotification.OnCurrent) then FThreadsNotification.OnCurrent(nil);
  finally
    EndUpdate;
  end;
end;

procedure TDebuggerDlg.SetCallStackMonitor(const AValue: TIdeCallStackMonitor);
begin
  if FCallStackMonitor = AValue then exit;
  BeginUpdate;
  try
    if (FCallStackMonitor <> nil) and (FCallStackNotification <> nil)
    then FCallStackMonitor.RemoveNotification(FCallStackNotification);
    FCallStackMonitor := AValue;
    if (FCallStackMonitor <> nil) and (FCallStackNotification <> nil)
    then FCallStackMonitor.AddNotification(FCallStackNotification);
    if assigned(FCallStackNotification.OnChange) then FCallStackNotification.OnChange(nil);
    if assigned(FCallStackNotification.OnCurrent) then FCallStackNotification.OnCurrent(nil);
  finally
    EndUpdate;
  end;
end;

procedure TDebuggerDlg.SetLocalsMonitor(const AValue: TIdeLocalsMonitor);
begin
  if FLocalsMonitor = AValue then exit;
  BeginUpdate;
  try
    if (FLocalsMonitor <> nil) and (FLocalsNotification <> nil)
    then FLocalsMonitor.RemoveNotification(FLocalsNotification);
    FLocalsMonitor := AValue;
    if (FLocalsMonitor <> nil) and (FLocalsNotification <> nil)
    then FLocalsMonitor.AddNotification(FLocalsNotification);
    if assigned(FLocalsNotification.OnChange) then FLocalsNotification.OnChange(nil);
  finally
    EndUpdate;
  end;
end;

procedure TDebuggerDlg.SetWatchesMonitor(const AValue: TIdeWatchesMonitor);
begin
  if FWatchesMonitor = AValue then exit;
  BeginUpdate;
  try
    if (FWatchesMonitor <> nil) and (FWatchesNotification <> nil)
    then FWatchesMonitor.RemoveNotification(FWatchesNotification);
    FWatchesMonitor := AValue;
    if (FWatchesMonitor <> nil) and (FWatchesNotification <> nil)
    then FWatchesMonitor.AddNotification(FWatchesNotification);
    DoWatchesChanged;
  finally
    EndUpdate;
  end;
end;

procedure TDebuggerDlg.SetBreakPoints(const AValue: TIDEBreakPoints);
begin
  if FBreakPoints = AValue then exit;
  BeginUpdate;
  try
    if (FBreakPoints <> nil) and (FBreakpointsNotification <> nil)
    then FBreakPoints.RemoveNotification(FBreakpointsNotification);
    FBreakPoints := AValue;
    if (FBreakPoints <> nil) and (FBreakpointsNotification <> nil)
    then FBreakPoints.AddNotification(FBreakpointsNotification);
    DoBreakPointsChanged;
  finally
    EndUpdate;
  end;
end;

procedure TDebuggerDlg.JumpToUnitSource(AnUnitInfo: TDebuggerUnitInfo; ALine: Integer);
const
  JmpFlags: TJumpToCodePosFlags =
    [jfAddJumpPoint, jfFocusEditor, jfMarkLine, jfMapLineFromDebug, jfSearchVirtualFullPath];
var
  Filename: String;
  ok: Boolean;
begin
  if AnUnitInfo = nil then exit;
  debugln(DBG_LOCATION_INFO, ['JumpToUnitSource AnUnitInfo=', AnUnitInfo.DebugText ]);
  // avoid any process-messages, so this proc can not be re-entered (avoid opening one files many times)
  DebugBoss.LockCommandProcessing;
  try
  (* Maybe trim the filename here and use jfDoNotExpandFilename
     ExpandFilename works with the current IDE path, and may be wrong
  *)
  // TODO: better detection of unsaved project files
    if DebugBoss.GetFullFilename(AnUnitInfo, Filename, False) then
    begin
      ok := false;
      if ALine <= 0 then
        ALine := AnUnitInfo.SrcLine;
      if FilenameIsAbsolute(Filename) then
        ok := MainIDEInterface.DoJumpToSourcePosition(Filename, 0, ALine, 0, JmpFlags) = mrOK;
      if not ok then
        MainIDEInterface.DoJumpToSourcePosition(Filename, 0, ALine, 0, JmpFlags+[jfDoNotExpandFilename]);
    end;
  finally
    DebugBoss.UnLockCommandProcessing;
  end;
end;

procedure TDebuggerDlg.DoWatchesChanged;
begin
  //
end;

procedure TDebuggerDlg.DoRegistersChanged;
begin
  //
end;

procedure TDebuggerDlg.DoBreakPointsChanged;
begin
  //
end;

function TDebuggerDlg.GetBreakPointImageIndex(ABreakPoint: TIDEBreakPoint;
  AIsCurLine: Boolean = False): Integer;
var
  i: Integer;
begin
  Result := -1;

  if not BrkImgIdxInitialized then begin
    ImgBreakPoints[0] := IDEImages.LoadImage('ActiveBreakPoint');  // red dot
    ImgBreakPoints[1] := IDEImages.LoadImage('InvalidBreakPoint'); // red dot "X"
    ImgBreakPoints[2] := IDEImages.LoadImage('UnknownBreakPoint'); // red dot "?"
    ImgBreakPoints[3] := IDEImages.LoadImage('PendingBreakPoint'); // red dot "||"


    ImgBreakPoints[4] := IDEImages.LoadImage('InactiveBreakPoint');// green dot
    ImgBreakPoints[5] := IDEImages.LoadImage('InvalidDisabledBreakPoint');// green dot "X"
    ImgBreakPoints[6] := IDEImages.LoadImage('UnknownDisabledBreakPoint');// green dot "?"
    ImgBreakPoints[7] := IDEImages.LoadImage('InactiveBreakPoint');// green dot

    ImgBreakPoints[8] := IDEImages.LoadImage('debugger_current_line');
    ImgBreakPoints[9] := IDEImages.LoadImage('debugger_current_line_breakpoint');
    ImgBreakPoints[10] := IDEImages.LoadImage('debugger_current_line_disabled_breakpoint');

    BrkImgIdxInitialized := True;
  end;

  if AIsCurLine
  then begin
    if ABreakPoint = nil
    then Result := ImgBreakPoints[8]
    else if ABreakPoint.Enabled
    then Result := ImgBreakPoints[9]
    else Result := ImgBreakPoints[10];
  end
  else
  if (ABreakPoint <> nil)
  then begin
    if ABreakPoint.Enabled
    then i := 0
    else i := 4;
    case ABreakPoint.Valid of
      vsValid:   i := i + 0;
      vsInvalid: i := i + 1;
      vsUnknown: i := i + 2;
      vsPending: i := i + 3; // TODO
    end;
    Result := ImgBreakPoints[i];
  end;
end;

destructor TDebuggerDlg.Destroy;
begin
  if FSnapshotNotification <> nil then begin;
    FSnapshotNotification.OnChange := nil;
    FSnapshotNotification.OnCurrent := nil;
  end;
  SetSnapshotManager(nil);
  ReleaseRefAndNil(FSnapshotNotification);

  if FThreadsNotification <> nil then begin;
    FThreadsNotification.OnChange := nil;
    FThreadsNotification.OnCurrent := nil;
  end;
  SetThreadsMonitor(nil);
  ReleaseRefAndNil(FThreadsNotification);

  if FCallStackNotification <> nil then begin;
    FCallStackNotification.OnChange := nil;
    FCallStackNotification.OnCurrent := nil;
  end;
  SetCallStackMonitor(nil);
  ReleaseRefAndNil(FCallStackNotification);

  if FLocalsNotification <> nil then begin;
    FLocalsNotification.OnChange := nil;
  end;
  SetLocalsMonitor(nil);
  ReleaseRefAndNil(FLocalsNotification);

  if FWatchesNotification <> nil then begin;
    FWatchesNotification.OnAdd := nil;
    FWatchesNotification.OnRemove := nil;
    FWatchesNotification.OnUpdate := nil;
  end;
  SetWatchesMonitor(nil);
  ReleaseRefAndNil(FWatchesNotification);

  if FRegistersNotification <> nil then begin;
    FRegistersNotification.OnChange := nil;
  end;
  SetRegistersMonitor(nil);
  ReleaseRefAndNil(FRegistersNotification);

  if FBreakpointsNotification <> nil then begin;
    FBreakpointsNotification.OnAdd := nil;
    FBreakpointsNotification.OnRemove := nil;
    FBreakpointsNotification.OnUpdate := nil;
  end;
  SetBreakPoints(nil);
  ReleaseRefAndNil(FBreakpointsNotification);

  inherited Destroy;
end;

procedure TDebuggerDlg.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Command: Word;
  Handled: Boolean;
begin
  Command := EditorOpts.KeyMap.TranslateKey(Key,Shift,TDebuggerDlg);

  if Assigned(OnProcessCommand) and (Command <> ecNone) and
     (Command <> ecContextHelp) and(Command <> ecEditContextHelp)
  then begin
    Handled:=false;
    OnProcessCommand(Self,Command,Handled);
    Key := 0;
  end;
end;

(*
procedure TDebuggerDlg.SetDebugger(const ADebugger: TDebugger);
begin
  FDebugger := ADebugger;
end;
*)
procedure TDebuggerDlg.DoClose(var CloseAction: TCloseAction);
begin
  CloseAction := caFree; // we default to free
  inherited DoClose(CloseAction);
end;

procedure TDebuggerDlg.DoBeginUpdate;
begin
end;

procedure TDebuggerDlg.DoEndUpdate;
begin
end;

initialization
  DBG_LOCATION_INFO := DebugLogger.FindOrRegisterLogGroup('DBG_LOCATION_INFO' {$IFDEF DBG_LOCATION_INFO} , True {$ENDIF} );

end.
