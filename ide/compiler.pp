{  $Id$  }
{
 /***************************************************************************
                        compiler.pp  -  Lazarus IDE unit
                        -------------------------------------
               TCompiler is responsible for configuration and running
               the Free Pascal Compiler.


                   Initial Revision  : Sun Mar 28 23:15:32 CST 1999


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
unit Compiler;

{$mode objfpc}
{$H+}

interface

uses
  Classes, SysUtils, Process, LCLProc, Forms, Controls, FileUtil, InfoBuild,
  LazarusIDEStrConsts, CompilerOptions, Project, OutputFilter, UTF8Process,
  LazIDEIntf, ProjectIntf;

type
  TOnCmdLineCreate = procedure(var CmdLine: string; var Abort:boolean)
      of object;

  {$IFDEF WithAsyncCompile}
  TBuildProjectData = class
  public
    Reason: TCompileReason;
    Flags: TProjectBuildFlags;
    CompilerFilename: String;
    CompilerParams: String;
  end;
  {$ENDIF}

  { TCompiler }

  TCompiler = class(TObject)
  private
    {$IFDEF WithAsyncCompile}
    FASyncResult: TModalResult;
    {$ENDIF}
    FOnCmdLineCreate : TOnCmdLineCreate;
    FOutputFilter: TOutputFilter;
    FTheProcess: TProcessUTF8;
    FOldCurDir: string;
    {$IFDEF WithAsyncCompile}
    FFinishedCallback: TNotifyEvent;
    procedure CompilationFinished(Sender: TObject);
    {$ENDIF}
  {$IFDEF WithAsyncCompile}
  public
    // Values stored by caller, to be rtrieved on callback
    CallerData: TObject;
  {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;
    function Compile(AProject: TProject;
                     const WorkingDir, CompilerFilename, CompilerParams: string;
                     BuildAll, SkipLinking, SkipAssembler: boolean
                     {$IFDEF WithAsyncCompile}
                     ; aFinishedCallback: TNotifyEvent = nil
                     {$ENDIF}
                    ): TModalResult;
    procedure WriteError(const Msg: string);
    property OnCommandLineCreate: TOnCmdLineCreate read FOnCmdLineCreate
                                                   write FOnCmdLineCreate;
    property OutputFilter: TOutputFilter read FOutputFilter write FOutputFilter;
    property TheProcess: TProcessUTF8 read FTheProcess;
    {$IFDEF WithAsyncCompile}
    property ASyncResult: TModalResult read FASyncResult;
    {$ENDIF}
  end;


implementation


{ TCompiler }

{------------------------------------------------------------------------------
  TCompiler Constructor
------------------------------------------------------------------------------}

constructor TCompiler.Create;
begin
  inherited Create;
end;

{------------------------------------------------------------------------------
  TCompiler Destructor
------------------------------------------------------------------------------}
destructor TCompiler.Destroy;
begin
  FreeAndNil(FTheProcess);
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  TCompiler Compile
------------------------------------------------------------------------------}
function TCompiler.Compile(AProject: TProject;
  const WorkingDir, CompilerFilename, CompilerParams: string;
  BuildAll, SkipLinking, SkipAssembler: boolean
  {$IFDEF WithAsyncCompile} ; aFinishedCallback: TNotifyEvent = nil {$ENDIF}
  ): TModalResult;
var
  CmdLine : String;
  Abort : Boolean;
begin
  Result:=mrCancel;
  {$IFDEF WithAsyncCompile}
  FASyncResult:= mrNone;
  FFinishedCallback := aFinishedCallback;
  {$ENDIF}
  DebugLn('TCompiler.Compile WorkingDir="',WorkingDir,'" CompilerFilename="',CompilerFilename,'" CompilerParams="',CompilerParams,'"');

  // if we want to show the compile progress, it's now time to show the dialog
  CompileProgress.Show;

  // change working directory
  FOldCurDir:=GetCurrentDirUTF8;
  if not SetCurrentDirUTF8(WorkingDir) then begin
    WriteError('TCompiler.Compile unable to set working directory '+WorkingDir);
    exit;
  end;
  try
    CmdLine := CompilerFilename;
    
    if Assigned(FOnCmdLineCreate) then begin
      Abort:=false;
      FOnCmdLineCreate(CmdLine,Abort);
      if Abort then begin
        Result:=mrAbort;
        exit;
      end;
    end;
    try
      CheckIfFileIsExecutable(CmdLine);
    except
      on E: Exception do begin
        WriteError(Format(lisCompilerErrorInvalidCompiler, [E.Message]));
        if CmdLine='' then begin
          WriteError(lisCompilerHintYouCanSetTheCompilerPath);
        end;
        exit;
      end;
    end;
    if BuildAll then
      CmdLine := CmdLine+' -B';
    if SkipLinking and SkipAssembler then
      CmdLine := CmdLine+' -s'
    else if SkipLinking then
      CmdLine := CmdLine+' -Cn';
      
    if CompilerParams<>'' then
    CmdLine := CmdLine+' '+CompilerParams;
    if Assigned(FOnCmdLineCreate) then begin
      Abort:=false;
      FOnCmdLineCreate(CmdLine,Abort);
      if Abort then begin
        Result:=mrAbort;
        exit;
      end;
    end;
    DebugLn('[TCompiler.Compile] CmdLine="',CmdLine,'"');

    try
      if TheProcess=nil then
        FTheProcess := TOutputFilterProcess.Create(nil);
      TheProcess.CommandLine := CmdLine;
      TheProcess.Options:= [poUsePipes, poStdErrToOutput];
      TheProcess.ShowWindow := swoHide;
      Result:=mrOk;
      try
        TheProcess.CurrentDirectory:=WorkingDir;
        
        if OutputFilter<>nil then begin
          OutputFilter.Options:=[ofoSearchForFPCMessages,ofoExceptionOnError];
          OutputFilter.CompilerOptions:=AProject.CompilerOptions;
          {$IFDEF WithAsyncCompile}
          if aFinishedCallback <> nil then
          begin
            OutputFilter.ExecuteAsyncron(TheProcess, @CompilationFinished, Self);
          end
          else
          {$ENDIF}
            if not OutputFilter.Execute(TheProcess,Self) then
              if OutputFilter.Aborted then
                Result := mrAbort
              else
                Result := mrCancel;
        end else begin
          TheProcess.Execute;
        end;
      finally
        if TheProcess.Running
        {$IFDEF WithAsyncCompile} and ((OutputFilter = nil) or (aFinishedCallback = nil)) {$ENDIF}
        then begin
          TheProcess.WaitOnExit;
          if not (TheProcess.ExitStatus in [0,1]) then  begin
            WriteError(Format(listCompilerInternalError,[TheProcess.ExitStatus]));
            Result:=mrCancel;
          end;
        end;
      end;
    except
      on e: EOutputFilterError do begin
        Result:=mrCancel;
        exit;
      end;
      on e: Exception do begin
        DebugLn('[TCompiler.Compile] exception "',E.Message,'"');
        WriteError(E.Message);
        Result:=mrCancel;
        exit;
      end;
    end;
  finally
    SetCurrentDirUTF8(FOldCurDir);
  end;
  DebugLn('[TCompiler.Compile] end');
end;

{$IFDEF WithAsyncCompile}
procedure TCompiler.CompilationFinished(Sender: TObject);
begin
  if OutputFilter.Aborted then
    FASyncrResult := mrAbort
  else
    FASyncResult := mrOK;
  if TheProcess.Running then
  begin
    TheProcess.WaitOnExit;
    if (FASyncResult = mrOk) and not (TheProcess.ExitStatus in [0,1]) then
    begin
      WriteError(Format(listCompilerInternalError, [TheProcess.ExitStatus]));
      FASyncResult := mrCancel;
    end;
  end;
  DebugLn('[TCompiler.Compile] Async end');

  if Assigned(FFinishedCallback) then
    FFinishedCallback(Self);
end;
{$ENDIF}

procedure TCompiler.WriteError(const Msg: string);
begin
  DebugLn('TCompiler.WriteError ',Msg);
  if OutputFilter <> nil then
    OutputFilter.ReadConstLine(Msg, True);
end;


end.

