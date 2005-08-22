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
  Classes, SysUtils, LCLProc, Forms, Controls, CompilerOptions, Project,
  Process, LazarusIDEStrConsts, IDEProcs, OutputFilter, FileUtil;

type
  TOnCmdLineCreate = procedure(var CmdLine: string; var Abort:boolean)
      of object;
  
  TCompiler = class(TObject)
  private
    FOnCmdLineCreate : TOnCmdLineCreate;
    FOutputFilter: TOutputFilter;
  public
    constructor Create;
    destructor Destroy; override;
    function Compile(AProject: TProject; BuildAll: boolean;
      const DefaultFilename: string): TModalResult;
    property OnCommandLineCreate: TOnCmdLineCreate
      read FOnCmdLineCreate write FOnCmdLineCreate;
    property OutputFilter: TOutputFilter
      read FOutputFilter write FOutputFilter;
  end;


implementation


{ TCompiler }

{------------------------------------------------------------------------------}
{  TCompiler Constructor                                                       }
{------------------------------------------------------------------------------}
constructor TCompiler.Create;
begin
  inherited Create;
end;

{------------------------------------------------------------------------------}
{  TCompiler Destructor                                                        }
{------------------------------------------------------------------------------}
destructor TCompiler.Destroy;
begin
  inherited Destroy;
end;

{------------------------------------------------------------------------------}
{  TCompiler Compile                                                           }
{------------------------------------------------------------------------------}
function TCompiler.Compile(AProject: TProject; BuildAll: boolean;
  const DefaultFilename: string): TModalResult;
var
  CmdLine : String;
  Abort : Boolean;
  OldCurDir, ProjectDir, ProjectFilename: string;
  TheProcess : TProcess;
begin
  Result:=mrCancel;
  if AProject.MainUnitID<0 then exit;
  OldCurDir:=GetCurrentDir;
  if AProject.IsVirtual then
    ProjectFilename:=DefaultFilename
  else
    ProjectFilename:=AProject.MainUnitInfo.Filename;
  if ProjectFilename='' then exit;
  ProjectDir:=ExtractFilePath(ProjectFilename);
  if not SetCurrentDir(ProjectDir) then exit;
  try
    CmdLine := AProject.CompilerOptions.CompilerPath;
    
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
        if OutputFilter<>nil then
          OutputFilter.ReadLine(Format(lisCompilerErrorInvalidCompiler, [
            E.Message]), true);
        if CmdLine='' then begin
          if OutputFilter<>nil then
            OutputFilter.ReadLine(lisCompilerHintYouCanSetTheCompilerPath, true
              );
        end;
        exit;
      end;
    end;
    if BuildAll then
      CmdLine := CmdLine+' -B';
    CmdLine := CmdLine
               + ' '+ AProject.CompilerOptions.MakeOptionsString(
                                                         ProjectFilename,nil,[])
               + ' '+ PrepareCmdLineOption(ProjectFilename);
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
      TheProcess := TProcess.Create(nil);
      TheProcess.CommandLine := CmdLine;
      TheProcess.Options:= [poUsePipes, poStdErrToOutput];
      TheProcess.ShowWindow := swoHide;
      Result:=mrOk;
      try
        TheProcess.CurrentDirectory:=ProjectDir;
        
        if OutputFilter<>nil then begin
          OutputFilter.Options:=[ofoSearchForFPCMessages,ofoExceptionOnError];
          OutputFilter.CompilerOptions:=AProject.CompilerOptions;
          OutputFilter.Execute(TheProcess);
        end else begin
          TheProcess.Execute;
        end;
      finally
        TheProcess.WaitOnExit;
        TheProcess.Free;
      end;
    except
      on e: EOutputFilterError do begin
        Result:=mrCancel;
        exit;
      end;
      on e: Exception do begin
        DebugLn('[TCompiler.Compile] exception "',E.Message,'"');
        if OutputFilter<>nil then
          OutputFilter.ReadLine(E.Message,true);
        Result:=mrCancel;
        exit;
      end;
    end;
  finally
    SetCurrentDir(OldCurDir);
  end;
  DebugLn('[TCompiler.Compile] end');
end;


end.

