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
  Classes, SysUtils, Forms, Controls, CompilerOptions, Project, Process,
  IDEProcs, OutputFilter;

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
  if AProject.MainUnit<0 then exit;
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
          OutputFilter.ReadLine('Error: invalid compiler: '+E.Message,true);
        if CmdLine='' then begin
          if OutputFilter<>nil then
            OutputFilter.ReadLine('Hint: you can set the compiler path in '
             +'Environment->General Options->Files->Compiler Path',true);
        end;
        exit;
      end;
    end;
    if BuildAll then
      CmdLine := CmdLine+' -B';
    CmdLine := CmdLine
               + ' '+ AProject.CompilerOptions.MakeOptionsString(ProjectFilename)
               + ' '+ PrepareCmdLineOption(ProjectFilename);
    if Assigned(FOnCmdLineCreate) then begin
      Abort:=false;
      FOnCmdLineCreate(CmdLine,Abort);
      if Abort then begin
        Result:=mrAbort;
        exit;
      end;
    end;
    Writeln('[TCompiler.Compile] CmdLine="',CmdLine,'"');

    try
      TheProcess := TProcess.Create(nil);
      TheProcess.CommandLine := CmdLine;
      TheProcess.Options:= [poUsePipes, poNoConsole, poStdErrToOutPut];
      TheProcess.ShowWindow := swoNone;
      Result:=mrOk;
      try
        TheProcess.CurrentDirectory:=ProjectDir;
        
        if OutputFilter<>nil then begin
          OutputFilter.PrgSourceFilename:=ProjectFilename;
          OutputFilter.Options:=[ofoSearchForFPCMessages,ofoExceptionOnError];
          OutputFilter.Project:=AProject;
          OutputFilter.Execute(TheProcess);
        end else begin
          TheProcess.Execute;
          TheProcess.WaitOnExit;
        end;
      finally
        TheProcess.Free;
      end;
    except
      on e: EOutputFilterError do begin
        Result:=mrCancel;
        exit;
      end;
      on e: Exception do begin
        writeln('[TCompiler.Compile] exception "',E.Message,'"');
        if OutputFilter<>nil then
          OutputFilter.ReadLine(E.Message,true);
        Result:=mrCancel;
        exit;
      end;
    end;
  finally
    SetCurrentDir(OldCurDir);
  end;
  writeln('[TCompiler.Compile] end');
end;


end.

{
  $Log$
  Revision 1.35  2002/10/21 22:20:00  lazarus
  MG: reduced output

  Revision 1.34  2002/10/21 22:12:46  lazarus
  MG: fixed frmactivate

  Revision 1.33  2002/09/05 19:03:34  lazarus
  MG: improved handling of ambigious source files

  Revision 1.32  2002/07/05 12:34:08  lazarus
  MG: assembler errors are now shown in output

  Revision 1.31  2002/07/05 10:53:25  lazarus
  MG: fixed compiling for invalid programnames

  Revision 1.30  2002/05/10 06:57:38  lazarus
  MG: updated licenses

  Revision 1.29  2002/03/28 00:11:04  lazarus
  MG: removed unused

  Revision 1.28  2002/01/23 20:07:20  lazarus
  MG: added outputfilter

  Revision 1.27  2002/01/15 08:49:56  lazarus
  MG: fixed zombie compilers

  Revision 1.26  2002/01/13 12:46:17  lazarus
  MG: fixed linker options, compiler options dialog

  Revision 1.25  2001/12/16 22:24:54  lazarus
  MG: changes for new compiler 20011216

  Revision 1.24  2001/12/10 08:19:52  lazarus
  MG: added hint for unset compiler path

  Revision 1.23  2001/12/10 07:47:00  lazarus
  MG: minor fixes

  Revision 1.22  2001/11/21 13:09:49  lazarus
  MG: moved executable check to ideprocs.pp

  Revision 1.20  2001/11/09 20:48:36  lazarus
  Minor fixes
  Shane

  Revision 1.19  2001/11/09 18:39:11  lazarus
  MG: turned back to stable ground (use old process.pp)

  Revision 1.18  2001/11/07 16:14:11  lazarus
  MG: fixes for the new compiler

  Revision 1.17  2001/11/06 15:47:31  lazarus
  MG: added build all

  Revision 1.16  2001/11/05 18:18:13  lazarus
  added popupmenu+arrows to notebooks, added target filename

  Revision 1.15  2001/10/23 09:13:50  lazarus
  MG: fixed TestProject

  Revision 1.14  2001/07/08 22:33:56  lazarus
  MG: added rapid testing project

  Revision 1.13  2001/05/29 08:16:26  lazarus
  MG: bugfixes + starting programs

  Revision 1.12  2001/04/04 12:20:34  lazarus
  MG: added  add to/remove from project, small bugfixes

  Revision 1.11  2001/03/31 13:35:22  lazarus
  MG: added non-visual-component code to IDE and LCL

  Revision 1.10  2001/03/29 12:38:58  lazarus
  MG: new environment opts, ptApplication bugfixes

  Revision 1.9  2001/03/26 14:52:30  lazarus
  MG: TSourceLog + compiling bugfixes

  Revision 1.8  2001/03/12 09:34:51  lazarus
  MG: added transfermacros, renamed dlgmessage.pp to msgview.pp

  Revision 1.7  2001/02/06 13:38:57  lazarus
  Fixes from Mattias for EditorOPtions
  Fixes to COmpiler that should allow people to compile if their path is set up.
  Changes to code completion.
  Shane

  Revision 1.6  2001/02/04 18:24:41  lazarus
  Code cleanup
  Shane

  Revision 1.5  2001/01/31 06:26:23  lazarus
  Removed global unit.                                         CAW

  Revision 1.4  2001/01/13 06:11:06  lazarus
  Minor fixes
  Shane

  Revision 1.2  2000/12/20 20:04:30  lazarus
  Made PRoject Build compile the active unit.  This way we can actually play with it by compiling units.

  Revision 1.1  2000/07/13 10:27:46  michael
  + Initial import

  Revision 1.13  2000/07/09 20:18:55  lazarus
  MWE:
    + added new controlselection
    + some fixes
    ~ some cleanup

  Revision 1.12  2000/05/10 02:34:43  lazarus
  Changed writelns to Asserts except for ERROR and WARNING messages.   CAW

  Revision 1.11  2000/05/01 06:11:59  lazarus
  Changed to get compiler options from the Compiler Options dialog. This
  now makes the Compiler Options dialog fully functional.            CAW

  Revision 1.10  2000/04/18 20:06:39  lazarus
  Added some functions to Compiler.pp

  Revision 1.9  2000/04/17 06:47:40  lazarus
  Started implementing the ability to compile.          CAW

  Revision 1.8  1999/07/04 03:29:57  lazarus
  Code Cleaning

  Revision 1.7  1999/05/24 21:20:12  lazarus
  *** empty log message ***

  Revision 1.6  1999/05/17 22:22:34  lazarus
  *** empty log message ***

  Revision 1.5  1999/05/14 18:44:04  lazarus
  *** empty log message ***

  Revision 1.4  1999/05/14 14:53:00  michael
  + Removed objpas from uses clause

  Revision 1.3  1999/04/20 02:56:42  lazarus
  *** empty log message ***

  Revision 1.2  1999/04/18 05:42:05  lazarus
  *** empty log message ***

  Revision 1.1  1999/04/14 07:31:44  michael
  + Initial implementation

}
