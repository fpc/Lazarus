{  $Id$  }
{
 /***************************************************************************
                        compiler.pp  -  Main application unit
                        -------------------------------------
                   TCompiler is responsible for configuration and running
                   the PPC386 compiler.


                   Initial Revision  : Sun Mar 28 23:15:32 CST 1999


 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
}
unit compiler;

{$mode objfpc}
{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, CompilerOptions, Project, Process;

type
  TOnOutputString = procedure (Value: String) of Object;
  TErrorType = (etNone, etHint, etWarning, etError, etFatal);
  TOnCmdLineCreate = procedure(var CmdLine: string; var Abort:boolean)
      of object;
  
  TCompiler = class(TObject)
  private
    FOnOutputString : TOnOutputString;
    FOutputList : TStringList;
    FOnCmdLineCreate : TOnCmdLineCreate;
  public
    constructor Create;
    destructor Destroy; override;
    function Compile(AProject: TProject): TModalResult;
    function GetSourcePosition(Line: string; var Filename:string;
       var CaretXY: TPoint; var MsgType: TErrorType): boolean;
    property OnOutputString : TOnOutputString
       read FOnOutputString write FOnOutputString;
    property OutputList : TStringList read FOutputList;
    property OnCommandLineCreate: TOnCmdLineCreate
       read FOnCmdLineCreate write FOnCmdLineCreate;
  end;

const
  ErrorTypeNames : array[TErrorType] of string = (
      'None','Hint','Warning','Error','Fatal'
    );

var
  Compiler1 : TCompiler;

function ErrorTypeNameToType(Name:string): TErrorType;


implementation

// to get more detailed error messages consider the os
{$IFDEF linux}
uses linux;
{$ENDIF linux}


function ErrorTypeNameToType(Name:string): TErrorType;
begin
  Name:=lowercase(Name);
  for Result:=Low(TErrorType) to High(TErrorType) do
    if lowercase(ErrorTypeNames[Result])=Name then exit;
  Result:=etNone;
end;

{ TCompiler }

{------------------------------------------------------------------------------}
{  TCompiler Constructor                                                       }
{------------------------------------------------------------------------------}
constructor TCompiler.Create;
begin
  inherited Create;
  FOutputList := TStringList.Create;
end;

{------------------------------------------------------------------------------}
{  TCompiler Destructor                                                        }
{------------------------------------------------------------------------------}
destructor TCompiler.Destroy;
begin
  FOutputList.Free;
  inherited Destroy;
end;

{------------------------------------------------------------------------------}
{  TCompiler Compile                                                           }
{------------------------------------------------------------------------------}
function TCompiler.Compile(AProject: TProject): TModalResult;
const
  BufSize = 1024;
var
  CmdLine : String;
  I, Count, LineStart : longint;
  OutputLine, Buf : String;
  WriteMessage, ABort : Boolean;
  OldCurDir, ProjectDir: string;
  TheProcess : TProcess;

  procedure ProcessOutputLine;
  begin
writeln('[TCompiler.Compile] Output="',OutputLine,'"');
    FOutputList.Add(OutputLine);

    //determine what type of message it is
    if (pos(') Hint:',OutputLine) <> 0) then
      WriteMessage := AProject.CompilerOptions.ShowHints
                   or AProject.CompilerOptions.ShowAll
    else if (pos(') Note:',OutputLine) <> 0) then
      WriteMessage := AProject.CompilerOptions.ShowNotes
                   or AProject.CompilerOptions.ShowAll
    else if (pos(') Error:',OutputLine) <> 0) then
      WriteMessage := AProject.CompilerOptions.ShowErrors
                   or AProject.CompilerOptions.ShowAll
    else if (pos(') Warning:',OutputLine) <> 0) then
      WriteMessage := AProject.CompilerOptions.ShowWarn
                   or AProject.CompilerOptions.ShowAll
    else if (copy(OutputLine,1,5)='Panic') or (pos(') Fatal:',OutputLine) <> 0)
    then begin
      Result:=mrCancel;
      WriteMessage := true;
    end;
    if (WriteMessage) and Assigned(OnOutputString) then
      OnOutputString(OutputLine);

    Application.ProcessMessages;
    OutputLine:='';
  end;

// TCompiler.Compile
begin
  Result:=mrCancel;
  if AProject.MainUnit<0 then exit;
  OldCurDir:=GetCurrentDir;
  ProjectDir:=ExtractFilePath(AProject.ProjectFile);
  if not SetCurrentDir(ProjectDir) then exit;
  try
    FOutputList.Clear;
    SetLength(Buf,BufSize);
    CmdLine := AProject.CompilerOptions.CompilerPath;
    if Assigned(FOnCmdLineCreate) then begin
      Abort:=false;
      FOnCmdLineCreate(CmdLine,Abort);
      if Abort then begin
        Result:=mrAbort;
        exit;
      end;
    end;
    // TProcess does not report, if a program can not be executed
    // to get good error messages consider the os
    {$IFDEF linux}
    if not Linux.Access(CmdLine,Linux.X_OK) then begin
      case LinuxError of
        sys_eacces: OutputLine:='execute access denied for "'+CmdLine+'"';
        sys_enoent: OutputLine:='a directory component in "'+CmdLine+'"'
                      +' does not exist or is a dangling symlink';
        sys_enotdir: OutputLine:='a directory component in "'+CmdLine+'"'
                      +' is not a directory';
        sys_enomem: OutputLine:='insufficient memory';
        sys_eloop: OutputLine:='"'+CmdLine+'" has a circular symbolic link';
      else
        OutputLine:='unable to execute "'+CmdLine+'"';
      end;
      OutputLine:='Error: '+OutputLine;
      if Assigned(OnOutputString) then
        OnOutputString(OutputLine);
      exit;
    end;
    {$ENDIF linux}
    CmdLine := CmdLine + ' '+ AProject.CompilerOptions.MakeOptionsString;
    CmdLine := CmdLine + ' '+ AProject.Units[AProject.MainUnit].Filename;
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
      TheProcess:=TProcess.Create(CmdLine,[poUsePipes,poNoConsole
           ,poStdErrToOutput]);
      Result:=mrOk;
      try
        TheProcess.CurrentDirectory:=ProjectDir;
        TheProcess.Execute;
        Application.ProcessMessages;

        OutputLine:='';
        repeat
          if TheProcess.Output<>nil then
            Count:=TheProcess.Output.Read(Buf[1],length(Buf))
          else
            Count:=0;         
          WriteMessage := False;
          LineStart:=1;
          i:=1;
          while i<=Count do begin
            if Buf[i] in [#10,#13] then begin
              OutputLine:=OutputLine+copy(Buf,LineStart,i-LineStart);
              ProcessOutputLine;
              if (i<Count) and (Buf[i+1] in [#10,#13]) and (Buf[i]<>Buf[i+1]) then
                inc(i);
              LineStart:=i+1;
            end;
            inc(i);
          end;
          OutputLine:=copy(Buf,LineStart,Count-LineStart+1);
        until Count=0;
      finally
        TheProcess.Free;
      end;
    except
      on e: Exception do begin
        writeln('[TCompiler.Compile] exception "',E.Message,'"');
        FOutputList.Add(E.Message);
        if Assigned(OnOutputString) then
          OnOutputString(E.Message);
        Result:=mrCancel;
        exit;
      end;
    end;
  finally
    SetCurrentDir(OldCurDir);
  end;
  writeln('[TCompiler.Compile] end');
end;

{--------------------------------------------------------------------------
            TCompiler GetSourcePosition
---------------------------------------------------------------------------}
function TCompiler.GetSourcePosition(Line: string; var Filename:string;
  var CaretXY: TPoint; var MsgType: TErrorType): boolean;
{This assumes the line will have the format
<filename>(123,45) <ErrorType>: <some text>
}
var StartPos, EndPos: integer;
begin
  Result:=false;
  StartPos:=1;
  // find filename
  EndPos:=StartPos;
  while (EndPos<=length(Line)) and (Line[EndPos]<>'(') do inc(EndPos);
  if EndPos>length(Line) then exit;
  FileName:=copy(Line,StartPos,EndPos-StartPos);
  // read linenumber
  StartPos:=EndPos+1;
  EndPos:=StartPos;
  while (EndPos<=length(Line)) and (Line[EndPos] in ['0'..'9']) do inc(EndPos);
  if EndPos>length(Line) then exit;
  CaretXY.Y:=StrToIntDef(copy(Line,StartPos,EndPos-StartPos),-1);
  // read column
  StartPos:=EndPos+1;
  EndPos:=StartPos;
  while (EndPos<=length(Line)) and (Line[EndPos] in ['0'..'9']) do inc(EndPos);
  if EndPos>length(Line) then exit;
  CaretXY.X:=StrToIntDef(copy(Line,StartPos,EndPos-StartPos),-1);
  // read error type
  StartPos:=EndPos+2;
  while (EndPos<=length(Line)) and (Line[EndPos]<>':') do inc(EndPos);
  if EndPos>length(Line) then exit;
  MsgType:=ErrorTypeNameToType(copy(Line,StartPos,EndPos-StartPos));
  Result:=true;
end;


end.

{
  $Log$
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
