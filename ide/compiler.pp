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
    property OnOutputString : TOnOutputString read FOnOutputString write FOnOutputString;
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
  Buf : Array[1..BUFSIZE] of char;
  I, Count, LineStart : longint;
  Texts : String;
  WriteMessage, ABort : Boolean;
  OldCurDir: string;
  TheProcess : TProcess;
begin
  Result:=mrCancel;
  if AProject.MainUnit<0 then exit;
  OldCurDir:=GetCurrentDir;
  if not SetCurrentDir(ExtractFilePath(AProject.ProjectFile)) then exit;
  try
    Texts := '';
    FOutputList.Clear;
    CmdLine := CompilerOpts.CompilerPath;
    CmdLine := CmdLine + ' '+ CompilerOpts.MakeOptionsString;
    CmdLine := CmdLine + ' '+ AProject.ProjectFile;
    Writeln('[TCompiler.Compile] CmdLine="',CmdLine,'"');
    if Assigned(FOnCmdLineCreate) then begin
      Abort:=false;
      FOnCmdLineCreate(CmdLine,Abort);
      if Abort then begin
        Result:=mrAbort;
        exit;
      end;
    end;

    try
      TheProcess:=TProcess.Create(CmdLine,[poExecuteOnCreate,poUsePipes
         ,poNoConsole,poStdErrToOutput]);

      repeat
        Count:=TheProcess.Output.Read(Buf,BufSize);
writeln('[TCompiler.Compile] ',Count,' ',TheProcess.Running,' ',TheProcess.ExitStatus);
        if (Count=0) then begin
          TheProcess.Free;
          TheProcess:=nil;
        end;
        LineStart:=1;
        WriteMessage := False;
        for I:=1 to Count do begin
          if Buf[i] in [#10,#13] then begin
            SetLength(TextS,i-LineStart);
            if TextS<>'' then
              Move(Buf[LineStart],TextS[1],length(TextS));
writeln('[TCompiler.Compile] Output="',TextS,'"');
            //determine what type of message it is
            if (pos(') Hint:',Texts) <> 0) then
              WriteMessage := CompilerOpts.ShowHints or CompilerOpts.ShowAll
            else if (pos(') Note:',Texts) <> 0) then
              WriteMessage := CompilerOpts.ShowNotes or CompilerOpts.ShowAll
            else if (pos(') Error:',Texts) <> 0) then
              WriteMessage := CompilerOpts.ShowErrors or CompilerOpts.ShowAll
            else if (pos(') Warning:',Texts) <> 0) then
              WriteMessage := CompilerOpts.ShowWarn or CompilerOpts.ShowAll
            else if (copy(TextS,1,5)='Panic') or (pos(') Fatal:',Texts) <> 0) then
              WriteMessage := true;
            FOutputList.Add(Texts);

            if (WriteMessage) and Assigned(OnOutputString) then
              OnOutputString(Texts);

//            Application.ProcessMessages;
            if (Buf[i]=#13) and (i<Count) and (Buf[i+1]=#10) then inc(i);
            LineStart:=i+1;
          end;
        end;
      until Count=0;
      Writeln('-----------Exiting Compiler.Compile');
      Application.ProcessMessages;
      writeln('[TCompiler.Compile] 2');
    except
      on e: Exception do begin
        writeln('[TCompiler.Compile] exectpion "',E.Message,'"');
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
  Result:=mrOk;
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
