{  $Id$  }
{
 /***************************************************************************
                          compiler.pp  -  Main application unit
                             -------------------
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
{$H+}
unit compiler;

{$mode objfpc}

interface

uses
  classes, sysutils, forms, compileroptions, Project,Global,Process,dlgMessage;

type

  TOutString = procedure (Value: String) of Object;
  TErrorType = (etNone, etHint, etError, etFatal, etWarning);
  
  TCompiler = class(TObject)
  private
    FProjectFile: String;
    FOutputFile: String;
    FOutputString : TOutString;
    FOutputList : TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Compile(MainUnit : String);
    function GetLineNumber(Value : String) : Integer;
    function GetColumnNumber(Value : String) : Integer;
    function GetMessageType(Value : String) : TErrorType;
    function GetUnitName(Value : String) : String;
    property OutputString : TOutString read FOutputString write FOutputString;
    property OutputList : TStringList read FOutputList;
  end;

var
  Compiler1 : TCompiler;

implementation


{------------------------------------------------------------------------------}
{  TCompiler Constructor                                                       }
{------------------------------------------------------------------------------}
constructor TCompiler.Create;
begin
  FOutputList := TStringList.Create;
end;

{------------------------------------------------------------------------------}
{  TCompiler Destructor                                                        }
{------------------------------------------------------------------------------}
destructor TCompiler.Destroy;
begin
  FOutputList.Free;
end;

{------------------------------------------------------------------------------}
{  TCompiler Compile                                                           }
{------------------------------------------------------------------------------}
procedure TCompiler.Compile(MainUnit : String);
const
  BufSize = 1024;
var
  TheProgram : String;
  Buf : Array[1..BUFSIZE] of char;
  I,Count : longint;
  Texts : String;
  NUm : Integer;
  WriteMessage : Boolean;

  TheProcess : TProcess;
begin

  Texts := '';
  FOutputList.Clear;
  TheProgram := CompilerOPts.CompilerPath;
  //TheProgram := TheProgram + ' -Ch'+inttostr(CompilerOpts.HeapSize);
  TheProgram := TheProgram + ' '+ CompilerOpts.MakeOptionsString;
  TheProgram := TheProgram + ' '+ MainUnit;
  Writeln('TheProgram = '+TheProgram);

  Assert(False, 'Trace:' + TheProgram);

  TheProcess:=TProcess.Create(TheProgram,[poRunSuspended,poUsePipes,poNoConsole]);

  TheProcess.Execute;

  if Assigned(OutputString) 
  then 
  repeat
    Count:=TheProcess.output.read(buf,BufSize);
    WriteMessage := False;
    for I:=1 to Count do
    begin
      if buf[i] = #10 
      then begin
        //determine what type of message it is
        if (pos(') Hint:',Texts) <> 0) then WriteMessage := CompilerOpts.ShowHints
        else
        if (pos(') Note:',Texts) <> 0) then WriteMessage := CompilerOpts.ShowNotes
        else
        if (pos(') Error:',Texts) <> 0) then WriteMessage := CompilerOpts.ShowErrors
        else
        if (pos(') Warning:',Texts) <> 0) then WriteMessage := CompilerOpts.ShowWarn
        else
        WriteMessage := True;
        
        FOutputList.Add(Texts);
  
        if (WriteMessage) or (CompilerOpts.ShowAll) 
        then begin
          OutputString(Texts);
          Application.ProcessMessages;
        end;
        Texts := '';
      end
      else Texts := Texts + buf[i];
    end;
  until Count=0;
  TheProcess.Free;
end;

{--------------------------------------------------------------------------
            TCompiler GetLineNumber
---------------------------------------------------------------------------}
function TCompiler.GetLineNumber(Value : String) : Integer;
var
  Texts : String;
  num : Integer;
  Temp : String;
begin
  {This assumes the error message will have the line number
   in the format like:
   (123,45)
  }

  Result := -1;
  Texts := Value;

  Num := Pos('(',Texts);
  if Num = 0 then Exit;
  try
    Temp := Copy(Texts,num+1,(pos(',',texts)-1)-Num);
    Result := StrtoInt(Temp);
  except
  end;

end;


{--------------------------------------------------------------------------
            TCompiler GetColumnNumber
---------------------------------------------------------------------------}
function TCompiler.GetColumnNumber(Value : String) : Integer;
var
  Texts : String;
  num : Integer;
  Temp : String;
begin
  {This assumes the error message will have the line number
   in the format like:
   (123,45)
  }

  Result := -1;
  Texts := Value;

  Num := Pos(',',Texts);
  if Num = 0 then Exit;
  try
    Temp := Copy(Texts,num+1,(pos(')',texts)-1)-Num);
    Result := StrtoInt(Temp);
  except
  end;
end;

{--------------------------------------------------------------------------
            TCompiler GetMessageType
---------------------------------------------------------------------------}
function TCompiler.GetMessageType(Value : String) : TErrorType;
var
  Texts : String;
  num : Integer;
  Temp : String;
begin
  {This assumes the error message will have the line number
   in the format like:
   (123,45)
  }
  
  Result := etNone;
  
  Texts := Value;
end;

{--------------------------------------------------------------------------
            TCompiler GetUnitname
---------------------------------------------------------------------------}
function TCompiler.GetUnitName(Value : String) : String;
var
  Texts : String;
  num : Integer;
  Temp : String;
begin
  {This assumes the error message will have the line number
   in the format like:
  Unitname(123,45)
  }
  Result := Copy(Value,1,pos('(',Value)-1);
end;


end.

{
  $Log$
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
