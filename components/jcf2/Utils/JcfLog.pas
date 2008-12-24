{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is Log.pas, released April 2000.
The Initial Developer of the Original Code is Anthony Steele. 
Portions created by Anthony Steele are Copyright (C) 1999-2008 Anthony Steele.
All Rights Reserved. 
Contributor(s): Anthony Steele. 

The contents of this file are subject to the Mozilla Public License Version 1.1
(the "License"). you may not use this file except in compliance with the License.
You may obtain a copy of the License at http://www.mozilla.org/NPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied.
See the License for the specific language governing rights and limitations 
under the License.

Alternatively, the contents of this file may be used under the terms of
the GNU General Public License Version 2 or later (the "GPL") 
See http://www.gnu.org/licenses/gpl.html
------------------------------------------------------------------------------*)
{*)}

unit JcfLog;

{ Created AFS 2 Dec 1999

  Log file }

{$I JcfGlobal.inc}

interface

{ log level: only have two levels at present
  message and error
  messages can be turned off, errors are always logged }
type
  TLogLevel = (llMessage, llError);

type
  TJCFLog = class(TObject)
  private
    fOpen: boolean;
    fsLogFileName: string;
    feLogLevel: TLogLevel;

    { worker vars }
    fLog: TextFile;
    { worker procs }
    procedure OpenLog;

  protected

  public
    constructor Create(const psFileName: string);
    destructor Destroy; override;

    procedure Write(const ps: string; const peLogLevel: TLogLevel = llMessage);
    procedure WriteError(const ps: string);

    procedure EmptyLine;

    procedure CloseLog;

    // can't change file at runtime
    property FileName: string Read fsLogFileName;

    // can change log level
    property LogLevel: TLogLevel Read feLogLevel Write feLogLevel;
  end;

function Log: TJCFLog;

implementation

uses
  SysUtils,
  JcfRegistrySettings;

procedure TJCFLog.CloseLog;
begin
  if fOpen then
  begin
    Flush(FLog);
    CloseFile(FLog);
    fOpen := False;
  end;
end;

constructor TJCFLog.Create(const psFileName: string);
begin
  inherited Create;

  Assert(psFileName <> '');
  fOpen := False;
  fsLogFileName := psFileName;
end;

destructor TJCFLog.Destroy;
begin
  CloseLog;
  inherited;
end;

procedure TJCFLog.EmptyLine;
begin
  OpenLog;
  WriteLn(Flog, '');
  // no need to flush now - if theprogram dies right here, no new info is lost
end;


// this one always gets through
procedure TJCFLog.Write(const ps: string; const peLogLevel: TLogLevel = llMessage);
begin
  if peLogLevel < LogLevel then
    exit;

  OpenLog;
  WriteLn(Flog, ps);
  Flush(FLog);
end;

procedure TJCFLog.WriteError(const ps: string);
begin
  Write(ps, llError);
end;

procedure TJCFLog.OpenLog;
begin
  if not fOpen then
  begin
    AssignFile(FLog, fsLogFileName);
    Rewrite(FLog);
    fOpen := True;

    { do this no matter what the logging level, unless a log = off level is introduced }
    WriteLn(Flog, 'Logging started at ' + FormatDateTime('dd mmm yyyy hh:mm:ss',
      Date + Time));
  end;
end;


{ module var for singleton }
var
  mcLog: TJCFLog;

function Log: TJCFLog;
begin
  if mcLog = nil then
  begin
    mcLog := TJCFLog.Create(GetRegSettings.LogFileName);
  end;

  Result := mcLog;
end;

initialization
  mcLog := nil;

finalization
  FreeAndNil(mcLog);
end.
