unit Delay;

{ AFS 14 Jan 2K
 abstraction of timer mechanism
 Use this to call a proc after a short delay

 Needed for IDE regiestering

 See delayed reg. technique from sample code by Mike Remec
  http://www.miharemec.com/doc/ota-nmi.html

 usage:

 lcMyDelay := TDelay.Create;
 lcMyDelay.ObjectProc := lcSomeObject.Proc;
 lcMyDelay.DoItSoon;
 ....
 lcMyDelay.Free;

 or

 lcMyDelay := TDelay.Create;
 lcMyDelay.Proc := SomeProc;
 lcMyDelay.DoItSoon;
 ....
 lcMyDelay.Free;

}

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is Delay, released May 2003.
The Initial Developer of the Original Code is Anthony Steele. 
Portions created by Anthony Steele are Copyright (C) 1999-2000 Anthony Steele.
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

{$I JcfGlobal.inc}

interface

uses ExtCtrls;

type
  TProcedure = procedure(var pbTryAgain: boolean);
  TObjectProcedure = procedure(var pbTryAgain: boolean) of object;

  TDelay = class(TObject)
  private
    fiDelay: integer;

    // can call a proc, or a proc on an object (or both)
    fcProc: TProcedure;
    fcObjectProc: TObjectProcedure;

    fcTimer: TTimer;
    fbDone: boolean;

    procedure DoItNow(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;

    procedure DoItSoon;

    { how long to delay in Miliseconds}
    property Delay: integer Read fiDelay Write fiDelay;

    { done yet? }
    property Done: boolean Read fbDone;

    { proc to call }
    property Proc: TProcedure Read fcProc Write fcProc;
    property objectProc: TobjectProcedure Read fcObjectProc Write fcObjectProc;
  end;


implementation

uses SysUtils;

const
  // default of 100ms = 1/0 second
  DEFAULT_DELAY = 100;

{ TDelay }
constructor TDelay.Create;
begin
  inherited;

  fcTimer := nil; // create the timer when needed
  fcProc  := nil;
  fcObjectProc := nil;

  fiDelay := DEFAULT_DELAY; // default 1/2 sec
  fbDone  := False;
end;

destructor TDelay.Destroy;
begin
  FreeAndNil(fcTimer);
  inherited;
end;


procedure TDelay.DoItNow(Sender: TObject);
var
  lbDoAgain: boolean;
begin
  Assert(fcTimer <> nil);

  lbDoAgain := False;
  // disable until finished
  fcTimer.Enabled := False;

  if Assigned(fcProc) then
    fcProc(lbDoAgain);
  if assigned(fcObjectProc) then
    fcObjectProc(lbDoAgain);

  //FreeAndNil(fcTimer); this causes problems in IDE plug-ins

  // stop unless the proc called raised the falg
  if lbDoAgain then
  begin
    // restart
    fcTimer.Enabled := True;
  end
  else
  begin
    fbDone := True;
      { no longer timing }
    fcTimer.OnTimer := nil;
  end;
end;

procedure TDelay.DoItSoon;
begin
  // need a timer now 
  if fcTimer = nil then
    fcTimer := TTimer.Create(nil);

  fcTimer.Interval := fiDelay;
  fcTimer.OnTimer := DoItNow;
  fcTimer.Enabled := True;
  fbDone := False;
end;

end.
