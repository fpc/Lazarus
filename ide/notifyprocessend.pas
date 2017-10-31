{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Pascal Riekenberg

  Abstract:
    Provides a general classes for calling an event when a process ends
}
unit NotifyProcessEnd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process;

type
  { TNotifyProcessEnd }

  TNotifyProcessEnd = class(TThread)
  private
    fEvent: TThreadMethod;
    fProcess: TProcess;
  protected
    procedure Execute; override;
  public
    constructor Create(pProcess: TProcess; pEvent: TThreadMethod);
  end;

implementation

{ TNotifyProcessEnd }

procedure TNotifyProcessEnd.Execute;
begin
  fProcess.Execute;
  Synchronize(fEvent);
  fProcess.Free;
end;

constructor TNotifyProcessEnd.Create(pProcess: TProcess; pEvent: TThreadMethod);
begin
  inherited Create(True);
  fProcess := pProcess;
  fProcess.Options := fProcess.Options + [poWaitOnExit];
  fEvent := pEvent;
  FreeOnTerminate := True;
  Start;
end;

end.

