unit DaemonWorkerThread;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

// --------------------------------------------------------------------------------
// This is the "workhorse" of the daemon, and just a normal therad, see the Lazarus
// docs about threads and mutlitasking for details.

// Execute holds the main work code of the service
// Do not try the execute method of TDaemon, since it does not multitask,
// the service thread will stop responding to control messages if looping in the
// TDaemon execute method. Thus we need a worker thread.
// --------------------------------------------------------------------------------

type
  TDaemonWorkerThread = class(TThread)
  private
  public
    procedure Execute; override;  // the actual worker thread code goes here
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses DaemonUnit;

// --------------------------------------------------------------------------
// TThread.execute: this is the core of the workhorse, the routine which does
// actually hold the service's working code
// --------------------------------------------------------------------------

procedure TDaemonWorkerThread.Execute;

var
  i: integer;

begin
  LogToFile('Daemon worker thread executing');
  while not Terminated do
  begin
    // placeholder, put your actual service code here
    // ...
    LogToFile('Daemon worker thread running');
    // Thread- and CPUload friendly 5s delay loop
    for i := 1 to 50 do
    begin
      if Terminated then break;
      Sleep(100);
    end;
    // ...
    // ----------------------------------------
  end;
  LogToFile('Daemon worker thread terminated');
end;

// -------------------------------------------------
// Construction and destruction of the worker thread
// -------------------------------------------------

constructor TDaemonWorkerThread.Create;

begin
  // Create a suspended worker thread to allow further parametrizon before it
  // does actually start
  // The thread will be started if the OS sends a "Start" Signal to TDaemon
  // See OnStart event handler of the TDeamon class
  inherited Create(True);
  LogToFile('Daemon worker thread created');
end;

destructor TDaemonWorkerThread.Destroy;

begin
  // Nothing to do here, just for logging
  LogToFile('Daemon worker thread destroyed');
  inherited Destroy;
end;

end.
