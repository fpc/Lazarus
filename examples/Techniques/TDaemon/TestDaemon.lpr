Program TestDaemon;

Uses
{$IFDEF UNIX}{$IFDEF UseCThreads}
  CThreads,
{$ENDIF}{$ENDIF}
  DaemonApp, lazdaemonapp, daemonmapperunit, DaemonUnit, DaemonWorkerThread,
  DaemonSystemdInstallerUnit
  { add your units here };

begin
  Application.Initialize;
  Application.Run;
end.
