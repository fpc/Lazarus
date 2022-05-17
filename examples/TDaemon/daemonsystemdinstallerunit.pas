unit DaemonSystemdInstallerUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DaemonApp, IniFiles;

// ---------------------------------------------------------------------
// Helper functions for Unix service install/uninstall support (systemd)
// 2/2022 by arminlinder@arminlinder.de
// ---------------------------------------------------------------------

const
  DAEMON_CONFIG_FILE_PATH = '/lib/systemd/system';   // Unix systemd config file path

function GetSystemdControlFilePath(aDaemonName: string): string;
function CreateSystemdControlFile(aDaemon: TDaemon; aFilePath: string): boolean;
function RemoveSystemdControlFile(aFilePath: string): boolean;

implementation

function GetSystemdControlFilePath(aDaemonName: string): string;

begin
  Result := IncludetrailingBackslash(DAEMON_CONFIG_FILE_PATH) + aDaemonName + '.service';
end;

function CreateSystemdControlFile(aDaemon: TDaemon; aFilePath: string): boolean;

var
  f: TIniFile;

begin
  Result := False;
  try
    f := TIniFile.Create(aFilePath, []);
    // We use the definition given in the mapper class
    // The mapper class is assigned to the "Definition" property
    f.WriteString('Unit', 'Description', aDaemon.Definition.Description);
    f.WriteString('Unit', 'After', 'network.target');
    f.WriteString('Service', 'Type', 'simple');
    f.WriteString('Service', 'ExecStart', Application.ExeName + ' -r');
    f.WriteString('Install', 'WantedBy', 'multi-user.target');
    Result := True;
    // Removed Taken from old file sample, IMHO not required
    // f.WriteString('Service', 'TimeoutSec', '25');
    // f.WriteString('Service', 'RemainAfterExit', 'yes');
  finally
    f.Free;
  end;
end;

function RemoveSystemdControlFile(aFilePath: string): boolean;

  // Remove the control file, if it does exist

begin
  Result := True;
  if FileExists(aFilePath) then
    Result := DeleteFile(aFilePath);
end;

end.
