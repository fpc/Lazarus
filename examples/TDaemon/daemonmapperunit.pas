unit DaemonMapperUnit;

{$mode objfpc}{$H+}

interface

{$R *.lfm}

uses
  Classes, SysUtils, DaemonApp;

// ---------------------------------------------------------------------
// The mapper class holds the description of a daemon and provides the
// parameters for configuration, mostly required during the installation

// For a basic daemon, there is absolutely nothing to do in code here
// the defaults and what the lfm file provide are just fine.
// ---------------------------------------------------------------------

type
  TTestDaemonMapper = class(TDaemonMapper)
  private

  public

  end;

implementation

// ----------------------------------------
// Mapper registration. Created by template
// ----------------------------------------

procedure RegisterMapper;
begin
  RegisterDaemonMapper(TTestDaemonMapper);
end;

initialization
  RegisterMapper;
end.
