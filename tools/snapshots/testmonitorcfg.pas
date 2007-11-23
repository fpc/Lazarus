unit TestMonitorCfg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, monitorcfg;

type

  { TTestMonitorCfg }

  TTestMonitorCfg= class(TTestCase)
  private
    FMonitorCfg: TMonitorConfig;
  protected
    procedure SetUp; override; 
    procedure TearDown; override; 
  published
    procedure Test;
    procedure TestVersion;
  end; 

implementation

procedure TTestMonitorCfg.Test;
var
  Server : TServer;
begin
  AssertEquals('Wrong number of servers', 1, FMonitorCfg.ServerCount);
  Server := FMonitorCfg.Servers[0];
  AssertEquals(ord(stFtp), ord(Server.ServerType));
  AssertEquals('Snapshot on scenergy', Server.Description);
  AssertEquals('Wrong number of files', 18, Server.FileCount);
end;

procedure TTestMonitorCfg.TestVersion;
begin
  AssertEquals('Wrong Lazarus Version', '0.9.25', FMonitorCfg.LazVersion);
  AssertEquals('Wrong FPC Release Version', '2.2.0', FMonitorCfg.FPCReleaseVersion);
  AssertEquals('Wrong FPC Fixes Version', '2.2.1', FMonitorCfg.FPCFixesVersion);
  AssertEquals('Wrong FPC Devel Version', '2.3.1', FMonitorCfg.FPCDevelVersion);
end;

procedure TTestMonitorCfg.SetUp; 
begin
  FMonitorCfg := TMonitorConfig.Create;
  FMonitorCfg.Load('monitorconfig.xml');
end;

procedure TTestMonitorCfg.TearDown; 
begin
  FMonitorCfg.Free;
end; 

initialization

  //RegisterTest(TTestMonitorCfg);
end.

