unit TestMonitorCfg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, monitorcfg;

type

  TTestMonitorCfg= class(TTestCase)
  private
    FMonitorCfg: TMonitorConfig;
  protected
    procedure SetUp; override; 
    procedure TearDown; override; 
  published
    procedure Test;
  end; 

implementation

procedure TTestMonitorCfg.Test;
var
  Server : TServer;
begin
  FMonitorCfg.Load('monitorconfig.xml');
  AssertEquals('Wrong number of servers', 1, FMonitorCfg.ServerCount);
  Server := FMonitorCfg.Servers[0];
  AssertEquals(ord(stFtp), ord(Server.ServerType));
  AssertEquals('Snapshot on scenergy', Server.Description);
  AssertEquals('Wrong number of files', 7, Server.FileCount);
end;

procedure TTestMonitorCfg.SetUp; 
begin
  FMonitorCfg := TMonitorConfig.Create;
end; 

procedure TTestMonitorCfg.TearDown; 
begin
  FMonitorCfg.Free;
end; 

initialization

  //RegisterTest(TTestMonitorCfg);
end.

