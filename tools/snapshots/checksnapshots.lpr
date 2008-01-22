program checksnapshots;

{$mode objfpc}{$H+}

uses
  Classes, consoletestrunner,
  SnapshotsUptodate;

type

  { TLazTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TMyTestRunner;

begin
  Application := TMyTestRunner.Create(nil);
  Application.Title:='Lazarus snapshots monitor';
  Application.Initialize;
  Application.Run;
  Application.Free;
end.
