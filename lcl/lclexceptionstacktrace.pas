unit LCLExceptionStackTrace;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, LazLoggerBase, Forms, LCLProc;

implementation

type
  TEventContainer = class
  public
    procedure HandleApplicationException(Sender: TObject; E: Exception);
  end;

{ TEventContainer }

procedure TEventContainer.HandleApplicationException(Sender: TObject;
  E: Exception);
begin
  if not (E is EAbort) then
  begin
    DebugLn('TApplication.HandleException ',E.Message);
    DumpExceptionBackTrace;
    Application.ShowException(E);
  end;
end;

procedure HandleOnShowException(Msg: ShortString);
begin
  DebugLn('TApplication.HandleException Strange Exception');
  DebugLn(Msg);
  DumpExceptionBackTrace;
end;

initialization
  SysUtils.OnShowException := @HandleOnShowException;
  Application.OnException := @TEventContainer(nil).HandleApplicationException;

end.

