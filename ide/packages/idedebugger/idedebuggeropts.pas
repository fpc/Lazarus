unit IdeDebuggerOpts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IDEOptionsIntf, IdeDebuggerStringConstants;

type

  TDebuggerOptions = class(TAbstractIDEEnvironmentOptions)
    class function GetGroupCaption:string; override;
    class function GetInstance: TAbstractIDEOptions; override;
  end;

var
  DebuggerOptions: TDebuggerOptions = nil;


implementation

const
  DebuggerOptsConfFileName = 'debuggeroptions.xml';

{ TDebuggerOptions }

class function TDebuggerOptions.GetGroupCaption: string;
begin
  Result := dlgIdeDbgDebugger;
end;

class function TDebuggerOptions.GetInstance: TAbstractIDEOptions;
begin
  Result := DebuggerOptions;
end;



end.

