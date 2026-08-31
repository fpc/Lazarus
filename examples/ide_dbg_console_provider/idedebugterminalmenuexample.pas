{
  debugterminalmenu.pas
  ---------------------
  View > Debug Windows > Debug Terminal.

  A plug-in that brings its own window has to bring its own way of opening it:
  the IDE's chooser decides which window receives the stream, not how a given
  package's window is summoned. Without this the window can only ever appear by
  itself, when output arrives.

  SPDX-License-Identifier: MIT
}
unit IdeDebugTerminalMenuExample;

{$mode objfpc}{$H+}

interface

procedure Register;

implementation

uses
  Classes, SysUtils,
  // IdeIntf
  MenuIntf,
  // local
  IdeDebugTerminalPanelExample;

type

  { TDebugTerminalMenuHandler }

  TDebugTerminalMenuHandler = class
    procedure ShowClick(Sender: TObject);
  end;

var
  GHandler: TDebugTerminalMenuHandler = nil;

procedure TDebugTerminalMenuHandler.ShowClick(Sender: TObject);
begin
  (* Opens this plug-in's window whether or not it is the selected one. An
     unselected window says so in its caption and still shows what the last run
     printed, which is the point of keeping it around. *)
  ShowDebugTerminalForm;
end;

procedure Register;
begin
  if GHandler = nil then
    GHandler := TDebugTerminalMenuHandler.Create;
  { arg 4 is the method form of the click handler; arg 5 takes a plain
    procedure, which is what a method reference will not fit. }
  RegisterIDEMenuCommand(itmViewDebugWindows, 'ViewDebugTerminal',
    'Debug Terminal', @GHandler.ShowClick);
end;

finalization
  FreeAndNil(GHandler);

end.
