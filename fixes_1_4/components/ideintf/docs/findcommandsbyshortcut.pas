unit findcommandsbyshortcut;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, IDECommands;

procedure ListCtrlD;

implementation

procedure ListCtrlD;
// list all idecommands with shortcut Ctrl-D
var
  i: integer;
  Cmd: TIDECommand;
  Commands: TFPList;
begin
  Commands:=IDECommandList.FindCommandsByShortCut(IDEShortCut(VK_D,[],VK_UNKNOWN,[]));
  try
    for i:=0 to Commands.Count-1 do begin
      Cmd:=TIDECommand(Commands[i]);
      writeln('Cmd: ',Cmd.Name,
        ' A=',dbgs(Cmd.ShortcutA.Shift1),'-',Cmd.ShortcutA.Key1,
        ',',dbgs(Cmd.ShortcutA.Shift2),'-',Cmd.ShortcutA.Key2,
        ' B=',dbgs(Cmd.ShortcutB.Shift1),'-',Cmd.ShortcutB.Key1,
        ',',dbgs(Cmd.ShortcutB.Shift2),'-',Cmd.ShortcutB.Key2
        );
    end;
  finally
    Commands.Free;
  end;
end;

end.

