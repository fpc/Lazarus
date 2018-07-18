program WatchConsoleSize;

(* This console-mode program for Linux or other unix implementations reports	*)
(* on the initial console size and outputs a message every time it gets a	*)
(* SIGWINCH indicating that the console window has been resized.		*)
(*										*)
(* It DOES NOT attempt any formatted output using escape sequences etc. MarkMLl	*)

uses
  SysUtils, Keyboard, Crt, TermIO, BaseUnix;

var
  signalCount: integer= 0;


procedure reportSize;

var
  winSize: TWinSize;

begin
  Write(signalCount, ': ');
  FillChar(winSize, sizeof(winSize), 0);
  if IsaTty(StdInputHandle) = 1 then
    if fpioctl(StdInputHandle, TIOCGWINSZ, @winSize) >= 0 then
      Write(winSize.ws_row, ' x ', winSize.ws_col);
  WriteLn;
  signalCount += 1
end { reportSize } ;


procedure winchHandler(sig: longint; {%H-}info: PSigInfo; {%H-}context: PSigContext); cdecl;

begin
  case sig of
    SIGWINCH: reportSize
  otherwise
  end
end { winchHandler } ;


function hookWinch(): boolean;

var
  action: SigActionRec;

begin
  FillChar(action{%H-}, SizeOf(action), 0);
  action.Sa_Handler := @winchHandler;
  action.Sa_Flags := SA_SIGINFO;
  hookWinch := fpSigAction(SIGWINCH, @action, nil) = 0
end { hookWinch } ;


begin
  WriteLn('This header line comprises 50 characters plus EOL.');
  WriteLn;
  WriteLn('Press key to terminate.'); // http://ars.userfriendly.org/cartoons/?id=20030128
  reportSize;
  if not hookWinch() then
    WriteLn('Failed: SIGWINCH not hooked, error ', fpGetErrNo)
  else begin
    while not KeyPressed() do
      Sleep(10);
    ReadKey
  end;
  WriteLn('It ends here.')
end.
  
