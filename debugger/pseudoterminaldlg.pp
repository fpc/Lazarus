{              ------------------------------------------------
                PseudoTerminalDlg.pp  -  Debugger helper class
               ------------------------------------------------

  This unit supports a form with a window acting as the console of a
  program being debugged, in particular in manages resize events.


 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}

unit PseudoTerminalDlg;

{$mode objfpc}{$H+}

{$IFDEF linux} {$DEFINE DBG_ENABLE_TERMINAL} {$ENDIF}

interface

uses
  IDEWindowIntf, Classes, Graphics,
  Forms, StdCtrls, DebuggerDlg, BaseDebugManager, LazarusIDEStrConsts, LCLType;

type

  { TPseudoConsoleDlg }

  TPseudoConsoleDlg = class(TDebuggerDlg)
    Memo1: TMemo;
    procedure FormResize(Sender: TObject);
    procedure Memo1UTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
  private
    { private declarations }
    ttyHandle: System.THandle;         (* Used only by unix for console size tracking  *)
    fCharHeight: word;
    fCharWidth: word;
    fRowsPerScreen: integer;
    fColsPerRow: integer;
    procedure getCharHeightAndWidth(consoleFont: TFont; out h, w: word);
    procedure consoleSizeChanged;
  protected
    procedure DoClose(var CloseAction: TCloseAction); override;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    procedure AddOutput(const AText: String);
    procedure Clear;
    property CharHeight: word read fCharHeight;
    property CharWidth: word read fCharWidth;
    property RowsPerScreen: integer read fRowsPerScreen;
    property ColsPerRow: integer read fColsPerRow;
  end;

var
  PseudoConsoleDlg: TPseudoConsoleDlg;


implementation

uses
  SysUtils, LazLoggerBase
{$IFDEF DBG_ENABLE_TERMINAL}
  , Unix, BaseUnix, termio
{$ENDIF DBG_ENABLE_TERMINAL}
  ;

const
  handleUnopened= THandle(-$80000000);

var
  DBG_VERBOSE, DBG_WARNINGS: PLazLoggerLogGroup;
  PseudoTerminalDlgWindowCreator: TIDEWindowCreator;

{ TPseudoConsoleDlg }

procedure TPseudoConsoleDlg.Memo1UTF8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
begin
  DebugBoss.DoSendConsoleInput(Utf8Key);
  Utf8Key := '';
end;


(* The form size has changed. Call a procedure to pass this to the kernel etc.,
  assuming that this works out the best control to track.
*)
procedure TPseudoConsoleDlg.FormResize(Sender: TObject);

var
  ttyNotYetInitialised: boolean;

begin

(* These are not errors so much as conditions we will see while the IDE is      *)
(* starting up.                                                                 *)

  if DebugBoss = nil then
    exit;
  if DebugBoss.PseudoTerminal = nil then
    exit;

(* Even if the IDE is initialised this can be called before the TTY is set up,  *)
(* so while we prefer success we also consider that failure /is/ an acceptable  *)
(* option in this case.                                                         *)

  ttyNotYetInitialised := ttyHandle = handleUnopened;
  //DebugLn(DBG_VERBOSE, ['TPseudoConsoleDlg.FormResize Calling consoleSizeChanged']);
  consoleSizeChanged;
  if ttyNotYetInitialised and (integer(ttyHandle) < 0) then begin
    DebugLn(DBG_WARNINGS, ['TPseudoConsoleDlg.FormResize Bad PseudoTerminal -> unopened']);
    ttyHandle := handleUnopened
  end
end;


procedure TPseudoConsoleDlg.DoClose(var CloseAction: TCloseAction);
begin
{$IFDEF DBG_ENABLE_TERMINAL}
  if integer(ttyHandle) >= 0 then begin
    FileClose(ttyHandle);
    ttyHandle := handleUnopened
  end;
{$ENDIF DBG_ENABLE_TERMINAL}
  inherited DoClose(CloseAction);
  CloseAction := caHide;
end;

constructor TPseudoConsoleDlg.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  font.Name := 'monospace';
  Caption:= lisDbgTerminal;
  ttyHandle := handleUnopened;
  fRowsPerScreen := -1;
  fColsPerRow := -1
end;


(* Get the height and width for characters described by the fount specified by
  the first parameter. This will normally be monospaced, but in case it's not
  use "W" which is normally the widest character in a typeface so that a
  subsequent conversion from a window size in pixels to one in character cells
  errs on the side of fewer rather than more rows and columns.
*)
procedure TPseudoConsoleDlg.getCharHeightAndWidth(consoleFont: TFont; out h, w: word);

var
  bm: TBitMap;

begin
  bm := TBitmap.Create;
  try
    bm.Canvas.Font.Assign(consoleFont);
    h := bm.Canvas.TextHeight('W');
    w := bm.Canvas.TextWidth('W')
  finally
    bm.Free
  end
end;


(* Assume that the console size has changed, either because it's just starting
  to be used or because a window has been resized. Use an ioctl() to tell a TTY
  to reconsider its opinion of itself, and if necessary send an explicit signal
  to the process being debugged. Assume that this is peculiar to unix-like OSes,
  but may be called safely by others.
*)
procedure TPseudoConsoleDlg.consoleSizeChanged;

{$IFDEF DBG_ENABLE_TERMINAL }
{ DEFINE USE_SLAVE_HANDLE }
{ DEFINE SEND_EXPLICIT_SIGNAL }

var
{$IFDEF USE_SLAVE_HANDLE }
  s: string;
{$ENDIF USE_SLAVE_HANDLE }
  winSize: TWinSize;

begin
  if ttyHandle = handleUnopened then

(* Assume that we get here when the first character is to be written by the     *)
(* program being debugged, and that the form and memo are fully initialised.    *)
(* Leave ttyHandle either open (i.e. >= 0) or -ve but no longer handleUnopened, *)
(* in the latter case no further attempt will be made to use it.                *)

// Requires -dDBG_WITH_DEBUGGER_DEBUG

    if DebugBoss.PseudoTerminal <> nil then begin
      //DebugLn(DBG_VERBOSE, ['TPseudoConsoleDlg.AddOutput PseudoTerminal.DevicePtyMaster=',
      //                  DebugBoss.PseudoTerminal.DevicePtyMaster]);
{$IFDEF USE_SLAVE_HANDLE }
      s := DebugBoss.PseudoTerminal.Devicename;
      //DebugLn(DBG_VERBOSE, ['TPseudoConsoleDlg.AddOutput PseudoTerminal.Devicename="', s, '"']);
      ttyHandle := fileopen(s, fmOpenWrite)
{$ELSE                   }
      ttyHandle := DebugBoss.PseudoTerminal.DevicePtyMaster;
{$ENDIF USE_SLAVE_HANDLE }
      //DebugLn(DBG_VERBOSE, ['TPseudoConsoleDlg.AddOutput ttyHandle=', ttyHandle]);
      getCharHeightAndWidth(Memo1.Font, fCharHeight, fCharWidth)
    end else begin                      (* Can't get pseudoterminal             *)
      DebugLn(DBG_WARNINGS, ['TPseudoConsoleDlg.AddOutput Unopened -> bad PseudoTerminal']);
      ttyHandle := THandle(-1)
    end;

(* Every time we're called, provided that we were able to open the TTY, work    *)
(* out the window size and tell the kernel and/or process.                      *)

  if integer(ttyHandle) >= 0 then begin (* Got slave TTY name and valid handle  *)
    with winSize do begin
      ws_xpixel := Memo1.ClientWidth;
      ws_ypixel := Memo1.ClientHeight;      (* Assume the font is monospaced         *)
      ws_row := ws_ypixel div fCharHeight;
      ws_col := ws_xpixel div fCharwidth;
      //DebugLn(DBG_VERBOSE, ['TPseudoConsoleDlg.AddOutput (rows x cols)=(', ws_row, ' x ', ws_col, ')']);

(* TIOCGWINSZ reports the console size in both character cells and pixels, but  *)
(* since we're not likely to be emulating e.g. a Tektronix terminal or one of   *)
(* the higher-end DEC ones it's reasonable to bow out here if the size hasn't   *)
(* changed by at least a full row or character.                                 *)

      if (ws_row = fRowsPerScreen) and (ws_col = fColsPerRow) then
        exit;
      fRowsPerScreen := ws_row;
      fColsPerRow := ws_col
    end;

(* Note that when the Linux kernel (or appropriate driver etc.) gets TIOCSWINSZ *)
(* it takes it upon itself to raise a SIGWINCH, I've not tested whether other   *)
(* unix implementations do the same. Because this is an implicit action, and    *)
(* because by and large the process receiving the signal can identify the       *)
(* sender and would be entitled to be unhappy if the sender appeared to vary,   *)
(* I've not attempted to defer signal sending in cases where the process being  *)
(* debugged is in a paused state or is otherwise suspected to not be able to    *)
(* handle it immediately. MarkMLl (so you know who to kick).                    *)

    if fpioctl(ttyHandle, TIOCSWINSZ, @winSize) < 0 then begin
      fileclose(ttyHandle);
      DebugLn(DBG_WARNINGS, ['TPseudoConsoleDlg.AddOutput Write failed, closed handle']);
      ttyHandle := THandle(-1)      (* Attempted ioctl() failed                 *)
    end;
    if integer(ttyHandle) >= 0 then begin (* Handle not closed by error         *)
{$IFDEF SEND_EXPLICIT_SIGNAL }
{$WARNING TPseudoConsoleDlg.consoleSizeChanged: Explicit signal untested }

// If I'm reading things correctly ReqCmd() is private, so this needs fettling.
// Need to introduce DebugBoss.SendSignal and Debugger.SendSignal

      //DebugBoss.Debugger.ReqCmd(dcSendSignal, [SIGWINCH]);
{$ENDIF SEND_EXPLICIT_SIGNAL }
      FillChar(winSize, sizeof(winSize), 0); (* Did it work?                    *)
      fpioctl(ttyHandle, TIOCGWINSZ, @winSize);
      //DebugLn(DBG_VERBOSE, ['TPseudoConsoleDlg.AddOutput readback=(', winSize.ws_row, ' x ', winSize.ws_col, ')'])
    end
  end;
{$ELSE       }
begin
  ttyHandle := THandle(-1);             (* Not used in non-unix OSes            *)
{$ENDIF DBG_ENABLE_TERMINAL }
  Assert(ttyHandle <> handleUnopened, 'TPseudoConsoleDlg.consoleSizeChanged: TTY handle still in virgin state at exit')
end;


procedure TPseudoConsoleDlg.AddOutput(const AText: String);

begin
  if ttyHandle = handleUnopened then begin (* Do this at first output only      *)
    //DebugLn(DBG_VERBOSE, ['TPseudoConsoleDlg.AddOutput Calling consoleSizeChanged']);
    consoleSizeChanged
  end;
  while Memo1.Lines.Count > 5000 do
    Memo1.Lines.Delete(0);

// Working note: make any adjustment to the number of lines etc. before we
// start to add text which might include escape handling.

  Memo1.Text:=Memo1.Text+AText;
  Memo1.SelStart := length(Memo1.Text);
end;

procedure TPseudoConsoleDlg.Clear;
begin
  //DebugLn(DBG_VERBOSE, ['TPseudoConsoleDlg.Clear Calling FormResize']);
  FormResize(nil);                      (* Safe during IDE initialisation       *)
  Memo1.Text := '';
end;

{$R *.lfm}

initialization
  DBG_VERBOSE := DebugLogger.FindOrRegisterLogGroup('DBG_VERBOSE' {$IFDEF DBG_VERBOSE} , True {$ENDIF} );
  DBG_WARNINGS := DebugLogger.FindOrRegisterLogGroup('DBG_WARNINGS' {$IFDEF DBG_WARNINGS} , True {$ENDIF} );

  PseudoTerminalDlgWindowCreator := IDEWindowCreators.Add(DebugDialogNames[ddtPseudoTerminal]);
  PseudoTerminalDlgWindowCreator.OnCreateFormProc := @CreateDebugDialog;
  PseudoTerminalDlgWindowCreator.CreateSimpleLayout;

end.

