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
{$IFDEF linux} {$DEFINE DBG_ENABLE_TERMINAL} {$ENDIF}


{$mode objfpc}{$H+}

interface

uses
  IDEWindowIntf, Classes, Graphics, Forms, StdCtrls, DebuggerDlg,
  BaseDebugManager, LazarusIDEStrConsts, LCLType, ComCtrls, ExtCtrls, MaskEdit,
  PairSplitter;

type

  { TPseudoConsoleDlg }

  TPseudoConsoleDlg = class(TDebuggerDlg)
    CheckGroupRight: TCheckGroup;
    GroupBoxRight: TGroupBox;
    MaskEdit1: TMaskEdit;
    Memo1: TMemo;
    PageControl1: TPageControl;
    PairSplitterRaw: TPairSplitter;
    PairSplitterRawLeft: TPairSplitterSide;
    PairSplitterRawRight: TPairSplitterSide;
    Panel1: TPanel;
    PanelRightBelowRG: TPanel;
    PanelRightBelowCG: TPanel;
    RadioGroupRight: TRadioGroup;
    StatusBar1: TStatusBar;
    TabSheet1: TTabSheet;
    TabSheetRaw: TTabSheet;
    procedure FormResize(Sender: TObject);
    procedure Memo1UTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure PairSplitterRawRightResize(Sender: TObject);
    procedure RadioGroupRightSelectionChanged(Sender: TObject);
  private
    { private declarations }
    ttyHandle: System.THandle;         (* Used only by unix for console size tracking  *)
    fCharHeight: word;
    fCharWidth: word;
    fRowsPerScreen: integer;
    fColsPerRow: integer;
    fFirstLine: integer;
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
  SysUtils, StrUtils, LazLoggerBase
{$IFDEF DBG_ENABLE_TERMINAL}
  , Unix, BaseUnix, termio
{$ENDIF DBG_ENABLE_TERMINAL}
  ;

const
  handleUnopened= THandle(-$80000000);

var
  //DBG_VERBOSE,
  DBG_WARNINGS: PLazLoggerLogGroup;
  PseudoTerminalDlgWindowCreator: TIDEWindowCreator;

{ TPseudoConsoleDlg }

procedure TPseudoConsoleDlg.Memo1UTF8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
begin
  DebugBoss.DoSendConsoleInput(Utf8Key);
  Utf8Key := '';
end;


procedure TPseudoConsoleDlg.PairSplitterRawRightResize(Sender: TObject);

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
  //DebugLn(DBG_VERBOSE, ['TPseudoConsoleDlg.SplitterRawRightResize Calling consoleSizeChanged']);
  consoleSizeChanged;
  if ttyNotYetInitialised and (integer(ttyHandle) < 0) then begin
    DebugLn(DBG_WARNINGS, ['TPseudoConsoleDlg.SplitterRawRightResize Bad PseudoTerminal -> unopened']);
    ttyHandle := handleUnopened
  end;
  StatusBar1.Panels[3].Text := 'Splitter resized'
end { TPseudoConsoleDlg.PairSplitterRawRightResize } ;


(* The C1 underbar decoration is only relevant when C0 is being displayed as
  control pictures or ISO 2047 glyphs.
*)
procedure TPseudoConsoleDlg.RadioGroupRightSelectionChanged(Sender: TObject);

begin
  case RadioGroupRight.ItemIndex of
    1, 2: CheckGroupRight.CheckEnabled[1] := true
  otherwise
    CheckGroupRight.CheckEnabled[1] := false
  end
end { TPseudoConsoleDlg.RadioGroupRightSelectionChanged } ;


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
  end;
  StatusBar1.Panels[3].Text := 'Window resized'
end { TPseudoConsoleDlg.FormResize } ;


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
end { TPseudoConsoleDlg.DoClose } ;


constructor TPseudoConsoleDlg.Create(TheOwner: TComponent);

begin
  inherited Create(TheOwner);
  font.Name := 'monospace';
  Caption:= lisDbgTerminal;
  ttyHandle := handleUnopened;
  fRowsPerScreen := -1;
  fColsPerRow := -1;
  fFirstLine := 1
end { TPseudoConsoleDlg.Create } ;


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
end { TPseudoConsoleDlg.getCharHeightAndWidth } ;


(* Assume that the console size has changed, either because it's just starting
  to be used or because a window has been resized. Use an ioctl() to tell a TTY
  to reconsider its opinion of itself, and if necessary send an explicit signal
  to the process being debugged. Assume that this is peculiar to unix-like OSes,
  but may be called safely by others.
*)
procedure TPseudoConsoleDlg.consoleSizeChanged;

{$IFDEF DBG_ENABLE_TERMINAL}
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
{$ENDIF DBG_ENABLE_TERMINAL}
  Assert(ttyHandle <> handleUnopened, 'TPseudoConsoleDlg.consoleSizeChanged: TTY handle still in virgin state at exit');
  RadioGroupRightSelectionChanged(nil); (* Sort out initial state               *)
  StatusBar1.Panels[0].Width := Width div 4;
  StatusBar1.Panels[0].Text := '    ' ; // + DebugBoss.Debugger.Environment.Values['TERM'];
  StatusBar1.Panels[1].Width := Width div 4;
{$IFDEF DBG_ENABLE_TERMINAL}
  StatusBar1.Panels[1].Text := Format('%d cols x %d rows', [winsize.ws_col, winsize.ws_row]);
{$ENDIF DBG_ENABLE_TERMINAL}
  StatusBar1.Panels[2].Width := Width div 4;
{$IFDEF DBG_ENABLE_TERMINAL}
  StatusBar1.Panels[2].Text := Format('%d x %d pixels', [winsize.ws_xpixel, winsize.ws_ypixel])
{$ENDIF DBG_ENABLE_TERMINAL}
end { TPseudoConsoleDlg.consoleSizeChanged } ;


procedure TPseudoConsoleDlg.AddOutput(const AText: String);

var
  lineLimit, numLength, i: integer;
  buffer: TStringList;


  (* Translate C0 control codes to "control pictures", and optionally C1 codes
    to the same glyph but with an underbar.
  *)
  function withControlPictures(const str: string; c1Underbar: boolean): widestring;

  const
    nul= #$2400;                        // ␀
    soh= #$2401;                        // ␁
    stx= #$2402;                        // ␂
    etx= #$2403;                        // ␃
    eot= #$2404;                        // ␄
    enq= #$2405;                        // ␅
    ack= #$2406;                        // ␆
    bel= #$2407;                        // ␇
    bs=  #$2408;                        // ␈
    ht=  #$2409;                        // ␉
    lf=  #$240a;                        // ␊
    vt=  #$240b;                        // ␋
    ff=  #$240c;                        // ␌
    cr=  #$240d;                        // ␍
    so=  #$240e;                        // ␎
    si=  #$240f;                        // ␏
    dle= #$2410;                        // ␐
    dc1= #$2411;                        // ␑
    dc2= #$2412;                        // ␒
    dc3= #$2413;                        // ␓
    dc4= #$2414;                        // ␔
    nak= #$2415;                        // ␕
    syn= #$2416;                        // ␖
    etb= #$2417;                        // ␗
    can= #$2418;                        // ␘
    em=  #$2419;                        // ␙
    sub= #$241a;                        // ␚
    esc= #$241b;                        // ␛
    fs=  #$241c;                        // ␜
    gs=  #$241d;                        // ␝
    rs=  #$241e;                        // ␞
    us=  #$241f;                        // ␟
    del= #$2420;                        // ␡
    bar= #$033c;                        // ̼'

  var
    i, test, masked: integer;
    changed: boolean;

  begin
    SetLength(result, Length(str));

  (* This should probably be recoded to use a persistent table, but doing it    *)
  (* this way results in no lookup for plain text which is likely to be the     *)
  (* bulk of the output. I'm not making any assumptions about the Unicode       *)
  (* characters being sequential so that this code can be used both for control *)
  (* pictures and ISO-2047 glyphs, and so that if somebody has (good) reason to *)
  (* want to adjust them he can do so.                                          *)

    for i := Length(str) downto 1 do begin
      test := Ord(str[i]);
      if c1Underbar then
        masked := test and $7f          (* Handle both C0 and C1 in one operation *)
      else
        masked := test;
      changed := true;
      case masked of
        $00: result[i] := nul;
        $01: result[i] := soh;
        $02: result[i] := stx;
        $03: result[i] := etx;
        $04: result[i] := eot;
        $05: result[i] := enq;
        $06: result[i] := ack;
        $07: result[i] := bel;
        $08: result[i] := bs;
        $09: result[i] := ht;
        $0a: result[i] := lf;
        $0b: result[i] := vt;
        $0c: result[i] := ff;
        $0d: result[i] := cr;
        $0e: result[i] := so;
        $0f: result[i] := si;
        $10: result[i] := dle;
        $11: result[i] := dc1;
        $12: result[i] := dc2;
        $13: result[i] := dc3;
        $14: result[i] := dc4;
        $15: result[i] := nak;
        $16: result[i] := syn;
        $17: result[i] := etb;
        $18: result[i] := can;
        $19: result[i] := em;
        $1a: result[i] := sub;
        $1b: result[i] := esc;
        $1c: result[i] := fs;
        $1d: result[i] := gs;
        $1e: result[i] := rs;
        $1f: result[i] := us;
        $7f: result[i] := del
      otherwise
        result[i] := Chr(test);
        changed := false;
      end;
      if c1Underbar and changed and     (* Now fix changed C1 characters        *)
                                (masked <> test) then
        Insert(bar, result, i)
    end
  end { withControlPictures } ;


  (* Translate C0 control codes to "pretty pictures", and optionally C1 codes
    to the same glyph but with an underbar.
  *)
  function withIso2047(const str: string; c1Underbar: boolean): widestring;

  (* I've not got access to a pukka copy of ISO-2047, so like (it appears)      *)
  (* almost everybody else I'm assuming that the Wikipedia page is correct.     *)
  (* this differs from the ECMA standard (only) in the backspace glyph, some    *)
  (* terminals in particular the Burroughs TD730/830 range manufactured in the  *)
  (* 1970s and 1980s depart slightly more. I've found limited open source       *)
  (* projects that refer to this encoding, and those I've found have attempted  *)
  (* to "correct" details like the "direction of rotation" of the glyphs for    *)
  (* the DC1 through DC4 codes.                                                 *)
  (*                                                                            *)
  (* Suffixes W, E and B below refer to the variants found in the Wikipedia     *)
  (* article, the ECMA standard and the Burroughs terminal documentation.       *)

  const
    nul=  #$2395;                       // ⎕
    soh=  #$2308;                       // ⌈
    stx=  #$22A5;                       // ⊥
    etx=  #$230B;                       // ⌋
    eot=  #$2301;                       // ⌁
    enq=  #$22A0;                       // ⊠
    ack=  #$2713;                       // ✓
    bel=  #$237E;                       // ⍾
    bsW=  #$232B;                       // ⌫
    bsB=  #$2196;                       // ↖ The ECMA glyph is slightly curved
    bs=   bsB;                          //   and has no Unicode representation.
    ht=   #$2AAB;                       // ⪫
    lf=   #$2261;                       // ≡
    vt=   #$2A5B;                       // ⩛
    ff=   #$21A1;                       // ↡
    crW=  #$2aaa;                       // ⪪ ECMA the same
    crB=  #$25bf;                       // ▿
    cr=   crW;
    so=   #$2297;                       // ⊗
    si=   #$2299;                       // ⊙
    dle=  #$229F;                       // ⊟
    dc1=  #$25F7;                       // ◷ Nota bene: these rotate deosil
    dc2=  #$25F6;                       // ◶
    dc3=  #$25F5;                       // ◵
    dc4=  #$25F4;                       // ◴
    nak=  #$237B;                       // ⍻
    syn=  #$238D;                       // ⎍
    etb=  #$22A3;                       // ⊣
    can=  #$29D6;                       // ⧖
    em=   #$237F;                       // ⍿
    sub=  #$2426;                       // ␦
    esc=  #$2296;                       // ⊖
    fs=   #$25F0;                       // ◰ Nota bene: these rotate widdershins
    gsW=  #$25F1;                       // ◱ ECMA the same
    gsB=  #$25b5;                       // ▵
    gs=   gsW;
    rsW=  #$25F2;                       // ◲ ECMA the same
    rsB=  #$25c3;                       // ◃
    rs=   rsW;
    usW=  #$25F3;                       // ◳ ECMA the same
    usB=  #$25b9;                       // ▹
    us=   usW;
    del=  #$2425;                       // ␥
    bar=  #$033c;                       // ̼'

(* Not represented above is a Burroughs glyph for ETX, which in the material    *)
(* available to me appears indistinguisable from CAN. If anybody has variant    *)
(* glyphs from other manufacturers please contribute.                           *)

  var
    i, test, masked: integer;
    changed: boolean;

  begin
    SetLength(result, Length(str));

  (* This should probably be recoded to use a persistent table, but doing it    *)
  (* this way results in no lookup for plain text which is likely to be the     *)
  (* bulk of the output. I'm not making any assumptions about the Unicode       *)
  (* characters being sequential so that this code can be used both for control *)
  (* pictures and ISO-2047 glyphs, and so that if somebody has (good) reason to *)
  (* want to adjust them she can do so.                                         *)

    for i := Length(str) downto 1 do begin
      test := Ord(str[i]);
      if c1Underbar then
        masked := test and $7f          (* Handle both C0 and C1 in one operation *)
      else
        masked := test;
      changed := true;
      case masked of
        $00: result[i] := nul;
        $01: result[i] := soh;
        $02: result[i] := stx;
        $03: result[i] := etx;
        $04: result[i] := eot;
        $05: result[i] := enq;
        $06: result[i] := ack;
        $07: result[i] := bel;
        $08: result[i] := bs;
        $09: result[i] := ht;
        $0a: result[i] := lf;
        $0b: result[i] := vt;
        $0c: result[i] := ff;
        $0d: result[i] := cr;
        $0e: result[i] := so;
        $0f: result[i] := si;
        $10: result[i] := dle;
        $11: result[i] := dc1;
        $12: result[i] := dc2;
        $13: result[i] := dc3;
        $14: result[i] := dc4;
        $15: result[i] := nak;
        $16: result[i] := syn;
        $17: result[i] := etb;
        $18: result[i] := can;
        $19: result[i] := em;
        $1a: result[i] := sub;
        $1b: result[i] := esc;
        $1c: result[i] := fs;
        $1d: result[i] := gs;
        $1e: result[i] := rs;
        $1f: result[i] := us;
        $7f: result[i] := del
      otherwise
        result[i] := Chr(test);
        changed := false;
      end;
      if c1Underbar and changed and     (* Now fix changed C1 characters        *)
                                (masked <> test) then
        Insert(bar, result, i)
    end
  end { withIso2047 } ;


  (* Convert the string that's arrived from GDB etc. into UTF-8. In this case
    it's mostly a dummy operation, except that there might be widget-set-specific
    hacks.
  *)
  function widen(const str: string): widestring;

  const
    dot= #$00B7;                        // ·

  var
    i: integer;

  begin
    SetLength(result, Length(str));
    for i := Length(str) downto 1 do
      case str[i] of
        ' ': result[i] := ' ';          (* Satisfy syntax requirement           *)
        #$00: result[i] := dot;         (* GTK2 really doesn't like seeing this *)
//        #$01..#$0f:   result[i] := dot;
//        #$10..#$1f: result[i] := dot;
//        #$7f:       result[i] := dot;
//        #$80..#$ff: result[i] := dot
      otherwise
        result[i] := str[i]
      end
  end { widen } ;


  (* Look at the line index cl in a TStringList. Assume that at the start there
    will be a line number and padding occupying nl characters, after that will
    be text. Convert the text to hex possibly inserting extra lines after the
    one being processed, only the first (i.e. original) line has a line number.
  *)
  procedure expandAsHex(var stringList: TStringList; currentLine, lineNumberLength: integer);

  var
    lineNumberAsText: string;
    dataAsByteArray: TBytes;
    lengthLastBlock, startLastBlock: integer;


    (* Recursively process the byte array from the end to the beginning. All
      lines are inserted immediately after the original current line, except for
      the final line processed which overwrites the original.
    *)
    procedure hexLines(start, bytes: integer);


      (* The parameter is a line number as text or an equivalent run of spaces.
        The result is a line of hex + ASCII data.
      *)
      function oneHexLine(const lineNum: string): widestring;

      var
        i: integer;

      begin
        result := lineNum;
        for i := 0 to 15 do
          if i < bytes then
            result += LowerCase(HexStr(dataAsByteArray[start + i], 2)) + ' '
          else
            result += '   ';
        result += ' ';                  (* Between hex and ASCII                *)
        for i := 0 to 15 do
          if i < bytes then
            case dataAsByteArray[start + i] of
              $20..$7e: result += Chr(dataAsByteArray[start + i])
            otherwise
              result += #$00B7          // ·
            end
      end { oneHexLine } ;


    begin
      if start = 0 then
        stringList[currentLine] := oneHexLine(lineNumberAsText)
      else begin
        stringList.insert(currentLine + 1, oneHexLine(PadLeft('', Length(lineNumberAsText))));
        hexLines(start - 16, 16)
      end
    end { hexLines } ;


  begin
    if lineNumberLength = 0 then begin
      lineNumberAsText := '';
      dataAsByteArray := BytesOf(Copy(stringList[currentLine], 1,
                                Length(stringList[currentLine])))
    end else begin                      (* Remember one extra space after number *)
      lineNumberAsText := Copy(stringList[currentLine], 1, lineNumberLength + 1);
      dataAsByteArray := BytesOf(Copy(stringList[currentLine], lineNumberLength + 2,
                                Length(stringList[currentLine]) - (lineNumberLength + 1)))
    end;
    if (Length(dataAsByteArray) > 0) and ((Length(dataAsByteArray) mod 16) = 0) then
      lengthLastBlock := 16
    else
      lengthLastBlock := Length(dataAsByteArray) mod 16;
    startLastBlock := Length(dataAsByteArray) - lengthLastBlock;
    hexLines(startLastBlock, lengthLastBlock)
  end { expandAsHex } ;


begin
  if ttyHandle = handleUnopened then begin (* Do this at first output only      *)
    //DebugLn(DBG_VERBOSE, ['TPseudoConsoleDlg.AddOutput Calling consoleSizeChanged']);
    consoleSizeChanged
  end;

(* Get the maximum number of lines to be displayed from the user interface,     *)
(* work out how much space is needed to display a line number, and if necessary *)
(* trim the amount of currently-stored text.                                    *)

  try
    lineLimit := StrToInt(Trim(MaskEdit1.Text))
  except
    MaskEdit1.Text := '5000';
    lineLimit := 5000
  end;
  if CheckGroupRight.Checked[0] then    (* Line numbers?                        *)
    case lineLimit + fFirstLine - 1 of
      0..999:          numLength := 3;
      1000..99999:     numLength := 5;
      100000..9999999: numLength := 7
    otherwise
      numLength := 9
    end
  else
    numLength := 0;
  Memo1.Lines.BeginUpdate;
  while Memo1.Lines.Count > lineLimit do
    Memo1.Lines.Delete(0);

(* Use an intermediate buffer to process the line or potentially lines of text  *)
(* passed as the parameter; where formatting as hex breaks it up into multiple  *)
(* lines, the line number is blanked on the synthetic ones. When lines or lists *)
(* of lines are processed in reverse it is because an indeterminate number of   *)
(* insertions (e.g. Unicode combining diacritics or extended hex output) may be *)
(* inserted after the current index.                                            *)
(*                                                                              *)
(* This might look like a bit of a palaver, but a standard memo might exhibit   *)
(* "interesting" behavior once the amount of text causes it to start scrolling  *)
(* so having an intermediate that can be inspected might be useful.             *)

  buffer := TStringList.Create;
  try
    buffer.Text := AText;     (* Decides what line breaks it wants to swallow   *)
    if buffer.Count = 1 then
      i := 12345              (* Good place for a breakpoint                    *)
    else
      i := 67890;             (* Another good place for a breakpoint            *)
    case RadioGroupRight.ItemIndex of
      0: for i := 0 to buffer.Count - 1 do
           buffer[i] := widen(buffer[i]);
      1: for i := 0 to buffer.Count - 1 do
           buffer[i] := withControlPictures(buffer[i], CheckGroupRight.Checked[1]);
      2: for i := 0 to buffer.Count - 1 do
           buffer[i] := withIso2047(buffer[i], CheckGroupRight.Checked[1])
    otherwise
    end;
    for i := 0 to buffer.Count - 1 do begin             (* Line numbers         *)
      if numLength > 0 then
        buffer[i] := PadLeft(IntToStr(fFirstLine), numLength) + ' ' + buffer[i];
      fFirstLine += 1
    end;
    if RadioGroupRight.ItemIndex = 3 then (* Expand hex line-by-line in reverse *)
      for i := buffer.Count - 1 downto 0 do
        expandAsHex(buffer, i, numLength);

(* Add the buffered text to the visible control(s), and clean up.               *)

    Memo1.Lines.AddStrings(buffer)
  finally
    buffer.Free;
    Memo1.Lines.EndUpdate
  end;
  Memo1.SelStart := length(Memo1.Text)
end { TPseudoConsoleDlg.AddOutput } ;


procedure TPseudoConsoleDlg.Clear;

begin
  //DebugLn(DBG_VERBOSE, ['TPseudoConsoleDlg.Clear Calling FormResize']);
  Memo1.Lines.BeginUpdate;
  try
    FormResize(nil);                    (* Safe during IDE initialisation       *)
    Memo1.Text := ''
  finally
    Memo1.Lines.EndUpdate;
  end;
  fFirstLine := 1
end { TPseudoConsoleDlg.Clear } ;


{$R *.lfm}

initialization
  DBG_VERBOSE := DebugLogger.FindOrRegisterLogGroup('DBG_VERBOSE' {$IFDEF DBG_VERBOSE} , True {$ENDIF} );
  DBG_WARNINGS := DebugLogger.FindOrRegisterLogGroup('DBG_WARNINGS' {$IFDEF DBG_WARNINGS} , True {$ENDIF} );

  PseudoTerminalDlgWindowCreator := IDEWindowCreators.Add(DebugDialogNames[ddtPseudoTerminal]);
  PseudoTerminalDlgWindowCreator.OnCreateFormProc := @CreateDebugDialog;
  PseudoTerminalDlgWindowCreator.CreateSimpleLayout;

end.

