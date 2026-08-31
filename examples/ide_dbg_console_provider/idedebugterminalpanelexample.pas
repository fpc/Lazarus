{
  debugterminalpanel.pas
  ----------------------
  TDebugTerminalForm -- a dockable in-IDE terminal window built on SynEdit.

  A single ReadOnly TSynEdit serves as both output and input, character at a
  time: firmware/program output is appended via AddOutput; typed keystrokes are
  reported through the OnSendData event (the provider forwards them to the
  debuggee's stdin). The window is a persistent, Application-owned singleton
  registered with IDEWindowCreators, so it keeps its dock site across debug runs
  like the built-in debug windows.

  Adapted from the WCHDongle RISC-V embedded console panel, with the target
  transport removed: output is pushed in (not pulled from a channel by a reader
  thread) and input rides an event, so the widget has no project coupling.

  Display formatting is limited to three received control bytes (BS/DEL ->
  destructive backspace, FF -> clear); anything richer (ANSI/VT) is a future
  full-terminal concern.

  SPDX-License-Identifier: MIT
}
unit IdeDebugTerminalPanelExample;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  LCLType, Forms, Menus, SynEdit,
  // local
  IdeDebugTerminalOptionsExample;

type
  { Fired when the user types (or pastes) in the terminal. AText is the raw
    bytes to hand to the debuggee's stdin. }
  TDebugTerminalSendEvent = procedure(Sender: TObject; const AText: string) of object;

  { TDebugTerminalForm -- the terminal window (built in code, no .lfm).
    Persistent singleton (see GDebugTerminalForm); a provider attaches/detaches
    the event and applies options around it. }
  TDebugTerminalForm = class(TForm)
  private
    FOutput:   TSynEdit;
    FOptions:  TDebugTerminalDisplayOptions;
    FOnSend:   TDebugTerminalSendEvent;   { keystrokes -> debuggee stdin }
    FMenu:     TPopupMenu;
    FMiCopy:   TMenuItem;
    FMiPaste:  TMenuItem;
    procedure SendData(const S: string);
    procedure EchoLocal(const S: string);
    procedure EchoBackspace;
    procedure PasteToTarget;
    procedure ApplyColors;
    procedure SetOptions(const AValue: TDebugTerminalDisplayOptions);
    procedure BuildPopupMenu;
    procedure MenuPopup(Sender: TObject);
    procedure MenuCopyClick(Sender: TObject);
    procedure MenuSelectAllClick(Sender: TObject);
    procedure MenuClearClick(Sender: TObject);
    procedure MenuPasteClick(Sender: TObject);
    procedure OutputKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OutputUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure FormCloseHandler(Sender: TObject; var CloseAction: TCloseAction);
  public
    constructor CreateConsole(AOwner: TComponent; const ACaption: string);
    destructor  Destroy; override;
    { Append S (may contain line breaks / BS / DEL / FF) at the end and scroll to
      the bottom. Main-thread only. }
    procedure AppendText(const S: string);
    { Wipe the display. Main-thread only. }
    procedure ClearDisplay;
    { Raised when the user types; the provider forwards AText to stdin. }
    property OnSendData: TDebugTerminalSendEvent read FOnSend write FOnSend;
    { Presentation options (line-ending, local echo, backspace byte, colours).
      Assigning re-applies the colours to the editor. }
    property Options: TDebugTerminalDisplayOptions read FOptions write SetOptions;
  end;

{ Return the terminal window, creating it on first use (main thread). }
function EnsureDebugTerminalForm: TDebugTerminalForm;
{ The current window, or nil if not created yet. }
function DebugTerminalForm: TDebugTerminalForm;
{ Show/raise the window through the IDE dock master (plain Show as a fallback). }
procedure ShowDebugTerminalForm;

(* Mark the window as receiving the debuggee's output, or not.

   A deselected plug-in keeps its window: destroying one loses its place.
   TSimpleWindowLayout only reads coordinates from its OnClose handler and
   merely drops the reference on destruction, and AnchorDocking records a
   restore layout when a site is closed rather than when a control disappears
   from under it. Neither is reachable from here without knowing which dock
   master is installed.

   So the window stays where the user put it and says what it is instead. It
   also keeps the previous run's output readable, which is worth having in its
   own right. *)
procedure SetDebugTerminalActive(AnActive: Boolean);

implementation

uses
  Types,
  Controls, Graphics, Clipbrd,
  IDEWindowIntf;   { IDEWindowCreators -- registers the window for IDE docking }

const
  MAX_LINES = 5000;   { soft scrollback cap -- oldest lines drop past this }
  { Stable IDE window / layout identity (a valid Pascal identifier). The dock
    master keys the remembered site on this name. }
  CONSOLE_FORM_NAME = 'IDEDebugTerminal';
  CONSOLE_CAPTION = 'Debug Terminal';
  CONSOLE_CAPTION_INACTIVE = ' (inactive)';

var
  { The window, once created. Owned by Application; the destructor nils this.
    It is never destroyed on deselection -- see SetDebugTerminalActive. }
  GConsoleForm: TDebugTerminalForm = nil;

function EnsureDebugTerminalForm: TDebugTerminalForm;
begin
  if GConsoleForm = nil then
    GConsoleForm := TDebugTerminalForm.CreateConsole(Application, CONSOLE_CAPTION);
  Result := GConsoleForm;
end;

procedure SetDebugTerminalActive(AnActive: Boolean);
begin
  if GConsoleForm = nil then
    exit;
  if AnActive then
    GConsoleForm.Caption := CONSOLE_CAPTION
  else
    GConsoleForm.Caption := CONSOLE_CAPTION + CONSOLE_CAPTION_INACTIVE;
end;

function DebugTerminalForm: TDebugTerminalForm;
begin
  Result := GConsoleForm;
end;

procedure ShowDebugTerminalForm;
var
  F: TDebugTerminalForm;
begin
  F := EnsureDebugTerminalForm;
  if F = nil then
    exit;
  if IDEWindowCreators <> nil then
    IDEWindowCreators.ShowForm(F, True)
  else
    F.Show;
end;

{ TIDEWindowCreator callback. Creates the window so the dock master can
  restore it into its saved site and the Window menu can open it. }
procedure CreateConsoleIDEWindow(Sender: TObject; aFormName: string;
  var AForm: TCustomForm; DoDisableAutoSizing: boolean);
begin
  AForm := EnsureDebugTerminalForm;
  if DoDisableAutoSizing then
    AForm.DisableAutoSizing;
end;

{ ================================================================= }
{  TDebugTerminalForm                                               }
{ ================================================================= }

constructor TDebugTerminalForm.CreateConsole(AOwner: TComponent;
  const ACaption: string);
begin
  inherited CreateNew(AOwner);   { built in code -- no .lfm resource }
  Name        := CONSOLE_FORM_NAME;   { required for docking / ShowForm }
  Caption     := ACaption;
  Width       := 720;
  Height      := 420;
  Position    := poScreenCenter;
  OnClose     := @FormCloseHandler;

  { Sensible colour defaults until Options is assigned. }
  FOptions.BackgroundColor := clBlack;
  FOptions.ForegroundColor := clWhite;

  FOutput := TSynEdit.Create(Self);
  FOutput.Parent         := Self;
  FOutput.Align          := alClient;
  FOutput.ReadOnly       := True;   { never self-edits; input is driven by hand }
  FOutput.WantTabs       := True;   { Tab reaches OnKeyDown, not focus traversal }
  FOutput.Gutter.Visible := False;
  FOutput.Font.Name      := 'Consolas';   { Courier New is the universal fallback }
  FOutput.Font.Size      := 10;
  FOutput.Font.Style     := [fsBold];
  FOutput.OnKeyDown      := @OutputKeyDown;
  FOutput.OnUTF8KeyPress := @OutputUTF8KeyPress;

  BuildPopupMenu;
  FOutput.PopupMenu := FMenu;
  ApplyColors;
end;

destructor TDebugTerminalForm.Destroy;
begin
  if GConsoleForm = Self then
    GConsoleForm := nil;
  inherited Destroy;
end;

procedure TDebugTerminalForm.BuildPopupMenu;

  function AddItem(const ACaption: string; AHandler: TNotifyEvent): TMenuItem;
  begin
    Result := TMenuItem.Create(FMenu);
    Result.Caption := ACaption;
    Result.OnClick := AHandler;
    FMenu.Items.Add(Result);
  end;

  procedure AddSeparator;
  var
    M: TMenuItem;
  begin
    M := TMenuItem.Create(FMenu);
    M.Caption := '-';
    FMenu.Items.Add(M);
  end;

begin
  FMenu := TPopupMenu.Create(Self);
  FMenu.OnPopup := @MenuPopup;
  FMiCopy := AddItem('Copy',             @MenuCopyClick);
  AddItem('Select All',                  @MenuSelectAllClick);
  AddSeparator;
  AddItem('Clear',                       @MenuClearClick);
  AddSeparator;
  { The editor is ReadOnly, so a real edit-paste is impossible -- this ships the
    clipboard to the target instead (same as Ctrl+V / Shift+Insert). }
  FMiPaste := AddItem('Paste to Target', @MenuPasteClick);
end;

procedure TDebugTerminalForm.MenuPopup(Sender: TObject);
begin
  FMiCopy.Enabled  := FOutput.SelAvail;
  FMiPaste.Enabled := Clipboard.HasFormat(CF_TEXT) and (Clipboard.AsText <> '');
end;

procedure TDebugTerminalForm.MenuCopyClick(Sender: TObject);
begin
  if FOutput.SelAvail then
    FOutput.CopyToClipboard;
end;

procedure TDebugTerminalForm.MenuSelectAllClick(Sender: TObject);
begin
  FOutput.SelectAll;
end;

procedure TDebugTerminalForm.MenuClearClick(Sender: TObject);
begin
  ClearDisplay;
end;

procedure TDebugTerminalForm.MenuPasteClick(Sender: TObject);
begin
  PasteToTarget;
end;

procedure TDebugTerminalForm.ApplyColors;
begin
  if FOutput = nil then
    Exit;
  FOutput.Color      := TColor(FOptions.BackgroundColor);
  FOutput.Font.Color := TColor(FOptions.ForegroundColor);
  FOutput.SelectedColor.Background := clHighlight;
  FOutput.SelectedColor.Foreground := clHighlightText;
end;

procedure TDebugTerminalForm.SetOptions(const AValue: TDebugTerminalDisplayOptions);
begin
  FOptions := AValue;
  ApplyColors;
end;

procedure TDebugTerminalForm.ClearDisplay;
begin
  FOutput.ReadOnly := False;
  try
    FOutput.Lines.Clear;
    FOutput.Lines.Add('');   { keep the Lines.Count >= 1 invariant AppendText needs }
  finally
    FOutput.ReadOnly := True;
  end;
  FOutput.CaretX := 1;
  FOutput.CaretY := 1;
end;

procedure TDebugTerminalForm.PasteToTarget;
begin
  if Clipboard.HasFormat(CF_TEXT) and (Clipboard.AsText <> '') then
  begin
    SendData(Clipboard.AsText);
    if FOptions.LocalEcho then
      EchoLocal(Clipboard.AsText);
  end;
end;

procedure TDebugTerminalForm.FormCloseHandler(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caHide;   { closing just hides the persistent window }
end;

procedure TDebugTerminalForm.SendData(const S: string);
begin
  if (S = '') or (not Assigned(FOnSend)) then
    Exit;
  FOnSend(Self, S);
end;

procedure TDebugTerminalForm.OutputKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  { Paste: Ctrl+V or Shift+Insert -> ship the clipboard to the target. }
  if (((Key = VK_V) and (ssCtrl in Shift)) or
      ((Key = VK_INSERT) and (ssShift in Shift))) then
  begin
    PasteToTarget;
    Key := 0;
    Exit;
  end;

  { Control keys handled and swallowed here so they never reach OnUTF8KeyPress.
    Everything else falls through (printable chars come via OnUTF8KeyPress;
    arrows/PageUp navigate the read-only scrollback). }
  case Key of
    VK_RETURN:
      begin
        SendData(LineEndingBytes(FOptions.LineEnding));
        if FOptions.LocalEcho then
          EchoLocal(#10);   { on-screen newline is always a visual LF }
        Key := 0;
      end;
    VK_BACK:
      begin
        SendData(BackspaceByte(FOptions.Backspace));
        if FOptions.LocalEcho then
          EchoBackspace;
        Key := 0;
      end;
    VK_TAB:
      begin
        SendData(#9);
        if FOptions.LocalEcho then
          EchoLocal(#9);
        Key := 0;
      end;
  end;
end;

procedure TDebugTerminalForm.OutputUTF8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
begin
  if UTF8Key = '' then
    Exit;
  { Enter / Backspace / Tab and the paste shortcuts were consumed in OnKeyDown,
    so anything here is text (incl. Ctrl-letter control codes a terminal passes
    through). }
  SendData(UTF8Key);
  if FOptions.LocalEcho then
    EchoLocal(UTF8Key);
  UTF8Key := '';
end;

procedure TDebugTerminalForm.EchoLocal(const S: string);
begin
  AppendText(S);
end;

procedure TDebugTerminalForm.EchoBackspace;
var
  Last: Integer;
  Ln:   string;
begin
  { Best-effort destructive backspace: drop the last char of the last line. }
  Last := FOutput.Lines.Count - 1;
  if Last < 0 then
    Exit;
  Ln := FOutput.Lines[Last];
  if Ln = '' then
    Exit;
  FOutput.ReadOnly := False;
  try
    FOutput.Lines[Last] := Copy(Ln, 1, Length(Ln) - 1);
  finally
    FOutput.ReadOnly := True;
  end;
  FOutput.CaretY := FOutput.Lines.Count;
  FOutput.CaretX := Length(FOutput.Lines[FOutput.Lines.Count - 1]) + 1;
  FOutput.EnsureCursorPosVisible;
end;

procedure TDebugTerminalForm.AppendText(const S: string);
var
  I, RunStart: Integer;

  { Insert S[AFrom..ATo] (a printable run) at the very end. TextBetweenPoints
    parses embedded CR/LF/CRLF into line breaks. Assumes ReadOnly cleared and
    Lines.Count >= 1. }
  procedure FlushRun(AFrom, ATo: Integer);
  var
    Run: string;
    P:   TPoint;
  begin
    if ATo < AFrom then
      Exit;
    Run := Copy(S, AFrom, ATo - AFrom + 1);
    if Run = '' then
      Exit;
    P := Point(Length(FOutput.Lines[FOutput.Lines.Count - 1]) + 1,
               FOutput.Lines.Count);
    FOutput.TextBetweenPoints[P, P] := Run;
  end;

  { Destructive backspace, line-local (a BS at column 1 is ignored). }
  procedure BackOne;
  var
    Idx: Integer;
    Ln:  string;
  begin
    Idx := FOutput.Lines.Count - 1;
    if Idx < 0 then
      Exit;
    Ln := FOutput.Lines[Idx];
    if Ln <> '' then
      FOutput.Lines[Idx] := Copy(Ln, 1, Length(Ln) - 1);
  end;

  { Clear the panel (received FF / Ctrl-L); re-seed one empty line. }
  procedure ClearScreen;
  begin
    FOutput.Lines.Clear;
    FOutput.Lines.Add('');
  end;

begin
  if S = '' then
    Exit;
  FOutput.ReadOnly := False;
  try
    { A freshly-created SynEdit reports Lines.Count = 0; seed one empty line so
      the first insert behaves like every later one (and self-heals on clear). }
    if FOutput.Lines.Count = 0 then
      FOutput.Lines.Add('');
    { Render printable runs verbatim; act on three control bytes rather than
      drawing them: BS $08 / DEL $7F -> destructive backspace, FF $0C -> clear. }
    RunStart := 1;
    for I := 1 to Length(S) do
      case S[I] of
        #8, #127:
          begin
            FlushRun(RunStart, I - 1);
            BackOne;
            RunStart := I + 1;
          end;
        #12:
          begin
            ClearScreen;
            RunStart := I + 1;
          end;
      end;
    FlushRun(RunStart, Length(S));
    while FOutput.Lines.Count > MAX_LINES do
      FOutput.Lines.Delete(0);
  finally
    FOutput.ReadOnly := True;
  end;
  FOutput.CaretY := FOutput.Lines.Count;
  FOutput.CaretX := Length(FOutput.Lines[FOutput.Lines.Count - 1]) + 1;
  FOutput.EnsureCursorPosVisible;
end;

initialization
  { Register the terminal as a dockable IDE window. Guarded because the unit is
    portable -- IDEWindowCreators is nil outside the IDE. }
  if IDEWindowCreators <> nil then
    IDEWindowCreators.Add(CONSOLE_FORM_NAME,
      @CreateConsoleIDEWindow, nil,
      '250', '250', '+720', '+420');

end.
