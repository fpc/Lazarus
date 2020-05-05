{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditKeyCmds.pas, released 2000-04-07.
The Original Code is based on the mwKeyCmds.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Brad Stowers.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id$

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit SynEditKeyCmds;

{$I synedit.inc}

interface

uses
  Classes, Menus, SysUtils, LCLIntf, LCLType;

const
  //****************************************************************************
  // NOTE!  If you add an editor command, you must also update the
  //    EditorCommandStrs constant array in implementation section below, or the
  //    command will not show up in the IDE.
  //****************************************************************************

  // "Editor Commands".  Key strokes are translated from a table into these
  // I used constants instead of a set so that additional commands could be
  // added in descendants (you can't extend a set)

  // There are two ranges of editor commands: the ecViewXXX commands are always
  // valid, while the ecEditXXX commands are ignored when the editor is in
  // read-only mode

  ecNone             =    0; // Nothing. Useful for user event to handle command
  ecViewCommandFirst =    0;
  ecViewCommandLast  =  500;
  ecEditCommandFirst =  501;
  ecEditCommandLast  = 1000;

  ecLeft            = 1;    // Move cursor left one char
  ecRight           = 2;    // Move cursor right one char
  ecUp              = 3;    // Move cursor up one line
  ecDown            = 4;    // Move cursor down one line
  ecWordLeft        = 5;    // Move cursor left one word
  ecWordRight       = 6;    // Move cursor right one word
  ecLineStart       = 7;    // Move cursor to beginning of line (smart home)
  ecLineEnd         = 8;    // Move cursor to end of line
  ecPageUp          = 9;    // Move cursor up one page
  ecPageDown        = 10;   // Move cursor down one page
  ecPageLeft        = 11;   // Move cursor right one page
  ecPageRight       = 12;   // Move cursor left one page
  ecPageTop         = 13;   // Move cursor to top of page
  ecPageBottom      = 14;   // Move cursor to bottom of page
  ecEditorTop       = 15;   // Move cursor to absolute beginning
  ecEditorBottom    = 16;   // Move cursor to absolute end
  ecGotoXY          = 17;   // Move cursor to specific coordinates, Data = PPoint
  ecLineTextStart   = 18;   // Move cursor to the first none whitespace in the line
  ecWordEndLeft     = 19;   // Move cursor left one word (to end of word)
  ecWordEndRight    = 20;   // Move cursor right one word (to end of word)
  ecHalfWordLeft    = 21;   // Move cursor left to word-begin/end or case change lower to uppper
  ecHalfWordRight   = 22;   // Move cursor right to word-begin/end or case change lower to uppper
  ecSmartWordLeft   = 23;   // Move cursor left one word with smart boundaries (either start of current word or end of previous word)
  ecSmartWordRight  = 24;   // Move cursor right one word with smart boundaries (either end of current word or start of next word)

  // Allow selecting with any movement
  ecStickySelection     = 95;
  ecStickySelectionCol  = 96;
  ecStickySelectionLine = 97;
  ecStickySelectionStop = 99;

  ecSelection       = 100;  // Add this to ecXXX command to get equivalent
                            // command, but with selection enabled. This is not
                            // a command itself.

  ecSelectionStart  = 100;  // The lowest  Selection Command
  ecSelectionEnd    = 199;  // The highest Selection Command

  // Same as commands above, except they affect selection, too
  ecSelLeft         = ecLeft + ecSelection;
  ecSelRight        = ecRight + ecSelection;
  ecSelUp           = ecUp + ecSelection;
  ecSelDown         = ecDown + ecSelection;
  ecSelWordLeft     = ecWordLeft + ecSelection;
  ecSelWordRight    = ecWordRight + ecSelection;
  ecSelLineStart    = ecLineStart + ecSelection;
  ecSelLineEnd      = ecLineEnd + ecSelection;
  ecSelPageUp       = ecPageUp + ecSelection;
  ecSelPageDown     = ecPageDown + ecSelection;
  ecSelPageLeft     = ecPageLeft + ecSelection;
  ecSelPageRight    = ecPageRight + ecSelection;
  ecSelPageTop      = ecPageTop + ecSelection;
  ecSelPageBottom   = ecPageBottom + ecSelection;
  ecSelEditorTop    = ecEditorTop + ecSelection;
  ecSelEditorBottom = ecEditorBottom + ecSelection;
  ecSelGotoXY       = ecGotoXY + ecSelection;  // Data = PPoint
  ecSelLineTextStart= ecLineTextStart + ecSelection;   // Move cursor to the first none whitespace in the line
  ecSelWordEndLeft  = ecWordEndLeft + ecSelection;
  ecSelWordEndRight = ecWordEndRight + ecSelection;
  ecSelHalfWordLeft = ecHalfWordLeft + ecSelection;
  ecSelHalfWordRight= ecHalfWordRight + ecSelection;
  ecSelSmartWordLeft= ecSmartWordLeft + ecSelection;
  ecSelSmartWordRight=ecSmartWordRight + ecSelection;

  ecSelCmdRangeStart = ecLeft + ecSelection;
  ecSelCmdRangeEnd   = ecLeft + ecSelection + 49;

  // Allow access to column mode selection
  ecColumnSelection = ecSelection+50;

  ecColSelLeft       = ecLeft + ecColumnSelection;
  ecColSelRight      = ecRight + ecColumnSelection;
  ecColSelUp         = ecUp + ecColumnSelection;
  ecColSelDown       = ecDown + ecColumnSelection;
  ecColSelWordLeft   = ecWordLeft + ecColumnSelection;
  ecColSelWordRight  = ecWordRight + ecColumnSelection;
  ecColSelLineStart  = ecLineStart + ecColumnSelection;
  ecColSelLineEnd    = ecLineEnd + ecColumnSelection;
  ecColSelPageUp     = ecPageUp + ecColumnSelection;
  ecColSelPageDown   = ecPageDown + ecColumnSelection;
  ecColSelPageLeft   = ecPageLeft + ecColumnSelection;
  ecColSelPageRight  = ecPageRight + ecColumnSelection;
  ecColSelPageTop    = ecPageTop + ecColumnSelection;
  ecColSelPageBottom = ecPageBottom + ecColumnSelection;
  ecColSelEditorTop    = ecEditorTop + ecColumnSelection;
  ecColSelEditorBottom = ecEditorBottom + ecColumnSelection;
  ecColSelLineTextStart= ecLineTextStart + ecColumnSelection;

  ecSelColCmdRangeStart = ecColumnSelection;
  ecSelColCmdRangeEnd   = ecColumnSelection + 48; // 1 less for ecSelectAll


  ecSelectAll       = 199;  // Select entire contents of editor, cursor to end

  ecCopy            = 201;  // Copy selection to clipboard

  ecScrollUp        = 211;  // Scroll up one line leaving cursor position unchanged.
  ecScrollDown      = 212;  // Scroll down one line leaving cursor position unchanged.
  ecScrollLeft      = 213;  // Scroll left one char leaving cursor position unchanged.
  ecScrollRight     = 214;  // Scroll right one char leaving cursor position unchanged.

  ecInsertMode      = 221;  // Set insert mode
  ecOverwriteMode   = 222;  // Set overwrite mode
  ecToggleMode      = 223;  // Toggle ins/ovr mode

  ecNormalSelect    = 231;  // Normal selection mode
  ecColumnSelect    = 232;  // Column selection mode
  ecLineSelect      = 233;  // Line selection mode

  // Persistent Block
  ecBlockSetBegin   = 235;
  ecBlockSetEnd     = 236;
  ecBlockToggleHide = 237;
  ecBlockHide       = 238;
  ecBlockShow       = 239;
  ecBlockMove       = 240;
  ecBlockCopy       = 241;
  ecBlockDelete     = 242;
  ecBlockGotoBegin  = 243;
  ecBlockGotoEnd    = 244;

  ecMatchBracket    = 250;  // Go to matching bracket

  ecGotoMarker0     = 301;  // Goto marker
  ecGotoMarker1     = 302;  // Goto marker
  ecGotoMarker2     = 303;  // Goto marker
  ecGotoMarker3     = 304;  // Goto marker
  ecGotoMarker4     = 305;  // Goto marker
  ecGotoMarker5     = 306;  // Goto marker
  ecGotoMarker6     = 307;  // Goto marker
  ecGotoMarker7     = 308;  // Goto marker
  ecGotoMarker8     = 309;  // Goto marker
  ecGotoMarker9     = 310;  // Goto marker
  ecSetMarker0      = 351;  // Set marker, Data = PPoint - X, Y Pos
  ecSetMarker1      = 352;  // Set marker, Data = PPoint - X, Y Pos
  ecSetMarker2      = 353;  // Set marker, Data = PPoint - X, Y Pos
  ecSetMarker3      = 354;  // Set marker, Data = PPoint - X, Y Pos
  ecSetMarker4      = 355;  // Set marker, Data = PPoint - X, Y Pos
  ecSetMarker5      = 356;  // Set marker, Data = PPoint - X, Y Pos
  ecSetMarker6      = 357;  // Set marker, Data = PPoint - X, Y Pos
  ecSetMarker7      = 358;  // Set marker, Data = PPoint - X, Y Pos
  ecSetMarker8      = 359;  // Set marker, Data = PPoint - X, Y Pos
  ecSetMarker9      = 360;  // Set marker, Data = PPoint - X, Y Pos
  ecToggleMarker0   = 361;  // If marker is in the lie, remove marker, lese set marker, Data = PPoint - X, Y Pos
  ecToggleMarker1   = 362;
  ecToggleMarker2   = 363;
  ecToggleMarker3   = 364;
  ecToggleMarker4   = 365;
  ecToggleMarker5   = 366;
  ecToggleMarker6   = 367;
  ecToggleMarker7   = 368;
  ecToggleMarker8   = 369;
  ecToggleMarker9   = 370;

  EcFoldLevel1             = 371;  // fold all folds, greater/equal than nesting level 1
  EcFoldLevel2             = EcFoldLevel1 + 1;
  EcFoldLevel3             = EcFoldLevel2 + 1;
  EcFoldLevel4             = EcFoldLevel3 + 1;
  EcFoldLevel5             = EcFoldLevel4 + 1;
  EcFoldLevel6             = EcFoldLevel5 + 1;
  EcFoldLevel7             = EcFoldLevel6 + 1;
  EcFoldLevel8             = EcFoldLevel7 + 1;
  EcFoldLevel9             = EcFoldLevel8 + 1;
  EcFoldLevel0             = EcFoldLevel9 + 1;
  EcFoldCurrent            = 381;
  EcUnFoldCurrent          = 382;
  EcToggleMarkupWord       = 383;

  ecZoomOut             = 400;
  ecZoomIn              = 401;
  ecZoomNorm            = 402;

  ecDeleteLastChar  = 501;  // Delete last char (i.e. backspace key)
  ecDeleteChar      = 502;  // Delete char at cursor (i.e. delete key)
  ecDeleteWord      = 503;  // Delete from cursor to end of word
  ecDeleteLastWord  = 504;  // Delete from cursor to start of word
  ecDeleteBOL       = 505;  // Delete from cursor to beginning of line
  ecDeleteEOL       = 506;  // Delete from cursor to end of line
  ecDeleteLine      = 507;  // Delete current line
  ecClearAll        = 508;  // Delete everything
  ecLineBreak       = 509;  // Break line at current position, move caret to new line
  ecInsertLine      = 510;  // Break line at current position, leave caret
  ecChar            = 511;  // Insert a character at current position
  ecSmartUnindent   = 512;  // NOT regocniced as command, used for group-undo, set by beautifier
  ecDeleteCharNoCrLf= 513;  // Delete char at cursor (i.e. delete key), but do not join lines

  ecImeStr          = 550;  // Insert character(s) from IME

  ecUndo            = 601;  // Perform undo if available
  ecRedo            = 602;  // Perform redo if available
  ecCut             = 603;  // Cut selection to clipboard
  ecPaste           = 604;  // Paste clipboard to current position

  ecCopyAdd            = 605; // add selection to existing clipboard (space separated, unless whitespace exists)
  ecCutAdd             = 606; //
  ecCopyCurrentLine    = 607; // copy current line (or all covered at least partly by selection) to clipboard
  ecCopyAddCurrentLine = 608; // add to existing clipboard
  ecCutCurrentLine     = 609; //
  ecCutAddCurrentLine  = 610; //

  ecPasteAsColumns  = 611;  // Paste clipboard to current position as if it was column mode

  ecBlockIndent     = 615;  // Indent selection
  ecBlockUnindent   = 616;  // Unindent selection
  ecTab             = 617;  // Tab key
  ecShiftTab        = 618;  // Shift+Tab key

  ecUpperCase       = 620; // apply to the current or previous word
  ecLowerCase       = 621;
  ecToggleCase      = 622;
  ecTitleCase       = 623;
  ecUpperCaseBlock  = 625; // apply to current selection, or current char if no selection
  ecLowerCaseBlock  = 626;
  ecToggleCaseBlock = 627;

  ecMoveLineUp      = 630; // Moves current line (or selection) one line up
  ecMoveLineDown    = 631; // Moves current line (or selection) one line down
  ecDuplicateLine   = 632; // Line or selection (full lines)

  ecMoveSelectUp      = 633; // Moves selection one line up
  ecMoveSelectDown    = 634; // Moves selection one line down
  ecMoveSelectLeft    = 635; // Moves selection one line up
  ecMoveSelectRight   = 636; // Moves selection one line down
  ecDuplicateSelection= 637;


  ecString          = 640;  //Insert a whole string

  ecAutoCompletion  = 650;

  ecGotFocus        = 700;
  ecLostFocus       = 701;

  ecUserDefinedFirst  = 900;
  ecUserDefinedLast   = 999;

  ecUserFirst       = 1001; // Start of user-defined commands

  ecPluginFirstCompletion = 19000;
  ecPluginFirstSyncro     = 19010;
  ecPluginFirstTemplEdit  = 19030;
  ecPluginFirstMultiCaret = 19050;

  ecPluginFirst = 20000;

// Plugins don't know of other plugins, so they need to map the codes
// In order to save Keymaps, Plugins all start at ecPluginFirst (overlapping)
// If ask by SynEdit they add an offset

// Return the next offset
function AllocatePluginKeyRange(Count: Integer; OffsetOnly: Boolean = False): integer;

type
  ESynKeyError = class(Exception);

  TSynEditorCommand = type word;

  { TSynEditKeyStroke }

  TSynEditKeyStroke = class(TCollectionItem)
  private
    FKey: word;          // Virtual keycode, i.e. VK_xxx
    FShift: TShiftState;
    FKey2: word;
    FShift2: TShiftState;
    FCommand: TSynEditorCommand;
    FShiftMask: TShiftState;
    FShiftMask2: TShiftState;
    function GetCommand: TSynEditorCommand;
    function GetShortCut: TShortCut;
    function GetShortCut2: TShortCut;
    procedure SetCommand(Value: TSynEditorCommand);
    procedure SetKey(const Value: word);
    procedure SetKey2(const Value: word);
    procedure SetShift(const Value: TShiftState);
    procedure SetShift2(const Value: TShiftState);
    procedure SetShortCut(const Value: TShortCut);
    procedure SetShortCut2(const Value: TShortCut);
  protected
    function GetDisplayName: string; override;
  public
    procedure Assign(Source: TPersistent); override;
    // TODO: add ShiftMask optional
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
    // No duplicate checking is done if assignment made via these properties!
    property Key: word read FKey write SetKey;
    property Key2: word read FKey2 write SetKey2;
    property Shift: TShiftState read FShift write SetShift;
    property Shift2: TShiftState read FShift2 write SetShift2;
    // Modifier keys, that should be ignored
    property ShiftMask: TShiftState read FShiftMask write FShiftMask;
    property ShiftMask2: TShiftState read FShiftMask2 write FShiftMask2;
  published
    property Command: TSynEditorCommand read GetCommand write SetCommand;
    property ShortCut: TShortCut read GetShortCut write SetShortCut default 0;
    property ShortCut2: TShortCut read GetShortCut2 write SetShortCut2 default 0;
  end;

  { TSynEditKeyStrokes }

  TSynEditKeyStrokes = class(TCollection)
  private
    FOwner: TPersistent;
    fLastKey: word;
    fLastShiftState: TShiftState;
    FPluginOffset: Integer;
    FUsePluginOffset: Boolean;
    function GetItem(Index: Integer): TSynEditKeyStroke;
    procedure SetItem(Index: Integer; Value: TSynEditKeyStroke);
  protected
    function GetOwner: TPersistent; override;
    function FindKeycode2(Code1: word; SS1: TShiftState;
      Code2: word; SS2: TShiftState): integer;
    function FindKeycode2Start(Code: word; SS: TShiftState): integer;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TSynEditKeyStroke;
    procedure Assign(Source: TPersistent); override;
    function FindCommand(Cmd: TSynEditorCommand): integer;
    function FindKeycode(Code: word; SS: TShiftState): integer;
    function FindKeycodeEx(Code: word; SS: TShiftState; var Data: pointer;
                           out IsStartOfCombo: boolean;
                           FinishComboOnly: Boolean = False;
                           ComboStart: TSynEditKeyStrokes = nil): TSynEditorCommand;
    procedure ResetKeyCombo;
    function FindShortcut(SC: TShortcut): integer;
    function FindShortcut2(SC, SC2: TShortcut): integer;
    procedure LoadFromStream(AStream: TStream);
    procedure ResetDefaults; virtual;
    procedure SaveToStream(AStream: TStream);
  public
    property Items[Index: Integer]: TSynEditKeyStroke read GetItem
      write SetItem; default;
    property PluginOffset: Integer read FPluginOffset write FPluginOffset;
    // only switch on while needed.
    // So streaming will always see the constant, unmodified values
    property UsePluginOffset: Boolean read FUsePluginOffset write FUsePluginOffset;
  end;

  TGetEditorCommandValuesProc = procedure(Proc: TGetStrProc);

// These are mainly for the TSynEditorCommand property editor, but could be
// useful elsewhere.
function EditorCommandToDescrString(Cmd: TSynEditorCommand): string;
function EditorCommandToCodeString(Cmd: TSynEditorCommand): string;
procedure GetEditorCommandValues(Proc: TGetStrProc);
function IdentToEditorCommand(const Ident: string; var Cmd: longint): boolean;
function EditorCommandToIdent(Cmd: longint; var Ident: string): boolean;

procedure RegisterKeyCmdIdentProcs(IdentToIntFn: TIdentToInt; IntToIdentFn: TIntToIdent);
procedure RegisterExtraGetEditorCommandValues(AProc: TGetEditorCommandValuesProc);

implementation

// FOR LAZARUS
uses
  SynEditStrConst;

//=============================================================================
// This code should move to the menus.pas
type
  TMenuKeyCap = (
    mkcBkSp, mkcTab, mkcEsc, mkcEnter, mkcSpace, mkcPgUp,
    mkcPgDn, mkcEnd, mkcHome, mkcLeft, mkcUp, mkcRight, mkcDown, 
    mkcIns, mkcDel, mkcShift, mkcCtrl, mkcAlt);

// this code should be moved to whereever mkcXXX are defined
const
  SmkcBkSp = 'BkSp';
  SmkcTab = 'Tab';
  SmkcEsc = 'Esc';
  SmkcEnter = 'Enter';
  SmkcSpace = 'Space';
  SmkcPgUp = 'PgUp';
  SmkcPgDn = 'PgDn';
  SmkcEnd = 'End';
  SmkcHome = 'Home';
  SmkcLeft = 'Left';
  SmkcUp = 'Up';
  SmkcRight = 'Right';
  SmkcDown = 'Down';
  SmkcIns = 'Ins';
  SmkcDel = 'Del';
  SmkcShift = 'Shift+';
  SmkcCtrl = 'Ctrl+';
  SmkcAlt = 'Alt+';

// this code should be moved to menus.pas
  MenuKeyCaps: array[TMenuKeyCap] of ansistring = (
    SmkcBkSp, SmkcTab, SmkcEsc, SmkcEnter, SmkcSpace, SmkcPgUp,
    SmkcPgDn, SmkcEnd, SmkcHome, SmkcLeft, SmkcUp, SmkcRight, SmkcDown,
    SmkcIns, SmkcDel, SmkcShift, SmkcCtrl, SmkcAlt);

function GetSpecialName(ShortCut: TShortCut): string;
// FOR LAZARUS: ToDo
{
var
  ScanCode: Integer;
  KeyName: array[0..255] of Char;
}
begin
  Result := '';
// FOR LAZARUS: ToDo
{
  ScanCode := MapVirtualKey(WordRec(ShortCut).Lo, 0) shl 16;
  if ScanCode <> 0 then
  begin
    GetKeyNameText(ScanCode, KeyName, SizeOf(KeyName));
    GetSpecialName := KeyName;
  end; }
end;

function ShortCutToText(ShortCut: TShortCut): string;
var
  Name: string;
begin
  case WordRec(ShortCut).Lo of
    $08, $09:
      Name := MenuKeyCaps[TMenuKeyCap(Ord(mkcBkSp) + WordRec(ShortCut).Lo - $08)];
    $0D: Name := MenuKeyCaps[mkcEnter];
    $1B: Name := MenuKeyCaps[mkcEsc];
    $20..$28:
      Name := MenuKeyCaps[TMenuKeyCap(Ord(mkcSpace) + WordRec(ShortCut).Lo - $20)];
    $2D..$2E:
      Name := MenuKeyCaps[TMenuKeyCap(Ord(mkcIns) + WordRec(ShortCut).Lo - $2D)];
    $30..$39: Name := Chr(WordRec(ShortCut).Lo - $30 + Ord('0'));
    $41..$5A: Name := Chr(WordRec(ShortCut).Lo - $41 + Ord('A'));
    $60..$69: Name := Chr(WordRec(ShortCut).Lo - $60 + Ord('0'));
    $70..$87: Name := 'F' + IntToStr(WordRec(ShortCut).Lo - $6F);
  else
    Name := GetSpecialName(ShortCut);
  end;
  if Name <> '' then
  begin
    Result := '';
    if ShortCut and scShift <> 0 then Result := Result + MenuKeyCaps[mkcShift];
    if ShortCut and scCtrl <> 0 then Result := Result + MenuKeyCaps[mkcCtrl];
    if ShortCut and scAlt <> 0 then Result := Result + MenuKeyCaps[mkcAlt];
    Result := Result + Name;
  end
  else Result := '';
end;

//=============================================================================


{ Command mapping routines }

const
  EditorCommandStrs: array[0..171] of TIdentMapEntry = (
    (Value: ecNone; Name: 'ecNone'),
    (Value: ecLeft; Name: 'ecLeft'),
    (Value: ecRight; Name: 'ecRight'),
    (Value: ecUp; Name: 'ecUp'),
    (Value: ecDown; Name: 'ecDown'),
    (Value: ecWordLeft; Name: 'ecWordLeft'),
    (Value: ecWordRight; Name: 'ecWordRight'),
    (Value: ecWordEndLeft; Name: 'ecWordEndLeft'),
    (Value: ecWordEndRight; Name: 'ecWordEndRight'),
    (Value: ecHalfWordLeft; Name: 'ecHalfWordLeft'),
    (Value: ecHalfWordRight; Name: 'ecHalfWordRight'),
    (Value: ecSmartWordLeft; Name: 'ecSmartWordLeft'),
    (Value: ecSmartWordRight; Name: 'ecSmartWordRight'),
    (Value: ecLineStart; Name: 'ecLineStart'),
    (Value: ecLineEnd; Name: 'ecLineEnd'),
    (Value: ecPageUp; Name: 'ecPageUp'),
    (Value: ecPageDown; Name: 'ecPageDown'),
    (Value: ecPageLeft; Name: 'ecPageLeft'),
    (Value: ecPageRight; Name: 'ecPageRight'),
    (Value: ecPageTop; Name: 'ecPageTop'),
    (Value: ecPageBottom; Name: 'ecPageBottom'),
    (Value: ecEditorTop; Name: 'ecEditorTop'),
    (Value: ecEditorBottom; Name: 'ecEditorBottom'),
    (Value: ecGotoXY; Name: 'ecGotoXY'),
    (Value: ecLineTextStart; Name: 'ecLineTextStart'),
    (Value: ecStickySelection; Name: 'ecStickySelection'),
    (Value: ecStickySelectionCol; Name: 'ecStickySelectionCol'),
    (Value: ecStickySelectionLine; Name: 'ecStickySelectionLine'),
    (Value: ecStickySelectionStop; Name: 'ecStickySelectionStop'),
    (Value: ecSelLeft; Name: 'ecSelLeft'),
    (Value: ecSelRight; Name: 'ecSelRight'),
    (Value: ecSelUp; Name: 'ecSelUp'),
    (Value: ecSelDown; Name: 'ecSelDown'),
    (Value: ecSelWordLeft; Name: 'ecSelWordLeft'),
    (Value: ecSelWordRight; Name: 'ecSelWordRight'),
    (Value: ecSelWordEndLeft; Name: 'ecSelWordEndLeft'),
    (Value: ecSelWordEndRight; Name: 'ecSelWordEndRight'),
    (Value: ecSelHalfWordLeft; Name: 'ecSelHalfWordLeft'),
    (Value: ecSelHalfWordRight; Name: 'ecSelHalfWordRight'),
    (Value: ecSelSmartWordLeft; Name: 'ecSelSmartWordLeft'),
    (Value: ecSelSmartWordRight; Name: 'ecSelSmartWordRight'),
    (Value: ecSelLineStart; Name: 'ecSelLineStart'),
    (Value: ecSelLineEnd; Name: 'ecSelLineEnd'),
    (Value: ecSelPageUp; Name: 'ecSelPageUp'),
    (Value: ecSelPageDown; Name: 'ecSelPageDown'),
    (Value: ecSelPageLeft; Name: 'ecSelPageLeft'),
    (Value: ecSelPageRight; Name: 'ecSelPageRight'),
    (Value: ecSelPageTop; Name: 'ecSelPageTop'),
    (Value: ecSelPageBottom; Name: 'ecSelPageBottom'),
    (Value: ecSelEditorTop; Name: 'ecSelEditorTop'),
    (Value: ecSelEditorBottom; Name: 'ecSelEditorBottom'),
    (Value: ecSelGotoXY; Name: 'ecSelGotoXY'),
    (Value: ecSelLineTextStart; Name: 'ecSelLineTextStart'),
    (Value: ecColSelLeft; Name: 'ecColSelLeft'),
    (Value: ecColSelRight; Name: 'ecColSelRight'),
    (Value: ecColSelUp; Name: 'ecColSelUp'),
    (Value: ecColSelDown; Name: 'ecColSelDown'),
    (Value: ecColSelWordLeft; Name: 'ecColSelWordLeft'),
    (Value: ecColSelWordRight; Name: 'ecColSelWordRight'),
    (Value: ecColSelLineStart; Name: 'ecColSelLineStart'),
    (Value: ecColSelLineEnd; Name: 'ecColSelLineEnd'),
    (Value: ecColSelPageUp; Name: 'ecColSelPageUp'),
    (Value: ecColSelPageDown; Name: 'ecColSelPageDown'),
    (Value: ecColSelPageLeft; Name: 'ecColSelPageLeft'),
    (Value: ecColSelPageRight; Name: 'ecColSelPageRight'),
    (Value: ecColSelPageTop; Name: 'ecColSelPageTop'),
    (Value: ecColSelPageBottom; Name: 'ecColSelPageBottom'),
    (Value: ecColSelEditorTop; Name: 'ecColSelEditorTop'),
    (Value: ecColSelEditorBottom; Name: 'ecColSelEditorBottom'),
    (Value: ecColSelLineTextStart; Name: 'ecColSelLineTextStart'),
    (Value: ecSelectAll; Name: 'ecSelectAll'),
    (Value: ecDeleteLastChar; Name: 'ecDeleteLastChar'),
    (Value: ecDeleteChar; Name: 'ecDeleteChar'),
    (Value: ecDeleteWord; Name: 'ecDeleteWord'),
    (Value: ecDeleteLastWord; Name: 'ecDeleteLastWord'),
    (Value: ecDeleteBOL; Name: 'ecDeleteBOL'),
    (Value: ecDeleteEOL; Name: 'ecDeleteEOL'),
    (Value: ecDeleteLine; Name: 'ecDeleteLine'),
    (Value: ecClearAll; Name: 'ecClearAll'),
    (Value: ecLineBreak; Name: 'ecLineBreak'),
    (Value: ecInsertLine; Name: 'ecInsertLine'),
    (Value: ecChar; Name: 'ecChar'),
    (Value: ecImeStr; Name: 'ecImeStr'),
    (Value: ecUndo; Name: 'ecUndo'),
    (Value: ecRedo; Name: 'ecRedo'),
    (Value: ecCut; Name: 'ecCut'),
    (Value: ecCopy; Name: 'ecCopy'),
    (Value: ecPaste; Name: 'ecPaste'),
    (Value: ecCopyAdd; Name: 'ecCopyAdd'),
    (Value: ecCutAdd; Name: 'ecCutAdd'),
    (Value: ecCopyCurrentLine; Name: 'ecCopyCurrentLine'),
    (Value: ecCopyAddCurrentLine; Name: 'ecCopyAddCurrentLine'),
    (Value: ecCutCurrentLine; Name: 'ecCutCurrentLine'),
    (Value: ecCutAddCurrentLine; Name: 'ecCutAddCurrentLine'),
    (Value: ecMoveLineUp; Name: 'ecMoveLineUp'),
    (Value: ecMoveLineDown; Name: 'ecMoveLineDown'),
    (Value: ecMoveSelectUp; Name: 'ecMoveSelectUp'),
    (Value: ecMoveSelectDown; Name: 'ecMoveSelectDown'),
    (Value: ecMoveSelectLeft; Name: 'ecMoveSelectLeft'),
    (Value: ecMoveSelectRight; Name: 'ecMoveSelectRight'),
    (Value: ecDuplicateLine; Name: 'ecDuplicateLine'),
    (Value: ecDuplicateSelection; Name: 'ecDuplicateSelection'),
    (Value: ecScrollUp; Name: 'ecScrollUp'),
    (Value: ecScrollDown; Name: 'ecScrollDown'),
    (Value: ecScrollLeft; Name: 'ecScrollLeft'),
    (Value: ecScrollRight; Name: 'ecScrollRight'),
    (Value: ecInsertMode; Name: 'ecInsertMode'),
    (Value: ecOverwriteMode; Name: 'ecOverwriteMode'),
    (Value: ecToggleMode; Name: 'ecToggleMode'),
    (Value: ecBlockIndent; Name: 'ecBlockIndent'),
    (Value: ecBlockUnindent; Name: 'ecBlockUnindent'),
    (Value: ecTab; Name: 'ecTab'),
    (Value: ecShiftTab; Name: 'ecShiftTab'),
    (Value: ecMatchBracket; Name: 'ecMatchBracket'),
    (Value: ecNormalSelect; Name: 'ecNormalSelect'),
    (Value: ecColumnSelect; Name: 'ecColumnSelect'),
    (Value: ecLineSelect; Name: 'ecLineSelect'),
    (Value: ecBlockSetBegin;   Name: 'ecBlockSetBegin'),
    (Value: ecBlockSetEnd;     Name: 'ecBlockSetEnd'),
    (Value: ecBlockToggleHide; Name: 'ecBlockToggleHide'),
    (Value: ecBlockHide;       Name: 'ecBlockHide'),
    (Value: ecBlockShow;       Name: 'ecBlockShow'),
    (Value: ecBlockMove;       Name: 'ecBlockMove'),
    (Value: ecBlockCopy;       Name: 'ecBlockCopy'),
    (Value: ecBlockDelete;     Name: 'ecBlockDelete'),
    (Value: ecBlockGotoBegin;  Name: 'ecBlockGotoBegin'),
    (Value: ecBlockGotoEnd;    Name: 'ecBlockGotoEnd'),
    (Value: ecAutoCompletion; Name: 'ecAutoCompletion'),
    (Value: ecUserFirst; Name: 'ecUserFirst'),
    (Value: ecGotoMarker0; Name: 'ecGotoMarker0'),
    (Value: ecGotoMarker1; Name: 'ecGotoMarker1'),
    (Value: ecGotoMarker2; Name: 'ecGotoMarker2'),
    (Value: ecGotoMarker3; Name: 'ecGotoMarker3'),
    (Value: ecGotoMarker4; Name: 'ecGotoMarker4'),
    (Value: ecGotoMarker5; Name: 'ecGotoMarker5'),
    (Value: ecGotoMarker6; Name: 'ecGotoMarker6'),
    (Value: ecGotoMarker7; Name: 'ecGotoMarker7'),
    (Value: ecGotoMarker8; Name: 'ecGotoMarker8'),
    (Value: ecGotoMarker9; Name: 'ecGotoMarker9'),
    (Value: ecSetMarker0; Name: 'ecSetMarker0'),
    (Value: ecSetMarker1; Name: 'ecSetMarker1'),
    (Value: ecSetMarker2; Name: 'ecSetMarker2'),
    (Value: ecSetMarker3; Name: 'ecSetMarker3'),
    (Value: ecSetMarker4; Name: 'ecSetMarker4'),
    (Value: ecSetMarker5; Name: 'ecSetMarker5'),
    (Value: ecSetMarker6; Name: 'ecSetMarker6'),
    (Value: ecSetMarker7; Name: 'ecSetMarker7'),
    (Value: ecSetMarker8; Name: 'ecSetMarker8'),
    (Value: ecSetMarker9; Name: 'ecSetMarker9'),
    (Value: ecToggleMarker0; Name: 'ecToggleMarker0'),
    (Value: ecToggleMarker1; Name: 'ecToggleMarker1'),
    (Value: ecToggleMarker2; Name: 'ecToggleMarker2'),
    (Value: ecToggleMarker3; Name: 'ecToggleMarker3'),
    (Value: ecToggleMarker4; Name: 'ecToggleMarker4'),
    (Value: ecToggleMarker5; Name: 'ecToggleMarker5'),
    (Value: ecToggleMarker6; Name: 'ecToggleMarker6'),
    (Value: ecToggleMarker7; Name: 'ecToggleMarker7'),
    (Value: ecToggleMarker8; Name: 'ecToggleMarker8'),
    (Value: ecToggleMarker9; Name: 'ecToggleMarker9'),
    (Value: EcFoldLevel1; Name: 'EcFoldLevel1'),
    (Value: EcFoldLevel2; Name: 'EcFoldLevel2'),
    (Value: EcFoldLevel3; Name: 'EcFoldLevel3'),
    (Value: EcFoldLevel4; Name: 'EcFoldLevel4'),
    (Value: EcFoldLevel5; Name: 'EcFoldLevel5'),
    (Value: EcFoldLevel6; Name: 'EcFoldLevel6'),
    (Value: EcFoldLevel7; Name: 'EcFoldLevel7'),
    (Value: EcFoldLevel8; Name: 'EcFoldLevel8'),
    (Value: EcFoldLevel9; Name: 'EcFoldLevel9'),
    (Value: EcFoldLevel0; Name: 'EcFoldLevel0'),
    (Value: EcFoldCurrent; Name: 'EcFoldCurrent'),
    (Value: EcUnFoldCurrent; Name: 'EcUnFoldCurrent'),
    (Value: EcToggleMarkupWord; Name: 'EcToggleMarkupWord')
  );

var
  ExtraIdentToIntFn: Array of TIdentToInt = nil;
  ExtraIntToIdentFn: Array of TIntToIdent = nil;
  ExtraGetEditorCommandValues: Array of TGetEditorCommandValuesProc = nil;

procedure GetEditorCommandValues(Proc: TGetStrProc);
var
  i: integer;
begin
  for i := 0 to 19 do
    Proc('ecUserDefined' + IntToStr(i));
  for i := Low(EditorCommandStrs) to High(EditorCommandStrs) do
    Proc(EditorCommandStrs[I].Name);
  i := 0;
  while (i < length(ExtraGetEditorCommandValues)) do begin
    ExtraGetEditorCommandValues[i](Proc);
    inc(i);
  end;
end;

function IdentToEditorCommand(const Ident: string; var Cmd: longint): boolean;
var
  i: Integer;
begin
  if (copy(Ident, 1 , 13) = 'ecUserDefined') then begin
    Cmd := StrToIntDef(copy(Ident, 14, length(Ident)), -1) + ecUserDefinedFirst;
    Result := (Cmd >= ecUserDefinedFirst) and (Cmd <= ecUserDefinedLast);
    if Result then
      exit;
  end;
  Result := IdentToInt(Ident, Cmd, EditorCommandStrs);
  i := 0;
  while (i < length(ExtraIdentToIntFn)) and (not Result) do begin
    Result := ExtraIdentToIntFn[i](Ident, Cmd);
    inc(i);
  end;
end;

function EditorCommandToIdent(Cmd: longint; var Ident: string): boolean;
var
  i: Integer;
begin
  if (Cmd >= ecUserDefinedFirst) and (Cmd <= ecUserDefinedLast) then begin
    Ident := 'ecUserDefined' + IntToStr(Cmd - ecUserDefinedFirst);
    Result := True;
    exit;
  end;
  Result := IntToIdent(Cmd, Ident, EditorCommandStrs);
  i := 0;
  while (i < length(ExtraIntToIdentFn)) and (not Result) do begin
    Result := ExtraIntToIdentFn[i](Cmd, Ident);
    inc(i);
  end;
end;

procedure RegisterKeyCmdIdentProcs(IdentToIntFn: TIdentToInt;
  IntToIdentFn: TIntToIdent);
var
  i: Integer;
begin
  i := length(ExtraIdentToIntFn);
  SetLength(ExtraIdentToIntFn, i + 1);
  ExtraIdentToIntFn[i] := IdentToIntFn;
  i := length(ExtraIntToIdentFn);
  SetLength(ExtraIntToIdentFn, i + 1);
  ExtraIntToIdentFn[i] := IntToIdentFn;
end;

procedure RegisterExtraGetEditorCommandValues(AProc: TGetEditorCommandValuesProc);
var
  i: Integer;
begin
  i := length(ExtraGetEditorCommandValues);
  SetLength(ExtraGetEditorCommandValues, i + 1);
  ExtraGetEditorCommandValues[i] := AProc;
end;


function AllocatePluginKeyRange(Count: Integer; OffsetOnly: Boolean): integer;
const
  CurOffset : integer = 0;
begin
  Result := CurOffset;
  inc(CurOffset, Count);
  if not OffsetOnly then
    inc(Result, ecPluginFirst);
end;

function EditorCommandToDescrString(Cmd: TSynEditorCommand): string;
begin
  // Doesn't do anything yet.
  Result := '';
end;

function EditorCommandToCodeString(Cmd: TSynEditorCommand): string;
begin
  Result := '';
  if not EditorCommandToIdent(Cmd, Result) then
    Result := IntToStr(Cmd);
end;

{ TSynEditKeyStroke }

procedure TSynEditKeyStroke.Assign(Source: TPersistent);
begin
  if Source is TSynEditKeyStroke then
  begin
    Command := TSynEditKeyStroke(Source).Command;
    Key        := TSynEditKeyStroke(Source).Key;
    Key2       := TSynEditKeyStroke(Source).Key2;
    Shift      := TSynEditKeyStroke(Source).Shift;
    Shift2     := TSynEditKeyStroke(Source).Shift2;
    ShiftMask  := TSynEditKeyStroke(Source).ShiftMask;
    ShiftMask2 := TSynEditKeyStroke(Source).ShiftMask2;
  end else
    inherited Assign(Source);
end;

function TSynEditKeyStroke.GetDisplayName: string;
begin
  Result := EditorCommandToCodeString(Command) + ' - ' + ShortCutToText(ShortCut);
  if ShortCut <> 0 then
    Result := Result + ' ' + ShortCutToText(ShortCut2);
  if Result = '' then
    Result := inherited GetDisplayName;
end;

function TSynEditKeyStroke.GetShortCut: TShortCut;
begin
  Result := Menus.ShortCut(Key, Shift);
end;

function TSynEditKeyStroke.GetCommand: TSynEditorCommand;
begin
  Result:= FCommand;
  if TSynEditKeyStrokes(Collection).UsePluginOffset and (Result >= ecPluginFirst) then
    Inc(Result, TSynEditKeyStrokes(Collection).PluginOffset);
end;

procedure TSynEditKeyStroke.SetCommand(Value: TSynEditorCommand);
begin
  if TSynEditKeyStrokes(Collection).UsePluginOffset and (Value >= ecPluginFirst) then
  begin
    Dec(Value, TSynEditKeyStrokes(Collection).PluginOffset);
    if (Value < ecPluginFirst) then
      Value := ecNone;
  end;
  if Value <> FCommand then
    FCommand := Value;
end;

procedure TSynEditKeyStroke.SetKey(const Value: word);
begin
  if Value <> FKey then
    FKey := Value;
end;

procedure TSynEditKeyStroke.SetShift(const Value: TShiftState);
begin
  if Value <> FShift then
    FShift := Value;
end;

procedure TSynEditKeyStroke.SetShortCut(const Value: TShortCut);
var
  NewKey: Word;
  NewShift: TShiftState;
  Dup: integer;
begin
  // Duplicate values of no shortcut are OK.
  if Value <> 0 then
  begin
    // Check for duplicate shortcut in the collection and disallow if there is.
    Dup := TSynEditKeyStrokes(Collection).FindShortcut2(Value, Key2);
    if (Dup <> -1) and (Dup <> Self.Index) then
      raise ESynKeyError.Create(SYNS_EDuplicateShortCut);
  end;

  Menus.ShortCutToKey(Value, NewKey, NewShift);
  ShiftMask := [];
  if (NewKey <> Key) or (NewShift <> Shift) then
  begin
    Key := NewKey;
    Shift := NewShift;
  end;
end;

procedure TSynEditKeyStroke.SetKey2(const Value: word);
begin
  if Value <> FKey2 then
    FKey2 := Value;
end;

procedure TSynEditKeyStroke.SetShift2(const Value: TShiftState);
begin
  if Value <> FShift2 then
    FShift2 := Value;
end;

procedure TSynEditKeyStroke.SetShortCut2(const Value: TShortCut);
var
  NewKey: Word;
  NewShift: TShiftState;
  Dup: integer;
begin
  // Duplicate values of no shortcut are OK.
  if Value <> 0 then
  begin
    // Check for duplicate shortcut in the collection and disallow if there is.
    Dup := TSynEditKeyStrokes(Collection).FindShortcut2(Key, Value);
    if (Dup <> -1) and (Dup <> Self.Index) then
      raise ESynKeyError.Create(SYNS_EDuplicateShortCut);
  end;

  Menus.ShortCutToKey(Value, NewKey, NewShift);
  ShiftMask2 := [];
  if (NewKey <> Key2) or (NewShift <> Shift2) then
  begin
    Key2 := NewKey;
    Shift2 := NewShift;
  end;
end;

function TSynEditKeyStroke.GetShortCut2: TShortCut;
begin
  Result := Menus.ShortCut(Key2, Shift2);
end;

{begin}                                                                         //ac 2000-07-05
procedure TSynEditKeyStroke.LoadFromStream(AStream: TStream);
begin
  with AStream do begin
    Read(fKey, SizeOf(fKey));
    Read(fShift, SizeOf(fShift));
    Read(fKey2, SizeOf(fKey2));
    Read(fShift2, SizeOf(fShift2));
    Read(fCommand, SizeOf(fCommand));
  end;
end;

procedure TSynEditKeyStroke.SaveToStream(AStream: TStream);
begin
  with AStream do begin
    Write(fKey, SizeOf(fKey));
    Write(fShift, SizeOf(fShift));
    Write(fKey2, SizeOf(fKey2));
    Write(fShift2, SizeOf(fShift2));
    Write(fCommand, SizeOf(fCommand));
  end;
end;
{end}                                                                           //ac 2000-07-05

{ TSynEditKeyStrokes }

function TSynEditKeyStrokes.Add: TSynEditKeyStroke;
begin
  Result := TSynEditKeyStroke(inherited Add);
end;

procedure TSynEditKeyStrokes.Assign(Source: TPersistent);
var
  x: integer;
begin
  if Source is TSynEditKeyStrokes then
  begin
    Clear;
    for x := 0 to TSynEditKeyStrokes(Source).Count-1 do
    begin
      with Add do
        Assign(TSynEditKeyStrokes(Source)[x]);
    end;
  end else
    inherited Assign(Source);
end;

constructor TSynEditKeyStrokes.Create(AOwner: TPersistent);
begin
  inherited Create(TSynEditKeyStroke);
  FOwner := AOwner;
  fLastKey := 0;
  fLastShiftState := [];
  FPluginOffset := 0;
  FUsePluginOffset := False;
end;

function TSynEditKeyStrokes.FindCommand(Cmd: TSynEditorCommand): integer;
var
  x: integer;
begin
  Result := -1;
  for x := 0 to Count-1 do
    if Items[x].Command = Cmd then
    begin
      Result := x;
      break;
    end;
end;

function TSynEditKeyStrokes.FindKeycode(Code: word; SS: TShiftState): integer;
var
  x: integer;
begin
  Result := -1;
  for x := 0 to Count-1 do
    if (Items[x].Key = Code) and (Items[x].Shift = SS-Items[x].ShiftMask)
    and (Items[x].Key2 = 0) then
    begin
      Result := x;
      break;
    end;
end;

function TSynEditKeyStrokes.FindKeycodeEx(Code: word; SS: TShiftState; var Data: pointer;
  out IsStartOfCombo: boolean;
  FinishComboOnly: Boolean; ComboStart: TSynEditKeyStrokes): TSynEditorCommand;
var
  i: integer;
  CurComboStart: TSynEditKeyStrokes;
begin
  (* if FinishComboOnly=True then ComboStart are the KeyStrokes, which have the
     already received keys.
     If several TSynEditKeyStrokes have combos, starting with the same key(s)
     only one needs to keep the info. The others chek, if they have a matching combo
  *)

  Result := ecNone;
  IsStartOfCombo := False;

  if ComboStart = nil then
    CurComboStart := self
  else
    CurComboStart := ComboStart;

  if CurComboStart.fLastKey <> 0 then begin
    // Try to finish the combo
    i := FindKeycode2(CurComboStart.fLastKey, CurComboStart.fLastShiftState, Code, SS);
    if (i >= 0) then begin
      Result := Items[i].Command;
      CurComboStart.ResetKeyCombo;
      exit;
    end;
  end;
  if FinishComboOnly then
    exit;

  // Check for single stroke
  i := FindKeycode(Code, SS);
  if i >= 0 then begin
    Result := Items[i].Command;
    ResetKeyCombo;
    if (ComboStart <> nil) and (ComboStart <> self) then
      ComboStart.ResetKeyCombo;
    exit;
  end;

  if (FindKeycode2Start(Code, SS) >= 0) and not FinishComboOnly then
  begin
    fLastKey := Code;
    fLastShiftState := SS;
    IsStartOfCombo := True;
    // Now this is the start of combo
    if (ComboStart <> nil) and (ComboStart <> self) then
      ComboStart.ResetKeyCombo;
  end
  else begin
    // Nothing was found.
    // Keep CurComboStart.fLastKey. It may be ued by another TSynEditKeyStrokes
    if (ComboStart <> self) then
      ResetKeyCombo; // reset self, if not CurComboStart
  end
end;

procedure TSynEditKeyStrokes.ResetKeyCombo;
begin
  fLastKey := 0;
  fLastShiftState := [];
end;

function TSynEditKeyStrokes.FindKeycode2(Code1: word; SS1: TShiftState;
  Code2: word; SS2: TShiftState): integer;
var
  x: integer;
begin
  Result := -1;
  for x := 0 to Count-1 do
    // If ShiftMask2 = [ssCtrl] and shortcut is "Ctrl-X, Y", then both
    // "Ctrl-X, Y" and "Ctrl-X, Ctrl-Y" are accepted. The second CTRL is ignored.
    with Items[x] do
      if (Key = Code1) and (Shift = SS1 - ShiftMask)
      and (Key2 = Code2) and (Shift2 = SS2 - ShiftMask2) then
      begin
        Result := x;
        break;
      end;
end;

function TSynEditKeyStrokes.FindKeycode2Start(Code: word; SS: TShiftState): integer;
var
  x: integer;
begin
  Result := -1;
  for x := 0 to Count-1 do
    if (Items[x].Key = Code) and (Items[x].Shift = SS - Items[x].ShiftMask) and
       (Items[x].Key2 <> VK_UNKNOWN) then
    begin
      Result := x;
      break;
    end;
end;

function TSynEditKeyStrokes.FindShortcut(SC: TShortcut): integer;
var
  x: integer;
begin
  Result := -1;
  for x := 0 to Count-1 do
    if Items[x].Shortcut = SC then
    begin
      Result := x;
      break;
    end;
end;

function TSynEditKeyStrokes.FindShortcut2(SC, SC2: TShortcut): integer;
var
  x: integer;
begin
  Result := -1;
  for x := 0 to Count-1 do
    if (Items[x].Shortcut = SC) and (Items[x].Shortcut2 = SC2) then
    begin
      Result := x;
      break;
    end;
end;

function TSynEditKeyStrokes.GetItem(Index: Integer): TSynEditKeyStroke;
begin
  Result := TSynEditKeyStroke(inherited GetItem(Index));
end;

function TSynEditKeyStrokes.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

{begin}                                                                         //ac 2000-07-05
procedure TSynEditKeyStrokes.LoadFromStream(AStream: TStream);
var
  Num: integer;
begin
  Num := 0;
  AStream.Read(Num, SizeOf(Num));
  while Num > 0 do begin
    with Add do
      LoadFromStream(AStream);
    Dec(Num);
  end;
end;
{end}                                                                           //ac 2000-07-05

procedure TSynEditKeyStrokes.ResetDefaults;

  procedure AddKey(const ACmd: TSynEditorCommand; const AKey: word;
     const AShift: TShiftState; const AShiftMask: TShiftState = []);
  begin
    with Add do
    begin
      Key       := AKey;
      Shift     := AShift;
      ShiftMask := AShiftMask;
      Command   := ACmd;
    end;
  end;

begin
  Clear;

  AddKey(ecUp, VK_UP, []);
  AddKey(ecSelUp, VK_UP, [ssShift]);
  AddKey(ecScrollUp, VK_UP, [ssCtrl]);
  AddKey(ecDown, VK_DOWN, []);
  AddKey(ecSelDown, VK_DOWN, [ssShift]);
  AddKey(ecScrollDown, VK_DOWN, [ssCtrl]);
  AddKey(ecLeft, VK_LEFT, []);
  AddKey(ecSelLeft, VK_LEFT, [ssShift]);
  AddKey(ecWordLeft, VK_LEFT, [ssCtrl]);
  AddKey(ecSelWordLeft, VK_LEFT, [ssShift,ssCtrl]);
  AddKey(ecRight, VK_RIGHT, []);
  AddKey(ecSelRight, VK_RIGHT, [ssShift]);
  AddKey(ecWordRight, VK_RIGHT, [ssCtrl]);
  AddKey(ecSelWordRight, VK_RIGHT, [ssShift,ssCtrl]);
  AddKey(ecPageDown, VK_NEXT, []);
  AddKey(ecSelPageDown, VK_NEXT, [ssShift]);
  AddKey(ecPageBottom, VK_NEXT, [ssCtrl]);
  AddKey(ecSelPageBottom, VK_NEXT, [ssShift,ssCtrl]);
  AddKey(ecPageUp, VK_PRIOR, []);
  AddKey(ecSelPageUp, VK_PRIOR, [ssShift]);
  AddKey(ecPageTop, VK_PRIOR, [ssCtrl]);
  AddKey(ecSelPageTop, VK_PRIOR, [ssShift,ssCtrl]);
  AddKey(ecLineStart, VK_HOME, []);
  AddKey(ecSelLineStart, VK_HOME, [ssShift]);
  AddKey(ecEditorTop, VK_HOME, [ssCtrl]);
  AddKey(ecSelEditorTop, VK_HOME, [ssShift,ssCtrl]);
  AddKey(ecLineEnd, VK_END, []);
  AddKey(ecSelLineEnd, VK_END, [ssShift]);
  AddKey(ecEditorBottom, VK_END, [ssCtrl]);
  AddKey(ecSelEditorBottom, VK_END, [ssShift,ssCtrl]);
  AddKey(ecToggleMode, VK_INSERT, []);
  AddKey(ecCopy, VK_INSERT, [ssCtrl]);
  AddKey(ecPaste, VK_INSERT, [ssShift]);
  AddKey(ecDeleteChar, VK_DELETE, []);
  AddKey(ecCut, VK_DELETE, [ssShift]);
  AddKey(ecDeleteLastChar, VK_BACK, []);
  AddKey(ecDeleteLastChar, VK_BACK, [ssShift]);                                 //jr 2000-09-23
  AddKey(ecDeleteLastWord, VK_BACK, [ssCtrl]);
  AddKey(ecUndo, VK_BACK, [ssAlt]);
  AddKey(ecRedo, VK_BACK, [ssAlt,ssShift]);
  AddKey(ecLineBreak, VK_RETURN, []);
  AddKey(ecSelectAll, ord('A'), [ssCtrl]);
  AddKey(ecCopy, ord('C'), [ssCtrl]);
  AddKey(ecBlockIndent, ord('I'), [ssCtrl,ssShift]);
  AddKey(ecLineBreak, ord('M'), [ssCtrl]);
  AddKey(ecInsertLine, ord('N'), [ssCtrl]);
  AddKey(ecDeleteWord, ord('T'), [ssCtrl]);
  AddKey(ecBlockUnindent, ord('U'), [ssCtrl,ssShift]);
  AddKey(ecPaste, ord('V'), [ssCtrl]);
  AddKey(ecCut, ord('X'), [ssCtrl]);
  AddKey(ecDeleteLine, ord('Y'), [ssCtrl]);
  AddKey(ecDeleteEOL, ord('Y'), [ssCtrl,ssShift]);
  AddKey(ecUndo, ord('Z'), [ssCtrl]);
  AddKey(ecRedo, ord('Z'), [ssCtrl,ssShift]);
  AddKey(ecGotoMarker0, ord('0'), [ssCtrl]);
  AddKey(ecGotoMarker1, ord('1'), [ssCtrl]);
  AddKey(ecGotoMarker2, ord('2'), [ssCtrl]);
  AddKey(ecGotoMarker3, ord('3'), [ssCtrl]);
  AddKey(ecGotoMarker4, ord('4'), [ssCtrl]);
  AddKey(ecGotoMarker5, ord('5'), [ssCtrl]);
  AddKey(ecGotoMarker6, ord('6'), [ssCtrl]);
  AddKey(ecGotoMarker7, ord('7'), [ssCtrl]);
  AddKey(ecGotoMarker8, ord('8'), [ssCtrl]);
  AddKey(ecGotoMarker9, ord('9'), [ssCtrl]);
  AddKey(ecSetMarker0, ord('0'), [ssCtrl,ssShift]);
  AddKey(ecSetMarker1, ord('1'), [ssCtrl,ssShift]);
  AddKey(ecSetMarker2, ord('2'), [ssCtrl,ssShift]);
  AddKey(ecSetMarker3, ord('3'), [ssCtrl,ssShift]);
  AddKey(ecSetMarker4, ord('4'), [ssCtrl,ssShift]);
  AddKey(ecSetMarker5, ord('5'), [ssCtrl,ssShift]);
  AddKey(ecSetMarker6, ord('6'), [ssCtrl,ssShift]);
  AddKey(ecSetMarker7, ord('7'), [ssCtrl,ssShift]);
  AddKey(ecSetMarker8, ord('8'), [ssCtrl,ssShift]);
  AddKey(ecSetMarker9, ord('9'), [ssCtrl,ssShift]);
  AddKey(ecFoldLevel1, ord('1'), [ssAlt,ssShift]);
  AddKey(ecFoldLevel2, ord('2'), [ssAlt,ssShift]);
  AddKey(ecFoldLevel3, ord('3'), [ssAlt,ssShift]);
  AddKey(ecFoldLevel4, ord('4'), [ssAlt,ssShift]);
  AddKey(ecFoldLevel5, ord('5'), [ssAlt,ssShift]);
  AddKey(ecFoldLevel6, ord('6'), [ssAlt,ssShift]);
  AddKey(ecFoldLevel7, ord('7'), [ssAlt,ssShift]);
  AddKey(ecFoldLevel8, ord('8'), [ssAlt,ssShift]);
  AddKey(ecFoldLevel9, ord('9'), [ssAlt,ssShift]);
  AddKey(ecFoldLevel0, ord('0'), [ssAlt,ssShift]);
  AddKey(ecFoldCurrent, ord('-'), [ssAlt,ssShift]);
  AddKey(ecUnFoldCurrent, ord('+'), [ssAlt,ssShift]);
  AddKey(EcToggleMarkupWord, ord('M'), [ssAlt]);
  AddKey(ecNormalSelect, ord('N'), [ssCtrl,ssShift]);
  AddKey(ecColumnSelect, ord('C'), [ssCtrl,ssShift]);
  AddKey(ecLineSelect, ord('L'), [ssCtrl,ssShift]);
  AddKey(ecTab, VK_TAB, []);
  AddKey(ecShiftTab, VK_TAB, [ssShift]);
  AddKey(ecMatchBracket, ord('B'), [ssCtrl,ssShift]);

  AddKey(ecColSelUp, VK_UP,    [ssAlt, ssShift]);
  AddKey(ecColSelDown, VK_DOWN,  [ssAlt, ssShift]);
  AddKey(ecColSelLeft, VK_LEFT, [ssAlt, ssShift]);
  AddKey(ecColSelRight, VK_RIGHT, [ssAlt, ssShift]);
  AddKey(ecColSelPageDown, VK_NEXT, [ssAlt, ssShift]);
  AddKey(ecColSelPageBottom, VK_NEXT, [ssAlt, ssShift,ssCtrl]);
  AddKey(ecColSelPageUp, VK_PRIOR, [ssAlt, ssShift]);
  AddKey(ecColSelPageTop, VK_PRIOR, [ssAlt, ssShift,ssCtrl]);
  AddKey(ecColSelLineStart, VK_HOME, [ssAlt, ssShift]);
  AddKey(ecColSelLineEnd, VK_END, [ssAlt, ssShift]);
  AddKey(ecColSelEditorTop, VK_HOME, [ssAlt, ssShift,ssCtrl]);
  AddKey(ecColSelEditorBottom, VK_END, [ssAlt, ssShift,ssCtrl]);
end;

procedure TSynEditKeyStrokes.SetItem(Index: Integer; Value: TSynEditKeyStroke);
begin
  inherited SetItem(Index, Value);
end;

{begin}                                                                         //ac 2000-07-05
procedure TSynEditKeyStrokes.SaveToStream(AStream: TStream);
var
  i, Num: integer;
begin
  Num := Count;
  AStream.Write(Num, SizeOf(Num));
  for i := 0 to Num - 1 do
    Items[i].SaveToStream(AStream);
end;
{end}                                                                           //ac 2000-07-05

initialization
  RegisterIntegerConsts(TypeInfo(TSynEditorCommand),
                        @IdentToEditorCommand,
                        @EditorCommandToIdent);

finalization
  ExtraIdentToIntFn := nil;
  ExtraIntToIdentFn := nil;
  ExtraGetEditorCommandValues := nil;

end.

