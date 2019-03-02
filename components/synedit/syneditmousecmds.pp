{ Mouse Command Configuration for SynEdit

  Copyright (C) 2009 Martn Friebe

  The contents of this file are subject to the Mozilla Public License
  Version 1.1 (the "License"); you may not use this file except in compliance
  with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
  the specific language governing rights and limitations under the License.

  Alternatively, the contents of this file may be used under the terms of the
  GNU General Public License Version 2 or later (the "GPL"), in which case
  the provisions of the GPL are applicable instead of those above.
  If you wish to allow use of your version of this file only under the terms
  of the GPL and not to allow others to use your version of this file
  under the MPL, indicate your decision by deleting the provisions above and
  replace them with the notice and other provisions required by the GPL.
  If you do not delete the provisions above, a recipient may use your version
  of this file under either the MPL or the GPL.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
  Boston, MA 02110-1335, USA.

}

unit SynEditMouseCmds;

{$I synedit.inc}

interface

uses
  Classes, SysUtils,
  // LCL
  LCLProc, Controls, Dialogs, Menus,
  // LazUtils
  LazMethodList,
  // SynEdit
  LazSynEditMouseCmdsTypes, SynEditStrConst, SynEditPointClasses, SynEditKeyCmds;

type

  TSynEditorMouseOption = (
    emUseMouseActions,         // Enable mouse actions
    emAltSetsColumnMode,       // Allows to activate "column" selection mode, if <Alt> key is pressed and text is being selected with mouse
    emDragDropEditing,         // Allows to drag-and-drop text blocks within the control
    emRightMouseMovesCursor,   // When clicking with the right mouse button, for a popup menu, move the caret to clicked position
    emDoubleClickSelectsLine,  // Selects entire line with double-click, otherwise double-click selects only current word
    emShowCtrlMouseLinks,      // Pressing <Ctrl> key (SYNEDIT_LINK_MODIFIER) will highlight the word under mouse cursor
    emCtrlWheelZoom            // Allows to zoom editor by <Ctrl+MouseWheel> commands
  );
  TSynEditorMouseOptions = set of TSynEditorMouseOption;

  TSynEditorMouseCommand = type word;
  TSynEditorMouseCommandOpt = type word;
  TSynMouseButton = LazSynEditMouseCmdsTypes.TSynMouseButton;
  TSynMAClickCount = (ccSingle, ccDouble, ccTriple, ccQuad, ccAny);
  TSynMAClickDir = (cdUp, cdDown);
  TSynMAUpRestriction = ( // Restrict cdUp
    crLastDownPos,                     // check if the lasth MouseDown had same Pos (as in would have triggered the same command)
    crLastDownPosSameLine,             // check if the last dow
    crLastDownPosSearchAll,            // allow to find downclick at lower priority or parent list
    crLastDownButton, crLastDownShift, // check if the lasth MouseDown had same Button / Shift
    crAllowFallback  // If action is restricted, continue search for up action in (fallback, or parent list)
  );
  TSynMAUpRestrictions = set of TSynMAUpRestriction;
  ESynMouseCmdError = class(Exception);

const
  crRestrictAll = [crLastDownPos, crLastDownPosSameLine, crLastDownButton, crLastDownShift];

  mbXLeft      =  LazSynEditMouseCmdsTypes.mbLeft;
  mbXRight     =  LazSynEditMouseCmdsTypes.mbRight;
  mbXMiddle    =  LazSynEditMouseCmdsTypes.mbMiddle;
  mbXExtra1    =  LazSynEditMouseCmdsTypes.mbExtra1;
  mbXExtra2    =  LazSynEditMouseCmdsTypes.mbExtra2;
  mbXWheelUp   =  LazSynEditMouseCmdsTypes.mbWheelUp;
  mbXWheelDown =  LazSynEditMouseCmdsTypes.mbWheelDown;
  mbXWheelLeft =  LazSynEditMouseCmdsTypes.mbWheelLeft;
  mbXWheelRight=  LazSynEditMouseCmdsTypes.mbWheelRight;

  SynMouseButtonMap: Array [TMouseButton] of TSynMouseButton =
    (mbXLeft, mbXRight, mbXMiddle, mbXExtra1, mbXExtra2);

  SynMouseButtonBackMap: Array [TSynMouseButton] of TMouseButton =
    (Controls.mbLeft, Controls.mbRight, Controls.mbMiddle,
     Controls.mbExtra1, Controls.mbExtra2,
     Controls.mbLeft, Controls.mbLeft, Controls.mbLeft, Controls.mbLeft);

type

  // Mouse actions to be handled *after* paintlock
  TSynEditMouseActionResult = record
    DoPopUpEvent: Boolean;               // Trigger OnContextPopUp, only valid if PopUpMenu is set
    PopUpEventX, PopUpEventY: Integer;
    PopUpMenu: TPopupMenu;               // PopupMenu to Display (must be outside PaintLock)
  end;

  TSynEditMouseActionInfo = record
    NewCaret: TSynEditCaret;
    Button: TSynMouseButton;
    Shift: TShiftState;
    MouseX, MouseY, WheelDelta: Integer;
    CCount: TSynMAClickCount;
    Dir: TSynMAClickDir;
    CaretDone: Boolean; // Return Value
    IgnoreUpClick: Boolean;
    ActionResult: TSynEditMouseActionResult;
  end;

  { TSynEditMouseAction }

  TSynEditMouseAction = class(TCollectionItem)
  private
    FButtonUpRestrictions: TSynMAUpRestrictions;
    FClickDir: TSynMAClickDir;
    FIgnoreUpClick: Boolean;
    FOption: TSynEditorMouseCommandOpt;
    FOption2: Integer;
    FPriority: TSynEditorMouseCommandOpt;
    FShift, FShiftMask: TShiftState;
    FButton: TSynMouseButton;
    FClickCount: TSynMAClickCount;
    FCommand: TSynEditorMouseCommand;
    FMoveCaret: Boolean;
    procedure SetButton(const AValue: TSynMouseButton);
    procedure SetButtonUpRestrictions(AValue: TSynMAUpRestrictions);
    procedure SetClickCount(const AValue: TSynMAClickCount);
    procedure SetClickDir(AValue: TSynMAClickDir);
    procedure SetCommand(const AValue: TSynEditorMouseCommand);
    procedure SetIgnoreUpClick(AValue: Boolean);
    procedure SetMoveCaret(const AValue: Boolean);
    procedure SetOption(const AValue: TSynEditorMouseCommandOpt);
    procedure SetOption2(AValue: Integer);
    procedure SetPriority(const AValue: TSynEditorMouseCommandOpt);
    procedure SetShift(const AValue: TShiftState);
    procedure SetShiftMask(const AValue: TShiftState);
  protected
    function GetDisplayName: string; override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    function IsMatchingShiftState(AShift: TShiftState): Boolean;
    function IsMatchingClick(ABtn: TSynMouseButton; ACCount: TSynMAClickCount;
                             ACDir: TSynMAClickDir): Boolean;
    function IsFallback: Boolean;
    function Conflicts(Other: TSynEditMouseAction): Boolean;
    function Equals(Other: TSynEditMouseAction; IgnoreCmd: Boolean = False): Boolean; reintroduce;
  published
    property Shift: TShiftState read FShift write SetShift                      default [];
    property ShiftMask: TShiftState read FShiftMask write SetShiftMask          default [];
    property Button: TSynMouseButton read FButton write SetButton               default mbXLeft;
    property ClickCount: TSynMAClickCount read FClickCount write SetClickCount  default ccSingle;
    property ClickDir: TSynMAClickDir read FClickDir write SetClickDir          default cdUp;
    property ButtonUpRestrictions: TSynMAUpRestrictions
             read FButtonUpRestrictions write SetButtonUpRestrictions           default [];
    property Command: TSynEditorMouseCommand read FCommand write SetCommand;
    property MoveCaret: Boolean read FMoveCaret write SetMoveCaret              default False;
    property IgnoreUpClick: Boolean read FIgnoreUpClick write SetIgnoreUpClick  default False; // only for mouse down
    property Option: TSynEditorMouseCommandOpt read FOption write SetOption     default 0;
    property Option2: Integer read FOption2 write SetOption2                    default 0;
    // Priority: 0 = highest / MaxInt = lowest
    property Priority: TSynEditorMouseCommandOpt read FPriority write SetPriority default 0;
  end;

  { TSynEditMouseActions }

  TSynEditMouseActions = class(TCollection)
  private
    FOwner: TPersistent;
    FAssertLock: Integer;
    function GetItem(Index: Integer): TSynEditMouseAction;
    procedure SetItem(Index: Integer; const AValue: TSynEditMouseAction);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TSynEditMouseAction;
    procedure Assign(Source: TPersistent); override;
    procedure AssertNoConflict(MAction: TSynEditMouseAction);
    function Equals(Other: TSynEditMouseActions): Boolean; reintroduce;
    function FindCommand(AnInfo: TSynEditMouseActionInfo;
                         APrevious: TSynEditMouseAction = nil): TSynEditMouseAction;
    procedure ResetDefaults; virtual;
    procedure IncAssertLock;
    procedure DecAssertLock;
    function  IndexOf(MAction: TSynEditMouseAction;
                      IgnoreCmd: Boolean = False): Integer;
    procedure AddCommand(const ACmd: TSynEditorMouseCommand;
             const AMoveCaret: Boolean;
             const AButton: TSynMouseButton; const AClickCount: TSynMAClickCount;
             const ADir: TSynMAClickDir; const AShift, AShiftMask: TShiftState;
             const AOpt: TSynEditorMouseCommandOpt = 0;
             const APrior: Integer = 0;
             const AOpt2: integer = 0;
             const AIgnoreUpClick: Boolean = False); overload;
    procedure AddCommand(const ACmd: TSynEditorMouseCommand;
             const AMoveCaret: Boolean;
             const AButton: TSynMouseButton; const AClickCount: TSynMAClickCount;
             const ADir: TSynMAClickDir; const AnUpRestrict: TSynMAUpRestrictions;
             const AShift, AShiftMask: TShiftState;
             const AOpt: TSynEditorMouseCommandOpt = 0;
             const APrior: Integer = 0;
             const AOpt2: integer = 0;
             const AIgnoreUpClick: Boolean = False); overload;
  public
    property Items[Index: Integer]: TSynEditMouseAction read GetItem
      write SetItem; default;
  end;

  { TSynEditMouseInternalActions }

  TSynEditMouseInternalActions = class(TSynEditMouseActions)
  private
    FOptions, FInternOptions: TSynEditorMouseOptions;
    FUserActions: TSynEditMouseActions;
    procedure SetOptions(AValue: TSynEditorMouseOptions);
    procedure SetUserActions(AValue: TSynEditMouseActions);
  protected
    procedure InitForOptions(AnOptions: TSynEditorMouseOptions); virtual;
  public
    constructor Create(AOwner: TPersistent);
    destructor Destroy; override;
    procedure ResetDefaults; override;
    procedure ResetUserActions;
    function  GetActionsForOptions(AnOptions: TSynEditorMouseOptions): TSynEditMouseActions;
    property  Options: TSynEditorMouseOptions read FOptions write SetOptions;
    property  UserActions: TSynEditMouseActions read FUserActions write SetUserActions;
  end;


  TSynEditMouseActionHandler = function(AnActionList: TSynEditMouseActions;
    var AnInfo: TSynEditMouseActionInfo): Boolean of object;

  // Called by SynEdit
  // Should Call "HandleActionProc" for each ActionList it want's to check
  TSynEditMouseActionSearchProc = function(var AnInfo: TSynEditMouseActionInfo;
    HandleActionProc: TSynEditMouseActionHandler): Boolean of object;

  // Called by "HandleActionProc", if an Action was found in the list
  TSynEditMouseActionExecProc = function(AnAction: TSynEditMouseAction;
    var AnInfo: TSynEditMouseActionInfo): Boolean of object;

  { TSynEditMouseActionSearchList }

  TSynEditMouseActionSearchList = Class(TMethodList)
  public
    function CallSearchHandlers(var AnInfo: TSynEditMouseActionInfo;
                         HandleActionProc: TSynEditMouseActionHandler): Boolean;
  end;

  { TSynEditMouseActionExecList }

  TSynEditMouseActionExecList = Class(TMethodList)
  public
    function CallExecHandlers(AnAction: TSynEditMouseAction;
                                  var AnInfo: TSynEditMouseActionInfo): Boolean;
  end;

const
  // EditorMouseCommands

  emcNone                     =  TSynEditorMouseCommand(0);
  emcStartSelections          =  TSynEditorMouseCommand(1);    // Start BlockSelection (Default Left Mouse Btn)
  emcStartColumnSelections    =  TSynEditorMouseCommand(3);    // Column BlockSelection (Default Alt - Left Mouse Btn)
  emcStartLineSelections      =  TSynEditorMouseCommand(4);    // Line BlockSelection (Default Alt - Left Mouse Btn)
  emcStartLineSelectionsNoneEmpty =  TSynEditorMouseCommand(5);    // Line BlockSelection (Default Alt - Left Mouse Btn)

  emcSelectWord               =  TSynEditorMouseCommand(6);
  emcSelectLine               =  TSynEditorMouseCommand(7);
  emcSelectPara               =  TSynEditorMouseCommand(8);

  emcStartDragMove            =  TSynEditorMouseCommand(9);
  emcPasteSelection           = TSynEditorMouseCommand(10);
  emcMouseLink                = TSynEditorMouseCommand(11);

  emcContextMenu              = TSynEditorMouseCommand(12);

  emcOnMainGutterClick        = TSynEditorMouseCommand(13);    // OnGutterClick

  emcCodeFoldCollaps          = TSynEditorMouseCommand(14);
  emcCodeFoldExpand           = TSynEditorMouseCommand(15);
  emcCodeFoldContextMenu      = TSynEditorMouseCommand(16);

  emcSynEditCommand           = TSynEditorMouseCommand(17);    // Key-Commands

  emcWheelScrollDown          = TSynEditorMouseCommand(18);
  emcWheelScrollUp            = TSynEditorMouseCommand(19);

  emcWheelVertScrollDown      = TSynEditorMouseCommand(20);
  emcWheelVertScrollUp        = TSynEditorMouseCommand(21);
  emcWheelHorizScrollDown     = TSynEditorMouseCommand(22);
  emcWheelHorizScrollUp       = TSynEditorMouseCommand(23);

  emcWheelZoomOut             = TSynEditorMouseCommand(24);
  emcWheelZoomIn              = TSynEditorMouseCommand(25);
  emcWheelZoomNorm            = TSynEditorMouseCommand(26);

  emcStartSelectTokens        =  TSynEditorMouseCommand(27);    // Start BlockSelection, word/token wise
  emcStartSelectWords         =  TSynEditorMouseCommand(28);    // Start BlockSelection, wordwise
  emcStartSelectLines         =  TSynEditorMouseCommand(29);    // Start BlockSelection, linewise (but not line mode)

  emcOverViewGutterGotoMark   = TSynEditorMouseCommand(30);
  emcOverViewGutterScrollTo   = TSynEditorMouseCommand(31);

  emcMax = 31;

  emcPluginFirstSyncro     = 19000;
  emcPluginFirstMultiCaret = 19010;
  emcPluginFirst = 20000;

  // Options
  emcoSelectionStart          = TSynEditorMouseCommandOpt(0);
  emcoSelectionContinue       = TSynEditorMouseCommandOpt(1);

  emcoSelectLineSmart         =  TSynEditorMouseCommandOpt(0);
  emcoSelectLineFull          =  TSynEditorMouseCommandOpt(1);
  emcoMouseLinkShow           =  TSynEditorMouseCommandOpt(0);
  emcoMouseLinkHide           =  TSynEditorMouseCommandOpt(1);

  emcoNotDragedSetCaretOnUp   = TSynEditorMouseCommandOpt(0);
  emcoNotDragedNoCaretOnUp    = TSynEditorMouseCommandOpt(1);

  emcoCodeFoldCollapsOne      = TSynEditorMouseCommandOpt(0);
  emcoCodeFoldCollapsAll      = TSynEditorMouseCommandOpt(1);
  emcoCodeFoldCollapsAtCaret  = TSynEditorMouseCommandOpt(2);
  emcoCodeFoldCollapsPreCaret = TSynEditorMouseCommandOpt(3);
  emcoCodeFoldExpandOne       = TSynEditorMouseCommandOpt(0);
  emcoCodeFoldExpandAll       = TSynEditorMouseCommandOpt(1);

  // menu, and caret move
  emcoSelectionCaretMoveNever     = TSynEditorMouseCommandOpt(0);
  emcoSelectionCaretMoveOutside   = TSynEditorMouseCommandOpt(1); // click is outside selected area
  emcoSelectionCaretMoveAlways    = TSynEditorMouseCommandOpt(2);

  emcoWheelScrollSystem         = TSynEditorMouseCommandOpt(0); // Opt2 > 0 ==> percentage
  emcoWheelScrollLines          = TSynEditorMouseCommandOpt(1); // Opt2 > 0 ==> amount of lines
  emcoWheelScrollPages          = TSynEditorMouseCommandOpt(2); // Opt2 > 0 ==> percentage
  emcoWheelScrollPagesLessOne   = TSynEditorMouseCommandOpt(3); // Opt2 > 0 ==> percentage

type
  TMouseCmdNameAndOptProcs = function(emc: TSynEditorMouseCommand): String;

// Plugins don't know of other plugins, so they need to map the codes
// Plugins all start at ecPluginFirst (overlapping)
// If ask by SynEdit they add an offset

// Return the next offset
function AllocatePluginMouseRange(Count: Integer; OffsetOnly: Boolean = False): integer;

function MouseCommandName(emc: TSynEditorMouseCommand): String;
function MouseCommandConfigName(emc: TSynEditorMouseCommand): String;

function SynMouseCmdToIdent(SynMouseCmd: Longint; var Ident: String): Boolean;
function IdentToSynMouseCmd(const Ident: string; var SynMouseCmd: Longint): Boolean;
procedure GetEditorMouseCommandValues(Proc: TGetStrProc);

procedure RegisterMouseCmdIdentProcs(IdentToIntFn: TIdentToInt; IntToIdentFn: TIntToIdent);
procedure RegisterExtraGetEditorMouseCommandValues(AProc: TGetEditorCommandValuesProc);

procedure RegisterMouseCmdNameAndOptProcs(ANamesProc: TMouseCmdNameAndOptProcs; AOptProc: TMouseCmdNameAndOptProcs = nil);

const
  SYNEDIT_LINK_MODIFIER = {$IFDEF LCLcarbon}ssMeta{$ELSE}ssCtrl{$ENDIF};

implementation

const
  SynMouseCommandNames: array [0..30] of TIdentMapEntry = (
    (Value: emcNone;                  Name: 'emcNone'),
    (Value: emcStartSelections;       Name: 'emcStartSelections'),
    (Value: emcStartColumnSelections; Name: 'emcStartColumnSelections'),
    (Value: emcStartLineSelections;   Name: 'emcStartLineSelections'),
    (Value: emcStartLineSelectionsNoneEmpty;   Name: 'emcStartLineSelectionsNoneEmpty'),

    (Value: emcSelectWord;            Name: 'emcSelectWord'),
    (Value: emcSelectLine;            Name: 'emcSelectLine'),
    (Value: emcSelectPara;            Name: 'emcSelectPara'),

    (Value: emcStartDragMove;         Name: 'emcStartDragMove'),
    (Value: emcPasteSelection;        Name: 'emcPasteSelection'),
    (Value: emcMouseLink;             Name: 'emcMouseLink'),

    (Value: emcContextMenu;           Name: 'emcContextMenu'),

    (Value: emcOnMainGutterClick;     Name: 'emcOnMainGutterClick'),

    (Value: emcCodeFoldCollaps;       Name: 'emcCodeFoldCollaps'),
    (Value: emcCodeFoldExpand;        Name: 'emcCodeFoldExpand'),
    (Value: emcCodeFoldContextMenu;   Name: 'emcCodeFoldContextMenu'),

    (Value: emcSynEditCommand;        Name: 'emcSynEditCommand'),

    (Value: emcWheelScrollDown;       Name: 'emcWheelScrollDown'),
    (Value: emcWheelScrollUp;         Name: 'emcWheelScrollUp'),
    (Value: emcWheelVertScrollDown;   Name: 'emcWheelVertScrollDown'),
    (Value: emcWheelVertScrollUp;     Name: 'emcWheelVertScrollUp'),
    (Value: emcWheelHorizScrollDown;  Name: 'emcWheelHorizScrollDown'),
    (Value: emcWheelHorizScrollUp;    Name: 'emcWheelHorizScrollUp'),

    (Value: emcWheelZoomOut;          Name: 'emcWheelZoomOut'),
    (Value: emcWheelZoomIn;           Name: 'emcWheelZoomIn'),
    (Value: emcWheelZoomNorm;         Name: 'emcWheelZoomNorm'),

    (Value: emcStartSelectTokens;     Name: 'emcStartSelectTokens'),
    (Value: emcStartSelectWords;      Name: 'emcStartSelectWords'),
    (Value: emcStartSelectLines;      Name: 'emcStartSelectLines'),

    (Value: emcOverViewGutterGotoMark;Name: 'emcOverViewGutterGotoMark'),
    (Value: emcOverViewGutterScrollTo;Name: 'emcOverViewGutterScrollTo')

  );

var
  ExtraIdentToIntFn: Array of TIdentToInt = nil;
  ExtraIntToIdentFn: Array of TIntToIdent = nil;
  ExtraGetEditorCommandValues: Array of TGetEditorCommandValuesProc = nil;
  ExtraMouseCmdNameFn: Array of TMouseCmdNameAndOptProcs = nil;
  ExtraMouseCmdOptFn: Array of TMouseCmdNameAndOptProcs = nil;

function AllocatePluginMouseRange(Count: Integer; OffsetOnly: Boolean = False): integer;
const
  CurOffset : integer = 0;
begin
  Result := CurOffset;
  inc(CurOffset, Count);
  if not OffsetOnly then
    inc(Result, emcPluginFirst);
end;

function MouseCommandName(emc: TSynEditorMouseCommand): String;
var
  i: Integer;
begin
  case emc of
    emcNone:                  Result := SYNS_emcNone;
    emcStartSelections:       Result := SYNS_emcStartSelection;
    emcStartColumnSelections: Result := SYNS_emcStartColumnSelections;
    emcStartLineSelections:   Result := SYNS_emcStartLineSelections;
    emcStartLineSelectionsNoneEmpty: Result := SYNS_emcStartLineSelectionsNoneEmpty;
    emcSelectWord:            Result := SYNS_emcSelectWord;
    emcSelectLine:            Result := SYNS_emcSelectLine;
    emcSelectPara:            Result := SYNS_emcSelectPara;
    emcStartDragMove:         Result := SYNS_emcStartDragMove;
    emcPasteSelection:        Result := SYNS_emcPasteSelection;
    emcMouseLink:             Result := SYNS_emcMouseLink;
    emcContextMenu:           Result := SYNS_emcContextMenu;

    emcOnMainGutterClick:     Result := SYNS_emcBreakPointToggle;

    emcCodeFoldCollaps:       Result := SYNS_emcCodeFoldCollaps;
    emcCodeFoldExpand:        Result := SYNS_emcCodeFoldExpand;
    emcCodeFoldContextMenu:   Result := SYNS_emcCodeFoldContextMenu;

    emcSynEditCommand:        Result := SYNS_emcSynEditCommand;

    emcWheelScrollDown:       Result := SYNS_emcWheelScrollDown;
    emcWheelScrollUp:         Result := SYNS_emcWheelScrollUp;
    emcWheelHorizScrollDown:  Result := SYNS_emcWheelHorizScrollDown;
    emcWheelHorizScrollUp:    Result := SYNS_emcWheelHorizScrollUp;
    emcWheelVertScrollDown:   Result := SYNS_emcWheelVertScrollDown;
    emcWheelVertScrollUp:     Result := SYNS_emcWheelVertScrollUp;

    emcWheelZoomOut:          Result := SYNS_emcWheelZoomOut;
    emcWheelZoomIn:           Result := SYNS_emcWheelZoomIn;
    emcWheelZoomNorm:         Result := SYNS_emcWheelZoomNorm;

    emcStartSelectTokens:     Result := SYNS_emcStartSelectTokens;
    emcStartSelectWords:      Result := SYNS_emcStartSelectWords;
    emcStartSelectLines:      Result := SYNS_emcStartSelectLines;

    emcOverViewGutterGotoMark: Result := SYNS_emcOverViewGutterGotoMark;
    emcOverViewGutterScrollTo: Result := SYNS_emcOverViewGutterScrollTo;

    else begin
      Result := '';
      i := 0;
      while (i < length(ExtraMouseCmdNameFn)) and (Result = '') do begin
        Result := ExtraMouseCmdNameFn[i](emc);
        inc(i);
      end;
    end;
  end;
end;

function MouseCommandConfigName(emc: TSynEditorMouseCommand): String;
var
  i: Integer;
begin
  case emc of
    emcStartSelections,
    emcStartColumnSelections,
    emcStartLineSelections,
    emcStartLineSelectionsNoneEmpty:   Result := SYNS_emcSelection_opt;
    emcSelectLine:            Result := SYNS_emcSelectLine_opt;
    emcMouseLink:             Result := SYNS_emcMouseLink_opt;
    emcStartDragMove:         Result := SYNS_emcStartDragMove_opt;
    emcCodeFoldCollaps:       Result := SYNS_emcCodeFoldCollaps_opt;
    emcCodeFoldExpand:        Result := SYNS_emcCodeFoldExpand_opt;
    emcContextMenu:           Result := SYNS_emcContextMenuCaretMove_opt;
    emcWheelScrollDown..emcWheelHorizScrollUp:
                              Result := SYNS_emcWheelScroll_opt;
    else begin
      Result := '';
      i := 0;
      while (i < length(ExtraMouseCmdOptFn)) and (Result = '') do begin
        Result := ExtraMouseCmdOptFn[i](emc);
        inc(i);
      end;
    end;
  end;
end;

function SynMouseCmdToIdent(SynMouseCmd: Longint; var Ident: String): Boolean;
var
  i: Integer;
begin
  Ident := '';
  Result := IntToIdent(SynMouseCmd, Ident, SynMouseCommandNames);
  i := 0;
  while (i < length(ExtraIntToIdentFn)) and (not Result) do begin
    Result := ExtraIntToIdentFn[i](SynMouseCmd, Ident);
    inc(i);
  end;
end;

function IdentToSynMouseCmd(const Ident: string; var SynMouseCmd: Longint): Boolean;
var
  i: Integer;
begin
  SynMouseCmd := 0;
  Result := IdentToInt(Ident, SynMouseCmd, SynMouseCommandNames);
  i := 0;
  while (i < length(ExtraIdentToIntFn)) and (not Result) do begin
    Result := ExtraIdentToIntFn[i](Ident, SynMouseCmd);
    inc(i);
  end;
end;

procedure GetEditorMouseCommandValues(Proc: TGetStrProc);
var
  i: Integer;
begin
  for i := Low(SynMouseCommandNames) to High(SynMouseCommandNames) do
    Proc(SynMouseCommandNames[I].Name);
  i := 0;
  while (i < length(ExtraGetEditorCommandValues)) do begin
    ExtraGetEditorCommandValues[i](Proc);
    inc(i);
  end;
end;

procedure RegisterMouseCmdIdentProcs(IdentToIntFn: TIdentToInt; IntToIdentFn: TIntToIdent);
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

procedure RegisterExtraGetEditorMouseCommandValues(AProc: TGetEditorCommandValuesProc);
var
  i: Integer;
begin
  i := length(ExtraGetEditorCommandValues);
  SetLength(ExtraGetEditorCommandValues, i + 1);
  ExtraGetEditorCommandValues[i] := AProc;
end;

procedure RegisterMouseCmdNameAndOptProcs(ANamesProc: TMouseCmdNameAndOptProcs;
  AOptProc: TMouseCmdNameAndOptProcs);
var
  i: Integer;
begin
  i := length(ExtraMouseCmdNameFn);
  SetLength(ExtraMouseCmdNameFn, i + 1);
  ExtraMouseCmdNameFn[i] := ANamesProc;
  if AOptProc = nil then
    exit;
  i := length(ExtraMouseCmdOptFn);
  SetLength(ExtraMouseCmdOptFn, i + 1);
  ExtraMouseCmdOptFn[i] := AOptProc;
end;

{ TSynEditMouseInternalActions }

procedure TSynEditMouseInternalActions.SetOptions(AValue: TSynEditorMouseOptions);
begin
  FOptions := AValue;
  if emUseMouseActions in FOptions then exit;

  AValue := AValue - [emUseMouseActions];
  if (FInternOptions = AValue) and (Count > 0) then exit;
  FInternOptions := AValue;
  InitForOptions(FInternOptions);
end;

procedure TSynEditMouseInternalActions.SetUserActions(AValue: TSynEditMouseActions);
begin
  if AValue =nil then
    FUserActions.Clear
  else
    FUserActions.Assign(AValue);
end;

procedure TSynEditMouseInternalActions.InitForOptions(AnOptions: TSynEditorMouseOptions);
begin
  Clear;
end;

constructor TSynEditMouseInternalActions.Create(AOwner: TPersistent);
begin
  FOptions := [];
  FUserActions := TSynEditMouseActions.Create(AOwner);
  inherited Create(AOwner);
end;

destructor TSynEditMouseInternalActions.Destroy;
begin
  FreeAndNil(FUserActions);
  inherited Destroy;
end;

procedure TSynEditMouseInternalActions.ResetDefaults;
begin
  InitForOptions(FOptions);
end;

procedure TSynEditMouseInternalActions.ResetUserActions;
begin
  if emUseMouseActions in FOptions then begin
    if (FInternOptions <> FOptions - [emUseMouseActions]) or (Count = 0) then begin
      FInternOptions := FOptions - [emUseMouseActions];
      InitForOptions(FInternOptions);
    end;
    FUserActions.Assign(Self);
  end
  else begin
    FUserActions.Clear;
  end;
end;

function TSynEditMouseInternalActions.GetActionsForOptions(AnOptions: TSynEditorMouseOptions): TSynEditMouseActions;
begin
  Options := AnOptions;
  if emUseMouseActions in FOptions then
    Result := FUserActions
  else
    Result := Self;
end;

{ TSynEditMouseAction }

procedure TSynEditMouseAction.SetButton(const AValue: TSynMouseButton);
begin
  if FButton = AValue then exit;
  FButton := AValue;
  if Collection <> nil then
    TSynEditMouseActions(Collection).AssertNoConflict(self);

  if FButton in [mbXWheelUp, mbXWheelDown, mbXWheelLeft, mbXWheelRight] then
    ClickDir := cdDown;
end;

procedure TSynEditMouseAction.SetButtonUpRestrictions(AValue: TSynMAUpRestrictions);
begin
  if FButtonUpRestrictions = AValue then Exit;
  FButtonUpRestrictions := AValue;
  if Collection <> nil then
    TSynEditMouseActions(Collection).AssertNoConflict(self);
end;

procedure TSynEditMouseAction.SetClickCount(const AValue: TSynMAClickCount);
begin
  if FClickCount = AValue then exit;
  FClickCount := AValue;
  if Collection <> nil then
    TSynEditMouseActions(Collection).AssertNoConflict(self);
end;

procedure TSynEditMouseAction.SetClickDir(AValue: TSynMAClickDir);
begin
  if FButton in [mbXWheelUp, mbXWheelDown, mbXWheelLeft, mbXWheelRight] then
    AValue := cdDown;
  if FClickDir = AValue then exit;
  FClickDir := AValue;
  if Collection <> nil then
    TSynEditMouseActions(Collection).AssertNoConflict(self);
end;

procedure TSynEditMouseAction.SetCommand(const AValue: TSynEditorMouseCommand);
begin
  if FCommand = AValue then exit;
  FCommand := AValue;
  if Collection <> nil then
    TSynEditMouseActions(Collection).AssertNoConflict(self);
end;

procedure TSynEditMouseAction.SetIgnoreUpClick(AValue: Boolean);
begin
  if FIgnoreUpClick = AValue then Exit;
  FIgnoreUpClick := AValue;
  if Collection <> nil then
    TSynEditMouseActions(Collection).AssertNoConflict(self);
end;

procedure TSynEditMouseAction.SetMoveCaret(const AValue: Boolean);
begin
  if FMoveCaret = AValue then exit;
  FMoveCaret := AValue;
  if Collection <> nil then
    TSynEditMouseActions(Collection).AssertNoConflict(self);
end;

procedure TSynEditMouseAction.SetOption(const AValue: TSynEditorMouseCommandOpt);
begin
  if FOption = AValue then exit;
  FOption := AValue;
  if Collection <> nil then
    TSynEditMouseActions(Collection).AssertNoConflict(self);
end;

procedure TSynEditMouseAction.SetOption2(AValue: Integer);
begin
  if FOption2 = AValue then Exit;
  FOption2 := AValue;
  if Collection <> nil then
    TSynEditMouseActions(Collection).AssertNoConflict(self);
end;

procedure TSynEditMouseAction.SetPriority(const AValue: TSynEditorMouseCommandOpt);
begin
  if FPriority = AValue then exit;
  FPriority := AValue;
  if Collection <> nil then
    TSynEditMouseActions(Collection).AssertNoConflict(self);
end;

procedure TSynEditMouseAction.SetShift(const AValue: TShiftState);
begin
  if FShift = AValue then exit;
  FShift := AValue;
  if Collection <> nil then
    TSynEditMouseActions(Collection).AssertNoConflict(self);
end;

procedure TSynEditMouseAction.SetShiftMask(const AValue: TShiftState);
begin
  if FShiftMask = AValue then exit;
  FShiftMask := AValue;
  if Collection <> nil then
    TSynEditMouseActions(Collection).AssertNoConflict(self);
end;

function TSynEditMouseAction.GetDisplayName: string;
begin
  Result := MouseCommandName(FCommand);
end;

procedure TSynEditMouseAction.Assign(Source: TPersistent);
begin
  if Source is TSynEditMouseAction then
  begin
    FCommand    := TSynEditMouseAction(Source).Command;
    FClickCount := TSynEditMouseAction(Source).ClickCount;
    FClickDir   := TSynEditMouseAction(Source).ClickDir;
    FButtonUpRestrictions := TSynEditMouseAction(Source).ButtonUpRestrictions;
    FButton     := TSynEditMouseAction(Source).Button;
    FShift      := TSynEditMouseAction(Source).Shift;
    FShiftMask  := TSynEditMouseAction(Source).ShiftMask;
    FMoveCaret  := TSynEditMouseAction(Source).MoveCaret;
    FIgnoreUpClick := TSynEditMouseAction(Source).IgnoreUpClick;
    FOption     := TSynEditMouseAction(Source).FOption;
    FOption2    := TSynEditMouseAction(Source).FOption2;
    FPriority   := TSynEditMouseAction(Source).Priority;
  end else
    inherited Assign(Source);
  if Collection <> nil then
    TSynEditMouseActions(Collection).AssertNoConflict(self);
end;

procedure TSynEditMouseAction.Clear;
begin
  FCommand    := 0;
  FClickCount := ccSingle;
  FClickDir   := cdUp;
  FButton     := mbXLeft;
  FShift      := [];
  FShiftMask  := [];
  FMoveCaret  := False;
  FOption     := 0;
  FPriority   := 0;
  FIgnoreUpClick := False;
end;

function TSynEditMouseAction.IsMatchingShiftState(AShift: TShiftState): Boolean;
begin
  Result := AShift * FShiftMask = FShift;
end;

function TSynEditMouseAction.IsMatchingClick(ABtn: TSynMouseButton; ACCount: TSynMAClickCount;
  ACDir: TSynMAClickDir): Boolean;
begin
  Result := (Button     = ABtn)
        and ((ClickCount = ACCount) or (ClickCount = ccAny))
        and (ClickDir   = ACDir)
end;

function TSynEditMouseAction.IsFallback: Boolean;
begin
  Result := FShiftMask = [];
end;

function TSynEditMouseAction.Conflicts(Other: TSynEditMouseAction): Boolean;
begin
  If (Other = nil) or (Other = self) then exit(False);
  Result := (Other.Button     = self.Button)
        and ((Other.ClickCount = self.ClickCount)
             or (self.ClickCount = ccAny) or (Other.ClickCount = ccAny))
        and (Other.ClickDir   = self.ClickDir)
        and (Other.Shift * self.ShiftMask = self.Shift * Other.ShiftMask)
        and ((Other.Command   <> self.Command) or  // Only conflicts, if Command differs
             (Other.MoveCaret <> self.MoveCaret) or
             (Other.IgnoreUpClick <> self.IgnoreUpClick) or
             (Other.Option    <> self.Option) or
             (Other.Option2   <> self.Option2) )
        and not(Other.IsFallback xor self.IsFallback)
        and (Other.Priority   = self.Priority);
end;

function TSynEditMouseAction.Equals(Other: TSynEditMouseAction;
  IgnoreCmd: Boolean = False): Boolean;
begin
  Result := (Other.Button     = self.Button)
        and (Other.ClickCount = self.ClickCount)
        and (Other.ClickDir   = self.ClickDir)
        and (Other.ButtonUpRestrictions   = self.ButtonUpRestrictions)
        and (Other.Shift      = self.Shift)
        and (Other.ShiftMask  = self.ShiftMask)
        and (Other.Priority   = self.Priority)
        and ((Other.Command   = self.Command) or IgnoreCmd)
        and ((Other.Option    = self.Option) or IgnoreCmd)
        and ((Other.Option2   = self.Option2) or IgnoreCmd)
        and ((Other.MoveCaret = self.MoveCaret) or IgnoreCmd)
        and ((Other.IgnoreUpClick = self.IgnoreUpClick) or IgnoreCmd);
end;

{ TSynEditMouseActions }

function TSynEditMouseActions.GetItem(Index: Integer): TSynEditMouseAction;
begin
 Result := TSynEditMouseAction(inherited GetItem(Index));
end;

procedure TSynEditMouseActions.SetItem(Index: Integer; const AValue: TSynEditMouseAction);
begin
  inherited SetItem(Index, AValue);
end;

function TSynEditMouseActions.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TSynEditMouseActions.Update(Item: TCollectionItem);
var
  i: Integer;
  Err : ESynMouseCmdError;
begin
  inherited Update(Item);
  i := Count - 1;
  Err := nil;
  while i > 0 do begin
    try
      AssertNoConflict(Items[i]);
    except
      on E : ESynMouseCmdError do begin
        if assigned(Owner) and (csDesigning in TComponent(Owner).ComponentState)
        then
          MessageDlg(SYNS_EDuplicateShortCut, E.Message + LineEnding + Items[i].DisplayName,
                     mtWarning, [mbOK], '')
        else
          Err := E;
        //if not(assigned(Owner) and (csLoading in TComponent(Owner).ComponentState))
        //then
        //  raise E;
      end;
    end;
    dec(i);
  end;
  if assigned(Err) then
    raise Err;
end;

constructor TSynEditMouseActions.Create(AOwner: TPersistent);
begin
  inherited Create(TSynEditMouseAction);
  FOwner := AOwner;
  FAssertLock := 0;
end;

function TSynEditMouseActions.Add: TSynEditMouseAction;
begin
  Result := TSynEditMouseAction(inherited Add);
end;

procedure TSynEditMouseActions.Assign(Source: TPersistent);
var
  i: Integer;
begin
  if Source is TSynEditMouseActions then
  begin
    Clear;
    BeginUpdate;
    for i := 0 to TSynEditMouseActions(Source).Count-1 do
      Add.Assign(TSynEditMouseActions(Source)[i]);
    EndUpdate;
  end
  else
    inherited Assign(Source);
end;

procedure TSynEditMouseActions.AssertNoConflict(MAction: TSynEditMouseAction);
var
  i: Integer;
begin
  if (FAssertLock > 0) or (UpdateCount > 0) then exit;
  for i := 0 to Count-1 do begin
    if Items[i].Conflicts(MAction) then
      raise ESynMouseCmdError.Create(SYNS_EDuplicateShortCut);
  end;
end;

function TSynEditMouseActions.Equals(Other: TSynEditMouseActions): Boolean;
var
  i: Integer;
begin
  Result := False;
  if Count <> Other.Count then exit;

  for i := 0 to Count - 1 do
    if Other.IndexOf(Items[i]) < 0 then
      exit;
  Result := True;
end;

function TSynEditMouseActions.FindCommand(AnInfo: TSynEditMouseActionInfo;
  APrevious: TSynEditMouseAction = nil): TSynEditMouseAction;
var
  i, MinPriority: Integer;
  act, found, fback: TSynEditMouseAction;
begin
  MinPriority := 0;
  if assigned(APrevious) then
    MinPriority := APrevious.Priority + 1;
  fback := nil;
  found := nil;
  for i := 0 to Count-1 do begin
    act := Items[i];
    if act.Priority < MinPriority then
      continue;

    if act.IsMatchingClick(AnInfo.Button, AnInfo.CCount, AnInfo.Dir) and
       act.IsMatchingShiftState(AnInfo.Shift)
    then begin
      if act.IsFallback then begin
        if (fback = nil) or (act.Priority < fback.Priority) then
          fback := act;
      end
      else begin
        if (found = nil) or (act.Priority < found.Priority) then
          found := act;
      end;
    end;
  end;
  if found <> nil then begin
    if (fback <> nil) and (fback.Priority < found.Priority) then
      Result := fback
    else
      Result := found;
  end
  else if fback <> nil then
    Result := fback
  else
    Result := nil;
end;

procedure TSynEditMouseActions.AddCommand(const ACmd: TSynEditorMouseCommand;
  const AMoveCaret: Boolean; const AButton: TSynMouseButton;
  const AClickCount: TSynMAClickCount; const ADir: TSynMAClickDir;
  const AShift, AShiftMask: TShiftState; const AOpt: TSynEditorMouseCommandOpt = 0;
  const APrior: Integer = 0; const AOpt2: integer = 0; const AIgnoreUpClick: Boolean = False);
begin
  AddCommand(ACmd, AMoveCaret, AButton, AClickCount, ADir, [], AShift, AShiftMask, AOpt, APrior,
    AOpt2, AIgnoreUpClick);
end;

procedure TSynEditMouseActions.AddCommand(const ACmd: TSynEditorMouseCommand;
  const AMoveCaret: Boolean; const AButton: TSynMouseButton;
  const AClickCount: TSynMAClickCount; const ADir: TSynMAClickDir;
  const AnUpRestrict: TSynMAUpRestrictions; const AShift, AShiftMask: TShiftState;
  const AOpt: TSynEditorMouseCommandOpt; const APrior: Integer; const AOpt2: integer;
  const AIgnoreUpClick: Boolean);
var
  new: TSynEditMouseAction;
begin
  inc(FAssertLock);
  try
    new := Add;
    with new do begin
      Command := ACmd;
      MoveCaret := AMoveCaret;
      Button := AButton;
      ClickCount := AClickCount;
      ClickDir := ADir;
      ButtonUpRestrictions := AnUpRestrict;
      Shift := AShift;
      ShiftMask := AShiftMask;
      Option := AOpt;
      Priority := APrior;
      Option2 := AOpt2;
      IgnoreUpClick := AIgnoreUpClick;
    end;
  finally
    dec(FAssertLock);
  end;
  try
    AssertNoConflict(new);
  except
    Delete(Count-1);
    raise;
  end;
end;

procedure TSynEditMouseActions.ResetDefaults;
begin
  Clear;
end;

procedure TSynEditMouseActions.IncAssertLock;
begin
  inc(FAssertLock);
end;

procedure TSynEditMouseActions.DecAssertLock;
begin
  dec(FAssertLock);
end;

function TSynEditMouseActions.IndexOf(MAction: TSynEditMouseAction;
  IgnoreCmd: Boolean = False): Integer;
begin
  Result := Count - 1;
  while Result >= 0 do begin
    if Items[Result].Equals(MAction, IgnoreCmd) then exit;
    Dec(Result);
  end;
end;

{ TSynEditMouseActionSearchList }

function TSynEditMouseActionSearchList.CallSearchHandlers(var AnInfo: TSynEditMouseActionInfo;
  HandleActionProc: TSynEditMouseActionHandler): Boolean;
var
  i: LongInt;
begin
  i:=Count;
  Result := False;
  while NextDownIndex(i) and (not Result) do
    Result := TSynEditMouseActionSearchProc(Items[i])(AnInfo, HandleActionProc);
end;

{ TSynEditMouseActionExecList }

function TSynEditMouseActionExecList.CallExecHandlers(AnAction: TSynEditMouseAction;
  var AnInfo: TSynEditMouseActionInfo): Boolean;
var
  i: LongInt;
begin
  i:=Count;
  Result := False;
  while NextDownIndex(i) and (not Result) do
    Result := TSynEditMouseActionExecProc(Items[i])(AnAction, AnInfo);
end;

initialization
  RegisterIntegerConsts(TypeInfo(TSynEditorMouseCommand), @IdentToSynMouseCmd, @SynMouseCmdToIdent);

finalization
  ExtraIdentToIntFn := nil;
  ExtraIntToIdentFn := nil;
  ExtraGetEditorCommandValues := nil;
  ExtraMouseCmdNameFn := nil;
  ExtraMouseCmdOptFn := nil;

end.

