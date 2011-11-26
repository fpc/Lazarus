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
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.

}

unit SynEditMouseCmds;

{$I synedit.inc}

interface

uses
  Classes, Controls, SysUtils, SynEditStrConst, SynEditPointClasses, Dialogs,
  LCLProc;

type

  TSynEditorMouseCommand = type word;
  TSynEditorMouseCommandOpt = type word;
  TSynMAClickCount = (ccSingle, ccDouble, ccTriple, ccQuad, ccAny);
  TSynMAClickDir = (cdUp, cdDown);
  ESynMouseCmdError = class(Exception);

  TSynEditMouseActionInfo = record
    NewCaret: TSynEditCaret;
    Button: TMouseButton;
    Shift: TShiftState;
    MouseX, MouseY: Integer;
    CCount: TSynMAClickCount;
    Dir: TSynMAClickDir;
    CaretDone: Boolean; // Return Value
    IgnoreUpClick: Boolean;
  end;

  { TSynEditMouseAction }

  TSynEditMouseAction = class(TCollectionItem)
  private
    FClickDir: TSynMAClickDir;
    FOption: TSynEditorMouseCommandOpt;
    FPriority: TSynEditorMouseCommandOpt;
    FShift, FShiftMask: TShiftState;
    FButton: TMouseButton;
    FClickCount: TSynMAClickCount;
    FCommand: TSynEditorMouseCommand;
    FMoveCaret: Boolean;
    procedure SetButton(const AValue: TMouseButton);
    procedure SetClickCount(const AValue: TSynMAClickCount);
    procedure SetClickDir(const AValue: TSynMAClickDir);
    procedure SetCommand(const AValue: TSynEditorMouseCommand);
    procedure SetMoveCaret(const AValue: Boolean);
    procedure SetOption(const AValue: TSynEditorMouseCommandOpt);
    procedure SetPriority(const AValue: TSynEditorMouseCommandOpt);
    procedure SetShift(const AValue: TShiftState);
    procedure SetShiftMask(const AValue: TShiftState);
  protected
    function GetDisplayName: string; override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    function IsMatchingShiftState(AShift: TShiftState): Boolean;
    function IsMatchingClick(ABtn: TMouseButton; ACCount: TSynMAClickCount;
                             ACDir: TSynMAClickDir): Boolean;
    function IsFallback: Boolean;
    function Conflicts(Other: TSynEditMouseAction): Boolean;
    function Equals(Other: TSynEditMouseAction; IgnoreCmd: Boolean = False): Boolean; reintroduce;
  published
    property Shift: TShiftState read FShift write SetShift                      default [];
    property ShiftMask: TShiftState read FShiftMask write SetShiftMask          default [];
    property Button: TMouseButton read FButton write SetButton                  default mbLeft;
    property ClickCount: TSynMAClickCount read FClickCount write SetClickCount  default ccSingle;
    property ClickDir: TSynMAClickDir read FClickDir write SetClickDir          default cdUp;
    property Command: TSynEditorMouseCommand read FCommand write SetCommand;
    property MoveCaret: Boolean read FMoveCaret write SetMoveCaret              default False;
    property Option: TSynEditorMouseCommandOpt read FOption write SetOption     default 0;
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
             const AButton: TMouseButton; const AClickCount: TSynMAClickCount;
             const ADir: TSynMAClickDir; const AShift, AShiftMask: TShiftState;
             const AOpt: TSynEditorMouseCommandOpt = 0;
             const APrior: Integer = 0);
  public
    property Items[Index: Integer]: TSynEditMouseAction read GetItem
      write SetItem; default;
  end;

  { TSynEditMouseTextActions }

  TSynEditMouseTextActions = class(TSynEditMouseActions)
  public
    procedure ResetDefaults; override;
  end;

  { TSynEditSelMouseActions }

  TSynEditMouseSelActions = class(TSynEditMouseActions)
  public
    procedure ResetDefaults; override;
  end;

  TSynEditMouseActionHandler = function(AnActionList: TSynEditMouseActions;
    AnInfo: TSynEditMouseActionInfo): Boolean of object;

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

  emcMax = 17;

  emcPluginFirst = 20000;

  // Options
  emcoSelectionStart          = 0;
  emcoSelectionContinue       = 1;

  emcoSelectLineSmart         =  0;
  emcoSelectLineFull          =  1;
  emcoMouseLinkShow           =  0;
  emcoMouseLinkHide           =  1;

  emcoCodeFoldCollapsOne      = 0;
  emcoCodeFoldCollapsAll      = 1;
  emcoCodeFoldCollapsAtCaret  = 2;
  emcoCodeFoldCollapsPreCaret = 3;
  emcoCodeFoldExpandOne       = 0;
  emcoCodeFoldExpandAll       = 1;

  // menu, and caret move
  emcoSelectionCaretMoveNever     = 0;
  emcoSelectionCaretMoveOutside   = 1; // click is outside selected area
  emcoSelectionCaretMoveAlways    = 2;

// Plugins don't know of other plugins, so they need to map the codes
// Plugins all start at ecPluginFirst (overlapping)
// If ask by SynEdit they add an offset

// Return the next offset
function AllocatePluginMouseRange(Count: Integer): integer;

function MouseCommandName(emc: TSynEditorMouseCommand): String;
function MouseCommandConfigName(emc: TSynEditorMouseCommand): String;

function SynMouseCmdToIdent(SynMouseCmd: Longint; out Ident: String): Boolean;
function IdentToSynMouseCmd(const Ident: string; out SynMouseCmd: Longint): Boolean;

const
  SYNEDIT_LINK_MODIFIER = {$IFDEF LCLcarbon}ssMeta{$ELSE}ssCtrl{$ENDIF};

implementation

const
  SynMouseCommandNames: array [0..15] of TIdentMapEntry = (
    (Value: emcNone; Name: 'emcNone'),
    (Value: emcStartSelections; Name: 'emcStartSelections'),
    (Value: emcStartColumnSelections; Name: 'emcStartColumnSelections'),
    (Value: emcStartLineSelections; Name: 'emcStartLineSelections'),

    (Value: emcSelectWord; Name: 'emcSelectWord'),
    (Value: emcSelectLine; Name: 'emcSelectLine'),
    (Value: emcSelectPara; Name: 'emcSelectPara'),

    (Value: emcStartDragMove; Name: 'emcStartDragMove'),
    (Value: emcPasteSelection; Name: 'emcPasteSelection'),
    (Value: emcMouseLink; Name: 'emcMouseLink'),

    (Value: emcContextMenu; Name: 'emcContextMenu'),

    (Value: emcOnMainGutterClick; Name: 'emcOnMainGutterClick'),

    (Value: emcCodeFoldCollaps; Name: 'emcCodeFoldCollaps'),
    (Value: emcCodeFoldExpand; Name: 'emcCodeFoldExpand'),
    (Value: emcCodeFoldContextMenu; Name: 'emcCodeFoldContextMenu'),

    (Value: emcSynEditCommand; Name: 'emcSynEditCommand')
  );

function AllocatePluginMouseRange(Count: Integer): integer;
const
  CurOffset : integer = 0;
begin
  Result := CurOffset;
  inc(CurOffset, Count);
end;

function MouseCommandName(emc: TSynEditorMouseCommand): String;
begin
  case emc of
    emcNone:                  Result := SYNS_emcNone;
    emcStartSelections:       Result := SYNS_emcStartSelection;
    emcStartColumnSelections: Result := SYNS_emcStartColumnSelections;
    emcStartLineSelections:   Result := SYNS_emcStartLineSelections;
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

    else Result := ''
  end;
end;

function MouseCommandConfigName(emc: TSynEditorMouseCommand): String;
begin
  case emc of
    emcStartSelections,
    emcStartColumnSelections,
    emcStartLineSelections: Result := SYNS_emcSelection_opt;
    emcSelectLine: Result := SYNS_emcSelectLine_opt;
    emcMouseLink:   Result := SYNS_emcMouseLink_opt;
    emcCodeFoldCollaps: Result := SYNS_emcCodeFoldCollaps_opt;
    emcCodeFoldExpand:  Result := SYNS_emcCodeFoldExpand_opt;
    emcContextMenu:  Result := SYNS_emcContextMenuCaretMove_opt;
    else Result := ''
  end;
end;

function SynMouseCmdToIdent(SynMouseCmd: Longint; out Ident: String): Boolean;
begin
  Result := IntToIdent(SynMouseCmd, Ident, SynMouseCommandNames);
end;

function IdentToSynMouseCmd(const Ident: string; out SynMouseCmd: Longint): Boolean;
begin
  Result := IdentToInt(Ident, SynMouseCmd, SynMouseCommandNames);
end;

{ TSynEditMouseAction }

procedure TSynEditMouseAction.SetButton(const AValue: TMouseButton);
begin
  if FButton = AValue then exit;
  FButton := AValue;
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

procedure TSynEditMouseAction.SetClickDir(const AValue: TSynMAClickDir);
begin
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
    FButton     := TSynEditMouseAction(Source).Button;
    FShift      := TSynEditMouseAction(Source).Shift;
    FShiftMask  := TSynEditMouseAction(Source).ShiftMask;
    FMoveCaret  := TSynEditMouseAction(Source).MoveCaret;
    FOption     := TSynEditMouseAction(Source).FOption;
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
  FButton     := mbLeft;
  FShift      := [];
  FShiftMask  := [];
  FMoveCaret  := False;
  FOption     := 0;
  FPriority   := 0;
end;

function TSynEditMouseAction.IsMatchingShiftState(AShift: TShiftState): Boolean;
begin
  Result := AShift * FShiftMask = FShift;
end;

function TSynEditMouseAction.IsMatchingClick(ABtn: TMouseButton; ACCount: TSynMAClickCount;
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
             (Other.Option    <> self.Option) )
        and not(Other.IsFallback xor self.IsFallback)
        and (Other.Priority   = self.Priority);
end;

function TSynEditMouseAction.Equals(Other: TSynEditMouseAction;
  IgnoreCmd: Boolean = False): Boolean;
begin
  Result := (Other.Button     = self.Button)
        and (Other.ClickCount = self.ClickCount)
        and (Other.ClickDir   = self.ClickDir)
        and (Other.Shift      = self.Shift)
        and (Other.ShiftMask  = self.ShiftMask)
        and (Other.Priority   = self.Priority)
        and ((Other.Command   = self.Command) or IgnoreCmd)
        and ((Other.Option    = self.Option) or IgnoreCmd)
        and ((Other.MoveCaret = self.MoveCaret) or IgnoreCmd);
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
        Delete(i);
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
  const AMoveCaret: Boolean; const AButton: TMouseButton;
  const AClickCount: TSynMAClickCount; const ADir: TSynMAClickDir;
  const AShift, AShiftMask: TShiftState; const AOpt: TSynEditorMouseCommandOpt = 0;
  const APrior: Integer = 0);
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
      Shift := AShift;
      ShiftMask := AShiftMask;
      Option := AOpt;
      Priority := APrior;
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

{ TSynEditSelMouseActions }

procedure TSynEditMouseSelActions.ResetDefaults;
begin
  Clear;
  AddCommand(emcStartDragMove, False, mbLeft, ccSingle, cdDown, [], []);
end;

{ TSynEditMouseTextActions }

procedure TSynEditMouseTextActions.ResetDefaults;
begin
  Clear;
  AddCommand(emcStartSelections, True,    mbLeft, ccSingle, cdDown, [],        [ssShift, ssAlt], emcoSelectionStart);
  AddCommand(emcStartSelections, True, mbLeft, ccSingle, cdDown, [ssShift], [ssShift, ssAlt], emcoSelectionContinue);
  AddCommand(emcStartColumnSelections, True,    mbLeft, ccSingle, cdDown, [ssAlt],          [ssShift, ssAlt], emcoSelectionStart);
  AddCommand(emcStartColumnSelections, True, mbLeft, ccSingle, cdDown, [ssShift, ssAlt], [ssShift, ssAlt], emcoSelectionContinue);
  AddCommand(emcContextMenu, False, mbRight, ccSingle, cdUp, [], [], emcoSelectionCaretMoveNever);

  AddCommand(emcSelectWord, True, mbLeft, ccDouble, cdDown, [], []);
  AddCommand(emcSelectLine, True, mbLeft, ccTriple, cdDown, [], []);
  AddCommand(emcSelectPara, True, mbLeft, ccQuad, cdDown, [], []);

  AddCommand(emcPasteSelection, True, mbMiddle, ccSingle, cdDown, [], []);

  AddCommand(emcMouseLink, False, mbLeft, ccSingle, cdUp, [SYNEDIT_LINK_MODIFIER], [ssShift, ssAlt, ssCtrl]);
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
  RegisterIntegerConsts(TypeInfo(TSynEditorMouseCommand), TIdentToInt(@IdentToSynMouseCmd), TIntToIdent(@SynMouseCmdToIdent));

end.

