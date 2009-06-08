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
  Classes, Controls, SysUtils, SynEditStrConst, SynEditPointClasses, Dialogs;

const
  // EditorMouseCommands

  emcNone                     =  0;
  emcStartSelections          =  1;    // Start BlockSelection (Default Left Mouse Btn)
  emcStartColumnSelections    =  3;    // Column BlockSelection (Default Alt - Left Mouse Btn)
  emcStartLineSelections      =  4;    // Line BlockSelection (Default Alt - Left Mouse Btn)

  emcSelectWord               =  6;
  emcSelectLine               =  7;
  emcSelectPara               =  8;

  emcStartDragMove            =  9;
  emcPasteSelection           = 10;
  emcMouseLink                = 11;

  emcContextMenu              = 12;

  emcOnMainGutterClick        = 13;    // OnGutterClick

  emcCodeFoldCollaps          = 14;
  emcCodeFoldExpand           = 15;
  emcCodeFoldContextMenu      = 16;

  emcMax = 16;

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
  end;

  { TSynEditMouseAction }

  TSynEditMouseAction = class(TCollectionItem)
  private
    FClickDir: TSynMAClickDir;
    FOption: TSynEditorMouseCommandOpt;
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
    procedure SetShift(const AValue: TShiftState);
    procedure SetShiftMask(const AValue: TShiftState);
  protected
    function GetDisplayName: string; override;
  public
    procedure Assign(Source: TPersistent); override;
    function IsMatchingShiftState(AShift: TShiftState): Boolean;
    function IsMatchingClick(ABtn: TMouseButton; ACCount: TSynMAClickCount;
                             ACDir: TSynMAClickDir): Boolean;
    function IsFallback: Boolean;
    function Conflicts(Other: TSynEditMouseAction): Boolean;
    function Equals(Other: TSynEditMouseAction; IgnoreCmd: Boolean = False): Boolean;
  published
    property Shift: TShiftState read FShift write SetShift;
    property ShiftMask: TShiftState read FShiftMask write SetShiftMask;
    property Button: TMouseButton read FButton write SetButton;
    property ClickCount: TSynMAClickCount read FClickCount write SetClickCount;
    property ClickDir: TSynMAClickDir read FClickDir write SetClickDir;
    property Command: TSynEditorMouseCommand read FCommand write SetCommand;
    property MoveCaret: Boolean read FMoveCaret write SetMoveCaret;
    property Option: TSynEditorMouseCommandOpt read FOption write SetOption;
  end;

  TSynEditMouseActionHandler = function(AnAction: TSynEditMouseAction;
    AnInfo: TSynEditMouseActionInfo): Boolean of object;

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
    function FindCommand(AnInfo: TSynEditMouseActionInfo): TSynEditMouseAction;
    procedure ResetDefaults; virtual;
    procedure IncAssertLock;
    procedure DecAssertLock;
    function  IndexOf(MAction: TSynEditMouseAction;
                      IgnoreCmd: Boolean = False): Integer;
    procedure AddCommand(const ACmd: TSynEditorMouseCommand;
             const AMoveCaret: Boolean;
             const AButton: TMouseButton; const AClickCount: TSynMAClickCount;
             const ADir: TSynMAClickDir; const AShift, AShiftMask: TShiftState;
             const AOpt: TSynEditorMouseCommandOpt = 0);
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

  function MouseCommandName(emc: TSynEditorMouseCommand): String;
  function MouseCommandConfigName(emc: TSynEditorMouseCommand): String;

const
  SYNEDIT_LINK_MODIFIER = {$IFDEF LCLcarbon}ssMeta{$ELSE}ssCtrl{$ENDIF};

implementation

function MouseCommandName(emc: TSynEditorMouseCommand): String;
begin
  case emc of
    emcNone:    Result := SYNS_emcNone;
    emcStartSelections:    Result := SYNS_emcStartSelection;
    emcStartColumnSelections:    Result := SYNS_emcStartColumnSelections;
    emcStartLineSelections: Result := SYNS_emcStartLineSelections;
    emcSelectWord: Result := SYNS_emcSelectWord;
    emcSelectLine: Result := SYNS_emcSelectLine;
    emcSelectPara: Result := SYNS_emcSelectPara;
    emcStartDragMove:  Result := SYNS_emcStartDragMove;
    emcPasteSelection: Result := SYNS_emcPasteSelection;
    emcMouseLink:   Result := SYNS_emcMouseLink;
    emcContextMenu: Result := SYNS_emcContextMenu;

    emcOnMainGutterClick: Result := SYNS_emcBreakPointToggle;

    emcCodeFoldCollaps:     Result := SYNS_emcCodeFoldCollaps;
    emcCodeFoldExpand:      Result := SYNS_emcCodeFoldExpand;
    emcCodeFoldContextMenu: Result := SYNS_emcCodeFoldContextMenu;

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
    else Result := ''
  end;
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
    FOption     := TSynEditMouseAction(Source).Option;
  end else
    inherited Assign(Source);
  if Collection <> nil then
    TSynEditMouseActions(Collection).AssertNoConflict(self);
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
        and not(Other.IsFallback xor self.IsFallback);
end;

function TSynEditMouseAction.Equals(Other: TSynEditMouseAction;
  IgnoreCmd: Boolean = False): Boolean;
begin
  Result := (Other.Button     = self.Button)
        and (Other.ClickCount = self.ClickCount)
        and (Other.ClickDir   = self.ClickDir)
        and (Other.Shift      = self.Shift)
        and (Other.ShiftMask  = self.ShiftMask)
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
    for i := 0 to TSynEditMouseActions(Source).Count-1 do
      Add.Assign(TSynEditMouseActions(Source)[i]);
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

function TSynEditMouseActions.FindCommand(AnInfo: TSynEditMouseActionInfo): TSynEditMouseAction;
var
  i: Integer;
  act, fback: TSynEditMouseAction;
begin
  fback := nil;
  for i := 0 to Count-1 do begin
    act := Items[i];
    if act.IsMatchingClick(AnInfo.Button, AnInfo.CCount, AnInfo.Dir) and
       act.IsMatchingShiftState(AnInfo.Shift)
    then begin
      if act.IsFallback then
        fback := act
      else
        exit(act);
    end;
  end;
  if fback <> nil then
    exit(fback);
  Result := nil;
end;

procedure TSynEditMouseActions.AddCommand(const ACmd: TSynEditorMouseCommand;
  const AMoveCaret: Boolean; const AButton: TMouseButton;
  const AClickCount: TSynMAClickCount; const ADir: TSynMAClickDir;
  const AShift, AShiftMask: TShiftState; const AOpt: TSynEditorMouseCommandOpt = 0);
var
  new: TSynEditMouseAction;
begin
  new := Add;
  try
    inc(FAssertLock);
    with new do begin
      Command := ACmd;
      MoveCaret := AMoveCaret;
      Button := AButton;
      ClickCount := AClickCount;
      ClickDir := ADir;
      Shift := AShift;
      ShiftMask := AShiftMask;
      Option := AOpt;
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
  AddCommand(emcContextMenu, False, mbRight, ccSingle, cdUp, [], []);

  AddCommand(emcSelectWord, True, mbLeft, ccDouble, cdDown, [], []);
  AddCommand(emcSelectLine, True, mbLeft, ccTriple, cdDown, [], []);
  AddCommand(emcSelectPara, True, mbLeft, ccQuad, cdDown, [], []);

  AddCommand(emcPasteSelection, True, mbMiddle, ccSingle, cdDown, [], []);

  AddCommand(emcMouseLink, False, mbLeft, ccSingle, cdUp, [SYNEDIT_LINK_MODIFIER], [ssShift, ssAlt, ssCtrl]);
end;

end.

