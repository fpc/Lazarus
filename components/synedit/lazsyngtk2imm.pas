unit LazSynGtk2IMM;

{ Input Method (IM) support for SynEdit on the GTK2 widgetset.

  Inline composition, modelled after LazSynImeFull (LazSynIMM, Windows):
  - The preedit text is kept apart from the "real" text: it lives in the
    buffer only while composing, is tracked by its own selection object
    (FImeBlockSelection), is never recorded in the undo/redo list and is shown
    with an IM markup (underline / target highlight) instead of the normal
    selection.
  - Only the committed text is a real edit (one undo group, ecImeStr).
  - Anything that would change caret, text or selection (commands, mouse,
    modifications by others, focus loss) first finishes the composition.

  It handles the LM_IM_COMPOSITION messages sent by the GTK2 interface
  (lcl/interfaces/gtk2/gtk2proc.pas):
    Message.WParam = GTK_IM_FLAG_START / _PREEDIT / _COMMIT / _END (+ _REPLACE, ignored)
    Message.LParam = PChar (UTF-8) for PREEDIT / COMMIT
    Message.Result := 0 for COMMIT means "inserted", the widgetset must not insert it again.

  The attributes of the preedit (im_context_PangoAttrList) are owned by the widgetset
  (see gtk_preedit_changed_cb). They are only read here, never fetched from the IM
  context again and never unref'ed.

  -dGTK2IMEDEBUG writes a trace of all IM messages and of what finished a composition. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, LCLType, Messages, LMessages, LazUTF8,
  Gtk2, Gtk2Int, Gtk2Proc, Gtk2Globals, Pango,
  {$IFDEF GTK2IMEDEBUG}LazLoggerBase,{$ENDIF}
  SynEditMiscClasses, SynEditPointClasses, SynEditMarkupSelection, SynEditMarkup,
  SynEditTypes, SynEditKeyCmds, LazSynEditText, SynEditTextBase, LazEditTextAttributes,
  LazSynIMMBase;

type

  { LazSynImeGtk2 }

  LazSynImeGtk2 = class(LazSynIme)
  private
    FImeBlockSelection, FImeBlockSelection2, FImeBlockSelection3: TSynEditSelection;
    FImeMarkupSelection, FImeMarkupSelection2, FImeMarkupSelection3: TSynEditMarkupSelection;
    FInImeMsg: Boolean;
    FStopping: Boolean;
    FCommitSeen: Boolean;
    FPreeditStr: string;
    FCandidatePt: TPoint;          // where the candidate window goes (editor pixels)
    FCandidatePtValid: Boolean;
    FUndoStamp1, FUndoStamp2: TSynEditUndoGroup;
    FNeedUndoOnCancel: Boolean;
    procedure SetImeTempText(const s: string);
    procedure StartComposition;
    procedure EndComposition;
    procedure ApplyPreeditAttrs(const s: string);
    procedure UpdateCaretAndCandidate(const s: string);
    procedure DoPreedit(const s: string);
    procedure DoCommit(const s: string);
    procedure InsertTyped(const s: string);
    procedure UpdateCandidatePos;
    procedure DoOnCommand(Sender: TObject; AfterProcessing: boolean; var Handled: boolean;
      var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer;
      HandlerData: pointer);
    procedure DoOnMouse(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure DoStatusChanged(Sender: TObject; Changes: TSynStatusChanges);
  protected
    procedure StopIme(Success: Boolean); override;
  public
    constructor Create(AOwner: TSynEditBase);
    destructor Destroy; override;
    procedure WMImeComposition(var Message: TMessage); override;
    procedure FocusKilled; override;
  end;

implementation

uses
  SynEdit;

{ LazSynImeGtk2 }

constructor LazSynImeGtk2.Create(AOwner: TSynEditBase);
begin
  inherited Create(AOwner);

  FImeBlockSelection := TSynEditSelection.Create(ViewedTextBuffer, False);
  FImeBlockSelection.InvalidateLinesMethod := @InvalidateLines;
  FImeBlockSelection2 := TSynEditSelection.Create(ViewedTextBuffer, False);
  FImeBlockSelection2.InvalidateLinesMethod := @InvalidateLines;
  FImeBlockSelection3 := TSynEditSelection.Create(ViewedTextBuffer, False);
  FImeBlockSelection3.InvalidateLinesMethod := @InvalidateLines;

  FImeMarkupSelection  := TSynEditMarkupSelection.Create(FriendEdit, FImeBlockSelection);
  FImeMarkupSelection2 := TSynEditMarkupSelection.Create(FriendEdit, FImeBlockSelection2);
  FImeMarkupSelection3 := TSynEditMarkupSelection.Create(FriendEdit, FImeBlockSelection3);

  TSynEditMarkupManager(MarkupMgr).AddMarkUp(FImeMarkupSelection);
  TSynEditMarkupManager(MarkupMgr).AddMarkUp(FImeMarkupSelection2);
  TSynEditMarkupManager(MarkupMgr).AddMarkUp(FImeMarkupSelection3);

  // whole preedit: dotted underline
  FImeMarkupSelection.MarkupInfo.Clear;
  FImeMarkupSelection.MarkupInfo.FramePriority := MaxInt-1;
  FImeMarkupSelection.MarkupInfo.FrameColor := clDefault;
  FImeMarkupSelection.MarkupInfo.FrameStyle := slsDotted;
  FImeMarkupSelection.MarkupInfo.FrameEdges := sfeBottom;
  FImeMarkupSelection.MarkupInfo.StylePriority[fsUnderline] := MaxInt;
  FImeMarkupSelection.MarkupInfo.Style := [];
  FImeMarkupSelection.MarkupInfo.StyleMask := [fsUnderline];

  // converted target: solid underline
  FImeMarkupSelection2.MarkupInfo.Clear;
  FImeMarkupSelection2.MarkupInfo.FramePriority := MaxInt;
  FImeMarkupSelection2.MarkupInfo.FrameColor := clDefault;
  FImeMarkupSelection2.MarkupInfo.FrameStyle := slsSolid;
  FImeMarkupSelection2.MarkupInfo.FrameEdges := sfeBottom;

  // highlighted target: looks like a selection
  FImeMarkupSelection3.MarkupInfo.Assign(FriendEdit.SelectedColor);

  FriendEdit.RegisterStatusChangedHandler(@DoStatusChanged, [scCaretX, scCaretY, scModified]);
  FriendEdit.RegisterCommandHandler(@DoOnCommand, nil, [hcfInit]);
  FriendEdit.RegisterBeforeMouseDownHandler(@DoOnMouse);
end;

destructor LazSynImeGtk2.Destroy;
begin
  FriendEdit.UnregisterBeforeMouseDownHandler(@DoOnMouse);
  FriendEdit.UnregisterCommandHandler(@DoOnCommand);
  FriendEdit.UnRegisterStatusChangedHandler(@DoStatusChanged);
  TSynEditMarkupManager(MarkupMgr).RemoveMarkUp(FImeMarkupSelection);
  TSynEditMarkupManager(MarkupMgr).RemoveMarkUp(FImeMarkupSelection2);
  TSynEditMarkupManager(MarkupMgr).RemoveMarkUp(FImeMarkupSelection3);

  FreeAndNil(FImeMarkupSelection);
  FreeAndNil(FImeMarkupSelection2);
  FreeAndNil(FImeMarkupSelection3);
  FreeAndNil(FImeBlockSelection);
  FreeAndNil(FImeBlockSelection2);
  FreeAndNil(FImeBlockSelection3);
  inherited Destroy;
end;

procedure LazSynImeGtk2.DoOnCommand(Sender: TObject; AfterProcessing: boolean;
  var Handled: boolean; var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer;
  HandlerData: pointer);
begin
  {$IFDEF GTK2IMEDEBUG}
  if FInCompose then DebugLn(['LazSynImeGtk2: composition finished by command ', Command]);
  {$ENDIF}
  StopIme(True);
end;

procedure LazSynImeGtk2.DoOnMouse(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  {$IFDEF GTK2IMEDEBUG}
  if FInCompose then DebugLn('LazSynImeGtk2: composition finished by mouse');
  {$ENDIF}
  StopIme(True);
end;

procedure LazSynImeGtk2.DoStatusChanged(Sender: TObject; Changes: TSynStatusChanges);
begin
  if FInImeMsg then
    exit;
  {$IFDEF GTK2IMEDEBUG}
  if FInCompose then DebugLn(['LazSynImeGtk2: composition finished by status change (caret/modified), changes=', Integer(Changes <> [])]);
  {$ENDIF}
  StopIme(True);
end;

procedure LazSynImeGtk2.SetImeTempText(const s: string);
var
  p1, p2: TPoint;
  f: Boolean;
begin
  p1 := FImeBlockSelection.FirstLineBytePos;

  f := FInImeMsg;
  FInImeMsg := True;
  // the preedit must not become part of the undo history
  ViewedTextBuffer.UndoList.Lock;
  ViewedTextBuffer.RedoList.Lock;
  FImeBlockSelection.SelText := s;
  ViewedTextBuffer.UndoList.Unlock;
  ViewedTextBuffer.RedoList.Unlock;
  FInImeMsg := f;

  p2 := FImeBlockSelection.FirstLineBytePos;
  FImeBlockSelection.StartLineBytePos := p1;
  FImeBlockSelection.EndLineBytePos := p2;
end;

procedure LazSynImeGtk2.UpdateCandidatePos;
var
  P: TPoint;
  Target: TObject;
begin
  if not FriendEdit.HandleAllocated then
    exit;
  // position of the candidate window: below the target clause / the caret
  if FCandidatePtValid then
    P := FCandidatePt
  else
    P := Point(FriendEdit.CaretXPix, FriendEdit.CaretYPix + FriendEdit.LineHeight);
  // IM_Context_Set_Cursor_Pos is relative to the IM client window: the widget that has the
  // focus and gets the key events. This may be another control, e.g. the code completion form.
  if im_context_widget <> nil then
  begin
    Target := GetLCLObject(im_context_widget);
    if (Target is TWinControl) and (Target <> FriendEdit) then
    begin
      P := FriendEdit.ClientToScreen(P);
      P := TWinControl(Target).ScreenToClient(P);
    end;
  end;
  {$IFDEF GTK2IMEDEBUG}
  DebugLn(['LazSynImeGtk2: candidate window at ', P.x, ',', P.y, ' (target widget ', GTK2WidgetSet.IMTarget <> nil, ')']);
  {$ENDIF}
  IM_Context_Set_Cursor_Pos(P.x, P.y);
end;

procedure LazSynImeGtk2.StartComposition;
begin
  if FInCompose then
    exit;

  // same as Windows (non deferred): the selection is overwritten when the composition starts
  if SelectionObj.SelAvail and (not SelectionObj.Persistent) and
     (eoOverwriteBlock in FriendEdit.Options2) then
  begin
    ViewedTextBuffer.UndoList.ForceGroupEnd;
    FUndoStamp1 := ViewedTextBuffer.UndoList.PeekItem;
    FriendEdit.BeginUpdate;
    ViewedTextBuffer.UndoList.CurrentReason := ecImeStr;
    SelectionObj.SelText := '';
    FriendEdit.EndUpdate;
    FUndoStamp2 := ViewedTextBuffer.UndoList.PeekItem;
    FNeedUndoOnCancel := FUndoStamp1 <> FUndoStamp2;
  end
  else
    FNeedUndoOnCancel := False;

  FImeMarkupSelection3.MarkupInfo.Assign(FriendEdit.SelectedColor);
  FImeBlockSelection.StartLineBytePos := CaretObj.LineBytePos;
  FImeBlockSelection2.StartLineBytePos := CaretObj.LineBytePos;
  FImeBlockSelection3.StartLineBytePos := CaretObj.LineBytePos;
  FPreeditStr := '';
  FCandidatePtValid := False;
  FInCompose := True;
  DoIMEStarted;
  UpdateCandidatePos;
end;

procedure LazSynImeGtk2.EndComposition;
begin
  if not FInCompose then
    exit;
  SetImeTempText('');
  CaretObj.LineBytePos := FImeBlockSelection.LastLineBytePos;
  if FNeedUndoOnCancel and (ViewedTextBuffer.UndoList.PeekItem = FUndoStamp2) then
    FriendEdit.Undo;
  FNeedUndoOnCancel := False;

  FImeBlockSelection.StartLineBytePos := CaretObj.LineBytePos;
  FImeBlockSelection2.StartLineBytePos := CaretObj.LineBytePos;
  FImeBlockSelection3.StartLineBytePos := CaretObj.LineBytePos;
  FPreeditStr := '';
  FCandidatePtValid := False;
  FInCompose := False;
  DoIMEEnded;
end;

procedure LazSynImeGtk2.ApplyPreeditAttrs(const s: string);
var
  attrs: PPangoAttrList;
  it: PPangoAttrIterator;
  a: PPangoAttribute;
  r1, r2, x, Guard: Integer;
  xy: TPoint;
  GotTarget, GotSel: Boolean;
begin
  // Owned by the widgetset: only read, do not unref, do not keep.
  attrs := im_context_PangoAttrList;
  if (attrs = nil) or (s = '') then
    exit;

  x := FImeBlockSelection.StartBytePos;   // pango indexes are byte offsets
  xy := FImeBlockSelection.StartLineBytePos;
  GotTarget := False;
  GotSel := False;
  Guard := 0;
  it := pango_attr_list_get_iterator(attrs);
  if it = nil then
    exit;
  try
    repeat
      pango_attr_iterator_range(it, @r1, @r2);
      if r2 > Length(s) then
        r2 := Length(s);
      if (r1 >= 0) and (r1 < r2) then
      begin
        a := pango_attr_iterator_get(it, PANGO_ATTR_BACKGROUND);
        if (a <> nil) and (not GotSel) then
        begin
          // highlighted clause (e.g. the segment being converted)
          xy.x := x + r1;
          FImeBlockSelection3.StartLineBytePos := xy;
          xy.x := x + r2;
          FImeBlockSelection3.EndLineBytePos := xy;
          GotSel := True;
        end
        else
        begin
          a := pango_attr_iterator_get(it, PANGO_ATTR_UNDERLINE);
          if (a <> nil) and (not GotTarget) and
             (PPangoAttrInt(a)^.value in [Ord(PANGO_UNDERLINE_DOUBLE)]) then
          begin
            xy.x := x + r1;
            FImeBlockSelection2.StartLineBytePos := xy;
            xy.x := x + r2;
            FImeBlockSelection2.EndLineBytePos := xy;
            GotTarget := True;
          end;
        end;
      end;
      Inc(Guard);
      // gboolean is a 32 bit value: compare it, do not use "not"
    until (pango_attr_iterator_next(it) = False) or (Guard > 1024);
  finally
    pango_attr_iterator_destroy(it);
  end;
end;

procedure LazSynImeGtk2.DoPreedit(const s: string);
var
  xy: TPoint;
begin
  if not FInCompose then
    StartComposition;

  FPreeditStr := s;
  SetImeTempText(s);

  xy := FImeBlockSelection.StartLineBytePos;
  FImeBlockSelection2.StartLineBytePos := xy;
  FImeBlockSelection2.EndLineBytePos := xy;
  FImeBlockSelection3.StartLineBytePos := xy;
  FImeBlockSelection3.EndLineBytePos := xy;

  try
    ApplyPreeditAttrs(s);
  except
    // the target highlight is only decoration: never lose the input because of it
    on E: Exception do
    begin
      {$IFDEF GTK2IMEDEBUG}
      DebugLn(['LazSynImeGtk2: preedit attributes: ', E.Message]);
      {$ENDIF}
      FImeBlockSelection2.StartLineBytePos := xy;
      FImeBlockSelection2.EndLineBytePos := xy;
      FImeBlockSelection3.StartLineBytePos := xy;
      FImeBlockSelection3.EndLineBytePos := xy;
    end;
  end;

  UpdateCaretAndCandidate(s);
  UpdateCandidatePos;
end;

procedure LazSynImeGtk2.UpdateCaretAndCandidate(const s: string);
var
  CursorPos: TPoint;
  Target: TSynEditSelection;
  CurChars, CharCount: Integer;
begin
  CharCount := UTF8Length(s);
  CurChars := CharCount;
  // cursor inside of the preedit (e.g. moved by the user between the clauses), in characters
  CurChars := im_context_curpos;
  if (CurChars < 0) or (CurChars > CharCount) then
    CurChars := CharCount;

  CursorPos := FImeBlockSelection.StartLineBytePos;
  CursorPos.x := CursorPos.x + Length(UTF8Copy(s, 1, CurChars));
  CaretObj.LineBytePos := CursorPos;

  FCandidatePtValid := False;
  if s = '' then
    exit;

  // The candidate window belongs to the clause that is converted (highlighted), if there is one.
  Target := nil;
  if FImeBlockSelection3.SelAvail then
    Target := FImeBlockSelection3
  else
  if FImeBlockSelection2.SelAvail then
    Target := FImeBlockSelection2;
  if Target = nil then
    exit;

  CaretObj.LineBytePos := Target.FirstLineBytePos;
  FCandidatePt := Point(FriendEdit.CaretXPix, FriendEdit.CaretYPix + FriendEdit.LineHeight);
  FCandidatePtValid := True;
  CaretObj.LineBytePos := CursorPos;
end;

procedure LazSynImeGtk2.DoCommit(const s: string);
var
  grp: Boolean;
begin
  FCommitSeen := True;

  if not FInCompose then
  begin
    if s <> '' then
      InsertTyped(s);
    exit;
  end;

  // The commit replaces the preedit. An empty commit only removes the preedit: the
  // owner of the real IM focus (e.g. the code completion form) inserts the text itself.
  SetImeTempText('');
  CaretObj.LineBytePos := FImeBlockSelection.StartLineBytePos;
  FPreeditStr := '';
  FCandidatePtValid := False;
  if s = '' then
    exit;
  grp := ViewedTextBuffer.UndoList.GroupUndo;
  ViewedTextBuffer.UndoList.GroupUndo := True;
  FriendEdit.BeginUpdate;
  try
    ViewedTextBuffer.UndoList.CurrentReason := ecImeStr;
    FImeBlockSelection.SelText := s;
    FImeBlockSelection.StartLineBytePos := FImeBlockSelection.EndLineBytePos;
    CaretObj.LineBytePos := FImeBlockSelection.StartLineBytePos;
    FNeedUndoOnCancel := False;
  finally
    FriendEdit.EndUpdate;
    ViewedTextBuffer.UndoList.GroupUndo := grp;
  end;
end;

procedure LazSynImeGtk2.InsertTyped(const s: string);
var
  p, Len, CharLen: Integer;
begin
  // commit without a composition (e.g. a key translated by the IM): same as typing
  p := 1;
  Len := Length(s);
  while p <= Len do
  begin
    CharLen := UTF8CodepointSize(@s[p]);
    if (CharLen < 1) or (p + CharLen - 1 > Len) then
      CharLen := 1;
    TCustomSynEdit(FriendEdit).CommandProcessor(ecChar, Copy(s, p, CharLen), nil);
    Inc(p, CharLen);
  end;
end;

procedure LazSynImeGtk2.WMImeComposition(var Message: TMessage);
var
  Flags: PtrUInt;
begin
  Flags := Message.WParam;
  Message.Result := -1;  // handled: the widgetset must not insert a commit string again
  {$IFDEF GTK2IMEDEBUG}
  DebugLn(['LazSynImeGtk2.WMImeComposition flags=', Flags, ' str="',
           string(PChar(Message.LParam)), '" InCompose=', FInCompose, ' ReadOnly=', FriendEdit.ReadOnly]);
  {$ENDIF}

  if FriendEdit.ReadOnly then
    Exit;

  FInImeMsg := True;
  try
    if (Flags and GTK_IM_FLAG_START) <> 0 then
      StartComposition;

    if (Flags and GTK_IM_FLAG_COMMIT) <> 0 then
      DoCommit(string(PChar(Message.LParam)))
    else
    if (Flags and GTK_IM_FLAG_PREEDIT) <> 0 then
      DoPreedit(string(PChar(Message.LParam)));

    if (Flags and GTK_IM_FLAG_END) <> 0 then
      EndComposition;
  finally
    FInImeMsg := False;
  end;
end;

procedure LazSynImeGtk2.StopIme(Success: Boolean);
var
  s: string;
begin
  if (not FInCompose) or FStopping then
    exit;
  FStopping := True;
  try
    s := FPreeditStr;
    FCommitSeen := False;
    FInImeMsg := True;
    try
      // The IM answers with commit / preedit-changed('') / preedit-end,
      // which are handled by WMImeComposition.
      ResetDefaultIMContext;
      // The IM may not have finished the composition by itself
      // (or it was done while the widget had no IM target).
      if FInCompose then
        EndComposition;
      if Success and (not FCommitSeen) and (s <> '') then
        InsertTyped(s);   // "complete": keep what was shown
    finally
      FInImeMsg := False;
    end;
  finally
    FStopping := False;
  end;
end;

procedure LazSynImeGtk2.FocusKilled;
begin
  {$IFDEF GTK2IMEDEBUG}
  if FInCompose then DebugLn('LazSynImeGtk2: composition finished by focus loss');
  {$ENDIF}
  StopIme(True);
end;

end.
