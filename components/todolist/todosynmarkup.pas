unit TodoSynMarkup;

{$mode objfpc}{$H+}
{$inline off}

interface

uses
  Classes, SysUtils, Controls, Graphics,
  LazLoggerBase,
  // TodoList
  ToDoListStrConsts, ToDoListCore,
  // IdeIntf
  SrcEditorIntf, EditorSyntaxHighlighterDef,
  // LazEdit
  LazEditMiscProcs, LazEditTextAttributes,
  // SynEdit
  SynEditMarkup, SynHighlighterPas, SynEditMiscProcs, SynEdit, SynEditMiscClasses, SynEditTypes,
  SynEditHighlighter, SynEditHighlighterFoldBase;

type
  TFoundTodo = record
    StartPos: TLogPoint;
    EndPos: TLogPoint;
    Kind: TToDoType;
  end;

  { TSynEditTodoMarkup }

  TSynEditTodoMarkup = class(TSynEditMarkup)
  private
    FPasHl: TSynPasSyn;
    FLineLen: integer;
    FFoundPos: array of TFoundTodo;
    FSkipStartLine, FSkipEndLine: integer;
    FNxtIdx, FMrkIdx: integer;
    function GetStartOfComment(var ALineNum: integer; out ALogX: integer): boolean;
    function MarkupFor(AKind: TToDoType): TLazEditTextAttributeModifier;
    function StartsWithContinuedComment(ALineNum: integer): boolean;
    function TokenEndX(aKind: TToDoType): integer;
  public
    procedure BeginMarkup; override;
    procedure PrepareMarkupForRow(aRow: Integer); override;
    procedure GetNextMarkupColAfterRowCol(const aRow: Integer;
      const aStartCol: TLazSynDisplayTokenBound; const AnRtlInfo: TLazSynDisplayRtlInfo; out
      ANextPhys, ANextLog: Integer); override;
    function GetMarkupAttributeAtRowCol(const aRow: Integer;
      const aStartCol: TLazSynDisplayTokenBound; const AnRtlInfo: TLazSynDisplayRtlInfo
      ): TLazEditTextAttributeModifier; override;
    function GetMarkupAttributeAtWrapEnd(const aRow: Integer;
      const aWrapCol: TLazSynDisplayTokenBound): TLazEditTextAttributeModifier; override;
    function CursorInsideToDo(aSrcPos: TPoint; out aToDo: TFoundTodo): boolean;
  end;

procedure Register;

implementation

type

  { TTodoEditorHandler }

  TTodoEditorHandler = class
    procedure DoEditorCreated(Sender: TObject);
    procedure DoColorsChanged(Sender: TObject);
    procedure DoRegisterAttribs(Sender: TObject);
  end;

var
  AttribGroupIdx: Integer;
  CommentAttribTodo, CommentAttribDone, CommentAttribNote: TSynSelectedColor;

procedure Register;
begin
  SourceEditorManagerIntf.RegisterChangeEvent(semEditorCreate, @TTodoEditorHandler(nil).DoEditorCreated);
  SourceEditorManagerIntf.RegisterChangeEvent(semEditorCloned, @TTodoEditorHandler(nil).DoEditorCreated);
  TTodoEditorHandler(nil).DoColorsChanged(nil);
end;

procedure RegisterAttribs;
var
  pas: TIdeSyntaxHighlighterID;
begin
  CommentAttribTodo := TSynSelectedColor.Create('', '', [lafPastEOL]);
  CommentAttribDone := TSynSelectedColor.Create('', '', [lafPastEOL]);
  CommentAttribNote := TSynSelectedColor.Create('', '', [lafPastEOL]);
  CommentAttribTodo.Clear;
  CommentAttribDone.Clear;
  CommentAttribNote.Clear;
  CommentAttribTodo.Features := [lafPastEOL];
  CommentAttribDone.Features := [lafPastEOL];
  CommentAttribNote.Features := [lafPastEOL];
  CommentAttribTodo.InternalSaveDefaultValues;
  CommentAttribDone.InternalSaveDefaultValues;
  CommentAttribNote.InternalSaveDefaultValues;

  IdeColorSchemeList.RegisterChangedHandler(@TTodoEditorHandler(nil).DoColorsChanged);

  // Register colors before the IDE loads them

  pas := IdeSyntaxHighlighters.GetIdForLazSyntaxHighlighter(lshFreePascal);
  AttribGroupIdx := IdeColorSchemeList.RegisterAttributeGroup(@AttribGroupName);

  IdeColorSchemeList.AddAttribute(AttribGroupIdx, pas, 'LazTodoListCommentTodo', @AttribNameTodo, [hafBackColor..hafFrameEdges], CommentAttribTodo);
  IdeColorSchemeList.AddAttribute(AttribGroupIdx, pas, 'LazTodoListCommentDone', @AttribNameDone, [hafBackColor..hafFrameEdges], CommentAttribDone);
  IdeColorSchemeList.AddAttribute(AttribGroupIdx, pas, 'LazTodoListCommentNote', @AttribNameNote, [hafBackColor..hafFrameEdges], CommentAttribNote);
end;

procedure FreeAttribs;
begin
  CommentAttribTodo.Free;
  CommentAttribDone.Free;
  CommentAttribNote.Free;
end;

{ TTodoEditorHandler }

procedure TTodoEditorHandler.DoEditorCreated(Sender: TObject);
var
  Syn: TSynEdit;
begin
  Syn := TSourceEditorInterface(Sender).EditorControl as TSynEdit;
  Syn.MarkupManager.AddMarkUp(TSynEditTodoMarkup.Create(Syn));
end;

procedure TTodoEditorHandler.DoColorsChanged(Sender: TObject);
var
  pas: TIdeSyntaxHighlighterID;
  cs: IColorScheme;
  csl: IColorSchemeLanguage;
  attr: IColorSchemeAttribute;
begin
  pas := IdeSyntaxHighlighters.GetIdForLazSyntaxHighlighter(lshFreePascal);
  cs := IdeColorSchemeList.GetCurrentSchemeForHighlighter(pas);
  csl := cs.GetLanguageForHighlighter(pas);
  attr := csl.GetAttributeIntf('LazTodoListCommentTodo');
  attr.ApplyTo(CommentAttribTodo);
  attr := csl.GetAttributeIntf('LazTodoListCommentDone');
  attr.ApplyTo(CommentAttribDone);
  attr := csl.GetAttributeIntf('LazTodoListCommentNote');
  attr.ApplyTo(CommentAttribNote);
end;

procedure TTodoEditorHandler.DoRegisterAttribs(Sender: TObject);
begin
  RegisterAttribs;
end;

{ TSynEditTodoMarkup }

function TSynEditTodoMarkup.MarkupFor(AKind: TToDoType): TLazEditTextAttributeModifier;
begin
  case AKind of
    tdTodo: Result := CommentAttribTodo;
    tdDone: Result := CommentAttribDone;
    else    Result := CommentAttribNote;
  end;
end;

function TSynEditTodoMarkup.StartsWithContinuedComment(ALineNum: integer): boolean;
begin
  FPasHl.StartAtLineIndex(ToIdx(ALineNum));
  Result := FPasHl.GetTokenIsComment and
            not FPasHl.GetTokenIsCommentStart(True);
end;

function TSynEditTodoMarkup.TokenEndX(aKind: TToDoType): integer;
var
  IsEnd: Boolean;
begin
  repeat
    Result := ToPos(FPasHl.GetTokenPos) + FPasHl.GetTokenLen;
    IsEnd := FPasHl.GetTokenIsCommentEnd;
    FPasHl.Next;
  until IsEnd or FPasHl.GetEol or not FPasHl.GetTokenIsComment;
  if FPasHl.GetEol and (not IsEnd) and (lafPastEOL in MarkupFor(aKind).Features) then
    Result := MaxInt; // past eol
end;

procedure TSynEditTodoMarkup.BeginMarkup;
begin
  FFoundPos := nil;
  FSkipStartLine := -1;
  inherited BeginMarkup;

  FPasHl := TSynPasSyn(TSynEdit(SynEdit).Highlighter);
  if (FPasHl <> nil) and not(TSynCustomHighlighter(FPasHl) is TSynPasSyn) then
    FPasHl := nil;

  if not (CommentAttribTodo.IsEnabled or
          CommentAttribDone.IsEnabled or
          CommentAttribNote.IsEnabled)
  then
    FPasHl := nil;
end;

procedure TSynEditTodoMarkup.PrepareMarkupForRow(aRow: Integer);
var
  LineText: String;
  p: PChar;
  pe: Pointer;

  function GetLine(ALineNum: integer): boolean;
  begin
    Result := False;
    if ALineNum >= SynEdit.Lines.Count then
      exit;
    Result := True;
    LineText := SynEdit.Lines[ToIdx(ALineNum)];
    p := PChar(LineText);
    pe := p + Length(LineText);
  end;

  function AdvanceToNextLine(var ALineNum: integer): boolean;
  begin
    repeat
      inc(ALineNum);
      Result := GetLine(ALineNum);
    until (not Result) or (LineText <> '');
  end;

var
  curRow: integer;
  StartPos: TPoint;

  procedure MaybeSetSkipRows;
  begin
    if curRow > aRow then begin
      FSkipStartLine := StartPos.Y + 1;
      FSkipEndLine := curRow - 1;
    end;
  end;

var
  fnd, firstRun, HasHash: Boolean;
  LogX, xx: integer;
  i: SizeInt;
  ToDoKind: TToDoType;
begin
  if FPasHl = nil then
    exit;

  FNxtIdx := 0;
  FMrkIdx := 0;
  FLineLen := Length(SynEdit.Lines[ToIdx(aRow)]);

  if (FSkipStartLine >=0) and (FSkipStartLine <= aRow) and (FSkipEndLine >= aRow) then begin
    FFoundPos := nil;
    exit;
  end;

  curRow := aRow;
  LogX := 1;
  i := Length(FFoundPos) - 1;
  if (i >= 0) and
     (FFoundPos[i].StartPos.Y < aRow) and
     (FFoundPos[i].EndPos.Y >= aRow - 1)
  then begin
    if (FFoundPos[i].EndPos.Y >= aRow) then begin
      if i > 0 then
        FFoundPos[0] := FFoundPos[i];
      SetLength(FFoundPos, 1);
      if FFoundPos[0].EndPos.Y > aRow then
        exit;
      LogX := FFoundPos[0].EndPos.X;
    end
    else
    if StartsWithContinuedComment(aRow) then begin
      if i > 0 then
        FFoundPos[0] := FFoundPos[i];
      SetLength(FFoundPos, 1);
      LogX := TokenEndX(FFoundPos[0].Kind);
      FFoundPos[0].EndPos.Y := aRow;
      FFoundPos[0].EndPos.X := LogX;
      if FPasHl.GetEol then
        exit;
    end
    else
      FFoundPos := nil;
  end
  else
    FFoundPos := nil;

  if (FFoundPos = nil) and not GetStartOfComment(curRow, LogX) then begin
    curRow := aRow;
    LogX := 1;
  end;
  GetLine(curRow);
  if LineText = '' then
    exit;

  p := p + LogX - 1;
  fnd := False;
  firstRun := True;
  repeat
    if (not firstRun) and (curRow < aRow) then begin
      // There was only one continuous comment in front of aRow => skip it;
      curRow := aRow;
      GetLine(curRow);
    end;
    firstRun := False;

    (* *** find potential comment start *** *)
    while p < pe do begin
      case p^ of
        '{': break;
        '(': if p[1] = '*' then break;
        '/': if p[1] = '/' then break;
      end;
      inc(p);
    end;
    if p >= pe then
      exit;

    StartPos.x := p - PChar(LineText) + 1;
    StartPos.y := curRow;
    case p^ of
      '(', '/': inc(p, 2);
      else      inc(p);
    end;

    (* ***  skip whitespace *** *)
    repeat
      p := p + CountLeadWhiteSpace(p);
      if p >= pe then begin
        if not AdvanceToNextLine(curRow) then begin
          MaybeSetSkipRows;
          exit;
        end;
        continue; // continue skip whitespace for new line
      end;
      break;
    until false;

    (* ***  skip hash *** *)
    HasHash := (p+5 < pe) and (p^ = '#'); // hash must be on same line as keyword
    if HasHash then
      inc(p);

    (* ***  keyword *** *)
    if (p+4 <= pe) then begin
      case p^ of
        't', 'T': begin
            ToDoKind := tdTodo;
            fnd := (p[1] in ['o', 'O']) and (p[2] in ['d', 'D']) and (p[3] in ['o', 'O']);
          end;
        'd', 'D': begin
            ToDoKind := tdDone;
            fnd := (p[1] in ['o', 'O']) and (p[2] in ['n', 'N']) and (p[3] in ['e', 'E']);
          end;
        'n', 'N': begin
            ToDoKind := tdNote;
            fnd := (p[1] in ['o', 'O']) and (p[2] in ['t', 'T']) and (p[3] in ['e', 'E']);
          end;
      end;
    end;
    if not fnd then begin
      MaybeSetSkipRows;
      Continue;
    end;
    fnd := False; // for next round, in case of several todo on one line
    inc(p, 4);
    if not ( (p = pe) or (p^ in [#9, #10, #13, ' ',':'])) then begin
      MaybeSetSkipRows;
      continue;
    end;

    (* ***  Check arguments / parse *** *)
    if not HasHash then begin
      while p^ <> ':' do begin
        while p^ in [#9, ' ', '0'..'9'] do
          inc(p);

        if p >= pe then begin
          if not AdvanceToNextLine(curRow) then begin
            MaybeSetSkipRows;
            break;
          end;
          continue; // continue search for colon
        end;

        if (p^ = '-') and (p[1] in ['o', 'O', 'c', 'C']) then begin
          if p[2] = '''' then begin
            inc(p, 3);
            while (p^ <> '''') do begin
              inc(p);
              if p >= pe then begin
                if not AdvanceToNextLine(curRow) then begin
                  MaybeSetSkipRows;
                  break;
                end;
                continue; // continue search for quote
              end;
            end;
            inc(p);
          end
          else begin
            inc(p, 2);
            while (p < pe) and not(p^ in [#9, ' ', ':']) do
              inc(p);
          end;
          continue; // continue search for colon
        end;

        break; // not allowed char
      end;
      if p^ <> ':' then begin
        MaybeSetSkipRows;
        continue;
      end;
    end;

    (* ***  found *** *)
    assert(StartPos.Y <= aRow, 'TSynEditTodoMarkup.PrepareMarkupForRow: StartPos.Y <= aRow');

    if (StartPos.Y = aRow) then begin
      FPasHl.StartAtLineIndex(ToIdx(StartPos.Y));
      FPasHl.NextToLogX(StartPos.X);
      if (ToPos(FPasHl.GetTokenPos) <> StartPos.x) or not FPasHl.GetTokenIsCommentStart(True) then
      begin
        MaybeSetSkipRows;
        Continue;
      end;
    end;

    if curRow < aRow then begin
      curRow := aRow;
      GetLine(curRow);
    end;
    if StartPos.Y = curRow then
      xx := StartPos.X
    else
      xx := 1;
    if (StartPos.Y <> aRow) or (StartPos.Y <> curRow) or (StartPos.X > xx) then
      FPasHl.StartAtLineIndex(ToIdx(curRow));
    FPasHl.NextToLogX(xx);
    xx := TokenEndX(ToDoKind);
    i := Length(FFoundPos);
    SetLength(FFoundPos, i + 1);
    FFoundPos[i].StartPos := StartPos;
    FFoundPos[i].EndPos.Y := curRow;
    FFoundPos[i].EndPos.X := xx;
    FFoundPos[i].Kind     := ToDoKind;

    p := PChar(LineText) + xx - 1;
  until (p >= pe) or (curRow > aRow);
end;

function TSynEditTodoMarkup.GetStartOfComment(var ALineNum: integer;
                                              out ALogX: integer): boolean;
var
  lvl: Integer;
  IsComment, IsNewCommentStart: Boolean;
begin
  ALogX := 1;
  FPasHl.StartAtLineIndex(ToIdx(ALineNum));
  Result := FPasHl.GetTokenIsComment and (ALineNum > 1);
  if (not Result) or FPasHl.GetTokenIsCommentStart(True) then
    exit;

  repeat
    dec(ALineNum);
    lvl := FPasHl.FoldBlockEndLevel(ToIdx(ALineNum), FOLDGROUP_PASCAL, [sfbIncludeDisabled]);
    while (ALineNum > 1) and
          (FPasHl.FoldBlockMinLevel(ToIdx(ALineNum), FOLDGROUP_PASCAL, [sfbIncludeDisabled]) >= lvl)
    do begin
      dec(ALineNum);
      if ALineNum = FSkipEndLine then
        exit(False);
    end;

    // Get ALogX and check for nested comment
    FPasHl.StartAtLineIndex(ToIdx(ALineNum));
    IsComment := FPasHl.GetTokenIsComment;
    // if not a comment, then pretend its a new start => used for "result"
    IsNewCommentStart := (not IsComment) or FPasHl.GetTokenIsCommentStart(True);
    if IsComment and (not IsNewCommentStart) and FPasHl.GetEol then
      continue;

    Result := IsNewCommentStart;
    while not FPasHl.GetEol do begin
      if not IsComment then begin
        ALogX := ToPos(FPasHl.GetTokenPos);
        Result := True;
      end;
      FPasHl.Next;
      IsComment := FPasHl.GetTokenIsComment;
    end;
    if Result then
      exit;
  until false;
  Result := False;
end;

function TSynEditTodoMarkup.CursorInsideToDo(aSrcPos: TPoint; out aToDo: TFoundTodo): boolean;
begin
  Result := False;
  FFoundPos := nil;
  FSkipStartLine := -1;
  FPasHl := TSynPasSyn(TSynEdit(SynEdit).Highlighter);
  if (FPasHl <> nil) and not(TSynCustomHighlighter(FPasHl) is TSynPasSyn) then exit;

  PrepareMarkupForRow(aSrcPos.Y);
  if Length(FFoundPos) > 0 then begin
    aToDo := FFoundPos[0];
    debugln(['TSynEditTodoMarkup.CursorInsideToDo Start=',aToDo.StartPos.X,':',aToDo.StartPos.Y,
             ', End=',aToDo.EndPos.X,':',aToDo.EndPos.Y, ', aSrcPos=',aSrcPos.X,':',aSrcPos.Y]);
    //Example inside a single line ToDo comment:
    //  Start=7:28, End=54:28, aSrcPos=22:28
    //Example inside a 3 line ToDo comment:
    //  Start=2:6, End=2147483647:6, aSrcPos=16:6
    //  Start=2:6, End=2147483647:7, aSrcPos=9:7
    //  Start=2:6, End=3:8, aSrcPos=2:8
    Assert(aSrcPos.Y = aToDo.EndPos.Y, 'TSynEditTodoMarkup.CursorInsideToDo: aSrcPos.Y <> aToDo.EndPos.Y');
    Result := (aSrcPos.X < aToDo.EndPos.X) and (
          // First line
          ((aSrcPos.Y = aToDo.StartPos.Y) and (aSrcPos.X > aToDo.StartPos.X))
        or (aSrcPos.Y > aToDo.StartPos.Y) // The last line of multiline
      );
    if Result and (aToDo.EndPos.X = MaxInt) then begin
      // We are inside a multiline comment but not on its last line.
      // Iterate to the last line and update EndPos.
      repeat
        Inc(aSrcPos.Y);
        FFoundPos := nil;
        PrepareMarkupForRow(aSrcPos.Y);
        Assert(Length(FFoundPos) > 0, 'TSynEditTodoMarkup.CursorInsideToDo: FFoundPos is empty');
      until FFoundPos[0].EndPos.X <> MaxInt;
      aToDo.EndPos := FFoundPos[0].EndPos;   // This is the real end of comment.
    end;
  end
  else begin
    aToDo.StartPos := Point(0,0);
    aToDo.EndPos := Point(0,0);
    aToDo.Kind := tdToDo;
  end;
end;

procedure TSynEditTodoMarkup.GetNextMarkupColAfterRowCol(const aRow: Integer;
  const aStartCol: TLazSynDisplayTokenBound; const AnRtlInfo: TLazSynDisplayRtlInfo; out
  ANextPhys, ANextLog: Integer);
begin
  ANextLog := -1;
  ANextPhys := -1;
  if (Length(FFoundPos) > 0) and (aStartCol.Logical < FLineLen + 1) then
    ANextLog := FLineLen + 1;
  while (FNxtIdx < Length(FFoundPos)) do begin
    if (FFoundPos[FNxtIdx].StartPos.X > aStartCol.Logical) then begin
      ANextLog := FFoundPos[FNxtIdx].StartPos.X;
      exit;
    end
    else
    if (FFoundPos[FNxtIdx].EndPos.X > aStartCol.Logical) and
       (FFoundPos[FNxtIdx].EndPos.X <> MaxInt) and
       (FFoundPos[FNxtIdx].EndPos.y = aRow)
    then begin
      ANextLog := FFoundPos[FNxtIdx].EndPos.X;
      exit;
    end;
    inc(FNxtIdx);
  end;
end;

function TSynEditTodoMarkup.GetMarkupAttributeAtRowCol(const aRow: Integer;
  const aStartCol: TLazSynDisplayTokenBound; const AnRtlInfo: TLazSynDisplayRtlInfo
  ): TLazEditTextAttributeModifier;
begin
  Result := nil;
  while (Result = nil) and (FMrkIdx < Length(FFoundPos)) do begin
    if (FFoundPos[FMrkIdx].StartPos.X > aStartCol.Logical) and
       (FFoundPos[FMrkIdx].StartPos.Y = aRow)
    then
      exit;
    if (FFoundPos[FMrkIdx].EndPos.X > aStartCol.Logical) or
       (FFoundPos[FMrkIdx].EndPos.X = MaxInt) or
       (FFoundPos[FMrkIdx].EndPos.y > aRow)
    then begin
      Result := MarkupFor(FFoundPos[FMrkIdx].Kind);
      if (FFoundPos[FMrkIdx].EndPos.y > aRow) and
         not (lafPastEOL in MarkupFor(FFoundPos[FMrkIdx].Kind).Features)
      then
        Result := nil;
      exit;
    end;

    inc(FMrkIdx);
  end;
end;

function TSynEditTodoMarkup.GetMarkupAttributeAtWrapEnd(const aRow: Integer;
  const aWrapCol: TLazSynDisplayTokenBound): TLazEditTextAttributeModifier;
begin
  Result := nil;
  if aWrapCol.Logical > FLineLen then
    exit;
  while (Result = nil) and (FMrkIdx < Length(FFoundPos)) do begin
    if (FFoundPos[FMrkIdx].StartPos.X > aWrapCol.Logical) and
       (FFoundPos[FMrkIdx].StartPos.Y = aRow)
    then
      exit;
    if (FFoundPos[FMrkIdx].EndPos.X > aWrapCol.Logical) or
       (FFoundPos[FMrkIdx].EndPos.X = MaxInt) or
       (FFoundPos[FMrkIdx].EndPos.y > aRow)
    then begin
      Result := MarkupFor(FFoundPos[FMrkIdx].Kind);
      exit;
    end;

    inc(FMrkIdx);
  end;
end;

initialization
  RegisterOnIdeColorSchemeListCreated(@TTodoEditorHandler(nil).DoRegisterAttribs);
finalization
  FreeAttribs;
end.

