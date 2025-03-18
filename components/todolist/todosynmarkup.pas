unit TodoSynMarkup;

{$mode objfpc}{$H+}
{$inline off}

interface

uses
  Classes, SysUtils, Controls, Graphics,
  LazLoggerBase,
  // IdeIntf
  SrcEditorIntf, EditorSyntaxHighlighterDef,
  // LazEdit
  LazEditMiscProcs,
  // SynEdit
  SynEditMarkup, SynHighlighterPas, SynEditMiscProcs, SynEdit, SynEditMiscClasses, SynEditTypes,
  SynEditHighlighter;

type

  { TSynEditTodoMarkup }

  TSynEditTodoMarkup = class(TSynEditMarkup)
  private type
    TCommentKind = (ckTodo, ckDone, ckNote);
    TFoundTodo = record
      StartPos: TLogPoint;
      EndPos: TLogPoint;
      Kind: TCommentKind;
    end;
  private
    FPasHl: TSynPasSyn;
    FLineLen: integer;
    FFoundPos: array of TFoundTodo;
    FSkipStartLine, FSkipEndLine: integer;
    FNxtIdx, FMrkIdx: integer;
  public
    procedure BeginMarkup; override;
    procedure PrepareMarkupForRow(aRow: Integer); override;
    procedure GetNextMarkupColAfterRowCol(const aRow: Integer;
      const aStartCol: TLazSynDisplayTokenBound; const AnRtlInfo: TLazSynDisplayRtlInfo; out
      ANextPhys, ANextLog: Integer); override;
    function GetMarkupAttributeAtRowCol(const aRow: Integer;
      const aStartCol: TLazSynDisplayTokenBound; const AnRtlInfo: TLazSynDisplayRtlInfo
      ): TSynSelectedColor; override;
    function GetMarkupAttributeAtWrapEnd(const aRow: Integer;
      const aWrapCol: TLazSynDisplayTokenBound): TSynSelectedColor; override;
  end;

procedure Register;

implementation

type

  { TTodoEditorHandler }

  TTodoEditorHandler = class
    procedure DoEditorCreated(Sender: TObject);
    procedure DoColorsChanged(Sender: TObject);
  end;

resourcestring
  AttribGroupName = 'Todo comments';
  AttribNameTodo  = 'Todo comment';
  AttribNameDone  = 'Done comment';
  AttribNameNote  = 'Note comment';

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
  CommentAttribTodo := TSynSelectedColor.Create('', '');
  CommentAttribDone := TSynSelectedColor.Create('', '');
  CommentAttribNote := TSynSelectedColor.Create('', '');
  CommentAttribTodo.Clear;
  CommentAttribDone.Clear;
  CommentAttribNote.Clear;

  IdeColorSchemeList.RegisterChangedHandler(@TTodoEditorHandler(nil).DoColorsChanged);

  // Register colors before the IDE loads them

  pas := IdeSyntaxHighlighters.GetIdForLazSyntaxHighlighter(lshFreePascal);
  AttribGroupIdx := IdeColorSchemeList.RegisterAttributeGroup(@AttribGroupName);

  IdeColorSchemeList.AddAttribute(AttribGroupIdx, pas, 'LazTodoListCommentTodo', @AttribNameTodo, [hafBackColor..hafFrameEdges]);
  IdeColorSchemeList.AddAttribute(AttribGroupIdx, pas, 'LazTodoListCommentDone', @AttribNameDone, [hafBackColor..hafFrameEdges]);
  IdeColorSchemeList.AddAttribute(AttribGroupIdx, pas, 'LazTodoListCommentNote', @AttribNameNote, [hafBackColor..hafFrameEdges]);
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

{ TSynEditTodoMarkup }

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

  function GetLine(ALineNum: integer): boolean; inline;
  begin
    Result := False;
    if ALineNum >= SynEdit.Lines.Count then
      exit;
    Result := True;
    LineText := SynEdit.Lines[ToIdx(ALineNum)];
    p := PChar(LineText);
    pe := p + Length(LineText);
  end;
  function AdvanceToNextLine(var ALineNum: integer): boolean; inline;
  begin
    repeat
      inc(ALineNum);
      Result := GetLine(ALineNum);
    until (not Result) or (LineText <> '');
  end;

  function StartsWithContinuedComment(ALineNum: integer): boolean; inline;
  begin
    FPasHl.StartAtLineIndex(ToIdx(ALineNum));
    Result := FPasHl.GetTokenIsComment and
              not FPasHl.GetTokenIsCommentStart(True);
  end;

  function GetStartOfComment(var ALineNum: integer; out ALogX: integer): boolean; inline;
  var
    IsComment, IsNewCommentStart: Boolean;
  begin
    ALogX := 1;
    FPasHl.StartAtLineIndex(ToIdx(ALineNum));
    Result := FPasHl.GetTokenIsComment;
    if (not Result) or
       (FPasHl.GetTokenIsCommentStart(True))
    then
      exit;

    while ALineNum > 1 do begin
      dec(ALineNum);
      if ALineNum = FSkipEndLine then
        exit(False);

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
    end;
    Result := False;
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
  LogX, TkEnd: integer;
  Kind: TCommentKind;
  pos: TPoint;
  i: SizeInt;
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
      TkEnd := 1 + FPasHl.GetTokenLen;
      FFoundPos[0].EndPos.Y := aRow;
      FFoundPos[0].EndPos.X := TkEnd;
      LogX := TkEnd;
    end
    else
      FFoundPos := nil;
  end
  else
    FFoundPos := nil;

  if (FFoundPos = nil) and (not GetStartOfComment(curRow, LogX)) then begin
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
            Kind := ckTodo;
            fnd := (p[1] in ['o', 'O']) and (p[2] in ['d', 'D']) and (p[3] in ['o', 'O']);
          end;
        'd', 'D': begin
            Kind := ckDone;
            fnd := (p[1] in ['o', 'O']) and (p[2] in ['n', 'N']) and (p[3] in ['e', 'E']);
          end;
        'n', 'N': begin
            Kind := ckNote;
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
          else
          begin
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
      if (ToPos(FPasHl.GetTokenPos) <> StartPos.x) or (not FPasHl.GetTokenIsCommentStart(True)) then begin
        MaybeSetSkipRows;
        Continue;
      end;
    end;

    if curRow < aRow then begin
      curRow := aRow;
      GetLine(curRow);
    end;
    pos.Y := curRow;
    pos.x := 1;
    if StartPos.Y = curRow then
      pos.X := StartPos.x;

    if (StartPos.Y <> aRow) or
       (StartPos.Y <> pos.Y) or (StartPos.X > pos.X)
    then
      FPasHl.StartAtLineIndex(ToIdx(pos.Y));
    FPasHl.NextToLogX(pos.X);
    TkEnd := ToPos(FPasHl.GetTokenPos) + FPasHl.GetTokenLen;

    i := Length(FFoundPos);
    SetLength(FFoundPos, i + 1);
    FFoundPos[i].StartPos := StartPos;
    FFoundPos[i].EndPos.y := curRow;
    FFoundPos[i].EndPos.x := TkEnd;
    FFoundPos[i].Kind     := Kind;

    p := PChar(LineText) + TkEnd - 1;
  until (p >= pe) or (curRow > aRow);

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
  ): TSynSelectedColor;
begin
  Result := nil;
  if aStartCol.Logical > FLineLen then
    exit;
  while (Result = nil) and (FMrkIdx < Length(FFoundPos)) do begin
    if (FFoundPos[FMrkIdx].StartPos.X > aStartCol.Logical) and
       (FFoundPos[FMrkIdx].StartPos.Y = aRow)
    then
      exit;
    if (FFoundPos[FMrkIdx].EndPos.X > aStartCol.Logical) or
       (FFoundPos[FMrkIdx].EndPos.y > aRow)
    then begin
      case FFoundPos[FMrkIdx].Kind of
        ckTodo: Result := CommentAttribTodo;
        ckDone: Result := CommentAttribDone;
        else    Result := CommentAttribNote;
      end;
      exit;
    end;

    inc(FMrkIdx);
  end;
end;

function TSynEditTodoMarkup.GetMarkupAttributeAtWrapEnd(const aRow: Integer;
  const aWrapCol: TLazSynDisplayTokenBound): TSynSelectedColor;
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
       (FFoundPos[FMrkIdx].EndPos.y > aRow)
    then begin
      case FFoundPos[FMrkIdx].Kind of
        ckTodo: Result := CommentAttribTodo;
        ckDone: Result := CommentAttribDone;
        else    Result := CommentAttribNote;
      end;
      exit;
    end;

    inc(FMrkIdx);
  end;
end;

initialization
  RegisterAttribs;
finalization
  FreeAttribs;
end.

