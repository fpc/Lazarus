{%mainunit gtkwsstdctrls.pp}
{ $Id$}
{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

//////// Callbacks   //////
procedure GtkWS_GtkTextInserting(GtkEditable: PGtkEditable; NewText: pgchar;
  NewTextLength: gint; position: pgint; TextInserted:
  PGtkTextInsertEvent); cdecl;
begin
  TextInserted^(NewText, NewTextLength, position);
end;

procedure GtkWS_GtkTextDeleting(GtkEditable: PGtkEditable; StartPos,
  EndPos: gint; TextDeleted : PGtkTextDeleteEvent); cdecl;
begin
  TextDeleted^(StartPos, EndPos);
end;

//////// Code        //////

function TGtkMemoStrings.GetTextStr: string;
var
  AText: PgChar;
begin
  AText := gtk_editable_get_chars(PGtkEditable(FGtkText), 0,
    gtk_text_get_length(FGtkText));
  Result := StrPas(AText);
  g_free(AText);
end;

function TGtkMemoStrings.GetCount: integer;
begin
  Result := FCachedCount;
  if Get(FCachedCount-1) = '' then Dec(Result);
end;

function TGtkMemoStrings.Get(Index: Integer): string;
var
  LineStart,
  LineEnd: Integer;
  AText: PgChar;
begin
  LineStart := FLineStartPos[Index];
  if FCachedCount - 1 = Index then
    LineEnd := gtk_text_get_length(FGtkText)
  else
    LineEnd := FLineStartPos[Index + 1] - Length(LineEnding);

  if LineStart <> LineEnd then
  begin
    AText := gtk_editable_get_chars(@(FGtkText^.editable), LineStart, LineEnd);
    Result := StrPas(AText);
    g_free(AText);
  end
  else
    Result := ''
end;

procedure TGtkMemoStrings.AdjustIndices(FromIndex, ToIndex: Integer;
  ACount: Integer; Back: Boolean);
var
  X: Integer;
begin
  for X := FromIndex to ToIndex do begin;
    if Back then
      Dec(FLineStartPos[X], ACount)
    else Inc(FLineStartPos[X], ACount);
  end;
end;

procedure TGtkMemoStrings.InsertIndices(StartIndex: Integer;
  AValue: array of integer; ACount: Integer);
var
  X: Integer;
begin
  SetCachedCount(FCachedCount+ACount);
  System.Move(FLineStartPos[StartIndex],FLineStartPos[StartIndex+ACount],
              (FCachedCount-StartIndex-ACount)*SizeOf(Integer));
  for X := 0 to ACount-1 do begin
    FLineStartPos[StartIndex+X] := Avalue[X];
  end;
end;

procedure TGtkMemoStrings.DeleteIndices(StartIndex, ACount: Integer);
begin
  System.Move(FLineStartPos[StartIndex+ACount],FLineStartPos[StartIndex],
              (FCachedCount-StartIndex)*SizeOf(Integer));
  SetCachedCount(FCachedCount-ACount);
end;

procedure TGtkMemoStrings.SetCachedCount(AValue: Integer);
begin
  if AValue = FCachedCount then exit;
  if AValue >= FLineStartCapacity then begin
    while AValue >= FLineStartCapacity do Inc(FLineStartCapacity, 50);
    SetLength(FLineStartPos, FLineStartCapacity);
  end
  else if FLineStartCapacity-50 > AValue then begin
    Dec(FLineStartCapacity, 50);
    if FLineStartCapacity < 0 then FLineStartCapacity := 0;
    SetLength(FLineStartPos, FLineStartCapacity);
  end;
  FCachedCount := AValue;
end;

procedure TGtkMemoStrings.TextInserted(NewText: pgchar; NewTextLength: gint;
  position: pgint);
var
  LineIndices: array of Integer;
  LineIndicesCapacity: Integer;
  LineCount: Integer;
  X: Integer;
  StartLine: Integer;
  RealStartLine: Integer;
begin
  if NewTextLength = 0 then exit;
  fDeleting := False;
  //If (FCachedCount = 0) and (NewTextLength > 0) then SetCachedCount(1);
  LineCount := 0;
  LineIndicesCapacity := 0;
  for X := 0 to NewTextLength-1 do begin
    if NewText[X] = #10 then begin
      if LineCount >= LineIndicesCapacity then begin
        Inc(LineIndicesCapacity, 50);
        SetLength(LineIndices, LineIndicesCapacity);
      end;
      LineIndices[LineCount] := Position^+X+1;
      Inc(LineCount);
    end;
  end;
  StartLine:= PositionToLine(Position^);
  RealStartLine := StartLine + Ord(Position^ >= FLineStartPos[StartLine]);

  if RealStartLine <= FCachedCount-1 then
    AdjustIndices(RealStartLine, FCachedCount, NewTextLength, False);

  if LineCount > 0 then
    InsertIndices(RealStartLine, LineIndices, LineCount);

  SetLength(LineIndices, 0);
  // detect lineendings and  Increase Count and Lines.InsertIndices(); and AdjustIndices();
  //debugln('TGtkMemoStrings.TextInserted ',DbgSName(Owner));
end;

procedure TGtkMemoStrings.TextDeleted(StartPos, EndPos: gint);
var
  ShrinkCount: Integer;
  StartLine,
  EndLine: Integer;
begin
  if EndPos - StartPos = 0 then Exit;
  fDeleting := True;
  ShrinkCount := 0;
  StartLine := PositionToLine(StartPos+1);
  EndLine := PositionToLine(EndPos+1, StartLine);
  ShrinkCount := EndLine - StartLine;

  if ShrinkCount > 0 then begin
    DeleteIndices(StartLine, ShrinkCount);
  end;
  AdjustIndices(StartLine, FCachedCount-1, EndPos-StartPos, True);
  
  //if (FCachedCount = 1) and (gtk_text_get_length(FGtkText)-(endpos-startpos) = 0) then SetCachedCount(0);
  // see if any Indices in the cache are within the array, shrink the array , adjust the indices
  fDeleting := False;

  //debugln('TGtkMemoStrings.TextDeleted ',DbgSName(Owner));
end;

function TGtkMemoStrings.PositionToLine(Position: Integer; StartLine: Integer = 0): Integer;
var
 X: Integer;
begin
 Result := 0;
 // TODO maybe make this work faster by moving by more than 1 Index at a time
 // although it doesn't seem slow....
 X := StartLine;
// if FCachedCount = 0 then Exit;

 while (FLineStartPos[X] < Position) and (X <= FCachedCount-1) do begin
   if (not fDeleting) and (X = FCachedCount-1) then Break;
   Inc(X);
 end;
 Result := X;
end;

procedure TGtkMemoStrings.SetUpdateState(Updating: Boolean);
begin
  inherited SetUpdateState(Updating);
  //DebugLn(['TGtkMemoStrings.SetUpdateState Updating=',Updating,' ',FGtkText<>nil,' FFreezed=',FFreezed]);
  if Updating and (FGtkText<>nil) and (not FFreezed) then begin
    gtk_text_freeze(FGtkText);
    FFreezed:=true;
  end;
  if (not Updating) and (FFreezed) then begin
    if FGtkText<>nil then
      gtk_text_thaw(FGtkText);
    FFreezed:=false;
  end;
end;

constructor TGtkMemoStrings.Create(GtkText: PGtkText; TheOwner: TWinControl);
begin
  inherited Create;
  FGtkText := GtkText;
  FOwner := TheOwner;
  FLineStartCapacity := 50;
  SetLength(FLineStartPos, FLineStartCapacity);
  FLineStartPos[0] := 0;
  FCachedCount := 1;
  fDeleting := False;
  fTextInsertEvent := @TextInserted;
  fTextDeleteEvent := @TextDeleted;
  g_signal_connect(PGtkObject(FGtkText), 'insert-text', TGtkSignalFunc(@GtkWS_GtkTextInserting), @fTextInsertEvent);
  g_signal_connect(PGtkObject(FGtkText), 'delete-text', TGtkSignalFunc(@GtkWS_GtkTextDeleting), @fTextDeleteEvent);
end;

destructor TGtkMemoStrings.Destroy;
begin
  SetLength(FLineStartPos, 0);
  inherited Destroy;
end;

procedure TGtkMemoStrings.Assign(Source: TPersistent);
begin
  if (Source=Self) or (Source=nil) then exit;
  if Source is TStrings then begin
    SetText(PChar(TStrings(Source).Text));
    exit;
  end;
  Inherited Assign(Source);
end;

procedure TGtkMemoStrings.Clear;
begin
  LockOnChange(PGtkObject(FGtkText),+1);

  gtk_editable_delete_text(PGtkEditable(FGtkText), 0, gtk_text_get_length(FGtkText));
  LockOnChange(PGtkObject(FGtkText),-1);
  //SetCachedCount(0);
  //debugln('TGtkMemoStrings.Clear ',DbgSName(Owner));
end;

procedure TGtkMemoStrings.Delete(Index: integer);
var
  LineStart,
  LineEnd: Integer;
begin
  if Index = 0 then
    LineStart := 0
  else
    LineStart := FLineStartPos[Index];
  if FCachedCount-1 = Index then begin
    LineEnd := gtk_text_get_length(FGtkText);
  end
  else begin
    LineEnd := FLineStartPos[Index+1];
  end;
  gtk_editable_delete_text(PGtkEditable(FGtkText), LineStart, LineEnd);
  //debugln('TGtkMemoStrings.Delete ',DbgSName(Owner));
end;

procedure TGtkMemoStrings.Insert(Index: integer; const S: string);
var
  LineStart: Integer;
  NewLine: String;
  LocalFreeze: Boolean;
begin
  LocalFreeze:=FGtkText^.freeze_count = 0;
  if LocalFreeze then
    gtk_text_freeze(FGtkText);

  LockOnChange(PGtkObject(FGtkText),+1);

  if Index < FCachedCount then begin
    //insert with LineEnding
    LineStart := FLineStartPos[Index];
    NewLine := S+LineEnding;
    gtk_editable_insert_text(PGtkEditable(FGtkText),PChar(NewLine), Length(NewLine), @LineStart);
  end
  else begin
    //append with a preceding and appending LineEnding
    LineStart := gtk_text_get_length(FGtkText);
    if (FCachedCount = Count) then
      NewLine := LineEnding+S+LineEnding
    else
      NewLine := S+LineEnding;
    gtk_editable_insert_text(PGtkEditable(FGtkText),PChar(NewLine), Length(NewLine), @LineStart);
  end;

  // when we are thawed out we will move the cursor
  if LocalFreeze then begin
    gtk_text_thaw(FGtkText);
    if (LineStart <> -1) then
      gtk_editable_set_position(PGtkEditable(FGtkText), LineStart);
  end;

  LockOnChange(PGtkObject(FGtkText),-1);
end;

procedure TGtkMemoStrings.SetText(TheText: PChar);
var
StartPos: Integer;
begin
  StartPos := 0;
  LockOnChange(PGtkObject(FGtkText),+1);
  Clear;
  if theText^<>#0 then
    gtk_editable_insert_text(PGtkEditable(FGtkText),TheText, Length(TheText), @StartPos);
  LockOnChange(PGtkObject(FGtkText),-1);
  //debugln('TGtkMemoStrings.SetText ',DbgSName(Owner),' "',TheText,'" ',dbgs(gtk_text_get_length(FGtkText)));
end;
