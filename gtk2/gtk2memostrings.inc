{$IFDEF MEMOHEADER}

type

  { TGtk2MemoStrings }

  TGtk2MemoStrings = class(TStrings)
  private
    FGtkText : PGtkTextView;
    FGtkBuf: PGtkTextBuffer;
    FTimerMove: guint;
    FTimerSel: guint;
    FOwner: TWinControl;
    FQueueCursorMove: Integer;
    FQueueSelLength: Integer;
  protected
    function GetTextStr: string; override;
    function GetCount: integer; override;
    function Get(Index : Integer) : string; override;
    //procedure PutObject(Index: Integer; AObject: TObject); override;
    //function GetObject(Index: Integer): TObject; override;
    //procedure SetSorted(Val : boolean); virtual;
  public
    constructor Create(TextView : PGtkTextView; TheOwner: TWinControl);
    destructor Destroy; override;
    procedure Assign(Source : TPersistent); override;
    procedure AddStrings(TheStrings: TStrings); override;
    procedure Clear; override;
    procedure Delete(Index : integer); override;
    procedure Insert(Index : integer; const S: string); override;
    procedure SetTextStr(const Value: string); override;
    //procedure Sort; virtual;
    procedure QueueCursorMove(APosition: Integer);
    procedure QueueSelectLength(ALength: Integer);
  public
    //property Sorted: boolean read FSorted write SetSorted;
    property Owner: TWinControl read FOwner;
    property QueueSelLength: Integer read FQueueSelLength;
  end;
{$ELSE}
{

Implementation

}

function UpdateMemoCursorCB(AStrings: TGtk2MemoStrings): gboolean; cdecl;
var
  TextMark: PGtkTextMark;
  CursorIter: TGtkTextIter;
begin
  Result := gtk_false; // stop this timer

  if AStrings.FQueueCursorMove = -1 then
  begin
    // always scroll so the cursor is visible
    TextMark := gtk_text_buffer_get_insert(AStrings.FGtkBuf);
    gtk_text_buffer_get_iter_at_mark(AStrings.FGtkBuf, @CursorIter, TextMark);
  end
  else begin
    // SelStart was used and we should move to that location
    gtk_text_buffer_get_iter_at_offset(AStrings.FGtkBuf, @CursorIter, AStrings.FQueueCursorMove);
    gtk_text_buffer_place_cursor(AStrings.FGtkBuf, @CursorIter); // needed to move the cursor
  end;
  gtk_text_view_scroll_to_iter(AStrings.FGtkText, @CursorIter, 0, False, 0, 0);

  AStrings.FQueueCursorMove := 0;
end;

function UpdateMemoSelLengthCB(AStrings: TGtk2MemoStrings): gboolean; cdecl;
var
  TextMark: PGtkTextMark;
  StartIter,
  EndIter: TGtkTextIter;
  Offset: Integer;
begin
  Result := gtk_false; // stop this timer

  TextMark := gtk_text_buffer_get_insert(AStrings.FGtkBuf);
  gtk_text_buffer_get_iter_at_mark(AStrings.FGtkBuf, @StartIter, TextMark);

  Offset := gtk_text_iter_get_offset(@StartIter);

  gtk_text_buffer_get_iter_at_offset(AStrings.FGtkBuf, @EndIter, Offset+AStrings.FQueueSelLength);

  gtk_text_buffer_select_range(AStrings.FGtkBuf, @StartIter, @EndIter);

  AStrings.FQueueSelLength := -1;
end;

function TGtk2MemoStrings.GetTextStr: string;
var
  StartIter, EndIter: TGtkTextIter;
  AText: PgChar;
begin
  Result := '';
  gtk_text_buffer_get_start_iter(FGtkBuf, @StartIter);
  gtk_text_buffer_get_end_iter(FGtkBuf, @EndIter);

  AText := gtk_text_iter_get_text(@StartIter, @EndIter);
  Result := StrPas(AText);
  if AText <> nil then
    g_free(AText);
end;

function TGtk2MemoStrings.GetCount: integer;
begin
  Result := gtk_text_buffer_get_line_count(FGtkBuf);
  if Get(Result-1) = '' then Dec(Result);
end;

function TGtk2MemoStrings.Get(Index: Integer): string;
var
  StartIter, EndIter: TGtkTextIter;
  AText: PgChar;
begin
  gtk_text_buffer_get_iter_at_line(FGtkBuf, @StartIter, Index);
  if Index = gtk_text_buffer_get_line_count(FGtkBuf) then
    gtk_text_buffer_get_end_iter(FGtkBuf, @EndIter)
  else begin
    gtk_text_buffer_get_iter_at_line(FGtkBuf, @EndIter, Index);
    gtk_text_iter_forward_to_line_end(@EndIter);
  end;
  // if a row is blank gtk_text_iter_forward_to_line_end will goto the row ahead
  // this is not desired. so if it jumped ahead a row then the row we want is blank
  if gtk_text_iter_get_line(@StartIter) = gtk_text_iter_get_line(@EndIter) then
  begin
    AText := gtk_text_iter_get_text(@StartIter, @EndIter);
    Result := StrPas(AText);
    g_free(AText);
  end
  else
    Result := '';
end;

constructor TGtk2MemoStrings.Create(TextView: PGtkTextView;
  TheOwner: TWinControl);
begin
  inherited Create;
  if TextView = nil then RaiseGDBException(
    'TGtk2MemoStrings.Create Unspecified Text widget');
  FGtkText:= TextView;
  FGtkBuf := gtk_text_view_get_buffer(FGtkText);
  if TheOwner = nil then RaiseGDBException(
    'TGtk2MemoStrings.Create Unspecified owner');
  FOwner:=TheOwner;
  FQueueSelLength := -1;
  FTimerMove := 0;
  FTimerSel := 0;
end;

destructor TGtk2MemoStrings.Destroy;
begin
  gtk_timeout_remove(FTimerSel);
  gtk_timeout_remove(FTimerMove);
  // don't destroy the widgets
  inherited Destroy;
end;

procedure TGtk2MemoStrings.Assign(Source: TPersistent);
var
  S: TStrings absolute Source;
begin
  if Source is TStrings then
  begin
    // to prevent Clear and then SetText we need to use our own Assign
    QuoteChar := S.QuoteChar;
    Delimiter := S.Delimiter;
    NameValueSeparator := S.NameValueSeparator;
    TextLineBreakStyle := S.TextLineBreakStyle;
    Text := S.Text;
  end
  else
    inherited Assign(Source);
end;

procedure TGtk2MemoStrings.AddStrings(TheStrings: TStrings);
begin
  SetTextStr(GetTextStr + TStrings(TheStrings).Text);
end;

procedure TGtk2MemoStrings.Clear;
begin
  SetText('');
end;

procedure TGtk2MemoStrings.Delete(Index: integer);
var
StartIter,
EndIter: TGtkTextIter;
begin
  gtk_text_buffer_get_iter_at_line(FGtkBuf, @StartIter, Index);
  if Index = Count-1 then begin
    gtk_text_iter_backward_char(@StartIter);
    gtk_text_buffer_get_end_iter(FGtkBuf, @EndIter)
  end
  else
    gtk_text_buffer_get_iter_at_line(FGtkBuf, @EndIter, Index+1);
  gtk_text_buffer_delete(FGtkBuf, @StartIter, @EndIter);
end;

procedure TGtk2MemoStrings.Insert(Index: integer; const S: string);
var
  StartIter,
  CursorIter: TGtkTextIter;
  NewLine: String;
  TextMark: PGtkTextMark;
begin
  if Index < gtk_text_buffer_get_line_count(FGtkBuf) then begin
    //insert with LineEnding
    NewLine := S+LineEnding;
    gtk_text_buffer_get_iter_at_line(FGtkBuf, @StartIter, Index);
  end
  else begin
    //append with a preceding LineEnding
    gtk_text_buffer_get_end_iter(FGtkBuf, @StartIter);
    if gtk_text_buffer_get_line_count(FGtkBuf) = Count then
      NewLine := LineEnding+S+LineEnding
    else
      NewLine := S+LineEnding;
  end;

  if FQueueCursorMove = 0 then
  begin
    TextMark := gtk_text_buffer_get_insert(FGtkBuf);
    gtk_text_buffer_get_iter_at_mark(FGtkBuf, @CursorIter, TextMark);
    if gtk_text_iter_equal(@StartIter, @CursorIter) then
      QueueCursorMove(-1);
  end;
  
  // and finally insert the new text
  gtk_text_buffer_insert(FGtkBuf, @StartIter, PChar(NewLine) ,-1);
end;

procedure TGtk2MemoStrings.SetTextStr(const Value: string);
begin
  if (Value <> Text) then
  begin
    LockOnChange(PGtkObject(Owner.Handle), 1);
    gtk_text_buffer_set_text(FGtkBuf, PChar(Value), -1);
  end;
end;

procedure TGtk2MemoStrings.QueueCursorMove(APosition: Integer);
begin
  // needed because there is a callback that updates the GtkBuffer
  // internally so that it actually knows where the cursor is
  if FQueueCursorMove = 0 then
    FTimerMove := gtk_timeout_add(0,TGSourceFunc(@UpdateMemoCursorCB), Pointer(Self));
  FQueueCursorMove := APosition;
end;

procedure TGtk2MemoStrings.QueueSelectLength(ALength: Integer);
begin
  // needed because there is a callback that updates the GtkBuffer
  // internally so that it actually knows where the cursor is
  if FQueueSelLength = -1 then
    FTimerSel := gtk_timeout_add(0,TGSourceFunc(@UpdateMemoSelLengthCB), Pointer(Self));
  FQueueSelLength := ALength;
end;

{$ENDIF}
