{$IFDEF MEMOHEADER}

type

  { TGtk2MemoStrings }

  TGtk2MemoStrings = class(TStrings)
  private
    FGtkText : PGtkTextView;
    FGtkBuf: PGtkTextBuffer;
    FOwner: TWinControl;
  protected
    function GetTextStr: string; override;
    function GetCount: integer; override;
    function Get(Index : Integer) : string; override;
    //procedure SetSorted(Val : boolean); virtual;
  public
    constructor Create(TextView : PGtkTextView; TheOwner: TWinControl);
    destructor Destroy; override;
    procedure Assign(Source : TPersistent); override;
    procedure Clear; override;
    procedure Delete(Index : integer); override;
    procedure Insert(Index : integer; const S: string); override;
    procedure SetText(TheText: PChar); override;
    //procedure Sort; virtual;
  public
    //property Sorted: boolean read FSorted write SetSorted;
    property Owner: TWinControl read FOwner;
  end;
{$ELSE}
{

Implementation

}

function TGtk2MemoStrings.GetTextStr: string;
var
StartIter,
EndIter: TGtkTextIter;
begin
  Result := '';
  gtk_text_buffer_get_start_iter(FGtkBuf, @StartIter);
  gtk_text_buffer_get_end_iter(FGtkBuf, @EndIter);

  Result := PChar(gtk_text_iter_get_text(@StartIter, @EndIter));
end;

function TGtk2MemoStrings.GetCount: integer;
var
StartIter,
EndIter: TGtkTextIter;
begin
  Result := 0;
  Result := gtk_text_buffer_get_line_count(FGtkBuf);
  // even with no text there is 1 row. so if length of row 0 = 0 then there are "0" rows
  if Result = 1 then begin
    gtk_text_buffer_get_start_iter(FGtkBuf, @StartIter);
    gtk_text_buffer_get_end_iter(FGtkBuf, @EndIter);
    if Length(gtk_text_iter_get_text(@StartIter, @EndIter)) = 0 then Result := 0;
  end;
end;

function TGtk2MemoStrings.Get(Index: Integer): string;
var
StartIter,
EndIter: TGtkTextIter;
begin
  gtk_text_buffer_get_iter_at_line(FGtkBuf, @StartIter, Index);
  if Index = Count then
    gtk_text_buffer_get_end_iter(FGtkBuf, @EndIter)
  else begin
    gtk_text_buffer_get_iter_at_line(FGtkBuf, @EndIter, Index);
    gtk_text_iter_forward_to_line_end(@EndIter);
  end;
  // if a row is blank gtk_text_iter_forward_to_line_end will goto the row ahead
  // this is not desired. so if it jumped ahead a row then the row we want is blank
  if gtk_text_iter_get_line(@StartIter) = gtk_text_iter_get_line(@EndIter) then
    Result := PChar(gtk_text_iter_get_text(@StartIter, @EndIter))
  else Result := '';

end;

constructor TGtk2MemoStrings.Create(TextView: PGtkTextView;
  TheOwner: TWinControl);
begin
  inherited Create;
  if TextView = nil then RaiseException(
    'TGtk2MemoStrings.Create Unspecified Text widget');
  FGtkText:= TextView;
  FGtkBuf := gtk_text_view_get_buffer(FGtkText);
  if TheOwner = nil then RaiseException(
    'TGtk2MemoStrings.Create Unspecified owner');
  FOwner:=TheOwner;
end;

destructor TGtk2MemoStrings.Destroy;
begin
  // don't destroy the widgets
  inherited Destroy;
end;

procedure TGtk2MemoStrings.Assign(Source: TPersistent);
begin
  if (Source=Self) or (Source=nil) then exit;
  if Source is TStrings then begin
    gtk_text_buffer_set_text(FGtkBuf, PChar(TStrings(Source).Text), -1);
    exit;
  end;
  Inherited Assign(Source);
end;

procedure TGtk2MemoStrings.Clear;
begin
  gtk_text_buffer_set_text(FGtkBuf, PChar(''), -1);
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
StartIter: TGtkTextIter;
begin
  if Count = 0 then begin
    // insert
    gtk_text_buffer_get_start_iter(FGtkBuf, @StartIter);
    gtk_text_buffer_insert(FGtkBuf, @StartIter, PChar(S) ,-1);
  end
  else if Index < Count then begin
    //insert with LineEnding
    gtk_text_buffer_get_iter_at_line(FGtkBuf, @StartIter, Index);
    gtk_text_buffer_insert(FGtkBuf, @StartIter, PChar(S+LineEnding) ,-1);
  end
  else begin
    //append with a preceding LineEnding
    gtk_text_buffer_get_end_iter(FGtkBuf, @StartIter);
    gtk_text_buffer_insert(FGtkBuf, @StartIter, PChar(LineEnding+S) ,-1);
  end;
end;

procedure TGtk2MemoStrings.SetText(TheText: PChar);
begin
  gtk_text_buffer_set_text(FGtkBuf, TheText, -1);
end;


{$ENDIF}
