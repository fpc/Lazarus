{$IFDEF MEMOHEADER}
type

{ TWin32MemoStrings }

TWin32MemoStrings = class(TStrings)
  private
    fHandle: HWND;
    FOwner: TWinControl;
    function GetLineLength(Index: Integer): Integer;
    function GetLineStart(Index: Integer): Integer;
  protected
    function GetTextStr: string; override;
    function GetCount: integer; override;
    function Get(Index : Integer) : string; override;
    //procedure SetSorted(Val : boolean); virtual;
  public
    constructor Create(Handle: HWND; TheOwner: TWinControl);
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

{$ELSE} // Implementation

function TWin32MemoStrings.GetLineLength(Index: Integer): Integer;
begin
  Result := SendMessage(fHandle, EM_LINELENGTH, SendMessage(fHandle, EM_LINEINDEX, Index, 0),0);
end;

function TWin32MemoStrings.GetLineStart(Index: Integer): Integer;
begin
  Result := SendMessage(fHandle, EM_LINEINDEX, Index, 0);
end;

function TWin32MemoStrings.GetTextStr: string;
var
  CapLen: dword;
  Caption: PChar;
  Handle: HWND;
begin
  Result := '';
  // TODO: this can be made shorter probably, using SetLength(AText, ...)
  CapLen := GetWindowTextLength(fHandle);
  Caption := StrAlloc(CapLen + 1);
  GetWindowText(fHandle, Caption, CapLen + 1);
  Result := StrPas(Caption);
  StrDispose(Caption);
end;

function TWin32MemoStrings.GetCount: integer;
begin
 Result := SendMessage(fHandle, EM_GETLINECOUNT, 0, 0);
 if (Result = 1) and (GetLineLength(0) = 0) then Result := 0;
end;

function TWin32MemoStrings.Get(Index: Integer): string;
var
textbuf: pchar;
fLength: Integer;
begin
  fLength := GetLineLength(Index);
  if fLength = 0 then Result := ''
  else begin
    GetMem(textbuf, sizeof(char)*fLength+1);
    PWord(textbuf)^ := Word(sizeof(char)*fLength+1);
    SendMessage(fHandle, EM_GETLINE, Index, lparam(textbuf));
    Result := StrPas(textbuf);
    ReAllocMem(textbuf, 0);
  end;

end;

constructor TWin32MemoStrings.Create(Handle: HWND; TheOwner: TWinControl);
begin
  inherited Create;
  fHandle := Handle;
  FOwner := TheOwner;
end;

destructor TWin32MemoStrings.Destroy;
begin
  // do nothing
  inherited Destroy;
end;

procedure TWin32MemoStrings.Assign(Source: TPersistent);
begin
  if (Source=Self) or (Source=nil) then exit;
  if Source is TStrings then begin
    SetText(PChar(TStrings(Source).Text));
    exit;
  end;
  Inherited Assign(Source);
end;

procedure TWin32MemoStrings.Clear;
begin
  SetText('');
end;

procedure TWin32MemoStrings.Delete(Index: integer);
var
LineStart,
LineEnd: Integer;
begin
 LineStart := GetLineStart(Index);
 LineEnd := GetLineStart(Index+1)- Length(LineEnding);
 if LineEnd < 0 then LineEnd := LineStart+GetLineLength(Index);
 SendMessage(fHandle, EM_SETSEL, LineStart, LineEnd);
 SendMessage(fHandle, EM_REPLACESEL,0 , lparam(PChar('')));
end;

procedure TWin32MemoStrings.Insert(Index: integer; const S: string);
var
  LineStart: Integer;
begin
  LineStart := GetLineStart(Index);
  if Count = 0 then begin
    // insert
    SendMessage(fHandle, EM_SETSEL, LineStart, LineStart);
    SendMessage(fHandle, EM_REPLACESEL,0 , lparam(PChar(S)));
  end
  else if Index < Count then begin
    //insert with LineEnding
    LineStart := GetLineStart(Index);
    SendMessage(fHandle, EM_SETSEL, LineStart, LineStart);
    SendMessage(fHandle, EM_REPLACESEL,0 , lparam(PChar(S+LineEnding)));
  end
  else begin
    //append with a preceding LineEnding
    LineStart := GetLineStart(Index-1)+GetLineLength(Index-1);
    SendMessage(fHandle, EM_SETSEL, LineStart, LineStart);
    SendMessage(fHandle, EM_REPLACESEL,0 , lparam(PChar(LineEnding+S)));
  end;

end;

procedure TWin32MemoStrings.SetText(TheText: PChar);
begin
  SendMessage(fHandle, WM_SETTEXT, 0, LPARAM(TheText));
end;

{$ENDIF}
