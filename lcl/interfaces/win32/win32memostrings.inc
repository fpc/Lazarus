{%MainUnit win32wsstdctrls.pp}
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
    function GetRealCount: integer;
    function GetCount: integer; override;
    function Get(Index : Integer) : string; override;
    //procedure SetSorted(Val : boolean); virtual;
    procedure SetUpdateState(Updating: Boolean); override;
  public
    constructor Create(Handle: HWND; TheOwner: TWinControl);
    destructor Destroy; override;
    procedure AddStrings(TheStrings: TStrings); override;
    procedure Clear; override;
    procedure Delete(Index : integer); override;
    procedure Insert(Index : integer; const S: string); override;
    procedure SetTextStr(const Value: string); override;
    //procedure Sort; virtual;
  public
    //property Sorted: boolean read FSorted write SetSorted;
    property Owner: TWinControl read FOwner;
  end;

{$ELSE} // Implementation

function TWin32MemoStrings.GetLineLength(Index: Integer): Integer;
begin
  Result := SendMessage(fHandle, EM_LINELENGTH, SendMessage(fHandle, EM_LINEINDEX, Index, 0), 0);
end;

function TWin32MemoStrings.GetLineStart(Index: Integer): Integer;
begin
  Result := SendMessage(fHandle, EM_LINEINDEX, Index, 0);
end;

function TWin32MemoStrings.GetTextStr: string;
{$ifndef WindowsUnicodeSupport}
var
  CapLen: dword;
  Caption: PChar;
{$endif}
begin
{$ifdef WindowsUnicodeSupport}
  Result := win32proc.GetControlText(fHandle);
{$else}
  Result := '';

  // TODO: this can be made shorter probably, using SetLength(AText, ...)
  CapLen := GetWindowTextLength(fHandle);
  Caption := StrAlloc(CapLen + 1);
  GetWindowText(fHandle, Caption, CapLen + 1);
  Result := StrPas(Caption);
  StrDispose(Caption);
{$endif}
end;

function TWin32MemoStrings.GetRealCount: integer;
begin
  Result := SendMessage(fHandle, EM_GETLINECOUNT, 0, 0);
end;

function TWin32MemoStrings.GetCount: integer;
begin
  Result := GetRealCount;
  if Get(Result-1) = '' then Dec(Result);
end;

function TWin32MemoStrings.Get(Index: Integer): string;
var
  len: Integer;
{$ifdef WindowsUnicodeSupport}
  WideBuffer: WideString;
  AnsiBuffer: string;
{$endif WindowsUnicodeSupport}
begin
  len := GetLineLength(Index);
  if len=0 then
  begin
    Result := '';
    exit;
  end;
{$ifdef WindowsUnicodeSupport}
  if UnicodeEnabledOS then
  begin
    Setlength(WideBuffer, len);
    PWord(@WideBuffer[1])^ := len+1;
    len := SendMessageW(fHandle, EM_GETLINE, Index, lparam(PWideChar(WideBuffer)));

    Result := Utf8Encode(WideBuffer);
  end
  else
  begin
    Setlength(AnsiBuffer, len);
    PWord(@AnsiBuffer[1])^ := len+1;
    len := SendMessage(fHandle, EM_GETLINE, Index, lparam(PChar(AnsiBuffer)));

    Result := AnsiToUtf8(AnsiBuffer);
  end;
{$else}
  Setlength(Result, len);

  // no need for temp buf and moving
  // the result is without null terminator.
  PWord(@Result[1])^ := len+1;
  len := SendMessage(fHandle, EM_GETLINE, Index, lparam(pchar(Result)));

  // read just length in case something went wrong
  Setlength(Result, len);
{$endif}
end;

procedure TWin32MemoStrings.SetUpdateState(Updating: Boolean);
begin
  Windows.SendMessage(fHandle, WM_SETREDRAW, WPARAM(not Updating), 0);
  if not Updating then
    Windows.InvalidateRect(fHandle, nil, TRUE);
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

procedure TWin32MemoStrings.AddStrings(TheStrings: TStrings);
begin
  SetTextStr(TStrings(TheStrings).Text);
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
  LineEnd := GetLineStart(Index+1);
  if LineEnd < 0 then LineEnd := LineStart+GetLineLength(Index);
  SendMessage(fHandle, EM_SETSEL, LineStart, LineEnd);
  SendMessage(fHandle, EM_REPLACESEL,0 , lparam(PChar('')));
end;

procedure TWin32MemoStrings.Insert(Index: integer; const S: string);
var
  LineStart: Integer;
  NewLine: String;
begin
  LineStart := GetLineStart(Index);
  if Index < GetRealCount then
  begin
    //insert with LineEnding
    LineStart := GetLineStart(Index);
    NewLine := S+LineEnding;
    SendMessage(fHandle, EM_SETSEL, LineStart, LineStart);
{$ifdef WindowsUnicodeSupport}
    if UnicodeEnabledOS
    then SendMessageW(fHandle, EM_REPLACESEL, 0, lparam(PWideChar(Utf8Decode(NewLine))))
    else SendMessage(fHandle, EM_REPLACESEL, 0, lparam(Utf8ToAnsi(NewLine)));
{$else}
    SendMessage(fHandle, EM_REPLACESEL, 0, lparam(PChar(NewLine)));
{$endif}
  end
  else
  begin
    //append with a preceding LineEnding
    LineStart := GetLineStart(Index-1)+GetLineLength(Index-1);
    SendMessage(fHandle, EM_SETSEL, LineStart, LineStart);
    if GetRealCount = Count then
      NewLine := LineEnding+S+LineEnding
    else
      NewLine := S+LineEnding;
{$ifdef WindowsUnicodeSupport}
    if UnicodeEnabledOS
    then SendMessageW(fHandle, EM_REPLACESEL, 0, lparam(PWideChar(Utf8Decode(NewLine))))
    else SendMessage(fHandle, EM_REPLACESEL, 0, lparam(Utf8ToAnsi(NewLine)));
{$else}
    SendMessage(fHandle, EM_REPLACESEL, 0, lparam(PChar(NewLine)));
{$endif}
  end;
end;

procedure TWin32MemoStrings.SetTextStr(const Value: string);
begin
{$ifdef WindowsUnicodeSupport}
  if UnicodeEnabledOS
  then Windows.SetWindowTextW(fHandle, PWideChar(UTF8Decode(Value)))
  else Windows.SetWindowText(fHandle, PChar(Utf8ToAnsi(Value)));
{$else}
  SendMessage(fHandle, WM_SETTEXT, 0, LPARAM(PChar(Value)));
{$endif}
end;

{$ENDIF}
