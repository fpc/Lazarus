{%MainUnit WinCEWSStdCtrls.pp}
{$IFDEF MEMOHEADER}
type

{ TWinCEMemoStrings }

TWinCEMemoStrings = class(TStrings)
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
    procedure Assign(Source: TPersistent); override;
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

function TWinCEMemoStrings.GetLineLength(Index: Integer): Integer;
begin
  Result := SendMessageW(fHandle, EM_LINELENGTH, SendMessage(fHandle, EM_LINEINDEX, Index, 0),0);
end;

function TWinCEMemoStrings.GetLineStart(Index: Integer): Integer;
begin
  Result := SendMessageW(fHandle, EM_LINEINDEX, Index, 0);
end;

function TWinCEMemoStrings.GetTextStr: string;
begin
  Result := GetControlText(fHandle);
end;

function TWinCEMemoStrings.GetRealCount: integer;
begin
  Result := SendMessageW(fHandle, EM_GETLINECOUNT, 0, 0);
end;

function TWinCEMemoStrings.GetCount: integer;
begin
  Result := GetRealCount;
  if Get(Result-1) = '' then Dec(Result);
end;

function TWinCEMemoStrings.Get(Index: Integer): string;
var
  len: Integer;
  WideBuffer: WideString;
begin
  len := GetLineLength(Index);
  if len=0 then
  begin
    Result := '';
    exit;
  end;

  Setlength(WideBuffer, len);
  PWord(@WideBuffer[1])^ := len+1;
  len := SendMessageW(FHandle, EM_GETLINE, Index, lparam(PWideChar(WideBuffer)));

  Result := UTF16ToUTF8(WideBuffer);
end;

procedure TWinCEMemoStrings.SetUpdateState(Updating: Boolean);
begin
  Windows.SendMessage(fHandle, WM_SETREDRAW, WPARAM(not Updating), 0);
  if not Updating then
    Windows.InvalidateRect(fHandle, nil, TRUE);
end;

constructor TWinCEMemoStrings.Create(Handle: HWND; TheOwner: TWinControl);
begin
  inherited Create;
  fHandle := Handle;
  FOwner := TheOwner;
end;

destructor TWinCEMemoStrings.Destroy;
begin
  // do nothing
  inherited Destroy;
end;

procedure TWinCEMemoStrings.Assign(Source: TPersistent);
var
  S: TStrings absolute Source;
begin
  if (Source=Self) or (Source=nil) then exit;

  if Source is TStrings then
  begin
    // to prevent Clear and then SetText we need to use our own Assign
    QuoteChar := S.QuoteChar;
    Delimiter := S.Delimiter;
    NameValueSeparator := S.NameValueSeparator;
    {$ifndef ver2_2_0}
    TextLineBreakStyle := S.TextLineBreakStyle;
    {$endif}
    Text := S.Text;
  end
  else
    inherited Assign(Source);
end;

procedure TWinCEMemoStrings.AddStrings(TheStrings: TStrings);
begin
  SetTextStr(GetTextStr + TStrings(TheStrings).Text);
end;

procedure TWinCEMemoStrings.Clear;
begin
  SetText('');
end;

procedure TWinCEMemoStrings.Delete(Index: integer);
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

procedure TWinCEMemoStrings.Insert(Index: integer; const S: string);
var
  LineStart: Integer;
  NewLine: String;
begin
  LineStart := GetLineStart(Index);
  if Index < GetRealCount then begin
    //insert with LineEnding
    LineStart := GetLineStart(Index);
    NewLine := S+LineEnding;
    SendMessageW(fHandle, EM_SETSEL, LineStart, LineStart);
    SendMessageW(fHandle, EM_REPLACESEL, 0, lparam(PWideChar(Utf8Decode(NewLine))))
  end
  else begin
    //append with a preceding LineEnding
    LineStart := GetLineStart(Index-1)+GetLineLength(Index-1);
    SendMessage(fHandle, EM_SETSEL, LineStart, LineStart);
    if GetRealCount = Count then
      NewLine := LineEnding+S+LineEnding
    else
      NewLine := S+LineEnding;
    SendMessageW(fHandle, EM_REPLACESEL, 0, lparam(PWideChar(Utf8Decode(NewLine))))
  end;
end;

procedure TWinCEMemoStrings.SetTextStr(const Value: string);
var
  Msg: TLMessage;
begin
  if (Value <> Text) then
  begin
    Windows.SetWindowTextW(fHandle, PWideChar(Utf8Decode(Value)));
    FillChar(Msg, SizeOf(Msg), 0);
    Msg.Msg := CM_TEXTCHANGED;
    DeliverMessage(Owner, Msg);
  end;
end;

{$ENDIF}
