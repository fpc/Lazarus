{%MainUnit win32wsstdctrls.pp}
{$IFDEF MEMOHEADER}
type

{ TWin32MemoStrings }

  TWin32MemoStrings = class(TCustomMemoStrings)
  private
    FHandle: HWND;
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
    procedure Assign(Source: TPersistent); override;
    procedure AddStrings(TheStrings: TStrings); override;
    procedure Clear; override;
    procedure Delete(Index : integer); override;
    procedure Insert(Index : integer; const S: string); override;
    procedure SetTextStr(const Value: string); override;
    procedure LoadFromFile(const FileName: string); override;
    procedure SaveToFile(const FileName: string); override;
    //procedure Sort; virtual;
  public
    //property Sorted: boolean read FSorted write SetSorted;
    property Owner: TWinControl read FOwner;
  end;

{$ELSE} // Implementation

function TWin32MemoStrings.GetLineLength(Index: Integer): Integer;
begin
  Result := Integer(SendMessageW(FHandle, EM_LINELENGTH, SendMessageW(FHandle, EM_LINEINDEX, Index, 0), 0));
end;

function TWin32MemoStrings.GetLineStart(Index: Integer): Integer;
begin
  Result := Integer(SendMessageW(FHandle, EM_LINEINDEX, Index, 0));
end;

function TWin32MemoStrings.GetTextStr: string;
begin
  Result := win32proc.GetControlText(FHandle);
end;

function TWin32MemoStrings.GetRealCount: integer;
begin
  Result := SendMessage(FHandle, EM_GETLINECOUNT, 0, 0);
end;

function TWin32MemoStrings.GetCount: integer;
begin
  Result := GetRealCount;
  if Get(Result-1) = '' then Dec(Result);
end;

function TWin32MemoStrings.Get(Index: Integer): string;
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

procedure TWin32MemoStrings.SetUpdateState(Updating: Boolean);
begin
  Windows.SendMessage(FHandle, WM_SETREDRAW, WPARAM(not Updating), 0);
  if not Updating then
    Windows.InvalidateRect(FHandle, nil, TRUE);
end;

constructor TWin32MemoStrings.Create(Handle: HWND; TheOwner: TWinControl);
begin
  inherited Create;
  FHandle := Handle;
  FOwner := TheOwner;
end;

procedure TWin32MemoStrings.Assign(Source: TPersistent);
var
  S: TStrings absolute Source;
begin
  if Source is TStrings then
  begin
    // to prevent Clear and then SetText we need to use our own Assign
    TextLineBreakStyle := S.TextLineBreakStyle; //put this first to call CheckSpecialChars if not done yet
    QuoteChar := S.QuoteChar;
    Delimiter := S.Delimiter;
    NameValueSeparator := S.NameValueSeparator;
    Text := S.Text;
  end
  else
    inherited Assign(Source);
end;

procedure TWin32MemoStrings.AddStrings(TheStrings: TStrings);
begin
  SetTextStr(GetTextStr + TStrings(TheStrings).Text);
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
  SendMessageW(FHandle, EM_SETSEL, LineStart, LineEnd);
  SendMessageW(FHandle, EM_REPLACESEL,0 , lparam(PWChar('')));
end;

procedure TWin32MemoStrings.Insert(Index: integer; const S: string);
var
  LineStart, RealCount: Integer;
  NewLine: String;
begin
  RealCount := GetRealCount;
  if Index < RealCount then
  begin
    //insert with LineEnding
    LineStart := GetLineStart(Index);
    NewLine := S+LineEnding;
    SendMessageW(FHandle, EM_SETSEL, LineStart, LineStart);
    SendMessageW(FHandle, EM_REPLACESEL, 0, lparam(PWideChar(UTF8ToUTF16(NewLine))));
  end
  else
  begin
    //append with a preceding LineEnding
    LineStart := GetLineStart(Index-1)+GetLineLength(Index-1);
    SendMessageW(FHandle, EM_SETSEL, LineStart, LineStart);
    //check if last line is empty
    if Get(RealCount - 1) <> '' then
      NewLine := LineEnding+S+LineEnding
    else
      NewLine := S+LineEnding;
    SendMessageW(FHandle, EM_REPLACESEL, 0, lparam(PWideChar(UTF8ToUTF16(NewLine))));
  end;
end;

procedure TWin32MemoStrings.SetTextStr(const Value: string);
var
  Msg: TLMessage;
  AdjustedValue: String;
begin
  AdjustedValue := AdjustLineBreaks(Value);
  if (AdjustedValue <> Text) then
  begin
    Windows.SetWindowTextW(FHandle, PWideChar(UTF8ToUTF16(AdjustedValue)));
    FillChar(Msg, SizeOf(Msg), 0);
    Msg.Msg := CM_TEXTCHANGED;
    DeliverMessage(Owner, Msg);
  end;
end;

procedure TWin32MemoStrings.LoadFromFile(const FileName: string);
var
  TheStream: TFileStreamUTF8;
begin
  TheStream:=TFileStreamUtf8.Create(FileName,fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(TheStream);
  finally
    TheStream.Free;
  end;
end;

procedure TWin32MemoStrings.SaveToFile(const FileName: string);
var
  TheStream: TFileStreamUTF8;
begin
  TheStream:=TFileStreamUtf8.Create(FileName,fmCreate);
  try
    SaveToStream(TheStream);
  finally
    TheStream.Free;
  end;
end;

{$ENDIF}
