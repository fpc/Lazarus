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

function TWinCEMemoStrings.GetLineLength(Index: Integer): Integer;
begin
  Result := SendMessage(fHandle, EM_LINELENGTH, SendMessage(fHandle, EM_LINEINDEX, Index, 0),0);
end;

function TWinCEMemoStrings.GetLineStart(Index: Integer): Integer;
begin
  Result := SendMessage(fHandle, EM_LINEINDEX, Index, 0);
end;

function TWinCEMemoStrings.GetTextStr: string;
var
  CapLen: dword;
  Caption: PChar;
begin
  Result := GetControlText(fHandle);
end;

function TWinCEMemoStrings.GetRealCount: integer;
begin
  Result := SendMessage(fHandle, EM_GETLINECOUNT, 0, 0);
end;

function TWinCEMemoStrings.GetCount: integer;
begin
  Result := GetRealCount;
  if Get(Result-1) = '' then Dec(Result);
end;

function TWinCEMemoStrings.Get(Index: Integer): string;
var
textbuf: PWideChar;
fLength: Integer;
begin
  fLength := GetLineLength(Index);
  if fLength = 0 then Result := ''
  else begin
    textbuf := PWideChar(SysAllocStringLen(nil,fLength+1));
    PWord(textbuf)^ := Word(fLength+1);
    SendMessage(fHandle, EM_GETLINE, Index, lparam(textbuf));
    Result := WideStringToString(WideString(textbuf));
    SysFreeString(textbuf);
  end;
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
begin
  if (Source=Self) or (Source=nil) then exit;
  if Source is TStrings then begin
    SetText(PChar(TStrings(Source).Text));
    exit;
  end;
  Inherited Assign(Source);
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

procedure TWinCEMemoStrings.SetText(TheText: PChar);
begin
  Windows.SetWindowTextW(fHandle, PWideChar(Utf8Decode(TheText)))
end;

{$ENDIF}
