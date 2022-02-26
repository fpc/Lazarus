unit IdeDebuggerUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function HexDigicCount(ANum: QWord; AByteSize: Integer = 0; AForceAddr: Boolean = False): integer;
function QuoteText(AText: Utf8String): UTf8String;
function QuoteWideText(AText: WideString): WideString;

implementation

function HexDigicCount(ANum: QWord; AByteSize: Integer = 0; AForceAddr: Boolean = False): integer;
begin
  if (ANum > high(DWord)) then
    Result := 16
  else
  if (ANum > high(Word)) then
    Result := 8
  else
  if (ANum > high(Byte)) then
    Result := 4
  else
    Result := 2;

  if AByteSize*2 > Result then
    Result := AByteSize*2;

  if AForceAddr then begin
     // Fallback / TODO: Use Target-AddrSize
    if Result < SizeOf(Pointer)*2 then
      Result := 16;
  end;
end;

function QuoteWideText(AText: WideString): WideString;
const
  HEXCHR: array [0..15] of char = ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
var
  Len: Integer;
  c: WideChar;
  RPos, SPos, SEnd, QPos: PWideChar;
begin
  if AText = '' then
    exit('''''');

  Len := Length(AText);

  SetLength(Result, Len * 4); // This is the maximal length result can get
  RPos := @Result[1];
  SPos := @AText[1];
  SEnd := PWideChar(@AText[Len]) + 1;

  repeat
    RPos^ := ''''; inc(RPos);
    QPos := RPos;


    repeat
      c := SPos^;
      case c of
        #0..#31, #127, #$80..#$9F:
          break;
        '''': begin
          RPos^ := c; inc(RPos);
          RPos^ := c; inc(RPos);
          inc(SPos);
        end;
        else begin
          RPos^ := c; inc(RPos);
          inc(SPos);
        end;
      end;

      c := SPos^;
    until False;

    if RPos = QPos then
      dec(RPos)
    else begin
      RPos^ := ''''; inc(RPos);
    end;

    repeat
      c := SPos^;
      if (c = #0) and (SPos >= SEnd) then begin
        // END OF TEXT
        Assert(RPos-1 <= @Result[Length(Result)], 'RPos-1 <= @Result[Length(Result)]');
        SetLength(Result, RPos - PWideChar(@Result[1]));
        exit;
      end;

      RPos^ := '#'; inc(RPos);
      RPos^ := '$'; inc(RPos);
      RPos^ := HEXCHR[Byte(c) >> 4]; inc(RPos);
      RPos^ := HEXCHR[Byte(c) and 15]; inc(RPos);
      inc(SPos);
      c := SPos^;
    until not(c in [#0..#31, #127, #$80..#$9F]);

  until False;
end;

function QuoteText(AText: Utf8String): UTf8String;
// TODO: process large text in chunks to avoid allocating huge memory
const
  HEXCHR: array [0..15] of char = ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
var
  Len: Integer;
  c: Char;
  RPos, SPos, SEnd, QPos: PChar;
begin
  if AText = '' then
    exit('''''');

  Len := Length(AText);

  SetLength(Result, Len * 4); // This is the maximal length result can get
  RPos := @Result[1];
  SPos := @AText[1];
  SEnd := PChar(@AText[Len]) + 1;

  repeat
    RPos^ := ''''; inc(RPos);
    QPos := RPos;


    repeat
      c := SPos^;
      case c of
        '''': begin
          RPos^ := c; inc(RPos);
          RPos^ := c; inc(RPos);
          inc(SPos);
        end;
        #32..Pred(''''), Succ('''')..#126: begin
          RPos^ := c; inc(RPos);
          inc(SPos);
        end;
        #192..#223: begin
          if ((Byte(SPos[1]) and $C0) <> $80)
          then
            break; // invalid utf8 -> escape
          RPos^ := c; inc(RPos);
          RPos^ := SPos[1]; inc(RPos);
          inc(SPos, 2);
        end;
        #224..#239: begin
          if ((Byte(SPos[1]) and $C0) <> $80) or ((Byte(SPos[2]) and $C0) <> $80)
          then
            break; // invalid utf8 -> escape
          RPos^ := c; inc(RPos);
          RPos^ := SPos[1]; inc(RPos);
          RPos^ := SPos[2]; inc(RPos);
          inc(SPos, 3);
        end;
        #240..#247: begin
          if ((Byte(SPos[1]) and $C0) <> $80) or ((Byte(SPos[2]) and $C0) <> $80) or
             ((Byte(SPos[3]) and $C0) <> $80)
          then
            break; // invalid utf8 -> escape
          RPos^ := c; inc(RPos);
          RPos^ := SPos[1]; inc(RPos);
          RPos^ := SPos[2]; inc(RPos);
          RPos^ := SPos[3]; inc(RPos);
          inc(SPos, 4);
        end;
        #0: begin
          if (SPos < SEnd) then
            break; // need escaping

          // END OF TEXT
          RPos^ := ''''; inc(RPos);
          Assert(RPos-1 <= @Result[Length(Result)], 'RPos-1 <= @Result[Length(Result)]');
          SetLength(Result, RPos - @Result[1]);
          exit;
        end;
        else
          break; // need escaping
      end;

      c := SPos^;
    until False;

    if RPos = QPos then
      dec(RPos)
    else begin
      RPos^ := ''''; inc(RPos);
    end;

    repeat
      c := SPos^;
      if (c = #0) and (SPos >= SEnd) then begin
        // END OF TEXT
        Assert(RPos-1 <= @Result[Length(Result)], 'RPos-1 <= @Result[Length(Result)]');
        SetLength(Result, RPos - @Result[1]);
        exit;
      end;

      RPos^ := '#'; inc(RPos);
      RPos^ := '$'; inc(RPos);
      RPos^ := HEXCHR[Byte(c) >> 4]; inc(RPos);
      RPos^ := HEXCHR[Byte(c) and 15]; inc(RPos);
      inc(SPos);
      c := SPos^;
    until not(c in [#0..#31, #127, #$80..#$BF]);

  until False;
end;

end.

