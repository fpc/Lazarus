unit IdeDebuggerUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazMethodList;

type

  { TChangeNotificationGeneric }

  generic TChangeNotificationGeneric<_BASE: TObject> = class(_BASE)
  strict private
    FChangeNotifications: TMethodList;
  protected
    procedure FreeChangeNotifications;
  public
    // destructor Destroy; override; // not supported by fpc
    procedure AddChangeNotification(AHandler: TNotifyEvent);
    procedure RemoveChangeNotification(AHandler: TNotifyEvent);
    procedure CallChangeNotifications;
  end;

  TQuoteTextOpt = (qtMultiLine);
  TQuoteTextOpts = set of TQuoteTextOpt;

function HexDigicCount(ANum: QWord; AByteSize: Integer = 0; AForceAddr: Boolean = False): integer;
function QuoteText(AText: Utf8String; AnOpts: TQuoteTextOpts = []): UTf8String;
function QuoteWideText(AText: WideString; AnOpts: TQuoteTextOpts = []): WideString;
function ClearMultiline(const AValue: ansistring): ansistring;

(* GetExpressionForArrayElement
  If "AnArrayExpression" returns an array, get a new Expression that returns
  the element "[AnIndex]"
  In case "AnArrayExpression" is an array slice, the new "AnIndex" has to
  replace the slice-range.
  E.g. Entry [15] from "Foo[11..19]" must result in "Foo[15]"

  By default "AnIndex" has the lower slice-value as index.
  That is
  - For a slice "Foo[11..19]" (with type TFoo = array [0..100])
  - The slice would defined as "Slice[11..19]"
    The Index "11" returns Foo[11]
*)
function GetExpressionForArrayElement(AnArrayExpression: AnsiString; AnIndex: String): AnsiString; overload;
function GetExpressionForArrayElement(AnArrayExpression: AnsiString; AnIndex: Int64): AnsiString; overload;

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

function QuoteWideText(AText: WideString; AnOpts: TQuoteTextOpts): WideString;
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

  // An non-quote char (ctrl char) will become 4 chars: #$12
  // But if qtMultiLine then it can be #$0A + LineEnding
  if qtMultiLine in AnOpts then
    SetLength(Result, Len * 6) // This is the maximal length result can get
  else
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
      if (qtMultiLine in AnOpts) and
       ( (c = #10) or
         ((c=#13) and (SPos^ <> #10))
        )
      then begin
        if LineEnding = #13#10 then
          RPos^ := #13; inc(RPos);
        RPos^ := #10; inc(RPos);
      end;
    c := SPos^;
    until not(c in [#0..#31, #127, #$80..#$9F]);

  until False;
end;

function QuoteText(AText: Utf8String; AnOpts: TQuoteTextOpts): UTf8String;
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

  // An non-quote char (ctrl char) will become 4 chars: #$12
  // But if qtMultiLine then it can be #$0A + LineEnding
  if qtMultiLine in AnOpts then
    SetLength(Result, Len * 6) // This is the maximal length result can get
  else
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
      if (qtMultiLine in AnOpts) and
         ( (c = #10) or
           ((c=#13) and (SPos^ <> #10))
          )
      then begin
        if LineEnding = #13#10 then
          RPos^ := #13; inc(RPos);
        RPos^ := #10; inc(RPos);
      end;
      c := SPos^;
    until not(c in [#0..#31, #127, #$80..#$BF]);

  until False;
end;

function ClearMultiline(const AValue: ansistring): ansistring;
var
  j: SizeInt;
  ow: SizeInt;
  NewLine: Boolean;
begin
  ow:=0;
  SetLength(Result{%H-},Length(AValue));
  NewLine:=true;
  for j := 1 to Length(AValue) do begin
    if (AValue[j]=#13) or (AValue[j]=#10) then begin
      NewLine:=true;
      inc(ow);
      Result[ow]:=#32; // insert one space instead of new line
    end
    else if Avalue[j] in [#9,#32] then begin
      if not NewLine then begin // strip leading spaces after new line
        inc(ow);
        Result[ow]:=#32;
      end;
    end else begin
      inc(ow);
      Result[ow]:=AValue[j];
      NewLine:=false;
    end;
  end;
  If ow>255 then begin
    //Limit watch to 255 chars in length
    Result:=Copy(Result,1,252)+'...';
  end else begin
    SetLength(Result,ow);
  end;
end;

function GetExpressionForArrayElement(AnArrayExpression: AnsiString;
  AnIndex: String): AnsiString;
var
  s, e, p: PChar;
  MaybeBeforeArrayIdx, InString, FoundDotDot: Boolean;
  RStart, InRndBracket, InSqrBracket, InArrayIdxBracket: Integer;

  RStartStack: array of record
    RStart: integer;
    InRnd, InSqr: integer;
    FoundDotDot: Boolean;
  end;

  procedure PushData;
  begin
    if InArrayIdxBracket >= Length(RStartStack) then
      SetLength(RStartStack, InArrayIdxBracket + 8);
    RStartStack[InArrayIdxBracket].RStart      := RStart;
    RStartStack[InArrayIdxBracket].InRnd       := InRndBracket;
    RStartStack[InArrayIdxBracket].InSqr       := InSqrBracket;
    RStartStack[InArrayIdxBracket].FoundDotDot := FoundDotDot;
    FoundDotDot := False;
    InRndBracket := 0;
    InSqrBracket := 0;
  end;
  procedure PopData;
  begin
    RStart       := RStartStack[InArrayIdxBracket].RStart;
    InRndBracket := RStartStack[InArrayIdxBracket].InRnd;
    InSqrBracket := RStartStack[InArrayIdxBracket].InSqr;
    FoundDotDot  := RStartStack[InArrayIdxBracket].FoundDotDot;
  end;
begin
  Result := AnArrayExpression + '[' + AnIndex + ']';
  if AnArrayExpression = '' then
    exit;

  s := @AnArrayExpression[1];
  p := s;
  e := @AnArrayExpression[Length(AnArrayExpression)];

  MaybeBeforeArrayIdx := False;
  InRndBracket := 0;
  InSqrBracket := 0;
  FoundDotDot := False;
  InArrayIdxBracket := 0;
  InString := False;
  dec(p);
  while p < e do begin
    inc(p);
    if p^ = '''' then begin
      InString := not InString;
      MaybeBeforeArrayIdx := True; // sub-range of string
      Continue;
    end;
    if InString then
      Continue;


    if p^ in ['@', '.', '+', '-', '*', '/', '(', ',', '=', '<', '>', '#', '$', '%', '&', '!'] then begin
      MaybeBeforeArrayIdx := False;  // after operator. A [1..5] would be a set of byte

      if (p^ = '(') and (InArrayIdxBracket > 0) then begin
        inc(InRndBracket);
        MaybeBeforeArrayIdx := False;
      end
      else
      if (InArrayIdxBracket > 0) and (InSqrBracket = 0) and (InRndBracket = 0) then begin
        if (p^ = '.') and (p[1] = '.') then begin
          if FoundDotDot then
            break; // something wrong
          FoundDotDot := True;
          inc(p);
        end
        else
        if (p^ = ',') then begin
          if FoundDotDot then begin
            Result := copy(AnArrayExpression, 1 , RStart+1) +
              AnIndex +
              copy(AnArrayExpression, p-s + 1, Length(AnArrayExpression));
            exit;
          end;
          RStart := p - s; // Length of substring before "["
        end;
      end;
    end


    else
    if (p - s >= 2) and
       (p[-2] in [#1..#32]) and (p[-1] in ['i', 'I']) and
       (p^ in ['n', 'N']) and  (p[1] in [#1..#32])
    then begin
      MaybeBeforeArrayIdx := False;  // after IN operator. A [1..5] would be a set of byte
    end


    else
    if p^ in ['a'..'z', 'A'..'Z', '_', ')', ']'] then begin
      MaybeBeforeArrayIdx := True;  // after identifier, or after ")" or "]"
      if (p^ = ')') and (InArrayIdxBracket > 0) then begin
        dec(InRndBracket);
        MaybeBeforeArrayIdx := True;
        if InRndBracket < 0 then
          break; // something wrong.
      end
      else
      if (p^ = ']') then begin
        if InSqrBracket > 0 then begin
          dec(InSqrBracket);
        end
        else
        if InArrayIdxBracket > 0 then begin
          if InRndBracket > 0 then
            break; // something wrong
          if FoundDotDot then begin
            Result := copy(AnArrayExpression, 1 , RStart+1) +
              AnIndex +
              copy(AnArrayExpression, p-s + 1, Length(AnArrayExpression));
            exit;
          end;
          dec(InArrayIdxBracket);
          PopData;
        end
        else
          break; //something wrong
      end;
    end


    else
    if (p^ = '[') then begin
      if (not MaybeBeforeArrayIdx) then begin
        inc(InSqrBracket);
      end
      else
      begin
        // maybe found first range
        PushData;

        inc(InArrayIdxBracket);
        RStart := p - s; // Length of substring before "["
      end;
    end;

  end;
end;

function GetExpressionForArrayElement(AnArrayExpression: AnsiString;
  AnIndex: Int64): AnsiString;
begin
  Result := GetExpressionForArrayElement(AnArrayExpression, IntToStr(AnIndex));
end;

{ TChangeNotificationGeneric }

procedure TChangeNotificationGeneric.FreeChangeNotifications;
begin
  FreeAndNil(FChangeNotifications);
end;

procedure TChangeNotificationGeneric.AddChangeNotification(AHandler: TNotifyEvent);
begin
  if FChangeNotifications = nil then
    FChangeNotifications := TMethodList.Create;
  FChangeNotifications.Add(TMethod(AHandler));
end;

procedure TChangeNotificationGeneric.RemoveChangeNotification(AHandler: TNotifyEvent);
begin
  if FChangeNotifications <> nil then
    FChangeNotifications.Remove(TMethod(AHandler));
end;

procedure TChangeNotificationGeneric.CallChangeNotifications;
begin
  if FChangeNotifications <> nil then
    FChangeNotifications.CallNotifyEvents(Self);
end;

end.

