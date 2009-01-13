{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

-------------------------------------------------------------------------------}
unit SynEditTextBase;

{$I synedit.inc}

interface

uses
  Classes, SysUtils, LCLProc, SynEditTypes;

type
  TSynEditStrings = class;

  TStringListLineCountEvent = procedure(Sender: TSynEditStrings;
                                        Index, Count: Integer) of object;
  TSynEditNotifyReason = (senrLineCount, senrLineChange);

  { TSynEditStrings }

  TSynEditStrings = class(TStrings)
  protected
    FIsUtf8: Boolean;
    function  GetIsUtf8 : Boolean; virtual;
    procedure SetIsUtf8(const AValue : Boolean); virtual;
    function GetRange(Index: integer): TSynEditRange; virtual; abstract;
    procedure PutRange(Index: integer; ARange: TSynEditRange); virtual; abstract;
    function  GetAttribute(const Owner: TClass; const Index: Integer): Pointer; virtual; abstract;
    procedure SetAttribute(const Owner: TClass; const Index: Integer; const AValue: Pointer); virtual; abstract;

    function GetExpandedString(Index: integer): string; virtual; abstract;
    function GetLengthOfLongestLine: integer; virtual; abstract;
    procedure SetTextStr(const Value: string); override;

  public
    constructor Create;
    procedure DeleteLines(Index, NumLines: integer); virtual; abstract;
    procedure InsertLines(Index, NumLines: integer); virtual; abstract;
    procedure InsertStrings(Index: integer; NewStrings: TStrings); virtual; abstract;
    procedure ClearRanges(ARange: TSynEditRange); virtual; abstract;

    procedure RegisterAttribute(const Index: TClass; const Size: Word); virtual; abstract;
    property Attribute[Owner: TClass; Index: Integer]: Pointer
      read GetAttribute write SetAttribute;
    procedure AddChangeHandler(AReason: TSynEditNotifyReason;
                AHandler: TStringListLineCountEvent); virtual; abstract;
    procedure RemoveChangeHandler(AReason: TSynEditNotifyReason;
                AHandler: TStringListLineCountEvent); virtual; abstract;
  public
    // Byte to Char
    function LogicalToPhysicalPos(const p: TPoint): TPoint;
    function LogicalToPhysicalCol(const Line: string;
                                  LogicalPos: integer): integer;
    function LogicalToPhysicalCol(Line: PChar; LineLen: integer;
                  LogicalPos, StartBytePos,
                  StartPhysicalPos: integer): integer; virtual;
    // Char to Byte
    function PhysicalToLogicalPos(const p: TPoint): TPoint;
    function PhysicalToLogicalCol(const Line: string;
                                  PhysicalPos: integer): integer;
    function PhysicalToLogicalCol(const Line: string;
                  PhysicalPos, StartBytePos,
                  StartPhysicalPos: integer): integer; virtual;
  public
    property ExpandedStrings[Index: integer]: string read GetExpandedString;
    property LengthOfLongestLine: integer read GetLengthOfLongestLine;
    property IsUtf8: Boolean read GetIsUtf8 write SetIsUtf8;
    property Ranges[Index: integer]: TSynEditRange read GetRange write PutRange;
  end;

  { TSynEditStringsLinked }

  TSynEditStringsLinked = class(TSynEditStrings)
  protected
    fSynStrings: TSynEditStrings;

    function  GetIsUtf8 : Boolean;  override;
    procedure SetIsUtf8(const AValue : Boolean);  override;

    function GetRange(Index: integer): TSynEditRange;  override;
    procedure PutRange(Index: integer; ARange: TSynEditRange);  override;

    function  GetAttribute(const Owner: TClass; const Index: Integer): Pointer; override;
    procedure SetAttribute(const Owner: TClass; const Index: Integer; const AValue: Pointer); override;
  protected
    function GetCount: integer; override;
    function GetCapacity: integer;
      {$IFDEF SYN_COMPILER_3_UP} override; {$ELSE} virtual; {$ENDIF}
    procedure SetCapacity(NewCapacity: integer);
      {$IFDEF SYN_COMPILER_3_UP} override; {$ELSE} virtual; {$ENDIF}
    function  Get(Index: integer): string; override;
    function  GetObject(Index: integer): TObject; override;
    procedure Put(Index: integer; const S: string); override;
    procedure PutObject(Index: integer; AObject: TObject); override;

    procedure SetUpdateState(Updating: Boolean); override;
  public
    constructor Create(ASynStringSource: TSynEditStrings);

    function Add(const S: string): integer; override;
    procedure AddStrings(AStrings: TStrings); override;
    procedure Clear; override;
    procedure Delete(Index: integer); override;
    procedure DeleteLines(Index, NumLines: integer);  override;
    procedure Insert(Index: integer; const S: string); override;
    procedure InsertLines(Index, NumLines: integer); override;
    procedure InsertStrings(Index: integer; NewStrings: TStrings); override;
    procedure Exchange(Index1, Index2: integer); override;

    procedure ClearRanges(ARange: TSynEditRange); override;

    // Size: 0 = Bit (TODO); 1..8 Size In Byte "SizeOf()"
    procedure RegisterAttribute(const Index: TClass; const Size: Word); override;
    procedure AddChangeHandler(AReason: TSynEditNotifyReason;
                AHandler: TStringListLineCountEvent); override;
    procedure RemoveChangeHandler(AReason: TSynEditNotifyReason;
                AHandler: TStringListLineCountEvent); override;
  end;



implementation


{ TSynEditStrings }

constructor TSynEditStrings.Create;
begin
  inherited Create;
  IsUtf8 := True;
end;

function TSynEditStrings.GetIsUtf8 : Boolean;
begin
  Result := FIsUtf8;
end;

procedure TSynEditStrings.SetIsUtf8(const AValue : Boolean);
begin
  FIsUtf8 := AValue;
end;

procedure TSynEditStrings.SetTextStr(const Value : string);
var
  StartPos: Integer;
  p: Integer;
  Len: Integer;
  sl: TStringList;
begin
  BeginUpdate;
  sl:=TStringList.Create;
  try
    Clear;
    p:=1;
    StartPos:=p;
    Len:=length(Value);
    while p<=Len do begin
      if not (Value[p] in [#10,#13]) then begin
        inc(p);
      end else begin
        sl.Add(copy(Value,StartPos,p-StartPos));
        inc(p);
        if (p<=Len) and (Value[p] in [#10,#13]) and (Value[p-1]<>Value[p]) then
          inc(p);
        StartPos:=p;
      end;
    end;
    if StartPos<=Len then
      sl.Add(copy(Value,StartPos,Len-StartPos+1));
    AddStrings(sl);
  finally
    sl.Free;
    EndUpdate;
  end;
end;

function TSynEditStrings.LogicalToPhysicalPos(const p : TPoint) : TPoint;
begin
  Result := p;
  if Result.Y - 1 < Count then
    Result.X:=LogicalToPhysicalCol(self[Result.Y - 1],Result.X);
end;

function TSynEditStrings.LogicalToPhysicalCol(const Line : string; LogicalPos : integer) : integer;
begin
  Result := LogicalToPhysicalCol(PChar(Pointer(Line)),length(Line),LogicalPos,1,1);
end;

function TSynEditStrings.LogicalToPhysicalCol(Line : PChar; LineLen : integer; LogicalPos, StartBytePos, StartPhysicalPos : integer) : integer;
var
  BytePos, ByteLen: integer;
  ScreenPos: integer;
begin
  ByteLen := LineLen;
  // map UTF8
  ScreenPos := StartPhysicalPos;
  BytePos:= StartBytePos;
  while BytePos<LogicalPos do begin
    if (BytePos <= ByteLen) then begin
      inc(ScreenPos);
      if IsUTF8 then
        inc(BytePos,UTF8CharacterLength(@Line[BytePos-1]))
      else
        inc(BytePos);
    end else begin
      // beyond end of line
      inc(ScreenPos,LogicalPos-BytePos);
      break;
    end;
  end;
  if (BytePos>LogicalPos) and (ScreenPos>StartPhysicalPos) then
    dec(ScreenPos);
  Result := ScreenPos;
end;

function TSynEditStrings.PhysicalToLogicalPos(const p : TPoint) : TPoint;
begin
  Result := p;
  if (Result.Y>=1) and (Result.Y <= Count) then
    Result.X:=PhysicalToLogicalCol(self[Result.Y - 1],Result.X,1,1);
end;

function TSynEditStrings.PhysicalToLogicalCol(const Line : string; PhysicalPos : integer) : integer;
begin
  Result:=PhysicalToLogicalCol(Line,PhysicalPos,1,1);
end;

function TSynEditStrings.PhysicalToLogicalCol(const Line : string; PhysicalPos, StartBytePos, StartPhysicalPos : integer) : integer;
var
  BytePos, ByteLen: integer;
  ScreenPos: integer;
  PLine: PChar;
begin
  ByteLen := Length(Line);
  ScreenPos := StartPhysicalPos;
  BytePos := StartBytePos;
  PLine := PChar(Line);
  // map utf
  while ScreenPos < PhysicalPos do begin
    if (BytePos <= ByteLen) then begin
      inc(ScreenPos);
      if IsUTF8 then
        inc(BytePos,UTF8CharacterLength(@PLine[BytePos-1]))
      else
        inc(BytePos);
    end else begin
      // beyond end of line
      inc(BytePos,PhysicalPos-ScreenPos);
      break;
    end;
  end;
  if (ScreenPos>PhysicalPos) and (BytePos>1) and (BytePos-2<ByteLen)
  and (PLine[BytePos-2]=#9) then
    dec(BytePos);
  Result := BytePos;
end;

{ TSynEditStringsLinked }

constructor TSynEditStringsLinked.Create(ASynStringSource: TSynEditStrings);
begin
  fSynStrings := ASynStringSource;
  Inherited Create;
end;

function TSynEditStringsLinked.Add(const S: string): integer;
begin
  Result := fSynStrings.Add(S);
end;

procedure TSynEditStringsLinked.AddStrings(AStrings: TStrings);
begin
  fSynStrings.AddStrings(AStrings);
end;

procedure TSynEditStringsLinked.Clear;
begin
  fSynStrings.Clear;
end;

procedure TSynEditStringsLinked.Delete(Index: integer);
begin
  fSynStrings.Delete(Index);
end;

procedure TSynEditStringsLinked.DeleteLines(Index, NumLines: integer);
begin
  fSynStrings.DeleteLines(Index, NumLines);
end;

procedure TSynEditStringsLinked.Insert(Index: integer; const S: string);
begin
  fSynStrings.Insert(Index, S);
end;

procedure TSynEditStringsLinked.InsertLines(Index, NumLines: integer);
begin
  fSynStrings.InsertLines(Index, NumLines);
end;

procedure TSynEditStringsLinked.InsertStrings(Index: integer; NewStrings: TStrings);
begin
  fSynStrings.InsertStrings(Index, NewStrings);
end;

procedure TSynEditStringsLinked.Exchange(Index1, Index2: integer);
begin
  fSynStrings.Exchange(Index1, Index2);
end;

function TSynEditStringsLinked.GetIsUtf8: Boolean;
begin
  Result := FSynStrings.IsUtf8;
end;

procedure TSynEditStringsLinked.SetIsUtf8(const AValue: Boolean);
begin
  FSynStrings.IsUtf8 := AValue;
end;

//Ranges
function TSynEditStringsLinked.GetRange(Index: integer): TSynEditRange;
begin
  Result:= fSynStrings.Ranges[Index];
end;

procedure TSynEditStringsLinked.PutRange(Index: integer; ARange: TSynEditRange);
begin
  fSynStrings.Ranges[Index] := ARange;
end;

function TSynEditStringsLinked.GetAttribute(const Owner: TClass; const Index: Integer): Pointer;
begin
  Result := fSynStrings.Attribute[Owner, Index];
end;

procedure TSynEditStringsLinked.SetAttribute(const Owner: TClass;
                                const Index: Integer; const AValue: Pointer);
begin
  fSynStrings.Attribute[Owner, Index] := AValue;
end;

procedure TSynEditStringsLinked.RegisterAttribute(const Index: TClass; const Size: Word);
begin
  fSynStrings.RegisterAttribute(Index, Size);
end;

procedure TSynEditStringsLinked.ClearRanges(ARange: TSynEditRange);
begin
  fSynStrings.ClearRanges(ARange);
end;

procedure TSynEditStringsLinked.AddChangeHandler(AReason: TSynEditNotifyReason; AHandler: TStringListLineCountEvent);
begin
  fSynStrings.AddChangeHandler(AReason, AHandler);
end;

procedure TSynEditStringsLinked.RemoveChangeHandler(AReason: TSynEditNotifyReason; AHandler: TStringListLineCountEvent);
begin
  fSynStrings.RemoveChangeHandler(AReason, AHandler);
end;

// Count
function TSynEditStringsLinked.GetCount: integer;
begin
  Result:= fSynStrings.Count;
end;

function TSynEditStringsLinked.GetCapacity: integer;
begin
  Result:= fSynStrings.Capacity;
end;

procedure TSynEditStringsLinked.SetCapacity(NewCapacity: integer);
begin
  fSynStrings.Capacity := NewCapacity;
end;

function TSynEditStringsLinked.Get(Index: integer): string;
begin
  Result:= fSynStrings.Get(Index);
end;

function TSynEditStringsLinked.GetObject(Index: integer): TObject;
begin
  Result:= fSynStrings.GetObject(Index);
end;

procedure TSynEditStringsLinked.Put(Index: integer; const S: string);
begin
  fSynStrings.Put(Index, S);
end;

procedure TSynEditStringsLinked.PutObject(Index: integer; AObject: TObject);
begin
  fSynStrings.PutObject(Index, AObject);
end;

procedure TSynEditStringsLinked.SetUpdateState(Updating: Boolean);
begin
  if Updating then
    fSynStrings.BeginUpdate
  else
   fSynStrings.EndUpdate;
end;

end.

