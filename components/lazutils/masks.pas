{
 *****************************************************************************
  This file is part of LazUtils.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit Masks;

{$mode objfpc}{$H+}

// RANGES_AUTOREVERSE
// If reverse ranges if needed, so range "[z-a]" is interpreted as "[a-z]"
{$DEFINE RANGES_AUTOREVERSE}
{$DEFINE USE_INLINE}

interface

uses
  Classes, SysUtils, Contnrs, LazUtilsStrConsts, LazUTF8;

type
  { EMaskError }

  EMaskError=class(EConvertError)
  public
    type
      TMaskExceptionCode=(eMaskException_InternalError,
                          eMaskException_InvalidCharMask,
                          eMaskException_MissingClose,
                          eMaskException_IncompleteMask,
                          eMaskException_InvalidEscapeChar,
                          eMaskException_InvalidUTF8Sequence
                          );
  protected
    cCode: TMaskExceptionCode;
  public
    constructor Create(const msg: string; aCode: TMaskExceptionCode);
    constructor CreateFmt(const msg: string; args: array of const; aCode: TMaskExceptionCode);
    property Code: TMaskExceptionCode read cCode;
  end;

  { TMaskBase }

  TMaskBase = class
  private
    procedure SetMaskEscapeChar(AValue: Char);
  protected

    type
      // Literal = It must match
      // Range = Match any char in the range
      // Negate = Negate match in a group
      // AnyChar = It matches any char, but one must match
      // AnyCharOrNone = Matches one or none char (only in a group)
      // AnyCharToNext = Matches any chars amount, if fail, restart in the
      //                 next position up to finish the mask or the matched string
      // OptionalChar = Optional char
      // CharsGroupBegin = Begin optional chars or ranges "["
      // CharsGroupEnd = End optional chars or ranges "]"
      TMaskOpCode = (
        Literal=0,
        Range=1,
        Negate=2,
        AnyChar=3,
        AnyCharOrNone=4,
        AnyCharToNext=5,
        OptionalChar=6,
        CharsGroupBegin=10,
        CharsGroupEnd=11
      );
      TMaskOpcodesEnum=(eMaskOpcodeAnyChar,
                        eMaskOpcodeAnyCharOrNone,
                        eMaskOpcodeAnyText,
                        eMaskOpcodeRange,
                        eMaskOpcodeOptionalChar,
                        eMaskOpcodeNegateGroup,
                        eMaskOpcodeEscapeChar);
      TMaskOpcodesSet=set of TMaskOpcodesEnum;

      TMaskFailCause = (
        Success = 0,
        MatchStringExhausted = 1,
        MaskExhausted = 2,
        MaskNotMatch = 3,
        UnexpectedEnd = 4
      );
      (*
        Windows mask works in a different mode than regular mask, it has too many
        quirks and corner cases inherited from CP/M, then adapted to DOS (8.3) file
        names and adapted again for long file names.

        Anyth?ng.abc    = "?" matches exactly 1 char
        Anyth*ng.abc    = "*" matches 0 or more of chars

        ------- Quirks -------

        --eWindowsQuirk_AnyExtension
          Anything*.*     = ".*" is removed.

        --eWindowsQuirk_FilenameEnd
          Anything??.abc  = "?" matches 1 or 0 chars (except '.')
                            (Not the same as "Anything*.abc", but the same
                            as regex "Anything.{0,2}\.abc")
                            Internally converted to "Anything[??].abc"

        --eWindowsQuirk_Extension3More
          Anything.abc    = Matches "Anything.abc" but also "Anything.abc*" (3 char extension)
          Anything.ab     = Matches "Anything.ab" and never "anything.abcd"

        --eWindowsQuirk_EmptyIsAny
          ""              = Empty string matches anything "*"

        --eWindowsQuirk_AllByExtension (Not in use anymore)
          .abc            = Runs as "*.abc"

        --eWindowsQuirk_NoExtension
          Anything*.      = Matches "Anything*" without extension
      *)
      TWindowsQuirks=(eWindowsQuirk_AnyExtension, eWindowsQuirk_FilenameEnd,
                      eWindowsQuirk_Extension3More, eWindowsQuirk_EmptyIsAny,
                      eWindowsQuirk_AllByExtension, eWindowsQuirk_NoExtension);
      TWindowsQuirkSet=set of TWindowsQuirks;
    const GROW_BY=100;
          TWindowsQuirksAllAllowed=[eWindowsQuirk_AnyExtension,
                                    eWindowsQuirk_FilenameEnd,
                                    eWindowsQuirk_Extension3More,
                                    eWindowsQuirk_EmptyIsAny,
                                    eWindowsQuirk_AllByExtension,
                                    eWindowsQuirk_NoExtension];
          TWindowsQuirksDefaultAllowed=[eWindowsQuirk_AnyExtension,
                                        eWindowsQuirk_FilenameEnd,
                                        eWindowsQuirk_Extension3More,
                                        eWindowsQuirk_EmptyIsAny,
                                        {eWindowsQuirk_AllByExtension,} // Not in use anymore
                                        eWindowsQuirk_NoExtension];
          TMaskOpCodesAllAllowed=[eMaskOpcodeAnyChar,
                                  eMaskOpcodeAnyCharOrNone,
                                  eMaskOpcodeAnyText,
                                  eMaskOpcodeRange,
                                  eMaskOpcodeOptionalChar,
                                  eMaskOpcodeNegateGroup,
                                  eMaskOpcodeEscapeChar];
          TMaskOpCodesDefaultAllowed=TMaskOpCodesAllAllowed;
  protected
    procedure Add(const aLength: integer; const aData: PBYTE);
    procedure Add(const aValue: integer);{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure Add(const aValue: TMaskOpCode);{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure IncrementLastCounterBy(const aOpcode: TMaskOpCode; const aValue: integer);
  protected
    cCaseSensitive: Boolean;
    cMaskIsCompiled: Boolean;
    cMaskCompiled: TBytes;
    cMaskCompiledIndex: integer;
    cMaskCompiledAllocated: integer;
    cMaskCompiledLimit: integer;
    cMaskLimit: integer;
    cMatchStringLimit: integer;
    cMatchMinimumLiteralBytes: SizeInt;
    cMatchMaximumLiteralBytes: SizeInt;
    cMaskOpcodesAllowed: TMaskOpcodesSet;
    // EscapeChar forces next char to be a literal one, not a wildcard.
    cMaskEscapeChar: Char;
    procedure Compile; virtual;
    class procedure Exception_InvalidCharMask(const aMaskChar: string; const aOffset: integer=-1); static;
    class procedure Exception_MissingCloseChar(const aMaskChar: string; const aOffset: integer=-1); static;
    class procedure Exception_IncompleteMask(); static;
    class procedure Exception_InvalidEscapeChar(); static;
    procedure Exception_InternalError();
    function intfMatches(aMatchOffset: integer; aMaskIndex: integer): TMaskFailCause; virtual; abstract;
    property OPCodesAllowed: TMaskOpcodesSet read cMaskOpcodesAllowed write cMaskOpcodesAllowed;
  public
    constructor Create(aCaseSensitive: Boolean=false);
    constructor CreateAdvanced(aCaseSensitive: Boolean=false; aOpcodesAllowed: TMaskOpcodesSet=TMaskOpCodesAllAllowed);
    property CaseSensitive: Boolean read cCaseSensitive;
    property EscapeChar: Char read cMaskEscapeChar write SetMaskEscapeChar;
  end;

  { TMaskUTF8 }

  TMaskUTF8 = class (TMaskBase)
  private
    cMatchString: RawByteString;
  protected
    cOriginalMask: RawByteString;
    class function CompareUTF8Sequences(const P1,P2: PChar): integer; static;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function intfMatches(aMatchOffset: integer; aMaskIndex: integer): TMaskFailCause; override;
  public
    constructor Create(const aMask: RawByteString; aCaseSensitive: Boolean = False);
    constructor CreateAdvanced(const aMask: RawByteString; aCaseSensitive: Boolean=false;
      aOpcodesAllowed: TMaskOpcodesSet=TMaskOpCodesAllAllowed);
    procedure Compile; override;
    function Matches(const aStringToMatch: RawByteString): Boolean; virtual;
    property Mask: RawByteString read cOriginalMask write cOriginalMask;
    property OPCodesAllowed;
  end;

  TMask = class(TMaskUTF8);

  { TMaskUTF8Windows }

  TMaskUTF8Windows=class(TMask)
  protected
    cMaskWindowsQuirkAllowed: TWindowsQuirkSet;
    cMaskWindowsQuirkInUse: TWindowsQuirkSet;
    cWindowsMask: RawByteString;
    class procedure SplitFileNameExtension(const aSourceFileName: RawByteString;
      out aFileName: RawByteString; out aExtension: RawByteString; aIsMask: Boolean=false); static;
  public
    constructor Create(const aMask: RawByteString; aCaseSensitive: Boolean = False);
    constructor CreateAdvanced(const aMask: RawByteString; aCaseSensitive: Boolean=false;
      aWindowsQuirksAllowed: TWindowsQuirkSet=TWindowsQuirksAllAllowed);
    procedure Compile; override;
    function Matches(const aFileName: RawByteString): Boolean; override;
    property Mask: RawByteString read cWindowsMask write cWindowsMask;
    property Quirks: TWindowsQuirkSet read cMaskWindowsQuirkAllowed write cMaskWindowsQuirkAllowed;
  end;

  TMaskWindows = class(TMaskUTF8Windows);

  { TParseStringList }

  TParseStringList = class(TStringList)
  public
    constructor Create(const AText, ASeparators: String);
  end;

  { TMaskList }

  TMaskList = class
  private
    FMasks: TObjectList;
    function GetCount: Integer;
    function GetItem(Index: Integer): TMask;
  public
    constructor Create(const AValue: String);
    constructor Create(const AValue: String; ASeparator: Char; CaseSensitive: Boolean = False);
    constructor CreateWindows(const AValue: String; ASeparator: Char; CaseSensitive: Boolean);
    constructor CreateSysNative(const AValue: String; ASeparator: Char; CaseSensitive: Boolean);
    destructor Destroy; override;

    function Matches(const AFileName: String): Boolean;
    // Don't call this. Create with TMaskList.CreateWindows, then call Matches.
    function MatchesWindowsMask(const AFileName: String): Boolean;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TMask read GetItem;
  end;

function MatchesMask(const FileName, Mask: String; CaseSensitive: Boolean = False): Boolean;
function MatchesWindowsMask(const FileName, Mask: String; CaseSensitive: Boolean = False): Boolean;

function MatchesMaskList(const FileName, Mask: String): Boolean;
function MatchesMaskList(const FileName, Mask: String; Separator: Char): Boolean;
function MatchesMaskList(const FileName, Mask: String; Separator: Char; CaseSensitive: Boolean): Boolean;
function MatchesWindowsMaskList(const FileName, Mask: String): Boolean;
function MatchesWindowsMaskList(const FileName, Mask: String; Separator: Char): Boolean;
function MatchesWindowsMaskList(const FileName, Mask: String; Separator: Char; CaseSensitive: Boolean): Boolean;


implementation

function MatchesMask(const FileName, Mask: String; CaseSensitive: Boolean): Boolean;
var
  AMask: TMask;
begin
  AMask := TMask.Create(Mask, CaseSensitive);
  try
    Result := AMask.Matches(FileName);
  finally
    AMask.Free;
  end;
end;

function MatchesWindowsMask(const FileName, Mask: String; CaseSensitive: Boolean): Boolean;
var
  AMask: TMaskWindows;
begin
  AMask := TMaskWindows.Create(Mask, CaseSensitive);
  try
    Result := AMask.Matches(FileName);
  finally
    AMask.Free;
  end;
end;

function MatchesMaskList(const FileName, Mask: String): Boolean;
begin
  Result := MatchesMaskList(FileName, Mask, ';', False);
end;

function MatchesMaskList(const FileName, Mask: String; Separator: Char): Boolean;
begin
  Result := MatchesMaskList(FileName, Mask, Separator, False);
end;

function MatchesMaskList(const FileName, Mask: String; Separator: Char; CaseSensitive: Boolean): Boolean;
var
  AMaskList: TMaskList;
begin
  AMaskList := TMaskList.Create(Mask, Separator, CaseSensitive);
  try
    Result := AMaskList.Matches(FileName);
  finally
    AMaskList.Free;
  end;
end;

function MatchesWindowsMaskList(const FileName, Mask: String): Boolean;
begin
  Result := MatchesWindowsMaskList(FileName, Mask, ';', False);
end;

function MatchesWindowsMaskList(const FileName, Mask: String; Separator: Char): Boolean;
begin
  Result := MatchesWindowsMaskList(FileName, Mask, Separator, False);
end;

function MatchesWindowsMaskList(const FileName, Mask: String; Separator: Char; CaseSensitive: Boolean): Boolean;
var
  AMaskList: TMaskList;
begin
  AMaskList := TMaskList.CreateWindows(Mask, Separator, CaseSensitive);
  try
    Result := AMaskList.Matches(FileName);
  finally
    AMaskList.Free;
  end;
end;

{ EMaskError }

constructor EMaskError.Create(const msg: string; aCode: TMaskExceptionCode);
begin
  CreateFmt(msg,[],aCode);
end;

constructor EMaskError.CreateFmt(const msg: string; args: array of const;
  aCode: TMaskExceptionCode);
begin
  cCode:=aCode;
  Inherited CreateFmt(msg,args);
end;

{ TMaskBase }

procedure TMaskBase.SetMaskEscapeChar(AValue: Char);
begin
  if cMaskEscapeChar=AValue then Exit;
  if cMaskEscapeChar>#127 then begin
    Exception_InvalidEscapeChar();
  end;
  cMaskEscapeChar:=AValue;
end;

procedure TMaskBase.Add(const aLength: integer; const aData: PBYTE);
var
  lCounter: integer;
begin
  if cMaskCompiledIndex+aLength>=cMaskCompiledAllocated then begin
    cMaskCompiledAllocated:=cMaskCompiledAllocated+aLength+GROW_BY;
    SetLength(cMaskCompiled,cMaskCompiledAllocated);
  end;
  for lCounter := 0 to Pred(aLength) do begin
    cMaskCompiled[cMaskCompiledIndex]:=(aData+lCounter)^;
    inc(cMaskCompiledIndex);
  end;
end;

procedure TMaskBase.Add(const aValue: integer);
begin
  Add(sizeof(aValue),@aValue);
end;

procedure TMaskBase.Add(const aValue: TMaskOpCode);
var
  v: BYTE;
begin
  v:=BYTE(aValue);
  Add(1,@v);
end;

procedure TMaskBase.IncrementLastCounterBy(const aOpcode: TMaskOpCode;
  const aValue: integer);
var
  p: PINTEGER;
begin
  cMaskCompiledIndex:=cMaskCompiledIndex-sizeof(aValue);
  if TMaskOpCode(cMaskCompiled[cMaskCompiledIndex-1])<>aOpcode then begin
    Exception_InternalError();
  end;
  P:=@cMaskCompiled[cMaskCompiledIndex];
  Add(P^+aValue);
end;

procedure TMaskBase.Compile;
begin
  cMaskIsCompiled:=true;
end;

class procedure TMaskBase.Exception_InvalidCharMask(const aMaskChar: string;
  const aOffset: integer);
begin
  if aOffset>=0 then begin
    raise EMaskError.CreateFmt(rsInvalidCharMaskAt, [aMaskChar, aOffset], eMaskException_InvalidCharMask);
  end else begin
    raise EMaskError.CreateFmt(rsInvalidCharMask, [aMaskChar], eMaskException_InvalidCharMask);
  end;
end;

class procedure TMaskBase.Exception_MissingCloseChar(const aMaskChar: string;
  const aOffset: integer);
begin
  if aOffset>=0 then begin
    raise EMaskError.CreateFmt(rsMissingCloseCharMaskAt, [aMaskChar, aOffset], eMaskException_MissingClose);
  end else begin
    raise EMaskError.CreateFmt(rsMissingCloseCharMask, [aMaskChar], eMaskException_MissingClose);
  end;
end;

class procedure TMaskBase.Exception_IncompleteMask();
begin
  raise EMaskError.CreateFmt(rsIncompleteMask, [], eMaskException_IncompleteMask);
end;

class procedure TMaskBase.Exception_InvalidEscapeChar();
begin
  raise EMaskError.Create(rsInvalidEscapeChar, eMaskException_InvalidEscapeChar);
end;

procedure TMaskBase.Exception_InternalError();
begin
  raise EMaskError.CreateFmt(rsInternalError, [self.ClassName], eMaskException_InternalError);
end;

constructor TMaskBase.CreateAdvanced(aCaseSensitive: Boolean;
  aOpcodesAllowed: TMaskOpcodesSet);
begin
  cMaskOpcodesAllowed:=aOpcodesAllowed;
  cCaseSensitive:=aCaseSensitive;
  cMaskEscapeChar:='\';
end;

constructor TMaskBase.Create(aCaseSensitive: Boolean);
begin
  CreateAdvanced(aCaseSensitive,TMaskOpCodesDefaultAllowed);
end;

{ TMask }

procedure TMaskUTF8.Compile;
var
  j: Integer;
  lCharsGroupInsertSize: integer;
  lCPLength: integer;
  lLast: TMaskOpCode;
  lMask: RawByteString;

begin
  inherited Compile;
  if cCaseSensitive then
    lMask:=cOriginalMask
  else
    lMask:=UTF8LowerCase(cOriginalMask);
  cMaskLimit:=Length(lMask);
  lLast:=TMaskOpCode.Literal;
  SetLength(cMaskCompiled,0);
  j:=1;
  while j<=cMaskLimit do begin
    lCPLength:=UTF8CodepointSizeFast(@lMask[j]);
    if (eMaskOpcodeEscapeChar in cMaskOpcodesAllowed) and (lMask[j]=cMaskEscapeChar) then begin
      // next is Literal
      inc(j,lCPLength);
      if j<=cMaskLimit then begin
        lCPLength:=UTF8CodepointSizeFast(@lMask[j]);
        Add(TMaskOpCode.Literal);
        Add(lCPLength,@lMask[j]);
        inc(cMatchMinimumLiteralBytes,lCPLength);
        if cMatchMaximumLiteralBytes<High(cMatchMaximumLiteralBytes) then inc(cMatchMaximumLiteralBytes,lCPLength);
        lLast:=TMaskOpCode.Literal;
        inc(j,lCPLength);
      end else begin
        Exception_IncompleteMask();
      end;
    end else begin
      if lMask[j] in ['*','?','['] then begin
        case lMask[j] of
          '*':
            begin
              if eMaskOpcodeAnyText in cMaskOpcodesAllowed then begin
                if lLast<>TMaskOpCode.AnyCharToNext then begin
                  Add(TMaskOpCode.AnyCharToNext);
                  lLast:=TMaskOpCode.AnyCharToNext;
                  // * = No limit
                  cMatchMaximumLiteralBytes:=High(cMatchMaximumLiteralBytes);
                end;
              end else begin
                Add(TMaskOpCode.Literal);
                Add(lCPLength,@lMask[j]);
                inc(cMatchMinimumLiteralBytes,lCPLength);
                if cMatchMaximumLiteralBytes<High(cMatchMaximumLiteralBytes) then inc(cMatchMaximumLiteralBytes,lCPLength);
                lLast:=TMaskOpCode.Literal;
              end;
            end;
          '?':
            begin
              if eMaskOpcodeAnyChar in cMaskOpcodesAllowed then begin
                Add(TMaskOpCode.AnyChar);
                inc(cMatchMinimumLiteralBytes,1);
                if cMatchMaximumLiteralBytes<High(cMatchMaximumLiteralBytes) then inc(cMatchMaximumLiteralBytes,4);
                lLast:=TMaskOpCode.AnyChar;
              end else begin
                Add(TMaskOpCode.Literal);
                Add(lCPLength,@lMask[j]);
                inc(cMatchMinimumLiteralBytes,lCPLength);
                if cMatchMaximumLiteralBytes<High(cMatchMaximumLiteralBytes) then inc(cMatchMaximumLiteralBytes,lCPLength);
                lLast:=TMaskOpCode.Literal;
              end;
            end;
          '[':
            begin
              if (eMaskOpcodeOptionalChar in cMaskOpcodesAllowed) or
                 (eMaskOpcodeRange in cMaskOpcodesAllowed) or
                 (eMaskOpcodeAnyCharOrNone in cMaskOpcodesAllowed)
                 then begin
                lLast:=TMaskOpCode.CharsGroupBegin;
                Add(TMaskOpCode.CharsGroupBegin);
                inc(cMatchMinimumLiteralBytes,1);
                if cMatchMaximumLiteralBytes<High(cMatchMaximumLiteralBytes) then inc(cMatchMaximumLiteralBytes,4);
                lCharsGroupInsertSize:=cMaskCompiledIndex;
                Add(0);
                inc(j); // CP length is 1 because it is "["
                if j<cMaskLimit then begin
                  if (lMask[j]='!') and (eMaskOpcodeNegateGroup in cMaskOpcodesAllowed) then begin
                    Add(TMaskOpCode.Negate);
                    inc(j); // CP length is 1 because it is "!"
                    lLast:=TMaskOpCode.Negate;
                  end;
                end;

                while j<=cMaskLimit do begin
                  lCPLength:=UTF8CodepointSizeFast(@lMask[j]);

                  if (lMask[j]='?') and (eMaskOpcodeAnyCharOrNone in cMaskOpcodesAllowed) then begin
                    // This syntax is permitted [??] but not this one [?a] or [a?]
                    if (lLast=TMaskOpCode.CharsGroupBegin) or (lLast=TMaskOpCode.AnyCharOrNone) then begin
                      if lLast=TMaskOpCode.AnyCharOrNone then begin
                        // Increment counter
                        IncrementLastCounterBy(TMaskOpCode.AnyCharOrNone,1);
                      end else begin
                        Add(TMaskOpCode.AnyCharOrNone);
                        Add(1); // Counter
                        // Discount minimun bytes added at the "CharGroupBegin"
                        // because [?] could be 1 or 0 chars, so minimum is zero
                        // but the CharsGroupBegin assumes 1 char as all other
                        // masks replace the group by 1 char position.
                        // This code will run 1 time per group at maximun.
                        dec(cMatchMinimumLiteralBytes,1);
                        if cMatchMaximumLiteralBytes<High(cMatchMaximumLiteralBytes) then dec(cMatchMaximumLiteralBytes,4);
                      end;
                      if cMatchMaximumLiteralBytes<High(cMatchMaximumLiteralBytes) then inc(cMatchMaximumLiteralBytes,4);
                      lLast:=TMaskOpCode.AnyCharOrNone;
                    end else begin
                      Exception_InvalidCharMask(lMask[j],j);
                    end;

                  end else if (lLast=TMaskOpCode.AnyCharOrNone) and (lMask[j]<>']') then begin
                    //lMask[j] is not '?', but previous mask was '?' and it is an invalid sequence.
                    // "[??] = Valid" // "[a?] or [?a] = Invalid"
                    Exception_InvalidCharMask(lMask[j],j);

                  end else if ((j+lCPLength+1)<=cMaskLimit) and (lMask[j+lCPLength]='-') and (eMaskOpcodeRange in cMaskOpcodesAllowed) then begin
                    // j+lCPLength+1 --explained--
                    //------------------------------
                    // j+lCPLength is next UTF8 after current UTF8 CP
                    // +1 is at least one byte in UTF8 sequence after "-"
                    // Check if it is a range
                    Add(TMaskOpCode.Range);
                    // Check if reverse range is needed
                    {$IFDEF RANGES_AUTOREVERSE}
                    if CompareUTF8Sequences(@lMask[j],@lMask[j+lCPLength+1])<0 then begin
                      Add(lCPLength,@lMask[j]);
                      inc(j,lCPLength);
                      inc(j,1); // The "-"
                      lCPLength:=UTF8CodepointSizeFast(@lMask[j]);
                      Add(lCPLength,@lMask[j]);
                    end else begin
                      Add(UTF8CodepointSizeFast(@lMask[j+lCPLength+1]),@lMask[j+lCPLength+1]);
                      Add(lCPLength,@lMask[j]);
                      inc(j,lCPLength+1);
                      lCPLength:=UTF8CodepointSizeFast(@lMask[j]);
                    end;
                    {$ELSE}
                      Add(lCPLength,@lMask[j]);
                      inc(j,lCPLength);
                      inc(j,1); // The "-"
                      lCPLength:=UTF8CodepointSizeFast(@lMask[j]);
                      Add(lCPLength,@lMask[j]);
                    {$ENDIF}
                    lLast:=TMaskOpCode.Range;

                  end else if lMask[j]=']' then begin
                    if lLast=TMaskOpCode.CharsGroupBegin then begin
                      //Error empty match
                      Exception_InvalidCharMask(lMask[j],j);
                    end;
                    // Insert the new offset in case of a positive match in CharsGroup
                    PInteger(@cMaskCompiled[lCharsGroupInsertSize])^:=cMaskCompiledIndex;
                    Add(TMaskOpCode.CharsGroupEnd);
                    lLast:=TMaskOpCode.CharsGroupEnd;
                    break;
                  end else begin
                    Add(TMaskOpCode.OptionalChar);
                    Add(lCPLength,@lMask[j]);
                    lLast:=TMaskOpCode.OptionalChar;
                  end;
                  inc(j,lCPLength);
                end;
                if j>cMaskLimit then begin
                  Exception_MissingCloseChar(']',cMaskLimit);
                end;
              end else begin
                Add(TMaskOpCode.Literal);
                Add(lCPLength,@lMask[j]);
                inc(cMatchMinimumLiteralBytes,lCPLength);
                if cMatchMaximumLiteralBytes<High(cMatchMaximumLiteralBytes) then inc(cMatchMaximumLiteralBytes,lCPLength);
                lLast:=TMaskOpCode.Literal;
              end;
            end;
        end;
      end else begin
        // Literal
        Add(TMaskOpCode.Literal);
        Add(lCPLength,@lMask[j]);
        inc(cMatchMinimumLiteralBytes,lCPLength);
        if cMatchMaximumLiteralBytes<High(cMatchMaximumLiteralBytes) then inc(cMatchMaximumLiteralBytes,lCPLength);
        lLast:=TMaskOpCode.Literal;
      end;
      inc(j,lCPLength);
    end;
  end;
  SetLength(cMaskCompiled,cMaskCompiledIndex);
  cMaskCompiledLimit:=cMaskCompiledIndex-1;
end;

class function TMaskUTF8.CompareUTF8Sequences(const P1, P2: PChar): integer;
var
  l1,l2: integer;
  l: integer;
begin
  l1:=UTF8CodepointSizeFast(p1);
  l2:=UTF8CodepointSizeFast(p2);
  Result:=0;
  l:=0;
  while (l<l1) and (l<l2) do begin
    Result:=Integer((P1+l)^)-integer((P2+l)^);
    if Result<>0 then exit;
    inc(l);
  end;
  Result:=l1-l2;
end;

function TMaskUTF8.intfMatches(aMatchOffset: integer; aMaskIndex: integer): TMaskFailCause;
var
  c1,c2: PChar;
  lFailCause: TMaskFailCause;
  lNegateCharGroup: Boolean;
  lSkipOnSuccessGroup: integer;
  t1: Boolean;
  j: integer;
  lTryCounter: integer;
begin
  lSkipOnSuccessGroup:=0;
  Result:=UnexpectedEnd;
  lNegateCharGroup:=false;
  while aMaskIndex<=cMaskCompiledLimit do begin
    case TMaskOpCode(cMaskCompiled[aMaskIndex]) of
      TMaskOpCode.Literal:
        begin
          if aMatchOffset>cMatchStringLimit then begin
            // Error, no char to match.
            Result:=TMaskFailCause.MatchStringExhausted;
            exit;
          end;
          inc(aMaskIndex);
          if CompareUTF8Sequences(@cMaskCompiled[aMaskIndex],@cMatchString[aMatchOffset])<>0 then begin
            Result:=TMaskFailCause.MaskNotMatch;
            Exit;
          end;
          inc(aMaskIndex,UTF8CodepointSizeFast(@cMaskCompiled[aMaskIndex]));
          inc(aMatchOffset,UTF8CodepointSizeFast(@cMatchString[aMatchOffset]));
        end;
      TMaskOpCode.AnyChar:
        begin
          inc(aMaskIndex);
          if aMatchOffset>cMatchStringLimit then begin
            // Error, no char to match.
            Result:=TMaskFailCause.MatchStringExhausted;
            exit;
          end;
          inc(aMatchOffset,UTF8CodepointSizeFast(@cMatchString[aMatchOffset]));
        end;
      TMaskOpCode.Negate:
        begin
          lNegateCharGroup:=true;
          inc(aMaskIndex);
        end;
      TMaskOpCode.CharsGroupBegin:
        begin
          lNegateCharGroup:=false;
          inc(aMaskIndex);
          lSkipOnSuccessGroup:=PInteger(@cMaskCompiled[aMaskIndex])^;
          inc(aMaskIndex,sizeof(integer));
        end;
      TMaskOpCode.CharsGroupEnd:
        begin
          if lNegateCharGroup then begin
            aMaskIndex:=lSkipOnSuccessGroup+1;
            inc(aMatchOffset,UTF8CodepointSizeFast(@cMatchString[aMatchOffset]));
          end else begin
            Result:=TMaskFailCause.MaskNotMatch;
            exit;
          end;
        end;
      TMaskOpCode.OptionalChar:
        begin
          inc(aMaskIndex);
          if aMatchOffset>cMatchStringLimit then begin
            // Error, no char to match.
            Result:=TMaskFailCause.MatchStringExhausted;
            exit;
          end;
          if CompareUTF8Sequences(@cMaskCompiled[aMaskIndex],@cMatchString[aMatchOffset])=0 then begin
            if lNegateCharGroup then begin
              Result:=TMaskFailCause.MaskNotMatch;
              exit;
            end;
            aMaskIndex:=lSkipOnSuccessGroup+1;
            inc(aMatchOffset,UTF8CodepointSizeFast(@cMatchString[aMatchOffset]));
          end else begin
            inc(aMaskIndex,UTF8CodepointSizeFast(@cMaskCompiled[aMaskIndex]));
          end;
        end;
      TMaskOpCode.Range:
        begin
          if aMatchOffset>cMatchStringLimit then begin
            // Error, no char to match.
            Result:=TMaskFailCause.MatchStringExhausted;
            exit;
          end;
          inc(aMaskIndex);
          c1:=@cMaskCompiled[aMaskIndex];
          inc(aMaskIndex,UTF8CodepointSizeFast(C1));
          c2:=@cMaskCompiled[aMaskIndex];
          inc(aMaskIndex,UTF8CodepointSizeFast(C2));
          t1:=(CompareUTF8Sequences(@cMatchString[aMatchOffset],c1)>=0) and
              (CompareUTF8Sequences(@cMatchString[aMatchOffset],c2)<=0);
          if t1 then begin
            if not lNegateCharGroup then begin
              //Jump to CharsGroupEnd+1 because if CharsGroupEnd is reached
              //it means that all optional chars and ranges have not matched the string.
              aMaskIndex:=lSkipOnSuccessGroup+1;
              inc(aMatchOffset,UTF8CodepointSizeFast(@cMatchString[aMatchOffset]));
            end else begin
              Result:=TMaskFailCause.MaskNotMatch;
              exit;
            end;
          end
        end;
      TMaskOpCode.AnyCharToNext:
        begin
          // if last is "*", everything in remain data matches
          if aMaskIndex=cMaskCompiledLimit then begin
            Result:=TMaskFailCause.Success;
            exit;
          end;
          if aMatchOffset>cMatchStringLimit then begin
            if aMaskIndex=cMaskCompiledLimit then begin
              Result:=TMaskFailCause.Success;
              exit;
            end;
            Result:=TMaskFailCause.MatchStringExhausted;
            exit;
          end;
          inc(aMaskIndex);
          while aMatchOffset<=cMatchStringLimit do begin
            lFailCause:=intfMatches(aMatchOffset,aMaskIndex);
            if lFailCause=TMaskFailCause.Success then begin
              Result:=TMaskFailCause.Success;
              exit;
            end else if lFailCause=TMaskFailCause.MatchStringExhausted then begin
              Result:=TMaskFailCause.MatchStringExhausted;
              exit;
            end;
            inc(aMatchOffset,UTF8CodepointSizeFast(@cMatchString[aMatchOffset]));
          end;
          Result:=TMaskFailCause.MatchStringExhausted;
          exit;
        end;
      TMaskOpCode.AnyCharOrNone:
        begin
          inc(aMaskIndex);
          lTryCounter:=PInteger(@cMaskCompiled[aMaskIndex])^;
          inc(aMaskIndex,sizeof(integer));
          if TMaskOpCode(cMaskCompiled[aMaskIndex])<>TMaskOpCode.CharsGroupEnd then begin
            Exception_InternalError();
          end else begin
            aMaskIndex:=lSkipOnSuccessGroup+1;
          end;

          // Try to match remain mask eating, 0,1,2,...,lTryCounter chars.
          for j := 0 to lTryCounter do begin
            if aMatchOffset>cMatchStringLimit then begin
              if aMaskIndex=cMaskCompiledLimit+1 then begin
                Result:=TMaskFailCause.Success;
                exit;
              end;
              Result:=TMaskFailCause.MatchStringExhausted;
              exit;
            end;
            lFailCause:=intfMatches(aMatchOffset,aMaskIndex);
            if lFailCause=TMaskFailCause.Success then begin
              Result:=TMaskFailCause.Success;
              exit;
            end else if lFailCause=TMaskFailCause.MatchStringExhausted then begin
              Result:=TMaskFailCause.MatchStringExhausted;
              exit;
            end;
            inc(aMatchOffset,UTF8CodepointSizeFast(@cMatchString[aMatchOffset]));
          end;
          Result:=TMaskFailCause.MatchStringExhausted;
          exit;
        end;
      else
        begin
          Exception_InternalError();
        end;
    end;
  end;
  if (aMaskIndex>cMaskCompiledLimit) and (aMatchOffset>cMatchStringLimit) then begin
    Result:=TMaskFailCause.Success;
  end else begin
    if aMaskIndex>cMaskCompiledLimit then begin
      Result:=TMaskFailCause.MaskExhausted;
    end else begin
      Result:=TMaskFailCause.MatchStringExhausted;
    end;
  end;
end;

constructor TMaskUTF8.Create(const aMask: RawByteString; aCaseSensitive: Boolean);
begin
  inherited Create(aCaseSensitive);
  cOriginalMask:=aMask;
end;

constructor TMaskUTF8.CreateAdvanced(const aMask: RawByteString;
  aCaseSensitive: Boolean; aOpcodesAllowed: TMaskOpcodesSet);
begin
  inherited CreateAdvanced(aCaseSensitive,aOpcodesAllowed);
  cOriginalMask:=aMask;
end;

function TMaskUTF8.Matches(const aStringToMatch: RawByteString): Boolean;
begin
  if not cMaskIsCompiled then Compile;
  if cCaseSensitive then
    cMatchString:=aStringToMatch
  else
    cMatchString:=UTF8LowerCase(aStringToMatch);
  cMatchStringLimit:=length(cMatchString);
  if (cMatchStringLimit>=cMatchMinimumLiteralBytes)
  and (cMatchStringLimit<=cMatchMaximumLiteralBytes) then
    Result:=intfMatches(1,0)=TMaskFailCause.Success
  else
    Result:=false; // There are too many or not enough bytes to match the string
end;

{ TMaskWindows }

class procedure TMaskUTF8Windows.SplitFileNameExtension(
  const aSourceFileName: RawByteString; out aFileName: RawByteString;
  out aExtension: RawByteString; aIsMask: Boolean);
var
  j: Integer;
  lLowLimit: integer;
begin
  // Default values
  aFileName:=aSourceFileName;
  aExtension:='';

  // This is because .foo is considered a file name ".foo" as one.
  if aIsMask then begin
    lLowLimit:=0;
  end else begin
    lLowLimit:=1;
  end;

  j:=Length(aSourceFileName);
  while j>lLowLimit do begin
    if aSourceFileName[j]='.' then begin
      aFileName:=copy(aSourceFileName,1,j-1);
      aExtension:=copy(aSourceFileName,j);
      break;
    end;
    dec(j);
  end;
end;

constructor TMaskUTF8Windows.Create(const aMask: RawByteString; aCaseSensitive: Boolean);
begin
  CreateAdvanced(aMask,aCaseSensitive,TWindowsQuirksDefaultAllowed);
  Compile;
end;

constructor TMaskUTF8Windows.CreateAdvanced(const aMask: RawByteString;
  aCaseSensitive: Boolean; aWindowsQuirksAllowed: TWindowsQuirkSet);
begin
  cMaskWindowsQuirkAllowed:=aWindowsQuirksAllowed;
  cWindowsMask:=aMask;
  inherited CreateAdvanced(aMask,aCaseSensitive,TMaskOpCodesAllAllowed);
end;

procedure TMaskUTF8Windows.Compile;

  function OptionalQMarksAtEnd(aMask: RawByteString): RawByteString;
  var
    lCounter: integer;
    k: integer;
  begin
    lCounter:=0;
    for k := Length(aMask) downto 1 do begin
      if aMask[k]='?' then begin
        inc(lCounter);
      end else begin
        break;
      end;
    end;
    if lCounter>0 then begin
      aMask:=copy(aMask,1,Length(aMask)-lCounter)+'['+StringOfChar('?',lCounter)+']';
    end;
    Result:=aMask;
  end;

  function EscapeSpecialChars(const aString: RawByteString): RawByteString;
  var
    j: integer;
  begin
    Result:=aString;
    for j := Length(Result) downto 1 do begin
      if Result[j] in ['[',']',cMaskEscapeChar] then begin
        // Escape the []\ chars as in Windows mask mode they are plain chars.
        insert(cMaskEscapeChar,Result,j);
      end;
    end;
  end;

var
  lFileNameMask: RawByteString;
  lExtensionMask: RawByteString;
  lModifiedMask: RawByteString;

begin
  lModifiedMask:=cWindowsMask;

  // Quirk "blah.*" = "blah*"
  if eWindowsQuirk_AnyExtension in cMaskWindowsQuirkAllowed then begin
    if RightStr(lModifiedMask,3)='*.*' then begin
      lModifiedMask:=copy(lModifiedMask,1,Length(lModifiedMask)-2);
      cMaskWindowsQuirkInUse:=cMaskWindowsQuirkInUse+[eWindowsQuirk_AnyExtension];
    end;
  end;

  SplitFileNameExtension(lModifiedMask,lFileNameMask,lExtensionMask,true);

  // Quirk "blah.abc" = "blah.abc*"
  if eWindowsQuirk_Extension3More in cMaskWindowsQuirkAllowed then begin
    if (Length(lExtensionMask)=4) and (Length(lFileNameMask)>0) then begin
      lExtensionMask:=lExtensionMask+'*';
      cMaskWindowsQuirkInUse:=cMaskWindowsQuirkInUse+[eWindowsQuirk_Extension3More];
    end;
  end;

  // Quirk "" = "*"
  if (Length(lFileNameMask)=0) and (Length(lExtensionMask)=0) then begin
    if eWindowsQuirk_EmptyIsAny in cMaskWindowsQuirkAllowed then begin
      lFileNameMask:='*';
      cMaskWindowsQuirkInUse:=cMaskWindowsQuirkInUse+[eWindowsQuirk_EmptyIsAny];
    end;
  end else begin
  // Quirk ".abc"
    if eWindowsQuirk_AllByExtension in cMaskWindowsQuirkAllowed then begin
      if (Length(lFileNameMask)=0) and (length(lExtensionMask)>0) then begin
        if lExtensionMask[1]='.' then begin
          lFileNameMask:='*';
          cMaskWindowsQuirkInUse:=cMaskWindowsQuirkInUse+[eWindowsQuirk_AllByExtension];
        end;
      end;
    end;
  end;

  lFileNameMask:=EscapeSpecialChars(lFileNameMask);
  lExtensionMask:=EscapeSpecialChars(lExtensionMask);

  // Quirk "file???.ab?" matches "file1.ab1" and "file123.ab"
  if eWindowsQuirk_FilenameEnd in cMaskWindowsQuirkAllowed then begin
    lFileNameMask:=OptionalQMarksAtEnd(lFileNameMask);
    lExtensionMask:=OptionalQMarksAtEnd(lExtensionMask);
  end;

  if eWindowsQuirk_NoExtension in cMaskWindowsQuirkAllowed then begin
    if Length(lExtensionMask)=1 then begin
      cMaskWindowsQuirkInUse:=[eWindowsQuirk_NoExtension];
      lExtensionMask:='';
    end;
  end;

  inherited Mask:=lFileNameMask+lExtensionMask;
  inherited Compile;
end;

function TMaskUTF8Windows.Matches(const aFileName: RawByteString): Boolean;
var
  lFileName, lExtension: RawByteString;
begin
  if eWindowsQuirk_NoExtension in cMaskWindowsQuirkInUse then begin
    SplitFileNameExtension(aFileName,lFileName,lExtension,false);
    // eWindowsQuirk_NoExtension = Empty extension
    if lExtension<>'' then exit(false);
  end;
  Result:=Inherited Matches(aFileName);
end;


{ TParseStringList }

constructor TParseStringList.Create(const AText, ASeparators: String);
var
  I, S: Integer;
begin
  inherited Create;
  S := 1;
  for I := 1 to Length(AText) do
  begin
    if Pos(AText[I], ASeparators) > 0 then
    begin
      if I > S then
        Add(Copy(AText, S, I - S));
      S := I + 1;
    end;
  end;
  if Length(AText) >= S then
    Add(Copy(AText, S, Length(AText) - S + 1));
end;

{ TMaskList }

constructor TMaskList.Create(const AValue: String);
begin
  Create(AValue, ';', False);
end;

constructor TMaskList.Create(const AValue: String; ASeparator: Char; CaseSensitive: Boolean);
var
  S: TParseStringList;
  I: Integer;
begin
  FMasks := TObjectList.Create(True);
  S := TParseStringList.Create(AValue, ASeparator);
  try
    for I := 0 to S.Count - 1 do
      FMasks.Add(TMask.Create(S[I], CaseSensitive));
  finally
    S.Free;
  end;
end;

constructor TMaskList.CreateWindows(const AValue: String; ASeparator: Char; CaseSensitive: Boolean);
var
  S: TParseStringList;
  I: Integer;
begin
  FMasks := TObjectList.Create(True);
  S := TParseStringList.Create(AValue, ASeparator);
  try
    for I := 0 to S.Count - 1 do
      FMasks.Add(TMaskWindows.Create(S[I], CaseSensitive));
  finally
    S.Free;
  end;
end;

constructor TMaskList.CreateSysNative(const AValue: String; ASeparator: Char; CaseSensitive: Boolean);
begin
  {$IFDEF Windows}
  CreateWindows(AValue, ASeparator, CaseSensitive);
  {$ELSE}
  Create(AValue, ASeparator, CaseSensitive);
  {$ENDIF}
end;

destructor TMaskList.Destroy;
begin
  FMasks.Free;
  inherited Destroy;
end;

function TMaskList.GetItem(Index: Integer): TMask;
begin
  Result := TMask(FMasks.Items[Index]);
end;

function TMaskList.GetCount: Integer;
begin
  Result := FMasks.Count;
end;

function TMaskList.Matches(const AFileName: String): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to FMasks.Count - 1 do
  begin
    if TMask(FMasks.Items[I]).Matches(AFileName) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TMaskList.MatchesWindowsMask(const AFileName: String): Boolean;
begin
  raise Exception.Create('Create with TMaskList.CreateWindows, then call Matches.');
end;

end.

