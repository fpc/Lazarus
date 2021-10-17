{
 *****************************************************************************
  This file is part of LazUtils.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Implement TMask and related classes.
 It is fast and optimized, and fully supports Unicode. Also supports
  DOS/Windows compatible mask which behaves differently from standard mask.

 Author: José Mejuto
 Changes and improvements by Juha Manninen
}
unit Masks;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs,
  // LazUtils
  LazUtilsStrConsts, LazUTF8;

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
      *.pas           = Matches "Unit1.pas.bak". Not good.

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

const
  WindowsQuirksAllAllowed=[eWindowsQuirk_AnyExtension,
                           eWindowsQuirk_FilenameEnd,
                           eWindowsQuirk_Extension3More,
                           eWindowsQuirk_EmptyIsAny,
                           eWindowsQuirk_AllByExtension,
                           eWindowsQuirk_NoExtension];

  // Leave out eWindowsQuirk_Extension3More and eWindowsQuirk_AllByExtension
  WindowsQuirksDefaultAllowed=[eWindowsQuirk_AnyExtension,
                               eWindowsQuirk_FilenameEnd,
                               eWindowsQuirk_EmptyIsAny,
                               eWindowsQuirk_NoExtension];

  MaskOpCodesAllAllowed=[eMaskOpcodeAnyChar,
                         eMaskOpcodeAnyCharOrNone,
                         eMaskOpcodeAnyText,
                         eMaskOpcodeRange,
                         eMaskOpcodeOptionalChar,
                         eMaskOpcodeNegateGroup,
                         eMaskOpcodeEscapeChar];

  MaskOpCodesDefaultAllowed=MaskOpCodesAllAllowed;

  // Match [x] literally, not as a range.
  // Leave out eMaskOpcodeAnyCharOrNone, eMaskOpcodeRange and eMaskOpcodeOptionalChar.
  MaskOpCodesDisableRange=[eMaskOpcodeAnyChar,
                           eMaskOpcodeAnyText,
                           eMaskOpcodeNegateGroup,
                           eMaskOpcodeEscapeChar];

  // Interpret [?] as literal question mark instead of 0..1 chars wildcard.
  // Disable backslash escaping characters like "\?".
  // Leave out eMaskOpcodeAnyCharOrNone and eMaskOpcodeEscapeChar
  MaskOpCodesNoEscape=[eMaskOpcodeAnyChar,
                       eMaskOpcodeAnyText,
                       eMaskOpcodeRange,
                       eMaskOpcodeOptionalChar,
                       eMaskOpcodeNegateGroup];

type

  // Backwards compatible options.
  TMaskOption = (moCaseSensitive, moDisableSets);
  TMaskOptions = set of TMaskOption;

  { TMaskBase }

  TMaskBase = class
  private
    procedure SetMaskEscapeChar(AValue: Char);
  protected
    const GROW_BY=100;
    procedure Add(const aLength: integer; const aData: PBYTE);
    procedure Add(const aValue: integer);
    procedure Add(const aValue: TMaskOpCode);
    procedure IncrementLastCounterBy(const aOpcode: TMaskOpCode; const aValue: integer);
  protected
    cCaseSensitive: Boolean;
    cRangeAutoReverse: Boolean; // If enabled, range [z-a] is interpreted as [a-z]
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
    //function intfMatches(aMatchOffset: integer; aMaskIndex: integer): TMaskFailCause; virtual; abstract;
  public
    constructor Create(aCaseSensitive: Boolean=False;
      aOpcodesAllowed: TMaskOpcodesSet=MaskOpCodesDefaultAllowed);
    constructor CreateLegacy(aCaseSensitive: Boolean=False);
    constructor Create(aOptions: TMaskOptions);
  public
    property CaseSensitive: Boolean read cCaseSensitive;
    property RangeAutoReverse: Boolean read cRangeAutoReverse write cRangeAutoReverse;
    property EscapeChar: Char read cMaskEscapeChar write SetMaskEscapeChar;
  end;

  { TMaskUTF8 }

  TMaskUTF8 = class (TMaskBase)
  private
    cMatchString: String;
    // Used by Compile.
    FMaskInd: Integer;
    FCPLength: integer;    // Size of codepoint.
    FLastOC: TMaskOpCode;  // Last OpCode.
    FMask: String;
    procedure AddAnyChar;
    procedure AddLiteral;
    procedure AddRange;
    procedure CompileRange;
    procedure ReverseRange;
  protected
    cOriginalMask: String;
    class function CompareUTF8Sequences(const P1,P2: PChar): integer; static;
    function intfMatches(aMatchOffset: integer; aMaskIndex: integer): TMaskFailCause; //override;
  public
    constructor Create(const aMask: String; aCaseSensitive: Boolean=False;
      aOpcodesAllowed: TMaskOpcodesSet=MaskOpCodesDefaultAllowed);
    constructor CreateLegacy(const aMask: String; aCaseSensitive: Boolean);
    constructor Create(const aMask: String; aOptions: TMaskOptions);
        deprecated 'Use CreateLegacy or Create with other params.'; // in Lazarus 2.3, remove in 2.5.

    procedure Compile; override;
    function Matches(const aStringToMatch: String): Boolean; virtual;
    function MatchesWindowsMask(const AFileName: String): Boolean;
        deprecated 'Create with TMaskWindows.Create, then call Matches.'; // in Lazarus 2.3, remove in 2.5.
  public
    property Mask: String read cOriginalMask write cOriginalMask;
    property OPCodesAllowed: TMaskOpcodesSet read cMaskOpcodesAllowed;// write cMaskOpcodesAllowed;
  end;

  TMask = class(TMaskUTF8);

  { TMaskUTF8Windows }

  TMaskUTF8Windows=class(TMask)
  protected
    cMaskWindowsQuirkAllowed: TWindowsQuirkSet;
    cMaskWindowsQuirkInUse: TWindowsQuirkSet;
    cWindowsMask: String;
    class procedure SplitFileNameExtension(const aSourceFileName: String;
      out aFileName: String; out aExtension: String; aIsMask: Boolean=False); static;
  public
    constructor Create(const aMask: String; aCaseSensitive: Boolean=False;
      aOpcodesAllowed: TMaskOpcodesSet=MaskOpCodesDefaultAllowed;
      aWindowsQuirksAllowed: TWindowsQuirkSet=WindowsQuirksDefaultAllowed);

    procedure Compile; override;
    function Matches(const aFileName: String): Boolean; override;
  public
    property Mask: String read cWindowsMask write cWindowsMask;
    property Quirks: TWindowsQuirkSet read cMaskWindowsQuirkAllowed;// write cMaskWindowsQuirkAllowed;
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
    // Creating also Windows masks is a hack needed for deprecated MatchesWindowsMask.
    FWindowsMasks: TObjectList;
    function GetCount: Integer;
    function GetItem(Index: Integer): TMask;
  public
    constructor Create(const aValue: String; aSeparator: Char=';'; CaseSensitive: Boolean=False;
      aOpcodesAllowed: TMaskOpcodesSet=MaskOpCodesDefaultAllowed);
    constructor Create(const aValue: String; aSeparator: Char; aOptions: TMaskOptions);
    constructor CreateWindows(const aValue: String; aSeparator: Char=';'; CaseSensitive: Boolean=False;
      aOpcodesAllowed: TMaskOpcodesSet=MaskOpCodesDefaultAllowed);
    constructor CreateWindows(const aValue: String; aSeparator: Char; aOptions: TMaskOptions);
    constructor CreateSysNative(const aValue: String; aSeparator: Char; CaseSensitive: Boolean);
    destructor Destroy; override;

    function Matches(const AFileName: String): Boolean;
    // Deprecated in Lazarus 2.3, October 2021. Remove in 2.5.
    function MatchesWindowsMask(const AFileName: String): Boolean;
      deprecated 'Create with TMaskList.CreateWindows, then call Matches.';

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TMask read GetItem;
  end;

function MatchesMask(const FileName, Mask: String; CaseSensitive: Boolean=False;
  aOpcodesAllowed: TMaskOpcodesSet=MaskOpCodesDefaultAllowed): Boolean;
function MatchesMaskLegacy(const FileName, Mask: String; CaseSensitive: Boolean=False): Boolean;
function MatchesMask(const FileName, Mask: String; Options: TMaskOptions): Boolean;
    deprecated 'Use MatchesMaskLegacy or MatchesMask with other params.'; // in Lazarus 2.3, remove in 2.5.

function MatchesWindowsMask(const FileName, Mask: String; CaseSensitive: Boolean=False;
  aOpcodesAllowed: TMaskOpcodesSet=MaskOpCodesDefaultAllowed;
  aWindowsQuirksAllowed: TWindowsQuirkSet=WindowsQuirksDefaultAllowed): Boolean;
function MatchesWindowsMask(const FileName, Mask: String; Options: TMaskOptions): Boolean;

function MatchesMaskList(const FileName, Mask: String; Separator: Char=';';
  CaseSensitive: Boolean=False;
  aOpcodesAllowed: TMaskOpcodesSet=MaskOpCodesDefaultAllowed): Boolean;
function MatchesMaskList(const FileName, Mask: String; Separator: Char;
  Options: TMaskOptions): Boolean;
    deprecated 'Use MatchesMaskList with other params.'; // in Lazarus 2.3, remove in 2.5.

function MatchesWindowsMaskList(const FileName, Mask: String; Separator: Char=';';
  CaseSensitive: Boolean=False;
  aOpcodesAllowed: TMaskOpcodesSet=MaskOpCodesDefaultAllowed): Boolean;
function MatchesWindowsMaskList(const FileName, Mask: String; Separator: Char;
  Options: TMaskOptions): Boolean;
    deprecated 'Use MatchesWindowsMaskList with other params.'; // in Lazarus 2.3, remove in 2.5.


implementation

function EncodeDisableRange(Options: TMaskOptions): TMaskOpcodesSet;
// Encode the Disable Range option from legacy TMaskOptions.
begin
  if moDisableSets in Options then
    Result:=MaskOpCodesDisableRange
  else  // Disable '\' escaping for legacy code.
    Result:=MaskOpCodesNoEscape; //MaskOpCodesDefaultAllowed;
end;

function MatchesMask(const FileName, Mask: String; CaseSensitive: Boolean;
  aOpcodesAllowed: TMaskOpcodesSet): Boolean;
var
  AMask: TMask;
begin
  AMask := TMask.Create(Mask, CaseSensitive, aOpcodesAllowed);
  try
    Result := AMask.Matches(FileName);
  finally
    AMask.Free;
  end;
end;

function MatchesMaskLegacy(const FileName, Mask: String; CaseSensitive: Boolean): Boolean;
// Use [?] syntax for literal '?', no escaping chars with '\'.
begin
  Result := MatchesMask(FileName, Mask, CaseSensitive, MaskOpCodesNoEscape);
end;

function MatchesMask(const FileName, Mask: String; Options: TMaskOptions): Boolean;
begin
  Result := MatchesMask(FileName, Mask, moCaseSensitive in Options,
                        EncodeDisableRange(Options));
end;

function MatchesWindowsMask(const FileName, Mask: String; CaseSensitive: Boolean;
  aOpcodesAllowed: TMaskOpcodesSet; aWindowsQuirksAllowed: TWindowsQuirkSet): Boolean;
var
  AMask: TMaskWindows;
begin
  AMask := TMaskWindows.Create(Mask, CaseSensitive, aOpcodesAllowed, aWindowsQuirksAllowed);
  try
    Result := AMask.Matches(FileName);
  finally
    AMask.Free;
  end;
end;

function MatchesWindowsMask(const FileName, Mask: String; Options: TMaskOptions): Boolean;
begin
  Result := MatchesWindowsMask(FileName, Mask, moCaseSensitive in Options,
                               EncodeDisableRange(Options));
end;

function MatchesMaskList(const FileName, Mask: String; Separator: Char;
  CaseSensitive: Boolean; aOpcodesAllowed: TMaskOpcodesSet): Boolean;
var
  AMaskList: TMaskList;
begin
  AMaskList := TMaskList.Create(Mask, Separator, CaseSensitive, aOpcodesAllowed);
  try
    Result := AMaskList.Matches(FileName);
  finally
    AMaskList.Free;
  end;
end;

function MatchesMaskList(const FileName, Mask: String; Separator: Char;
  Options: TMaskOptions): Boolean;
begin
  Result := MatchesMaskList(FileName, Mask, Separator, moCaseSensitive in Options,
                            EncodeDisableRange(Options));
end;

function MatchesWindowsMaskList(const FileName, Mask: String; Separator: Char;
  CaseSensitive: Boolean; aOpcodesAllowed: TMaskOpcodesSet): Boolean;
var
  AMaskList: TMaskList;
begin
  AMaskList := TMaskList.CreateWindows(Mask, Separator, CaseSensitive, aOpcodesAllowed);
  try
    Result := AMaskList.Matches(FileName);
  finally
    AMaskList.Free;
  end;
end;

function MatchesWindowsMaskList(const FileName, Mask: String; Separator: Char;
  Options: TMaskOptions): Boolean;
begin
  Result := MatchesWindowsMaskList(FileName, Mask, Separator, moCaseSensitive in Options,
                                   EncodeDisableRange(Options));
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
  if cMaskEscapeChar>#127 then
    Exception_InvalidEscapeChar();
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
  p: PInteger;
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

constructor TMaskBase.Create(aCaseSensitive: Boolean; aOpcodesAllowed: TMaskOpcodesSet);
begin
  cCaseSensitive:=aCaseSensitive;
  cMaskOpcodesAllowed:=aOpcodesAllowed;
  cRangeAutoReverse:=True;
  cMaskEscapeChar:='\';
end;

constructor TMaskBase.CreateLegacy(aCaseSensitive: Boolean);
// Use [?] syntax for literal '?', no escaping chars with '\'.
begin
  Create(aCaseSensitive, MaskOpCodesNoEscape);
end;

constructor TMaskBase.Create(aOptions: TMaskOptions);
begin
  Create(moCaseSensitive in aOptions, EncodeDisableRange(aOptions));
end;

{ TMask }

procedure TMaskUTF8.AddAnyChar;
begin
  Add(TMaskOpCode.AnyChar);
  inc(cMatchMinimumLiteralBytes,1);
  if cMatchMaximumLiteralBytes<High(cMatchMaximumLiteralBytes) then
    inc(cMatchMaximumLiteralBytes,4);
  FLastOC:=TMaskOpCode.AnyChar;
end;

procedure TMaskUTF8.AddLiteral;
begin
  Add(TMaskOpCode.Literal);
  Add(FCPLength,@FMask[FMaskInd]);
  inc(cMatchMinimumLiteralBytes,FCPLength);
  if cMatchMaximumLiteralBytes<High(cMatchMaximumLiteralBytes) then
    inc(cMatchMaximumLiteralBytes,FCPLength);
  FLastOC:=TMaskOpCode.Literal;
end;

procedure TMaskUTF8.AddRange;
begin
  Add(FCPLength,@FMask[FMaskInd]);
  inc(FMaskInd,FCPLength+1);  // Add 1 for "-"
  FCPLength:=UTF8CodepointSizeFast(@FMask[FMaskInd]);
  Add(FCPLength,@FMask[FMaskInd]);
end;

procedure TMaskUTF8.ReverseRange;
begin
  Add(UTF8CodepointSizeFast(@FMask[FMaskInd+FCPLength+1]),@FMask[FMaskInd+FCPLength+1]);
  Add(FCPLength,@FMask[FMaskInd]);
  inc(FMaskInd,FCPLength+1);
  FCPLength:=UTF8CodepointSizeFast(@FMask[FMaskInd]);
end;

procedure TMaskUTF8.CompileRange;
var
  lCharsGroupInsertSize: integer;
begin
  FLastOC:=TMaskOpCode.CharsGroupBegin;
  Add(TMaskOpCode.CharsGroupBegin);
  inc(cMatchMinimumLiteralBytes,1);
  if cMatchMaximumLiteralBytes<High(cMatchMaximumLiteralBytes) then
    inc(cMatchMaximumLiteralBytes,4);
  lCharsGroupInsertSize:=cMaskCompiledIndex;
  Add(0);
  inc(FMaskInd); // CP length is 1 because it is "["
  if FMaskInd<cMaskLimit then begin
    if (FMask[FMaskInd]='!') and (eMaskOpcodeNegateGroup in cMaskOpcodesAllowed) then begin
      Add(TMaskOpCode.Negate);
      inc(FMaskInd); // CP length is 1 because it is "!"
      FLastOC:=TMaskOpCode.Negate;
    end;
  end;

  while FMaskInd<=cMaskLimit do begin
    FCPLength:=UTF8CodepointSizeFast(@FMask[FMaskInd]);
    if (FMask[FMaskInd]='?') and (eMaskOpcodeAnyCharOrNone in cMaskOpcodesAllowed) then begin
      // This syntax is permitted [??] but not this one [?a] or [a?]
      if (FLastOC=TMaskOpCode.CharsGroupBegin) or (FLastOC=TMaskOpCode.AnyCharOrNone) then begin
        if FLastOC=TMaskOpCode.AnyCharOrNone then begin
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
          if cMatchMaximumLiteralBytes<High(cMatchMaximumLiteralBytes) then
            dec(cMatchMaximumLiteralBytes,4);
        end;
        if cMatchMaximumLiteralBytes<High(cMatchMaximumLiteralBytes) then
          inc(cMatchMaximumLiteralBytes,4);
        FLastOC:=TMaskOpCode.AnyCharOrNone;
      end
      else
        Exception_InvalidCharMask(FMask[FMaskInd],FMaskInd);
    end
    else if (FLastOC=TMaskOpCode.AnyCharOrNone) and (FMask[FMaskInd]<>']') then begin
      //FMask[FMaskInd] is not '?', but previous mask was '?' and it is an invalid sequence.
      // "[??] = Valid" // "[a?] or [?a] = Invalid"
      Exception_InvalidCharMask(FMask[FMaskInd],FMaskInd);
    end
    else if ((FMaskInd+FCPLength+1)<=cMaskLimit) and (FMask[FMaskInd+FCPLength]='-')
    and (eMaskOpcodeRange in cMaskOpcodesAllowed) then begin
      // FMaskInd+FCPLength+1 --explained--
      //------------------------------
      // FMaskInd+FCPLength is next UTF8 after current UTF8 CP
      // +1 is at least one byte in UTF8 sequence after "-"
      // Check if it is a range
      Add(TMaskOpCode.Range);
      // Check if reverse range is needed
      if (not cRangeAutoReverse)
      or (CompareUTF8Sequences(@FMask[FMaskInd],@FMask[FMaskInd+FCPLength+1])<0) then
        AddRange
      else
        ReverseRange;
      FLastOC:=TMaskOpCode.Range;

    end else if FMask[FMaskInd]=']' then begin
      if FLastOC=TMaskOpCode.CharsGroupBegin then
        Exception_InvalidCharMask(FMask[FMaskInd],FMaskInd); //Error empty match
      // Insert the new offset in case of a positive match in CharsGroup
      PInteger(@cMaskCompiled[lCharsGroupInsertSize])^:=cMaskCompiledIndex;
      Add(TMaskOpCode.CharsGroupEnd);
      FLastOC:=TMaskOpCode.CharsGroupEnd;
      break;
    end else begin
      Add(TMaskOpCode.OptionalChar);
      Add(FCPLength,@FMask[FMaskInd]);
      FLastOC:=TMaskOpCode.OptionalChar;
    end;
    inc(FMaskInd,FCPLength);
  end;
  if FMaskInd>cMaskLimit then
    Exception_MissingCloseChar(']',cMaskLimit);
end;

procedure TMaskUTF8.Compile;
begin
  inherited Compile;
  if cCaseSensitive then
    FMask:=cOriginalMask
  else
    FMask:=UTF8LowerCase(cOriginalMask);
  cMaskLimit:=Length(FMask);
  FLastOC:=TMaskOpCode.Literal;
  SetLength(cMaskCompiled,0);
  FMaskInd:=1;
  while FMaskInd<=cMaskLimit do begin
    FCPLength:=UTF8CodepointSizeFast(@FMask[FMaskInd]);
    if (eMaskOpcodeEscapeChar in cMaskOpcodesAllowed)
    and (FMask[FMaskInd]=cMaskEscapeChar) then begin
      // next is Literal
      inc(FMaskInd,FCPLength);
      if FMaskInd<=cMaskLimit then begin
        FCPLength:=UTF8CodepointSizeFast(@FMask[FMaskInd]);
        Add(TMaskOpCode.Literal);
        Add(FCPLength,@FMask[FMaskInd]);
        inc(cMatchMinimumLiteralBytes,FCPLength);
        if cMatchMaximumLiteralBytes<High(cMatchMaximumLiteralBytes) then
          inc(cMatchMaximumLiteralBytes,FCPLength);
        FLastOC:=TMaskOpCode.Literal;
        inc(FMaskInd,FCPLength);
      end
      else
        Exception_IncompleteMask();
    end else begin
      if FMask[FMaskInd] in ['*','?','['] then begin
        case FMask[FMaskInd] of
          '*':
            begin
              if eMaskOpcodeAnyText in cMaskOpcodesAllowed then begin
                if FLastOC<>TMaskOpCode.AnyCharToNext then begin
                  Add(TMaskOpCode.AnyCharToNext);
                  FLastOC:=TMaskOpCode.AnyCharToNext;
                  // * = No limit
                  cMatchMaximumLiteralBytes:=High(cMatchMaximumLiteralBytes);
                end;
              end
              else
                AddLiteral;
            end;
          '?':
            begin
              if eMaskOpcodeAnyChar in cMaskOpcodesAllowed then
                AddAnyChar
              else
                AddLiteral;
            end;
          '[':
            begin
              if [eMaskOpcodeOptionalChar,eMaskOpcodeRange,
                  eMaskOpcodeAnyCharOrNone] * cMaskOpcodesAllowed <> []
              then
                CompileRange
              else
                AddLiteral;
            end;
        end;
      end
      else
        AddLiteral;
      inc(FMaskInd,FCPLength);
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
          if aMatchOffset>cMatchStringLimit then
            exit(TMaskFailCause.MatchStringExhausted); // Error, no char to match.
          inc(aMaskIndex);
          if CompareUTF8Sequences(@cMaskCompiled[aMaskIndex],@cMatchString[aMatchOffset])<>0 then
            exit(TMaskFailCause.MaskNotMatch);
          inc(aMaskIndex,UTF8CodepointSizeFast(@cMaskCompiled[aMaskIndex]));
          inc(aMatchOffset,UTF8CodepointSizeFast(@cMatchString[aMatchOffset]));
        end;
      TMaskOpCode.AnyChar:
        begin
          inc(aMaskIndex);
          if aMatchOffset>cMatchStringLimit then
            exit(TMaskFailCause.MatchStringExhausted); // Error, no char to match.
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
          end
          else
            exit(TMaskFailCause.MaskNotMatch);
        end;
      TMaskOpCode.OptionalChar:
        begin
          inc(aMaskIndex);
          if aMatchOffset>cMatchStringLimit then
            exit(TMaskFailCause.MatchStringExhausted); // Error, no char to match.
          if CompareUTF8Sequences(@cMaskCompiled[aMaskIndex],@cMatchString[aMatchOffset])=0 then begin
            if lNegateCharGroup then
              exit(TMaskFailCause.MaskNotMatch);
            aMaskIndex:=lSkipOnSuccessGroup+1;
            inc(aMatchOffset,UTF8CodepointSizeFast(@cMatchString[aMatchOffset]));
          end
          else
            inc(aMaskIndex,UTF8CodepointSizeFast(@cMaskCompiled[aMaskIndex]));
        end;
      TMaskOpCode.Range:
        begin
          if aMatchOffset>cMatchStringLimit then
            exit(TMaskFailCause.MatchStringExhausted); // Error, no char to match.
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
            end
            else
              exit(TMaskFailCause.MaskNotMatch);
          end
        end;
      TMaskOpCode.AnyCharToNext:
        begin
          // if last is "*", everything in remain data matches
          if aMaskIndex=cMaskCompiledLimit then
            exit(TMaskFailCause.Success);
          if aMatchOffset>cMatchStringLimit then begin
            if aMaskIndex=cMaskCompiledLimit then
              exit(TMaskFailCause.Success);
            exit(TMaskFailCause.MatchStringExhausted);
          end;
          inc(aMaskIndex);
          while aMatchOffset<=cMatchStringLimit do begin
            lFailCause:=intfMatches(aMatchOffset,aMaskIndex);
            if lFailCause=TMaskFailCause.Success then
              exit(TMaskFailCause.Success)
            else if lFailCause=TMaskFailCause.MatchStringExhausted then
              exit(TMaskFailCause.MatchStringExhausted);
            inc(aMatchOffset,UTF8CodepointSizeFast(@cMatchString[aMatchOffset]));
          end;
          exit(TMaskFailCause.MatchStringExhausted);
        end;
      TMaskOpCode.AnyCharOrNone:
        begin
          inc(aMaskIndex);
          lTryCounter:=PInteger(@cMaskCompiled[aMaskIndex])^;
          inc(aMaskIndex,sizeof(integer));
          if TMaskOpCode(cMaskCompiled[aMaskIndex])<>TMaskOpCode.CharsGroupEnd then
            Exception_InternalError()
          else
            aMaskIndex:=lSkipOnSuccessGroup+1;

          // Try to match remain mask eating, 0,1,2,...,lTryCounter chars.
          for j := 0 to lTryCounter do begin
            if aMatchOffset>cMatchStringLimit then begin
              if aMaskIndex=cMaskCompiledLimit+1 then
                exit(TMaskFailCause.Success);
              exit(TMaskFailCause.MatchStringExhausted);
            end;
            lFailCause:=intfMatches(aMatchOffset,aMaskIndex);
            if lFailCause=TMaskFailCause.Success then
              exit(TMaskFailCause.Success)
            else if lFailCause=TMaskFailCause.MatchStringExhausted then
              exit(TMaskFailCause.MatchStringExhausted);
            inc(aMatchOffset,UTF8CodepointSizeFast(@cMatchString[aMatchOffset]));
          end;
          exit(TMaskFailCause.MatchStringExhausted);
        end;
      else  // case
        begin
          Exception_InternalError();
        end;
    end;
  end;
  if (aMaskIndex>cMaskCompiledLimit) and (aMatchOffset>cMatchStringLimit) then
    Result:=TMaskFailCause.Success
  else if aMaskIndex>cMaskCompiledLimit then
    Result:=TMaskFailCause.MaskExhausted
  else
    Result:=TMaskFailCause.MatchStringExhausted;
end;

constructor TMaskUTF8.Create(const aMask: String;
  aCaseSensitive: Boolean; aOpcodesAllowed: TMaskOpcodesSet);
begin
  inherited Create(aCaseSensitive,aOpcodesAllowed);
  cOriginalMask:=aMask;
end;

constructor TMaskUTF8.CreateLegacy(const aMask: String; aCaseSensitive: Boolean);
// Use [?] syntax for literal '?', no escaping chars with '\'.
begin
  Create(aMask, aCaseSensitive, MaskOpCodesNoEscape);
end;

constructor TMaskUTF8.Create(const aMask: String; aOptions: TMaskOptions);
begin
  inherited Create(aOptions);
  cOriginalMask:=aMask;
end;

function TMaskUTF8.Matches(const aStringToMatch: String): Boolean;
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

function TMaskUTF8.MatchesWindowsMask(const AFileName: String): Boolean;
var
  WinMask: TMaskUTF8Windows;
begin
  WinMask:=TMaskUTF8Windows.Create(cOriginalMask, CaseSensitive, OPCodesAllowed);
  try
    Result:=WinMask.Matches(AFileName);
  finally
    WinMask.Free;
  end;
end;

{ TMaskWindows }

class procedure TMaskUTF8Windows.SplitFileNameExtension(
  const aSourceFileName: String; out aFileName: String;
  out aExtension: String; aIsMask: Boolean);
var
  j: Integer;
  lLowLimit: integer;
begin
  // Default values
  aFileName:=aSourceFileName;
  aExtension:='';

  // This is because .foo is considered a file name ".foo" as one.
  if aIsMask then
    lLowLimit:=0
  else
    lLowLimit:=1;

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

constructor TMaskUTF8Windows.Create(const aMask: String; aCaseSensitive: Boolean;
  aOpcodesAllowed: TMaskOpcodesSet; aWindowsQuirksAllowed: TWindowsQuirkSet);
begin
  cMaskWindowsQuirkAllowed:=aWindowsQuirksAllowed;
  cWindowsMask:=aMask;
  inherited Create(aMask,aCaseSensitive,aOpcodesAllowed);
  Compile;
end;

procedure TMaskUTF8Windows.Compile;

  function OptionalQMarksAtEnd(aMask: String): String;
  var
    lCounter: integer;
    k: integer;
  begin
    lCounter:=0;
    for k := Length(aMask) downto 1 do begin
      if aMask[k]='?' then
        inc(lCounter)
      else
        break;
    end;
    if lCounter>0 then
      aMask:=copy(aMask,1,Length(aMask)-lCounter)+'['+StringOfChar('?',lCounter)+']';
    Result:=aMask;
  end;

  function EscapeSpecialChars(const aString: String): String;
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
  lFileNameMask: String;
  lExtensionMask: String;
  lModifiedMask: String;

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

function TMaskUTF8Windows.Matches(const aFileName: String): Boolean;
var
  lFileName, lExtension: String;
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

constructor TMaskList.Create(const aValue: String; aSeparator: Char;
  CaseSensitive: Boolean; aOpcodesAllowed: TMaskOpcodesSet);
var
  S: TParseStringList;
  I: Integer;
begin
  FMasks := TObjectList.Create(True);
  FWindowsMasks := TObjectList.Create(True);
  S := TParseStringList.Create(aValue, aSeparator);
  try
    for I := 0 to S.Count-1 do begin
      FMasks.Add(TMask.Create(S[I], CaseSensitive, aOpcodesAllowed));
      // A hack, add also to FWindowsMasks.
      FWindowsMasks.Add(TMaskWindows.Create(S[I], CaseSensitive));
    end;
  finally
    S.Free;
  end;
end;

constructor TMaskList.Create(const aValue: String; aSeparator: Char; aOptions: TMaskOptions);
var
  CaseSens: Boolean;
  Opcodes: TMaskOpcodesSet;
begin
  CaseSens:=moCaseSensitive in aOptions;
  if moDisableSets in aOptions then
    Opcodes:=MaskOpCodesDisableRange
  else
    Opcodes:=MaskOpCodesDefaultAllowed;
  Create(aValue, aSeparator, CaseSens, Opcodes);
end;

constructor TMaskList.CreateWindows(const aValue: String; aSeparator: Char;
  CaseSensitive: Boolean; aOpcodesAllowed: TMaskOpcodesSet);
var
  S: TParseStringList;
  I: Integer;
begin
  FMasks := TObjectList.Create(True);
  S := TParseStringList.Create(AValue, ASeparator);
  try
    for I := 0 to S.Count-1 do
      FMasks.Add(TMaskWindows.Create(S[I], CaseSensitive, aOpcodesAllowed));
  finally
    S.Free;
  end;
end;

constructor TMaskList.CreateWindows(const aValue: String; aSeparator: Char;
  aOptions: TMaskOptions);
begin
  CreateWindows(aValue, aSeparator, moCaseSensitive in aOptions, EncodeDisableRange(aOptions));
end;

constructor TMaskList.CreateSysNative(const aValue: String; aSeparator: Char;
  CaseSensitive: Boolean);
begin
  {$IFDEF Windows}
  CreateWindows(AValue, ASeparator, CaseSensitive);
  {$ELSE}
  Create(AValue, ASeparator, CaseSensitive);
  {$ENDIF}
end;

destructor TMaskList.Destroy;
begin
  FWindowsMasks.Free;
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
  for I := 0 to FMasks.Count-1 do
    if TMask(FMasks.Items[I]).Matches(AFileName) then
      Exit(True);
end;

function TMaskList.MatchesWindowsMask(const AFileName: String): Boolean;
// Uses a hack FWindowsMasks which can be removed when this method is removed.
var
  I: Integer;
begin
  Result := False;
  for I := 0 to FWindowsMasks.Count-1 do
    if TMaskWindows(FWindowsMasks.Items[I]).Matches(AFileName) then
      Exit(True);
  //raise Exception.Create('Create with TMaskList.CreateWindows, then call Matches.');
end;

end.

