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
 Changes and improvements by Juha Manninen and Bart Broersma
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
      TMaskExceptionCode=(mecInternalError,
                          mecInvalidCharMask,
                          mecMissingClose,
                          mecIncompleteMask,
                          mecInvalidEscapeChar,
                          mecInvalidUTF8Sequence
                          );
  protected
    FCode: TMaskExceptionCode;
  public
    constructor Create(const msg: string; aCode: TMaskExceptionCode);
    constructor CreateFmt(const msg: string; args: array of const; aCode: TMaskExceptionCode);
    property Code: TMaskExceptionCode read FCode;
  end;


  TMaskOpCode=(mocAnyChar,          //treat ? as a wildcard to match exactly one char
               mocAnyCharOrNone,    //treat [?] to match any char or the absence of a char
               mocAnyText,          //treat * as a wildcard to mach zero or any mumber of chars
               mocRange,            //treat [a-c] to match either 'a', 'b' or 'c'. '-' is always treated as a range indicator.
                                    //to have a literal '-' in a range, you must escape it with EscapeChar (defaults to '\'): [+-\-] matches '+', ',', or '-'.
               mocSet,              //treat [a-c] to match either 'a', '-' or 'c'
               mocNegateGroup,      //treat [!a-c] to not match 'a', 'b', or 'c', but match any other char. Requires mocRange and/or mocSet
               mocEscapeChar        //treat EscapeChar (defaults to '\') to take the next char as a literal, so '\*' is treated as a literal '*'.
               );
  TMaskOpCodes=set of TMaskOpCode;

  (*
    Windows mask works in a different mode than regular mask, it has too many
    quirks and corner cases inherited from CP/M, then adapted to DOS (8.3) file
    names and adapted again for long file names.

    Anyth?ng.abc    = "?" matches exactly 1 char
    Anyth*ng.abc    = "*" matches 0 or more of chars

    ------- Quirks -------
   *)
  TWindowsQuirk=(wqAnyExtension,      // Anything*.*     : ".*" is removed.
                 wqFilenameEnd,       // Anything??.abc  : "?" matches 1 or 0 chars (except '.')
                                      // (Not the same as "Anything*.abc", but the same
                                      // as regex "Anything.{0,2}\.abc")
                                      // Internally converted to "Anything[??].abc"

                 wqExtension3More,    // Anything.abc    : Matches "Anything.abc" but also "Anything.abc*" (3 char extension)
                                      // Anything.ab     : Matches "Anything.ab" and never "anything.abcd"
                                      // *.pas           : Matches "Unit1.pas.bak". Not good.
                 wqEmptyIsAny,        // ""              : Empty string matches anything "*"
                 wqAllByExtension,    // .abc            : Runs as "*.abc" (Not in use anymore)
                 wqNoExtension);      // Anything*.      : Matches "Anything*" without extension
  TWindowsQuirks=set of TWindowsQuirk;

const
  AllWindowsQuirks=[wqAnyExtension,
                    wqFilenameEnd,
                    wqExtension3More,
                    wqEmptyIsAny,
                    wqAllByExtension,
                    wqNoExtension];

  // Leave out wqExtension3More and wqAllByExtension
  DefaultWindowsQuirks=[wqAnyExtension,
                        wqFilenameEnd,
                        wqEmptyIsAny,
                        wqNoExtension];

  AllMaskOpCodes=[mocAnyChar,
                  mocAnyCharOrNone,
                  mocAnyText,
                  mocRange,
                  mocSet,
                  mocNegateGroup,
                  mocEscapeChar];


  // Match [x] literally, not as a range.
  // Leave out mocAnyCharOrNone, mocRange and mocSet.
  MaskOpCodesDisableRange=[mocAnyChar,
                           mocAnyText,
                           mocNegateGroup,
                           mocEscapeChar];

  // Interpret [?] as literal question mark instead of 0..1 chars wildcard.
  // Disable backslash escaping characters like "\?".
  // Leave out mocAnyCharOrNone and mocEscapeChar
  MaskOpCodesNoEscape=[mocAnyChar,
                       mocAnyText,
                       mocRange,
                       mocSet,
                       mocNegateGroup];

  DefaultMaskOpCodes=AllMaskOpCodes-[mocEscapeChar];//MaskOpCodesNoEscape;

type

  // Backwards compatible options.
  TMaskOption = (moCaseSensitive, moDisableSets);
  TMaskOptions = set of TMaskOption;

  { TMaskBase }

  TMaskBase = class
  private
    procedure SetAutoReverseRange(AValue: Boolean);
    procedure SetCaseSensitive(AValue: Boolean);
    procedure SetMaskEscapeChar(AValue: Char);
    procedure SetMaskOpCodesAllowed(AValue: TMaskOpCodes);
  protected
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
    type
    TMaskParsedCode = (
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
    TMaskFailCause = (
      mfcSuccess,
      mfcMatchStringExhausted,
      mfcMaskExhausted,
      mfcMaskNotMatch,
      mfcUnexpectedEnd
    );
    const GROW_BY=100;
    procedure Add(aLength: integer; aData: PBYTE);
    procedure Add(aValue: integer);
    procedure Add(aValue: TMaskParsedCode);
    procedure IncrementLastCounterBy(aOpcode: TMaskParsedCode; aValue: integer);
  protected
    fCaseSensitive: Boolean;
    fAutoReverseRange: Boolean; // If enabled, range [z-a] is interpreted as [a-z]
    fMaskIsCompiled: Boolean;
    fMaskCompiled: TBytes;
    fMaskCompiledIndex: integer;
    fMaskCompiledAllocated: integer;
    fMaskCompiledLimit: integer;
    fMaskLimit: integer;
    fMatchStringLimit: integer;
    fMatchMinimumLiteralBytes: SizeInt;
    fMatchMaximumLiteralBytes: SizeInt;
    fMaskOpcodesAllowed: TMaskOpCodes;
    // EscapeChar forces next char to be a literal one, not a wildcard.
    fMaskEscapeChar: Char;
    procedure Compile; virtual;
    class procedure Exception_InvalidCharMask(const aMaskChar: string; aOffset: integer=-1); static;
    class procedure Exception_MissingCloseChar(const aMaskChar: string; aOffset: integer=-1); static;
    class procedure Exception_IncompleteMask(); static;
    class procedure Exception_InvalidEscapeChar(); static;
    procedure Exception_InternalError();
    //function intfMatches(aMatchOffset: integer; aMaskIndex: integer): TMaskFailCause; virtual; abstract;
  public
    constructor Create(aCaseSensitive: Boolean=False;
      aOpcodesAllowed: TMaskOpCodes=DefaultMaskOpCodes);
    constructor Create(aOptions: TMaskOptions);
  public
    property CaseSensitive: Boolean read fCaseSensitive write SetCaseSensitive;
    property AutoReverseRange: Boolean read fAutoReverseRange write SetAutoReverseRange;
    property EscapeChar: Char read fMaskEscapeChar write SetMaskEscapeChar;  //Must be lower ASCII (#0-#127)
    property MaskOpCodes: TMaskOpCodes read fMaskOpcodesAllowed write SetMaskOpCodesAllowed;
  end;

  { TMaskUTF8 }

  TMaskUTF8 = class (TMaskBase)
  private
    fMatchString: String;
    // Used by Compile.
    fMaskInd: Integer;
    fCPLength: integer;    // Size of codepoint.
    fLastOC: TMaskParsedCode;  // Last OpCode.
    fMask: String;
    procedure AddAnyChar;
    procedure AddLiteral;
    procedure AddRange(lFirstRange, lSecondRange: Integer);
    procedure CompileRange;
    function GetMask: String; virtual;
    procedure ReverseRange(lFirstRange, lSecondRange: Integer);
    procedure SetMask(AValue: String); virtual;
  protected
    fOriginalMask: String;
    class function CompareUTF8Sequences(const P1,P2: PChar): integer; static;
    function intfMatches(aMatchOffset: integer; aMaskIndex: integer): TMaskFailCause; //override;
  public
    constructor Create(const aMask: String);
    constructor Create(const aMask: String; aCaseSensitive: Boolean);
    constructor Create(const aMask: String; aCaseSensitive: Boolean; aOpcodesAllowed: TMaskOpCodes); virtual; overload;
    constructor Create(const aMask: String; aOptions: TMaskOptions);
        deprecated 'Use Create with TMaskOpCodes params.'; // in Lazarus 2.3, remove in 2.5.

    procedure Compile; override;
    function Matches(const aStringToMatch: String): Boolean; virtual;
    function MatchesWindowsMask(const AFileName: String): Boolean;
        deprecated 'Create with TMaskWindows.Create, then call Matches.'; // in Lazarus 2.3, remove in 2.5.
  public
    property Mask: String read GetMask write SetMask;
  end;

  TMask = class(TMaskUTF8);

  { TWindowsMaskUTF8 }

  TWindowsMaskUTF8=class(TMask)
  private
    procedure SetMask(AValue: String); override;
    function GetMask: String; override;
    procedure SetWindowsQuirkAllowed(AValue: TWindowsQuirks);
  protected
    fWindowsQuirkAllowed: TWindowsQuirks;
    fWindowsQuirkInUse: TWindowsQuirks;
    fWindowsMask: String;
    class procedure SplitFileNameExtension(const aSourceFileName: String;
      out aFileName: String; out aExtension: String; aIsMask: Boolean=False); static;
  public
    constructor Create(const aMask: String; aCaseSensitive: Boolean; aOpcodesAllowed: TMaskOpCodes); override;
    constructor Create(const aMask: String; aCaseSensitive: Boolean; aOpcodesAllowed: TMaskOpCodes; aWindowsQuirksAllowed: TWindowsQuirks);

    procedure Compile; override;
    function Matches(const aFileName: String): Boolean; override;
  public
    property Quirks: TWindowsQuirks read fWindowsQuirkAllowed write SetWindowsQuirkAllowed;
  end;

  TWindowsMask = class(TWindowsMaskUTF8);

  TMaskClass = class of TMaskUtf8;

  { TParseStringList }

  TParseStringList = class(TStringList)
  public
    constructor Create(const AText, ASeparators: String);
  end;

  { TMaskList }

  TMaskList = class
  private
    fAutoReverseRange: Boolean;
    fMasks: TObjectList;
    FMaskClass: TMaskClass;
    fMask: String;
    fSeparator: Char;
    fCaseSensitive: Boolean;
    fMaskOpcodes: TMaskOpcodes;
    function GetCount: Integer;
    function GetItem(Index: Integer): TMask;
    procedure SetAutoReverseRange(AValue: Boolean);
    procedure SetCaseSensitive(AValue: Boolean);
    procedure SetMask(AValue: String); virtual;
    procedure SetMaskOpCodes(AValue: TMaskOpCodes);
  protected
    function GetMaskClass: TMaskClass; virtual;
    procedure AddMasksToList(const aValue: String; aSeparator: Char; CaseSensitive: Boolean;
      aOpcodesAllowed: TMaskOpCodes); virtual;
  public
    constructor Create(const aValue: String; aSeparator: Char=';'; CaseSensitive: Boolean=False;
      aOpcodesAllowed: TMaskOpCodes=DefaultMaskOpCodes);

    //Remove in 2.5
    constructor Create(const aValue: String; aSeparator: Char; aOptions: TMaskOptions); virtual;
      deprecated 'Use Create with TMaskOpcodes paramater';

    destructor Destroy; override;

    function Matches(const AFileName: String): Boolean;

    // Deprecated in Lazarus 2.3, October 2021. Remove in 2.5.
    function MatchesWindowsMask(const AFileName: String): Boolean;
      deprecated 'Use a TWindowsMaskList instead.';

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TMask read GetItem;
    property Mask: String read fMask write SetMask;
    property MaskOpCodes: TMaskOpCodes read fMaskOpCodes write SetMaskOpCodes;
    property AutoReverseRange: Boolean read fAutoReverseRange write SetAutoReverseRange;
    property CaseSensitive: Boolean read fCaseSensitive write SetCaseSensitive;
  end;


  { TWindowsMaskList }

  TWindowsMaskList = class(TMaskList)
  private
    fWindowsQuirks: TWindowsQuirks;
    procedure SetQuirks(AValue: TWindowsQuirks);
  protected
    function GetMaskClass: TMaskClass; override;
    procedure AddMasksToList(const aValue: String; aSeparator: Char; aCaseSensitive: Boolean;
      aOpcodesAllowed: TMaskOpCodes); override;
  public
    constructor Create(const aValue: String; aSeparator: Char=';'; aCaseSensitive: Boolean=False;
      aOpcodesAllowed: TMaskOpCodes=DefaultMaskOpCodes;
      aWindowsQuirksAllowed: TWindowsQuirks=DefaultWindowsQuirks); reintroduce;

    //Remove in 2.5
    constructor Create(const aValue: String; aSeparator: Char; aOptions: TMaskOptions); override;
      deprecated 'Use Create with TMaskOpcodes paramater';

    //Remove in 2.5
    function MatchesWindowsMask(const AFileName: String): Boolean; reintroduce;
      deprecated 'Use a Matches instead.';


    property Quirks: TWindowsQuirks read fWindowsQuirks write SetQuirks;
  end;

function MatchesMask(const FileName, Mask: String; CaseSensitive: Boolean=False;
  aOpcodesAllowed: TMaskOpCodes=DefaultMaskOpCodes): Boolean;
function MatchesMask(const FileName, Mask: String; Options: TMaskOptions): Boolean;
    deprecated 'Use MatchesMask with TMaskOpCodes params.'; // in Lazarus 2.3, remove in 2.5.

function MatchesWindowsMask(const FileName, Mask: String; CaseSensitive: Boolean=False;
  aOpcodesAllowed: TMaskOpCodes=DefaultMaskOpCodes;
  aWindowsQuirksAllowed: TWindowsQuirks=DefaultWindowsQuirks): Boolean;
function MatchesWindowsMask(const FileName, Mask: String; Options: TMaskOptions): Boolean;

function MatchesMaskList(const FileName, Mask: String; Separator: Char=';';
  CaseSensitive: Boolean=False;
  aOpcodesAllowed: TMaskOpCodes=DefaultMaskOpCodes): Boolean;
function MatchesMaskList(const FileName, Mask: String; Separator: Char;
  Options: TMaskOptions): Boolean;
    deprecated 'Use MatchesMaskList with TMaskOpCodes params.'; // in Lazarus 2.3, remove in 2.5.

function MatchesWindowsMaskList(const FileName, Mask: String; Separator: Char=';';
  CaseSensitive: Boolean=False;
  aOpcodesAllowed: TMaskOpCodes=DefaultMaskOpCodes;
  aWindowsQuirksAllowed: TWindowsQuirks=DefaultWindowsQuirks): Boolean;
function MatchesWindowsMaskList(const FileName, Mask: String; Separator: Char;
  Options: TMaskOptions): Boolean;
    deprecated 'Use MatchesWindowsMaskList with TMaskOpCodes params.'; // in Lazarus 2.3, remove in 2.5.


function DbgS(O: TMaskOpCodes): String ; overload;
function DbgS(Q: TWindowsQuirks): String ; overload;

implementation

function DbgS(O: TMaskOpCodes): String ;
var
  S: String;
  Op: TMaskOpcode;
begin
  Result := '[';
  for Op in O do
  begin
    WriteStr(S, Op);
    Result := Result + S + ',';
  end;
  if (Result[Length(Result)] = ',') then
    System.Delete(Result, Length(Result), 1);
  Result := Result + ']';
end;

function DbgS(Q: TWindowsQuirks): String ;
var
  S: String;
  Quirk: TWindowsQuirk;
begin
  Result := '[';
  for Quirk in Q do
  begin
    WriteStr(S, Quirk);
    Result := Result + S + ',';
  end;
  if (Result[Length(Result)] = ',') then
    System.Delete(Result, Length(Result), 1);
  Result := Result + ']';
end;


function EncodeDisableRange(Options: TMaskOptions): TMaskOpCodes;
// Encode the Disable Range option from legacy TMaskOptions.
begin
  if moDisableSets in Options then
    Result:=MaskOpCodesDisableRange
  else  // Disable '\' escaping for legacy code.
    Result:=MaskOpCodesNoEscape; //DefaultMaskOpCodes;
end;

function MatchesMask(const FileName, Mask: String; CaseSensitive: Boolean;
  aOpcodesAllowed: TMaskOpCodes): Boolean;
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


function MatchesMask(const FileName, Mask: String; Options: TMaskOptions): Boolean;
begin
  Result := MatchesMask(FileName, Mask, moCaseSensitive in Options,
                        EncodeDisableRange(Options));
end;

function MatchesWindowsMask(const FileName, Mask: String; CaseSensitive: Boolean;
  aOpcodesAllowed: TMaskOpCodes; aWindowsQuirksAllowed: TWindowsQuirks): Boolean;
var
  AMask: TWindowsMask;
begin
  AMask := TWindowsMask.Create(Mask, CaseSensitive, aOpcodesAllowed, aWindowsQuirksAllowed);
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
  CaseSensitive: Boolean; aOpcodesAllowed: TMaskOpCodes): Boolean;
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
  CaseSensitive: Boolean; aOpcodesAllowed: TMaskOpCodes; aWindowsQuirksAllowed: TWindowsQuirks): Boolean;
var
  AMaskList: TWindowsMaskList;
begin
  AMaskList := TWindowsMaskList.Create(Mask, Separator, CaseSensitive, aOpcodesAllowed, aWindowsQuirksAllowed);
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

{ TWindowsMaskList }

procedure TWindowsMaskList.SetQuirks(AValue: TWindowsQuirks);
var
  i: Integer;
begin
  if fWindowsQuirks = AValue then Exit;
  fWindowsQuirks := AValue;
  if (wqFilenameEnd in fWindowsQuirks) then
    Include(fMaskOpcodes, mocAnyCharOrNone);
  for i := 0 to fMasks.Count - 1 do
  begin
    TWindowsMask(fMasks.Items[i]).Quirks := FWindowsQuirks;
    TWindowsMask(fMasks.Items[i]).MaskOpCodes := fMaskOpcodes;
    //TWindowsMask(fMasks.Items[i]).Compile; //to apply Quirks
  end;
  //fMasks.Clear;
  //AddMasksToList(fMask, fSeparator, fCaseSensitive, fMaskOpCodes);
end;

function TWindowsMaskList.GetMaskClass: TMaskClass;
begin
  Result := TWindowsMask;
end;

procedure TWindowsMaskList.AddMasksToList(const aValue: String;
  aSeparator: Char; aCaseSensitive: Boolean; aOpcodesAllowed: TMaskOpCodes);
var
  i: Integer;
begin
  inherited AddMasksToList(aValue, aSeparator, aCaseSensitive, aOpcodesAllowed);
  if (FWindowsQuirks <> DefaultWindowsQuirks) then  //inherited did not pass Quirks to the constructor
    begin
    for i := 0 to fMasks.Count - 1 do
    begin
      TWindowsMask(fMasks.Items[i]).fWindowsQuirkAllowed := FWindowsQuirks;
      //TWindowsMask(fMasks.Items[i]).Compile; //to apply Quirks
    end;
  end;
end;

constructor TWindowsMaskList.Create(const aValue: String; aSeparator: Char;
  aCaseSensitive: Boolean; aOpcodesAllowed: TMaskOpCodes;
  aWindowsQuirksAllowed: TWindowsQuirks);
begin
  fWindowsQuirks := aWindowsQuirksAllowed;
  if (wqFilenameEnd in aWindowsQuirksAllowed) then
    Include(aOpcodesAllowed, mocAnyCharOrNone);
  inherited Create(aValue, aSeparator, aCaseSensitive, aOpcodesAllowed);
end;

constructor TWindowsMaskList.Create(const aValue: String; aSeparator: Char;
  aOptions: TMaskOptions);
begin
  Create(aValue, aSeparator, (moCaseSensitive in aOptions), DefaultMaskOpCodes, DefaultWindowsQuirks);
end;

function TWindowsMaskList.MatchesWindowsMask(const AFileName: String): Boolean;
begin
  Result := Matches(AFilename);
end;

{ EMaskError }

constructor EMaskError.Create(const msg: string; aCode: TMaskExceptionCode);
begin
  CreateFmt(msg,[],aCode);
end;

constructor EMaskError.CreateFmt(const msg: string; args: array of const;
  aCode: TMaskExceptionCode);
begin
  FCode:=aCode;
  Inherited CreateFmt(msg,args);
end;

{ TMaskBase }

procedure TMaskBase.SetAutoReverseRange(AValue: Boolean);
begin
  if fAutoReverseRange = AValue then Exit;
  fAutoReverseRange := AValue;
  fMaskIsCompiled := False;
end;

procedure TMaskBase.SetCaseSensitive(AValue: Boolean);
begin
  if fCaseSensitive = AValue then Exit;
  fCaseSensitive := AValue;
  fMaskIsCompiled := False;
end;

procedure TMaskBase.SetMaskEscapeChar(AValue: Char);
begin
  if fMaskEscapeChar=AValue then Exit;
  if fMaskEscapeChar>#127 then
    Exception_InvalidEscapeChar();
  fMaskEscapeChar:=AValue;
  fMaskIsCompiled:=False;
end;

procedure TMaskBase.SetMaskOpCodesAllowed(AValue: TMaskOpCodes);
begin
  if fMaskOpcodesAllowed = AValue then Exit;
  fMaskOpcodesAllowed := AValue;
  fMaskIsCompiled := False;
end;

procedure TMaskBase.Add(aLength: integer; aData: PBYTE);
var
  lCounter: integer;
begin
  if fMaskCompiledIndex+aLength>=fMaskCompiledAllocated then begin
    fMaskCompiledAllocated:=fMaskCompiledAllocated+aLength+GROW_BY;
    SetLength(fMaskCompiled,fMaskCompiledAllocated);
  end;
  for lCounter := 0 to Pred(aLength) do begin
    fMaskCompiled[fMaskCompiledIndex]:=(aData+lCounter)^;
    inc(fMaskCompiledIndex);
  end;
end;

procedure TMaskBase.Add(aValue: integer);
begin
  Add(sizeof(aValue),@aValue);
end;

procedure TMaskBase.Add(aValue: TMaskParsedCode);
var
  v: BYTE;
begin
  v:=BYTE(aValue);
  Add(1,@v);
end;

procedure TMaskBase.IncrementLastCounterBy(aOpcode: TMaskParsedCode; aValue: integer);
var
  p: PInteger;
begin
  fMaskCompiledIndex:=fMaskCompiledIndex-sizeof(aValue);
  if TMaskParsedCode(fMaskCompiled[fMaskCompiledIndex-1])<>aOpcode then
    Exception_InternalError();
  P:=@fMaskCompiled[fMaskCompiledIndex];
  Add(P^+aValue);
end;

procedure TMaskBase.Compile;
begin
  fMaskCompiled := nil;
  fMaskCompiledAllocated := 0;
  fMaskCompiledIndex := 0;
  fMaskIsCompiled:=true;
end;

class procedure TMaskBase.Exception_InvalidCharMask(const aMaskChar: string;
  aOffset: integer);
begin
  if aOffset>=0 then
    raise EMaskError.CreateFmt(rsInvalidCharMaskAt, [aMaskChar, aOffset], mecInvalidCharMask)
  else
    raise EMaskError.CreateFmt(rsInvalidCharMask, [aMaskChar], mecInvalidCharMask);
end;

class procedure TMaskBase.Exception_MissingCloseChar(const aMaskChar: string;
  aOffset: integer);
begin
  if aOffset>=0 then
    raise EMaskError.CreateFmt(rsMissingCloseCharMaskAt, [aMaskChar, aOffset], mecMissingClose)
  else
    raise EMaskError.CreateFmt(rsMissingCloseCharMask, [aMaskChar], mecMissingClose);
end;

class procedure TMaskBase.Exception_IncompleteMask();
begin
  raise EMaskError.CreateFmt(rsIncompleteMask, [], mecIncompleteMask);
end;

class procedure TMaskBase.Exception_InvalidEscapeChar();
begin
  raise EMaskError.Create(rsInvalidEscapeChar, mecInvalidEscapeChar);
end;

procedure TMaskBase.Exception_InternalError();
begin
  raise EMaskError.CreateFmt(rsInternalError, [self.ClassName], mecInternalError);
end;

constructor TMaskBase.Create(aCaseSensitive: Boolean; aOpcodesAllowed: TMaskOpCodes);
begin
  fCaseSensitive:=aCaseSensitive;
  fMaskOpcodesAllowed:=aOpcodesAllowed;
  fAutoReverseRange:=True;
  fMaskEscapeChar:='\';
end;


constructor TMaskBase.Create(aOptions: TMaskOptions);
begin
  Create(moCaseSensitive in aOptions, EncodeDisableRange(aOptions));
end;

{ TMask }

procedure TMaskUTF8.AddAnyChar;
begin
  Add(TMaskParsedCode.AnyChar);
  inc(fMatchMinimumLiteralBytes,1);
  if fMatchMaximumLiteralBytes<High(fMatchMaximumLiteralBytes) then
    inc(fMatchMaximumLiteralBytes,4);
  fLastOC:=TMaskParsedCode.AnyChar;
end;

procedure TMaskUTF8.AddLiteral;
begin
  Add(TMaskParsedCode.Literal);
  Add(fCPLength,@fMask[fMaskInd]);
  inc(fMatchMinimumLiteralBytes,fCPLength);
  if fMatchMaximumLiteralBytes<High(fMatchMaximumLiteralBytes) then
    inc(fMatchMaximumLiteralBytes,fCPLength);
  fLastOC:=TMaskParsedCode.Literal;
end;

procedure TMaskUTF8.AddRange(lFirstRange, lSecondRange: Integer);
begin
  fCPLength:=UTF8CodepointSizeFast(@fMask[lFirstRange]);
  Add(fCPLength,@fMask[lFirstRange]);
  fCPLength:=UTF8CodepointSizeFast(@fMask[lSecondRange]);
  Add(fCPLength,@fMask[lSecondRange]);
  fMaskInd:=lSecondRange;
end;

procedure TMaskUTF8.ReverseRange(lFirstRange, lSecondRange: Integer);
begin
  fCPLength:=UTF8CodepointSizeFast(@fMask[lSecondRange]);
  Add(fCPLength,@fMask[lSecondRange]);
  Add(UTF8CodepointSizeFast(@fMask[lFirstRange]),@fMask[lFirstRange]);
  fMaskInd:=lSecondRange;
end;

procedure TMaskUTF8.SetMask(AValue: String);
begin
  if fOriginalMask = AValue then Exit;
  fOriginalMask := AValue;
  fMaskIsCompiled := False;
end;

procedure TMaskUTF8.CompileRange;
  function IsARange(aPosition: integer; out aFirstSequence: integer; out aSecondSequence: integer): Boolean;// {$IFDEF USE_INLINE}inline;{$ENDIF}
    var
      llCPL: integer;
    begin
      Result:=false;
      aFirstSequence:=0;
      aSecondSequence:=0;
      if (mocEscapeChar in FMaskOpcodesAllowed) and (fMask[aPosition]=FMaskEscapeChar) then begin
        llCPL:=UTF8CodepointSizeFast(@fMask[aPosition]);
        if aPosition+llCPL>fMaskLimit then exit; // ==>
        inc(aPosition,llCPL);
        aFirstSequence:=aPosition;
      end else begin
        aFirstSequence:=aPosition;
      end;
      llCPL:=UTF8CodepointSizeFast(@fMask[aPosition]);
      inc(aPosition,llCPL);
      if aPosition+llCPL>fMaskLimit then exit; // ==>
      // It is not a range, return false
      if fMask[aPosition]<>'-' then exit;

      llCPL:=UTF8CodepointSizeFast(@fMask[aPosition]);
      inc(aPosition,llCPL);
      if (mocEscapeChar in FMaskOpcodesAllowed) and (fMask[aPosition]=FMaskEscapeChar) then begin
        llCPL:=UTF8CodepointSizeFast(@fMask[aPosition]);
        if aPosition+llCPL>fMaskLimit then exit; // ==>
        inc(aPosition,llCPL);
        aSecondSequence:=aPosition;
      end else begin
        aSecondSequence:=aPosition;
      end;
      Result:=true;
    end;

var
  lCharsGroupInsertSize, lFirstRange, lSecondRange: integer;
begin
  //writeln('CompileRange');
  fLastOC:=TMaskParsedCode.CharsGroupBegin;
  Add(TMaskParsedCode.CharsGroupBegin);
  inc(fMatchMinimumLiteralBytes,1);
  if fMatchMaximumLiteralBytes<High(fMatchMaximumLiteralBytes) then
    inc(fMatchMaximumLiteralBytes,4);
  lCharsGroupInsertSize:=fMaskCompiledIndex;
  Add(0);
  inc(fMaskInd); // CP length is 1 because it is "["
  if fMaskInd<fMaskLimit then begin
    if (fMask[fMaskInd]='!') and (mocNegateGroup in fMaskOpcodesAllowed) then begin
      Add(TMaskParsedCode.Negate);
      inc(fMaskInd); // CP length is 1 because it is "!"
      fLastOC:=TMaskParsedCode.Negate;
    end;
  end;

  while fMaskInd<=fMaskLimit do begin
    fCPLength:=UTF8CodepointSizeFast(@fMask[fMaskInd]);
    if (fMask[fMaskInd]='?') and (mocAnyCharOrNone in fMaskOpcodesAllowed) then begin //mocAnyCharOrNone and '?' found
      // This syntax is permitted [??] but not this one [?a] or [a?]
      if (fLastOC=TMaskParsedCode.CharsGroupBegin) or (fLastOC=TMaskParsedCode.AnyCharOrNone) then begin
        if fLastOC=TMaskParsedCode.AnyCharOrNone then begin
          // Increment counter
          IncrementLastCounterBy(TMaskParsedCode.AnyCharOrNone,1);
        end else begin
          Add(TMaskParsedCode.AnyCharOrNone);
          Add(1); // Counter
          // Discount minimun bytes added at the "CharGroupBegin"
          // because [?] could be 1 or 0 chars, so minimum is zero
          // but the CharsGroupBegin assumes 1 char as all other
          // masks replace the group by 1 char position.
          // This code will run 1 time per group at maximun.
          dec(fMatchMinimumLiteralBytes,1);
          if fMatchMaximumLiteralBytes<High(fMatchMaximumLiteralBytes) then
            dec(fMatchMaximumLiteralBytes,4);
        end;
        if fMatchMaximumLiteralBytes<High(fMatchMaximumLiteralBytes) then
          inc(fMatchMaximumLiteralBytes,4);
        fLastOC:=TMaskParsedCode.AnyCharOrNone;
      end
      else
        begin
          Exception_InvalidCharMask(fMask[fMaskInd],fMaskInd);
        end;
    end //mocAnyCharOrNone and '?' found
    else if (fLastOC=TMaskParsedCode.AnyCharOrNone) and (fMask[fMaskInd]<>']') then begin //mocAnyCharOrNone and inside [?
      //writeln('Compile found not ? after [?: ',fMask[fMaskInd]);
      //fMask[fMaskInd] is not '?', but previous mask was '?' and it is an invalid sequence.
      // "[??] = Valid" // "[a?] or [?a] = Invalid"
      Exception_InvalidCharMask(fMask[fMaskInd],fMaskInd);
    end
    else if (mocRange in fMaskOpcodesAllowed) and IsARange(fMaskInd,lFirstRange,lSecondRange) then begin //is a range
      Add(TMaskParsedCode.Range);
      // Check if reverse range is needed
      if (not fAutoReverseRange)
      or (CompareUTF8Sequences(@fMask[lFirstRange],@fMask[lSecondRange])<0) then
        AddRange(lFirstRange, lSecondRange)
      else
        ReverseRange(lFirstRange, lSecondRange);
      fLastOC:=TMaskParsedCode.Range;

    end //is a range
    else if fMask[fMaskInd]=']' then begin  //found ]
      if (fLastOC=TMaskParsedCode.CharsGroupBegin) or (fLastOC=TMaskParsedCode.Negate) then
        begin //empty set or range
          //writeln('CompileRange: empty match');
          Exception_InvalidCharMask(fMask[fMaskInd],fMaskInd); //Error empty match
        end; //empty set or range
      // Insert the new offset in case of a positive match in CharsGroup
      PInteger(@fMaskCompiled[lCharsGroupInsertSize])^:=fMaskCompiledIndex;
      Add(TMaskParsedCode.CharsGroupEnd);
      fLastOC:=TMaskParsedCode.CharsGroupEnd;
      break;
    end // end of range or set
    else begin  //not a range, not AnyCharOrNone, must be a set
      // handle escaping if mocSet is enabled, but mocRange not
      if (fMask[fMaskInd]=FMaskEscapeChar) and (mocEscapeChar in fMaskOpcodesAllowed) then begin
        // next is optional char in set or literal
        inc(fMaskInd,fCPLength);
        if fMaskInd<=fMaskLimit then begin
          fCPLength:=UTF8CodepointSizeFast(@fMask[fMaskInd]);
        end else begin
          //writeln('CompileRange: incomplete mask');
          Exception_IncompleteMask();
        end;
      end;

      if (mocSet in fMaskOpcodesAllowed) then begin
        Add(TMaskParsedCode.OptionalChar);
        Add(fCPLength,@fMask[fMaskInd]);
        fLastOC:=TMaskParsedCode.OptionalChar;
      end else begin
         //writeln('CompileRange: exception but why??');
         Exception_InvalidCharMask(fMask[fMaskInd],fMaskInd);
      end;
    end;
    inc(fMaskInd,fCPLength);
  end;
  if fMaskInd>fMaskLimit then
    Exception_MissingCloseChar(']',fMaskLimit);
  //writeln('CompileRange end.');
end;

function TMaskUTF8.GetMask: String;
begin
  Result := fOriginalMask;
end;

procedure TMaskUTF8.Compile;
  procedure HandleEscapeCharPlusLiteral;
  begin
    inc(fMaskInd,fCPLength);
    if fMaskInd<=fMaskLimit then begin
      fCPLength:=UTF8CodepointSizeFast(@fMask[fMaskInd]);
      AddLiteral;
      inc(fMaskInd,fCPLength);
    end
    else
      Exception_IncompleteMask();
  end;

  procedure HandleSpecialChar;
  begin
    case fMask[fMaskInd] of
      '*':
        begin
          if mocAnyText in fMaskOpcodesAllowed then begin
            if fLastOC<>TMaskParsedCode.AnyCharToNext then begin
              Add(TMaskParsedCode.AnyCharToNext);
              fLastOC:=TMaskParsedCode.AnyCharToNext;
              // * = No limit
              fMatchMaximumLiteralBytes:=High(fMatchMaximumLiteralBytes);
            end;
          end
          else
            AddLiteral;
        end;
      '?':
        begin
          if mocAnyChar in fMaskOpcodesAllowed then
            AddAnyChar
          else
            AddLiteral;
        end;
      '[':
        begin
          if ([mocSet,mocRange,
              mocAnyCharOrNone] * fMaskOpcodesAllowed <> [])
              and not
              (
                ([mocSet,mocRange]*fMaskOpCodesAllowed = []) //only mocAnyCharOrNone enabled
                 and
                (fMaskInd<fMaskLimit) and (fMask[fMaskInd+1]<>'?')// next char is not '?', so basically then the '[' is a literal (or an escapechar)
              )
          then
            CompileRange
          else begin
            if (fMask[fMaskInd]=FMaskEscapeChar) and (mocEscapeChar in FMaskOpcodesAllowed) then begin  //DEAD CODE?
              // next is Literal
              inc(fMaskInd,fCPLength);
              if fMaskInd<=fMaskLimit then begin
                fCPLength:=UTF8CodepointSizeFast(@fMask[fMaskInd]);
              end else begin
                Exception_IncompleteMask();
              end;
            end; //DEAD CODE??
            AddLiteral;
          end;
        end;
    end;
  end;

begin
  inherited Compile;
  //if Compile fails and a new call to Matches is made and Mask is unchanged
  //then Matches simply returns False, when raising the exception again would be more appropriate IMO (BB)
  fMaskIsCompiled:=False;
  if fCaseSensitive then
    fMask:=fOriginalMask
  else
    fMask:=UTF8LowerCase(fOriginalMask);
  fMaskLimit:=Length(fMask);
  fLastOC:=TMaskParsedCode.Literal;
  SetLength(fMaskCompiled,0);
  fMaskInd:=1;

  while fMaskInd<=fMaskLimit do begin //while
    fCPLength:=UTF8CodepointSizeFast(@fMask[fMaskInd]);
    if (mocEscapeChar in fMaskOpcodesAllowed) and (fMask[fMaskInd]=fMaskEscapeChar) then
      HandleEscapeCharPlusLiteral
    else begin // not an escaped literal
      if fMask[fMaskInd] in ['*','?','['] then begin // handle special chars
        HandleSpecialChar;
      end  //handle special chars
      else
      begin
        AddLiteral;
      end;
      inc(fMaskInd,fCPLength);
    end; //not an escaped literal
  end;  //while

  SetLength(fMaskCompiled,fMaskCompiledIndex);
  fMaskCompiledLimit:=fMaskCompiledIndex-1;
  fMaskIsCompiled:=True;
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
  Result:=mfcUnexpectedEnd;
  lNegateCharGroup:=false;
  while aMaskIndex<=fMaskCompiledLimit do begin
    case TMaskParsedCode(fMaskCompiled[aMaskIndex]) of
      TMaskParsedCode.Literal:
        begin
          if aMatchOffset>fMatchStringLimit then
            exit(TMaskFailCause.mfcMatchStringExhausted); // Error, no char to match.
          inc(aMaskIndex);
          if CompareUTF8Sequences(@fMaskCompiled[aMaskIndex],@fMatchString[aMatchOffset])<>0 then
            exit(TMaskFailCause.mfcMaskNotMatch);
          inc(aMaskIndex,UTF8CodepointSizeFast(@fMaskCompiled[aMaskIndex]));
          inc(aMatchOffset,UTF8CodepointSizeFast(@fMatchString[aMatchOffset]));
        end;
      TMaskParsedCode.AnyChar:
        begin
          inc(aMaskIndex);
          if aMatchOffset>fMatchStringLimit then
            exit(TMaskFailCause.mfcMatchStringExhausted); // Error, no char to match.
          inc(aMatchOffset,UTF8CodepointSizeFast(@fMatchString[aMatchOffset]));
        end;
      TMaskParsedCode.Negate:
        begin
          lNegateCharGroup:=true;
          inc(aMaskIndex);
        end;
      TMaskParsedCode.CharsGroupBegin:
        begin
          lNegateCharGroup:=false;
          inc(aMaskIndex);
          lSkipOnSuccessGroup:=PInteger(@fMaskCompiled[aMaskIndex])^;
          inc(aMaskIndex,sizeof(integer));
        end;
      TMaskParsedCode.CharsGroupEnd:
        begin
          if lNegateCharGroup then begin
            aMaskIndex:=lSkipOnSuccessGroup+1;
            inc(aMatchOffset,UTF8CodepointSizeFast(@fMatchString[aMatchOffset]));
          end
          else
            exit(TMaskFailCause.mfcMaskNotMatch);
        end;
      TMaskParsedCode.OptionalChar:
        begin
          inc(aMaskIndex);
          if aMatchOffset>fMatchStringLimit then
            exit(TMaskFailCause.mfcMatchStringExhausted); // Error, no char to match.
          if CompareUTF8Sequences(@fMaskCompiled[aMaskIndex],@fMatchString[aMatchOffset])=0 then begin
            if lNegateCharGroup then
              exit(TMaskFailCause.mfcMaskNotMatch);
            aMaskIndex:=lSkipOnSuccessGroup+1;
            inc(aMatchOffset,UTF8CodepointSizeFast(@fMatchString[aMatchOffset]));
          end
          else
            inc(aMaskIndex,UTF8CodepointSizeFast(@fMaskCompiled[aMaskIndex]));
        end;
      TMaskParsedCode.Range:
        begin
          if aMatchOffset>fMatchStringLimit then
            exit(TMaskFailCause.mfcMatchStringExhausted); // Error, no char to match.
          inc(aMaskIndex);
          c1:=@fMaskCompiled[aMaskIndex];
          inc(aMaskIndex,UTF8CodepointSizeFast(C1));
          c2:=@fMaskCompiled[aMaskIndex];
          inc(aMaskIndex,UTF8CodepointSizeFast(C2));
          t1:=(CompareUTF8Sequences(@fMatchString[aMatchOffset],c1)>=0) and
              (CompareUTF8Sequences(@fMatchString[aMatchOffset],c2)<=0);
          if t1 then begin
            if not lNegateCharGroup then begin
              //Jump to CharsGroupEnd+1 because if CharsGroupEnd is reached
              //it means that all optional chars and ranges have not matched the string.
              aMaskIndex:=lSkipOnSuccessGroup+1;
              inc(aMatchOffset,UTF8CodepointSizeFast(@fMatchString[aMatchOffset]));
            end
            else
              exit(TMaskFailCause.mfcMaskNotMatch);
          end
        end;
      TMaskParsedCode.AnyCharToNext:
        begin
          // if last is "*", everything in remain data matches
          if aMaskIndex=fMaskCompiledLimit then
            exit(TMaskFailCause.mfcSuccess);
          if aMatchOffset>fMatchStringLimit then begin
            if aMaskIndex=fMaskCompiledLimit then
              exit(TMaskFailCause.mfcSuccess);
            exit(TMaskFailCause.mfcMatchStringExhausted);
          end;
          inc(aMaskIndex);
          while aMatchOffset<=fMatchStringLimit do begin
            lFailCause:=intfMatches(aMatchOffset,aMaskIndex);
            if lFailCause=TMaskFailCause.mfcSuccess then
              exit(TMaskFailCause.mfcSuccess)
            else if lFailCause=TMaskFailCause.mfcMatchStringExhausted then
              exit(TMaskFailCause.mfcMatchStringExhausted);
            inc(aMatchOffset,UTF8CodepointSizeFast(@fMatchString[aMatchOffset]));
          end;
          exit(TMaskFailCause.mfcMatchStringExhausted);
        end;
      TMaskParsedCode.AnyCharOrNone:
        begin
          inc(aMaskIndex);
          lTryCounter:=PInteger(@fMaskCompiled[aMaskIndex])^;
          inc(aMaskIndex,sizeof(integer));
          if TMaskParsedCode(fMaskCompiled[aMaskIndex])<>TMaskParsedCode.CharsGroupEnd then
            Exception_InternalError()
          else
            aMaskIndex:=lSkipOnSuccessGroup+1;

          // Try to match remain mask eating, 0,1,2,...,lTryCounter chars.
          for j := 0 to lTryCounter do begin
            if aMatchOffset>fMatchStringLimit then begin
              if aMaskIndex=fMaskCompiledLimit+1 then
                exit(TMaskFailCause.mfcSuccess);
              exit(TMaskFailCause.mfcMatchStringExhausted);
            end;
            lFailCause:=intfMatches(aMatchOffset,aMaskIndex);
            if lFailCause=TMaskFailCause.mfcSuccess then
              exit(TMaskFailCause.mfcSuccess)
            else if lFailCause=TMaskFailCause.mfcMatchStringExhausted then
              exit(TMaskFailCause.mfcMatchStringExhausted);
            inc(aMatchOffset,UTF8CodepointSizeFast(@fMatchString[aMatchOffset]));
          end;
          exit(TMaskFailCause.mfcMatchStringExhausted);
        end;
      else  // case
        begin
          Exception_InternalError();
        end;
    end;
  end;
  if (aMaskIndex>fMaskCompiledLimit) and (aMatchOffset>fMatchStringLimit) then
    Result:=TMaskFailCause.mfcSuccess
  else if aMaskIndex>fMaskCompiledLimit then
    Result:=TMaskFailCause.mfcMaskExhausted
  else
    Result:=TMaskFailCause.mfcMatchStringExhausted;
end;

constructor TMaskUTF8.Create(const aMask: String);
begin
  Create(aMask, False, DefaultMaskOpCodes);
end;

constructor TMaskUTF8.Create(const aMask: String; aCaseSensitive: Boolean);
begin
  Create(aMask, aCaseSensitive, DefaultMaskOpCodes);
end;

constructor TMaskUTF8.Create(const aMask: String;
  aCaseSensitive: Boolean; aOpcodesAllowed: TMaskOpCodes);
begin
  inherited Create(aCaseSensitive,aOpcodesAllowed);
  fOriginalMask:=aMask;
end;


constructor TMaskUTF8.Create(const aMask: String; aOptions: TMaskOptions);
begin
  inherited Create(aOptions);
  fOriginalMask:=aMask;
end;

function TMaskUTF8.Matches(const aStringToMatch: String): Boolean;
begin
  if not fMaskIsCompiled then Compile;
  if fCaseSensitive then
    fMatchString:=aStringToMatch
  else
    fMatchString:=UTF8LowerCase(aStringToMatch);
  fMatchStringLimit:=length(fMatchString);
  if (fMatchStringLimit>=fMatchMinimumLiteralBytes)
  and (fMatchStringLimit<=fMatchMaximumLiteralBytes) then
    Result:=intfMatches(1,0)=TMaskFailCause.mfcSuccess
  else
    Result:=false; // There are too many or not enough bytes to match the string
end;

function TMaskUTF8.MatchesWindowsMask(const AFileName: String): Boolean;
var
  WinMask: TWindowsMaskUTF8;
begin
  WinMask:=TWindowsMaskUTF8.Create(fOriginalMask, CaseSensitive, fMaskOpCodesAllowed);
  try
    Result:=WinMask.Matches(AFileName);
  finally
    WinMask.Free;
  end;
end;

{ TWindowsMask }

procedure TWindowsMaskUTF8.SetMask(AValue: String);
begin
  if (AValue = fWindowsMask) then Exit;
  inherited SetMask(AValue);
  fWindowsMask := AValue;
end;

function TWindowsMaskUTF8.GetMask: String;
begin
  Result := fWindowsMask;
end;

procedure TWindowsMaskUTF8.SetWindowsQuirkAllowed(AValue: TWindowsQuirks);
begin
  if fWindowsQuirkAllowed = AValue then Exit;
  fWindowsQuirkAllowed := AValue;
  if (wqFilenameEnd in fWindowsQuirkAllowed) then
    Include(fMaskOpcodesAllowed, mocAnyCharOrNone);
  fMaskIsCompiled := False;
end;

class procedure TWindowsMaskUTF8.SplitFileNameExtension(
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

constructor TWindowsMaskUTF8.Create(const aMask: String;
  aCaseSensitive: Boolean; aOpcodesAllowed: TMaskOpCodes);
begin
  Create(aMask, aCaseSensitive, aOpcodesAllowed, DefaultWindowsQuirks);
end;

constructor TWindowsMaskUTF8.Create(const aMask: String; aCaseSensitive: Boolean;
  aOpcodesAllowed: TMaskOpCodes; aWindowsQuirksAllowed: TWindowsQuirks);
begin
  fWindowsQuirkAllowed:=aWindowsQuirksAllowed;
  fWindowsMask:=aMask;
  if (wqFilenameEnd in fWindowsQuirkAllowed) then
    Include(aOpcodesAllowed, mocAnyCharOrNone);
  inherited Create(aMask,aCaseSensitive,aOpcodesAllowed);
  //don't compile on create: we do NOT want a crash in the constructor
  {
  Compile;
  }
end;

procedure TWindowsMaskUTF8.Compile;

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

  {
  function EscapeSpecialChars(const aString: String): String;
  var
    j: integer;
  begin
    Result:=aString;
    for j := Length(Result) downto 1 do begin
      if Result[j] in ['[',']',fMaskEscapeChar] then begin
        // Escape the []\ chars as in Windows mask mode they are plain chars.
        insert(fMaskEscapeChar,Result,j);
      end;
    end;
  end;
  }
var
  lFileNameMask: String;
  lExtensionMask: String;
  lModifiedMask: String;

begin
  lModifiedMask:=fWindowsMask;

  // Quirk "blah.*" = "blah*"
  if wqAnyExtension in fWindowsQuirkAllowed then begin
    if RightStr(lModifiedMask,3)='*.*' then begin
      lModifiedMask:=copy(lModifiedMask,1,Length(lModifiedMask)-2);
      fWindowsQuirkInUse:=fWindowsQuirkInUse+[wqAnyExtension];
    end;
  end;

  SplitFileNameExtension(lModifiedMask,lFileNameMask,lExtensionMask,true);

  // Quirk "blah.abc" = "blah.abc*"
  if wqExtension3More in fWindowsQuirkAllowed then begin
    if (Length(lExtensionMask)=4) and (Length(lFileNameMask)>0) then begin
      lExtensionMask:=lExtensionMask+'*';
      fWindowsQuirkInUse:=fWindowsQuirkInUse+[wqExtension3More];
    end;
  end;

  // Quirk "" = "*"
  if (Length(lFileNameMask)=0) and (Length(lExtensionMask)=0) then begin
    if wqEmptyIsAny in fWindowsQuirkAllowed then begin
      lFileNameMask:='*';
      fWindowsQuirkInUse:=fWindowsQuirkInUse+[wqEmptyIsAny];
    end;
  end else begin
  // Quirk ".abc"
    if wqAllByExtension in fWindowsQuirkAllowed then begin
      if (Length(lFileNameMask)=0) and (length(lExtensionMask)>0) then begin
        if lExtensionMask[1]='.' then begin
          lFileNameMask:='*';
          fWindowsQuirkInUse:=fWindowsQuirkInUse+[wqAllByExtension];
        end;
      end;
    end;
  end;

  //Don't disable ranges for Windows
  {
  lFileNameMask:=EscapeSpecialChars(lFileNameMask);
  lExtensionMask:=EscapeSpecialChars(lExtensionMask);
  }
  // Quirk "file???.ab?" matches "file1.ab1" and "file123.ab"
  if wqFilenameEnd in fWindowsQuirkAllowed then begin
    lFileNameMask:=OptionalQMarksAtEnd(lFileNameMask);
    lExtensionMask:=OptionalQMarksAtEnd(lExtensionMask);
  end;

  if wqNoExtension in fWindowsQuirkAllowed then begin
    if Length(lExtensionMask)=1 then begin
      fWindowsQuirkInUse:=fWindowsQuirkInUse+[wqNoExtension];
      lExtensionMask:='';
    end;
  end;

  inherited Mask:=lFileNameMask+lExtensionMask;
  inherited Compile;
end;

function TWindowsMaskUTF8.Matches(const aFileName: String): Boolean;
var
  lFileName, lExtension: String;
begin
  if not fMaskIsCompiled then
    Compile;
  if wqNoExtension in fWindowsQuirkInUse then begin
    SplitFileNameExtension(aFileName,lFileName,lExtension,false);
    // wqNoExtension = Empty extension
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
  CaseSensitive: Boolean; aOpcodesAllowed: TMaskOpCodes);
begin
  fMask := aValue;
  fSeparator := aSeparator;
  fCaseSensitive := CaseSensitive;
  fMaskOpcodes := aOpcodesAllowed;
  fAutoReverseRange := True;

  fMasks := TObjectList.Create(True);
  FMaskClass := GetMaskClass;
  AddMasksToList(aValue, aSeparator, CaseSensitive, aOpcodesAllowed);
end;

constructor TMaskList.Create(const aValue: String; aSeparator: Char; aOptions: TMaskOptions);
var
  CaseSens: Boolean;
  Opcodes: TMaskOpCodes;
begin
  CaseSens:=moCaseSensitive in aOptions;
  if moDisableSets in aOptions then
    Opcodes:=MaskOpCodesDisableRange
  else
    Opcodes:=DefaultMaskOpCodes;
  Create(aValue, aSeparator, CaseSens, Opcodes);
end;

destructor TMaskList.Destroy;
begin
  fMasks.Free;
  inherited Destroy;
end;

function TMaskList.GetItem(Index: Integer): TMask;
begin
  Result := TMask(fMasks.Items[Index]);
end;

procedure TMaskList.SetAutoReverseRange(AValue: Boolean);
var
  i: Integer;
begin
  if fAutoReverseRange = AValue then Exit;
  fAutoReverseRange := AValue;
  for i := 0 to fMasks.Count - 1 do
    TMask(fMasks.Items[i]).AutoReverseRange := fAutoReverseRange;
end;

procedure TMaskList.SetCaseSensitive(AValue: Boolean);
var
  i: Integer;
begin
  if fCaseSensitive = AValue then Exit;
  fCaseSensitive := AValue;
  for i := 0 to fMasks.Count - 1 do
    TMask(fMasks.Items[i]).CaseSensitive := fCaseSensitive;
end;

procedure TMaskList.SetMask(AValue: String);
begin
  if fMask = AValue then Exit;
  fMask := AValue;
  fMasks.Clear;
  AddMasksToList(fMask, fSeparator, fCaseSensitive, fMaskOpCodes);
end;

procedure TMaskList.SetMaskOpCodes(AValue: TMaskOpCodes);
var
  i: Integer;
begin
  if FMaskOpCodes = AValue then Exit;
  fMaskOpCodes := AValue;
  for i := 0 to fMasks.Count - 1 do
    TMask(fMasks.Items[i]).MaskOpCodes := fMaskOpcodes;
end;

function TMaskList.GetMaskClass: TMaskClass;
begin
  Result := TMask;
end;

procedure TMaskList.AddMasksToList(const aValue: String; aSeparator: Char; CaseSensitive: Boolean;
      aOpcodesAllowed: TMaskOpCodes);
var
  S: TParseStringList;
  i: Integer;
begin
  S := TParseStringList.Create(aValue, aSeparator);
  try
    for i := 0 to S.Count-1 do begin
      fMasks.Add(FMaskClass.Create(S[i], CaseSensitive, aOpcodesAllowed));
    end;
  finally
    S.Free;
  end;
end;

function TMaskList.GetCount: Integer;
begin
  Result := fMasks.Count;
end;

function TMaskList.Matches(const AFileName: String): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to fMasks.Count-1 do
    if ((fMasks.Items[I]) as FMaskClass).Matches(AFileName) then
      Exit(True);
end;

function TMaskList.MatchesWindowsMask(const AFileName: String): Boolean;
//use the same hack as in TMask.MatchesWindowsMask
var
  WML: TWindowsMaskList;
begin
  WML := TWindowsMaskList.Create(fMask, fSeparator, fCaseSensitive, fMaskOpcodes, DefaultWindowsQuirks);
  try
    Result := WML.Matches(AFileName);
  finally
    WML.Free;
  end;
end;

end.

