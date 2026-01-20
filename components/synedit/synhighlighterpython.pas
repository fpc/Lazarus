{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterPython.pas, released 2000-06-23.
The Original Code is based on the odPySyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Olivier Deckmyn.
Portions created by M.Utku Karatas and Dennis Chuah.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id$

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(A Python language highlighter for SynEdit)
@author(Olivier Deckmyn, converted to SynEdit by David Muir <dhmn@dmsoftware.co.uk>)
@created(unknown, converted to SynEdit on 2000-06-23)
@lastmod(2003-02-13)
The SynHighlighterPython implements a highlighter for Python for the SynEdit projects.
}
unit SynHighlighterPython;

{$I SynEdit.inc}

interface

uses
  IniFiles, //THashedStringList
  LCLIntf, LCLType,
  SynEditHighlighter, SynEditTypes, SynEditStrConst, LazEditTextAttributes,
  Graphics, SysUtils, Classes;

const
  ALPHA_CHARS = ['_', 'a'..'z', 'A'..'Z'];
  IDENTIFIER_CHARS = ['0'..'9'] + ALPHA_CHARS;

type
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull, tkNumber, tkSpace,
    tkString, tkSymbol, tkNonKeyword, tkTripleQuotedString,
    tkSystemDefined, tkHex, tkOct, tkFloat, tkUnknown);

  TRangeState = (rsANil, rsComment, rsUnKnown, rsMultilineString, rsMultilineString2,
                 rsMultilineString3 //this is to indicate if a string is made multiline by backslash char at line end (as in C++ highlighter)
                );

  TProcTableProc = procedure of object;

type

  { TSynPythonSyn }

  TSynPythonSyn = class(TSynCustomHighLighter)
  private type
    TSynPasAttribute = (
    attribComment,
    attribIdentifier,
    attribKey,
    attribNonKey,
    attribSystem,
    attribNumber,
    attribHex,
    attribOctal,
    attribFloat,
    attribSpace,
    attribString,
    attribDocString,
    attribSymbol,
    attribError
    );
  private
    fStringStarter: char;  // used only for rsMultilineString3 stuff
    fRange: TRangeState;
    fProcTable: array[#0..#255] of TProcTableProc;
    fToIdent: PChar;
    fTokenPos: Integer;
    FTokenID: TtkTokenKind;
    FKeywords: TStringList;

    fStringAttri: TSynHighlighterAttributes;
    fDocStringAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fHexAttri: TSynHighlighterAttributes;
    fOctalAttri: TSynHighlighterAttributes;
    fFloatAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNonKeyAttri: TSynHighlighterAttributes;
    fSystemAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fErrorAttri: TSynHighlighterAttributes;

    procedure SetAttribute(AnIndex: TSynPasAttribute; AValue: TSynHighlighterAttributes);
    procedure SymbolProc;
    procedure CRProc;
    procedure CommentProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure NullProc;
    procedure NumberProc;
    procedure SpaceProc;
    procedure PreStringProc;
    procedure UnicodeStringProc;
    procedure StringProc;
    procedure String2Proc;
    procedure StringEndProc(EndChar:char);
    procedure UnknownProc;
    procedure MakeMethodTables;

  protected
    Run: LongInt;
    fStringLen: Integer;

    function GetIdentChars: TSynIdentChars; override;
    function GetSampleSource: string; override;
    function IdentKind(MayBe: PChar): TtkTokenKind; virtual;
    property Keywords: TStringlist read FKeywords;
    property TokenID: TtkTokenKind read FTokenID;

  public
    class function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    procedure InitForScanningLine; override;
    function GetKeywordIdentifiers: TStringList; virtual;
    function GetToken: string; override;
    function GetTokenAttribute: TLazEditTextAttribute; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
    property IdentChars;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
  published
    property CommentAttri: TSynHighlighterAttributes index attribComment read fCommentAttri write SetAttribute;
    property IdentifierAttri: TSynHighlighterAttributes index attribIdentifier read fIdentifierAttri write SetAttribute;
    property KeyAttri: TSynHighlighterAttributes index attribKey read fKeyAttri write SetAttribute;
    property NonKeyAttri: TSynHighlighterAttributes index attribNonKey read fNonKeyAttri write SetAttribute;
    property SystemAttri: TSynHighlighterAttributes index attribSystem read fSystemAttri write SetAttribute;
    property NumberAttri: TSynHighlighterAttributes index attribNumber read fNumberAttri write SetAttribute;
    property HexAttri: TSynHighlighterAttributes index attribHex read fHexAttri write SetAttribute;
    property OctalAttri: TSynHighlighterAttributes index attribOctal read fOctalAttri write SetAttribute;
    property FloatAttri: TSynHighlighterAttributes index attribFloat read fFloatAttri write SetAttribute;
    property SpaceAttri: TSynHighlighterAttributes index attribSpace read fSpaceAttri write SetAttribute;
    property StringAttri: TSynHighlighterAttributes index attribString read fStringAttri write SetAttribute;
    property DocStringAttri: TSynHighlighterAttributes index attribDocString read fDocStringAttri write SetAttribute;
    property SymbolAttri: TSynHighlighterAttributes index attribSymbol read fSymbolAttri write SetAttribute;
    property ErrorAttri: TSynHighlighterAttributes index attribError read fErrorAttri write SetAttribute;
  end;

implementation

var
  GlobalKeywords: TStringList;

function TSynPythonSyn.GetKeywordIdentifiers: TStringList;
const
  // No need to localise keywords!

  // List of keywords
  KEYWORDCOUNT = 35;
  KEYWORDSIdents: array [1..KEYWORDCOUNT] of string =
    (
    'as',
    'and',
    'assert',
    'async',
    'await',
    'break',
    'class',
    'continue',
    'def',
    'del',
    'elif',
    'else',
    'except',
    'False',
    'finally',
    'for',
    'from',
    'global',
    'if',
    'import',
    'in',
    'is',
    'lambda',
    'None',
    'nonlocal',
    'not',
    'or',
    'pass',
    'raise',
    'return',
    'True',
    'try',
    'while',
    'with',
    'yield'
    );

  // List of non-keyword identifiers
  NONKEYWORDCOUNT = 78;
  NONKEYWORDS: array [1..NONKEYWORDCOUNT] of string =
    (
    '__future__',
    '__import__',
    'abs',
    'aiter',
    'all',
    'anext',
    'any',
    'ascii',
    'bin',
    'bool',
    'breakpoint',
    'bytearray',
    'bytes',
    'callable',
    'chr',
    'classmethod',
    'compile',
    'complex',
    'copyright',
    'credits',
    'delattr',
    'dict',
    'dir',
    'divmod',
    'enumerate',
    'eval',
    'exec',
    'exit',
    'filter',
    'float',
    'format',
    'frozenset',
    'getattr',
    'globals',
    'hasattr',
    'hash',
    'help',
    'hex',
    'id',
    'input',
    'int',
    'isinstance',
    'issubclass',
    'iter',
    'len',
    'license',
    'list',
    'locals',
    'map',
    'max',
    'memoryview',
    'min',
    'next',
    'NotImplemented',
    'object',
    'oct',
    'open',
    'ord',
    'pow',
    'print',
    'property',
    'quit',
    'range',
    'repr',
    'reversed',
    'round',
    'set',
    'setattr',
    'slice',
    'sorted',
    'staticmethod',
    'str',
    'sum',
    'super',
    'tuple',
    'type',
    'vars',
    'zip'
    );
var
  f: Integer;
begin
  if not Assigned (GlobalKeywords) then begin
    // Create the string list of keywords - only once
    GlobalKeywords := TStringList.Create;

    for f := 1 to KEYWORDCOUNT do
      GlobalKeywords.AddObject (KEYWORDSIdents[f],
        TObject(Ord(tkKey)));
    for f := 1 to NONKEYWORDCOUNT do
      GlobalKeywords.AddObject (NONKEYWORDS[f],
        TObject(Ord(tkNonKeyword)));
  end; // if
  Result := GlobalKeywords;
end;

function TSynPythonSyn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  index: Integer;
  temp: PChar;
  s: string;

begin
  // Extract the identifier out - it is assumed to terminate in a
  //   non-alphanumeric character
  fToIdent := MayBe;
  temp := MayBe;
  while temp^ in IDENTIFIER_CHARS do
    Inc (temp);
  fStringLen := temp - fToIdent;

  // Check to see if it is a keyword
  SetString (s, fToIdent, fStringLen);
  index := FKeywords.IndexOf (s);

  if index <> -1 then
    Result := TtkTokenKind (PtrInt(FKeywords.Objects[index]))

  // Check if it is a system identifier (__*__)
  else if (fStringLen >= 5) and
     (MayBe[0] = '_') and (MayBe[1] = '_') and (MayBe[2] <> '_') and
     (MayBe[fStringLen-1] = '_') and (MayBe[fStringLen-2] = '_') and
     (MayBe[fStringLen-3] <> '_') then
    Result := tkSystemDefined

  // Else, hey, it is an ordinary run-of-the-mill identifier!
  else
    Result := tkIdentifier;
end;
  
procedure TSynPythonSyn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      '&', '}', '{', ':', ',', ']', '[', '*', '`',
      '^', ')', '(', ';', '/', '=', '-', '+', '!', '\',
      '%', '|', '~' :
        fProcTable[I] := @SymbolProc;
      #13: fProcTable[I] := @CRProc;
      '#': fProcTable[I] := @CommentProc;
      '>': fProcTable[I] := @GreaterProc;
      'A'..'Q', 'S', 'T', 'V'..'Z', 'a'..'q', 's', 't', 'v'..'z', '_': fProcTable[I] := @IdentProc;
      #10: fProcTable[I] := @LFProc;
      '<': fProcTable[I] := @LowerProc;
      #0: fProcTable[I] := @NullProc;
      '.', '0'..'9': fProcTable[I] := @NumberProc;
      #1..#9, #11, #12, #14..#32:
        fProcTable[I] := @SpaceProc;
      'r', 'R': fProcTable[I] := @PreStringProc;
      'u', 'U': fProcTable[I] := @UnicodeStringProc;
      '''': fProcTable[I] := @StringProc;
      '"': fProcTable[I] := @String2Proc;
    else
      fProcTable[I] := @UnknownProc;
    end;
end;

constructor TSynPythonSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
  FKeywords := THashedStringList.Create;
  FKeywords.CaseSensitive := True;
  FKeywords.Duplicates := dupError;
  FKeywords.Assign (GetKeywordIdentifiers);

  fRange := rsUnknown;
  fCommentAttri := TSynHighlighterAttributes.Create(@SYNS_AttrComment, SYNS_XML_AttrComment);
  fCommentAttri.Foreground := clGray;
  fCommentAttri.Style := [fsItalic];
  AddAttribute(fCommentAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(@SYNS_AttrIdentifier, SYNS_XML_AttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(@SYNS_AttrReservedWord, SYNS_XML_AttrReservedWord);
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);
  fNonKeyAttri := TSynHighlighterAttributes.Create(@SYNS_AttrNonReservedKeyword, SYNS_XML_AttrNonReservedKeyword);
  fNonKeyAttri.Foreground := clNavy;
  fNonKeyAttri.Style := [fsBold];
  AddAttribute (fNonKeyAttri);
  fSystemAttri := TSynHighlighterAttributes.Create(@SYNS_AttrSystem, SYNS_XML_AttrSystem);
  fSystemAttri.Style := [fsBold];
  AddAttribute (fSystemAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(@SYNS_AttrNumber, SYNS_XML_AttrNumber);
  fNumberAttri.Foreground := clBlue;
  AddAttribute(fNumberAttri);
  fHexAttri := TSynHighlighterAttributes.Create(@SYNS_AttrHexadecimal, SYNS_XML_AttrHexadecimal);
  fHexAttri.Foreground := clBlue;
  AddAttribute(fHexAttri);
  fOctalAttri := TSynHighlighterAttributes.Create(@SYNS_AttrOctal, SYNS_XML_AttrOctal);
  fOctalAttri.Foreground := clBlue;
  AddAttribute(fOctalAttri);
  fFloatAttri := TSynHighlighterAttributes.Create(@SYNS_AttrFloat, SYNS_XML_AttrFloat);
  fFloatAttri.Foreground := clBlue;
  AddAttribute(fFloatAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(@SYNS_AttrSpace, SYNS_XML_AttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(@SYNS_AttrString, SYNS_XML_AttrString);
  fStringAttri.Foreground := clBlue;
  AddAttribute(fStringAttri);
  fDocStringAttri := TSynHighlighterAttributes.Create(@SYNS_AttrDocumentation, SYNS_XML_AttrDocumentation);
  fDocStringAttri.Foreground := clTeal;
  AddAttribute(fDocStringAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(@SYNS_AttrSymbol, SYNS_XML_AttrSymbol);
  AddAttribute(fSymbolAttri);
  fErrorAttri := TSynHighlighterAttributes.Create(@SYNS_AttrSyntaxError, SYNS_XML_AttrSyntaxError);
  fErrorAttri.Foreground := clRed;
  AddAttribute(fErrorAttri);
  SetAttributesOnChange(@DefHighlightChange);
  MakeMethodTables;
  fDefaultFilter := SYNS_FilterPython;
end; { Create }

destructor TSynPythonSyn.Destroy;
begin
  FKeywords.Free;
  
  inherited;
end;

procedure TSynPythonSyn.InitForScanningLine;
begin
  inherited;
  Run := 0;
  Next;
end;

procedure TSynPythonSyn.GetTokenEx(out TokenStart: PChar;
  out TokenLength: integer);
begin
  TokenLength:=Run-fTokenPos;
  TokenStart:=LinePtr + fTokenPos;
end;

procedure TSynPythonSyn.SymbolProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynPythonSyn.SetAttribute(AnIndex: TSynPasAttribute; AValue: TSynHighlighterAttributes);
begin
  case AnIndex of
    attribComment:    FCommentAttri.Assign(AValue);
    attribIdentifier: FIdentifierAttri.Assign(AValue);
    attribKey:        FKeyAttri.Assign(AValue);
    attribNonKey:     FNonKeyAttri.Assign(AValue);
    attribSystem:     FSystemAttri.Assign(AValue);
    attribNumber:     FNumberAttri.Assign(AValue);
    attribHex:        FHexAttri.Assign(AValue);
    attribOctal:      FOctalAttri.Assign(AValue);
    attribFloat:      FFloatAttri.Assign(AValue);
    attribSpace:      FSpaceAttri.Assign(AValue);
    attribString:     FStringAttri.Assign(AValue);
    attribDocString:  FDocStringAttri.Assign(AValue);
    attribSymbol:     FSymbolAttri.Assign(AValue);
    attribError:      FErrorAttri.Assign(AValue);
  end;
end;

procedure TSynPythonSyn.CRProc;
begin
  fTokenID := tkSpace;
  case LinePtr[Run + 1] of
    #10: inc(Run, 2);
  else
    inc(Run);
  end;
end;

procedure TSynPythonSyn.CommentProc;
begin
  fTokenID := tkComment;
  inc(Run);
  while not (LinePtr[Run] in [#13, #10, #0]) do
    inc(Run);
end;

procedure TSynPythonSyn.GreaterProc;
begin
  case LinePtr[Run + 1] of
    '=': begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
  else begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPythonSyn.IdentProc;
begin
  fTokenID := IdentKind((LinePtr + Run));
  inc(Run, fStringLen);
end;

procedure TSynPythonSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynPythonSyn.LowerProc;
begin
  case LinePtr[Run + 1] of
    '=': begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end;
    '>': begin
        inc(Run, 2);
        fTokenID := tkSymbol;
      end
  else begin
      inc(Run);
      fTokenID := tkSymbol;
    end;
  end;
end;

procedure TSynPythonSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynPythonSyn.NumberProc;
const
  INTCHARS = ['0' .. '9'];
  HEXCHARS = ['a' .. 'f', 'A' .. 'F'] + INTCHARS;
  OCTCHARS = ['0' .. '7'];
  HEXINDICATOR = ['x', 'X'];
  OCTINDICATOR = ['o', 'O'];
  LONGINDICATOR = ['l', 'L'];
  IMAGINARYINDICATOR = ['j', 'J'];
  EXPONENTINDICATOR = ['e', 'E'];
  EXPONENTSIGN = ['+', '-'];
  DOT = '.';
  ZERO = '0';

type
  TNumberState =
    (
    nsStart,
    nsDotFound,
    nsFloatNeeded,
    nsHex,
    nsOct,
    nsExpFound
    );

var
  temp: Char;
  State: TNumberState;

  function CheckSpecialCases: Boolean;
  begin
    case temp of
      // Look for dot (.)
      DOT: begin
        // .45
        if LinePtr[Run] in INTCHARS then begin
          Inc (Run);
          fTokenID := tkFloat;
          State := nsDotFound;

        // Non-number dot
        end else begin
          // Ellipsis
          if (LinePtr[Run] = DOT) and (LinePtr[Run+1] = DOT) then
            Inc (Run, 2);
          fTokenID := tkSymbol;
          Result := False;
          Exit;
        end; // if
      end; // DOT

      // Look for zero (0)
      ZERO: begin
        temp := LinePtr[Run];
        // 0x123ABC
        if temp in HEXINDICATOR then begin
          Inc (Run);
          fTokenID := tkHex;
          State := nsHex;
        // 0o17
        end else if temp in OCTINDICATOR then begin
          Inc (Run);
          fTokenID := tkOct;
          State := nsOct;
        // 0.45
        end else if temp = DOT then begin
          Inc (Run);
          State := nsDotFound;
          fTokenID := tkFloat;
        end else if temp in INTCHARS then begin
          (* Before 3.0 (2008) this may have been octal
             Now it is float (must have decimal dot, otherwise it is invalid)
           *)
          Inc (Run);
          fTokenID := tkFloat;
          State := nsFloatNeeded;
        end; // if
      end; // ZERO
    end; // case

    Result := True;
  end; // CheckSpecialCases

  function HandleBadNumber: Boolean;
  begin
    Result := False;
    fTokenID := tkUnknown;
    // Ignore all tokens till end of "number"
    while LinePtr[Run] in (IDENTIFIER_CHARS + ['.']) do
      Inc (Run);
  end; // HandleBadNumber

  function HandleExponent: Boolean;
  begin
    State := nsExpFound;
    fTokenID := tkFloat;
    // Skip e[+/-]
    if LinePtr[Run+1] in EXPONENTSIGN then
      Inc (Run);
    // Invalid token : 1.0e
    if not (LinePtr[Run+1] in INTCHARS) then begin
      Inc (Run);
      Result := HandleBadNumber;
      Exit;
    end; // if

    Result := True;
  end; // HandleExponent

  function HandleDot: Boolean;
  begin
    // Check for ellipsis
    Result := (LinePtr[Run+1] <> DOT) or (LinePtr[Run+2] <> DOT);
    if Result then begin
      State := nsDotFound;
      fTokenID := tkFloat;
    end; // if
  end; // HandleDot

  function CheckStart: Boolean;
  begin
    // 1234
    if temp in INTCHARS then begin
      Result := True;
    //123e4
    end else if temp in EXPONENTINDICATOR then begin
      Result := HandleExponent;
    // 123.45j
    end else if temp in IMAGINARYINDICATOR then begin
      Inc (Run);
      fTokenID := tkFloat;
      Result := False;
    // 123.45
    end else if temp = DOT then begin
      Result := HandleDot;
    // Error!
    end else if temp in IDENTIFIER_CHARS then begin
      Result := HandleBadNumber;
    // End of number
    end else begin
      Result := False;
    end; // if
  end; // CheckStart

  function CheckDotFound: Boolean;
  begin
    // 1.0e4
    if temp in EXPONENTINDICATOR then begin
      Result := HandleExponent;
    // 123.45
    end else if temp in INTCHARS then begin
      Result := True;
    // 123.45j
    end else if temp in IMAGINARYINDICATOR then begin
      Inc (Run);
      Result := False;
    // 123.45.45: Error!
    end else if temp = DOT then begin
      Result := False;
      if HandleDot then
        HandleBadNumber;
    // Error!
    end else if temp in IDENTIFIER_CHARS then begin
      Result := HandleBadNumber;
    // End of number
    end else begin
      Result := False;
    end; // if
  end; // CheckDotFound

  function CheckFloatNeeded: Boolean;
  begin
    // 091.0e4
    if temp in EXPONENTINDICATOR then begin
      Result := HandleExponent;
    // 0912345
    end else if temp in INTCHARS then begin
      Result := True;
    // 09123.45
    end else if temp = DOT then begin
      Result := HandleDot or HandleBadNumber; // Bad octal
    // 09123.45j
    end else if temp in IMAGINARYINDICATOR then begin
      Inc (Run);
      Result := False;
    // End of number (error: Bad oct number) 0912345
    end else begin
      Result := HandleBadNumber;
    end;
  end; // CheckFloatNeeded

  function CheckHex: Boolean;
  begin
    // 0x123ABC
    if temp in HEXCHARS then begin
      Result := True;
    // 0x123ABCL
    end else if temp in LONGINDICATOR then begin
      Inc (Run);
      Result := False;
    // 0x123.45: Error!
    end else if temp = DOT then begin
      Result := False;
      if HandleDot then
        HandleBadNumber;
    // Error!
    end else if temp in IDENTIFIER_CHARS then begin
      Result := HandleBadNumber;
    // End of number
    end else begin
      Result := False;
    end; // if
  end; // CheckHex

  function CheckOct: Boolean;
  begin
    // 012345
    if temp in OCTCHARS then begin
      Result := True;
    // 012345L
    end else if temp in LONGINDICATOR then begin
      Inc (Run);
      Result := False;
    // 0123e4
    end else if temp in EXPONENTINDICATOR then begin
      Result := HandleExponent;
    // 0123j
    end else if temp in IMAGINARYINDICATOR then begin
      Inc (Run);
      fTokenID := tkFloat;
      Result := False;
    // 0123.45
    end else if temp = DOT then begin
      Result := HandleDot;
    // Error!
    end else if temp in IDENTIFIER_CHARS then begin
      Result := HandleBadNumber;
    // End of number
    end else begin
      Result := False;
    end; // if
  end; // CheckOct

  function CheckExpFound: Boolean;
  begin
    // 1e+123
    if temp in INTCHARS then begin
      Result := True;
    // 1e+123j
    end else if temp in IMAGINARYINDICATOR then begin
      Inc (Run);
      Result := False;
    // 1e4.5: Error!
    end else if temp = DOT then begin
      Result := False;
      if HandleDot then
        HandleBadNumber;
    // Error!
    end else if temp in IDENTIFIER_CHARS then begin
      Result := HandleBadNumber;
    // End of number
    end else begin
      Result := False;
    end; // if
  end; // CheckExpFound

begin
  State := nsStart;
  fTokenID := tkNumber;

  temp := LinePtr[Run];
  Inc (Run);

  // Special cases
  if not CheckSpecialCases then
    Exit;

  // Use a state machine to parse numbers
  while True do begin
    temp := LinePtr[Run];

    case State of
      nsStart:
        if not CheckStart then Exit;
      nsDotFound:
        if not CheckDotFound then Exit;
      nsFloatNeeded:
        if not CheckFloatNeeded then Exit;
      nsHex:
        if not CheckHex then Exit;
      nsOct:
        if not CheckOct then Exit;
      nsExpFound:
        if not CheckExpFound then Exit;
    end; // case

    Inc (Run);
  end; // while

{
begin
  while LinePtr[Run] in ['0'..'9', '.', 'e', 'E'] do begin
    case LinePtr[Run] of
      '.':
        if LinePtr[Run + 1] = '.' then break;
    end;
    inc(Run);
  end;
}
end;

procedure TSynPythonSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while LinePtr[Run] in [#1..#9, #11, #12, #14..#32] do
    inc(Run);
end;

procedure TSynPythonSyn.String2Proc;
var fBackslashCount:integer;
begin
  fTokenID := tkString;
  if (LinePtr[Run + 1] = '"') and (LinePtr[Run + 2] = '"') then begin
    fTokenID := tkTripleQuotedString;
    inc(Run, 3);

    fRange:=rsMultilineString2;
    while LinePtr[Run] <> #0 do begin
      case LinePtr[Run] of

        '\':begin
               { If we're looking at a backslash, and the following character is an
               end quote, and it's preceeded by an odd number of backslashes, then
               it shouldn't mark the end of the string.  If it's preceeded by an
               even number, then it should. !!!THIS RULE DOESNT APPLY IN RAW STRINGS}
               if LinePtr[Run + 1] = '"' then
                 begin
                   fBackslashCount := 1;

                   while ((Run > fBackslashCount) and (LinePtr[Run - fBackslashCount] = '\')) do
                     fBackslashCount := fBackslashCount + 1;

                   if (fBackslashCount mod 2 = 1) then inc(Run)
               end;
               inc(Run);
            end;// '\':

        '"':
          if (LinePtr[Run + 1] = '"') and (LinePtr[Run + 2] = '"') then begin
            fRange := rsUnKnown;
            inc(Run, 3);
            EXIT;
          end else
            inc(Run);
        #10:EXIT;
        #13:EXIT;
        else
          inc(Run);
      end;
    end;
  end
      else //if short string
  repeat
    case LinePtr[Run] of
      #0, #10, #13 : begin
        if LinePtr[Run-1] = '\' then begin
          fStringStarter := '"';
          fRange := rsMultilineString3;
        end;
        BREAK;
        end;
      {The same backslash stuff above...}
      '\':begin
             if LinePtr[Run + 1] = '"' then
               begin
                 fBackslashCount := 1;

                 while ((Run > fBackslashCount) and (LinePtr[Run - fBackslashCount] = '\')) do
                   fBackslashCount := fBackslashCount + 1;

                 if (fBackslashCount mod 2 = 1) then inc(Run)
             end;
             inc(Run);
          end;// '\':

      else inc(Run);
    end; //case
  until (LinePtr[Run] = '"');
  if LinePtr[Run] <> #0 then inc(Run);
end;

procedure TSynPythonSyn.PreStringProc;
var
  temp: Char;

begin
  // Handle python raw strings
  // r""
  temp := LinePtr[Run + 1];
  if temp = '''' then begin
    Inc (Run);
    StringProc;
  end else if temp = '"' then begin
    Inc (Run);
    String2Proc;
  end else begin
    // If not followed by quote char, must be ident
    IdentProc;
  end; // if
end;

procedure TSynPythonSyn.UnicodeStringProc;
begin
  // Handle python raw and unicode strings
  // Valid syntax: u"", or ur""
  if (LinePtr[Run + 1] in ['r', 'R']) and (LinePtr[Run + 2] in ['''', '"']) then
    // for ur, Remove the "u" and...
    Inc (Run);
  // delegate to raw strings
  PreStringProc;
end;

procedure TSynPythonSyn.StringProc;
var fBackslashCount:integer;
begin
  fTokenID := tkString;
  if (LinePtr[Run + 1] = #39) and (LinePtr[Run + 2] = #39) then begin
    fTokenID := tkTripleQuotedString;
    inc(Run, 3);

    fRange:=rsMultilineString;
    while LinePtr[Run] <> #0 do begin
      case LinePtr[Run] of

        '\': begin
             { If we're looking at a backslash, and the following character is an
             end quote, and it's preceeded by an odd number of backslashes, then
             it shouldn't mark the end of the string.  If it's preceeded by an
             even number, then it should. !!!THIS RULE DOESNT APPLY IN RAW STRINGS}
              if LinePtr[Run + 1] = #39 then
                begin
                  fBackslashCount := 1;

                  while ((Run > fBackslashCount) and (LinePtr[Run - fBackslashCount] = '\')) do
                    fBackslashCount := fBackslashCount + 1;

                  if (fBackslashCount mod 2 = 1) then inc(Run)
              end;
              inc(Run);
            end;// '\':

        #39:
          if (LinePtr[Run + 1] = #39) and (LinePtr[Run + 2] = #39) then begin
            fRange := rsUnKnown;
            inc(Run, 3);
            EXIT;
          end else
            inc(Run);
        #10: EXIT;
        #13: EXIT;
        else
          inc(Run);
      end;
    end;
  end
      else //if short string
  repeat
    case LinePtr[Run] of
      #0, #10, #13 : begin
        if LinePtr[Run-1] = '\' then begin
          fStringStarter := #39;
          fRange := rsMultilineString3;
        end;
        BREAK;
        end;

      {The same backslash stuff above...}
      '\':begin
             if LinePtr[Run + 1] = #39 then
               begin
                 fBackslashCount := 1;

                 while ((Run > fBackslashCount) and (LinePtr[Run - fBackslashCount] = '\')) do
                   fBackslashCount := fBackslashCount + 1;

                 if (fBackslashCount mod 2 = 1) then inc(Run)
             end;
             inc(Run);
          end;// '\':

      else inc(Run);
    end; //case
  until (LinePtr[Run] = #39);
  if LinePtr[Run] <> #0 then inc(Run);
end;

procedure TSynPythonSyn.StringEndProc(EndChar:char);
var fBackslashCount:integer;
begin
  if fRange = rsMultilineString3 then
    fTokenID := tkString
  else
    fTokenID := tkTripleQuotedString;

  case LinePtr[Run] of
    #0:
      begin
        NullProc;
        EXIT;
      end;
    #10:
      begin
        LFProc;
        EXIT;
    end;
    #13:
      begin
        CRProc;
        EXIT;
      end;
  end;

  if fRange = rsMultilineString3 then begin
    repeat
      if LinePtr[Run]=fStringStarter then begin
        inc(Run);
        fRange:=rsUnknown;
        EXIT;
      end else if LinePtr[Run]='\' then {The same backslash stuff above...}
          begin
             if LinePtr[Run + 1] = fStringStarter then
               begin
                 fBackslashCount := 1;

                 while ((Run > fBackslashCount) and (LinePtr[Run - fBackslashCount] = '\')) do
                   fBackslashCount := fBackslashCount + 1;

                 if (fBackslashCount mod 2 = 1) then inc(Run);
             end;
           end;// if LinePtr[Run]...

      inc(Run);
    until (LinePtr[Run] in [#0, #10, #13]);
    if LinePtr[Run-1]<>'\' then begin
      fRange:=rsUnknown;
      EXIT;
    end;
  end else
  repeat
    if LinePtr[Run] = '\' then
    begin
       if LinePtr[Run + 1] = EndChar then
         begin
           fBackslashCount := 1;

           while ((Run > fBackslashCount) and (LinePtr[Run - fBackslashCount] = '\')) do
             fBackslashCount := fBackslashCount + 1;

           if (fBackslashCount mod 2 = 1) then inc(Run,2);
       end;
     end;// if LinePtr[Run]...
    if (LinePtr[Run]=EndChar) and (LinePtr[Run+1]=EndChar) and (LinePtr[Run+2]=EndChar) then begin
      inc(Run,3);
      fRange:=rsUnknown;
      EXIT;
    end;
    inc(Run);
  until (LinePtr[Run] in [#0, #10, #13]);
end;

procedure TSynPythonSyn.UnknownProc;
begin
  inc(Run);
  while (LinePtr[Run] in [#128..#191]) OR // continued utf8 subcode
   ((LinePtr[Run]<>#0) and (fProcTable[LinePtr[Run]] = @UnknownProc)) do inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynPythonSyn.Next;
begin
  fTokenPos := Run;

  case fRange of
    rsMultilineString:
      StringEndProc(#39);
    rsMultilineString2:
      StringEndProc('"');
    rsMultilineString3:
      StringEndProc(fStringStarter);
    else
      fProcTable[LinePtr[Run]];
  end;
end;

function TSynPythonSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
    SYN_ATTR_NUMBER: Result := fNumberAttri;
  else
    Result := nil;
  end;
end;

function TSynPythonSyn.GetEol: Boolean;
begin
  Result := fTokenId = tkNull;
end;

function TSynPythonSyn.GetRange: Pointer;
begin
  Result := Pointer(PtrInt(fRange));
end;

function TSynPythonSyn.GetToken: string;
var
  Len: LongInt;
begin
  Result := '';
  Len := Run - fTokenPos;
  SetString(Result, (LinePtr + fTokenPos), Len);
end;

function TSynPythonSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynPythonSyn.GetTokenAttribute: TLazEditTextAttribute;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNonKeyword: Result := fNonKeyAttri;
    tkSystemDefined: Result := fSystemAttri;
    tkNumber: Result := fNumberAttri;
    tkHex: Result := fHexAttri;
    tkOct: Result := fOctalAttri;
    tkFloat: Result := fFloatAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkTripleQuotedString: Result := fDocStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkUnknown: Result := fErrorAttri;
  else
    Result := nil;
  end;
end;

function TSynPythonSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynPythonSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

procedure TSynPythonSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynPythonSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(PtrUInt(Value));
end;

function TSynPythonSyn.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars;
end;

class function TSynPythonSyn.GetLanguageName: string;
begin
  Result := SYNS_LangPython;
end;

function TSynPythonSyn.GetSampleSource: string;
begin
  Result :=
    '#!/usr/local/bin/python'#13#10 +
    'import string, sys'#13#10 +
    '""" If no arguments were given, print a helpful message """'#13#10 +
    'if len(sys.argv)==1:'#13#10 +
    '    print ''Usage: celsius temp1 temp2 ...'''#13#10 +
    '    sys.exit(0)';
end;

initialization
  RegisterPlaceableHighlighter(TSynPythonSyn);

finalization
  GlobalKeywords.Free;
end.

