{
 /***************************************************************************
                                  lclproc.pas
                                  -----------
                             Component Library Code


 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Useful lower level helper functions and classes.
}
unit LCLProc;

{$mode objfpc}{$H+}
{$inline on}

interface

uses
  {$IFDEF Darwin}MacOSAll, {$ENDIF}
  Classes, SysUtils, Math, TypInfo, Types, FPCAdds, AvgLvlTree, FileUtil,
  LCLStrConsts, LCLType, WSReferences
  {$IFNDEF DisableCWString}{$ifdef unix}{$ifndef DisableIconv}, cwstring{$endif}{$endif}{$ENDIF}
  ;

type
  { TMethodList - array of TMethod }

  TMethodList = class
  private
    FItems: ^TMethod;
    FCount: integer;
    function GetItems(Index: integer): TMethod;
    procedure SetItems(Index: integer; const AValue: TMethod);
  public
    destructor Destroy; override;
    function Count: integer;
    function NextDownIndex(var Index: integer): boolean;
    function IndexOf(const AMethod: TMethod): integer;
    procedure Delete(Index: integer);
    procedure Remove(const AMethod: TMethod);
    procedure Add(const AMethod: TMethod);
    procedure Add(const AMethod: TMethod; AsLast: boolean);
    procedure Insert(Index: integer; const AMethod: TMethod);
    procedure Move(OldIndex, NewIndex: integer);
    procedure RemoveAllMethodsOfObject(const AnObject: TObject);
    procedure CallNotifyEvents(Sender: TObject);
  public
    property Items[Index: integer]: TMethod read GetItems write SetItems; default;
  end;

type
  TStackTracePointers = array of Pointer;

  { TDebugLCLItemInfo }

  TDebugLCLItemInfo = class
  public
    Item: Pointer;
    IsDestroyed: boolean;
    Info: string;
    CreationStack: TStackTracePointers; // stack trace at creationg
    DestructionStack: TStackTracePointers;// stack trace at destruction
    function AsString(WithStackTraces: boolean): string;
    destructor Destroy; override;
  end;

  { TDebugLCLItems }

  TDebugLCLItems = class
  private
    FItems: TAvgLvlTree;// tree of TDebugLCLItemInfo
    FName: string;
  public
    constructor Create(const TheName: string);
    destructor Destroy; override;
    function FindInfo(p: Pointer; CreateIfNotExists: boolean = false
                      ): TDebugLCLItemInfo;
    function IsDestroyed(p: Pointer): boolean;
    function IsCreated(p: Pointer): boolean;
    function MarkCreated(p: Pointer; const InfoText: string): TDebugLCLItemInfo;
    procedure MarkDestroyed(p: Pointer);
    function GetInfo(p: Pointer; WithStackTraces: boolean): string;
    property Name: string read FName;
  end;

  TLineInfoCacheItem = record
    Addr: Pointer;
    Info: string;
  end;
  PLineInfoCacheItem = ^TLineInfoCacheItem;

{$IFDEF DebugLCLComponents}
var
  DebugLCLComponents: TDebugLCLItems = nil;
{$ENDIF}

function CompareDebugLCLItemInfos(Data1, Data2: Pointer): integer;
function CompareItemWithDebugLCLItemInfo(Item, DebugItemInfo: Pointer): integer;

function CompareLineInfoCacheItems(Data1, Data2: Pointer): integer;
function CompareAddrWithLineInfoCacheItem(Addr, Item: Pointer): integer;


type
  TStringsSortCompare = function(const Item1, Item2: string): Integer;


procedure MergeSort(List: TFPList; const OnCompare: TListSortCompare);
procedure MergeSort(List: TStrings; const OnCompare: TStringsSortCompare);

function GetEnumValueDef(TypeInfo: PTypeInfo; const Name: string;
                         const DefaultValue: Integer): Integer;

function ShortCutToText(ShortCut: TShortCut): string;// untranslated
function TextToShortCut(const ShortCutText: string): TShortCut;// untranslated

function GetCompleteText(sText: string; iSelStart: Integer;
  bCaseSensitive, bSearchAscending: Boolean; slTextList: TStrings): string;
function IsEditableTextKey(Key: Word): Boolean;

// Hooks used to prevent unit circles
type
  TSendApplicationMessageFunction =
    function(Msg: Cardinal; WParam: WParam; LParam: LParam):Longint;
  TOwnerFormDesignerModifiedProc =
    procedure(AComponent: TComponent);


var
  SendApplicationMessageFunction: TSendApplicationMessageFunction=nil;
  OwnerFormDesignerModifiedProc: TOwnerFormDesignerModifiedProc=nil;

function SendApplicationMessage(Msg: Cardinal; WParam: WParam; LParam: LParam):Longint;
procedure OwnerFormDesignerModified(AComponent: TComponent);
procedure FreeThenNil(var obj);

{ the LCL interfaces finalization sections are called before the finalization
  sections of the LCL. Those parts, that should be finalized after the LCL, can
  be registered here. }
procedure RegisterInterfaceInitializationHandler(p: TProcedure);
procedure CallInterfaceInitializationHandlers;
procedure RegisterInterfaceFinalizationHandler(p: TProcedure);
procedure CallInterfaceFinalizationHandlers;

function OffsetRect(var ARect: TRect; dx, dy: Integer): Boolean;
procedure MoveRect(var ARect: TRect; x, y: Integer);
procedure MoveRectToFit(var ARect: TRect; const MaxRect: TRect);
procedure MakeMinMax(var i1, i2: integer);
procedure CalculateLeftTopWidthHeight(X1,Y1,X2,Y2: integer;
  var Left,Top,Width,Height: integer);

function DeleteAmpersands(var Str : String) : Longint;
function BreakString(const s: string; MaxLineLength, Indent: integer): string;

function ComparePointers(p1, p2: Pointer): integer;
function CompareHandles(h1, h2: THandle): integer;
function CompareLCLHandles(h1, h2: TLCLHandle): integer;
function CompareRect(R1, R2: PRect): Boolean;
function ComparePoints(const p1, p2: TPoint): integer;
function CompareMethods(const m1, m2: TMethod): boolean;

function RoundToInt(const e: Extended): integer;
function RoundToCardinal(const e: Extended): cardinal;
function TruncToInt(const e: Extended): integer;
function TruncToCardinal(const e: Extended): cardinal;
function StrToDouble(const s: string): double;



// debugging
procedure RaiseGDBException(const Msg: string);
procedure RaiseAndCatchException;
procedure DumpExceptionBackTrace;
procedure DumpStack;
function GetStackTrace(UseCache: boolean): string;
procedure GetStackTracePointers(var AStack: TStackTracePointers);
function StackTraceAsString(const AStack: TStackTracePointers;
                            UseCache: boolean): string;
function GetLineInfo(Addr: Pointer; UseCache: boolean): string;

procedure DebugLn(Args: array of const);
procedure DebugLn(const S: String; Args: array of const);// similar to Format(s,Args)
procedure DebugLn;
procedure DebugLn(const s: string);
procedure DebugLn(const s1,s2: string);
procedure DebugLn(const s1,s2,s3: string);
procedure DebugLn(const s1,s2,s3,s4: string);
procedure DebugLn(const s1,s2,s3,s4,s5: string);
procedure DebugLn(const s1,s2,s3,s4,s5,s6: string);
procedure DebugLn(const s1,s2,s3,s4,s5,s6,s7: string);
procedure DebugLn(const s1,s2,s3,s4,s5,s6,s7,s8: string);
procedure DebugLn(const s1,s2,s3,s4,s5,s6,s7,s8,s9: string);
procedure DebugLn(const s1,s2,s3,s4,s5,s6,s7,s8,s9,s10: string);
procedure DebugLn(const s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11: string);
procedure DebugLn(const s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12: string);
procedure DebugLn(const s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13: string);
procedure DebugLn(const s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14: string);
procedure DebugLn(const s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15: string);
procedure DebugLn(const s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,s16: string);

procedure DebugLnEnter(const s: string = nil);
procedure DebugLnEnter(Args: array of const);
procedure DebugLnEnter(s: string; Args: array of const);
procedure DebugLnEnter(const s1, s2: string; const s3: string = nil;
                     const s4: string = nil; const s5: string = nil; const s6: string = nil;
                     const s7: string = nil; const s8: string = nil; const s9: string = nil;
                     const s10: string = nil; const s11: string = nil; const s12: string = nil;
                     const s13: string = nil; const s14: string = nil; const s15: string = nil;
                     const s16: string = nil; const s17: string = nil; const s18: string = nil);
procedure DebugLnExit(const s: string = nil);
procedure DebugLnExit(Args: array of const);
procedure DebugLnExit(s: string; Args: array of const);
procedure DebugLnExit (const s1, s2: string; const s3: string = nil;
                     const s4: string = nil; const s5: string = nil; const s6: string = nil;
                     const s7: string = nil; const s8: string = nil; const s9: string = nil;
                     const s10: string = nil; const s11: string = nil; const s12: string = nil;
                     const s13: string = nil; const s14: string = nil; const s15: string = nil;
                     const s16: string = nil; const s17: string = nil; const s18: string = nil);

function ConvertLineEndings(const s: string): string;

procedure DbgOut(const S: String; Args: array of const);
procedure DbgOut(const s: string);
procedure DbgOut(const s1,s2: string);
procedure DbgOut(const s1,s2,s3: string);
procedure DbgOut(const s1,s2,s3,s4: string);
procedure DbgOut(const s1,s2,s3,s4,s5: string);
procedure DbgOut(const s1,s2,s3,s4,s5,s6: string);
procedure DbgOut(const s1,s2,s3,s4,s5,s6,s7: string);
procedure DbgOut(const s1,s2,s3,s4,s5,s6,s7,s8: string);
procedure DbgOut(const s1,s2,s3,s4,s5,s6,s7,s8,s9: string);
procedure DbgOut(const s1,s2,s3,s4,s5,s6,s7,s8,s9,s10: string);
procedure DbgOut(const s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11: string);
procedure DbgOut(const s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12: string);

function DbgS(const c: cardinal): string; overload;
function DbgS(const i: longint): string; overload;
function DbgS(const i: int64): string; overload;
function DbgS(const q: qword): string; overload;
function DbgS(const r: TRect): string; overload;
function DbgS(const p: TPoint): string; overload;
function DbgS(const p: pointer): string; overload;
function DbgS(const e: extended; MaxDecimals: integer = 999): string; overload;
function DbgS(const b: boolean): string; overload;
function DbgS(const s: TComponentState): string; overload;
function DbgS(const m: TMethod): string; overload;
function DbgSName(const p: TObject): string; overload;
function DbgSName(const p: TClass): string; overload;
function DbgStr(const StringWithSpecialChars: string): string; overload;
function DbgWideStr(const StringWithSpecialChars: widestring): string; overload;
function dbgMemRange(P: PByte; Count: integer; Width: integer = 0): string; overload;
function dbgMemStream(MemStream: TCustomMemoryStream; Count: integer): string; overload;
function dbgObjMem(AnObject: TObject): string; overload;
function dbghex(i: Int64): string; overload;
function DbgSWindowPosFlags(Flags: UInt): String;

function DbgS(const i1,i2,i3,i4: integer): string; overload;
function DbgS(const Shift: TShiftState): string; overload;
function DbgsVKCode(c: word): string;

function DbgS(const ASize: TSize): string; overload;
function DbgS(const ATM: TTextMetric): string; overload;
function DbgS(const AScrollInfo: TScrollInfo): string; overload;

procedure DbgOutThreadLog(const Msg: string); overload;
procedure DebuglnThreadLog(const Msg: string); overload;
procedure DebuglnThreadLog(Args: array of const); overload;
procedure DebuglnThreadLog; overload;
procedure DbgSaveData(FileName: String; AData: PChar; ADataSize: PtrUInt);
procedure DbgAppendToFile(FileName, S: String);
procedure DbgAppendToFileWithoutLn(FileName, S: String);

procedure CloseDebugOutput;

// some string manipulation functions
function StripLN(const ALine: String): String;
function GetPart(const ASkipTo, AnEnd: String; var ASource: String;
  const AnIgnoreCase: Boolean = False; const AnUpdateSource: Boolean = True): String;
function GetPart(const ASkipTo, AnEnd: array of String; var ASource: String;
  const AnIgnoreCase: Boolean = False; const AnUpdateSource: Boolean = True): String;
function TextToSingleLine(const AText: string): string;

// case..of utility functions
function StringCase(const AString: String; const ACase: array of String {; const AIgnoreCase = False, APartial = false: Boolean}): Integer; overload;
function StringCase(const AString: String; const ACase: array of String; const AIgnoreCase, APartial: Boolean): Integer; overload;
function ClassCase(const AClass: TClass; const ACase: array of TClass {; const ADescendant: Boolean = True}): Integer; overload;
function ClassCase(const AClass: TClass; const ACase: array of TClass; const ADecendant: Boolean): Integer; overload;


// UTF utility functions
// MG: Should be moved to the RTL  

// MWE: define (missing) UTF16string similar to UTF8 
//      strictly spoken, a widestring <> utf16string
// todo: use it in existing functions
type
  UTF16String = type WideString;
  PUTF16String = ^UTF16String;

function UTF8CharacterLength(p: PChar): integer;
function UTF8Length(const s: string): PtrInt;
function UTF8Length(p: PChar; ByteCount: PtrInt): PtrInt;
function UTF8CharacterToUnicode(p: PChar; out CharLen: integer): Cardinal;
function UnicodeToUTF8(u: cardinal; Buf: PChar): integer; inline;
function UnicodeToUTF8SkipErrors(u: cardinal; Buf: PChar): integer;
function UnicodeToUTF8(u: cardinal): shortstring; inline;
function UTF8ToDoubleByteString(const s: string): string;
function UTF8ToDoubleByte(UTF8Str: PChar; Len: PtrInt; DBStr: PByte): PtrInt;
function UTF8FindNearestCharStart(UTF8Str: PChar; Len: integer;
                                  BytePos: integer): integer;
// find the n-th UTF8 character, ignoring BIDI
function UTF8CharStart(UTF8Str: PChar; Len, CharIndex: PtrInt): PChar;
// find the byte index of the n-th UTF8 character, ignoring BIDI (byte len of substr)
function UTF8CharToByteIndex(UTF8Str: PChar; Len, CharIndex: PtrInt): PtrInt;
procedure UTF8FixBroken(P: PChar);
function UTF8CharacterStrictLength(P: PChar): integer;
function UTF8CStringToUTF8String(SourceStart: PChar; SourceLen: PtrInt) : string;
function UTF8Pos(const SearchForText, SearchInText: string): PtrInt;
function UTF8Copy(const s: string; StartCharIndex, CharCount: PtrInt): string;
procedure UTF8Delete(var s: String; StartCharIndex, CharCount: PtrInt);
procedure UTF8Insert(const source: String; var s: string; StartCharIndex: PtrInt);
function UTF8LowerCase(const s: String): String;
function UTF8UpperCase(const s: String): String;
{$ifdef NewLowerCase}
function UTF8LowerCaseNew(const s: String): String;
{$endif}
function FindInvalidUTF8Character(p: PChar; Count: PtrInt;
                                  StopOnNonASCII: Boolean = false): PtrInt;
function ValidUTF8String(const s: String): String;

procedure AssignUTF8ListToAnsi(UTF8List, AnsiList: TStrings);

function UTF16CharacterLength(p: PWideChar): integer;
function UTF16Length(const s: widestring): PtrInt;
function UTF16Length(p: PWideChar; WordCount: PtrInt): PtrInt;
function UTF16CharacterToUnicode(p: PWideChar; out CharLen: integer): Cardinal;
function UnicodeToUTF16(u: cardinal): widestring;

type
  TConvertResult = (trNoError, trNullSrc, trNullDest, trDestExhausted,
    trInvalidChar, trUnfinishedChar);

  TConvertOption = (toInvalidCharError, toInvalidCharToSymbol,
    toUnfinishedCharError, toUnfinishedCharToSymbol);
  TConvertOptions = set of TConvertOption;

function ConvertUTF8ToUTF16(Dest: PWideChar; DestWideCharCount: SizeUInt;
  Src: PChar; SrcCharCount: SizeUInt; Options: TConvertOptions;
  out ActualWideCharCount: SizeUInt): TConvertResult;

function ConvertUTF16ToUTF8(Dest: PChar; DestCharCount: SizeUInt;
  Src: PWideChar; SrcWideCharCount: SizeUInt; Options: TConvertOptions;
  out ActualCharCount: SizeUInt): TConvertResult;

function UTF8ToUTF16(const S: AnsiString): UTF16String;
function UTF16ToUTF8(const S: UTF16String): AnsiString;

// locale
procedure LCLGetLanguageIDs(var Lang, FallbackLang: String);

// identifier
function CreateFirstIdentifier(const Identifier: string): string;
function CreateNextIdentifier(const Identifier: string): string;

var
  DebugLnMaxNestPrefixLen: Integer = 15;
  DebugLnNestLvlIndent: Integer = 2;

implementation

uses gettext;

const
  Str_LCL_Debug_File = 'lcldebug.log';

var
  InterfaceInitializationHandlers: TFPList = nil;
  InterfaceFinalizationHandlers: TFPList = nil;
  DebugTextAllocated: boolean;
  DebugText: ^Text;
  DebugNestLvl: Integer = 0;
  DebugNestPrefix: PChar = nil;
  DebugNestAtBOL: Boolean;
  LineInfoCache: TAvgLvlTree = nil;

{$ifdef NewLowerCase}
var
  UnicodeLower00C0_00DE: array[$00C0..$00DE] of word;
  UnicodeLower0100_024E: array[$0100..$024E] of word;
  UnicodeLower0386_03AB: array[$0386..$03AB] of word;
  UnicodeLower03D8_042F: array[$03D8..$042F] of word;
  UnicodeLower0460_0512: array[$0460..$0512] of word;
  UnicodeLower1E00_1FFC: array[$1E00..$1FFC] of word;
  UnicodeLower2126_2183: array[$2126..$2183] of word;
  UnicodeLower2C60_2CE2: array[$2C60..$2CE2] of word;

procedure InitUnicodeTables;
var
  i: Integer;
begin
  for i:=Low(UnicodeLower00C0_00DE) to High(UnicodeLower00C0_00DE) do
    UnicodeLower00C0_00DE[i]:=i+32;
  UnicodeLower00C0_00DE[$00D7]:=$00D7;

  for i:=Low(UnicodeLower0100_024E) to High(UnicodeLower0100_024E) do
    UnicodeLower0100_024E[i]:=i;
  UnicodeLower0100_024E[$0100]:=$0101;
  UnicodeLower0100_024E[$0102]:=$0103;
  UnicodeLower0100_024E[$0104]:=$0105;
  UnicodeLower0100_024E[$0106]:=$0107;
  UnicodeLower0100_024E[$0108]:=$0109;
  UnicodeLower0100_024E[$010A]:=$010B;
  UnicodeLower0100_024E[$010C]:=$010D;
  UnicodeLower0100_024E[$010E]:=$010F;
  UnicodeLower0100_024E[$0110]:=$0111;
  UnicodeLower0100_024E[$0112]:=$0113;
  UnicodeLower0100_024E[$0114]:=$0115;
  UnicodeLower0100_024E[$0116]:=$0117;
  UnicodeLower0100_024E[$0118]:=$0119;
  UnicodeLower0100_024E[$011A]:=$011B;
  UnicodeLower0100_024E[$011C]:=$011D;
  UnicodeLower0100_024E[$011E]:=$011F;
  UnicodeLower0100_024E[$0120]:=$0121;
  UnicodeLower0100_024E[$0122]:=$0123;
  UnicodeLower0100_024E[$0124]:=$0125;
  UnicodeLower0100_024E[$0126]:=$0127;
  UnicodeLower0100_024E[$0128]:=$0129;
  UnicodeLower0100_024E[$012A]:=$012B;
  UnicodeLower0100_024E[$012C]:=$012D;
  UnicodeLower0100_024E[$012E]:=$012F;
  UnicodeLower0100_024E[$0130]:=$0069;
  UnicodeLower0100_024E[$0132]:=$0133;
  UnicodeLower0100_024E[$0134]:=$0135;
  UnicodeLower0100_024E[$0136]:=$0137;
  UnicodeLower0100_024E[$0139]:=$013A;
  UnicodeLower0100_024E[$013B]:=$013C;
  UnicodeLower0100_024E[$013D]:=$013E;
  UnicodeLower0100_024E[$013F]:=$0140;
  UnicodeLower0100_024E[$0141]:=$0142;
  UnicodeLower0100_024E[$0143]:=$0144;
  UnicodeLower0100_024E[$0145]:=$0146;
  UnicodeLower0100_024E[$0147]:=$0148;
  UnicodeLower0100_024E[$014A]:=$014B;
  UnicodeLower0100_024E[$014C]:=$014D;
  UnicodeLower0100_024E[$014E]:=$014F;
  UnicodeLower0100_024E[$0150]:=$0151;
  UnicodeLower0100_024E[$0152]:=$0153;
  UnicodeLower0100_024E[$0154]:=$0155;
  UnicodeLower0100_024E[$0156]:=$0157;
  UnicodeLower0100_024E[$0158]:=$0159;
  UnicodeLower0100_024E[$015A]:=$015B;
  UnicodeLower0100_024E[$015C]:=$015D;
  UnicodeLower0100_024E[$015E]:=$015F;
  UnicodeLower0100_024E[$0160]:=$0161;
  UnicodeLower0100_024E[$0162]:=$0163;
  UnicodeLower0100_024E[$0164]:=$0165;
  UnicodeLower0100_024E[$0166]:=$0167;
  UnicodeLower0100_024E[$0168]:=$0169;
  UnicodeLower0100_024E[$016A]:=$016B;
  UnicodeLower0100_024E[$016C]:=$016D;
  UnicodeLower0100_024E[$016E]:=$016F;
  UnicodeLower0100_024E[$0170]:=$0171;
  UnicodeLower0100_024E[$0172]:=$0173;
  UnicodeLower0100_024E[$0174]:=$0175;
  UnicodeLower0100_024E[$0176]:=$0177;
  UnicodeLower0100_024E[$0178]:=$00FF;
  UnicodeLower0100_024E[$0179]:=$017A;
  UnicodeLower0100_024E[$017B]:=$017C;
  UnicodeLower0100_024E[$017D]:=$017E;
  UnicodeLower0100_024E[$0181]:=$0253;
  UnicodeLower0100_024E[$0182]:=$0183;
  UnicodeLower0100_024E[$0184]:=$0185;
  UnicodeLower0100_024E[$0186]:=$0254;
  UnicodeLower0100_024E[$0187]:=$0188;
  UnicodeLower0100_024E[$0189]:=$0256;
  UnicodeLower0100_024E[$018A]:=$0257;
  UnicodeLower0100_024E[$018B]:=$018C;
  UnicodeLower0100_024E[$018E]:=$01DD;
  UnicodeLower0100_024E[$018F]:=$0259;
  UnicodeLower0100_024E[$0190]:=$025B;
  UnicodeLower0100_024E[$0191]:=$0192;
  UnicodeLower0100_024E[$0193]:=$0260;
  UnicodeLower0100_024E[$0194]:=$0263;
  UnicodeLower0100_024E[$0196]:=$0269;
  UnicodeLower0100_024E[$0197]:=$0268;
  UnicodeLower0100_024E[$0198]:=$0199;
  UnicodeLower0100_024E[$019C]:=$026F;
  UnicodeLower0100_024E[$019D]:=$0272;
  UnicodeLower0100_024E[$019F]:=$0275;
  UnicodeLower0100_024E[$01A0]:=$01A1;
  UnicodeLower0100_024E[$01A2]:=$01A3;
  UnicodeLower0100_024E[$01A4]:=$01A5;
  UnicodeLower0100_024E[$01A6]:=$0280;
  UnicodeLower0100_024E[$01A7]:=$01A8;
  UnicodeLower0100_024E[$01A9]:=$0283;
  UnicodeLower0100_024E[$01AC]:=$01AD;
  UnicodeLower0100_024E[$01AE]:=$0288;
  UnicodeLower0100_024E[$01AF]:=$01B0;
  UnicodeLower0100_024E[$01B1]:=$028A;
  UnicodeLower0100_024E[$01B2]:=$028B;
  UnicodeLower0100_024E[$01B3]:=$01B4;
  UnicodeLower0100_024E[$01B5]:=$01B6;
  UnicodeLower0100_024E[$01B7]:=$0292;
  UnicodeLower0100_024E[$01B8]:=$01B9;
  UnicodeLower0100_024E[$01BC]:=$01BD;
  UnicodeLower0100_024E[$01C4]:=$01C6;
  UnicodeLower0100_024E[$01C5]:=$01C6;
  UnicodeLower0100_024E[$01C7]:=$01C9;
  UnicodeLower0100_024E[$01C8]:=$01C9;
  UnicodeLower0100_024E[$01CA]:=$01CC;
  UnicodeLower0100_024E[$01CB]:=$01CC;
  UnicodeLower0100_024E[$01CD]:=$01CE;
  UnicodeLower0100_024E[$01CF]:=$01D0;
  UnicodeLower0100_024E[$01D1]:=$01D2;
  UnicodeLower0100_024E[$01D3]:=$01D4;
  UnicodeLower0100_024E[$01D5]:=$01D6;
  UnicodeLower0100_024E[$01D7]:=$01D8;
  UnicodeLower0100_024E[$01D9]:=$01DA;
  UnicodeLower0100_024E[$01DB]:=$01DC;
  UnicodeLower0100_024E[$01DE]:=$01DF;
  UnicodeLower0100_024E[$01E0]:=$01E1;
  UnicodeLower0100_024E[$01E2]:=$01E3;
  UnicodeLower0100_024E[$01E4]:=$01E5;
  UnicodeLower0100_024E[$01E6]:=$01E7;
  UnicodeLower0100_024E[$01E8]:=$01E9;
  UnicodeLower0100_024E[$01EA]:=$01EB;
  UnicodeLower0100_024E[$01EC]:=$01ED;
  UnicodeLower0100_024E[$01EE]:=$01EF;
  UnicodeLower0100_024E[$01F1]:=$01F3;
  UnicodeLower0100_024E[$01F2]:=$01F3;
  UnicodeLower0100_024E[$01F4]:=$01F5;
  UnicodeLower0100_024E[$01F6]:=$0195;
  UnicodeLower0100_024E[$01F7]:=$01BF;
  UnicodeLower0100_024E[$01F8]:=$01F9;
  UnicodeLower0100_024E[$01FA]:=$01FB;
  UnicodeLower0100_024E[$01FC]:=$01FD;
  UnicodeLower0100_024E[$01FE]:=$01FF;
  UnicodeLower0100_024E[$0200]:=$0201;
  UnicodeLower0100_024E[$0202]:=$0203;
  UnicodeLower0100_024E[$0204]:=$0205;
  UnicodeLower0100_024E[$0206]:=$0207;
  UnicodeLower0100_024E[$0208]:=$0209;
  UnicodeLower0100_024E[$020A]:=$020B;
  UnicodeLower0100_024E[$020C]:=$020D;
  UnicodeLower0100_024E[$020E]:=$020F;
  UnicodeLower0100_024E[$0210]:=$0211;
  UnicodeLower0100_024E[$0212]:=$0213;
  UnicodeLower0100_024E[$0214]:=$0215;
  UnicodeLower0100_024E[$0216]:=$0217;
  UnicodeLower0100_024E[$0218]:=$0219;
  UnicodeLower0100_024E[$021A]:=$021B;
  UnicodeLower0100_024E[$021C]:=$021D;
  UnicodeLower0100_024E[$021E]:=$021F;
  UnicodeLower0100_024E[$0220]:=$019E;
  UnicodeLower0100_024E[$0222]:=$0223;
  UnicodeLower0100_024E[$0224]:=$0225;
  UnicodeLower0100_024E[$0226]:=$0227;
  UnicodeLower0100_024E[$0228]:=$0229;
  UnicodeLower0100_024E[$022A]:=$022B;
  UnicodeLower0100_024E[$022C]:=$022D;
  UnicodeLower0100_024E[$022E]:=$022F;
  UnicodeLower0100_024E[$0230]:=$0231;
  UnicodeLower0100_024E[$0232]:=$0233;
  UnicodeLower0100_024E[$023A]:=$2C65;
  UnicodeLower0100_024E[$023B]:=$023C;
  UnicodeLower0100_024E[$023D]:=$019A;
  UnicodeLower0100_024E[$023E]:=$2C66;
  UnicodeLower0100_024E[$0241]:=$0242;
  UnicodeLower0100_024E[$0243]:=$0180;
  UnicodeLower0100_024E[$0244]:=$0289;
  UnicodeLower0100_024E[$0245]:=$028C;
  UnicodeLower0100_024E[$0246]:=$0247;
  UnicodeLower0100_024E[$0248]:=$0249;
  UnicodeLower0100_024E[$024A]:=$024B;
  UnicodeLower0100_024E[$024C]:=$024D;
  UnicodeLower0100_024E[$024E]:=$024F;

  for i:=Low(UnicodeLower0386_03AB) to High(UnicodeLower0386_03AB) do
    UnicodeLower0386_03AB[i]:=i;
  UnicodeLower0386_03AB[$0386]:=$03AC;
  UnicodeLower0386_03AB[$0388]:=$03AD;
  UnicodeLower0386_03AB[$0389]:=$03AE;
  UnicodeLower0386_03AB[$038A]:=$03AF;
  UnicodeLower0386_03AB[$038C]:=$03CC;
  UnicodeLower0386_03AB[$038E]:=$03CD;
  UnicodeLower0386_03AB[$038F]:=$03CE;
  UnicodeLower0386_03AB[$0391]:=$03B1;
  UnicodeLower0386_03AB[$0392]:=$03B2;
  UnicodeLower0386_03AB[$0393]:=$03B3;
  UnicodeLower0386_03AB[$0394]:=$03B4;
  UnicodeLower0386_03AB[$0395]:=$03B5;
  UnicodeLower0386_03AB[$0396]:=$03B6;
  UnicodeLower0386_03AB[$0397]:=$03B7;
  UnicodeLower0386_03AB[$0398]:=$03B8;
  UnicodeLower0386_03AB[$0399]:=$03B9;
  UnicodeLower0386_03AB[$039A]:=$03BA;
  UnicodeLower0386_03AB[$039B]:=$03BB;
  UnicodeLower0386_03AB[$039C]:=$03BC;
  UnicodeLower0386_03AB[$039D]:=$03BD;
  UnicodeLower0386_03AB[$039E]:=$03BE;
  UnicodeLower0386_03AB[$039F]:=$03BF;
  UnicodeLower0386_03AB[$03A0]:=$03C0;
  UnicodeLower0386_03AB[$03A1]:=$03C1;
  UnicodeLower0386_03AB[$03A3]:=$03C3;
  UnicodeLower0386_03AB[$03A4]:=$03C4;
  UnicodeLower0386_03AB[$03A5]:=$03C5;
  UnicodeLower0386_03AB[$03A6]:=$03C6;
  UnicodeLower0386_03AB[$03A7]:=$03C7;
  UnicodeLower0386_03AB[$03A8]:=$03C8;
  UnicodeLower0386_03AB[$03A9]:=$03C9;
  UnicodeLower0386_03AB[$03AA]:=$03CA;
  UnicodeLower0386_03AB[$03AB]:=$03CB;

  for i:=Low(UnicodeLower03D8_042F) to High(UnicodeLower03D8_042F) do
    UnicodeLower03D8_042F[i]:=i;
  UnicodeLower03D8_042F[$03D8]:=$03D9;
  UnicodeLower03D8_042F[$03DA]:=$03DB;
  UnicodeLower03D8_042F[$03DC]:=$03DD;
  UnicodeLower03D8_042F[$03DE]:=$03DF;
  UnicodeLower03D8_042F[$03E0]:=$03E1;
  UnicodeLower03D8_042F[$03E2]:=$03E3;
  UnicodeLower03D8_042F[$03E4]:=$03E5;
  UnicodeLower03D8_042F[$03E6]:=$03E7;
  UnicodeLower03D8_042F[$03E8]:=$03E9;
  UnicodeLower03D8_042F[$03EA]:=$03EB;
  UnicodeLower03D8_042F[$03EC]:=$03ED;
  UnicodeLower03D8_042F[$03EE]:=$03EF;
  UnicodeLower03D8_042F[$03F4]:=$03B8;
  UnicodeLower03D8_042F[$03F7]:=$03F8;
  UnicodeLower03D8_042F[$03F9]:=$03F2;
  UnicodeLower03D8_042F[$03FA]:=$03FB;
  UnicodeLower03D8_042F[$03FD]:=$037B;
  UnicodeLower03D8_042F[$03FE]:=$037C;
  UnicodeLower03D8_042F[$03FF]:=$037D;
  UnicodeLower03D8_042F[$0400]:=$0450;
  UnicodeLower03D8_042F[$0401]:=$0451;
  UnicodeLower03D8_042F[$0402]:=$0452;
  UnicodeLower03D8_042F[$0403]:=$0453;
  UnicodeLower03D8_042F[$0404]:=$0454;
  UnicodeLower03D8_042F[$0405]:=$0455;
  UnicodeLower03D8_042F[$0406]:=$0456;
  UnicodeLower03D8_042F[$0407]:=$0457;
  UnicodeLower03D8_042F[$0408]:=$0458;
  UnicodeLower03D8_042F[$0409]:=$0459;
  UnicodeLower03D8_042F[$040A]:=$045A;
  UnicodeLower03D8_042F[$040B]:=$045B;
  UnicodeLower03D8_042F[$040C]:=$045C;
  UnicodeLower03D8_042F[$040D]:=$045D;
  UnicodeLower03D8_042F[$040E]:=$045E;
  UnicodeLower03D8_042F[$040F]:=$045F;
  UnicodeLower03D8_042F[$0410]:=$0430;
  UnicodeLower03D8_042F[$0411]:=$0431;
  UnicodeLower03D8_042F[$0412]:=$0432;
  UnicodeLower03D8_042F[$0413]:=$0433;
  UnicodeLower03D8_042F[$0414]:=$0434;
  UnicodeLower03D8_042F[$0415]:=$0435;
  UnicodeLower03D8_042F[$0416]:=$0436;
  UnicodeLower03D8_042F[$0417]:=$0437;
  UnicodeLower03D8_042F[$0418]:=$0438;
  UnicodeLower03D8_042F[$0419]:=$0439;
  UnicodeLower03D8_042F[$041A]:=$043A;
  UnicodeLower03D8_042F[$041B]:=$043B;
  UnicodeLower03D8_042F[$041C]:=$043C;
  UnicodeLower03D8_042F[$041D]:=$043D;
  UnicodeLower03D8_042F[$041E]:=$043E;
  UnicodeLower03D8_042F[$041F]:=$043F;
  UnicodeLower03D8_042F[$0420]:=$0440;
  UnicodeLower03D8_042F[$0421]:=$0441;
  UnicodeLower03D8_042F[$0422]:=$0442;
  UnicodeLower03D8_042F[$0423]:=$0443;
  UnicodeLower03D8_042F[$0424]:=$0444;
  UnicodeLower03D8_042F[$0425]:=$0445;
  UnicodeLower03D8_042F[$0426]:=$0446;
  UnicodeLower03D8_042F[$0427]:=$0447;
  UnicodeLower03D8_042F[$0428]:=$0448;
  UnicodeLower03D8_042F[$0429]:=$0449;
  UnicodeLower03D8_042F[$042A]:=$044A;
  UnicodeLower03D8_042F[$042B]:=$044B;
  UnicodeLower03D8_042F[$042C]:=$044C;
  UnicodeLower03D8_042F[$042D]:=$044D;
  UnicodeLower03D8_042F[$042E]:=$044E;
  UnicodeLower03D8_042F[$042F]:=$044F;

  for i:=Low(UnicodeLower0460_0512) to High(UnicodeLower0460_0512) do
    UnicodeLower0460_0512[i]:=i;
  UnicodeLower0460_0512[$0460]:=$0461;
  UnicodeLower0460_0512[$0462]:=$0463;
  UnicodeLower0460_0512[$0464]:=$0465;
  UnicodeLower0460_0512[$0466]:=$0467;
  UnicodeLower0460_0512[$0468]:=$0469;
  UnicodeLower0460_0512[$046A]:=$046B;
  UnicodeLower0460_0512[$046C]:=$046D;
  UnicodeLower0460_0512[$046E]:=$046F;
  UnicodeLower0460_0512[$0470]:=$0471;
  UnicodeLower0460_0512[$0472]:=$0473;
  UnicodeLower0460_0512[$0474]:=$0475;
  UnicodeLower0460_0512[$0476]:=$0477;
  UnicodeLower0460_0512[$0478]:=$0479;
  UnicodeLower0460_0512[$047A]:=$047B;
  UnicodeLower0460_0512[$047C]:=$047D;
  UnicodeLower0460_0512[$047E]:=$047F;
  UnicodeLower0460_0512[$0480]:=$0481;
  UnicodeLower0460_0512[$048A]:=$048B;
  UnicodeLower0460_0512[$048C]:=$048D;
  UnicodeLower0460_0512[$048E]:=$048F;
  UnicodeLower0460_0512[$0490]:=$0491;
  UnicodeLower0460_0512[$0492]:=$0493;
  UnicodeLower0460_0512[$0494]:=$0495;
  UnicodeLower0460_0512[$0496]:=$0497;
  UnicodeLower0460_0512[$0498]:=$0499;
  UnicodeLower0460_0512[$049A]:=$049B;
  UnicodeLower0460_0512[$049C]:=$049D;
  UnicodeLower0460_0512[$049E]:=$049F;
  UnicodeLower0460_0512[$04A0]:=$04A1;
  UnicodeLower0460_0512[$04A2]:=$04A3;
  UnicodeLower0460_0512[$04A4]:=$04A5;
  UnicodeLower0460_0512[$04A6]:=$04A7;
  UnicodeLower0460_0512[$04A8]:=$04A9;
  UnicodeLower0460_0512[$04AA]:=$04AB;
  UnicodeLower0460_0512[$04AC]:=$04AD;
  UnicodeLower0460_0512[$04AE]:=$04AF;
  UnicodeLower0460_0512[$04B0]:=$04B1;
  UnicodeLower0460_0512[$04B2]:=$04B3;
  UnicodeLower0460_0512[$04B4]:=$04B5;
  UnicodeLower0460_0512[$04B6]:=$04B7;
  UnicodeLower0460_0512[$04B8]:=$04B9;
  UnicodeLower0460_0512[$04BA]:=$04BB;
  UnicodeLower0460_0512[$04BC]:=$04BD;
  UnicodeLower0460_0512[$04BE]:=$04BF;
  UnicodeLower0460_0512[$04C0]:=$04CF;
  UnicodeLower0460_0512[$04C1]:=$04C2;
  UnicodeLower0460_0512[$04C3]:=$04C4;
  UnicodeLower0460_0512[$04C5]:=$04C6;
  UnicodeLower0460_0512[$04C7]:=$04C8;
  UnicodeLower0460_0512[$04C9]:=$04CA;
  UnicodeLower0460_0512[$04CB]:=$04CC;
  UnicodeLower0460_0512[$04CD]:=$04CE;
  UnicodeLower0460_0512[$04D0]:=$04D1;
  UnicodeLower0460_0512[$04D2]:=$04D3;
  UnicodeLower0460_0512[$04D4]:=$04D5;
  UnicodeLower0460_0512[$04D6]:=$04D7;
  UnicodeLower0460_0512[$04D8]:=$04D9;
  UnicodeLower0460_0512[$04DA]:=$04DB;
  UnicodeLower0460_0512[$04DC]:=$04DD;
  UnicodeLower0460_0512[$04DE]:=$04DF;
  UnicodeLower0460_0512[$04E0]:=$04E1;
  UnicodeLower0460_0512[$04E2]:=$04E3;
  UnicodeLower0460_0512[$04E4]:=$04E5;
  UnicodeLower0460_0512[$04E6]:=$04E7;
  UnicodeLower0460_0512[$04E8]:=$04E9;
  UnicodeLower0460_0512[$04EA]:=$04EB;
  UnicodeLower0460_0512[$04EC]:=$04ED;
  UnicodeLower0460_0512[$04EE]:=$04EF;
  UnicodeLower0460_0512[$04F0]:=$04F1;
  UnicodeLower0460_0512[$04F2]:=$04F3;
  UnicodeLower0460_0512[$04F4]:=$04F5;
  UnicodeLower0460_0512[$04F6]:=$04F7;
  UnicodeLower0460_0512[$04F8]:=$04F9;
  UnicodeLower0460_0512[$04FA]:=$04FB;
  UnicodeLower0460_0512[$04FC]:=$04FD;
  UnicodeLower0460_0512[$04FE]:=$04FF;
  UnicodeLower0460_0512[$0500]:=$0501;
  UnicodeLower0460_0512[$0502]:=$0503;
  UnicodeLower0460_0512[$0504]:=$0505;
  UnicodeLower0460_0512[$0506]:=$0507;
  UnicodeLower0460_0512[$0508]:=$0509;
  UnicodeLower0460_0512[$050A]:=$050B;
  UnicodeLower0460_0512[$050C]:=$050D;
  UnicodeLower0460_0512[$050E]:=$050F;
  UnicodeLower0460_0512[$0510]:=$0511;
  UnicodeLower0460_0512[$0512]:=$0513;

  for i:=Low(UnicodeLower1E00_1FFC) to High(UnicodeLower1E00_1FFC) do
    UnicodeLower1E00_1FFC[i]:=i;
  UnicodeLower1E00_1FFC[$1E00]:=$1E01;
  UnicodeLower1E00_1FFC[$1E02]:=$1E03;
  UnicodeLower1E00_1FFC[$1E04]:=$1E05;
  UnicodeLower1E00_1FFC[$1E06]:=$1E07;
  UnicodeLower1E00_1FFC[$1E08]:=$1E09;
  UnicodeLower1E00_1FFC[$1E0A]:=$1E0B;
  UnicodeLower1E00_1FFC[$1E0C]:=$1E0D;
  UnicodeLower1E00_1FFC[$1E0E]:=$1E0F;
  UnicodeLower1E00_1FFC[$1E10]:=$1E11;
  UnicodeLower1E00_1FFC[$1E12]:=$1E13;
  UnicodeLower1E00_1FFC[$1E14]:=$1E15;
  UnicodeLower1E00_1FFC[$1E16]:=$1E17;
  UnicodeLower1E00_1FFC[$1E18]:=$1E19;
  UnicodeLower1E00_1FFC[$1E1A]:=$1E1B;
  UnicodeLower1E00_1FFC[$1E1C]:=$1E1D;
  UnicodeLower1E00_1FFC[$1E1E]:=$1E1F;
  UnicodeLower1E00_1FFC[$1E20]:=$1E21;
  UnicodeLower1E00_1FFC[$1E22]:=$1E23;
  UnicodeLower1E00_1FFC[$1E24]:=$1E25;
  UnicodeLower1E00_1FFC[$1E26]:=$1E27;
  UnicodeLower1E00_1FFC[$1E28]:=$1E29;
  UnicodeLower1E00_1FFC[$1E2A]:=$1E2B;
  UnicodeLower1E00_1FFC[$1E2C]:=$1E2D;
  UnicodeLower1E00_1FFC[$1E2E]:=$1E2F;
  UnicodeLower1E00_1FFC[$1E30]:=$1E31;
  UnicodeLower1E00_1FFC[$1E32]:=$1E33;
  UnicodeLower1E00_1FFC[$1E34]:=$1E35;
  UnicodeLower1E00_1FFC[$1E36]:=$1E37;
  UnicodeLower1E00_1FFC[$1E38]:=$1E39;
  UnicodeLower1E00_1FFC[$1E3A]:=$1E3B;
  UnicodeLower1E00_1FFC[$1E3C]:=$1E3D;
  UnicodeLower1E00_1FFC[$1E3E]:=$1E3F;
  UnicodeLower1E00_1FFC[$1E40]:=$1E41;
  UnicodeLower1E00_1FFC[$1E42]:=$1E43;
  UnicodeLower1E00_1FFC[$1E44]:=$1E45;
  UnicodeLower1E00_1FFC[$1E46]:=$1E47;
  UnicodeLower1E00_1FFC[$1E48]:=$1E49;
  UnicodeLower1E00_1FFC[$1E4A]:=$1E4B;
  UnicodeLower1E00_1FFC[$1E4C]:=$1E4D;
  UnicodeLower1E00_1FFC[$1E4E]:=$1E4F;
  UnicodeLower1E00_1FFC[$1E50]:=$1E51;
  UnicodeLower1E00_1FFC[$1E52]:=$1E53;
  UnicodeLower1E00_1FFC[$1E54]:=$1E55;
  UnicodeLower1E00_1FFC[$1E56]:=$1E57;
  UnicodeLower1E00_1FFC[$1E58]:=$1E59;
  UnicodeLower1E00_1FFC[$1E5A]:=$1E5B;
  UnicodeLower1E00_1FFC[$1E5C]:=$1E5D;
  UnicodeLower1E00_1FFC[$1E5E]:=$1E5F;
  UnicodeLower1E00_1FFC[$1E60]:=$1E61;
  UnicodeLower1E00_1FFC[$1E62]:=$1E63;
  UnicodeLower1E00_1FFC[$1E64]:=$1E65;
  UnicodeLower1E00_1FFC[$1E66]:=$1E67;
  UnicodeLower1E00_1FFC[$1E68]:=$1E69;
  UnicodeLower1E00_1FFC[$1E6A]:=$1E6B;
  UnicodeLower1E00_1FFC[$1E6C]:=$1E6D;
  UnicodeLower1E00_1FFC[$1E6E]:=$1E6F;
  UnicodeLower1E00_1FFC[$1E70]:=$1E71;
  UnicodeLower1E00_1FFC[$1E72]:=$1E73;
  UnicodeLower1E00_1FFC[$1E74]:=$1E75;
  UnicodeLower1E00_1FFC[$1E76]:=$1E77;
  UnicodeLower1E00_1FFC[$1E78]:=$1E79;
  UnicodeLower1E00_1FFC[$1E7A]:=$1E7B;
  UnicodeLower1E00_1FFC[$1E7C]:=$1E7D;
  UnicodeLower1E00_1FFC[$1E7E]:=$1E7F;
  UnicodeLower1E00_1FFC[$1E80]:=$1E81;
  UnicodeLower1E00_1FFC[$1E82]:=$1E83;
  UnicodeLower1E00_1FFC[$1E84]:=$1E85;
  UnicodeLower1E00_1FFC[$1E86]:=$1E87;
  UnicodeLower1E00_1FFC[$1E88]:=$1E89;
  UnicodeLower1E00_1FFC[$1E8A]:=$1E8B;
  UnicodeLower1E00_1FFC[$1E8C]:=$1E8D;
  UnicodeLower1E00_1FFC[$1E8E]:=$1E8F;
  UnicodeLower1E00_1FFC[$1E90]:=$1E91;
  UnicodeLower1E00_1FFC[$1E92]:=$1E93;
  UnicodeLower1E00_1FFC[$1E94]:=$1E95;
  UnicodeLower1E00_1FFC[$1EA0]:=$1EA1;
  UnicodeLower1E00_1FFC[$1EA2]:=$1EA3;
  UnicodeLower1E00_1FFC[$1EA4]:=$1EA5;
  UnicodeLower1E00_1FFC[$1EA6]:=$1EA7;
  UnicodeLower1E00_1FFC[$1EA8]:=$1EA9;
  UnicodeLower1E00_1FFC[$1EAA]:=$1EAB;
  UnicodeLower1E00_1FFC[$1EAC]:=$1EAD;
  UnicodeLower1E00_1FFC[$1EAE]:=$1EAF;
  UnicodeLower1E00_1FFC[$1EB0]:=$1EB1;
  UnicodeLower1E00_1FFC[$1EB2]:=$1EB3;
  UnicodeLower1E00_1FFC[$1EB4]:=$1EB5;
  UnicodeLower1E00_1FFC[$1EB6]:=$1EB7;
  UnicodeLower1E00_1FFC[$1EB8]:=$1EB9;
  UnicodeLower1E00_1FFC[$1EBA]:=$1EBB;
  UnicodeLower1E00_1FFC[$1EBC]:=$1EBD;
  UnicodeLower1E00_1FFC[$1EBE]:=$1EBF;
  UnicodeLower1E00_1FFC[$1EC0]:=$1EC1;
  UnicodeLower1E00_1FFC[$1EC2]:=$1EC3;
  UnicodeLower1E00_1FFC[$1EC4]:=$1EC5;
  UnicodeLower1E00_1FFC[$1EC6]:=$1EC7;
  UnicodeLower1E00_1FFC[$1EC8]:=$1EC9;
  UnicodeLower1E00_1FFC[$1ECA]:=$1ECB;
  UnicodeLower1E00_1FFC[$1ECC]:=$1ECD;
  UnicodeLower1E00_1FFC[$1ECE]:=$1ECF;
  UnicodeLower1E00_1FFC[$1ED0]:=$1ED1;
  UnicodeLower1E00_1FFC[$1ED2]:=$1ED3;
  UnicodeLower1E00_1FFC[$1ED4]:=$1ED5;
  UnicodeLower1E00_1FFC[$1ED6]:=$1ED7;
  UnicodeLower1E00_1FFC[$1ED8]:=$1ED9;
  UnicodeLower1E00_1FFC[$1EDA]:=$1EDB;
  UnicodeLower1E00_1FFC[$1EDC]:=$1EDD;
  UnicodeLower1E00_1FFC[$1EDE]:=$1EDF;
  UnicodeLower1E00_1FFC[$1EE0]:=$1EE1;
  UnicodeLower1E00_1FFC[$1EE2]:=$1EE3;
  UnicodeLower1E00_1FFC[$1EE4]:=$1EE5;
  UnicodeLower1E00_1FFC[$1EE6]:=$1EE7;
  UnicodeLower1E00_1FFC[$1EE8]:=$1EE9;
  UnicodeLower1E00_1FFC[$1EEA]:=$1EEB;
  UnicodeLower1E00_1FFC[$1EEC]:=$1EED;
  UnicodeLower1E00_1FFC[$1EEE]:=$1EEF;
  UnicodeLower1E00_1FFC[$1EF0]:=$1EF1;
  UnicodeLower1E00_1FFC[$1EF2]:=$1EF3;
  UnicodeLower1E00_1FFC[$1EF4]:=$1EF5;
  UnicodeLower1E00_1FFC[$1EF6]:=$1EF7;
  UnicodeLower1E00_1FFC[$1EF8]:=$1EF9;
  UnicodeLower1E00_1FFC[$1F08]:=$1F00;
  UnicodeLower1E00_1FFC[$1F09]:=$1F01;
  UnicodeLower1E00_1FFC[$1F0A]:=$1F02;
  UnicodeLower1E00_1FFC[$1F0B]:=$1F03;
  UnicodeLower1E00_1FFC[$1F0C]:=$1F04;
  UnicodeLower1E00_1FFC[$1F0D]:=$1F05;
  UnicodeLower1E00_1FFC[$1F0E]:=$1F06;
  UnicodeLower1E00_1FFC[$1F0F]:=$1F07;
  UnicodeLower1E00_1FFC[$1F18]:=$1F10;
  UnicodeLower1E00_1FFC[$1F19]:=$1F11;
  UnicodeLower1E00_1FFC[$1F1A]:=$1F12;
  UnicodeLower1E00_1FFC[$1F1B]:=$1F13;
  UnicodeLower1E00_1FFC[$1F1C]:=$1F14;
  UnicodeLower1E00_1FFC[$1F1D]:=$1F15;
  UnicodeLower1E00_1FFC[$1F28]:=$1F20;
  UnicodeLower1E00_1FFC[$1F29]:=$1F21;
  UnicodeLower1E00_1FFC[$1F2A]:=$1F22;
  UnicodeLower1E00_1FFC[$1F2B]:=$1F23;
  UnicodeLower1E00_1FFC[$1F2C]:=$1F24;
  UnicodeLower1E00_1FFC[$1F2D]:=$1F25;
  UnicodeLower1E00_1FFC[$1F2E]:=$1F26;
  UnicodeLower1E00_1FFC[$1F2F]:=$1F27;
  UnicodeLower1E00_1FFC[$1F38]:=$1F30;
  UnicodeLower1E00_1FFC[$1F39]:=$1F31;
  UnicodeLower1E00_1FFC[$1F3A]:=$1F32;
  UnicodeLower1E00_1FFC[$1F3B]:=$1F33;
  UnicodeLower1E00_1FFC[$1F3C]:=$1F34;
  UnicodeLower1E00_1FFC[$1F3D]:=$1F35;
  UnicodeLower1E00_1FFC[$1F3E]:=$1F36;
  UnicodeLower1E00_1FFC[$1F3F]:=$1F37;
  UnicodeLower1E00_1FFC[$1F48]:=$1F40;
  UnicodeLower1E00_1FFC[$1F49]:=$1F41;
  UnicodeLower1E00_1FFC[$1F4A]:=$1F42;
  UnicodeLower1E00_1FFC[$1F4B]:=$1F43;
  UnicodeLower1E00_1FFC[$1F4C]:=$1F44;
  UnicodeLower1E00_1FFC[$1F4D]:=$1F45;
  UnicodeLower1E00_1FFC[$1F59]:=$1F51;
  UnicodeLower1E00_1FFC[$1F5B]:=$1F53;
  UnicodeLower1E00_1FFC[$1F5D]:=$1F55;
  UnicodeLower1E00_1FFC[$1F5F]:=$1F57;
  UnicodeLower1E00_1FFC[$1F68]:=$1F60;
  UnicodeLower1E00_1FFC[$1F69]:=$1F61;
  UnicodeLower1E00_1FFC[$1F6A]:=$1F62;
  UnicodeLower1E00_1FFC[$1F6B]:=$1F63;
  UnicodeLower1E00_1FFC[$1F6C]:=$1F64;
  UnicodeLower1E00_1FFC[$1F6D]:=$1F65;
  UnicodeLower1E00_1FFC[$1F6E]:=$1F66;
  UnicodeLower1E00_1FFC[$1F6F]:=$1F67;
  UnicodeLower1E00_1FFC[$1F88]:=$1F80;
  UnicodeLower1E00_1FFC[$1F89]:=$1F81;
  UnicodeLower1E00_1FFC[$1F8A]:=$1F82;
  UnicodeLower1E00_1FFC[$1F8B]:=$1F83;
  UnicodeLower1E00_1FFC[$1F8C]:=$1F84;
  UnicodeLower1E00_1FFC[$1F8D]:=$1F85;
  UnicodeLower1E00_1FFC[$1F8E]:=$1F86;
  UnicodeLower1E00_1FFC[$1F8F]:=$1F87;
  UnicodeLower1E00_1FFC[$1F98]:=$1F90;
  UnicodeLower1E00_1FFC[$1F99]:=$1F91;
  UnicodeLower1E00_1FFC[$1F9A]:=$1F92;
  UnicodeLower1E00_1FFC[$1F9B]:=$1F93;
  UnicodeLower1E00_1FFC[$1F9C]:=$1F94;
  UnicodeLower1E00_1FFC[$1F9D]:=$1F95;
  UnicodeLower1E00_1FFC[$1F9E]:=$1F96;
  UnicodeLower1E00_1FFC[$1F9F]:=$1F97;
  UnicodeLower1E00_1FFC[$1FA8]:=$1FA0;
  UnicodeLower1E00_1FFC[$1FA9]:=$1FA1;
  UnicodeLower1E00_1FFC[$1FAA]:=$1FA2;
  UnicodeLower1E00_1FFC[$1FAB]:=$1FA3;
  UnicodeLower1E00_1FFC[$1FAC]:=$1FA4;
  UnicodeLower1E00_1FFC[$1FAD]:=$1FA5;
  UnicodeLower1E00_1FFC[$1FAE]:=$1FA6;
  UnicodeLower1E00_1FFC[$1FAF]:=$1FA7;
  UnicodeLower1E00_1FFC[$1FB8]:=$1FB0;
  UnicodeLower1E00_1FFC[$1FB9]:=$1FB1;
  UnicodeLower1E00_1FFC[$1FBA]:=$1F70;
  UnicodeLower1E00_1FFC[$1FBB]:=$1F71;
  UnicodeLower1E00_1FFC[$1FBC]:=$1FB3;
  UnicodeLower1E00_1FFC[$1FC8]:=$1F72;
  UnicodeLower1E00_1FFC[$1FC9]:=$1F73;
  UnicodeLower1E00_1FFC[$1FCA]:=$1F74;
  UnicodeLower1E00_1FFC[$1FCB]:=$1F75;
  UnicodeLower1E00_1FFC[$1FCC]:=$1FC3;
  UnicodeLower1E00_1FFC[$1FD8]:=$1FD0;
  UnicodeLower1E00_1FFC[$1FD9]:=$1FD1;
  UnicodeLower1E00_1FFC[$1FDA]:=$1F76;
  UnicodeLower1E00_1FFC[$1FDB]:=$1F77;
  UnicodeLower1E00_1FFC[$1FE8]:=$1FE0;
  UnicodeLower1E00_1FFC[$1FE9]:=$1FE1;
  UnicodeLower1E00_1FFC[$1FEA]:=$1F7A;
  UnicodeLower1E00_1FFC[$1FEB]:=$1F7B;
  UnicodeLower1E00_1FFC[$1FEC]:=$1FE5;
  UnicodeLower1E00_1FFC[$1FF8]:=$1F78;
  UnicodeLower1E00_1FFC[$1FF9]:=$1F79;
  UnicodeLower1E00_1FFC[$1FFA]:=$1F7C;
  UnicodeLower1E00_1FFC[$1FFB]:=$1F7D;
  UnicodeLower1E00_1FFC[$1FFC]:=$1FF3;

  for i:=Low(UnicodeLower2126_2183) to High(UnicodeLower2126_2183) do
    UnicodeLower2126_2183[i]:=i;
  UnicodeLower2126_2183[$2126]:=$03C9;
  UnicodeLower2126_2183[$212A]:=$006B;
  UnicodeLower2126_2183[$212B]:=$00E5;
  UnicodeLower2126_2183[$2132]:=$214E;
  UnicodeLower2126_2183[$2160]:=$2170;
  UnicodeLower2126_2183[$2161]:=$2171;
  UnicodeLower2126_2183[$2162]:=$2172;
  UnicodeLower2126_2183[$2163]:=$2173;
  UnicodeLower2126_2183[$2164]:=$2174;
  UnicodeLower2126_2183[$2165]:=$2175;
  UnicodeLower2126_2183[$2166]:=$2176;
  UnicodeLower2126_2183[$2167]:=$2177;
  UnicodeLower2126_2183[$2168]:=$2178;
  UnicodeLower2126_2183[$2169]:=$2179;
  UnicodeLower2126_2183[$216A]:=$217A;
  UnicodeLower2126_2183[$216B]:=$217B;
  UnicodeLower2126_2183[$216C]:=$217C;
  UnicodeLower2126_2183[$216D]:=$217D;
  UnicodeLower2126_2183[$216E]:=$217E;
  UnicodeLower2126_2183[$216F]:=$217F;
  UnicodeLower2126_2183[$2183]:=$2184;

  for i:=Low(UnicodeLower2C60_2CE2) to High(UnicodeLower2C60_2CE2) do
    UnicodeLower2C60_2CE2[i]:=i;
  UnicodeLower2C60_2CE2[$2C60]:=$2C61;
  UnicodeLower2C60_2CE2[$2C62]:=$026B;
  UnicodeLower2C60_2CE2[$2C63]:=$1D7D;
  UnicodeLower2C60_2CE2[$2C64]:=$027D;
  UnicodeLower2C60_2CE2[$2C67]:=$2C68;
  UnicodeLower2C60_2CE2[$2C69]:=$2C6A;
  UnicodeLower2C60_2CE2[$2C6B]:=$2C6C;
  UnicodeLower2C60_2CE2[$2C75]:=$2C76;
  UnicodeLower2C60_2CE2[$2C80]:=$2C81;
  UnicodeLower2C60_2CE2[$2C82]:=$2C83;
  UnicodeLower2C60_2CE2[$2C84]:=$2C85;
  UnicodeLower2C60_2CE2[$2C86]:=$2C87;
  UnicodeLower2C60_2CE2[$2C88]:=$2C89;
  UnicodeLower2C60_2CE2[$2C8A]:=$2C8B;
  UnicodeLower2C60_2CE2[$2C8C]:=$2C8D;
  UnicodeLower2C60_2CE2[$2C8E]:=$2C8F;
  UnicodeLower2C60_2CE2[$2C90]:=$2C91;
  UnicodeLower2C60_2CE2[$2C92]:=$2C93;
  UnicodeLower2C60_2CE2[$2C94]:=$2C95;
  UnicodeLower2C60_2CE2[$2C96]:=$2C97;
  UnicodeLower2C60_2CE2[$2C98]:=$2C99;
  UnicodeLower2C60_2CE2[$2C9A]:=$2C9B;
  UnicodeLower2C60_2CE2[$2C9C]:=$2C9D;
  UnicodeLower2C60_2CE2[$2C9E]:=$2C9F;
  UnicodeLower2C60_2CE2[$2CA0]:=$2CA1;
  UnicodeLower2C60_2CE2[$2CA2]:=$2CA3;
  UnicodeLower2C60_2CE2[$2CA4]:=$2CA5;
  UnicodeLower2C60_2CE2[$2CA6]:=$2CA7;
  UnicodeLower2C60_2CE2[$2CA8]:=$2CA9;
  UnicodeLower2C60_2CE2[$2CAA]:=$2CAB;
  UnicodeLower2C60_2CE2[$2CAC]:=$2CAD;
  UnicodeLower2C60_2CE2[$2CAE]:=$2CAF;
  UnicodeLower2C60_2CE2[$2CB0]:=$2CB1;
  UnicodeLower2C60_2CE2[$2CB2]:=$2CB3;
  UnicodeLower2C60_2CE2[$2CB4]:=$2CB5;
  UnicodeLower2C60_2CE2[$2CB6]:=$2CB7;
  UnicodeLower2C60_2CE2[$2CB8]:=$2CB9;
  UnicodeLower2C60_2CE2[$2CBA]:=$2CBB;
  UnicodeLower2C60_2CE2[$2CBC]:=$2CBD;
  UnicodeLower2C60_2CE2[$2CBE]:=$2CBF;
  UnicodeLower2C60_2CE2[$2CC0]:=$2CC1;
  UnicodeLower2C60_2CE2[$2CC2]:=$2CC3;
  UnicodeLower2C60_2CE2[$2CC4]:=$2CC5;
  UnicodeLower2C60_2CE2[$2CC6]:=$2CC7;
  UnicodeLower2C60_2CE2[$2CC8]:=$2CC9;
  UnicodeLower2C60_2CE2[$2CCA]:=$2CCB;
  UnicodeLower2C60_2CE2[$2CCC]:=$2CCD;
  UnicodeLower2C60_2CE2[$2CCE]:=$2CCF;
  UnicodeLower2C60_2CE2[$2CD0]:=$2CD1;
  UnicodeLower2C60_2CE2[$2CD2]:=$2CD3;
  UnicodeLower2C60_2CE2[$2CD4]:=$2CD5;
  UnicodeLower2C60_2CE2[$2CD6]:=$2CD7;
  UnicodeLower2C60_2CE2[$2CD8]:=$2CD9;
  UnicodeLower2C60_2CE2[$2CDA]:=$2CDB;
  UnicodeLower2C60_2CE2[$2CDC]:=$2CDD;
  UnicodeLower2C60_2CE2[$2CDE]:=$2CDF;
  UnicodeLower2C60_2CE2[$2CE0]:=$2CE1;
  UnicodeLower2C60_2CE2[$2CE2]:=$2CE3;
end;
{$endif NewLowerCase}

function DeleteAmpersands(var Str : String) : Longint;
// Replace all &x with x
// and return the position of the first ampersand letter in the resulting Str.
// double ampersands && are converted to a single & and are ignored.
var
  SrcPos, DestPos, SrcLen: Integer;
begin
  Result:=-1;
  SrcLen:=length(Str);
  SrcPos:=1;
  DestPos:=1;
  while SrcPos<=SrcLen do begin
    if (Str[SrcPos]='&') and (SrcPos<SrcLen) then begin
      // & found
      inc(SrcPos); // skip &
      if (Str[SrcPos]<>'&') and (Result<1) then
        Result:=DestPos;
    end;
    if DestPos<SrcPos then
      Str[DestPos]:=Str[SrcPos];
    inc(SrcPos);
    inc(DestPos);
  end;
  if DestPos<SrcPos then
    SetLength(Str,DestPos-1);
end;

//-----------------------------------------------------------------------------
// Keys and shortcuts

type
  TMenuKeyCap = (mkcBkSp, mkcTab, mkcEsc, mkcEnter, mkcSpace, mkcPgUp,
    mkcPgDn, mkcEnd, mkcHome, mkcLeft, mkcUp, mkcRight, mkcDown, mkcIns,
    mkcDel, mkcShift, mkcCtrl, mkcAlt, mkcMeta);

var
  MenuKeyCaps: array[TMenuKeyCap] of string;
  MenuKeyCapsInited: boolean = false;

procedure InitializeMenuKeyCaps;
begin
  if MenuKeyCapsInited=false then
  begin
    MenuKeyCaps[mkcBkSp]:=SmkcBkSp;
    MenuKeyCaps[mkcTab]:=SmkcTab;
    MenuKeyCaps[mkcEsc]:=SmkcEsc;
    MenuKeyCaps[mkcEnter]:=SmkcEnter;
    MenuKeyCaps[mkcSpace]:=SmkcSpace;
    MenuKeyCaps[mkcPgUp]:=SmkcPgUp;
    MenuKeyCaps[mkcPgDn]:=SmkcPgDn;
    MenuKeyCaps[mkcEnd]:=SmkcEnd;
    MenuKeyCaps[mkcHome]:=SmkcHome;
    MenuKeyCaps[mkcLeft]:=SmkcLeft;
    MenuKeyCaps[mkcUp]:=SmkcUp;
    MenuKeyCaps[mkcRight]:=SmkcRight;
    MenuKeyCaps[mkcDown]:=SmkcDown;
    MenuKeyCaps[mkcIns]:=SmkcIns;
    MenuKeyCaps[mkcDel]:=SmkcDel;
    MenuKeyCaps[mkcShift]:=SmkcShift;
    MenuKeyCaps[mkcCtrl]:=SmkcCtrl;
    MenuKeyCaps[mkcAlt]:=SmkcAlt;
    MenuKeyCaps[mkcMeta]:=SmkcMeta;
    MenuKeyCapsInited:=true;
  end;
end;

function GetSpecialShortCutName(ShortCut: TShortCut): string;
begin
  // ToDo
  Result := '';
end;

function CompareDebugLCLItemInfos(Data1, Data2: Pointer): integer;
begin
  Result:=ComparePointers(TDebugLCLItemInfo(Data1).Item,
                          TDebugLCLItemInfo(Data2).Item);
end;

function CompareItemWithDebugLCLItemInfo(Item, DebugItemInfo: Pointer): integer;
begin
  Result:=ComparePointers(Item,TDebugLCLItemInfo(DebugItemInfo).Item);
end;

function CompareLineInfoCacheItems(Data1, Data2: Pointer): integer;
begin
  Result:=ComparePointers(PLineInfoCacheItem(Data1)^.Addr,
                          PLineInfoCacheItem(Data2)^.Addr);
end;

function CompareAddrWithLineInfoCacheItem(Addr, Item: Pointer): integer;
begin
  Result:=ComparePointers(Addr,PLineInfoCacheItem(Item)^.Addr);
end;

function GetEnumValueDef(TypeInfo: PTypeInfo; const Name: string;
  const DefaultValue: Integer): Integer;
begin
  Result:=GetEnumValue(TypeInfo,Name);
  if Result<0 then
    Result:=DefaultValue;
end;

function ShortCutToText(ShortCut: TShortCut): string;
var
  Name: string;
  Key: Byte;
begin
  InitializeMenuKeyCaps;
  Key := ShortCut and $FF;
  case Key of
    $08, $09:
      Name := MenuKeyCaps[TMenuKeyCap(Ord(mkcBkSp) + Key - $08)];
    $0D: Name := MenuKeyCaps[mkcEnter];
    $1B: Name := MenuKeyCaps[mkcEsc];
    $20..$28:
      Name := MenuKeyCaps[TMenuKeyCap(Ord(mkcSpace) + Key - $20)];
    $2D..$2E:
      Name := MenuKeyCaps[TMenuKeyCap(Ord(mkcIns) + Key - $2D)];
    $30..$39: Name := Chr(Key - $30 + Ord('0'));
    $41..$5A: Name := Chr(Key - $41 + Ord('A'));
    $60..$69: Name := Chr(Key - $60 + Ord('0'));
    $70..$87: Name := 'F' + IntToStr(Key - $6F);
  else
    Name := GetSpecialShortCutName(ShortCut);
  end;
  if Name <> '' then
  begin
    Result := '';
    if ShortCut and scShift <> 0 then Result := Result + MenuKeyCaps[mkcShift];
    if ShortCut and scCtrl <> 0 then Result := Result + MenuKeyCaps[mkcCtrl];
    if ShortCut and scMeta <> 0 then Result := Result + MenuKeyCaps[mkcMeta];
    if ShortCut and scAlt <> 0 then Result := Result + MenuKeyCaps[mkcAlt];
    Result := Result + Name;
  end
  else Result := '';
end;

function TextToShortCut(const ShortCutText: string): TShortCut;

  function CompareFront(var StartPos: integer; const Front: string): Boolean;
  begin
    if (Front<>'') and (StartPos+length(Front)-1<=length(ShortCutText))
    and (AnsiStrLIComp(@ShortCutText[StartPos], PChar(Front), Length(Front))= 0)
    then begin
      Result:=true;
      inc(StartPos,length(Front));
    end else
      Result:=false;
  end;

var
  Key: TShortCut;
  Shift: TShortCut;
  StartPos: integer;
  Name: string;
begin
  Result := 0;
  Shift := 0;
  StartPos:=1;
  InitializeMenuKeyCaps;
  while True do
  begin
    if CompareFront(StartPos, MenuKeyCaps[mkcShift]) then
      Shift := Shift or scShift
    else if CompareFront(StartPos, '^') then
      Shift := Shift or scCtrl
    else if CompareFront(StartPos, MenuKeyCaps[mkcCtrl]) then
      Shift := Shift or scCtrl
    else if CompareFront(StartPos, MenuKeyCaps[mkcAlt]) then
      Shift := Shift or scAlt
    else if CompareFront(StartPos, MenuKeyCaps[mkcMeta]) then
      Shift := Shift or scMeta
    else
      Break;
  end;
  if ShortCutText = '' then Exit;
  for Key := $08 to $FF do begin { Copy range from table in ShortCutToText }
    Name:=ShortCutToText(Key);
    if (Name<>'') and (length(Name)=length(ShortCutText)-StartPos+1)
    and (AnsiStrLIComp(@ShortCutText[StartPos], PChar(Name), length(Name)) = 0)
    then begin
      Result := Key or Shift;
      Exit;
    end;
  end;
end;

function GetCompleteText(sText: string; iSelStart: Integer;
  bCaseSensitive, bSearchAscending: Boolean; slTextList: TStrings): string;

  function IsSamePrefix(sCompareText, sPrefix: string; iStart: Integer;
    var ResultText: string): Boolean;
  var
    sTempText: string;
  begin
    Result := False;
    sTempText := UTF8Copy(sCompareText, 1, iStart);
    if not bCaseSensitive then
      sTempText := UTF8UpperCase(sTempText);
    if (sTempText = sPrefix) then
    begin
      ResultText := sCompareText;
      Result := True;
    end;
  end;

var
  i: Integer;
  sPrefixText: string;
begin
  //DebugLn(['GetCompleteText sText=',sText,' iSelStart=',iSelStart,' bCaseSensitive=',bCaseSensitive,' bSearchAscending=',bSearchAscending,' slTextList.Count=',slTextList.Count]);
  Result := sText;//Default to return original text if no identical text are found
  if (sText = '') then Exit;//Everything is compatible with nothing, Exit.
  if (iSelStart = 0) then Exit;//Cursor at beginning
  if (slTextList.Count = 0) then Exit;//No text list to search for idtenticals, Exit.
  sPrefixText := UTF8Copy(sText, 1, iSelStart);//Get text from beginning to cursor position.
  if not bCaseSensitive then
    sPrefixText := UTF8UpperCase(sPrefixText);
  if bSearchAscending then
  begin
    for i := 0 to slTextList.Count - 1 do
      if IsSamePrefix(slTextList[i], sPrefixText, iSelStart, Result) then
        break;
  end else
  begin
    for i := slTextList.Count - 1 downto 0 do
      if IsSamePrefix(slTextList[i], sPrefixText, iSelStart, Result) then
        break;
  end;
end;

function IsEditableTextKey(Key: Word): Boolean;
begin
 Result := (((Key >= VK_A) and (Key <= VK_Z)) or
            ((Key >= VK_NUMPAD0) and (Key <= VK_DIVIDE)) or
            ((Key >= 186) and (Key <= 188)) or
            ((Key >= 190) and (Key <= 192)) or
            ((Key >= 219) and (Key <= 222)));
end;

function SendApplicationMessage(Msg: Cardinal; WParam: WParam; LParam: LParam
  ): Longint;
begin
  if SendApplicationMessageFunction<>nil then
    Result:=SendApplicationMessageFunction(Msg, WParam, LParam)
  else
    Result:=0;
end;

procedure OwnerFormDesignerModified(AComponent: TComponent);
begin
  if ([csDesigning,csLoading,csDestroying]*AComponent.ComponentState
    =[csDesigning])
  then begin
    if OwnerFormDesignerModifiedProc<>nil then
      OwnerFormDesignerModifiedProc(AComponent);
  end;
end;

function OffSetRect(var ARect: TRect; dx,dy: Integer): Boolean;
Begin
  with ARect do
  begin
    Left := Left + dx;
    Right := Right + dx;
    Top := Top + dy;
    Bottom := Bottom + dy;
  end;
  if (ARect.Left >= 0) and (ARect.Top >= 0) then
    Result := True
  else
    Result := False;
end;

procedure FreeThenNil(var obj);
begin
  if Pointer(obj) <> nil then 
  begin
    TObject(obj).Free;
    Pointer(obj) := nil;
  end;
end;

procedure RegisterInterfaceInitializationHandler(p: TProcedure);
begin
  InterfaceInitializationHandlers.Add(p);
end;

procedure CallInterfaceInitializationHandlers;
var
  i: Integer;
begin
  for i:=0 to InterfaceInitializationHandlers.Count-1 do
    TProcedure(InterfaceInitializationHandlers[i])();
end;

procedure RegisterInterfaceFinalizationHandler(p: TProcedure);
begin
  InterfaceFinalizationHandlers.Add(p);
end;

procedure CallInterfaceFinalizationHandlers;
var
  i: Integer;
begin
  for i:=InterfaceFinalizationHandlers.Count-1 downto 0 do
    TProcedure(InterfaceFinalizationHandlers[i])();
end;

{ TMethodList }

function TMethodList.GetItems(Index: integer): TMethod;
begin
  Result:=FItems[Index];
end;

procedure TMethodList.SetItems(Index: integer; const AValue: TMethod);
begin
  FItems[Index]:=AValue;
end;

destructor TMethodList.Destroy;
begin
  ReAllocMem(FItems,0);
  inherited Destroy;
end;

function TMethodList.Count: integer;
begin
  if Self<>nil then
    Result:=FCount
  else
    Result:=0;
end;

function TMethodList.NextDownIndex(var Index: integer): boolean;
begin
  if Self<>nil then begin
    dec(Index);
    if (Index>=FCount) then
      Index:=FCount-1;
  end else
    Index:=-1;
  Result:=(Index>=0);
end;

function TMethodList.IndexOf(const AMethod: TMethod): integer;
begin
  if Self<>nil then begin
    Result:=FCount-1;
    while Result>=0 do begin
      if (FItems[Result].Code=AMethod.Code)
      and (FItems[Result].Data=AMethod.Data) then exit;
      dec(Result);
    end;
  end else
    Result:=-1;
end;

procedure TMethodList.Delete(Index: integer);
begin
  dec(FCount);
  if FCount>Index then
    System.Move(FItems[Index+1],FItems[Index],(FCount-Index)*SizeOf(TMethod));
  ReAllocMem(FItems,FCount*SizeOf(TMethod));
end;

procedure TMethodList.Remove(const AMethod: TMethod);
var
  i: integer;
begin
  if Self<>nil then begin
    i:=IndexOf(AMethod);
    if i>=0 then Delete(i);
  end;
end;

procedure TMethodList.Add(const AMethod: TMethod);
begin
  inc(FCount);
  ReAllocMem(FItems,FCount*SizeOf(TMethod));
  FItems[FCount-1]:=AMethod;
end;

procedure TMethodList.Add(const AMethod: TMethod; AsLast: boolean);
begin
  if AsLast then
    Add(AMethod)
  else
    Insert(0,AMethod);
end;

procedure TMethodList.Insert(Index: integer; const AMethod: TMethod);
begin
  inc(FCount);
  ReAllocMem(FItems,FCount*SizeOf(TMethod));
  if Index<FCount then
    System.Move(FItems[Index],FItems[Index+1],(FCount-Index-1)*SizeOf(TMethod));
  FItems[Index]:=AMethod;
end;

procedure TMethodList.Move(OldIndex, NewIndex: integer);
var
  MovingMethod: TMethod;
begin
  if OldIndex=NewIndex then exit;
  MovingMethod:=FItems[OldIndex];
  if OldIndex>NewIndex then
    System.Move(FItems[NewIndex],FItems[NewIndex+1],
                SizeOf(TMethod)*(OldIndex-NewIndex))
  else
    System.Move(FItems[NewIndex+1],FItems[NewIndex],
                SizeOf(TMethod)*(NewIndex-OldIndex));
  FItems[NewIndex]:=MovingMethod;
end;

procedure TMethodList.RemoveAllMethodsOfObject(const AnObject: TObject);
var
  i: Integer;
begin
  if Self=nil then exit;
  i:=FCount-1;
  while i>=0 do begin
    if TObject(FItems[i].Data)=AnObject then Delete(i);
    dec(i);
  end;
end;

procedure TMethodList.CallNotifyEvents(Sender: TObject);
var
  i: LongInt;
begin
  i:=Count;
  while NextDownIndex(i) do
    TNotifyEvent(Items[i])(Sender);
end;

{------------------------------------------------------------------------------
  procedure RaiseGDBException(const Msg: string);

  Raises an exception.
  Normally gdb does not catch fpc Exception objects, therefore this procedure
  raises a standard "division by zero" exception which is catched by gdb.
  This allows to stop a program, without extra gdb configuration.
 ------------------------------------------------------------------------------}
procedure RaiseGDBException(const Msg: string);
begin
  debugln(rsERRORInLCL, Msg);
  // creates an exception, that gdb catches:
  debugln(rsCreatingGdbCatchableError);
  DumpStack;
  if (length(Msg) div (length(Msg) div 10000))=0 then ;
end;

procedure RaiseAndCatchException;
begin
  try
    if (length(rsERRORInLCL) div (length(rsERRORInLCL) div 10000))=0 then ;
  except
  end;
end;

procedure DumpAddr(Addr: Pointer);
begin
  // preventing another exception, while dumping stack trace
  try
    DebugLn(BackTraceStrFunc(Addr));
  except
    DebugLn(SysBackTraceStr(Addr));
  end;
end;

procedure DumpExceptionBackTrace;
var
  FrameCount: integer;
  Frames: PPointer;
  FrameNumber:Integer;
begin
  DebugLn('  Stack trace:');
  DumpAddr(ExceptAddr);
  FrameCount:=ExceptFrameCount;
  Frames:=ExceptFrames;
  for FrameNumber := 0 to FrameCount-1 do
    DumpAddr(Frames[FrameNumber]);
end;

procedure DumpStack;
begin
  if Assigned(DebugText) then
    Dump_Stack(DebugText^, get_frame);
end;

function GetStackTrace(UseCache: boolean): string;
var
  bp: Pointer;
  addr: Pointer;
  oldbp: Pointer;
  CurAddress: Shortstring;
begin
  Result:='';
  { retrieve backtrace info }
  bp:=get_caller_frame(get_frame);
  while bp<>nil do begin
    addr:=get_caller_addr(bp);
    CurAddress:=GetLineInfo(addr,UseCache);
    //DebugLn('GetStackTrace ',CurAddress);
    Result:=Result+CurAddress+LineEnding;
    oldbp:=bp;
    bp:=get_caller_frame(bp);
    if (bp<=oldbp) or (bp>(StackBottom + StackLength)) then
      bp:=nil;
  end;
end;

procedure GetStackTracePointers(var AStack: TStackTracePointers);
var
  Depth: Integer;
  bp: Pointer;
  oldbp: Pointer;
begin
  // get stack depth
  Depth:=0;
  bp:=get_caller_frame(get_frame);
  while bp<>nil do begin
    inc(Depth);
    oldbp:=bp;
    bp:=get_caller_frame(bp);
    if (bp<=oldbp) or (bp>(StackBottom + StackLength)) then
      bp:=nil;
  end;
  SetLength(AStack,Depth);
  if Depth>0 then begin
    Depth:=0;
    bp:=get_caller_frame(get_frame);
    while bp<>nil do begin
      AStack[Depth]:=get_caller_addr(bp);
      inc(Depth);
      oldbp:=bp;
      bp:=get_caller_frame(bp);
      if (bp<=oldbp) or (bp>(StackBottom + StackLength)) then
        bp:=nil;
    end;
  end;
end;

function StackTraceAsString(const AStack: TStackTracePointers;
  UseCache: boolean): string;
var
  i: Integer;
  CurAddress: String;
begin
  Result:='';
  for i:=0 to length(AStack)-1 do begin
    CurAddress:=GetLineInfo(AStack[i],UseCache);
    Result:=Result+CurAddress+LineEnding;
  end;
end;

function GetLineInfo(Addr: Pointer; UseCache: boolean): string;
var
  ANode: TAvgLvlTreeNode;
  Item: PLineInfoCacheItem;
begin
  if UseCache then begin
    if LineInfoCache=nil then
      LineInfoCache:=TAvgLvlTree.Create(@CompareLineInfoCacheItems);
    ANode:=LineInfoCache.FindKey(Addr,@CompareAddrWithLineInfoCacheItem);
    if ANode=nil then begin
      Result:=BackTraceStrFunc(Addr);
      New(Item);
      Item^.Addr:=Addr;
      Item^.Info:=Result;
      LineInfoCache.Add(Item);
    end else begin
      Result:=PLineInfoCacheItem(ANode.Data)^.Info;
    end;
  end else
    Result:=BackTraceStrFunc(Addr);
end;

procedure MoveRect(var ARect: TRect; x, y: Integer);
begin
  inc(ARect.Right,x-ARect.Left);
  inc(ARect.Bottom,y-ARect.Top);
  ARect.Left:=x;
  ARect.Top:=y;
end;

procedure MoveRectToFit(var ARect: TRect; const MaxRect: TRect);
// move ARect, so it fits into MaxRect
// if MaxRect is too small, ARect is resized.
begin
  if ARect.Left<MaxRect.Left then begin
    // move rectangle right
    ARect.Right:=Min(ARect.Right+MaxRect.Left-ARect.Left,MaxRect.Right);
    ARect.Left:=MaxRect.Left;
  end;
  if ARect.Top<MaxRect.Top then begin
    // move rectangle down
    ARect.Bottom:=Min(ARect.Bottom+MaxRect.Top-ARect.Top,MaxRect.Bottom);
    ARect.Top:=MaxRect.Top;
  end;
  if ARect.Right>MaxRect.Right then begin
    // move rectangle left
    ARect.Left:=Max(ARect.Left-ARect.Right+MaxRect.Right,MaxRect.Left);
    ARect.Right:=MaxRect.Right;
  end;
  if ARect.Bottom>MaxRect.Bottom then begin
    // move rectangle left
    ARect.Top:=Max(ARect.Top-ARect.Bottom+MaxRect.Bottom,MaxRect.Top);
    ARect.Bottom:=MaxRect.Bottom;
  end;
end;

procedure MakeMinMax(var i1, i2: integer);
var
  h: Integer;
begin
  if i1>i2 then begin
    h:=i1;
    i1:=i2;
    i2:=h;
  end;
end;

procedure CalculateLeftTopWidthHeight(X1, Y1, X2, Y2: integer;
  var Left, Top, Width, Height: integer);
begin
  if X1 <= X2 then 
   begin
    Left := X1;
    Width := X2 - X1;
  end 
  else 
  begin
    Left := X2;
    Width := X1 - X2;
  end;
  if Y1 <= Y2 then 
  begin
    Top := Y1;
    Height := Y2 - Y1;
  end 
  else 
  begin
    Top := Y2;
    Height := Y1 - Y2;
  end;
end;

function BreakString(const s: string; MaxLineLength, Indent: integer): string;
var
  SrcLen: Integer;
  APos: Integer;
  Src: String;
  SplitPos: Integer;
  CurMaxLineLength: Integer;
begin
  Result:='';
  Src:=s;
  CurMaxLineLength:=MaxLineLength;
  if Indent>MaxLineLength-2 then Indent:=MaxLineLength-2;
  if Indent<0 then MaxLineLength:=0;
  repeat
    SrcLen:=length(Src);
    if SrcLen<=CurMaxLineLength then begin
      Result:=Result+Src;
      break;
    end;
    // split line
    SplitPos:=0;
    // search new line chars
    APos:=1;
    while (APos<=CurMaxLineLength) do begin
      if Src[APos] in [#13,#10] then begin
        SplitPos:=APos;
        break;
      end;
      inc(APos);
    end;
    // search a space boundary
    if SplitPos=0 then begin
      APos:=CurMaxLineLength;
      while APos>1 do begin
        if (Src[APos-1] in [' ',#9])
        and (not (Src[APos] in [' ',#9])) then begin
          SplitPos:=APos;
          break;
        end;
        dec(APos);
      end;
    end;
    // search a word boundary
    if SplitPos=0 then begin
      APos:=CurMaxLineLength;
      while APos>1 do begin
        if (Src[APos] in ['A'..'Z','a'..'z'])
        and (not (Src[APos-1] in ['A'..'Z','a'..'z'])) then begin
          SplitPos:=APos;
          break;
        end;
        dec(APos);
      end;
    end;
    if SplitPos=0 then begin
      // no word boundary found -> split chars
      SplitPos:=CurMaxLineLength;
    end;
    // append part and newline
    if (SplitPos<=SrcLen) and (Src[SplitPos] in [#10,#13]) then begin
      // there is already a new line char at position
      inc(SplitPos);
      if (SplitPos<=SrcLen) and (Src[SplitPos] in [#10,#13])
      and (Src[SplitPos]<>Src[SplitPos-1]) then
        inc(SplitPos);
      Result:=Result+copy(Src,1,SplitPos-1);
    end else begin
      Result:=Result+copy(Src,1,SplitPos-1)+LineEnding;
    end;
    // append indent
    if Indent>0 then
      Result:=Result+StringOfChar(' ',Indent);
    // calculate new LineLength
    CurMaxLineLength:=MaxLineLength-Indent;
    // cut string
    Src:=copy(Src,SplitPos,length(Src)-SplitPos+1);
  until false;
end;

function ComparePointers(p1, p2: Pointer): integer;
begin
  if p1>p2 then
    Result:=1
  else if p1<p2 then
    Result:=-1
  else
    Result:=0;
end;

function CompareHandles(h1, h2: THandle): integer;
begin
  if h1>h2 then
    Result:=1
  else if h1<h2 then
    Result:=-1
  else
    Result:=0;
end;

function CompareLCLHandles(h1, h2: TLCLHandle): integer;
begin
  if h1>h2 then
    Result:=1
  else if h1<h2 then
    Result:=-1
  else
    Result:=0;
end;

function CompareRect(R1, R2: PRect): Boolean;
begin
  Result:=(R1^.Left=R2^.Left) and (R1^.Top=R2^.Top) and
          (R1^.Bottom=R2^.Bottom) and (R1^.Right=R2^.Right);
  {if not Result then begin
    DebugLn(' DIFFER: ',R1^.Left,',',R1^.Top,',',R1^.Right,',',R1^.Bottom
      ,' <> ',R2^.Left,',',R2^.Top,',',R2^.Right,',',R2^.Bottom);
  end;}
end;

function ComparePoints(const p1, p2: TPoint): integer;
begin
  if p1.Y>p2.Y then
    Result:=1
  else if p1.Y<p2.Y then
    Result:=-1
  else if p1.X>p2.X then
    Result:=1
  else if p1.X<p2.X then
    Result:=-1
  else
    Result:=0;
end;

function CompareMethods(const m1, m2: TMethod): boolean;
begin
  Result:=(m1.Code=m2.Code) and (m1.Data=m2.Data);
end;

function RoundToInt(const e: Extended): integer;
begin
  Result:=integer(Round(e));
  {$IFDEF VerboseRound}
  DebugLn('RoundToInt ',e,' ',Result);
  {$ENDIF}
end;

function RoundToCardinal(const e: Extended): cardinal;
begin
  Result:=cardinal(Round(e));
  {$IFDEF VerboseRound}
  DebugLn('RoundToCardinal ',e,' ',Result);
  {$ENDIF}
end;

function TruncToInt(const e: Extended): integer;
begin
  Result:=integer(Trunc(e));
  {$IFDEF VerboseRound}
  DebugLn('TruncToInt ',e,' ',Result);
  {$ENDIF}
end;

function TruncToCardinal(const e: Extended): cardinal;
begin
  Result:=cardinal(Trunc(e));
  {$IFDEF VerboseRound}
  DebugLn('TruncToCardinal ',e,' ',Result);
  {$ENDIF}
end;

function StrToDouble(const s: string): double;
begin
  {$IFDEF VerboseRound}
  DebugLn('StrToDouble "',s,'"');
  {$ENDIF}
  Result:=Double(StrToFloat(s));
end;

procedure MergeSort(List: TFPList; const OnCompare: TListSortCompare);
var
  MergeList: PPointer;

  procedure Merge(Pos1, Pos2, Pos3: PtrInt);
  // merge two sorted arrays
  // the first array ranges Pos1..Pos2-1, the second ranges Pos2..Pos3
  var Src1Pos,Src2Pos,DestPos,cmp,a:PtrInt;
  begin
    while (Pos3>=Pos2) and (OnCompare(List[Pos2-1],List[Pos3])<=0) do
      dec(Pos3);
    if (Pos1>=Pos2) or (Pos2>Pos3) then exit;
    Src1Pos:=Pos2-1;
    Src2Pos:=Pos3;
    DestPos:=Pos3;
    while (Src2Pos>=Pos2) and (Src1Pos>=Pos1) do begin
      cmp:=OnCompare(List[Src1Pos],List[Src2Pos]);
      if cmp>0 then begin
        MergeList[DestPos]:=List[Src1Pos];
        dec(Src1Pos);
      end else begin
        MergeList[DestPos]:=List[Src2Pos];
        dec(Src2Pos);
      end;
      dec(DestPos);
    end;
    while Src2Pos>=Pos2 do begin
      MergeList[DestPos]:=List[Src2Pos];
      dec(Src2Pos);
      dec(DestPos);
    end;
    for a:=DestPos+1 to Pos3 do
      List[a]:=MergeList[a];
  end;

  procedure Sort(StartPos, EndPos: PtrInt);
  // sort an interval in List. Use MergeList as work space.
  var
    cmp, mid: integer;
    p: Pointer;
  begin
    if StartPos=EndPos then begin
    end else if StartPos+1=EndPos then begin
      cmp:=OnCompare(List[StartPos],List[EndPos]);
      if cmp>0 then begin
        p:=List[StartPos];
        List[StartPos]:=List[EndPos];
        List[EndPos]:=p;
      end;
    end else if EndPos>StartPos then begin
      mid:=(StartPos+EndPos) shr 1;
      Sort(StartPos,mid);
      Sort(mid+1,EndPos);
      Merge(StartPos,mid+1,EndPos);
    end;
  end;

begin
  if (List=nil) or (List.Count<=1) then exit;
  MergeList:=GetMem(List.Count*SizeOf(Pointer));
  Sort(0,List.Count-1);
  Freemem(MergeList);
end;

procedure MergeSort(List: TStrings; const OnCompare: TStringsSortCompare);
var
  MergeList: PAnsiString;

  procedure Merge(Pos1, Pos2, Pos3: PtrInt);
  // merge two sorted arrays
  // the first array ranges Pos1..Pos2-1, the second ranges Pos2..Pos3
  var Src1Pos,Src2Pos,DestPos,cmp,a:integer;
  begin
    while (Pos3>=Pos2) and (OnCompare(List[Pos2-1],List[Pos3])<=0) do
      dec(Pos3);
    if (Pos1>=Pos2) or (Pos2>Pos3) then exit;
    Src1Pos:=Pos2-1;
    Src2Pos:=Pos3;
    DestPos:=Pos3;
    while (Src2Pos>=Pos2) and (Src1Pos>=Pos1) do begin
      cmp:=OnCompare(List[Src1Pos],List[Src2Pos]);
      if cmp>0 then begin
        MergeList[DestPos]:=List[Src1Pos];
        dec(Src1Pos);
      end else begin
        MergeList[DestPos]:=List[Src2Pos];
        dec(Src2Pos);
      end;
      dec(DestPos);
    end;
    while Src2Pos>=Pos2 do begin
      MergeList[DestPos]:=List[Src2Pos];
      dec(Src2Pos);
      dec(DestPos);
    end;
    for a:=DestPos+1 to Pos3 do
      List[a]:=MergeList[a];
  end;

  procedure Sort(StartPos, EndPos: PtrInt);
  // sort an interval in List. Use MergeList as work space.
  var
    cmp, mid: integer;
    s: string;
  begin
    if StartPos=EndPos then begin
    end else if StartPos+1=EndPos then begin
      cmp:=OnCompare(List[StartPos],List[EndPos]);
      if cmp>0 then begin
        s:=List[StartPos];
        List[StartPos]:=List[EndPos];
        List[EndPos]:=s;
      end;
    end else if EndPos>StartPos then begin
      mid:=(StartPos+EndPos) shr 1;
      Sort(StartPos,mid);
      Sort(mid+1,EndPos);
      Merge(StartPos,mid+1,EndPos);
    end;
  end;

var
  CurSize: PtrInt;
  i: PtrInt;
begin
  if (List=nil) or (List.Count<=1) then exit;
  CurSize:=PtrInt(List.Count)*SizeOf(Pointer);
  MergeList:=GetMem(CurSize);
  FillChar(MergeList^,CurSize,0);
  Sort(0,List.Count-1);
  for i:=0 to List.Count-1 do MergeList[i]:='';
  Freemem(MergeList);
end;

procedure InitializeDebugOutput;
var
  DebugFileName: string;

  function GetDebugFileName: string;
  const
    DebugLogStart = '--debug-log=';
    DebugLogStartLength = length(DebugLogStart);
  var
    i: integer;
    EnvVarName: string;
  begin
    Result := '';
    // first try to find the log file name in the command line parameters
    for i:= 1 to Paramcount do begin
      if copy(ParamStrUTF8(i),1, DebugLogStartLength)=DebugLogStart then begin
        Result := copy(ParamStrUTF8(i), DebugLogStartLength+1,
                   Length(ParamStrUTF8(i))-DebugLogStartLength);
      end;
    end;
    // if not found yet, then try to find in the environment variables
    if (length(result)=0) then begin
      EnvVarName:= ChangeFileExt(ExtractFileName(ParamStrUTF8(0)),'') + '_debuglog';
      Result := GetEnvironmentVariableUTF8(EnvVarName);
    end;
    if (length(result)>0) then
      Result := ExpandFileNameUTF8(Result);
  end;

var
  fm: Byte;
begin
  DebugText := nil;
  DebugFileName := GetDebugFileName;
  if (length(DebugFileName)>0) and
    (DirPathExists(ExtractFileDir(DebugFileName))) then
  begin
    fm:=Filemode;
    new(DebugText);
    try
      Filemode:=fmShareDenyNone;
      Assign(DebugText^, DebugFileName);
      if FileExistsUTF8(DebugFileName) then
        Append(DebugText^)
      else
        Rewrite(DebugText^);
    except
      Freemem(DebugText);
      DebugText := nil;
      // Add extra line ending: a dialog will be shown in windows gui application
      writeln(StdOut, 'Cannot open file: ', DebugFileName+LineEnding);
    end;
    Filemode:=fm;
  end;
  if DebugText=nil then
  begin
    if TextRec(Output).Mode=fmClosed then
      DebugText := nil
    else
      DebugText := @Output;
    DebugTextAllocated := false;
  end else
    DebugTextAllocated := true;
end;

procedure CloseDebugOutput;
begin
  if DebugTextAllocated then begin
    Close(DebugText^);
    Dispose(DebugText);
    DebugTextAllocated := false;
  end;
  DebugText := nil;
end;

procedure FinalizeDebugOutput;
begin
  CloseDebugOutput;
end;

procedure DebugLn(Args: array of const);
var
  i: Integer;
begin
  for i:=Low(Args) to High(Args) do begin
    case Args[i].VType of
    vtInteger: DbgOut(dbgs(Args[i].vinteger));
    vtInt64: DbgOut(dbgs(Args[i].VInt64^));
    vtQWord: DbgOut(dbgs(Args[i].VQWord^));
    vtBoolean: DbgOut(dbgs(Args[i].vboolean));
    vtExtended: DbgOut(dbgs(Args[i].VExtended^));
{$ifdef FPC_CURRENCY_IS_INT64}
    // MWE:
    // fpc 2.x has troubles in choosing the right dbgs()
    // so we convert here
    vtCurrency: DbgOut(dbgs(int64(Args[i].vCurrency^)/10000, 4));
{$else}
    vtCurrency: DbgOut(dbgs(Args[i].vCurrency^));
{$endif}
    vtString: DbgOut(Args[i].VString^);
    vtAnsiString: DbgOut(AnsiString(Args[i].VAnsiString));
    vtChar: DbgOut(Args[i].VChar);
    vtPChar: DbgOut(Args[i].VPChar);
    vtPWideChar: DbgOut(Args[i].VPWideChar);
    vtWideChar: DbgOut(Args[i].VWideChar);
    vtWidestring: DbgOut(WideString(Args[i].VWideString));
    vtObject: DbgOut(DbgSName(Args[i].VObject));
    vtClass: DbgOut(DbgSName(Args[i].VClass));
    vtPointer: DbgOut(Dbgs(Args[i].VPointer));
    else
      DbgOut('?unknown variant?');
    end;
  end;
  DebugLn;
end;

procedure DebugLn(const S: String; Args: array of const);
begin
  DebugLn(Format(S, Args));
end;

procedure DebugLn;
begin
  DebugLn('');
end;

procedure DebugLn(const s: string);
begin
  {$ifdef WinCE}
  if DebugNestAtBOL and (s <> '') then
    DbgAppendToFile(ExtractFilePath(ParamStr(0)) + Str_LCL_Debug_File, DebugNestPrefix+s)
  else
    DbgAppendToFile(ExtractFilePath(ParamStr(0)) + Str_LCL_Debug_File, s);
  {$else}
  if not Assigned(DebugText) then exit;
  if DebugNestAtBOL and (s <> '') then
    write(DebugText^, DebugNestPrefix);
  writeln(DebugText^, ConvertLineEndings(s));
  {$endif}
  DebugNestAtBOL := True;
end;

procedure DebugLn(const s1, s2: string);
begin
  DebugLn(s1+s2);
end;

procedure DebugLn(const s1, s2, s3: string);
begin
  DebugLn(s1+s2+s3);
end;

procedure DebugLn(const s1, s2, s3, s4: string);
begin
  DebugLn(s1+s2+s3+s4);
end;

procedure DebugLn(const s1, s2, s3, s4, s5: string);
begin
  DebugLn(s1+s2+s3+s4+s5);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6: string);
begin
  DebugLn(s1+s2+s3+s4+s5+s6);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6, s7: string);
begin
  DebugLn(s1+s2+s3+s4+s5+s6+s7);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6, s7, s8: string);
begin
  DebugLn(s1+s2+s3+s4+s5+s6+s7+s8);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6, s7, s8, s9: string);
begin
  DebugLn(s1+s2+s3+s4+s5+s6+s7+s8+s9);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6, s7, s8, s9, s10: string);
begin
  DebugLn(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11: string);
begin
  DebugLn(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11,
  s12: string);
begin
  DebugLn(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12,
  s13: string);
begin
  DebugLn(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13,
  s14: string);
begin
  DebugLn(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13,
  s14, s15: string);
begin
  DebugLn(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15);
end;

procedure DebugLn(const s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13,
  s14, s15, s16: string);
begin
  DebugLn(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16);
end;

procedure DebugLnNestCreatePrefix;
const
  CurrentLen: Integer = 0;
var
  s: String;
  NewLen: Integer;
begin
  NewLen := DebugNestLvl * DebugLnNestLvlIndent;
  if NewLen < 0 then NewLen := 0;
  if (NewLen >= DebugLnMaxNestPrefixLen) then begin
    NewLen := DebugLnMaxNestPrefixLen;
    s := IntToStr(DebugNestLvl);
    if length(s)+1 > NewLen then
      NewLen := length(s)+1;
  end else
    s := '';

  if NewLen > CurrentLen then
    ReAllocMem(DebugNestPrefix, NewLen+21);
  CurrentLen := NewLen+20;

  FillChar(DebugNestPrefix^, NewLen, ' ');
  if s <> '' then
    System.Move(s[1], DebugNestPrefix[0], length(s));

  if (NewLen >= DebugLnMaxNestPrefixLen) then
    DebugNestPrefix[DebugLnMaxNestPrefixLen] := #0
  else
    DebugNestPrefix[NewLen] := #0;
end;

procedure DebugLnNestFreePrefix;
begin
  if DebugNestPrefix <> nil then
    ReAllocMem(DebugNestPrefix, 0);
end;

procedure DebugLnEnter(const s: string);
begin
  if not DebugNestAtBOL then
    DebugLn;
  if s <> '' then
    DebugLn(s);
  inc(DebugNestLvl);
  DebugLnNestCreatePrefix;
end;

procedure DebugLnEnter(Args: array of const);
begin
  if not DebugNestAtBOL then
    DebugLn;
  DebugLn(Args);
  inc(DebugNestLvl);
  DebugLnNestCreatePrefix;
end;

procedure DebugLnEnter(s: string; Args: array of const);
begin
  DebugLnEnter(Format(s, Args));
end;

procedure DebugLnEnter(const s1: string; const s2: string; const s3: string;
  const s4: string; const s5: string; const s6: string; const s7: string;
  const s8: string; const s9: string; const s10: string; const s11: string;
  const s12: string; const s13: string; const s14: string; const s15: string;
  const s16: string; const s17: string; const s18: string);
begin
  DebugLnEnter(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18);
end;

procedure DebugLnExit(const s: string);
begin
  dec(DebugNestLvl);
  if DebugNestLvl < 0 then DebugNestLvl := 0;
  DebugLnNestCreatePrefix;
  if not DebugNestAtBOL then
    DebugLn;
  if s <> '' then
    DebugLn(s);
end;

procedure DebugLnExit(Args: array of const);
begin
  dec(DebugNestLvl);
  if DebugNestLvl < 0 then DebugNestLvl := 0;
  DebugLnNestCreatePrefix;
  if not DebugNestAtBOL then
    DebugLn;
  DebugLn(Args);
end;

procedure DebugLnExit(s: string; Args: array of const);
begin
  DebugLnExit(Format(s, Args));
end;

procedure DebugLnExit(const s1: string; const s2: string; const s3: string;
  const s4: string; const s5: string; const s6: string; const s7: string;
  const s8: string; const s9: string; const s10: string; const s11: string;
  const s12: string; const s13: string; const s14: string; const s15: string;
  const s16: string; const s17: string; const s18: string);
begin
  DebugLnExit(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12+s13+s14+s15+s16+s17+s18);
end;

function ConvertLineEndings(const s: string): string;
var
  i: Integer;
  EndingStart: LongInt;
begin
  Result:=s;
  i:=1;
  while (i<=length(Result)) do begin
    if Result[i] in [#10,#13] then begin
      EndingStart:=i;
      inc(i);
      if (i<=length(Result)) and (Result[i] in [#10,#13])
      and (Result[i]<>Result[i-1]) then begin
        inc(i);
      end;
      if (length(LineEnding)<>i-EndingStart)
      or (LineEnding<>copy(Result,EndingStart,length(LineEnding))) then begin
        // line end differs => replace with current LineEnding
        Result:=
          copy(Result,1,EndingStart-1)+LineEnding+copy(Result,i,length(Result));
        i:=EndingStart+length(LineEnding);
      end;
    end else
      inc(i);
  end;
end;

procedure DbgOut(const S: String; Args: array of const);
begin
  DbgOut(Format(S, Args));
end;

procedure DBGOut(const s: string);
begin
  {$ifdef WinCE}
  if DebugNestAtBOL and (s <> '') then
    DbgAppendToFileWithoutLn(ExtractFilePath(ParamStr(0)) + Str_LCL_Debug_File, DebugNestPrefix);
  DbgAppendToFileWithoutLn(ExtractFilePath(ParamStr(0)) + Str_LCL_Debug_File, s);
  {$else}
  if Assigned(DebugText) then begin
    if DebugNestAtBOL and (s <> '') then
      write(DebugText^, DebugNestPrefix);
    write(DebugText^, s);
  end;
  {$endif}
  DebugNestAtBOL := (s = '') or (s[length(s)] in [#10,#13]);
end;

procedure DBGOut(const s1, s2: string);
begin
  DbgOut(s1+s2);
end;

procedure DbgOut(const s1, s2, s3: string);
begin
  DbgOut(s1+s2+s3);
end;

procedure DbgOut(const s1, s2, s3, s4: string);
begin
  DbgOut(s1+s2+s3+s4);
end;

procedure DbgOut(const s1, s2, s3, s4, s5: string);
begin
  DbgOut(s1+s2+s3+s4+s5);
end;

procedure DbgOut(const s1, s2, s3, s4, s5, s6: string);
begin
  DbgOut(s1+s2+s3+s4+s5+s6);
end;

procedure DbgOut(const s1, s2, s3, s4, s5, s6, s7: string);
begin
  DbgOut(s1+s2+s3+s4+s5+s6+s7);
end;

procedure DbgOut(const s1, s2, s3, s4, s5, s6, s7, s8: string);
begin
  DbgOut(s1+s2+s3+s4+s5+s6+s7+s8);
end;

procedure DbgOut(const s1, s2, s3, s4, s5, s6, s7, s8, s9: string);
begin
  DbgOut(s1+s2+s3+s4+s5+s6+s7+s8+s9);
end;

procedure DbgOut(const s1, s2, s3, s4, s5, s6, s7, s8, s9, s10: string);
begin
  DbgOut(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10);
end;

procedure DbgOut(const s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11: string);
begin
  DbgOut(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11);
end;

procedure DbgOut(const s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12: string
  );
begin
  DbgOut(s1+s2+s3+s4+s5+s6+s7+s8+s9+s10+s11+s12);
end;

function DbgS(const c: cardinal): string;
begin
  Result:=IntToStr(c);
end;

function DbgS(const i: longint): string;
begin
  Result:=IntToStr(i);
end;

function DbgS(const i: int64): string;
begin
  Result:=IntToStr(i);
end;

function DbgS(const q: qword): string;
begin
  Result:=IntToStr(q);
end;

function DbgS(const r: TRect): string;
begin
  Result:='l='+IntToStr(r.Left)+',t='+IntToStr(r.Top)
         +',r='+IntToStr(r.Right)+',b='+IntToStr(r.Bottom);
end;

function DbgS(const p: TPoint): string;
begin
  Result:='(x='+IntToStr(p.x)+',y='+IntToStr(p.y)+')';
end;

function DbgS(const p: pointer): string;
begin
  Result:=HexStr(PtrUInt(p),2*sizeof(PtrInt));
end;

function DbgS(const e: extended; MaxDecimals: integer): string;
begin
  Result:=copy(FloatToStr(e),1,MaxDecimals);
end;

function DbgS(const b: boolean): string;
begin
  if b then Result:='True' else Result:='False';
end;

function DbgS(const s: TComponentState): string;

  procedure Add(const a: string);
  begin
    if Result<>'' then
      Result:=Result+',';
    Result:=Result+a;
  end;

begin
  Result:='';
  if csLoading in s then Add('csLoading');
  if csReading in s then Add('csReading');
  if csWriting in s then Add('csWriting');
  if csDestroying in s then Add('csDestroying');
  if csDesigning in s then Add('csDesigning');
  if csAncestor in s then Add('csAncestor');
  if csUpdating in s then Add('csUpdating');
  if csFixups in s then Add('csFixups');
  if csFreeNotification in s then Add('csFreeNotification');
  if csInline in s then Add('csInline');
  if csDesignInstance in s then Add('csDesignInstance');
  Result:='['+Result+']';
end;

function DbgS(const m: TMethod): string;
var
  o: TObject;
  aMethodName: ShortString;
begin
  o:=TObject(m.Data);
  Result:=dbgsname(o)+'.'+dbgs(m.Code);
  if (o<>nil) and (m.Code<>nil) then begin
    aMethodName:=o.MethodName(m.Code);
    Result:=Result+'='''+aMethodName+'''';
  end;
end;

function DbgSName(const p: TObject): string;
begin
  if p=nil then
    Result:='nil'
  else if p is TComponent then
    Result:=TComponent(p).Name+':'+p.ClassName
  else
    Result:=p.ClassName;
end;

function DbgSName(const p: TClass): string;
begin
  if p=nil then
    Result:='nil'
  else
    Result:=p.ClassName;
end;

function DbgStr(const StringWithSpecialChars: string): string;
var
  i: Integer;
  s: String;
begin
  Result:=StringWithSpecialChars;
  i:=1;
  while (i<=length(Result)) do begin
    case Result[i] of
    ' '..#126: inc(i);
    else
      s:='#'+HexStr(ord(Result[i]),2);
      Result:=copy(Result,1,i-1)+s+copy(Result,i+1,length(Result)-i);
      inc(i,length(s));
    end;
  end;
end;

function DbgWideStr(const StringWithSpecialChars: widestring): string;
var
  s: String;
  SrcPos: Integer;
  DestPos: Integer;
  i: Integer;
begin
  SetLength(Result,length(StringWithSpecialChars));
  SrcPos:=1;
  DestPos:=1;
  while SrcPos<=length(StringWithSpecialChars) do begin
    i:=ord(StringWithSpecialChars[SrcPos]);
    case i of
    32..126:
      begin
        Result[DestPos]:=chr(i);
        inc(SrcPos);
        inc(DestPos);
      end;
    else
      s:='#'+HexStr(i,4);
      inc(SrcPos);
      Result:=copy(Result,1,DestPos-1)+s+copy(Result,DestPos+1,length(Result));
      inc(DestPos,length(s));
    end;
  end;
end;

function dbgMemRange(P: PByte; Count: integer; Width: integer): string;
const
  HexChars: array[0..15] of char = '0123456789ABCDEF';
  LineEnd: shortstring = LineEnding;
var
  i: Integer;
  NewLen: Integer;
  Dest: PChar;
  Col: Integer;
  j: Integer;
begin
  Result:='';
  if (p=nil) or (Count<=0) then exit;
  NewLen:=Count*2;
  if Width>0 then begin
    inc(NewLen,(Count div Width)*length(LineEnd));
  end;
  SetLength(Result,NewLen);
  Dest:=PChar(Result);
  Col:=1;
  for i:=0 to Count-1 do begin
    Dest^:=HexChars[PByte(P)[i] shr 4];
    inc(Dest);
    Dest^:=HexChars[PByte(P)[i] and $f];
    inc(Dest);
    inc(Col);
    if (Width>0) and (Col>Width) then begin
      Col:=1;
      for j:=1 to length(LineEnd) do begin
        Dest^:=LineEnd[j];
        inc(Dest);
      end;
    end;
  end;
end;

function dbgMemStream(MemStream: TCustomMemoryStream; Count: integer): string;
var
  s: string;
begin
  Result:='';
  if (MemStream=nil) or (not (MemStream is TCustomMemoryStream)) or (Count<=0)
  then exit;
  Count:=Min(Count,MemStream.Size);
  if Count<=0 then exit;
  SetLength(s,Count);
  Count:=MemStream.Read(s[1],Count);
  Result:=dbgMemRange(PByte(s),Count);
end;

function dbgObjMem(AnObject: TObject): string;
begin
  Result:='';
  if AnObject=nil then exit;
  Result:=dbgMemRange(PByte(AnObject),AnObject.InstanceSize);
end;

function dbghex(i: Int64): string;
const
  Hex = '0123456789ABCDEF';
var
  Negated: Boolean;
begin
  Result:='';
  if i<0 then begin
    Negated:=true;
    i:=-i;
  end else
    Negated:=false;
  repeat
    Result:=Hex[(i mod 16)+1]+Result;
    i:=i div 16;
  until i=0;
  if Negated then
    Result:='-'+Result;
end;

function DbgSWindowPosFlags(Flags: UInt): String;
begin
  Result := '';
  if (SWP_NOSIZE and Flags) <> 0 then
    Result := Result + 'SWP_NOSIZE, ';
  if (SWP_NOMOVE and Flags) <> 0 then
    Result := Result + 'SWP_NOMOVE, ';
  if (SWP_NOZORDER and Flags) <> 0 then
    Result := Result + 'SWP_NOZORDER, ';
  if (SWP_NOREDRAW and Flags) <> 0 then
    Result := Result + 'SWP_NOREDRAW, ';
  if (SWP_NOACTIVATE and Flags) <> 0 then
    Result := Result + 'SWP_NOACTIVATE, ';
  if (SWP_DRAWFRAME and Flags) <> 0 then
    Result := Result + 'SWP_DRAWFRAME, ';
  if (SWP_SHOWWINDOW and Flags) <> 0 then
    Result := Result + 'SWP_SHOWWINDOW, ';
  if (SWP_HIDEWINDOW and Flags) <> 0 then
    Result := Result + 'SWP_HIDEWINDOW, ';
  if (SWP_NOCOPYBITS and Flags) <> 0 then
    Result := Result + 'SWP_NOCOPYBITS, ';
  if (SWP_NOOWNERZORDER and Flags) <> 0 then
    Result := Result + 'SWP_NOOWNERZORDER, ';
  if (SWP_NOSENDCHANGING and Flags) <> 0 then
    Result := Result + 'SWP_NOSENDCHANGING, ';
  if (SWP_DEFERERASE and Flags) <> 0 then
    Result := Result + 'SWP_DEFERERASE, ';
  if (SWP_ASYNCWINDOWPOS and Flags) <> 0 then
    Result := Result + 'SWP_ASYNCWINDOWPOS, ';
  if (SWP_STATECHANGED and Flags) <> 0 then
    Result := Result + 'SWP_STATECHANGED, ';
  if (SWP_SourceIsInterface and Flags) <> 0 then
    Result := Result + 'SWP_SourceIsInterface, ';
  if Result <> '' then
    Delete(Result, Length(Result) - 1, 2);
end;

function DbgS(const i1, i2, i3, i4: integer): string;
begin
  Result:=dbgs(i1)+','+dbgs(i2)+','+dbgs(i3)+','+dbgs(i4);
end;

function DbgS(const Shift: TShiftState): string;

  procedure Add(const s: string);
  begin
    if Result<>'' then Result:=Result+',';
    Result:=Result+s;
  end;

begin
  Result:='';
  if ssShift in Shift then Add('ssShift');
  if ssAlt in Shift then Add('ssAlt');
  if ssCtrl in Shift then Add('ssCtrl');
  if ssLeft in Shift then Add('ssLeft');
  if ssRight in Shift then Add('ssRight');
  if ssMiddle in Shift then Add('ssMiddle');
  if ssDouble in Shift then Add('ssDouble');
  if ssMeta in Shift then Add('ssMeta');
  if ssSuper in Shift then Add('ssSuper');
  if ssHyper in Shift then Add('ssHyper');
  if ssAltGr in Shift then Add('ssAltGr');
  if ssCaps in Shift then Add('ssCaps');
  if ssNum in Shift then Add('ssNum');
  if ssScroll in Shift then Add('ssScroll');
  if ssTriple in Shift then Add('ssTriple');
  if ssQuad in Shift then Add('ssQuad');
  Result:='['+Result+']';
end;

function DbgsVKCode(c: word): string;
begin
  case c of
  VK_UNKNOWN: Result:='VK_UNKNOWN';
  VK_LBUTTON: Result:='VK_LBUTTON';
  VK_RBUTTON: Result:='VK_RBUTTON';
  VK_CANCEL: Result:='VK_CANCEL';
  VK_MBUTTON: Result:='VK_MBUTTON';
  VK_BACK: Result:='VK_BACK';
  VK_TAB: Result:='VK_TAB';
  VK_CLEAR: Result:='VK_CLEAR';
  VK_RETURN: Result:='VK_RETURN';
  VK_SHIFT: Result:='VK_SHIFT';
  VK_CONTROL: Result:='VK_CONTROL';
  VK_MENU: Result:='VK_MENU';
  VK_PAUSE: Result:='VK_PAUSE';
  VK_CAPITAL: Result:='VK_CAPITAL';
  VK_KANA: Result:='VK_KANA';
  VK_JUNJA: Result:='VK_JUNJA';
  VK_FINAL: Result:='VK_FINAL';
  VK_HANJA: Result:='VK_HANJA';
  VK_ESCAPE: Result:='VK_ESCAPE';
  VK_CONVERT: Result:='VK_CONVERT';
  VK_NONCONVERT: Result:='VK_NONCONVERT';
  VK_ACCEPT: Result:='VK_ACCEPT';
  VK_MODECHANGE: Result:='VK_MODECHANGE';
  VK_SPACE: Result:='VK_SPACE';
  VK_PRIOR: Result:='VK_PRIOR';
  VK_NEXT: Result:='VK_NEXT';
  VK_END: Result:='VK_END';
  VK_HOME: Result:='VK_HOME';
  VK_LEFT: Result:='VK_LEFT';
  VK_UP: Result:='VK_UP';
  VK_RIGHT: Result:='VK_RIGHT';
  VK_DOWN: Result:='VK_DOWN';
  VK_SELECT: Result:='VK_SELECT';
  VK_PRINT: Result:='VK_PRINT';
  VK_EXECUTE: Result:='VK_EXECUTE';
  VK_SNAPSHOT: Result:='VK_SNAPSHOT';
  VK_INSERT: Result:='VK_INSERT';
  VK_DELETE: Result:='VK_DELETE';
  VK_HELP: Result:='VK_HELP';

  VK_0: Result:='VK_0';
  VK_1: Result:='VK_1';
  VK_2: Result:='VK_2';
  VK_3: Result:='VK_3';
  VK_4: Result:='VK_4';
  VK_5: Result:='VK_5';
  VK_6: Result:='VK_6';
  VK_7: Result:='VK_7';
  VK_8: Result:='VK_8';
  VK_9: Result:='VK_9';

  VK_A: Result:='VK_A';
  VK_B: Result:='VK_B';
  VK_C: Result:='VK_C';
  VK_D: Result:='VK_D';
  VK_E: Result:='VK_E';
  VK_F: Result:='VK_F';
  VK_G: Result:='VK_G';
  VK_H: Result:='VK_H';
  VK_I: Result:='VK_I';
  VK_J: Result:='VK_J';
  VK_K: Result:='VK_K';
  VK_L: Result:='VK_L';
  VK_M: Result:='VK_M';
  VK_N: Result:='VK_N';
  VK_O: Result:='VK_O';
  VK_P: Result:='VK_P';
  VK_Q: Result:='VK_Q';
  VK_R: Result:='VK_R';
  VK_S: Result:='VK_S';
  VK_T: Result:='VK_T';
  VK_U: Result:='VK_U';
  VK_V: Result:='VK_V';
  VK_W: Result:='VK_W';
  VK_X: Result:='VK_X';
  VK_Y: Result:='VK_Y';
  VK_Z: Result:='VK_Z';

  VK_LWIN: Result:='VK_LWIN';
  VK_RWIN: Result:='VK_RWIN';
  VK_APPS: Result:='VK_APPS';
  VK_SLEEP: Result:='VK_SLEEP';

  VK_NUMPAD0: Result:='VK_NUMPAD0';
  VK_NUMPAD1: Result:='VK_NUMPAD1';
  VK_NUMPAD2: Result:='VK_NUMPAD2';
  VK_NUMPAD3: Result:='VK_NUMPAD3';
  VK_NUMPAD4: Result:='VK_NUMPAD4';
  VK_NUMPAD5: Result:='VK_NUMPAD5';
  VK_NUMPAD6: Result:='VK_NUMPAD6';
  VK_NUMPAD7: Result:='VK_NUMPAD7';
  VK_NUMPAD8: Result:='VK_NUMPAD8';
  VK_NUMPAD9: Result:='VK_NUMPAD9';
  VK_MULTIPLY: Result:='VK_MULTIPLY';
  VK_ADD: Result:='VK_ADD';
  VK_SEPARATOR: Result:='VK_SEPARATOR';
  VK_SUBTRACT: Result:='VK_SUBTRACT';
  VK_DECIMAL: Result:='VK_DECIMAL';
  VK_DIVIDE: Result:='VK_DIVIDE';
  VK_F1: Result:='VK_F1';
  VK_F2: Result:='VK_F2';
  VK_F3: Result:='VK_F3';
  VK_F4: Result:='VK_F4';
  VK_F5: Result:='VK_F5';
  VK_F6: Result:='VK_F6';
  VK_F7: Result:='VK_F7';
  VK_F8: Result:='VK_F8';
  VK_F9: Result:='VK_F9';
  VK_F10: Result:='VK_F10';
  VK_F11: Result:='VK_F11';
  VK_F12: Result:='VK_F12';
  VK_F13: Result:='VK_F13';
  VK_F14: Result:='VK_F14';
  VK_F15: Result:='VK_F15';
  VK_F16: Result:='VK_F16';
  VK_F17: Result:='VK_F17';
  VK_F18: Result:='VK_F18';
  VK_F19: Result:='VK_F19';
  VK_F20: Result:='VK_F20';
  VK_F21: Result:='VK_F21';
  VK_F22: Result:='VK_F22';
  VK_F23: Result:='VK_F23';
  VK_F24: Result:='VK_F24';

  VK_NUMLOCK: Result:='VK_NUMLOCK';
  VK_SCROLL: Result:='VK_SCROLL';

  VK_LSHIFT: Result:='VK_LSHIFT';
  VK_RSHIFT: Result:='VK_RSHIFT';
  VK_LCONTROL: Result:='VK_LCONTROL';
  VK_RCONTROL: Result:='VK_RCONTROL';
  VK_LMENU: Result:='VK_LMENU';
  VK_RMENU: Result:='VK_RMENU';

  VK_BROWSER_BACK: Result:='VK_BROWSER_BACK';
  VK_BROWSER_FORWARD: Result:='VK_BROWSER_FORWARD';
  VK_BROWSER_REFRESH: Result:='VK_BROWSER_REFRESH';
  VK_BROWSER_STOP: Result:='VK_BROWSER_STOP';
  VK_BROWSER_SEARCH: Result:='VK_BROWSER_SEARCH';
  VK_BROWSER_FAVORITES: Result:='VK_BROWSER_FAVORITES';
  VK_BROWSER_HOME: Result:='VK_BROWSER_HOME';
  VK_VOLUME_MUTE: Result:='VK_VOLUME_MUTE';
  VK_VOLUME_DOWN: Result:='VK_VOLUME_DOWN';
  VK_VOLUME_UP: Result:='VK_VOLUME_UP';
  VK_MEDIA_NEXT_TRACK: Result:='VK_MEDIA_NEXT_TRACK';
  VK_MEDIA_PREV_TRACK: Result:='VK_MEDIA_PREV_TRACK';
  VK_MEDIA_STOP: Result:='VK_MEDIA_STOP';
  VK_MEDIA_PLAY_PAUSE: Result:='VK_MEDIA_PLAY_PAUSE';
  VK_LAUNCH_MAIL: Result:='VK_LAUNCH_MAIL';
  VK_LAUNCH_MEDIA_SELECT: Result:='VK_LAUNCH_MEDIA_SELECT';
  VK_LAUNCH_APP1: Result:='VK_LAUNCH_APP1';
  VK_LAUNCH_APP2: Result:='VK_LAUNCH_APP2';
  else
    Result:='VK_('+dbgs(c)+')';
  end;
end;

function DbgS(const ASize: TSize): string;
begin
   Result := 'cx: ' + DbgS(ASize.cx) + ' cy: ' + DbgS(ASize.cy);
end;

function DbgS(const ATM: TTextMetric): string;
begin
  with ATM do
    Result :=
      'tmHeight: ' + DbgS(tmHeight) +
      ' tmAscent: ' + DbgS(tmAscent) +
      ' tmDescent: ' + DbgS(tmDescent) +
      ' tmInternalLeading: ' + DbgS(tmInternalLeading) +
      ' tmExternalLeading: ' + DbgS(tmExternalLeading) +
      ' tmAveCharWidth: ' + DbgS(tmAveCharWidth) +
      ' tmMaxCharWidth: ' + DbgS(tmMaxCharWidth) +
      ' tmWeight: ' + DbgS(tmWeight) +
      ' tmOverhang: ' + DbgS(tmOverhang) +
      ' tmDigitizedAspectX: ' + DbgS(tmDigitizedAspectX) +
      ' tmDigitizedAspectY: ' + DbgS(tmDigitizedAspectY) +
      ' tmFirstChar: ' + tmFirstChar +
      ' tmLastChar: ' + tmLastChar +
      ' tmDefaultChar: ' + tmDefaultChar +
      ' tmBreakChar: ' + tmBreakChar +
      ' tmItalic: ' + DbgS(tmItalic) +
      ' tmUnderlined: ' + DbgS(tmUnderlined) +
      ' tmStruckOut: ' + DbgS(tmStruckOut) +
      ' tmPitchAndFamily: ' + DbgS(tmPitchAndFamily) +
      ' tmCharSet: ' + DbgS(tmCharSet);
end;

function DbgS(const AScrollInfo: TScrollInfo): string;
begin
  Result := '';

  if (SIF_POS and AScrollInfo.fMask) > 0 then
    Result := 'Pos: ' + DbgS(AScrollInfo.nPos);
  if (SIF_RANGE and AScrollInfo.fMask) > 0 then
    Result := Result + ' Min: ' + DbgS(AScrollInfo.nMin) + ' Max: ' +
      DbgS(AScrollInfo.nMax);
  if (SIF_PAGE and AScrollInfo.fMask) > 0 then
    Result := Result + ' Page: ' + DbgS(AScrollInfo.nPage);
  if (SIF_TRACKPOS and AScrollInfo.fMask) > 0 then
    Result := Result + ' TrackPos: ' + DbgS(AScrollInfo.nTrackPos);

  if Result = '' then Result := '(no scrollinfo)';
end;

procedure DbgOutThreadLog(const Msg: string);
var
  PID: PtrInt;
  fs: TFileStream;
  Filename: string;
begin
  PID:=PtrInt(GetThreadID);
  Filename:='Log'+IntToStr(PID);
  if FileExistsUTF8(Filename) then
    fs:=TFileStream.Create(UTF8ToSys(Filename),fmOpenWrite)
  else
    fs:=TFileStream.Create(UTF8ToSys(Filename),fmCreate);
  fs.Position:=fs.Size;
  fs.Write(Msg[1], length(Msg));
  fs.Free;
end;

procedure DebuglnThreadLog(const Msg: string);
var
  PID: PtrInt;
begin
  PID:=PtrInt(GetThreadID);
  DbgOutThreadLog(IntToStr(PtrInt(PID))+' : '+Msg+LineEnding);
end;

procedure DebuglnThreadLog(Args: array of const);
var
  i: Integer;
  s: String;
begin
  s:='';
  for i:=Low(Args) to High(Args) do begin
    case Args[i].VType of
    vtInteger: s:=s+dbgs(Args[i].vinteger);
    vtInt64: s:=s+dbgs(Args[i].VInt64^);
    vtQWord: s:=s+dbgs(Args[i].VQWord^);
    vtBoolean: s:=s+dbgs(Args[i].vboolean);
    vtExtended: s:=s+dbgs(Args[i].VExtended^);
{$ifdef FPC_CURRENCY_IS_INT64}
    // MWE:
    // ppcppc 2.0.2 has troubles in choosing the right dbgs()
    // so we convert here (i don't know about other versions
    vtCurrency: s:=s+dbgs(int64(Args[i].vCurrency^)/10000, 4);
{$else}
    vtCurrency: s:=s+dbgs(Args[i].vCurrency^);
{$endif}
    vtString: s:=s+Args[i].VString^;
    vtAnsiString: s:=s+AnsiString(Args[i].VAnsiString);
    vtChar: s:=s+Args[i].VChar;
    vtPChar: s:=s+Args[i].VPChar;
    vtPWideChar: s:=s+Args[i].VPWideChar;
    vtWideChar: s:=s+Args[i].VWideChar;
    vtWidestring: s:=s+WideString(Args[i].VWideString);
    vtObject: s:=s+DbgSName(Args[i].VObject);
    vtClass: s:=s+DbgSName(Args[i].VClass);
    vtPointer: s:=s+Dbgs(Args[i].VPointer);
    else
      DbgOutThreadLog('?unknown variant?');
    end;
  end;
  DebuglnThreadLog(s);
end;

procedure DebuglnThreadLog;
begin
  DebuglnThreadLog('');
end;

procedure DbgSaveData(FileName: String; AData: PChar; ADataSize: PtrUInt);
var
  S: TStream;
begin
  S := TFileStream.Create(UTF8ToSys(FileName), fmCreate);
  S.Write(AData^, ADataSize);
  S.Free;
end;

procedure DbgAppendToFile(FileName, S: String);
var
  F: TextFile;
begin
  AssignFile(F, FileName);
  {$I-}
  Append(F);
  if IOResult <> 0 then
    Rewrite(F);
  {$I+}
  WriteLn(F, S);
  CloseFile(F);
end;

procedure DbgAppendToFileWithoutLn(FileName, S: String);
var
  F: TextFile;
begin
  AssignFile(F, FileName);
  {$I-}
  Append(F);
  if IOResult <> 0 then
    Rewrite(F);
  {$I+}
  Write(F, S);
  CloseFile(F);
end;

function StripLN(const ALine: String): String;
var
  idx: Integer;
begin
  Result := ALine;
  idx := Pos(#10, Result);
  if idx = 0
  then begin
    idx := Pos(#13, Result);
    if idx = 0 then Exit;
  end
  else begin
    if (idx > 1)
    and (Result[idx - 1] = #13)
    then Dec(idx);
  end;
  SetLength(Result, idx - 1);
end;

function GetPart(const ASkipTo, AnEnd: String; var ASource: String;
  const AnIgnoreCase, AnUpdateSource: Boolean): String;
begin
  Result := GetPart([ASkipTo], [AnEnd], ASource, AnIgnoreCase, AnUpdateSource);
end;

function GetPart(const ASkipTo, AnEnd: array of String; var ASource: String;
  const AnIgnoreCase: Boolean = False; const AnUpdateSource: Boolean = True): String;
var
  n, i, idx: Integer;
  S, Source, Match: String;
  HasEscape: Boolean;
begin
  Source := ASource;

  if High(ASkipTo) >= 0
  then begin
    idx := 0;
    HasEscape := False;
    if AnIgnoreCase
    then S := UpperCase(Source)
    else S := Source;
    for n := Low(ASkipTo) to High(ASkipTo) do
    begin
      if ASkipTo[n] = ''
      then begin
        HasEscape := True;
        Continue;
      end;
      if AnIgnoreCase
      then i := Pos(UpperCase(ASkipTo[n]), S)
      else i := Pos(ASkipTo[n], S);
      if i > idx
      then begin
        idx := i;
        Match := ASkipTo[n];
      end;
    end;
    if (idx = 0) and not HasEscape
    then begin
      Result := '';
      Exit;
    end;
    if idx > 0
    then Delete(Source, 1, idx + Length(Match) - 1);
  end;

  if AnIgnoreCase
  then S := UpperCase(Source)
  else S := Source;
  idx := MaxInt;
  for n := Low(AnEnd) to High(AnEnd) do
  begin
    if AnEnd[n] = '' then Continue;
    if AnIgnoreCase
    then i := Pos(UpperCase(AnEnd[n]), S)
    else i := Pos(AnEnd[n], S);
    if (i > 0) and (i < idx) then idx := i;
  end;

  if idx = MaxInt
  then begin
    Result := Source;
    Source := '';
  end
  else begin
    Result := Copy(Source, 1, idx - 1);
    Delete(Source, 1, idx - 1);
  end;

  if AnUpdateSource
  then ASource := Source;
end;

{
  Ensures the covenient look of multiline string
  when displaying it in the single line
  * Replaces CR and LF with spaces
  * Removes duplicate spaces
}
function TextToSingleLine(const AText: string): string;
var
  str: string;
  i, wstart, wlen: Integer;
begin
  str := Trim(AText);
  wstart := 0;
  wlen := 0;
  i := 1;
  while i < Length(str) - 1 do
  begin
    if (str[i] in [' ', #13, #10]) then
    begin
      if (wstart = 0) then
      begin
        wstart := i;
        wlen := 1;
      end else
        Inc(wlen);
    end else
    begin
      if wstart > 0 then
      begin
        str[wstart] := ' ';
        Delete(str, wstart+1, wlen-1);
        Dec(i, wlen-1);
        wstart := 0;
      end;
    end;
    Inc(i);
  end;
  Result := str;
end;

function StringCase(const AString: String; const ACase: array of String {; const AIgnoreCase = False, APartial = false: Boolean}): Integer;
begin
  Result := StringCase(AString, ACase, False, False);
end;

function StringCase(const AString: String; const ACase: array of String; const AIgnoreCase, APartial: Boolean): Integer;
var
  Search, S: String;
begin
  if High(ACase) = -1
  then begin
    Result := -1;
    Exit;
  end;

  if AIgnoreCase
  then Search := UpperCase(AString)
  else Search := AString;

  for Result := Low(ACase) to High(ACase) do
  begin
    if AIgnoreCase
    then S := UpperCase(ACase[Result])
    else S := ACase[Result];

    if Search = S then Exit;
    if not APartial then Continue;
    if Length(Search) >= Length(S) then Continue;
    if StrLComp(PChar(Search), PChar(S), Length(Search)) = 0 then Exit;
  end;

  Result := -1;
end;

function ClassCase(const AClass: TClass; const ACase: array of TClass {; const ADecendant: Boolean = True}): Integer;
begin
  Result := ClassCase(AClass, ACase, True);
end;

function ClassCase(const AClass: TClass; const ACase: array of TClass; const ADecendant: Boolean): Integer;
begin
  for Result := Low(ACase) to High(ACase) do
  begin
    if AClass = ACase[Result] then Exit;
    if not ADecendant then Continue;
    if AClass.InheritsFrom(ACase[Result]) then Exit;
  end;

  Result := -1;
end;

function UTF8CharacterLength(p: PChar): integer;
begin
  if p<>nil then begin
    if ord(p^)<%11000000 then begin
      // regular single byte character (#0 is a character, this is pascal ;)
      Result:=1;
    end
    else if ((ord(p^) and %11100000) = %11000000) then begin
      // could be 2 byte character
      if (ord(p[1]) and %11000000) = %10000000 then
        Result:=2
      else
        Result:=1;
    end
    else if ((ord(p^) and %11110000) = %11100000) then begin
      // could be 3 byte character
      if ((ord(p[1]) and %11000000) = %10000000)
      and ((ord(p[2]) and %11000000) = %10000000) then
        Result:=3
      else
        Result:=1;
    end
    else if ((ord(p^) and %11111000) = %11110000) then begin
      // could be 4 byte character
      if ((ord(p[1]) and %11000000) = %10000000)
      and ((ord(p[2]) and %11000000) = %10000000)
      and ((ord(p[3]) and %11000000) = %10000000) then
        Result:=4
      else
        Result:=1;
    end
    else
      Result:=1
  end else
    Result:=0;
end;

function UTF8Length(const s: string): PtrInt;
begin
  Result:=UTF8Length(PChar(s),length(s));
end;

function UTF8Length(p: PChar; ByteCount: PtrInt): PtrInt;
var
  CharLen: LongInt;
begin
  Result:=0;
  while (ByteCount>0) do begin
    inc(Result);
    CharLen:=UTF8CharacterLength(p);
    inc(p,CharLen);
    dec(ByteCount,CharLen);
  end;
end;

function UTF8CharacterToUnicode(p: PChar; out CharLen: integer): Cardinal;
begin
  if p<>nil then begin
    if ord(p^)<%11000000 then begin
      // regular single byte character (#0 is a normal char, this is pascal ;)
      Result:=ord(p^);
      CharLen:=1;
    end
    else if ((ord(p^) and %11100000) = %11000000) then begin
      // could be double byte character
      if (ord(p[1]) and %11000000) = %10000000 then begin
        Result:=((ord(p^) and %00011111) shl 6)
                or (ord(p[1]) and %00111111);
        CharLen:=2;
      end else begin
        Result:=ord(p^);
        CharLen:=1;
      end;
    end
    else if ((ord(p^) and %11110000) = %11100000) then begin
      // could be triple byte character
      if ((ord(p[1]) and %11000000) = %10000000)
      and ((ord(p[2]) and %11000000) = %10000000) then begin
        Result:=((ord(p^) and %00011111) shl 12)
                or ((ord(p[1]) and %00111111) shl 6)
                or (ord(p[2]) and %00111111);
        CharLen:=3;
      end else begin
        Result:=ord(p^);
        CharLen:=1;
      end;
    end
    else if ((ord(p^) and %11111000) = %11110000) then begin
      // could be 4 byte character
      if ((ord(p[1]) and %11000000) = %10000000)
      and ((ord(p[2]) and %11000000) = %10000000)
      and ((ord(p[3]) and %11000000) = %10000000) then begin
        Result:=((ord(p^) and %00001111) shl 18)
                or ((ord(p[1]) and %00111111) shl 12)
                or ((ord(p[2]) and %00111111) shl 6)
                or (ord(p[3]) and %00111111);
        CharLen:=4;
      end else begin
        Result:=ord(p^);
        CharLen:=1;
      end;
    end
    else begin
      // invalid character
      Result:=ord(p^);
      CharLen:=1;
    end;
  end else begin
    Result:=0;
    CharLen:=0;
  end;
end;

function UnicodeToUTF8(u: cardinal; Buf: PChar): integer;

  procedure RaiseInvalidUnicode;
  begin
    raise Exception.Create('UnicodeToUTF8: invalid unicode: '+IntToStr(u));
  end;

begin
  Result:=UnicodeToUTF8SkipErrors(u,Buf);
  if Result=0 then
    RaiseInvalidUnicode;
end;

function UnicodeToUTF8SkipErrors(u: cardinal; Buf: PChar): integer;
begin
  case u of
    0..$7f:
      begin
        Result:=1;
        Buf[0]:=char(byte(u));
      end;
    $80..$7ff:
      begin
        Result:=2;
        Buf[0]:=char(byte($c0 or (u shr 6)));
        Buf[1]:=char(byte($80 or (u and $3f)));
      end;
    $800..$ffff:
      begin
        Result:=3;
        Buf[0]:=char(byte($e0 or (u shr 12)));
        Buf[1]:=char(byte((u shr 6) and $3f) or $80);
        Buf[2]:=char(byte(u and $3f) or $80);
      end;
    $10000..$10ffff:
      begin
        Result:=4;
        Buf[0]:=char(byte($f0 or (u shr 18)));
        Buf[1]:=char(byte((u shr 12) and $3f) or $80);
        Buf[2]:=char(byte((u shr 6) and $3f) or $80);
        Buf[3]:=char(byte(u and $3f) or $80);
      end;
  else
    Result:=0;
  end;
end;

function UnicodeToUTF8(u: cardinal): shortstring;
begin
  Result[0]:=chr(UnicodeToUTF8(u,@Result[1]));
end;

function UTF8ToDoubleByteString(const s: string): string;
var
  Len: Integer;
begin
  Len:=UTF8Length(s);
  SetLength(Result,Len*2);
  if Len=0 then exit;
  UTF8ToDoubleByte(PChar(s),length(s),PByte(Result));
end;

{ returns number of double bytes }
function UTF8ToDoubleByte(UTF8Str: PChar; Len: PtrInt; DBStr: PByte): PtrInt;
var
  SrcPos: PChar;
  CharLen: LongInt;
  DestPos: PByte;
  u: Cardinal;
begin
  SrcPos:=UTF8Str;
  DestPos:=DBStr;
  Result:=0;
  while Len>0 do begin
    u:=UTF8CharacterToUnicode(SrcPos,CharLen);
    DestPos^:=byte((u shr 8) and $ff);
    inc(DestPos);
    DestPos^:=byte(u and $ff);
    inc(DestPos);
    inc(SrcPos,CharLen);
    dec(Len,CharLen);
    inc(Result);
  end;
end;

{ Find the start of the UTF8 character which contains BytePos,
  Len is length in byte, BytePos starts at 0 }
function UTF8FindNearestCharStart(UTF8Str: PChar; Len: integer;
  BytePos: integer): integer;
begin
  Result:=0;
  if (UTF8Str<>nil) and (Len>0) and (BytePos>=0) then begin
    Result:=BytePos;
    if Result>Len then Result:=Len-1;
    if (Result>0) and (ord(UTF8Str[Result]) and %11000000=%10000000) then begin
      dec(Result);
      if (Result>0) and (ord(UTF8Str[Result]) and %11000000=%10000000) then begin
        dec(Result);
        if (Result>0) and (ord(UTF8Str[Result]) and %11000000=%10000000) then begin
          dec(Result);
          // should be four byte character
          if (ord(UTF8Str[Result]) and %11111000<>%11110000) then begin
            // broken UTF8 character
            inc(Result,3);
          end else begin
            // is four byte character
          end;
        end else if (ord(UTF8Str[Result]) and %11110000<>%11100000) then begin
          // broken UTF8 character, should be three byte
          inc(Result,2);
        end else
        begin
          // is three byte character
        end;
      end else if (ord(UTF8Str[Result]) and %11100000<>%11000000) then begin
        // broken UTF8 character, should be two byte
        inc(Result);
      end else
      begin
        // is two byte character
      end;
    end;
  end;
end;

{ Len is the length in bytes of UTF8Str
  CharIndex is the position of the desired char (starting at 0), in chars

  This function is similar to UTF8FindNearestCharStart
}
function UTF8CharStart(UTF8Str: PChar; Len, CharIndex: PtrInt): PChar;
var
  CharLen: LongInt;
begin
  Result:=UTF8Str;
  if Result<>nil then begin
    while (CharIndex>0) and (Len>0) do begin
      CharLen:=UTF8CharacterLength(Result);
      dec(Len,CharLen);
      dec(CharIndex);
      inc(Result,CharLen);
    end;
    if (CharIndex>0) or (Len<0) then
      Result:=nil;
  end;
end;

function UTF8CharToByteIndex(UTF8Str: PChar; Len, CharIndex: PtrInt): PtrInt;
var
  p: PChar;
begin
  p := UTF8CharStart(UTF8Str, Len, CharIndex);
  if p = nil
  then Result := -1
  else Result := p - UTF8Str;
end;

{ fix any broken UTF8 sequences with spaces }
procedure UTF8FixBroken(P: PChar);
begin
  if p=nil then exit;
  while p^<>#0 do begin
    if ord(p^)<%10000000 then begin
      // regular single byte character
      inc(p);
    end
    else if ord(p^)<%11000000 then begin
      // invalid
      p^:=' ';
      inc(p);
    end
    else if ((ord(p^) and %11100000) = %11000000) then begin
      // should be 2 byte character
      if (ord(p[1]) and %11000000) = %10000000 then
        inc(p,2)
      else if p[1]<>#0 then
        p^:=' ';
    end
    else if ((ord(p^) and %11110000) = %11100000) then begin
      // should be 3 byte character
      if ((ord(p[1]) and %11000000) = %10000000)
      and ((ord(p[2]) and %11000000) = %10000000) then
        inc(p,3)
      else
        p^:=' ';
    end
    else if ((ord(p^) and %11111000) = %11110000) then begin
      // should be 4 byte character
      if ((ord(p[1]) and %11000000) = %10000000)
      and ((ord(p[2]) and %11000000) = %10000000)
      and ((ord(p[3]) and %11000000) = %10000000) then
        inc(p,4)
      else
        p^:=' ';
    end
    else begin
      p^:=' ';
      inc(p);
    end;
  end;
end;

function UTF8CharacterStrictLength(P: PChar): integer;
begin
  if p=nil then exit(0);
  if ord(p^)<%10000000 then begin
    // regular single byte character
    exit(1);
  end
  else if ord(p^)<%11000000 then begin
    // invalid single byte character
    exit(0);
  end
  else if ((ord(p^) and %11100000) = %11000000) then begin
    // should be 2 byte character
    if (ord(p[1]) and %11000000) = %10000000 then
      exit(2)
    else
      exit(0);
  end
  else if ((ord(p^) and %11110000) = %11100000) then begin
    // should be 3 byte character
    if ((ord(p[1]) and %11000000) = %10000000)
    and ((ord(p[2]) and %11000000) = %10000000) then
      exit(3)
    else
      exit(0);
  end
  else if ((ord(p^) and %11111000) = %11110000) then begin
    // should be 4 byte character
    if ((ord(p[1]) and %11000000) = %10000000)
    and ((ord(p[2]) and %11000000) = %10000000)
    and ((ord(p[3]) and %11000000) = %10000000) then
      exit(4)
    else
      exit(0);
  end else
    exit(0);
end;

function UTF8CStringToUTF8String(SourceStart: PChar; SourceLen: PtrInt) : string;
var
  Source: PChar;
  Dest: PChar;
  SourceEnd: PChar;
  CharLen: integer;
  SourceCopied: PChar;

  // Copies from SourceStart till Source to Dest and updates Dest
  procedure CopyPart; inline;
  var
    CopyLength: SizeInt;
  begin
    CopyLength := Source - SourceCopied;
    if CopyLength=0 then exit;
    System.move(SourceCopied^ , Dest^, CopyLength);
    SourceCopied:=Source;
    inc(Dest, CopyLength);
  end;

begin
  SetLength(Result, SourceLen);
  if SourceLen=0 then exit;
  SourceCopied:=SourceStart;
  Source:=SourceStart;
  Dest:=PChar(Result);
  SourceEnd := Source + SourceLen;
  while Source<SourceEnd do begin
    CharLen := UTF8CharacterLength(Source);
    if (CharLen=1) and (Source^='\') then begin
      CopyPart;
      inc(Source);
      if Source^ in ['t', 'n', '"', '\'] then begin
        case Source^ of
         't' : Dest^ := #9;
         '"' : Dest^ := '"';
         '\' : Dest^ := '\';
         'n' :
         // fpc 2.1.1 stores string constants as array of char so maybe this
         // will work for without ifdef (once available in 2.0.x too):
         // move(lineending, dest^, sizeof(LineEnding));
{$IFDEF WINDOWS}
               begin
                 move(lineending[1], dest^, length(LineEnding));
                 inc(dest, length(LineEnding)-1);
               end;
{$ELSE}
               Dest^ := LineEnding;
{$ENDIF}
        end;
        inc(Source);
        inc(Dest);
      end;
      SourceCopied := Source;
    end
    else
      Inc(Source, CharLen);
  end;
  CopyPart;
  SetLength(Result, Dest - PChar(Result));
end;

function UTF8Pos(const SearchForText, SearchInText: string): PtrInt;
// returns the character index, where the SearchForText starts in SearchInText
var
  p: LongInt;
begin
  p:=System.Pos(SearchForText,SearchInText);
  if p>0 then
    Result:=UTF8Length(PChar(SearchInText),p-1)+1
  else
    Result:=0;
end;

function UTF8Copy(const s: string; StartCharIndex, CharCount: PtrInt): string;
// returns substring
var
  StartBytePos: PChar;
  EndBytePos: PChar;
  MaxBytes: PtrInt;
begin
  StartBytePos:=UTF8CharStart(PChar(s),length(s),StartCharIndex-1);
  if StartBytePos=nil then
    Result:=''
  else begin
    MaxBytes:=PtrInt(PChar(s)+length(s)-StartBytePos);
    EndBytePos:=UTF8CharStart(StartBytePos,MaxBytes,CharCount);
    if EndBytePos=nil then
      Result:=copy(s,StartBytePos-PChar(s)+1,MaxBytes)
    else
      Result:=copy(s,StartBytePos-PChar(s)+1,EndBytePos-StartBytePos);
  end;
end;

procedure UTF8Delete(var s: String; StartCharIndex, CharCount: PtrInt);
var
  StartBytePos: PChar;
  EndBytePos: PChar;
  MaxBytes: PtrInt;
begin
  StartBytePos:=UTF8CharStart(PChar(s),length(s),StartCharIndex-1);
  if StartBytePos <> nil then
  begin
    MaxBytes:=PtrInt(PChar(s)+length(s)-StartBytePos);
    EndBytePos:=UTF8CharStart(StartBytePos,MaxBytes,CharCount);
    if EndBytePos=nil then
      Delete(s,StartBytePos-PChar(s)+1,MaxBytes)
    else
      Delete(s,StartBytePos-PChar(s)+1,EndBytePos-StartBytePos);
  end;
end;

procedure UTF8Insert(const source: String; var s: string; StartCharIndex: PtrInt);
var
  StartBytePos: PChar;
begin
  StartBytePos:=UTF8CharStart(PChar(s),length(s),StartCharIndex-1);
  if StartBytePos <> nil then
    Insert(source, s, StartBytePos-PChar(s)+1);
end;

function UTF8LowerCase(const s: String): String;
begin
  Result := UTF8Encode(WideLowerCase(UTF8Decode(s)));
end;

function UTF8UpperCase(const s: String): String;
begin
  Result := UTF8Encode(WideUpperCase(UTF8Decode(s)));
end;

{$ifdef NewLowerCase}
function UnicodeLowercase(u: cardinal): cardinal;
begin
  if u<$00C0 then begin
    // most common
    if (u>=$0041) and (u<=$0061) then
      Result:=u+32
    else
      Result:=u;
  end else
    case u of
    $00C0..$00DE: Result:=UnicodeLower00C0_00DE[u];
    $0100..$024E: Result:=UnicodeLower0100_024E[u];
    $0386..$03AB: Result:=UnicodeLower0386_03AB[u];
    $03D8..$042F: Result:=UnicodeLower03D8_042F[u];
    $0460..$0512: Result:=UnicodeLower0460_0512[u];
    $0531..$0556: Result:=u+48;
    $10A0..$10C5: Result:=u+7264;
    $1E00..$1FFC: Result:=UnicodeLower1E00_1FFC[u];
    $2126..$2183: Result:=UnicodeLower2126_2183[u];
    $24B6..$24CF: Result:=u+26;
    $2C00..$2C2E: Result:=u+48;
    $2C60..$2CE2: Result:=UnicodeLower2C60_2CE2[u];
    $FF21..$FF3A: Result:=u+32;
    else          Result:=u;
  end;
end;

function UTF8LowercaseDynLength(const s: string): string;
var
  Buf: shortstring;
  SrcPos: PtrInt;
  DstPos: PtrInt;
  CharLen: integer;
  OldCode: LongWord;
  NewCode: LongWord;
begin
  // first compute needed length
  SrcPos:=1;
  DstPos:=1;
  while SrcPos<=length(s) do begin
    case s[SrcPos] of
    #192..#240:
      begin
        OldCode:=UTF8CharacterToUnicode(@s[SrcPos],CharLen);
        NewCode:=UnicodeLowercase(OldCode);
        if NewCode=OldCode then begin
          inc(DstPos,CharLen);
        end else begin
          inc(DstPos,UnicodeToUTF8(NewCode,@Buf[1]));
        end;
        inc(SrcPos,CharLen);
      end;
    else
      inc(SrcPos);
      inc(DstPos);
    end;
  end;
  SetLength(Result,DstPos-1);
  if Result='' then exit;
  // create the new string
  SrcPos:=1;
  DstPos:=1;
  while SrcPos<=length(s) do begin
    case s[SrcPos] of
    #192..#240:
      begin
        OldCode:=UTF8CharacterToUnicode(@s[SrcPos],CharLen);
        NewCode:=UnicodeLowercase(OldCode);
        if NewCode=OldCode then begin
          System.Move(s[SrcPos],Result[DstPos],CharLen);
          inc(DstPos,CharLen);
        end else begin
          inc(DstPos,UnicodeToUTF8(NewCode,@Result[DstPos]));
        end;
        inc(SrcPos,CharLen);
      end;
    else
      Result[DstPos]:=s[SrcPos];
      inc(SrcPos);
      inc(DstPos);
    end;
  end;
end;

function UTF8LowerCaseNew(const s: string): string;
var
  i: PtrInt;
  CharLen: integer;
  OldCode: LongWord;
  NewCode: LongWord;
  NewCharLen: integer;
begin
  Result:=s;
  i:=1;
  while i<=length(Result) do begin
    case Result[i] of
    { First ASCII chars }
    'A'..'Z':
      begin
        Result[i]:=chr(ord(Result[i])+32);
        inc(i);
      end;
    { Now chars with multiple bytes }
    #192..#240:
      begin
        OldCode:=UTF8CharacterToUnicode(@Result[i],CharLen);
        NewCode:=UnicodeLowercase(OldCode);
        if NewCode=OldCode then begin
          inc(i,CharLen);
        end else begin
          UniqueString(Result);
          NewCharLen:=UnicodeToUTF8(NewCode,@Result[i]);
          if CharLen=NewCharLen then begin
            inc(i,NewCharLen);
          end else begin
            // string size changed => use slower function
            Result:=UTF8LowercaseDynLength(s);
            exit;
          end;
        end;
      end;
    else
      inc(i);
    end;
  end;
end;
{$endif NewLowerCase}

function FindInvalidUTF8Character(p: PChar; Count: PtrInt;
  StopOnNonASCII: Boolean): PtrInt;
// return -1 if ok
var
  CharLen: Integer;
begin
  if (p<>nil) then begin
    Result:=0;
    while Result<Count do begin
      if ord(p^)<128 then begin
        // regular single byte ASCII character (#0 is a character, this is pascal ;)
        CharLen:=1;
      end
      else if ord(p^)<%11000000 then begin
        // regular single byte character
        if StopOnNonASCII then
          exit;
        CharLen:=1;
      end
      else if ((ord(p^) and %11100000) = %11000000) then begin
        // could be 2 byte character
        if (Result<Count-1) and ((ord(p[1]) and %11000000) = %10000000) then
          CharLen:=2
        else
          exit; // missing following bytes
      end
      else if ((ord(p^) and %11110000) = %11100000) then begin
        // could be 3 byte character
        if (Result<Count-2) and ((ord(p[1]) and %11000000) = %10000000)
        and ((ord(p[2]) and %11000000) = %10000000) then
          CharLen:=3
        else
          exit; // missing following bytes
      end
      else if ((ord(p^) and %11111000) = %11110000) then begin
        // could be 4 byte character
        if (Result<Count-3) and ((ord(p[1]) and %11000000) = %10000000)
        and ((ord(p[2]) and %11000000) = %10000000)
        and ((ord(p[3]) and %11000000) = %10000000) then
          CharLen:=4
        else
          exit; // missing following bytes
      end
      else begin
        if StopOnNonASCII then
          exit;
        CharLen:=1;
      end;
      inc(Result,CharLen);
      inc(p,CharLen);
      if Result>Count then begin
        dec(Result,CharLen);
        exit; // missing following bytes
      end;
    end;
  end;
  // ok
  Result:=-1;
end;

function ValidUTF8String(const s: String): String;
var
  p, cur: PChar;
  l, lr: integer;
  NeedFree: Boolean;
begin
  if FindInvalidUTF8Character(PChar(s), Length(s)) <> -1 then
  begin
    NeedFree := True;
    GetMem(p, Length(s) + 1);
    StrPCopy(p, s);
    UTF8FixBroken(p);
  end
  else
  begin
    p := PChar(s);
    NeedFree := False;
  end;

  Result := '';
  cur := p;
  while cur^ <> #0 do
  begin
    l := UTF8CharacterLength(cur);
    if (l = 1) and (cur^ < #32) then
      Result := Result + '#' + IntToStr(Ord(cur^))
    else
    begin
      lr := Length(Result);
      SetLength(Result, lr + l);
      System.Move(cur^, Result[lr + 1], l);
    end;
    inc(cur, l)
  end;

  if NeedFree then
    FreeMem(p);
end;

procedure AssignUTF8ListToAnsi(UTF8List, AnsiList: TStrings);
var
  i: Integer;
begin
  AnsiList.Clear;
  if UTF8List=nil then exit;
  for i:=0 to UTF8List.Count-1 do
    AnsiList.Add(UTF8ToSys(UTF8List[i]));
end;

function UTF16CharacterLength(p: PWideChar): integer;
// returns length of UTF16 character in number of words
// The endianess of the machine will be taken.
begin
  if p<>nil then begin
    if (ord(p[0]) < $D800) or (ord(p[0]) > $DFFF) then
      Result:=1
    else
      Result:=2;
  end else begin
    Result:=0;
  end;
end;

function UTF16Length(const s: widestring): PtrInt;
begin
  Result:=UTF16Length(PWideChar(s),length(s));
end;

function UTF16Length(p: PWideChar; WordCount: PtrInt): PtrInt;
var
  CharLen: LongInt;
begin
  Result:=0;
  while (WordCount>0) do begin
    inc(Result);
    CharLen:=UTF16CharacterLength(p);
    inc(p,CharLen);
    dec(WordCount,CharLen);
  end;
end;

function UTF16CharacterToUnicode(p: PWideChar; out CharLen: integer): Cardinal;
var
  w1: cardinal;
  w2: Cardinal;
begin
  if p<>nil then begin
    w1:=ord(p[0]);
    if (w1 < $D800) or (w1 > $DFFF) then begin
      // is 1 word character
      Result:=w1;
      CharLen:=1;
    end else begin
      // could be 2 word character
      w2:=ord(p[1]);
      if (w2>=$DC00) then begin
        // is 2 word character
        Result:=(w1-$D800) shl 10 + (w2-$DC00) + $10000;
        CharLen:=2;
      end else begin
        // invalid character
        Result:=w1;
        CharLen:=1;
      end;
    end;
  end else begin
    Result:=0;
    CharLen:=0;
  end;
end;

function UnicodeToUTF16(u: cardinal): widestring;
begin
  // u should be <= $10FFFF to fit into UTF-16

  if u < $10000 then
    // Note: codepoints $D800 - $DFFF are reserved
    Result:=system.widechar(u)
  else
    Result:=system.widechar($D800+((u - $10000) shr 10))+system.widechar($DC00+((u - $10000) and $3ff));
end;

{------------------------------------------------------------------------------
  Name:    ConvertUTF8ToUTF16
  Params:  Dest                - Pointer to destination string
           DestWideCharCount   - Wide char count allocated in destination string
           Src                 - Pointer to source string
           SrcCharCount        - Char count allocated in source string
           Options             - Conversion options, if none is set, both
             invalid and unfinished source chars are skipped

             toInvalidCharError       - Stop on invalid source char and report
                                      error
             toInvalidCharToSymbol    - Replace invalid source chars with '?'
             toUnfinishedCharError    - Stop on unfinished source char and
                                      report error
             toUnfinishedCharToSymbol - Replace unfinished source char with '?'

           ActualWideCharCount - Actual wide char count converted from source
                               string to destination string
  Returns:
    trNoError        - The string was successfully converted without
                     any error
    trNullSrc        - Pointer to source string is nil
    trNullDest       - Pointer to destination string is nil
    trDestExhausted  - Destination buffer size is not big enough to hold
                     converted string
    trInvalidChar    - Invalid source char has occured
    trUnfinishedChar - Unfinished source char has occured

  Converts the specified UTF-8 encoded string to UTF-16 encoded (system endian)
 ------------------------------------------------------------------------------}
function ConvertUTF8ToUTF16(Dest: PWideChar; DestWideCharCount: SizeUInt;
  Src: PChar; SrcCharCount: SizeUInt; Options: TConvertOptions;
  out ActualWideCharCount: SizeUInt): TConvertResult;
var
  DestI, SrcI: SizeUInt;
  B1, B2, B3, B4: Byte;
  W: Word;
  C: Cardinal;

  function UnfinishedCharError: Boolean;
  begin
    if toUnfinishedCharToSymbol in Options then
    begin
      Dest[DestI] := System.WideChar('?');
      Inc(DestI);
      Result := False;
    end
    else
      if toUnfinishedCharError in Options then
      begin
        ConvertUTF8ToUTF16 := trUnfinishedChar;
        Result := True;
      end
      else Result := False;
  end;

  function InvalidCharError(Count: SizeUInt): Boolean; inline;
  begin
    if not (toInvalidCharError in Options) then
    begin
      if toInvalidCharToSymbol in Options then
      begin
        Dest[DestI] := System.WideChar('?');
        Inc(DestI);
      end;

      Dec(SrcI, Count);

      // skip trailing UTF-8 char bytes
      while (Count > 0) do
      begin
        if (Byte(Src[SrcI]) and %11000000) <> %10000000 then Break;
        Inc(SrcI);
        Dec(Count);
      end;

      Result := False;
    end
    else
      if toInvalidCharError in Options then
      begin
        ConvertUTF8ToUTF16 := trUnfinishedChar;
        Result := True;
      end;
  end;

begin
  ActualWideCharCount := 0;

  if not Assigned(Src) then
  begin
    Result := trNullSrc;
    Exit;
  end;

  if not Assigned(Dest) then
  begin
    Result := trNullDest;
    Exit;
  end;
  SrcI := 0;
  DestI := 0;

  while (DestI < DestWideCharCount) and (SrcI < SrcCharCount) do
  begin
    B1 := Byte(Src[SrcI]);
    Inc(SrcI);

    if B1 < 128 then // single byte UTF-8 char
    begin
      Dest[DestI] := System.WideChar(B1);
      Inc(DestI);
    end
    else
    begin
      if SrcI >= SrcCharCount then
        if UnfinishedCharError then Exit(trInvalidChar)
        else Break;

      B2 := Byte(Src[SrcI]);
      Inc(SrcI);

      if (B1 and %11100000) = %11000000 then // double byte UTF-8 char
      begin
        if (B2 and %11000000) = %10000000 then
        begin
          Dest[DestI] := System.WideChar(((B1 and %00011111) shl 6) or (B2 and %00111111));
          Inc(DestI);
        end
        else // invalid character, assume single byte UTF-8 char
          if InvalidCharError(1) then Exit(trInvalidChar);
      end
      else
      begin
        if SrcI >= SrcCharCount then
          if UnfinishedCharError then Exit(trInvalidChar)
          else Break;

        B3 := Byte(Src[SrcI]);
        Inc(SrcI);

        if (B1 and %11110000) = %11100000 then // triple byte UTF-8 char
        begin
          if ((B2 and %11000000) = %10000000) and ((B3 and %11000000) = %10000000) then
          begin
            W := ((B1 and %00011111) shl 12) or ((B2 and %00111111) shl 6) or (B3 and %00111111);
            if (W < $D800) or (W > $DFFF) then // to single wide char UTF-16 char
            begin
              Dest[DestI] := System.WideChar(W);
              Inc(DestI);
            end
            else // invalid UTF-16 character, assume double byte UTF-8 char
              if InvalidCharError(2) then Exit(trInvalidChar);
          end
          else // invalid character, assume double byte UTF-8 char
            if InvalidCharError(2) then Exit(trInvalidChar);
        end
        else
        begin
          if SrcI >= SrcCharCount then
            if UnfinishedCharError then Exit(trInvalidChar)
            else Break;

          B4 := Byte(Src[SrcI]);
          Inc(SrcI);

          if ((B1 and %11111000) = %11110000) and ((B2 and %11000000) = %10000000)
            and ((B3 and %11000000) = %10000000) and ((B4 and %11000000) = %10000000) then
          begin // 4 byte UTF-8 char
            C := ((B1 and %00011111) shl 18) or ((B2 and %00111111) shl 12)
              or ((B3 and %00111111) shl 6)  or (B4 and %00111111);
            // to double wide char UTF-16 char
            Dest[DestI] := System.WideChar($D800 or ((C - $10000) shr 10));
            Inc(DestI);
            if DestI >= DestWideCharCount then Break;
            Dest[DestI] := System.WideChar($DC00 or ((C - $10000) and %0000001111111111));
            Inc(DestI);
          end
          else // invalid character, assume triple byte UTF-8 char
            if InvalidCharError(3) then Exit(trInvalidChar);
        end;
      end;
    end;
  end;

  if DestI >= DestWideCharCount then
  begin
    DestI := DestWideCharCount - 1;
    Result := trDestExhausted;
  end
  else
    Result := trNoError;

  Dest[DestI] := #0;
  ActualWideCharCount := DestI + 1;
end;

{------------------------------------------------------------------------------
  Name:    ConvertUTF16ToUTF8
  Params:  Dest             - Pointer to destination string
           DestCharCount    - Char count allocated in destination string
           Src              - Pointer to source string
           SrcWideCharCount - Wide char count allocated in source string
           Options          - Conversion options, if none is set, both
             invalid and unfinished source chars are skipped.
             See ConvertUTF8ToUTF16 for details.

           ActualCharCount  - Actual char count converted from source
                            string to destination string
  Returns: See ConvertUTF8ToUTF16

  Converts the specified UTF-16 encoded string (system endian) to UTF-8 encoded
 ------------------------------------------------------------------------------}
function ConvertUTF16ToUTF8(Dest: PChar; DestCharCount: SizeUInt;
  Src: PWideChar; SrcWideCharCount: SizeUInt; Options: TConvertOptions;
  out ActualCharCount: SizeUInt): TConvertResult;
var
  DestI, SrcI: SizeUInt;
  W1, W2: Word;
  C: Cardinal;

  function UnfinishedCharError: Boolean;
  begin
    if toUnfinishedCharToSymbol in Options then
    begin
      Dest[DestI] := Char('?');
      Inc(DestI);
      Result := False;
    end
    else
      if toUnfinishedCharError in Options then
      begin
        ConvertUTF16ToUTF8 := trUnfinishedChar;
        Result := True;
      end
      else Result := False;
  end;

  function InvalidCharError(Count: SizeUInt): Boolean; inline;
  begin
    if not (toInvalidCharError in Options) then
    begin
      if toInvalidCharToSymbol in Options then
      begin
        Dest[DestI] := Char('?');
        Inc(DestI);
      end;

      Dec(SrcI, Count);
      // skip trailing UTF-16 wide char
      if (Word(Src[SrcI]) and $FC00) = $DC00 then Inc(SrcI);

      Result := False;
    end
    else
      if toInvalidCharError in Options then
      begin
        ConvertUTF16ToUTF8 := trUnfinishedChar;
        Result := True;
      end;
  end;

begin
  ActualCharCount := 0;

  if not Assigned(Src) then
  begin
    Result := trNullSrc;
    Exit;
  end;

  if not Assigned(Dest) then
  begin
    Result := trNullDest;
    Exit;
  end;
  SrcI := 0;
  DestI := 0;

  while (DestI < DestCharCount) and (SrcI < SrcWideCharCount) do
  begin
    W1 := Word(Src[SrcI]);
    Inc(SrcI);

    if (W1 < $D800) or (W1 > $DFFF) then // single wide char UTF-16 char
    begin
      if W1 < $0080 then // to single byte UTF-8 char
      begin
        Dest[DestI] := Char(W1);
        Inc(DestI);
      end
      else
        if W1 < $0800 then // to double byte UTF-8 char
        begin
          Dest[DestI] := Char(%11000000 or ((W1 and %11111000000) shr 6));
          Inc(DestI);
          if DestI >= DestCharCount then Break;
          Dest[DestI] := Char(%10000000 or (W1 and %111111));
          Inc(DestI);
        end
        else
        begin // to triple byte UTF-8 char
          Dest[DestI] := Char(%11100000 or ((W1 and %1111000000000000) shr 12));
          Inc(DestI);
          if DestI >= DestCharCount then Break;
          Dest[DestI] := Char(%10000000 or ((W1 and %111111000000) shr 6));
          Inc(DestI);
          if DestI >= DestCharCount then Break;
          Dest[DestI] := Char(%10000000 or (W1 and %111111));
          Inc(DestI);
        end;
    end
    else
    begin
      if SrcI >= SrcWideCharCount then
        if UnfinishedCharError then Exit(trInvalidChar)
        else Break;

      W2 := Word(Src[SrcI]);
      Inc(SrcI);

      if (W1 and $F800) = $D800 then // double wide char UTF-16 char
      begin
        if (W2 and $FC00) = $DC00 then
        begin
          C := (W1 - $D800) shl 10 + (W2 - $DC00) + $10000;

          // to 4 byte UTF-8 char
          Dest[DestI] := Char(%11110000 or (C shr 18));
          Inc(DestI);
          if DestI >= DestCharCount then Break;
          Dest[DestI] := Char(%10000000 or ((C and $3F000) shr 12));
          Inc(DestI);
          if DestI >= DestCharCount then Break;
          Dest[DestI] := Char(%10000000 or ((C and %111111000000) shr 6));
          Inc(DestI);
          if DestI >= DestCharCount then Break;
          Dest[DestI] := Char(%10000000 or (C and %111111));
          Inc(DestI);
        end
        else // invalid character, assume single wide char UTF-16 char
          if InvalidCharError(1) then Exit(trInvalidChar);
      end
      else // invalid character, assume single wide char UTF-16 char
        if InvalidCharError(1) then Exit(trInvalidChar);
    end;
  end;

  if DestI >= DestCharCount then
  begin
    DestI := DestCharCount - 1;
    Result := trDestExhausted;
  end
  else
    Result := trNoError;

  Dest[DestI] := #0;
  ActualCharCount := DestI + 1;
end;

{------------------------------------------------------------------------------
  Name:    UTF8ToUTF16
  Params:  S - Source UTF-8 string
  Returns: UTF-16 encoded string

  Converts the specified UTF-8 encoded string to UTF-16 encoded (system endian)
  Avoid copying the result string since on windows a widestring requires a full 
  copy
 ------------------------------------------------------------------------------}
function UTF8ToUTF16(const S: AnsiString): UTF16String;
var
  L: SizeUInt;
begin
  if S = '' 
  then begin
    Result := '';
    Exit;
  end;

  SetLength(Result, Length(S));
  // wide chars of UTF-16 <= bytes of UTF-8 string
  if ConvertUTF8ToUTF16(PWideChar(Result), Length(Result) + 1, PChar(S), Length(S),
    [toInvalidCharToSymbol], L) = trNoError 
  then SetLength(Result, L - 1)
  else Result := '';
end;

{------------------------------------------------------------------------------
  Name:    UTF16ToUTF8
  Params:  S - Source UTF-16 string (system endian)
  Returns: UTF-8 encoded string

  Converts the specified UTF-16 encoded string (system endian) to UTF-8 encoded
 ------------------------------------------------------------------------------}
function UTF16ToUTF8(const S: UTF16String): AnsiString;
var
  L: SizeUInt;
  R: AnsiString;
begin
  Result := '';
  if S = '' then Exit;

  SetLength(R, Length(S) * 3);
  // bytes of UTF-8 <= 3 * wide chars of UTF-16 string
  // e.g. %11100000 10100000 10000000 (UTF-8) is $0800 (UTF-16)
  if ConvertUTF16ToUTF8(PChar(R), Length(R) + 1, PWideChar(S), Length(S),
    [toInvalidCharToSymbol], L) = trNoError then
  begin
    SetLength(R, L - 1);
    Result := R;
  end;
end;

procedure LCLGetLanguageIDs(var Lang, FallbackLang: String);

  {$IFDEF DARWIN}
  function GetLanguage: boolean;
  var
    Ref: CFStringRef;
    LangArray: CFMutableArrayRef;
    StrSize: CFIndex;
    StrRange: CFRange;
    Locals: CFArrayRef;
    Bundle: CFBundleRef;
  begin
    Result := false;
    Bundle:=CFBundleGetMainBundle;
    if Bundle=nil then exit;
    Locals:=CFBundleCopyBundleLocalizations(Bundle);
    if Locals=nil then exit;
    LangArray := CFBundleCopyLocalizationsForPreferences(Locals, nil);
    try
      if CFArrayGetCount(LangArray) > 0 then
      begin
        Ref := CFArrayGetValueAtIndex(LangArray, 0);
        StrRange.location := 0;
        StrRange.length := CFStringGetLength(Ref);

        CFStringGetBytes(Ref, StrRange, kCFStringEncodingUTF8,
          Ord('?'), False, nil, 0, StrSize);
        SetLength(Lang, StrSize);

        if StrSize > 0 then
        begin
          CFStringGetBytes(Ref, StrRange, kCFStringEncodingUTF8,
            Ord('?'), False, @Lang[1], StrSize, StrSize);
          Result:=true;
          FallbackLang := Copy(Lang, 1, 2);
        end;
      end;
    finally
      CFRelease(LangArray);
    end;
  end;
  {$ENDIF}
begin
{$IFDEF DARWIN}
  if not GetLanguage then
    GetLanguageIDs(Lang, FallbackLang);
{$ELSE}
  GetLanguageIDs(Lang, FallbackLang);
{$ENDIF}
end;

function CreateFirstIdentifier(const Identifier: string): string;
// example: Ident59 becomes Ident1
var
  p: Integer;
begin
  p:=length(Identifier);
  while (p>=1) and (Identifier[p] in ['0'..'9']) do dec(p);
  Result:=copy(Identifier,1,p)+'1';
end;

function CreateNextIdentifier(const Identifier: string): string;
// example: Ident59 becomes Ident60
var
  p: Integer;
begin
  p:=length(Identifier);
  while (p>=1) and (Identifier[p] in ['0'..'9']) do dec(p);
  Result:=copy(Identifier,1,p)
          +IntToStr(1+StrToIntDef(copy(Identifier,p+1,length(Identifier)-p),0));
end;

procedure FreeLineInfoCache;
var
  ANode: TAvgLvlTreeNode;
  Item: PLineInfoCacheItem;
begin
  if LineInfoCache=nil then exit;
  ANode:=LineInfoCache.FindLowest;
  while ANode<>nil do begin
    Item:=PLineInfoCacheItem(ANode.Data);
    Dispose(Item);
    ANode:=LineInfoCache.FindSuccessor(ANode);
  end;
  LineInfoCache.Free;
  LineInfoCache:=nil;
end;

{ TDebugLCLItems }

constructor TDebugLCLItems.Create(const TheName: string);
begin
  FName:=TheName;
  FItems:=TAvgLvlTree.Create(@CompareDebugLCLItemInfos);
end;

destructor TDebugLCLItems.Destroy;
begin
  FItems.FreeAndClear;
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TDebugLCLItems.FindInfo(p: Pointer; CreateIfNotExists: boolean
  ): TDebugLCLItemInfo;
var
  ANode: TAvgLvlTreeNode;
begin
  ANode:=FItems.FindKey(p,@CompareItemWithDebugLCLItemInfo);
  if ANode<>nil then
    Result:=TDebugLCLItemInfo(ANode.Data)
  else begin
    // does not yet exists
    if CreateIfNotExists then begin
      Result:=MarkCreated(p,'TDebugLCLItems.FindInfo');
    end else begin
      Result:=nil;
    end;
  end;
end;

function TDebugLCLItems.IsDestroyed(p: Pointer): boolean;
var
  Info: TDebugLCLItemInfo;
begin
  Info:=FindInfo(p);
  if Info=nil then
    Result:=false
  else
    Result:=Info.IsDestroyed;
end;

function TDebugLCLItems.IsCreated(p: Pointer): boolean;
var
  Info: TDebugLCLItemInfo;
begin
  Info:=FindInfo(p);
  if Info=nil then
    Result:=false
  else
    Result:=not Info.IsDestroyed;
end;

procedure TDebugLCLItems.MarkDestroyed(p: Pointer);
var
  Info: TDebugLCLItemInfo;

  procedure RaiseNotCreated;
  begin
    DebugLn('TDebugLCLItems.MarkDestroyed not created: p=',dbgs(p));
    DumpStack;
    RaiseGDBException('TDebugLCLItems.MarkDestroyed');
  end;

  procedure RaiseDoubleDestroyed;
  begin
    debugLn('TDebugLCLItems.MarkDestroyed Double destroyed:');
    debugln(Info.AsString(true));
    debugln('Now:');
    DebugLn(GetStackTrace(true));
    RaiseGDBException('RaiseDoubleDestroyed');
  end;

begin
  Info:=FindInfo(p);
  if Info=nil then
    RaiseNotCreated;
  if Info.IsDestroyed then
    RaiseDoubleDestroyed;
  Info.IsDestroyed:=true;
  GetStackTracePointers(Info.DestructionStack);
  //DebugLn(['TDebugLCLItems.MarkDestroyed ',dbgs(p)]);
end;

function TDebugLCLItems.GetInfo(p: Pointer; WithStackTraces: boolean): string;
var
  Info: TDebugLCLItemInfo;
begin
  Info:=FindInfo(p,false);
  if Info<>nil then
    Result:=Info.AsString(WithStackTraces)
  else
    Result:='';
end;

function TDebugLCLItems.MarkCreated(p: Pointer;
  const InfoText: string): TDebugLCLItemInfo;
var
  Info: TDebugLCLItemInfo;

  procedure RaiseDoubleCreated;
  begin
    debugLn('TDebugLCLItems.MarkCreated CREATED TWICE. Old:');
    debugln(Info.AsString(true));
    debugln(' New=',dbgs(p),' InfoText="',InfoText,'"');
    DebugLn(GetStackTrace(true));
    RaiseGDBException('RaiseDoubleCreated');
  end;

begin
  Info:=FindInfo(p);
  if Info=nil then begin
    Info:=TDebugLCLItemInfo.Create;
    Info.Item:=p;
    FItems.Add(Info);
  end else if not Info.IsDestroyed then begin
    RaiseDoubleCreated;
  end;
  Info.IsDestroyed:=false;
  Info.Info:=InfoText;
  GetStackTracePointers(Info.CreationStack);
  SetLength(Info.DestructionStack,0);
  //DebugLn(['TDebugLCLItems.MarkCreated ',Name,' ',dbgs(p),' ',FItems.Count]);
  //DebugLn(GetStackTrace(true));
  Result:=Info;
end;

{ TDebugLCLItemInfo }

function TDebugLCLItemInfo.AsString(WithStackTraces: boolean): string;
begin
  Result:='Item='+Dbgs(Item)+LineEnding
          +'Info="'+DbgStr(Info)+LineEnding;
  if WithStackTraces then
    Result:=Result+'Creation:'+LineEnding+StackTraceAsString(CreationStack,true);
  if IsDestroyed then begin
    Result:=Result+'Destroyed:'+LineEnding;
    if WithStackTraces then
      Result:=Result+StackTraceAsString(DestructionStack,true);
  end;
end;

destructor TDebugLCLItemInfo.Destroy;
begin
  SetLength(CreationStack,0);
  SetLength(DestructionStack,0);
  inherited Destroy;
end;

initialization
  InitializeDebugOutput;
  {$ifdef WinCE}
  // The stabs based back trace function crashes on wince,
  // see http://bugs.freepascal.org/view.php?id=14330
  // To prevent crashes, replace it with the default system back trace function
  // that just outputs addresses and not source and line number
  BackTraceStrFunc := @SysBackTraceStr;
  {$endif}
  InterfaceInitializationHandlers := TFPList.Create;
  InterfaceFinalizationHandlers := TFPList.Create;
  {$IFDEF DebugLCLComponents}
  DebugLCLComponents:=TDebugLCLItems.Create('LCLComponents');
  {$ENDIF}
  {$ifdef NewLowerCase}
  InitUnicodeTables;
  {$endif NewLowerCase}
finalization
  InterfaceInitializationHandlers.Free;
  InterfaceInitializationHandlers:=nil;
  InterfaceFinalizationHandlers.Free;
  InterfaceFinalizationHandlers:=nil;
  {$IFDEF DebugLCLComponents}
  DebugLCLComponents.Free;
  DebugLCLComponents:=nil;
  {$ENDIF}
  FreeLineInfoCache;
  FinalizeDebugOutput;
  DebugLnNestFreePrefix;

end.
