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
{$include include/lcl_defines.inc}

interface

uses
  {$IFDEF Darwin}MacOSAll, {$ENDIF}
  Classes, SysUtils, Math, TypInfo, Types, FPCAdds, AvgLvlTree, FileUtil,
  LCLStrConsts, LCLType, WSReferences, LazUTF8
  {$IFDEF EnablePasWString}, paswstring{$ENDIF}
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

procedure MergeSort(List: TFPList; const OnCompare: TListSortCompare); // sort so that for each i is OnCompare(List[i],List[i+1])<=0
procedure MergeSort(List: TStrings; const OnCompare: TStringsSortCompare); // sort so that for each i is OnCompare(List[i],List[i+1])<=0

function GetEnumValueDef(TypeInfo: PTypeInfo; const Name: string;
                         const DefaultValue: Integer): Integer;

function KeyAndShiftStateToKeyString(Key: word; ShiftState: TShiftState): String;
function KeyStringIsIrregular(const s: string): boolean;
function ShortCutToText(ShortCut: TShortCut): string;// untranslated
function TextToShortCut(const ShortCutText: string): TShortCut;// untranslated

function GetCompleteText(const sText: string; iSelStart: Integer;
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

procedure DebugLn(Args: array of const); overload;
procedure DebugLn(const S: String; Args: array of const); overload;// similar to Format(s,Args)
procedure DebugLn; overload;
procedure DebugLn(const s: string); overload;
procedure DebugLn(const s1,s2: string); overload;
procedure DebugLn(const s1,s2,s3: string); overload;
procedure DebugLn(const s1,s2,s3,s4: string); overload;
procedure DebugLn(const s1,s2,s3,s4,s5: string); overload;
procedure DebugLn(const s1,s2,s3,s4,s5,s6: string); overload;
procedure DebugLn(const s1,s2,s3,s4,s5,s6,s7: string); overload;
procedure DebugLn(const s1,s2,s3,s4,s5,s6,s7,s8: string); overload;
procedure DebugLn(const s1,s2,s3,s4,s5,s6,s7,s8,s9: string); overload;
procedure DebugLn(const s1,s2,s3,s4,s5,s6,s7,s8,s9,s10: string); overload;
procedure DebugLn(const s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11: string); overload;
procedure DebugLn(const s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12: string); overload;
procedure DebugLn(const s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13: string); overload;
procedure DebugLn(const s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14: string); overload;
procedure DebugLn(const s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15: string); overload;
procedure DebugLn(const s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,s16: string); overload;

procedure DebugLnEnter(const s: string = nil); overload;
procedure DebugLnEnter(Args: array of const); overload;
procedure DebugLnEnter(s: string; Args: array of const); overload;
procedure DebugLnEnter(const s1, s2: string; const s3: string = nil;
                     const s4: string = nil; const s5: string = nil; const s6: string = nil;
                     const s7: string = nil; const s8: string = nil; const s9: string = nil;
                     const s10: string = nil; const s11: string = nil; const s12: string = nil;
                     const s13: string = nil; const s14: string = nil; const s15: string = nil;
                     const s16: string = nil; const s17: string = nil; const s18: string = nil); overload;
procedure DebugLnExit(const s: string = nil); overload;
procedure DebugLnExit(Args: array of const); overload;
procedure DebugLnExit(s: string; Args: array of const); overload;
procedure DebugLnExit (const s1, s2: string; const s3: string = nil;
                     const s4: string = nil; const s5: string = nil; const s6: string = nil;
                     const s7: string = nil; const s8: string = nil; const s9: string = nil;
                     const s10: string = nil; const s11: string = nil; const s12: string = nil;
                     const s13: string = nil; const s14: string = nil; const s15: string = nil;
                     const s16: string = nil; const s17: string = nil; const s18: string = nil); overload;

function ConvertLineEndings(const s: string): string;

procedure DbgOut(const S: String; Args: array of const); overload;
procedure DbgOut(const s: string); overload;
procedure DbgOut(const s1,s2: string); overload;
procedure DbgOut(const s1,s2,s3: string); overload;
procedure DbgOut(const s1,s2,s3,s4: string); overload;
procedure DbgOut(const s1,s2,s3,s4,s5: string); overload;
procedure DbgOut(const s1,s2,s3,s4,s5,s6: string); overload;
procedure DbgOut(const s1,s2,s3,s4,s5,s6,s7: string); overload;
procedure DbgOut(const s1,s2,s3,s4,s5,s6,s7,s8: string); overload;
procedure DbgOut(const s1,s2,s3,s4,s5,s6,s7,s8,s9: string); overload;
procedure DbgOut(const s1,s2,s3,s4,s5,s6,s7,s8,s9,s10: string); overload;
procedure DbgOut(const s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11: string); overload;
procedure DbgOut(const s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12: string); overload;

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
  const AnIgnoreCase: Boolean = False; const AnUpdateSource: Boolean = True): String; overload;
function GetPart(const ASkipTo, AnEnd: array of String; var ASource: String;
  const AnIgnoreCase: Boolean = False; const AnUpdateSource: Boolean = True): String; overload;
function TextToSingleLine(const AText: string): string;
function SwapCase(Const S: String): String;

// case..of utility functions
function StringCase(const AString: String; const ACase: array of String {; const AIgnoreCase = False, APartial = false: Boolean}): Integer; overload;
function StringCase(const AString: String; const ACase: array of String; const AIgnoreCase, APartial: Boolean): Integer; overload;
function ClassCase(const AClass: TClass; const ACase: array of TClass {; const ADescendant: Boolean = True}): Integer; overload;
function ClassCase(const AClass: TClass; const ACase: array of TClass; const ADecendant: Boolean): Integer; overload;


// UTF-8 Routines in LCLProc are provided only for backwards compatibility,
// use the routines from LazUTF8 instead

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
function FindInvalidUTF8Character(p: PChar; Count: PtrInt;
                                  StopOnNonASCII: Boolean = false): PtrInt;
function ValidUTF8String(const s: String): String;

procedure AssignUTF8ListToAnsi(UTF8List, AnsiList: TStrings);

// Felipe: Don't substitute with calls to lazutf16 because lazutf16 includes
// some initialization code and tables, which are not necessary for the LCL
function UTF16CharacterLength(p: PWideChar): integer;
function UTF16Length(const s: widestring): PtrInt;
function UTF16Length(p: PWideChar; WordCount: PtrInt): PtrInt;
function UTF16CharacterToUnicode(p: PWideChar; out CharLen: integer): Cardinal;
function UnicodeToUTF16(u: cardinal): widestring;

//compare functions

function UTF8CompareStr(const S1, S2: String): Integer;
function UTF8CompareText(const S1, S2: String): Integer;

type
  TConvertResult = LazUTF8.TConvertResult;

  TConvertOption = LazUTF8.TConvertOption;
  TConvertOptions = LazUTF8.TConvertOptions;

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
  DebugText: ^Text;

implementation

uses gettext;

const
  Str_LCL_Debug_File = 'lcldebug.log';
  UNKNOWN_VK_PREFIX = 'Word(''';
  UNKNOWN_VK_POSTFIX = ''')';

var
  InterfaceInitializationHandlers: TFPList = nil;
  InterfaceFinalizationHandlers: TFPList = nil;
  DebugTextAllocated: boolean;
  DebugNestLvl: Integer = 0;
  DebugNestPrefix: PChar = nil;
  DebugNestAtBOL: Boolean;
  LineInfoCache: TAvgLvlTree = nil;

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

// Used also by TWidgetSet.GetAcceleratorString
function KeyAndShiftStateToKeyString(Key: word; ShiftState: TShiftState): String;
//function AcceleratorString(const AVKey: Byte; const AShiftState: TShiftState): String;

  procedure AddPart(const APart: string);
  begin
    if Result <> '' then
      Result := Result + '+';
    Result := Result + APart;
  end;

  // Tricky routine. This only works for western languages
  procedure AddKey;
  begin
    case Key of
      VK_UNKNOWN    :AddPart(ifsVK_UNKNOWN);
      VK_LBUTTON    :AddPart(ifsVK_LBUTTON);
      VK_RBUTTON    :AddPart(ifsVK_RBUTTON);
      VK_CANCEL     :AddPart(ifsVK_CANCEL);
      VK_MBUTTON    :AddPart(ifsVK_MBUTTON);
      VK_BACK       :AddPart(ifsVK_BACK);
      VK_TAB        :AddPart(ifsVK_TAB);
      VK_CLEAR      :AddPart(ifsVK_CLEAR);
      VK_RETURN     :AddPart(ifsVK_RETURN);
      VK_SHIFT      :AddPart(ifsVK_SHIFT);
      VK_CONTROL    :AddPart(ifsVK_CONTROL);
      VK_MENU       :AddPart(ifsVK_MENU);
      VK_PAUSE      :AddPart(ifsVK_PAUSE);
      VK_CAPITAL    :AddPart(ifsVK_CAPITAL);
      VK_KANA       :AddPart(ifsVK_KANA);
    //  VK_HANGUL     :AddPart('Hangul');
      VK_JUNJA      :AddPart(ifsVK_JUNJA);
      VK_FINAL      :AddPart(ifsVK_FINAL);
      VK_HANJA      :AddPart(ifsVK_HANJA );
    //  VK_KANJI      :AddPart('Kanji');
      VK_ESCAPE     :AddPart(ifsVK_ESCAPE);
      VK_CONVERT    :AddPart(ifsVK_CONVERT);
      VK_NONCONVERT :AddPart(ifsVK_NONCONVERT);
      VK_ACCEPT     :AddPart(ifsVK_ACCEPT);
      VK_MODECHANGE :AddPart(ifsVK_MODECHANGE);
      VK_SPACE      :AddPart(ifsVK_SPACE);
      VK_PRIOR      :AddPart(ifsVK_PRIOR);
      VK_NEXT       :AddPart(ifsVK_NEXT);
      VK_END        :AddPart(ifsVK_END);
      VK_HOME       :AddPart(ifsVK_HOME);
      VK_LEFT       :AddPart(ifsVK_LEFT);
      VK_UP         :AddPart(ifsVK_UP);
      VK_RIGHT      :AddPart(ifsVK_RIGHT);
      VK_DOWN       :AddPart(ifsVK_DOWN);
      VK_SELECT     :AddPart(ifsVK_SELECT);
      VK_PRINT      :AddPart(ifsVK_PRINT);
      VK_EXECUTE    :AddPart(ifsVK_EXECUTE);
      VK_SNAPSHOT   :AddPart(ifsVK_SNAPSHOT);
      VK_INSERT     :AddPart(ifsVK_INSERT);
      VK_DELETE     :AddPart(ifsVK_DELETE);
      VK_HELP       :AddPart(ifsVK_HELP);
      VK_0..VK_9    :AddPart(chr(ord('0')+Key-VK_0));
      VK_A..VK_Z    :AddPart(chr(ord('A')+Key-VK_A));
      VK_LWIN       :AddPart(ifsVK_LWIN);
      VK_RWIN       :AddPart(ifsVK_RWIN);
      VK_APPS       :AddPart(ifsVK_APPS);
      VK_NUMPAD0..VK_NUMPAD9:  AddPart(Format(ifsVK_NUMPAD,[Key-VK_NUMPAD0]));
      VK_MULTIPLY   :AddPart('*');
      VK_ADD        :AddPart('+');
      VK_SEPARATOR  :AddPart('|');
      VK_SUBTRACT   :AddPart('-');
      VK_DECIMAL    :AddPart('.');
      VK_DIVIDE     :AddPart('/');
      VK_F1..VK_F24: AddPart('F'+IntToStr(Key-VK_F1+1));
      VK_NUMLOCK    :AddPart(ifsVK_NUMLOCK);
      VK_SCROLL     :AddPart(ifsVK_SCROLL);
//    VK_EQUAL      :AddPart('=');
//    VK_COMMA      :AddPart(',');
//    VK_POINT      :AddPart('.');
//    VK_SLASH      :AddPart('/');
//    VK_AT         :AddPart('@');
    else
      AddPart(UNKNOWN_VK_PREFIX + IntToStr(Key) + UNKNOWN_VK_POSTFIX);
    end;
  end;

begin
  Result := '';
  if ssCtrl in ShiftState then AddPart(ifsCtrl);
  if ssAlt in ShiftState then AddPart(ifsAlt);
  if ssShift in ShiftState then AddPart(ifsVK_SHIFT);
  if ssMeta in ShiftState then
    {$IFDEF LCLcarbon}
    AddPart(ifsVK_CMD);
    {$ELSE}
    AddPart(ifsVK_META);
    {$ENDIF}
  if ssSuper in ShiftState then AddPart(ifsVK_SUPER);
  AddKey;
end;

function KeyStringIsIrregular(const s: string): boolean;
begin
  Result:=(length(UNKNOWN_VK_PREFIX)<length(s)) and
    (AnsiStrLComp(PChar(s),PChar(UNKNOWN_VK_PREFIX),length(UNKNOWN_VK_PREFIX))=0);
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

function GetCompleteText(const sText: string; iSelStart: Integer;
  bCaseSensitive, bSearchAscending: Boolean; slTextList: TStrings): string;

  function IsSamePrefix(const sCompareText, sPrefix: string; iStart: Integer;
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
  Result := (ARect.Left >= 0) and (ARect.Top >= 0);
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
// sort so that for each i is OnCompare(List[i],List[i+1])<=0
var
  MergeList: PPointer;

  procedure SmallSort(StartPos, EndPos: PtrInt);
  // use insertion sort for small lists
  var
    i: PtrInt;
    Best: PtrInt;
    j: PtrInt;
    Item: Pointer;
  begin
    for i:=StartPos to EndPos-1 do begin
      Best:=i;
      for j:=i+1 to EndPos do
        if OnCompare(List[Best],List[j])>0 then
          Best:=j;
      if Best>i then begin
        Item:=List[i];
        List[i]:=List[Best];
        List[Best]:=Item;
      end;
    end;
  end;

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
    mid: integer;
  begin
    if EndPos-StartPos<6 then begin
      SmallSort(StartPos,EndPos);
    end else begin
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
// sort so that for each i is OnCompare(List[i],List[i+1])<=0
var
  MergeList: PAnsiString;

  procedure SmallSort(StartPos, EndPos: PtrInt);
  // use insertion sort for small lists
  var
    i: PtrInt;
    Best: PtrInt;
    j: PtrInt;
    Item: string;
  begin
    for i:=StartPos to EndPos-1 do begin
      Best:=i;
      for j:=i+1 to EndPos do
        if OnCompare(List[Best],List[j])>0 then
          Best:=j;
      if Best>i then begin
        Item:=List[i];
        List[i]:=List[Best];
        List[Best]:=Item;
      end;
    end;
  end;

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
    mid: integer;
  begin
    if EndPos-StartPos<6 then begin
      SmallSort(StartPos,EndPos);
    end else begin
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


// Debug funcs :

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
    vtWideChar: DbgOut(AnsiString(Args[i].VWideChar));
    vtWidestring: DbgOut(AnsiString(WideString(Args[i].VWideString)));
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

procedure DbgOut(const s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12: string);
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
    vtPWideChar: s:=AnsiString(WideString(s)+Args[i].VPWideChar);
    vtWideChar: s:=AnsiString(WideString(s)+Args[i].VWideChar);
    vtWidestring: s:=AnsiString(WideString(s)+WideString(Args[i].VWideString));
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

function SwapCase(Const S: String): String;
// Inverts the character case. Like LowerCase and UpperCase combined.
var
  i : Integer;
  P : PChar;
begin
  Result := S;
  if not assigned(pointer(result)) then exit;
  UniqueString(Result);
  P:=Pchar(pointer(Result));
  for i := 1 to Length(Result) do begin
    if (P^ in ['a'..'z']) then
      P^ := char(byte(p^) - 32)
    else if (P^ in ['A'..'Z']) then
      P^ := char(byte(p^) + 32);
    Inc(P);
  end;
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
  Result := LazUTF8.UTF8CharacterLength(p);
end;

function UTF8Length(const s: string): PtrInt;
begin
  Result:=UTF8Length(PChar(s),length(s));
end;

function UTF8Length(p: PChar; ByteCount: PtrInt): PtrInt;
begin
  Result := LazUTF8.UTF8Length(p, ByteCount);
end;

function UTF8CharacterToUnicode(p: PChar; out CharLen: integer): Cardinal;
begin
  Result := LazUTF8.UTF8CharacterToUnicode(p, CharLen);
end;

function UnicodeToUTF8(u: cardinal; Buf: PChar): integer;
begin
  Result := LazUTF8.UnicodeToUTF8(u, Buf);
end;

function UnicodeToUTF8SkipErrors(u: cardinal; Buf: PChar): integer;
begin
  Result := LazUTF8.UnicodeToUTF8SkipErrors(u, Buf);
end;

function UnicodeToUTF8(u: cardinal): shortstring;
begin
  Result[0]:=chr(UnicodeToUTF8(u,@Result[1]));
end;

function UTF8ToDoubleByteString(const s: string): string;
begin
  Result := LazUTF8.UTF8ToDoubleByteString(s);
end;

{ returns number of double bytes }
function UTF8ToDoubleByte(UTF8Str: PChar; Len: PtrInt; DBStr: PByte): PtrInt;
begin
  Result := LazUTF8.UTF8ToDoubleByte(UTF8Str, Len, DBStr);
end;

{ Find the start of the UTF8 character which contains BytePos,
  Len is length in byte, BytePos starts at 0 }
function UTF8FindNearestCharStart(UTF8Str: PChar; Len: integer;
  BytePos: integer): integer;
begin
  Result := LazUTF8.UTF8FindNearestCharStart(UTF8Str, Len, BytePos);
end;

{ Len is the length in bytes of UTF8Str
  CharIndex is the position of the desired char (starting at 0), in chars

  This function is similar to UTF8FindNearestCharStart
}
function UTF8CharStart(UTF8Str: PChar; Len, CharIndex: PtrInt): PChar;
begin
  Result := LazUTF8.UTF8CharStart(UTF8Str, Len, CharIndex);
end;

function UTF8CharToByteIndex(UTF8Str: PChar; Len, CharIndex: PtrInt): PtrInt;
begin
  Result := LazUTF8.UTF8CharToByteIndex(UTF8Str, Len, CharIndex);
end;

{ fix any broken UTF8 sequences with spaces }
procedure UTF8FixBroken(P: PChar);
begin
  LazUTF8.UTF8FixBroken(P);
end;

function UTF8CharacterStrictLength(P: PChar): integer;
begin
  Result := LazUTF8.UTF8CharacterStrictLength(P);
end;

function UTF8CStringToUTF8String(SourceStart: PChar; SourceLen: PtrInt) : string;
begin
  Result := LazUTF8.UTF8CStringToUTF8String(SourceStart, SourceLen);
end;

function UTF8Pos(const SearchForText, SearchInText: string): PtrInt;
begin
  Result := LazUTF8.UTF8Pos(SearchForText, SearchInText);
end;

function UTF8Copy(const s: string; StartCharIndex, CharCount: PtrInt): string;
begin
  Result := LazUTF8.UTF8Copy(s, StartCharIndex, CharCount);
end;

procedure UTF8Delete(var s: String; StartCharIndex, CharCount: PtrInt);
begin
  LazUTF8.UTF8Delete(s, StartCharIndex, CharCount);
end;

procedure UTF8Insert(const source: String; var s: string; StartCharIndex: PtrInt);
begin
  LazUTF8.UTF8Insert(source, s, StartCharIndex);
end;

function UTF8LowerCase(const s: String): String;
begin
  Result := UTF8Encode(WideLowerCase(UTF8Decode(s)));
end;

function UTF8UpperCase(const s: String): String;
begin
  Result := UTF8Encode(WideUpperCase(UTF8Decode(s)));
end;

function FindInvalidUTF8Character(p: PChar; Count: PtrInt;
  StopOnNonASCII: Boolean): PtrInt;
// return -1 if ok
begin
  Result := LazUTF8.FindInvalidUTF8Character(p, Count, StopOnNonASCII);
end;

function ValidUTF8String(const s: String): String;
begin
  Result := LazUTF8.ValidUTF8String(s);
end;

procedure AssignUTF8ListToAnsi(UTF8List, AnsiList: TStrings);
begin
  LazUTF8.AssignUTF8ListToAnsi(UTF8List, AnsiList);
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
  Name:    UTF8CompareStr
  Params: S1, S2 - UTF8 encoded strings
  Returns: < 0 if S1 < S2, 0 if S1 = S2, > 0 if S2 > S1.
  Compare 2 UTF8 encoded strings, case sensitive.
  Remark: A widestring manager must be installed in order for this function
  to work correctly with various character sets. eg. under unixes cwstring unit
  must be included in project.
  Note: Use this function instead of AnsiCompareStr.
  This function guarantees proper collation on all supported platforms.
 ------------------------------------------------------------------------------}
function UTF8CompareStr(const S1, S2: String): Integer;
begin
  Result := WideCompareStr(UTF8ToUTF16(S1), UTF8ToUTF16(S2));
end;

{------------------------------------------------------------------------------
  Name:    UTF8CompareText
  Params: S1, S2 - UTF8 encoded strings
  Returns: < 0 if S1 < S2, 0 if S1 = S2, > 0 if S2 > S1.
  Compare 2 UTF8 encoded strings, case insensitive.
  Remark: A widestring manager must be installed in order for this function
  to work correctly with various character sets. eg. under unixes cwstring unit
  must be included in project.
  Note: Use this function instead of AnsiCompareText.
  This function guarantees proper collation on all supported platforms.
 ------------------------------------------------------------------------------}
function UTF8CompareText(const S1, S2: String): Integer;
begin
  Result := WideCompareText(UTF8ToUTF16(S1), UTF8ToUTF16(S2));
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
begin
  Result := LazUTF8.ConvertUTF8ToUTF16(Dest, DestWideCharCount,
    Src, SrcCharCount, Options, ActualWideCharCount);
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
begin
  Result := LazUTF8.ConvertUTF16ToUTF8(Dest, DestCharCount,
    Src, SrcWideCharCount, Options, ActualCharCount);
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
begin
  Result := LazUTF8.UTF8ToUTF16(S);
end;

{------------------------------------------------------------------------------
  Name:    UTF16ToUTF8
  Params:  S - Source UTF-16 string (system endian)
  Returns: UTF-8 encoded string

  Converts the specified UTF-16 encoded string (system endian) to UTF-8 encoded
 ------------------------------------------------------------------------------}
function UTF16ToUTF8(const S: UTF16String): AnsiString;
begin
  Result := LazUTF8.UTF16ToUTF8(S);
end;

procedure LCLGetLanguageIDs(var Lang, FallbackLang: String);
begin
  LazUTF8.LazGetLanguageIDs(Lang, FallbackLang);
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
