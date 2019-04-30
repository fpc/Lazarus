{ $Id$ }
{                        ----------------------------------------------
                            GDBTypeInfo.pp  -  Debugger helper class 
                         ----------------------------------------------

 @created(Wed Mar 29th WET 2003)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@dommelstein.net>)

 This unit contains a helper class for decoding PType output.


 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}
unit GDBTypeInfo;
{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, math,
  // LCL
  LclProc,
  // LazUtils
  LazLoggerBase, LazStringUtils,
  // DebuggerIntf
  DbgIntfBaseTypes, DbgIntfDebuggerBase,
  // LazDebuggerGdbmi
  DebugUtils, GDBMIMiscClasses;

(*
  ptype = {
     family = "class" | "record" | "enum" | "set" | "procedure" | "function" | "simple" | "pointer"
    [ ancestor = "...", ]
    [ private = "[" ( "{" name = "...", type = ptype "}" )* "}," ]
    [ protected = "[" ( "{" name = "...", type = ptype "}" )* "}," ]
    [ public = "[" ( "{" name = "...", type = ptype "}" )* "},"]
    [ published = "[" ( "{" name = "...", type = ptype "}" )* "}," ]
    [ members = "[" ( "..." )* "]," | "[" ( "{" name = "...", type = "..." "}" )* "]," ]
    [ args = "[" ( "..." )* "]," ]
    [ result = "..." ]
    [ name = "..." ]
    [ type = "..." ]


  For example results, comparision between similar types, different GDB versions
  or stabs vs dwarf, see the folder "test/gdb responses/"

*)
type

  TGDBPTypeResultFlag =
    (ptprfParamByRef,
     ptprfPointer,
     ptprfNoStructure,     // for Class or Record: no full class declaration, type ends after class keyword; DWARF "whatis TFoo"
                           // includes "record {...}"
     ptprfDynArray,
     ptprfNoBounds,        // no bounds for array found
     ptprfEmpty,
     ptprfDeclarationInBrackets  // e.g ^(array ...) / "&^()" is/are not included in BaseDeclaration
    );
  TGDBPTypeResultFlags = set of TGDBPTypeResultFlag;
  TGDBPTypeResultKind =
    (ptprkNotEvaluated, ptprkError, ptprkSimple, ptprkClass, ptprkRecord,
     ptprkEnum, ptprkSet, ptprkArray, ptprkProcedure, ptprkFunction);

  TGDBPTypeResult = record
    GdbDescription: string;
    Flags: TGDBPTypeResultFlags;
    Kind: TGDBPTypeResultKind;
    Name, BaseName: TPCharWithLen; // BaseName is without ^&
    BoundLow, BoundHigh: TPCharWithLen;
    Declaration, BaseDeclaration: TPCharWithLen; // BaseDeclaration only for Array and Set types, see note on ptprfDeclarationInBrackets
    PointerCount: Integer;
    // type of array entry, or set-enum
    SubName, BaseSubName: TPCharWithLen;
    SubFlags: TGDBPTypeResultFlags;
    SubKind: TGDBPTypeResultKind;
    // multi-dim array
    NestArrayCount: Integer;
    NestArray: array of record  // reverse order, last entry is first nest level
      Flags: TGDBPTypeResultFlags;
      BoundLow, BoundHigh: TPCharWithLen;
      PointerCount: Integer;
    end;
  end;

  TGDBCommandRequestType = (gcrtPType, gcrtEvalExpr);

  PGDBPTypeRequest = ^TGDBPTypeRequest;
  TGDBPTypeRequest = record
    Request: string;
    ReqType: TGDBCommandRequestType;
    Result: TGDBPTypeResult;
    Error: string;
    Next: PGDBPTypeRequest;
  end;


  (* List:      "ACount", "+", "1"
     Array:     "Item[1][2]"
     Cast/Call: "Foo(Bar)"
  *)

  TGDBExprTextOption = (
    toWithStringFix,   // Adjust index for string (1 based)
    toSkipArrayIdx     // Replace array index with low bound (for ptype)
  );
  TGDBExprTextOptions = set of TGDBExprTextOption;

  { TGDBExpressionPart }

  TGDBExpressionPart = class
  protected
    FText: TPCharWithLen;
    function GetParts({%H-}Index: Integer): TGDBExpressionPart; virtual;
    function GetTextFixed({%H-}AOpts: TGDBExprTextOptions=[]): String; virtual;
    function GetText: String;
    function GetTextStrFixed: String;
    function ParseExpression(AText: PChar; ATextLen: Integer): TGDBExpressionPart;
    procedure Init; virtual;
    procedure InitReq(var AReqPtr: PGDBPTypeRequest; var AReqVar: TGDBPTypeRequest;
                      AReqText: String; AType: TGDBCommandRequestType = gcrtPType);
  public
    function NeedValidation(var AReqPtr: PGDBPTypeRequest): Boolean; virtual;
    function MayNeedStringFix: Boolean; virtual;
    function MayNeedTypeCastFix: Boolean; virtual;
  public
    constructor Create;
    function IsNamedOperator: Boolean;
    function PartCount: Integer; virtual;
    property Parts[Index: Integer]: TGDBExpressionPart read GetParts;
    property Text: String read GetText;
    property TextStrFixed: String read GetTextStrFixed;
    property TextEx[AOpts: TGDBExprTextOptions]: String read GetTextFixed;
  end;

  { TGDBExpression }

  TGDBExpression = class(TGDBExpressionPart)
  private
    FTextStr: String;
  protected
    FExpressionPart: TGDBExpressionPart;
    function GetTextFixed(AOpts: TGDBExprTextOptions=[]): String; override;
    function GetParts(Index: Integer): TGDBExpressionPart; override;
  public
    constructor CreateSimple(AText: PChar; ATextLen: Integer);
    constructor Create(AText: PChar; ATextLen: Integer); virtual; overload;
    constructor Create(ATextStr: String); overload;
    destructor Destroy; override;
    function PartCount: Integer; override;
    function IsCommaSeparated: Boolean;
  end;

  { TGDBExpressionPartNumber }

  TGDBExpressionPartNumber = class(TGDBExpression)
  protected
    function GetTextFixed(AOpts: TGDBExprTextOptions=[]): String; override;
  public
    constructor Create(AText: PChar; ATextLen: Integer); override; overload;
  end;

  { TGDBExpressionPartBracketed }

  TGDBExpressionPartBracketed = class(TGDBExpression)
  protected
    function GetTextFixed(AOpts: TGDBExprTextOptions=[]): String; override;
    function GetPlainText(AOpts: TGDBExprTextOptions=[]): String;
  public
    constructor Create(AText: PChar; ATextLen: Integer); override; overload;
  end;

  { TGDBExpressionPartListBase }

  TGDBExpressionPartListBase = class(TGDBExpressionPart)
  private
    FList: TFPList;
  protected
    function GetParts(Index: Integer): TGDBExpressionPart; override;
    function GetTextFixed(AOpts: TGDBExprTextOptions=[]): String; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure ClearShared;
    function  Add(APart: TGDBExpressionPart):Integer;
    procedure Insert(AIndex: Integer; APart: TGDBExpressionPart);
    procedure Delete(AIndex: Integer);
    function  PartCount: Integer; override;
  end;

  TGDBExpressionPartList = class(TGDBExpressionPartListBase)
  public
    function AddList(APartList: TGDBExpressionPartList):Integer;
  end;

  { TGDBExpressionPartCommaList }

  TGDBExpressionPartCommaList = class(TGDBExpressionPartList)
  protected
    function GetTextFixed(AOpts: TGDBExprTextOptions=[]): String; override;
  end;

  { TGDBExpressionPartArrayIdx }

  TGDBExpressionPartArrayIdx = class(TGDBExpressionPartBracketed)
  private
    FArrayPTypeNestIdx: integer;
    FArrayPTypePointerIdx: integer;
    FPTypeIndexReq: TGDBPTypeRequest;
    FVarParam: Boolean;
    FPTypeReq: TGDBPTypeRequest;
    FPTypeDeRefReq: TGDBPTypeRequest;
    function GetArrayPTypeIsDeRef: boolean;
    function GetArrayPTypeIsPointer: boolean;
    function GetArrayPTypeResult: TGDBPTypeResult;
  protected
    procedure Init; override;
    procedure InitReq(var AReqPtr: PGDBPTypeRequest; AReqText: String); overload;
    procedure InitDeRefReq(var AReqPtr: PGDBPTypeRequest; AReqText: String);
    procedure InitIndexReq(var AReqPtr: PGDBPTypeRequest);
    function GetTextFixed(AOpts: TGDBExprTextOptions=[]): String; override;
    property VarParam: Boolean read FVarParam write FVarParam;
    property PTypeReq: TGDBPTypeRequest read FPTypeReq write FPTypeReq;
    property PTypeDeRefReq: TGDBPTypeRequest read FPTypeDeRefReq write FPTypeDeRefReq;
    property PTypeIndexReq: TGDBPTypeRequest read FPTypeIndexReq write FPTypeIndexReq;
    property ArrayPTypeResult: TGDBPTypeResult read GetArrayPTypeResult;
    property ArrayPTypeIsDeRef: boolean read GetArrayPTypeIsDeRef;
    property ArrayPTypeIsPointer: boolean read GetArrayPTypeIsPointer;
    property ArrayPTypeNestIdx: integer read FArrayPTypeNestIdx write FArrayPTypeNestIdx;
    property ArrayPTypePointerIdx: integer read FArrayPTypePointerIdx write FArrayPTypePointerIdx;
    // for comma separated
    function CreateExpressionForSubIndex(AIndex: Integer): TGDBExpressionPartArrayIdx;
  end;

  { TGDBExpressionPartArray }

  TGDBExpressionPartArray = class(TGDBExpressionPartListBase)
  private
    FNeedTypeCast: Boolean;
    FMaybeString: Boolean;
    function GetIndexParts(Index: Integer): TGDBExpressionPartArrayIdx;
  protected
    function GetTextFixed(AOpts: TGDBExprTextOptions=[]): String; override;
    function GetTextToIdx(AIdx: Integer; AOpts: TGDBExprTextOptions=[]): String;
    function IndexCount: Integer;
    property IndexPart[Index: Integer]: TGDBExpressionPartArrayIdx read GetIndexParts;
  public
    constructor Create(ALeadExpresion: TGDBExpressionPart);
    function AddIndex(APart: TGDBExpressionPartArrayIdx):Integer;
    function NeedValidation(var AReqPtr: PGDBPTypeRequest): Boolean; override;
    function MayNeedStringFix: Boolean; override;
    property NeedTypeCast: Boolean read FNeedTypeCast write FNeedTypeCast;
  end;

  { TGDBExpressionPartCastCall }
  TTypeCastFixFlag = (tcfUnknown, tcfEvalNeeded, tcfNoFixNeeded, tcfFixNeeded);

  TGDBExpressionPartCastCall = class(TGDBExpressionPartListBase)
  private
    FIsFunction: Boolean;
    FIsTypeCast: Boolean;
    FPTypeReq: TGDBPTypeRequest;
    FTypeCastFixFlag: TTypeCastFixFlag;
  protected
    procedure Init; override;
    function GetTextFixed(AOpts: TGDBExprTextOptions=[]): String; override;
    property PTypeReq: TGDBPTypeRequest read FPTypeReq write FPTypeReq;
  public
    constructor Create(ALeadExpresion: TGDBExpressionPart);
    function AddBrackets(APart: TGDBExpressionPart):Integer;
    function NeedValidation(var AReqPtr: PGDBPTypeRequest): Boolean; override;
    function MayNeedTypeCastFix: Boolean; override;
    property IsFunction: Boolean read FIsFunction;
    property IsTypeCast: Boolean read FIsTypeCast;
  end;


  { TGDBPTypeRequestCacheEntry }

  TGDBPTypeRequestCacheEntry = class
  protected
    FRequest: TGDBPTypeRequest;
    FStackFrame: Integer;
    FThreadId: Integer;
  public
    property ThreadId: Integer read FThreadId;
    property StackFrame: Integer read FStackFrame;
    property Request: TGDBPTypeRequest read FRequest;
  end;

const
  TGDBPTypeReqCacheListCount = 64; // minimum 8
type
  TGDBPTypeRequestCache = class
  private
    FLists: Array[0..TGDBPTypeReqCacheListCount - 1] of TFPList;
    function GetRequest(Index: Integer): TGDBPTypeRequest;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function IndexOf(AThreadId, AStackFrame: Integer; ARequest: TGDBPTypeRequest): Integer; virtual;
    procedure Add(AThreadId, AStackFrame: Integer; ARequest: TGDBPTypeRequest);
    property Request[Index: Integer]: TGDBPTypeRequest read GetRequest;
  end;

  { TGDBTypes }

  TGDBTypes = class(TDBGTypes)
  public
    constructor CreateFromCSV(AValues: String);
  end;

  { TGDBType }

  TGDBTypeCreationFlag = (gtcfClassIsPointer,
                          gtcfFullTypeInfo,
                          gtcfSkipTypeName,
                          gtcfExprIsType,
                          gtcfExprEvaluate,
                          gtcfExprEvalStrFixed,      // Evaluate with string fix, if needed; only if gtcfExprEvaluate is set
                          gtcfAutoCastClass,         // Find real class of instance, and use, instead of declared class of variable
                          gtcfForceArrayEval         // Used by RepeatCount, in case of "SomePointer[i]"
                         );
  TGDBTypeCreationFlags = set of TGDBTypeCreationFlag;

  TGDBTypeProcessState =
    (gtpsInitial, gtpsInitialSimple,
     gtpsSimplePointer,
     gtpsClass, gtpsClassAutoCast, gtpsClassPointer, gtpsFinishProcessClass, gtpsClassAncestor,
     gtpsArray,
     gtpsEvalExpr, gtpsEvalExprRepeated,
     gtpsEvalExprArray, gtpsEvalExprDynArray, gtpsEvalExprDynArrayGetData,
     gtpsFinished
    );

  TGDBTypeProcessRequest =
    (gptrPTypeExpr, gptrWhatisExpr, gptrPTypeOfWhatis,
     gptrPTypeExprDeRef, gptrPTypeExprDeDeRef,  // "Foo^", "Foo^^"  for Foo=Object, or &Object
     gptrEvalExpr, gptrEvalExprDeRef, gptrEvalExprCast,
     gptrEvalExpr2, gptrEvalExprDeRef2, gptrEvalExprCast2, // used by MaybeString
     gptrPtypeCustomAutoCast, gptrPtypeCustomAutoCast2,
     gptrInstanceClassName,
     gptrEvalCustomEval
    );
  TGDBTypeProcessRequests = set of TGDBTypeProcessRequest;

  TGDBType = class(TDBGType)
  private
    FInternalTypeName: string;
  private
    FEvalStarted: Boolean;
    FExpression, FPTypeExpression, FOrigExpression: string;
    FHasStringExprEvaluatedAsText: Boolean;
    FCreationFlags: TGDBTypeCreationFlags;
    FMaybeShortString: Boolean;

    // Value-Eval
    FExprEvaluatedAsText: String;
    FHasExprEvaluatedAsText: Boolean;
    FExprEvaluateFormat: TWatchDisplayFormat;
    FRepeatCount: Integer;

    // Sub-Types (FNext is managed by creator / linked list)
    FFirstProcessingSubType, FNextProcessingSubType: TGDBType;
    FRepeatFirstIndex: Integer;
    FStringExprEvaluatedAsText: String;
    FTypeInfoAncestor: TGDBType;

    FArrayIndexValues: Array of TGDBType;
    FArrayIndexValueLimit: Integer;
    FRepeatCountEval: TGDBType;

    // Gdb-Requests
    FEvalError: boolean;
    FEvalRequest, FLastEvalRequest: PGDBPTypeRequest;

    FProcessState: TGDBTypeProcessState;
    FProccesReuestsMade: TGDBTypeProcessRequests;
    FReqResults: Array [TGDBTypeProcessRequest] of TGDBPTypeRequest;

    FParsedExpression: TGDBExpression;

    FHasAutoTypeCastFix: Boolean;
    FAutoTypeCastName: String;

    procedure AddTypeReq(var AReq :TGDBPTypeRequest; const ACmd: string = '');
    procedure AddSubType(ASubType :TGDBType);
    function GetIsFinished: Boolean;
    function RequireRequests(ARequired: TGDBTypeProcessRequests; ACustomData: String = ''): Boolean;
    function IsReqError(AReqType: TGDBTypeProcessRequest; CheckResKind: Boolean = True): Boolean;
  protected
    procedure Init; override;
    function DebugString: String;
    property RepeatFirstIndex: Integer read FRepeatFirstIndex write FRepeatFirstIndex;
  public
    constructor CreateForExpression(const AnExpression: string;
                                    const AFlags: TGDBTypeCreationFlags;
                                    AFormat: TWatchDisplayFormat = wdfDefault;
                                    ARepeatCount: Integer = 0);
    destructor Destroy; override;
    function ProcessExpression: Boolean;
    property EvalRequest: PGDBPTypeRequest read FEvalRequest;
    property EvalError: boolean read FEvalError;
    property IsFinished: Boolean read GetIsFinished;

    property HasExprEvaluatedAsText: Boolean read FHasExprEvaluatedAsText;
    property ExprEvaluatedAsText: String read FExprEvaluatedAsText;

    // Expression with index fixed by -1 for string access
    property HasStringExprEvaluatedAsText: Boolean read FHasStringExprEvaluatedAsText;
    property StringExprEvaluatedAsText: String read FStringExprEvaluatedAsText;
  public
    // InternalTypeName: include ^ for TObject, if needed
    property InternalTypeName: string read FInternalTypeName;
  end;


function CreatePTypeValueList(AResultValues: String): TStringList;
function ParseTypeFromGdb(const ATypeText: string): TGDBPTypeResult;
function GDBMIMaybeApplyBracketsToExpr(e: string): string;

function dbgs(AFlag: TGDBPTypeResultFlag): string; overload;
function dbgs(AFlags: TGDBPTypeResultFlags): string; overload;
function dbgs(AFlag: TGDBTypeCreationFlag): string; overload;
function dbgs(AFlags: TGDBTypeCreationFlags): string; overload;
function dbgs(AState: TGDBTypeProcessState): string; overload;
function dbgs(AKind: TGDBPTypeResultKind): string; overload;
function dbgs(AReqType: TGDBCommandRequestType): string; overload;
function dbgs(AReq: TGDBPTypeRequest): string; overload;
function dbgs(AReqType: TGDBTypeProcessRequest): string; overload;
function dbgs(AReqTypes: TGDBTypeProcessRequests): string; overload;

implementation

const
  GdbCmdPType = 'ptype ';
  GdbCmdWhatIs = 'whatis ';
  GdbCmdEvaluate = '-data-evaluate-expression ';
var
  DBGMI_TYPE_INFO, DBG_WARNINGS: PLazLoggerLogGroup;

function GDBMIMaybeApplyBracketsToExpr(e: string): string;
var
  i: Integer;
  f: Boolean;
begin
  Result := e;
  if (e='') or ( (e[1] = '(') and (e[length(e)] = ')') ) then exit;
  f := False;
  i := length(e);
  while (i > 0) and (not f) do begin
    f := f or not(e[i] in ['a'..'z', 'A'..'Z', '0'..'9', '_']);
    dec(i);
  end;
  if f then
    Result := '(' + Result + ')';
end;

function CreatePTypeValueList(AResultValues: String): TStringList;
var
  S, Line: String;
  Lines: TStringList;

  procedure DoRecord;
  var
    n: Integer;
    S, Members: String;
  begin
    Result.Add('family=record');
    Members := '';

    //concatinate all lines and skip last end
    S := '';
    for n := 0 to Lines.Count - 2 do
      S := S + Lines[n];

    while S <> '' do
    begin
      if Members <> '' then Members := Members + ',';
      Members := Members + '{name=' + GetPart(['    '], [' '], S);
      Members := Members + ',type=' + GetPart([' : '], [';'], S) + '}';
    end;
    Result.Add('members=[' + Members + ']');
  end;

  procedure DoEnum;
  var
    n: Integer;
    S: String;
  begin
    Result.Add('family=enum');

    S := GetPart(['('], [], Line);
    //concatinate all lines
    for n := 0 to Lines.Count - 1 do
      S := S + Lines[n];

    S := GetPart([], [')'], S);
    Result.Add('members=[' + StringReplace(S, ' ', '', [rfReplaceAll]) + ']');
  end;

  procedure DoProcedure;
  var
    n: Integer;
    S: String;
  begin
    Result.Add('family=procedure');

    S := GetPart(['('], [''], Line);
    //concatinate all lines
    for n := 0 to Lines.Count - 1 do
      S := S + Lines[n];

    S := GetPart([''], [')'], S);
    Result.Add('args=[' + StringReplace(S, ', ', ',', [rfReplaceAll]) + ']');
  end;

  procedure DoFunction;
  var
    n: Integer;
    S, Args: String;
  begin
    Result.Add('family=function');

    S := GetPart(['('], [], Line);
    //concatinate all lines
    for n := 0 to Lines.Count - 1 do
      S := S + Lines[n];

    Args := GetPart([], [')'], S);
    S := GetPart([' : '], [], S);
    Result.Add('args=[' + StringReplace(Args, ', ', ',', [rfReplaceAll]) + ']');
    Result.Add('result=' + S);
  end;

  procedure DoClass;
  begin
    Result.Add('family=class');
    Result.Add('ancestor=' + GetPart([': public '], [' '], Line));
  end;

begin
  Result := TStringList.Create;
  if AResultValues = '' then Exit;

  Lines := TStringList.Create;
  try
    Lines.Text := AResultValues;
    if Lines.Count = 0 then Exit;
    Line := Lines[0];
    Lines.Delete(0);

    S := GetPart(['type = '], [' '], Line);
    if S = '' then Exit;
    if Pos(' = class ', Line) > 0
    then DoClass
    else if S[1] = '^'
    then begin
      Result.Add('family=pointer');
      Result.Add('type=' + GetPart(['^'], [' ='], S));
    end
    else if S = 'set'
    then begin
      Result.Add('family=set');
      Result.Add('type=' + Copy(Line, 5, Length(Line)));
    end
    else if S = 'procedure'
    then DoProcedure
    else if S = 'function'
    then DoFunction
    else if Pos(' = (', Line) > 0
    then DoEnum
    else if Pos(' = record', Line) > 0
    then DoRecord
    else begin
      Result.Add('family=simple');
      Result.Add('type=' + S);
    end;

  finally
    Lines.Free;
  end;
end;

function ParseTypeFromGdb(const ATypeText: PChar; const ATypeTextLen: Integer): TGDBPTypeResult;
var
  i: Integer;
  CurPtr, LineEndPtr, EndPtr, BaseDeclPtr, DeclPtr, DeclEndPtr: PChar;
  HelpPtr, HelpPtr2: PChar;
  SubRes: TGDBPTypeResult;

  procedure SkipSpaces(var p: PChar); inline;
  begin
    while (p^ = ' ') do inc(p);
  end;

  function CheckKeyword: TGDBPTypeResultKind;
  begin
    Result := ptprkSimple;
    // Might be: "set of ", "class", "record =", "array [", '<invalid unnamed"...,
    case CurPtr^ of
      's', 'S': begin
          if (LineEndPtr - CurPtr >= 6 )
          and  (UpperCase(copy(CurPtr, 1, 7)) = 'SET OF ')
          then
            Result := ptprkSet;
        end;
      'r', 'R': begin
          if (LineEndPtr - CurPtr >= 5 )
          and (UpperCase(copy(CurPtr, 1, 6)) = 'RECORD')
          and ((CurPtr+6)^ in [' ', ')', #13, #0])
          then
            Result := ptprkRecord;
        end;
      'c', 'C': begin
          if (LineEndPtr - CurPtr >= 4 )
          and (UpperCase(copy(CurPtr, 1, 5)) = 'CLASS')
          and ((CurPtr+5)^ in [' ', ')', #13, #0])
          then
            Result := ptprkClass;
        end;
      'a', 'A': begin
          if (LineEndPtr - CurPtr >= 5 )
          and (UpperCase(copy(CurPtr, 1, 6)) = 'ARRAY ')
          then
            Result := ptprkArray;
        end;
      '<': begin
          if (LineEndPtr - CurPtr >= 35 )
          and (copy(CurPtr, 1, 36) = '<invalid unnamed pascal type code 8>')
          then
            Result := ptprkSet;
        end;
      'p', 'P': begin
          if (LineEndPtr - CurPtr >= 8 )
          and (UpperCase(copy(CurPtr, 1, 9)) = 'PROCEDURE')
          and ((CurPtr+9)^ in [' ', '(', ')', #13, #0])
          then
            Result := ptprkProcedure;
        end;
      'f', 'F': begin
          if (LineEndPtr - CurPtr >= 7 )
          and (UpperCase(copy(CurPtr, 1, 8)) = 'FUNCTION')
          and ((CurPtr+8)^ in [' ', '(', ')', #13, #0])
          then
            Result := ptprkFunction;
        end;

    end;
  end;

  function CheckIsEnum: Integer;
  var
    p: PChar;
  begin
    Result := -1;
    if CurPtr^ <> '(' then exit;

    p := CurPtr;
    while not(p^ in [')', #0]) do inc(p);
    if (p <= LineEndPtr) and (p^ = ')') then
      Result := p - CurPtr + 1;
  end;

  procedure SetPCharLen(var ATarget: TPCharWithLen; AStartPtr, AEndPtr: PChar);
  begin
    ATarget.Ptr := AStartPtr;
    ATarget.Len := AEndPtr - AStartPtr + 1;
  end;

begin
  try
  Result.Flags := [];
  Result.Kind := ptprkError;
  Result.Name.Ptr := nil;
  Result.Name.Len := 0;
  Result.BaseName.Ptr := nil;
  Result.BaseName.Len := 0;
  Result.Declaration.Ptr := nil;
  Result.Declaration.Len := 0;
  Result.BaseDeclaration.Ptr := nil;
  Result.BaseDeclaration.Len := 0;
  Result.PointerCount := 0;
  Result.BoundLow.Ptr := nil;
  Result.BoundLow.Len := 0;
  Result.BoundHigh.Ptr := nil;
  Result.BoundHigh.Len := 0;
  Result.SubName.Ptr := nil;
  Result.SubName.Len := 0;
  Result.BaseSubName.Ptr := nil;
  Result.BaseSubName.Len := 0;
  Result.SubFlags := [];
  Result.SubKind := ptprkError;
  Result.NestArrayCount := 0;
  If (ATypeText = nil) or (ATypeTextLen = 0) then exit;

  (* type = [&^][name]
     type = [&^][name] = class|record : public
     type = [&^][name] = (a,b,c)
     type = [&^]array ...
     type = [&^]set of [name] = (a,b)
     type = [&^](.....)
  *)



  CurPtr := ATypeText;
  EndPtr := ATypeText + ATypeTextLen-1;

  while (EndPtr > CurPtr) and (EndPtr^ in [#10, #13, ' ']) do dec (EndPtr);

  LineEndPtr := EndPtr;
  //limit LineEndPtr to first \n
  HelpPtr := CurPtr;
  while (true) do begin
    if HelpPtr > LineEndPtr - 1 then break;
    if (HelpPtr[0] in [#10, #13])
    then begin
      LineEndPtr := HelpPtr-1;
      while (LineEndPtr > CurPtr) and (LineEndPtr^ in [#10, #13, ' ']) do dec (LineEndPtr);
      break;
    end;
    inc(HelpPtr);
  end;


  BaseDeclPtr := CurPtr;
  DeclPtr := BaseDeclPtr;
  DeclEndPtr := LineEndPtr;

  // Leading ^&
  while True do begin
    case CurPtr^ of
      '^': begin
          include(Result.Flags, ptprfPointer);
          inc(Result.PointerCount);
        end;
      '&': include(Result.Flags, ptprfParamByRef);
      else break;
    end;
    inc(CurPtr);
  end;
  SkipSpaces(CurPtr); // shouldn'tever happen
  BaseDeclPtr := CurPtr;

  if CurPtr > LineEndPtr then begin
    include(Result.Flags, ptprfEmpty);
    exit;
  end;

  // entite type in brackest (), eg ^(array...)
  if CurPtr^ = '(' then begin
    Include(Result.Flags, ptprfDeclarationInBrackets);
    inc(CurPtr);
    SkipSpaces(CurPtr); // shouldn'tever happen
    BaseDeclPtr := CurPtr;
    DeclPtr := CurPtr; // not possible to capture with one line, as closing bracket may be on other line
    if DeclEndPtr^ = ')' then dec(DeclEndPtr);
    if LineEndPtr^ = ')' then dec(LineEndPtr);
    if EndPtr^ = ')' then dec(EndPtr);
  end;

  SetPCharLen(Result.BaseDeclaration, BaseDeclPtr, DeclEndPtr);
  SetPCharLen(Result.Declaration,     DeclPtr,     DeclEndPtr);

  if CurPtr^ = '=' then begin
    // skip ' = '
    inc(CurPtr);
    SkipSpaces(CurPtr);
  end
  else begin
    // process part before ' = '
    Result.Kind := CheckKeyword;
    if Result.Kind = ptprkSimple
    then begin
      // we may have   type = NAME = ....
      HelpPtr := CurPtr;
      while (HelpPtr <= LineEndPtr) and  not (HelpPtr^ in [#0..#31, ' ']) do inc(HelpPtr);
      HelpPtr2 := HelpPtr;  // HelpPtr2 = after [name]
      SkipSpaces(HelpPtr2);

      if (HelpPtr2^ = '=') or // TYPE WITH = (EQUAL)
         ((HelpPtr^ in [#0, #10, #13]) or (HelpPtr > LineEndPtr))
      then begin
        // Type without space, use as name
        SetPCharLen(Result.Name,     DeclPtr, HelpPtr-1);
        SetPCharLen(Result.BaseName, BaseDeclPtr, HelpPtr-1);

        if (HelpPtr^ in [#0, #10, #13]) or (HelpPtr > LineEndPtr) then exit;

        CurPtr := HelpPtr2 + 1; // after ' = '
        SkipSpaces(CurPtr);
        BaseDeclPtr := CurPtr;  // Declaration after ' = '
        DeclPtr := CurPtr;
      end
      else begin
        // Type is a declaration with spaces
        // (base)declaration is already set
        exit;
      end;
    end;
  end;

  // after ' = '

  i := CheckIsEnum;
  if i > 0 then begin
    Result.Kind := ptprkEnum;
    SetPCharLen(Result.BaseDeclaration, CurPtr, CurPtr+i-1);
    SetPCharLen(Result.Declaration,     CurPtr, CurPtr+i-1);
    exit;
  end;

  Result.Kind := CheckKeyword;
  if Result.Kind = ptprkSimple then begin
    Result.Kind := ptprkError;
    debugln('** WARNING: ptype info format error: ' + ATypeText);
    exit;
  end;

  // now we should be AT a keyword, we may have a name set already // Enum are handled already too
  while LineEndPtr^ = ' ' do dec(LineEndPtr);
  case Result.Kind of
    ptprkClass: begin
        HelpPtr := CurPtr + 5;
        SkipSpaces(HelpPtr);
        if HelpPtr^ in [#10, #13] then include(Result.Flags, ptprfNoStructure);
        SetPCharLen(Result.Declaration, DeclPtr, LineEndPtr);
      end;
    ptprkRecord: begin
        HelpPtr := CurPtr + 6;
        SkipSpaces(HelpPtr);
        if HelpPtr^ in ['{'] then begin
          include(Result.Flags, ptprfNoStructure);
          SetPCharLen(Result.Declaration, DeclPtr, CurPtr + 5);
        end
        else
          SetPCharLen(Result.Declaration, DeclPtr, LineEndPtr);
      end;
    ptprkSet: begin
        if CurPtr^ <> '<' then begin;
          SetPCharLen(Result.Declaration, DeclPtr, LineEndPtr);
          //CurPtr := Result.BaseDeclaration.Ptr + 3;
          CurPtr := CurPtr + 6;
          SkipSpaces(CurPtr);
          if (CurPtr^ = '=') then begin  // has enum, no name,
            CurPtr := CurPtr + 1;
            SkipSpaces(CurPtr);
          end;
          SetPCharLen(Result.SubName, CurPtr, LineEndPtr);
          while (CurPtr^ in ['^', '&']) and (CurPtr < LineEndPtr) do inc(CurPtr); // should not happen
          SetPCharLen(Result.BaseSubName, CurPtr, LineEndPtr);
          Result.SubKind := ptprkSimple;
        end
        else begin
          Result.Declaration.Ptr := nil;
          Result.Declaration.Len := 0;
          Result.BaseDeclaration.Ptr := nil;
          Result.BaseDeclaration.Len := 0;
        end;
      end;
    ptprkArray: begin
        SetPCharLen(Result.Declaration, DeclPtr, LineEndPtr);
        SetPCharLen(Result.BaseDeclaration, BaseDeclPtr, LineEndPtr);
        CurPtr := CurPtr + 5;
        SkipSpaces(CurPtr);
        include(Result.Flags, ptprfNoBounds);
        include(Result.Flags, ptprfDynArray);
        if CurPtr^ = '[' then begin
          inc(CurPtr);
          HelpPtr := CurPtr;
          while (HelpPtr^ in ['-', '0'..'9']) and (HelpPtr < LineEndPtr - 3) do inc (HelpPtr);
          if (HelpPtr > CurPtr) and (HelpPtr^ = '.') and  ((HelpPtr+1)^ = '.') then begin
            HelpPtr2 := HelpPtr + 2;
            while (HelpPtr2^ in ['-', '0'..'9']) and (HelpPtr2 < LineEndPtr - 1) do inc (HelpPtr2);
            if (HelpPtr2 > HelpPtr) and (HelpPtr2^ = ']') then begin
              exclude(Result.Flags, ptprfNoBounds);
              Result.BoundLow.Ptr := CurPtr;
              Result.BoundLow.Len := HelpPtr - CurPtr;
              Result.BoundHigh.Ptr := HelpPtr + 2;
              Result.BoundHigh.Len := HelpPtr2 - (HelpPtr + 2);
              if (HelpPtr2 - CurPtr <> 5) or (strlcomp(Result.BoundLow.Ptr, PChar('0..-1'), 5) <> 0) then
                exclude(Result.Flags, ptprfDynArray);
              CurPtr := HelpPtr2 + 1;
            end;
          end;
        end;

        SkipSpaces(CurPtr);
        if (CurPtr^ in ['o', 'O']) and ((CurPtr+1)^ in ['f', 'F']) then begin
          CurPtr := CurPtr + 2;
          SkipSpaces(CurPtr);

          SubRes := ParseTypeFromGdb(CurPtr, EndPtr - CurPtr + 1);
          if SubRes.Kind = ptprkArray then begin
            Result.SubName        := SubRes.SubName;
            Result.BaseSubName    := SubRes.BaseSubName;
            Result.SubFlags       := SubRes.SubFlags;
            Result.SubKind        := SubRes.SubKind;
            Result.NestArrayCount := SubRes.NestArrayCount + 1;
            Result.NestArray      := SubRes.NestArray;
            if length(Result.NestArray) < Result.NestArrayCount
            then SetLength(Result.NestArray, Result.NestArrayCount + 3);
            Result.NestArray[SubRes.NestArrayCount].Flags        := SubRes.Flags;
            Result.NestArray[SubRes.NestArrayCount].PointerCount := SubRes.PointerCount;
            Result.NestArray[SubRes.NestArrayCount].BoundLow     := SubRes.BoundLow;
            Result.NestArray[SubRes.NestArrayCount].BoundHigh    := SubRes.BoundHigh;
          end else begin
            Result.SubName        := SubRes.Name;
            Result.BaseSubName    := SubRes.BaseName;
            Result.SubFlags       := SubRes.Flags;
            Result.SubKind        := SubRes.Kind;
          end;


          //SetPCharLen(Result.SubName, CurPtr, LineEndPtr);
          //while (CurPtr^ in ['^', '&']) and (CurPtr < LineEndPtr) do inc(CurPtr);
          //SetPCharLen(Result.BaseSubName, CurPtr, LineEndPtr);
        end;
      end;
    ptprkProcedure, ptprkFunction: begin
        SetPCharLen(Result.Declaration, DeclPtr, LineEndPtr);
      end;
  end;
  finally
    DebugLn(DBGMI_TYPE_INFO, ['ParseTypeFromGdb: Flags=', dbgs(Result.Flags), ' Kind=', dbgs(Result.Kind), ' Name="', PCLenToString(Result.Name),'"' ]);
  end;
end;

function ParseTypeFromGdb(const ATypeText: string): TGDBPTypeResult;
var
  i: SizeInt;
begin
  i := pos('type = ', ATypeText);
  if i < 1
  then Result := ParseTypeFromGdb(PChar(ATypeText), length(ATypeText))
  else Result := ParseTypeFromGdb((@ATypeText[i])+7, length(ATypeText)-6-i);
  Result.GdbDescription := ATypeText;
end;

function dbgs(AFlag: TGDBPTypeResultFlag): string;
begin
  writestr(Result, AFlag);
end;

function dbgs(AFlags: TGDBPTypeResultFlags): string;
var
  i: TGDBPTypeResultFlag;
begin
  Result:='';
  for i := low(TGDBPTypeResultFlags) to high(TGDBPTypeResultFlags) do
    if i in AFlags then begin
      if Result <> '' then Result := Result + ', ';
      Result := Result + dbgs(i);
    end;
  if Result <> '' then Result := '[' + Result + ']';
end;

function dbgs(AFlag: TGDBTypeCreationFlag): string;
begin
  writestr(Result, AFlag);
end;

function dbgs(AFlags: TGDBTypeCreationFlags): string;
var
  i: TGDBTypeCreationFlag;
begin
  Result:='';
  for i := low(TGDBTypeCreationFlags) to high(TGDBTypeCreationFlags) do
    if i in AFlags then begin
      if Result <> '' then Result := Result + ', ';
      Result := Result + dbgs(i);
    end;
  if Result <> '' then Result := '[' + Result + ']';
end;

function dbgs(AState: TGDBTypeProcessState): string;
begin
  writestr(Result, AState);
end;

function dbgs(AKind: TGDBPTypeResultKind): string;
begin
  writestr(Result, AKind);
end;

function dbgs(AReqType: TGDBCommandRequestType): string;
begin
  WriteStr(Result, AReqType);
end;

function dbgs(AReq: TGDBPTypeRequest): string;
begin
  Result := 'Req="'+AReq.Request+'" type='+dbgs(AReq.ReqType)
    +' HasNext='+dbgs(AReq.Next <> nil)
    ;
end;

function dbgs(AReqType: TGDBTypeProcessRequest): string;
begin
  WriteStr(Result, AReqType);
end;

function dbgs(AReqTypes: TGDBTypeProcessRequests): string;
var
  i: TGDBTypeProcessRequest;
begin
  Result:='';
  for i := low(TGDBTypeProcessRequests) to high(TGDBTypeProcessRequests) do
    if i in AReqTypes then begin
      if Result <> '' then Result := Result + ', ';
      Result := Result + dbgs(i);
    end;
  if Result <> '' then Result := '[' + Result + ']';
end;

//TGDBTypeProcessRequests

{ TGDBExpressionPartCommaList }

function TGDBExpressionPartCommaList.GetTextFixed(AOpts: TGDBExprTextOptions
  ): String;
var
  i: Integer;
begin
  Result := '';
  if PartCount = 0 then
    exit;
  Result := Parts[0].GetTextFixed(AOpts);
  for i := 1 to PartCount - 1 do
    Result := Result + ',' + Parts[i].GetTextFixed(AOpts);
end;

{ TGDBExpressionPartArrayIdx }

function TGDBExpressionPartArrayIdx.GetArrayPTypeIsDeRef: boolean;
begin
  Result := (FPTypeReq.Result.Kind <> ptprkArray);
end;

function TGDBExpressionPartArrayIdx.GetArrayPTypeIsPointer: boolean;
begin
  if FArrayPTypeNestIdx < 0 then begin
    if ArrayPTypeIsDeRef
    then Result := True
    else Result := ptprfPointer in FPTypeReq.Result.Flags;
  end
  else begin
    Result := ptprfPointer in ArrayPTypeResult.NestArray[FArrayPTypeNestIdx].Flags;
  end;
end;

function TGDBExpressionPartArrayIdx.GetArrayPTypeResult: TGDBPTypeResult;
begin
  Result := FPTypeReq.Result;
  if (Result.Kind <> ptprkArray) then
    Result := FPTypeDeRefReq.Result;
end;

procedure TGDBExpressionPartArrayIdx.Init;
begin
  inherited Init;
  FPTypeReq.Result.Kind := ptprkNotEvaluated;
  FPTypeDeRefReq.Result.Kind := ptprkNotEvaluated;
  FPTypeIndexReq.Result.Kind := ptprkNotEvaluated;
  FVarParam := False;
  FArrayPTypeNestidx := -1;
  FArrayPTypePointerIdx := 0;
end;

procedure TGDBExpressionPartArrayIdx.InitReq(var AReqPtr: PGDBPTypeRequest; AReqText: String);
begin
  InitReq(AReqPtr, FPTypeReq, AReqText, gcrtPType);
end;

procedure TGDBExpressionPartArrayIdx.InitDeRefReq(var AReqPtr: PGDBPTypeRequest;
  AReqText: String);
begin
  InitReq(AReqPtr, FPTypeDeRefReq, AReqText, gcrtPType);
end;

procedure TGDBExpressionPartArrayIdx.InitIndexReq(var AReqPtr: PGDBPTypeRequest);
begin
  InitReq(AReqPtr, FPTypeIndexReq,
          GdbCmdEvaluate + Quote(GetPlainText([toSkipArrayIdx])), gcrtEvalExpr);
end;

function TGDBExpressionPartArrayIdx.GetTextFixed(AOpts: TGDBExprTextOptions
  ): String;
begin
  if toWithStringFix in AOpts then begin
    if FExpressionPart = nil
    then Result := PCLenPartToString(FText, 1, FText.Len-2)
    else Result := FExpressionPart.TextEx[AOpts];
    Result := FText.Ptr^ + Result + '-1' + (FText.Ptr + FText.Len-1)^;
  end
  else
     Result := inherited GetTextFixed(AOpts);
end;

function TGDBExpressionPartArrayIdx.CreateExpressionForSubIndex(AIndex: Integer): TGDBExpressionPartArrayIdx;
begin
  Result := TGDBExpressionPartArrayIdx.Create
            (FText.Ptr^ + Parts[AIndex].GetText + (FText.Ptr + FText.Len-1)^);
end;

{ TGDBExpressionPartList }

function TGDBExpressionPartList.AddList(APartList: TGDBExpressionPartList): Integer;
var
  i: Integer;
begin
  Result := -1;
  if APartList.PartCount = 0 then exit;
  Result := FList.add(APartList.Parts[0]);
  for i := 1 to APartList.PartCount - 1 do
    FList.add(APartList.Parts[i]);
end;

{ TGDBExpressionPartArray }

function TGDBExpressionPartArray.GetIndexParts(Index: Integer): TGDBExpressionPartArrayIdx;
begin
  Result := TGDBExpressionPartArrayIdx(Parts[Index+1]);
  Assert(not Result.IsCommaSeparated, 'GetIndexParts not IsCommaSeparated');
end;

function TGDBExpressionPartArray.GetTextFixed(AOpts: TGDBExprTextOptions
  ): String;
begin
  Result := GetTextToIdx(IndexCount-1, AOpts);
end;

function TGDBExpressionPartArray.GetTextToIdx(AIdx: Integer;
  AOpts: TGDBExprTextOptions): String;
// toSkipArrayIdx: replace all indexes with 0. For ptype the position does not matter

  function GetPointerCast(AnIdxPart: TGDBExpressionPartArrayIdx; out PointerCnt: Integer): String;
  var
    PTRes: TGDBPTypeResult;
    i: Integer;
  begin
    Result := '';
    PointerCnt := 0;
    if not AnIdxPart.ArrayPTypeIsPointer then exit;
    PTRes := AnIdxPart.ArrayPTypeResult;
    if PTRes.SubName.Len = 0 then exit;

    i := PTRes.NestArrayCount - 1;
    if i >= 0 then begin
      while (i >= 0) and (ptprfPointer in PTRes.NestArray[i].Flags) do dec(i);
      if i >= 0 then exit; // cant cast, if contains static array
      PointerCnt := PTRes.NestArrayCount+1;
      Result := StringOfChar('^', PointerCnt) + PCLenToString(PTRes.SubName);
      exit;
    end;

    PointerCnt := PTRes.PointerCount;
    // If PTRes is the result of an extra de-ref in the ptype, then we need to add that pointer back
    if AnIdxPart.ArrayPTypeIsDeRef then
      inc(PointerCnt);
    Result := StringOfChar('^', PointerCnt) + PCLenToString(PTRes.SubName);
  end;

var
  i, j, PCastCnt: Integer;
  IdxPart: TGDBExpressionPartArrayIdx;
  PTResult: TGDBPTypeResult;
  NeedTCast: Boolean;
  s, LowVal: String;
begin
  Result := Parts[0].TextEx[AOpts];
  PCastCnt := 0;

  if AIdx < 0 then exit;

  for i := 0 to AIdx do begin
    IdxPart := TGDBExpressionPartArrayIdx(IndexPart[i]);
    PTResult := IdxPart.ArrayPTypeResult;
    if toSkipArrayIdx in AOpts then
      LowVal := '[' + IntToStr(PCLenToInt(PTResult.BoundLow)) + ']';

    if PCastCnt > 0 then dec(PCastCnt);

    if not (PTResult.Kind = ptprkArray)
    then begin
      // maybe pointer with index access
      if toSkipArrayIdx in AOpts
      then Result := Result + LowVal
      else
      if (toWithStringFix in AOpts) and (i = IndexCount - 1)
      then Result := Result + IdxPart.TextStrFixed
      else Result := Result + IdxPart.Text;
      continue;
    end;

    if ((PTResult.NestArrayCount > 0) and (IdxPart.ArrayPTypeNestIdx <> PTResult.NestArrayCount-1)) or
       (IdxPart.ArrayPTypePointerIdx > 0)
    then begin
      // nested array / no named type known
      if (PCastCnt = 0) and IdxPart.ArrayPTypeIsPointer
      then Result := Result + '^';
      if toSkipArrayIdx in AOpts
      then Result := Result + LowVal
      else Result := Result + IdxPart.Text;
      continue;
    end;


    NeedTCast := FNeedTypeCast and (i = IndexCount-1);

    if IdxPart.ArrayPTypeIsPointer
    then begin
      //dyn array
      s := '';
      if IdxPart.VarParam then
        s := GetPointerCast(IdxPart, j);
      if s <> '' // IdxPart.VarParam and (PTResult.SubName.Len > 0)                        // var param can only be set for the un-inxed variable
      then begin
        // fpc 2.4.4 Var-param dynarray
        // var param are marked with a "&" in fpc 2.4. They are a semi automatic pointer.
        // Any such var param, that points to an internal pointer type (e.g dyn array) must be typecasted, to trigger the semi automatic pointer of the var-param
        // For single dyn array: ^Foo(var)[1]
        // For nested dyn array: ^^Foo(var)[1][2]  // the ^ in front of the index must be skipped, as the dyn array was casted into a pointer
        Result := s + '(' + Result + ')';
        NeedTCast := False;
        PCastCnt := j;
      end
      else
      if (PCastCnt = 0) then
        Result := Result + '^';
    end;

    if toSkipArrayIdx in AOpts
    then Result := Result + LowVal
    else Result := Result + IdxPart.Text;

    if NeedTCast and (PTResult.SubName.Len > 0)
    then
      Result := PCLenToString(PTResult.SubName) + '(' + Result + ')';

  end;

end;

function TGDBExpressionPartArray.IndexCount: Integer;
begin
  Result := PartCount - 1;
end;

constructor TGDBExpressionPartArray.Create(ALeadExpresion: TGDBExpressionPart);
begin
  inherited Create;
  FNeedTypeCast := False;
  Add(ALeadExpresion);
end;

function TGDBExpressionPartArray.AddIndex(APart: TGDBExpressionPartArrayIdx): Integer;
var
  j: Integer;
begin
  if APart.IsCommaSeparated then begin
    For j := 0 to APart.PartCount-1 do
      Result := Add(APart.CreateExpressionForSubIndex(j));
    APart.Free;
  end
  else
    Result := Add(APart);
end;

function TGDBExpressionPartArray.NeedValidation(var AReqPtr: PGDBPTypeRequest): Boolean;
var
  i, j: Integer;
  IdxPart, IdxPart2: TGDBExpressionPartArrayIdx;
  PTReq, PTDeRefReq: TGDBPTypeRequest;
  ArrRes: TGDBPTypeResult;
  ResultList: TGDBMINameValueList;
  s: String;
begin
  Result := False;
  // Index
  for i := 1 to PartCount - 1 do
    if Parts[i].NeedValidation(AReqPtr) then
      Result := True;

  if Parts[0].NeedValidation(AReqPtr) // Array-Variable
  then begin
    Result := True;
    exit;
  end;
  if Result then exit;

  i := 0;
  while i < IndexCount do begin
    // IdxPart is the NEXT index. We evaluate the expression BEFORE IdxPart
    IdxPart := IndexPart[i];
    PTReq := IdxPart.PTypeReq;

    if PTReq.Result.Kind = ptprkError
    then begin
      // "Parts[i]" Check if the part before IndexParts[i] needs typecastfixing
      if (pos('address 0x0', PTReq.Error) > 0) and Parts[i].MayNeedTypeCastFix
      then begin
        Result := Parts[i].NeedValidation(AReqPtr);
        PTReq.Result.Kind := ptprkNotEvaluated; // Reset the request
        IdxPart.PTypeReq := PTReq;
      end;

      exit; // If Result = False; // no way to find more info
            // Todo, simply to next entry, and check for "pointer math on incomplete type"
    end;

    if PTReq.Result.Kind = ptprkNotEvaluated
    then begin
      (* ptype ArrayBaseWithoutIndex *)
      IdxPart.VarParam := False;
      // InitReq sets: PTReq.Result.Kind = ptprkError;
      IdxPart.InitReq(AReqPtr, GdbCmdPType + GetTextToIdx(i-1, [toSkipArrayIdx]));
      Result := True;
      exit;
    end
    else
    if (not IdxPart.VarParam) and (ptprfParamByRef in PTReq.Result.Flags) // seen an "&" in the gdb result
    then begin
      (* ptype ArrayBaseWithoutIndex^ *)
      // FPC 2.2.4 encoded "var param" in a special way, and we need an extra deref)
      IdxPart.VarParam := True;
      IdxPart.InitReq(AReqPtr, GdbCmdPType + GDBMIMaybeApplyBracketsToExpr(GetTextToIdx(i-1, [toSkipArrayIdx])) + '^');
      Result := True;
      exit;
    end;

    (* With Dwarf gdb may return "type = ^TFoo" for an array
       And the for the derefferenced expr "type = array of TFoo"
    *)
    PTDeRefReq := IdxPart.PTypeDeRefReq;
    if (PTReq.Result.Kind <> ptprkArray) and
       (ptprfPointer in PTReq.Result.Flags) and
       (PTDeRefReq.Result.Kind = ptprkNotEvaluated)
    then begin
      (* ptype ArrayBaseWithoutIndex^ or ptype ArrayBaseWithoutIndex^^ *)
      if IdxPart.VarParam
      then IdxPart.InitDeRefReq(AReqPtr, GdbCmdPType + GDBMIMaybeApplyBracketsToExpr(GetTextToIdx(i-1, [toSkipArrayIdx])) + '^^')
      else IdxPart.InitDeRefReq(AReqPtr, GdbCmdPType + GDBMIMaybeApplyBracketsToExpr(GetTextToIdx(i-1, [toSkipArrayIdx])) + '^');
      Result := True;
      exit;
    end;

    // we may have nested array (dyn array only):
    // - ^^(array ...)
    // - array ... of array
    // A combination of both is not expected

    ArrRes := IdxPart.ArrayPTypeResult;
    if (ArrRes.Kind = ptprkArray) and (ArrRes.NestArrayCount > 0) then begin
      j := ArrRes.NestArrayCount;
      while j > 0 do begin
        inc(i);
        dec(j);
        if i >= IndexCount then break;
        IdxPart2 := IndexPart[i];
        IdxPart2.PTypeReq      := IdxPart.PTypeReq;
        IdxPart2.PTypeDeRefReq := IdxPart.PTypeDeRefReq;
        IdxPart2.ArrayPTypeNestIdx := j;
      end;
    end

    else
    if (ArrRes.Kind = ptprkArray) and (ArrRes.PointerCount > 1) then begin
      j := ArrRes.PointerCount - 1;
      while j > 0 do begin
        inc(i);
        dec(j);
        if i >= IndexCount then break;
        IdxPart2 := IndexPart[i];
        IdxPart2.PTypeReq      := IdxPart.PTypeReq;
        IdxPart2.PTypeDeRefReq := IdxPart.PTypeDeRefReq;
        IdxPart2.ArrayPTypePointerIdx := j;
      end;
    end;

    inc(i);
  end;

  if IndexCount=0 then exit;

  // check if we may access a char in a string
  IdxPart := IndexPart[IndexCount-1];
  PTReq := IdxPart.PTypeReq;
  if (PTReq.Result.Kind = ptprkSimple) and
     not(IdxPart.PTypeDeRefReq.Result.Kind = ptprkArray)
  then begin
    s := LowerCase(PCLenToString(PTReq.Result.BaseName));
    if (ptprfPointer in PTReq.Result.Flags) and
       ( ( s = 'char') or (s = 'character') or (s = 'wchar') or (s = 'widechar') )
    then begin
      if IsNumber(IdxPart.GetPlainText)
      then begin
        FMaybeString := True;
      end
      else begin
        PTReq := IdxPart.PTypeIndexReq;
        if PTReq.Result.Kind = ptprkNotEvaluated
        then begin
          IdxPart.InitIndexReq(AReqPtr);
          Result := True;
          exit;
        end;

        if (PTReq.Result.Kind = ptprkSimple)
        then begin
          ResultList := TGDBMINameValueList.Create(PTReq.Result.GdbDescription);
          FMaybeString := IsNumber(ResultList.Values['value']);
          ResultList.Free;
        end;
      end;
    end;
  end;

end;

function TGDBExpressionPartArray.MayNeedStringFix: Boolean;
begin
  Result := FMaybeString;
  if not Result then
    Result := inherited MayNeedStringFix;
end;

{ TGDBExpressionPartCastCall }

procedure TGDBExpressionPartCastCall.Init;
begin
  inherited Init;
  FPTypeReq.Result.Kind := ptprkNotEvaluated;
end;

function TGDBExpressionPartCastCall.GetTextFixed(AOpts: TGDBExprTextOptions
  ): String;
begin
  Result := inherited GetTextFixed(AOpts);
  if FTypeCastFixFlag = tcfFixNeeded then
    Result := '^'+Result;
end;

function TGDBExpressionPartCastCall.NeedValidation(var AReqPtr: PGDBPTypeRequest): Boolean;
begin
  Result := inherited NeedValidation(AReqPtr);

  if IsFunction or (FTypeCastFixFlag <> tcfEvalNeeded) then
    exit;

  if FPTypeReq.Result.Kind = ptprkNotEvaluated then begin
    InitReq(AReqPtr, FPTypeReq, GdbCmdPType + Parts[0].GetTextFixed([toSkipArrayIdx]) , gcrtPType);
    Result := True;
    exit;
  end;

  if (FPTypeReq.Result.Kind = ptprkError) or (FPTypeReq.Error <> '') then begin
    FTypeCastFixFlag := tcfNoFixNeeded;
    exit;
  end;

  if FPTypeReq.Result.Kind = ptprkClass then begin
    FTypeCastFixFlag := tcfFixNeeded;
    FIsTypeCast := True;
    exit;
  end;

  if FPTypeReq.Result.Kind = ptprkRecord then begin // Includes pointer to array
    FTypeCastFixFlag := tcfNoFixNeeded; // TODO: Maybe it is needed?
    FIsTypeCast := True;
    exit;
  end;

  if FPTypeReq.Result.Kind in [ptprkProcedure, ptprkFunction] then begin
    FTypeCastFixFlag := tcfNoFixNeeded;
    FIsFunction := True;
    exit;
  end;

  FTypeCastFixFlag := tcfNoFixNeeded;
end;

constructor TGDBExpressionPartCastCall.Create(ALeadExpresion: TGDBExpressionPart);
var
  i, l: Integer;
  s: String;
begin
  inherited Create;
  Add(ALeadExpresion);
  FIsFunction := False;
  FIsTypeCast := False;
  FTypeCastFixFlag := tcfUnknown;
  s := ALeadExpresion.GetText;
  i := 1;
  l := Length(s);
  while (i <= l) and (s[i] in [' ', #9]) do inc(i);
  if i < l then begin
    while (i <= l) and (s[i] in ['a'..'z', 'A'..'Z', '0'..'9', '_']) do inc(i);
    while (i <= l) and (s[i] in [' ', #9]) do inc(i);
    FIsFunction := i < l;  // Contains chars that are not allowed in type identifiers (like foo.bar())
  end;

end;

function TGDBExpressionPartCastCall.AddBrackets(APart: TGDBExpressionPart): Integer;
begin
  Result := Add(APart);
end;

function TGDBExpressionPartCastCall.MayNeedTypeCastFix: Boolean;
begin
  Result := inherited MayNeedTypeCastFix;
  if IsFunction then
    exit;

  if not(FTypeCastFixFlag in [tcfUnknown, tcfEvalNeeded]) then
    exit;

  Result := True;
  FTypeCastFixFlag := tcfEvalNeeded;
end;

{ TGDBExpressionPartNumber }

function TGDBExpressionPartNumber.GetTextFixed(AOpts: TGDBExprTextOptions): String;
begin
  if AOpts=[] then ;
  Result := FTextStr;
end;

constructor TGDBExpressionPartNumber.Create(AText: PChar; ATextLen: Integer);
var
  i: QWord;
  e: word;
begin
  if AText^ = '%' then begin
    Val(Copy(AText, 1, ATextLen), i, e);
    if e <> 0 then begin
      FTextStr := Copy(AText, 1, ATextLen); // pass error to gdb
      exit;
    end;
    FTextStr := '('+IntToStr(Int64(i))+')'; // brackets so no decimal dot can follow
    exit;
  end;

  if AText^ = '$' then
    FTextStr := '0x'+Copy(AText, 2, ATextLen-1)
  else
  if AText^ = '&' then
    FTextStr := '(0'+ Copy(AText, 2, ATextLen-1)+')'
  else begin // strip leading zeros
    while (AText^ = '0') and (ATextLen > 1) do begin
      inc(AText);
      dec(ATextLen);
    end;
    FTextStr := Copy(AText, 1, ATextLen);
  end;

end;

{ TGDBExpressionPartBracketed }

function TGDBExpressionPartBracketed.GetTextFixed(AOpts: TGDBExprTextOptions
  ): String;
begin
  if FExpressionPart = nil
  then Result := inherited GetTextFixed(AOpts)
  else Result := FText.Ptr^ + FExpressionPart.GetTextFixed(AOpts) + (FText.Ptr + FText.Len-1)^;
end;

function TGDBExpressionPartBracketed.GetPlainText(AOpts: TGDBExprTextOptions
  ): String;
begin
  if FExpressionPart = nil
  then Result := PCLenPartToString(FText, 1, FText.Len-2)
  else Result := FExpressionPart.TextEx[AOpts];
end;

constructor TGDBExpressionPartBracketed.Create(AText: PChar; ATextLen: Integer);
begin
  CreateSimple(AText, ATextLen);
  FExpressionPart := ParseExpression(FText.Ptr+1, FText.Len-2);
end;

{ TGDBExpressionPart }

function TGDBExpressionPart.GetTextFixed(AOpts: TGDBExprTextOptions): String;
begin
  Result := PCLenToString(FText);
end;

function TGDBExpressionPart.GetText: String;
begin
  Result := GetTextFixed([]);
end;

function TGDBExpressionPart.ParseExpression(AText: PChar; ATextLen: Integer): TGDBExpressionPart;
const
  // include "." (dots). currently there is no need to break expressions like "foo.bar"
  // Include "^" (deref)
  // do NOT include "@", it is applied after []() resolution
  WordChar = ['a'..'z', 'A'..'Z', '0'..'9', '_', '#', '^', '.'];
var
  CurPtr, EndPtr: PChar;
  CurPartPtr: PChar;

  procedure SkipSpaces;
  begin
    while (CurPtr < EndPtr) and (CurPtr^ in [#9, ' ']) do inc(CurPtr);
  end;

  procedure ScanToWordEnd;
  var
    c: Char;
    f: Boolean;
  begin
    // include "." (dots). currently there is no need to break expressions like "foo.bar"
    // Include "^" (deref)
    while (CurPtr < EndPtr) do begin
      c := CurPtr^;
      if (c in WordChar) then begin
        inc(CurPtr);
      end
      else if (c in [' ', #9]) then begin
        f := ((CurPtr-1)^ in ['.', '^']);
        SkipSpaces;
        if not(f  or  ((CurPtr < EndPtr) and (CurPtr^ in ['.', '^'])) ) then
          break;
      end
      else
        break;
    end;
  end;

  procedure ScanToWordStart;
  begin
    while (CurPtr < EndPtr) and not( (CurPtr^ in WordChar) or (CurPtr^  = ',') )
    do inc(CurPtr);
  end;

  function ScanToCallCastEnd: Boolean;
  var
    i: Integer;
  begin
    i := 0;
    while (CurPtr < EndPtr) do begin
      case CurPtr^ of
        '(': inc(i);
        ')': begin
            dec(i);
            inc(CurPtr);
            if i = 0
            then break
            else continue;
          end;
      end;
      inc(CurPtr);
    end;
    Result := i = 0;
  end;

  function ScanToIndexEnd: Boolean;
  var
    i: Integer;
  begin
    i := 0;
    while (CurPtr < EndPtr) do begin
      case CurPtr^ of
        '[': inc(i);
        ']': begin
            dec(i);
            inc(CurPtr);
            if i = 0
            then break
            else continue;
          end;
      end;
      inc(CurPtr);
    end;
    Result := i = 0;
  end;

  procedure AddExpPart(aList: TGDBExpressionPartList);
  var
    NewList: TGDBExpressionPartList;
  begin
    if aList.PartCount = 0 then exit;
    if (aList.PartCount = 1) and (Result = nil) then begin
      Result := aList.Parts[0];
      aList.ClearShared;
      exit;
    end;

    If Result = nil
    then Result := TGDBExpressionPartList.Create
    else
    if not (Result is TGDBExpressionPartList)
    then begin
      NewList := TGDBExpressionPartList.Create;
      NewList.Add(Result);
      Result := NewList;
    end;

    TGDBExpressionPartList(Result).AddList(aList);
    aList.ClearShared;
  end;

  function MoveListToCopy(aList: TGDBExpressionPartList): TGDBExpressionPart;
  begin
    if aList.PartCount = 1
    then begin
      Result := aList.Parts[0];
    end
    else begin
      Result := TGDBExpressionPartList.Create;
      TGDBExpressionPartList(Result).AddList(aList);
    end;
    aList.ClearShared;
  end;

var
  CurList: TGDBExpressionPartList;
  CurArray: TGDBExpressionPartArray;
  CurCast: TGDBExpressionPartCastCall;
  FCommaList: TGDBExpressionPartCommaList;
  CurWord: TGDBExpression;
begin
  Result := nil;
  FCommaList := nil;
  CurPtr := AText;
  EndPtr := AText + ATextLen;


  while (CurPtr < EndPtr) and not(CurPtr^ in ['[', '(', ',', '%', '&', '$', '0', '''', '"']) do begin
    if CurPtr^ = '''' then begin
      inc(CurPtr);
      while (CurPtr < EndPtr) and (CurPtr^ <> '''') do
        inc(CurPtr);
    end
    else
    if CurPtr^ = '"' then begin
      inc(CurPtr);
      while (CurPtr < EndPtr) and (CurPtr^ <> '"') do
        inc(CurPtr);
    end;
    (* uppercase due to https://sourceware.org/bugzilla/show_bug.cgi?id=17835
       gdb 7.7 and 7.8 fail to find members, if lowercased
       Alternative prefix with "self." if gdb returns &"Type TCLASSXXXX has no component named EXPRESSION.\n"
    *)
    if (CurPtr < EndPtr) and (CurPtr^ in ['a'..'z']) then
      CurPtr^ := UpperCase(CurPtr^)[1];
    inc(CurPtr);
  end;
  if CurPtr = EndPtr then
    exit; // no fixup needed

  CurPtr := AText;
  CurList:= TGDBExpressionPartList.Create;

  while CurPtr < EndPtr do begin

    if (CurPtr^ = ',')
    then begin
      if FCommaList = nil then
        FCommaList := TGDBExpressionPartCommaList.Create;
      AddExpPart(CurList);
      FCommaList.Add(Result);
      Result := nil;
      inc(CurPtr);
    end
    else
    if (CurPtr^ in ['%', '&', '$'])
       or ((CurPtr^ = '0') and ((CurPtr+1)^ in ['0'..'9'])) // octal for gdb
       // no need to handle decimal or 0xabcd
    then begin
      CurPartPtr := CurPtr;
      if CurPtr^ in ['0'..'9'] then begin
        while CurPtr^ in ['0'..'9'] do inc(CurPtr);
        if (CurPtr^ = '.') and ((CurPtr+1)^ <> '.') then begin
          // decimal, no need to convert
          inc(CurPtr);
          while CurPtr^ in ['0'..'9'] do inc(CurPtr);
          CurWord := TGDBExpression.CreateSimple(CurPartPtr, CurPtr - CurPartPtr);
        end
        else begin
          CurWord := TGDBExpressionPartNumber.Create(CurPartPtr, CurPtr - CurPartPtr);
        end;
      end
      else begin
        inc(CurPtr);
        case CurPartPtr^ of
          '$': while CurPtr^ in ['a'..'z', 'A'..'Z', '0'..'9'] do inc(CurPtr);
          '&': while CurPtr^ in ['0'..'7'] do inc(CurPtr);
          '%': while CurPtr^ in ['0'..'1'] do inc(CurPtr);
        end;
        CurWord := TGDBExpressionPartNumber.Create(CurPartPtr, CurPtr - CurPartPtr);
      end;
      CurList.Add(CurWord);
    end
    else
    if CurPtr^ in WordChar
    then begin
      CurPartPtr := CurPtr;
      ScanToWordEnd;
      CurWord := TGDBExpression.CreateSimple(CurPartPtr, CurPtr - CurPartPtr);
      CurList.Add(CurWord);
      if (CurPtr^ in WordChar) or CurWord.IsNamedOperator then // 2 words => named operator (and/or)
        AddExpPart(CurList);
    end
    else
    if (CurList.PartCount > 0) and (CurPtr^ = '[')
    then begin
      CurArray := TGDBExpressionPartArray.Create(MoveListToCopy(CurList));
      CurList.Add(CurArray);
      while (CurPtr^ = '[') do begin
        CurPartPtr := CurPtr;
        if not ScanToIndexEnd then break; // broken expression, do not attempt to do anything
        CurArray.AddIndex(TGDBExpressionPartArrayIdx.Create(CurPartPtr, CurPtr - CurPartPtr));
        SkipSpaces;
      end;
      if (CurPtr < EndPtr ) and (CurPtr^ in ['.', '^', '(']) then
        CurArray.NeedTypeCast := True;
    end
    else
    if (CurList.PartCount > 0) and (CurPtr^ = '(')
    then begin
      CurCast := TGDBExpressionPartCastCall.Create(MoveListToCopy(CurList));
      CurList.Add(CurCast);
      CurPartPtr := CurPtr;
      if not ScanToCallCastEnd then break; // broken expression, do not attempt to do anything
      CurCast.AddBrackets(TGDBExpressionPartBracketed.Create(CurPartPtr, CurPtr - CurPartPtr));
    end
    else begin
      CurPartPtr := CurPtr;
      ScanToWordStart;
      CurList.Add(TGDBExpression.CreateSimple(CurPartPtr, CurPtr - CurPartPtr));
      AddExpPart(CurList);
    end;

  end;

  AddExpPart(CurList);
  CurList.Free;

  if FCommaList <> nil then begin
    if Result <> nil then
      FCommaList.Add(Result);
    Result := FCommaList;
  end;


  if CurPtr < EndPtr then debugln(['Scan aborted: ', PCLenToString(FText)]);
  if CurPtr < EndPtr then FreeAndNil(Result);
end;

procedure TGDBExpressionPart.Init;
begin
  //
end;

procedure TGDBExpressionPart.InitReq(var AReqPtr: PGDBPTypeRequest;
  var AReqVar: TGDBPTypeRequest; AReqText: String; AType: TGDBCommandRequestType);
begin
  AReqVar.Request := AReqText;
  AReqVar.Error := '';
  AReqVar.ReqType := AType;
  AReqVar.Next := AReqPtr;
  AReqVar.Result.Kind := ptprkError;
  AReqPtr := @AReqVar;
end;

function TGDBExpressionPart.NeedValidation(var AReqPtr: PGDBPTypeRequest): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to PartCount - 1 do
    if Parts[i].NeedValidation(AReqPtr) then
      Result := True;
end;

function TGDBExpressionPart.MayNeedStringFix: Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to PartCount - 1 do
    if Parts[i].MayNeedStringFix then
      Result := True;
end;

function TGDBExpressionPart.MayNeedTypeCastFix: Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to PartCount - 1 do
    if Parts[i].MayNeedTypeCastFix then
      Result := True;
end;

constructor TGDBExpressionPart.Create;
begin
  Init;
end;

function TGDBExpressionPart.IsNamedOperator: Boolean;
var
  s: String;
begin
  s := LowerCase(Trim(GetText));
  Result := (s = 'not') or (s = 'or') or (s = 'xor') or (s = 'and');
end;

function TGDBExpressionPart.GetTextStrFixed: String;
begin
  Result := GetTextFixed([toWithStringFix]);
end;

function TGDBExpressionPart.GetParts(Index: Integer): TGDBExpressionPart;
begin
  Result := nil;
end;

function TGDBExpressionPart.PartCount: Integer;
begin
  Result := 0;
end;

{ TGDBExpressionPartListBase }

function TGDBExpressionPartListBase.GetParts(Index: Integer): TGDBExpressionPart;
begin
  Result := TGDBExpressionPart(FList[Index]);
end;

function TGDBExpressionPartListBase.GetTextFixed(AOpts: TGDBExprTextOptions
  ): String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to PartCount - 1 do
    Result := Result + Parts[i].GetTextFixed(AOpts);
end;

constructor TGDBExpressionPartListBase.Create;
begin
  inherited Create;
  FList := TFPList.Create;
end;

destructor TGDBExpressionPartListBase.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TGDBExpressionPartListBase.Clear;
begin
  while FList.Count > 0 do begin
    TGDBExpressionPart(Flist[0]).Free;
    FList.Delete(0);
  end;
end;

procedure TGDBExpressionPartListBase.ClearShared;
begin
  FList.Clear;
end;

function TGDBExpressionPartListBase.Add(APart: TGDBExpressionPart): Integer;
begin
  Result := FList.Add(APart);
end;

procedure TGDBExpressionPartListBase.Insert(AIndex: Integer; APart: TGDBExpressionPart);
begin
  FList.Insert(AIndex, APart);
end;

procedure TGDBExpressionPartListBase.Delete(AIndex: Integer);
begin
  FList.Delete(AIndex);
end;

function TGDBExpressionPartListBase.PartCount: Integer;
begin
  Result := FList.Count;
end;

{ TGDBExpression }

function TGDBExpression.GetTextFixed(AOpts: TGDBExprTextOptions): String;
begin
  if FExpressionPart = nil
  then Result := inherited GetTextFixed(AOpts)
  else Result := FExpressionPart.GetTextFixed(AOpts);
end;

function TGDBExpression.GetParts(Index: Integer): TGDBExpressionPart;
begin
  Result := nil;
  if FExpressionPart = nil then exit;
  if FExpressionPart is TGDBExpressionPartList
  then Result := FExpressionPart.Parts[Index]
  else Result := FExpressionPart;
end;

constructor TGDBExpression.CreateSimple(AText: PChar; ATextLen: Integer);
begin
  inherited Create;
  // not to be parsed
  FExpressionPart := nil;
  FText.Ptr := AText;
  FText.Len := ATextLen;
end;

constructor TGDBExpression.Create(AText: PChar; ATextLen: Integer);
begin
  CreateSimple(AText, ATextLen);
  FExpressionPart := ParseExpression(FText.Ptr, FText.Len);
end;

constructor TGDBExpression.Create(ATextStr: String);
begin
  FTextStr := ATextStr;
  UniqueString(FTextStr);
  Create(PChar(FTextStr), length(FTextStr));
end;

destructor TGDBExpression.Destroy;
begin
  FreeAndNil(FExpressionPart);
  inherited Destroy;
end;

function TGDBExpression.PartCount: Integer;
begin
  Result := 0;
  if FExpressionPart = nil then exit;
  if FExpressionPart is TGDBExpressionPartList
  then Result := FExpressionPart.PartCount
  else Result := 1;
end;

function TGDBExpression.IsCommaSeparated: Boolean;
begin
  Result := (FExpressionPart <> nil) and (FExpressionPart is TGDBExpressionPartCommaList);
end;

{ TGDBPTypeRequestCache }

function TGDBPTypeRequestCache.GetRequest(Index: Integer): TGDBPTypeRequest;
begin
  Result := TGDBPTypeRequestCacheEntry(FLists[Index mod TGDBPTypeReqCacheListCount][Index div TGDBPTypeReqCacheListCount]).FRequest;
end;

constructor TGDBPTypeRequestCache.Create;
begin
end;

destructor TGDBPTypeRequestCache.Destroy;
var
  i: Integer;
begin
  Clear;
  inherited Destroy;
  for i := 0 to high(FLists) do
    FreeAndNil(FLists[i]);
end;

procedure TGDBPTypeRequestCache.Clear;
var
  i: Integer;
begin
  for i := 0 to high(FLists) do
    if FLists[i] <> nil then
      while FLists[i].Count > 0 do begin
        TGDBPTypeRequestCacheEntry(FLists[i][0]).Free;
        FLists[i].Delete(0);
      end;
end;

function TGDBPTypeRequestCache.IndexOf(AThreadId, AStackFrame: Integer;
  ARequest: TGDBPTypeRequest): Integer;
var
  e: TGDBPTypeRequestCacheEntry;
  s: String;
  HashVal: Integer;
begin
  s := UpperCase(ARequest.Request);
  // There are usually a couple of dozen entry total. Even if most are the same len the search will be quick
  // Including stackframe, means nested procedures go in different lists.
  HashVal := Length(s) mod (TGDBPTypeReqCacheListCount div 8) * 8
             + AStackFrame mod 4 * 2
             + ord(ARequest.ReqType);
  Result := -1;
  if FLists[HashVal] = nil then
    exit;
  Result := FLists[HashVal].Count - 1;
  while Result >= 0 do begin
    e := TGDBPTypeRequestCacheEntry(FLists[HashVal][Result]);
    if (e.ThreadId = AThreadId) and (e.StackFrame = AStackFrame) and
       (e.Request.Request = s) and
       (e.Request.ReqType = ARequest.ReqType)
    then begin
      Result := Result * TGDBPTypeReqCacheListCount + HashVal;
      exit;
    end;
    dec(Result);
  end;
end;

procedure TGDBPTypeRequestCache.Add(AThreadId, AStackFrame: Integer;
  ARequest: TGDBPTypeRequest);
var
  e: TGDBPTypeRequestCacheEntry;
  HashVal: Integer;
begin
  e := TGDBPTypeRequestCacheEntry.Create;
  e.FThreadId := AThreadId;
  e.FStackFrame := AStackFrame;
  e.FRequest := ARequest;
  e.FRequest.Request := UpperCase(e.FRequest.Request);
  e.FRequest.Next := nil;
  HashVal := Length(ARequest.Request) mod (TGDBPTypeReqCacheListCount div 8) * 8
             + AStackFrame mod 4 * 2
             + ord(ARequest.ReqType);
  if FLists[HashVal] = nil then
    FLists[HashVal] := TFPList.Create;
  FLists[HashVal].Add(e);
end;

{ TGDBPType }

procedure TGDBType.AddTypeReq(var AReq: TGDBPTypeRequest; const ACmd: string = '');
begin
  AReq.Result.Kind := ptprkError;
  AReq.Request := ACmd;
  AReq.Error := '';
  AReq.Next := FEvalRequest;
  FEvalRequest := @AReq;
  if FLastEvalRequest = nil then
    FLastEvalRequest := @AReq;
end;

procedure TGDBType.AddSubType(ASubType: TGDBType);
begin
  if ASubType.ProcessExpression then
    exit;
  ASubType.FNextProcessingSubType := FFirstProcessingSubType;
  FFirstProcessingSubType := ASubType;
end;

function TGDBType.GetIsFinished: Boolean;
begin
  Result := FProcessState = gtpsFinished;
end;

function TGDBType.RequireRequests(ARequired: TGDBTypeProcessRequests; ACustomData: String = ''): Boolean;

  function GetReqText(AReq: TGDBTypeProcessRequest): String;
  begin
    case areq of
      gptrPTypeExpr:        Result := GdbCmdPType + FPTypeExpression;
      gptrWhatisExpr:       Result := GdbCmdWhatIs + FPTypeExpression;
      gptrPTypeOfWhatis:    Result := GdbCmdPType + PCLenToString(FReqResults[gptrWhatisExpr].Result.BaseName);
      gptrPTypeExprDeRef:   Result := GdbCmdPType + GDBMIMaybeApplyBracketsToExpr(FPTypeExpression) + '^';
      gptrPTypeExprDeDeRef: Result := GdbCmdPType + GDBMIMaybeApplyBracketsToExpr(FPTypeExpression) + '^^';
      gptrEvalExpr:       Result := GdbCmdEvaluate+Quote(FExpression);
      gptrEvalExprDeRef:  Result := GdbCmdEvaluate+Quote(FExpression+'^');
      gptrEvalExprCast:   Result := GdbCmdEvaluate+Quote(InternalTypeName+'('+FExpression+')');
      gptrEvalExpr2:      Result := GdbCmdEvaluate+Quote(ACustomData);
      gptrEvalExprDeRef2: Result := GdbCmdEvaluate+Quote(ACustomData+'^');
      gptrEvalExprCast2:  Result := GdbCmdEvaluate+Quote(InternalTypeName+'('+ACustomData+')');
      gptrPtypeCustomAutoCast, gptrPtypeCustomAutoCast2:
                         Result := GdbCmdPType + ACustomData;
      gptrInstanceClassName: Result := GdbCmdEvaluate+Quote('(^^^char('+FExpression+')^+3)^');
      gptrEvalCustomEval:   Result := GdbCmdEvaluate+Quote(ACustomData);
    end;
  end;

var
  NeededReq: TGDBTypeProcessRequests;
  i: TGDBTypeProcessRequest;
begin
  NeededReq := ARequired - FProccesReuestsMade;
  Result := NeededReq = [];
  if Result then exit;
  //DebugLn(DBGMI_TYPE_INFO, ['TGDBType.ProcessExpression: Adding Req ', dbgs(NeededReq), ', CD=', ACustomData]);

  if (gptrPTypeOfWhatis in NeededReq) and not (gptrWhatisExpr in FProccesReuestsMade)
  then begin
    Exclude(NeededReq, gptrPTypeOfWhatis);
    Include(NeededReq, gptrWhatisExpr);
  end;

  FProccesReuestsMade := FProccesReuestsMade + NeededReq;
  for i := low(TGDBTypeProcessRequest) to high(TGDBTypeProcessRequest) do
    if i in NeededReq then begin
      AddTypeReq(FReqResults[i], GetReqText(i));
      if i in [gptrEvalExpr, gptrEvalExprDeRef, gptrEvalExprCast, gptrInstanceClassName, gptrEvalCustomEval]
      then FReqResults[i].ReqType := gcrtEvalExpr
      else FReqResults[i].ReqType := gcrtPType;
    end;
end;

function TGDBType.IsReqError(AReqType: TGDBTypeProcessRequest; CheckResKind: Boolean = True): Boolean;
begin
  Result := (not (AReqType in FProccesReuestsMade))
         or (FReqResults[AReqType].Error <> '')
         or (CheckResKind and (FReqResults[AReqType].Result.Kind = ptprkError));
end;

procedure TGDBType.Init;
begin
  inherited Init;
  FProcessState := gtpsFinished;
  FParsedExpression := nil;
  FMaybeShortString := False;
end;

function TGDBType.DebugString: String;
begin
  Result := Format('Expr="%s", Flags=%s, State=%s', [FExpression, dbgs(FCreationFlags), dbgs(FProcessState)]);
end;

constructor TGDBType.CreateForExpression(const AnExpression: string;
  const AFlags: TGDBTypeCreationFlags; AFormat: TWatchDisplayFormat; ARepeatCount: Integer);
begin
  Create(skSimple, ''); // initialize
  FInternalTypeName := '';
  FEvalError := False;
  FExpression := AnExpression;
  FOrigExpression := FExpression;
  FCreationFlags := AFlags;
  FExprEvaluateFormat := AFormat;
  FEvalStarted := False;
  FEvalRequest := nil;
  FFirstProcessingSubType := nil;
  FNextProcessingSubType := nil;
  FProcessState := gtpsInitial;
  FHasExprEvaluatedAsText := False;
  FHasAutoTypeCastFix := False;
  FAutoTypeCastName := '';
  FArrayIndexValueLimit := 5;
  FRepeatCountEval := nil;
  FRepeatCount := ARepeatCount;
  FRepeatFirstIndex := 0;
end;

destructor TGDBType.Destroy;
var
  i: Integer;
begin
  inherited Destroy;
  FreeAndNil(FTypeInfoAncestor);
  for i := 0 to Length(FArrayIndexValues) - 1 do
    FArrayIndexValues[i].Free;
  FArrayIndexValues := nil;
  FreeAndNil(FParsedExpression);
  FreeAndNil(FRepeatCountEval);
end;

function TGDBType.ProcessExpression: Boolean;
var
  Lines: TStringList;
  procedure ProcessInitial; forward;
  procedure ProcessInitialSimple; forward;
  procedure ProcessSimplePointer; forward;
  procedure EvaluateExpression; forward;


  function ClearAmpersand(s: string): string;
  var i: Integer;
  begin
    Result := s;
    i := pos('&', Result);
    if i > 0 then delete(Result, i, 1);
  end;

  procedure SetTypNameFromReq(AReqType: TGDBTypeProcessRequest;
    AnUseBaseName: Boolean = False; ADefaultName: String = '');
  begin
    if IsReqError(AReqType) or (FReqResults[AReqType].Result.BaseName.Len = 0)
    then AReqType := gptrPTypeExpr;

    if AnUseBaseName
    then FTypeName := PCLenToString(FReqResults[AReqType].Result.BaseName)
    else FTypeName := ClearAmpersand(PCLenToString(FReqResults[AReqType].Result.Name));

    if FTypeName = ''
    then FTypeName := ADefaultName;
    FInternalTypeName := FTypeName;
  end;

  Procedure InitLinesFrom(AReq: TGDBPTypeRequest);
  begin
    FreeAndNil(Lines);
    Lines := TStringList.Create;
    Lines.Text := AReq.Result.GdbDescription;
  end;

  procedure DoEnum;
  var
    S: String;
  begin
    FKind := skEnum;

    S := PCLenToString(FReqResults[gptrPTypeExpr].Result.Declaration);
    S := GetPart(['('], [')'], S);
    if (S = '') or (S = '...') then
      exit;

    FMembers := TStringList.Create;
    FMembers.Text := StringReplace(S, ' ', #13#10, [rfReplaceAll]);
  end;

  procedure DoProcedure;
  var
    S: String;
  begin
    FKind := skProcedure;

    S := PCLenToString(FReqResults[gptrPTypeExpr].Result.Declaration);
    S := GetPart(['('], [')'], S);
    if (S = '') then
      exit;

    FArguments := TGDBTypes.CreateFromCSV(S);
  end;

  procedure DoFunction;
  var
    S: String;
  begin
    FKind := skFunction;

    S := PCLenToString(FReqResults[gptrPTypeExpr].Result.Declaration);
    S := GetPart(['('], [')'], S);
    if (S = '') then
      exit;

    FArguments := TGDBTypes.CreateFromCSV(S);

    S := PCLenToString(FReqResults[gptrPTypeExpr].Result.Declaration);
    FResult := TGDBType.Create(skSimple, GetPart([' : '], [], S));
  end;

  procedure DoSet;
  var
    S: String;
  begin
    FKind := skSet;

    S := PCLenToString(FReqResults[gptrPTypeExpr].Result.Declaration);
    S := GetPart(['('], [')'], S);
    if (S = '') or (S = '...') then
      exit;

    FMembers := TStringList.Create;
    FMembers.Text := StringReplace(StringReplace(S, ',', #13#10, [rfReplaceAll]), ' ', '', [rfReplaceAll]);
  end;

  {%region    * Record * }
  procedure DoRecord;
  var
    n: Integer;
    S, S1, S2: String;
    Field: TDBGField;
  begin
    FFields := TDBGFields.Create;
    InitLinesFrom(FReqResults[gptrPTypeExpr]);

    //concatenate all lines and skip last end
    for n := 1 to Lines.Count - 2 do begin
      S := Lines[n];
      S1 := Trim(GetPart([' '], [':'], S));
      S2 := Trim(GetPart([':'], [';'], S));
      Field := TDBGField.Create(
        S1,
        TGDBType.Create(skSimple, S2),
        flPublic
      );
      FFields.Add(Field);
    end;

    FMaybeShortString := (FFields.Count = 2) and // shortstring have 2 fields: length and st
       (lowercase(FFields[0].Name) = 'length') and
       (lowercase(FFields[1].Name) = 'st');

    if (FTypeName = 'Variant') or
       (FTypeName = 'VARIANT') then
      FKind := skVariant
    else
    if (FTypeName = 'ShortString') or
       (FTypeName = 'SHORTSTRING') or
       (FTypeName = '&ShortString')
    then begin
      if (gtcfExprEvaluate in FCreationFlags) then
        FMaybeShortString := True // will be checked later
       else
        FKind := skSimple
     end
    else
      FKind := skRecord;


  end;
  {%endregion    * Record * }

  {%region    * Class * }
  procedure DoClass;
  var
    n, i, j, l: Integer;
    S, S2: String;

    Name: String;
    DBGType: TDBGType;
    Location: TDBGFieldLocation;
    Flags: TDBGFieldFlags;
  begin
    include(FAttributes, saInternalPointer);
    FKind := skClass;
    InitLinesFrom(FReqResults[gptrPTypeExpr]);
    FFields := TDBGFields.Create;
    if Lines.Count < 1 then exit;
    s := Lines[0];
    FAncestor := GetPart([': public '], [' '], s);

    Location := flPublished;
    n := 0;
    while n <  Lines.Count - 2 do
    begin
      inc(n);
      S := Lines[n];
      if S = '' then Continue; // TODO: clear location (private, peblic)
      if S = 'end' then break;
      if S = '  private' then Location := flPrivate
      else if S = '  protected' then Location := flProtected
      else if S = '  public' then Location := flPublic
      else if S = '  published' then Location := flPublished
      else begin
        Flags := [];
        if Pos('  procedure ', S) > 0
        then begin
          Name := GetPart(['procedure '], [' ', ';'], S);
          DBGType := TGDBType.Create(
            skProcedure,
            TGDBTypes.CreateFromCSV(GetPart(['('], [')'], S))
          );
          if GetPart(['; '], [';'], S) = 'virtual'
          then Flags := [ffVirtual];
        end
        else if Pos('  destructor  ~', S) > 0
        then begin
          Name := GetPart(['destructor  ~'], [' ', ';'], S);
          DBGType := TGDBType.Create(
            skProcedure,
            TGDBTypes.CreateFromCSV(GetPart(['('], [')'], S))
          );
          if GetPart(['; '], [';'], S) = 'virtual'
          then Flags := [ffVirtual];
          Include(Flags, ffDestructor);
        end
        else if Pos('  constructor ', S) > 0
        then begin
          Name := GetPart(['constructor '], [' ', ';'], S);
          DBGType := TGDBType.Create(
            skFunction,
            TGDBTypes.CreateFromCSV(GetPart(['('], [')'], S)),
            TGDBType.Create(skSimple, GetPart([' : '], [';'], S))
          );
          if GetPart(['; '], [';'], S) = 'virtual'
          then Flags := [ffVirtual];
          Include(Flags, ffConstructor);
        end
        else if Pos('  function ', S) > 0
        then begin
          Name := GetPart(['function  '], [' ', ';'], S);
          DBGType := TGDBType.Create(
            skFunction,
            TGDBTypes.CreateFromCSV(GetPart(['('], [')'], S)),
            TGDBType.Create(skSimple, GetPart([' : '], [';'], S))
          );
          if GetPart(['; '], [';'], S) = 'virtual'
          then Flags := [ffVirtual];
        end
        else begin
          Name := GetPart(['    '], [' '], S);
          S2 := GetPart([' : '], [';'], S);
          l := Length(S2);
          j := 1;
          while true do begin
            while (j <= l) and (S2[j] in ['^','(', ' ']) do inc(j);
            if (lowercase(copy(S2, j, 6)) = 'array ') then begin
              inc(j, 5+3);
              while (j <= l) and
                    not ( (S2[j-3] = ' ') and (S2[j-2] in ['o','O']) and (S2[j-1] in ['f','F']) and (S2[j] = ' ') )
              do
                inc(j);
              continue;
            end;
            break;
          end;
          if (lowercase(copy(S2, j, 7)) = 'record ') and
             not( (copy(S2, j+7, 1) = ';') or (copy(S2, j+7, 6) = '{...};') )
          then begin
            i := 1;
            while (n <  Lines.Count - 2) and (i > 0) do
            begin
              inc(n);
              S := Lines[n];
              if S = '' then Continue;
              j := pos(': record ', S);
              if (j > 0) and not( (copy(S, j+9, 1) = ';') or (copy(S, j+9, 6) = '{...};') )
              then
                inc(i);
              S := Trim(S);
              if (pos('end;', S) = 1) or (pos('end)', S) = 1) then dec(i);
              S2 := S2 + ' ' + S;
            end;
          end;
          DBGType := TGDBType.Create(skSimple, S2);
        end;
        FFields.Add(TDBGField.Create(Name, DBGType, Location, Flags, FTypeName));
      end;
    end;
  end;

  procedure ProcessClassPointer;
  begin
    FProcessState := gtpsClassPointer;
    if not RequireRequests([gptrWhatisExpr]) then
      exit;

      FKind := skPointer;
      SetTypNameFromReq(gptrWhatisExpr);
      Result := True;
      // ====> DONE
  end;

  procedure ProcessClassAncestor;
  var
    i: Integer;
  begin
    FProcessState := gtpsClassAncestor;

    If FTypeInfoAncestor = nil then begin
      FTypeInfoAncestor := TGDBType.CreateForExpression(FAncestor,
        FCreationFlags*[gtcfClassIsPointer, gtcfFullTypeInfo, gtcfSkipTypeName] + [gtcfExprIsType]
      );
      AddSubType(FTypeInfoAncestor);
    end;
    if not FTypeInfoAncestor.IsFinished then
      exit;

    // add ancestor
    if FTypeInfoAncestor.FFields <> nil then
      for i := 0 to FTypeInfoAncestor.FFields.Count - 1 do
        FFields.Add(FTypeInfoAncestor.FFields[i]);
    Result := True;
  end;

  procedure FinishProcessClass;
  begin
    FProcessState := gtpsFinishProcessClass;
    if (gtcfFullTypeInfo in FCreationFlags) and  not (gtcfExprIsType in FCreationFlags) then
      if not RequireRequests([gptrWhatisExpr]) then
        exit;

    // Handle Error in ptype^ as normal class
    // May need a whatis, if aliased names are needed "type TFooAlias = type TFoo"
    SetTypNameFromReq(gptrWhatisExpr, True);
    DoClass;
    if (gtcfFullTypeInfo in FCreationFlags) and (FAncestor <> '')
    then ProcessClassAncestor
    else Result := True; // ====> DONE
  end;

  procedure ProcessClass;
  var
    t: TGDBTypeProcessRequest;
    ResultList: TGDBMINameValueList;
    s: String;
    i: Integer;
  begin
    FProcessState := gtpsClass;

    if (gtcfExprIsType in FCreationFlags) then begin
      SetTypNameFromReq(gptrPTypeExpr, True);
      DoClass;
      if (gtcfFullTypeInfo in FCreationFlags) and (FAncestor <> '')
      then ProcessClassAncestor
      else Result := True; // ====> DONE
      exit;
    end;

    if saRefParam in FAttributes
    then t := gptrPTypeExprDeDeRef  // &Class (var param; dwarf)
    else t := gptrPTypeExprDeRef;   // Class

    if not RequireRequests([gptrPTypeExpr, t])
    then exit;

    if IsReqError(t)
    then debugln('Failed "ptype expr^[^]" request for class expression');

    if (not IsReqError(t)) and (ptprfPointer in FReqResults[t].Result.Flags)
    then begin
      ProcessClassPointer;
      exit;
    end
    else begin
      // before type cast
      FTypeDeclaration := PCLenToString(FReqResults[gptrPTypeExpr].Result.BaseName);

      if (gtcfAutoCastClass in FCreationFlags) then begin
        if not RequireRequests([gptrInstanceClassName]) then
          exit;
        if not IsReqError(gptrInstanceClassName) then begin
          ResultList := TGDBMINameValueList.Create(FReqResults[gptrInstanceClassName].Result.GdbDescription);
          s := ParseGDBString(ResultList.Values['value']);
          ResultList.Free;
          if s <> ''
          then i := ord(s[1])
          else i := 1;
          if i <= length(s)-1 then begin
            FAutoTypeCastName := copy(s, 2, i);
            RequireRequests([gptrPtypeCustomAutoCast], FAutoTypeCastName);
            FProcessState := gtpsClassAutoCast;
            exit;
          end;
          // continue without type cast
        end;
      end;

      FinishProcessClass;
    end;
  end;

  procedure ProcessClassAutoCast;
  var
    s: String;
  begin
    if IsReqError(gptrPtypeCustomAutoCast) or
       (not(FReqResults[gptrPtypeCustomAutoCast].Result.Kind = ptprkClass)) or
       (LowerCase(FAutoTypeCastName) = LowerCase(PCLenToString(FReqResults[gptrPTypeExpr].Result.BaseName))) // don't typecast to itself
    then begin
      FinishProcessClass; // normal class finish
      exit;
    end;

    s := FAutoTypeCastName + '(' + FExpression + ')';
    if not RequireRequests([gptrPtypeCustomAutoCast2], s)
    then exit;

    if FHasAutoTypeCastFix
    then s := '^' + s;

    if IsReqError(gptrPtypeCustomAutoCast2) and (not FHasAutoTypeCastFix)
    then begin
      s := '^' + s;
      FHasAutoTypeCastFix := True;
      exclude(FProccesReuestsMade, gptrPtypeCustomAutoCast2);
      RequireRequests([gptrPtypeCustomAutoCast2], s);
      exit;
    end;

    if IsReqError(gptrPtypeCustomAutoCast2) or
       not(FReqResults[gptrPtypeCustomAutoCast2].Result.Kind = ptprkClass)
    then begin
      FinishProcessClass; // normal class finish
      exit;
    end;

    FExpression := s;
    FPTypeExpression := FExpression; // TODO: keep FPTypeExpression

    FReqResults[gptrPTypeExpr] := FReqResults[gptrPtypeCustomAutoCast2];
    exclude(FProccesReuestsMade, gptrWhatisExpr);
    FinishProcessClass;
  end;
  {%endregion    * Class * }

  {%region    * Array * }
  procedure ProcessArray;
  var
    PTypeResult: TGDBPTypeResult;
  begin
    FProcessState := gtpsArray;

    PTypeResult := FReqResults[gptrPTypeExpr].Result;
    // In DWARF, some Dynamic Array, are pointer to there base type
    if (ptprfPointer in PTypeResult.Flags) and (PTypeResult.Kind =ptprkSimple)
    then begin
      if not RequireRequests([gptrPTypeExprDeRef])
      then exit;
      if (not IsReqError(gptrPTypeExprDeRef)) then
      PTypeResult := FReqResults[gptrPTypeExprDeRef].Result;
      // This implies it is an internal pointer
      if (ptprfDynArray in PTypeResult.Flags)
      then include(FAttributes, saInternalPointer);
    end;

    if (PTypeResult.Flags * [ptprfDynArray, ptprfPointer] =  [ptprfDynArray, ptprfPointer])
    then include(FAttributes, saInternalPointer);

    if (saInternalPointer in FAttributes) then begin
      if not RequireRequests([gptrPTypeExprDeRef])
      then exit;
    end;

    if (saInternalPointer in FAttributes) and (not IsReqError(gptrPTypeExprDeRef)) then
      PTypeResult := FReqResults[gptrPTypeExprDeRef].Result
    else
      PTypeResult := FReqResults[gptrPTypeExpr].Result;

    if ptprfPointer in PTypeResult.Flags then begin
      ProcessSimplePointer;
      exit;
    end;

    FKind := skSimple;
    if (ptprfDynArray in PTypeResult.Flags)
    then include(FAttributes, saDynArray)
    else include(FAttributes, saArray);

    if not(gtcfSkipTypeName in FCreationFlags) then begin
      if not RequireRequests([gptrWhatisExpr])
      then exit;
      SetTypNameFromReq(gptrWhatisExpr, True);
    end;

    FTypeDeclaration := ClearAmpersand(PCLenToString(PTypeResult.Declaration));
    Result := True;
    // ====> DONE
  end;
  {%endregion    * Array * }

  {%region    * Simple * }
  procedure ProcessSimplePointer;
  begin
    FProcessState := gtpsSimplePointer;
    // there may be multiply levels of pointer, get the name of this pointer
    if not RequireRequests([gptrPTypeExpr, gptrWhatisExpr])
    then exit;

    FKind := skPointer;
    if not IsReqError(gptrWhatisExpr) and (FReqResults[gptrWhatisExpr].Result.Kind = ptprkSimple)
    then begin
      // Whatis result is ok
      if (ptprfParamByRef in FReqResults[gptrWhatisExpr].Result.Flags) then
        include(FAttributes, saRefParam);
      SetTypNameFromReq(gptrWhatisExpr);
    end
    else begin
      // Whatis result failed
      SetTypNameFromReq(gptrPTypeExpr);
    end;
    Result := True;
    // ====> DONE
  end;
  {%endregion    * Simple * }

  {%region    * EvaluateExpression * }
  function GetParsedFromResult(AGdbDesc, AField: String): String;
  var
    ResultList: TGDBMINameValueList;
  begin
    ResultList := TGDBMINameValueList.Create(AGdbDesc);
    Result := ResultList.Values[AField];
    //FTextValue := DeleteEscapeChars(FTextValue);
    ResultList.Free;
  end;
  procedure ParseFromResult(AGdbDesc, AField: String);
  begin
    FExprEvaluatedAsText := GetParsedFromResult(AGdbDesc, AField);
    FHasExprEvaluatedAsText := True;
  end;
  procedure ParseFromResultForStrFixed(AGdbDesc, AField: String);
  begin
    FStringExprEvaluatedAsText := GetParsedFromResult(AGdbDesc, AField);
    FHasStringExprEvaluatedAsText := True;
  end;

  procedure EvaluateExpressionDynArrayGetData;
  var
    i, m: Integer;
    s: String;
  begin
    FProcessState := gtpsEvalExprDynArrayGetData;

    if (FLen <= 0) or (FArrayIndexValueLimit <= 0) then begin
      if FLen > 0 then
        FExprEvaluatedAsText := '(...)'
      else
        FExprEvaluatedAsText := '()';
      FHasExprEvaluatedAsText := True;
      Result := True;
      exit;
    end;

    if (Length(FArrayIndexValues) > 0) then begin
      FExprEvaluatedAsText := '';
      FFields := TDBGFields.Create;
      for i := 0 to Length(FArrayIndexValues) - 1 do begin
        s := FArrayIndexValues[i].ExprEvaluatedAsText;
        if (pos(',', s) > 0) and not(s[1] in ['(', '[', '{', '"', '''', '#']) then
          s := '{'+s+'}';
        if i > 0 then
          FExprEvaluatedAsText := FExprEvaluatedAsText + ', ';
        FExprEvaluatedAsText := FExprEvaluatedAsText + s;

        if FRepeatFirstIndex <> 0
        then m := FRepeatFirstIndex
        else m := FBoundLow;
        FFields.Add(TDBGField.Create(IntToStr(m + i), FArrayIndexValues[i], flPublic));
      end;
      if Length(FArrayIndexValues) < FLen then
        FExprEvaluatedAsText := FExprEvaluatedAsText + ', ...';
      FExprEvaluatedAsText := '(' + FExprEvaluatedAsText + ')';

      SetLength(FArrayIndexValues, 0);
      FHasExprEvaluatedAsText := True;
      Result := True;
      exit;
    end;

    if (FExprEvaluatedAsText <> '') and
       (FExprEvaluatedAsText[1] = '{') and   // gdb returned array data
       not(gtcfForceArrayEval in FCreationFlags)
    then begin
      if (FLen = 0) or
         ((Length(FExprEvaluatedAsText) > 1) and (FExprEvaluatedAsText[2] <> '}') )
      then begin
        Result := True;
        exit;
      end;
    end;

    // Get Data
    m := Min(Max(FArrayIndexValueLimit, FRepeatCount), FLen);
    SetLength(FArrayIndexValues, m);
    for i := 0 to m-1 do begin
      FArrayIndexValues[i] := TGDBType.CreateForExpression(FExpression+'['+IntToStr(FRepeatFirstIndex + i)+']',
        FCreationFlags + [gtcfExprEvaluate] - [gtcfForceArrayEval]);
      if i <= 1
      then FArrayIndexValues[i].FArrayIndexValueLimit := FArrayIndexValueLimit - 2
      else FArrayIndexValues[i].FArrayIndexValueLimit := FArrayIndexValueLimit - 3;
      AddSubType(FArrayIndexValues[i]);
    end;
  end;

  procedure EvaluateExpressionDynArray;
  begin
    FProcessState := gtpsEvalExprDynArray;
    if FExprEvaluateFormat <> wdfDefault then begin;
      Result := True;
      exit;
    end;


    FBoundLow :=  -1;
    FBoundHigh := -1;
    FLen := -1;

    if (FArrayIndexValueLimit < 0) then begin
      FExprEvaluatedAsText := '(...)';
      FHasExprEvaluatedAsText := True;
      Result := True;
      exit;
    end;

    if not RequireRequests([gptrEvalCustomEval], '^^longint('+FExpression+')[-1]') then exit;
    if not IsReqError(gptrEvalCustomEval, False) then begin
      FBoundLow :=  0;
      FBoundHigh  := StrToIntDef(GetParsedFromResult(FReqResults[gptrEvalCustomEval].Result.GdbDescription, 'value'), -1);
      FLen := FBoundHigh + 1;
    end;

    if (saInternalPointer in FAttributes) then begin
      if not RequireRequests([gptrEvalExprDeRef]) then exit;
      if not IsReqError(gptrEvalExprDeRef, False) then begin
        ParseFromResult(FReqResults[gptrEvalExprDeRef].Result.GdbDescription, 'value');
        EvaluateExpressionDynArrayGetData;
        exit;
      end;
    end;

    if (saRefParam in FAttributes) then begin
      if not RequireRequests([gptrEvalExprCast]) then exit;
      if not IsReqError(gptrEvalExprCast, False) then begin
        ParseFromResult(FReqResults[gptrEvalExprCast].Result.GdbDescription, 'value');
        EvaluateExpressionDynArrayGetData;
        exit;
      end;
    end;

    if not RequireRequests([gptrEvalExpr]) then exit;
    if not IsReqError(gptrEvalExpr, False) then begin
      ParseFromResult(FReqResults[gptrEvalExpr].Result.GdbDescription, 'value');
      EvaluateExpressionDynArrayGetData;
      exit;
    end;

    if FLen > 0 then begin
      EvaluateExpressionDynArrayGetData;
      exit;
    end;

    // TODO: set Validity = error
    ParseFromResult(FReqResults[gptrEvalExpr].Result.GdbDescription, 'msg');
    Result := True;
  end;

  procedure EvaluateExpressionArray;
  var
    PTypeResult: TGDBPTypeResult;
  begin
    FProcessState := gtpsEvalExprArray;
    if FExprEvaluateFormat <> wdfDefault then begin;
      Result := True;
      exit;
    end;

    PTypeResult := FReqResults[gptrPTypeExpr].Result;
    FBoundLow :=  PCLenToInt(PTypeResult.BoundLow);
    FBoundHigh := PCLenToInt(PTypeResult.BoundHigh);
    FLen := PCLenToInt(PTypeResult.BoundHigh) - PCLenToInt(PTypeResult.BoundLow) + 1;

    if (gtcfForceArrayEval in FCreationFlags) then begin
      EvaluateExpressionDynArrayGetData;
      exit;
    end;

    if (saInternalPointer in FAttributes) then begin
      if not RequireRequests([gptrEvalExprDeRef]) then exit;
      if not IsReqError(gptrEvalExprDeRef, False) then begin
        ParseFromResult(FReqResults[gptrEvalExprDeRef].Result.GdbDescription, 'value');
        Result := True;
        exit;
      end;
    end;

    if (saRefParam in FAttributes) then begin
      if not RequireRequests([gptrEvalExprCast]) then exit;
      if not IsReqError(gptrEvalExprCast, False) then begin
        ParseFromResult(FReqResults[gptrEvalExprCast].Result.GdbDescription, 'value');
        Result := True;
        exit;
      end;
    end;

    if not RequireRequests([gptrEvalExpr]) then exit;
    if not IsReqError(gptrEvalExpr, False) then begin
      ParseFromResult(FReqResults[gptrEvalExpr].Result.GdbDescription, 'value');
      Result := True;
      exit;
    end;

    // TODO: set Validity = error
    ParseFromResult(FReqResults[gptrEvalExpr].Result.GdbDescription, 'msg');
    Result := True;
  end;

  procedure EvaluateExpressionRepeated;
  var
    ExpArray: TGDBExpressionPartArray;
    s: String;
    Idx: Int64;
    Error: word;
    i: Integer;
  begin
    FProcessState := gtpsEvalExprRepeated;

    if (FRepeatCount < 1) then begin
      Result := True;
      exit;
    end;

    if (FRepeatCount < 1) or (FParsedExpression.PartCount <> 1) or
       not (FParsedExpression.Parts[0] is TGDBExpressionPartArray)
    then begin
      FRepeatCount := 0;
      EvaluateExpression;
      exit;
    end;

    if FRepeatCountEval <> nil then begin
      if not FRepeatCountEval.HasExprEvaluatedAsText then begin
        FRepeatCount := 0;
        EvaluateExpression;
        exit;
      end;
      FExprEvaluatedAsText := FRepeatCountEval.ExprEvaluatedAsText;
      FFields.Free;
      FFields := TDBGFields.Create;
      for i := 0 to FRepeatCountEval.Fields.Count - 1 do
        FFields.Add(FRepeatCountEval.Fields[i]);
      FHasExprEvaluatedAsText := True;
      FreeAndNil(FRepeatCountEval);
      Result := True;
      exit;
    end;

    ExpArray := TGDBExpressionPartArray(FParsedExpression.Parts[0]);
	if ExpArray.IndexCount < 1 then begin
      FRepeatCount := 0;
      EvaluateExpression;
      exit;
    end;

    s := ExpArray.IndexPart[ExpArray.IndexCount - 1].GetPlainText;
    if not RequireRequests([gptrEvalExpr2], Quote('('+s+')+0')) then exit;

    if IsReqError(gptrEvalExpr2, False) then begin
      FRepeatCount := 0;
      EvaluateExpression;
      exit;
    end;

    s := GetParsedFromResult(FReqResults[gptrEvalExpr2].Result.GdbDescription, 'value');
    Val(s, Idx, Error);

    if Error <> 0  then begin
      FRepeatCount := 0;
      EvaluateExpression;
      exit;
    end;

    FRepeatCountEval := TGDBType.CreateForExpression(
      ExpArray.GetTextToIdx(ExpArray.IndexCount-2),
      FCreationFlags + [gtcfExprEvaluate, gtcfForceArrayEval],
      FExprEvaluateFormat,
      FRepeatCount
    );
    FRepeatCountEval.RepeatFirstIndex := Idx;
    AddSubType(FRepeatCountEval);

  end;

  procedure EvaluateExpression;
  begin
    FProcessState := gtpsEvalExpr;

    if not(gtcfExprEvaluate in FCreationFlags) then begin
      Result := True;
      exit;
    end;

    if (FRepeatCount > 1) and (FParsedExpression.PartCount = 1) and
       (FParsedExpression.Parts[0] is TGDBExpressionPartArray) and
       not(gtcfForceArrayEval in FCreationFlags)
    then begin
      exclude(FProccesReuestsMade, gptrEvalExpr2);
      EvaluateExpressionRepeated;
      exit;
    end;

    if saDynArray in FAttributes then begin
      EvaluateExpressionDynArray;
      exit;
    end;
    if saArray in FAttributes then begin
      EvaluateExpressionArray;
      exit;
    end;

    if FExprEvaluateFormat <> wdfDefault then begin;
      Result := True;
      exit;
    end;

    if (gtcfForceArrayEval in FCreationFlags) then begin
      FBoundLow :=  FRepeatFirstIndex;
      FBoundHigh := FRepeatFirstIndex + FRepeatCount - 1;
      FLen := FRepeatCount;
      EvaluateExpressionDynArrayGetData;
      exit;
    end;

    // TODO: stringFixed need to know about:
    // - AutoTypeCast

    if (saInternalPointer in FAttributes) then begin
      if not RequireRequests([gptrEvalExprDeRef]) then exit;
      if not IsReqError(gptrEvalExprDeRef, False) then begin
        ParseFromResult(FReqResults[gptrEvalExprDeRef].Result.GdbDescription, 'value');

        if (gtcfExprEvalStrFixed in FCreationFlags) and
           (FParsedExpression <> nil) and FParsedExpression.MayNeedStringFix
        then begin
          if not RequireRequests([gptrEvalExprDeRef2], FParsedExpression.TextStrFixed) then exit;
          ParseFromResultForStrFixed(FReqResults[gptrEvalExprDeRef2].Result.GdbDescription, 'value');
        end;

        Result := True;
        exit;
      end;
    end;

    if (saRefParam in FAttributes) then begin
      if not RequireRequests([gptrEvalExprCast]) then exit;
      if not IsReqError(gptrEvalExprCast, False) then begin
        ParseFromResult(FReqResults[gptrEvalExprCast].Result.GdbDescription, 'value');

        if (gtcfExprEvalStrFixed in FCreationFlags) and
           (FParsedExpression <> nil) and FParsedExpression.MayNeedStringFix
        then begin
          if not RequireRequests([gptrEvalExprCast2], FParsedExpression.TextStrFixed) then exit;
          ParseFromResultForStrFixed(FReqResults[gptrEvalExprCast2].Result.GdbDescription, 'value');
        end;

        Result := True;
        exit;
      end;
    end;

      if not RequireRequests([gptrEvalExpr]) then exit;
      if not IsReqError(gptrEvalExpr, False) then begin
        ParseFromResult(FReqResults[gptrEvalExpr].Result.GdbDescription, 'value');

        if (gtcfExprEvalStrFixed in FCreationFlags) and
           (FParsedExpression <> nil) and FParsedExpression.MayNeedStringFix
        then begin
          if not RequireRequests([gptrEvalExpr2], FParsedExpression.TextStrFixed) then exit;
          ParseFromResultForStrFixed(FReqResults[gptrEvalExpr2].Result.GdbDescription, 'value');
        end;

        Result := True;
      exit;
    end;

      // TODO: set Validity = error
      ParseFromResult(FReqResults[gptrEvalExpr].Result.GdbDescription, 'msg');
      Result := True;
    end;
  {%endregion    * EvaluateExpression * }

  procedure ProcessInitialSimple;
  var
    i: Integer;
    PTypeResult: TGDBPTypeResult;
  begin
    FProcessState := gtpsInitialSimple;

    // TODO: ptype may be known by FParsedExpression
    if not RequireRequests([gptrPTypeExpr]) //+wi)
    then exit;

    if IsReqError(gptrPTypeExpr) then begin
       //Cannot access memory at address 0x0
      if (pos('address 0x0', FReqResults[gptrPTypeExpr].Error) > 0) and
         FParsedExpression.MayNeedTypeCastFix
      then begin
        exclude(FProccesReuestsMade, gptrPTypeExpr);
        ProcessInitial;
        exit;
      end;

      FEvalError := True;
      exit;
    end;
    PTypeResult := FReqResults[gptrPTypeExpr].Result;

    if (ptprfParamByRef in PTypeResult.Flags) then
      include(FAttributes, saRefParam);

    // In DWARF, some Dynamic Array, are pointer to there base type
    if (ptprfPointer in PTypeResult.Flags) and (PTypeResult.Kind =ptprkSimple)
    then begin
      if not RequireRequests([gptrPTypeExprDeRef])
      then exit;
      if (not IsReqError(gptrPTypeExprDeRef)) and
         (FReqResults[gptrPTypeExprDeRef].Result.Kind = ptprkArray)
      then begin
        ProcessArray;
        exit;
      end;
    end;

    case PTypeResult.Kind of
      //ptprkError: ;
      //ptprkSimple: ;
      ptprkClass: begin
          ProcessClass;
          exit;
        end;
      //ptprkRecord: ;
      //ptprkEnum: ;
      //ptprkSet: ;
      ptprkArray: begin
          ProcessArray;
          exit;
      end;
      //ptprkProcedure: ;
      //ptprkFunction: ;
    end;

    if (ptprfPointer in PTypeResult.Flags)
    and ( (PTypeResult.Kind in [ptprkSimple, ptprkRecord, ptprkEnum, ptprkSet])
          or ( (gtcfClassIsPointer in FCreationFlags) and
               (PTypeResult.Kind in [ptprkProcedure, ptprkFunction])  )
        )
    then begin
      ProcessSimplePointer;
      exit;
    end;

    if (ptprfParamByRef in PTypeResult.Flags)
    and not (PTypeResult.Kind in [ptprkError])
    then begin
      // could be a pointer // need ptype of whatis
      if not RequireRequests([gptrWhatisExpr])
      then exit;

      if (FReqResults[gptrWhatisExpr].Result.BaseName.Len > 0)
      then begin
        if not RequireRequests([gptrPTypeOfWhatis])
        then exit;

        if (not IsReqError(gptrPTypeOfWhatis, False))
        and (ptprfPointer in FReqResults[gptrPTypeOfWhatis].Result.Flags) then begin
          // pointer
          FKind := skPointer;
          SetTypNameFromReq(gptrWhatisExpr);
          Result := True;
          // ====> DONE
          exit;
        end;
      end;
    end;

    case PTypeResult.Kind of
      ptprkError: begin
          // could be empty pointer @ArgProcedure
          Result := True; // nothing to be done, keep simple type, no name
        end;
      ptprkSimple: begin
          // may only need whatis, if current name isn't usable?
          if not RequireRequests([gptrWhatisExpr])
          then exit;

          SetTypNameFromReq(gptrWhatisExpr, True);
          FKind := skSimple;
          Result := True;
          // ====> DONE
        end;
      ptprkClass: begin
          Assert(False, 'GDBTypeInfo Class: Should be handled before');
          ProcessClass;
        end;
      ptprkRecord: begin
          SetTypNameFromReq(gptrWhatisExpr, True);
          DoRecord;
          Result := True;
          // ====> DONE
        end;
      ptprkEnum: begin
          SetTypNameFromReq(gptrWhatisExpr, True);
          FTypeDeclaration := ClearAmpersand(PCLenToString(PTypeResult.Declaration));
          DoEnum;
          Result := True;
          // ====> DONE
        end;
      ptprkSet: begin
          if not RequireRequests([gptrWhatisExpr])
          then exit;

          SetTypNameFromReq(gptrWhatisExpr, True);
          // TODO: resolve enum-name (set of SomeEnum) if mode-full ?
          FTypeDeclaration := ClearAmpersand(PCLenToString(PTypeResult.Declaration));
          i := pos('set of  = ', FTypeDeclaration);
          if  i > 0 then delete(FTypeDeclaration, i+7, 3);
          DoSet;
          Result := True;
          // ====> DONE
        end;
      ptprkArray: begin
          Assert(False, 'GDBTypeInfo Array: Should be handled before');
          ProcessArray;
        end;
      ptprkProcedure: begin
          // under stabs, procedure/function are always pointer // pointer to proc/func return empty type
          if (gtcfClassIsPointer in FCreationFlags) // Dwarf
          and (ptprfPointer in PTypeResult.Flags)
          then begin
            ProcessSimplePointer;
            exit;
          end;

          if not RequireRequests([gptrWhatisExpr])
          then exit;

          SetTypNameFromReq(gptrWhatisExpr, True, 'procedure');
          DoProcedure;
          Result := True;
          // ====> DONE
        end;
      ptprkFunction: begin
          // under stabs, procedure/function are always pointer // pointer to proc/func return empty type
          if (gtcfClassIsPointer in FCreationFlags) // Dwarf
          and (ptprfPointer in PTypeResult.Flags)
          then begin
            ProcessSimplePointer;
            exit;
          end;

          if not RequireRequests([gptrWhatisExpr])
          then exit;

          SetTypNameFromReq(gptrWhatisExpr, True, 'function');
          DoFunction;
          Result := True;
          // ====> DONE
      end;
    end;
  end;

  procedure ProcessInitial;
  begin
    FProcessState := gtpsInitial;
    if FExpression = '' then begin;
      FPTypeExpression := FExpression;
      ProcessInitialSimple;
      exit;
    end;

    if FParsedExpression = nil
    then FParsedExpression := TGDBExpression.Create(FExpression);
    // Does not set FLastEvalRequest
    if FParsedExpression.NeedValidation(FEvalRequest)
    then exit;

    FExpression := FParsedExpression.Text;
    FPTypeExpression := FParsedExpression.TextEx[[toSkipArrayIdx]];

    ProcessInitialSimple;
  end;

  procedure MergeSubProcessRequests;
  var
    SubType: TGDBType;
  begin
    SubType := FFirstProcessingSubType;
    while SubType <> nil do begin
      if (FEvalRequest =  nil)
      then FEvalRequest := SubType.FEvalRequest
      else if FLastEvalRequest <> nil
      then FLastEvalRequest^.Next := SubType.FEvalRequest
      else begin
        // Find last req
        FLastEvalRequest := FEvalRequest;
        while (FLastEvalRequest^.Next <> nil) do
          FLastEvalRequest := FLastEvalRequest^.Next;
        FLastEvalRequest^.Next := SubType.FEvalRequest;
      end;
      FLastEvalRequest := SubType.FLastEvalRequest;
      SubType := SubType.FNextProcessingSubType;
    end;
  end;

  function ProcessSubProcessRequests: Boolean;
  var
    SubType, PrevSubType: TGDBType;
  begin
    DebugLnEnter(DBGMI_TYPE_INFO, ['>>Enter Sub-Requests']);
    PrevSubType := nil;
    SubType := FFirstProcessingSubType;
    while SubType <> nil do begin
      if SubType.ProcessExpression then begin
        if PrevSubType = nil
        then FFirstProcessingSubType := SubType.FNextProcessingSubType
        else PrevSubType.FNextProcessingSubType := SubType.FNextProcessingSubType;
      end
      else
        PrevSubType := SubType;
      SubType := SubType.FNextProcessingSubType;
    end;

    Result := FFirstProcessingSubType = nil;
    DebugLnExit(DBGMI_TYPE_INFO, ['>>Leave Sub-Request']);
  end;

var
  OldProcessState: TGDBTypeProcessState;
  OldReqMade: TGDBTypeProcessRequests;
  s: string;
begin
  Result := False;
  FEvalRequest := nil;
  FLastEvalRequest := nil;
  Lines := nil;
  DebugLnEnter(DBGMI_TYPE_INFO, ['>>Enter: TGDBType.ProcessExpression: ', DebugString]);
  try


  if FFirstProcessingSubType <> nil then begin
    if not ProcessSubProcessRequests then begin
      MergeSubProcessRequests;
      exit;
    end;
  end;

  OldProcessState := FProcessState;
  OldReqMade := FProccesReuestsMade;

  case FProcessState of
    gtpsInitial:            ProcessInitial;
    gtpsInitialSimple:      ProcessInitialSimple;
    gtpsSimplePointer:      ProcessSimplePointer;
    gtpsClass:              ProcessClass;
    gtpsClassAutoCast:      ProcessClassAutoCast;
    gtpsClassPointer:       ProcessClassPointer;
    gtpsFinishProcessClass: FinishProcessClass;
    gtpsClassAncestor:      ProcessClassAncestor;
    gtpsArray:              ProcessArray;
    gtpsEvalExpr:           EvaluateExpression;
    gtpsEvalExprRepeated:   EvaluateExpressionRepeated;
    gtpsEvalExprArray:      EvaluateExpressionArray;
    gtpsEvalExprDynArray:   EvaluateExpressionDynArray;
    gtpsEvalExprDynArrayGetData: EvaluateExpressionDynArrayGetData;
  end;

  FreeAndNil(Lines);
  if Result and not(FEvalStarted)
  then begin
    Result := False;
    FEvalStarted := True;
    EvaluateExpression;
  end;

  if Result
  then begin
    if FHasExprEvaluatedAsText and FMaybeShortString and
       (length(FExprEvaluatedAsText) > 0) and
       (FExprEvaluatedAsText[1] in ['''', '#']) // not a record struct
    then begin
      FTypeName := 'ShortString';
      FKind := skSimple;
      FreeAndNil(FFields);
    end;
    if Value.AsString = '' then
      Value.AsString := ExprEvaluatedAsText;

    FProcessState := gtpsFinished;
  end;

  if FFirstProcessingSubType <> nil then
    MergeSubProcessRequests
  else
  if (FProcessState = OldProcessState) and (FProccesReuestsMade = OldReqMade)
  and (not Result) and (FEvalRequest = nil)
  then begin
    debugln(DBG_WARNINGS, ['ERROR: detected state loop in ProcessExpression']);
    Result := True;
  end;
  finally
    WriteStr(s, FProcessState);
    DebugLnExit(DBGMI_TYPE_INFO, ['<<Exit:  TGDBType.ProcessExpression: state = ', s, '  Result=', dbgs(Result),
                 ' Kind=', dbgs(Kind), ' Attr=', dbgs(Attributes), ' Typename="', TypeName, '" InternTpName="', FInternalTypeName,'" TypeDeclaration="', TypeDeclaration, '"']);
  end;
end;

{ TGDBPTypes }

constructor TGDBTypes.CreateFromCSV(AValues: String);
var
  GDBType: TGDBType;
begin
  Create;
  while AValues <> '' do
  begin
    GDBType := TGDBType.Create(skSimple, GetPart([], [', '], AValues));
    FList.Add(GDBType);
    {if Length(AValues) >= 2 then} Delete(AValues, 1, 2);
  end;
end;

initialization
  DBGMI_TYPE_INFO := DebugLogger.RegisterLogGroup('DBGMI_TYPE_INFO' {$IFDEF DBGMI_TYPE_INFO} , True {$ENDIF} );
  DBG_WARNINGS := DebugLogger.FindOrRegisterLogGroup('DBG_WARNINGS' {$IFDEF DBG_WARNINGS} , True {$ENDIF} );

end.
