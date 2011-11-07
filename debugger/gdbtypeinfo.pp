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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}
unit GDBTypeInfo;
{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, Debugger, LclProc, DebugUtils, GDBMIMiscClasses;

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

  Examples: (tested with fpc 2.4.2 and 2.5.1 (Jan 2011) / gdb 7.0, gdb 7.2
  (excluding the '~"type = '  and the '\n"')

  * procedure x(ArgTFoo: TFoo; var VArgTFoo: TFoo); // TFoo = class end;
  * procedure x(ArgPFoo: PFoo; var VArgPFoo: PFoo); // PFoo = ^TFoo;

  "PType" Results (for the "???" part):
    ptype Arg<YYY>       ~"type = <???> = class : public TOBJECT \n"   ## followed by lines of fields (exlude inherited)
                       Normal          |                Param-by-ref
                 Stabs    Dwarf        |               Stabs    Dwarf   Dwarf(fpc 2.6 up)
    ArgTFoo      ^TFOO    ^TFOO        |  VArgTFoo     ^TFOO    &TFOO   ^TFOO
    ArgTFoo^      TFOO     TFOO        |  VArgTFoo^    ^TFOO    ^TFOO    TFOO
   @ArgTFoo      ^TFOO    ^TFOO        | @VArgTFoo     ^TFOO   ^&TFOO   ^TFOO

    ArgPFoo      ^TFOO    ^TFOO        |  VArgPFoo     ^TFOO    &TFOO   ^TFOO
    ArgPFoo^     ^TFOO    ^TFOO        |  VArgPFoo^    ^TFOO    ^TFOO   ^TFOO
   @ArgPFoo      ^TFOO    ^TFOO        | @VArgPFoo     ^TFOO   ^&TFOO   ^TFOO

                 Stabs    Dwarf
    TFoo         TFOO     ^TFOO
    PFoo         ^TFOO    ^TFOO

  "WhatIs" Results:     Normal         |                Param-by-ref
    - some "whatis" have a trailing "=class\n" (indicated by a "=" below
                 Stabs    Dwarf        |               Stabs    Dwarf   Dwarf(fpc 2.6 up)
    ArgTFoo      TFOO      TFOO        |  VArgTFoo     TFOO     &TFOO   TFOO
    ArgTFoo^     TFOO      TFOO=       |  VArgTFoo^    TFOO      TFOO   TFOO=
   @ArgTFoo      PFOO     ^TFOO        | @VArgTFoo     PFOO    ^&TFOO  ^TFOO    ## whatis @ArgTFoo  may be ^TFoo under Stabs if no named type PFoo exists

    ArgPFoo      PFOO      PFOO        |  VArgPFoo     PFOO     &PFOO   PFOO
    ArgPFoo^     TFOO      TFOO        |  VArgPFoo^    TFOO      PFOO   TFOO
   @ArgPFoo      PPFOO    ^PFOO        | @VArgPFoo     PPFOO   ^&PFOO  ^PFOO    ## whatis @ArgPFoo  may be ^PFoo under Stabs if no named type PPFoo exists

                 Stabs    Dwarf
    TFoo         TFOO     ^TFOO = class
    PFoo         PFOO     ^TFOO               ## requires gdb 7 (mayb 6.7)

    ==> "ptype SomeVariable" does not differ between TFoo and PFoo
    ==> dwarf ptype is the same for TFoo and PFoo (whatis can tell the diff)


  * procedure x(ArgEnum: TEnum); // TEnum = (One, Two, Three);
  * procedure x(ArgEnumSet: TEnumSet; var VArgEnumSet: TEnumSet); // TEnumSet = set of TEnum;
  * procedure x(ArgSet: TSet; var VArgSet: TSet); // TSet = Set of (Alpha, Beta, Gamma);
  * var VarEnumA: (e1,e2,e3); VarEnumSetA: set of TEnum; VarSetA: Set of (s1,s2,s3);

  "WhatIs" Results (| marks a new line / gdb starts a new line with ~"):
                 Stabs                 Dwarf                     Dwarf without -godwarfset
    ArgEnumSet   TENUMSET              TENUMSET                  TENUMSET
    VArgEnumSet  TENUMSET              &TENUMSET                 &TENUMSET
    ArgSet       TSET                  TSET                      TSET
    VArgSet      TSET                  &TSET                     &TSET
    VarEnumSetA  set of TENUM          set of |ONE..THREE        <invalid unnamed pascal type code 8>
    VarSetA      set of  = (...)       set of |S1..S3            <invalid unnamed pascal type code 8>

    TEnumSet     TENUMSET              set of |ONE..THREE        TENUMSET
    TSet         TSET                  set of |ALPHA..GAMMA      TSET

    ArgEnum      TENUM                        ## same for stabs (both)
    VarEnumA      = (...)                     ## same for stabs (both)
    TEnum        TENUN                        ## same for stabs (both)

  "PType" Results:
                 Stabs                            Dwarf                      Dwarf without -godwarfset

    ArgEnumSet   set of TENUM                     set of |ONE..THREE         TENUMSET
    VArgEnumSet  set of TENUM                     &set of |ONE..THREE        &TENUMSET
    ArgSet       set of  = (ALPHA, BETA, GAMMA)   set of |ALPHA..GAMMA       TSET
    VArgSet      set of  = (ALPHA, BETA, GAMMA)   &set of |ALPHA..GAMMA      &TSET
    VarEnumSetA  set of TENUM                     set of |ONE..THREE         <invalid unnamed pascal type code 8>
    VarSetA      set of  = (S1, S2, S3)           set of |S1..S3             <invalid unnamed pascal type code 8>

    TEnumSet     set of TENUM                     set of |ONE..THREE         TENUMSET
    TSet         set of  = (ALPHA, BETA, GAMMA)   set of |ALPHA..GAMMA       TSET

    ArgEnum      TENUM  = (ONE, TWO, THREE)      ## same for stabs (both)
    VarEnumA      = (E1, E2, E3)                 ## same for stabs (both)
    TEnum        TENUM  = (ONE, TWO, THREE)      ## same for stabs (both)

      ## Alternative new lines:  set of ONE|..THREE|      set of S1|..S3|
      ## All results can be prefixed by ^, for unamed pointertypes (^& for var param)



  TODO: functions ? Stabs seem to always add pointer; dwarf does not?

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
    // type of array entry, or set-enum
    SubName, BaseSubName: TPCharWithLen;
    SubFlags: TGDBPTypeResultFlags;
    SubKind: TGDBPTypeResultKind;
    // multi-dim array
    NestArrayCount: Integer;
    NestArray: array of record  // reverse order, last entry is first nest level
      Flags: TGDBPTypeResultFlags;
      BoundLow, BoundHigh: TPCharWithLen;
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

  { TGDBExpressionPart }

  TGDBExpressionPart = class
  protected
    FText: TPCharWithLen;
    function GetParts(Index: Integer): TGDBExpressionPart; virtual;
    function GetText: String; virtual;
    function ParseExpression(AText: PChar; ATextLen: Integer): TGDBExpressionPart;
    procedure Init; virtual;
  public
    function NeedValidation(var AReqPtr: PGDBPTypeRequest): Boolean; virtual;
  public
    constructor Create;
    function PartCount: Integer; virtual;
    property Parts[Index: Integer]: TGDBExpressionPart read GetParts;
    property Text: String read GetText;
  end;

  { TGDBExpression }

  TGDBExpression = class(TGDBExpressionPart)
  private
    FTextStr: String;
  protected
    FExpressionPart: TGDBExpressionPart;
    function GetText: String; override;
    function GetParts(Index: Integer): TGDBExpressionPart; override;
  public
    constructor CreateSimple(AText: PChar; ATextLen: Integer);
    constructor Create(AText: PChar; ATextLen: Integer);
    constructor Create(ATextStr: String);
    destructor Destroy; override;
    function PartCount: Integer; override;
  end;

  { TGDBExpressionPartBracketed }

  TGDBExpressionPartBracketed = class(TGDBExpression)
  protected
    function GetText: String; override;
  public
    constructor Create(AText: PChar; ATextLen: Integer);
  end;

  { TGDBExpressionPartListBase }

  TGDBExpressionPartListBase = class(TGDBExpressionPart)
  private
    FList: TFPList;
  protected
    function GetParts(Index: Integer): TGDBExpressionPart; override;
    function GetText: String; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure ClearShared;
    function Add(APart: TGDBExpressionPart):Integer;
    function PartCount: Integer; override;
  end;

  TGDBExpressionPartList = class(TGDBExpressionPartListBase)
  public
    function AddList(APartList: TGDBExpressionPartList):Integer;
  end;

  { TGDBExpressionPartArrayIdx }

  TGDBExpressionPartArrayIdx = class(TGDBExpressionPartBracketed)
  private
    FVarParam: Boolean;
    FPTypeReq: TGDBPTypeRequest;
    FPTypeDeRefReq: TGDBPTypeRequest;
  protected
    procedure Init; override;
    procedure InitReq(var AReqPtr: PGDBPTypeRequest; AReqText: String);
    procedure InitDeRefReq(var AReqPtr: PGDBPTypeRequest; AReqText: String);
    property VarParam: Boolean read FVarParam write FVarParam;
    property PTypeReq: TGDBPTypeRequest read FPTypeReq write FPTypeReq;
    property PTypeDeRefReq: TGDBPTypeRequest read FPTypeDeRefReq write FPTypeDeRefReq;
  end;

  { TGDBExpressionPartArray }

  TGDBExpressionPartArray = class(TGDBExpressionPartListBase)
  private
    FNeedTypeCast: Boolean;
    function GetIndexParts(Index: Integer): TGDBExpressionPartArrayIdx;
  protected
    function GetText: String; override;
    function GetTextToIdx(AIdx: Integer): String;
    function IndexCount: Integer;
    property IndexPart[Index: Integer]: TGDBExpressionPartArrayIdx read GetIndexParts;
  public
    constructor Create(ALeadExpresion: TGDBExpressionPart);
    function AddIndex(APart: TGDBExpressionPartArrayIdx):Integer;
    function NeedValidation(var AReqPtr: PGDBPTypeRequest): Boolean; override;
    property NeedTypeCast: Boolean read FNeedTypeCast write FNeedTypeCast;
  end;

  { TGDBExpressionPartCastCall }

  TGDBExpressionPartCastCall = class(TGDBExpressionPartListBase)
  private
  public
    constructor Create(ALeadExpresion: TGDBExpressionPart);
    function AddBrackets(APart: TGDBExpressionPart):Integer;
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

  TGDBPTypeRequestCache = class
  private
    FList: TFPList;
    function GetRequest(Index: Integer): TGDBPTypeRequest;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function IndexOf(AThreadId, AStackFrame: Integer; ARequest: TGDBPTypeRequest): Integer;
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
                          gtcfAutoCastClass          // Find real class of instance, and use, instead of declared class of variable
                         );
  TGDBTypeCreationFlags = set of TGDBTypeCreationFlag;

  TGDBTypeProcessState =
    (gtpsInitial, gtpsInitialSimple, gtpsInitFixTypeCast,
     gtpsSimplePointer,
     gtpsClass, gtpsClassAutoCast, gtpsClassPointer, gtpsClassAncestor,
     gtpsArray,
     gtpsEvalExpr,
     gtpsFinished
    );
  TGDBTypeProcessRequest =
    (gptrPTypeExpr, gptrWhatisExpr, gptrPTypeOfWhatis,
     gptrPTypeExprDeRef, gptrPTypeExprDeDeRef,  // "Foo^", "Foo^^"  for Foo=Object, or &Object
     gptrEvalExpr, gptrEvalExprDeRef, gptrEvalExprCast,
     gptrPtypeCustomFixCast, gptrPtypeCustomAutoCast, gptrPtypeCustomAutoCast2,
     gptrInstanceClassName
    );
  TGDBTypeProcessRequests = set of TGDBTypeProcessRequest;

  TGDBType = class(TDBGType)
  private
    FInternalTypeName: string;
  private
    FExpression, FOrigExpression: string;
    FCreationFlags: TGDBTypeCreationFlags;

    // Value-Eval
    FExprEvaluatedAsText: String;
    FHasExprEvaluatedAsText: Boolean;
    FExprEvaluateFormat: TWatchDisplayFormat;

    // Sub-Types (FNext is managed by creator / linked list)
    FFirstProcessingSubType, FNextProcessingSubType: TGDBType;
    FTypeInfoAncestor: TGDBType;
    FTypeInfoArrayExpression: TGDBType;

    // Gdb-Requests
    FEvalError: boolean;
    FEvalRequest, FLastEvalRequest: PGDBPTypeRequest;

    FProcessState: TGDBTypeProcessState;
    FProccesReuestsMade: TGDBTypeProcessRequests;
    FReqResults: Array [TGDBTypeProcessRequest] of TGDBPTypeRequest;

    FParsedExpression: TGDBExpression;

    FHasTypeCastFix, FHasAutoTypeCastFix: Boolean;
    FAutoTypeCastName: String;

    procedure AddTypeReq(var AReq :TGDBPTypeRequest; const ACmd: string = '');
    procedure AddSubType(ASubType :TGDBType);
    function GetIsFinished: Boolean;
    function RequireRequests(ARequired: TGDBTypeProcessRequests; ACustomData: String = ''): Boolean;
    function IsReqError(AReqType: TGDBTypeProcessRequest; CheckResKind: Boolean = True): Boolean;
  protected
    procedure Init; override;
  public
    constructor CreateForExpression(const AnExpression: string;
                                    const AFlags: TGDBTypeCreationFlags;
                                    AFormat: TWatchDisplayFormat = wdfDefault);
    destructor Destroy; override;
    function ProcessExpression: Boolean;
    property EvalRequest: PGDBPTypeRequest read FEvalRequest;
    property EvalError: boolean read FEvalError;
    property IsFinished: Boolean read GetIsFinished;

    property HasExprEvaluatedAsText: Boolean read FHasExprEvaluatedAsText;
    property ExprEvaluatedAsText: String read FExprEvaluatedAsText;
  public
    // InternalTypeName: include ^ for TObject, if needed
    property InternalTypeName: string read FInternalTypeName;
  end;


function CreatePTypeValueList(AResultValues: String): TStringList;
function ParseTypeFromGdb(const ATypeText: string): TGDBPTypeResult;

function dbgs(AFlag: TGDBPTypeResultFlag): string; overload;
function dbgs(AFlags: TGDBPTypeResultFlags): string; overload;
function dbgs(AKind: TGDBPTypeResultKind): string; overload;
function dbgs(AReqType: TGDBCommandRequestType): string; overload;
function dbgs(AReq: TGDBPTypeRequest): string; overload;

implementation

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
  {$IFDEF DBGMI_TYPE_INFO}
  try
  {$ENDIF}
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
      '^': include(Result.Flags, ptprfPointer);
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
            Result.NestArray[SubRes.NestArrayCount].Flags     := SubRes.Flags;
            Result.NestArray[SubRes.NestArrayCount].BoundLow  := SubRes.BoundLow;
            Result.NestArray[SubRes.NestArrayCount].BoundHigh := SubRes.BoundHigh;
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
  {$IFDEF DBGMI_TYPE_INFO}
  finally
    DebugLn(['ParseTypeFromGdb: Flags=', dbgs(Result.Flags), ' Kind=', dbgs(Result.Kind), ' Name="', PCLenToString(Result.Name),'"' ]);
  end;
  {$ENDIF}
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
  for i := low(TGDBPTypeResultFlags) to high(TGDBPTypeResultFlags) do
    if i in AFlags then begin
      if Result <> '' then Result := Result + ', ';
      Result := Result + dbgs(i);
    end;
  if Result <> '' then Result := '[' + Result + ']';
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

{ TGDBExpressionPartArrayIdx }

procedure TGDBExpressionPartArrayIdx.Init;
begin
  inherited Init;
  FPTypeReq.Result.Kind := ptprkNotEvaluated;
  FPTypeDeRefReq.Result.Kind := ptprkNotEvaluated;
  FVarParam := False;
end;

procedure TGDBExpressionPartArrayIdx.InitReq(var AReqPtr: PGDBPTypeRequest; AReqText: String);
begin
  FPTypeReq.Request := AReqText;
  FPTypeReq.Error := '';
  FPTypeReq.ReqType := gcrtPType;
  FPTypeReq.Next := AReqPtr;
  FPTypeReq.Result.Kind := ptprkError;
  AReqPtr := @FPTypeReq;
end;

procedure TGDBExpressionPartArrayIdx.InitDeRefReq(var AReqPtr: PGDBPTypeRequest;
  AReqText: String);
begin
  FPTypeDeRefReq.Request := AReqText;
  FPTypeDeRefReq.Error := '';
  FPTypeDeRefReq.ReqType := gcrtPType;
  FPTypeDeRefReq.Next := AReqPtr;
  FPTypeDeRefReq.Result.Kind := ptprkError;
  AReqPtr := @FPTypeDeRefReq;
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
end;

function TGDBExpressionPartArray.GetText: String;
begin
  Result := GetTextToIdx(IndexCount-1);
end;

function TGDBExpressionPartArray.GetTextToIdx(AIdx: Integer): String;
var
  i: Integer;
  IdxPart: TGDBExpressionPartArrayIdx;
  PTReqRes, PTDeRefReqRes, PTResult: TGDBPTypeResult;
  IsPointer, NeedTCast: Boolean;
begin
  Result := Parts[0].Text;

  if AIdx < 0 then exit;

  for i := 0 to AIdx do begin
    IdxPart := IndexPart[i];
    PTReqRes := IdxPart.PTypeReq.Result;
    PTDeRefReqRes := IdxPart.PTypeDeRefReq.Result; // only evaluated, if PTReqRes is pointer and not array


    PTResult := PTReqRes;
    if (PTReqRes.Kind <> ptprkArray) and
       (PTDeRefReqRes.Kind = ptprkArray) and (ptprfDynArray in PTDeRefReqRes.Flags)
    then begin
      PTResult := PTDeRefReqRes;
      IsPointer := True;
    end
    else
      IsPointer := ptprfPointer in PTResult.Flags;


    if not (PTResult.Kind = ptprkArray)
    then begin
      // maybe pointer with index access
      Result := Result + IdxPart.Text;
      continue;
    end;


    if PTResult.NestArrayCount > 0 then  // not yet supported
    begin
      if IsPointer
      then Result := Result + '^';
      Result := Result + IdxPart.Text;
      continue;
    end;


    NeedTCast := FNeedTypeCast and (i = IndexCount-1);

    if IsPointer
    then begin
      // dyn array
      if IdxPart.VarParam and (PTResult.SubName.Len > 0)
      //and
      //   (  NeedTCast or (i < IndexCount-1)  )
      then begin
        Result := '^' + PCLenToString(PTResult.SubName) + '(' + Result + ')';
        NeedTCast := False;
      end
      else
        Result := Result + '^';
    end;

    Result := Result + IdxPart.Text;

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
begin
  Result := Add(APart);
end;

function TGDBExpressionPartArray.NeedValidation(var AReqPtr: PGDBPTypeRequest): Boolean;
var
  i: Integer;
  IdxPart: TGDBExpressionPartArrayIdx;
  PTReq, PTDeRefReq: TGDBPTypeRequest;
begin
  Result := False;
  for i := 1 to PartCount - 1 do
    if Parts[i].NeedValidation(AReqPtr) then
      Result := True;

  if Parts[0].NeedValidation(AReqPtr)
  then begin
    Result := True;
    exit;
  end;

  i := 0;
  while i < IndexCount do begin
    IdxPart := IndexPart[i];
    PTReq := IdxPart.PTypeReq;

    if PTReq.Result.Kind = ptprkError
    then exit; // no way to find more info

    if PTReq.Result.Kind = ptprkNotEvaluated
    then begin
      IdxPart.VarParam := False;
      IdxPart.InitReq(AReqPtr, 'ptype ' + GetTextToIdx(i-1));
      Result := True;
      exit;
    end
    else
    if (not IdxPart.VarParam) and (ptprfParamByRef in PTReq.Result.Flags)
    then begin
      // FPC 2.2.4 encoded "var param" in a special way, and we need an extra deref)
      IdxPart.VarParam := True;
      IdxPart.InitReq(AReqPtr, 'ptype ' + GetTextToIdx(i-1) + '^');
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
      if IdxPart.VarParam
      then IdxPart.InitDeRefReq(AReqPtr, 'ptype ' + GetTextToIdx(i-1) + '^^')
      else IdxPart.InitDeRefReq(AReqPtr, 'ptype ' + GetTextToIdx(i-1) + '^');
      Result := True;
      exit;
    end;

    inc(i);
  end;
end;

{ TGDBExpressionPartCastCall }

constructor TGDBExpressionPartCastCall.Create(ALeadExpresion: TGDBExpressionPart);
begin
  inherited Create;
  Add(ALeadExpresion);
end;

function TGDBExpressionPartCastCall.AddBrackets(APart: TGDBExpressionPart): Integer;
begin
  Result := Add(APart);
end;

{ TGDBExpressionPartBracketed }

function TGDBExpressionPartBracketed.GetText: String;
begin
  if FExpressionPart = nil
  then Result := inherited GetText
  else Result := FText.Ptr^ + FExpressionPart.Text + (FText.Ptr + FText.Len-1)^;
end;

constructor TGDBExpressionPartBracketed.Create(AText: PChar; ATextLen: Integer);
begin
  CreateSimple(AText, ATextLen);
  FExpressionPart := ParseExpression(FText.Ptr+1, FText.Len-2);
end;

{ TGDBExpressionPart }

function TGDBExpressionPart.GetText: String;
begin
  Result := PCLenToString(FText);
end;

function TGDBExpressionPart.ParseExpression(AText: PChar; ATextLen: Integer): TGDBExpressionPart;
const
  // include "." (dots). currently there is no need to break expressions like "foo.bar"
  // Include "^" (deref)
  // do NOT include "@", it is applied after []() resolution
  WordChar = [' ', #9, 'a'..'z', 'A'..'Z', '0'..'9', '_', '.', '^'];
var
  CurPtr, EndPtr: PChar;
  CurPartPtr: PChar;

  procedure SkipSpaces;
  begin
    while (CurPtr < EndPtr) and (CurPtr^ in [#9, ' ']) do inc(CurPtr);
  end;

  procedure ScanToWordEnd;
  begin
    // include "." (dots). currently there is no need to break expressions like "foo.bar"
    // Include "^" (deref)
    while (CurPtr < EndPtr) and (CurPtr^ in WordChar)
    do inc(CurPtr);
  end;

  procedure ScanToWordStart;
  begin
    while (CurPtr < EndPtr) and not (CurPtr^ in WordChar)
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
begin
  Result := nil;
  CurPtr := AText;
  EndPtr := AText + ATextLen;

  while (CurPtr < EndPtr) and not(CurPtr^ in ['[', '(']) do inc(CurPtr);
  if CurPtr = EndPtr then exit; // no fixup needed

  CurPtr := AText;
  CurArray := nil;
  CurList:= TGDBExpressionPartList.Create;

  while CurPtr < EndPtr do begin

    if CurPtr^ in WordChar
    then begin
      if CurArray <> nil then CurArray.NeedTypeCast := True;
      CurArray := nil;
      CurPartPtr := CurPtr;
      ScanToWordEnd;
      CurList.Add(TGDBExpression.CreateSimple(CurPartPtr, CurPtr - CurPartPtr));
    end
    else
    if (CurList.PartCount > 0) and (CurPtr^ = '[')
    then begin
      if CurArray <> nil then CurArray.NeedTypeCast := True;
      CurArray := TGDBExpressionPartArray.Create(MoveListToCopy(CurList));
      CurList.Add(CurArray);
      while (CurPtr^ = '[') do begin
        CurPartPtr := CurPtr;
        if not ScanToIndexEnd then break; // broken expression, do not attempt to do anything
        CurArray.AddIndex(TGDBExpressionPartArrayIdx.Create(CurPartPtr, CurPtr - CurPartPtr));
        SkipSpaces;
      end;
    end
    else
    if (CurList.PartCount > 0) and (CurPtr^ = '(')
    then begin
      if CurArray <> nil then CurArray.NeedTypeCast := True;
      CurArray := nil;
      CurCast := TGDBExpressionPartCastCall.Create(MoveListToCopy(CurList));
      CurList.Add(CurCast);
      CurPartPtr := CurPtr;
      if not ScanToCallCastEnd then break; // broken expression, do not attempt to do anything
      CurCast.AddBrackets(TGDBExpressionPartBracketed.Create(CurPartPtr, CurPtr - CurPartPtr));
    end
    else begin
      CurArray := nil;
      CurPartPtr := CurPtr;
      ScanToWordStart;
      CurList.Add(TGDBExpression.CreateSimple(CurPartPtr, CurPtr - CurPartPtr));
      AddExpPart(CurList);
    end;

  end;

  AddExpPart(CurList);
  CurList.Free;

  if CurPtr < EndPtr then debugln(['Scan aborted: ', PCLenToString(FText)]);
  if CurPtr < EndPtr then FreeAndNil(Result);
end;

procedure TGDBExpressionPart.Init;
begin
  //
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

constructor TGDBExpressionPart.Create;
begin
  Init;
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

function TGDBExpressionPartListBase.GetText: String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to PartCount - 1 do
    Result := Result + Parts[i].Text;
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

function TGDBExpressionPartListBase.PartCount: Integer;
begin
  Result := FList.Count;
end;

{ TGDBExpression }

function TGDBExpression.GetText: String;
begin
  if FExpressionPart = nil
  then Result := inherited GetText
  else Result := FExpressionPart.Text;
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

{ TGDBPTypeRequestCache }

function TGDBPTypeRequestCache.GetRequest(Index: Integer): TGDBPTypeRequest;
begin
  Result := TGDBPTypeRequestCacheEntry(FList[Index]).FRequest;
end;

constructor TGDBPTypeRequestCache.Create;
begin
  FList := TFPList.Create;
end;

destructor TGDBPTypeRequestCache.Destroy;
begin
  Clear;
  inherited Destroy;
  FreeAndNil(FList);
end;

procedure TGDBPTypeRequestCache.Clear;
begin
  while FList.Count > 0 do begin
    TGDBPTypeRequestCacheEntry(FList[0]).Free;
    FList.Delete(0);
  end;
end;

function TGDBPTypeRequestCache.IndexOf(AThreadId, AStackFrame: Integer;
  ARequest: TGDBPTypeRequest): Integer;
var
  e: TGDBPTypeRequestCacheEntry;
begin
  Result := FList.Count - 1;
  while Result >= 0 do begin
    e := TGDBPTypeRequestCacheEntry(FList[Result]);
    if (e.ThreadId = AThreadId) and (e.StackFrame = AStackFrame) and
       (e.Request.Request =ARequest.Request) and
       (e.Request.ReqType =ARequest.ReqType)
    then
      exit;
    dec(Result);
  end;
end;

procedure TGDBPTypeRequestCache.Add(AThreadId, AStackFrame: Integer;
  ARequest: TGDBPTypeRequest);
var
  e: TGDBPTypeRequestCacheEntry;
begin
  e := TGDBPTypeRequestCacheEntry.Create;
  e.FThreadId := AThreadId;
  e.FStackFrame := AStackFrame;
  e.FRequest := ARequest;
  e.FRequest.Next := nil;
  FList.Add(e);
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

  function ApplyBrackets(e: string): string;
  var
    i: Integer;
    f: Boolean;
  begin
    Result := e;
    if (e='') or ( (e[1] = '(') and (e[length(e)] = ')') ) then exit;
    f := False;
    for i := 1 to length(e) do
      f := f or not(e[i] in ['a'..'z', 'A'..'Z', '0'..'9', '_']);
    if f then
      Result := '(' + Result + ')';
  end;

  function GetReqText(AReq: TGDBTypeProcessRequest): String;
  begin
    case areq of
      gptrPTypeExpr:        Result := 'ptype ' + FExpression;
      gptrWhatisExpr:       Result := 'whatis ' + FExpression;
      gptrPTypeOfWhatis:    Result := 'ptype ' + PCLenToString(FReqResults[gptrWhatisExpr].Result.BaseName);
      gptrPTypeExprDeRef:   Result := 'ptype ' + ApplyBrackets(FExpression) + '^';
      gptrPTypeExprDeDeRef: Result := 'ptype ' + ApplyBrackets(FExpression) + '^^';
      gptrEvalExpr:      Result := '-data-evaluate-expression '+FExpression;
      gptrEvalExprDeRef: Result := '-data-evaluate-expression '+FExpression+'^';
      gptrEvalExprCast:  Result := '-data-evaluate-expression '+InternalTypeName+'('+FExpression+')';
      gptrPtypeCustomFixCast, gptrPtypeCustomAutoCast, gptrPtypeCustomAutoCast2:
                         Result := 'ptype ' + ACustomData;
      gptrInstanceClassName: Result := '-data-evaluate-expression (^^^char('+FExpression+')^+3)^';
    end;
  end;

var
  NeededReq: TGDBTypeProcessRequests;
  i: TGDBTypeProcessRequest;
begin
  NeededReq := ARequired - FProccesReuestsMade;
  Result := NeededReq = [];
  if Result then exit;

  if (gptrPTypeOfWhatis in NeededReq) and not (gptrWhatisExpr in FProccesReuestsMade)
  then begin
    Exclude(NeededReq, gptrPTypeOfWhatis);
    Include(NeededReq, gptrWhatisExpr);
  end;

  FProccesReuestsMade := FProccesReuestsMade + NeededReq;
  for i := low(TGDBTypeProcessRequest) to high(TGDBTypeProcessRequest) do
    if i in NeededReq then begin
      AddTypeReq(FReqResults[i], GetReqText(i));
      if i in [gptrEvalExpr, gptrEvalExprDeRef, gptrEvalExprCast, gptrInstanceClassName]
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
end;

constructor TGDBType.CreateForExpression(const AnExpression: string;
  const AFlags: TGDBTypeCreationFlags; AFormat: TWatchDisplayFormat = wdfDefault);
begin
  Create(skSimple, ''); // initialize
  FInternalTypeName := '';
  FEvalError := False;
  FExpression := AnExpression;
  FOrigExpression := FExpression;
  FCreationFlags := AFlags;
  FExprEvaluateFormat := AFormat;
  FEvalRequest := nil;
  FFirstProcessingSubType := nil;
  FNextProcessingSubType := nil;
  FProcessState := gtpsInitial;
  FHasExprEvaluatedAsText := False;
  FHasTypeCastFix := False;
  FHasAutoTypeCastFix := False;
  FAutoTypeCastName := '';
end;

destructor TGDBType.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FTypeInfoAncestor);
  FreeAndNil(FTypeInfoArrayExpression);
  FreeAndNil(FParsedExpression);
end;

function TGDBType.ProcessExpression: Boolean;
var
  Lines: TStringList;
  procedure ProcessInitialSimple; forward;
  procedure ProcessSimplePointer; forward;


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
    if (FTypeName = 'Variant') or
       (FTypeName = 'VARIANT') then
      FKind := skVariant
    else
    if (FTypeName = 'ShortString') or
       (FTypeName = 'SHORTSTRING') or
       (FTypeName = '&ShortString') then
      FKind := skSimple
    else
      FKind := skRecord;

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
  end;
  {%endregion    * Record * }

  {%region    * Class * }
  procedure DoClass;
  var
    n, i: Integer;
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
      if S = '' then Continue;
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
          if (lowercase(copy(S2, 1, 7)) = 'record ') then begin
            i := 1;
            while (n <  Lines.Count - 2) and (i > 0) do
            begin
              inc(n);
              S := Lines[n];
              if S = '' then Continue;
              if pos(': record ', S) > 0 then inc(i);
              if pos(' end;', S) > 0 then dec(i);
              S2 := S2 + ' ' + Trim(S);
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
      RequireRequests([gptrPtypeCustomAutoCast2], FExpression);
      exit;
    end;

    if IsReqError(gptrPtypeCustomAutoCast2) or
       not(FReqResults[gptrPtypeCustomAutoCast2].Result.Kind = ptprkClass)
    then begin
      FinishProcessClass; // normal class finish
      exit;
    end;

    FExpression := s;
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
  procedure EvaluateExpression;
    procedure ParseFromResult(AGdbDesc, AField: String);
    var
      ResultList: TGDBMINameValueList;
    begin
      ResultList := TGDBMINameValueList.Create(AGdbDesc);
      FExprEvaluatedAsText := ResultList.Values[AField];
      FHasExprEvaluatedAsText := True;
      //FTextValue := DeleteEscapeChars(FTextValue);
      ResultList.Free;
    end;
  begin
    FProcessState := gtpsEvalExpr;
    if not(gtcfExprEvaluate in FCreationFlags) then begin
      Result := True;
      exit;
    end;
    if FExprEvaluateFormat <> wdfDefault then begin;
      Result := True;
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
  {%endregion    * EvaluateExpression * }

  procedure ProcessInitialSimple;
  var
    i, j: Integer;
    PTypeResult: TGDBPTypeResult;
  begin
    FProcessState := gtpsInitialSimple;

    (* Will get gptrWhatisExpr later, it may fail, if expression is a typecast and needs ^ prefix *)
    //if (gtcfFullTypeInfo in FCreationFlags)
    //and not (gtcfExprIsType in FCreationFlags)
    //then wi := [gptrWhatisExpr]
    //else wi := [];
    if not RequireRequests([gptrPTypeExpr]) //+wi)
    then exit;

    if IsReqError(gptrPTypeExpr) then begin
      // maybe a typecast
      if not FHasTypeCastFix then begin
        j := length(FExpression);
        i := 1;
        while (i < j) and (FExpression[i] in ['a'..'z', 'A'..'Z', '0'..'9', '_']) do inc(i);
        if (i <= j) and (i > 1) and (FExpression[i] = '(')
        then begin
          RequireRequests([gptrPtypeCustomFixCast], copy(FExpression, 1, i-1));
          FProcessState := gtpsInitFixTypeCast;
          exit;
        end;
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
    if FExpression = '' then begin;
      ProcessInitialSimple;
      exit;
    end;

    if FParsedExpression = nil
    then FParsedExpression := TGDBExpression.Create(FExpression);
    // Does not set FLastEvalRequest, so there can be no MergeSubProcessRequests
    if FParsedExpression.NeedValidation(FEvalRequest)
    then exit;

    FExpression := FParsedExpression.Text;

    ProcessInitialSimple;
  end;

  procedure ProcessInitFixTypeCast;
  begin
    // Some GDB can not process: "TFoo(SomeFoo).Bar"
    // Because TFoo Is actually ^TFoo, therefore they need "^TFoo(SomeFoo).Bar"
    if FHasTypeCastFix or IsReqError(gptrPtypeCustomFixCast) or
       not(FReqResults[gptrPtypeCustomFixCast].Result.Kind = ptprkClass)
    then begin
      FEvalError := True;
      exit;
    end;

    FHasTypeCastFix := True;
    FExpression := '^' + FExpression;

    // Redo the ptype
    exclude(FProccesReuestsMade, gptrPTypeExpr);
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
      else FLastEvalRequest^.Next := SubType.FEvalRequest;;
      FLastEvalRequest := SubType.FLastEvalRequest;
      SubType := SubType.FNextProcessingSubType;
    end;
  end;

  function ProcessSubProcessRequests: Boolean;
  var
    SubType, PrevSubType: TGDBType;
  begin
    Result := False;
    PrevSubType := nil;
    SubType := FFirstProcessingSubType;
    Result := SubType = nil;
    while SubType <> nil do begin
      if SubType.ProcessExpression then begin
        Result := True;
        if PrevSubType = nil
        then FFirstProcessingSubType := SubType.FNextProcessingSubType
        else PrevSubType.FNextProcessingSubType := SubType.FNextProcessingSubType;
      end;
      PrevSubType := SubType;
      SubType := SubType.FNextProcessingSubType;
    end;
  end;

var
  OldProcessState: TGDBTypeProcessState;
  OldReqMade: TGDBTypeProcessRequests;
  {$IFDEF DBGMI_TYPE_INFO}
  s: string;
  {$ENDIF}
begin
  Result := False;
  FEvalRequest := nil;
  FLastEvalRequest := nil;
  Lines := nil;
  {$IFDEF DBGMI_TYPE_INFO}
  WriteStr(s, FProcessState);
  DebugLnEnter(['>>Enter: TGDBType.ProcessExpression: state = ', s, '   Expression="', FExpression, '"']);
  try
  {$ENDIF}


  if FFirstProcessingSubType <> nil then begin
    if not ProcessSubProcessRequests then begin
      MergeSubProcessRequests;
      exit;
    end;
  end;

  OldProcessState := FProcessState;
  OldReqMade := FProccesReuestsMade;

  case FProcessState of
    gtpsInitial:         ProcessInitial;
    gtpsInitialSimple:   ProcessInitialSimple;
    gtpsInitFixTypeCast: ProcessInitFixTypeCast;
    gtpsSimplePointer:   ProcessSimplePointer;
    gtpsClass:           ProcessClass;
    gtpsClassAutoCast:   ProcessClassAutoCast;
    gtpsClassPointer:    ProcessClassPointer;
    gtpsClassAncestor:   ProcessClassAncestor;
    gtpsArray:           ProcessArray;
    gtpsEvalExpr:        EvaluateExpression;
  end;

  FreeAndNil(Lines);
  if Result and not(FProcessState = gtpsEvalExpr)
  then begin
    Result := False;
    EvaluateExpression;
  end;

  if Result
  then FProcessState := gtpsFinished;

  if FFirstProcessingSubType <> nil then
    MergeSubProcessRequests
  else
  if (FProcessState = OldProcessState) and (FProccesReuestsMade = OldReqMade)
  and (not Result) and (FEvalRequest = nil)
  then begin
    debugln('ERROR: detected state loop in ProcessExpression');
    Result := True;
  end;
  {$IFDEF DBGMI_TYPE_INFO}
  finally
    WriteStr(s, FProcessState);
    DebugLnExit(['<<Exit:  TGDBType.ProcessExpression: state = ', s, '  Result=', dbgs(Result),
                 ' Kind=', dbgs(Kind), ' Attr=', dbgs(Attributes), ' Typename="', TypeName, '" InternTpName="', FInternalTypeName,'"']);
  end;
  {$ENDIF}
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

end.
