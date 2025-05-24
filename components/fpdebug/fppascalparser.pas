{
 ---------------------------------------------------------------------------
 FpPascalParser.pas  -  Native Freepascal debugger - Parse pascal expressions
 ---------------------------------------------------------------------------

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
unit FpPascalParser;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}
{$IFDEF INLINE_OFF}{$INLINE OFF}{$ENDIF}
{$IF FPC_Fullversion=30202}{$Optimization NOPEEPHOLE}{$ENDIF}
{$TYPEDADDRESS on}
{$SafeFPUExceptions off}

interface

uses
  Classes, sysutils, math, fgl, DbgIntfBaseTypes, LazDebuggerIntfFloatTypes,
  FpDbgInfo, FpdMemoryTools, FpErrorMessages,
  FpDbgDwarf, FpWatchResultData, FpDbgClasses,
  {$ifdef FORCE_LAZLOGGER_DUMMY} LazLoggerDummy {$else} LazLoggerBase {$endif},
  LazClasses;

const
  MAX_ERR_EXPR_QUOTE_LEN = 200;

type

  TFpPascalExpressionPartList= class;

  TFpPascalExpression = class;
  TFpPascalExpressionPart = class;
  TFpPascalExpressionPartContainer = class;
  TFpPascalExpressionPartWithPrecedence = class;
  TFpPascalExpressionPartBracket = class;
  TFpPascalExpressionPartOperator = class;
  TFpPascalExpressionPartIntrinsicBase = class;
  TFpPascalExpressionPartOperatorArraySlice = class;
  PFpPascalExpressionPartOperatorArraySlice = ^TFpPascalExpressionPartOperatorArraySlice;

  TFpPascalExpressionPartClass = class of TFpPascalExpressionPart;
  TFpPascalExpressionPartBracketClass = class of TFpPascalExpressionPartBracket;

  TSeparatorType = (ppstComma);

  TFpIntrinsicPrefix = (ipColon, ipExclamation, ipNoPrefix);
  TFpIntrinsicFunc = (
    ifErrorNotFound,
    ifChildClass,
    ifTry, ifTryN,
    ifObj,
    ifFlatten, ifFlattenPlaceholder,
    ifLength, ifRefCount, ifPos, ifSubStr, ifLower, ifUpper,
    ifOrd,
    ifRound, ifTrunc, ifSqrt, ifPi, ifLn, ifLog, ifSin, ifCos, ifTan
  );

  TFpPascalParserGetSymbolForIdentProc = function(APart: TFpPascalExpressionPart; AnIdent: String): TFpValue of object;
  TFpPascalParserGetIntrinsicForIdentProc = function(AnExpression: TFpPascalExpression; AStart: PChar; ALen: Integer): TFpPascalExpressionPartIntrinsicBase of object;

  TFpPascalParserCallFunctionProc = function (AnExpressionPart: TFpPascalExpressionPart;
    AFunctionValue: TFpValue; ASelfValue: TFpValue; AParams: TFpPascalExpressionPartList;
    out AResult: TFpValue; var AnError: TFpError): boolean of object;

  { TFpPascalExpressionSharedData }

  TFpPascalExpressionSharedData = class(TRefCountedObject)
  // Data used while EVALUATING the expression result
  strict private
    FTextExpression: String;
    FScope: TFpDbgSymbolScope;
    FError: TFpError;

    FAutoDeref: Boolean;
    FFixPCharIndexAccess: Boolean;
    FOnFunctionCall: TFpPascalParserCallFunctionProc;
    FGlobalCache: TFpDbgDataCache;

    function GetTextExpressionAddr: PChar; inline;
    function GetValid: Boolean;
  protected
    FHasPCharIndexAccess: Boolean;
  public
    constructor Create(ATextExpression: String; AScope: TFpDbgSymbolScope);
    destructor Destroy; override;

    function GetDbgSymbolForIdentifier({%H-}AnIdent: String; AFindFlags: TFindExportedSymbolsFlags = []): TFpValue;
    function GetRegisterValue({%H-}AnIdent: String): TFpValue;

    procedure SetError(AMsg: String);  // deprecated;
    procedure SetError(AnErrorCode: TFpErrorCode; const AnNestedErr: TFpError = nil);
    procedure SetError(AnErrorCode: TFpErrorCode; AData: array of const; const AnNestedErr: TFpError = nil);
    procedure SetError(const AnErr: TFpError);
    procedure ClearError;
    function PosFromPChar(APChar: PChar): Integer;

    property TextExpression: String read FTextExpression;
    property TextExpressionAddr: PChar read GetTextExpressionAddr;
    property Scope: TFpDbgSymbolScope read FScope;
    property Error: TFpError read FError;
    property Valid: Boolean read GetValid;

    (* *** CONFIG *** *)
    property AutoDeref: Boolean read FAutoDeref write FAutoDeref;
    property HasPCharIndexAccess: Boolean read FHasPCharIndexAccess;    // Set by GetResultValue
    property FixPCharIndexAccess: Boolean read FFixPCharIndexAccess write FFixPCharIndexAccess;   // handle pchar as string (adjust index)
    property OnFunctionCall: TFpPascalParserCallFunctionProc read FOnFunctionCall write FOnFunctionCall;
    property GlobalCache: TFpDbgDataCache read FGlobalCache write FGlobalCache;
  end;

  { TFpPascalExpression }

  TFpPascalExpression = class
  strict private
    FSharedData: TFpPascalExpressionSharedData;

    function GetAutoDeref: Boolean; inline;
    function GetFixPCharIndexAccess: Boolean; inline;
    function GetHasPCharIndexAccess: Boolean; inline;
    function GetGlobalCache: TFpDbgDataCache; inline;
    function GetOnFunctionCall: TFpPascalParserCallFunctionProc; inline;
    procedure SetAutoDeref(AValue: Boolean); inline;
    procedure SetFixPCharIndexAccess(AValue: Boolean); inline;
    procedure SetGlobalCache(AValue: TFpDbgDataCache); inline;
    procedure SetOnFunctionCall(AValue: TFpPascalParserCallFunctionProc); inline;

    function GetError: TFpError; inline;
    function GetValid: Boolean; inline;
  strict private
    FOnFindIntrinsc: TFpPascalParserGetIntrinsicForIdentProc;
    FIntrinsicPrefix: TFpIntrinsicPrefix;
    FExpressionPart: TFpPascalExpressionPart;
    FPreviousArraySlice: TFpPascalExpressionPartOperatorArraySlice;

    function GetResultValue: TFpValue;

    function LookupIntrinsic(AStart: PChar; ALen: Integer): TFpPascalExpressionPart;
  protected
    procedure SetError(AnErrorCode: TFpErrorCode; const AnNestedErr: TFpError = nil);
    procedure SetError(AnErrorCode: TFpErrorCode; AData: array of const; const AnNestedErr: TFpError = nil);
  protected
    property ExpressionPart: TFpPascalExpressionPart read FExpressionPart;
  public
    constructor Create(ATextExpression: String; AScope: TFpDbgSymbolScope; ASkipParse: Boolean = False);
    destructor Destroy; override;
    procedure Parse;
    function DebugDump(AWithResults: Boolean = False): String;
    procedure ResetEvaluation;
//    property TextExpression: String read GetTextExpression;
    property Error: TFpError read GetError;
    property Valid: Boolean read GetValid;

    // ResultValue
    // - May be a type, if expression is a type
    // - Only valid, as long as the expression is not destroyed
    property ResultValue: TFpValue read GetResultValue;

    (* *** CONFIG *** *)
    // Parser-only
    property IntrinsicPrefix: TFpIntrinsicPrefix read FIntrinsicPrefix write FIntrinsicPrefix;
    property OnFindIntrinsc: TFpPascalParserGetIntrinsicForIdentProc read FOnFindIntrinsc write FOnFindIntrinsc;
    // Shared, available during evaluation
    property AutoDeref: Boolean read GetAutoDeref write SetAutoDeref;
    property HasPCharIndexAccess: Boolean read GetHasPCharIndexAccess;  // Set by GetResultValue
    property FixPCharIndexAccess: Boolean read GetFixPCharIndexAccess write SetFixPCharIndexAccess;   // handle pchar as string (adjust index)
    property OnFunctionCall: TFpPascalParserCallFunctionProc read GetOnFunctionCall write SetOnFunctionCall;
    property GlobalCache: TFpDbgDataCache read GetGlobalCache write SetGlobalCache;

    property SharedData: TFpPascalExpressionSharedData read FSharedData;
  end;


  { TFpPascalExpressionPartList }

  TFpPascalExpressionPartList = class(TStrings)
  public // TStrings
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;
  protected // TStrings
    function Get(Index: Integer): string; override;
    //function GetCount: Integer; virtual; abstract;
  protected
    function GetItems(AIndex: Integer): TFpPascalExpressionPart; virtual; abstract;
  public
    //property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TFpPascalExpressionPart read GetItems;
  end;

  TFindInParentsFlag = (fipIncludeBracketFunction);
  TFindInParentsFlags = set of TFindInParentsFlag;

  { TFpPascalExpressionPart }

  TFpPascalExpressionPart = class
  private
    FExpressionData: TFpPascalExpressionSharedData;
    FEndChar: PChar;
    FParent: TFpPascalExpressionPartContainer;
    FStartChar: PChar;
    FResultValue: TFpValue;
    FResultValDone: Boolean;
    FIsCopy: Boolean;
    function GetResultValue: TFpValue;
    function GetSurroundingOpenBracket: TFpPascalExpressionPartBracket;
    function GetTopParent: TFpPascalExpressionPart;
    procedure SetEndChar(AValue: PChar);
    procedure SetParent(AValue: TFpPascalExpressionPartContainer); virtual;
    procedure SetStartChar(AValue: PChar);
    function  CreateErrorWithPos(AnErrorCode: TFpErrorCode; AData: array of const; APos: integer = -1): TFpError;
    procedure SetErrorWithPos(AnErrorCode: TFpErrorCode; AData: array of const);
  protected
    function DebugText(AIndent: String; {%H-}AWithResults: Boolean): String; virtual; // Self desc only
    function DebugDump(AIndent: String; AWithResults: Boolean): String; virtual;
  protected
    FPrecedence: Integer;
    procedure Init; virtual;
    procedure Assign(ASourcePart: TFpPascalExpressionPart); virtual;
    function  FindCopiedInParents(ASourcePart, AFindInSourcePart: TFpPascalExpressionPart; AFindFlags: TFindInParentsFlags = []): TFpPascalExpressionPart;
    function  DoGetIsTypeCast: Boolean; virtual; deprecated;
    function  DoGetResultValue: TFpValue; virtual;
    procedure SetError(AMsg: String = ''); // deprecated;
    procedure SetError(APart: TFpPascalExpressionPart; AMsg: String = ''); // deprecated;
    //procedure SetError(AnErrorCode: TFpErrorCode; const AnNestedErr: TFpError = nil);
    procedure SetError(AnErrorCode: TFpErrorCode; AData: array of const; const AnNestedErr: TFpError = nil);
    procedure SetError(AnError: TFpError);
    procedure ResetEvaluation; virtual;
    procedure ResetEvaluationRecursive; virtual;
    procedure ResetEvaluationForAnchestors; virtual;

    Procedure ReplaceInParent(AReplacement: TFpPascalExpressionPart);
    procedure DoHandleEndOfExpression; virtual;
    procedure DoParentIndexBraceClosed(APreviousArraySliceList: PFpPascalExpressionPartOperatorArraySlice); virtual; // called on index-Item in array[XXX]

    function IsValidNextPart(APart: TFpPascalExpressionPart): Boolean; virtual;
    function IsValidAfterPart({%H-}APrevPart: TFpPascalExpressionPart): Boolean; virtual;
    function MaybeHandlePrevPart({%H-}APrevPart: TFpPascalExpressionPart;
                                 var {%H-}AResult: TFpPascalExpressionPart): Boolean; virtual;
    // HasPrecedence: an operator with follows precedence rules: the last operand can be taken by the next operator
    function HasPrecedence: Boolean; virtual;
    function FindLeftSideOperandByPrecedence({%H-}AnOperator: TFpPascalExpressionPartWithPrecedence):
                                             TFpPascalExpressionPart; virtual;
    function CanHaveOperatorAsNext: Boolean; virtual; // True
    function HandleSeparator(ASeparatorType: TSeparatorType; var APart: TFpPascalExpressionPart): Boolean; virtual; // False

    procedure GetFirstLastChar(out AFirst, ALast: PChar); virtual;
  public
    constructor Create(AnExpressionData: TFpPascalExpressionSharedData; AStartChar: PChar; AnEndChar: PChar = nil);
    destructor Destroy; override;
    function CreateCopy(ACopiedParent: TFpPascalExpressionPartContainer): TFpPascalExpressionPart;
    procedure BeginNeedCopy; virtual;
    procedure EndNeedCopy; virtual;
    function  HandleNextPart(APart: TFpPascalExpressionPart): TFpPascalExpressionPart; virtual;
    procedure HandleEndOfExpression; virtual;

    // AcceptParamAsSeparator: called on the first Item in TFpPascalExpressionPartBracketArgumentList
    function AcceptParamAsSeparator(AParamPart: TFpPascalExpressionPart;
      ABracketsPart: TFpPascalExpressionPartContainer;
      var AResult: TFpPascalExpressionPart): boolean; virtual;
    // HandleNewParameterInList: called on the first Item in TFpPascalExpressionPartBracketArgumentList
    procedure HandleNewParameterInList(AParamPart: TFpPascalExpressionPart; ABracketsPart: TFpPascalExpressionPartContainer); virtual;
    procedure HandleEndOfParameterInList(AParamPart: TFpPascalExpressionPart; ABracketsPart: TFpPascalExpressionPartContainer); virtual;

    function GetText(AMaxLen: Integer=0): String;
    function GetPos: Integer;
    function GetFullText(AMaxLen: Integer=0): String; virtual; // including children
    function ReturnsVariant: boolean; virtual;
    function IsClosed: boolean; virtual; // for brackets
    property StartChar: PChar read FStartChar write SetStartChar;
    property EndChar: PChar read FEndChar write SetEndChar;
    property Parent: TFpPascalExpressionPartContainer read FParent write SetParent;
    function FindInParents(APart: TFpPascalExpressionPart): Boolean;
    property TopParent: TFpPascalExpressionPart read GetTopParent; // or self
    property Precedence: Integer read FPrecedence;
    property SurroundingBracket: TFpPascalExpressionPartBracket read GetSurroundingOpenBracket; // incl self
    property ResultValue: TFpValue read GetResultValue;
    property ExpressionData: TFpPascalExpressionSharedData read FExpressionData;
  end;

  { TFpPascalExpressionPartContainer }

  TFpPascalExpressionPartContainer = class(TFpPascalExpressionPart)
  private
    FList: TList;
    function GetCount: Integer;
    function GetItems(AIndex: Integer): TFpPascalExpressionPart;
    function GetLastItem: TFpPascalExpressionPart;
    procedure SetItems(AIndex: Integer; AValue: TFpPascalExpressionPart);
    procedure SetLastItem(AValue: TFpPascalExpressionPart);
  protected
    procedure Init; override;
    procedure Assign(ASourcePart: TFpPascalExpressionPart); override;
    procedure ResetEvaluationRecursive; override;
    procedure GetFirstLastChar(out AFirst, ALast: PChar); override;
    function DebugDump(AIndent: String; AWithResults: Boolean): String; override;
  public
    destructor Destroy; override;
    procedure BeginNeedCopy; override;
    procedure EndNeedCopy; override;
    function GetFullText(AMaxLen: Integer=0): String; override; // including children
    function Add(APart: TFpPascalExpressionPart): Integer;
    function IndexOf(APart: TFpPascalExpressionPart): Integer;
    procedure Clear;
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TFpPascalExpressionPart read GetItems write SetItems;
    property LastItem: TFpPascalExpressionPart read GetLastItem write SetLastItem;
  end;

  { TFpPascalExpressionPartIdentifier }

  TFpPascalExpressionPartIdentifier = class(TFpPascalExpressionPartContainer)
  private
    FOnGetSymbol: TFpPascalParserGetSymbolForIdentProc;
  protected
    function DoGetIsTypeCast: Boolean; override;
    function DoGetResultValue: TFpValue; override;
    property OnGetSymbol: TFpPascalParserGetSymbolForIdentProc read FOnGetSymbol write FOnGetSymbol;
    procedure Assign(ASourcePart: TFpPascalExpressionPart); override;
    procedure ResetEvaluation; override;
  end;

  { TFpPascalExpressionPartCpuRegister }

  TFpPascalExpressionPartCpuRegister = class(TFpPascalExpressionPartContainer)
  protected
    function DoGetResultValue: TFpValue; override;
  end;


  TFpPascalExpressionPartBracketArgumentList = class;

  { TFpPascalExpressionPartIntrinsicBase }

  TFpPascalExpressionPartIntrinsicBase = class(TFpPascalExpressionPartContainer)
  protected
    function CheckArgumentCount(AParams: TFpPascalExpressionPartBracketArgumentList; ARequiredCount: Integer; AMaxAccepted: Integer = -1): Boolean;
    // GetArg; ANum is 1 based
    function GetArg(AParams: TFpPascalExpressionPartBracketArgumentList; ANum: Integer; out AValue: TFpValue;
                    AnErr: String = ''): Boolean;
    function DoGetResultValue: TFpValue; override;
    function DoGetResultValue(AParams: TFpPascalExpressionPartBracketArgumentList): TFpValue; virtual;
  end;

  { TFpPascalExpressionPartIntrinsic }

  TFpPascalExpressionPartIntrinsic = class(TFpPascalExpressionPartIntrinsicBase)
  private
    FIntrinsic: TFpIntrinsicFunc;
    FChildClassCastType: TFpValue;
    FFlattenCurrentVal, FFlattenCurrentValOrig: TFpValue;
    FFlattenMemberName: String;
    FFlattenMemberNotFound: boolean;

    function DoGetMemberForFlattenExpr(APart: TFpPascalExpressionPart; AnIdent: String): TFpValue;
  protected
    function DoTry(AParams: TFpPascalExpressionPartBracketArgumentList): TFpValue;
    function DoTryN(AParams: TFpPascalExpressionPartBracketArgumentList): TFpValue;
    function DoObj(AParams: TFpPascalExpressionPartBracketArgumentList): TFpValue;
    function DoOrd(AParams: TFpPascalExpressionPartBracketArgumentList): TFpValue;
    function DoLength(AParams: TFpPascalExpressionPartBracketArgumentList): TFpValue;
    function DoChildClass(AParams: TFpPascalExpressionPartBracketArgumentList): TFpValue;
    function DoFlatten(AParams: TFpPascalExpressionPartBracketArgumentList): TFpValue;
    function DoFlattenPlaceholder(AParams: TFpPascalExpressionPartBracketArgumentList): TFpValue;
    function DoRefCnt(AParams: TFpPascalExpressionPartBracketArgumentList): TFpValue;
    function DoPos(AParams: TFpPascalExpressionPartBracketArgumentList): TFpValue;
    function DoSubStr(AParams: TFpPascalExpressionPartBracketArgumentList): TFpValue;
    function DoLower(AParams: TFpPascalExpressionPartBracketArgumentList): TFpValue;
    function DoUpper(AParams: TFpPascalExpressionPartBracketArgumentList): TFpValue;
    function DoRound(AParams: TFpPascalExpressionPartBracketArgumentList): TFpValue;
    function DoTrunc(AParams: TFpPascalExpressionPartBracketArgumentList): TFpValue;
    function DoSqrt(AParams: TFpPascalExpressionPartBracketArgumentList): TFpValue;
    function DoPi(AParams: TFpPascalExpressionPartBracketArgumentList): TFpValue;
    function DoLn(AParams: TFpPascalExpressionPartBracketArgumentList): TFpValue;
    function DoLog(AParams: TFpPascalExpressionPartBracketArgumentList): TFpValue;
    function DoSin(AParams: TFpPascalExpressionPartBracketArgumentList): TFpValue;
    function DoCos(AParams: TFpPascalExpressionPartBracketArgumentList): TFpValue;
    function DoTan(AParams: TFpPascalExpressionPartBracketArgumentList): TFpValue;

    function DoGetResultValue: TFpValue; override;
    function DoGetResultValue(AParams: TFpPascalExpressionPartBracketArgumentList): TFpValue; override;
    procedure Assign(ASourcePart: TFpPascalExpressionPart); override;
  public
    constructor Create(AnExpressionData: TFpPascalExpressionSharedData; AStartChar: PChar;
      AnEndChar: PChar; AnIntrinsic: TFpIntrinsicFunc);
    destructor Destroy; override;
    function ReturnsVariant: boolean; override;
    function AcceptParamAsSeparator(AParamPart: TFpPascalExpressionPart;
      ABracketsPart: TFpPascalExpressionPartContainer; var AResult: TFpPascalExpressionPart
      ): boolean; override;
    procedure HandleNewParameterInList(AParamPart: TFpPascalExpressionPart; ABracketsPart: TFpPascalExpressionPartContainer); override;
    procedure HandleEndOfParameterInList(AParamPart: TFpPascalExpressionPart;
      ABracketsPart: TFpPascalExpressionPartContainer); override;
  end;

  TFpPascalExpressionPartConstant = class(TFpPascalExpressionPartContainer)
  end;

  { TFpPascalExpressionPartConstantNumber }

  TFpPascalExpressionPartConstantNumber = class(TFpPascalExpressionPartConstant)
  protected
    function DoGetResultValue: TFpValue; override;
  end;

  { TFpPascalExpressionPartConstantNumberFloat }

  TFpPascalExpressionPartConstantNumberFloat = class(TFpPascalExpressionPartConstantNumber)
  protected
    function DoGetResultValue: TFpValue; override;
  end;

  { TFpPascalExpressionPartConstantText }

  TFpPascalExpressionPartConstantText = class(TFpPascalExpressionPartConstant)
  protected
    FValue: String;
    function DoGetResultValue: TFpValue; override;
    procedure Assign(ASourcePart: TFpPascalExpressionPart); override;
  end;

  { TFpPascalExpressionPartWithPrecedence }

  TFpPascalExpressionPartWithPrecedence = class(TFpPascalExpressionPartContainer)
  protected
    function HasPrecedence: Boolean; override;
  end;

  { TFpPascalExpressionPartBracket }

  TFpPascalExpressionPartBracket = class(TFpPascalExpressionPartWithPrecedence)
  // some, but not all bracket expr have precedence
  private
    FIsClosed: boolean;
    FIsClosing: boolean;
    FIsSeparatorChecking: boolean;
    FAfterComma: Integer;
    FFullEndChar: PChar;
    function GetAfterComma: Boolean;
  protected
    procedure Init; override;
    function HasPrecedence: Boolean; override;
    procedure DoHandleEndOfExpression; override;
    function CanHaveOperatorAsNext: Boolean; override;
    function HandleNextPartInBracket(APart: TFpPascalExpressionPart): TFpPascalExpressionPart; virtual;
    procedure SetAfterCommaFlag;
    property AfterComma: Boolean read GetAfterComma;
    procedure GetFirstLastChar(out AFirst, ALast: PChar); override;
    procedure CheckBeforeSeparator(APart: TFpPascalExpressionPart);
    procedure Assign(ASourcePart: TFpPascalExpressionPart); override;
  public
    procedure CloseBracket(ALastAddedPart: TFpPascalExpressionPart;
                           APreviousArraySliceList: PFpPascalExpressionPartOperatorArraySlice;
                           AStartChar: PChar; AnEndChar: PChar = nil); virtual;
    function HandleNextPart(APart: TFpPascalExpressionPart): TFpPascalExpressionPart; override;
    procedure HandleEndOfExpression; override;
    function IsClosed: boolean; override;
  end;

  { TFpPascalExpressionPartRoundBracket }

  TFpPascalExpressionPartRoundBracket = class(TFpPascalExpressionPartBracket)
  protected
  end;

  { TFpPascalExpressionPartBracketSubExpression }

  TFpPascalExpressionPartBracketSubExpression = class(TFpPascalExpressionPartRoundBracket)
  protected
    function HandleNextPartInBracket(APart: TFpPascalExpressionPart): TFpPascalExpressionPart; override;
    function DoGetResultValue: TFpValue; override;
    function HandleSeparator(ASeparatorType: TSeparatorType; var APart: TFpPascalExpressionPart): Boolean; override;
  public
    procedure CloseBracket(ALastAddedPart: TFpPascalExpressionPart;
                           APreviousArraySliceList: PFpPascalExpressionPartOperatorArraySlice;
                           AStartChar: PChar; AnEndChar: PChar = nil); override;
  end;

  { TFpPascalExpressionPartBracketArgumentList }

  TFpPascalExpressionPartBracketArgumentList = class(TFpPascalExpressionPartRoundBracket)
  // function arguments or type cast // this acts a operator: first element is the function/type
  protected
    procedure Init; override;
    function DoGetResultValue: TFpValue; override;
    function DoGetIsTypeCast: Boolean; override;
    function IsValidAfterPart(APrevPart: TFpPascalExpressionPart): Boolean; override;
    function HandleNextPartInBracket(APart: TFpPascalExpressionPart): TFpPascalExpressionPart; override;
    function MaybeHandlePrevPart(APrevPart: TFpPascalExpressionPart;
      var AResult: TFpPascalExpressionPart): Boolean; override;
    function HandleSeparator(ASeparatorType: TSeparatorType; var APart: TFpPascalExpressionPart): Boolean; override;
  public
    procedure CloseBracket(ALastAddedPart: TFpPascalExpressionPart;
                           APreviousArraySliceList: PFpPascalExpressionPartOperatorArraySlice;
                           AStartChar: PChar; AnEndChar: PChar = nil); override;
    function ReturnsVariant: boolean; override;
    function IntrinsicType: TFpIntrinsicFunc;
  end;


  { TFpPascalExpressionPartSquareBracket }

  TFpPascalExpressionPartSquareBracket = class(TFpPascalExpressionPartBracket)
  end;

  { TFpPascalExpressionPartBracketSet }

  TFpPascalExpressionPartBracketSet = class(TFpPascalExpressionPartSquareBracket)
  // a in [x, y, z]
  protected
    function DoGetResultValue: TFpValue; override;
    function HandleNextPartInBracket(APart: TFpPascalExpressionPart): TFpPascalExpressionPart; override;
    function HandleSeparator(ASeparatorType: TSeparatorType; var APart: TFpPascalExpressionPart): Boolean; override;
  end;

  { TFpPascalExpressionPartBracketIndex }

  TFpPascalExpressionPartBracketIndex = class(TFpPascalExpressionPartSquareBracket)
  // array[1]
  protected
    procedure Init; override;
    function DoGetResultValue: TFpValue; override;
    function IsValidAfterPart(APrevPart: TFpPascalExpressionPart): Boolean; override;
    function HandleNextPartInBracket(APart: TFpPascalExpressionPart): TFpPascalExpressionPart; override;
    function MaybeHandlePrevPart(APrevPart: TFpPascalExpressionPart;
      var AResult: TFpPascalExpressionPart): Boolean; override;
    procedure DoHandleEndOfExpression; override;
    function HandleSeparator(ASeparatorType: TSeparatorType; var APart: TFpPascalExpressionPart): Boolean; override;
  public
    procedure CloseBracket(ALastAddedPart: TFpPascalExpressionPart;
                           APreviousArraySliceList: PFpPascalExpressionPartOperatorArraySlice;
                           AStartChar: PChar; AnEndChar: PChar = nil); override;
    function ReturnsVariant: boolean; override;
  end;

  { TFpPascalExpressionPartOperator }

  TFpPascalExpressionPartOperator = class(TFpPascalExpressionPartWithPrecedence)
  protected
    function DebugText(AIndent: String; AWithResults: Boolean): String; override;
    function CanHaveOperatorAsNext: Boolean; override;
    function FindLeftSideOperandByPrecedence(AnOperator: TFpPascalExpressionPartWithPrecedence):
                                             TFpPascalExpressionPart; override;
    function HasAllOperands: Boolean; virtual; abstract;
    function MaybeAddLeftOperand(APrevPart: TFpPascalExpressionPart;
      var AResult: TFpPascalExpressionPart): Boolean;
    procedure DoHandleEndOfExpression; override;
    function HandleSeparator(ASeparatorType: TSeparatorType; var APart: TFpPascalExpressionPart): Boolean; override;
  public
    function HandleNextPart(APart: TFpPascalExpressionPart): TFpPascalExpressionPart; override;
  end;

  { TFpPascalExpressionPartUnaryOperator }

  TFpPascalExpressionPartUnaryOperator = class(TFpPascalExpressionPartOperator)
  protected
    function HasAllOperands: Boolean; override;
  public
  end;

  { TFpPascalExpressionPartBinaryOperator }

  TFpPascalExpressionPartBinaryOperator = class(TFpPascalExpressionPartOperator)
  protected
    function HasAllOperands: Boolean; override;
    function IsValidAfterPart(APrevPart: TFpPascalExpressionPart): Boolean; override;
    function IsValidAfterPartWithPrecedence(APrevPart: TFpPascalExpressionPart): Boolean; virtual;
  public
    function HandleNextPart(APart: TFpPascalExpressionPart): TFpPascalExpressionPart; override;
    function MaybeHandlePrevPart(APrevPart: TFpPascalExpressionPart;
      var AResult: TFpPascalExpressionPart): Boolean; override;
  end;

  { TFpPascalExpressionPartOperatorAddressOf }

  TFpPascalExpressionPartOperatorAddressOf = class(TFpPascalExpressionPartUnaryOperator)  // @
  protected
    procedure Init; override;
    function DoGetResultValue: TFpValue; override;
  end;

  { TFpPascalExpressionPartOperatorMakeRef }

  TFpPascalExpressionPartOperatorMakeRef = class(TFpPascalExpressionPartUnaryOperator)  // ^TTYpe
  protected
    procedure Init; override;
    function IsValidNextPart(APart: TFpPascalExpressionPart): Boolean; override;
    function DoGetResultValue: TFpValue; override;
    function DoGetIsTypeCast: Boolean; override;
  end;

  { TFpPascalExpressionPartOperatorDeRef }

  TFpPascalExpressionPartOperatorDeRef = class(TFpPascalExpressionPartUnaryOperator)  // ptrval^
  protected
    procedure Init; override;
    function DoGetResultValue: TFpValue; override;
    function MaybeHandlePrevPart(APrevPart: TFpPascalExpressionPart;
      var AResult: TFpPascalExpressionPart): Boolean; override;
    function FindLeftSideOperandByPrecedence({%H-}AnOperator: TFpPascalExpressionPartWithPrecedence):
                                             TFpPascalExpressionPart;
      override;
    // IsValidAfterPart: same as binary op
    function IsValidAfterPart(APrevPart: TFpPascalExpressionPart): Boolean; override;
  end;

  { TFpPascalExpressionPartOperatorUnaryPlusMinus }

  TFpPascalExpressionPartOperatorUnaryPlusMinus = class(TFpPascalExpressionPartUnaryOperator)  // + -
  // Unary + -
  protected
    procedure Init; override;
    function DoGetResultValue: TFpValue; override;
  end;

  { TFpPascalExpressionPartOperatorQuestionMark }

  TFpPascalExpressionPartOperatorQuestionMark = class(TFpPascalExpressionPartBinaryOperator)  // ? :
  protected
    procedure Init; override;
    function DoGetResultValue: TFpValue; override;
    function FindLeftSideOperandByPrecedence(AnOperator: TFpPascalExpressionPartWithPrecedence
      ): TFpPascalExpressionPart; override;
    function IsValidAfterPartWithPrecedence(APrevPart: TFpPascalExpressionPart): Boolean; override;
    procedure DoHandleEndOfExpression; override;
  public
    function ReturnsVariant: boolean; override;
    function IsClosed: boolean; override;
  end;

  { TFpPascalExpressionPartOperatorColon }

  TFpPascalExpressionPartOperatorColon = class(TFpPascalExpressionPartBinaryOperator)  // ? :
  private
    FIsClosed: boolean;
  protected
    procedure Init; override;
    function DoGetResultValue: TFpValue; override;
    function FindLeftSideOperandByPrecedence(AnOperator: TFpPascalExpressionPartWithPrecedence
      ): TFpPascalExpressionPart; override;
    function IsValidAfterPartWithPrecedence(APrevPart: TFpPascalExpressionPart): Boolean; override;
    procedure DoHandleEndOfExpression; override;
  public
    function IsClosed: boolean; override;
  end;

  { TFpPascalExpressionPartOperatorPlusMinus }

  TFpPascalExpressionPartOperatorPlusMinus = class(TFpPascalExpressionPartBinaryOperator)  // + -
  // Binary + -
  protected
    procedure Init; override;
    function DoGetResultValue: TFpValue; override;
  end;

  { TFpPascalExpressionPartOperatorMulDiv }

  TFpPascalExpressionPartOperatorMulDiv = class(TFpPascalExpressionPartBinaryOperator)    // * /
  protected
    procedure Init; override;
    function DoGetResultValue: TFpValue; override;
  end;

  { TFpPascalExpressionPartOperatorUnaryNot }

  TFpPascalExpressionPartOperatorUnaryNot = class(TFpPascalExpressionPartUnaryOperator)  // not
  protected
    procedure Init; override;
    function DoGetResultValue: TFpValue; override;
  end;

  { TFpPascalExpressionPartOperatorAnd }

  TFpPascalExpressionPartOperatorAnd = class(TFpPascalExpressionPartBinaryOperator)    // AND
  protected
    procedure Init; override;
    function DoGetResultValue: TFpValue; override;
  end;

  { TFpPascalExpressionPartOperatorBitShift }

  TFpPascalExpressionPartOperatorBitShift = class(TFpPascalExpressionPartBinaryOperator)    // shr shl << >>
  protected
    procedure Init; override;
    function CheckOperators(out AVal, AShift: QWord): boolean;
  end;

  { TFpPascalExpressionPartOperatorShr }

  TFpPascalExpressionPartOperatorShr = class(TFpPascalExpressionPartOperatorBitShift)    // shr >>
  protected
    function DoGetResultValue: TFpValue; override;
  end;

  { TFpPascalExpressionPartOperatorShl }

  TFpPascalExpressionPartOperatorShl = class(TFpPascalExpressionPartOperatorBitShift)    // shl <<
  protected
    function DoGetResultValue: TFpValue; override;
  end;

  { TFpPascalExpressionPartOperatorOr }

  TFpPascalExpressionPartOperatorOr = class(TFpPascalExpressionPartBinaryOperator)    // OR XOR
  public type
    TOpOrType = (ootOr, ootXor);
  protected
    FOp: TOpOrType;
    procedure Init; override;
    function DoGetResultValue: TFpValue; override;
    procedure Assign(ASourcePart: TFpPascalExpressionPart); override;
  public
    constructor Create(AnExpressionData: TFpPascalExpressionSharedData; AnOp: TOpOrType; AStartChar: PChar;
      AnEndChar: PChar = nil);
  end;

  { TFpPascalExpressionPartOperatorCompare }

  TFpPascalExpressionPartOperatorCompare = class(TFpPascalExpressionPartBinaryOperator)    // = < > <> ><
  protected
    procedure Init; override;
    function DoGetResultValue: TFpValue; override;
  end;

  { TFpPascalExpressionPartOperatorMemberOf }

  TFpPascalExpressionPartOperatorMemberOf = class(TFpPascalExpressionPartBinaryOperator)    // struct.member
  protected
    procedure Init; override;
    function IsValidNextPart(APart: TFpPascalExpressionPart): Boolean; override;
    function DoGetResultValue: TFpValue; override;
  end;

  { TFpPascalExpressionPartOperatorMemberIn }

  TFpPascalExpressionPartOperatorMemberIn = class(TFpPascalExpressionPartBinaryOperator)    // enum in set
  protected
    procedure Init; override;
    function DoGetResultValue: TFpValue; override;
  end;

  { TFpPascalExpressionPartOperatorArraySliceController }

  TFpPascalExpressionPartOperatorArraySliceController = class(TFpPascalExpressionPartBracketSubExpression)
  private
    FDisableSlice: boolean;
    FSlicePart: TFpPascalExpressionPartOperatorArraySlice;
    FInResetEvaluationForIndex: Boolean;
    FHasVariantPart: Boolean;
    FCheckedForVariantPart: Boolean;
    FNeedCopy: integer;
    function GetCanDisableSlice: boolean;
  protected
    function DoGetResultValue: TFpValue; override;
    function InternalDoGetResultValue(ANeedCopy: boolean): TFpValue;
    procedure DoParentIndexBraceClosed(APreviousArraySliceList: PFpPascalExpressionPartOperatorArraySlice); override;
    procedure ResetEvaluation; override;
    procedure ResetEvaluationForIndex;
    procedure ResetEvaluationForAnchestors; override;
  public
    constructor Create(AnExpressionData: TFpPascalExpressionSharedData;
      ASlicePart: TFpPascalExpressionPartOperatorArraySlice;
      AStartChar: PChar; AnEndChar: PChar = nil);
    procedure BeginNeedCopy; override;
    procedure EndNeedCopy; override;
    function GetFullText(AMaxLen: Integer = 0): String; override;
    function DebugText(AIndent: String; AWithResults: Boolean): String; override;
    property SlicePart: TFpPascalExpressionPartOperatorArraySlice read FSlicePart;
    property DisableSlice: boolean read FDisableSlice write FDisableSlice;
    property CanDisableSlice: boolean read GetCanDisableSlice;
  end;

  { TFpPascalExpressionPartOperatorArraySlice }

  TFpPascalExpressionPartOperatorArraySlice = class(TFpPascalExpressionPartBinaryOperator)    // enum in set
  private
    FCurrentIndex: Int64;
    FController: TFpPascalExpressionPartOperatorArraySliceController;
    FPreviousArraySlice: TFpPascalExpressionPartOperatorArraySlice;
    FHasUpperLimit: Boolean;
    FUpperLimit: Integer;
  protected
    FTouched: boolean;
    procedure Init; override;
    procedure DoParentIndexBraceClosed(APreviousArraySliceList: PFpPascalExpressionPartOperatorArraySlice); override;
    function DoGetResultValue: TFpValue; override;
    function StartValue: Int64;
    function EndValue: Int64;
    procedure CheckForVariantExpressionParts;
    procedure Assign(ASourcePart: TFpPascalExpressionPart); override;
  public
    destructor Destroy; override;
    function IsClosed: boolean; override;
    function AddController(ACurPart: TFpPascalExpressionPart): TFpPascalExpressionPart;
    function AddControllerRecursive(ACurPart: TFpPascalExpressionPart): TFpPascalExpressionPart;
    property Controller: TFpPascalExpressionPartOperatorArraySliceController read FController;
  end;

  { TFpPasParserValueSlicedArrayIndex }

  TFpPasParserValueSlicedArrayIndex = class(TFpSymbol)
  private
    FLowBound: Int64;
  public
    function GetValueLowBound(AValueObj: TFpValue; out ALowBound: Int64): Boolean; override;
  end;

  { TFpPasParserValue }

  TFpPasParserValue = class(TFpValue)
  private
    FContext: TFpDbgSimpleLocationContext;
    // Pos and Text from expressionpart / used in errors
    FExprPos: Integer;
    FExprText: String;
  protected
    function DebugText(AIndent: String): String; virtual;
    function  CreateErrorWithPos(AnErrorCode: TFpErrorCode; AData: array of const): TFpError;
  public
    constructor Create(AnExpressionPart: TFpPascalExpressionPart);
    constructor Create(AContext: TFpDbgSimpleLocationContext; AnExprPos: Integer; AnExprText: String);
    property Context: TFpDbgSimpleLocationContext read FContext;
  end;

  { TFpPasParserValueSlicedArray }

  TFpPasParserValueSlicedArray = class(TFpPasParserValue)
  private
    FLowBoundIndex: TFpPasParserValueSlicedArrayIndex;
    FArraySlice: TFpPascalExpressionPartOperatorArraySliceController;
    FOwnsController: Boolean;
    FExpressionPartInValue: TFpPascalExpressionPart;
  protected
    //function DebugText(AIndent: String): String; override;
    function SlicePart: TFpPascalExpressionPartOperatorArraySlice; inline;
  protected
    function GetKind: TDbgSymbolKind; override;
    function GetFieldFlags: TFpValueFieldFlags; override;
    function GetTypeInfo: TFpSymbol; override;

    function GetMember(AIndex: Int64): TFpValue; override;
    function GetMemberCount: Integer; override;

    function GetIndexType(AIndex: Integer): TFpSymbol; override;
    function GetIndexTypeCount: Integer; override;
    function GetOrdLowBound: Int64; override;
  public
    constructor Create(ASlice: TFpPascalExpressionPartOperatorArraySliceController; AnOwnsController: boolean = False);
    destructor Destroy; override;
    function SliceBracketStartOffs: integer;
    function SliceBracketLength: integer;
  end;


implementation

var
  DBG_WARNINGS: PLazLoggerLogGroup;

const
  // 1 highest
  PRECEDENCE_MEMBER_OF  =  1;        // foo.bar
  PRECEDENCE_MAKE_REF   =  1;        // ^TFoo
  PRECEDENCE_ARG_LIST   =  2;        // foo() / TFoo()
  PRECEDENCE_ARRAY_IDX  =  2;        // foo[1]
  PRECEDENCE_DEREF      =  5;        // a^    // Precedence acts only to the left side
  PRECEDENCE_ADRESS_OF  =  6;        // @a
  //PRECEDENCE_POWER      = 10;        // ** (power) must be stronger than unary -
  PRECEDENCE_UNARY_SIGN = 11;        // -a
  PRECEDENCE_UNARY_NOT  = 11;        // NOT a
  PRECEDENCE_MUL_DIV    = 12;        // a * b
  PRECEDENCE_AND        = 12;        // a AND b
  PRECEDENCE_BIT_SHIFT  = 12;        // << >> shr shr
  PRECEDENCE_PLUS_MINUS = 13;        // a + b
  PRECEDENCE_OR         = 13;        // a OR b  // XOR
  PRECEDENCE_IN         = 19;        // enum IN set // officially the same as PRECEDENCE_COMPARE
  PRECEDENCE_COMPARE    = 20;        // a <> b // a=b
  PRECEDENCE_QUEST_COLON= 27;        // ? :
  PRECEDENCE_ARRAY_SLICE= 30;        // array[5..9] // array slice
  PRECEDENCE_SEPARATOR_COLON= MaxInt; // : used as separator in intrinsics // Operator used to hold both sides

type

  { TFpPascalExpressionPartListForwarder }

  TFpPascalExpressionPartListForwarder = class(TFpPascalExpressionPartList)
  private
    FExpressionPart: TFpPascalExpressionPartContainer;
    FListOffset, FCount: Integer;
  protected
    function GetCount: Integer; override;
    function GetItems(AIndex: Integer): TFpPascalExpressionPart; override;
  public
    constructor Create(AnExpressionPart: TFpPascalExpressionPartContainer; AListOffset, ACount: Integer);
  end;

  {%region  DebugSymbol }

  { TPasParserSymbolPointer
    used by TFpPasParserValueMakeReftype.GetDbgSymbol
  }

  TPasParserSymbolPointer = class(TFpSymbol)
  private
    FPointerLevels: Integer;
    FPointedTo: TFpSymbol;
    FContext: TFpDbgSimpleLocationContext;
    // Pos and Text from expressionpart / used in errors
    FExprPos: Integer;
    FExprText: String;
  protected
    // NameNeeded //  "^TPointedTo"
    procedure TypeInfoNeeded; override;
    function DoReadSize(const AValueObj: TFpValue; out ASize: TFpDbgValueSize): Boolean; override;
  public
    constructor Create(const APointedTo: TFpSymbol; AContext: TFpDbgSimpleLocationContext; AnExprPos: Integer; AnExprText: String; APointerLevels: Integer);
    constructor Create(const APointedTo: TFpSymbol; AnExpressionPart: TFpPascalExpressionPart; APointerLevels: Integer);
    constructor Create(const APointedTo: TFpSymbol; AnExpressionPart: TFpPascalExpressionPart);
    destructor Destroy; override;
    function TypeCastValue(AValue: TFpValue): TFpValue; override;
  end;

  { TPasParserSymbolArrayDeIndex }

  TPasParserSymbolArrayDeIndex = class(TFpSymbolForwarder) // 1 index level off
  private
    FArray: TFpSymbol;
  protected
    //procedure ForwardToSymbolNeeded; override;
    function GetNestedSymbolCount: Integer; override;
    function GetNestedSymbol(AIndex: Int64): TFpSymbol; override;
  public
    constructor Create(const AnArray: TFpSymbol);
    destructor Destroy; override;
  end;

  {%endregion  DebugSymbol }

  {%region  DebugSymbolValue }

  { TFpPasParserValueCastToPointer
    used by TPasParserSymbolPointer.TypeCastValue (which is used by TFpPasParserValueMakeReftype.GetDbgSymbol)
  }

  TFpPasParserValueCastToPointer = class(TFpPasParserValue)
  private
    FValue: TFpValue;
    FTypeSymbol: TFpSymbol;
  protected
    function DebugText(AIndent: String): String; override;
  protected
    function GetKind: TDbgSymbolKind; override;
    function GetFieldFlags: TFpValueFieldFlags; override;
    function GetTypeInfo: TFpSymbol; override;
    function GetAsCardinal: QWord; override;
    function GetAsString: AnsiString; override;
    function GetAsWideString: WideString; override;
    function GetAddress: TFpDbgMemLocation; override;
    function GetDerefAddress: TFpDbgMemLocation; override;
    function GetMember(AIndex: Int64): TFpValue; override;
  public
    constructor Create(AValue: TFpValue; ATypeInfo: TFpSymbol; AContext: TFpDbgSimpleLocationContext; AnExprPos: Integer; AnExprText: String);
    destructor Destroy; override;
  end;

  { TFpPasParserValueMakeReftype }

  TFpPasParserValueMakeReftype = class(TFpPasParserValue)
  private
    FSourceTypeSymbol, FTypeSymbol: TFpSymbol;
    FRefLevel: Integer;
  protected
    function DebugText(AIndent: String): String; override;
  protected
    function GetDbgSymbol: TFpSymbol; override; // returns a TPasParserSymbolPointer
  public
    constructor Create(ATypeInfo: TFpSymbol; AnExpressionPart: TFpPascalExpressionPart);
    destructor Destroy; override;
    procedure IncRefLevel;
    function GetTypeCastedValue(ADataVal: TFpValue): TFpValue; override;
  end;

  { TFpPasParserValueDerefPointer
    Used as address source in typecast
  }

  TFpPasParserValueDerefPointer = class(TFpPasParserValue)
  private
    FValue: TFpValue;
    FAddressOffset: Int64; // Add to address
    FCardinal: QWord; // todo: TFpDbgMemLocation ?
    FCardinalRead: Boolean;
  protected
    function DebugText(AIndent: String): String; override;
  protected
    function GetFieldFlags: TFpValueFieldFlags; override;
    function GetAddress: TFpDbgMemLocation; override;
    function DoGetSize(out ASize: TFpDbgValueSize): Boolean; override;
    function GetAsCardinal: QWord; override; // reads men
    function GetTypeInfo: TFpSymbol; override; // TODO: Cardinal? Why? // TODO: does not handle AOffset
  public
    constructor Create(AValue: TFpValue; AnExpressionPart: TFpPascalExpressionPart);
    constructor Create(AValue: TFpValue; AnExpressionPart: TFpPascalExpressionPart; AOffset: Int64);
    destructor Destroy; override;
  end;

  { TFpPasParserValueAddressOf }

  TFpPasParserValueAddressOf = class(TFpPasParserValue)
  private
    FValue: TFpValue;
    FTypeInfo: TFpSymbol;
    function GetPointedToValue: TFpValue;
  protected
    function DebugText(AIndent: String): String; override;
  protected
    function GetKind: TDbgSymbolKind; override;
    function GetFieldFlags: TFpValueFieldFlags; override;
    function GetAsInteger: Int64; override;
    function GetAsCardinal: QWord; override;
    function GetTypeInfo: TFpSymbol; override;
    function GetDerefAddress: TFpDbgMemLocation; override;
    function GetMember(AIndex: Int64): TFpValue; override;
    function GetAsString: AnsiString; override;
    function GetAsWideString: WideString; override;
  public
    constructor Create(AValue: TFpValue; AnExpressionPart: TFpPascalExpressionPart);
    destructor Destroy; override;
    property PointedToValue: TFpValue read GetPointedToValue;
  end;

  {%endregion  DebugSymbolValue }

  {%region  Intrinsic Flatten}

  { TFpValueFlatteArray }

  TFpValueFlatteArray = class(TFpValueConstArray)
  private
    FList: TRefCntObjList;
    FFullEvaluated: boolean;
  protected
    function GetOrdHighBound: Int64; override;
  public
    constructor Create(ALowBound: Integer);
    destructor Destroy; override;

    function GetMember(AIndex: Int64): TFpValue; override;
    function GetMemberCount: Integer; override;
  end;
  PFpValueFlatteArray = ^TFpValueFlatteArray;

  TFpPascalExpressionFlattenFlag = (
    iffShowNil,
    iffShowNoMember,
    iffShowRecurse,
    iffShowSeen,
    iffShowErrAny,
    iffDerefPtr,
    iffObj1, iffObj2, iffObj3, iffObj4
  );
  TFpPascalExpressionFlattenFlags = set of TFpPascalExpressionFlattenFlag;

  { TFpPascalExpressionCacheFlattenKey }

  TFpPascalExpressionCacheFlattenKey = record
    CtxThread, CtxStack: Integer;
    Key: String;
    Flags: TFpPascalExpressionFlattenFlags;
    ExpandArrayDepth: integer;

    class operator = (a,b: TFpPascalExpressionCacheFlattenKey): boolean;
    class operator < (a,b: TFpPascalExpressionCacheFlattenKey): boolean;
    class operator > (a,b: TFpPascalExpressionCacheFlattenKey): boolean;
  end;
  PFpPascalExpressionCacheFlattenKey = ^TFpPascalExpressionCacheFlattenKey;

  { TFpPascalExpressionCacheFlatten }

  TFpPascalExpressionCacheFlatten = class(specialize TFPGMap<TFpPascalExpressionCacheFlattenKey, TFpValueFlatteArray>)
  private
    function DoKeyPtrComp(Key1, Key2: Pointer): Integer;
  protected
    procedure Deref(Item: Pointer); override;
  public
    constructor Create;
    function Add(const AKey: TFpPascalExpressionCacheFlattenKey; const AData: TFpValueFlatteArray): Integer; inline;
    function Replace(const AKey: TFpPascalExpressionCacheFlattenKey; const AData: TFpValueFlatteArray): Integer; inline;
  end;

  { TAddrSeenList }

  TAddrSeenList = class(specialize TFPGMap<TFpDbgMemLocation, Integer>)
  private
    function DoKeyPtrComp(Key1, Key2: Pointer): Integer;
  public
    constructor Create;
  end;

  {%endregion  Intrinsic Flatten}

  {%region  Intrinsic Separator ":" }

  { TFpPascalExpressionPartOperatorColonAsSeparator }

  TFpPascalExpressionPartOperatorColonAsSeparator = class(TFpPascalExpressionPartBinaryOperator)  // + -
  protected
    procedure Init; override;
    function DoGetResultValue: TFpValue; override;
  public
    function IsClosed: boolean; override;
  end;

  {%endregion  Intrinsic Separator ":" }

function DbgsResultValue(AVal: TFpValue; AIndent: String): String;
begin
  if AVal is TFpPasParserValue then
    Result := LineEnding + TFpPasParserValue(AVal).DebugText(AIndent)
  else
  if AVal <> nil then
    Result := DbgSName(AVal) + '  DbsSym='+DbgSName(AVal.DbgSymbol)+' Type='+DbgSName(AVal.TypeInfo)
  else
    Result := DbgSName(AVal);
end;

function DbgsSymbol(AVal: TFpSymbol; {%H-}AIndent: String): String;
begin
  Result := DbgSName(AVal);
end;

function ValueToExprText(AnValue: TFpValue; AMaxLen: Integer = 0): String;
begin
  if AnValue is TFpPasParserValue then
    Result := TFpPasParserValue(AnValue).FExprText
  else
  if AnValue.DbgSymbol <> nil then
    Result := AnValue.DbgSymbol.Name
  else
    Result := '?';
end;

procedure ForwardError(ATarget, ASrc: TFpValue); inline;
begin
  if (ATarget = nil) or (ASrc = nil) then
    exit;
  if not IsError(ATarget.LastError) and
     IsError(ASrc.LastError)
  then
    ATarget.SetLastError(ASrc.LastError);
end;

procedure TFpPascalExpressionPartList.Clear;
begin
  assert(False, 'TFpPascalExpressionPartList.Clear: False');
end;

procedure TFpPascalExpressionPartList.Delete(Index: Integer);
begin
  assert(False, 'TFpPascalExpressionPartList.Delete: False');
end;

procedure TFpPascalExpressionPartList.Insert(Index: Integer; const S: string);
begin
  assert(False, 'TFpPascalExpressionPartList.Insert: False');
end;

function TFpPascalExpressionPartList.Get(Index: Integer): string;
begin
  Result := Items[Index].GetText();
end;

{ TFpPascalExpressionPartListForwarder }

function TFpPascalExpressionPartListForwarder.GetCount: Integer;
begin
  Result := FCount;
end;

function TFpPascalExpressionPartListForwarder.GetItems(AIndex: Integer
  ): TFpPascalExpressionPart;
begin
  Result := FExpressionPart.Items[AIndex + FListOffset];
end;

constructor TFpPascalExpressionPartListForwarder.Create(
  AnExpressionPart: TFpPascalExpressionPartContainer; AListOffset,
  ACount: Integer);
begin
  FExpressionPart := AnExpressionPart;
  FListOffset := AListOffset;
  FCount := ACount;
end;

function TFpPasParserValue.DebugText(AIndent: String): String;
begin
  Result := AIndent + DbgSName(Self)  + '  DbsSym='+DbgSName(DbgSymbol)+' Type='+DbgSName(TypeInfo) + LineEnding;
end;

function TFpPasParserValue.CreateErrorWithPos(AnErrorCode: TFpErrorCode;
  AData: array of const): TFpError;
begin
  if FExprPos = 1
  then Result := CreateError(AnErrorCode, AData, CreateError(fpErrPasParser_AtStart,  []      ))
  else Result := CreateError(AnErrorCode, AData, CreateError(fpErrPasParser_Position, [FExprPos]));
end;

constructor TFpPasParserValue.Create(AnExpressionPart: TFpPascalExpressionPart);
begin
  Create(AnExpressionPart.ExpressionData.Scope.LocationContext,
    AnExpressionPart.GetPos,
    AnExpressionPart.GetText(MAX_ERR_EXPR_QUOTE_LEN));
  inherited Create;
end;

constructor TFpPasParserValue.Create(AContext: TFpDbgSimpleLocationContext; AnExprPos: Integer;
  AnExprText: String);
begin
  FContext  := AContext;
  FExprPos  := AnExprPos;
  FExprText := AnExprText;
  inherited Create;
end;

{ TPasParserSymbolValueCastToPointer }

function TFpPasParserValueCastToPointer.DebugText(AIndent: String): String;
begin
  Result := inherited DebugText(AIndent)
          + AIndent + '-Value= ' + DbgsResultValue(FValue, AIndent + '  ') + LineEnding
          + AIndent + '-Symbol = ' + DbgsSymbol(FTypeSymbol, AIndent + '  ') + LineEnding;
end;

function TFpPasParserValueCastToPointer.GetKind: TDbgSymbolKind;
begin
  Result := skPointer;
end;

function TFpPasParserValueCastToPointer.GetFieldFlags: TFpValueFieldFlags;
var
  t: TFpSymbol;
  Size: TFpDbgValueSize;
begin
  if (FValue.FieldFlags * [svfAddress, svfOrdinal] <> [])
  then begin
    Result := [svfOrdinal, svfCardinal, svfSizeOfPointer, svfDataAddress];

    t := TypeInfo;
    if (t <> nil) then t := t.TypeInfo;
    if (t <> nil) and (t.Kind = skChar) and
       //(IsNilLoc(OrdOrDataAddr) or
       IsValidLoc(GetDerefAddress) //)  // always true
    then begin // pchar
      if not t.ReadSize(nil, Size) then
        Size := ZeroSize;
      case Size.Size of
        1: Result := Result + [svfString];
        2: Result := Result + [svfWideString];
      end;
    end;
  end
  else
    Result := [];
end;

function TFpPasParserValueCastToPointer.GetTypeInfo: TFpSymbol;
begin
  Result := FTypeSymbol;
end;

function TFpPasParserValueCastToPointer.GetAsCardinal: QWord;
var
  f: TFpValueFieldFlags;
begin
  Result := 0;
  f := FValue.FieldFlags;
  if svfOrdinal in f then
    Result := FValue.AsCardinal
  else
  if svfAddress in f then begin
    if not FContext.ReadUnsignedInt(FValue.Address, SizeVal(FContext.SizeOfAddress), Result) then begin
      Result := 0;
      SetLastError(FContext.LastMemError);
    end;
  end
  else begin
    if FValue is TFpPasParserValue then
      SetLastError(CreateErrorWithPos(fpErrCannotCastToPointer_p,
        [ValueToExprText(FValue, MAX_ERR_EXPR_QUOTE_LEN)]
      ));
  end;
end;

function TFpPasParserValueCastToPointer.GetAsString: AnsiString;
var
  t: TFpSymbol;
  Size: TFpDbgValueSize;
  a: TFpDbgMemLocation;
begin
  Result := '';
  if (FValue = nil) or (Context.MemManager = nil) then
    exit;

  t := TypeInfo;
  if t <> nil then
    t := t.TypeInfo;
  if (t = nil) or (t.Kind <> skChar) then
    exit;

  a := GetDerefAddress;
  if not IsReadableMem(a) then
    exit;

  // Only test for hardcoded size. TODO: dwarf 3 could have variable size, but for char that is not expected
  if not t.ReadSize(nil, Size) then
    exit;

  if Size = 2 then
    Result := GetAsWideString
  else
  if Size = 1 then begin // pchar
    if not Context.MemManager.ReadPChar(a, 0, Result) then begin
      Result := '';
      SetLastError(Context.LastMemError);
      exit;
    end;
  end
  else
    Result := inherited GetAsString;
end;

function TFpPasParserValueCastToPointer.GetAsWideString: WideString;
var
  t: TFpSymbol;
  Size: TFpDbgValueSize;
  a: TFpDbgMemLocation;
begin
  Result := '';
  if (FValue = nil) or (Context.MemManager = nil) then
    exit;

  t := TypeInfo;
  if t <> nil then
    t := t.TypeInfo;
  if (t = nil) or (t.Kind <> skChar) then
    exit;

  a := GetDerefAddress;
  if not IsReadableMem(a) then
    exit;

  // Only test for hardcoded size. TODO: dwarf 3 could have variable size, but for char that is not expected
  if not t.ReadSize(nil, Size) then
    exit;

  if Size = 1 then
    Result := GetAsString
  else
  if Size = 2 then begin // pchar
    if not Context.MemManager.ReadPWChar(a, 0, Result) then begin
      Result := '';
      SetLastError(Context.LastMemError);
      exit;
    end;
  end
  else
    Result := inherited GetAsWideString;
end;

function TFpPasParserValueCastToPointer.GetAddress: TFpDbgMemLocation;
begin
  Result := FValue.Address;
end;

function TFpPasParserValueCastToPointer.GetDerefAddress: TFpDbgMemLocation;
begin
  Result := TargetLoc(TDbgPtr(AsCardinal));
end;

function TFpPasParserValueCastToPointer.GetMember(AIndex: Int64): TFpValue;
var
  ti: TFpSymbol;
  addr: TFpDbgMemLocation;
  Tmp: TFpValueConstAddress;
  Size: TFpDbgValueSize;
begin
  Result := nil;

  ti := FTypeSymbol.TypeInfo;
  addr := DerefAddress;
  if not IsTargetAddr(addr) then begin
    SetLastError(CreateErrorWithPos(fpErrCannotDeref_p,
      [ValueToExprText(FValue, MAX_ERR_EXPR_QUOTE_LEN)]
    ));
    exit;
  end;
  {$PUSH}{$R-}{$Q-} // TODO: check overflow
  if (ti <> nil) and (AIndex <> 0) then begin
    // Only test for hardcoded size. TODO: dwarf 3 could have variable size, but for char that is not expected
    // TODO: Size of member[0] ?
    if not ti.ReadSize(nil, Size) then begin
      SetLastError(CreateError(fpErrAnyError, ['Can index element of unknown size']));
      exit;
    end;
    AIndex := AIndex * SizeToFullBytes(Size);
  end;
  addr.Address := addr.Address + AIndex;
  {$POP}

  Tmp := TFpValueConstAddress.Create(addr);
  if ti <> nil then begin
    Result := ti.TypeCastValue(Tmp);
    if Result is TFpValueDwarfBase then
      TFpValueDwarfBase(Result).Context := Context;
    Tmp.ReleaseReference;
  end
  else
    Result := Tmp;
end;

constructor TFpPasParserValueCastToPointer.Create(AValue: TFpValue; ATypeInfo: TFpSymbol;
  AContext: TFpDbgSimpleLocationContext; AnExprPos: Integer; AnExprText: String);
begin
  inherited Create(AContext, AnExprPos, AnExprText);
  FValue := AValue;
  FValue.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FValue, 'TPasParserSymbolValueCastToPointer'){$ENDIF};
  FTypeSymbol := ATypeInfo;
  FTypeSymbol.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FTypeSymbol, 'TPasParserSymbolValueCastToPointer'){$ENDIF};
  Assert((FTypeSymbol=nil) or (FTypeSymbol.Kind = skPointer), 'TPasParserSymbolValueCastToPointer.Create');
end;

destructor TFpPasParserValueCastToPointer.Destroy;
begin
  FValue.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FValue, 'TPasParserSymbolValueCastToPointer'){$ENDIF};
  FTypeSymbol.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FTypeSymbol, 'TPasParserSymbolValueCastToPointer'){$ENDIF};
  inherited Destroy;
end;

{ TPasParserSymbolValueMakeReftype }

function TFpPasParserValueMakeReftype.DebugText(AIndent: String): String;
begin
  Result := inherited DebugText(AIndent)
          + AIndent + '-RefLevel = ' + dbgs(FRefLevel) + LineEnding
          + AIndent + '-SourceSymbol = ' + DbgsSymbol(FSourceTypeSymbol, AIndent + '  ') + LineEnding
          + AIndent + '-Symbol = ' + DbgsSymbol(FTypeSymbol, AIndent + '  ') + LineEnding;
end;

function TFpPasParserValueMakeReftype.GetDbgSymbol: TFpSymbol;
begin
  if FTypeSymbol = nil then begin
    FTypeSymbol := TPasParserSymbolPointer.Create(FSourceTypeSymbol, FContext, FExprPos, FExprText, FRefLevel);
    {$IFDEF WITH_REFCOUNT_DEBUG}FTypeSymbol.DbgRenameReference(@FSourceTypeSymbol, 'TPasParserSymbolValueMakeReftype'){$ENDIF};
  end;
  Result := FTypeSymbol;
end;

constructor TFpPasParserValueMakeReftype.Create(ATypeInfo: TFpSymbol;
  AnExpressionPart: TFpPascalExpressionPart);
begin
  inherited Create(AnExpressionPart);
  FSourceTypeSymbol := ATypeInfo;
  FSourceTypeSymbol.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FSourceTypeSymbol, 'TPasParserSymbolValueMakeReftype'){$ENDIF};
  FRefLevel := 1;
end;

destructor TFpPasParserValueMakeReftype.Destroy;
begin
  FSourceTypeSymbol.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FSourceTypeSymbol, 'TPasParserSymbolValueMakeReftype'){$ENDIF};
  FTypeSymbol.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FSourceTypeSymbol, 'TPasParserSymbolValueMakeReftype'){$ENDIF};
  inherited Destroy;
end;

procedure TFpPasParserValueMakeReftype.IncRefLevel;
begin
  inc(FRefLevel);
end;

function TFpPasParserValueMakeReftype.GetTypeCastedValue(ADataVal: TFpValue): TFpValue;
begin
  Result := DbgSymbol.TypeCastValue(ADataVal);
  if Result is TFpValueDwarfBase then
    TFpValueDwarfBase(Result).Context := Context;
end;


{ TPasParserDerefPointerSymbolValue }

function TFpPasParserValueDerefPointer.DebugText(AIndent: String): String;
begin
  Result := inherited DebugText(AIndent)
          + AIndent + '-Value= ' + DbgsResultValue(FValue, AIndent + '  ') + LineEnding;
end;

function TFpPasParserValueDerefPointer.GetFieldFlags: TFpValueFieldFlags;
var
  t: TFpSymbol;
begin
  // MUST *NOT* have ordinal
  Result := [svfAddress];
  t := FValue.TypeInfo;
  if t <> nil then t := t.TypeInfo;
  if t <> nil then
    if t.Kind = skPointer then begin
      //Result := Result + [svfSizeOfPointer];
      Result := Result + [svfSizeOfPointer, svfCardinal, svfOrdinal]; // TODO: svfCardinal ???
    end
    else
      Result := Result + [svfSize];
end;

function TFpPasParserValueDerefPointer.GetAddress: TFpDbgMemLocation;
begin
  Result := FValue.DerefAddress;
  if IsValidLoc(Result) then begin
    SetLastError(Context.LastMemError);
    exit;
  end;

  if FAddressOffset <> 0 then begin
    assert(IsTargetAddr(Result ), 'TFpPasParserValueDerefPointer.GetAddress: TargetLoc(Result)');
    if IsTargetAddr(Result) then
      Result.Address := Result.Address + FAddressOffset
    else
      Result := InvalidLoc;
  end;
end;

function TFpPasParserValueDerefPointer.DoGetSize(out ASize: TFpDbgValueSize
  ): Boolean;
var
  t: TFpSymbol;
begin
  t := FValue.TypeInfo;
  if t <> nil then t := t.TypeInfo;
  if t <> nil then
    t.ReadSize(nil, ASize) // TODO: create a value object for the deref
  else
    Result := inherited DoGetSize(ASize);
end;

function TFpPasParserValueDerefPointer.GetAsCardinal: QWord;
var
  m: TFpDbgMemManager;
  Addr: TFpDbgMemLocation;
  Ctx: TFpDbgLocationContext;
  AddrSize: Integer;
begin
  Result := FCardinal;
  if FCardinalRead then exit;

  Ctx := Context;
  if Ctx = nil then exit;
  AddrSize := Ctx.SizeOfAddress;
  if (AddrSize <= 0) or (AddrSize > SizeOf(FCardinal)) then exit;
  m := Ctx.MemManager;
  if m = nil then exit;

  FCardinal := 0;
  FCardinalRead := True;
  Addr := GetAddress;
  if not Context.MemModel.IsReadableLocation(Addr) then exit;
  FCardinal := LocToAddrOrNil(Ctx.ReadAddress(Addr, SizeVal(Ctx.SizeOfAddress)));

  Result := FCardinal;
end;

function TFpPasParserValueDerefPointer.GetTypeInfo: TFpSymbol;
var
  t: TFpSymbol;
begin
  t := FValue.TypeInfo;
  if t <> nil then t := t.TypeInfo;
  if t <> nil then
    Result := t
  else
    Result := inherited GetTypeInfo;
end;

constructor TFpPasParserValueDerefPointer.Create(AValue: TFpValue;
  AnExpressionPart: TFpPascalExpressionPart);
begin
  Create(AValue, AnExpressionPart, 0);
end;

constructor TFpPasParserValueDerefPointer.Create(AValue: TFpValue;
  AnExpressionPart: TFpPascalExpressionPart; AOffset: Int64);
begin
  inherited Create(AnExpressionPart);
  FValue := AValue;
  FValue.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FValue, 'TPasParserDerefPointerSymbolValue'){$ENDIF};
  FAddressOffset := AOffset;
end;

destructor TFpPasParserValueDerefPointer.Destroy;
begin
  inherited Destroy;
  FValue.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FValue, 'TPasParserDerefPointerSymbolValue'){$ENDIF};
end;

{ TPasParserAddressOfSymbolValue }

function TFpPasParserValueAddressOf.GetPointedToValue: TFpValue;
begin
  Result := FValue;
end;

function TFpPasParserValueAddressOf.DebugText(AIndent: String): String;
begin
  Result := inherited DebugText(AIndent)
          + AIndent + '-Value= ' + DbgsResultValue(FValue, AIndent + '  ') + LineEnding
          + AIndent + '-Symbol = ' + DbgsSymbol(FTypeInfo, AIndent + '  ') + LineEnding;
end;

function TFpPasParserValueAddressOf.GetKind: TDbgSymbolKind;
begin
  Result := skPointer;
end;

function TFpPasParserValueAddressOf.GetFieldFlags: TFpValueFieldFlags;
begin
    Result := [svfOrdinal, svfCardinal, svfSizeOfPointer, svfDataAddress];
    if FValue.Kind in [skChar] then
      Result := Result + FValue.FieldFlags * [svfString, svfWideString];
end;

function TFpPasParserValueAddressOf.GetAsInteger: Int64;
begin
  Result := Int64(LocToAddrOrNil(FValue.Address));
end;

function TFpPasParserValueAddressOf.GetAsCardinal: QWord;
begin
  Result := QWord(LocToAddrOrNil(FValue.Address));
end;

function TFpPasParserValueAddressOf.GetTypeInfo: TFpSymbol;
begin
  Result := FTypeInfo;
  if Result <> nil then
    exit;
  if FValue.TypeInfo = nil then
    exit;

  FTypeInfo := TPasParserSymbolPointer.Create(FValue.TypeInfo, FContext, FExprPos, FExprText, 1);
  {$IFDEF WITH_REFCOUNT_DEBUG}FTypeInfo.DbgRenameReference(@FTypeInfo, 'TPasParserAddressOfSymbolValue');{$ENDIF}
  Result := FTypeInfo;
end;

function TFpPasParserValueAddressOf.GetDerefAddress: TFpDbgMemLocation;
begin
  Result := FValue.Address;
end;

function TFpPasParserValueAddressOf.GetMember(AIndex: Int64): TFpValue;
var
  ti: TFpSymbol;
  addr: TFpDbgMemLocation;
  Tmp: TFpValueConstAddress;
  Size: TFpDbgValueSize;
begin
  if (AIndex = 0) or (FValue = nil) then begin
    Result := FValue;
    if Result <> nil then
      Result.AddReference;
    exit;
  end;

  Result := nil;
  ti := FValue.TypeInfo;
  addr := FValue.Address;
  if not IsTargetAddr(addr) then begin
    //LastError := CreateError(fpErrAnyError, ['Internal dereference error']);
    exit;
  end;
  {$PUSH}{$R-}{$Q-} // TODO: check overflow
  if (ti <> nil) and (AIndex <> 0) then begin
    // Only test for hardcoded size. TODO: dwarf 3 could have variable size, but for char that is not expected
    // TODO: Size of member[0] ?
    if not ti.ReadSize(nil, Size) then begin
      SetLastError(CreateError(fpErrAnyError, ['Can index element of unknown size']));
      exit;
    end;
    AIndex := AIndex * SizeToFullBytes(Size);
  end;
  addr.Address := addr.Address + AIndex;
  {$POP}

  Tmp := TFpValueConstAddress.Create(addr);
  if ti <> nil then begin
    Result := ti.TypeCastValue(Tmp);
    if Result is TFpValueDwarfBase then
      TFpValueDwarfBase(Result).Context := Context;
    Tmp.ReleaseReference;
  end
  else
    Result := Tmp;
end;

function TFpPasParserValueAddressOf.GetAsString: AnsiString;
var
  a: TFpDbgMemLocation;
  WResult: WideString;
begin
  a := FValue.Address;

  if (FValue.Kind = skChar) and IsTargetNotNil(a) then begin
    if (FValue.DataSize = 1) and Context.MemManager.ReadPChar(a, 0, Result) then
      exit;
    if (FValue.DataSize = 2) and Context.MemManager.ReadPWChar(a, 0, WResult) then
      exit(WResult);
  end;

  //if (FValue.Kind = skChar) and IsTargetNotNil(a) and
  //   Context.MemManager.ReadPChar(a, 0, Result)
  //then
  //  exit;
  //
  //if (FValue.Kind = skWideString) and IsTargetNotNil(a) and
  //   Context.MemManager.ReadPWChar(a, 0, WResult)
  //then
  //  exit(WResult);

  Result := FValue.AsString;
  if IsError(FValue.LastError) then
    SetLastError(FValue.LastError);
end;

function TFpPasParserValueAddressOf.GetAsWideString: WideString;
var
  AResult: AnsiString;
  a: TFpDbgMemLocation;
begin
  a := FValue.Address;

  if (FValue.Kind = skChar) and IsTargetNotNil(a) then begin
    if (FValue.DataSize = 1) and Context.MemManager.ReadPChar(a, 0, AResult) then
      exit(AResult);
    if (FValue.DataSize = 2) and Context.MemManager.ReadPWChar(a, 0, Result) then
      exit;
  end;

  Result := FValue.AsWideString;
  if IsError(FValue.LastError) then
    SetLastError(FValue.LastError);
end;

constructor TFpPasParserValueAddressOf.Create(AValue: TFpValue;
  AnExpressionPart: TFpPascalExpressionPart);
begin
  inherited Create(AnExpressionPart);
  FValue := AValue;
  FValue.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FValue, 'TPasParserAddressOfSymbolValue'){$ENDIF};
end;

destructor TFpPasParserValueAddressOf.Destroy;
begin
  inherited Destroy;
  FValue.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FValue, 'TPasParserAddressOfSymbolValue'){$ENDIF};
  FTypeInfo.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FTypeInfo, 'TPasParserAddressOfSymbolValue'){$ENDIF};
end;

{ TFpValueFlatteArray }

function TFpValueFlatteArray.GetOrdHighBound: Int64;
begin
  Result := FList.Count - 1;
end;

constructor TFpValueFlatteArray.Create(ALowBound: Integer);
begin
  inherited Create(ALowBound);
  FList := TRefCntObjList.Create;
  Flags := Flags + [vfArrayUpperBoundLimit];
end;

destructor TFpValueFlatteArray.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TFpValueFlatteArray.GetMember(AIndex: Int64): TFpValue;
begin
  if AIndex >= FList.Count then
    exit(nil);
  Result := TFpValue(FList[AIndex]);
  Result.AddReference;
end;

function TFpValueFlatteArray.GetMemberCount: Integer;
begin
  Result := FList.Count;
end;

{ TFpPascalExpressionCacheFlattenKey }

class operator TFpPascalExpressionCacheFlattenKey. = (a, b: TFpPascalExpressionCacheFlattenKey
  ): boolean;
begin
  Result := (a.CtxThread = b.CtxThread) and
            (a.CtxStack  = b.CtxStack) and
            (a.Flags     = b.Flags) and
            (a.ExpandArrayDepth  = b.ExpandArrayDepth) and
            (a.Key = b.Key);
end;

class operator TFpPascalExpressionCacheFlattenKey.<(a, b: TFpPascalExpressionCacheFlattenKey
  ): boolean;
begin
  raise Exception.Create('not supported');
  result := false;
end;

class operator TFpPascalExpressionCacheFlattenKey.>(a, b: TFpPascalExpressionCacheFlattenKey
  ): boolean;
begin
  raise Exception.Create('not supported');
  result := false;
end;

{ TFpPascalExpressionCacheFlatten }

function TFpPascalExpressionCacheFlatten.DoKeyPtrComp(Key1, Key2: Pointer): Integer;
begin
  if PFpPascalExpressionCacheFlattenKey(Key1)^ = PFpPascalExpressionCacheFlattenKey(Key2)^
  then Result := 0
  else Result := -1; // no sorting needed
end;

procedure TFpPascalExpressionCacheFlatten.Deref(Item: Pointer);
begin
  Finalize(TFpPascalExpressionCacheFlattenKey(Item^));
  PFpValueFlatteArray(Pointer(PByte(Item)+KeySize))^.ReleaseReference;
end;

constructor TFpPascalExpressionCacheFlatten.Create;
begin
  inherited Create;
  OnKeyPtrCompare := @DoKeyPtrComp;
end;

function TFpPascalExpressionCacheFlatten.Add(const AKey: TFpPascalExpressionCacheFlattenKey;
  const AData: TFpValueFlatteArray): Integer;
begin
  while Count >= 5 do
    Delete(0);
  AData.AddReference;
  Result := inherited Add(AKey, AData);
end;

function TFpPascalExpressionCacheFlatten.Replace(const AKey: TFpPascalExpressionCacheFlattenKey;
  const AData: TFpValueFlatteArray): Integer;
var
  i: Integer;
begin
  i := IndexOf(AKey);
  if i >= 0 then
    Delete(i);
  Result := Add(AKey, AData);
end;

{ TAddrSeenList }

function TAddrSeenList.DoKeyPtrComp(Key1, Key2: Pointer): Integer;
begin
  if PFpDbgMemLocation(Key1)^ = PFpDbgMemLocation(Key2)^
  then Result := 0
  else Result := -1; // no sorting needed
end;

constructor TAddrSeenList.Create;
begin
  inherited Create;
  OnKeyPtrCompare := @DoKeyPtrComp;
end;

{ TFpPascalExpressionPartOperatorColonAsSeparator }

procedure TFpPascalExpressionPartOperatorColonAsSeparator.Init;
begin
  FPrecedence := PRECEDENCE_SEPARATOR_COLON;
  inherited Init;
end;

function TFpPascalExpressionPartOperatorColonAsSeparator.DoGetResultValue: TFpValue;
begin
  assert(False, 'TFpPascalExpressionPartOperatorColonAsSeparator.DoGetResultValue: False');
  Result := nil;
end;

function TFpPascalExpressionPartOperatorColonAsSeparator.IsClosed: boolean;
begin
  // Colon separators should only exist in intrinsics
  Result := (Parent <> nil) and
            (Parent is TFpPascalExpressionPartBracket) and
            (Parent.IsClosed);
end;

{ TPasParserSymbolArrayDeIndex }

function TPasParserSymbolArrayDeIndex.GetNestedSymbolCount: Integer;
begin
  Result := (inherited GetNestedSymbolCount) - 1;
end;

function TPasParserSymbolArrayDeIndex.GetNestedSymbol(AIndex: Int64): TFpSymbol;
begin
  Result := inherited GetNestedSymbol(AIndex + 1);
end;

constructor TPasParserSymbolArrayDeIndex.Create(const AnArray: TFpSymbol);
begin
  FArray := AnArray;
  FArray.AddReference;
  inherited Create('');
  SetKind(skArray);
  SetForwardToSymbol(FArray);
end;

destructor TPasParserSymbolArrayDeIndex.Destroy;
begin
  ReleaseRefAndNil(FArray);
  inherited Destroy;
end;

{ TPasParserSymbolPointer }

procedure TPasParserSymbolPointer.TypeInfoNeeded;
var
  t: TPasParserSymbolPointer;
begin
  if FPointerLevels = 0 then begin
    SetTypeInfo(FPointedTo);
    exit;
  end;
  assert(FPointerLevels > 1, 'TPasParserSymbolPointer.TypeInfoNeeded: FPointerLevels > 1');
  t := TPasParserSymbolPointer.Create(FPointedTo, FContext, FExprPos, FExprText, FPointerLevels-1);
  SetTypeInfo(t);
  t.ReleaseReference;
end;

function TPasParserSymbolPointer.DoReadSize(const AValueObj: TFpValue; out
  ASize: TFpDbgValueSize): Boolean;
begin
  ASize := SizeVal(FContext.SizeOfAddress);
  Result := True;
end;

constructor TPasParserSymbolPointer.Create(const APointedTo: TFpSymbol;
  AContext: TFpDbgSimpleLocationContext; AnExprPos: Integer; AnExprText: String;
  APointerLevels: Integer);
begin
  FContext  := AContext;
  FExprPos  := AnExprPos;
  FExprText := AnExprText;
  inherited Create('');
  FPointerLevels := APointerLevels;
  FPointedTo := APointedTo;
  FPointedTo.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(FPointedTo, 'TPasParserSymbolPointer'){$ENDIF};
  if APointerLevels = 1 then
    SetTypeInfo(APointedTo);
  SetKind(skPointer);
  SetSymbolType(stType);
end;

constructor TPasParserSymbolPointer.Create(const APointedTo: TFpSymbol;
  AnExpressionPart: TFpPascalExpressionPart; APointerLevels: Integer);
begin
  Create(APointedTo, AnExpressionPart.ExpressionData.Scope.LocationContext,
    AnExpressionPart.GetPos, AnExpressionPart.GetText(MAX_ERR_EXPR_QUOTE_LEN),
    APointerLevels
  );
end;

constructor TPasParserSymbolPointer.Create(const APointedTo: TFpSymbol;
  AnExpressionPart: TFpPascalExpressionPart);
begin
  Create(APointedTo, AnExpressionPart, 1);
end;

destructor TPasParserSymbolPointer.Destroy;
begin
  FPointedTo.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(FPointedTo, 'TPasParserSymbolPointer'){$ENDIF};
  inherited Destroy;
end;

function TPasParserSymbolPointer.TypeCastValue(AValue: TFpValue): TFpValue;
begin
  Result := TFpPasParserValueCastToPointer.Create(AValue, Self, FContext, FExprPos, FExprText );
end;


{ TFpPascalExpressionPartBracketIndex }

procedure TFpPascalExpressionPartBracketIndex.Init;
begin
  FPrecedence := PRECEDENCE_ARRAY_IDX;
  inherited Init;
end;

function TFpPascalExpressionPartBracketIndex.DoGetResultValue: TFpValue;
var
  TmpVal, TmpVal2, TmpIndex, AutoDereVal: TFpValue;
  i: Integer;
  Offs, Len: Int64;
  ti: TFpSymbol;
  IsPChar: Boolean;
  v: String;
  w: WideString;
  a: TFpDbgMemLocation;
begin
  Result := nil;
  assert(Count >= 2, 'TFpPascalExpressionPartBracketIndex.DoGetResultValue: Count >= 2');
  if Count < 2 then begin
    SetError(fpErrPasParserMissingIndexExpression, [GetFullText(MAX_ERR_EXPR_QUOTE_LEN), GetPos]);
    exit;
  end;

  TmpVal := Items[0].ResultValue;
  if TmpVal = nil then exit;

  TmpVal.AddReference;
  for i := 1 to Count - 1 do begin
    TmpVal2 := nil;
    TmpIndex := Items[i].ResultValue;
    if TmpIndex = nil then begin
      // error should be set by Items[i]
      TmpVal.ReleaseReference;
      exit;
    end;

    if ExpressionData.AutoDeref and (TmpVal.Kind = skPointer) and
      (TmpVal.TypeInfo <> nil) and (TmpVal.TypeInfo.TypeInfo <> nil) and
      (TmpVal.TypeInfo.TypeInfo.Kind in [skString, skAnsiString, skWideString, skArray])
    then begin
      // Copy from TFpPascalExpressionPartOperatorDeRef.DoGetResultValue
      if (svfDataAddress in TmpVal.FieldFlags) and (IsReadableLoc(TmpVal.DerefAddress)) and // TODO, what if Not readable addr
         (TmpVal.TypeInfo <> nil) //and (TmpVal.TypeInfo.TypeInfo <> nil)
      then begin
        AutoDereVal := TmpVal.Member[0];
        TmpVal.ReleaseReference;
        TmpVal := AutoDereVal;
      end;
      if (TmpVal = nil) then begin
        SetErrorWithPos(fpErrCannotDeref_p, [Items[0].GetText(MAX_ERR_EXPR_QUOTE_LEN)]);
        exit;
      end;
    end;

    case TmpVal.Kind of
      skArray: begin
          if (svfInteger in TmpIndex.FieldFlags) then
            TmpVal2 := TmpVal.Member[TmpIndex.AsInteger]
          else
          if (svfOrdinal in TmpIndex.FieldFlags) and
             (TmpIndex.AsCardinal <= high(Int64))
          then
            TmpVal2 := TmpVal.Member[TmpIndex.AsCardinal]
          else
          begin
            SetError(fpErrPasParserIndexError_Wrapper,
              [Items[0].GetFullText(MAX_ERR_EXPR_QUOTE_LEN), Items[0].GetPos],
              CreateErrorWithPos(fpErrExpectedOrdinalVal_p, [Items[i].GetFullText(MAX_ERR_EXPR_QUOTE_LEN)], Items[i].GetPos)
            );
            TmpVal.ReleaseReference;
            exit;
          end;
        end; // Kind = skArray
      skPointer: begin
          if (svfInteger in TmpIndex.FieldFlags) then
            Offs := TmpIndex.AsInteger
          else
          if (svfOrdinal in TmpIndex.FieldFlags) and (TmpIndex.AsCardinal <= high(Int64))
          then
            Offs := Int64(TmpIndex.AsCardinal)
          else
          begin
            SetError(fpErrPasParserIndexError_Wrapper,
              [Items[0].GetFullText(MAX_ERR_EXPR_QUOTE_LEN), Items[0].GetPos],
              CreateErrorWithPos(fpErrExpectedOrdinalVal_p, [Items[i].GetFullText(MAX_ERR_EXPR_QUOTE_LEN)], Items[i].GetPos)
            );
            TmpVal.ReleaseReference;
            exit;
          end;

          ti := TmpVal.TypeInfo;
          if (ti <> nil) then ti := ti.TypeInfo;
          IsPChar := (ti <> nil) and (ti.Kind in [skChar]) and (Offs > 0) and
                     (not(TmpVal is TFpPasParserValueAddressOf)) and
                     (not(TmpVal is TFpPasParserValueCastToPointer)) and
                     (not(TmpVal is TFpPasParserValueMakeReftype));
          if IsPChar then ExpressionData.FHasPCharIndexAccess := True;
          if IsPChar and ExpressionData.FixPCharIndexAccess then begin
            // fix for string in dwarf 2
            if Offs > 0 then
              dec(Offs);
          end;

          TmpVal2 := TmpVal.Member[Offs];
          if IsError(TmpVal.LastError) then
            SetError(fpErrPasParserIndexError_Wrapper,
              [Items[0].GetFullText(MAX_ERR_EXPR_QUOTE_LEN), Items[0].GetPos],
              TmpVal.LastError
            );
        end;
      skString, skAnsiString: begin
          //TODO: move to FpDwarfValue.member ??
          if (Count = 2) and (Items[1] is TFpPascalExpressionPartOperatorArraySlice) and
             TFpPascalExpressionPartOperatorArraySlice(Items[1]).Controller.CanDisableSlice
          then begin
            TFpPascalExpressionPartOperatorArraySlice(Items[1]).Controller.DisableSlice := True;
            Offs := TFpPascalExpressionPartOperatorArraySlice(Items[1]).StartValue;
            Len  := TFpPascalExpressionPartOperatorArraySlice(Items[1]).EndValue - Offs + 1;
            TmpVal.GetSubString(Offs, Len, v);
            TmpVal2 := TFpValueConstString.Create(v);
          end
          else begin
            if (svfInteger in TmpIndex.FieldFlags) then
              Offs := TmpIndex.AsInteger
            else
            if (svfOrdinal in TmpIndex.FieldFlags) and (TmpIndex.AsCardinal <= high(Int64))
            then
              Offs := Int64(TmpIndex.AsCardinal)
            else
            begin
              SetError(fpErrPasParserIndexError_Wrapper,
                [Items[0].GetFullText(MAX_ERR_EXPR_QUOTE_LEN), Items[0].GetPos],
                CreateErrorWithPos(fpErrExpectedOrdinalVal_p, [Items[i].GetFullText(MAX_ERR_EXPR_QUOTE_LEN)], Items[i].GetPos)
              );
              TmpVal.ReleaseReference;
              exit;
            end;

            if (not TmpVal.GetSubString(Offs, 1, v)) or (v = '') then begin
              SetError(fpErrPasParserIndexError_Wrapper,
                [Items[0].GetFullText(MAX_ERR_EXPR_QUOTE_LEN), Items[0].GetPos],
                CreateError(fpErrIndexOutOfRange, [Offs])
              );
              TmpVal.ReleaseReference;
              exit;
            end;

            TmpVal2 := TFpValueConstChar.Create(v[1]);
            if TmpVal.TypeInfo <> nil then
              TFpValueConstChar(TmpVal2).SetType(TmpVal.TypeInfo.TypeInfo);
          end;
          a := TmpVal.DataAddress;
          if IsTargetAddr(a) and IsReadableMem(a) then
            TFpValueConstWithType(TmpVal2).SetAddress(a + Offs-1);
        end;
      skWideString: begin
          //TODO: move to FpDwarfValue.member ??
          if (Count = 2) and (Items[1] is TFpPascalExpressionPartOperatorArraySlice)
          then begin
            Offs := TFpPascalExpressionPartOperatorArraySlice(Items[1]).StartValue;
            Len  := TFpPascalExpressionPartOperatorArraySlice(Items[1]).EndValue - Offs + 1;
            TmpVal.GetSubWideString(Offs, Len, w);
            TmpVal2 := TFpValueConstString.Create(w);
          end
          else begin
            if (svfInteger in TmpIndex.FieldFlags) then
              Offs := TmpIndex.AsInteger
            else
            if (svfOrdinal in TmpIndex.FieldFlags) and (TmpIndex.AsCardinal <= high(Int64))
            then
              Offs := Int64(TmpIndex.AsCardinal)
            else
            begin
              SetError(fpErrPasParserIndexError_Wrapper,
                [Items[0].GetFullText(MAX_ERR_EXPR_QUOTE_LEN), Items[0].GetPos],
                CreateErrorWithPos(fpErrExpectedOrdinalVal_p, [Items[i].GetFullText(MAX_ERR_EXPR_QUOTE_LEN)], Items[i].GetPos)
              );
              TmpVal.ReleaseReference;
              exit;
            end;

            if (not TmpVal.GetSubWideString(Offs, 1, w)) or (w='') then begin
              SetError(fpErrPasParserIndexError_Wrapper,
                [Items[0].GetFullText(MAX_ERR_EXPR_QUOTE_LEN), Items[0].GetPos],
                CreateError(fpErrIndexOutOfRange, [Offs])
              );
              TmpVal.ReleaseReference;
              exit;
            end;

            TmpVal2 := TFpValueConstWideChar.Create(w[1]);
          end;
          a := TmpVal.DataAddress;
          if IsTargetAddr(a) and IsReadableMem(a) then
            TFpValueConstWideChar(TmpVal2).SetAddress(a + (Offs-1)*2);
        end;
      else
        begin
          SetError(fpErrPasParserIndexError_Wrapper,
            [Items[0].GetFullText(MAX_ERR_EXPR_QUOTE_LEN), Items[0].GetPos],
            CreateError(fpErrTypeNotIndexable, [])
          );
          TmpVal.ReleaseReference;
          exit;
        end;
    end;

    TmpVal.ReleaseReference;
    if TmpVal2 = nil then begin
      SetError('Internal Error, attempting to read array element');
      exit;
    end;
    TmpVal := TmpVal2;
  end;

  Result := TmpVal;
  {$IFDEF WITH_REFCOUNT_DEBUG}if Result <> nil then Result.DbgRenameReference(nil, 'DoGetResultValue'){$ENDIF};
end;

function TFpPascalExpressionPartBracketIndex.IsValidAfterPart(APrevPart: TFpPascalExpressionPart): Boolean;
begin
  Result := inherited IsValidAfterPart(APrevPart);
  Result := Result and APrevPart.CanHaveOperatorAsNext;
  if (APrevPart.Parent <> nil) and (APrevPart.Parent.HasPrecedence) then
    Result := False;
end;

function TFpPascalExpressionPartBracketIndex.HandleNextPartInBracket(APart: TFpPascalExpressionPart): TFpPascalExpressionPart;
begin
  Result := Self;
  if Count < 1 then begin // Todo a,b,c
    SetError(APart, 'Internal error handling [] '+GetText+': '); // Missing the array on which this index works
    APart.Free;
    exit;
  end;
  if (Count > 1) and (not AfterComma) then begin
    SetError(APart, 'Comma or closing "]" expected '+GetText+': ');
    APart.Free;
    exit;
  end;
  if not IsValidNextPart(APart) then begin
    SetError(APart, 'Invalid operand in [] '+GetText+': ');
    APart.Free;
    exit;
  end;

  Add(APart);
  Result := APart;
end;

function TFpPascalExpressionPartBracketIndex.MaybeHandlePrevPart(APrevPart: TFpPascalExpressionPart;
  var AResult: TFpPascalExpressionPart): Boolean;
var
  ALeftSide: TFpPascalExpressionPart;
begin
  //Result := MaybeAddLeftOperand(APrevPart, AResult);

  Result := APrevPart.IsValidNextPart(Self);
  if not Result then
    exit;

  AResult := Self;
  if (Count > 0)  // function/type already set
  then begin
    SetError(APrevPart, 'Parse error in () '+GetText+': ');
    APrevPart.Free;
    exit;
  end;

  ALeftSide := APrevPart.FindLeftSideOperandByPrecedence(Self);
  if ALeftSide = nil then begin
    SetError(Self, 'Internal parser error for operator '+GetText+': ');
    APrevPart.Free;
    exit;
  end;

  ALeftSide.ReplaceInParent(Self);
  Add(ALeftSide);
end;

procedure TFpPascalExpressionPartBracketIndex.DoHandleEndOfExpression;
begin
  inherited DoHandleEndOfExpression;
  if (Count < 2) then
    SetError(fpErrPasParserMissingIndexExpression, [GetFullText(MAX_ERR_EXPR_QUOTE_LEN), GetPos]);
end;

function TFpPascalExpressionPartBracketIndex.HandleSeparator(
  ASeparatorType: TSeparatorType; var APart: TFpPascalExpressionPart): Boolean;
begin
  if (not (ASeparatorType = ppstComma)) or IsClosed then begin
    Result := inherited HandleSeparator(ASeparatorType, APart);
    exit;
  end;

  Result := (Count > FAfterComma) and (Count > 1); // First element is name of array (in front of "[")
  if Result then begin
    CheckBeforeSeparator(APart);
    SetAfterCommaFlag;
    APart := Self;
  end;
end;

procedure TFpPascalExpressionPartBracketIndex.CloseBracket(
  ALastAddedPart: TFpPascalExpressionPart;
  APreviousArraySliceList: PFpPascalExpressionPartOperatorArraySlice; AStartChar: PChar;
  AnEndChar: PChar);
var
  i: Integer;
begin
  inherited CloseBracket(ALastAddedPart, APreviousArraySliceList, AStartChar, AnEndChar);
  for i := 1 to Count - 1 do
    Items[i].DoParentIndexBraceClosed(APreviousArraySliceList);
end;

function TFpPascalExpressionPartBracketIndex.ReturnsVariant: boolean;
var
  Itm0: TFpPascalExpressionPart;
begin
  Result := inherited ReturnsVariant;
  if Result then
    exit;

  Itm0 := Items[0];
  if Itm0 = nil then
    exit;

  Result := Itm0.ReturnsVariant;
end;

{ TFpPascalExpressionPartBracketSet }

function TFpPascalExpressionPartBracketSet.DoGetResultValue: TFpValue;
var
  i: Integer;
  itm: TFpPascalExpressionPart;
  m: TFpValue;
begin
  Result := TFpValueConstSet.Create;
  for i := 0 to Count - 1 do begin
    itm := Items[i];
    m := itm.ResultValue;
    TFpValueConstSet(Result).AddVal(m);
  end;
end;

function TFpPascalExpressionPartBracketSet.HandleNextPartInBracket(APart: TFpPascalExpressionPart): TFpPascalExpressionPart;
begin
  Result := Self;
  if (Count > 0) and (not AfterComma) then begin
    SetError('To many expressions'); // TODO comma
    APart.Free;
    exit;
  end;

  Result := APart;
  Add(APart);
end;

function TFpPascalExpressionPartBracketSet.HandleSeparator(
  ASeparatorType: TSeparatorType; var APart: TFpPascalExpressionPart): Boolean;
begin
  if (not (ASeparatorType = ppstComma)) or IsClosed then begin
    Result := inherited HandleSeparator(ASeparatorType, APart);
    exit;
  end;

  Result := (Count > FAfterComma) and (Count > 0);
  if Result then begin
    CheckBeforeSeparator(APart);
    SetAfterCommaFlag;
    APart := Self;
  end;
end;

{ TFpPascalExpressionPartWithPrecedence }

function TFpPascalExpressionPartWithPrecedence.HasPrecedence: Boolean;
begin
  Result := True;
end;


{ TFpPascalExpressionPartBracketArgumentList }

procedure TFpPascalExpressionPartBracketArgumentList.Init;
begin
  FPrecedence := PRECEDENCE_ARG_LIST;
  inherited Init;
end;

function TFpPascalExpressionPartBracketArgumentList.DoGetResultValue: TFpValue;
var
  tmp, tmp2, tmpSelf: TFpValue;
  err: TFpError;
  Itm0: TFpPascalExpressionPart;
  ItmMO: TFpPascalExpressionPartOperatorMemberOf absolute Itm0;
  Params: TFpPascalExpressionPartListForwarder;
begin
  Result := nil;

  if (Count = 0) then begin
    SetError(fpErrPasParserInvalidExpression, []);
    exit;
  end;

  Itm0 := Items[0];

  if Itm0 is TFpPascalExpressionPartIntrinsicBase then begin
    Result := TFpPascalExpressionPartIntrinsicBase(Itm0).DoGetResultValue(Self);
    exit;
  end;

  (* If Itm0 is an identifer we could use [fsfIgnoreEnumVals]
     But then alTop(1) would give "identifer not found", rather than a proper error
  *)
  tmp := Itm0.ResultValue;
  if (tmp = nil) or (not ExpressionData.Valid) then
    exit;

  if (tmp.DbgSymbol <> nil) and (tmp.DbgSymbol.Kind in [skFunction, skFunctionRef]) then begin
    if not Assigned(ExpressionData.OnFunctionCall) then begin
      SetError('calling functions not allowed');
      exit;
    end;

    tmpSelf := nil;
    if (Itm0 is TFpPascalExpressionPartOperatorMemberOf) then begin
      if ItmMO.Count = 2 then
        tmpSelf := ItmMO.Items[0].ResultValue;
      if tmpSelf = nil then begin
        SetError('internal error evaluating method call');
        exit;
      end;
    end;

    err := NoError;
    Params := TFpPascalExpressionPartListForwarder.Create(Self, 1, Count - 1);
    try
      if not ExpressionData.OnFunctionCall(Self, tmp, tmpSelf, Params, Result, err) then begin
        if not IsError(err) then
          SetError('unknown error calling function')
        else
          ExpressionData.SetError(err);
        Result := nil;
      end;
    finally
      Params.Free;
    end;
    {$IFDEF WITH_REFCOUNT_DEBUG}if Result <> nil then Result.DbgRenameReference(nil, 'DoGetResultValue'){$ENDIF};

    exit;
  end;

  if (Count = 2) then begin
    //TODO if tmp is TFpPascalExpressionPartOperatorMakeRef then
    //     AVOID creating the TPasParserSymbolPointer by calling tmp.DbgSymbol
    //     it ran be created in TPasParserSymbolValueCastToPointer if needed.
    if (tmp <> nil) and (tmp.DbgSymbol <> nil) and
       (tmp.DbgSymbol.SymbolType = stType)
    then begin
      // This is a typecast
      tmp2 := Items[1].ResultValue;
      if tmp2 <> nil then
        Result := tmp.GetTypeCastedValue(tmp2);
        //Result := tmp.DbgSymbol.TypeCastValue(tmp2);
      if Result <> nil then
        {$IFDEF WITH_REFCOUNT_DEBUG}Result.DbgRenameReference(nil, 'DoGetResultValue'){$ENDIF};

      exit;
    end;
  end;

  // Must be function call // skProcedure is not handled
  SetError('unknown type or function');
end;

function TFpPascalExpressionPartBracketArgumentList.DoGetIsTypeCast: Boolean;
begin
  Result := False;
end;

function TFpPascalExpressionPartBracketArgumentList.IsValidAfterPart(APrevPart: TFpPascalExpressionPart): Boolean;
begin
  Result := inherited IsValidAfterPart(APrevPart);
  Result := Result and APrevPart.CanHaveOperatorAsNext;
  if (APrevPart.Parent <> nil) and (APrevPart.Parent.HasPrecedence) then
    Result := False;
end;

function TFpPascalExpressionPartBracketArgumentList.HandleNextPartInBracket(APart: TFpPascalExpressionPart): TFpPascalExpressionPart;
begin
  Result := Self;
  if Count < 1 then begin // Todo a,b,c
    SetError(APart, 'Internal error handling () '+GetText+': '); // Missing the functionname on which this index works
    APart.Free;
    exit;
  end;

  if Items[0].AcceptParamAsSeparator(APart, Self, Result) then // Must "Add" APart
    exit;

  if (Count > 1) and (not AfterComma) then begin // Todo a,b,c
    SetError(APart, 'Comma or closing ")" expected: '+GetText+': ');
    APart.Free;
    exit;
  end;

  Add(APart);
  Result := APart;

  Items[0].HandleNewParameterInList(APart, Self);
end;

function TFpPascalExpressionPartBracketArgumentList.MaybeHandlePrevPart(APrevPart: TFpPascalExpressionPart;
  var AResult: TFpPascalExpressionPart): Boolean;
var
  ALeftSide: TFpPascalExpressionPart;
begin
  //Result := MaybeAddLeftOperand(APrevPart, AResult);

  Result := APrevPart.IsValidNextPart(Self);
  if not Result then
    exit;

  AResult := Self;
  if (Count > 0)  // function/type already set
  then begin
    SetError(APrevPart, 'Parse error in () '+GetText+': ');
    APrevPart.Free;
    exit;
  end;

  ALeftSide := APrevPart.FindLeftSideOperandByPrecedence(Self);
  if ALeftSide = nil then begin
    SetError(Self, 'Internal parser error for operator '+GetText+': ');
    APrevPart.Free;
    exit;
  end;

  ALeftSide.ReplaceInParent(Self);
  Add(ALeftSide);
end;

function TFpPascalExpressionPartBracketArgumentList.HandleSeparator(
  ASeparatorType: TSeparatorType; var APart: TFpPascalExpressionPart): Boolean;
begin
  if (not (ASeparatorType = ppstComma)) or IsClosed then begin
    Result := inherited HandleSeparator(ASeparatorType, APart);
    exit;
  end;

  Result := (Count > FAfterComma) and (Count > 1); // First element is name of function (in front of "(")
  if Result then begin
    CheckBeforeSeparator(APart);
    SetAfterCommaFlag;
    Items[0].HandleEndOfParameterInList(APart, Self);
    APart := Self;
  end;
end;

procedure TFpPascalExpressionPartBracketArgumentList.CloseBracket(
  ALastAddedPart: TFpPascalExpressionPart;
  APreviousArraySliceList: PFpPascalExpressionPartOperatorArraySlice; AStartChar: PChar;
  AnEndChar: PChar);
begin
  inherited CloseBracket(ALastAddedPart, APreviousArraySliceList, AStartChar, AnEndChar);
  if ExpressionData.Valid then
    Items[0].HandleEndOfParameterInList(ALastAddedPart, Self);
end;

function TFpPascalExpressionPartBracketArgumentList.ReturnsVariant: boolean;
var
  Itm0: TFpPascalExpressionPart;
begin
  Result := inherited ReturnsVariant;
  if Result then
    exit;

  Itm0 := Items[0];
  if Itm0 = nil then
    exit;

  Result := Itm0.ReturnsVariant;
end;

function TFpPascalExpressionPartBracketArgumentList.IntrinsicType: TFpIntrinsicFunc;
begin
  Result := ifErrorNotFound;
  if (Count > 1) and (Items[0] is TFpPascalExpressionPartIntrinsic) then
    Result := TFpPascalExpressionPartIntrinsic(Items[0]).FIntrinsic;
end;

{ TFpPascalExpressionPartIntrinsicBase }

function TFpPascalExpressionPartIntrinsicBase.CheckArgumentCount(
  AParams: TFpPascalExpressionPartBracketArgumentList; ARequiredCount: Integer;
  AMaxAccepted: Integer): Boolean;
var
  i: Integer;
begin
  if AMaxAccepted < 0 then
    Result := AParams.Count - 1 = ARequiredCount
  else
    Result := (AParams.Count - 1 >= ARequiredCount) and
              (AParams.Count - 1 <= AMaxAccepted);
  if not Result then begin
    SetError('wrong argument count');
    exit;
  end;

  for i := 1 to ARequiredCount do
    if (AParams.Items[i] = nil) then begin
      Result := False;
      SetError('wrong argument count');
      exit;
    end;
end;

function TFpPascalExpressionPartIntrinsicBase.GetArg(
  AParams: TFpPascalExpressionPartBracketArgumentList; ANum: Integer; out AValue: TFpValue;
  AnErr: String): Boolean;
begin
  AValue := nil;
  Result := ANum < AParams.Count;
  if not Result then begin
    if AnErr <> '' then
      SetError(AnErr);
    exit;
  end;

  AValue := AParams.Items[ANum].ResultValue;
  Result := (AValue <> nil) and (not IsError(ExpressionData.Error)) and (not IsError(AValue.LastError));
  if not Result then begin
    if AnErr <> '' then
      SetError(AnErr);
  end;
end;

function TFpPascalExpressionPartIntrinsicBase.DoGetResultValue: TFpValue;
begin
  SetError('wrong argument count');
  Result := nil;
end;

function TFpPascalExpressionPartIntrinsicBase.DoGetResultValue(
  AParams: TFpPascalExpressionPartBracketArgumentList): TFpValue;
begin
  SetError('internal error');
  Result := nil;
end;

{ TFpPascalExpressionPartBracketSubExpression }

function TFpPascalExpressionPartBracketSubExpression.HandleNextPartInBracket(APart: TFpPascalExpressionPart): TFpPascalExpressionPart;
begin
  Result := Self;
  if Count > 0 then begin
    SetError('To many expressions');
    APart.Free;
    exit;
  end;

  Result := APart;
  Add(APart);
end;

function TFpPascalExpressionPartBracketSubExpression.DoGetResultValue: TFpValue;
begin
  if Count <> 1 then
    Result := nil
  else
    Result := Items[0].ResultValue;
  if Result <> nil then
    Result.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(nil, 'DoGetResultValue'){$ENDIF};
end;

function TFpPascalExpressionPartBracketSubExpression.HandleSeparator(
  ASeparatorType: TSeparatorType; var APart: TFpPascalExpressionPart): Boolean;
begin
  if IsClosed then
    Result := inherited HandleSeparator(ASeparatorType, APart)
  else
    Result := False;
end;

procedure TFpPascalExpressionPartBracketSubExpression.CloseBracket(
  ALastAddedPart: TFpPascalExpressionPart;
  APreviousArraySliceList: PFpPascalExpressionPartOperatorArraySlice; AStartChar: PChar;
  AnEndChar: PChar);
begin
  if (Count <> 1) then
    SetError('Empty brackets')
  else
    inherited CloseBracket(ALastAddedPart, APreviousArraySliceList, AStartChar, AnEndChar);
end;


{ TFpPascalExpressionPartIdentifier }

function TFpPascalExpressionPartIdentifier.DoGetIsTypeCast: Boolean;
begin
  Result := (ResultValue <> nil) and (ResultValue.DbgSymbol <> nil) and (ResultValue.DbgSymbol.SymbolType = stType);
end;

function TFpPascalExpressionPartIdentifier.DoGetResultValue: TFpValue;
var
  s: String;
  tmp: TFpValueConstAddress;
begin
  s := GetText;
  if FOnGetSymbol <> nil then
    Result := FOnGetSymbol(Self, s)
  else
    Result := ExpressionData.GetDbgSymbolForIdentifier(s);
  if Result = nil then begin
    if CompareText(s, 'nil') = 0 then begin
      tmp := TFpValueConstAddress.Create(NilLoc);
      Result := TFpPasParserValueAddressOf.Create(tmp, Self);
      tmp.ReleaseReference;
      {$IFDEF WITH_REFCOUNT_DEBUG}Result.DbgRenameReference(nil, 'DoGetResultValue');{$ENDIF}
    end
    else
    if CompareText(s, 'true') = 0 then begin
      Result := TFpValueConstBool.Create(True);
      {$IFDEF WITH_REFCOUNT_DEBUG}Result.DbgRenameReference(nil, 'DoGetResultValue');{$ENDIF}
    end
    else
    if CompareText(s, 'false') = 0 then begin
      Result := TFpValueConstBool.Create(False);
      {$IFDEF WITH_REFCOUNT_DEBUG}Result.DbgRenameReference(nil, 'DoGetResultValue');{$ENDIF}
    end
    else begin
      SetErrorWithPos(fpErrSymbolNotFound_p, [GetText]);
      exit;
    end;
  end
{$IFDEF WITH_REFCOUNT_DEBUG}
  else
    Result.DbgRenameReference(nil, 'DoGetResultValue')
{$ENDIF}
  ;
end;

procedure TFpPascalExpressionPartIdentifier.Assign(ASourcePart: TFpPascalExpressionPart);
var
  IdentSourcePart: TFpPascalExpressionPartIdentifier absolute ASourcePart;
  p: TFpPascalExpressionPart;
begin
  inherited Assign(ASourcePart);
  if ASourcePart is TFpPascalExpressionPartIdentifier then begin
    if IdentSourcePart.FOnGetSymbol <> nil then begin
      p := FindCopiedInParents(ASourcePart,
           TFpPascalExpressionPart(TMethod(IdentSourcePart.FOnGetSymbol).Data),
           [fipIncludeBracketFunction]);
      if p <> nil then
        FOnGetSymbol := @TFpPascalExpressionPartIntrinsic(p).DoGetMemberForFlattenExpr;
    end;
  end;
end;

procedure TFpPascalExpressionPartIdentifier.ResetEvaluation;
begin
  if (FResultValue <> nil) and (FOnGetSymbol = nil) then
    FResultValue.Reset
  else
    inherited ResetEvaluation;
end;

function GetFirstToken(AText: PChar): String;
begin
  Result := AText[0];
  if AText^ in ['a'..'z', 'A'..'Z', '_', '0'..'9', '$', '&', '%'] then begin
    inc(AText);
    while (AText^ in ['a'..'z', 'A'..'Z', '_', '0'..'9']) and (Length(Result) < 200) do begin
      Result := Result + AText[0];
      inc(AText);
    end;
  end
  else
  begin
    inc(AText);
    while not (AText^ in [#0..#32, 'a'..'z', 'A'..'Z', '_', '0'..'9']) and (Length(Result) < 100) do begin
      Result := Result + AText[0];
      inc(AText);
    end;
  end;
end;

{ TFpPascalExpressionPartCpuRegister }

function TFpPascalExpressionPartCpuRegister.DoGetResultValue: TFpValue;
begin
  Result := ExpressionData.GetRegisterValue(GetText);
  {$IFDEF WITH_REFCOUNT_DEBUG}
  if Result <> nil then
    Result.DbgRenameReference(nil, 'DoGetResultValue')
  {$ENDIF}
end;

{ TFpPascalExpressionPartIntrinsic }

function TFpPascalExpressionPartIntrinsic.DoGetMemberForFlattenExpr(
  APart: TFpPascalExpressionPart; AnIdent: String): TFpValue;
begin
  if FFlattenCurrentVal = nil then
    exit(nil);

  Result := FFlattenCurrentVal.MemberByName[AnIdent];
  if Result = nil then begin
    SetError(fpErrNoMemberWithName, [AnIdent]);
    FFlattenMemberNotFound := True;
    FFlattenMemberName := AnIdent;
  end;
end;

function TFpPascalExpressionPartIntrinsic.DoTry(AParams: TFpPascalExpressionPartBracketArgumentList
  ): TFpValue;
var
  Expr: TFpPascalExpressionPart;
  HighIdx, i: Integer;
  ff: TFpValueFieldFlags;
begin
  Result := nil;
  if IsError(ExpressionData.Error) then
    exit;
  if not CheckArgumentCount(AParams, 2, 999) then
    exit;

  HighIdx := AParams.Count-1;
  for i := 1 to HighIdx-1 do begin
    Expr := AParams.Items[i];
    Result := Expr.GetResultValue;
    if (Result <> nil) and (not IsError(ExpressionData.Error)) then begin
      ff := Result.FieldFlags;
      if ( (not (svfAddress in ff))     or (IsValidLoc(Result.Address))     )
      then begin
        Result.AddReference;
        exit;
      end;
    end;

    Expr.ResetEvaluationRecursive;
    ExpressionData.ClearError;
  end;

  Expr := AParams.Items[HighIdx];
  Result := Expr.GetResultValue;
  Result.AddReference;
end;

function TFpPascalExpressionPartIntrinsic.DoTryN(
  AParams: TFpPascalExpressionPartBracketArgumentList): TFpValue;
var
  Expr: TFpPascalExpressionPart;
  HighIdx, i: Integer;
  ff: TFpValueFieldFlags;
  deref: TFpDbgMemLocation;
begin
  Result := nil;
  if IsError(ExpressionData.Error) then
    exit;
  if not CheckArgumentCount(AParams, 2, 999) then
    exit;

  HighIdx := AParams.Count-1;
  for i := 1 to HighIdx-1 do begin
    Expr := AParams.Items[i];
    Result := Expr.GetResultValue;
    if (Result <> nil) and (not IsError(ExpressionData.Error)) then begin
      ff := Result.FieldFlags;
      deref := Result.DerefAddress;
      if ( (not (svfAddress in ff))     or (IsReadableLoc(Result.Address))     ) and
         ( (not (svfDataAddress in ff)) or (IsReadableLoc(Result.DataAddress)) ) and
         // if deref returned invalid, then its not a pointer
         ( (not IsValidLoc(deref))      or (IsReadableLoc(deref)) )
      then begin
        Result.AddReference;
        exit;
      end;
    end;

    Expr.ResetEvaluationRecursive;
    ExpressionData.ClearError;
  end;

  Expr := AParams.Items[HighIdx];
  Result := Expr.GetResultValue;
  Result.AddReference;
end;

function TFpPascalExpressionPartIntrinsic.DoObj(AParams: TFpPascalExpressionPartBracketArgumentList
  ): TFpValue;
var
  Res: TFpValueConstStruct absolute Result;
  i: Integer;
  p: TFpPascalExpressionPart;
  p_c: TFpPascalExpressionPartOperatorColonAsSeparator absolute p;
  rv: TFpValue;
begin
  Result := TFpValueConstStruct.Create;
  for i := 1 to AParams.Count - 1 do begin
    p := AParams.Items[i];
    if not (p is TFpPascalExpressionPartOperatorColonAsSeparator) then begin
      ReleaseRefAndNil(Result);
      exit;
    end;

    rv := p_c.Items[1].ResultValue;
    if (rv = nil) or IsError(ExpressionData.Error) then begin
      if IsError(ExpressionData.Error) then
        rv := TFpValueConstError.Create(ExpressionData.Error)
      else
        rv := TFpValueConstError.Create(CreateError(fpErrAnyError, ['internal error']));
      ExpressionData.ClearError;

      Res.AddMember(p_c.Items[0].GetText, rv);
      rv.ReleaseReference;
    end
    else
      Res.AddMember(p_c.Items[0].GetText, rv);

    p_c.Items[1].ResetEvaluationRecursive;
  end;
end;

function TFpPascalExpressionPartIntrinsic.DoOrd(AParams: TFpPascalExpressionPartBracketArgumentList
  ): TFpValue;
var
  Arg: TFpValue;
begin
  Result := nil;
  if not CheckArgumentCount(AParams, 1) then
    exit;
  if not GetArg(AParams, 1, Arg, 'argument required') then
    exit;

  if Arg.FieldFlags * [svfOrdinal, svfCardinal] <> [] then
    Result := TFpValueConstNumber.Create(Arg.AsCardinal, False)
  else
  if Arg.FieldFlags * [svfInteger] <> [] then
    Result := TFpValueConstNumber.Create(Arg.AsInteger, True)
  else
    SetError('Can''t get ordinal value of argument');
end;

function TFpPascalExpressionPartIntrinsic.DoLength(
  AParams: TFpPascalExpressionPartBracketArgumentList): TFpValue;
var
  Arg: TFpValue;
  ResLen: Integer;
begin
  Result := nil;
  if not CheckArgumentCount(AParams, 1) then
    exit;

  if not GetArg(AParams, 1, Arg, 'argument required') then
    exit;

  ResLen := 0;
  case Arg.Kind of
    skChar:       ResLen := 1;
    skString,
    skAnsiString,
    skWideString,
    skArray:      ResLen := Arg.MemberCount;
    otherwise begin
      SetError('argument not supported');
      exit;
    end;

  end;

  Result := TFpValueConstNumber.Create(ResLen, True)
end;

function TFpPascalExpressionPartIntrinsic.DoChildClass(
  AParams: TFpPascalExpressionPartBracketArgumentList): TFpValue;
var
  CastName: String;
  NewResult, Arg: TFpValue;
begin
  Result := nil;
  if not CheckArgumentCount(AParams, 1) then
    exit;

  if not GetArg(AParams, 1, Arg, 'argument required') then
    exit;
  Result := Arg;
  Result.AddReference;
  if (Result.Kind <> skClass) or (Result.AsCardinal = 0)
  then
    exit;

  if not Result.GetInstanceClassName(CastName) then
    exit;

  FChildClassCastType.ReleaseReference;
  FChildClassCastType := ExpressionData.GetDbgSymbolForIdentifier(CastName, [fsfIgnoreEnumVals]);
  if (FChildClassCastType = nil) or (FChildClassCastType.DbgSymbol = nil) or
     (FChildClassCastType.DbgSymbol.SymbolType <> stType) or
     (FChildClassCastType.DbgSymbol.Kind <> skClass)
  then begin
    ReleaseRefAndNil(FChildClassCastType);
    exit;
  end;

  // FChildClassCastType.DbgSymbol is part of NewResult, but not refcounted;
  NewResult := FChildClassCastType.GetTypeCastedValue(Result);
  if NewResult <> nil then begin
    Result.ReleaseReference;
    Result := NewResult;
  end;
end;

function TFpPascalExpressionPartIntrinsic.DoFlatten(
  AParams: TFpPascalExpressionPartBracketArgumentList): TFpValue;
var
  Res: TFpValueFlatteArray absolute Result;
  Seen: TAddrSeenList;
  HighParam: integer;
  Flags: TFpPascalExpressionFlattenFlags;
  MaxCnt, ExpandArrayDepth: integer;
  TpSym: TFpSymbol;
  CacheKey: TFpPascalExpressionCacheFlattenKey;
  SkipCache: Boolean;


  function FlattenRecurse(ACurrentVal: TFpValue; ACurDepth: integer; ACurKey: String): boolean; forward;
  function FlattenArray(ACurrentVal: TFpValue; AMapExpr: TFpPascalExpressionPart; ACurDepth, ACurKeyIdx: integer; ACurKey: String;
                        AnExpandDepth: integer): boolean; forward;

  function InternalAdd(ACurrentVal: TFpValue; ACurDepth, ACurKeyIdx: integer; ACurKey: String): Integer;
  var
    TmpVal: TFpValueConstStruct;
    TmpVal2: TFpValue;
  begin
    if ACurrentVal is TFpPasParserValueSlicedArray then
      SkipCache := True;
    if iffObj1 in Flags then begin
      TmpVal := TFpValueConstStruct.Create;
      TmpVal2 := TFpValueConstNumber.Create(ACurDepth, False);
      TmpVal.AddMember('d', TmpVal2);
      TmpVal2.ReleaseReference;

      TmpVal2 := TFpValueConstString.Create(ACurKey);
      TmpVal.AddMember('k', TmpVal2);
      TmpVal2.ReleaseReference;

      TmpVal.AddMember('v', ACurrentVal);
      Result := Res.FList.Add(TmpVal);
      TmpVal.ReleaseReference;
    end
    else
    if iffObj2 in Flags then begin
      TmpVal := TFpValueConstStruct.Create;
      TmpVal2 := TFpValueConstNumber.Create(ACurDepth, False);
      TmpVal.AddMember('d', TmpVal2);
      TmpVal2.ReleaseReference;

      TmpVal2 := TFpValueConstNumber.Create(QWord(ACurKeyIdx), True);
      TmpVal.AddMember('k', TmpVal2);
      TmpVal2.ReleaseReference;

      TmpVal.AddMember('v', ACurrentVal);
      Result := Res.FList.Add(TmpVal);
      TmpVal.ReleaseReference;
    end
    else
    if Flags * [iffObj3, iffObj4] <> [] then begin
      TmpVal := TFpValueConstStruct.Create;
      TmpVal2 := TFpValueConstString.Create(ACurKey);
      TmpVal.AddMember('k', TmpVal2);
      TmpVal2.ReleaseReference;

      TmpVal.AddMember('v', ACurrentVal);
      Result := Res.FList.Add(TmpVal);
      TmpVal.ReleaseReference;
    end
    else
      Result := Res.FList.Add(ACurrentVal);
  end;

  procedure AddErrToList(AnErr: TFpError; ACurDepth, ACurKeyIdx: integer; ACurKey: String);
  var
    E: TFpValueConstError;
  begin
    E := TFpValueConstError.Create(AnErr);
    InternalAdd(E, ACurDepth, ACurKeyIdx, ACurKey);
    E.ReleaseReference
  end;

  function EvalExression(AnExpr: TFpPascalExpressionPart; ACurrentVal, AnOrigVal: TFpValue;
    ShowMemberNotFoundErr: boolean; ACurDepth, ACurKeyIdx: integer; ACurKey: String): TFpValue;
  var
    s: String;
    Err: TFpError;
  begin
    Result := nil;
    Err := NoError;
    if AnExpr is TFpPascalExpressionPartIdentifier then begin
      FFlattenMemberNotFound := not(ACurrentVal.Kind in [skClass, skInterface, skRecord, skObject]);
      if not FFlattenMemberNotFound then begin
        FFlattenMemberName := AnExpr.GetText;
        Result := ACurrentVal.MemberByName[FFlattenMemberName];
        FFlattenMemberNotFound := (Result = nil) and ShowMemberNotFoundErr;
      end;
    end
    else begin
      FFlattenCurrentVal := ACurrentVal;
      FFlattenCurrentValOrig := AnOrigVal; // for :_
      FFlattenMemberNotFound := False;
      FFlattenMemberName := '';
      AnExpr.ResetEvaluationRecursive;
      Result := AnExpr.GetResultValue;
      if Result <> nil then Result.AddReference;

      if not ShowMemberNotFoundErr then
        FFlattenMemberNotFound := False; // show general error instead

      Err := ExpressionData.Error;
      //AnExpr.ResetEvaluationRecursive;
      ExpressionData.ClearError;

      if (not FFlattenMemberNotFound) and IsError(Err) then begin
        if (iffShowErrAny in Flags) then
          AddErrToList(CreateError(fpErrAnyError, ['Failed eval for member: ' + FFlattenMemberName + ' '+ErrorHandler.ErrorAsString(Err)]), ACurDepth, ACurKeyIdx, ACurKey);
        ReleaseRefAndNil(Result);
        exit
      end;

      if (not FFlattenMemberNotFound) and AnExpr.ReturnsVariant then
        Res.Flags := Res.Flags + [vfArrayOfVariant];
    end;

    if FFlattenMemberNotFound then begin
      if (iffShowNoMember in Flags) then
        AddErrToList(CreateError(fpErrAnyError, ['Member not found: ' + FFlattenMemberName]), ACurDepth, ACurKeyIdx, ACurKey);
      ReleaseRefAndNil(Result);
      exit;
    end;

    if Result = nil then begin
      if (iffShowErrAny in Flags) then begin
        s := '';
        if IsError(Err) then
          s := ErrorHandler.ErrorAsString(Err);
        AddErrToList(CreateError(fpErrAnyError, ['Internal error for member: ' + FFlattenMemberName + ' '+s]), ACurDepth, ACurKeyIdx, ACurKey);
      end;
      exit;
    end;
  end;

  function AddFlatValue(ACurrentVal: TFpValue; AMapExpr: TFpPascalExpressionPart; ACurDepth, ACurKeyIdx: integer; ACurKey: String; AnExpandDepth: integer): boolean;
  var
    s, ResIdx, SeenIdx, ValIdx: Integer;
    PrevVal, TmpAutoDereVal, DisplayVal, OrigVal: TFpValue;
    DA: TFpDbgMemLocation;
    DoExpArray, HasDtAddr: Boolean;
  begin
    Result := True;
    if ACurrentVal = nil then begin
      if (iffShowErrAny in Flags) then
        AddErrToList(CreateError(fpErrAnyError, ['Internal error for member: ' + FFlattenMemberName + ' '+ErrorHandler.ErrorAsString(ExpressionData.Error)]), ACurDepth, ACurKeyIdx, ACurKey);
      exit;
    end;

    OrigVal := ACurrentVal;
    OrigVal.AddReference;
    DisplayVal := nil;
    ResIdx := -1;
    try
      if (iffDerefPtr in Flags) and (ACurrentVal.Kind = skPointer) and
        (ACurrentVal.TypeInfo <> nil) and (ACurrentVal.TypeInfo.TypeInfo <> nil) and
        (ACurrentVal.TypeInfo.TypeInfo.Kind in [skClass, skInterface, skRecord, skObject])
      then begin
        if (svfDataAddress in ACurrentVal.FieldFlags) and (IsReadableLoc(ACurrentVal.DerefAddress)) and // TODO, what if Not readable addr
           (ACurrentVal.TypeInfo <> nil) //and (ACurrentVal.TypeInfo.TypeInfo <> nil)
        then begin
          TmpAutoDereVal := ACurrentVal.Member[0];
          if TmpAutoDereVal <> nil then begin
            ACurrentVal.ReleaseReference;
            ACurrentVal := TmpAutoDereVal;
          end;
        end;
      end;

      DoExpArray := (AnExpandDepth > 0) and (ACurrentVal.Kind = skArray);

      HasDtAddr := (svfDataAddress in ACurrentVal.FieldFlags);
      DA := ACurrentVal.DataAddress;
      if IsReadableLoc(DA) then begin
        SeenIdx := Seen.IndexOf(DA);
        if (SeenIdx >= 0) then begin
          ValIdx := Seen.Data[SeenIdx];
          if (not DoExpArray) and (not (ACurrentVal.Kind in [skClass, skInterface])) then begin
            PrevVal := TFpValue(Res.FList[ValIdx]);
            if (ACurrentVal.TypeInfo = nil) or (PrevVal.TypeInfo = nil) or
               (not ACurrentVal.TypeInfo.IsEqual(PrevVal.TypeInfo))
            then
              SeenIdx := -1;
          end;
          if (SeenIdx >= 0) then begin
            if (iffShowRecurse in Flags) and (ValIdx >= 0) then begin
              if DoExpArray then
                AddErrToList(CreateError(fpErrAnyError, [Format('Recursion detected for array at member: %s (At Index %d)', [FFlattenMemberName, ValIdx])]), ACurDepth, ACurKeyIdx, ACurKey)
              else
                AddErrToList(CreateError(fpErrAnyError, [Format('Recursion detected for member: %s (At Index %d)', [FFlattenMemberName, ValIdx])]), ACurDepth, ACurKeyIdx, ACurKey);
            end
            else
            if (iffShowSeen in Flags) then begin
              if ValIdx < 0 then ValIdx := -(ValIdx + 1);
              if DoExpArray then
                AddErrToList(CreateError(fpErrAnyError, [Format('Array for member already shown: %s (At Index %d)', [FFlattenMemberName, ValIdx])]), ACurDepth, ACurKeyIdx, ACurKey)
              else
                AddErrToList(CreateError(fpErrAnyError, [Format('Member already shown: %s (At Index %d)', [FFlattenMemberName, ValIdx])]), ACurDepth, ACurKeyIdx, ACurKey);
            end;
            ReleaseRefAndNil(ACurrentVal);
            exit;
          end;
        end;
      end;

      if ( (not DoExpArray) or
           (HasDtAddr and not IsReadableLoc(DA))
         ) and
         ( (iffShowNil in Flags) or (not IsNilLoc(DA))
         )
      then begin
        // not an array, or array can not be expanded => show value
        if (AMapExpr = nil) then begin
          DisplayVal := ACurrentVal;
          DisplayVal.AddReference;
        end
        else
          DisplayVal := EvalExression(AMapExpr, ACurrentVal, OrigVal, False, ACurDepth, ACurKeyIdx, ACurKey);

        if (DisplayVal <> nil) then begin
          ResIdx := InternalAdd(DisplayVal, ACurDepth, ACurKeyIdx, ACurKey);
          if (DisplayVal.TypeInfo = nil) or (not DisplayVal.TypeInfo.IsEqual(TpSym)) or
             (DisplayVal.Flags * [vfArrayOfVariant, vfVariant] <> [])
          then
            Res.Flags := Res.Flags + [vfArrayOfVariant];
        end;
      end;

      if IsNilLoc(DA) or
         ( (not IsReadableLoc(DA)) and ((not DoExpArray) or HasDtAddr) )
      then begin
        ReleaseRefAndNil(ACurrentVal);
        exit;
      end;

      if DoExpArray then begin
        Result := FlattenArray(ACurrentVal, AMapExpr, ACurDepth + 1, ACurKeyIdx, ACurKey, AnExpandDepth);
      end
      else begin
        s := Seen.Add(DA, ResIdx);
        Result := FlattenRecurse(ACurrentVal, ACurDepth+1, ACurKey);
        if (iffShowSeen in Flags) then
          Seen.Data[s] := -1-ResIdx
        else
          Seen.Delete(s);
      end;
      ReleaseRefAndNil(ACurrentVal);

    finally
      DisplayVal.ReleaseReference;
      OrigVal.ReleaseReference;
    end;
  end;

  function FlattenArray(ACurrentVal: TFpValue; AMapExpr: TFpPascalExpressionPart; ACurDepth, ACurKeyIdx: integer; ACurKey: String;
    AnExpandDepth: integer): boolean;
  var
    Idx, Cnt: Integer;
    TmpNew: TFpValue;
    LBnd: Int64;
  begin
    //       Seen.Add(DA, -2 - FList.Count); // array seen
    LBnd := ACurrentVal.OrdLowBound;
    Cnt := ACurrentVal.MemberCount;
    if Cnt > 1 then begin
// LOCK the ExpressionData
    end;
    for Idx := 0 to Cnt - 1 do begin
      if Res.FList.Count >= MaxCnt then
        exit(False);
      TmpNew := ACurrentVal.Member[Idx+LBnd];
      // AddFlatValue will release TmpNew
      Result := AddFlatValue(TmpNew, AMapExpr, ACurDepth, ACurKeyIdx, ACurKey+'['+IntToStr(Idx+LBnd)+']', Max(0, AnExpandDepth-1));
      if not Result then
        exit;
    end;
// UNLOCK
    Result := True;
  end;

  function FlattenRecurse(ACurrentVal: TFpValue; ACurDepth: integer; ACurKey: String): boolean;
  var
    i: Integer;
    OrigVal, AutoDereVal, TmpNew: TFpValue;
    Expr, MapExpr, TmpExpr: TFpPascalExpressionPart;
    Expr_as_ColSep: TFpPascalExpressionPartOperatorColonAsSeparator absolute Expr;
    r: Boolean;
    NxtKey: String;
  begin
    Result := True;
    if HighParam < 2 then
      exit;

    AutoDereVal := nil;
    OrigVal := ACurrentVal;
    OrigVal.AddReference;
    try
      if ExpressionData.AutoDeref and (ACurrentVal.Kind = skPointer) and
        (ACurrentVal.TypeInfo <> nil) and (ACurrentVal.TypeInfo.TypeInfo <> nil) and
        (ACurrentVal.TypeInfo.TypeInfo.Kind in [skClass, skInterface, skRecord, skObject])
      then begin
        if (svfDataAddress in ACurrentVal.FieldFlags) and (IsReadableLoc(ACurrentVal.DerefAddress)) and // TODO, what if Not readable addr
           (ACurrentVal.TypeInfo <> nil) //and (ACurrentVal.TypeInfo.TypeInfo <> nil)
        then begin
          AutoDereVal := ACurrentVal.Member[0];
          ACurrentVal := AutoDereVal;
        end;
        if (ACurrentVal = nil) then begin
          //if (iffShowErrAny in Flags) then
          //  AddErrToList(CreateError(fpErrAnyError, ['Can't flatten nil pointer']), ACurDepth, -1, ACurKey);
          exit;
        end;
      end;


      case ACurrentVal.Kind of
        skClass, skInterface, skRecord, skObject: begin
          for i := 2 to HighParam do begin
            if Res.FList.Count >= MaxCnt then
              exit(False);

            Expr := AParams.Items[i];
            MapExpr := nil;
            if Expr is TFpPascalExpressionPartOperatorColonAsSeparator then begin
              if Expr_as_ColSep.Count <> 2 then begin
                SetError('Internal erorr');
                exit(false);
              end;
              MapExpr := Expr_as_ColSep.Items[1];
              Expr    := Expr_as_ColSep.Items[0];

              if MapExpr is TFpPascalExpressionPartConstantNumber then begin
                TmpNew := MapExpr.ResultValue;
                if (TmpNew is TFpValueConstNumber) then begin
                  TmpExpr := AParams.Items[1+TmpNew.AsInteger];
                  if (not (TmpExpr is TFpPascalExpressionPartOperatorColonAsSeparator)) or
                     (TFpPascalExpressionPartOperatorColonAsSeparator(TmpExpr).Count <> 2)
                  then begin
                    SetError('Incorrect reference to map-expression');
                    exit(false);
                  end;
                  MapExpr := TFpPascalExpressionPartOperatorColonAsSeparator(TmpExpr).Items[1];
                  if (MapExpr is TFpPascalExpressionPartConstantNumber) and
                     (MapExpr.ResultValue is TFpValueConstNumber)
                  then begin
                    SetError('Incorrect reference to map-expression');
                    exit(false);
                  end;
                end;
              end;
            end;

            if (iffObj4 in Flags) and (Length(ACurKey) < 1000) then begin
              if (ACurKey = '') then
                NxtKey := IntToStr(i-2)
              else
                NxtKey := ACurKey + '.' + IntToStr(i-2);
            end
            else begin
              NxtKey := Expr.GetFullText;
              if (iffObj3 in Flags) and (ACurKey <> '') and (Length(ACurKey) < 1000) then
                NxtKey := ACurKey + '.' + NxtKey
            end;

            Expr.BeginNeedCopy;

            TmpNew := EvalExression(Expr, ACurrentVal, OrigVal, True, ACurDepth, i-2, NxtKey);
            if TmpNew = nil then begin
              Expr.EndNeedCopy;
              Continue;
            end;

            // AddFlatValue will release TmpNew
            r := AddFlatValue(TmpNew, MapExpr, ACurDepth, i-2, NxtKey, ExpandArrayDepth);

            Expr.EndNeedCopy;

            if not r then
              exit(False);
          end;

        end;
        //skArray: begin end;
        else begin
          //if (iffShowErrAny in Flags) then
          //  AddErrToList(CreateError(fpErrAnyError, ['Can''t flatten value']), ACurDepth, -1, ACurKey);
        end;
      end;
    finally
      OrigVal.ReleaseReference;
      AutoDereVal.ReleaseReference;
    end;
    Result := True;
  end;

var
  FirstVal: TFpValue;
  DA: TFpDbgMemLocation;
  LastParam, Itm: TFpPascalExpressionPart;
  OptSet: TFpPascalExpressionPartBracketSet absolute LastParam;
  i: Integer;
  OName: String;
  OVal, CustomMaxCnt, LastParamNeg: Boolean;
  PParent: TFpPascalExpressionPartContainer;
  ListCache: TFpPascalExpressionCacheFlatten;
begin
  Result := nil;
  if not CheckArgumentCount(AParams, 1, 999) then
    exit;

  if not GetArg(AParams, 1, FirstVal, 'Value required')  then exit;

  if (FirstVal.Kind <> skArray) and (not CheckArgumentCount(AParams, 2, 999)) then
    exit;

  Flags := [iffShowNil, iffShowNoMember, iffShowRecurse, iffShowSeen, iffShowErrAny, iffDerefPtr];
  ExpandArrayDepth  := 0;
  MaxCnt       := 1000;
  CustomMaxCnt := False;

  HighParam := AParams.Count - 1;
  if HighParam > 0 then begin
    LastParam := AParams.Items[HighParam];

    LastParamNeg := False;
    if (LastParam is TFpPascalExpressionPartOperatorUnaryPlusMinus) and
       (TFpPascalExpressionPartOperatorUnaryPlusMinus(LastParam).Count = 1) and
       (TFpPascalExpressionPartOperatorUnaryPlusMinus(LastParam).Items[0] is TFpPascalExpressionPartBracketSet)
    then begin
      // Add or Sub from defaults
      LastParamNeg := LastParam.GetText = '-';
      LastParam := TFpPascalExpressionPartOperatorUnaryPlusMinus(LastParam).Items[0];
    end
    else
    if LastParam is TFpPascalExpressionPartBracketSet then
      Flags := [];  // NO +/- => Start with empty,

    if LastParam is TFpPascalExpressionPartBracketSet then begin
      if (FirstVal.Kind <> skArray) and (not CheckArgumentCount(AParams, 3, 999)) then
        exit;

      dec(HighParam);
      if HighParam < 2 then begin
        SetError('Not enough parameter');
        exit;
      end;
      if (HighParam > 2) and (Flags <> []) then
        Flags := Flags + [iffObj1];

      for i := 0 to OptSet.Count - 1 do begin
        Itm := OptSet.Items[i];
        OName := '';
        OVal := True;
        if (Itm is TFpPascalExpressionPartIdentifier) then
          OName := Itm.GetText
        else
        if (Itm is TFpPascalExpressionPartOperatorCompare) and (Itm.GetText = '=') and
           (TFpPascalExpressionPartOperatorCompare(Itm).Count = 2) and
           (TFpPascalExpressionPartOperatorCompare(Itm).Items[1].ResultValue <> nil)
        then begin
          OName := TFpPascalExpressionPartOperatorCompare(Itm).Items[0].GetText;
          if LowerCase(OName)= 'max' then begin
            MaxCnt := TFpPascalExpressionPartOperatorCompare(Itm).Items[1].ResultValue.AsInteger;
            CustomMaxCnt := True;
            Continue;
          end;
          if LowerCase(OName)= 'array' then begin
            ExpandArrayDepth := TFpPascalExpressionPartOperatorCompare(Itm).Items[1].ResultValue.AsInteger;
            Continue;
          end;
          OVal := TFpPascalExpressionPartOperatorCompare(Itm).Items[1].ResultValue.AsBool;
        end;

        OVal := OVal xor LastParamNeg;
        case LowerCase(OName) of
          'nil':             if OVal then include(Flags, iffShowNil)      else exclude(Flags, iffShowNil);
          'field', 'fld':    if OVal then include(Flags, iffShowNoMember) else exclude(Flags, iffShowNoMember);
          'loop', 'recurse': if OVal then include(Flags, iffShowRecurse)  else exclude(Flags, iffShowRecurse);
          'seen', 'dup':     if OVal then include(Flags, iffShowSeen)     else exclude(Flags, iffShowSeen);
          'err', 'error':    if OVal then include(Flags, iffShowErrAny)   else exclude(Flags, iffShowErrAny);
          'array':           ExpandArrayDepth  := 1;
          'ptr', 'deref':    if OVal then include(Flags, iffDerefPtr)     else exclude(Flags, iffDerefPtr);
          'obj':           begin
                             Flags := Flags - [iffObj1, iffObj2, iffObj3, iffObj4];
                             if OVal then include(Flags, iffObj1);
                           end;
          'o1', 'obj1':    begin
                             if Oval then Flags := Flags - [iffObj1, iffObj2, iffObj3, iffObj4];
                             if OVal then include(Flags, iffObj1)         else exclude(Flags, iffObj1);
                           end;
          'o2', 'obj2':    begin
                             if Oval then Flags := Flags - [iffObj1, iffObj2, iffObj3, iffObj4];
                             if OVal then include(Flags, iffObj2)         else exclude(Flags, iffObj2);
                           end;
          'o3', 'obj3':    begin
                             if Oval then Flags := Flags - [iffObj1, iffObj2, iffObj3, iffObj4];
                             if OVal then include(Flags, iffObj3)         else exclude(Flags, iffObj3);
                           end;
          'o4', 'obj4':    begin
                             if Oval then Flags := Flags - [iffObj1, iffObj2, iffObj3, iffObj4];
                             if OVal then include(Flags, iffObj4)         else exclude(Flags, iffObj4);
                           end;
          else begin
            SetError('Unknown flag: '+Itm.GetText);
            exit;
          end;
        end;
      end;
    end
    else
    if HighParam > 2 then
      Flags := Flags + [iffObj1];
  end;

  ListCache := nil;
  SkipCache := False;
  if (ExpressionData.GlobalCache <> nil) then begin
    Itm := TopParent;
    while (not SkipCache) and (Itm is TFpPascalExpressionPartOperatorArraySliceController)
    do begin
      SkipCache := TFpPascalExpressionPartOperatorArraySliceController(Itm).SlicePart.FindInParents(Self.Parent);
      Itm := TFpPascalExpressionPartContainer(Itm).Items[0];
    end;
  end;

  if (not SkipCache) and (ExpressionData.GlobalCache <> nil) then begin
    CacheKey.CtxThread := ExpressionData.Scope.LocationContext.ThreadId;
    CacheKey.CtxStack := ExpressionData.Scope.LocationContext.StackFrame;
    CacheKey.Flags := Flags;
    CacheKey.ExpandArrayDepth := ExpandArrayDepth;
    CacheKey.Key := Parent.GetFullText;
    i := ExpressionData.GlobalCache.IndexOf(Pointer(TFpPascalExpressionPartIntrinsic));
    if i >= 0 then begin
      ListCache := TFpPascalExpressionCacheFlatten(ExpressionData.GlobalCache.Data[i]);
      i := ListCache.IndexOf(CacheKey);
      if i >= 0 then begin
        Result := ListCache.Data[i];
        if Res.FFullEvaluated then begin
          Res.AddReference;
          exit;
        end;
      end;
    end;
  end;

  // check the maximum needed
  PParent := Parent.Parent;
  if (PParent is TFpPascalExpressionPartBracketIndex) and (PParent.Count = 2) then begin
    Itm := PParent.Items[1];
    if (Itm is TFpPascalExpressionPartOperatorArraySlice) then begin
      if TFpPascalExpressionPartOperatorArraySlice(Itm).Count = 2 then
        Itm := TFpPascalExpressionPartOperatorArraySlice(Itm).Items[1]
      else
        Itm := nil;
    end;

    if (Itm <> nil) and (Itm.ResultValue <> nil) then
      if CustomMaxCnt
      then MaxCnt := Min(MaxCnt, Itm.ResultValue.AsInteger+101) // Cache 100 extra
      else MaxCnt := Itm.ResultValue.AsInteger+101;
  end;

  if (Result <> nil) and (Res.FList.Count >= MaxCnt) then begin// cached
    Res.AddReference;
    exit;
  end;

  Result := TFpValueFlatteArray.Create(0);
  Seen := TAddrSeenList.Create;
  Seen.Capacity := 256;
  TpSym := FirstVal.TypeInfo;
  try
    if (FirstVal.Kind = skArray) then begin
      FlattenArray(FirstVal, nil, 0, -1, '', Max(1, ExpandArrayDepth));
    end
    else begin
      DA := FirstVal.DataAddress;
      Seen.Add(DA, 0);
      InternalAdd(FirstVal, 0, -1, '');

      if not IsReadableLoc(DA) then
        exit;
      if IsError(ExpressionData.Error) then
        exit;

      Res.FFullEvaluated := FlattenRecurse(FirstVal, 0, '');
    end;
  finally
    Seen.Free;
  end;

  if (not SkipCache) and (ExpressionData.GlobalCache <> nil) then begin
    if ListCache = nil then begin
      ListCache := TFpPascalExpressionCacheFlatten.Create;
      ExpressionData.GlobalCache[Pointer(TFpPascalExpressionPartIntrinsic)] := ListCache;
    end;
    ListCache.Replace(CacheKey, Res);
  end;
end;

function TFpPascalExpressionPartIntrinsic.DoFlattenPlaceholder(
  AParams: TFpPascalExpressionPartBracketArgumentList): TFpValue;
var
  f: TFpPascalExpressionPartContainer;
begin
  Result := nil;
  f := Parent;
  while (f <> nil) and
        not( (f is TFpPascalExpressionPartBracketArgumentList) and
             (f.Count > 1) and (f.Items[0] is TFpPascalExpressionPartIntrinsic) and
             (TFpPascalExpressionPartIntrinsic(f.Items[0]).FIntrinsic = ifFlatten)
           )
  do
    f := f.Parent;
  if f = nil then begin
    SetError(':_ outside of :flatten');
    exit;
  end;

  if not CheckArgumentCount(AParams, 0) then
    exit;

  Result := TFpPascalExpressionPartIntrinsic(f.Items[0]).FFlattenCurrentValOrig;
  if Result <> nil then
    Result.AddReference;
end;

function TFpPascalExpressionPartIntrinsic.DoRefCnt(
  AParams: TFpPascalExpressionPartBracketArgumentList): TFpValue;
var
  Tmp: TFpValue;
  rcnt: Int64;
begin
  Result := nil;
  if not CheckArgumentCount(AParams, 1) then
    exit;

  if not GetArg(AParams, 1, Tmp, 'argument required') then
    exit;

  if not Tmp.GetFpcRefCount(rcnt) then begin
    SetError('argument not supported');
    exit;
  end;

  Result := TFpValueConstNumber.Create(QWord(rcnt), True)
end;

function TFpPascalExpressionPartIntrinsic.DoPos(
  AParams: TFpPascalExpressionPartBracketArgumentList): TFpValue;
var
  Tmp, Tmp2, CmpCase: TFpValue;
  s1, s2: String;
begin
  Result := nil;
  if not CheckArgumentCount(AParams, 2, 3) then
    exit;

  if not GetArg(AParams, 1, Tmp, 'argument required')  then exit;
  if not GetArg(AParams, 2, Tmp2, 'argument required') then exit;

  CmpCase := nil;
  if AParams.Count = 4 then begin
    if not GetArg(AParams, 3, CmpCase, 'argument required') then
      exit;
    if (CmpCase.Kind <> skBoolean) then begin
      SetError('bool argument expected');
      exit;
    end;
  end;

  s1 := Tmp.AsString;
  s2 := Tmp2.AsString;

  if (CmpCase <> nil) and (CmpCase.AsBool) then begin
    s1 := AnsiLowerCase(s1);
    s2 := AnsiLowerCase(s2);
  end;

  if (s1 = '') or (s2 = '') then
    Result := TFpValueConstNumber.Create(0, True)
  else
    Result := TFpValueConstNumber.Create(pos(s1, s2), True);
end;

function TFpPascalExpressionPartIntrinsic.DoSubStr(
  AParams: TFpPascalExpressionPartBracketArgumentList): TFpValue;
var
  Tmp, Tmp2, Tmp3, Tmp4: TFpValue;
  s1: String;
  w1: WideString;
  p1, p2: Int64;
  UsePtr: Boolean;
  Addr: QWord;
  t: TFpSymbol;
  Size: TFpDbgValueSize;
  ctx: TFpDbgLocationContext;
begin
  Result := nil;
  if not CheckArgumentCount(AParams, 3,4) then
    exit;

  if not GetArg(AParams, 1, Tmp, 'argument required')  then exit;
  if not GetArg(AParams, 2, Tmp2, 'argument required') then exit;
  if not GetArg(AParams, 3, Tmp3, 'argument required') then exit;

  UsePtr := False;
  if AParams.Count = 5 then begin
    if not GetArg(AParams, 4, Tmp4, 'argument required') then
      exit;
    if (Tmp4.Kind <> skBoolean) then begin
      SetError('bool argument expected');
      exit;
    end;
    UsePtr := Tmp4.AsBool;
  end;

  if not (Tmp.Kind in [skString, skAnsiString, skWideString]) then
    UsePtr := True;

  if svfInteger in Tmp2.FieldFlags then
    p1 := Tmp2.AsInteger
  else
  if svfCardinal in Tmp2.FieldFlags then
    {$PUSH}{$R-}{$Q-}
    p1 := Int64(Tmp2.AsCardinal)
    {$POP}
  else begin
    SetError('int argument expected');
    exit;
  end;
  if (p1 < 1) and (not UsePtr) then begin
    SetError('argument >= 1 expected');
    exit;
  end;

  if svfInteger in Tmp3.FieldFlags then
    p2 := Tmp3.AsInteger
  else
  if svfCardinal in Tmp3.FieldFlags then
    {$PUSH}{$R-}{$Q-}
    p2 := Int64(Tmp3.AsCardinal)
    {$POP}
  else begin
    SetError('int argument expected');
    exit;
  end;
  if (p2 < 1) and (not UsePtr) then begin
    SetError('argument >= 1 expected');
    exit;
  end;

  if UsePtr then begin
    if not (Tmp.Kind in [skPointer, skString, skAnsiString, skWideString, skAddress]) then begin
      SetError('argument 1 not supported');
    end;
    Addr := Tmp.AsCardinal;
    if Addr = 0 then begin
      Result := TFpValueConstString.Create('');
      exit;
    end;

    if Tmp.Kind = skPointer then begin
      Size := SizeVal(1);
      t := Tmp.TypeInfo;
      if t <> nil then
        t := t.TypeInfo;
      if (t = nil) or      // Only test for hardcoded size. TODO: dwarf 3 could have variable size, but for char that is not expected
         not t.ReadSize(nil, Size)
      then
        Size := SizeVal(1);
    end;

    ctx := ExpressionData.Scope.LocationContext;
    {$PUSH}{$R-}{$Q-}
    if (Tmp.Kind = skWideString) or
       ( (Tmp.Kind = skPointer) and (Size.Size = 2))
    then begin
      if not ( (ctx.MemManager.SetLength(w1, p2)) and
               (ctx.ReadMemory(TargetLoc(Addr+QWord(p1*2)), SizeVal(p2*2), @w1[1])) )
      then begin
        SetError(ctx.LastMemError);
        s1 := '';
      end
      else
        s1 := w1;
    end
    else begin
      if not ( (ctx.MemManager.SetLength(s1, p2)) and
               (ctx.ReadMemory(TargetLoc(Addr+QWord(p1)), SizeVal(p2), @s1[1])) )
      then begin
        SetError(ctx.LastMemError);
        s1 := '';
      end;
    end;
    {$POP}
    Result := TFpValueConstString.Create(s1);
  end
  else
  begin
    if not Tmp.GetSubString(p1, p2, s1) and (s1 = '') then
      SetError(Tmp.LastError);
    Result := TFpValueConstString.Create(s1);
  end;

end;

function TFpPascalExpressionPartIntrinsic.DoLower(
  AParams: TFpPascalExpressionPartBracketArgumentList): TFpValue;
var
  Arg: TFpValue;
begin
  Result := nil;
  if not CheckArgumentCount(AParams, 1) then
    exit;
  if not GetArg(AParams, 1, Arg, 'argument required') then
    exit;

  Result := TFpValueConstString.Create(AnsiLowerCase(Arg.AsString));
end;

function TFpPascalExpressionPartIntrinsic.DoUpper(
  AParams: TFpPascalExpressionPartBracketArgumentList): TFpValue;
var
  Arg: TFpValue;
begin
  Result := nil;
  if not CheckArgumentCount(AParams, 1) then
    exit;
  if not GetArg(AParams, 1, Arg, 'argument required') then
    exit;

  Result := TFpValueConstString.Create(AnsiUpperCase(Arg.AsString));
end;

function TFpPascalExpressionPartIntrinsic.DoRound(
  AParams: TFpPascalExpressionPartBracketArgumentList): TFpValue;
var
  Arg, Digits: TFpValue;
begin
  Result := nil;
  if not CheckArgumentCount(AParams, 1, 2) then
    exit;
  if not GetArg(AParams, 1, Arg, 'argument required') then
    exit;

  Digits := nil;
  if AParams.Count = 3 then begin
    if not GetArg(AParams, 2, Digits, 'optional') then
      exit;
    if not (Digits.Kind in [skCardinal, skInteger]) then begin
      SetError('int argument expected');
      exit;
    end;
  end;

  if (Digits = nil) or (Digits.AsInteger = 0) then
    Result := TFpValueConstNumber.Create(QWord(Round(Arg.AsFloat)), True)
  else
    Result := TFpValueConstFloat.Create(RoundTo(Arg.AsFloat, Digits.AsInteger));
end;

function TFpPascalExpressionPartIntrinsic.DoTrunc(
  AParams: TFpPascalExpressionPartBracketArgumentList): TFpValue;
var
  Arg: TFpValue;
begin
  Result := nil;
  if not CheckArgumentCount(AParams, 1) then
    exit;
  if not GetArg(AParams, 1, Arg, 'argument required') then
    exit;

  Result := TFpValueConstNumber.Create(QWord(trunc(Arg.AsFloat)), True);
end;

function TFpPascalExpressionPartIntrinsic.DoSqrt(
  AParams: TFpPascalExpressionPartBracketArgumentList): TFpValue;
var
  Arg: TFpValue;
begin
  Result := nil;
  if not CheckArgumentCount(AParams, 1) then
    exit;
  if not GetArg(AParams, 1, Arg, 'argument required') then
    exit;

  if svfFloat in Arg.FieldFlags then
    Result := TFpValueConstFloat.Create(Sqrt(Arg.AsFloat))
  else
  if svfInteger in Arg.FieldFlags then
    Result := TFpValueConstFloat.Create(Sqrt(Arg.AsInteger))
  else
  if svfCardinal in Arg.FieldFlags then
    Result := TFpValueConstFloat.Create(Sqrt(Arg.AsInteger))
  else
    SetError('Argument not numeric');
end;

function TFpPascalExpressionPartIntrinsic.DoPi(AParams: TFpPascalExpressionPartBracketArgumentList
  ): TFpValue;
begin
  if not CheckArgumentCount(AParams, 0) then
    exit;

  Result := TFpValueConstFloat.Create(Pi)
end;

function TFpPascalExpressionPartIntrinsic.DoLn(AParams: TFpPascalExpressionPartBracketArgumentList
  ): TFpValue;
var
  Arg: TFpValue;
begin
  Result := nil;
  if not CheckArgumentCount(AParams, 1) then
    exit;
  if not GetArg(AParams, 1, Arg, 'argument required') then
    exit;

  if svfFloat in Arg.FieldFlags then
    Result := TFpValueConstFloat.Create(Ln(Arg.AsFloat))
  else
  if svfInteger in Arg.FieldFlags then
    Result := TFpValueConstFloat.Create(Ln(Arg.AsInteger))
  else
  if svfCardinal in Arg.FieldFlags then
    Result := TFpValueConstFloat.Create(Ln(Arg.AsInteger))
  else
    SetError('Argument not numeric');
end;

function TFpPascalExpressionPartIntrinsic.DoLog(AParams: TFpPascalExpressionPartBracketArgumentList
  ): TFpValue;
var
  Arg: TFpValue;
  n: Extended;
begin
  Result := nil;
  if not CheckArgumentCount(AParams, 2) then
    exit;
  if not GetArg(AParams, 1, Arg, 'argument required') then
    exit;

  if svfFloat in Arg.FieldFlags then
    n := Arg.AsFloat
  else
  if Arg.FieldFlags * [svfInteger, svfCardinal] <> [] then
    n := Arg.AsCardinal
  else
    SetError('Argument not numeric');

  if not GetArg(AParams, 2, Arg, 'argument required') then
    exit;

  if svfFloat in Arg.FieldFlags then
    Result := TFpValueConstFloat.Create(LogN(n,Arg.AsFloat))
  else
  if svfInteger in Arg.FieldFlags then
    Result := TFpValueConstFloat.Create(LogN(n,Arg.AsInteger))
  else
  if svfCardinal in Arg.FieldFlags then
    Result := TFpValueConstFloat.Create(LogN(n,Arg.AsInteger))
  else
    SetError('Argument not numeric');
end;

function TFpPascalExpressionPartIntrinsic.DoSin(AParams: TFpPascalExpressionPartBracketArgumentList
  ): TFpValue;
var
  Arg: TFpValue;
begin
  Result := nil;
  if not CheckArgumentCount(AParams, 1) then
    exit;
  if not GetArg(AParams, 1, Arg, 'argument required') then
    exit;

  if svfFloat in Arg.FieldFlags then
    Result := TFpValueConstFloat.Create(Sin(Arg.AsFloat))
  else
  if svfInteger in Arg.FieldFlags then
    Result := TFpValueConstFloat.Create(Sin(Arg.AsInteger))
  else
  if svfCardinal in Arg.FieldFlags then
    Result := TFpValueConstFloat.Create(Sin(Arg.AsInteger))
  else
    SetError('Argument not numeric');
end;

function TFpPascalExpressionPartIntrinsic.DoCos(AParams: TFpPascalExpressionPartBracketArgumentList
  ): TFpValue;
var
  Arg: TFpValue;
begin
  Result := nil;
  if not CheckArgumentCount(AParams, 1) then
    exit;
  if not GetArg(AParams, 1, Arg, 'argument required') then
    exit;

  if svfFloat in Arg.FieldFlags then
    Result := TFpValueConstFloat.Create(Cos(Arg.AsFloat))
  else
  if svfInteger in Arg.FieldFlags then
    Result := TFpValueConstFloat.Create(Cos(Arg.AsInteger))
  else
  if svfCardinal in Arg.FieldFlags then
    Result := TFpValueConstFloat.Create(Cos(Arg.AsInteger))
  else
    SetError('Argument not numeric');
end;

function TFpPascalExpressionPartIntrinsic.DoTan(AParams: TFpPascalExpressionPartBracketArgumentList
  ): TFpValue;
var
  Arg: TFpValue;
begin
  Result := nil;
  if not CheckArgumentCount(AParams, 1) then
    exit;
  if not GetArg(AParams, 1, Arg, 'argument required') then
    exit;

  if svfFloat in Arg.FieldFlags then
    Result := TFpValueConstFloat.Create(Tan(Arg.AsFloat))
  else
  if svfInteger in Arg.FieldFlags then
    Result := TFpValueConstFloat.Create(Tan(Arg.AsInteger))
  else
  if svfCardinal in Arg.FieldFlags then
    Result := TFpValueConstFloat.Create(Tan(Arg.AsInteger))
  else
    SetError('Argument not numeric');
end;

function TFpPascalExpressionPartIntrinsic.DoGetResultValue: TFpValue;
var
  p: TFpPascalExpressionPartBracketArgumentList;
begin
  Result := nil;
  if not (FIntrinsic in [ifFlattenPlaceholder, ifPi]) then begin
    // this gets called, if an intrinsic has no () after it. I.e. no arguments and no empty brackets
    SetError('wrong argument count');
    exit;
  end;

  p := TFpPascalExpressionPartBracketArgumentList.Create(ExpressionData, nil);
  p.FList.Add(Self);
  Result := DoGetResultValue(p);
  p.FList.Clear; // make sure the container does not destroy self
  p.Free;
end;

function TFpPascalExpressionPartIntrinsic.DoGetResultValue(
  AParams: TFpPascalExpressionPartBracketArgumentList): TFpValue;
begin
  Result := nil;
  case FIntrinsic of
    ifTry:        Result := DoTry(AParams);
    ifTryN:       Result := DoTryN(AParams);
    ifObj:        Result := DoObj(AParams);
    ifOrd:        Result := DoOrd(AParams);
    ifLength:     Result := DoLength(AParams);
    ifChildClass: Result := DoChildClass(AParams);
    ifRefCount:   Result := DoRefCnt(AParams);
    ifPos:        Result := DoPos(AParams);
    ifFlatten:    Result := DoFlatten(AParams);
    ifFlattenPlaceholder:    Result := DoFlattenPlaceholder(AParams);
    ifSubStr:     Result := DoSubStr(AParams);
    ifLower:      Result := DoLower(AParams);
    ifUpper:      Result := DoUpper(AParams);
    ifRound:      Result := DoRound(AParams);
    ifTrunc:      Result := DoTrunc(AParams);
    ifSqrt:       Result := DoSqrt(AParams);
    ifPi:         Result := DoPi(AParams);
    ifLn:         Result := DoLn(AParams);
    ifLog:        Result := DoLog(AParams);
    ifSin:        Result := DoSin(AParams);
    ifCos:        Result := DoCos(AParams);
    ifTan:        Result := DoTan(AParams);
  end;
  {$IFDEF WITH_REFCOUNT_DEBUG}
  if Result <> nil then
    Result.DbgRenameReference(nil, 'DoGetResultValue')
  {$ENDIF}
end;

procedure TFpPascalExpressionPartIntrinsic.Assign(ASourcePart: TFpPascalExpressionPart);
begin
  inherited Assign(ASourcePart);
  if ASourcePart is TFpPascalExpressionPartIntrinsic then
    FIntrinsic := TFpPascalExpressionPartIntrinsic(ASourcePart).FIntrinsic;
end;

constructor TFpPascalExpressionPartIntrinsic.Create(
  AnExpressionData: TFpPascalExpressionSharedData; AStartChar: PChar; AnEndChar: PChar;
  AnIntrinsic: TFpIntrinsicFunc);
begin
  inherited Create(AnExpressionData, AStartChar, AnEndChar);
  FIntrinsic := AnIntrinsic;
end;

destructor TFpPascalExpressionPartIntrinsic.Destroy;
begin
  inherited Destroy;
  FChildClassCastType.ReleaseReference;
end;

function TFpPascalExpressionPartIntrinsic.ReturnsVariant: boolean;
begin
  Result := (inherited ReturnsVariant) or
            (FIntrinsic in [ifChildClass, ifTry, ifTryN]);
            // TODO: compare types of each argument for ifTry/N
end;

function TFpPascalExpressionPartIntrinsic.AcceptParamAsSeparator(
  AParamPart: TFpPascalExpressionPart; ABracketsPart: TFpPascalExpressionPartContainer;
  var AResult: TFpPascalExpressionPart): boolean;
var
  LastItm: TFpPascalExpressionPart;
begin
  Result := False;
  LastItm := ABracketsPart.LastItem;
  if ( ((FIntrinsic = ifFlatten) and (ABracketsPart.Count >= 3)) or // only for keys / not for the initial value
       ((FIntrinsic = ifObj) and (ABracketsPart.Count >= 2))
     ) and
     (AParamPart is TFpPascalExpressionPartOperatorColon) and
     (TFpPascalExpressionPartOperatorColon(AParamPart).Count = 0) and
     not(LastItm is TFpPascalExpressionPartOperatorColonAsSeparator)
  then begin
    // Handle ":" as separator
    AResult := TFpPascalExpressionPartOperatorColonAsSeparator.Create(ExpressionData, AParamPart.FStartChar, AParamPart.FEndChar);
    AParamPart.Free;
    LastItm.HandleNextPart(AResult);
    Result := True;
  end;
end;

procedure TFpPascalExpressionPartIntrinsic.HandleNewParameterInList(AParamPart: TFpPascalExpressionPart;
  ABracketsPart: TFpPascalExpressionPartContainer);
begin
  if (FIntrinsic = ifFlatten) and (ABracketsPart.Count > 2) then begin
    // part 1 is the intrinsic / part 2 is the initial object
    // Part 3..n are the member expressions
    if AParamPart is TFpPascalExpressionPartIdentifier then begin
      TFpPascalExpressionPartIdentifier(AParamPart).OnGetSymbol := @DoGetMemberForFlattenExpr;
    end;
  end;
end;

procedure TFpPascalExpressionPartIntrinsic.HandleEndOfParameterInList(
  AParamPart: TFpPascalExpressionPart; ABracketsPart: TFpPascalExpressionPartContainer);
var
  n: TFpPascalExpressionPart;
begin
  inherited HandleEndOfParameterInList(AParamPart, ABracketsPart);
  if FIntrinsic = ifObj then begin
    if (ABracketsPart.Count > 1) and
       (not (ABracketsPart.LastItem is TFpPascalExpressionPartOperatorColonAsSeparator))
    then begin
      SetError('Argument must be "key:name"');
      exit;
    end;
    n := TFpPascalExpressionPartOperatorColonAsSeparator(ABracketsPart.LastItem).Items[0];
    if (not (n is TFpPascalExpressionPartIdentifier)) then
      SetError('Argument must be "key" must be identifier');
  end;
end;

{ TFpPascalExpressionPartConstantNumber }

function TFpPascalExpressionPartConstantNumber.DoGetResultValue: TFpValue;
var
  i: QWord;
  e: word;
  ds, ts: Char;
begin
  ds := DecimalSeparator;
  ts := ThousandSeparator;
  DecimalSeparator := '.';
  ThousandSeparator := #0;
  Val(GetText, i, e);
  DecimalSeparator := ds;
  ThousandSeparator := ts;
  if e <> 0 then begin
    Result := nil;
    SetErrorWithPos(fpErrPasParserExpectedNumber_p, [GetText(MAX_ERR_EXPR_QUOTE_LEN)]);
    exit;
  end;

  if FStartChar^ in ['0'..'9'] then
    Result := TFpValueConstNumber.Create(i, False)
  else
    Result := TFpValueConstNumber.Create(i, True); // hex,oct,bin values default to signed
  {$IFDEF WITH_REFCOUNT_DEBUG}Result.DbgRenameReference(nil, 'DoGetResultValue'){$ENDIF};
end;

{ TFpPascalExpressionPartConstantNumberFloat }

function TFpPascalExpressionPartConstantNumberFloat.DoGetResultValue: TFpValue;
var
  f: Extended;
  s: String;
  ds, ts: Char;
  ok: Boolean;
begin
  s := GetText;
  ds := DecimalSeparator;
  ts := ThousandSeparator;
  DecimalSeparator := '.';
  ThousandSeparator := #0;
  ok := TextToFloat(PChar(s), f);
  DecimalSeparator := ds;
  ThousandSeparator := ts;

  if not ok then begin
    Result := nil;
    SetErrorWithPos(fpErrPasParserExpectedNumber_p, [GetText(MAX_ERR_EXPR_QUOTE_LEN)]);
    exit;
  end;

  Result := TFpValueConstFloat.Create(f);
  {$IFDEF WITH_REFCOUNT_DEBUG}Result.DbgRenameReference(nil, 'DoGetResultValue'){$ENDIF};
end;

{ TFpPascalExpressionPartConstantText }

function TFpPascalExpressionPartConstantText.DoGetResultValue: TFpValue;
begin
  if Length(FValue) = 1 then
    Result := TFpValueConstChar.Create(FValue[1])
  else
    Result := TFpValueConstString.Create(FValue);
  {$IFDEF WITH_REFCOUNT_DEBUG}Result.DbgRenameReference(nil, 'DoGetResultValue'){$ENDIF};
end;

procedure TFpPascalExpressionPartConstantText.Assign(ASourcePart: TFpPascalExpressionPart);
begin
  inherited Assign(ASourcePart);
  if ASourcePart is TFpPascalExpressionPartConstantText then
    FValue := TFpPascalExpressionPartConstantText(ASourcePart).FValue;
end;

function CheckToken(const tk: String; CurPtr: PChar): boolean; inline;
var
  p, t: PChar;
  l: Integer;
begin
  Result := False;
  l := Length(tk);
  p := CurPtr + l;
  t := @tk[l];
  while p > CurPtr do begin
    if chr(ord(p^) and $DF) <> t^ then
      exit;
    dec(p);
    dec(t);
  end;
  Result := True;
end;

{ TFpPascalExpressionSharedData }

function TFpPascalExpressionSharedData.GetValid: Boolean;
begin
  Result := not IsError(FError);
end;

function TFpPascalExpressionSharedData.GetTextExpressionAddr: PChar;
begin
  Result := PChar(FTextExpression);
end;

procedure TFpPascalExpressionSharedData.SetError(AMsg: String);
begin
  if IsError(FError) then begin
DebugLn(DBG_WARNINGS, ['Skipping error ', AMsg]);
    exit;
  end;
  SetError(fpErrAnyError, [AMsg]);
DebugLn(DBG_WARNINGS, ['PARSER ERROR ', AMsg]);
end;

procedure TFpPascalExpressionSharedData.SetError(AnErrorCode: TFpErrorCode;
  const AnNestedErr: TFpError);
begin
  SetError(AnErrorCode, [], AnNestedErr);
end;

procedure TFpPascalExpressionSharedData.SetError(AnErrorCode: TFpErrorCode; AData: array of const;
  const AnNestedErr: TFpError);
begin
  FError := ErrorHandler.CreateError(AnErrorCode, AnNestedErr, AData);
  DebugLn(DBG_WARNINGS, ['Setting error ', ErrorHandler.ErrorAsString(FError)]);
end;

procedure TFpPascalExpressionSharedData.SetError(const AnErr: TFpError);
begin
  FError := AnErr;
  DebugLn(DBG_WARNINGS, ['Setting error ', ErrorHandler.ErrorAsString(FError)]);
end;

procedure TFpPascalExpressionSharedData.ClearError;
begin
  FError := NoError;
end;

function TFpPascalExpressionSharedData.PosFromPChar(APChar: PChar): Integer;
begin
  Result := APChar - @FTextExpression[1] + 1;
end;

constructor TFpPascalExpressionSharedData.Create(ATextExpression: String; AScope: TFpDbgSymbolScope
  );
begin
  inherited Create;
  AddReference;

  DisableFloatExceptions;

  FTextExpression := ATextExpression;
  FScope := AScope;
  FScope.AddReference;
  FError := NoError;
end;

destructor TFpPascalExpressionSharedData.Destroy;
begin
  EnableFloatExceptions;

  inherited Destroy;
  FScope.ReleaseReference;
end;

function TFpPascalExpressionSharedData.GetDbgSymbolForIdentifier(AnIdent: String;
  AFindFlags: TFindExportedSymbolsFlags): TFpValue;
begin
  if FScope <> nil then
    Result := FScope.FindSymbol(AnIdent, '', AFindFlags)
  else
    Result := nil;
end;

function TFpPascalExpressionSharedData.GetRegisterValue(AnIdent: String): TFpValue;
var
  RNum: Cardinal;
  RSize: Integer;
  RVal: QWord;
  Reg: TDbgRegisterValue;
begin
  Result := nil;
  if FScope = nil then
    exit;

  if not FScope.MemManager.RegisterNumber(AnIdent, RNum) then
    exit;

  RSize := FScope.MemManager.RegisterSize(RNum);
  if (RSize <= 8) and
     FScope.LocationContext.ReadUnsignedInt(RegisterLoc(RNum), SizeVal(RSize), RVal)
  then begin
    Result := TFpValueConstNumber.Create(RVal, False);
  end
  else begin
    Reg := FScope.LocationContext.GetRegister(RNum);
    if Reg <> nil then
      Result := TFpValueConstString.Create(Reg.StrValue);
  end;
end;

{ TFpPascalExpression }

procedure TFpPascalExpression.Parse;
var
  CurPtr, EndPtr, TokenEndPtr: PChar;
  CurPart, PrevPart, NewPart: TFpPascalExpressionPart;

  // "Foo-Error 'token' at pos N after 'prev token'"
  procedure SetParserError(AnErrorCode: TFpErrorCode);
  begin
    if PrevPart = nil
    then SetError(AnErrorCode, [GetFirstToken(CurPtr)], CreateError(fpErrPasParser_AtStart, []) )
    else SetError(AnErrorCode, [GetFirstToken(CurPtr)], CreateError(fpErrPasParser_PositionAfter, [FSharedData.PosFromPChar(CurPtr), PrevPart.GetText(MAX_ERR_EXPR_QUOTE_LEN)]) );
  end;
  procedure SetParserError(AnErrorCode: TFpErrorCode; AData: array of const);
  begin
    if PrevPart = nil
    then SetError(AnErrorCode, AData, CreateError(fpErrPasParser_AtStart, []) )
    else SetError(AnErrorCode, AData, CreateError(fpErrPasParser_PositionAfter, [FSharedData.PosFromPChar(CurPtr), PrevPart.GetText(MAX_ERR_EXPR_QUOTE_LEN)]) );
  end;
  procedure SetParserErrorPosOnly(AnErrorCode: TFpErrorCode);
  begin
    if PrevPart = nil
    then SetError(AnErrorCode, [], CreateError(fpErrPasParser_AtStart, []) )
    else SetError(AnErrorCode, [], CreateError(fpErrPasParser_PositionAfter, [FSharedData.PosFromPChar(CurPtr), PrevPart.GetText(MAX_ERR_EXPR_QUOTE_LEN)]) );
  end;

  procedure AddPart(AClass: TFpPascalExpressionPartClass);
  begin
    NewPart := AClass.Create(FSharedData, CurPtr, TokenEndPtr-1);
  end;

  procedure AddPlusMinus;
  begin
    if (CurPart = nil) or (not CurPart.CanHaveOperatorAsNext)
    then AddPart(TFpPascalExpressionPartOperatorUnaryPlusMinus)
    else AddPart(TFpPascalExpressionPartOperatorPlusMinus);
  end;

  procedure AddIntrinsic;
  var
    intr: TFpPascalExpressionPart;
  begin
    while TokenEndPtr^ in ['a'..'z', 'A'..'Z', '_', '0'..'9', '$'] do
      inc(TokenEndPtr);
    intr := LookupIntrinsic(CurPtr, TokenEndPtr - CurPtr);
    if intr = nil then
      SetParserError(fpErrPasParserUnknownIntrinsic_p)
    else
      NewPart := intr;
  end;

  function CheckOpenBracket: Boolean;
  var
    p: PChar;
  begin
    p := TokenEndPtr;
    while p^ in [' ', #9, #10, #13] do
      inc(p);
    Result := p^ = '(';
  end;

  procedure AddIdentifier;
  begin
    while TokenEndPtr^ in ['a'..'z', 'A'..'Z', '_', '0'..'9', '$'] do
      inc(TokenEndPtr);
    // TODO: Check functions not, and, in, as, is ...
    if (CurPart <> nil) and (CurPart.CanHaveOperatorAsNext) then
    case TokenEndPtr - CurPtr of
      3: case chr(ord(CurPtr^) AND $DF) of
          'D': if CheckToken('IV', CurPtr) then
              NewPart := TFpPascalExpressionPartOperatorMulDiv.Create(FSharedData, CurPtr, TokenEndPtr-1);
          'M': if CheckToken('OD', CurPtr) then
              NewPart := TFpPascalExpressionPartOperatorMulDiv.Create(FSharedData, CurPtr, TokenEndPtr-1);
          'A': if CheckToken('ND', CurPtr) then
              NewPart := TFpPascalExpressionPartOperatorAnd.Create(FSharedData, CurPtr, TokenEndPtr-1);
          'X': if CheckToken('OR', CurPtr) then
              NewPart := TFpPascalExpressionPartOperatorOr.Create(FSharedData, ootXor, CurPtr, TokenEndPtr-1);
          'N': if CheckToken('OT', CurPtr) then
              NewPart := TFpPascalExpressionPartOperatorUnaryNot.Create(FSharedData, CurPtr, TokenEndPtr-1);
          'S': if CheckToken('HL', CurPtr) then
              NewPart := TFpPascalExpressionPartOperatorShl.Create(FSharedData, CurPtr, TokenEndPtr-1)
            else
            if CheckToken('HR', CurPtr) then
              NewPart := TFpPascalExpressionPartOperatorShr.Create(FSharedData, CurPtr, TokenEndPtr-1);
        end;
      2: case chr(ord(CurPtr^) AND $DF) of
          'I': if CheckToken('N', CurPtr) then
              NewPart := TFpPascalExpressionPartOperatorMemberIn.Create(FSharedData, CurPtr, TokenEndPtr-1);
          'O': if CheckToken('R', CurPtr) then
              NewPart := TFpPascalExpressionPartOperatorOr.Create(FSharedData, ootOr, CurPtr, TokenEndPtr-1);
        end;
    end
    else
    case TokenEndPtr - CurPtr of
      3: case chr(ord(CurPtr^) AND $DF) of
          'N': if CheckToken('OT', CurPtr) then
              NewPart := TFpPascalExpressionPartOperatorUnaryNot.Create(FSharedData, CurPtr, TokenEndPtr-1);
        end;
    end;

    if (FIntrinsicPrefix = ipNoPrefix) and CheckOpenBracket then begin
      NewPart := LookupIntrinsic(CurPtr, TokenEndPtr - CurPtr);
      if (NewPart <> nil)  then
        exit;
    end;

    if NewPart = nil then
      NewPart := TFpPascalExpressionPartIdentifier.Create(FSharedData, CurPtr, TokenEndPtr-1);
  end;

  procedure HandleExclamation;
  begin
    if (CurPart = nil) or (not CurPart.CanHaveOperatorAsNext) then begin
      SetParserError(fpErrPasParserUnexpectedToken_p);
      exit;
    end;
    if not Valid then
      exit;
    if FPreviousArraySlice <> nil then begin
      CurPart := FPreviousArraySlice.AddController(CurPart);
      FPreviousArraySlice := FPreviousArraySlice.FPreviousArraySlice;
    end;
  end;

  procedure HandleDot;
  begin
    while TokenEndPtr^ = '.' do
      inc(TokenEndPtr);
    case TokenEndPtr - CurPtr of
      1: AddPart(TFpPascalExpressionPartOperatorMemberOf);
      2: if CurPart.SurroundingBracket is TFpPascalExpressionPartBracketIndex
        then AddPart(TFpPascalExpressionPartOperatorArraySlice)
        else SetParserError(fpErrPasParserUnexpectedToken_p);
      otherwise
        SetParserError(fpErrPasParserUnexpectedToken_p);
    end;
  end;

  procedure AddRefOperator;
  begin
    if (CurPart = nil) or (not CurPart.CanHaveOperatorAsNext)
    then AddPart(TFpPascalExpressionPartOperatorMakeRef)
    else AddPart(TFpPascalExpressionPartOperatorDeRef);
  end;

  procedure HandleRoundBracket;
  begin
    if (CurPart = nil) or (not CurPart.CanHaveOperatorAsNext)
    then AddPart(TFpPascalExpressionPartBracketSubExpression)
    else AddPart(TFpPascalExpressionPartBracketArgumentList);
  end;

  procedure HandleSqareBracket;
  begin
    if (CurPart = nil) or (not CurPart.CanHaveOperatorAsNext)
    then AddPart(TFpPascalExpressionPartBracketSet)
    else AddPart(TFpPascalExpressionPartBracketIndex);
  end;

  procedure CloseBracket(ABracketClass: TFpPascalExpressionPartBracketClass);
  var
    BracketPart: TFpPascalExpressionPartBracket;
  begin
    if (CurPart=nil) then begin
      SetParserError(fpErrPasParserUnexpectedToken_p);
      exit;
    end;
    BracketPart := CurPart.SurroundingBracket;
    if BracketPart = nil then begin
      SetParserError(fpErrPasParserMissingOpenBracket_p);
    end
    else
    if not (BracketPart is ABracketClass) then begin
      SetParserError(fpErrPasParserWrongOpenBracket_p, [GetFirstToken(CurPtr), FSharedData.PosFromPChar(BracketPart.StartChar), BracketPart.GetText(MAX_ERR_EXPR_QUOTE_LEN)]);
    end
    else begin
      TFpPascalExpressionPartBracket(BracketPart).CloseBracket(CurPart, @FPreviousArraySlice, CurPtr, TokenEndPtr-1);
      CurPart := BracketPart;
    end;
  end;

  procedure AddRegister;
  begin
    while TokenEndPtr^ in ['a'..'z', 'A'..'Z', '0'..'9', '_'] do
      inc(TokenEndPtr);
    AddPart(TFpPascalExpressionPartCpuRegister);
  end;

  procedure AddConstNumber;
  begin
    case CurPtr^ of
      '$': while TokenEndPtr^ in ['a'..'f', 'A'..'F', '0'..'9'] do inc(TokenEndPtr);
      '&': if TokenEndPtr^ in ['a'..'z', 'A'..'Z'] then begin
             // escaped keyword used as identifier
             while TokenEndPtr^ in ['a'..'z', 'A'..'Z', '0'..'9', '_'] do inc(TokenEndPtr);
             inc(CurPtr);
             NewPart := TFpPascalExpressionPartIdentifier.Create(FSharedData, CurPtr, TokenEndPtr-1);
             exit;
           end
           else
            while TokenEndPtr^ in ['0'..'7'] do inc(TokenEndPtr);
      '%': if TokenEndPtr^ in ['a'..'z', 'A'..'Z'] then begin
             inc(CurPtr);
             AddRegister;
             exit;
           end
           else
             while TokenEndPtr^ in ['0'..'1'] do inc(TokenEndPtr);
      '0'..'9':
        if (CurPtr^ = '0') and ((CurPtr + 1)^ in ['x', 'X']) and
           ((CurPtr + 2)^ in ['a'..'f', 'A'..'F', '0'..'9'])
        then begin
          inc(TokenEndPtr, 2);
          while TokenEndPtr^ in ['a'..'f', 'A'..'F', '0'..'9'] do inc(TokenEndPtr);
        end
        else begin
          while TokenEndPtr^ in ['0'..'9'] do inc(TokenEndPtr);
          // identify "2.", but not "[2..3]"  // CurExpr.IsFloatAllowed
          if (TokenEndPtr^ = '.') and (TokenEndPtr[1] <> '.') then begin
            inc(TokenEndPtr);
            while TokenEndPtr^ in ['0'..'9'] do inc(TokenEndPtr);
            if TokenEndPtr^ in ['a'..'z', 'A'..'Z', '_'] then
              SetParserError(fpErrPasParserUnexpectedToken_p)
            else
              AddPart(TFpPascalExpressionPartConstantNumberFloat);
            exit;
          end;
        end;
    end;
    if (TokenEndPtr < EndPtr) and (TokenEndPtr^ in ['0'..'9', 'a'..'z', 'A'..'Z']) or
       (TokenEndPtr[-1] in ['x', '$', '&', '%'])
    then begin
      SetError(fpErrPasParserExpectedNumber_p, [GetFirstToken(CurPtr+1)], CreateError(fpErrPasParser_PositionAfter, [FSharedData.PosFromPChar(CurPtr+1), '#']) );
      exit;
    end
    else
      AddPart(TFpPascalExpressionPartConstantNumber);
  end;

  procedure HandleCompare;
  begin
    if (CurPtr^ = '<') then begin
      if (TokenEndPtr^ = '<') then begin
        inc(TokenEndPtr);
        AddPart(TFpPascalExpressionPartOperatorShl);
        exit;
      end;

      if (TokenEndPtr^ in ['>', '=']) then
        inc(TokenEndPtr);
    end
    else
    if (CurPtr^ = '>') then begin
      if (TokenEndPtr^ = '>') then begin
        inc(TokenEndPtr);
        AddPart(TFpPascalExpressionPartOperatorShr);
        exit;
      end;

      if (TokenEndPtr^ in ['<', '=']) then
        inc(TokenEndPtr);
    end;

    AddPart(TFpPascalExpressionPartOperatorCompare);
  end;

  procedure HandleComma;
  begin
    if (CurPart=nil) or (not CurPart.HandleSeparator(ppstComma, CurPart)) then
      SetParserError(fpErrPasParserUnexpectedToken_p);
  end;

  procedure AddConstChar;
  var
    str: string;
    p: PChar;
    c: LongInt;
    WasQuote: Boolean;
  begin
    dec(TokenEndPtr);
    str := '';
    WasQuote := False;
    while (TokenEndPtr < EndPtr) and Valid do begin
      case TokenEndPtr^ of
        '''': begin
            if WasQuote then
              str := str + '''';
            WasQuote := False;
            inc(TokenEndPtr);
            p := TokenEndPtr;
            while (TokenEndPtr < EndPtr) and (TokenEndPtr^ <> '''') do
              inc(TokenEndPtr);
            str := str + copy(p, 1, TokenEndPtr - p);
            if (TokenEndPtr < EndPtr) and (TokenEndPtr^ = '''') then begin
              inc(TokenEndPtr);
            end
            else begin
              SetParserErrorPosOnly(fpErrPasParserUnterminatedString_p);
              exit;
            end;
          end;
        '#': begin
            WasQuote := False;
            inc(TokenEndPtr);
            if not (TokenEndPtr < EndPtr) then begin
              SetError(fpErrPasParserUnexpectedEndOfExpression, [GetFirstToken(CurPtr)]);
              exit;
            end;
            p := TokenEndPtr;
            case TokenEndPtr^  of
              '$': begin
                  inc(TokenEndPtr);
                  if (not (TokenEndPtr < EndPtr)) or (not (TokenEndPtr^ in ['0'..'9', 'a'..'f', 'A'..'F'])) then begin
                    SetError(fpErrPasParserExpectedNumber_p, [GetFirstToken(CurPtr+1)], CreateError(fpErrPasParser_PositionAfter, [FSharedData.PosFromPChar(CurPtr+1), '#']) );
                    exit;
                  end;
                  while (TokenEndPtr < EndPtr) and (TokenEndPtr^ in ['0'..'9', 'a'..'f', 'A'..'F']) do
                    inc(TokenEndPtr);
                end;
              '&': begin
                  inc(TokenEndPtr);
                  if (not (TokenEndPtr < EndPtr)) or (not (TokenEndPtr^ in ['0'..'7'])) then begin
                    SetError(fpErrPasParserExpectedNumber_p, [GetFirstToken(CurPtr+1)], CreateError(fpErrPasParser_PositionAfter, [FSharedData.PosFromPChar(CurPtr+1), '#']) );
                    exit;
                  end;
                  while (TokenEndPtr < EndPtr) and (TokenEndPtr^ in ['0'..'7']) do
                    inc(TokenEndPtr);
                end;
              '%': begin
                  inc(TokenEndPtr);
                  if (not (TokenEndPtr < EndPtr)) or (not (TokenEndPtr^ in ['0'..'1'])) then begin
                    SetError(fpErrPasParserExpectedNumber_p, [GetFirstToken(CurPtr+1)], CreateError(fpErrPasParser_PositionAfter, [FSharedData.PosFromPChar(CurPtr+1), '#']) );
                    exit;
                  end;
                  while (TokenEndPtr < EndPtr) and (TokenEndPtr^ in ['0'..'1']) do
                    inc(TokenEndPtr);
                end;
              '0'..'9': begin
                  while (TokenEndPtr < EndPtr) and (TokenEndPtr^ in ['0'..'9']) do
                    inc(TokenEndPtr);
                end;
              else begin
                  SetError(fpErrPasParserExpectedNumber_p, [GetFirstToken(CurPtr+1)], CreateError(fpErrPasParser_PositionAfter, [FSharedData.PosFromPChar(CurPtr+1), '#']) );
                  exit;
                end;
            end;
            c := StrToIntDef(copy(p , 1 , TokenEndPtr - p), -1);
            if (c < 0) or ( (TokenEndPtr < EndPtr) and (TokenEndPtr^ in ['0'..'9', 'a'..'z', 'A'..'Z']) )
            then begin
              SetError(fpErrPasParserExpectedNumber_p, [GetFirstToken(CurPtr+1)], CreateError(fpErrPasParser_PositionAfter, [FSharedData.PosFromPChar(CurPtr+1), '#']) );
              exit;
            end;
            if c > 255 then // todo: need wide handling
              str := str + WideChar(c)
            else
              str := str + Char(c);
          end;
        ' ', #9, #10, #13:
          inc(TokenEndPtr);
        else
          break;
      end;
    end;
    if not Valid then
      exit;
    AddPart(TFpPascalExpressionPartConstantText);
    TFpPascalExpressionPartConstantText(NewPart).FValue := str;
  end;

begin
  if FSharedData.TextExpression = '' then begin
    SetError(fpErrPasParserEmptyExpression);
    exit;
  end;
  CurPtr := FSharedData.TextExpressionAddr;
  EndPtr := CurPtr + length(FSharedData.TextExpression);
  while (CurPtr^ in [' ', #9, #10, #13]) and (CurPtr < EndPtr) do
    Inc(CurPtr);
  if CurPtr = EndPtr then begin
    SetError(fpErrPasParserEmptyExpression);
    exit;
  end;


  CurPart := nil;
  PrevPart := nil;
  While (CurPtr < EndPtr) and Valid do begin
    if CurPtr^ in [' ', #9, #10, #13] then begin
      while (CurPtr^ in [' ', #9, #10, #13]) and (CurPtr < EndPtr) do
        Inc(CurPtr);
      continue;
    end;

    NewPart := nil;
    TokenEndPtr := CurPtr + 1;
    if (FIntrinsicPrefix = ipExclamation) and (CurPtr^ = '!') and
       ((CurPart = nil) or (not CurPart.CanHaveOperatorAsNext) )
    then begin
      inc(CurPtr);
      AddIntrinsic;
    end
    else
    if (CurPtr^ = ':') then begin
      if (CurPart <> nil) and CurPart.CanHaveOperatorAsNext then begin
        AddPart(TFpPascalExpressionPartOperatorColon);
      end
      else
      if (FIntrinsicPrefix = ipColon) then begin
        inc(CurPtr);
        AddIntrinsic;
      end
      else begin
        SetParserError(fpErrPasParserUnexpectedToken_p);
        break;
      end;
    end
    else
    case CurPtr^ of
      '@' :      AddPart(TFpPascalExpressionPartOperatorAddressOf);
      '?' :      AddPart(TFpPascalExpressionPartOperatorQuestionMark);
      '!' :      HandleExclamation;
      '^':       AddRefOperator; // ^A may be #$01
      '.':       HandleDot;
      '+', '-' : AddPlusMinus;
      '*', '/' : AddPart(TFpPascalExpressionPartOperatorMulDiv);
      '(':       HandleRoundBracket;
      ')':       CloseBracket(TFpPascalExpressionPartRoundBracket);
      '[':       HandleSqareBracket;
      ']':       CloseBracket(TFpPascalExpressionPartSquareBracket);
      ',':       HandleComma;
      '=', '<',
      '>':       HandleCompare;//TFpPascalExpressionPartOperatorCompare
      '''', '#': AddConstChar;
      '0'..'9',
      '$', '%', '&':  AddConstNumber;
      'a'..'z',
      'A'..'Z', '_': AddIdentifier;
      else begin
          SetParserError(fpErrPasParserUnexpectedToken_p);
          break;
        end;
    end;
    if not Valid then
      break;

    PrevPart := NewPart;
    if CurPart = nil then
      CurPart := NewPart
    else
    if NewPart <> nil then
      CurPart := CurPart.HandleNextPart(NewPart);

    CurPtr :=  TokenEndPtr;
  end; // While CurPtr < EndPtr do begin



  if Valid then begin
    if CurPart <> nil then begin
      CurPart.HandleEndOfExpression;
      CurPart := CurPart.TopParent;
    end
    else
      SetError(fpErrPasParserEmptyExpression);
  end
  else
  if CurPart <> nil then
    CurPart := CurPart.TopParent;

  if FPreviousArraySlice <> nil then
    CurPart := FPreviousArraySlice.AddControllerRecursive(CurPart);
  FExpressionPart := CurPart;
end;

function TFpPascalExpression.GetResultValue: TFpValue;
begin
  if (FExpressionPart = nil) or (not Valid) then
    Result := nil
  else begin
    Result := FExpressionPart.ResultValue;
    if (Result = nil) and Valid then
      SetError(fpErrAnyError, ['Internal eval error']);
  end;
end;

function TFpPascalExpression.GetGlobalCache: TFpDbgDataCache;
begin
  Result := FSharedData.GlobalCache;
end;

function TFpPascalExpression.GetOnFunctionCall: TFpPascalParserCallFunctionProc;
begin
  Result := FSharedData.OnFunctionCall;
end;

function TFpPascalExpression.GetAutoDeref: Boolean;
begin
  Result := FSharedData.AutoDeref;
end;

function TFpPascalExpression.GetError: TFpError;
begin
  Result := FSharedData.Error;
end;

function TFpPascalExpression.GetFixPCharIndexAccess: Boolean;
begin
  Result := FSharedData.FixPCharIndexAccess;
end;

function TFpPascalExpression.GetHasPCharIndexAccess: Boolean;
begin
  Result := FSharedData.HasPCharIndexAccess;
end;

function TFpPascalExpression.GetValid: Boolean;
begin
  Result := FSharedData.Valid;
end;

procedure TFpPascalExpression.SetAutoDeref(AValue: Boolean);
begin
  FSharedData.AutoDeref := AValue;
end;

procedure TFpPascalExpression.SetGlobalCache(AValue: TFpDbgDataCache);
begin
  FSharedData.GlobalCache := AValue;
end;

procedure TFpPascalExpression.SetOnFunctionCall(AValue: TFpPascalParserCallFunctionProc);
begin
  FSharedData.OnFunctionCall := AValue;
end;

procedure TFpPascalExpression.SetFixPCharIndexAccess(AValue: Boolean);
begin
  FSharedData.FixPCharIndexAccess := AValue;
end;

function TFpPascalExpression.LookupIntrinsic(AStart: PChar; ALen: Integer
  ): TFpPascalExpressionPart;
var
  Intr: TFpIntrinsicFunc;
begin
  Result := nil;
  Intr := ifErrorNotFound;
  case ALen of
    1: begin
        if AStart^ = '_'   then Intr := ifFlattenPlaceholder;
    end;
    2: begin
        if strlicomp(AStart, 'CC', 2) = 0     then Intr := ifChildClass
        else
        if strlicomp(AStart, 'F_', 2) = 0     then Intr := ifFlatten
        else
        if strlicomp(AStart, 'PI', 2) = 0     then Intr := ifPi
        else
        if strlicomp(AStart, 'LN', 2) = 0     then Intr := ifLn
        ;
    end;
    3: case AStart^ of
        'l', 'L': if strlicomp(AStart, 'LEN', 3) = 0 then Intr := ifLength
                  else
                  if strlicomp(AStart, 'LOG', 3) = 0 then Intr := ifLog;
        'p', 'P': if strlicomp(AStart, 'POS', 3) = 0 then Intr := ifPos;
        'o', 'O': if strlicomp(AStart, 'ORD', 3) = 0 then Intr := ifOrd
                  else
                  if strlicomp(AStart, 'OBJ', 3) = 0 then Intr := ifObj;
        't', 'T': if strlicomp(AStart, 'TRY', 3) = 0 then Intr := ifTry
                  else
                  if strlicomp(AStart, 'TAN', 3) = 0 then Intr := ifTan;
        's', 'S': if strlicomp(AStart, 'SIN', 3) = 0 then Intr := ifSin;
        'c', 'C': if strlicomp(AStart, 'COS', 3) = 0 then Intr := ifCos;
    end;
    4: case AStart^ of
        's', 'S': if strlicomp(AStart, 'SQRT', 4) = 0 then Intr := ifSqrt;
        'l', 'L': if strlicomp(AStart, 'LOGN', 4) = 0 then Intr := ifLog;
        't', 'T': if strlicomp(AStart, 'TRYN', 4) = 0 then Intr := ifTryN;
    end;
    5: case AStart^ of
        'l', 'L': if strlicomp(AStart, 'LOWER', 5) = 0 then Intr := ifLower;
        'u', 'U': if strlicomp(AStart, 'UPPER', 5) = 0 then Intr := ifUpper;
        'r', 'R': if strlicomp(AStart, 'ROUND', 5) = 0 then Intr := ifRound;
        't', 'T': if strlicomp(AStart, 'TRUNC', 5) = 0 then Intr := ifTrunc
                  else
                  if strlicomp(AStart, 'TRYNN', 5) = 0 then Intr := ifTryN;
       end;
    6: case AStart^ of
        'l', 'L': if strlicomp(AStart, 'LENGTH', 6) = 0 then Intr := ifLength;
        'r', 'R': if strlicomp(AStart, 'REFCNT', 6) = 0 then Intr := ifRefCount;
        's', 'S': if strlicomp(AStart, 'SUBSTR', 6) = 0 then Intr := ifSubStr;
      end;
    7: case AStart^ of
        'f', 'F': if strlicomp(AStart, 'FLATTEN', 7) = 0 then Intr := ifFlatten;
      end;
  end;
  if Intr <> ifErrorNotFound then begin
    Result := TFpPascalExpressionPartIntrinsic.Create(FSharedData, AStart, AStart+ALen, Intr);
    exit;
  end;
  if (FOnFindIntrinsc <> nil) and (ALen > 0) then begin
    Result := FOnFindIntrinsc(Self, AStart, ALen);
  end;
end;

procedure TFpPascalExpression.SetError(AnErrorCode: TFpErrorCode; const AnNestedErr: TFpError);
begin
  FSharedData.SetError(AnErrorCode, AnNestedErr);
end;

procedure TFpPascalExpression.SetError(AnErrorCode: TFpErrorCode; AData: array of const;
  const AnNestedErr: TFpError);
begin
  FSharedData.SetError(AnErrorCode, AData, AnNestedErr);
end;

constructor TFpPascalExpression.Create(ATextExpression: String; AScope: TFpDbgSymbolScope;
  ASkipParse: Boolean);
begin
  FSharedData := TFpPascalExpressionSharedData.Create(ATextExpression, AScope);
  if not ASkipParse then
    Parse;
end;

destructor TFpPascalExpression.Destroy;
begin
  FreeAndNil(FExpressionPart);
  inherited Destroy;
  FSharedData.ReleaseReference;
end;

function TFpPascalExpression.DebugDump(AWithResults: Boolean): String;
begin
  Result := 'TFpPascalExpression: ' + FSharedData.TextExpression + LineEnding +
            'Valid: ' + dbgs(Valid) + '   Error: "' + dbgs(ErrorCode(Error)) + '"'+ LineEnding
            ;
  if FExpressionPart <> nil then
    Result := Result + FExpressionPart.DebugDump('  ', AWithResults);
  if AWithResults and (ResultValue <> nil) then
    if (ResultValue is TFpPasParserValue) then
      Result := Result + 'ResultValue = ' + LineEnding + TFpPasParserValue(ResultValue).DebugText('  ')
    else
      Result := Result + 'ResultValue = ' + LineEnding + DbgSName(ResultValue) + LineEnding ;
end;

procedure TFpPascalExpression.ResetEvaluation;
begin
  FExpressionPart.ResetEvaluationRecursive;
end;

{ TFpPascalExpressionPart }

procedure TFpPascalExpressionPart.SetEndChar(AValue: PChar);
begin
  if FEndChar = AValue then Exit;
  FEndChar := AValue;
end;

function TFpPascalExpressionPart.GetTopParent: TFpPascalExpressionPart;
begin
  Result := Self;
  while Result.Parent <> nil do
    Result := Result.Parent;
end;

function TFpPascalExpressionPart.GetSurroundingOpenBracket: TFpPascalExpressionPartBracket;
var
  tmp: TFpPascalExpressionPart;
begin
  Result := nil;
  tmp := Self;
  while (tmp <> nil) and
        ( not(tmp is TFpPascalExpressionPartBracket) or ((tmp as TFpPascalExpressionPartBracket).IsClosed) )
  do
    tmp := tmp.Parent;
  if tmp <> nil then
    Result := TFpPascalExpressionPartBracket(tmp);
end;

function TFpPascalExpressionPart.GetResultValue: TFpValue;
begin
  Result := FResultValue;
  if FResultValDone then
    exit;
  FResultValue := DoGetResultValue;
  {$IFDEF WITH_REFCOUNT_DEBUG}if FResultValue <> nil then FResultValue.DbgRenameReference(nil, 'DoGetResultValue', @FResultValue, 'DoGetResultValue');{$ENDIF}
  FResultValDone := True;
  Result := FResultValue;
end;

procedure TFpPascalExpressionPart.SetParent(AValue: TFpPascalExpressionPartContainer);
begin
  if FParent = AValue then Exit;
  FParent := AValue;
end;

procedure TFpPascalExpressionPart.SetStartChar(AValue: PChar);
begin
  if FStartChar = AValue then Exit;
  FStartChar := AValue;
end;

function TFpPascalExpressionPart.GetText(AMaxLen: Integer): String;
var
  Len: Integer;
begin
  if FEndChar <> nil
  then Len := FEndChar - FStartChar + 1
  else Len := min(AMaxLen, 10);
  if (AMaxLen > 0) and (Len > AMaxLen) then
    Len := AMaxLen;
  Result := Copy(FStartChar, 1, Len);
end;

function TFpPascalExpressionPart.GetPos: Integer;
begin
  Result := ExpressionData.PosFromPChar(FStartChar);
end;

function TFpPascalExpressionPart.GetFullText(AMaxLen: Integer): String;
begin
  Result := GetText(AMaxLen);
end;

function TFpPascalExpressionPart.ReturnsVariant: boolean;
begin
  Result := False;
  if FResultValue <> nil then
    Result := (FResultValue.Flags * [vfVariant, vfArrayOfVariant] <> []);
end;

function TFpPascalExpressionPart.IsClosed: boolean;
begin
  Result := True;
end;

function TFpPascalExpressionPart.FindInParents(APart: TFpPascalExpressionPart): Boolean;
var
  p: TFpPascalExpressionPart;
begin
  p := Self;
  while p <> nil do begin
    Result := APart = p;
    if Result then
      exit;
    p := p.Parent;
  end;
  Result := False;
end;

procedure TFpPascalExpressionPart.SetError(AMsg: String);
begin
  if AMsg = '' then
    AMsg := 'Invalid Expression';
  ExpressionData.SetError(Format('%0:s at %1:d: "%2:s"', [AMsg, ExpressionData.PosFromPChar(FStartChar), GetText(20)]));
end;

procedure TFpPascalExpressionPart.SetError(APart: TFpPascalExpressionPart; AMsg: String);
begin
  if APart <> nil
  then APart.SetError(AMsg)
  else Self.SetError(AMsg);
end;

function TFpPascalExpressionPart.CreateErrorWithPos(AnErrorCode: TFpErrorCode;
  AData: array of const; APos: integer): TFpError;
begin
  if APos < 0 then
    APos := GetPos;
  if APos = 1
  then Result := CreateError(AnErrorCode, AData, CreateError(fpErrPasParser_AtStart,  []      ))
  else Result := CreateError(AnErrorCode, AData, CreateError(fpErrPasParser_Position, [GetPos]));
end;

procedure TFpPascalExpressionPart.SetErrorWithPos(AnErrorCode: TFpErrorCode;
  AData: array of const);
begin
  ExpressionData.SetError(CreateErrorWithPos(AnErrorCode, AData));
end;

procedure TFpPascalExpressionPart.SetError(AnErrorCode: TFpErrorCode;
  AData: array of const; const AnNestedErr: TFpError);
begin
  ExpressionData.SetError(AnErrorCode, AData, AnNestedErr);
end;

procedure TFpPascalExpressionPart.SetError(AnError: TFpError);
begin
  ExpressionData.SetError(AnError);
end;

procedure TFpPascalExpressionPart.Init;
begin
  //
end;

procedure TFpPascalExpressionPart.Assign(ASourcePart: TFpPascalExpressionPart);
begin
  FStartChar      := ASourcePart.FStartChar;
  FEndChar        := ASourcePart.FEndChar;
  FExpressionData := ASourcePart.ExpressionData;
  FPrecedence     := ASourcePart.FPrecedence;
end;

function TFpPascalExpressionPart.FindCopiedInParents(ASourcePart,
  AFindInSourcePart: TFpPascalExpressionPart; AFindFlags: TFindInParentsFlags
  ): TFpPascalExpressionPart;
begin
  Result := Self;
  while (Result <> nil) and (ASourcePart <> nil) do begin
    if AFindInSourcePart = ASourcePart then
      exit;
    if (fipIncludeBracketFunction in AFindFlags) and
       (ASourcePart is TFpPascalExpressionPartBracketArgumentList) and
       (TFpPascalExpressionPartContainer(ASourcePart).Count > 1) and
       (AFindInSourcePart = TFpPascalExpressionPartContainer(ASourcePart).Items[0])
    then
      exit(TFpPascalExpressionPartContainer(ASourcePart).Items[0]);

    ASourcePart := ASourcePart.Parent;
    Result := Result.Parent;
  end;
end;

function TFpPascalExpressionPart.DoGetIsTypeCast: Boolean;
begin
  Result := False;
end;

function TFpPascalExpressionPart.DoGetResultValue: TFpValue;
begin
  Result := nil;
  SetError('Can not evaluate: "'+GetText+'"');
end;

procedure TFpPascalExpressionPart.ResetEvaluation;
begin
  FResultValue.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FResultValue, 'DoGetResultValue'){$ENDIF};
  FResultValue := nil;
  FResultValDone := False;
end;

procedure TFpPascalExpressionPart.ResetEvaluationRecursive;
begin
  ResetEvaluation;
end;

procedure TFpPascalExpressionPart.ResetEvaluationForAnchestors;
begin
  ResetEvaluation;
  if Parent <> nil then
    Parent.ResetEvaluationForAnchestors;
end;

procedure TFpPascalExpressionPart.ReplaceInParent(AReplacement: TFpPascalExpressionPart);
var
  i: Integer;
begin
  if Parent = nil then exit;
  i := Parent.IndexOf(Self);
  Assert(i >= 0);
  Parent.Items[i] := AReplacement;
  Parent := nil;
end;

procedure TFpPascalExpressionPart.DoHandleEndOfExpression;
begin
  //
end;

procedure TFpPascalExpressionPart.DoParentIndexBraceClosed(
  APreviousArraySliceList: PFpPascalExpressionPartOperatorArraySlice);
begin
  //
end;

function TFpPascalExpressionPart.IsValidNextPart(APart: TFpPascalExpressionPart): Boolean;
begin
  Result := APart.IsValidAfterPart(Self);
end;

function TFpPascalExpressionPart.IsValidAfterPart(APrevPart: TFpPascalExpressionPart): Boolean;
begin
  Result := True;
end;

function TFpPascalExpressionPart.MaybeHandlePrevPart(APrevPart: TFpPascalExpressionPart;
  var AResult: TFpPascalExpressionPart): Boolean;
begin
  Result := False;
end;

function TFpPascalExpressionPart.HasPrecedence: Boolean;
begin
  Result := False;
end;

function TFpPascalExpressionPart.FindLeftSideOperandByPrecedence(AnOperator: TFpPascalExpressionPartWithPrecedence): TFpPascalExpressionPart;
begin
  Result := Self;
end;

function TFpPascalExpressionPart.CanHaveOperatorAsNext: Boolean;
begin
  Result := True;
end;

function TFpPascalExpressionPart.HandleSeparator(
  ASeparatorType: TSeparatorType; var APart: TFpPascalExpressionPart): Boolean;
begin
  Result := (Parent <> nil) and Parent.HandleSeparator(ASeparatorType, APart);
end;

procedure TFpPascalExpressionPart.GetFirstLastChar(out AFirst, ALast: PChar);
begin
  AFirst := FStartChar;
  ALast  := FEndChar;
end;

function TFpPascalExpressionPart.DebugText(AIndent: String; AWithResults: Boolean): String;
begin
  Result := Format('%s%s at %d: "%s"',
                   [AIndent, ClassName, ExpressionData.PosFromPChar(FStartChar), GetText])
                   + LineEnding;
end;

function TFpPascalExpressionPart.DebugDump(AIndent: String; AWithResults: Boolean): String;
begin
  Result := DebugText(AIndent, AWithResults);
  if AWithResults and (FResultValue <> nil) then
    if (FResultValue is TFpPasParserValue) then
      Result := Result + TFpPasParserValue(FResultValue).DebugText(AIndent+'    //  ')
    else
      Result := Result + AIndent+'    //  FResultValue = ' + DbgSName(FResultValue) + LineEnding;
end;

constructor TFpPascalExpressionPart.Create(AnExpressionData: TFpPascalExpressionSharedData;
  AStartChar: PChar; AnEndChar: PChar);
begin
  FExpressionData := AnExpressionData;
  FExpressionData.AddReference;
  FStartChar := AStartChar;
  FEndChar := AnEndChar;
  //FResultTypeFlag := rtUnknown;
  FResultValDone := False;
  Init;
end;

destructor TFpPascalExpressionPart.Destroy;
begin
  inherited Destroy;
  FExpressionData.ReleaseReference;
  //FResultType.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(nil, 'DoGetResultType'){$ENDIF};
  FResultValue.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FResultValue, 'DoGetResultValue'){$ENDIF};
end;

function TFpPascalExpressionPart.CreateCopy(ACopiedParent: TFpPascalExpressionPartContainer
  ): TFpPascalExpressionPart;
begin
  Result := TFpPascalExpressionPartClass(ClassType).Create(ExpressionData, FStartChar, FEndChar);
  Result.FParent := ACopiedParent;
  Result.FIsCopy := True;
  //Result.ExpressionData.AddReference;
  Result.Assign(Self);
end;

procedure TFpPascalExpressionPart.BeginNeedCopy;
begin
  //
end;

procedure TFpPascalExpressionPart.EndNeedCopy;
begin
  //
end;

function TFpPascalExpressionPart.HandleNextPart(APart: TFpPascalExpressionPart): TFpPascalExpressionPart;
begin
  Result := APart;
  if APart.MaybeHandlePrevPart(Self, Result) then
    exit;

  if Parent <> nil then begin
    Result := Parent.HandleNextPart(APart);
    exit;
  end;

  SetError(APart, 'Unexpected ');
  APart.Free;
  Result := Self;
end;

function TFpPascalExpressionPart.AcceptParamAsSeparator(AParamPart: TFpPascalExpressionPart;
  ABracketsPart: TFpPascalExpressionPartContainer; var AResult: TFpPascalExpressionPart): boolean;
begin
  Result := False;
end;

procedure TFpPascalExpressionPart.HandleNewParameterInList(AParamPart: TFpPascalExpressionPart;
  ABracketsPart: TFpPascalExpressionPartContainer);
begin
  //
end;

procedure TFpPascalExpressionPart.HandleEndOfParameterInList(AParamPart: TFpPascalExpressionPart;
  ABracketsPart: TFpPascalExpressionPartContainer);
begin
  //
end;

procedure TFpPascalExpressionPart.HandleEndOfExpression;
begin
  DoHandleEndOfExpression;
  if Parent <> nil then
    Parent.HandleEndOfExpression;
end;

{ TFpPascalExpressionPartContainer }

function TFpPascalExpressionPartContainer.GetItems(AIndex: Integer): TFpPascalExpressionPart;
begin
  Result := TFpPascalExpressionPart(FList[AIndex]);
end;

function TFpPascalExpressionPartContainer.GetLastItem: TFpPascalExpressionPart;
begin
  if Count > 0 then
    Result := Items[Count - 1]
  else
    Result := nil;
end;

procedure TFpPascalExpressionPartContainer.SetItems(AIndex: Integer;
  AValue: TFpPascalExpressionPart);
begin
  AValue.Parent := Self;
  FList[AIndex] := AValue;
end;

procedure TFpPascalExpressionPartContainer.SetLastItem(AValue: TFpPascalExpressionPart);
begin
  assert(Count >0);
  Items[Count-1] := AValue;
end;

procedure TFpPascalExpressionPartContainer.Init;
begin
  FList := TList.Create;
  inherited Init;
end;

procedure TFpPascalExpressionPartContainer.Assign(ASourcePart: TFpPascalExpressionPart);
var
  AContainerSourcePart: TFpPascalExpressionPartContainer absolute ASourcePart;
  i: Integer;
begin
  inherited Assign(ASourcePart);
  Clear;
  if ASourcePart is TFpPascalExpressionPartContainer then begin
    for i := 0 to AContainerSourcePart.Count - 1 do
      Add(AContainerSourcePart.Items[i].CreateCopy(Self));
  end;
end;

procedure TFpPascalExpressionPartContainer.ResetEvaluationRecursive;
var
  i: Integer;
begin
  inherited ResetEvaluationRecursive;
  for i := 0 to Count - 1 do
    Items[i].ResetEvaluationRecursive;
end;

procedure TFpPascalExpressionPartContainer.GetFirstLastChar(out AFirst, ALast: PChar);
var
  i: Integer;
  f, l: PChar;
begin
  inherited GetFirstLastChar(AFirst, ALast);
  for i := 0 to Count -1 do begin
    Items[i].GetFirstLastChar(f,l);
    if (AFirst = nil) or ( (f <> nil) and (f < AFirst) ) then
      AFirst := f;
    if (ALast = nil) or ( (l <> nil) and (l > ALast) ) then
      ALast := l;
  end;
end;

function TFpPascalExpressionPartContainer.DebugDump(AIndent: String;
  AWithResults: Boolean): String;
var
  i: Integer;
begin
  Result := inherited DebugDump(AIndent, AWithResults);
  for i := 0 to Count - 1 do
    Result := Result + Items[i].DebugDump(AIndent+'  ', AWithResults);
end;

function TFpPascalExpressionPartContainer.GetCount: Integer;
begin
  Result := FList.Count;
end;

destructor TFpPascalExpressionPartContainer.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TFpPascalExpressionPartContainer.BeginNeedCopy;
var
  i: Integer;
begin
  inherited BeginNeedCopy;
  for i := 0 to Count - 1 do
    Items[i].BeginNeedCopy;
end;

procedure TFpPascalExpressionPartContainer.EndNeedCopy;
var
  i: Integer;
begin
  inherited EndNeedCopy;
  for i := 0 to Count - 1 do
    Items[i].EndNeedCopy;
end;

function TFpPascalExpressionPartContainer.GetFullText(AMaxLen: Integer): String;
var
  s, e: PChar;
  Len: Integer;
begin
  GetFirstLastChar(s,e);

  if e <> nil
  then Len := e - s + 1
  else Len := min(AMaxLen, 10);
  if (AMaxLen > 0) and (Len > AMaxLen) then
    Len := AMaxLen;
  Result := Copy(s, 1, Len);
end;

function TFpPascalExpressionPartContainer.Add(APart: TFpPascalExpressionPart): Integer;
begin
  APart.Parent := Self;
  Result := FList.Add(APart);
end;

function TFpPascalExpressionPartContainer.IndexOf(APart: TFpPascalExpressionPart): Integer;
begin
  Result := Count - 1;
  while (Result >= 0) and (Items[Result] <> APart) do
    dec(Result);
end;

procedure TFpPascalExpressionPartContainer.Clear;
begin
  while Count > 0 do begin
    Items[0].Free;
    FList.Delete(0);
  end;
end;

{ TFpPascalExpressionPartBracket }

function TFpPascalExpressionPartBracket.GetAfterComma: Boolean;
begin
  Result := (FAfterComma = Count);
end;

procedure TFpPascalExpressionPartBracket.Init;
begin
  inherited Init;
  FIsClosed := False;
  FIsClosing := False;
  FAfterComma := -1;
end;

function TFpPascalExpressionPartBracket.HasPrecedence: Boolean;
begin
  Result := False;
end;

procedure TFpPascalExpressionPartBracket.DoHandleEndOfExpression;
begin
  if not IsClosed then begin
    SetError('Bracket not closed');
    exit;
  end;
  inherited DoHandleEndOfExpression;
end;

function TFpPascalExpressionPartBracket.CanHaveOperatorAsNext: Boolean;
begin
  Result := IsClosed;
end;

function TFpPascalExpressionPartBracket.HandleNextPartInBracket(APart: TFpPascalExpressionPart): TFpPascalExpressionPart;
begin
  Result := Self;
  APart.Free;
  SetError('Error in ()');
end;

procedure TFpPascalExpressionPartBracket.SetAfterCommaFlag;
begin
  FAfterComma := Count;
end;

procedure TFpPascalExpressionPartBracket.GetFirstLastChar(out AFirst, ALast: PChar);
begin
  inherited GetFirstLastChar(AFirst, ALast);
  if (FFullEndChar <> nil) and
     ( (FFullEndChar > ALast) or (ALast = nil))
  then
    ALast  := FFullEndChar;
end;

procedure TFpPascalExpressionPartBracket.CheckBeforeSeparator(APart: TFpPascalExpressionPart);
begin
  if APart = nil then
    exit;
  FIsSeparatorChecking := True;
  APart.HandleEndOfExpression;
  FIsSeparatorChecking := False;
end;

procedure TFpPascalExpressionPartBracket.Assign(ASourcePart: TFpPascalExpressionPart);
var
  ABracketSourcePart: TFpPascalExpressionPartBracket absolute ASourcePart;
begin
  inherited Assign(ASourcePart);
  if ASourcePart is TFpPascalExpressionPartBracket then begin
    FIsClosed    := ABracketSourcePart.FIsClosed;
    FIsClosing   := ABracketSourcePart.FIsClosing;
    FFullEndChar := ABracketSourcePart.FFullEndChar;
  end;
end;

procedure TFpPascalExpressionPartBracket.CloseBracket(ALastAddedPart: TFpPascalExpressionPart;
  APreviousArraySliceList: PFpPascalExpressionPartOperatorArraySlice; AStartChar: PChar;
  AnEndChar: PChar);
begin
  FFullEndChar := AnEndChar;
  if AfterComma then begin
    SetError(fpErrPasParserMissingExprAfterComma, [GetText(MAX_ERR_EXPR_QUOTE_LEN), GetPos]);
    exit;
  end;
  FIsClosing := True;
  if ALastAddedPart <> nil then
    ALastAddedPart.HandleEndOfExpression;
  FIsClosing := False;
  FIsClosed := True;
end;

function TFpPascalExpressionPartBracket.HandleNextPart(APart: TFpPascalExpressionPart): TFpPascalExpressionPart;
begin
  if IsClosed then begin
    Result := inherited HandleNextPart(APart);
    exit;
  end;

  Result := HandleNextPartInBracket(APart);
end;

procedure TFpPascalExpressionPartBracket.HandleEndOfExpression;
begin
  if not (FIsClosing or FIsSeparatorChecking) then
    inherited HandleEndOfExpression;
end;

function TFpPascalExpressionPartBracket.IsClosed: boolean;
begin
  Result := FIsClosed;
end;

{ TFpPascalExpressionPartOperator }

function TFpPascalExpressionPartOperator.DebugText(AIndent: String;
  AWithResults: Boolean): String;
begin
  Result := inherited DebugText(AIndent, AWithResults);
  while Result[Length(Result)] in [#10, #13] do SetLength(Result, Length(Result)-1);
  Result := Result + ' Precedence:' + dbgs(FPrecedence) +
    LineEnding;
end;

function TFpPascalExpressionPartOperator.CanHaveOperatorAsNext: Boolean;
begin
  Result := HasAllOperands and LastItem.CanHaveOperatorAsNext;
end;

function TFpPascalExpressionPartOperator.FindLeftSideOperandByPrecedence(AnOperator: TFpPascalExpressionPartWithPrecedence): TFpPascalExpressionPart;
begin
  Result := Self;

  if (not HasAllOperands) or (LastItem = nil) then begin
    Result := nil;
    exit
  end;

  // precedence: 1 = highest
  if Precedence > AnOperator.Precedence then
    Result := LastItem.FindLeftSideOperandByPrecedence(AnOperator);
end;

function TFpPascalExpressionPartOperator.MaybeAddLeftOperand(APrevPart: TFpPascalExpressionPart;
  var AResult: TFpPascalExpressionPart): Boolean;
var
  ALeftSide: TFpPascalExpressionPart;
begin
  Result := APrevPart.IsValidNextPart(Self);
  if not Result then
    exit;

  AResult := Self;
  if (Count > 0) or // Previous already set
     (not APrevPart.CanHaveOperatorAsNext) // can not have 2 operators follow each other
  then begin
    SetError(APrevPart, 'Can not apply operator '+GetText+': ');
    APrevPart.Free;
    exit;
  end;

  ALeftSide := APrevPart.FindLeftSideOperandByPrecedence(Self);
  if ALeftSide = nil then begin
    SetError(Self, 'Internal parser error for operator '+GetText+': ');
    APrevPart.Free;
    exit;
  end;

  ALeftSide.ReplaceInParent(Self);
  Add(ALeftSide);
end;

procedure TFpPascalExpressionPartOperator.DoHandleEndOfExpression;
begin
  if not HasAllOperands then
    SetError(Self, 'Not enough operands')
  else
    inherited DoHandleEndOfExpression;
end;

function TFpPascalExpressionPartOperator.HandleSeparator(
  ASeparatorType: TSeparatorType; var APart: TFpPascalExpressionPart): Boolean;
begin
  Result := HasAllOperands and (inherited HandleSeparator(ASeparatorType, APart));
end;

function TFpPascalExpressionPartOperator.HandleNextPart(APart: TFpPascalExpressionPart): TFpPascalExpressionPart;
begin
  Result := Self;
  if HasAllOperands then begin
    Result := inherited HandleNextPart(APart);
    exit;
  end;
  if not IsValidNextPart(APart) then begin
    SetError(APart, 'Not possible after Operator '+GetText+': ');
    APart.Free;
    exit;
  end;

  Add(APart);
  Result := APart;
end;

{ TFpPascalExpressionPartUnaryOperator }

function TFpPascalExpressionPartUnaryOperator.HasAllOperands: Boolean;
begin
  Result := Count = 1;
end;

{ TFpPascalExpressionPartBinaryOperator }

function TFpPascalExpressionPartBinaryOperator.HasAllOperands: Boolean;
begin
  Result := Count = 2;
end;

function TFpPascalExpressionPartBinaryOperator.IsValidAfterPart(APrevPart: TFpPascalExpressionPart): Boolean;
begin
  Result := inherited IsValidAfterPart(APrevPart);
  if not Result then
    exit;

  Result := APrevPart.CanHaveOperatorAsNext;
  if Result then
    Result := IsValidAfterPartWithPrecedence(APrevPart);
end;

function TFpPascalExpressionPartBinaryOperator.IsValidAfterPartWithPrecedence(
  APrevPart: TFpPascalExpressionPart): Boolean;
begin
  Result := True;
  (*
   BinaryOperator...
     # (e.g. Self = "+")
     # APrevPart = Identifier
     # APrevPart.Parent = "*" or "-"
       foo * Identifier +
       foo - Identifier +
   The binary-op "+" after "Identifier" must be applied to the parent.
   So, if SELF is the "+", then it is not valid after "Identifier".
   If new operator has a higher precedence, it go down to the child again and replace it
  *)
  // precedence: 1 = highest
  Result := (APrevPart.Parent = nil) or not (APrevPart.Parent.HasPrecedence) or
            (Precedence < APrevPart.Parent.Precedence)
end;

function TFpPascalExpressionPartBinaryOperator.HandleNextPart(APart: TFpPascalExpressionPart
  ): TFpPascalExpressionPart;
begin
  if Count = 0 then
    SetError('Missing operand');
  Result := inherited HandleNextPart(APart);
end;

function TFpPascalExpressionPartBinaryOperator.MaybeHandlePrevPart(APrevPart: TFpPascalExpressionPart;
  var AResult: TFpPascalExpressionPart): Boolean;
begin
  Result := MaybeAddLeftOperand(APrevPart, AResult);
end;

{ TFpPascalExpressionPartOperatorAddressOf }

procedure TFpPascalExpressionPartOperatorAddressOf.Init;
begin
  FPrecedence := PRECEDENCE_ADRESS_OF;
  inherited Init;
end;

function TFpPascalExpressionPartOperatorAddressOf.DoGetResultValue: TFpValue;
var
  tmp: TFpValue;
begin
  Result := nil;
  if Count <> 1 then exit;

  tmp := Items[0].ResultValue;
  if (tmp = nil) or not IsTargetAddr(tmp.Address) then begin
    SetError(fpErrAnyError, []);
  // seterror / cant take address
    exit;
  end;

  Result := TFpPasParserValueAddressOf.Create(tmp, Items[0]);
  {$IFDEF WITH_REFCOUNT_DEBUG}Result.DbgRenameReference(nil, 'DoGetResultValue');{$ENDIF}
end;

{ TFpPascalExpressionPartOperatorMakeRef }

procedure TFpPascalExpressionPartOperatorMakeRef.Init;
begin
  FPrecedence := PRECEDENCE_MAKE_REF;
  inherited Init;
end;

function TFpPascalExpressionPartOperatorMakeRef.IsValidNextPart(APart: TFpPascalExpressionPart): Boolean;
begin
  if HasAllOperands then
    Result := (inherited IsValidNextPart(APart))
  else
    Result := (inherited IsValidNextPart(APart)) and
              ( (APart is TFpPascalExpressionPartIdentifier) or
                (APart is TFpPascalExpressionPartOperatorMakeRef)
              );
end;

function TFpPascalExpressionPartOperatorMakeRef.DoGetResultValue: TFpValue;
var
  tmp: TFpValue;
begin
  Result := nil;
  if Count <> 1 then exit;

  tmp := Items[0].ResultValue;
  if tmp = nil then
    exit;
  if tmp is TFpPasParserValueMakeReftype then begin
    TFpPasParserValueMakeReftype(tmp).IncRefLevel;
    Result := tmp;
    Result.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(nil, 'DoGetResultValue'){$ENDIF};
    exit;
  end;

  if (tmp.DbgSymbol = nil) or (tmp.DbgSymbol.SymbolType <> stType) then
    exit;

  Result := TFpPasParserValueMakeReftype.Create(tmp.DbgSymbol, Items[0]);
  {$IFDEF WITH_REFCOUNT_DEBUG}Result.DbgRenameReference(nil, 'DoGetResultValue'){$ENDIF};
end;

function TFpPascalExpressionPartOperatorMakeRef.DoGetIsTypeCast: Boolean;
begin
  Result := True;
end;

{ TFpPascalExpressionPartOperatorDeRef }

procedure TFpPascalExpressionPartOperatorDeRef.Init;
begin
  FPrecedence := PRECEDENCE_DEREF;
  inherited Init;
end;

function TFpPascalExpressionPartOperatorDeRef.DoGetResultValue: TFpValue;
var
  tmp: TFpValue;
  addr: TFpDbgMemLocation;
begin
  Result := nil;
  if Count <> 1 then exit;

  tmp := Items[0].ResultValue;
  if tmp = nil then
    exit;

  if tmp is TFpPasParserValueAddressOf then begin // TODO: remove IF, handled in GetMember
    Result := TFpPasParserValueAddressOf(tmp).PointedToValue;
    Result.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(nil, 'DoGetResultValue'){$ENDIF};
  end
  else
  if tmp.Kind = skPointer then begin
    if (svfDataAddress in tmp.FieldFlags) then begin
      addr := tmp.DerefAddress;
      if not IsAddress(addr) then begin
        SetErrorWithPos(fpErrCannotDeref_p, [Items[0].GetText(MAX_ERR_EXPR_QUOTE_LEN)]);
      end
      else
      if tmp.TypeInfo <> nil then begin
        Result := tmp.Member[0];
        if Result = nil then begin
          SetErrorWithPos(fpErrCannotDeref_p, [Items[0].GetText(MAX_ERR_EXPR_QUOTE_LEN)]);
          SetError(fpInternalErr, [], ExpressionData.Error); // mark as internal error
        end
        else
          {$IFDEF WITH_REFCOUNT_DEBUG}Result.DbgRenameReference(nil, 'DoGetResultValue'){$ENDIF};
      end
      else begin
        Result := TFpValueConstAddress.Create(addr);
        {$IFDEF WITH_REFCOUNT_DEBUG}Result.DbgRenameReference(nil, 'DoGetResultValue'){$ENDIF};
      end;
    end;
  end
  else
  if tmp is TFpValueConstNumber then begin
    addr := TargetLoc(tmp.AsCardinal);
    Result := TFpValueConstAddress.Create(addr);
    {$IFDEF WITH_REFCOUNT_DEBUG}Result.DbgRenameReference(nil, 'DoGetResultValue'){$ENDIF};
  end
  //if tmp.Kind = skArray then // dynarray
  else
  begin
    Result := nil;
    SetErrorWithPos(fpErrCannotDeref_p, [Items[0].GetText(MAX_ERR_EXPR_QUOTE_LEN)]);
  end;
end;

function TFpPascalExpressionPartOperatorDeRef.MaybeHandlePrevPart(APrevPart: TFpPascalExpressionPart;
  var AResult: TFpPascalExpressionPart): Boolean;
begin
  Result := MaybeAddLeftOperand(APrevPart, AResult);
end;

function TFpPascalExpressionPartOperatorDeRef.FindLeftSideOperandByPrecedence(AnOperator: TFpPascalExpressionPartWithPrecedence): TFpPascalExpressionPart;
begin
  Result := Self;
end;

function TFpPascalExpressionPartOperatorDeRef.IsValidAfterPart(APrevPart: TFpPascalExpressionPart): Boolean;
begin
  Result := inherited IsValidAfterPart(APrevPart);
  if not Result then
    exit;

  Result := APrevPart.CanHaveOperatorAsNext;

  // BinaryOperator...
  //   foo
  //   Identifier
  // "Identifier" can hane a binary-op next. But it must be applied to the parent.
  // So it is not valid here.
  // If new operator has a higher precedence, it go down to the child again and replace it
  if (APrevPart.Parent <> nil) and (APrevPart.Parent is TFpPascalExpressionPartOperator) then
    Result := False;
end;

{ TFpPascalExpressionPartOperatorUnaryPlusMinus }

procedure TFpPascalExpressionPartOperatorUnaryPlusMinus.Init;
begin
  FPrecedence := PRECEDENCE_UNARY_SIGN;
  inherited Init;
end;

function TFpPascalExpressionPartOperatorUnaryPlusMinus.DoGetResultValue: TFpValue;
var
  tmp1: TFpValue;
  IsAdd: Boolean;
begin
  Result := nil;
  if Count <> 1 then exit;
  assert((GetText = '+') or (GetText = '-'), 'TFpPascalExpressionPartOperatorUnaryPlusMinus.DoGetResultValue: (GetText = +) or (GetText = -)');

  tmp1 := Items[0].ResultValue;
  IsAdd := GetText = '+';
  if (tmp1 = nil) then exit;

  {$PUSH}{$R-}{$Q-}
  if IsAdd then begin
    case tmp1.Kind of
      skPointer: ;
      skInteger:  Result := tmp1;
      skCardinal: Result := tmp1;
      skFloat:    Result := tmp1;
    end;
    Result.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(nil, 'DoGetResultValue'){$ENDIF};
  end
  else begin
    case tmp1.Kind of
      skPointer: ;
      skInteger:  Result := TFpValueConstNumber.Create(-tmp1.AsInteger, True);
      skCardinal: Result := TFpValueConstNumber.Create(-tmp1.AsCardinal, True);
      skFloat:    Result := TFpValueConstFloat.Create(-tmp1.AsFloat);
    end;
    {$IFDEF WITH_REFCOUNT_DEBUG}if Result <> nil then Result.DbgRenameReference(nil, 'DoGetResultValue');{$ENDIF}
  end;
  {$POP}

  ForwardError(Result, tmp1);
end;

{ TFpPascalExpressionPartOperatorQuestionMark }

procedure TFpPascalExpressionPartOperatorQuestionMark.Init;
begin
  FPrecedence := PRECEDENCE_QUEST_COLON;
  inherited Init;
end;

function TFpPascalExpressionPartOperatorQuestionMark.DoGetResultValue: TFpValue;
var
  tmp: TFpValue;
  ff: TFpValueFieldFlags;
  b: Boolean;
begin
  Result := nil;
  if (Count <> 2) or not (Items[1] is TFpPascalExpressionPartOperatorColon) or
     (TFpPascalExpressionPartOperatorColon(Items[1]).Count <> 2)
  then begin
    SetError('internal error ?:');
    exit;
  end;

  tmp := Items[0].ResultValue;
  if (tmp = nil) then exit;
  ff := tmp.FieldFlags;
  if      (ff * [svfBoolean] <> [])               then b := tmp.AsBool
  else if (ff * [svfCardinal, svfOrdinal] <> [])  then b := tmp.AsCardinal <> 0
  else if (ff * [svfString, svfWideString] <> []) then b := Length(tmp.AsString) <> 0
  else begin
    SetError('"?" needs an input than can be cast to bool');
    exit;
  end;

  if b
  then Result := TFpPascalExpressionPartOperatorColon(Items[1]).Items[0].ResultValue
  else Result := TFpPascalExpressionPartOperatorColon(Items[1]).Items[1].ResultValue;

  if Result = nil then
    exit;
  Result.AddReference;
end;

function TFpPascalExpressionPartOperatorQuestionMark.FindLeftSideOperandByPrecedence(
  AnOperator: TFpPascalExpressionPartWithPrecedence): TFpPascalExpressionPart;
begin
  Result := Self;

  if (not HasAllOperands) or (LastItem = nil) then begin
    Result := nil;
    exit
  end;

  (* If precedence is equal, APart can be
    - a :  - we don't have one yet => Return from LastItem
           - we have a : => return self
    - a ?  - => Return from LastItem
  *)
  if (Precedence = AnOperator.Precedence) then begin
    if (Count = 2) and (Items[1] is TFpPascalExpressionPartOperatorColon)
      and (AnOperator is TFpPascalExpressionPartOperatorColon)
    then
      Result := Self
    else
      Result := LastItem.FindLeftSideOperandByPrecedence(AnOperator);
  end
  else
  // precedence: 1 = highest
  if Precedence >= AnOperator.Precedence then
    Result := LastItem.FindLeftSideOperandByPrecedence(AnOperator);
end;

function TFpPascalExpressionPartOperatorQuestionMark.IsValidAfterPartWithPrecedence(
  APrevPart: TFpPascalExpressionPart): Boolean;
begin
  Result := (APrevPart.Parent = nil) or not (APrevPart.Parent.HasPrecedence) or
            (Precedence <= APrevPart.Parent.Precedence);
  (* inherited only checks for "<" instead of "<="
     Other operators at the same precedence must have the 2nd (in left to right
     reading order) operator become the parent, so that it executes the entire
     left operation first.
     ? : act as nested constructs. The right part is a nested operation
     Therefore at same precedence, the new "?" is valid after any ongoing ? or :
  *)
end;

procedure TFpPascalExpressionPartOperatorQuestionMark.DoHandleEndOfExpression;
begin
  inherited DoHandleEndOfExpression;
  if (Count <> 2) or not(Items[1] is TFpPascalExpressionPartOperatorColon) then
    SetError('Missing ":"');
end;

function TFpPascalExpressionPartOperatorQuestionMark.ReturnsVariant: boolean;
begin
  Result := True; // TODO: compare types of each argument
end;

function TFpPascalExpressionPartOperatorQuestionMark.IsClosed: boolean;
begin
  Result := (Count = 2) and (Items[1] is TFpPascalExpressionPartOperatorColon);
end;

{ TFpPascalExpressionPartOperatorColon }

procedure TFpPascalExpressionPartOperatorColon.Init;
begin
  FPrecedence := PRECEDENCE_QUEST_COLON;
  inherited Init;
end;

function TFpPascalExpressionPartOperatorColon.DoGetResultValue: TFpValue;
begin
  raise Exception.Create('invalid call to ":"');
  Result := nil;
end;

function TFpPascalExpressionPartOperatorColon.FindLeftSideOperandByPrecedence(
  AnOperator: TFpPascalExpressionPartWithPrecedence): TFpPascalExpressionPart;
begin
  Result := Self;

  if (not HasAllOperands) or (LastItem = nil) then begin
    Result := nil;
    exit
  end;

  (* If precedence is equal
    - a : => must be outer => return self
    - a ? => LastItem
  *)

  if (Precedence = AnOperator.Precedence) then begin
    if AnOperator is TFpPascalExpressionPartOperatorQuestionMark then
      Result := LastItem.FindLeftSideOperandByPrecedence(AnOperator)
    else
      Result := Self;
  end
  else
  // precedence: 1 = highest
  if Precedence > AnOperator.Precedence then
    Result := LastItem.FindLeftSideOperandByPrecedence(AnOperator);
end;

function TFpPascalExpressionPartOperatorColon.IsValidAfterPartWithPrecedence(
  APrevPart: TFpPascalExpressionPart): Boolean;
var
  Prev: TFpPascalExpressionPartOperatorQuestionMark absolute APrevPart;
begin
  Result := (APrevPart.Parent = nil) or not (APrevPart.Parent.HasPrecedence) or
            (Precedence <= APrevPart.Parent.Precedence);
  if not Result then
    exit;

  // A colon is valid only after a questionmark that does not yet have its ":"
  // Otherwise it belongs to an outer question mark
  Result := (APrevPart is TFpPascalExpressionPartOperatorQuestionMark) and
            ( (Prev.Count = 2) and not(Prev.Items[1] is TFpPascalExpressionPartOperatorColon) )
end;

procedure TFpPascalExpressionPartOperatorColon.DoHandleEndOfExpression;
begin
  inherited DoHandleEndOfExpression;
  FIsClosed := True;
  if not(Parent is TFpPascalExpressionPartOperatorQuestionMark) then
    SetError('No "?" for ":"');
end;

function TFpPascalExpressionPartOperatorColon.IsClosed: boolean;
begin
  Result := FIsClosed;
end;

{ TFpPascalExpressionPartOperatorPlusMinus }

procedure TFpPascalExpressionPartOperatorPlusMinus.Init;
begin
  FPrecedence := PRECEDENCE_PLUS_MINUS;
  inherited Init;
end;

function TFpPascalExpressionPartOperatorPlusMinus.DoGetResultValue: TFpValue;
{$PUSH}{$R-}{$Q-}
  function AddSubValueToPointer(APointerVal, AOtherVal: TFpValue; ADoSubtract: Boolean = False): TFpValue;
  var
    Idx, m: Int64;
    TmpVal: TFpValue;
    s1, s2: TFpDbgValueSize;
  begin
    Result := nil;
    case AOtherVal.Kind of
      skPointer: if ADoSubtract then begin
          if ( (APointerVal.TypeInfo = nil) or (APointerVal.TypeInfo.TypeInfo = nil) ) and
             ( (AOtherVal.TypeInfo = nil)   or (AOtherVal.TypeInfo.TypeInfo = nil) )
          then begin
            Idx := APointerVal.AsCardinal - AOtherVal.AsCardinal;
            Result := TFpValueConstNumber.Create(Idx, True);
            exit;
          end
          else
          if (APointerVal.TypeInfo <> nil) and (APointerVal.TypeInfo.TypeInfo <> nil) and
             (AOtherVal.TypeInfo <> nil)   and (AOtherVal.TypeInfo.TypeInfo <> nil) and
             (APointerVal.TypeInfo.TypeInfo.Kind = AOtherVal.TypeInfo.TypeInfo.Kind) and
             (APointerVal.TypeInfo.TypeInfo.ReadSize(nil, s1)) and
             (AOtherVal.TypeInfo.TypeInfo.ReadSize(nil, s2)) and
             (s1 = s2)
          then begin
            TmpVal := APointerVal.Member[1];
            if (TmpVal = nil) or (s1 <> (TmpVal.Address.Address - APointerVal.DerefAddress.Address)) then begin
              TmpVal.ReleaseReference;
              debugln(DBG_WARNINGS, 'Size mismatch for pointer math');
              exit;
            end;
            TmpVal.ReleaseReference;
            Idx := APointerVal.AsCardinal - AOtherVal.AsCardinal;
            if SizeToFullBytes(s1) > 0 then begin
              m := Idx mod SizeToFullBytes(s1);
              Idx := Idx div SizeToFullBytes(s1);
              if m <> 0 then begin
                debugln(DBG_WARNINGS, 'Size mismatch for pointer math');
                exit;
              end;
            end;

            Result := TFpValueConstNumber.Create(Idx, True);
            exit;
          end
          else
            exit;
        end
        else
          exit;
      skInteger:  Idx := AOtherVal.AsInteger;
      skCardinal: begin
          Idx := AOtherVal.AsInteger;
          if Idx > High(Int64) then
            exit; // TODO: error
        end;
      else
        exit; // TODO: error
    end;
    if ADoSubtract then begin
      if Idx < -(High(Int64)) then
        exit; // TODO: error
      Idx := -Idx;
    end;
    TmpVal := APointerVal.Member[Idx];
    if IsError(APointerVal.LastError) or (TmpVal = nil) then begin
      SetError('Error dereferencing'); // TODO: set correct error
      exit;
    end;
    Result := TFpPasParserValueAddressOf.Create(TmpVal, Self);
    TmpVal.ReleaseReference;
  end;
  function AddValueToInt(AIntVal, AOtherVal: TFpValue): TFpValue;
  begin
    Result := nil;
    case AOtherVal.Kind of
      skPointer:  Result := AddSubValueToPointer(AOtherVal, AIntVal);
      skInteger:  Result := TFpValueConstNumber.Create(AIntVal.AsInteger + AOtherVal.AsInteger, True);
      skCardinal: Result := TFpValueConstNumber.Create(AIntVal.AsInteger + AOtherVal.AsCardinal, True);
      skFloat:    Result := TFpValueConstFloat.Create(AIntVal.AsInteger + AOtherVal.AsFloat);
      else SetError('Addition not supported');
    end;
  end;
  function AddValueToCardinal(ACardinalVal, AOtherVal: TFpValue): TFpValue;
  begin
    Result := nil;
    case AOtherVal.Kind of
      skPointer:  Result := AddSubValueToPointer(AOtherVal, ACardinalVal);
      skInteger:  Result := TFpValueConstNumber.Create(ACardinalVal.AsCardinal + AOtherVal.AsInteger, True);
      skCardinal: Result := TFpValueConstNumber.Create(ACardinalVal.AsCardinal + AOtherVal.AsCardinal, False);
      skFloat:    Result := TFpValueConstFloat.Create(ACardinalVal.AsCardinal + AOtherVal.AsFloat);
      else SetError('Addition not supported');
    end;
  end;
  function AddValueToFloat(AFloatVal, AOtherVal: TFpValue): TFpValue;
  begin
    Result := nil;
    case AOtherVal.Kind of
      skInteger:  Result := TFpValueConstFloat.Create(AFloatVal.AsFloat + AOtherVal.AsInteger);
      skCardinal: Result := TFpValueConstFloat.Create(AFloatVal.AsFloat + AOtherVal.AsCardinal);
      skFloat:    Result := TFpValueConstFloat.Create(AFloatVal.AsFloat + AOtherVal.AsFloat);
      else SetError('Addition not supported');
    end;
  end;
  function ConcateCharData(ACharVal, AOtherVal: TFpValue): TFpValue;
  begin
    Result := nil;
    if AOtherVal.FieldFlags * [svfString, svfWideString] <> [] then
      Result := TFpValueConstString.Create(ACharVal.AsString + AOtherVal.AsString)
    else
      SetError('Operation + not supported');
  end;
  function AddSets(ASetVal, AOtherVal: TFpValue): TFpValue;
  var
    i, j: Integer;
    m, m2: TFpValue;
    f: TFpValueFieldFlags;
    r: Boolean;
  begin
    Result := nil;
    case AOtherVal.Kind of
      skSet: begin
        Result := TFpValueConstSet.Create;
        for i := 0 to ASetVal.MemberCount - 1 do begin
          m := ASetVal.Member[i];
          TFpValueConstSet(Result).AddVal(m);
          m.ReleaseReference;
        end;

        for i := 0 to AOtherVal.MemberCount - 1 do begin
          m := AOtherVal.Member[i];
          j := ASetVal.MemberCount - 1;
          while j >= 0 do begin
            m2 := ASetVal.Member[j];
            f := m.FieldFlags * m2.FieldFlags;
            if svfOrdinal in f then
              r := m.AsCardinal = m2.AsCardinal
            else
            if svfIdentifier in f then
              r := m.AsString = m2.AsString
            else
              r := False;
            m2.ReleaseReference;
            if r then
              break;
            dec(j);
          end;

          if j < 0 then
            TFpValueConstSet(Result).AddVal(m);
          m.ReleaseReference;
        end;
      end;
      else SetError('Operator +: set union requires a set as 2nd operator');
    end;
  end;

  function SubPointerFromValue(APointerVal, AOtherVal: TFpValue): TFpValue;
  begin
    Result := nil;       // Error
  end;
  function SubValueFromInt(AIntVal, AOtherVal: TFpValue): TFpValue;
  begin
    Result := nil;
    case AOtherVal.Kind of
      skPointer:  Result := SubPointerFromValue(AOtherVal, AIntVal);
      skInteger:  Result := TFpValueConstNumber.Create(AIntVal.AsInteger - AOtherVal.AsInteger, True);
      skCardinal: Result := TFpValueConstNumber.Create(AIntVal.AsInteger - AOtherVal.AsCardinal, True);
      skFloat:    Result := TFpValueConstFloat.Create(AIntVal.AsInteger - AOtherVal.AsFloat);
      else SetError('Subtraction not supported');
    end;
  end;
  function SubValueFromCardinal(ACardinalVal, AOtherVal: TFpValue): TFpValue;
  begin
    Result := nil;
    case AOtherVal.Kind of
      skPointer:  Result := SubPointerFromValue(AOtherVal, ACardinalVal);
      skInteger:  Result := TFpValueConstNumber.Create(ACardinalVal.AsCardinal - AOtherVal.AsInteger, True);
      skCardinal: Result := TFpValueConstNumber.Create(ACardinalVal.AsCardinal - AOtherVal.AsCardinal, False);
      skFloat:    Result := TFpValueConstFloat.Create(ACardinalVal.AsCardinal - AOtherVal.AsFloat);
      else SetError('Subtraction not supported');
    end;
  end;
  function SubValueFromFloat(AFloatVal, AOtherVal: TFpValue): TFpValue;
  begin
    Result := nil;
    case AOtherVal.Kind of
      skInteger:  Result := TFpValueConstFloat.Create(AFloatVal.AsFloat - AOtherVal.AsInteger);
      skCardinal: Result := TFpValueConstFloat.Create(AFloatVal.AsFloat - AOtherVal.AsCardinal);
      skFloat:    Result := TFpValueConstFloat.Create(AFloatVal.AsFloat - AOtherVal.AsFloat);
      else SetError('Subtraction not supported');
    end;
  end;
  function SubtractSets(ASetVal, AOtherVal: TFpValue): TFpValue;
  var
    i, j: Integer;
    m, m2: TFpValue;
    f: TFpValueFieldFlags;
    r: Boolean;
  begin
    Result := nil;
    case AOtherVal.Kind of
      skSet: begin
        Result := TFpValueConstSet.Create;
        for i := 0 to ASetVal.MemberCount - 1 do begin
          m := ASetVal.Member[i];
          j := AOtherVal.MemberCount - 1;
          while j >= 0 do begin
            m2 := AOtherVal.Member[j];
            f := m.FieldFlags * m2.FieldFlags;
            if svfOrdinal in f then
              r := m.AsCardinal = m2.AsCardinal
            else
            if svfIdentifier in f then
              r := m.AsString = m2.AsString
            else
              r := False;
            m2.ReleaseReference;
            if r then
              break;
            dec(j);
          end;

          if j < 0 then
            TFpValueConstSet(Result).AddVal(m);
          m.ReleaseReference;
        end;
      end;
      else SetError('Operator -: set diff requires a set as 2nd operator');
    end;
  end;
{$POP}
var
  tmp1, tmp2: TFpValue;
  IsAdd: Boolean;
begin
  Result := nil;
  if Count <> 2 then exit;
  assert((GetText = '+') or (GetText = '-'), 'TFpPascalExpressionPartOperatorUnaryPlusMinus.DoGetResultValue: (GetText = +) or (GetText = -)');

  tmp1 := Items[0].ResultValue;
  tmp2 := Items[1].ResultValue;
  IsAdd := GetText = '+';
  if (tmp1 = nil) or (tmp2 = nil) then exit;

  if IsAdd then begin
    case tmp1.Kind of
      skInteger:  Result := AddValueToInt(tmp1, tmp2);
      skCardinal: Result := AddValueToCardinal(tmp1, tmp2);
      skFloat:    Result := AddValueToFloat(tmp1, tmp2);
      skPointer: begin
                  // Pchar can concatenate with String. But not with other Pchar
                  // Maybe allow optional: This does limit undetected/mis-detected strings
                  if (tmp1.FieldFlags * [svfString, svfWideString] <> []) and
                     (tmp2.Kind in [skString, skAnsiString, skWideString, skChar{, skWideChar}])
                  then
                    Result := ConcateCharData(tmp1, tmp2)
                  else
                    Result := AddSubValueToPointer(tmp1, tmp2);
                 end;
      skString, skAnsiString, skWideString, skChar{, skWideChar}:
                  Result := ConcateCharData(tmp1, tmp2);
      skSet:
                  Result := AddSets(tmp1, tmp2);
    end;
  end
  else begin
    case tmp1.Kind of
      skPointer:  Result := AddSubValueToPointer(tmp1, tmp2, True);
      skInteger:  Result := SubValueFromInt(tmp1, tmp2);
      skCardinal: Result := SubValueFromCardinal(tmp1, tmp2);
      skFloat:    Result := SubValueFromFloat(tmp1, tmp2);
      skSet:
                  Result := SubtractSets(tmp1, tmp2);
    end;
  end;
  ForwardError(Result, tmp1);
  ForwardError(Result, tmp2);

 {$IFDEF WITH_REFCOUNT_DEBUG}if Result <> nil then
   Result.DbgRenameReference(nil, 'DoGetResultValue');{$ENDIF}
end;

{ TFpPascalExpressionPartOperatorMulDiv }

procedure TFpPascalExpressionPartOperatorMulDiv.Init;
begin
  FPrecedence := PRECEDENCE_MUL_DIV;
  inherited Init;
end;

function TFpPascalExpressionPartOperatorMulDiv.DoGetResultValue: TFpValue;
{$PUSH}{$R-}{$Q-}
  function MultiplyIntWithValue(AIntVal, AOtherVal: TFpValue): TFpValue;
  begin
    Result := nil;
    case AOtherVal.Kind of
      skInteger:  Result := TFpValueConstNumber.Create(AIntVal.AsInteger * AOtherVal.AsInteger, True);
      skCardinal: Result := TFpValueConstNumber.Create(AIntVal.AsInteger * AOtherVal.AsCardinal, True);
      skFloat:    Result := TFpValueConstFloat.Create(AIntVal.AsInteger * AOtherVal.AsFloat);
      else SetError('Multiply not supported');
    end;
  end;
  function MultiplyCardinalWithValue(ACardinalVal, AOtherVal: TFpValue): TFpValue;
  begin
    Result := nil;
    case AOtherVal.Kind of
      skInteger:  Result := TFpValueConstNumber.Create(ACardinalVal.AsCardinal * AOtherVal.AsInteger, True);
      skCardinal: Result := TFpValueConstNumber.Create(ACardinalVal.AsCardinal * AOtherVal.AsCardinal, False);
      skFloat:    Result := TFpValueConstFloat.Create(ACardinalVal.AsCardinal * AOtherVal.AsFloat);
      else SetError('Multiply not supported');
    end;
  end;
  function MultiplyFloatWithValue(AFloatVal, AOtherVal: TFpValue): TFpValue;
  begin
    Result := nil;
    case AOtherVal.Kind of
      skInteger:  Result := TFpValueConstFloat.Create(AFloatVal.AsFloat * AOtherVal.AsInteger);
      skCardinal: Result := TFpValueConstFloat.Create(AFloatVal.AsFloat * AOtherVal.AsCardinal);
      skFloat:    Result := TFpValueConstFloat.Create(AFloatVal.AsFloat * AOtherVal.AsFloat);
      else SetError('Multiply not supported');
    end;
  end;
  function MultiplySets(ASetVal, AOtherVal: TFpValue): TFpValue;
  var
    i, j: Integer;
    m, m2: TFpValue;
    f: TFpValueFieldFlags;
    r: Boolean;
  begin
    Result := nil;
    case AOtherVal.Kind of
      skSet: begin
        Result := TFpValueConstSet.Create;
        for i := 0 to ASetVal.MemberCount - 1 do begin
          m := ASetVal.Member[i];
          j := AOtherVal.MemberCount - 1;
          while j >= 0 do begin
            m2 := AOtherVal.Member[j];
            f := m.FieldFlags * m2.FieldFlags;
            if svfOrdinal in f then
              r := m.AsCardinal = m2.AsCardinal
            else
            if svfIdentifier in f then
              r := m.AsString = m2.AsString
            else
              r := False;
            m2.ReleaseReference;
            if r then
              break;
            dec(j);
          end;

          if j >= 0 then
            TFpValueConstSet(Result).AddVal(m);
          m.ReleaseReference;
        end;
      end;
      else SetError('Operator *: set intersection requires a set as 2nd operator');
    end;
  end;

  function FloatDivIntByValue(AIntVal, AOtherVal: TFpValue): TFpValue;
  begin
    Result := nil;
    case AOtherVal.Kind of
      skInteger:  Result := TFpValueConstFloat.Create(AIntVal.AsInteger / AOtherVal.AsInteger);
      skCardinal: Result := TFpValueConstFloat.Create(AIntVal.AsInteger / AOtherVal.AsCardinal);
      skFloat:    Result := TFpValueConstFloat.Create(AIntVal.AsInteger / AOtherVal.AsFloat);
      else SetError('/ not supported');
    end;
  end;
  function FloatDivCardinalByValue(ACardinalVal, AOtherVal: TFpValue): TFpValue;
  begin
    Result := nil;
    case AOtherVal.Kind of
      skInteger:  Result := TFpValueConstFloat.Create(ACardinalVal.AsCardinal / AOtherVal.AsInteger);
      skCardinal: Result := TFpValueConstFloat.Create(ACardinalVal.AsCardinal / AOtherVal.AsCardinal);
      skFloat:    Result := TFpValueConstFloat.Create(ACardinalVal.AsCardinal / AOtherVal.AsFloat);
      else SetError('/ not supported');
    end;
  end;
  function FloatDivFloatByValue(AFloatVal, AOtherVal: TFpValue): TFpValue;
  begin
    Result := nil;
    case AOtherVal.Kind of
      skInteger:  Result := TFpValueConstFloat.Create(AFloatVal.AsFloat / AOtherVal.AsInteger);
      skCardinal: Result := TFpValueConstFloat.Create(AFloatVal.AsFloat / AOtherVal.AsCardinal);
      skFloat:    Result := TFpValueConstFloat.Create(AFloatVal.AsFloat / AOtherVal.AsFloat);
      else SetError('/ not supported');
    end;
  end;

  function NumDivIntByValue(AIntVal, AOtherVal: TFpValue): TFpValue;
  begin
    Result := nil;
    if (AOtherVal.AsInteger = 0) then begin
      if (IsError(AOtherVal.LastError)) then
        Result := TFpValueConstNumber.Create(0) // just for the error
      else
        SetError('Division by zero');
    end
    else
    case AOtherVal.Kind of
      skInteger:  Result := TFpValueConstNumber.Create(AIntVal.AsInteger div AOtherVal.AsInteger, True);
      skCardinal: Result := TFpValueConstNumber.Create(AIntVal.AsInteger div AOtherVal.AsCardinal, True);
      else SetError('Div not supported');
    end;
  end;
  function NumDivCardinalByValue(ACardinalVal, AOtherVal: TFpValue): TFpValue;
  begin
    Result := nil;
    if (AOtherVal.AsInteger = 0) then begin
      if (IsError(AOtherVal.LastError)) then
        Result := TFpValueConstNumber.Create(0) // just for the error
      else
        SetError('Division by zero');
    end
    else
    case AOtherVal.Kind of
      skInteger:  Result := TFpValueConstNumber.Create(ACardinalVal.AsCardinal div AOtherVal.AsInteger, True);
      skCardinal: Result := TFpValueConstNumber.Create(ACardinalVal.AsCardinal div AOtherVal.AsCardinal, False);
      else SetError('Div not supported');
    end;
  end;

  function NumModIntByValue(AIntVal, AOtherVal: TFpValue): TFpValue;
  begin
    Result := nil;
    if (AOtherVal.AsInteger = 0) then begin
      if (IsError(AOtherVal.LastError)) then
        Result := TFpValueConstNumber.Create(0) // just for the error
      else
        SetError('Modulo by zero')
    end
    else
    case AOtherVal.Kind of
      skInteger:  Result := TFpValueConstNumber.Create(AIntVal.AsInteger mod AOtherVal.AsInteger, True);
      skCardinal: Result := TFpValueConstNumber.Create(AIntVal.AsInteger mod AOtherVal.AsCardinal, True);
      else SetError('Div not supported');
    end;
  end;
  function NumModCardinalByValue(ACardinalVal, AOtherVal: TFpValue): TFpValue;
  begin
    Result := nil;
    if (AOtherVal.AsInteger = 0) then begin
      if (IsError(AOtherVal.LastError)) then
        Result := TFpValueConstNumber.Create(0) // just for the error
      else
        SetError('Modulo by zero')
    end
    else
    case AOtherVal.Kind of
      skInteger:  Result := TFpValueConstNumber.Create(ACardinalVal.AsCardinal mod AOtherVal.AsInteger, True);
      skCardinal: Result := TFpValueConstNumber.Create(ACardinalVal.AsCardinal mod AOtherVal.AsCardinal, False);
      else SetError('Mod not supported');
    end;
  end;
{$POP}
var
  tmp1, tmp2: TFpValue;
begin
  Result := nil;
  if Count <> 2 then exit;

  tmp1 := Items[0].ResultValue;
  tmp2 := Items[1].ResultValue;
  if (tmp1 = nil) or (tmp2 = nil) then exit;

  if GetText = '*' then begin
    case tmp1.Kind of
      skInteger:  Result := MultiplyIntWithValue(tmp1, tmp2);
      skCardinal: Result := MultiplyCardinalWithValue(tmp1, tmp2);
      skFloat:    Result := MultiplyFloatWithValue(tmp1, tmp2);
      skSet:      Result := MultiplySets(tmp1, tmp2);
    end;
  end
  else
  if GetText = '/' then begin
    case tmp1.Kind of
      skInteger:  Result := FloatDivIntByValue(tmp1, tmp2);
      skCardinal: Result := FloatDivCardinalByValue(tmp1, tmp2);
      skFloat:    Result := FloatDivFloatByValue(tmp1, tmp2);
    end;
  end
  else
  if CompareText(GetText, 'div') = 0 then begin
    case tmp1.Kind of
      skInteger:  Result := NumDivIntByValue(tmp1, tmp2);
      skCardinal: Result := NumDivCardinalByValue(tmp1, tmp2);
    end;
  end
  else
  if CompareText(GetText, 'mod') = 0 then begin
    case tmp1.Kind of
      skInteger:  Result := NumModIntByValue(tmp1, tmp2);
      skCardinal: Result := NumModCardinalByValue(tmp1, tmp2);
    end;
  end;

  ForwardError(Result, tmp1);
  ForwardError(Result, tmp2);

 {$IFDEF WITH_REFCOUNT_DEBUG}if Result <> nil then
   Result.DbgRenameReference(nil, 'DoGetResultValue');{$ENDIF}
end;

{ TFpPascalExpressionPartOperatorUnaryNot }

procedure TFpPascalExpressionPartOperatorUnaryNot.Init;
begin
  FPrecedence := PRECEDENCE_UNARY_NOT;
  inherited Init;
end;

function TFpPascalExpressionPartOperatorUnaryNot.DoGetResultValue: TFpValue;
var
  tmp1: TFpValue;
begin
  Result := nil;
  if Count <> 1 then exit;

  tmp1 := Items[0].ResultValue;
  if (tmp1 = nil) then exit;

  {$PUSH}{$R-}{$Q-}
  case tmp1.Kind of
    skInteger: Result := TFpValueConstNumber.Create(not tmp1.AsInteger, True);
    skCardinal: Result := TFpValueConstNumber.Create(not tmp1.AsCardinal, False);
    skBoolean: Result := TFpValueConstBool.Create(not tmp1.AsBool);
  end;
  {$POP}
  ForwardError(Result, tmp1);

 {$IFDEF WITH_REFCOUNT_DEBUG}if Result <> nil then Result.DbgRenameReference(nil, 'DoGetResultValue');{$ENDIF}
end;

{ TFpPascalExpressionPartOperatorAnd }

procedure TFpPascalExpressionPartOperatorAnd.Init;
begin
  FPrecedence := PRECEDENCE_AND;
  inherited Init;
end;

function TFpPascalExpressionPartOperatorAnd.DoGetResultValue: TFpValue;
var
  tmp1, tmp2: TFpValue;
begin
  Result := nil;
  if Count <> 2 then exit;

  tmp1 := Items[0].ResultValue;
  tmp2 := Items[1].ResultValue;
  if (tmp1 = nil) or (tmp2 = nil) then exit;

  {$PUSH}{$R-}{$Q-}
  case tmp1.Kind of
    skInteger: if tmp2.Kind = skInteger then
                 Result := TFpValueConstNumber.Create(tmp1.AsInteger AND tmp2.AsInteger, True)
               else
                  Result := TFpValueConstNumber.Create(tmp1.AsCardinal AND tmp2.AsCardinal, False);
    skCardinal: if tmp2.Kind in [skInteger, skCardinal] then
                  Result := TFpValueConstNumber.Create(tmp1.AsCardinal AND tmp2.AsCardinal, False);
    skBoolean: if tmp2.Kind = skBoolean then
                 {$PUSH}{$BOOLEVAL on}
                 Result := TFpValueConstBool.Create(tmp1.AsBool AND tmp2.AsBool);
                 {$POP}
  end;
  {$POP}
  ForwardError(Result, tmp1);
  ForwardError(Result, tmp2);

 {$IFDEF WITH_REFCOUNT_DEBUG}if Result <> nil then Result.DbgRenameReference(nil, 'DoGetResultValue');{$ENDIF}
end;

{ TFpPascalExpressionPartOperatorBitShift }

procedure TFpPascalExpressionPartOperatorBitShift.Init;
begin
  FPrecedence := PRECEDENCE_BIT_SHIFT;
  inherited Init;
end;

function TFpPascalExpressionPartOperatorBitShift.CheckOperators(out AVal,
  AShift: QWord): boolean;
var
  tmp1, tmp2: TFpValue;
begin
  Result := False;

  if Count <> 2 then
    exit;

  tmp1 := Items[0].ResultValue;
  tmp2 := Items[1].ResultValue;
  if (tmp1 = nil) or (tmp2 = nil) then
    exit;

  if not (tmp1.Kind in [skInteger, skCardinal]) then begin
    SetError(GetText + ' requires a numeric value as first operand');
    exit;
  end;

  if not (tmp2.Kind in [skInteger, skCardinal]) then begin
    SetError(GetText + ' requires a numeric value as second operand');
    exit;
  end;

  AVal := tmp1.AsCardinal;
  AShift := tmp2.AsCardinal;

  Result := True;
end;

{ TFpPascalExpressionPartOperatorShr }

function TFpPascalExpressionPartOperatorShr.DoGetResultValue: TFpValue;
var
  AVal, AShift: QWord;
begin
  Result := nil;
  if not CheckOperators(AVal, AShift) then
    exit;

  {$PUSH}{$R-}{$Q-}
  Result := TFpValueConstNumber.Create(AVal >> AShift, False);
  {$POP}

  ForwardError(Result, Items[0].ResultValue);
  ForwardError(Result, Items[1].ResultValue);

 {$IFDEF WITH_REFCOUNT_DEBUG}if Result <> nil then Result.DbgRenameReference(nil, 'DoGetResultValue');{$ENDIF}
end;

{ TFpPascalExpressionPartOperatorShl }

function TFpPascalExpressionPartOperatorShl.DoGetResultValue: TFpValue;
var
  AVal, AShift: QWord;
begin
  Result := nil;
  if not CheckOperators(AVal, AShift) then
    exit;

  {$PUSH}{$R-}{$Q-}
  Result := TFpValueConstNumber.Create(AVal << AShift, False);
  {$POP}

  ForwardError(Result, Items[0].ResultValue);
  ForwardError(Result, Items[1].ResultValue);

 {$IFDEF WITH_REFCOUNT_DEBUG}if Result <> nil then Result.DbgRenameReference(nil, 'DoGetResultValue');{$ENDIF}
end;

{ TFpPascalExpressionPartOperatorOr }

procedure TFpPascalExpressionPartOperatorOr.Init;
begin
  FPrecedence := PRECEDENCE_OR;
  inherited Init;
end;

function TFpPascalExpressionPartOperatorOr.DoGetResultValue: TFpValue;
var
  tmp1, tmp2: TFpValue;
begin
  Result := nil;
  if Count <> 2 then exit;

  tmp1 := Items[0].ResultValue;
  tmp2 := Items[1].ResultValue;
  if (tmp1 = nil) or (tmp2 = nil) then exit;

  {$PUSH}{$R-}{$Q-}
  case FOp of
    ootOr:
    case tmp1.Kind of
      skInteger: if tmp2.Kind in [skInteger, skCardinal] then
                   Result := TFpValueConstNumber.Create(tmp1.AsInteger OR tmp2.AsInteger, True);
      skCardinal: if tmp2.Kind = skInteger then
                    Result := TFpValueConstNumber.Create(tmp1.AsInteger OR tmp2.AsInteger, True)
                  else
                  if tmp2.Kind = skCardinal then
                    Result := TFpValueConstNumber.Create(tmp1.AsInteger OR tmp2.AsInteger, False);
      skBoolean: if tmp2.Kind = skBoolean then
                   {$PUSH}{$BOOLEVAL on}
                   Result := TFpValueConstBool.Create(tmp1.AsBool OR tmp2.AsBool);
                   {$POP}
    end;
    ootXor:
    case tmp1.Kind of
      skInteger: if tmp2.Kind in [skInteger, skCardinal] then
                   Result := TFpValueConstNumber.Create(tmp1.AsInteger XOR tmp2.AsInteger, True);
      skCardinal: if tmp2.Kind = skInteger then
                    Result := TFpValueConstNumber.Create(tmp1.AsInteger XOR tmp2.AsInteger, True)
                  else
                  if tmp2.Kind = skCardinal then
                    Result := TFpValueConstNumber.Create(tmp1.AsInteger XOR tmp2.AsInteger, False);
      skBoolean: if tmp2.Kind = skBoolean then
                   {$PUSH}{$BOOLEVAL on}
                   Result := TFpValueConstBool.Create(tmp1.AsBool XOR tmp2.AsBool);
                   {$POP}
    end;
  end;
  {$POP}
  ForwardError(Result, tmp1);
  ForwardError(Result, tmp2);

 {$IFDEF WITH_REFCOUNT_DEBUG}if Result <> nil then Result.DbgRenameReference(nil, 'DoGetResultValue');{$ENDIF}
end;

procedure TFpPascalExpressionPartOperatorOr.Assign(ASourcePart: TFpPascalExpressionPart);
begin
  inherited Assign(ASourcePart);
  if ASourcePart is TFpPascalExpressionPartOperatorOr then
    FOp := TFpPascalExpressionPartOperatorOr(ASourcePart).FOp;
end;

constructor TFpPascalExpressionPartOperatorOr.Create(
  AnExpressionData: TFpPascalExpressionSharedData; AnOp: TOpOrType; AStartChar: PChar;
  AnEndChar: PChar);
begin
  inherited Create(AnExpressionData, AStartChar, AnEndChar);
  FOp := AnOp;
end;

{ TFpPascalExpressionPartOperatorCompare }

procedure TFpPascalExpressionPartOperatorCompare.Init;
begin
  FPrecedence := PRECEDENCE_COMPARE;
  inherited Init;
end;

function TFpPascalExpressionPartOperatorCompare.DoGetResultValue: TFpValue;
{$PUSH}{$R-}{$Q-}
  function IntEqualToValue(AIntVal, AOtherVal: TFpValue; AReverse: Boolean = False): TFpValue;
  begin
    Result := nil;
    case AOtherVal.Kind of
      skInteger:  Result := TFpValueConstBool.Create((AIntVal.AsInteger = AOtherVal.AsInteger) xor AReverse);
      skCardinal: Result := TFpValueConstBool.Create((AIntVal.AsInteger = AOtherVal.AsCardinal) xor AReverse);
      skFloat:    Result := TFpValueConstBool.Create((AIntVal.AsInteger = AOtherVal.AsFloat) xor AReverse);
      skPointer, skAddress:
                  Result := TFpValueConstBool.Create((AIntVal.AsCardinal = AOtherVal.AsCardinal) xor AReverse)
      else SetError('= not supported');
    end;
  end;
  function CardinalEqualToValue(ACardinalVal, AOtherVal: TFpValue; AReverse: Boolean = False): TFpValue;
  begin
    Result := nil;
    case AOtherVal.Kind of
      skInteger:  Result := TFpValueConstBool.Create((ACardinalVal.AsCardinal = AOtherVal.AsInteger) xor AReverse);
      skCardinal: Result := TFpValueConstBool.Create((ACardinalVal.AsCardinal = AOtherVal.AsCardinal) xor AReverse);
      skFloat:    Result := TFpValueConstBool.Create((ACardinalVal.AsCardinal = AOtherVal.AsFloat) xor AReverse);
      skPointer, skAddress:
                  Result := TFpValueConstBool.Create((ACardinalVal.AsCardinal = AOtherVal.AsCardinal) xor AReverse)
      else SetError('= not supported');
    end;
  end;
  function FloatEqualToValue(AFloatVal, AOtherVal: TFpValue; AReverse: Boolean = False): TFpValue;
  begin
    Result := nil;
    case AOtherVal.Kind of
      skInteger:  Result := TFpValueConstBool.Create((AFloatVal.AsFloat = AOtherVal.AsInteger) xor AReverse);
      skCardinal: Result := TFpValueConstBool.Create((AFloatVal.AsFloat = AOtherVal.AsCardinal) xor AReverse);
      skFloat:    Result := TFpValueConstBool.Create((AFloatVal.AsFloat = AOtherVal.AsFloat) xor AReverse);
      else SetError('= not supported');
    end;
  end;
  function AddressPtrEqualToValue(AIntVal, AOtherVal: TFpValue; AReverse: Boolean = False): TFpValue;
  begin
    Result := nil;
    if (AOtherVal.Kind in [skClass,skInterface,skAddress,skPointer]) or
       ((AIntVal.Kind in [skPointer, skAddress]) and (AOtherVal.Kind in [skInteger,skCardinal]))
    then
      Result := TFpValueConstBool.Create((AIntVal.AsCardinal = AOtherVal.AsCardinal) xor AReverse)
    else
      SetError('= not supported');
  end;
  function CharDataEqualToValue(ACharVal, AOtherVal: TFpValue; AReverse: Boolean = False): TFpValue;
  begin
    Result := nil;
    if (AOtherVal.FieldFlags * [svfString, svfWideString] <> []) then
      Result := TFpValueConstBool.Create((ACharVal.AsString = AOtherVal.AsString) xor AReverse)
    else
      SetError('= not supported');
  end;
  function SetEqual(ASetVal, AOtherVal: TFpValue; AReverse: Boolean = False): TFpValue;
  var
    r: TStringList;
    i: Integer;
    m: TFpValue;
    s: String;
  begin
    Result := nil;
    if AOtherVal.Kind <> skSet then begin
      SetError(GetText + ' not supported');
      exit;
    end;

    r := TStringList.Create;
    try
      r.CaseSensitive := False;
      for i := 0 to ASetVal.MemberCount - 1 do begin
        m := ASetVal.Member[i];
        s := m.AsString;
        m.ReleaseReference;
        if r.IndexOf(s) < 0 then
          r.Add(s);
      end;
      r.Sorted := True;

      for i := 0 to AOtherVal.MemberCount - 1 do begin
        m := AOtherVal.Member[i];
        s := m.AsString;
        m.ReleaseReference;
        if r.IndexOf(s) < 0 then begin
          Result := TFpValueConstBool.Create(AReverse);
          exit;
        end;
      end;

      Result := TFpValueConstBool.Create(not AReverse);
    finally
      r.Free;
    end;
  end;
  function BoolEqual(ABoolVal, AOtherVal: TFpValue; AReverse: Boolean = False): TFpValue;
  begin
    Result := nil;
    if (AOtherVal.Kind = skBoolean) then
      Result := TFpValueConstBool.Create((ABoolVal.AsBool = AOtherVal.AsBool) xor AReverse)
    else
      SetError('= not supported');
  end;
  function CheckEnumCompatible(AName: String; AExpVal: Integer; AnEnum: TFpValue): boolean;
  var
    m, t: TFpSymbol;
  begin
    Result := AName = ''; // un-named / maybe invalid ordinal value
    if Result then
      exit;

    t := AnEnum.TypeInfo;
    Result := t = nil;
    if Result then // TODO: TFpValueDwarfEnumMember currently does not have type info
      exit;

    m := t.NestedSymbolByName[AName];
    Result := m <> nil;
    if not Result then
      exit;
    Result := m.HasOrdinalValue and (m.OrdinalValue = AExpVal);
  end;
  function EnumEqual(AnEnumVal, AOtherVal: TFpValue; AReverse: Boolean = False): TFpValue;
  begin
    Result := nil;
    if (AOtherVal.Kind in [skEnum, skEnumValue]) and
       (AnEnumVal.FieldFlags * [svfInteger, svfCardinal, svfOrdinal] <> []) and
       (AOtherVal.FieldFlags * [svfInteger, svfCardinal, svfOrdinal] <> [])
    then begin
      if CheckEnumCompatible(AnEnumVal.AsString, AnEnumVal.AsInteger, AOtherVal) and
         CheckEnumCompatible(AOtherVal.AsString, AOtherVal.AsInteger, AnEnumVal)
      then
        Result := TFpValueConstBool.Create((AnEnumVal.AsInteger = AOtherVal.AsInteger) xor AReverse)
      else
        SetError('type mismatch between enum');
    end
    else
      SetError('= not supported');
  end;

  function IntGreaterThanValue(AIntVal, AOtherVal: TFpValue; AReverse: Boolean = False): TFpValue;
  begin
    Result := nil;
    case AOtherVal.Kind of
      skInteger:  Result := TFpValueConstBool.Create((AIntVal.AsInteger > AOtherVal.AsInteger) xor AReverse);
      skCardinal: Result := TFpValueConstBool.Create((AIntVal.AsInteger > AOtherVal.AsCardinal) xor AReverse);
      skFloat:    Result := TFpValueConstBool.Create((AIntVal.AsInteger > AOtherVal.AsFloat) xor AReverse);
      else SetError('= not supported');
    end;
  end;
  function CardinalGreaterThanValue(ACardinalVal, AOtherVal: TFpValue; AReverse: Boolean = False): TFpValue;
  begin
    Result := nil;
    case AOtherVal.Kind of
      skInteger:  Result := TFpValueConstBool.Create((ACardinalVal.AsCardinal > AOtherVal.AsInteger) xor AReverse);
      skCardinal: Result := TFpValueConstBool.Create((ACardinalVal.AsCardinal > AOtherVal.AsCardinal) xor AReverse);
      skFloat:    Result := TFpValueConstBool.Create((ACardinalVal.AsCardinal > AOtherVal.AsFloat) xor AReverse);
      else SetError('= not supported');
    end;
  end;
  function FloatGreaterThanValue(AFloatVal, AOtherVal: TFpValue; AReverse: Boolean = False): TFpValue;
  begin
    Result := nil;
    case AOtherVal.Kind of
      skInteger:  Result := TFpValueConstBool.Create((AFloatVal.AsFloat > AOtherVal.AsInteger) xor AReverse);
      skCardinal: Result := TFpValueConstBool.Create((AFloatVal.AsFloat > AOtherVal.AsCardinal) xor AReverse);
      skFloat:    Result := TFpValueConstBool.Create((AFloatVal.AsFloat > AOtherVal.AsFloat) xor AReverse);
      else SetError('= not supported');
    end;
  end;
  function CharDataGreaterThanValue(ACharVal, AOtherVal: TFpValue; AReverse: Boolean = False): TFpValue;
  begin
    Result := nil;
    if (AOtherVal.FieldFlags * [svfString, svfWideString] <> []) then
      Result := TFpValueConstBool.Create((ACharVal.AsString > AOtherVal.AsString) xor AReverse)
    else
      SetError('= not supported');
  end;
  function EnumGreaterThanValue(AnEnumVal, AOtherVal: TFpValue; AReverse: Boolean = False): TFpValue;
  begin
    Result := nil;
    if (AOtherVal.Kind in [skEnum, skEnumValue]) and
       (AnEnumVal.FieldFlags * [svfInteger, svfCardinal, svfOrdinal] <> []) and
       (AOtherVal.FieldFlags * [svfInteger, svfCardinal, svfOrdinal] <> [])
    then begin
      if CheckEnumCompatible(AnEnumVal.AsString, AnEnumVal.AsInteger, AOtherVal) and
         CheckEnumCompatible(AOtherVal.AsString, AOtherVal.AsInteger, AnEnumVal)
      then
        Result := TFpValueConstBool.Create((AnEnumVal.AsInteger > AOtherVal.AsInteger) xor AReverse)
      else
        SetError('type mismatch between enum');
    end
    else
      SetError(GetText+' not supported');
  end;

  function IntSmallerThanValue(AIntVal, AOtherVal: TFpValue; AReverse: Boolean = False): TFpValue;
  begin
    Result := nil;
    case AOtherVal.Kind of
      skInteger:  Result := TFpValueConstBool.Create((AIntVal.AsInteger < AOtherVal.AsInteger) xor AReverse);
      skCardinal: Result := TFpValueConstBool.Create((AIntVal.AsInteger < AOtherVal.AsCardinal) xor AReverse);
      skFloat:    Result := TFpValueConstBool.Create((AIntVal.AsInteger < AOtherVal.AsFloat) xor AReverse);
      else SetError('= not supported');
    end;
  end;
  function CardinalSmallerThanValue(ACardinalVal, AOtherVal: TFpValue; AReverse: Boolean = False): TFpValue;
  begin
    Result := nil;
    case AOtherVal.Kind of
      skInteger:  Result := TFpValueConstBool.Create((ACardinalVal.AsCardinal < AOtherVal.AsInteger) xor AReverse);
      skCardinal: Result := TFpValueConstBool.Create((ACardinalVal.AsCardinal < AOtherVal.AsCardinal) xor AReverse);
      skFloat:    Result := TFpValueConstBool.Create((ACardinalVal.AsCardinal < AOtherVal.AsFloat) xor AReverse);
      else SetError('= not supported');
    end;
  end;
  function FloatSmallerThanValue(AFloatVal, AOtherVal: TFpValue; AReverse: Boolean = False): TFpValue;
  begin
    Result := nil;
    case AOtherVal.Kind of
      skInteger:  Result := TFpValueConstBool.Create((AFloatVal.AsFloat < AOtherVal.AsInteger) xor AReverse);
      skCardinal: Result := TFpValueConstBool.Create((AFloatVal.AsFloat < AOtherVal.AsCardinal) xor AReverse);
      skFloat:    Result := TFpValueConstBool.Create((AFloatVal.AsFloat < AOtherVal.AsFloat) xor AReverse);
      else SetError('= not supported');
    end;
  end;
  function CharDataSmallerThanValue(ACharVal, AOtherVal: TFpValue; AReverse: Boolean = False): TFpValue;
  begin
    Result := nil;
    if (AOtherVal.FieldFlags * [svfString, svfWideString] <> []) then
      Result := TFpValueConstBool.Create((ACharVal.AsString < AOtherVal.AsString) xor AReverse)
    else
      SetError('= not supported');
  end;
  function EnumSmallerThanValue(AnEnumVal, AOtherVal: TFpValue; AReverse: Boolean = False): TFpValue;
  begin
    Result := nil;
    if (AOtherVal.Kind in [skEnum, skEnumValue]) and
       (AnEnumVal.FieldFlags * [svfInteger, svfCardinal, svfOrdinal] <> []) and
       (AOtherVal.FieldFlags * [svfInteger, svfCardinal, svfOrdinal] <> [])
    then begin
      if CheckEnumCompatible(AnEnumVal.AsString, AnEnumVal.AsInteger, AOtherVal) and
         CheckEnumCompatible(AOtherVal.AsString, AOtherVal.AsInteger, AnEnumVal)
      then
        Result := TFpValueConstBool.Create((AnEnumVal.AsInteger < AOtherVal.AsInteger) xor AReverse)
      else
        SetError('type mismatch between enum');
    end
    else
      SetError(GetText+' not supported');
  end;

  function SymDiffSets(ASetVal, AOtherVal: TFpValue): TFpValue;
  var
    i, j: Integer;
    m, m2: TFpValue;
    f: TFpValueFieldFlags;
    r: Boolean;
  begin
    Result := TFpValueConstSet.Create;

    for i := 0 to ASetVal.MemberCount - 1 do begin
      m := ASetVal.Member[i];
      j := AOtherVal.MemberCount - 1;
      while j >= 0 do begin
        m2 := AOtherVal.Member[j];
        f := m.FieldFlags * m2.FieldFlags;
        if svfOrdinal in f then
          r := m.AsCardinal = m2.AsCardinal
        else
        if svfIdentifier in f then
          r := m.AsString = m2.AsString
        else
          r := False;
        m2.ReleaseReference;
        if r then
          break;
        dec(j);
      end;

      if j < 0 then
        TFpValueConstSet(Result).AddVal(m);
      m.ReleaseReference;
    end;

    for i := 0 to AOtherVal.MemberCount - 1 do begin
      m := AOtherVal.Member[i];
      j := ASetVal.MemberCount - 1;
      while j >= 0 do begin
        m2 := ASetVal.Member[j];
        f := m.FieldFlags * m2.FieldFlags;
        if svfOrdinal in f then
          r := m.AsCardinal = m2.AsCardinal
        else
        if svfIdentifier in f then
          r := m.AsString = m2.AsString
        else
          r := False;
        m2.ReleaseReference;
        if r then
          break;
        dec(j);
      end;

      if j < 0 then
        TFpValueConstSet(Result).AddVal(m);
      m.ReleaseReference;
    end;
  end;
{$POP}
var
  tmp1, tmp2: TFpValue;
  s: String;
begin
  Result := nil;
  if Count <> 2 then exit;

  tmp1 := Items[0].ResultValue;
  tmp2 := Items[1].ResultValue;
  if (tmp1 = nil) or (tmp2 = nil) then exit;
  s := GetText;

  if (s = '=') or (s = '<>') then begin
    case tmp1.Kind of
      skInteger:  Result := IntEqualToValue(tmp1, tmp2, (s = '<>'));
      skCardinal: Result := CardinalEqualToValue(tmp1, tmp2, (s = '<>'));
      skFloat:    Result := FloatEqualToValue(tmp1, tmp2, (s = '<>'));
      skPointer: begin
                  // Pchar can concatenate with String. But not with other Pchar
                  // Maybe allow optional: This does limit undetected/mis-detected strings
                  if (tmp1.FieldFlags * [svfString, svfWideString] <> []) and
                     (tmp2.Kind in [skString, skAnsiString, skWideString, skChar{, skWideChar}])
                  then
                    Result := CharDataEqualToValue(tmp1, tmp2, (s = '<>'))
                  else
                    Result := AddressPtrEqualToValue(tmp1, tmp2, (s = '<>'));
        end;
      skClass,skInterface:
                  Result := AddressPtrEqualToValue(tmp1, tmp2, (s = '<>'));
      skAddress: begin
                  if tmp2.Kind in [skClass,skInterface,skPointer,skAddress] then
                    Result := AddressPtrEqualToValue(tmp1, tmp2, (s = '<>'));
        end;
      skString, skAnsiString, skWideString, skChar{, skWideChar}:
                  Result := CharDataEqualToValue(tmp1, tmp2, (s = '<>'));
      skSet:      Result := SetEqual(tmp1, tmp2, (s = '<>'));
      skBoolean:  Result := BoolEqual(tmp1, tmp2, (s = '<>'));
      skEnum, skEnumValue:
                  Result := EnumEqual(tmp1, tmp2, (s = '<>'));
    end;
  end
  else
  if (s = '>') or (s = '<=') then begin
    case tmp1.Kind of
      skInteger:  Result := IntGreaterThanValue(tmp1, tmp2, (s = '<='));
      skCardinal: Result := CardinalGreaterThanValue(tmp1, tmp2, (s = '<='));
      skFloat:    Result := FloatGreaterThanValue(tmp1, tmp2, (s = '<='));
      skPointer:  if (tmp1.FieldFlags * [svfString, svfWideString] <> []) and
                     (tmp2.Kind in [skString, skAnsiString, skWideString, skChar{, skWideChar}])
                   then
                     Result := CharDataGreaterThanValue(tmp1, tmp2, (s = '<='));
      skString, skAnsiString, skWideString, skChar{, skWideChar}:
                  Result := CharDataGreaterThanValue(tmp1, tmp2, (s = '<='));
      skEnum, skEnumValue:
                  Result := EnumGreaterThanValue(tmp1, tmp2, (s = '<='));
    end;
  end
  else
  if (s = '<') or (s = '>=') then begin
    case tmp1.Kind of
      skInteger:  Result := IntSmallerThanValue(tmp1, tmp2, (s = '>='));
      skCardinal: Result := CardinalSmallerThanValue(tmp1, tmp2, (s = '>='));
      skFloat:    Result := FloatSmallerThanValue(tmp1, tmp2, (s = '>='));
      skPointer:  if (tmp1.FieldFlags * [svfString, svfWideString] <> []) and
                     (tmp2.Kind in [skString, skAnsiString, skWideString, skChar{, skWideChar}])
                   then
                     Result := CharDataSmallerThanValue(tmp1, tmp2, (s = '>='));
      skString, skAnsiString, skWideString, skChar{, skWideChar}:
                  Result := CharDataSmallerThanValue(tmp1, tmp2, (s = '>='));
      skEnum, skEnumValue:
                  Result := EnumSmallerThanValue(tmp1, tmp2, (s = '>='));
    end;
  end
  else
  if GetText = '><' then begin
    if (tmp1.Kind = skSet) and (tmp2.Kind = skSet) then
      Result := SymDiffSets(tmp1, tmp2);
  end;

  ForwardError(Result, tmp1);
  ForwardError(Result, tmp2);
 {$IFDEF WITH_REFCOUNT_DEBUG}if Result <> nil then
   Result.DbgRenameReference(nil, 'DoGetResultValue');{$ENDIF}
end;

{ TFpPascalExpressionPartOperatorMemberOf }

procedure TFpPascalExpressionPartOperatorMemberOf.Init;
begin
  FPrecedence := PRECEDENCE_MEMBER_OF;
  inherited Init;
end;

function TFpPascalExpressionPartOperatorMemberOf.IsValidNextPart(APart: TFpPascalExpressionPart): Boolean;
begin
  Result := inherited IsValidNextPart(APart);
  if not HasAllOperands then
    Result := Result and (APart is TFpPascalExpressionPartIdentifier);
end;

function TFpPascalExpressionPartOperatorMemberOf.DoGetResultValue: TFpValue;
var
  tmp, AutoDereVal: TFpValue;
  MemberName: String;
  MemberSym: TFpSymbol;
begin
  Result := nil;
  if Count <> 2 then exit;

  tmp := Items[0].ResultValue;
  if (tmp = nil) then
    exit;

  MemberName := Items[1].GetText;

  AutoDereVal := nil;
  try
    if ExpressionData.AutoDeref then begin
      // Copy from TFpPascalExpressionPartOperatorDeRef.DoGetResultValue
      if tmp.Kind = skPointer then begin
        if (svfDataAddress in tmp.FieldFlags) and (IsReadableLoc(tmp.DerefAddress)) and // TODO, what if Not readable addr
           (tmp.TypeInfo <> nil) //and (tmp.TypeInfo.TypeInfo <> nil)
        then begin
          tmp := tmp.Member[0];
          AutoDereVal := tmp;
        end;
        if (tmp = nil) then begin
          SetErrorWithPos(fpErrCannotDeref_p, [Items[0].GetText(MAX_ERR_EXPR_QUOTE_LEN)]);
          exit;
        end;
      end;
    end;

    if (tmp.Kind in [skClass, skInterface, skRecord, skObject]) then begin
      Result := tmp.MemberByName[MemberName];
      if Result = nil then begin
        SetError(fpErrNoMemberWithName, [MemberName]);
        exit;
      end;
      {$IFDEF WITH_REFCOUNT_DEBUG}Result.DbgRenameReference(nil, 'DoGetResultValue'){$ENDIF};
      Assert((Result.DbgSymbol=nil)or(Result.DbgSymbol.SymbolType=stValue), 'member is value');
      exit;
    end;
  finally
    AutoDereVal.ReleaseReference;
  end;

  if (tmp.Kind in [skType]) and
     (tmp.DbgSymbol <> nil) and (tmp.DbgSymbol.Kind in [skClass, skInterface, skRecord, skObject])
  then begin
    Result := tmp.MemberByName[MemberName];
    if Result <> nil then begin
      // only class fields/constants can have an address without valid "self" instance
      if IsReadableLoc(result.DataAddress) then begin   // result.Address?
        {$IFDEF WITH_REFCOUNT_DEBUG}Result.DbgRenameReference(nil, 'DoGetResultValue'){$ENDIF};
        exit;
      end
      else begin
        ReleaseRefAndNil(Result);
        MemberSym := tmp.DbgSymbol.NestedSymbolByName[MemberName];
        if MemberSym <> nil then begin
          Result := TFpValueTypeDefinition.Create(MemberSym);
          {$IFDEF WITH_REFCOUNT_DEBUG}Result.DbgRenameReference(nil, 'DoGetResultValue'){$ENDIF};
          exit;
        end;
      end;
    end;
    SetError(fpErrNoMemberWithName, [MemberName]);
    exit
  end;

  if (tmp.Kind in [skType]) and
     (tmp.DbgSymbol <> nil) and (tmp.DbgSymbol.Kind in [skEnum])
  then begin
    Result := tmp.MemberByName[MemberName];
    if Result <> nil then begin
      {$IFDEF WITH_REFCOUNT_DEBUG}Result.DbgRenameReference(nil, 'DoGetResultValue'){$ENDIF};
      exit;
    end;
    SetError(fpErrNoMemberWithName, [MemberName]);
    exit
  end;

  if (tmp.Kind = skUnit) or
     ( (tmp.DbgSymbol <> nil) and (tmp.DbgSymbol.Kind = skUnit) )
  then begin
    (* If a class/record/object matches the typename, but did not have the member,
       then this could still be a unit.
       If the class/record/object is in the same unit as the current contexct (selected function)
       then it would hide the unitname, but otherwise a unit in the uses clause would
       hide the structure.
    *)
    Result := ExpressionData.Scope.FindSymbol(MemberName, Items[0].GetText);
    if Result <> nil then begin
      {$IFDEF WITH_REFCOUNT_DEBUG}Result.DbgRenameReference(nil, 'DoGetResultValue'){$ENDIF};
      exit;
    end;
  end;


  SetError(fpErrorNotAStructure, [MemberName, Items[0].GetText]);
end;

{ TFpPascalExpressionPartOperatorMemberIn }

procedure TFpPascalExpressionPartOperatorMemberIn.Init;
begin
  FPrecedence := PRECEDENCE_IN;
  inherited Init;
end;

function TFpPascalExpressionPartOperatorMemberIn.DoGetResultValue: TFpValue;
var
  AVal, ASet, m: TFpValue;
  s: String;
  i: Integer;
  f, af: TFpValueFieldFlags;
  r: Boolean;
  v: QWord;
begin
  Result := nil;
  if Count <> 2 then begin
    SetError('"in" requires 2 values');
    exit;
  end;

  ASet := Items[1].ResultValue;
  if (ASet = nil) or (ASet.Kind <> skSet) then begin
    SetError('"in" requires a set');
    exit;
  end;

  AVal := Items[0].ResultValue;
  if AVal = nil then begin
    SetError('"in" requires an enum');
    exit;
  end;


  if (AVal.Kind in [skEnum, skEnumValue]) then begin
    s := '';
    v := 0;
    af := AVal.FieldFlags;
    if svfIdentifier in af then
      s := LowerCase(AVal.AsString);
    if svfOrdinal in af then
      v := AVal.AsCardinal;

    r := False;
    for i := 0 to ASet.MemberCount-1 do begin
      m := ASet.Member[i];
      f := m.FieldFlags * af;
      if svfIdentifier in f then
        r := LowerCase(m.AsString) = s
      else
      if svfOrdinal in f then
        r := m.AsCardinal = v
      else
        r := False;
      m.ReleaseReference;

      if r then
        break;
    end;

    Result := TFpValueConstBool.Create(r);
    {$IFDEF WITH_REFCOUNT_DEBUG}
    if Result <> nil then
      Result.DbgRenameReference(nil, 'DoGetResultValue');{$ENDIF}
    exit;
  end;



  if (AVal.Kind in [skChar, skSimple, skCardinal, skInteger]) and
     (svfOrdinal in AVal.FieldFlags)
  then begin
    v := AVal.AsCardinal;
    r := False;
    for i := 0 to ASet.MemberCount-1 do begin
      m := ASet.Member[i];
      f := m.FieldFlags;
      if svfOrdinal in m.FieldFlags then
        r := m.AsCardinal = v
      else
        r := False;
      m.ReleaseReference;

      if r then
        break;
    end;

    Result := TFpValueConstBool.Create(r);
    {$IFDEF WITH_REFCOUNT_DEBUG}
    if Result <> nil then
      Result.DbgRenameReference(nil, 'DoGetResultValue');{$ENDIF}
    exit;
  end;

  SetError('"in" requires an enum');
end;

{ TFpPasParserValueSlicedArrayIndex }

function TFpPasParserValueSlicedArrayIndex.GetValueLowBound(
  AValueObj: TFpValue; out ALowBound: Int64): Boolean;
begin
  ALowBound := FLowBound;
  Result := True;
end;

{ TFpPasParserValueSlicedArray }

function TFpPasParserValueSlicedArray.SlicePart: TFpPascalExpressionPartOperatorArraySlice;
begin
  Result := FArraySlice.FSlicePart;
end;

function TFpPasParserValueSlicedArray.GetKind: TDbgSymbolKind;
begin
  Result := skArray;
end;

function TFpPasParserValueSlicedArray.GetFieldFlags: TFpValueFieldFlags;
begin
  Result := [svfMembers];
end;

function TFpPasParserValueSlicedArray.GetTypeInfo: TFpSymbol;
begin
  Result := nil;
end;

function TFpPasParserValueSlicedArray.GetMember(AIndex: Int64): TFpValue;
begin
  if SlicePart.FCurrentIndex <> AIndex then begin
    SlicePart.FCurrentIndex := AIndex;
    FArraySlice.ResetEvaluationForIndex;

    // Some evaluation errors (such as deref nil) are marked on the Expression,
    // rather than on the value(s).
    // If this code is called, the Expression should have been valid before.
    FExpressionPartInValue.ExpressionData.ClearError;
  end;
  Result := FArraySlice.Items[0].ResultValue;

  if Result <> nil then begin
    //if IsError(FExpressionPart.ExpressionData.FError) and not IsError(Result.LastError) then
    //  Result.SetLastError(FExpressionPart.ExpressionData.FError);

    Result.AddReference;
    Result.Reset;
    if FArraySlice.FHasVariantPart then
      Result.Flags := Result.Flags + [vfVariant];
  end;

  FExpressionPartInValue.ExpressionData.ClearError;
end;

function TFpPasParserValueSlicedArray.GetMemberCount: Integer;
begin
  Result := SlicePart.EndValue - SlicePart.StartValue + 1;
end;

function TFpPasParserValueSlicedArray.GetIndexType(AIndex: Integer): TFpSymbol;
begin
  Result := FLowBoundIndex;
end;

function TFpPasParserValueSlicedArray.GetIndexTypeCount: Integer;
begin
  Result := 1;
end;

function TFpPasParserValueSlicedArray.GetOrdLowBound: Int64;
begin
  Result := FLowBoundIndex.FLowBound;
end;

constructor TFpPasParserValueSlicedArray.Create(
  ASlice: TFpPascalExpressionPartOperatorArraySliceController; AnOwnsController: boolean);
begin
  FExpressionPartInValue := ASlice;
  inherited Create(ASlice);
  FArraySlice := ASlice;
  FOwnsController := AnOwnsController;
  FLowBoundIndex := TFpPasParserValueSlicedArrayIndex.Create('');
  FLowBoundIndex.FLowBound := SlicePart.StartValue;
end;

destructor TFpPasParserValueSlicedArray.Destroy;
begin
  inherited Destroy;
  FLowBoundIndex.ReleaseReference;
  if FOwnsController then
    FArraySlice.Free;
end;

function TFpPasParserValueSlicedArray.SliceBracketStartOffs: integer;
var
  sp: TFpPascalExpressionPartOperatorArraySlice;
  s, e: PChar;
begin
  Result := -1;
  sp := SlicePart;
  if (sp <> nil) then begin
    sp.GetFirstLastChar(s, e);
    Result := s - sp.ExpressionData.TextExpressionAddr + 1;
  end;
end;

function TFpPasParserValueSlicedArray.SliceBracketLength: integer;
var
  sp: TFpPascalExpressionPartOperatorArraySlice;
  s, e: PChar;
begin
  Result := -1;
  sp := SlicePart;
  if (sp <> nil) then begin
    sp.GetFirstLastChar(s, e);
    while e[1] = '!' do e := e + 1;
    Result := e - s + 1;
  end;
end;

{ TFpPascalExpressionPartOperatorArraySliceController }

function TFpPascalExpressionPartOperatorArraySliceController.GetFullText(AMaxLen: Integer): String;
begin
  Result := '';
  if Count > 0 then
    Result := Items[0].GetFullText(AMaxLen);
end;

function TFpPascalExpressionPartOperatorArraySliceController.DebugText(AIndent: String;
  AWithResults: Boolean): String;
begin
  Result := inherited DebugText(AIndent, AWithResults) +
            AIndent +'// '+ GetFullText+LineEnding;
end;

function TFpPascalExpressionPartOperatorArraySliceController.GetCanDisableSlice: boolean;
begin
  Result := (not FResultValDone);
end;

function TFpPascalExpressionPartOperatorArraySliceController.DoGetResultValue: TFpValue;
var
  me: TFpPascalExpressionPartOperatorArraySliceController;
begin
  if FNeedCopy > 0 then
    me := TFpPascalExpressionPartOperatorArraySliceController(CreateCopy(Parent))
  else
    me := Self;

  Result := me.InternalDoGetResultValue(FNeedCopy > 0);
end;

function TFpPascalExpressionPartOperatorArraySliceController.InternalDoGetResultValue(
  ANeedCopy: boolean): TFpValue;
var
  tmp: TFpValue;
begin
  FSlicePart.FCurrentIndex := FSlicePart.StartValue;
  FSlicePart.EndValue; // needs to be touched
  FSlicePart.FTouched := False;
  ResetEvaluationForIndex;
  Result := Items[0].ResultValue;

  if (not FSlicePart.FTouched) or DisableSlice then begin
    // The array slice is not part of the ExpressionData ("? :" or Try)
    // Or already handled.
    if Result <> nil then
      Result.AddReference;

    if ANeedCopy then
      Self.Free;
    exit;
  end;


  if (FSlicePart.Parent.Items[0].ResultValue <> nil) then begin
    tmp := FSlicePart.Parent.Items[0].ResultValue;
    if (vfArrayUpperBoundLimit in tmp.Flags) then begin
      FSlicePart.FUpperLimit := tmp.OrdHighBound;
      FSlicePart.FHasUpperLimit := True;
    end;
  end;


  // Need to return an array, the array itself should not set an error (the 1st (or any) member may do)
  // We need to get the error again later, when we get the first member
  if IsError(ExpressionData.Error) then
    Items[0].ResetEvaluation;
  ExpressionData.ClearError;

  if not FCheckedForVariantPart then begin
    FSlicePart.CheckForVariantExpressionParts;
    FCheckedForVariantPart := True;
  end;

  Result := TFpPasParserValueSlicedArray.Create(Self, ANeedCopy);
  if FHasVariantPart then
    Result.Flags := Result.Flags + [vfArrayOfVariant];
end;

procedure TFpPascalExpressionPartOperatorArraySliceController.DoParentIndexBraceClosed(
  APreviousArraySliceList: PFpPascalExpressionPartOperatorArraySlice);
begin
  if Count = 1 then
    Items[0].DoParentIndexBraceClosed(APreviousArraySliceList);
end;

procedure TFpPascalExpressionPartOperatorArraySliceController.ResetEvaluation;
begin
  inherited ResetEvaluation;
  FDisableSlice := False;
end;

procedure TFpPascalExpressionPartOperatorArraySliceController.ResetEvaluationForIndex;
begin
  FInResetEvaluationForIndex := True;
  FSlicePart.ResetEvaluationForAnchestors;
  FInResetEvaluationForIndex := False;
end;

procedure TFpPascalExpressionPartOperatorArraySliceController.ResetEvaluationForAnchestors;
begin
  if FInResetEvaluationForIndex then
    exit;
  inherited ResetEvaluationForAnchestors;
end;

constructor TFpPascalExpressionPartOperatorArraySliceController.Create(
  AnExpressionData: TFpPascalExpressionSharedData;
  ASlicePart: TFpPascalExpressionPartOperatorArraySlice; AStartChar: PChar; AnEndChar: PChar);
begin
  inherited Create(AnExpressionData, AStartChar, AnEndChar);
  FSlicePart := ASlicePart;
  FIsClosed := True;
end;

procedure TFpPascalExpressionPartOperatorArraySliceController.BeginNeedCopy;
begin
  inherited BeginNeedCopy;
  inc(FNeedCopy);
end;

procedure TFpPascalExpressionPartOperatorArraySliceController.EndNeedCopy;
begin
  inherited EndNeedCopy;
  dec(FNeedCopy);
end;

{ TFpPascalExpressionPartOperatorArraySlice }

procedure TFpPascalExpressionPartOperatorArraySlice.Init;
begin
  inherited Init;
  FPrecedence := PRECEDENCE_ARRAY_SLICE;
end;

procedure TFpPascalExpressionPartOperatorArraySlice.DoParentIndexBraceClosed(
  APreviousArraySliceList: PFpPascalExpressionPartOperatorArraySlice);
var
  p, p2: TFpPascalExpressionPartOperatorArraySlice;
begin
  FPreviousArraySlice := APreviousArraySliceList^;
  APreviousArraySliceList^ := Self;

  if FindInParents(FPreviousArraySlice) then  begin
    p := FPreviousArraySlice;
    p2 := p.FPreviousArraySlice;
    while FindInParents(p2) do begin
      p := p2;
      p2 := p.FPreviousArraySlice;
    end;
    if APreviousArraySliceList^ = Self then begin
      APreviousArraySliceList^ := p;
    end
    else begin
      p2 := APreviousArraySliceList^;
      while p2.FPreviousArraySlice <> self do
        p2 := p2.FPreviousArraySlice;
      p2.FPreviousArraySlice := p;
    end;
      FPreviousArraySlice := p.FPreviousArraySlice;
      p.FPreviousArraySlice := Self;
  end;
end;

function TFpPascalExpressionPartOperatorArraySlice.DoGetResultValue: TFpValue;
begin
  FTouched := True;
  Result := TFpValueConstNumber.Create(QWord(FCurrentIndex), True);
end;

function TFpPascalExpressionPartOperatorArraySlice.StartValue: Int64;
var
  tmp: TFpValue;
begin
  Result := 0;
  if Count < 1 then exit;
  tmp := Items[0].ResultValue;
  if tmp <> nil then
    Result := tmp.AsInteger;
end;

function TFpPascalExpressionPartOperatorArraySlice.EndValue: Int64;
var
  tmp: TFpValue;
  i: Int64;
begin
  Result := 0;
  if Count < 2 then exit;
  tmp := Items[1].ResultValue;
  if tmp <> nil then begin
    Result := tmp.AsInteger;
    if FHasUpperLimit and (FUpperLimit < Result) then
      Result := FUpperLimit;
  end;
end;

procedure TFpPascalExpressionPartOperatorArraySlice.CheckForVariantExpressionParts;
var
  AHasVariantPart: Boolean;
  APart: TFpPascalExpressionPartContainer;
begin
  AHasVariantPart := False;
  APart := Parent;
  while (APart <> nil) and not(APart is TFpPascalExpressionPartOperatorArraySliceController) do begin
    if APart.ReturnsVariant then
      AHasVariantPart := True;
    APart := APart.Parent;
  end;
  // The most inner TFpPascalExpressionPartOperatorArraySliceController, even if not belonging to this slice
  if APart is TFpPascalExpressionPartOperatorArraySliceController then
    TFpPascalExpressionPartOperatorArraySliceController(APart).FHasVariantPart := AHasVariantPart;
end;

procedure TFpPascalExpressionPartOperatorArraySlice.Assign(ASourcePart: TFpPascalExpressionPart);
var
  SliceSourcePart: TFpPascalExpressionPartOperatorArraySlice absolute ASourcePart;
begin
  inherited Assign(ASourcePart);
  if ASourcePart is TFpPascalExpressionPartOperatorArraySlice then begin
    FController := TFpPascalExpressionPartOperatorArraySliceController(FindCopiedInParents(ASourcePart, SliceSourcePart.FController));
    if FController <> nil then
      FController.FSlicePart := Self
    else
      FController := SliceSourcePart.FController;
  end;
end;

destructor TFpPascalExpressionPartOperatorArraySlice.Destroy;
begin
  if (FController <> nil) and (FController.SlicePart = Self) then
    FController.FSlicePart := nil; // in case of DebugDump
  inherited Destroy;
end;

function TFpPascalExpressionPartOperatorArraySlice.IsClosed: boolean;
begin
  Result := (Parent <> nil) and
            (Parent is TFpPascalExpressionPartBracketIndex) and
            Parent.IsClosed; // the enclosing index brackets "[   ]"
end;

function TFpPascalExpressionPartOperatorArraySlice.AddController(ACurPart: TFpPascalExpressionPart
  ): TFpPascalExpressionPart;
var
  p: TFpPascalExpressionPart;
begin
  Result := ACurPart;
  if not ExpressionData.Valid then
    exit;
  if FController <> nil then
    exit;

  p := ACurPart;
  while (ACurPart.Parent <> nil) and
        ( ACurPart.Parent.IsClosed or
          not FindInParents(ACurPart)
        )
  do
    ACurPart := ACurPart.Parent;

  FController := TFpPascalExpressionPartOperatorArraySliceController.Create(ExpressionData, Self, FStartChar, FEndChar);
  FController.Parent := ACurPart.Parent;
  ACurPart.ReplaceInParent(FController);
  p.HandleEndOfExpression;
  FController.Add(ACurPart);
  Result := FController;
end;

function TFpPascalExpressionPartOperatorArraySlice.AddControllerRecursive(
  ACurPart: TFpPascalExpressionPart): TFpPascalExpressionPart;
begin
  Result := AddController(ACurPart);
  if not ExpressionData.Valid then
    exit;
  if FPreviousArraySlice <> nil then
    Result := FPreviousArraySlice.AddControllerRecursive(Result);
end;

initialization
  DBG_WARNINGS := DebugLogger.FindOrRegisterLogGroup('DBG_WARNINGS' {$IFDEF DBG_WARNINGS} , True {$ENDIF} );

end.

