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
{$IFDEF INLINE_OFF}{$INLINE OFF}{$ENDIF}
{$TYPEDADDRESS on}

interface

uses
  Classes, sysutils, math, DbgIntfBaseTypes, FpDbgInfo, FpdMemoryTools,
  FpErrorMessages, FpDbgDwarf, {$ifdef FORCE_LAZLOGGER_DUMMY} LazLoggerDummy {$else} LazLoggerBase {$endif}, LazClasses;

const
  MAX_ERR_EXPR_QUOTE_LEN = 200;

type

  TFpPascalExpressionPartList= class;

  TFpPascalExpressionPart = class;
  TFpPascalExpressionPartContainer = class;
  TFpPascalExpressionPartWithPrecedence = class;
  TFpPascalExpressionPartBracket = class;
  TFpPascalExpressionPartOperator = class;

  TFpPascalExpressionPartClass = class of TFpPascalExpressionPart;
  TFpPascalExpressionPartBracketClass = class of TFpPascalExpressionPartBracket;

  TSeparatorType = (ppstComma);

  TFpIntrinsicPrefix = (ipColon, ipExclamation, ipNoPrefix);
  TFpIntrinsicFunc = (
    ifErrorNotFound,
    ifChildClass,
    ifLength, ifRefCount, ifPos, ifSubStr, ifLower, ifUpper,
    ifRound, ifTrunc
  );

  TFpPascalParserCallFunctionProc = function (AnExpressionPart: TFpPascalExpressionPart;
    AFunctionValue: TFpValue; ASelfValue: TFpValue; AParams: TFpPascalExpressionPartList;
    out AResult: TFpValue; var AnError: TFpError): boolean of object;

  { TFpPascalExpression }

  TFpPascalExpression = class
  private
    FError: TFpError;
    FContext: TFpDbgSymbolScope;
    FFixPCharIndexAccess: Boolean;
    FHasPCharIndexAccess: Boolean;
    FIntrinsicPrefix: TFpIntrinsicPrefix;
    FOnFunctionCall: TFpPascalParserCallFunctionProc;
    FTextExpression: String;
    FExpressionPart: TFpPascalExpressionPart;
    FValid: Boolean;
    function GetResultValue: TFpValue;
    function GetValid: Boolean;
    procedure SetError(AMsg: String);  // deprecated;
    procedure SetError(AnErrorCode: TFpErrorCode; const AnNestedErr: TFpError = nil);
    procedure SetError(AnErrorCode: TFpErrorCode; AData: array of const; const AnNestedErr: TFpError = nil);
    procedure SetError(const AnErr: TFpError);
    function PosFromPChar(APChar: PChar): Integer;
    function LookupIntrinsic(AStart: PChar; ALen: Integer): TFpIntrinsicFunc;
  protected
    function GetDbgSymbolForIdentifier({%H-}AnIdent: String): TFpValue;
    function GetRegisterValue({%H-}AnIdent: String): TFpValue;
    property ExpressionPart: TFpPascalExpressionPart read FExpressionPart;
    property Context: TFpDbgSymbolScope read FContext;
  public
    constructor Create(ATextExpression: String; AContext: TFpDbgSymbolScope; ASkipParse: Boolean = False);
    destructor Destroy; override;
    procedure Parse;
    function DebugDump(AWithResults: Boolean = False): String;
    procedure ResetEvaluation;
    property TextExpression: String read FTextExpression;
    property Error: TFpError read FError;
    property Valid: Boolean read GetValid;
    // Set by GetResultValue
    property HasPCharIndexAccess: Boolean read FHasPCharIndexAccess;
    // handle pchar as string (adjust index)
    property FixPCharIndexAccess: Boolean read FFixPCharIndexAccess write FFixPCharIndexAccess;
    property IntrinsicPrefix: TFpIntrinsicPrefix read FIntrinsicPrefix write FIntrinsicPrefix;
    // ResultValue
    // - May be a type, if expression is a type
    // - Only valid, as long as the expression is not destroyed
    property ResultValue: TFpValue read GetResultValue;
    property OnFunctionCall: TFpPascalParserCallFunctionProc read FOnFunctionCall write FOnFunctionCall;
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

  { TFpPascalExpressionPart }

  TFpPascalExpressionPart = class
  private
    FEndChar: PChar;
    FParent: TFpPascalExpressionPartContainer;
    FStartChar: PChar;
    FExpression: TFpPascalExpression;
    FResultValue: TFpValue;
    FResultValDone: Boolean;
    function GetResultValue: TFpValue;
    function GetSurroundingOpenBracket: TFpPascalExpressionPartBracket;
    function GetTopParent: TFpPascalExpressionPart;
    procedure SetEndChar(AValue: PChar);
    procedure SetParent(AValue: TFpPascalExpressionPartContainer); virtual;
    procedure SetStartChar(AValue: PChar);
    procedure SetError(AMsg: String = ''); // deprecated;
    procedure SetError(APart: TFpPascalExpressionPart; AMsg: String = ''); // deprecated;
    function  CreateErrorWithPos(AnErrorCode: TFpErrorCode; AData: array of const; APos: integer = -1): TFpError;
    procedure SetErrorWithPos(AnErrorCode: TFpErrorCode; AData: array of const);
    //procedure SetError(AnErrorCode: TFpErrorCode; const AnNestedErr: TFpError = nil);
    procedure SetError(AnErrorCode: TFpErrorCode; AData: array of const; const AnNestedErr: TFpError = nil);
    procedure SetError(AnError: TFpError);
  protected
    function DebugText(AIndent: String; {%H-}AWithResults: Boolean): String; virtual; // Self desc only
    function DebugDump(AIndent: String; AWithResults: Boolean): String; virtual;
  protected
    procedure Init; virtual;
    function  DoGetIsTypeCast: Boolean; virtual; deprecated;
    function  DoGetResultValue: TFpValue; virtual;
    procedure ResetEvaluation;
    procedure ResetEvaluationRecursive; virtual;
    procedure ResetEvaluationForAnchestors; virtual;

    Procedure ReplaceInParent(AReplacement: TFpPascalExpressionPart);
    procedure DoHandleEndOfExpression; virtual;

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
  public
    constructor Create(AExpression: TFpPascalExpression; AStartChar: PChar; AnEndChar: PChar = nil);
    destructor Destroy; override;
    function  HandleNextPart(APart: TFpPascalExpressionPart): TFpPascalExpressionPart; virtual;
    procedure HandleEndOfExpression; virtual;

    function GetText(AMaxLen: Integer=0): String;
    function GetPos: Integer;
    function GetFullText(AMaxLen: Integer=0): String; virtual; // including children
    property StartChar: PChar read FStartChar write SetStartChar;
    property EndChar: PChar read FEndChar write SetEndChar;
    property Parent: TFpPascalExpressionPartContainer read FParent write SetParent;
    property TopParent: TFpPascalExpressionPart read GetTopParent; // or self
    property SurroundingBracket: TFpPascalExpressionPartBracket read GetSurroundingOpenBracket; // incl self
    property ResultValue: TFpValue read GetResultValue;
    property Expression: TFpPascalExpression read FExpression;
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
    procedure ResetEvaluationRecursive; override;
    function DebugDump(AIndent: String; AWithResults: Boolean): String; override;
  public
    destructor Destroy; override;
    function GetFullText(AMaxLen: Integer=0): String; virtual; // including children
    function Add(APart: TFpPascalExpressionPart): Integer;
    function IndexOf(APart: TFpPascalExpressionPart): Integer;
    procedure Clear;
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TFpPascalExpressionPart read GetItems write SetItems;
    property LastItem: TFpPascalExpressionPart read GetLastItem write SetLastItem;
  end;

  { TFpPascalExpressionPartIdentifier }

  TFpPascalExpressionPartIdentifier = class(TFpPascalExpressionPartContainer)
  protected
    function DoGetIsTypeCast: Boolean; override;
    function DoGetResultValue: TFpValue; override;
  end;

  { TFpPascalExpressionPartCpuRegister }

  TFpPascalExpressionPartCpuRegister = class(TFpPascalExpressionPartContainer)
  protected
    function DoGetResultValue: TFpValue; override;
  end;

  { TFpPascalExpressionPartIntrinsic }
  TFpPascalExpressionPartBracketArgumentList = class;

  TFpPascalExpressionPartIntrinsic = class(TFpPascalExpressionPartContainer)
  private
    FIntrinsic: TFpIntrinsicFunc;
    FChildClassCastType: TFpValue;

    function CheckArgumentCount(AParams: TFpPascalExpressionPartBracketArgumentList; ARequiredCount: Integer; AMaxAccepted: Integer = -1): Boolean;
    // GetArg; ANum is 1 based
    function GetArg(AParams: TFpPascalExpressionPartBracketArgumentList; ANum: Integer; out AValue: TFpValue;
                    AnErr: String = ''): Boolean;
  protected
    function DoLength(AParams: TFpPascalExpressionPartBracketArgumentList): TFpValue;
    function DoChildClass(AParams: TFpPascalExpressionPartBracketArgumentList): TFpValue;
    function DoRefCnt(AParams: TFpPascalExpressionPartBracketArgumentList): TFpValue;
    function DoPos(AParams: TFpPascalExpressionPartBracketArgumentList): TFpValue;
    function DoSubStr(AParams: TFpPascalExpressionPartBracketArgumentList): TFpValue;
    function DoLower(AParams: TFpPascalExpressionPartBracketArgumentList): TFpValue;
    function DoUpper(AParams: TFpPascalExpressionPartBracketArgumentList): TFpValue;
    function DoRound(AParams: TFpPascalExpressionPartBracketArgumentList): TFpValue;
    function DoTrunc(AParams: TFpPascalExpressionPartBracketArgumentList): TFpValue;

    function DoGetResultValue: TFpValue; override;
    function DoGetResultValue(AParams: TFpPascalExpressionPartBracketArgumentList): TFpValue;
  public
    constructor Create(AExpression: TFpPascalExpression; AStartChar: PChar;
      AnEndChar: PChar; AnIntrinsic: TFpIntrinsicFunc);
    destructor Destroy; override;
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
  end;

  { TFpPascalExpressionPartWithPrecedence }

  TFpPascalExpressionPartWithPrecedence = class(TFpPascalExpressionPartContainer)
  protected
    FPrecedence: Integer;
    function HasPrecedence: Boolean; override;
  public
    property Precedence: Integer read FPrecedence;
  end;

  { TFpPascalExpressionPartBracket }

  TFpPascalExpressionPartBracket = class(TFpPascalExpressionPartWithPrecedence)
  // some, but not all bracket expr have precedence
  private
    FIsClosed: boolean;
    FIsClosing: boolean;
    FAfterComma: Integer;
    function GetAfterComma: Boolean;
  protected
    procedure Init; override;
    function HasPrecedence: Boolean; override;
    procedure DoHandleEndOfExpression; override;
    function CanHaveOperatorAsNext: Boolean; override;
    function HandleNextPartInBracket(APart: TFpPascalExpressionPart): TFpPascalExpressionPart; virtual;
    procedure SetAfterCommaFlag;
    property AfterComma: Boolean read GetAfterComma;
  public
    procedure CloseBracket;
    function HandleNextPart(APart: TFpPascalExpressionPart): TFpPascalExpressionPart; override;
    procedure HandleEndOfExpression; override;
    property IsClosed: boolean read FIsClosed;
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
  public
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

  { TFpPascalExpressionPartOperatorOr }

  TFpPascalExpressionPartOperatorOr = class(TFpPascalExpressionPartBinaryOperator)    // OR XOR
  public type
    TOpOrType = (ootOr, ootXor);
  protected
    FOp: TOpOrType;
    procedure Init; override;
    function DoGetResultValue: TFpValue; override;
  public
    constructor Create(AExpression: TFpPascalExpression; AnOp: TOpOrType; AStartChar: PChar;
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

  TFpPascalExpressionPartOperatorArraySlice = class;

  { TFpPascalExpressionPartOperatorArraySliceController }

  TFpPascalExpressionPartOperatorArraySliceController = class(TFpPascalExpressionPartContainer)    // enum in set
  private
    FSlicePart: TFpPascalExpressionPartOperatorArraySlice;
    FInResetEvaluationForIndex: Boolean;
  protected
    function DoGetResultValue: TFpValue; override;
    procedure ResetEvaluationForIndex;
    procedure ResetEvaluationForAnchestors; override;
  public
    constructor Create(AExpression: TFpPascalExpression;
      ASlicePart: TFpPascalExpressionPartOperatorArraySlice;
      ATopPart: TFpPascalExpressionPart;
      AStartChar: PChar; AnEndChar: PChar = nil);
    function HandleNextPart(APart: TFpPascalExpressionPart): TFpPascalExpressionPart; override;
  end;

  { TFpPascalExpressionPartOperatorArraySlice }

  TFpPascalExpressionPartOperatorArraySlice = class(TFpPascalExpressionPartBinaryOperator)    // enum in set
  private
    FCurrentIndex: Int64;
  protected
    procedure Init; override;
    procedure SetParent(AValue: TFpPascalExpressionPartContainer); override;
    function DoGetResultValue: TFpValue; override;
    function StartValue: Int64;
    function EndValue: Int64;
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
  PRECEDENCE_PLUS_MINUS = 13;        // a + b
  PRECEDENCE_OR         = 13;        // a OR b  // XOR
  PRECEDENCE_ARRAY_SLICE= 18;        // array[5..9] // array slice
  PRECEDENCE_IN         = 19;        // enum IN set // officially the same as PRECEDENCE_COMPARE
  PRECEDENCE_COMPARE    = 20;        // a <> b // a=b

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
    FExpressionPart: TFpPascalExpressionPart;
    FPointerLevels: Integer;
    FPointedTo: TFpSymbol;
    FContext: TFpDbgLocationContext;
  protected
    // NameNeeded //  "^TPointedTo"
    procedure TypeInfoNeeded; override;
    function DoReadSize(const AValueObj: TFpValue; out ASize: TFpDbgValueSize): Boolean; override;
  public
    constructor Create(const APointedTo: TFpSymbol; AnExpressionPart: TFpPascalExpressionPart; APointerLevels: Integer);
    constructor Create(const APointedTo: TFpSymbol; AnExpressionPart: TFpPascalExpressionPart);
    destructor Destroy; override;
    function TypeCastValue(AValue: TFpValue): TFpValue; override;
  end;

  { TPasParserSymbolArrayDeIndex }

  TPasParserSymbolArrayDeIndex = class(TFpSymbolForwarder) // 1 index level off
  private
    FExpressionPart: TFpPascalExpressionPart;
    FArray: TFpSymbol;
  protected
    //procedure ForwardToSymbolNeeded; override;
    function GetNestedSymbolCount: Integer; override;
    function GetNestedSymbol(AIndex: Int64): TFpSymbol; override;
  public
    constructor Create(AnExpressionPart: TFpPascalExpressionPart; const AnArray: TFpSymbol);
    destructor Destroy; override;
  end;

  {%endregion  DebugSymbol }

  {%region  DebugSymbolValue }

  { TFpPasParserValue }

  TFpPasParserValue = class(TFpValue)
  private
    FContext: TFpDbgLocationContext;
    FExpressionPart: TFpPascalExpressionPart;
  protected
    function DebugText(AIndent: String): String; virtual;
  public
    constructor Create(AnExpressionPart: TFpPascalExpressionPart);
    property Context: TFpDbgLocationContext read FContext;
  end;

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
    constructor Create(AValue: TFpValue; ATypeInfo: TFpSymbol; AnExpressionPart: TFpPascalExpressionPart);
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
    Result := TFpPasParserValue(AnValue).FExpressionPart.GetFullText(AMaxLen)
  else
  if AnValue.DbgSymbol <> nil then
    Result := AnValue.DbgSymbol.Name
  else
    Result := '?';
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

constructor TFpPasParserValue.Create(AnExpressionPart: TFpPascalExpressionPart);
begin
  FExpressionPart := AnExpressionPart;
  FContext := AnExpressionPart.Expression.Context.LocationContext;
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
      SetLastError(FExpressionPart.CreateErrorWithPos(fpErrCannotCastToPointer_p,
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
    SetLastError(FExpressionPart.CreateErrorWithPos(fpErrCannotDeref_p,
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

constructor TFpPasParserValueCastToPointer.Create(AValue: TFpValue;
  ATypeInfo: TFpSymbol; AnExpressionPart: TFpPascalExpressionPart);
begin
  inherited Create(AnExpressionPart);
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
    FTypeSymbol := TPasParserSymbolPointer.Create(FSourceTypeSymbol, FExpressionPart, FRefLevel);
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
  if not IsReadableLoc(Addr) then exit;
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

  FTypeInfo := TPasParserSymbolPointer.Create(FValue.TypeInfo, FExpressionPart);
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

{ TPasParserSymbolArrayDeIndex }

function TPasParserSymbolArrayDeIndex.GetNestedSymbolCount: Integer;
begin
  Result := (inherited GetNestedSymbolCount) - 1;
end;

function TPasParserSymbolArrayDeIndex.GetNestedSymbol(AIndex: Int64): TFpSymbol;
begin
  Result := inherited GetNestedSymbol(AIndex + 1);
end;

constructor TPasParserSymbolArrayDeIndex.Create(
  AnExpressionPart: TFpPascalExpressionPart; const AnArray: TFpSymbol);
begin
  FExpressionPart := AnExpressionPart;
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
  t := TPasParserSymbolPointer.Create(FPointedTo, FExpressionPart, FPointerLevels-1);
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
  AnExpressionPart: TFpPascalExpressionPart; APointerLevels: Integer);
begin
  inherited Create('');
  FExpressionPart := AnExpressionPart;
  FContext := FExpressionPart.Expression.Context.LocationContext;
  FPointerLevels := APointerLevels;
  FPointedTo := APointedTo;
  FPointedTo.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(FPointedTo, 'TPasParserSymbolPointer'){$ENDIF};
  if APointerLevels = 1 then
    SetTypeInfo(APointedTo);
  SetKind(skPointer);
  SetSymbolType(stType);
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
  Result := TFpPasParserValueCastToPointer.Create(AValue, Self, FExpressionPart);
end;


{ TFpPascalExpressionPartBracketIndex }

procedure TFpPascalExpressionPartBracketIndex.Init;
begin
  FPrecedence := PRECEDENCE_ARRAY_IDX;
  inherited Init;
end;

function TFpPascalExpressionPartBracketIndex.DoGetResultValue: TFpValue;
var
  TmpVal, TmpVal2, TmpIndex: TFpValue;
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
          if IsPChar then FExpression.FHasPCharIndexAccess := True;
          if IsPChar and FExpression.FixPCharIndexAccess then begin
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
          if (Count = 2) and (Items[1] is TFpPascalExpressionPartOperatorArraySlice)
          then begin
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
            TFpValueConstChar(TmpVal2).SetAddress(a + Offs-1);
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
    SetAfterCommaFlag;
    APart := Self;
  end;
end;

{ TFpPascalExpressionPartBracketSet }

function TFpPascalExpressionPartBracketSet.DoGetResultValue: TFpValue;
begin
  Result := TFpValueConstSet.Create(TFpPascalExpressionPartListForwarder.Create(Self, 0, Count));
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

  if Itm0 is TFpPascalExpressionPartIntrinsic then begin
    Result := TFpPascalExpressionPartIntrinsic(Itm0).DoGetResultValue(Self);
    exit;
  end;

  tmp := Itm0.ResultValue;
  if (tmp = nil) or (not Expression.Valid) then
    exit;

  if (tmp.DbgSymbol <> nil) and (tmp.DbgSymbol.Kind in [skFunction, skFunctionRef]) then begin
    if not Assigned(Expression.OnFunctionCall) then begin
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
      if not Expression.OnFunctionCall(Self, tmp, tmpSelf, Params, Result, err) then begin
        if not IsError(err) then
          SetError('unknown error calling function')
        else
          Expression.SetError(err);
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
  if (Count > 1) and (not AfterComma) then begin // Todo a,b,c
    SetError(APart, 'Comma or closing ")" expected: '+GetText+': ');
    APart.Free;
    exit;
  end;
  if not IsValidNextPart(APart) then begin
    SetError(APart, 'Invalid operand in () '+GetText+': ');
    APart.Free;
    exit;
  end;

  Add(APart);
  Result := APart;
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
    SetAfterCommaFlag;
    APart := Self;
  end;
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
  Result := FExpression.GetDbgSymbolForIdentifier(s);
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
  Result := FExpression.GetRegisterValue(GetText);
  {$IFDEF WITH_REFCOUNT_DEBUG}
  if Result <> nil then
    Result.DbgRenameReference(nil, 'DoGetResultValue')
  {$ENDIF}
end;

{ TFpPascalExpressionPartIntrinsic }

function TFpPascalExpressionPartIntrinsic.CheckArgumentCount(
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

function TFpPascalExpressionPartIntrinsic.GetArg(
  AParams: TFpPascalExpressionPartBracketArgumentList; ANum: Integer; out
  AValue: TFpValue; AnErr: String): Boolean;
begin
  AValue := nil;
  Result := ANum < AParams.Count;
  if not Result then begin
    if AnErr <> '' then
      SetError(AnErr);
    exit;
  end;

  AValue := AParams.Items[ANum].ResultValue;
  Result := (AValue <> nil) and (not IsError(Expression.Error)) and (not IsError(AValue.LastError));
  if not Result then begin
    if AnErr <> '' then
      SetError(AnErr);
  end;
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
  FChildClassCastType := Expression.GetDbgSymbolForIdentifier(CastName);
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

    ctx := Expression.Context.LocationContext;
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

function TFpPascalExpressionPartIntrinsic.DoGetResultValue: TFpValue;
begin
  Result := nil;
  SetError('wrong argument count');
  // this gets called, if an intrinsic has no () after it. I.e. no arguments and no empty brackets

  {$IFDEF WITH_REFCOUNT_DEBUG}
  if Result <> nil then
    Result.DbgRenameReference(nil, 'DoGetResultValue')
  {$ENDIF}
end;

function TFpPascalExpressionPartIntrinsic.DoGetResultValue(
  AParams: TFpPascalExpressionPartBracketArgumentList): TFpValue;
begin
  Result := nil;
  case FIntrinsic of
    ifLength:     Result := DoLength(AParams);
    ifChildClass: Result := DoChildClass(AParams);
    ifRefCount:   Result := DoRefCnt(AParams);
    ifPos:        Result := DoPos(AParams);
    ifSubStr:     Result := DoSubStr(AParams);
    ifLower:      Result := DoLower(AParams);
    ifUpper:      Result := DoUpper(AParams);
    ifRound:      Result := DoRound(AParams);
    ifTrunc:      Result := DoTrunc(AParams);
  end;
  {$IFDEF WITH_REFCOUNT_DEBUG}
  if Result <> nil then
    Result.DbgRenameReference(nil, 'DoGetResultValue')
  {$ENDIF}
end;

constructor TFpPascalExpressionPartIntrinsic.Create(
  AExpression: TFpPascalExpression; AStartChar: PChar; AnEndChar: PChar;
  AnIntrinsic: TFpIntrinsicFunc);
begin
  inherited Create(AExpression, AStartChar, AnEndChar);
  FIntrinsic := AnIntrinsic;
end;

destructor TFpPascalExpressionPartIntrinsic.Destroy;
begin
  inherited Destroy;
  FChildClassCastType.ReleaseReference;
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
    Result := TFpValueConstNumber.Create(Int64(i), True); // hex,oct,bin values default to signed
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
  //s := GetText;
  Result := TFpValueConstString.Create(FValue);
  {$IFDEF WITH_REFCOUNT_DEBUG}Result.DbgRenameReference(nil, 'DoGetResultValue'){$ENDIF};
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
    else SetError(AnErrorCode, [GetFirstToken(CurPtr)], CreateError(fpErrPasParser_PositionAfter, [PosFromPChar(CurPtr), PrevPart.GetText(MAX_ERR_EXPR_QUOTE_LEN)]) );
  end;
  procedure SetParserError(AnErrorCode: TFpErrorCode; AData: array of const);
  begin
    if PrevPart = nil
    then SetError(AnErrorCode, AData, CreateError(fpErrPasParser_AtStart, []) )
    else SetError(AnErrorCode, AData, CreateError(fpErrPasParser_PositionAfter, [PosFromPChar(CurPtr), PrevPart.GetText(MAX_ERR_EXPR_QUOTE_LEN)]) );
  end;
  procedure SetParserErrorPosOnly(AnErrorCode: TFpErrorCode);
  begin
    if PrevPart = nil
    then SetError(AnErrorCode, [], CreateError(fpErrPasParser_AtStart, []) )
    else SetError(AnErrorCode, [], CreateError(fpErrPasParser_PositionAfter, [PosFromPChar(CurPtr), PrevPart.GetText(MAX_ERR_EXPR_QUOTE_LEN)]) );
  end;

  procedure AddPart(AClass: TFpPascalExpressionPartClass);
  begin
    NewPart := AClass.Create(Self, CurPtr, TokenEndPtr-1);
  end;

  procedure AddPlusMinus;
  begin
    if (CurPart = nil) or (not CurPart.CanHaveOperatorAsNext)
    then AddPart(TFpPascalExpressionPartOperatorUnaryPlusMinus)
    else AddPart(TFpPascalExpressionPartOperatorPlusMinus);
  end;

  procedure AddIntrinsic(AnIntrinsic: TFpIntrinsicFunc);
  begin
    if AnIntrinsic = ifErrorNotFound then
      SetParserError(fpErrPasParserUnknownIntrinsic_p)
    else
      NewPart := TFpPascalExpressionPartIntrinsic.Create(Self, CurPtr, TokenEndPtr-1, AnIntrinsic);
  end;

  procedure AddIntrinsic;
  var
    intr: TFpIntrinsicFunc;
  begin
    while TokenEndPtr^ in ['a'..'z', 'A'..'Z', '_', '0'..'9', '$'] do
      inc(TokenEndPtr);
    intr := LookupIntrinsic(CurPtr, TokenEndPtr - CurPtr);
    AddIntrinsic(intr);
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
  var
    intr: TFpIntrinsicFunc;
  begin
    while TokenEndPtr^ in ['a'..'z', 'A'..'Z', '_', '0'..'9', '$'] do
      inc(TokenEndPtr);
    // TODO: Check functions not, and, in, as, is ...
    if (CurPart <> nil) and (CurPart.CanHaveOperatorAsNext) then
    case TokenEndPtr - CurPtr of
      3: case chr(ord(CurPtr^) AND $DF) of
          'D': if CheckToken('IV', CurPtr) then
              NewPart := TFpPascalExpressionPartOperatorMulDiv.Create(Self, CurPtr, TokenEndPtr-1);
          'M': if CheckToken('OD', CurPtr) then
              NewPart := TFpPascalExpressionPartOperatorMulDiv.Create(Self, CurPtr, TokenEndPtr-1);
          'A': if CheckToken('ND', CurPtr) then
              NewPart := TFpPascalExpressionPartOperatorAnd.Create(Self, CurPtr, TokenEndPtr-1);
          'X': if CheckToken('OR', CurPtr) then
              NewPart := TFpPascalExpressionPartOperatorOr.Create(Self, ootXor, CurPtr, TokenEndPtr-1);
          'N': if CheckToken('OT', CurPtr) then
              NewPart := TFpPascalExpressionPartOperatorUnaryNot.Create(Self, CurPtr, TokenEndPtr-1);
        end;
      2: case chr(ord(CurPtr^) AND $DF) of
          'I': if CheckToken('N', CurPtr) then
              NewPart := TFpPascalExpressionPartOperatorMemberIn.Create(Self, CurPtr, TokenEndPtr-1);
          'O': if CheckToken('R', CurPtr) then
              NewPart := TFpPascalExpressionPartOperatorOr.Create(Self, ootOr, CurPtr, TokenEndPtr-1);
        end;
    end
    else
    case TokenEndPtr - CurPtr of
      3: case chr(ord(CurPtr^) AND $DF) of
          'N': if CheckToken('OT', CurPtr) then
              NewPart := TFpPascalExpressionPartOperatorUnaryNot.Create(Self, CurPtr, TokenEndPtr-1);
        end;
    end;

    if (FIntrinsicPrefix = ipNoPrefix) then begin
      intr := LookupIntrinsic(CurPtr, TokenEndPtr - CurPtr);
      if (intr <> ifErrorNotFound) and CheckOpenBracket then begin
        AddIntrinsic(intr);
        exit;
      end;
    end;

    if NewPart = nil then
      NewPart := TFpPascalExpressionPartIdentifier.Create(Self, CurPtr, TokenEndPtr-1);
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
      SetParserError(fpErrPasParserWrongOpenBracket_p, [GetFirstToken(CurPtr), PosFromPChar(BracketPart.StartChar), BracketPart.GetText(MAX_ERR_EXPR_QUOTE_LEN)]);
    end
    else begin
      TFpPascalExpressionPartBracket(BracketPart).CloseBracket;
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
             NewPart := TFpPascalExpressionPartIdentifier.Create(Self, CurPtr, TokenEndPtr-1);
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
      SetError(fpErrPasParserExpectedNumber_p, [GetFirstToken(CurPtr+1)], CreateError(fpErrPasParser_PositionAfter, [PosFromPChar(CurPtr+1), '#']) );
      exit;
    end
    else
      AddPart(TFpPascalExpressionPartConstantNumber);
  end;

  procedure HandleCompare;
  begin
    if (CurPtr^ = '<') and (TokenEndPtr^ in ['>', '=']) then
      inc(TokenEndPtr);
    if (CurPtr^ = '>') and (TokenEndPtr^ in ['<', '=']) then
      inc(TokenEndPtr);
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
    while (TokenEndPtr < EndPtr) and FValid do begin
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
                    SetError(fpErrPasParserExpectedNumber_p, [GetFirstToken(CurPtr+1)], CreateError(fpErrPasParser_PositionAfter, [PosFromPChar(CurPtr+1), '#']) );
                    exit;
                  end;
                  while (TokenEndPtr < EndPtr) and (TokenEndPtr^ in ['0'..'9', 'a'..'f', 'A'..'F']) do
                    inc(TokenEndPtr);
                end;
              '&': begin
                  inc(TokenEndPtr);
                  if (not (TokenEndPtr < EndPtr)) or (not (TokenEndPtr^ in ['0'..'7'])) then begin
                    SetError(fpErrPasParserExpectedNumber_p, [GetFirstToken(CurPtr+1)], CreateError(fpErrPasParser_PositionAfter, [PosFromPChar(CurPtr+1), '#']) );
                    exit;
                  end;
                  while (TokenEndPtr < EndPtr) and (TokenEndPtr^ in ['0'..'7']) do
                    inc(TokenEndPtr);
                end;
              '%': begin
                  inc(TokenEndPtr);
                  if (not (TokenEndPtr < EndPtr)) or (not (TokenEndPtr^ in ['0'..'1'])) then begin
                    SetError(fpErrPasParserExpectedNumber_p, [GetFirstToken(CurPtr+1)], CreateError(fpErrPasParser_PositionAfter, [PosFromPChar(CurPtr+1), '#']) );
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
                  SetError(fpErrPasParserExpectedNumber_p, [GetFirstToken(CurPtr+1)], CreateError(fpErrPasParser_PositionAfter, [PosFromPChar(CurPtr+1), '#']) );
                  exit;
                end;
            end;
            c := StrToIntDef(copy(p , 1 , TokenEndPtr - p), -1);
            if (c < 0) or ( (TokenEndPtr < EndPtr) and (TokenEndPtr^ in ['0'..'9', 'a'..'z', 'A'..'Z']) )
            then begin
              SetError(fpErrPasParserExpectedNumber_p, [GetFirstToken(CurPtr+1)], CreateError(fpErrPasParser_PositionAfter, [PosFromPChar(CurPtr+1), '#']) );
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
    if not FValid then
      exit;
    // If Length(str) = 1 then // char
    AddPart(TFpPascalExpressionPartConstantText);
    TFpPascalExpressionPartConstantText(NewPart).FValue := str;
  end;

begin
  if FTextExpression = '' then begin
    SetError(fpErrPasParserEmptyExpression);
    exit;
  end;
  CurPtr := @FTextExpression[1];
  EndPtr := CurPtr + length(FTextExpression);
  while (CurPtr^ in [' ', #9, #10, #13]) and (CurPtr < EndPtr) do
    Inc(CurPtr);
  if CurPtr = EndPtr then begin
    SetError(fpErrPasParserEmptyExpression);
    exit;
  end;


  CurPart := nil;
  PrevPart := nil;
  While (CurPtr < EndPtr) and FValid do begin
    if CurPtr^ in [' ', #9, #10, #13] then begin
      while (CurPtr^ in [' ', #9, #10, #13]) and (CurPtr < EndPtr) do
        Inc(CurPtr);
      continue;
    end;

    NewPart := nil;
    TokenEndPtr := CurPtr + 1;
    if (FIntrinsicPrefix = ipExclamation) and (CurPtr^ = '!') then begin
      inc(CurPtr);
      AddIntrinsic;
    end
    else
    if (FIntrinsicPrefix = ipColon) and (CurPtr^ = ':') then begin
      inc(CurPtr);
      AddIntrinsic;
    end
    else
    case CurPtr^ of
      '@' :      AddPart(TFpPascalExpressionPartOperatorAddressOf);
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
    if not FValid then
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

  FExpressionPart := CurPart;
end;

function TFpPascalExpression.GetResultValue: TFpValue;
begin
  if (FExpressionPart = nil) or (not Valid) then
    Result := nil
  else begin
    Result := FExpressionPart.ResultValue;
    if (Result = nil) and (not IsError(FError)) then
      SetError(fpErrAnyError, ['Internal eval error']);
  end;
end;

function TFpPascalExpression.GetValid: Boolean;
begin
  Result := FValid and (not IsError(FError));
end;

procedure TFpPascalExpression.SetError(AMsg: String);
begin
  if IsError(FError) then begin
DebugLn(DBG_WARNINGS, ['Skipping error ', AMsg]);
    FValid := False;
    exit;
  end;
  SetError(fpErrAnyError, [AMsg]);
DebugLn(DBG_WARNINGS, ['PARSER ERROR ', AMsg]);
end;

procedure TFpPascalExpression.SetError(AnErrorCode: TFpErrorCode;
  const AnNestedErr: TFpError);
begin
  SetError(AnErrorCode, [], AnNestedErr);
end;

procedure TFpPascalExpression.SetError(AnErrorCode: TFpErrorCode;
  AData: array of const; const AnNestedErr: TFpError);
begin
  FValid := False;
  FError := ErrorHandler.CreateError(AnErrorCode, AnNestedErr, AData);
  DebugLn(DBG_WARNINGS, ['Setting error ', ErrorHandler.ErrorAsString(FError)]);
end;

procedure TFpPascalExpression.SetError(const AnErr: TFpError);
begin
  FValid := False;
  FError := AnErr;
  DebugLn(DBG_WARNINGS, ['Setting error ', ErrorHandler.ErrorAsString(FError)]);
end;

function TFpPascalExpression.PosFromPChar(APChar: PChar): Integer;
begin
  Result := APChar - @FTextExpression[1] + 1;
end;

function TFpPascalExpression.LookupIntrinsic(AStart: PChar; ALen: Integer
  ): TFpIntrinsicFunc;
begin
  Result := ifErrorNotFound;
  case ALen of
    2: if strlicomp(AStart, 'CC', 2) = 0     then Result := ifChildClass;
    3: case AStart^ of
        'l', 'L': if strlicomp(AStart, 'LEN', 3) = 0 then Result := ifLength;
        'p', 'P': if strlicomp(AStart, 'POS', 3) = 0 then Result := ifPos;
    end;
    5: case AStart^ of
        'l', 'L': if strlicomp(AStart, 'LOWER', 5) = 0 then Result := ifLower;
        'u', 'U': if strlicomp(AStart, 'UPPER', 5) = 0 then Result := ifUpper;
        'r', 'R': if strlicomp(AStart, 'ROUND', 5) = 0 then Result := ifRound;
        't', 'T': if strlicomp(AStart, 'TRUNC', 5) = 0 then Result := ifTrunc;
       end;
    6: case AStart^ of
        'l', 'L': if strlicomp(AStart, 'LENGTH', 6) = 0 then Result := ifLength;
        'r', 'R': if strlicomp(AStart, 'REFCNT', 6) = 0 then Result := ifRefCount;
        's', 'S': if strlicomp(AStart, 'SUBSTR', 6) = 0 then Result := ifSubStr;
      end;
  end;
end;

function TFpPascalExpression.GetDbgSymbolForIdentifier(AnIdent: String): TFpValue;
begin
  if FContext <> nil then
    Result := FContext.FindSymbol(AnIdent)
  else
    Result := nil;
end;

function TFpPascalExpression.GetRegisterValue(AnIdent: String): TFpValue;
var
  RNum: Cardinal;
  RSize: Integer;
  RVal: QWord;
begin
  Result := nil;
  if FContext = nil then
    exit;

  if not FContext.MemManager.RegisterNumber(AnIdent, RNum) then
    exit;

  RSize := FContext.MemManager.RegisterSize(RNum);
  if RSize > 8 then
    exit;
  if not FContext.LocationContext.ReadUnsignedInt(RegisterLoc(RNum), SizeVal(RSize), RVal) then
    exit;

  Result := TFpValueConstNumber.Create(RVal, False);
end;

constructor TFpPascalExpression.Create(ATextExpression: String;
  AContext: TFpDbgSymbolScope; ASkipParse: Boolean);
begin
  FContext := AContext;
  FContext.AddReference;
  FTextExpression := ATextExpression;
  FError := NoError;
  FValid := True;
  if not ASkipParse then
    Parse;
end;

destructor TFpPascalExpression.Destroy;
begin
  FreeAndNil(FExpressionPart);
  FContext.ReleaseReference;
  inherited Destroy;
end;

function TFpPascalExpression.DebugDump(AWithResults: Boolean): String;
begin
  Result := 'TFpPascalExpression: ' + FTextExpression + LineEnding +
            'Valid: ' + dbgs(FValid) + '   Error: "' + dbgs(ErrorCode(FError)) + '"'+ LineEnding
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
  Result := FExpression.PosFromPChar(FStartChar);
end;

function TFpPascalExpressionPart.GetFullText(AMaxLen: Integer): String;
begin
  Result := GetText(AMaxLen);
end;

procedure TFpPascalExpressionPart.SetError(AMsg: String);
begin
  if AMsg = '' then
    AMsg := 'Invalid Expression';
  FExpression.SetError(Format('%0:s at %1:d: "%2:s"', [AMsg, FExpression.PosFromPChar(FStartChar), GetText(20)]));
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
  FExpression.SetError(CreateErrorWithPos(AnErrorCode, AData));
end;

procedure TFpPascalExpressionPart.SetError(AnErrorCode: TFpErrorCode;
  AData: array of const; const AnNestedErr: TFpError);
begin
  FExpression.SetError(AnErrorCode, AData, AnNestedErr);
end;

procedure TFpPascalExpressionPart.SetError(AnError: TFpError);
begin
  FExpression.SetError(AnError);
end;

procedure TFpPascalExpressionPart.Init;
begin
  //
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

function TFpPascalExpressionPart.DebugText(AIndent: String; AWithResults: Boolean): String;
begin
  Result := Format('%s%s at %d: "%s"',
                   [AIndent, ClassName, FExpression.PosFromPChar(FStartChar), GetText])
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

constructor TFpPascalExpressionPart.Create(AExpression: TFpPascalExpression; AStartChar: PChar;
  AnEndChar: PChar);
begin
  FExpression := AExpression;
  FStartChar := AStartChar;
  FEndChar := AnEndChar;
  //FResultTypeFlag := rtUnknown;
  FResultValDone := False;
  Init;
end;

destructor TFpPascalExpressionPart.Destroy;
begin
  inherited Destroy;
  //FResultType.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(nil, 'DoGetResultType'){$ENDIF};
  FResultValue.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FResultValue, 'DoGetResultValue'){$ENDIF};
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

procedure TFpPascalExpressionPartContainer.ResetEvaluationRecursive;
var
  i: Integer;
begin
  inherited ResetEvaluationRecursive;
  for i := 0 to Count - 1 do
    Items[i].ResetEvaluationRecursive;
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

function TFpPascalExpressionPartContainer.GetFullText(AMaxLen: Integer): String;
var
  s, e: PChar;
  i: Integer;
  p: TFpPascalExpressionPart;
  Len: Integer;
begin
  s := FStartChar;
  e := FEndChar;
  for i := 0 to Count - 1 do begin
    p := Items[i];
    if p.FStartChar < s then s := p.FStartChar;
    if p.FEndChar   > e then e := p.FEndChar;
  end;

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

procedure TFpPascalExpressionPartBracket.CloseBracket;
begin
  if AfterComma then begin
    SetError(fpErrPasParserMissingExprAfterComma, [GetText(MAX_ERR_EXPR_QUOTE_LEN), GetPos]);
    exit;
  end;
  FIsClosing := True;
  if LastItem <> nil then
    LastItem.HandleEndOfExpression;
  FIsClosing := False;
  FIsClosed := True;
end;

function TFpPascalExpressionPartBracket.HandleNextPart(APart: TFpPascalExpressionPart): TFpPascalExpressionPart;
begin
  if IsClosed then begin
    Result := inherited HandleNextPart(APart);
    exit;
  end;

  if not IsValidNextPart(APart) then begin
    SetError(APart, 'Invalid operand in () '+GetText+': ');
    Result := self;
    APart.Free;
    exit;
  end;

  Result := HandleNextPartInBracket(APart);
end;

procedure TFpPascalExpressionPartBracket.HandleEndOfExpression;
begin
  if not FIsClosing then
    inherited HandleEndOfExpression;
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

  // BinaryOperator...
  //   foo
  //   Identifier
  // "Identifier" can hane a binary-op next. But it must be applied to the parent.
  // So it is not valid here.
  // If new operator has a higher precedence, it go down to the child again and replace it
  if (APrevPart.Parent <> nil) and (APrevPart.Parent.HasPrecedence) then
    Result := False;
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
    if (svfDataAddress in tmp.FieldFlags) and (IsReadableLoc(tmp.DerefAddress)) and // TODO, what if Not readable addr
       (tmp.TypeInfo <> nil) //and (tmp.TypeInfo.TypeInfo <> nil)
    then begin
      Result := tmp.Member[0];
      if Result <> nil then
        {$IFDEF WITH_REFCOUNT_DEBUG}Result.DbgRenameReference(nil, 'DoGetResultValue'){$ENDIF};

    end;
  end
  //if tmp.Kind = skArray then // dynarray
  else
  begin
    Result := nil;
    SetErrorWithPos(fpErrCannotDeref_p, [GetText]);
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
    r: TStringList;
    i: Integer;
    m: TFpValue;
    s: String;
  begin
    Result := nil;
    case AOtherVal.Kind of
      skSet: begin
        r := TStringList.Create;
        r.CaseSensitive := False;
        for i := 0 to ASetVal.MemberCount - 1 do begin
          m := ASetVal.Member[i];
          s := m.AsString;
          m.ReleaseReference;
          r.Add(s);
        end;
        for i := 0 to AOtherVal.MemberCount - 1 do begin
          m := AOtherVal.Member[i];
          s := m.AsString;
          m.ReleaseReference;
          if r.IndexOf(s) < 0 then
            r.Add(s);
        end;
        Result := TFpValueConstSet.Create(r);
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
    r: TStringList;
    i, j: Integer;
    m: TFpValue;
    s: String;
  begin
    Result := nil;
    case AOtherVal.Kind of
      skSet: begin
        r := TStringList.Create;
        r.CaseSensitive := False;
        for i := 0 to ASetVal.MemberCount - 1 do begin
          m := ASetVal.Member[i];
          s := m.AsString;
          m.ReleaseReference;
          r.Add(s);
        end;
        for i := 0 to AOtherVal.MemberCount - 1 do begin
          m := AOtherVal.Member[i];
          s := m.AsString;
          m.ReleaseReference;
          j := r.IndexOf(s);
          if j >= 0 then
            r.Delete(j);
        end;
        Result := TFpValueConstSet.Create(r);
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
    r: TStringList;
    i, j: Integer;
    m: TFpValue;
    s, s1, s2: String;
  begin
    Result := nil;
    case AOtherVal.Kind of
      skSet: begin
        r := TStringList.Create;
        for i := 0 to ASetVal.MemberCount - 1 do begin
          m := ASetVal.Member[i];
          s := m.AsString;
          s1 := LowerCase(s);
          m.ReleaseReference;
          for j := 0 to AOtherVal.MemberCount - 1 do begin
            m := AOtherVal.Member[j];
            s2 := LowerCase(m.AsString);
            m.ReleaseReference;
            if s1 = s2 then
              r.Add(s);
          end;
        end;
        Result := TFpValueConstSet.Create(r);
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
    case AOtherVal.Kind of
      skInteger:  Result := TFpValueConstNumber.Create(AIntVal.AsInteger div AOtherVal.AsInteger, True);
      skCardinal: Result := TFpValueConstNumber.Create(AIntVal.AsInteger div AOtherVal.AsCardinal, True);
      else SetError('Div not supported');
    end;
  end;
  function NumDivCardinalByValue(ACardinalVal, AOtherVal: TFpValue): TFpValue;
  begin
    Result := nil;
    case AOtherVal.Kind of
      skInteger:  Result := TFpValueConstNumber.Create(ACardinalVal.AsCardinal div AOtherVal.AsInteger, True);
      skCardinal: Result := TFpValueConstNumber.Create(ACardinalVal.AsCardinal div AOtherVal.AsCardinal, False);
      else SetError('Div not supported');
    end;
  end;

  function NumModIntByValue(AIntVal, AOtherVal: TFpValue): TFpValue;
  begin
    Result := nil;
    case AOtherVal.Kind of
      skInteger:  Result := TFpValueConstNumber.Create(AIntVal.AsInteger mod AOtherVal.AsInteger, True);
      skCardinal: Result := TFpValueConstNumber.Create(AIntVal.AsInteger mod AOtherVal.AsCardinal, True);
      else SetError('Div not supported');
    end;
  end;
  function NumModCardinalByValue(ACardinalVal, AOtherVal: TFpValue): TFpValue;
  begin
    Result := nil;
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
                 Result := TFpValueConstBool.Create(tmp1.AsBool AND tmp2.AsBool);
  end;
  {$POP}

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
                   Result := TFpValueConstBool.Create(tmp1.AsBool OR tmp2.AsBool);
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
                   Result := TFpValueConstBool.Create(tmp1.AsBool XOR tmp2.AsBool);
    end;
  end;
  {$POP}

 {$IFDEF WITH_REFCOUNT_DEBUG}if Result <> nil then Result.DbgRenameReference(nil, 'DoGetResultValue');{$ENDIF}
end;

constructor TFpPascalExpressionPartOperatorOr.Create(
  AExpression: TFpPascalExpression; AnOp: TOpOrType; AStartChar: PChar;
  AnEndChar: PChar);
begin
  inherited Create(AExpression, AStartChar, AnEndChar);
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
    if AOtherVal.Kind in [skClass,skInterface,skAddress,skPointer] then
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

  function SymDiffSets(ASetVal, AOtherVal: TFpValue): TFpValue;
  var
    r: TStringList;
    i, j, c: Integer;
    m: TFpValue;
    s: String;
  begin
    Result := nil;
    r := TStringList.Create;
    r.CaseSensitive := False;
    for i := 0 to ASetVal.MemberCount - 1 do begin
      m := ASetVal.Member[i];
      s := m.AsString;
      m.ReleaseReference;
      if r.IndexOf(s) < 0 then
        r.Add(s);
    end;
    c := r.Count;
    for i := 0 to AOtherVal.MemberCount - 1 do begin
      m := AOtherVal.Member[i];
      s := m.AsString;
      m.ReleaseReference;
      j := r.IndexOf(s);
      if j < c then begin // otherwise the 2nd set has duplicate idents
        if j >= 0 then begin
          r.Delete(j);
          dec(c)
        end
        else
          r.Add(s);
      end;
    end;
    Result := TFpValueConstSet.Create(r);
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
    end;
  end
  else
  if GetText = '><' then begin
    if (tmp1.Kind = skSet) and (tmp2.Kind = skSet) then
      Result := SymDiffSets(tmp1, tmp2);
  end;

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
  tmp: TFpValue;
  MemberName: String;
  MemberSym: TFpSymbol;
  {$IFDEF FpDebugAutoDerefMember}
  tmp2: TFpValue;
  {$ENDIF}
begin
  Result := nil;
  if Count <> 2 then exit;

  tmp := Items[0].ResultValue;
  if (tmp = nil) then
    exit;

  MemberName := Items[1].GetText;

  {$IFDEF FpDebugAutoDerefMember}
  // Copy from TFpPascalExpressionPartOperatorDeRef.DoGetResultValue
  tmp2 := nil;
  if tmp.Kind = skPointer then begin
    if (svfDataAddress in tmp.FieldFlags) and (IsReadableLoc(tmp.DerefAddress)) and // TODO, what if Not readable addr
       (tmp.TypeInfo <> nil) //and (tmp.TypeInfo.TypeInfo <> nil)
    then begin
      tmp := tmp.Member[0];
      tmp2 := tmp;
    end;
    if (tmp = nil) then begin
      SetErrorWithPos(MsgfpErrCannotDeref_p, [Items[0].GetText]); // TODO: better error
      exit;
    end;
  end;
  {$ENDIF}

  if (tmp.Kind in [skClass, skRecord, skObject]) then begin
    Result := tmp.MemberByName[MemberName];
    if Result = nil then begin
      SetError(fpErrNoMemberWithName, [MemberName]);
      exit;
    end;
    {$IFDEF WITH_REFCOUNT_DEBUG}Result.DbgRenameReference(nil, 'DoGetResultValue'){$ENDIF};
    Assert((Result.DbgSymbol=nil)or(Result.DbgSymbol.SymbolType=stValue), 'member is value');
    exit;
  end;
  {$IFDEF FpDebugAutoDerefMember}
  tmp2.ReleaseReference;
  {$ENDIF}

  if (tmp.Kind in [skType]) and
     (tmp.DbgSymbol <> nil) and (tmp.DbgSymbol.Kind in [skClass, skRecord, skObject])
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

  if (tmp.Kind = skUnit) or
     ( (tmp.DbgSymbol <> nil) and (tmp.DbgSymbol.Kind = skUnit) )
  then begin
    (* If a class/record/object matches the typename, but did not have the member,
       then this could still be a unit.
       If the class/record/object is in the same unit as the current contexct (selected function)
       then it would hide the unitname, but otherwise a unit in the uses clause would
       hide the structure.
    *)
    Result := Expression.FContext.FindSymbol(MemberName, Items[0].GetText);
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
  s, s2: String;
  i: Integer;
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


  if AVal.Kind <> skEnumValue then begin
    SetError('"in" requires an enum');
    exit;
  end;


  s := LowerCase(AVal.AsString);

  for i := 0 to ASet.MemberCount-1 do begin
    m := ASet.Member[i];
    s2 := LowerCase(m.AsString);
    m.ReleaseReference;
    if s = s2 then begin
      Result := TFpValueConstBool.Create(True);
      {$IFDEF WITH_REFCOUNT_DEBUG}
      if Result <> nil then
        Result.DbgRenameReference(nil, 'DoGetResultValue');{$ENDIF}
      exit;
    end;
  end;

  Result := TFpValueConstBool.Create(False);
  {$IFDEF WITH_REFCOUNT_DEBUG}
  if Result <> nil then
    Result.DbgRenameReference(nil, 'DoGetResultValue');{$ENDIF}
end;

type

  { TFpPasParserValueSlicedArrayIndex }

  TFpPasParserValueSlicedArrayIndex = class(TFpSymbol)
  private
    FLowBound: Int64;
  public
    function GetValueLowBound(AValueObj: TFpValue; out ALowBound: Int64): Boolean; override;
  end;

  { TFpPasParserValueSlicedArray }

  TFpPasParserValueSlicedArray = class(TFpPasParserValue)
  private
    FLowBoundIndex: TFpPasParserValueSlicedArrayIndex;
    FArraySlice: TFpPascalExpressionPartOperatorArraySliceController;
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
  public
    constructor Create(ASlice: TFpPascalExpressionPartOperatorArraySliceController);
    destructor Destroy; override;
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
  if SlicePart.FCurrentIndex = AIndex then begin
    Result := FArraySlice.Items[0].ResultValue;
    if Result <> nil then begin
      Result.AddReference;
      Result.Reset;
    end;
    exit;
  end;

  SlicePart.FCurrentIndex := AIndex;
  FArraySlice.ResetEvaluationForIndex;
  Result := FArraySlice.Items[0].ResultValue;
  if Result <> nil then begin
    Result.AddReference;
    Result.Reset;
  end;
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

constructor TFpPasParserValueSlicedArray.Create(ASlice: TFpPascalExpressionPartOperatorArraySliceController);
begin
  inherited Create(ASlice);
  FArraySlice := ASlice;
  FLowBoundIndex := TFpPasParserValueSlicedArrayIndex.Create('');
  FLowBoundIndex.FLowBound := SlicePart.StartValue;
end;

destructor TFpPasParserValueSlicedArray.Destroy;
begin
  inherited Destroy;
  FLowBoundIndex.ReleaseReference;
end;

{ TFpPascalExpressionPartOperatorArraySliceController }

function TFpPascalExpressionPartOperatorArraySliceController.HandleNextPart
  (APart: TFpPascalExpressionPart): TFpPascalExpressionPart;
begin
  SetError(APart, 'array slice error');
  APart.Free;
  Result := Self;
end;

function TFpPascalExpressionPartOperatorArraySliceController.DoGetResultValue: TFpValue;
var
  tmp: TFpValue;
begin
  if (FSlicePart.Parent <> nil) and
     (FSlicePart.Parent is TFpPascalExpressionPartBracketIndex) and
     (FSlicePart.Parent.Count = 2) // variable and ONE index
  then begin
    tmp := FSlicePart.Parent.Items[0].ResultValue;
    if (tmp <> nil) and (Tmp.Kind in [skString, skAnsiString, skWideString])
    then begin
      // Handled in TFpPascalExpressionPartBracketIndex.DoGetResultValue
      Result := Items[0].ResultValue;
      if Result <> nil then
        Result.AddReference;
      exit;
    end;
  end;

  Result := TFpPasParserValueSlicedArray.Create(Self);
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
  AExpression: TFpPascalExpression;
  ASlicePart: TFpPascalExpressionPartOperatorArraySlice;
  ATopPart: TFpPascalExpressionPart; AStartChar: PChar; AnEndChar: PChar);
begin
  inherited Create(AExpression, AStartChar, AnEndChar);
  FSlicePart := ASlicePart;

  while ATopPart is TFpPascalExpressionPartOperatorArraySliceController do
    ATopPart := TFpPascalExpressionPartOperatorArraySliceController(ATopPart).Items[0];
  ATopPart.ReplaceInParent(Self);
  Add(ATopPart);
end;

{ TFpPascalExpressionPartOperatorArraySlice }

procedure TFpPascalExpressionPartOperatorArraySlice.Init;
begin
  inherited Init;
  FPrecedence := PRECEDENCE_ARRAY_SLICE;
end;

procedure TFpPascalExpressionPartOperatorArraySlice.SetParent(
  AValue: TFpPascalExpressionPartContainer);
begin
  if (Parent = nil) and (AValue <> nil) then
    TFpPascalExpressionPartOperatorArraySliceController.Create(FExpression,
      Self, AValue.TopParent, FStartChar, FEndChar);

  inherited SetParent(AValue);
end;

function TFpPascalExpressionPartOperatorArraySlice.DoGetResultValue: TFpValue;
begin
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
begin
  Result := 0;
  if Count < 2 then exit;
  tmp := Items[1].ResultValue;
  if tmp <> nil then
    Result := tmp.AsInteger;
end;

initialization
  DBG_WARNINGS := DebugLogger.FindOrRegisterLogGroup('DBG_WARNINGS' {$IFDEF DBG_WARNINGS} , True {$ENDIF} );

end.

