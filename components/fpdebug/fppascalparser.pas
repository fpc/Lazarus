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

interface

uses
  Classes, sysutils, math, DbgIntfBaseTypes, FpDbgInfo, FpdMemoryTools, FpErrorMessages,
  LazLoggerBase, LazClasses;

type

  TFpPascalExpressionPart = class;
  TFpPascalExpressionPartContainer = class;
  TFpPascalExpressionPartWithPrecedence = class;
  TFpPascalExpressionPartBracket = class;
  TFpPascalExpressionPartOperator = class;

  TFpPascalExpressionPartClass = class of TFpPascalExpressionPart;
  TFpPascalExpressionPartBracketClass = class of TFpPascalExpressionPartBracket;

  TSeparatorType = (ppstComma);

  { TFpPascalExpression }

  TFpPascalExpression = class
  private
    FError: TFpError;
    FContext: TFpDbgInfoContext;
    FFixPCharIndexAccess: Boolean;
    FHasPCharIndexAccess: Boolean;
    FTextExpression: String;
    FExpressionPart: TFpPascalExpressionPart;
    FValid: Boolean;
    function GetResultValue: TFpDbgValue;
    function GetValid: Boolean;
    procedure Parse;
    procedure SetError(AMsg: String);  // deprecated;
    procedure SetError(AnErrorCode: TFpErrorCode; AData: array of const);
    function PosFromPChar(APChar: PChar): Integer;
  protected
    function GetDbgSymbolForIdentifier({%H-}AnIdent: String): TFpDbgValue;
    property ExpressionPart: TFpPascalExpressionPart read FExpressionPart;
    property Context: TFpDbgInfoContext read FContext;
  public
    constructor Create(ATextExpression: String; AContext: TFpDbgInfoContext);
    destructor Destroy; override;
    function DebugDump(AWithResults: Boolean = False): String;
    procedure ResetEvaluation;
    property TextExpression: String read FTextExpression;
    property Error: TFpError read FError;
    property Valid: Boolean read GetValid;
    // Set by GetResultValue
    property HasPCharIndexAccess: Boolean read FHasPCharIndexAccess;
    // handle pchar as string (adjust index)
    property FixPCharIndexAccess: Boolean read FFixPCharIndexAccess write FFixPCharIndexAccess;
    // ResultValue
    // - May be a type, if expression is a type
    // - Only valid, as long as the expression is not destroyed
    property ResultValue: TFpDbgValue read GetResultValue;
  end;


  { TFpPascalExpressionPart }

  TFpPascalExpressionPart = class
  private
    FEndChar: PChar;
    FParent: TFpPascalExpressionPartContainer;
    FStartChar: PChar;
    FExpression: TFpPascalExpression;
    FResultValue: TFpDbgValue;
    FResultValDone: Boolean;
    function GetResultValue: TFpDbgValue;
    function GetSurroundingOpenBracket: TFpPascalExpressionPartBracket;
    function GetTopParent: TFpPascalExpressionPart;
    procedure SetEndChar(AValue: PChar);
    procedure SetParent(AValue: TFpPascalExpressionPartContainer);
    procedure SetStartChar(AValue: PChar);
    procedure SetError(AMsg: String = ''); // deprecated;
    procedure SetError(APart: TFpPascalExpressionPart; AMsg: String = ''); // deprecated;
    procedure SetError(AnErrorCode: TFpErrorCode; AData: array of const);
  protected
    function DebugText(AIndent: String; {%H-}AWithResults: Boolean): String; virtual; // Self desc only
    function DebugDump(AIndent: String; AWithResults: Boolean): String; virtual;
  protected
    procedure Init; virtual;
    function  DoGetIsTypeCast: Boolean; virtual; deprecated;
    function  DoGetResultValue: TFpDbgValue; virtual;
    procedure ResetEvaluation;

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
    function HandleSeparator(ASeparatorType: TSeparatorType): Boolean; virtual; // False
    property Expression: TFpPascalExpression read FExpression;
  public
    constructor Create(AExpression: TFpPascalExpression; AStartChar: PChar; AnEndChar: PChar = nil);
    destructor Destroy; override;
    function  HandleNextPart(APart: TFpPascalExpressionPart): TFpPascalExpressionPart; virtual;
    procedure HandleEndOfExpression; virtual;

    function GetText(AMaxLen: Integer=0): String;
    property StartChar: PChar read FStartChar write SetStartChar;
    property EndChar: PChar read FEndChar write SetEndChar;
    property Parent: TFpPascalExpressionPartContainer read FParent write SetParent;
    property TopParent: TFpPascalExpressionPart read GetTopParent; // or self
    property SurroundingBracket: TFpPascalExpressionPartBracket read GetSurroundingOpenBracket; // incl self
    property ResultValue: TFpDbgValue read GetResultValue;
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
    function DebugDump(AIndent: String; AWithResults: Boolean): String; override;
  public
    destructor Destroy; override;
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
    function DoGetResultValue: TFpDbgValue; override;
  end;

  TFpPascalExpressionPartConstant = class(TFpPascalExpressionPartContainer)
  end;

  { TFpPascalExpressionPartConstantNumber }

  TFpPascalExpressionPartConstantNumber = class(TFpPascalExpressionPartConstant)
  protected
    function DoGetResultValue: TFpDbgValue; override;
  end;

  { TFpPascalExpressionPartConstantNumberFloat }

  TFpPascalExpressionPartConstantNumberFloat = class(TFpPascalExpressionPartConstantNumber)
  protected
    function DoGetResultValue: TFpDbgValue; override;
  end;

  TFpPascalExpressionPartConstantText = class(TFpPascalExpressionPartConstant)
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
    function DoGetResultValue: TFpDbgValue; override;
  end;

  { TFpPascalExpressionPartBracketArgumentList }

  TFpPascalExpressionPartBracketArgumentList = class(TFpPascalExpressionPartRoundBracket)
  // function arguments or type cast // this acts a operator: first element is the function/type
  protected
    procedure Init; override;
    function DoGetResultValue: TFpDbgValue; override;
    function DoGetIsTypeCast: Boolean; override;
    function IsValidAfterPart(APrevPart: TFpPascalExpressionPart): Boolean; override;
    function HandleNextPartInBracket(APart: TFpPascalExpressionPart): TFpPascalExpressionPart; override;
    function MaybeHandlePrevPart(APrevPart: TFpPascalExpressionPart;
      var AResult: TFpPascalExpressionPart): Boolean; override;
    function HandleSeparator(ASeparatorType: TSeparatorType): Boolean; override;
  end;


  { TFpPascalExpressionPartSquareBracket }

  TFpPascalExpressionPartSquareBracket = class(TFpPascalExpressionPartBracket)
  end;

  { TFpPascalExpressionPartBracketSet }

  TFpPascalExpressionPartBracketSet = class(TFpPascalExpressionPartSquareBracket)
  // a in [x, y, z]
  protected
    function HandleNextPartInBracket(APart: TFpPascalExpressionPart): TFpPascalExpressionPart; override;
    function HandleSeparator(ASeparatorType: TSeparatorType): Boolean; override;
  end;

  { TFpPascalExpressionPartBracketIndex }

  TFpPascalExpressionPartBracketIndex = class(TFpPascalExpressionPartSquareBracket)
  // array[1]
  protected
    procedure Init; override;
    function DoGetResultValue: TFpDbgValue; override;
    function IsValidAfterPart(APrevPart: TFpPascalExpressionPart): Boolean; override;
    function HandleNextPartInBracket(APart: TFpPascalExpressionPart): TFpPascalExpressionPart; override;
    function MaybeHandlePrevPart(APrevPart: TFpPascalExpressionPart;
      var AResult: TFpPascalExpressionPart): Boolean; override;
    procedure DoHandleEndOfExpression; override;
    function HandleSeparator(ASeparatorType: TSeparatorType): Boolean; override;
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
    function HandleSeparator(ASeparatorType: TSeparatorType): Boolean; override;
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
    function DoGetResultValue: TFpDbgValue; override;
  end;

  { TFpPascalExpressionPartOperatorMakeRef }

  TFpPascalExpressionPartOperatorMakeRef = class(TFpPascalExpressionPartUnaryOperator)  // ^TTYpe
  protected
    procedure Init; override;
    function IsValidNextPart(APart: TFpPascalExpressionPart): Boolean; override;
    function DoGetResultValue: TFpDbgValue; override;
    function DoGetIsTypeCast: Boolean; override;
  end;

  { TFpPascalExpressionPartOperatorDeRef }

  TFpPascalExpressionPartOperatorDeRef = class(TFpPascalExpressionPartUnaryOperator)  // ptrval^
  protected
    procedure Init; override;
    function DoGetResultValue: TFpDbgValue; override;
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
    function DoGetResultValue: TFpDbgValue; override;
  end;

  { TFpPascalExpressionPartOperatorPlusMinus }

  TFpPascalExpressionPartOperatorPlusMinus = class(TFpPascalExpressionPartBinaryOperator)  // + -
  // Binary + -
  protected
    procedure Init; override;
    function DoGetResultValue: TFpDbgValue; override;
  end;

  { TFpPascalExpressionPartOperatorMulDiv }

  TFpPascalExpressionPartOperatorMulDiv = class(TFpPascalExpressionPartBinaryOperator)    // * /
  protected
    procedure Init; override;
    function DoGetResultValue: TFpDbgValue; override;
  end;

  { TFpPascalExpressionPartOperatorCompare }

  TFpPascalExpressionPartOperatorCompare = class(TFpPascalExpressionPartBinaryOperator)    // = < > <> ><
  protected
    procedure Init; override;
    function DoGetResultValue: TFpDbgValue; override;
  end;

  { TFpPascalExpressionPartOperatorMemberOf }

  TFpPascalExpressionPartOperatorMemberOf = class(TFpPascalExpressionPartBinaryOperator)    // struct.member
  protected
    procedure Init; override;
    function IsValidNextPart(APart: TFpPascalExpressionPart): Boolean; override;
    function DoGetResultValue: TFpDbgValue; override;
  end;

implementation

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
  PRECEDENCE_MUL_DIV    = 12;        // a * b
  PRECEDENCE_PLUS_MINUS = 13;        // a + b
  PRECEDENCE_COMPARE    = 20;        // a <> b // a=b

type

  {%region  DebugSymbol }

  { TPasParserSymbolPointer
    used by TFpPasParserValueMakeReftype.GetDbgSymbol
  }

  TPasParserSymbolPointer = class(TFpDbgSymbol)
  private
    FPointerLevels: Integer;
    FPointedTo: TFpDbgSymbol;
    FContext: TFpDbgInfoContext;
  protected
    // NameNeeded //  "^TPointedTo"
    procedure TypeInfoNeeded; override;
  public
    constructor Create(const APointedTo: TFpDbgSymbol; AContext: TFpDbgInfoContext; APointerLevels: Integer);
    constructor Create(const APointedTo: TFpDbgSymbol; AContext: TFpDbgInfoContext);
    destructor Destroy; override;
    function TypeCastValue(AValue: TFpDbgValue): TFpDbgValue; override;
  end;

  { TPasParserSymbolArrayDeIndex }

  TPasParserSymbolArrayDeIndex = class(TDbgSymbolForwarder) // 1 index level off
  private
    FArray: TFpDbgSymbol;
  protected
    //procedure ForwardToSymbolNeeded; override;
    function GetMemberCount: Integer; override;
    function GetMember(AIndex: Int64): TFpDbgSymbol; override;
  public
    constructor Create(const AnArray: TFpDbgSymbol);
    destructor Destroy; override;
  end;

  {%endregion  DebugSymbol }

  {%region  DebugSymbolValue }

  { TFpPasParserValue }

  TFpPasParserValue = class(TFpDbgValue)
  private
    FContext: TFpDbgInfoContext;
  protected
    function DebugText(AIndent: String): String; virtual;
  public
    constructor Create(AContext: TFpDbgInfoContext);
    property Context: TFpDbgInfoContext read FContext;
  end;

  { TFpPasParserValueCastToPointer
    used by TPasParserSymbolPointer.TypeCastValue (which is used by TFpPasParserValueMakeReftype.GetDbgSymbol)
  }

  TFpPasParserValueCastToPointer = class(TFpPasParserValue)
  private
    FValue: TFpDbgValue;
    FTypeSymbol: TFpDbgSymbol;
    FLastMember: TFpDbgValue;
  protected
    function DebugText(AIndent: String): String; override;
  protected
    function GetKind: TDbgSymbolKind; override;
    function GetFieldFlags: TFpDbgValueFieldFlags; override;
    function GetTypeInfo: TFpDbgSymbol; override;
    function GetAsCardinal: QWord; override;
    function GetDataAddress: TFpDbgMemLocation; override;
    function GetMember(AIndex: Int64): TFpDbgValue; override;
  public
    constructor Create(AValue: TFpDbgValue; ATypeInfo: TFpDbgSymbol; AContext: TFpDbgInfoContext);
    destructor Destroy; override;
  end;

  { TFpPasParserValueMakeReftype }

  TFpPasParserValueMakeReftype = class(TFpPasParserValue)
  private
    FSourceTypeSymbol, FTypeSymbol: TFpDbgSymbol;
    FRefLevel: Integer;
  protected
    function DebugText(AIndent: String): String; override;
  protected
    function GetDbgSymbol: TFpDbgSymbol; override; // returns a TPasParserSymbolPointer
  public
    constructor Create(ATypeInfo: TFpDbgSymbol; AContext: TFpDbgInfoContext);
    destructor Destroy; override;
    procedure IncRefLevel;
    function GetTypeCastedValue(ADataVal: TFpDbgValue): TFpDbgValue; override;
  end;

  { TFpPasParserValueDerefPointer
    Used as address source in typecast
  }

  TFpPasParserValueDerefPointer = class(TFpPasParserValue)
  private
    FValue: TFpDbgValue;
    FAddressOffset: Int64; // Add to address
    FCardinal: QWord; // todo: TFpDbgMemLocation ?
    FCardinalRead: Boolean;
  protected
    function DebugText(AIndent: String): String; override;
  protected
    function GetFieldFlags: TFpDbgValueFieldFlags; override;
    function GetAddress: TFpDbgMemLocation; override;
    function GetSize: Integer; override;
    function GetAsCardinal: QWord; override; // reads men
    function GetTypeInfo: TFpDbgSymbol; override; // TODO: Cardinal? Why? // TODO: does not handle AOffset
  public
    constructor Create(AValue: TFpDbgValue; AContext: TFpDbgInfoContext);
    constructor Create(AValue: TFpDbgValue; AContext: TFpDbgInfoContext; AOffset: Int64);
    destructor Destroy; override;
  end;

  { TFpPasParserValueAddressOf }

  TFpPasParserValueAddressOf = class(TFpPasParserValue)
  private
    FValue: TFpDbgValue;
    FTypeInfo: TFpDbgSymbol;
    FLastMember: TFpDbgValue;
    function GetPointedToValue: TFpDbgValue;
  protected
    function DebugText(AIndent: String): String; override;
  protected
    function GetKind: TDbgSymbolKind; override;
    function GetFieldFlags: TFpDbgValueFieldFlags; override;
    function GetAsInteger: Int64; override;
    function GetAsCardinal: QWord; override;
    function GetTypeInfo: TFpDbgSymbol; override;
    function GetDataAddress: TFpDbgMemLocation; override;
    function GetMember(AIndex: Int64): TFpDbgValue; override;
  public
    constructor Create(AValue: TFpDbgValue; AContext: TFpDbgInfoContext);
    destructor Destroy; override;
    property PointedToValue: TFpDbgValue read GetPointedToValue;
  end;

  {%endregion  DebugSymbolValue }

function DbgsResultValue(AVal: TFpDbgValue; AIndent: String): String;
begin
  if (AVal <> nil) and (AVal is TFpPasParserValue) then
    Result := LineEnding + TFpPasParserValue(AVal).DebugText(AIndent)
  else
  if AVal <> nil then
    Result := DbgSName(AVal) + '  DbsSym='+DbgSName(AVal.DbgSymbol)+' Type='+DbgSName(AVal.TypeInfo)
  else
    Result := DbgSName(AVal);
end;

function DbgsSymbol(AVal: TFpDbgSymbol; {%H-}AIndent: String): String;
begin
  Result := DbgSName(AVal);
end;

function TFpPasParserValue.DebugText(AIndent: String): String;
begin
  Result := AIndent + DbgSName(Self)  + '  DbsSym='+DbgSName(DbgSymbol)+' Type='+DbgSName(TypeInfo) + LineEnding;
end;

constructor TFpPasParserValue.Create(AContext: TFpDbgInfoContext);
begin
  FContext := AContext;
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

function TFpPasParserValueCastToPointer.GetFieldFlags: TFpDbgValueFieldFlags;
begin
  if (FValue.FieldFlags * [svfAddress, svfCardinal] <> [])
  then
    Result := [svfOrdinal, svfCardinal, svfSizeOfPointer, svfDataAddress]
  else
    Result := [];
end;

function TFpPasParserValueCastToPointer.GetTypeInfo: TFpDbgSymbol;
begin
  Result := FTypeSymbol;
end;

function TFpPasParserValueCastToPointer.GetAsCardinal: QWord;
var
  f: TFpDbgValueFieldFlags;
begin
  Result := 0;
  f := FValue.FieldFlags;
  if svfCardinal in f then
    Result := FValue.AsCardinal
  else
  if svfAddress in f then begin
    if not FContext.MemManager.ReadUnsignedInt(FValue.Address, FContext.SizeOfAddress, Result) then
      Result := 0;
  end
  else
    Result := 0;
end;

function TFpPasParserValueCastToPointer.GetDataAddress: TFpDbgMemLocation;
begin
  Result := TargetLoc(TDbgPtr(AsCardinal));
end;

function TFpPasParserValueCastToPointer.GetMember(AIndex: Int64): TFpDbgValue;
var
  ti: TFpDbgSymbol;
  addr: TFpDbgMemLocation;
  Tmp: TFpDbgValueConstAddress;
begin
  Result := nil;

  ti := FTypeSymbol.TypeInfo;
  addr := DataAddress;
  if not IsTargetAddr(addr) then begin
    //LastError := CreateError(fpErrAnyError, ['Internal dereference error']);
    exit;
  end;
  {$PUSH}{$R-}{$Q-} // TODO: check overflow
  if ti <> nil then
    AIndex := AIndex * ti.Size;
  addr.Address := addr.Address + AIndex;
  {$POP}

  Tmp := TFpDbgValueConstAddress.Create(addr);
  if ti <> nil then begin
    Result := ti.TypeCastValue(Tmp);
    Tmp.ReleaseReference;
  end
  else
    Result := Tmp;
  FLastMember := Result;
end;

constructor TFpPasParserValueCastToPointer.Create(AValue: TFpDbgValue;
  ATypeInfo: TFpDbgSymbol; AContext: TFpDbgInfoContext);
begin
  inherited Create(AContext);
  FValue := AValue;
  FValue.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FValue, 'TPasParserSymbolValueCastToPointer'){$ENDIF};
  FTypeSymbol := ATypeInfo;
  FTypeSymbol.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FTypeSymbol, 'TPasParserSymbolValueCastToPointer'){$ENDIF};
  Assert((FTypeSymbol=nil) or (FTypeSymbol.Kind = skPointer), 'TPasParserSymbolValueCastToPointer.Create');
end;

destructor TFpPasParserValueCastToPointer.Destroy;
begin
  FLastMember.ReleaseReference;
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

function TFpPasParserValueMakeReftype.GetDbgSymbol: TFpDbgSymbol;
begin
  if FTypeSymbol = nil then begin
    FTypeSymbol := TPasParserSymbolPointer.Create(FSourceTypeSymbol, FContext, FRefLevel);
    {$IFDEF WITH_REFCOUNT_DEBUG}FTypeSymbol.DbgRenameReference(@FSourceTypeSymbol, 'TPasParserSymbolValueMakeReftype'){$ENDIF};
  end;
  Result := FTypeSymbol;
end;

constructor TFpPasParserValueMakeReftype.Create(ATypeInfo: TFpDbgSymbol;
  AContext: TFpDbgInfoContext);
begin
  inherited Create(AContext);
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

function TFpPasParserValueMakeReftype.GetTypeCastedValue(ADataVal: TFpDbgValue): TFpDbgValue;
begin
  Result := DbgSymbol.TypeCastValue(ADataVal);
end;


{ TPasParserDerefPointerSymbolValue }

function TFpPasParserValueDerefPointer.DebugText(AIndent: String): String;
begin
  Result := inherited DebugText(AIndent)
          + AIndent + '-Value= ' + DbgsResultValue(FValue, AIndent + '  ') + LineEnding;
end;

function TFpPasParserValueDerefPointer.GetFieldFlags: TFpDbgValueFieldFlags;
var
  t: TFpDbgSymbol;
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
  Result := FValue.DataAddress;
  if FAddressOffset <> 0 then begin
    assert(IsTargetAddr(Result ), 'TFpPasParserValueDerefPointer.GetAddress: TargetLoc(Result)');
    if IsTargetAddr(Result) then
      Result.Address := Result.Address + FAddressOffset
    else
      Result := InvalidLoc;
  end;
end;

function TFpPasParserValueDerefPointer.GetSize: Integer;
var
  t: TFpDbgSymbol;
begin
  t := FValue.TypeInfo;
  if t <> nil then t := t.TypeInfo;
  if t <> nil then
    Result := t.Size
  else
    Result := inherited GetSize;
end;

function TFpPasParserValueDerefPointer.GetAsCardinal: QWord;
var
  m: TFpDbgMemManager;
  Addr: TFpDbgMemLocation;
  Ctx: TFpDbgInfoContext;
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
  FCardinal := LocToAddrOrNil(m.ReadAddress(Addr, Ctx.SizeOfAddress));

  Result := FCardinal;
end;

function TFpPasParserValueDerefPointer.GetTypeInfo: TFpDbgSymbol;
var
  t: TFpDbgSymbol;
begin
  t := FValue.TypeInfo;
  if t <> nil then t := t.TypeInfo;
  if t <> nil then
    Result := t
  else
    Result := inherited GetTypeInfo;
end;

constructor TFpPasParserValueDerefPointer.Create(AValue: TFpDbgValue;
  AContext: TFpDbgInfoContext);
begin
  Create(AValue, AContext, 0);
end;

constructor TFpPasParserValueDerefPointer.Create(AValue: TFpDbgValue;
  AContext: TFpDbgInfoContext; AOffset: Int64);
begin
  inherited Create(AContext);
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

function TFpPasParserValueAddressOf.GetPointedToValue: TFpDbgValue;
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

function TFpPasParserValueAddressOf.GetFieldFlags: TFpDbgValueFieldFlags;
begin
    Result := [svfOrdinal, svfCardinal, svfSizeOfPointer, svfDataAddress];
end;

function TFpPasParserValueAddressOf.GetAsInteger: Int64;
begin
  Result := Int64(LocToAddrOrNil(FValue.Address));
end;

function TFpPasParserValueAddressOf.GetAsCardinal: QWord;
begin
  Result := QWord(LocToAddrOrNil(FValue.Address));
end;

function TFpPasParserValueAddressOf.GetTypeInfo: TFpDbgSymbol;
begin
  Result := FTypeInfo;
  if Result <> nil then
    exit;
  if FValue.TypeInfo = nil then
    exit;

  FTypeInfo := TPasParserSymbolPointer.Create(FValue.TypeInfo, FContext);
  {$IFDEF WITH_REFCOUNT_DEBUG}FTypeInfo.DbgRenameReference(@FTypeInfo, 'TPasParserAddressOfSymbolValue');{$ENDIF}
  Result := FTypeInfo;
end;

function TFpPasParserValueAddressOf.GetDataAddress: TFpDbgMemLocation;
begin
  Result := FValue.Address;
end;

function TFpPasParserValueAddressOf.GetMember(AIndex: Int64): TFpDbgValue;
var
  ti: TFpDbgSymbol;
  addr: TFpDbgMemLocation;
  Tmp: TFpDbgValueConstAddress;
begin
  if (AIndex = 0) or (FValue = nil) then begin
    Result := FValue;
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
  if ti <> nil then
    AIndex := AIndex * ti.Size;
  addr.Address := addr.Address + AIndex;
  {$POP}

  Tmp := TFpDbgValueConstAddress.Create(addr);
  if ti <> nil then begin
    Result := ti.TypeCastValue(Tmp);
    Tmp.ReleaseReference;
  end
  else
    Result := Tmp;
  FLastMember := Result;
end;

constructor TFpPasParserValueAddressOf.Create(AValue: TFpDbgValue;
  AContext: TFpDbgInfoContext);
begin
  inherited Create(AContext);
  FValue := AValue;
  FValue.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FValue, 'TPasParserAddressOfSymbolValue'){$ENDIF};
end;

destructor TFpPasParserValueAddressOf.Destroy;
begin
  inherited Destroy;
  FLastMember.ReleaseReference;
  FValue.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FValue, 'TPasParserAddressOfSymbolValue'){$ENDIF};
  FTypeInfo.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FTypeInfo, 'TPasParserAddressOfSymbolValue'){$ENDIF};
end;

{ TPasParserSymbolArrayDeIndex }

function TPasParserSymbolArrayDeIndex.GetMemberCount: Integer;
begin
  Result := (inherited GetMemberCount) - 1;
end;

function TPasParserSymbolArrayDeIndex.GetMember(AIndex: Int64): TFpDbgSymbol;
begin
  Result := inherited GetMember(AIndex + 1);
end;

constructor TPasParserSymbolArrayDeIndex.Create(const AnArray: TFpDbgSymbol);
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
  assert(FPointerLevels > 1, 'TPasParserSymbolPointer.TypeInfoNeeded: FPointerLevels > 1');
  t := TPasParserSymbolPointer.Create(FPointedTo, FContext, FPointerLevels-1);
  SetTypeInfo(t);
  t.ReleaseReference;
end;

constructor TPasParserSymbolPointer.Create(const APointedTo: TFpDbgSymbol;
  AContext: TFpDbgInfoContext; APointerLevels: Integer);
begin
  inherited Create('');
  FContext := AContext;
  FPointerLevels := APointerLevels;
  FPointedTo := APointedTo;
  FPointedTo.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(FPointedTo, 'TPasParserSymbolPointer'){$ENDIF};
  if APointerLevels = 1 then
    SetTypeInfo(APointedTo);
  SetKind(skPointer);
  SetSymbolType(stType);
end;

constructor TPasParserSymbolPointer.Create(const APointedTo: TFpDbgSymbol;
  AContext: TFpDbgInfoContext);
begin
  Create(APointedTo, AContext, 1);
end;

destructor TPasParserSymbolPointer.Destroy;
begin
  FPointedTo.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(FPointedTo, 'TPasParserSymbolPointer'){$ENDIF};
  inherited Destroy;
end;

function TPasParserSymbolPointer.TypeCastValue(AValue: TFpDbgValue): TFpDbgValue;
begin
  Result := TFpPasParserValueCastToPointer.Create(AValue, Self, FContext);
end;


{ TFpPascalExpressionPartBracketIndex }

procedure TFpPascalExpressionPartBracketIndex.Init;
begin
  FPrecedence := PRECEDENCE_ARRAY_IDX;
  inherited Init;
end;

function TFpPascalExpressionPartBracketIndex.DoGetResultValue: TFpDbgValue;
var
  TmpVal, TmpVal2, TmpIndex: TFpDbgValue;
  i: Integer;
  Offs: Int64;
  ti: TFpDbgSymbol;
  IsPChar: Boolean;
  v: String;
begin
  Result := nil;
  assert(Count >= 2, 'TFpPascalExpressionPartBracketIndex.DoGetResultValue: Count >= 2');
  if Count < 2 then exit;

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
            SetError('Can not calculate Index');
            TmpVal.ReleaseReference;
            exit;
          end;
          if TmpVal2 <> nil then TmpVal2.AddReference;
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
            SetError('Can not calculate Index');
            TmpVal.ReleaseReference;
            exit;
          end;

          ti := TmpVal.TypeInfo;
          if (ti <> nil) then ti := ti.TypeInfo;
          IsPChar := (ti <> nil) and (ti.Kind in [skChar]) and (Offs > 0);
          if IsPChar then FExpression.FHasPCharIndexAccess := True;
          if IsPChar and FExpression.FixPCharIndexAccess then begin
            // fix for string in dwarf 2
            if Offs > 0 then
              dec(Offs);
          end;

          TmpVal2 := TmpVal.Member[Offs];
          if IsError(TmpVal.LastError) then
            SetError('Error dereferencing'); // TODO: set correct error
          if TmpVal2 <> nil then TmpVal2.AddReference;
        end;
      skString, skAnsiString: begin
          //TODO: move to FpDwarfValue.member ??
          if (svfInteger in TmpIndex.FieldFlags) then
            Offs := TmpIndex.AsInteger
          else
          if (svfOrdinal in TmpIndex.FieldFlags) and (TmpIndex.AsCardinal <= high(Int64))
          then
            Offs := Int64(TmpIndex.AsCardinal)
          else
          begin
            SetError('Can not calculate Index');
            TmpVal.ReleaseReference;
            exit;
          end;

          v := TmpVal.AsString;
          if (Offs < 1) or (Offs > Length(v)) then begin
            SetError('Index out of range');
            TmpVal.ReleaseReference;
            exit;
          end;

          TmpVal2 := TFpDbgValueConstChar.Create(v[Offs]);
        end
      else
        begin
          SetError(fpErrTypeHasNoIndex, [GetText]);
          TmpVal.ReleaseReference;
          exit;
        end;
    end;

    if TmpVal2 = nil then begin
      SetError('Internal Error, attempting to read array element');
      TmpVal.ReleaseReference;
      exit;
    end;
    TmpVal.ReleaseReference;
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
    SetError(fpErrPasParserMissingIndexExpression, [GetText]);
end;

function TFpPascalExpressionPartBracketIndex.HandleSeparator(ASeparatorType: TSeparatorType): Boolean;
begin
  if (not (ASeparatorType = ppstComma)) or IsClosed then begin
    Result := inherited HandleSeparator(ASeparatorType);
    exit;
  end;

  Result := (Count > FAfterComma) and (Count > 1); // First element is name of array (in front of "[")
  if Result then
    SetAfterCommaFlag;
end;

{ TFpPascalExpressionPartBracketSet }

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

function TFpPascalExpressionPartBracketSet.HandleSeparator(ASeparatorType: TSeparatorType): Boolean;
begin
  if (not (ASeparatorType = ppstComma)) or IsClosed then begin
    Result := inherited HandleSeparator(ASeparatorType);
    exit;
  end;

  Result := (Count > FAfterComma) and (Count > 0);
  if Result then
    SetAfterCommaFlag;
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

function TFpPascalExpressionPartBracketArgumentList.DoGetResultValue: TFpDbgValue;
var
  tmp, tmp2: TFpDbgValue;
begin
  Result := nil;

  if (Count = 0) then begin
    SetError(fpErrPasParserInvalidExpression, []);
    exit;
  end;

  tmp := Items[0].ResultValue;
  if (tmp = nil) or (not Expression.Valid) then
    exit;

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

  // Must be function call
  SetError('No support for calling functions');
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

function TFpPascalExpressionPartBracketArgumentList.HandleSeparator(ASeparatorType: TSeparatorType): Boolean;
begin
  if (not (ASeparatorType = ppstComma)) or IsClosed then begin
    Result := inherited HandleSeparator(ASeparatorType);
    exit;
  end;

  Result := (Count > FAfterComma) and (Count > 1); // First element is name of function (in front of "(")
  if Result then
    SetAfterCommaFlag;
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

function TFpPascalExpressionPartBracketSubExpression.DoGetResultValue: TFpDbgValue;
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

function TFpPascalExpressionPartIdentifier.DoGetResultValue: TFpDbgValue;
var
  s: String;
  tmp: TFpDbgValueConstAddress;
begin
  s := GetText;
  Result := FExpression.GetDbgSymbolForIdentifier(s);
  if Result = nil then begin
    s := LowerCase(s);
    if s = 'nil' then begin
      tmp := TFpDbgValueConstAddress.Create(NilLoc);
      Result := TFpPasParserValueAddressOf.Create(tmp, Expression.Context);
      tmp.ReleaseReference;
      {$IFDEF WITH_REFCOUNT_DEBUG}Result.DbgRenameReference(nil, 'DoGetResultValue');{$ENDIF}
    end
    else
    if s = 'true' then begin
      Result := TFpDbgValueConstBool.Create(True);
      {$IFDEF WITH_REFCOUNT_DEBUG}Result.DbgRenameReference(nil, 'DoGetResultValue');{$ENDIF}
    end
    else
    if s = 'false' then begin
      Result := TFpDbgValueConstBool.Create(False);
      {$IFDEF WITH_REFCOUNT_DEBUG}Result.DbgRenameReference(nil, 'DoGetResultValue');{$ENDIF}
    end
    else begin
      SetError(fpErrSymbolNotFound, [GetText]);
      exit;
    end;
  end

  else
    Result.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(nil, 'DoGetResultValue'){$ENDIF};
end;

function GetFirstToken(AText: PChar): String;
begin
  Result := AText[0];
  if AText^ in ['a'..'z', 'A'..'Z', '_', '0'..'9'] then begin
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

{ TFpPascalExpressionPartConstantNumber }

function TFpPascalExpressionPartConstantNumber.DoGetResultValue: TFpDbgValue;
var
  i: QWord;
  e: word;
begin
  Val(GetText, i, e);
  if e <> 0 then begin
    Result := nil;
    SetError(fpErrInvalidNumber, [GetText]);
    exit;
  end;

  if FStartChar^ in ['0'..'9'] then
    Result := TFpDbgValueConstNumber.Create(i, False)
  else
    Result := TFpDbgValueConstNumber.Create(Int64(i), True); // hex,oct,bin values default to signed
  {$IFDEF WITH_REFCOUNT_DEBUG}Result.DbgRenameReference(nil, 'DoGetResultValue'){$ENDIF};
end;

{ TFpPascalExpressionPartConstantNumberFloat }

function TFpPascalExpressionPartConstantNumberFloat.DoGetResultValue: TFpDbgValue;
var
  f: Extended;
  s: String;
begin
  s := GetText;
  if not TextToFloat(PChar(s), f) then begin
    Result := nil;
    SetError(fpErrInvalidNumber, [GetText]);
    exit;
  end;

  Result := TFpDbgValueConstFloat.Create(f);
  {$IFDEF WITH_REFCOUNT_DEBUG}Result.DbgRenameReference(nil, 'DoGetResultValue'){$ENDIF};
end;

{ TFpPascalExpression }

procedure TFpPascalExpression.Parse;
var
  CurPtr, EndPtr, TokenEndPtr: PChar;
  CurPart, NewPart: TFpPascalExpressionPart;

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

  procedure AddIdentifier;
  begin
    while TokenEndPtr^ in ['a'..'z', 'A'..'Z', '_', '0'..'9'] do
      inc(TokenEndPtr);
    // TODO: Check functions not, and, in, as, is ...
    case TokenEndPtr - CurPtr of
      3: case CurPtr^ of
        'd', 'D':
          if (CurPtr[1] in ['i', 'I']) and (CurPtr[2] in ['v', 'V']) then
            NewPart := TFpPascalExpressionPartOperatorMulDiv.Create(Self, CurPtr, TokenEndPtr-1);
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
      //2: ; // ".."
      else SetError('Failed parsing ...');
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
    BracketPart := CurPart.SurroundingBracket;
    if BracketPart = nil then begin
      SetError('Closing bracket found without opening')
    end
    else
    if not (BracketPart is ABracketClass) then begin
      SetError('Mismatch bracket')
    end
    else begin
      TFpPascalExpressionPartBracket(BracketPart).CloseBracket;
      CurPart := BracketPart;
    end;
  end;

  procedure AddConstNumber;
  begin
    case CurPtr^ of
      '$': while TokenEndPtr^ in ['a'..'z', 'A'..'Z', '0'..'9'] do inc(TokenEndPtr);
      '&': while TokenEndPtr^ in ['0'..'7'] do inc(TokenEndPtr);
      '%': while TokenEndPtr^ in ['0'..'1'] do inc(TokenEndPtr);
      '0'..'9':
        if (CurPtr^ = '0') and ((CurPtr + 1)^ in ['x', 'X']) and
           ((CurPtr + 2)^ in ['a'..'z', 'A'..'Z', '0'..'9'])
        then begin
          inc(TokenEndPtr, 2);
          while TokenEndPtr^ in ['a'..'z', 'A'..'Z', '0'..'9'] do inc(TokenEndPtr);
        end
        else begin
          while TokenEndPtr^ in ['0'..'9'] do inc(TokenEndPtr);
          // identify "2.", but not "[2..3]"  // CurExpr.IsFloatAllowed
          if (TokenEndPtr^ = DecimalSeparator) and (TokenEndPtr[1] <> '.') then begin
            inc(TokenEndPtr);
            while TokenEndPtr^ in ['0'..'9'] do inc(TokenEndPtr);
            if TokenEndPtr^ in ['a'..'z', 'A'..'Z', '_'] then
              SetError(fpErrPasParserUnexpectedToken, [GetFirstToken(CurPtr), PosFromPChar(CurPtr)])
            else
              AddPart(TFpPascalExpressionPartConstantNumberFloat);
            exit;
          end;
        end;
    end;
    if TokenEndPtr^ in ['a'..'z', 'A'..'Z', '_'] then
      SetError(fpErrPasParserUnexpectedToken, [GetFirstToken(CurPtr), PosFromPChar(CurPtr)])
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
    if not CurPart.HandleSeparator(ppstComma) then
      SetError(fpErrPasParserUnexpectedToken, [GetFirstToken(CurPtr), PosFromPChar(CurPtr)]);
  end;

  procedure AddConstChar;
  begin
    SetError(Format('Unexpected char ''%0:s'' at pos %1:d', [CurPtr^, PosFromPChar(CurPtr)])); // error
  end;

begin
  if FTextExpression = '' then
    exit;
  CurPtr := @FTextExpression[1];
  EndPtr := CurPtr + length(FTextExpression);
  CurPart := nil;

  While (CurPtr < EndPtr) and FValid do begin
    if CurPtr^ in [' ', #9, #10, #13] then begin
      while (CurPtr^ in [' ', #9, #10, #13]) and (CurPtr < EndPtr) do
        Inc(CurPtr);
      continue;
    end;

    NewPart := nil;
    TokenEndPtr := CurPtr + 1;
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
          //SetError(fpErrPasParserUnexpectedToken, [GetFirstToken(CurPtr), PosFromPChar(CurPtr)])
          SetError(Format('Unexpected token ''%0:s'' at pos %1:d', [CurPtr^, PosFromPChar(CurPtr)])); // error
          break;
        end;
    end;
    if not FValid then
      break;

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
      SetError('No Expression');
  end
  else
  if CurPart <> nil then
    CurPart := CurPart.TopParent;

  FExpressionPart := CurPart;
end;

function TFpPascalExpression.GetResultValue: TFpDbgValue;
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
DebugLn(['Skipping error ', AMsg]);
    FValid := False;
    exit;
  end;
  SetError(fpErrAnyError, [AMsg]);
DebugLn(['PARSER ERROR ', AMsg]);
end;

procedure TFpPascalExpression.SetError(AnErrorCode: TFpErrorCode; AData: array of const);
begin
  FValid := False;
  FError := ErrorHandler.CreateError(AnErrorCode, AData);
  DebugLn(['Setting error ', ErrorHandler.ErrorAsString(FError)]);
end;

function TFpPascalExpression.PosFromPChar(APChar: PChar): Integer;
begin
  Result := APChar - @FTextExpression[1] + 1;
end;

function TFpPascalExpression.GetDbgSymbolForIdentifier(AnIdent: String): TFpDbgValue;
begin
  if FContext <> nil then
    Result := FContext.FindSymbol(AnIdent)
  else
    Result := nil;
end;

constructor TFpPascalExpression.Create(ATextExpression: String;
  AContext: TFpDbgInfoContext);
begin
  FContext := AContext;
  FTextExpression := ATextExpression;
  FError := NoError;
  FValid := True;
  Parse;
end;

destructor TFpPascalExpression.Destroy;
begin
  FreeAndNil(FExpressionPart);
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
  FExpressionPart.ResetEvaluation;
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

function TFpPascalExpressionPart.GetResultValue: TFpDbgValue;
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

procedure TFpPascalExpressionPart.SetError(AnErrorCode: TFpErrorCode; AData: array of const);
begin
  FExpression.SetError(AnErrorCode, AData);
end;

procedure TFpPascalExpressionPart.Init;
begin
  //
end;

function TFpPascalExpressionPart.DoGetIsTypeCast: Boolean;
begin
  Result := False;
end;

function TFpPascalExpressionPart.DoGetResultValue: TFpDbgValue;
begin
  Result := nil;
end;

procedure TFpPascalExpressionPart.ResetEvaluation;
begin
  FResultValue.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FResultValue, 'DoGetResultValue'){$ENDIF};
  FResultValue := nil;
  FResultValDone := False;
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

function TFpPascalExpressionPart.HandleSeparator(ASeparatorType: TSeparatorType): Boolean;
begin
  Result := (Parent <> nil) and Parent.HandleSeparator(ASeparatorType);
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
    SetError(fpErrPasParserMissingExprAfterComma, [GetText]);
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

function TFpPascalExpressionPartOperator.HandleSeparator(ASeparatorType: TSeparatorType): Boolean;
begin
  Result := HasAllOperands and (inherited HandleSeparator(ASeparatorType));
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

function TFpPascalExpressionPartOperatorAddressOf.DoGetResultValue: TFpDbgValue;
var
  tmp: TFpDbgValue;
begin
  Result := nil;
  if Count <> 1 then exit;

  tmp := Items[0].ResultValue;
  if (tmp = nil) or not IsTargetAddr(tmp.Address) then
    exit;

  Result := TFpPasParserValueAddressOf.Create(tmp, Expression.Context);
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

function TFpPascalExpressionPartOperatorMakeRef.DoGetResultValue: TFpDbgValue;
var
  tmp: TFpDbgValue;
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

  Result := TFpPasParserValueMakeReftype.Create(tmp.DbgSymbol, Expression.Context);
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

function TFpPascalExpressionPartOperatorDeRef.DoGetResultValue: TFpDbgValue;
var
  tmp: TFpDbgValue;
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
    if (svfDataAddress in tmp.FieldFlags) and (IsReadableLoc(tmp.DataAddress)) and // TODO, what if Not readable addr
       (tmp.TypeInfo <> nil) //and (tmp.TypeInfo.TypeInfo <> nil)
    then begin
      Result := tmp.Member[0];
      if Result <> nil then
        Result.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(nil, 'DoGetResultValue'){$ENDIF};

    end;
  end
  //if tmp.Kind = skArray then // dynarray
  else
  begin
    Result := nil;
    SetError(fpErrCannotDereferenceType, [GetText]);
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

function TFpPascalExpressionPartOperatorUnaryPlusMinus.DoGetResultValue: TFpDbgValue;
var
  tmp1: TFpDbgValue;
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
      skInteger: Result := tmp1;
      skCardinal: Result := tmp1;
    end;
  end
  else begin
    case tmp1.Kind of
      skPointer: ;
      skInteger: Result := TFpDbgValueConstNumber.Create(-tmp1.AsInteger, True);
      skCardinal: Result := TFpDbgValueConstNumber.Create(-tmp1.AsCardinal, True);
    end;
  end;
  {$POP}

 {$IFDEF WITH_REFCOUNT_DEBUG}if Result <> nil then Result.DbgRenameReference(nil, 'DoGetResultValue');{$ENDIF}
end;

{ TFpPascalExpressionPartOperatorPlusMinus }

procedure TFpPascalExpressionPartOperatorPlusMinus.Init;
begin
  FPrecedence := PRECEDENCE_PLUS_MINUS;
  inherited Init;
end;

function TFpPascalExpressionPartOperatorPlusMinus.DoGetResultValue: TFpDbgValue;
{$PUSH}{$R-}{$Q-}
  function AddSubValueToPointer(APointerVal, AOtherVal: TFpDbgValue; ADoSubtract: Boolean = False): TFpDbgValue;
  var
    Idx: Int64;
    TmpVal: TFpDbgValue;
  begin
    Result := nil;
    case AOtherVal.Kind of
    //  skPointer:  Result := nil;
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
    if IsError(APointerVal.LastError) then begin
      SetError('Error dereferencing'); // TODO: set correct error
      exit;
    end;
    Result := TFpPasParserValueAddressOf.Create(TmpVal, Expression.Context);
  end;
  function AddValueToInt(AIntVal, AOtherVal: TFpDbgValue): TFpDbgValue;
  begin
    Result := nil;
    case AOtherVal.Kind of
      skPointer:  Result := AddSubValueToPointer(AOtherVal, AIntVal);
      skInteger:  Result := TFpDbgValueConstNumber.Create(AIntVal.AsInteger + AOtherVal.AsInteger, True);
      skCardinal: Result := TFpDbgValueConstNumber.Create(AIntVal.AsInteger + AOtherVal.AsCardinal, True);
      skFloat:    Result := TFpDbgValueConstFloat.Create(AIntVal.AsInteger + AOtherVal.AsFloat);
      else SetError('Addition not supported');
    end;
  end;
  function AddValueToCardinal(ACardinalVal, AOtherVal: TFpDbgValue): TFpDbgValue;
  begin
    Result := nil;
    case AOtherVal.Kind of
      skPointer:  Result := AddSubValueToPointer(AOtherVal, ACardinalVal);
      skInteger:  Result := TFpDbgValueConstNumber.Create(ACardinalVal.AsCardinal + AOtherVal.AsInteger, True);
      skCardinal: Result := TFpDbgValueConstNumber.Create(ACardinalVal.AsCardinal + AOtherVal.AsCardinal, False);
      skFloat:    Result := TFpDbgValueConstFloat.Create(ACardinalVal.AsCardinal + AOtherVal.AsFloat);
      else SetError('Addition not supported');
    end;
  end;
  function AddValueToFloat(AFloatVal, AOtherVal: TFpDbgValue): TFpDbgValue;
  begin
    Result := nil;
    case AOtherVal.Kind of
      skInteger:  Result := TFpDbgValueConstFloat.Create(AFloatVal.AsFloat + AOtherVal.AsInteger);
      skCardinal: Result := TFpDbgValueConstFloat.Create(AFloatVal.AsFloat + AOtherVal.AsCardinal);
      skFloat:    Result := TFpDbgValueConstFloat.Create(AFloatVal.AsFloat + AOtherVal.AsFloat);
      else SetError('Addition not supported');
    end;
  end;

  function SubPointerFromValue(APointerVal, AOtherVal: TFpDbgValue): TFpDbgValue;
  begin
    Result := nil;       // Error
  end;
  function SubValueFromInt(AIntVal, AOtherVal: TFpDbgValue): TFpDbgValue;
  begin
    Result := nil;
    case AOtherVal.Kind of
      skPointer:  Result := SubPointerFromValue(AOtherVal, AIntVal);
      skInteger:  Result := TFpDbgValueConstNumber.Create(AIntVal.AsInteger - AOtherVal.AsInteger, True);
      skCardinal: Result := TFpDbgValueConstNumber.Create(AIntVal.AsInteger - AOtherVal.AsCardinal, True);
      skFloat:    Result := TFpDbgValueConstFloat.Create(AIntVal.AsInteger - AOtherVal.AsFloat);
      else SetError('Subtraction not supported');
    end;
  end;
  function SubValueFromCardinal(ACardinalVal, AOtherVal: TFpDbgValue): TFpDbgValue;
  begin
    Result := nil;
    case AOtherVal.Kind of
      skPointer:  Result := SubPointerFromValue(AOtherVal, ACardinalVal);
      skInteger:  Result := TFpDbgValueConstNumber.Create(ACardinalVal.AsCardinal - AOtherVal.AsInteger, True);
      skCardinal: Result := TFpDbgValueConstNumber.Create(ACardinalVal.AsCardinal - AOtherVal.AsCardinal, False);
      skFloat:    Result := TFpDbgValueConstFloat.Create(ACardinalVal.AsCardinal - AOtherVal.AsFloat);
      else SetError('Subtraction not supported');
    end;
  end;
  function SubValueFromFloat(AFloatVal, AOtherVal: TFpDbgValue): TFpDbgValue;
  begin
    Result := nil;
    case AOtherVal.Kind of
      skInteger:  Result := TFpDbgValueConstFloat.Create(AFloatVal.AsFloat - AOtherVal.AsInteger);
      skCardinal: Result := TFpDbgValueConstFloat.Create(AFloatVal.AsFloat - AOtherVal.AsCardinal);
      skFloat:    Result := TFpDbgValueConstFloat.Create(AFloatVal.AsFloat - AOtherVal.AsFloat);
      else SetError('Subtraction not supported');
    end;
  end;
{$POP}
var
  tmp1, tmp2: TFpDbgValue;
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
      skPointer:  Result := AddSubValueToPointer(tmp1, tmp2);
      skInteger:  Result := AddValueToInt(tmp1, tmp2);
      skCardinal: Result := AddValueToCardinal(tmp1, tmp2);
      skFloat:    Result := AddValueToFloat(tmp1, tmp2);
    end;
  end
  else begin
    case tmp1.Kind of
      skPointer:  Result := AddSubValueToPointer(tmp1, tmp2, True);
      skInteger:  Result := SubValueFromInt(tmp1, tmp2);
      skCardinal: Result := SubValueFromCardinal(tmp1, tmp2);
      skFloat:    Result := SubValueFromFloat(tmp1, tmp2);
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

function TFpPascalExpressionPartOperatorMulDiv.DoGetResultValue: TFpDbgValue;
{$PUSH}{$R-}{$Q-}
  function MultiplyIntWithValue(AIntVal, AOtherVal: TFpDbgValue): TFpDbgValue;
  begin
    Result := nil;
    case AOtherVal.Kind of
      skInteger:  Result := TFpDbgValueConstNumber.Create(AIntVal.AsInteger * AOtherVal.AsInteger, True);
      skCardinal: Result := TFpDbgValueConstNumber.Create(AIntVal.AsInteger * AOtherVal.AsCardinal, True);
      skFloat:    Result := TFpDbgValueConstFloat.Create(AIntVal.AsInteger * AOtherVal.AsFloat);
      else SetError('Multiply not supported');
    end;
  end;
  function MultiplyCardinalWithValue(ACardinalVal, AOtherVal: TFpDbgValue): TFpDbgValue;
  begin
    Result := nil;
    case AOtherVal.Kind of
      skInteger:  Result := TFpDbgValueConstNumber.Create(ACardinalVal.AsCardinal * AOtherVal.AsInteger, True);
      skCardinal: Result := TFpDbgValueConstNumber.Create(ACardinalVal.AsCardinal * AOtherVal.AsCardinal, False);
      skFloat:    Result := TFpDbgValueConstFloat.Create(ACardinalVal.AsCardinal * AOtherVal.AsFloat);
      else SetError('Multiply not supported');
    end;
  end;
  function MultiplyFloatWithValue(AFloatVal, AOtherVal: TFpDbgValue): TFpDbgValue;
  begin
    Result := nil;
    case AOtherVal.Kind of
      skInteger:  Result := TFpDbgValueConstFloat.Create(AFloatVal.AsFloat * AOtherVal.AsInteger);
      skCardinal: Result := TFpDbgValueConstFloat.Create(AFloatVal.AsFloat * AOtherVal.AsCardinal);
      skFloat:    Result := TFpDbgValueConstFloat.Create(AFloatVal.AsFloat * AOtherVal.AsFloat);
      else SetError('Multiply not supported');
    end;
  end;

  function FloatDivIntByValue(AIntVal, AOtherVal: TFpDbgValue): TFpDbgValue;
  begin
    Result := nil;
    case AOtherVal.Kind of
      skInteger:  Result := TFpDbgValueConstFloat.Create(AIntVal.AsInteger / AOtherVal.AsInteger);
      skCardinal: Result := TFpDbgValueConstFloat.Create(AIntVal.AsInteger / AOtherVal.AsCardinal);
      skFloat:    Result := TFpDbgValueConstFloat.Create(AIntVal.AsInteger / AOtherVal.AsFloat);
      else SetError('/ not supported');
    end;
  end;
  function FloatDivCardinalByValue(ACardinalVal, AOtherVal: TFpDbgValue): TFpDbgValue;
  begin
    Result := nil;
    case AOtherVal.Kind of
      skInteger:  Result := TFpDbgValueConstFloat.Create(ACardinalVal.AsCardinal / AOtherVal.AsInteger);
      skCardinal: Result := TFpDbgValueConstFloat.Create(ACardinalVal.AsCardinal / AOtherVal.AsCardinal);
      skFloat:    Result := TFpDbgValueConstFloat.Create(ACardinalVal.AsCardinal / AOtherVal.AsFloat);
      else SetError('/ not supported');
    end;
  end;
  function FloatDivFloatByValue(AFloatVal, AOtherVal: TFpDbgValue): TFpDbgValue;
  begin
    Result := nil;
    case AOtherVal.Kind of
      skInteger:  Result := TFpDbgValueConstFloat.Create(AFloatVal.AsFloat / AOtherVal.AsInteger);
      skCardinal: Result := TFpDbgValueConstFloat.Create(AFloatVal.AsFloat / AOtherVal.AsCardinal);
      skFloat:    Result := TFpDbgValueConstFloat.Create(AFloatVal.AsFloat / AOtherVal.AsFloat);
      else SetError('/ not supported');
    end;
  end;

  function NumDivIntByValue(AIntVal, AOtherVal: TFpDbgValue): TFpDbgValue;
  begin
    Result := nil;
    case AOtherVal.Kind of
      skInteger:  Result := TFpDbgValueConstNumber.Create(AIntVal.AsInteger div AOtherVal.AsInteger, True);
      skCardinal: Result := TFpDbgValueConstNumber.Create(AIntVal.AsInteger div AOtherVal.AsCardinal, True);
      else SetError('Div not supported');
    end;
  end;
  function NumDivCardinalByValue(ACardinalVal, AOtherVal: TFpDbgValue): TFpDbgValue;
  begin
    Result := nil;
    case AOtherVal.Kind of
      skInteger:  Result := TFpDbgValueConstNumber.Create(ACardinalVal.AsCardinal div AOtherVal.AsInteger, True);
      skCardinal: Result := TFpDbgValueConstNumber.Create(ACardinalVal.AsCardinal div AOtherVal.AsCardinal, False);
      else SetError('Div not supported');
    end;
  end;
{$POP}
var
  tmp1, tmp2: TFpDbgValue;
begin
  Result := nil;
  if Count <> 2 then exit;
  assert((GetText = '*') or (GetText = '/') or (LowerCase(GetText) = 'div') , 'TFpPascalExpressionPartOperatorUnaryPlusMinus.DoGetResultValue: (GetText = +) or (GetText = -)');

  tmp1 := Items[0].ResultValue;
  tmp2 := Items[1].ResultValue;
  if (tmp1 = nil) or (tmp2 = nil) then exit;

  if GetText = '*' then begin
    case tmp1.Kind of
      skInteger:  Result := MultiplyIntWithValue(tmp1, tmp2);
      skCardinal: Result := MultiplyCardinalWithValue(tmp1, tmp2);
      skFloat:    Result := MultiplyFloatWithValue(tmp1, tmp2);
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
  if LowerCase(GetText) = 'div' then begin
    case tmp1.Kind of
      skInteger:  Result := NumDivIntByValue(tmp1, tmp2);
      skCardinal: Result := NumDivCardinalByValue(tmp1, tmp2);
    end;
  end;

 {$IFDEF WITH_REFCOUNT_DEBUG}if Result <> nil then
   Result.DbgRenameReference(nil, 'DoGetResultValue');{$ENDIF}
end;

{ TFpPascalExpressionPartOperatorCompare }

procedure TFpPascalExpressionPartOperatorCompare.Init;
begin
  FPrecedence := PRECEDENCE_COMPARE;
  inherited Init;
end;

function TFpPascalExpressionPartOperatorCompare.DoGetResultValue: TFpDbgValue;
{$PUSH}{$R-}{$Q-}
  function IntEqualToValue(AIntVal, AOtherVal: TFpDbgValue; ARevert: Boolean = False): TFpDbgValue;
  begin
    Result := nil;
    case AOtherVal.Kind of
      skInteger:  Result := TFpDbgValueConstBool.Create((AIntVal.AsInteger = AOtherVal.AsInteger) xor ARevert);
      skCardinal: Result := TFpDbgValueConstBool.Create((AIntVal.AsInteger = AOtherVal.AsCardinal) xor ARevert);
      skFloat:    Result := TFpDbgValueConstBool.Create((AIntVal.AsInteger = AOtherVal.AsFloat) xor ARevert);
      else SetError('= not supported');
    end;
  end;
  function CardinalEqualToValue(ACardinalVal, AOtherVal: TFpDbgValue; ARevert: Boolean = False): TFpDbgValue;
  begin
    Result := nil;
    case AOtherVal.Kind of
      skInteger:  Result := TFpDbgValueConstBool.Create((ACardinalVal.AsCardinal = AOtherVal.AsInteger) xor ARevert);
      skCardinal: Result := TFpDbgValueConstBool.Create((ACardinalVal.AsCardinal = AOtherVal.AsCardinal) xor ARevert);
      skFloat:    Result := TFpDbgValueConstBool.Create((ACardinalVal.AsCardinal = AOtherVal.AsFloat) xor ARevert);
      else SetError('= not supported');
    end;
  end;
  function FloatEqualToValue(AFloatVal, AOtherVal: TFpDbgValue; ARevert: Boolean = False): TFpDbgValue;
  begin
    Result := nil;
    case AOtherVal.Kind of
      skInteger:  Result := TFpDbgValueConstBool.Create((AFloatVal.AsFloat = AOtherVal.AsInteger) xor ARevert);
      skCardinal: Result := TFpDbgValueConstBool.Create((AFloatVal.AsFloat = AOtherVal.AsCardinal) xor ARevert);
      skFloat:    Result := TFpDbgValueConstBool.Create((AFloatVal.AsFloat = AOtherVal.AsFloat) xor ARevert);
      else SetError('= not supported');
    end;
  end;

  function IntGreaterThanValue(AIntVal, AOtherVal: TFpDbgValue; ARevert: Boolean = False): TFpDbgValue;
  begin
    Result := nil;
    case AOtherVal.Kind of
      skInteger:  Result := TFpDbgValueConstBool.Create((AIntVal.AsInteger > AOtherVal.AsInteger) xor ARevert);
      skCardinal: Result := TFpDbgValueConstBool.Create((AIntVal.AsInteger > AOtherVal.AsCardinal) xor ARevert);
      skFloat:    Result := TFpDbgValueConstBool.Create((AIntVal.AsInteger > AOtherVal.AsFloat) xor ARevert);
      else SetError('= not supported');
    end;
  end;
  function CardinalGreaterThanValue(ACardinalVal, AOtherVal: TFpDbgValue; ARevert: Boolean = False): TFpDbgValue;
  begin
    Result := nil;
    case AOtherVal.Kind of
      skInteger:  Result := TFpDbgValueConstBool.Create((ACardinalVal.AsCardinal > AOtherVal.AsInteger) xor ARevert);
      skCardinal: Result := TFpDbgValueConstBool.Create((ACardinalVal.AsCardinal > AOtherVal.AsCardinal) xor ARevert);
      skFloat:    Result := TFpDbgValueConstBool.Create((ACardinalVal.AsCardinal > AOtherVal.AsFloat) xor ARevert);
      else SetError('= not supported');
    end;
  end;
  function FloatGreaterThanValue(AFloatVal, AOtherVal: TFpDbgValue; ARevert: Boolean = False): TFpDbgValue;
  begin
    Result := nil;
    case AOtherVal.Kind of
      skInteger:  Result := TFpDbgValueConstBool.Create((AFloatVal.AsFloat > AOtherVal.AsInteger) xor ARevert);
      skCardinal: Result := TFpDbgValueConstBool.Create((AFloatVal.AsFloat > AOtherVal.AsCardinal) xor ARevert);
      skFloat:    Result := TFpDbgValueConstBool.Create((AFloatVal.AsFloat > AOtherVal.AsFloat) xor ARevert);
      else SetError('= not supported');
    end;
  end;

  function IntSmallerThanValue(AIntVal, AOtherVal: TFpDbgValue; ARevert: Boolean = False): TFpDbgValue;
  begin
    Result := nil;
    case AOtherVal.Kind of
      skInteger:  Result := TFpDbgValueConstBool.Create((AIntVal.AsInteger < AOtherVal.AsInteger) xor ARevert);
      skCardinal: Result := TFpDbgValueConstBool.Create((AIntVal.AsInteger < AOtherVal.AsCardinal) xor ARevert);
      skFloat:    Result := TFpDbgValueConstBool.Create((AIntVal.AsInteger < AOtherVal.AsFloat) xor ARevert);
      else SetError('= not supported');
    end;
  end;
  function CardinalSmallerThanValue(ACardinalVal, AOtherVal: TFpDbgValue; ARevert: Boolean = False): TFpDbgValue;
  begin
    Result := nil;
    case AOtherVal.Kind of
      skInteger:  Result := TFpDbgValueConstBool.Create((ACardinalVal.AsCardinal < AOtherVal.AsInteger) xor ARevert);
      skCardinal: Result := TFpDbgValueConstBool.Create((ACardinalVal.AsCardinal < AOtherVal.AsCardinal) xor ARevert);
      skFloat:    Result := TFpDbgValueConstBool.Create((ACardinalVal.AsCardinal < AOtherVal.AsFloat) xor ARevert);
      else SetError('= not supported');
    end;
  end;
  function FloatSmallerThanValue(AFloatVal, AOtherVal: TFpDbgValue; ARevert: Boolean = False): TFpDbgValue;
  begin
    Result := nil;
    case AOtherVal.Kind of
      skInteger:  Result := TFpDbgValueConstBool.Create((AFloatVal.AsFloat < AOtherVal.AsInteger) xor ARevert);
      skCardinal: Result := TFpDbgValueConstBool.Create((AFloatVal.AsFloat < AOtherVal.AsCardinal) xor ARevert);
      skFloat:    Result := TFpDbgValueConstBool.Create((AFloatVal.AsFloat < AOtherVal.AsFloat) xor ARevert);
      else SetError('= not supported');
    end;
  end;

{$POP}
var
  tmp1, tmp2: TFpDbgValue;
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
    end;
  end
  else
  if (s = '>') or (s = '<=') then begin
    case tmp1.Kind of
      skInteger:  Result := IntGreaterThanValue(tmp1, tmp2, (s = '<='));
      skCardinal: Result := CardinalGreaterThanValue(tmp1, tmp2, (s = '<='));
      skFloat:    Result := FloatGreaterThanValue(tmp1, tmp2, (s = '<='));
    end;
  end
  else
  if (s = '<') or (s = '>=') then begin
    case tmp1.Kind of
      skInteger:  Result := IntSmallerThanValue(tmp1, tmp2, (s = '>='));
      skCardinal: Result := CardinalSmallerThanValue(tmp1, tmp2, (s = '>='));
      skFloat:    Result := FloatSmallerThanValue(tmp1, tmp2, (s = '>='));
    end;
  end
  else
  if GetText = '><' then begin
    // compare SET
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

function TFpPascalExpressionPartOperatorMemberOf.DoGetResultValue: TFpDbgValue;
var
  tmp: TFpDbgValue;
begin
  Result := nil;
  if Count <> 2 then exit;

  tmp := Items[0].ResultValue;
  if (tmp = nil) then exit;

  if (tmp.Kind in [skClass, skRecord, skObject]) then begin
    Result := tmp.MemberByName[Items[1].GetText];
    if Result = nil then begin
      SetError(fpErrNoMemberWithName, [Items[1].GetText]);
      exit;
    end;
    Result.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(nil, 'DoGetResultValue'){$ENDIF};
    Assert((Result.DbgSymbol=nil)or(Result.DbgSymbol.SymbolType=stValue), 'member is value');
    exit;
  end;

  // Todo unit

  SetError(fpErrorNotAStructure, [Items[1].GetText, Items[0].GetText]);

end;

end.

