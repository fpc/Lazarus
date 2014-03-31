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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
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
    FContext: TDbgInfoAddressContext;
    FTextExpression: String;
    FExpressionPart: TFpPascalExpressionPart;
    FValid: Boolean;
    function GetResultValue: TFpDbgValue;
    procedure Parse;
    procedure SetError(AMsg: String);  // deprecated;
    procedure SetError(AnErrorCode: TFpErrorCode; AData: array of const);
    function PosFromPChar(APChar: PChar): Integer;
  protected
    function GetDbgSymbolForIdentifier({%H-}AnIdent: String): TFpDbgSymbol;
    property ExpressionPart: TFpPascalExpressionPart read FExpressionPart;
    property Context: TDbgInfoAddressContext read FContext;
  public
    constructor Create(ATextExpression: String; AContext: TDbgInfoAddressContext);
    destructor Destroy; override;
    function DebugDump(AWithResults: Boolean = False): String;
    property Error: TFpError read FError;
    property Valid: Boolean read FValid;
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

  { TFpPascalExpressionPartIdentifer }

  TFpPascalExpressionPartIdentifer = class(TFpPascalExpressionPartContainer)
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

type

  {%region  DebugSymbol }

  { TPasParserSymbolPointer
    used by TPasParserSymbolValueMakeReftype.GetDbgSymbol
  }

  TPasParserSymbolPointer = class(TFpDbgSymbol)
  private
    FPointerLevels: Integer;
    FPointedTo: TFpDbgSymbol;
  protected
    // NameNeeded //  "^TPointedTo"
    procedure TypeInfoNeeded; override;
  public
    constructor Create(const APointedTo: TFpDbgSymbol; APointerLevels: Integer);
    constructor Create(const APointedTo: TFpDbgSymbol);
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
  protected
    function DebugText(AIndent: String): String; virtual;
  end;

  { TFpPasParserValueWrapper }

  TFpPasParserValueWrapper = class(TFpPasParserValue)
  private
    FSymbol: TFpDbgSymbol;
    //FTypeSymbol: TFpDbgSymbol;
  protected
    function DebugText(AIndent: String): String; override;
  protected
    function GetKind: TDbgSymbolKind; override;
    function GetDbgSymbol: TFpDbgSymbol; override;
  public
    constructor Create(ASymbol: TFpDbgSymbol); // Only for stType
    destructor Destroy; override;
  end;

  { TFpPasParserValueCastToPointer
    used by TPasParserSymbolPointer.TypeCastValue (which is used by TPasParserSymbolValueMakeReftype.GetDbgSymbol)
  }

  TFpPasParserValueCastToPointer = class(TFpPasParserValue)
  private
    FValue: TFpDbgValue;
    FTypeSymbol: TFpDbgSymbol;
  protected
    function DebugText(AIndent: String): String; override;
  protected
    function GetKind: TDbgSymbolKind; override;
    function GetFieldFlags: TFpDbgValueFieldFlags; override;
    function GetTypeInfo: TFpDbgSymbol; override;
    function GetAsCardinal: QWord; override;
    function GetDataAddress: TFpDbgMemLocation; override;
  public
    constructor Create(AValue: TFpDbgValue; ATypeInfo: TFpDbgSymbol);
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
    constructor Create(ATypeInfo: TFpDbgSymbol);
    destructor Destroy; override;
    procedure IncRefLevel;
  end;

  { TFpPasParserValueDerefPointer
    Used as address source in typecast
  }

  TFpPasParserValueDerefPointer = class(TFpPasParserValue)
  private
    FValue: TFpDbgValue;
    FExpression: TFpPascalExpression; // MemReader / AddrSize
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
    constructor Create(AValue: TFpDbgValue; AExpression: TFpPascalExpression);
    constructor Create(AValue: TFpDbgValue; AExpression: TFpPascalExpression; AOffset: Int64);
    destructor Destroy; override;
  end;

  { TFpPasParserValueAddressOf }

  TFpPasParserValueAddressOf = class(TFpPasParserValue)
  private
    FValue: TFpDbgValue;
    FTypeInfo: TFpDbgSymbol;
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
  public
    constructor Create(AValue: TFpDbgValue);
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
  if svfCardinal in FValue.FieldFlags then
    Result := [svfOrdinal, svfCardinal, svfSizeOfPointer, svfDataAddress]
  else
    Result := [];
end;

function TFpPasParserValueCastToPointer.GetTypeInfo: TFpDbgSymbol;
begin
  Result := FTypeSymbol;
end;

function TFpPasParserValueCastToPointer.GetAsCardinal: QWord;
begin
  if svfCardinal in FValue.FieldFlags then
    Result := FValue.AsCardinal
  else
    Result := 0;
end;

function TFpPasParserValueCastToPointer.GetDataAddress: TFpDbgMemLocation;
begin
  Result := TargetLoc(TDbgPtr(FValue.AsCardinal));
end;

constructor TFpPasParserValueCastToPointer.Create(AValue: TFpDbgValue;
  ATypeInfo: TFpDbgSymbol);
begin
  inherited Create;
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

function TFpPasParserValueMakeReftype.GetDbgSymbol: TFpDbgSymbol;
begin
  if FTypeSymbol = nil then begin
    FTypeSymbol := TPasParserSymbolPointer.Create(FSourceTypeSymbol, FRefLevel);
    {$IFDEF WITH_REFCOUNT_DEBUG}FTypeSymbol.DbgRenameReference(@FSourceTypeSymbol, 'TPasParserSymbolValueMakeReftype'){$ENDIF};
  end;
  Result := FTypeSymbol;
end;

constructor TFpPasParserValueMakeReftype.Create(ATypeInfo: TFpDbgSymbol);
begin
  inherited Create;
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
  Ctx: TDbgInfoAddressContext;
  AddrSize: Integer;
begin
  Result := FCardinal;
  if FCardinalRead then exit;

  Ctx := FExpression.Context;
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
  AExpression: TFpPascalExpression);
begin
  Create(AValue, AExpression, 0);
end;

constructor TFpPasParserValueDerefPointer.Create(AValue: TFpDbgValue;
  AExpression: TFpPascalExpression; AOffset: Int64);
begin
  inherited Create;
  FValue := AValue;
  FValue.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FValue, 'TPasParserDerefPointerSymbolValue'){$ENDIF};
  FExpression := AExpression;
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

  FTypeInfo := TPasParserSymbolPointer.Create(FValue.TypeInfo);
  {$IFDEF WITH_REFCOUNT_DEBUG}FTypeInfo.DbgRenameReference(@FTypeInfo, 'TPasParserAddressOfSymbolValue');{$ENDIF}
  Result := FTypeInfo;
end;

function TFpPasParserValueAddressOf.GetDataAddress: TFpDbgMemLocation;
begin
  Result := FValue.Address;
end;

constructor TFpPasParserValueAddressOf.Create(AValue: TFpDbgValue);
begin
  inherited Create;
  FValue := AValue;
  FValue.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FValue, 'TPasParserAddressOfSymbolValue'){$ENDIF};
end;

destructor TFpPasParserValueAddressOf.Destroy;
begin
  inherited Destroy;
  FValue.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FValue, 'TPasParserAddressOfSymbolValue'){$ENDIF};
  FTypeInfo.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FTypeInfo, 'TPasParserAddressOfSymbolValue'){$ENDIF};
end;

{ TPasParserWrapperSymbolValue }

function TFpPasParserValueWrapper.DebugText(AIndent: String): String;
begin
  Result := inherited DebugText(AIndent)
          + AIndent + '-Symbol = ' + DbgsSymbol(FSymbol, AIndent + '  ') + LineEnding;
end;

function TFpPasParserValueWrapper.GetKind: TDbgSymbolKind;
begin
    Result := skNone;
end;

function TFpPasParserValueWrapper.GetDbgSymbol: TFpDbgSymbol;
begin
  Result := FSymbol;
end;

constructor TFpPasParserValueWrapper.Create(ASymbol: TFpDbgSymbol);
begin
  inherited Create;
  FSymbol := ASymbol;
  FSymbol.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FSymbol, 'TPasParserWrapperSymbolValue'){$ENDIF};
end;

destructor TFpPasParserValueWrapper.Destroy;
begin
  inherited Destroy;
  FSymbol.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FSymbol, 'TPasParserWrapperSymbolValue'){$ENDIF};
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
  t := TPasParserSymbolPointer.Create(FPointedTo, FPointerLevels-1);
  SetTypeInfo(t);
  t.ReleaseReference;
end;

constructor TPasParserSymbolPointer.Create(const APointedTo: TFpDbgSymbol;
  APointerLevels: Integer);
begin
  inherited Create('');
  FPointerLevels := APointerLevels;
  FPointedTo := APointedTo;
  FPointedTo.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(FPointedTo, 'TPasParserSymbolPointer'){$ENDIF};
  if APointerLevels = 1 then
    SetTypeInfo(APointedTo);
  SetKind(skPointer);
  SetSymbolType(stType);
end;

constructor TPasParserSymbolPointer.Create(const APointedTo: TFpDbgSymbol);
begin
  Create(APointedTo, 1);
end;

destructor TPasParserSymbolPointer.Destroy;
begin
  FPointedTo.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(FPointedTo, 'TPasParserSymbolPointer'){$ENDIF};
  inherited Destroy;
end;

function TPasParserSymbolPointer.TypeCastValue(AValue: TFpDbgValue): TFpDbgValue;
begin
  Result := TFpPasParserValueCastToPointer.Create(AValue, Self);
end;


{ TFpPascalExpressionPartBracketIndex }

procedure TFpPascalExpressionPartBracketIndex.Init;
begin
  FPrecedence := PRECEDENCE_ARRAY_IDX;
  inherited Init;
end;

function TFpPascalExpressionPartBracketIndex.DoGetResultValue: TFpDbgValue;
var
  TmpVal, TmpVal2, TmpIndex, TmpDeref: TFpDbgValue;
  i: Integer;
  Offs: Int64;
  ti: TFpDbgSymbol;
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
    if (TmpVal.Kind = skArray) then begin
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
    end // Kind = skArray
    else
    if (TmpVal.Kind = skPointer) then begin
      if (TmpVal.TypeInfo = nil) or (TmpVal.TypeInfo.TypeInfo = nil) or
         (TmpVal.TypeInfo.TypeInfo.Size <= 0)
      then begin
        SetError('Can not dereference an untyped pointer');
        TmpVal.ReleaseReference;
        exit;
      end;
      // TODO: check svfDataAddress / readable ? (see normal pointer deref);
      ti := TmpVal.TypeInfo.TypeInfo;
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
      {$PUSH}{$R-}{$Q-} // TODO: check overflow
      Offs := Offs * ti.Size;
      {$POP}
      TmpDeref := TFpPasParserValueDerefPointer.Create(TmpVal, Expression, Offs);
      TmpVal2 := ti.TypeCastValue(TmpDeref);
      TmpDeref.ReleaseReference;
    end
    else
    if (TmpVal.Kind = skString) then begin
      //TODO
      SetError('Not implemented');
      TmpVal.ReleaseReference;
      exit;
    end
    else
    begin
      SetError(fpErrCannotDereferenceType, [GetText]);
      TmpVal.ReleaseReference;
      exit;
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

  if (Count = 2) then begin
    //TODO if tmp is TFpPascalExpressionPartOperatorMakeRef then
    //     AVOID creating the TPasParserSymbolPointer by calling tmp.DbgSymbol
    //     it ran be created in TPasParserSymbolValueCastToPointer if needed.
    tmp := Items[0].ResultValue;
    if (tmp <> nil) and (tmp.DbgSymbol <> nil) and
       (tmp.DbgSymbol.SymbolType = stType)
    then begin
      // This is a typecast
      tmp2 := Items[1].ResultValue;
      if tmp2 <> nil then
        Result := tmp.DbgSymbol.TypeCastValue(tmp2);
      if Result <> nil then
        {$IFDEF WITH_REFCOUNT_DEBUG}Result.DbgRenameReference(nil, 'DoGetResultValue'){$ENDIF};
      exit;
    end;
  end;
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

{ TFpPascalExpressionPartIdentifer }

function TFpPascalExpressionPartIdentifer.DoGetIsTypeCast: Boolean;
begin
  Result := (ResultValue <> nil) and (ResultValue.DbgSymbol <> nil) and (ResultValue.DbgSymbol.SymbolType = stType);
end;

function TFpPascalExpressionPartIdentifer.DoGetResultValue: TFpDbgValue;
var
  DbgSymbol: TFpDbgSymbol;
begin
  Result := nil;
  DbgSymbol := FExpression.GetDbgSymbolForIdentifier(GetText);
  if DbgSymbol = nil then begin
    SetError(fpErrSymbolNotFound, [GetText]);
    exit;
  end;

  Result := DbgSymbol.Value;
  if Result = nil then begin
    Result := TFpPasParserValueWrapper.Create(DbgSymbol);
    {$IFDEF WITH_REFCOUNT_DEBUG}Result.DbgRenameReference(nil, 'DoGetResultValue'){$ENDIF};
  end
  else
    Result.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(nil, 'DoGetResultValue'){$ENDIF};

  DbgSymbol.ReleaseReference; // hold via value
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

  Result := TFpDbgValueConstNumber.Create(i, False);
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
    NewPart := TFpPascalExpressionPartIdentifer.Create(Self, CurPtr, TokenEndPtr-1);
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
        else
          while TokenEndPtr^ in ['0'..'9'] do inc(TokenEndPtr);
    end;
    AddPart(TFpPascalExpressionPartConstantNumber);
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
  else
    Result := FExpressionPart.ResultValue;
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
end;

function TFpPascalExpression.PosFromPChar(APChar: PChar): Integer;
begin
  Result := APChar - @FTextExpression[1] + 1;
end;

function TFpPascalExpression.GetDbgSymbolForIdentifier(AnIdent: String): TFpDbgSymbol;
begin
  if FContext <> nil then
    Result := FContext.FindSymbol(AnIdent)
  else
    Result := nil;
end;

constructor TFpPascalExpression.Create(ATextExpression: String;
  AContext: TDbgInfoAddressContext);
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
  FResultValue.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(nil, 'DoGetResultValue'){$ENDIF};
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
  //   Identifer
  // "Identifer" can hane a binary-op next. But it must be applied to the parent.
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

  Result := TFpPasParserValueAddressOf.Create(tmp);
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
              ( (APart is TFpPascalExpressionPartIdentifer) or
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

  Result := TFpPasParserValueMakeReftype.Create(tmp.DbgSymbol);
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
  tmp, tmp2: TFpDbgValue;
begin
  Result := nil;
  if Count <> 1 then exit;

  tmp := Items[0].ResultValue;
  if tmp = nil then
    exit;

  if tmp is TFpPasParserValueAddressOf then begin
    Result := TFpPasParserValueAddressOf(tmp).PointedToValue;
    Result.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(nil, 'DoGetResultValue'){$ENDIF};
  end
  else
  if tmp.Kind = skPointer then begin
    if (svfDataAddress in tmp.FieldFlags) and (IsReadableLoc(tmp.DataAddress)) and // TODO, what if Not readable addr
       (tmp.TypeInfo <> nil) //and (tmp.TypeInfo.TypeInfo <> nil)
    then begin
      //TODO: maybe introduce a method TypeCastFromAddress, so we can skip the twp2 object
//todo, if tmp2 is a TPasParserAddressOfSymbolValue, then no new object is neede....
      tmp2 := TFpPasParserValueDerefPointer.Create(tmp, Expression);
      if (tmp.TypeInfo.TypeInfo <> nil) then
        Result := tmp.TypeInfo.TypeInfo.TypeCastValue(tmp2)
      else
        Result := tmp2;
      {$IFDEF WITH_REFCOUNT_DEBUG} if Result <> nil then Result.DbgRenameReference(nil, 'DoGetResultValue'){$ENDIF};
      if (tmp.TypeInfo.TypeInfo <> nil) then
        tmp2.ReleaseReference;
    end;
  end
  //if tmp.Kind = skArray then // dynarray
  else
    Result := nil;

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
  //   Identifer
  // "Identifer" can hane a binary-op next. But it must be applied to the parent.
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

  {$PUSH}{$R-}{$Q-}
  if IsAdd then begin
    case tmp1.Kind of
      skPointer: ;
      skInteger: case tmp2.Kind of
        skPointer: ;
        skInteger:  Result := TFpDbgValueConstNumber.Create(tmp1.AsInteger + tmp2.AsInteger, True);
        skCardinal: Result := TFpDbgValueConstNumber.Create(tmp1.AsInteger + tmp2.AsCardinal, True);
      end;
      skCardinal: case tmp2.Kind of
        skPointer: ;
        skInteger:  Result := TFpDbgValueConstNumber.Create(tmp1.AsCardinal + tmp2.AsInteger, True);
        skCardinal: Result := TFpDbgValueConstNumber.Create(tmp1.AsCardinal + tmp2.AsCardinal, False);
      end;
    end;
  end
  else begin
    case tmp1.Kind of
      skPointer: ;
      skInteger: case tmp2.Kind of
        skPointer: ;
        skInteger:  Result := TFpDbgValueConstNumber.Create(tmp1.AsInteger - tmp2.AsInteger, True);
        skCardinal: Result := TFpDbgValueConstNumber.Create(tmp1.AsInteger - tmp2.AsCardinal, True);
      end;
      skCardinal: case tmp2.Kind of
        skPointer: ;
        skInteger:  Result := TFpDbgValueConstNumber.Create(tmp1.AsCardinal - tmp2.AsInteger, True);
        skCardinal: Result := TFpDbgValueConstNumber.Create(tmp1.AsCardinal - tmp2.AsCardinal, True);
      end;
    end;
  end;
  {$POP}

 {$IFDEF WITH_REFCOUNT_DEBUG}if Result <> nil then
   Result.DbgRenameReference(nil, 'DoGetResultValue');{$ENDIF}
end;

{ TFpPascalExpressionPartOperatorMulDiv }

procedure TFpPascalExpressionPartOperatorMulDiv.Init;
begin
  FPrecedence := PRECEDENCE_MUL_DIV;
  inherited Init;
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
    Result := Result and (APart is TFpPascalExpressionPartIdentifer);
end;

function TFpPascalExpressionPartOperatorMemberOf.DoGetResultValue: TFpDbgValue;
var
  tmp: TFpDbgValue;
begin
  Result := nil;
  if Count <> 2 then exit;

  tmp := Items[0].ResultValue;
  if (tmp = nil) then exit;
  // Todo unit
  if (tmp.Kind in [skClass, skRecord, skObject]) then begin
    Result := tmp.MemberByName[Items[1].GetText];
    if Result = nil then begin
      SetError(fpErrNoMemberWithName, [Items[1].GetText]);
      exit;
    end;
    Result.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(nil, 'DoGetResultValue'){$ENDIF};
    Assert((Result.DbgSymbol=nil)or(Result.DbgSymbol.SymbolType=stValue), 'member is value');
  end;
end;

end.

