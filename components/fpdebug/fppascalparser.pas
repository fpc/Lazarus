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
  Classes, sysutils, math, FpDbgInfo, LazLoggerBase, LazClasses;

type

  TFpPascalExpressionPart = class;
  TFpPascalExpressionPartContainer = class;
  TFpPascalExpressionPartWithPrecedence = class;
  TFpPascalExpressionPartBracket = class;
  TFpPascalExpressionPartOperator = class;

  TFpPascalExpressionPartClass = class of TFpPascalExpressionPart;
  TFpPascalExpressionPartBracketClass = class of TFpPascalExpressionPartBracket;

  { TFpPascalExpression }

  TFpPascalExpression = class
  private
    FError: String;
    FContext: TDbgInfoAddressContext;
    FTextExpression: String;
    FExpressionPart: TFpPascalExpressionPart;
    FValid: Boolean;
    function GetResultValue: TDbgSymbolValue;
    procedure Parse;
    procedure SetError(AMsg: String);
    function PosFromPChar(APChar: PChar): Integer;
  protected
    function GetDbgSymbolForIdentifier({%H-}AnIdent: String): TDbgSymbol;
    property ExpressionPart: TFpPascalExpressionPart read FExpressionPart;
    property Context: TDbgInfoAddressContext read FContext;
  public
    constructor Create(ATextExpression: String; AContext: TDbgInfoAddressContext);
    destructor Destroy; override;
    function DebugDump(AWithResults: Boolean = False): String;
    property Error: String read FError;
    property Valid: Boolean read FValid;
    // ResultValue
    // - May be a type, if expression is a type
    // - Only valid, as long as the expression is not destroyed
    property ResultValue: TDbgSymbolValue read GetResultValue;
  end;


  { TFpPascalExpressionPart }

  TFpPascalExpressionPart = class
  private
    FEndChar: PChar;
    FParent: TFpPascalExpressionPartContainer;
    FStartChar: PChar;
    FExpression: TFpPascalExpression;
    FResultValue: TDbgSymbolValue;
    FResultValDone: Boolean;
    function GetResultValue: TDbgSymbolValue;
    function GetSurroundingOpenBracket: TFpPascalExpressionPartBracket;
    function GetTopParent: TFpPascalExpressionPart;
    procedure SetEndChar(AValue: PChar);
    procedure SetParent(AValue: TFpPascalExpressionPartContainer);
    procedure SetStartChar(AValue: PChar);
    procedure SetError(AMsg: String = '');
    procedure SetError(APart: TFpPascalExpressionPart; AMsg: String = '');
  protected
    function DebugText(AIndent: String; AWithResults: Boolean): String; virtual; // Self desc only
    function DebugDump(AIndent: String; AWithResults: Boolean): String; virtual;
  protected
    procedure Init; virtual;
    function  DoGetIsTypeCast: Boolean; virtual; deprecated;
    function  DoGetResultValue: TDbgSymbolValue; virtual;

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
    property ResultValue: TDbgSymbolValue read GetResultValue;
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
    function DoGetResultValue: TDbgSymbolValue; override;
  end;

  TFpPascalExpressionPartConstant = class(TFpPascalExpressionPartContainer)
  end;

  { TFpPascalExpressionPartConstantNumber }

  TFpPascalExpressionPartConstantNumber = class(TFpPascalExpressionPartConstant)
  protected
    function DoGetResultValue: TDbgSymbolValue; override;
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
  protected
    procedure Init; override;
    function HasPrecedence: Boolean; override;
    procedure DoHandleEndOfExpression; override;
    function CanHaveOperatorAsNext: Boolean; override;
    function HandleNextPartInBracket(APart: TFpPascalExpressionPart): TFpPascalExpressionPart; virtual;
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
    function DoGetResultValue: TDbgSymbolValue; override;
  end;

  { TFpPascalExpressionPartBracketArgumentList }

  TFpPascalExpressionPartBracketArgumentList = class(TFpPascalExpressionPartRoundBracket)
  // function arguments or type cast // this acts a operator: first element is the function/type
  protected
    procedure Init; override;
    function DoGetResultValue: TDbgSymbolValue; override;
    function DoGetIsTypeCast: Boolean; override;
    function IsValidAfterPart(APrevPart: TFpPascalExpressionPart): Boolean; override;
    function HandleNextPartInBracket(APart: TFpPascalExpressionPart): TFpPascalExpressionPart; override;
    function MaybeHandlePrevPart(APrevPart: TFpPascalExpressionPart;
      var AResult: TFpPascalExpressionPart): Boolean; override;
  end;


  TFpPascalExpressionPartSquareBracket = class(TFpPascalExpressionPartBracket)
  protected
  end;

  { TFpPascalExpressionPartBracketSet }

  TFpPascalExpressionPartBracketSet = class(TFpPascalExpressionPartSquareBracket)
  // a in [x, y, z]
  protected
    function HandleNextPartInBracket(APart: TFpPascalExpressionPart): TFpPascalExpressionPart; override;
  end;

  { TFpPascalExpressionPartBracketIndex }

  TFpPascalExpressionPartBracketIndex = class(TFpPascalExpressionPartSquareBracket)
  // array[1]
  protected
    procedure Init; override;
    //function DoGetResultType: TDbgSymbol; override;
    function IsValidAfterPart(APrevPart: TFpPascalExpressionPart): Boolean; override;
    function HandleNextPartInBracket(APart: TFpPascalExpressionPart): TFpPascalExpressionPart; override;
    function MaybeHandlePrevPart(APrevPart: TFpPascalExpressionPart;
      var AResult: TFpPascalExpressionPart): Boolean; override;
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
    function DoGetResultValue: TDbgSymbolValue; override;
  end;

  { TFpPascalExpressionPartOperatorMakeRef }

  TFpPascalExpressionPartOperatorMakeRef = class(TFpPascalExpressionPartUnaryOperator)  // ^TTYpe
  protected
    procedure Init; override;
    function IsValidNextPart(APart: TFpPascalExpressionPart): Boolean; override;
    function DoGetResultValue: TDbgSymbolValue; override;
    function DoGetIsTypeCast: Boolean; override;
  end;

  { TFpPascalExpressionPartOperatorDeRef }

  TFpPascalExpressionPartOperatorDeRef = class(TFpPascalExpressionPartUnaryOperator)  // ptrval^
  protected
    procedure Init; override;
    function DoGetResultValue: TDbgSymbolValue; override;
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
  end;

  { TFpPascalExpressionPartOperatorPlusMinus }

  TFpPascalExpressionPartOperatorPlusMinus = class(TFpPascalExpressionPartBinaryOperator)  // + -
  // Binary + -
  protected
    procedure Init; override;
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
    function DoGetResultValue: TDbgSymbolValue; override;
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

  TPasParserSymbolPointer = class(TDbgSymbol)
  private
    FPointerLevels: Integer;
    FPointedTo: TDbgSymbol;
  protected
    // NameNeeded //  "^TPointedTo"
    procedure TypeInfoNeeded; override;
  public
    constructor Create(const APointedTo: TDbgSymbol; APointerLevels: Integer);
    constructor Create(const APointedTo: TDbgSymbol);
    destructor Destroy; override;
    function TypeCastValue(AValue: TDbgSymbolValue): TDbgSymbolValue; override;
  end;

  { TPasParserSymbolArrayDeIndex }

  TPasParserSymbolArrayDeIndex = class(TDbgSymbolForwarder) // 1 index level off
  private
    FArray: TDbgSymbol;
  protected
    //procedure ForwardToSymbolNeeded; override;
    function GetMemberCount: Integer; override;
    function GetMember(AIndex: Integer): TDbgSymbol; override;
  public
    constructor Create(const AnArray: TDbgSymbol);
    destructor Destroy; override;
  end;

  {%endregion  DebugSymbol }

  {%region  DebugSymbolValue }

  { TPasParserSymbolValue }

  TPasParserSymbolValue = class(TDbgSymbolValue)
  protected
    function DebugText(AIndent: String): String; virtual;
  end;

  { TPasParserSymbolValueWrapper }

  TPasParserSymbolValueWrapper = class(TPasParserSymbolValue)
  private
    FSymbol: TDbgSymbol;
    //FTypeSymbol: TDbgSymbol;
  protected
    function DebugText(AIndent: String): String; override;
  protected
    function GetKind: TDbgSymbolKind; override;
    function GetDbgSymbol: TDbgSymbol; override;
  public
    constructor Create(ASymbol: TDbgSymbol); // Only for stType
    destructor Destroy; override;
  end;

  { TPasParserSymbolValueCastToPointer
    used by TPasParserSymbolPointer.TypeCastValue (which is used by TPasParserSymbolValueMakeReftype.GetDbgSymbol)
  }

  TPasParserSymbolValueCastToPointer = class(TPasParserSymbolValue)
  private
    FValue: TDbgSymbolValue;
    FTypeSymbol: TDbgSymbol;
  protected
    function DebugText(AIndent: String): String; override;
  protected
    function GetKind: TDbgSymbolKind; override;
    function GetFieldFlags: TDbgSymbolValueFieldFlags; override;
    function GetTypeInfo: TDbgSymbol; override;
    function GetAsCardinal: QWord; override;
    function GetDataAddress: TDbgPtr; override;
  public
    constructor Create(AValue: TDbgSymbolValue; ATypeInfo: TDbgSymbol);
    destructor Destroy; override;
  end;

  { TPasParserConstNumberSymbolValue }

  TPasParserSymbolValueConstNumber = class(TPasParserSymbolValue)
  private
    FValue: QWord;
    FSigned: Boolean;
  protected
    function GetKind: TDbgSymbolKind; override;
    function GetFieldFlags: TDbgSymbolValueFieldFlags; override;
    function GetAsCardinal: QWord; override;
    function GetAsInteger: Int64; override;
  public
    constructor Create(AValue: QWord; ASigned: Boolean = False);
  end;

  { TPasParserSymbolValueMakeReftype }

  TPasParserSymbolValueMakeReftype = class(TPasParserSymbolValue)
  private
    FSourceTypeSymbol, FTypeSymbol: TDbgSymbol;
    FRefLevel: Integer;
  protected
    function DebugText(AIndent: String): String; override;
  protected
    function GetDbgSymbol: TDbgSymbol; override; // returns a TPasParserSymbolPointer
  public
    constructor Create(ATypeInfo: TDbgSymbol);
    destructor Destroy; override;
    procedure IncRefLevel;
  end;

  { TPasParserDerefPointerSymbolValue }

  { TPasParserSymbolValueDerefPointer }

  TPasParserSymbolValueDerefPointer = class(TPasParserSymbolValue)
  private
    FValue: TDbgSymbolValue;
    FExpression: TFpPascalExpression; // MemReader / AddrSize
    FCardinal: QWord;
    FCardinalRead: Boolean;
  protected
    function DebugText(AIndent: String): String; override;
  protected
    function GetFieldFlags: TDbgSymbolValueFieldFlags; override;
    function GetAddress: TDbgPtr; override;
    function GetSize: Integer; override;
    function GetAsCardinal: QWord; override; // reads men
    function GetTypeInfo: TDbgSymbol; override;
  public
    constructor Create(AValue: TDbgSymbolValue; AExpression: TFpPascalExpression);
    destructor Destroy; override;
  end;

  { TPasParserAddressOfSymbolValue }

  { TPasParserSymbolValueAddressOf }

  TPasParserSymbolValueAddressOf = class(TPasParserSymbolValue)
  private
    FValue: TDbgSymbolValue;
    FTypeInfo: TDbgSymbol;
    function GetPointedToValue: TDbgSymbolValue;
  protected
    function DebugText(AIndent: String): String; override;
  protected
    function GetKind: TDbgSymbolKind; override;
    function GetFieldFlags: TDbgSymbolValueFieldFlags; override;
    function GetAsInteger: Int64; override;
    function GetAsCardinal: QWord; override;
    function GetTypeInfo: TDbgSymbol; override;
    function GetDataAddress: TDbgPtr; override;
  public
    constructor Create(AValue: TDbgSymbolValue);
    destructor Destroy; override;
    property PointedToValue: TDbgSymbolValue read GetPointedToValue;
  end;

  {%endregion  DebugSymbolValue }

function DbgsResultValue(AVal: TDbgSymbolValue; AIndent: String): String;
begin
  if (AVal <> nil) and (AVal is TPasParserSymbolValue) then
    Result := LineEnding + TPasParserSymbolValue(AVal).DebugText(AIndent)
  else
  if AVal <> nil then
    Result := DbgSName(AVal) + '  DbsSym='+DbgSName(AVal.DbgSymbol)+' Type='+DbgSName(AVal.TypeInfo)
  else
    Result := DbgSName(AVal);
end;

function DbgsSymbol(AVal: TDbgSymbol; AIndent: String): String;
begin
  Result := DbgSName(AVal);
end;

function TPasParserSymbolValue.DebugText(AIndent: String): String;
begin
  Result := AIndent + DbgSName(Self)  + '  DbsSym='+DbgSName(DbgSymbol)+' Type='+DbgSName(TypeInfo) + LineEnding;
end;

{ TPasParserSymbolValueCastToPointer }

function TPasParserSymbolValueCastToPointer.DebugText(AIndent: String): String;
begin
  Result := inherited DebugText(AIndent)
          + AIndent + '-Value= ' + DbgsResultValue(FValue, AIndent + '  ') + LineEnding
          + AIndent + '-Symbol = ' + DbgsSymbol(FTypeSymbol, AIndent + '  ') + LineEnding;
end;

function TPasParserSymbolValueCastToPointer.GetKind: TDbgSymbolKind;
begin
  Result := skPointer;
end;

function TPasParserSymbolValueCastToPointer.GetFieldFlags: TDbgSymbolValueFieldFlags;
begin
  if svfCardinal in FValue.FieldFlags then
    Result := [svfOrdinal, svfCardinal, svfDataAddress]
  else
    Result := [];
end;

function TPasParserSymbolValueCastToPointer.GetTypeInfo: TDbgSymbol;
begin
  Result := FTypeSymbol;
end;

function TPasParserSymbolValueCastToPointer.GetAsCardinal: QWord;
begin
  if svfCardinal in FValue.FieldFlags then
    Result := FValue.AsCardinal
  else
    Result := 0;
end;

function TPasParserSymbolValueCastToPointer.GetDataAddress: TDbgPtr;
begin
  Result := TDbgPtr(FValue.AsCardinal);
end;

constructor TPasParserSymbolValueCastToPointer.Create(AValue: TDbgSymbolValue;
  ATypeInfo: TDbgSymbol);
begin
  inherited Create;
  FValue := AValue;
  FValue.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FValue, 'TPasParserSymbolValueCastToPointer'){$ENDIF};
  FTypeSymbol := ATypeInfo;
  FTypeSymbol.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FTypeSymbol, 'TPasParserSymbolValueCastToPointer'){$ENDIF};
  Assert((FTypeSymbol=nil) or (FTypeSymbol.Kind = skPointer), 'TPasParserSymbolValueCastToPointer.Create');
end;

destructor TPasParserSymbolValueCastToPointer.Destroy;
begin
  FValue.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FValue, 'TPasParserSymbolValueCastToPointer'){$ENDIF};
  FTypeSymbol.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FTypeSymbol, 'TPasParserSymbolValueCastToPointer'){$ENDIF};
  inherited Destroy;
end;

{ TPasParserSymbolValueMakeReftype }

function TPasParserSymbolValueMakeReftype.DebugText(AIndent: String): String;
begin
  Result := inherited DebugText(AIndent)
          + AIndent + '-RefLevel = ' + dbgs(FRefLevel) + LineEnding
          + AIndent + '-SourceSymbol = ' + DbgsSymbol(FSourceTypeSymbol, AIndent + '  ') + LineEnding
          + AIndent + '-Symbol = ' + DbgsSymbol(FTypeSymbol, AIndent + '  ') + LineEnding;
end;

function TPasParserSymbolValueMakeReftype.GetDbgSymbol: TDbgSymbol;
begin
  if FTypeSymbol = nil then begin
    FTypeSymbol := TPasParserSymbolPointer.Create(FSourceTypeSymbol, FRefLevel);
    {$IFDEF WITH_REFCOUNT_DEBUG}FTypeSymbol.DbgRenameReference(@FSourceTypeSymbol, 'TPasParserSymbolValueMakeReftype'){$ENDIF};
  end;
  Result := FTypeSymbol;
end;

constructor TPasParserSymbolValueMakeReftype.Create(ATypeInfo: TDbgSymbol);
begin
  inherited Create;
  FSourceTypeSymbol := ATypeInfo;
  FSourceTypeSymbol.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FSourceTypeSymbol, 'TPasParserSymbolValueMakeReftype'){$ENDIF};
  FRefLevel := 1;
end;

destructor TPasParserSymbolValueMakeReftype.Destroy;
begin
  FSourceTypeSymbol.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FSourceTypeSymbol, 'TPasParserSymbolValueMakeReftype'){$ENDIF};
  FTypeSymbol.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FSourceTypeSymbol, 'TPasParserSymbolValueMakeReftype'){$ENDIF};
  inherited Destroy;
end;

procedure TPasParserSymbolValueMakeReftype.IncRefLevel;
begin
  inc(FRefLevel);
end;


{ TPasParserDerefPointerSymbolValue }

function TPasParserSymbolValueDerefPointer.DebugText(AIndent: String): String;
begin
  Result := inherited DebugText(AIndent)
          + AIndent + '-Value= ' + DbgsResultValue(FValue, AIndent + '  ') + LineEnding;
end;

function TPasParserSymbolValueDerefPointer.GetFieldFlags: TDbgSymbolValueFieldFlags;
var
  t: TDbgSymbol;
begin
  // MUST *NOT* have ordinal
  Result := [svfAddress];
  t := FValue.TypeInfo;
  if t <> nil then t := t.TypeInfo;
  if t <> nil then
    if t.Kind = skPointer then begin
      //Result := Result + [svfSizeOfPointer];
      Result := Result + [svfSizeOfPointer, svfCardinal, svfOrdinal];
    end
    else
      Result := Result + [svfSize];
end;

function TPasParserSymbolValueDerefPointer.GetAddress: TDbgPtr;
begin
  Result := FValue.DataAddress;
end;

function TPasParserSymbolValueDerefPointer.GetSize: Integer;
var
  t: TDbgSymbol;
begin
  t := FValue.TypeInfo;
  if t <> nil then t := t.TypeInfo;
  if t <> nil then
    Result := t.Size
  else
    Result := inherited GetSize;
end;

function TPasParserSymbolValueDerefPointer.GetAsCardinal: QWord;
var
  m: TFpDbgMemReaderBase;
  Addr: TDbgPtr;
  Ctx: TDbgInfoAddressContext;
  AddrSize: Integer;
begin
  Result := FCardinal;
  if FCardinalRead then exit;

  Ctx := FExpression.Context;
  if Ctx = nil then exit;
  AddrSize := Ctx.SizeOfAddress;
  if (AddrSize <= 0) or (AddrSize > SizeOf(FCardinal)) then exit;
  m := Ctx.MemReader;
  if m = nil then exit;

  FCardinal := 0;
  FCardinalRead := True;
  Addr := GetAddress;
  if Addr = 0 then exit;
  m.ReadMemory(Addr, Ctx.SizeOfAddress, @FCardinal);

  Result := FCardinal;
end;

function TPasParserSymbolValueDerefPointer.GetTypeInfo: TDbgSymbol;
var
  t: TDbgSymbol;
begin
  t := FValue.TypeInfo;
  if t <> nil then t := t.TypeInfo;
  if t <> nil then
    Result := t
  else
    Result := inherited GetTypeInfo;
end;

constructor TPasParserSymbolValueDerefPointer.Create(AValue: TDbgSymbolValue;
  AExpression: TFpPascalExpression);
begin
  inherited Create;
  FValue := AValue;
  FValue.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FValue, 'TPasParserDerefPointerSymbolValue'){$ENDIF};
  FExpression := AExpression;
end;

destructor TPasParserSymbolValueDerefPointer.Destroy;
begin
  inherited Destroy;
  FValue.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FValue, 'TPasParserDerefPointerSymbolValue'){$ENDIF};
end;

{ TPasParserAddressOfSymbolValue }

function TPasParserSymbolValueAddressOf.GetPointedToValue: TDbgSymbolValue;
begin
  Result := FValue;
end;

function TPasParserSymbolValueAddressOf.DebugText(AIndent: String): String;
begin
  Result := inherited DebugText(AIndent)
          + AIndent + '-Value= ' + DbgsResultValue(FValue, AIndent + '  ') + LineEnding
          + AIndent + '-Symbol = ' + DbgsSymbol(FTypeInfo, AIndent + '  ') + LineEnding;
end;

function TPasParserSymbolValueAddressOf.GetKind: TDbgSymbolKind;
begin
  Result := skPointer;
end;

function TPasParserSymbolValueAddressOf.GetFieldFlags: TDbgSymbolValueFieldFlags;
begin
    Result := [svfOrdinal, svfCardinal, svfDataAddress];
end;

function TPasParserSymbolValueAddressOf.GetAsInteger: Int64;
begin
  Result := Int64(FValue.Address);
end;

function TPasParserSymbolValueAddressOf.GetAsCardinal: QWord;
begin
  Result := QWord(FValue.Address);
end;

function TPasParserSymbolValueAddressOf.GetTypeInfo: TDbgSymbol;
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

function TPasParserSymbolValueAddressOf.GetDataAddress: TDbgPtr;
begin
  Result := FValue.Address;
end;

constructor TPasParserSymbolValueAddressOf.Create(AValue: TDbgSymbolValue);
begin
  inherited Create;
  FValue := AValue;
  FValue.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FValue, 'TPasParserAddressOfSymbolValue'){$ENDIF};
end;

destructor TPasParserSymbolValueAddressOf.Destroy;
begin
  inherited Destroy;
  FValue.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FValue, 'TPasParserAddressOfSymbolValue'){$ENDIF};
  FTypeInfo.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FTypeInfo, 'TPasParserAddressOfSymbolValue'){$ENDIF};
end;

{ TPasParserConstNumberSymbolValue }

function TPasParserSymbolValueConstNumber.GetKind: TDbgSymbolKind;
begin
  if FSigned then
    Result := skInteger
  else
    Result := skCardinal;
end;

function TPasParserSymbolValueConstNumber.GetFieldFlags: TDbgSymbolValueFieldFlags;
begin
  if FSigned then
    Result := [svfOrdinal, svfInteger]
  else
    Result := [svfOrdinal, svfCardinal];
end;

function TPasParserSymbolValueConstNumber.GetAsCardinal: QWord;
begin
  Result := FValue;
end;

function TPasParserSymbolValueConstNumber.GetAsInteger: Int64;
begin
  Result := Int64(FValue);
end;

constructor TPasParserSymbolValueConstNumber.Create(AValue: QWord; ASigned: Boolean);
begin
  inherited Create;
  FValue := AValue;
  FSigned := ASigned;
end;

{ TPasParserWrapperSymbolValue }

function TPasParserSymbolValueWrapper.DebugText(AIndent: String): String;
begin
  Result := inherited DebugText(AIndent)
          + AIndent + '-Symbol = ' + DbgsSymbol(FSymbol, AIndent + '  ') + LineEnding;
end;

function TPasParserSymbolValueWrapper.GetKind: TDbgSymbolKind;
begin
    Result := skNone;
end;

function TPasParserSymbolValueWrapper.GetDbgSymbol: TDbgSymbol;
begin
  Result := FSymbol;
end;

constructor TPasParserSymbolValueWrapper.Create(ASymbol: TDbgSymbol);
begin
  inherited Create;
  FSymbol := ASymbol;
  FSymbol.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FSymbol, 'TPasParserWrapperSymbolValue'){$ENDIF};
end;

destructor TPasParserSymbolValueWrapper.Destroy;
begin
  inherited Destroy;
  FSymbol.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(@FSymbol, 'TPasParserWrapperSymbolValue'){$ENDIF};
end;

{ TPasParserSymbolArrayDeIndex }

function TPasParserSymbolArrayDeIndex.GetMemberCount: Integer;
begin
  Result := (inherited GetMemberCount) - 1;
end;

function TPasParserSymbolArrayDeIndex.GetMember(AIndex: Integer): TDbgSymbol;
begin
  Result := inherited GetMember(AIndex + 1);
end;

constructor TPasParserSymbolArrayDeIndex.Create(const AnArray: TDbgSymbol);
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

constructor TPasParserSymbolPointer.Create(const APointedTo: TDbgSymbol;
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

constructor TPasParserSymbolPointer.Create(const APointedTo: TDbgSymbol);
begin
  Create(APointedTo, 1);
end;

destructor TPasParserSymbolPointer.Destroy;
begin
  FPointedTo.ReleaseReference{$IFDEF WITH_REFCOUNT_DEBUG}(FPointedTo, 'TPasParserSymbolPointer'){$ENDIF};
  inherited Destroy;
end;

function TPasParserSymbolPointer.TypeCastValue(AValue: TDbgSymbolValue): TDbgSymbolValue;
begin
  Result := TPasParserSymbolValueCastToPointer.Create(AValue, Self);
end;


{ TFpPascalExpressionPartBracketIndex }

procedure TFpPascalExpressionPartBracketIndex.Init;
begin
  FPrecedence := PRECEDENCE_ARRAY_IDX;
  inherited Init;
end;

//function TFpPascalExpressionPartBracketIndex.DoGetResultType: TDbgSymbol;
//var
//  tmp: TDbgSymbol;
//begin
//  Result := nil;
//  if Count <> 2 then exit;
//
//  tmp := Items[0].ResultType;
//  if tmp = nil then exit;
//
//  if (tmp.Kind = skArray) then begin
//    // TODO: check type of index
//    if tmp.MemberCount < 1 then exit; // TODO error
//    if tmp.MemberCount = 1 then begin
//      Result := tmp.TypeInfo;
//      Result.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(nil, 'DoGetResultType'){$ENDIF};
//      exit;
//    end;
//
//    Result := TPasParserSymbolArrayDeIndex.Create(tmp);
//  end
//  else
//  if (tmp.Kind = skPointer) then begin
//    Result := tmp.TypeInfo;
//    Result.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(nil, 'DoGetResultType'){$ENDIF};
//    exit;
//  end
//  else
//  if (tmp.Kind = skString) then begin
//    //TODO
//    exit;
//  end;
//end;

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
  if Count <> 1 then begin // Todo a,b,c
    SetError(APart, 'Too many operands in [] '+GetText+': ');
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

{ TFpPascalExpressionPartBracketSet }

function TFpPascalExpressionPartBracketSet.HandleNextPartInBracket(APart: TFpPascalExpressionPart): TFpPascalExpressionPart;
begin
  Result := Self;
  if Count > 0 then begin
    SetError('To many expressions'); // TODO comma
    APart.Free;
    exit;
  end;

  Result := APart;
  Add(APart);
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

function TFpPascalExpressionPartBracketArgumentList.DoGetResultValue: TDbgSymbolValue;
var
  tmp, tmp2: TDbgSymbolValue;
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
  if Count <> 1 then begin // Todo a,b,c
    SetError(APart, 'Too many operands in () '+GetText+': ');
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

function TFpPascalExpressionPartBracketSubExpression.DoGetResultValue: TDbgSymbolValue;
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

function TFpPascalExpressionPartIdentifer.DoGetResultValue: TDbgSymbolValue;
var
  DbgSymbol: TDbgSymbol;
begin
  Result := nil;
  DbgSymbol := FExpression.GetDbgSymbolForIdentifier(GetText);
  if DbgSymbol = nil then
    exit;

  Result := DbgSymbol.Value;
  if Result = nil then begin
    Result := TPasParserSymbolValueWrapper.Create(DbgSymbol);
    {$IFDEF WITH_REFCOUNT_DEBUG}Result.DbgRenameReference(nil, 'DoGetResultValue'){$ENDIF};
  end
  else
    Result.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(nil, 'DoGetResultValue'){$ENDIF};

  DbgSymbol.ReleaseReference; // hold via value
end;

{ TFpPascalExpressionPartConstantNumber }

function TFpPascalExpressionPartConstantNumber.DoGetResultValue: TDbgSymbolValue;
begin
  Result := TPasParserSymbolValueConstNumber.Create(StrToQWordDef(GetText, 0));
  {$IFDEF WITH_REFCOUNT_DEBUG}Result.DbgRenameReference(nil, 'DoGetResultValue'){$ENDIF};
end;

{ TFpPascalExpressionPartOperatorUnaryPlusMinus }

procedure TFpPascalExpressionPartOperatorUnaryPlusMinus.Init;
begin
  FPrecedence := PRECEDENCE_UNARY_SIGN;
  inherited Init;
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
      //',': ;
      '''', '#': AddConstChar;
      '0'..'9',
      '$', '%', '&':  AddConstNumber;
      'a'..'z',
      'A'..'Z', '_': AddIdentifier;
      else begin
          SetError(Format('Unexpected char ''%0:s'' at pos %1:d', [CurPtr^, PosFromPChar(CurPtr)])); // error
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



  if CurPart <> nil then begin
    CurPart.HandleEndOfExpression;
    CurPart := CurPart.TopParent;
  end
  else
  if Valid then
    SetError('No Expression');

  FExpressionPart := CurPart;
end;

function TFpPascalExpression.GetResultValue: TDbgSymbolValue;
begin
  if (FExpressionPart = nil) or (not Valid) then
    Result := nil
  else
    Result := FExpressionPart.ResultValue;
end;

procedure TFpPascalExpression.SetError(AMsg: String);
begin
  FValid := False;
  FError := AMsg;
DebugLn(['PARSER ERROR ', AMsg]);
end;

function TFpPascalExpression.PosFromPChar(APChar: PChar): Integer;
begin
  Result := APChar - @FTextExpression[1] + 1;
end;

function TFpPascalExpression.GetDbgSymbolForIdentifier(AnIdent: String): TDbgSymbol;
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
            'Valid: ' + dbgs(FValid) + '   Error: "' + FError + '"'+ LineEnding
            ;
  if FExpressionPart <> nil then
    Result := Result + FExpressionPart.DebugDump('  ', AWithResults);
  if AWithResults and (ResultValue <> nil) then
    if (ResultValue is TPasParserSymbolValue) then
      Result := Result + 'ResultValue = ' + LineEnding + TPasParserSymbolValue(ResultValue).DebugText('  ')
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

function TFpPascalExpressionPart.GetResultValue: TDbgSymbolValue;
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

procedure TFpPascalExpressionPart.Init;
begin
  //
end;

function TFpPascalExpressionPart.DoGetIsTypeCast: Boolean;
begin
  Result := False;
end;

function TFpPascalExpressionPart.DoGetResultValue: TDbgSymbolValue;
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

function TFpPascalExpressionPart.DebugText(AIndent: String; AWithResults: Boolean): String;
begin
  Result := Format('%s%s at %d: "%s"',
                   [AIndent, ClassName, FExpression.PosFromPChar(FStartChar), GetText])
                   + LineEnding;
end;

function TFpPascalExpressionPart.DebugDump(AIndent: String; AWithResults: Boolean): String;
begin
  Result := DebugText(AIndent, AWithResults);
  if AWithResults and (ResultValue <> nil) then
    if (ResultValue is TPasParserSymbolValue) then
      Result := Result + TPasParserSymbolValue(ResultValue).DebugText(AIndent+'    //  ')
    else
      Result := Result + AIndent+'    //  ResultValue = ' + DbgSName(ResultValue) + LineEnding;
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

procedure TFpPascalExpressionPartBracket.Init;
begin
  inherited Init;
  FIsClosed := False;
  FIsClosing := False;
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

procedure TFpPascalExpressionPartBracket.CloseBracket;
begin
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

function TFpPascalExpressionPartOperatorAddressOf.DoGetResultValue: TDbgSymbolValue;
var
  tmp: TDbgSymbolValue;
begin
  Result := nil;
  if Count <> 1 then exit;

  tmp := Items[0].ResultValue;
  if (tmp = nil) or (tmp.Address = 0) then
    exit;

  Result := TPasParserSymbolValueAddressOf.Create(tmp);
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

function TFpPascalExpressionPartOperatorMakeRef.DoGetResultValue: TDbgSymbolValue;
var
  tmp: TDbgSymbolValue;
begin
  Result := nil;
  if Count <> 1 then exit;

  tmp := Items[0].ResultValue;
  if tmp = nil then
    exit;
  if tmp is TPasParserSymbolValueMakeReftype then begin
    TPasParserSymbolValueMakeReftype(tmp).IncRefLevel;
    Result := tmp;
    Result.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(nil, 'DoGetResultValue');{$ENDIF}
    exit;
  end;

  if (tmp.DbgSymbol = nil) or (tmp.DbgSymbol.SymbolType <> stType) then
    exit;

  Result := TPasParserSymbolValueMakeReftype.Create(tmp.DbgSymbol);
  {$IFDEF WITH_REFCOUNT_DEBUG}Result.DbgRenameReference(nil, 'DoGetResultValue');{$ENDIF}
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

function TFpPascalExpressionPartOperatorDeRef.DoGetResultValue: TDbgSymbolValue;
var
  tmp, tmp2: TDbgSymbolValue;
begin
  Result := nil;
  if Count <> 1 then exit;

  tmp := Items[0].ResultValue;
  if tmp = nil then
    exit;

  if tmp is TPasParserSymbolValueAddressOf then begin
    Result := TPasParserSymbolValueAddressOf(tmp).PointedToValue;
    Result.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(nil, 'DoGetResultValue'){$ENDIF};
  end
  else
  if tmp.Kind = skPointer then begin
    if (svfDataAddress in tmp.FieldFlags) and (tmp.DataAddress <> 0) and
       (tmp.TypeInfo <> nil) //and (tmp.TypeInfo.TypeInfo <> nil)
    then begin
      //TODO: maybe introduce a method TypeCastFromAddress, so we can skip the twp2 object
//todo, if tmp2 is a TPasParserAddressOfSymbolValue, then no new object is neede....
      tmp2 := TPasParserSymbolValueDerefPointer.Create(tmp, Expression);
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

{ TFpPascalExpressionPartOperatorPlusMinus }

procedure TFpPascalExpressionPartOperatorPlusMinus.Init;
begin
  FPrecedence := PRECEDENCE_PLUS_MINUS;
  inherited Init;
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

function TFpPascalExpressionPartOperatorMemberOf.DoGetResultValue: TDbgSymbolValue;
var
  tmp: TDbgSymbolValue;
begin
  Result := nil;
  if Count <> 2 then exit;

  tmp := Items[0].ResultValue;
  if (tmp = nil) then exit;
  // Todo unit
  if (tmp.Kind in [skClass, skRecord, skObject]) then begin
    Result := tmp.MemberByName[Items[1].GetText];
    if Result <> nil then
      Result.AddReference{$IFDEF WITH_REFCOUNT_DEBUG}(nil, 'DoGetResultValue'){$ENDIF};
    Assert((Result=nil) or (Result.DbgSymbol=nil)or(Result.DbgSymbol.SymbolType=stValue), 'member is value');
  end;
end;

end.

