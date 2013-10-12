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
  Classes, sysutils, math, FpDbgDwarf, FpDbgClasses, LazLoggerBase, LazClasses;

type

  TFpPascalExpressionPart = class;
  TFpPascalExpressionPartContainer = class;
  TFpPascalExpressionPartBracket = class;
  TFpPascalExpressionPartOperator = class;

  TFpPascalExpressionPartClass = class of TFpPascalExpressionPart;
  TFpPascalExpressionPartBracketClass = class of TFpPascalExpressionPartBracket;

  TFpPasExprTypeKind = (
    ptkUnknown, ptkInvalid,
    ptkValueDbgType,               // a Value (variable, function) of the type in DbgType
    ptkTypeDbgType,                // the type (for type-cast) specified by DbgType
    ptkPointerToValueDbgType,      // an address pointing to a value of DbgType
    ptkPointerOfTypeDbgType,       // ^ TType: pointertype of the type  (for type-cast) specified by DbgType
    ptkNumber                      // a number (constant or expression result)
    // ...
  );

  TFpPasExprType = record
    DbgType: TDbgDwarfTypeIdentifier; //TDbgSymbol
    Kind: TFpPasExprTypeKind;
  end;

  { TFpPascalExpression }

  TFpPascalExpression = class
  private
    FError: String;
    FTextExpression: String;
    FExpressionPart: TFpPascalExpressionPart;
    FValid: Boolean;
    FResultType: TFpPasExprType;
    function GetResultType: TFpPasExprType;
    procedure Parse;
    procedure SetError(AMsg: String);
    function PosFromPChar(APChar: PChar): Integer;
  protected
    function GetDbgTyeForIdentifier(AnIdent: String): TDbgSymbol; virtual;
    property ExpressionPart: TFpPascalExpressionPart read FExpressionPart;
  public
    constructor Create(ATextExpression: String);
    destructor Destroy; override;
    function DebugDump: String;
    property Error: String read FError;
    property Valid: Boolean read FValid;
    property ResultType: TFpPasExprType read GetResultType;
  end;


  { TFpPascalExpressionPart }

  TFpPascalExpressionPart = class
  private
    FEndChar: PChar;
    FParent: TFpPascalExpressionPartContainer;
    FStartChar: PChar;
    FExpression: TFpPascalExpression;
    FResultType: TFpPasExprType;
    function GetSurroundingBracket: TFpPascalExpressionPartBracket;
    function GetResultType: TFpPasExprType;
    function GetTopParent: TFpPascalExpressionPart;
    procedure SetEndChar(AValue: PChar);
    procedure SetParent(AValue: TFpPascalExpressionPartContainer);
    procedure SetStartChar(AValue: PChar);
    procedure SetError(AMsg: String = '');
    procedure SetError(APart: TFpPascalExpressionPart; AMsg: String = '');
  protected
    function DebugText(AIndent: String): String; virtual; // Self desc only
    function DebugDump(AIndent: String): String; virtual;
  protected
    procedure Init; virtual;
    procedure DoGetResultType(var AResultType: TFpPasExprType); virtual;
    procedure InitResultTypeFromDbgInfo(var AResultType: TFpPasExprType; ADbgInfo: TDbgSymbol);

    Procedure ReplaceInParent(AReplacement: TFpPascalExpressionPart);
    procedure DoHandleEndOfExpression; virtual;

    function IsValidNextPart(APart: TFpPascalExpressionPart): Boolean; virtual;
    function IsValidAfterPart(APrevPart: TFpPascalExpressionPart): Boolean; virtual;
    function MaybeHandlePrevPart(APrevPart: TFpPascalExpressionPart;
                                 var AResult: TFpPascalExpressionPart): Boolean; virtual;
    function FindLeftSideOperandByPrecedence(AnOperator: TFpPascalExpressionPartOperator): TFpPascalExpressionPart; virtual;
    function CanHaveOperatorAsNext: Boolean; virtual; // True
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
    property SurroundingBracket: TFpPascalExpressionPartBracket read GetSurroundingBracket; // incl self
    property ResultType: TFpPasExprType read GetResultType;
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
    function DebugDump(AIndent: String): String; override;
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
  private
    FDbgType: TDbgSymbol; // may be a variable or function or a type ...
  protected
    procedure DoGetResultType(var AResultType: TFpPasExprType); override;
  public
    destructor Destroy; override;
  end;

  { TFpPascalExpressionPartBracket }

  TFpPascalExpressionPartBracket = class(TFpPascalExpressionPartContainer)
  private
    FIsClosed: boolean;
    FIsClosing: boolean;
  protected
    procedure Init; override;
    procedure DoHandleEndOfExpression; override;
    function CanHaveOperatorAsNext: Boolean; override;
  public
    procedure CloseBracket;
    function HandleNextPart(APart: TFpPascalExpressionPart): TFpPascalExpressionPart; override;
    procedure HandleEndOfExpression; override;
    property IsClosed: boolean read FIsClosed;
  end;

  { TFpPascalExpressionPartRoundBracket }

  TFpPascalExpressionPartRoundBracket = class(TFpPascalExpressionPartBracket)
  protected
    procedure DoGetResultType(var AResultType: TFpPasExprType); override;
  end;

  { TFpPascalExpressionPartOperator }

  TFpPascalExpressionPartOperator = class(TFpPascalExpressionPartContainer)
  private
    FPrecedence: Integer;
  protected
    function DebugText(AIndent: String): String; override;
    function CanHaveOperatorAsNext: Boolean; override;
    function FindLeftSideOperandByPrecedence(AnOperator: TFpPascalExpressionPartOperator): TFpPascalExpressionPart; override;
    function HasAllOperands: Boolean; virtual; abstract;
    function MaybeAddLeftOperand(APrevPart: TFpPascalExpressionPart;
      var AResult: TFpPascalExpressionPart): Boolean;
    procedure DoHandleEndOfExpression; override;
  public
    function HandleNextPart(APart: TFpPascalExpressionPart): TFpPascalExpressionPart; override;
    property Precedence: Integer read FPrecedence;
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
    procedure DoGetResultType(var AResultType: TFpPasExprType); override;
  end;

  { TFpPascalExpressionPartOperatorMakeRef }

  TFpPascalExpressionPartOperatorMakeRef = class(TFpPascalExpressionPartUnaryOperator)  // ^TTYpe
  protected
    procedure Init; override;
    function IsValidNextPart(APart: TFpPascalExpressionPart): Boolean; override;
    procedure DoGetResultType(var AResultType: TFpPasExprType); override;
  end;

  { TFpPascalExpressionPartOperatorDeRef }

  TFpPascalExpressionPartOperatorDeRef = class(TFpPascalExpressionPartUnaryOperator)  // ptrval^
  protected
    procedure Init; override;
    procedure DoGetResultType(var AResultType: TFpPasExprType); override;
    function MaybeHandlePrevPart(APrevPart: TFpPascalExpressionPart;
      var AResult: TFpPascalExpressionPart): Boolean; override;
    function FindLeftSideOperandByPrecedence(AnOperator: TFpPascalExpressionPartOperator): TFpPascalExpressionPart;
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
    procedure DoGetResultType(var AResultType: TFpPasExprType); override;
  end;

implementation

{ TFpPascalExpressionPartRoundBracket }

procedure TFpPascalExpressionPartRoundBracket.DoGetResultType(var AResultType: TFpPasExprType);
begin
  if Count <> 1 then
    AResultType.Kind := ptkInvalid
  else
    AResultType := Items[0].ResultType;
  if AResultType.DbgType <> nil then
    AResultType.DbgType.AddReference;
end;

{ TFpPascalExpressionPartIdentifer }

procedure TFpPascalExpressionPartIdentifer.DoGetResultType(var AResultType: TFpPasExprType);
begin
  FDbgType := FExpression.GetDbgTyeForIdentifier(GetText);
  InitResultTypeFromDbgInfo(AResultType, FDbgType);
end;

destructor TFpPascalExpressionPartIdentifer.Destroy;
begin
  inherited Destroy;
  ReleaseRefAndNil(FDbgType);
end;

{ TFpPascalExpressionPartOperatorUnaryPlusMinus }

procedure TFpPascalExpressionPartOperatorUnaryPlusMinus.Init;
begin
  FPrecedence := 1;
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

  procedure AddConstChar;
  begin
  end;

  procedure AddConstNumber;
  begin
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

  procedure CloseBracket(ABracketClass: TFpPascalExpressionPartBracketClass);
  begin
    NewPart := CurPart.SurroundingBracket;
    if NewPart = nil then begin
      SetError('Closing bracket found without opening')
    end
    else
    if not (NewPart is ABracketClass) then begin
      SetError('Mismatch bracket')
    end
    else begin
      TFpPascalExpressionPartBracket(NewPart).CloseBracket;
      CurPart := nil;
    end;
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
      '^':       AddRefOperator;
      '.':       HandleDot;
      '+', '-' : AddPlusMinus;
      '*', '/' : AddPart(TFpPascalExpressionPartOperatorMulDiv);
      '(':       AddPart(TFpPascalExpressionPartRoundBracket);
      ')':       CloseBracket(TFpPascalExpressionPartRoundBracket);
      //'[': ;
      //'''':     AddConstChar;
      //'0'..'9',
      //'$', '%': AddConstNumber;
      'a'..'z',
      'A'..'Z', '_': AddIdentifier;
      else begin
          SetError(Format('Unexpected char ''%0:s'' at pos %1:s', [CurPtr^, PosFromPChar(CurPtr)])); // error
          break;
        end;
    end;
    if not FValid then
      break;
    assert(NewPart <> nil);

    if CurPart = nil
    then CurPart := NewPart
    else CurPart := CurPart.HandleNextPart(NewPart);

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

function TFpPascalExpression.GetResultType: TFpPasExprType;
begin
  if (FExpressionPart = nil) or (not Valid) then
    FResultType.Kind := ptkInvalid;
  if FResultType.Kind = ptkUnknown then
    FResultType := FExpressionPart.GetResultType;
  Result := FResultType;
end;

procedure TFpPascalExpression.SetError(AMsg: String);
begin
  FValid := False;
  FError := AMsg;
end;

function TFpPascalExpression.PosFromPChar(APChar: PChar): Integer;
begin
  Result := APChar - @FTextExpression[1] + 1;
end;

function TFpPascalExpression.GetDbgTyeForIdentifier(AnIdent: String): TDbgSymbol;
begin
  Result := nil;
end;

constructor TFpPascalExpression.Create(ATextExpression: String);
begin
  FTextExpression := ATextExpression;
  FValid := True;
  FResultType.Kind := ptkUnknown;
  Parse;
end;

destructor TFpPascalExpression.Destroy;
begin
  FreeAndNil(FExpressionPart);
  inherited Destroy;
end;

function TFpPascalExpression.DebugDump: String;
begin
  Result := 'TFpPascalExpression: ' + FTextExpression + LineEnding +
            'Valid: ' + dbgs(FValid) + '   Error: "' + FError + '"'+ LineEnding
            ;
  if FExpressionPart <> nil then
    Result := Result + FExpressionPart.DebugDump('  ');
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

function TFpPascalExpressionPart.GetSurroundingBracket: TFpPascalExpressionPartBracket;
var
  tmp: TFpPascalExpressionPart;
begin
  Result := nil;
  tmp := Self;
  while (tmp <> nil) and not(tmp is TFpPascalExpressionPartBracket) do
    tmp := tmp.Parent;
  if tmp <> nil then
    Result := TFpPascalExpressionPartBracket(tmp);
end;

function TFpPascalExpressionPart.GetResultType: TFpPasExprType;
begin
  if FResultType.Kind = ptkUnknown then
    DoGetResultType(FResultType);
  Result := FResultType;
end;

procedure TFpPascalExpressionPart.SetParent(AValue: TFpPascalExpressionPartContainer);
var
  Old: TFpPascalExpressionPart;
begin
  if FParent = AValue then Exit;
  Old := FParent;
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

procedure TFpPascalExpressionPart.DoGetResultType(var AResultType: TFpPasExprType);
begin
  FResultType.Kind := ptkInvalid;
end;

procedure TFpPascalExpressionPart.InitResultTypeFromDbgInfo(var AResultType: TFpPasExprType;
  ADbgInfo: TDbgSymbol);
begin
  AResultType.Kind := ptkInvalid;
  if (ADbgInfo = nil) then
    exit;

  if (ADbgInfo is TDbgDwarfTypeIdentifier) then begin
    AResultType.DbgType := TDbgDwarfTypeIdentifier(ADbgInfo);
    AResultType.DbgType.AddReference;
    AResultType.Kind := ptkTypeDbgType;
    exit;
  end;

  if ADbgInfo is TDbgDwarfValueIdentifier then begin
    AResultType.DbgType := TDbgDwarfValueIdentifier(ADbgInfo).TypeInfo;
    AResultType.DbgType.AddReference;
    if AResultType.DbgType <> nil then
      AResultType.Kind := ptkValueDbgType;
    exit;
  end;

  debugln(['TFpPascalExpressionPartIdentifer.DoGetResultType UNKNOWN: ', DbgSName(ADbgInfo)]);

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

function TFpPascalExpressionPart.FindLeftSideOperandByPrecedence(AnOperator: TFpPascalExpressionPartOperator): TFpPascalExpressionPart;
begin
  Result := Self;
end;

function TFpPascalExpressionPart.CanHaveOperatorAsNext: Boolean;
begin
  Result := True;
end;

function TFpPascalExpressionPart.DebugText(AIndent: String): String;
begin
  Result := Format('%s%s at %d: "%s"',
                   [AIndent, ClassName, FExpression.PosFromPChar(FStartChar), GetText])
                   + LineEnding;
end;

function TFpPascalExpressionPart.DebugDump(AIndent: String): String;
begin
  Result := DebugText(AIndent);
end;

constructor TFpPascalExpressionPart.Create(AExpression: TFpPascalExpression; AStartChar: PChar;
  AnEndChar: PChar);
begin
  FExpression := AExpression;
  FStartChar := AStartChar;
  FEndChar := AnEndChar;
  FResultType.Kind := ptkUnknown;
  Init;
end;

destructor TFpPascalExpressionPart.Destroy;
begin
  inherited Destroy;
  ReleaseRefAndNil(FResultType.DbgType);
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

function TFpPascalExpressionPartContainer.DebugDump(AIndent: String): String;
var
  i: Integer;
begin
  Result := inherited DebugDump(AIndent);
  for i := 0 to Count - 1 do
    Result := Result + Items[i].DebugDump(AIndent+'  ');
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

  Result := Self;
  if Count > 0 then begin
    SetError('To many expressions');
    exit;
  end;

  Result := APart;
  Add(APart);
end;

procedure TFpPascalExpressionPartBracket.HandleEndOfExpression;
begin
  if not FIsClosing then
    inherited HandleEndOfExpression;
end;

{ TFpPascalExpressionPartOperator }

function TFpPascalExpressionPartOperator.DebugText(AIndent: String): String;
begin
  Result := inherited DebugText(AIndent);
  while Result[Length(Result)] in [#10, #13] do SetLength(Result, Length(Result)-1);
  Result := Result + ' Precedence:' + dbgs(FPrecedence) +
    LineEnding;
end;

function TFpPascalExpressionPartOperator.CanHaveOperatorAsNext: Boolean;
begin
  Result := HasAllOperands and LastItem.CanHaveOperatorAsNext;
end;

function TFpPascalExpressionPartOperator.FindLeftSideOperandByPrecedence(AnOperator: TFpPascalExpressionPartOperator): TFpPascalExpressionPart;
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
  if (APrevPart.Parent <> nil) and (APrevPart.Parent is TFpPascalExpressionPartOperator) then
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
  FPrecedence := 1; // highest
  inherited Init;
end;

procedure TFpPascalExpressionPartOperatorAddressOf.DoGetResultType(var AResultType: TFpPasExprType);
begin
  AResultType.Kind := ptkInvalid;
  if Count <> 1 then exit;
  AResultType := Items[0].ResultType;
  if AResultType.Kind = ptkValueDbgType then
    AResultType.Kind := ptkPointerToValueDbgType
  else
    AResultType.Kind := ptkInvalid; // can not take address of...

  if FResultType.DbgType <> nil then
    FResultType.DbgType.AddReference;
end;

{ TFpPascalExpressionPartOperatorMakeRef }

procedure TFpPascalExpressionPartOperatorMakeRef.Init;
begin
  FPrecedence := 1;
  inherited Init;
end;

function TFpPascalExpressionPartOperatorMakeRef.IsValidNextPart(APart: TFpPascalExpressionPart): Boolean;
begin
  Result := (inherited IsValidNextPart(APart)) and
            (APart is TFpPascalExpressionPartIdentifer);
end;

procedure TFpPascalExpressionPartOperatorMakeRef.DoGetResultType(var AResultType: TFpPasExprType);
begin
  AResultType.Kind := ptkInvalid;
  if Count <> 1 then exit;
  AResultType := Items[0].ResultType;
  if AResultType.Kind = ptkTypeDbgType then
    AResultType.Kind := ptkPointerOfTypeDbgType
  else
    AResultType.Kind := ptkInvalid; // can not take address of...

  if FResultType.DbgType <> nil then
    FResultType.DbgType.AddReference;
end;

{ TFpPascalExpressionPartOperatorDeRef }

procedure TFpPascalExpressionPartOperatorDeRef.Init;
begin
  FPrecedence := 1;
  inherited Init;
end;

procedure TFpPascalExpressionPartOperatorDeRef.DoGetResultType(var AResultType: TFpPasExprType);
begin
  AResultType.Kind := ptkInvalid;
  if Count <> 1 then exit;

  AResultType := Items[0].ResultType;
  case AResultType.Kind of
    ptkValueDbgType: begin
      AResultType.Kind := ptkInvalid;
      if AResultType.DbgType.IsPointerType then begin
        AResultType.DbgType := AResultType.DbgType.PointedToType;
        if AResultType.DbgType <> nil then
          AResultType.Kind := ptkPointerToValueDbgType;
      end;
    end;
    ptkPointerToValueDbgType: AResultType.Kind := ptkValueDbgType;
    else
      AResultType.Kind := ptkInvalid;
  end;

  if FResultType.DbgType <> nil then
    FResultType.DbgType.AddReference;
end;

function TFpPascalExpressionPartOperatorDeRef.MaybeHandlePrevPart(APrevPart: TFpPascalExpressionPart;
  var AResult: TFpPascalExpressionPart): Boolean;
begin
  Result := MaybeAddLeftOperand(APrevPart, AResult);
end;

function TFpPascalExpressionPartOperatorDeRef.FindLeftSideOperandByPrecedence(AnOperator: TFpPascalExpressionPartOperator): TFpPascalExpressionPart;
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
  FPrecedence := 3;
  inherited Init;
end;

{ TFpPascalExpressionPartOperatorMulDiv }

procedure TFpPascalExpressionPartOperatorMulDiv.Init;
begin
  FPrecedence := 2;
  inherited Init;
end;

{ TFpPascalExpressionPartOperatorMemberOf }

procedure TFpPascalExpressionPartOperatorMemberOf.Init;
begin
  FPrecedence := 0;
  inherited Init;
end;

function TFpPascalExpressionPartOperatorMemberOf.IsValidNextPart(APart: TFpPascalExpressionPart): Boolean;
begin
  Result := (inherited IsValidNextPart(APart)) and
            (APart is TFpPascalExpressionPartIdentifer);
end;

procedure TFpPascalExpressionPartOperatorMemberOf.DoGetResultType(var AResultType: TFpPasExprType);
var
  tmp: TFpPasExprType;
  struct: TDbgDwarfIdentifierStructure;
  member: TDbgDwarfIdentifierMember;
begin
  AResultType.Kind := ptkInvalid;
  if Count <> 2 then exit;

  tmp := Items[0].ResultType;
  // Todo unit
  if (tmp.Kind = ptkValueDbgType) and (tmp.DbgType.IsStructType) then begin
    struct := tmp.DbgType.StructTypeInfo;
    member := struct.MemberByName[Items[1].GetText];
    InitResultTypeFromDbgInfo(AResultType, member);
    ReleaseRefAndNil(member);
  end;
end;

end.

