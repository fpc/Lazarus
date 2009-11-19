{ * This file is part of ObjCParser tool
  * Copyright (C) 2008-2009 by Dmitry Boyarintsev under the GNU LGPL
  * license version 2.0 or 2.1.  You should have received a copy of the
  * LGPL license along with at http://www.gnu.org/                                              
}


unit ObjCParserTypes;

interface

{$ifdef fpc}{$mode delphi}{$h+}
{$else}
{$warn unsafe_code off}
{$warn unsafe_type off}
{$warn unsafe_cast off}
{$endif}

uses
  Classes, SysUtils;

const
  Err_Ident   = 'Identifier';
  Err_Expect  = '%s, excepted, but "%s" found';
  Err_BadPrecompile   = 'Bad precompile directive';

type
  TTokenType = (tt_Ident, tt_Symbol, tt_Numeric, tt_String);


  TCharSet = set of Char;

  TTokenPair = record
    Open       : AnsiString;
    Close      : AnsiString;
  end;

  TTokenTable = class(TObject)
    SpaceChars  : TCharSet;
    CmtBlock    : array of TTokenPair;
    CmtCount    : Integer;
    CmtLine     : TStrings;
    StringStart : TCharSet;
    Symbols     : TCharSet;
    Precompile  : AnsiString;
    MultiLine   : AnsiChar; 
    constructor Create;
    destructor Destroy; override;
  end;

  TPrecompilerEvent = procedure (Sender: TObject; PrecompEntity: TObject) of object;

  TTextParser = class;

  TMacroHandler = class(TObject)
  public
    function ParseMacro(const Parser: TTextParser; var MacroStr, ReplaceStr: AnsiString): Boolean; virtual; abstract;
    function MacroDefined(const Macro: AnsisTring): Boolean; virtual; abstract;
  end;

  TCMacroStruct = class(TObject)
    MacroName   : AnsiString;
    MacroParams : TStringList;
    ReplaceText : AnsiString;

    constructor Create;
    destructor Destroy; override;
  end;

  TCMacroHandler = class(TMacroHandler)
  public
    MacrosNames : TStringList;
    constructor Create;
    destructor Destroy; override;
    function ParseMacro(const Parser: TTextParser; var MacroStr, ReplaceStr: AnsiString): Boolean; override;
    function MacroDefined(const Macro: AnsisTring): Boolean; override;

    procedure AddSimpleMacro(const MacroStr, ReplaceStr: AnsiString);

    procedure Clear;
  end;


  { TTextParser }

  TTextParser = class(TObject)
  protected
    ProcessingMacro : Boolean;
    function HandlePrecomiler: Boolean; virtual;
    function HandleMacro(var MacroStr: AnsiString; var ReplaceStr: AnsiString): Boolean;

    function IsMultiLine: Boolean;
    procedure SkipSingleEoLnChars;

    function AddChildToStackEntity(ent: TObject): Boolean;
  public
    Buf           : AnsiString;
    Index         : Integer;      // current index where text parsing goes on
    TokenPos      : Integer;      // position of currently found token by (FindTextToken)
    TokenTable    : TTokenTable;
    OnPrecompile  : TPrecompilerEvent;
    OnComment     : procedure (Sender: TObject; const Comment: AnsiString) of object;
    OnIgnoreToken : procedure (Sender: TObject; const Ignored: AnsiString) of object;
    Line          : Integer;

    Stack         : TList;
    Errors        : TStringList;
    //IgnoreTokens  : TStringList;
    MacroHandler  : TMacroHandler;

    UseCommentEntities     : Boolean;
    UsePrecompileEntities  : Boolean;

    Comments      : TList;

    constructor Create;
    destructor Destroy; override;

    procedure BeginParse(AObject: TObject);
    procedure EndParse;

    function GetBufWideStr(const Cmd: AnsiString): WideString;

    function SkipComments: Boolean;

    function FindNextToken(var Token: AnsiString; var TokenType: TTokenType): Boolean;

    procedure SetError(const ErrorCmt: AnsiString);
  end;

  TCallingConv = (ccRegister {FastCall}, ccCdecl, ccStdcall, ccSafecall, ccMwPascal);

  { TEntity }

  TEntity = class(TObject)
  protected
    function DoParse(AParser: TTextParser): Boolean; virtual; abstract;

  public
    Owner       : TEntity;
    Items       : TList;

    TagComment  : AnsiString;
    
    constructor Create(AOwner: TEntity); virtual;
    destructor Destroy; override;
    function Parse(AParser: TTextParser): Boolean; virtual;
    procedure Assign(AEntity: TEntity); virtual;
  end;
  TEntityClass = class of TEntity;

  TCPrepocessor = class(TEntity);

  TCPrepDefine = class(TCPrepocessor)
  protected
    function DoParse(AParser: TTextParser): Boolean; override;
  public
    Params    : TStringList;
    _Name     : AnsiString;
    SubsText  : AnsiString;
  end;

  TCPrepInclude = class(TCPrepocessor)
  protected
    Params    : TStringList;
    Included  : AnsiString;
    function DoParse(AParser: TTextParser): Boolean; override;
  end;

  TCPrepElse = class(TCPrepocessor)
    function DoParse(AParser: TTextParser): Boolean; override;
  end;

  TCPrepEndif = class(TCPrepocessor)
    function DoParse(AParser: TTextParser): Boolean; override;
  end;

  TCPrepIf = class(TCPrepocessor)
    _Cond : AnsiString;
    _isElIf : Boolean;
    function DoParse(AParser: TTextParser): Boolean; override;
  end;

  TCPrepElIf = TCPrepIf;

  TCPrepPragma = class(TCPrepocessor)
    _Text : AnsiString;
    function DoParse(AParser: TTextParser): Boolean; override;
  end;


  //C tokens: /*, //

  { TComment }

  TComment = class(TEntity)
  protected
    function DoParse(AParser: TTextParser): Boolean; override;
  public
     _Comment : WideString; // in case sources are UTF8 or Unicode
  end;

  { TSkip }

  TSkip = class(TEntity)
  protected
    function DoParse(AParser: TTextParser): Boolean; override;
  public
    _Skip   : AnsiString;
  end;

  { TPrecompiler }

  //C token: #
  TPrecompiler = class(TEntity)
  {updated}
  protected
    function DoParse(AParser: TTextParser): Boolean; override;
  public
    _Directive : AnsiString;
    _Params    : AnsiString;
  end;

  { TVariable }

  TVariable = class(TEntity) 
  protected
    function DoParse(AParser: TTextParser): Boolean; override; 
    function ParseAfterTypeName(AParser: TTextParser): Boolean; 
  public
    _Type     : TEntity;
    _Name     : AnsiString;
    _isConst  : Boolean;
    _isExtern : Boolean;
  end; 

  { TFunctionParam }
  TFunctionParam = class(TEntity)
  protected
    function DoParse(AParser: TTextParser): Boolean; override;
  public
    _Type     : TEntity;
    _Name     : AnsiString;
    _IsAny    : Boolean;
    _IsArray  : Boolean;
  end;

  { TFunctionParamsList }

  TFunctionParamsList = class(TEntity)
  protected
    function DoParse(AParser: TTextParser): Boolean; override;
  end;

  TFunctionTypeDef = class(TEntity) // Parses only Parameters list (starting with Bracket "(")
  protected
    function DoParse(APArser: TTextParser): Boolean; override;
  public
    _ResultType   : TEntity;
    _ParamsList   : TFunctionParamsList;
    
    _isPointer    : Boolean;
    _isPointerRef : Boolean;
  end;

  TCCodeSection = class(TEntity)
  protected
    function DoParse(AParser: TTextParser): Boolean; override;
  public
    _RawText  : AnsiSTring;
  end;

  TFunctionBody = class(TCCodeSection);

  { TFunctionDef }

  TFunctionDef = class(TEntity)
  protected
    function DoParse(APArser: TTextParser): Boolean; override;
    function ParseParams(AParser: TTextParser): Boolean; 
  public    
    _ResultType   : TEntity;
    _ParamsList   : TFunctionParamsList;
    _Name         : AnsiString;
    _isPointer    : Boolean;
    _isPointerRef : Boolean;
    _isExternal   : Boolean;
    _isInLine     : Boolean; 
    _CallConv     : TCallingConv;
    _Body         : TFunctionBody; // can be nil!
  end;


  { TEnumValue }

  TEnumValue = class(TEntity)
  protected
    function DoParse(AParser: TTextParser): Boolean; override;
  public
    _Name     : AnsiString;
    _Value    : AnsiString;
  end;

  { TEnumTypeDef }

  //C token: enum
  {updated}
  TEnumTypeDef = class(TEntity)
  protected
    fValCount  : Integer;
    function GetValue(idx: integer): TEnumValue;
    function DoParse(AParser: TTextParser): Boolean; override;
  public
    _Name     : AnsiString;
    property Value[idx: Integer]: TEnumValue read GetValue;
    property ValuesCount: Integer read fValCount;
  end;

  { TStructField }

  TStructField = class(TEntity)
  {updated}
  protected
    function DoParse(AParser: TTextParser): Boolean; override;
  public
    _Name       : AnsiString;
    _IsArray    : Boolean;
    _ArraySize  : AnsiSTring;
    _BitSize    : Integer;
    _Type       : TEntity;
    _TypeName   : AnsiString;
  end;

  //C token: struct
  TEntityStruct = class(TEntity)
  {update}
  protected
    function DoParse(AParser: TTextParser): Boolean; override;
  public
    _Name         : AnsiString;
    //todo: remove???
    _isPointer    : Boolean;
    _isPointerRef : Boolean;
  end;

  TUnionTypeDef = class(TEntity)
  protected
    function DoParse(AParser: TTextParser): Boolean; override;
  public
    _Name       : AnsiString;
    //todo: remove??/
    _isPointer    : Boolean;
    _isPointerRef : Boolean;
  end;

  { TTypeDef }
  //C token - any type, including unsigned short

  TTypeDefSpecs = set of (td_Unsigned, td_Signed, td_Volitale, td_Const, td_InOut, td_Long, td_Short, td_Char, td_Int);

  {updated}
  TTypeDef = class(TEntity)
  protected
    function DoParse(AParser: TTextParser): Boolean; override;
  public
    _Name     : AnsiString;
    _Spec     : TTypeDefSpecs;
    _IsPointer    : Boolean;
    _IsPointerRef : Boolean;
  end;

  { TTypeNameDef }

  //C token: typdef
  TTypeNameDef = class(TEntity)
  protected
    function DoParse(AParser: TTextParser): Boolean; override;
  public
    _Inherited  : AnsiString;
    _TypeName   : AnsiString;
    _Type       : TEntity;
  end;

  { TObjCParameterDef }

  TObjCResultTypeDef = class(TEntity)
  protected
    function DoParse(AParser: TTextParser): Boolean; override;
  public
    _Type     : TEntity;
    _isRef    : Boolean;
    _isConst  : Boolean; // (const Sometype)
    _Prefix   : AnsiString; // reserved-word  type descriptors
  end;

  TObjCParameterDef = class(TEntity)
  protected
    function DoParse(AParser: TTextParser): Boolean; override;
  public
    _Type : TObjCResultTypeDef;
    _Name : AnsiString;
  end;

  { TParamDescr }

  TParamDescr = class(TEntity)
  protected
    function DoParse(AParser: TTextParser): Boolean; override;
  public
    _Descr   : AnsiString;
  end;

  { TClassMethodDef }

  TClassMethodDef = class(TEntity)
  {update}
  protected
    function DoParse(AParser: TTextParser): Boolean; override;
  public
    _IsClassMethod  : Boolean;  // is class function as delphi would say
    _CallChar       : AnsiChar; // + or -
    _Name           : AnsiString;
    function GetResultType: TObjCResultTypeDef;
  end;

  { TSubSection }

  //todo: implement
  TSubSection = class(TEntity) // for public, protected and private sections
  protected
    function DoParse(AParser: TTextParser): Boolean; override;
  public
    _EntityName : AnsiString;
  end;

  { TClassDef }
  
  { TClassesForward }

  TClassesForward = class(TEntity)
  protected
    function DoParse(AParser: TTextParser): Boolean; override;
  public
    _Classes    : TStringList;
    _isClasses  : Boolean; // classes or protocols
    constructor Create(AOwner: TEntity); override;
    destructor Destroy; override;
  end;

  TObjCPropertyAttributes = set of (pa_readwrite, pa_readonly, pa_assign, pa_retain, pa_copy, pa_nonatomic);

  TObjCClassProperty = class(TEntity)
  protected
    function DoParse(AParser: TTextParser): Boolean; override;
  public
    _Attribs  : TObjCPropertyAttributes;
    _Getter   : AnsiString;
    _Setter   : AnsiString;
    _Type     : TEntity;
    _Name     : AnsiString;
  end;

  TClassDef = class(TEntity)
  protected
    function DoParse(AParser: TTextParser): Boolean; override;
  public
    _ClassName    : AnsiString;
    _SuperClass   : AnsiString;
    _Category     : AnsiString;
    _Protocols    : TStringList;
    constructor Create(AOwner : TEntity); override;
    destructor Destroy; override;
  end;

  TCHeader = class(TEntity); // it's CCodeSection ??

  { TObjCHeader }

  TObjCHeader = class(TCHeader)
  protected
    function DoParse(AParser: TTextParser): Boolean; override;
  public
    _FileName     : AnsiString;
    constructor Create(AOwner: TEntity = nil); override;
  end;

const
  EoLnChars : TCharSet = [#10,#13];
  InvsChars : TCharSet = [#32,#9];
  WhiteSpaceChars : TCharSet = [#10,#13,#32,#9];

// utility functions
function SkipEndOfLineChars(const Src: AnsiString; idx: integer): Integer;

function ParseSeq(Parser: TTextParser; const OpenSeq, CloseSeq: AnsiString): AnsiString;


function IsSubStr(const sbs, s: AnsiString; index: Integer): Boolean;
function SkipLine(const s: AnsiString; var index: Integer): AnsiString;
procedure SetCComments(Table: TTokenTable);
procedure SetCSymbols(var ch: TCharSet);

function LastEntity(ent: TEntity): TEntity;

function ScanWhile(const s: AnsiString; var index: Integer; const ch: TCharSet): AnsiString;
function ScanTo(const s: AnsiString; var index: Integer; const ch: TCharSet): AnsiString;

function ParseTypeDef(Owner: TEntity; AParser: TTextParser): TEntity;
function ParseCVarDef(AParser: TTextParser; var Name: AnsiString; var isArray: Boolean; var ArraySize:AnsiString): Boolean;

function GetTypeNameFromEntity(Entity: TEntity): AnsiString;
function IsTypeDefIsPointer(Entity: TEntity): Boolean;

procedure FreeEntity(Item: TEntity);

function ParseCExpression(AParser: TTextParser; var ExpS: AnsiString): Boolean;
procedure ParseCNumeric(const S: AnsiString; var idx: integer; var NumStr: AnsiSTring);
function ParseCString(const S: AnsiString; var idx: Integer; var CStr: AnsiString): Boolean;

function CToPascalNumeric(const Cnum: AnsiString): AnsiString;

function IsTypePointer(AType: TEntity; DefResult: Boolean ): Boolean;
function ErrExpectStr(const Expected, Found: AnsiString): AnsiString;

function IsTypeOrTypeDef(const Token: AnsiString): Boolean;

function ParseTypeOrTypeDef(AParser: TTextParser; Owner: TEntity; var Ent: TEntity): Boolean;

function IsTypeDefEntity(Ent: TEntity): Boolean;
function isEmptyStruct(AStruct: TEntityStruct): Boolean;

// general functions
function CreateObjCTokenTable: TTokenTable;

// high-level functions
function CreateCParser(const CHeaderText: AnsiString;
  WithCMacroHandler: Boolean = false): TTextParser;

// custom entities


type
  TCustomEntityProc = function (Parent: TEntity; Parser: TTextParser): TEntity;

//todo: Entitiy location
// TEntityLocation = set of [el_Anywher];

procedure RegisterEntity( CheckProc: TCustomEntityProc); {Location: TEntityLocation}
function ParseCustomEntity(Parent: TEntity; Parser: TTextParser): TEntity;

implementation

var
  CustomList : TList = nil;

function SkipEndOfLineChars(const Src: AnsiString; idx: integer): Integer;
begin
  if idx < length(Src) then begin
    if (Src[idx] = #10) and (Src[idx+1]=#13) then inc(idx)
    else if (Src[idx] = #13) and (Src[idx+1]=#10) then inc(idx);
  end;
  Result := idx+1;
end;

function IsCReserved(const Token: AnsiString): Boolean;
begin
  if Token = '' then begin
    Result := false;
    Exit;
  end;
  Result := true;
  case Token[1] of
    'c':
      if Token = 'const' then Exit;
    'e':
      if Token = 'extern' then Exit;
    'i':
      if Token = 'inline' then Exit;
  end;
  
  Result := false;
end;

function ParseSeq(Parser: TTextParser; const OpenSeq, CloseSeq: AnsiString): AnsiString;
var
  i     : integer;
  s     : AnsiString;
  tt    : TTokenType;
  count : integer;
begin
  count := 0;
  i := Parser.Index;
  repeat
    if not Parser.FindNextToken(s, tt) then
      Count := 0
    else if tt = tt_Symbol then begin
      if s = OpenSeq then inc(count)
      else if s = CloseSeq then dec(count);
    end else
  until count = 0;
  Result := Copy(Parser.Buf, i, Parser.Index - i);
end;


function ParseObjCProtocol(AParent: TEntity; Parser: TTextParser): TEntity;
var
  idx : integer;
  s  : string;
  tt : TTokenType;
begin
  Result := nil;
  idx := Parser.TokenPos;
  try
    Parser.FindNextToken(s, tt);
    if s <> '@protocol' then Exit;

    // find out if it's just a forward declaration or not.
    Parser.FindNextToken(s, tt);
    Parser.FindNextToken(s, tt);
    if (tt = tt_Symbol) and ((s = ';') or (s = ',')) then begin // is forward declaration
      Parser.Index := idx;
      Result := TClassesForward.Create(AParent);
    end else begin
      Parser.Index := idx;
      Result := TClassDef.Create(AParent);
    end;
    
  finally
    if not Assigned(Result) then Parser.Index := idx;
  end;
  
end;

function CreateCParser(const CHeaderText: AnsiString; WithCMacroHandler: Boolean): TTextParser;
begin
  Result := TTextParser.Create;
  Result.TokenTable := CreateObjCTokenTable;
  if WithCMacroHandler then
    Result.MacroHandler := TCMacroHandler.Create;
  Result.Buf := CHeaderText;
end;

function IsTypeDefEntity(Ent: TEntity): Boolean;
begin
  Result := (Ent is TTypeDef) or (Ent is TEntityStruct)
    or (Ent is TUnionTypeDef) or (Ent is TTypeNameDef) or (Ent is TEnumTypeDef); 
end;

function IsTypeOrTypeDef(const Token: AnsiString): Boolean;
begin
  Result := false;
  if Token = '' then Exit;
  case Token[1] of
    't': Result := Token = 'typedef';
    'e': Result := Token = 'enum';
    's': Result := Token = 'struct';
    'u': Result := Token = 'union';
  end;
end;

function ParseTypeOrTypeDef(AParser: TTextParser; Owner: TEntity; var Ent: TEntity): Boolean;
var
  s   : AnsiString;
  tt  : TTokenType;
begin
  AParser.FindNextToken(s, tt);
  Result := (tt = tt_Ident) and IsTypeOrTypeDef(s);
  if (not Result) then begin
    AParser.Index := AParser.TokenPos;
    Exit;
  end;

  if s = 'typedef' then begin
    AParser.Index := AParser.TokenPos;
    Ent := TTypeNameDef.Create(Owner);
    Result := Ent.Parse(AParser);
  end else begin
    AParser.Index := AParser.TokenPos;  
    Ent := ParseTypeDef(Owner, AParser);
    Result := Assigned(ent);
    if Result then begin
      AParser.FindNextToken(s, tt);
      Result := (tt=tt_Symbol) and (s = ';');
    end;
  end;
end;

// isPointer returned the * is declared
// isPointerRef return the ** is declared
procedure ParsePointerDef(AParser: TTextParser; var isPointer, isPointerRef: Boolean);
var
  s   : AnsiString;
  tt  : TTokenType;
begin
  isPointer := false;
  isPointerRef := false;
  if not AParser.FindNextToken(s, tt) then Exit;
  isPointer := (tt=tt_Symbol) and (s = '*');
  if isPointer then begin
    if not AParser.FindNextToken(s, tt) then Exit;

    if (tt=tt_Symbol) and (s = '*') then isPointerRef := true
    else AParser.Index := AParser.TokenPos;
  end else
    AParser.Index := AParser.TokenPos;
end;


function ParseCVarDef(AParser: TTextParser; var Name: AnsiString; var isArray: Boolean; var ArraySize:AnsiString): Boolean;
var
  tt    : TTokenType;
  s     : AnsiString;
begin
  Result := AParser.FindNextToken(Name, tt);
  if Result then Result := tt = tt_Ident;
  if not Result then begin
    AParser.SetError(ErrExpectStr('Identifier', Name) );
    Exit;
  end;
  Result := true;

  AParser.FindNextToken(s, tt);
  if not ((tt = tt_Symbol) and (s = '[')) then begin
    AParser.Index := AParser.TokenPos;
    Exit;
  end;

  isArray := true;
  ParseCExpression(APArser, ArraySize);
  AParser.FindNextToken(s, tt);
  if s <> ']' then begin
    Result := false;
    AParser.SetError( ErrExpectStr('[', ArraySize));
    AParser.Index := AParser.TokenPos;
  end;

end;

function IsTypePointer(AType: TEntity; DefResult: Boolean ): Boolean;
begin
  Result := DefResult;
  if not Assigned(AType) then Exit;
  if AType is TTypeDef then
    Result := TTypeDef(AType)._IsPointer
  else if AType is TEntityStruct then
    Result := TEntityStruct(AType)._isPointer;
end;

function ErrExpectStr(const Expected, Found: AnsiString): AnsiString;
begin
  Result := Format(Err_Expect, [Expected, Found]);
end;

procedure FreeEntity(Item: TEntity);
var
  i    : Integer;
begin
  for i := 0 to Item.Items.Count - 1 do
    FreeEntity(TEntity(Item.Items[i]));
  Item.Free;
end;

function GetTypeNameFromEntity(Entity: TEntity): AnsiString;
begin
  Result := '';
  if Assigned(Entity) then begin
    if Entity is TEntityStruct then // hmm... a common ancsessotor should be used?
      Result := TEntityStruct(Entity)._Name
    else if Entity is TEnumTypeDef then
      Result := TEnumTypeDef(Entity)._Name
    else if Entity is TTypeDef then begin
      Result := TTypeDef(Entity)._Name;
    end;
  end;
end;

function IsTypeDefIsPointer(Entity: TEntity): Boolean;
begin
  Result := false;
  if Assigned(Entity) then begin
    if Entity is TEntityStruct then // hmm... a common ancsessotor should be used?
      Result := TEntityStruct(Entity)._isPointer
    else if Entity is TTypeDef then begin
      Result := TTypeDef(Entity)._isPointer;
    end;
  end;
end;


(* ANSI C reserved words
auto  break case char const continue default do double else enum
extern float for goto if int  long register  return short signed
sizeof static struct switch typedef union  unsigned void volatile while
*)

function ParseTypeDef(Owner: TEntity; AParser: TTextParser): TEntity;
var
  s   : AnsiString;
  tt  : TTokenType;
  res : Boolean;
  i   : Integer;
begin
  Result := nil;
  res := AParser.FindNextToken(s, tt);
  if not Res or (tt <> tt_Ident) then Exit;

  i := AParser.TokenPos;
  s := AnsiLowerCase(s);
  if (s = 'const') {or (s = 'volatile')}  then begin
    AParser.FindNextToken(s, tt);
    if s <> 'struct' then begin
      AParser.TokenPos := i;
      AParser.Index := i;
    end;
  end;

  if s = 'enum' then
    Result := TEnumTypeDef.Create(Owner)
  else if s = 'struct' then
    Result := TEntityStruct.Create(Owner)
  else if s = 'union' then
    Result := TUnionTypeDef.Create(Owner)
  else
    Result := TTypeDef.Create(Owner);

  AParser.Index := AParser.TokenPos;
  if Assigned(Result) then
    if not Result.Parse(AParser) then begin
      Result.Free;
      Result := nil;
    end;
end;

function LastEntity(ent: TEntity): TEntity;
var
  i   : integer;
  pre : TEntity;
begin
  pre := nil;
  while Assigned(ent) do begin
    pre := ent;
    i := pre.Items.Count - 1;
    if i >= 0 then ent := TEntity(pre.Items[i])
    else ent := nil;
  end;
  Result := pre;
end;

function CreateObjCTokenTable: TTokenTable;
begin
  Result := TTokenTable.Create;
  SetCComments(Result);
  SetCSymbols(Result.Symbols);
  Result.SpaceChars := EoLnChars + InvsChars;
  Result.Precompile := '#';
  Result.MultiLine := '\';
  Result.StringStart := ['"', #39];
end;

procedure SetCSymbols(var ch: TCharSet);
begin
  ch := ['(',')', '{','}', ':', '-','+','<','>','*',';', ',','|','&','[',']'{, #39 ,'"'} ]
end;

procedure SetCComments(Table: TTokenTable);
begin
  SetLength(Table.CmtBlock, 1);
  Table.CmtCount := 1;
  Table.CmtBlock[0].Open := '/*';
  Table.CmtBlock[0].Close := '*/';
  Table.CmtLine.Add('//');
end;

function ScanWhile(const s: AnsiString; var index: Integer; const ch: TCharSet): AnsiString;
var
  i : Integer;
begin
  Result := '';
  if (index <= 0) or (index > length(s)) then Exit;
  for i := index to length(s) do
    if not (s[i] in ch) then begin
      if i = index then Result := ''
      else Result := Copy(s, index, i - index);
      index := i;
      Exit;
    end;
  Result := Copy(s, index, length(s) - index + 1);
  index := length(s) + 1;
end;

function ScanTo(const s: AnsiString; var index: Integer; const ch: TCharSet): AnsiString;
var
  i : Integer;
begin
  Result := '';
  if (index <= 0) or (index > length(s)) then Exit;
  for i := index to length(s) do
    if (s[i] in ch) then begin
      if i = index then Result := ''
      else Result := Copy(s, index, i - index);
      index := i;
      Exit;
    end;
  Result := Copy(s, index, length(s) - index + 1);
  index := length(s) + 1;
end;

function IsSubStr(const sbs, s: AnsiString; index: Integer): Boolean;
var
  i : Integer;
  j : Integer;
begin
  Result := false;
  if (sbs = '') or (length(sbs) > length(s) - index) then Exit;
  j := index;
  for i := 1 to length(sbs) do begin
    if sbs[i] <> s[j] then Exit;
    inc(j);
  end;
  Result := true;
end;

function SkipCommentBlock(const s: AnsiString; var index: Integer; const closecmt: AnsiString): AnsiString;
begin
  Result := '';
  if closecmt = '' then begin
    index := length(s) + 1;
    Exit;
  end;
  while index <= length(s) do begin
    Result := Result + ScanTo(s, index, [closecmt[1]]);
    if IsSubStr(closecmt, s, index) then begin
      inc(index, length(closecmt));
      Exit;
    end else begin
      Result := Result + s[index];
      inc(index);
    end;
  end;
end;

function SkipLine(const s: AnsiString; var index: Integer): AnsiString;
begin
  Result := ScanTo(s, index, EoLnChars);
  ScanWhile(s, index, EoLnChars); // todo: skip a single line!
end;

{ TTextParser }

constructor TTextParser.Create;
begin
  Index := 1;
  Line := 1;
  Stack := TList.Create;
  Errors := TStringList.Create;
  //IgnoreTokens := TStringList.Create;
  UsePrecompileEntities := true;
  Comments := TList.Create;
end;

destructor TTextParser.Destroy;
begin
  Comments.Free;
  //IgnoreTokens.Free;
  Errors.Free;
  Stack.Free;
  inherited Destroy;
end;

procedure TTextParser.BeginParse(AObject: TObject);
begin
  Stack.Add(AObject);
end;

procedure TTextParser.EndParse;
begin
  if Stack.Count > 0 then Stack.Delete(Stack.Count - 1);
end;

function TTextParser.HandlePrecomiler: Boolean;
var
  idx : Integer;
  s   : AnsiString;
  df  : TCPrepocessor;
  i   : integer;
begin
  Result := false;
  if ProcessingMacro then Exit;

  ProcessingMacro := true;
  try
    idx := Index;
    i := idx;

    s := ScanTo(Buf, i, WhiteSpaceChars);
    if s = '#define' then df := TCPrepDefine.Create(nil)
    else if s = '#include' then df := TCPrepInclude.Create(nil)
    else if s = '#else' then df := TCPrepInclude.Create(nil)
    else if s = '#endif' then
      df := TCPrepEndif.Create(nil)
    else if (s = '#if') or (s = '#elif') or (s = '#ifdef') or (s = '#ifndef') then df := TCPrepIf.Create(nil)
    else if s = '#pragma' then df := TCPrepPragma.Create(nil)
    else df := nil;

    Result := Assigned(df);
    if Result  then begin
      Result := df.Parse(Self);
      if UsePrecompileEntities then AddChildToStackEntity(df);
      if Assigned(OnPrecompile) then
        OnPrecompile(Self, df);
    end;

    if not Result then begin
      SetError('cannot handle preprocessor');
      Exit;
    end;

    //Result := Index <> idx;
  finally
    ProcessingMacro := false;
  end;
end;

function ParseHexNumber(const S:AnsiString; var idx: Integer): AnsiString;
begin
  Result := ScanWhile(s, idx, ['0'..'9', 'A'..'F', 'a'..'f']);
end;

procedure ParseCNumeric(const S: AnsiString; var idx: integer; var NumStr: AnsiSTring);
var
  l : integer;
  i : Integer;
  f : AnsiString;
begin
  l := length(s);
  if (idx <= 0) or (idx > l) then Exit;
  
  if (s[idx] = '0') and (idx < l) and ((s[idx+1] = 'x') or (s[idx+1] = 'X')) then begin
    inc(idx,2);
    NumStr := '0x'+ParseHexNumber(s, idx);
  end else begin
    NumStr := ScanWhile(s, idx, ['0'..'9']);
    if (idx < l) and (s[idx] = '.') then begin
      i := idx + 1;
      f := ScanWhile(s, i, ['0'..'9']);
      if f <> '' then begin
        idx := i;
        NumStr := NumStr + '.' + f;
      end;
    end;
  end;

  ScanWhile(s, idx, ['U','L','u','l']);
end;

function ParseCString(const S: AnsiString; var idx: Integer; var CStr: AnsiString): Boolean;
var
  quit  : Boolean;
  i     : Integer;
  ch    : AnsiChar;
begin
  Result := false;
  CStr := '';
  if not (S[idx] in ['"', #39]) then Exit;
  
  quit := false;
  i := idx+1;
  ch := S[idx];

  while (not quit) and (i <= length(s)) do begin
    ScanTo(s, i, [ch, #10, #13] );
    quit := (i > length(s)) or (s[i] in [ch, #10, #13]);
    if quit and (i <= length(s)) and ((s[i] ='"')) then
      if ((s[i] = ch) and (s[i-1] = '\')) then begin
        inc(i);
        quit := false;
      end;
  end;

  Result := (i <= length(s)) and (s[i] = ch);
  if Result then begin
    inc(i);
    CStr := Copy(s, idx, i-idx);
    idx := i;
  end;
end;

function isFloatNum(const num: AnsiString): Boolean;
begin
  Result := Pos('.', num)>0;
end;

function CToPascalNumeric(const Cnum: AnsiString): AnsiString;
var
  i   : Integer;
  num : Int64;
  c   : Int64;
begin
  if isFloatNum(cNum) then
    Result := cNum
  else if length(cNum) < 3 then
    Result := cNum
  else if cNum[1] <> '0' then
    Result := cNum
  else begin
    if cNum[2] = 'x'
      then Result := '$'+Copy(cNum, 3, length(cNum) - 2)
    else begin
      num := 0;
      c := 1;
      for i := length(cnum) downto 1 do begin
        if not (cnum[i] in['0'..'7']) then begin
          Result := cNum;
          Exit;
        end;
        num := num + c * (byte(cnum[i]) - byte('0'));
        c := c * 8;
      end;
      Result := IntToStr(num);
    end;
  end;
end;


function TTextParser.FindNextToken(var Token: AnsiString; var TokenType: TTokenType): Boolean;
var
  srch  : TCharSet;
  blck  : TCharSet;
  i     : Integer;
  t     : AnsiString;
  spaces  : TCharSet;
  Repl    : AnsiString;
begin
  Result := Index <= length(Buf);
  if not Result then Exit;

  srch := TokenTable.SpaceChars;
  blck := [];
  for i := 0 to TokenTable.CmtCount - 1 do begin
    t := TokenTable.CmtBlock[i].Open[1];
    if t <> '' then blck := blck + [t[1]];
  end;
  for i := 0 to TokenTable.CmtLine.Count - 1 do begin
    t := TokenTable.CmtLine[i];
    if t <> '' then blck := blck + [t[1]];
  end;
  srch := srch + blck;

  Token := '';
  Result := false;
  TokenType := tt_Ident;

  spaces := TokenTable.SpaceChars;
  try
    while (not Result) and (index <= length(Buf)) do begin
      ScanWhile(Buf, index, spaces);
      if isMultiline then begin
        ScanTo(Buf, index, EoLnChars);
        SkipSingleEoLnChars;

      end else begin
        if (IsSubStr(TokenTable.Precompile, Buf, Index) and HandlePrecomiler) then
           // 1. check is Preprocessor directive is found
        else if (Buf[index] in TokenTable.Symbols) then begin                 // 2. symbol has been found, so it's not an ident
          if (not (Buf[index] in blck)) or (not SkipComments) then begin //   2.1 check if comment is found (comment prefixes match to the symbols)
            Result := true;                                              //   2.2 check if symbol is found
            if (Buf[index] = '.') and (index < length(Buf)) and (Buf[index+1] in ['0'..'9']) then begin
              // is float number
              inc(index);
              Token := '.' + ScanWhile(Buf, index, ['0'..'9']);
              TokenType := tt_Numeric;
            end else begin
              TokenType := tt_Symbol;
              Token := Buf[index];
              inc(index);
            end;
            Exit;
          end;
        end else if (Buf[index] in ['0'..'9']) then begin  // 3. a number is found, so it's possibl a number
          //todo: Hex and floats support!
          //todo: Negative numbers support;
          ParseCNumeric(Buf, index, Token);
          TokenType := tt_Numeric;
          Result := true;
          Exit;
        end else if (Buf[index] in TokenTable.StringStart) then begin
          ParseCString(Buf, index, Token);
          TokenType := tt_String;
          Result := true;
          Exit;
        end else begin
          Token := Token + ScanTo(Buf, index, srch+TokenTable.Symbols+[TokenTable.MultiLine]); // scanning for token
          if (Buf[index] in blck)  then begin
            Result := SkipComments;
            Result := Result or (Buf[index] in TokenTable.SpaceChars);
            if not Result then begin
              Token := Token + Buf[index];
              inc(index);
            end;
          end else
            Result := true;
          Result := Result and (Token <> '');
        end;
      end;

      {if Result and (IgnoreTokens.Count > 0) then
        if IgnoreTokens.IndexOf(Token) >= 0 then begin
          if Assigned(OnIgnoreToken) then
            OnIgnoreToken(Self, Token);
          Result := false;
          TokenType := tt_None;
          Token := '';
        end;}

      if (Token <> '') and (TokenType = tt_Ident) and Result then begin
        TokenPos := Index - length(Token);
        if HandleMacro(Token, Repl) then begin
          Delete(buf, TokenPos, length(Token));
          Insert(Repl, Buf, TokenPos);
          Index := TokenPos;
          Result := false;
          TokenType := tt_Ident;
          Token := '';
        end;
      end;
      
    end; {of while}
  finally
    if not Result
      then TokenType := tt_Ident
      else TokenPos := Index - length(Token);
    //todo: make an event or something
    if TokenType = tt_Numeric then
      Token := CToPascalNumeric(Token);
  end;
end;

function TTextParser.SkipComments: Boolean;
var
  i   : Integer;
  cmt : AnsiString;
  comment : TComment;
begin
  try
    cmt := '';
    Result := false;
    for i := 0 to TokenTable.CmtCount - 1 do
      if IsSubStr(TokenTable.CmtBlock[i].Open, Buf, index) then begin
        inc(index, length(TokenTable.CmtBlock[i].Open));
        cmt := SkipCommentBlock(Buf, index, TokenTable.CmtBlock[i].Close);
        Result := true;
        Exit;
      end;
    for i := 0 to TokenTable.CmtLine.Count - 1 do
      if IsSubStr(TokenTable.CmtLine[i], Buf, index) then begin
        cmt := SkipLine(Buf, index);
        Delete(cmt, 1, length(TokenTable.CmtLine[i]) );
        Result := true;
        Exit;
      end;
  finally

    if UseCommentEntities then begin
      comment := TComment.Create(nil);
      comment._Comment := GetBufWideStr(cmt);
      Comments.Add(Comment);
    end;
    if (Assigned(OnComment)) and (cmt <> '') then
      OnComment(Self, cmt);
  end;
end;

procedure TTextParser.SetError(const ErrorCmt: AnsiString);
begin
  Errors.Add(ErrorCmt);
end;

function TTextParser.HandleMacro(var MacroStr: AnsiString; var ReplaceStr: AnsiString): Boolean;
var
  m : AnsiString;
begin
  Result := false;
  if ProcessingMacro or not Assigned(MacroHandler) then Exit;

  ProcessingMacro := true;
  try
    Result := MacroHandler.MacroDefined(MacroStr);
    if not Result then Exit;

    m := MacroStr;
    Index := TokenPos;
    Result := MacroHandler.ParseMacro(Self, MacroStr, ReplaceStr);
  finally
    ProcessingMacro := false;
  end;
end;

function TTextParser.GetBufWideStr(const Cmd: AnsiString): WideString;
begin
  Result := Cmd;
end;

function TTextParser.AddChildToStackEntity(ent: TObject): Boolean;
var
  parent : TEntity;
begin
  Result := Assigned(stack) and (stack.Count>0);
  if not Result then Exit;

  parent := stack[stack.Count-1];
  if Assigned(parent) and (parent is TEntity) then
    (parent as TEntity).Items.Add(ent);
end;

function TTextParser.IsMultiLine: Boolean;
begin
  Result := TokenTable.MultiLine <> #0;
  if not Result then Exit;
  Result := (Buf[index] = TokenTable.MultiLine);
end;

procedure TTextParser.SkipSingleEoLnChars;
var
  next  : integer;
begin
  next := index + 1;
  if next > length(Buf) then next := -1;

  if next < 0 then
    inc(index)
  else begin
    if (Buf[index] = #10) and (Buf[next] = #13) then
      Index := next+1
    else if (Buf[index] = #13) and (Buf[next] = #10) then
      Index := next + 1
    else
      inc(Index);
  end;
end;

{ TTokenTable }

constructor TTokenTable.Create;
begin
  CmtLine := TStringList.Create;
end;

destructor TTokenTable.Destroy;
begin
  CmtLine.Free;
  inherited;
end;

{ TEntity }

procedure TEntity.Assign(AEntity: TEntity);
begin
  TagComment := AEntity.TagComment;
end;

constructor TEntity.Create(AOwner: TEntity);
begin
  inherited Create;
  Owner := AOwner;
  Items := TList.Create;
end;

destructor TEntity.Destroy;
begin
  Items.Free;
  inherited Destroy;
end;

function TEntity.Parse(AParser: TTextParser): Boolean;
begin
  Result := false;
  AParser.BeginParse(Self);
  try
    Result := DoParse(AParser);
  except
    on e: Exception do
      AParser.SetError('Internal error. Exception: ' + e.Message);
  end;
  AParser.EndParse;
end;

{ TClassDef }

constructor TClassDef.Create(AOwner: TEntity);
begin
  inherited Create(AOwner);
  _Protocols := TStringList.Create;
end;

destructor TClassDef.Destroy;
begin
  _Protocols.Free;
  inherited;
end;

function TClassDef.DoParse(AParser: TTextParser): Boolean;
var
  s   : AnsiString;
  tt  : TTokenType;
  cnt : Integer;
  mtd : TClassMethodDef;
  prop  : TObjCClassProperty;
  ent   : TEntity;
begin
  Result := false;
  AParser.FindNextToken(s, tt);
  if (s <> '@interface') and (s <> '@protocol')  then begin
    AParser.SetError(ErrExpectStr('@interface', s));
    Exit;
  end;

  AParser.FindNextToken(_ClassName, tt);

  if (not AParser.FindNextToken(s, tt)) then Exit; // parsing super class or category
  if tt = tt_Symbol then begin
    if s[1] = ':' then
      AParser.FindNextToken(_SuperClass, tt)
    else if s[1] = '(' then begin
      AParser.FindNextToken(_Category, tt);
      AParser.FindNextToken(s, tt);
    end else
      AParser.Index := AParser.TokenPos;
  end;

  AParser.FindNextToken(s, tt); // parsing protocols
  if (tt = tt_Symbol) and (s = '<') then begin
    repeat
      if not AParser.FindNextToken(s, tt) then Exit;
      if (s <> '>') then _Protocols.Add(s);
      AParser.FindNextToken(s, tt); // "," or ">"
    until (s = '>');
  end else
    AParser.Index := AParser.TokenPos;


  cnt := 0; // pasring private declarations
  repeat
    if not AParser.FindNextToken(s, tt) then begin
      s := '';
      AParser.SetError('error while parsing class declaration');
      exit;
    end;

    //work around for not using preprocessor! #if @interface #else @interface #endif
    if s = '@interface' then
      SkipLine(AParser.buf, AParser.index)
    else if s = '{' then inc(cnt)
    else if s = '}' then dec(cnt)
    else if s = '@property' then begin
      AParser.Index := AParser.TokenPos; // roll back to the start of method
      prop  := TObjCClassProperty.Create(Self);
      if not prop.Parse(AParser) then Exit;
      Items.Add(prop);
    end else if (cnt = 0) then begin
      //todo: better parsing
      // parsing methods
      if s[1] ='#' then SkipLine(AParser.buf, AParser.Index);
      if (s = '+') or (s = '-') then begin
        AParser.Index := AParser.TokenPos; // roll back to the start of method
        mtd := TClassMethodDef.Create(Self);
        mtd.Parse(AParser);
        Items.Add(mtd);
      end else if IsTypeOrTypeDef(s) then begin
        AParser.Index := AParser.TokenPos;
        if ParseTypeOrTypeDef(AParser, Self, ent) then
          Items.Add(ent);
        //AParser.FindNextToken(s, tt);
      end;

    end;
  until (s = '@end') or (s = ''); // looking for declaration end
  Result := true;
end;

function ParseFunctionOrVar(Owner: TEntity; AParser: TTextParser): Boolean;
var
  ctype : TEntity; 
  _name : AnsiString;
  isfunc  : Boolean; 
  tt    : TTokenType;
  s     : AnsiString;
  v     : TVariable;
  fn    : TFunctionDef;
  idx   : Integer;

  Modifiers : TStringList;
  ent : TEntity;

begin
  idx := AParser.TokenPos;
  Result := false;
  Modifiers := TStringList.Create;
  ctype:=nil;
  fn := nil;
  try
    repeat
      if not AParser.FindNextToken(s, tt) or (tt <> tt_Ident) then begin
        Result := false;
        Exit;
      end;
      if isCReserved (s) then begin
        Modifiers.Add(s); // C reserved tokens cannot be name of a function
        s := '';
      end;
    until s <> '';

    AParser.Index := AParser.TokenPos;

{    if s = 'struct' then
      TTy
      ctype :=}


    ctype := ParseTypeDef(nil, AParser);
    {   TTypeDef.Create(nil);
    Result := ctype.Parse(AParser);
    if not Result then Exit;}

    // expecting name of Variable or Function name
    if not AParser.FindNextToken(_name, tt) or (tt <> tt_Ident) then begin
      Result := false;
      Exit;
    end;
    //rep := AParser.TokenPos;

    AParser.FindNextToken(s, tt);
    isfunc := (tt = tt_Symbol) and (s = '(');
    if isfunc then begin // is function
      AParser.Index := AParser.TokenPos;
      fn := TFunctionDef.Create(Owner);
      fn._ResultType := ctype;
      fn._Name := _name;
      fn._IsExternal := Modifiers.IndexOf('extern')>=0;
      fn._isInline := Modifiers.IndexOf('inline')>=0;
      fn.ParseParams(AParser);
      ent := fn;
    end else begin
      v := TVariable.Create(Owner);
      v._Type := ctype;
      v._Name := _name;
      v._isExtern := Modifiers.IndexOf('extern')>=0;
      ent := v;
      AParser.Index := AParser.TokenPos;
    end;
    AParser.FindNextToken(s, tt);

    Result := (tt = tt_Symbol) and (s = ';');
    if isfunc and not Result and Assigned(fn) then begin
      AParser.Index := AParser.TokenPos;
      fn._Body := TFunctionBody.Create(fn);
      Result := fn._Body.Parse(AParser);
    end;

    if Result then owner.Items.Add(ent)
      else ent.Free;

  finally
    if not Result then begin
      ctype.Free;
      AParser.Index := idx;
    end;
    Modifiers.Free;
  end;
end;

{ TObjCHeader }

constructor TObjCHeader.Create(AOwner: TEntity);
begin
  //obj-c header does not have any entity owners
  inherited Create(AOwner);
end;

function TObjCHeader.DoParse(AParser: TTextParser): Boolean;
var
  s   : AnsiString;
  tt  : TTokenType;
  ent : TEntity;
begin
  Result := false;
  while AParser.FindNextToken(s, tt) do begin
    if s = 'typedef' then begin
      AParser.Index := AParser.TokenPos;
      ent := TTypeNameDef.Create(Self);
      if not ent.Parse(AParser) then Exit;
    end else if s = 'enum' then begin
      AParser.Index := AParser.TokenPos;
      ent := TEnumTypeDef.Create(Self);
      if not ent.Parse(AParser) then Exit;
      AParser.FindNextToken(s, tt); // skipping last ';'
      if s <> ';' then AParser.Index := AParser.TokenPos;
    end else if s = 'struct' then begin
      APArser.index := APArser.TokenPos;
      ent := TEntityStruct.Create(SElf);
      if not ent.Parse(AParser) then Exit;
      AParser.FindNextToken(s, tt); //? skipping last ';'?
      if s <> ';' then AParser.Index := AParser.TokenPos;
    end else if (s = '@interface') then begin
      AParser.Index := AParser.TokenPos;
      ent := TClassDef.Create(Self);
      if not ent.Parse(AParser) then Exit;
    end else if s = '@class' then begin
      AParser.Index := AParser.TokenPos;
      ent := TClassesForward.create(Self);
      if not ent.Parse(AParser) then Exit;
    end else if s = '@protocol' then begin
      AParser.Index := AParser.TokenPos;
      ent := ParseObjCProtocol(Self, AParser);
      if not Assigned(ent) or not ent.Parse(AParser) then
        Exit;
    end else begin
      AParser.Index := AParser.TokenPos;
      ent := nil;
      if not ParseFunctionOrVar(Self, AParser) then begin
        ent := ParseCustomEntity(Self, AParser);
        if not Assigned(ent) then begin
          // anything else is skipped, though should not!
          ent := TSkip.Create(Self);
          AParser.Index := AParser.TokenPos;
          TSkip(ent)._Skip := SkipLine(AParser.Buf, AParser.Index);
        end;
      end;
    end;
   if Assigned(ent) then Items.Add(ent);
  end;
  Result := true;
end;

{ TClassMethodDef }

function TClassMethodDef.GetResultType: TObjCResultTypeDef;
var
  i   : integer;
begin
  for i := 0 to Items.Count - 1 do
    if TObject(Items[i]) is TObjCResultTypeDef then begin
      Result := TObjCResultTypeDef(Items[i]);
      Exit;
    end;
  Result := nil;
end;



function TClassMethodDef.DoParse(AParser: TTextParser): Boolean;
var
  s     : AnsiString;
  tt    : TTokenType;
  res   : TObjCResultTypeDef;
  para  : TObjCParameterDef;
  des   : TParamDescr;
begin
  Result := false;
  AParser.FindNextToken(s, tt);
  if (s <> '+') and (s <> '-') then begin
    AParser.SetError( ErrExpectStr(' + or -, method descriptor ', s));
    Exit;
  end;

  _CallChar := s[1];
  _IsClassMethod := _CallChar = '+';

  AParser.FindNextToken(s, tt);
  if (tt = tt_Symbol) and(s = '(') then begin
    // _Class methods can be with out type
    AParser.Index:=AParser.TokenPos;
    res := TObjCResultTypeDef.Create(Self);
    if not res.Parse(AParser) then begin
      res.Free;
      Exit;
    end;
    Items.Add(res);
  end else if (tt = tt_Ident) then begin
    // if type is not defined, that it's assumed to be obj-c 'id'
    res := TObjCResultTypeDef.Create(Self);
    res._Type := TTypeDef.Create(res);
    TTypeDef(res._Type)._Name := 'id';

    Items.Add(res);
    AParser.Index := AParser.TokenPos;
  end else
    APArser.SetError(ErrExpectStr('(', s));

  if not AParser.FindNextToken(_Name, tt) then begin
    AParser.SetError(ErrExpectStr('method name Identifier', s));
    Exit;
  end;

  while AParser.FindNextToken(s, tt) do begin
    if s = ';' then
      Break // successfuly parsed!
    else if s = ':' then begin
      para := TObjCParameterDef.Create(Self);
      if not para.Parse(AParser) then begin
        para.Free;
        Exit;
      end;
      Items.Add(para);
    end else if tt = tt_Ident then begin
      des := TParamDescr.Create(Self);
      des._Descr := s;
      Items.Add(des);
    end else begin
      AParser.SetError(ErrExpectStr('type identifier', s));
      Exit;
    end;
  end;
//  AParser.FindNextToken()
  Result := true;
end;

{ TParameterDef }

function TObjCParameterDef.DoParse(AParser: TTextParser): Boolean;
var
  tt  : TTokenType;
begin
  Result := false;
  _Type := TObjCResultTypeDef.Create(Self);
  if not _Type.Parse(AParser) then Exit;

  Items.Add(_Type);
  AParser.FindNextToken(_Name, tt);
  if tt <> tt_Ident then begin
    AParser.SetError(ErrExpectStr('Identifier', _Name));
    Exit;
  end;
  Result := true;
end;

function isParamFuncPointer(AParser: TTextParser): Boolean;
var
  //i   : Integer;
  s   : AnsiString;
  tt  : TTokenType;
begin
  //i := AParser.Index;

  AParser.FindNextToken(s, tt);
  Result := (tt = tt_Symbol) and (s = '(');
  if not Result then Exit;

  AParser.FindNextToken(s, tt);
  Result := (tt = tt_Symbol) and (s = '*');
  if not Result then Exit;

  AParser.FindNextToken(s, tt);
  Result := (tt = tt_Symbol) and (s = ')');
  if not Result then Exit;
end;

{ TResultTypeDef }

function TObjCResultTypeDef.DoParse(AParser: TTextParser): Boolean;
var
  s   : AnsiString;
  tt  : TTokenType;
  fnt : TFunctionTypeDef;

  openbracket : Boolean; // introduced to support property type parsing,
                         // that might be without brackets
begin
  AParser.FindNextToken(s, tt);
  openbracket := (tt = tt_Symbol) and (s = '(');
  if not openbracket  then begin
    //AParser.SetError(ErrExpectStr('"("', s));
    AParser.Index := AParser.TokenPos;
    //Exit;
  end;

  _Type := TTypeDef.Create(Self);
  Result := _Type.Parse(AParser);
  if not Result then Exit;

  if Result then begin
    AParser.FindNextToken(s, tt);
    if (tt=tt_Symbol) and (s='<') then begin // skip protocol
      while (s <> '>') and AParser.FindNextToken(s, tt) do ;
      AParser.FindNextToken(s, tt);
    end;

    if s = '(' then begin // ptr funciton (*)?
      AParser.Index := AParser.TokenPos;
      if not isParamFuncPointer(APArser) then begin
        AParser.SetError(ErrExpectStr(')', s));
        Result := false;
        Exit;
      end;
      fnt := TFunctionTypeDef.Create(Self);
      fnt._ResultType := _Type;
      Result := fnt.Parse(AParser);
      _Type := fnt;
      if not Result then Exit;
      AParser.FindNextToken(s, tt);
    end;
    Result := (not openbracket) or (s = ')');
    if Result and not openbracket then
      AParser.Index := AParser.TokenPos;


    if not Result then
      AParser.SetError( ErrExpectStr(')', s));
  end;

end;



{ TParamDescr }

function TParamDescr.DoParse(AParser: TTextParser): Boolean;
var
  tt  : TTokenType;
begin
  Result := false;
  AParser.FindNextToken(_Descr, tt);
  if tt <> tt_Ident then begin
    AParser.SetError(ErrExpectStr('Identifier', '_Descr'));
    Exit; 
  end;
  Result := true;
end;

{ TSubSection }

function TSubSection.DoParse(AParser: TTextParser): Boolean;
begin
 //todo:
  Result := true;
end;

{ TPrecompiler }

function TPrecompiler.DoParse(AParser: TTextParser): Boolean;
var
  tt  : TTokenType;
begin
  Result := false;
  if not AParser.FindNextToken(_Directive, tt) then begin
    AParser.SetError('precompiler directive not found');
    Exit;
  end;
  if (_Directive = '') or (_Directive[1] <> '#') then begin
    AParser.Index := AParser.TokenPos;
    AParser.SetError('identifier is not precompiler directive');
    Exit;
  end;
  _Params := SkipLine(AParser.Buf, AParser.Index);
  Result := true;
end;

{ TEnumTypeDef }

function TEnumTypeDef.GetValue(idx: integer): TEnumValue;
var
  i   : Integer;
  v   : Integer;
begin
  v := 0;
  for i := 0 to Items.Count - 1 do
    if (TObject(Items[i]) is TEnumValue) and (v=idx) then begin
      Result := TEnumValue(Items[i]);
      Exit;
    end else
      inc(v);
  Result := nil;
end;

function TEnumTypeDef.DoParse(AParser: TTextParser): Boolean;
var
  token : AnsiString;
  tt    : TTokenType;
  nm    : AnsiString;
  vl    : TEnumValue;
begin
  Result := false;
  if not AParser.FindNextToken(token, tt) then Exit;
  if token <> 'enum' then begin
    AParser.SetError(ErrExpectStr('enum', token));
    Exit;
  end;

  if not AParser.FindNextToken(nm, tt) then begin
    AParser.SetError(ErrExpectStr('identifier', token));
    Exit;
  end;

  if tt <> tt_Ident then AParser.Index := AParser.TokenPos
  else _Name := nm;

  AParser.FindNextToken(nm, tt);
  if nm <> '{' then begin
    AParser.SetError(ErrExpectStr('"{" for enumeration', token));
    Exit;
  end;

  repeat
    vl := TEnumValue.Create(Self);
    if not vl.Parse(AParser) then begin
      vl.Free;
      Exit;
    end;

    if vl._Name <> '' then begin
      inc(fValCount);
      Items.Add(vl)
    end;

    AParser.FindNextToken(nm, tt);
    if tt = tt_Symbol then begin
      if (nm = ',') then begin
        AParser.FindNextToken(nm, tt);
        if tt = tt_Ident then
          AParser.Index := AParser.TokenPos;
      end;
    end else begin
      AParser.SetError(ErrExpectStr('"}"', token));
      Exit;
    end;

  until nm = '}';


  Result := true;
  //AParser.FindNextToken(nm, tt); // skip last ';'
end;

function ParseCOperator(AParser: TTextParser; var Vl: AnsiString): Boolean;
var
  nm  : AnsiSTring;
  tt  : TTokenType;
begin
  Result := false;
  if not AParser.FindNextToken(nm, tt) then Exit;
  Result := nm <> '';
  if not Result then Exit;
  vl := nm[1];
  case vl[1] of
    '+', '-', '*': Result := true;
    '|', '&': begin
      Result := true;
    end;
    '<', '>': begin
      vl := nm[1];
      Result := AParser.FindNextToken(nm, tt);
      if (not Result) or (nm = '') then Exit;
      Result := nm[1] = vl[1] ;
      if Result then vl := vl[1] + nm[1];
    end;
  else
    Result := false;
  end;
end;

function ParseCExpression(AParser: TTextParser; var ExpS: AnsiString): Boolean;
var
  i     : integer;
  nm    : AnsiString;
  tt    : TTokenType;
  brac  : Integer;
begin
//todo: better code. it's just a work around
//  i := AParser.Index;
  brac := 0;
  ExpS := '';
  Result := false;

  try
    while AParser.FindNextToken(nm, tt) do begin
      if (nm = #39) then begin
        ExpS := #39 + ScanTo(AParser.Buf, AParser.Index, [#39]) + #39;
        inc(AParser.Index);
        Result := true;
        Exit;
      end else if (tt = tt_Numeric) or (tt = tt_Ident) then begin
        ExpS := ExpS + nm;
        i := AParser.Index;
        if not ParseCOperator(AParser, nm) then begin
          AParser.Index := i;
          Break;
        end else
          ExpS := ExpS + ' ' + nm + ' ';
      end else if (tt = tt_Symbol) then begin
        if nm ='(' then inc(brac)
        else if nm = ')' then dec(brac);
      end else begin
        //i := AParser.Index;
        Exit;
      end;
    end;
    Result := true;

  finally
    if brac > 0 then
      while (brac > 0) and (AParser.FindNextToken(nm, tt)) do
        if nm = ')' then
          dec(brac);
  end;
end;

{ TEnumValue }

function TEnumValue.DoParse(AParser: TTextParser): Boolean;
var
  s   : AnsiString;
  tt  : TTokenType;
begin
  Result := false;
  AParser.FindNextToken(_Name, tt);
  if tt <> tt_Ident then begin
    AParser.SetError( ErrExpectStr('Identifier', _Name) );
    Exit;
  end;

  AParser.FindNextToken(s, tt);
  if s <> '=' then begin
    AParser.Index := AParser.TokenPos;
    _Value := '';
  end else begin
    if not ParseCExpression(AParser, _Value) then
      Exit;
  end;
  Result := true;
end;

{ TComment }

function TComment.DoParse(AParser: TTextParser): Boolean;
begin
  Result := true;
  //todo:! Comment parsing is now executed by TTextParser
end;

{ TTypeNameDef }

function TTypeNameDef.DoParse(AParser: TTextParser): Boolean;
var
  s   : AnsiString;
  tt  : TTokenType;
  fntype : TFunctionTypeDef;
begin
  Result := false;
  AParser.FindNextToken(s, tt);
  if s <> 'typedef' then begin
    AParser.SetError( ErrExpectStr('typedef', s));
    Exit;
  end;

  _Type := ParseTypeDef(Self, AParser);
  if not Assigned(_Type) then Exit;

  Result := AParser.FindNextToken(_TypeName, tt);
  if (tt = tt_Symbol) and (_TypeName = '(') then begin
    fntype := TFunctionTypeDef.Create(Self);
    fnType._ResultType := _Type;
    _Type.Owner := fntype;
    _Type:=fntype;

    // function-type
    Result := AParser.FindNextToken(s, tt);
    if (tt<>tt_Symbol) and (s <>'*') then AParser.SetError( ErrExpectStr('*', s) );
    AParser.FindNextToken(_TypeName, tt);
    AParser.FindNextToken(s, tt);
    if not Result then Exit;
    if (tt<>tt_Symbol)and (s <> ')') then AParser.SetError( ErrExpectStr(')', s) );
    
    Result := fnType.Parse(AParser);
    if not Result then Exit;
    AParser.FindNextToken(s, tt); // skip last ';';

  end else begin
    if not Result then begin
      AParser.SetError( ErrExpectStr('Type name identifier', _TypeName) );
      Exit;
    end;
    _inherited := GetTypeNameFromEntity( _Type );
    AParser.FindNextToken(s, tt); // skip last ';';
  end;
  if Assigned(_Type) then Items.Add(_Type);
  Result := true;
end;

{ TEntityStruct }

function TEntityStruct.DoParse(AParser: TTextParser): Boolean;
var
  s   : AnsiString;
  tt  : TTokenType;
  i   : Integer;
  st    : TStructField;
  prev  : TStructField;
begin
  Result := false;
  AParser.FindNextToken(s, tt);
  if s <> 'struct' then begin
    AParser.SetError(ErrExpectStr('struct', s));
    Exit;
  end;

  AParser.FindNextToken(s, tt);
  i := AParser.TokenPos;
  if (tt = tt_Ident) then begin
    _Name := s;
    AParser.FindNextToken(s, tt);
    i := AParser.TokenPos;
  end;

  if not ((tt = tt_Symbol) and (s = '{')) then begin
    AParser.Index := i;
    ParsePointerDef(AParser, _isPointer, _isPointerRef);
    Result := true;
    Exit;
  end;

  AParser.FindNextToken(s, tt);
  if s <> '}' then
    AParser.Index := AParser.TokenPos;
  prev := nil;
  while (s <> '}') do begin
    //i := AParser.TokenPos;
    st := TStructField.Create(Self);
    if not Assigned(prev) then begin
      if not st.Parse(AParser) then Exit;
    end else begin
      Result := ParseCVarDef(APArser, st._Name, st._IsArray, st._ArraySize );
      if not Result then
        Exit;
      {if tt <> tt_Ident then begin
        AParser.SetError(ErrExpectStr('field name', st._Name));
        Exit;
      end;}
      st._TypeName := prev._TypeName;
    end;

    Items.Add(st);
    AParser.FindNextToken(s, tt);
    if s = ','
      then prev := st
      else prev := nil;

    if s = ';' then begin
      AParser.FindNextToken(s, tt);
      if s <> '}' then
        AParser.Index := AParser.TokenPos;
    end;{ else begin
      AParser.Index := AParser.TokenPos;
    end;}
  end;

  Result := true;
  //no skipping last ';', because after structure a variable can be defined
  //ie: struct POINT {int x; int y} point;
end;

{ TStructField }

function CVal(c: AnsiString; var v: Integer): Boolean; // todo: hex, oct handling (0x, x)
var
  err : Integer;
begin
  Val(c, v, err);
  Result := err = 0;
end;

function TStructField.DoParse(AParser: TTextParser): Boolean;
var
  tt  : TTokenType;
  s   : AnsiString;
  fnc : TFunctionTypeDef;
//  fld : TStructField;
begin
  Result := false;
  _Type := ParseTypeDef(Self, AParser);
  if not Assigned(_Type) then Exit;
  
  _TypeName := GetTypeNameFromEntity(_Type);

  {if not (AParser.FindNextToken(s, tt)) or (tt <> tt_Ident) then begin
    AParser.SetError(ErrExpectStr('Identifier', s));
    Exit;
  end;}

  AParser.FindNextToken(s, tt);
  if (tt=tt_Symbol) and (s = '(') then begin
    fnc := TFunctionTypeDef.Create(Self);
    fnc._ResultType := _Type;
    _Type := fnc;
    _TypeName := '';

    ParsePointerDef(AParser, fnc._isPointer, fnc._isPointerRef);
    Result := ParseCVarDef(AParser, _Name, _IsArray, _ArraySize );
    if not Result then Exit;
    //AParser.FindNextToken(_Name, tt);
    {if (tt <> tt_Ident) then begin
      AParser.SetError( ErrExpectStr('Identifier', _Name));
      Result := false;
      Exit;
    end;}

    AParser.FindNextToken(s, tt);
    if (tt <> tt_Symbol) and (s <> ')') then begin
      AParser.SetError(ErrExpectStr(')', s));
      Result := false;
      Exit;
    end;

    Result := fnc.Parse(AParser);


  end else begin
    AParser.Index := AParser.TokenPos;
    Result := ParseCVarDef(AParser, _Name, _IsArray, _ArraySize );
    if not Result then Exit;

    AParser.FindNextToken(s, tt);
    if (tt = tt_Symbol) and (s = ':') then begin
      AParser.FindNextToken(s, tt);
      if tt <> tt_Numeric then begin
        AParser.SetError(ErrExpectStr('number', s));
        Exit;
      end;
      CVal(s, _BitSize);
    end else if (tt = tt_Symbol) and (s = '(') then begin
      //
    end else
      AParser.Index := AParser.TokenPos;
    Result := true;
    //success: (tt = tt_Symbol) and (s = ';')
  end;

end;

{ TTypeDef }

function IsSpecifier(const s: AnsiSTring; var SpecVal, SpecMask: TTypeDefSpecs): Boolean;
begin
  Result := true;
  if (s = 'volitle') then begin
    SpecVal := [td_Volitale];
    SpecMask := [td_Volitale];
  end else if (s = 'const') then begin
    SpecVal := [td_Const];
    SpecMask := [td_InOut, td_Const];
  end else if (s = 'signed') then begin
    SpecVal := [td_Signed];
    SpecMask := [td_Signed, td_Unsigned];
  end else if (s = 'unsigned') then begin
    SpecVal := [td_Unsigned];
    SpecMask := [td_Signed, td_Unsigned];
  end else if (s = 'long') then begin
    SpecVal := [td_Long];
    SpecMask := [td_Long, td_Short, td_Char];
  end else if (s = 'short') then begin
    SpecVal := [td_Short];
    SpecMask := [td_Long, td_Short, td_Char];
  end else if (s = 'char') then begin
    SpecVal := [td_Char];
    SpecMask := [td_Long, td_Short, td_Char];
  end else if (s = 'int') then begin
    SpecVal := [td_Int];
    SpecMask := [td_Int];
  end else if (s = 'inout') then begin
    SpecVal := [td_inout];
    SpecMask := [td_inout, td_const];
  end else
    Result := false;
end;

function TTypeDef.DoParse(AParser: TTextParser): Boolean;
var
  s   : AnsiString;
  tt  : TTokenType;
  vl  : TTypeDefSpecs;
  msk : TTypeDefSpecs;
begin
  Result := false;
  AParser.FindNextToken(s, tt);
  if (tt = tt_Ident) and (IsSpecifier(s, vl, msk)) then begin
    // search all specifiers
    while (tt = tt_Ident) and (IsSpecifier(s, vl, msk)) do begin
      if (_Spec * msk <> []) and (s <> 'long') then begin
        AParser.SetError( ErrExpectStr('Type identifier', s));
        Exit;
      end;
      _Spec := _Spec + vl;
      if (s <> 'const') and (s <> 'volatile') then begin
        if _Name = '' then _Name := s
        else _Name := _Name + ' ' + s;
      end;
      AParser.FindNextToken(s, tt);
    end; {of while}

    if ((_Spec * [td_Unsigned, td_Int, td_Short, td_Char, td_Long]) = [])  then begin
      // if int, short long or char is not specified
      // volatile or const are
      Result := tt = tt_Ident;
      if not Result then begin
        AParser.SetError(ErrExpectStr('Identifier', s));
        Exit;
      end;
      _Name := s;
      Result := true;
      //AParser.FindNextToken(s, tt);
    end else begin
      AParser.Index := AParser.TokenPos;
      Result := true;
    end;

  end else begin
    _Name := s;
    //AParser.FindNextToken(s, tt);
    Result := true;
  end;

  if (Result) {and (tt=tt_Symbol) and (s = '*') }then begin
//    AParser.Index := AParser.TokenPos;
    ParsePointerDef(AParser, _isPointer, _isPointerRef);
  end;

end;

{ TSkip }

function TSkip.DoParse(AParser: TTextParser): Boolean;
begin
  Result := true;
end;

{ TUnionTypeDef }

function TUnionTypeDef.DoParse(AParser: TTextParser): Boolean;
var
  s   : AnsiString;
  tt  : TTokenType;
  i   : Integer;
  st    : TStructField;
  prev  : TStructField;
begin
  Result := false;
  AParser.FindNextToken(s, tt);
  if s <> 'union' then begin
    AParser.SetError(ErrExpectStr('union', s));
    Exit;
  end;

  AParser.FindNextToken(s, tt);
  i := AParser.TokenPos;
  if (tt = tt_Ident) then begin
    _Name := s;
    AParser.FindNextToken(s, tt);
    i := AParser.TokenPos;
  end;

  if not ((tt = tt_Symbol) and (s = '{')) then begin
    if (tt = tt_Symbol) and (s = '*')
      then _isPointer := true
      else AParser.Index := i;
    Exit;
  end;

  AParser.FindNextToken(s, tt);
  if s <> '}' then
    AParser.Index := AParser.TokenPos;
  prev := nil;
  while (s <> '}') do begin
    //i := AParser.TokenPos;
    st := TStructField.Create(Self);
    if not Assigned(prev) then begin
      if not st.Parse(AParser) then Exit;
    end else begin
      AParser.FindNextToken(st._Name, tt);
      if tt <> tt_Ident then begin
        AParser.SetError(ErrExpectStr('field name', st._Name));
        Exit;
      end;
      st._TypeName := prev._TypeName;
    end;

    Items.Add(st);
    AParser.FindNextToken(s, tt);
    if s = ','
      then prev := st
      else prev := nil;

    if s = ';' then begin
      AParser.FindNextToken(s, tt);
      if s <> '}' then AParser.Index := AParser.TokenPos;
    end else begin
      AParser.Index := AParser.TokenPos;
    end;
  end;

  Result := true;
  //no skipping last ';', because after structure a variable can be defined
  //ie: struct POINT {int x; int y} point;
end;

function isEmptyStruct(AStruct: TEntityStruct): Boolean;
var
  i   : integer;
begin
  for i := 0 to AStruct.Items.Count - 1 do
    if TEntity(AStruct.Items[i]) is TStructField then begin
      Result := false;
      Exit;
    end;
  Result := true;
end;



{ TFunctionParamsList }

function TFunctionParamsList.DoParse(AParser: TTextParser): Boolean;
var
  s   : AnsiString;
  tt  : TTokenType;
  ent : TEntity;
  i   : Integer;
begin
  Result := (AParser.FindNextToken(s, tt)) and (tt=tt_Symbol) and (s = '(');
  if not Result then begin
    AParser.SetError( ErrExpectStr('(', s));
    Exit;
  end;
  Result := AParser.FindNextToken(s, tt);

  if not Result then begin
    AParser.SetError( ErrExpectStr(')', s));
    Exit;
  end;

  i := AParser.TokenPos;
  if (tt = tt_Ident) and (s='void') then begin
    AParser.FindNextToken(s, tt);
    if not ((tt = tt_Symbol) and (s = ')')) then
      AParser.Index := i;
  end else
    AParser.Index := i;

  while (s <> ')') do begin
    ent := TFunctionParam.Create(Self);
    Result := ent.Parse(AParser);
    if not Result then begin
      ent.Free;
      Exit;
    end;
    Items.Add(ent);
    AParser.FindNextToken(s, tt);
    if (s <> ')') then begin
      if not ((tt=tt_Symbol) and (s = ',')) then
        AParser.Index := AParser.TokenPos;
    end;
  end;

  Result := true;
end;

function isAnyParam(AParser: TTextParser): Boolean;
var
  i   : integer;
  s   : AnsiString;
  tt  : TTokenType;
begin
  Result := false;
  i := AParser.Index;
  if AParser.FindNextToken(s, tt) and (s = '.') then
    if AParser.FindNextToken(s, tt) and (s = '.') then
      if AParser.FindNextToken(s, tt) and (s = '.') then
        Result := true;
  if not Result then AParser.Index := i;
end;

{ TFunctionParam }

function TFunctionParam.DoParse(AParser: TTextParser): Boolean;
var
  s   : AnsiString;
  tt  : TTokenType;
begin
  _IsAny := isAnyParam(AParser);
  if _IsAny then begin
    Result := true;
    Exit;
  end;

  _Type := ParseTypeDef(Self, AParser);
  if not Assigned(_Type) then begin
    AParser.SetError( ErrExpectStr('type identifier', '' ));
    Result := false;
    Exit;
  end;
  AParser.FindNextToken(s, tt);

  if tt <> tt_Ident then
    AParser.Index := AParser.TokenPos
  else
    _Name := s;

  AParser.FindNextToken(s, tt);
  if (tt = tt_Symbol) and (s = '[') then begin
    _IsArray := true;
    AParser.FindNextToken(s, tt);
    if s <> ']' then begin
      AParser.SetError( ErrExpectStr(']', s));
      Result := false;
      Exit;
    end;
      
  end else
    AParser.Index := AParser.TokenPos; 
  Result:=true;
end;

{ TFunctionTypeDef }

function TFunctionTypeDef.DoParse(AParser: TTextParser): Boolean;
var
  s   : AnsiString;
  tt  : TTokenType;
begin
  _ParamsList := TFunctionParamsList.Create(Self);
  Items.Add(_ParamsList);

  AParser.FindNextToken(s, tt);
  if (tt = tt_Symbol) and (s = '(') then begin
    AParser.Index := AParser.TokenPos;
    Result := _ParamsList.Parse(AParser);
  end else if (tt = tt_Symbol) and (s = ';') then begin
    AParser.Index := AParser.TokenPos;
    Result := true;
  end else begin
    AParser.SetError(ErrExpectStr('(', s));
    Result := false;
  end;
end;

{ TClassesForward }

function TClassesForward.DoParse(AParser: TTextParser): Boolean;
var
  s   : AnsiString;
  tt  : TTokenType;
begin
  AParser.FindNextToken(s, tt);
  if (s <> '@class') and (s <> '@protocol') then begin
    AParser.SetError( ErrExpectStr('@class', s));
    Result := false;
    Exit;
  end;
  _isClasses := s = '@class';

  while s <> ';' do begin
    AParser.FindNextToken(s, tt);
    if tt = tt_Ident then
      _Classes.Add(s);
  end;
  Result := true;
end;

constructor TClassesForward.Create(AOwner: TEntity);
begin
  inherited Create(AOwner);
  _Classes:=TStringList.Create;
end;

destructor TClassesForward.Destroy;
begin
  _Classes.Free;
  inherited Destroy;
end;

{ TVariable }

function TVariable.DoParse(AParser: TTextParser): Boolean;  
var
  s   : AnsiString;
  tt  : TTokenType;
  _isAny  : Boolean; 
begin
  _IsAny := isAnyParam(AParser);
  if _IsAny then begin
    Result := true;
    Exit;
  end;

  _Type := ParseTypeDef(Self, AParser);
  if not Assigned(_Type) then begin
    AParser.SetError( ErrExpectStr('type identifier', '' ));
    Result := false;
    Exit;
  end;
  AParser.FindNextToken(s, tt);

  if tt <> tt_Ident then
    AParser.Index := AParser.TokenPos
  else
    _Name := s;
  Result:=true;
end;

function TVariable.ParseAfterTypeName(AParser: TTextParser): Boolean;  
begin
  Result := true; 
end;

{ TFunctionDef }

function TFunctionDef.DoParse(AParser: TTextParser): Boolean;
var
  s   : AnsiString;
  tt  : TTokenType;
begin
//  Result := false;
  Items.Add(_ParamsList);

  AParser.FindNextToken(s, tt);
  if (tt = tt_Symbol) and (s = '(') then begin
    AParser.Index := AParser.TokenPos;
    ParseParams(APArser) 
  end else if (tt = tt_Symbol) and (s = ';') then begin
    AParser.Index := AParser.TokenPos;
  end else begin
    AParser.SetError(ErrExpectStr('(', s));
    Result := false;
    Exit;
  end;
  if not Assigned(_ParamsList) then
    _ParamsList := TFunctionParamsList.Create(Self); // an empty param list
  Result := true;

  AParser.FindNextToken(s, tt);
  if (tt = tt_Symbol) and (s = '{') then begin
    AParser.Index := AParser.TokenPos;
    _Body := TFunctionBody.Create(Self);
    _Body.Parse(AParser);
  end else
    AParser.Index := AParser.TokenPos;

end;

function TFunctionDef.ParseParams(AParser: TTextParser): Boolean;
begin
  if not Assigned(_ParamsList) then
    _ParamsList := TFunctionParamsList.Create(Self);
  Result := _ParamsList.Parse(AParser);
end;

// detects if line ends + '\' symbol
// that means that macros is multilined
// Fix - returns the fixed string, with last '\' removed
function IsEofDefine(const macro: AnsiString): Boolean;
var
  i : integer;
begin
  for i := length(macro) downto 1 do
    if not (macro[i] in WhiteSpaceChars) then begin
      Result := macro[i] <> '\';
      Exit;
    end;
  Result := true;
end;

function RemoveMacroSlash(const macro: AnsiString): AnsiString;
var
  i : integer;
begin
  for i := length(macro) downto 1 do
    if not (macro[i] in WhiteSpaceChars) then begin
      if macro[i] = '\' then Result := Copy(macro, 1, i-1);
      Exit;
    end;
  Result := macro;
end;

{ TCPrepDefine }

function TCPrepDefine.DoParse(AParser: TTextParser): Boolean;
var
  s   : AnsiString;
  tt  : TTokenType;
  prs : AnsiString;

  SpaceChars : TCharSet;
  SymChars   : TCharSet;
//  idx : integer;
begin
  AParser.FindNextToken(s, tt);
  Result := s = '#define';
  if not Result then exit;

  AParser.FindNextToken(_name, tt);
  Result := tt = tt_Ident;
  if not Result then Exit;

  SpaceChars := AParser.TokenTable.SpaceChars;
  SymChars := AParser.TokenTable.Symbols;
  with AParser.TokenTable do SpaceChars := SpaceChars - [#10,#13];
  with AParser.TokenTable do Symbols := [#10, #13];
  try
//    idx := AParser.Index;
    AParser.FindNextToken(prs, tt);
    while (prs <> '') and (not (prs[1] in [#10, #13])) do begin
      SubsText := SubsText + ' ' + prs;
      AParser.FindNextToken(prs, tt);
    end;
    RemoveMacroSlash(SubsText);
    if prs <> '' then
      AParser.Index := AParser.TokenPos;

    {prs := SkipLine(AParser.buf, AParser.Index);
    while not IsEofDefine(prs) do begin
      SubsText := SubsText + RemoveMacroSlash(prs);
      prs := SkipLine(AParser.buf, AParser.Index);
    end;
    SubsText := SubsText + prs;}
  finally
    AParser.TokenTable.SpaceChars := SpaceChars;
    AParser.TokenTable.Symbols := SymChars;
  end;
end;

{ TCPrepInclude }

function TCPrepInclude.DoParse(AParser: TTextParser): Boolean;
var
  s     : AnsiString;
  tt    : TTokenType;
  exp   : AnsiString;
  chars : TCharSet;
begin
  AParser.FindNextToken(s, tt);
  Result := s = '#include';
  if not Result then exit;

  chars := AParser.TokenTable.Symbols;
  try
    AParser.TokenTable.Symbols := AParser.TokenTable.Symbols + ['"'];

    //i := AParser.TokenPos;

    AParser.FindNextToken(s, tt);
    Result := (s = '"') or (s = '<');
    if not Result then Exit;

    if s = '"' then exp := '"'
    else if s = '<' then exp := '>';

    repeat
      AParser.FindNextToken(s, tt);
      if (s = '/') or (s = '\') or (tt = tt_Ident) then
        Included := Included + s;
    until (tt =tt_Symbol) and ((s <> '\') or (s <> '/'));

    Result := s = exp;
    SkipLine(AParser.buf, AParser.Index);
  finally
    AParser.TokenTable.Symbols := chars ;
  end;


end;

{ TCPrepElse }

function TCPrepElse.DoParse(AParser: TTextParser): Boolean;
var
  s : AnsiString;
  tt  : TTokenType;
begin
  AParser.FindNextToken(s, tt);
  Result := s = '#else';
  SkipLine(AParser.buf, AParser.Index);
end;

{ TCPrepEndif }

function TCPrepEndif.DoParse(AParser: TTextParser): Boolean;
var
  s : AnsiString;
  tt  : TTokenType;
begin
  AParser.FindNextToken(s, tt);
  Result := s = '#endif';
  SkipLine(AParser.buf, AParser.Index);
end;

{ TCPrepIf }

function TCPrepIf.DoParse(AParser: TTextParser): Boolean;
var
  s : AnsiString;
  tt  : TTokenType;
begin
  AParser.FindNextToken(s, tt);
  Result := (s = '#if') or (s = '#ifdef') or (s = '#elif') or (s = '#ifndef');
  _Cond := SkipLine(AParser.buf, AParser.Index);
end;

{ TCPrepPragma }

function TCPrepPragma.DoParse(AParser: TTextParser): Boolean;
var
  s : AnsiString;
  tt  : TTokenType;
begin
  AParser.FindNextToken(s, tt);
  Result := (s = '#pragma');
  _Text := SkipLine(AParser.buf, AParser.Index);
end;

{ TCMacroHandler }

procedure TCMacroHandler.AddSimpleMacro(const MacroStr,
  ReplaceStr: AnsiString);
var
  cm  : TCMacroStruct;
  i   : Integer;
begin
  cm := TCMacroStruct.Create;
  cm.MacroName := MacroStr;
  cm.ReplaceText := ReplaceStr;

  i := MacrosNames.IndexOf(MacroStr);
  if i >= 0 then begin
    MacrosNames.Objects[i].Free;
    MacrosNames.Delete(i);
  end;
  MacrosNames.AddObject(MacroStr, cm);
end;

procedure TCMacroHandler.Clear;
var
  i : Integer;
begin
  for i := 0 to MacrosNames.Count - 1 do MacrosNames.Objects[i].Free;
  MacrosNames.Clear;
end;

constructor TCMacroHandler.Create;
begin
  MacrosNames := TStringList.Create;
end;

destructor TCMacroHandler.Destroy;
begin
  Clear;
  MacrosNames.Free;
  inherited;
end;

function TCMacroHandler.MacroDefined(const Macro: AnsisTring): Boolean;
begin
  Result := MacrosNames.IndexOf(Macro) >= 0;
end;

function TCMacroHandler.ParseMacro(const Parser: TTextParser; var MacroStr,
  ReplaceStr: AnsiString): Boolean;
var
  s   : String;
  tt  : TTokenType;
  i   : Integer;
  //j   : Integer;
  cm  : TCMacroStruct;
  ReplaceValues : TStringList;
  cnt : Integer;
begin
  Parser.FindNextToken(s, tt);
  i := MacrosNames.IndexOf(s);
  Result := (i >= 0);
  if not Result then begin
    Parser.Index := Parser.TokenPos;
    Exit;
  end;

  cm := TCMacroStruct(MacrosNames.Objects[i]);
  if Assigned(cm.MacroParams) and (cm.MacroParams.Count > 0) then begin
    //j := Parser.TokenPos;
    Parser.FindNextToken(s, tt);
    if s <> '(' then begin
      Result := false;
      Parser.SetError('error while parsing macros usage');
      Exit;
    end;
    ReplaceValues := TStringList.Create;
    try
      cnt := 1;
      while (s <> ')') and (cnt > 0) do begin
      end;
    finally
      ReplaceValues.Free;
    end;

  end else begin
    MacroStr := cm.MacroName;
    ReplaceStr := cm.ReplaceText;
  end;



end;

{ TCMacroStruct }

constructor TCMacroStruct.Create;
begin
  MacroParams := TStringList.Create;
end;

destructor TCMacroStruct.Destroy;
begin
  MacroParams.Free;
  inherited;
end;

// custom entities

procedure RegisterEntity( CheckProc: TCustomEntityProc {; Location: TEntityLocation});
begin
  if not Assigned(CustomList) then
    CustomList := TList.Create;
  CustomList.Add(@CheckProc);
end;

function ParseCustomEntity(Parent: TEntity; Parser: TTextParser): TEntity;
var
  i     : integer;
  proc  :  TCustomEntityProc;
  index : Integer;
begin
  if not Assigned(CustomList) then begin
    Result := nil;
    Exit;
  end;
  
  index := Parser.TokenPos;
  for i := 0 to CustomList.Count - 1 do begin
    proc := TCustomEntityProc(CustomList[i]);
    Parser.TokenPos := index;
    if Assigned(@proc) then begin
      Result := proc(Parent, Parser);
      if Assigned(Result) then Exit;
    end;
  end;
  Result := nil;
end;

procedure ReleaseCustomEntities;
begin
  if Assigned(CustomList) then CustomList.Free;
end;

{ TObjCClassProperty }

function ParseGetterSetterName(AParser: TTextParser): AnsiString;
var
  tt: TTokenType;
  s : string;
begin
  Result := '';
  AParser.FindNextToken(s, tt);
  if (tt <> tt_Symbol) and (s <> '=') then Exit;
  AParser.FindNextToken(Result, tt);
end;

function TObjCClassProperty.DoParse(AParser: TTextParser): Boolean;
var
  s   : string;
  tt  : TTokenType;
begin
  Result := AParser.FindNextToken(s, tt);
  if not Result then begin
    AParser.SetError(ErrExpectStr('@property', s));
    Exit;
  end;

  AParser.FindNextToken(s, tt);
  if (tt = tt_Symbol) and (s = '(') then begin
    while s <> ')' do begin
      if (tt = tt_Symbol) and (s = ',') then
        AParser.FindNextToken(s, tt);

      if tt = tt_Ident then begin
        if s = 'setter' then _Setter := ParseGetterSetterName(AParser)
        else if s = 'getter' then _Getter  := ParseGetterSetterName(AParser)

        else if s = 'readwrite' then Include(_Attribs, pa_readwrite)
        else if s = 'readonly' then Include(_Attribs, pa_readonly)

        else if s = 'assign' then Include(_Attribs, pa_assign)
        else if s = 'retain' then Include(_Attribs, pa_retain)
        else if s = 'copy' then Include(_Attribs, pa_copy)

        else if s = 'nonatomic' then Include(_Attribs, pa_nonatomic);
      end;
      AParser.FindNextToken(s, tt);
    end;
  end;

  _Type := TObjCResultTypeDef.Create(Self);
  Result := _Type.Parse(AParser);
  if not Result then Exit;

  AParser.FindNextToken(_Name, tt);
  AParser.FindNextToken(s, tt); // skipping last ';';  
  Result := true;
end;


function TCCodeSection.DoParse(AParser: TTextParser): Boolean;
var
  s   : String;
  tt  : TTokenType;
  braces : Integer;
  idx   : Integer;
begin
  AParser.FindNextToken(s, tt);
  Result := (tt = tt_Symbol) and (s = '{');
  if not Result then begin
    AParser.SetError(ErrExpectStr('{', s));
    Exit;
  end;
  idx := AParser.TokenPos;

  braces := 1; // brace opened
  while braces > 0 do begin
    AParser.FindNextToken(s, tt);
    // todo: c expressions and declarations parsing
    if s = '{' then inc(braces) // another brace opened
    else if s = '}' then dec(braces); // brace closed 
  end;
  Result := true;
  _RawText := Copy(APArser.Buf, idx, AParser.Index - idx);
end;

initialization

finalization
  ReleaseCustomEntities;


end.
