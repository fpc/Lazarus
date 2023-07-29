unit SynTextMateSyn;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, Math,
  jsonparser, jsonscanner, fpjson,
  // LazUtils
  LazFileUtils,
  Laz2_XMLRead, PList2JSon, Laz2_DOM,
  // LazEdit
  TextMateGrammar,
  // SynEdit
  SynEditHighlighter, SynEditHighlighterFoldBase, SynEditTypes;

type

  TNameAttributesMap   = specialize TFPGMapObject<string, TSynHighlighterAttributes>;
  TGrammarLoadEvent = procedure(AGrammarFile, AGrammarPath: String; out AGrammarDef: String);

  { TSynTextMateSyn }

  TSynTextMateSyn = class(TSynCustomFoldHighlighter)
  private
    FGrammarPath: String;
    FOnLoadGrammarFile: TGrammarLoadEvent;
    FTextMateGrammar: TTextMateGrammar;

  private
    FAttriMap: TNameAttributesMap;

    function  LoadFile(AGrammarFile: String): String;
    procedure SetGrammarPath(AValue: String);

    function GetOrCreateAttribIdxForName(AName: String): integer;
    procedure DoPopulateAttributeInfo(Sender: TTextMateGrammar; APattern: TTextMatePattern;
      AContextName: String; var AnAttribInfo: TSynAttributeInfo);
    procedure DoCheckAttributeInfo(Sender: TTextMatePattern;
      const AnAttribInfo: TSynAttributeInfo; out AnUseId, AnUseObject: Boolean);
  private
    FCurrentRange: Integer;
    FCurrentTokenPos, FCurrentTokenLen: Integer;
    FCurrentTokenKind: integer;
    FCurrentAttrib: TSynHighlighterAttributes;
    function GetParserError: String;

  protected
    function GetInstanceLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadGrammar(AGrammarDef: String);
    procedure LoadGrammar(AGrammarFile, AGrammarPath: String);

    procedure SetLine(const NewValue: String; LineNumber: Integer); override;
    procedure Next; override;
    function GetEol: Boolean; override;

    function GetToken: String; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function GetTokenPos: Integer; override;
    function GetTokenKind: integer; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    //
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;

    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
    function GetRange: Pointer; override;


    function FoldBlockOpeningCount(ALineIndex: TLineIdx; const AFilter: TSynFoldBlockFilter): integer; override; overload;
    function FoldBlockClosingCount(ALineIndex: TLineIdx; const AFilter: TSynFoldBlockFilter): integer; override; overload;
    function FoldBlockEndLevel(ALineIndex: TLineIdx; const AFilter: TSynFoldBlockFilter): integer; override; overload;
    function FoldBlockMinLevel(ALineIndex: TLineIdx; const AFilter: TSynFoldBlockFilter): integer; override; overload;
    function FoldBlockNestedTypes(ALineIndex: TLineIdx; ANestIndex: Integer;
      out AType: Pointer; const AFilter: TSynFoldBlockFilter): boolean; override; overload;

    property OnLoadGrammarFile: TGrammarLoadEvent read FOnLoadGrammarFile write FOnLoadGrammarFile;
    property GrammarPath: String read FGrammarPath write SetGrammarPath;
    property ParserError: String read GetParserError;
    property TextMateGrammar: TTextMateGrammar read FTextMateGrammar;
  end;

implementation

{ TSynTextMateSyn }

function TSynTextMateSyn.LoadFile(AGrammarFile: String): String;
var
  s: TStringStream;
begin
  if Assigned(FOnLoadGrammarFile) then begin
    OnLoadGrammarFile(AGrammarFile, FGrammarPath, Result);
  end
  else begin
    s := TStringStream.Create('');
    try
      s.LoadFromFile(FGrammarPath + AGrammarFile);
      Result := s.DataString;
    finally
      s.Free;
    end;
  end;
end;

procedure TSynTextMateSyn.SetGrammarPath(AValue: String);
begin
  if FGrammarPath = AValue then Exit;
  if AValue <> '' then
    AppendPathDelim(AValue);
  FGrammarPath := AValue;
end;

function TSynTextMateSyn.GetOrCreateAttribIdxForName(AName: String): integer;
var
  attr: TSynHighlighterAttributes;
begin
  if AName = '' then
    exit(-1);
  Result := FAttriMap.IndexOf(AName);
  if Result >= 0 then
    exit;
  attr := TSynHighlighterAttributes.Create(AName, AName);
  AddAttribute(attr);
  Result := FAttriMap.Add(AName, attr);
end;

function TSynTextMateSyn.GetInstanceLanguageName: string;
begin
  Result := FTextMateGrammar.LanguageName;
end;

procedure TSynTextMateSyn.DoPopulateAttributeInfo(
  Sender: TTextMateGrammar; APattern: TTextMatePattern;
  AContextName: String; var AnAttribInfo: TSynAttributeInfo);
begin
  AnAttribInfo.TokId := GetOrCreateAttribIdxForName(AContextName);
  if AnAttribInfo.TokId < 0 then
    AnAttribInfo.TokObject := nil
  else
    AnAttribInfo.TokObject := FAttriMap.Data[AnAttribInfo.TokId];
end;

procedure TSynTextMateSyn.DoCheckAttributeInfo(Sender: TTextMatePattern;
  const AnAttribInfo: TSynAttributeInfo; out AnUseId, AnUseObject: Boolean);
begin
  AnUseId := AnAttribInfo.TokId >= 0;
  AnUseObject := (AnAttribInfo.TokObject <> nil) and
                 (TSynHighlighterAttributes(AnAttribInfo.TokObject).IsEnabled);
end;

function TSynTextMateSyn.GetParserError: String;
begin
  Result := FTextMateGrammar.ParserError;
end;

constructor TSynTextMateSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAttriMap := TNameAttributesMap.Create(False);
  FTextMateGrammar := TTextMateGrammar.Create;
  FTextMateGrammar.OnPopulateAttributeInfo := @DoPopulateAttributeInfo;
  FTextMateGrammar.OnCheckAttributeInfo := @DoCheckAttributeInfo;
end;

destructor TSynTextMateSyn.Destroy;
begin
  FTextMateGrammar.ClearGrammar;
  FTextMateGrammar.Free;
  FreeHighlighterAttributes;
  FAttriMap.Clear;
  FAttriMap.Free;
  inherited Destroy;
end;

procedure TSynTextMateSyn.LoadGrammar(AGrammarDef: String);
begin
  FTextMateGrammar.ParseGrammar(AGrammarDef);
end;

procedure TSynTextMateSyn.LoadGrammar(AGrammarFile, AGrammarPath: String);
begin
  GrammarPath := AGrammarPath;
  FTextMateGrammar.ParseGrammar(LoadFile(AGrammarFile));
  if FTextMateGrammar.LanguageName = '' then
    FTextMateGrammar.LanguageName := AGrammarFile;
end;

procedure TSynTextMateSyn.SetLine(const NewValue: String;
  LineNumber: Integer);
begin
  inherited SetLine(NewValue, LineNumber);

  if FCurrentRange = -2 then
    FCurrentRange := FTextMateGrammar.CurrentPatternIndex;
// TODO setline - keep range?

  FTextMateGrammar.SetLine(CurrentLineText, FCurrentRange);
  FCurrentRange := -2;

  if IsScanning then begin
    FTextMateGrammar.NextToEol;
  end
  else begin
    FTextMateGrammar.First;
    FCurrentTokenKind := FTextMateGrammar.CurrentTokenKind;
    FCurrentAttrib    := TSynHighlighterAttributes(FTextMateGrammar.CurrentAttrib);
    FCurrentTokenPos  := FTextMateGrammar.CurrentTokenPos;
    FCurrentTokenLen  := FTextMateGrammar.CurrentTokenLen;
  end;
  //FCurrentRange := FTextMateGrammar.CurrentPatternIndex;

end;

procedure TSynTextMateSyn.Next;
begin
  FTextMateGrammar.Next;
  FCurrentTokenKind := FTextMateGrammar.CurrentTokenKind;
  FCurrentAttrib    := TSynHighlighterAttributes(FTextMateGrammar.CurrentAttrib);
  FCurrentTokenPos  := FTextMateGrammar.CurrentTokenPos;
  FCurrentTokenLen  := FTextMateGrammar.CurrentTokenLen;
  //FCurrentRange     := FTextMateGrammar.CurrentPatternIndex;
end;

function TSynTextMateSyn.GetEol: Boolean;
begin
  Result := FTextMateGrammar.IsAtEol;
end;

function TSynTextMateSyn.GetToken: String;
begin
  Result := Copy(CurrentLineText, FCurrentTokenPos, FCurrentTokenLen);
end;

procedure TSynTextMateSyn.GetTokenEx(out TokenStart: PChar; out
  TokenLength: integer);
begin
  TokenStart := @CurrentLineText[FCurrentTokenPos];
  TokenLength := FCurrentTokenLen;
end;

function TSynTextMateSyn.GetTokenPos: Integer;
begin
  Result := FCurrentTokenPos - 1;
end;

function TSynTextMateSyn.GetTokenKind: integer;
begin
  Result := FCurrentTokenKind;
end;

function TSynTextMateSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  Result := FCurrentAttrib;
end;

function TSynTextMateSyn.GetDefaultAttribute(Index: integer
  ): TSynHighlighterAttributes;
begin
  Result := FAttriMap.Data[FTextMateGrammar.RootPattern.AttribInfo.TokId];
end;

procedure TSynTextMateSyn.SetRange(Value: Pointer);
begin
  FCurrentRange := PtrUInt(Value);
end;

procedure TSynTextMateSyn.ResetRange;
begin
  FCurrentRange := -1;
end;

function TSynTextMateSyn.GetRange: Pointer;
begin
  FCurrentRange     := FTextMateGrammar.CurrentPatternIndex;
  Result := Pointer(PtrUInt(FCurrentRange));
end;

function TSynTextMateSyn.FoldBlockOpeningCount(ALineIndex: TLineIdx;
  const AFilter: TSynFoldBlockFilter): integer;
begin
  Result := 0;
end;

function TSynTextMateSyn.FoldBlockClosingCount(ALineIndex: TLineIdx;
  const AFilter: TSynFoldBlockFilter): integer;
begin
  Result := 0;
end;

function TSynTextMateSyn.FoldBlockEndLevel(ALineIndex: TLineIdx;
  const AFilter: TSynFoldBlockFilter): integer;
begin
  Result := 0;
end;

function TSynTextMateSyn.FoldBlockMinLevel(ALineIndex: TLineIdx;
  const AFilter: TSynFoldBlockFilter): integer;
begin
  Result := 0;
end;

function TSynTextMateSyn.FoldBlockNestedTypes(ALineIndex: TLineIdx;
  ANestIndex: Integer; out AType: Pointer; const AFilter: TSynFoldBlockFilter
  ): boolean;
begin
  Result := False;
end;

end.

