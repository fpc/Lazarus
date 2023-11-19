unit TestTextMateGrammar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, fpcunit, testutils, testregistry,
  TextMateGrammar, LazLoggerBase;

type
  TCaptIdx = (c0, c1, c2, c3, c4, c5, c6);
  TCaptSet = set of TCaptIdx;

  TGrammarMap = specialize TFPGMapObject<string, TTextMateGrammar>;

  { TTestTextMateGrammar }

  TTestTextMateGrammar = class(TTestCase)
  private
    procedure DoCheckAttributeInfo(Sender: TTextMatePattern;
      const AnAttribInfo: TSynAttributeInfo; out AnUseId, AnUseObject: Boolean);
    function DoGetIncludedGrammar(Sender: TTextMateGrammar; AScopeName: string
      ): TTextMateGrammar;
//TODO: separate FNames for each grammar
    procedure DoPopulateAttributeInfo(Sender: TTextMateGrammar;
      APattern: TTextMatePattern; AContextName: String;
      var AnAttribInfo: TSynAttributeInfo);

    function BuildMatch(Name, Ptn: String; Extra: String = ''): String;
    function BuildBeginEnd(Name, PtnBegin, PtnEnd: String; Extra: String = ''): String;

    function GetTestA1Foo: String;
    function GetTestA1(WithContentName: Boolean; ASubPatterns: array of String): String;
    function GetTestA1(WithContentName: Boolean; BCaptures, ECaptures, ACaptures: TCaptSet;
      B3Pattern, B5Pattern, E3Pattern, E5Pattern, ASubPatterns: array of String): String;

    function GetTestB1Bar(ASubPatterns: array of String): String;
    function GetTestB1Nest(ASubPatterns: array of String): String;
    function GetTestB1(WithContentName, WithCaptures: Boolean; ASubPatterns: array of String): String;

    function GetTestC1(ASubPatterns: array of String): String;
    function GetTestM1(ASubPatterns: array of String): String;

    function GetTestW1(ASubPatterns: array of String): String;

    function GetTestNest(ASubPatterns: array of String): String;

    function  Join(APatterns: array of String): String;
    function  Include(const AName: String; SkipHash: Boolean = False): String;
    function  BuildPatterns(const APatterns: array of String; AMore: String = ''; LeadComma: boolean = True): String;
    function  BuildCaptures(APatterns: array of String): String;
    procedure SetGrammar(AText: String);
    procedure SetGrammar(const ARootPatterns, ARepository: array of String);
    procedure SetGrammar(const AScopeName: String; ARootPatterns, ARepository: array of String);

    function RunGrammar(ATestName, ATextLine: String; out LastPatternIndex: Integer; AStartPatternIndex: integer = -1): String;
    procedure RunNextToEol(ATestName, ATextLine: String; out LastPatternIndex: Integer; AStartPatternIndex: integer = -1);
    function TestLine(ATestName, ATextLine, Expect: String; AStartPatternIndex: integer = -1): Integer;
    function TestLine(ATestName, ATextLine, Expect: String; AStartPatternName: String): Integer;

  private
    FNewScopeName: String;
    FGrammarMap: TGrammarMap;
  protected
    FGrammar: TTextMateGrammar;
    FGrammarText: String;
    FNames: TStringList;
    procedure SetUp; override;
    procedure TearDown; override;
    procedure ResetGramarMap;
  published
    procedure TestFlatNested;
    procedure TestInclude;
    procedure TestBeginEnd;
    procedure TestBeginEndInCapture;
    procedure TestWhile;
    procedure TestForwarder;
    procedure TestRecurse;
    procedure TestIncludeOtherGrammar;
    procedure TestVarious;
  end;

implementation

procedure TTestTextMateGrammar.DoCheckAttributeInfo(Sender: TTextMatePattern;
  const AnAttribInfo: TSynAttributeInfo; out AnUseId, AnUseObject: Boolean);
begin
  AnUseId := True;
  AnUseObject := True;
end;

function TTestTextMateGrammar.DoGetIncludedGrammar(Sender: TTextMateGrammar;
  AScopeName: string): TTextMateGrammar;
begin
  Result := nil;
  if FGrammarMap.IndexOf(AScopeName) >= 0 then
    Result := FGrammarMap[AScopeName];
end;

procedure TTestTextMateGrammar.DoPopulateAttributeInfo(
  Sender: TTextMateGrammar; APattern: TTextMatePattern; AContextName: String;
  var AnAttribInfo: TSynAttributeInfo);
begin
  if AContextName = 'Default-text' then AContextName := '-';
  AnAttribInfo.TokId := FNames.IndexOf(AContextName);
  if AnAttribInfo.TokId < 0 then
    AnAttribInfo.TokId := FNames.Add(AContextName);
  AnAttribInfo.TokObject := nil;
end;

function TTestTextMateGrammar.BuildMatch(Name, Ptn: String; Extra: String
  ): String;
begin
  Result := '{' +
            Join([
              '"contentName": "'+Name+'"',
              '"name":  "'+Name+'"',
              '"match": "'+Ptn+'"',
              Extra
            ])+
            '}';
end;

function TTestTextMateGrammar.BuildBeginEnd(Name, PtnBegin, PtnEnd: String;
  Extra: String): String;
begin
  Result := '{' +
            Join([
              '"contentName": "'+Name+'"',
              '"name":  "'+Name+'"',
              '"begin": "'+PtnBegin+'"',
              '"end": "'+PtnEnd+'"',
              Extra
            ])+
            '}';
end;

function TTestTextMateGrammar.GetTestA1Foo: String;
begin
  Result := '{ "name":  "a1.foo",' +
            '  "match": "foo"' +
            '}';
end;

function TTestTextMateGrammar.GetTestA1(WithContentName: Boolean;
  ASubPatterns: array of String): String;
begin
  Result := '{ "name": "a1",';
  if WithContentName then Result := Result +
            '  "contentName": "a1.c",';
  Result := Result +
            '  "begin": "<a1.*?>",' +
            '  "end":   "</a1.*?>"' +
               BuildPatterns(ASubPatterns) +
            '}';
end;

function TTestTextMateGrammar.GetTestA1(WithContentName: Boolean; BCaptures,
  ECaptures, ACaptures: TCaptSet; B3Pattern, B5Pattern, E3Pattern, E5Pattern,
  ASubPatterns: array of String): String;
begin
  Result := '{ "name": "a1",';
  if WithContentName then Result := Result +
            '  "contentName": "a1.c",';
  Result := Result +
            '  "begin": "(<)(a1)([^->]+?)?-?(M*)(.*?)?(>)",' +
            '  "end":  "(<)(/a1)([^->]+?)?-?(M*)(.*?)?(>)",' +

            '  "beginCaptures": {';
  if c0 in BCaptures then Result := Result +' "0": { "name": "a1.b0" },';
  if c1 in BCaptures then Result := Result +' "1": { "name": "a1.b1" },';
  if c2 in BCaptures then Result := Result +' "2": { "name": "a1.b2" },';
  if c3 in BCaptures then begin
    Result := Result +' "3": { "name": "a1.b3",' +
    BuildPatterns(B3Pattern,
    '  { "name": "a1.b3.x", "match": "x*" },' +
    '  { "name": "a1.b3.y", "match": "y.*" },' +
    '  { "name": "a1.b3.z", "match": ".*z" }',
    False
    ) +
    ' },';
  end;
  if c4 in ECaptures then Result := Result +' "4": { "name": "a1.b4" },';
  if c5 in BCaptures then begin
    Result := Result +' "5": { ' +
    BuildPatterns(B5Pattern,
    '  { "name": "a1.b5.x", "match": "x*" },' +
    '  { "name": "a1.b5.y", "match": "y.*" },' +
    '  { "name": "a1.b5.z", "match": ".*z" }',
    False
    ) +
    ' },';
  end;
  if c6 in BCaptures then Result := Result +' "6": { "name": "a1.b6" },';
  Result := Result + '"99": { "name": "DUMMY"}' +
    '        },' +

    '      "endCaptures": {';
  if c0 in ECaptures then Result := Result +' "0": { "name": "a1.e0" },';
  if c1 in ECaptures then Result := Result +' "1": { "name": "a1.e1" },';
  if c2 in ECaptures then Result := Result +' "2": { "name": "a1.e2" },';
  if c3 in ECaptures then begin
    Result := Result +' "3": { "name": "a1.e3",' +
    BuildPatterns(E3Pattern,
    '  { "name": "a1.e3.x", "match": "x*" },' +
    '  { "name": "a1.e3.y", "match": "y.*" },' +
    '  { "name": "a1.e3.z", "match": ".*z" }',
    False
    ) +
    ' },';
  end;
  if c4 in ECaptures then Result := Result +' "4": { "name": "a1.e4" },';
  if c5 in ECaptures then begin
    Result := Result +' "5": { ' +
    BuildPatterns(E5Pattern,
    '  { "name": "a1.e5.x", "match": "x*" },' +
    '  { "name": "a1.e5.y", "match": "y.*" },' +
    '  { "name": "a1.e5.z", "match": ".*z" }',
    False
    ) +
    ' },';
  end;
  if c6 in ECaptures then Result := Result +' "6": { "name": "a1.e6" },';
  Result := Result + '"99": { "name": "DUMMY"}' +
    '        },' +

    '      "captures": {';
  if c0 in ACaptures then Result := Result +' "0": { "name": "a1.a0" },';
  if c1 in ACaptures then Result := Result +' "1": { "name": "a1.a1" },';
  if c2 in ACaptures then Result := Result +' "2": { "name": "a1.a2" },';
  if c3 in ACaptures then Result := Result +' "3": { "name": "a1.a3" },';
  if c4 in ACaptures then Result := Result +' "4": { "name": "a1.a4" },';
  if c5 in ACaptures then Result := Result +' "5": { "name": "a1.a5" },';
  if c6 in ACaptures then Result := Result +' "6": { "name": "a1.a6" },';
  Result := Result + '"99": { "name": "DUMMY"}' +
    '        }';

  Result := Result +
    BuildPatterns(ASubPatterns) +
    '}';
end;

function TTestTextMateGrammar.GetTestB1Bar(ASubPatterns: array of String
  ): String;
begin
  Result := '{ "name": "b1.bar",' +
            '  "match": "bar.*?end"' +
               BuildPatterns(ASubPatterns) +
            '}';
end;

function TTestTextMateGrammar.GetTestB1Nest(ASubPatterns: array of String
  ): String;
begin
  Result := '{ "name": "b1.nest",' +
            '  "begin": "nest",' +
            '  "end":   "back" ' +
               BuildPatterns(ASubPatterns) +
            '}';
end;

function TTestTextMateGrammar.GetTestB1(WithContentName, WithCaptures: Boolean;
  ASubPatterns: array of String): String;
begin
  Result := '{ "name": "b1",';
  if WithContentName then Result := Result +
            '  "contentName": "b1.c",';

  if WithCaptures then begin
    Result := Result +
      '      "begin": "(\\[)(b1)(\\])",' +
      '      "end":  "(\\[)(/b1)(\\])",' +
      '      "captures": {' +
      '         "1": { "name": "b1.a1" },' +
      '         "3": { "name": "b1.a3" }' +
      '        }';
  end
  else Result := Result +
    '      "begin": "\\[b1.*?\\]",' +
    '      "end": "\\[/b1.*?\\]"';

  Result := Result +
    BuildPatterns(ASubPatterns) +
    '}' ;
end;

function TTestTextMateGrammar.GetTestC1(ASubPatterns: array of String): String;
begin
  Result := '{ "name": "c1",' +
            '  "begin": "<c1>",' +
            '  "end":   "</c1>" ' +
               BuildPatterns(ASubPatterns) +
            '}';
end;

function TTestTextMateGrammar.GetTestM1(ASubPatterns: array of String): String;
begin
  Result := '{ "name": "m1",' +
            '  "match": "==>([^!]*).*$",' +
            '  "captures": {' +
            '    "1": { "name": "m1c" ' +
                         BuildPatterns(ASubPatterns) +
            '         },' +
            '  }' +
            '}';
end;

function TTestTextMateGrammar.GetTestW1(ASubPatterns: array of String): String;
begin
  Result := '{ "name": "w1",' +
            '  "begin": "W",' +
            '  "while":   "w" ' +
               BuildPatterns(ASubPatterns) +
            '}';
end;

function TTestTextMateGrammar.GetTestNest(ASubPatterns: array of String
  ): String;
begin
  Result := '{ ' +
               BuildPatterns(ASubPatterns, '', False) +
            '}';
end;

function TTestTextMateGrammar.Join(APatterns: array of String): String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Length(APatterns)-1 do begin
    if APatterns[i] = '' then
      Continue;
    if Result <> '' then Result := Result + ',';
    Result := Result + APatterns[i];
  end;
end;

function TTestTextMateGrammar.Include(const AName: String; SkipHash: Boolean
  ): String;
begin
  if SkipHash or (pos('#', AName) > 0) or (pos('$', AName) > 0) then
    Result := '{ "include": "'+AName+'" }'
  else
    Result := '{ "include": "#'+AName+'" }';
end;

function TTestTextMateGrammar.BuildPatterns(const APatterns: array of String;
  AMore: String; LeadComma: boolean): String;
begin
  Result := Join([AMore, Join(APatterns)]);
  if Result <> '' then
    Result := ' "patterns": [' + Result + ' ]';
  if (Result <> '') and LeadComma then
    Result := ',' + Result;
end;

function TTestTextMateGrammar.BuildCaptures(APatterns: array of String): String;
var
  i: Integer;
  n: String;
begin
  Result := '';
  for i := 0 to Length(APatterns) do begin
    n := APatterns[i];
    if n = '' then continue;
    if n[1] <> '{' then
      n := '{ "name": "' + n + '"}';
    Result := Join([Result, '"'+IntToStr(i+1)+'": ' + n]);
  end;
  Result := ' "captures": { ' + Result + ' }';
end;

procedure TTestTextMateGrammar.SetGrammar(AText: String);
begin
  FGrammar.ClearGrammar;
  FGrammarText := AText;
  FGrammar.ParseGrammar(AText);
  AssertTrue('no missing includes', FGrammar.MissingIncludes = '');

if FGrammar.ParserError = '' then
debugln(FGrammar.DebugDump());
end;

procedure TTestTextMateGrammar.SetGrammar(const ARootPatterns,
  ARepository: array of String);
var
  RepoPtn: String;
  i: Integer;
begin
  RepoPtn := '';
  for i := 0 to Length(ARepository) div 2 -1 do begin
    if RepoPtn <> '' then RepoPtn := RepoPtn + ',';
    RepoPtn := RepoPtn + '"' + ARepository[i*2] + '": ' + ARepository[i*2+1];
  end;

  SetGrammar(
    '{ "name": "root",'+
    '  "scopeName": "'+FNewScopeName+'" '+
    BuildPatterns(ARootPatterns) +
    ' , "repository": {' +
    RepoPtn +
    '  }' +
    '}'
  );
end;

procedure TTestTextMateGrammar.SetGrammar(const AScopeName: String;
  ARootPatterns, ARepository: array of String);
var
  t: TTextMateGrammar;
begin
  t := FGrammar;

  FNewScopeName := 'source.'+AScopeName;
  FGrammar := TTextMateGrammar.Create;
  FGrammarMap.Add(FNewScopeName, FGrammar);
  FGrammar.OnPopulateAttributeInfo := @DoPopulateAttributeInfo;
  FGrammar.OnCheckAttributeInfo := @DoCheckAttributeInfo;
  FGrammar.OnGetIncludedGrammar := @DoGetIncludedGrammar;

  SetGrammar(ARootPatterns, ARepository);

  FGrammar := t;
  FNewScopeName := 'source.maintest';
end;

function TTestTextMateGrammar.RunGrammar(ATestName, ATextLine: String; out
  LastPatternIndex: Integer; AStartPatternIndex: integer): String;
var
  LastPattern: TTextMatePattern;
begin
  Result := '';
  DebugLn(['TTestTextMateGrammar.RunGrammar START: ',ATextLine]);

  FGrammar.SetLine(ATextLine, AStartPatternIndex);
  FGrammar.First;
  Result := Result + IntToStr(FGrammar.CurrentTokenPos) + ':' + FNames[FGrammar.CurrentTokenKind];

  while not FGrammar.IsAtEol do begin
    FGrammar.Next;
    if not FGrammar.IsAtEol then
      Result := Result + ',' + IntToStr(FGrammar.CurrentTokenPos) + ':' + FNames[FGrammar.CurrentTokenKind];
  end;

  LastPatternIndex := FGrammar.CurrentPatternIndex;
  LastPattern := FGrammar.MainPatternList[LastPatternIndex];
  while LastPattern is TTextMatePatternForwarder do
    LastPattern := TTextMatePatternForwarder(LastPattern).ForwardTo;

  Result := Result + '/' +  TTextMatePatternBaseNested(LastPattern).Name;
  Result := Result + '/' +  IntToStr(FGrammar.CurrentState.StateIdx);
end;

procedure TTestTextMateGrammar.RunNextToEol(ATestName, ATextLine: String; out
  LastPatternIndex: Integer; AStartPatternIndex: integer);
begin
  DebugLn(['TTestTextMateGrammar.RunNextToEol START: ',ATextLine]);

  FGrammar.SetLine(ATextLine, AStartPatternIndex);
  FGrammar.NextToEol;
  AssertTrue(ATestName+': at eol', FGrammar.IsAtEol);

  LastPatternIndex := FGrammar.CurrentPatternIndex;
end;

function TTestTextMateGrammar.TestLine(ATestName, ATextLine, Expect: String;
  AStartPatternIndex: integer): Integer;
var
  t: String;
  p, LastIdx, d: Integer;
begin
  t := RunGrammar(ATestName, ATextLine, p, AStartPatternIndex);
  DebugLn(['--- Line: ''', ATextLine, '      // ',ATestName]);
  DebugLn(['Expect: ''', Expect, '''']);
  DebugLn(['   GOT: ''', t, '''']);
  AssertEquals(ATestName+' Result', Expect, t);

  t := RunGrammar(ATestName, ATextLine, Result, AStartPatternIndex);
  AssertEquals(ATestName+' Result', Expect, t); // run twice // no interaction between runs
  AssertEquals(ATestName+' LastId', PtrUInt(p), PtrUInt(Result));
  d := FGrammar.CurrentState.StateIdx;

  RunNextToEol(ATestName, ATextLine, LastIdx, AStartPatternIndex);
  AssertEquals(ATestName+' NextToEol LastId', PtrUInt(p), PtrUInt(LastIdx));
  AssertEquals(ATestName+' NextToEol StateIdx', d, FGrammar.CurrentState.StateIdx);


  t := RunGrammar(ATestName, ATextLine, Result, AStartPatternIndex);
  AssertEquals(ATestName+' Result', Expect, t); // run twice // no interaction between runs
  AssertEquals(ATestName+' LastId', PtrUInt(p), PtrUInt(Result));
end;

function TTestTextMateGrammar.TestLine(ATestName, ATextLine, Expect: String;
  AStartPatternName: String): Integer;
var
  AStartPattern: TTextMatePattern;
  i: Integer;
begin
  AStartPattern := nil;
  for i := 0 to FGrammar.MainPatternList.Count - 1 do
    if (FGrammar.MainPatternList[i] is TTextMatePatternBaseNested) and
       (TTextMatePatternBaseNested(FGrammar.MainPatternList[i]).Name = AStartPatternName)
    then begin
      AStartPattern := FGrammar.MainPatternList[i];
      break;
    end;
  AssertTrue(ATestName+': Found AStartPattern', AStartPattern <> nil);

  Result := TestLine(ATestName, ATextLine, Expect, AStartPattern.Index);
end;

procedure TTestTextMateGrammar.SetUp;
begin
  inherited SetUp;
  FNewScopeName := 'source.maintest';
  FGrammarMap := TGrammarMap.Create(True);
  FGrammar := TTextMateGrammar.Create;
  FGrammarMap.Add(FNewScopeName, FGrammar);
  FGrammar.OnPopulateAttributeInfo := @DoPopulateAttributeInfo;
  FGrammar.OnCheckAttributeInfo := @DoCheckAttributeInfo;
  FGrammar.OnGetIncludedGrammar := @DoGetIncludedGrammar;
  FNames := TStringList.Create;
end;

procedure TTestTextMateGrammar.TearDown;
begin
  inherited TearDown;
  //FGrammar.Free;
  FGrammarMap.Free;
  FNames.Free;
end;

procedure TTestTextMateGrammar.ResetGramarMap;
begin
  TearDown;
  SetUp;
end;

procedure TTestTextMateGrammar.TestFlatNested;
begin
  SetGrammar([GetTestA1(False, [ GetTestNest( [GetTestC1([]) ] ),
                                 GetTestNest( [GetTestC1([]), GetTestB1(False, False, []) ] ),
                                 Include('X1')
                               ])
             ],
             [ 'X1', GetTestNest( [GetTestC1([]), GetTestB1(False, False, []) ] )
             ]);
  AssertEquals('no error', FGrammar.ParserError, '');

  SetGrammar([Include('X1')
             ],
             [ 'X1', GetTestNest( [GetTestC1([]), Include('X1')] )
             ]);
  AssertFalse('got error', FGrammar.ParserError = '');

  SetGrammar([Include('X1')
             ],
             [ 'X1', GetTestNest( [Include('X1')] )
             ]);
  AssertFalse('got error', FGrammar.ParserError = '');

  SetGrammar([Include('X1')
             ],
             [ 'X1', GetTestNest( [Include('Y1')] ),
               'Y1', GetTestNest( [Include('X1')] )
             ]);
  AssertFalse('got error', FGrammar.ParserError = '');

  SetGrammar([Include('X1')
             ],
             [ 'X1', GetTestNest( [GetTestC1([]), Include('Y1')] ),
               'Y1', GetTestNest( [GetTestC1([]), Include('X1')] )
             ]);
  AssertFalse('got error', FGrammar.ParserError = '');

end;

procedure TTestTextMateGrammar.TestInclude;
begin
  SetGrammar(
    [ Include('Foo') ],
    [ 'Foo', GetTestA1(False, [Include('Bar')]),
      'Bar', GetTestC1([])
    ]
  );
  TestLine('simple',         'a<a1><c1></c1></a1>c', '1:-,2:a1,6:c1,15:a1,20:-/root/0');

  SetGrammar(
    [ Include('$self#Foo') ],
    [ 'Foo', GetTestA1(False, [Include('$self#Bar')]),
      'Bar', GetTestC1([])
    ]
  );
  TestLine('simple',         'a<a1><c1></c1></a1>c', '1:-,2:a1,6:c1,15:a1,20:-/root/0');

  SetGrammar(
    [ Include('$base#Foo') ],
    [ 'Foo', GetTestA1(False, [Include('$base#Bar')]),
      'Bar', GetTestC1([])
    ]
  );
  TestLine('simple',         'a<a1><c1></c1></a1>c', '1:-,2:a1,6:c1,15:a1,20:-/root/0');


  // Forward include
  SetGrammar(
    [ Include('FooX') ],
    [ 'AFoo', Include('Foo'),
      'Foo',  GetTestA1(False, [Include('BarX')]),
      'FooX', Include('AFoo'),
      'ABar', Include('Bar'),
      'Bar',  GetTestC1([]),
      'BarX', Include('ABar')
    ]
  );
  TestLine('simple',         'a<a1><c1></c1></a1>c', '1:-,2:a1,6:c1,15:a1,20:-/root/0');


  // Full self include
  SetGrammar(
    [ Include('Foo'), GetTestC1([]) ],
    [ 'Foo', GetTestA1(False, [Include('$self')])
    ]
  );
  TestLine('simple',         'a<a1><c1></c1></a1>c', '1:-,2:a1,6:c1,15:a1,20:-/root/0');
  TestLine('simple',         'a<a1><a1>', '1:-,2:a1,6:a1/a1/2');

  SetGrammar(
    [ Include('Foo'), GetTestC1([]) ],
    [ 'Foo', GetTestA1(False, [Include('$base')])
    ]
  );
  TestLine('simple',         'a<a1><c1></c1></a1>c', '1:-,2:a1,6:c1,15:a1,20:-/root/0');
  TestLine('simple',         'a<a1><a1>', '1:-,2:a1,6:a1/a1/2');


end;

procedure TTestTextMateGrammar.TestBeginEnd;

  procedure TestNoCtxName;
  begin
    TestLine('simple',         'a<a1>b</a1>c', '1:-,2:a1,12:-/root/0');
    TestLine('Repeat',         'a<a1>b</a1>c<a1>d</a1>e', '1:-,2:a1,12:-,13:a1,23:-/root/0'); // repeat
    TestLine('No Gap',         'a<a1>b</a1><a1>d</a1>e',  '1:-,2:a1,12:a1,22:-/root/0');     // repeat, no gap
    TestLine('At EOL/BOL',     '<a1>b</a1>',   '1:a1/root/0');  // line start/end
    TestLine('immediate End',  'a<a1></a1>c',  '1:-,2:a1,11:-/root/0'); // empty

    TestLine('M-L ', 'a<a1>b',  '1:-,2:a1/a1/1');               // multilnie
    TestLine('M-L ', 'b',       '1:a1/a1/1',          'a1');
    TestLine('M-L ', 'b</a1>c', '1:a1,7:-/root/0',  'a1');
    TestLine('M-L ', 'a<a1>',   '1:-,2:a1/a1/1');               // multilnie bounds
    TestLine('M-L ', 'b',       '1:a1/a1/1',          'a1');
    TestLine('M-L ', '</a1>c',  '1:a1,6:-/root/0',  'a1');

    TestLine('no foo in match',     'a<a1 foo>b</a1 foo>c', '1:-,2:a1,20:-/root/0');

    TestLine('pattern',          'a<a1> foo </a1>c',     '1:-,2:a1,7:a1.foo,10:a1,16:-/root/0'); // pattern
    TestLine('pattern no-space', 'a<a1>foo</a1>c',       '1:-,2:a1,6:a1.foo,9:a1,14:-/root/0'); // pattern no space
    TestLine('pattern twice',    'a<a1> foo foo </a1>c', '1:-,2:a1,7:a1.foo,10:a1,11:a1.foo,14:a1,20:-/root/0'); // pattern / twice
    TestLine('pattern twice-s',  'a<a1>foo foo</a1>c',   '1:-,2:a1,6:a1.foo,9:a1,10:a1.foo,13:a1,18:-/root/0'); // pattern no space / twice
    TestLine('pattern twice-sg', 'a<a1>foofoo</a1>c',    '1:-,2:a1,6:a1.foo,9:a1.foo,12:a1,17:-/root/0'); // pattern no space / twice / no gap
    TestLine('M-L pattern',   'a<a1>foo',    '1:-,2:a1,6:a1.foo/a1/1');  // multilnie pattern
    TestLine('M-L pattern',   'foo',         '1:a1.foo/a1/1',               'a1');
    TestLine('M-L pattern',   ' foofoo',     '1:a1,2:a1.foo,5:a1.foo/a1/1',      'a1');
    TestLine('M-L pattern',   'foo</a1>foo', '1:a1.foo,4:a1,9:-/root/0',  'a1');

    TestLine('B', '<a1> [b1] [/b1] </a1>',           '1:a1,6:b1,16:a1/root/0'); // pattern B
    TestLine('B', '<a1> [b1]foo[/b1] </a1>',         '1:a1,6:b1,18:a1/root/0'); // pattern B / no foo
    TestLine('B', '<a1> [b1]bar end[/b1] </a1>',     '1:a1,6:b1,10:b1.bar,17:b1,22:a1/root/0'); // pattern B / bar end
    TestLine('B', '<a1> [b1]bar[/b1]end[/b1] </a1>', '1:a1,6:b1,10:b1.bar,21:b1,26:a1/root/0'); // pattern B / bar end overlap
    TestLine('B', '<a1> [b1] </a1> [/b1] </a1>',     '1:a1,6:b1,22:a1/root/0'); // pattern B overlap
    TestLine('B', '<a1>[b1]</a1>[/b1]</a1>',         '1:a1,5:b1,19:a1/root/0'); // pattern B overlap no space

    TestLine('', '<a1>',                   '1:a1/a1/1'); // multiline
    TestLine('', '[b1]',                   '1:b1/b1/2',                       'a1');
    TestLine('', 'foo',                    '1:b1/b1/2',                       'b1');
    TestLine('', 'nest<a1>[/b1]</a1>back', '1:b1.nest,5:a1,19:b1.nest/b1/2',  'b1');
    TestLine('', 'B[/b1]bar end',          '1:b1,7:a1/a1/1',                  'b1');
    TestLine('', 'foo</a1>foo',            '1:a1.foo,4:a1,9:-/root/0',      'a1'); // pattern B / no foo // A foo

    TestLine('', 'a<a1 A-A> foo </a1 A-A>c',       '1:-,2:a1,11:a1.foo,14:a1,24:-/root/0');
    TestLine('', 'a<a1 Axnn-A> foo </a1 xnn-A>c',  '1:-,2:a1,14:a1.foo,17:a1,29:-/root/0');
    TestLine('', 'a<a1 nnyA-A> foo </a1 A-nny>c',  '1:-,2:a1,14:a1.foo,17:a1,29:-/root/0');
    TestLine('', 'a<a1 Az-A> foo </a1 A-zA>c',     '1:-,2:a1,12:a1.foo,15:a1,26:-/root/0');
    TestLine('', 'a<a1 A-xnnA> foo </a1 A-Axnn>c', '1:-,2:a1,14:a1.foo,17:a1,30:-/root/0');
    TestLine('', 'a<a1 A-Anny> foo </a1 A-nnyA>c', '1:-,2:a1,14:a1.foo,17:a1,30:-/root/0');
    TestLine('', 'a<a1 A-z> foo </a1 A-z>c',       '1:-,2:a1,11:a1.foo,14:a1,24:-/root/0');
    TestLine('', 'a<a1-y> foo </a1-A>c',           '1:-,2:a1,9:a1.foo,12:a1,20:-/root/0');
    TestLine('', 'a<a1-> foo </a1->c',             '1:-,2:a1,8:a1.foo,11:a1,18:-/root/0');
  end;

begin

  SetGrammar(
    [ GetTestA1(False,
      [ GetTestA1Foo,
        GetTestB1(False, False,
        [ GetTestB1Bar([]),
          GetTestB1Nest([ GetTestA1(False, []) ])
        ])
      ])
    ],
    []
  );
  TestNoCtxName;

  SetGrammar(
    [ Include('RepoA1')
    ],
    [ 'RepoA1', GetTestA1(False,
                [ GetTestA1Foo,
                  GetTestB1(False, False,
                  [ GetTestB1Bar([]),
                    GetTestB1Nest([ GetTestA1(False, []) ])
                  ])
                ])
    ]
  );
  TestNoCtxName;

  SetGrammar(
    [ Include('RepoA1')
    ],
    [ 'RepoA1', GetTestA1(False,
                [ GetTestA1Foo,
                  Include('RepoB1')
                ]),
      'RepoB1', GetTestB1(False, False,
                  [ GetTestB1Bar([]),
                    GetTestB1Nest([ Include('RepoA1Sub') ])
                  ]),
      'RepoA1Sub', GetTestA1(False, [])
    ]
  );
  TestNoCtxName;



  // With Content name
  SetGrammar(
    [ GetTestA1(True,
      [ GetTestA1Foo,
        GetTestB1(False, False,
        [ GetTestB1Bar([]),
          GetTestB1Nest([ GetTestA1(False, []) ])
        ])
      ])
    ],
    []
  );
    TestLine('simple',         'a<a1>b</a1>c', '1:-,2:a1,6:a1.c,7:a1,12:-/root/0');
    TestLine('Repeat',         'a<a1>b</a1>c<a1>d</a1>e', '1:-,2:a1,6:a1.c,7:a1,12:-,13:a1,17:a1.c,18:a1,23:-/root/0'); // repeat
    TestLine('immediate End',  'a<a1></a1>c',  '1:-,2:a1,11:-/root/0'); // empty

    TestLine('M-L ', 'a<a1>b',  '1:-,2:a1,6:a1.c/a1/1');               // multilnie
    TestLine('M-L ', 'b',       '1:a1.c/a1/1',          'a1');
    TestLine('M-L ', 'b</a1>c', '1:a1.c,2:a1,7:-/root/0',  'a1');
    TestLine('M-L ', 'a<a1>',   '1:-,2:a1/a1/1');               // multilnie bounds
    TestLine('M-L ', 'b',       '1:a1.c/a1/1',          'a1');
    TestLine('M-L ', '</a1>c',  '1:a1,6:-/root/0',  'a1');

    TestLine('no foo in match',     'a<a1 foo>b</a1 foo>c', '1:-,2:a1,10:a1.c,11:a1,20:-/root/0');

    TestLine('pattern',          'a<a1> foo </a1>c',     '1:-,2:a1,6:a1.c,7:a1.foo,10:a1.c,11:a1,16:-/root/0'); // pattern
    TestLine('pattern no-space', 'a<a1>foo</a1>c',       '1:-,2:a1,6:a1.foo,9:a1,14:-/root/0'); // pattern no space
    TestLine('pattern twice',    'a<a1> foo foo </a1>c', '1:-,2:a1,6:a1.c,7:a1.foo,10:a1.c,11:a1.foo,14:a1.c,15:a1,20:-/root/0'); // pattern / twice
    TestLine('pattern twice-s',  'a<a1>foo foo</a1>c',   '1:-,2:a1,6:a1.foo,9:a1.c,10:a1.foo,13:a1,18:-/root/0'); // pattern no space / twice
    TestLine('pattern twice-sg', 'a<a1>foofoo</a1>c',    '1:-,2:a1,6:a1.foo,9:a1.foo,12:a1,17:-/root/0'); // pattern no space / twice / no gap
    TestLine('M-L pattern',   'a<a1>foo',    '1:-,2:a1,6:a1.foo/a1/1');  // multilnie pattern
    TestLine('M-L pattern',   'foo',         '1:a1.foo/a1/1',               'a1');
    TestLine('M-L pattern',   ' foofoo',     '1:a1.c,2:a1.foo,5:a1.foo/a1/1',      'a1');
    TestLine('M-L pattern',   'foo</a1>foo', '1:a1.foo,4:a1,9:-/root/0',  'a1');


  // capture at end
  SetGrammar(
    [ GetTestA1(False, [], [c6], [],
                [], [], [], [],
                [])
    ],
    []
  );
  TestLine('capt at end',  '<a1>b</a1>', '1:a1,10:a1.e6/root/0');

  // capture at end
  SetGrammar(
    [ GetTestA1(False, [c6], [c6], [],
                [], [], [], [],
                [])
    ],
    []
  );
  TestLine('capt at end',  '<a1>', '1:a1,4:a1.b6/a1/1');

  // capture at end
  SetGrammar(
    [ GetTestA1(False, [], [c5,c6], [],
                [], [], [], [GetTestB1(False, False, [])],
                [])
    ],
    []
  );
  TestLine('capt at end',  '<a1>b</a1-M[b1]>', '1:a1,12:b1,16:a1.e6/root/0');


  SetGrammar([
      BuildBeginEnd('comment', '//(.*)', '(?=$)', BuildCaptures(['slash'])),
      BuildBeginEnd('comment2', '(!!).*', '(?=$)', BuildCaptures(['c2'])),
      BuildBeginEnd('comment3', '(\\?\\?)', '(?=$)', BuildCaptures(['c3'])),
      BuildMatch('foo', 'foo')
    ], []);

  TestLine('// abc foo',  '// abc foo', '1:comment,3:slash/root/0');
  TestLine('!!a',  '!!a', '1:c2,3:comment2/root/0');
  TestLine('??a',  '??a', '1:c3,3:comment3/root/0');

end;

procedure TTestTextMateGrammar.TestBeginEndInCapture;
begin
  SetGrammar(
    [ GetTestM1([ GetTestA1(False, [ GetTestA1Foo ] ) ])
    ],
    []
  );

  TestLine('M1', 'a==><a1>b</a1>c',  '1:-,2:m1,5:a1,15:m1c/root/0');

  TestLine('M1', 'a==><a1>b!c',  '1:-,2:m1,5:a1,10:m1/root/0'); // "a1" did return to m1
  TestLine('M1', 'a==><a1>b',    '1:-,2:m1,5:a1/root/0'); // "a1" and "m1" did return
end;

procedure TTestTextMateGrammar.TestWhile;
var
  p: Integer;
begin
  SetGrammar(
    [ GetTestW1([])
    ],
    []
  );

  p := TestLine('M1', 'a',  '1:-/root/0');
  p := TestLine('M1', 'w',  '1:-/root/0',    p);
  p := TestLine('M1', '_W', '1:-,2:w1/w1/1', p);
  p := TestLine('M1', 'w',  '1:w1/w1/1',     p);
  p := TestLine('M1', '_w', '1:w1/w1/1',     p);
  p := TestLine('M1', 'a',  '1:-/root/0',    p);
  p := TestLine('M1', 'w',  '1:-/root/0',    p);
end;

procedure TTestTextMateGrammar.TestForwarder;
var
  p: Integer;
begin
  SetGrammar(
    [ Include('RepoA1'), Include('RepoB1'), Include('RepoC1')
    ],
    [ 'RepoA1', GetTestA1(False, [ GetTestA1Foo ] ),
      'RepoB1', GetTestB1(False, False,
                  [ GetTestB1Bar([]),
                    Include('RepoB1Nest')
                  ]),
      'RepoC1', GetTestC1(
                  [ Include('RepoB1'),
                    Include('RepoB1Nest'),
                    GetTestB1Nest([ Include('RepoA1Dup') ])
                  ]),
      'RepoA1Dup', GetTestA1(True, [ Include('RepoA1'), Include('RepoB1') ] ),
      'RepoB1Nest', GetTestB1Nest([ Include('RepoA1') ])

    ]
  );

  TestLine('A1',         'a<a1>b</a1>c', '1:-,2:a1,12:-/root/0');
  p := TestLine('A1 M-L ', 'a<a1>b',  '1:-,2:a1/a1/1');
  p := TestLine('A1 M-L ', 'bfoob',       '1:a1,2:a1.foo,5:a1/a1/1',          p);
  p := TestLine('A1 M-L ', 'b</a1>c', '1:a1,7:-/root/0',                      p);

  TestLine('B1 A1',   'a[b1]foo nest foo<a1>nest foo back</a1>bar back bar <a1> end[/b1]nest foo back',
                 '1:-,2:b1,10:b1.nest,18:a1,27:a1.foo,30:a1,40:b1.nest,48:b1,49:b1.bar,61:b1,66:-/root/0');
  p := TestLine('M-L B1 A1',  'a',                '1:-/root/0');
  p := TestLine('M-L B1 A1',  '[b1]',             '1:b1/b1/1',                p);
  p := TestLine('M-L B1 A1',  'foo',              '1:b1/b1/1',                p);
  p := TestLine('M-L B1 A1',  'nest',             '1:b1.nest/b1.nest/2',      p);
  p := TestLine('M-L B1 A1',  'foo',              '1:b1.nest/b1.nest/2',      p);
  p := TestLine('M-L B1 A1',  '<a1>',             '1:a1/a1/3',                p);
  p := TestLine('M-L B1 A1',  'nest foo',         '1:a1,6:a1.foo/a1/3',       p);
  p := TestLine('M-L B1 A1',  'back',             '1:a1/a1/3',                p);
  p := TestLine('M-L B1 A1',  '</a1>',            '1:a1/b1.nest/2',           p);
  p := TestLine('M-L B1 A1',  'x<a1>',            '1:b1.nest,2:a1/a1/3',      p);
  p := TestLine('M-L B1 A1',  'x</a1>x',          '1:a1,7:b1.nest/b1.nest/2', p);
  p := TestLine('M-L B1 A1',  'bar',              '1:b1.nest/b1.nest/2',      p);
  p := TestLine('M-L B1 A1',  'back',             '1:b1.nest/b1/1',           p);
  p := TestLine('M-L B1 A1',  'bar <a1> end',     '1:b1.bar/b1/1',            p);
  p := TestLine('M-L B1 A1',  '[/b1]bar foo end', '1:b1,6:-/root/0',          p);


  // Get to B1Nest via all the diff paths:
  //   B1 -> B1Nest -> A1
  //   C1 -> B1Nest -> A1
  //   C1 -> B1 -> B1Nest -> A1
  p := TestLine('M-L B1 A1',  '[b1]',      '1:b1/b1/1');
  p := TestLine('M-L B1 A1',  'nest',      '1:b1.nest/b1.nest/2',           p);
  p := TestLine('M-L B1 A1',  '<a1>',      '1:a1/a1/3',                     p);
  p := TestLine('M-L B1 A1',  '</a1>',     '1:a1/b1.nest/2',                p);
  p := TestLine('M-L B1 A1',  'back',      '1:b1.nest/b1/1',                p);
  p := TestLine('M-L B1 A1',  '[/b1]',     '1:b1/root/0',                   p);

  p := TestLine('M-L B1 A1',  '<c1>',      '1:c1/c1/1');
  p := TestLine('M-L B1 A1',  'nest',      '1:b1.nest/b1.nest/2',           p);
  p := TestLine('M-L B1 A1',  '<a1>',      '1:a1/a1/3',                     p);
  p := TestLine('M-L B1 A1',  '</a1>',     '1:a1/b1.nest/2',                p);
  p := TestLine('M-L B1 A1',  'back',      '1:b1.nest/c1/1',                p);
  p := TestLine('M-L B1 A1',  '</c1>',     '1:c1/root/0',                   p);

  p := TestLine('M-L B1 A1',  '<c1>',      '1:c1/c1/1');
  p := TestLine('M-L B1 A1',  '[b1]',      '1:b1/b1/2',                     p);
  p := TestLine('M-L B1 A1',  'nest',      '1:b1.nest/b1.nest/3',           p);
  p := TestLine('M-L B1 A1',  '<a1>',      '1:a1/a1/4',                     p);
  p := TestLine('M-L B1 A1',  '</a1>',     '1:a1/b1.nest/3',                p);
  p := TestLine('M-L B1 A1',  'back',      '1:b1.nest/b1/2',                p);
  p := TestLine('M-L B1 A1',  '[/b1]',     '1:b1/c1/1',                     p);
  p := TestLine('M-L B1 A1',  '</c1>',     '1:c1/root/0',                   p);


end;

procedure TTestTextMateGrammar.TestRecurse;
var
  p, p1, p2, p3, p4, p5, p6, p7, p8, p9, pa, pb, pc, pd, pe, pf, pg: Integer;
begin
  SetGrammar(
    [ Include('RepoA1')
    ],
    [ 'RepoA1', GetTestA1(True, [ Include('RepoB1') ] ),
      'RepoB1', GetTestB1(True, False, [ Include('RepoC1') ]),
      'RepoC1', GetTestC1([ Include('RepoA1') ])
    ]
  );

  p1 := TestLine('A1 M-L ', 'a<a1>b',  '1:-,2:a1,6:a1.c/a1/1');
  p2 := TestLine('A1 M-L ', 'a[b1]b',  '1:a1.c,2:b1,6:b1.c/b1/2',  p1);
  p3 := TestLine('A1 M-L ', 'a<c1>b',  '1:b1.c,2:c1/c1/3',         p2);
  p4 := TestLine('A1 M-L ', 'a<a1>b',  '1:c1,2:a1,6:a1.c/a1/4',    p3);
  p5 :=  TestLine('A1 M-L ', 'a<a1>b',  '1:a1.c/a1/4',              p4);
  AssertEquals(p4,p5);
  p6 :=  TestLine('A1 M-L ', 'a<c1>b',  '1:a1.c/a1/4',              p5);
  AssertEquals(p4,p6);
  p7 := TestLine('A1 M-L ', 'a[b1]b',  '1:a1.c,2:b1,6:b1.c/b1/5',  p6);
  p8 := TestLine('A1 M-L ', 'a<c1>b',  '1:b1.c,2:c1/c1/6',         p7);
  p9 := TestLine('A1 M-L ', 'a<a1>b',  '1:c1,2:a1,6:a1.c/a1/7',    p8);
  pa := TestLine('A1 M-L ', 'a</a1>b', '1:a1.c,2:a1,7:c1/c1/6',    p9);
  pb := TestLine('A1 M-L ', 'a</c1>b', '1:c1,7:b1.c/b1/5',         pa);
  pc := TestLine('A1 M-L ', 'a[/b1]b', '1:b1.c,2:b1,7:a1.c/a1/4',  pb);
  pd := TestLine('A1 M-L ', 'a</a1>b', '1:a1.c,2:a1,7:c1/c1/3',    pc);
  pe := TestLine('A1 M-L ', 'a</c1>b', '1:c1,7:b1.c/b1/2',         pd);
  pf := TestLine('A1 M-L ', 'a[/b1]b', '1:b1.c,2:b1,7:a1.c/a1/1',  pe);
  pg := TestLine('A1 M-L ', 'a</a1>b', '1:a1.c,2:a1,7:-/root/0',   pf);
  AssertEquals(p1,pf);
  AssertEquals(p2,pe);
  AssertEquals(p3,pd);
  AssertEquals(p4,pc);
  AssertEquals(p7,pb);
  AssertEquals(p8,pa);

  p := TestLine('A1 M-L ', 'a<a1>b',  '1:-,2:a1,6:a1.c/a1/1');
  AssertEquals(p1,p);
  p := TestLine('A1 M-L ', 'a[b1]b',  '1:a1.c,2:b1,6:b1.c/b1/2',  p);
  AssertEquals(p2,p);
  p := TestLine('A1 M-L ', 'a<c1>b',  '1:b1.c,2:c1/c1/3',         p);
  AssertEquals(p3,p);
  p := TestLine('A1 M-L ', 'a<a1>b',  '1:c1,2:a1,6:a1.c/a1/4',    p);
  AssertEquals(p4,p);
  p := TestLine('A1 M-L ', 'a<a1>b',  '1:a1.c/a1/4',              p);
  AssertEquals(p5,p);
  p := TestLine('A1 M-L ', 'a<c1>b',  '1:a1.c/a1/4',              p);
  AssertEquals(p6,p);
  p := TestLine('A1 M-L ', 'a[b1]b',  '1:a1.c,2:b1,6:b1.c/b1/5',  p);
  AssertEquals(p7,p);
  p := TestLine('A1 M-L ', 'a<c1>b',  '1:b1.c,2:c1/c1/6',         p);
  AssertEquals(p8,p);
  p := TestLine('A1 M-L ', 'a<a1>b',  '1:c1,2:a1,6:a1.c/a1/7',    p);
  AssertEquals(p9,p);
  p := TestLine('A1 M-L ', 'a</a1>b', '1:a1.c,2:a1,7:c1/c1/6',    p);
  AssertEquals(pa,p);
  p := TestLine('A1 M-L ', 'a</c1>b', '1:c1,7:b1.c/b1/5',         p);
  AssertEquals(pb,p);
  p := TestLine('A1 M-L ', 'a[/b1]b', '1:b1.c,2:b1,7:a1.c/a1/4',  p);
  AssertEquals(pc,p);
  p := TestLine('A1 M-L ', 'a</a1>b', '1:a1.c,2:a1,7:c1/c1/3',    p);
  AssertEquals(pd,p);
  p := TestLine('A1 M-L ', 'a</c1>b', '1:c1,7:b1.c/b1/2',         p);
  AssertEquals(pe,p);
  p := TestLine('A1 M-L ', 'a[/b1]b', '1:b1.c,2:b1,7:a1.c/a1/1',  p);
  AssertEquals(pf,p);
  p := TestLine('A1 M-L ', 'a</a1>b', '1:a1.c,2:a1,7:-/root/0',   p);
  AssertEquals(pg,p);

  SetGrammar(
    [ Include('RepoA1')
    ],
    [ 'RepoA1', GetTestA1(True, [ Include('RepoB1') ] ),
      'RepoB1', GetTestB1(False, False, [ Include('RepoC1'), Include('RepoB1Nest') ]),
      'RepoC1', GetTestC1([ Include('RepoA1') ]),

      'RepoB1Nest', GetTestB1Nest([ Include('RepoA1') ])
    ]
  );

  SetGrammar(
    [ Include('RepoA1')
    ],
    [ 'RepoA1', GetTestA1(True, [ Include('RepoB1') ] ),
      'RepoB1', GetTestB1(False, False, [ Include('RepoC1'), Include('RepoB1Nest') ]),
      'RepoC1', GetTestC1([ Include('RepoA1') ]),

      'RepoB1Nest', GetTestB1Nest([ Include('RepoB1') ])
    ]
  );


  SetGrammar(
    [ Include('RepoA1')
    ],
    [ 'RepoA1', GetTestA1(True, [ Include('RepoA1') ] )
    ]
  );

  TestLine('A1',  'a<a1>c<a1>b</a1>c</a1>c', '1:-,2:a1,6:a1.c,7:a1,11:a1.c,12:a1,17:a1.c,18:a1,23:-/root/0');

  p := TestLine('A1 M-L ', 'a<a1>b',  '1:-,2:a1,6:a1.c/a1/1');
  p := TestLine('A1 M-L ', 'a<a1>b',  '1:a1.c,2:a1,6:a1.c/a1/2',  p);
  p := TestLine('A1 M-L ', 'a</a1>b',  '1:a1.c,2:a1,7:a1.c/a1/1', p);

  SetGrammar(
    [ Include('RepoA1'), Include('RepoB1')
    ],
    [ 'RepoA1', GetTestA1(True, [ Include('RepoB1') ] ),
      'RepoB1', GetTestB1(False, False, [ Include('RepoA1') ])
    ]
  );



  SetGrammar(
    [ Include('RepoA1'), Include('RepoB1'), Include('RepoC1')
    ],
    [ 'RepoA1', GetTestA1(False, [ GetTestA1Foo,
                                   Include('RepoB1')
                                 ] ),
      'RepoB1', GetTestB1(False, False,
                  [ GetTestB1Bar([]),
                    Include('RepoB1Nest'),
                    Include('RepoC1')
                  ]),
      'RepoC1', GetTestC1(
                  [ Include('RepoB1'),
                    Include('RepoB1Nest'),
                    GetTestB1Nest([ Include('RepoA1Dup') ])
                  ]),
      'RepoA1Dup', GetTestA1(True, [ Include('RepoA1'), Include('RepoB1') ] ),
      'RepoB1Nest', GetTestB1Nest([ Include('RepoA1') ])

    ]
  );


end;

procedure TTestTextMateGrammar.TestIncludeOtherGrammar;
begin
  SetGrammar(
    [ Include('source.foo', True)
    ],
    [ 'Main1', Include('source.foo#FooA1', True),
      'Main2', Include('source.foo#FooA2', True),
      'RepoC1', GetTestC1([])
    ]
  );

  SetGrammar('foo',
    [ Include('source.maintest#Main1', True)
    ],
    [ 'FooA1', Include('source.maintest#Main2', True),
      'FooA2', Include('source.maintest#RepoC1', True),
      'RepoA1', GetTestA1(False, [])
    ]
  );
  FGrammar.ResolveExternalIncludes;
  AssertTrue('no missing includes', FGrammar.MissingIncludes = '');
writeln( '###########');
writeln( FGrammar.DebugDump());

  TestLine('', '<a1><c1>',  '1:-,5:c1/c1/1');




  ///////////////
  /// miss incl
  ResetGramarMap;
  SetGrammar(
    [ Include('source.foo', True), GetTestC1([])  ],
    []
  );
  FGrammar.ResolveExternalIncludes;
  AssertTrue('missing includes', pos('source.foo',FGrammar.MissingIncludes) > 0);


  ResetGramarMap;
  SetGrammar(
    [ Include('source.foo#Bar', True), GetTestC1([])  ],
    []
  );
  SetGrammar('foo',
    [ GetTestC1([]) ],
    [ 'RepoA1', GetTestA1(False, [])
    ]
  );
  FGrammar.ResolveExternalIncludes;
  AssertTrue('missing includes', pos('source.foo#Bar',FGrammar.MissingIncludes) > 0);


  ResetGramarMap;
  SetGrammar(
    [ Include('source.foo#RepoA1', True), GetTestC1([])  ],
    []
  );
  SetGrammar('foo',
    [ GetTestC1([]) ],
    [ 'RepoA1', GetTestA1(False, [Include('source.foo#What', True)])
    ]
  );
  FGrammar.ResolveExternalIncludes;
  AssertTrue('missing includes', pos('source.foo#What',FGrammar.MissingIncludes) > 0);

  ///////////////
  /// recursions

  ResetGramarMap;
  SetGrammar(
    [ Include('source.foo#RepoA1', True)
    ],
    [ 'RepoA1', Include('source.foo', True),
      'RepoB1', Include('source.foo#RepoB1', True)
    ]
  );
  SetGrammar('foo',
    [ Include('#RepoB1', True)
    ],
    [ 'RepoA1', Include('source.maintest#RepoB1', True),
      'RepoB1', Include('source.maintest#RepoA1', True)
    ]
  );
  FGrammar.ResolveExternalIncludes;
  AssertTrue('Got error', FGrammar.ParserError <> '');

end;

procedure TTestTextMateGrammar.TestVarious;
var
  p: Integer;
begin
  SetGrammar(
    [ '{ "name": "t1", "begin": "(^[ \\t]+)?(?=//)", "end":   "(?!\\G)" ' +
      BuildPatterns(['{ "name": "t2", "begin": "//", "end":   "$" }']) +
      '}'
    ],
    []
  );

  p := TestLine('t1', '  // abc',  '1:t1,3:t2/t1/1');
  p := TestLine('t1', '',          '1:t1/t1/1',  p);

  FGrammar.SetLine('  // abc', -1);
  FGrammar.NextToEol;
  FGrammar.SetLine('', FGrammar.CurrentPatternIndex);
  FGrammar.NextToEol;


  SetGrammar(
    [ '{ "name": "t1", "begin": "(^[ \\t]+)?(?=//)", "end":   "(?!\\G)", "beginCaptures": { "1": { "name": "t1.c" } } ' +
      BuildPatterns(['{ "name": "t2", "begin": "//", "end":   "$" }']) +
      '}'
    ],
    []
  );

  p := TestLine('t1', '  // abc',  '1:t1.c,3:t2/t1/1');
  p := TestLine('t1', '',          '1:t1/t1/1',  p);

  FGrammar.SetLine('  // abc', -1);
  FGrammar.NextToEol;
  FGrammar.SetLine('', FGrammar.CurrentPatternIndex);
  FGrammar.NextToEol;



end;



initialization

  RegisterTest(TTestTextMateGrammar);
end.

