unit TestWordChecker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, SynSpellCheckWordBreaker,
  SynSpellDictionary, LazEditTypes;

type

  { TTestCase1 }

  TTestCase1 = class(TTestCase)
  private
    FChecker: TSynSpellWordCheckerSourceCode;
    FSynDict: TSynSpellDictionaryWordList;
  protected
    procedure CheckWordOk(AWord: String);
    procedure CheckWordFailAll(AWord: String);
    procedure CheckWordFailSome(AWord: String);
    procedure CheckWordFail(AWord: String; FailText: array of string; ACombineAdjacent: Boolean = False);
    procedure CreateChecker;
    procedure FreeChecker;
  published
    procedure TestWordCheckerTokensAndParts;
    procedure TestWordCheckerWordAndTokensWithXCaps;
    procedure TestWordCheckerPartMinLen;
    procedure TestWordCheckerIgnoreShort;
    procedure TestWordCheckerIgnoreAtStart;
    procedure TestWordCheckerIgnoreArticle;
  end;

implementation

procedure TTestCase1.CheckWordOk(AWord: String);
begin
  AssertTrue('Is word ok: '+ AWord, FChecker.CheckWord(AWord, 1, Length(AWord)));
end;

procedure TTestCase1.CheckWordFailAll(AWord: String);
var
  r: Boolean;
  fp: IntPos;
  fl: integer;
begin
  r := FChecker.CheckWord(AWord, 1, Length(AWord));
  AssertFalse('Has Error: '+ AWord, r);
  r := FChecker.GetNextError(fp, fl, True);
  AssertFalse('Has one Error: '+ AWord, r);
  AssertEquals('Error Start: '+ AWord, 0, fp);
  AssertEquals('Error Len: '+ AWord, Length(AWord), fl);


  r := FChecker.CheckWord(AWord, 1, Length(AWord));
  AssertFalse('Has Error: '+ AWord, r);
  r := FChecker.GetNextError(fp, fl, False);
  AssertFalse('Has one Error: '+ AWord, r);
  AssertEquals('Error Start: '+ AWord, 0, fp);
  AssertEquals('Error Len: '+ AWord, Length(AWord), fl);
end;

procedure TTestCase1.CheckWordFailSome(AWord: String);
begin
  AssertFalse('Is wrong word: '+ AWord, FChecker.CheckWord(AWord, 1, Length(AWord)));
end;

procedure TTestCase1.CheckWordFail(AWord: String; FailText: array of string;
  ACombineAdjacent: Boolean);
var
  fp, fl, c, i: integer;
  m: Boolean;
begin
  AssertFalse('Word has error: '+ AWord, FChecker.CheckWord(AWord, 1, Length(AWord)));
  c := Length(FailText) - 1;
  for i := 0 to c do begin
    m := FChecker.GetNextError(fp, fl, ACombineAdjacent);
    inc(fp);
    AssertEquals(Format('Err word "%s" (%d/%d) ', [AWord, i, c]), FailText[i], copy(AWord, fp, fl));
    AssertEquals(Format('More Err word "%s" (%d/%d) ', [AWord, i, c]), i <> c, m);
  end;
end;

procedure TTestCase1.CreateChecker;
begin
  FChecker := TSynSpellWordCheckerSourceCode.Create;
  FSynDict := TSynSpellDictionaryWordList.Create;
  FChecker.SynSpellDictionary := FSynDict;

  FChecker.SpecialUpperLetters := 'ÄÖÜ';
  FChecker.SpecialLowerLetters := 'äöü';
end;

procedure TTestCase1.FreeChecker;
begin
  FChecker.Free;
  FSynDict.ReleaseReference;
end;

procedure TTestCase1.TestWordCheckerTokensAndParts;
var
  i, j, k: Integer;
  c1, c2, c3: Boolean;
begin
  CreateChecker;
  FChecker.IgnoreLettersAtStart := '';

  FSynDict.Words.Add('hello');
  FSynDict.Words.Add('world');
  FSynDict.Words.Add('welcome');
  FSynDict.Words.Add('train');
  FSynDict.Words.Add('hell');
  FSynDict.Words.Add('over');
  FSynDict.Words.Add('ver');
  FSynDict.Words.Add('order');
  FSynDict.Words.Add('hellöö');
  FSynDict.Words.Add('über');
  FSynDict.Words.Add('Über');

  // all word,token,parts are longer
  for i := 0 to 3 do
  for j := 0 to 2 do
  for k := 0 to 2 do
  for c1 := False to True do
  for c2 := False to True do
  for c3 := False to True do
  begin
    FChecker.Options := [
      //coAllowWordWithXCaps,           // Allow WOrld as World
      //coAllowIgnoreWordWithXCaps,     // Allow CAt to be ignored like Cat
      coAllowMultiTokenWord         // Allow ReStoRation as word (otherwise must be matched as tokens)
      //coTreatPrefixedWordAsToken,    // AnOption, after removing "An", "Option" will be treated as token
      //coAllowTokenWithXCaps,        // Allow HelloWOrld as HelloWorld
      //coAllowIgnoreTokenWithXCaps,  // Allow XYz in FooXYz or XYzFoo to be ignored like Xyz or xyz
    ];
    if c1 then FChecker.Options := FChecker.Options + [coAllowWordWithXCaps];
    if c2 then FChecker.Options := FChecker.Options + [coAllowTokenWithXCaps];
    if c3 then FChecker.Options := FChecker.Options + [coAllowIgnoreWordWithXCaps, coAllowIgnoreTokenWithXCaps];

    FChecker.IgnoreShortWord  := i;
    FChecker.IgnoreShortToken := j;
    FChecker.IgnoreLowerStart := 3-i;
    FChecker.SetAllPartMinLen(k);
    FChecker.SetAllPartIgnoreLen(i);
    FChecker.SetAllPartIgnoreRequiredOther(5-i);


    // Tokens

    CheckWordOk('hello');
    CheckWordOk('Hello');
    CheckWordOk('HelloHello');
    CheckWordOk('helloHello');
    CheckWordFailAll('hellohello');
    CheckWordFailAll('unknown');
    CheckWordFailAll('Unknown');
    CheckWordFail('HelloUnknown', ['Unknown']);
    CheckWordFail('UnknownHello', ['Unknown']);
    CheckWordFail('HelloUnknownHello', ['Unknown']);

    CheckWordOk('über');
    CheckWordOk('Über');
    CheckWordOk('überÜber');
    CheckWordOk('ÜberÜber');
    CheckWordOk('überÜberÜber');
    CheckWordOk('hellööÜberHellööÜber');
    CheckWordFailAll('überüber');

    // Parts

    CheckWordOk('helloWorld');
    CheckWordOk('HELLOWorld');
    CheckWordOk('HELLOworld');
    CheckWordOk('helloWORLD');
    CheckWordOk('helloWorld');
    CheckWordOk('helloWORLDTrain');
    CheckWordOk('helloWORLDtrain');
    CheckWordOk('HELLOWorldTRAIN');
    CheckWordOk('HELLOworldTRAIN');

    CheckWordOk('HellOrder');
    CheckWordOk('HELLOrder');
    CheckWordOk('HELLorder');
    CheckWordOk('HELLOrder');
    CheckWordOk('hellOrder');
    CheckWordOk('hellORDER');
    CheckWordFailSome('Hellorder');
    CheckWordFailSome('HelloRDER');

    CheckWordOk('HellOver');
    CheckWordOk('HELLOver');
    CheckWordOk('HELLover');
    CheckWordOk('HELLOver');
    CheckWordOk('hellOver');
    CheckWordOk('hellOVER');
    CheckWordOk('HelloVer');
    CheckWordOk('HelloVER');
    CheckWordFailSome('Hellover');


    CheckWordFail('HELLwhat', ['what']);
    //CheckWordFail('HELLWhat', ['What']);
    CheckWordFail('hellWhat', ['What']);
    CheckWordFail('hellWHAT', ['WHAT']);
    CheckWordFail('whatHELL', ['what']);
    CheckWordFail('WHAThell', ['WHAT']);
    CheckWordFail('WHATHell', ['WHAT']);
    CheckWordFail('HELLwhatHELL', ['what']);
    CheckWordFail('hellWHAThell', ['WHAT']);
    CheckWordFail('whatHELLwhat', ['what', 'what']);
    CheckWordFail('WHAThellWHAT', ['WHAT', 'WHAT']);

    // Words

    CheckWordOk('WelCome');
    FChecker.Options := FChecker.Options - [coAllowMultiTokenWord];
    CheckWordFailSome('WelCome');

    FChecker.Options := FChecker.Options + [coAllowMultiTokenWord];
  end;

  FreeChecker;
end;

procedure TTestCase1.TestWordCheckerWordAndTokensWithXCaps;
var
  i, j: Integer;
  c1, c2: Boolean;
begin
  CreateChecker;
  FChecker.IgnoreLettersAtStart := '';

  FSynDict.Words.Add('hello');
  FSynDict.Words.Add('world');
  FSynDict.Words.Add('welcome');
  FSynDict.Words.Add('hell');

  // all word,token,parts are longer
  for i := 0 to 2 do
  for j := 0 to 2 do
  begin
    FChecker.IgnoreShortWord  := i;
    FChecker.IgnoreShortToken := j;
    FChecker.IgnoreLowerStart := i;
    FChecker.SetAllPartMinLen(2-j);
    FChecker.SetAllPartIgnoreLen(i);
    FChecker.SetAllPartIgnoreRequiredOther(5-i);

    // XCaps tests
    for c1 := False to True do
    for c2 := False to True do
    begin
      FChecker.Options := [];
      if c1 then FChecker.Options := FChecker.Options + [coAllowMultiTokenWord];
      if c2 then FChecker.Options := FChecker.Options + [coAllowIgnoreWordWithXCaps, coAllowIgnoreTokenWithXCaps];


      CheckWordOk      ('Welcome');
      CheckWordFailAll ('WElcome');
      CheckWordOk      ('HelloWorld');
      CheckWordFailSome('HElloWorld');
      CheckWordFailSome('HelloWOrld');
      CheckWordFailSome('HElloWOrld');


      FChecker.Options := FChecker.Options + [coAllowWordWithXCaps];
      FChecker.Options := FChecker.Options - [coAllowTokenWithXCaps];
      CheckWordOk      ('Welcome');
      CheckWordOk      ('WElcome');
      CheckWordOk      ('HelloWorld');
      CheckWordFailSome('HElloWorld');
      CheckWordFailSome('HelloWOrld');
      CheckWordFailSome('HElloWOrld');

      FChecker.Options := FChecker.Options - [coAllowWordWithXCaps];
      FChecker.Options := FChecker.Options + [coAllowTokenWithXCaps];
      CheckWordOk      ('Welcome');
      CheckWordFailAll ('WElcome');
      CheckWordOk      ('HelloWorld');
      CheckWordOk      ('HElloWorld');
      CheckWordOk      ('HelloWOrld');
      CheckWordOk      ('HElloWOrld');

      FChecker.Options := FChecker.Options + [coAllowWordWithXCaps];
      FChecker.Options := FChecker.Options + [coAllowTokenWithXCaps];
      CheckWordOk      ('Welcome');
      CheckWordOk      ('WElcome');
      CheckWordOk      ('HelloWorld');
      CheckWordOk      ('HElloWorld');
      CheckWordOk      ('HelloWOrld');
      CheckWordOk      ('HElloWOrld');

      FChecker.IgnoreLettersAtStart := 'T';
      FChecker.Options := FChecker.Options - [coAllowWordWithXCaps, coAllowTokenWithXCaps];
      CheckWordOk      ('TWelcome');
      CheckWordFailAll ('TWElcome');

      FChecker.Options := FChecker.Options - [coAllowWordWithXCaps, coAllowTokenWithXCaps];
      FChecker.Options := FChecker.Options + [coAllowWordWithXCaps];
      CheckWordOk      ('TWelcome');
      CheckWordOk      ('TWElcome');

      FChecker.Options := FChecker.Options - [coAllowWordWithXCaps, coAllowTokenWithXCaps];
      FChecker.Options := FChecker.Options + [coAllowTokenWithXCaps];
      CheckWordOk      ('TWelcome');
      CheckWordFailAll ('TWElcome');

      // Word as Token
      FChecker.Options := FChecker.Options + [coTreatPrefixedWordAsToken];

      FChecker.Options := FChecker.Options - [coAllowWordWithXCaps, coAllowTokenWithXCaps];
      CheckWordOk      ('TWelcome');
      CheckWordFailAll ('TWElcome');

      FChecker.Options := FChecker.Options - [coAllowWordWithXCaps, coAllowTokenWithXCaps];
      FChecker.Options := FChecker.Options + [coAllowWordWithXCaps];
      CheckWordOk      ('TWelcome');
      CheckWordFailAll ('TWElcome');

      FChecker.Options := FChecker.Options - [coAllowWordWithXCaps, coAllowTokenWithXCaps];
      FChecker.Options := FChecker.Options + [coAllowTokenWithXCaps];
      CheckWordOk      ('TWelcome');
      CheckWordOk      ('TWElcome');

    end;
  end;

  FreeChecker;
end;

procedure TTestCase1.TestWordCheckerPartMinLen;
var
  i, j: Integer;
  c1: Boolean;
begin
  CreateChecker;
  FChecker.IgnoreLettersAtStart := '';

  FSynDict.Words.Add('hello');
  FSynDict.Words.Add('welcome');
  FSynDict.Words.Add('hell');
  FSynDict.Words.Add('onto');
  FSynDict.Words.Add('order');

  // all word,token,parts are longer
  for i := 0 to 2 do
  for j := 0 to 2 do
  for c1 := False to True do
  begin
    FChecker.Options := [coAllowMultiTokenWord];
    if i = j then FChecker.Options := [];
    if c1 then FChecker.Options := FChecker.Options + [coAllowWordWithXCaps, coAllowTokenWithXCaps];

    FChecker.IgnoreShortWord  := i;
    FChecker.IgnoreShortToken := j;
    FChecker.IgnoreLowerStart := i;
    FChecker.SetAllPartMinLen(2-j);
    FChecker.SetAllPartIgnoreLen(i);
    FChecker.SetAllPartIgnoreRequiredOther(5-i);


    FChecker.PartLeadConstraints.MinLen := 4; // Hell/Welcome
    // Lead/Mix
    CheckWordOk     ('HELLOrder');
    CheckWordOk     ('WELCOMEOnto');
    // Up/Low
    CheckWordOk     ('HELLorder');
    CheckWordOk     ('WELCOMEonto');
    FChecker.PartLeadConstraints.MinLen := 5; // HELL is to short => Hello is forced
    CheckWordFail   ('HELLOrder', ['rder']);
    CheckWordOk     ('WELCOMEOnto');
    CheckWordOk     ('HELLorder');
    CheckWordOk     ('WELCOMEonto');
    FChecker.PartLeadConstraints.MinLen := 7;
    CheckWordFail   ('HELLOrder', ['rder']);
    CheckWordOk     ('WELCOMEOnto');
    CheckWordOk     ('HELLorder');
    CheckWordOk     ('WELCOMEonto');
    FChecker.PartLeadConstraints.MinLen := 8; // Welcome to short
    CheckWordFail   ('HELLOrder', ['rder']);
    CheckWordFailAll('WELCOMEOnto');
    CheckWordOk     ('HELLorder');
    CheckWordOk     ('WELCOMEonto');
    FChecker.PartLeadConstraints.MinLen := 99;
    CheckWordFail   ('HELLOrder', ['rder']);
    CheckWordFailAll('WELCOMEOnto');
    CheckWordOk     ('HELLorder');
    CheckWordOk     ('WELCOMEonto');
    FChecker.PartLeadConstraints.MinLen := 0;

    FChecker.PartMixedConstraints.MinLen := 4; // Onto/Order
    CheckWordOk     ('HELLOrder');
    CheckWordOk     ('WELCOMEOnto');
    CheckWordOk     ('HELLorder');
    CheckWordOk     ('WELCOMEonto');
    FChecker.PartMixedConstraints.MinLen := 5; // Onto to short
    CheckWordOk     ('HELLOrder');
    CheckWordFailAll('WELCOMEOnto');
    CheckWordOk     ('HELLorder');
    CheckWordOk     ('WELCOMEonto');
    FChecker.PartMixedConstraints.MinLen := 6; // Order to short
    CheckWordFail   ('HELLOrder', ['rder']);
    CheckWordFailAll('WELCOMEOnto');
    CheckWordOk     ('HELLorder');
    CheckWordOk     ('WELCOMEonto');
    FChecker.PartMixedConstraints.MinLen := 99;
    CheckWordFail   ('HELLOrder', ['rder']);
    CheckWordFailAll('WELCOMEOnto');
    CheckWordOk     ('HELLorder');
    CheckWordOk     ('WELCOMEonto');
    FChecker.PartMixedConstraints.MinLen := 0;

    FChecker.PartUpperConstraints.MinLen := 4; // HELL/HELLO/Welcome
    CheckWordOk     ('HELLOrder');
    CheckWordOk     ('WELCOMEOnto');
    CheckWordOk     ('HELLorder');
    CheckWordOk     ('WELCOMEonto');
    FChecker.PartUpperConstraints.MinLen := 5; // HELL to short
    CheckWordOk     ('HELLOrder');
    CheckWordOk     ('WELCOMEOnto');
    CheckWordFailAll('HELLorder');
    CheckWordOk     ('WELCOMEonto');
    FChecker.PartUpperConstraints.MinLen := 7; // HELL to short
    CheckWordOk     ('HELLOrder');
    CheckWordOk     ('WELCOMEOnto');
    CheckWordFailAll('HELLorder');
    CheckWordOk     ('WELCOMEonto');
    FChecker.PartUpperConstraints.MinLen := 8; // WELCOME to short
    CheckWordOk     ('HELLOrder');
    CheckWordOk     ('WELCOMEOnto');
    CheckWordFailAll('HELLorder');
    CheckWordFailAll('WELCOMEonto');
    FChecker.PartUpperConstraints.MinLen := 99;
    CheckWordOk     ('HELLOrder');
    CheckWordOk     ('WELCOMEOnto');
    CheckWordFailAll('HELLorder');
    CheckWordFailAll('WELCOMEonto');
    FChecker.PartUpperConstraints.MinLen := 0;

    FChecker.PartLowerConstraints.MinLen := 4; // order/onto
    CheckWordOk     ('HELLOrder');
    CheckWordOk     ('WELCOMEOnto');
    CheckWordOk     ('HELLorder');
    CheckWordOk     ('WELCOMEonto');
    FChecker.PartLowerConstraints.MinLen := 5; // onto to short
    CheckWordOk     ('HELLOrder');
    CheckWordOk     ('WELCOMEOnto');
    CheckWordOk     ('HELLorder');
    CheckWordFailAll('WELCOMEonto');
    FChecker.PartLowerConstraints.MinLen := 6; // order to short
    CheckWordOk     ('HELLOrder');
    CheckWordOk     ('WELCOMEOnto');
    CheckWordFailAll('HELLorder');
    CheckWordFailAll('WELCOMEonto');
    FChecker.PartLowerConstraints.MinLen := 99;
    CheckWordOk     ('HELLOrder');
    CheckWordOk     ('WELCOMEOnto');
    CheckWordFailAll('HELLorder');
    CheckWordFailAll('WELCOMEonto');
    FChecker.PartLowerConstraints.MinLen := 0;



    FChecker.PartUpperConstraints.MinLen := 0;
    FChecker.PartLowerConstraints.MinLen := 5; // onto
    FChecker.PartLeadConstraints.MinLen := 0;
    FChecker.PartMixedConstraints.MinLen := 5; // Onto
    CheckWordFailAll('WELCOMEonto');
    CheckWordOk     ('HELLOrder');
    CheckWordFailAll('WELCOMEOnto');
    CheckWordOk     ('HELLorder');

    FChecker.PartUpperConstraints.MinLen := 0;
    FChecker.PartLowerConstraints.MinLen := 6; // order
    FChecker.PartLeadConstraints.MinLen := 0;
    FChecker.PartMixedConstraints.MinLen := 6; // Order
    CheckWordFailAll('WELCOMEonto');
    CheckWordFailAll('HELLOrder');
    CheckWordFailAll('WELCOMEOnto');
    CheckWordFailAll('HELLorder');


    FChecker.PartUpperConstraints.MinLen := 9;
    FChecker.PartLowerConstraints.MinLen := 0;
    FChecker.PartLeadConstraints.MinLen  := 0;
    FChecker.PartMixedConstraints.MinLen := 9;
    CheckWordFailAll('WELCOMEonto');
    CheckWordFailAll('HELLOrder');
    CheckWordFailAll('WELCOMEOnto');
    CheckWordFailAll('HELLorder');

    FChecker.PartUpperConstraints.MinLen := 9;
    FChecker.PartLowerConstraints.MinLen := 0;
    FChecker.PartLeadConstraints.MinLen  := 9;
    FChecker.PartMixedConstraints.MinLen := 0;
    CheckWordFailAll('WELCOMEonto');
    CheckWordFailAll('HELLOrder');
    CheckWordFailAll('WELCOMEOnto');
    CheckWordFailAll('HELLorder');

    FChecker.PartUpperConstraints.MinLen := 0;
    FChecker.PartLowerConstraints.MinLen := 9;
    FChecker.PartLeadConstraints.MinLen  := 9;
    FChecker.PartMixedConstraints.MinLen := 0;
    CheckWordFailAll('WELCOMEonto');
    CheckWordFailAll('HELLOrder');
    CheckWordFailAll('WELCOMEOnto');
    CheckWordFailAll('HELLorder');

    FChecker.PartUpperConstraints.MinLen := 9;
    FChecker.PartLowerConstraints.MinLen := 9;
    FChecker.PartLeadConstraints.MinLen  := 9;
    FChecker.PartMixedConstraints.MinLen := 9;
    CheckWordFailAll('WELCOMEonto');
    CheckWordFailAll('HELLOrder');
    CheckWordFailAll('WELCOMEOnto');
    CheckWordFailAll('HELLorder');
  end;

  FreeChecker;
end;

procedure TTestCase1.TestWordCheckerIgnoreShort;
var
  c1, c2, c3, c4: Boolean;
  p: String;
  i, Ld1, Ld2, Mx1, Mx2, Up1, Up2, Lw1, Lw2: Integer;
begin
  CreateChecker;
  FSynDict.Words.Add('hello');
  FSynDict.Words.Add('world');
  FSynDict.Words.Add('train');
  FSynDict.Words.Add('welcome');

  FChecker.SetAllPartIgnoreLen(0);
  FChecker.SetAllPartMinLen(0);
  // Word

  for c1 := False to True do
  for c2 := False to True do
  begin
    FChecker.IgnoreShortWord := 5;
    FChecker.SetAllPartIgnoreLen(0);
    FChecker.IgnoreShortToken := 2;
    FChecker.Options := [];
    // token settings should not matter
    if c1 then FChecker.IgnoreShortToken := 9;
    if c2 then FChecker.Options := FChecker.Options + [coAllowIgnoreTokenWithXCaps];

    for c3 := False to True do
    for c4 := False to True do begin
      FChecker.Options := FChecker.Options - [coAllowMultiTokenWord, coAllowIgnoreWordWithXCaps];
      if c3 then FChecker.Options := FChecker.Options + [coAllowMultiTokenWord];
      if c4 then FChecker.Options := FChecker.Options + [coAllowIgnoreWordWithXCaps];

      CheckWordFailAll ('abcdef');
      CheckWordFailAll ('äöüdef');
      CheckWordFailAll ('Abcdef');
      CheckWordFailAll ('ABcdef');
      CheckWordFailAll ('ÄÖcdef');
      if not c1 then begin
      CheckWordFailSome('AbcDef');
      CheckWordFailSome('AbcDEF');
      end;
    end;


    FChecker.IgnoreShortWord := 6;

    FChecker.Options := FChecker.Options - [coAllowMultiTokenWord, coAllowIgnoreWordWithXCaps];
    CheckWordOk      ('abcdef');
    CheckWordOk      ('äöüdef');
    CheckWordOk      ('Abcdef');
    CheckWordFailAll ('ABcdef');
    CheckWordFailAll ('ÄÖcdef');
    if not c1 then begin
    CheckWordFailSome('AbcDef');
    CheckWordFailSome('AbcDEF');
    end;


    FChecker.Options := FChecker.Options + [coAllowMultiTokenWord];
    CheckWordOk      ('abcdef');
    CheckWordOk      ('äöüdef');
    CheckWordOk      ('Abcdef');
    CheckWordFailAll ('ABcdef');
    CheckWordFailAll ('ÄÖcdef');
    CheckWordOk      ('AbcDef');
    CheckWordOk      ('AbcDEF');

    FChecker.Options := FChecker.Options - [coAllowMultiTokenWord];
    FChecker.Options := FChecker.Options + [coAllowIgnoreWordWithXCaps];
    CheckWordOk      ('abcdef');
    CheckWordOk      ('äöüdef');
    CheckWordOk      ('Abcdef');
    CheckWordOk      ('ABcdef');
    CheckWordOk      ('ÄÖcdef');
    if not c1 then begin
    CheckWordFailSome('AbcDef');
    CheckWordFailSome('AbcDEF');
    end;

    FChecker.Options := FChecker.Options + [coAllowMultiTokenWord, coAllowIgnoreWordWithXCaps];
    CheckWordOk      ('abcdef');
    CheckWordOk      ('äöüdef');
    CheckWordOk      ('Abcdef');
    CheckWordOk      ('ABcdef');
    CheckWordOk      ('ÄÖcdef');
    CheckWordOk      ('AbcDef');
    CheckWordOk      ('AbcDEF');

  end;

  // Word with Prefix as token
  // The prefix does not count as part as the word len
  for c1 := false to true do begin
    p := 'T';
    if c1 then p := 'Ü';
    FChecker.IgnoreLettersAtStart := p;

    FChecker.IgnoreShortWord := 5;
    FChecker.IgnoreShortToken := 6;
    FChecker.Options := [coAllowIgnoreWordWithXCaps, coAllowIgnoreTokenWithXCaps];
    CheckWordFailAll (p+'abcdef');
    CheckWordFailAll (p+'Abcdef');
    CheckWordFailAll (p+'ABcdef');

    FChecker.IgnoreShortWord := 6;
    FChecker.Options := [coAllowIgnoreTokenWithXCaps];
    CheckWordOk      (p+'abcdef');
    CheckWordOk      (p+'Abcdef');
    CheckWordFailAll (p+'ABcdef');

    FChecker.IgnoreShortWord := 6;
    FChecker.Options := [coAllowIgnoreWordWithXCaps];
    CheckWordOk      (p+'abcdef');
    CheckWordOk      (p+'Abcdef');
    CheckWordOk      (p+'ABcdef');

    FChecker.IgnoreShortWord := 5;
    FChecker.IgnoreShortToken := 5;
    FChecker.Options := [coTreatPrefixedWordAsToken, coAllowIgnoreWordWithXCaps, coAllowIgnoreTokenWithXCaps];
    CheckWordFailAll (p+'abcdef');
    CheckWordFailAll (p+'Abcdef');
    CheckWordFailAll (p+'ABcdef');

    FChecker.IgnoreShortToken := 6;
    FChecker.Options := [coTreatPrefixedWordAsToken, coAllowIgnoreWordWithXCaps];
    CheckWordOk      (p+'abcdef');
    CheckWordOk      (p+'Abcdef');
    CheckWordFailAll (p+'ABcdef');

    FChecker.IgnoreShortToken := 6;
    FChecker.Options := [coTreatPrefixedWordAsToken, coAllowIgnoreTokenWithXCaps];
    CheckWordOk      (p+'abcdef');
    CheckWordOk      (p+'Abcdef');
    CheckWordOk      (p+'ABcdef');

    FChecker.IgnoreLettersAtStart := '';
  end;

  // Ignore tokens
  FChecker.IgnoreShortWord := 0;
  FChecker.Options := [];

  FChecker.IgnoreShortToken := 2;

  CheckWordFailSome('HelloFooWorld');
  CheckWordFailSome('FooHelloWorld');
  CheckWordFailSome('HelloWorldFoo');
  CheckWordFailSome('HelloWorldFOO'); // last token all upper

  FChecker.IgnoreShortToken := 3;

  CheckWordOk('HelloFooWorld');
  CheckWordOk('FooHelloWorld');
  CheckWordOk('HelloWorldFoo');
  CheckWordOk('HelloWorldFOO');

  // XCaps
  CheckWordFailSome('HelloFOoWorld');
  CheckWordFailSome('FOoHelloWorld');
  CheckWordFailSome('HelloWorldFOo');

  FChecker.Options := [coAllowIgnoreWordWithXCaps];
  CheckWordFailSome('HelloFOoWorld');
  CheckWordFailSome('FOoHelloWorld');
  CheckWordFailSome('HelloWorldFOo');

  FChecker.Options := [coAllowIgnoreTokenWithXCaps];
  CheckWordOk('HelloFOoWorld');
  CheckWordOk('FOoHelloWorld');
  CheckWordOk('HelloWorldFOo');

  // Ignore token after prefix
  FChecker.IgnoreLettersAtStart := 'T';
  FChecker.IgnoreShortWord := 0;
  FChecker.Options := [];

  FChecker.IgnoreShortToken := 2;
  CheckWordFailSome('TFooHelloWorld');

  FChecker.IgnoreShortToken := 3;
  CheckWordOk('TFooHelloWorld');

  FChecker.IgnoreLettersAtStart := '';


  // IgnoreLowerStart

  FChecker.Options := [];
  FChecker.IgnoreShortToken := 3;
  FChecker.IgnoreShortWord  := 5;
  FChecker.IgnoreLowerStart := 0;
  CheckWordFailSome('barHelloWorld');
  CheckWordFailSome('barHelloWorldFoo');

  FChecker.IgnoreShortToken := 3;
  FChecker.IgnoreShortWord  := 5;
  FChecker.IgnoreLowerStart := 2;
  CheckWordFailSome('barHelloWorld');
  CheckWordFailSome('barHelloWorldFoo');

  FChecker.IgnoreShortToken := 0;
  FChecker.IgnoreShortWord  := 5;
  FChecker.IgnoreLowerStart := 3;
  CheckWordOk      ('barHelloWorld');
  CheckWordFailSome('barHelloWorldFoo');

  FChecker.IgnoreShortToken := 3;
  FChecker.IgnoreShortWord  := 5;
  FChecker.IgnoreLowerStart := 3;
  CheckWordOk      ('barHelloWorld');
  CheckWordOk      ('barHelloWorldFoo');

  // IgnoreLowerStart should not happen after prefix
  FChecker.IgnoreShortToken := 0;
  FChecker.IgnoreLowerStart := 3;
  FChecker.IgnoreLettersAtStart := 'T';
  CheckWordFailSome      ('TbarHelloWorld');
  FChecker.IgnoreLettersAtStart := '';



  // Ignore parts
  FChecker.Options := [];
  FChecker.IgnoreShortToken := 0;
  FChecker.IgnoreShortWord  := 0;
  FChecker.IgnoreLowerStart := 0;

  // THE is part of a token
  CheckWordFailSome('WelcomeTHEWorld'); // Lead part
  CheckWordFailSome('WelcomeTHEworld'); // UP   part
  CheckWordFailSome('WELCOMETheWorld'); // Mix  part
  CheckWordFailSome('WELCOMEtheWorld'); // low  part

  for i := 0 to 1 do
  for Ld1 := 0 to 4 do
  for Ld2 := 0 to 6 do // world = 5
  for Mx1 := 0 to 4 do
  for Mx2 := 0 to 8 do // welcome = 7

  for Up1 := 2 to 3 do
  for Up2 := 5 to 6 do
  for Lw1 := 2 to 3 do
  for Lw2 := 7 to 8 do
  begin
    // i=1: skip the part entirely => only check when limits are near the turn-point
    if (i=1) and
       ( (Ld1 < 2) or (Mx1 < 2) or (Ld1 > 3) or (Mx1 > 3) )
    then
      continue;
    // only check: 0 (zero) and 2..4 (one below min, to one above min)
    if (Ld1 = 1) or (Mx1 = 1) or (Ld2 in [1..3]) or (Mx2 in [1..5])
    then
      continue;

    FChecker.PartLeadConstraints.Init (i*4, Ld1, Ld2);
    FChecker.PartUpperConstraints.Init(i*4, Up1, Up2);
    FChecker.PartMixedConstraints.Init(i*4, Mx1, Mx2);
    FChecker.PartLowerConstraints.Init(i*4, Lw1, Lw2);
    if (Ld1 >= 3) and (Ld2 <= 5) and (i=0)
    then CheckWordOk      ('WelcomeTHEWorld')
    else CheckWordFailSome('WelcomeTHEWorld');

    if (Up1 >= 3) and (Up2 <= 5) and (i=0)
    then CheckWordOk      ('WelcomeTHEworld')
    else CheckWordFailSome('WelcomeTHEworld');

    if (Mx1 >= 3) and (Mx2 <= 7) and (i=0)
    then CheckWordOk      ('WELCOMETheWorld')
    else CheckWordFailSome('WELCOMETheWorld');

    if (Lw1 >= 3) and (Lw2 <= 7) and (i=0)
    then CheckWordOk      ('WELCOMEtheWorld')
    else CheckWordFailSome('WELCOMEtheWorld');
  end;

  FChecker.PartLeadConstraints.Init (0, 2, 3);
  FChecker.PartUpperConstraints.Init(0, 2, 3);
  FChecker.PartMixedConstraints.Init(0, 2, 3);
  FChecker.PartLowerConstraints.Init(0, 2, 3);
  FChecker.Options := [];
  CheckWordFail('Xabc', ['Xabc']);
  CheckWordFail('XYabc', ['XYabc']);
  CheckWordFail('Xabcd', ['Xabcd']);
  CheckWordFail('XYabcd', ['XYabcd']);
  CheckWordFail('HelloXabcWorld', ['Xabc']);
  CheckWordFail('HelloXYabcWorld', ['XYabc']);

  FChecker.Options := [coAllowIgnoreTokenWithXCaps, coAllowIgnoreWordWithXCaps];
  CheckWordFail('Xabc', ['Xabc']);
  CheckWordFail('XYabc', ['XYabc']);
  CheckWordFail('Xabcd', ['Xabcd']);
  CheckWordFail('XYabcd', ['XYabcd']);
  CheckWordFail('HelloXabcWorld', ['Xabc']);
  CheckWordFail('HelloXYabcWorld', ['XYabc']);

  FChecker.Options := [coAllowIgnoreTokenWithXCaps, coAllowIgnoreWordWithXCaps, coAllowMultiTokenWord];
  CheckWordFail('Xabc', ['Xabc']);
  CheckWordFail('XYabc', ['XYabc']);
  CheckWordFail('Xabcd', ['Xabcd']);
  CheckWordFail('XYabcd', ['XYabcd']);
  CheckWordFail('HelloXabcWorld', ['Xabc']);
  CheckWordFail('HelloXYabcWorld', ['XYabc']);

  FChecker.PartLeadConstraints.Init (0, 2, 7);
  FChecker.PartUpperConstraints.Init(0, 2, 7);
  FChecker.PartMixedConstraints.Init(0, 2, 7);
  FChecker.PartLowerConstraints.Init(0, 2, 7);
  FChecker.Options := [];
  CheckWordFail('Xabc', ['Xabc']);
  CheckWordFail('XYabc', ['XYabc']);
  CheckWordFail('HelloXabcWorld', ['Xabc']);
  CheckWordFail('HelloXYabcWorld', ['XYabc']);
  FreeChecker;
end;

procedure TTestCase1.TestWordCheckerIgnoreAtStart;
var
  Opts: TSynSpellWordCheckerSourceCodeOptions;
begin
  CreateChecker;

  Opts := [coAllowMultiTokenWord, coAllowWordWithXCaps, coAllowTokenWithXCaps];
  FChecker.Options := Opts;
  FChecker.IgnoreLettersAtStart := 'TtFpÜö';
  FChecker.IgnoreAtStartMinRemainderLen := 3;

  FSynDict.Words.Add('hello');
  FSynDict.Words.Add('FooBar');
  FSynDict.Words.Add('train');
  FSynDict.Words.Add('welcome');
  FSynDict.Words.Add('über');
  FSynDict.Words.Add('Über');


  CheckWordOk('thello');
  CheckWordOk('Thello');
  CheckWordOk('tHello');
  CheckWordOk('THello');
  CheckWordFailSome('xhello');

  CheckWordOk('train');
  CheckWordOk('Train');
  CheckWordOk('ttrain');
  CheckWordOk('Ttrain');
  CheckWordOk('tTrain');
  CheckWordOk('TTrain');
  CheckWordOk('ttrain');
  CheckWordFailSome('xtrain');
  CheckWordFailSome('Xttrain');


  CheckWordOk('Tüber');
  CheckWordOk('TÜber');
  CheckWordOk('Üüber');
  CheckWordOk('ÜÜber');
  CheckWordFailSome('xüber');
  CheckWordFailSome('äüber');

  // Check min len

  FChecker.IgnoreAtStartMinRemainderLen := 5;
  CheckWordOk('thello');
  CheckWordOk('thelloHello');
  CheckWordOk('twelcome');
  CheckWordOk('TFooBar');
  CheckWordOk('tFooBar');
  FChecker.IgnoreAtStartMinRemainderLen := 6;
  CheckWordFailSome('thello');
  CheckWordFailSome('thelloHello'); // hello is to short
  CheckWordOk('twelcome');
  CheckWordOk('TFooBar');  // entire FooBar is still long enough
  CheckWordOk('tFooBar');

  FChecker.IgnoreAtStartMinRemainderLen := 4;
  CheckWordOk('tüber');
  CheckWordOk('öüber');
  FChecker.IgnoreAtStartMinRemainderLen := 5;
  CheckWordFailSome('tüber');
  CheckWordFailSome('öüber');


  FChecker.IgnoreAtStartMinRemainderLen := 0;
  FChecker.Options := Opts + [coIgnoredUpperStartMustMatchCase];

  CheckWordOk('train');
  CheckWordOk('Train');
  CheckWordOk('ttrain');
  CheckWordFailSome('Ttrain');
  CheckWordOk('tTrain');
  CheckWordOk('TTrain');
  CheckWordOk('ttrain');

  FChecker.IgnoreAtStartMinRemainderLen := 0;
  FChecker.Options := Opts + [coIgnoredLowerStartMustMatchCase];

  CheckWordOk('train');
  CheckWordOk('Train');
  CheckWordOk('ttrain');
  CheckWordOk('Ttrain');
  CheckWordFailSome('tTrain');
  CheckWordOk('TTrain');
  CheckWordOk('ttrain');

  FChecker.IgnoreAtStartMinRemainderLen := 0;
  FChecker.Options := Opts + [coIgnoredUpperStartMustMatchCase, coIgnoredLowerStartMustMatchCase];

  CheckWordOk('train');
  CheckWordOk('Train');
  CheckWordOk('ttrain');
  CheckWordFailSome('Ttrain');
  CheckWordFailSome('tTrain');
  CheckWordOk('TTrain');
  CheckWordOk('ttrain');


  FreeChecker;
end;

procedure TTestCase1.TestWordCheckerIgnoreArticle;
var
  u, l, uc, lc, a, an: Boolean;
  r, i: Integer;
  s1, s2: String;
begin
  CreateChecker;
  FChecker.Options := [];
  FChecker.IgnoreLettersAtStart := '';
  FChecker.IgnoreShortWord  := 0;
  FChecker.IgnoreShortToken := 0;
  FChecker.IgnoreLowerStart := 0;
  FChecker.SetAllPartMinLen(3);
  FChecker.SetAllPartIgnoreLen(0);


  FSynDict.Words.Add('hello');
  FSynDict.Words.Add('world');
  FSynDict.Words.Add('newer');
  FSynDict.Words.Add('never');
  FSynDict.Words.Add('ever');
  FSynDict.Words.Add('again');
  FSynDict.Words.Add('anchor');

  for u := false to true do
  for l := false to true do
  for uc := false to true do
  for lc := false to true do
  for r  := 0 to 1 do
  begin
    FChecker.Options := [];
    if u then FChecker.Options  := FChecker.Options + [coIgnoreUpperArticle];
    if l then FChecker.Options  := FChecker.Options + [coIgnoreLowerArticle];
    if uc then FChecker.Options := FChecker.Options + [coIgnoreUpperArticleMustMatchCase];
    if lc then FChecker.Options := FChecker.Options + [coIgnoreLowerArticleMustMatchCase];


    CheckWordFailSome('aNhello');  // never
    CheckWordOk('again');
    CheckWordOk('Again');
    CheckWordOk('anchor');
    CheckWordOk('Anchor');

    for i := 0 to 6 do begin
      case i of
        0: begin s1 := 'hello';  s2 := 'Hello';  a := True;  an := True; end;
        1: begin s1 := 'world';  s2 := 'World';  a := True;  an := False; end;
        2: begin s1 := 'newer';  s2 := 'Newer';  a := True;  an := False; end;
        3: begin s1 := 'never';  s2 := 'Never';  a := True;  an := False; end;
        4: begin s1 := 'ever';   s2 := 'Ever';   a := False; an := True; end;
        5: begin s1 := 'again';  s2 := 'Again';  a := False; an := True; end;
        6: begin s1 := 'anchor'; s2 := 'Anchor'; a := False; an := True; end;
      end;

      // an + ever = a + never
      if (i = 4) and (r = 1) then continue; // because "never" is still long enough
      if (i = 4) and (uc or lc) then continue; // because n in "never" may mismatch

      FChecker.IgnoreAtStartMinRemainderLen := Length(s1) + r; //block by remainder

      if (r=0) and l and a
      then CheckWordOk      ('a' +s1)
      else CheckWordFailSome('a' +s1);
      if (r=0) and l and an
      then CheckWordOk      ('an'+s1)
      else CheckWordFailSome('an'+s1);

      if (r=0) and u and (not uc)  and a
      then CheckWordOk      ('A' +s1)
      else CheckWordFailSome('A' +s1);
      if (r=0) and u and (not uc) and an
      then CheckWordOk      ('An'+s1)
      else CheckWordFailSome('An'+s1);
      if (r=0) and u and (not uc) and an
      then CheckWordOk      ('AN'+s1)
      else CheckWordFailSome('AN'+s1);


      if (r=0) and l and (not lc) and a
      then CheckWordOk      ('a' +s2)
      else CheckWordFailSome('a' +s2);
      if (r=0) and l and (not lc) and an
      then CheckWordOk      ('an'+s2)
      else CheckWordFailSome('an'+s2);

      if (r=0) and u and a
      then CheckWordOk      ('A' +s2)
      else CheckWordFailSome('A' +s2);
      if (r=0) and u and an
      then CheckWordOk      ('An'+s2)
      else CheckWordFailSome('An'+s2);
      if (r=0) and u and an
      then CheckWordOk      ('AN'+s2)
      else CheckWordFailSome('AN'+s2);
    end;

  end;

  FreeChecker;
end;


initialization

  RegisterTest(TTestCase1);
end.

