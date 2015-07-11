program testregexpr;

{$mode objfpc}{$H+}

uses
  SynRegExpr;

type
   tregexprflag = (
     ref_singleline,
     {** This indicates that a start of line is either the
         start of the pattern or a linebreak. }
     ref_multiline,
     {** The match will be done in a case-insensitive way
          according to US-ASCII character set. }
     ref_caseinsensitive);
   tregexprflags = set of tregexprflag;
   TRegExprEngine = integer;

var
   re : TRegExpr;
   r:TRegExprEngine;
  initok:boolean;
  index,len:integer;


procedure Check (ASubExprMatchCount, APos, ALen : integer);
 begin
  if (re.SubExprMatchCount <> ASubExprMatchCount)
     or (re.MatchPos [0] <> APos) or (re.MatchLen [0] <> ALen) then begin
    writeln ('Error. '#$d#$a'Expression "', re.Expression, '"'#$d#$a,
     'Modifiers "', re.ModifierStr, '"'#$d#$a,
     'Input text "', re.InputString, '"'#$d#$a,
     'Actual/expected results: '#$d#$a,
     '  Sub-expressions matched: ', re.SubExprMatchCount, ' / ', ASubExprMatchCount, #$d#$a,
     '  Expression match starting position: ', re.MatchPos [0], ' / ', APos, #$d#$a,
     '  Expression match length: ', re.MatchLen [0], ' / ', ALen);
    writeln ('P-Code:'#$d#$a, re.Dump);
    halt (1);
   end;
 end;

function GenerateRegExprEngine(regexpr : pchar;flags : tregexprflags;var RegExprEngine: TRegExprEngine): boolean;

begin
 re.expression:=regexpr;
 re.ModifierI:=ref_caseinsensitive in flags;
 result:=true;
end;

function RegExprPos(RegExprEngine : TRegExprEngine;p : pchar;var index,len : longint) : boolean;

begin
 result:=re.Exec(p);
 index:=re.MatchPos[0]-1;
 len:=re.MatchLen[0];
end;

procedure do_error(i : longint);

  begin
     writeln('error near ',i,' index: ',index,' len: ',len);
  end;

begin
   writeln ('*** Testing library SynRegExpr ***');

   { basic tests }

//   acon;
   re := TRegExpr.Create;

   re.Expression := '[A-Z]';
   re.Exec ('234578923457823659GHJK38');
   Check (0, 19, 1);

   re.Expression := '[A-Z]*?';
   re.Exec ('234578923457823659ARTZU38');
   Check (0, 1, 0);

   re.Expression := '[A-Z]+';
   re.Exec ('234578923457823659ARTZU38');
   Check (0, 19, 5);

   re.Expression := '[A-Z][A-Z]*';
   re.Exec ('234578923457823659ARTZU38');
   Check (0, 19, 5);

   re.Expression := '[A-Z][A-Z]?';
   re.Exec ('234578923457823659ARTZU38');
   Check (0, 19, 2);

   re.Expression := '[^\d]+';
   re.Exec ('234578923457823659ARTZU38');
   Check (0, 19, 5);

   { test chaining }

   re.Expression := '[A-Z][A-Z]?[A-Z]';
   re.Exec ('234578923457823659ARTZU38');
   Check (0, 19, 3);

   re.Expression := '[A-Z][A-Z]*[0-9]';
   re.Exec ('234578923457823659ARTZU38');
   Check (0, 19, 6);

   re.Expression := '[A-Z]+[0-9]';
   re.Exec ('234578923457823659ARTZU38');
   Check (0, 19, 6);

   { case insensitive: }
   re.ModifierI := True;

   re.Expression := '[A-Z]';
   re.Exec ('234578923457823659a38');
   Check (0, 19, 1);

   { case insensitive: }
   re.Expression := '[a-z]';
   re.Exec ('234578923457823659A38');
   Check (0, 19, 1);

   re.ModifierI := False;

   { with parenthsis }
   re.Expression := '(foo)1234';
   re.Exec ('1234   foo1234XXXX');
   Check (1, 8, 7);

   re.Expression := '(((foo)))1234';
   re.Exec ('1234   foo1234XXXX');
   Check (3, 8, 7);

   re.Expression := '(foo)(1234)';
   re.Exec ('1234   foo1234XXXX');
   Check (2, 8, 7);

   { test real backtracking }

   re.Expression := 'nofoo|foo';
   re.Exec ('1234   foo1234XXXX');
   Check (0, 8, 3);

   re.Expression := '(nofoo|foo)1234';
   re.Exec ('1234   nofoo1234XXXX');
   Check (1, 8, 9);

   re.Expression := '(nofoo|foo|anotherfoo)1234';
   re.Exec ('1234   nofoo1234XXXX');
   Check (1, 8, 9);

   re.Expression := 'nofoo1234|foo1234';
   re.Exec ('1234   foo1234XXXX');
   Check (0, 8, 7);

      { basic tests }

   initok:=GenerateRegExprEngine('.*',[],r);
   if not initok then
     do_error(50);
   if not(RegExprPos(r,'CXXXX',index,len)) or
     (index<>0) or (len<>5) then
     do_error(51);

   initok:=GenerateRegExprEngine('\t\t',[],r);
   if not initok then
     do_error(52);
   if not(RegExprPos(r,'a'+#9+#9+'b'+'\t\t',index,len)) or
     (index<>1) or (len<>2) then
     do_error(52);


   initok:=GenerateRegExprEngine('\t',[],r);
   if not initok then
     do_error(53);
   if not(RegExprPos(r,'a'+#9+#9+'b'+'\t\t',index,len)) or
     (index<>1) or (len<>1) then
     do_error(53);


   initok:=GenerateRegExprEngine('\w',[],r);
   if not initok then
     do_error(54);
   if not(RegExprPos(r,'- abc \w',index,len)) or
     (index<>2) or (len<>1) then
     do_error(54);


   { java package name }
   initok:=GenerateRegExprEngine('[A-Za-z]+([.][0-9A-Za-z]+)*',[],r);
   if not initok then
     do_error(92);
   if not(RegExprPos(r,'CXXXX',index,len)) or
     (index<>0) or (len<>5) then
     do_error(92);


   initok:=GenerateRegExprEngine('[A-Za-z]+([.][0-9A-Za-z]+)*',[],r);
   if not initok then
     do_error(92);
   if not(RegExprPos(r,'CXXXX.A',index,len)) or
     (index<>0) or (len<>7) then
     do_error(92);


   initok:=GenerateRegExprEngine('[A-Za-z]+([.][0-9A-Za-z]+)*',[],r);
   if not initok then
     do_error(92);
   if not(RegExprPos(r,'CXXXX.A9Package',index,len)) or
     (index<>0) or (len<>15) then
     do_error(92);


   initok:=GenerateRegExprEngine('[A-Za-z]+([.][0-9A-Za-z]+)*',[],r);
   if not initok then
     do_error(92);
   if not(RegExprPos(r,'1CXXXX.A9Package',index,len)) or
     (index<>1) or (len<>15) then
     do_error(92);


   { singleline }

   initok:=GenerateRegExprEngine('^TEST1',[],r);
   if not initok then
     do_error(101);
   if (RegExprPos(r,'THISISATEST1TEST1THIS',index,len)) then
     do_error(101);


   initok:=GenerateRegExprEngine('^TEST1(ANOTHER)',[],r);
   if not initok then
     do_error(102);
   if (RegExprPos(r,'THISISATEST1ANOTHER',index,len)) then
     do_error(102);


   initok:=GenerateRegExprEngine('^TEST1(ANOTHER)',[],r);
   if not initok then
     do_error(103);
   if not(RegExprPos(r,'TEST1ANOTHER',index,len)) or
     (index<>0) or (len<>12) then
     do_error(103);


   { multiline }

   { UNIX Newline }
   initok:=GenerateRegExprEngine('^TEST1',[ref_multiline],r);
   if not initok then
     do_error(120);
   if not(RegExprPos(r,'THISISATEST1'#10'TEST12',index,len)) or
     (index<>13) or (len<>5) then
     do_error(120);


   { Apple Newline }
   initok:=GenerateRegExprEngine('^TEST1',[ref_multiline],r);
   if not initok then
     do_error(121);
   if not(RegExprPos(r,'THISISATEST1'#13'TEST12',index,len)) or
     (index<>13) or (len<>5) then
     do_error(121);


   { DOS Newline }
   initok:=GenerateRegExprEngine('^TEST1',[ref_multiline],r);
   if not initok then
     do_error(122);
   if not(RegExprPos(r,'THISISATEST1'#13#10'TEST12',index,len)) or
     (index<>14) or (len<>5) then
     do_error(122);


   { IBM Mainframe Newline }
   initok:=GenerateRegExprEngine('^TEST1',[ref_multiline],r);
   if not initok then
     do_error(123);
   if not(RegExprPos(r,'THISISATEST1'#$85'TEST12',index,len)) or
     (index<>13) or (len<>5) then
     do_error(123);


   { Some weird cases }
   initok:=GenerateRegExprEngine('^TEST1',[ref_multiline],r);
   if not initok then
     do_error(124);
   if not(RegExprPos(r,#13#10#13#10'TEST12',index,len)) or
     (index<>4) or (len<>5) then
     do_error(124);


   initok:=GenerateRegExprEngine('^TEST1',[ref_multiline],r);
   if not initok then
     do_error(125);
   if RegExprPos(r,#13#10#13#10'F',index,len) then
     do_error(125);



   initok:=GenerateRegExprEngine('^TEST1(ANOTHER)',[ref_multiline],r);
   if not initok then
     do_error(102);
   if (RegExprPos(r,'THISISATEST1ANOTHER',index,len)) then
     do_error(102);


   initok:=GenerateRegExprEngine('^TEST1(ANOTHER)',[],r);
   if not initok then
     do_error(103);
   if not(RegExprPos(r,'TEST1ANOTHER',index,len)) or
     (index<>0) or (len<>12) then
     do_error(103);


   { END OF LINE CASES }
   initok:=GenerateRegExprEngine('TEST1$',[],r);
   if not initok then
     do_error(101);
   if (RegExprPos(r,'THISISATEST1TEST1THIS',index,len)) then
     do_error(101);


   initok:=GenerateRegExprEngine('TEST1(ANOTHER)$',[],r);
   if not initok then
     do_error(102);
   if not(RegExprPos(r,'!TEST1ANOTHER',index,len)) or
     (index<>1) or (len<>12) then


   initok:=GenerateRegExprEngine('TEST1(ANOTHER)$',[],r);
   if not initok then
     do_error(102);
   if not(RegExprPos(r,'!TEST1ANOTHERFOOBARTEST1ANOTHER',index,len)) or
     (index<>19) or (len<>12) then


   { UNIX Newline }
   initok:=GenerateRegExprEngine('TEST1$',[ref_multiline],r);
   if not initok then
     do_error(120);
   if not(RegExprPos(r,'THISISATEST1'#10'TEST12',index,len)) or
     (index<>7) or (len<>5) then
     do_error(120);


   { Apple Newline }
   initok:=GenerateRegExprEngine('TEST1$',[ref_multiline],r);
   if not initok then
     do_error(121);
   if not(RegExprPos(r,'THISISATEST1'#13'TEST12',index,len)) or
     (index<>7) or (len<>5) then
     do_error(121);


   { DOS Newline }
   initok:=GenerateRegExprEngine('TEST1$',[ref_multiline],r);
   if not initok then
     do_error(122);
   if not(RegExprPos(r,'THISISATEST1'#13#10'TEST12',index,len)) or
     (index<>7) or (len<>5) then
     do_error(122);


   { IBM Mainframe Newline }
   initok:=GenerateRegExprEngine('TEST1$',[ref_multiline],r);
   if not initok then
     do_error(123);
   if not(RegExprPos(r,'THISISATEST1'#$85'TEST12',index,len)) or
     (index<>7) or (len<>5) then
     do_error(123);


   { Some weird cases }
   initok:=GenerateRegExprEngine('TEST1$',[ref_multiline],r);
   if not initok then
     do_error(124);
   if not(RegExprPos(r,#13#10#13#10'TEST1'#13#10,index,len)) or
     (index<>4) or (len<>5) then
     do_error(124);


   initok:=GenerateRegExprEngine('TEST1$',[ref_multiline],r);
   if not initok then
     do_error(125);
   if RegExprPos(r,#13#10#13#10'F'#13#10#13#10,index,len) then
     do_error(125);



   initok:=GenerateRegExprEngine('TEST1(ANOTHER)$',[ref_multiline],r);
   if not initok then
     do_error(102);
   if (RegExprPos(r,'THISISATEST1ANOTHERFOO',index,len)) then
     do_error(102);


   initok:=GenerateRegExprEngine('TEST1(ANOTHER)$',[],r);
   if not initok then
     do_error(103);
   if not(RegExprPos(r,'TEST1ANOTHER',index,len)) or
     (index<>0) or (len<>12) then
     do_error(103);


   { start and end of string handling }
   initok:=GenerateRegExprEngine('^TEST1(ANOTHER)$',[],r);
   if not initok then
     do_error(103);
   if not(RegExprPos(r,'TEST1ANOTHER',index,len)) or
     (index<>0) or (len<>12) then
     do_error(103);


   initok:=GenerateRegExprEngine('^TEST1(ANOTHER)$',[],r);
   if not initok then
     do_error(103);
   if RegExprPos(r,'FOOTEST1ANOTHER',index,len) then
     do_error(103);



   (* {n,} tests *)
   initok:=GenerateRegExprEngine('(AZ){0,}',[],r);
   if not initok then
     do_error(700);
   if not(RegExprPos(r,'C',index,len)) or
     (index<>0) or (len<>0) then
     do_error(700);



   initok:=GenerateRegExprEngine('(AZ){0,}',[],r);
   if not initok then
     do_error(701);
   if not(RegExprPos(r,'AZ',index,len)) or
     (index<>0) or (len<>2) then
     do_error(701);



   initok:=GenerateRegExprEngine('(AZ){0,}',[],r);
   if not initok then
     do_error(702);
   if not(RegExprPos(r,'AZAZ',index,len)) or
     (index<>0) or (len<>4) then
     do_error(702);


   initok:=GenerateRegExprEngine('Cat2(AZ){0,}',[],r);
   if not initok then
     do_error(703);
   if not(RegExprPos(r,'Cat2AZAZ',index,len)) or
     (index<>0) or (len<>7) then
     do_error(703);


   initok:=GenerateRegExprEngine('C(AZ){3,}',[],r);
   if not initok then
     do_error(704);
   if (RegExprPos(r,'AZAZAZ',index,len)) then
     do_error(704);


   initok:=GenerateRegExprEngine('Cat(AZ){3,}',[],r);
   if not initok then
     do_error(705);
   if not(RegExprPos(r,'BCatAZAZAZDABCD',index,len)) or
     (index<>1) or (len<>9) then
     do_error(705);


   initok:=GenerateRegExprEngine('Cat(AZ){2,}Q',[],r);
   if not initok then
     do_error(705);
   if not(RegExprPos(r,'BCatAZAZAZAZQDABCD',index,len)) or
     (index<>1) or (len<>12) then
     do_error(705);


   initok:=GenerateRegExprEngine('CatAZ{0,}',[],r);
   if not initok then
     do_error(706);
   if RegExprPos(r,'BCatDAZZZBCD',index,len) then
     do_error(706);


   (* {n} tests *)
   initok:=GenerateRegExprEngine('Cat(AZ){3}',[],r);
   if not initok then
     do_error(715);
   if not(RegExprPos(r,'BCatAZAZAZDABCD',index,len)) or
     (index<>1) or (len<>9) then
     do_error(715);


   initok:=GenerateRegExprEngine('CatAz{5}',[],r);
   if not initok then
     do_error(716);
   if not(RegExprPos(r,'BCatAzzzzzDABCD',index,len)) or
     (index<>1) or (len<>9) then
     do_error(716);

   initok:=GenerateRegExprEngine('CatAz{5}',[],r);
   if not initok then
     do_error(717);
   if RegExprPos(r,'BCatDAzizzzzHello',index,len) then
     do_error(717);


   initok:=GenerateRegExprEngine('CatAz{0}',[],r);
   if not initok then
     do_error(718);
   if RegExprPos(r,'BCatDAzizzzzHello',index,len) then
     do_error(718);


   initok:=GenerateRegExprEngine('o{2}',[],r);
   if not initok then
     do_error(719);
   if not(RegExprPos(r,'book',index,len)) or
     (index<>1) or (len<>2) then
     do_error(719);


   (* {n,m} tests *)
   initok:=GenerateRegExprEngine('Cat(AZ){1,3}',[],r);
   if not initok then
     do_error(725);
   if not(RegExprPos(r,'BCatAZAZAZDABCD',index,len)) or
     (index<>1) or (len<>9) then
     do_error(725);


   initok:=GenerateRegExprEngine('Cat(AZ){1,3}',[],r);
   if not initok then
     do_error(725);
   if not(RegExprPos(r,'BCatAZAZDABCD',index,len)) or
     (index<>1) or (len<>7) then
     do_error(725);


   initok:=GenerateRegExprEngine('CatAz{1,5}',[],r);
   if not initok then
     do_error(726);
   if not(RegExprPos(r,'BCatAzzzzzzzzzzDABCD',index,len)) or
     (index<>1) or (len<>9) then
     do_error(726);

   initok:=GenerateRegExprEngine('CatAz{1,1}',[],r);
   if not initok then
     do_error(727);
   if not(RegExprPos(r,'BCatAzzzzzzzzzzDABCD',index,len)) or
     (index<>1) or (len<>5) then
     do_error(727);


   initok:=GenerateRegExprEngine('CatAz{3,4}',[],r);
   if not initok then
     do_error(728);
   if not(RegExprPos(r,'BCatAzzzzzzzzzzDABCD',index,len)) or
     (index<>1) or (len<>8) then
     do_error(728);



   initok:=GenerateRegExprEngine('CatAz{0,0}',[],r);
   if not initok then
     do_error(729);
   if RegExprPos(r,'BCatDAzizzzzHello',index,len) then
     do_error(729);


   initok:=GenerateRegExprEngine('o{2,2}',[],r);
   if not initok then
     do_error(730);
   if not(RegExprPos(r,'book',index,len)) or
     (index<>1) or (len<>2) then
     do_error(730);



   { ()* tests }
   initok:=GenerateRegExprEngine('(AZ)*',[],r);
   if not initok then
     do_error(800);
   if not(RegExprPos(r,'C',index,len)) or
     (index<>0) or (len<>0) then
     do_error(800);



   initok:=GenerateRegExprEngine('(AZ)*',[],r);
   if not initok then
     do_error(801);
   if not(RegExprPos(r,'AZ',index,len)) or
     (index<>0) or (len<>2) then
     do_error(801);



   initok:=GenerateRegExprEngine('(AZ)*',[],r);
   if not initok then
     do_error(802);
   if not(RegExprPos(r,'AZAZ',index,len)) or
     (index<>0) or (len<>4) then
     do_error(802);


   initok:=GenerateRegExprEngine('Cat(AZ)*',[],r);
   if not initok then
     do_error(803);
   if not(RegExprPos(r,'CatAZAZ',index,len)) or
     (index<>0) or (len<>7) then
     do_error(803);


   initok:=GenerateRegExprEngine('C(AZ)*',[],r);
   if not initok then
     do_error(804);
   if (RegExprPos(r,'AZAZ',index,len)) then
     do_error(804);


   initok:=GenerateRegExprEngine('Cat(AZ)*',[],r);
   if not initok then
     do_error(805);
   if not(RegExprPos(r,'BCatAZAZDABCD',index,len)) or
     (index<>1) or (len<>7) then
     do_error(805);


   initok:=GenerateRegExprEngine('Cat(AZ)*',[],r);
   if not initok then
     do_error(806);
   if not(RegExprPos(r,'BCatDABCD',index,len)) or
     (index<>1) or (len<>3) then
     do_error(806);



   { ()+ tests }
   initok:=GenerateRegExprEngine('(AZ)+',[],r);
   if not initok then
     do_error(850);
   if (RegExprPos(r,'C',index,len)) then
     do_error(850);



   initok:=GenerateRegExprEngine('(AZ)+',[],r);
   if not initok then
     do_error(851);
   if not(RegExprPos(r,'AZ',index,len)) or
     (index<>0) or (len<>2) then
     do_error(851);



   initok:=GenerateRegExprEngine('(AZ)+',[],r);
   if not initok then
     do_error(852);
   if not(RegExprPos(r,'AZAZ',index,len)) or
     (index<>0) or (len<>4) then
     do_error(852);


   initok:=GenerateRegExprEngine('Cat(AZ)+',[],r);
   if not initok then
     do_error(853);
   if not(RegExprPos(r,'CatAZAZ',index,len)) or
     (index<>0) or (len<>7) then
     do_error(853);


   initok:=GenerateRegExprEngine('C(AZ)+',[],r);
   if not initok then
     do_error(854);
   if (RegExprPos(r,'AZAZ',index,len)) then
     do_error(854);


   initok:=GenerateRegExprEngine('Cat(AZ)+',[],r);
   if not initok then
     do_error(855);
   if not(RegExprPos(r,'BCatAZAZDABCD',index,len)) or
     (index<>1) or (len<>7) then
     do_error(855);


   { ()? tests }

   initok:=GenerateRegExprEngine('(AZ)?',[],r);
   if not initok then
     do_error(900);
   if not(RegExprPos(r,'C',index,len)) or
     (index<>0) or (len<>0) then
     do_error(900);



   initok:=GenerateRegExprEngine('(AZ)?',[],r);
   if not initok then
     do_error(901);
   if not(RegExprPos(r,'AZ',index,len)) or
     (index<>0) or (len<>2) then
     do_error(901);



   initok:=GenerateRegExprEngine('(AZ)?',[],r);
   if not initok then
     do_error(902);
   if not(RegExprPos(r,'AZAZ',index,len)) or
     (index<>0) or (len<>2) then
     do_error(902);


   GenerateRegExprEngine('Cat(AZ)?',[],r);
   if not(RegExprPos(r,'CatAZAZ',index,len)) or
     (index<>0) or (len<>5) then
     do_error(903);


   GenerateRegExprEngine('C(AZ)?',[],r);
   if (RegExprPos(r,'AZAZ',index,len)) then
     do_error(904);


   GenerateRegExprEngine('Cat(AZ)?',[],r);
   if not(RegExprPos(r,'BCatAZAZDABCD',index,len)) or
     (index<>1) or (len<>5) then
     do_error(905);


   GenerateRegExprEngine('Cat(AZ)?',[],r);
   if not(RegExprPos(r,'BCatDABCD',index,len)) or
     (index<>1) or (len<>3) then
     do_error(906);



   { Character classes tests }

   GenerateRegExprEngine('[A-Z]',[],r);
   if not(RegExprPos(r,'234578923457823659GHJK38',index,len)) or
     (index<>18) or (len<>1) then
     do_error(1000);


   GenerateRegExprEngine('[A-Z]*',[],r);
   if not(RegExprPos(r,'2345ARTZU38',index,len)) or
     (index<>0) or (len<>0) then
     do_error(1002);


   GenerateRegExprEngine('[A-Z]+',[],r);
   if not(RegExprPos(r,'234578923457823659ARTZU38',index,len)) or
     (index<>18) or (len<>5) then
     do_error(1003);


   GenerateRegExprEngine('[A-Z][A-Z]*',[],r);
   if not(RegExprPos(r,'234578923457823659ARTZU38',index,len)) or
     (index<>18) or (len<>5) then
     do_error(1004);


   GenerateRegExprEngine('[A-Z][A-Z]?',[],r);
   if not(RegExprPos(r,'234578923457823659ARTZU38',index,len)) or
     (index<>18) or (len<>2) then
     do_error(1005);


   GenerateRegExprEngine('[^\d]+',[],r);
   if not(RegExprPos(r,'234578923457823659ARTZU38',index,len)) or
     (index<>18) or (len<>5) then
     do_error(1006);


   { test chaining }

   GenerateRegExprEngine('[A-Z][A-Z]?[A-Z]',[],r);
   if not(RegExprPos(r,'234578923457823659ARTZU38',index,len)) or
     (index<>18) or (len<>3) then
     do_error(1007);


   GenerateRegExprEngine('[A-Z][A-Z]*[0-9]',[],r);
   if not(RegExprPos(r,'234578923457823659ARTZU38',index,len)) or
     (index<>18) or (len<>6) then
     do_error(1008);


   GenerateRegExprEngine('[A-Z]+[0-9]',[],r);
   if not(RegExprPos(r,'234578923457823659ARTZU38',index,len)) or
     (index<>18) or (len<>6) then
     do_error(1009);


   { case insensitive: }

   GenerateRegExprEngine('[A-Z]',[ref_caseinsensitive],r);
   if not(RegExprPos(r,'234578923457823659a38',index,len)) or
     (index<>18) or (len<>1) then
     do_error(1100);


   { case insensitive: }
   GenerateRegExprEngine('[a-z]',[ref_caseinsensitive],r);
   if not(RegExprPos(r,'234578923457823659A38',index,len)) or
     (index<>18) or (len<>1) then
     do_error(1101);


   { with parenthsis }
   GenerateRegExprEngine('(foo)1234',[],r);
   if not(RegExprPos(r,'1234   foo1234XXXX',index,len)) or
     (index<>7) or (len<>7) then
     do_error(1200);


   GenerateRegExprEngine('(((foo)))1234',[],r);
   if not(RegExprPos(r,'1234   foo1234XXXX',index,len)) or
     (index<>7) or (len<>7) then
     do_error(1201);


   GenerateRegExprEngine('(foo)(1234)',[],r);
   if not(RegExprPos(r,'1234   foo1234XXXX',index,len)) or
     (index<>7) or (len<>7) then
     do_error(1202);


   { test real backtracking }

   GenerateRegExprEngine('nofoo|foo',[],r);
   if not(RegExprPos(r,'1234   foo1234XXXX',index,len)) or
     (index<>7) or (len<>3) then
     do_error(1300);


  GenerateRegExprEngine('abc\(123\)$',[],r);
  if not (RegExprPos(r,'1234 abc(123)', index, len)) or
         (index <> 5) or (len <> 8) then
    do_error (1400);


  GenerateRegExprEngine('^\t$',[ref_singleline],r);
  if not (RegExprPos(r,#9, index, len)) or
         (index <> 0) or (len <> 1) then
    do_error (1401);


  GenerateRegExprEngine('^\n$',[ref_singleline],r);
  if not (RegExprPos(r,#10, index, len)) or
         (index <> 0) or (len <> 1) then
    do_error (1402);


  GenerateRegExprEngine('^\f$',[ref_singleline],r);
  if not (RegExprPos(r,#12, index, len)) or
         (index <> 0) or (len <> 1) then
    do_error (1403);


  GenerateRegExprEngine('^\r$',[ref_singleline],r);
  if not (RegExprPos(r,#13, index, len)) or
         (index <> 0) or (len <> 1) then
    do_error (1404);


  GenerateRegExprEngine('^\a$',[ref_singleline],r);
  if not (RegExprPos(r,#7, index, len)) or
         (index <> 0) or (len <> 1) then
    do_error (1405);


  initok:=GenerateRegExprEngine('^(([^:/?#]+):)',[],r);
  if not (RegExprPos(r,'http:',index, len)) or
         (index <> 0) or (len <> 5) then
    do_error (1406);


  initok:=GenerateRegExprEngine('^(([^:/?#]+):)?(//([^/?#]*))?',[],r);
  if not initok then
     do_error(1407);
  if not (RegExprPos(r,'http://www.myurl.com',index, len)) or
         (index <> 0) or (len <> 20) then
    do_error (1407);


  initok:=GenerateRegExprEngine('^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\?([^#]*))?(#(.*))?',[],r);
  if not initok then
     do_error(1408);
  if not (RegExprPos(r,'http://www.myurl.com',index, len)) or
         (index <> 0) or (len <> 20) then
    do_error (1408);


//   acoff;

   writeln ('*** The test have been successfully finished ***');

end.
