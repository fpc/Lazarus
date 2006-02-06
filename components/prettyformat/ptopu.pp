{$mode objfpc}
{$h+}
Unit PtoPu;
{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt, member of
    the Free Pascal development team

    Pascal Pretty-Printer object implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
 This unit is based heavily on the code by

 Author:  Peter Grogono
   This program is based on a Pascal pretty-printer written by Ledgard,
   Hueras, and Singer.  See SIGPLAN Notices, Vol. 12, No. 7, July 1977,
   pages 101-105, and PP.DOC/HLP.
   This version of PP developed under Pascal/Z V4.0 or later.
   Very minor modifications for Turbo Pascal made by Willett Kempton
   March 1984 and Oct 84.  Runs under 8-bit Turbo or 16-bit Turbo.
   Toad Hall tweak, rewrite for TP 5, 28 Nov 89

The following was changed :
 - Object oriented
 - Uses streams
 - Run-time customizable.
}
{ $define debug}
Interface

uses Classes,Sysutils;

Const

  MAXSYMBOLSIZE = 65500;
  MAXSHOWSIZE = 40;
  MAXSTACKSIZE = 100;
  MAXKEYLENGTH = 15;     { The longest keywords are IMPLEMENTATION INITIALIZATION }
  DEFLINESIZE = 100;
  DEFINDENT = 2;
  
TYPE
  Token    = AnsiString;
  FileName = STRING;

  TTokenScope = (tsInterface,tsImplementation);

  { Keysymbols }
  { If you add keysyms, adjust the definition of lastkey }
  keysymbol =  { keywords }
              (endsym,beginsym,ifsym,thensym,elsesym,procsym,varsym,ofsym,
               whilesym,dosym,casesym,withsym,forsym,repeatsym,untilsym,
               funcsym,labelsym,constsym,typesym,recordsym,stringsym,progsym,
               { TP and Delphi keywords}
               asmsym, trysym, finallysym,exceptsym,raisesym,classsym,objectsym,
               constructorsym,destructorsym,inheritedsym,propertysym,
               privatesym,publicsym,protectedsym,publishedsym,
               initializationsym,finalizationsym,
               inlinesym,librarysym,interfacesym,implementationsym,
               readsym,writesym,unitsym,
               { Not used for formatting }
               andsym,arrsym,divsym,downsym,filesym,gotosym,insym,modsym,
               notsym,nilsym,orsym,setsym,tosym,virtualsym,usessym,
               casevarsym,ofobjectsym,
               { other symbols }
               becomes,delphicomment,dopencomment,dclosecomment,opencomment,closecomment,semicolon,colon,equals,
               openparen,closeparen,period,endoffile,othersym);

  { Formatting options }
  { If you add options, adjust the definition of lastopt }
  options = (crsupp, // suppress CR before the keyword
             crbefore, // force CR before keyword
             blinbefore, // blank line before keyword
             dindonkey, // de−indent on assiociated keywords
             dindent, // deindent
             spbef, // space before
             spaft, // space after
             gobsym, // Print symbols which follow a keyword but which do not
                     //  affect layout. prints until terminators occur.
             inbytab, // indent by tab.
             inbyindent, //
             crafter, // force CR after keyword.
             upper, // prints keyword all uppercase
             lower, // prints keyword all lowercase
             capital // capitalizes keyword: 1st letter uppercase, rest lowercase.
             );

  optionset = SET OF options;
  keysymset = SET OF keysymbol;

  tableentry = RECORD
                 selected : optionset;
                 dindsym : keysymset;
                 terminators : keysymset
               END;

  { Character identification }

  charname = (letter,digit,space,quote,endofline,
              filemark,otherchar);

  charinfo = RECORD
               name : charname;
               Value : CHAR
             END;

  symbol = RECORD
             name : keysymbol;
             Value : Token;
             IsKeyWord : BOOLEAN;
             length, spacesbefore, crsbefore : INTEGER;
           END;

  symbolinfo = ^ symbol;

  stackentry = RECORD
                 indentsymbol : keysymbol;
                 prevmargin : INTEGER;
               END;

  symbolstack = ARRAY [1..MAXSTACKSIZE] OF stackentry;

Const FirstOpt = crsupp;
      LastOpt = capital; { Adjust this if you add options }
      FirstKey = endsym;
      LastKey = othersym; { Adjust this if you add options }
      LastFormatsym = usessym;

Type
  tableptr = ^tableentry;
  optiontable = ARRAY [Ttokenscope,keysymbol] OF tableptr;
  OEntriesTable = Array [keysymbol] OF String[15];
  ONamesTable = Array [Options] of String[15];
  KeywordTable = ARRAY [endsym..lastFormatsym] OF String[MAXKEYLENGTH];
  SpecialChar = ARRAY [1..2] OF CHAR;
  dblcharset = SET OF endsym..othersym;
  DblCharTable = ARRAY [becomes..dclosecomment] OF SpecialChar;
  SglCharTable = ARRAY [opencomment..period] OF CHAR;
  
  TVerboseEvent = Procedure (Sender : TObject; Const Msg : String) of Object;

  { TPrettyPrinter }

  TPrettyPrinter=Class(TObject)
  Private
    FTokenScope: TTokenScope;
{$ifdef debug}
    GobbleLevel : Integer;
{$endif debug}
    PreviousSymbol : keysymbol;
    RecordLevel : Integer;
    ClassSeen,ObjectSeen : Boolean;
    LastStruct : KeySymbol;
    CRPending : BOOLEAN;
    currchar,nextchar : charinfo;
    currsym,nextsym : symbolinfo;
    inlines,outlines : INTEGER;
    stack   : symbolstack;
    top,startpos,currlinepos,currmargin : Integer;
    option : OptionTable;
    FOnVerbose :  TVerboseEvent;
    FirstWordStackPos,
    FirstWordPos,
    FLineSize,
    FIndent : Integer;
    ins,outs,cfgs : TStream;
    Procedure Verbose (Const Msg : String);
    Procedure GetChar;
    Procedure StoreNextChar(VAR lngth: INTEGER;
                            VAR Value: Token);
    Procedure SkipBlanks(VAR spacesbefore, crsbefore: INTEGER);
    Procedure GetComment(sym: symbolinfo);
    Procedure GetDoubleComment(sym: symbolinfo);
    Procedure GetDelphiComment(sym: symbolinfo);
    Procedure GetNumber(sym: symbolinfo);
    Procedure GetCharLiteral(sym: symbolinfo);
    Function  char_Type: keysymbol;
    Procedure GetSpecialChar(sym: symbolinfo);
    Procedure GetNextSymbol(sym: symbolinfo);
    Procedure GetIdentifier(sym: symbolinfo);
    Procedure GetSymbol;
    Procedure PopStack(OUT indentsymbol: keysymbol;
                       OUT prevmargin: INTEGER);
    Procedure PushStack(indentsymbol: keysymbol;
                        prevmargin: INTEGER );
    Procedure WriteCRs(numberofcrs: INTEGER);
    Procedure InsertCR;
    Procedure InsertBlankLine;
    Procedure LShiftOn(dindsym: keysymset);
    Procedure LShift;
    Procedure InsertSpace(VAR symbol: symbolinfo);
    Procedure MoveLinePos(newlinepos: INTEGER);
    Procedure PrintSymbol;
    Procedure PPSymbol;
    Procedure Gobble(terminators: keysymset);
    Procedure RShift(currmsym: keysymbol);
    Procedure RShiftIndent(currmsym: keysymbol);
    Function ReadConfigFile: Boolean;
  Public
    Constructor Create;
    Function PrettyPrint : Boolean;
    Property OnVerbose : TVerboseEvent Read FOnVerbose Write FOnVerbose;
    Property LineSize : Integer Read FLineSize Write FLineSize;
    Property Indent : Integer Read FIndent Write FIndent;    { How many characters to indent ? }
    Property Source : TStream Read Ins Write Ins;
    Property Dest : TStream Read OutS Write Outs;
    Property Config : Tstream Read cfgS Write cfgs;
    Property CurrentScope : TTokenScope Read FTokenScope Write FTokenScope;
  end;

Procedure GenerateCfgFile(S: TStream);

CONST
  PtoPuVersion = '20 February 2005';  {was '11 October 1984','28 November 1989'; ..ancient stuff!}

Implementation

const
  //NUL = 0;      { ASCII null character }
  //TAB = 9;      { ASCII tab character }
  //FF = 12;      { ASCII formfeed character }
  //CR = 13;      { ASCII carriage return }
  //ESC = 27;     { ASCII escape character }
  Blank = ' ';
  //MAXBYTE = 255;{ Largest value of 1 byte variable }


VAR
  sets : tableptr;
  dblch   : dblcharset;

CONST
  Keyword : KeywordTable =
     ('END', 'BEGIN', 'IF', 'THEN',
      'ELSE', 'PROCEDURE', 'VAR', 'OF',
      'WHILE', 'DO', 'CASE', 'WITH',
      'FOR', 'REPEAT', 'UNTIL', 'FUNCTION',
      'LABEL', 'CONST', 'TYPE', 'RECORD',
      'STRING', 'PROGRAM',
      'ASM','TRY','FINALLY','EXCEPT','RAISE','CLASS','OBJECT',
      'CONSTRUCTOR','DESCTRUCTOR','INHERITED','PROPERTY',
      'PRIVATE','PUBLIC','PROTECTED','PUBLISHED',
      'INITIALIZATION','FINALIZATION',
      'INLINE','LIBRARY','INTERFACE','IMPLEMENTATION',
      'READ','WRITE','UNIT',
      {keywords not used for formatting }
      'AND', 'ARRAY', 'DIV', 'DOWNTO',
      'FILE', 'GOTO', 'IN', 'MOD',
      'NOT', 'NIL', 'OR', 'SET','TO','VIRTUAL','USES'
     );


  EntryNames : OEntriesTable =
              ('end','begin','if','then','else','proc','var',
               'of','while','do','case','with','for','repeat','until',
               'func','label','const','type','record','string',
               'prog',
               'asm','try','finally','except','raise','class','object',
               'constructor','destructor','inherited','property',
               'private','public','protected','published',
               'initialization','finalization',
               'inline','library','interface','implementation',
               'read','write','unit',

               'and','arr','div','down','file','goto',
               'in','mod','not','nil','or','set','to','virtual','uses',
               'casevar','ofobject',
               'becomes','delphicomment','dopencomment','dclosecomment',
               'opencomment','closecomment','semicolon',
               'colon','equals',
               'openparen','closeparen','period','endoffile','other');

  OptionNames : ONamesTable =
       ('crsupp','crbefore','blinbefore',
        'dindonkey','dindent','spbef','spaft',
        'gobsym','inbytab','inbyindent','crafter','upper',
        'lower','capital');


  DblChar : DblCharTable =
     ( ':=', '//','(*','*)' );

  SglChar : SglCharTable =
    ('{', '}', ';', ':', '=', '(', ')', '.' );

{ ---------------------------------------------------------------------
    General functions, not part of the object.
  ---------------------------------------------------------------------}

  function upperStr(const s : string) : string;
  var
    i  : longint;
  begin
     setLength(upperStr,length(s));
     for i:=1 to length(s) do
      if s[i] in ['a'..'z'] then
       upperStr[i]:=char(byte(s[i])-32)
      else
       upperStr[i]:=s[i];
  end;

  function LowerStr(const s : string) : string;
  var
    i  : longint;
  begin
     setLength(LowerStr,length(s));
     for i:=1 to length(s) do
      if s[i] in ['A'..'Z'] then
       LowerStr[i]:=char(byte(s[i])+32)
      else
       LowerStr[i]:=s[i];
  end;



Function IntToStr(I : LongInt) : String;

var
 s : string;
begin
  s:='';
  str(I,s);
  IntToStr := s;
end;

Function StrToInt(Const S : String) : Integer;

Var Code : integer;
    Res : Integer;

begin
  Code:=0;
  Val(S, Res, Code);
  StrToInt := Res;
  If Code<>0 then StrToInt:=0;
end;

Procedure Strip (Var S : String);

Const WhiteSpace =  [#32,#9,#10,#13];

Var I,J : Longint;

begin
  If length(s)=0 then exit;
  I:=1;
  While (S[I] in whitespace) and (I<Length(S)) do inc(i);
  J:=length(S);
  While (S[J] in whitespace) and (J>1) do dec(j);
  If I<=J then
    S:=Copy(S,i,j-i+1)
  else
    S:='';
end;

Procedure ClassID(Value: Token;
                  lngth: INTEGER;
                  VAR idtype: keysymbol;
                  VAR IsKeyWord: BOOLEAN);
  { Classify an identifier.  We are only interested
    in it if it is a keyword, so we use the hash table. }
  VAR
    Keyvalue: String[MAXKEYLENGTH];
    //tabent: INTEGER;
    //found : Integer;
    Sym : keysymbol;
    
  BEGIN
    IF lngth > MAXKEYLENGTH THEN BEGIN
      idtype := othersym;
      IsKeyWord := FALSE
    END
    ELSE
      BEGIN
      IsKeyWord := FALSE;
      KeyValue:= UpperStr(Value);
      sym:=endsym;
      While (Not IsKeyword) and (sym<=lastformatsym) DO
         begin
         iskeyword:=(KeyValue=Keyword[sym]);
         if not iskeyword then
           Sym:=Succ(sym);
         end;
      if IsKeyWord then
        idtype:=sym
      ELSE
        idtype := othersym;
      END
  END; { of ClassID }

{ ---------------------------------------------------------------------
    Functions to create options and set defaults.
  ---------------------------------------------------------------------}

Procedure CreateOptions (Out Option : OptionTable);

Var Sym : KeySymbol;
    T : TTokenScope;

begin
  FOR sym := endsym TO othersym DO
    For T:=Low(TTokenScope) to High(TTokenScope) do
      begin
      NEW(option[T,sym]);
      option[T,sym]^.selected := [];
      option[T,sym]^.dindsym := [];
      option[T,sym]^.terminators := []
      END;
end;

Procedure SetTerminators(Var Option : OptionTable);

Var
  T : TTokenScope;
  
begin
  For T:=Low(TTokenScope) to High(TTokenScope) do
    begin
    option[t,casesym]^.terminators    := [ofsym];
    option[t,casevarsym]^.terminators := [ofsym];
    option[t,forsym]^.terminators     := [dosym];
    option[t,whilesym]^.terminators   := [dosym];
    option[t,withsym]^.terminators    := [dosym];
    option[t,ifsym]^.terminators      := [thensym];
    option[t,untilsym]^.terminators   := [endsym, untilsym, elsesym, semicolon];
    option[t,becomes]^.terminators    := [endsym, untilsym, elsesym, semicolon];
    option[t,openparen]^.terminators  := [closeparen];
    option[t,usessym]^.terminators    := [semicolon];
    end;
end;

Procedure SetDefaultIndents (Var Option : OptionTable);

Var
  T : TTokenScope;

begin
  For T:=Low(TTokenScope) to High(TTokenScope) do
    begin
    option[t,recordsym]^.dindsym    := [endsym];
    option[t,funcsym]^.dindsym      := [labelsym, constsym, typesym, varsym];
    option[t,procsym]^.dindsym      := [labelsym, constsym, typesym, varsym];
    option[t,constsym]^.dindsym     := [labelsym, constsym, typesym, varsym];
    option[t,typesym]^.dindsym      := [labelsym, constsym, typesym, varsym];
    option[t,varsym]^.dindsym       := [labelsym, constsym, typesym, varsym];
    option[t,beginsym]^.dindsym     := [labelsym, constsym, typesym, varsym];
    option[t,publicsym]^.dindsym    := [endsym,protectedsym,privatesym,publicsym,publishedsym];
    option[t,privatesym]^.dindsym   := [endsym,protectedsym,privatesym,publicsym,publishedsym];
    option[t,protectedsym]^.dindsym := [endsym,protectedsym,privatesym,publicsym,publishedsym];
    option[t,publishedsym]^.dindsym := [endsym,protectedsym,privatesym,publicsym,publishedsym];
    option[t,finallysym]^.dindsym   := [trysym];
    option[t,exceptsym]^.dindsym   := [trysym];
    option[t,elsesym]^.dindsym      := [ifsym, thensym, elsesym];
    option[t,untilsym]^.dindsym     := [ifsym, thensym, elsesym, forsym, whilesym,
                                      withsym, colon, equals];
    option[t,endsym]^.dindsym       := [ifsym, thensym, elsesym, forsym, whilesym,
                                      withsym, casevarsym, colon, equals, recordsym,
                                      trysym,classsym,objectsym];
    option[t,semicolon]^.dindsym    := [ifsym, thensym, elsesym, forsym,
                                      whilesym, withsym, colon, equals];
    end;
end;

Procedure SetDefaults (Var Option : OptionTable);

{ Sets default values for the formatting rules. }

Var
  T : TTokenScope;

begin
  For T:=Low(TTokenScope) to High(TTokenScope) do
    begin
    option[t,progsym]^.selected         := [capital,blinbefore, spaft];
    option[t,unitsym]^.selected         := [capital,blinbefore, spaft];
    option[t,librarysym]^.selected      := [capital,blinbefore, spaft];
    option[t,funcsym]^.selected         := [capital,blinbefore, dindonkey, spaft];
    option[t,procsym]^.selected         := [capital,blinbefore, dindonkey, spaft];
    option[t,labelsym]^.selected        := [capital,blinbefore, spaft, inbytab];
    option[t,constsym]^.selected        := [capital,blinbefore, dindonkey, spaft, inbytab];
    option[t,typesym]^.selected         := [capital,blinbefore, dindonkey, spaft, inbytab];
    option[t,varsym]^.selected          := [capital,blinbefore, dindonkey, spaft, inbytab];
    option[t,beginsym]^.selected        := [capital,dindonkey, crbefore, crafter, inbytab];
    option[t,repeatsym]^.selected       := [capital,inbytab, crafter];
    option[t,recordsym]^.selected       := [capital,inbyIndent, crafter];
    option[t,objectsym]^.selected       := [capital,inbyIndent];
    option[t,classsym]^.selected        := [capital,inbyIndent];
    option[t,publicsym]^.selected       := [capital,crbefore, dindonkey, spaft];
    option[t,publishedsym]^.selected    := [capital,crbefore, dindonkey, spaft];
    option[t,protectedsym]^.selected    := [capital,crbefore, dindonkey, spaft];
    option[t,privatesym]^.selected      := [capital,crbefore, dindonkey, spaft];
    option[t,trysym]^.Selected          := [capital,crbefore,crafter,inbytab];
    option[t,finallysym]^.selected      := [capital,crbefore,dindent,crafter,inbytab];
    option[t,exceptsym]^.selected       := [capital,crbefore,dindent,crafter,inbytab];
    option[t,casesym]^.selected         := [capital,spaft, inbytab, gobsym, crafter];
    option[t,casevarsym]^.selected      := [capital,spaft, inbytab, gobsym, crafter];
    option[t,ofsym]^.selected           := [capital,crsupp, spbef, spaft];
    option[t,forsym]^.selected          := [capital,spaft, inbytab, gobsym, crafter];
    option[t,whilesym]^.selected        := [capital,spaft, inbytab, gobsym, crafter];
    option[t,withsym]^.selected         := [capital,spaft, inbytab, gobsym, crafter];
    option[t,dosym]^.selected           := [capital,crsupp, spbef];
    option[t,ifsym]^.selected           := [capital,spaft, inbytab, gobsym];
    option[t,implementationsym]^.selected := [capital,blinbefore,crafter];
    option[t,interfacesym]^.selected    := [capital,blinbefore,crafter];
    option[t,usessym]^.selected         := [capital,blinbefore,spaft];
    option[t,thensym]^.selected         := [capital];
    option[t,elsesym]^.selected         := [capital,crbefore, dindonkey, inbytab];
    option[t,endsym]^.selected          := [capital,crbefore, crafter,dindonkey,dindent];
    option[t,untilsym]^.selected        := [capital,crbefore, dindonkey, dindent, spaft,
                                          gobsym, crafter];
    option[t,becomes]^.selected         := [capital,spbef, spaft, gobsym];
    option[t,Delphicomment]^.Selected   := [crafter];
    option[t,opencomment]^.selected     := [capital,crsupp];
    option[t,closecomment]^.selected    := [capital,crsupp];
    option[t,semicolon]^.selected       := [capital,crsupp, dindonkey, crafter];
    option[t,colon]^.selected           := [capital,inbytab];
    option[t,equals]^.selected          := [capital,spbef, spaft, inbytab];
    option[t,openparen]^.selected       := [capital,gobsym];
    option[t,period]^.selected          := [capital,crsupp];
    end;
  option[tsInterface,funcsym]^.selected         := [capital, dindonkey, spaft];
  option[tsInterface,procsym]^.selected         := [capital, dindonkey, spaft];
end;

{ ---------------------------------------------------------------------
    Stream handling routines
  ---------------------------------------------------------------------}

Function ReadChar (S : TStream) : Char;

Var C : Char;

begin
  repeat
    if S.Position=S.Size then
      C:=#0
    else
      S.Read(C,1);
  Until (C<>#13);
  ReadChar:=C;
end;

Function EoSLn (S : TStream) : Char;

Const WhiteSpace = [' ', #9, #13 ];

Var C : Char;

begin
  Repeat
    if S.Position = S.Size then
      C:=#0
    else
      S.Read(C,1);
  Until (Not (C in WhiteSpace)) or ((C=#10));
  EoSln:=C;
end;

Function ReadString (S: TStream): String;

Var
  I : Byte;
  Count : Integer;
    
begin
  Result:='';
  I:=0;
  Repeat
    If ((I+1)>Length(Result)) then
      SetLength(Result,Length(Result)+255);
    Count:=S.Read(Result[I+1],1);
    If Count>0 then
      Inc(I);
  until (Result[I]=#10) or (Count=0);
  If Result[i]=#10 Then Dec(I);
  If Result[I]=#13 then Dec(I);
  SetLength(Result,I);
end;

Procedure WriteString (S : TStream; ST : String);

begin
  S.Write(St[1],length(St));
end;

Procedure WriteAnsiString (S : TStream; ST : AnsiString);

begin
  S.Write(St[1],length(St));
end;


Procedure WriteCR (S: TStream);

Const
  Newline = System.LineEnding;

begin
  WriteString(S,Newline);
end;


Procedure WriteLnString (S : TStream; ST : String);

begin
  WriteString(S,ST);
  WriteCR(S);
end;


{ ---------------------------------------------------------------------
    TPrettyPrinter object
  ---------------------------------------------------------------------}

Procedure TPrettyPrinter.Verbose (Const Msg : String);

begin
  If Assigned (FOnVerbose) then
    FOnVerbose(Self,Msg);
end;

Procedure TPrettyPrinter.GetChar;
{ Read the next character and classify it }
  VAR  Ch: CHAR;
  BEGIN
    currchar := nextchar;
    WITH nextchar DO
      begin
      Ch:=ReadCHar(Ins);
      If Ch=#0 then
        BEGIN
        name := filemark;
        Value := Blank
        END
      ELSE If (Ch=#10) THEN
        BEGIN
        name := endofline;
        Value := Ch;
        Inc(inlines);
        END
      ELSE
        BEGIN
        Value := Ch;
        IF Ch IN ['a'..'z', 'A'..'Z', '_'] THEN name := letter
        ELSE IF Ch IN ['0'..'9'] THEN name := digit
        ELSE IF Ch = '''' THEN name := quote
        ELSE IF Ch in [#13,' ',#9] THEN name := space
        ELSE name := otherchar
        END
      end;
  END; { of GetChar }


Procedure TPrettyPrinter.StoreNextChar(VAR lngth: INTEGER;
                        VAR Value: Token);
  { Store a character in the current symbol }
  BEGIN
    GetChar;
    IF lngth < MAXSYMBOLSIZE THEN BEGIN {XXX - should there be a limit at all?}
      Inc(lngth);
      setlength(Value,lngth);
      Value[lngth] := currchar.Value;
    END;
  END; { of StoreNextChar }


Procedure TPrettyPrinter.SkipBlanks(VAR spacesbefore, crsbefore: INTEGER);
  { Count the spaces between symbols }
  BEGIN
    spacesbefore := 0;
    crsbefore := 0;
    WHILE nextchar.name IN [space, endofline] DO BEGIN
      GetChar;
      CASE currchar.name OF
        space:      Inc(spacesbefore);
        endofline:  BEGIN
                      Inc(crsbefore);
                      spacesbefore := 0;
                    END;
      END;  {case}
    END;
  END; { of SkipBlanks }


Procedure TPrettyPrinter.GetComment(sym: symbolinfo);
  { Process comments using brace notation }
  BEGIN
    sym^.name := opencomment;
    WHILE NOT ((currchar.Value = '}') 
    OR (nextchar.name = filemark)) DO
      StoreNextChar(sym^.length, sym^.Value);
    IF currchar.Value = '}' THEN sym^.name := closecomment;
  END; { of GetCommment }

Procedure TPrettyPrinter.GetDoubleComment(sym: symbolinfo);
  { Process comments using parenthesis notation }
  BEGIN
    sym^.name := dopencomment;
    WHILE NOT (((currchar.Value = '*') AND (nextchar.Value = ')'))
    OR (nextchar.name = filemark)) DO
      StoreNextChar(sym^.length, sym^.Value);
    IF (currchar.Value = '*') AND (nextchar.Value = ')') THEN BEGIN
      StoreNextChar(sym^.length, sym^.Value);
      sym^.name := dclosecomment;
    END;
  END; { of GetDoubleCommment }

Procedure TPrettyPrinter.GetDelphiComment(sym: symbolinfo);
  { Process comments using either brace or parenthesis notation }
  BEGIN
    sym^.name := Delphicomment;
    WHILE NOT ((nextchar.name = endofline) OR (nextchar.name = filemark)) DO
      StoreNextChar(sym^.length, sym^.Value);

  END; { of GetDelphiCommment }



Procedure TPrettyPrinter.GetIdentifier(sym: symbolinfo);
  { Read an identifier and classify it }
  BEGIN
    WHILE nextchar.name IN [letter, digit] DO
      StoreNextChar(sym^.length, sym^.Value);
    ClassID(sym^.Value, sym^.length, sym^.name, sym^.IsKeyWord);
    IF sym^.name IN [recordsym, objectsym,classsym, casesym, endsym] THEN
      begin
      if sym^.name=implementationsym then
        FTokenScope:=tsImplementation;
      if sym^.name in [recordsym,objectsym,classsym] then
        LastStruct:=sym^.name;
      CASE sym^.name OF
        RecordSym : Inc(RecordLevel);
        ClassSym : ClassSeen:=True;
        objectsym : begin
                    if (PreviousSymbol=Ofsym) then
                      sym^.name:=ofobjectsym
                    else
                      ObjectSeen:=True;
                    end;
        casesym   : IF (RecordLevel>0) and (LastStruct=recordsym) THEN sym^.name := casevarsym;
        endsym    : If (LastStruct=recordsym) then
                      Dec(Recordlevel);
                    else
                      begin
                      ClassSeen:=False;
                      ObjectSeen:=False;
                      end
      END;  {case}
      end;
     If (PreviousSymbol=ClassSym) and (sym^.Name=ofsym) then
       ClassSeen:=False;
     PreviousSymbol:=sym^.Name;
  END; { of GetIdentifier }


{ Read a number and store it as a string }
Procedure TPrettyPrinter.GetNumber(sym: symbolinfo);
  BEGIN
    WHILE nextchar.name = digit DO StoreNextChar(sym^.length, sym^.Value);
    sym^.name := othersym;
  END; { of GetNumber }


PROCEDURE TPrettyPrinter.GetCharLiteral(sym: symbolinfo);
  { Read a quoted string }
  BEGIN
    WHILE nextchar.name = quote DO BEGIN
      StoreNextChar(sym^.length, sym^.Value);
      WHILE NOT (nextchar.name IN [quote, endofline, filemark]) DO
        StoreNextChar(sym^.length, sym^.Value);
      IF nextchar.name = quote THEN StoreNextChar(sym^.length, sym^.Value);
    END;
    sym^.name := othersym;
  END; { of GetCharLiteral }


FUNCTION TPrettyPrinter.char_Type: keysymbol;

  { Classify a character pair }

  VAR
    NextTwoChars: SpecialChar;
    Hit: BOOLEAN;
    thischar: keysymbol;
  BEGIN
    NextTwoChars[1] := currchar.Value;
    NextTwoChars[2] := nextchar.Value;
    thischar := becomes;
    Hit := FALSE;
    WHILE NOT (Hit OR (thischar = opencomment)) DO BEGIN
      IF NextTwoChars = DblChar[thischar] THEN Hit := TRUE
      ELSE Inc(thischar);
    END;
    IF NOT Hit THEN BEGIN
      thischar := opencomment;
      WHILE NOT (Hit OR (PRED(thischar) = period)) DO BEGIN
        IF currchar.Value = SglChar[thischar] THEN Hit := TRUE
        ELSE Inc(thischar);
      END;
    END;
    IF Hit THEN char_Type := thischar
    ELSE char_Type := othersym;
  END; { of char_Type }


Procedure TPrettyPrinter.GetSpecialChar(sym: symbolinfo);
   { Read special characters }
  BEGIN
    StoreNextChar(sym^.length, sym^.Value);
    sym^.name := char_Type;
    IF sym^.name IN dblch THEN StoreNextChar(sym^.length, sym^.Value)
  END; { of GetSpecialChar }


Procedure TPrettyPrinter.GetNextSymbol(sym: symbolinfo);
  { Read a symbol using the appropriate procedure }
  BEGIN
    CASE nextchar.name OF
      letter:     GetIdentifier(sym);
      digit:      GetNumber(sym);
      quote:      GetCharLiteral(sym);
      otherchar:  BEGIN
                    GetSpecialChar(sym);
                    IF sym^.name = opencomment THEN GetComment(sym)
                    else IF sym^.name = dopencomment THEN GetDoubleComment(sym)
                    else IF sym^.name= DelphiComment then GetDelphiComment(Sym)
                  END;
      filemark:   sym^.name := endoffile;
      ELSE {:} {Turbo}
        WRITELN('Unknown character type: ', ORD(nextchar.name));
    END;  {case}
  END; { of GetNextSymbol }


Procedure TprettyPrinter.GetSymbol;
{ Store the next symbol in NEXTSYM }
  VAR
    dummy: symbolinfo;
  BEGIN
    dummy := currsym;
    currsym := nextsym;
    nextsym := dummy;
    SkipBlanks(nextsym^.spacesbefore, nextsym^.crsbefore);
    nextsym^.length := 0;
    nextsym^.IsKeyWord := FALSE;
    IF currsym^.name = opencomment THEN GetComment(nextsym)
    ELSE IF currsym^.name = dopencomment THEN GetDoubleComment(nextsym)
    ELSE GetNextSymbol(nextsym);
  END;  {of GetSymbol}


Procedure TprettyPrinter.PopStack(OUT indentsymbol: keysymbol;
                                  OUT prevmargin: INTEGER);
  { Manage stack of indentation symbols and margins }
  BEGIN
    IF top > 0 THEN BEGIN
      indentsymbol := stack[top].indentsymbol;
      prevmargin := stack[top].prevmargin;
      Dec(top);
    END
    ELSE BEGIN
      indentsymbol := othersym;
      prevmargin := 0;
    END;
  END; { of PopStack }


Procedure TPrettyPrinter.PushStack(indentsymbol: keysymbol;
                                   prevmargin: INTEGER );
  BEGIN
    Inc(top);
    stack[top].indentsymbol := indentsymbol;
    stack[top].prevmargin := prevmargin;
  END; { of PushStack }


Procedure TPrettyPrinter.WriteCRs(numberofcrs: INTEGER);
  VAR
    i: INTEGER;
  BEGIN
    IF numberofcrs > 0 THEN BEGIN
      FOR i := 1 TO numberofcrs DO
        WriteCr(OutS);
      Inc(outlines,numberofcrs);
      Currlinepos := 0;
      FirstWordStackPos:=-1;
    END;
  END; { of WriteCRs }


Procedure TPrettyPrinter.InsertCR;
  BEGIN
    IF currsym^.crsbefore = 0 THEN BEGIN
      WriteCRs(1);
      currsym^.spacesbefore := 0;
    END;
  END; { of InsertCR }


Procedure TPrettyPrinter.InsertBlankLine;
BEGIN
  IF currsym^.crsbefore = 0 THEN 
    BEGIN
    IF currlinepos = 0 THEN 
      WriteCRs(1)
    ELSE 
      WriteCRs(2);
      currsym^.spacesbefore := 0;
    END
  ELSE 
    IF currsym^.crsbefore = 1 THEN
      IF currlinepos > 0 THEN 
        begin
        WriteCRs(1);
        currsym^.spacesbefore := 0;
        end;
END; { of InsertBlankLine }


Procedure TPrettyPrinter.LShiftOn(dindsym: keysymset);
  { Move margin left according to stack configuration and current symbol }
  VAR
    indentsymbol: keysymbol;
    prevmargin: INTEGER;
  BEGIN
{$ifdef debug}
    Write('LShiftOn ',EntryNames[currsym^.name],' : ',FirstWordPos,'/',CurrMargin);
{$endif debug}
    IF top > 0 THEN BEGIN
      REPEAT
        PopStack(indentsymbol, prevmargin);
        IF indentsymbol IN dindsym THEN currmargin := prevmargin;
      UNTIL NOT (indentsymbol IN dindsym) OR (top = 0);
      IF NOT (indentsymbol IN dindsym) THEN
        PushStack(indentsymbol, prevmargin);
    END;
{$ifdef debug}
    Writeln('-> ',CurrMargin);
{$endif debug}
  END; { of LShiftOn }


Procedure TprettyPrinter.LShift;
{ Move margin left according to stack top }
  VAR
    indentsymbol: keysymbol;
    prevmargin: INTEGER;
  BEGIN
{$ifdef debug}
    Write('LShift ',EntryNames[currsym^.name],' : ',FirstWordPos,'/',CurrMargin);
{$endif debug}
    IF top > 0 THEN BEGIN
      PopStack(indentsymbol, prevmargin);
      currmargin := prevmargin;
(* maybe PopStack(indentsymbol,currmargin); *)
    END;
{$ifdef debug}
    Writeln('-> ',CurrMargin);
{$endif debug}
  END; { of LShift }

Procedure TprettyPrinter.RShift(currmsym: keysymbol);
  { Move right, stacking margin positions }
  BEGIN
{$ifdef debug}
    Write('RShift ',EntryNames[currmsym],' : ',FirstWordPos,'/',Currmargin);
{$endif debug}
    IF top < MAXSTACKSIZE THEN PushStack(currmsym, currmargin);
    IF startpos > currmargin THEN currmargin := startpos;
    Inc(currmargin,INDENT);
{$ifdef debug}
    Writeln(' -> ',Currmargin)
{$endif debug}
  END; { of RShift }

Procedure TprettyPrinter.RShiftIndent(currmsym: keysymbol);
  { Move right, stacking margin positions }
  BEGIN
{$ifdef debug}
    Write('RShiftIndent ',EntryNames[currmsym],' : ',FirstWordPos,'/',Currmargin);
{$ELSE}
    if currmsym=endsym then ;
{$endif debug}
    If (FirstWordStackPos>=0) then
      Top:=FirstWordStackPos
    else
      Top:=0;
{$ifdef debug}
    If (Top>0) then
      Write(' Stackpos ',Top,' Item: ',EntryNames[Stack[Top].IndentSymbol],' Pos: ',Stack[Top].Prevmargin)
    else
      Write(' no item on stack');
{$endif debug}
    IF top < MAXSTACKSIZE THEN PushStack(othersym, FirstWordPos);
//    IF top < MAXSTACKSIZE THEN PushStack(currmsym, currmargin);
    CurrMargin:=FirstWordPos+Indent;
{$ifdef debug}
    Writeln(' -> ',Currmargin)
{$endif debug}
  END; { of RShift }


Procedure TPrettyPrinter.InsertSpace(VAR symbol: symbolinfo);
  { Insert space if room on line }
  BEGIN
    IF currlinepos < LineSize THEN BEGIN
      WriteString(OutS, Blank);
      Inc(currlinepos);
      IF (symbol^.crsbefore = 0) AND (symbol^.spacesbefore > 0)
      THEN Dec(symbol^.spacesbefore);
    END;
  END; { of InsertSpace }


Procedure TPrettyPrinter.MoveLinePos(newlinepos: INTEGER);
  { Insert spaces until correct line position reached }
  VAR  i: INTEGER;
  BEGIN
    FOR i := SUCC(currlinepos) TO newlinepos DO
      WriteString(OutS, Blank);
    currlinepos := newlinepos;
  END; { of MoveLinePos }


Procedure TPrettyPrinter.PrintSymbol;

  BEGIN
    IF (currsym^.IsKeyWord) then
      begin
      If upper in sets^.selected Then
        WriteString (OutS,UpperStr(currsym^.value))
      else if lower in sets^.selected then
        WriteString (OutS,LowerStr(currsym^.value))
      else if capital in sets^.selected then
        begin
        WriteString(OutS,UpCase(CurrSym^.Value[1]));
        WriteString(OutS,LowerStr(Copy(CurrSym^.Value,2,MAXSYMBOLSIZE)));{XXX - ?should it be length?}
        end
      else
        WriteString(OutS,Currsym^.Value);
      end
    ELSE
      WriteAnsiString(OutS, currsym^.Value);
    startpos := currlinepos;
    Inc(currlinepos,currsym^.length);
    if (FirstWordStackPos=-1) then
      begin
      FirstWordPos:=startpos;
      FirstWordStackPos:=Top;
{$ifdef debug}
      write('First word : ',currlinepos,': ',currsym^.value);
      If (FirstWordStackPos>0) then
        writeln(' [Stack: ',FirstWordStackPos,' Item: "',EntryNames[Stack[FirstWordStackPos].IndentSymbol],'" Pos: ',Stack[FirstWordStackPos].Prevmargin,']')
      else
        Writeln(' No stack')
{$endif debug}
      end;
  END; { of PrintSymbol }


Procedure TPrettyPrinter.PPSymbol;
{ Find position for symbol and then print it }
  VAR  newlinepos: INTEGER;
  BEGIN
    WriteCRs(currsym^.crsbefore);
    IF ((currLinePos<>0) and (currlinepos + currsym^.spacesbefore > currmargin)) OR
       (currsym^.name IN [opencomment, closecomment,dopencomment, dclosecomment])
    THEN
      newlinepos := currlinepos + currsym^.spacesbefore
    ELSE
      newlinepos := currmargin;
    IF newlinepos + currsym^.length > LINESIZE THEN
      BEGIN {XXX - this needs to be cleaned for case of long symbol values}
      WriteCRs(1);
      IF currmargin + currsym^.length <= LINESIZE THEN
        newlinepos := currmargin
      ELSE IF currsym^.length < LINESIZE THEN
        newlinepos := LINESIZE - currsym^.length
      ELSE
        newlinepos := 0;
      END;
    MoveLinePos(newlinepos);
    PrintSymbol;
  END; { of PPSymbol }


Procedure TPrettyPrinter.Gobble(terminators: keysymset);
  { Print symbols which follow a formatting symbol but which do not
    affect layout }
  BEGIN
{$ifdef debug}
    Inc(GobbleLevel);
    Writeln('Gobble start ',GobbleLevel,' : ',EntryNames[currsym^.name]);
{$endif debug}
    IF top < MAXSTACKSIZE THEN PushStack(currsym^.name, currmargin);
    currmargin := currlinepos;
    WHILE NOT ((nextsym^.name IN terminators)
    OR (nextsym^.name = endoffile)) DO BEGIN
      GetSymbol;
      PPSymbol;
    END;
    LShift;
{$ifdef debug}
    Writeln('Gobble end ',gobblelevel,' : ',EntryNames[nextsym^.name],' ',nextsym^.name in terminators );
    Dec(GobbleLevel);
{$endif debug}
  END; { of Gobble }



Function TPrettyPrinter.ReadConfigFile : Boolean;

Var
  I,J : Longint;

  Procedure SetOption(TheKey : KeySymbol;Var OptionList : String);

  Var TheOpt  : Options;
      Found : Boolean;
      K : longint;
      opt : string;
      TS : TTokenScope;

  begin
    Repeat
      K:=pos(',',optionlist);
      If k>0 then
        begin
        opt:=Copy(OptionList,1,k-1);
        strip(opt);
        Delete(OptionList,1,k);
        end
      else
        opt:=OptionList;
      If Length(Opt)>0 then
        begin
        Found:=False;
        for TheOpt :=firstopt to lastopt do
          begin
          found:=opt=OptionNames[Theopt];
          If found then break;
          end;
        If not found then
          Verbose ('Unknown option on line '+inttostr(i)+': '+Opt)
        else
          For TS:=Low(TTokenScope) to High(TTokenScope) do
            Option[TS,TheKey]^.Selected:=Option[TS,TheKey]^.Selected+[TheOpt];
        end;
    until k=0;
  end;

  Procedure SetIndent(TheKey : KeySymbol; Var OptionList : String);

  Var
      TheIndent : Keysymbol;
      Found : Boolean;
      K : longint;
      opt : string;
      TS : TTokenScope;

  begin
    Repeat
      K:=pos(',',optionlist);
      If k>0 then
        begin
        opt:=Copy(OptionList,1,k-1);
        strip(opt);
        Delete(OptionList,1,k);
        end
      else
        opt:=OptionList;
      If Length(Opt)>0 then
        begin
        Found:=False;
        for TheIndent :=firstKey to lastKey do
          begin
          found:=opt=EntryNames[Theindent];
          If found then break;
          end;
        If not found then
          begin
          Verbose ('Unknown indent keysym on line '+inttostr(i)+': '+Opt);
          exit;
          end;
        For TS:=Low(TTokenScope) to High(TTokenScope) do
          Option[TS,TheKey]^.dindsym:=Option[TS,TheKey]^.dindsym+[Theindent];
        end;
    until k=0;
  end;

Var TheKey : KeySymbol;
    Found,DoIndent : Boolean;
    Line, Name : String;
    L : TStringList;
    
begin
  ReadConfigFile:=false;
  L:=TStringList.Create;
  Try
    L.LoadFromStream(CfgS);
    For I:=1 to L.Count do
      begin
      Line:=L[i-1];
      { Strip comment }
      If pos('#',Line)<>0 then
        Line:=Copy(Line,1,Pos('#',Line)-1);
      If length(Line)<>0 then
        begin
        J:=Pos('=',Line);
        If J>0 then
          begin
          Line:=LowerStr(Line);
          Name:=Copy(Line,1,j-1);
          Delete(Line,1,J);
          { indents or options ? }
          If (Name[1]='[') and
             (Name[Length(Name)]=']') then
             begin
             Name:=Copy(Name,2,Length(Name)-2);
             Doindent:=True;
             end
          else
             DoIndent:=False;
          Strip(Name);
          found:=false;
          for thekey:=firstkey to lastkey do
            begin
            found:=Name=EntryNames[thekey];
            If Found then break;
            end;
          If not found then
            Verbose ('Unknown keyword on line '+inttostr(i)+': '+Name)
          else
            If DoIndent then
              SetIndent(TheKey,Line)
            else
              SetOption(TheKey,Line)
          end
        else
          verbose ('Error in config file on line '+IntToStr(i));
        end;
      end;
  Finally
    L.Free;
  end;
  Verbose ('Processed configfile: read '+IntToStr(I)+' lines');
  ReadConfigFile:=true;
end;

Procedure GenerateCfgFile(S : TStream);

Var TheKey,TheIndent : KeySymbol;
    TheOpt : Options;
    Written : Boolean;
    Option : OptionTable;

begin
  CreateOptions(option);
  SetDefaults(option);
  SetDefaultIndents(option);
  For TheKey:=Firstkey to lastkey do
    begin
    { Write options }
    WriteString (S,EntryNames[TheKey]+'=');
    Written:=False;
    for TheOpt:=FirstOpt to LastOpt do
      If TheOpt in Option[tsInterface,TheKey]^.Selected then
        begin
        if written then
           WriteString (S,',')
        else
           Written:=True;
        writeString (S,OptionNames[TheOpt]);
        end;
    WriteCr (S);
    { Write de-indent keysyms, if any }
    If Option[tsInterface,TheKey]^.dindsym<>[] then
      begin
      WriteString (S,'['+EntryNames[TheKey]+']=');
      Written:=False;
      For TheIndent:=FirstKey to lastkey do
      If TheIndent in Option[tsInterface,TheKey]^.dindsym then
        begin
        if written then
           WriteString (S,',')
        else
           Written:=True;
        WriteString (S,EntryNames[Theindent]);
        end;
      WriteCr (S);
      end;
    end;
end;

Function trimMiddle ( a:ansistring; lnght: integer; size: integer):string;
var
    half:Integer;
begin
    if lnght > size 
    then
    begin
      half := (size - 3) div 2;
      trimMiddle := copy(a,1,half) + '...' + copy(a,lnght-half+1,half);
    end
    else
      trimMiddle := a;
end;

Function TPrettyPrinter.PrettyPrint : Boolean;

Begin
  PrettyPrint:=False;
  If Not Assigned(Ins) or Not Assigned(OutS) then
    exit;
  If Not Assigned(CfgS) then
    begin
    SetDefaults(Option);
    SetDefaultIndents(Option);
    end
  else
    ReadConfigFile;
  { Initialize variables }
  top := 0;
  currlinepos := 0;
  currmargin := 0;
  inlines := 0;
  outlines := 0;
  CrPending := FALSE;
  FirstWordStackPos:=-1;
  RecordLevel := 0;
  GetChar;
  NEW(currsym);
  NEW(nextsym);
  GetSymbol;
  WHILE nextsym^.name <> endoffile DO BEGIN
    GetSymbol;
{$ifdef debug}
    Writeln('line in-'+IntToStr(inlines)+' out-'+IntToStr(outlines)+
            ' symbol "'+EntryNames[currsym^.name]+'" = "'+
            trimMiddle(currsym^.value,length(currsym^.value),MAXSHOWSIZE)+'"');
{$endif debug}
    sets := option[FTokenScope,currsym^.name];
    IF (CrPending AND NOT (crsupp IN sets^.selected))
    OR (crbefore IN sets^.selected) THEN BEGIN
      InsertCR;
      CrPending := FALSE
    END;
    IF blinbefore IN sets^.selected THEN BEGIN
      InsertBlankLine;
      CrPending := FALSE
    END;
    IF dindonkey IN sets^.selected THEN
      LShiftOn(sets^.dindsym);
    IF dindent IN sets^.selected THEN
      LShift;
    IF spbef IN sets^.selected THEN InsertSpace(currsym);
    PPSymbol;
    IF spaft IN sets^.selected THEN InsertSpace(nextsym);
    IF inbytab IN sets^.selected THEN
      RShift(currsym^.name)
    else IF inbyindent IN sets^.selected THEN
      RShiftIndent(currsym^.name);
    IF gobsym IN sets^.selected THEN Gobble(sets^.terminators);
    IF crafter IN sets^.selected THEN CrPending := TRUE
  END;
  IF CrPending THEN WriteCRs(1);
  Verbose(IntToStr(inlines)+' lines read, '+IntToStr(outlines)+' lines written.');
  PrettyPrint:=True;
end;

Constructor TPrettyPrinter.Create;

Begin
  Indent:=DefIndent;
  LineSize:=DefLineSize;
  CreateOptions (Option);
  SetTerminators(Option);
  InS:=Nil;
  OutS:=Nil;
  CfgS:=Nil;
End;

{ ---------------------------------------------------------------------
    Unit initialization
  ---------------------------------------------------------------------}


Begin
  dblch := [becomes, opencomment];
end.
